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
026900080623     c                             TasCca = 'R')
027000040218     c   11
027100040218     can 10              ExSr      StampaEdp
027200040217     c                   Iter
027300040218    4c                   EndIf
027400040513      * Bolla mamma
027500040518     c                   Eval      kLblAa = TasAas
027600040518     c                   Eval      kLblLp = TasLnp
027700040518     c                   Eval      kLblNr = TasNrs
027800040518     c                   Eval      kLblNs = TasNsp
027900040518     c     kLbl          Setll     Fnlbl02l
028000040513     c                   If        %Equal(Fnlbl02l)
028100040513     c                   Iter
028200040513     c                   EndIf
028300040518      * Controllo se è una figlia
028400040518     c                   Clear                   conta
028500040518     c     kLbl          Chain     Fnlbl01l
028600040518     c                   If        %Found(Fnlbl01l)
028700040518      * Controllo se la mamma ha generato una figlia successiva
028800040518     c     kLblmf        Setll     Fnlbl02l
028900040518     c                   Do        *Hival
029000040518     c     kLblm         Reade     Fnlbl02l
029100040518     c                   If        %Eof(Fnlbl02l)
029200040518     c                   Leave
029300040518     c                   EndIf
029400040518     c                   Add       1             conta
029500040518     c                   EndDo
029600040518      * se ho contato almeno 2 bolle con la stessa mamma vuol dire che la mamma ha generato una
029700040518      * figlia successiva alla figlia che sto leggendo
029800040518     c                   If        conta > 1
029900040518     c                   Iter
030000040518     c                   EndIf
030100040518     c                   EndIf
030200040518
030300040218      * Autotrasportatore
030400040218     c                   Move      TasPdc        w0040
030500040218if  4c                   If        w0040 = 9876 or TasPdc = 0010103
030600040218     c                   Iter
030700040218    4c                   EndIf
030800040715      * se richiesta la generazione del file
030900040715if  4c                   If        PaFILE = 'S'
031000040715      * sommo le bolle elaborate
031100040715     c                   add       1             LdvTote
031200040715e   4c                   EndIf
031300040217      * Flag immagine
031400040218     c   11              Iter
031500040226
031600040226      * Conto quante bolle
031700040226     c                   Add       1             TotDcm
031800040715      * se richiesta la generazione del file
031900040715if  4c                   If        PaFILE = 'S'
032000040715      * sommo le bolle non scannerizzate
032100040715     c                   add       1             LdvTotn
032200040715e   4c                   EndIf
032300040218
032400060814      * se richiesta la generazione del file dettaglio
032500060814      * scrivo Wfldvd0f
032600060814if  4c                   If        PaFILEd = 'S'
032700060814     c                   ExSr      file_det
032800060814e   4c                   EndIf
032900060814
033000040217      * Stampo
033100040217     c                   ExSr      Stampa
033200040217
033300040218    3c                   EndDo
033400040226
033500040226      * Salto pagina x overflow
033600040226if  3c                   If        TotDcm > *Zeros
033700040226if  4c                   If        *In99
033800040226     c                   Write     Sb12T
033900040226     c                   Write     Sb12T1
034000040226     c                   Write     Sb12T2
034100040226     c                   Eval      *In99 = *Off
034200040226    4c                   EndIf
034300040226      * Stampo il totale per data consegna merce
034400040226     c                   Write     Sb12TDcm
034500040226      * Totale linea arrivo
034600040226     c                   Add       TotDcm        TotLna
034700040226     c                   Clear                   TotDcm
034800040226    3c                   EndIf
034900040715      * se richiesta la generazione del file
035000040715if  4c                   If        PaFILE = 'S'
035100040715      * lo scrivo se ho elaborato qualcosa
035200040715     c                   If        LdvTote > 0
035300040715     c                   Write     Wfldv000
035400040715     c                   EndIf
035500040715e   4c                   EndIf
035600040218
035700040218    2c                   EndDo
035800040226
035900040226      * Salto pagina x overflow
036000040226if  2c                   If        TotLna > *Zeros
036100040226if  3c                   If        *In99
036200040226     c                   Write     Sb12T
036300040226     c                   Write     Sb12T1
036400040226     c                   Write     Sb12T2
036500040226     c                   Eval      *In99 = *Off
036600040226    3c                   EndIf
036700040226      * Stampo il totale per linea di arrivo
036800040226     c                   Write     Sb12TLna
036900040226      * Totale finale
037000040226     c                   Add       TotLna        TotFin
037100040226     c                   Clear                   TotLna
037200040226    2c                   EndIf
037300040217
037400040218    1c                   EndDo
037500040226
037600040226      * Salto pagina x overflow
037700040226if  1c                   If        *In99
037800040226     c                   Write     Sb12T
037900040226     c                   Write     Sb12T1
038000040226     c                   Write     Sb12T2
038100040226     c                   Eval      *In99 = *Off
038200040226    1c                   EndIf
038300040226
038400040226      * Stampo il totale finale
038500040226     c                   If        TotFin > *Zeros
038600040226     c                   Write     Sb12TFin
038700040226     c                   EndIf
038800040218
038900040217      * Fine stampa
039000040217     c                   If        wokstp = *On
039100040217     c                   Write     Sb12e
039200040217     c                   EndIf
039300090720
039400090720      * Stampa per nessuna LDV da scannerizzare
039500090720     c                   If        wokstp = *off
039600090720     c                   Write     Sb12T
039700090720      * Decodifico la lna
039800090720     c                   If        PaLna > *Zeros
039900090720     c                   Eval      CodLna = PaLna
040000090720     c                   Else
040100090720     c                   Eval      CodLna = SkLna(01)
040200090720     c                   EndIf
040300090720     c     CodLna        Chain     Azorg01l
040400090720     c                   If        %Found(Azorg01l)
040500090720     c                   Movel     OrgDes        DesLna
040600090720     c                   EndIf
040700090720     c                   Write     Sb12T1
040800090720     c                   Write     Sb12Fine
040900090720     c                   endif
041000040216
041100040216     c                   Eval      *InLr = *On
041200040216
041300040217      *------------------------------------------------------------------------*
041400060814      * SCRIVO FILE DETTAGLIO
041500040217      *------------------------------------------------------------------------*
041600060814     c     File_det      BegSr
041700040217
041800060814     c                   z-add     TASaas        dLDVaas
041900060814     c                   z-add     TASlnp        dLDVlnp
042000060814     c                   z-add     TASnrs        dLDVnrs
042100060814     c                   z-add     TASnsp        dLDVnsp
042200060814     c                   z-add     TASlna        dLDVlna
042300060814     c                   z-add     TASdcm        dLDVdcm
042400060814
042500060814      * Verifica se p.o. mittente è DPD
042600060814     c                   movel     TASccm        W0030
042700060814     c     W0030         lookup    SkLnpd                                 21
042800060814     c                   If        *in21 = *on
042900060814     c                   movel     'S'           dLDVdpd
043000060814     c                   Else
043100060814     c                   movel     *blank        dLDVdpd
043200060814     c                   Endif
043300060814      *
043400060814     C                   write     WFldvd00
043500060814      *
043600060814     c                   EndSr
043700060814
043800060814      *------------------------------------------------------------------------*
043900060814      * STAMPA
044000060814      *------------------------------------------------------------------------*
044100060814     c     Stampa        BegSr
044200060814
044300040217      * Stampo la testata per la prima volta
044400040217if  1c                   If        wokstp = *Off
044500040217     c                   Write     Sb12T
044600040217      * Decodifico la lna richiesta
044700040310if  2c                   If        PaLna > *Zeros
044800040217     c                   Eval      CodLna = PaLna
044900040217   x2c                   Else
045000040217      * Decodifico la prima lna
045100040218     c                   Eval      CodLna = SkLna(xx)
045200040217    2c                   EndIf
045300040217     c     CodLna        Chain     Azorg01l
045400040217if  2c                   If        %Found(Azorg01l)
045500040217     c                   Movel     OrgDes        DesLna
045600040217    2c                   EndIf
045700040217     c                   Write     Sb12T1
045800040217     c                   Eval      wokstp = *On
045900040217     c                   Write     Sb12T2
046000040218     c                   Eval      OldLna = CodLna
046100040217    1c                   EndIf
046200040218      * Se richieste tutte le linee devo saltare pagina a cambio lna
046300040310if  1c                   If        (PaLna = *Zeros) and
046400040218     c                              OldLna > *Zeros and SkLna(xx) <> OldLna
046500040218     c                   Write     Sb12T
046600040218     c                   Eval      CodLna = SkLna(xx)
046700040218     c     CodLna        Chain     Azorg01l
046800040218if  2c                   If        %Found(Azorg01l)
046900040218     c                   Movel     OrgDes        DesLna
047000040218    2c                   EndIf
047100040218     c                   Write     Sb12T1
047200040218     c                   Write     Sb12T2
047300040218     c                   Eval      OldLna = SkLna(xx)
047400040226     c                   Eval      *In99 = *Off
047500040218    1c                   EndIf
047600040217
047700040217      * Salto pagina x overflow
047800040217if  1c                   If        *In99
047900040217     c                   Write     Sb12T
048000040217     c                   Write     Sb12T1
048100040217     c                   Write     Sb12T2
048200040217     c                   Eval      *In99 = *Off
048300040217    1c                   EndIf
048400040218
048500040218      * Giro data spedizione
048600040218     c                   Clear                   wlbdat
048700040218     c                   Z-add     TasDsp        g02inv
048800040218     c                   Movel     '3'           g02err
048900040218     c                   Call      'XSRDA8'
049000040218     c                   Parm                    wlbdat
049100040218     c                   Z-add     g02dat        DatDsp
049200040217
049300040217      * Mittente
049400040217     c                   Move      TasCcm        w0040
049500040217      * non codificato vado su Titaa
049600040217if  1c                   If        w0040 = *Zeros or w0040 = 8888
049700040217     c     kTaa          Chain     Titaa30c
049800040217if  2c                   If        %Found(Titaa30c)
049900040217     c                   Movel     TaaRsc        RscMit
050000040217    2c                   EndIF
050100040217      * codificato vado su Cnaco
050200040217   x1c                   Else
050300040217     c                   Clear                   Tibs69ds
050400040217     c                   Z-add     TasCcm        I69kac
050500040217     c                   call      'TIBS69R'
050600040217     c                   parm                    tibs69DS
050700040217     c                   parm                    ds_cnaco
050800040217     c                   parm                    ds_cnind
050900040217     c                   parm                    ds_cnclp
051000040217     c                   parm                    ds_fncls
051100040217if  2c                   If        O69err <> '1'
051200040217     c                   Movel     AcoRag        RscMit
051300040217    2c                   EndIf
051400040217    1c                   EndIf
051500040217      * Giro data consegna merce
051600040217     c                   Clear                   wlbdat
051700040217     c                   Z-add     TasDcm        g02inv
051800040217     c                   Movel     '3'           g02err
051900040217     c                   Call      'XSRDA8'
052000040217     c                   Parm                    wlbdat
052100040217     c                   Z-add     g02dat        DatDcm
052200040217      * Giro data distinta
052300040217     c                   Clear                   wlbdat
052400040217     c                   Z-add     TasDdc        g02inv
052500040217     c                   Movel     '3'           g02err
052600040217     c                   Call      'XSRDA8'
052700040217     c                   Parm                    wlbdat
052800040217     c                   Z-add     g02dat        DatDdc
052900101229      * Controllo se distinta a PDA in BUONA
053000110113      * su TITAS campo lungo 7 su FIDST campo lungo 6
053100110113      * se valore supera 999999 non provo a cercare la distinta
053200110113     c                   IF        §ogpdacon <> *blanks
053300110113     c                             and  tasndc <= 999999
053400101229     c                   clear                   ddstflr
053500101229     c                   clear                   flgpda
053600101229     c                   eval       dstnfv = tasndc
053700101229     c                   eval       dstfgs =
053800101229     c                              %dec(%subst(%editc(taspdc:'X'):1:3):3:0)
053900101229     c                   eval       dstnpg = 4
054000101229     c     kfidst        chain     fidst01l
054100101229    5c                   if        %found(fidst01l)
054200101229     c                   eval      ddstflr = dstflr
054300101229      * attiva su PDA e non in test
054400101229    6c                   if        (dstpda = 'C' or dstpda = 'E') and
054500101229      * in TEST
054600101229     c                              §dsttstpda <> 'C' and §dsttstpda <> 'E'
054700101229     c                   eval      flgpda = 'P'
054800101229     c                   endif
054900101229     c                   endif
055000110113     c                   ENDIF
055100040217      * Decodifico autotrasportatore
055200040217     c     kApd          Chain     Fiapd01l
055300040217if  1c                   If        %Found(Fiapd01l)
055400040217     c                   Movel     ApdRsc        DesPdc
055500040217    1c                   EndIf
055600040217
055700040217      * Dettaglio
055800040217     c                   Write     Sb12R
055900040217
056000040217     c                   EndSr
056100040218
056200040218      *------------------------------------------------------------------------*
056300040218      * STAMPA x EDP
056400040218      *------------------------------------------------------------------------*
056500040218     c     StampaEdp     BegSr
056600040218
056700040218     c                   If        *In98 = *Off  or *InOf
056800040218     c                   Except    Testa
056900040218     c  n98              Eval      *In98 = *On
057000040218     c                   EndIf
057100040218
057200040218     c                   Except    Bolla
057300040218
057400040218     c                   EndSr
057500040216
057600040216      *------------------------------------------------------------------------*
057700040216      * ROUTINE INIZIALE
057800040216      *------------------------------------------------------------------------*
057900040216     c     *Inzsr        BegSr
058000040216
058100040216     c     *entry        plist
058200040216     c                   parm                    Kpjba
058300040216     c                   Movel(p)  Kpjbu         Param
058400040702      * Imposto libreria - Se sono in ambiente di prova - GRPS
058500040702     c                   If        %subst(knsif:7:1) = 'P'
058600040702     c                   Eval      wlib = 'GAITRAGRPS'
058700101229     c                   eval      wlibfil = 'FILTRAPRD '
058800040702      *                  - Se sono in ambiente buono - GRU
058900040630     c                   Else
059000040702     c                   Eval      wlib = 'GAITRAGRU '
059100101229     c                   eval      wlibfil = 'FILTRA201 '
059200040630     c                   EndIf
059300040630      * OVRDBF su TITAS e TITAA
059400040630     c                   Clear                   comman
059500040630     c                   Movel(p)  cmd(1)        comman
059600040630     c                   Eval      %Subst(comman:30:10) = wlib
059700040630     c                   Eval      comman =
059800041021     c                             %trim(comman) + '/TITAS26C)'
059900040630     c                   Call      'QCMDEXC'                            99
060000040630     c                   Parm                    comman
060100040630     c                   Parm                    lenght
060200041021     c  n99              Open      Titas26c
060300040630     c                   Movel(p)  cmd(2)        comman
060400040630     c                   Eval      %Subst(comman:30:10) = wlib
060500040630     c                   Eval      comman =
060600040630     c                             %trim(comman) + '/TITAA30C)'
060700040630     c                   Call      'QCMDEXC'                            99
060800040630     c                   Parm                    comman
060900040630     c                   Parm                    lenght
061000040630     c  n99              Open      Titaa30c
061100101229
061200101229      * OVRDBF su FIDST
061300101229     c                   Clear                   comman
061400101229     c                   Movel(p)  cmd(4)        comman
061500101229     c                   Eval      %Subst(comman:30:10) = wlibfil
061600101229     c                   Eval      comman =
061700110113     c                             %trim(comman) + '/FIDST01L)'
061800101229     c                   Call      'QCMDEXC'                            99
061900101229     c                   Parm                    comman
062000101229     c                   Parm                    lenght
062100101229     c  n99              Open      FIDST01L
062200040715
062300040715      * Se richiesta la generazione del file di totali
062400040715     c                   If        PaFILE = 'S'
062500040715      * prima lo pulisco
062600040715     c                   Clear                   comman
062700040715     c                   Movel(p)  cmd(3)        comman
062800040715     c                   Call      'QCMDEXC'
062900040715     c                   Parm                    comman
063000040715     c                   Parm                    lenght
063100040715      * poi lo apro
063200040715     c                   Open      Wfldv00f
063300040715     c                   EndIf
063400040218
063500060814      * Se richiesta la generazione del file di dettaglio
063600060814     c                   If        PaFILEd = 'S'
063700060814      * prima lo pulisco
063800060814     c                   Clear                   comman
063900060814     c                   Movel(p)  cmd(6)        comman
064000060814     c                   Call      'QCMDEXC'
064100060814     c                   Parm                    comman
064200060814     c                   Parm                    lenght
064300060814      * poi lo apro
064400060814     c                   Open      Wfldvd0f
064500060814     c                   EndIf
064600060814
064700040218      * Utente EDP
064800040218     c                   If        %subst(Knmus:1:3) = 'EDP'
064900040218     c                   Eval      *In10 = *On
065000040218     c                   Else
065100040218     c                   Eval      *In10 = *Off
065200040218     c                   EndIF
065300040727
065400040727      * Recupero la data partenza ultima fase poste
065500040727     c                   Clear                   Tibs02ds
065600040727     c                   Clear                   dVpoPpt
065700040727     c                   Eval      T02Mod = 'C'
065800040727     c                   Eval      T02Sif = knsif
065900040727     c                   Eval      T02Cod = 'VPO'
066000040727     c                   Eval      T02Ke1 = 'PPT'
066100040727     c                   Call      'TIBS02R'
066200040727     c                   Parm                    kpjba
066300040727     c                   Parm                    Tibs02ds
066400040727     c                   If        T02Err = *Blanks
066500040727     c                   Eval      dVpoPpt = T02Uni
066600040727     c                   EndIf
066700040216
066800040216     c                   Clear                   Lin
066900040216     c                   Clear                   SkLna
067000040216     c                   Clear                   xx
067100040311     c                   Clear                   jj
067200040519     c                   Clear                   wwp
067300040519     c                   Clear                   wwd
067400040216     c                   Eval      yy = 1
067500040216
067600040311      * Leggo AZORG x caricare delle skiere di lavoro
067700040311      * SKLNA contiene:
067800040311      *      i p.o. validi con scannerizzazione attiva tranne DPD e PPT, nel
067900040630      *      caso che PaEEX = 'S' o PaDPD = 'S' contiene tutti i p.o. tranne DPD e PPT
068000040311      *      CARICO SKLNA SOLO SE PALNA = 0
068100040630      * SKEEX  contiene:
068200040311      *      i p.o. EEX
068300040630      * SKLNPD contiene:
068400040630      *      i p.o. DPD
068500040630      * SKLNPP contiene:
068600040630      *      i p.o. PPT
068700040311
068800040311     c     *Loval        Setll     Azorg01l
068900040311     c                   Read      Azorg01l
069000040311do  1c                   DOW       NOT %Eof(Azorg01l)
069100040311if  2c                   IF        OrgFva = *Blanks  AND
069200040311     c                             (OrgFag = 'F' or OrgFag = 'A')
069300040311     c                   Movel     OrgDe3        Og143
069400040311     c                   Movel     OrgDe6        Og146
069500040311      *
069600040311if  3c                   If        §OgNtw = 'PPT' or  §OgNtw = 'DPD'
069700040519if  4c                   If        §OgNtw = 'PPT'
069800040519     c                   Add       1             wwp
069900040519     c                   Eval      SkLnpp(wwp) = OrgFil
070000040519    4c                   Endif
070100040519if  4c                   If        §OgNtw = 'DPD'
070200040519     c                   Add       1             wwd
070300040519     c                   Eval      SkLnpd(wwd) = OrgFil
070400040519    4c                   Endif
070500040630   x3c                   Else
070600040311      *
070700040311if  4c                   If        §OgNtw = 'EEX'
070800040311     c                   Add       1             jj
070900040311     c                   Eval      SkEEX(jj) = OrgFil
071000040311    4c                   EndIF
071100040311if  4c                   If        Palna = *zeros  AND
071200040630     c                             (PaEEX = 'S'  or PaDPD = 'S' or
071300040311     c                             (§OgDse <> *Blanks and §OgDse <> *Zeros))
071400040311     c                   Add       1             xx
071500040311     c                   Eval      SkLna(xx) = OrgFil
071600040311    4c                   EndIf
071700040311    3c                   EndIf
071800040311      *
071900040311    2c                   EndIf
072000040311     c                   Read      Azorg01l
072100040311    1c                   EndDo
072200040630
072300040310      * Se la linea di arrivo è <> zero carico la sk con la lna richiesta
072400040218      * più i p.o. gestiti
072500040310if  1c                   If        PaLna > *Zeros
072600040216     c                   Eval      SkLna(1) = PaLna
072700040216      * Carico £6 della linea di arrivo richiesta
072800040216     c                   Clear                   Trul06ds
072900040216     c                   Eval      d06cod = '£6'
073000040216     c                   Movel     PaLna         d06key
073100040216     c                   Movel(p)  Trul06ds      Kpjbu
073200040216     c                   Call      'TRUL06R'
073300040216     c                   Parm                    Kpjba
073400040216     c                   Movel     Kpjbu         Trul06ds
073500040216do  2c                   Do        30            xx
073600040216if  3c                   If        Lin(xx) = *Zeros
073700040216     c                   Leave
073800040216    3c                   EndIf
073900040216if  3c                   If        Lin(xx) <> PaLna
074000040216     c                   Add       1             yy
074100040216     c                   Eval      SkLna(yy) = Lin(xx)
074200040216    3c                   EndIf
074300040216    2c                   EndDo
074400040217      * Ordino la sk
074500040217     c                   Movea     SkLna         wSkLna
074600040217     c                   Clear                   SkLna
074700040217     c                   Sorta     wSkLna
074800040217     c                   Clear                   yy
074900040217do  2c                   Do        250           xx
075000040217if  3c                   If        wSkLna(xx) > *Zeros
075100040217     c                   Add       1             yy
075200040217     c                   Eval      SkLna(yy) = wSkLna(xx)
075300040217    3c                   EndIf
075400040217    2c                   EndDo
075500040216    1c                   EndIf
075600040218
075700040218      * Carico una sk con le date da elaborare
075800040218     c                   Clear                   SkDcm
075900040218     c                   Eval      xx = 1
076000040218     c                   Eval      SkDcm(xx) = PaDcd
076100040218if  1c                   If        PaDcd <> PaDca
076200040218     c                   Eval      wdata = PaDcd
076300040218     c     *Iso          Movel     PaDcd         wdataiso
076400040218do  2c                   Dow       wdata < PaDca
076500040218     c                   Eval      xx = xx + 1
076600040218     c                   Adddur    1:*d          wdataiso
076700040218     c                   Move      wdataiso      wdata
076800040218     c                   Eval      SkDcm(xx) = wdata
076900040218    2c                   EndDo
077000040218    1c                   EndIf
077100041118
077200041118      * Carico uns sk con i tipi bolla da scartare
077300041118     c                   Clear                   skcbo
077400041118     c                   Clear                   xx
077500041118     c                   Eval      TblCod = '3A'
077600041118     c     kTab01        Setll     Tabel00f
077700041118     c                   Do        *Hival
077800041118     c     kTab01        Reade     Tabel00f
077900041118     c                   If        %Eof(Tabel00f)
078000041118     c                   Leave
078100041118     c                   EndIf
078200041118     c                   If        TblFlg <> *Blanks
078300041118     c                   Iter
078400041118     c                   EndIf
078500041118     c                   Eval      ds3a = TblUni
078600041118     c                   If        §3aTb1 = 'F3' or §3aTb1 = 'A3'
078700080707     c                             or §3atb1 = 'AR'
078800041118     c                   Eval      xx = xx + 1
078900041118     c                   Movel     TblKey        skcbo(xx)
079000041118     c                   EndIf
079100041118     c                   EndDo
079200120912
079300120912       //?Controllo se è stato richiesto un cliente
079400120912     c                   eval      *in12 = (PaKsc > 0)
079500120913     c   12              eval      CodKsc = PaKsc
079600120913     c   12              eval      DesKsc = PaRag
079700040216
079800040216     c     kTas          Klist
079900040218     c                   Kfld                    SkDcm(yy)
080000040218     c                   Kfld                    SkLna(xx)
080100040217
080200040217     c     kTaa          Klist
080300040217     c                   Kfld                    TasAas
080400040217     c                   Kfld                    TasLnp
080500040217     c                   Kfld                    TasNrs
080600040217     c                   Kfld                    TasNsp
080700040217     c                   Kfld                    kTaaTrc
080800101229
080900101229     c     kfidst        klist
081000101229     c                   Kfld                    dstnpg
081100101229     c                   kfld                    dstnfv
081200101229     c                   kfld                    dstfgs
081300040217
081400040217     c     kApd          Klist
081500040217     c                   Kfld                    kApdTip
081600040217     c                   Kfld                    TasPdc
081700040513
081800040518     c     kLbl          Klist
081900040518     c                   Kfld                    kLblAa
082000040518     c                   Kfld                    kLblLp
082100040518     c                   Kfld                    kLblNr
082200040518     c                   Kfld                    kLblNs
082300040518
082400040518     c     kLblm         Klist
082500040518     c                   Kfld                    LblAap
082600040518     c                   Kfld                    LblLpp
082700040518     c                   Kfld                    LblNrp
082800040518     c                   Kfld                    LblNsp
082900040518
083000040518     c     kLblmf        Klist
083100040518     c                   Kfld                    LblAap
083200040518     c                   Kfld                    LblLpp
083300040518     c                   Kfld                    LblNrp
083400040518     c                   Kfld                    LblNsp
083500040518     c                   Kfld                    LblAan
083600040518     c                   Kfld                    LblLpn
083700040518     c                   Kfld                    LblNrn
083800040518     c                   Kfld                    LblNsn
083900040715
084000040715     c     ktab          klist
084100040715     c                   kfld                    codut
084200040715     c                   kfld                    tblcod
084300040715     c                   kfld                    tblkey
084400041118
084500041118     c     ktab01        klist
084600041118     c                   kfld                    codut
084700041118     c                   kfld                    tblcod
084800040216
084900040217      * reperisco data e ora
085000040217     c                   time                    w0140
085100040217      * udate in GGMMAAAA
085200040217     c                   Move      w0140         wdate
085300040217      * ora
085400040217     c                   movel     w0140         wora
085500040217      * Data consegna merce dal
085600040217     c                   Clear                   wlbdat
085700040217     c                   Z-add     PaDcd         g02inv
085800040217     c                   Movel     '3'           g02err
085900040217     c                   Call      'XSRDA8'
086000040217     c                   Parm                    wlbdat
086100040217     c                   Z-add     g02dat        DatDcd
086200040217      * Data consegna merce al
086300040217     c                   Clear                   wlbdat
086400040217     c                   Z-add     PaDca         g02inv
086500040217     c                   Movel     '3'           g02err
086600040217     c                   Call      'XSRDA8'
086700040217     c                   Parm                    wlbdat
086800040217     c                   Z-add     g02dat        DatDca
086900040216
087000040216     c                   EndSr
087100040218
087200040218     oQsysprt   e            Testa          1 02
087300040218     o                                        +  20 'ELENCO SPEDIZIONI SCARTATE'
087400040218     o                                        +   1 'MA CON IMMAGINE LDV'
087500040218     o                                        +   1 'dal'
087600040218     o                       datdcd           +   1 '  /  /    '
087700040218     o                                        +   1 'al'
087800040218     o                       datdca           +   1 '  /  /    '
087900040218     o                                          110 'TNSB12R'
088000040218     o                                          128 'PAG.'
088100040218     o                       Page          Z    132
088200040218     o          e            Testa          2
088300040218     o                       Knsif            +   1
088400040218     o                       Knmus            +   1
088500040218     o                       wdate              110 '  /  /    '
088600040218     o                       wora               132 '  :  :  '
088700040218     o          e            Testa          2
088800040218     o                                        +   4 'Spedizione'
088900040218     o                                        +   7 'Cod.bolla'
089000040218     o                                        +   2 'Anomalia'
089100040218     o                                        +   2 'Autotrasportatore'
089200040218     o          e            Bolla       1
089300040218     o                       TasLnp
089400040218     O                       TasNrs        Z  +   1
089500040218     O                       TasNsp        Z  +   1
089600040218     O                       TasAas           +   1
089700040218     O                       TasCbo           +   5
089800040218     O                       TasCca           +   9
089900040218     O                       TasPdc           +   6
090000040630** cmd
090100041021OVRDBF FILE(TITAS26C) TOFILE(
090200040630OVRDBF FILE(TITAA30C) TOFILE(
090300040715CLRPFM FILE(WFLDV00F)
090400110113OVRDBF FILE(FIDST01L) TOFILE(
090500040827OVRDBF FILE(FNLBL02L) TOFILE(
090600060814CLRPFM FILE(WFLDVD0F)
