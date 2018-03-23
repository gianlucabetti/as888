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
010800101229
010900101229     d ddstflr       e ds
011000040216
011100040216     d Kpjba         e ds
011200040216
011300040216      *------------------------------------------------------------------------*
011400040216
011500040218      * Ciclo per lna
011600040218do  1c                   Do        250           xx
011700040218     c                   If        SkLna(xx) = *Zeros
011800040218     c                   Leave
011900040218     c                   EndIf
012000040630      * Recupero la data inizio scannerizzazione
012100110113      * e se filiale attiva con PDA consegne
012200040630     c                   Clear                   Og146
012300110113     c                   Clear                   Og148
012400040630     c     SkLna(xx)     Chain     Azorg01l
012500040630     c                   If        %Found(Azorg01l)
012600040630     c                   Movel     OrgDe6        Og146
012700110113     c                   Movel     OrgDe8        Og148
012800040630     c                   EndIf
012900040630     c                   Move      §OgDse        wdse
013000040630     c                   Move      §OgDsd        wdsd
013100040630      * Se non ha ancora iniziato a scannerizzazione leggo un'altra lna
013200040630     c                   If        wdse = *Zeros and PaEEX = *Blanks
013300040630     c                                           and PaDPD = *Blanks
013400040630     c                   Iter
013500040630     c                   EndIf
013600040715      * se richiesta la generazione del file
013700040715if  2c                   If        PaFILE = 'S'
013800040715      * pulisco il rcd
013900040715     c                   Clear                   Wfldv000
014000040715      * recupero la descrizione della lna e la lna
014100040715     c                   Eval      LdvdLna = OrgDes
014200040715     c                   Eval      LdvLna = Sklna(xx)
014300040715      * recupero il distretto e l'area
014400040715     c                   Eval      LdvDis = OrgFl3
014500040715     c                   Eval      LdvAre = OrgCar
014600040715      * e le relative descrizioni
014700040715      * distretto
014800040715     c                   Eval      TblCod = '17'
014900040715     c                   Eval      TblKey = LdvDis
015000040715     c     ktab          Chain     Tabel00f
015100040715     c                   If        %Found(Tabel00f)
015200040715     c                   Movel     TblUni        LdvdDis
015300040715     c                   EndIf
015400040715      * area
015500040715     c                   Eval      TblCod = '05'
015600040715     c                   Movel     LdvAre        TblKey
015700040715     c     ktab          Chain     Tabel00f
015800040715     c                   If        %Found(Tabel00f)
015900040715     c                   Movel     TblUni        LdvdAre
016000040715     c                   EndIf
016100040715e   2c                   EndIf
016200040218      * Ciclo per dcm
016300040218do  2c                   Do        31            yy
016400040218     c                   If        SkDcm(yy) = *Zeros
016500040218     c                   Leave
016600040218     c                   EndIf
016700040630      * Se dcm è minore dell'inizio scannerizzazione leggo un'altra data
016800040630     c                   If        SkDcm(yy) < wdse and PaEEX = *Blanks
016900040630     c                                              and PaDPD = *Blanks
017000040630     c                   Iter
017100040630     c                   EndIf
017200040715      * se richiesta la generazione del file
017300040715if  3c                   If        PaFILE = 'S'
017400040715      * pulisco i totali
017500040715     c                   Clear                   LdvTote
017600040715     c                   Clear                   LdvTotn
017700040715      * recupero la data consegna
017800040715     c                   Eval      LdvDcm = SkDcm(yy)
017900040715e   3c                   EndIF
018000040217      * Leggo TITAS
018100041021     c     kTas          Setll     Titas26c
018200040218do  3c                   Do        *Hival
018300041021     c     kTas          Reade     Titas26c
018400041021if  4c                   If        %Eof(Titas26c)
018500040216     c                   Leave
018600040218    4c                   EndIf
018700040218
018800040218     c                   Movel     TasFlo        dtasflo
018900040218     c                   Eval      *In11 = (§floiml <> *Blanks)
019000120913
019100120913      * RICHIESTA: Cliente
019200120913    4c                   IF        *in12  and  TASccm <> PaKsc
019300120913     c                   iter
019400120913     c                   ENDIF
019500040310
019600040310      * RICHIESTA: lnp solo EEX (+ cliente TIMBERLAND)
019700040310    4C                   IF        paEEX = 'S'
019800040310     C                   eval      *in30 = (TASKSC <> CLITIM)
019900040311     c   30TasLnp        Lookup    SkEEX                                  29
020000040311     C   30              eval      *in30 = (*in29 = *OFF)
020100040310     c   30              Iter
020200040310    4c                   EndIf
020300040630
020400040630      * RICHIESTA: lnp solo DPD
020500040630    4c                   If        paDPD = 'S'
020600040630     c     TasLnp        Lookup    SkLnpd                                 30
020700040630     c  n30              Movel     TasCcm        wpoccm
020800040630     c  n30wpoccm        Lookup    SkLnpd                                 30
020900040630     c                   If        not *In30
021000040630     c                   Iter
021100040630     c                   EndIf
021200040630   x4c                   Else
021300040630      * Linea di partenza DPD o p.o. del cliente mittente DPD
021400040630      * escludo se data scannerizzazione a 0 o se dcm minore
021500040630     c     TasLnp        Lookup    SkLnpd                                 30
021600040630     c  n30              Movel     TasCcm        wpoccm
021700040630     c  n30wpoccm        Lookup    SkLnpd                                 30
021800040630     c                   If        *In30 and (wdsd = *Zeros or
021900040630     c                                        SkDcm(yy) < wdsd)
022000040630     c                   Iter
022100040630     c                   EndIf
022200040630    4c                   EndIf
022300040218
022400040519      * Linea di partenza poste escludo
022500040519     c     TasLnp        Lookup    SkLnpp                                 30
022600151103     c                   If        *In30
022700040727     c                   Iter
022800040727     c                   EndIf
022900040310
023000040217      * Tipo bolla
023100041118     c     TasCbo        Lookup    skcbo                                  30
023200041118     c                   If        *In30
023300040218     c   11
023400040218     can 10              ExSr      StampaEdp
023500040217     c                   Iter
023600040218    4c                   EndIf
023700060405      * Codice bolla 'M' prepagati annullati
023800060405if  4c                   if        tascbo = 'M '
023900060405     c   11
024000060405     can 10              ExSr      StampaEdp
024100060405     c                   Iter
024200060405    4c                   EndIf
024300041227      * Anomalia fino al 10/01/05
024400041227     c                   If        TasDsp < 20050110 and
024500041227if  4c                             (TasCca = '1' or TasCca = '2' or
024600041227     c                             TasCca = '3' or TasCca = '5' or
024700041227     c                             TasCca = '6' or TasCca = '7' or
024800041227     c                             TasCca = '9' or TasCca = 'S' or
024900041227     c                             TasCca = 'D' or TasCca = 'E' or
025000041227     c                             TasCca = 'F' or TasCca = 'G' or
025100041227     c                             TasCca = 'H' or TasCca = 'L' or
025200041227     c                             TasCca = 'R')
025300041227     c   11
025400041227     can 10              ExSr      StampaEdp
025500041227     c                   Iter
025600041227    4c                   EndIf
025700041227      * Anomalia dal 10/01/05
025800041227     c                   If        TasDsp >= 20050110 and
025900041227if  4c                             (TasCca = '1' or TasCca = '2' or
026000041221     c                             TasCca = '7' or TasCca = '9' or
026100040820     c                             TasCca = 'D' or TasCca = 'E' or
026200040820     c                             TasCca = 'F' or TasCca = 'G' or
026300040820     c                             TasCca = 'H' or TasCca = 'L' or
026400130530     c****                         TasCca = 'R')
026500130530     c                             TasCca = 'R' or TAScca = 'S')
026600040218     c   11
026700040218     can 10              ExSr      StampaEdp
026800040217     c                   Iter
026900040218    4c                   EndIf
027000040513      * Bolla mamma
027100040518     c                   Eval      kLblAa = TasAas
027200040518     c                   Eval      kLblLp = TasLnp
027300040518     c                   Eval      kLblNr = TasNrs
027400040518     c                   Eval      kLblNs = TasNsp
027500040518     c     kLbl          Setll     Fnlbl02l
027600040513     c                   If        %Equal(Fnlbl02l)
027700040513     c                   Iter
027800040513     c                   EndIf
027900040518      * Controllo se è una figlia
028000040518     c                   Clear                   conta
028100040518     c     kLbl          Chain     Fnlbl01l
028200040518     c                   If        %Found(Fnlbl01l)
028300040518      * Controllo se la mamma ha generato una figlia successiva
028400040518     c     kLblmf        Setll     Fnlbl02l
028500040518     c                   Do        *Hival
028600040518     c     kLblm         Reade     Fnlbl02l
028700040518     c                   If        %Eof(Fnlbl02l)
028800040518     c                   Leave
028900040518     c                   EndIf
029000040518     c                   Add       1             conta
029100040518     c                   EndDo
029200040518      * se ho contato almeno 2 bolle con la stessa mamma vuol dire che la mamma ha generato una
029300040518      * figlia successiva alla figlia che sto leggendo
029400040518     c                   If        conta > 1
029500040518     c                   Iter
029600040518     c                   EndIf
029700040518     c                   EndIf
029800040518
029900040218      * Autotrasportatore
030000040218     c                   Move      TasPdc        w0040
030100040218if  4c                   If        w0040 = 9876 or TasPdc = 0010103
030200040218     c                   Iter
030300040218    4c                   EndIf
030400040715      * se richiesta la generazione del file
030500040715if  4c                   If        PaFILE = 'S'
030600040715      * sommo le bolle elaborate
030700040715     c                   add       1             LdvTote
030800040715e   4c                   EndIf
030900040217      * Flag immagine
031000040218     c   11              Iter
031100040226
031200040226      * Conto quante bolle
031300040226     c                   Add       1             TotDcm
031400040715      * se richiesta la generazione del file
031500040715if  4c                   If        PaFILE = 'S'
031600040715      * sommo le bolle non scannerizzate
031700040715     c                   add       1             LdvTotn
031800040715e   4c                   EndIf
031900040218
032000060814      * se richiesta la generazione del file dettaglio
032100060814      * scrivo Wfldvd0f
032200060814if  4c                   If        PaFILEd = 'S'
032300060814     c                   ExSr      file_det
032400060814e   4c                   EndIf
032500060814
032600040217      * Stampo
032700040217     c                   ExSr      Stampa
032800040217
032900040218    3c                   EndDo
033000040226
033100040226      * Salto pagina x overflow
033200040226if  3c                   If        TotDcm > *Zeros
033300040226if  4c                   If        *In99
033400040226     c                   Write     Sb12T
033500040226     c                   Write     Sb12T1
033600040226     c                   Write     Sb12T2
033700040226     c                   Eval      *In99 = *Off
033800040226    4c                   EndIf
033900040226      * Stampo il totale per data consegna merce
034000040226     c                   Write     Sb12TDcm
034100040226      * Totale linea arrivo
034200040226     c                   Add       TotDcm        TotLna
034300040226     c                   Clear                   TotDcm
034400040226    3c                   EndIf
034500040715      * se richiesta la generazione del file
034600040715if  4c                   If        PaFILE = 'S'
034700040715      * lo scrivo se ho elaborato qualcosa
034800040715     c                   If        LdvTote > 0
034900040715     c                   Write     Wfldv000
035000040715     c                   EndIf
035100040715e   4c                   EndIf
035200040218
035300040218    2c                   EndDo
035400040226
035500040226      * Salto pagina x overflow
035600040226if  2c                   If        TotLna > *Zeros
035700040226if  3c                   If        *In99
035800040226     c                   Write     Sb12T
035900040226     c                   Write     Sb12T1
036000040226     c                   Write     Sb12T2
036100040226     c                   Eval      *In99 = *Off
036200040226    3c                   EndIf
036300040226      * Stampo il totale per linea di arrivo
036400040226     c                   Write     Sb12TLna
036500040226      * Totale finale
036600040226     c                   Add       TotLna        TotFin
036700040226     c                   Clear                   TotLna
036800040226    2c                   EndIf
036900040217
037000040218    1c                   EndDo
037100040226
037200040226      * Salto pagina x overflow
037300040226if  1c                   If        *In99
037400040226     c                   Write     Sb12T
037500040226     c                   Write     Sb12T1
037600040226     c                   Write     Sb12T2
037700040226     c                   Eval      *In99 = *Off
037800040226    1c                   EndIf
037900040226
038000040226      * Stampo il totale finale
038100040226     c                   If        TotFin > *Zeros
038200040226     c                   Write     Sb12TFin
038300040226     c                   EndIf
038400040218
038500040217      * Fine stampa
038600040217     c                   If        wokstp = *On
038700040217     c                   Write     Sb12e
038800040217     c                   EndIf
038900090720
039000090720      * Stampa per nessuna LDV da scannerizzare
039100090720     c                   If        wokstp = *off
039200090720     c                   Write     Sb12T
039300090720      * Decodifico la lna
039400090720     c                   If        PaLna > *Zeros
039500090720     c                   Eval      CodLna = PaLna
039600090720     c                   Else
039700090720     c                   Eval      CodLna = SkLna(01)
039800090720     c                   EndIf
039900090720     c     CodLna        Chain     Azorg01l
040000090720     c                   If        %Found(Azorg01l)
040100090720     c                   Movel     OrgDes        DesLna
040200090720     c                   EndIf
040300090720     c                   Write     Sb12T1
040400090720     c                   Write     Sb12Fine
040500090720     c                   endif
040600040216
040700040216     c                   Eval      *InLr = *On
040800040216
040900040217      *------------------------------------------------------------------------*
041000060814      * SCRIVO FILE DETTAGLIO
041100040217      *------------------------------------------------------------------------*
041200060814     c     File_det      BegSr
041300040217
041400060814     c                   z-add     TASaas        dLDVaas
041500060814     c                   z-add     TASlnp        dLDVlnp
041600060814     c                   z-add     TASnrs        dLDVnrs
041700060814     c                   z-add     TASnsp        dLDVnsp
041800060814     c                   z-add     TASlna        dLDVlna
041900060814     c                   z-add     TASdcm        dLDVdcm
042000060814
042100060814      * Verifica se p.o. mittente è DPD
042200060814     c                   movel     TASccm        W0030
042300060814     c     W0030         lookup    SkLnpd                                 21
042400060814     c                   If        *in21 = *on
042500060814     c                   movel     'S'           dLDVdpd
042600060814     c                   Else
042700060814     c                   movel     *blank        dLDVdpd
042800060814     c                   Endif
042900060814      *
043000060814     C                   write     WFldvd00
043100060814      *
043200060814     c                   EndSr
043300060814
043400060814      *------------------------------------------------------------------------*
043500060814      * STAMPA
043600060814      *------------------------------------------------------------------------*
043700060814     c     Stampa        BegSr
043800060814
043900040217      * Stampo la testata per la prima volta
044000040217if  1c                   If        wokstp = *Off
044100040217     c                   Write     Sb12T
044200040217      * Decodifico la lna richiesta
044300040310if  2c                   If        PaLna > *Zeros
044400040217     c                   Eval      CodLna = PaLna
044500040217   x2c                   Else
044600040217      * Decodifico la prima lna
044700040218     c                   Eval      CodLna = SkLna(xx)
044800040217    2c                   EndIf
044900040217     c     CodLna        Chain     Azorg01l
045000040217if  2c                   If        %Found(Azorg01l)
045100040217     c                   Movel     OrgDes        DesLna
045200040217    2c                   EndIf
045300040217     c                   Write     Sb12T1
045400040217     c                   Eval      wokstp = *On
045500040217     c                   Write     Sb12T2
045600040218     c                   Eval      OldLna = CodLna
045700040217    1c                   EndIf
045800040218      * Se richieste tutte le linee devo saltare pagina a cambio lna
045900040310if  1c                   If        (PaLna = *Zeros) and
046000040218     c                              OldLna > *Zeros and SkLna(xx) <> OldLna
046100040218     c                   Write     Sb12T
046200040218     c                   Eval      CodLna = SkLna(xx)
046300040218     c     CodLna        Chain     Azorg01l
046400040218if  2c                   If        %Found(Azorg01l)
046500040218     c                   Movel     OrgDes        DesLna
046600040218    2c                   EndIf
046700040218     c                   Write     Sb12T1
046800040218     c                   Write     Sb12T2
046900040218     c                   Eval      OldLna = SkLna(xx)
047000040226     c                   Eval      *In99 = *Off
047100040218    1c                   EndIf
047200040217
047300040217      * Salto pagina x overflow
047400040217if  1c                   If        *In99
047500040217     c                   Write     Sb12T
047600040217     c                   Write     Sb12T1
047700040217     c                   Write     Sb12T2
047800040217     c                   Eval      *In99 = *Off
047900040217    1c                   EndIf
048000040218
048100040218      * Giro data spedizione
048200040218     c                   Clear                   wlbdat
048300040218     c                   Z-add     TasDsp        g02inv
048400040218     c                   Movel     '3'           g02err
048500040218     c                   Call      'XSRDA8'
048600040218     c                   Parm                    wlbdat
048700040218     c                   Z-add     g02dat        DatDsp
048800040217
048900040217      * Mittente
049000040217     c                   Move      TasCcm        w0040
049100040217      * non codificato vado su Titaa
049200040217if  1c                   If        w0040 = *Zeros or w0040 = 8888
049300040217     c     kTaa          Chain     Titaa30c
049400040217if  2c                   If        %Found(Titaa30c)
049500040217     c                   Movel     TaaRsc        RscMit
049600040217    2c                   EndIF
049700040217      * codificato vado su Cnaco
049800040217   x1c                   Else
049900040217     c                   Clear                   Tibs69ds
050000040217     c                   Z-add     TasCcm        I69kac
050100040217     c                   call      'TIBS69R'
050200040217     c                   parm                    tibs69DS
050300040217     c                   parm                    ds_cnaco
050400040217     c                   parm                    ds_cnind
050500040217     c                   parm                    ds_cnclp
050600040217     c                   parm                    ds_fncls
050700040217if  2c                   If        O69err <> '1'
050800040217     c                   Movel     AcoRag        RscMit
050900040217    2c                   EndIf
051000040217    1c                   EndIf
051100040217      * Giro data consegna merce
051200040217     c                   Clear                   wlbdat
051300040217     c                   Z-add     TasDcm        g02inv
051400040217     c                   Movel     '3'           g02err
051500040217     c                   Call      'XSRDA8'
051600040217     c                   Parm                    wlbdat
051700040217     c                   Z-add     g02dat        DatDcm
051800040217      * Giro data distinta
051900040217     c                   Clear                   wlbdat
052000040217     c                   Z-add     TasDdc        g02inv
052100040217     c                   Movel     '3'           g02err
052200040217     c                   Call      'XSRDA8'
052300040217     c                   Parm                    wlbdat
052400040217     c                   Z-add     g02dat        DatDdc
052500101229      * Controllo se distinta a PDA in BUONA
052600110113      * su TITAS campo lungo 7 su FIDST campo lungo 6
052700110113      * se valore supera 999999 non provo a cercare la distinta
052800110113     c                   IF        §ogpdacon <> *blanks
052900110113     c                             and  tasndc <= 999999
053000101229     c                   clear                   ddstflr
053100101229     c                   clear                   flgpda
053200101229     c                   eval       dstnfv = tasndc
053300101229     c                   eval       dstfgs =
053400101229     c                              %dec(%subst(%editc(taspdc:'X'):1:3):3:0)
053500101229     c                   eval       dstnpg = 4
053600101229     c     kfidst        chain     fidst01l
053700101229    5c                   if        %found(fidst01l)
053800101229     c                   eval      ddstflr = dstflr
053900101229      * attiva su PDA e non in test
054000101229    6c                   if        (dstpda = 'C' or dstpda = 'E') and
054100101229      * in TEST
054200101229     c                              §dsttstpda <> 'C' and §dsttstpda <> 'E'
054300101229     c                   eval      flgpda = 'P'
054400101229     c                   endif
054500101229     c                   endif
054600110113     c                   ENDIF
054700040217      * Decodifico autotrasportatore
054800040217     c     kApd          Chain     Fiapd01l
054900040217if  1c                   If        %Found(Fiapd01l)
055000040217     c                   Movel     ApdRsc        DesPdc
055100040217    1c                   EndIf
055200040217
055300040217      * Dettaglio
055400040217     c                   Write     Sb12R
055500040217
055600040217     c                   EndSr
055700040218
055800040218      *------------------------------------------------------------------------*
055900040218      * STAMPA x EDP
056000040218      *------------------------------------------------------------------------*
056100040218     c     StampaEdp     BegSr
056200040218
056300040218     c                   If        *In98 = *Off  or *InOf
056400040218     c                   Except    Testa
056500040218     c  n98              Eval      *In98 = *On
056600040218     c                   EndIf
056700040218
056800040218     c                   Except    Bolla
056900040218
057000040218     c                   EndSr
057100040216
057200040216      *------------------------------------------------------------------------*
057300040216      * ROUTINE INIZIALE
057400040216      *------------------------------------------------------------------------*
057500040216     c     *Inzsr        BegSr
057600040216
057700040216     c     *entry        plist
057800040216     c                   parm                    Kpjba
057900040216     c                   Movel(p)  Kpjbu         Param
058000040702      * Imposto libreria - Se sono in ambiente di prova - GRPS
058100040702     c                   If        %subst(knsif:7:1) = 'P'
058200040702     c                   Eval      wlib = 'GAITRAGRPS'
058300101229     c                   eval      wlibfil = 'FILTRAPRD '
058400040702      *                  - Se sono in ambiente buono - GRU
058500040630     c                   Else
058600040702     c                   Eval      wlib = 'GAITRAGRU '
058700101229     c                   eval      wlibfil = 'FILTRA201 '
058800040630     c                   EndIf
058900040630      * OVRDBF su TITAS e TITAA
059000040630     c                   Clear                   comman
059100040630     c                   Movel(p)  cmd(1)        comman
059200040630     c                   Eval      %Subst(comman:30:10) = wlib
059300040630     c                   Eval      comman =
059400041021     c                             %trim(comman) + '/TITAS26C)'
059500040630     c                   Call      'QCMDEXC'                            99
059600040630     c                   Parm                    comman
059700040630     c                   Parm                    lenght
059800041021     c  n99              Open      Titas26c
059900040630     c                   Movel(p)  cmd(2)        comman
060000040630     c                   Eval      %Subst(comman:30:10) = wlib
060100040630     c                   Eval      comman =
060200040630     c                             %trim(comman) + '/TITAA30C)'
060300040630     c                   Call      'QCMDEXC'                            99
060400040630     c                   Parm                    comman
060500040630     c                   Parm                    lenght
060600040630     c  n99              Open      Titaa30c
060700101229
060800101229      * OVRDBF su FIDST
060900101229     c                   Clear                   comman
061000101229     c                   Movel(p)  cmd(4)        comman
061100101229     c                   Eval      %Subst(comman:30:10) = wlibfil
061200101229     c                   Eval      comman =
061300110113     c                             %trim(comman) + '/FIDST01L)'
061400101229     c                   Call      'QCMDEXC'                            99
061500101229     c                   Parm                    comman
061600101229     c                   Parm                    lenght
061700101229     c  n99              Open      FIDST01L
061800040715
061900040715      * Se richiesta la generazione del file di totali
062000040715     c                   If        PaFILE = 'S'
062100040715      * prima lo pulisco
062200040715     c                   Clear                   comman
062300040715     c                   Movel(p)  cmd(3)        comman
062400040715     c                   Call      'QCMDEXC'
062500040715     c                   Parm                    comman
062600040715     c                   Parm                    lenght
062700040715      * poi lo apro
062800040715     c                   Open      Wfldv00f
062900040715     c                   EndIf
063000040218
063100060814      * Se richiesta la generazione del file di dettaglio
063200060814     c                   If        PaFILEd = 'S'
063300060814      * prima lo pulisco
063400060814     c                   Clear                   comman
063500060814     c                   Movel(p)  cmd(6)        comman
063600060814     c                   Call      'QCMDEXC'
063700060814     c                   Parm                    comman
063800060814     c                   Parm                    lenght
063900060814      * poi lo apro
064000060814     c                   Open      Wfldvd0f
064100060814     c                   EndIf
064200060814
064300040218      * Utente EDP
064400040218     c                   If        %subst(Knmus:1:3) = 'EDP'
064500040218     c                   Eval      *In10 = *On
064600040218     c                   Else
064700040218     c                   Eval      *In10 = *Off
064800040218     c                   EndIF
064900040216
065000040216     c                   Clear                   Lin
065100040216     c                   Clear                   SkLna
065200040216     c                   Clear                   xx
065300040311     c                   Clear                   jj
065400040519     c                   Clear                   wwp
065500040519     c                   Clear                   wwd
065600040216     c                   Eval      yy = 1
065700040216
065800040311      * Leggo AZORG x caricare delle skiere di lavoro
065900040311      * SKLNA contiene:
066000040311      *      i p.o. validi con scannerizzazione attiva tranne DPD e PPT, nel
066100040630      *      caso che PaEEX = 'S' o PaDPD = 'S' contiene tutti i p.o. tranne DPD e PPT
066200040311      *      CARICO SKLNA SOLO SE PALNA = 0
066300040630      * SKEEX  contiene:
066400040311      *      i p.o. EEX
066500040630      * SKLNPD contiene:
066600040630      *      i p.o. DPD
066700040630      * SKLNPP contiene:
066800040630      *      i p.o. PPT
066900040311
067000040311     c     *Loval        Setll     Azorg01l
067100040311     c                   Read      Azorg01l
067200040311do  1c                   DOW       NOT %Eof(Azorg01l)
067300040311if  2c                   IF        OrgFva = *Blanks  AND
067400040311     c                             (OrgFag = 'F' or OrgFag = 'A')
067500040311     c                   Movel     OrgDe3        Og143
067600040311     c                   Movel     OrgDe6        Og146
067700040311      *
067800040311if  3c                   If        §OgNtw = 'PPT' or  §OgNtw = 'DPD'
067900040519if  4c                   If        §OgNtw = 'PPT'
068000040519     c                   Add       1             wwp
068100040519     c                   Eval      SkLnpp(wwp) = OrgFil
068200040519    4c                   Endif
068300040519if  4c                   If        §OgNtw = 'DPD'
068400040519     c                   Add       1             wwd
068500040519     c                   Eval      SkLnpd(wwd) = OrgFil
068600040519    4c                   Endif
068700040630   x3c                   Else
068800040311      *
068900040311if  4c                   If        §OgNtw = 'EEX'
069000040311     c                   Add       1             jj
069100040311     c                   Eval      SkEEX(jj) = OrgFil
069200040311    4c                   EndIF
069300040311if  4c                   If        Palna = *zeros  AND
069400040630     c                             (PaEEX = 'S'  or PaDPD = 'S' or
069500040311     c                             (§OgDse <> *Blanks and §OgDse <> *Zeros))
069600040311     c                   Add       1             xx
069700040311     c                   Eval      SkLna(xx) = OrgFil
069800040311    4c                   EndIf
069900040311    3c                   EndIf
070000040311      *
070100040311    2c                   EndIf
070200040311     c                   Read      Azorg01l
070300040311    1c                   EndDo
070400040630
070500040310      * Se la linea di arrivo è <> zero carico la sk con la lna richiesta
070600040218      * più i p.o. gestiti
070700040310if  1c                   If        PaLna > *Zeros
070800040216     c                   Eval      SkLna(1) = PaLna
070900040216      * Carico £6 della linea di arrivo richiesta
071000040216     c                   Clear                   Trul06ds
071100040216     c                   Eval      d06cod = '£6'
071200040216     c                   Movel     PaLna         d06key
071300040216     c                   Movel(p)  Trul06ds      Kpjbu
071400040216     c                   Call      'TRUL06R'
071500040216     c                   Parm                    Kpjba
071600040216     c                   Movel     Kpjbu         Trul06ds
071700040216do  2c                   Do        30            xx
071800040216if  3c                   If        Lin(xx) = *Zeros
071900040216     c                   Leave
072000040216    3c                   EndIf
072100040216if  3c                   If        Lin(xx) <> PaLna
072200040216     c                   Add       1             yy
072300040216     c                   Eval      SkLna(yy) = Lin(xx)
072400040216    3c                   EndIf
072500040216    2c                   EndDo
072600040217      * Ordino la sk
072700040217     c                   Movea     SkLna         wSkLna
072800040217     c                   Clear                   SkLna
072900040217     c                   Sorta     wSkLna
073000040217     c                   Clear                   yy
073100040217do  2c                   Do        250           xx
073200040217if  3c                   If        wSkLna(xx) > *Zeros
073300040217     c                   Add       1             yy
073400040217     c                   Eval      SkLna(yy) = wSkLna(xx)
073500040217    3c                   EndIf
073600040217    2c                   EndDo
073700040216    1c                   EndIf
073800040218
073900040218      * Carico una sk con le date da elaborare
074000040218     c                   Clear                   SkDcm
074100040218     c                   Eval      xx = 1
074200040218     c                   Eval      SkDcm(xx) = PaDcd
074300040218if  1c                   If        PaDcd <> PaDca
074400040218     c                   Eval      wdata = PaDcd
074500040218     c     *Iso          Movel     PaDcd         wdataiso
074600040218do  2c                   Dow       wdata < PaDca
074700040218     c                   Eval      xx = xx + 1
074800040218     c                   Adddur    1:*d          wdataiso
074900040218     c                   Move      wdataiso      wdata
075000040218     c                   Eval      SkDcm(xx) = wdata
075100040218    2c                   EndDo
075200040218    1c                   EndIf
075300041118
075400041118      * Carico uns sk con i tipi bolla da scartare
075500041118     c                   Clear                   skcbo
075600041118     c                   Clear                   xx
075700041118     c                   Eval      TblCod = '3A'
075800041118     c     kTab01        Setll     Tabel00f
075900041118     c                   Do        *Hival
076000041118     c     kTab01        Reade     Tabel00f
076100041118     c                   If        %Eof(Tabel00f)
076200041118     c                   Leave
076300041118     c                   EndIf
076400041118     c                   If        TblFlg <> *Blanks
076500041118     c                   Iter
076600041118     c                   EndIf
076700041118     c                   Eval      ds3a = TblUni
076800041118     c                   If        §3aTb1 = 'F3' or §3aTb1 = 'A3'
076900080707     c                             or §3atb1 = 'AR'
077000041118     c                   Eval      xx = xx + 1
077100041118     c                   Movel     TblKey        skcbo(xx)
077200041118     c                   EndIf
077300041118     c                   EndDo
077400120912
077500120912       //?Controllo se è stato richiesto un cliente
077600120912     c                   eval      *in12 = (PaKsc > 0)
077700120913     c   12              eval      CodKsc = PaKsc
077800120913     c   12              eval      DesKsc = PaRag
077900040216
078000040216     c     kTas          Klist
078100040218     c                   Kfld                    SkDcm(yy)
078200040218     c                   Kfld                    SkLna(xx)
078300040217
078400040217     c     kTaa          Klist
078500040217     c                   Kfld                    TasAas
078600040217     c                   Kfld                    TasLnp
078700040217     c                   Kfld                    TasNrs
078800040217     c                   Kfld                    TasNsp
078900040217     c                   Kfld                    kTaaTrc
079000101229
079100101229     c     kfidst        klist
079200101229     c                   Kfld                    dstnpg
079300101229     c                   kfld                    dstnfv
079400101229     c                   kfld                    dstfgs
079500040217
079600040217     c     kApd          Klist
079700040217     c                   Kfld                    kApdTip
079800040217     c                   Kfld                    TasPdc
079900040513
080000040518     c     kLbl          Klist
080100040518     c                   Kfld                    kLblAa
080200040518     c                   Kfld                    kLblLp
080300040518     c                   Kfld                    kLblNr
080400040518     c                   Kfld                    kLblNs
080500040518
080600040518     c     kLblm         Klist
080700040518     c                   Kfld                    LblAap
080800040518     c                   Kfld                    LblLpp
080900040518     c                   Kfld                    LblNrp
081000040518     c                   Kfld                    LblNsp
081100040518
081200040518     c     kLblmf        Klist
081300040518     c                   Kfld                    LblAap
081400040518     c                   Kfld                    LblLpp
081500040518     c                   Kfld                    LblNrp
081600040518     c                   Kfld                    LblNsp
081700040518     c                   Kfld                    LblAan
081800040518     c                   Kfld                    LblLpn
081900040518     c                   Kfld                    LblNrn
082000040518     c                   Kfld                    LblNsn
082100040715
082200040715     c     ktab          klist
082300040715     c                   kfld                    codut
082400040715     c                   kfld                    tblcod
082500040715     c                   kfld                    tblkey
082600041118
082700041118     c     ktab01        klist
082800041118     c                   kfld                    codut
082900041118     c                   kfld                    tblcod
083000040216
083100040217      * reperisco data e ora
083200040217     c                   time                    w0140
083300040217      * udate in GGMMAAAA
083400040217     c                   Move      w0140         wdate
083500040217      * ora
083600040217     c                   movel     w0140         wora
083700040217      * Data consegna merce dal
083800040217     c                   Clear                   wlbdat
083900040217     c                   Z-add     PaDcd         g02inv
084000040217     c                   Movel     '3'           g02err
084100040217     c                   Call      'XSRDA8'
084200040217     c                   Parm                    wlbdat
084300040217     c                   Z-add     g02dat        DatDcd
084400040217      * Data consegna merce al
084500040217     c                   Clear                   wlbdat
084600040217     c                   Z-add     PaDca         g02inv
084700040217     c                   Movel     '3'           g02err
084800040217     c                   Call      'XSRDA8'
084900040217     c                   Parm                    wlbdat
085000040217     c                   Z-add     g02dat        DatDca
085100040216
085200040216     c                   EndSr
085300040218
085400040218     oQsysprt   e            Testa          1 02
085500040218     o                                        +  20 'ELENCO SPEDIZIONI SCARTATE'
085600040218     o                                        +   1 'MA CON IMMAGINE LDV'
085700040218     o                                        +   1 'dal'
085800040218     o                       datdcd           +   1 '  /  /    '
085900040218     o                                        +   1 'al'
086000040218     o                       datdca           +   1 '  /  /    '
086100040218     o                                          110 'TNSB12R'
086200040218     o                                          128 'PAG.'
086300040218     o                       Page          Z    132
086400040218     o          e            Testa          2
086500040218     o                       Knsif            +   1
086600040218     o                       Knmus            +   1
086700040218     o                       wdate              110 '  /  /    '
086800040218     o                       wora               132 '  :  :  '
086900040218     o          e            Testa          2
087000040218     o                                        +   4 'Spedizione'
087100040218     o                                        +   7 'Cod.bolla'
087200040218     o                                        +   2 'Anomalia'
087300040218     o                                        +   2 'Autotrasportatore'
087400040218     o          e            Bolla       1
087500040218     o                       TasLnp
087600040218     O                       TasNrs        Z  +   1
087700040218     O                       TasNsp        Z  +   1
087800040218     O                       TasAas           +   1
087900040218     O                       TasCbo           +   5
088000040218     O                       TasCca           +   9
088100040218     O                       TasPdc           +   6
088200040630** cmd
088300041021OVRDBF FILE(TITAS26C) TOFILE(
088400040630OVRDBF FILE(TITAA30C) TOFILE(
088500040715CLRPFM FILE(WFLDV00F)
088600110113OVRDBF FILE(FIDST01L) TOFILE(
088700040827OVRDBF FILE(FNLBL02L) TOFILE(
088800060814CLRPFM FILE(WFLDVD0F)
