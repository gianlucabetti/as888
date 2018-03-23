000100050727      *===============================================================*
000200051026      *?FIDN88R * Stampa richiesta recupero C.A.                     ?*
000300050727      *===============================================================*
000400050727
000500050727     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600050727
000700050727      *---------------------------------------------------------------*
000800050727
000900050727     fFIDN86D   cf   e             workstn
001000050728     f                                     include(FI86S02)
001100050728     f                                     include(FI86C02)
001200050727     f                                     sfile(FI86S02:S02nrr)
001300050728     f                                     infds(dsFMT)
001400050727      *
001500050728     fFNDET01L  Uf   e           k disk
001600050728     fFNDRA01L  Uf a e           k disk
001700050727      *
001800050727     fFNDCT01L  if   e           k disk
001900050727      *
002000050727     fTITAS30C  if   e           k disk
002100050727     fFNDKA01L  if   e           k disk
002200050727     fFNSPE01L  if   e           k disk
002300050727     fTNTMD01L  if   e           k disk
002400050727      *
002500050727     fAZORG01L  if   e           k disk
002600050727     fTABEL00F  if   e           k disk
002700050727      *
002800050728     fFIDN88P   o    e             printer oflind(*in01)
002900050727     f                                     infds(PrtfInf)
003000050727
003100050727      *---------------------------------------------------------------*
003200050727
003300050728      *
003400050728      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003500050728      *
003600050727      *
003700050727      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003800050727      *
003900050727      *
004000050727      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004100050727      *
004200050727     d KPJBA         e ds
004300050727      *
004400050727      * - Parametri x Controllo profilo utenti
004500050727     d TIBS34ds      e ds                  inz
004600050727      * - Ds di riferimento al file esterno AZUTE00F
004700050727     d AZUTEds       e ds                  extname(AZUTE00F)
004800050727      * - Ds per dati organigramma
004900050727     d DDatiUte      e ds
005000050727      *
005100050727      * - Parametri x Interrogazione tabelle
005200050728     d TIBS02ds      e ds                  inz
005300050727     d   T02mod      e                     inz('C')
005400050727     d   T02cod      e                     inz('TAD')
005500050728      * - Tab. "GED/DN"
005600050728     d dGEDdn        e ds                  inz
005700050727      * - Tab. "STD/4"
005800050727     d dSTD4         e ds                  inz
005900050727      * - Tab. "TAD"
006000050727     d dTAD          e ds                  inz
006100050727      * - Tab. "TB"
006200050727     d dsTB          e ds                  inz
006300050727      *
006400050727     d TIBS69ds      e ds                  inz
006500050727     d ds_CNACO      e ds                  inz extname(CNACO00F)
006600050727     d ds_CNIND      e ds                  inz extname(CNIND00F)
006700050727     d ds_CNCLP      e ds                  inz extname(CNCLP00F)
006800050727     d ds_FNCLS      e ds                  inz extname(FNCLS00F)
006900050727      *
007000050727     d ds_TNTMD      e ds                  inz extname(TNTMD00F)
007100050727      *
007200050727     d dDCT01        e ds                  inz
007300050727      *
007400050727     d Status         sds           333
007500050727     d   SDSpgm          *proc
007600050727      *
007700050728     d dsFMT           ds           512
007800050727     d  £Tasto               369    369
007900050727     d  £Nrg                 370    370
008000050727     d  £Ncl                 371    371
008100050727     d  £SFLnrr              378    379B 0
008200050727      *
008300050727     d PrtfInf         ds
008400050727     d  Cur_Line             367    368I 0
008500050728      *
008600050728     d ds_Evento       ds                  inz
008700050728     d  H2Caae                             inz
008800050728     d  H2Cnev                             inz
008900050727      *
009000050727     d ds_NumCA        ds                  inz
009100050728     d  H2Caac                             inz
009200050728     d  S2Cfil                             inz
009300050728     d  S2Cnca                             inz
009400050727      *
009500050727      * - Controllo/Inversione date
009600050727     d WLBdat          ds                  inz
009700050727     d   G08dat                       8  0 inz
009800050727     d   G08inv                       8  0 inz
009900050727     d   G08err                       1    inz('3')
010000050727     d   G08tgi                       5  0 inz
010100050727      *
010200050727      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
010300050727      *
010400050727      * - flag
010500050727     d $First          s              1    inz(*on)
010600050727     d $Rivalsa        s              1    inz(*off)
010700050728     d $TIBS69R        s              1    inz(*off)
010800050728      * - indici di schiera
010900050727      * - Variabili riferite al data base o al display file
011000050728     d S02nrr          s                   like(C02rcd)   inz
011100050728     d w§DCTport       s                   like(§DCTport) inz
011200050728     d wCLI            s                   like(TASksc)   inz
011300050728     d wTAD            s                   like(DETtad)   inz
011400050728      * - Variabili definite internamente
011500050728     d wDate           s               d   inz  datfmt(*iso)
011600050728     d w0040a          s              4  0 inz
011700050728     d w0040b          s              4  0 inz
011800050727      *
011900050727      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
012000050727      *
012100050727      * - FNDET01L
012200050727     c     K02DET01      klist
012300050727     c                   kfld                    H2Caae
012400050727     c                   kfld                    H2Cnev
012500050727      * - FNDRA01L / FNDCL01L
012600050727     c     K03DRA01      klist
012700050727     c                   kfld                    H2Caac
012800050727     c                   kfld                    S2Cfil
012900050727     c                   kfld                    S2Cnca
013000050727      * - TITAS30C / TNTMD01L
013100050727     c     K04TAS30      klist
013200050727     c                   kfld                    H2Caas
013300050727     c                   kfld                    H2Clnp
013400050727     c                   kfld                    H2Cnrs
013500050727     c                   kfld                    H2Cnsp
013600050727      * - FNDKA01L
013700050727     c     K04DKA01      klist
013800050727     c                   kfld                    H2Caac
013900050727     c                   kfld                    S2Cfil
014000050727     c                   kfld                    S2Cnca
014100050727     c                   kfld                    DKAtrc
014200050727      * - FNSPE01L
014300050727     c     K03SPE01      klist
014400050727     c                   kfld                    SPEfls
014500050727     c                   kfld                    SPEcli
014600050727     c                   kfld                    SPEcod
014700050727      * - TABEL00F
014800050727     c     K03TABEL      klist
014900050727     c                   kfld                    TBLkut
015000050728     c                   kfld                    TBLcod
015100050727     c                   kfld                    TBLkey
015200050727
015300050727      *---------------------------------------------------------------*
015400050727      *  RIEPILOGO INDICATORI                                         *
015500050727      *---------------------------------------------------------------*
015600050727      * 04    - Alta intensità riga di testo                          *
015700050727      * 10    - Comodo                                                *
015800050728      * 35    - Ripulisce il subfile S02                              *
015900050728      * 36    - NON emette il subfile S02                             *
016000050728      * 37    - Record modificato nel subfile S02 (sflnxtchg)         *
016100050728      * 38    - Fine dati nel subfile S02         (sflend)            *
016200051025      * 41    - Flag Esclusione Evento da Richiesta Recupero in (RI)  *
016300050727      *---------------------------------------------------------------*
016400050727
016500050727     c     *Entry        plist
016600050727     c                   parm                    KPJBA
016700050727      *
016800050727      * Operazioni iniziali
016900050727     c                   exsr      OpeIni
017000050727      *
017100050727      * Stampa
017200050727     c                   exsr      Stampa
017300050727      *
017400050727      * Fine
017500050727     c                   clear                   TIBS02ds
017600050728     c                   eval      T02tla   = 'C'
017700050727     c                   call      'TIBS02R'
017800050727     c                   parm                    KPJBA
017900050727     c                   parm                    TIBS02ds
018000050728     c                   if        $TIBS69R = *on
018100050728     c                   clear                   TIBS69ds
018200050728     c                   eval      I69tla   = 'C'
018300050728     c                   call      'TIBS69R'
018400050728     c                   parm                    TIBS69ds
018500050728     c                   endif
018600050727      *
018700050727     c                   movel     *on           *inLR
018800050727      *
018900050727      *---------------------------------------------------------------*
019000050727      * Operazioni Iniziali                                           *
019100050727      *---------------------------------------------------------------*
019200050727     c     OpeIni        BEGSR
019300050727      *
019400050727      * Reperimento dati del lavoro
019500050727     c                   exsr      DatiJob
019600050728      *
019700050728     c                   move      *date         wDate
019800050728      *
019900050728      * Impostazione campi fissi
020000050728     c                   eval      TBLkut = 1
020100050728     c                   eval      SPEfls = 'L'
020200050728     c                   eval      SPEcod = '006'
020300050728      *
020400050728      * Aggancio tabella GED (Gestionale: Divise)
020500050728     c                   clear                   dGEDdn
020600050728     c                   clear                   TIBS02ds
020700050728     c                   eval      T02mod = 'C'
020800050728     c                   eval      T02sif = KNSIF
020900050728     c                   eval      T02cod = 'GED'
021000050728     c                   movel(p)  'DANNI'       T02ke1
021100050728     c                   call      'TIBS02R'
021200050728     c                   parm                    KPJBA
021300050728     c                   parm                    TIBS02ds
021400050728     c                   if        T02err = *blanks
021500050728     c                   movel     T02uni        dGEDdn
021600050728     c                   endif
021700050727      *
021800050921      * Reperimento tab. "STD/4" = Dati A.I.G.
021900050727     c                   clear                   dSTD4
022000050727     c                   clear                   TIBS02ds
022100050728     c                   eval      T02mod = 'C'
022200050728     c                   eval      T02sif = KNSIF
022300050728     c                   eval      T02cod = 'STD'
022400050727     c                   movel(p)  '4'           T02ke1
022500050727     c                   call      'TIBS02R'
022600050727     c                   parm                    KPJBA
022700050727     c                   parm                    TIBS02ds
022800050727if  1c                   if        T02err = *blanks
022900050727     c                   movel     T02uni        dSTD4
023000050727e   1c                   endif
023100050727      *
023200050727     c                   ENDSR
023300050727      *
023400050727      *---------------------------------------------------------------*
023500050727      * Reperimento Dati del job (Utente/Operativi)                   *
023600050727      *---------------------------------------------------------------*
023700050727     c     DatiJob       BEGSR
023800050727      *
023900050727     c     *dtaara       define    §azute        azuteds
024000050727     c     *dtaara       define    §datiute      ddatiute
024100050727      *
024200050727     c                   in(E)     *dtaara
024300050727     c                   IF        %ERROR or RSUT = *blanks
024400050727     c                   clear                   Tibs34Ds
024500050727     c                   call      'TIBS34R'
024600050727     c                   parm                    Tibs34Ds
024700050727     c                   in        *dtaara
024800050727     c                   ENDIF
024900050727      *
025000050727     c                   ENDSR
025100050727      *
025200050727      *---------------------------------------------------------------*
025300050727      * Stampa elenco C.A.                                            *
025400050727      *---------------------------------------------------------------*
025500050727     c     Stampa        BEGSR
025600050727      *
025700050727      * Elaborazione sfl FIDN86D
025800050728     c                   eval      *in28  = *off
025900050728     c                   readc     FI86S02
026000050727      *
026100050727do  1c                   DOW       NOT %eof(FIDN86D)
026200050727      *
026300050728     c                   eval      *in37  = *off
026400050728     c                   movea     *zeros        *in(50)
026500050728     c                   z-add     S02nrr        C02rcd
026600050727      *
026700051025      * R = Richiesta recupero ad A.I.G.
026800050728if  2c                   if            S2Copz = 'R'
026900050728     c                             and S2Cesc = *blanks
027000050727     c                   exsr      StampaCA
027100050727e   2c                   endif
027200050727      *
027300050727      * Reimpostazione indicatori per DSPATR
027400050728     c     S2Cesc        comp      *blanks                            4141
027500050727      *
027600050727      * Aggiornamento sfl
027700050727if  2c                   if        *in28
027800050728     c                   eval      *in37  = *on
027900050728     c                   z-add     C02rcd        C02csr
028000050727x   2c                   else
028100050728     c                   clear                   S2Copz
028200050728     c                   eval      S2Cdrr = *date
028300050727e   2c                   endif
028400050728     c                   UPDATE    FI86S02
028500050727if  2c                   if        *in28
028600050727     c                   leave
028700050727e   2c                   endif
028800050727      *
028900050728     c                   readc     FI86S02
029000050727      *
029100050727e   1c                   ENDDO
029200050727      *
029300050727      * Stampa piede lettera
029400050727if  1c                   if        NOT *in28
029500050802     c                   exsr      StampaFin
029600050727e   1c                   endif
029700050727      *
029800050727     c                   ENDSR
029900050727      *
030000050727      *---------------------------------------------------------------*
030100051025      * Stampa intestazione per richiesta recupero ad A.I.G.          *
030200050727      *---------------------------------------------------------------*
030300050802     c     StampaIni     BEGSR
030400050728      *
030500050802     c                   clear                   DN88pI
030600050802     c                   clear                   DN88pO
030700050802     c*** solo testo:    clear                   DN88pT
030800050727      *
030900050727      * Reperimento dati evento
031000050727     c     K02DET01      chain     FNDET000
031100050727     c                   if        NOT %found(FNDET01L)
031200050914     c                             or  DETatb <> *blanks
031300050727      * ...impossibile !!!
031400050727     c                   clear                   FNDET000
031500050727     c                   endif
031600050727      *
031700050727      * Decodifica Tipo Anomalia
031800050727     c                   eval      wTAD = DETtad
031900050727     c                   exsr      DecodTAD
032000050727      *
032100050727      * Decodifica P.O. che ha Comunicato l'Evento
032200050727     c                   exsr      DecodFCE
032300050727      *
032400050727      * Salto ad inizio pagina
032500050802     c                   write     DN88pP
032600050727      *
032700050727      * Stampa intestazione lettera
032800050727     c                   eval      PICraa = §STD4rsc
032900050727     c                   eval      PICina = §STD4ind
033000050727     c                   eval      PICcaa = §STD4cap
033100050727     c                   eval      PICloa = §STD4loc
033200050727     c                   eval      PICpra = §STD4prv
033300050802     c                   eval      PICdal = 'Bologna, '
033400050727     c                                    + %editc(*date:'Y')
033500050802     c                   write     DN88pI
033600050801      *
033700050801     c                   do        2
033800050802     c                   write     DN88p0
033900050801     c                   enddo
034000050727      *
034100050727      * Stampa oggetto lettera
034200050914     c                   reset                   WLBdat
034300050914     c                   eval      G08inv = (H2Caae * 10000) + H2Cmge
034400050914     c                   call      'XSRDA8'
034500050914     c                   parm                    WLBdat
034600050727     c                   eval      POCogg = 'Evento nr. '
034700050727     c                                    + %trim(%editc(H2Cnev:'Z'))
034800050914     c                                    + ' del '
034900050914     c                                    + %trim(%editc(G08dat:'Y'))
035000050914     c                                    + '         '
035100050727     c                                    + 'Tipo anomalia: '
035200050923     c                                    + DETtad
035300050727     c                                    + '  '
035400050727     c                                    + %trim(§TADdest)
035500050802     c                   write     DN88pO
035600050801      *
035700050801     c                   eval      POCogg = 'P.O. che ha segnalato +
035800050801     c                                       l''evento:'
035900050801     c                                    + %editw(DETfce:'0   ')
036000050801     c                                    + '  '
036100050801     c                                    + %trim(ORGdes)
036200050802     c                   write     DN88pO
036300050802      *
036400050802      * Stampa inizio lettera
036500050802     c                   write     DN88pT
036600050727      *
036700050727     c                   ENDSR
036800050727      *
036900050727      *---------------------------------------------------------------*
037000051025      * Stampa elenco C.A. per richiesta recupero ad A.I.G.           *
037100050727      *---------------------------------------------------------------*
037200050727     c     StampaCA      BEGSR
037300050727      *
037400050727      * Stampa intestazione lettera
037500050727if  1c                   if        $First = *on
037600050802     c                   exsr      StampaIni
037700050727e   1c                   endif
037800050728      *
037900050802     c                   clear                   DN88pD
038000050727      *
038100051025      * Reperimento dati richiesta recupero
038200050727     c     K03DRA01      chain     FNDRA000
038300050727if  1c                   if        NOT %found(FNDRA01L)
038400050914     c                             or  DRAatb <> *blanks
038500050727     c                   clear                   FNDRA000
038600050727     c                   eval      DRAaac = H2Caac
038700050727     c                   eval      DRAfil = S2Cfil
038800050727     c                   eval      DRAnca = S2Cnca
038900050727e   1c                   endif
039000050727      *
039100050727      * Reperimento dati C.A.
039200050727     c     K03DRA01      chain     FNDCT000
039300050727if  1c                   if        NOT %found(FNDCT01L)
039400050914     c                             or  DCTatb <> *blanks
039500050727     c                   clear                   DCTmgc
039600050804     c                   clear                   DCTprn
039700050804     c                   clear                   DCTpra
039800050727     c                   clear                   DCTflo
039900050727e   1c                   endif
040000050727     c                   movel     DCTflo        dDCT01
040100050727      *
040200050727      * Reperimento dati LdV
040300050727     c     K04TAS30      chain     TITAS30C
040400050914if  1c*** impossibile:   if        NOT %found(TITAS30C)
040500050727     c***                clear                   TAS...
040600050727e   1c***                endif
040700050727      * - se tipo porto "recupero": rileggo
040800050727     c                   clear                   dsTB
040900050727     c                   movel     'TB'          TBLcod
041000050727     c                   movel(P)  TAStbl        TBLkey
041100050727     c     K03TABEL      chain     TABEL
041200050914if  1c                   if            %found(TABEL00F)
041300050914     c                             and TBLflg =  *blanks
041400050727     c                   movel     TBLuni        dsTB
041500050727e   1c                   endif
041600050727if  1c                   if        §TBrbl = 'R'
041700050727     c     K04TAS30      reade     TITAS30C
041800050727e   1c                   endif
041900050727      *
042000050727      * Decodifica beneficiario
042100050727     c                   exsr      sr_Benef
042200050727      *
042300050727      * Stampa elenco C.A.
042400050727      * - impostazione dati singola C.A.
042500050727      *   - Numero e data C.A.
042600050728     c                   eval      POCnca = %trim(%editw(DRAfil:'0   '))
042700050728     c                                    + '/'
042800050728     c                                    + %trim(%editc(DRAnca:'Z'))
042900050728     c                   reset                   WLBdat
043000050728     c                   eval      G08inv = (DCTaac * 10000) + DCTmgc
043100050728     c                   call      'XSRDA8'
043200050728     c                   parm                    WLBdat
043300050728if  1c                   if        G08err = *off
043400050728     c                   eval      POCdca = G08dat
043500050728e   1c                   endif
043600050804      *   - Numero e data Pratica Assicurativa
043700050804if  1c                   if        DCTprn > *zeros
043800050804     c                   eval      POCprn = 'Pratica assicurativa nr. '
043900050804     c                                    + %trim(%editc(DCTprn:'Z'))
044000050804     c                                    + ' Anno '
044100050804     c                                    + %trim(%editc(DCTpra:'Z'))
044200050804e   1c                   endif
044300050804      *
044400050728      *   - Importo liquidato
044500050728     c                   eval      POCipl = %trim(%editc(S2Cipl:'1'))
044600050728     c                                    + ' '
044700050728     c                                    + §GEDdba
044800050727      *
044900050727      * - test OF
045000050727if  1c                   if        *in01  = *on
045100050802     c                   write     DN88pP
045200050727do  2c                   do        2
045300050802     c                   write     DN88p0
045400050727e   2c                   enddo
045500050727     c                   eval      *in01  = *off
045600050727e   1c                   endif
045700050727      * - stampa dati singola C.A.
045800050802     c                   write     DN88pD
045900050727      *
046000050727      * Aggiornamento flag DETFTR in FNDET00F (per evento)
046100050727     c                   if        $First = *on
046200050727     c                   eval      DETftr = 'S'
046300050727     c                   UPDATE    FNDET000
046400050727     c                   eval      $First = *off
046500050727     c                   endif
046600050727      *
046700050727      * Aggiornamento flag DRAESC in FNDRA00F (per singola C.A.)
046800050728     c                   move      wDate         DRAdrr
046900050728if  1c                   if        NOT %found(FNDRA01L)
047000050728     c                   WRITE     FNDRA000
047100050728x   1c                   else
047200050727     c                   UPDATE    FNDRA000
047300050728e   1c                   endif
047400050727      *
047500050727     c                   ENDSR
047600050728      *
047700050728      *---------------------------------------------------------------*
047800050802      * Stampa piede lettera                                          *
047900050728      *---------------------------------------------------------------*
048000050802     c     StampaFin     BEGSR
048100050728      *
048200050802      * Stampa fine lettera
048300051026     c                   write     DN88pF
048400050728      *
048500050728     c                   ENDSR
048600050727      *
048700050727      *---------------------------------------------------------------*
048800050727      * Decodifica / Controllo TIPO ANOMALIA                          *
048900050727      *---------------------------------------------------------------*
049000050727     c     DecodTAD      BEGSR
049100050727      *
049200050727     c                   clear                   dTAD
049300050727     c                   reset                   TIBS02ds
049400050727     c                   movel     KNSIF         T02sif
049500050727     c*** già impostato: movel     'TAD'         T02cod
049600050727     c                   movel(p)  wTAD          T02ke1
049700050727     c                   call      'TIBS02R'
049800050727     c                   parm                    KPJBA
049900050727     c                   parm                    TIBS02ds
050000050727     c                   if        T02err   = *blanks
050100050727     c                   movel     T02uni        dTAD
050200050727     c                   endif
050300050727      *
050400050727     c                   if        §TADdest = *blanks
050500050727     c                   movel(p)  §TADdesc      §TADdest
050600050727     c                   endif
050700050727      *
050800050727     c                   ENDSR
050900050727      *
051000050727      *---------------------------------------------------------------*
051100050727      * Decodifica P.O. che ha Comunicato l'Evento                    *
051200050727      *---------------------------------------------------------------*
051300050727     c     DecodFCE      BEGSR
051400050727      *
051500050727     c     DETfce        chain     AZORG
051600050727     c                   if        NOT %found(AZORG01L)
051700050727     c                             or  ORGfva <> *blanks
051800050727     c                   clear                   ORGdes
051900050727     c                   endif
052000050727      *
052100050727     c                   ENDSR
052200050727      *
052300050727      *---------------------------------------------------------------*
052400050727      * Reperimento dati cliente beneficiario                         *
052500050727      *---------------------------------------------------------------*
052600050727     c     sr_Benef      BEGSR
052700050727      *
052800050727     c                   clear                   wCli
052900050727     c                   reset                   $Rivalsa
053000050727      *
053100050727      * Dalla spedizione determino il codice beneficiario
053200050727      *
053300050727      * Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con
053400050727      *  quello C.A. tranne se la sped. è "Mamma di cambio porto" e mi trovo
053500050727      *  in filiale, in questo caso inverto il porto C.A.
053600050727sel 1c                   select
053700110504w   1c                   when      §DCTCCA   <> '9'
053800050727     c                   eval      w§DCTport =  §DCTport
053900050727w   1c                   when      §DCTport  =  'A'
054000050727     c                   eval      w§DCTport =  'F'
054100050727w   1c                   when      §DCTport  =  'F'
054200050727     c                   eval      w§DCTport =  'A'
054300050727e   1c                   endsl
054400050727      *
054500050727sel 1c                   select
054600050727      * - destinatario / pf
054700050727w   1c                   when          DCTptr    = 'D'
054800050727     c                             and w§DCTport = 'F'
054900050727      * - mittente     / pa
055000050727w   1c                   when          DCTptr    = 'M'
055100050727     c                             and w§DCTport = 'A'
055200050727     c                   eval      wCli      =  TASccm
055300050727x   1c                   other
055400050727     c                   eval      wCli      =  TASksc
055500050727e   1c                   endsl
055600050727      *
055700050727      * Scorporo i codici cliente
055800050727     c                   move      wCli          w0040a
055900050727     c                   move      DCTksc        w0040b
056000050727      *
056100050727      * Controllo se ho rivalsa
056200050727     c                   eval      DKAtrc    =  'R'
056300050728     c     K04DKA01      chain     FNDKA000
056400050914if  1c                   if            %found(FNDKA01L)
056500050914     c                             and DKAatb =  *blanks
056600050727     c                   eval      $Rivalsa  =  *on
056700050727e   1c                   endif
056800050727      *
056900050727      *
057000050727sel 1c                   SELECT
057100050727      * Rivalsa
057200050727      * -------
057300050727w   1c                   WHEN      $Rivalsa  =  *on
057400050727     c                   exsr      Benef_DKA
057500050727     *** *   se beneficiario alternativo codificato memorizzo il codice
057600050727     *** *   per ricavare la società
057700050727     ***c                   if             w0040b >  *zeros
057800050727     ***c                             and  w0040b <> 8888
057900050727     ***c                   eval      wCli      =  DCTksc
058000050727     ***c                   endif
058100050727      *
058200050727      * Beneficiario alternativo generico
058300050727      * ---------------------------------
058400050727w   1c                   WHEN      w0040b    =  8888
058500050727     c                   eval      DKAtrc    =  'C'
058600050727     c     K04DKA01      chain     FNDKA000
058700050914if  2c                   if            %found(FNDKA01L)
058800050914     c                             and DKAatb =  *blanks
058900050727     c                   exsr      Benef_DKA
059000050728e   2c                   endif
059100050727      *
059200050727      * Beneficiario alternativo codificato
059300050727      * -----------------------------------
059400050727w   1c                   WHEN      DCTksc    >  *zeros
059500050727     c                   eval      wCli      =  DCTksc
059600050727     c                   clear                   TIBS69ds
059700050727     c                   eval      I69kac    =  wCli
059800050727     c                   eval      I69kin    =  wCli
059900050727     c                   eval      I69kcs    =  wCli
060000050728     c                   exsr      Rep_Anagra
060100050727     c                   movel     ACOrag        POCrsb
060200050727     c                   movel     INDvia        POCinb
060300050727     c                   movel     INDcae        POCcab
060400050727     c                   movel     INDcit        POClob
060500050727if  2c                   if        INDsta    =  *blanks
060600050727     c                   movel     INDprv        POCprb
060700050727x   2c                   else
060800050727     c                   movel     INDsta        POCnab
060900050727e   2c                   endif
061000050727      * - controllo se esiste luogo 6
061100050727     c     K03SPE01      chain     FNSPE000
061200050914if  2c                   if            %found(FNSPE01L)
061300050914     c                             and SPEflg  = *blanks
061400050727     c                   movel     SPErag        POCrsb
061500050727     c                   movel     SPEind        POCinb
061600050727     c                   movel     SPEcap        POCcab
061700050727     c                   movel     SPEloc        POClob
061800050727if  3c                   if        SPEnaz    =  *blanks
061900050727     c                   movel     SPEpro        POCprb
062000050727x   3c                   else
062100050727     c                   movel     SPEnaz        POCnab
062200050727e   3c                   endif
062300050727e   2c                   endif
062400050727      *
062500050727      * Cliente in spedizione
062600050727      * ---------------------
062700050727x   1c                   OTHER
062800050727     c                   clear                   TIBS69ds
062900050727     c                   eval      I69kcs    =  wCli
063000050728     c                   exsr      Rep_Anagra
063100050727     c                   eval      *in10 = *off
063200050727if  2c                   if             w0040a >  *zeros
063300050727     c                             and  w0040a <> 8888
063400050727     c                             and  w0040a <> 9999
063500050727      * - indirizzo da luogo 6
063600050727     c     K03SPE01      chain     FNSPE000                           10
063700050727x   2c                   else
063800050727     c                   eval      *in10     =  *on
063900050727e   2c                   endif
064000050727if  2c                   IF        not *in10
064100050727     c                   movel     SPErag        POCrsb
064200050727     c                   movel     SPEind        POCinb
064300050727     c                   movel     SPEcap        POCcab
064400050727     c                   movel     SPEloc        POClob
064500050727if  3c                   if        SPEnaz    =  *blanks
064600050727     c                   movel     SPEpro        POCprb
064700050727x   3c                   else
064800050727     c                   movel     SPEnaz        POCnab
064900050727w   3c                   endif
065000050727x   2c                   ELSE
065100050727      * - Aggancio file mittenti/destinatari
065200050727     c     K04TAS30      chain     TNTMD000
065300050727if  3c                   if        NOT %found(TNTMD01L)
065400050727     c                   clear                   ds_TNTMD
065500050727e   3c                   endif
065600050727      *   - indirizzo da destinatario spedizione
065700050727if  3c                   if        DCTptr    =  'D'
065800050727     c                   movel     TMDrsd        POCrsb
065900050727     c                   movel     TMDind        POCinb
066000050727     c                   movel     TMDcad        POCcab
066100050727     c                   movel     TMDlod        POClob
066200050727if  4c                   if        TMDnzd    =  *blanks
066300050727     c                   movel     TMDprd        POCprb
066400050727x   4c                   else
066500050727     c                   movel     TMDnzd        POCnab
066600050727e   4c                   endif
066700050727x   3c                   else
066800050727      *   - indirizzo da mittente spedizione
066900050727     c                   movel     TMDrsm        POCrsb
067000050727     c                   movel     TMDinm        POCinb
067100050727     c                   movel     TMDcam        POCcab
067200050727     c                   movel     TMDlom        POClob
067300050727if  4c                   if        TMDnzm    =  *blanks
067400050727     c                   movel     TMDprm        POCprb
067500050727x   4c                   else
067600050727     c                   movel     TMDnzm        POCnab
067700050727e   4c                   endif
067800050727e   3c                   endif
067900050727      *
068000050727e   2c                   ENDIF
068100050727      *
068200050727e   1c                   ENDSL
068300050727      *
068400050727     c                   ENDSR
068500050727      *
068600050727      *---------------------------------------------------------------*
068700050727      * Decodifica beneficiario da FNDKA                              *
068800050727      *---------------------------------------------------------------*
068900050727     c     Benef_DKA     BEGSR
069000050727      *
069100050728     c                   movel     DKArag        POCrsb
069200050728     c                   movel     DKAvia        POCinb
069300050728     c                   movel     DKAcap        POCcab
069400050728     c                   movel     DKAloc        POClob
069500050727     c                   if        DKAnaz = *blanks
069600050728     c                   movel     DKAprv        POCprb
069700050727     c                   else
069800050728     c                   movel     DKAnaz        POCnab
069900050727     c                   endif
070000050727      *
070100050727     c                   ENDSR
070200050728      *
070300050728      *---------------------------------------------------------------*
070400050728      * Reperimento dati anagrafici                                   *
070500050728      *---------------------------------------------------------------*
070600050728     c     Rep_Anagra    BEGSR
070700050728      *
070800050728     c                   clear                   ds_CNACO
070900050728     c                   clear                   ds_CNIND
071000050728     c                   clear                   ds_CNCLP
071100050728     c                   clear                   ds_FNCLS
071200050728      *
071300050728     c                   call      'TIBS69R'
071400050728     c                   parm                    TIBS69ds
071500050728     c                   parm                    ds_CNACO
071600050728     c                   parm                    ds_CNIND
071700050728     c                   parm                    ds_CNCLP
071800050728     c                   parm                    ds_FNCLS
071900050728      *
072000050728     c                   eval      *in35 = (O69err = *on)
072100050728      *
072200050728     c                   eval      $TIBS69R = *on
072300050728      *
072400050728     c                   ENDSR
