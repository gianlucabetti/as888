000100050727      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200051026      *PARMS DFTACTGRP(*NO)  ACTGRP(*CALLER)
000300050727      *===============================================================*
000400051026      *?FIDN88R * Stampa richiesta recupero C.A.                     ?*
000500050727      *===============================================================*
000600050727
000700050727     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000800050727
000900050727      *---------------------------------------------------------------*
001000050727
001100050727     fFIDN86D   cf   e             workstn
001200050728     f                                     include(FI86S02)
001300050728     f                                     include(FI86C02)
001400050727     f                                     sfile(FI86S02:S02nrr)
001500050728     f                                     infds(dsFMT)
001600050727      *
001700050728     fFNDET01L  Uf   e           k disk
001800050728     fFNDRA01L  Uf a e           k disk
001900050727      *
002000050727     fFNDCT01L  if   e           k disk
002100050727      *
002200050727     fTITAS30C  if   e           k disk
002300050727     fFNDKA01L  if   e           k disk
002400050727     fFNSPE01L  if   e           k disk
002500050727     fTNTMD01L  if   e           k disk
002600050727      *
002700050727     fAZORG01L  if   e           k disk
002800050727     fTABEL00F  if   e           k disk
002900050727      *
003000050728     fFIDN88P   o    e             printer oflind(*in01)
003100050727     f                                     infds(PrtfInf)
003200050727
003300050727      *---------------------------------------------------------------*
003400050727
003500050728      *
003600050728      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003700050728      *
003800050727      *
003900050727      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004000050727      *
004100050727      *
004200050727      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004300050727      *
004400050727     d KPJBA         e ds
004500050727      *
004600050727      * - Parametri x Controllo profilo utenti
004700050727     d TIBS34ds      e ds                  inz
004800050727      * - Ds di riferimento al file esterno AZUTE00F
004900050727     d AZUTEds       e ds                  extname(AZUTE00F)
005000050727      * - Ds per dati organigramma
005100050727     d DDatiUte      e ds
005200050727      *
005300050727      * - Parametri x Interrogazione tabelle
005400050728     d TIBS02ds      e ds                  inz
005500050727     d   T02mod      e                     inz('C')
005600050727     d   T02cod      e                     inz('TAD')
005700050728      * - Tab. "GED/DN"
005800050728     d dGEDdn        e ds                  inz
005900050727      * - Tab. "STD/4"
006000050727     d dSTD4         e ds                  inz
006100050727      * - Tab. "TAD"
006200050727     d dTAD          e ds                  inz
006300050727      * - Tab. "TB"
006400050727     d dsTB          e ds                  inz
006500050727      *
006600050727     d TIBS69ds      e ds                  inz
006700050727     d ds_CNACO      e ds                  inz extname(CNACO00F)
006800050727     d ds_CNIND      e ds                  inz extname(CNIND00F)
006900050727     d ds_CNCLP      e ds                  inz extname(CNCLP00F)
007000050727     d ds_FNCLS      e ds                  inz extname(FNCLS00F)
007100050727      *
007200050727     d ds_TNTMD      e ds                  inz extname(TNTMD00F)
007300050727      *
007400050727     d dDCT01        e ds                  inz
007500050727      *
007600050727     d Status         sds           333
007700050727     d   SDSpgm          *proc
007800050727      *
007900050728     d dsFMT           ds           512
008000050727     d  £Tasto               369    369
008100050727     d  £Nrg                 370    370
008200050727     d  £Ncl                 371    371
008300050727     d  £SFLnrr              378    379B 0
008400050727      *
008500050727     d PrtfInf         ds
008600050727     d  Cur_Line             367    368I 0
008700050728      *
008800050728     d ds_Evento       ds                  inz
008900050728     d  H2Caae                             inz
009000050728     d  H2Cnev                             inz
009100050727      *
009200050727     d ds_NumCA        ds                  inz
009300050728     d  H2Caac                             inz
009400050728     d  S2Cfil                             inz
009500050728     d  S2Cnca                             inz
009600050727      *
009700050727      * - Controllo/Inversione date
009800050727     d WLBdat          ds                  inz
009900050727     d   G08dat                       8  0 inz
010000050727     d   G08inv                       8  0 inz
010100050727     d   G08err                       1    inz('3')
010200050727     d   G08tgi                       5  0 inz
010300050727      *
010400050727      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
010500050727      *
010600050727      * - flag
010700050727     d $First          s              1    inz(*on)
010800050727     d $Rivalsa        s              1    inz(*off)
010900050728     d $TIBS69R        s              1    inz(*off)
011000050728      * - indici di schiera
011100050727      * - Variabili riferite al data base o al display file
011200050728     d S02nrr          s                   like(C02rcd)   inz
011300050728     d w§DCTport       s                   like(§DCTport) inz
011400050728     d wCLI            s                   like(TASksc)   inz
011500050728     d wTAD            s                   like(DETtad)   inz
011600050728      * - Variabili definite internamente
011700050728     d wDate           s               d   inz  datfmt(*iso)
011800050728     d w0040a          s              4  0 inz
011900050728     d w0040b          s              4  0 inz
012000050727      *
012100050727      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
012200050727      *
012300050727      * - FNDET01L
012400050727     c     K02DET01      klist
012500050727     c                   kfld                    H2Caae
012600050727     c                   kfld                    H2Cnev
012700050727      * - FNDRA01L / FNDCL01L
012800050727     c     K03DRA01      klist
012900050727     c                   kfld                    H2Caac
013000050727     c                   kfld                    S2Cfil
013100050727     c                   kfld                    S2Cnca
013200050727      * - TITAS30C / TNTMD01L
013300050727     c     K04TAS30      klist
013400050727     c                   kfld                    H2Caas
013500050727     c                   kfld                    H2Clnp
013600050727     c                   kfld                    H2Cnrs
013700050727     c                   kfld                    H2Cnsp
013800050727      * - FNDKA01L
013900050727     c     K04DKA01      klist
014000050727     c                   kfld                    H2Caac
014100050727     c                   kfld                    S2Cfil
014200050727     c                   kfld                    S2Cnca
014300050727     c                   kfld                    DKAtrc
014400050727      * - FNSPE01L
014500050727     c     K03SPE01      klist
014600050727     c                   kfld                    SPEfls
014700050727     c                   kfld                    SPEcli
014800050727     c                   kfld                    SPEcod
014900050727      * - TABEL00F
015000050727     c     K03TABEL      klist
015100050727     c                   kfld                    TBLkut
015200050728     c                   kfld                    TBLcod
015300050727     c                   kfld                    TBLkey
015400050727
015500050727      *---------------------------------------------------------------*
015600050727      *  RIEPILOGO INDICATORI                                         *
015700050727      *---------------------------------------------------------------*
015800050727      * 04    - Alta intensità riga di testo                          *
015900050727      * 10    - Comodo                                                *
016000050728      * 35    - Ripulisce il subfile S02                              *
016100050728      * 36    - NON emette il subfile S02                             *
016200050728      * 37    - Record modificato nel subfile S02 (sflnxtchg)         *
016300050728      * 38    - Fine dati nel subfile S02         (sflend)            *
016400051025      * 41    - Flag Esclusione Evento da Richiesta Recupero in (RI)  *
016500050727      *---------------------------------------------------------------*
016600050727
016700050727     c     *Entry        plist
016800050727     c                   parm                    KPJBA
016900050727      *
017000050727      * Operazioni iniziali
017100050727     c                   exsr      OpeIni
017200050727      *
017300050727      * Stampa
017400050727     c                   exsr      Stampa
017500050727      *
017600050727      * Fine
017700050727     c                   clear                   TIBS02ds
017800050728     c                   eval      T02tla   = 'C'
017900050727     c                   call      'TIBS02R'
018000050727     c                   parm                    KPJBA
018100050727     c                   parm                    TIBS02ds
018200050728     c                   if        $TIBS69R = *on
018300050728     c                   clear                   TIBS69ds
018400050728     c                   eval      I69tla   = 'C'
018500050728     c                   call      'TIBS69R'
018600050728     c                   parm                    TIBS69ds
018700050728     c                   endif
018800050727      *
018900050727     c                   movel     *on           *inLR
019000050727      *
019100050727      *---------------------------------------------------------------*
019200050727      * Operazioni Iniziali                                           *
019300050727      *---------------------------------------------------------------*
019400050727     c     OpeIni        BEGSR
019500050727      *
019600050727      * Reperimento dati del lavoro
019700050727     c                   exsr      DatiJob
019800050728      *
019900050728     c                   move      *date         wDate
020000050728      *
020100050728      * Impostazione campi fissi
020200050728     c                   eval      TBLkut = 1
020300050728     c                   eval      SPEfls = 'L'
020400050728     c                   eval      SPEcod = '006'
020500050728      *
020600050728      * Aggancio tabella GED (Gestionale: Divise)
020700050728     c                   clear                   dGEDdn
020800050728     c                   clear                   TIBS02ds
020900050728     c                   eval      T02mod = 'C'
021000050728     c                   eval      T02sif = KNSIF
021100050728     c                   eval      T02cod = 'GED'
021200050728     c                   movel(p)  'DANNI'       T02ke1
021300050728     c                   call      'TIBS02R'
021400050728     c                   parm                    KPJBA
021500050728     c                   parm                    TIBS02ds
021600050728     c                   if        T02err = *blanks
021700050728     c                   movel     T02uni        dGEDdn
021800050728     c                   endif
021900050727      *
022000050921      * Reperimento tab. "STD/4" = Dati A.I.G.
022100050727     c                   clear                   dSTD4
022200050727     c                   clear                   TIBS02ds
022300050728     c                   eval      T02mod = 'C'
022400050728     c                   eval      T02sif = KNSIF
022500050728     c                   eval      T02cod = 'STD'
022600050727     c                   movel(p)  '4'           T02ke1
022700050727     c                   call      'TIBS02R'
022800050727     c                   parm                    KPJBA
022900050727     c                   parm                    TIBS02ds
023000050727if  1c                   if        T02err = *blanks
023100050727     c                   movel     T02uni        dSTD4
023200050727e   1c                   endif
023300050727      *
023400050727     c                   ENDSR
023500050727      *
023600050727      *---------------------------------------------------------------*
023700050727      * Reperimento Dati del job (Utente/Operativi)                   *
023800050727      *---------------------------------------------------------------*
023900050727     c     DatiJob       BEGSR
024000050727      *
024100050727     c     *dtaara       define    §azute        azuteds
024200050727     c     *dtaara       define    §datiute      ddatiute
024300050727      *
024400050727     c                   in(E)     *dtaara
024500050727     c                   IF        %ERROR or RSUT = *blanks
024600050727     c                   clear                   Tibs34Ds
024700050727     c                   call      'TIBS34R'
024800050727     c                   parm                    Tibs34Ds
024900050727     c                   in        *dtaara
025000050727     c                   ENDIF
025100050727      *
025200050727     c                   ENDSR
025300050727      *
025400050727      *---------------------------------------------------------------*
025500050727      * Stampa elenco C.A.                                            *
025600050727      *---------------------------------------------------------------*
025700050727     c     Stampa        BEGSR
025800050727      *
025900050727      * Elaborazione sfl FIDN86D
026000050728     c                   eval      *in28  = *off
026100050728     c                   readc     FI86S02
026200050727      *
026300050727do  1c                   DOW       NOT %eof(FIDN86D)
026400050727      *
026500050728     c                   eval      *in37  = *off
026600050728     c                   movea     *zeros        *in(50)
026700050728     c                   z-add     S02nrr        C02rcd
026800050727      *
026900051025      * R = Richiesta recupero ad A.I.G.
027000050728if  2c                   if            S2Copz = 'R'
027100050728     c                             and S2Cesc = *blanks
027200050727     c                   exsr      StampaCA
027300050727e   2c                   endif
027400050727      *
027500050727      * Reimpostazione indicatori per DSPATR
027600050728     c     S2Cesc        comp      *blanks                            4141
027700050727      *
027800050727      * Aggiornamento sfl
027900050727if  2c                   if        *in28
028000050728     c                   eval      *in37  = *on
028100050728     c                   z-add     C02rcd        C02csr
028200050727x   2c                   else
028300050728     c                   clear                   S2Copz
028400050728     c                   eval      S2Cdrr = *date
028500050727e   2c                   endif
028600050728     c                   UPDATE    FI86S02
028700050727if  2c                   if        *in28
028800050727     c                   leave
028900050727e   2c                   endif
029000050727      *
029100050728     c                   readc     FI86S02
029200050727      *
029300050727e   1c                   ENDDO
029400050727      *
029500050727      * Stampa piede lettera
029600050727if  1c                   if        NOT *in28
029700050802     c                   exsr      StampaFin
029800050727e   1c                   endif
029900050727      *
030000050727     c                   ENDSR
030100050727      *
030200050727      *---------------------------------------------------------------*
030300051025      * Stampa intestazione per richiesta recupero ad A.I.G.          *
030400050727      *---------------------------------------------------------------*
030500050802     c     StampaIni     BEGSR
030600050728      *
030700050802     c                   clear                   DN88pI
030800050802     c                   clear                   DN88pO
030900050802     c*** solo testo:    clear                   DN88pT
031000050727      *
031100050727      * Reperimento dati evento
031200050727     c     K02DET01      chain     FNDET000
031300050727     c                   if        NOT %found(FNDET01L)
031400050914     c                             or  DETatb <> *blanks
031500050727      * ...impossibile !!!
031600050727     c                   clear                   FNDET000
031700050727     c                   endif
031800050727      *
031900050727      * Decodifica Tipo Anomalia
032000050727     c                   eval      wTAD = DETtad
032100050727     c                   exsr      DecodTAD
032200050727      *
032300050727      * Decodifica P.O. che ha Comunicato l'Evento
032400050727     c                   exsr      DecodFCE
032500050727      *
032600050727      * Salto ad inizio pagina
032700050802     c                   write     DN88pP
032800050727      *
032900050727      * Stampa intestazione lettera
033000050727     c                   eval      PICraa = §STD4rsc
033100050727     c                   eval      PICina = §STD4ind
033200050727     c                   eval      PICcaa = §STD4cap
033300050727     c                   eval      PICloa = §STD4loc
033400050727     c                   eval      PICpra = §STD4prv
033500050802     c                   eval      PICdal = 'Bologna, '
033600050727     c                                    + %editc(*date:'Y')
033700050802     c                   write     DN88pI
033800050801      *
033900050801     c                   do        2
034000050802     c                   write     DN88p0
034100050801     c                   enddo
034200050727      *
034300050727      * Stampa oggetto lettera
034400050914     c                   reset                   WLBdat
034500050914     c                   eval      G08inv = (H2Caae * 10000) + H2Cmge
034600050914     c                   call      'XSRDA8'
034700050914     c                   parm                    WLBdat
034800050727     c                   eval      POCogg = 'Evento nr. '
034900050727     c                                    + %trim(%editc(H2Cnev:'Z'))
035000050914     c                                    + ' del '
035100050914     c                                    + %trim(%editc(G08dat:'Y'))
035200050914     c                                    + '         '
035300050727     c                                    + 'Tipo anomalia: '
035400050923     c                                    + DETtad
035500050727     c                                    + '  '
035600050727     c                                    + %trim(§TADdest)
035700050802     c                   write     DN88pO
035800050801      *
035900050801     c                   eval      POCogg = 'P.O. che ha segnalato +
036000050801     c                                       l''evento:'
036100050801     c                                    + %editw(DETfce:'0   ')
036200050801     c                                    + '  '
036300050801     c                                    + %trim(ORGdes)
036400050802     c                   write     DN88pO
036500050802      *
036600050802      * Stampa inizio lettera
036700050802     c                   write     DN88pT
036800050727      *
036900050727     c                   ENDSR
037000050727      *
037100050727      *---------------------------------------------------------------*
037200051025      * Stampa elenco C.A. per richiesta recupero ad A.I.G.           *
037300050727      *---------------------------------------------------------------*
037400050727     c     StampaCA      BEGSR
037500050727      *
037600050727      * Stampa intestazione lettera
037700050727if  1c                   if        $First = *on
037800050802     c                   exsr      StampaIni
037900050727e   1c                   endif
038000050728      *
038100050802     c                   clear                   DN88pD
038200050727      *
038300051025      * Reperimento dati richiesta recupero
038400050727     c     K03DRA01      chain     FNDRA000
038500050727if  1c                   if        NOT %found(FNDRA01L)
038600050914     c                             or  DRAatb <> *blanks
038700050727     c                   clear                   FNDRA000
038800050727     c                   eval      DRAaac = H2Caac
038900050727     c                   eval      DRAfil = S2Cfil
039000050727     c                   eval      DRAnca = S2Cnca
039100050727e   1c                   endif
039200050727      *
039300050727      * Reperimento dati C.A.
039400050727     c     K03DRA01      chain     FNDCT000
039500050727if  1c                   if        NOT %found(FNDCT01L)
039600050914     c                             or  DCTatb <> *blanks
039700050727     c                   clear                   DCTmgc
039800050804     c                   clear                   DCTprn
039900050804     c                   clear                   DCTpra
040000050727     c                   clear                   DCTflo
040100050727e   1c                   endif
040200050727     c                   movel     DCTflo        dDCT01
040300050727      *
040400050727      * Reperimento dati LdV
040500050727     c     K04TAS30      chain     TITAS30C
040600050914if  1c*** impossibile:   if        NOT %found(TITAS30C)
040700050727     c***                clear                   TAS...
040800050727e   1c***                endif
040900050727      * - se tipo porto "recupero": rileggo
041000050727     c                   clear                   dsTB
041100050727     c                   movel     'TB'          TBLcod
041200050727     c                   movel(P)  TAStbl        TBLkey
041300050727     c     K03TABEL      chain     TABEL
041400050914if  1c                   if            %found(TABEL00F)
041500050914     c                             and TBLflg =  *blanks
041600050727     c                   movel     TBLuni        dsTB
041700050727e   1c                   endif
041800050727if  1c                   if        §TBrbl = 'R'
041900050727     c     K04TAS30      reade     TITAS30C
042000050727e   1c                   endif
042100050727      *
042200050727      * Decodifica beneficiario
042300050727     c                   exsr      sr_Benef
042400050727      *
042500050727      * Stampa elenco C.A.
042600050727      * - impostazione dati singola C.A.
042700050727      *   - Numero e data C.A.
042800050728     c                   eval      POCnca = %trim(%editw(DRAfil:'0   '))
042900050728     c                                    + '/'
043000050728     c                                    + %trim(%editc(DRAnca:'Z'))
043100050728     c                   reset                   WLBdat
043200050728     c                   eval      G08inv = (DCTaac * 10000) + DCTmgc
043300050728     c                   call      'XSRDA8'
043400050728     c                   parm                    WLBdat
043500050728if  1c                   if        G08err = *off
043600050728     c                   eval      POCdca = G08dat
043700050728e   1c                   endif
043800050804      *   - Numero e data Pratica Assicurativa
043900050804if  1c                   if        DCTprn > *zeros
044000050804     c                   eval      POCprn = 'Pratica assicurativa nr. '
044100050804     c                                    + %trim(%editc(DCTprn:'Z'))
044200050804     c                                    + ' Anno '
044300050804     c                                    + %trim(%editc(DCTpra:'Z'))
044400050804e   1c                   endif
044500050804      *
044600050728      *   - Importo liquidato
044700050728     c                   eval      POCipl = %trim(%editc(S2Cipl:'1'))
044800050728     c                                    + ' '
044900050728     c                                    + §GEDdba
045000050727      *
045100050727      * - test OF
045200050727if  1c                   if        *in01  = *on
045300050802     c                   write     DN88pP
045400050727do  2c                   do        2
045500050802     c                   write     DN88p0
045600050727e   2c                   enddo
045700050727     c                   eval      *in01  = *off
045800050727e   1c                   endif
045900050727      * - stampa dati singola C.A.
046000050802     c                   write     DN88pD
046100050727      *
046200050727      * Aggiornamento flag DETFTR in FNDET00F (per evento)
046300050727     c                   if        $First = *on
046400050727     c                   eval      DETftr = 'S'
046500050727     c                   UPDATE    FNDET000
046600050727     c                   eval      $First = *off
046700050727     c                   endif
046800050727      *
046900050727      * Aggiornamento flag DRAESC in FNDRA00F (per singola C.A.)
047000050728     c                   move      wDate         DRAdrr
047100050728if  1c                   if        NOT %found(FNDRA01L)
047200050728     c                   WRITE     FNDRA000
047300050728x   1c                   else
047400050727     c                   UPDATE    FNDRA000
047500050728e   1c                   endif
047600050727      *
047700050727     c                   ENDSR
047800050728      *
047900050728      *---------------------------------------------------------------*
048000050802      * Stampa piede lettera                                          *
048100050728      *---------------------------------------------------------------*
048200050802     c     StampaFin     BEGSR
048300050728      *
048400050802      * Stampa fine lettera
048500051026     c                   write     DN88pF
048600050728      *
048700050728     c                   ENDSR
048800050727      *
048900050727      *---------------------------------------------------------------*
049000050727      * Decodifica / Controllo TIPO ANOMALIA                          *
049100050727      *---------------------------------------------------------------*
049200050727     c     DecodTAD      BEGSR
049300050727      *
049400050727     c                   clear                   dTAD
049500050727     c                   reset                   TIBS02ds
049600050727     c                   movel     KNSIF         T02sif
049700050727     c*** già impostato: movel     'TAD'         T02cod
049800050727     c                   movel(p)  wTAD          T02ke1
049900050727     c                   call      'TIBS02R'
050000050727     c                   parm                    KPJBA
050100050727     c                   parm                    TIBS02ds
050200050727     c                   if        T02err   = *blanks
050300050727     c                   movel     T02uni        dTAD
050400050727     c                   endif
050500050727      *
050600050727     c                   if        §TADdest = *blanks
050700050727     c                   movel(p)  §TADdesc      §TADdest
050800050727     c                   endif
050900050727      *
051000050727     c                   ENDSR
051100050727      *
051200050727      *---------------------------------------------------------------*
051300050727      * Decodifica P.O. che ha Comunicato l'Evento                    *
051400050727      *---------------------------------------------------------------*
051500050727     c     DecodFCE      BEGSR
051600050727      *
051700050727     c     DETfce        chain     AZORG
051800050727     c                   if        NOT %found(AZORG01L)
051900050727     c                             or  ORGfva <> *blanks
052000050727     c                   clear                   ORGdes
052100050727     c                   endif
052200050727      *
052300050727     c                   ENDSR
052400050727      *
052500050727      *---------------------------------------------------------------*
052600050727      * Reperimento dati cliente beneficiario                         *
052700050727      *---------------------------------------------------------------*
052800050727     c     sr_Benef      BEGSR
052900050727      *
053000050727     c                   clear                   wCli
053100050727     c                   reset                   $Rivalsa
053200050727      *
053300050727      * Dalla spedizione determino il codice beneficiario
053400050727      *
053500050727      * Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con
053600050727      *  quello C.A. tranne se la sped. è "Mamma di cambio porto" e mi trovo
053700050727      *  in filiale, in questo caso inverto il porto C.A.
053800050727sel 1c                   select
053900110504w   1c                   when      §DCTCCA   <> '9'
054000050727     c                   eval      w§DCTport =  §DCTport
054100050727w   1c                   when      §DCTport  =  'A'
054200050727     c                   eval      w§DCTport =  'F'
054300050727w   1c                   when      §DCTport  =  'F'
054400050727     c                   eval      w§DCTport =  'A'
054500050727e   1c                   endsl
054600050727      *
054700050727sel 1c                   select
054800050727      * - destinatario / pf
054900050727w   1c                   when          DCTptr    = 'D'
055000050727     c                             and w§DCTport = 'F'
055100050727      * - mittente     / pa
055200050727w   1c                   when          DCTptr    = 'M'
055300050727     c                             and w§DCTport = 'A'
055400050727     c                   eval      wCli      =  TASccm
055500050727x   1c                   other
055600050727     c                   eval      wCli      =  TASksc
055700050727e   1c                   endsl
055800050727      *
055900050727      * Scorporo i codici cliente
056000050727     c                   move      wCli          w0040a
056100050727     c                   move      DCTksc        w0040b
056200050727      *
056300050727      * Controllo se ho rivalsa
056400050727     c                   eval      DKAtrc    =  'R'
056500050728     c     K04DKA01      chain     FNDKA000
056600050914if  1c                   if            %found(FNDKA01L)
056700050914     c                             and DKAatb =  *blanks
056800050727     c                   eval      $Rivalsa  =  *on
056900050727e   1c                   endif
057000050727      *
057100050727      *
057200050727sel 1c                   SELECT
057300050727      * Rivalsa
057400050727      * -------
057500050727w   1c                   WHEN      $Rivalsa  =  *on
057600050727     c                   exsr      Benef_DKA
057700050727     *** *   se beneficiario alternativo codificato memorizzo il codice
057800050727     *** *   per ricavare la società
057900050727     ***c                   if             w0040b >  *zeros
058000050727     ***c                             and  w0040b <> 8888
058100050727     ***c                   eval      wCli      =  DCTksc
058200050727     ***c                   endif
058300050727      *
058400050727      * Beneficiario alternativo generico
058500050727      * ---------------------------------
058600050727w   1c                   WHEN      w0040b    =  8888
058700050727     c                   eval      DKAtrc    =  'C'
058800050727     c     K04DKA01      chain     FNDKA000
058900050914if  2c                   if            %found(FNDKA01L)
059000050914     c                             and DKAatb =  *blanks
059100050727     c                   exsr      Benef_DKA
059200050728e   2c                   endif
059300050727      *
059400050727      * Beneficiario alternativo codificato
059500050727      * -----------------------------------
059600050727w   1c                   WHEN      DCTksc    >  *zeros
059700050727     c                   eval      wCli      =  DCTksc
059800050727     c                   clear                   TIBS69ds
059900050727     c                   eval      I69kac    =  wCli
060000050727     c                   eval      I69kin    =  wCli
060100050727     c                   eval      I69kcs    =  wCli
060200050728     c                   exsr      Rep_Anagra
060300050727     c                   movel     ACOrag        POCrsb
060400050727     c                   movel     INDvia        POCinb
060500050727     c                   movel     INDcae        POCcab
060600050727     c                   movel     INDcit        POClob
060700050727if  2c                   if        INDsta    =  *blanks
060800050727     c                   movel     INDprv        POCprb
060900050727x   2c                   else
061000050727     c                   movel     INDsta        POCnab
061100050727e   2c                   endif
061200050727      * - controllo se esiste luogo 6
061300050727     c     K03SPE01      chain     FNSPE000
061400050914if  2c                   if            %found(FNSPE01L)
061500050914     c                             and SPEflg  = *blanks
061600050727     c                   movel     SPErag        POCrsb
061700050727     c                   movel     SPEind        POCinb
061800050727     c                   movel     SPEcap        POCcab
061900050727     c                   movel     SPEloc        POClob
062000050727if  3c                   if        SPEnaz    =  *blanks
062100050727     c                   movel     SPEpro        POCprb
062200050727x   3c                   else
062300050727     c                   movel     SPEnaz        POCnab
062400050727e   3c                   endif
062500050727e   2c                   endif
062600050727      *
062700050727      * Cliente in spedizione
062800050727      * ---------------------
062900050727x   1c                   OTHER
063000050727     c                   clear                   TIBS69ds
063100050727     c                   eval      I69kcs    =  wCli
063200050728     c                   exsr      Rep_Anagra
063300050727     c                   eval      *in10 = *off
063400050727if  2c                   if             w0040a >  *zeros
063500050727     c                             and  w0040a <> 8888
063600050727     c                             and  w0040a <> 9999
063700050727      * - indirizzo da luogo 6
063800050727     c     K03SPE01      chain     FNSPE000                           10
063900050727x   2c                   else
064000050727     c                   eval      *in10     =  *on
064100050727e   2c                   endif
064200050727if  2c                   IF        not *in10
064300050727     c                   movel     SPErag        POCrsb
064400050727     c                   movel     SPEind        POCinb
064500050727     c                   movel     SPEcap        POCcab
064600050727     c                   movel     SPEloc        POClob
064700050727if  3c                   if        SPEnaz    =  *blanks
064800050727     c                   movel     SPEpro        POCprb
064900050727x   3c                   else
065000050727     c                   movel     SPEnaz        POCnab
065100050727w   3c                   endif
065200050727x   2c                   ELSE
065300050727      * - Aggancio file mittenti/destinatari
065400050727     c     K04TAS30      chain     TNTMD000
065500050727if  3c                   if        NOT %found(TNTMD01L)
065600050727     c                   clear                   ds_TNTMD
065700050727e   3c                   endif
065800050727      *   - indirizzo da destinatario spedizione
065900050727if  3c                   if        DCTptr    =  'D'
066000050727     c                   movel     TMDrsd        POCrsb
066100050727     c                   movel     TMDind        POCinb
066200050727     c                   movel     TMDcad        POCcab
066300050727     c                   movel     TMDlod        POClob
066400050727if  4c                   if        TMDnzd    =  *blanks
066500050727     c                   movel     TMDprd        POCprb
066600050727x   4c                   else
066700050727     c                   movel     TMDnzd        POCnab
066800050727e   4c                   endif
066900050727x   3c                   else
067000050727      *   - indirizzo da mittente spedizione
067100050727     c                   movel     TMDrsm        POCrsb
067200050727     c                   movel     TMDinm        POCinb
067300050727     c                   movel     TMDcam        POCcab
067400050727     c                   movel     TMDlom        POClob
067500050727if  4c                   if        TMDnzm    =  *blanks
067600050727     c                   movel     TMDprm        POCprb
067700050727x   4c                   else
067800050727     c                   movel     TMDnzm        POCnab
067900050727e   4c                   endif
068000050727e   3c                   endif
068100050727      *
068200050727e   2c                   ENDIF
068300050727      *
068400050727e   1c                   ENDSL
068500050727      *
068600050727     c                   ENDSR
068700050727      *
068800050727      *---------------------------------------------------------------*
068900050727      * Decodifica beneficiario da FNDKA                              *
069000050727      *---------------------------------------------------------------*
069100050727     c     Benef_DKA     BEGSR
069200050727      *
069300050728     c                   movel     DKArag        POCrsb
069400050728     c                   movel     DKAvia        POCinb
069500050728     c                   movel     DKAcap        POCcab
069600050728     c                   movel     DKAloc        POClob
069700050727     c                   if        DKAnaz = *blanks
069800050728     c                   movel     DKAprv        POCprb
069900050727     c                   else
070000050728     c                   movel     DKAnaz        POCnab
070100050727     c                   endif
070200050727      *
070300050727     c                   ENDSR
070400050728      *
070500050728      *---------------------------------------------------------------*
070600050728      * Reperimento dati anagrafici                                   *
070700050728      *---------------------------------------------------------------*
070800050728     c     Rep_Anagra    BEGSR
070900050728      *
071000050728     c                   clear                   ds_CNACO
071100050728     c                   clear                   ds_CNIND
071200050728     c                   clear                   ds_CNCLP
071300050728     c                   clear                   ds_FNCLS
071400050728      *
071500050728     c                   call      'TIBS69R'
071600050728     c                   parm                    TIBS69ds
071700050728     c                   parm                    ds_CNACO
071800050728     c                   parm                    ds_CNIND
071900050728     c                   parm                    ds_CNCLP
072000050728     c                   parm                    ds_FNCLS
072100050728      *
072200050728     c                   eval      *in35 = (O69err = *on)
072300050728      *
072400050728     c                   eval      $TIBS69R = *on
072500050728      *
072600050728     c                   ENDSR
