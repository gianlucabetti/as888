000100050923      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050923      *===============================================================*
000300050923      *?TNTA11R * Interrogazione variazioni tariffe                  ?*
000400050923      *===============================================================*
000500050923
000600050923     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700050923
000800050923      *---------------------------------------------------------------*
000900050923
001000050923     fTABEL00F  if   e           k disk
001100051003     fAZUTE01L  if   e           k disk
001200051104     fCNCLP01L  if   e           k disk
001300050923     fTITAV01L  if   e           k disk
001400050923      *
001500050923     fTNTA11D   cf   e             workstn
001600050923     f                                     sfile(TA11S01:S01nrr)
001700050923     f                                     infds(DSFMT)
001800050923
001900050923      *---------------------------------------------------------------*
002000050923
002100050923      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002200050923      *
002300050923     d DigitN          c                   const('0123456789')
002400050923      * - Tasti di funzione
002500050923     d Enter           c                   const(x'F1')
002600050923     d RollDwn         c                   const(x'F4')
002700050923     d RollUp          c                   const(x'F5')
002800050923      * - Nr. di righe del sfl (SFLPAG)
002900050930     d C_SflPag        c                   const(14)
003000050923      *
003100050923      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003200050923      *
003300050923      * - Messaggi di errore
003400090701     d $Msg            s             78    dim( 8) ctdata  perrcd(1)
003500050927      *
003600050927     d $TarP_Cod       s              2    dim(50)  inz
003700050927     d $TarP_Des       s             20    dim(50)  inz
003800050927     d $TarP_Dfe       s              1    dim(50)  inz
003900050923      *
004000050923      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004100050923      *
004200050923     d KPJBA         e ds
004300050923      * - Parametri x Controllo profilo utenti
004400050923     d TIBS34ds      e ds                  inz
004500050923      * - Ds di riferimento al file esterno AZUTE00F
004600050923     d AZUTEds       e ds                  inz extname(AZUTE00F)
004700050923      * - Ds per dati organigramma
004800050923     d DDatiUte      e ds                  inz
004900090630
005000090630     d TNTAA1DS      e ds                  inz
005100090630
005200050923      * - Reperimento/Interrogazione tabelle
005300050923     d TIBS02ds      e ds                  inz
005400050923     d   T02mod      e                     inz('C')
005500050923     d   T02cod      e                     inz('LAT')
005600050923      * - Tab. "LAT"
005700050923     d dLAT          e ds                  inz
005800050923      * - Tab. "01"
005900050923     d ds01          e ds                  inz
006000050927      * - Tab. "1P"
006100050927     d ds1P          e ds                  inz
006200050927      * - Tab. "SP"
006300050927     d dsSP          e ds                  inz
006400050927      * Decodifica clienti
006500050927     d TIBS69ds      e ds                  inz
006600050927     d   I69tla      e                     inz('L')
006700050927     d ds_CNACO      e ds                  extname(CNACO00F) inz
006800050927     d ds_CNIND      e ds                  extname(CNIND00F) inz
006900050927     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
007000050927     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
007100050923      *
007200050923     d Status         sds           333
007300050923     d   SDSpgm          *proc
007400050923      *
007500050923     d DSFMT           ds           512
007600050923     d  £Tasto               369    369
007700050923     d  £SFLnrr              378    379B 0
007800050923      *
007900050923      * - Controllo/Inversione date
008000050923     d WLBdat          ds                  inz
008100050923     d   G08dat                       8  0 inz
008200050923     d   G08inv                       8  0 inz
008300050923     d   G08err                       1    inz('3')
008400050923     d   G08tgi                       5  0 inz
008500051003      *
008600051003      * - Ridefinizione profili utente in schiera
008700051003     d ds_Utenti       ds            50    inz
008800051003     d   V1Cpu1                            inz
008900051003     d   V1Cpu2                            inz
009000051003     d   V1Cpu3                            inz
009100051003     d   V1Cpu4                            inz
009200051003     d   V1Cpu5                            inz
009300051003     d   $Usr                  1     50    inz dim( 5)
009400050923      *
009500050923      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
009600050923      *
009700050923      * - Flags
009800050927     d $EoF            s              1    inz(*off)
009900050927     d $EoF2           s              1    inz(*off)
010000050923     d $Fine           s              1    inz(*off)
010100050923     d $InzD01         s              1    inz(*on)
010200050923     d $InzS01         s              1    inz(*on)
010300050927     d $ToWrite        s              1    inz(*off)
010400050923      *
010500050923     d $Video          s              1    inz('1')
010600050923      *
010700050923      * - Indici di schiera
010800050927     d XX              s              3  0 inz
010900050923      *
011000050923      * - Variabili riferite al data base o al display file
011100050923     d S01nrr          s                   like(C01rcd) inz
011200050923     d W1Cage          s                   like(CLPksc) inz
011300050923     d W1Cdti          s                   like(TAVdav) inz
011400050923     d W1Cdtf          s                   like(TAVdav) inz
011500050923     d Wabilit         s                   like(UTEaut) inz
011600050930     d SAVksc          s                   like(TAVksc) inz
011700050930     d SAVctr          s                   like(TAVctr) inz
011800050930     d SAVprg          s                   like(TAVprg) inz
011900050923      *
012000050923      * - Variabili definite a programma
012100050923     d wPag            s              4  0 inz
012200050923     d wRig            s              3  0 inz
012300050927     d W_SflPag        s              3  0 inz
012400050930     d Wpo             s              3    inz(*zeros)
012500050923     d Wcampo          s             10    inz
012600050923      *
012700050923      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
012800050923      *
012900050927      * - TABEL00F
013000050927     c     K03TABEL      klist
013100050927     c                   kfld                    TBLkut
013200050927     c                   kfld                    TBLcod
013300050927     c                   kfld                    TBLkey
013400050927     c     K02TABEL      klist
013500050927     c                   kfld                    TBLkut
013600050927     c                   kfld                    TBLcod
013700051104      * - CNCLP01L
013800051104     c     K03CLP01      klist
013900050930     c                   kfld                    TBLkut
014000050930     c                   kfld                    DUTkci
014100051104     c                   kfld                    CLPage
014200050923     c*** NO:            kfld                    CLPksc
014300050923
014400050923      *---------------------------------------------------------------*
014500050923      *  RIEPILOGO INDICATORI                                         *
014600050923      *---------------------------------------------------------------*
014700050923      * 10    - Comodo                                                *
014800050923      * 28    - Emette il messaggio di errore a video                 *
014900050923      * 30    - Ripulisce il subfile S01                              *
015000050923      * 31    - NON emette il subfile S01                             *
015100050923      * 32    - Record modificato nel subfile S01 (sflnxtchg)         *
015200050923      * 33    - Fine dati nel subfile S01         (sflend)            *
015300050923      * 41    - S01: Condiziona DSPATR(RI)                            *
015400050930      * 42    -  "   Condiziona DSPATR(HI)                            *
015500050923      * 51    - D01: Codice commerciale errato                        *
015600050923      * 52    -  "   Data inizio periodo errata                       *
015700050923      * 53    -  "   Data fine   periodo errata                       *
015800050923      * 54    -  "   1° profilo utente errato                         *
015900050923      * 55    -  "   2° profilo utente errato                         *
016000050923      * 56    -  "   3° profilo utente errato                         *
016100050923      * 57    -  "   4° profilo utente errato                         *
016200050923      * 58    -  "   5° profilo utente errato                         *
016300050923      * 90    - Generico di errore                                    *
016400050923      *---------------------------------------------------------------*
016500050923
016600050923     c     *Entry        plist
016700050923     c                   parm                    KPJBA
016800050923      *
016900050923      * Operazioni Iniziali
017000050923     c                   exsr      RoutInz
017100050923      *
017200050923      * Gestione Video
017300050923do  1c                   DOW       $Fine    = *off
017400050923      * - Filtro di lancio
017500050923cas 2c     $Video        caseq     '1'           GesD01
017600050923      * - SubFile
017700050923cas 2c     $Video        caseq     '2'           GesS01
017800050923e   2c                   endcs
017900050923e   1c                   ENDDO
018000050923      *
018100050923      * Fine
018200050923     c                   clear                   TIBS02ds
018300050923     c                   eval      T02tla   = 'C'
018400050923     c                   call      'TIBS02R'
018500050923     c                   parm                    KPJBA
018600050923     c                   parm                    TIBS02ds
018700090630     C*
018800090630     C                   clear                   tntaa1ds
018900090630     C                   movel     'C'           Itaa1Tla
019000090630     C                   movel(p)  tntaa1ds      kpjbu
019100090703     C                   CALL      'TNTAA1C'
019200090630     C                   PARM                    KPJBA
019300090630     c
019400050923     c                   movel     *on           *inLR
019500050923      *
019600050923      *---------------------------------------------------------------*
019700050923      * Operazioni Iniziali                                           *
019800050923      *---------------------------------------------------------------*
019900050923     c     RoutInz       BEGSR
020000050923      *
020100050923      * Reperisce dati job
020200050923     c                   exsr      DatiJob
020300090701     C* CONTROLLO se l'utente è abilitato alla gestione tariffe
020400090701     c                   clear                   tntaa1ds
020500090701     c                   eval      itaa1caut='§utegtc'
020600090701     c                   clear                   kpjbu
020700090701     c                   movel     tntaa1ds      kpjbu
020800090703     c                   call      'TNTAA1C'
020900090701     c                   parm                    kpjba
021000090701     c                   movel     kpjbu         tntaa1ds
021100090701     c
021200090701     c* Utente non abilitato alla gestione tariffe
021300090701     c                   if        otaa1fabi='N'
021400090701     c                   Seton                                        08
021500090701e   2c                   endif
021600050927      *
021700050927      * Impostazione dati di testata a video
021800050927     c                   movel     SDSpgm        V1Tpgm
021900050927      *
022000050927      * Caricamento codici e descrizione delle tariffe particolari
022100050927      *   da tabella 1P
022200050927     c                   eval      TBLkut   = 1
022300050927     c                   eval      TBLcod   = '1P'
022400050927     c                   clear                   xx
022500050927     c     K02TABEL      setll     TABEL
022600050927     c     K02TABEL      reade     TABEL
022700050927do  1c                   DOW       NOT %eof(TABEL00F)
022800050927      *
022900050927if  2c                   IF        TBLflg   = *blanks
023000050927      *
023100050927      * - Controllo validità tariffa particolare x la gestione tariffe
023200050927     c                   movel     TBLuni        ds1P
023300050927      * - Flag di comodo per tabella 'SP'
023400050927if  3c                   if        §1Puta   = 'S'
023500050927     c                   add       1             xx
023600050927     c                   movel     TBLkey        $TarP_Cod(xx)
023700050927     c                   movel     §1Pdes        $TarP_Des(xx)
023800050927     c                   movel     §1Pdfe        $TarP_Dfe(xx)
023900050927e   3c                   endif
024000050927      *
024100050927e   2c                   ENDIF
024200050927      *
024300050927     c     K02TABEL      reade     TABEL
024400050927e   1c                   ENDDO
024500050927      *
024600050927      * - Per tariffe FedEx controllo se devo recupare la descrizione
024700050930      *   dalla tabella "SP"
024800050927     c                   clear                   xx
024900050927do  1c                   DOW       xx          < %elem($TarP_Cod)
025000050927     c                   add       1             xx
025100050927if  2c                   if        $TarP_Cod(xx)  = *blanks
025200050927     c                   leave
025300050927e   2c                   endif
025400050927if  2c                   if        $TarP_Dfe(xx) <> 'S'
025500050927     c                   iter
025600050927e   2c                   endif
025700050927     c                   clear                   dsSP
025800050927     c                   movel     'SP'          TBLcod
025900050927     c                   movel(p)  $TarP_Cod(xx) TBLkey
026000050927     c     K02TABEL      chain     TABEL
026100050927if  2c                   If            %found(TABEL00F)
026200050927     c                             and TBLflg  = *blanks
026300050927     c                   movel     TBLuni        dsSP
026400050927if  3c                   if        §SPdfe     <> *blanks
026500050927     c                   movel     §SPdfe        $TarP_Des(xx)
026600050927e   3c                   endif
026700050927e   2c                   EndIf
026800050927e   1c                   ENDDO
026900050923      *
027000050923     c                   ENDSR
027100050923      *
027200050923      *---------------------------------------------------------------*
027300050923      * Reperimento Dati del job (Utente/Operativi)                   *
027400050923      *---------------------------------------------------------------*
027500050923     c     DatiJob       BEGSR
027600050923      *
027700050923     c     *dtaara       define    §azute        azuteds
027800050923     c     *dtaara       define    §datiute      ddatiute
027900050923      *
028000050923     c                   in(E)     *dtaara
028100050923     c                   IF        %ERROR or RSUT = *blanks
028200050923     c                   clear                   Tibs34Ds
028300050923     c                   call      'TIBS34R'
028400050923     c                   parm                    Tibs34Ds
028500050923     c                   in        *dtaara
028600050923     c                   ENDIF
028700050923      *
028800050923     c                   ENDSR
028900050923      *
029000050923      *---------------------------------------------------------------*
029100050923      * Gestione videata D01                                          *
029200050923      *---------------------------------------------------------------*
029300050923     c     GesD01        BEGSR
029400050923      *
029500050923      * Inizializzo la videata
029600050923if  1c                   if        $InzD01     = *on
029700050923     c                   exsr      InzD01
029800050923     c                   movel     *off          $InzD01
029900050923e   1c                   endif
030000050923      *
030100050923      * Reimposto il codice comm.le a *blank se *zero (numerico editato)
030200050923if  2c                   if        V1Cage      = *zeros
030300050923     c                   eval      V1Cage      = *blanks
030400050923e   2c                   endif
030500050923      *
030600050923      * Scrivo la testata e la riga tasti funzionali abilitati
030700050923if  1c                   if        NOT *in90
030800050923     c                   write     TA11T01
030900050923     c                   write     TA11Z01
031000050923e   1c                   endif
031100050923      * Emetto la videata
031200050923     c                   exfmt     TA11D01
031300050923     c                   setoff                                       28  90
031400050923     c                   clear                   V1Dmsg
031500050923      *
031600050923sel 1c                   select
031700050923      * F3=Fine
031800090701w   1c                   when      *inKC or *in08
031900050923     c                   exsr      F03D01
032000050923     c                   leavesr
032100050923      *
032200050923e   1c                   endsl
032300050923      *
032400050923      * Controllo dati immessi a video
032500050923     c                   exsr      CtrD01
032600050923     c                   if        *in90
032700050923     c                   leavesr
032800050923     c                   endif
032900050923      *
033000050923      * Passaggio al subfile
033100050923     c                   eval      $InzS01     = *on
033200050923     c                   eval      $Video      = '2'
033300050923      *
033400050923     c                   ENDSR
033500050923      *
033600050923      *---------------------------------------------------------------*
033700050923      * Inizializzazione videata D01                                  *
033800050923      *---------------------------------------------------------------*
033900050923     c     InzD01        BEGSR
034000050923      *
034100050923     c                   clear                   TA11D01
034200090701     c* Errore utente non abilitato
034300090701     c                   if        *in08
034400090701     c                   Seton                                        5128
034500090701     c                   movel     $MSG(8)       v1dmsg
034600090701     c                   endif
034700050923      *
034800050923      * Imposto i dati di default
034900050923     c***                move      '?'           V1Cage
035000050923      *
035100050923     c                   ENDSR
035200050923      *
035300050923      *---------------------------------------------------------------*
035400050923      * Gestione tasto funzionale F03 da videata D01                  *
035500050923      *---------------------------------------------------------------*
035600050923     c     F03D01        BEGSR
035700050923      *
035800050923      * Chiudo il programma
035900050923     c                   eval      $Fine       = *on
036000050923      *
036100050923     c                   ENDSR
036200050923      *
036300050923      *---------------------------------------------------------------*
036400050923      * Controllo dati immessi in videata D01                         *
036500050923      *---------------------------------------------------------------*
036600050923     c     CtrD01        BEGSR
036700050923      *
036800050923     c                   movea     *zeros        *in(50)
036900050923     c                   clear                   W1Cage
037000050927     c                   clear                   W1Cdti
037100050927     c                   clear                   W1Cdtf
037200050923      *
037300050923      * Codice Commerciale
037400051003     c                   clear                   V1Dage
037500050923      * - se *blanks lo considero *zeros (sarebbe numerico)
037600050923if  1c                   if        V1Cage      = *blanks
037700050923     c                   eval      V1Cage      = *zeros
037800050923e   1c                   endif
037900050923      * - ricerca
038000050923     c     '?'           scan      V1Cage                                 10
038100050923if  1c                   if        *in10       = *on
038200050923     c                   eval      V1Cage      = *zeros
038300050923     c                   eval      Wcampo      = 'V1CAGE    '
038400050923     c                   exsr      srSEARCH
038500050923e   1c                   endif
038600050923      * - controlli:
038700050923      * - - effettiva immissione
038800050923     c                   clear                   ds01
038900050923if  1c                   IF        V1Cage      = *zeros
039000050923     c                   seton                                        512890
039100050923     c                   eval      V1Dmsg      = $Msg(1)
039200050923     c                   leavesr
039300050923x   1c                   ELSE
039400050923      * - - numericità
039500050923     c     DigitN        check     V1Cage                                 10
039600050923if  2c                   if        *in10       = *on
039700050923     c                   seton                                        512890
039800050923     c                   eval      V1Dmsg      = $Msg(2)
039900050923     c                   leavesr
040000050923e   2c                   endif
040100050923      * - - validità
040200050930      * - - - esistenza in tab. "01"
040300050923     c                   eval      TBLcod      = '01'
040400050923     c                   movel(p)  V1Cage        TBLkey
040500050923     c     K03TABEL      chain     TABEL
040600050923if  2c                   if        NOT %found(TABEL00F)
040700050923     c                             or  TBLflg <> *blanks
040800050923     c                   seton                                        512890
040900050923     c                   eval      V1Dmsg      = $Msg(2)
041000050923     c                   leavesr
041100050923e   2c                   endif
041200050923     c                   movel     TBLuni        ds01
041300051003     c                   movel     §01age        V1Dage
041400050923      * - - - appartenenza a P.O. gestito dall'utente
041500090630     c                   clear                   tntaa1ds
041600090630     c                   eval      itaa1caut='§utegtc'
041700090630     c                   movel     v1cage        itaa1cmm
041800090630     c                   clear                   kpjbu
041900090630     c                   movel     tntaa1ds      kpjbu
042000090703     c                   call      'TNTAA1C'
042100090630     c                   parm                    kpjba
042200090630     c                   movel     kpjbu         tntaa1ds
042300090630     c
042400090630    2c                   if        otaa1fabi='N'
042500090701     c
042600090630     c**                 movel     V1Cage        wPO
042700090630     c**   wPO           lookup    SKpog                                  10
042800090630if  2c**                 if        NOT *in10
042900050930      * - - - - se non trovato: provo con il comm.le unificante
043000090630     c**                 movel     §01rgf        wPO
043100090630     c**   wPO           lookup    SKpog                                  10
043200090630if  3c**                 if        NOT *in10
043300090630     c
043400050923     c                   seton                                        512890
043500090630     c                   eval      V1Dmsg = $Msg(7)
043600050923     c                   leavesr
043700090630e    c                   endif
043800090630e    c                   endif
043900090630e    c***                ENDIF
044000050923      *
044100050923      * Periodo
044200050923      * - 1ª data
044300050923     c                   clear                   W1Cdti
044400050923if  1c                   if        V1Cdti      = *zeros
044500050923     c                   seton                                        522890
044600050923     c                   eval      V1Dmsg      = $Msg(3)
044700050923     c                   leavesr
044800050923x   1c                   else
044900050923     c                   clear                   WLBdat
045000050923     c                   eval      G08dat      = V1Cdti
045100050923     c                   call      'XSRDA8'
045200050923     c                   parm                    WLBdat
045300050923if  2c                   if        G08err      = *on
045400050923     c                   seton                                        522890
045500050923     c                   eval      V1Dmsg      = $Msg(4)
045600050923     c                   leavesr
045700050923e   2c                   endif
045800050923     c                   eval      V1Cdti      = G08dat
045900050923     c                   eval      W1Cdti      = G08inv
046000050923e   1c                   endif
046100050923      * - 2ª data
046200050923     c                   clear                   W1Cdtf
046300050923if  1c                   if        V1Cdtf      = *zeros
046400050923     c                   seton                                        532890
046500050923     c                   eval      V1Dmsg      = $Msg(3)
046600050923     c                   leavesr
046700050923x   1c                   else
046800050923     c                   clear                   WLBdat
046900050923     c                   eval      G08dat      = V1Cdtf
047000050923     c                   call      'XSRDA8'
047100050923     c                   parm                    WLBdat
047200050923if  2c                   if        G08err      = *on
047300050923     c                   seton                                        532890
047400050923     c                   eval      V1Dmsg      = $Msg(4)
047500050923     c                   leavesr
047600050923e   2c                   endif
047700050923     c                   eval      V1Cdtf      = G08dat
047800050923     c                   eval      W1Cdtf      = G08inv
047900050923e   1c                   endif
048000050923      * - range
048100050927if  1c                   if        W1Cdtf     <= *zeros
048200050927     c                   eval      W1Cdtf      = W1Cdti
048300050923e   1c                   endif
048400050927if  1c                   if        W1Cdti      > W1Cdtf
048500050923     c                   seton                                        522890
048600050923     c                   eval      V1Dmsg      = $Msg(5)
048700050923     c                   leavesr
048800050923e   1c                   endif
048900050923      *
049000050923      * Profili utente
049100051003     c                   clear                   xx
049200051003do  1c                   dow       xx          < %elem($Usr)
049300051003     c                   add       1             xx
049400051003if  2c                   if        $Usr(xx)   <> *blanks
049500051003     c     $Usr(xx)      chain     AZUTE000
049600051003if  3c                   if        NOT %found(AZUTE01L)
049700051003     c                   eval      *in(53+xx)  = *on
049800051003     c                   seton                                        28  90
049900051003     c                   eval      V1Dmsg      = $Msg(6)
050000051003     c                   leavesr
050100051003e   3c                   endif
050200051003e   2c                   endif
050300051003e   1c                   enddo
050400050923      *
050500050923     c                   ENDSR
050600050923      *
050700050923      *---------------------------------------------------------------*
050800050923      * Ricerche                                                      *
050900050923      *---------------------------------------------------------------*
051000050923     c     srSEARCH      BEGSR
051100050923      *
051200050923sel 1c                   SELECT
051300050923      *
051400050923      * Codice Commerciale (Agente)
051500050923w   1c                   WHEN      Wcampo      = 'V1CAGE    '
051600050927     c                   if        DUTlpo      = 'S'
051700050927     c                   clear                   §Fil
051800050927     c                   else
051900050927     c                   movel     DUTpou        §Fil
052000050927     c                   endif
052100050923     c                   call      'TRTB85R'
052200050923     c                   parm                    KPJBA
052300050927     c                   parm                    §FIL              3
052400050927     c                   parm      *blanks       §KEY              8
052500050927     c                   parm      *blanks       §DES1            25
052600050927     c                   movel     §KEY          V1Cage
052700050923     c                   movel     §DES1         V1Dage
052800050923      *
052900050923e   1c                   ENDSL
053000050923      *
053100050923     c                   eval      *in90       = *on
053200050923      *
053300050923     c                   ENDSR
053400050923      *
053500050923      *---------------------------------------------------------------*
053600050923      * Gestione videata C01/S01                                      *
053700050923      *---------------------------------------------------------------*
053800050923     c     GesS01        BEGSR
053900050923      *
054000050923      * Inizializzo la videata
054100050923if  1c                   if        $InzS01     = *on
054200050923     c                   exsr      InzS01
054300050923     c                   eval      $InzS01     = *off
054400050923e   1c                   endif
054500050923      *
054600050923      * Posiziono il cursore
054700050923if  1c                   if        C01csr      > *zeros
054800050923     c                   z-add     C01csr        C01rcd
054900050923e   1c                   endif
055000050923      *
055100050923      * Scrivo la testata e la riga tasti funzionali abilitati
055200050923if  1c                   if        NOT *in90
055300050923     c                   write     TA11T01
055400050923     c                   write     TA11Z02
055500050923e   1c                   endif
055600050923      *
055700050923      * Visualizzo (eventualmente) il msg per la mancanza di dati
055800050923if  1c                   if        C01rcd     <= *zeros
055900050923     c                   write     TA11D02
056000050923e   1c                   endif
056100050923      *
056200050923      * Emetto la videata
056300051005     c                   write     TA11C01
056400051005     c                   exfmt     PROTECT
056500050923     c                   z-add     £SFLnrr       C01rcd
056600050923     c                   setoff                                       28  90
056700050923     c                   clear                   V1Dmsg
056800050923      *
056900050923sel 1c                   SELECT
057000050923      * F3=Fine
057100050923w   1c                   WHEN      *inKC
057200050923     c                   exsr      F03D01
057300050923      * F12=Ritorno
057400050923w   1c                   WHEN      *inKL
057500050923     c                   exsr      F12S01
057600050923      * Roll-UP
057700050923w   1c                   WHEN      £Tasto      = RollUp
057800050923     c                   exsr      RollUpS01
057900050923e   1c                   ENDSL
058000050923      *
058100050923     c                   ENDSR
058200050923      *
058300050923      *---------------------------------------------------------------*
058400050923      * Inizializzazione videata S01                                  *
058500050923      *---------------------------------------------------------------*
058600050923     c     InzS01        BEGSR
058700050927      *
058800050927     c                   move      V1Cage        V2Cage
058900050923      *
059000050923      * Pulizia subfile
059100050923     c                   seton                                        3031
059200050923     c                   write     TA11C01
059300050923     c                   setoff                                         3133
059400050923      *
059500050927     c                   reset                   $EoF
059600050927     c                   reset                   $EoF2
059700050923     c                   clear                   W_SflPag
059800050923     c                   clear                   C01rcd
059900050923     c                   clear                   C01csr
060000050923     c                   clear                   S01nrr
060100050923     c                   clear                   V1Dmsg
060200050930     c                   clear                   SAVksc
060300050930     c                   clear                   SAVctr
060400050930     c                   clear                   SAVprg
060500050930     c                   reset                   $ToWrite
060600050923     c                   setoff                                       28  90
060700050923     c                   movea     *zeros        *in(50)
060800050923      *
060900050923      * Caricamento dei dati da presentare nel sfl
061000050923      *   (a pagine)
061100051104     c                   move      V1Cage        CLPage
061200051104     c     K03CLP01      setll     CNCLP000
061300050927     c                   exsr      ReadCNCLP
061400050930      *
061500050930     c                   exsr      RollUpS01
061600050923      *
061700050923     c                   ENDSR
061800050923      *
061900050923      *---------------------------------------------------------------*
062000051104      * Lettura archivio CNCLP01L                                     *
062100050923      *---------------------------------------------------------------*
062200050927     c     ReadCNCLP     BEGSR
062300050927      *
062400050927     c                   eval      $EoF        = *off
062500050927     c                   eval      $EoF2       = *off
062600050923      *
062700050927do  1c                   DO        *hival
062800050927      *
062900051104     c     K03CLP01      reade     CNCLP000
063000051104     c                   eval      $EoF        = (%eof(CNCLP01L))
063100050927if  2c                   if        $EoF        = *on
063200050923     c                   leavesr
063300050927e   2c                   endif
063400050927if  2c                   if        CLPflg     <> *blanks
063500050927     c                   iter
063600050927e   2c                   endif
063700050923      *
063800050923     c                   reset                   $EoF2
063900050923     c     CLPksc        setll     TITAV000
064000050927     c     CLPksc        reade     TITAV000
064100050923     c                   eval      $EoF2       = (%eof(TITAV01L))
064200050927if  2c                   if        $EoF2       = *off
064300050927     c                   leave
064400050927e   2c                   endif
064500050927      *
064600050927e   1c                   ENDDO
064700050923      *
064800050923     c                   ENDSR
064900050923      *
065000050923      *---------------------------------------------------------------*
065100050923      * Caricamento pagina successiva di dati                         *
065200050923      *---------------------------------------------------------------*
065300050923     c     RollUpS01     BEGSR
065400050923      *
065500050923     c                   eval      S01nrr      = (W_SflPag * C_SflPag)
065600050923     c                   add       1             W_SflPag
065700050923     c                   eval      *in32       = *off
065800051003     c                   eval      $ToWrite    = *off
065900050923      *
066000050923      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
066100050923      * (una pagina per volta)
066200050927do  1c                   DOW            $EoF   = *off
066300050923     c                             and  S01nrr < (W_SflPag * C_SflPag)
066400050923      *
066500050927do  2c                   DoW            $EoF2  = *off
066600050923     c                             and  S01nrr < (W_SflPag * C_SflPag)
066700050923      *
066800050927if  2c                   if        TAVatb      = *blanks
066900050923     c                   exsr      CarS01
067000050927      * - se ho già caricato il num max di rec nel sfl (avendo appena
067100050927      *   scitto la "testata" per cliente, senza alcuna variazione),
067200050927      *   esco dal ciclo di lettura; la prima variazione sarà nel primo
067300050927      *   rec verrà scritto nella nuova pagina...
067400050927      *   Insomma: $ToWrite = *on   se   S01nrr = (W_SflPag * C_SflPag)
067500050927if  3c                   if        $ToWrite    = *on
067600050927     c                   leave
067700050927e   3c                   endif
067800050927e   2c                   endif
067900050923      *
068000050927     c     CLPksc        reade     TITAV000
068100050927     c                   eval      $EoF2       = (%eof(TITAV01L))
068200050923      *
068300050927e   2c                   EndDo
068400050923      *
068500051003      * Se rimane già un riga di dettaglio da scrivere nel sfl (perché
068600051003      *   è stato raggiunto il numero max di righe del sfl)
068700051003      * => NON leggo il record successivo (perchè il 1° record della
068800051003      *    prossima pagina è già in "canna".
068900051003      * Se non esistono altri records per il cliente in esame
069000051003      * => reperisco il cliente successivo (da CNCLP) e cerco records
069100051003      *    per egli (in TITAV).
069200051003if  2c                   if           $ToWrite = *off
069300051003     c                             and $EoF2   = *on
069400051003     c                   exsr      ReadCNCLP
069500051003e   2c                   endif
069600050927if  2c                   if        S01nrr >= (W_SflPag * C_SflPag)
069700050927     c                             or $ToWrite = *on
069800050923     c                   leave
069900050923e   2c                   endif
070000050923      *
070100050927e   1c                   ENDDO
070200050923      *
070300051003      * Visualizzazione del SFL (se ci sono dati)
070400050923     c                   eval      *in30       = (S01nrr = *zeros)
070500050923      *
070600050923      * Attivazione (eventuale) del SFLEND
070700050927     c                   eval      *in33       = $EoF
070800050923      *
070900050923      * Posizionamento cursore al 1° rcd della pagina
071000050927if  1c                   if        S01nrr      > *zeros
071100050923     c     S01nrr        div       C_SflPag      wPag
071200050923     c                   mvr                     wRig
071300050923     c                   eval      C01rcd      = wPag * C_SflPag
071400050923     c                   if        wRig        > *zeros
071500050923     c                   eval      C01rcd      = C01rcd + 1
071600050923     c                   else
071700050923     c                   eval      C01rcd      = C01rcd - C_SflPag + 1
071800050923     c                   endif
071900050923x   1c                   else
072000050923     c                   clear                   C01rcd
072100050923e   1c                   endif
072200050923     c                   eval      C01csr      = C01rcd
072300050923      *
072400050923     c                   ENDSR
072500050923      *
072600050923      *---------------------------------------------------------------*
072700050923      * Caricamento dati in subfile (singolo record)                  *
072800050923      *---------------------------------------------------------------*
072900050923     c     CarS01        BEGSR
073000050923      *
073100050923      * Selezione record - - - - - - - - - - - - - - - - - - - - - - -*
073200050923sel 1c                   select
073300050923      * - record annullato
073400050923w   1c                   when      CLPflg     <> *blanks
073500050923     c                   leavesr
073600050923w   1c                   when      TAVatb     <> *blanks
073700050923     c                   leavesr
073800050927      * - codice commerciale
073900050927      *   (essendo in chiave in entrambi gli archivi, dò per certo che
074000050927      *   sia quello giusto !!!)
074100050930     c***                when          CLPscf <> V1Cage
074200050930     c***                          or  TAVksc <> CLPksc
074300050927     c***                leavesr
074400050923      * - data modifica non compresa nel range
074500050927w   1c                   when          TAVdav  < W1Cdti
074600050927     c                             or  TAVdav  > W1Cdtf
074700050923     c                   leavesr
074800050927      * - profilo utente da non considerare
074900050927w   1c                   when          TAVpru <> *blanks
075000050927     c                             and(TAVpru  = V1Cpu1
075100050927     c                              or TAVpru  = V1Cpu2
075200050927     c                              or TAVpru  = V1Cpu3
075300050927     c                              or TAVpru  = V1Cpu4
075400050927     c                              or TAVpru  = V1Cpu5)
075500050927     c                   leavesr
075600050923e   1c                   endsl
075700050927      *
075800050927      * Scrittura record "testata" (dati cliente)  - - - - - - - - - -*
075900050930if  1c                   if            SAVksc <> TAVksc
076000050930     c                             or  SAVctr <> TAVctr
076100050930     c                             or  SAVprg <> TAVprg
076200050927      * - Caricamento dati nel record del sfl
076300050927     c                   exsr      RieS01clp
076400050930     c                   eval      SAVksc      = TAVksc
076500050930     c                   eval      SAVctr      = TAVctr
076600050930     c                   eval      SAVprg      = TAVprg
076700050927      * - Scrittura record subfile
076800050923     c                   add       1             S01nrr
076900050923     c                   write     TA11S01
077000050927      * - Se raggiunto il limite di rec nel sfl (SFLPAG):
077100050927      *   memorizzo che c'è un rec in "sospeso" da  scrivere nel sfl
077200050927      *   nella prossima pagina
077300050927if  2c                   if        S01nrr >= (W_SflPag * C_SflPag)
077400050927     c                   eval      $ToWrite    = *on
077500050927     c                   leavesr
077600050927e   2c                   endif
077700050927e   1c                   endif
077800050927      *
077900050927      * Scrittura record "dettaglio" (variazioni tariffe)  - - - - - -*
078000050927     c*** non serve qui: eval      $ToWrite    = *off
078100050927      * - Caricamento dati nel record del sfl
078200050927     c                   exsr      RieS01tav
078300050927      * - Scrittura record subfile
078400050927      * (se NON già superato il limite di n° righe per pagina nel sfl)
078500050927     c                   add       1             S01nrr
078600050927     c                   write     TA11S01
078700050923      *
078800050923     c                   ENDSR
078900050923      *
079000050923      *---------------------------------------------------------------*
079100050927      * Caricamento dati CNCLP nel record del sfl                     *
079200050923      *---------------------------------------------------------------*
079300050927     c     RieS01clp     BEGSR
079400050923      *
079500050927     c                   clear                   TA11S01
079600050927      *
079700051005sel 1***c                   select
079800051005     *** * Rottura codice cliente
079900051005w   1***c                   when      SAVksc <> TAVksc
080000051005     *** * - Recupero dati anagrafici del cliente
080100051005     ***c                   exsr      DecodKSC
080200051005     *** * - Testo
080300051005     ***c                   eval      S1Dtxt = 'Cliente:'
080400051005     ***c                                    + %editw(TAVksc :'0       -')
080500051005     ***c                                    + %subst(ACOrag :1 :30)
080600051005     ***c                                    + '  Cod.Tariffa:'
080700051005     ***c                                    + %editw(TAVctr :'0   ')
080800051005     ***c                                    + '  Prg:'
080900051005     ***c                                    + %editw(TAVprg :'0   ')
081000051005     *** * - Indicatori
081100051005     ***c                   eval      *in41  = *on
081200051005     ***c*****              eval      *in42  = *on
081300051005     *** * Rottura codice tariffa
081400051005w   1***c                   when      SAVctr <> TAVctr
081500051005     *** * - Testo
081600051005     ***c                   eval      S1Dtxt = '        '
081700051005     ***c                                    + '         '
081800051005     ***c                                    + '                    '
081900051005     ***c                                    +   '          '
082000051005     ***c                                    + '  Cod.Tariffa:'
082100051005     ***c                                    + %editw(TAVctr :'0   ')
082200051005     ***c                                    + '  Prg:'
082300051005     ***c                                    + %editw(TAVprg :'0   ')
082400051005     *** * - Indicatori
082500051005     ***c                   eval      *in41  = *on
082600051005     *** * Rottura progressivo tariffa
082700051005w   1***c                   when      SAVprg <> TAVprg
082800051005     *** * - Testo
082900051005     ***c                   eval      S1Dtxt = '        '
083000051005     ***c                                    + '         '
083100051005     ***c                                    + '                    '
083200051005     ***c                                    +   '          '
083300051005     ***c                                    + '              '
083400051005     ***c                                    + '    '
083500051005     ***c                                    + '  Prg:'
083600051005     ***c                                    + %editw(TAVprg :'0   ')
083700051005     *** * - Indicatori
083800051005     ***c                   eval      *in41  = *on
083900051005e   1***c                   endsl
084000051005      *
084100051005      * Rottura codice cliente
084200051005      *       o codice tariffa
084300051005      *       o progressivo tariffa
084400051005if  1c                   if            SAVksc <> TAVksc
084500051005     c                             or  SAVctr <> TAVctr
084600051005     c                             or  SAVprg <> TAVprg
084700051005      * - Recupero dati anagrafici del cliente
084800051005if  2c                   if            SAVksc <> TAVksc
084900051005     c                   exsr      DecodKSC
085000051005e   2c                   endif
085100051005      * - Testo
085200051005     c                   eval      S1Dtxt = 'Cliente:'
085300051005     c                                    + %editw(TAVksc :'0       -')
085400051005     c                                    + %subst(ACOrag :1 :30)
085500051005     c                                    + '  Cod.Tariffa:'
085600051005     c                                    + %editw(TAVctr :'0   ')
085700051005     c                                    + '  Prg:'
085800051005     c                                    + %editw(TAVprg :'0   ')
085900051005      * - Indicatori
086000051005     c                   eval      *in41  = *on
086100051005e   1c                   endif
086200050923      *
086300050923     c                   ENDSR
086400050927      *
086500050927      *---------------------------------------------------------------*
086600050927      * Caricamento dati TITAV nel record del sfl                     *
086700050927      *---------------------------------------------------------------*
086800050927     c     RieS01tav     BEGSR
086900050927      *
087000050927     c                   clear                   TA11S01
087100050927      *
087200050930      * Tipo tariffa: se non è 'particolare' decodifico il tipo tariffa
087300050930      *  TAVtrd, altrimenti ricerco la decodifica della tariffa partic.
087400050927if  1c                   IF        TAVtrd     <> 'PAR'
087500050927sel 2c                   select
087600050927w   2c                   when      TAVtrd      = 'TES'
087700050927     c                   eval      S1Dtxt      = 'TESTATA  '
087800050927w   2c                   when      TAVtrd      = 'DET'
087900050927     c                   eval      S1Dtxt      = 'DETTAGLIO'
088000050927w   2c                   when      TAVtrd      = 'GIA'
088100050927     c                   eval      S1Dtxt      = 'GIACENZA '
088200050927e   2c                   endsl
088300050927x   1c                   ELSE
088400050930      * recupero la descrizione della tariffa particolare dalla schiera
088500050930      *  caricata
088600050927     c                   z-add     1             xx
088700050927     c     TAVftc        lookup    $TarP_Cod(xx)                          10
088800050927if  2c                   if        *in10
088900050927     c                   eval      S1Dtxt      = $TarP_Cod(xx)
089000050927     c                                         + '-'
089100050927     c                                         + $TarP_Des(xx)
089200050927x   2c                   else
089300050927     c                   eval      S1Dtxt      = TAVftc
089400050927     c                                         + '-'
089500050927     c                                         + '? ? ? ? ? ? ? ? ? ?'
089600050927e   2c                   endif
089700050927e   1c                   ENDIF
089800050927      * Tipo azione
089900050927sel 1c                   select
090000050927w   1c                   when      TAVcta      = 'A'
090100050927     c                   eval      %subst(S1Dtxt :25 :4)
090200050927     c                                         = 'Ann.'
090300050927w   1c                   when      TAVcta      = 'I'
090400050927     c                   eval      %subst(S1Dtxt :25 :4)
090500050927     c                                         = 'Ins.'
090600050927w   1c                   when      TAVcta      = 'M'
090700050927     c                   eval      %subst(S1Dtxt :25 :4)
090800050927     c                                         = 'Man.'
090900050930w   1c                   when      TAVcta      = 'D'
091000050927     c                   eval      %subst(S1Dtxt :25 :4)
091100050927     c                                         = 'Dup.'
091200050930w   1c                   when      TAVcta      = 'O'
091300050927     c                   eval      %subst(S1Dtxt :25 :4)
091400050927     c                                         = 'Off.'
091500050927e   1c                   endsl
091600050927      *
091700050930      * Data variazione
091800050927     c                   reset                   WLBdat
091900050927     c                   eval      G08inv      = TAVdav
092000050927     c                   call      'XSRDA8'
092100050927     c                   parm                    WLBDAT
092200050927      *
092300050927     c                   eval      S1Dtxt      = %trimr(S1Dtxt)
092400050927     c                                         + '   '
092500050927     c                                         + %editc(G08dat :'Y')
092600050927     c                                         + ' '
092700050927     c                                         + %editw(TAVorv
092800050927     c                                                 :'0 :  :  ')
092900050927     c                                         + '  '
093000050927     c                                         + TAVpru
093100050927     c                                         + '  '
093200050927     c                                         + TAVnoj
093300050927      *
093400050927      * - Indicatori
093500050927     c***                eval      *in41       = *off
093600050927      *
093700050927     c                   ENDSR
093800050923      *
093900050923      *---------------------------------------------------------------*
094000050923      * Gestione tasto funzionale F12 da videata S01                  *
094100050923      * F12-Ritorno                                                   *
094200050923      *---------------------------------------------------------------*
094300050923     c     F12S01        BEGSR
094400050923      *
094500050923      * Torno alla videata precedente
094600050923     c                   eval      $Video      = '1'
094700050923      *
094800050923     c                   ENDSR
094900050930      *
095000050930      *---------------------------------------------------------------*
095100050930      * Recupero dati anagrafici del cliente                          *
095200050930      *---------------------------------------------------------------*
095300050930     c     DecodKSC      BEGSR
095400050930      *
095500050930     c                   clear                   ds_CNACO
095600050930     c                   clear                   ds_CNIND
095700050930     c*** già in input:  clear                   ds_CNCLP
095800050930     c                   clear                   ds_FNCLS
095900050930     c                   reset                   TIBS69ds
096000050930      *
096100050930     c                   eval      I69kac   = TAVksc
096200050930      *
096300050930     c                   call      'TIBS69R'
096400050930     c                   parm                    TIBS69ds
096500050930     c                   parm                    ds_CNACO
096600050930     c                   parm                    ds_CNIND
096700050930     c                   parm                    ds_CNCLP
096800050930     c                   parm                    ds_FNCLS
096900050930      *
097000050930     c                   ENDSR
097100050923      *
097200050923**   $Msg -------------------------------------------------------------------*
097300050923Immettere il codice commerciale                                                1
097400050923Codice commerciale errato                                                      2
097500050923Immettere una data variazione                                                  3
097600050923Data variazione errata                                                         4
097700050923Range di date errato                                                           5
097800051003Profilo utente errato                                                          6
097900090630Utente non abilitato alla gestione del commerciale indicato                    7
098000090701UTENTE NON ABILITATO ALLA GESTIONE TARIFFE                                     8
