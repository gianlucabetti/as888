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
001300130805     fAZCMM01L  if   e           k disk
001400050923     fTITAV01L  if   e           k disk
001500050923      *
001600050923     fTNTA11D   cf   e             workstn
001700050923     f                                     sfile(TA11S01:S01nrr)
001800050923     f                                     infds(DSFMT)
001900050923
002000050923      *---------------------------------------------------------------*
002100050923
002200050923      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002300050923      *
002400050923     d DigitN          c                   const('0123456789')
002500050923      * - Tasti di funzione
002600050923     d Enter           c                   const(x'F1')
002700050923     d RollDwn         c                   const(x'F4')
002800050923     d RollUp          c                   const(x'F5')
002900050923      * - Nr. di righe del sfl (SFLPAG)
003000050930     d C_SflPag        c                   const(14)
003100050923      *
003200050923      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003300050923      *
003400050923      * - Messaggi di errore
003500090701     d $Msg            s             78    dim( 8) ctdata  perrcd(1)
003600050927      *
003700050927     d $TarP_Cod       s              2    dim(50)  inz
003800050927     d $TarP_Des       s             20    dim(50)  inz
003900050927     d $TarP_Dfe       s              1    dim(50)  inz
004000050923      *
004100050923      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004200050923      *
004300050923     d KPJBA         e ds
004400050923      * - Parametri x Controllo profilo utenti
004500050923     d TIBS34ds      e ds                  inz
004600050923      * - Ds di riferimento al file esterno AZUTE00F
004700050923     d AZUTEds       e ds                  inz extname(AZUTE00F)
004800050923      * - Ds per dati organigramma
004900050923     d DDatiUte      e ds                  inz
005000130805      *
005100130805      * - Conrollo Abilitazioni Utente
005200090630     d TNTAA1DS      e ds                  inz
005300130805      *
005400050927      * - Tab. "1P"
005500050927     d ds1P          e ds                  inz
005600050927      * - Tab. "SP"
005700050927     d dsSP          e ds                  inz
005800050927      * Decodifica clienti
005900050927     d TIBS69ds      e ds                  inz
006000050927     d   I69tla      e                     inz('L')
006100050927     d ds_CNACO      e ds                  extname(CNACO00F) inz
006200050927     d ds_CNIND      e ds                  extname(CNIND00F) inz
006300050927     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
006400050927     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
006500130805      * Decodifica commerciali
006600130805     d TRMK43ds      e ds                  inz
006700130805     d   oMK43err    e                     inz(*off)
006800050923      *
006900050923     d Status         sds           333
007000050923     d   SDSpgm          *proc
007100050923      *
007200050923     d DSFMT           ds           512
007300050923     d  £Tasto               369    369
007400050923     d  £SFLnrr              378    379B 0
007500050923      *
007600050923      * - Controllo/Inversione date
007700050923     d WLBdat          ds                  inz
007800050923     d   G08dat                       8  0 inz
007900050923     d   G08inv                       8  0 inz
008000050923     d   G08err                       1    inz('3')
008100050923     d   G08tgi                       5  0 inz
008200051003      *
008300051003      * - Ridefinizione profili utente in schiera
008400051003     d ds_Utenti       ds            50    inz
008500051003     d   V1Cpu1                            inz
008600051003     d   V1Cpu2                            inz
008700051003     d   V1Cpu3                            inz
008800051003     d   V1Cpu4                            inz
008900051003     d   V1Cpu5                            inz
009000051003     d   $Usr                  1     50    inz dim( 5)
009100050923      *
009200050923      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
009300050923      *
009400050923      * - Flags
009500050927     d $EoF            s              1    inz(*off)
009600050927     d $EoF2           s              1    inz(*off)
009700050923     d $Fine           s              1    inz(*off)
009800050923     d $InzD01         s              1    inz(*on)
009900050923     d $InzS01         s              1    inz(*on)
010000050927     d $ToWrite        s              1    inz(*off)
010100050923      *
010200050923     d $Video          s              1    inz('1')
010300050923      *
010400050923      * - Indici di schiera
010500050927     d XX              s              3  0 inz
010600050923      *
010700050923      * - Variabili riferite al data base o al display file
010800050923     d S01nrr          s                   like(C01rcd) inz
010900050923     d W1Cage          s                   like(CLPksc) inz
011000050923     d W1Cdti          s                   like(TAVdav) inz
011100050923     d W1Cdtf          s                   like(TAVdav) inz
011200050930     d SAVksc          s                   like(TAVksc) inz
011300050930     d SAVctr          s                   like(TAVctr) inz
011400050930     d SAVprg          s                   like(TAVprg) inz
011500050923      *
011600050923      * - Variabili definite a programma
011700050923     d wPag            s              4  0 inz
011800050923     d wRig            s              3  0 inz
011900050927     d W_SflPag        s              3  0 inz
012000050923     d Wcampo          s             10    inz
012100050923      *
012200050923      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
012300050923      *
012400050927      * - TABEL00F
012500050927     c     K02TABEL      klist
012600050927     c                   kfld                    TBLkut
012700050927     c                   kfld                    TBLcod
012800130805      * - AZCMM01L
012900130805     c     K_azcmm01     klist
013000130805     c                   kfld                    k_CMMcod
013100051104      * - CNCLP01L
013200051104     c     K03CLP01      klist
013300050930     c                   kfld                    TBLkut
013400050930     c                   kfld                    DUTkci
013500051104     c                   kfld                    CLPage
013600050923     c*** NO:            kfld                    CLPksc
013700130805      *
013800130805     c     *like         define    CMMcod        k_CMMcod
013900050923
014000050923      *---------------------------------------------------------------*
014100050923      *  RIEPILOGO INDICATORI                                         *
014200050923      *---------------------------------------------------------------*
014300050923      * 10    - Comodo                                                *
014400050923      * 28    - Emette il messaggio di errore a video                 *
014500050923      * 30    - Ripulisce il subfile S01                              *
014600050923      * 31    - NON emette il subfile S01                             *
014700050923      * 32    - Record modificato nel subfile S01 (sflnxtchg)         *
014800050923      * 33    - Fine dati nel subfile S01         (sflend)            *
014900050923      * 41    - S01: Condiziona DSPATR(RI)                            *
015000050930      * 42    -  "   Condiziona DSPATR(HI)                            *
015100050923      * 51    - D01: Codice commerciale errato                        *
015200050923      * 52    -  "   Data inizio periodo errata                       *
015300050923      * 53    -  "   Data fine   periodo errata                       *
015400050923      * 54    -  "   1° profilo utente errato                         *
015500050923      * 55    -  "   2° profilo utente errato                         *
015600050923      * 56    -  "   3° profilo utente errato                         *
015700050923      * 57    -  "   4° profilo utente errato                         *
015800050923      * 58    -  "   5° profilo utente errato                         *
015900050923      * 90    - Generico di errore                                    *
016000050923      *---------------------------------------------------------------*
016100050923
016200050923     c     *Entry        plist
016300050923     c                   parm                    KPJBA
016400050923      *
016500050923      * Operazioni Iniziali
016600050923     c                   exsr      RoutInz
016700050923      *
016800050923      * Gestione Video
016900050923do  1c                   DOW       $Fine    = *off
017000050923      * - Filtro di lancio
017100050923cas 2c     $Video        caseq     '1'           GesD01
017200050923      * - SubFile
017300050923cas 2c     $Video        caseq     '2'           GesS01
017400050923e   2c                   endcs
017500050923e   1c                   ENDDO
017600050923      *
017700050923      * Fine
017800090630     C                   clear                   tntaa1ds
017900090630     C                   movel     'C'           Itaa1Tla
018000090630     C                   movel(p)  tntaa1ds      kpjbu
018100090703     C                   CALL      'TNTAA1C'
018200090630     C                   PARM                    KPJBA
018300090630     c
018400050923     c                   movel     *on           *inLR
018500050923      *
018600050923      *---------------------------------------------------------------*
018700050923      * Operazioni Iniziali                                           *
018800050923      *---------------------------------------------------------------*
018900050923     c     RoutInz       BEGSR
019000050923      *
019100050923      * Reperisce dati job
019200050923     c                   exsr      DatiJob
019300090701     C* CONTROLLO se l'utente è abilitato alla gestione tariffe
019400090701     c                   clear                   tntaa1ds
019500090701     c                   eval      itaa1caut='§utegtc'
019600090701     c                   clear                   kpjbu
019700090701     c                   movel     tntaa1ds      kpjbu
019800090703     c                   call      'TNTAA1C'
019900090701     c                   parm                    kpjba
020000090701     c                   movel     kpjbu         tntaa1ds
020100090701     c
020200090701     c* Utente non abilitato alla gestione tariffe
020300090701     c                   if        otaa1fabi='N'
020400090701     c                   Seton                                        08
020500090701e   2c                   endif
020600050927      *
020700050927      * Impostazione dati di testata a video
020800050927     c                   movel     SDSpgm        V1Tpgm
020900050927      *
021000050927      * Caricamento codici e descrizione delle tariffe particolari
021100050927      *   da tabella 1P
021200050927     c                   eval      TBLkut   = 1
021300050927     c                   eval      TBLcod   = '1P'
021400050927     c                   clear                   xx
021500050927     c     K02TABEL      setll     TABEL
021600050927     c     K02TABEL      reade     TABEL
021700050927do  1c                   DOW       NOT %eof(TABEL00F)
021800050927      *
021900050927if  2c                   IF        TBLflg   = *blanks
022000050927      *
022100050927      * - Controllo validità tariffa particolare x la gestione tariffe
022200050927     c                   movel     TBLuni        ds1P
022300050927      * - Flag di comodo per tabella 'SP'
022400050927if  3c                   if        §1Puta   = 'S'
022500050927     c                   add       1             xx
022600050927     c                   movel     TBLkey        $TarP_Cod(xx)
022700050927     c                   movel     §1Pdes        $TarP_Des(xx)
022800050927     c                   movel     §1Pdfe        $TarP_Dfe(xx)
022900050927e   3c                   endif
023000050927      *
023100050927e   2c                   ENDIF
023200050927      *
023300050927     c     K02TABEL      reade     TABEL
023400050927e   1c                   ENDDO
023500050927      *
023600050927      * - Per tariffe FedEx controllo se devo recupare la descrizione
023700050930      *   dalla tabella "SP"
023800050927     c                   clear                   xx
023900050927do  1c                   DOW       xx          < %elem($TarP_Cod)
024000050927     c                   add       1             xx
024100050927if  2c                   if        $TarP_Cod(xx)  = *blanks
024200050927     c                   leave
024300050927e   2c                   endif
024400050927if  2c                   if        $TarP_Dfe(xx) <> 'S'
024500050927     c                   iter
024600050927e   2c                   endif
024700050927     c                   clear                   dsSP
024800050927     c                   movel     'SP'          TBLcod
024900050927     c                   movel(p)  $TarP_Cod(xx) TBLkey
025000050927     c     K02TABEL      chain     TABEL
025100050927if  2c                   If            %found(TABEL00F)
025200050927     c                             and TBLflg  = *blanks
025300050927     c                   movel     TBLuni        dsSP
025400050927if  3c                   if        §SPdfe     <> *blanks
025500050927     c                   movel     §SPdfe        $TarP_Des(xx)
025600050927e   3c                   endif
025700050927e   2c                   EndIf
025800050927e   1c                   ENDDO
025900050923      *
026000050923     c                   ENDSR
026100050923      *
026200050923      *---------------------------------------------------------------*
026300050923      * Reperimento Dati del job (Utente/Operativi)                   *
026400050923      *---------------------------------------------------------------*
026500050923     c     DatiJob       BEGSR
026600050923      *
026700050923     c     *dtaara       define    §azute        azuteds
026800050923     c     *dtaara       define    §datiute      ddatiute
026900050923      *
027000050923     c                   in(E)     *dtaara
027100050923     c                   IF        %ERROR or RSUT = *blanks
027200050923     c                   clear                   Tibs34Ds
027300050923     c                   call      'TIBS34R'
027400050923     c                   parm                    Tibs34Ds
027500050923     c                   in        *dtaara
027600050923     c                   ENDIF
027700050923      *
027800050923     c                   ENDSR
027900050923      *
028000050923      *---------------------------------------------------------------*
028100050923      * Gestione videata D01                                          *
028200050923      *---------------------------------------------------------------*
028300050923     c     GesD01        BEGSR
028400050923      *
028500050923      * Inizializzo la videata
028600050923if  1c                   if        $InzD01     = *on
028700050923     c                   exsr      InzD01
028800050923     c                   movel     *off          $InzD01
028900050923e   1c                   endif
029000050923      *
029100050923      * Reimposto il codice comm.le a *blank se *zero (numerico editato)
029200050923if  2c                   if        V1Cage      = *zeros
029300050923     c                   eval      V1Cage      = *blanks
029400050923e   2c                   endif
029500050923      *
029600050923      * Scrivo la testata e la riga tasti funzionali abilitati
029700050923if  1c                   if        NOT *in90
029800050923     c                   write     TA11T01
029900050923     c                   write     TA11Z01
030000050923e   1c                   endif
030100050923      * Emetto la videata
030200050923     c                   exfmt     TA11D01
030300050923     c                   setoff                                       28  90
030400050923     c                   clear                   V1Dmsg
030500050923      *
030600050923sel 1c                   select
030700050923      * F3=Fine
030800090701w   1c                   when      *inKC or *in08
030900050923     c                   exsr      F03D01
031000050923     c                   leavesr
031100050923      *
031200050923e   1c                   endsl
031300050923      *
031400050923      * Controllo dati immessi a video
031500050923     c                   exsr      CtrD01
031600050923     c                   if        *in90
031700050923     c                   leavesr
031800050923     c                   endif
031900050923      *
032000050923      * Passaggio al subfile
032100050923     c                   eval      $InzS01     = *on
032200050923     c                   eval      $Video      = '2'
032300050923      *
032400050923     c                   ENDSR
032500050923      *
032600050923      *---------------------------------------------------------------*
032700050923      * Inizializzazione videata D01                                  *
032800050923      *---------------------------------------------------------------*
032900050923     c     InzD01        BEGSR
033000050923      *
033100050923     c                   clear                   TA11D01
033200090701     c* Errore utente non abilitato
033300090701     c                   if        *in08
033400090701     c                   Seton                                        5128
033500090701     c                   movel     $MSG(8)       v1dmsg
033600090701     c                   endif
033700050923      *
033800050923      * Imposto i dati di default
033900050923     c***                move      '?'           V1Cage
034000050923      *
034100050923     c                   ENDSR
034200050923      *
034300050923      *---------------------------------------------------------------*
034400050923      * Gestione tasto funzionale F03 da videata D01                  *
034500050923      *---------------------------------------------------------------*
034600050923     c     F03D01        BEGSR
034700050923      *
034800050923      * Chiudo il programma
034900050923     c                   eval      $Fine       = *on
035000050923      *
035100050923     c                   ENDSR
035200050923      *
035300050923      *---------------------------------------------------------------*
035400050923      * Controllo dati immessi in videata D01                         *
035500050923      *---------------------------------------------------------------*
035600050923     c     CtrD01        BEGSR
035700050923      *
035800050923     c                   movea     *zeros        *in(50)
035900050923     c                   clear                   W1Cage
036000050927     c                   clear                   W1Cdti
036100050927     c                   clear                   W1Cdtf
036200050923      *
036300050923      * Codice Commerciale
036400051003     c                   clear                   V1Dage
036500050923      * - se *blanks lo considero *zeros (sarebbe numerico)
036600050923if  1c                   if        V1Cage      = *blanks
036700050923     c                   eval      V1Cage      = *zeros
036800050923e   1c                   endif
036900050923      * - ricerca
037000050923     c     '?'           scan      V1Cage                                 10
037100050923if  1c                   if        *in10       = *on
037200050923     c                   eval      V1Cage      = *zeros
037300050923     c                   eval      Wcampo      = 'V1CAGE    '
037400050923     c                   exsr      srSEARCH
037500050923e   1c                   endif
037600050923      * - controlli:
037700050923      * - - effettiva immissione
037800050923if  1c                   IF        V1Cage      = *zeros
037900050923     c                   seton                                        512890
038000050923     c                   eval      V1Dmsg      = $Msg(1)
038100050923     c                   leavesr
038200050923x   1c                   ELSE
038300050923      * - - numericità
038400050923     c     DigitN        check     V1Cage                                 10
038500050923if  2c                   if        *in10       = *on
038600050923     c                   seton                                        512890
038700050923     c                   eval      V1Dmsg      = $Msg(2)
038800050923     c                   leavesr
038900050923e   2c                   endif
039000050923      * - - validità
039100050930      * - - - esistenza in tab. "01"
039200130805     c                   clear                   k_CMMcod
039300130805     c                   move      V1Cage        k_CMMcod
039400130805     c     K_azcmm01     chain     AZCMM000
039500130805if  2c                   if        NOT %found(AZCMM01L)
039600050923     c                   seton                                        512890
039700050923     c                   eval      V1Dmsg      = $Msg(2)
039800050923     c                   leavesr
039900050923e   2c                   endif
040000130805     c                   movel     CMMdes        V1Dage
040100050923      * - - - appartenenza a P.O. gestito dall'utente
040200090630     c                   clear                   tntaa1ds
040300090630     c                   eval      itaa1caut='§utegtc'
040400090630     c                   movel     v1cage        itaa1cmm
040500090630     c                   clear                   kpjbu
040600090630     c                   movel     tntaa1ds      kpjbu
040700090703     c                   call      'TNTAA1C'
040800090630     c                   parm                    kpjba
040900090630     c                   movel     kpjbu         tntaa1ds
041000090630     c
041100130805if  2c                   if        otaa1fabi='N'
041200050923     c                   seton                                        512890
041300090630     c                   eval      V1Dmsg = $Msg(7)
041400050923     c                   leavesr
041500130805e   2c                   endif
041600130805e   1c                   ENDIF
041700050923      *
041800050923      * Periodo
041900050923      * - 1ª data
042000050923     c                   clear                   W1Cdti
042100050923if  1c                   if        V1Cdti      = *zeros
042200050923     c                   seton                                        522890
042300050923     c                   eval      V1Dmsg      = $Msg(3)
042400050923     c                   leavesr
042500050923x   1c                   else
042600050923     c                   clear                   WLBdat
042700050923     c                   eval      G08dat      = V1Cdti
042800050923     c                   call      'XSRDA8'
042900050923     c                   parm                    WLBdat
043000050923if  2c                   if        G08err      = *on
043100050923     c                   seton                                        522890
043200050923     c                   eval      V1Dmsg      = $Msg(4)
043300050923     c                   leavesr
043400050923e   2c                   endif
043500050923     c                   eval      V1Cdti      = G08dat
043600050923     c                   eval      W1Cdti      = G08inv
043700050923e   1c                   endif
043800050923      * - 2ª data
043900050923     c                   clear                   W1Cdtf
044000050923if  1c                   if        V1Cdtf      = *zeros
044100050923     c                   seton                                        532890
044200050923     c                   eval      V1Dmsg      = $Msg(3)
044300050923     c                   leavesr
044400050923x   1c                   else
044500050923     c                   clear                   WLBdat
044600050923     c                   eval      G08dat      = V1Cdtf
044700050923     c                   call      'XSRDA8'
044800050923     c                   parm                    WLBdat
044900050923if  2c                   if        G08err      = *on
045000050923     c                   seton                                        532890
045100050923     c                   eval      V1Dmsg      = $Msg(4)
045200050923     c                   leavesr
045300050923e   2c                   endif
045400050923     c                   eval      V1Cdtf      = G08dat
045500050923     c                   eval      W1Cdtf      = G08inv
045600050923e   1c                   endif
045700050923      * - range
045800050927if  1c                   if        W1Cdtf     <= *zeros
045900050927     c                   eval      W1Cdtf      = W1Cdti
046000050923e   1c                   endif
046100050927if  1c                   if        W1Cdti      > W1Cdtf
046200050923     c                   seton                                        522890
046300050923     c                   eval      V1Dmsg      = $Msg(5)
046400050923     c                   leavesr
046500050923e   1c                   endif
046600050923      *
046700050923      * Profili utente
046800051003     c                   clear                   xx
046900051003do  1c                   dow       xx          < %elem($Usr)
047000051003     c                   add       1             xx
047100051003if  2c                   if        $Usr(xx)   <> *blanks
047200051003     c     $Usr(xx)      chain     AZUTE000
047300051003if  3c                   if        NOT %found(AZUTE01L)
047400051003     c                   eval      *in(53+xx)  = *on
047500051003     c                   seton                                        28  90
047600051003     c                   eval      V1Dmsg      = $Msg(6)
047700051003     c                   leavesr
047800051003e   3c                   endif
047900051003e   2c                   endif
048000051003e   1c                   enddo
048100050923      *
048200050923     c                   ENDSR
048300050923      *
048400050923      *---------------------------------------------------------------*
048500050923      * Ricerche                                                      *
048600050923      *---------------------------------------------------------------*
048700050923     c     srSEARCH      BEGSR
048800050923      *
048900050923sel 1c                   SELECT
049000050923      *
049100050923      * Codice Commerciale (Agente)
049200050923w   1c                   WHEN      Wcampo      = 'V1CAGE    '
049300130805     c                   reset                   TRMK43ds
049400130805     c                   if        DUTlpo     <> 'S'
049500130805     c                   movel     DUTpou        iMK43fil
049600050927     c                   endif
049700130805     c                   call      'TRMK43R'
049800050923     c                   parm                    KPJBA
049900130805     c                   parm                    TRMK43ds
050000130805     c                   if        oMK43err = *off  and
050100130805     c                             oMK43fxx = *blank
050200130805     c                   movel     oMK43cmm      V1Cage
050300130805     c                   movel     oMK43des      V1Dage
050400130805     c                   endif
050500050923      *
050600050923e   1c                   ENDSL
050700050923      *
050800050923     c                   eval      *in90       = *on
050900050923      *
051000050923     c                   ENDSR
051100050923      *
051200050923      *---------------------------------------------------------------*
051300050923      * Gestione videata C01/S01                                      *
051400050923      *---------------------------------------------------------------*
051500050923     c     GesS01        BEGSR
051600050923      *
051700050923      * Inizializzo la videata
051800050923if  1c                   if        $InzS01     = *on
051900050923     c                   exsr      InzS01
052000050923     c                   eval      $InzS01     = *off
052100050923e   1c                   endif
052200050923      *
052300050923      * Posiziono il cursore
052400050923if  1c                   if        C01csr      > *zeros
052500050923     c                   z-add     C01csr        C01rcd
052600050923e   1c                   endif
052700050923      *
052800050923      * Scrivo la testata e la riga tasti funzionali abilitati
052900050923if  1c                   if        NOT *in90
053000050923     c                   write     TA11T01
053100050923     c                   write     TA11Z02
053200050923e   1c                   endif
053300050923      *
053400050923      * Visualizzo (eventualmente) il msg per la mancanza di dati
053500050923if  1c                   if        C01rcd     <= *zeros
053600050923     c                   write     TA11D02
053700050923e   1c                   endif
053800050923      *
053900050923      * Emetto la videata
054000051005     c                   write     TA11C01
054100051005     c                   exfmt     PROTECT
054200050923     c                   z-add     £SFLnrr       C01rcd
054300050923     c                   setoff                                       28  90
054400050923     c                   clear                   V1Dmsg
054500050923      *
054600050923sel 1c                   SELECT
054700050923      * F3=Fine
054800050923w   1c                   WHEN      *inKC
054900050923     c                   exsr      F03D01
055000050923      * F12=Ritorno
055100050923w   1c                   WHEN      *inKL
055200050923     c                   exsr      F12S01
055300050923      * Roll-UP
055400050923w   1c                   WHEN      £Tasto      = RollUp
055500050923     c                   exsr      RollUpS01
055600050923e   1c                   ENDSL
055700050923      *
055800050923     c                   ENDSR
055900050923      *
056000050923      *---------------------------------------------------------------*
056100050923      * Inizializzazione videata S01                                  *
056200050923      *---------------------------------------------------------------*
056300050923     c     InzS01        BEGSR
056400050927      *
056500050927     c                   move      V1Cage        V2Cage
056600050923      *
056700050923      * Pulizia subfile
056800050923     c                   seton                                        3031
056900050923     c                   write     TA11C01
057000050923     c                   setoff                                         3133
057100050923      *
057200050927     c                   reset                   $EoF
057300050927     c                   reset                   $EoF2
057400050923     c                   clear                   W_SflPag
057500050923     c                   clear                   C01rcd
057600050923     c                   clear                   C01csr
057700050923     c                   clear                   S01nrr
057800050923     c                   clear                   V1Dmsg
057900050930     c                   clear                   SAVksc
058000050930     c                   clear                   SAVctr
058100050930     c                   clear                   SAVprg
058200050930     c                   reset                   $ToWrite
058300050923     c                   setoff                                       28  90
058400050923     c                   movea     *zeros        *in(50)
058500050923      *
058600050923      * Caricamento dei dati da presentare nel sfl
058700050923      *   (a pagine)
058800051104     c                   move      V1Cage        CLPage
058900051104     c     K03CLP01      setll     CNCLP000
059000050927     c                   exsr      ReadCNCLP
059100050930      *
059200050930     c                   exsr      RollUpS01
059300050923      *
059400050923     c                   ENDSR
059500050923      *
059600050923      *---------------------------------------------------------------*
059700051104      * Lettura archivio CNCLP01L                                     *
059800050923      *---------------------------------------------------------------*
059900050927     c     ReadCNCLP     BEGSR
060000050927      *
060100050927     c                   eval      $EoF        = *off
060200050927     c                   eval      $EoF2       = *off
060300050923      *
060400050927do  1c                   DO        *hival
060500050927      *
060600051104     c     K03CLP01      reade     CNCLP000
060700051104     c                   eval      $EoF        = (%eof(CNCLP01L))
060800050927if  2c                   if        $EoF        = *on
060900050923     c                   leavesr
061000050927e   2c                   endif
061100050927if  2c                   if        CLPflg     <> *blanks
061200050927     c                   iter
061300050927e   2c                   endif
061400050923      *
061500050923     c                   reset                   $EoF2
061600050923     c     CLPksc        setll     TITAV000
061700050927     c     CLPksc        reade     TITAV000
061800050923     c                   eval      $EoF2       = (%eof(TITAV01L))
061900050927if  2c                   if        $EoF2       = *off
062000050927     c                   leave
062100050927e   2c                   endif
062200050927      *
062300050927e   1c                   ENDDO
062400050923      *
062500050923     c                   ENDSR
062600050923      *
062700050923      *---------------------------------------------------------------*
062800050923      * Caricamento pagina successiva di dati                         *
062900050923      *---------------------------------------------------------------*
063000050923     c     RollUpS01     BEGSR
063100050923      *
063200050923     c                   eval      S01nrr      = (W_SflPag * C_SflPag)
063300050923     c                   add       1             W_SflPag
063400050923     c                   eval      *in32       = *off
063500051003     c                   eval      $ToWrite    = *off
063600050923      *
063700050923      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
063800050923      * (una pagina per volta)
063900050927do  1c                   DOW            $EoF   = *off
064000050923     c                             and  S01nrr < (W_SflPag * C_SflPag)
064100050923      *
064200050927do  2c                   DoW            $EoF2  = *off
064300050923     c                             and  S01nrr < (W_SflPag * C_SflPag)
064400050923      *
064500050927if  2c                   if        TAVatb      = *blanks
064600050923     c                   exsr      CarS01
064700050927      * - se ho già caricato il num max di rec nel sfl (avendo appena
064800050927      *   scitto la "testata" per cliente, senza alcuna variazione),
064900050927      *   esco dal ciclo di lettura; la prima variazione sarà nel primo
065000050927      *   rec verrà scritto nella nuova pagina...
065100050927      *   Insomma: $ToWrite = *on   se   S01nrr = (W_SflPag * C_SflPag)
065200050927if  3c                   if        $ToWrite    = *on
065300050927     c                   leave
065400050927e   3c                   endif
065500050927e   2c                   endif
065600050923      *
065700050927     c     CLPksc        reade     TITAV000
065800050927     c                   eval      $EoF2       = (%eof(TITAV01L))
065900050923      *
066000050927e   2c                   EndDo
066100050923      *
066200051003      * Se rimane già un riga di dettaglio da scrivere nel sfl (perché
066300051003      *   è stato raggiunto il numero max di righe del sfl)
066400051003      * => NON leggo il record successivo (perchè il 1° record della
066500051003      *    prossima pagina è già in "canna".
066600051003      * Se non esistono altri records per il cliente in esame
066700051003      * => reperisco il cliente successivo (da CNCLP) e cerco records
066800051003      *    per egli (in TITAV).
066900051003if  2c                   if           $ToWrite = *off
067000051003     c                             and $EoF2   = *on
067100051003     c                   exsr      ReadCNCLP
067200051003e   2c                   endif
067300050927if  2c                   if        S01nrr >= (W_SflPag * C_SflPag)
067400050927     c                             or $ToWrite = *on
067500050923     c                   leave
067600050923e   2c                   endif
067700050923      *
067800050927e   1c                   ENDDO
067900050923      *
068000051003      * Visualizzazione del SFL (se ci sono dati)
068100050923     c                   eval      *in30       = (S01nrr = *zeros)
068200050923      *
068300050923      * Attivazione (eventuale) del SFLEND
068400050927     c                   eval      *in33       = $EoF
068500050923      *
068600050923      * Posizionamento cursore al 1° rcd della pagina
068700050927if  1c                   if        S01nrr      > *zeros
068800050923     c     S01nrr        div       C_SflPag      wPag
068900050923     c                   mvr                     wRig
069000050923     c                   eval      C01rcd      = wPag * C_SflPag
069100050923     c                   if        wRig        > *zeros
069200050923     c                   eval      C01rcd      = C01rcd + 1
069300050923     c                   else
069400050923     c                   eval      C01rcd      = C01rcd - C_SflPag + 1
069500050923     c                   endif
069600050923x   1c                   else
069700050923     c                   clear                   C01rcd
069800050923e   1c                   endif
069900050923     c                   eval      C01csr      = C01rcd
070000050923      *
070100050923     c                   ENDSR
070200050923      *
070300050923      *---------------------------------------------------------------*
070400050923      * Caricamento dati in subfile (singolo record)                  *
070500050923      *---------------------------------------------------------------*
070600050923     c     CarS01        BEGSR
070700050923      *
070800050923      * Selezione record - - - - - - - - - - - - - - - - - - - - - - -*
070900050923sel 1c                   select
071000050923      * - record annullato
071100050923w   1c                   when      CLPflg     <> *blanks
071200050923     c                   leavesr
071300050923w   1c                   when      TAVatb     <> *blanks
071400050923     c                   leavesr
071500050927      * - codice commerciale
071600050927      *   (essendo in chiave in entrambi gli archivi, dò per certo che
071700050927      *   sia quello giusto !!!)
071800050930     c***                when          CLPscf <> V1Cage
071900050930     c***                          or  TAVksc <> CLPksc
072000050927     c***                leavesr
072100050923      * - data modifica non compresa nel range
072200050927w   1c                   when          TAVdav  < W1Cdti
072300050927     c                             or  TAVdav  > W1Cdtf
072400050923     c                   leavesr
072500050927      * - profilo utente da non considerare
072600050927w   1c                   when          TAVpru <> *blanks
072700050927     c                             and(TAVpru  = V1Cpu1
072800050927     c                              or TAVpru  = V1Cpu2
072900050927     c                              or TAVpru  = V1Cpu3
073000050927     c                              or TAVpru  = V1Cpu4
073100050927     c                              or TAVpru  = V1Cpu5)
073200050927     c                   leavesr
073300050923e   1c                   endsl
073400050927      *
073500050927      * Scrittura record "testata" (dati cliente)  - - - - - - - - - -*
073600050930if  1c                   if            SAVksc <> TAVksc
073700050930     c                             or  SAVctr <> TAVctr
073800050930     c                             or  SAVprg <> TAVprg
073900050927      * - Caricamento dati nel record del sfl
074000050927     c                   exsr      RieS01clp
074100050930     c                   eval      SAVksc      = TAVksc
074200050930     c                   eval      SAVctr      = TAVctr
074300050930     c                   eval      SAVprg      = TAVprg
074400050927      * - Scrittura record subfile
074500050923     c                   add       1             S01nrr
074600050923     c                   write     TA11S01
074700050927      * - Se raggiunto il limite di rec nel sfl (SFLPAG):
074800050927      *   memorizzo che c'è un rec in "sospeso" da  scrivere nel sfl
074900050927      *   nella prossima pagina
075000050927if  2c                   if        S01nrr >= (W_SflPag * C_SflPag)
075100050927     c                   eval      $ToWrite    = *on
075200050927     c                   leavesr
075300050927e   2c                   endif
075400050927e   1c                   endif
075500050927      *
075600050927      * Scrittura record "dettaglio" (variazioni tariffe)  - - - - - -*
075700050927     c*** non serve qui: eval      $ToWrite    = *off
075800050927      * - Caricamento dati nel record del sfl
075900050927     c                   exsr      RieS01tav
076000050927      * - Scrittura record subfile
076100050927      * (se NON già superato il limite di n° righe per pagina nel sfl)
076200050927     c                   add       1             S01nrr
076300050927     c                   write     TA11S01
076400050923      *
076500050923     c                   ENDSR
076600050923      *
076700050923      *---------------------------------------------------------------*
076800050927      * Caricamento dati CNCLP nel record del sfl                     *
076900050923      *---------------------------------------------------------------*
077000050927     c     RieS01clp     BEGSR
077100050923      *
077200050927     c                   clear                   TA11S01
077300050927      *
077400051005sel 1***c                   select
077500051005     *** * Rottura codice cliente
077600051005w   1***c                   when      SAVksc <> TAVksc
077700051005     *** * - Recupero dati anagrafici del cliente
077800051005     ***c                   exsr      DecodKSC
077900051005     *** * - Testo
078000051005     ***c                   eval      S1Dtxt = 'Cliente:'
078100051005     ***c                                    + %editw(TAVksc :'0       -')
078200051005     ***c                                    + %subst(ACOrag :1 :30)
078300051005     ***c                                    + '  Cod.Tariffa:'
078400051005     ***c                                    + %editw(TAVctr :'0   ')
078500051005     ***c                                    + '  Prg:'
078600051005     ***c                                    + %editw(TAVprg :'0   ')
078700051005     *** * - Indicatori
078800051005     ***c                   eval      *in41  = *on
078900051005     ***c*****              eval      *in42  = *on
079000051005     *** * Rottura codice tariffa
079100051005w   1***c                   when      SAVctr <> TAVctr
079200051005     *** * - Testo
079300051005     ***c                   eval      S1Dtxt = '        '
079400051005     ***c                                    + '         '
079500051005     ***c                                    + '                    '
079600051005     ***c                                    +   '          '
079700051005     ***c                                    + '  Cod.Tariffa:'
079800051005     ***c                                    + %editw(TAVctr :'0   ')
079900051005     ***c                                    + '  Prg:'
080000051005     ***c                                    + %editw(TAVprg :'0   ')
080100051005     *** * - Indicatori
080200051005     ***c                   eval      *in41  = *on
080300051005     *** * Rottura progressivo tariffa
080400051005w   1***c                   when      SAVprg <> TAVprg
080500051005     *** * - Testo
080600051005     ***c                   eval      S1Dtxt = '        '
080700051005     ***c                                    + '         '
080800051005     ***c                                    + '                    '
080900051005     ***c                                    +   '          '
081000051005     ***c                                    + '              '
081100051005     ***c                                    + '    '
081200051005     ***c                                    + '  Prg:'
081300051005     ***c                                    + %editw(TAVprg :'0   ')
081400051005     *** * - Indicatori
081500051005     ***c                   eval      *in41  = *on
081600051005e   1***c                   endsl
081700051005      *
081800051005      * Rottura codice cliente
081900051005      *       o codice tariffa
082000051005      *       o progressivo tariffa
082100051005if  1c                   if            SAVksc <> TAVksc
082200051005     c                             or  SAVctr <> TAVctr
082300051005     c                             or  SAVprg <> TAVprg
082400051005      * - Recupero dati anagrafici del cliente
082500051005if  2c                   if            SAVksc <> TAVksc
082600051005     c                   exsr      DecodKSC
082700051005e   2c                   endif
082800051005      * - Testo
082900051005     c                   eval      S1Dtxt = 'Cliente:'
083000051005     c                                    + %editw(TAVksc :'0       -')
083100051005     c                                    + %subst(ACOrag :1 :30)
083200051005     c                                    + '  Cod.Tariffa:'
083300051005     c                                    + %editw(TAVctr :'0   ')
083400051005     c                                    + '  Prg:'
083500051005     c                                    + %editw(TAVprg :'0   ')
083600051005      * - Indicatori
083700051005     c                   eval      *in41  = *on
083800051005e   1c                   endif
083900050923      *
084000050923     c                   ENDSR
084100050927      *
084200050927      *---------------------------------------------------------------*
084300050927      * Caricamento dati TITAV nel record del sfl                     *
084400050927      *---------------------------------------------------------------*
084500050927     c     RieS01tav     BEGSR
084600050927      *
084700050927     c                   clear                   TA11S01
084800050927      *
084900050930      * Tipo tariffa: se non è 'particolare' decodifico il tipo tariffa
085000050930      *  TAVtrd, altrimenti ricerco la decodifica della tariffa partic.
085100050927if  1c                   IF        TAVtrd     <> 'PAR'
085200050927sel 2c                   select
085300050927w   2c                   when      TAVtrd      = 'TES'
085400050927     c                   eval      S1Dtxt      = 'TESTATA  '
085500050927w   2c                   when      TAVtrd      = 'DET'
085600050927     c                   eval      S1Dtxt      = 'DETTAGLIO'
085700050927w   2c                   when      TAVtrd      = 'GIA'
085800050927     c                   eval      S1Dtxt      = 'GIACENZA '
085900050927e   2c                   endsl
086000050927x   1c                   ELSE
086100050930      * recupero la descrizione della tariffa particolare dalla schiera
086200050930      *  caricata
086300050927     c                   z-add     1             xx
086400050927     c     TAVftc        lookup    $TarP_Cod(xx)                          10
086500050927if  2c                   if        *in10
086600050927     c                   eval      S1Dtxt      = $TarP_Cod(xx)
086700050927     c                                         + '-'
086800050927     c                                         + $TarP_Des(xx)
086900050927x   2c                   else
087000050927     c                   eval      S1Dtxt      = TAVftc
087100050927     c                                         + '-'
087200050927     c                                         + '? ? ? ? ? ? ? ? ? ?'
087300050927e   2c                   endif
087400050927e   1c                   ENDIF
087500050927      * Tipo azione
087600050927sel 1c                   select
087700050927w   1c                   when      TAVcta      = 'A'
087800050927     c                   eval      %subst(S1Dtxt :25 :4)
087900050927     c                                         = 'Ann.'
088000050927w   1c                   when      TAVcta      = 'I'
088100050927     c                   eval      %subst(S1Dtxt :25 :4)
088200050927     c                                         = 'Ins.'
088300050927w   1c                   when      TAVcta      = 'M'
088400050927     c                   eval      %subst(S1Dtxt :25 :4)
088500050927     c                                         = 'Man.'
088600050930w   1c                   when      TAVcta      = 'D'
088700050927     c                   eval      %subst(S1Dtxt :25 :4)
088800050927     c                                         = 'Dup.'
088900050930w   1c                   when      TAVcta      = 'O'
089000050927     c                   eval      %subst(S1Dtxt :25 :4)
089100050927     c                                         = 'Off.'
089200050927e   1c                   endsl
089300050927      *
089400050930      * Data variazione
089500050927     c                   reset                   WLBdat
089600050927     c                   eval      G08inv      = TAVdav
089700050927     c                   call      'XSRDA8'
089800050927     c                   parm                    WLBDAT
089900050927      *
090000050927     c                   eval      S1Dtxt      = %trimr(S1Dtxt)
090100050927     c                                         + '   '
090200050927     c                                         + %editc(G08dat :'Y')
090300050927     c                                         + ' '
090400050927     c                                         + %editw(TAVorv
090500050927     c                                                 :'0 :  :  ')
090600050927     c                                         + '  '
090700050927     c                                         + TAVpru
090800050927     c                                         + '  '
090900050927     c                                         + TAVnoj
091000050927      *
091100050927      * - Indicatori
091200050927     c***                eval      *in41       = *off
091300050927      *
091400050927     c                   ENDSR
091500050923      *
091600050923      *---------------------------------------------------------------*
091700050923      * Gestione tasto funzionale F12 da videata S01                  *
091800050923      * F12-Ritorno                                                   *
091900050923      *---------------------------------------------------------------*
092000050923     c     F12S01        BEGSR
092100050923      *
092200050923      * Torno alla videata precedente
092300050923     c                   eval      $Video      = '1'
092400050923      *
092500050923     c                   ENDSR
092600050930      *
092700050930      *---------------------------------------------------------------*
092800050930      * Recupero dati anagrafici del cliente                          *
092900050930      *---------------------------------------------------------------*
093000050930     c     DecodKSC      BEGSR
093100050930      *
093200050930     c                   clear                   ds_CNACO
093300050930     c                   clear                   ds_CNIND
093400050930     c*** già in input:  clear                   ds_CNCLP
093500050930     c                   clear                   ds_FNCLS
093600050930     c                   reset                   TIBS69ds
093700050930      *
093800050930     c                   eval      I69kac   = TAVksc
093900050930      *
094000050930     c                   call      'TIBS69R'
094100050930     c                   parm                    TIBS69ds
094200050930     c                   parm                    ds_CNACO
094300050930     c                   parm                    ds_CNIND
094400050930     c                   parm                    ds_CNCLP
094500050930     c                   parm                    ds_FNCLS
094600050930      *
094700050930     c                   ENDSR
094800050923      *
094900050923**   $Msg -------------------------------------------------------------------*
095000050923Immettere il codice commerciale                                                1
095100050923Codice commerciale errato                                                      2
095200050923Immettere una data variazione                                                  3
095300050923Data variazione errata                                                         4
095400050923Range di date errato                                                           5
095500051003Profilo utente errato                                                          6
095600090630Utente non abilitato alla gestione del commerciale indicato                    7
095700090701UTENTE NON ABILITATO ALLA GESTIONE TARIFFE                                     8
