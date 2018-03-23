000100050928      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050928      *===============================================================*
000300050928      *?FIDN8CR * Gestione disposizioni A.I.G.                       ?*
000400050928      *===============================================================*
000500050928
000600050928     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700050928
000800050928      *---------------------------------------------------------------*
000900050928
001000050928     fFNDRA01L  Uf A e           k disk
001100050928     fFNDCS01L  if A e           k disk
001200050928      *
001300050928     fFIDN8CD   cf   e             workstn
001400050928
001500050928      *---------------------------------------------------------------*
001600050928
001700050928      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001800050928      *
001900050928     d C_DataLimite    c                   const(20050418)
002000050928     d C_DspAtrPR      c                   const(x'A4')
002100050928     d C_DspAtrHIUL    c                   const(x'26')
002200050928      *
002300050928      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002400050928      *
002500050928      * - Messaggi di errore
002600050928     d $Msg            s             78    dim(04) ctdata  perrcd(1)
002700050928      *
002800050928      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002900050928      *
003000050928      * - Parametri in input
003100050928     d KPJBA         e ds
003200050928     d FIDN00ds      e ds                  inz
003300050928      *
003400050928      * - Parametri x Controllo profilo utenti
003500050928     d TIBS34DS      e ds                  inz
003600050928      * - Ds di riferimento al file esterno AZUTE00F
003700050928     d AZUTEds       e ds                  extname(AZUTE00F)
003800050928      * - Ds per dati organigramma
003900050928     d DDatiUte      e ds
004000050928      *
004100050928     d Status         sds           333
004200050928     d   SDSpgm          *proc
004300050928     d   SDSusr              254    263
004400050928      *
004500050928      * - Controllo/Inversione date
004600050928     d WLBdat          ds                  inz
004700050928     d   G08dat                       8  0 inz
004800050928     d   G08inv                       8  0 inz
004900050928     d   G08err                       1    inz('3')
005000050928     d   G08tgi                       5  0 inz
005100050928      *
005200050928     d DS_NumCA        ds                  inz
005300050928     d   H1Caac                            inz
005400050928     d   H1Cfil                            inz
005500050928     d   H1Cnca                            inz
005600050928      *
005700050928     d ds_NoteDisp     ds                  inz
005800050928     d   W1Cn01                            inz
005900050928     d   W1Cn02                            inz
006000050928     d   W1Cn03                            inz
006100050928     d   W1Cn04                            inz
006200050928     d   W1Cn05                            inz
006300050928     d   W1Cn06                            inz
006400050928     d   W1Cn07                            inz
006500050928     d   W1Cn08                            inz
006600050928     d   W1Cn09                            inz
006700050928     d   W1Cn10                            inz
006800050928     d  $NoteDisp              1    350    dim(10)
006900050928      *
007000050928     d ds_DspAtrNoteD  ds                  inz
007100050928     d   da01                              inz
007200050928     d   da02                              inz
007300050928     d   da03                              inz
007400050928     d   da04                              inz
007500050928     d   da05                              inz
007600050928     d   da06                              inz
007700050928     d   da07                              inz
007800050928     d   da08                              inz
007900050928     d   da09                              inz
008000050928     d   da10                              inz
008100050928     d  $DspAtrNoteD           1     10    dim(10)
008200050928      *
008300050928      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
008400050928      *
008500050928      * - Flags
008600050928     d $Fine           s              1    inz(*off)
008700050928     d $InzW01         s              1    inz(*on)
008800050928      *
008900050928      * - Indici di schiera
009000050928     d XX              s              3  0 inz
009100050928      *
009200050928      * - Variabili riferite al data base o al display-file
009300050928     d wLastPNO        s                   like(DCSpno)  inz
009400050928     d WWWdrd          s                   like(DRAdrd)  inz
009500050928      *
009600050928      * - Variabili definite a programma
009700050929     d Wdate_ISO       s               d   datfmt(*iso) inz
009800050928     d Wdate           s              8  0 inz
009900050929     d*Wtime***        s              6  0 inz
010000050928      *
010100050928      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
010200050928      *
010300050928      * - FNDRA01L
010400050928     c     K03DRA01      klist
010500050928     c                   kfld                    H1Caac
010600050928     c                   kfld                    H1Cfil
010700050928     c                   kfld                    H1Cnca
010800050928      * - FNDCS01L
010900050928     c     K07DCS01      klist
011000050928     c                   kfld                    DCStpd
011100050928     c                   kfld                    DCSnkt
011200050928     c                   kfld                    DCSstd
011300050928     c                   kfld                    DCSdim
011400050928     c                   kfld                    DCShim
011500050928     c                   kfld                    DCSnks
011600050928     c                   kfld                    DCStrc
011700050928     c*** NO progressivo kfld                    DCSpno
011800050928
011900050928      *---------------------------------------------------------------*
012000050928      *  RIEPILOGO INDICATORI                                         *
012100050928      *---------------------------------------------------------------*
012200050928      * 06     - Manutenzione (consentita immissione note)            *
012300050928      * 06*off - Visualizzazione                                      *
012400050928      * 10     - Comodo                                               *
012500050928      * 28     - Emette il messaggio di errore a video                *
012600050928      * 90     - Generico di errore                                   *
012700050928      *---------------------------------------------------------------*
012800050928
012900050928     c     *Entry        plist
013000050928     c                   parm                    KPJBA
013100050928     c                   movel     KPJBU         FIDN00ds
013200050928      *
013300050928      * Operazioni Iniziali
013400050928     c                   exsr      RoutInz
013500050928      *
013600050928      * Gestione Video
013700050928do  1c                   DOW       $Fine    = *off
013800050928cas 2c                   exsr      GesW01
013900050928e   1c                   ENDDO
014000050928      *
014100050928      * Fine
014200050929     c                   movel(p)  FIDN00ds      KPJBU
014300050928     c                   movel     *on           *inLR
014400050928      *
014500050928      *---------------------------------------------------------------*
014600050928      * Operazioni Iniziali                                           *
014700050928      *---------------------------------------------------------------*
014800050928     c     RoutInz       BEGSR
014900050928      *
015000050928      * Reperisco dati job
015100050928     c                   exsr      DatiJob
015200050928      *
015300050928     c***                movel     SDSpgm        V1Tpgm
015400050929     c                   move      *date         Wdate_iso
015500050929     c                   move      Wdate_iso     Wdate
015600050929     c***                time                    Wtime
015700050928      *
015800050928     c                   ENDSR
015900050928      *
016000050928      *---------------------------------------------------------------*
016100050928      * Reperimento Dati del job (Utente/Operativi)                   *
016200050928      *---------------------------------------------------------------*
016300050928     c     DatiJob       BEGSR
016400050928      *
016500050928     c     *dtaara       define    §azute        azuteds
016600050928     c     *dtaara       define    §datiute      ddatiute
016700050928      *
016800050928     c                   in(E)     *dtaara
016900050928     c                   IF        %ERROR or RSUT = *blanks
017000050928     c                   clear                   Tibs34Ds
017100050928     c                   call      'TIBS34R'
017200050928     c                   parm                    Tibs34Ds
017300050928     c                   in        *dtaara
017400050928     c                   ENDIF
017500050928      *
017600050928     c                   ENDSR
017700050928      *
017800050928      *---------------------------------------------------------------*
017900050928      * Gestione videata D01                                          *
018000050928      *---------------------------------------------------------------*
018100050928     c     GesW01        BEGSR
018200050928      *
018300050928      * Inizializzo la videata
018400050928if  1c                   if        $InzW01     = *on
018500050928     c                   exsr      InzW01
018600050928     c                   movel     *off          $InzW01
018700050928e   1c                   endif
018800050928      *
018900050928      * Emetto la videata
019000050928     c                   exfmt     FI8CW01
019100050928     c                   setoff                                       28  90
019200050928     c                   clear                   V1Dmsg
019300050928      *
019400050928sel 1c                   select
019500050928      * F12=Ritorno
019600050928w   1c                   when      *inKL
019700050928     c                   exsr      F12W01
019800050928     c                   leavesr
019900050928      *
020000050928e   1c                   endsl
020100050928      *
020200050928      * Controllo dati immessi a video
020300050929      * (se NON visualizzazione)
020400050929if  1c                   if        I00mod     <> 'I'
020500050928     c                   exsr      CtrW01
020600050928     c     *in90         cabeq     *on           EndGesW01
020700050929e   1c                   endif
020800050928      *
020900050928      * F6=Conferma
021000050929      * (NON consentito in visualizzazione)
021100050928if  1c                   if        *inKF
021200050928     c                   exsr      F06W01
021300050928e   1c                   endif
021400050928      *
021500050928     c     EndGesW01     ENDSR
021600050928      *
021700050928      *---------------------------------------------------------------*
021800050928      * Inizializzazione videata W01                                  *
021900050928      *---------------------------------------------------------------*
022000050928     c     InzW01        BEGSR
022100050928      *
022200050928     c                   clear                   FI8CW01
022300050928      *
022400050928      * Impostazione campi hidden
022500050928     c                   eval      H1Caac      = I00aac
022600050928     c                   eval      H1Cfil      = I00fil
022700050928     c                   eval      H1Cnca      = I00nca
022800050929      *
022900050929      * Testata della window W01
023000051005     ***c                   eval      W1Dtxt      = 'Disposizioni A.I.G.'
023100051005     ***c                                         + '         '
023200051005     ***c                                         + %trim(SDSpgm)
023300051005     c                   eval      %subst(W1Tpgm :
023400051005     c                                    %len(W1Tpgm) -
023500051005     c                                      %len(%trim(SDSpgm)) +
023600051005     c                                      1 :
023700051005     c                                    %len(%trim(SDSpgm))) =
023800051005     c                             %trim(SDSpgm)
023900050928      *
024000050928      * Reperimento data ricezione delle disposizioni A.I.G.
024100051006if  1c                   if        I00mod      = 'I'
024200051006     c     K03DRA01      chain(n)  FNDRA000
024300051006x   1c                   else
024400050928     c     K03DRA01      chain     FNDRA000
024500051006e   1c                   endif
024600050928      * - annullato
024700051006if  1c                   dow           %found(FNDRA01L)
024800051006     c                             and DRAatb <> *blanks
024900051006     c                             and I00mod <> 'I'
025000050928     c                   delete    FNDRA000
025100050928     c     K03DRA01      chain     FNDRA000
025200050928e   1c                   enddo
025300050928      * - non reperito
025400050928if  1c                   if        NOT %found(FNDRA01L)
025500051006     c                             or  DRAatb <> *blanks
025600050928     c                   clear                   FNDRA000
025700050928     c                   eval      DRAaac      = H1Caac
025800050928     c                   eval      DRAfil      = H1Cfil
025900050928     c                   eval      DRAnca      = H1Cnca
026000050928e   1c                   endif
026100050929      * - reperito     (emesso in solo visualizzazione)
026200050928if  1c                   if        %found(FNDRA01L)
026300051006if  2c                   if        I00mod     <> 'I'
026400050928     c                   unlock    FNDRA01L
026500051006e   2c                   endif
026600050928if  2c                   if             DRAatb = *blanks
026700050928     c                             and  DRAdrd > *zeros
026800050928     c                   reset                   WLBdat
026900050928     c                   z-add     DRAdrd        G08inv
027000050928     c                   call      'XSRDA8'
027100050928     c                   parm                    WLBdat
027200050928     c                   z-add     G08dat        W1Cdrd
027300050929     c                   movel     C_DspAtrPR    DA00
027400050928e   2c                   endif
027500050928e   1c                   endif
027600050928      *
027700050928      * Caricamento note
027800050928     c                   movel     'C'           DCStpd
027900050928     c                   movel(p)  DS_numca      DCSnkt
028000050928     c                   movel     'D'           DCSstd
028100050928     c                   clear                   DCSdim
028200050928     c                   clear                   DCShim
028300050928     c                   clear                   DCSnks
028400050928     c                   movel     'D'           DCStrc
028500050928     c     K07DCS01      setll     FNDCS000
028600050928     c     K07DCS01      reade     FNDCS000
028700050928     c                   clear                   xx
028800050928      *
028900050928do  1c                   DOW       NOT  %eof(FNDCS01L)
029000050928     c                             and  xx     < %elem($NoteDisp)
029100050928      *
029200050928if  2c                   if        DCSatb      = *blanks
029300050928     c                   eval      xx               = xx + 1
029400050928     c                   eval      $NoteDisp(xx)    = DCSnot
029500050928     c                   eval      $DspAtrNoteD(xx) = C_DspAtrPR
029600050928     c                   eval      wLastPNO         = DCSpno
029700050928e   2c                   endif
029800050928      *
029900050928     c     K07DCS01      reade     FNDCS000
030000050928      *
030100050928e   1c                   ENDDO
030200050928      *
030300050928      * Se sono in visualizzazione proteggo comunque TUTTE le note
030400050929      *  e la data ricezione disposizioni
030500050928if  1c                   if        I00mod      = 'I'
030600050929     c                   movel     C_DspAtrPR    DA00
030700050928     c                   movel     C_DspAtrPR    $DspAtrNoteD
030800050928e   1c                   endif
030900050928      *
031000050928      * Indicatori
031100050929      * - inibizione tasto funzionale F6-Conferma
031200050928     c                   eval      *in06       = (I00mod <> 'I')
031300050928      *
031400050928     c                   ENDSR
031500050928      *
031600050928      *---------------------------------------------------------------*
031700050928      * Controllo dati immessi in videata W01                         *
031800050928      *---------------------------------------------------------------*
031900050928     c     CtrW01        BEGSR
032000050928      *
032100050928     c                   movea     *zeros        *in(50)
032200050928      *
032300050928      * Data ricezione disposizioni A.I.G.
032400050928     c                   clear                   WWWdrd
032500050928      * - controllo immissione
032600050928if  1c                   if        W1Cdrd      = *zeros
032700050928     c                   seton                                        502890
032800050928     c                   eval      V1Dmsg      = $Msg(01)
032900050928     c                   leavesr
033000050928x   1c                   else
033100050928      * - controllo validità formale data
033200050928     c                   clear                   WLBdat
033300050928     c                   eval      G08dat      = W1Cdrd
033400050928     c                   call      'XSRDA8'
033500050928     c                   parm                    WLBdat
033600050928if  2c                   if        G08err      = *on
033700050928     c                   seton                                        502890
033800050928     c                   eval      V1Dmsg      = $Msg(02)
033900050928     c                   leavesr
034000050928e   2c                   endif
034100050928     c                   eval      W1Cdrd      = G08dat
034200050928     c                   eval      WWWdrd      = G08inv
034300050928e   1c                   endif
034400050928      * - controllo valori immessi
034500050928sel 1c                   select
034600050928w   1c                   when      WWWdrd      < C_DataLimite
034700050928     c                   seton                                        502890
034800050928     c                   eval      V1Dmsg      = $Msg(03)
034900050928     c                   leavesr
035000050928w   1c                   when      WWWdrd      > Wdate
035100050928     c                   seton                                        502890
035200050928     c                   eval      V1Dmsg      = $Msg(04)
035300050928     c                   leavesr
035400050928e   1c                   endsl
035500050928      *
035600050928     c                   ENDSR
035700050928      *
035800050928      *---------------------------------------------------------------*
035900050928      * Gestione tasto funzionale F12 da videata W01                  *
036000050928      *---------------------------------------------------------------*
036100050928     c     F12W01        BEGSR
036200050928      *
036300050929      * Informo il chiamante che è stato premuto F12
036400050929     c                   eval      O00F12      = 'S'
036500050928      * Chiudo il programma
036600050928     c                   eval      $Fine       = *on
036700050928      *
036800050928     c                   ENDSR
036900050928      *
037000050928      *---------------------------------------------------------------*
037100050928      * Gestione tasto funzionale F06 da videata W01                  *
037200050928      *---------------------------------------------------------------*
037300050928     c     F06W01        BEGSR
037400050928      *
037500050928      * Aggiornamento FNDRA00F
037600050928      * (solo scrittura se nuovo record,
037700050928      *  non aggiornamento se già esistente)
037800050928if  1c                   if        NOT %found(FNDRA01L)
037900050928     c                   clear                   FNDRA000
038000050928     c                   eval      DRAaac = H1Caac
038100050928     c                   eval      DRAfil = H1Cfil
038200050928     c                   eval      DRAnca = H1Cnca
038300050928     c                   eval      DRAdrd = WWWdrd
038400050928      *
038500050928     c                   WRITE     FNDRA000
038600050928      *
038700050928e   1c                   endif
038800050928      *
038900050928      * Aggiornamento FNDCS00F
039000050928      * (solo scrittura se nuovo record,
039100050928      *  non aggiornamento se già esistente)
039200050928     c                   clear                   FNDCS000
039300050928     c                   movel     'C'           DCStpd
039400050928     c                   movel(p)  DS_numca      DCSnkt
039500050928     c                   movel     'D'           DCSstd
039600050929     c*** NO:            z-add     Wdate         DCSdim
039700050929     c*** NO:            movel     Wtime         DCShim
039800050928     c                   clear                   DCSnks
039900050928     c                   movel     'D'           DCStrc
040000050928     c                   z-add     wLastPNO      DCSpno
040100050928     c                   eval      xx          = 1
040200050928      *
040300050928do  1c                   DOW       xx          < %elem($NoteDisp)
040400050928      *
040500050928if  2c                   if            $NoteDisp(xx)    <> *blanks
040600050928     c                             and $DspAtrNoteD(xx) <> C_DspAtrPR
040700050928      *
040800050928     c                   add       1             DCSpno
040900050928     c                   eval      DCSnot      = $NoteDisp(xx)
041000050928     c                   eval      DCSpru      = SDSusr
041100050928     c                   eval      DCSpos      = SIMfel
041200050928      *
041300050928     c                   WRITE     FNDCS000
041400050928      *
041500050928e   2c                   endif
041600050928      *
041700050928     c                   add       1             xx
041800050928      *
041900050928e   1c                   ENDDO
042000050928      *
042100050928      * Chiudo il programma
042200050928     c                   eval      $Fine       = *on
042300050928      *
042400050928     c                   ENDSR
042500050928
042600050928      *---------------------------------------------------------------*
042700050928
042800050928**    $Msg ------------------------------------------------------------------*
042900050928Data ricezione disposizioni A.I.G. obbligatoria                                1
043000050928Data ricezione disposizioni A.I.G. formalmente errata                          2
043100050928Data ricezione disposizioni A.I.G. precedente il 18/04/2005                    3
043200050928Data ricezione disposizioni A.I.G. successiva a quella odierna                 4
