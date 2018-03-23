000100061222      *---------------------------------------------------------------*
000200061222      * Gestione tabella "PSC" = % supplemento carburante             *
000300061222      *---------------------------------------------------------------*
000400061222
000500061222     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600061222
000700061222      *---------------------------------------------------------------*
000800061222
000900061222     fTNTBE01L  Uf A e           k disk
001000061222      *
001100061222     fTNTB43D   cf   e             workstn
001200061222     f                                     sfile(TB43S01:S01nrr)
001300061222
001400061222      *---------------------------------------------------------------*
001500061222
001600061222      *
001700061222      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001800061222      *
001900061222      * - Codice tabella in gestione
002000061222     d C_CodTab        c                   const('PSC')
002100061222      * - Nr. di righe in ogni pagina del sfl (SFLPAG)
002200061222     d C_SflPag        c                   const(14)
002300061222      *
002400061222      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002500061222      *
002600061222     d $Msg            s             78    dim(10)  ctdata  perrcd(1)
002700061222      *
002800061222      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
002900061222      *
003000061222      * Parametri
003100061222     d KPJBA         e ds
003200061222      *
003300061222      * Tabella PSC = % supplemento carburante
003400061222     d dPSC          e ds                  inz
003500061222      *
003600061222      * Tracciato record file TNTBE00F
003700061222     d TNTBEds       e ds                  extname(TNTBE00F) inz
003800061222     d xTNTBEds      e ds                  extname(TNTBE00F) inz
003900061222     d                                     prefix(TBX:3)
004000061222      *
004100061222     d TIBS34ds      e ds                  inz
004200061222     d dDatiUte      e ds                  inz
004300061222     d AZUTEds       e ds                  extname(AZUTE00F) inz
004400061222      *
004500061222     d WLBdat          ds                  inz
004600061222     d  G08dat                 1      8  0 inz
004700061222     d  G08inv                 9     16  0 inz
004800061222     d  G08err                17     17    inz('3')
004900061222     d  G08tgi                18     22  0 inz
005000061222      *
005100061222     d Status         sds
005200061222     d  SDSpgm           *proc
005300061222      *
005400061222      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
005500061222      *
005600061222      * - Flags
005700061222     d $Fine           s              1    inz(*off)
005800061222     d $F10            s              1    inz(*off)
005900061222     d $InzS01         s              1    inz(*on)
006000061222     d $InzD01         s              1    inz(*on)
006100061222      *
006200061222     d $Video          s              1    inz('1')
006300061222      *
006400061222      * - Indici di schiera
006500061222     d xx              s              5  0 inz
006600061222      *
006700061222      * - Variabili riferite al data base o al display file
006800061222     d S01nrr          s                   like(C01rcd)  inz
006900061222     d SavS01csr       s                   like(C01rcd) inz
007000061222     d wPag            s              4  0 inz
007100061222     d wRig            s              3  0 inz
007200061222      *
007300061222      * - Altri campi
007400061222     d wDate           s              8  0 inz
007500061222     d wData_1         s               d   inz  datfmt(*jul)
007600061222     d wData_2         s               d   inz  datfmt(*jul)
007700061222     d W1Cdad          s                   inz  like(§PSCdad)
007800061222      *
007900061222      * Chiavi di accesso  - - - - - - - - - - - - - - - - - - - - - -*
008000061222      *
008100061222     c     K05TBE01      klist                                                  *tntbe01l
008200061222     c                   kfld                    TBEcod                         -tabella
008300061222     c                   kfld                    TBEke1                         -chiave uno
008400061222     c                   kfld                    TBEke2                         -chiave due
008500061222     c                   kfld                    TBElin                         -lingua
008600061222     c                   kfld                    TBEsif                         -s.informativo
008700061222
008800061222      *---------------------------------------------------------------*
008900061222      * I N D I C A T O R I                                           *
009000061222      *---------------------------------------------------------------*
009100061222      *  06     - Abilitato F6=Conferma in D02                        *
009200061222      *  10     - Comodo                                              *
009300061222      *  28     - Emissione messaggio di errore a video               *
009400061222      *  30     - Pulizia del subfile                                 *
009500061222      *  31     - NON emissione del subfile                           *
009600061222      *  32     - Record modificato nel subfile (SflNxtChg)           *
009700061222      *  33     - Fine dati nel subfile         (SflEnd)              *
009800061222      *  50     - Opzione errata                                      *
009900061222      *  51     - Data decorrenza errata                              *
010000061222      *  52     - Percentuale supplemento carburante errata           *
010100061222      *  90     - Generico di errore                                  *
010200061222      *---------------------------------------------------------------*
010300061222
010400061222     c     *Entry        plist
010500061222     c                   parm                    KPJBA
010600061222      *
010700061222      * Operazioni iniziali
010800061222     c                   exsr      RoutInz
010900061222      *
011000061222      * Gestione video
011100061222     c                   dow       $Fine   = *off
011200061222     c     $Video        caseq     '1'           GesS01
011300061222     c     $Video        caseq     '2'           GesD01
011400061222     c                   endcs
011500061222     c                   enddo
011600061222      *
011700061222      * Fine
011800061222     c                   eval      *inLR   = *on
011900061222
012000061222      *---------------------------------------------------------------*
012100061222      * Operazioni Iniziali                                           *
012200061222      *---------------------------------------------------------------*
012300061222     c     RoutInz       BEGSR
012400061222      *
012500061222      * Reperisco dati job
012600061222     c                   exsr      DatiJob
012700061222      *
012800061222     c                   movel     SDSpgm        V1Tpgm
012900061222     c                   clear                   WLBdat
013000061222     c                   eval      G08dat  = *date
013100061222     c                   call      'XSRDA8'
013200061222     c                   parm                    WLBdat
013300061222     c                   movel     G08inv        wDate
013400061222      *
013500061222      * Aggancio dati generali della tabella in esame
013600061222     c                   clear                   TBEcod
013700061222     c                   move      *zeros        TBEke1
013800061222     c                   move      C_CodTab      TBEke1
013900061222     c                   clear                   TBEke2
014000061222     c                   clear                   TBElin
014100061222     c                   movel     KNSIF         TBEsif
014200061222     c     K05TBE01      chain(n)  TNTBE01L
014300061222     c                   if        not %found(TNTBE01L)
014400061222     c                   clear                   TBEsif
014500061222     c     K05TBE01      chain(n)  TNTBE01L
014600061222     c                   endif
014700061222     c                   if        %found(TNTBE01L)
014800061222     c                   movel     TNTBEds       xTNTBEds
014900061222     c                   else
015000061222     c                   clear                   xTNTBEds
015100061222     c                   endif
015200061222      *
015300061222     c                   ENDSR
015400061222
015500061222      *---------------------------------------------------------------*
015600061222      * Reperimento Dati del job (Utente/Operativi)                   *
015700061222      *---------------------------------------------------------------*
015800061222     c     DatiJob       BEGSR
015900061222      *
016000061222     c     *dtaara       define    §azute        azuteds
016100061222     c     *dtaara       define    §datiute      ddatiute
016200061222      *
016300061222     c                   in(E)     *dtaara
016400061222     c                   IF        %ERROR or RSUT = *blanks
016500061222     c                   clear                   Tibs34Ds
016600061222     c                   call      'TIBS34R'
016700061222     c                   parm                    Tibs34Ds
016800061222     c                   in        *dtaara
016900061222     c                   ENDIF
017000061222      *
017100061222      *-- Verifica errori e autorità profilo
017200061222sel 1c                   SELECT
017300061222      *-- controllo se ho errori nei dati utente
017400061222      *--   nel qual caso NON risulta un profilo abilitato
017500061222w   1c                   WHEN      DUTerr  = 'E'
017600061222     c                   eval      $Fine   = *on
017700061222      *
017800061222      *-- CONTROLLO AUTORITA'
017900061222      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
018000061222      *-- se il chiamante non richiede autorità specifica verificare
018100061222      *--   quella del profilo
018200061222      *-- se il chiamante richiede autorità specifica verificarla,
018300061222      *--  se è blank verificare quella del profilo
018400061222      *
018500061222      * se UTEAUT = *BLANK non siamo in sede
018600061222w   1c                   WHEN      UTEaut  = *blank
018700061222      *
018800061222x   1c                   OTHER
018900061222      *
019000061222e   1c                   ENDSL
019100061222      *
019200061222     c                   ENDSR
019300061222
019400061222      *---------------------------------------------------------------*
019500061222      * Gestione videata C01/S01                                      *
019600061222      *---------------------------------------------------------------*
019700061222     c     GesS01        BEGSR
019800061222      *
019900061222      * Inizializzazione videata
020000061222if  1c                   if        $InzS01 = *on
020100061222     c                   exsr      InzS01
020200061222     c                   movel     *off          $InzS01
020300061222e   1c                   endif
020400061222      *
020500061222      * Emissione Testata e Piede con tasti funzionali abilitati
020600061222if  1c                   if        NOT *in90
020700061222     c                   write     TB43T01
020800061222     c                   write     TB43P01
020900061222e   1c                   endif
021000061222      *
021100061222      * Posizionamento del cursore
021200061222w   1c                   if        C01csr  > *zeros
021300061222     c                   eval      C01rcd  = C01csr
021400061222     c                   else
021500061222     c                   eval      C01rcd  = 1
021600061222     c                   endif
021700061222     c                   eval      *in30       = (C01rcd <= *zeros)
021800061222      *
021900061222      * Emissione videata
022000061222     c                   exfmt     TB43C01
022100061222     c                   setoff                                       28  90
022200061222     c                   clear                   V1Dmsg
022300061222      *
022400061222sel 1c                   SELECT
022500061222      *
022600061222      * F3=Fine
022700061222w   1c                   WHEN      *inKC
022800061222     c                   exsr      F03S01
022900061222      *
023000061222      * F10=Inserimento
023100061222w   1c                   WHEN      *inKJ
023200061222     c                   exsr      F10S01
023300061222      *
023400061222x   1c                   OTHER
023500061222      * Gestione Opzioni
023600061222     c                   exsr      OpzS01
023700061222if  2c                   if        *in90
023800061222     c                   leavesr
023900061222e   2c                   endif
024000061222      *
024100061222e   1c                   ENDSL
024200061222      *
024300061222     c                   ENDSR
024400061222
024500061222      *---------------------------------------------------------------*
024600061222      * Inizializzazione videata C01/S01                              *
024700061222      *---------------------------------------------------------------*
024800061222     c     InzS01        BEGSR
024900061222      *
025000061222      * Pulizia subfile
025100061222     c                   seton                                        3031
025200061222     c                   write     TB43C01
025300061222     c                   setoff                                         3133
025400061222      *
025500061222     c                   clear                   C01rcd
025600061222     c                   clear                   C01csr
025700061222     c                   clear                   S01nrr
025800061222     c                   clear                   V1Dmsg
025900061222     c                   setoff                                       28  90
026000061222     c                   movea     *zeros        *in(50)
026100061222      *
026200061222      * Caricamento dati già immessi (protetti)
026300061222      *N.B.?- i dati dell'ultimo record letto rimangono nella ds DPSC!
026400061222     c     C_CodTab      setll     TNTBE000
026500061222     c     C_CodTab      reade     TNTBE000
026600061222     c                   exsr      CarS01
026700061222      *
026800061222      * Visualizzazione del SFL (se ci sono dati)
026900061222     c                   eval      *in30   = (S01nrr = *zeros)
027000061222      * Attivazione (eventuale) del SFLEND
027100061222     c                   eval      *in33   = %eof(TNTBE01L)
027200061222      *
027300061222      * Posizionamento cursore al 1° rcd dell'ultima pagina
027400061222if  1c                   if        S01nrr  > *zeros
027500061222     c     S01nrr        div       C_SflPag      wPag
027600061222     c                   mvr                     wRig
027700061222     c                   eval      C01rcd  = wPag * C_SflPag
027800061222     c                   if        wRig    > *zeros
027900061222     c                   eval      C01rcd  = C01rcd + 1
028000061222     c                   else
028100061222     c                   eval      C01rcd  = C01rcd - C_SflPag  + 1
028200061222     c                   endif
028300061222x   1c                   else
028400061222     c                   clear                   C01rcd
028500061222e   1c                   endif
028600061222     c                   eval      C01csr  = C01rcd
028700061222      *
028800061222     c                   ENDSR
028900061222
029000061222      *---------------------------------------------------------------*
029100061222      * Caricamento dati nel subfile                                  *
029200061222      *---------------------------------------------------------------*
029300061222     c     CarS01        BEGSR
029400061222      *
029500061222      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
029600061222do  1c                   DOW       NOT  %eof(TNTBE01L)
029700061222     c                             and  NOT *in33
029800061222      *
029900061222if  2c*** anche ann.:    if        TBEatb  = *blanks
030000061222     c                   movel     TBEuni        dPSC
030100061222     c                   exsr      RieS01
030200061222     c                   add       1             S01nrr
030300061222     c                   write     TB43S01
030400061222e   2c***                endif
030500061222      *
030600061222     c     C_CodTab      reade     TNTBE000
030700061222      *
030800061222e   1c                   ENDDO
030900061222      *
031000061222     c                   ENDSR
031100061222
031200061222      *---------------------------------------------------------------*
031300061222      * Impostazione dati in singolo record del sfl S01               *
031400061222      *---------------------------------------------------------------*
031500061222     c     RieS01        BEGSR
031600061222      *
031700061222     c                   clear                   TB43S01
031800061222      *
031900061222      * Campi hidden
032000061222     c                   eval      H1Cdad  = §PSCdad
032100061222      * Campi di i/o
032200061222     c*** già così:      clear                   S1Copz
032300061222      * Campi di solo output
032400061222     c                   eval      S1Catb  = TBEatb
032500061222     c                   movel     TBEke1        S1Cke1
032600061222      * - Data decorrenza
032700061222     c                   reset                   WLBdat
032800061222     c                   eval      G08inv  = §PSCdad
032900061222     c                   call      'XSRDA8'
033000061222     c                   parm                    WLBdat
033100061222     c                   eval      S1Cdad  = G08dat
033200061222      * - % supplemento carburante
033300061222     c                   eval      S1Cper  = §PSCper
033400061222      *
033500061222     c                   ENDSR
033600061222
033700061222      *---------------------------------------------------------------*
033800061222      * Gestione tasto funzionale F03 da videata S01                  *
033900061222      *---------------------------------------------------------------*
034000061222     c     F03S01        BEGSR
034100061222      *
034200061222      * Chiusura del programma
034300061222     c                   movel     *on           $Fine                          fine pgm
034400061222      *
034500061222     c                   ENDSR
034600061222
034700061222      *---------------------------------------------------------------*
034800061222      * Gestione tasto funzionale F10 da videata S01                  *
034900061222      *---------------------------------------------------------------*
035000061222     c     F10S01        BEGSR
035100061222      *
035200061222      * Passaggio alla videata di immissione (D01)
035300061222     c                   eval      $Video  = '2'
035400061222     c                   eval      $inzD01 = *on
035500061222      *
035600061222     c                   eval      $F10    = *on
035700061222      *
035800061222     c                   ENDSR
035900061222
036000061222      *---------------------------------------------------------------*
036100061222      * Gestione opzioni subfile                                      *
036200061222      *---------------------------------------------------------------*
036300061222     c     OpzS01        BEGSR
036400061222      *
036500061222     c                   clear                   SavS01csr
036600061222      *
036700061222     c                   readc     TB43S01
036800061222      *
036900061222do  1c                   DOW       NOT %eof(TNTB43D)
037000061222      *
037100061222     c                   eval      *in32   = *off
037200061222     c                   movea     *zeros        *in(51)
037300061222     c                   z-add     S01nrr        C01rcd
037400061222      *
037500061222sel 2c                   SELECT
037600061222      * - Nessuna opzione
037700061222w   2c                   WHEN      S1Copz  = *blanks
037800061222      * - 5 = Visualizzazione prezzo medio gasolio
037900061222w   2c                   WHEN      S1Copz  = '5'
038000061222     c                   exsr      sr_OpzS1_5
038100061222      * - ? = Opzione NON valida
038200061222x   2c                   OTHER
038300061222     c                   seton                                        285190
038400061222     c                   eval      V1Dmsg  = $Msg(1)
038500061222e   2c                   ENDSL
038600061222      *
038700061222      * Memorizzazione nrr del sfl con la 1ª opzione
038800061222      * per riposizionarvici il cursore dopo il tasto "Invio"
038900061222if  1c                   if             S1Copz   <> *blanks
039000061222     c                             and (SavS01csr = *zeros
039100061222     c                              or  SavS01csr < C01rcd)
039200061222     c                   eval      SavS01csr   = C01rcd
039300061222e   1c                   endif
039400061222      *
039500061222      * Aggiornamento sfl
039600061222sel 2c                   select
039700061222w   2c                   when      *in28
039800061222     c                   eval      *in32       = *on
039900061222     c                   z-add     C01rcd        C01csr
040000061222w   2c                   when      *in90       = *on
040100061222     c                   z-add     C01rcd        C01csr
040200061222     c                   clear                   S1Copz
040300061222x   2c                   other
040400061222     c                   clear                   S1Copz
040500061222e   2c                   endsl
040600061222     c                   UPDATE    TB43S01
040700061222if  2c                   if        *in28  OR  *in90
040800061222     c                   leave
040900061222e   2c                   endif
041000061222      *
041100061222     c                   readc     TB43S01
041200061222      *
041300061222e   1c                   ENDDO
041400061222      *
041500061222      * Riposiziono il cursore sul record contenente la prima opzione
041600061222      *  (se non sono stati rilevati errori)
041700061222if  1c                   if        NOT *in28  and  NOT *in90
041800061222     c                             and SavS01csr  > *zeros
041900061222     c                   z-add     SavS01csr     C01csr
042000061222e   1c                   endif
042100061222      *
042200061222     c                   ENDSR
042300061222
042400061222      *---------------------------------------------------------------*
042500061222      * S1_Opz.5 = Visualizzazione singolo record                     *
042600061222      *---------------------------------------------------------------*
042700061222     c     sr_OpzS1_5    BEGSR
042800061222      *
042900061222      * Passaggio alla videata di visualizzazione (D01)
043000061222     c                   eval      $Video  = '2'
043100061222     c                   eval      $inzD01 = *on
043200061222      *
043300061222     c                   eval      $F10    = *off
043400061222      *
043500061222      * ...Meglio l'esecuzione immediata:
043600061222do  1c                   dow       $Video  = '2'
043700061222     c                   exsr      GesD01
043800061222e   1c                   enddo
043900061222      *
044000061222     c                   ENDSR
044100061222      *
044200061222      *---------------------------------------------------------------*
044300061222      * Gestione videata D01                                          *
044400061222      *---------------------------------------------------------------*
044500061222     c     GesD01        BEGSR
044600061222      *
044700061222      * Inizializzazione videata
044800061222if  1c                   if        $InzD01 = *on
044900061222     c                   exsr      InzD01
045000061222     c                   movel     *off          $InzD01
045100061222e   1c                   endif
045200061222      *
045300061222      * Emissione Testata e Piede con tasti funzionali abilitati
045400061222if  1c                   if        NOT  *in28
045500061222     c                   write     TB43T01
045600061222     c                   write     TB43P02
045700061222e   1c                   endif
045800061222      * Emissione videata
045900061222if  1c                   if        $F10    = *off
046000061222     c                   write     TB43D01
046100061222     c                   exfmt     PROTECT
046200061222x   1c                   else
046300061222     c                   exfmt     TB43D01
046400061222e   1c                   endif
046500061222     c                   setoff                                       28  90
046600061222     c                   clear                   V1Dmsg
046700061222      *
046800061222sel 1c                   SELECT
046900061222      * F3=Fine visualizzazioni
047000061222w   1c                   WHEN      *inKC
047100061222     c                   exsr      F03D01
047200061222      * F12=Ritorno
047300061222w   1c                   WHEN      *inKL
047400061222     c                   exsr      F12D01
047500061222      * Invio / F6 (in immissione)
047600061222x   1c                   OTHER
047700061222if  2c                   if        $F10    = *on
047800061222     c                   exsr      CtrD01
047900061222if  3c                   if        *in90
048000061222     c                   leavesr
048100061222e   3c                   endif
048200061222e   2c                   endif
048300061222      * F6=Conferma
048400061222if  2c                   if             *inKF   = *on
048500061222     c                             and  *in06   = *on
048600061222     c                   exsr      F06D01
048700061222if  3c                   if        NOT  *in90
048800061222     c                   eval      $InzS01 = *on
048900061222     c                   eval      $Video  = '1'
049000061222e   3c                   endif
049100061222e   2c                   endif
049200061222      *
049300061222e   1c                   ENDSL
049400061222      *
049500061222     c                   ENDSR
049600061222
049700061222      *---------------------------------------------------------------*
049800061222      * Inizializzazione videata D01                                  *
049900061222      *---------------------------------------------------------------*
050000061222     c     InzD01        BEGSR
050100061222      *
050200061222     c                   clear                   TB43D01
050300061222      *
050400061222      * Impostazione indicatori:
050500061222      * *in03 = abilita F3 per ritorno a S01 senza proseguire con
050600061222      *         l'elaborazione delle opzioni successive
050700061222     c                   eval      *in03   = ($F10  = *off)
050800061222      * *in06 = abilita F6 per conferma inserimento record
050900061222     c                   eval      *in06   = ($F10  = *on)
051000061222      *
051100061222sel 1c                   select
051200061222      * Se è stata richiesta l'opzione 5=Visualizzazione
051300061222      *   si reperiscono i dati da visualizzare dal sfl:
051400061222w   1c                   when           S1Copz = '5'
051500061222     c                             and  $F10   = *off
051600061222      * - Visualizzazione dati
051700061222     c                   eval      V1Cke1  = S1Cke1
051800061222     c                   eval      V1Cdad  = S1Cdad
051900061222     c                   eval      V1Cper  = S1Cper
052000061222      *
052100061222w   1c                   when      $F10    = *on
052200061222      * - Incremento numero progressivo
052300061222      *N.B.?- i dati dell'ultimo record letto in fase di caricamento
052400061222      *        del subfile sono ancora nella ds DPSC!
052500061222     c     C_CodTab      setgt     TNTBE000
052600061222     C     C_CodTab      readpe(n) TNTBE000
052700061222     c                   movel     TBEke1        V1Cke1
052800061222     c                   add       1             V1Cke1
052900061222      *
053000061222e   1c                   endsl
053100061222      *
053200061222     c                   ENDSR
053300061222
053400061222      *---------------------------------------------------------------*
053500061222      * Controllo dati immessi in videata D01                         *
053600061222      *---------------------------------------------------------------*
053700061222     c     CtrD01        BEGSR
053800061222      *
053900061222      *N.B.?- i dati dell'ultimo record letto in fase di caricamento
054000061222      *        del subfile sono ancora nella ds DPSC!
054100061222     c                   movea     *zeros        *in(51)
054200061222      *
054300061222      * Controllo formale data
054400061222      * - mancanza
054500061222if  1c                   if        V1Cdad  = *zeros
054600061222     c                   seton                                        285290
054700061222     c                   eval      V1Dmsg  = $Msg(3)
054800061222     c                   leavesr
054900061222e   1c                   endif
055000061222      * - correttezza
055100061222     c                   clear                   WLBdat
055200061222     c                   eval      G08dat  = V1Cdad
055300061222     c                   call      'XSRDA8'
055400061222     c                   parm                    WLBdat
055500061222if  1c                   if        G08err  = *on
055600061222     c                   seton                                        285290
055700061222     c                   eval      V1Dmsg  = $Msg(4)
055800061222     c                   leavesr
055900061222e   1c                   endif
056000061222      *
056100061222     c                   eval      V1Cdad  = G08dat
056200061222     c                   eval      W1Cdad  = G08inv
056300061222      *
056400061222      * Controllo validità data
056500061222if  1c                   if        §PSCdad = *zeros
056600061222     c                   leavesr
056700061222e   1c                   endif
056800061222      * (DEVE essere più recente dell'ultima già inserita in tab.)
056900061222if  1c                   IF        W1Cdad <= §PSCdad
057000061222     c                   reset                   WLBdat
057100061222     c                   eval      G08inv  = §PSCdad
057200061222     c                   call      'XSRDA8'
057300061222     c                   parm                    WLBdat
057400061222     c                   seton                                        285290
057500061222     c                   eval      V1Dmsg  = %trimr($Msg(5))
057600061222     c                                     + ' '
057700061222     c                                     + %editc(G08dat:'Y')
057800061222     c                   leavesr
057900061222x   1c                   ELSE
058000061222      * (DEVE essere di max 40 gg. successiva a quella nell'ultimo
058100061222      *  record già inserito)
058200061222     c     *iso          move      §PSCdad       wData_1
058300061222     c     *iso          move      W1Cdad        wData_2
058400061222if  2c                   if        %diff( wData_2 : wData_1 : *days )
058500061222     c                                     > 40
058600061222     c                   reset                   WLBdat
058700061222     c                   eval      G08inv  = §PSCdad
058800061222     c                   call      'XSRDA8'
058900061222     c                   parm                    WLBdat
059000061222     c                   seton                                        285290
059100061222     c                   eval      V1Dmsg  = %trim($Msg(6))
059200061222     c                                     + ' '
059300061222     c                                     + %editc(G08dat:'Y')
059400061222     c                   leavesr
059500061222e   2c                   endif
059600061222e   1c                   ENDIF
059700061222      *
059800061222      * Controllo validità percentuale di scostamento
059900061222      * - obbligatoria
060000061222sel 1c                   select
060100061222w   1c                   when      V1Cper  = *zeros
060200061222     c                   seton                                        285390
060300061222     c                   eval      V1Dmsg  = $Msg(7)
060400061222     c                   leavesr
060500061222      * - errata (negativa)
060600061222w   1c                   when      V1Cper  < *zeros
060700061222     c                   seton                                        285390
060800061222     c                   eval      V1Dmsg  = $Msg(8)
060900061222     c                   leavesr
061000061222      * - oltre il 3% (+/-) differente rispetto alla precedente
061100061222w   1c                   when      %abs( V1Cper - §PSCper ) > 3
061200061222     c                   seton                                        285390
061300061222     c                   eval      V1Dmsg  = %trimr($Msg(9))
061400061222     c                                     + %editc(§PSCper:'O')
061500061222     c                   leavesr
061600061222e   1c                   endsl
061700061222      *
061800061222     c                   ENDSR
061900061222
062000061222      *---------------------------------------------------------------*
062100061222      * Gestione tasto funzionale F03 da videata D01                  *
062200061222      *---------------------------------------------------------------*
062300061222     c     F03D01        BEGSR
062400061222      *
062500061222      * Ritorno alla 1ª videata (subfile)
062600061222     c                   eval      $Video  = '1'
062700061222     c                   eval      *in90   = *on
062800061222      *
062900061222     c                   ENDSR
063000061222
063100061222      *---------------------------------------------------------------*
063200061222      * Gestione tasto funzionale F06 da videata D01                  *
063300061222      *---------------------------------------------------------------*
063400061222     c     F06D01        BEGSR
063500061222      *
063600061222     c                   movel     C_CodTab      TBEcod
063700061222     c                   movel(p)  V1Cke1        TBEke1
063800061222     c                   clear                   TBEke2
063900061222     c                   clear                   TBElin
064000061222     c                   if        TBXsif  <> *blanks
064100061222     c                   movel     KNSIF         TBEsif
064200061222     c                   else
064300061222     c                   clear                   TBEsif
064400061222     c                   endif
064500061222     c     K05TBE01      chain(n)  TNTBE000
064600061222      *
064700061222     c                   exsr      AggTBE
064800061222      *
064900061222     c                   ENDSR
065000061222
065100061222      *---------------------------------------------------------------*
065200061222      * Gestione tasto funzionale F12 da videata D01                  *
065300061222      *---------------------------------------------------------------*
065400061222     c     F12D01        BEGSR
065500061222      *
065600061222      * Ritorno alla videata precedente
065700061222     c                   eval      $Video  = '1'
065800061222      *
065900061222     c                   ENDSR
066000061222
066100061222      *---------------------------------------------------------------*
066200061222      * AGGTBE * Aggiornamento tabella (singolo record)               *
066300061222      *---------------------------------------------------------------*
066400061222     c     AggTBE        BEGSR
066500061222      *
066600061222if  1c                   IF        %found(TNTBE01L)
066700061222      *
066800061227      * Record già esistente
066900061227      * (aggiornamento non previsto!!!)
067000061227     c                   eval      $InzS01 = *on
067100061222     c                   eval      V1Dmsg  =  $Msg(10)
067200061222     c                   seton                                        28  90
067300061222     c                   leavesr
067400061222      *
067500061222x   1c                   ELSE
067600061222      * Immissione scaglione
067700061222     c                   exsr      RieTBE
067800061222     c                   eval      TBEftt  =  TBXftt
067900061222     c                   clear                   TBEflt
068000061222     c                   clear                   TBEdtr
068100061222     c*** 22 not tested: WRITE     TNTBE000                             22
068200061222     c                   WRITE     TNTBE000
068300061222      *
068400061222e   1c                   ENDIF
068500061222     c
068600061222     c                   ENDSR
068700061222
068800061222      *---------------------------------------------------------------*
068900061222      * RIETBE * Riempimento dati tabella per IMMISSIONE              *
069000061222      *---------------------------------------------------------------*
069100061222     c     RieTBE        BEGSR
069200061222      *
069300061222     c                   clear                   TNTBE000
069400061222      *
069500061222     c*** già così:      clear                   TBEatb
069600061222     c                   if        TBXsif  <> *blanks
069700061222     c                   movel     KNSIF         TBEsif
069800061222     c                   else
069900061222     c                   clear                   TBEsif
070000061222     c                   endif
070100061222     c                   clear                   TBElin
070200061222     c                   movel     TBXapl        TBEapl
070300061222     c                   movel     C_CodTab      TBEcod
070400061222     c                   movel(p)  V1Cke1        TBEke1
070500061222     c*** già così:      clear                   TBEke2
070600061222      *
070700061222     c                   exsr      RieDS
070800061222     c*** già fatto:     movel(p)  dPSC          TBEuni
070900061222      *
071000061222     c*** già così:      clear                   TBEftt
071100061222     c*** già così:      clear                   TBEflt
071200061222     c*** già così:      clear                   TBEftr
071300061222     c*** già così:      clear                   TBEdtr
071400061222      *
071500061222     c                   ENDSR
071600061222
071700061222      *---------------------------------------------------------------*
071800061222      * RIEDS  * Riempimento struttura dati                           *
071900061222      *---------------------------------------------------------------*
072000061222     c     RieDS         BEGSR
072100061222      *
072200061222     c                   clear                   dPSC
072300061222      *
072400061227     c                   eval      §PSCdad =  W1Cdad
072500061222     c                   eval      §PSCper =  V1Cper
072600061222      *
072700061222     c                   movel(p)  dPSC          TBEuni
072800061222      *
072900061222     c                   ENDSR
073000061222
073100061222** - $Msg -------------------------------------------------------------------*
073200061222Opzione errata                                                                  1
073300061222Numero progressivo errato o mancante                                            2
073400061222Data decorrenza obbligatoria                                                    3
073500061222Data decorrenza formalmente errata                                              4
073600061222Data decorrenza antecedente o uguale all'ultima inserita:                       5
073700061222Data decorrenza successiva di OLTRE 40 giorni dall'ultima inserita:             6
073800061222Percentuale di supplemento obbligatoria                                         7
073900061222Percentuale di supplemento errata                                               8
074000061229Percentuale differente di OLTRE 3 punti rispetto all'ultima inserita:           9
074100061222RECORD GIA' INSERITO                                                           10
