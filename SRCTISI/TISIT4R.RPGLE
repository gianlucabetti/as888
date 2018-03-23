000100060109      *===============================================================*
000200060109      *?TISIT4R * Manutenzione sinonimi destinatari particolari      ?*
000300060109      *===============================================================*
000400060109
000500060109     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060109
000700060109     fAZDST01L  if   e           k disk
000800060109     fAZDSTP1L  Uf   e           k disk    prefix(W_)
000900060109     fAZDSS01L  Uf A e           k disk
001000060109      *
001100060109     fTISIT4D   cf   e             workstn
001200060109
001300060109      *---------------------------------------------------------------*
001400060109
001500060109      *
001600060109      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001700060109      *
001800060109      * - Decodifiche funzioni
001900060109     d $Op1            s                   like(IT0op1)
002000060109     d                                     dim( 8)  ctdata  perrcd(1)
002100060109     d $Fun            s             20    dim( 8)  alt($Op1)
002200060109      * - Messaggi
002300060109     d $Msg            s             78    dim(21)  ctdata  perrcd(1)
002400060109      *
002500060109      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002600060109      *
002700060109     d KPJBA         e ds
002800060109     d Param           ds
002900060109     d  DEPop0                        3
003000060109     d  DEPpro                        1
003100060109     d  DEPlna                        3  0
003200060109      * - Parametri x Controllo profilo utenti
003300060109     d TIBS34ds      e ds
003400060109      * - Ds di riferimento al file esterno AZUTE00F
003500060109     d AZUTEds       e ds                  extname(AZUTE00F)
003600060109      * - Ds per dati organigramma
003700060109     d DDatiUte      e ds
003800060109      *
003900060109      * - DS di procedura per passaggio parametri
004000060109     d TISIT0ds      e ds
004100060109     d WrkT0ds       e ds                  extname(TISIT0ds)
004200060109     d                                     prefix(W_)
004300060109      *
004400060109      * - Controllo dati di "instradamento"
004500060109     d TISI95ds      e ds                  inz
004600060109      *
004700060109      * - Tab. "PR"  = Province
004800060109     d dsPR          e ds                  inz
004900060104      *
005000060104     d Status         sds           333
005100060104     d   SDSpgm          *proc
005200060109      *
005300060109      * - Controllo/Inversione date
005400060109     d WLBdat          ds                  inz
005500060109     d   G08dat                       8  0 inz
005600060109     d   G08inv                       8  0 inz
005700060109     d   G08err                       1    inz('3')
005800060109     d   G08tgi                       5  0 inz
005900060109      *
006000060109      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
006100060109      *
006200060109      * - Flags
006300060109     d $Fine           s              1    inz(*off)
006400060109     d $InzD01         s              1    inz(*on)
006500060109      * - Indici di schiera
006600060109     d xx              s              3  0 inz
006700060109      * - Campi di comodo
006800060109     d wDate           s              8  0 inz
006900060109      * - Campi chiave
007000060109     d K_tip_C         s                   like(DSTtip) inz('C')
007100060109     d K_tip_P         s                   like(DSTtip) inz('P')
007200060109     d KDScap          s                   like(DSScap) inz                     *CAP
007300060109     d KDSrag          s                   like(DSSrag) inz                     *RAGIONE SOCIALE
007400060109     d KDSsin          s                   like(DSSsin) inz                     *SINONIMO
007500060109     d KDSnaz          s                   like(DSSnaz) inz                     *NAZIONE
007600060109     d KDSprv          s                   like(DSSprv) inz                     *PROVINCIA
007700060109     d KDSloc          s                   like(DSSloc) inz                     *LOCALITA
007800060109     d KDSkwi          s                   like(DSSkwi) inz                     *KEYWORD INDIRIZZO
007900060109      *
008000060109      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
008100060109      *
008200060109      * - AZDSS01L
008300060109     c     K07DSS01      klist
008400060109     c                   kfld                    KDScap                         *CAP
008500060109     c                   kfld                    KDSrag                         *RAGIONE SOCIALE
008600060109     c                   kfld                    KDSsin                         *SINONIMO
008700060109     c                   kfld                    KDSnaz                         *NAZIONE
008800060109     c                   kfld                    KDSprv                         *PROVINCIA
008900060109     c                   kfld                    KDSloc                         *LOCALITA
009000060109     c                   kfld                    KDSkwi                         *KEYWORD INDIRIZZO
009100060109      * - AZDST01L
009200060109     c     K07DST01C     klist
009300060109     c                   kfld                    K_tip_C                        *TIPO
009400060109     c                   kfld                    KDScap                         *CAP
009500060109     c                   kfld                    KDSrag                         *RAGIONE SOCIALE
009600060109     c                   kfld                    KDSnaz                         *NAZIONE
009700060109     c                   kfld                    KDSprv                         *PROVINCIA
009800060109     c                   kfld                    KDSloc                         *LOCALITA
009900060109     c                   kfld                    KDSkwi                         *KEYWORD INDIRIZZO
010000060109      * - AZDSTP1L
010100060109     c     K08DSTP1P     klist
010200060109     c                   kfld                    K_tip_P
010300060109     c                   kfld                    IT0naz
010400060109     c                   kfld                    IT0prv
010500060109     c                   kfld                    IT0loc
010600060109     c                   kfld                    IT0cap
010700060109     c                   kfld                    IT0sin
010800060109     c                   kfld                    IT0ind
010900060109     c                   kfld                    DEPlna
011000060109
011100060109      *---------------------------------------------------------------*
011200060109      *  RIEPILOGO INDICATORI                                         *
011300060109      *---------------------------------------------------------------*
011400060109      * 10    - Comodo                                                *
011500060109      * 28    - Visualizza messaggio di errore                        *
011600060109      * 90    - Errore generico                                       *
011700060109      *---------------------------------------------------------------*
011800060109
011900060109     c     *Entry        plist
012000060109     c                   parm                    KPJBA
012100060109     c                   parm                    TISIT0ds
012200060109     c                   movel     KPJBU         Param
012300060109      *
012400060109      * Operazioni Iniziali
012500060109     c                   exsr      RoutInz
012600060109      *
012700060109      * Gestione Video
012800060109do  1c                   DOW       $Fine   = *off
012900060109     c                   exsr      GesD01
013000060109e   1c                   ENDDO
013100060109      *
013200060109      * Fine
013300060109     c                   clear                   TISI95ds
013400060109     c                   eval      I95tla  = 'C'
013500060109     c                   call      'TISI95R'
013600060109     c                   parm                    TISI95ds
013700060109      *
013800060109     c                   movel     *on           *inLR
013900060109      *
014000060109      *---------------------------------------------------------------*
014100060109      * Operazioni Iniziali                                           *
014200060109      *---------------------------------------------------------------*
014300060109     c     RoutInz       BEGSR
014400060109      *
014500060109      * Reperisco dati job
014600060109     c                   exsr      DatiJob
014700060109      *
014800060109     c                   movel     SDSpgm        V1Tpgm
014900060109      *
015000060109      * Reperimento data corrente
015100060109     c                   clear                   WLBdat
015200060109     c                   eval      G08dat  = *date
015300060109     c                   call      'XSRDA8'
015400060109     c                   parm                    WLBdat
015500060109     c                   eval      Wdate   = G08inv
015600060109      *
015700060109      * Imposta la funzione di ingresso come "sottotitolo"
015800060109     c                   eval      xx      = 1
015900060109     c                   eval      *in10   = *off
016000060109     c     IT0op1        lookup    $Op1(xx)                               10
016100060109     c                   if        *in10
016200060109     c                   movel     $Fun(xx)      V1Tfun
016300060109     c                   endif
016400060109      *
016500060109     c                   ENDSR
016600060109      *
016700060109      *---------------------------------------------------------------*
016800060109      * Reperimento Dati del job (Utente/Operativi)                   *
016900060109      *---------------------------------------------------------------*
017000060109     c     DatiJob       BEGSR
017100060109      *
017200060109     c     *dtaara       define    §azute        azuteds
017300060109     c     *dtaara       define    §datiute      ddatiute
017400060109      *
017500060109     c                   in(E)     *dtaara
017600060109     c                   IF        %ERROR or RSUT = *blanks
017700060109     c                   clear                   Tibs34Ds
017800060109     c                   call      'TIBS34R'
017900060109     c                   parm                    Tibs34Ds
018000060109     c                   in        *dtaara
018100060109     c                   ENDIF
018200060109      *
018300060109     c                   ENDSR
018400060109      *
018500060109      *---------------------------------------------------------------*
018600060109      * Gestione videata D01                                          *
018700060109      *---------------------------------------------------------------*
018800060109     c     GesD01        BEGSR
018900060109      *
019000060109      * Inizializzo la videata
019100060109if  1c                   if        $InzD01 = *on
019200060109     c                   exsr      InzD01
019300060109     c                   movel     *off          $InzD01
019400060109e   1c                   endif
019500060109      *
019600060109      * Scrivo la testata e la riga tasti funzionali abilitati
019700060109if  1c                   if        NOT *in90
019800060109     c                   write     SIT4T01
019900060109     c                   if        IT0op1  = 'O05'
020000060109     c                   write     SIT4P01
020100060109     c                   else
020200060109     c                   write     SIT4P02
020300060109     c                   endif
020400060109e   1c                   endif
020500060109      * Emetto la videata
020600060109     c                   exfmt     SIT4D01
020700060109     c                   setoff                                       28  90
020800060109     c                   clear                   V1Dmsg
020900060109      *
021000060109sel 1c                   select
021100060109      * F3=Fine
021200060109w   1c                   when      *inKC
021300060109     c                   exsr      F03D01
021400060109     c                   leavesr
021500060109      * F12=Ritorno
021600060109w   1c                   when      *inKL
021700060109     c                   exsr      F03D01
021800060109     c                   leavesr
021900060109      *
022000060109e   1c                   endsl
022100060109      *
022200060109      * Invio o F6
022300060109sel 1c                   select
022400060109      * Opzione "Annullamento" o "Ripristino"
022500060109w   1c                   when           IT0op1  = 'O04'
022600060109     c                             or   IT0op1  = 'O07'
022700060109      *
022800060109      * Opzione "Visualizzazione"
022900060109w   1c                   when           IT0op1  = 'O05'
023000060109     c***                          or   IT0op1  = 'P05'
023100060109     c                   eval      $Fine   = *on
023200060109      *
023300060109      * Altrimenti
023400060109      * => si controllano i dati di questa videata
023500060109x   1c                   other
023600060109     c                   exsr      CtrD01
023700060109if  2c                   if        *in90
023800060109     c                   leavesr
023900060109e   2c                   endif
024000060109      *
024100060109e   1c                   endsl
024200060109      *
024300060109      * F6=Conferma
024400060109     c                   if        *inKF
024500060109     c                   exsr      F06D01
024600060109     c                   if        *in90   = *off
024700060109     c                   eval      $Fine   = *on
024800060109     c                   endif
024900060109     c                   endif
025000060109      *
025100060109     c                   ENDSR
025200060109      *
025300060109      *---------------------------------------------------------------*
025400060109      * Inizializzazione videata D01                                  *
025500060109      *---------------------------------------------------------------*
025600060109     c     InzD01        BEGSR
025700060109      *
025800060109     c                   clear                   SIT4D01
025900060109      *
026000060109      * Se richiesto inserimento non si caricano dati
026100060109if  1c                   if             IT0op1  = 'O13'
026200060109     c                             or   IT0op1  = 'P13'
026300060109      *
026400060109     c                   eval      V1Crag = IT0rag
026500060109     c                   eval      V1Csin = IT0sin
026600060109     c                   eval      V1Cnaz = IT0naz
026700060109     c                   eval      V1Cprv = IT0prv
026800060109     c                   eval      V1Ccap = IT0cap
026900060109     c                   eval      V1Cloc = IT0loc
027000060109     c                   eval      V1Ckwi = IT0kwi
027100060109      *
027200060109x   1c                   else
027300060109      *
027400060109     c                   eval      KDSrag = IT0rag
027500060109     c                   eval      KDSsin = IT0sin
027600060109     c                   eval      KDSnaz = IT0naz
027700060109     c                   eval      KDSprv = IT0prv
027800060109     c                   eval      KDScap = IT0cap
027900060109     c                   eval      KDSloc = IT0loc
028000060109     c                   eval      KDSkwi = IT0kwi
028100060109      *
028200060109e   1c                   endif
028300060109      *
028400060109      * Verifica del tipo di operazioni da effettuare
028500060109sel 1c                   select
028600060109w   1c                   when      IT0op1 = 'O04'
028700060109     c                   eval      *in41  = *on
028800060109w   1c                   when      IT0op1 = 'O05'
028900060109     c                   eval      *in41  = *on
029000060109w   1c                   when      IT0op1 = 'O07'
029100060109     c                   eval      *in41  = *on
029200060109w   1c                   when      IT0op1 = 'O02'
029300060109     c                   eval      *in42  = *on
029400060109w   1c                   when      IT0op1 = 'O03'
029500060109     c                   eval      *in42  = *on
029600060109e   1c                   endsl
029700060109      *
029800060109      * Se richiesto inserimento non si caricano dati
029900060109if  1c                   if             IT0op1  = 'F06'
030000060109     c                             or   IT0op1  = 'O13'
030100060109     c                             or   IT0op1  = 'P13'
030200060109     c                   leavesr
030300060109e   1c                   endif
030400060109      *
030500060109      * Reperimento dati da aggiornare
030600060109     c                   exsr      Rep_AZDSS
030700060109      *
030800060109      * Impostazione dei dati in D01
030900060109     c                   exsr      RieD01
031000060109      *
031100060109     c                   ENDSR
031200060109      *
031300060109      *---------------------------------------------------------------*
031400060109      * Impostazione dati nella videata D01                           *
031500060109      *---------------------------------------------------------------*
031600060109     c     RieD01        BEGSR
031700060109      *
031800060109     c                   movel     DSSrag        V1Crag
031900060109     c                   movel     DSSsin        V1Csin
032000060109     c                   movel     DSSnaz        V1Cnaz
032100060109     c                   movel     DSSprv        V1Cprv
032200060109     c                   movel     DSScap        V1Ccap
032300060109     c                   movel     DSSloc        V1Cloc
032400060109     c                   movel     DSSkwi        V1Ckwi
032500060109     c                   movel     DSSute        V1Cute
032600060109     c                   z-add     DSSdiv        V1Cdiv
032700060109     c                   movel     DSSftr        V1Cftr
032800060109     c                   z-add     DSSdtr        V1Cdtr
032900060109      *
033000060109     c                   ENDSR
033100060109      *
033200060109      *---------------------------------------------------------------*
033300060109      * Gestione tasto funzionale F03 da videata D01                  *
033400060109      *---------------------------------------------------------------*
033500060109     c     F03D01        BEGSR
033600060109      *
033700060109      * Chiudo il programma
033800060109     c                   eval      $Fine   = *on
033900060109      *
034000060109     c                   ENDSR
034100060109      *
034200060109      *---------------------------------------------------------------*
034300060109      * Controllo dati immessi in videata D01                         *
034400060109      *---------------------------------------------------------------*
034500060109     c     CtrD01        BEGSR
034600060109      *
034700060109     c                   movea     *zeros        *in(51)
034800060109      *
034900060109      * Controllo dati di instradamento
035000060321     c***                exsr      Chk_INSTR
035100060321     c***                if        *in90
035200060321     c***                leavesr
035300060321     c***                endif
035400060109      *
035500060109      * Controllo che NON sia stato inserito il carattere jolly sulla
035600060109      *   ragione sociale
035700060109     c     '*'           scan      V1Crag
035800060109     c                   if        %found
035900060109     c                   seton                                        285190
036000060109     c                   movel     $Msg( 9)      V1Dmsg
036100060109     c                   leavesr
036200060109     c                   endif
036300060109      *
036400060109      * Controllo sinonimo diverso dalla ragione sociale
036500060109     c                   if         %trim(V1Csin) = %trim(V1Crag)
036600060109     c                   seton                                        28  90
036700060109     c                   seton                                        5152
036800060109     c                   movel     $Msg(10)      V1Dmsg
036900060109     c                   leavesr
037000060109     c                   endif
037100060109      *
037200060109      * Controllo che NON sia stato inserito il carattere jolly sul
037300060109      *   sinonimo come carattere singolo o in prima posizione
037400060109     c                   if            V1Csin                   <> *blanks
037500060109     c                             and %subst(%trim(V1Csin):1:1) = '*'
037600060109     c                   seton                                        285290
037700060109     c                   movel     $Msg(21)      V1Dmsg
037800060109     c                   leavesr
037900060109     c                   endif
038000060109      *
038100060109      * Campi obbligatori
038200060109sel 1c                   select
038300060109w   1c                   when      V1Crag  = *blanks
038400060109     c                   seton                                        285190
038500060109w   1c                   when      V1Csin  = *blanks
038600060109     c                   seton                                        285290
038700060109w   1c                   when      V1Cloc  = *blanks
038800060109     c                   seton                                        285490
038900060109w   1c                   when      V1Ckwi  = *blanks
039000060109     c                   seton                                        285390
039100060109w   1c                   when      V1Ccap  = *blanks
039200060109     c                   seton                                        285590
039300060109w   1c                   when      V1Cprv  = *blanks
039400060109     c                   seton                                        285690
039500060109e   1c                   endsl
039600060109if  1c                   if        *in28
039700060109     c                   movel     $Msg(12)      V1Dmsg
039800060109     c                   leavesr
039900060109e   1c                   endif
040000060109      *
040100060109      * Controllo tramite TISIT5R =>
040200060109      *   inserimento possibile SOLO se restituito "errore"
040300060109if  1c                   IF             IT0op1   =  'F06'
040400060109     c                             or   IT0op1   =  'P13'
040500060109     c                             or   IT0op1   =  'O02'
040600060109     c                             or   IT0op1   =  'O03'
040700060109     c                             or   IT0op1   =  'O07'
040800060109     c                             or   IT0op1   =  'O13'
040900060109      *
041000060109      * IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA DI CONTROLLO/REPERIMENTO
041100060109     c                   clear                   WrkT0ds
041200060109     c                   eval      W_IT0tla = 'P'
041300060109     c                   eval      W_IT0rag = V1Csin
041400060109     c                   eval      W_IT0naz = V1Cnaz
041500060109     c                   eval      W_IT0prv = V1Cprv
041600060109     c                   eval      W_IT0cap = V1Ccap
041700060109     c                   eval      W_IT0loc = V1Cloc
041800060109     c                   eval      W_IT0ind = V1Ckwi
041900060109     c                   call      'TISIT5R'
042000060109     c                   parm                    WrkT0ds
042100060109      *
042200060109      * Se trattasi di inserimento sinonimo da proposte e se validato
042300060109      *   il sinonimo dal driver => reimposto il CAP del sinonimo a
042400060109      *   video con quello dell'anagrafica destinatario relativa
042500060109if  2c                   if             IT0op1   =  'P13'
042600060109     c                             and (W_OT0err <> '1'
042700060109     c                              or  W_OT0liv =  '9')
042800060109     c                   eval      V1Ccap = W_OT0cap
042900060109     c***                seton                                        28  90
043000060109     c***                movel     $Msg(16)      V1Dmsg
043100060109e   2c                   endif
043200060109      *
043300060109sel 2c                   select
043400060109w   2c                   when           W_OT0liv =  '1'
043500060109     c                             or   W_OT0liv =  '3'
043600060109     c                   seton                                        28  90
043700060109     c                   movel     $Msg(19)      V1Dmsg
043800060109w   2c                   when           W_OT0liv =  '2'
043900060109     c                             or   W_OT0liv =  '4'
044000060109     c                   seton                                        28  90
044100060109     c                   movel     $Msg(20)      V1Dmsg
044200060109w   2c                   when           W_OT0liv <> '9'
044300060109     c                   seton                                        28  90
044400060109     c                   movel     $Msg(18)      V1Dmsg
044500060109e   2c                   endsl
044600060109      *
044700060109e   1c                   ENDIF
044800060109      *
044900060109      * A questo punto:
045000060109      * => Reimposto la chiave coi campi del video in base al tipo di
045100060109      *    lancio
045200060109if  1c                   if             IT0op1  =  'O13'
045300060109     c                             or   IT0op1  =  'P13'
045400060109     c                   eval      KDSrag = V1Crag
045500060109     c                   eval      KDSsin = V1Csin
045600060109     c                   eval      KDSnaz = V1Cnaz
045700060109     c                   eval      KDSprv = V1Cprv
045800060109     c                   eval      KDScap = V1Ccap
045900060109     c                   eval      KDSloc = V1Cloc
046000060109     c                   eval      KDSkwi = V1Ckwi
046100060109x   1c                   else
046200060109     c                   eval      KDSrag = IT0rag
046300060109     c                   eval      KDSsin = IT0sin
046400060109     c                   eval      KDSnaz = IT0naz
046500060109     c                   eval      KDSprv = IT0prv
046600060109     c                   eval      KDScap = IT0cap
046700060109     c                   eval      KDSloc = IT0loc
046800060109     c                   eval      KDSkwi = IT0kwi
046900060109e   1c                   endif
047000060109      *
047100060109      * Controllo che per il sinonimo esista veramente il record
047200060109      *   "principale" relativo
047300060109     c     K07DST01C     chain     AZDST000
047400060109     c                   if        NOT %found(AZDST01L)
047500060109     c                   seton                                        28  90
047600060109     c                   seton                                        515253
047700060109     c                   seton                                        545556
047800060109     c                   seton                                        57
047900060109     c                   movel     $Msg(13)      V1Dmsg
048000060109     c                   leavesr
048100060109     c                   endif
048200060109      *
048300060109     c                   ENDSR
048400060109      *
048500060109      *---------------------------------------------------------------*
048600060109      * Aggiornamento record                                          *
048700060109      *---------------------------------------------------------------*
048800060109     c     F06D01        BEGSR
048900060109      *
049000060109      * Si lancia la subroutine di conferma interessata
049100060109      *   in base alla provenienza
049200060109sel 1c                   select
049300060109w   1c                   when           IT0op1 = 'F06'
049400060109     c                             or   IT0op1 = 'O13'
049500060109     c                             or   IT0op1 = 'P13'
049600060109     c                   exsr      Wrt_AZDSS
049700060109w   1c                   when      IT0op1 = 'O02'
049800060109     c                   exsr      Upd_AZDSS
049900060109w   1c                   when      IT0op1 = 'O03'
050000060109     c                   exsr      Cpy_AZDSS
050100060109w   1c                   when      IT0op1 = 'O04'
050200060109     c                   exsr      Dlt_AZDSS
050300060109w   1c                   when      IT0op1 = 'O07'
050400060109     c                   exsr      Rip_AZDSS
050500060109     c                   endsl
050600060109      *
050700060109     c                   ENDSR
050800060109      *
050900060109      *---------------------------------------------------------------*
051000060109      * INSERIMENTO RECORD                                            *
051100060109      *---------------------------------------------------------------*
051200060109     c     Wrt_AZDSS     BEGSR
051300060109      *
051400060109     c                   exsr      Rep_AZDSS
051500060109     c                   if        *in28
051600060109     c                   leavesr
051700060109     c                   endif
051800060109      *
051900060109      * Impostazione dei campi nel record da scrivere
052000060109     c                   clear                   AZDSS000
052100060109     c                   exsr      sr_RieFld
052200060109     c                   eval      DSSute  = KNMUS
052300060109     c                   eval      DSSdiv  = wDate                              *DATA INS/VAR
052400060109     c***                clear                   DSSdtr                         *DATA TRASMISSIONE
052500060109     c***                clear                   DSSftr                         *FLAG TRASMISSIONE
052600060109     c***                clear                   DSSatb                         *FLAG ANNULLAMENTO
052700060109      *
052800060109     c                   WRITE     AZDSS000
052900060109      *
053000060109      * Se proveniente da proposte in automatico
053100060109      * => Cancella la proposta ed esce
053200060109if  1c                   IF        IT0op1 = 'P13'
053300060109     c     K08DSTP1P     chain     AZDSTP00
053400060109     c                   if        %found(AZDSTP1L)
053500060109     c                   DELETE    AZDSTP00
053600060109     c                   endif
053700060109x   1c                   ELSE
053800060109      * Altrimenti
053900060109      * => dopo l'inserimento lascia impostata a video SOLO la chiave
054000060109     c                   clear                   V1Csin
054100060109e   1c                   ENDIF
054200060109      *
054300060109     c                   movel     $Msg(2)       OT0msg                         *MESSAGGIO
054400060109     c                   eval      $Fine   = *on
054500060109      *
054600060109     c                   ENDSR
054700060109      *
054800060109      *---------------------------------------------------------------*
054900060109      * MODIFICA RECORD                                               *
055000060109      *---------------------------------------------------------------*
055100060109     c     Upd_AZDSS     BEGSR
055200060109      *
055300060109     c                   exsr      Rep_AZDSS
055400060109     c                   if        *in28
055500060109     c                   leavesr
055600060109     c                   endif
055700060109      *
055800060109      * Impostazione dei campi nel record da scrivere
055900060109     c                   exsr      sr_RieFld
056000060109     c                   clear                   DSSftr                         *FLAG TRASMISSIONE
056100060109     c                   eval      DSSute  = KNMUS                              *UTENTE INS/VAR
056200060109     c                   eval      DSSdiv  = wDate                              *DATA INS/VAR
056300060109      *
056400060109     c                   UPDATE    AZDSS000
056500060109      *
056600060109     c                   movel     $Msg(3)       OT0msg                         *MESSAGGIO
056700060109     c                   eval      $Fine   = *on
056800060109      *
056900060109     c                   ENDSR
057000060109      *
057100060109      *---------------------------------------------------------------*
057200060109      * COPIA RECORD                                                  *
057300060109      *---------------------------------------------------------------*
057400060109     c     Cpy_AZDSS     BEGSR
057500060109      *
057600060109     c                   exsr      Rep_AZDSS
057700060109     c                   if        *in28
057800060109     c                   leavesr
057900060109     c                   endif
058000060109      *
058100060109      * Impostazione dei campi nel record da scrivere
058200060109     c                   exsr      sr_RieFld
058300060109     c                   clear                   DSSftr                         *FLAG TRASMISSIONE
058400060109     c                   eval      DSSute  = KNMUS                              *UTENTE INS/VAR
058500060109     c                   eval      DSSdiv  = wDate                              *DATA INS/VAR
058600060109      *
058700060109     c                   WRITE     AZDSS000
058800060109      *
058900060109     c                   movel     $Msg(15)      OT0msg                         *MESSAGGIO
059000060109     c                   eval      $Fine   = *on
059100060109      *
059200060109     c                   ENDSR
059300060109      *
059400060109      *---------------------------------------------------------------*
059500060109      * CANCELLAZIONE RECORD                                          *
059600060109      *---------------------------------------------------------------*
059700060109     c     Dlt_AZDSS     BEGSR
059800060109      *
059900060109     c                   exsr      Rep_AZDSS
060000060109     c                   if        *in28
060100060109     c                   leavesr
060200060109     c                   endif
060300060109      *
060400060109      * Essendo ormai su "AS UNICO" ed essendo il file in UNITRAGRU
060500060109     c                   DELETE    AZDSS000
060600060109      *
060700060109     c                   movel     $Msg(4)       OT0msg                         *MESSAGGIO
060800060109     c                   eval      $Fine   = *on
060900060109      *
061000060109     c                   ENDSR
061100060109      *
061200060109      *---------------------------------------------------------------*
061300060109      * RIPRISTINO RECORD                                             *
061400060109      *---------------------------------------------------------------*
061500060109     c     Rip_AZDSS     BEGSR
061600060109      *
061700060109     c                   exsr      Rep_AZDSS
061800060109     c                   if        *in28
061900060109     c                   leavesr
062000060109     c                   endif
062100060109      *
062200060109     c                   clear                   DSSATB
062300060109     c                   clear                   DSSFTR                         *FLAG TRASMISSIONE
062400060109     c                   eval      DSSute  = KNMUS                              *UTENTE INS/VAR
062500060109     c                   eval      DSSdiv  = wDate                              *DATA INS/VAR
062600060109      *
062700060109     c                   UPDATE    AZDSS000
062800060109      *
062900060109     c                   movel     $Msg(5)       OT0msg                         *MESSAGGIO
063000060109     c                   eval      $Fine   = *on
063100060109      *
063200060109     c                   ENDSR
063300060109      *
063400060109      *---------------------------------------------------------------*
063500060109      * Reperimento record                                            *
063600060109      *---------------------------------------------------------------*
063700060109     c     Rep_AZDSS     BEGSR
063800060109      *
063900060109     c                   if             IT0op1 = 'O03'
064000060109     c                              or  IT0op1 = 'O05'
064100060109     c     K07DSS01      chain(n)  AZDSS000
064200060109     c                   else
064300060109     c     K07DSS01      chain     AZDSS000
064400060109     c                   endif
064500060109      *
064600060109     c                   if             %found(AZDSS01L)
064700060109     c                             and (IT0op1 = 'F06'
064800060109     c                              or  IT0op1 = 'P13'
064900060109     c                              or  IT0op1 = 'O13')
065000060109     c                   seton                                        28  90
065100060109     c                   eval      OT0msg  = $Msg( 7)
065200060109     c                   eval      $Fine   = *on
065300060109     c                   endif
065400060109      *
065500060109     c                   ENDSR
065600060109      *
065700060109      *---------------------------------------------------------------*
065800060109      * IMPOSTAZIONE (PARZIALE) DEI CAMPI DEL RECORD DEL FILE         *
065900060109      *---------------------------------------------------------------*
066000060109     c     sr_RieFld     BEGSR
066100060109      *
066200060109     c                   eval      DSSrag = V1Crag
066300060109     c                   eval      DSSsin = V1Csin
066400060109     c                   eval      DSSnaz = V1Cnaz
066500060109     c                   eval      DSSprv = V1Cprv
066600060109     c                   eval      DSScap = V1Ccap
066700060109     c                   eval      DSSloc = V1Cloc
066800060109     c                   eval      DSSkwi = V1Ckwi
066900060109      *
067000060109     c                   ENDSR
067100060109      *
067200060109      *---------------------------------------------------------------*
067300060109      * Controllo di tutti i dati di "INSTRADAMENTO"                  *
067400060109      *---------------------------------------------------------------*
067500060109     c     Chk_INSTR     BEGSR
067600060109      *
067700060109     c                   clear                   TISI95ds                       *azzera i parametri
067800060109     c***                clear                   I95tla                         *tipo lancio
067900060109     c                   movel     '7'           I95tcn                         *tipo controllo
068000060109     c                   movel     V1Cnaz        I95nar                         *nazione
068100060109     c                   movel     V1Ccap        I95cap                         *cap
068200060109     c                   movel     V1Cloc        I95loc                         *località
068300060109     c                   movel     V1Cprv        I95prv                         *provincia
068400060109     c                   z-add     Wdate         I95dat                         *data di riferimento
068500060109      *
068600060109     c                   call      'TISI95R'
068700060109     c                   parm                    TISI95DS
068800060109      *
068900060109      * Se livello dati 3 o 4: "normalizzo" i dati di "instradamento"
069000060109      *   inseriti a video
069100060109if  1c                   IF            (O95lid  = '3'
069200060109     c                             or   O95lid  = '4')
069300060109     c***                          and  O95flg <> 'S'
069400060109      *
069500060109if  2c                   if             V1Cloc <> O95loc
069600060109     c                             or   V1Ccap <> O95cap
069700060109     c                             or   V1Cprv <> O95prv
069800060109     c                             or   V1Cnaz <> O95nar
069900060109     c                   eval      V1Ccap  = O95cap
070000060109     c                   eval      V1Cloc  = O95loc
070100060109     c                   eval      V1Cprv  = O95prv
070200060109     c                   eval      V1Cnaz  = O95nar
070300060109     c                   seton                                        28  90
070400060109     c                   seton                                        545556
070500060109     c                   seton                                        57
070600060109     c                   movel     $Msg(16)      V1Dmsg
070700060109e   2c                   endif
070800060109      *
070900060109x   1c                   ELSE
071000060109      *
071100060321     c                   seton                                        28  90
071200060109     c                   seton                                        545556
071300060109     c                   seton                                        57
071400060321     c                   movel     $Msg(17)      V1Dmsg
071500060109      *
071600060109e   1c                   ENDIF
071700060109      *
071800060109     c                   ENDSR
071900060109
072000060109      *---------------------------------------------------------------*
072100060109
072200060109** - $Op1 / $Fun
072300060109O02    MODIFICA                                                      1
072400060109O03     COPIA                                                        2
072500060109O04  ANNULLAMENTO                                                    3
072600060109O05VISUALIZZAZIONE                                                   4
072700060109O07   RIPRISTINO                                                     5
072800060109O13  INSERIMENTO                                                     6
072900060109F06  INSERIMENTO                                                     7
073000060109P13 CONFERMA PROP.                                                   8
073100020416** MSG - MESSAGGI
073200060109TISIT4R-                                                                       1
073300020415TISIT4R- Inserimento effettuato                                                2
073400020415TISIT4R- Modifica effettuata                                                   3
073500020415TISIT4R- Annullamento effettuato                                               4
073600020415TISIT4R- Ripristino effettuato                                                 5
073700060109TISIT4R- Richiesta funzione per record inesistente                             6
073800060109TISIT4R- Impossibile inserire, record già esistente                            7
073900060109TISIT4R-                                                                       8
074000060109TISIT4R- Impossibile indicare '*' nella Ragione Sociale, correggere            9
074100060109TISIT4R- Impossibile indicare il sinonimo uguale alla Ragione Sociale          10
074200060109TISIT4R-                                                                       11
074300020415TISIT4R- Tutti i campi sono obbligatori                                        12
074400060109TISIT4R- Il sinonimo non si riferisce a nessun destinatario particolare        13
074500060317TISIT2R- NAZ/PRV/CAP/LOC incongruenti, F17 per forzare.                        14
074600020415TISIT4R- Copia effettuata                                                      15
074700020415TISIT4R- ATTENZIONE... dati a video sostituiti !                               16
074800060109TISIT4R- NAZ/PRV/CAP/LOC incongruenti, verificare e correggere                 17
074900060109TISIT4R- Errore, per il sinonimo non esiste nessun possibile destinatario      18
075000060109TISIT4R- Errore, il sinonimo già presente come anagrafica dest. particolari    19
075100060109TISIT4R- Errore, il sinonimo già presente come sinonimo dest. particolari      20
075200060109TISIT4R- La Ragione Sociale non può essere o iniziare con '*'                  21
