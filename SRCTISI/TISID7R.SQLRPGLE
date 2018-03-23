000100061109      *PARMS CLOSQLCSR(*ENDMOD)  COMMIT(*NONE)  DBGVIEW(*SOURCE)
000200061109
000300060927      *---------------------------------------------------------------*
000400060927      *?CAPPARIO DPD - GESTIONE SINGOLA VERSIONE                     ?*
000500060927      *---------------------------------------------------------------*
000600060927      *?Parametri RICHIESTI in ds TISI00DS:?                          *
000700060927      * D00OP1: "F06" = Immissione                                    *
000800060927      *         "O02" = Modifica                                      *
000900060927      *         "O03" = Copia           - NON gestito                 *
001000061109      *         "O04" = Cancellazione   - era Annullamento            *
001100060927      *         "O05" = Visualizzazione                               *
001200060927      *         "O06" = Stampa          - NON gestito                 *
001300060927      *         "O07" = Ripristino                                    *
001400060927      * D00VER: Versione cappario da elaborare                        *
001500060927      *?Parametri DI OUTPUT in ds TISI00DS:?                          *
001600060927      * D00F03: Uscita via F3=Fine                                    *
001700060927      * D00F12: Uscita via F12=Ritorno                                *
001800060927      * D00ERR: non gestito                                           *
001900060927      * D00MSG: non gestito                                           *
002000060927      *---------------------------------------------------------------*
002100060927
002200061109     h debug decedit('0,') datedit(*dmy/) option(*nodebugio)
002300060927
002400060927      *---------------------------------------------------------------*
002500060927
002600060927      * Versioni
002700170628     fDPCVE12I  if   e           k disk    rename(DPCVE100:DPCVE102)
002800060927     f                                     prefix(CV2:3)
002900170628     fDPCVE11I  Uf A e           k disk
003000060927      *
003100170628     fTISID7D   cf   e             workstn
003200060927
003300060927      *---------------------------------------------------------------*
003400060927
003500060927      *
003600060927      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
003700060927      *
003800061109     d C_Delete        c                   const('Cancellazione in cor-
003900061109     d                                     so... Attendere prego.')
004000060927      *
004100060927      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
004200060927      *
004300060927      * - Testi di decodifica funzione richiesta
004400060927     d $Fun            s             17    dim( 6) ctdata  perrcd(1)
004500060927      * - Messaggi di errore
004600060927     d $Msg            s             78    dim(12) ctdata  perrcd(1)
004700060927      *
004800060927      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
004900060927      *
005000060927      * - Architettura
005100060927     d KPJBA         e ds
005200060927      *
005300060927      * - Parametri ricevuti
005400060927     d TISI00ds      e ds
005500170628     d ds_DPCVE      e ds                  extname(DPCVE10F)
005600060927      *
005700060927      * - Reperimento dati utente
005800060927     d TIBS34ds      e ds                                                       *Profili utente
005900060927     d dDatiUte      e ds                                                       *Dati utente
006000060927     d AZUTEds       e ds                  extname(AZUTE00F)                    *Utenti
006100060927      *
006200060927     d Status         sds           333
006300060927     d   SDSpgm          *proc
006400060927     d   SDSprm          *parms
006500060927      *
006600060927      * - Controllo/Inversione date ("XSRDA8")
006700060927     d WLBdat          ds                  inz
006800060927     d  G08dat                 1      8  0 inz
006900060927     d  G08inv                 9     16  0 inz
007000060927     d  G08err                17     17    inz('3')
007100060927     d  G08tgi                18     22  0 inz
007200060927      *
007300060927      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
007400060927      *
007500060927      * - Flags
007600060927     d $InzD01         s              1    inz(*on)
007700060927     d $Fine           s              1    inz(*off)
007800060927     d $OK             s              1    inz(*off)
007900060927      *
008000060927     d wDate           s              8  0 inz
008100060927     d wDate_ISO       s               d   datfmt(*ISO) inz
008200060927      *
008300060927     d W1Cdde          s                   like(CVEdde) inz
008400060927     d W1Cdsc          s                   like(CVEdsc) inz
008500170628     d W1Cdim          s                   like(CVEdim) inz
008600060927      *
008700060927      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
008800060927      *
008900060927
009000060927      *---------------------------------------------------------------*
009100060927      *  RIEPILOGO INDICATORI                                         *
009200060927      *---------------------------------------------------------------*
009300060927      * 10    - Comodo                                                *
009400060927      * 28    - Emissione del messaggio di errore a video             *
009500060927      * 90    - Generico di errore                                    *
009600060927      *---------------------------------------------------------------*
009700060927
009800060927     c     *Entry        plist
009900060927     c                   parm                    KPJBA
010000060927     c                   parm                    ds_DPCVE
010100060927     c                   movel     KPJBU         TISI00ds
010200060927      *
010300060927      * Operazioni Iniziali
010400060927     c                   exsr      RoutInz
010500060927      *
010600060927      * Gestione Video
010700060927do  1c                   DOW       $Fine   = *off
010800060927     c                   exsr      GesD01
010900060927e   1c                   ENDDO
011000060927      *
011100060927      * Operazioni Finali
011200060927     c                   movel     TISI00ds      KPJBU
011300060927     c                   eval      *inLR   = *on
011400060927
011500060927      *---------------------------------------------------------------*
011600060927      *?Operazioni Iniziali                                          ?*
011700060927      *---------------------------------------------------------------*
011800060927     c     RoutInz       BEGSR
011900060927      *
012000061109     c/exec sql
012100061109     c+     SET OPTION Commit    = *NONE,
012200061109     c+                CloSQLCsr = *ENDMOD,
012300061109     c+                DatFmt    = *ISO,
012400061109     c+                TimFmt    = *ISO
012500061109     c/end-exec
012600061109      *
012700060927      * Reperimento dati job
012800060927     c                   exsr      DatiJob
012900060927      *
013000060927      * Impostazione nome programma a video
013100060927     c                   movel     SDSpgm        V1Tpgm
013200060927      *
013300060927      * Reperimento data corrente in formato gg/mm/aaaa
013400060927     c                   move      *date         wDate_ISO
013500060927      * Conversione data corrente in formato aaaa/mm/gg
013600060927     c                   move      wDate_ISO     wDate
013700060927      *
013800060927      * Inizializzazione parametri di output
013900060927     c                   eval      D00f03  = *off
014000060927     c                   eval      D00f12  = *off
014100060927     c                   eval      D00err  = *off
014200060927     c                   eval      D00msg  = *blanks
014300060927     c                   if        SDSprm  > 1
014400060927     c                   clear                   ds_DPCVE
014500060927     c                   endif
014600060927      *
014700060927      * Controllo parametri ricevti
014800060927      * => eseguiti nella subr. INZD01
014900060927      *
015000060927     c                   ENDSR
015100060927
015200060927      *---------------------------------------------------------------*
015300060927      *?Reperimento Dati del job (Utente/Operativi)                  ?*
015400060927      *---------------------------------------------------------------*
015500060927     c     DatiJob       BEGSR
015600060927      *
015700060927     c     *dtaara       define    §azute        azuteds
015800060927     c     *dtaara       define    §datiute      ddatiute
015900060927      *
016000060927     c                   in(E)     *dtaara
016100060927     c                   IF        %ERROR or RSUT = *blanks
016200060927     c                   clear                   Tibs34Ds
016300060927     c                   call      'TIBS34R'
016400060927     c                   parm                    Tibs34Ds
016500060927     c                   in        *dtaara
016600060927     c                   ENDIF
016700060927      *
016800060927     c                   ENDSR
016900060927
017000060927      *---------------------------------------------------------------*
017100060927      *?Gestione videata D01                                         ?*
017200060927      *---------------------------------------------------------------*
017300060927     c     GesD01        BEGSR
017400060927      *
017500060927      * Inizializzazione videata
017600060927if  1c                   if        $InzD01 = *on
017700060927     c                   exsr      InzD01
017800060927     c                   eval      $InzD01 = (*in90 = *on)
017900060927e   1c                   endif
018000060927      *
018100060927      * Scrittura di testata e di riga tasti funzionali abilitati
018200060927if  1c                   if        NOT *in90
018300170628     c                   write     SID7T01
018400170628     c                   write     SID7Z01
018500060927e   1c                   endif
018600060927      *
018700060927      * Emissione videata
018800061109if  1c                   if        NOT *in90
018900061109     c                             and  D00op1 = 'O04'
019000170628     c                   write     SID7D02
019100061109e   1c                   endif
019200170628     c                   exfmt     SID7D01
019300060927     c                   setoff                                       28  90
019400060927     c                   clear                   V1Dmsg
019500060927      *
019600060927sel 1c                   SELECT
019700060927      * F3=Fine
019800060927w   1c                   WHEN      *inKC
019900060927     c                   exsr      F03D01
020000060927      * F12=Ritorno
020100060927w   1c                   WHEN      *inKL
020200060927     c                   exsr      F12D01
020300060927      * Visualizzazione (nessun controllo)
020400060927w   1c                   WHEN      D00op1  = 'O05'
020500060927      * Annullamento    (nessun controllo)
020600060927w   1c                   WHEN      D00op1  = 'O04'
020700060927if  2c                   if        NOT *in90   and   *inKF
020800061109     c                   move      C_Delete      D2Dtxt
020900170628     c                   write     SID7D02
021000060927     c                   exsr      F06D01
021100061109x   2c                   else
021200061109      *   (serve solo a velocizzare la riemissione delle videate)
021300061109     c                   eval      *in90   = *on
021400060927e   2c                   endif
021500060927      * Inserimento/Modifica/Ripristino
021600060927x   1c                   OTHER
021700060927     c                   exsr      CtrD01
021800060927      * - F6=Conferma
021900060927if  2c                   if        NOT *in90   and   *inKF
022000060927     c                   exsr      F06D01
022100060927e   2c                   endif
022200060927e   1c                   ENDSL
022300060927      *
022400060927     c                   ENDSR
022500060927
022600060927      *---------------------------------------------------------------*
022700060927      *?Inizializzazione videata D01                                 ?*
022800060927      *---------------------------------------------------------------*
022900060927     c     InzD01        BEGSR
023000060927      *
023100170628     c                   clear                   SID7D01
023200060927      *
023300060927      * Abilitazione tasto funzionale F6=Conferma
023400060927     c                   eval      *in06   = (D00op1 = 'F06'  or
023500060927     c                                        D00op1 = 'O02'  or
023600060927     c                                        D00op1 = 'O03'  or
023700060927     c                                        D00op1 = 'O04'  or
023800060927     c                                        D00op1 = 'O07')
023900060927      * Protezione campo chiave a video
024000060927     c                   eval      *in41   = (D00op1 = 'O02')
024100060927      * Protezione tutti i campi a video
024200060927     c                   eval      *in42   = (D00op1 = 'O04'  or
024300060927     c                                        D00op1 = 'O05'  or
024400060927     c                                        D00op1 = 'O07')
024500060927      *
024600060927      * Controllo parametri ricevti
024700060927sel 1c                   SELECT
024800060927      * - Se richiesto inserimento:
024900060927      *   il controllo di effettiva inesistenza verrà eseguito nella
025000060927      *   subr. CTRD01.
025100060927w   1c                   WHEN      D00op1  = 'F06'
025200060927     c                   eval      V1Ttxt  = $Fun(1)
025300060927     c                   leavesr
025400061109      * - Se richiesta modifica/cancellazione/annullamento/ripristino:
025500060927      *   il record da aggiornare va allocato
025600060927w   1c                   WHEN           D00op1 = 'O02'
025700060927     c                             or   D00op1 = 'O04'
025800060927     c                             or   D00op1 = 'O07'
025900060927sel 2c                   select
026000060927w   2c                   when      D00op1  = 'O02'
026100060927     c                   eval      V1Ttxt  = $Fun(2)
026200060927w   2c                   when      D00op1  = 'O04'
026300060927     c                   eval      V1Ttxt  = $Fun(4)
026400060927w   2c                   when      D00op1  = 'O07'
026500060927     c                   eval      V1Ttxt  = $Fun(6)
026600060927e   2c                   endsl
026700170628     c     D00ver        chain     DPCVE100
026800170628if  2c                   if        NOT  %found(DPCVE11I)
026900060927     c                   seton                                        28  90
027000060927     c                   eval      V1Dmsg  = %trim($Msg(2))
027100060927     c                   eval      V1Cver  = D00ver
027200060927     c                   leavesr
027300060927e   2c                   endif
027400060927      * - Se richiesta visualizzazione/copia
027500060927      *   il record da visualizzare NON va allocato
027600060927w   1c                   WHEN           D00op1 = 'O03'
027700060927     c                             or   D00op1 = 'O05'
027800060927sel 2c                   select
027900060927w   2c                   when      D00op1  = 'O03'
028000060927     c                   eval      V1Ttxt  = $Fun(3)
028100060927w   2c                   when      D00op1  = 'O05'
028200060927     c                   eval      V1Ttxt  = $Fun(5)
028300060927e   2c                   endsl
028400170628     c     D00ver        chain(n)  DPCVE100
028500170628if  2c                   if        NOT  %found(DPCVE11I)
028600060927     c                   seton                                        28  90
028700060927     c                   eval      V1Dmsg  = %trim($Msg(2))
028800060927     c                   eval      V1Cver  = D00ver
028900060927     c                   leavesr
029000060927e   2c                   endif
029100060927      * - Altrimenti non risulta una richiesta valida
029200060927x   1c                   OTHER
029300060927     c                   eval      *in41   = *on
029400060927     c                   seton                                        28  90
029500060927     c                   eval      V1Dmsg  = %trim($Msg(1))
029600060927     c                                     + ' ('
029700060927     c                                     + D00op1
029800060927     c                                     + ')'
029900060927     c                   eval      V1Cver  = D00ver
030000060927     c                   leavesr
030100060927e   1c                   ENDSL
030200060927      *
030300060927      * Impostazione campi chiave a video
030400060927if  1c                   if        D00op1 <> 'O03'
030500060927     c                   eval      V1Cver  = CVEver
030600060927e   3c                   endif
030700060927      *
030800060927      * Impostazione altri campi a video
030900060927     c***                eval      V1Cdde  = CVEdde
031000060927     c                   reset                   WLBdat
031100060927     c                   eval      G08inv  = CVEdde
031200060927     c                   call      'XSRDA8'
031300060927     c                   parm                    WLBdat
031400060927if  1c                   if        G08err  = *off
031500060927     c                   eval      V1Cdde  = G08dat
031600060927e   1c                   endif
031700060927     c***                eval      V1Cdsc  = CVEdsc
031800060927     c                   reset                   WLBdat
031900060927     c                   eval      G08inv  = CVEdsc
032000060927     c                   call      'XSRDA8'
032100060927     c                   parm                    WLBdat
032200060927if  1c                   if        G08err  = *off
032300060927     c                   eval      V1Cdsc  = G08dat
032400060927e   1c                   endif
032500060927     c                   eval      V1Cverd = CVEverd
032600170628     c                   eval      V1CBCID = CVEBCID
032700170628     c                   reset                   WLBdat
032800170628     c                   eval      G08inv  = CVEdim
032900170628     c                   call      'XSRDA8'
033000170628     c                   parm                    WLBdat
033100170628if  1c                   if        G08err  = *off
033200170628     c                   eval      V1Cdim  = G08dat
033300170628e   1c                   endif
033400060927      *
033500060927     c                   ENDSR
033600060927
033700060927      *---------------------------------------------------------------*
033800060927      *?Gestione tasto funzionale F3 da videata D01                  ?*
033900060927      *?F3=Fine                                                      ?*
034000060927      *---------------------------------------------------------------*
034100060927     c     F03D01        BEGSR
034200060927      *
034300060927      * Disallocazione del record
034400060927     c                   if             D00op1 = 'O02'
034500060927     c                             or   D00op1 = 'O04'
034600060927     c                             or   D00op1 = 'O07'
034700170628     c                   UNLOCK    DPCVE11I
034800060927     c                   endif
034900060927      * Impostazione flag di ritorno
035000060927     c                   eval      D00f03  = *on
035100060927      * Chiusura del programma
035200060927     c                   eval      $Fine   = *on
035300060927      *
035400060927     c                   ENDSR
035500060927
035600060927      *---------------------------------------------------------------*
035700060927      *?Gestione tasto funzionale F6 da videata D01                  ?*
035800060927      *?F6=Conferma                                                  ?*
035900060927      *---------------------------------------------------------------*
036000060927     c     F06D01        BEGSR
036100060927      *
036200060927      * Aggiornamento record
036300060927     c                   exsr      Upd_DPCVE
036400060927      *
036500060927      * Chiusura del programma
036600060927     c                   eval      $Fine   = *on
036700060927      *
036800060927     c                   ENDSR
036900060927
037000060927      *---------------------------------------------------------------*
037100060927      *?Gestione tasto funzionale F12 da videata D01                 ?*
037200060927      *?F12=Ritorno                                                  ?*
037300060927      *---------------------------------------------------------------*
037400060927     c     F12D01        BEGSR
037500060927      *
037600060927      * Disallocazione del record
037700060927     c                   if             D00op1 = 'O02'
037800060927     c                             or   D00op1 = 'O04'
037900060927     c                             or   D00op1 = 'O07'
038000170628     c                   UNLOCK    DPCVE11I
038100060927     c                   endif
038200060927      * Impostazione flag di ritorno
038300060927     c                   eval      D00f12  = *on
038400060927      * Chiusura del programma
038500060927     c                   eval      $Fine   = *on
038600060927      *
038700060927     c                   ENDSR
038800060927
038900060927      *---------------------------------------------------------------*
039000060927      *?Controllo videata D01                                        ?*
039100060927      *---------------------------------------------------------------*
039200060927     c     CtrD01        BEGSR
039300060927      *
039400060927      * Numero Versione
039500060927sel 1c                   SELECT
039600060927      * - obbligatorio
039700060927w   1c                   WHEN      V1Cver  = *zeros
039800060927     c                   seton                                        285090
039900060927     c                   eval      V1Dmsg  = $Msg( 4)
040000060927     c                   leavesr
040100060927      * - errato (negativo)
040200060927w   1c                   WHEN      V1Cver  < *zeros
040300060927     c                   seton                                        285090
040400060927     c                   eval      V1Dmsg  = $Msg( 5)
040500060927     c                   leavesr
040600060927e   1c                   ENDSL
040700060927      * - già esistente (se immissione)
040800060927if  1c                   if             D00op1  = 'F06'
040900060927     c                             or   D00op1  = 'O03'
041000170628     c     V1Cver        chain(n)  DPCVE100
041100170628if  2c                   if        %found(DPCVE11I)
041200060927     c                   seton                                        285090
041300060927     c                   eval      V1Dmsg  = $Msg( 3)
041400060927     c                   leavesr
041500060927e   2c                   endif
041600060927e   1c                   endif
041700060927      *
041800060927      * Data decorrenza
041900060927if  1c                   IF        V1Cdde  = *zeros
042000060927     c                   seton                                        285190
042100060927     c                   eval      V1Dmsg  = $Msg( 6)
042200060927     c                   leavesr
042300060927x   1c                   ELSE
042400060927     c                   clear                   WLBdat
042500060927     c                   eval      G08dat  = V1Cdde
042600060927     c                   call      'XSRDA8'
042700060927     c                   parm                    WLBdat
042800060927if  2c                   if        G08err  = *on
042900060927     c                   seton                                        285190
043000060927     c                   eval      V1Dmsg  = $Msg( 7)
043100060927     c                   leavesr
043200060927e   2c                   endif
043300060927     c                   eval      V1Cdde  = G08dat
043400060927     c                   eval      W1Cdde  = G08inv
043500060927e   1c                   ENDIF
043600060927      *
043700060927      * Data scadenza
043800060927if  1c                   IF        V1Cdsc  = *zeros
043900060927     c                   seton                                        285290
044000060927     c                   eval      V1Dmsg  = $Msg( 6)
044100060927     c                   leavesr
044200060927x   1c                   ELSE
044300060927     c                   clear                   WLBdat
044400060927     c                   eval      G08dat  = V1Cdsc
044500060927     c                   call      'XSRDA8'
044600060927     c                   parm                    WLBdat
044700060927if  2c                   if        G08err  = *on
044800060927     c                   seton                                        285290
044900060927     c                   eval      V1Dmsg  = $Msg( 7)
045000060927     c                   leavesr
045100060927e   2c                   endif
045200060927     c                   eval      V1Cdsc  = G08dat
045300060927     c                   eval      W1Cdsc  = G08inv
045400060927e   1c                   ENDIF
045500060927      *
045600060927sel 1c                   select
045700060927w   1c                   when      W1Cdsc  < W1Cdde
045800060927     c                   seton                                        285290
045900060927     c                   eval      V1Dmsg  = $Msg( 8)
046000060927     c                   leavesr
046100060927w   1c                   when      W1Cdsc  < wDate
046200060927     c                   seton                                        285290
046300060927     c                   eval      V1Dmsg  = $Msg( 9)
046400060927     c                   leavesr
046500060927e   1c                   endsl
046600060927      *
046700060927      * Sovrapposizione date...
046800170628     c     W1Cdsc        setgt     DPCVE102
046900170628     c                   readp     DPCVE102
047000170628do  1c                   DOW       NOT  %eof(DPCVE12I)
047100060927if  2c                   if             CV2atb  = *blanks
047200060927     c                             and  CV2ver <> V1Cver
047300060927     c                             and (CV2dde <= W1Cdde
047400060927     c                             and  CV2dsc >= W1Cdde
047500060927     c                             or   CV2dde <= W1Cdsc
047600060927     c                             and  CV2dsc >= W1Cdsc)
047700060927     c                   seton                                        285190
047800060927     c                   eval      V1Dmsg  = $Msg(10)
047900060927     c                   leavesr
048000060927e   2c                   endif
048100170628     c                   readp     DPCVE102
048200060927e   1c                   ENDDO
048300060927      *
048400060927      * Versione DPD
048500060927if  1c                   IF        V1Cverd = *blanks
048600060927     c                   seton                                        285390
048700060927     c                   eval      V1Dmsg  = $Msg(11)
048800060927     c                   leavesr
048900060927e   1c                   ENDIF
049000170628      *
049100170628      * Data importazione
049200170628if  1c                   IF        V1Cdim  = *zeros
049300170628     c                   seton                                        285590
049400170628     c                   eval      V1Dmsg  = $Msg( 6)
049500170628     c                   leavesr
049600170628x   1c                   ELSE
049700170628     c                   clear                   WLBdat
049800170628     c                   eval      G08dat  = V1Cdim
049900170628     c                   call      'XSRDA8'
050000170628     c                   parm                    WLBdat
050100170628if  2c                   if        G08err  = *on
050200170628     c                   seton                                        285590
050300170628     c                   eval      V1Dmsg  = $Msg( 7)
050400170628     c                   leavesr
050500170628e   2c                   endif
050600170628     c                   eval      V1Cdim  = G08dat
050700170628     c                   eval      W1Cdim  = G08inv
050800170628e   1c                   ENDIF
050900060927      *
051000060927     c                   ENDSR
051100060927
051200060927      *---------------------------------------------------------------*
051300060927      *?Aggiornamento dati                                           ?*
051400060927      *---------------------------------------------------------------*
051500060927     c     Upd_DPCVE     BEGSR
051600060927      *
051700060927     c                   select
051800060927      *
051900060927      * Inserimento / Copia
052000060927w   1c                   when           D00op1 = 'F06'
052100060927     c                             or   D00op1 = 'O03'
052200060927     c                   exsr      RieRec
052300170628     c                   WRITE     DPCVE100
052400060927      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
052500060927      * Modifica
052600060927w   1c                   when      D00op1  = 'O02'
052700060927     c                   exsr      RieRec
052800170628     c                   UPDATE    DPCVE100
052900061109      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
053000061109      * Cancellazione
053100061109w   1c                   when      D00op1  = 'O04'
053200170628     c                   DELETE    DPCVE100
053300061109      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
053400060927e   1c                   endsl
053500060927      *
053600060927      * Ritorno al chiamante dei dati aggiornati
053700060927     c***                if        SDSprm  > 1
053800170628     c*** già così...:   movel     DPCVE100      ds_DPCVE
053900060927     c***                endif
054000060927      *
054100060927     c                   ENDSR
054200060927
054300060927      *---------------------------------------------------------------*
054400060927      *?Impostazione valori nei campi del record                     ?*
054500060927      *---------------------------------------------------------------*
054600060927     c     RieRec        BEGSR
054700060927      *
054800170628     c                   clear                   DPCVE100
054900060927      *
055000060927     c                   eval      CVEver  = V1Cver
055100060927     c                   eval      CVEdde  = W1Cdde
055200060927     c                   eval      CVEdsc  = W1Cdsc
055300060927     c                   eval      CVEverd = V1Cverd
055400170628     c                   eval      CVEBCID = V1CBCID
055500170628     c                   eval      CVEdim  = W1Cdim
055600060927     c*** già così:      eval      CVEatb  = *blanks
055700060927      *
055800060927     c                   ENDSR
055900060927
056000060927** - $Fun ------*
056100060927   INSERIMENTO      1
056200060927     MODIFICA       2
056300060927      COPIA         3
056400061109  CANCELLAZIONE     4   ex   ANNULLAMENTO     4
056500060927 VISUALIZZAZIONE    5
056600060927    RIPRISTINO      6
056700060927** - $Msg -------------------------------------------------------------------*
056800060927Richiesta funzione non prevista                                                 1
056900060927Richiesta funzione errata per record inesistente                                2
057000060927Record già esistente per la "versione cappario DPD" specificata                 3
057100060927Numero versione obbligatorio                                                    4
057200060927Numero versione errato                                                          5
057300060927Data obbligatoria                                                               6
057400060927Data formalmente errata                                                         7
057500060927La data scadenza non può essere precedente alla data decorrenza                 8
057600060927La versione non può scadere in un giorno già trascorso                          9
057700060927Questa versione si sovrappone ad una già inserita:                             10
057800060927Versione DPD obbligatoria                                                      11
057900060927Descrizione obbligatoria                                                       12
