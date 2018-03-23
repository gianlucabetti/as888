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
002700060927     fDPCVE02L  if   e           k disk    rename(DPCVE000:DPCVE002)
002800060927     f                                     prefix(CV2:3)
002900060927     fDPCVE01L  Uf A e           k disk
003000060927      *
003100060927     fTISIE7D   cf   e             workstn
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
005500060927     d ds_DPCVE      e ds                  extname(DPCVE00F)
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
008500060927      *
008600060927      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
008700060927      *
008800060927
008900060927      *---------------------------------------------------------------*
009000060927      *  RIEPILOGO INDICATORI                                         *
009100060927      *---------------------------------------------------------------*
009200060927      * 10    - Comodo                                                *
009300060927      * 28    - Emissione del messaggio di errore a video             *
009400060927      * 90    - Generico di errore                                    *
009500060927      *---------------------------------------------------------------*
009600060927
009700060927     c     *Entry        plist
009800060927     c                   parm                    KPJBA
009900060927     c                   parm                    ds_DPCVE
010000060927     c                   movel     KPJBU         TISI00ds
010100060927      *
010200060927      * Operazioni Iniziali
010300060927     c                   exsr      RoutInz
010400060927      *
010500060927      * Gestione Video
010600060927do  1c                   DOW       $Fine   = *off
010700060927     c                   exsr      GesD01
010800060927e   1c                   ENDDO
010900060927      *
011000060927      * Operazioni Finali
011100060927     c                   movel     TISI00ds      KPJBU
011200060927     c                   eval      *inLR   = *on
011300060927
011400060927      *---------------------------------------------------------------*
011500060927      *?Operazioni Iniziali                                          ?*
011600060927      *---------------------------------------------------------------*
011700060927     c     RoutInz       BEGSR
011800060927      *
011900061109     c/exec sql
012000061109     c+     SET OPTION Commit    = *NONE,
012100061109     c+                CloSQLCsr = *ENDMOD,
012200061109     c+                DatFmt    = *ISO,
012300061109     c+                TimFmt    = *ISO
012400061109     c/end-exec
012500061109      *
012600060927      * Reperimento dati job
012700060927     c                   exsr      DatiJob
012800060927      *
012900060927      * Impostazione nome programma a video
013000060927     c                   movel     SDSpgm        V1Tpgm
013100060927      *
013200060927      * Reperimento data corrente in formato gg/mm/aaaa
013300060927     c                   move      *date         wDate_ISO
013400060927      * Conversione data corrente in formato aaaa/mm/gg
013500060927     c                   move      wDate_ISO     wDate
013600060927      *
013700060927      * Inizializzazione parametri di output
013800060927     c                   eval      D00f03  = *off
013900060927     c                   eval      D00f12  = *off
014000060927     c                   eval      D00err  = *off
014100060927     c                   eval      D00msg  = *blanks
014200060927     c                   if        SDSprm  > 1
014300060927     c                   clear                   ds_DPCVE
014400060927     c                   endif
014500060927      *
014600060927      * Controllo parametri ricevti
014700060927      * => eseguiti nella subr. INZD01
014800060927      *
014900060927     c                   ENDSR
015000060927
015100060927      *---------------------------------------------------------------*
015200060927      *?Reperimento Dati del job (Utente/Operativi)                  ?*
015300060927      *---------------------------------------------------------------*
015400060927     c     DatiJob       BEGSR
015500060927      *
015600060927     c     *dtaara       define    §azute        azuteds
015700060927     c     *dtaara       define    §datiute      ddatiute
015800060927      *
015900060927     c                   in(E)     *dtaara
016000060927     c                   IF        %ERROR or RSUT = *blanks
016100060927     c                   clear                   Tibs34Ds
016200060927     c                   call      'TIBS34R'
016300060927     c                   parm                    Tibs34Ds
016400060927     c                   in        *dtaara
016500060927     c                   ENDIF
016600060927      *
016700060927     c                   ENDSR
016800060927
016900060927      *---------------------------------------------------------------*
017000060927      *?Gestione videata D01                                         ?*
017100060927      *---------------------------------------------------------------*
017200060927     c     GesD01        BEGSR
017300060927      *
017400060927      * Inizializzazione videata
017500060927if  1c                   if        $InzD01 = *on
017600060927     c                   exsr      InzD01
017700060927     c                   eval      $InzD01 = (*in90 = *on)
017800060927e   1c                   endif
017900060927      *
018000060927      * Scrittura di testata e di riga tasti funzionali abilitati
018100060927if  1c                   if        NOT *in90
018200060927     c                   write     SIE7T01
018300060927     c                   write     SIE7Z01
018400060927e   1c                   endif
018500060927      *
018600060927      * Emissione videata
018700061109if  1c                   if        NOT *in90
018800061109     c                             and  D00op1 = 'O04'
018900061109     c                   write     SIE7D02
019000061109e   1c                   endif
019100060927     c                   exfmt     SIE7D01
019200060927     c                   setoff                                       28  90
019300060927     c                   clear                   V1Dmsg
019400060927      *
019500060927sel 1c                   SELECT
019600060927      * F3=Fine
019700060927w   1c                   WHEN      *inKC
019800060927     c                   exsr      F03D01
019900060927      * F12=Ritorno
020000060927w   1c                   WHEN      *inKL
020100060927     c                   exsr      F12D01
020200060927      * Visualizzazione (nessun controllo)
020300060927w   1c                   WHEN      D00op1  = 'O05'
020400060927      * Annullamento    (nessun controllo)
020500060927w   1c                   WHEN      D00op1  = 'O04'
020600060927if  2c                   if        NOT *in90   and   *inKF
020700061109     c                   move      C_Delete      D2Dtxt
020800061109     c                   write     SIE7D02
020900060927     c                   exsr      F06D01
021000061109x   2c                   else
021100061109      *   (serve solo a velocizzare la riemissione delle videate)
021200061109     c                   eval      *in90   = *on
021300060927e   2c                   endif
021400060927      * Inserimento/Modifica/Ripristino
021500060927x   1c                   OTHER
021600060927     c                   exsr      CtrD01
021700060927      * - F6=Conferma
021800060927if  2c                   if        NOT *in90   and   *inKF
021900060927     c                   exsr      F06D01
022000060927e   2c                   endif
022100060927e   1c                   ENDSL
022200060927      *
022300060927     c                   ENDSR
022400060927
022500060927      *---------------------------------------------------------------*
022600060927      *?Inizializzazione videata D01                                 ?*
022700060927      *---------------------------------------------------------------*
022800060927     c     InzD01        BEGSR
022900060927      *
023000060927     c                   clear                   SIE7D01
023100060927      *
023200060927      * Abilitazione tasto funzionale F6=Conferma
023300060927     c                   eval      *in06   = (D00op1 = 'F06'  or
023400060927     c                                        D00op1 = 'O02'  or
023500060927     c                                        D00op1 = 'O03'  or
023600060927     c                                        D00op1 = 'O04'  or
023700060927     c                                        D00op1 = 'O07')
023800060927      * Protezione campo chiave a video
023900060927     c                   eval      *in41   = (D00op1 = 'O02')
024000060927      * Protezione tutti i campi a video
024100060927     c                   eval      *in42   = (D00op1 = 'O04'  or
024200060927     c                                        D00op1 = 'O05'  or
024300060927     c                                        D00op1 = 'O07')
024400060927      *
024500060927      * Controllo parametri ricevti
024600060927sel 1c                   SELECT
024700060927      * - Se richiesto inserimento:
024800060927      *   il controllo di effettiva inesistenza verrà eseguito nella
024900060927      *   subr. CTRD01.
025000060927w   1c                   WHEN      D00op1  = 'F06'
025100060927     c                   eval      V1Ttxt  = $Fun(1)
025200060927     c                   leavesr
025300061109      * - Se richiesta modifica/cancellazione/annullamento/ripristino:
025400060927      *   il record da aggiornare va allocato
025500060927w   1c                   WHEN           D00op1 = 'O02'
025600060927     c                             or   D00op1 = 'O04'
025700060927     c                             or   D00op1 = 'O07'
025800060927sel 2c                   select
025900060927w   2c                   when      D00op1  = 'O02'
026000060927     c                   eval      V1Ttxt  = $Fun(2)
026100060927w   2c                   when      D00op1  = 'O04'
026200060927     c                   eval      V1Ttxt  = $Fun(4)
026300060927w   2c                   when      D00op1  = 'O07'
026400060927     c                   eval      V1Ttxt  = $Fun(6)
026500060927e   2c                   endsl
026600060927     c     D00ver        chain     DPCVE000
026700060927if  2c                   if        NOT  %found(DPCVE01L)
026800060927     c                   seton                                        28  90
026900060927     c                   eval      V1Dmsg  = %trim($Msg(2))
027000060927     c                   eval      V1Cver  = D00ver
027100060927     c                   leavesr
027200060927e   2c                   endif
027300060927      * - Se richiesta visualizzazione/copia
027400060927      *   il record da visualizzare NON va allocato
027500060927w   1c                   WHEN           D00op1 = 'O03'
027600060927     c                             or   D00op1 = 'O05'
027700060927sel 2c                   select
027800060927w   2c                   when      D00op1  = 'O03'
027900060927     c                   eval      V1Ttxt  = $Fun(3)
028000060927w   2c                   when      D00op1  = 'O05'
028100060927     c                   eval      V1Ttxt  = $Fun(5)
028200060927e   2c                   endsl
028300060927     c     D00ver        chain(n)  DPCVE000
028400060927if  2c                   if        NOT  %found(DPCVE01L)
028500060927     c                   seton                                        28  90
028600060927     c                   eval      V1Dmsg  = %trim($Msg(2))
028700060927     c                   eval      V1Cver  = D00ver
028800060927     c                   leavesr
028900060927e   2c                   endif
029000060927      * - Altrimenti non risulta una richiesta valida
029100060927x   1c                   OTHER
029200060927     c                   eval      *in41   = *on
029300060927     c                   seton                                        28  90
029400060927     c                   eval      V1Dmsg  = %trim($Msg(1))
029500060927     c                                     + ' ('
029600060927     c                                     + D00op1
029700060927     c                                     + ')'
029800060927     c                   eval      V1Cver  = D00ver
029900060927     c                   leavesr
030000060927e   1c                   ENDSL
030100060927      *
030200060927      * Impostazione campi chiave a video
030300060927if  1c                   if        D00op1 <> 'O03'
030400060927     c                   eval      V1Cver  = CVEver
030500060927e   3c                   endif
030600060927      *
030700060927      * Impostazione altri campi a video
030800060927     c***                eval      V1Cdde  = CVEdde
030900060927     c                   reset                   WLBdat
031000060927     c                   eval      G08inv  = CVEdde
031100060927     c                   call      'XSRDA8'
031200060927     c                   parm                    WLBdat
031300060927if  1c                   if        G08err  = *off
031400060927     c                   eval      V1Cdde  = G08dat
031500060927e   1c                   endif
031600060927     c***                eval      V1Cdsc  = CVEdsc
031700060927     c                   reset                   WLBdat
031800060927     c                   eval      G08inv  = CVEdsc
031900060927     c                   call      'XSRDA8'
032000060927     c                   parm                    WLBdat
032100060927if  1c                   if        G08err  = *off
032200060927     c                   eval      V1Cdsc  = G08dat
032300060927e   1c                   endif
032400060927     c                   eval      V1Cverd = CVEverd
032500060927     c                   eval      V1Cdes  = CVEdes
032600060927      *
032700060927     c                   ENDSR
032800060927
032900060927      *---------------------------------------------------------------*
033000060927      *?Gestione tasto funzionale F3 da videata D01                  ?*
033100060927      *?F3=Fine                                                      ?*
033200060927      *---------------------------------------------------------------*
033300060927     c     F03D01        BEGSR
033400060927      *
033500060927      * Disallocazione del record
033600060927     c                   if             D00op1 = 'O02'
033700060927     c                             or   D00op1 = 'O04'
033800060927     c                             or   D00op1 = 'O07'
033900060927     c                   UNLOCK    DPCVE01L
034000060927     c                   endif
034100060927      * Impostazione flag di ritorno
034200060927     c                   eval      D00f03  = *on
034300060927      * Chiusura del programma
034400060927     c                   eval      $Fine   = *on
034500060927      *
034600060927     c                   ENDSR
034700060927
034800060927      *---------------------------------------------------------------*
034900060927      *?Gestione tasto funzionale F6 da videata D01                  ?*
035000060927      *?F6=Conferma                                                  ?*
035100060927      *---------------------------------------------------------------*
035200060927     c     F06D01        BEGSR
035300060927      *
035400060927      * Aggiornamento record
035500060927     c                   exsr      Upd_DPCVE
035600060927      *
035700060927      * Chiusura del programma
035800060927     c                   eval      $Fine   = *on
035900060927      *
036000060927     c                   ENDSR
036100060927
036200060927      *---------------------------------------------------------------*
036300060927      *?Gestione tasto funzionale F12 da videata D01                 ?*
036400060927      *?F12=Ritorno                                                  ?*
036500060927      *---------------------------------------------------------------*
036600060927     c     F12D01        BEGSR
036700060927      *
036800060927      * Disallocazione del record
036900060927     c                   if             D00op1 = 'O02'
037000060927     c                             or   D00op1 = 'O04'
037100060927     c                             or   D00op1 = 'O07'
037200060927     c                   UNLOCK    DPCVE01L
037300060927     c                   endif
037400060927      * Impostazione flag di ritorno
037500060927     c                   eval      D00f12  = *on
037600060927      * Chiusura del programma
037700060927     c                   eval      $Fine   = *on
037800060927      *
037900060927     c                   ENDSR
038000060927
038100060927      *---------------------------------------------------------------*
038200060927      *?Controllo videata D01                                        ?*
038300060927      *---------------------------------------------------------------*
038400060927     c     CtrD01        BEGSR
038500060927      *
038600060927      * Numero Versione
038700060927sel 1c                   SELECT
038800060927      * - obbligatorio
038900060927w   1c                   WHEN      V1Cver  = *zeros
039000060927     c                   seton                                        285090
039100060927     c                   eval      V1Dmsg  = $Msg( 4)
039200060927     c                   leavesr
039300060927      * - errato (negativo)
039400060927w   1c                   WHEN      V1Cver  < *zeros
039500060927     c                   seton                                        285090
039600060927     c                   eval      V1Dmsg  = $Msg( 5)
039700060927     c                   leavesr
039800060927e   1c                   ENDSL
039900060927      * - già esistente (se immissione)
040000060927if  1c                   if             D00op1  = 'F06'
040100060927     c                             or   D00op1  = 'O03'
040200060927     c     V1Cver        chain(n)  DPCVE000
040300060927if  2c                   if        %found(DPCVE01L)
040400060927     c                   seton                                        285090
040500060927     c                   eval      V1Dmsg  = $Msg( 3)
040600060927     c                   leavesr
040700060927e   2c                   endif
040800060927e   1c                   endif
040900060927      *
041000060927      * Data decorrenza
041100060927if  1c                   IF        V1Cdde  = *zeros
041200060927     c                   seton                                        285190
041300060927     c                   eval      V1Dmsg  = $Msg( 6)
041400060927     c                   leavesr
041500060927x   1c                   ELSE
041600060927     c                   clear                   WLBdat
041700060927     c                   eval      G08dat  = V1Cdde
041800060927     c                   call      'XSRDA8'
041900060927     c                   parm                    WLBdat
042000060927if  2c                   if        G08err  = *on
042100060927     c                   seton                                        285190
042200060927     c                   eval      V1Dmsg  = $Msg( 7)
042300060927     c                   leavesr
042400060927e   2c                   endif
042500060927     c                   eval      V1Cdde  = G08dat
042600060927     c                   eval      W1Cdde  = G08inv
042700060927e   1c                   ENDIF
042800060927      *
042900060927      * Data scadenza
043000060927if  1c                   IF        V1Cdsc  = *zeros
043100060927     c                   seton                                        285290
043200060927     c                   eval      V1Dmsg  = $Msg( 6)
043300060927     c                   leavesr
043400060927x   1c                   ELSE
043500060927     c                   clear                   WLBdat
043600060927     c                   eval      G08dat  = V1Cdsc
043700060927     c                   call      'XSRDA8'
043800060927     c                   parm                    WLBdat
043900060927if  2c                   if        G08err  = *on
044000060927     c                   seton                                        285290
044100060927     c                   eval      V1Dmsg  = $Msg( 7)
044200060927     c                   leavesr
044300060927e   2c                   endif
044400060927     c                   eval      V1Cdsc  = G08dat
044500060927     c                   eval      W1Cdsc  = G08inv
044600060927e   1c                   ENDIF
044700060927      *
044800060927sel 1c                   select
044900060927w   1c                   when      W1Cdsc  < W1Cdde
045000060927     c                   seton                                        285290
045100060927     c                   eval      V1Dmsg  = $Msg( 8)
045200060927     c                   leavesr
045300060927w   1c                   when      W1Cdsc  < wDate
045400060927     c                   seton                                        285290
045500060927     c                   eval      V1Dmsg  = $Msg( 9)
045600060927     c                   leavesr
045700060927e   1c                   endsl
045800060927      *
045900060927      * Sovrapposizione date...
046000060927     c     W1Cdsc        setgt     DPCVE002
046100060927     c                   readp     DPCVE002
046200060927do  1c                   DOW       NOT  %eof(DPCVE02L)
046300060927if  2c                   if             CV2atb  = *blanks
046400060927     c                             and  CV2ver <> V1Cver
046500060927     c                             and (CV2dde <= W1Cdde
046600060927     c                             and  CV2dsc >= W1Cdde
046700060927     c                             or   CV2dde <= W1Cdsc
046800060927     c                             and  CV2dsc >= W1Cdsc)
046900060927     c                   seton                                        285190
047000060927     c                   eval      V1Dmsg  = $Msg(10)
047100060927     c                   leavesr
047200060927e   2c                   endif
047300060927     c                   readp     DPCVE002
047400060927e   1c                   ENDDO
047500060927      *
047600060927      * Versione DPD
047700060927if  1c                   IF        V1Cverd = *blanks
047800060927     c                   seton                                        285390
047900060927     c                   eval      V1Dmsg  = $Msg(11)
048000060927     c                   leavesr
048100060927e   1c                   ENDIF
048200060927      *
048300060927      * Descrizione versione
048400060927if  1c                   IF        V1Cdes  = *blanks
048500060927     c                   seton                                        285490
048600060927     c                   eval      V1Dmsg  = $Msg(12)
048700060927     c                   leavesr
048800060927e   1c                   ENDIF
048900060927      *
049000060927     c                   ENDSR
049100060927
049200060927      *---------------------------------------------------------------*
049300060927      *?Aggiornamento dati                                           ?*
049400060927      *---------------------------------------------------------------*
049500060927     c     Upd_DPCVE     BEGSR
049600060927      *
049700060927     c                   select
049800060927      *
049900060927      * Inserimento / Copia
050000060927w   1c                   when           D00op1 = 'F06'
050100060927     c                             or   D00op1 = 'O03'
050200060927     c                   exsr      RieRec
050300060927     c                   WRITE     DPCVE000
050400060927      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
050500060927      * Modifica
050600060927w   1c                   when      D00op1  = 'O02'
050700060927     c                   exsr      RieRec
050800060927     c                   UPDATE    DPCVE000
050900061109      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
051000061109      * Cancellazione
051100061109w   1c                   when      D00op1  = 'O04'
051200061109     c                   exsr      sr_DeleteDPC
051300061109     c                   DELETE    DPCVE000
051400061109      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
051500061109     *** * Annullamento
051600061109w   1***c                   when      D00op1  = 'O04'
051700061109     ***c                   eval      CVEatb  = 'A'
051800061109     ***c                   UPDATE    DPCVE000
051900061109     *** *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
052000061109     *** * Ripristino
052100061109w   1***c                   when      D00op1  = 'O07'
052200061109     ***c                   eval      CVEatb  = *blanks
052300061109     ***c                   UPDATE    DPCVE000
052400061109     *** *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
052500060927e   1c                   endsl
052600060927      *
052700060927      * Ritorno al chiamante dei dati aggiornati
052800060927     c***                if        SDSprm  > 1
052900060927     c*** già così...:   movel     DPCVE000      ds_DPCVE
053000060927     c***                endif
053100060927      *
053200060927     c                   ENDSR
053300060927
053400060927      *---------------------------------------------------------------*
053500060927      *?Impostazione valori nei campi del record                     ?*
053600060927      *---------------------------------------------------------------*
053700060927     c     RieRec        BEGSR
053800060927      *
053900060927     c                   clear                   DPCVE000
054000060927      *
054100060927     c                   eval      CVEver  = V1Cver
054200060927     c                   eval      CVEdde  = W1Cdde
054300060927     c                   eval      CVEdsc  = W1Cdsc
054400060927     c                   eval      CVEverd = V1Cverd
054500060927     c                   eval      CVEdes  = V1Cdes
054600060927     c*** già così:      eval      CVEatb  = *blanks
054700060927      *
054800060927     c                   ENDSR
054900061109
055000061109      *---------------------------------------------------------------*
055100061109      *?Cancellazione record da tutti gli archivi legati...          ?*
055200061109      *---------------------------------------------------------------*
055300061109     c     sr_DeleteDPC  BEGSR
055400061109      *
055500061109      *?Cappario DPD - Allow?
055600061109     c/exec sql
055700061109     c+   DELETE
055800061109     c+   from   DPCAL01L
055900061109     c+   where  CALver = :V1Cver
056000061109     c/end-exec
056100061109      *
056200061109      *?Cappario DPD - Country Code Iso?
056300061109     c/exec sql
056400061109     c+   DELETE
056500061109     c+   from   DPCCC01L
056600061109     c+   where  CCCver = :V1Cver
056700061109     c/end-exec
056800061109      *
056900061109      *?Cappario DPD - Customs?
057000061109     c/exec sql
057100061109     c+   DELETE
057200061109     c+   from   DPCCU01L
057300061109     c+   where  CCUver = :V1Cver
057400061109     c/end-exec
057500061109      *
057600061109      *?Cappario DPD - Depots?
057700061109     c/exec sql
057800061109     c+   DELETE
057900061109     c+   from   DPCDP01L
058000061109     c+   where  CDPver = :V1Cver
058100061109     c/end-exec
058200061109      *
058300061109      *?Cappario DPD - Leny?
058400061109     c/exec sql
058500061109     c+   DELETE
058600061109     c+   from   DPCDY01L
058700061109     c+   where  CDYver = :V1Cver
058800061109     c/end-exec
058900061109      *
059000061109      *?Cappario DPD - Location?
059100061109     c/exec sql
059200061109     c+   DELETE
059300061109     c+   from   DPCLO01L
059400061109     c+   where  CLOver = :V1Cver
059500061109     c/end-exec
059600061109      *
059700061109      *?Cappario DPD - Routes?
059800061109     c/exec sql
059900061109     c+   DELETE
060000061109     c+   from   DPCRO01L
060100061109     c+   where  CROver = :V1Cver
060200061109     c/end-exec
060300061109      *
060400061109      *?Cappario DPD - Service?
060500061109     c/exec sql
060600061109     c+   DELETE
060700061109     c+   from   DPCSC01L
060800061109     c+   where  CSCver = :V1Cver
060900061109     c/end-exec
061000061109      *
061100061109     c                   ENDSR
061200060927
061300060927** - $Fun ------*
061400060927   INSERIMENTO      1
061500060927     MODIFICA       2
061600060927      COPIA         3
061700061109  CANCELLAZIONE     4   ex   ANNULLAMENTO     4
061800060927 VISUALIZZAZIONE    5
061900060927    RIPRISTINO      6
062000060927** - $Msg -------------------------------------------------------------------*
062100060927Richiesta funzione non prevista                                                 1
062200060927Richiesta funzione errata per record inesistente                                2
062300060927Record già esistente per la "versione cappario DPD" specificata                 3
062400060927Numero versione obbligatorio                                                    4
062500060927Numero versione errato                                                          5
062600060927Data obbligatoria                                                               6
062700060927Data formalmente errata                                                         7
062800060927La data scadenza non può essere precedente alla data decorrenza                 8
062900060927La versione non può scadere in un giorno già trascorso                          9
063000060927Questa versione si sovrappone ad una già inserita:                             10
063100060927Versione DPD obbligatoria                                                      11
063200060927Descrizione obbligatoria                                                       12
