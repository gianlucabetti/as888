000100060927      *---------------------------------------------------------------*
000200060927      *?CAPPARIO DPD - GESTIONE SINGOLA VERSIONE                     ?*
000300060927      *---------------------------------------------------------------*
000400060927      *?Parametri RICHIESTI in ds TISI00DS:?                          *
000500060927      * D00OP1: "F06" = Immissione                                    *
000600060927      *         "O02" = Modifica                                      *
000700060927      *         "O03" = Copia           - NON gestito                 *
000800060927      *         "O04" = Annullamento                                  *
000900060927      *         "O05" = Visualizzazione                               *
001000060927      *         "O06" = Stampa          - NON gestito                 *
001100060927      *         "O07" = Ripristino                                    *
001200060927      * D00VER: Versione cappario da elaborare                        *
001300060927      *?Parametri DI OUTPUT in ds TISI00DS:?                          *
001400060927      * D00F03: Uscita via F3=Fine                                    *
001500060927      * D00F12: Uscita via F12=Ritorno                                *
001600060927      * D00ERR: non gestito                                           *
001700060927      * D00MSG: non gestito                                           *
001800060927      *---------------------------------------------------------------*
001900060927
002000060927     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002100060927
002200060927      *---------------------------------------------------------------*
002300060927
002400060927      * Versioni
002500060927     fDPCVE02L  if   e           k disk    rename(DPCVE000:DPCVE002)
002600060927     f                                     prefix(CV2:3)
002700060927     fDPCVE01L  Uf A e           k disk
002800060927      *
002900060927     fTISIE7D   cf   e             workstn
003000060927
003100060927      *---------------------------------------------------------------*
003200060927
003300060927      *
003400060927      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
003500060927      *
003600060927      *
003700060927      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
003800060927      *
003900060927      * - Testi di decodifica funzione richiesta
004000060927     d $Fun            s             17    dim( 6) ctdata  perrcd(1)
004100060927      * - Messaggi di errore
004200060927     d $Msg            s             78    dim(12) ctdata  perrcd(1)
004300060927      *
004400060927      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
004500060927      *
004600060927      * - Architettura
004700060927     d KPJBA         e ds
004800060927      *
004900060927      * - Parametri ricevuti
005000060927     d TISI00ds      e ds
005100060927     d ds_DPCVE      e ds                  extname(DPCVE00F)
005200060927      *
005300060927      * - Reperimento dati utente
005400060927     d TIBS34ds      e ds                                                       *Profili utente
005500060927     d dDatiUte      e ds                                                       *Dati utente
005600060927     d AZUTEds       e ds                  extname(AZUTE00F)                    *Utenti
005700060927      *
005800060927     d Status         sds           333
005900060927     d   SDSpgm          *proc
006000060927     d   SDSprm          *parms
006100060927      *
006200060927      * - Controllo/Inversione date ("XSRDA8")
006300060927     d WLBdat          ds                  inz
006400060927     d  G08dat                 1      8  0 inz
006500060927     d  G08inv                 9     16  0 inz
006600060927     d  G08err                17     17    inz('3')
006700060927     d  G08tgi                18     22  0 inz
006800060927      *
006900060927      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
007000060927      *
007100060927      * - Flags
007200060927     d $InzD01         s              1    inz(*on)
007300060927     d $Fine           s              1    inz(*off)
007400060927     d $OK             s              1    inz(*off)
007500060927      *
007600060927     d wDate           s              8  0 inz
007700060927     d wDate_ISO       s               d   datfmt(*ISO) inz
007800060927      *
007900060927     d W1Cdde          s                   like(CVEdde) inz
008000060927     d W1Cdsc          s                   like(CVEdsc) inz
008100060927      *
008200060927      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
008300060927      *
008400060927
008500060927      *---------------------------------------------------------------*
008600060927      *  RIEPILOGO INDICATORI                                         *
008700060927      *---------------------------------------------------------------*
008800060927      * 10    - Comodo                                                *
008900060927      * 28    - Emissione del messaggio di errore a video             *
009000060927      * 90    - Generico di errore                                    *
009100060927      *---------------------------------------------------------------*
009200060927
009300060927     c     *Entry        plist
009400060927     c                   parm                    KPJBA
009500060927     c                   parm                    ds_DPCVE
009600060927     c                   movel     KPJBU         TISI00ds
009700060927      *
009800060927      * Operazioni Iniziali
009900060927     c                   exsr      RoutInz
010000060927      *
010100060927      * Gestione Video
010200060927do  1c                   DOW       $Fine   = *off
010300060927     c                   exsr      GesD01
010400060927e   1c                   ENDDO
010500060927      *
010600060927      * Operazioni Finali
010700060927     c                   movel     TISI00ds      KPJBU
010800060927     c                   eval      *inLR   = *on
010900060927
011000060927      *---------------------------------------------------------------*
011100060927      *?Operazioni Iniziali                                          ?*
011200060927      *---------------------------------------------------------------*
011300060927     c     RoutInz       BEGSR
011400060927      *
011500060927      * Reperimento dati job
011600060927     c                   exsr      DatiJob
011700060927      *
011800060927      * Impostazione nome programma a video
011900060927     c                   movel     SDSpgm        V1Tpgm
012000060927      *
012100060927      * Reperimento data corrente in formato gg/mm/aaaa
012200060927     c                   move      *date         wDate_ISO
012300060927      * Conversione data corrente in formato aaaa/mm/gg
012400060927     c                   move      wDate_ISO     wDate
012500060927      *
012600060927      * Inizializzazione parametri di output
012700060927     c                   eval      D00f03  = *off
012800060927     c                   eval      D00f12  = *off
012900060927     c                   eval      D00err  = *off
013000060927     c                   eval      D00msg  = *blanks
013100060927     c                   if        SDSprm  > 1
013200060927     c                   clear                   ds_DPCVE
013300060927     c                   endif
013400060927      *
013500060927      * Controllo parametri ricevti
013600060927      * => eseguiti nella subr. INZD01
013700060927      *
013800060927     c                   ENDSR
013900060927
014000060927      *---------------------------------------------------------------*
014100060927      *?Reperimento Dati del job (Utente/Operativi)                  ?*
014200060927      *---------------------------------------------------------------*
014300060927     c     DatiJob       BEGSR
014400060927      *
014500060927     c     *dtaara       define    §azute        azuteds
014600060927     c     *dtaara       define    §datiute      ddatiute
014700060927      *
014800060927     c                   in(E)     *dtaara
014900060927     c                   IF        %ERROR or RSUT = *blanks
015000060927     c                   clear                   Tibs34Ds
015100060927     c                   call      'TIBS34R'
015200060927     c                   parm                    Tibs34Ds
015300060927     c                   in        *dtaara
015400060927     c                   ENDIF
015500060927      *
015600060927     c                   ENDSR
015700060927
015800060927      *---------------------------------------------------------------*
015900060927      *?Gestione videata D01                                         ?*
016000060927      *---------------------------------------------------------------*
016100060927     c     GesD01        BEGSR
016200060927      *
016300060927      * Inizializzazione videata
016400060927if  1c                   if        $InzD01 = *on
016500060927     c                   exsr      InzD01
016600060927     c                   eval      $InzD01 = (*in90 = *on)
016700060927e   1c                   endif
016800060927      *
016900060927      * Scrittura di testata e di riga tasti funzionali abilitati
017000060927if  1c                   if        NOT *in90
017100060927     c                   write     SIE7T01
017200060927     c                   write     SIE7Z01
017300060927e   1c                   endif
017400060927      *
017500060927      * Emissione videata
017600060927     c                   exfmt     SIE7D01
017700060927     c                   setoff                                       28  90
017800060927     c                   clear                   V1Dmsg
017900060927      *
018000060927sel 1c                   SELECT
018100060927      * F3=Fine
018200060927w   1c                   WHEN      *inKC
018300060927     c                   exsr      F03D01
018400060927      * F12=Ritorno
018500060927w   1c                   WHEN      *inKL
018600060927     c                   exsr      F12D01
018700060927      * Visualizzazione (nessun controllo)
018800060927w   1c                   WHEN      D00op1  = 'O05'
018900060927      * Annullamento    (nessun controllo)
019000060927w   1c                   WHEN      D00op1  = 'O04'
019100060927if  2c                   if        NOT *in90   and   *inKF
019200060927     c                   exsr      F06D01
019300060927e   2c                   endif
019400060927      * Inserimento/Modifica/Ripristino
019500060927x   1c                   OTHER
019600060927     c                   exsr      CtrD01
019700060927      * - F6=Conferma
019800060927if  2c                   if        NOT *in90   and   *inKF
019900060927     c                   exsr      F06D01
020000060927e   2c                   endif
020100060927e   1c                   ENDSL
020200060927      *
020300060927     c                   ENDSR
020400060927
020500060927      *---------------------------------------------------------------*
020600060927      *?Inizializzazione videata D01                                 ?*
020700060927      *---------------------------------------------------------------*
020800060927     c     InzD01        BEGSR
020900060927      *
021000060927     c                   clear                   SIE7D01
021100060927      *
021200060927      * Abilitazione tasto funzionale F6=Conferma
021300060927     c                   eval      *in06   = (D00op1 = 'F06'  or
021400060927     c                                        D00op1 = 'O02'  or
021500060927     c                                        D00op1 = 'O03'  or
021600060927     c                                        D00op1 = 'O04'  or
021700060927     c                                        D00op1 = 'O07')
021800060927      * Protezione campo chiave a video
021900060927     c                   eval      *in41   = (D00op1 = 'O02')
022000060927      * Protezione tutti i campi a video
022100060927     c                   eval      *in42   = (D00op1 = 'O04'  or
022200060927     c                                        D00op1 = 'O05'  or
022300060927     c                                        D00op1 = 'O07')
022400060927      *
022500060927      * Controllo parametri ricevti
022600060927sel 1c                   SELECT
022700060927      * - Se richiesto inserimento:
022800060927      *   il controllo di effettiva inesistenza verrà eseguito nella
022900060927      *   subr. CTRD01.
023000060927w   1c                   WHEN      D00op1  = 'F06'
023100060927     c                   eval      V1Ttxt  = $Fun(1)
023200060927     c                   leavesr
023300060927      * - Se richiesta modifica/annullamento/ripristino:
023400060927      *   il record da aggiornare va allocato
023500060927w   1c                   WHEN           D00op1 = 'O02'
023600060927     c                             or   D00op1 = 'O04'
023700060927     c                             or   D00op1 = 'O07'
023800060927sel 2c                   select
023900060927w   2c                   when      D00op1  = 'O02'
024000060927     c                   eval      V1Ttxt  = $Fun(2)
024100060927w   2c                   when      D00op1  = 'O04'
024200060927     c                   eval      V1Ttxt  = $Fun(4)
024300060927w   2c                   when      D00op1  = 'O07'
024400060927     c                   eval      V1Ttxt  = $Fun(6)
024500060927e   2c                   endsl
024600060927     c     D00ver        chain     DPCVE000
024700060927if  2c                   if        NOT  %found(DPCVE01L)
024800060927     c                   seton                                        28  90
024900060927     c                   eval      V1Dmsg  = %trim($Msg(2))
025000060927     c                   eval      V1Cver  = D00ver
025100060927     c                   leavesr
025200060927e   2c                   endif
025300060927      * - Se richiesta visualizzazione/copia
025400060927      *   il record da visualizzare NON va allocato
025500060927w   1c                   WHEN           D00op1 = 'O03'
025600060927     c                             or   D00op1 = 'O05'
025700060927sel 2c                   select
025800060927w   2c                   when      D00op1  = 'O03'
025900060927     c                   eval      V1Ttxt  = $Fun(3)
026000060927w   2c                   when      D00op1  = 'O05'
026100060927     c                   eval      V1Ttxt  = $Fun(5)
026200060927e   2c                   endsl
026300060927     c     D00ver        chain(n)  DPCVE000
026400060927if  2c                   if        NOT  %found(DPCVE01L)
026500060927     c                   seton                                        28  90
026600060927     c                   eval      V1Dmsg  = %trim($Msg(2))
026700060927     c                   eval      V1Cver  = D00ver
026800060927     c                   leavesr
026900060927e   2c                   endif
027000060927      * - Altrimenti non risulta una richiesta valida
027100060927x   1c                   OTHER
027200060927     c                   eval      *in41   = *on
027300060927     c                   seton                                        28  90
027400060927     c                   eval      V1Dmsg  = %trim($Msg(1))
027500060927     c                                     + ' ('
027600060927     c                                     + D00op1
027700060927     c                                     + ')'
027800060927     c                   eval      V1Cver  = D00ver
027900060927     c                   leavesr
028000060927e   1c                   ENDSL
028100060927      *
028200060927      * Impostazione campi chiave a video
028300060927if  1c                   if        D00op1 <> 'O03'
028400060927     c                   eval      V1Cver  = CVEver
028500060927e   3c                   endif
028600060927      *
028700060927      * Impostazione altri campi a video
028800060927     c***                eval      V1Cdde  = CVEdde
028900060927     c                   reset                   WLBdat
029000060927     c                   eval      G08inv  = CVEdde
029100060927     c                   call      'XSRDA8'
029200060927     c                   parm                    WLBdat
029300060927if  1c                   if        G08err  = *off
029400060927     c                   eval      V1Cdde  = G08dat
029500060927e   1c                   endif
029600060927     c***                eval      V1Cdsc  = CVEdsc
029700060927     c                   reset                   WLBdat
029800060927     c                   eval      G08inv  = CVEdsc
029900060927     c                   call      'XSRDA8'
030000060927     c                   parm                    WLBdat
030100060927if  1c                   if        G08err  = *off
030200060927     c                   eval      V1Cdsc  = G08dat
030300060927e   1c                   endif
030400060927     c                   eval      V1Cverd = CVEverd
030500060927     c                   eval      V1Cdes  = CVEdes
030600060927      *
030700060927     c                   ENDSR
030800060927
030900060927      *---------------------------------------------------------------*
031000060927      *?Gestione tasto funzionale F3 da videata D01                  ?*
031100060927      *?F3=Fine                                                      ?*
031200060927      *---------------------------------------------------------------*
031300060927     c     F03D01        BEGSR
031400060927      *
031500060927      * Disallocazione del record
031600060927     c                   if             D00op1 = 'O02'
031700060927     c                             or   D00op1 = 'O04'
031800060927     c                             or   D00op1 = 'O07'
031900060927     c                   UNLOCK    DPCVE01L
032000060927     c                   endif
032100060927      * Impostazione flag di ritorno
032200060927     c                   eval      D00f03  = *on
032300060927      * Chiusura del programma
032400060927     c                   eval      $Fine   = *on
032500060927      *
032600060927     c                   ENDSR
032700060927
032800060927      *---------------------------------------------------------------*
032900060927      *?Gestione tasto funzionale F6 da videata D01                  ?*
033000060927      *?F6=Conferma                                                  ?*
033100060927      *---------------------------------------------------------------*
033200060927     c     F06D01        BEGSR
033300060927      *
033400060927      * Aggiornamento record
033500060927     c                   exsr      Upd_DPCVE
033600060927      *
033700060927      * Chiusura del programma
033800060927     c                   eval      $Fine   = *on
033900060927      *
034000060927     c                   ENDSR
034100060927
034200060927      *---------------------------------------------------------------*
034300060927      *?Gestione tasto funzionale F12 da videata D01                 ?*
034400060927      *?F12=Ritorno                                                  ?*
034500060927      *---------------------------------------------------------------*
034600060927     c     F12D01        BEGSR
034700060927      *
034800060927      * Disallocazione del record
034900060927     c                   if             D00op1 = 'O02'
035000060927     c                             or   D00op1 = 'O04'
035100060927     c                             or   D00op1 = 'O07'
035200060927     c                   UNLOCK    DPCVE01L
035300060927     c                   endif
035400060927      * Impostazione flag di ritorno
035500060927     c                   eval      D00f12  = *on
035600060927      * Chiusura del programma
035700060927     c                   eval      $Fine   = *on
035800060927      *
035900060927     c                   ENDSR
036000060927
036100060927      *---------------------------------------------------------------*
036200060927      *?Controllo videata D01                                        ?*
036300060927      *---------------------------------------------------------------*
036400060927     c     CtrD01        BEGSR
036500060927      *
036600060927      * Numero Versione
036700060927sel 1c                   SELECT
036800060927      * - obbligatorio
036900060927w   1c                   WHEN      V1Cver  = *zeros
037000060927     c                   seton                                        285090
037100060927     c                   eval      V1Dmsg  = $Msg( 4)
037200060927     c                   leavesr
037300060927      * - errato (negativo)
037400060927w   1c                   WHEN      V1Cver  < *zeros
037500060927     c                   seton                                        285090
037600060927     c                   eval      V1Dmsg  = $Msg( 5)
037700060927     c                   leavesr
037800060927e   1c                   ENDSL
037900060927      * - già esistente (se immissione)
038000060927if  1c                   if             D00op1  = 'F06'
038100060927     c                             or   D00op1  = 'O03'
038200060927     c     V1Cver        chain(n)  DPCVE000
038300060927if  2c                   if        %found(DPCVE01L)
038400060927     c                   seton                                        285090
038500060927     c                   eval      V1Dmsg  = $Msg( 3)
038600060927     c                   leavesr
038700060927e   2c                   endif
038800060927e   1c                   endif
038900060927      *
039000060927      * Data decorrenza
039100060927if  1c                   IF        V1Cdde  = *zeros
039200060927     c                   seton                                        285190
039300060927     c                   eval      V1Dmsg  = $Msg( 6)
039400060927     c                   leavesr
039500060927x   1c                   ELSE
039600060927     c                   clear                   WLBdat
039700060927     c                   eval      G08dat  = V1Cdde
039800060927     c                   call      'XSRDA8'
039900060927     c                   parm                    WLBdat
040000060927if  2c                   if        G08err  = *on
040100060927     c                   seton                                        285190
040200060927     c                   eval      V1Dmsg  = $Msg( 7)
040300060927     c                   leavesr
040400060927e   2c                   endif
040500060927     c                   eval      V1Cdde  = G08dat
040600060927     c                   eval      W1Cdde  = G08inv
040700060927e   1c                   ENDIF
040800060927      *
040900060927      * Data scadenza
041000060927if  1c                   IF        V1Cdsc  = *zeros
041100060927     c                   seton                                        285290
041200060927     c                   eval      V1Dmsg  = $Msg( 6)
041300060927     c                   leavesr
041400060927x   1c                   ELSE
041500060927     c                   clear                   WLBdat
041600060927     c                   eval      G08dat  = V1Cdsc
041700060927     c                   call      'XSRDA8'
041800060927     c                   parm                    WLBdat
041900060927if  2c                   if        G08err  = *on
042000060927     c                   seton                                        285290
042100060927     c                   eval      V1Dmsg  = $Msg( 7)
042200060927     c                   leavesr
042300060927e   2c                   endif
042400060927     c                   eval      V1Cdsc  = G08dat
042500060927     c                   eval      W1Cdsc  = G08inv
042600060927e   1c                   ENDIF
042700060927      *
042800060927sel 1c                   select
042900060927w   1c                   when      W1Cdsc  < W1Cdde
043000060927     c                   seton                                        285290
043100060927     c                   eval      V1Dmsg  = $Msg( 8)
043200060927     c                   leavesr
043300060927w   1c                   when      W1Cdsc  < wDate
043400060927     c                   seton                                        285290
043500060927     c                   eval      V1Dmsg  = $Msg( 9)
043600060927     c                   leavesr
043700060927e   1c                   endsl
043800060927      *
043900060927      * Sovrapposizione date...
044000060927     c     W1Cdsc        setgt     DPCVE002
044100060927     c                   readp     DPCVE002
044200060927do  1c                   DOW       NOT  %eof(DPCVE02L)
044300060927if  2c                   if             CV2atb  = *blanks
044400060927     c                             and  CV2ver <> V1Cver
044500060927     c                             and (CV2dde <= W1Cdde
044600060927     c                             and  CV2dsc >= W1Cdde
044700060927     c                             or   CV2dde <= W1Cdsc
044800060927     c                             and  CV2dsc >= W1Cdsc)
044900060927     c                   seton                                        285190
045000060927     c                   eval      V1Dmsg  = $Msg(10)
045100060927     c                   leavesr
045200060927e   2c                   endif
045300060927     c                   readp     DPCVE002
045400060927e   1c                   ENDDO
045500060927      *
045600060927      * Versione DPD
045700060927if  1c                   IF        V1Cverd = *blanks
045800060927     c                   seton                                        285390
045900060927     c                   eval      V1Dmsg  = $Msg(11)
046000060927     c                   leavesr
046100060927e   1c                   ENDIF
046200060927      *
046300060927      * Descrizione versione
046400060927if  1c                   IF        V1Cdes  = *blanks
046500060927     c                   seton                                        285490
046600060927     c                   eval      V1Dmsg  = $Msg(12)
046700060927     c                   leavesr
046800060927e   1c                   ENDIF
046900060927      *
047000060927     c                   ENDSR
047100060927
047200060927      *---------------------------------------------------------------*
047300060927      *?Aggiornamento dati                                           ?*
047400060927      *---------------------------------------------------------------*
047500060927     c     Upd_DPCVE     BEGSR
047600060927      *
047700060927     c                   select
047800060927      *
047900060927      * Inserimento / Copia
048000060927w   1c                   when           D00op1 = 'F06'
048100060927     c                             or   D00op1 = 'O03'
048200060927     c                   exsr      RieRec
048300060927     c                   WRITE     DPCVE000
048400060927      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
048500060927      * Modifica
048600060927w   1c                   when      D00op1  = 'O02'
048700060927     c                   exsr      RieRec
048800060927     c                   UPDATE    DPCVE000
048900060927      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
049000060927      * Annullamento
049100060927w   1c                   when      D00op1  = 'O04'
049200060927     c                   eval      CVEatb  = 'A'
049300060927     c                   UPDATE    DPCVE000
049400060927      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
049500060927      * Ripristino
049600060927w   1c                   when      D00op1  = 'O07'
049700060927     c                   eval      CVEatb  = *blanks
049800060927     c                   UPDATE    DPCVE000
049900060927      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
050000060927e   1c                   endsl
050100060927      *
050200060927      * Ritorno al chiamante dei dati aggiornati
050300060927     c***                if        SDSprm  > 1
050400060927     c*** già così...:   movel     DPCVE000      ds_DPCVE
050500060927     c***                endif
050600060927      *
050700060927     c                   ENDSR
050800060927
050900060927      *---------------------------------------------------------------*
051000060927      *?Impostazione valori nei campi del record                     ?*
051100060927      *---------------------------------------------------------------*
051200060927     c     RieRec        BEGSR
051300060927      *
051400060927     c                   clear                   DPCVE000
051500060927      *
051600060927     c                   eval      CVEver  = V1Cver
051700060927     c                   eval      CVEdde  = W1Cdde
051800060927     c                   eval      CVEdsc  = W1Cdsc
051900060927     c                   eval      CVEverd = V1Cverd
052000060927     c                   eval      CVEdes  = V1Cdes
052100060927     c*** già così:      eval      CVEatb  = *blanks
052200060927      *
052300060927     c                   ENDSR
052400060927
052500060927** - $Fun ------*
052600060927   INSERIMENTO      1
052700060927     MODIFICA       2
052800060927      COPIA         3
052900060927   ANNULLAMENTO     4
053000060927 VISUALIZZAZIONE    5
053100060927    RIPRISTINO      6
053200060927** - $Msg -------------------------------------------------------------------*
053300060927Richiesta funzione non prevista                                                 1
053400060927Richiesta funzione errata per record inesistente                                2
053500060927Record già esistente per la "versione cappario DPD" specificata                 3
053600060927Numero versione obbligatorio                                                    4
053700060927Numero versione errato                                                          5
053800060927Data obbligatoria                                                               6
053900060927Data formalmente errata                                                         7
054000060927La data scadenza non può essere precedente alla data decorrenza                 8
054100060927La versione non può scadere in un giorno già trascorso                          9
054200060927Questa versione si sovrappone ad una già inserita:                             10
054300060927Versione DPD obbligatoria                                                      11
054400060927Descrizione obbligatoria                                                       12
