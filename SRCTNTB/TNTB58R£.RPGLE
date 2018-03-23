000100060810      *---------------------------------------------------------------*
000200060810      * Gestione tabella "IPG" = Scaglioni incremento prezzo gasolio  *
000300060810      *---------------------------------------------------------------*
000400060810
000500060810     h decedit('0,') datedit(*dmy/)
000600060810
000700060810      *---------------------------------------------------------------*
000800060810      *   A R C H I V I                                               *
000900060810      *---------------------------------------------------------------*
001000060810      *
001100060810     fTNTBE01L  Uf A e           k disk
001200060810      *
001300060810     fTNTB58D   cf   e             workstn
001400060810     f                                     sfile(TB58S01:S01nrr)
001500060810     f                                     infds(DSFMT)
001600060810
001700060810      *---------------------------------------------------------------*
001800060810      *   C O S T A N T I                                             *
001900060810      *---------------------------------------------------------------*
002000060810      *
002100060810      * - Codice tabella in gestione
002200060810     d C_CodTab        c                   const('IPG')
002300060810      * - Nr. di righe in ogni pagina del sfl
002400060810     d C_SflPag        c                   const(18)
002500060810      * - Tasti funzionali
002600060810     ***d Enter           c                   const(x'F1')
002700060810     ***d RollDwn         c                   const(x'F4')
002800060810     d RollUp          c                   const(x'F5')
002900060811      * Attributi video
003000060811     d C_Atr_Norm      c                   const(X'A0')
003100060811     d C_Atr_BL_RI     c                   const(X'A9')
003200060810
003300060810      *---------------------------------------------------------------*
003400060810      *   S C H I E R E                                               *
003500060810      *---------------------------------------------------------------*
003600060810      *
003700060810      * - Messaggi di errore
003800060811     d $Msg            s             78    dim(  6) ctdata perrcd(1)
003900060810      * - Scaglioni già inseriti
004000060810     d $IPGmax         s                   dim(999) like(§IPGmax) inz
004100060810     d                                              ascend
004200060810
004300060810      *---------------------------------------------------------------*
004400060810      *   S T R U T T U R E   D A T I                                 *
004500060810      *---------------------------------------------------------------*
004600060810      *
004700060810      * Parametri
004800060810     d KPJBA         e ds
004900060810      *
005000060810      * Tabella IPG = Scaglioni di incremento del prezzo del gasolio
005100060810     d dIPG          e ds                  inz
005200060810      *
005300060810      * Tracciato record file TNTBE00F
005400060810     d TNTBEds       e ds                  extname(TNTBE00F) inz
005500060810     d xTNTBEds      e ds                  extname(TNTBE00F) inz
005600060810     d                                     prefix(TBX:3)
005700060810      *
005800060810     d TIBS34ds      e ds                  inz
005900060810     d dDatiUte      e ds                  inz
006000060810     d AZUTEds       e ds                  extname(AZUTE00F) inz
006100060810      *
006200060810     d WLBdat          ds                  inz
006300060810     d  G02dat                 1      8  0 inz
006400060810     d  G02inv                 9     16  0 inz
006500060810     d  G02err                17     17    inz('3')
006600060810     d  G02tgi                18     22  0 inz
006700060810      *
006800060810     d Status         sds
006900060810     d  VTCpgm           *proc
007000060810      *
007100060810     d DSFMT           ds           512
007200060810     d  £Tasto               369    369
007300060810     d  £SFLnrr              378    379B 0
007400060810      *
007500060810      *---------------------------------------------------------------*
007600060810      *   V A R I A B I L I                                           *
007700060810      *---------------------------------------------------------------*
007800060810      *
007900060810      * - Flags
008000060810     d $Fine           s              1a   inz(*off)
008100060810     d $InzS01         s              1a   inz(*on)
008200060811     d SAVin41         s              1a   inz(*off)
008300060828     d SAVin42         s              1a   inz(*off)
008400060810      *
008500060810      * - Indici di schiera
008600060810     d xx              s              5  0 inz
008700060810      *
008800060810      * - Variabili riferite al data base o al display file
008900060811     d S01nrr          s                   like(C01rcd)  inz
009000060811     d wKE1prg         s                   like(V1Cke1)  inz
009100060901     d W1Cke1          s                   like(V1Cke1)  inz
009200060810      *
009300060810      * - Variabili definite a programma
009400060811     d wDate           s               d   datfmt(*iso)  inz(*loval)
009500060810     d wPage           s              4  0 inz
009600060810     d wRow            s              3  0 inz
009700060810
009800060810      *---------------------------------------------------------------*
009900060810      *   M A I N   L I N E                                           *
010000060810      *---------------------------------------------------------------*
010100060810      *  Riepilogo indicatori utilizzati:                             *
010200060810      *  --------------------------------                             *
010300060810      *  10 - Comodo                                                  *
010400060810      *  22 - Errori in scrittura record (WRITE)                      *
010500060810      *  28 - Emissione messaggio di errore a video                   *
010600060810      *  40 - Record in modifica                                      *
010700060901      *  41 - Campo V1CKE1 in dspatr(BL) per scaglioni mancanti       *
010800060901      *  42 - Campo V1CKE1 in dspatr(BL) per scaglioni errati         *
010900060810      *  50 - ERR: Flag annullamento errato                           *
011000060811     *** *  51 - ERR: Prezzo minimo  del gasolio errato                  *
011100060810      *  52 - ERR: Prezzo massimo del gasolio errato                  *
011200060810      *  90 - Riemissione videata                                     *
011300060810      *---------------------------------------------------------------*
011400060810      *
011500060810      * Operazioni iniziali
011600060810     c                   exsr      RoutInz
011700060810      *
011800060810      * Gestione video
011900060810     c                   dow       $Fine   = *off
012000060810     c                   exsr      GesS01
012100060810     c                   enddo
012200060810      *
012300060810      * Fine
012400060810     c                   eval      *inLR   = *on
012500060810
012600060810      *---------------------------------------------------------------*
012700060810      * RoutInz - Operazioni Iniziali                                 *
012800060810      *---------------------------------------------------------------*
012900060810     c     RoutInz       BEGSR
013000060810      *
013100060810      * Ricezione parametri
013200060810     c     *entry        plist
013300060810     c                   parm                    KPJBA
013400060810      *
013500060810      * Definizioni chiavi di accesso
013600060810     c     K05TBE01      klist                                                  *tntbe01l
013700060810     c                   kfld                    TBEcod                         -tabella
013800060810     c                   kfld                    TBEke1                         -chiave uno
013900060810     c                   kfld                    TBEke2                         -chiave due
014000060810     c                   kfld                    TBElin                         -lingua
014100060810     c                   kfld                    TBEsif                         -s.informativo
014200060810      *
014300060810      * Reperisco la data odierna
014400060810     c                   movel     *date         Wdate
014500060810      *
014600060810      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
014700060810     c     *dtaara       define    §azute        AZUTEds
014800060810     c     *dtaara       define    §datiute      dDatiUte
014900060810      *
015000060810     c                   clear                   AZUTEds
015100060810     c                   clear                   dDatiUte
015200060810     c                   clear                   TIBS34ds
015300060810     c                   in(E)     *dtaara
015400060810if  1c                   if        %Error  or  RSUT = *blanks
015500060810     c                   call      'TIBS34R'
015600060810     c                   parm                    TIBS34ds
015700060810     c                   in        *dtaara
015800060810e   1c                   endif
015900060810      *-- Verifica errori e autorità profilo
016000060810sel 1c                   SELECT
016100060810      *-- controllo se ho errori nei dati utente
016200060810      *--   nel qual caso NON risulta un profilo abilitato
016300060810w   1c                   WHEN      DUTerr  = 'E'
016400060810     c                   eval      $Fine   = *on
016500060810      *
016600060810      *-- CONTROLLO AUTORITA'
016700060810      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
016800060810      *-- se il chiamante non richiede autorità specifica verificare
016900060810      *--   quella del profilo
017000060810      *-- se il chiamante richiede autorità specifica verificarla,
017100060810      *--  se è blank verificare quella del profilo
017200060810      *
017300060810      * se UTEAUT = *BLANK non siamo in sede
017400060810w   1c                   WHEN      UTEaut  = *blank
017500060810      *
017600060810x   1c                   OTHER
017700060810      *
017800060810e   1c                   ENDSL
017900060810      *
018000060810      * Aggancio dati generali della tabella in esame
018100060810     c                   clear                   TBEcod
018200060810     c                   move      *zeros        TBEke1
018300060810     c                   move      C_CodTab      TBEke1
018400060810     c                   clear                   TBEke2
018500060810     c                   clear                   TBElin
018600060810     c                   movel     KNSIF         TBEsif
018700060810     c     K05TBE01      chain     TNTBE01L
018800060810     c                   if        not %found(TNTBE01L)
018900060810     c                   clear                   TBEsif
019000060810     c     K05TBE01      chain     TNTBE01L
019100060810     c                   endif
019200060810     c                   if        %found(TNTBE01L)
019300060810     c                   movel     TNTBEds       xTNTBEds
019400060810     c                   else
019500060810     c                   clear                   xTNTBEds
019600060810     c                   endif
019700060810      *
019800060810     c                   ENDSR
019900060810
020000060810      *---------------------------------------------------------------*
020100060810      * GESS01 - Gestione subfile                                     *
020200060810      *---------------------------------------------------------------*
020300060810     c     GesS01        BEGSR
020400060810      *
020500060810      * Inizializzazione videata
020600060810if  1c                   if        $InzS01 = *on
020700060810     c                   exsr      InzS01
020800060810     c                   movel     *off          $InzS01
020900060810e   1c                   endif
021000060810      *
021100060810      * Scrivo la testata e la riga tasti funzionali abilitati
021200060810if  1c                   if        NOT *in90
021300060810     c                   write     TB58T01
021400060810     c                   write     TB58P01
021500060810e   1c                   endif
021600060810      *
021700060810      * Posiziono il cursore
021800060810w   1c                   if        C01csr  > *zeros
021900060810     c                   eval      C01rcd  = C01csr
022000060810     c                   else
022100060905     c                   eval      C01rcd  = 1
022200060810     c                   endif
022300060810     c                   eval      *in30       = (C01rcd <= *zeros)
022400060810      *
022500060810      * Emetto la videata
022600060810     c                   exfmt     TB58C01
022700060810      *
022800060810     c                   setoff                                       28  90
022900060810     c                   clear                   V1Dmsg
023000060810      *
023100060810sel 1c                   SELECT
023200060810      *
023300060810      * F3=Fine
023400060810w   1c                   WHEN      *inKC
023500060810     c                   exsr      F03S01
023600060810      *
023700060810      * Roll-Up
023800060810w   1c                   WHEN      £Tasto  = RollUp
023900060810do  2c                   do        C_SflPag
024000060810     c                   exsr      RecS01
024100060810if  3c                   if        *in33
024200060810     c                   leave
024300060810e   3c                   endif
024400060810e   2c                   enddo
024500060810     c     S01nrr        div       C_SflPag      wPage
024600060810     c                   mvr                     wRow
024700060811     c                   if        wRow    = *zeros
024800060810     c                   eval      C01csr  = (C_SflPag * (wPage - 1))
024900060810     c                                     + 1
025000060811     c                   else
025100060811     c                   eval      C01csr  = (C_SflPag * wPage)
025200060811     c                                     + 1
025300060811     c                   endif
025400060810      *
025500060810      * Controllo dati immessi a video
025600060810x   1c                   OTHER
025700060810     c                   exsr      CtrC01
025800060810      * - Rilevati Errori
025900060810if  2c                   if        *in90
026000060810     c                   leavesr
026100060810e   2c                   endif
026200060810      * F6=Conferma
026300060810if  2c                   if        *inKF
026400060810     c                   exsr      F06S01
026500060810     c                   eval      $Fine   = *on
026600060810e   2c                   endif
026700060810      *
026800060810e   1c                   ENDSL
026900060810      *
027000060810     c                   ENDSR
027100060810
027200060810      *---------------------------------------------------------------*
027300060810      * INZS01 - Inizializzazione subfile (S01)                       *
027400060810      *---------------------------------------------------------------*
027500060810     c     InzS01        BEGSR
027600060810      *
027700060810      * Pulizia subfile
027800060810     c                   seton                                        3031
027900060810     c                   write     TB58C01
028000060810     c                   setoff                                         31
028100060810     c                   eval      *in33   = *off
028200060810      *
028300060810     c                   clear                   C01rcd
028400060810     c                   clear                   C01csr
028500060811     c                   clear                   C01txt
028600060810     c                   clear                   V1Dmsg
028700060810     c                   setoff                                       28  90
028800060811     c                   movea     *zeros        *in(40)
028900060810      *
029000060810      * Caricamento dati già immessi (protetti)
029100060810     c     C_CodTab      setll     TNTBE000
029200060810     c     C_CodTab      reade     TNTBE000
029300060810do  1c                   DOW       NOT  %eof(TNTBE01L)
029400060810     c                             and  NOT *in33
029500060905if  2c*** anche ann.:    if        TBEatb  = *blanks
029600060905     c                   exsr      RecS01
029700060905e   2c***                endif
029800060810     c     C_CodTab      reade     TNTBE000
029900060810e   1c                   ENDDO
030000060810      *
030100060810      * Aggiunta di righe libere a completamento della pagina
030200060810     c     S01nrr        div       C_SflPag      wPage
030300060810     c                   mvr                     wRow
030400060810do  1c                   dow       S01nrr  < (C_SflPag * (wPage + 1))
030500060810     c                             and  NOT *in33
030600060810     c                   exsr      RecS01
030700060810e   1c                   enddo
030800060810      *
030900060905      * Imposta il posizionamento cursore sul 1° rec libero del sfl
031000060811if  1c                   if        C01csr  > *zeros
031100060905     c                   eval      C01csr  = 1
031200060810e   1c                   endif
031300060901      *
031400060901      * Impostazione eventuale testo di avvertimento...
031500060901     c                   exsr      sr_TxtC01
031600060810      *
031700060810     c                   ENDSR
031800060810
031900060810      *---------------------------------------------------------------*
032000060810      * RECS01 - Caricamento singolo record del subfile S01           *
032100060810      *---------------------------------------------------------------*
032200060810     c     RecS01        BEGSR
032300060810      *
032400060901if  1c                   if        NOT  %eof(TNTBE01L)
032500060901     c                   movel     TBEke1        W1Cke1
032600060901x   1c                   else
032700060901     c                   clear                   W1Cke1
032800060901e   1c                   endif
032900060901     c                   add       1             wKE1prg
033000060901      *
033100060810     c                   clear                   TB58S01
033200060901     c                   eval      V1Cke1  = wKE1prg
033300060810      *
033400060901sel 1c                   SELECT
033500060810      *
033600060810      * Record già esistente
033700060901w   1c                   WHEN      NOT  %eof(TNTBE01L)
033800060901     c                             and  W1Cke1 = V1Cke1
033900060810     c                   eval      H1Ctbe  = *on
034000060810     c                   eval      V1Catb  = TBEatb
034100060810     c                   movel     TBEuni        dIPG
034200060810     c                   eval      V1Cmin  = §IPGmin
034300060810     c                   eval      V1Cmax  = §IPGmax
034400060811     c                   eval      V1Cum   = 'Euro/Litro'
034500060901     c                   eval      $IPGmax(V1Cke1) = §IPGmax
034600060901      *
034700060901      * Record mancante (possibile ???)
034800060901w   1c                   WHEN      NOT  %eof(TNTBE01L)
034900060901     c                             and  W1Cke1 > wKE1prg
035000060901     c                   eval      H1Ctbe  = *off
035100060901do  2c                   dou       W1Cke1  = wKE1prg
035200060901     c                   exsr      WrtS01
035300060901     c                   add       1             wKE1prg
035400060901     c                   eval      V1Cke1  = wKE1prg
035500060901e   2c                   enddo
035600060810      *
035700060810      * Record nuovo (vuoto)
035800060901w   1c                   WHEN      %eof(TNTBE01L)
035900060810     c                   eval      H1Ctbe  = *off
036000060810      *
036100060901e   1c                   ENDSL
036200060901      *
036300060901      * Scrittura singolo record nel subfile S01
036400060901     c                   exsr      WrtS01
036500060901      *
036600060901     c                   ENDSR
036700060901
036800060901      *---------------------------------------------------------------*
036900060901      * WRTS01 - Scrittura singolo record nel subfile S01             *
037000060901      *---------------------------------------------------------------*
037100060901     c     WrtS01        BEGSR
037200060901      *
037300060810      * Attributi di visualizzazione
037400060810      * - record già immesso
037500060810     c                   eval      *in40   = (H1Ctbe = *on)
037600060901      * - scaglione mancante - IMPOSSIBILE !!!
037700060901     c                   eval      *in41   = (wKE1prg > 1       and
037800060901     c                                        H1Ctbe  = *on     and
037900060901     c                                        V1Catb  = *blanks and
038000060811     c                                        V1Cmin <>
038100060811     c                                          $IPGmax(wKE1prg-1)
038200060811     c                                          + 0,001)
038300060811     c                   eval      H1in41  = *in41
038400060811if  1c                   if        *in41   = *on
038500060811     c                   eval      SAVin41 = *on
038600060811e   1c                   endif
038700060828      * - scaglione errato
038800060901     c                   eval      *in42   = (wKE1prg > 1       and
038900060901     c                                        H1Ctbe  = *on     and
039000060901     c***                                     V1Catb  = *blanks and
039100060828     c                                        V1Cmin  > V1Cmax)
039200060828     c                   eval      H1in42  = *in42
039300060828if  1c                   if        *in42   = *on
039400060828     c                   eval      SAVin42 = *on
039500060828e   1c                   endif
039600060905      *
039700060905      * Impostazione posizionamento del cursore sul campo di I/O
039800060905      *   del 1° record libero nel sfl
039900060905     c                   eval      *in52   = (H1Ctbe = *off)
040000060810      *
040100060905     c                   eval      *in32   = *off
040200060901     c                   z-add     wKE1prg       S01nrr
040300060810     c                   write     TB58S01
040400060810      *
040500060810      * Impostazione del SflEnd
040600060901     c                   eval      *in33   = (wKE1prg = *hival)
040700060810      *
040800060810     c                   ENDSR
040900060901
041000060901      *---------------------------------------------------------------*
041100060901      * sr_TxtC01 - Impostazione testo di avvertimento in C01         *
041200060901      *---------------------------------------------------------------*
041300060901     c     sr_TxtC01     BEGSR
041400060901      *
041500060901     c                   clear                   C01txt
041600060901     c                   eval      C01txtA = C_Atr_Norm
041700060901      *
041800060901sel 1c                   select
041900060901      * Visualizza avvertimento relativi a scaglioni mancanti
042000060901w   1c                   when           SAVin41 = *on
042100060901     c                             and  SAVin42 = *off
042200060901     c                   eval      C01txt  = 'Rilevata mancanza di sca+
042300060905     c                                       glioni!                  +
042400060905     c                                                                +
042500060905     c                                           '
042600060901     c                   eval      C01txtA = C_Atr_BL_RI
042700060901      * Visualizza avvertimento relativi a scaglioni errati
042800060901w   1c                   when      SAVin42 = *on
042900060901     c                   eval      C01txt  = 'Rilevati scaglioni errat+
043000060905     c                                       i!                       +
043100060905     c                                                                +
043200060905     c                                           '
043300060901     c                   eval      C01txtA = C_Atr_BL_RI
043400060905e   1c                   endsl
043500060901      *
043600060901     c                   ENDSR
043700060810
043800060810      *---------------------------------------------------------------*
043900060810      * CTRC01 - Controllo di tutto il subfile                        *
044000060810      *---------------------------------------------------------------*
044100060810     c     CtrC01        BEGSR
044200060810      *
044300060810     c                   readc     TB58S01
044400060810      *
044500060810do  1c                   DOW       NOT %eof(TNTB58D)
044600060810      *
044700060901     c*** già così:      eval      *in32   = *off
044800060810     c                   movea     *zeros        *in(50)
044900060810     c                   z-add     S01nrr        C01rcd
045000060810      *
045100060810     c                   exsr      CtrS01
045200060810      *
045300060810      * Reimpostazione indicatori per DSPATR
045400060810      * - record già immesso
045500060810     c                   eval      *in40   = (H1Ctbe = *on)
045600060901      * - scaglione (precedente) mancante - IMPOSSIBILE !!!
045700060811     c                   eval      *in41   = (H1in41 = *on)
045800060901      * - scaglione errato
045900060901     c                   eval      *in42   = (H1in42 = *on)
046000060810      *
046100060810      * Aggiornamento sfl
046200060810     c                   eval      *in32   = *on
046300060810if  2c                   if        *in28  OR  *in90
046400060810     c                   z-add     C01rcd        C01csr
046500060810e   2c                   endif
046600060810     c                   UPDATE    TB58S01
046700060810if  2c                   if        *in28  OR  *in90
046800060810     c                   leave
046900060810e   2c                   endif
047000060810      *
047100060810     c                   readc     TB58S01
047200060810      *
047300060810e   1c                   ENDDO
047400060901      *
047500060901      * Impostazione eventuale testo di avvertimento...
047600060901     c                   exsr      sr_TxtC01
047700060810      *
047800060810     c                   ENDSR
047900060810
048000060810      *---------------------------------------------------------------*
048100060810      * CTRS01 - Controllo del singolo record del subfile             *
048200060810      *---------------------------------------------------------------*
048300060810     c     CtrS01        BEGSR
048400060810      *
048500060810     c                   movea     *zeros        *in(50)
048600060810      *
048700060810      * Numero scaglione
048800060811      * (già impostato automaticamente)
048900060901      *
049000060901      * Scaglione prima immesso poi rimosso...
049100060901     c                   if             V1Cmax <= *zeros
049200060901     c                   clear                   V1Cmin
049300060901     c                   clear                   V1Cmax
049400060901     c                   endif
049500060811      *
049600060811      * Sequenza errata (lasciati scaglioni vuoti)
049700060811if  1c                   if             V1Cke1  > 1
049800060811     c                             and  V1Cmax <> *zeros
049900060811     c                             and  $IPGmax(V1Cke1-1) = *zeros
050000060811     c                   seton                                        285290
050100060811     c                   eval      V1Dmsg  = $Msg(6)
050200060811     c                   leavesr
050300060811e   1c                   endif
050400060810      *
050500060810      * Prezzo Massimo
050600060810if  2c                   if        V1Cmax  < *zeros
050700060810     c                   seton                                        285290
050800060810     c                   eval      V1Dmsg  = $Msg(2)
050900060810     c                   leavesr
051000060810e   1c                   endif
051100060810      *
051200060828      * Range prezzi
051300060828if  2c                   if        V1Cmin  > V1Cmax
051400060828     c                   seton                                        285190
051500060828     c                   eval      V1Dmsg  = $Msg(3)
051600060828     c                   leavesr
051700060828e   1c                   endif
051800060811      *
051900060811     c                   clear                   V1Cmin
052000060811     c                   clear                   V1Cum
052100060811     c                   eval      $IPGmax(V1Cke1) = *zeros
052200060810      *
052300060810      *?SEGUONO CONTROLLI DA ESEGUIRE SOLO PER I RECORDS CON DATI?
052400060810if  1c                   if             V1Cmin  = *zeros
052500060810     c                             and  V1Cmax  = *zeros
052600060810     c                   leavesr
052700060810e   1c                   endif
052800060811      *
052900060811     c                   eval      V1Cum   = 'Euro/Litro'
053000060810      *
053100060811      * Prezzo massimo già compreso in altro scaglione
053200060810     c                   eval      xx       = 1
053300060811     c     V1Cmax        lookup    $IPGmax(xx)                        10  10
053400060810      * - Scaglione reperito
053500060811if  1c                   if        *in10
053600060811     c                             and  V1Cke1  > xx
053700060811     c                   seton                                        285290
053800060810     c                   eval      V1Dmsg   = %trim($Msg(4))
053900060810     c                                      + ' '
054000060810     c                                      + %trim(%editc(xx : 'Z'))
054100060810     c                   leavesr
054200060810e   1c                   endif
054300060811      *
054400060811      * Ricalcolo prezzo minimo
054500060811if  1c                   if             V1Cke1  > 1
054600060811     c                             and  V1Cmax <> *zeros
054700060901     c                   eval      V1Cmin  = $IPGmax(V1Cke1-1) + 0,001
054800060811e   1c                   endif
054900060811      *
055000060811      * Aggiornamento schiera prezzi massimi x scaglione
055100060811     c                   eval      $IPGmax(V1Cke1) = V1Cmax
055200060810      *
055300060810     c                   ENDSR
055400060810
055500060810      *---------------------------------------------------------------*
055600060810      * F03S01 - Tasto funzionale F03 -> Fine programma               *
055700060810      *---------------------------------------------------------------*
055800060810     c     F03S01        BEGSR
055900060810      *
056000060810     c                   movel     *on           $Fine                          fine pgm
056100060810      *
056200060810     c                   ENDSR
056300060810
056400060810      *---------------------------------------------------------------*
056500060810      * F06S01 * Elaborazione subfile per aggiornamento tabella       *
056600060810      *---------------------------------------------------------------*
056700060810     c     F06S01        BEGSR
056800060810      *
056900060811     c                   clear                   S01nrr
057000060811      *
057100060811do  1c                   DO        *hival        S01nrr
057200060811      *
057300060811     c     S01nrr        chain     TB58S01
057400060810      *
057500060811      * Fine ciclo di lettura sfl
057600060811if  2c                   if        NOT %found(TNTB58D)
057700060811     c                   leave
057800060811e   2c                   endif
057900060811      * Fine dati immessi a video
058000060811if  2c                   if             H1Ctbe  = *off
058100060811     c                             and  V1Cmax  = *zeros
058200060811     c                   leave
058300060811e   2c                   endif
058400060905      * Record già inserito (solo in visualizzazione)
058500060905if  2c                   if        H1Ctbe  = *on
058600060905     c                   iter
058700060905e   2c                   endif
058800060905      *
058900060811      * Record nuovo
059000060811     c                   movel     C_CodTab      TBEcod
059100060901     c                   movel(p)  V1Cke1        TBEke1
059200060810     c                   clear                   TBEke2
059300060810     c                   clear                   TBElin
059400060810     c                   if        TBXsif  <> *blanks
059500060810     c                   movel     KNSIF         TBEsif
059600060810     c                   else
059700060810     c                   clear                   TBEsif
059800060810     c                   endif
059900060810     c     K05TBE01      chain     TNTBE000
060000060810      *
060100060810     c                   exsr      AggTBE
060200060810      *
060300060810e   1c                   ENDDO
060400060810      *
060500060810     c                   ENDSR
060600060810
060700060810      *---------------------------------------------------------------*
060800060810      * AGGTBE * Aggiornamento tabella (singolo record)               *
060900060810      *---------------------------------------------------------------*
061000060810     c     AggTBE        BEGSR
061100060810      *
061200060811if  1c                   IF        %found(TNTBE01L)
061300060811      *
061400060811      * Aggiornamento scaglione (annullato rec. precedente)
061500060811     c                   exsr      RieDS
061600060901     c                   eval      TBEatb  =  V1Catb
061700060811     c                   eval      TBEftt  =  TBXftt
061800060810     c                   clear                   TBEftr
061900060810     c                   UPDATE    TNTBE000
062000060810      *
062100060811x   1c                   ELSE
062200060811      * Immissione scaglione
062300060810     c                   exsr      RieTBE
062400060811     c                   eval      TBEftt  =  TBXftt
062500060810     c                   clear                   TBEflt
062600060810     c                   clear                   TBEdtr
062700060811     c*** 22 not tested: WRITE     TNTBE000                             22
062800060811     c                   WRITE     TNTBE000
062900060810      *
063000060811e   1c                   ENDIF
063100060810     c
063200060810     c                   ENDSR
063300060810
063400060810      *---------------------------------------------------------------*
063500060810      * RIETBE * Riempimento dati tabella per IMMISSIONE              *
063600060810      *---------------------------------------------------------------*
063700060810     c     RieTBE        BEGSR
063800060810      *
063900060810     c                   clear                   TNTBE000
064000060810      *
064100060810     c*** già così:      clear                   TBEatb
064200060811     c                   if        TBXsif  <> *blanks
064300060810     c                   movel     KNSIF         TBEsif
064400060810     c                   else
064500060810     c                   clear                   TBEsif
064600060810     c                   endif
064700060810     c                   clear                   TBElin
064800060810     c                   movel     TBXapl        TBEapl
064900060810     c                   movel     C_CodTab      TBEcod
065000060905     c                   movel(p)  V1Cke1        TBEke1
065100060810     c*** già così:      clear                   TBEke2
065200060810      *
065300060811     c                   exsr      RieDS
065400060811     c*** già fatto:     movel(p)  dIPG          TBEuni
065500060810      *
065600060810     c*** già così:      clear                   TBEftt
065700060810     c*** già così:      clear                   TBEflt
065800060810     c*** già così:      clear                   TBEftr
065900060810     c*** già così:      clear                   TBEdtr
066000060810      *
066100060810     c                   ENDSR
066200060811
066300060811      *---------------------------------------------------------------*
066400060811      * RIEDS  * Riempimento struttura dati                           *
066500060811      *---------------------------------------------------------------*
066600060811     c     RieDS         BEGSR
066700060811      *
066800060811     c                   clear                   dIPG
066900060811      *
067000060901sel 1c                   select
067100060905      * - scaglione successivo al 1°
067200060905w   1c                   when      V1Cke1  >  1
067300060905     c                   eval      §IPGmin =  $IPGmax(V1Cke1 - 1)
067400060901     c                                     +  0,001
067500060905      * - 1° scaglione
067600060905w   1c                   when      V1Cke1 <= 1
067700060811     c                   eval      §IPGmin =  *zeros
067800060905      * - record "manualmente" annullato (???)
067900060905w   1c                   other
068000060905     c                   eval      §IPGmin =  V1Cmin
068100060901e   1c                   endsl
068200060811     c                   eval      §IPGmax =  V1Cmax
068300060811      *
068400060811     c                   movel(p)  dIPG          TBEuni
068500060811      *
068600060811     c                   ENDSR
068700060810
068800060810** - $Msg -------------------------------------------------------------------*
068900060905     ...libero...                                                               1
069000060810Valore errato                                                                   2
069100060810Prezzo minimo > Prezzo massimo                                                  3
069200060811Prezzo massimo già compreso nello scaglione precedente                          4
069300060811Prezzo minimo non corretto. E' previsto                                         5
069400060811Sequenza scaglioni errata: NON ammessi "vuoti" tra uno scaglione e l'altro      6
