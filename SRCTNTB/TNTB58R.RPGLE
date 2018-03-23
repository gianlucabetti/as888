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
009700061221     d wOpz            s              2    inz
009800060810
009900060810      *---------------------------------------------------------------*
010000060810      *   M A I N   L I N E                                           *
010100060810      *---------------------------------------------------------------*
010200060810      *  Riepilogo indicatori utilizzati:                             *
010300060810      *  --------------------------------                             *
010400061221      *  06 - F6 abilitato                                            *
010500060810      *  10 - Comodo                                                  *
010600060810      *  22 - Errori in scrittura record (WRITE)                      *
010700061221      *  28 - Emissione messaggio di errore a video                   *
010800061221      *  30 - Pulizia subfile                                         *
010900061221      *  31 - NON emissione del subfile                               *
011000061221      *  32 - Record modificato nel subfile (SFLNXTCHG)               *
011100061221      *  33 - Fine dati nel subfile         (SFLEND)                  *
011200060810      *  40 - Record in modifica                                      *
011300060901      *  41 - Campo V1CKE1 in dspatr(BL) per scaglioni mancanti       *
011400060901      *  42 - Campo V1CKE1 in dspatr(BL) per scaglioni errati         *
011500060810      *  50 - ERR: Flag annullamento errato                           *
011600060811     *** *  51 - ERR: Prezzo minimo  del gasolio errato                  *
011700060810      *  52 - ERR: Prezzo massimo del gasolio errato                  *
011800060810      *  90 - Riemissione videata                                     *
011900060810      *---------------------------------------------------------------*
012000060810      *
012100060810      * Operazioni iniziali
012200060810     c                   exsr      RoutInz
012300060810      *
012400060810      * Gestione video
012500060810     c                   dow       $Fine   = *off
012600060810     c                   exsr      GesS01
012700060810     c                   enddo
012800060810      *
012900060810      * Fine
013000060810     c                   eval      *inLR   = *on
013100060810
013200060810      *---------------------------------------------------------------*
013300060810      * RoutInz - Operazioni Iniziali                                 *
013400060810      *---------------------------------------------------------------*
013500060810     c     RoutInz       BEGSR
013600060810      *
013700060810      * Ricezione parametri
013800060810     c     *entry        plist
013900060810     c                   parm                    KPJBA
014000060810      *
014100060810      * Definizioni chiavi di accesso
014200060810     c     K05TBE01      klist                                                  *tntbe01l
014300060810     c                   kfld                    TBEcod                         -tabella
014400060810     c                   kfld                    TBEke1                         -chiave uno
014500060810     c                   kfld                    TBEke2                         -chiave due
014600060810     c                   kfld                    TBElin                         -lingua
014700060810     c                   kfld                    TBEsif                         -s.informativo
014800061221      *
014900061221      * Imposto eventuali parametri ricevuti
015000061221     c                   if        %subst(KPJBU:1:2) > *zeros
015100061221     c                   movel     KPJBU         wOpz
015200061221     c                   endif
015300060810      *
015400060810      * Reperisco la data odierna
015500060810     c                   movel     *date         Wdate
015600060810      *
015700060810      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
015800060810     c     *dtaara       define    §azute        AZUTEds
015900060810     c     *dtaara       define    §datiute      dDatiUte
016000060810      *
016100060810     c                   clear                   AZUTEds
016200060810     c                   clear                   dDatiUte
016300060810     c                   clear                   TIBS34ds
016400060810     c                   in(E)     *dtaara
016500060810if  1c                   if        %Error  or  RSUT = *blanks
016600060810     c                   call      'TIBS34R'
016700060810     c                   parm                    TIBS34ds
016800060810     c                   in        *dtaara
016900060810e   1c                   endif
017000060810      *-- Verifica errori e autorità profilo
017100060810sel 1c                   SELECT
017200060810      *-- controllo se ho errori nei dati utente
017300060810      *--   nel qual caso NON risulta un profilo abilitato
017400060810w   1c                   WHEN      DUTerr  = 'E'
017500060810     c                   eval      $Fine   = *on
017600060810      *
017700060810      *-- CONTROLLO AUTORITA'
017800060810      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
017900060810      *-- se il chiamante non richiede autorità specifica verificare
018000060810      *--   quella del profilo
018100060810      *-- se il chiamante richiede autorità specifica verificarla,
018200060810      *--  se è blank verificare quella del profilo
018300060810      *
018400060810      * se UTEAUT = *BLANK non siamo in sede
018500060810w   1c                   WHEN      UTEaut  = *blank
018600060810      *
018700060810x   1c                   OTHER
018800060810      *
018900060810e   1c                   ENDSL
019000060810      *
019100060810      * Aggancio dati generali della tabella in esame
019200060810     c                   clear                   TBEcod
019300060810     c                   move      *zeros        TBEke1
019400060810     c                   move      C_CodTab      TBEke1
019500060810     c                   clear                   TBEke2
019600060810     c                   clear                   TBElin
019700060810     c                   movel     KNSIF         TBEsif
019800060810     c     K05TBE01      chain     TNTBE01L
019900060810     c                   if        not %found(TNTBE01L)
020000060810     c                   clear                   TBEsif
020100060810     c     K05TBE01      chain     TNTBE01L
020200060810     c                   endif
020300060810     c                   if        %found(TNTBE01L)
020400060810     c                   movel     TNTBEds       xTNTBEds
020500060810     c                   else
020600060810     c                   clear                   xTNTBEds
020700060810     c                   endif
020800060810      *
020900060810     c                   ENDSR
021000060810
021100060810      *---------------------------------------------------------------*
021200060810      * GESS01 - Gestione subfile                                     *
021300060810      *---------------------------------------------------------------*
021400060810     c     GesS01        BEGSR
021500060810      *
021600060810      * Inizializzazione videata
021700060810if  1c                   if        $InzS01 = *on
021800060810     c                   exsr      InzS01
021900060810     c                   movel     *off          $InzS01
022000060810e   1c                   endif
022100060810      *
022200060810      * Scrivo la testata e la riga tasti funzionali abilitati
022300060810if  1c                   if        NOT *in90
022400060810     c                   write     TB58T01
022500060810     c                   write     TB58P01
022600060810e   1c                   endif
022700060810      *
022800060810      * Posiziono il cursore
022900060810w   1c                   if        C01csr  > *zeros
023000060810     c                   eval      C01rcd  = C01csr
023100060810     c                   else
023200060905     c                   eval      C01rcd  = 1
023300060810     c                   endif
023400060810     c                   eval      *in30       = (C01rcd <= *zeros)
023500060810      *
023600060810      * Emetto la videata
023700060810     c                   exfmt     TB58C01
023800060810      *
023900060810     c                   setoff                                       28  90
024000060810     c                   clear                   V1Dmsg
024100060810      *
024200060810sel 1c                   SELECT
024300060810      *
024400060810      * F3=Fine
024500060810w   1c                   WHEN      *inKC
024600060810     c                   exsr      F03S01
024700060810      *
024800060810      * Roll-Up
024900060810w   1c                   WHEN      £Tasto  = RollUp
025000060810do  2c                   do        C_SflPag
025100060810     c                   exsr      RecS01
025200060810if  3c                   if        *in33
025300060810     c                   leave
025400060810e   3c                   endif
025500060810e   2c                   enddo
025600060810     c     S01nrr        div       C_SflPag      wPage
025700060810     c                   mvr                     wRow
025800060811     c                   if        wRow    = *zeros
025900060810     c                   eval      C01csr  = (C_SflPag * (wPage - 1))
026000060810     c                                     + 1
026100060811     c                   else
026200060811     c                   eval      C01csr  = (C_SflPag * wPage)
026300060811     c                                     + 1
026400060811     c                   endif
026500060810      *
026600060810      * Controllo dati immessi a video
026700060810x   1c                   OTHER
026800060810     c                   exsr      CtrC01
026900060810      * - Rilevati Errori
027000060810if  2c                   if        *in90
027100060810     c                   leavesr
027200060810e   2c                   endif
027300060810      * F6=Conferma
027400061221if  2c                   if        *inKF
027500060810     c                   exsr      F06S01
027600060810     c                   eval      $Fine   = *on
027700060810e   2c                   endif
027800060810      *
027900060810e   1c                   ENDSL
028000060810      *
028100060810     c                   ENDSR
028200060810
028300060810      *---------------------------------------------------------------*
028400060810      * INZS01 - Inizializzazione subfile (S01)                       *
028500060810      *---------------------------------------------------------------*
028600060810     c     InzS01        BEGSR
028700060810      *
028800060810      * Pulizia subfile
028900060810     c                   seton                                        3031
029000060810     c                   write     TB58C01
029100060810     c                   setoff                                         31
029200060810     c                   eval      *in33   = *off
029300060810      *
029400060810     c                   clear                   C01rcd
029500060810     c                   clear                   C01csr
029600060811     c                   clear                   C01txt
029700060810     c                   clear                   V1Dmsg
029800060810     c                   setoff                                       28  90
029900060811     c                   movea     *zeros        *in(40)
030000060810      *
030100060810      * Caricamento dati già immessi (protetti)
030200060810     c     C_CodTab      setll     TNTBE000
030300060810     c     C_CodTab      reade     TNTBE000
030400060810do  1c                   DOW       NOT  %eof(TNTBE01L)
030500060810     c                             and  NOT *in33
030600060905if  2c*** anche ann.:    if        TBEatb  = *blanks
030700060905     c                   exsr      RecS01
030800060905e   2c***                endif
030900060810     c     C_CodTab      reade     TNTBE000
031000060810e   1c                   ENDDO
031100061221      *
031200061221      * Se pgm. richiamato in interrogazione NON consento l'immissione
031300061221      *   di altri dati
031400061221if  0c                   IF        wOpz    = '05'
031500061221      *
031600061221     c                   eval      *in33   = *on
031700061221      *
031800061221x   0c                   ELSE
031900060810      *
032000060810      * Aggiunta di righe libere a completamento della pagina
032100060810     c     S01nrr        div       C_SflPag      wPage
032200060810     c                   mvr                     wRow
032300060810do  1c                   dow       S01nrr  < (C_SflPag * (wPage + 1))
032400060810     c                             and  NOT *in33
032500060810     c                   exsr      RecS01
032600060810e   1c                   enddo
032700060810      *
032800060905      * Imposta il posizionamento cursore sul 1° rec libero del sfl
032900060811if  1c                   if        C01csr  > *zeros
033000060905     c                   eval      C01csr  = 1
033100060810e   1c                   endif
033200061221      *
033300061221e   0c                   ENDIF
033400060901      *
033500060901      * Impostazione eventuale testo di avvertimento...
033600060901     c                   exsr      sr_TxtC01
033700061221      *
033800061221      * Abilitazione tasti funzionali
033900061221      * - F6 = Conferma
034000061221     c                   eval      *in06   = (wOpz <> '05')
034100060810      *
034200060810     c                   ENDSR
034300060810
034400060810      *---------------------------------------------------------------*
034500060810      * RECS01 - Caricamento singolo record del subfile S01           *
034600060810      *---------------------------------------------------------------*
034700060810     c     RecS01        BEGSR
034800060810      *
034900060901if  1c                   if        NOT  %eof(TNTBE01L)
035000060901     c                   movel     TBEke1        W1Cke1
035100060901x   1c                   else
035200060901     c                   clear                   W1Cke1
035300060901e   1c                   endif
035400060901     c                   add       1             wKE1prg
035500060901      *
035600060810     c                   clear                   TB58S01
035700060901     c                   eval      V1Cke1  = wKE1prg
035800060810      *
035900060901sel 1c                   SELECT
036000060810      *
036100060810      * Record già esistente
036200060901w   1c                   WHEN      NOT  %eof(TNTBE01L)
036300060901     c                             and  W1Cke1 = V1Cke1
036400060810     c                   eval      H1Ctbe  = *on
036500060810     c                   eval      V1Catb  = TBEatb
036600060810     c                   movel     TBEuni        dIPG
036700060810     c                   eval      V1Cmin  = §IPGmin
036800060810     c                   eval      V1Cmax  = §IPGmax
036900060811     c                   eval      V1Cum   = 'Euro/Litro'
037000060901     c                   eval      $IPGmax(V1Cke1) = §IPGmax
037100060901      *
037200060901      * Record mancante (possibile ???)
037300060901w   1c                   WHEN      NOT  %eof(TNTBE01L)
037400060901     c                             and  W1Cke1 > wKE1prg
037500060901     c                   eval      H1Ctbe  = *off
037600060901do  2c                   dou       W1Cke1  = wKE1prg
037700060901     c                   exsr      WrtS01
037800060901     c                   add       1             wKE1prg
037900060901     c                   eval      V1Cke1  = wKE1prg
038000060901e   2c                   enddo
038100060810      *
038200060810      * Record nuovo (vuoto)
038300060901w   1c                   WHEN      %eof(TNTBE01L)
038400060810     c                   eval      H1Ctbe  = *off
038500060810      *
038600060901e   1c                   ENDSL
038700060901      *
038800060901      * Scrittura singolo record nel subfile S01
038900060901     c                   exsr      WrtS01
039000060901      *
039100060901     c                   ENDSR
039200060901
039300060901      *---------------------------------------------------------------*
039400060901      * WRTS01 - Scrittura singolo record nel subfile S01             *
039500060901      *---------------------------------------------------------------*
039600060901     c     WrtS01        BEGSR
039700060901      *
039800060810      * Attributi di visualizzazione
039900060810      * - record già immesso
040000060810     c                   eval      *in40   = (H1Ctbe = *on)
040100060901      * - scaglione mancante - IMPOSSIBILE !!!
040200060901     c                   eval      *in41   = (wKE1prg > 1       and
040300060901     c                                        H1Ctbe  = *on     and
040400060901     c                                        V1Catb  = *blanks and
040500060811     c                                        V1Cmin <>
040600060811     c                                          $IPGmax(wKE1prg-1)
040700060811     c                                          + 0,001)
040800060811     c                   eval      H1in41  = *in41
040900060811if  1c                   if        *in41   = *on
041000060811     c                   eval      SAVin41 = *on
041100060811e   1c                   endif
041200060828      * - scaglione errato
041300060901     c                   eval      *in42   = (wKE1prg > 1       and
041400060901     c                                        H1Ctbe  = *on     and
041500060901     c***                                     V1Catb  = *blanks and
041600060828     c                                        V1Cmin  > V1Cmax)
041700060828     c                   eval      H1in42  = *in42
041800060828if  1c                   if        *in42   = *on
041900060828     c                   eval      SAVin42 = *on
042000060828e   1c                   endif
042100060905      *
042200060905      * Impostazione posizionamento del cursore sul campo di I/O
042300060905      *   del 1° record libero nel sfl
042400060905     c                   eval      *in52   = (H1Ctbe = *off)
042500060810      *
042600060905     c                   eval      *in32   = *off
042700060901     c                   z-add     wKE1prg       S01nrr
042800060810     c                   write     TB58S01
042900060810      *
043000060810      * Impostazione del SflEnd
043100060901     c                   eval      *in33   = (wKE1prg = *hival)
043200060810      *
043300060810     c                   ENDSR
043400060901
043500060901      *---------------------------------------------------------------*
043600060901      * sr_TxtC01 - Impostazione testo di avvertimento in C01         *
043700060901      *---------------------------------------------------------------*
043800060901     c     sr_TxtC01     BEGSR
043900060901      *
044000060901     c                   clear                   C01txt
044100060901     c                   eval      C01txtA = C_Atr_Norm
044200060901      *
044300060901sel 1c                   select
044400060901      * Visualizza avvertimento relativi a scaglioni mancanti
044500060901w   1c                   when           SAVin41 = *on
044600060901     c                             and  SAVin42 = *off
044700060901     c                   eval      C01txt  = 'Rilevata mancanza di sca+
044800060905     c                                       glioni!                  +
044900060905     c                                                                +
045000060905     c                                           '
045100060901     c                   eval      C01txtA = C_Atr_BL_RI
045200060901      * Visualizza avvertimento relativi a scaglioni errati
045300060901w   1c                   when      SAVin42 = *on
045400060901     c                   eval      C01txt  = 'Rilevati scaglioni errat+
045500060905     c                                       i!                       +
045600060905     c                                                                +
045700060905     c                                           '
045800060901     c                   eval      C01txtA = C_Atr_BL_RI
045900060905e   1c                   endsl
046000060901      *
046100060901     c                   ENDSR
046200060810
046300060810      *---------------------------------------------------------------*
046400060810      * CTRC01 - Controllo di tutto il subfile                        *
046500060810      *---------------------------------------------------------------*
046600060810     c     CtrC01        BEGSR
046700060810      *
046800060810     c                   readc     TB58S01
046900060810      *
047000060810do  1c                   DOW       NOT %eof(TNTB58D)
047100060810      *
047200060901     c*** già così:      eval      *in32   = *off
047300060810     c                   movea     *zeros        *in(50)
047400060810     c                   z-add     S01nrr        C01rcd
047500060810      *
047600060810     c                   exsr      CtrS01
047700060810      *
047800060810      * Reimpostazione indicatori per DSPATR
047900060810      * - record già immesso
048000060810     c                   eval      *in40   = (H1Ctbe = *on)
048100060901      * - scaglione (precedente) mancante - IMPOSSIBILE !!!
048200060811     c                   eval      *in41   = (H1in41 = *on)
048300060901      * - scaglione errato
048400060901     c                   eval      *in42   = (H1in42 = *on)
048500060810      *
048600060810      * Aggiornamento sfl
048700060810     c                   eval      *in32   = *on
048800060810if  2c                   if        *in28  OR  *in90
048900060810     c                   z-add     C01rcd        C01csr
049000060810e   2c                   endif
049100060810     c                   UPDATE    TB58S01
049200060810if  2c                   if        *in28  OR  *in90
049300060810     c                   leave
049400060810e   2c                   endif
049500060810      *
049600060810     c                   readc     TB58S01
049700060810      *
049800060810e   1c                   ENDDO
049900060901      *
050000060901      * Impostazione eventuale testo di avvertimento...
050100060901     c                   exsr      sr_TxtC01
050200060810      *
050300060810     c                   ENDSR
050400060810
050500060810      *---------------------------------------------------------------*
050600060810      * CTRS01 - Controllo del singolo record del subfile             *
050700060810      *---------------------------------------------------------------*
050800060810     c     CtrS01        BEGSR
050900060810      *
051000060810     c                   movea     *zeros        *in(50)
051100060810      *
051200060810      * Numero scaglione
051300060811      * (già impostato automaticamente)
051400060901      *
051500060901      * Scaglione prima immesso poi rimosso...
051600060901     c                   if             V1Cmax <= *zeros
051700060901     c                   clear                   V1Cmin
051800060901     c                   clear                   V1Cmax
051900060901     c                   endif
052000060811      *
052100060811      * Sequenza errata (lasciati scaglioni vuoti)
052200060811if  1c                   if             V1Cke1  > 1
052300060811     c                             and  V1Cmax <> *zeros
052400060811     c                             and  $IPGmax(V1Cke1-1) = *zeros
052500060811     c                   seton                                        285290
052600060811     c                   eval      V1Dmsg  = $Msg(6)
052700060811     c                   leavesr
052800060811e   1c                   endif
052900060810      *
053000060810      * Prezzo Massimo
053100060810if  2c                   if        V1Cmax  < *zeros
053200060810     c                   seton                                        285290
053300060810     c                   eval      V1Dmsg  = $Msg(2)
053400060810     c                   leavesr
053500060810e   1c                   endif
053600060810      *
053700060828      * Range prezzi
053800060828if  2c                   if        V1Cmin  > V1Cmax
053900060828     c                   seton                                        285190
054000060828     c                   eval      V1Dmsg  = $Msg(3)
054100060828     c                   leavesr
054200060828e   1c                   endif
054300060811      *
054400060811     c                   clear                   V1Cmin
054500060811     c                   clear                   V1Cum
054600060811     c                   eval      $IPGmax(V1Cke1) = *zeros
054700060810      *
054800060810      *?SEGUONO CONTROLLI DA ESEGUIRE SOLO PER I RECORDS CON DATI?
054900060810if  1c                   if             V1Cmin  = *zeros
055000060810     c                             and  V1Cmax  = *zeros
055100060810     c                   leavesr
055200060810e   1c                   endif
055300060811      *
055400060811     c                   eval      V1Cum   = 'Euro/Litro'
055500060810      *
055600060811      * Prezzo massimo già compreso in altro scaglione
055700060810     c                   eval      xx       = 1
055800060811     c     V1Cmax        lookup    $IPGmax(xx)                        10  10
055900060810      * - Scaglione reperito
056000060811if  1c                   if        *in10
056100060811     c                             and  V1Cke1  > xx
056200060811     c                   seton                                        285290
056300060810     c                   eval      V1Dmsg   = %trim($Msg(4))
056400060810     c                                      + ' '
056500060810     c                                      + %trim(%editc(xx : 'Z'))
056600060810     c                   leavesr
056700060810e   1c                   endif
056800060811      *
056900060811      * Ricalcolo prezzo minimo
057000060811if  1c                   if             V1Cke1  > 1
057100060811     c                             and  V1Cmax <> *zeros
057200060901     c                   eval      V1Cmin  = $IPGmax(V1Cke1-1) + 0,001
057300060811e   1c                   endif
057400060811      *
057500060811      * Aggiornamento schiera prezzi massimi x scaglione
057600060811     c                   eval      $IPGmax(V1Cke1) = V1Cmax
057700060810      *
057800060810     c                   ENDSR
057900060810
058000060810      *---------------------------------------------------------------*
058100060810      * F03S01 - Tasto funzionale F03 -> Fine programma               *
058200060810      *---------------------------------------------------------------*
058300060810     c     F03S01        BEGSR
058400060810      *
058500060810     c                   movel     *on           $Fine                          fine pgm
058600060810      *
058700060810     c                   ENDSR
058800060810
058900060810      *---------------------------------------------------------------*
059000060810      * F06S01 * Elaborazione subfile per aggiornamento tabella       *
059100060810      *---------------------------------------------------------------*
059200060810     c     F06S01        BEGSR
059300060810      *
059400060811     c                   clear                   S01nrr
059500060811      *
059600060811do  1c                   DO        *hival        S01nrr
059700060811      *
059800060811     c     S01nrr        chain     TB58S01
059900060810      *
060000060811      * Fine ciclo di lettura sfl
060100060811if  2c                   if        NOT %found(TNTB58D)
060200060811     c                   leave
060300060811e   2c                   endif
060400060811      * Fine dati immessi a video
060500060811if  2c                   if             H1Ctbe  = *off
060600060811     c                             and  V1Cmax  = *zeros
060700060811     c                   leave
060800060811e   2c                   endif
060900060905      * Record già inserito (solo in visualizzazione)
061000060905if  2c                   if        H1Ctbe  = *on
061100060905     c                   iter
061200060905e   2c                   endif
061300060905      *
061400060811      * Record nuovo
061500060811     c                   movel     C_CodTab      TBEcod
061600060901     c                   movel(p)  V1Cke1        TBEke1
061700060810     c                   clear                   TBEke2
061800060810     c                   clear                   TBElin
061900060810     c                   if        TBXsif  <> *blanks
062000060810     c                   movel     KNSIF         TBEsif
062100060810     c                   else
062200060810     c                   clear                   TBEsif
062300060810     c                   endif
062400060810     c     K05TBE01      chain     TNTBE000
062500060810      *
062600060810     c                   exsr      AggTBE
062700060810      *
062800060810e   1c                   ENDDO
062900060810      *
063000060810     c                   ENDSR
063100060810
063200060810      *---------------------------------------------------------------*
063300060810      * AGGTBE * Aggiornamento tabella (singolo record)               *
063400060810      *---------------------------------------------------------------*
063500060810     c     AggTBE        BEGSR
063600060810      *
063700060811if  1c                   IF        %found(TNTBE01L)
063800060811      *
063900060811      * Aggiornamento scaglione (annullato rec. precedente)
064000060811     c                   exsr      RieDS
064100060901     c                   eval      TBEatb  =  V1Catb
064200060811     c                   eval      TBEftt  =  TBXftt
064300060810     c                   clear                   TBEftr
064400060810     c                   UPDATE    TNTBE000
064500060810      *
064600060811x   1c                   ELSE
064700060811      * Immissione scaglione
064800060810     c                   exsr      RieTBE
064900060811     c                   eval      TBEftt  =  TBXftt
065000060810     c                   clear                   TBEflt
065100060810     c                   clear                   TBEdtr
065200060811     c*** 22 not tested: WRITE     TNTBE000                             22
065300060811     c                   WRITE     TNTBE000
065400060810      *
065500060811e   1c                   ENDIF
065600060810     c
065700060810     c                   ENDSR
065800060810
065900060810      *---------------------------------------------------------------*
066000060810      * RIETBE * Riempimento dati tabella per IMMISSIONE              *
066100060810      *---------------------------------------------------------------*
066200060810     c     RieTBE        BEGSR
066300060810      *
066400060810     c                   clear                   TNTBE000
066500060810      *
066600060810     c*** già così:      clear                   TBEatb
066700060811     c                   if        TBXsif  <> *blanks
066800060810     c                   movel     KNSIF         TBEsif
066900060810     c                   else
067000060810     c                   clear                   TBEsif
067100060810     c                   endif
067200060810     c                   clear                   TBElin
067300060810     c                   movel     TBXapl        TBEapl
067400060810     c                   movel     C_CodTab      TBEcod
067500060905     c                   movel(p)  V1Cke1        TBEke1
067600060810     c*** già così:      clear                   TBEke2
067700060810      *
067800060811     c                   exsr      RieDS
067900060811     c*** già fatto:     movel(p)  dIPG          TBEuni
068000060810      *
068100060810     c*** già così:      clear                   TBEftt
068200060810     c*** già così:      clear                   TBEflt
068300060810     c*** già così:      clear                   TBEftr
068400060810     c*** già così:      clear                   TBEdtr
068500060810      *
068600060810     c                   ENDSR
068700060811
068800060811      *---------------------------------------------------------------*
068900060811      * RIEDS  * Riempimento struttura dati                           *
069000060811      *---------------------------------------------------------------*
069100060811     c     RieDS         BEGSR
069200060811      *
069300060811     c                   clear                   dIPG
069400060811      *
069500060901sel 1c                   select
069600060905      * - scaglione successivo al 1°
069700060905w   1c                   when      V1Cke1  >  1
069800060905     c                   eval      §IPGmin =  $IPGmax(V1Cke1 - 1)
069900060901     c                                     +  0,001
070000060905      * - 1° scaglione
070100060905w   1c                   when      V1Cke1 <= 1
070200060811     c                   eval      §IPGmin =  *zeros
070300060905      * - record "manualmente" annullato (???)
070400060905w   1c                   other
070500060905     c                   eval      §IPGmin =  V1Cmin
070600060901e   1c                   endsl
070700060811     c                   eval      §IPGmax =  V1Cmax
070800060811      *
070900060811     c                   movel(p)  dIPG          TBEuni
071000060811      *
071100060811     c                   ENDSR
071200060810
071300060810** - $Msg -------------------------------------------------------------------*
071400060905     ...libero...                                                               1
071500060810Valore errato                                                                   2
071600060810Prezzo minimo > Prezzo massimo                                                  3
071700060811Prezzo massimo già compreso nello scaglione precedente                          4
071800060811Prezzo minimo non corretto. E' previsto                                         5
071900060811Sequenza scaglioni errata: NON ammessi "vuoti" tra uno scaglione e l'altro      6
