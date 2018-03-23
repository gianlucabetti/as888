000100060810      *---------------------------------------------------------------*
000200081007      * Gestione tabella "GMP" = prezzo medio gasolio l.133           *
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
001300081007     fTNTB69D   cf   e             workstn
001400060810     f                                     sfile(TB58S01:S01nrr)
001500060810     f                                     infds(DSFMT)
001600060810
001700060810      *---------------------------------------------------------------*
001800060810      *   C O S T A N T I                                             *
001900060810      *---------------------------------------------------------------*
002000060810      *
002100060810      * - Codice tabella in gestione
002200081007     d C_CodTab        c                   const('GPM')
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
003800081007     d $Msg            s             78    dim(  4) ctdata perrcd(1)
003801081118     d mese            s             10    dim( 12) ctdata perrcd(1)
004101081007     d dtacom          s               d
004103081007     d comdta8         s              8  0
004104081007     d comodo8         s              8
004105081118     d wmese           s              2  0
004200060810
004300060810      *---------------------------------------------------------------*
004400060810      *   S T R U T T U R E   D A T I                                 *
004500060810      *---------------------------------------------------------------*
004600060810      *
004700060810      * Parametri
004800060810     d KPJBA         e ds
004900060810      *
005000081007      * Tabella gpm = prezzo medio mensile gasolio x l.133
005100081007     d dgpm          e ds                  inz
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
008301081007     d SAVin43         s              1a   inz(*off)
008302081007     d SAVin44         s              1a   inz(*off)
008400060810      *
008500060810      * - Indici di schiera
008600060810     d xx              s              5  0 inz
008700060810      *
008800060810      * - Variabili riferite al data base o al display file
008900060811     d S01nrr          s                   like(C01rcd)  inz
009100081007     d W1Cke1          s              6    inz
009101081007     d Wke1prg         s              6    inz
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
011100061221      *  33 - Fine dati nel subfile         (SFLEND)                  *
011200060810      *  40 - Record in modifica                                      *
011300081008      *  41 - Campo V1CKE1 in dspatr(RI) per mese mancante            *
011400081008      *  42 - Campo V1CKE1 in dspatr(RI) per mese errato              *
011401081008      *  43 - Campo V1CDES in dspatr(RI) per des. mancante            *
011402081008      *  44 - Campo V1CGPM in dspatr(RI) per prezzo medio mancante    *
011500060810      *  50 - ERR: Flag annullamento errato                           *
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
015501081008     c                   movel     wdate         comdta8
015502081008     c                   movel     comdta8       limite            6 0
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
018501081008     c                   eval      $Fine   = *on
018600060810      *
018700060810x   1c                   OTHER
018800060810      *
018900060810e   1c                   ENDSL
019000060810      *
019100060810      * Aggancio dati generali della tabella in esame
019200060810     c                   clear                   TBEcod
019300060810     c                   move      *zeros        TBEke1
019400081007     c                   movel     C_CodTab      TBEke1
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
029901081007     c                   eval      wke1prg = '200807'
030000060810      *
030100060810      * Caricamento dati già immessi (protetti)
030200060810     c     C_CodTab      setll     TNTBE000
030300060810     c     C_CodTab      reade     TNTBE000
030400060810do  1c                   DOW       NOT  %eof(TNTBE01L)
030500060810     c                             and  NOT *in33
030700060905     c                   exsr      RecS01
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
034702081007     c                   clear                   TB58S01
034703081008if  1c                   if        NOT  %eof(TNTBE01L)
034704081008     c                   movel     TBEke1        v1Cke1
034705081008     c                   movel     v1cke1        w1cke1
034706081008     c                   eval      V1Catb  = TBEatb
034707081008     c                   movel     TBEke1        v1Cke1
034708081008     c                   movel     TBEuni        dgpm
034709081008     c                   eval      V1Cgpm  = d§gpmgpm
034710081008     c                   eval      V1Cdes  = d§gpmdes
034711081008     c                   eval      H1Ctbe  = *on
034712081008e   1c                   endif
034800060810      *
035900060901sel 1c                   SELECT
037200081007      * Record mancante
037300060901w   1c                   WHEN      NOT  %eof(TNTBE01L)
037400060901     c                             and  W1Cke1 > wKE1prg
037401081008     c                   seton                                        41
037500060901     c                   eval      H1Ctbe  = *off
037902081007      * Record già esistente
037903081007w   1c                   WHEN      NOT  %eof(TNTBE01L)
037904081007     c                             and  W1Cke1 < wKE1prg
037905081008     c                   seton                                        42
037906081008     c                   eval      H1Ctbe  = *off
038100060810      *
038200060810      * Record nuovo (vuoto)
038300060901w   1c                   WHEN      %eof(TNTBE01L)
038401081008     c                   movel     wke1prg       v1cke1
038404081118     c                   eval      v1cdes = 'COSTO DI ' +
038405081118     c                             mese(wmese)
038406081008     c                   if        v1cke1> limite
038407081008     c                   seton                                        33
038408081008     c                   leavesr
038409081008     c                   end
038410081008     c                   eval      H1Ctbe  = *off
038600060901e   1c                   ENDSL
038605081007     c*
038611081007      * Scrittura singolo record nel subfile S01
038612081007     c                   exsr      WrtS01
038613081007      *
038615081007     c                   movel     v1cke1        comdta8
038616081007     c                   move      01            comdta8
038617081007     c                   movel     comdta8       dtacom
038618081007     c                   adddur    1:*m          dtacom
038619081007     c                   movel     dtacom        comdta8
038620081007     c                   movel     comdta8       comodo8
038621081007     c                   eval      wke1prg = %subst(comodo8: 1: 6)
038622081118     C                   EXTRCT    DTACOM:*M     WMESE
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
040100081007      * - mese mancante
040800060811     c                   eval      H1in41  = *in41
040900060811if  1c                   if        *in41   = *on
041000060811     c                   eval      SAVin41 = *on
041100060811e   1c                   endif
041200081007      * - mese errato
041700060828     c                   eval      H1in42  = *in42
041800060828if  1c                   if        *in42   = *on
041900060828     c                   eval      SAVin42 = *on
042000060828e   1c                   endif
042200060905      * Impostazione posizionamento del cursore sul campo di I/O
042300060905      *   del 1° record libero nel sfl
042400060905     c                   eval      *in52   = (H1Ctbe = *off)
042500060810      *
042700081007     c                   add       1             S01nrr
042800060810     c                   write     TB58S01
042900060810      *
043000060810      * Impostazione del SflEnd
043001081007     c                   if        s01nrr = 9999
043100081007     c                   seton                                        33
043101081007     c                   end
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
044700081007     c                   eval      C01txt  = 'Rilevata mancanza di uno o  +
044800081007     c                                       più mesi! '
045100060901     c                   eval      C01txtA = C_Atr_BL_RI
045200060901      * Visualizza avvertimento relativi a scaglioni errati
045300060901w   1c                   when      SAVin42 = *on
045400081007     c                   eval      C01txt  = 'Rilevati mesi errat+
045500081007     c                                       i! '
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
046701081008     c                   z-add     0             ok                1 0
046702081008do  1c                   DO        *hival        xx
046800081008     c     xx            chain     TB58S01                            29
047100081008     c                   if        *in29
047101081008     c                   leave
047102081008     c                   end
047300060810     c                   movea     *zeros        *in(50)
047400060810     c                   z-add     S01nrr        C01rcd
047500060810      *
047600060810     c                   exsr      CtrS01
047700060810      *
047800060810      * Reimpostazione indicatori per DSPATR
047900060810      * - record già immesso
048000060810     c                   eval      *in40   = (H1Ctbe = *on)
048100081007      * - mese precedente mancante
048200060811     c                   eval      *in41   = (H1in41 = *on)
048300081007      * - mese errato
048400060901     c                   eval      *in42   = (H1in42 = *on)
048401081007      * - prezzo errato
048402081007     c                   eval      *in43   = (H1in43 = *on)
048403081007      * - descr. errato
048404081007     c                   eval      *in44   = (H1in44 = *on)
048500060810      *
048600060810      * Aggiornamento sfl
048800060810if  2c                   if        *in28  OR  *in90
048900060810     c                   z-add     C01rcd        C01csr
049000060810e   2c                   endif
049100060810     c                   UPDATE    TB58S01
049200060810if  2c                   if        *in28  OR  *in90
049300060810     c                   leave
049400060810e   2c                   endif
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
051001081008     c                   setoff                                       414243
051002081008     c                   setoff                                       4450
051003081008     c                   eval      savin41= *off
051004081008     c                   eval      savin42= *off
051005081008     c                   eval      savin43= *off
051006081008     c                   eval      savin44= *off
051007081008     c                   eval      h1in41 = *off
051008081008     c                   eval      h1in42 = *off
051009081008     c                   eval      h1in43 = *off
051010081008     c                   eval      h1in44 = *off
051011081008     c*
051012081008     c                   if        v1cgpm <> 0
051013081008     c                   movel     v1cke1        comdta8
051014081007     c                   move      01            comdta8
051015081007     c                   movel     comdta8       dtacom
051016081007     c                   adddur    1:*m          dtacom
051017081007     c                   movel     dtacom        comdta8
051018081007     c                   movel     comdta8       comodo8
051019081007     c                   eval      w1cke1 = %subst(comodo8: 1: 6)
051024081007     c                   end
051025081008      *?SEGUONO CONTROLLI DA ESEGUIRE SOLO PER I RECORDS CON DATI?
051026081008if  1c                   if             V1Cgpm  = *zeros
051028081008     c                   z-add     1             ok
051029081008     c                   leavesr
051030081008e   1c                   endif
051100060810      *
052100081007      * inizio 200807
052200081008if  1c                   if             V1Cgpm <> 0 and v1cke1 < 200807
052500081008     c                   seton                                        289042
052501081008     c                   eval      H1in42  = *in42
052502081008if  1c                   if        *in42   = *on
052503081008     c                   eval      SAVin42 = *on
052504081007e   1c                   endif
052600081007     c                   eval      V1Dmsg  = $Msg(4)
052700060811     c                   leavesr
052800060811e   1c                   endif
052801081008      * sequenza errata
052803081008     c                   if                 ok = 1
052804081008     c                   seton                                        289041
052805081008     c                   eval      H1in41  = *in41
052806081008if  1c                   if        *in41   = *on
052807081008     c                   eval      SAVin41 = *on
052808081008e   1c                   endif
052809081008     c                   eval      V1Dmsg  = $Msg(3)
052810081008     c                   leavesr
052811081008e   1c                   endif
053600060810      *
053700081007      * descrizione
053800081007if  2c                   if        V1Cdes = ' '
053900081007     c                   seton                                        284390
053901081007     c                   eval      H1in43  = *in43
053902081007if  1c                   if        *in43   = *on
053903081007     c                   eval      SAVin43 = *on
053904081007e   1c                   endif
054000081008     c                   eval      V1Dmsg  = $Msg(2)
054100060828     c                   leavesr
054200060828e   1c                   endif
054201081008      *
054202081008      * Prezzo medio
054203081008if  2c                   if        V1Cgpm =  *zeros
054204081008     c                   seton                                        284490
054205081008     c                   eval      H1in44  = *in44
054206081008if  1c                   if        *in44   = *on
054207081008     c                   eval      SAVin44 = *on
054208081008e   1c                   endif
054209081008     c                   eval      V1Dmsg  = $Msg(1)
054210081008     c                   leavesr
054211081008e   1c                   endif
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
060100081007if  2c                   if        NOT %found(TNTB69D)
060200060811     c                   leave
060300060811e   2c                   endif
060400060811      * Fine dati immessi a video
060500060811if  2c                   if             H1Ctbe  = *off
060600081008     c                             and  V1Cgpm  = *zeros
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
066700060811     c                   if        TBXsif  <> *blanks
066800060810     c                   movel     KNSIF         TBEsif
066900060810     c                   else
067000060810     c                   clear                   TBEsif
067100060810     c                   endif
067200060810     c                   clear                   TBElin
067300060810     c                   movel     TBXapl        TBEapl
067400060810     c                   movel     C_CodTab      TBEcod
067500060905     c                   movel(p)  V1Cke1        TBEke1
067700060810      *
067800060811     c                   exsr      RieDS
068500060810      *
068600060810     c                   ENDSR
068700060811
068800060811      *---------------------------------------------------------------*
068900060811      * RIEDS  * Riempimento struttura dati                           *
069000060811      *---------------------------------------------------------------*
069100060811     c     RieDS         BEGSR
069200060811      *
069300081007     c                   clear                   dgpm
069301081007     c                   eval      d§gpmdes = v1cdes
069302081007     c                   eval      d§gpmgpm = v1cgpm
070800060811      *
070900081007     c                   movel(p)  dgpm          TBEuni
071000060811      *
071100060811     c                   ENDSR
071200060810
071300060810** - $Msg -------------------------------------------------------------------*
071600081007Prezzo medio mensile mancante
071601081007Descrizione mancante
071900081007Sequenza mesi errata: NON ammessi "vuoti" tra un mese e l'altro
072000081007Anno mese inferiore al 200807
072100081118** mese
072200081118GENNAIO
072300081118FEBBRAIO
072400081118MARZO
072500081118APRILE
072600081118MAGGIO
072700081118GIUGNO
072800081118LUGLIO
072900081118AGOSTO
073000081118SETTEMBRE
073100081118OTTOBRE
073200081118NOVEMBRE
073300081118DICEMBRE
