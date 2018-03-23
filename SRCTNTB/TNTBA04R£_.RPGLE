000100060810      *---------------------------------------------------------------*
000200160928      * Gestione tabella "agl" = gg mensili per moltiplicatori fissi  *
000300091028      *                          tempi uffici                         *
000400060810      *---------------------------------------------------------------*
000500060810
000600060810     h decedit('0,') datedit(*dmy/)
000700060810
000800060810      *---------------------------------------------------------------*
000900060810      *   A R C H I V I                                               *
001000060810      *---------------------------------------------------------------*
001100060810      *
001200160928     fazorg01l  if   e           k disk
001300091028     fTNTBE01L  Uf A e           k disk
001400060810      *
001500160928     fTNTBa03D  cf   e             workstn
001600060810     f                                     sfile(TB58S01:S01nrr)
001700060810     f                                     infds(DSFMT)
001800060810
001900060810      *---------------------------------------------------------------*
002000060810      *   C O S T A N T I                                             *
002100060810      *---------------------------------------------------------------*
002200060810      *
002300060810      * - Codice tabella in gestione
002400160928     d C_CodTab        c                   const('AGL')
002500060810      * - Nr. di righe in ogni pagina del sfl
002600060810     d C_SflPag        c                   const(18)
002700060810      * - Tasti funzionali
002800060810     d RollUp          c                   const(x'F5')
002900060811      * Attributi video
003000060811     d C_Atr_Norm      c                   const(X'A0')
003100060811     d C_Atr_BL_RI     c                   const(X'A9')
003200060810
003300060810      *---------------------------------------------------------------*
003400060810      *   S C H I E R E                                               *
003500060810      *---------------------------------------------------------------*
003600060810      *
003700091030     d wDate           s               d   datfmt(*iso)  inz(*loval)
003800060810      * - Messaggi di errore
003900091030     d $Msg            s             78    dim(  5) ctdata perrcd(1)
004000091030     c* schiera per individuare record doppi
004100091030     d sdup            s             11    dim(9999)
004200060810
004300060810      *---------------------------------------------------------------*
004400060810      *   S T R U T T U R E   D A T I                                 *
004500060810      *---------------------------------------------------------------*
004600060810      *
004700060810      * Parametri
004800060810     d KPJBA         e ds
004900060810      *
005000091028      * Tabella
005100160928     d dagl          e ds                  inz
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
006200060810     d Status         sds
006300060810     d  VTCpgm           *proc
006400060810      *
006500060810     d DSFMT           ds           512
006600060810     d  £Tasto               369    369
006700060810     d  £SFLnrr              378    379B 0
006800060810      *
006900160928     d £tbeKE1         s                   like(tbeKE1)
007000091030     d $tbecod         s                   like(tbecod)
007100091030     d $tbekey         s             11
007200060810      *---------------------------------------------------------------*
007300060810      *   V A R I A B I L I                                           *
007400060810      *---------------------------------------------------------------*
007500060810      *
007600060810      * - Flags
007700060810     d $Fine           s              1a   inz(*off)
007800060810     d $InzS01         s              1a   inz(*on)
007900060811     d SAVin41         s              1a   inz(*off)
008000060828     d SAVin42         s              1a   inz(*off)
008100081007     d SAVin43         s              1a   inz(*off)
008200081007     d SAVin44         s              1a   inz(*off)
008300060810      *
008400060810      * - Indici di schiera
008500060810     d xx              s              5  0 inz
008600060810      *
008700060810      * - Variabili riferite al data base o al display file
008800060811     d S01nrr          s                   like(C01rcd)  inz
008900060810      *
009000060810      * - Variabili definite a programma
009100091029     d dtaiso          s               d   datfmt(*iso)  inz(*loval)
009200091029     d dtaeur          s               d   datfmt(*eur)  inz(*loval)
009300060810     d wPage           s              4  0 inz
009400060810     d wRow            s              3  0 inz
009500061221     d wOpz            s              2    inz
009600091029     c*
009700091029     D Pvid            S               *
009800091029     D                                     INZ(%ADDR(v1c001))
009900091029     D mvid            S                   LIKE(v1c001)
010000091029     D                                     DIM(12)
010100091029     D                                     BASED(Pvid)
010200091029     D ptab            S               *
010300160928     D                                     INZ(%ADDR(D§agl001))
010400160928     D mtab            S                   LIKE(D§agl001)
010500091029     D                                     DIM(12)
010600091029     D                                     BASED(Ptab)
010700060810
010800060810      *---------------------------------------------------------------*
010900060810      *   M A I N   L I N E                                           *
011000060810      *---------------------------------------------------------------*
011100060810      *  Riepilogo indicatori utilizzati:                             *
011200060810      *  --------------------------------                             *
011300061221      *  06 - F6 abilitato                                            *
011400060810      *  10 - Comodo                                                  *
011500060810      *  22 - Errori in scrittura record (WRITE)                      *
011600061221      *  28 - Emissione messaggio di errore a video                   *
011700061221      *  30 - Pulizia subfile                                         *
011800061221      *  31 - NON emissione del subfile                               *
011900061221      *  33 - Fine dati nel subfile         (SFLEND)                  *
012000060810      *  40 - Record in modifica                                      *
012100081008      *  41 - Campo V1CKE1 in dspatr(RI) per mese mancante            *
012200081008      *  42 - Campo V1CKE1 in dspatr(RI) per mese errato              *
012300091029      *  43 - Campi mesi   in dspatr(RI) per des. mancante            *
012400091029      *  44 - Campo V1Cke2 in dspatr(RI) per prezzo medio mancante    *
012500060810      *  90 - Riemissione videata                                     *
012600060810      *---------------------------------------------------------------*
012700060810      *
012800060810      * Operazioni iniziali
012900060810     c                   exsr      RoutInz
013000060810      *
013100060810      * Gestione video
013200060810     c                   dow       $Fine   = *off
013300060810     c                   exsr      GesS01
013400060810     c                   enddo
013500060810      *
013600060810      * Fine
013700060810     c                   eval      *inLR   = *on
013800060810
013900060810      *---------------------------------------------------------------*
014000060810      * RoutInz - Operazioni Iniziali                                 *
014100060810      *---------------------------------------------------------------*
014200060810     c     RoutInz       BEGSR
014300060810      *
014400060810      * Ricezione parametri
014500060810     c     *entry        plist
014600060810     c                   parm                    KPJBA
014700160928     c                   movel     kpjbu         v1cke1            3
014800060810      *
014900060810      * Definizioni chiavi di accesso
015000060810     c     K05TBE01      klist                                                  *tntbe01l
015100060810     c                   kfld                    TBEcod                         -tabella
015200060810     c                   kfld                    TBEke1                         -chiave uno
015300060810     c                   kfld                    TBEke2                         -chiave due
015400060810     c                   kfld                    TBElin                         -lingua
015500060810     c                   kfld                    TBEsif                         -s.informativo
015600091029      * Definizioni chiavi di accesso
015700091029     c     Keytab        klist                                                  *tntbe01l
015800091030     c                   kfld                    $tbecod                        -tabella
015900091029     c                   kfld                    £TBEke1                        -chiave uno
016000160928     c                   movel     v1cke1        £tbeke1
016100160928     c                   eval      $tbecod = 'AGL'
016200061221      *
016300061221      * Imposto eventuali parametri ricevuti
016400061221     c                   if        %subst(KPJBU:1:2) > *zeros
016500061221     c                   movel     KPJBU         wOpz
016600061221     c                   endif
016700091030     c                   movel     *date         Wdate
016800060810      *
016900060810      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
017000060810     c     *dtaara       define    §azute        AZUTEds
017100060810     c     *dtaara       define    §datiute      dDatiUte
017200060810      *
017300060810     c                   clear                   AZUTEds
017400060810     c                   clear                   dDatiUte
017500060810     c                   clear                   TIBS34ds
017600060810     c                   in(E)     *dtaara
017700060810if  1c                   if        %Error  or  RSUT = *blanks
017800060810     c                   call      'TIBS34R'
017900060810     c                   parm                    TIBS34ds
018000060810     c                   in        *dtaara
018100060810e   1c                   endif
018200060810      *-- Verifica errori e autorità profilo
018300060810sel 1c                   SELECT
018400060810      *-- controllo se ho errori nei dati utente
018500060810      *--   nel qual caso NON risulta un profilo abilitato
018600060810w   1c                   WHEN      DUTerr  = 'E'
018700060810     c                   eval      $Fine   = *on
018800060810      *
018900060810      *-- CONTROLLO AUTORITA'
019000060810      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
019100060810      *-- se il chiamante non richiede autorità specifica verificare
019200060810      *--   quella del profilo
019300060810      *-- se il chiamante richiede autorità specifica verificarla,
019400060810      *--  se è blank verificare quella del profilo
019500060810      *
019600060810      * se UTEAUT = *BLANK non siamo in sede
019700060810w   1c                   WHEN      UTEaut  = *blank
019800081008     c                   eval      $Fine   = *on
019900060810      *
020000060810x   1c                   OTHER
020100060810      *
020200060810e   1c                   ENDSL
020300060810      *
020400060810      * Aggancio dati generali della tabella in esame
020500060810     c                   clear                   TBEcod
020600060810     c                   move      *zeros        TBEke1
020700091102     c                   move      C_CodTab      TBEke1
020800060810     c                   clear                   TBEke2
020900060810     c                   clear                   TBElin
021000060810     c                   movel     KNSIF         TBEsif
021100060810     c     K05TBE01      chain     TNTBE01L
021200060810     c                   if        not %found(TNTBE01L)
021300060810     c                   clear                   TBEsif
021400060810     c     K05TBE01      chain     TNTBE01L
021500060810     c                   endif
021600060810     c                   if        %found(TNTBE01L)
021700060810     c                   movel     TNTBEds       xTNTBEds
021800060810     c                   else
021900060810     c                   clear                   xTNTBEds
022000060810     c                   endif
022100060810      *
022200060810     c                   ENDSR
022300060810
022400091028      *---------------------------------------------------------------*
022500091028      * GESS01 - Gestione subfile                                     *
022600091028      *---------------------------------------------------------------*
022700060810     c     GesS01        BEGSR
022800060810      *
022900060810      * Inizializzazione videata
023000060810if  1c                   if        $InzS01 = *on
023100060810     c                   exsr      InzS01
023200060810     c                   movel     *off          $InzS01
023300060810e   1c                   endif
023400060810      *
023500060810      * Scrivo la testata e la riga tasti funzionali abilitati
023600060810if  1c                   if        NOT *in90
023700060810     c                   write     TB58T01
023800060810     c                   write     TB58P01
023900060810e   1c                   endif
024000060810      *
024100060810      * Posiziono il cursore
024200091223     c                   eval      *in30       = (C01rcd <= *zeros and
024300091223     c                                         s01nrr = *zeros )
024400060810w   1c                   if        C01csr  > *zeros
024500060810     c                   eval      C01rcd  = C01csr
024600060810     c                   else
024700060905     c                   eval      C01rcd  = 1
024800060810     c                   endif
024900091223     c                   if        s01nrr = 0
025000091223     c                   eval      s01nrr  = 1
025100091223     c                   end
025200060810      *
025300060810      * Emetto la videata
025400060810     c                   exfmt     TB58C01
025500060810      *
025600060810     c                   setoff                                       28  90
025700060810     c                   clear                   V1Dmsg
025800060810      *
025900060810sel 1c                   SELECT
026000060810      *
026100060810      * F3=Fine
026200160929w   1c                   WHEN      *inKl
026300060810     c                   exsr      F03S01
026400060810      *
026500060810      * Roll-Up
026600060810w   1c                   WHEN      £Tasto  = RollUp
026700060810do  2c                   do        C_SflPag
026800060810     c                   exsr      RecS01
026900060810if  3c                   if        *in33
027000060810     c                   leave
027100060810e   3c                   endif
027200060810e   2c                   enddo
027300060810     c     S01nrr        div       C_SflPag      wPage
027400060810     c                   mvr                     wRow
027500060811     c                   if        wRow    = *zeros
027600060810     c                   eval      C01csr  = (C_SflPag * (wPage - 1))
027700060810     c                                     + 1
027800060811     c                   else
027900060811     c                   eval      C01csr  = (C_SflPag * wPage)
028000060811     c                                     + 1
028100060811     c                   endif
028200060810      *
028300060810      * Controllo dati immessi a video
028400060810x   1c                   OTHER
028500060810     c                   exsr      CtrC01
028600060810      * - Rilevati Errori
028700060810if  2c                   if        *in90
028800060810     c                   leavesr
028900060810e   2c                   endif
029000060810      * F6=Conferma
029100061221if  2c                   if        *inKF
029200060810     c                   exsr      F06S01
029300060810     c                   eval      $Fine   = *on
029400060810e   2c                   endif
029500060810      *
029600060810e   1c                   ENDSL
029700060810      *
029800060810     c                   ENDSR
029900060810
030000060810      *---------------------------------------------------------------*
030100060810      * INZS01 - Inizializzazione subfile (S01)                       *
030200060810      *---------------------------------------------------------------*
030300060810     c     InzS01        BEGSR
030400060810      *
030500060810      * Pulizia subfile
030600060810     c                   seton                                        3031
030700060810     c                   write     TB58C01
030800060810     c                   setoff                                         31
030900060810     c                   eval      *in33   = *off
031000060810      *
031100060810     c                   clear                   C01rcd
031200060810     c                   clear                   C01csr
031300060811     c                   clear                   C01txt
031400060810     c                   clear                   V1Dmsg
031500060810     c                   setoff                                       28  90
031600060811     c                   movea     *zeros        *in(40)
031700091030      *
031800060810      * Caricamento dati già immessi (protetti)
031900091029     c
032000091028     c     keytab        setll     TNTBE000
032100091028     c                   do        *hival
032200091028     c     keytab        reade     TNTBE000
032300091028     c                   exsr      RecS01
032400091028     c   33              leave
032500091028e   1c                   ENDDO
032600091223     c*
032700091223     c                   if        s01nrr = 0
032800091223     c                   leavesr
032900091223     c                   end
033000061221      *
033100061221      * Se pgm. richiamato in interrogazione NON consento l'immissione
033200061221      *   di altri dati
033300091223if  0c                   IF        wOpz    = '05'
033400061221      *
033500061221     c                   eval      *in33   = *on
033600061221      *
033700061221x   0c                   ELSE
033800060810      *
033900060810      * Aggiunta di righe libere a completamento della pagina
034000060810     c     S01nrr        div       C_SflPag      wPage
034100060810     c                   mvr                     wRow
034200060810do  1c                   dow       S01nrr  < (C_SflPag * (wPage + 1))
034300060810     c                             and  NOT *in33
034400060810     c                   exsr      RecS01
034500060810e   1c                   enddo
034600060810      *
034700060905      * Imposta il posizionamento cursore sul 1° rec libero del sfl
034800060811if  1c                   if        C01csr  > *zeros
034900060905     c                   eval      C01csr  = 1
035000060810e   1c                   endif
035100061221      *
035200061221e   0c                   ENDIF
035300060901      *
035400060901      * Impostazione eventuale testo di avvertimento...
035500060901     c                   exsr      sr_TxtC01
035600061221      *
035700061221      * Abilitazione tasti funzionali
035800061221      * - F6 = Conferma
035900061221     c                   eval      *in06   = (wOpz <> '05')
036000060810      *
036100060810     c                   ENDSR
036200060810
036300060810      *---------------------------------------------------------------*
036400060810      * RECS01 - Caricamento singolo record del subfile S01           *
036500060810      *---------------------------------------------------------------*
036600060810     c     RecS01        BEGSR
036700081007     c                   clear                   TB58S01
036800091028     c                   setoff                                       33
036900091028if  1c                   if        not %eof(TNTBE01L)
037000081008     c                   movel     TBEke1        v1Cke1
037100081008     c                   eval      V1Catb  = TBEatb
037200160928     c                   movel     TBEuni        dagl
037300091028     c                   move      mtab          mvid
037400091028     c                   if        tbeke2 <> ' '
037500091029     c                   movel     tbeke2        com08             8
037600091029     c                   move      com08         comdta            8 0
037700091028     c                   move      comdta        dtaiso
037800091028     c                   move      dtaiso        dtaeur
037900091029     c                   move      dtaeur        v1cke2
038000091030     c                   move      dtaeur        h1cke2
038100091028     c                   end
038200081008     c                   eval      H1Ctbe  = *on
038300091030     c                   else
038400060810      * Record nuovo (vuoto)
038500091030     c                   movel     £tbeke1       v1cke1
038600091030     c                   clear                   h1cke2
038700091030     c                   eval      v1cke2 = 31122039
038800091028     c                   clear                   mvid
038900081008     c                   eval      H1Ctbe  = *off
039000091028     c                   seton                                        33
039100091028e   1c                   END
039200081007     c*
039300081007      * Scrittura singolo record nel subfile S01
039400081007     c                   exsr      WrtS01
039500060901      *
039600060901     c                   ENDSR
039700060901
039800060901      *---------------------------------------------------------------*
039900060901      * WRTS01 - Scrittura singolo record nel subfile S01             *
040000060901      *---------------------------------------------------------------*
040100060901     c     WrtS01        BEGSR
040200060901      *
040300060810      * Attributi di visualizzazione
040400060810      * - record già immesso
040500060810     c                   eval      *in40   = (H1Ctbe = *on)
040600081007     c                   add       1             S01nrr
040700060810     c                   write     TB58S01
040800060810      *
040900060810      * Impostazione del SflEnd
041000081007     c                   if        s01nrr = 9999
041100081007     c                   seton                                        33
041200081007     c                   end
041300060810      *
041400060810     c                   ENDSR
041500060901
041600060901      *---------------------------------------------------------------*
041700060901      * sr_TxtC01 - Impostazione testo di avvertimento in C01         *
041800060901      *---------------------------------------------------------------*
041900060901     c     sr_TxtC01     BEGSR
042000060901      *
042100160928     c                   movel     v1cke1        vfil              3 0
042200160928     c     vfil          chain     azorg01l
042300160928     c                   eval      c01txt = v1cke1 + ' ' + orgdes
042400060901      *
042500060901     c                   ENDSR
042600060810
042700060810      *---------------------------------------------------------------*
042800060810      * CTRC01 - Controllo di tutto il subfile                        *
042900060810      *---------------------------------------------------------------*
043000060810     c     CtrC01        BEGSR
043100060810      *
043200091030     c                   clear                   sdup
043300081008     c                   z-add     0             ok                1 0
043400081008do  1c                   DO        *hival        xx
043500081008     c     xx            chain     TB58S01                            29
043600081008     c                   if        *in29
043700081008     c                   leave
043800081008     c                   end
043900060810     c                   z-add     S01nrr        C01rcd
044000060810      *
044100060810     c                   exsr      CtrS01
044200060810      *
044300060810      * Reimpostazione indicatori per DSPATR
044400060810      * - record già immesso
044500060810     c                   eval      *in40   = (H1Ctbe = *on)
044600091029      * - moltiplicatore errato
044700091030     c                   eval      *in41   = (H1in41 = *on)
044800091030     c                   eval      *in42   = (H1in42 = *on)
044900091030      * - gg mensili
045000091030     c                   eval      *in43   = (H1in43 = *on)
045100091030      * - data errata
045200091030     c                   eval      *in44   = (H1in44 = *on)
045300060810      * Aggiornamento sfl
045400060810if  2c                   if        *in28  OR  *in90
045500060810     c                   z-add     C01rcd        C01csr
045600060810e   2c                   endif
045700060810     c                   UPDATE    TB58S01
045800060810if  2c                   if        *in28  OR  *in90
045900060810     c                   leave
046000060810e   2c                   endif
046100060810      *
046200060810e   1c                   ENDDO
046300060901      *
046400060901      * Impostazione eventuale testo di avvertimento...
046500060901     c                   exsr      sr_TxtC01
046600060810      *
046700060810     c                   ENDSR
046800060810
046900060810      *---------------------------------------------------------------*
047000060810      * CTRS01 - Controllo del singolo record del subfile             *
047100060810      *---------------------------------------------------------------*
047200060810     c     CtrS01        BEGSR
047300060810      *
047400081008     c                   setoff                                       414243
047500091030     c                   setoff                                       44
047600081008     c                   eval      savin41= *off
047700081008     c                   eval      savin42= *off
047800081008     c                   eval      savin43= *off
047900081008     c                   eval      savin44= *off
048000081008     c                   eval      h1in41 = *off
048100081008     c                   eval      h1in42 = *off
048200081008     c                   eval      h1in43 = *off
048300081008     c                   eval      h1in44 = *off
048400091030     C                   XFOOT     MVID          COMODO            4 0
048500081008     c*
048600091030if  1c                   if             comodo  = *zeros
048700081008     c                   z-add     1             ok
048800081008     c                   leavesr
048900081008e   1c                   endif
049000091030      *?SEGUONO CONTROLLI DA ESEGUIRE SOLO PER I RECORDS CON DATI?
049100060810      *
049200160928if  1c                   if        h1ctbe = *off
049300091102      * DATA
049400091102if  2c                   if        V1CKE2 =  *zeros
049500091102     c                   seton                                        284490
049600091102     c                   eval      H1in44  = *in44
049700091102     c                   eval      SAVin44 = *on
049800091102     c                   eval      V1Dmsg  = $Msg(1)
049900091102     c                   leavesr
050000160928    2C                   ELSE
050100091102     C     *EUR          test(d)                 V1CKE2                 44
050200160928    3c                   if        *in44
050300091102     c                   seton                                        2890
050400091102     c                   eval      H1in44  = *in44
050500091102     c                   eval      SAVin44 = *on
050600091102     c                   eval      V1Dmsg  = $Msg(1)
050700091102     c                   leavesr
050800160928    3c                   else
050900091102     c* non deve essere retroattiva e deve essere fine mese
051000091102     c                   move      v1cke2        dtaeur
051100091102     c                   move      dtaeur        dtaiso
051200160928    4c                   if        dtaiso < wdate and v1cke2 <> h1cke2
051300091102     c                   seton                                        289044
051400091102     c                   eval      H1in44  = *in44
051500091102     c                   eval      SAVin44 = *on
051600091102     c                   eval      V1Dmsg  = $Msg(3)
051700091102     c                   leavesr
051800160928    4c                   else
051900091102     c                   extrct    dtaiso:*m     mesep             2 0
052000091102     c                   adddur    1:*d          dtaiso
052100091102     c                   extrct    dtaiso:*m     mesed             2 0
052200160928    5c                   if        mesep = mesed
052300091102     c                   seton                                        289044
052400091102     c                   eval      H1in44  = *in44
052500091102     c                   eval      SAVin44 = *on
052600091102     c                   eval      V1Dmsg  = $Msg(3)
052700091102     c                   leavesr
052800160928    5C                   end
052900160928    4C                   end
053000160928    3C                   end
053100160928    2C                   end
053200091102     c* non ci devono essere righe doppie
053300091030     c                   move      v1cke2        $tbekey
053400091102     c     $tbekey       lookup    sdup                                   44
053500160928     c                   if        *in44
053600091102     c                   seton                                        2890
053700091102     c                   eval      H1in44  = *in44
053800091030     c                   eval      SAVin44 = *on
053900091030     c                   eval      V1Dmsg  = $Msg(5)
054000091030     c                   leavesr
054100160928     C                   endif
054200091030e   1c                   endif
054300091102     c* carico la schiera per verificare l'inserimento di righe doppie
054400091102     c                   movel     v1cke1        sdup(s01nrr)
054500091102     c                   move      v1cke2        sdup(s01nrr)
054600060810      *
054700060810     c                   ENDSR
054800060810
054900060810      *---------------------------------------------------------------*
055000060810      * F03S01 - Tasto funzionale F03 -> Fine programma               *
055100060810      *---------------------------------------------------------------*
055200060810     c     F03S01        BEGSR
055300060810      *
055400060810     c                   movel     *on           $Fine                          fine pgm
055500060810      *
055600060810     c                   ENDSR
055700060810
055800060810      *---------------------------------------------------------------*
055900060810      * F06S01 * Elaborazione subfile per aggiornamento tabella       *
056000060810      *---------------------------------------------------------------*
056100060810     c     F06S01        BEGSR
056200060810      *
056300060811     c                   clear                   S01nrr
056400060811      *
056500060811do  1c                   DO        *hival        S01nrr
056600060811      *
056700060811     c     S01nrr        chain     TB58S01
056800060810      *
056900060811      * Fine ciclo di lettura sfl
057000160928if  2c                   if        NOT %found(TNTBa03D)
057100060811     c                   leave
057200060811e   2c                   endif
057300091030     c                   xfoot     mvid          comodo
057400060811      * Fine dati immessi a video
057500060811if  2c                   if             H1Ctbe  = *off
057600091030     c                             and  comodo  = 0
057700091030     c                   iter
057800060811e   2c                   endif
057900060905      * Record già inserito (solo in visualizzazione)
058000091030if  2c                   if        H1Ctbe  = *on and h1cke2 = v1cke2
058100060905     c                   iter
058200060905e   2c                   endif
058300060905      *
058400060811      * Record nuovo
058500060811     c                   movel     C_CodTab      TBEcod
058600060901     c                   movel(p)  V1Cke1        TBEke1
058700091102     c                   if        H1Ctbe  = *on
058800091102     c                   move      h1cke2        dtaeur
058900091102     c                   else
059000091030     c                   move      v1cke2        dtaeur
059100091102     c                   end
059200091030     c                   move      dtaeur        dtaiso
059300091030     c                   move      dtaiso        comdta
059400091030     c                   movel(p)  comdta        tbeke2
059500060810     c                   clear                   TBElin
059600060810     c                   if        TBXsif  <> *blanks
059700060810     c                   movel     KNSIF         TBEsif
059800060810     c                   else
059900060810     c                   clear                   TBEsif
060000060810     c                   endif
060100060810     c     K05TBE01      chain     TNTBE000
060200060810      *
060300060810     c                   exsr      AggTBE
060400060810      *
060500060810e   1c                   ENDDO
060600060810      *
060700060810     c                   ENDSR
060800060810
060900060810      *---------------------------------------------------------------*
061000060810      * AGGTBE * Aggiornamento tabella (singolo record)               *
061100060810      *---------------------------------------------------------------*
061200060810     c     AggTBE        BEGSR
061300091102     c                   exsr      RieTBE
061400091102      * aggiornamento
061500060811if  1c                   IF        %found(TNTBE01L)
061600060901     c                   eval      TBEatb  =  V1Catb
061700060811     c                   eval      TBEftt  =  TBXftt
061800060810     c                   clear                   TBEftr
061900060810     c                   UPDATE    TNTBE000
062000060811x   1c                   ELSE
062100091102      * Immissione
062200060811     c                   eval      TBEftt  =  TBXftt
062300060810     c                   clear                   TBEflt
062400060810     c                   clear                   TBEdtr
062500060811     c                   WRITE     TNTBE000
062600060811e   1c                   ENDIF
062700060810     c
062800060810     c                   ENDSR
062900060810
063000060810      *---------------------------------------------------------------*
063100060810      * RIETBE * Riempimento dati tabella per IMMISSIONE              *
063200060810      *---------------------------------------------------------------*
063300060810     c     RieTBE        BEGSR
063400060810      *
063500060810     c                   clear                   TNTBE000
063600060810      *
063700060811     c                   if        TBXsif  <> *blanks
063800060810     c                   movel     KNSIF         TBEsif
063900060810     c                   else
064000060810     c                   clear                   TBEsif
064100060810     c                   endif
064200060810     c                   clear                   TBElin
064300060810     c                   movel     TBXapl        TBEapl
064400060810     c                   movel     C_CodTab      TBEcod
064500060905     c                   movel(p)  V1Cke1        TBEke1
064600091030     c                   move      v1cke2        dtaeur
064700091030     c                   move      dtaeur        dtaiso
064800091030     c                   move      dtaiso        comdta
064900091030     c                   movel(p)  comdta        tbeke2
065000060810      *
065100060811     c                   exsr      RieDS
065200060810      *
065300060810     c                   ENDSR
065400060811
065500060811      *---------------------------------------------------------------*
065600060811      * RIEDS  * Riempimento struttura dati                           *
065700060811      *---------------------------------------------------------------*
065800060811     c     RieDS         BEGSR
065900060811      *
066000160928     c                   clear                   dagl
066100160928     c                   eval      D§agl001 = v1c001
066200160928     c                   eval      D§agl002 = v1c002
066300160928     c                   eval      D§agl003 = v1c003
066400160928     c                   eval      D§agl004 = v1c004
066500160928     c                   eval      D§agl005 = v1c005
066600160928     c                   eval      D§agl006 = v1c006
066700160928     c                   eval      D§agl007 = v1c007
066800160928     c                   eval      D§agl008 = v1c008
066900160928     c                   eval      D§agl009 = v1c009
067000160928     c                   eval      D§agl010 = v1c010
067100160928     c                   eval      D§agl011 = v1c011
067200160928     c                   eval      D§agl012 = v1c012
067300060811      *
067400160928     c                   movel(p)  dagl          TBEuni
067500060811      *
067600060811     c                   ENDSR
067700060810
067800060810** - $Msg -------------------------------------------------------------------*
067900091030Data scadenza errata
068000091030E' obbligatorio inserire almeno un gg
068100091030La data non può essere retroattiva o diversa da fine mese
068200091030Moltiplicatore fisso errato
068300091030Moltiplicatore fisso doppio
