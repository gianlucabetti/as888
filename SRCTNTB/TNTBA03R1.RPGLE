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
001300160929     fTNTBE01L  if   e           k disk
001400060810      *
001500160929     fTNTBa03D1 cf   e             workstn
001600160929     f                                     sfile(TB03S01:S01nrr)
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
006900091030     d $tbecod         s                   like(tbecod)
007000091030     d $tbekey         s             11
007100060810      *---------------------------------------------------------------*
007200060810      *   V A R I A B I L I                                           *
007300060810      *---------------------------------------------------------------*
007400060810      *
007500060810      * - Flags
007600060810     d $Fine           s              1a   inz(*off)
007700060810     d $InzS01         s              1a   inz(*on)
007800060811     d SAVin41         s              1a   inz(*off)
007900060828     d SAVin42         s              1a   inz(*off)
008000081007     d SAVin43         s              1a   inz(*off)
008100081007     d SAVin44         s              1a   inz(*off)
008200060810      *
008201161107     d SAVke1          s                   like(v1cke1)
008202161107      *
008300060810      * - Indici di schiera
008400060810     d xx              s              5  0 inz
008500060810      *
008600060810      * - Variabili riferite al data base o al display file
008700060811     d S01nrr          s                   like(C01rcd)  inz
008800060810      *
008900060810      * - Variabili definite a programma
009000091029     d dtaiso          s               d   datfmt(*iso)  inz(*loval)
009100091029     d dtaeur          s               d   datfmt(*eur)  inz(*loval)
009200060810     d wPage           s              4  0 inz
009300060810     d wRow            s              3  0 inz
009400061221     d wOpz            s              2    inz
009500091029     c*
009600091029     D Pvid            S               *
009700091029     D                                     INZ(%ADDR(v1c001))
009800091029     D mvid            S                   LIKE(v1c001)
009900091029     D                                     DIM(12)
010000091029     D                                     BASED(Pvid)
010100091029     D ptab            S               *
010200160928     D                                     INZ(%ADDR(D§agl001))
010300160928     D mtab            S                   LIKE(D§agl001)
010400091029     D                                     DIM(12)
010500091029     D                                     BASED(Ptab)
010600060810
010700060810      *---------------------------------------------------------------*
010800060810      *   M A I N   L I N E                                           *
010900060810      *---------------------------------------------------------------*
011000060810      *  Riepilogo indicatori utilizzati:                             *
011100060810      *  --------------------------------                             *
011200061221      *  06 - F6 abilitato                                            *
011300060810      *  10 - Comodo                                                  *
011400060810      *  22 - Errori in scrittura record (WRITE)                      *
011500061221      *  28 - Emissione messaggio di errore a video                   *
011600061221      *  30 - Pulizia subfile                                         *
011700061221      *  31 - NON emissione del subfile                               *
011800061221      *  33 - Fine dati nel subfile         (SFLEND)                  *
011900060810      *  40 - Record in modifica                                      *
012000081008      *  41 - Campo V1CKE1 in dspatr(RI) per mese mancante            *
012100081008      *  42 - Campo V1CKE1 in dspatr(RI) per mese errato              *
012200091029      *  43 - Campi mesi   in dspatr(RI) per des. mancante            *
012300091029      *  44 - Campo V1Cke2 in dspatr(RI) per prezzo medio mancante    *
012400060810      *  90 - Riemissione videata                                     *
012500060810      *---------------------------------------------------------------*
012501161107      * Fine
012502161107     c                   eval      *inLR   = *on
012600060810      *
012700060810      * Operazioni iniziali
012800060810     c                   exsr      RoutInz
012900060810      *
013000060810      * Gestione video
013100060810     c                   dow       $Fine   = *off
013200060810     c                   exsr      GesS01
013300060810     c                   enddo
013400060810      *
013701161107     c                   RETURN
013800060810      *---------------------------------------------------------------*
013900060810      * RoutInz - Operazioni Iniziali                                 *
014000060810      *---------------------------------------------------------------*
014100060810     c     RoutInz       BEGSR
014200060810      *
014300060810      * Ricezione parametri
014400060810     c     *entry        plist
014500060810     c                   parm                    KPJBA
014600060810      *
014700060810      * Definizioni chiavi di accesso
014800060810     c     K05TBE01      klist                                                  *tntbe01l
014900060810     c                   kfld                    TBEcod                         -tabella
015000060810     c                   kfld                    TBEke1                         -chiave uno
015100060810     c                   kfld                    TBEke2                         -chiave due
015200060810     c                   kfld                    TBElin                         -lingua
015300060810     c                   kfld                    TBEsif                         -s.informativo
015400091029      * Definizioni chiavi di accesso
015500091029     c     Keytab        klist                                                  *tntbe01l
015600091030     c                   kfld                    $tbecod                        -tabella
015700160928     c                   eval      $tbecod = 'AGL'
015800061221      *
015900061221      * Imposto eventuali parametri ricevuti
016000061221     c                   if        %subst(KPJBU:1:2) > *zeros
016100061221     c                   movel     KPJBU         wOpz
016200061221     c                   endif
016300091030     c                   movel     *date         Wdate
016400060810      *
016500060810      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
016600060810     c     *dtaara       define    §azute        AZUTEds
016700060810     c     *dtaara       define    §datiute      dDatiUte
016800060810      *
016900060810     c                   clear                   AZUTEds
017000060810     c                   clear                   dDatiUte
017100060810     c                   clear                   TIBS34ds
017200060810     c                   in(E)     *dtaara
017300060810if  1c                   if        %Error  or  RSUT = *blanks
017400060810     c                   call      'TIBS34R'
017500060810     c                   parm                    TIBS34ds
017600060810     c                   in        *dtaara
017700060810e   1c                   endif
017800060810      *-- Verifica errori e autorità profilo
017900060810sel 1c                   SELECT
018000060810      *-- controllo se ho errori nei dati utente
018100060810      *--   nel qual caso NON risulta un profilo abilitato
018200060810w   1c                   WHEN      DUTerr  = 'E'
018300060810     c                   eval      $Fine   = *on
018400060810      *
018500060810      *-- CONTROLLO AUTORITA'
018600060810      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
018700060810      *-- se il chiamante non richiede autorità specifica verificare
018800060810      *--   quella del profilo
018900060810      *-- se il chiamante richiede autorità specifica verificarla,
019000060810      *--  se è blank verificare quella del profilo
019100060810      *
019200060810      * se UTEAUT = *BLANK non siamo in sede
019300060810w   1c                   WHEN      UTEaut  = *blank
019400081008     c                   eval      $Fine   = *on
019500060810      *
019600060810x   1c                   OTHER
019700060810      *
019800060810e   1c                   ENDSL
019900060810      *
020000060810      * Aggancio dati generali della tabella in esame
020100060810     c                   clear                   TBEcod
020200060810     c                   move      *zeros        TBEke1
020300091102     c                   move      C_CodTab      TBEke1
020400060810     c                   clear                   TBEke2
020500060810     c                   clear                   TBElin
020600060810     c                   movel     KNSIF         TBEsif
020700060810     c     K05TBE01      chain     TNTBE01L
020800060810     c                   if        not %found(TNTBE01L)
020900060810     c                   clear                   TBEsif
021000060810     c     K05TBE01      chain     TNTBE01L
021100060810     c                   endif
021200060810     c                   if        %found(TNTBE01L)
021300060810     c                   movel     TNTBEds       xTNTBEds
021400060810     c                   else
021500060810     c                   clear                   xTNTBEds
021600060810     c                   endif
021700060810      *
021800060810     c                   ENDSR
021900060810
022000091028      *---------------------------------------------------------------*
022100091028      * GESS01 - Gestione subfile                                     *
022200091028      *---------------------------------------------------------------*
022300060810     c     GesS01        BEGSR
022400060810      *
022500060810      * Inizializzazione videata
022600060810if  1c                   if        $InzS01 = *on
022700060810     c                   exsr      InzS01
022800060810     c                   movel     *off          $InzS01
022900060810e   1c                   endif
023000060810      *
023100060810      * Scrivo la testata e la riga tasti funzionali abilitati
023200060810if  1c                   if        NOT *in90
023300160929     c                   write     TB03T01
023400160929     c                   write     TB03P01
023500060810e   1c                   endif
023600060810      *
023700060810      * Posiziono il cursore
023800091223     c                   eval      *in30       = (C01rcd <= *zeros and
023900091223     c                                         s01nrr = *zeros )
024000060810w   1c                   if        C01csr  > *zeros
024100060810     c                   eval      C01rcd  = C01csr
024200060810     c                   else
024300060905     c                   eval      C01rcd  = 1
024400060810     c                   endif
024500091223     c                   if        s01nrr = 0
024600091223     c                   eval      s01nrr  = 1
024700091223     c                   end
024800060810      *
024900060810      * Emetto la videata
025000160929     c                   exfmt     TB03C01
025100060810      *
025200060810     c                   setoff                                       28  90
025300060810     c                   clear                   V1Dmsg
025400060810      *
025500060810sel 1c                   SELECT
025600060810      *
025700060810      * F3=Fine
025701161107w   1c                   WHEN      *inKc
025702161107     c                   move      '03'          kpjbu
025703161107     c                   return
025704161107      *
025705161108      * F12=Ritorno o nessun dato
025800161108w   1c                   WHEN      *inKl or *in30
025900060810     c                   exsr      F03S01
026000060810      *
026100060810      * Roll-Up
026200060810w   1c                   WHEN      £Tasto  = RollUp
026300060810do  2c                   do        C_SflPag
026400060810     c                   exsr      RecS01
026500060810if  3c                   if        *in33
026600060810     c                   leave
026700060810e   3c                   endif
026800060810e   2c                   enddo
026900060810     c     S01nrr        div       C_SflPag      wPage
027000060810     c                   mvr                     wRow
027100060811     c                   if        wRow    = *zeros
027200060810     c                   eval      C01csr  = (C_SflPag * (wPage - 1))
027300060810     c                                     + 1
027400060811     c                   else
027500060811     c                   eval      C01csr  = (C_SflPag * wPage)
027600060811     c                                     + 1
027700060811     c                   endif
027800060810      *
027900060810      * Controllo dati immessi a video
028000060810x   1c                   OTHER
028100060810     c                   exsr      CtrC01
028200060810      * - Rilevati Errori
028300060810if  2c                   if        *in90
028400060810     c                   leavesr
028500060810e   2c                   endif
028600060810      *
028700060810e   1c                   ENDSL
028800060810      *
028900060810     c                   ENDSR
029000060810
029100060810      *---------------------------------------------------------------*
029200060810      * INZS01 - Inizializzazione subfile (S01)                       *
029300060810      *---------------------------------------------------------------*
029400060810     c     InzS01        BEGSR
029500060810      *
029600060810      * Pulizia subfile
029700060810     c                   seton                                        3031
029800160929     c                   write     TB03C01
029900060810     c                   setoff                                         31
030000060810     c                   eval      *in33   = *off
030100060810      *
030101161108     c                   clear                   savke1
030102161108     c                   z-add     0             S01nrr
030200060810     c                   clear                   C01rcd
030300060810     c                   clear                   C01csr
030400060810     c                   clear                   V1Dmsg
030500060810     c                   setoff                                       28  90
030600060811     c                   movea     *zeros        *in(40)
030700091030      *
030800060810      * Caricamento dati già immessi (protetti)
030900091029     c
031000091028     c     keytab        setll     TNTBE000
031100091028     c                   do        *hival
031200091028     c     keytab        reade     TNTBE000
031300091028     c                   exsr      RecS01
031400091028     c   33              leave
031500091028e   1c                   ENDDO
031600091223     c*
031700091223     c                   if        s01nrr = 0
031800091223     c                   leavesr
031900091223     c                   end
032000061221      *
032100061221      * Se pgm. richiamato in interrogazione NON consento l'immissione
032200061221      *   di altri dati
032300091223if  0c                   IF        wOpz    = '05'
032400061221      *
032500061221     c                   eval      *in33   = *on
032600061221      *
032700061221x   0c                   ELSE
032800060810      *
032900060810      * Aggiunta di righe libere a completamento della pagina
033000060810     c     S01nrr        div       C_SflPag      wPage
033100060810     c                   mvr                     wRow
033200060810do  1c                   dow       S01nrr  < (C_SflPag * (wPage + 1))
033300060810     c                             and  NOT *in33
033400060810     c                   exsr      RecS01
033500060810e   1c                   enddo
033600060810      *
033700060905      * Imposta il posizionamento cursore sul 1° rec libero del sfl
033800060811if  1c                   if        C01csr  > *zeros
033900060905     c                   eval      C01csr  = 1
034000060810e   1c                   endif
034100061221      *
034200061221e   0c                   ENDIF
034300060901      *
034400060901      * Impostazione eventuale testo di avvertimento...
034500060901     c                   exsr      sr_TxtC01
034600061221      *
034700061221      * Abilitazione tasti funzionali
034800061221      * - F6 = Conferma
034900061221     c                   eval      *in06   = (wOpz <> '05')
035000060810      *
035100060810     c                   ENDSR
035200060810
035300060810      *---------------------------------------------------------------*
035400060810      * RECS01 - Caricamento singolo record del subfile S01           *
035500060810      *---------------------------------------------------------------*
035600060810     c     RecS01        BEGSR
035601161107      *
035700160929     c                   clear                   TB03S01
035800091028     c                   setoff                                       33
035801161107      *
035900091028if  1c                   if        not %eof(TNTBE01L)
036000081008     c                   movel     TBEke1        v1Cke1
036009161107      ***
036100160928     c                   movel     TBEuni        dagl
036200091028     c                   move      mtab          mvid
036201161107      *
036300091028     c                   if        tbeke2 <> ' '
036400091029     c                   movel     tbeke2        com08             8
036500091029     c                   move      com08         comdta            8 0
036600160929      *in lettura se la data scadenza è minore di udate non faccio più vedere la riga
036700091028     c                   move      comdta        dtaiso
036800091028     c                   move      dtaiso        dtaeur
036900091029     c                   move      dtaeur        v1cke2
037000091030     c                   move      dtaeur        h1cke2
037001161107      *
037100160929     c                   if        dtaiso < wdate
037200160929     c                   goto      salta
037300160929     c                   endif
037400091028     c                   end
037401161107      *
037500160929     c                   movel     v1cke1        vfil              3 0
037600160929     c     vfil          chain     azorg01l
037700160929     c                   eval      v1cke1d =  orgdes
037800081008     c                   eval      H1Ctbe  = *on
037801161107      ***
037802161107     c                   seton                                        55
037803161107     c                   if        v1cke1 <> savke1
037804161107     c                   movel     v1cke1        savke1
037805161107     c                   setoff                                       55
037806161107     c                   end
037807161107      ***
037808161107     c                   eval      hin55 = *in55
037900091030     c                   else
038000091028     c                   seton                                        33
038100091028e   1c                   END
038200081007     c*
038300081007      * Scrittura singolo record nel subfile S01
038400160929     c  n33              exsr      WrtS01
038500060901      *
038600160929     c     salta         tag
038700060901     c                   ENDSR
038800060901
038900060901      *---------------------------------------------------------------*
039000060901      * WRTS01 - Scrittura singolo record nel subfile S01             *
039100060901      *---------------------------------------------------------------*
039200060901     c     WrtS01        BEGSR
039300060901      *
039400060810      * Attributi di visualizzazione
039500060810      * - record già immesso
039600060810     c                   eval      *in40   = (H1Ctbe = *on)
039700081007     c                   add       1             S01nrr
039800160929     c                   write     TB03S01
039900060810      *
040000060810      * Impostazione del SflEnd
040100081007     c                   if        s01nrr = 9999
040200081007     c                   seton                                        33
040300081007     c                   end
040400060810      *
040500060810     c                   ENDSR
040600060901
040700060901      *---------------------------------------------------------------*
040800060901      * sr_TxtC01 - Impostazione testo di avvertimento in C01         *
040900060901      *---------------------------------------------------------------*
041000060901     c     sr_TxtC01     BEGSR
041100060901      *
041200160929     c                   eval      vidopz = 'Opzioni: 2=MODIFICA'
041300060901      *
041400060901     c                   ENDSR
041500060810
041600060810      *---------------------------------------------------------------*
041700060810      * CTRC01 - Controllo di tutto il subfile                        *
041800060810      *---------------------------------------------------------------*
041900060810     c     CtrC01        BEGSR
042000060810      *
042100091030     c                   clear                   sdup
042200081008     c                   z-add     0             ok                1 0
042300081008do  1c                   DO        *hival        xx
042400160929     c     xx            chain     TB03S01                            29
042500081008     c                   if        *in29
042600081008     c                   leave
042700081008     c                   end
042800060810     c                   z-add     S01nrr        C01rcd
042900160929      * richiama tuti i rekord di una filiale
043000160929     c                   if        v1copz = '2'
043100160929     c                   movel     v1cke1        kpjbu
043200160929     c                   call      'TNTBA03R'
043300160929     C                   parm                    kpjba
043301161107     c                   move      kpjbu         cmd02             2
043302161107      * F3=Fine
043303161107     c                   if        cmd02='03'
043304161107     c                   return
043305161107     c                   end
043400160929     c                   clear                   v1copz
043500160929     c                   endif
043600060810      *
043700060810     c                   exsr      CtrS01
043800060810      *
043900060810      * Reimpostazione indicatori per DSPATR
044000060810      * - record già immesso
044100060810     c                   eval      *in40   = (H1Ctbe = *on)
044200091029      * - moltiplicatore errato
044300091030     c                   eval      *in41   = (H1in41 = *on)
044400091030     c                   eval      *in42   = (H1in42 = *on)
044500091030      * - gg mensili
044600091030     c                   eval      *in43   = (H1in43 = *on)
044700091030      * - data errata
044800091030     c                   eval      *in44   = (H1in44 = *on)
044900060810      * Aggiornamento sfl
045000060810if  2c                   if        *in28  OR  *in90
045100060810     c                   z-add     C01rcd        C01csr
045200060810e   2c                   endif
045201161107     c                   eval        *in55 = hin55
045300160929     c                   UPDATE    TB03S01
045400060810if  2c                   if        *in28  OR  *in90
045500060810     c                   leave
045600060810e   2c                   endif
045700060810      *
045800060810e   1c                   ENDDO
045900060901      *
046000060901      * Impostazione eventuale testo di avvertimento...
046100060901     c                   exsr      sr_TxtC01
046102161107if  1c                   eval      $InzS01 = *on
046200060810      *
046300060810     c                   ENDSR
046400060810
046500060810      *---------------------------------------------------------------*
046600060810      * CTRS01 - Controllo del singolo record del subfile             *
046700060810      *---------------------------------------------------------------*
046800060810     c     CtrS01        BEGSR
046900060810      *
047000081008     c                   setoff                                       414243
047100091030     c                   setoff                                       44
047200081008     c                   eval      savin41= *off
047300081008     c                   eval      savin42= *off
047400081008     c                   eval      savin43= *off
047500081008     c                   eval      savin44= *off
047600081008     c                   eval      h1in41 = *off
047700081008     c                   eval      h1in42 = *off
047800081008     c                   eval      h1in43 = *off
047900081008     c                   eval      h1in44 = *off
048000091030     C                   XFOOT     MVID          COMODO            4 0
048100081008     c*
048200091030if  1c                   if             comodo  = *zeros
048300081008     c                   z-add     1             ok
048400081008     c                   leavesr
048500081008e   1c                   endif
048600091030      *?SEGUONO CONTROLLI DA ESEGUIRE SOLO PER I RECORDS CON DATI?
048700060810      *
048800160928if  1c                   if        h1ctbe = *off
048900091102      * DATA
049000091102if  2c                   if        V1CKE2 =  *zeros
049100091102     c                   seton                                        284490
049200091102     c                   eval      H1in44  = *in44
049300091102     c                   eval      SAVin44 = *on
049400091102     c                   eval      V1Dmsg  = $Msg(1)
049500091102     c                   leavesr
049600160928    2C                   ELSE
049700091102     C     *EUR          test(d)                 V1CKE2                 44
049800160928    3c                   if        *in44
049900091102     c                   seton                                        2890
050000091102     c                   eval      H1in44  = *in44
050100091102     c                   eval      SAVin44 = *on
050200091102     c                   eval      V1Dmsg  = $Msg(1)
050300091102     c                   leavesr
050400160928    3c                   else
050500091102     c* non deve essere retroattiva e deve essere fine mese
050600091102     c                   move      v1cke2        dtaeur
050700091102     c                   move      dtaeur        dtaiso
050800160928    4c                   if        dtaiso < wdate and v1cke2 <> h1cke2
050900091102     c                   seton                                        289044
051000091102     c                   eval      H1in44  = *in44
051100091102     c                   eval      SAVin44 = *on
051200091102     c                   eval      V1Dmsg  = $Msg(3)
051300091102     c                   leavesr
051400160928    4c                   else
051500091102     c                   extrct    dtaiso:*m     mesep             2 0
051600091102     c                   adddur    1:*d          dtaiso
051700091102     c                   extrct    dtaiso:*m     mesed             2 0
051800160928    5c                   if        mesep = mesed
051900091102     c                   seton                                        289044
052000091102     c                   eval      H1in44  = *in44
052100091102     c                   eval      SAVin44 = *on
052200091102     c                   eval      V1Dmsg  = $Msg(3)
052300091102     c                   leavesr
052400160928    5C                   end
052500160928    4C                   end
052600160928    3C                   end
052700160928    2C                   end
052800091102     c* non ci devono essere righe doppie
052900091030     c                   move      v1cke2        $tbekey
053000091102     c     $tbekey       lookup    sdup                                   44
053100160928     c                   if        *in44
053200091102     c                   seton                                        2890
053300091102     c                   eval      H1in44  = *in44
053400091030     c                   eval      SAVin44 = *on
053500091030     c                   eval      V1Dmsg  = $Msg(5)
053600091030     c                   leavesr
053700160928     C                   endif
053800091030e   1c                   endif
053900091102     c* carico la schiera per verificare l'inserimento di righe doppie
054000091102     c                   movel     v1cke1        sdup(s01nrr)
054100091102     c                   move      v1cke2        sdup(s01nrr)
054200060810      *
054300060810     c                   ENDSR
054400060810
054500060810      *---------------------------------------------------------------*
054600060810      * F03S01 - Tasto funzionale F03 -> Fine programma               *
054700060810      *---------------------------------------------------------------*
054800060810     c     F03S01        BEGSR
054900060810      *
055000060810     c                   movel     *on           $Fine                          fine pgm
055001161107     c                   move      '12'          kpjbu                          fine pgm
055100060810      *
055200060810     c                   ENDSR
055300060810
055400060810** - $Msg -------------------------------------------------------------------*
055500091030Data scadenza errata
055600091030E' obbligatorio inserire almeno un gg
055700091030La data non può essere retroattiva o diversa da fine mese
055800091030Moltiplicatore fisso errato
055900091030Moltiplicatore fisso doppio
