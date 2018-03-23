000100060810      *---------------------------------------------------------------*
000200160930      * Gestione tabella "AIG"
000300060810      *---------------------------------------------------------------*
000400060810     h decedit('0,') datedit(*dmy/)
000500060810      *---------------------------------------------------------------*
000600060810      *   A R C H I V I                                               *
000700060810      *---------------------------------------------------------------*
000800160928     fazorg01l  if   e           k disk
000900160929     fTNTBE01L  if   e           k disk
001000161028     fTNTBE02L  if   e           k disk    rename(TNTBE000:TNTBE2)
001100161028     f                                     prefix(T2_)
001200060810      *
001300160930     fTNTBa04D1 cf   e             workstn
001400160929     f                                     sfile(TB03S01:S01nrr)
001500060810     f                                     infds(DSFMT)
001600060810      *---------------------------------------------------------------*
001700060810      *   C O S T A N T I                                             *
001800060810      *---------------------------------------------------------------*
001900060810      * - Codice tabella in gestione
002000160930     d C_CodTab        c                   const('AIG')
002100060810      * - Nr. di righe in ogni pagina del sfl
002200060810     d C_SflPag        c                   const(18)
002300060810      * - Tasti funzionali
002400060810     d RollUp          c                   const(x'F5')
002500060811      * Attributi video
002600060811     d C_Atr_Norm      c                   const(X'A0')
002700060811     d C_Atr_BL_RI     c                   const(X'A9')
002800060810
002900060810      *---------------------------------------------------------------*
003000060810      *   S C H I E R E                                               *
003100060810      *---------------------------------------------------------------*
003200091030     d wDate           s               d   datfmt(*iso)  inz(*loval)
003300060810      * - Messaggi di errore
003400091030     d $Msg            s             78    dim(  5) ctdata perrcd(1)
003500091030     c* schiera per individuare record doppi
003600091030     d sdup            s             11    dim(9999)
003700060810
003800060810      *---------------------------------------------------------------*
003900060810      *   S T R U T T U R E   D A T I                                 *
004000060810      *---------------------------------------------------------------*
004100060810      * Parametri
004200060810     d KPJBA         e ds
004300161028      **
004400161028      * Parametri in ricezione
004500161028     D  TABDS          DS
004600161028     D  XTAOPZ                 1      2
004700161028     D  XTARET                 3      3
004800161028     D  XTAOPR                 4      4
004900161028     D  XTAERR                 5      5
005000161028     D  XTAKE1                 6     20
005100161028     D  XTAKE2                21     35
005200161028      *-------------
005300161027     D tibs02ds      E DS
005400091028      * Tabella
005500161027     d dAIG          e ds                  inz
005600161027     D dTAI          E DS
005700161027     D dATS          E DS
005800161027     D dMAS          E DS
005900161027     D dAAT          E DS
006000161027     D dALL          E DS
006100161027     d TNTBA04DS     e ds
006200060810      *
006300060810      * Tracciato record file TNTBE00F
006400060810     d TNTBEds       e ds                  extname(TNTBE00F) inz
006500060810     d xTNTBEds      e ds                  extname(TNTBE00F) inz
006600060810     d                                     prefix(TBX:3)
006700060810      *
006800060810     d TIBS34ds      e ds                  inz
006900060810     d dDatiUte      e ds                  inz
007000060810     d AZUTEds       e ds                  extname(AZUTE00F) inz
007100060810      *
007200060810     d Status         sds
007300060810     d  VTCpgm           *proc
007400060810      *
007500060810     d DSFMT           ds           512
007600060810     d  £Tasto               369    369
007700060810     d  £SFLnrr              378    379B 0
007800060810      *
007900161027     d dschiave        ds            15
008000161027     d  dskfil                 1      3
008100161027     d  dskasp                 4      4
008200161027     d  dskter                 5      5
008300161027     d  dskmas                 6      6
008400161027     d  dskaat                 7      7
008500161027     d  dskall                 8      8
008600161027      *
008700091030     d $tbecod         s                   like(tbecod)
008800161104     d $tbeke1         s                   like(tbeke1)
008900161104     d $tbeke2         s                   like(tbeke2)
009000060810      *---------------------------------------------------------------*
009100060810      *   V A R I A B I L I                                           *
009200060810      *---------------------------------------------------------------*
009300060810      * - Flags
009400060810     d $Fine           s              1a   inz(*off)
009500060810     d $InzS01         s              1a   inz(*on)
009600060810      *
009700060810      * - Indici di schiera
009800060810     d xx              s              5  0 inz
009900060810      *
010000060810      * - Variabili riferite al data base o al display file
010100060811     d S01nrr          s                   like(C01rcd)  inz
010200060810      *
010300060810      * - Variabili definite a programma
010400091029     d dtaiso          s               d   datfmt(*iso)  inz(*loval)
010500091029     d dtaeur          s               d   datfmt(*eur)  inz(*loval)
010600060810     d wPage           s              4  0 inz
010700060810     d wRow            s              3  0 inz
010800061221     d wOpz            s              2    inz
010900060810      *---------------------------------------------------------------*
011000060810      *   M A I N   L I N E                                           *
011100060810      *---------------------------------------------------------------*
011200060810      *  Riepilogo indicatori utilizzati:                             *
011300060810      *  --------------------------------                             *
011400061221      *  06 - F6 abilitato                                            *
011500060810      *  10 - Comodo                                                  *
011600060810      *  22 - Errori in scrittura record (WRITE)                      *
011700161104      *  25 - Alta intensità la data Scadenza in vigore               *
011800061221      *  28 - Emissione messaggio di errore a video                   *
011900061221      *  30 - Pulizia subfile                                         *
012000061221      *  31 - NON emissione del subfile                               *
012100061221      *  33 - Fine dati nel subfile         (SFLEND)                  *
012200060810      *  90 - Riemissione videata                                     *
012300060810      *---------------------------------------------------------------*
012400161028     c                   eval      *inLR   = *on
012500060810      * Operazioni iniziali
012600060810     c                   exsr      RoutInz
012700060810      *
012800060810      * Gestione video
012900060810     c                   dow       $Fine   = *off
013000060810     c                   exsr      GesS01
013100060810     c                   enddo
013200060810      * Fine
013300161028     c                   eval      KPJBU = TNTBA04DS
013400161028     c                   return
013500060810      *---------------------------------------------------------------*
013600060810      * RoutInz - Operazioni Iniziali                                 *
013700060810      *---------------------------------------------------------------*
013800060810     c     RoutInz       BEGSR
013900060810      *
014000060810      * Ricezione parametri
014100060810     c     *entry        plist
014200060810     c                   parm                    KPJBA
014300161027     c                   movel     KPJBU         TNTBA04DS
014400060810      *
014500060810      * Definizioni chiavi di accesso
014600060810     c     K05TBE01      klist                                                  *tntbe01l
014700060810     c                   kfld                    TBEcod                         -tabella
014800060810     c                   kfld                    TBEke1                         -chiave uno
014900060810     c                   kfld                    TBEke2                         -chiave due
015000060810     c                   kfld                    TBElin                         -lingua
015100060810     c                   kfld                    TBEsif                         -s.informativo
015200161027      **
015300091029      * Definizioni chiavi di accesso
015400091029     c     Keytab        klist                                                  *tntbe01l
015500091030     c                   kfld                    $tbecod                        -tabella
015600160930     c                   eval      $tbecod = 'AIG'
015700061221      *
015800061221      * Imposto eventuali parametri ricevuti
015900091030     c                   movel     *date         Wdate
016000060810      *
016100060810      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
016200060810     c     *dtaara       define    §azute        AZUTEds
016300060810     c     *dtaara       define    §datiute      dDatiUte
016400060810      *
016500060810     c                   clear                   AZUTEds
016600060810     c                   clear                   dDatiUte
016700060810     c                   clear                   TIBS34ds
016800060810     c                   in(E)     *dtaara
016900060810if  1c                   if        %Error  or  RSUT = *blanks
017000060810     c                   call      'TIBS34R'
017100060810     c                   parm                    TIBS34ds
017200060810     c                   in        *dtaara
017300060810e   1c                   endif
017400161027      *
017500060810      *-- Verifica errori e autorità profilo
017600060810sel 1c                   SELECT
017700060810      *-- controllo se ho errori nei dati utente
017800060810      *--   nel qual caso NON risulta un profilo abilitato
017900060810w   1c                   WHEN      DUTerr  = 'E'
018000060810     c                   eval      $Fine   = *on
018100060810      *
018200060810      *-- CONTROLLO AUTORITA'
018300060810      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
018400060810      *-- se il chiamante non richiede autorità specifica verificare
018500060810      *--   quella del profilo
018600060810      *-- se il chiamante richiede autorità specifica verificarla,
018700060810      *--  se è blank verificare quella del profilo
018800060810      *
018900060810      * se UTEAUT = *BLANK non siamo in sede
019000060810w   1c                   WHEN      UTEaut  = *blank
019100081008     c                   eval      $Fine   = *on
019200060810      *
019300060810x   1c                   OTHER
019400060810      *
019500060810e   1c                   ENDSL
019600060810      *
019700060810      * Aggancio dati generali della tabella in esame
019800060810     c                   clear                   TBEcod
019900060810     c                   move      *zeros        TBEke1
020000091102     c                   move      C_CodTab      TBEke1
020100060810     c                   clear                   TBEke2
020200060810     c                   clear                   TBElin
020300060810     c                   movel     KNSIF         TBEsif
020400060810     c     K05TBE01      chain     TNTBE01L
020500060810     c                   if        not %found(TNTBE01L)
020600060810     c                   clear                   TBEsif
020700060810     c     K05TBE01      chain     TNTBE01L
020800060810     c                   endif
020900060810     c                   if        %found(TNTBE01L)
021000060810     c                   movel     TNTBEds       xTNTBEds
021100060810     c                   else
021200060810     c                   clear                   xTNTBEds
021300060810     c                   endif
021400060810      *
021500060810     c                   ENDSR
021600060810
021700091028      *---------------------------------------------------------------*
021800091028      * GESS01 - Gestione subfile                                     *
021900091028      *---------------------------------------------------------------*
022000060810     c     GesS01        BEGSR
022100060810      *
022200060810      * Inizializzazione videata
022300060810if  1c                   if        $InzS01 = *on
022400060810     c                   exsr      InzS01
022500060810     c                   movel     *off          $InzS01
022600060810e   1c                   endif
022700060810      *
022800060810      * Scrivo la testata e la riga tasti funzionali abilitati
022900060810if  1c                   if        NOT *in90
023000160929     c                   write     TB03T01
023100160929     c                   write     TB03P01
023200060810e   1c                   endif
023300060810      *
023400060810      * Posiziono il cursore
023500091223     c                   eval      *in30       = (C01rcd <= *zeros and
023600091223     c                                         s01nrr = *zeros )
023700060810w   1c                   if        C01csr  > *zeros
023800060810     c                   eval      C01rcd  = C01csr
023900060810     c                   else
024000060905     c                   eval      C01rcd  = 1
024100060810     c                   endif
024200091223     c                   if        s01nrr = 0
024300161028     c                   eval      a04MSG ='Nessun record per la selezione'
024400161028     c                   eval      KPJBU = TNTBA04DS
024500161028     c                   RETURN
024600091223     c                   end
024700161028      *-----
024800060810      *
024900060810      * Emetto la videata
025000160929     c                   exfmt     TB03C01
025100060810      *
025200060810     c                   setoff                                       28  90
025300060810     c                   clear                   V1Dmsg
025400060810      *
025500060810sel 1c                   SELECT
025600060810      * F3=Fine
025700161028w   1c                   WHEN      *inKc
025800060810     c                   exsr      F03S01
025900161028      * F12=Fine
026000161028w   1c                   WHEN      *inKl
026100161028     c                   exsr      F12S01
026200161028      * F10=Inserimento
026300161028w   1c                   WHEN      *inKj
026400161028     c                   exsr      F10S01
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
029000060810      *
029100060810e   1c                   ENDSL
029200060810      *
029300060810     c                   ENDSR
029400060810
029500060810      *---------------------------------------------------------------*
029600060810      * INZS01 - Inizializzazione subfile (S01)                       *
029700060810      *---------------------------------------------------------------*
029800060810     c     InzS01        BEGSR
029900060810      *
030000060810      * Pulizia subfile
030100060810     c                   seton                                        3031
030200160929     c                   write     TB03C01
030300060810     c                   setoff                                         31
030400060810     c                   eval      *in33   = *off
030500060810      *
030600161031     c                   clear                   S01nrr
030700060810     c                   clear                   C01rcd
030800060810     c                   clear                   C01csr
030900060810     c                   clear                   V1Dmsg
031000161031     c                   clear                   savfil            3
031100161104     c                   clear                   $tbeke1
031200060810     c                   setoff                                       28  90
031300091030      *
031400060810      * Caricamento dati già immessi (protetti)
031500091029     c
031600091028     c     keytab        setll     TNTBE000
031700091028     c                   do        *hival
031800091028     c     keytab        reade     TNTBE000
031900091028     c                   exsr      RecS01
032000091028     c   33              leave
032100091028e   1c                   ENDDO
032200091223     c*
032300091223     c                   if        s01nrr = 0
032400091223     c                   leavesr
032500091223     c                   end
032600060810      *
032700060810      * Aggiunta di righe libere a completamento della pagina
032800060810     c     S01nrr        div       C_SflPag      wPage
032900060810     c                   mvr                     wRow
033000060810do  1c                   dow       S01nrr  < (C_SflPag * (wPage + 1))
033100060810     c                             and  NOT *in33
033200060810     c                   exsr      RecS01
033300060810e   1c                   enddo
033400060810      *
033500060905      * Imposta il posizionamento cursore sul 1° rec libero del sfl
033600060811if  1c                   if        C01csr  > *zeros
033700060905     c                   eval      C01csr  = 1
033800060810e   1c                   endif
033900060901      *
034000060901      * Impostazione eventuale testo di avvertimento...
034100161028     c                   eval      vidopz = '2=Modifica'
034200060810      *
034300060810     c                   ENDSR
034400060810
034500060810      *---------------------------------------------------------------*
034600060810      * RECS01 - Caricamento singolo record del subfile S01           *
034700060810      *---------------------------------------------------------------*
034800060810     c     RecS01        BEGSR
034900161027      **
035000160929     c                   clear                   TB03S01
035100091028     c                   setoff                                       33
035200161027      **
035300091028if  1c                   if        not %eof(TNTBE01L)
035400161027      ****  filtra in base alle selezioni
035500161027     c                   movel     TBEke1        dschiave
035600161027      **
035700161028     c                   if           A04FIL <>'000' and
035800161027     c                                A04FIL <> dskFIL
035900161027     c                   LeaveSR
036000161027     c                   end
036100161027     c                   if           A04TAI <> *blank and
036200161027     c                                A04TAI <> dskasp
036300161027     c                   LeaveSR
036400161027     c                   end
036500161027     c                   if           A04ATS <> *blank and
036600161027     c                                A04ATS <> dskter
036700161027     c                   LeaveSR
036800161027     c                   end
036900161027     c                   if           A04MAS <> *blank and
037000161027     c                                A04MAS <> dskmas
037100161027     c                   LeaveSR
037200161027     c                   end
037300161027     c                   if           A04AAT <> *blank and
037400161027     c                                A04AAT <> dskaat
037500161027     c                   LeaveSR
037600161027     c                   end
037700161027      **
037800161027     c                   if           A04ALL <> *blank and
037900161027     c                                A04ALL <> dskall
038000161027     c                   LeaveSR
038100161027     c                   end
038200161027      **  controlla la DATA
038300161027     c                   if        tbeke2 <> ' '
038400161027     c                   movel     tbeke2        com08             8
038500161027     c                   move      com08         comdta            8 0
038600161027     c                   move      comdta        dtaiso
038700161027     c                   move      dtaiso        dtaeur
038800161027     c                   move      dtaeur        v1cke2
038900161027     c                   move      dtaeur        h1cke2
039000161027      *in lettura se la data scadenza è minore di udate non faccio più vedere la riga
039100161027     c                   if        dtaiso < wdate
039200161027     c                   LeaveSR
039300161027     c                   endif
039400161027     c                   end
039500161027      **    Carica campi
039600161027     c                   movel     TBEke1        h1Cke1hid
039700161027     c                   movel     dskFIL        v1Cfil
039800161031      **
039900161031      ** protegge la filiale
040000161031     c                   seton                                        11
040100161031     c                   if        v1cfil <> savfil
040200161031     c                   eval      savfil = v1cfil
040300161031     c                   setoff                                       11
040400161031     c                   end
040500161104     c                   eval      H1in11 = *in11
040600161104      **
040700161104      ** evidenzia la data in vigore appena superiore alla UDATE
040800161104      **  ossia alla rottura della prima chiave
040900161104     c                   setoff                                       25
041000161104     c                   if        tbeke1 <> $tbeke1
041100161104     c                   eval      $tbeke1 = tbeke1
041200161104     c                   seton                                        25
041300161104     c                   end
041400161104     c                   eval      H1in25 = *in25
041500161027      **
041600161028     c                   if          dskfil = '000'
041700161027     c                   eval      v1cfild =  'Tutte'
041800161028      **
041900161028     c                   elseif      dskfil = '999'
042000161028     c                   eval      v1cfild =  'Azienda'
042100161028      **
042200161028     c                   else
042300161027      **
042400161027     c                   movel     v1cfil        vfil              3 0
042500161027     c     vfil          chain     azorg01l
042600161027     c                   if        %Found(azorg01l)
042700161027     c                   eval      v1cfild =  orgdes
042800161027     c                   end
042900161027     c                   endif
043000161027      **
043100161027      **  decodifica i singoli flags
043200161027      *
043300161027      **  Tipo aspettativa
043400161027     C                   CLEAR                   tibs02ds
043500161027     C                   MOVEL     KNSIF         T02SIF
043600161027     C                   MOVEL     'TAI'         T02COD
043700161027     C                   MOVEL     'C'           T02MOD
043800161027     C                   movel(p)  dskasp        T02KE1
043900161027     c                   if        dskasp <> *blank
044000161027     C                   CALL      'TIBS02R'
044100161027     C                   PARM                    KPJBA
044200161027     C                   PARM                    tibs02ds
044300161027     C     T02ERR        IFEQ      *BLANKS
044400161027     C                   MOVEL     T02UNI        dtai
044500161027     c                   movel     d§taides      v1Casp
044600161027     c                   movel     dskasp        h1Casp
044700161027     C                   ELSE
044800161027     c                   clear                   v1Casp
044900161027     c                   clear                   h1Casp
045000161027     C                   CLEAR                   Dtai
045100161027     C                   ENDIF
045200161027     c                   end
045300161027      *
045400161027      **  Territorio
045500161027     C                   CLEAR                   tibs02ds
045600161027     C                   MOVEL     KNSIF         T02SIF
045700161027     C                   MOVEL     'ATS'         T02COD
045800161027     C                   MOVEL     'C'           T02MOD
045900161027     C                   movel(p)  dskter        T02KE1
046000161027     c                   if        dskter <> *blank
046100161027     C                   CALL      'TIBS02R'
046200161027     C                   PARM                    KPJBA
046300161027     C                   PARM                    tibs02ds
046400161027     C     T02ERR        IFEQ      *BLANKS
046500161027     C                   MOVEL     T02UNI        dats
046600161027     c                   movel     d§atsdes      v1Cter
046700161027     c                   movel     dskter        h1Cter
046800161027     C                   ELSE
046900161027     c                   clear                   v1Cter
047000161027     C                   CLEAR                   h1Cter
047100161027     C                   ENDIF
047200161027     c                   end
047300161027      *
047400161027      **  Massa
047500161028     c     'MAS'         setll     tntbe02l
047600161028     c     'MAS'         reade     tntbe02l
047700161028     c                   dow       not %EoF(tntbe02l)
047800161028     c                   if        dskMas = %subst(T2_tbeKE2:1:1)
047900161028     C                   MOVEL     T2_tbeUNI     Dmas
048000161027     c                   movel     d§masdes      v1cMas
048100161027     c                   movel     dskMas        h1cMas
048200161027     c                   leave
048300161027     c                   end
048400161028     c     'MAS'         reade     tntbe02l
048500161027     c                   end
048600161027      *
048700161027      **  Tipo Attività
048800161027     C                   CLEAR                   tibs02ds
048900161027     C                   MOVEL     KNSIF         T02SIF
049000161027     C                   MOVEL     'AAT'         T02COD
049100161027     C                   MOVEL     'C'           T02MOD
049200161027     C                   movel(p)  dskaat        T02KE1
049300161027     c                   if        dskaat <> *blank
049400161027     C                   CALL      'TIBS02R'
049500161027     C                   PARM                    KPJBA
049600161027     C                   PARM                    tibs02ds
049700161027     C     T02ERR        IFEQ      *BLANKS
049800161027     C                   MOVEL     T02UNI        Daat
049900161027     c                   movel     d§aatdes      v1cAat
050000161027     c                   movel     dskaat        h1cAat
050100161027     C                   ELSE
050200161027     c                   clear                   v1cAat
050300161027     C                   CLEAR                   h1cAat
050400161027     C                   ENDIF
050500161027     c                   end
050600161027      *
050700161027      **  Tipo Allestimento
050800161027     C                   CLEAR                   tibs02ds
050900161027     C                   MOVEL     KNSIF         T02SIF
051000161027     C                   MOVEL     'ALL'         T02COD
051100161027     C                   MOVEL     'C'           T02MOD
051200161027     C                   movel(p)  dskall        T02KE1
051300161027     C                   if        dskall <> *blank
051400161027     C                   CALL      'TIBS02R'
051500161027     C                   PARM                    KPJBA
051600161027     C                   PARM                    tibs02ds
051700161027     C     T02ERR        IFEQ      *BLANKS
051800161027     C                   MOVEL     T02UNI        Dall
051900161027     c                   movel     d§alldes      v1Call
052000161027     c                   movel     dskall        h1Call
052100161027     C                   ELSE
052200161027     c                   clear                   v1Call
052300161027     C                   CLEAR                   h1Call
052400161027     C                   ENDIF
052500161027     c                   end
052600161028      **    al momento lo riempio di puntini
052700161028     c                   eval      v1Call = *all'.'
052800161028      **
052900161027      **
053000161027     c                   movel     TBEuni        dAIG
053100161027     c                   z-add     D§aigIMP      v1cIMP
053200170313     c                   z-add     D§aigQTAs     v1cQTAs
053300170313     c                   z-add     D§aigQTAv     v1cQTAv
053400161027     c*
053500081008     c                   eval      H1Ctbe  = *on
053600161027     c*
053700091030     c                   else
053800161027     c*  EoF
053900091028     c                   seton                                        33
054000161027     C                   LEAVEsr
054100091028e   1c                   END
054200081007     c*
054300081007      * Scrittura singolo record nel subfile S01
054400161027     c                   exsr      WrtS01
054500060901      *
054600060901     c                   ENDSR
054700060901
054800060901      *---------------------------------------------------------------*
054900060901      * WRTS01 - Scrittura singolo record nel subfile S01             *
055000060901      *---------------------------------------------------------------*
055100060901     c     WrtS01        BEGSR
055200060901      *
055300081007     c                   add       1             S01nrr
055400160929     c                   write     TB03S01
055500060810      *
055600060810      * Impostazione del SflEnd
055700081007     c                   if        s01nrr = 9999
055800081007     c                   seton                                        33
055900081007     c                   end
056000060810      *
056100060810     c                   ENDSR
056200060810
056300060810      *---------------------------------------------------------------*
056400060810      * CTRC01 - Controllo di tutto il subfile                        *
056500060810      *---------------------------------------------------------------*
056600060810     c     CtrC01        BEGSR
056700060810      *
056800091030     c                   clear                   sdup
056900081008     c                   z-add     0             ok                1 0
057000081008do  1c                   DO        *hival        xx
057100160929     c     xx            chain     TB03S01                            29
057200081008     c                   if        *in29
057300081008     c                   leave
057400081008     c                   end
057500161028      *
057600060810     c                   z-add     S01nrr        C01rcd
057700161028      *
057800160929      * richiama tuti i rekord di una filiale
057900160929     c                   if        v1copz = '2'
058000161028     c                   movel(p)  v1cfil        kpjbu
058100161028     c                   call      'TNTBA04R2'
058200160929     C                   parm                    kpjba
058300161028      **  AL RITORNO PUO AVER FATTO F3:
058400161028      * se sulla destra c'è il tasto funzionale premuto F3 deve uscire dal pgm
058500161028     c                   move      kpjbu         a04FUN
058600161028     c                   if        a04fun = '03'
058700161028     c                   eval      kpjbu = TNTBA04DS
058800161028     c                   RETURN
058900161028     c                   endif
059000161028      *
059100161031     c                   eval      $InzS01 = *on
059200160929     c                   clear                   v1copz
059300160929     c                   endif
059400060810      *
059500060810     c                   exsr      CtrS01
059600060810      *
059700060810      * Aggiornamento sfl
059800060810if  2c                   if        *in28  OR  *in90
059900060810     c                   z-add     C01rcd        C01csr
060000060810e   2c                   endif
060100161104     c                   eval       *in25 =  h1in25
060200161104     c                   eval       *in11 =  h1in11
060300160929     c                   UPDATE    TB03S01
060400060810if  2c                   if        *in28  OR  *in90
060500060810     c                   leave
060600060810e   2c                   endif
060700060810      *
060800060810e   1c                   ENDDO
060900060901      *
061000060901      * Impostazione eventuale testo di avvertimento...
061100161028     c                   eval      vidopz = '2=Modifica'
061200060810      *
061300060810     c                   ENDSR
061400060810
061500060810      *---------------------------------------------------------------*
061600060810      * CTRS01 - Controllo del singolo record del subfile             *
061700060810      *---------------------------------------------------------------*
061800060810     c     CtrS01        BEGSR
061900060810      *
062000060810      *
062100060810     c                   ENDSR
062200060810
062300060810      *---------------------------------------------------------------*
062400161028      * F12S01 - Tasto funzionale F03 -> Fine programma               *
062500060810      *---------------------------------------------------------------*
062600161028     c     F12S01        BEGSR
062700060810      *
062800060810     c                   movel     *on           $Fine                          fine pgm
062900060810      *
063000060810     c                   ENDSR
063100161028      *---------------------------------------------------------------*
063200161028      * F03S01 - Tasto funzionale F03 -> Fine programma               *
063300161028      *---------------------------------------------------------------*
063400161028     c     F03S01        BEGSR
063500161028      *
063600161028     c                   movel     *on           $Fine                          fine pgm
063700161028     c                   move      '03'          a04FUN
063800161028      *
063900161028     c                   ENDSR
064000161028      *---------------------------------------------------------------*
064100161028      * F10S01 - Tasto funzionale F10 -> Inserimento nuovo record     *
064200161028      *---------------------------------------------------------------*
064300161028     c     F10S01        BEGSR
064400161028     C*
064500161028     C                   RESET                   tabds
064600161028     C                   MOVEL     '01'          xtaopz
064700161028     C                   MOVE      *ZERO         xtaret
064800161028     C                   MOVE      *ZERO         xtaopr
064900161031     C                   clear                   xtake1
065000161031     C                   clear                   xtake2
065100161028     C                   MOVE      *BLANKS       KPJBU
065200161028     C                   MOVEL     tabds         KPJBU
065300161028$004 C                   CALL      'TNTBA04R3'
065400161028     C                   PARM                    KPJBA
065500161028     C                   MOVEL     KPJBU         tabds
065600161028      *
065700161028     C* ritorno da PGM gestione
065800161028     C                   EXSR      GESRET
065900161028      *
066000161028     c                   ENDSR
066100161028     C************************************************************
066200161028     C     GESRET        BEGSR
066300161028     C*
066400161028     C* modo di ritorno
0665001610281    C                   SELECT
066600161028    >C* << questi modi di utilizzo dei valori di ritorno dal
066700161028    >C*    pgm di manutenzione rcd di anagrafica sono delle
066800161028    >C*    proposte, normalmente sempre valide, ma modificabili
066900161028    >C*    per situazioni particolari >>
067000161028     C* 1 = F3
067100161028    >C     xtaret        WHENEQ    '1'
067200161028     c                   exsr      F03S01
067300161028     C* 2 = F12
067400161028    >C     xtaret        WHENEQ    '2'
067500161028     c                   exsr      F12S01
0676001610281-   C                   ENDSL
067700161028     C*
067800161028     C* operazione eseguite dal pgm chiamato
0679001610281    C                   SELECT
068000161028     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
068100161028    >C     xtaopr        WHENEQ    '1'
068200161028     c                   eval      $InzS01 = *on
0683001610281-   C                   ENDSL
068400161028     C*
068500161028     C* funzione non eseguibile per errore :
0686001610281    C                   SELECT
068700161028     C* 1 = funzione richiamata chiusa in errore
068800161028    >C*  eventualmente gestire altri codici di errore
068900161028    >C     xtaerr        WHENEQ    '1'
069000161028     c                   eval      a04MSG ='Problemi in inserimento tabella'
069100161028     c                   eval      KPJBU = TNTBA04DS
069200161028     c                   RETURN
0693001610281-   C                   ENDSL
069400161028     C*
069500161028     C                   ENDSR
069600060810
069700060810** - $Msg -------------------------------------------------------------------*
069800091030Data scadenza errata
069900161028E'obbligatorio inserire almeno un gg
070000091030La data non può essere retroattiva o diversa da fine mese
070100091030Moltiplicatore fisso errato
070200091030Moltiplicatore fisso doppio
