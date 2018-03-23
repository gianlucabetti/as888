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
008800091030     d $tbekey         s             11
008900060810      *---------------------------------------------------------------*
009000060810      *   V A R I A B I L I                                           *
009100060810      *---------------------------------------------------------------*
009200060810      * - Flags
009300060810     d $Fine           s              1a   inz(*off)
009400060810     d $InzS01         s              1a   inz(*on)
009500060810      *
009600060810      * - Indici di schiera
009700060810     d xx              s              5  0 inz
009800060810      *
009900060810      * - Variabili riferite al data base o al display file
010000060811     d S01nrr          s                   like(C01rcd)  inz
010100060810      *
010200060810      * - Variabili definite a programma
010300091029     d dtaiso          s               d   datfmt(*iso)  inz(*loval)
010400091029     d dtaeur          s               d   datfmt(*eur)  inz(*loval)
010500060810     d wPage           s              4  0 inz
010600060810     d wRow            s              3  0 inz
010700061221     d wOpz            s              2    inz
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
012000060810      *  90 - Riemissione videata                                     *
012100060810      *---------------------------------------------------------------*
012200161028     c                   eval      *inLR   = *on
012300060810      * Operazioni iniziali
012400060810     c                   exsr      RoutInz
012500060810      *
012600060810      * Gestione video
012700060810     c                   dow       $Fine   = *off
012800060810     c                   exsr      GesS01
012900060810     c                   enddo
013000060810      * Fine
013100161028     c                   eval      KPJBU = TNTBA04DS
013200161028     c                   return
013300060810      *---------------------------------------------------------------*
013400060810      * RoutInz - Operazioni Iniziali                                 *
013500060810      *---------------------------------------------------------------*
013600060810     c     RoutInz       BEGSR
013700060810      *
013800060810      * Ricezione parametri
013900060810     c     *entry        plist
014000060810     c                   parm                    KPJBA
014100161027     c                   movel     KPJBU         TNTBA04DS
014200060810      *
014300060810      * Definizioni chiavi di accesso
014400060810     c     K05TBE01      klist                                                  *tntbe01l
014500060810     c                   kfld                    TBEcod                         -tabella
014600060810     c                   kfld                    TBEke1                         -chiave uno
014700060810     c                   kfld                    TBEke2                         -chiave due
014800060810     c                   kfld                    TBElin                         -lingua
014900060810     c                   kfld                    TBEsif                         -s.informativo
015000161027      **
015100091029      * Definizioni chiavi di accesso
015200091029     c     Keytab        klist                                                  *tntbe01l
015300091030     c                   kfld                    $tbecod                        -tabella
015400160930     c                   eval      $tbecod = 'AIG'
015500061221      *
015600061221      * Imposto eventuali parametri ricevuti
015700091030     c                   movel     *date         Wdate
015800060810      *
015900060810      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
016000060810     c     *dtaara       define    §azute        AZUTEds
016100060810     c     *dtaara       define    §datiute      dDatiUte
016200060810      *
016300060810     c                   clear                   AZUTEds
016400060810     c                   clear                   dDatiUte
016500060810     c                   clear                   TIBS34ds
016600060810     c                   in(E)     *dtaara
016700060810if  1c                   if        %Error  or  RSUT = *blanks
016800060810     c                   call      'TIBS34R'
016900060810     c                   parm                    TIBS34ds
017000060810     c                   in        *dtaara
017100060810e   1c                   endif
017200161027      *
017300060810      *-- Verifica errori e autorità profilo
017400060810sel 1c                   SELECT
017500060810      *-- controllo se ho errori nei dati utente
017600060810      *--   nel qual caso NON risulta un profilo abilitato
017700060810w   1c                   WHEN      DUTerr  = 'E'
017800060810     c                   eval      $Fine   = *on
017900060810      *
018000060810      *-- CONTROLLO AUTORITA'
018100060810      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
018200060810      *-- se il chiamante non richiede autorità specifica verificare
018300060810      *--   quella del profilo
018400060810      *-- se il chiamante richiede autorità specifica verificarla,
018500060810      *--  se è blank verificare quella del profilo
018600060810      *
018700060810      * se UTEAUT = *BLANK non siamo in sede
018800060810w   1c                   WHEN      UTEaut  = *blank
018900081008     c                   eval      $Fine   = *on
019000060810      *
019100060810x   1c                   OTHER
019200060810      *
019300060810e   1c                   ENDSL
019400060810      *
019500060810      * Aggancio dati generali della tabella in esame
019600060810     c                   clear                   TBEcod
019700060810     c                   move      *zeros        TBEke1
019800091102     c                   move      C_CodTab      TBEke1
019900060810     c                   clear                   TBEke2
020000060810     c                   clear                   TBElin
020100060810     c                   movel     KNSIF         TBEsif
020200060810     c     K05TBE01      chain     TNTBE01L
020300060810     c                   if        not %found(TNTBE01L)
020400060810     c                   clear                   TBEsif
020500060810     c     K05TBE01      chain     TNTBE01L
020600060810     c                   endif
020700060810     c                   if        %found(TNTBE01L)
020800060810     c                   movel     TNTBEds       xTNTBEds
020900060810     c                   else
021000060810     c                   clear                   xTNTBEds
021100060810     c                   endif
021200060810      *
021300060810     c                   ENDSR
021400060810
021500091028      *---------------------------------------------------------------*
021600091028      * GESS01 - Gestione subfile                                     *
021700091028      *---------------------------------------------------------------*
021800060810     c     GesS01        BEGSR
021900060810      *
022000060810      * Inizializzazione videata
022100060810if  1c                   if        $InzS01 = *on
022200060810     c                   exsr      InzS01
022300060810     c                   movel     *off          $InzS01
022400060810e   1c                   endif
022500060810      *
022600060810      * Scrivo la testata e la riga tasti funzionali abilitati
022700060810if  1c                   if        NOT *in90
022800160929     c                   write     TB03T01
022900160929     c                   write     TB03P01
023000060810e   1c                   endif
023100060810      *
023200060810      * Posiziono il cursore
023300091223     c                   eval      *in30       = (C01rcd <= *zeros and
023400091223     c                                         s01nrr = *zeros )
023500060810w   1c                   if        C01csr  > *zeros
023600060810     c                   eval      C01rcd  = C01csr
023700060810     c                   else
023800060905     c                   eval      C01rcd  = 1
023900060810     c                   endif
024000091223     c                   if        s01nrr = 0
024100161028     c                   eval      a04MSG ='Nessun record per la selezione'
024200161028     c                   eval      KPJBU = TNTBA04DS
024300161028     c                   RETURN
024400091223     c                   end
024500161028      *-----
024600060810      *
024700060810      * Emetto la videata
024800160929     c                   exfmt     TB03C01
024900060810      *
025000060810     c                   setoff                                       28  90
025100060810     c                   clear                   V1Dmsg
025200060810      *
025300060810sel 1c                   SELECT
025400060810      * F3=Fine
025500161028w   1c                   WHEN      *inKc
025600060810     c                   exsr      F03S01
025700161028      * F12=Fine
025800161028w   1c                   WHEN      *inKl
025900161028     c                   exsr      F12S01
026000161028      * F10=Inserimento
026100161028w   1c                   WHEN      *inKj
026200161028     c                   exsr      F10S01
026300060810      * Roll-Up
026400060810w   1c                   WHEN      £Tasto  = RollUp
026500060810do  2c                   do        C_SflPag
026600060810     c                   exsr      RecS01
026700060810if  3c                   if        *in33
026800060810     c                   leave
026900060810e   3c                   endif
027000060810e   2c                   enddo
027100060810     c     S01nrr        div       C_SflPag      wPage
027200060810     c                   mvr                     wRow
027300060811     c                   if        wRow    = *zeros
027400060810     c                   eval      C01csr  = (C_SflPag * (wPage - 1))
027500060810     c                                     + 1
027600060811     c                   else
027700060811     c                   eval      C01csr  = (C_SflPag * wPage)
027800060811     c                                     + 1
027900060811     c                   endif
028000060810      *
028100060810      * Controllo dati immessi a video
028200060810x   1c                   OTHER
028300060810     c                   exsr      CtrC01
028400060810      * - Rilevati Errori
028500060810if  2c                   if        *in90
028600060810     c                   leavesr
028700060810e   2c                   endif
028800060810      *
028900060810e   1c                   ENDSL
029000060810      *
029100060810     c                   ENDSR
029200060810
029300060810      *---------------------------------------------------------------*
029400060810      * INZS01 - Inizializzazione subfile (S01)                       *
029500060810      *---------------------------------------------------------------*
029600060810     c     InzS01        BEGSR
029700060810      *
029800060810      * Pulizia subfile
029900060810     c                   seton                                        3031
030000160929     c                   write     TB03C01
030100060810     c                   setoff                                         31
030200060810     c                   eval      *in33   = *off
030300060810      *
030400161031     c                   clear                   S01nrr
030500060810     c                   clear                   C01rcd
030600060810     c                   clear                   C01csr
030700060810     c                   clear                   V1Dmsg
030800060810     c                   setoff                                       28  90
030900091030      *
031000060810      * Caricamento dati già immessi (protetti)
031100091029     c
031200091028     c     keytab        setll     TNTBE000
031300091028     c                   do        *hival
031400091028     c     keytab        reade     TNTBE000
031500091028     c                   exsr      RecS01
031600091028     c   33              leave
031700091028e   1c                   ENDDO
031800091223     c*
031900091223     c                   if        s01nrr = 0
032000091223     c                   leavesr
032100091223     c                   end
032200060810      *
032300060810      * Aggiunta di righe libere a completamento della pagina
032400060810     c     S01nrr        div       C_SflPag      wPage
032500060810     c                   mvr                     wRow
032600060810do  1c                   dow       S01nrr  < (C_SflPag * (wPage + 1))
032700060810     c                             and  NOT *in33
032800060810     c                   exsr      RecS01
032900060810e   1c                   enddo
033000060810      *
033100060905      * Imposta il posizionamento cursore sul 1° rec libero del sfl
033200060811if  1c                   if        C01csr  > *zeros
033300060905     c                   eval      C01csr  = 1
033400060810e   1c                   endif
033500060901      *
033600060901      * Impostazione eventuale testo di avvertimento...
033700161028     c                   eval      vidopz = '2=Modifica'
033800060810      *
033900060810     c                   ENDSR
034000060810
034100060810      *---------------------------------------------------------------*
034200060810      * RECS01 - Caricamento singolo record del subfile S01           *
034300060810      *---------------------------------------------------------------*
034400060810     c     RecS01        BEGSR
034500161027      **
034600160929     c                   clear                   TB03S01
034700091028     c                   setoff                                       33
034800161027      **
034900091028if  1c                   if        not %eof(TNTBE01L)
035000161027      ****  filtra in base alle selezioni
035100161027     c                   movel     TBEke1        dschiave
035200161027      **
035300161028     c                   if           A04FIL <>'000' and
035400161027     c                                A04FIL <> dskFIL
035500161027     c                   LeaveSR
035600161027     c                   end
035700161027     c                   if           A04TAI <> *blank and
035800161027     c                                A04TAI <> dskasp
035900161027     c                   LeaveSR
036000161027     c                   end
036100161027     c                   if           A04ATS <> *blank and
036200161027     c                                A04ATS <> dskter
036300161027     c                   LeaveSR
036400161027     c                   end
036500161027     c                   if           A04MAS <> *blank and
036600161027     c                                A04MAS <> dskmas
036700161027     c                   LeaveSR
036800161027     c                   end
036900161027     c                   if           A04AAT <> *blank and
037000161027     c                                A04AAT <> dskaat
037100161027     c                   LeaveSR
037200161027     c                   end
037300161027      **
037400161027     c                   if           A04ALL <> *blank and
037500161027     c                                A04ALL <> dskall
037600161027     c                   LeaveSR
037700161027     c                   end
037800161027      **  controlla la DATA
037900161027     c                   if        tbeke2 <> ' '
038000161027     c                   movel     tbeke2        com08             8
038100161027     c                   move      com08         comdta            8 0
038200161027     c                   move      comdta        dtaiso
038300161027     c                   move      dtaiso        dtaeur
038400161027     c                   move      dtaeur        v1cke2
038500161027     c                   move      dtaeur        h1cke2
038600161027      *in lettura se la data scadenza è minore di udate non faccio più vedere la riga
038700161027     c                   if        dtaiso < wdate
038800161027     c                   LeaveSR
038900161027     c                   endif
039000161027     c                   end
039100161027      **    Carica campi
039200161027     c                   movel     TBEke1        h1Cke1hid
039300161027     c                   movel     dskFIL        v1Cfil
039400161027      **
039500161028     c                   if          dskfil = '000'
039600161027     c                   eval      v1cfild =  'Tutte'
039700161028      **
039800161028     c                   elseif      dskfil = '999'
039900161028     c                   eval      v1cfild =  'Azienda'
040000161028      **
040100161028     c                   else
040200161027      **
040300161027     c                   movel     v1cfil        vfil              3 0
040400161027     c     vfil          chain     azorg01l
040500161027     c                   if        %Found(azorg01l)
040600161027     c                   eval      v1cfild =  orgdes
040700161027     c                   end
040800161027     c                   endif
040900161027      **
041000161027      **  decodifica i singoli flags
041100161027      *
041200161027      **  Tipo aspettativa
041300161027     C                   CLEAR                   tibs02ds
041400161027     C                   MOVEL     KNSIF         T02SIF
041500161027     C                   MOVEL     'TAI'         T02COD
041600161027     C                   MOVEL     'C'           T02MOD
041700161027     C                   movel(p)  dskasp        T02KE1
041800161027     c                   if        dskasp <> *blank
041900161027     C                   CALL      'TIBS02R'
042000161027     C                   PARM                    KPJBA
042100161027     C                   PARM                    tibs02ds
042200161027     C     T02ERR        IFEQ      *BLANKS
042300161027     C                   MOVEL     T02UNI        dtai
042400161027     c                   movel     d§taides      v1Casp
042500161027     c                   movel     dskasp        h1Casp
042600161027     C                   ELSE
042700161027     c                   clear                   v1Casp
042800161027     c                   clear                   h1Casp
042900161027     C                   CLEAR                   Dtai
043000161027     C                   ENDIF
043100161027     c                   end
043200161027      *
043300161027      **  Territorio
043400161027     C                   CLEAR                   tibs02ds
043500161027     C                   MOVEL     KNSIF         T02SIF
043600161027     C                   MOVEL     'ATS'         T02COD
043700161027     C                   MOVEL     'C'           T02MOD
043800161027     C                   movel(p)  dskter        T02KE1
043900161027     c                   if        dskter <> *blank
044000161027     C                   CALL      'TIBS02R'
044100161027     C                   PARM                    KPJBA
044200161027     C                   PARM                    tibs02ds
044300161027     C     T02ERR        IFEQ      *BLANKS
044400161027     C                   MOVEL     T02UNI        dats
044500161027     c                   movel     d§atsdes      v1Cter
044600161027     c                   movel     dskter        h1Cter
044700161027     C                   ELSE
044800161027     c                   clear                   v1Cter
044900161027     C                   CLEAR                   h1Cter
045000161027     C                   ENDIF
045100161027     c                   end
045200161027      *
045300161027      **  Massa
045400161028     c     'MAS'         setll     tntbe02l
045500161028     c     'MAS'         reade     tntbe02l
045600161028     c                   dow       not %EoF(tntbe02l)
045700161028     c                   if        dskMas = %subst(T2_tbeKE2:1:1)
045800161028     C                   MOVEL     T2_tbeUNI     Dmas
045900161027     c                   movel     d§masdes      v1cMas
046000161027     c                   movel     dskMas        h1cMas
046100161027     c                   leave
046200161027     c                   end
046300161028     c     'MAS'         reade     tntbe02l
046400161027     c                   end
046500161027      *
046600161027      **  Tipo Attività
046700161027     C                   CLEAR                   tibs02ds
046800161027     C                   MOVEL     KNSIF         T02SIF
046900161027     C                   MOVEL     'AAT'         T02COD
047000161027     C                   MOVEL     'C'           T02MOD
047100161027     C                   movel(p)  dskaat        T02KE1
047200161027     c                   if        dskaat <> *blank
047300161027     C                   CALL      'TIBS02R'
047400161027     C                   PARM                    KPJBA
047500161027     C                   PARM                    tibs02ds
047600161027     C     T02ERR        IFEQ      *BLANKS
047700161027     C                   MOVEL     T02UNI        Daat
047800161027     c                   movel     d§aatdes      v1cAat
047900161027     c                   movel     dskaat        h1cAat
048000161027     C                   ELSE
048100161027     c                   clear                   v1cAat
048200161027     C                   CLEAR                   h1cAat
048300161027     C                   ENDIF
048400161027     c                   end
048500161027      *
048600161027      **  Tipo Allestimento
048700161027     C                   CLEAR                   tibs02ds
048800161027     C                   MOVEL     KNSIF         T02SIF
048900161027     C                   MOVEL     'ALL'         T02COD
049000161027     C                   MOVEL     'C'           T02MOD
049100161027     C                   movel(p)  dskall        T02KE1
049200161027     C                   if        dskall <> *blank
049300161027     C                   CALL      'TIBS02R'
049400161027     C                   PARM                    KPJBA
049500161027     C                   PARM                    tibs02ds
049600161027     C     T02ERR        IFEQ      *BLANKS
049700161027     C                   MOVEL     T02UNI        Dall
049800161027     c                   movel     d§alldes      v1Call
049900161027     c                   movel     dskall        h1Call
050000161027     C                   ELSE
050100161027     c                   clear                   v1Call
050200161027     C                   CLEAR                   h1Call
050300161027     C                   ENDIF
050400161027     c                   end
050500161028      **    al momento lo riempio di puntini
050600161028     c                   eval      v1Call = *all'.'
050700161028      **
050800161027      **
050900161027     c                   movel     TBEuni        dAIG
051000161027     c                   z-add     D§aigIMP      v1cIMP
051100161027     c                   z-add     D§aigQTA      v1cQTA
051200161027     c*
051300081008     c                   eval      H1Ctbe  = *on
051400161027     c*
051500091030     c                   else
051600161027     c*  EoF
051700091028     c                   seton                                        33
051800161027     C                   LEAVEsr
051900091028e   1c                   END
052000081007     c*
052100081007      * Scrittura singolo record nel subfile S01
052200161027     c                   exsr      WrtS01
052300060901      *
052400060901     c                   ENDSR
052500060901
052600060901      *---------------------------------------------------------------*
052700060901      * WRTS01 - Scrittura singolo record nel subfile S01             *
052800060901      *---------------------------------------------------------------*
052900060901     c     WrtS01        BEGSR
053000060901      *
053100081007     c                   add       1             S01nrr
053200160929     c                   write     TB03S01
053300060810      *
053400060810      * Impostazione del SflEnd
053500081007     c                   if        s01nrr = 9999
053600081007     c                   seton                                        33
053700081007     c                   end
053800060810      *
053900060810     c                   ENDSR
054000060810
054100060810      *---------------------------------------------------------------*
054200060810      * CTRC01 - Controllo di tutto il subfile                        *
054300060810      *---------------------------------------------------------------*
054400060810     c     CtrC01        BEGSR
054500060810      *
054600091030     c                   clear                   sdup
054700081008     c                   z-add     0             ok                1 0
054800081008do  1c                   DO        *hival        xx
054900160929     c     xx            chain     TB03S01                            29
055000081008     c                   if        *in29
055100081008     c                   leave
055200081008     c                   end
055300161028      *
055400060810     c                   z-add     S01nrr        C01rcd
055500161028      *
055600160929      * richiama tuti i rekord di una filiale
055700160929     c                   if        v1copz = '2'
055800161028     c                   movel(p)  v1cfil        kpjbu
055900161028     c                   call      'TNTBA04R2'
056000160929     C                   parm                    kpjba
056100161028      **  AL RITORNO PUO AVER FATTO F3:
056200161028      * se sulla destra c'è il tasto funzionale premuto F3 deve uscire dal pgm
056300161028     c                   move      kpjbu         a04FUN
056400161028     c                   if        a04fun = '03'
056500161028     c                   eval      kpjbu = TNTBA04DS
056600161028     c                   RETURN
056700161028     c                   endif
056800161028      *
056900161031     c                   eval      $InzS01 = *on
057000160929     c                   clear                   v1copz
057100160929     c                   endif
057200060810      *
057300060810     c                   exsr      CtrS01
057400060810      *
057500060810      * Aggiornamento sfl
057600060810if  2c                   if        *in28  OR  *in90
057700060810     c                   z-add     C01rcd        C01csr
057800060810e   2c                   endif
057900160929     c                   UPDATE    TB03S01
058000060810if  2c                   if        *in28  OR  *in90
058100060810     c                   leave
058200060810e   2c                   endif
058300060810      *
058400060810e   1c                   ENDDO
058500060901      *
058600060901      * Impostazione eventuale testo di avvertimento...
058700161028     c                   eval      vidopz = '2=Modifica'
058800060810      *
058900060810     c                   ENDSR
059000060810
059100060810      *---------------------------------------------------------------*
059200060810      * CTRS01 - Controllo del singolo record del subfile             *
059300060810      *---------------------------------------------------------------*
059400060810     c     CtrS01        BEGSR
059500060810      *
059600060810      *
059700060810     c                   ENDSR
059800060810
059900060810      *---------------------------------------------------------------*
060000161028      * F12S01 - Tasto funzionale F03 -> Fine programma               *
060100060810      *---------------------------------------------------------------*
060200161028     c     F12S01        BEGSR
060300060810      *
060400060810     c                   movel     *on           $Fine                          fine pgm
060500060810      *
060600060810     c                   ENDSR
060700161028      *---------------------------------------------------------------*
060800161028      * F03S01 - Tasto funzionale F03 -> Fine programma               *
060900161028      *---------------------------------------------------------------*
061000161028     c     F03S01        BEGSR
061100161028      *
061200161028     c                   movel     *on           $Fine                          fine pgm
061300161028     c                   move      '03'          a04FUN
061400161028      *
061500161028     c                   ENDSR
061600161028      *---------------------------------------------------------------*
061700161028      * F10S01 - Tasto funzionale F10 -> Inserimento nuovo record     *
061800161028      *---------------------------------------------------------------*
061900161028     c     F10S01        BEGSR
062000161028     C*
062100161028     C                   RESET                   tabds
062200161028     C                   MOVEL     '01'          xtaopz
062300161028     C                   MOVE      *ZERO         xtaret
062400161028     C                   MOVE      *ZERO         xtaopr
062500161031     C                   clear                   xtake1
062600161031     C                   clear                   xtake2
062700161028     C                   MOVE      *BLANKS       KPJBU
062800161028     C                   MOVEL     tabds         KPJBU
062900161028$004 C                   CALL      'TNTBA04R3'
063000161028     C                   PARM                    KPJBA
063100161028     C                   MOVEL     KPJBU         tabds
063200161028      *
063300161028     C* ritorno da PGM gestione
063400161028     C                   EXSR      GESRET
063500161028      *
063600161028     c                   ENDSR
063700161028     C************************************************************
063800161028     C     GESRET        BEGSR
063900161028     C*
064000161028     C* modo di ritorno
0641001610281    C                   SELECT
064200161028    >C* << questi modi di utilizzo dei valori di ritorno dal
064300161028    >C*    pgm di manutenzione rcd di anagrafica sono delle
064400161028    >C*    proposte, normalmente sempre valide, ma modificabili
064500161028    >C*    per situazioni particolari >>
064600161028     C* 1 = F3
064700161028    >C     xtaret        WHENEQ    '1'
064800161028     c                   exsr      F03S01
064900161028     C* 2 = F12
065000161028    >C     xtaret        WHENEQ    '2'
065100161028     c                   exsr      F12S01
0652001610281-   C                   ENDSL
065300161028     C*
065400161028     C* operazione eseguite dal pgm chiamato
0655001610281    C                   SELECT
065600161028     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
065700161028    >C     xtaopr        WHENEQ    '1'
065800161028     c                   eval      $InzS01 = *on
0659001610281-   C                   ENDSL
066000161028     C*
066100161028     C* funzione non eseguibile per errore :
0662001610281    C                   SELECT
066300161028     C* 1 = funzione richiamata chiusa in errore
066400161028    >C*  eventualmente gestire altri codici di errore
066500161028    >C     xtaerr        WHENEQ    '1'
066600161028     c                   eval      a04MSG ='Problemi in inserimento tabella'
066700161028     c                   eval      KPJBU = TNTBA04DS
066800161028     c                   RETURN
0669001610281-   C                   ENDSL
067000161028     C*
067100161028     C                   ENDSR
067200060810
067300060810** - $Msg -------------------------------------------------------------------*
067400091030Data scadenza errata
067500161028E'obbligatorio inserire almeno un gg
067600091030La data non può essere retroattiva o diversa da fine mese
067700091030Moltiplicatore fisso errato
067800091030Moltiplicatore fisso doppio
