000100060810      *---------------------------------------------------------------*
000200091028      * Gestione tabella "MFM" = gg mensili per moltiplicatori fissi  *
000201091028      *                          tempi uffici                         *
000300060810      *---------------------------------------------------------------*
000400060810
000500060810     h decedit('0,') datedit(*dmy/)
000600060810
000700060810      *---------------------------------------------------------------*
000800060810      *   A R C H I V I                                               *
000900060810      *---------------------------------------------------------------*
001000060810      *
001100091028     fTNTBEx1L  if   e           k disk    rename(tntbe000: tntbe£)
001101091028     f                                     prefix(£)
001102091028     fTNTBE01L  Uf A e           k disk
001200060810      *
001300091029     fTNTB77D   cf   e             workstn
001400060810     f                                     sfile(TB58S01:S01nrr)
001500060810     f                                     infds(DSFMT)
001600060810
001700060810      *---------------------------------------------------------------*
001800060810      *   C O S T A N T I                                             *
001900060810      *---------------------------------------------------------------*
002000060810      *
002100060810      * - Codice tabella in gestione
002200091028     d C_CodTab        c                   const('MFM')
002300060810      * - Nr. di righe in ogni pagina del sfl
002400060810     d C_SflPag        c                   const(18)
002500060810      * - Tasti funzionali
002800060810     d RollUp          c                   const(x'F5')
002900060811      * Attributi video
003000060811     d C_Atr_Norm      c                   const(X'A0')
003100060811     d C_Atr_BL_RI     c                   const(X'A9')
003200060810
003300060810      *---------------------------------------------------------------*
003400060810      *   S C H I E R E                                               *
003500060810      *---------------------------------------------------------------*
003600060810      *
003601091030     d wDate           s               d   datfmt(*iso)  inz(*loval)
003700060810      * - Messaggi di errore
003800091030     d $Msg            s             78    dim(  5) ctdata perrcd(1)
003801091030     c* schiera per individuare record doppi
003802091030     d sdup            s             11    dim(9999)
004200060810
004300060810      *---------------------------------------------------------------*
004400060810      *   S T R U T T U R E   D A T I                                 *
004500060810      *---------------------------------------------------------------*
004600060810      *
004700060810      * Parametri
004800060810     d KPJBA         e ds
004900060810      *
005000091028      * Tabella
005100091029     d dtut          e ds                  inz
005101091028     d dmfm          e ds                  inz
005200060810      *
005300060810      * Tracciato record file TNTBE00F
005400060810     d TNTBEds       e ds                  extname(TNTBE00F) inz
005500060810     d xTNTBEds      e ds                  extname(TNTBE00F) inz
005600060810     d                                     prefix(TBX:3)
005700060810      *
005800060810     d TIBS34ds      e ds                  inz
005900060810     d dDatiUte      e ds                  inz
006000060810     d AZUTEds       e ds                  extname(AZUTE00F) inz
006700060810      *
006800060810     d Status         sds
006900060810     d  VTCpgm           *proc
007000060810      *
007100060810     d DSFMT           ds           512
007200060810     d  £Tasto               369    369
007300060810     d  £SFLnrr              378    379B 0
007400060810      *
007401091030     d $tbecod         s                   like(tbecod)
007402091030     d $tbekey         s             11
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
009200060810      *
009300060810      * - Variabili definite a programma
009400091029     d dtaiso          s               d   datfmt(*iso)  inz(*loval)
009401091029     d dtaeur          s               d   datfmt(*eur)  inz(*loval)
009500060810     d wPage           s              4  0 inz
009600060810     d wRow            s              3  0 inz
009700061221     d wOpz            s              2    inz
009701091029     c*
009702091029     D Pvid            S               *
009703091029     D                                     INZ(%ADDR(v1c001))
009704091029     D mvid            S                   LIKE(v1c001)
009705091029     D                                     DIM(12)
009706091029     D                                     BASED(Pvid)
009707091029     D ptab            S               *
009708091029     D                                     INZ(%ADDR(D§mfm001))
009709091029     D mtab            S                   LIKE(D§mfm001)
009710091029     D                                     DIM(12)
009711091029     D                                     BASED(Ptab)
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
011401091029      *  43 - Campi mesi   in dspatr(RI) per des. mancante            *
011402091029      *  44 - Campo V1Cke2 in dspatr(RI) per prezzo medio mancante    *
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
014701091029      * Definizioni chiavi di accesso
014702091029     c     Keytab        klist                                                  *tntbe01l
014703091030     c                   kfld                    $tbecod                        -tabella
014704091029     c                   kfld                    £TBEke1                        -chiave uno
014705091029     c     £Keytab       klist                                                  *tntbe01l
014706091029     c                   kfld                    £tbecod                        -tabella
014707091029     c                   kfld                    £tbeke1                        -chiave uno
014708091029     c                   eval      $tbecod = 'MFM'
014709091029     c                   eval      £tbecod = 'TUT'
014800061221      *
014900061221      * Imposto eventuali parametri ricevuti
015000061221     c                   if        %subst(KPJBU:1:2) > *zeros
015100061221     c                   movel     KPJBU         wOpz
015200061221     c                   endif
015201091030     c                   movel     *date         Wdate
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
019400091102     c                   move      C_CodTab      TBEke1
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
021365091028      *---------------------------------------------------------------*
021366091028      * GESS01 - Gestione subfile                                     *
021367091028      *---------------------------------------------------------------*
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
022801091223     c                   eval      *in30       = (C01rcd <= *zeros and
022802091223     c                                         s01nrr = *zeros )
022900060810w   1c                   if        C01csr  > *zeros
023000060810     c                   eval      C01rcd  = C01csr
023100060810     c                   else
023200060905     c                   eval      C01rcd  = 1
023300060810     c                   endif
023301091223     c                   if        s01nrr = 0
023302091223     c                   eval      s01nrr  = 1
023303091223     c                   end
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
029902091029     c* moltiplicatori fissi
029903091028     c     'TUT'         setll     tntbex1l
029904091028     c                   do        *hival
029905091028     c     'TUT'         reade     tntbex1l
029906091028     c                   if        %eof(tntbex1l)
029907091028     c                   leave
029908091028     c                   end
029909091028     c* annullati o non moltiplicatori fissi
029910091030     c                   if        £tbeatb <> ' '
029911091028     c                   iter
029912091028     c                   end
029913091030     c                   movel     £tbeuni       dtut
029914091028     c                   if        §tutril = 'S'
029915091028     c                   iter
029916091028     c                   end
029917091030     c* non carico il moltiplicatore 900 gg lavorativi aziendali
029918091030     c* devono rimanere così
029919091030     c                   if        £tbeke1 = '900'
029920091030     c                   iter
029921091030     c                   end
030000091030      *
030100060810      * Caricamento dati già immessi (protetti)
030101091029     c
030202091028     c     keytab        setll     TNTBE000
030203091028     c                   do        *hival
030204091028     c     keytab        reade     TNTBE000
030206091028     c                   exsr      RecS01
030207091028     c   33              leave
030208091028e   1c                   ENDDO
031001091028     c*
031002091028e   1c                   ENDDO
031003091223     c*
031004091223     c                   if        s01nrr = 0
031005091223     c                   leavesr
031006091223     c                   end
031100061221      *
031200061221      * Se pgm. richiamato in interrogazione NON consento l'immissione
031300061221      *   di altri dati
031400091223if  0c                   IF        wOpz    = '05'
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
034703091028     c                   setoff                                       33
034704091028if  1c                   if        not %eof(TNTBE01L)
034705081008     c                   movel     TBEke1        v1Cke1
034707081008     c                   eval      V1Catb  = TBEatb
034709091028     c                   movel     TBEuni        dmfm
034710091028     c                   move      mtab          mvid
034711091028     c                   if        tbeke2 <> ' '
034712091029     c                   movel     tbeke2        com08             8
034713091029     c                   move      com08         comdta            8 0
034714091028     c                   move      comdta        dtaiso
034715091028     c                   move      dtaiso        dtaeur
034716091029     c                   move      dtaeur        v1cke2
034717091030     c                   move      dtaeur        h1cke2
034720091028     c                   end
034721081008     c                   eval      H1Ctbe  = *on
038100091030     c                   else
038200060810      * Record nuovo (vuoto)
038401091030     c                   movel     £tbeke1       v1cke1
038402091030     c                   clear                   h1cke2
038403091030     c                   eval      v1cke2 = 31122039
038404091028     c                   clear                   mvid
038410081008     c                   eval      H1Ctbe  = *off
038411091028     c                   seton                                        33
038600091028e   1c                   END
038605081007     c*
038611081007      * Scrittura singolo record nel subfile S01
038612081007     c                   exsr      WrtS01
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
046000060901      *
046100060901     c                   ENDSR
046200060810
046300060810      *---------------------------------------------------------------*
046400060810      * CTRC01 - Controllo di tutto il subfile                        *
046500060810      *---------------------------------------------------------------*
046600060810     c     CtrC01        BEGSR
046700060810      *
046701091030     c                   clear                   sdup
046702081008     c                   z-add     0             ok                1 0
046703081008do  1c                   DO        *hival        xx
046800081008     c     xx            chain     TB58S01                            29
047100081008     c                   if        *in29
047101081008     c                   leave
047102081008     c                   end
047400060810     c                   z-add     S01nrr        C01rcd
047500060810      *
047600060810     c                   exsr      CtrS01
047700060810      *
047800060810      * Reimpostazione indicatori per DSPATR
047900060810      * - record già immesso
048000060810     c                   eval      *in40   = (H1Ctbe = *on)
048001091029      * - moltiplicatore errato
048002091030     c                   eval      *in41   = (H1in41 = *on)
048003091030     c                   eval      *in42   = (H1in42 = *on)
048004091030      * - gg mensili
048005091030     c                   eval      *in43   = (H1in43 = *on)
048006091030      * - data errata
048007091030     c                   eval      *in44   = (H1in44 = *on)
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
051002091030     c                   setoff                                       44
051003081008     c                   eval      savin41= *off
051004081008     c                   eval      savin42= *off
051005081008     c                   eval      savin43= *off
051006081008     c                   eval      savin44= *off
051007081008     c                   eval      h1in41 = *off
051008081008     c                   eval      h1in42 = *off
051009081008     c                   eval      h1in43 = *off
051010081008     c                   eval      h1in44 = *off
051011091030     C                   XFOOT     MVID          COMODO            4 0
051012081008     c*
051026091030if  1c                   if             comodo  = *zeros
051028081008     c                   z-add     1             ok
051029081008     c                   leavesr
051030081008e   1c                   endif
051031091030      *?SEGUONO CONTROLLI DA ESEGUIRE SOLO PER I RECORDS CON DATI?
051100060810      *
052200091102if  1c                   if             V1CKE1 <> ' ' and h1ctbe = *off
052201091029     C     £KEYTAB       CHAIN     TNTBEX1L
052202091029     C                   IF        NOT %FOUND(TNTBEX1L) OR
052203091029     C                             (%FOUND(TNTBEX1L) AND £TBEATB <> ' ')
052500081008     c                   seton                                        289042
052501081008     c                   eval      H1in42  = *in42
052503081008     c                   eval      SAVin42 = *on
052600091030     c                   eval      V1Dmsg  = $Msg(4)
052700060811     c                   leavesr
052701091029     C                   ELSE
052702091030     C                   EVAL      DTUT= £TBEUNI
052703091029     c                   if        §tutril = 'S'
052704091029     c                   seton                                        289042
052705091029     c                   eval      H1in42  = *in42
052707091029     c                   eval      SAVin42 = *on
052709091029     c                   eval      V1Dmsg  = $Msg(4)
052710091029     c                   leavesr
052801091030     c                   else
052802091030     c* non si può inserire il 900 perchè sono i gg lavorativi aziendali
052803091030     c                   if        v1cke1 = '900'
052804091030     c                   seton                                        289042
052805091030     c                   eval      H1in42  = *in42
052807091030     c                   eval      SAVin42 = *on
052809091030     c                   eval      V1Dmsg  = $Msg(4)
052810091030     c                   leavesr
052811091030e   1c                   endif
052812091030e   1c                   endif
052813091030e   1c                   endif
052814091102e   1c                   endif
052815091102      * DATA
052816091102if  2c                   if        V1CKE2 =  *zeros
052817091102     c                   seton                                        284490
052818091102     c                   eval      H1in44  = *in44
052819091102     c                   eval      SAVin44 = *on
052820091102     c                   eval      V1Dmsg  = $Msg(1)
052821091102     c                   leavesr
052822091102     C                   ELSE
052823091102     C     *EUR          test(d)                 V1CKE2                 44
052824091102     c                   if        *in44
052825091102     c                   seton                                        2890
052826091102     c                   eval      H1in44  = *in44
052827091102     c                   eval      SAVin44 = *on
052828091102     c                   eval      V1Dmsg  = $Msg(1)
052829091102     c                   leavesr
052830091102     c                   else
052831091102     c* non deve essere retroattiva e deve essere fine mese
052832091102     c                   move      v1cke2        dtaeur
052833091102     c                   move      dtaeur        dtaiso
052834091102     c                   if        dtaiso < wdate and v1cke2 <> h1cke2
052835091102     c                   seton                                        289044
052836091102     c                   eval      H1in44  = *in44
052837091102     c                   eval      SAVin44 = *on
052838091102     c                   eval      V1Dmsg  = $Msg(3)
052839091102     c                   leavesr
052840091102     c                   else
052841091102     c                   extrct    dtaiso:*m     mesep             2 0
052842091102     c                   adddur    1:*d          dtaiso
052843091102     c                   extrct    dtaiso:*m     mesed             2 0
052844091102     c                   if        mesep = mesed
052845091102     c                   seton                                        289044
052846091102     c                   eval      H1in44  = *in44
052847091102     c                   eval      SAVin44 = *on
052848091102     c                   eval      V1Dmsg  = $Msg(3)
052849091102     c                   leavesr
052850091102     C                   end
052851091102     C                   end
052852091102     C                   end
052853091102     C                   end
052854091102     c* non ci devono essere righe doppie
052856091030     c                   movel(p)  v1cke1        $tbekey
052857091030     c                   move      v1cke2        $tbekey
052858091102     c     $tbekey       lookup    sdup                                   44
052859091102     c                   if        *in44
052860091102     c                   seton                                        2890
052861091102     c                   eval      H1in44  = *in44
052862091030     c                   eval      SAVin44 = *on
052863091030     c                   eval      V1Dmsg  = $Msg(5)
052864091030     c                   leavesr
052866091030e   1c                   endif
052867091102     c* carico la schiera per verificare l'inserimento di righe doppie
052868091102     c                   movel     v1cke1        sdup(s01nrr)
052869091102     c                   move      v1cke2        sdup(s01nrr)
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
060100091029if  2c                   if        NOT %found(TNTB77D)
060200060811     c                   leave
060300060811e   2c                   endif
060301091030     c                   xfoot     mvid          comodo
060400060811      * Fine dati immessi a video
060500060811if  2c                   if             H1Ctbe  = *off
060600091030     c                             and  comodo  = 0
060700091030     c                   iter
060800060811e   2c                   endif
060900060905      * Record già inserito (solo in visualizzazione)
061000091030if  2c                   if        H1Ctbe  = *on and h1cke2 = v1cke2
061100060905     c                   iter
061200060905e   2c                   endif
061300060905      *
061400060811      * Record nuovo
061500060811     c                   movel     C_CodTab      TBEcod
061600060901     c                   movel(p)  V1Cke1        TBEke1
061601091102     c                   if        H1Ctbe  = *on
061602091102     c                   move      h1cke2        dtaeur
061603091102     c                   else
061604091030     c                   move      v1cke2        dtaeur
061605091102     c                   end
061606091030     c                   move      dtaeur        dtaiso
061607091030     c                   move      dtaiso        comdta
061608091030     c                   movel(p)  comdta        tbeke2
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
063501091102     c                   exsr      RieTBE
063600091102      * aggiornamento
063700060811if  1c                   IF        %found(TNTBE01L)
064100060901     c                   eval      TBEatb  =  V1Catb
064200060811     c                   eval      TBEftt  =  TBXftt
064300060810     c                   clear                   TBEftr
064400060810     c                   UPDATE    TNTBE000
064600060811x   1c                   ELSE
064700091102      * Immissione
064900060811     c                   eval      TBEftt  =  TBXftt
065000060810     c                   clear                   TBEflt
065100060810     c                   clear                   TBEdtr
065300060811     c                   WRITE     TNTBE000
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
067501091030     c                   move      v1cke2        dtaeur
067502091030     c                   move      dtaeur        dtaiso
067503091030     c                   move      dtaiso        comdta
067504091030     c                   movel(p)  comdta        tbeke2
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
069300091029     c                   clear                   dmfm
069301091029     c                   eval      D§MFM001 = v1c001
069302091029     c                   eval      D§MFM002 = v1c002
069303091029     c                   eval      D§MFM003 = v1c003
069304091029     c                   eval      D§MFM004 = v1c004
069305091029     c                   eval      D§MFM005 = v1c005
069306091029     c                   eval      D§MFM006 = v1c006
069307091029     c                   eval      D§MFM007 = v1c007
069308091029     c                   eval      D§MFM008 = v1c008
069309091029     c                   eval      D§MFM009 = v1c009
069310091029     c                   eval      D§MFM010 = v1c010
069311091029     c                   eval      D§MFM011 = v1c011
069312091029     c                   eval      D§MFM012 = v1c012
070800060811      *
070900091029     c                   movel(p)  dmfm          TBEuni
071000060811      *
071100060811     c                   ENDSR
071200060810
071300060810** - $Msg -------------------------------------------------------------------*
071600091030Data scadenza errata
071601091030E' obbligatorio inserire almeno un gg
071900091030La data non può essere retroattiva o diversa da fine mese
072000091030Moltiplicatore fisso errato
072100091030Moltiplicatore fisso doppio
