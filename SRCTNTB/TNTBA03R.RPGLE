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
001301161107     F                                     INFDS(TBEDS)
001400161005     fTNTBEx1L  Uf A e           k disk    rename(tntbe000:tntbex) prefix(x)
001500060810      *
001600160928     fTNTBa03D  cf   e             workstn
001700060810     f                                     sfile(TB58S01:S01nrr)
001800060810     f                                     infds(DSFMT)
001900060810
001901161107     D*-------------
001902161107     D TBEds           DS
001903161107     d  TBE_rec              397    400b 0
001904161107     D*-------------
001905161107      *
001906161107     D WLBDA8          DS
001907161107     D  G02DAT                 1      8  0
001908161107     D  G02INV                 9     16  0
001909161107     D  G02ERR                17     17
001910161107     D  G02TGI                18     22  0
001911161107      *
002000060810      *---------------------------------------------------------------*
002100060810      *   C O S T A N T I                                             *
002200060810      *---------------------------------------------------------------*
002300060810      *
002400060810      * - Codice tabella in gestione
002500160928     d C_CodTab        c                   const('AGL')
002600060810      * - Nr. di righe in ogni pagina del sfl
002700060810     d C_SflPag        c                   const(18)
002800060810      * - Tasti funzionali
002900060810     d RollUp          c                   const(x'F5')
003000060811      * Attributi video
003100060811     d C_Atr_Norm      c                   const(X'A0')
003200060811     d C_Atr_BL_RI     c                   const(X'A9')
003201161107     D*-------------
003300060810
003400060810      *---------------------------------------------------------------*
003500060810      *   S C H I E R E                                               *
003600060810      *---------------------------------------------------------------*
003700060810      *
003800091030     d wDate           s               d   datfmt(*iso)  inz(*loval)
003900060810      * - Messaggi di errore
004000091030     d $Msg            s             78    dim(  5) ctdata perrcd(1)
004100091030     c* schiera per individuare record doppi
004200091030     d sdup            s             11    dim(9999)
004201161107     d sREC            s              9S 0 dim(9999)
004300060810
004400060810      *---------------------------------------------------------------*
004500060810      *   S T R U T T U R E   D A T I                                 *
004600060810      *---------------------------------------------------------------*
004700060810      *
004800060810      * Parametri
004900060810     d KPJBA         e ds
005000060810      *
005100091028      * Tabella
005200160928     d dagl          e ds                  inz
005300060810      *
005400060810      * Tracciato record file TNTBE00F
005500060810     d TNTBEds       e ds                  extname(TNTBE00F) inz
005600161005     d xTNTBEds      e ds                  extname(TNTBEx1l) inz
005700161005     d                                     prefix(x)
005800060810      *
005900060810     d TIBS34ds      e ds                  inz
006000060810     d dDatiUte      e ds                  inz
006100060810     d AZUTEds       e ds                  extname(AZUTE00F) inz
006200060810      *
006300060810     d Status         sds
006400060810     d  VTCpgm           *proc
006500060810      *
006600060810     d DSFMT           ds           512
006700060810     d  £Tasto               369    369
006800060810     d  £SFLnrr              378    379B 0
006900060810      *
007000160928     d £tbeKE1         s                   like(tbeKE1)
007100091030     d $tbecod         s                   like(tbecod)
007200091030     d $tbekey         s             11
007300060810      *---------------------------------------------------------------*
007400060810      *   V A R I A B I L I                                           *
007500060810      *---------------------------------------------------------------*
007600060810      *
007700060810      * - Flags
007800060810     d $Fine           s              1a   inz(*off)
007900060810     d $InzS01         s              1a   inz(*on)
008400060810      *
008500060810      * - Indici di schiera
008600060810     d xx              s              5  0 inz
008700060810      *
008800060810      * - Variabili riferite al data base o al display file
008900060811     d S01nrr          s                   like(C01rcd)  inz
009000060810      *
009100060810      * - Variabili definite a programma
009200091029     d dtaiso          s               d   datfmt(*iso)  inz(*loval)
009300091029     d dtaeur          s               d   datfmt(*eur)  inz(*loval)
009400060810     d wPage           s              4  0 inz
009500060810     d wRow            s              3  0 inz
009600061221     d wOpz            s              2    inz
009700091029     c*
009800091029     D Pvid            S               *
009900091029     D                                     INZ(%ADDR(v1c001))
010000091029     D mvid            S                   LIKE(v1c001)
010100091029     D                                     DIM(12)
010200091029     D                                     BASED(Pvid)
010300091029     D ptab            S               *
010400160928     D                                     INZ(%ADDR(D§agl001))
010500160928     D mtab            S                   LIKE(D§agl001)
010600091029     D                                     DIM(12)
010700091029     D                                     BASED(Ptab)
010800060810
010900060810      *---------------------------------------------------------------*
011000060810      *   M A I N   L I N E                                           *
011100060810      *---------------------------------------------------------------*
011200060810      *  Riepilogo indicatori utilizzati:                             *
011300060810      *  --------------------------------                             *
011400061221      *  06 - F6 abilitato                                            *
011500060810      *  10 - Comodo                                                  *
011600060810      *  22 - Errori in scrittura record (WRITE)                      *
011700061221      *  28 - Emissione messaggio di errore a video                   *
011800061221      *  30 - Pulizia subfile                                         *
011900061221      *  31 - NON emissione del subfile                               *
012000061221      *  33 - Fine dati nel subfile         (SFLEND)                  *
012100060810      *  40 - Record in modifica                                      *
012200161107      *  41 - Data   in modifica
012300081008      *  42 - Campo V1CKE1 in dspatr(RI) per mese errato              *
012400091029      *  43 - Campi mesi   in dspatr(RI) per des. mancante            *
012500091029      *  44 - Campo V1Cke2 in dspatr(RI) per prezzo medio mancante    *
012600060810      *  90 - Riemissione videata                                     *
012700060810      *---------------------------------------------------------------*
012701161107      * Fine
012702161107     c                   eval      *inLR   = *on
012800060810      *
012900060810      * Operazioni iniziali
013000060810     c                   exsr      RoutInz
013100060810      *
013200060810      * Gestione video
013300060810     c                   dow       $Fine   = *off
013400060810     c                   exsr      GesS01
013500060810     c                   enddo
013900060810
013901161107     c                   RETURN
014000060810      *---------------------------------------------------------------*
014100060810      * RoutInz - Operazioni Iniziali                                 *
014200060810      *---------------------------------------------------------------*
014300060810     c     RoutInz       BEGSR
014400060810      *
014500060810      * Ricezione parametri
014600060810     c     *entry        plist
014700060810     c                   parm                    KPJBA
014800160928     c                   movel     kpjbu         v1cke1            3
014900060810      *
015000060810      * Definizioni chiavi di accesso
015100060810     c     K05TBE01      klist                                                  *tntbe01l
015200060810     c                   kfld                    TBEcod                         -tabella
015300060810     c                   kfld                    TBEke1                         -chiave uno
015400060810     c                   kfld                    TBEke2                         -chiave due
015500060810     c                   kfld                    TBElin                         -lingua
015600060810     c                   kfld                    TBEsif                         -s.informativo
015601161107     c     K05TBEx1      klist                                                  *tntbe01l
015602161107     c                   kfld                    TBEcod                         -tabella
015603161107     c                   kfld                    TBEke1                         -chiave uno
015604161107     c                   kfld                    xTBEke2                        -chiave due
015605161107     c                   kfld                    TBElin                         -lingua
015606161107     c                   kfld                    TBEsif                         -s.informativo
015700091029      * Definizioni chiavi di accesso
015800091029     c     Keytab        klist                                                  *tntbe01l
015900091030     c                   kfld                    $tbecod                        -tabella
016000091029     c                   kfld                    £TBEke1                        -chiave uno
016100160928     c                   movel     v1cke1        £tbeke1
016200160928     c                   eval      $tbecod = 'AGL'
016300061221      *
016400061221      * Imposto eventuali parametri ricevuti
016500061221     c                   if        %subst(KPJBU:1:2) > *zeros
016600061221     c                   movel     KPJBU         wOpz
016700061221     c                   endif
016800091030     c                   movel     *date         Wdate
016900060810      *
017000060810      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
017100060810     c     *dtaara       define    §azute        AZUTEds
017200060810     c     *dtaara       define    §datiute      dDatiUte
017300060810      *
017400060810     c                   clear                   AZUTEds
017500060810     c                   clear                   dDatiUte
017600060810     c                   clear                   TIBS34ds
017700060810     c                   in(E)     *dtaara
017800060810if  1c                   if        %Error  or  RSUT = *blanks
017900060810     c                   call      'TIBS34R'
018000060810     c                   parm                    TIBS34ds
018100060810     c                   in        *dtaara
018200060810e   1c                   endif
018300060810      *-- Verifica errori e autorità profilo
018400060810sel 1c                   SELECT
018500060810      *-- controllo se ho errori nei dati utente
018600060810      *--   nel qual caso NON risulta un profilo abilitato
018700060810w   1c                   WHEN      DUTerr  = 'E'
018800060810     c                   eval      $Fine   = *on
018900060810      *
019000060810      *-- CONTROLLO AUTORITA'
019100060810      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
019200060810      *-- se il chiamante non richiede autorità specifica verificare
019300060810      *--   quella del profilo
019400060810      *-- se il chiamante richiede autorità specifica verificarla,
019500060810      *--  se è blank verificare quella del profilo
019600060810      *
019700060810      * se UTEAUT = *BLANK non siamo in sede
019800060810w   1c                   WHEN      UTEaut  = *blank
019900081008     c                   eval      $Fine   = *on
020000060810      *
020100060810x   1c                   OTHER
020200060810      *
020300060810e   1c                   ENDSL
020400060810      *
020500060810     c                   ENDSR
020600060810
020700091028      *---------------------------------------------------------------*
020800091028      * GESS01 - Gestione subfile                                     *
020900091028      *---------------------------------------------------------------*
021000060810     c     GesS01        BEGSR
021100060810      *
021200060810      * Inizializzazione videata
021300060810if  1c                   if        $InzS01 = *on
021400060810     c                   exsr      InzS01
021500060810     c                   movel     *off          $InzS01
021600060810e   1c                   endif
021700060810      *
021800060810      * Scrivo la testata e la riga tasti funzionali abilitati
021900060810if  1c                   if        NOT *in90
022000060810     c                   write     TB58T01
022100060810     c                   write     TB58P01
022200060810e   1c                   endif
022300060810      *
022400060810      * Posiziono il cursore
022500091223     c                   eval      *in30       = (C01rcd <= *zeros and
022600091223     c                                         s01nrr = *zeros )
022700060810w   1c                   if        C01csr  > *zeros
022800060810     c                   eval      C01rcd  = C01csr
022900060810     c                   else
023000060905     c                   eval      C01rcd  = 1
023100060810     c                   endif
023200091223     c                   if        s01nrr = 0
023300091223     c                   eval      s01nrr  = 1
023400091223     c                   end
023500060810      *
023600060810      * Emetto la videata
023700060810     c                   exfmt     TB58C01
023800060810      *
023900060810     c                   setoff                                       28  90
024000060810     c                   clear                   V1Dmsg
024100060810      *
024200060810sel 1c                   SELECT
024300161107      *
024400060810      * F3=Fine
024500161107w   1c                   WHEN      *inKc
024502161107     c                   move      '03'          kpjbu
024503161107     c                   return
024504161107      *
024506161107      * F3=Fine
024507161107w   1c                   WHEN      *inKl
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
028702161107     c                   clear                   sdup
028703161107     c                   clear                   sREC
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
030000091030      *
030100060810      * Caricamento dati già immessi (protetti)
030200091029     c
030300091028     c     keytab        setll     TNTBE000
030400091028     c                   do        *hival
030500091028     c     keytab        reade     TNTBE000
030600091028     c                   exsr      RecS01
030700091028     c   33              leave
030800091028e   1c                   ENDDO
030900091223     c*
031000091223     c                   if        s01nrr = 0
031100091223     c                   leavesr
031200091223     c                   end
031300061221      *
031400061221      * Se pgm. richiamato in interrogazione NON consento l'immissione
031500061221      *   di altri dati
031600091223if  0c                   IF        wOpz    = '05'
031700061221      *
031800061221     c                   eval      *in33   = *on
031900061221      *
032000061221x   0c                   ELSE
032100060810      *
032200060810      * Aggiunta di righe libere a completamento della pagina
032300060810     c     S01nrr        div       C_SflPag      wPage
032400060810     c                   mvr                     wRow
032500060810do  1c                   dow       S01nrr  < (C_SflPag * (wPage + 1))
032600060810     c                             and  NOT *in33
032700060810     c                   exsr      RecS01
032800060810e   1c                   enddo
032900060810      *
033000060905      * Imposta il posizionamento cursore sul 1° rec libero del sfl
033100060811if  1c                   if        C01csr  > *zeros
033200060905     c                   eval      C01csr  = 1
033300060810e   1c                   endif
033400061221      *
033500061221e   0c                   ENDIF
033600060901      *
033700060901      * Impostazione eventuale testo di avvertimento...
033800060901     c                   exsr      sr_TxtC01
033900061221      *
034000061221      * Abilitazione tasti funzionali
034100061221      * - F6 = Conferma
034200061221     c                   eval      *in06   = (wOpz <> '05')
034300060810      *
034400060810     c                   ENDSR
034500060810
034600060810      *---------------------------------------------------------------*
034700060810      * RECS01 - Caricamento singolo record del subfile S01           *
034800060810      *---------------------------------------------------------------*
034900060810     c     RecS01        BEGSR
034901161107      **
035000081007     c                   clear                   TB58S01
035100161107     c                   setoff                                       33  41
035101161107      **
035200091028if  1c                   if        not %eof(TNTBE01L)
035201161107     c                   seton                                        40
035202161107     c                   z-add     TBE_rec       tbeREC
035300081008     c                   movel     TBEke1        v1Cke1
035400081008     c                   eval      V1Catb  = TBEatb
035500160928     c                   movel     TBEuni        dagl
035600091028     c                   move      mtab          mvid
035601161107      **
035700091028     c                   if        tbeke2 <> ' '
035800161107     c                   movel     tbeke2        h1cke2
036100161107     c                   movel     h1cke2        dtaiso
036101161107     c                   move      dtaiso        dtaeur
036200091029     c                   move      dtaeur        v1cke2
036201161107      **
036202161107     c                   if         dtaiso < wdate
036203161107     c                   seton                                        41
036205161107     c                   end
036206161107     c                   eval      h1in41 = *in41
036217161107      *
036400091028     c                   end
036600091030     c                   else
036601161107     **
036700060810      * Record nuovo (vuoto)
036800091030     c                   movel     £tbeke1       v1cke1
036900091030     c                   clear                   h1cke2
037000091030     c                   eval      v1cke2 = 31122039
037100091028     c                   clear                   mvid
037101161107     c                   setoff                                       40
037300091028     c                   seton                                        33
037400091028e   1c                   END
037500161107      *
037501161107      *
037502161107     c                   eval      H1IN40  = *in40
037503161107      *
037600081007      * Scrittura singolo record nel subfile S01
037700081007     c                   exsr      WrtS01
037800060901      *
037900060901     c                   ENDSR
038000060901
038100060901      *---------------------------------------------------------------*
038200060901      * WRTS01 - Scrittura singolo record nel subfile S01             *
038300060901      *---------------------------------------------------------------*
038400060901     c     WrtS01        BEGSR
038500060901      *
038600060810      * Attributi di visualizzazione
038700060810      * - record già immesso
038900081007     c                   add       1             S01nrr
038901161107      ***   CARICA SCHIERA
038902161107     c* carico la schiera per verificare l'inserimento di righe doppie
038903161107     c   40              movel     v1cke1        sdup(s01nrr)
038904161107     c   40              move      H1cke2        sdup(s01nrr)
038905161107     c   40              Z-ADD     tbeREC        sREC(s01nrr)
038906161107      *
039000060810     c                   write     TB58S01
039100060810      *
039200060810      * Impostazione del SflEnd
039300081007     c                   if        s01nrr = 9999
039400081007     c                   seton                                        33
039500081007     c                   end
039600060810      *
039700060810     c                   ENDSR
039900060901      *---------------------------------------------------------------*
040000060901      * sr_TxtC01 - Impostazione testo di avvertimento in C01         *
040100060901      *---------------------------------------------------------------*
040200060901     c     sr_TxtC01     BEGSR
040300060901      *
040400160928     c                   movel     v1cke1        vfil              3 0
040401161108     c                   if        vfil <> 999
040500160928     c     vfil          chain     azorg01l
040600160928     c                   eval      c01txt = v1cke1 + ' ' + orgdes
040601161108     c                   else
040602161108     c                   eval      c01txt = v1cke1 + ' ' + 'Azienda'
040603161108     c                   end
040700060901      *
040800060901     c                   ENDSR
041000060810      *---------------------------------------------------------------*
041100060810      * CTRC01 - Controllo di tutto il subfile                        *
041200060810      *---------------------------------------------------------------*
041300060810     c     CtrC01        BEGSR
041701161107      *
041702161107do  1c                   DO        *hival        xx
041800081008     c     xx            chain     TB58S01                            29
042000161107     c   29              leave
042101161107      *
042200060810     c                   z-add     S01nrr        C01rcd
042400060810     c                   exsr      CtrS01
042500060810      *
042600060810      * Reimpostazione indicatori per DSPATR
042700060810      * - record già immesso
042800161107     c                   eval      *in40   =  H1in40
042801161107      *
042900091029      * - moltiplicatore errato
043400091030      * - data errata
043600060810      * Aggiornamento sfl
043700060810if  2c                   if        *in28  OR  *in90
043800060810     c                   z-add     C01rcd        C01csr
043900060810e   2c                   endif
043901161107      *
043902161107     c                   eval        *in41 = h1in41
044000060810     c                   UPDATE    TB58S01
044001161107      *
044100060810if  2c                   if        *in28  OR  *in90
044200060810     c                   leave
044300060810e   2c                   endif
044400060810      *
044500060810e   1c                   ENDDO
044600060901      *
044700060901      * Impostazione eventuale testo di avvertimento...
044800060901     c                   exsr      sr_TxtC01
044900060810      *
045000060810     c                   ENDSR
045100060810
045200060810      *---------------------------------------------------------------*
045300060810      * CTRS01 - Controllo del singolo record del subfile             *
045400060810      *---------------------------------------------------------------*
045500060810     c     CtrS01        BEGSR
045600060810      *
045700161107     c                   setoff                                       424344
046601161107     c*
046700091030     C                   XFOOT     MVID          COMODO            4 0
046800081008     c*
046900091030if  1c                   if             comodo  = *zeros
047100081008     c                   leavesr
047200081008e   1c                   endif
047201161107     c*
047300091030      *?SEGUONO CONTROLLI DA ESEGUIRE SOLO PER I RECORDS CON DATI?
047600161107      *
047601161107      * DATA
047700091102if  2c                   if        V1CKE2 =  *zeros
047800091102     c                   seton                                        284490
048100091102     c                   eval      V1Dmsg  = $Msg(1)
048200091102     c                   leavesr
048300160928    2C                   ELSE
048301161107     C*
048302161107     C                   MOVEl     V1CKE2        G02DAT
048303161107     C                   MOVEL     *BLANK        G02ERR
048304161107     C                   CALL      'XSRDA8'
048305161107     C                   PARM                    WLBDA8
048306161107     C*
048307161107     C     G02ERR        IFEQ      '1'
048310161107     c                   seton                                        284490
048313161107     c                   eval      V1Dmsg  = $Msg(1)
048314161107     c                   leavesr
048315161107     C                   ElsE
048316161107     c                   move      g02dat        V1CKE2
048317161107     c                   move      g02inv        V1CKE2amg         8 0
048318161107     C                   END
049101161107      ***
049200091102     c* non deve essere retroattiva e deve essere fine mese
049400161107     c                   move      V1CKE2amg     dtaiso
049403161107      ***
049500161107    4c                   if        dtaiso < wdate and v1cke2amg <> h1cke2
049600091102     c                   seton                                        289044
049900091102     c                   eval      V1Dmsg  = $Msg(3)
050000091102     c                   leavesr
050001161107      ***
050100160928    4c                   else
050101161107      ***
050200091102     c                   extrct    dtaiso:*m     mesep             2 0
050300091102     c                   adddur    1:*d          dtaiso
050400091102     c                   extrct    dtaiso:*m     mesed             2 0
050401161107      ***
050500160928    5c                   if        mesep = mesed
050600091102     c                   seton                                        289044
050900091102     c                   eval      V1Dmsg  = $Msg(3)
051000091102     c                   leavesr
051100160928    5C                   end
051101161107      ***
051200160928    4C                   end
051402161107      ***
051500091102     c* non ci devono essere righe doppie
051600161107     c                   movel     v1cke1        $tbekey
051601161107     c                   move      v1cke2amg     $tbekey
051701161107      ***
051702161107     C                   Z-ADD     1             ZZ                5 0
051703161107     c     $tbekey       lookup    sdup(ZZ)                               21
051800161107     c                   if        %Found
051801161107     c                   if        srec(zz) <> tbeREC
051900161107     c                   seton                                        289044
052200091030     c                   eval      V1Dmsg  = $Msg(5)
052300091030     c                   leavesr
052400161107     C                   end
052401161107     C                   endif
052500091030e   1c                   endif
052900060810      *
053000060810     c                   ENDSR
053100060810
053200060810      *---------------------------------------------------------------*
053300060810      * F03S01 - Tasto funzionale F03 -> Fine programma               *
053400060810      *---------------------------------------------------------------*
053500060810     c     F03S01        BEGSR
053600060810      *
053700060810     c                   movel     *on           $Fine                          fine pgm
053701161107     c                   move      '12'          kpjbu                          fine pgm
053800060810      *
053900060810     c                   ENDSR
054100060810      *---------------------------------------------------------------*
054200060810      * F06S01 * Elaborazione subfile per aggiornamento tabella       *
054300060810      *---------------------------------------------------------------*
054400060810     c     F06S01        BEGSR
054500060810      *
054600060811     c                   clear                   S01nrr
054700060811      *
054800060811do  1c                   DO        *hival        S01nrr
054900060811      *
055000060811     c     S01nrr        chain     TB58S01
055100060810      *
055200060811      * Fine ciclo di lettura sfl
055300160928if  2c                   if        NOT %found(TNTBa03D)
055400060811     c                   leave
055500060811e   2c                   endif
055501161107     c**
055600091030     c                   xfoot     mvid          comodo
055601161107     c**
055700060811      * Fine dati immessi a video
055800161107if  2c                   if             H1in40  = *off
055900091030     c                             and  comodo  = 0
056000091030     c                   iter
056100060811e   2c                   endif
056101161107      ****
056200060905      * Record già inserito (solo in visualizzazione)
056300161107if  2c                   if        H1in40  = *on and h1cke2 = v1cke2amg
056400060905     c                   iter
056500060905e   2c                   endif
056600060905      *
056700060811      * Record nuovo
056800060811     c                   movel     C_CodTab      TBEcod
056900161107     c                   movel(p)  V1Cke1        TBEke1
057000161107     c                   if        H1in40  = *on
057100161107     c                   movel(p)  h1cke2        tbeke2
057200091102     c                   else
057300161107     c                   movel(p)  v1cke2amg     tbeke2
057400091102     c                   end
057401161107     c                   movel(p)  tbeke2        Xtbeke2
057800060810     c                   clear                   TBElin
057900060810     c                   clear                   TBEsif
058000060810     c     K05TBE01      chain     TNTBE000
058100060810      *
058200060810     c                   exsr      AggTBE
058300060810      *
058400060810e   1c                   ENDDO
058500060810      *
058600060810     c                   ENDSR
058700060810
058800060810      *---------------------------------------------------------------*
058900060810      * AGGTBE * Aggiornamento tabella (singolo record)               *
059000060810      *---------------------------------------------------------------*
059100060810     c     AggTBE        BEGSR
059200091102     c                   exsr      RieTBE
059300091102      * aggiornamento
059400060811if  1c                   IF        %found(TNTBE01L)
059500060901     c                   eval      TBEatb  =  V1Catb
059600161005     c                   clear                   TBEftt
059700060810     c                   clear                   TBEftr
059800060810     c                   UPDATE    TNTBE000
059900060811x   1c                   ELSE
060000091102      * Immissione
060100161005     c                   clear                   TBEftt
060200060810     c                   clear                   TBEflt
060300060810     c                   clear                   TBEdtr
060400060811     c                   WRITE     TNTBE000
060500060811e   1c                   ENDIF
060600161005     c                   exsr      AggTbeFil
060700060810     c
060800060810     c                   ENDSR
060900060810
061000060810      *---------------------------------------------------------------*
061100060810      * RIETBE * Riempimento dati tabella per IMMISSIONE              *
061200060810      *---------------------------------------------------------------*
061300060810     c     RieTBE        BEGSR
061400060810      *
061500060810     c                   clear                   TNTBE000
061600060810      *
061700060810     c                   clear                   TBEsif
061800060810     c                   clear                   TBElin
061900161005     c                   movel     'BI'          TBEapl
062000060810     c                   movel     C_CodTab      TBEcod
062100161107     c                   movel(p)  V1Cke1        TBEke1
062201161107     c                   movel(p)  v1cke2amg     tbeke2
062600060810      *
062700060811     c                   exsr      RieDS
062800060810      *
062900060810     c                   ENDSR
063000060811
063100161005      *---------------------------------------------------------------*
063200161005      * AggTbeFil * Riempimento dati tabella in filiale
063300161005      *---------------------------------------------------------------*
063400161005     c     AggTbeFil     BEGSR
063500161005      *
063600161107     c     K05TBEx1      chain     tntbex1l
063700161005      *
063800161005     c                   movel     TNTBEds       xTNTBEds
063900161005     c                   if        %found(tntbex1l)
064000161005     c                   update    tntbex
064100161005     c                   else
064200161005     c                   write     tntbex
064300161005     c                   endif
064400161005     c                   ENDSR
064500161005
064600060811      *---------------------------------------------------------------*
064700060811      * RIEDS  * Riempimento struttura dati                           *
064800060811      *---------------------------------------------------------------*
064900060811     c     RieDS         BEGSR
065000060811      *
065100160928     c                   clear                   dagl
065200160928     c                   eval      D§agl001 = v1c001
065300160928     c                   eval      D§agl002 = v1c002
065400160928     c                   eval      D§agl003 = v1c003
065500160928     c                   eval      D§agl004 = v1c004
065600160928     c                   eval      D§agl005 = v1c005
065700160928     c                   eval      D§agl006 = v1c006
065800160928     c                   eval      D§agl007 = v1c007
065900160928     c                   eval      D§agl008 = v1c008
066000160928     c                   eval      D§agl009 = v1c009
066100160928     c                   eval      D§agl010 = v1c010
066200160928     c                   eval      D§agl011 = v1c011
066300160928     c                   eval      D§agl012 = v1c012
066400060811      *
066500160928     c                   movel(p)  dagl          TBEuni
066600060811      *
066700060811     c                   ENDSR
066800060810
066900060810** - $Msg -------------------------------------------------------------------*
067000091030Data scadenza errata
067100091030E' obbligatorio inserire almeno un gg
067200091030La data non può essere retroattiva o diversa da fine mese
067300091030Moltiplicatore fisso errato
067400091030Moltiplicatore fisso doppio
