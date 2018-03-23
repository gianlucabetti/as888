000100041221      *---------------------------------------------------------------*
000200041221      * Gestione tabella "CXB" = Clienti "extra budget"               *
000300041221      *---------------------------------------------------------------*
000400041221
000500041221     h decedit('0,') datedit(*dmy/)
000600041221
000700041221      *---------------------------------------------------------------*
000800041221      *   A R C H I V I                                               *
000900041221      *---------------------------------------------------------------*
001000041221      *
001100041221     fTNTBE01L  uf a e           k disk
001200041221      *
001300041221     fTNTB36D   cf   e             workstn
001400041221
001500041221      *---------------------------------------------------------------*
001600041221      *   C O S T A N T I                                             *
001700041221      *---------------------------------------------------------------*
001800041221      *
001900041221     d DigitN          c                   const('0123456789')
002000041221
002100041221      *---------------------------------------------------------------*
002200041221      *   S C H I E R E                                               *
002300041221      *---------------------------------------------------------------*
002400041221      *
002500041221     d $Opz            s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
002600041221     d $Msg            s             78    dim(03) ctdata perrcd(1)             Messaggi video
002700041221
002800041221      *---------------------------------------------------------------*
002900041221      *   S T R U T T U R E   D A T I                                 *
003000041221      *---------------------------------------------------------------*
003100041221      *
003200041221      * Parametri
003300041221     d KPJBA         e ds
003400041221      *
003500041221      * Passaggio Parametri al pgm TIBS02R
003600041221     d TIBS02DS      e ds                  inz
003700041221     d  T02mod       e                     inz('R')
003800041221     d  T02cod       e                     inz('CXB')
003900041221      *
004000041221      * Tabella CXB = Clienti "extra budget"
004100041221     d dCXB          e ds                  inz
004200041221      *
004300041221      * Tracciato record file TNTBE00F
004400041221     d TNTBEds       e ds                  extname(TNTBE00F) inz
004500041221     d xTNTBEds      e ds                  extname(TNTBE00F) inz
004600041221     d                                     prefix(TBX:3)
004700041221      *
004800041221     d TIBS34DS      e ds                  inz
004900041221     d DDATIUTE      e ds                  inz
005000041221     d AZUTEDS       e ds                  extname(AZUTE00F) inz
005100041221      *
005200041221      * Decodifica anagrafica clienti
005300041221     d TIBS69ds      e ds                  inz
005400041221     d ds_CNACO      e ds                  extname(CNACO00F) inz
005500041221     d ds_CNIND      e ds                  extname(CNIND00F) inz
005600041221     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
005700041221     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
005800041221      *
005900041221     d WLBdat          ds                  inz
006000041221     d  G02dat                 1      8  0 inz
006100041221     d  G02inv                 9     16  0 inz
006200041221     d  G02err                17     17    inz('3')
006300041221     d  G02tgi                18     22  0 inz
006400041221      *
006500041221     d                sds
006600041221     d  VTCpgm           *proc
006700041221
006800041221      *---------------------------------------------------------------*
006900041221      *   V A R I A B I L I                                           *
007000041221      *---------------------------------------------------------------*
007100041221      *
007200041221     d $Fine           s              1a   inz(*off)
007300041221     d $CarV1          s              1a   inz(*on)
007400041221     d $CarV2          s              1a   inz(*off)
007500041221     d $TipVid         s              1a   inz('1')
007600041221     d $Tibs02         s              1a   inz(*off)
007700041221     d*** Win             s             99a   inz(*zeros)
007800041221     d wTasto          s              2a   inz(*zeros)
007900041221     d wDate           s               d   datfmt(*iso) inz(*loval)
008000041221
008100041221      *---------------------------------------------------------------*
008200041221      *   M A I N   L I N E                                           *
008300041221      *---------------------------------------------------------------*
008400041221      *  Riepilogo indicatori utilizzati:                             *
008500041221      *  --------------------------------                             *
008600041221      *  01 - Record inesistente (inserimento)                        *
008700041221      *  02 - Record esistente   (modifica)                           *
008800041221      *  04 - Record annullato   (ripristino)                         *
008900041221      *  20 - Comodo                                                  *
009000041221      *  22 - Errori in scrittura record (WRITE)                      *
009100041221      *  43 - Visualizzazione "Data creazione"                        *
009200041221      *  50 - Posizionamento cursore su "Codice cliente"              *
009300041221      *  51 - Posizionamento cursore su "Note"                        *
009400041221      *  99 - Visualizzazione messaggio di errore                     *
009500041221      *---------------------------------------------------------------*
009600041221      *
009700041221      * Operazioni iniziali
009800041221     c                   exsr      RutInz
009900041221      *
010000041221      * Gestione video
010100041221     c                   dow       $Fine = *off
010200041221     c     $TipVid       caseq     '1'           GesV1
010300041221     c     $TipVid       caseq     '2'           GesV2
010400041221     c                   endcs
010500041221     c                   enddo
010600041221      *
010700041221      * Fine
010800041221     c                   if        $Tibs02 = *on
010900041221     c                   clear                   TIBS02ds
011000041221     c                   movel     'C'           T02tla
011100041221     c                   call      'TIBS02R'
011200041221     c                   parm                    KPJBA
011300041221     c                   parm                    TIBS02ds
011400041221     c                   endif
011500041221      *
011600041221     c                   movel     *on           *inLR
011700041221
011800041221      *---------------------------------------------------------------*
011900041221      * RutInz - Operazioni Iniziali                                  *
012000041221      *---------------------------------------------------------------*
012100041221     c     RutInz        BEGSR
012200041221      *
012300041221      * Ricezione parametri
012400041221     c     *entry        plist
012500041221     c                   parm                    KPJBA
012600041221      *
012700041221      * Definizioni chiavi di accesso
012800041221     c     K05TBE01      klist                                                  *tntbe01l
012900041221     c                   kfld                    TBEcod                         -tabella
013000041221     c                   kfld                    TBEke1                         -chiave uno
013100041221     c                   kfld                    TBEke2                         -chiave due
013200041221     c                   kfld                    TBElin                         -lingua
013300041221     c                   kfld                    TBEsif                         -s.informativo
013400041221      *
013500041221      * Reperisco la data odierna
013600041221     c                   movel     *date         Wdate
013700041221      *
013800041221      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
013900041221     c     *dtaara       define    §azute        azuteds
014000041221     c     *dtaara       define    §datiute      ddatiute
014100041221      *
014200041221     c                   clear                   AzUteDs
014300041221     c                   clear                   DDatiUte
014400041221     c                   clear                   Tibs34Ds
014500041221     c                   in(E)     *dtaara
014600041221if  1c                   if        %Error  or  RSUT = *blanks
014700041221     c                   call      'TIBS34R'
014800041221     c                   parm                    Tibs34Ds
014900041221     c                   in        *dtaara
015000041221e   1c                   endif
015100041221      *-- Verifica errori e autorità profilo
015200041221sel 1c                   SELECT
015300041221      *-- controllo se ho errori nei dati utente
015400041221      *--   nel qual caso NON risulta un profilo abilitato
015500041221w   1c                   WHEN      DUTerr = 'E'
015600041221     c                   eval      $Fine  = *on
015700041221      *
015800041221      *-- CONTROLLO AUTORITA'
015900041221      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
016000041221      *-- se il chiamante non richiede autorità specifica verificare
016100041221      *--   quella del profilo
016200041221      *-- se il chiamante richiede autorità specifica verificarla,
016300041221      *--  se è blank verificare quella del profilo
016400041221      *
016500041221      * se UTEAUT = *BLANK non siamo in sede
016600041221w   1c                   WHEN      UTEaut = *blank
016700041221      *
016800041221x   1c                   OTHER
016900041221      *
017000041221e   1c                   ENDSL
017100041221      *
017200041221      * Aggancio dati generali della tabella in esame
017300041221     c                   clear                   TBEcod
017400041221     c                   move      *zeros        TBEke1
017500041221     c                   move      T02cod        TBEke1
017600041221     c                   clear                   TBEke2
017700041221     c                   clear                   TBElin
017800041221     c                   movel     KNSIF         TBEsif
017900041221     c     K05TBE01      chain     TNTBE01L
018000041221     c                   if        not %found(TNTBE01L)
018100041221     c                   clear                   TBEsif
018200041221     c     K05TBE01      chain     TNTBE01L
018300041221     c                   endif
018400041221     c                   if        %found(TNTBE01L)
018500041221     c                   movel     TNTBEds       xTNTBEds
018600041221     c                   else
018700041221     c                   clear                   xTNTBEds
018800041221     c                   endif
018900041221      *
019000041221     c                   clear                   TB36V1
019100041221      *
019200041221     c                   ENDSR
019300041221
019400041221      *---------------------------------------------------------------*
019500041221      * GESV1  - Gestione videata selezione codice tabella            *
019600041221      *---------------------------------------------------------------*
019700041221     c     GesV1         BEGSR
019800041221      *
019900041221      * Inizializzazione videata
020000041221if  1c                   if        $CarV1 = *on
020100041221     c                   exsr      CarV1
020200041221     c                   movel     *off          $CarV1
020300041221e   1c                   endif
020400041221      *
020500041221      * Scrivo la testata
020600041221     c                   clear                   T1opz
020700041221     c                   write     TB36T1
020800041221      *
020900041221      * Se esistono errori sulla videata
021000041221      * emetto la write del formato a indicatori spenti per vedere
021100041221      * le eventuali decodifiche
021200041221      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
021300041221if  1c***                if        *in99
021400041221     c***                movea     *in           Win
021500041221     c***                movea     *zeros        *in(50)
021600041221     c***                write     TB36V1
021700041221     c***                movea     Win           *in
021800041221e   1c***                endif
021900041221      *
022000041221     c                   exfmt     TB36V1
022100041221     c                   eval      *in99 = *off
022200041221     c                   clear                   V1Dmsg
022300041221      * F3=Fine
022400041221if  1c                   if        *inKC
022500041221     c                   exsr      F03V1
022600041221     c                   goto      EndGesV1
022700041221e   1c                   endif
022800041221      *
022900041221      * Controllo dati immessi a video
023000041221     c                   exsr      CtrV1
023100041221     c     *in99         cabeq     *on           EndGesV1
023200041221      *
023300041221      * Passaggio alla videata di dettaglio
023400041221if  1c                   if        not *in99
023500041221     c                   eval      $CarV2 = *on
023600041221     c                   eval      $TipVid = '2'
023700041221e   1c                   endif
023800041221      *
023900041221     c     EndGesV1      ENDSR
024000041221
024100041221      *---------------------------------------------------------------*
024200041221      * CARV1  - Caricamento dati prima videata                       *
024300041221      *---------------------------------------------------------------*
024400041221     c     CarV1         BEGSR
024500041221      *
024600041221     c                   movea     *zeros        *in(40)
024700041221     c                   movea     '00000'       *in(01)
024800041221      *
024900041221      * Imposto il campo "codice cliente" per l'interrogazione
025000041221     c                   movel(p)  '?'           V1Cksc
025100041221      *
025200041221     c                   ENDSR
025300041221
025400041221      *---------------------------------------------------------------*
025500041221      * CTRV1  - Controllo e decodifica prima videata                 *
025600041221      *---------------------------------------------------------------*
025700041221     c     CtrV1         BEGSR
025800041221      *
025900041221     c                   movea     *zeros        *in(50)
026000041221     c                   clear                   V1Dmsg
026100041221      *
026200041221      * Codice cliente
026300041221      * - Ricerca:
026400041221     c     '?'           scan      V1Cksc                                 20
026500041221if  1c                   if        *in20
026600041221     c                   eval      $Tibs02 = *on
026700041221     c                   clear                   V1Cksc
026800041221     c                   eval      *in50   = *on
026900041221     c                   reset                   TIBS02ds
027000041221     c                   movel     KNSIF         T02sif
027100041221     c                   call      'TIBS02R'
027200041221     c                   parm                    KPJBA
027300041221     c                   parm                    TIBS02ds
027400041221if  2c                   if        T02err  = *blanks
027500041221     c                   movel     T02uni        dCXB
027600041221     c                   movel     T02ke1        V1Cksc
027700041221x   2c                   else
027800041221     c                   eval      *in99   = *on
027900041221     c                   movel     T02msg        V1Dmsg
028000041221     c                   endif
028100041221e   1c                   endif
028200041221      * - Controllo:
028300041221      * - - effettiva immissione
028400041221if  1c                   if           V1Cksc  = *blanks
028500041221     c                             or V1Cksc = *zeros
028600041221     c                   seton                                        50  99
028700041221     c                   eval      V1Dmsg  = $Msg(01)
028800041221     c                   goto      EndCtrV1
028900041221x   1c                   else
029000041221      * - - numericità
029100041221     c     DigitN        check     V1Cksc                                 10
029200041221if  2c                   if        *in10  =  *on
029300041221     c                   seton                                        50  99
029400041221     c                   eval      V1Dmsg  = $Msg(02)
029500041221     c                   goto      EndCtrV1
029600041221e   2c                   endif
029700041221      * - - decodifica
029800041221     c                   exsr      DecodCLI
029900041221e   1c                   endif
030000041221      *
030100041221     c     EndCtrV1      ENDSR
030200041221
030300041221      *---------------------------------------------------------------*
030400041221      * DecodCLI   * Decodifica codice cliente                        *
030500041221      *---------------------------------------------------------------*
030600041221     c     DecodCLI      BEGSR
030700041221      *
030800041221     c                   clear                   TIBS69ds
030900041221     c                   clear                   ds_CNACO
031000041221     c                   clear                   ds_CNIND
031100041221     c                   clear                   ds_CNCLP
031200041221     c                   clear                   ds_FNCLS
031300041221     c                   move      V1Cksc        I69kac
031400041221     c                   call      'TIBS69R'
031500041221     c                   parm                    TIBS69ds
031600041221     c                   parm                    ds_CNACO
031700041221     c                   parm                    ds_CNIND
031800041221     c                   parm                    ds_CNCLP
031900041221     c                   parm                    ds_FNCLS
032000041221      *
032100041221     c                   if        O69err = *blanks
032200041221     c                   movel     ACOrag        V2Dksc
032300041221     c                   else
032400041221     c                   seton                                        50  99
032500041221     c                   eval      V1Dmsg  = $Msg(02)
032600041221     C                   endif
032700041221      *
032800041221     c                   ENDSR
032900041221
033000041221      *---------------------------------------------------------------*
033100041221      * F03V1  - Tasto funzionale F03 -> Fine programma               *
033200041221      *---------------------------------------------------------------*
033300041221     c     F03V1         BEGSR
033400041221      *
033500041221     c                   movel     *on           $Fine                          fine pgm
033600041221      *
033700041221     c                   ENDSR
033800041221
033900041221      *---------------------------------------------------------------*
034000041221      * GESV2  - Gestione videata dettaglio dati                      *
034100041221      *---------------------------------------------------------------*
034200041221     c     GesV2         BEGSR
034300041221      *
034400041221      * Inizializzazione videata
034500041221     c                   if        $CarV2 = *on
034600041221     c                   exsr      CarV2
034700041221     c                   move      *off          $CarV2
034800041221     c                   endif
034900041221      * Scrivo la testata
035000041221     c                   write     TB36T1
035100041221      *
035200041221      * Se esistono errori sulla videata
035300041221      * emetto la write del formato a indicatori spenti per vedere
035400041221      * le eventuali decodifiche
035500041221      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
035600041221if  1c***                if        *in99
035700041221     c***                movea     *in           Win
035800041221     c***                movea     *zeros        *in(50)
035900041221     c***                write     TB36V2
036000041221     c***                movea     Win           *in
036100041221e   1c***                endif
036200041221      *
036300041221if  1c                   if        *in05
036400041221     c                   write     TB36V2
036500041221     c                   exfmt     PROTECT
036600041221x   1c                   else
036700041221     c                   exfmt     TB36V2
036800041221e   1c                   endif
036900041221     c                   eval      *in99 = *off
037000041221     c                   clear                   V1Dmsg
037100041221     c                   clear                   wTasto
037200041221      *
037300041221sel 1c                   select
037400041221      * F03=Fine
037500041221w   1c                   when      *inKC
037600041221     c                   exsr      F03V1
037700041221     c                   goto      EndGesV2
037800041221      * F12=Ritorno
037900041221w   1c                   when      *inKL
038000041221     c                   exsr      F12V2
038100041221     c                   goto      EndGesV2
038200041221e   1c                   endsl
038300041221      *
038400041221      * Controllo dati immessi a video
038500041221      * (non si fanno se richisto l'annullamento)
038600041221if  1c                   if        not *inKQ
038700041221     c                   exsr      CtrV2
038800041221e   1c                   endif
038900041221      *
039000041221      * Aggiornamento se non ci sono errori
039100041221if  1c                   if        not *in99
039200041221     c                             and (*inKF or *inKE or *inKQ)
039300041221sel 2c                   select
039400041221w   2c                   when      *inKE
039500041221     c                   eval      wTasto = '05'
039600041221w   2c                   when      *inKF
039700041221     c                   eval      wTasto = '06'
039800041221w   2c                   when      *inKQ
039900041221     c                   eval      wTasto = '16'
040000041221e   2c                   endsl
040100041221     c                   exsr      AggTBE
040200041221e   1c                   endif
040300041221      *
040400041221     c     EndGesV2      ENDSR
040500041221
040600041221      *---------------------------------------------------------------*
040700041221      * CARV2  - Caricamento dati seconda videata                     *
040800041221      *---------------------------------------------------------------*
040900041221     c     CarV2         BEGSR
041000041221      *
041100041221     c                   clear                   T1opz
041200041221     c                   movea     '00000'       *in(01)
041300041221     c                   eval      *in43 = *off
041400041221      *
041500041221      * reimposto il campo chiave (qui di solo output)
041600041221     c                   movel     V1Cksc        V2Cksc
041700041221     c                   movel     ACOrag        V2Dksc
041800041221      *
041900041221      * ripulisco i campi in manutenzione in questa videata
042000041221      * (e relative decodifiche)
042100041221     c                   clear                   V2Cnot
042200041221     c                   clear                   V2Cdac
042300041221     c                   clear                   V2Hdac
042400041221      *
042500041221      * Aggancio la tabella, se trovo il codice sono in modifica
042600041221      * o ripristino (se record annullato), altrimenti in immissione
042700041221     c                   exsr      ChnTBE
042800041221      *
042900041221if  1c                   if        %found(TNTBE01L)
043000041221      *
043100041221      * MODIFICA/RIPRISTINO
043200041221     c                   eval      *in43  = *on
043300041221     c                   movel     TBEuni        dCXB
043400041221if  2c                   if        TBEatb = *blanks
043500041221     c                   eval      *in02  = *on
043600041221     c                   eval      T1opz  = $Opz(02)
043700041221x   2c                   else
043800041221     c                   eval      *in04  = *on
043900041221     c                   eval      T1opz  = $Opz(06)
044000041221e   2c                   endif
044100041221      *
044200041221x   1c                   else
044300041221      *
044400041221      * IMMISSIONE
044500041221     c*** già così:      eval      *in43  = *off
044600041221     c                   clear                   dCXB
044700041221     c                   eval      *in01  = *on
044800041221     c                   eval      T1opz  = $Opz(01)
044900041221      *
045000041221e   1c                   endif
045100041221      *
045200041221      *
045300041221     c                   eval      V2Cnot = §CXBnot
045400041221if  1c                   if        %found(TNTBE01L)
045500041221     c                   reset                   WLBdat
045600041221     c                   z-add     §CXBdac       G02inv
045700041221     c                   call      'XSRDA8'
045800041221     c                   parm                    WLBdat
045900041221     c                   z-add     G02dat        V2Cdac
046000041221     c                   eval      V2Hdac = §CXBdac
046100041221x   1c                   else
046200041221     c                   movel     *date         V2Cdac
046300041221     c                   movel     Wdate         V2Hdac
046400041221e   1c                   endif
046500041221      *
046600041221     c                   ENDSR
046700041221
046800041221      *---------------------------------------------------------------*
046900041221      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
047000041221      *---------------------------------------------------------------*
047100041221     c     F12V2         BEGSR
047200041221      *
047300041221     c                   eval      $TipVid = '1'
047400041221      *
047500041221     c                   ENDSR
047600041221
047700041221      *---------------------------------------------------------------*
047800041221      * CTRV2  - Controllo e decodifica seconda videata               *
047900041221      *---------------------------------------------------------------*
048000041221     c     CtrV2         BEGSR
048100041221      *
048200041221     c                   movea     *zeros        *in(50)
048300041221     c                   clear                   V1Dmsg
048400041221      *
048500041221      * Note obbligatorie
048600041221if  1c                   if        V2Cnot = *blanks
048700041221     c                   seton                                        51  99
048800041221     c                   movel     $Msg(03)      V1Dmsg
048900041221     c                   goto      EndCtrV2
049000041221e   1c                   endif
049100041221      *
049200041221     c     EndCtrV2      ENDSR
049300041221
049400041221      *---------------------------------------------------------------*
049500041221      * CHNTBE * Aggancio tabella                                     *
049600041221      *---------------------------------------------------------------*
049700041221     c     ChnTBE        BEGSR
049800041221      *
049900041221     c                   movel     T02cod        TBEcod
050000041221     c                   movel(p)  V2Cksc        TBEke1
050100041221     c                   clear                   TBEke2
050200041221     c                   clear                   TBElin
050300041221     c                   movel     KNSIF         TBEsif
050400041221     c     K05TBE01      chain     TNTBE01L
050500041221      * Se non ho trovato il record con il sistema informativo
050600041221      * che ho in linea, lo abblenco
050700041221if  1c                   if        not %found(TNTBE01L)
050800041221     c                   clear                   TBEsif
050900041221     c     K05TBE01      chain     TNTBE01L
051000041221e   1c                   endif
051100041221      *
051200041221     c                   ENDSR
051300041221
051400041221      *---------------------------------------------------------------*
051500041221      * AGGTBE * Aggiornamento tabella                                *
051600041221      *---------------------------------------------------------------*
051700041221     c     AggTBE        BEGSR
051800041221      *
051900041221sel 1c                   SELECT
052000041221      *
052100041221      * F5=Ripristino
052200041221w   1c                   WHEN      wTasto='05'  and  *in04
052300041221     c                   clear                   TBEatb
052400041221     c                   clear                   TBEftr
052500041221     c                   UPDATE    TNTBE000
052600041221      *
052700041221      * F6=Conferma
052800041221w   1c                   WHEN      wTasto='06'
052900041221     c                   exsr      RieTBE
053000041221sel 2c                   select
053100041221      *   Immissione
053200041221w   2c                   when      *in01
053300041221     c                   clear                   TBEflt
053400041221     c                   clear                   TBEdtr
053500041221     c                   WRITE     TNTBE000                             22
053600041221      *   Modifica / Ripristino
053700041221w   2c                   when      *in02   or    *in04
053800041221     c                   UPDATE    TNTBE000
053900041221e   2c                   endsl
054000041221      *
054100041221      * F16=Annullamento
054200041221w   1c                   WHEN      wTasto='16'  and  not *in04
054300041221     c                   movel     'A'           TBEatb
054400041221     c                   clear                   TBEftr
054500041221     c                   UPDATE    TNTBE000
054600041221      *
054700041221e   1c                   ENDSL
054800041221      *
054900041221      * Torno alla prima videata che carico come da inizio pgm
055000041221     c                   movel     '1'           $TipVid
055100041221     c                   movel     *on           $CarV1
055200041221     c                   movel     *on           $CarV2
055300041221     c
055400041221     c                   ENDSR
055500041221
055600041221      *---------------------------------------------------------------*
055700041221      * RIETBE * Riempimento dati tabella                             *
055800041221      *---------------------------------------------------------------*
055900041221     c     RieTBE        BEGSR
056000041221      *
056100041221     c                   clear                   TBEatb
056200041221     c                   if        TBXsif <> *blanks
056300041221     c                   movel     KNSIF         TBEsif
056400041221     c                   else
056500041221     c                   clear                   TBEsif
056600041221     c                   endif
056700041221     c                   movel     TBXapl        TBEapl
056800041221     c                   movel     T02cod        TBEcod
056900041221     c                   movel     V2Cksc        TBEke1
057000041221     c                   clear                   TBEke2
057100041221      *
057200041221     c                   clear                   dCXB
057300041221     c                   eval      §CXBdes = V2Dksc
057400041221     c                   eval      §CXBnot = V2Cnot
057500041221     c                   eval      §CXBdac = V2Hdac
057600041221     c                   movel(p)  dCXB          TBEuni
057700041221      *
057800041221     c                   clear                   TBEftt
057900041221     c                   clear                   TBEflt
058000041221     c                   clear                   TBEftr
058100041221     c                   clear                   TBEdtr
058200041221      *
058300041221     c                   ENDSR
058400041221
058500041221** - $Opz     *
058600041221  Inserimento
058700041221    Modifica
058800041221     Copia
058900041221  Annullamento
059000041221Visualizzazione
059100041221   ANNULLATO
059200041221** - $Msg                                                                    *
059300041221Immettere il codice cliente                                                    01
059400041221Codice cliente errato                                                          02
059500041221Immettere le note                                                              03
