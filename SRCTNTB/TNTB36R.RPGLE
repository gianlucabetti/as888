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
002600091005     d $Msg            s             78    dim(04) ctdata perrcd(1)             Messaggi video
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
004800091005     d fnlv59ds      e ds                  inz
004900091005     d
005000091005     d TIBS34DS      e ds                  inz
005100041221     d DDATIUTE      e ds                  inz
005200041221     d AZUTEDS       e ds                  extname(AZUTE00F) inz
005300041221      *
005400041221      * Decodifica anagrafica clienti
005500041221     d TIBS69ds      e ds                  inz
005600041221     d ds_CNACO      e ds                  extname(CNACO00F) inz
005700041221     d ds_CNIND      e ds                  extname(CNIND00F) inz
005800041221     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
005900041221     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
006000041221      *
006100041221     d WLBdat          ds                  inz
006200041221     d  G02dat                 1      8  0 inz
006300041221     d  G02inv                 9     16  0 inz
006400041221     d  G02err                17     17    inz('3')
006500041221     d  G02tgi                18     22  0 inz
006600041221      *
006700041221     d                sds
006800041221     d  VTCpgm           *proc
006900041221
007000041221      *---------------------------------------------------------------*
007100041221      *   V A R I A B I L I                                           *
007200041221      *---------------------------------------------------------------*
007300041221      *
007400041221     d $Fine           s              1a   inz(*off)
007500041221     d $CarV1          s              1a   inz(*on)
007600041221     d $CarV2          s              1a   inz(*off)
007700041221     d $TipVid         s              1a   inz('1')
007800041221     d $Tibs02         s              1a   inz(*off)
007900041221     d*** Win             s             99a   inz(*zeros)
008000041221     d wTasto          s              2a   inz(*zeros)
008100041221     d wDate           s               d   datfmt(*iso) inz(*loval)
008200041221
008300041221      *---------------------------------------------------------------*
008400041221      *   M A I N   L I N E                                           *
008500041221      *---------------------------------------------------------------*
008600041221      *  Riepilogo indicatori utilizzati:                             *
008700041221      *  --------------------------------                             *
008800041221      *  01 - Record inesistente (inserimento)                        *
008900041221      *  02 - Record esistente   (modifica)                           *
009000041221      *  04 - Record annullato   (ripristino)                         *
009100041221      *  20 - Comodo                                                  *
009200041221      *  22 - Errori in scrittura record (WRITE)                      *
009300041221      *  43 - Visualizzazione "Data creazione"                        *
009400041221      *  50 - Posizionamento cursore su "Codice cliente"              *
009500041221      *  51 - Posizionamento cursore su "Note"                        *
009600041221      *  99 - Visualizzazione messaggio di errore                     *
009700041221      *---------------------------------------------------------------*
009800041221      *
009900041221      * Operazioni iniziali
010000041221     c                   exsr      RutInz
010100041221      *
010200041221      * Gestione video
010300041221     c                   dow       $Fine = *off
010400041221     c     $TipVid       caseq     '1'           GesV1
010500041221     c     $TipVid       caseq     '2'           GesV2
010600041221     c                   endcs
010700041221     c                   enddo
010800041221      *
010900041221      * Fine
011000041221     c                   if        $Tibs02 = *on
011100041221     c                   clear                   TIBS02ds
011200041221     c                   movel     'C'           T02tla
011300041221     c                   call      'TIBS02R'
011400041221     c                   parm                    KPJBA
011500041221     c                   parm                    TIBS02ds
011600041221     c                   endif
011700091006
011800091006     c                   clear                   fnlv59ds
011900091006     c                   movel     'C'           ilv59tla
012000091006     c                   call      'FNLV59R'
012100091006     c                   parm                    fnlv59ds
012200041221      *
012300041221     c                   movel     *on           *inLR
012400041221
012500041221      *---------------------------------------------------------------*
012600041221      * RutInz - Operazioni Iniziali                                  *
012700041221      *---------------------------------------------------------------*
012800041221     c     RutInz        BEGSR
012900041221      *
013000041221      * Ricezione parametri
013100041221     c     *entry        plist
013200041221     c                   parm                    KPJBA
013300041221      *
013400041221      * Definizioni chiavi di accesso
013500041221     c     K05TBE01      klist                                                  *tntbe01l
013600041221     c                   kfld                    TBEcod                         -tabella
013700041221     c                   kfld                    TBEke1                         -chiave uno
013800041221     c                   kfld                    TBEke2                         -chiave due
013900041221     c                   kfld                    TBElin                         -lingua
014000041221     c                   kfld                    TBEsif                         -s.informativo
014100041221      *
014200041221      * Reperisco la data odierna
014300041221     c                   movel     *date         Wdate
014400041221      *
014500041221      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
014600041221     c     *dtaara       define    §azute        azuteds
014700041221     c     *dtaara       define    §datiute      ddatiute
014800041221      *
014900041221     c                   clear                   AzUteDs
015000041221     c                   clear                   DDatiUte
015100041221     c                   clear                   Tibs34Ds
015200041221     c                   in(E)     *dtaara
015300041221if  1c                   if        %Error  or  RSUT = *blanks
015400041221     c                   call      'TIBS34R'
015500041221     c                   parm                    Tibs34Ds
015600041221     c                   in        *dtaara
015700041221e   1c                   endif
015800041221      *-- Verifica errori e autorità profilo
015900041221sel 1c                   SELECT
016000041221      *-- controllo se ho errori nei dati utente
016100041221      *--   nel qual caso NON risulta un profilo abilitato
016200041221w   1c                   WHEN      DUTerr = 'E'
016300041221     c                   eval      $Fine  = *on
016400041221      *
016500041221      *-- CONTROLLO AUTORITA'
016600041221      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
016700041221      *-- se il chiamante non richiede autorità specifica verificare
016800041221      *--   quella del profilo
016900041221      *-- se il chiamante richiede autorità specifica verificarla,
017000041221      *--  se è blank verificare quella del profilo
017100041221      *
017200041221      * se UTEAUT = *BLANK non siamo in sede
017300041221w   1c                   WHEN      UTEaut = *blank
017400041221      *
017500041221x   1c                   OTHER
017600041221      *
017700041221e   1c                   ENDSL
017800041221      *
017900041221      * Aggancio dati generali della tabella in esame
018000041221     c                   clear                   TBEcod
018100041221     c                   move      *zeros        TBEke1
018200041221     c                   move      T02cod        TBEke1
018300041221     c                   clear                   TBEke2
018400041221     c                   clear                   TBElin
018500041221     c                   movel     KNSIF         TBEsif
018600041221     c     K05TBE01      chain     TNTBE01L
018700041221     c                   if        not %found(TNTBE01L)
018800041221     c                   clear                   TBEsif
018900041221     c     K05TBE01      chain     TNTBE01L
019000041221     c                   endif
019100041221     c                   if        %found(TNTBE01L)
019200041221     c                   movel     TNTBEds       xTNTBEds
019300041221     c                   else
019400041221     c                   clear                   xTNTBEds
019500041221     c                   endif
019600041221      *
019700041221     c                   clear                   TB36V1
019800041221      *
019900041221     c                   ENDSR
020000041221
020100041221      *---------------------------------------------------------------*
020200041221      * GESV1  - Gestione videata selezione codice tabella            *
020300041221      *---------------------------------------------------------------*
020400041221     c     GesV1         BEGSR
020500041221      *
020600041221      * Inizializzazione videata
020700041221if  1c                   if        $CarV1 = *on
020800041221     c                   exsr      CarV1
020900041221     c                   movel     *off          $CarV1
021000041221e   1c                   endif
021100041221      *
021200041221      * Scrivo la testata
021300041221     c                   clear                   T1opz
021400041221     c                   write     TB36T1
021500041221      *
021600041221      * Se esistono errori sulla videata
021700041221      * emetto la write del formato a indicatori spenti per vedere
021800041221      * le eventuali decodifiche
021900041221      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
022000041221if  1c***                if        *in99
022100041221     c***                movea     *in           Win
022200041221     c***                movea     *zeros        *in(50)
022300041221     c***                write     TB36V1
022400041221     c***                movea     Win           *in
022500041221e   1c***                endif
022600041221      *
022700041221     c                   exfmt     TB36V1
022800041221     c                   eval      *in99 = *off
022900041221     c                   clear                   V1Dmsg
023000041221      * F3=Fine
023100041221if  1c                   if        *inKC
023200041221     c                   exsr      F03V1
023300041221     c                   goto      EndGesV1
023400041221e   1c                   endif
023500041221      *
023600041221      * Controllo dati immessi a video
023700041221     c                   exsr      CtrV1
023800041221     c     *in99         cabeq     *on           EndGesV1
023900041221      *
024000041221      * Passaggio alla videata di dettaglio
024100041221if  1c                   if        not *in99
024200041221     c                   eval      $CarV2 = *on
024300041221     c                   eval      $TipVid = '2'
024400041221e   1c                   endif
024500041221      *
024600041221     c     EndGesV1      ENDSR
024700041221
024800041221      *---------------------------------------------------------------*
024900041221      * CARV1  - Caricamento dati prima videata                       *
025000041221      *---------------------------------------------------------------*
025100041221     c     CarV1         BEGSR
025200041221      *
025300041221     c                   movea     *zeros        *in(40)
025400041221     c                   movea     '00000'       *in(01)
025500041221      *
025600041221      * Imposto il campo "codice cliente" per l'interrogazione
025700041221     c                   movel(p)  '?'           V1Cksc
025800041221      *
025900041221     c                   ENDSR
026000041221
026100041221      *---------------------------------------------------------------*
026200041221      * CTRV1  - Controllo e decodifica prima videata                 *
026300041221      *---------------------------------------------------------------*
026400041221     c     CtrV1         BEGSR
026500041221      *
026600041221     c                   movea     *zeros        *in(50)
026700041221     c                   clear                   V1Dmsg
026800041221      *
026900041221      * Codice cliente
027000041221      * - Ricerca:
027100041221     c     '?'           scan      V1Cksc                                 20
027200041221if  1c                   if        *in20
027300041221     c                   eval      $Tibs02 = *on
027400041221     c                   clear                   V1Cksc
027500041221     c                   eval      *in50   = *on
027600041221     c                   reset                   TIBS02ds
027700041221     c                   movel     KNSIF         T02sif
027800041221     c                   call      'TIBS02R'
027900041221     c                   parm                    KPJBA
028000041221     c                   parm                    TIBS02ds
028100041221if  2c                   if        T02err  = *blanks
028200041221     c                   movel     T02uni        dCXB
028300041221     c                   movel     T02ke1        V1Cksc
028400041221x   2c                   else
028500041221     c                   eval      *in99   = *on
028600041221     c                   movel     T02msg        V1Dmsg
028700041221     c                   endif
028800041221e   1c                   endif
028900041221      * - Controllo:
029000041221      * - - effettiva immissione
029100041221if  1c                   if           V1Cksc  = *blanks
029200041221     c                             or V1Cksc = *zeros
029300041221     c                   seton                                        50  99
029400041221     c                   eval      V1Dmsg  = $Msg(01)
029500041221     c                   goto      EndCtrV1
029600041221x   1c                   else
029700041221      * - - numericità
029800041221     c     DigitN        check     V1Cksc                                 10
029900041221if  2c                   if        *in10  =  *on
030000041221     c                   seton                                        50  99
030100041221     c                   eval      V1Dmsg  = $Msg(02)
030200041221     c                   goto      EndCtrV1
030300041221e   2c                   endif
030400041221      * - - decodifica
030500041221     c                   exsr      DecodCLI
030600041221e   1c                   endif
030700041221      *
030800041221     c     EndCtrV1      ENDSR
030900041221
031000041221      *---------------------------------------------------------------*
031100041221      * DecodCLI   * Decodifica codice cliente                        *
031200041221      *---------------------------------------------------------------*
031300041221     c     DecodCLI      BEGSR
031400041221      *
031500041221     c                   clear                   TIBS69ds
031600041221     c                   clear                   ds_CNACO
031700041221     c                   clear                   ds_CNIND
031800041221     c                   clear                   ds_CNCLP
031900041221     c                   clear                   ds_FNCLS
032000041221     c                   move      V1Cksc        I69kac
032100041221     c                   call      'TIBS69R'
032200041221     c                   parm                    TIBS69ds
032300041221     c                   parm                    ds_CNACO
032400041221     c                   parm                    ds_CNIND
032500041221     c                   parm                    ds_CNCLP
032600041221     c                   parm                    ds_FNCLS
032700041221      *
032800041221     c                   if        O69err = *blanks
032900041221     c                   movel     ACOrag        V2Dksc
033000041221     c                   else
033100041221     c                   seton                                        50  99
033200041221     c                   eval      V1Dmsg  = $Msg(02)
033300041221     C                   endif
033400041221      *
033500041221     c                   ENDSR
033600041221
033700041221      *---------------------------------------------------------------*
033800041221      * F03V1  - Tasto funzionale F03 -> Fine programma               *
033900041221      *---------------------------------------------------------------*
034000041221     c     F03V1         BEGSR
034100041221      *
034200041221     c                   movel     *on           $Fine                          fine pgm
034300041221      *
034400041221     c                   ENDSR
034500041221
034600041221      *---------------------------------------------------------------*
034700041221      * GESV2  - Gestione videata dettaglio dati                      *
034800041221      *---------------------------------------------------------------*
034900041221     c     GesV2         BEGSR
035000041221      *
035100041221      * Inizializzazione videata
035200041221     c                   if        $CarV2 = *on
035300041221     c                   exsr      CarV2
035400041221     c                   move      *off          $CarV2
035500041221     c                   endif
035600041221      * Scrivo la testata
035700041221     c                   write     TB36T1
035800041221      *
035900041221      * Se esistono errori sulla videata
036000041221      * emetto la write del formato a indicatori spenti per vedere
036100041221      * le eventuali decodifiche
036200041221      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
036300041221if  1c***                if        *in99
036400041221     c***                movea     *in           Win
036500041221     c***                movea     *zeros        *in(50)
036600041221     c***                write     TB36V2
036700041221     c***                movea     Win           *in
036800041221e   1c***                endif
036900041221      *
037000041221if  1c                   if        *in05
037100041221     c                   write     TB36V2
037200041221     c                   exfmt     PROTECT
037300041221x   1c                   else
037400041221     c                   exfmt     TB36V2
037500041221e   1c                   endif
037600041221     c                   eval      *in99 = *off
037700041221     c                   clear                   V1Dmsg
037800041221     c                   clear                   wTasto
037900041221      *
038000041221sel 1c                   select
038100041221      * F03=Fine
038200041221w   1c                   when      *inKC
038300041221     c                   exsr      F03V1
038400041221     c                   goto      EndGesV2
038500041221      * F12=Ritorno
038600041221w   1c                   when      *inKL
038700041221     c                   exsr      F12V2
038800041221     c                   goto      EndGesV2
038900041221e   1c                   endsl
039000041221      *
039100041221      * Controllo dati immessi a video
039200041221      * (non si fanno se richisto l'annullamento)
039300041221if  1c                   if        not *inKQ
039400041221     c                   exsr      CtrV2
039500041221e   1c                   endif
039600041221      *
039700041221      * Aggiornamento se non ci sono errori
039800041221if  1c                   if        not *in99
039900041221     c                             and (*inKF or *inKE or *inKQ)
040000041221sel 2c                   select
040100041221w   2c                   when      *inKE
040200041221     c                   eval      wTasto = '05'
040300041221w   2c                   when      *inKF
040400041221     c                   eval      wTasto = '06'
040500041221w   2c                   when      *inKQ
040600041221     c                   eval      wTasto = '16'
040700041221e   2c                   endsl
040800041221     c                   exsr      AggTBE
040900041221e   1c                   endif
041000041221      *
041100041221     c     EndGesV2      ENDSR
041200041221
041300041221      *---------------------------------------------------------------*
041400041221      * CARV2  - Caricamento dati seconda videata                     *
041500041221      *---------------------------------------------------------------*
041600041221     c     CarV2         BEGSR
041700041221      *
041800041221     c                   clear                   T1opz
041900041221     c                   movea     '00000'       *in(01)
042000041221     c                   eval      *in43 = *off
042100041221      *
042200041221      * reimposto il campo chiave (qui di solo output)
042300041221     c                   movel     V1Cksc        V2Cksc
042400041221     c                   movel     ACOrag        V2Dksc
042500041221      *
042600041221      * ripulisco i campi in manutenzione in questa videata
042700041221      * (e relative decodifiche)
042800041221     c                   clear                   V2Cnot
042900041221     c                   clear                   V2Cdac
043000041221     c                   clear                   V2Hdac
043100091005     c                   clear                   V2cctr
043200091006     c                   clear                   V2dctr
043300041221      *
043400041221      * Aggancio la tabella, se trovo il codice sono in modifica
043500041221      * o ripristino (se record annullato), altrimenti in immissione
043600041221     c                   exsr      ChnTBE
043700041221      *
043800041221if  1c                   if        %found(TNTBE01L)
043900041221      *
044000041221      * MODIFICA/RIPRISTINO
044100041221     c                   eval      *in43  = *on
044200041221     c                   movel     TBEuni        dCXB
044300041221if  2c                   if        TBEatb = *blanks
044400041221     c                   eval      *in02  = *on
044500041221     c                   eval      T1opz  = $Opz(02)
044600041221x   2c                   else
044700041221     c                   eval      *in04  = *on
044800041221     c                   eval      T1opz  = $Opz(06)
044900041221e   2c                   endif
045000091005
045100091005     c* Riempimento campi video
045200091005     c                   eval      V2Cnot = §CXBnot
045300091005     c                   eval      V2Cctr = §CXBctr
045400091005
045500091005     c                   reset                   WLBdat
045600091005     c                   z-add     §CXBdac       G02inv
045700091005     c                   call      'XSRDA8'
045800091005     c                   parm                    WLBdat
045900091005     c                   z-add     G02dat        V2Cdac
046000091005     c                   eval      V2Hdac = §CXBdac
046100041221      *
046200041221x   1c                   else
046300041221      *
046400041221      * IMMISSIONE
046500041221     c                   clear                   dCXB
046600041221     c                   eval      *in01  = *on
046700041221     c                   eval      T1opz  = $Opz(01)
046800091005
046900091005     c                   movel     *date         V2Cdac
047000091005     c                   movel     Wdate         V2Hdac
047100041221      *
047200041221e   1c                   endif
047300041221      *
047400041221      *
047500041221      *
047600041221     c                   ENDSR
047700041221
047800041221      *---------------------------------------------------------------*
047900041221      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
048000041221      *---------------------------------------------------------------*
048100041221     c     F12V2         BEGSR
048200041221      *
048300041221     c                   eval      $TipVid = '1'
048400041221      *
048500041221     c                   ENDSR
048600041221
048700041221      *---------------------------------------------------------------*
048800041221      * CTRV2  - Controllo e decodifica seconda videata               *
048900041221      *---------------------------------------------------------------*
049000041221     c     CtrV2         BEGSR
049100041221      *
049200041221     c                   movea     *zeros        *in(50)
049300041221     c                   clear                   V1Dmsg
049400041221      *
049500091005      * cocie tariffa non obbligatorio ma deve  essere esistente
049600091006     c                   clear                   v2dctr
049700091005     c                   if        v2cctr<>*blanks
049800091005     c                   clear                   fnlv59ds
049900091005     C                   MOVEL     'P'           ilv59tbo
050000091005     C                   MOVEL     V2CCTR        ilv59ctr
050100091005     C                   MOVEL     v1cksc        ilv59ksc
050200091005     C                   MOVEL     dutkci        ilv59kci
050300091005     C                   MOVEL     'S'           ilv59con
050400091006     C                   MOVEL     'I'           ilv59fie
050500091005     C                   MOVEL     wdate         ilv59dsp
050600091005     C                   CALL      'FNLV59R'
050700091005     C                   PARM                    fnlv59ds
050800091005     c* Attivata ricerca
050900091005     c                   if        olv59err='R'
051000091005     c                   movel     olv59ctr      v2cctr
051100091006     c                   movel     olv59dfs      v2dctr
051200091005     c                   goto      EndCtrV2
051300091005     c                   endif
051400091005     c
051500091005     c                   if        olv59err<>*blanks
051600091005     c                   seton                                        52  99
051700091005     c                   movel     olv59msg      V1Dmsg
051800091005     c                   goto      EndCtrV2
051900091005     c                   endif
052000091006     c
052100091006     c                   movel     olv59dfs      v2dctr
052200091005     c                   endif
052300091005     c
052400091005      *
052500041221      * Note obbligatorie
052600041221if  1c                   if        V2Cnot = *blanks
052700041221     c                   seton                                        51  99
052800041221     c                   movel     $Msg(03)      V1Dmsg
052900041221     c                   goto      EndCtrV2
053000041221e   1c                   endif
053100041221      *
053200041221     c     EndCtrV2      ENDSR
053300041221
053400041221      *---------------------------------------------------------------*
053500041221      * CHNTBE * Aggancio tabella                                     *
053600041221      *---------------------------------------------------------------*
053700041221     c     ChnTBE        BEGSR
053800041221      *
053900041221     c                   movel     T02cod        TBEcod
054000041221     c                   movel(p)  V2Cksc        TBEke1
054100041221     c                   clear                   TBEke2
054200041221     c                   clear                   TBElin
054300041221     c                   movel     KNSIF         TBEsif
054400041221     c     K05TBE01      chain     TNTBE01L
054500041221      * Se non ho trovato il record con il sistema informativo
054600041221      * che ho in linea, lo abblenco
054700041221if  1c                   if        not %found(TNTBE01L)
054800041221     c                   clear                   TBEsif
054900041221     c     K05TBE01      chain     TNTBE01L
055000041221e   1c                   endif
055100041221      *
055200041221     c                   ENDSR
055300041221
055400041221      *---------------------------------------------------------------*
055500041221      * AGGTBE * Aggiornamento tabella                                *
055600041221      *---------------------------------------------------------------*
055700041221     c     AggTBE        BEGSR
055800041221      *
055900041221sel 1c                   SELECT
056000041221      *
056100041221      * F5=Ripristino
056200041221w   1c                   WHEN      wTasto='05'  and  *in04
056300041221     c                   clear                   TBEatb
056400041221     c                   clear                   TBEftr
056500041221     c                   UPDATE    TNTBE000
056600041221      *
056700041221      * F6=Conferma
056800041221w   1c                   WHEN      wTasto='06'
056900041221     c                   exsr      RieTBE
057000041221sel 2c                   select
057100041221      *   Immissione
057200041221w   2c                   when      *in01
057300041221     c                   clear                   TBEflt
057400041221     c                   clear                   TBEdtr
057500041221     c                   WRITE     TNTBE000                             22
057600041221      *   Modifica / Ripristino
057700041221w   2c                   when      *in02   or    *in04
057800041221     c                   UPDATE    TNTBE000
057900041221e   2c                   endsl
058000041221      *
058100041221      * F16=Annullamento
058200041221w   1c                   WHEN      wTasto='16'  and  not *in04
058300041221     c                   movel     'A'           TBEatb
058400041221     c                   clear                   TBEftr
058500041221     c                   UPDATE    TNTBE000
058600041221      *
058700041221e   1c                   ENDSL
058800041221      *
058900041221      * Torno alla prima videata che carico come da inizio pgm
059000041221     c                   movel     '1'           $TipVid
059100041221     c                   movel     *on           $CarV1
059200041221     c                   movel     *on           $CarV2
059300041221     c
059400041221     c                   ENDSR
059500041221
059600041221      *---------------------------------------------------------------*
059700041221      * RIETBE * Riempimento dati tabella                             *
059800041221      *---------------------------------------------------------------*
059900041221     c     RieTBE        BEGSR
060000041221      *
060100041221     c                   clear                   TBEatb
060200041221     c                   if        TBXsif <> *blanks
060300041221     c                   movel     KNSIF         TBEsif
060400041221     c                   else
060500041221     c                   clear                   TBEsif
060600041221     c                   endif
060700041221     c                   movel     TBXapl        TBEapl
060800041221     c                   movel     T02cod        TBEcod
060900041221     c                   movel     V2Cksc        TBEke1
061000041221     c                   clear                   TBEke2
061100041221      *
061200041221     c                   clear                   dCXB
061300041221     c                   eval      §CXBdes = V2Dksc
061400041221     c                   eval      §CXBnot = V2Cnot
061500041221     c                   eval      §CXBdac = V2Hdac
061600091006     c                   eval      §CXBctr = V2cctr
061700041221     c                   movel(p)  dCXB          TBEuni
061800041221      *
061900041221     c                   clear                   TBEftt
062000041221     c                   clear                   TBEflt
062100041221     c                   clear                   TBEftr
062200041221     c                   clear                   TBEdtr
062300041221      *
062400041221     c                   ENDSR
062500041221
062600041221** - $Opz     *
062700041221  Inserimento
062800041221    Modifica
062900041221     Copia
063000041221  Annullamento
063100041221Visualizzazione
063200041221   ANNULLATO
063300041221** - $Msg                                                                    *
063400041221Immettere il codice cliente                                                    01
063500041221Codice cliente errato                                                          02
063600041221Immettere le note                                                              03
063700091005Immettere un codice tariffa valido ed esistente per il cliente                 04
