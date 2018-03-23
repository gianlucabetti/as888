000100060403      *---------------------------------------------------------------*
000200161129      * Gestione tabella "VPC" = Abilitazioni a Clienti VAS           *
000300060403      *---------------------------------------------------------------*
000400060403
000500060403     h decedit('0,') datedit(*dmy/)
000600060403
000700060403      *---------------------------------------------------------------*
000800060403      *   A R C H I V I                                               *
000900060403      *---------------------------------------------------------------*
001000060403      *
001100071207     fTabel00f  if   e           k disk
001200120919     Fazorg01L  IF   E           K DISK
001300091125     fTNTBE01L  uf a e           k disk
001400060403      *
001500161129     fTNTBA06D  cf   e             workstn
001600060403
001700060403      *---------------------------------------------------------------*
001800060403      *   C O S T A N T I                                             *
001900060403      *---------------------------------------------------------------*
002000060403      *
002100060403     d DigitN          c                   const('0123456789')
002200060403
002300060403      *---------------------------------------------------------------*
002400060403      *   S C H I E R E                                               *
002500060403      *---------------------------------------------------------------*
002600060403      *
002700060403     d $Opz            s             15    dim( 6) ctdata perrcd(1)             Decodifica OPZ
002800170824     d $Msg            s             78    dim(12) ctdata perrcd(1)             Messaggi video
002900060403
003000060403      *---------------------------------------------------------------*
003100060403      *   S T R U T T U R E   D A T I                                 *
003200060403      *---------------------------------------------------------------*
003300060403      *
003400060403      * Parametri
003500060403     d KPJBA         e ds
003600060403      *
003700060403      * Passaggio Parametri al pgm TIBS02R
003800060403     d TIBS02ds      e ds                  inz
003900060403     d  T02mod       e                     inz('R')
004000161129     d  T02cod       e                     inz('VPC')
004100060403      *
004200161129      * Tabella VPC = Clienti ABILITAZIONI VAS
004300161129     d DVPC          e ds                  inz
004400071207
004500071207     d ds3c          e ds                  inz
004600131002     d ds5e          e ds                  inz
004700060403      *
004800060403      * Tracciato record file TNTBE00F
004900060403     d TNTBEds       e ds                  extname(TNTBE00F) inz
005000060403     d xTNTBEds      e ds                  extname(TNTBE00F) inz
005100060403     d                                     prefix(TBX:3)
005200060403      *
005300060403     d TIBS34ds      e ds                  inz
005400060403     d dDatiUte      e ds                  inz
005500060403     d AZUTEds       e ds                  extname(AZUTE00F) inz
005600060403      *
005700060403      * Decodifica anagrafica clienti
005800060403     d TIBS69ds      e ds                  inz
005900060403     d ds_CNACO      e ds                  extname(CNACO00F) inz
006000060403     d ds_CNIND      e ds                  extname(CNIND00F) inz
006100060403     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
006200060403     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
006300151105      *
006400060403     d WLBdat          ds                  inz
006500060403     d  G02dat                 1      8  0 inz
006600060403     d  G02inv                 9     16  0 inz
006700060403     d  G02err                17     17    inz('3')
006800060403     d  G02tgi                18     22  0 inz
006900060403      *
007000060403     d Status         sds
007100060403     d  VTCpgm           *proc
007200060403
007300060403      *---------------------------------------------------------------*
007400060403      *   V A R I A B I L I                                           *
007500060403      *---------------------------------------------------------------*
007600060403      *
007700060403     d $Fine           s              1a   inz(*off)
007800060403     d $InzV1          s              1a   inz(*on)
007900060403     d $InzV2          s              1a   inz(*off)
008000060403     d $InzW1          s              1a   inz(*off)
008100060403     d $TipVid         s              1a   inz('1')
008200060403     d $TIBS02         s              1a   inz(*off)
008300060403     d*** Win             s             99a   inz(*zeros)
008400060403     d wTasto          s              2a   inz(*zeros)
008500060403     d wDate           s               d   datfmt(*iso) inz(*loval)
008600071207     d w_diskb         s              1a
008700071207     d w_forzb         s              1a
008800071207     d codut           s              1  0 inz(1)
008900071207     d cod             s                   like(tblcod)
009000071207     d key             s                   like(tblkey)
009100131002     d §key            s                   like(tblkey)
009200060403
009300060403      *---------------------------------------------------------------*
009400060403      *   M A I N   L I N E                                           *
009500060403      *---------------------------------------------------------------*
009600060403      *  Riepilogo indicatori utilizzati:                             *
009700060403      *  --------------------------------                             *
009800060403      *  01 - Record inesistente (inserimento)                        *
009900060403      *  02 - Record esistente   (modifica)                           *
010000060403      *  04 - Record annullato   (ripristino)                         *
010100060403      *  10 - Comodo                                                  *
010200060403      *  22 - Errori in scrittura record (WRITE)                      *
010300060403      *  28 - Emissione messaggio di errore a video                   *
010400060403      *  50 - ERR: Codice cliente                                     *
010500170421      *  52 - ERR: su maschera file                                   *
010600131001      *  65 - ERR:                                                    *
010700060403      *  90 - Riemissione videata                                     *
010800060403      *---------------------------------------------------------------*
010900060403      *
011000060403      * Operazioni iniziali
011100060403     c                   exsr      RutInz
011200060403      *
011300060403      * Gestione video
011400060403     c                   dow       $Fine   = *off
011500060403     c     $TipVid       caseq     '1'           GesV1
011600060403     c     $TipVid       caseq     '2'           GesV2
011700060403     c     $TipVid       caseq     'A'           GesW1
011800060403     c                   endcs
011900060403     c                   enddo
012000060403      *
012100060403      * Fine
012200060403     c                   if        $TIBS02 = *on
012300060403     c                   clear                   TIBS02ds
012400060403     c                   movel     'C'           T02tla
012500060403     c                   call      'TIBS02R'
012600060403     c                   parm                    KPJBA
012700060403     c                   parm                    TIBS02ds
012800060403     c                   endif
012900060403      *
013000060403     c                   movel     *on           *inLR
013100060403
013200060403      *---------------------------------------------------------------*
013300060403      * RutInz - Operazioni Iniziali                                  *
013400060403      *---------------------------------------------------------------*
013500060403     c     RutInz        BEGSR
013600060403      *
013700060403      * Ricezione parametri
013800060403     c     *entry        plist
013900060403     c                   parm                    KPJBA
014000060403      *
014100060403      * Definizioni chiavi di accesso
014200060403     c     K05TBE01      klist                                                  *tntbe01l
014300060403     c                   kfld                    TBEcod                         -tabella
014400060403     c                   kfld                    TBEke1                         -chiave uno
014500060403     c                   kfld                    TBEke2                         -chiave due
014600060403     c                   kfld                    TBElin                         -lingua
014700060403     c                   kfld                    TBEsif                         -s.informativo
014800071207     C*
014900071207     C     KTAB2         KLIST
015000071207     C                   KFLD                    codut
015100071207     C                   KFLD                    cod
015200071207     C                   KFLD                    key
015300060403      *
015400060403      * Reperisco la data odierna
015500060403     c                   movel     *date         Wdate
015600060403      *
015700060403      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
015800060403     c     *dtaara       define    §azute        AZUTEds
015900060403     c     *dtaara       define    §datiute      dDatiUte
016000060403      *
016100060403     c                   clear                   AZUTEds
016200060403     c                   clear                   dDatiUte
016300060403     c                   clear                   TIBS34ds
016400060403     c                   in(E)     *dtaara
016500060403if  1c                   if        %Error  or  RSUT = *blanks
016600060403     c                   call      'TIBS34R'
016700060403     c                   parm                    TIBS34ds
016800060403     c                   in        *dtaara
016900060403e   1c                   endif
017000060403      *-- Verifica errori e autorità profilo
017100060403sel 1c                   SELECT
017200060403      *-- controllo se ho errori nei dati utente
017300060403      *--   nel qual caso NON risulta un profilo abilitato
017400060403w   1c                   WHEN      DUTerr  = 'E'
017500060403     c                   eval      $Fine   = *on
017600060403      *
017700060403      *-- CONTROLLO AUTORITA'
017800060403      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
017900060403      *-- se il chiamante non richiede autorità specifica verificare
018000060403      *--   quella del profilo
018100060403      *-- se il chiamante richiede autorità specifica verificarla,
018200060403      *--  se è blank verificare quella del profilo
018300060403      *
018400060403      * se UTEAUT = *BLANK non siamo in sede
018500060403w   1c                   WHEN      UTEaut  = *blank
018600060403      *
018700060403x   1c                   OTHER
018800060403      *
018900060403e   1c                   ENDSL
019000060403      *
019100060403      * Aggancio dati generali della tabella in esame
019200060403     c                   clear                   TBEcod
019300060403     c                   move      *zeros        TBEke1
019400060403     c                   move      T02cod        TBEke1
019500060403     c                   clear                   TBEke2
019600060403     c                   clear                   TBElin
019700060403     c                   movel     KNSIF         TBEsif
019800060403     c     K05TBE01      chain     TNTBE01L
019900060403     c                   if        not %found(TNTBE01L)
020000060403     c                   clear                   TBEsif
020100060403     c     K05TBE01      chain     TNTBE01L
020200060403     c                   endif
020300060403     c                   if        %found(TNTBE01L)
020400060403     c                   movel     TNTBEds       xTNTBEds
020500060403     c                   else
020600060403     c                   clear                   xTNTBEds
020700060403     c                   endif
020800060403      *
020900161129     c                   clear                   TBA06V1
021000120925     C*
021100120925     C* ctrl i dati presenti in KPJBU
021200170720     C* aggiunto il dato KE2 dalla pos 8 x 3
021300120925     C* se da pos. 1 x 7 ci sono solo caratteri numerici, allora è arrivato in ingresso il codice
021400120925     C* cliente, quindi lo testo e se corretto passo subito alla seconda videata
021500120925x   1c                   IF        KPJBU <> *blank
021600120925     C* faccio in modo che anche se devo andare in videata 1 per errore rimanga il codice ricevuto
021700161129     c                   clear                   TBA06V1
021800120925if  1c                   EVAL      $InzV1  = *off
021900170720x   1c                   EVAL      V1CKE2 = %subst(KPJBU : 8 : 3)
022000170720x   1c                   EVAL      V1CKsc = %subst(KPJBU : 1 : 7)
022100120925x   1c                   TESTN                   V1CKsc               10
022200120925     C                   IF        *in10 = *on                                  *E'NUMERO
022300120925     C* e l'ultimo carattere non è il segno ma una cifra anch'essa
022400120925     C                             and %subst(V1CKsc : 7 : 1) >= '0'
022500120925     c                   exsr      CtrV1
022600120925     C* se non rilevati errori
022700120925if  2c                   if        *in90 = *off
022800120925     C* Passo alla videata di dettaglio
022900120925     c                   eval      $InzV2  = *on
023000120925     c                   eval      $TipVid = '2'
023100120925     C                   ENDIF
023200120925     C                   ENDIF
023300120925     C                   ENDIF
023400060403      *
023500060403     c                   ENDSR
023600060403
023700060403      *---------------------------------------------------------------*
023800060403      * GESV1  - Gestione videata selezione codice tabella            *
023900060403      *---------------------------------------------------------------*
024000060403     c     GesV1         BEGSR
024100060403      *
024200060403      * Inizializzazione videata
024300060403if  1c                   if        $InzV1  = *on
024400060403     c                   exsr      InzV1
024500060403     c                   movel     *off          $InzV1
024600060403e   1c                   endif
024700060403      *
024800060403      * Scrivo la testata
024900161129     c                   write     TBA06T1
025000060403      *
025100060403      * Se esistono errori sulla videata
025200060403      * emetto la write del formato a indicatori spenti per vedere
025300060403      * le eventuali decodifiche
025400060403      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
025500060403if  1c***                if        *in90
025600060403     c***                movea     *in           Win
025700060403     c***                movea     *zeros        *in(50)
025800161129     c***                write     TBA06V1
025900060403     c***                movea     Win           *in
026000060403e   1c***                endif
026100060403      *
026200161129     c                   exfmt     TBA06V1
026300060403     c                   setoff                                       28  90
026400060403     c                   clear                   V1Dmsg
026500060403      *
026600060403sel 1c                   select
026700060403      * F3=Fine
026800060403w   1c                   when      *inKC
026900060403     c                   exsr      F03V1
027000060403     c                   leavesr
027100060403      *
027200060403      * Controllo dati immessi a video
027300060403x   1c                   other
027400060403     c                   exsr      CtrV1
027500060403      * - Rilevati Errori
027600060403if  2c                   if        *in90
027700060403     c                   leavesr
027800060403e   2c                   endif
027900060403      * - Passaggio alla videata di dettaglio
028000060403     c                   eval      $InzV2  = *on
028100060403     c                   eval      $TipVid = '2'
028200060403      *
028300060403e   1c                   endsl
028400060403      *
028500060403     c                   ENDSR
028600060403
028700060403      *---------------------------------------------------------------*
028800060403      * INZV1  - Inizializzazione prima videata (V1)                  *
028900060403      *---------------------------------------------------------------*
029000060403     c     InzV1         BEGSR
029100060403      *
029200161129     c                   clear                   TBA06V1
029300060403     *** *
029400060403     ***c                   movea     *zeros        *in(50)
029500060403     ***c                   movea     '00000'       *in(01)
029600060403      *
029700060403      * Imposto il campo "codice cliente" per l'interrogazione
029800060403     c                   move      '?'           V1Cksc
029900170720     c                   clear                   V1Cke2
030000060403      *
030100060403     c                   ENDSR
030200060403
030300060403      *---------------------------------------------------------------*
030400060403      * CTRV1  - Controllo e decodifica prima videata                 *
030500060403      *---------------------------------------------------------------*
030600060403     c     CtrV1         BEGSR
030700060403      *
030800060403     c                   movea     *zeros        *in(50)
030900060403      *
031000060403      * Codice cliente
031100060403      * - Ricerca:
031200060403     c     '?'           scan      V1Cksc                                 10
031300060403if  1c                   if        *in10
031400060403     c                   eval      $TIBS02 = *on
031500060403     c                   clear                   V1Cksc
031600060403     c                   seton                                          5090
031700060403     c                   reset                   TIBS02ds
031800060403     c                   movel     KNSIF         T02sif
031900060403if  2c                   if            DUTlpo             <> 'S'
032000060403     c                             and %subst(DUTute:1:3) <> 'EDP'
032100060403     c                   movel     DUTpou        T02ke1
032200060403e   2c                   endif
032300060403     c                   call      'TIBS02R'
032400060403     c                   parm                    KPJBA
032500060403     c                   parm                    TIBS02ds
032600060403if  2c                   if        T02err  = *blanks
032700161129     c                   movel     T02uni        DVPC
032800060403     c                   movel     T02ke1        V1Cksc
032900170720     c                   movel     T02ke2        V1Cke2
033000060403x   2c                   else
033100060403     c                   seton                                        285090
033200060403     c                   movel     T02msg        V1Dmsg
033300060403     c                   leavesr
033400060403     c                   endif
033500060403e   1c                   endif
033600060403      * - Controllo:
033700060403      * - - effettiva immissione
033800060403if  1c                   if           V1Cksc = *blanks
033900060403     c                             or V1Cksc = *zeros
034000060403     c                   seton                                        285090
034100060403     c                   eval      V1Dmsg  = $Msg(1)
034200060403     c                   leavesr
034300060403e   1c                   endif
034400060403      * - - numericità
034500060403     c     DigitN        check     V1Cksc                                 10
034600060403if  1c                   if        *in10   =  *on
034700060403     c                   seton                                        285090
034800060403     c                   eval      V1Dmsg  = $Msg(2)
034900060403     c                   leavesr
035000060403e   1c                   endif
035100060403      * - - decodifica
035200161129     c                   exsr      DecoDVPC
035300060403      * - - "appartenenza" al P.O. dell'utente
035400060403     c                   if            %subst(V1Cksc:1:3) <> %char(DUTpou)
035500060403     c                             and DUTlpo             <> 'S'
035600060403     c                             and %subst(DUTute:1:3) <> 'EDP'
035700060403     c                   seton                                        285090
035800060403     c                   eval      V1Dmsg  = $Msg(2)
035900060403     c                   leavesr
036000060403     c                   endif
036100170720      *
036200170720      * Codice opzione
036300170720      * - Ricerca:
036400170720     c     '?'           scan      V1Cke2                                 10
036500170720if  1c                   if        *in10
036600170720     c                   eval      $TIBS02 = *on
036700170720     c                   clear                   V1Cke2
036800170720     c                   seton                                          5090
036900170720     c                   reset                   TIBS02ds
037000170720     c                   movel     KNSIF         T02sif
037100170720     c                   movel     'VPR'         T02cod
037200170720     c                   movel     'VPCKE2'      T02ke1
037300170720if  2c                   if            DUTlpo             <> 'S'
037400170720     c                             and %subst(DUTute:1:3) <> 'EDP'
037500170720     c                   movel     DUTpou        T02ke1
037600170720e   2c                   endif
037700170720     c                   call      'TIBS02R'
037800170720     c                   parm                    KPJBA
037900170720     c                   parm                    TIBS02ds
038000170720if  2c                   if        T02err  = *blanks
038100170720     c                   movel     T02uni        V1DKE2
038200170720     c                   movel     T02ke2        V1Cke2
038300170720x   2c                   else
038400170720     c                   seton                                        285090
038500170720     c                   movel     T02msg        V1Dmsg
038600170720     c                   leavesr
038700170720     c                   endif
038800170720     c                   reset                   TIBS02ds
038900170720     c                   else
039000170720     c                   clear                   V1DKE2
039100170720e   1c                   endif
039200170720      * - Controllo:
039300170720      * - - blank è un valore valido
039400060403      *
039500060403     c                   ENDSR
039600060403
039700060403      *---------------------------------------------------------------*
039800161129      * DecoDVPC   * Decodifica codice cliente                        *
039900060403      *---------------------------------------------------------------*
040000161129     c     DecodVPC      BEGSR
040100060403      *
040200060403     c                   clear                   TIBS69ds
040300060403     c                   clear                   ds_CNACO
040400060403     c                   clear                   ds_CNIND
040500060403     c                   clear                   ds_CNCLP
040600060403     c                   clear                   ds_FNCLS
040700060403     c                   move      V1Cksc        I69kac
040800060403     c                   call      'TIBS69R'
040900060403     c                   parm                    TIBS69ds
041000060403     c                   parm                    ds_CNACO
041100060403     c                   parm                    ds_CNIND
041200060403     c                   parm                    ds_CNCLP
041300060403     c                   parm                    ds_FNCLS
041400060403      *
041500060403     c                   if        O69err  = *blanks
041600060403     c                   movel     ACOrag        V2Dksc
041700060403     c                   else
041800060403     c                   seton                                        285090
041900060403     c                   eval      V1Dmsg  = $Msg(2)
042000060403     C                   endif
042100060403      *
042200060403     c                   ENDSR
042300060403
042400060403      *---------------------------------------------------------------*
042500060403      * F03V1  - Tasto funzionale F03 -> Fine programma               *
042600060403      *---------------------------------------------------------------*
042700060403     c     F03V1         BEGSR
042800060403      *
042900060403     c                   movel     *on           $Fine                          fine pgm
043000060403      *
043100060403     c                   ENDSR
043200060403
043300060403      *---------------------------------------------------------------*
043400060403      * GESV2  - Gestione videata dettaglio dati                      *
043500060403      *---------------------------------------------------------------*
043600060403     c     GesV2         BEGSR
043700060403      *
043800060403      * Inizializzazione videata
043900060403     c                   if        $InzV2  = *on
044000060403     c                   exsr      InzV2
044100060403     c                   move      *off          $InzV2
044200060403     c                   endif
044300060403      *
044400060403      * Scrivo la testata
044500161129     c                   write     TBA06T1
044600060403      *
044700060403      * Se esistono errori sulla videata
044800060403      * emetto la write del formato a indicatori spenti per vedere
044900060403      * le eventuali decodifiche
045000060403      *?...se l'unica decodifica NON fosse già stata visualizzata?
045100060403      *  ?in fase di prima emissione?
045200060403if  1c***                if        *in90
045300060403     c***                movea     *in           Win
045400060403     c***                movea     *zeros        *in(50)
045500161129     c***                write     TBA06V2
045600060403     c***                movea     Win           *in
045700060403e   1c***                endif
045800060403      *
045900060403if  1***c                   if        *in05
046000161129     ***c                   write     TBA06V2
046100060403     ***c                   exfmt     PROTECT
046200060403x   1***c                   else
046300161129     c                   exfmt     TBA06V2
046400060403e   1***c                   endif
046500060403     c                   setoff                                       28  90
046600060403     c                   clear                   V1Dmsg
046700060403     c                   reset                   wTasto
046800060403      *
046900060403sel 1c                   select
047000060403      * F03=Fine
047100060403w   1c                   when      *inKC
047200060403     c                   exsr      F03V1
047300060403     c***                leavesr
047400060403      * F12=Ritorno
047500060403w   1c                   when      *inKL
047600060403     c                   exsr      F12V2
047700060403     c***                leavesr
047800060403      *
047900060403x   1c                   other
048000060403      * Controllo dati immessi a video
048100060403      * (non si fanno se richisto l'annullamento)
048200060403if  2c                   if        not *inKQ
048300060403     c                   exsr      CtrV2
048400060403e   2c                   endif
048500060403      * Aggiornamento se non ci sono errori
048600060403sel 2c                   select
048700060403      * - Rilevati errori
048800060403w   2c                   when      *in90
048900060403      * - F5=Ripristino
049000060403w   2c                   when      *inKE
049100060403     c                   eval      wTasto  = '05'
049200060403      * - F6=Conferma
049300060403w   2c                   when      *inKF
049400060403     c                   eval      wTasto  = '06'
049500060403      * - F16=Annullamento
049600060403w   2c                   when      *inKQ
049700060403     c                   eval      wTasto  = '16'
049800060403e   2c                   endsl
049900060403     ***c                   exsr      AggTBE
050000060403if  2c                   if        wTasto  > *zeros
050100060403     c                   eval      $InzW1  = *on
050200060403     c                   eval      $TipVid = 'A'
050300060403e   2c                   endif
050400060403e   1c                   endsl
050500060403      *
050600060403     c                   ENDSR
050700060403
050800060403      *---------------------------------------------------------------*
050900060403      * INZV2  - Inizializzazione seconda videata (V2)                *
051000060403      *---------------------------------------------------------------*
051100060403     c     InzV2         BEGSR
051200060403      *
051300161129     c                   clear                   TBA06V2
051400060403      *
051500060403      * reimposto il campo chiave (qui di solo output)
051600060403     c                   movel     V1Cksc        V2Cksc
051700060403     c                   movel     ACOrag        V2Dksc
051800170720     c                   movel     V1Cke2        V2Cke2
051900170720     c                   movel     V1dke2        V2dke2
052000060403      *
052100060403      * Aggancio la tabella, se trovo il codice sono in modifica
052200060403      * o ripristino (se record annullato), altrimenti in immissione
052300161129     c                   clear                   DVPC
052400060403     c                   exsr      ChnTBE
052500060403      *
052600060403sel 1c                   SELECT
052700060403      * IMMISSIONE
052800060403w   1c                   WHEN      NOT %found(TNTBE01L)
052900060403     c                   eval      *in01   = *on
053000060403     c                   eval      T1opz   = $Opz(01)
053100170831     c                   eval      V2CPDL  = 'S'
053200170831     c                   eval      V2CROW  = *blank
053300060403      * MODIFICA
053400060403w   1c                   WHEN          %found(TNTBE01L)
053500060403     c                             and TBEatb  = *blanks
053600060403     c                   eval      *in02   = *on
053700060403     c                   eval      T1opz   = $Opz(02)
053800060403      * RIPRISTINO
053900060403w   1c                   WHEN          %found(TNTBE01L)
054000060403     c                             and TBEatb <> *blanks
054100060403     c                   eval      *in04   = *on
054200060403     c                   eval      T1opz   = $Opz(06)
054300060403e   1c                   ENDSL
054400060403      *
054500060403if  1c                   if        %found(TNTBE01L)
054600161129     c                   movel     TBEuni        DVPC
054700161129     c                   eval      V2Cvta  = §VPCvta
054800170421     c                   eval      V2CUPL  = §VPCUPL
054900170421     c                   eval      V2CCIS  = §VPCCIS
055000170522     c                   eval      V2CCFD  = §VPCCFD
055100170522     c                   eval      V2CTLL  = §VPCTLL
055200170522     c                   eval      V2CIED  = §VPCIED
055300170421     c                   eval      V2CASG  = §VPCASG
055400170529     c                   eval      V2CTIS  = §VPCTIS
055500170421     c                   eval      V2CSGF  = §VPCSGF
055600170421     c                   eval      V2CSGN  = §VPCSGN
055700170421     c                   eval      V2CSGR  = §VPCSGR
055800170831     c                   if        §VPCOFX <> *blank
055900170831     c                   eval      V2COFX = %dec(§VPCOFX:8:0)/1000
056000170831     c                   endif
056100170831     c                   if        §VPCOFY <> *blank
056200170831     c                   eval      V2COFY = %dec(§VPCOFY:8:0)/1000
056300170831     c                   endif
056400170831     c                   eval      V2CPDL  = §VPCPDL
056500170831     c                   eval      V2CROW  = §VPCROW
056600170831     c                   eval      V2CBRD  = §VPCBRD
056700170818     c                   eval      V2CUPLTO = §VPCUPLTO
056800161129e   1c                   endif
056900060403      *
057000060403     c                   ENDSR
057100060403
057200060403      *---------------------------------------------------------------*
057300060403      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
057400060403      *---------------------------------------------------------------*
057500060403     c     F12V2         BEGSR
057600060403      *
057700060403     c                   eval      $TipVid = '1'
057800060403      *
057900060403     c                   ENDSR
058000060403
058100060403      *---------------------------------------------------------------*
058200060403      * CTRV2  - Controllo e decodifica seconda videata               *
058300060403      *---------------------------------------------------------------*
058400060403     c     CtrV2         BEGSR
058500060403      *
058600060403     c                   movea     *zeros        *in(50)
058700170720      *
058800170720      * Controllo che la KE2 sia presente in tab. VPR se non è blank
058900170720     c                   if        V2Cke2 <> *blank
059000170720     c                   clear                   V1Cke2
059100170720     c                   reset                   TIBS02ds
059200170720     c                   movel     KNSIF         T02sif
059300170720     c                   movel     'VPR'         T02cod
059400170720     c                   movel     'C'           T02mod
059500170720     c                   movel     'VPCKE2'      T02ke1
059600170720     c                   movel     V2Cke2        T02ke2
059700170720     c                   call      'TIBS02R'
059800170720     c                   parm                    KPJBA
059900170720     c                   parm                    TIBS02ds
060000170720if  2c                   if        T02err <> *blanks
060100170720     c                   seton                                        285690
060200170720     c                   eval      V1Dmsg  = $Msg(11)
060300170720     c                   eval      V2Dke2  = *all'*'
060400170720     c                   endif
060500170720     c                   reset                   tibs02ds
060600170720     c                   endif
060700170421      *
060800170421      * Spool etichette per spedizione/collo deve essere valorizzato se si generano etichette
060900170421     c                   if            V2CSGF <> *blanks
061000170421     c                             and V2CSGR =  *blanks
061100170421      * lo imposto a S di default
061200170421     c                   eval      V2CSGR = 'S'
061300170421     c                   endif
061400170421      *
061500170421      * Maschera file deve essere valorizzato se si generano etichette
061600170421     c                   if            V2CSGF <> *blanks
061700170421     c                             and V2CSGN =  *blanks
061800170421      * lo imposto a N1 di default
061900170421     c                   eval      V2CSGN = 'N1'
062000170421     c                   endif
062100170831      *
062200170831      * Logo in PDF deve essere valorizzato se si generano etichette
062300170831     c                   if            V2CSGF <> *blanks
062400170831     c                             and V2CPDL =  *blanks
062500170831      * lo imposto a S  di default
062600170831     c                   eval      V2CPDL = 'S'
062700170831     c                   endif
062800170831      *
062900170831      * Riga contrasto deve essere valorizzato se si generano etichette
063000170831     c                   if            V2CSGF <> *blanks
063100170831     c                             and V2CROW =  *blanks
063200170831      * lo imposto a N  di default
063300170831     c                   eval      V2CROW = 'N'
063400170831     c                   endif
063500170421      *
063600170421      * Esiste Upload deve essere valorizzato se si generano etichette
063700170818     c                   if           (V2CSGF   <> *blanks or
063800170818     c                                 V2CUPLTO <> *blanks)
063900170818     c                             and V2CUPL =  *blanks
064000170421      * lo imposto a S di default
064100170421     c                   eval      V2CUPL = 'S'
064200170421     c                   endif
064300060403      *
064400060403sel 1c                   SELECT
064500170421      *
064600170523      * Flag verificare dati... / Controlla indirizzo con Tellus / Forzare instradamento export
064700170523      * NON devono essere valorizzati se NON si calcola instradamento
064800170523w   1c                   WHEN          V2CCIS =  *blanks
064900170523     c                             and (V2CCFD <> *blanks or
065000170523     c                                  V2CTLL <> *blanks or
065100170523     c                                  V2CIED <> *blanks)
065200170523     c                   seton                                        285490
065300170523     c                   eval      V1Dmsg  = $Msg(8)
065400170529      *
065500170529      * Tipo ID spedizione
065600170529      * NON deve essere valorizzato se NON si assegnano segnacolli a spedizione
065700170529w   1c                   WHEN          V2CASG =  *blanks
065800170529     c                             and V2CTIS <> *blanks
065900170529     c                   seton                                        285590
066000170529     c                   eval      V1Dmsg  = $Msg(9)
066100170529      *
066200170529      * Tipo ID spedizione
066300170529      * se si assegnano segnacolli a spedizione, il tipo ID spedizione deve essere valorizzato
066400170529w   1c                   WHEN          V2CASG <> *blanks
066500170529     c                             and V2CTIS =  *blanks
066600170529     c                   seton                                        285590
066700170529     c                   eval      V1Dmsg  = $Msg(10)
066800170523      *
066900170523      * Maschera file etichette segnacollo NON deve essere valorizzato se NON si generano etichette
067000170523w   1c                   WHEN          V2CSGF =  *blanks
067100170523     c                             and V2CSGN <> *blanks
067200170523     c                   seton                                        285290
067300170523     c                   eval      V1Dmsg  = $Msg(6)
067400170421      *
067500170421      * valore errato per Maschera file
067600170421     c                   WHEN          V2CSGF <> *blanks
067700171009      * i valori validi sono: N1, CN, CA, NA
067800170421     c                             and V2CSGN <> 'N1'
067900171009     c                             and V2CSGN <> 'C1'
068000171009     c                             and V2CSGN <> 'CN'
068100171009     c                             and V2CSGN <> 'CA'
068200171021     c                             and V2CSGN <> 'LN'
068300171021     c                             and V2CSGN <> 'LA'
068400171021     c                             and V2CSGN <> 'NA'
068500170421     c                   seton                                        285290
068600170421     c                   eval      V1Dmsg  = $Msg(5)
068700170421      *
068800170421      * Spool etichette per spedizione/collo NON deve essere valorizzato se NONsi generano etichette
068900170421w   1c                   WHEN          V2CSGF =  *blanks
069000170831     c                             and (V2CSGR <> *blanks OR
069100170831     c                                  V2COFX <> *zero   OR
069200170831     c                                  V2COFY <> *zero   OR
069300170831     c                                  V2CPDL <> *blank  OR
069400170831     c                                  V2CROW <> *blank)
069500170421     c                   seton                                        285390
069600170421     c                   eval      V1Dmsg  = $Msg(7)
069700170824      *
069800170824      * Ridirigere dati verso può essere E solo se "assegnare segnacolli a spedizioni" e
069900170824      * "generare etichette segnacollo spedizioni" non valorizzati
070000170824w   1c                   WHEN      V2CUPLTO = 'E' and
070100170824     c                             (V2CASG <> *blanks OR
070200170824     c                              V2CSGF <> *blanks)
070300170824     c                   seton                                        285790
070400170824     c                   eval      V1Dmsg  = $Msg(12)
070500060403      *
070600060403e   1c                   ENDSL
070700170421      *
070800170421      * Esiste Upload deve essere pulito se NON si generano etichette
070900170818     c                   if            V2CSGF   = *blanks and
071000170818     c                                 V2CUPLTO = *blanks
071100170421     c                   eval      V2CUPL = *blank
071200170421     c                   endif
071300060403      *
071400060403     c                   ENDSR
071500060403
071600060403      *---------------------------------------------------------------*
071700060403      * GESW1  - Gestione videata dati relativi alla trasmissione     *
071800060403      *---------------------------------------------------------------*
071900060403     c     GesW1         BEGSR
072000060403      *
072100060403      * Inizializzazione videata
072200060403if  1c                   if        $InzW1 = *on
072300060403     c                   exsr      InzW1
072400060403     c                   movel     *off          $InzW1
072500060403e   1c                   endif
072600060403      *
072700060403if  1***c                   if        *in05
072800161129     ***c                   write     TBA06W1
072900060403     ***c                   exfmt     PROTECT
073000060403x   1***c                   else
073100161129     c                   exfmt     TBA06W1
073200060403e   1***c                   endif
073300060403     c                   eval      *in90 = *off
073400060403     c                   clear                   W1MSG
073500060403      *
073600060403sel 1c                   select
073700060403      * F12=Ritorno
073800060403w   1c                   when      *inKL
073900060403     c                   exsr      F12W1
074000060403     c                   goto      EndGesW1
074100060403e   1c                   endsl
074200060403      *
074300060403      * Controllo dati immessi a video
074400060403     c                   exsr      CtrW1
074500060403      *
074600060403      * Aggiornamento se non ci sono errori
074700060403if  1c                   if        not *in90 and *inKF
074800060403     c                   exsr      AggTBE
074900060403e   1c                   endif
075000060403      *
075100060403     c     EndGesW1      ENDSR
075200060403
075300060403      *---------------------------------------------------------------*
075400060403      * INZW1  - Inzializzazione window (W1)                          *
075500060403      *---------------------------------------------------------------*
075600060403     c     InzW1         BEGSR
075700060403      *
075800060403     c                   movea     *zeros        *in(50)
075900060403      *
076000060403sel 1c                   select
076100060403      *
076200060403      * F5=Ripristino
076300060403w   1c                   when      *inKE   and  *in04
076400060403     c                   eval      W1FTT = TBEftt
076500060403      *
076600060403      * F6=Conferma
076700060403w   1c                   when      *inKF
076800060403sel 2c                   select
076900060403      *   Immissione
077000060403w   2c                   when      *in01
077100060403     c                   eval      W1FTT = TBXftt
077200060403      *   Modifica / Ripristino
077300060403w   2c                   when      *in02   or    *in04
077400060403     c                   eval      W1FTT = TBEftt
077500060403e   2c                   endsl
077600060403      *
077700060403      * F16=Annullamento
077800060403w   1c                   when      *inKQ   and  not *in04
077900060403     c                   eval      W1FTT = TBEftt
078000060403      *
078100060403e   1c                   endsl
078200060403      *
078300060403      * Se NON immissione: visualizzo i dati relativi all'ultima
078400060403      *   trasmissione
078500060403if  1c                   if        not *in01
078600060403     c                   eval      W1FLT = TBEflt
078700060403     c                   eval      W1FTR = TBEftr
078800060403if  2c                   if        TBEdtr <> 0
078900060403     c                   clear                   WLBDAT
079000060403     c                   z-add     TBEdtr        G02inv
079100060403     c                   movel     '3'           G02err
079200060403     c                   call      'XSRDA8'
079300060403     c                   parm                    WLBDAT
079400060403     c                   z-add     G02dat        W1DTR
079500060403e   2c                   endif
079600060403e   1c                   endif
079700060403      *
079800060403     c                   ENDSR
079900060403
080000060403      *---------------------------------------------------------------*
080100060403      * CTRW1  - Controllo e decodifica window                        *
080200060403      *---------------------------------------------------------------*
080300060403     c     CtrW1         BEGSR
080400060403      *
080500060403     c                   movea     *zeros        *in(50)
080600060403      *
080700060403     c     EndCtrW1      ENDSR
080800060403
080900060403      *---------------------------------------------------------------*
081000060403      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
081100060403      *---------------------------------------------------------------*
081200060403     c     F12W1         BEGSR
081300060403      *
081400060403     c                   eval      $TipVid = '2'
081500060403      *
081600060403     c                   ENDSR
081700060403
081800060403      *---------------------------------------------------------------*
081900060403      * CHNTBE * Aggancio tabella                                     *
082000060403      *---------------------------------------------------------------*
082100060403     c     ChnTBE        BEGSR
082200060403      *
082300060403     c                   movel     T02cod        TBEcod
082400060403     c                   movel(p)  V2Cksc        TBEke1
082500170720     c                   movel(p)  V2Cke2        TBEke2
082600060403     c                   clear                   TBElin
082700060403     c                   movel     KNSIF         TBEsif
082800060403     c     K05TBE01      chain     TNTBE01L
082900060403      * Se non ho trovato il record con il sistema informativo
083000060403      * che ho in linea, lo abblenco
083100060403if  1c                   if        not %found(TNTBE01L)
083200060403     c                   clear                   TBEsif
083300060403     c     K05TBE01      chain     TNTBE01L
083400060403e   1c                   endif
083500060403      *
083600060403     c                   ENDSR
083700060403
083800060403      *---------------------------------------------------------------*
083900060403      * AGGTBE * Aggiornamento tabella                                *
084000060403      *---------------------------------------------------------------*
084100060403     c     AggTBE        BEGSR
084200060403      *
084300060403sel 1c                   SELECT
084400060403      *
084500060403      * F5=Ripristino
084600060403w   1c                   WHEN      wTasto='05'  and  *in04
084700060403     c                   clear                   TBEatb
084800060525     c                   movel     W1ftt         TBEftt
084900060403     c                   clear                   TBEftr
085000060403     c                   UPDATE    TNTBE000
085100060403      *
085200060403      * F6=Conferma
085300060403w   1c                   WHEN      wTasto='06'
085400060403     c                   exsr      RieTBE
085500060525     c*** già in RieTBE: movel     W1ftt         TBEftt
085600060403sel 2c                   select
085700060403      *   Immissione
085800060403w   2c                   when      *in01
085900060403     c                   clear                   TBEflt
086000060403     c                   clear                   TBEdtr
086100060403     c                   WRITE     TNTBE000                             22
086200060403      *   Modifica / Ripristino
086300060403w   2c                   when      *in02   or    *in04
086400060403     c                   UPDATE    TNTBE000
086500060403e   2c                   endsl
086600060403      *
086700060403      * F16=Annullamento
086800060403w   1c                   WHEN      wTasto='16'  and  not *in04
086900060403     c                   movel     'A'           TBEatb
087000060525     c                   movel     W1ftt         TBEftt
087100060403     c                   clear                   TBEftr
087200060403     c                   UPDATE    TNTBE000
087300060403      *
087400060403e   1c                   ENDSL
087500060403      *
087600060403      * Torno alla prima videata che carico come da inizio pgm
087700060403     c                   movel     '1'           $TipVid
087800060403     c                   movel     *on           $InzV1
087900060403     c                   movel     *on           $InzV2
088000060403     c                   movel     *on           $InzW1
088100060403     c
088200060403     c                   ENDSR
088300060403
088400060403      *---------------------------------------------------------------*
088500060403      * RIETBE * Riempimento dati tabella                             *
088600060403      *---------------------------------------------------------------*
088700060403     c     RieTBE        BEGSR
088800060403      *
088900060403     c                   clear                   TBEatb
089000060403     c                   if        TBXsif <> *blanks
089100060403     c                   movel     KNSIF         TBEsif
089200060403     c                   else
089300060403     c                   clear                   TBEsif
089400060403     c                   endif
089500060403     c                   movel     TBXapl        TBEapl
089600060403     c                   movel     T02cod        TBEcod
089700060403     c                   movel     V2Cksc        TBEke1
089800170720     c                   movel     V2Cke2        TBEke2
089900060403      *
090000161129     c                   clear                   DVPC
090100161129     c                   eval      §VPCvta = V2Cvta
090200170421     c                   eval      §VPCUPL = V2CUPL
090300170421     c                   eval      §VPCCIS = V2CCIS
090400170522     c                   eval      §VPCCFD = V2CCFD
090500170522     c                   eval      §VPCTLL = V2CTLL
090600170522     c                   eval      §VPCIED = V2CIED
090700170421     c                   eval      §VPCASG = V2CASG
090800170529     c                   eval      §VPCTIS = V2CTIS
090900170421     c                   eval      §VPCSGF = V2CSGF
091000170421     c                   eval      §VPCSGN = V2CSGN
091100170421     c                   eval      §VPCSGR = V2CSGR
091200170831     c                   if        V2COFX <> *zero
091300170831     c                   eval      §VPCOFX = %editc(V2COFX:'X')
091400170831     c                   endif
091500170831     c                   if        V2COFY <> *zero
091600170831     c                   eval      §VPCOFY = %editc(V2COFY:'X')
091700170831     c                   endif
091800170831     c                   eval      §VPCPDL = V2CPDL
091900170831     c                   eval      §VPCROW = V2CROW
092000170831     c                   if        V2CSGR <> *blank
092100170831     c                   if        V2CBRD <> *blank
092200170831     c                   eval      §VPCBRD = V2CBRD
092300170831     c                   else
092400170831     c                   eval      §VPCBRD = 'N'
092500170831     c                   endif
092600170831     c                   endif
092700170818     c                   eval      §VPCUPLTO = V2CUPLTO
092800161129     c                   movel(p)  DVPC          TBEuni
092900060403      *
093000060525     c                   movel     W1ftt         TBEftt
093100060403     c                   clear                   TBEflt
093200060403     c                   clear                   TBEftr
093300060403     c                   clear                   TBEdtr
093400060403      *
093500060403     c                   ENDSR
093600060403
093700060403** - $Opz ----*
093800060403  Inserimento
093900060403    Modifica
094000060403     Copia
094100060403  Annullamento
094200060403Visualizzazione
094300060403   ANNULLATO
094400060403** - $Msg -------------------------------------------------------------------*
094500060403Immettere il codice cliente                                                    01
094600060403Codice cliente errato                                                          02
09470017042103                                                                             03
094800170421Se si generano le etichette, va indicata la maschera file                      04
094900170421Maschera file non valida                                                       05
095000170421Se NON si generano le etichette, NON va indicata la maschera file              06
095100170831Se NON si generano le etichette, NON vanno indicati i parametri sottesi        07
095200170523Se NON si calcola l'instradamento, NON vanno indicati i flag legati            08
095300170529Se NON si assegnano segnacolli a spedizione, NON va indicato il tipo ID sped.  09
095400170529Se si assegnano segnacolli a spedizione, va indicato il tipo ID spedizione     10
095500170720Codice opzione non presente in tab. VPR                                        11
095600170824Valore E=EasyWeb valido solo se NON assegno segnacolli e NON genero etichette  12
