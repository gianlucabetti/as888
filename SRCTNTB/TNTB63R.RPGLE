000100060403      *---------------------------------------------------------------*
000200080212      * Gestione tabella "LLT" = Limite liquidazione transattiva      *
000300060403      *---------------------------------------------------------------*
000400110811
000500110811      *?  ATTENZIONE!!  ?
000600110811      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000700110811      *?    'Interrogazione abilitazioni clienti'              ?
000800110811      *?    In caso di aggiunta/modifica campi alla tabella    ?
000900110811      *?    verificare se sono validi per la visualizzazione   ?
001000110811      *?    da TA61                                            ?
001100060403
001200080228     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001300060403
001400060403      *---------------------------------------------------------------*
001500060403      *   A R C H I V I                                               *
001600060403      *---------------------------------------------------------------*
001700060403      *
001800060403     fTNTBE01L  uf a e           k disk
001900080226     FTNTBE02L  IF   E           K DISK    RENAME(tntbe000:tntbe002)
002000060403      *
002100080226     fTNTB63D   cf   e             workstn sfile(tb63sf:nrr)
002200060403
002300060403      *---------------------------------------------------------------*
002400060403      *   C O S T A N T I                                             *
002500060403      *---------------------------------------------------------------*
002600060403      *
002700060403     d DigitN          c                   const('0123456789')
002800060403
002900060403      *---------------------------------------------------------------*
003000060403      *   S C H I E R E                                               *
003100060403      *---------------------------------------------------------------*
003200060403      *
003300060403     d $Opz            s             15    dim( 6) ctdata perrcd(1)             Decodifica OPZ
003400071207     d $Msg            s             78    dim( 7) ctdata perrcd(1)             Messaggi video
003500060403
003600060403      *---------------------------------------------------------------*
003700060403      *   S T R U T T U R E   D A T I                                 *
003800060403      *---------------------------------------------------------------*
003900060403      *
004000060403      * Parametri
004100060403     d KPJBA         e ds
004200060403      *
004300060403      * Passaggio Parametri al pgm TIBS02R
004400060403     d TIBS02ds      e ds                  inz
004500060403      *
004600080212      * Tabella LLT = Limite liquidazione transattiva personalizzata per cliente
004700080212     d dLLT          e ds                  inz
004800080228      * Tabella STD = Parametri standard recupero password
004900080228     d dSTD          e ds                  inz
005000071207
005100060403      *
005200060403      * Tracciato record file TNTBE00F
005300060403     d TNTBEds       e ds                  extname(TNTBE00F) inz
005400060403     d xTNTBEds      e ds                  extname(TNTBE00F) inz
005500060403     d                                     prefix(TBX:3)
005600060403      *
005700060403     d TIBS34ds      e ds                  inz
005800060403     d dDatiUte      e ds                  inz
005900060403     d AZUTEds       e ds                  extname(AZUTE00F) inz
006000060403      *
006100060403      * Decodifica anagrafica clienti
006200060403     d TIBS69ds      e ds                  inz
006300060403     d ds_CNACO      e ds                  extname(CNACO00F) inz
006400060403     d ds_CNIND      e ds                  extname(CNIND00F) inz
006500060403     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
006600060403     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
006700060403      *
006800060403     d WLBdat          ds                  inz
006900060403     d  G02dat                 1      8  0 inz
007000060403     d  G02inv                 9     16  0 inz
007100060403     d  G02err                17     17    inz('3')
007200060403     d  G02tgi                18     22  0 inz
007300060403      *
007400060403     d Status         sds
007500060403     d  VTCpgm           *proc
007600060403
007700060403      *---------------------------------------------------------------*
007800060403      *   V A R I A B I L I                                           *
007900060403      *---------------------------------------------------------------*
008000060403      *
008100060403     d $Fine           s              1a   inz(*off)
008200060403     d $InzV1          s              1a   inz(*on)
008300060403     d $InzV2          s              1a   inz(*off)
008400080228     d $TipVid         s              1a   inz('P')
008500060403     d wTasto          s              2a   inz(*zeros)
008600060403     d wDate           s               d   datfmt(*iso) inz(*loval)
008700080212     d w_forz          s              1a
008800080226     d Kcod            S                   LIKE(TBEcod)   inz('LLT')
008900080226     d Klin            S                   LIKE(TBElin)
009000080226     d Ksif            S                   LIKE(TBEsif)
009100060403
009200060403      *---------------------------------------------------------------*
009300060403      *   M A I N   L I N E                                           *
009400060403      *---------------------------------------------------------------*
009500060403      *  Riepilogo indicatori utilizzati:                             *
009600060403      *  --------------------------------                             *
009700060403      *  01 - Record inesistente (inserimento)                        *
009800060403      *  02 - Record esistente   (modifica)                           *
009900060403      *  04 - Record annullato   (ripristino)                         *
010000060403      *  10 - Comodo                                                  *
010100060403      *  22 - Errori in scrittura record (WRITE)                      *
010200060403      *  28 - Emissione messaggio di errore a video                   *
010300060403      *  50 - ERR: Codice cliente                                     *
010400080212      *  51 - ERR: Importo Limite Liquidazione Transattiva            *
010500060403      *  90 - Riemissione videata                                     *
010600060403      *---------------------------------------------------------------*
010700060403      *
010800060403      * Operazioni iniziali
010900060403     c                   exsr      RutInz
011000060403      *
011100060403      * Gestione video
011200060403     c                   dow       $Fine   = *off
011300080228     c     $TipVid       caseq     'P'           GesVP
011400060403     c     $TipVid       caseq     '1'           GesV1
011500060403     c     $TipVid       caseq     '2'           GesV2
011600060403     c                   endcs
011700060403     c                   enddo
011800060403      *
011900060403      * Fine
012000060403      *
012100080228     c                   clear                   TIBS02ds
012200080228     c                   movel     'C'           T02tla
012300080228     c                   call      'TIBS02R'
012400080228     c                   parm                    KPJBA
012500080228     c                   parm                    TIBS02ds
012600080228      *
012700060403     c                   movel     *on           *inLR
012800060403
012900060403      *---------------------------------------------------------------*
013000060403      * RutInz - Operazioni Iniziali                                  *
013100060403      *---------------------------------------------------------------*
013200060403     c     RutInz        BEGSR
013300060403      *
013400060403      * Ricezione parametri
013500060403     c     *entry        plist
013600060403     c                   parm                    KPJBA
013700060403      *
013800060403      * Definizioni chiavi di accesso
013900060403     c     K05TBE01      klist                                                  *tntbe01l
014000060403     c                   kfld                    TBEcod                         -tabella
014100060403     c                   kfld                    TBEke1                         -chiave uno
014200060403     c                   kfld                    TBEke2                         -chiave due
014300060403     c                   kfld                    TBElin                         -lingua
014400060403     c                   kfld                    TBEsif                         -s.informativo
014500080226      *
014600080226     c     KTBE2         klist
014700080226     C                   KFLD                    Kcod
014800080226     C                   KFLD                    Klin
014900080226     C                   KFLD                    Ksif
015000080226      *
015100071207     C*
015200060403      *
015300060403      * Reperisco la data odierna
015400060403     c                   movel     *date         Wdate
015500060403      *
015600060403      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
015700060403     c     *dtaara       define    §azute        AZUTEds
015800060403     c     *dtaara       define    §datiute      dDatiUte
015900060403      *
016000060403     c                   clear                   AZUTEds
016100060403     c                   clear                   dDatiUte
016200060403     c                   clear                   TIBS34ds
016300060403     c                   in(E)     *dtaara
016400060403if  1c                   if        %Error  or  RSUT = *blanks
016500060403     c                   call      'TIBS34R'
016600060403     c                   parm                    TIBS34ds
016700060403     c                   in        *dtaara
016800060403e   1c                   endif
016900060403      *-- Verifica errori e autorità profilo
017000060403sel 1c                   SELECT
017100060403      *-- controllo se ho errori nei dati utente
017200060403      *--   nel qual caso NON risulta un profilo abilitato
017300060403w   1c                   WHEN      DUTerr  = 'E'
017400060403     c                   eval      $Fine   = *on
017500080212      *
017600080212e   1c                   ENDSL
017700080212      *
017800060403      * Aggancio dati generali della tabella in esame
017900060403     c                   clear                   TBEcod
018000060403     c                   move      *zeros        TBEke1
018100080228     c                   move      'LLT'         TBEke1
018200060403     c                   clear                   TBEke2
018300060403     c                   clear                   TBElin
018400060403     c                   movel     KNSIF         TBEsif
018500060403     c     K05TBE01      chain     TNTBE01L
018600060403     c                   if        not %found(TNTBE01L)
018700060403     c                   clear                   TBEsif
018800060403     c     K05TBE01      chain     TNTBE01L
018900060403     c                   endif
019000060403     c                   if        %found(TNTBE01L)
019100060403     c                   movel     TNTBEds       xTNTBEds
019200060403     c                   else
019300060403     c                   clear                   xTNTBEds
019400060403     c                   endif
019500060403      *
019600080212     c                   clear                   TB63V1
019700080228      *
019800080228      * Aggancio tabella std per recupero parola d'ordine se non sono utente EDP
019900080228     c                   if        %subst(KNMUS:1:3) = 'EDP'
020000080228     c                   eval      $tipvid = '1'
020100080228     c                   endif
020200060403      *
020300060403     c                   ENDSR
020400060403
020500080228      *---------------------------------------------------------------*
020600080228      * GESVP  - Gestione videata richiesta  password                 *
020700080228      *---------------------------------------------------------------*
020800080228     c     GesVP         BEGSR
020900080228      *
021000080228      * Recupero la tabella STD con parola d'ordine
021100080228      ***
021200080228      * leggo tabella standards danni
021300080228      ***
021400080228     c                   clear                   TIBS02DS
021500080228     C                   MOVEL     'C'           t02mod
021600080228     C                   MOVEL     knsif         t02sif
021700080228     C                   MOVEL     'STD'         t02cod
021800080228     C                   MOVEL(P)  1             t02ke1
021900080228     C                   MOVEL(P)  'EUR'         t02ke2
022000080228      *
022100080228     C                   CALL      'TIBS02R'
022200080228     C                   PARM                    KPJBA
022300080228     C                   PARM                    TIBS02DS
022400080228      *
022500080228     C                   if        t02err = *BLANKS
022600080228     C                   MOVEL     T02UNI        DSTD
022700080228     C                   endif
022800080228      *
022900080228      * Scrivo la testata
023000080228     c                   write     TB63T1
023100080228      *
023200080228      *
023300080228     c                   exfmt     TB63VP
023400080228     c                   setoff                                       28  90
023500080228     c                   clear                   V1Dmsg
023600080228      *
023700080228sel 1c                   select
023800080228      * F3=Fine
023900080228w   1c                   when      *inKC
024000080228     c                   eval      $fine = *on
024100080228     c                   leavesr
024200080228      *
024300080228      * Controllo dati immessi a video
024400080228x   1c                   other
024500080228     c                   if        vPword <> §STDPWLT
024600080228     c                   seton                                        2890
024700080228     c                   eval      V1Dmsg  = $Msg(4)
024800080228     c                   leavesr
024900080228e   1c                   endif
025000080228      * - Passaggio alla videata di dettaglio
025100080228     c                   eval      $TipVid = '1'
025200080228      *
025300080228e   1c                   endsl
025400080228      *
025500080228     c                   ENDSR
025600080228
025700060403      *---------------------------------------------------------------*
025800060403      * GESV1  - Gestione videata selezione codice tabella            *
025900060403      *---------------------------------------------------------------*
026000060403     c     GesV1         BEGSR
026100060403      *
026200060403      * Inizializzazione videata
026300060403if  1c                   if        $InzV1  = *on
026400060403     c                   exsr      InzV1
026500060403     c                   movel     *off          $InzV1
026600060403e   1c                   endif
026700060403      *
026800060403      * Scrivo la testata
026900080212     c                   write     TB63T1
027000060403      *
027100060403      *
027200080212     c                   exfmt     TB63V1
027300060403     c                   setoff                                       28  90
027400060403     c                   clear                   V1Dmsg
027500060403      *
027600060403sel 1c                   select
027700060403      * F3=Fine
027800060403w   1c                   when      *inKC
027900060403     c                   exsr      F03V1
028000060403     c                   leavesr
028100060403      *
028200060403      * Controllo dati immessi a video
028300060403x   1c                   other
028400060403     c                   exsr      CtrV1
028500060403      * - Rilevati Errori
028600060403if  2c                   if        *in90
028700060403     c                   leavesr
028800060403e   2c                   endif
028900060403      * - Passaggio alla videata di dettaglio
029000060403     c                   eval      $InzV2  = *on
029100060403     c                   eval      $TipVid = '2'
029200060403      *
029300060403e   1c                   endsl
029400060403      *
029500060403     c                   ENDSR
029600060403
029700060403      *---------------------------------------------------------------*
029800060403      * INZV1  - Inizializzazione prima videata (V1)                  *
029900060403      *---------------------------------------------------------------*
030000060403     c     InzV1         BEGSR
030100060403      *
030200080212     c                   clear                   TB63V1
030300080212     c                   clear                   W_forz
030400060403      *
030500060403      * Imposto il campo "codice cliente" per l'interrogazione
030600060403     c                   move      '?'           V1Cksc
030700060403      *
030800060403     c                   ENDSR
030900060403
031000060403      *---------------------------------------------------------------*
031100060403      * CTRV1  - Controllo e decodifica prima videata                 *
031200060403      *---------------------------------------------------------------*
031300060403     c     CtrV1         BEGSR
031400060403      *
031500060403     c                   movea     *zeros        *in(50)
031600060403      *
031700060403      * Codice cliente
031800060403      * - Ricerca:
031900060403     c     '?'           scan      V1Cksc                                 10
032000080228     c                   if        *in10
032100080226     c                   exsr      carica
032200080226      * gestione subfile
032300080226     c                   exsr      gessbf
032400080227     c   kc              eval      $Fine   = *on
032500080227     c   kc              leavesr
032600080228     c*  kl              leavesr
032700080228
032800080228     c                   endif
032900080226
033000060403      * - Controllo:
033100060403      * - - effettiva immissione
033200060403if  1c                   if           V1Cksc = *blanks
033300060403     c                             or V1Cksc = *zeros
033400060403     c                   seton                                        285090
033500060403     c                   eval      V1Dmsg  = $Msg(1)
033600060403     c                   leavesr
033700060403e   1c                   endif
033800060403      * - - numericità
033900060403     c     DigitN        check     V1Cksc                                 10
034000060403if  1c                   if        *in10   =  *on
034100060403     c                   seton                                        285090
034200060403     c                   eval      V1Dmsg  = $Msg(2)
034300060403     c                   leavesr
034400060403e   1c                   endif
034500060403      * - - decodifica
034600060403     c                   exsr      DecodCLI
034700080212      *
034800060403     c                   ENDSR
034900080226
035000080226      *---------------------------------------------------------------*
035100080226      * Gessbf     * Gestione Sub file                                *
035200080226      *---------------------------------------------------------------*
035300080226     c     Gessbf        BEGSR
035400080226
035500080226      * emissione della videata
035600080226     c                   write     TB63t1
035700080228     c                   write     tb63zsf
035800080226     c                   do        *hival
035900080227     c                   EXFMT     TB63ct
036000080227     c                   clear                   V1CMSG
036100080227     c                   setoff                                       9028
036200080227      *
036300080227      * F3=FINE
036400080227     c   KC              leave
036500080227      *
036600080227      * F12=FINE
036700080227     c   KL              leave
036800080227      *
036900080227     c                   do        *hival
037000080228     c                   readc     tb63sf                                 30
037100080228     c                   if        *in30
037200080227     c                   leave
037300080227     c                   endif
037400080227      * verifico se scelto un cliente
037500080228     c                   if        vscopz = '1'
037600080227     c                   move      vscksc        v1cksc
037700080228     c                   leavesr
037800080227     c                   endif
037900080226      *
038000080227     c                   enddo
038100080227      *
038200080227     c                   enddo
038300080226      *
038400080226     c                   ENDSR
038500060403
038600060403      *---------------------------------------------------------------*
038700060403      * DecodCLI   * Decodifica codice cliente                        *
038800060403      *---------------------------------------------------------------*
038900060403     c     DecodCLI      BEGSR
039000060403      *
039100060403     c                   clear                   TIBS69ds
039200060403     c                   clear                   ds_CNACO
039300060403     c                   clear                   ds_CNIND
039400060403     c                   clear                   ds_CNCLP
039500060403     c                   clear                   ds_FNCLS
039600060403     c                   move      V1Cksc        I69kac
039700060403     c                   call      'TIBS69R'
039800060403     c                   parm                    TIBS69ds
039900060403     c                   parm                    ds_CNACO
040000060403     c                   parm                    ds_CNIND
040100060403     c                   parm                    ds_CNCLP
040200060403     c                   parm                    ds_FNCLS
040300060403      *
040400060403     c                   if        O69err  = *blanks
040500060403     c                   movel     ACOrag        V2Dksc
040600060403     c                   else
040700060403     c                   seton                                        285090
040800060403     c                   eval      V1Dmsg  = $Msg(2)
040900060403     C                   endif
041000060403      *
041100060403     c                   ENDSR
041200060403
041300060403      *---------------------------------------------------------------*
041400060403      * F03V1  - Tasto funzionale F03 -> Fine programma               *
041500060403      *---------------------------------------------------------------*
041600060403     c     F03V1         BEGSR
041700060403      *
041800060403     c                   movel     *on           $Fine                          fine pgm
041900060403      *
042000060403     c                   ENDSR
042100060403
042200060403      *---------------------------------------------------------------*
042300060403      * GESV2  - Gestione videata dettaglio dati                      *
042400060403      *---------------------------------------------------------------*
042500060403     c     GesV2         BEGSR
042600060403      *
042700060403      * Inizializzazione videata
042800060403     c                   if        $InzV2  = *on
042900060403     c                   exsr      InzV2
043000060403     c                   move      *off          $InzV2
043100060403     c                   endif
043200060403      *
043300060403      * Scrivo la testata
043400080212     c                   write     TB63T1
043500060403      *
043600080212     c                   exfmt     TB63V2
043700080212      *
043800060403     c                   setoff                                       28  90
043900060403     c                   clear                   V1Dmsg
044000060403     c                   reset                   wTasto
044100060403      *
044200060403sel 1c                   select
044300060403      * F03=Fine
044400060403w   1c                   when      *inKC
044500060403     c                   exsr      F03V1
044600060403      * F12=Ritorno
044700060403w   1c                   when      *inKL
044800060403     c                   exsr      F12V2
044900060403      *
045000060403x   1c                   other
045100060403      * Controllo dati immessi a video
045200060403      * (non si fanno se richisto l'annullamento)
045300060403if  2c                   if        not *inKQ
045400060403     c                   exsr      CtrV2
045500060403e   2c                   endif
045600060403      * Aggiornamento se non ci sono errori
045700060403sel 2c                   select
045800060403      * - Rilevati errori
045900060403w   2c                   when      *in90
046000060403      * - F5=Ripristino
046100060403w   2c                   when      *inKE
046200060403     c                   eval      wTasto  = '05'
046300060403      * - F6=Conferma
046400060403w   2c                   when      *inKF
046500060403     c                   eval      wTasto  = '06'
046600060403      * - F16=Annullamento
046700060403w   2c                   when      *inKQ
046800060403     c                   eval      wTasto  = '16'
046900060403e   2c                   endsl
047000080212      *
047100080215      *
047200080228if  2c                   if        wTasto  <> '00'
047300080215     C                   EXSR      AGGTBE
047400080215     c                   eval      $TipVid = '1'
047500080215e   2c                   endif
047600060403e   1c                   endsl
047700060403      *
047800060403     c                   ENDSR
047900060403
048000060403      *---------------------------------------------------------------*
048100060403      * INZV2  - Inizializzazione seconda videata (V2)                *
048200060403      *---------------------------------------------------------------*
048300060403     c     InzV2         BEGSR
048400060403      *
048500080212     c                   clear                   TB63V2
048600060403      *
048700060403      * reimposto il campo chiave (qui di solo output)
048800060403     c                   movel     V1Cksc        V2Cksc
048900060403     c                   movel     ACOrag        V2Dksc
049000060403      *
049100060403      * Aggancio la tabella, se trovo il codice sono in modifica
049200060403      * o ripristino (se record annullato), altrimenti in immissione
049300080212     c                   clear                   dLLT
049400060403     c                   exsr      ChnTBE
049500060403      *
049600060403sel 1c                   SELECT
049700060403      * IMMISSIONE
049800060403w   1c                   WHEN      NOT %found(TNTBE01L)
049900060403     c                   eval      *in01   = *on
050000060403     c                   eval      T1opz   = $Opz(01)
050100060403      * MODIFICA
050200060403w   1c                   WHEN          %found(TNTBE01L)
050300060403     c                             and TBEatb  = *blanks
050400060403     c                   eval      *in02   = *on
050500060403     c                   eval      T1opz   = $Opz(02)
050600060403      * RIPRISTINO
050700060403w   1c                   WHEN          %found(TNTBE01L)
050800060403     c                             and TBEatb <> *blanks
050900060403     c                   eval      *in04   = *on
051000060403     c                   eval      T1opz   = $Opz(06)
051100060403e   1c                   ENDSL
051200060403      *
051300060403if  1c                   if        %found(TNTBE01L)
051400080212     c                   movel     TBEuni        dLLT
051500080212     c                   eval      V2CIMP  = §LLTimp
051600060403e   1c                   endif
051700060403      *
051800060403     c                   ENDSR
051900060403
052000060403      *---------------------------------------------------------------*
052100060403      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
052200060403      *---------------------------------------------------------------*
052300060403     c     F12V2         BEGSR
052400060403      *
052500060403     c                   eval      $TipVid = '1'
052600060403      *
052700060403     c                   ENDSR
052800060403
052900060403      *---------------------------------------------------------------*
053000060403      * CTRV2  - Controllo e decodifica seconda videata               *
053100060403      *---------------------------------------------------------------*
053200060403     c     CtrV2         BEGSR
053300060403      *
053400060403     c                   movea     *zeros        *in(50)
053500060403      *
053600060403sel 1c                   SELECT
053700060403      *
053800080212      * Importo limite liquidazione transattiva
053900080212w   1c                   WHEN      V2Cimp  = *zeros and W_forz = ' '
054000080212     c                   eval      w_forz ='S'
054100060403     c                   seton                                        285190
054200060403     c                   eval      V1Dmsg  = $Msg(3)
054300060403      *
054400060403e   1c                   ENDSL
054500060403      *
054600060403     c                   ENDSR
054700060403
054800060403      *---------------------------------------------------------------*
054900060403      * CHNTBE * Aggancio tabella                                     *
055000060403      *---------------------------------------------------------------*
055100060403     c     ChnTBE        BEGSR
055200060403      *
055300080228     c                   movel     'LLT'         TBEcod
055400080212     c                   move(p)   V2Cksc        TBEke1
055500060403     c                   clear                   TBEke2
055600060403     c                   clear                   TBElin
055700060403     c                   movel     KNSIF         TBEsif
055800060403     c     K05TBE01      chain     TNTBE01L
055900060403      * Se non ho trovato il record con il sistema informativo
056000060403      * che ho in linea, lo abblenco
056100060403if  1c                   if        not %found(TNTBE01L)
056200060403     c                   clear                   TBEsif
056300060403     c     K05TBE01      chain     TNTBE01L
056400060403e   1c                   endif
056500060403      *
056600060403     c                   ENDSR
056700060403
056800060403      *---------------------------------------------------------------*
056900060403      * AGGTBE * Aggiornamento tabella                                *
057000060403      *---------------------------------------------------------------*
057100060403     c     AggTBE        BEGSR
057200060403      *
057300060403sel 1c                   SELECT
057400060403      *
057500060403      * F5=Ripristino
057600060403w   1c                   WHEN      wTasto='05'  and  *in04
057700060403     c                   clear                   TBEatb
057800080215     c                   movel     'S'           TBEftt
057900060403     c                   clear                   TBEftr
058000060403     c                   UPDATE    TNTBE000
058100060403      *
058200060403      * F6=Conferma
058300060403w   1c                   WHEN      wTasto='06'
058400060403     c                   exsr      RieTBE
058500060403sel 2c                   select
058600060403      *   Immissione
058700060403w   2c                   when      *in01
058800060403     c                   clear                   TBEflt
058900060403     c                   clear                   TBEdtr
059000060403     c                   WRITE     TNTBE000                             22
059100060403      *   Modifica / Ripristino
059200060403w   2c                   when      *in02   or    *in04
059300060403     c                   UPDATE    TNTBE000
059400060403e   2c                   endsl
059500060403      *
059600060403      * F16=Annullamento
059700060403w   1c                   WHEN      wTasto='16'  and  not *in04
059800060403     c                   movel     'A'           TBEatb
059900080215     c                   movel     'S'           TBEftt
060000060403     c                   clear                   TBEftr
060100060403     c                   UPDATE    TNTBE000
060200060403      *
060300060403e   1c                   ENDSL
060400060403      *
060500060403      * Torno alla prima videata che carico come da inizio pgm
060600060403     c                   movel     '1'           $TipVid
060700060403     c                   movel     *on           $InzV1
060800060403     c                   movel     *on           $InzV2
060900060403     c
061000060403     c                   ENDSR
061100060403
061200060403      *---------------------------------------------------------------*
061300060403      * RIETBE * Riempimento dati tabella                             *
061400060403      *---------------------------------------------------------------*
061500060403     c     RieTBE        BEGSR
061600060403      *
061700060403     c                   clear                   TBEatb
061800060403     c                   if        TBXsif <> *blanks
061900060403     c                   movel     KNSIF         TBEsif
062000060403     c                   else
062100060403     c                   clear                   TBEsif
062200060403     c                   endif
062300060403     c                   movel     TBXapl        TBEapl
062400080228     c                   movel     'LLT'         TBEcod
062500080212     c                   move      V2Cksc        TBEke1
062600060403     c                   clear                   TBEke2
062700060403      *
062800080212     c                   clear                   dLLT
062900080212     c                   eval      §LLTimp = V2Cimp
063000080212     c                   movel(p)  dLLT          TBEuni
063100060403      *
063200080215     c                   movel     'S'           TBEftt
063300060403     c                   clear                   TBEflt
063400060403     c                   clear                   TBEftr
063500060403     c                   clear                   TBEdtr
063600060403      *
063700060403     c                   ENDSR
063800080226      *****************************************************************
063900080226      *   carico tutti gli elementi della tabella
064000080226      *****************************************************************
064100080226     c     CARICA        BEGSR
064200080226     c                   clear                   NRR               4 0
064300080226     c
064400080226     c                   eval      *in35='1'
064500080226     c                   write     TB63ct
064600080226     c                   eval      *in35='0'
064700080226     c
064800080226     c     ktbe2         chain     tntbe02l                           30
064900080226     c                   dow       not *in30
065000080226     c                   if        tbeatb=' '
065100080228     c                   setoff                                       48
065200080228     c                   else
065300080228     c                   seton                                        48
065400080228     c                   endif
065500080226      *
065600080228     c                   move      tbeke1        vscksc
065700080226     c
065800080226     C                   clear                   TIBS69DS
065900080226     c                   z-add     vscksc        I69kac
066000080226      *
066100080226     C                   CALL      'TIBS69R'
066200080226     C                   PARM                    tibs69DS
066300080226     C                   PARM                    DS_cnaco
066400080226     C                   PARM                    DS_cnind
066500080226     C                   PARM                    DS_cnclp
066600080226     C                   PARM                    DS_fncls
066700080226    2c                   if        o69err=' '
066800080226     c                   movel     acorag        vsdksc
066900080226    2c                   endif
067000080226      * recupero l'importo
067100080226     c                   movel     tbeuni        dllt
067200080226     c                   z-add     §lltimp       vscimp
067300080226
067400080226     c                   clear                   vscopz
067500080226
067600080226     c                   add       1             nrr
067700080226     c                   write     TB63sf
067800080226     c
067900080226     c     ktbe2         reade     tntbe02l                               30
068000080226     c                   enddo
068100080226     c
068200080226     c
068300080226      * Se ci sono dei record visualizzo il sfl
068400080226     c                   if        nrr>0
068500080226     c                   eval      nrr=1
068600080226     c                   eval      *in36='1'
068700080226     c                   endif
068800080226     c
068900080226     c                   ENDSR
069000060403
069100060403** - $Opz ----*
069200060403  Inserimento
069300060403    Modifica
069400060403     Copia
069500060403  Annullamento
069600060403Visualizzazione
069700060403   ANNULLATO
069800060403** - $Msg -------------------------------------------------------------------*
069900060403Immettere il codice cliente                                                    01
070000060403Codice cliente errato                                                          02
070100080212Importo limite liquidazione transattiva a zero ENTER per forzare               03
070200080228Password errata                                                                04
