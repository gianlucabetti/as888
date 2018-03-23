000100151007     /*END
000200060403      *---------------------------------------------------------------*
000300090324      * Gestione tabella "GPP" = Gestione pratiche particolari Clienti
000400060403      *---------------------------------------------------------------*
000500060403
000600080228     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700060403
000800060403      *---------------------------------------------------------------*
000900060403      *   A R C H I V I                                               *
001000060403      *---------------------------------------------------------------*
001100060403      *
001200060403     fTNTBE01L  uf a e           k disk
001300080226     FTNTBE02L  IF   E           K DISK    RENAME(tntbe000:tntbe002)
001400060403      *
001500090324     fTNTB72D   cf   e             workstn sfile(tb72sf:nrr)
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
002800071207     d $Msg            s             78    dim( 7) ctdata perrcd(1)             Messaggi video
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
003900060403      *
004000090324      * Tabella GPP = Gestione pratiche particolari personalizzata per cliente
004100090324     d dGPP          e ds                  inz
004200080228      * Tabella STD = Parametri standard recupero password
004300080228     d dSTD          e ds                  inz
004400090324      * Tabella CCH = Causali di chiusura CA
004500090324     d dCCH          e ds                  inz
004600071207
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
006300060403      *
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
008000080228     d $TipVid         s              1a   inz('P')
008100060403     d wTasto          s              2a   inz(*zeros)
008200060403     d wDate           s               d   datfmt(*iso) inz(*loval)
008300090324     d Kcod            S                   LIKE(TBEcod)   inz('GPP')
008400080226     d Klin            S                   LIKE(TBElin)
008500080226     d Ksif            S                   LIKE(TBEsif)
008600060403
008700060403      *---------------------------------------------------------------*
008800060403      *   M A I N   L I N E                                           *
008900060403      *---------------------------------------------------------------*
009000060403      *  Riepilogo indicatori utilizzati:                             *
009100060403      *  --------------------------------                             *
009200060403      *  01 - Record inesistente (inserimento)                        *
009300060403      *  02 - Record esistente   (modifica)                           *
009400060403      *  04 - Record annullato   (ripristino)                         *
009500060403      *  10 - Comodo                                                  *
009600060403      *  22 - Errori in scrittura record (WRITE)                      *
009700060403      *  28 - Emissione messaggio di errore a video                   *
009800060403      *  50 - ERR: Codice cliente                                     *
009900090324      *  51 - ERR: Immettere almeno un raggruppamento anomalia        *
010000090324      *  52 - ERR: Causale chiusura errata                            *
010100060403      *  90 - Riemissione videata                                     *
010200060403      *---------------------------------------------------------------*
010300060403      *
010400060403      * Operazioni iniziali
010500060403     c                   exsr      RutInz
010600060403      *
010700060403      * Gestione video
010800060403     c                   dow       $Fine   = *off
010900080228     c     $TipVid       caseq     'P'           GesVP
011000060403     c     $TipVid       caseq     '1'           GesV1
011100060403     c     $TipVid       caseq     '2'           GesV2
011200060403     c                   endcs
011300060403     c                   enddo
011400060403      *
011500060403      * Fine
011600060403      *
011700080228     c                   clear                   TIBS02ds
011800080228     c                   movel     'C'           T02tla
011900080228     c                   call      'TIBS02R'
012000080228     c                   parm                    KPJBA
012100080228     c                   parm                    TIBS02ds
012200080228      *
012300060403     c                   movel     *on           *inLR
012400060403
012500060403      *---------------------------------------------------------------*
012600060403      * RutInz - Operazioni Iniziali                                  *
012700060403      *---------------------------------------------------------------*
012800060403     c     RutInz        BEGSR
012900060403      *
013000060403      * Ricezione parametri
013100060403     c     *entry        plist
013200060403     c                   parm                    KPJBA
013300060403      *
013400060403      * Definizioni chiavi di accesso
013500060403     c     K05TBE01      klist                                                  *tntbe01l
013600060403     c                   kfld                    TBEcod                         -tabella
013700060403     c                   kfld                    TBEke1                         -chiave uno
013800060403     c                   kfld                    TBEke2                         -chiave due
013900060403     c                   kfld                    TBElin                         -lingua
014000060403     c                   kfld                    TBEsif                         -s.informativo
014100080226      *
014200080226     c     KTBE2         klist
014300080226     C                   KFLD                    Kcod
014400080226     C                   KFLD                    Klin
014500080226     C                   KFLD                    Ksif
014600080226      *
014700071207     C*
014800060403      *
014900060403      * Reperisco la data odierna
015000060403     c                   movel     *date         Wdate
015100060403      *
015200060403      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
015300060403     c     *dtaara       define    §azute        AZUTEds
015400060403     c     *dtaara       define    §datiute      dDatiUte
015500060403      *
015600060403     c                   clear                   AZUTEds
015700060403     c                   clear                   dDatiUte
015800060403     c                   clear                   TIBS34ds
015900060403     c                   in(E)     *dtaara
016000060403if  1c                   if        %Error  or  RSUT = *blanks
016100060403     c                   call      'TIBS34R'
016200060403     c                   parm                    TIBS34ds
016300060403     c                   in        *dtaara
016400060403e   1c                   endif
016500060403      *-- Verifica errori e autorità profilo
016600060403sel 1c                   SELECT
016700060403      *-- controllo se ho errori nei dati utente
016800060403      *--   nel qual caso NON risulta un profilo abilitato
016900060403w   1c                   WHEN      DUTerr  = 'E'
017000060403     c                   eval      $Fine   = *on
017100080212      *
017200080212e   1c                   ENDSL
017300080212      *
017400060403      * Aggancio dati generali della tabella in esame
017500060403     c                   clear                   TBEcod
017600060403     c                   move      *zeros        TBEke1
017700090324     c                   move      'GPP'         TBEke1
017800060403     c                   clear                   TBEke2
017900060403     c                   clear                   TBElin
018000060403     c                   movel     KNSIF         TBEsif
018100060403     c     K05TBE01      chain     TNTBE01L
018200060403     c                   if        not %found(TNTBE01L)
018300060403     c                   clear                   TBEsif
018400060403     c     K05TBE01      chain     TNTBE01L
018500060403     c                   endif
018600060403     c                   if        %found(TNTBE01L)
018700060403     c                   movel     TNTBEds       xTNTBEds
018800060403     c                   else
018900060403     c                   clear                   xTNTBEds
019000060403     c                   endif
019100060403      *
019200090324     c                   clear                   TB72V1
019300080228      *
019400080228      * Aggancio tabella std per recupero parola d'ordine se non sono utente EDP
019500080228     c                   if        %subst(KNMUS:1:3) = 'EDP'
019600080228     c                   eval      $tipvid = '1'
019700080228     c                   endif
019800060403      *
019900060403     c                   ENDSR
020000060403
020100080228      *---------------------------------------------------------------*
020200080228      * GESVP  - Gestione videata richiesta  password                 *
020300080228      *---------------------------------------------------------------*
020400080228     c     GesVP         BEGSR
020500080228      *
020600080228      * Recupero la tabella STD con parola d'ordine
020700080228      ***
020800080228      * leggo tabella standards danni
020900080228      ***
021000080228     c                   clear                   TIBS02DS
021100080228     C                   MOVEL     'C'           t02mod
021200080228     C                   MOVEL     knsif         t02sif
021300080228     C                   MOVEL     'STD'         t02cod
021400080228     C                   MOVEL(P)  1             t02ke1
021500080228     C                   MOVEL(P)  'EUR'         t02ke2
021600080228      *
021700080228     C                   CALL      'TIBS02R'
021800080228     C                   PARM                    KPJBA
021900080228     C                   PARM                    TIBS02DS
022000080228      *
022100080228     C                   if        t02err = *BLANKS
022200080228     C                   MOVEL     T02UNI        DSTD
022300080228     C                   endif
022400080228      *
022500080228      * Scrivo la testata
022600090324     c                   write     TB72T1
022700080228      *
022800080228      *
022900090324     c                   exfmt     TB72VP
023000080228     c                   setoff                                       28  90
023100080228     c                   clear                   V1Dmsg
023200080228      *
023300080228sel 1c                   select
023400080228      * F3=Fine
023500080228w   1c                   when      *inKC
023600080228     c                   eval      $fine = *on
023700080228     c                   leavesr
023800080228      *
023900080228      * Controllo dati immessi a video
024000080228x   1c                   other
024100080228     c                   if        vPword <> §STDPWLT
024200080228     c                   seton                                        2890
024300080228     c                   eval      V1Dmsg  = $Msg(4)
024400080228     c                   leavesr
024500080228e   1c                   endif
024600080228      * - Passaggio alla videata di dettaglio
024700080228     c                   eval      $TipVid = '1'
024800080228      *
024900080228e   1c                   endsl
025000080228      *
025100080228     c                   ENDSR
025200080228
025300060403      *---------------------------------------------------------------*
025400060403      * GESV1  - Gestione videata selezione codice tabella            *
025500060403      *---------------------------------------------------------------*
025600060403     c     GesV1         BEGSR
025700060403      *
025800060403      * Inizializzazione videata
025900060403if  1c                   if        $InzV1  = *on
026000060403     c                   exsr      InzV1
026100060403     c                   movel     *off          $InzV1
026200060403e   1c                   endif
026300060403      *
026400060403      * Scrivo la testata
026500090324     c                   write     TB72T1
026600060403      *
026700060403      *
026800090324     c                   exfmt     TB72V1
026900060403     c                   setoff                                       28  90
027000060403     c                   clear                   V1Dmsg
027100060403      *
027200060403sel 1c                   select
027300060403      * F3=Fine
027400060403w   1c                   when      *inKC
027500060403     c                   exsr      F03V1
027600060403     c                   leavesr
027700060403      *
027800060403      * Controllo dati immessi a video
027900060403x   1c                   other
028000060403     c                   exsr      CtrV1
028100060403      * - Rilevati Errori
028200060403if  2c                   if        *in90
028300060403     c                   leavesr
028400060403e   2c                   endif
028500060403      * - Passaggio alla videata di dettaglio
028600060403     c                   eval      $InzV2  = *on
028700060403     c                   eval      $TipVid = '2'
028800060403      *
028900060403e   1c                   endsl
029000060403      *
029100060403     c                   ENDSR
029200060403
029300060403      *---------------------------------------------------------------*
029400060403      * INZV1  - Inizializzazione prima videata (V1)                  *
029500060403      *---------------------------------------------------------------*
029600060403     c     InzV1         BEGSR
029700060403      *
029800090324     c                   clear                   TB72V1
029900060403      *
030000060403      * Imposto il campo "codice cliente" per l'interrogazione
030100060403     c                   move      '?'           V1Cksc
030200060403      *
030300060403     c                   ENDSR
030400060403
030500060403      *---------------------------------------------------------------*
030600060403      * CTRV1  - Controllo e decodifica prima videata                 *
030700060403      *---------------------------------------------------------------*
030800060403     c     CtrV1         BEGSR
030900060403      *
031000060403     c                   movea     *zeros        *in(50)
031100060403      *
031200060403      * Codice cliente
031300060403      * - Ricerca:
031400060403     c     '?'           scan      V1Cksc                                 10
031500080228     c                   if        *in10
031600080226     c                   exsr      carica
031700080226      * gestione subfile
031800080226     c                   exsr      gessbf
031900080227     c   kc              eval      $Fine   = *on
032000080227     c   kc              leavesr
032100080228     c*  kl              leavesr
032200080228
032300080228     c                   endif
032400080226
032500060403      * - Controllo:
032600060403      * - - effettiva immissione
032700060403if  1c                   if           V1Cksc = *blanks
032800060403     c                             or V1Cksc = *zeros
032900060403     c                   seton                                        285090
033000060403     c                   eval      V1Dmsg  = $Msg(1)
033100060403     c                   leavesr
033200060403e   1c                   endif
033300060403      * - - numericità
033400060403     c     DigitN        check     V1Cksc                                 10
033500060403if  1c                   if        *in10   =  *on
033600060403     c                   seton                                        285090
033700060403     c                   eval      V1Dmsg  = $Msg(2)
033800060403     c                   leavesr
033900060403e   1c                   endif
034000060403      * - - decodifica
034100060403     c                   exsr      DecodCLI
034200080212      *
034300060403     c                   ENDSR
034400080226
034500080226      *---------------------------------------------------------------*
034600080226      * Gessbf     * Gestione Sub file                                *
034700080226      *---------------------------------------------------------------*
034800080226     c     Gessbf        BEGSR
034900080226
035000080226      * emissione della videata
035100090324     c                   write     TB72t1
035200090324     c                   write     tb72zsf
035300080226     c                   do        *hival
035400090324     c                   EXFMT     TB72ct
035500080227     c                   clear                   V1CMSG
035600080227     c                   setoff                                       9028
035700080227      *
035800080227      * F3=FINE
035900080227     c   KC              leave
036000080227      *
036100080227      * F12=FINE
036200080227     c   KL              leave
036300080227      *
036400080227     c                   do        *hival
036500090324     c                   readc     tb72sf                                 30
036600080228     c                   if        *in30
036700080227     c                   leave
036800080227     c                   endif
036900080227      * verifico se scelto un cliente
037000080228     c                   if        vscopz = '1'
037100080227     c                   move      vscksc        v1cksc
037200080228     c                   leavesr
037300080227     c                   endif
037400080226      *
037500080227     c                   enddo
037600080227      *
037700080227     c                   enddo
037800080226      *
037900080226     c                   ENDSR
038000060403
038100060403      *---------------------------------------------------------------*
038200060403      * DecodCLI   * Decodifica codice cliente                        *
038300060403      *---------------------------------------------------------------*
038400060403     c     DecodCLI      BEGSR
038500060403      *
038600060403     c                   clear                   TIBS69ds
038700060403     c                   clear                   ds_CNACO
038800060403     c                   clear                   ds_CNIND
038900060403     c                   clear                   ds_CNCLP
039000060403     c                   clear                   ds_FNCLS
039100060403     c                   move      V1Cksc        I69kac
039200060403     c                   call      'TIBS69R'
039300060403     c                   parm                    TIBS69ds
039400060403     c                   parm                    ds_CNACO
039500060403     c                   parm                    ds_CNIND
039600060403     c                   parm                    ds_CNCLP
039700060403     c                   parm                    ds_FNCLS
039800060403      *
039900060403     c                   if        O69err  = *blanks
040000060403     c                   movel     ACOrag        V2Dksc
040100060403     c                   else
040200060403     c                   seton                                        285090
040300060403     c                   eval      V1Dmsg  = $Msg(2)
040400060403     C                   endif
040500060403      *
040600060403     c                   ENDSR
040700060403
040800060403      *---------------------------------------------------------------*
040900060403      * F03V1  - Tasto funzionale F03 -> Fine programma               *
041000060403      *---------------------------------------------------------------*
041100060403     c     F03V1         BEGSR
041200060403      *
041300060403     c                   movel     *on           $Fine                          fine pgm
041400060403      *
041500060403     c                   ENDSR
041600060403
041700060403      *---------------------------------------------------------------*
041800060403      * GESV2  - Gestione videata dettaglio dati                      *
041900060403      *---------------------------------------------------------------*
042000060403     c     GesV2         BEGSR
042100060403      *
042200060403      * Inizializzazione videata
042300060403     c                   if        $InzV2  = *on
042400060403     c                   exsr      InzV2
042500060403     c                   move      *off          $InzV2
042600060403     c                   endif
042700060403      *
042800060403      * Scrivo la testata
042900090324     c                   write     TB72T1
043000060403      *
043100090324     c                   exfmt     TB72V2
043200080212      *
043300060403     c                   setoff                                       28  90
043400060403     c                   clear                   V1Dmsg
043500060403     c                   reset                   wTasto
043600060403      *
043700060403sel 1c                   select
043800060403      * F03=Fine
043900060403w   1c                   when      *inKC
044000060403     c                   exsr      F03V1
044100060403      * F12=Ritorno
044200060403w   1c                   when      *inKL
044300060403     c                   exsr      F12V2
044400060403      *
044500060403x   1c                   other
044600060403      * Controllo dati immessi a video
044700060403      * (non si fanno se richisto l'annullamento)
044800060403if  2c                   if        not *inKQ
044900060403     c                   exsr      CtrV2
045000060403e   2c                   endif
045100060403      * Aggiornamento se non ci sono errori
045200060403sel 2c                   select
045300060403      * - Rilevati errori
045400060403w   2c                   when      *in90
045500060403      * - F5=Ripristino
045600060403w   2c                   when      *inKE
045700060403     c                   eval      wTasto  = '05'
045800060403      * - F6=Conferma
045900060403w   2c                   when      *inKF
046000060403     c                   eval      wTasto  = '06'
046100060403      * - F16=Annullamento
046200060403w   2c                   when      *inKQ
046300060403     c                   eval      wTasto  = '16'
046400060403e   2c                   endsl
046500080212      *
046600080215      *
046700080228if  2c                   if        wTasto  <> '00'
046800080215     C                   EXSR      AGGTBE
046900080215     c                   eval      $TipVid = '1'
047000080215e   2c                   endif
047100060403e   1c                   endsl
047200060403      *
047300060403     c                   ENDSR
047400060403
047500060403      *---------------------------------------------------------------*
047600060403      * INZV2  - Inizializzazione seconda videata (V2)                *
047700060403      *---------------------------------------------------------------*
047800060403     c     InzV2         BEGSR
047900060403      *
048000090324     c                   clear                   TB72V2
048100060403      *
048200060403      * reimposto il campo chiave (qui di solo output)
048300060403     c                   movel     V1Cksc        V2Cksc
048400060403     c                   movel     ACOrag        V2Dksc
048500060403      *
048600060403      * Aggancio la tabella, se trovo il codice sono in modifica
048700060403      * o ripristino (se record annullato), altrimenti in immissione
048800090324     c                   clear                   dGPP
048900060403     c                   exsr      ChnTBE
049000060403      *
049100060403sel 1c                   SELECT
049200060403      * IMMISSIONE
049300060403w   1c                   WHEN      NOT %found(TNTBE01L)
049400060403     c                   eval      *in01   = *on
049500060403     c                   eval      T1opz   = $Opz(01)
049600060403      * MODIFICA
049700060403w   1c                   WHEN          %found(TNTBE01L)
049800060403     c                             and TBEatb  = *blanks
049900060403     c                   eval      *in02   = *on
050000060403     c                   eval      T1opz   = $Opz(02)
050100060403      * RIPRISTINO
050200060403w   1c                   WHEN          %found(TNTBE01L)
050300060403     c                             and TBEatb <> *blanks
050400060403     c                   eval      *in04   = *on
050500060403     c                   eval      T1opz   = $Opz(06)
050600060403e   1c                   ENDSL
050700060403      *
050800060403if  1c                   if        %found(TNTBE01L)
050900090324     c                   movel     TBEuni        dGPP
051000090324     c                   eval      V2Crg1  = §GPPrg1
051100090324     c                   eval      V2Crg2  = §GPPrg2
051200090324     c                   eval      V2Crg3  = §GPPrg3
051300090324     c                   eval      V2Ccch  = §GPPcch
051400090325     c                   eval      V2Cevf  = §GPPevf
051500090325     c                   clear                   TIBS02DS
051600090325     C                   MOVEL     'C'           t02mod
051700090325     C                   MOVEL     knsif         t02sif
051800090325     C                   MOVEL     'CCH'         t02cod
051900090325     C                   MOVEL(P)  v2ccch        t02ke1
052000090325      *
052100090325     C                   CALL      'TIBS02R'
052200090325     C                   PARM                    KPJBA
052300090325     C                   PARM                    TIBS02DS
052400090325      *
052500090325     C                   if        t02err = *BLANKS
052600090325     C                   MOVEL     T02UNI        DCCH
052700090325     c                   eval      v2dcch = §cchdesc
052800090325     c                   endif
052900060403e   1c                   endif
053000060403      *
053100060403     c                   ENDSR
053200060403
053300060403      *---------------------------------------------------------------*
053400060403      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
053500060403      *---------------------------------------------------------------*
053600060403     c     F12V2         BEGSR
053700060403      *
053800060403     c                   eval      $TipVid = '1'
053900060403      *
054000060403     c                   ENDSR
054100060403
054200060403      *---------------------------------------------------------------*
054300060403      * CTRV2  - Controllo e decodifica seconda videata               *
054400060403      *---------------------------------------------------------------*
054500060403     c     CtrV2         BEGSR
054600060403      *
054700060403     c                   movea     *zeros        *in(50)
054800060403      *
054900060403sel 1c                   SELECT
055000060403      *
055100090324      * Codici raggruppamento anomalie
055200090324     c                   WHEN      V2Crg1 =' ' and V2Crg2 =' ' and V2Crg3 =' '
055300060403     c                   seton                                        285190
055400060403     c                   eval      V1Dmsg  = $Msg(3)
055500090324      *
055600090324      * Causale chiusura non inserita
055700090324     c                   WHEN      V2Ccch ='  '
055800090324     c                   seton                                        285290
055900090324     c                   eval      V1Dmsg  = $Msg(5)
056000090324      *
056100090324      * Ricerca Causale di chiusura
056200090324     c                   WHEN      %scan('?' : V2Ccch) > *zero
056300090324     c                   seton                                        9052
056400090324     c                   clear                   tibs02ds
056500090324     C                   MOVEL     'R'           t02mod
056600090324     C                   MOVEL     knsif         t02sif
056700090324     C                   MOVEL     'CCH'         t02cod
056800090324      *
056900090324     C                   CALL      'TIBS02R'
057000090324     C                   PARM                    KPJBA
057100090324     C                   PARM                    TIBS02DS
057200090324      *
057300090324     C                   IF        T02err = *BLANKS
057400090324     C                   movel     T02ke1        v2ccch
057500090325     c                   movel     t02uni        dcch
057600090325     c                   eval      v2dcch = §cchdesc
057700090324     c                   endif
057800090324     c
057900090324      *
058000090324      * Causale chiusura inserita controllo
058100090324     c                   WHEN      V2Ccch <> '  '
058200090324     c                   clear                   TIBS02DS
058300090324     C                   MOVEL     'C'           t02mod
058400090324     C                   MOVEL     knsif         t02sif
058500090324     C                   MOVEL     'CCH'         t02cod
058600090324     C                   MOVEL(P)  v2ccch        t02ke1
058700090324      *
058800090324     C                   CALL      'TIBS02R'
058900090324     C                   PARM                    KPJBA
059000090324     C                   PARM                    TIBS02DS
059100090324      *
059200090324     C                   if        t02err = *BLANKS
059300090324     C                   MOVEL     T02UNI        DCCH
059400090325     c                   eval      v2dcch = §cchdesc
059500090324     c                   else
059600090324     c                   seton                                        285290
059700090324     c                   eval      V1Dmsg  = $Msg(6)
059800090324     C                   endif
059900090324      *
060000090324     C                   If        §cchpaga <> ' '
060100090324     c                   seton                                        285290
060200090324     c                   eval      V1Dmsg  = $Msg(7)
060300090324     C                   endif
060400090324      *
060500060403e   1c                   ENDSL
060600060403      *
060700060403     c                   ENDSR
060800060403
060900060403      *---------------------------------------------------------------*
061000060403      * CHNTBE * Aggancio tabella                                     *
061100060403      *---------------------------------------------------------------*
061200060403     c     ChnTBE        BEGSR
061300060403      *
061400090324     c                   movel     'GPP'         TBEcod
061500080212     c                   move(p)   V2Cksc        TBEke1
061600060403     c                   clear                   TBEke2
061700060403     c                   clear                   TBElin
061800060403     c                   movel     KNSIF         TBEsif
061900060403     c     K05TBE01      chain     TNTBE01L
062000060403      * Se non ho trovato il record con il sistema informativo
062100060403      * che ho in linea, lo abblenco
062200060403if  1c                   if        not %found(TNTBE01L)
062300060403     c                   clear                   TBEsif
062400060403     c     K05TBE01      chain     TNTBE01L
062500060403e   1c                   endif
062600060403      *
062700060403     c                   ENDSR
062800060403
062900060403      *---------------------------------------------------------------*
063000060403      * AGGTBE * Aggiornamento tabella                                *
063100060403      *---------------------------------------------------------------*
063200060403     c     AggTBE        BEGSR
063300060403      *
063400060403sel 1c                   SELECT
063500060403      *
063600060403      * F5=Ripristino
063700060403w   1c                   WHEN      wTasto='05'  and  *in04
063800060403     c                   clear                   TBEatb
063900080215     c                   movel     'S'           TBEftt
064000060403     c                   clear                   TBEftr
064100060403     c                   UPDATE    TNTBE000
064200060403      *
064300060403      * F6=Conferma
064400060403w   1c                   WHEN      wTasto='06'
064500060403     c                   exsr      RieTBE
064600060403sel 2c                   select
064700060403      *   Immissione
064800060403w   2c                   when      *in01
064900060403     c                   clear                   TBEflt
065000060403     c                   clear                   TBEdtr
065100060403     c                   WRITE     TNTBE000                             22
065200060403      *   Modifica / Ripristino
065300060403w   2c                   when      *in02   or    *in04
065400060403     c                   UPDATE    TNTBE000
065500060403e   2c                   endsl
065600060403      *
065700060403      * F16=Annullamento
065800060403w   1c                   WHEN      wTasto='16'  and  not *in04
065900060403     c                   movel     'A'           TBEatb
066000080215     c                   movel     'S'           TBEftt
066100060403     c                   clear                   TBEftr
066200060403     c                   UPDATE    TNTBE000
066300060403      *
066400060403e   1c                   ENDSL
066500060403      *
066600060403      * Torno alla prima videata che carico come da inizio pgm
066700060403     c                   movel     '1'           $TipVid
066800060403     c                   movel     *on           $InzV1
066900060403     c                   movel     *on           $InzV2
067000060403     c
067100060403     c                   ENDSR
067200060403
067300060403      *---------------------------------------------------------------*
067400060403      * RIETBE * Riempimento dati tabella                             *
067500060403      *---------------------------------------------------------------*
067600060403     c     RieTBE        BEGSR
067700060403      *
067800060403     c                   clear                   TBEatb
067900060403     c                   if        TBXsif <> *blanks
068000060403     c                   movel     KNSIF         TBEsif
068100060403     c                   else
068200060403     c                   clear                   TBEsif
068300060403     c                   endif
068400060403     c                   movel     TBXapl        TBEapl
068500090324     c                   movel     'GPP'         TBEcod
068600080212     c                   move      V2Cksc        TBEke1
068700060403     c                   clear                   TBEke2
068800060403      *
068900090324     c                   clear                   dGPP
069000090324     c                   eval      §GPPrg1 = V2Crg1
069100090324     c                   eval      §GPPrg2 = V2Crg2
069200090324     c                   eval      §GPPrg3 = V2Crg3
069300090324     c                   eval      §GPPcch = V2Ccch
069400090325     c                   eval      §GPPevf = V2Cevf
069500090324     c                   movel(p)  dGPP          TBEuni
069600060403      *
069700080215     c                   movel     'S'           TBEftt
069800060403     c                   clear                   TBEflt
069900060403     c                   clear                   TBEftr
070000060403     c                   clear                   TBEdtr
070100060403      *
070200060403     c                   ENDSR
070300080226      *****************************************************************
070400080226      *   carico tutti gli elementi della tabella
070500080226      *****************************************************************
070600080226     c     CARICA        BEGSR
070700080226     c                   clear                   NRR               4 0
070800080226     c
070900080226     c                   eval      *in35='1'
071000090324     c                   write     TB72ct
071100080226     c                   eval      *in35='0'
071200080226     c
071300080226     c     ktbe2         chain     tntbe02l                           30
071400080226     c                   dow       not *in30
071500080226     c                   if        tbeatb=' '
071600080228     c                   setoff                                       48
071700080228     c                   else
071800080228     c                   seton                                        48
071900080228     c                   endif
072000080226      *
072100080228     c                   move      tbeke1        vscksc
072200080226     c
072300080226     C                   clear                   TIBS69DS
072400080226     c                   z-add     vscksc        I69kac
072500080226      *
072600080226     C                   CALL      'TIBS69R'
072700080226     C                   PARM                    tibs69DS
072800080226     C                   PARM                    DS_cnaco
072900080226     C                   PARM                    DS_cnind
073000080226     C                   PARM                    DS_cnclp
073100080226     C                   PARM                    DS_fncls
073200080226    2c                   if        o69err=' '
073300080226     c                   movel     acorag        vsdksc
073400080226    2c                   endif
073500080226      * recupero l'importo
073600090324     c                   movel     tbeuni        dGPP
073700090324     c                   move      §GPPrg1       vscrg1
073800090324     c                   move      §GPPrg2       vscrg2
073900090324     c                   move      §GPPrg3       vscrg3
074000090324     c                   move      §GPPcch       vsccch
074100090325     c                   If        §GPPevf = 'S'
074200090325     c                   eval      vscevf = 'SI'
074300090325     c                   else
074400151007     c                   clear                   vscEVF
074500090325     c                   endif
074600080226
074700080226     c                   clear                   vscopz
074800080226
074900080226     c                   add       1             nrr
075000090324     c                   write     TB72sf
075100080226     c
075200080226     c     ktbe2         reade     tntbe02l                               30
075300080226     c                   enddo
075400080226     c
075500080226     c
075600080226      * Se ci sono dei record visualizzo il sfl
075700080226     c                   if        nrr>0
075800080226     c                   eval      nrr=1
075900080226     c                   eval      *in36='1'
076000080226     c                   endif
076100080226     c
076200080226     c                   ENDSR
076300060403
076400060403** - $Opz ----*
076500060403  Inserimento
076600060403    Modifica
076700060403     Copia
076800060403  Annullamento
076900060403Visualizzazione
077000060403   ANNULLATO
077100060403** - $Msg -------------------------------------------------------------------*
077200060403Immettere il codice cliente                                                    01
077300060403Codice cliente errato                                                          02
077400090324Immettere almeno un raggruppamento anomalia                                    03
077500080228Password errata                                                                04
077600090324Inserire la causale di chiusura                                                05
077700090324Causale di chiusura errata                                                     06
077800090324Causale di chiusura errata perchè usata per pagamenti danni                    07
