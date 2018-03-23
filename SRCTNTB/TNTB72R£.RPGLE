000100060403      *---------------------------------------------------------------*
000200090324      * Gestione tabella "GPP" = Gestione pratiche particolari Clienti
000300060403      *---------------------------------------------------------------*
000400060403
000500080228     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060403
000700060403      *---------------------------------------------------------------*
000800060403      *   A R C H I V I                                               *
000900060403      *---------------------------------------------------------------*
001000060403      *
001100060403     fTNTBE01L  uf a e           k disk
001101080226     FTNTBE02L  IF   E           K DISK    RENAME(tntbe000:tntbe002)
001300060403      *
001400090324     fTNTB72D   cf   e             workstn sfile(tb72sf:nrr)
001500060403
001600060403      *---------------------------------------------------------------*
001700060403      *   C O S T A N T I                                             *
001800060403      *---------------------------------------------------------------*
001900060403      *
002000060403     d DigitN          c                   const('0123456789')
002100060403
002200060403      *---------------------------------------------------------------*
002300060403      *   S C H I E R E                                               *
002400060403      *---------------------------------------------------------------*
002500060403      *
002600060403     d $Opz            s             15    dim( 6) ctdata perrcd(1)             Decodifica OPZ
002700071207     d $Msg            s             78    dim( 7) ctdata perrcd(1)             Messaggi video
002800060403
002900060403      *---------------------------------------------------------------*
003000060403      *   S T R U T T U R E   D A T I                                 *
003100060403      *---------------------------------------------------------------*
003200060403      *
003300060403      * Parametri
003400060403     d KPJBA         e ds
003500060403      *
003600060403      * Passaggio Parametri al pgm TIBS02R
003700060403     d TIBS02ds      e ds                  inz
004000060403      *
004100090324      * Tabella GPP = Gestione pratiche particolari personalizzata per cliente
004200090324     d dGPP          e ds                  inz
004201080228      * Tabella STD = Parametri standard recupero password
004202080228     d dSTD          e ds                  inz
004203090324      * Tabella CCH = Causali di chiusura CA
004204090324     d dCCH          e ds                  inz
004300071207
004500060403      *
004600060403      * Tracciato record file TNTBE00F
004700060403     d TNTBEds       e ds                  extname(TNTBE00F) inz
004800060403     d xTNTBEds      e ds                  extname(TNTBE00F) inz
004900060403     d                                     prefix(TBX:3)
005000060403      *
005100060403     d TIBS34ds      e ds                  inz
005200060403     d dDatiUte      e ds                  inz
005300060403     d AZUTEds       e ds                  extname(AZUTE00F) inz
005400060403      *
005500060403      * Decodifica anagrafica clienti
005600060403     d TIBS69ds      e ds                  inz
005700060403     d ds_CNACO      e ds                  extname(CNACO00F) inz
005800060403     d ds_CNIND      e ds                  extname(CNIND00F) inz
005900060403     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
006000060403     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
006100060403      *
006200060403     d WLBdat          ds                  inz
006300060403     d  G02dat                 1      8  0 inz
006400060403     d  G02inv                 9     16  0 inz
006500060403     d  G02err                17     17    inz('3')
006600060403     d  G02tgi                18     22  0 inz
006700060403      *
006800060403     d Status         sds
006900060403     d  VTCpgm           *proc
007000060403
007100060403      *---------------------------------------------------------------*
007200060403      *   V A R I A B I L I                                           *
007300060403      *---------------------------------------------------------------*
007400060403      *
007500060403     d $Fine           s              1a   inz(*off)
007600060403     d $InzV1          s              1a   inz(*on)
007700060403     d $InzV2          s              1a   inz(*off)
007900080228     d $TipVid         s              1a   inz('P')
008200060403     d wTasto          s              2a   inz(*zeros)
008300060403     d wDate           s               d   datfmt(*iso) inz(*loval)
008302090324     d Kcod            S                   LIKE(TBEcod)   inz('GPP')
008303080226     d Klin            S                   LIKE(TBElin)
008304080226     d Ksif            S                   LIKE(TBEsif)
008900060403
009000060403      *---------------------------------------------------------------*
009100060403      *   M A I N   L I N E                                           *
009200060403      *---------------------------------------------------------------*
009300060403      *  Riepilogo indicatori utilizzati:                             *
009400060403      *  --------------------------------                             *
009500060403      *  01 - Record inesistente (inserimento)                        *
009600060403      *  02 - Record esistente   (modifica)                           *
009700060403      *  04 - Record annullato   (ripristino)                         *
009800060403      *  10 - Comodo                                                  *
009900060403      *  22 - Errori in scrittura record (WRITE)                      *
010000060403      *  28 - Emissione messaggio di errore a video                   *
010100060403      *  50 - ERR: Codice cliente                                     *
010200090324      *  51 - ERR: Immettere almeno un raggruppamento anomalia        *
010201090324      *  52 - ERR: Causale chiusura errata                            *
010900060403      *  90 - Riemissione videata                                     *
011000060403      *---------------------------------------------------------------*
011100060403      *
011200060403      * Operazioni iniziali
011300060403     c                   exsr      RutInz
011400060403      *
011500060403      * Gestione video
011600060403     c                   dow       $Fine   = *off
011601080228     c     $TipVid       caseq     'P'           GesVP
011700060403     c     $TipVid       caseq     '1'           GesV1
011800060403     c     $TipVid       caseq     '2'           GesV2
012000060403     c                   endcs
012100060403     c                   enddo
012200060403      *
012300060403      * Fine
013100060403      *
013101080228     c                   clear                   TIBS02ds
013102080228     c                   movel     'C'           T02tla
013103080228     c                   call      'TIBS02R'
013104080228     c                   parm                    KPJBA
013105080228     c                   parm                    TIBS02ds
013106080228      *
013200060403     c                   movel     *on           *inLR
013300060403
013400060403      *---------------------------------------------------------------*
013500060403      * RutInz - Operazioni Iniziali                                  *
013600060403      *---------------------------------------------------------------*
013700060403     c     RutInz        BEGSR
013800060403      *
013900060403      * Ricezione parametri
014000060403     c     *entry        plist
014100060403     c                   parm                    KPJBA
014200060403      *
014300060403      * Definizioni chiavi di accesso
014400060403     c     K05TBE01      klist                                                  *tntbe01l
014500060403     c                   kfld                    TBEcod                         -tabella
014600060403     c                   kfld                    TBEke1                         -chiave uno
014700060403     c                   kfld                    TBEke2                         -chiave due
014800060403     c                   kfld                    TBElin                         -lingua
014900060403     c                   kfld                    TBEsif                         -s.informativo
014901080226      *
014902080226     c     KTBE2         klist
014903080226     C                   KFLD                    Kcod
014904080226     C                   KFLD                    Klin
014905080226     C                   KFLD                    Ksif
014906080226      *
015000071207     C*
015500060403      *
015600060403      * Reperisco la data odierna
015700060403     c                   movel     *date         Wdate
015800060403      *
015900060403      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
016000060403     c     *dtaara       define    §azute        AZUTEds
016100060403     c     *dtaara       define    §datiute      dDatiUte
016200060403      *
016300060403     c                   clear                   AZUTEds
016400060403     c                   clear                   dDatiUte
016500060403     c                   clear                   TIBS34ds
016600060403     c                   in(E)     *dtaara
016700060403if  1c                   if        %Error  or  RSUT = *blanks
016800060403     c                   call      'TIBS34R'
016900060403     c                   parm                    TIBS34ds
017000060403     c                   in        *dtaara
017100060403e   1c                   endif
017200060403      *-- Verifica errori e autorità profilo
017300060403sel 1c                   SELECT
017400060403      *-- controllo se ho errori nei dati utente
017500060403      *--   nel qual caso NON risulta un profilo abilitato
017600060403w   1c                   WHEN      DUTerr  = 'E'
017700060403     c                   eval      $Fine   = *on
017701080212      *
017702080212e   1c                   ENDSL
017703080212      *
019300060403      * Aggancio dati generali della tabella in esame
019400060403     c                   clear                   TBEcod
019500060403     c                   move      *zeros        TBEke1
019600090324     c                   move      'GPP'         TBEke1
019700060403     c                   clear                   TBEke2
019800060403     c                   clear                   TBElin
019900060403     c                   movel     KNSIF         TBEsif
020000060403     c     K05TBE01      chain     TNTBE01L
020100060403     c                   if        not %found(TNTBE01L)
020200060403     c                   clear                   TBEsif
020300060403     c     K05TBE01      chain     TNTBE01L
020400060403     c                   endif
020500060403     c                   if        %found(TNTBE01L)
020600060403     c                   movel     TNTBEds       xTNTBEds
020700060403     c                   else
020800060403     c                   clear                   xTNTBEds
020900060403     c                   endif
021000060403      *
021100090324     c                   clear                   TB72V1
021101080228      *
021102080228      * Aggancio tabella std per recupero parola d'ordine se non sono utente EDP
021103080228     c                   if        %subst(KNMUS:1:3) = 'EDP'
021104080228     c                   eval      $tipvid = '1'
021105080228     c                   endif
021200060403      *
021300060403     c                   ENDSR
021400060403
021401080228      *---------------------------------------------------------------*
021402080228      * GESVP  - Gestione videata richiesta  password                 *
021403080228      *---------------------------------------------------------------*
021404080228     c     GesVP         BEGSR
021405080228      *
021406080228      * Recupero la tabella STD con parola d'ordine
021407080228      ***
021408080228      * leggo tabella standards danni
021409080228      ***
021410080228     c                   clear                   TIBS02DS
021411080228     C                   MOVEL     'C'           t02mod
021412080228     C                   MOVEL     knsif         t02sif
021413080228     C                   MOVEL     'STD'         t02cod
021414080228     C                   MOVEL(P)  1             t02ke1
021415080228     C                   MOVEL(P)  'EUR'         t02ke2
021416080228      *
021417080228     C                   CALL      'TIBS02R'
021418080228     C                   PARM                    KPJBA
021419080228     C                   PARM                    TIBS02DS
021420080228      *
021421080228     C                   if        t02err = *BLANKS
021422080228     C                   MOVEL     T02UNI        DSTD
021423080228     C                   endif
021437080228      *
021438080228      * Scrivo la testata
021439090324     c                   write     TB72T1
021440080228      *
021441080228      *
021442090324     c                   exfmt     TB72VP
021443080228     c                   setoff                                       28  90
021444080228     c                   clear                   V1Dmsg
021445080228      *
021446080228sel 1c                   select
021447080228      * F3=Fine
021448080228w   1c                   when      *inKC
021449080228     c                   eval      $fine = *on
021450080228     c                   leavesr
021451080228      *
021452080228      * Controllo dati immessi a video
021453080228x   1c                   other
021454080228     c                   if        vPword <> §STDPWLT
021455080228     c                   seton                                        2890
021456080228     c                   eval      V1Dmsg  = $Msg(4)
021457080228     c                   leavesr
021458080228e   1c                   endif
021463080228      * - Passaggio alla videata di dettaglio
021465080228     c                   eval      $TipVid = '1'
021466080228      *
021467080228e   1c                   endsl
021468080228      *
021469080228     c                   ENDSR
021470080228
021500060403      *---------------------------------------------------------------*
021600060403      * GESV1  - Gestione videata selezione codice tabella            *
021700060403      *---------------------------------------------------------------*
021800060403     c     GesV1         BEGSR
021900060403      *
022000060403      * Inizializzazione videata
022100060403if  1c                   if        $InzV1  = *on
022200060403     c                   exsr      InzV1
022300060403     c                   movel     *off          $InzV1
022400060403e   1c                   endif
022500060403      *
022600060403      * Scrivo la testata
022700090324     c                   write     TB72T1
022800060403      *
023900060403      *
024000090324     c                   exfmt     TB72V1
024100060403     c                   setoff                                       28  90
024200060403     c                   clear                   V1Dmsg
024300060403      *
024400060403sel 1c                   select
024500060403      * F3=Fine
024600060403w   1c                   when      *inKC
024700060403     c                   exsr      F03V1
024800060403     c                   leavesr
024900060403      *
025000060403      * Controllo dati immessi a video
025100060403x   1c                   other
025200060403     c                   exsr      CtrV1
025300060403      * - Rilevati Errori
025400060403if  2c                   if        *in90
025500060403     c                   leavesr
025600060403e   2c                   endif
025700060403      * - Passaggio alla videata di dettaglio
025800060403     c                   eval      $InzV2  = *on
025900060403     c                   eval      $TipVid = '2'
026000060403      *
026100060403e   1c                   endsl
026200060403      *
026300060403     c                   ENDSR
026400060403
026500060403      *---------------------------------------------------------------*
026600060403      * INZV1  - Inizializzazione prima videata (V1)                  *
026700060403      *---------------------------------------------------------------*
026800060403     c     InzV1         BEGSR
026900060403      *
027000090324     c                   clear                   TB72V1
027400060403      *
027500060403      * Imposto il campo "codice cliente" per l'interrogazione
027600060403     c                   move      '?'           V1Cksc
027700060403      *
027800060403     c                   ENDSR
027900060403
028000060403      *---------------------------------------------------------------*
028100060403      * CTRV1  - Controllo e decodifica prima videata                 *
028200060403      *---------------------------------------------------------------*
028300060403     c     CtrV1         BEGSR
028400060403      *
028500060403     c                   movea     *zeros        *in(50)
028600060403      *
028700060403      * Codice cliente
028800060403      * - Ricerca:
028900060403     c     '?'           scan      V1Cksc                                 10
028901080228     c                   if        *in10
029000080226     c                   exsr      carica
029001080226      * gestione subfile
029002080226     c                   exsr      gessbf
029003080227     c   kc              eval      $Fine   = *on
029004080227     c   kc              leavesr
029005080228     c*  kl              leavesr
029006080228
029007080228     c                   endif
029008080226
031200060403      * - Controllo:
031300060403      * - - effettiva immissione
031400060403if  1c                   if           V1Cksc = *blanks
031500060403     c                             or V1Cksc = *zeros
031600060403     c                   seton                                        285090
031700060403     c                   eval      V1Dmsg  = $Msg(1)
031800060403     c                   leavesr
031900060403e   1c                   endif
032000060403      * - - numericità
032100060403     c     DigitN        check     V1Cksc                                 10
032200060403if  1c                   if        *in10   =  *on
032300060403     c                   seton                                        285090
032400060403     c                   eval      V1Dmsg  = $Msg(2)
032500060403     c                   leavesr
032600060403e   1c                   endif
032700060403      * - - decodifica
032800060403     c                   exsr      DecodCLI
032801080212      *
033800060403     c                   ENDSR
033801080226
033802080226      *---------------------------------------------------------------*
033803080226      * Gessbf     * Gestione Sub file                                *
033804080226      *---------------------------------------------------------------*
033805080226     c     Gessbf        BEGSR
033806080226
033807080226      * emissione della videata
033808090324     c                   write     TB72t1
033809090324     c                   write     tb72zsf
033810080226     c                   do        *hival
033811090324     c                   EXFMT     TB72ct
033812080227     c                   clear                   V1CMSG
033813080227     c                   setoff                                       9028
033814080227      *
033815080227      * F3=FINE
033816080227     c   KC              leave
033817080227      *
033818080227      * F12=FINE
033819080227     c   KL              leave
033820080227      *
033821080227     c                   do        *hival
033822090324     c                   readc     tb72sf                                 30
033823080228     c                   if        *in30
033824080227     c                   leave
033825080227     c                   endif
033826080227      * verifico se scelto un cliente
033827080228     c                   if        vscopz = '1'
033828080227     c                   move      vscksc        v1cksc
033829080228     c                   leavesr
033830080227     c                   endif
033831080226      *
033832080227     c                   enddo
033833080227      *
033834080227     c                   enddo
033835080226      *
033836080226     c                   ENDSR
033900060403
034000060403      *---------------------------------------------------------------*
034100060403      * DecodCLI   * Decodifica codice cliente                        *
034200060403      *---------------------------------------------------------------*
034300060403     c     DecodCLI      BEGSR
034400060403      *
034500060403     c                   clear                   TIBS69ds
034600060403     c                   clear                   ds_CNACO
034700060403     c                   clear                   ds_CNIND
034800060403     c                   clear                   ds_CNCLP
034900060403     c                   clear                   ds_FNCLS
035000060403     c                   move      V1Cksc        I69kac
035100060403     c                   call      'TIBS69R'
035200060403     c                   parm                    TIBS69ds
035300060403     c                   parm                    ds_CNACO
035400060403     c                   parm                    ds_CNIND
035500060403     c                   parm                    ds_CNCLP
035600060403     c                   parm                    ds_FNCLS
035700060403      *
035800060403     c                   if        O69err  = *blanks
035900060403     c                   movel     ACOrag        V2Dksc
036000060403     c                   else
036100060403     c                   seton                                        285090
036200060403     c                   eval      V1Dmsg  = $Msg(2)
036300060403     C                   endif
036400060403      *
036500060403     c                   ENDSR
036600060403
036700060403      *---------------------------------------------------------------*
036800060403      * F03V1  - Tasto funzionale F03 -> Fine programma               *
036900060403      *---------------------------------------------------------------*
037000060403     c     F03V1         BEGSR
037100060403      *
037200060403     c                   movel     *on           $Fine                          fine pgm
037300060403      *
037400060403     c                   ENDSR
037500060403
037600060403      *---------------------------------------------------------------*
037700060403      * GESV2  - Gestione videata dettaglio dati                      *
037800060403      *---------------------------------------------------------------*
037900060403     c     GesV2         BEGSR
038000060403      *
038100060403      * Inizializzazione videata
038200060403     c                   if        $InzV2  = *on
038300060403     c                   exsr      InzV2
038400060403     c                   move      *off          $InzV2
038500060403     c                   endif
038600060403      *
038700060403      * Scrivo la testata
038800090324     c                   write     TB72T1
038900060403      *
040600090324     c                   exfmt     TB72V2
040700080212      *
040800060403     c                   setoff                                       28  90
040900060403     c                   clear                   V1Dmsg
041000060403     c                   reset                   wTasto
041100060403      *
041200060403sel 1c                   select
041300060403      * F03=Fine
041400060403w   1c                   when      *inKC
041500060403     c                   exsr      F03V1
041700060403      * F12=Ritorno
041800060403w   1c                   when      *inKL
041900060403     c                   exsr      F12V2
042100060403      *
042200060403x   1c                   other
042300060403      * Controllo dati immessi a video
042400060403      * (non si fanno se richisto l'annullamento)
042500060403if  2c                   if        not *inKQ
042600060403     c                   exsr      CtrV2
042700060403e   2c                   endif
042800060403      * Aggiornamento se non ci sono errori
042900060403sel 2c                   select
043000060403      * - Rilevati errori
043100060403w   2c                   when      *in90
043200060403      * - F5=Ripristino
043300060403w   2c                   when      *inKE
043400060403     c                   eval      wTasto  = '05'
043500060403      * - F6=Conferma
043600060403w   2c                   when      *inKF
043700060403     c                   eval      wTasto  = '06'
043800060403      * - F16=Annullamento
043900060403w   2c                   when      *inKQ
044000060403     c                   eval      wTasto  = '16'
044100060403e   2c                   endsl
044101080212      *
044102080215      *
044103080228if  2c                   if        wTasto  <> '00'
044104080215     C                   EXSR      AGGTBE
044105080215     c                   eval      $TipVid = '1'
044107080215e   2c                   endif
044700060403e   1c                   endsl
044800060403      *
044900060403     c                   ENDSR
045000060403
045100060403      *---------------------------------------------------------------*
045200060403      * INZV2  - Inizializzazione seconda videata (V2)                *
045300060403      *---------------------------------------------------------------*
045400060403     c     InzV2         BEGSR
045500060403      *
045600090324     c                   clear                   TB72V2
045700060403      *
045800060403      * reimposto il campo chiave (qui di solo output)
045900060403     c                   movel     V1Cksc        V2Cksc
046000060403     c                   movel     ACOrag        V2Dksc
047500060403      *
047600060403      * Aggancio la tabella, se trovo il codice sono in modifica
047700060403      * o ripristino (se record annullato), altrimenti in immissione
047800090324     c                   clear                   dGPP
047900060403     c                   exsr      ChnTBE
048000060403      *
048100060403sel 1c                   SELECT
048200060403      * IMMISSIONE
048300060403w   1c                   WHEN      NOT %found(TNTBE01L)
048400060403     c                   eval      *in01   = *on
048500060403     c                   eval      T1opz   = $Opz(01)
048600060403      * MODIFICA
048700060403w   1c                   WHEN          %found(TNTBE01L)
048800060403     c                             and TBEatb  = *blanks
048900060403     c                   eval      *in02   = *on
049000060403     c                   eval      T1opz   = $Opz(02)
049100060403      * RIPRISTINO
049200060403w   1c                   WHEN          %found(TNTBE01L)
049300060403     c                             and TBEatb <> *blanks
049400060403     c                   eval      *in04   = *on
049500060403     c                   eval      T1opz   = $Opz(06)
049600060403e   1c                   ENDSL
049700060403      *
049800060403if  1c                   if        %found(TNTBE01L)
049900090324     c                   movel     TBEuni        dGPP
050000090324     c                   eval      V2Crg1  = §GPPrg1
050001090324     c                   eval      V2Crg2  = §GPPrg2
050002090324     c                   eval      V2Crg3  = §GPPrg3
050003090324     c                   eval      V2Ccch  = §GPPcch
050004090325     c                   eval      V2Cevf  = §GPPevf
050005090325     c                   clear                   TIBS02DS
050006090325     C                   MOVEL     'C'           t02mod
050007090325     C                   MOVEL     knsif         t02sif
050008090325     C                   MOVEL     'CCH'         t02cod
050009090325     C                   MOVEL(P)  v2ccch        t02ke1
050010090325      *
050011090325     C                   CALL      'TIBS02R'
050012090325     C                   PARM                    KPJBA
050013090325     C                   PARM                    TIBS02DS
050014090325      *
050015090325     C                   if        t02err = *BLANKS
050016090325     C                   MOVEL     T02UNI        DCCH
050017090325     c                   eval      v2dcch = §cchdesc
050018090325     c                   endif
050900060403e   1c                   endif
051000060403      *
051100060403     c                   ENDSR
051200060403
051300060403      *---------------------------------------------------------------*
051400060403      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
051500060403      *---------------------------------------------------------------*
051600060403     c     F12V2         BEGSR
051700060403      *
051800060403     c                   eval      $TipVid = '1'
051900060403      *
052000060403     c                   ENDSR
052100060403
052200060403      *---------------------------------------------------------------*
052300060403      * CTRV2  - Controllo e decodifica seconda videata               *
052400060403      *---------------------------------------------------------------*
052500060403     c     CtrV2         BEGSR
052600060403      *
052700060403     c                   movea     *zeros        *in(50)
052800060403      *
052900060403sel 1c                   SELECT
053000060403      *
053100090324      * Codici raggruppamento anomalie
053200090324     c                   WHEN      V2Crg1 =' ' and V2Crg2 =' ' and V2Crg3 =' '
053300060403     c                   seton                                        285190
053400060403     c                   eval      V1Dmsg  = $Msg(3)
061201090324      *
061202090324      * Causale chiusura non inserita
061203090324     c                   WHEN      V2Ccch ='  '
061204090324     c                   seton                                        285290
061205090324     c                   eval      V1Dmsg  = $Msg(5)
061206090324      *
061207090324      * Ricerca Causale di chiusura
061208090324     c                   WHEN      %scan('?' : V2Ccch) > *zero
061209090324     c                   seton                                        9052
061210090324     c                   clear                   tibs02ds
061212090324     C                   MOVEL     'R'           t02mod
061213090324     C                   MOVEL     knsif         t02sif
061214090324     C                   MOVEL     'CCH'         t02cod
061215090324      *
061216090324     C                   CALL      'TIBS02R'
061217090324     C                   PARM                    KPJBA
061218090324     C                   PARM                    TIBS02DS
061219090324      *
061220090324     C                   IF        T02err = *BLANKS
061222090324     C                   movel     T02ke1        v2ccch
061223090325     c                   movel     t02uni        dcch
061224090325     c                   eval      v2dcch = §cchdesc
061225090324     c                   endif
061226090324     c
061227090324      *
061228090324      * Causale chiusura inserita controllo
061229090324     c                   WHEN      V2Ccch <> '  '
061232090324     c                   clear                   TIBS02DS
061233090324     C                   MOVEL     'C'           t02mod
061234090324     C                   MOVEL     knsif         t02sif
061235090324     C                   MOVEL     'CCH'         t02cod
061236090324     C                   MOVEL(P)  v2ccch        t02ke1
061238090324      *
061239090324     C                   CALL      'TIBS02R'
061240090324     C                   PARM                    KPJBA
061241090324     C                   PARM                    TIBS02DS
061242090324      *
061243090324     C                   if        t02err = *BLANKS
061244090324     C                   MOVEL     T02UNI        DCCH
061245090325     c                   eval      v2dcch = §cchdesc
061246090324     c                   else
061247090324     c                   seton                                        285290
061248090324     c                   eval      V1Dmsg  = $Msg(6)
061249090324     C                   endif
061250090324      *
061251090324     C                   If        §cchpaga <> ' '
061253090324     c                   seton                                        285290
061254090324     c                   eval      V1Dmsg  = $Msg(7)
061255090324     C                   endif
061256090324      *
061300060403e   1c                   ENDSL
061400060403      *
061500060403     c                   ENDSR
061600060403
071900060403      *---------------------------------------------------------------*
072000060403      * CHNTBE * Aggancio tabella                                     *
072100060403      *---------------------------------------------------------------*
072200060403     c     ChnTBE        BEGSR
072300060403      *
072400090324     c                   movel     'GPP'         TBEcod
072500080212     c                   move(p)   V2Cksc        TBEke1
072600060403     c                   clear                   TBEke2
072700060403     c                   clear                   TBElin
072800060403     c                   movel     KNSIF         TBEsif
072900060403     c     K05TBE01      chain     TNTBE01L
073000060403      * Se non ho trovato il record con il sistema informativo
073100060403      * che ho in linea, lo abblenco
073200060403if  1c                   if        not %found(TNTBE01L)
073300060403     c                   clear                   TBEsif
073400060403     c     K05TBE01      chain     TNTBE01L
073500060403e   1c                   endif
073600060403      *
073700060403     c                   ENDSR
073800060403
073900060403      *---------------------------------------------------------------*
074000060403      * AGGTBE * Aggiornamento tabella                                *
074100060403      *---------------------------------------------------------------*
074200060403     c     AggTBE        BEGSR
074300060403      *
074400060403sel 1c                   SELECT
074500060403      *
074600060403      * F5=Ripristino
074700060403w   1c                   WHEN      wTasto='05'  and  *in04
074800060403     c                   clear                   TBEatb
074900080215     c                   movel     'S'           TBEftt
075000060403     c                   clear                   TBEftr
075100060403     c                   UPDATE    TNTBE000
075200060403      *
075300060403      * F6=Conferma
075400060403w   1c                   WHEN      wTasto='06'
075500060403     c                   exsr      RieTBE
075700060403sel 2c                   select
075800060403      *   Immissione
075900060403w   2c                   when      *in01
076000060403     c                   clear                   TBEflt
076100060403     c                   clear                   TBEdtr
076200060403     c                   WRITE     TNTBE000                             22
076300060403      *   Modifica / Ripristino
076400060403w   2c                   when      *in02   or    *in04
076500060403     c                   UPDATE    TNTBE000
076600060403e   2c                   endsl
076700060403      *
076800060403      * F16=Annullamento
076900060403w   1c                   WHEN      wTasto='16'  and  not *in04
077000060403     c                   movel     'A'           TBEatb
077100080215     c                   movel     'S'           TBEftt
077200060403     c                   clear                   TBEftr
077300060403     c                   UPDATE    TNTBE000
077400060403      *
077500060403e   1c                   ENDSL
077600060403      *
077700060403      * Torno alla prima videata che carico come da inizio pgm
077800060403     c                   movel     '1'           $TipVid
077900060403     c                   movel     *on           $InzV1
078000060403     c                   movel     *on           $InzV2
078200060403     c
078300060403     c                   ENDSR
078400060403
078500060403      *---------------------------------------------------------------*
078600060403      * RIETBE * Riempimento dati tabella                             *
078700060403      *---------------------------------------------------------------*
078800060403     c     RieTBE        BEGSR
078900060403      *
079000060403     c                   clear                   TBEatb
079100060403     c                   if        TBXsif <> *blanks
079200060403     c                   movel     KNSIF         TBEsif
079300060403     c                   else
079400060403     c                   clear                   TBEsif
079500060403     c                   endif
079600060403     c                   movel     TBXapl        TBEapl
079700090324     c                   movel     'GPP'         TBEcod
079800080212     c                   move      V2Cksc        TBEke1
079900060403     c                   clear                   TBEke2
080000060403      *
080100090324     c                   clear                   dGPP
080200090324     c                   eval      §GPPrg1 = V2Crg1
080201090324     c                   eval      §GPPrg2 = V2Crg2
080202090324     c                   eval      §GPPrg3 = V2Crg3
080203090324     c                   eval      §GPPcch = V2Ccch
080204090325     c                   eval      §GPPevf = V2Cevf
081100090324     c                   movel(p)  dGPP          TBEuni
081200060403      *
081300080215     c                   movel     'S'           TBEftt
081400060403     c                   clear                   TBEflt
081500060403     c                   clear                   TBEftr
081600060403     c                   clear                   TBEdtr
081700060403      *
081800060403     c                   ENDSR
081801080226      *****************************************************************
081802080226      *   carico tutti gli elementi della tabella
081803080226      *****************************************************************
081804080226     c     CARICA        BEGSR
081805080226     c                   clear                   NRR               4 0
081806080226     c
081807080226     c                   eval      *in35='1'
081808090324     c                   write     TB72ct
081809080226     c                   eval      *in35='0'
081810080226     c
081811080226     c     ktbe2         chain     tntbe02l                           30
081812080226     c                   dow       not *in30
081813080226     c                   if        tbeatb=' '
081814080228     c                   setoff                                       48
081815080228     c                   else
081816080228     c                   seton                                        48
081817080228     c                   endif
081818080226      *
081819080228     c                   move      tbeke1        vscksc
081820080226     c
081821080226     C                   clear                   TIBS69DS
081822080226     c                   z-add     vscksc        I69kac
081823080226      *
081824080226     C                   CALL      'TIBS69R'
081825080226     C                   PARM                    tibs69DS
081826080226     C                   PARM                    DS_cnaco
081827080226     C                   PARM                    DS_cnind
081828080226     C                   PARM                    DS_cnclp
081829080226     C                   PARM                    DS_fncls
081830080226    2c                   if        o69err=' '
081831080226     c                   movel     acorag        vsdksc
081832080226    2c                   endif
081833080226      * recupero l'importo
081834090324     c                   movel     tbeuni        dGPP
081835090324     c                   move      §GPPrg1       vscrg1
081836090324     c                   move      §GPPrg2       vscrg2
081837090324     c                   move      §GPPrg3       vscrg3
081838090324     c                   move      §GPPcch       vsccch
081839090325     c                   If        §GPPevf = 'S'
081840090325     c                   eval      vscevf = 'SI'
081841090325     c                   else
081842090325     c                   clear                   vsccch
081843090325     c                   endif
081844080226
081845080226     c                   clear                   vscopz
081846080226
081847080226     c                   add       1             nrr
081848090324     c                   write     TB72sf
081849080226     c
081850080226     c     ktbe2         reade     tntbe02l                               30
081851080226     c                   enddo
081852080226     c
081853080226     c
081854080226      * Se ci sono dei record visualizzo il sfl
081855080226     c                   if        nrr>0
081856080226     c                   eval      nrr=1
081857080226     c                   eval      *in36='1'
081858080226     c                   endif
081859080226     c
081860080226     c                   ENDSR
081900060403
082000060403** - $Opz ----*
082100060403  Inserimento
082200060403    Modifica
082300060403     Copia
082400060403  Annullamento
082500060403Visualizzazione
082600060403   ANNULLATO
082700060403** - $Msg -------------------------------------------------------------------*
082800060403Immettere il codice cliente                                                    01
082900060403Codice cliente errato                                                          02
083000090324Immettere almeno un raggruppamento anomalia                                    03
083100080228Password errata                                                                04
083200090324Inserire la causale di chiusura                                                05
083300090324Causale di chiusura errata                                                     06
083400090324Causale di chiusura errata perchè usata per pagamenti danni                    07
