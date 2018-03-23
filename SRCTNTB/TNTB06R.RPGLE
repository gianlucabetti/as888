000100060215      *---------------------------------------------------------------*
000200060215      *?Gestione tabella "GEI" : importi gestionale                  ?*
000300060215      *---------------------------------------------------------------*
000400060215
000500060215     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600980909
000700060215      *---------------------------------------------------------------*
000800060215      *   A R C H I V I                                               *
000900060215      *---------------------------------------------------------------*
001000060215
001100060215     fTNTBE01L  Uf A e           k disk
001200060215      *
001300060215     fTNTB06D   cf   e             workstn
001400060215
001500060215      *---------------------------------------------------------------*
001600060215      *   C O S T A N T I                                             *
001700060215      *---------------------------------------------------------------*
001800060215
001900060215      * Codice tabella in esame
002000060215     d C_tab           c                   const('GEI')
002100060215
002200060215      *---------------------------------------------------------------*
002300060215      *   S C H I E R E                                               *
002400060215      *---------------------------------------------------------------*
002500060215
002600060215      * Decodifica status record
002700060217     d $Sts            s             10    dim( 3) ctdata perrcd(1)
002800060216      * Messaggi a video
002900060216     d $Msg            s             78    dim( 3) ctdata perrcd(1)
003000060215
003100060215      *---------------------------------------------------------------*
003200060215      *   S T R U T T U R E   D A T I                                 *
003300060215      *---------------------------------------------------------------*
003400060215
003500060215      * Parametri
003600060215     d KPJBA         e ds
003700981102      *
003800060215     d TIBS34ds      e ds
003900060215     d dDatiUte      e ds
004000060215     d AZUTEds       e ds                  extname(AZUTE00F)
004100060215      *
004200060215      * Tabella GEI = Gestionale: importi vari
004300060215     d dGEI          e ds                  inz
004400060215      *
004500060215      * - Controllo Tassazione
004600060215     d FNLV16ds      e ds                  inz
004700060215     d dTASV         e ds                  inz
004800060215      *
004900060215      * Tracciato record file TNTBE00F
005000060215     d TNTBEds       e ds                  inz  extname(TNTBE00F)
005100060215     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
005200060215     d                                          prefix(TBX:3)
005300060215      *
005400060215     d Status         sds
005500060215     d  V1Tpgm           *proc
005600060215      *
005700060215     d WLBdat          ds                  inz
005800060215     d  G08dat                 1      8  0 inz
005900060215     d  G08inv                 9     16  0 inz
006000060215     d  G08err                17     17    inz('3')
006100060215     d  G08tgi                18     22  0 inz
006200060215
006300060215      *---------------------------------------------------------------*
006400060215      *   V A R I A B I L I                                           *
006500060215      *---------------------------------------------------------------*
006600060215
006700060215     d $Called         s              1a   inz(*off)
006800060215     d $Fine           s              1a   inz(*off)
006900060215     d $Found          s              1a   inz(*off)
007000060215     d $InzD01         s              1a   inz(*on)
007100060215     d $InzD02         s              1a   inz(*on)
007200060215      *
007300060215     d $Video          s              1a   inz('1')
007400060215
007500060215      *---------------------------------------------------------------*
007600060215      *  Riepilogo indicatori utilizzati:                             *
007700060215      *  --------------------------------                             *
007800060215      *  01 - Record inesistente (inserimento)                        *
007900060215      *  02 - Record esistente   (modifica)                           *
008000060215      *  04 - Record annullato   (ripristino)                         *
008100060215      *  22 - Errori in scrittura record (WRITE)                      *
008200060215      *  28 - Visualizzazione messaggio di errore                     *
008300060215      *  90 - Errore generico                                         *
008400060215      *---------------------------------------------------------------*
008500060215
008600060215      *---------------------------------------------------------------*
008700060215      *   M A I N   L I N E                                           *
008800060215      *---------------------------------------------------------------*
008900060215
009000060215      * Ricezione parametri
009100060215     c     *entry        plist
009200060215     c                   parm                    KPJBA
009300060215      *
009400060215      * Operazioni iniziali
009500060215     c                   exsr      RoutInz
009600060215      *
009700060215      * Gestione video
009800060215     c                   dow       $Fine   = *off
009900060215     c     $Video        caseq     '1'           GesD01
010000060215     c     $Video        caseq     '2'           GesD02
010100060215     c                   endcs
010200060215     c                   enddo
010300060215      *
010400060215      * Fine
010500060215     c                   movel     *on           *inLR
010600060215      *
010700060215      *---------------------------------------------------------------*
010800060215      * Operazioni Iniziali                                           *
010900060215      *---------------------------------------------------------------*
011000060215     c     RoutInz       BEGSR
011100060215      *
011200060215      * Definizioni chiavi di accesso
011300060215      * - TNTBE01L
011400060215     c     K05TBE01      klist
011500060215     c                   kfld                    TBEcod
011600060215     c                   kfld                    TBEke1
011700060215     c                   kfld                    TBEke2
011800060215     c                   kfld                    TBElin
011900060215     c                   kfld                    TBEsif
012000060215     c     K02TBE01      klist
012100060215     c                   kfld                    TBEcod
012200060215     c                   kfld                    TBEke1
012300060215      *
012400060215      * Reperisco dati job
012500060215     c                   exsr      DatiJob
012600060215      *
012700060215      * Aggancio dati generali della tabella in esame
012800060215     c                   clear                   TBEcod
012900060215     c                   movel     *zeros        TBEke1
013000060215     c                   move      C_tab         TBEke1
013100060215     c                   clear                   TBEke2
013200060215     c                   clear                   TBElin
013300060215     c                   movel     KNSIF         TBEsif
013400060215     c     K05TBE01      chain     TNTBE000
013500060215     c                   if        not %found(TNTBE01L)
013600060215     c                   clear                   TBEsif
013700060215     c     K05TBE01      chain     TNTBE000
013800060215     c                   endif
013900060215     c                   if        %found(TNTBE01L)
014000060215     c                   movel     TNTBEds       xTNTBEds
014100060215     c                   else
014200060215     c                   clear                   xTNTBEds
014300060215     c                   endif
014400060215      *
014500060215      * Verifico se ho ricevuto parametri
014600060215if  1c                   if        %subst(KPJBU:1:8) = *blanks
014700060215     c                   leavesr
014800060215e   1c                   endif
014900060215      *
015000060215      * Controllo i parametri ricevuti
015100060215     c                   eval      $Called = *on
015200060215     c                   clear                   TN06D01
015300060215     c                   eval      V1Cdiv  = %subst(KPJBU:1:3)
015400060215     c                   exsr      CtrD01
015500060215if  1c                   if        *in28
015600060215     c                   movel(p)  V1Dmsg        KPJBU
015700060215     c                   eval      $Fine   = *on
015800060215     c                   leavesr
015900060215x   1c                   else
016000060215     c                   eval      $Video  = '2'
016100060215e   1c                   endif
016200060215      *
016300060215     c                   ENDSR
016400060215      *
016500060215      *---------------------------------------------------------------*
016600060215      * Reperimento Dati del job (Utente/Operativi)                   *
016700060215      *---------------------------------------------------------------*
016800060215     c     DatiJob       BEGSR
016900060215      *
017000060215      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
017100060215     c     *dtaara       define    §azute        azuteds
017200060215     c     *dtaara       define    §datiute      ddatiute
017300060215      *
017400060215     c                   clear                   AzUteDs
017500060215     c                   clear                   DDatiUte
017600060215     c                   clear                   Tibs34Ds
017700060215     c                   in(E)     *dtaara
017800060215if  1c                   if        %Error  or  RSUT = *blanks
017900060215     c                   call      'TIBS34R'
018000060215     c                   parm                    Tibs34Ds
018100060215     c                   in        *dtaara
018200060215e   1c                   endif
018300060215      *-- Verifica errori e autorità profilo
018400060215sel 1c                   SELECT
018500060215      *-- controllo se ho errori nei dati utente
018600060215      *--   nel qual caso NON risulta un profilo abilitato
018700060215w   1c                   WHEN      DUTerr  = 'E'
018800060215     c                   eval      $Fine   = *on
018900060215      *
019000060215      *-- CONTROLLO AUTORITA'
019100060215      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
019200060215      *-- se il chiamante non richiede autorità specifica verificare
019300060215      *--   quella del profilo
019400060215      *-- se il chiamante richiede autorità specifica verificarla,
019500060215      *--  se è blank verificare quella del profilo
019600060215      *
019700060215      * se UTEAUT = *BLANK non siamo in sede
019800060215w   1c                   WHEN      UTEaut  = *blank
019900060215      *
020000060215x   1c                   OTHER
020100060215      *
020200060215e   1c                   ENDSL
020300060215      *
020400060215     c                   ENDSR
020500060215      *
020600060215      *---------------------------------------------------------------*
020700060215      * Gestione videata D01                                          *
020800060215      *---------------------------------------------------------------*
020900060215     c     GesD01        BEGSR
021000060215      *
021100060215      * Inizializzo la videata
021200060215if  1c                   if        $InzD01 = *on
021300060215     c                   exsr      InzD01
021400060215     c                   movel     *off          $InzD01
021500060215e   1c                   endif
021600060215      *
021700060215      * Emetto la testata
021800060215     c                   clear                   V1Ttxt
021900060215     c                   write     TN06T01
022000060215      *
022100060215      * Emetto la videata
022200060215     c                   exfmt     TN06D01
022300060215     c                   setoff                                       28  90
022400060215     c                   movea     *zeros        *in(50)
022500060215     c                   clear                   V1Dmsg
022600060215      *
022700060215     c                   SELECT
022800060215      *
022900060215      * F3=Fine
023000060215w   1c                   WHEN      *inKC
023100060215     c                   exsr      F03D01
023200060215     c                   leavesr
023300060215      *
023400060215      * Controllo dati immessi a video
023500060215x   1c                   OTHER
023600060215     c                   exsr      CtrD01
023700060215if  2c                   if        *in90
023800060215     c                   leavesr
023900060215e   2c                   endif
024000060215      * - Passaggio alla videata successiva
024100060215     c                   eval      $InzD02 = *on
024200060215     c                   eval      $Video  = '2'
024300060215      *
024400060215e   1c                   ENDSL
024500060215      *
024600060215     c                   ENDSR
024700060215      *
024800060215      *---------------------------------------------------------------*
024900060215      * Inizializzazione videata D01                                  *
025000060215      *---------------------------------------------------------------*
025100060215     c     InzD01        BEGSR
025200060215      *
025300060215     c                   clear                   TN06D01
025400060215      *
025500060215      * - 40 = Pgm richiamato
025600060215     c***                eval      *in40 = ($Called = *on)
025700060215      *
025800060215      * Imposto i dati di default
025900060215     c                   move      '?'           V1Cdiv
026000060215      *
026100060215     c                   ENDSR
026200060215      *
026300060215      *---------------------------------------------------------------*
026400060215      * Gestione tasto funzionale F03 da videata D01                  *
026500060215      *---------------------------------------------------------------*
026600060215     c     F03D01        BEGSR
026700060215      *
026800060215      * Chiudo il programma
026900060215     c                   eval      $Fine   = *on
027000060215      *
027100060215     c                   ENDSR
027200060215      *
027300060215      *---------------------------------------------------------------*
027400060215      * Controllo dati immessi in videata D01                         *
027500060215      *---------------------------------------------------------------*
027600060215     c     CtrD01        BEGSR
027700060215      *
027800060215     c                   movea     *zeros        *in(50)
027900060215      *
028000060215      * Controllo Divisa
028100060215     c                   clear                   FNLV16ds
028200060215     c                   clear                   dTASV
028300060215     c                   eval      D17div =  V1Cdiv
028400060215     c                   eval      D17tbo =  '0'
028500060215     c                   eval      D17fim =  'D'
028600060215     c                   call      'FNLV16R'
028700060215     c                   parm                    FNLV16ds
028800060215     c                   parm                    dTASV
028900060215     c                   eval      V1Cdiv =  D17div
029000060215      *
029100060215if  1c                   if        O17err <> *blanks
029200060215     c                   eval      *in90  =  *on
029300060215e   1c                   endif
029400060215if  1c                   if        O17msg <> *blanks
029500060215     c                   movel     O17msg        V1Dmsg
029600060215     c                   seton                                        28  90
029700060215e   1c                   endif
029800060215      *
029900060215     c                   ENDSR
030000060215      *
030100060215      *---------------------------------------------------------------*
030200060215      * Gestione videata D02                                          *
030300060215      *---------------------------------------------------------------*
030400060215     c     GesD02        BEGSR
030500060215      *
030600060215      * Inizializzo la videata
030700060215if  1c                   if        $InzD02 = *on
030800060215     c                   exsr      InzD02
030900060215     c                   movel     *off          $InzD02
031000060215e   1c                   endif
031100060215      *
031200060215      * Emetto la testata
031300060215     c                   write     TN06T01
031400060215      *
031500060215      * Emetto la videata
031600060215     c                   if        NOT *in04
031700060215     c                   exfmt     TN06D02
031800060215     c                   else
031900060215     c                   write     TN06D02
032000060215     c                   exfmt     PROTECT
032100060215     c                   endif
032200060215     c                   setoff                                       28  90
032300060215     c                   movea     *zeros        *in(50)
032400060215     c                   clear                   V1Dmsg
032500060215      *
032600060215     c                   SELECT
032700060215      *
032800060215      * F3=Fine
032900060215w   1c                   WHEN      *inKC
033000060215     c                   exsr      F03D01
033100060217      *
033200060217      * F9=Interrogazione storico importi limite risarcibili
033300060217w   1c                   WHEN      *inKI
033400060217     c                   exsr      F09D02
033500060215      *
033600060215      * F12=Ritorno
033700060215w   1c                   WHEN      *inKL
033800060215     c                   exsr      F12D02
033900060215      *
034000060215      * F16=Annullamento
034100060215w   1c                   WHEN      *inKQ
034200060215     c                   exsr      F16D02
034300060215      *
034400060215      * Controllo dati immessi a video
034500060215x   1c                   OTHER
034600060215     c                   exsr      CtrD02
034700060215if  2c                   if        *in90
034800060215     c                   leavesr
034900060215e   2c                   endif
035000060215      *
035100060215sel 2c                   Select
035200060215      * F5=Ripristino
035300060215if  2c                   When      *inKE
035400060215     c                   exsr      F05D02
035500060215      * F6=Aggiornamento
035600060215if  2c                   When      *inKF
035700060215     c                   exsr      F06D02
035800060215e   2c                   EndSl
035900060215      *
036000060215e   1c                   ENDSL
036100060215      *
036200060215     c                   ENDSR
036300060215      *
036400060215      *---------------------------------------------------------------*
036500060215      * Inizializzazione videata D02                                  *
036600060215      *---------------------------------------------------------------*
036700060215     c     InzD02        BEGSR
036800060215      *
036900060216     c                   clear                   TN06D02
037000060215      *
037100060215      * Reperimento record da TNTBE01L
037200060215     c                   clear                   dGEI
037300060215     c                   exsr      Rep_TNTBE
037400060215     c                   if        $Found  = *on
037500060215     c                   movel     TBEuni        dGEI
037600060215     c                   endif
037700060215      *
037800060215      * - 01 = Record inesistente (inserimento)
037900060215     c                   eval      *in01   = ($Found = *off)
038000060215      * - 02 = Record esistente   (modifica)
038100060215     c                   eval      *in02   = ($Found = *on)
038200060215      * - 04 = Record annullato   (ripristino)
038300060215     c                   eval      *in04   = ($Found = *on  and
038400060215     c                                        TBEatb = 'A')
038500060215      * - 40 = Pgm richiamato
038600060215     c***                eval      *in40   = ($Called = *on)
038700060215      *
038800060215     c                   select
038900060215     c                   when      *in04
039000060215     c                   eval      V1Ttxt  = $Sts(3)
039100060216     c                                     + '  DIVISA ' + V1Cdiv
039200060215     c                   when      *in02
039300060215     c                   eval      V1Ttxt  = $Sts(2)
039400060216     c                                     + '  DIVISA ' + V1Cdiv
039500060215     c                   when      *in01
039600060215     c                   eval      V1Ttxt  = $Sts(1)
039700060216     c                                     + '  DIVISA ' + V1Cdiv
039800060215     c                   endsl
039900060215      *
040000060215     c                   z-add     §GEimb        V2Cimb
040100060215     c                   z-add     §GEibl        V2Cibl
040200060215     c                   z-add     §GEimf        V2Cimf
040300060215     c                   z-add     §GEimv        V2Cimv
040400060215     c                   z-add     §GElrp        V2Clrp
040500060217     c                   movel     §GEllr        V2Cllr
040600060217     c                   reset                   WLBdat
040700060217     c                   eval      G08inv  = §GEdlr
040800060217     c                   call      'XSRDA8'
040900060217     c                   parm                    WLBdat
041000060217if  1c                   if        G08err  = *off
041100060217     c                   eval      V2Cdlr  = G08dat
041200060217e   1c                   endif
041300060215     c                   z-add     §GEavl        V2Cavl
041400060215     c                   movel     §GEaed        V2Caed
041500060215     c                   z-add     §GEmia        V2Cmia
041600060215     c                   z-add     §GEmxa        V2Cmxa
041700060215     c                   z-add     §GEmao        V2Cmao
041800060215     c                   z-add     §GEach        V2Cach
041900060215     c                   z-add     §GEpin        V2Cpin
042000060503     c                   z-add     §GEtqg        V2Ctqg
042001060511     c                   z-add     §GEtqgt       V2Ctqgt
042002060503     c                   z-add     §GEtvr        V2Ctvr
042003060503     c                   z-add     §GEtcs        V2Ctcs
042100060215     c                   z-add     §GEdfc        V2Cdfc
042200060215     c                   z-add     §GEcmr        V2Ccmr
042300060215     c                   z-add     §GEccm        V2Cccm
042400060215     c                   z-add     §GEttr        V2Cttr
042500060215     c                   z-add     §GEdrf        V2Cdrf
042600060215      *
042700060215     c                   ENDSR
042800060215      *
042900060215      *---------------------------------------------------------------*
043000060215      * Gestione tasto funzionale F05 da videata D02                  *
043100060215      *---------------------------------------------------------------*
043200060215     c     F05D02        BEGSR
043300060215      *
043400060215      * Ripristino record annullato
043500060215     c                   clear                   TBEatb
043600060215      *
043700060215     c                   exsr      Agg_TNTBE
043800060215      *
043900060215     c                   ENDSR
044000060215      *
044100060215      *---------------------------------------------------------------*
044200060215      * Gestione tasto funzionale F16 da videata D02                  *
044300060215      *---------------------------------------------------------------*
044400060215     c     F16D02        BEGSR
044500060215      *
044600060215      * Annullamento record
044700060215     c                   eval      TBEatb  = 'A'
044800060215      *
044900060215     c                   exsr      F06D02
045000060215      *
045100060215     c                   ENDSR
045200060215      *
045300060215      *---------------------------------------------------------------*
045400060215      * Gestione tasto funzionale F06 da videata D02                  *
045500060215      *---------------------------------------------------------------*
045600060215     c     F06D02        BEGSR
045700060215      *
045800060215      * F16=Annullamento
045900060215     c*** già fatto:     if        *inKQ
046000060215     c*** già fatto:     movel     'A'           TBEatb
046100060215     c*** già fatto:     endif
046200060215      *
046300060215      * F5=Ripristino
046400060215     c*** già fatto:     if        *inKE
046500060215     c*** già fatto:     clear                   TBEatb
046600060215     c*** già fatto:     endif
046700060215      *
046800060215     c                   exsr      RieUNI
046900060215     c                   movel(p)  dGEI          TBEuni
047000060215      *
047100060215     c                   exsr      Agg_TNTBE
047200060215      *
047300060215     c                   ENDSR
047400060217      *
047500060217      *---------------------------------------------------------------*
047600060217      * Gestione tasto funzionale F09 da videata D02                  *
047700060217      *---------------------------------------------------------------*
047800060217     c     F09D02        BEGSR
047900060217      *
048000060217      * Visualizzazione storico importi limite risacibili
048100060217     c                   movel(p)  V1Cdiv        KPJBU
048200060217     c                   call      'TNTB48R'
048300060217     c                   parm                    KPJBA
048400060217      *
048500060217     c                   if        KPJBU <> *blanks
048600060217     c                   movel     KPJBU         V1Dmsg
048700060217     c                   seton                                        285290
048800060217     c                   leavesr
048900060217     c                   endif
049000060217      *
049100060217     c                   ENDSR
049200060215      *
049300060215      *---------------------------------------------------------------*
049400060215      * Gestione tasto funzionale F12 da videata D02                  *
049500060215      *---------------------------------------------------------------*
049600060215     c     F12D02        BEGSR
049700060215      *
049800060215     c                   UNLOCK    TNTBE01L
049900060215      *
050000060215     c                   if        $Called = *off
050100060215      * - Ritorno alla videata precedente
050200060215     c                   eval      $Video  = '1'
050300060215     c                   else
050400060215      * - Uscita dal pgm
050500060215     c                   exsr      F03D01
050600060215     c                   endif
050700060215      *
050800060215     c                   ENDSR
050900060215      *
051000060215      *---------------------------------------------------------------*
051100060215      * Controllo dati immessi in videata D02                         *
051200060215      *---------------------------------------------------------------*
051300060215     c     CtrD02        BEGSR
051400060215      *
051500060215     c                   movea     *zeros        *in(50)
051600060216      *
051700060216sel 1c                   select
051800060216      *
051900060216      * Numero legge attualmente valida
052000060216      * - obbligatorio se immessa data decorrenza
052100060217w   1c                   when          V2Cllr =  *blanks
052200060217     c                             and V2Cdlr <> *zeros
052300060216     c                   seton                                        285190
052400060216     c                   movel     $Msg(1)       V1Dmsg
052500060216      *
052600060216      * Data decorrenza numero legge attualmente valida
052700060216      * - obbligatoria se immesso numero legge
052800060217w   1c                   when          V2Cllr <> *blanks
052900060217     c                             and V2Cdlr =  *zeros
053000060216     c                   seton                                        285290
053100060216     c                   movel     $Msg(2)       V1Dmsg
053200060216      * - formato errato
053300060217w   1c                   when          V2Cdlr <> *zeros
053400060216     c                   clear                   WLBdat
053500060217     c                   eval      G08dat  = V2Cdlr
053600060216     c                   call      'XSRDA8'
053700060216     c                   parm                    WLBdat
053800060216if  2c                   if        G08err  = *off
053900060217     c                   eval      V2Cdlr  = G08dat
054000060216x   2c                   else
054100060216     c                   seton                                        285290
054200060216     c                   movel     $Msg(3)       V1Dmsg
054300060216e   2c                   endif
054400060216      *
054500060216e   1c                   endsl
054600060216      *
054700060216     c                   ENDSR
054800060215      *
054900060215      *---------------------------------------------------------------*
055000060215      * Reperimento record da TNTBE01L                                *
055100060215      *---------------------------------------------------------------*
055200060215     c     Rep_TNTBE     BEGSR
055300060215      *
055400060215     c                   reset                   $Found
055500060216      *
055600060215     c                   movel     C_Tab         TBEcod
055700060215     c                   movel(p)  V1Cdiv        TBEke1
055800060215     c                   clear                   TBEke2
055900060215     c                   movel     TBXlin        TBElin
056000060215     c                   movel     TBXsif        TBEsif
056100060215     c     K05TBE01      chain     TNTBE000
056200060215      *
056300060215      * Record non trovato per il codice KE1
056400060215     c                   if        NOT %found(TNTBE01L)
056500060215     c                   clear                   TNTBE000
056600060215     c                   eval      TBEsif  = TBXsif
056700060215     c                   eval      TBElin  = TBXlin
056800060215     c                   eval      TBEapl  = TBXapl
056900060215     c                   eval      TBEcod  = C_tab
057000060215     c                   movel(p)  V1Cdiv        TBEke1
057100060215     c                   clear                   TBEke2
057200060215     c                   leavesr
057300060215     c                   endif
057400060215      *
057500060215     c                   eval      $Found  = *on
057600060215      *
057700060215     c                   ENDSR
057800060216      *
057900060216      *---------------------------------------------------------------*
058000060216      * Impostazione campi nella DS x campo TBEUNI del file TNTBE01L  *
058100060216      *---------------------------------------------------------------*
058200060216     c     RieUNI        BEGSR
058300060216      *
058400060216     c                   clear                   dGEI
058500060216      *
058600060216     c                   eval      §GEimb = V2Cimb
058700060216     c                   eval      §GEibl = V2Cibl
058800060216     c                   eval      §GEimf = V2Cimf
058900060216     c                   eval      §GEimv = V2Cimv
059000060216     c                   eval      §GElrp = V2Clrp
059100060216     c                   clear                   WLBdat
059200060217     c                   eval      G08dat  = V2Cdlr
059300060216     c                   call      'XSRDA8'
059400060216     c                   parm                    WLBdat
059500060216if  1c                   if        G08err  = *off
059600060216     c                   eval      §GEdlr  = G08inv
059700060216e   1c                   endif
059800060217     c                   eval      §GEllr = V2Cllr
059900060216     c                   eval      §GEavl = V2Cavl
060000060216     c                   eval      §GEaed = V2Caed
060100060216     c                   eval      §GEmia = V2Cmia
060200060216     c                   eval      §GEmxa = V2Cmxa
060300060216     c                   eval      §GEmao = V2Cmao
060400060216     c                   eval      §GEach = V2Cach
060500060216     c                   eval      §GEpin = V2Cpin
060600060503     c                   eval      §GEtvr = V2Ctvr
060601060503     c                   eval      §GEtqg = V2Ctqg
060602060511     c                   eval      §GEtqgt= V2Ctqgt
060603060503     c                   eval      §GEtcs = V2Ctcs
060700060216     c                   eval      §GEdfc = V2Cdfc
060800060216     c                   eval      §GEcmr = V2Ccmr
060900060216     c                   eval      §GEccm = V2Cccm
061000060216     c                   eval      §GEdrf = V2Cdrf
061100060216     c                   eval      §GEttr = V2Cttr
061200060216      *
061300060216     c                   movel(p)  Dgei          TBEuni
061400060216      *
061500060216     c                   ENDSR
061600060215      *
061700060215      *---------------------------------------------------------------*
061800060215      * Aggiornamento record di TNTBE01L                              *
061900060215      *---------------------------------------------------------------*
062000060215     c     Agg_TNTBE     BEGSR
062100060215      *
062200060215     c                   eval      TBEftt  = 'S'
062300060215     c                   clear                   TBEftr
062400060215      *
062500060215      * Inserimento nuovo record
062600060215if  1c                   IF        $Found  = *off
062700060215      *
062800060215     c*** già così:      clear                   TBEatb
062900060215     c*** già impostato: eval      TBEsif  = TBXsif
063000060215     c*** già impostato: eval      TBElin  = TBXlin
063100060215     c*** già impostato: eval      TBEapl  = TBXapl
063200060215     c*** già impostato: eval      TBEcod  = C_tab
063300060215     c*** già impostato: movel(p)  V1Cdiv        TBEke1
063400060215     c*** già impostato: clear                   TBEke2
063500060215     c                   clear                   TBEflt
063600060215     c*** già impostato: clear                   TBEftr
063700060215     c                   clear                   TBEdtr
063800060215      *
063900060215     c                   WRITE     TNTBE000
064000060215      *
064100060215      * Modifica record già esistente
064200060215x   1c                   ELSE
064300060215      *
064400060215     c                   UPDATE    TNTBE000
064500060215      *
064600060215e   1c                   ENDIF
064700060215      *
064800060215      *
064900060215if  1c                   if        $Called = *off
065000060215      * - Ritorno alla videata precedente
065100060215     c                   eval      $Video  = '1'
065200060215     c                   eval      $InzD01 = *on
065300060215     c                   eval      $InzD02 = *on
065400060215x   1c                   else
065500060215      * - Uscita dal pgm
065600060215     c                   exsr      F03D01
065700060215e   1c                   endif
065800060215      *
065900060215     c                   ENDSR
066000060215
066100060215      *---------------------------------------------------------------*
066200060215      *   S C H I E R E   A   T E M P O   D I   C O M P I L A Z .     *
066300060215      *---------------------------------------------------------------*
066400060215
066500060215** - $STS
066600060215IMMISSIONE
066700060215VARIAZIONE
066800060215ANNULLATO
066900060216** - $MSG -------------------------------------------------------------------*
067000060216Numero legge obbligatorio se immessa la data di inizio decorrenza               1
067100060216Data inizio decorrenza legge obbligatoria se immesso il numero legge            2
067200060216Data formalmente errata                                                         3
