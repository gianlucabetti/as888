000100060223      *===============================================================*
000200060223      *?Controllo instradamenti in P.O. - Richiesta parametri        ?*
000300060223      *===============================================================*
000400060223      *
000500060223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060223      *
000700060223      *?A R C H I V I?------------------------------------------------*
000800060223      *
000900060223     fTISI75D   cf   e             workstn
001000060223      *
001100060223     fTABEL00F  if   e           k disk
001200060223     fAZORG01L  if   e           k disk
001300060223      *
001400060223      *?C O S T A N T I?----------------------------------------------*
001500060223      *
001600060223      *
001700060223      *?S C H I E R E?------------------------------------------------*
001800060223      *
001900060223      * - Messaggi
002000060420     d $Msg            s             78    dim(8)  ctdata  perrcd(1)
002100060223      *
002200060223      *?S T R U T T U R E   D A T I?----------------------------------*
002300060223      *
002400060223     d KPJBA         e ds
002500060223      *
002600060223      * - Parametri x Controllo profilo utenti
002700060223     d TIBS34ds      e ds
002800060223      * - Ds di riferimento al file esterno AZUTE00F
002900060223     d AZUTEds       e ds                  extname(AZUTE00F)
003000060223      * - Ds per dati organigramma
003100060223     d DDatiUte      e ds
003200060223      *
003300060223      * - Anagrafiche clienti
003400060223     d TIBS69ds      e ds                  inz
003500060223     d DS_cnaco      e ds                  inz  extname(CNACO00F)
003600060223     d DS_cnind      e ds                  inz  extname(CNIND00F)
003700060223     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
003800060223     d DS_fncls      e ds                  inz  extname(FNCLS00F)
003900060223      *
004000060223      * Tab. 5A = Giorni di pulizia archivi
004100060223     d ds5a          e ds                  inz
004200060223      *
004300060223     d Status         sds
004400060223     d   SDSpgm          *proc
004500060223      *
004600060223      * - Parametri di lancio per pgm. TISI76R
004700060223     d TISI76ds        ds                  inz
004800060223     d  P76pav                        1    inz                                  -tipo bolla
004900060223     d  P76ds1                        8  0 inz                                  -data iniziale
005000060223     d  P76ds2                        8  0 inz                                  -data finale
005100060223     d  P76ksc                        7  0 inz                                  -cliente
005200060223     d  P76lnp                        3  0 inz                                  -linea partenza
005300060223     d  P76lna                        3  0 inz                                  -linea arrivo
005400060223     d  P76fns                        1    inz                                  -bolle con serie
005500060223     d  P76fel                        1    inz                                  -errori di linea
005600060223     d  P76nrs                        2  0 inz                                  -serie
005700060223     d  P76tot                        1    inz                                  -solo totali
005800061018     d  P76obs                        1    inz                                  -solo totali
005900060223      *
006000060223      * - Controllo/Inversione date
006100060223     d WLBdat          ds                  inz
006200060223     d   G08dat                       8  0 inz
006300060223     d   G08inv                       8  0 inz
006400060223     d   G08err                       1    inz('3')
006500060223     d   G08tgi                       5  0 inz
006600060223      * - Creazione data da giorni
006700060223     d WGIdat          ds                  inz
006800060223     d   WGDdat                       8  0 inz                                  -data
006900060223     d   WGDinv                       8  0 inz                                  -data invertita
007000060223     d   WGDtgi                       5  0 inz                                  -n° giorni
007100060223      *
007200060223      *?V A R I A B I L I?--------------------------------------------*
007300060223      *
007400060223      * - Flags
007500060223     d $Fine           s              1    inz(*off)
007600060223     d $InzD01         s              1    inz(*on)
007700060223     d $Video          s              1    inz('1')
007800060223      *
007900060223      * - Parametri per TRUL49C
008000060223      *   (Reperisce data ultimo utilizzo di un pgm)
008100060223     d P49pgm          s             10    inz
008200060223     d P49dat          s              8    inz
008300060223     d P49err          s              1    inz
008400060223      *
008500060223      * - Campi di comodo
008600060223     d wGiorni         s              3  0 inz                                  *num. giorni
008700060223     d wAMGini         s              8  0 inz                                  *data inizio aa/m/g
008800060223     d wAMGfin         s              8  0 inz                                  *data fine   aa/m/g
008900060223     d wGMAini         s              8  0 inz                                  *data inizio g/m/aa
009000060223     d wGMAfin         s              8  0 inz                                  *data fine   g/m/aa
009100060223     d PamgINI         s              8  0 inz
009200060223     d PamgFIN         s              8  0 inz
009300060223     d PgmaINI         s              8  0 inz
009400060223     d PgmaFIN         s              8  0 inz
009500060223     d AamgINI         s              8  0 inz
009600060223     d AamgFIN         s              8  0 inz
009700060223     d AgmaINI         s              8  0 inz
009800060223     d AgmaFIN         s              8  0 inz
009900060223     d Wdate           s              8  0 inz
010000060223     d W1Cds1          s              8  0 inz                                  *data iniziale
010100060223     d W1Cds2          s              8  0 inz                                  *data finale
010200060223      *
010300060223      *?K E Y - L I S T?----------------------------------------------*
010400060223      *
010500060223      * - TABEL00F
010600060223     c     K03TABEL      klist
010700060223     c                   kfld                    TBLkut
010800060223     c                   kfld                    TBLcod
010900060223     c                   kfld                    TBLkey
011000060223      *
011100060223      *===============================================================*
011200060223      *                                                               *
011300060223      *?I N D I C A T O R I?------------------------------------------*
011400060223      *                                                               *
011500060223      *  28     - Emette il messaggio di errore a video               *
011600060223      *  60     - Tipo bolla (Partenza/Arrivo/Dkt/Sede) errato        *
011700060223      *  61     - Data spedizione iniziale errata                     *
011800060223      *  62     - Data spedizione finale   errata                     *
011900060223      *  63     - Parzializzazione per cliente errata                 *
012000060223      *  64     - Parzializzazione per LNP     errata                 *
012100060223      *  65     - Flag "Elaborare solo bolle con serie" errato        *
012200060223      *  66     - Flag "Solo errori sulla LNA" errato                 *
012300060223      *  67     - Parzializzazione per serie   errata                 *
012400060223      *  68     - Parzializzazione per LNA     errata                 *
012500060223      *  90     - Generico di errore                                  *
012600060223      *                                                               *
012700060223      *===============================================================*
012800060223      *
012900060223     c     *Entry        plist
013000060223     c                   parm                    KPJBA
013100060223      *
013200060223      * Operazioni iniziali
013300060223     c                   exsr      RoutInz
013400060223      *
013500060223      * Gestione Video
013600060223do  1c                   DOW       $Fine   =  *off
013700060223      * - Filtro di lancio
013800060223cas 2c     $Video        caseq     '1'           GesD01
013900060223e   2c                   endcs
014000060223e   1c                   ENDDO
014100060223      *
014200060223      * Fine programma
014300060223     c                   eval      *inLR   =  *on
014400060223      *
014500060223      *?S U B R O U T I N E S?----------------------------------------*
014600060223      *
014700060223      *---------------------------------------------------------------*
014800060223      *?Operazioni iniziali                                          ?*
014900060223      *---------------------------------------------------------------*
015000060223     c     RoutInz       BEGSR
015100060223      *
015200060223     c                   eval      T1Cpgm = SDSpgm
015300060223      *
015400060223      * Reperimento dati utente
015500060223     c                   exsr      RepDatiUte
015600060223      *
015700060223      * Reperimento date di pulizia files
015800060223     c                   clear                   ds5A
015900060223     c                   eval      TBLkut  = 1
016000060223     c                   eval      TBLcod  = '5A'
016100060223     c                   eval      TBLkey  = '1       '
016200060223     c     K03TABEL      chain     TABEL
016300060223if  1c                   if            %found(TABEL00F)
016400060223     c                             and TBLflg = *blanks
016500060223     c                   movel     TBLuni        ds5A
016600060223e   1c                   endif
016700060223      *
016800060223      * Impostazione delle date di lavoro
016900060223      * - data corrente
017000060223     c                   clear                   WLBdat
017100060223     c                   eval      G08dat  = *date
017200060223     c                   call      'XSRDA8'
017300060223     c                   parm                    WLBdat
017400060223     c                   eval      Wdate   = G08inv
017500060223      *
017600060223      * - Reperisce la data iniziale e finale minima/massima
017700060223      *   per elaborare le bolle partenza
017800060223     c                   eval      P49pgm  = 'FNLS61R   '
017900060223     c                   eval      wGiorni = §5Ablp
018000060223     c                   exsr      RepDatPul
018100060223     c                   eval      PamgINI = wAMGini
018200060223     c                   eval      PamgFIN = wAMGfin
018300060223     c                   eval      PgmaINI = wGMAini
018400060223     c                   eval      PgmaFIN = wGMAfin
018500060223      *
018600060223      * - Reperisce la data iniziale e finale minima/massima
018700060223      *   per elaborare le bolle arrivo
018800060223     c                   eval      P49pgm  = 'FNLR84R   '
018900060223     c                   eval      wGiorni = §5Aarb
019000060223     c                   exsr      RepDatPul
019100060223     c                   eval      AamgINI = wAMGini
019200060223     c                   eval      AamgFIN = wAMGfin
019300060223     c                   eval      AgmaINI = wGMAini
019400060223     c                   eval      AgmaFIN = wGMAfin
019500060223      *
019600060223     c                   ENDSR
019700060223      *
019800060223      *---------------------------------------------------------------*
019900060223      *?Reperimento Dati Utente                                      ?*
020000060223      *---------------------------------------------------------------*
020100060223     c     RepDatiUte    BEGSR
020200060223      *
020300060223      * Reperisce dati job
020400060223     c     *dtaara       define    §azute        azuteds
020500060223     c     *dtaara       define    §datiute      ddatiute
020600060223      *
020700060223     c                   in(E)     *dtaara
020800060223     c                   IF        %ERROR or RSUT = *blanks
020900060223     c                   clear                   Tibs34Ds
021000060223     c                   call      'TIBS34R'
021100060223     c                   parm                    Tibs34Ds
021200060223     c                   in        *dtaara
021300060223     c                   ENDIF
021400060223      *
021500060223     c                   ENDSR
021600060223      *
021700060223      *---------------------------------------------------------------*
021800060223      *?Gestione videata D01                                         ?*
021900060223      *---------------------------------------------------------------*
022000060223     c     GesD01        BEGSR
022100060223      *
022200060223      * Inizializzo la videata
022300060223if  1c                   if        $InzD01 =  *on
022400060223     c                   exsr      InzD01
022500060223     c                   eval      $InzD01 =  *off
022600060223e   1c                   endif
022700060223      *
022800060223      * Scrivo la testata e la riga tasti funzionali abilitati
022900060223if  1c                   if        NOT *in90
023000060223     c                   write     SI75T01
023100060223e   1c                   endif
023200060223      * Emetto la videata
023300060223     c                   exfmt     SI75D01
023400060223     c                   setoff                                       28  90
023500060223     c                   clear                   V1Dmsg
023600060223      *
023700060223sel 1c                   select
023800060223      *
023900060223      * F3=Fine
024000060223w   1c                   when      *inKC
024100060223     c                   exsr      F03D01
024200060223     c                   leavesr
024300060223      *
024400060223      * Invio  o  F6=Conferma
024500060223x   1c                   other
024600060223      *
024700060223      * - Controllo dati immessi a video
024800060223     c                   exsr      CtrD01
024900060223if  2c                   if        *in90
025000060223     c                   leavesr
025100060223e   2c                   endif
025200060223      * - F6=Conferma
025300060223if  2c                   if        *inKF
025400060223     c                   exsr      F06D01
025500060223     c                   eval      $Fine   =  *on
025600060223e   2c                   endif
025700060223      *
025800060223e   1c                   endsl
025900060223      *
026000060223     c                   ENDSR
026100060223      *
026200060223      *---------------------------------------------------------------*
026300060223      *?Inizializzazione videata D01                                 ?*
026400060223      *---------------------------------------------------------------*
026500060223     c     InzD01        BEGSR
026600060223      *
026700060223     c                   clear                   SI75D01
026800060223      *
026900060420     c                   eval      V1Cfns = 'S'
027000060223     c                   eval      V1Cfel = 'N'
027100060223     c                   eval      V1Ctot = 'N'
027200061018     c                   eval      V1Cobs = 'N'
027300060223      *
027400060223     c                   ENDSR
027500060223      *
027600060223      *---------------------------------------------------------------*
027700060223      *?Controllo dati immessi in videata D01                        ?*
027800060223      *---------------------------------------------------------------*
027900060223     c     CtrD01        BEGSR
028000060223      *
028100060223     c                   movea     *zeros        *in(50)
028200060223      *
028300060223sel 1c                   select
028400060223      * Tipo bolla
028500060223w   1c                   when          V1Cpav <> 'P'
028600060223     c                             and V1Cpav <> 'A'
028700060223     c                             and V1Cpav <> 'D'
028800060223     c                             and V1Cpav <> 'S'
028900060223     c                   seton                                        286090
029000060223     c                   movel     $Msg(1)       V1Dmsg
029100060223     c                   leavesr
029200060223      *
029300060223      * Controllo Sede/Filiale
029400060223w   1c                   when          V1Cpav =  'S'
029500060223     c                             and DUTlpo <> 'S'
029600060223     c                   seton                                        286090
029700060420     c                   movel     $Msg(6)       V1Dmsg
029800060223     c                   leavesr
029900060223      *
030000060223e   1c                   endsl
030100060223      *
030200060223      * Imposta le date se vuote
030300060223if  1c                   if            V1Cds1 = *zeros
030400060223     c                             and V1Cds2 = *zeros
030500060223if  2c                   if        V1Cpav  = 'P'
030600060223     c                   z-add     PgmaINI       V1Cds1
030700060223     c                   z-add     PgmaFIN       V1Cds2
030800060223e   2c                   endif
030900060223if  2c                   if        V1Cpav  = 'A'
031000060223     c                   z-add     AgmaINI       V1Cds1
031100060223     c                   z-add     AgmaFIN       V1Cds2
031200060223e   2c                   endif
031300060223e   1c                   endif
031400060223      *
031500060223      * Data iniziale
031600060223     c                   clear                   W1Cds1
031700060223     c                   clear                   WLBdat
031800060223     c                   eval      G08dat  = V1Cds1
031900060223     c                   call      'XSRDA8'
032000060223     c                   parm                    WLBdat
032100060223     c                   eval      V1Cds1  = G08dat
032200060223if  1c                   if        G08err  = *on
032300060223     c                   seton                                        286190
032400060223     c                   eval      V1Dmsg  = $Msg(2)
032500060223     c                   leavesr
032600060223e   1c                   endif
032700060223     c                   eval      W1Cds1  = G08inv
032800060223      *
032900060223      * Data finale
033000060223     c                   clear                   W1Cds2
033100060223     c                   clear                   WLBdat
033200060223     c                   eval      G08dat  = V1Cds2
033300060223     c                   call      'XSRDA8'
033400060223     c                   parm                    WLBdat
033500060223     c                   eval      V1Cds2  = G08dat
033600060223if  1c                   if        G08err  = *on
033700060223     c                   seton                                        286290
033800060223     c                   eval      V1Dmsg  = $Msg(2)
033900060223     c                   leavesr
034000060223e   1c                   endif
034100060223     c                   eval      W1Cds2  = G08inv
034200060223      *
034300060223      * Range di date
034400060223if  1c                   if        W1Cds1  > W1Cds2
034500060223     c                   seton                                        28  90
034600060223     c                   seton                                        6162
034700060223     c                   eval      V1Dmsg  = $Msg(3)
034800060223     c                   leavesr
034900060223e   1c                   endif
035000060223      *
035100060223      * Codice cliente
035200060223     c                   clear                   V1Dksc
035300060223if  1c                   if        V1Cksc <> *zeros
035400060223     c                   clear                   TIBS69ds
035500060223     c                   clear                   ds_CNACO
035600060223     c                   clear                   ds_CNIND
035700060223     c                   clear                   ds_CNCLP
035800060223     c                   clear                   ds_FNCLS
035900060223     c                   eval      I69kac  = V1Cksc
036000060223     c                   call      'TIBS69R'
036100060223     c                   parm                    TIBS69ds
036200060223     c                   parm                    ds_CNACO
036300060223     c                   parm                    ds_CNIND
036400060223     c                   parm                    ds_CNCLP
036500060223     c                   parm                    ds_FNCLS
036600060223if  2c                   if        O69err  = *on
036700060223     c                   seton                                        286390
036800060420     c                   eval      V1Dmsg  = $Msg(4)
036900060223x   2c                   else
037000060223     c                   movel     ACOrag        V1Dksc
037100060223e   2c                   endif
037200060223e   1c                   endif
037300060223      *
037400060223      * P.O. di partenza
037500060223     c                   clear                   V1Dlnp
037600060223if  1c                   if        V1Clnp <> *zeros
037700060223     c     V1Clnp        chain     AZORG
037800060223if  2c                   if        %found(AZORG01L)
037900060223     c                   movel     ORGdes        V1Dlnp
038000060223x   2c                   else
038100060223     c                   seton                                        286490
038200060420     c                   eval      V1Dmsg  = $Msg(5)
038300060223     c                   leavesr
038400060223e   2c                   endif
038500060223e   1c                   endif
038600060223      *
038700060223      * P.O. di arrivo
038800060223if  1c                   if            V1Cpav =  'A'
038900060223     c                             and V1Clna =  *zeros
039000060223     c                   seton                                        286890
039100060420     c                   eval      V1Dmsg  = $Msg(7)
039200060223     c                   leavesr
039300060223e   1c                   endif
039400060223     c                   clear                   V1Dlna
039500060223if  1c                   if        V1Clna <> *zeros
039600060223     c     V1Clna        chain     AZORG
039700060223if  2c                   if        %found(AZORG01L)
039800060223     c                   movel     ORGdes        V1Dlna
039900060223x   2c                   else
040000060223     c                   seton                                        286890
040100060420     c                   eval      V1Dmsg  = $Msg(7)
040200060223     c                   leavesr
040300060223e   2c                   endif
040400060223e   1c                   endif
040500060223      *
040600060223      * solo bolle con serie
040700060223if  1c                   if            V1Cfns <> 'S'
040800060223     c                             and V1Cfns <> 'N'
040900060223     c                   seton                                        286590
041000060223     c                   eval      V1Dmsg  = $Msg(1)
041100060223     c                   leavesr
041200060223e   1c                   endif
041300060420if  1c                   if            V1Cfns =  'N'
041400060420     c                             and V1Cpav <> 'D'
041500060420     c                   seton                                        286590
041600060420     c                   eval      V1Dmsg  = $Msg(8)
041700060420     c                   leavesr
041800060420e   1c                   endif
041900060223      *
042000060223      * solo errori sulla linea arrivo
042100060420if  1c                   IF            V1Cfel <> 'S'
042200060420     c                             and V1Cfel <> 'N'
042300060223     c                   seton                                        286690
042400060223     c                   eval      V1Dmsg  = $Msg(1)
042500060223     c                   leavesr
042600060223e   1c                   endif
042700060223      *
042800060223     c                   ENDSR
042900060223      *
043000060223      *---------------------------------------------------------------*
043100060223      *?Gestione tasto funzionale F03 da videata D01                 ?*
043200060223      *---------------------------------------------------------------*
043300060223     c     F03D01        BEGSR
043400060223      *
043500060223      * Chiudo il programma
043600060223     c                   eval      $Fine   =  *on
043700060223      *
043800060223     c                   ENDSR
043900060223      *
044000060223      *---------------------------------------------------------------*
044100060223      *?Gestione tasto funzionale F06 da videata D01                 ?*
044200060223      *---------------------------------------------------------------*
044300060223     c     F06D01        BEGSR
044400060223      *
044500060223     c                   clear                   TISI76ds
044600060223      *
044700060223     c                   eval      P76pav  = V1Cpav
044800060223     c                   eval      P76ds1  = W1Cds1
044900060223     c                   eval      P76ds2  = W1Cds2
045000060223     c                   eval      P76ksc  = V1Cksc
045100060223     c                   eval      P76lnp  = V1Clnp
045200060223     c                   eval      P76lna  = V1Clna
045300060223     c                   eval      P76fns  = V1Cfns
045400060223     c                   eval      P76fel  = V1Cfel
045500060223     c                   eval      P76nrs  = V1Cnrs
045600060223     c                   eval      P76tot  = V1Ctot
045700061018     c                   eval      P76obs  = V1Cobs
045800060223     c***                eval      P76bpt  = V1Cbpt
045900060223     c                   movel(p)  TISI76ds      KPJBU
046000060223     c                   eval      KCOAZ   = 'SI76'
046100060223     c                   if        KNMUS   = '1111111111'
046200060322     c                   call      'TISI76R'
046300060223     c                   parm                    KPJBA
046400060223     c                   else
046500060223     c                   call      'BCH10'
046600060223     c                   parm                    kpjba
046700060223     c                   endif
046800060223      *
046900060223     c                   ENDSR
047000060223      *
047100060223      *---------------------------------------------------------------*
047200060223      *?Reperimento Date di pulizia ammesse                          ?*
047300060223      *---------------------------------------------------------------*
047400060223     c     RepDatPul     BEGSR
047500060223      *
047600060223      * azzera le variabili di lavoro
047700060223     c                   clear                   wAMGini
047800060223     c                   clear                   wAMGfin
047900060223     c                   clear                   wGMAini
048000060223     c                   clear                   wGMAfin
048100060223      *
048200060223      * Data Iniziale = data ultimo utilizzo pgm pulizia
048300060223      *               - gg mantenimento bolle
048400060223     c                   call      'TRUL49C'                            90
048500060223     c                   parm                    P49pgm
048600060223     c                   parm      *blanks       P49dat
048700060223     c                   parm      *blanks       P49err
048800060223     c                   reset                   WLBdat
048900060223sel 1c                   select
049000060223      * - errore
049100060223w   1c                   when      P49err  = *on  or  *in90
049200060223     c                   eval      G08inv  = Wdate
049300060223      * - pgm. NON ancora usato
049400060223w   1c                   when      P49dat  = *blanks
049500060223     c*** già così:      clear                   G08inv
049600060223      * - pgm. usato
049700060223x   1c                   other
049800060223     c                   move      P49dat        G08inv
049900060223e   1c                   endsl
050000060223      *
050100060223if  1c                   if        G08inv <> *zeros
050200060223     c                   call      'XSRDA8'
050300060223     c                   parm                    WLBdat
050400060223     c                   clear                   WGIdat
050500060223     c                   eval      WGDtgi  = G08tgi - wGiorni
050600060223     c                   call      'XSRGI8'
050700060223     c                   parm                    WGIdat
050800060223     c                   eval      wAMGini = WGDinv
050900060223      * - Inverte le date in gg/mm/anno
051000060223     c                   reset                   WLBdat
051100060223     c                   eval      G08inv  = WGDinv
051200060223     c                   call      'XSRDA8'
051300060223     c                   parm                    WLBdat
051400060223     c                   eval      wGMAini = G08dat
051500060223e   1c                   endif
051600060223      *
051700060223      * Data Finale   = data corrente
051800060223     c                   eval      wGMAfin = *date
051900060223     c                   eval      wAMGfin = Wdate
052000060223      *
052100060223     c                   ENDSR
052200060223      *
052300060223      *?S C H I E R E?------------------------------------------------*
052400060223      *
052500060223** - $MSG -------------------------------------------------------------------*
052600060420Immettere uno dei valori possibili                                             1
052700060420Data errata                                                                    2
052800060420La data finale deve essere maggiore o uguale alla data iniziale                3
052900060420Cliente errato o non in gestione                                               4
053000060420P.O. errato                                                                    5
053100060420Da P.O. è impossibile verificare le bolle di sede                              6
053200060420Inserire la LNA se selezionate bolle di arrivo ("A=Arr")                       7
053300060420Selezionare anche le bolle senza serie è possibile solo per bolle D=Dkt        8
