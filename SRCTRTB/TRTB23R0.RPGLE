000100130829     /*END
000200060119      *===============================================================*
000300060119      *?TRTB23R * Gestione tabella "03" = Zone Arrivo                ?*
000400060119      *===============================================================*
000500060119
000600060119     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700060119
000800060119      *---------------------------------------------------------------*
000900060119
001000060119     fAZORG01L  if   e           k disk
001100160906     fTABEL00f  Uf A e           k disk    extfile(tabelg)
001101160906     f                                     usropn
001200060119     f                                     infds(dsFMT)
001202160906     fTABELfil  uf a e           k disk    extfile(tabelf)
001203160906     f                                     usropn prefix(f)
001204160906     f                                     rename(tabel:tabel0)
001300160905     fTRTB23D0  cf   e             workstn
001400060119     f                                     sfile(TB23S01:S01nrr)
001500060119
001600060119      *---------------------------------------------------------------*
001700060119
001800060119      *
001900060119      * COSTANTI - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002000060119      *
002100060120      * - Caratteri numerici
002101160906     d tabelds       E DS                  extname(tabel00f)
002102160906     d tabelfds      E DS                  extname(tabel00f) prefix(f)
002103160906     d tibs55ds      E DS
002200060120     d DigitN          c                   const('0123456789')
002300060119      * - Nr. di righe del sfl (SFLPAG)
002400060120     d C_SflPag        c                   const(17)
002500060119      *
002501160905     d password        c                   const('TURNO     ')
002600060119      * SCHIERE  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002700060119      *
002800160905     d $Msg            s             78    dim(12) ctdata perrcd(1)
002900060119      *
003000060119      * STRUTTURE DATI - - - - - - - - - - - - - - - - - - - - - - - -*
003100060119      *
003200060119     d KPJBA         e ds
003300060119      * - Parametri x Controllo profilo utenti
003400060119     d TIBS34ds      e ds
003500060119      * - Ds di riferimento al file esterno AZUTE00F
003600060119     d AZUTEds       e ds                  extname(AZUTE00F)
003700060119      * - Ds per dati organigramma
003800060119     d DDatiUte      e ds
003900060119      *
004000160512     d og150         e ds
004100060120      * - Tab. 03 = Zone di Consegna
004200060119     d ds03          e ds                  inz
004300060119      *
004400060119     d ds_ZONA         ds             8
004500060119     d  dsZfil                 1      3
004600060119     d  dsZznc                 4      5
004700060119      *
004800060119     d dsFMT           ds
004900060119     d  £SFLnrr              397    400B 0
005000060119      *
005100060119     d Status         sds           333
005200060119     d   SDSpgm          *proc
005300060119      *
005400060119      * VARIABILI  - - - - - - - - - - - - - - - - - - - - - - - - - -*
005500060119      *
005600060119      * - Flags
005700060119     d $EoF            s              1    inz(*off)
005800060119     d $Fine           s              1    inz(*off)
005900060119     d $RecOK          s              1    inz(*off)
006000060119     d $InzD01         s              1    inz(*on)
006100060120     d $InzS01         s              1    inz(*on)
006200060119      *
006300060119     d $Video          s              1    inz('1')
006400060119      *
006500060119      * - Variabili riferite al data base o al display file
006600060119     d S01nrr          s                   like(C01rcd) inz
006700060120     d SAVnrr          s                   like(C01rcd) inz
006800060120     d SAVcod          s                   like(TBLcod) inz
006900060120     d SAVkey          s                   like(TBLkey) inz
007000060120      *   Nr. di righe inserite
007100060120     d wTOTins         s                   like(C01rcd) inz
007200060120      *   Nr. di righe modificate
007300060120     d wTOTmod         s                   like(C01rcd) inz
007400060120      *   Nr. di righe annullate
007500060120     d wTOTann         s                   like(C01rcd) inz
007600060120      *   Nr. di righe ripristinate
007700060120     d wTOTrip         s                   like(C01rcd) inz
007800060120      * - Variabili definite a programma
007900060120     d W1Cznc          s              2    inz
008000060120     d Wcampo          s             10    inz
008100060119     d wPag            s              4  0 inz
008200060119     d wRig            s              3  0 inz
008201160906     d tabelg          s             21
008202160906     d tabelf          s             21
008300060119      *
008400060119      * KEY-LIST - - - - - - - - - - - - - - - - - - - - - - - - - - -*
008500060119      *
008600060119     c     K03TABEL      klist
008700060119     c                   kfld                    TBLkut
008800060119     c                   kfld                    TBLcod
008900060119     c                   kfld                    TBLkey
009000060119     c     K02TABEL      klist
009100060119     c                   kfld                    TBLkut
009200060119     c                   kfld                    TBLcod
009300060119
009400060119      *---------------------------------------------------------------*
009500060119      *  RIEPILOGO INDICATORI                                         *
009600060119      *---------------------------------------------------------------*
009700060119      * 10    - Comodo                                                *
009800060119      * 28    - Emette il messaggio di errore a video                 *
009900060119      * 30    - Ripulisce  il subfile S01                             *
010000060119      * 31    - NON emette il subfile S01                             *
010100060119      * 32    - Record modificato nel subfile S01 (sflnxtchg)         *
010200060119      * 33    - Fine dati         nel subfile S01 (sflend)            *
010600060119      * 51    - D01: P.O. arrivo  errato                              *
010700060119      * 52    -      Zona arrivo  errata                              *
010800060119      * 53    -      Località     errata                              *
010900060119      * 54    - S01: P.O. arrivo  errato                              *
011000060119      * 55    -      Zona arrivo  errata                              *
011100060123      * 57    -      Flag Annull. errato                              *
011200060119      * 90    - Generico di errore                                    *
011300060119      *---------------------------------------------------------------*
011400060119
011500060119     c     *Entry        plist
011600060119     c                   parm                    KPJBA
011700060119      *
011701160906     c                   exsr      tstobj
011800060119      * Operazioni Iniziali
011900060119     c                   exsr      RoutInz
011901160905     c                   eval      *in02 = (%subst(knmus:1:3) = 'EDP')
012000060119      *
012100060119      * Gestione Video
012200060119     c                   DOW       $Fine = *off
012300060119      * - Filtro
012400060119cas 2c     $Video        caseq     '1'           GesD01
012500060119      * - SubFile
012600060119cas 2c     $Video        caseq     '2'           GesS01
012700060119e   2c                   endcs
012800060119e   1c                   ENDDO
012900060119      *
013000060119      * Fine
013100060119     c                   movel     *on           *inLR
013200060119      *
013203160906      *-----------------------------------------------------***********
013204160906     C     TSTOBJ        BEGSR
013205160906      *-----------------------------------------------------***********
013206160906     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
013207160906     C* da/a cui spedire
013208160906     C                   CLEAR                   TIBS55ds
013209160906     C                   MOVEL     'L'           I50TLA
013211160906     C                   MOVEL     '046'         I50APO
013212160906     C                   CALL      'TIBS55R'
013213160906     C                   PARM                    TIBS55ds
013214160906     c                   eval      tabelg = O50ALG + '/TABEL00F'
013215160906     C                   CLEAR                   TIBS55ds
013216160906     C                   MOVEL     'L'           I50TLA
013217160906     C                   MOVEL     '001'         I50APO
013218160906     C                   CALL      'TIBS55R'
013219160906     C                   PARM                    TIBS55ds
013220160906     c                   eval      tabelf = O50ALG + '/TABEL00F'
013221160906     c                   open      tabel00f
013222160906     c                   open      tabelfil
013225160906     C                   ENDsr
013300060119      *---------------------------------------------------------------*
013400060119      * Operazioni Iniziali                                           *
013500060119      *---------------------------------------------------------------*
013600060119     c     RoutInz       BEGSR
013700060119      *
013800060119      * Reperisco dati job
013900060119     c                   exsr      DatiJob
014000060119      *
014100060119     c                   movel     SDSpgm        V1Tpgm
014200060119     c                   eval      TBLkut = 1
014300060119     c                   eval      TBLcod = '03'
014400060119     c                   clear                   TBLkey
014500060119      *
014600060119     c                   ENDSR
014700060119      *
014800060119      *---------------------------------------------------------------*
014900060119      * Reperimento Dati del job (Utente/Operativi)                   *
015000060119      *---------------------------------------------------------------*
015100060119     c     DatiJob       BEGSR
015200060119      *
015300060119     c     *dtaara       define    §azute        azuteds
015400060119     c     *dtaara       define    §datiute      ddatiute
015500060119      *
015600060119     c                   in(E)     *dtaara
015700060119     c                   IF        %ERROR or RSUT = *blanks
015800060119     c                   clear                   Tibs34Ds
015900060119     c                   call      'TIBS34R'
016000060119     c                   parm                    Tibs34Ds
016100060119     c                   in        *dtaara
016200060119     c                   ENDIF
016300060119      *
016400060119     c                   ENDSR
016500060119      *
016600060119      *---------------------------------------------------------------*
016700060119      * Gestione videata D01                                          *
016800060119      *---------------------------------------------------------------*
016900060119     c     GesD01        BEGSR
017000060119      *
017100060119      * Inizializzo la videata
017200060119if  1c                   if        $InzD01 = *on
017300060119     c                   exsr      InzD01
017400060119     c                   movel     *off          $InzD01
017500060119e   1c                   endif
017600060119      *
017700060119      * Scrivo la testata e la riga tasti funzionali abilitati
017800060119if  1c                   if        NOT *in90
017900060119     c                   write     TB23T01
018000060119     c                   write     TB23P01
018100060119e   1c                   endif
018200060119      * Emetto la videata
018300060119     c                   exfmt     TB23D01
018400060119     c                   setoff                                       28  90
018500060119     c                   clear                   V1Dmsg
018600060119      *
018700060119sel 1c                   select
018800060119      * F3=Fine
018900060119w   1c                   when      *inKC
019000060119     c                   exsr      F03D01
019100060119     c                   leavesr
019200060119      * F10=Aggiunta
019300060119w   1c                   when      *inKJ
019400060119     c                   eval      $Video  = '2'
019500060120     c                   eval      $InzS01 = *on
019600060119     c                   leavesr
019700060119      * Altrimenti (Invio)
019800060119      * => si controllano i dati di questa videata
019900060119x   1c                   other
020000060119     c                   exsr      CtrD01
020100060119if  2c                   if        *in90
020200060119     c                   leavesr
020300060119e   2c                   endif
020400060119     c                   eval      $Video  = '2'
020500060120     c                   eval      $InzS01 = *on
020600060119      *
020700060119e   1c                   endsl
020800060119      *
020900060119     c                   ENDSR
021000060119      *
021100060119      *---------------------------------------------------------------*
021200060119      * Inizializzazione videata D01                                  *
021300060119      *---------------------------------------------------------------*
021400060119     c     InzD01        BEGSR
021500060119      *
021600060120     c                   clear                   TB23D01
021601160905     c                   eval      *in02 = (%subst(knmus:1:3) = 'EDP')
021700060119      *
021800060119     c                   ENDSR
021900060119      *
022000060119      *---------------------------------------------------------------*
022100060119      * Gestione tasto funzionale F03 da videata D01                  *
022200060119      *---------------------------------------------------------------*
022300060119     c     F03D01        BEGSR
022400060119      *
022500060119      * Chiudo il programma
022600060119     c                   eval      $Fine       = *on
022700060119      *
022800060119     c                   ENDSR
022900060119      *
023000060119      *---------------------------------------------------------------*
023100060119      * Controllo dati immessi in videata D01                         *
023200060119      *---------------------------------------------------------------*
023300060119     c     CtrD01        BEGSR
023301160905     c                   if        %subst(knmus:1:3) <>'EDP'
023304160905      * controllo la password
023305160905     c                   if        w1cpsw = *blanks
023306160905     c                   seton                                        2890
023307160905     c                   eval      V1Dmsg  =  $Msg(11)
023308160905     c                   leavesr
023309160905     c                   endif
023310160905      * controllo se esatta
023311160905     c                   if        w1cpsw <> password
023312160905     c                   seton                                        2890
023313160905     c                   eval      V1Dmsg  =  $Msg(12)
023314160905     c                   leavesr
023315160905     c                   endif
023316160905     c                   endif
023400160905
023500160905     c*                  movea     *zeros        *in(50)
023600060119      *
023700060119      * P.O. ARRIVO
023800060119sel 1c                   SELECT
023900060119      * - vuoto
024000060119w   1c                   WHEN           V1Cfil =  *blanks
024100060119     c                             or   V1Cfil =  *zeros
024200060119     c                   seton                                        512890
024300060119     c                   eval      V1Dmsg  =  $Msg(1)
024400060119     c                   leavesr
024500060119      * - "999" = Tutti i P.O.
024600060119w   1c                   WHEN      V1Cfil  =  *all'9'
024700060119     c                   clear                   V1Cznc
024800060130     c                   clear                   V1Cdes
024900060119     c                   eval      $Video  =  '2'
025000060119     c                   eval      $InzS01 =  *on
025100060119     c                   leavesr
025200060119      *
025300060119x   1c                   OTHER
025400060119      * - "?"   = ricerca
025500060119     c     '?'           scan      V1Cfil                                 10
025600060119if  2c                   if        *in10   =  *on
025700060119     c                   eval      V1Cfil  =  *zeros
025800060119     c                   eval      Wcampo  =  'V1CFIL    '
025900060119     c                   exsr      sr_SEARCH
026000060120     c                   eval      *in51   =  *on
026100060119     c                   leavesr
026200060119e   2c                   endif
026300060119      * - controlli:
026400060119      * - - numericità
026500060120     c                   eval      *in10   =  *off
026600060119     c     DigitN        check     V1Cfil                                 10
026700060119if  2c                   if        *in10   =  *on
026800060119     c                   seton                                        512890
026900060119     c                   eval      V1Dmsg  =  $Msg(1)
027000060119     c                   leavesr
027100060119e   2c                   endif
027200060119      * - - validità (se immesso)
027300060119     c                   move      V1Cfil        ORGfil
027400060119     c     ORGfil        chain     AZORG
027500060119if  2c                   if        NOT %found(AZORG01L)
027600060119     c                             or  ORGfva <> *blanks
027700060411     c                             or  (ORGfag <> 'F'
027800060411     c                             and ORGfag <> 'A')
027900060119     c                   seton                                        512890
028000060119     c                   eval      V1Dmsg  =  $Msg(1)
028100060119     c                   leavesr
028200060119e   2c                   endif
028300060119e   1c                   ENDSL
028400060119      *
028500060119      * ZONA ARRIVO
028600060119sel 1c                   SELECT
028700060119      * - "999" = tutte le zone: controllo esistenza di almeno un rec.
028800060119w   1c                   WHEN      V1Cznc  =  *all'9'
028900060120     c                   clear                   ds_Zona
029000060119     c                   movel     V1Cfil        dsZfil
029100060120     c                   movel(p)  ds_Zona       TBLkey
029200060119     c     K03TABEL      setll     TABEL
029300060120     c     K02TABEL      reade(n)  TABEL
029400060119     c                   eval      $RecOK  =  *off
029500060120do  2c                   dow       NOT  %eof(TABEL00F)
029600060119     c                   movel     TBLuni        ds03
029700060119if  3c                   if             TBLflg  =  *blanks
029800060130     c                             and (V1Cdes  =  *blanks
029900060130     c                             or   V1Cdes  <> *blanks
030000060119     c                             and  %subst ( §03des : 1 :
030100060130     c                                        %len ( %trimr(V1Cdes) ) )
030200060130     c                                     = %trimr(V1Cdes))
030300060119     c                   eval      $RecOK  =  *on
030400060119     c                   leave
030500060119e   3c                   endif
030600060120     c     K02TABEL      reade(n)  TABEL
030700060119e   2c                   enddo
030800060119if  2c                   if        $RecOK  =  *off
030900060130if  3c                   if        V1Cdes  =  *blanks
031000060119     c                   seton                                        522890
031100060120     c                   else
031200060120     c                   seton                                        532890
031300060120     c                   endif
031400060130     c                   eval      V1Dmsg  =  $Msg(3)
031500060119     c                   leavesr
031600060119e   2c                   endif
031700060119      *
031800060119x   1c                   OTHER
031900060119      * - controllo validità
032000060120     c                   clear                   ds_Zona
032100060119     c                   movel     V1Cfil        dsZfil
032200060119     c                   movel     V1Cznc        dsZznc
032300060120     c                   movel(p)  ds_Zona       TBLkey
032400060120     c     K03TABEL      chain(n)  TABEL
032500060119if  2c                   if        NOT %found(TABEL00F)
032600060119     c                             or  TBLflg <> *blanks
032700060119     c                   seton                                        522890
032800060119     c                   eval      V1Dmsg  =  $Msg(2)
032900060119     c                   leavesr
033000060119e   2c                   endif
033100060119e   1c                   ENDSL
033200060119      *
033300060119      * Passaggio alla videata S01 (subfile)
033400060119     c                   eval      $Video  =  '2'
033500060119     c                   eval      $InzS01 =  *on
033600060119      *
033700060119     c                   ENDSR
033800060119      *
033900060119      *---------------------------------------------------------------*
034000060119      * Ricerche                                                      *
034100060119      *---------------------------------------------------------------*
034200060119     c     sr_SEARCH     BEGSR
034300060119      *
034400060119sel 1c                   SELECT
034500060119      *
034600060119      * P.O. arrivo
034700060120w   1c                   WHEN           Wcampo = 'V1CFIL    '
034800060120     c                             or   Wcampo = 'S1CLNA    '
034900060119     c                   call      'TNSD24R'
035000060119     c                   parm      *blanks       §COD3             3
035100060120     c                   parm      *blanks       §TIP              1
035200060119     c                   parm      *blanks       §DES25           25
035300060119     c                   parm      *blanks       §PRA              3
035400060120sel 2c                   select
035500060120w   2c                   when      Wcampo  =  'V1CFIL    '
035600060119     c                   movel     §COD3         V1Cfil
035700060119     c***                movel     §DES25        V1Dfil
035800060120w   2c                   when      Wcampo  =  'S1CLNA    '
035900060120     c                   movel     §COD3         S1Clna
036000060120     c                   movel     §DES25        S1Dlna
036100060120e   2c                   endsl
036200060119      *
036300060119e   1c                   ENDSL
036400060119      *
036500060119     c                   eval      *in90   =  *on
036600060119      *
036700060119     c                   ENDSR
036800060119      *
036900060119      *---------------------------------------------------------------*
037000060119      * Gestione videata C01/S01                                      *
037100060119      *---------------------------------------------------------------*
037200060119     c     GesS01        BEGSR
037300060119      *
037400060119      * Inizializzo la videata
037500060119if  1c                   if        $InzS01 =  *on
037600060119     c                   exsr      InzS01
037700060119     c                   eval      $InzS01 =  *off
037800060119e   1c                   endif
037900060120      *
038000060120      * Emetto il msg per la mancanza di dati
038100060120if  1c                   if        C01rcd  <= *zeros
038200060120     c                   eval      $Video  =  '1'
038300060120     c                   seton                                        512890
038400060120     c                   eval      V1Dmsg  =  $Msg(5)
038500060120     c                   leavesr
038600060120e   1c                   endif
038700060119      *
038800060119      * Posiziono il cursore
038900060119if  1c                   if        C01csr  >  *zeros
039000060119     c                   z-add     C01csr        C01rcd
039100060119e   1c                   endif
039200060119      *
039300060119      * Scrivo la testata e la riga tasti funzionali abilitati
039400060119if  1c                   if        NOT *in90
039500060119     c                   write     TB23T01
039600060119     c                   write     TB23P02
039700060119e   1c                   endif
039800060119      *
039900060119      * Visualizzo (eventualmente) il msg per la mancanza di dati
040000060119if  1c***                if        C01rcd  <= *zeros
040100060119     c***                write     TB23D02
040200060119e   1c***                endif
040300060119      *
040400060119      * Emetto la videata
040500060119     c                   exfmt     TB23C01
040600060119     c                   z-add     £SFLnrr       C01rcd
040700060119     c                   setoff                                       28  90
040800060119     c                   clear                   V1Dmsg
040900060119      *
041000060119sel 1c                   SELECT
041100060119      * F3=Fine
041200060119w   1c                   WHEN      *inKC
041300060119     c                   exsr      F03D01
041400060119      * F12=Ritorno
041500060119w   1c                   WHEN      *inKL
041600060119     c                   exsr      F12S01
041700060119      *
041800060119      * Gestione dati inseriti
041900060119x   1c                   OTHER
042000060119     c                   exsr      CtrS01
042100060120     c                   if        *in90
042200060120     c                   leavesr
042300060120     c                   endif
042400060120sel 2c                   select
042500060120     *** * - F5=Ripristino
042600060120w   2***c                   when      *inKE
042700060120     ***c                   exsr      F06S01
042800060120      * - F6=Conferma
042900060120w   2c                   when      *inKF
043000060120     c                   exsr      F06S01
043100060120      * - F10=Aggiunta
043200060120w   2c                   when      *inKJ
043300060120     c                   exsr      F10S01
043400060120     *** * - F16=Annullamento
043500060120w   2***c                   when      *inKQ
043600060120     ***c                   exsr      F06S01
043700060120e   2c                   endsl
043800060120      *
043900060119e   1c                   ENDSL
044000060119      *
044100060119     c                   ENDSR
044200060119      *
044300060119      *---------------------------------------------------------------*
044400060119      * Inizializzazione videata S01                                  *
044500060119      *---------------------------------------------------------------*
044600060119     c     InzS01        BEGSR
044700060119      *
044800060119      * Pulizia subfile
044900060119     c                   seton                                        3031
045000060120     c                   write     TB23C01
045100060119     c                   setoff                                         3133
045200060119      *
045300060119     c                   reset                   $EoF
045400060120     c                   clear                   SAVnrr
045500060119     c                   clear                   C01rcd
045600060119     c                   clear                   C01csr
045700060119     c                   clear                   S01nrr
045800060119     c                   clear                   V1Dmsg
045900060119     c                   setoff                                       28  90
046000060119     c                   movea     *zeros        *in(50)
046100060119      *
046200060119      * Caricamento dei dati da presentare nel sfl
046300060119      *   (NON a pagine)
046400060120     c                   eval      TBLcod  =  '03'
046500060120     c                   clear                   ds_Zona
046600060119sel 1c                   SELECT
046700060119      * - righe vuote (F10=Aggiunta)
046800060119w   1c                   WHEN      *inKJ   =  *on
046900060120     c                   exsr      F10S01
047000060119      * - Tutti i P.O.
047100060119w   1c                   WHEN      V1Cfil  =  *all'9'
047200060119     c     K02TABEL      setll     TABEL
047300060119     c     K02TABEL      reade     TABEL
047400060119     c                   exsr      CarS01
047500060119      * - Tutte le zone di un P.O.
047600060119w   1c                   WHEN      V1Cznc  =  *all'9'
047700060119     c                   movel     V1Cfil        dsZfil
047800060120     c                   movel(p)  ds_Zona       TBLkey
047900060119     c     K03TABEL      setll     TABEL
048000060119     c     K02TABEL      reade     TABEL
048100060119     c                   exsr      CarS01
048200060119      * - Zona specifica di un P.O.
048300060119w   1c                   OTHER
048400060119     c                   movel     V1Cfil        dsZfil
048500060119     c                   movel     V1Cznc        dsZznc
048600060120     c                   movel(p)  ds_Zona       TBLkey
048700060119     c     K03TABEL      setll     TABEL
048800060119     c     K03TABEL      reade     TABEL
048900060119     c                   exsr      CarS01
049000060119e   1c                   ENDSL
049100060119      *
049200060119      * Avendo caricato TUTTI i dati (NON SOLO quelli della 1ª pagina)
049300060119      *  occorre impostare il posizionamento cursore sul 1° rec del sfl
049400060119if  1c                   if        C01csr  >  *zeros
049500060119     c                   eval      C01csr      = 1
049600060119e   1c                   endif
049700060119      *
049800060119     c                   ENDSR
049900060119      *
050000060119      *---------------------------------------------------------------*
050100060119      * Caricamento subfile                                           *
050200060119      *---------------------------------------------------------------*
050300060119     c     CarS01        BEGSR
050400060119      *
050500060120     c                   eval      S01nrr  =  SAVnrr
050600060120     c                   eval      *in32   =  *off
050700060119      *
050800060119      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
050900060119      * => carico?non?solo una pagina, bensì?tutti?i rec. !
051000060120do  1c                   DOW       $EoF    =  *off
051100060119      *
051200060119     c                   eval      $RecOK  =  *off
051300060119     c                   movel     TBLuni        ds03
051400060119      *
051500060119sel 2c                   select
051600060119      * - si scartano i rec. annullati
051700060120      *   MICA VERO !!!
051800060120w   2c***                when      TBLflg  <> *blanks
051900060119      * - se richiesti tutti i P.O.: ok (nessun altra selezione)
052000060119w   2c                   when      V1Cfil  =  *all'9'
052100060119     c                   eval      $RecOK  =  *on
052200060130      * - se richieste tutte le zone: filtro per descrizione
052300060119w   2c                   when      V1Cznc  =  *all'9'
052400060130if  3c                   if             V1Cdes  =  *blanks
052500060130     c                             or   V1Cdes  <> *blanks
052600060119     c                             and  %subst ( §03des : 1 :
052700060130     c                                        %len ( %trimr(V1Cdes) ) )
052800060130     c                                     =  %trimr(V1Cdes)
052900060119     c                   eval      $RecOK  =  *on
053000060119e   3c                   endif
053100060119      * - se richiesta zona specifica: filtro per P.O./zona
053200060120x   2c                   other
053300060120     c                   movel     TBLkey        ds_Zona
053400060120     c                   move      V1Cznc        W1Cznc
053500060120     c                   if             V1Cfil  =  dsZfil
053600060120     c                             and  W1Cznc  =  dsZznc
053700060120     c                   eval      $RecOK  =  *on
053800060120     c                   endif
053900060119e   2c                   endsl
054000060119      *
054100060119     c                   if        $RecOK  =  *on
054200060119     c                   exsr      RieS01
054300060119     c                   endif
054400060119      *
054500060119      * Lettura record successivo
054600060119sel 2c                   SELECT
054700060119      * - Tutti i P.O.
054800060120w   2c                   WHEN      V1Cfil  =  *all'9'
054900060119     c     K02TABEL      reade     TABEL
055000060119     c                   eval      $EoF    = (%eof(TABEL00F))
055100060119      * - Tutte le zone di un P.O.
055200060120w   2c                   WHEN      V1Cznc  =  *all'9'
055300060119     c     K02TABEL      reade     TABEL
055400060119     c                   eval      $EoF    = (%eof(TABEL00F)
055500060119     c                                     or %subst(TBLkey:1:3)
055600060119     c                                        <> V1Cfil)
055700060119      * - Zona specifica di un P.O.
055800060120w   2c                   OTHER
055900060119     c                   eval      $EoF    =  *on
056000060120e   2c                   ENDSL
056100060119      *
056200060120e   1c                   ENDDO
056300060119      *
056400060119      * Visualizzazione del SFL (se ci sono dati)
056500060119     c                   eval      *in30   =  (S01nrr = *zeros)
056600060119      *
056700060119      * Attivazione (eventuale) del SFLEND
056800060119     c                   eval      *in33   =  $EoF
056900060119      *
057000060119      * Posizionamento cursore al 1° rcd della pagina
057100060119if  1c                   if        S01nrr  >  *zeros
057200060119     c     S01nrr        div       C_SflPag      wPag
057300060119     c                   mvr                     wRig
057400060119     c                   eval      C01rcd  =  wPag * C_SflPag
057500060119     c                   if        wRig    >  *zeros
057600060119     c                   eval      C01rcd  =  C01rcd + 1
057700060119     c                   else
057800060119     c                   eval      C01rcd  =  C01rcd - C_SflPag + 1
057900060119     c                   endif
058000060119x   1c                   else
058100060119     c                   clear                   C01rcd
058200060119e   1c                   endif
058300060119     c                   eval      C01csr  =  C01rcd
058400060120      *
058500060120     c                   eval      SAVnrr  =  S01nrr
058600060119      *
058700060119     c                   ENDSR
058800060119      *
058900060119      *---------------------------------------------------------------*
059000060119      * Caricamento dati nel record del sfl                           *
059100060119      *---------------------------------------------------------------*
059200060119     c     RieS01        BEGSR
059300060119      *
059400060120     c                   clear                   TB23S01
059500060119      *
059600060119if  1c                   if        *inKJ
059700060119      * inserimento
059800060120     c                   eval      S1Hnew  =  *on
059900060119      *
060000060119x   1c                   else
060100060119      * modifica
060200060119if  2c                   if        TBLflg  <> *blanks
060300060120     c                   eval      S1Catb  =  'A'
060400060119e   2c                   endif
060500060120     c                   movel     TBLkey        ds_Zona
060600060120     c                   movel     TBLuni        ds03
060700060120     c                   move      dsZfil        S1Clna
060800060120     c                   move      dsZznc        S1Czna
060900060123     c                   movel     §03nuc        S1Cnuc
061000060120     c                   movel     §03tpm        S1Ctpm
061100160426     c                   movel     §03turno      S1Ctur
061200060120     c                   movel     §03des        S1Cloc
061400060120     c                   movel     §03cds        S1Ciat
061500060120      * - campi hidden
061600060120     c                   eval      S1Hnew  =  *off
061700060120     c                   eval      S1Hatb  =  S1Catb
061800060120     c                   eval      S1Hlna  =  S1Clna
061900060120     c                   eval      S1Hzna  =  S1Czna
062000060123     c                   eval      S1Hnuc  =  S1Cnuc
062100060120     c                   eval      S1Htpm  =  S1Ctpm
062200160426     c                   eval      S1Htur  =  S1Ctur
062300060120     c                   eval      S1Hloc  =  S1Cloc
062400060120     c                   eval      S1Hiat  =  S1Ciat
062500060120      * - decodifiche
062600060120     c                   exsr      CtrLNA
062700060120     c                   setoff                                       28  90
062800060120     c                   movea     *zeros        *in(51)
062900060119      *
063000060119e   1c                   endif
063100060120      *
063500160905      * Scrittura record subfile solo per filiali abilitate al turno -*
063501160905     c                   if        not *in11
063600060119     c                   add       1             S01nrr
063700060120     c                   write     TB23S01
063701160905     c                   end
063800060119      *
063900060119     c                   ENDSR
064000060120      *
066400060120      *---------------------------------------------------------------*
066500060120      * Controllo dati nel sfl                                        *
066600060120      *---------------------------------------------------------------*
066700060120     c     CtrS01        BEGSR
066800060120      *
066900060120     c                   clear                   wTOTins
067000060120     c                   clear                   wTOTmod
067100060120     c                   clear                   wTOTann
067200060120     c                   clear                   wTOTrip
067300060120      *
067400060120     c                   readc     TB23S01
067500060120      *
067600160905do  1c                   DOW       NOT %eof(TRTB23D0)
067700060120      *
067800060120     c                   movea     *zeros        *in(51)
067900060120     c                   z-add     S01nrr        C01rcd
068000060120      *
068100060120      * Controllo dati
068200060120     c                   exsr      CtrS01rec
068300060120      *
068700060120      * Aggiornamento sfl
068800060120if  2c                   if        *in28
068900060120     c                   z-add     C01rcd        C01csr
069000060120e   2c                   endif
069100060120     c                   eval      *in32   =  *on
069200060120     c                   UPDATE    TB23S01
069300060120if  2c                   if        *in28  OR  *in90
069400060120     c                   leave
069500060120e   2c                   endif
069600060120      *
069700060120     c                   readc     TB23S01
069800060120      *
069900060120e   1c                   ENDDO
070000060120      *
070100060120      * F6, ma nessun record da modificare/inserire
070200060120if  1c                   if             *inKF
070300060120     c                             and  wTOTmod =  *zeros
070400060120     c                             and  wTOTins =  *zeros
070500060120     c                             and  wTOTann =  *zeros
070600060120     c                             and  wTOTrip =  *zeros
070700060120     c                   seton                                        28  90
070800060120     c                   eval      V1Dmsg  =  $Msg(6)
070900060120     c                   leavesr
071000060120e   1c                   endif
071100060120      *
071200060120     *** * F16, ma nessun record da annullare
071300060120if  1***c                   if             *inKQ
071400060120     ***c                             and  wTOTann =  *zeros
071500060120     ***c                   seton                                        28  90
071600060120     ***c                   eval      V1Dmsg  =  $Msg(7)
071700060120     ***c                   leavesr
071800060120e   1***c                   endif
071900060120     *** *
072000060120     *** * F5, ma nessun record da ripristinare
072100060120if  1***c                   if             *inKE
072200060120     ***c                             and  wTOTrip =  *zeros
072300060120     ***c                   seton                                        28  90
072400060120     ***c                   eval      V1Dmsg  =  $Msg(8)
072500060120     ***c                   leavesr
072600060120e   1***c                   endif
072700060120      *
072800060120     c                   ENDSR
072900060120      *
073000060120      *---------------------------------------------------------------*
073100060120      * Controllo dati del singolo record del sfl                     *
073200060120      *---------------------------------------------------------------*
073300060120     c     CtrS01rec     BEGSR
073400060123      *
073500060123     c                   if             S1Hnew  =  *on
073600060123     c                             and  S1Clna  =  *zeros
073700060123     c                   eval      S1Clna  =  *blanks
073800060123     c                   endif
073900060123      *
074000060123      * Nuovo rec. vuoto => Nessun controllo  &  Nessun conteggio
074100060123if  1c                   if             S1Hnew  =  *on
074200060123     c                             and (S1Clna  =  *blanks
074300060123     c                              or  S1Clna  =  *zeros)
074400060123     c                             and  S1Catb  =  *blanks
074500060123     c                             and  S1Czna  =  *blanks
074600060123     c                             and  S1Cnuc  =  *blanks
074700060123     c                             and  S1Ctpm  =  *blanks
074800160426     c                             and  S1Ctur  =  *blanks
074900060123     c                             and  S1Ciat  =  *blanks
075000060123     c                   clear                   S1Clna
075100060123     c                   clear                   S1Dlna
075200060123     c                   leavesr
075300060123e   1c                   endif
075400060123      *
075500060123      * Annullato rec. vuoto
075600060123if  1c                   if             S1Hnew  =  *on
075700060123     c                             and (S1Clna  =  *blanks
075800060123     c                              or  S1Clna  =  *zeros)
075900060123     c                             and  S1Catb  <> *blanks
076000060123     c                             and  S1Czna  =  *blanks
076100060123     c                             and  S1Cnuc  =  *blanks
076200060123     c                             and  S1Ctpm  =  *blanks
076300160426     c                             and  S1Ctur  =  *blanks
076400060123     c                             and  S1Ciat  =  *blanks
076500060123     c                   seton                                        572890
076600060123     c                   eval      V1Dmsg  =  $Msg(10)
076700060123     c                   leavesr
076800060123e   1c                   endif
076900060120      *
077000060120      * Rec. annullato => nessun controllo
077100060120if  0c                   IF        S1Catb  =  *blanks
077200060120      *
077300060120      * P.O.
077400060120     c                   exsr      CtrLNA
077500060120if  1c                   if        *in90
077600060120     c                   leavesr
077700060120e   1c                   endif
077800060120      *
077900060120      * ZONA
078000060120sel 1c                   SELECT
078100060120      * - vuoto
078200060120w   1c                   WHEN           S1Czna  =  *blanks
078300060120     c                             and  S1Clna  >  *zeros
078400060120     c                   seton                                        552890
078500060120     c                   eval      V1Dmsg  =  $Msg(2)
078600060120     c                   leavesr
078700060120      * - - numericità
078800060120x   1c                   OTHER
078900060120     c                   eval      *in10   =  *off
079000060120     c     DigitN        check     S1Clna                                 10
079100060120if  2c                   if        *in10   =  *on
079200060120     c                   seton                                        552890
079300060120     c                   eval      V1Dmsg  =  $Msg(2)
079400060120     c                   leavesr
079500060120e   2c                   endif
079600160711      * - - numericità ZONA
079700160711     C                   if        s1czna <> *blank
079800160711     c                   eval      *in10   =  *off
079900160711     c     DigitN        check     S1Czna                                 10
080000160711if  2c                   if        *in10   =  *on
080100160711     c                   seton                                        552890
080200160711     c                   eval      V1Dmsg  =  $Msg(2)
080300160711     c                   leavesr
080400160711e   2c                   endif
080500160711e   2c                   endif
080600060120e   1c                   ENDSL
080700060120      *
080800060123e   0c                   ENDIF
080900060123      *
081000060123      * Controllo se inserimento record già esistente
081100060123if  1c                   if        S1Hnew  =  *on
081200060123     c                   clear                   ds_Zona
081300060123     c                   movel     S1Clna        dsZfil
081400060123     c                   movel     S1Czna        dsZznc
081500060123     c                   movel(p)  ds_Zona       TBLkey
081600060123     c     K03TABEL      setll     TABEL
081700060123if  2c                   if        %equal(TABEL00F)
081800060123     c                   seton                                        28  90
081900060123     c                   seton                                        5455
082000060123     c                   eval      V1Dmsg  =  $Msg(9)
082100060123     c                   leavesr
082200060123e   2c                   endif
082300060123e   1c                   endif
082400060120      *
082500060120      * Conteggio record modificati
082600060120if  1c                   if             S1Hnew  =  *off
082700060120     c                             and (S1Hatb  <> S1Catb
082800060120     c                              or  S1Hlna  <> S1Clna
082900060123     c                              or  S1Hnuc  <> S1Cnuc
083000060120     c                              or  S1Hzna  <> S1Czna
083100060120     c                              or  S1Htpm  <> S1Ctpm
083200160426     c                              or  S1Htur  <> S1Ctur
083300060120     c                              or  S1Hloc  <> S1Cloc
083400130829     c                              or  S1Hiat  <> S1Ciat)
083500060120     c                   add       1             wTOTmod
083600060120e   1c                   endif
083700060120      * Conteggio record inseriti
083800060120if  1c                   if             S1Hnew  =  *on
083900060120     c                             and  S1Clna  <> *blanks
084000060120     c                             and  S1Clna  <> *zeros
084100060120     c                   add       1             wTOTins
084200060120e   1c                   endif
084300060120      * Conteggio record annullati
084400060120if  1c                   if             S1Catb  =  'A'
084500060120     c                             and  S1Hatb  =  S1Catb
084600060120     c                   add       1             wTOTann
084700060120e   1c                   endif
084800060120      * Conteggio record ripristinati
084900060120if  1***c                   if             S1Catb  =  'R'
085000060120     ***c                             and  S1Hatb  =  'A'
085100060120if  1c                   if             S1Catb  =  *blanks
085200060120     c                             and  S1Hatb  <> S1Catb
085300060120     c                   add       1             wTOTrip
085400060120e   1c                   endif
085500060120      *
085600060120     c                   ENDSR
085700060120      *
085800060120      *---------------------------------------------------------------*
085900060120      * Decodifica P.O. di Arrivo                                     *
086000060120      *---------------------------------------------------------------*
086100060120     c     CtrLNA        BEGSR
086200060120      *
086300060120     c                   eval      *in10   =  *off
086400060120     c     '?'           scan      S1Clna                                 10
086500060120      *
086600060120      * - "?"   = ricerca
086700060123if  1c                   if        *in10   =  *on
086800060120     c                   eval      S1Clna  =  *zeros
086900060120     c                   eval      Wcampo  =  'S1CLNA    '
087000060120     c                   exsr      sr_SEARCH
087100060120     c                   eval      *in54   =  *on
087200060120     c                   leavesr
087300060123e   1c                   endif
087400060123      *
087500060120      * - controlli:
087600060120      * - - numericità
087700060120     c                   eval      *in10   =  *off
087800060120     c     DigitN        check     S1Clna                                 10
087900060123if  1c                   if        *in10   =  *on
088000060120     c                   seton                                        542890
088100060120     c                   eval      V1Dmsg  =  $Msg(1)
088200060120     c                   leavesr
088300060123e   1c                   endif
088400060120      * - - validità
088500060120     c                   clear                   S1Dlna
088600060120     c                   move      S1Clna        ORGfil
088700060120     c     ORGfil        chain     AZORG
088800060123if  1c                   if        NOT %found(AZORG01L)
088900060120     c                             or  ORGfva <> *blanks
089000060411     c                             or  (ORGfag <> 'F' and orgfag<>'A')
089100060120     c                   seton                                        542890
089200060120     c                   eval      V1Dmsg  =  $Msg(1)
089300060120     c                   leavesr
089400060123e   1c                   endif
089500160512     c                   movel     orgdf0        og150
089600160516     c     §ogturno      comp      'S'                                1111
089700060120     c                   movel     ORGdes        S1Dlna
089800060120      *
089900060120     c                   ENDSR
090000060120      *
090100060120      *---------------------------------------------------------------*
090200060120      * F12 = Ritorno                                                 *
090300060120      * Ritorno alla videata D01                                      *
090400060120      *---------------------------------------------------------------*
090500060120     c     F12S01        BEGSR
090600060120      *
090700060120     c                   eval      $Video  =  '1'
090800060120      *
090900060120     c                   ENDSR
091000060120      *
091100060120      *---------------------------------------------------------------*
091200060120      * F6 = Conferma                                                 *
091300060120      * Aggiornamento Zone Consegna in tab. "03"                      *
091400060120      *---------------------------------------------------------------*
091500060120     c     F06S01        BEGSR
091600060120      *
091700060120     c                   clear                   S01nrr
091800060120     c                   eval      TBLcod  =  '03'
091900060120     c                   clear                   TBLkey
092900060120      *
093000060120     c                   readc     TB23S01
093100060120      *
093200160905do  1c                   DOW       NOT %eof(TRTB23D0)
093300060120      *
093400060120      * - nuovo rec. vuoto
093500060120if  2c                   if             S1Hnew  =  *on
093600060120     c                             and (S1Clna  =  *blanks
093700060120     c                              or  S1Clna  =  *zeros)
093800060120     c                             and  S1Czna  =  *blanks
093900060120     c                   iter
094000060120e   2c                   endif
094100060120      *
094200060120      * - aggiornamento/annullamento/ripristino
094300060120     c                   clear                   ds_Zona
094400160512     c                   exsr      ctrlna
094500060120     c                   movel     S1Clna        dsZfil
094600060120     c                   movel     S1Czna        dsZznc
094700060120     c                   movel(p)  ds_Zona       TBLkey
094800060120     c     K03TABEL      chain     TABEL
094801160906     c     K03TABEL      chain     TABELfil
094900060120      *
095000060120     c                   clear                   ds03
095100060120     c                   movel     S1Cloc        §03des
095200060120     c                   movel     S1Ciat        §03cds
095300160504     c                   movel     S1Ctpm        §03tpm
095400160504     c                   clear                   §03turno
095500160504     c                   if        s1ctpm <> 'M' and s1ctur = '2'
095600160512     c                             and §ogturno = 'S'
095700160426     c                   movel     S1Ctur        §03turno
095800160504     c                   end
095900060123     c                   movel     S1Cnuc        §03nuc
096000060120     c                   movel(p)  ds03          TBLuni
096100060120      *
096200060120if  2c                   if        S1Catb  =  'A'
096300060120     c                   eval      TBLflg  =  '*'
096400060120x   2c                   else
096500060120     c                   clear                   TBLflg
096600060120e   2c                   endif
096700060120     c                   movel(p)  ds03          TBLuni
096800060120     c                   eval      TBLftt  =  '1'
096900060120     c                   clear                   TBLflt
097000060120     c                   clear                   TBLftr
097100060120     c                   clear                   TBLdtr
097200060120      *
097300060120sel 2c                   select
097400060120w   2c                   when           %found(TABEL00F)
097500060120     c                             and  S1Hnew  =  *off
097600060120     c                   UPDATE    TABEL
097601160906     c                   movel     tabelds       tabelfds
097602160906     c                   update    tabel0
097700060120w   2c                   when      NOT  %found(TABEL00F)
097800060120     c                             and  S1Hnew  =  *on
097900060120     c                   WRITE     TABEL
098000060120w   2c                   when      NOT  %found(TABEL00F)
098100060120     c                             and  S1Hnew  =  *off
098200060120     c                   WRITE     TABEL
098201160906     c                   movel     tabelds       tabelfds
098202160906     c                   write     tabel0
098300060120w   2c                   when           %found(TABEL00F)
098400060120     c                             and  S1Hnew  =  *on
098500060120     c                   seton                                        28  90
098600060120     c                   eval      V1Dmsg  =  $Msg(9)
098700060120     c                   leavesr
098800060120e   2c                   endsl
098900060120      *
099000060120     c                   readc     TB23S01
099100060120      *
099200060120e   1c                   ENDDO
099300060120      *
099400060120      * Fine ad aggiornamento eseguito
099500060120     c                   if        NOT *in90
099600060120     c                   eval      $Video  =  '1'
099700060120     c                   eval      $InzD01 =  *on
099800060120     c                   eval      $InzS01 =  *on
099900060120     c                   endif
100000060120      *
100100060120     c                   ENDSR
100200060120      *
100300060120      *---------------------------------------------------------------*
100400060120      * F10 = Aggiunta                                                *
100500060120      * Caricamento righe vuote nel subfile                           *
100600060120      *---------------------------------------------------------------*
100700060120     c     F10S01        BEGSR
100800060120      *
100900060120      * Calcolo del numero di records mancanti al completamento della
101000060120      *   ultima pagina del sfl
101100060120     c     SAVnrr        div       C_SflPag      wPag
101200060120     c                   mvr                     wRig
101300060120     c                   if        wRig    >  *zeros
101400060120     c                   add       1             wPag
101500060120     c                   endif
101600060120     c                   eval      S01nrr  =  SAVnrr
101700060120      *
101800060120      * Posizionamento cursore sulla 1ª delle righe vuote aggiunte
101900060120     c                   eval      C01rcd  =  SAVnrr + 1
102000060120     c                   eval      C01csr  =  C01rcd
102100060120      *
102200060120      * "Completamento" ultima pagina del sfl con record vuoti
102300060120     c                   dow       S01nrr  <  (C_SflPag * wPag)
102400060120     c                   exsr      RieS01
102500060120     c                   enddo
102600060120      *
102700060120      * "Aggiunta" altre 4 pagine (vuote) nel sfl
102800060120     c                   add       4             wPag
102900060120     c                   dow       S01nrr  <  (C_SflPag * wPag)
103000060120     c                   exsr      RieS01
103100060120     c                   enddo
103200060120      *
103300060120     c                   eval      SAVnrr  =  S01nrr
103400060120      *
103500060120     c                   ENDSR
103600060119
103700060120** - $MSG -------------------------------------------------------------------*
103800060120P.O. Arrivo errato                                                             1
103900060120Codice Zona errato                                                             2
104000060120Codice Zona non trovato per il P.O. specificato                                3
104100060120Codice Commerciale errato                                                      4
104200060120Nessun record caricato                                                         5
104300060120Non ci sono record da aggiornare                                               6
104400060120Non ci sono record da annullare                                                7
104500060120Non ci sono record da ripristinare                                             8
104600060123Nuovo record  già presente  in archivio                                        9
104700060123Impossibile annullare un record vuoto                                         10
104800160905Immettere la password                                                         11
104900160905Password errata                                                               12
