000100060216      *===============================================================*
000200060216      *?TRUL39R * Reperimento importo risarcibile e numero legge RCV ?*
000300060216      *===============================================================*
000400060216
000500060216     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060216
000700060216      *---------------------------------------------------------------*
000800060216
000900060216      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001000060216      *
001100060216      * Codice tabella in esame
001200060217     d C_tab1          c                   const('GEI')
001300060217     d C_tab2          c                   const('RCV')
001400060216      *
001500060216      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001600060216      *
001700060216      * - Messaggi
001800060216     d $Msg            s             78    dim(2)  ctdata  perrcd(1)
001900060216      *
002000060216      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002100060216      *
002200060216      * Parametri
002300060216     d KPJBA         e ds
002400060216      *
002500060216     d TRUL39ds      e ds                  inz
002600060216      *
002700060216      * - Dati utente
002800060216     d TIBS34ds      e ds
002900060216     d dDatiUte      e ds
003000060216     d AZUTEds       e ds                  extname(AZUTE00F)
003100060216      *
003200060217      * Reperimento dati tabelle da TNTBE01L
003300060217     d TIBS02ds      e ds                  inz
003400060217      * - Tab. "GED" = Gestionale: divise
003500060217     d dGED          e ds                  inz
003600060216      * - Tab. "GEI" = Gestionale: importi vari
003700060216     d dGEI          e ds                  inz
003800060217      * - Tab. "RCV" = Storico importi limite risarcibili
003900060217     d dRCV          e ds                  inz
004000060216      *
004100060216      * - Controllo/Inversione date
004200060216     d WLBdat          ds                  inz
004300060216     d   G08dat                       8  0 inz
004400060216     d   G08inv                       8  0 inz
004500060216     d   G08err                       1    inz('3')
004600060216     d   G08tgi                       5  0 inz
004700060216
004800060216      *---------------------------------------------------------------*
004900060216      *  RIEPILOGO INDICATORI                                         *
005000060216      *---------------------------------------------------------------*
005100060216      * 90    - Errore generico                                       *
005200060216      *---------------------------------------------------------------*
005300060216
005400060216     c     *Entry        plist
005500060216     c                   parm                    KPJBA
005600060216      *
005700060216      * Operazioni iniziali
005800060216     c                   exsr      RoutInz
005900060216      *
006000060216      * Reperimento dati RCV in base alla data ricevuta
006100060216     c                   exsr      Rep_RCV
006200060216      *
006300060216      * Operazioni finali
006400060216     c                   exsr      RoutFin
006500060216      *
006600060216      *---------------------------------------------------------------*
006700060216      * Subroutine Iniziale                                           *
006800060216      *---------------------------------------------------------------*
006900060216     c     *InzSR        BEGSR
007000060216      *
007100060216      * Reperimento Dati del job (Utente/Operativi)
007200060216     c     *dtaara       define    §azute        azuteds
007300060216     c     *dtaara       define    §datiute      ddatiute
007400060216      *
007500060216     c                   in(E)     *dtaara
007600060216     c                   IF        %ERROR or RSUT = *blanks
007700060216     c                   clear                   Tibs34Ds
007800060216     c                   call      'TIBS34R'
007900060216     c                   parm                    Tibs34Ds
008000060216     c                   in        *dtaara
008100060216     c                   ENDIF
008200060216      *
008300060216     c                   ENDSR
008400060216      *
008500060216      *---------------------------------------------------------------*
008600060216      * Operazioni Iniziali                                           *
008700060216      *---------------------------------------------------------------*
008800060216     c     RoutInz       BEGSR
008900060216      *
009000060216      * Impostazione parametri (se ricevuti)
009100060216if  1c                   if        KPJBU  <> *blanks
009200060216     c                   movel     KPJBU         TRUL39ds
009300060216x   1c                   else
009400060216     c                   clear                   TRUL39ds
009500060216e   1c                   endif
009600060216      *
009700060216      * Ripulisco i campi di output
009800060216     c                   clear                   O39llr
009900060216     c                   clear                   O39lrp
010000060216     c                   clear                   O39err
010100060216     c                   clear                   O39msg
010200060216      *
010300060216if  1c                   if        I39tla =  'C'
010400060216     c                   exsr      RoutFin
010500060216e   1c                   endif
010600060216      *
010700060216      * Se ricevuto un tipo lancio errato: si assume quello di default
010800060216     c                   if            I39tla <> 'C'
010900060216     c                             and I39tla <> 'L'
011000060216     c                             and I39tla <> *blanks
011100060216     c                   clear                   I39tla
011200060216     c                   endif
011300060217      *
011400060217      * Se NON ricevuta la divisa: si assume la moneta di conto
011500060217if  1c                   if        I39div =  *blanks
011600060217     c                   exsr      Rep_GED
011700060217     c                   eval      I39div =  §GEDcn
011800060217e   1c                   endif
011900060216      *
012000060216      * Se NON ricevuta la data decorrenza: si assume la data odierna
012100060216if  1c                   if        I39dlr =  *zeros
012200060216     c                   clear                   WLBdat
012300060216     c                   eval      G08dat =  *date
012400060216     c                   call      'XSRDA8'
012500060216     c                   parm                    WLBdat
012600060216     c                   eval      I39dlr =  G08inv
012700060216e   1c                   endif
012800060216      *
012900060216     c                   ENDSR
013000060217      *
013100060217      *---------------------------------------------------------------*
013200060217      * Reperimento divisa da tab. GED/DN                             *
013300060217      *---------------------------------------------------------------*
013400060217     c     Rep_GED       BEGSR
013500060217      *
013600060217      * Reperimento tab. GED = divise Danni
013700060217     c                   clear                   dGED
013800060217     c                   clear                   TIBS02ds
013900060217     c                   eval      T02mod = 'C'
014000060217     c                   eval      T02sif = KNSIF
014100060217     c                   eval      T02cod = 'GED'
014200060217     c                   eval      T02ke1 = '1         '
014300060217      *
014400060217     c                   call      'TIBS02R'
014500060217     c                   parm                    KPJBA
014600060217     c                   parm                    TIBS02ds
014700060217      *
014800060217     c                   if        T02err = *blanks
014900060217     c                   movel     T02uni        dGED
015000060217     c                   else
015100060217     c                   eval      O39err =  'E'
015200060217     c                   eval      O39msg =  $Msg(1)
015300060217     c                   leavesr
015400060217     c                   endif
015500060217      *
015600060217     c                   ENDSR
015700060216      *
015800060216      *---------------------------------------------------------------*
015900060216      * Reperimento dati RCV in base alla data ricevuta               *
016000060216      *---------------------------------------------------------------*
016100060216     c     Rep_RCV       BEGSR
016200060216      *
016300060216      * Reperimento tab. GEI = importi vari
016400060217     c                   clear                   dGEI
016500060217     c                   clear                   TIBS02ds
016600060217     c                   eval      T02mod = 'C'
016700060217     c                   eval      T02sif = KNSIF
016800060217     c                   eval      T02cod = C_tab1
016900060217     c                   movel(p)  I39div        T02ke1
017000060217      *
017100060217     c                   call      'TIBS02R'
017200060217     c                   parm                    KPJBA
017300060217     c                   parm                    TIBS02ds
017400060217      *
017500060217     c                   if        T02err = *blanks
017600060217     c                   movel     T02uni        dGEI
017700060217     c                   else
017800060217     c                   eval      O39err =  'E'
017900060217     c                   eval      O39msg =  $Msg(1)
018000060217     c                   leavesr
018100060217     c                   endif
018200060217      *
018300060217      * Reperimento tab. RCV = Storico importi limite risarcib.
018400060217     c                   clear                   dRCV
018500060217     c                   clear                   TIBS02ds
018600060217     c                   eval      T02mod = 'C'
018700060217     c                   eval      T02sif = KNSIF
018800060217     c                   eval      T02cod = C_tab2
018900060217     c                   movel(p)  I39div        T02ke1
019000060217      *
019100060217     c                   call      'TIBS02R'
019200060217     c                   parm                    KPJBA
019300060217     c                   parm                    TIBS02ds
019400060217      *
019500060217     c                   if        T02err = *blanks
019600060217     c                   movel     T02uni        dRCV
019700060217     c                   endif
019800060216      *
019900060216      * Individuazione importo e legge corretti per data
020000060216sel 1c                   SELECT
020100060216      * - limite risarcibile valido dalla data in tab.GEI
020200060216w   1c                   WHEN      I39dlr >= §GEdlr
020300060216     c                   eval      O39llr =  §GEllr
020400060216     c                   eval      O39lrp =  §GElrp
020500060217      * - 5° limite risarcibile valido dalla data in tab.RCV
020600060217w   1c                   WHEN          §RCVss5 >  *zeros
020700060217     c                             and §RCVss5 >= I39dlr
020800060217     c                   eval      O39llr =  §RCVsl5
020900060217     c                   eval      O39lrp =  §RCVsi5
021000060217      * - 4° limite risarcibile valido dalla data in tab.RCV
021100060217w   1c                   WHEN          §RCVss4 >  *zeros
021200060217     c                             and §RCVss4 >= I39dlr
021300060217     c                   eval      O39llr =  §RCVsl4
021400060217     c                   eval      O39lrp =  §RCVsi4
021500060217      * - 3° limite risarcibile valido dalla data in tab.RCV
021600060217w   1c                   WHEN          §RCVss3 >  *zeros
021700060217     c                             and §RCVss3 >= I39dlr
021800060217     c                   eval      O39llr =  §RCVsl3
021900060217     c                   eval      O39lrp =  §RCVsi3
022000060217      * - 2° limite risarcibile valido dalla data in tab.RCV
022100060217w   1c                   WHEN          §RCVss2 >  *zeros
022200060217     c                             and §RCVss2 >= I39dlr
022300060217     c                   eval      O39llr =  §RCVsl2
022400060217     c                   eval      O39lrp =  §RCVsi2
022500060217      * - 1° limite risarcibile valido dalla data in tab.RCV
022600060217w   1c                   WHEN          §RCVss1 >  *zeros
022700060217     c                             and §RCVss1 >= I39dlr
022800060217     c                   eval      O39llr =  §RCVsl1
022900060217     c                   eval      O39lrp =  §RCVsi1
023000060217      * - ? ? ?
023100060216x   1c                   OTHER
023200060216     c                   eval      O39err =  'E'
023300060216     c                   eval      O39msg =  $Msg(2)
023400060216e   1c                   ENDSL
023500060216      *
023600060216     c                   ENDSR
023700060216      *
023800060216      *---------------------------------------------------------------*
023900060216      * Operazioni Finali                                             *
024000060216      *---------------------------------------------------------------*
024100060216     c     RoutFin       BEGSR
024200060216      *
024300060217if  1c                   if           I39tla = 'C'
024400060216     c                             or I39tla = 'L'
024500060217      *
024600060217     c                   clear                   TIBS02ds
024700060217     c                   eval      T02tla = 'C'
024800060217     c                   call      'TIBS02R'
024900060217     c                   parm                    KPJBA
025000060217     c                   parm                    TIBS02ds
025100060217      *
025200060216     c                   eval      *inLR = *on
025300060217      *
025400060217e   1c                   endif
025500060217      *
025600060217     c                   movel(p)  TRUL39ds      KPJBU
025700060216      *
025800060216     c                   return
025900060216      *
026000060216     c                   ENDSR
026100060216
026200060216      *---------------------------------------------------------------*
026300060216
026400060216** - $Msg -------------------------------------------------------------------*
026500060216Divisa non gestita in tab. "GEI"                                                 1
026600060216Non trovato importo valido per data decorrenza ricevuta                          2
