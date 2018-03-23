000100120217     /*PRM dbgview(*source)
000200120217     /*END
000300060907      *===============================================================*
000400060907      *?Reperimento cliente da numero serie segnacollo               ?*
000500060907      *===============================================================*
000600060907
000700060907     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000800060907
000900060907      *---------------------------------------------------------------*
001000060907
001100060907     fTABEL00F  if   e           k disk
001200060907     fTNTBE01L  if   e           k disk
001300060907     fAZORG01L  if   e           k disk
001400060907      *
001500060907     fFNLV03D   cf   e             workstn
001600060907     f                                     sfile(LV03S01:S01nrr)
001700060907
001800060907      *---------------------------------------------------------------*
001900060907
002000060907      *
002100060907      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
002200060907      *
002300121009      * -?Nr. di righe del sfl (SFLPAG)?
002400120217     d c_SflPag        c                   const(16)
002500120217
002600120217      * -?Costante per controllo "caratteri solo numerici"?
002700120217     d c_Digits        c                   const('0123456789')
002800060907      *
002900060907      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
003000060907      *
003100121009      * -?Messaggi di errore?
003200060907     d $Msg            s             78    dim( 4) ctdata  perrcd(1)
003300060907      *
003400060907      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
003500060907      *
003600121009      * -?Parametri ricevuti?
003700060907     d KPJBA         e ds
003800060907      *
003900121009      * -?Parametri x Controllo profilo utenti?
004000060907     d TIBS34ds      e ds                  inz
004100121009      * -?Ds di riferimento al file esterno AZUTE00F?
004200060907     d AZUTEds       e ds                  inz extname(AZUTE00F)
004300121009      * -?Ds per dati organigramma?
004400060907     d DDatiUte      e ds                  inz
004500060907      *
004600121009      * -?Tab. "3C"  = Clienti che inviano bolle partenza?
004700060907     d ds3C          e ds                  inz
004800120217      *
004900121009      * -?Tab. "3CL" = Serie tot. assegnate a gruppi d'unificante?
005000120217     d d3CL          e ds                  inz
005100060907      *
005200121009      * -?Tab. "3CP" = Serie parziali assegnate al cliente?
005300060907     d d3CP          e ds                  inz
005400101001      *
005500121009      * -?Tab. "3EW" = Serie parziali assegnate al cliente EasyWEB?
005600101001     d d3EW          e ds                  inz
005700060907      *
005800121009      * Parametri per pgm. TIBS69R (decodifica cliente)?
005900060907     d TIBS69ds      e ds                  inz
006000060907     d CNACOds       e ds                  extname(CNACO00F)  inz
006100060907     d CNINDds       e ds                  extname(CNIND00F)  inz
006200060907     d CNCLPds       e ds                  extname(CNCLP00F)  inz
006300060907     d FNCLSds       e ds                  extname(FNCLS00F)  inz
006400060907      *
006500060907     d Status         sds           333
006600060907     d   SDSpgm          *proc
006700060907      *
006800060907     d ds_KE2          ds                  inz
006900060907     d   V1Cnrs                            inz
007000060907     d   V1Cnsc                            inz
007100060907      *
007200060907     d ds_WK2          ds                  inz
007300060907     d   WK2nrs                            like(V1Cnrs)  inz
007400060907     d   WK2nsc                            like(V1Cnsc)  inz
007500060908      *
007600060908      *  ?P A R A M E T R I   P E R   O R D I N A M E N T O   S F L  ?*
007700060908      *
007800060908      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
007900121009      * -?Constants?
008000060908      *
008100121009      * ·?MaxKey - Maximum number of key fields allowed by this program?
008200060908     d MaxKey          c                   const(2)
008300121009      * ·?Sort order:?
008400121009      *  ?1 = Sort in an ascending sequence?
008500121009      *  ?2 = Sort in a descending sequence?
008600060908     d Ascendente      c                   const(1)
008700060908     d Discendente     c                   const(2)
008800121009      * ·?Key data type:?
008900121009      *  ? 0 = Signed binary?
009000121009      *  ? 2 = Signed zoned decimal?
009100121009      *  ? 3 = Signed packed decimal?
009200121009      *  ? 6 = Character with no national language sort sequence applied,?
009300121009      *       ?if specified?
009400121009      *  ? 7 = Unsigned packed decimal?
009500121009      *       ?All numbers will have the sign forced positive ('F'X).?
009600121009      *  ? 8 = Unsigned zoned decimal?
009700121009      *       ?All numbers will have the sign forced positive ('F'X).?
009800121009      *  ? 9 = Unsigned binary?
009900121009      *  ?14 = Date in form of DD/MM/YY?
010000121009      *  ?15 = Date in form of DD.MM.YYYY?
010100060908     d Numero          c                   const(2)
010200060908     d Carattere       c                   const(6)
010300060908     d Put             c                   const(1)
010400060908     d EndPut          c                   const(2)
010500060908     d Get             c                   const(3)
010600060908      *
010700060908      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
010800121009      * -?Data Structures?
010900121009      *  ?SflRcd     - The entire subfile record to pass to the sort?
011000121009      *  ?QLGSCB     - The sort request block for the QLGSORT API?
011100121009      *  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
011200121009      *  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
011300121009      *  ?QUSEC      - Error structure for the QLGSORT API?
011400060908      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
011500060908     d SflRcd          ds                  inz
011600060908     d  S1Cksc                             inz
011700060908     d  S1Dksc                             inz
011800101129     d  S1Ccba                             inz
011900060908     d  S1Cokd                             inz
012000120217     d*//  S1Cnsi                             inz
012100120217     d*//  S1Cnsf                             inz
012200120217     d  S1Drcl                             inz
012300120217     d  S1Cuni                             inz
012400120217     d  S1Duni                             inz
012500120217     d  S1H3cl                             inz
012600120217     d  S1H3cp                             inz
012700120217     d  S1H3ew                             inz
012800060908     d  Selected                      1a
012900060908      /COPY QSYSINC/QRPGLESRC,QLGSORT
013000060908     d QLGKL                         16    dim(MaxKey)
013100060908     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
013200060908     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
013300060908     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
013400060908     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
013500060908      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
013600060908      /COPY QSYSINC/QRPGLESRC,QUSEC
013700060908      *
013800060908      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
013900121009      * -?Standalone fields?
014000121009      *  ?Nrr        - Relative record number for subfile?
014100121009      *  ?SizeList   - Size of list?
014200121009      *  ?ReturnSize - Size of list returned by sort API?
014300060908      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
014400060908     d NotUsed         s             16a   inz
014500060908     d ReturnSize      s              9b 0 inz
014600060908     d SizeList        s              9b 0 inz
014700060908     d RrnLast         s              5i 0 inz
014800060908     d Nrr             s              5i 0 inz
014900121009      *  ?WrkSort    - Ordinamento impostato a video?
015000130219     d WrkSort         s              1  0 inz(-1)
015100060907      *
015200060907      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
015300060907      *
015400121009      * -?Flags?
015500120220     d $Called         s               n   inz
015600120217     d $InzS01         s               n   inz
015700120217     d $Fine           s               n   inz
015800120217     d $3CL            s               n   inz(*off)
015900120217     d Sav$3CL         s               n   inz(*off)
016000120217     d $3CP            s              1    inz(*off)
016100120217     d Sav$3CP         s              1    inz(*off)
016200101001     d $3EW            s               n   inz
016300101001     d Sav$3EW         s               n   inz
016400060907      *
016500121009      * -?Gestione subfile?
016600060907     d S01nrr          s                   like(C01rcd) inz
016700060907      *
016800121009      * -?Controllo nuove parzializzazioni?
016900120217     d SAVfls          s                   like(V1Cfls)  inz
017000060907     d SAVnrs          s                   like(V1Cnrs)  inz
017100060907     d SAVnsc          s                   like(V1Cnsc)  inz
017200121009      *        c
017300121009      * -?Campi di comodo?
017400060907     d wPag            s              4  0 inz
017500060907     d wRig            s              3  0 inz
017600120217     d SAVuni          s                   like(S1Cuni)  inz
017700120217     d SAVuniD         s                   like(S1Duni)  inz
017800060907      *
017900060907      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
018000060907      *
018100121009      * -?TABEL00F?
018200060907     c     K02tabel      klist
018300060907     c                   kfld                    TBLkut
018400060907     c                   kfld                    TBLcod
018500121009      * -?TNTBE01L?
018600060907     c     K03TBE01      klist
018700060907     c                   kfld                    TBEcod
018800060907     c                   kfld                    TBEke1
018900060907     c                   kfld                    TBEke2
019000060907     c     K02TBE01      klist
019100060907     c                   kfld                    TBEcod
019200060907     c                   kfld                    TBEke1
019300060907
019400060907      *---------------------------------------------------------------*
019500121009      * ?RIEPILOGO INDICATORI                                        ?*
019600060907      *---------------------------------------------------------------*
019700130219      *? 08    - Abilitazione F8=Ordinamento sfl                     ?*
019800130219      *? 10    - Comodo                                              ?*
019900130219      *? 28    - Emissione del messaggio di errore a video           ?*
020000130219      *? 30    - Pulizia del subfile                                 ?*
020100130219      *? 31    - NO emissione del subfile                            ?*
020200130219      *? 33    - Fine dati nel subfile         (sflend)              ?*
020300130219      *? 40    - Protezione campi Filiale e Serie segnacollo         ?*
020400130219      *?         + abilitazione F12=Ritorno e DISabilitazione F3=Fine?*
020500130219      *?         perchè richiamato da altro *pgm                     ?*
020600130219      *? 41    - Range totale in tab. "3CL"                          ?*
020700130219      *?N42N43 - Ordinamento SFL per Range Segnacolli                ?*
020800130219      *? 42N43 - Ordinamento SFL per Codice Cliente                  ?*
020900130219      *?N42 43 - Ordinamento SFL per Ragione Sociale Cliente         ?*
021000130219      *? 42 43 - Ordinamento SFL per Supporto Cli=>BRT               ?*
021100130219      *? 51    - Fil.  segnacollo errato o mancante                  ?*
021200130219      *? 52    - Serie segnacollo errata o mancante                  ?*
021300130219      ***53*** - Num.  segnacollo errato o mancante                   *
021400130219      *? 90    - Generico di errore                                  ?*
021500060907      *---------------------------------------------------------------*
021600060907
021700060907     c     *Entry        plist
021800060907     c                   parm                    KPJBA
021900060907      *
022000121009      *?Operazioni Iniziali?
022100060907     c                   exsr      RoutInz
022200060907      *
022300121009      *?Gestione Video?
022400060907do  1c                   DOW       $Fine   = *off
022500060907     c                   exsr      GesS01
022600060907e   1c                   ENDDO
022700060907      *
022800121009      *?Operazioni Finali?
022900060907     c                   exsr      RoutEnd
023000060907
023100060907      *---------------------------------------------------------------*
023200060907      *?Operazioni Iniziali                                          ?*
023300060907      *---------------------------------------------------------------*
023400060907     c     RoutInz       BEGSR
023500060907      *
023600121009      *?Reperimento dati job?
023700060907     c                   exsr      DatiJob
023800060907      *
023900121009      *?Impostazione nome programma a video?
024000060907     c                   movel     SDSpgm        V1Tpgm
024100060907      *
024200121009      *?NON visualizzazione iniziale del subfile?
024300060907     c                   eval      *in30   = *on
024400120217      *
024500120217      /free
024600120217       // -?Verifica se *pgm richiamato:?
024700120217       //  ?test delle prime 5 posizioni della KPJBA per sapere se il?
024800120217       //  ?pgm è stato richiamato da un altro *pgm o da menù.?
024900120220       $Called = ( %subst( KPJBU : 1 : 5 ) > *zero  and
025000120220                   %check( c_Digits : %subst( KPJBU : 1 : 5 ) )
025100120220                                             = *zero );
025200120217       // -?Impostazione dei parametri (SE ricevuti)?
025300120220       *in40 = $Called;
025400120220       if  $Called;
025500120217         V1Cfls  = %int( %subst( KPJBU : 1 : %len(V1Cfls) ) );
025600120217         V1Cnrs  = %int( %subst( KPJBU : %len(V1Cfls) + 1 :
025700120217                                         %len(V1Cnrs) ) );
025800120217         exsr  CtrC01;
025900120217         $InzS01 = (not *in90);
026000120217       endif;
026100120217      /end-free
026200060907      *
026300060907     c                   ENDSR
026400060907
026500060907      *---------------------------------------------------------------*
026600060907      *?Reperimento Dati del job (Utente/Operativi)                  ?*
026700060907      *---------------------------------------------------------------*
026800060907     c     DatiJob       BEGSR
026900060907      *
027000060907     c     *dtaara       define    §azute        azuteds
027100060907     c     *dtaara       define    §datiute      ddatiute
027200060907      *
027300060907     c                   in(E)     *dtaara
027400060907     c                   IF        %ERROR or RSUT = *blanks
027500060907     c                   clear                   Tibs34Ds
027600060907     c                   call      'TIBS34R'
027700060907     c                   parm                    Tibs34Ds
027800060907     c                   in        *dtaara
027900060907     c                   ENDIF
028000060907      *
028100060907     c                   ENDSR
028200060907
028300060907      *---------------------------------------------------------------*
028400060907      *?Operazioni Finali                                            ?*
028500060907      *---------------------------------------------------------------*
028600060907     c     RoutEnd       BEGSR
028700060907      *
028800060907     c                   clear                   TIBS69ds
028900060907     c                   eval      I69tla  = 'C'
029000060907     c                   call      'TIBS69R'
029100060907     c                   parm                    TIBS69ds
029200060907     c                   parm                    CNACOds
029300060907     c                   parm                    CNINDds
029400060907     c                   parm                    CNCLPds
029500060907     c                   parm                    FNCLSds
029600060907      *
029700060907     c                   eval      *inLR   = *on
029800060907     c                   return
029900060907      *
030000060907     c                   ENDSR
030100060907
030200060907      *---------------------------------------------------------------*
030300060907      *?Gestione videata S01                                         ?*
030400060907      *---------------------------------------------------------------*
030500060907     c     GesS01        BEGSR
030600060907      *
030700121009      *?Inizializzazione videata?
030800060907if  1c                   if        $InzS01 = *on
030900060907     c                   exsr      InzS01
031000060907     c                   movel     *off          $InzS01
031100060907e   1c                   endif
031200060907      *
031300121009      *?Posizionamento cursore sul sflctl C01 (SE NO *err)?
031400121009      *                                      ?( & NON chiamato)?
031500060907     c                   if             NOT *in90
031600060907     c                             and  NOT *in30
031700120220     c                             and  NOT $Called
031800060907     c                   eval      *in52   = *on
031900060907     c                   endif
032000060907      *
032100121009      *?Scrittura di testata e di riga tasti funzionali abilitati?
032200060907if  1c                   if        NOT *in90
032300060907     c                   write     LV03T01
032400060907     c                   write     LV03Z01
032500060907e   1c                   endif
032600121009      *?Emissione videata?
032700060908     c                   exfmt     LV03C01
032800060907     c                   setoff                                       28  90
032900060907     c                   clear                   V1Dmsg
033000060907      *
033100060907sel 1c                   SELECT
033200121009      *?F3=Fine?
033300060907w   1c                   WHEN      *inKC
033400060907     c                   exsr      F03S01
033500060908      *
033600121009      *?F5=Rivisualizzazione?
033700060908w   1c                   WHEN      *inKE
033800060908     c                   exsr      F05S01
033900060908      *
034000121009      *?F8=Ordinamento x Cliente / Range?
034100060908w   1c                   WHEN      *inKH
034200060908     c                   exsr      F08S01
034300121009      *?F12=Ritorno?
034400120220w   1c                   WHEN      *inKL
034500120220     c                   exsr      F03S01
034600060907      *
034700121009      *?Controllo nuove selezioni?
034800060907x   1c                   OTHER
034900060907     c                   exsr      CtrC01
035000060907if  2c                   if        *in90
035100060907     c                   leavesr
035200060907e   2c                   endif
035300060907      *
035400060907e   1c                   ENDSL
035500060907      *
035600060907     c                   ENDSR
035700060907
035800060907      *---------------------------------------------------------------*
035900060907      *?Gestione tasto funzionale F3 da videata C01/S01              ?*
036000060908      *?F3=Fine                                                      ?*
036100060907      *---------------------------------------------------------------*
036200060907     c     F03S01        BEGSR
036300060907      *
036400121009      *?Chiusura del programma?
036500060907     c                   eval      $Fine   = *on
036600060907      *
036700060907     c                   ENDSR
036800060908
036900060908      *---------------------------------------------------------------*
037000060908      *?Gestione tasto funzionale F5 da videata C01/S01              ?*
037100060908      *?F5=Rivisualizzazione                                         ?*
037200060908      *---------------------------------------------------------------*
037300060908     c     F05S01        BEGSR
037400060908      *
037500121009      *?Ricaricamento del subfile?
037600060908     c                   eval      $InzS01 = *on
037700060908      *
037800060908     c                   ENDSR
037900060908
038000060908      *---------------------------------------------------------------*
038100060908      *?Gestione tasto funzionale F8 da videata C01/S01              ?*
038200060908      *?F8=Cambio ordinamento sfl                                    ?*
038300060908      *---------------------------------------------------------------*
038400060908     c     F08S01        BEGSR
038500060908      *
038600130219      * -?Passaggio all'ordinamento successivo?
038700130219if  1c                   if        WrkSort  < 3
038800130219     c                   eval      WrkSort += 1
038900130219x   1c                   else
039000130219     c                   clear                   WrkSort
039100130219e   1c                   endif
039200130219      *
039300130219      * -?Ordinamento subfile?
039400060908if  1c                   if        S01nrr  >  1
039500060908     c                   exsr      Sort
039600060908e   1c                   endif
039700130219      *
039800130219      * -?Aggiornamento descrizione tasto funzionale F8?
039900130219sel 1c                   select
040000130219w   1c                   when      WrkSort = *zero
040100130219     c                   eval      Z1Df08  =  'F8=Ordinam. x Cliente'
040200130219w   1c                   when      WrkSort = 1
040300130219     c                   eval      Z1Df08  =  'F8=Ordinam. x Rag.Soc'
040400130219w   1c                   when      WrkSort = 2
040500130219     c                   eval      Z1Df08  =  'F8=Ordinam. x Supp.  '
040600130219w   1c                   when      WrkSort = 3
040700130219     c                   eval      Z1Df08  =  'F8=Ordinam. x Range  '
040800130219w   1c                   endsl
040900060908      *
041000060908     c                   ENDSR
041100060907
041200060907      *---------------------------------------------------------------*
041300060907      *?Inizializzazione videata S01                                 ?*
041400060907      *---------------------------------------------------------------*
041500060907     c     InzS01        BEGSR
041600060907      *
041700121009      *?Pulizia subfile?
041800060907     c                   seton                                        3031
041900060907     c                   write     LV03C01
042000060908     c                   setoff                                         3133
042100060907      *
042200060907     c                   clear                   C01rcd
042300060907     c                   clear                   C01csr
042400060907     c                   clear                   S01nrr
042500060907     c                   clear                   V1Dmsg
042600060907     c                   setoff                                       28  90
042700060907     c                   movea     *zeros        *in(41)
042800060907      *
042900121009      *?Caricamento dei dati da presentare nel sfl:?
043000121009      *?(non per pagina, vista l'abilitazione di F8=Sort)?
043100121009      *  ?ciclo di lettura tab. "3C"?
043200130219     c                   reset                   WrkSort
043300060907     c                   exsr      PosizInz
043400060907     c                   dow       NOT %eof(TABEL00F)
043500120217     c*** non x pag.:              and S01nrr < c_SflPag
043600060907     c                   exsr      CarS01
043700060907     c     K02tabel      reade     TABEL
043800060907     c                   enddo
043900060908      *
044000121009      *?Memorizzazione NRR dell'ultimo record scritto nel sfl?
044100060908     c                   eval      RRNlast =  S01nrr
044200060907      *
044300121009      *?Memorizzazione dati attualmente visualizzati?
044400120217     c                   eval      SAVfls  = V1Cfls
044500060907     c                   eval      SAVnrs  = V1Cnrs
044600060907     c                   eval      SAVnsc  = V1Cnsc
044700060907      *
044800121009      *?Impaginazione subfile?
044900121009      *?-> forza l'impaginazione sul 1° rec. del subfile?
045000060907if  1c                   if        S01nrr  >  *zeros
045100120217     c***  S01nrr        div       c_SflPag      wPag
045200060908     c***                mvr                     wRig
045300120217     c***                eval      C01rcd  =  wPag * c_SflPag
045400060908     c***                if        wRig    >  *zeros
045500060908     c***                eval      C01rcd  =  C01rcd + 1
045600060908     c***                else
045700120217     c***                eval      C01rcd  =  C01rcd - c_SflPag + 1
045800060908     c***                endif
045900060908     c                   eval      C01rcd  =  1
046000060907x   1c                   else
046100060907     c                   clear                   C01rcd
046200060907e   1c                   endif
046300060907      *
046400121009      *?Visualizzazione subfile SOLO SE ci sono dati?
046500060907     c                   eval      *in30   = (S01nrr <= *zeros)
046600121009      *?Eventuale attivazione del SFLEND?
046700060907     c                   eval      *in33   = (%eof(TABEL00F))
046800060908      *
046900121009      *?Impostazione iniziale descrizione tasto funzionale F8?
047000120217     c                   eval      *in08   = ((Sav$3CL = *on   or
047100120217     c                                         Sav$3CP = *on   or
047200101001     c                                         Sav$3EW = *on)  and
047300060908     c                                        S01nrr  > 1)
047400060911      *
047500121009      *?Forzatura "ordinamento x range" come iniziale?
047600060911     c                   if        *in08
047700060911     c                   exsr      F08S01
047800120217     c                   else
047900120217     c                   clear                   Z1Df08
048000060911     c                   endif
048100060907      *
048200060907     c                   ENDSR
048300060907
048400060907      *---------------------------------------------------------------*
048500060907      *?Posizionamento iniziale sul file TABEL00F                    ?*
048600060907      *---------------------------------------------------------------*
048700060907     c     PosizInz      BEGSR
048800060907      *
048900060907     c                   eval      TBLkut  = 1
049000060907     c                   eval      TBLcod  = '3C'
049100120217     c     K02tabel      setll     TABEL
049200060907     c     K02tabel      reade     TABEL
049300060907      *
049400060907     c                   ENDSR
049500060907
049600060907      *---------------------------------------------------------------*
049700060907      *?Caricamento del singolo record nel subfile S01               ?*
049800060907      *---------------------------------------------------------------*
049900060907     c     CarS01        BEGSR
050000060907      *
050100060907     c                   movel     TBLuni        ds3C
050200060907      *
050300121009      *?Selezione record?
050400060907sel 1c                   select
050500121009      *?- Scartati records annullati?
050600060907w   1c                   when      TBLflg <> *blanks
050700060907     c                   leavesr
050800121009      *?- Scartati records relativi a clienti di fil. diversa?
050900120217w   1c*//                when      %subst(TBLkey : 1 : 3) <>
051000120217     c*//                          %trim(%editw(V1Cfls : '0   '))
051100120220w   1c                   when           (§3Ccba <> 'ESWEB'
051200120220     c                             and   §3Cfls <> V1Cfls)
051300060907     c                   leavesr
051400121009      *?- Scartati records con serie diversa?
051500060907w   1c                   when      §3Cnrs <> V1Cnrs
051600060907     c                   leavesr
051700060907e   1c                   endsl
051800060907      *
051900101001     c                   reset                   $3EW
052000101001     c                   reset                   $3CP
052100120217     c                   reset                   $3CL
052200121009      *?Controllo segnacollo in tab. "3EW"?
052300101001if  1c                   if        §3Ccba  = 'ESWEB'
052400101001     c                   exsr      Search_3EW
052500121009      *?Controllo segnacollo in tab. "3CP"?
052600101001x   1c                   else
052700060907     c                   exsr      Search_3CP
052800101001e   1c                   endif
052900120220      *
053000120220if  1c                   if             §3Ccba  =  'ESWEB'
053100120220     c                             and  §3EWfls <> V1Cfls
053200120220     c                   leavesr
053300120220e   1c                   endif
053400101001      *
053500121009      *?Controllo filiale/serie segnacollo in tab. "3CL"?
053600120217     c                   exsr      Search_3CL
053700120217      *
053800120217if  1c                   if        $3CL    = *off   and
053900120217     c                             $3CP    = *off   and
054000101001     c                             $3EW    = *off
054100070316     c                   leavesr
054200101001e   1c                   endif
054300060907      *
054400121009      *?Impostazione campi nel record del subfile?
054500060907     c                   clear                   LV03S01
054600121009      *?- Cliente?
054700060907     c                   movel     TBLkey        S1Cksc
054800060908if  1c                   if        S1Cksc <> ACOksc
054900060907     c                   clear                   TIBS69ds
055000060907     c                   clear                   CNACOds
055100060907     c                   clear                   CNINDds
055200060907     c                   clear                   CNCLPds
055300060907     c                   clear                   FNCLSds
055400060908     c                   eval      I69kac  = S1Cksc
055500060907     c                   call      'TIBS69R'
055600060907     c                   parm                    TIBS69ds
055700060907     c                   parm                    CNACOds
055800060907     c                   parm                    CNINDds
055900060907     c                   parm                    CNCLPds
056000060907     c                   parm                    FNCLSds
056100060908e   1c                   endif
056200060907     c                   movel     ACOrag        S1Dksc
056300120217      * -?Unificante?
056400120217     c                   eval      S1Cuni  = §3Ccks
056500120217if  1c                   if        S1Cuni <> SAVuni
056600120217     c                   eval      SAVuni  = S1Cuni
056700120217     c                   clear                   TIBS69ds
056800120217     c                   clear                   CNACOds
056900120217     c                   clear                   CNINDds
057000120217     c                   clear                   CNCLPds
057100120217     c                   clear                   FNCLSds
057200120217     c                   eval      I69kac  = S1Cuni
057300120217     c                   call      'TIBS69R'
057400120217     c                   parm                    TIBS69ds
057500120217     c                   parm                    CNACOds
057600120217     c                   parm                    CNINDds
057700120217     c                   parm                    CNCLPds
057800120217     c                   parm                    FNCLSds
057900120217     c                   movel     ACOrag        SAVuniD
058000120217e   1c                   endif
058100120217     c                   movel     SAVuniD       S1Duni
058200121009      *?- Supporto Cliente a Bartolini?
058300101025     c                   eval      S1Ccba  = §3Ccba
058400121009      *?- Flag "Partito con procedura"?
058500060907     c                   eval      S1Cokd  = §3Cokd
058600121009      *?- Range segnacolli?
058700121009      *  ?(se trovati record in tab. "3CL" o "3CP" o "3EW")?
058800120217sel 1c                   select
058900120217w   1c                   when      $3EW    = *on
059000101001     c                   eval      Sav$3EW = *on
059100120217     c*//                eval      S1Cnsi  = §3EWnsi
059200120217     c*//                eval      S1Cnsf  = §3EWnsf
059300120217     c                   eval      S1Drcl  = %editc( §3EWnsi : '2' )
059400120217     c                                     + '  '
059500120217     c                                     + %editc( §3EWnsf : '2' )
059600120217w   1c                   when      $3CP    = *on
059700120217     c                   eval      Sav$3CP = *on
059800120217     c*//                eval      S1Cnsi  = WK2nsc
059900120217     c*//                eval      S1Cnsf  = §3CPal
060000120217     c                   eval      S1Drcl  = %editc( WK2nsc : '2' )
060100120217     c                                     + '  '
060200120217     c                                     + %editc( §3CPal : '2' )
060300120217w   1c                   when      $3CL    = *on
060400120217     c                   eval      Sav$3CL = *on
060500120217     c                   eval      S1Drcl  = §3CLdes
060600120217e   1c                   endsl
060700120217      *
060800121009      *?- Campi hidden?
060900120217     c                   eval      S1H3CL = $3CL
061000120217     c                   eval      S1H3CP = $3CP
061100120217     c                   eval      S1H3EW = $3EW
061200120217      *
061300121009      *?- Impostazione indicatori per attributi di visualizzazione?
061400120220     c                   eval      *in40 = $Called
061500120217     c                   eval      *in41 = ($3CL  and  $3CP <> *on  and
061600120217     c                                                 Not $3EW)
061700060907      *
061800121009      *?Scrittura record  nel subfile?
061900060907     c                   add       1             S01nrr
062000060907     c                   write     LV03S01
062100060907      *
062200060907     c                   ENDSR
062300120217      /free
062400120217
062500120217       //--------------------------------------------------------------
062600120217       //?Controllo segnacollo in tablla "3CL"                         ?
062700120217       //--------------------------------------------------------------
062800120217       BEGSR  Search_3CL;
062900120217
063000120217         $3CL = *off;
063100120217
063200120217         // -?Ricerca eventuali serie completa fissata in tab. "3CL"?
063300120217         TBEcod = '3CL';
063400120217         TBEke1 = %editc( §3Cfls : 'X' );
063500120217         TBEke2 = %editc( §3Cnrs : 'X' );
063600120217         chain  K03TBE01  TNTBE000;
063700120217         if  %found(TNTBE01L)  and  TBEatb = *blank;
063800120217           $3CL = *on;
063900120217           d3CL = TBEuni;
064000120217         endif;
064100120217
064200120217       ENDSR;
064300120217
064400120217      /end-free
064500060907
064600060907      *---------------------------------------------------------------*
064700060907      *?Controllo segnacollo in tablla "3CP"                         ?*
064800060907      *---------------------------------------------------------------*
064900060907     c     Search_3CP    BEGSR
065000060907      *
065100070316     c***                eval      $3CP    = *off
065200070316     c                   eval      $3CP    = '2'
065300060907      *
065400121009      *?Ricerca eventuali serie parziali assegnate al cliente:?
065500060907      *
065600121009      *?A) se per il cliente NON esistono record,?
065700121009      *   ?NON seleziono per numero serie e numero segnacollo?
065800060907     c                   eval      TBEcod  = '3CP'
065900060907     c                   eval      TBEke1  = TBLkey
066000060907     c     K02TBE01      setll     TNTBE000
066100060907if  1c                   if        NOT %equal
066200070316     c***                eval      $3CP    = '2'
066300060907     c                   leavesr
066400060907e   1c                   endif
066500060907      *
066600121009      *?B) altrimenti, se è stato immesso un numero segnacollo,?
066700121009      *   ?seleziono per numero serie e numero segnacollo?
066800060907do  1c                   DO        *hival
066900060907      *
067000060907     c     K02TBE01      reade     TNTBE000
067100121009      *?- E.o.F.?
067200060907if  2c                   if        %eof(TNTBE01L)
067300060907     c                   leave
067400060907e   2c                   endif
067500121009      *?- Rec. annullato?
067600060907if  2c                   if        TBEatb <> *blanks
067700060907     c                   iter
067800060907e   2c                   endif
067900121009      *?- Serie diversa - DOVREBBE essere impossibile !!!?
068000060907if  2c                   if        %subst(TBEke2 : 1 : 2)  <>
068100060907     c                             %trim(%editw(§3Cnrs : '0  '))
068200060907     c                   iter
068300060907e   2c                   endif
068400121009      *?- Ctrl serie & range segnacolli?
068500060907     c                   movel     TBEke2        ds_WK2
068600060907     c                   movel     TBEuni        d3CP
068700060907if  2c                   if             WK2nrs  = V1Cnrs
068800060907     c                             and (V1Cnsc  = *zeros
068900060907     c                              OR (WK2nsc <= V1Cnsc
069000060907     c                              and §3CPal >= V1Cnsc))
069100070316     c                   eval      $3CP    = *on
069200060907     c                   leavesr
069300070316x   2c                   else
069400070316     c                   eval      $3CP    = *off
069500070316     c                   iter
069600060907e   2c                   endif
069700060907      *
069800060907e   1c                   ENDDO
069900060907      *
070000060907     c                   ENDSR
070100101001      /free
070200101001
070300101001       //--------------------------------------------------------------
070400101001       //?Controllo segnacollo in tablla "3EW"                         ?
070500101001       //--------------------------------------------------------------
070600101001       BEGSR  Search_3EW;
070700101001
070800101001         // -?Ricerca eventuali serie parziali assegnate al cliente:?
070900101001
071000101001         // -?A) se per il cliente (UNIFICANTE) NON esistono record,?
071100101001         //     ?NON seleziono per numero serie e numero segnacollo?
071200101001         TBEcod = '3EW';
071300101001         TBEke1 = '0' + %editc( §3Ccks : 'X' );
071400101001         setll  K02TBE01  TNTBE000;
071500101001         if  NOT %equal(TNTBE01L);
071600101001           leavesr;
071700101001         endif;
071800101001
071900101001         // -?B) altrimenti, se è stato immesso un numero segnacollo,?
072000101001         //     ?seleziono per numero serie e numero segnacollo?
072100101001         DoU  %eof(TNTBE01L);
072200101001
072300101001           reade  K02TBE01  TNTBE000;
072400101001
072500101001           // -?E.o.F.?
072600101001           if  %eof(TNTBE01L);
072700101001             leave;
072800101001           endif;
072900101001
073000101001           // -?Rec. annullato?
073100101001           if  TBEatb <> *blank;
073200101001             iter;
073300101001           endif;
073400101001
073500101001           d3EW = TBEuni;
073600101001           // -?Serie diversa - DOVREBBE essere impossibile !!!?
073700101001           if  §3EWnrs <> §3Cnrs;
073800101001             iter;
073900101001           endif;
074000101001           // -?Ctrl serie & range segnacolli?
074100101001           if  §3EWnrs  = V1Cnrs   and
074200101001              (V1Cnsc   = *zero    or
074300101001              (§3EWnsi <= V1Cnsc   and   §3EWnsf >= V1Cnsc));
074400101001             $3EW = *on;
074500101001             leavesr;
074600101001           else;
074700101001             $3EW = *off;
074800101001             iter;
074900101001           endif;
075000101001
075100101001           reade  K02TBE01  TNTBE000;
075200101001
075300101001         ENDDO;
075400101001
075500101001       ENDSR;
075600101001
075700101001      /end-free
075800060907      *---------------------------------------------------------------*
075900060907      *?Controllo dati in videata C01                                ?*
076000060907      *---------------------------------------------------------------*
076100060907     c     CtrC01        BEGSR
076200060907      *
076300060907     c                   movea     *zeros        *in(50)
076400060907      *
076500121009      *?Controllo/Decodifica P.O. Segnacollo?
076600120217     c                   clear                   V1Dfls
076700120217if  1c                   IF        V1Cfls  = *zeros
076800060907     c                   seton                                        285190
076900060907     c                   eval      V1Dmsg  = $Msg(1)
077000060907     c                   leavesr
077100060907x   1c                   ELSE
077200120217     c     V1Cfls        chain     AZORG
077300060907if  2c                   if             %found(AZORG01L)
077400060907     c                             and  ORGfva  = *blanks
077500060907     c***                          and (ORGfag  = 'F'
077600060907     c***                           or  ORGfag  = 'A')
077700120217     c                   eval      V1Dfls  = ORGdes
077800060907if  3c                   if        ORGfag  = 'V'
077900060907     c                   seton                                        285190
078000060907     c                   eval      V1Dmsg  = $Msg(2)
078100060907     c                   leavesr
078200060907e   3c                   endif
078300060907x   2c                   else
078400060907     c                   seton                                        285190
078500060907     c                   eval      V1Dmsg  = $Msg(2)
078600060907     c                   leavesr
078700060907e   2c                   endif
078800060907e   1c                   ENDIF
078900060907      *
079000121009      *?Controllo Serie?
079100060907sel 1c                   select
079200060907w   1c                   when      V1Cnrs  = *zeros
079300060907     c                   seton                                        285290
079400060907     c                   eval      V1Dmsg  = $Msg(3)
079500060907     c                   leavesr
079600060907w   1c                   when      V1Cnrs  < *zeros
079700060907     c                   seton                                        285290
079800060907     c                   eval      V1Dmsg  = $Msg(4)
079900060907     c                   leavesr
080000060907e   1c                   endsl
080100060907      *
080200121009      *?Controllo Segnacollo?
080300121009      *?=> facoltativo !?
080400060907      *
080500121009      *?Impostazione inizializzazione del subfile?
080600120217     c                   if             V1Cfls <> SAVfls
080700060907     c                             or   V1Cnrs <> SAVnrs
080800060907     c                             or   V1Cnsc <> SAVnsc
080900060907     c                   eval      $InzS01 = *on
081000060907     c                   endif
081100060907      *
081200060907     c                   ENDSR
081300060908
081400060908      *---------------------------------------------------------------*
081500060908      *?Ordinamento subfile                                          ?*
081600060908      *?  Subroutine - Sort                                          ?*
081700060908      *?  This subroutine sorts the subfile records.                 ?*
081800060908      *---------------------------------------------------------------*
081900060908     c     Sort          BEGSR
082000060908      *
082100060908     c                   eval      C01rcd  =  1
082200060908     c                   clear                   C01csr
082300060908     c                   clear                   S01nrr
082400060908     c                   clear                   V1Dmsg
082500060908     c                   setoff                                       28  90
082600060908     c*** non usato:     eval      *in32   =  *off
082700060908     c                   movea     *zeros        *in(50)
082800060908      *
082900121009      *?Initialize the key fields to sort on. There is one extra field not in?
083000121009      *?the subfile -- Selected -- that is added to the record. This field?
083100121009      *?is used to place selected records in front of nonselected records.?
083200060908      *
083300060908     c                   clear                   QLGKL
083400060908     c                   clear                   QLGSKL
083500060908     c                   clear                   QLGSCB
083600060908     c                   clear                   QLGSCB00
083700060908      *
083800121009      *?Gestione della scelta ordinamento.?
083900060908sel 1c                   SELECT
084000121009      *?Ordinamento per Range/Cliente?
084100130219w   1c                   WHEN      WrkSort  = *zero
084200121009      *  ?2 campi chiave?
084300060908     c                   eval      QLGNBRK  = 2
084400121009      *  ?1° campo: range segnacolli (S1Drcl)?
084500121009      *            ?a posizone   51, 20 byte, char, ascending.?
084600120217     c                   eval      QLGSP    = 51
084700120217     c                   eval      QLGSS    = %size(S1Drcl)
084800120217     c                   eval      QLGDT    = Carattere
084900060908     c                   eval      QLGSO    = Ascendente
085000060908     c                   eval      QLGKL(1) = QLGSKL
085100121009      *  ?2° campo: codice cliente (S1Cksc)?
085200121009      *            ?a posizone    1,  7 byte, dec,  ascending.?
085300060908     c                   eval      QLGSP    = 1
085400060908     c                   eval      QLGSS    = %size(S1Cksc)
085500060908     c                   eval      QLGDT    = Numero
085600060908     c                   eval      QLGSO    = Ascendente
085700060908     c                   eval      QLGKL(2) = QLGSKL
085800060908      *
085900121009      *?Ordinamento per Cliente/Range?
086000130219w   1c                   WHEN      WrkSort  = 1
086100121009      *  ?2 campi chiave?
086200060908     c                   eval      QLGNBRK  = 2
086300121009      *  ?1° campo: codice cliente (S1Cksc)?
086400130219      *            ?a posizone    1,  7 byte, dec,  ascending.?
086500060908     c                   eval      QLGSP    = 1
086600060908     c                   eval      QLGSS    = %size(S1Cksc)
086700060908     c                   eval      QLGDT    = Numero
086800060908     c                   eval      QLGSO    = Ascendente
086900060908     c                   eval      QLGKL(1) = QLGSKL
087000121009      *  ?2° campo: range segnacolli (S1Drcl)?
087100121009      *            ?a posizone   51, 20 byte, char, ascending.?
087200120217     c                   eval      QLGSP    = 51
087300120217     c                   eval      QLGSS    = %size(S1Drcl)
087400120217     c                   eval      QLGDT    = Carattere
087500060908     c                   eval      QLGSO    = Ascendente
087600060908     c                   eval      QLGKL(2) = QLGSKL
087700130219      *
087800130219      *?Ordinamento per Rag.Soc.Cliente/Range?
087900130219w   1c                   WHEN      WrkSort  = 2
088000130219      *  ?2 campi chiave?
088100130219     c                   eval      QLGNBRK  = 2
088200130219      *  ?1° campo: ragione sociale cliente (S1Dksc)?
088300130219      *            ?a posizone    8, 37 byte, char, ascending.?
088400130219     c                   eval      QLGSP    = 8
088500130219     c                   eval      QLGSS    = %size(S1Dksc)
088600130219     c                   eval      QLGDT    = Carattere
088700130219     c                   eval      QLGSO    = Ascendente
088800130219     c                   eval      QLGKL(1) = QLGSKL
088900130219      *  ?2° campo: range segnacolli (S1Drcl)?
089000130219      *            ?a posizone   51, 20 byte, char, ascending.?
089100130219     c                   eval      QLGSP    = 51
089200130219     c                   eval      QLGSS    = %size(S1Drcl)
089300130219     c                   eval      QLGDT    = Carattere
089400130219     c                   eval      QLGSO    = Ascendente
089500130219     c                   eval      QLGKL(2) = QLGSKL
089600130219      *
089700130219      *?Ordinamento per Supporto_Cli=>BRT/Range?
089800130219w   1c                   WHEN      WrkSort  = 3
089900130219      *  ?2 campi chiave?
090000130219     c                   eval      QLGNBRK  = 2
090100130219      *  ?1° campo: supporto Cli=>BRT (S1Ccba)?
090200130219      *            ?a posizone   49,  5 byte, char, ascending.?
090300130219     c                   eval      QLGSP    = 49
090400130219     c                   eval      QLGSS    = %size(S1Ccba)
090500130219     c                   eval      QLGDT    = Carattere
090600130219     c                   eval      QLGSO    = Ascendente
090700130219     c                   eval      QLGKL(1) = QLGSKL
090800130219      *  ?2° campo: range segnacolli (S1Drcl)?
090900130219      *            ?a posizone   51, 20 byte, char, ascending.?
091000130219     c                   eval      QLGSP    = 51
091100130219     c                   eval      QLGSS    = %size(S1Drcl)
091200130219     c                   eval      QLGDT    = Carattere
091300130219     c                   eval      QLGSO    = Ascendente
091400130219     c                   eval      QLGKL(2) = QLGSKL
091500060908      *
091600060908e   1c                   ENDSL
091700060908      *
091800121009      *?Load other sort parameters.?
091900060908     c                   eval      QLGLB    = 80 + 16 * MaxKey
092000060908     c                   eval      QLGRL    = %size(SflRcd) - 1
092100060908     c                   eval      QLGRT    = 8
092200060908     c                   eval      QLGOKL   = 80
092300060908     c                   eval      QLGLKE   = 16
092400060908     c                   eval      QLGLSS   = 290
092500121009      *?Initialize Sort I/O API fields.?
092600060908     c                   eval      QLGRL00  = QLGRL
092700060908     c                   eval      QLGRC00  = 1
092800060908     c                   clear                   QUSEI
092900060908     c                   eval      QUSBPRV  = %size(QUSEC)
093000121009      *?First step - Initialize the sort routine.?
093100060908     c                   call      'QLGSORT'
093200060908     c                   parm                    QLGSCB
093300060908     c                   parm                    NotUsed
093400060908     c                   parm                    NotUsed
093500060908     c                   parm                    SizeList
093600060908     c                   parm                    ReturnSize
093700060908     c                   parm                    QUSEC
093800060908      *
093900121009      *?Next step - Write records to I/O routine.?
094000060908     c                   eval      QLGRT00  = Put
094100060908do  1c                   DO        RrnLast       Nrr
094200060908     c     Nrr           chain     LV03S01
094300121009      *?Solo le righe con Selected = 'Y' sono riordinate,?
094400121009      *?quindi per fare un ordinamento di tutte le righe?
094500121009      *?metto 'Y' sempre.?
094600060908     c                   eval      Selected = 'Y'
094700060908     c                   clear                   QUSEI
094800060908     c                   eval      QUSBPRV  = %size(QUSEC)
094900060908     c                   call      'QLGSRTIO'
095000060908     c                   parm                    QLGSCB00
095100060908     c                   parm                    SflRcd
095200060908     c                   parm                    NotUsed
095300060908     c                   parm                    SizeList
095400060908     c                   parm                    NotUsed
095500060908     c                   parm                    QUSEC
095600060908e   1c                   ENDDO
095700121009      *?Next step - Signal end of input, clear subfile for reload.?
095800060908     c                   eval      QLGRT00  = EndPut
095900060908     c                   clear                   QUSEI
096000060908     c                   eval      QUSBPRV  = %size(QUSEC)
096100060908      *
096200060908     c                   call      'QLGSRTIO'
096300060908     c                   parm                    QLGSCB00
096400060908     c                   parm                    SflRcd
096500060908     c                   parm                    NotUsed
096600060908     c                   parm                    SizeList
096700060908     c                   parm                    NotUsed
096800060908     c                   parm                    QUSEC
096900060908      *
097000121009      *?Pulizia subfile?
097100060908     c                   seton                                        3031
097200060908     c                   write     LV03C01
097300060908     c                   setoff                                         3133
097400060908      *
097500121009      *?Final step - Write the records back to the subfile.?
097600060908     c                   eval      QLGRT00  = Get
097700060908do  1c                   do        RrnLast       Nrr
097800060908     c                   clear                   QUSEI
097900060908     c                   eval      QUSBPRV  = %size(QUSEC)
098000060908     c                   call      'QLGSRTIO'
098100060908     c                   parm                    QLGSCB00
098200060908     c                   parm                    NotUsed
098300060908     c                   parm                    SflRcd
098400060908     c                   parm                    QLGRL00
098500060908     c                   parm                    NotUsed
098600060908     c                   parm                    QUSEC
098700121009      *?- Ripristino indicatori utilizzati nel sfl rec?
098800060908     c*** non usato:     eval      *in32   =  (S1Copz <> *zeros)
098900120217     c                   eval      *in41   =  (S1H3CL  = *on  and
099000120217     c                                         S1H3CP <> *on  and
099100120217     c                                         S1H3EW  = *off)
099200130219sel 2c                   select
099300130219w   2c                   when      WrkSort = *zero
099400130219     c                   eval      *in42   =  *off
099500130219     c                   eval      *in43   =  *off
099600130219w   2c                   when      WrkSort = 1
099700130219     c                   eval      *in42   =  *on
099800130219     c                   eval      *in43   =  *off
099900130219w   2c                   when      WrkSort = 2
100000130219     c                   eval      *in42   =  *off
100100130219     c                   eval      *in43   =  *on
100200130219w   2c                   when      WrkSort = 3
100300130219     c                   eval      *in42   =  *on
100400130219     c                   eval      *in43   =  *on
100500130219e   2c                   endsl
100600130219      *
100700060908     c                   z-add     Nrr           S01nrr
100800060908     c                   write     LV03S01
100900060908e   1c                   enddo
101000060908      *
101100121009      *?Visualizzazione subfile SOLO SE ci sono dati?
101200060908     c                   eval      *in30   = (S01nrr <= *zeros)
101300121009      *?Eventuale attivazione del SFLEND?
101400060908     c                   eval      *in33   = (%eof(TABEL00F))
101500060908      *
101600060908     c                   ENDSR
101700060907
101800060907      *---------------------------------------------------------------*
101900121009
102000121009      *
102100121009      *  ?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
102200121009      *
102300121009** -?$MSG?-?Messaggi di errore?----------------------------------------------*?
102400060907P.O. Segnacollo obbligatorio                                                   1
102500060907P.O. Segnacollo errato                                                         2
102600060907Serie Segnacollo obbligatoria                                                  3
102700060907Serie Segnacollo errata                                                        4
