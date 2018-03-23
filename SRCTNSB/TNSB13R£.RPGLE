000100040720      *===============================================================*
000200040720      * TNSB13R - Rigenerazione immagine LdV per DPD                  *
000300040720      *===============================================================*
000400040720
000500040720     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000600040720
000700040720      *===============================================================*
000800040720
000900040720     fTITAS30C  if   e           k disk
001000040720     fTILAO01L  uf a e           k disk
001100040720      *
001200040720     fTNSB13D   cf   e             workstn
001300040720
001400040720      *===============================================================*
001500040720
001600040720      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001700040720      *
001800040720     d C_txt1          c                   const('*          per  DPD -
001900040720     d                                              *')
002000040720      *
002100040720      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002200040720      *
002300040720      * - Messaggi di errore
002400040720     d $Msg            s             78    dim(07) ctdata  perrcd(1)
002500040720      *
002600040720      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
002700040720      *
002800040720     d KPJBA         e ds
002900040720      * - Parametri x Controllo profilo utenti
003000040720     d TIBS34DS      e ds                  inz
003100040720      * - Ds di riferimento al file esterno AZUTE00F
003200040720     d AZUTEDS       e ds                  extname(AZUTE00F)
003300040720      * - Ds per dati organigramma
003400040720     d DDatiUte      e ds
003500040720      * - Ds per campo TASFLO del file TITAS30C
003600040720     d dTASflo       e ds                  inz
003700040720      *
003800040720     d Status         sds           333
003900040720     d   SDSpgm          *proc
004000040720     d   SDSjob              244    253                                         Job name
004100040720     d***SDSusr              254    263                                         User name
004200040720     d***SDSjnr              264    269S 0                                      Job number
004300040720      *
004400040720     d TNSB14ds        ds                  inz
004500040720     d   D14dcd                       8  0 inz
004600040720     d   D14dca                       8  0 inz
004700040720      *
004800040720     d WLBdat          ds                  inz
004900040720     d   G08dat                       8  0 inz
005000040720     d   G08inv                       8  0 inz
005100040720     d   G08err                       1    inz(*off)
005200040720     d   G08tgi                       5  0 inz
005300040720      *
005400040720     d DS_Time1        ds            14    inz
005500040720     d   dsT1_hms                     6  0 inz
005600040720     d   dsT1_dmy                     8  0 inz
005700040720     d DS_Time2        ds            14    inz
005800040720     d   dsT2_ymd                     8  0 inz
005900040720     d   dsT2_hms                     6  0 inz
006000040720      *
006100040720      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
006200040720      *
006300040720      * - Flags
006400040720     d $Fine           s              1    inz(*off)
006500040720     d $InzD01         s              1    inz(*on)
006600040720     d $InzD02         s              1    inz(*on)
006700040720     d $Parz01         s              1    inz(*off)
006800040720     d $Parz02         s              1    inz(*off)
006900040720     d $Video          s              1    inz('1')
007000040720      * - Campi di comodo
007100040720     d W1Cdcd          s                   like(V1Cdcd) inz
007200040720     d W1Cdca          s                   like(V1Cdca) inz
007300040720     d WdateISO        s               d   datfmt(*ISO) inz
007400040720     d Wtime           s             14  0              inz
007500040720     d WtasDCM         s              8                 inz(*zeros)
007600040720     d WtasRMN         s             15                 inz(*zeros)
007700040720
007800040720      *===============================================================*
007900040720      *  RIEPILOGO INDICATORI                                         *
008000040720      *---------------------------------------------------------------*
008100040720      * 10    - Comodo                                                *
008200040720      * 20    - Visualizza 2ª testata                                 *
008300040720      * 28    - Emette il messaggio di errore a video                 *
008400040720      * 41    - Data consegna DAL errata o range errato               *
008500040720      * 42    - Data consegna AL  errata                              *
008600040720      * 43    - Numero spedizione errato                              *
008700040720      * 90    - Generico di errore                                    *
008800040720      *===============================================================*
008900040720
009000040720     c     *Entry        plist
009100040720     c                   parm                    KPJBA
009200040720      *
009300040720      * Operazioni iniziali
009400040720     c                   exsr      OperazIniz
009500040720      *
009600040720      * Gestione video
009700040720do  1c                   dow       $Fine = *off
009800040720      *
009900040720      * - Filtro di lancio
010000040720cas 2c     $Video        caseq     '1'           GesD01
010100040720      * - Videata di conferma per singola spedizione
010200040720cas 2c     $Video        caseq     '2'           GesD02
010300040720      * - Lancio batch
010400040720cas 2c     $Video        caseq     *zeros        GesBCH
010500040720e   2c                   endcs
010600040720      *
010700040720e   1c                   enddo
010800040720      *
010900040720      * Fine
011000040720     c                   eval      *inLR = *on
011100040720      *
011200040720      *---------------------------------------------------------------*
011300040720      * Operazioni iniziali                                           *
011400040720      *---------------------------------------------------------------*
011500040720     c     OperazIniz    BEGSR
011600040720      *
011700040720      * Definizione Key-List
011800040720      * - TITAS30C
011900040720     c     K04TAS30      klist
012000040720     c                   kfld                    V1Caas
012100040720     c                   kfld                    V1Clnp
012200040720     c                   kfld                    V1Cnrs
012300040720     c                   kfld                    V1Cnsp
012400040720      * - TILAO01L
012500040720     c     K05LAO01      klist
012600040720     c                   kfld                    LAOaas
012700040720     c                   kfld                    LAOlnp
012800040720     c                   kfld                    LAOnrs
012900040720     c                   kfld                    LAOnsp
013000040720     c                   kfld                    LAOcop
013100040720      *
013200040720      * Reperisco dati job
013300040720     c                   exsr      DatiJob
013400040720      *
013500040720      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
013600040720     c                   time                    Wtime
013700040720     c                   movel     Wtime         DS_Time1
013800040720     c                   eval      dsT2_hms   = dsT1_hms
013900040720     c     *eur          movel     dsT1_dmy      WdateISO
014000040720     c                   move      WdateISO      dsT2_ymd
014100040720      *
014200040720      * Imposto i dati in testata T01
014300040720     c                   eval      V1Tpgm = SDSpgm
014400040720     c                   eval      V1Ttxt = C_txt1
014500040720     c                   eval      *in20  = *on
014600040720      *
014700040720     c                   ENDSR
014800040720      *
014900040720      *---------------------------------------------------------------*
015000040720      * Reperimento Dati del job (Utente/Operativi)                   *
015100040720      *---------------------------------------------------------------*
015200040720     c     DatiJob       BEGSR
015300040720      *
015400040720     c     *dtaara       define    §azute        azuteds
015500040720     c     *dtaara       define    §datiute      ddatiute
015600040720      *
015700040720     c                   in(E)     *dtaara
015800040720     c                   IF        %ERROR or RSUT = *blanks
015900040720     c                   clear                   Tibs34Ds
016000040720     c                   call      'TIBS34R'
016100040720     c                   parm                    Tibs34Ds
016200040720     c                   in        *dtaara
016300040720     c                   ENDIF
016400040720      *
016500040720     c                   ENDSR
016600040720      *
016700040720      *---------------------------------------------------------------*
016800040720      * Gestione videata D01                                          *
016900040720      *---------------------------------------------------------------*
017000040720     c     GesD01        BEGSR
017100040720      *
017200040720      * Inizializzo la videata
017300040720if  1c                   if        $InzD01  = *on
017400040720     c                   exsr      InzD01
017500040720     c                   movel     *off          $InzD01
017600040720e   1c                   endif
017700040720      *
017800040720      * Emetto la testata
017900040720if  1c                   if        NOT *in90
018000040720     c                   write     SB13T01
018100040720e   1c                   endif
018200040720      * Emetto la videata
018300040720     c                   exfmt     SB13D01
018400040720     c                   setoff                                       28  90
018500040720     c                   clear                   V1Dmsg
018600040720      *
018700040720      * F3=Fine
018800040720if  1c                   if        *inKC
018900040720     c                   exsr      F03D01
019000040720     c                   goto      EndGesD01
019100040720e   1c                   endif
019200040720      *
019300040720      * Controllo dati immessi a video
019400040720     c                   exsr      CtrD01
019500040720     c     *in90         cabeq     *on           EndGesD01
019600040720      *
019700040720      * Se non premuto F6 torno ad emettere la videata
019800040720     c     *inKF         cabeq     *off          EndGesD01
019900040720      *
020000040720      * Verifico cosa richiesto:
020100040720sel 1c                   select
020200040720      * - parzializzato per data consegna => batch
020300040720w   1c                   when      $Parz01  = *on
020400040720     c                   eval      $Video   = *zeros
020500040720     c                   eval      $InzD01  = *on
020600040720      * - parzializzato per numero spedizione => interattivo
020700040720w   1c                   when      $Parz02  = *on
020800040720     c                   eval      $Video   = '2'
020900040720     c                   eval      $InzD02  = *on
021000040720e   1c                   endsl
021100040720      *
021200040720     c     EndGesD01     ENDSR
021300040720      *
021400040720      *---------------------------------------------------------------*
021500040720      * Inizializzazione videata D01                                  *
021600040720      *---------------------------------------------------------------*
021700040720     c     InzD01        BEGSR
021800040720      *
021900040720      * Imposto i dati di default
022000040720     c                   clear                   SB13D01
022100040720      *
022200040720     c                   ENDSR
022300040720      *
022400040720      *---------------------------------------------------------------*
022500040720      * Gestione tasto funzionale F03 da videata D01                  *
022600040720      *---------------------------------------------------------------*
022700040720     c     F03D01        BEGSR
022800040720      *
022900040720      * Chiudo il programma
023000040720     c                   eval      $Fine = *on
023100040720      *
023200040720     c                   ENDSR
023300040720      *
023400040720      *---------------------------------------------------------------*
023500040720      * Controllo dati immessi in videata D01                         *
023600040720      *---------------------------------------------------------------*
023700040720     c     CtrD01        BEGSR
023800040720      *
023900040720     c                   movea     *zeros        *in(41)
024000040720     c                   reset                   $Parz01
024100040720     c                   reset                   $Parz02
024200040720      *
024300040720      * Controllo parzializzazione effettuata
024400040720if  1c                   if            V1Cdcd <> *zeros
024500040720     c                             or  V1Cdca <> *zeros
024600040720     c                   eval      $Parz01 = *on
024700040720e   1c                   endif
024800040720if  1c                   if            V1Clnp <> *zeros
024900040720     c                             or  V1Cnrs <> *zeros
025000040720     c                             or  V1Cnsp <> *zeros
025100040720     c                             or  V1Caas <> *zeros
025200040720     c                   eval      $Parz02 = *on
025300040720e   1c                   endif
025400040720      *
025500040720sel 1c                   select
025600040720      *
025700040720      * Nessuna parzializzazione richiesta
025800040720w   1c                   when          $Parz01 = *off
025900040720     c                             and $Parz02 = *off
026000040720     c                   seton                                        412890
026100040720     c                   movel     $Msg(01)      V1Dmsg
026200040720     c                   goto      EndCtrD01
026300040720      *
026400040720      * Troppe parzializzazioni richieste
026500040720w   1c                   when          $Parz01 = *on
026600040720     c                             and $Parz02 = *on
026700040720     c                   seton                                        412890
026800040720     c                   movel     $Msg(02)      V1Dmsg
026900040720     c                   goto      EndCtrD01
027000040720      *
027100040720      * Controllo parzializzazione per data
027200040720w   1c                   when      $Parz01 = *on
027300040720      * - controllo data iniziale
027400040720     c                   reset                   WLBdat
027500040720     c                   eval      G08dat  = V1Cdcd
027600040720     c                   call      'XSRDA8'
027700040720     c                   parm                    WLBdat
027800040720if  2c                   if        G08err  = *off
027900040720     c                   eval      V1Cdcd  = G08dat
028000040720     c                   eval      W1Cdcd  = G08inv
028100040720x   2c                   else
028200040720     c                   seton                                        412890
028300040720     c                   movel     $Msg(03)      V1Dmsg
028400040720     c                   goto      EndCtrD01
028500040720e   2c                   endif
028600040720      * - controllo data finale
028700040720if  2c                   if        V1Cdca  = *zeros
028800040720     c                   eval      V1Cdca  = V1Cdcd
028900040720e   2c                   endif
029000040720     c                   reset                   WLBdat
029100040720     c                   eval      G08dat  = V1Cdca
029200040720     c                   call      'XSRDA8'
029300040720     c                   parm                    WLBdat
029400040720if  2c                   if        G08err  = *off
029500040720     c                   eval      V1Cdca  = G08dat
029600040720     c                   eval      W1Cdca  = G08inv
029700040720x   2c                   else
029800040720     c                   seton                                        422890
029900040720     c                   movel     $Msg(03)      V1Dmsg
030000040720     c                   goto      EndCtrD01
030100040720e   2c                   endif
030200040720      * - controllo range di date
030300040720if  2c                   if        W1Cdcd  > W1Cdca
030400040720     c                   seton                                        412890
030500040720     c                   movel     $Msg(04)      V1Dmsg
030600040720     c                   goto      EndCtrD01
030700040720e   2c                   endif
030800040720      *
030900040720      * Controllo selezione per spedizione
031000040720w   1c                   when      $Parz02 = *on
031100040720     c     K04TAS30      chain     TITAS30C
031200040720if  2c                   if        NOT %found(TITAS30C)
031300040720     c                   seton                                        432890
031400040720     c                   movel     $Msg(05)      V1Dmsg
031500040720     c                   goto      EndCtrD01
031600040720e   2c                   endif
031700040720     c                   movel     TASflo        dTASflo
031800040720sel 2c                   select
031900040720w   2c                   when      §FLOiml =  *blanks
032000040720     c                   seton                                        432890
032100040720     c                   movel     $Msg(06)      V1Dmsg
032200040720     c                   goto      EndCtrD01
032300040720w   2c                   when      §FLOiml <> 'I'
032400040720     c                   seton                                        432890
032500040720     c                   movel     $Msg(07)      V1Dmsg
032600040720     c                   goto      EndCtrD01
032700040720e   2c                   endsl
032800040720e   1c                   endsl
032900040720      *
033000040720     c     EndCtrD01     ENDSR
033100040720      *
033200040720      *---------------------------------------------------------------*
033300040720      * Gestione videata D02                                          *
033400040720      *---------------------------------------------------------------*
033500040720     c     GesD02        BEGSR
033600040720      *
033700040720      * Inizializzo la videata
033800040720if  1c                   if        $InzD02  = *on
033900040720     c                   exsr      InzD02
034000040720     c                   movel     *off          $InzD02
034100040720e   1c                   endif
034200040720      *
034300040720      * Emetto la testata
034400040720if  1c                   if        NOT *in90
034500040720     c                   write     SB13T01
034600040720e   1c                   endif
034700040720      * Emetto la videata
034800040720     c                   exfmt     SB13D02
034900040720      *
035000040720sel 1c                   select
035100040720      * F12=Ritorno
035200040720if  1c                   when      *inKL
035300040720     c                   exsr      F12D02
035400040720      * F6=Conferma
035500040720if  1c                   when      *inKF
035600040720     c                   exsr      F06D02
035700040720e   1c                   endsl
035800040720      *
035900040720     c                   ENDSR
036000040720      *
036100040720      *---------------------------------------------------------------*
036200040720      * Inizializzazione videata D02                                  *
036300040720      *---------------------------------------------------------------*
036400040720     c     InzD02        BEGSR
036500040720      *
036600040720     c                   clear                   SB13D02
036700040720      *
036800040720      * Impostazione dei campi numerici in campi alfanumerici di comodo
036900040720     c                   movel     TASdcm        WtasDCM
037000040720     c                   move      TASrmn        WtasRMN
037100040720      *
037200040720      * Reperisco il record da re-impostare
037300040720     c                   clear                   TILAO000
037400040720     c                   move      TASaas        LAOaas
037500040720     c                   move      TASlnp        LAOlnp
037600040720     c                   move      TASnrs        LAOnrs
037700040720     c                   move      TASnsp        LAOnsp
037800040720     c                   movel     'DPD'         LAOcop
037900040720      *
038000040720     c     K05LAO01      chain     TILAO000
038100040720      *
038200040720      * - Numero spedizione (chiave, da videata precedente)
038300040720     c                   eval      V2Clnp  = TASlnp
038400040720     c                   eval      V2Cnrs  = TASnrs
038500040720     c                   eval      V2Cnsp  = TASnsp
038600040720     c                   eval      V2Caas  = TASaas
038700040720      * - Campi comunque aggiornati
038800040720     c                   eval      V2Ntim  = DS_Time2
038900040720     c                   eval      V2Nidl  = SDSjob
039000040720     c                   eval      V2Nela  = *zeros
039100040720      * - Campi reperiti o impostati se NON reperiti
039200040720if  1c                   if        %found(TILAO01L)
039300040720     c                   eval      V2Otim  = LAOtim
039400040720     c                   eval      V2Oidl  = LAGidl
039500040720     c                   eval      V2Oela  = LAGela
039600040720     c                   eval      V2Ccop  = LAOcop
039700040720     c                   eval      V2Cope  = LAOope
039800040720x   1c                   else
039900040720     c                   eval      V2Ccop  = 'DPD'
040000040720     c                   eval      V2Cope  = %subst(wTASrmn:4:11)
040100040720     c                                     + '_'
040200040720     c                                     + wTASdcm
040300040720e   1c                   endif
040400040720      *
040500040720     c                   ENDSR
040600040720      *
040700040720      *---------------------------------------------------------------*
040800040720      * Gestione tasto funzionale F06 da videata D02                  *
040900040720      *---------------------------------------------------------------*
041000040720     c     F06D02        BEGSR
041100040720      *
041200040720      * Confermato aggiornamento file TILAO
041300040720     c                   movel     V2Ntim        LAOtim
041400040720     c                   movel     V2Nidl        LAGidl
041500040720     c                   movel     V2Nela        LAGela
041600040720      *
041700040720if  1c                   if        NOT %found(TILAO01L)
041800040720     c*** *blank:        clear                   LAOxx1
041900040720     c                   movel     V2Ccop        LAOcop
042000040720     c                   movel     V2Cope        LAOope
042100040720     c                   WRITE     TILAO000
042200040720x   1c                   else
042300040720     c                   UPDATE    TILAO000
042400040720e   1c                   endif
042500040720      *
042600040720      * Torno alla 1ª videata D01
042700040720     c                   reset                   $Video
042800040720     c                   reset                   $InzD01
042900040720     c                   reset                   $InzD02
043000040720      *
043100040720     c                   ENDSR
043200040720      *
043300040720      *---------------------------------------------------------------*
043400040720      * Gestione tasto funzionale F12 da videata D02                  *
043500040720      *---------------------------------------------------------------*
043600040720     c     F12D02        BEGSR
043700040720      *
043800040722      * Disalloco il record in esame
043900040722     c                   unlock    TILAO01L                             10
044000040722      *
044100040720      * Torno alla 1ª videata D01
044200040720     c                   reset                   $Video
044300040720      *
044400040720     c                   ENDSR
044500040720      *
044600040720      *---------------------------------------------------------------*
044700040720      * Lancio batch                                                  *
044800040720      *---------------------------------------------------------------*
044900040720     c     GesBCH        BEGSR
045000040720      *
045100040720     c                   reset                   TNSB14ds
045200040720      *
045300040720     c                   eval      D14dcd = W1Cdcd
045400040720     c                   eval      D14dca = W1Cdca
045500040720      *
045600040720     c                   eval      KCOAZ  = 'SB14'
045700040720     c                   movel(p)  TNSB14ds      KPJBU
045800040720     c***                movel(p)  'TNSB14R'     Wpgm             10
045900040720     c                   movel(p)  'BCH10'       Wpgm             10
046000040720     c                   call      Wpgm
046100040720     c                   parm                    KPJBA
046200040720      *
046300040720      * Esco dal pgm
046400040720     c                   eval      $Fine  = *on
046500040720     c                   reset                   $Video
046600040720     c                   reset                   $InzD01
046700040720     c                   reset                   $InzD02
046800040720      *
046900040720     c                   ENDSR
047000040720
047100040720      *===============================================================*
047200040720
047300040720**   $Msg
047400040720Immettere almeno una data consegna oppure un numero spedizione                01
047500040720Parzializzare per data consegna OPPURE per numero spedizione                  02
047600040720Data formalmente errata                                                       03
047700040720Data consegna merce "dal" maggiore di data consegna merce "al"                04
047800040720Numero spedizione errato                                                      05
047900040720Immagine LdV non presente per la spedizione indicata                          06
048000040720Spedizione non DPD                                                            07
