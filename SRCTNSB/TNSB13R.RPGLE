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
001100050318     fAZORG01L  if   e           k disk
001200040720      *
001300040720     fTNSB13D   cf   e             workstn
001400040720
001500040720      *===============================================================*
001600040720
001700040720      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001800040720      *
001900040720     d C_txt1          c                   const('*          per  DPD -
002000040720     d                                              *')
002100040720      *
002200040720      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002300040720      *
002400040720      * - Messaggi di errore
002500050318     d $Msg            s             78    dim(08) ctdata  perrcd(1)
002600040720      *
002700040720      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
002800040720      *
002900040720     d KPJBA         e ds
003000040720      * - Parametri x Controllo profilo utenti
003100040720     d TIBS34DS      e ds                  inz
003200040720      * - Ds di riferimento al file esterno AZUTE00F
003300040720     d AZUTEDS       e ds                  extname(AZUTE00F)
003400040720      * - Ds per dati organigramma
003500040720     d DDatiUte      e ds
003600050318     d OG143         e ds                  inz
003700040720      * - Ds per campo TASFLO del file TITAS30C
003800040720     d dTASflo       e ds                  inz
003900040720      *
004000040720     d Status         sds           333
004100040720     d   SDSpgm          *proc
004200040720     d   SDSjob              244    253                                         Job name
004300040720     d***SDSusr              254    263                                         User name
004400040720     d***SDSjnr              264    269S 0                                      Job number
004500040720      *
004600040720     d TNSB14ds        ds                  inz
004700040720     d   D14dcd                       8  0 inz
004800040720     d   D14dca                       8  0 inz
004900040720      *
005000040720     d WLBdat          ds                  inz
005100040720     d   G08dat                       8  0 inz
005200040720     d   G08inv                       8  0 inz
005300040720     d   G08err                       1    inz(*off)
005400040720     d   G08tgi                       5  0 inz
005500040720      *
005600040720     d DS_Time1        ds            14    inz
005700040720     d   dsT1_hms                     6  0 inz
005800040720     d   dsT1_dmy                     8  0 inz
005900040720     d DS_Time2        ds            14    inz
006000040720     d   dsT2_ymd                     8  0 inz
006100040720     d   dsT2_hms                     6  0 inz
006200040720      *
006300040720      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
006400040720      *
006500040720      * - Flags
006600040720     d $Fine           s              1    inz(*off)
006700040720     d $InzD01         s              1    inz(*on)
006800040720     d $InzD02         s              1    inz(*on)
006900040720     d $Parz01         s              1    inz(*off)
007000040720     d $Parz02         s              1    inz(*off)
007100040720     d $Video          s              1    inz('1')
007200040720      * - Campi di comodo
007300040720     d W1Cdcd          s                   like(V1Cdcd) inz
007400040720     d W1Cdca          s                   like(V1Cdca) inz
007500040720     d WdateISO        s               d   datfmt(*ISO) inz
007600040720     d Wtime           s             14  0              inz
007700040720     d WtasDCM         s              8                 inz(*zeros)
007800040720     d WtasRMN         s             15                 inz(*zeros)
007900050318     d WtasPOm         s              3  0 inz
008000040720
008100040720      *===============================================================*
008200040720      *  RIEPILOGO INDICATORI                                         *
008300040720      *---------------------------------------------------------------*
008400040720      * 10    - Comodo                                                *
008500040720      * 20    - Visualizza 2ª testata                                 *
008600040720      * 28    - Emette il messaggio di errore a video                 *
008700040720      * 41    - Data consegna DAL errata o range errato               *
008800040720      * 42    - Data consegna AL  errata                              *
008900040720      * 43    - Numero spedizione errato                              *
009000040720      * 90    - Generico di errore                                    *
009100040720      *===============================================================*
009200040720
009300040720     c     *Entry        plist
009400040720     c                   parm                    KPJBA
009500040720      *
009600040720      * Operazioni iniziali
009700040720     c                   exsr      OperazIniz
009800040720      *
009900040720      * Gestione video
010000040720do  1c                   dow       $Fine = *off
010100040720      *
010200040720      * - Filtro di lancio
010300040720cas 2c     $Video        caseq     '1'           GesD01
010400040720      * - Videata di conferma per singola spedizione
010500040720cas 2c     $Video        caseq     '2'           GesD02
010600040720      * - Lancio batch
010700040720cas 2c     $Video        caseq     *zeros        GesBCH
010800040720e   2c                   endcs
010900040720      *
011000040720e   1c                   enddo
011100040720      *
011200040720      * Fine
011300040720     c                   eval      *inLR = *on
011400040720      *
011500040720      *---------------------------------------------------------------*
011600040720      * Operazioni iniziali                                           *
011700040720      *---------------------------------------------------------------*
011800040720     c     OperazIniz    BEGSR
011900040720      *
012000040720      * Definizione Key-List
012100040720      * - TITAS30C
012200040720     c     K04TAS30      klist
012300040720     c                   kfld                    V1Caas
012400040720     c                   kfld                    V1Clnp
012500040720     c                   kfld                    V1Cnrs
012600040720     c                   kfld                    V1Cnsp
012700040720      * - TILAO01L
012800040720     c     K05LAO01      klist
012900040720     c                   kfld                    LAOaas
013000040720     c                   kfld                    LAOlnp
013100040720     c                   kfld                    LAOnrs
013200040720     c                   kfld                    LAOnsp
013300040720     c                   kfld                    LAOcop
013400040720      *
013500040720      * Reperisco dati job
013600040720     c                   exsr      DatiJob
013700040720      *
013800040720      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
013900040720     c                   time                    Wtime
014000040720     c                   movel     Wtime         DS_Time1
014100040720     c                   eval      dsT2_hms   = dsT1_hms
014200040720     c     *eur          movel     dsT1_dmy      WdateISO
014300040720     c                   move      WdateISO      dsT2_ymd
014400040720      *
014500040720      * Imposto i dati in testata T01
014600040720     c                   eval      V1Tpgm = SDSpgm
014700040720     c                   eval      V1Ttxt = C_txt1
014800040720     c                   eval      *in20  = *on
014900040720      *
015000040720     c                   ENDSR
015100040720      *
015200040720      *---------------------------------------------------------------*
015300040720      * Reperimento Dati del job (Utente/Operativi)                   *
015400040720      *---------------------------------------------------------------*
015500040720     c     DatiJob       BEGSR
015600040720      *
015700040720     c     *dtaara       define    §azute        azuteds
015800040720     c     *dtaara       define    §datiute      ddatiute
015900040720      *
016000040720     c                   in(E)     *dtaara
016100040720     c                   IF        %ERROR or RSUT = *blanks
016200040720     c                   clear                   Tibs34Ds
016300040720     c                   call      'TIBS34R'
016400040720     c                   parm                    Tibs34Ds
016500040720     c                   in        *dtaara
016600040720     c                   ENDIF
016700040720      *
016800040720     c                   ENDSR
016900040720      *
017000040720      *---------------------------------------------------------------*
017100040720      * Gestione videata D01                                          *
017200040720      *---------------------------------------------------------------*
017300040720     c     GesD01        BEGSR
017400040720      *
017500040720      * Inizializzo la videata
017600040720if  1c                   if        $InzD01  = *on
017700040720     c                   exsr      InzD01
017800040720     c                   movel     *off          $InzD01
017900040720e   1c                   endif
018000040720      *
018100040720      * Emetto la testata
018200040720if  1c                   if        NOT *in90
018300040720     c                   write     SB13T01
018400040720e   1c                   endif
018500040720      * Emetto la videata
018600040720     c                   exfmt     SB13D01
018700040720     c                   setoff                                       28  90
018800040720     c                   clear                   V1Dmsg
018900040720      *
019000040720      * F3=Fine
019100040720if  1c                   if        *inKC
019200040720     c                   exsr      F03D01
019300040720     c                   goto      EndGesD01
019400040720e   1c                   endif
019500040720      *
019600040720      * Controllo dati immessi a video
019700040720     c                   exsr      CtrD01
019800040720     c     *in90         cabeq     *on           EndGesD01
019900040720      *
020000040720      * Se non premuto F6 torno ad emettere la videata
020100040720     c     *inKF         cabeq     *off          EndGesD01
020200040720      *
020300040720      * Verifico cosa richiesto:
020400040720sel 1c                   select
020500040720      * - parzializzato per data consegna => batch
020600040720w   1c                   when      $Parz01  = *on
020700040720     c                   eval      $Video   = *zeros
020800040720     c                   eval      $InzD01  = *on
020900040720      * - parzializzato per numero spedizione => interattivo
021000040720w   1c                   when      $Parz02  = *on
021100040720     c                   eval      $Video   = '2'
021200040720     c                   eval      $InzD02  = *on
021300040720e   1c                   endsl
021400040720      *
021500040720     c     EndGesD01     ENDSR
021600040720      *
021700040720      *---------------------------------------------------------------*
021800040720      * Inizializzazione videata D01                                  *
021900040720      *---------------------------------------------------------------*
022000040720     c     InzD01        BEGSR
022100040720      *
022200040720      * Imposto i dati di default
022300040720     c                   clear                   SB13D01
022400040720      *
022500040720     c                   ENDSR
022600040720      *
022700040720      *---------------------------------------------------------------*
022800040720      * Gestione tasto funzionale F03 da videata D01                  *
022900040720      *---------------------------------------------------------------*
023000040720     c     F03D01        BEGSR
023100040720      *
023200040720      * Chiudo il programma
023300040720     c                   eval      $Fine = *on
023400040720      *
023500040720     c                   ENDSR
023600040720      *
023700040720      *---------------------------------------------------------------*
023800040720      * Controllo dati immessi in videata D01                         *
023900040720      *---------------------------------------------------------------*
024000040720     c     CtrD01        BEGSR
024100040720      *
024200040720     c                   movea     *zeros        *in(41)
024300040720     c                   reset                   $Parz01
024400040720     c                   reset                   $Parz02
024500040720      *
024600040720      * Controllo parzializzazione effettuata
024700040720if  1c                   if            V1Cdcd <> *zeros
024800040720     c                             or  V1Cdca <> *zeros
024900040720     c                   eval      $Parz01 = *on
025000040720e   1c                   endif
025100040720if  1c                   if            V1Clnp <> *zeros
025200040720     c                             or  V1Cnrs <> *zeros
025300040720     c                             or  V1Cnsp <> *zeros
025400040720     c                             or  V1Caas <> *zeros
025500040720     c                   eval      $Parz02 = *on
025600040720e   1c                   endif
025700040720      *
025800040720sel 1c                   select
025900040720      *
026000040720      * Nessuna parzializzazione richiesta
026100040720w   1c                   when          $Parz01 = *off
026200040720     c                             and $Parz02 = *off
026300040720     c                   seton                                        412890
026400040720     c                   movel     $Msg(01)      V1Dmsg
026500040720     c                   goto      EndCtrD01
026600040720      *
026700040720      * Troppe parzializzazioni richieste
026800040720w   1c                   when          $Parz01 = *on
026900040720     c                             and $Parz02 = *on
027000040720     c                   seton                                        412890
027100040720     c                   movel     $Msg(02)      V1Dmsg
027200040720     c                   goto      EndCtrD01
027300040720      *
027400040720      * Controllo parzializzazione per data
027500040720w   1c                   when      $Parz01 = *on
027600040720      * - controllo data iniziale
027700040720     c                   reset                   WLBdat
027800040720     c                   eval      G08dat  = V1Cdcd
027900040720     c                   call      'XSRDA8'
028000040720     c                   parm                    WLBdat
028100040720if  2c                   if        G08err  = *off
028200040720     c                   eval      V1Cdcd  = G08dat
028300040720     c                   eval      W1Cdcd  = G08inv
028400040720x   2c                   else
028500040720     c                   seton                                        412890
028600040720     c                   movel     $Msg(03)      V1Dmsg
028700040720     c                   goto      EndCtrD01
028800040720e   2c                   endif
028900040720      * - controllo data finale
029000040720if  2c                   if        V1Cdca  = *zeros
029100040720     c                   eval      V1Cdca  = V1Cdcd
029200040720e   2c                   endif
029300040720     c                   reset                   WLBdat
029400040720     c                   eval      G08dat  = V1Cdca
029500040720     c                   call      'XSRDA8'
029600040720     c                   parm                    WLBdat
029700040720if  2c                   if        G08err  = *off
029800040720     c                   eval      V1Cdca  = G08dat
029900040720     c                   eval      W1Cdca  = G08inv
030000040720x   2c                   else
030100040720     c                   seton                                        422890
030200040720     c                   movel     $Msg(03)      V1Dmsg
030300040720     c                   goto      EndCtrD01
030400040720e   2c                   endif
030500040720      * - controllo range di date
030600040720if  2c                   if        W1Cdcd  > W1Cdca
030700040720     c                   seton                                        412890
030800040720     c                   movel     $Msg(04)      V1Dmsg
030900040720     c                   goto      EndCtrD01
031000040720e   2c                   endif
031100040720      *
031200040720      * Controllo selezione per spedizione
031300040720w   1c                   when      $Parz02 = *on
031400040720     c     K04TAS30      chain     TITAS30C
031500040720if  2c                   if        NOT %found(TITAS30C)
031600040720     c                   seton                                        432890
031700040720     c                   movel     $Msg(05)      V1Dmsg
031800040720     c                   goto      EndCtrD01
031900040720e   2c                   endif
032000050318     c* verifico che sia mittente dpd
032100050318     c                   clear                   og143
032200050318     c                   movel     tasccm        wtaspom
032300050318     c     wtaspom       chain     azorg01l
032400050318     c                   if        %found(azorg01l)
032500050318     c                   movel     orgde3        og143
032600050318     c                   endif
032700050318     c                   if        §ogntw <> 'DPD'
032800050318     c                   seton                                        432890
032900050318     c                   movel     $Msg(07)      V1Dmsg
033000050318     c                   goto      EndCtrD01
033100050318     c                   endif
033200040720     c                   movel     TASflo        dTASflo
033300040720sel 2c                   select
033400040720w   2c                   when      §FLOiml =  *blanks
033500040720     c                   seton                                        432890
033600040720     c                   movel     $Msg(06)      V1Dmsg
033700040720     c                   goto      EndCtrD01
033800040720w   2c                   when      §FLOiml <> 'I'
033900040720     c                   seton                                        432890
034000050318     c                   movel     $Msg(08)      V1Dmsg
034100040720     c                   goto      EndCtrD01
034200040720e   2c                   endsl
034300040720e   1c                   endsl
034400040720      *
034500040720     c     EndCtrD01     ENDSR
034600040720      *
034700040720      *---------------------------------------------------------------*
034800040720      * Gestione videata D02                                          *
034900040720      *---------------------------------------------------------------*
035000040720     c     GesD02        BEGSR
035100040720      *
035200040720      * Inizializzo la videata
035300040720if  1c                   if        $InzD02  = *on
035400040720     c                   exsr      InzD02
035500040720     c                   movel     *off          $InzD02
035600040720e   1c                   endif
035700040720      *
035800040720      * Emetto la testata
035900040720if  1c                   if        NOT *in90
036000040720     c                   write     SB13T01
036100040720e   1c                   endif
036200040720      * Emetto la videata
036300040720     c                   exfmt     SB13D02
036400040720      *
036500040720sel 1c                   select
036600040720      * F12=Ritorno
036700040720if  1c                   when      *inKL
036800040720     c                   exsr      F12D02
036900040720      * F6=Conferma
037000040720if  1c                   when      *inKF
037100040720     c                   exsr      F06D02
037200040720e   1c                   endsl
037300040720      *
037400040720     c                   ENDSR
037500040720      *
037600040720      *---------------------------------------------------------------*
037700040720      * Inizializzazione videata D02                                  *
037800040720      *---------------------------------------------------------------*
037900040720     c     InzD02        BEGSR
038000040720      *
038100040720     c                   clear                   SB13D02
038200040720      *
038300040720      * Impostazione dei campi numerici in campi alfanumerici di comodo
038400040720     c                   movel     TASdcm        WtasDCM
038500040720     c                   move      TASrmn        WtasRMN
038600040720      *
038700040720      * Reperisco il record da re-impostare
038800040720     c                   clear                   TILAO000
038900040720     c                   move      TASaas        LAOaas
039000040720     c                   move      TASlnp        LAOlnp
039100040720     c                   move      TASnrs        LAOnrs
039200040720     c                   move      TASnsp        LAOnsp
039300040720     c                   movel     'DPD'         LAOcop
039400040720      *
039500040720     c     K05LAO01      chain     TILAO000
039600040720      *
039700040720      * - Numero spedizione (chiave, da videata precedente)
039800040720     c                   eval      V2Clnp  = TASlnp
039900040720     c                   eval      V2Cnrs  = TASnrs
040000040720     c                   eval      V2Cnsp  = TASnsp
040100040720     c                   eval      V2Caas  = TASaas
040200040720      * - Campi comunque aggiornati
040300040720     c                   eval      V2Ntim  = DS_Time2
040400040720     c                   eval      V2Nidl  = SDSjob
040500040720     c                   eval      V2Nela  = *zeros
040600040720      * - Campi reperiti o impostati se NON reperiti
040700040720if  1c                   if        %found(TILAO01L)
040800040720     c                   eval      V2Otim  = LAOtim
040900040720     c                   eval      V2Oidl  = LAGidl
041000040720     c                   eval      V2Oela  = LAGela
041100040720     c                   eval      V2Ccop  = LAOcop
041200040720     c                   eval      V2Cope  = LAOope
041300040720x   1c                   else
041400040720     c                   eval      V2Ccop  = 'DPD'
041500040720     c                   eval      V2Cope  = %subst(wTASrmn:4:11)
041600040720     c                                     + '_'
041700040720     c                                     + wTASdcm
041800040720e   1c                   endif
041900040720      *
042000040720     c                   ENDSR
042100040720      *
042200040720      *---------------------------------------------------------------*
042300040720      * Gestione tasto funzionale F06 da videata D02                  *
042400040720      *---------------------------------------------------------------*
042500040720     c     F06D02        BEGSR
042600040720      *
042700040720      * Confermato aggiornamento file TILAO
042800040720     c                   movel     V2Ntim        LAOtim
042900040720     c                   movel     V2Nidl        LAGidl
043000040720     c                   movel     V2Nela        LAGela
043100040720      *
043200040720if  1c                   if        NOT %found(TILAO01L)
043300040720     c*** *blank:        clear                   LAOxx1
043400040720     c                   movel     V2Ccop        LAOcop
043500040720     c                   movel     V2Cope        LAOope
043600040720     c                   WRITE     TILAO000
043700040720x   1c                   else
043800040720     c                   UPDATE    TILAO000
043900040720e   1c                   endif
044000040720      *
044100040720      * Torno alla 1ª videata D01
044200040720     c                   reset                   $Video
044300040720     c                   reset                   $InzD01
044400040720     c                   reset                   $InzD02
044500040720      *
044600040720     c                   ENDSR
044700040720      *
044800040720      *---------------------------------------------------------------*
044900040720      * Gestione tasto funzionale F12 da videata D02                  *
045000040720      *---------------------------------------------------------------*
045100040720     c     F12D02        BEGSR
045200040720      *
045300040722      * Disalloco il record in esame
045400040722     c                   unlock    TILAO01L                             10
045500040722      *
045600040720      * Torno alla 1ª videata D01
045700040720     c                   reset                   $Video
045800040720      *
045900040720     c                   ENDSR
046000040720      *
046100040720      *---------------------------------------------------------------*
046200040720      * Lancio batch                                                  *
046300040720      *---------------------------------------------------------------*
046400040720     c     GesBCH        BEGSR
046500040720      *
046600040720     c                   reset                   TNSB14ds
046700040720      *
046800040720     c                   eval      D14dcd = W1Cdcd
046900040720     c                   eval      D14dca = W1Cdca
047000040720      *
047100040720     c                   eval      KCOAZ  = 'SB14'
047200040720     c                   movel(p)  TNSB14ds      KPJBU
047300040720     c***                movel(p)  'TNSB14R'     Wpgm             10
047400040720     c                   movel(p)  'BCH10'       Wpgm             10
047500040720     c                   call      Wpgm
047600040720     c                   parm                    KPJBA
047700040720      *
047800040720      * Esco dal pgm
047900040720     c                   eval      $Fine  = *on
048000040720     c                   reset                   $Video
048100040720     c                   reset                   $InzD01
048200040720     c                   reset                   $InzD02
048300040720      *
048400040720     c                   ENDSR
048500040720
048600040720      *===============================================================*
048700040720
048800040720**   $Msg
048900040720Immettere almeno una data consegna oppure un numero spedizione                01
049000040720Parzializzare per data consegna OPPURE per numero spedizione                  02
049100040720Data formalmente errata                                                       03
049200040720Data consegna merce "dal" maggiore di data consegna merce "al"                04
049300040720Numero spedizione errato                                                      05
049400040720Immagine LdV non presente per la spedizione indicata                          06
049500040720Spedizione non DPD                                                            07
049600050318Immagine LdV mai inviata                                                      08
