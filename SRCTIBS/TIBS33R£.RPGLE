000100040830      *===============================================================*
000200040830      * TIBS33R - Aggiornamento LIBRERIA CORRENTE nei profili utente  *
000300040831      *           di max 15 P.O.                                      *
000400040830      *===============================================================*
000500040831      *? COMPILARE CON PROFILO QSECOFR                               ?*
000600040831      *===============================================================*
000700040830
000800040830     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000900040831     h Usrprf(*owner)
001000040830
001100040830      *===============================================================*
001200040830
001300040830     fAZORG01L  if   e           k disk
001400040830     fAZUTE02L  if   e           k disk
001500040830      *
001600040830     fTIBS33D   cf   e             workstn
001700040830
001800040830      *===============================================================*
001900040830
002000040830      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002100040830      *
002200040830     d C_CrtDft        c                   const('*CRTDFT   ')
002300040831     d $MaxPO          c                   const(15)
002400040830      *
002500040830      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002600040830      *
002700040830      * - Messaggi di errore
002800040901     d $Msg            s             78    dim(04) ctdata  perrcd(1)
002900040831      * - Comandi da eseguire
003000040831     d $Cmd            s             80    dim(02) ctdata  perrcd(1)
003100040831      *
003200040901     d W1Cpo           s              3    dim($MaxPO)              inz
003300040901     d V1Cpo           s                   dim($MaxPO) like(V1Cpo1) inz
003400040901     d V1Dpo           s                   dim($MaxPO) like(V1Dpo1) inz
003500040901     d V1Dlc           s                   dim($MaxPO) like(V1Dlc1) inz
003600040830      *
003700040830      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
003800040830      *
003900040830     d KPJBA         e ds
004000040830      * - Parametri x Controllo profilo utenti
004100040830     d TIBS34DS      e ds                  inz
004200040830      * - Ds di riferimento al file esterno AZUTE00F
004300040830     d AZUTEDS       e ds                  extname(AZUTE00F)
004400040830      * - Ds per dati organigramma
004500040830     d DDatiUte      e ds
004600040830      * - Ds per reperimento tabelle
004700040830     d Tibs02Ds      e ds                  inz
004800040830      * - Tabella con parametro CURLIB x P.O.
004900040830     d dCUR          e ds                  inz
005000040830     d   §CURlib     e                     inz(C_CrtDft)
005100040830      *
005200040830     d Status         sds           333
005300040830     d   SDSpgm          *proc
005400040831      *
005500040831     d DS_V1CPO        ds
005600040831     d  V1Cpo1
005700040831     d  V1Cpo2
005800040831     d  V1Cpo3
005900040831     d  V1Cpo4
006000040831     d  V1Cpo5
006100040831     d  V1Cpo6
006200040831     d  V1Cpo7
006300040831     d  V1Cpo8
006400040831     d  V1Cpo9
006500040831     d  V1CpoA
006600040831     d  V1CpoB
006700040831     d  V1CpoC
006800040831     d  V1CpoD
006900040831     d  V1CpoE
007000040831     d  V1CpoF
007100040831     d DS_V1Dpo        ds
007200040831     d  V1Dpo1
007300040831     d  V1Dpo2
007400040831     d  V1Dpo3
007500040831     d  V1Dpo4
007600040831     d  V1Dpo5
007700040831     d  V1Dpo6
007800040831     d  V1Dpo7
007900040831     d  V1Dpo8
008000040831     d  V1Dpo9
008100040831     d  V1DpoA
008200040831     d  V1DpoB
008300040831     d  V1DpoC
008400040831     d  V1DpoD
008500040831     d  V1DpoE
008600040831     d  V1DpoF
008700040831     d DS_V1Dlc        ds
008800040831     d  V1Dlc1
008900040831     d  V1Dlc2
009000040831     d  V1Dlc3
009100040831     d  V1Dlc4
009200040831     d  V1Dlc5
009300040831     d  V1Dlc6
009400040831     d  V1Dlc7
009500040831     d  V1Dlc8
009600040831     d  V1Dlc9
009700040831     d  V1DlcA
009800040831     d  V1DlcB
009900040831     d  V1DlcC
010000040831     d  V1DlcD
010100040831     d  V1DlcE
010200040831     d  V1DlcF
010300040830      *
010400040830      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
010500040830      *
010600040830      * - Flags
010700040830     d $Fine           s              1    inz(*off)
010800040830     d $InzD01         s              1    inz(*on)
010900040830     d $Video          s              1    inz('1')
011000040831      * - Indici di schiera
011100040831     d xx              s              3  0 inz
011200040901     d yy              s              3  0 inz
011300040831      * - Campi per QCMDEXC
011400040831     d Qcmd            s             80    inz
011500040831     d Qlen            s             15  5 inz(80)
011600040901      * - Campi di comodo
011700040901     d SAVind          s             59    inz(*zeros)
011800040830
011900040830      *===============================================================*
012000040830      *  RIEPILOGO INDICATORI                                         *
012100040830      *---------------------------------------------------------------*
012200040830      * 10    - Comodo                                                *
012300040830      * 28    - Emette il messaggio di errore a video                 *
012400040901      * 41-55 - Errore: P.O. errato o doppio                          *
012500040830      * 90    - Generico di errore                                    *
012600040830      *===============================================================*
012700040830
012800040830     c     *Entry        plist
012900040830     c                   parm                    KPJBA
013000040830      *
013100040830      * Operazioni iniziali
013200040830     c                   exsr      OperazIniz
013300040830      *
013400040830      * Gestione video
013500040830do  1c                   dow       $Fine = *off
013600040830      *
013700040830      * - Filtro di lancio
013800040830cas 2c     $Video        caseq     '1'           GesD01
013900040830      * - Esecuzione
014000040830cas 2c     $Video        caseq     *zeros        ExeCHG
014100040830e   2c                   endcs
014200040830      *
014300040830e   1c                   enddo
014400040830      *
014500040830      * Fine
014600040830     c                   eval      *inLR = *on
014700040830      *
014800040830      *---------------------------------------------------------------*
014900040830      * Operazioni iniziali                                           *
015000040830      *---------------------------------------------------------------*
015100040830     c     OperazIniz    BEGSR
015200040830      *
015300040830      * Reperisco dati job
015400040830     c                   exsr      DatiJob
015500040830      *
015600040830      * Imposto i dati in testata T01
015700040830     c                   eval      V1Tpgm = SDSpgm
015800040830     c                   clear                   V1Tmod
015900040830      *
016000040830     c                   ENDSR
016100040830      *
016200040830      *---------------------------------------------------------------*
016300040830      * Reperimento Dati del job (Utente/Operativi)                   *
016400040830      *---------------------------------------------------------------*
016500040830     c     DatiJob       BEGSR
016600040830      *
016700040830     c     *dtaara       define    §azute        azuteds
016800040830     c     *dtaara       define    §datiute      ddatiute
016900040830      *
017000040830     c                   in(E)     *dtaara
017100040830     c                   IF        %ERROR or RSUT = *blanks
017200040830     c                   clear                   Tibs34Ds
017300040830     c                   call      'TIBS34R'
017400040830     c                   parm                    Tibs34Ds
017500040830     c                   in        *dtaara
017600040830     c                   ENDIF
017700040830      *
017800040830     c                   ENDSR
017900040830      *
018000040830      *---------------------------------------------------------------*
018100040830      * Gestione videata D01                                          *
018200040830      *---------------------------------------------------------------*
018300040830     c     GesD01        BEGSR
018400040830      *
018500040830      * Inizializzo la videata
018600040901if  1c                   if        $InzD01 = *on
018700040830     c                   exsr      InzD01
018800040830     c                   movel     *off          $InzD01
018900040830e   1c                   endif
019000040830      *
019100040830      * Emetto la testata
019200040901if  1c                   if        NOT *in28
019300040830     c                   write     BS33T01
019400040901     c                   else
019500040901      * In caso di errore: riemetto la videata per visualizzarne
019600040901      *   eventuali decodifiche
019700040901     c                   eval      *in28   = *off
019800040901     c                   movea     *in(41)       SAVind
019900040901     c                   movea     *zeros        *in(41)
020000040901     c                   write     BS33T01
020100040901     c                   write     BS33D01
020200040901     c                   movea     SAVind        *in(41)
020300040901     c                   eval      *in28   = *on
020400040830e   1c                   endif
020500040830      * Emetto la videata
020600040830     c                   exfmt     BS33D01
020700040830     c                   setoff                                       28  90
020800040830     c                   clear                   V1Dmsg
020900040830      *
021000040830      * F3=Fine
021100040830if  1c                   if        *inKC
021200040830     c                   exsr      F03D01
021300040830     c                   goto      EndGesD01
021400040830e   1c                   endif
021500040830      *
021600040830      * Controllo dati immessi a video
021700040830     c                   exsr      CtrD01
021800040830     c     *in90         cabeq     *on           EndGesD01
021900040830      *
022000040830      * Se non premuto F6 torno ad emettere la videata
022100040830     c     *inKF         cabeq     *off          EndGesD01
022200040830      *
022300040830      * Imposto esecuzione
022400040901     c                   eval      $Video  = *zeros
022500040901     c*** NO:            eval      $InzD01 = *on
022600040830      *
022700040830     c     EndGesD01     ENDSR
022800040830      *
022900040830      *---------------------------------------------------------------*
023000040830      * Inizializzazione videata D01                                  *
023100040830      *---------------------------------------------------------------*
023200040830     c     InzD01        BEGSR
023300040830      *
023400040830     c                   clear                   BS33D01
023500040830      *
023600040830      * Imposto i dati di default
023700040830     c***                eval      V1Cfil = DUTfel
023800040831     c***                eval      V1Cfil = DUTpou
023900040830      *
024000040830     c                   ENDSR
024100040830      *
024200040830      *---------------------------------------------------------------*
024300040830      * Gestione tasto funzionale F03 da videata D01                  *
024400040830      *---------------------------------------------------------------*
024500040830     c     F03D01        BEGSR
024600040830      *
024700040830      * Chiudo il programma
024800040830     c                   eval      $Fine = *on
024900040830      *
025000040830     c                   ENDSR
025100040830      *
025200040830      *---------------------------------------------------------------*
025300040830      * Controllo dati immessi in videata D01                         *
025400040830      *---------------------------------------------------------------*
025500040830     c     CtrD01        BEGSR
025600040830      *
025700040830     c                   movea     *zeros        *in(41)
025800040901      *
025900040901      * Imposto le schiere utilizzate per facilitare il controllo di
026000040901      *   tutti i campi (15) del formato video D01
026100040901     c                   movea     DS_V1Cpo      W1Cpo
026200040901     c                   movel     W1Cpo         V1Cpo
026300040901     c                   clear                   DS_V1Dpo
026400040901     c                   clear                   DS_V1Dlc
026500040901     c                   clear                   V1Dpo
026600040901     c                   clear                   V1Dlc
026700040901      *
026800040901      * Controllo che sia immesso almeno un P.O.
026900040901if  3c                   if        DS_V1Cpo   = *zeros
027000040901     c                   seton                                        28  90
027100040901     c                   eval      *in41      = *on
027200040901     c                   movel     $Msg(01)      V1Dmsg
027300040901     c                   goto      EndCtrD01
027400040901e   3c                   endif
027500040831      *
027600040901      * Ciclo di controllo P.O.
027700040831do  1c                   DO        $MaxPO        xx
027800040831      *
027900040901if  2c                   IF        V1Cpo(xx) <> *zeros
028000040901      *
028100040901      * - Controllo "duplicazione" codice P.O.
028200040901     c                   eval      *in10 = *off
028300040901     c                   eval      yy    = 1
028400040901     c     V1Cpo(xx)     lookup    V1Cpo(yy)                              10
028500040901if  3c                   if            *in10  = *on
028600040901     c                             and yy     < xx
028700040901     c                   seton                                        28  90
028800040901     c                   eval      *in(40+xx) = *on
028900040901     c                   movel     $Msg(02)      V1Dmsg
029000040901     c                   leave
029100040901e   3c                   endif
029200040830      *
029300040901      * - Controllo singolo P.O. selezionato
029400040831     c     V1Cpo(xx)     chain     AZORG
029500040831if  3c                   if            %found(AZORG01L)
029600040830     c                             and ORGfva = *blanks
029700040831     c                   eval      V1Dpo(xx)  = ORGdes
029800040831x   3c                   else
029900040831     c                   eval      V1Dpo(xx)  = *all'? '
030000040831     c                   seton                                        28  90
030100040831     c                   eval      *in(40+xx) = *on
030200040901     c                   movel     $Msg(03)      V1Dmsg
030300040901     c                   leave
030400040831e   3c                   endif
030500040830      *
030600040901      * - Reperimento libreria corrente per P.O.
030700040830     c                   reset                   dCUR
030800040830     c                   clear                   TIBS02ds
030900040830     c                   movel     'C'           T02mod
031000040830     c                   movel     KNSIF         T02sif
031100040830     c                   movel     'CUR'         T02cod
031200040831     c                   movel(p)  V1Cpo(xx)     T02ke1
031300040830     c                   call      'TIBS02R'
031400040830     c                   parm                    KPJBA
031500040830     c                   parm                    TIBS02ds
031600040831if  3c                   if            T02err = *blanks
031700040830     c                             and %subst(T02uni:1:10) <> *blanks
031800040830     c                   movel     T02uni        dCUR
031900040831e   3c                   endif
032000040831     c                   movel     §CURLIB       V1Dlc(xx)
032100040831      *
032200040901e   2c                   ENDIF
032300040831      *
032400040901e   1c                   ENDDO
032500040901      *
032600040901      * Sposto le schiere nei campi a video per evidenziare le decodif.
032700040901     c     EndCtrD01     tag
032800040901     c*** NON serve:     movel     V1Cpo         W1Cpo
032900040901     c*** NON serve:     movea     W1Cpo         DS_V1Cpo
033000040901     c                   movea     V1Dpo         DS_V1Dpo
033100040901     c                   movea     V1Dlc         DS_V1Dlc
033200040830      *
033300040901     c                   ENDSR
033400040830      *
033500040830      *---------------------------------------------------------------*
033600040831      * Esecuzione del comando CHGUSRPRF per ogni profilo dei P.O.    *
033700040830      *---------------------------------------------------------------*
033800040830     c     ExeCHG        BEGSR
033900040831      *
034000040831      * Ciclo di elaborazione per ogni P.O.
034100040901do  1c                   DO        $MaxPO        xx
034200040831      *
034300040831if  2c                   if        V1Cpo(xx) = *zeros
034400040831     c                   iter
034500040831e   2c                   endif
034600040830      *
034700040830      * Ciclo di lettura file AZUTE
034800040831     c     V1Cpo(xx)     setll     AZUTE000
034900040831     c     V1Cpo(xx)     reade     AZUTE000
035000040830      *
035100040901do  2c                   DOW       NOT %eof(AZUTE02L)
035200040830      *
035300040831      * - Aggiornamento:
035400040831      *   imposto libreria corrente (CURLIB) a seconda dell'ambiente
035500040831     c                   eval      Qcmd = %trim($Cmd(1))
035600040831     c                                  + %trim(UTEute)    + ') '
035700040831     c                                  + %trim($Cmd(2))
035800040831     c                                  + %trim(V1Dlc(xx)) + ')'
035900040831     c                   call      'QCMDEXC'                            90
036000040831     c                   parm                    Qcmd
036100040831     c                   parm                    Qlen
036200040831      * - - Se?rilevato errore: esco dal ciclo per segnalarlo a video
036300040831if  3c                   if        *in90  = *on
036400040831     c                   seton                                        28  42
036500040901     c                   eval      V1Dmsg = %trim($Msg(04)) + ' '
036600040831     c                                    + UTEute
036700040831     c                   leave
036800040831e   3c                   endif
036900040830      *
037000040831     c     V1Cpo(xx)     reade     AZUTE000
037100040830      *
037200040901e   2c                   ENDDO
037300040901      *
037400040901      * - Se?tutto bene: ripulisco il campo V1Cpo per evidenziarne
037500040901      *   l'avvenuta elaborazione (lasciandone la decodifica per
037600040901      *   visualizzare i P.O. elaborati...)
037700040901if  2c                   if        *in90  = *off
037800040901     c                   clear                   V1Cpo(xx)
037900040901x   2c                   else
038000040901     c                   leave
038100040901e   2c                   endif
038200040831      *
038300040901e   1c                   ENDDO
038400040901      *
038500040901      * Sposto la schiere nei campi a video per evidenziare i P.O.
038600040901      *   elaborati
038700040901     c                   movel     V1Cpo         W1Cpo
038800040901     c                   movea     W1Cpo         DS_V1Cpo
038900040901     c*** NON serve:     movea     V1Dpo         DS_V1Dpo
039000040901     c*** NON serve:     movea     V1Dlc         DS_V1Dlc
039100040830      *
039200040830      * Torno alla 1ª videata D01
039300040830     c                   eval      $Fine  = *off
039400040830     c                   reset                   $Video
039500040830      *
039600040901     c                   ENDSR
039700040830
039800040830      *===============================================================*
039900040830
040000040830** $Msg
040100040901Immettere almeno un codice P.O.
040200040901Codice P.O. già immesso
040300040830Codice P.O. errato
040400040830NON riuscito aggiornamento del profilo utente
040500040831** $Cmd
040600040831CHGUSRPRF  usrprf(
040700040831           curlib(
