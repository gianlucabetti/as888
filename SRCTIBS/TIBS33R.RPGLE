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
001500070904     ***fKPPRF01L  if   e           k disk
001600040830      *
001700040830     fTIBS33D   cf   e             workstn
001800040830
001900040830      *===============================================================*
002000040830
002100040830      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002200040830      *
002300040830     d C_CrtDft        c                   const('*CRTDFT   ')
002400040831     d $MaxPO          c                   const(15)
002500040830      *
002600040830      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002700040830      *
002800040830      * - Messaggi di errore
002900040901     d $Msg            s             78    dim(04) ctdata  perrcd(1)
003000040831      * - Comandi da eseguire
003100040831     d $Cmd            s             80    dim(02) ctdata  perrcd(1)
003200040831      *
003300040901     d W1Cpo           s              3    dim($MaxPO)              inz
003400040901     d V1Cpo           s                   dim($MaxPO) like(V1Cpo1) inz
003500040901     d V1Dpo           s                   dim($MaxPO) like(V1Dpo1) inz
003600040901     d V1Dlc           s                   dim($MaxPO) like(V1Dlc1) inz
003700040830      *
003800040830      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
003900040830      *
004000040830     d KPJBA         e ds
004100040830      * - Parametri x Controllo profilo utenti
004200040830     d TIBS34DS      e ds                  inz
004300040830      * - Ds di riferimento al file esterno AZUTE00F
004400040830     d AZUTEDS       e ds                  extname(AZUTE00F)
004500040830      * - Ds per dati organigramma
004600040830     d DDatiUte      e ds
004700040830      * - Ds per reperimento tabelle
004800040830     d Tibs02Ds      e ds                  inz
004900040830      * - Tabella con parametro CURLIB x P.O.
005000040830     d dCUR          e ds                  inz
005100040830     d   §CURlib     e                     inz(C_CrtDft)
005200040830      *
005300040830     d Status         sds           333
005400040830     d   SDSpgm          *proc
005500040831      *
005600040831     d DS_V1CPO        ds
005700040831     d  V1Cpo1
005800040831     d  V1Cpo2
005900040831     d  V1Cpo3
006000040831     d  V1Cpo4
006100040831     d  V1Cpo5
006200040831     d  V1Cpo6
006300040831     d  V1Cpo7
006400040831     d  V1Cpo8
006500040831     d  V1Cpo9
006600040831     d  V1CpoA
006700040831     d  V1CpoB
006800040831     d  V1CpoC
006900040831     d  V1CpoD
007000040831     d  V1CpoE
007100040831     d  V1CpoF
007200040831     d DS_V1Dpo        ds
007300040831     d  V1Dpo1
007400040831     d  V1Dpo2
007500040831     d  V1Dpo3
007600040831     d  V1Dpo4
007700040831     d  V1Dpo5
007800040831     d  V1Dpo6
007900040831     d  V1Dpo7
008000040831     d  V1Dpo8
008100040831     d  V1Dpo9
008200040831     d  V1DpoA
008300040831     d  V1DpoB
008400040831     d  V1DpoC
008500040831     d  V1DpoD
008600040831     d  V1DpoE
008700040831     d  V1DpoF
008800040831     d DS_V1Dlc        ds
008900040831     d  V1Dlc1
009000040831     d  V1Dlc2
009100040831     d  V1Dlc3
009200040831     d  V1Dlc4
009300040831     d  V1Dlc5
009400040831     d  V1Dlc6
009500040831     d  V1Dlc7
009600040831     d  V1Dlc8
009700040831     d  V1Dlc9
009800040831     d  V1DlcA
009900040831     d  V1DlcB
010000040831     d  V1DlcC
010100040831     d  V1DlcD
010200040831     d  V1DlcE
010300040831     d  V1DlcF
010400040830      *
010500040830      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
010600040830      *
010700040830      * - Flags
010800040830     d $Fine           s              1    inz(*off)
010900040830     d $InzD01         s              1    inz(*on)
011000040830     d $Video          s              1    inz('1')
011100040831      * - Indici di schiera
011200040831     d xx              s              3  0 inz
011300040901     d yy              s              3  0 inz
011400040831      * - Campi per QCMDEXC
011500040831     d Qcmd            s             80    inz
011600040831     d Qlen            s             15  5 inz(80)
011700040901      * - Campi di comodo
011800040901     d SAVind          s             59    inz(*zeros)
011900040830
012000040830      *===============================================================*
012100040830      *  RIEPILOGO INDICATORI                                         *
012200040830      *---------------------------------------------------------------*
012300040830      * 10    - Comodo                                                *
012400040830      * 28    - Emette il messaggio di errore a video                 *
012500040901      * 41-55 - Errore: P.O. errato o doppio                          *
012600040830      * 90    - Generico di errore                                    *
012700040830      *===============================================================*
012800040830
012900040830     c     *Entry        plist
013000040830     c                   parm                    KPJBA
013100040830      *
013200040830      * Operazioni iniziali
013300040830     c                   exsr      OperazIniz
013400040830      *
013500040830      * Gestione video
013600040830do  1c                   dow       $Fine = *off
013700040830      *
013800040830      * - Filtro di lancio
013900040830cas 2c     $Video        caseq     '1'           GesD01
014000040830      * - Esecuzione
014100040830cas 2c     $Video        caseq     *zeros        ExeCHG
014200040830e   2c                   endcs
014300040830      *
014400040830e   1c                   enddo
014500040830      *
014600040830      * Fine
014700040830     c                   eval      *inLR = *on
014800040830      *
014900040830      *---------------------------------------------------------------*
015000040830      * Operazioni iniziali                                           *
015100040830      *---------------------------------------------------------------*
015200040830     c     OperazIniz    BEGSR
015300040830      *
015400040830      * Reperisco dati job
015500040830     c                   exsr      DatiJob
015600040830      *
015700040830      * Imposto i dati in testata T01
015800040830     c                   eval      V1Tpgm = SDSpgm
015900040830     c                   clear                   V1Tmod
016000040830      *
016100040830     c                   ENDSR
016200040830      *
016300040830      *---------------------------------------------------------------*
016400040830      * Reperimento Dati del job (Utente/Operativi)                   *
016500040830      *---------------------------------------------------------------*
016600040830     c     DatiJob       BEGSR
016700040830      *
016800040830     c     *dtaara       define    §azute        azuteds
016900040830     c     *dtaara       define    §datiute      ddatiute
017000040830      *
017100040830     c                   in(E)     *dtaara
017200040830     c                   IF        %ERROR or RSUT = *blanks
017300040830     c                   clear                   Tibs34Ds
017400040830     c                   call      'TIBS34R'
017500040830     c                   parm                    Tibs34Ds
017600040830     c                   in        *dtaara
017700040830     c                   ENDIF
017800040830      *
017900040830     c                   ENDSR
018000040830      *
018100040830      *---------------------------------------------------------------*
018200040830      * Gestione videata D01                                          *
018300040830      *---------------------------------------------------------------*
018400040830     c     GesD01        BEGSR
018500040830      *
018600040830      * Inizializzo la videata
018700040901if  1c                   if        $InzD01 = *on
018800040830     c                   exsr      InzD01
018900040830     c                   movel     *off          $InzD01
019000040830e   1c                   endif
019100040830      *
019200040830      * Emetto la testata
019300040901if  1c                   if        NOT *in28
019400040830     c                   write     BS33T01
019500040901     c                   else
019600040901      * In caso di errore: riemetto la videata per visualizzarne
019700040901      *   eventuali decodifiche
019800040901     c                   eval      *in28   = *off
019900040901     c                   movea     *in(41)       SAVind
020000040901     c                   movea     *zeros        *in(41)
020100040901     c                   write     BS33T01
020200040901     c                   write     BS33D01
020300040901     c                   movea     SAVind        *in(41)
020400040901     c                   eval      *in28   = *on
020500040830e   1c                   endif
020600040830      * Emetto la videata
020700040830     c                   exfmt     BS33D01
020800040830     c                   setoff                                       28  90
020900040830     c                   clear                   V1Dmsg
021000040830      *
021100040830      * F3=Fine
021200040830if  1c                   if        *inKC
021300040830     c                   exsr      F03D01
021400040830     c                   goto      EndGesD01
021500040830e   1c                   endif
021600040830      *
021700040830      * Controllo dati immessi a video
021800040830     c                   exsr      CtrD01
021900040830     c     *in90         cabeq     *on           EndGesD01
022000040830      *
022100040830      * Se non premuto F6 torno ad emettere la videata
022200040830     c     *inKF         cabeq     *off          EndGesD01
022300040830      *
022400040830      * Imposto esecuzione
022500040901     c                   eval      $Video  = *zeros
022600040901     c*** NO:            eval      $InzD01 = *on
022700040830      *
022800040830     c     EndGesD01     ENDSR
022900040830      *
023000040830      *---------------------------------------------------------------*
023100040830      * Inizializzazione videata D01                                  *
023200040830      *---------------------------------------------------------------*
023300040830     c     InzD01        BEGSR
023400040830      *
023500040830     c                   clear                   BS33D01
023600040830      *
023700040830      * Imposto i dati di default
023800040830     c***                eval      V1Cfil = DUTfel
023900040831     c***                eval      V1Cfil = DUTpou
024000040830      *
024100040830     c                   ENDSR
024200040830      *
024300040830      *---------------------------------------------------------------*
024400040830      * Gestione tasto funzionale F03 da videata D01                  *
024500040830      *---------------------------------------------------------------*
024600040830     c     F03D01        BEGSR
024700040830      *
024800040830      * Chiudo il programma
024900040830     c                   eval      $Fine = *on
025000040830      *
025100040830     c                   ENDSR
025200040830      *
025300040830      *---------------------------------------------------------------*
025400040830      * Controllo dati immessi in videata D01                         *
025500040830      *---------------------------------------------------------------*
025600040830     c     CtrD01        BEGSR
025700040830      *
025800040830     c                   movea     *zeros        *in(41)
025900040901      *
026000040901      * Imposto le schiere utilizzate per facilitare il controllo di
026100040901      *   tutti i campi (15) del formato video D01
026200040901     c                   movea     DS_V1Cpo      W1Cpo
026300040901     c                   movel     W1Cpo         V1Cpo
026400040901     c                   clear                   DS_V1Dpo
026500040901     c                   clear                   DS_V1Dlc
026600040901     c                   clear                   V1Dpo
026700040901     c                   clear                   V1Dlc
026800040901      *
026900040901      * Controllo che sia immesso almeno un P.O.
027000040901if  3c                   if        DS_V1Cpo   = *zeros
027100040901     c                   seton                                        28  90
027200040901     c                   eval      *in41      = *on
027300040901     c                   movel     $Msg(01)      V1Dmsg
027400040901     c                   goto      EndCtrD01
027500040901e   3c                   endif
027600040831      *
027700040901      * Ciclo di controllo P.O.
027800040831do  1c                   DO        $MaxPO        xx
027900040831      *
028000040901if  2c                   IF        V1Cpo(xx) <> *zeros
028100040901      *
028200040901      * - Controllo "duplicazione" codice P.O.
028300040901     c                   eval      *in10 = *off
028400040901     c                   eval      yy    = 1
028500040901     c     V1Cpo(xx)     lookup    V1Cpo(yy)                              10
028600040901if  3c                   if            *in10  = *on
028700040901     c                             and yy     < xx
028800040901     c                   seton                                        28  90
028900040901     c                   eval      *in(40+xx) = *on
029000040901     c                   movel     $Msg(02)      V1Dmsg
029100040901     c                   leave
029200040901e   3c                   endif
029300040830      *
029400040901      * - Controllo singolo P.O. selezionato
029500040831     c     V1Cpo(xx)     chain     AZORG
029600040831if  3c                   if            %found(AZORG01L)
029700040830     c                             and ORGfva = *blanks
029800040831     c                   eval      V1Dpo(xx)  = ORGdes
029900040831x   3c                   else
030000040831     c                   eval      V1Dpo(xx)  = *all'? '
030100040831     c                   seton                                        28  90
030200040831     c                   eval      *in(40+xx) = *on
030300040901     c                   movel     $Msg(03)      V1Dmsg
030400040901     c                   leave
030500040831e   3c                   endif
030600040830      *
030700040901      * - Reperimento libreria corrente per P.O.
030800040830     c                   reset                   dCUR
030900040830     c                   clear                   TIBS02ds
031000040830     c                   movel     'C'           T02mod
031100040830     c                   movel     KNSIF         T02sif
031200040830     c                   movel     'CUR'         T02cod
031300040831     c                   movel(p)  V1Cpo(xx)     T02ke1
031400040830     c                   call      'TIBS02R'
031500040830     c                   parm                    KPJBA
031600040830     c                   parm                    TIBS02ds
031700040831if  3c                   if            T02err = *blanks
031800040830     c                             and %subst(T02uni:1:10) <> *blanks
031900040830     c                   movel     T02uni        dCUR
032000040831e   3c                   endif
032100040831     c                   movel     §CURLIB       V1Dlc(xx)
032200040831      *
032300040901e   2c                   ENDIF
032400040831      *
032500040901e   1c                   ENDDO
032600040901      *
032700040901      * Sposto le schiere nei campi a video per evidenziare le decodif.
032800040901     c     EndCtrD01     tag
032900040901     c*** NON serve:     movel     V1Cpo         W1Cpo
033000040901     c*** NON serve:     movea     W1Cpo         DS_V1Cpo
033100040901     c                   movea     V1Dpo         DS_V1Dpo
033200040901     c                   movea     V1Dlc         DS_V1Dlc
033300040830      *
033400040901     c                   ENDSR
033500040830      *
033600040830      *---------------------------------------------------------------*
033700040831      * Esecuzione del comando CHGUSRPRF per ogni profilo dei P.O.    *
033800040830      *---------------------------------------------------------------*
033900040830     c     ExeCHG        BEGSR
034000040831      *
034100040831      * Ciclo di elaborazione per ogni P.O.
034200040901do  1c                   DO        $MaxPO        xx
034300040831      *
034400040831if  2c                   if        V1Cpo(xx) = *zeros
034500040831     c                   iter
034600040831e   2c                   endif
034700040830      *
034800040830      * Ciclo di lettura file AZUTE
034900040831     c     V1Cpo(xx)     setll     AZUTE000
035000040831     c     V1Cpo(xx)     reade     AZUTE000
035100040830      *
035200040901do  2c                   DOW       NOT %eof(AZUTE02L)
035300040830      *
035400040831      * - Aggiornamento:
035500040831      *   imposto libreria corrente (CURLIB) a seconda dell'ambiente
035600040831     c                   eval      Qcmd = %trim($Cmd(1))
035700040831     c                                  + %trim(UTEute)    + ') '
035800040831     c                                  + %trim($Cmd(2))
035900040831     c                                  + %trim(V1Dlc(xx)) + ')'
036000040831     c                   call      'QCMDEXC'                            90
036100040831     c                   parm                    Qcmd
036200040831     c                   parm                    Qlen
036300070904      * - - Se?rilevato errore: verifico esistenza profilo utente &
036400070904      *     solo se esistente esco dal ciclo per segnalarlo a video
036500070904if  3c                   if        *in90  = *on
036600070904     ***c     UTEute        chain     KPPRF000
036700070904if  4***c                   if        NOT %found(KPPRF01L)
036800070904     c                   call      'TIBS32C'
036900070904     c                   parm      'CHK'         w32Ctela          3
037000070904     c                   parm      *blanks       w32Cmoba          1
037100070904     c                   parm      UTEute        w32Cuser         10
037200070904     c                   parm      *blanks       w32Ctext         50
037300070904     c                   parm      *blanks       w32Ccurl         10
037400070904     c                   parm      *blanks       w32Cattn         10
037500070904     c                   parm      *blanks       w32Clatt         10
037600070904     c                   parm      *blanks       w32Cscad          4
037700070904     c                   parm      *blanks       w32Cpwd          10
037800070904     c                   parm      *blanks       w32Csts           9
037900070904     c                   parm      *blanks       w32Cerr           1
038000070904if  4c                   if        w32Cerr = 'E'
038100070904     c                   eval      *in90   = *off
038200070904x   4c                   else
038300040831     c                   seton                                        28  42
038400070904     c                   eval      V1Dmsg = %trim($Msg(04)) + ' '
038500040831     c                                    + UTEute
038600040831     c                   leave
038700070904e   4c                   endif
038800040831e   3c                   endif
038900040830      *
039000040831     c     V1Cpo(xx)     reade     AZUTE000
039100040830      *
039200040901e   2c                   ENDDO
039300040901      *
039400040901      * - Se?tutto bene: ripulisco il campo V1Cpo per evidenziarne
039500040901      *   l'avvenuta elaborazione (lasciandone la decodifica per
039600040901      *   visualizzare i P.O. elaborati...)
039700040901if  2c                   if        *in90  = *off
039800040901     c                   clear                   V1Cpo(xx)
039900040901x   2c                   else
040000040901     c                   leave
040100040901e   2c                   endif
040200040831      *
040300040901e   1c                   ENDDO
040400040901      *
040500040901      * Sposto la schiere nei campi a video per evidenziare i P.O.
040600040901      *   elaborati
040700040901     c                   movel     V1Cpo         W1Cpo
040800040901     c                   movea     W1Cpo         DS_V1Cpo
040900040901     c*** NON serve:     movea     V1Dpo         DS_V1Dpo
041000040901     c*** NON serve:     movea     V1Dlc         DS_V1Dlc
041100040830      *
041200040830      * Torno alla 1ª videata D01
041300040830     c                   eval      $Fine  = *off
041400040830     c                   reset                   $Video
041500040830      *
041600040901     c                   ENDSR
041700040830
041800040830      *===============================================================*
041900040830
042000040830** $Msg
042100040901Immettere almeno un codice P.O.
042200040901Codice P.O. già immesso
042300040830Codice P.O. errato
042400040830NON riuscito aggiornamento del profilo utente
042500040831** $Cmd
042600040831CHGUSRPRF  usrprf(
042700040831           curlib(
