000100070411      *PARMS DBGVIEW(*SOURCE)
000200130919     /*PRM  dbgview(*source)
000300130919     /*END
000400070411      *===============================================================*
000500070508      *?TNSDUJR1 - Inserimento multiplo unità EDP - trk "1" pf WFAIM ?*
000600070411      *===============================================================*
000700070605      *A V V E R T E N Z E :?                                       ?*
000800070605      * Occorre? ricompilare ANCHE ?il pgm.?TNSDUJR3? se ?si modifica *
000900070605      * il dspf TNSDUJD1.                                             *
001000070605      *===============================================================*
001100070412      *?N O T E :                                                    ?*
001200070417      * · DUJOPZ = '0' -> reperimento record trk "1"                  *
001300070412      * ·   "    = '1' -> immissione nuovo record trk "1"             *
001400070412      * ·   "    = '2' -> manutenzione record trk "1"                 *
001500070412      * ·   "    = '4' -> cancellazione records trk "1" e "2"         *
001600070517      * ·   "    = '6' -> ri-attivazione inserimento multiplo unità   *
001700070517      *                   (impostazione trk "1" dov'è trk "3" ed      *
001800070517      *                    impostazione trk "2" dov'è trk "4")        *
001900070412      * ·   "    = '7' -> "Blocco" dell'inserimento multipo unità     *
002000070508      *                   (impostazione WIMPRG = 0 in trk "1")        *
002100070412      * ·   "    = '8' -> "Sblocco" dell'inserimento multipo unità    *
002200070508      *                   (impostazione WIMPRG = 1 in trk "1")        *
002300070418      * ·   "    = '9' -> "Blocco" dell'inserimento multipo unità     *
002400070418      *                   per conferma ("passaggio" in UNANA00F).     *
002500070604      * ·   "    = 'd' -> cancellazione records trk "1" e "2"         *
002600070423      *                   (come "4", ma SENZA window di conferma)     *
002700070604      * ·   "    = 'D' -> modifica trk "1" e "2" in "3" e "4"         *
002800070604      *                   (cioè già confermato, riattivabile con '6') *
002900070910      * ·   "    = 'S' -> "Sblocco" dell'inserimento multipo unità    *
003000070910      *                   (come "8", ma  CON  window di conferma)     *
003100070412      *===============================================================*
003200070411
003300070411     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003400070411
003500070411      *---------------------------------------------------------------*
003600070411
003700070605     fWFAIM01L  Uf A e           k disk    usropn
003800070411      *
003900070417     fTNSDUJD1  cf   e             workstn
004000070411
004100070411      *---------------------------------------------------------------*
004200070411
004300070411      *
004400070411      *?Costanti  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
004500070411      *
004600070508      * - Lunghezza massima della matricola in lettura
004700070508     d C_MatLenMax     c                   const(050)
004800070508      * - Lunghezza massima della matricola in UNANA00F
004900070508     d C_StrLenMax     c                   const(015)
005000070411      *
005100070411      *?Schiere - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
005200070411      *
005300070411      * - Messaggi di errore
005400130911     d $Msg            s             78    dim(19)   ctdata  perrcd(1)
005500070411      *
005600070411      *?Ds  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
005700070411      *
005800070411      * - Parametri
005900070411     d KPJBA         e ds
006000070417     d TNSDUJds      e ds                  inz
006100070411      *
006200070411      * - Parametri x Controllo profilo utenti
006300070411     d TIBS34DS      e ds                  inz
006400070411      * - Ds di riferimento al file esterno AZUTE00F
006500070411     d AZUTEds       e ds                  extname(AZUTE00F)
006600070411      * - Ds per dati organigramma
006700070411     d DDatiUte      e ds
006800070411      *
006900130911      * DS campo WIMUNI file WFAIM00F trk "1" prg 01
007000070508     d dWIMuni1      e ds                  inz
007100130911      * DS campo WIMUNI file WFAIM00F trk "1" prg 02
007200130911     d dWIMuni12     e ds                  inz
007300070411      *
007400070411     d Status         sds           333
007500070411     d   SDSpgm          *proc
007600130911      *
007700130911      * -?Controllo/Inversione data?
007800130911     d WLBdat          ds                  inz
007900130911     d   G08dat                       8  0 inz
008000130911     d   G08inv                       8  0 inz
008100130911     d   G08err                       1    inz
008200130911     d   G08tgi                       5  0 inz
008300070411      *
008400070411      *?Variabili - - - - - - - - - - - - - - - - - - - - - - - - - -?*
008500070411      *
008600070411      * - Flags
008700070411     d $Fine           s              1    inz(*off)
008800070411     d $InzD01         s              1    inz(*on)
008900070412     d $InzW01         s              1    inz(*on)
009000070412      *
009100070412      * - Variabili di controllo
009200070412     d $Video          s              2    inz('D1')
009300070518      *
009400070518      * - Campi di comodo
009500070518     d W_trk           s                   inz   like(WIMtrk)
009600070411      *
009700070411      *?Key-List  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
009800070411      *
009900070508      * - WFAIM01L
010000130911     c     K03WIM01      klist
010100130911     c                   kfld                    WIMnum
010200130911     c                   kfld                    WIMtrk
010300130911     c                   kfld                    WIMprg
010400070508     c     K02WIM01      klist
010500070508     c                   kfld                    WIMnum
010600070508     c                   kfld                    WIMtrk
010700070518     c     K02WIM01w     klist
010800070518     c                   kfld                    WIMnum
010900070518     c                   kfld                    W_trk
011000070411
011100070411      *---------------------------------------------------------------*
011200070411      *?RIEPILOGO INDICATORI                                         ?*
011300070411      *---------------------------------------------------------------*
011400070411      * 25    - Comodo                                                *
011500070416      * 28    - Emissione del messaggio di errore a video             *
011600070411      * 51    - D01: Lunghezza matricola errata/mancante              *
011700070411      * 52    - D01: Posizione iniziale sottostringa errata/mancante  *
011800070411      * 53    - D01: Lunghezza sottostringa errata/mancante           *
011900070411      * 54    - D01: Numero di matricole da inserire errato/mancante  *
012000070411      * 55    - D01: Descrizione obbligatoria                         *
012100070411      * 90    - Generico di errore                                    *
012200070411      *---------------------------------------------------------------*
012300070411
012400070411     c     *Entry        plist
012500070411     c                   parm                    KPJBA
012600070411      *
012700070411      * Operazioni Iniziali
012800070411     c                   exsr      RoutInz
012900070411      *
013000070411      * Gestione Video
013100070411      * - Videata x manutenzione dati trk "1" (testata)
013200070411do  1c                   DOW       $Fine     = *off
013300070412cas 2c     $Video        caseq     'D1'          GesD01
013400070412cas 2c     $Video        caseq     'W1'          GesW01
013500070910cas 2c     $Video        caseq     'W2'          GesW02
013600070412e   2c                   endcs
013700070411e   1c                   ENDDO
013800070411      *
013900070411      * Fine
014000070508     c                   UNLOCK    WFAIM01L
014100070412      *
014200130916     c                   movel     dWIMuni1      DUJuni1
014300130916     c                   movel     dWIMuni12     DUJuni2
014400070417     c                   movel(p)  TNSDUJds      KPJBU
014500070418     c                   if             DUJopz    = '4'
014600070418     c                             or   DUJopz    = '8'
014700070604     c                             or   DUJopz    = 'd'
014800070423     c                             or   DUJopz    = 'D'
014900070910     c                             or   DUJopz    = 'S'
015000070412     c                   eval      *inLR     = *on
015100070412     c                   else
015200070605     c                   close     WFAIM01L
015300070412     c                   eval      *inRT     = *on
015400070412     c                   endif
015500070411      *
015600070411      *---------------------------------------------------------------*
015700070411      *?Operazioni Iniziali                                          ?*
015800070411      *---------------------------------------------------------------*
015900070411     c     RoutInz       BEGSR
016000070411      *
016100070411      * Reperimento dati job
016200070411     c                   exsr      DatiJob
016300070605      *
016400070605     c                   if        NOT  %open(WFAIM01L)
016500070605     c                   open      WFAIM01L
016600070605     c                   endif
016700070411      *
016800070412     c                   reset                   $Fine
016900070412     c                   reset                   $InzD01
017000070412     c                   reset                   $InzW01
017100070412      *
017200070411     c                   movel     SDSpgm        V1Tpgm
017300070411      *
017400070412      * Impostazione iniziale dei parametri i/o
017500070417     c                   movel     KPJBU         TNSDUJds
017600070417if  1c                   if        DUJopz    = '1'
017700070417     c                   clear                   DUJnum
017800070412e   1c                   endif
017900130916     c                   eval      DUJuni1   = *blanks
018000130916     c                   eval      DUJuni2   = *blanks
018100070417     c                   eval      DUJret    = *off
018200070417     c                   eval      DUJerr    = *off
018300070417     c                   eval      DUJmsg    = *blanks
018400070508     c                   clear                   dWIMuni1
018500130911     c                   clear                   dWIMuni12
018600070412      *
018700070412      * CONTROLLO PARAMETRI RICEVUTI
018800070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
018900070412sel 1c                   select
019000070418w   1c                   when           DUJopz   <> '0'
019100070418     c                             and  DUJopz   <> '1'
019200070418     c                             and  DUJopz   <> '2'
019300070418     c                             and  DUJopz   <> '4'
019400070518     c                             and  DUJopz   <> '6'
019500070418     c                             and  DUJopz   <> '7'
019600070418     c                             and  DUJopz   <> '8'
019700070418     c                             and  DUJopz   <> '9'
019800070604     c                             and  DUJopz   <> 'd'
019900070423     c                             and  DUJopz   <> 'D'
020000070910     c                             and  DUJopz   <> 'S'
020100070417     c                   eval      DUJerr    = *on
020200070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
020300070412     c                   eval      $Fine     = *on
020400070412     c                   leavesr
020500070418w   1c                   when           DUJopz    = '1'
020600070418     c                             and  DUJnum   <> *zeros
020700070417     c                   eval      DUJerr    = *on
020800070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
020900070412     c                   eval      $Fine     = *on
021000070412     c                   leavesr
021100070418w   1c                   when           DUJopz   <> '1'
021200070418     c                             and  DUJnum    = *zeros
021300070417     c                   eval      DUJerr    = *on
021400070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
021500070412     c                   eval      $Fine     = *on
021600070412     c                   leavesr
021700070412e   1c                   endsl
021800070411      *
021900070508      * REPERIMENTO REC. TRK "1" DAL FILE WFAIM01L
022000070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
022100070417      * - se NON richiesta un'immissione - DUJopz = '1'
022200070417if  1c                   if        DUJopz    = '1'
022300070411     c                   leavesr
022400070411e   1c                   endif
022500070411      *
022600070508     c                   eval      WIMnum    = DUJnum
022700070518     c                   if        DUJopz   <> '6'
022800070508     c                   eval      WIMtrk    = '1'
022900070518     c                   else
023000070518     c                   eval      WIMtrk    = '3'
023100070518     c                   endif
023200130911      *
023300130911     c                   eval      WIMprg    = 2
023400130911if  1c                   if        DUJopz   <> '0'
023500130911     c     K03WIM01      CHAIN     WFAIM000
023600130911x   1c                   else
023700130911     c     K03WIM01      chain(n)  WFAIM000
023800130911e   1c                   endif
023900130911if  1c                   if        NOT  %found(WFAIM01L)
024000130911     c                             and (DUJopz = '0'
024100130911     c                              or  DUJopz = '4')
024200130911     c                   eval      WIMtrk    = '3'
024300130911     c     K03WIM01      chain(n)  WFAIM000
024400130911e   1c                   endif
024500130911if  2c                   if        %found(WFAIM01L)
024600130911     c                   movel     WIMuni        dWIMuni12
024700130911e   1c                   endif
024800130911      *
024900130911     c                   clear                   WIMprg
025000070523if  1c                   if        DUJopz   <> '0'
025100130911     c     K02WIM01      CHAIN     WFAIM000
025200070523x   1c                   else
025300130911     c     K02WIM01      chain(n)  WFAIM000
025400070523e   1c                   endif
025500070518if  1c                   if        NOT  %found(WFAIM01L)
025600070521     c                             and (DUJopz = '0'
025700070521     c                              or  DUJopz = '4')
025800070518     c                   eval      WIMtrk    = '3'
025900130911     c     K02WIM01      chain(n)  WFAIM000
026000070518e   1c                   endif
026100070508if  1c                   if        %found(WFAIM01L)
026200070508     c                   movel     WIMuni        dWIMuni1
026300070411e   1c                   endif
026400070411      *
026500070412      * CONTROLLO COMPATIBILITÀ OPZ. RICHIESTA CON STATUS QUO DEL REC.
026600070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
026700070411sel 1c                   SELECT
026800070411      *
026900070412      * - Record inesistente -> inserim. multiplo cancellato/errato
027000070508w   1c                   WHEN      NOT  %found(WFAIM01L)
027100070417     c                   eval      DUJerr    = *on
027200070503     c                   eval      DUJmsg    = %replace(
027300070503     c                                           %editc(DUJnum:'L'):
027400070503     c                                           $Msg( 1):
027500070509     c                                           %scan('&IMU ':
027600070503     c                                                 $Msg( 1) ))
027700070411     c                   eval      $Fine     = *on
027800070411     c                   leavesr
027900070412      *
028000070412      * - Richiesto SOLO il reperimento del trk "1": uscita
028100070417w   1c                   WHEN      DUJopz    = '0'
028200070417     c*** già così:      eval      DUJerr    = *off
028300070412     c                   eval      $Fine     = *on
028400070412     c                   leavesr
028500070413      *
028600070413      * - Richiesta manutenzione dallo stesso utente: OK
028700130911w   1c                   WHEN           WIMprg    = *zeros
028800070508     c                             and  §WIM1usp  = KNMUS
028900070508     c                             and  §WIM1uin  = *blanks
029000070418     c                             and  DUJopz    = '2'
029100070518      *
029200070518      * - Richiesta RI-attivazione: OK
029300130911w   1c                   WHEN           WIMprg    = *zeros
029400070518     c                             and  DUJopz    = '6'
029500070411      *
029600070411      * - Record già in uso  -> inserim. multiplo già in manutenzione
029700070518      *   (richiedibile SOLO lo "sblocco" o la cancellazione,
029800070518      *    ma dallo STESSO utente)
029900130911w   1c                   WHEN           WIMprg    = *zeros
030000070418     c                             and  DUJopz   <> '8'
030100070910     c                             and  DUJopz   <> 'S'
030200070518     c                             and  DUJopz   <> '4'
030300070604     c                             and  DUJopz   <> 'd'
030400070423     c                             and  DUJopz   <> 'D'
030500130911     c                             OR   WIMprg    = *zeros
030600070518     c                             and (DUJopz    = '8'
030700070910     c                              or  DUJopz    = 'S'
030800070518     c                              or  DUJopz    = '4'
030900070604     c                              or  DUJopz    = 'd'
031000070518     c                              or  DUJopz    = 'D')
031100070508     c                             and (§WIM1usp <> KNMUS
031200120507     c*//                          and  KNMUS    <> 'EDPSM     '
031300070508     c                             and  §WIM1uin  = *blanks
031400070508     c                              or  §WIM1uin <> *blanks
031500070508     c                             and  §WIM1uin <> KNMUS)
031600070417     c                   eval      DUJerr    = *on
031700070508if  2c                   if        §WIM1uin <> *blanks
031800070503     c                   eval      DUJmsg    = %trimr( %replace(
031900070503     c                                           %editc(DUJnum:'L'):
032000070503     c                                           $Msg( 2):
032100070509     c                                           %scan('&IMU ':
032200070503     c                                                 $Msg( 2) )) )
032300070508     c                                       + ' ' + §WIM1uin
032400070411x   2c                   else
032500070503     c                   eval      DUJmsg    = %trimr( %replace(
032600070503     c                                           %editc(DUJnum:'L'):
032700070503     c                                           $Msg( 2):
032800070509     c                                           %scan('&IMU ':
032900070503     c                                                 $Msg( 2) )) )
033000070508     c                                       + ' ' + §WIM1usp
033100070411e   2c                   endif
033200070411     c                   eval      $Fine     = *on
033300070411     c                   leavesr
033400070411      *
033500070411      * - Record libero      -> inserim. multiplo non in manutenzione
033600070418      *   (NON richiedibile "sblocco")
033700070508w   1c                   WHEN           WIMprg    > *zeros
033800070910     c                             and (DUJopz    = '8'
033900070910     c                              or  DUJopz    = 'D')
034000070417     c                   eval      DUJerr    = *on
034100070503     c                   eval      DUJmsg    = %replace(
034200070503     c                                           %editc(DUJnum:'L'):
034300070503     c                                           $Msg( 3):
034400070509     c                                           %scan('&IMU ':
034500070503     c                                                 $Msg( 3) ))
034600070411     c                   eval      $Fine     = *on
034700070411     c                   leavesr
034800070412      *
034900070411e   1c                   ENDSL
035000070411      *
035100070412      * GESTIONE INIZIALE DELL'OPZIONE RICEVUTA
035200070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
035300070411sel 1c                   SELECT
035400070418      * - 7=Blocco / 8=Sblocco / 9=Blocco x Conferma
035500070418      *   (SENZA visualizzare nulla)
035600070418w   1c                   WHEN           DUJopz    = '7'
035700070418     c                             or   DUJopz    = '8'
035800070418     c                             or   DUJopz    = '9'
035900070508     c                   exsr      xLockWFAIM
036000070910      * - S=Sblocco
036100070910      *   CON visualizzazione window di conferma
036200070910w   1c                   WHEN      DUJopz    = 'S'
036300070910     c                   eval      $Video    = 'W2'
036400070910     c                   eval      $InzW01   = *on
036500070412      * - 4=Cancellazione ("totale": trk "1" e "2")
036600070423      *   · con window di conferma
036700070417w   1c                   WHEN      DUJopz    = '4'
036800070412     c                   eval      $Video    = 'W1'
036900070518     c                   eval      $InzW01   = *on
037000070604      *   · senza window di conferma
037100070604w   1c                   WHEN      DUJopz    = 'd'
037200070604     c                   exsr      Dlt_WFAIM
037300070604      *   · passaggio da trk "1"/"2" a trk "3"/"4"
037400070423w   1c                   WHEN      DUJopz    = 'D'
037500070518     c                   exsr      Igc_WFAIM
037600070412      * - 1=Immissione / 2=Modifica
037700070412x   1c                   OTHER
037800070412     c                   eval      $Video    = 'D1'
037900070412     c                   eval      $inzD01   = *on
038000070411e   1c                   ENDSL
038100070411      *
038200070411     c                   ENDSR
038300070411      *
038400070411      *---------------------------------------------------------------*
038500070411      *?Reperimento Dati del job (Utente/Operativi)                  ?*
038600070411      *---------------------------------------------------------------*
038700070411     c     DatiJob       BEGSR
038800070411      *
038900070411     c     *dtaara       define    §azute        azuteds
039000070411     c     *dtaara       define    §datiute      ddatiute
039100070411      *
039200070411     c                   in(E)     *dtaara
039300070411     c                   IF        %ERROR or RSUT = *blanks
039400070411     c                   clear                   Tibs34Ds
039500070411     c                   call      'TIBS34R'
039600070411     c                   parm                    Tibs34Ds
039700070411     c                   in        *dtaara
039800070411     c                   ENDIF
039900070411      *
040000070411     c                   ENDSR
040100070411      *
040200070411      *---------------------------------------------------------------*
040300070411      *?Gestione videata D01                                         ?*
040400070411      *---------------------------------------------------------------*
040500070411     c     GesD01        BEGSR
040600070411      *
040700070411      * Inizializzazione della videata
040800070411if  1c                   if        $InzD01   = *on
040900070411     c                   exsr      InzD01
041000070411     c                   eval      $InzD01   = *off
041100070411e   1c                   endif
041200070411      *
041300070411      * Scrittura di testata e riga tasti funzionali abilitati
041400070417     c                   write     SDUJT01
041500070417     c                   write     SDUJZ01
041600070411      *
041700070411      * Emissione della videata
041800070417     c                   exfmt     SDUJD01
041900070411     c                   setoff                                       28  90
042000070411     c                   clear                   V1Dmsg
042100070411      *
042200070411sel 1c                   SELECT
042300070411      * F3=Fine
042400070411w   1c                   WHEN      *inKC
042500070411     c                   exsr      F03D01
042600070411      * F12=Ritorno
042700070411w   1c                   WHEN      *inKL
042800070411     c                   exsr      F12D01
042900070411      * Invio / F6=Conferma
043000070411x   1c                   OTHER
043100070411      * - Controllo dati immessi a video
043200070411     c                   exsr      CtrD01
043300070411      * - F6=Conferma
043400070411if  2c                   if        *inKF  and  NOT *in90
043500070411     c                   exsr      F06D01
043600070411e   2c                   endif
043700070411e   1c                   ENDSL
043800070411      *
043900070411     c                   ENDSR
044000070411      *
044100070411      *---------------------------------------------------------------*
044200070411      *?Inizializzazione videata D01                                 ?*
044300070411      *---------------------------------------------------------------*
044400070411     c     InzD01        BEGSR
044500070411      *
044600070417     c                   clear                   SDUJD01
044700070411      *
044800070411      * Impostazione campi già memorizzati (solo se modifica)
044900070518     c                   SELECT
045000070518if  1c                   WHEN      DUJopz    = '1'
045100070411     c                   eval      V1Ctxt    = 'IMMISSIONE '
045200070411     c                                       + 'PARAMETRI PER UN NUOV+
045300070411     c                                          O INSERIMENTO MULTIPL+
045400070411     c                                          O UNITÀ'
045500070518if  1c                   WHEN      DUJopz    = '6'
045600070518     c                   eval      V1Ctxt    = 'RI-ATTIVAZIONE '
045700070518     c                                       + 'DELL''INSERIMENTO MUL+
045800070518     c                                          TIPLPLO UNITÀ N° '
045900070518     c                                       + %trim( %editc(
046000070518     c                                          WIMnum : 'K' ))
046100070518     c                   eval      V1Clm1    = §WIM1lm1
046200070518     c                   eval      V1Cim2    = §WIM1im2
046300070518     c                   eval      V1Clm2    = §WIM1lm2
046400070518     c                   eval      V1Cnmi    = §WIM1nmi
046500130911     c                   eval      V1Cdes    = §WIM1des + §WIM12des
046600130911     c                   eval      V1Cdoc    = §WIM12doc
046700130911     c                   eval      V1Cddo    = §WIM12ddo
046800130911     c                   eval      V1Cord    = §WIM12ord
046900070518x   1c                   OTHER
047000070411     c                   eval      V1Ctxt    = 'MODIFICA '
047100070411     c                                       + 'PARAMETRI DELL''INSERI+
047200070411     c                                          MENTO MULTIPLO UNITÀ N+
047300070411     c                                          ° '
047400070411     c                                       + %trim( %editc(
047500070508     c                                          WIMnum : 'K' ))
047600070508     c                   eval      V1Clm1    = §WIM1lm1
047700070508     c                   eval      V1Cim2    = §WIM1im2
047800070508     c                   eval      V1Clm2    = §WIM1lm2
047900070508     c                   eval      V1Cnmi    = §WIM1nmi
048000130911     c                   eval      V1Cdes    = §WIM1des + §WIM12des
048100130911     c                   eval      V1Cdoc    = §WIM12doc
048200130911     c                   eval      V1Cddo    = §WIM12ddo
048300130911     c                   eval      V1Cord    = §WIM12ord
048400070518e   1c                   ENDSL
048500070411      *
048600070411     c                   ENDSR
048700070411      *
048800070411      *---------------------------------------------------------------*
048900070411      *?Controllo dati immessi in videata D01                        ?*
049000070411      *---------------------------------------------------------------*
049100070411     c     CtrD01        BEGSR
049200070411      *
049300070411     c                   movea     *zeros        *in(50)
049400070411      *
049500070411sel 1c                   select
049600070411      * Lunghezza matricola
049700070411w   1c                   when      V1Clm1    = *zeros
049800070411     c                   seton                                        285190
049900070411     c                   eval      V1Dmsg    = $Msg( 4)
050000070418w   1c                   when      V1Clm1    > C_MatLenMax
050100070418     c                   seton                                        285190
050200070418     c                   eval      V1Dmsg    = $Msg( 5)
050300070411w   1c                   when      V1Clm1    < *zeros
050400070411     c                   seton                                        285190
050500070411     c                   eval      V1Dmsg    = $Msg( 5)
050600070411      * Posizione iniziale nel codice inserito
050700070411w   1c                   when      V1Cim2    = *zeros
050800070411     c                   seton                                        285290
050900070411     c                   eval      V1Dmsg    = $Msg( 6)
051000070411w   1c                   when      V1Cim2    < *zeros
051100070411     c                   seton                                        285290
051200070411     c                   eval      V1Dmsg    = $Msg( 7)
051300070411      * Conformità tra lunghezza-matricola e posizione-iniziale
051400070411w   1c                   when      V1Cim2    > V1Clm1
051500070411     c                   seton                                        285290
051600070411     c                   eval      V1Dmsg    = $Msg( 8)
051700070411      * Lunghezza da posizione iniziale nel codice inserito
051800070411w   1c                   when      V1Clm2    = *zeros
051900070411     c                   seton                                        285390
052000070411     c                   eval      V1Dmsg    = $Msg( 9)
052100070411w   1c                   when      V1Clm2    < *zeros
052200070411     c                   seton                                        285390
052300070411     c                   eval      V1Dmsg    = $Msg(10)
052400070508w   1c                   when      V1Clm2    > C_StrLenMax
052500070508     c                   seton                                        285390
052600070508     c                   eval      V1Dmsg    = $Msg(11)
052700070411      * Conformità tra lunghezza-matricola e lunghezza-sottostringa
052800070411     c                   when      V1Clm1    < (V1Cim2 + V1Clm2 - 1)
052900070411     c                   seton                                        285390
053000070508     c                   eval      V1Dmsg    = $Msg(12)
053100070411      * Numero totale di matricole da inserire obbligatorio
053200070411w   1c                   when      V1Cnmi    = *zeros
053300070411     c                   seton                                        285490
053400070508     c                   eval      V1Dmsg    = $Msg(13)
053500070411w   1c                   when      V1Cnmi    < *zeros
053600070411     c                   seton                                        285490
053700070508     c                   eval      V1Dmsg    = $Msg(14)
053800070411      * Descrizione obbligatoria
053900070411w   1c                   when      V1Cdes    = *blanks
054000070411     c                   seton                                        285590
054100070508     c                   eval      V1Dmsg    = $Msg(15)
054200130911      * Numero documento obbligatorio
054300130911w   1c                   when      V1Cdoc    = *zeros
054400130911     c                   seton                                        285690
054500130911     c                   eval      V1Dmsg    = $Msg(16)
054600130911      * Data documento obbligatoria
054700130911w   1c                   when      V1Cddo    = *zeros
054800130911     c                   seton                                        285790
054900130911     c                   eval      V1Dmsg    = $Msg(17)
055000130911      *
055100070411e   1c                   endsl
055200130911      *
055300130911if  1c                   if        *in90
055400130911     c                   leavesr
055500130911e   1c                   endif
055600130911      *
055700130911      * Data documento formalmente errata
055800130911     c                   eval      G08dat = V1Cddo
055900130911     c                   clear                   G08err
056000130911     c                   call      'XSRDA8'
056100130911     c                   parm                    WLBdat
056200130911if  1c                   if        G08err = *on
056300130911     c                   seton                                        285790
056400130911     c                   eval      V1Dmsg    = $Msg(18)
056500130911     c                   leavesr
056600130911x   1c                   else
056700130911     c                   z-add     G08dat        V1Cddo
056800130911e   1c                   endif
056900130911      *
057000130911      * Numero ordine obbligatorio
057100130911if  1c                   if        V1Cord    = *zeros
057200130911     c                   seton                                        285890
057300130911     c                   eval      V1Dmsg    = $Msg(19)
057400130911     c                   leavesr
057500130911e   1c                   endif
057600070411      *
057700070411     c                   ENDSR
057800070411      *
057900070411      *---------------------------------------------------------------*
058000070411      *?Gestione tasto funzionale F03 da videata D01                 ?*
058100070411      *---------------------------------------------------------------*
058200070411     c     F03D01        BEGSR
058300070411      *
058400070411      * Uscita dal pgm
058500070417     c                   eval      DUJret    = '1'
058600070411     c                   eval      $Fine     = *on
058700070411      *
058800070411     c                   ENDSR
058900070411      *
059000070411      *---------------------------------------------------------------*
059100070411      *?Gestione tasto funzionale F06 da videata D01                 ?*
059200070411      *---------------------------------------------------------------*
059300070411     c     F06D01        BEGSR
059400070411      *
059500070411      * Aggiornamento numeratore (se immissione)
059600070417if  1c                   IF        DUJopz    = '1'
059700070508     c                   exsr      RepNumFree
059800070411if  2c                   if        *in90
059900070411     c                   leavesr
060000070411e   2c                   endif
060100070411e   1c                   ENDIF
060200070411      *
060300070508      * Aggiornamento trk "1" nel file WFAIM00F
060400070508     c                   exsr      RepRecWFAIM
060500070518if  1c                   if        DUJopz   <> '6'
060600070508     c                   exsr      xLockWFAIM
060700070518x   1c                   else
060800070518     c                   exsr      Inc_WFAIM
060900070518e   1c                   endif
061000070411      *
061100070411     c                   ENDSR
061200070411      *
061300070411      *---------------------------------------------------------------*
061400070411      *?Gestione tasto funzionale F12 da videata D01                 ?*
061500070411      *---------------------------------------------------------------*
061600070411     c     F12D01        BEGSR
061700070411      *
061800070411      * Uscita dal pgm
061900070417     c                   eval      DUJret    = '2'
062000070411     c                   eval      $Fine     = *on
062100070411      *
062200070411     c                   ENDSR
062300070412      *
062400070412      *---------------------------------------------------------------*
062500070412      *?Gestione videata W01                                         ?*
062600070412      *---------------------------------------------------------------*
062700070412     c     GesW01        BEGSR
062800070412      *
062900070412      * Inizializzazione della videata
063000070412if  1c                   if        $InzW01   = *on
063100070417     c                   clear                   SDUJW01
063200070417     c                   eval      W1Cnum    = DUJnum
063300070412     c                   eval      $InzW01   = *off
063400070412e   1c                   endif
063500070412      *
063600070412      * Emissione della window di conferma
063700070417     c                   exfmt     SDUJW01
063800070412      *
063900070412sel 1c                   SELECT
064000070412      * F12 = Ritorno
064100070412w   1c                   WHEN      *inKL
064200070412     c                   exsr      F12D01
064300070412      * F6 = Conferma
064400070412w   1c                   WHEN      *inKF
064500070508     c                   exsr      Dlt_WFAIM
064600070412e   1c                   ENDSL
064700070412      *
064800070412     c                   ENDSR
064900070910      *
065000070910      *---------------------------------------------------------------*
065100070910      *?Gestione videata W02                                         ?*
065200070910      *---------------------------------------------------------------*
065300070910     c     GesW02        BEGSR
065400070910      *
065500070910      * Inizializzazione della videata
065600070910if  1c                   if        $InzW01   = *on
065700070910     c                   clear                   SDUJW02
065800070910     c                   eval      W1Cnum    = DUJnum
065900070910     c                   eval      $InzW01   = *off
066000070910e   1c                   endif
066100070910      *
066200070910      * Emissione della window di conferma
066300070910     c                   exfmt     SDUJW02
066400070910      *
066500070910sel 1c                   SELECT
066600070910      * F12 = Ritorno
066700070910w   1c                   WHEN      *inKL
066800070910     c                   exsr      F12D01
066900070910      * F6 = Conferma
067000070910w   1c                   WHEN      *inKF
067100070910     c                   exsr      xLockWFAIM
067200070910e   1c                   ENDSL
067300070910      *
067400070910     c                   ENDSR
067500070411      *
067600070411      *---------------------------------------------------------------*
067700070508      *?Reperimento nuovo numero progressivo                         ?*
067800070411      *---------------------------------------------------------------*
067900070508     c     RepNumFree    BEGSR
068000070411      *
068100070508     c     *hival        setgt     WFAIM000
068200070508     c                   readp(n)  WFAIM000
068300070411      *
068400070508if  1c                   if        %eof(WFAIM01L)
068500070508     c                   eval      DUJnum    = 1
068600070508x   1c                   else
068700070508     c                   eval      DUJnum    = WIMnum + 1
068800070411e   1c                   endif
068900070411      *
069000070411     c                   ENDSR
069100070412      *
069200070412      *---------------------------------------------------------------*
069300070508      *?Reperimento record trk "1" del file WFAIM00F                 ?*
069400070412      *---------------------------------------------------------------*
069500070508     c     RepRecWFAIM   BEGSR
069600070412      *
069700130911      * Reperimento record per aggiornamento - Prg 01
069800070508      * (anche se inserimento: nuovo numero DUJNUM già reperito)
069900070508     c                   eval      WIMnum    = DUJnum
070000070518     c                   if        DUJopz   <> '6'
070100070508     c                   eval      WIMtrk    = '1'
070200070518     c                   else
070300070518     c                   eval      WIMtrk    = '3'
070400070518     c                   endif
070500130911     c     K02WIM01      chain     WFAIM000
070600070412      *
070700130911      * Impostazione nuovi dati (prg 01)
070800070412      *
070900070508if  1c                   IF        NOT  %found(WFAIM01L)
071000070508     c                   clear                   dWIMuni1
071100070508     c                   clear                   WFAIM000
071200070508     c                   z-add     DUJnum        WIMnum
071300070508     c                   eval      WIMtrk    = '1'
071400070412x   1c                   ELSE
071500070508     c                   movel     WIMuni        dWIMuni1
071600070412e   1c                   ENDIF
071700070412      *
071800070508     c                   eval      §WIM1usp  = KNMUS
071900070508     c                   eval      §WIM1des  = V1Cdes
072000070508     c                   eval      §WIM1lm1  = V1Clm1
072100070508     c                   eval      §WIM1im2  = V1Cim2
072200070508     c                   eval      §WIM1lm2  = V1Clm2
072300070508     c                   eval      §WIM1nmi  = V1Cnmi
072400130923     c*//                movel(p)  dWIMuni1      WIMuni
072500130911      *
072600130911      * Reperimento record per aggiornamento - Prg 02
072700130916     c*// già così:      eval      WIMnum    = DUJnum
072800130916     c*// già così:      if        DUJopz   <> '6'
072900130916     c*// già così:      eval      WIMtrk    = '1'
073000130916     c*// già così:      else
073100130916     c*// già così:      eval      WIMtrk    = '3'
073200130916     c*// già così:      endif
073300130911     c                   eval      WIMprg    = 2
073400130911     c     K03WIM01      chain     WFAIM000
073500130911      *
073600130911      * Impostazione nuovi dati (prg 02)
073700130911      *
073800130911if  1c                   IF        NOT  %found(WFAIM01L)
073900130911     c                   clear                   dWIMuni12
074000130911     c                   clear                   WFAIM000
074100130911     c                   z-add     DUJnum        WIMnum
074200130911     c                   eval      WIMtrk    = '1'
074300130911     c                   eval      WIMprg    = 2
074400130911x   1c                   ELSE
074500130911     c                   movel     WIMuni        dWIMuni12
074600130911e   1c                   ENDIF
074700130911      *
074800130911     c                   eval      §WIM12des = %subst( V1Cdes :
074900130911     c                                                 %size(§WIM1des)+1 )
075000130911     c                   eval      §WIM12doc = V1Cdoc
075100130911     c                   eval      §WIM12ddo = V1Cddo
075200130911     c                   eval      §WIM12ord = V1Cord
075300130923     c*//                movel(p)  dWIMuni12     WIMuni
075400070412      *
075500070412     c                   ENDSR
075600070411      *
075700070411      *---------------------------------------------------------------*
075800130916      *?Aggiornamento record trk "1" del file WFAIM00F               ?*
075900070411      *---------------------------------------------------------------*
076000070508     c     xLockWFAIM    BEGSR
076100070412      *
076200130923      * Se già reperito il prg. 2 del trk "1":
076300130923      *   occorre rileggere il 1° rec. (con prg. 0 o 1)
076400130923if  1c                   if        WIMprg > 1
076500130923     c     K02WIM01      chain     WFAIM000
076600130923e   1c                   endif
076700130923      *
076800070418sel 1c                   select
076900070418      * Richiesta Disallocazione
077000070910w   1c                   when           DUJopz    = '8'
077100070910     c                             or   DUJopz    = 'S'
077200070508     c                   eval      WIMprg    = 1
077300070508     c                   eval      §WIM1uin  = *blanks
077400130923     c*//                movel(p)  dWIMuni1      WIMuni
077500070418      * Richiesta Allocazione x Conferma (inserimento in UNANA00F)
077600070418w   1c                   when      DUJopz    = '9'
077700130911     c                   eval      WIMprg    = *zeros
077800070508     c                   eval      §WIM1uin  = KNMUS
077900130923     c*//                movel(p)  dWIMuni1      WIMuni
078000070418      * Richiesta Allocazione
078100070418x   1c                   other
078200070508     c                   eval      WIMprg    = *zeros
078300070531     c                   eval      §WIM1usp  = KNMUS
078400130923     c*//                movel(p)  dWIMuni1      WIMuni
078500070418     c                   endsl
078600070411      *
078700130916      * Scrittura/Aggiornamento 1° record
078800130919     c*//  K02WIM01      chain     WFAIM000             ?NO: variata key WIMprg!!!?
078900130923     c                   movel(p)  dWIMuni1      WIMuni
079000070508if  1c                   if        %found(WFAIM01L)
079100070411      *                  __________________
079200070508     c                   UPDATE    WFAIM000
079300070411      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079400070411x   1c                   else
079500070411      *                  __________________
079600070508     c                   WRITE     WFAIM000
079700070411      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079800070411e   1c                   endif
079900130916      *
080000130916      * Scrittura/Aggiornamento 2° record
080100130916     c                   eval      WIMprg    = 2
080200130916     c     K03WIM01      chain     WFAIM000
080300130923     c                   movel(p)  dWIMuni12     WIMuni
080400130916if  1c                   if        %found(WFAIM01L)
080500130916      *                  __________________
080600130916     c                   UPDATE    WFAIM000
080700130916      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
080800130916x   1c                   else
080900130916      *                  __________________
081000130916     c                   WRITE     WFAIM000
081100130916      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
081200130916e   1c                   endif
081300070412      *
081400070412      * Uscita dal pgm
081500070417     c                   eval      DUJret    = *off
081600070412     c                   eval      $Fine     = *on
081700070411      *
081800070411     c                   ENDSR
081900070517      *
082000070517      *---------------------------------------------------------------*
082100070517      *?Cancellazione totale di un "inserimento multiplo unità"      ?*
082200070517      *---------------------------------------------------------------*
082300070517     c     Dlt_WFAIM     BEGSR
082400070517      *
082500070517     c     DUJnum        setll     WFAIM000
082600070517     c     DUJnum        reade     WFAIM000
082700070517      *
082800070517do  1c                   DOW       NOT  %eof(WFAIM01L)
082900070517      *                  __________________
083000070517     c                   DELETE    WFAIM000
083100070517      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
083200070517     c     DUJnum        reade     WFAIM000
083300070517e   1c                   ENDDO
083400070517      *
083500070517      * Uscita dal pgm
083600070517     c                   eval      DUJret    = *off
083700070517     c                   eval      $Fine     = *on
083800070517      *
083900070517     c                   ENDSR
084000070411      *
084100070411      *---------------------------------------------------------------*
084200070518      *?Impostazione "inserimento multiplo unità" come Già Confermato?*
084300070411      *---------------------------------------------------------------*
084400070518     c     Igc_WFAIM     BEGSR
084500070411      *
084600070517      * Elaborazione testata   (trk "1")
084700070508     c                   eval      WIMnum    = DUJnum
084800130916     c                   eval      W_trk     = '1'
084900130916     c     K02WIM01w     setll     WFAIM000
085000130916     c     K02WIM01w     reade     WFAIM000
085100130916do  1c                   DOW       NOT  %eof(WFAIM01L)
085200070517      *
085300070517     c                   eval      WIMtrk    = '3'
085400070411      *                  __________________
085500070517     c                   UPDATE    WFAIM000
085600070411      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
085700130916     c     K02WIM01w     reade     WFAIM000
085800130916e   1c                   ENDDO
085900070411      *
086000070517      * Elaborazione dettaglio (trk "2") esistente
086100070508     c                   eval      WIMnum    = DUJnum
086200070518     c                   eval      W_trk     = '2'
086300070518     c     K02WIM01w     setll     WFAIM000
086400070518     c     K02WIM01w     reade     WFAIM000
086500070508do  1c                   DOW       NOT  %eof(WFAIM01L)
086600070517      *
086700070517     c                   eval      WIMtrk    = '4'
086800070411      *                  __________________
086900070517     c                   UPDATE    WFAIM000
087000070411      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
087100070518     c     K02WIM01w     reade     WFAIM000
087200070411e   1c                   ENDDO
087300070412      *
087400070412      * Uscita dal pgm
087500070417     c                   eval      DUJret    = *off
087600070412     c                   eval      $Fine     = *on
087700070411      *
087800070411     c                   ENDSR
087900070518      *
088000070518      *---------------------------------------------------------------*
088100070518      *?Impostazione "inserimento multiplo unità" come Non Confermato?*
088200070518      *---------------------------------------------------------------*
088300070518     c     Inc_WFAIM     BEGSR
088400070518      *
088500070518      * Elaborazione testata   (trk "3")
088600070518     c                   eval      WIMnum    = DUJnum
088700130916     c                   eval      W_trk     = '3'
088800130916     c     K02WIM01w     setll     WFAIM000
088900130916     c     K02WIM01w     reade     WFAIM000
089000130916do  1c                   DOW       NOT  %eof(WFAIM01L)
089100070518      *
089200070518     c                   eval      WIMtrk    = '1'
089300130916if  2c                   if        WIMprg    = *zero
089400130916     c                   eval      WIMprg    = 1
089500130916e   2c                   endif
089600070518      *                  __________________
089700070518     c                   UPDATE    WFAIM000
089800070518      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
089900130916     c     K02WIM01w     reade     WFAIM000
090000130916e   1c                   ENDDO
090100070518      *
090200070518      * Elaborazione dettaglio (trk "4") esistente
090300070518     c                   eval      WIMnum    = DUJnum
090400070518     c                   eval      W_trk     = '4'
090500070518     c     K02WIM01w     setll     WFAIM000
090600070518     c     K02WIM01w     reade     WFAIM000
090700070518do  1c                   DOW       NOT  %eof(WFAIM01L)
090800070518      *
090900070518     c                   eval      WIMtrk    = '2'
091000070518      *                  __________________
091100070518     c                   UPDATE    WFAIM000
091200070518      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
091300070518     c     K02WIM01w     reade     WFAIM000
091400070518e   1c                   ENDDO
091500070518      *
091600070518      * Uscita dal pgm
091700070518     c                   eval      DUJret    = *off
091800070518     c                   eval      $Fine     = *on
091900070518      *
092000070518     c                   ENDSR
092100070411
092200070411** - $MSG -------------------------------------------------------------------*
092300070509"Inserimento multiplo unità" &IMU  non trovato nell'archivio                    1
092400070509"Inserimento multiplo unità" &IMU  in uso dall'utente                           2
092500070509"Inserimento multiplo unità" &IMU  NON più "bloccato"                           3
092600070411Lunghezza matricola obbligatoria                                                4
092700070508Lunghezza matricola errata (min 1 - max 50)                                     5
092800070411Posizione iniziale nel codice inserito obbligatoria                             6
092900070411Posizione iniziale nel codice inserito errata                                   7
093000070411Posizione iniziale nel codice OLTRE la lunghezza della matricola                8
093100070411Lunghezza da posizione iniziale nel codice inserito obbligatoria                9
093200070411Lunghezza da posizione iniziale nel codice inserito errata                     10
093300070508Lunghezza della sottostringa da considerare errata (min 1 - max 15)            11
093400070508Lunghezza della sottostringa OLTRE la lunghezza della matricola                12
093500070508Numero totale di matricole da inserire obbligatorio                            13
093600070508Numero totale di matricole da inserire errato                                  14
093700070508Descrizione obbligatoria                                                       15
093800130911Numero documento obbligatorio                                                  16
093900130911Data documento obbligatoria                                                    17
094000130911Data documento formalmente errata                                              18
094100130911Numero ordine obbligatorio                                                     19
