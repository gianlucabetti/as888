000100070411      *PARMS DBGVIEW(*SOURCE)
000200070411      *===============================================================*
000300070508      *?TNSDUJR1 - Inserimento multiplo unità EDP - trk "1" pf WFAIM ?*
000400070411      *===============================================================*
000500070605      *A V V E R T E N Z E :?                                       ?*
000600070605      * Occorre? ricompilare ANCHE ?il pgm.?TNSDUJR3? se ?si modifica *
000700070605      * il dspf TNSDUJD1.                                             *
000800070605      *===============================================================*
000900070412      *?N O T E :                                                    ?*
001000070417      * · DUJOPZ = '0' -> reperimento record trk "1"                  *
001100070412      * ·   "    = '1' -> immissione nuovo record trk "1"             *
001200070412      * ·   "    = '2' -> manutenzione record trk "1"                 *
001300070412      * ·   "    = '4' -> cancellazione records trk "1" e "2"         *
001400070517      * ·   "    = '6' -> ri-attivazione inserimento multiplo unità   *
001500070517      *                   (impostazione trk "1" dov'è trk "3" ed      *
001600070517      *                    impostazione trk "2" dov'è trk "4")        *
001700070412      * ·   "    = '7' -> "Blocco" dell'inserimento multipo unità     *
001800070508      *                   (impostazione WIMPRG = 0 in trk "1")        *
001900070412      * ·   "    = '8' -> "Sblocco" dell'inserimento multipo unità    *
002000070508      *                   (impostazione WIMPRG = 1 in trk "1")        *
002100070418      * ·   "    = '9' -> "Blocco" dell'inserimento multipo unità     *
002200070418      *                   per conferma ("passaggio" in UNANA00F).     *
002300070604      * ·   "    = 'd' -> cancellazione records trk "1" e "2"         *
002400070423      *                   (come "4", ma SENZA window di conferma)     *
002500070604      * ·   "    = 'D' -> modifica trk "1" e "2" in "3" e "4"         *
002600070604      *                   (cioè già confermato, riattivabile con '6') *
002700070910      * ·   "    = 'S' -> "Sblocco" dell'inserimento multipo unità    *
002800070910      *                   (come "8", ma  CON  window di conferma)     *
002900070412      *===============================================================*
003000070411
003100070411     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003200070411
003300070411      *---------------------------------------------------------------*
003400070411
003500070605     fWFAIM01L  Uf A e           k disk    usropn
003600070411      *
003700070417     fTNSDUJD1  cf   e             workstn
003800070411
003900070411      *---------------------------------------------------------------*
004000070411
004100070411      *
004200070411      *?Costanti  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
004300070411      *
004400070508      * - Lunghezza massima della matricola in lettura
004500070508     d C_MatLenMax     c                   const(050)
004600070508      * - Lunghezza massima della matricola in UNANA00F
004700070508     d C_StrLenMax     c                   const(015)
004800070411      *
004900070411      *?Schiere - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
005000070411      *
005100070411      * - Messaggi di errore
005200070508     d $Msg            s             78    dim(15)   ctdata  perrcd(1)
005300070411      *
005400070411      *?Ds  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
005500070411      *
005600070411      * - Parametri
005700070411     d KPJBA         e ds
005800070417     d TNSDUJds      e ds                  inz
005900070411      *
006000070411      * - Parametri x Controllo profilo utenti
006100070411     d TIBS34DS      e ds                  inz
006200070411      * - Ds di riferimento al file esterno AZUTE00F
006300070411     d AZUTEds       e ds                  extname(AZUTE00F)
006400070411      * - Ds per dati organigramma
006500070411     d DDatiUte      e ds
006600070411      *
006700070508      * DS campo WIMUNI file WFAIM00F trk "1"
006800070508     d dWIMuni1      e ds                  inz
006900070411      *
007000070411     d Status         sds           333
007100070411     d   SDSpgm          *proc
007200070411      *
007300070411      *?Variabili - - - - - - - - - - - - - - - - - - - - - - - - - -?*
007400070411      *
007500070411      * - Flags
007600070411     d $Fine           s              1    inz(*off)
007700070411     d $InzD01         s              1    inz(*on)
007800070412     d $InzW01         s              1    inz(*on)
007900070412      *
008000070412      * - Variabili di controllo
008100070412     d $Video          s              2    inz('D1')
008200070518      *
008300070518      * - Campi di comodo
008400070518     d W_trk           s                   inz   like(WIMtrk)
008500070411      *
008600070411      *?Key-List  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
008700070411      *
008800070508      * - WFAIM01L
008900070508     c     K02WIM01      klist
009000070508     c                   kfld                    WIMnum
009100070508     c                   kfld                    WIMtrk
009200070518     c     K02WIM01w     klist
009300070518     c                   kfld                    WIMnum
009400070518     c                   kfld                    W_trk
009500070411
009600070411      *---------------------------------------------------------------*
009700070411      *?RIEPILOGO INDICATORI                                         ?*
009800070411      *---------------------------------------------------------------*
009900070411      * 25    - Comodo                                                *
010000070416      * 28    - Emissione del messaggio di errore a video             *
010100070411      * 51    - D01: Lunghezza matricola errata/mancante              *
010200070411      * 52    - D01: Posizione iniziale sottostringa errata/mancante  *
010300070411      * 53    - D01: Lunghezza sottostringa errata/mancante           *
010400070411      * 54    - D01: Numero di matricole da inserire errato/mancante  *
010500070411      * 55    - D01: Descrizione obbligatoria                         *
010600070411      * 90    - Generico di errore                                    *
010700070411      *---------------------------------------------------------------*
010800070411
010900070411     c     *Entry        plist
011000070411     c                   parm                    KPJBA
011100070411      *
011200070411      * Operazioni Iniziali
011300070411     c                   exsr      RoutInz
011400070411      *
011500070411      * Gestione Video
011600070411      * - Videata x manutenzione dati trk "1" (testata)
011700070411do  1c                   DOW       $Fine     = *off
011800070412cas 2c     $Video        caseq     'D1'          GesD01
011900070412cas 2c     $Video        caseq     'W1'          GesW01
012000070910cas 2c     $Video        caseq     'W2'          GesW02
012100070412e   2c                   endcs
012200070411e   1c                   ENDDO
012300070411      *
012400070411      * Fine
012500070508     c                   UNLOCK    WFAIM01L
012600070412      *
012700070508     c                   movel     dWIMuni1      DUJuni
012800070417     c                   movel(p)  TNSDUJds      KPJBU
012900070418     c                   if             DUJopz    = '4'
013000070418     c                             or   DUJopz    = '8'
013100070604     c                             or   DUJopz    = 'd'
013200070423     c                             or   DUJopz    = 'D'
013300070910     c                             or   DUJopz    = 'S'
013400070412     c                   eval      *inLR     = *on
013500070412     c                   else
013600070605     c                   close     WFAIM01L
013700070412     c                   eval      *inRT     = *on
013800070412     c                   endif
013900070411      *
014000070411      *---------------------------------------------------------------*
014100070411      *?Operazioni Iniziali                                          ?*
014200070411      *---------------------------------------------------------------*
014300070411     c     RoutInz       BEGSR
014400070411      *
014500070411      * Reperimento dati job
014600070411     c                   exsr      DatiJob
014700070605      *
014800070605     c                   if        NOT  %open(WFAIM01L)
014900070605     c                   open      WFAIM01L
015000070605     c                   endif
015100070411      *
015200070412     c                   reset                   $Fine
015300070412     c                   reset                   $InzD01
015400070412     c                   reset                   $InzW01
015500070412      *
015600070411     c                   movel     SDSpgm        V1Tpgm
015700070411      *
015800070412      * Impostazione iniziale dei parametri i/o
015900070417     c                   movel     KPJBU         TNSDUJds
016000070417if  1c                   if        DUJopz    = '1'
016100070417     c                   clear                   DUJnum
016200070412e   1c                   endif
016300070417     c                   eval      DUJuni    = *blanks
016400070417     c                   eval      DUJret    = *off
016500070417     c                   eval      DUJerr    = *off
016600070417     c                   eval      DUJmsg    = *blanks
016700070508     c                   clear                   dWIMuni1
016800070412      *
016900070412      * CONTROLLO PARAMETRI RICEVUTI
017000070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
017100070412sel 1c                   select
017200070418w   1c                   when           DUJopz   <> '0'
017300070418     c                             and  DUJopz   <> '1'
017400070418     c                             and  DUJopz   <> '2'
017500070418     c                             and  DUJopz   <> '4'
017600070518     c                             and  DUJopz   <> '6'
017700070418     c                             and  DUJopz   <> '7'
017800070418     c                             and  DUJopz   <> '8'
017900070418     c                             and  DUJopz   <> '9'
018000070604     c                             and  DUJopz   <> 'd'
018100070423     c                             and  DUJopz   <> 'D'
018200070910     c                             and  DUJopz   <> 'S'
018300070417     c                   eval      DUJerr    = *on
018400070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
018500070412     c                   eval      $Fine     = *on
018600070412     c                   leavesr
018700070418w   1c                   when           DUJopz    = '1'
018800070418     c                             and  DUJnum   <> *zeros
018900070417     c                   eval      DUJerr    = *on
019000070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
019100070412     c                   eval      $Fine     = *on
019200070412     c                   leavesr
019300070418w   1c                   when           DUJopz   <> '1'
019400070418     c                             and  DUJnum    = *zeros
019500070417     c                   eval      DUJerr    = *on
019600070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
019700070412     c                   eval      $Fine     = *on
019800070412     c                   leavesr
019900070412e   1c                   endsl
020000070411      *
020100070508      * REPERIMENTO REC. TRK "1" DAL FILE WFAIM01L
020200070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
020300070417      * - se NON richiesta un'immissione - DUJopz = '1'
020400070417if  1c                   if        DUJopz    = '1'
020500070411     c                   leavesr
020600070411e   1c                   endif
020700070411      *
020800070508     c                   eval      WIMnum    = DUJnum
020900070518     c                   if        DUJopz   <> '6'
021000070508     c                   eval      WIMtrk    = '1'
021100070518     c                   else
021200070518     c                   eval      WIMtrk    = '3'
021300070518     c                   endif
021400070523if  1c                   if        DUJopz   <> '0'
021500070508     c     K02WIM01      CHAIN     WFAIM000
021600070523x   1c                   else
021700070523     c     K02WIM01      chain(n)  WFAIM000
021800070523e   1c                   endif
021900070518if  1c                   if        NOT  %found(WFAIM01L)
022000070521     c                             and (DUJopz = '0'
022100070521     c                              or  DUJopz = '4')
022200070518     c                   eval      WIMtrk    = '3'
022300070518     c     K02WIM01      chain(n)  WFAIM000
022400070518e   1c                   endif
022500070508if  1c                   if        %found(WFAIM01L)
022600070508     c                   movel     WIMuni        dWIMuni1
022700070411e   1c                   endif
022800070411      *
022900070412      * CONTROLLO COMPATIBILITÀ OPZ. RICHIESTA CON STATUS QUO DEL REC.
023000070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
023100070411sel 1c                   SELECT
023200070411      *
023300070412      * - Record inesistente -> inserim. multiplo cancellato/errato
023400070508w   1c                   WHEN      NOT  %found(WFAIM01L)
023500070417     c                   eval      DUJerr    = *on
023600070503     c                   eval      DUJmsg    = %replace(
023700070503     c                                           %editc(DUJnum:'L'):
023800070503     c                                           $Msg( 1):
023900070509     c                                           %scan('&IMU ':
024000070503     c                                                 $Msg( 1) ))
024100070411     c                   eval      $Fine     = *on
024200070411     c                   leavesr
024300070412      *
024400070412      * - Richiesto SOLO il reperimento del trk "1": uscita
024500070417w   1c                   WHEN      DUJopz    = '0'
024600070417     c*** già così:      eval      DUJerr    = *off
024700070412     c                   eval      $Fine     = *on
024800070412     c                   leavesr
024900070413      *
025000070413      * - Richiesta manutenzione dallo stesso utente: OK
025100070508w   1c                   WHEN           WIMprg    = *zeros
025200070508     c                             and  §WIM1usp  = KNMUS
025300070508     c                             and  §WIM1uin  = *blanks
025400070418     c                             and  DUJopz    = '2'
025500070518      *
025600070518      * - Richiesta RI-attivazione: OK
025700070518w   1c                   WHEN           WIMprg    = *zeros
025800070518     c                             and  DUJopz    = '6'
025900070411      *
026000070411      * - Record già in uso  -> inserim. multiplo già in manutenzione
026100070518      *   (richiedibile SOLO lo "sblocco" o la cancellazione,
026200070518      *    ma dallo STESSO utente)
026300070508w   1c                   WHEN           WIMprg    = *zeros
026400070418     c                             and  DUJopz   <> '8'
026500070910     c                             and  DUJopz   <> 'S'
026600070518     c                             and  DUJopz   <> '4'
026700070604     c                             and  DUJopz   <> 'd'
026800070423     c                             and  DUJopz   <> 'D'
026900070508     c                             OR   WIMprg    = *zeros
027000070518     c                             and (DUJopz    = '8'
027100070910     c                              or  DUJopz    = 'S'
027200070518     c                              or  DUJopz    = '4'
027300070604     c                              or  DUJopz    = 'd'
027400070518     c                              or  DUJopz    = 'D')
027500070508     c                             and (§WIM1usp <> KNMUS
027600120507     c*//                          and  KNMUS    <> 'EDPSM     '
027700070508     c                             and  §WIM1uin  = *blanks
027800070508     c                              or  §WIM1uin <> *blanks
027900070508     c                             and  §WIM1uin <> KNMUS)
028000070417     c                   eval      DUJerr    = *on
028100070508if  2c                   if        §WIM1uin <> *blanks
028200070503     c                   eval      DUJmsg    = %trimr( %replace(
028300070503     c                                           %editc(DUJnum:'L'):
028400070503     c                                           $Msg( 2):
028500070509     c                                           %scan('&IMU ':
028600070503     c                                                 $Msg( 2) )) )
028700070508     c                                       + ' ' + §WIM1uin
028800070411x   2c                   else
028900070503     c                   eval      DUJmsg    = %trimr( %replace(
029000070503     c                                           %editc(DUJnum:'L'):
029100070503     c                                           $Msg( 2):
029200070509     c                                           %scan('&IMU ':
029300070503     c                                                 $Msg( 2) )) )
029400070508     c                                       + ' ' + §WIM1usp
029500070411e   2c                   endif
029600070411     c                   eval      $Fine     = *on
029700070411     c                   leavesr
029800070411      *
029900070411      * - Record libero      -> inserim. multiplo non in manutenzione
030000070418      *   (NON richiedibile "sblocco")
030100070508w   1c                   WHEN           WIMprg    > *zeros
030200070910     c                             and (DUJopz    = '8'
030300070910     c                              or  DUJopz    = 'D')
030400070417     c                   eval      DUJerr    = *on
030500070503     c                   eval      DUJmsg    = %replace(
030600070503     c                                           %editc(DUJnum:'L'):
030700070503     c                                           $Msg( 3):
030800070509     c                                           %scan('&IMU ':
030900070503     c                                                 $Msg( 3) ))
031000070411     c                   eval      $Fine     = *on
031100070411     c                   leavesr
031200070412      *
031300070411e   1c                   ENDSL
031400070411      *
031500070412      * GESTIONE INIZIALE DELL'OPZIONE RICEVUTA
031600070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
031700070411sel 1c                   SELECT
031800070418      * - 7=Blocco / 8=Sblocco / 9=Blocco x Conferma
031900070418      *   (SENZA visualizzare nulla)
032000070418w   1c                   WHEN           DUJopz    = '7'
032100070418     c                             or   DUJopz    = '8'
032200070418     c                             or   DUJopz    = '9'
032300070508     c                   exsr      xLockWFAIM
032400070910      * - S=Sblocco
032500070910      *   CON visualizzazione window di conferma
032600070910w   1c                   WHEN      DUJopz    = 'S'
032700070910     c                   eval      $Video    = 'W2'
032800070910     c                   eval      $InzW01   = *on
032900070412      * - 4=Cancellazione ("totale": trk "1" e "2")
033000070423      *   · con window di conferma
033100070417w   1c                   WHEN      DUJopz    = '4'
033200070412     c                   eval      $Video    = 'W1'
033300070518     c                   eval      $InzW01   = *on
033400070604      *   · senza window di conferma
033500070604w   1c                   WHEN      DUJopz    = 'd'
033600070604     c                   exsr      Dlt_WFAIM
033700070604      *   · passaggio da trk "1"/"2" a trk "3"/"4"
033800070423w   1c                   WHEN      DUJopz    = 'D'
033900070518     c                   exsr      Igc_WFAIM
034000070412      * - 1=Immissione / 2=Modifica
034100070412x   1c                   OTHER
034200070412     c                   eval      $Video    = 'D1'
034300070412     c                   eval      $inzD01   = *on
034400070411e   1c                   ENDSL
034500070411      *
034600070411     c                   ENDSR
034700070411      *
034800070411      *---------------------------------------------------------------*
034900070411      *?Reperimento Dati del job (Utente/Operativi)                  ?*
035000070411      *---------------------------------------------------------------*
035100070411     c     DatiJob       BEGSR
035200070411      *
035300070411     c     *dtaara       define    §azute        azuteds
035400070411     c     *dtaara       define    §datiute      ddatiute
035500070411      *
035600070411     c                   in(E)     *dtaara
035700070411     c                   IF        %ERROR or RSUT = *blanks
035800070411     c                   clear                   Tibs34Ds
035900070411     c                   call      'TIBS34R'
036000070411     c                   parm                    Tibs34Ds
036100070411     c                   in        *dtaara
036200070411     c                   ENDIF
036300070411      *
036400070411     c                   ENDSR
036500070411      *
036600070411      *---------------------------------------------------------------*
036700070411      *?Gestione videata D01                                         ?*
036800070411      *---------------------------------------------------------------*
036900070411     c     GesD01        BEGSR
037000070411      *
037100070411      * Inizializzazione della videata
037200070411if  1c                   if        $InzD01   = *on
037300070411     c                   exsr      InzD01
037400070411     c                   eval      $InzD01   = *off
037500070411e   1c                   endif
037600070411      *
037700070411      * Scrittura di testata e riga tasti funzionali abilitati
037800070417     c                   write     SDUJT01
037900070417     c                   write     SDUJZ01
038000070411      *
038100070411      * Emissione della videata
038200070417     c                   exfmt     SDUJD01
038300070411     c                   setoff                                       28  90
038400070411     c                   clear                   V1Dmsg
038500070411      *
038600070411sel 1c                   SELECT
038700070411      * F3=Fine
038800070411w   1c                   WHEN      *inKC
038900070411     c                   exsr      F03D01
039000070411      * F12=Ritorno
039100070411w   1c                   WHEN      *inKL
039200070411     c                   exsr      F12D01
039300070411      * Invio / F6=Conferma
039400070411x   1c                   OTHER
039500070411      * - Controllo dati immessi a video
039600070411     c                   exsr      CtrD01
039700070411      * - F6=Conferma
039800070411if  2c                   if        *inKF  and  NOT *in90
039900070411     c                   exsr      F06D01
040000070411e   2c                   endif
040100070411e   1c                   ENDSL
040200070411      *
040300070411     c                   ENDSR
040400070411      *
040500070411      *---------------------------------------------------------------*
040600070411      *?Inizializzazione videata D01                                 ?*
040700070411      *---------------------------------------------------------------*
040800070411     c     InzD01        BEGSR
040900070411      *
041000070417     c                   clear                   SDUJD01
041100070411      *
041200070411      * Impostazione campi già memorizzati (solo se modifica)
041300070518     c                   SELECT
041400070518if  1c                   WHEN      DUJopz    = '1'
041500070411     c                   eval      V1Ctxt    = 'IMMISSIONE '
041600070411     c                                       + 'PARAMETRI PER UN NUOV+
041700070411     c                                          O INSERIMENTO MULTIPL+
041800070411     c                                          O UNITÀ'
041900070518if  1c                   WHEN      DUJopz    = '6'
042000070518     c                   eval      V1Ctxt    = 'RI-ATTIVAZIONE '
042100070518     c                                       + 'DELL''INSERIMENTO MUL+
042200070518     c                                          TIPLPLO UNITÀ N° '
042300070518     c                                       + %trim( %editc(
042400070518     c                                          WIMnum : 'K' ))
042500070518     c                   eval      V1Clm1    = §WIM1lm1
042600070518     c                   eval      V1Cim2    = §WIM1im2
042700070518     c                   eval      V1Clm2    = §WIM1lm2
042800070518     c                   eval      V1Cnmi    = §WIM1nmi
042900070518     c                   eval      V1Cdes    = §WIM1des
043000070518x   1c                   OTHER
043100070411     c                   eval      V1Ctxt    = 'MODIFICA '
043200070411     c                                       + 'PARAMETRI DELL''INSERI+
043300070411     c                                          MENTO MULTIPLO UNITÀ N+
043400070411     c                                          ° '
043500070411     c                                       + %trim( %editc(
043600070508     c                                          WIMnum : 'K' ))
043700070508     c                   eval      V1Clm1    = §WIM1lm1
043800070508     c                   eval      V1Cim2    = §WIM1im2
043900070508     c                   eval      V1Clm2    = §WIM1lm2
044000070508     c                   eval      V1Cnmi    = §WIM1nmi
044100070508     c                   eval      V1Cdes    = §WIM1des
044200070518e   1c                   ENDSL
044300070411      *
044400070411     c                   ENDSR
044500070411      *
044600070411      *---------------------------------------------------------------*
044700070411      *?Controllo dati immessi in videata D01                        ?*
044800070411      *---------------------------------------------------------------*
044900070411     c     CtrD01        BEGSR
045000070411      *
045100070411     c                   movea     *zeros        *in(50)
045200070411      *
045300070411sel 1c                   select
045400070411      * Lunghezza matricola
045500070411w   1c                   when      V1Clm1    = *zeros
045600070411     c                   seton                                        285190
045700070411     c                   eval      V1Dmsg    = $Msg( 4)
045800070418w   1c                   when      V1Clm1    > C_MatLenMax
045900070418     c                   seton                                        285190
046000070418     c                   eval      V1Dmsg    = $Msg( 5)
046100070411w   1c                   when      V1Clm1    < *zeros
046200070411     c                   seton                                        285190
046300070411     c                   eval      V1Dmsg    = $Msg( 5)
046400070411      * Posizione iniziale nel codice inserito
046500070411w   1c                   when      V1Cim2    = *zeros
046600070411     c                   seton                                        285290
046700070411     c                   eval      V1Dmsg    = $Msg( 6)
046800070411w   1c                   when      V1Cim2    < *zeros
046900070411     c                   seton                                        285290
047000070411     c                   eval      V1Dmsg    = $Msg( 7)
047100070411      * Conformità tra lunghezza-matricola e posizione-iniziale
047200070411w   1c                   when      V1Cim2    > V1Clm1
047300070411     c                   seton                                        285290
047400070411     c                   eval      V1Dmsg    = $Msg( 8)
047500070411      * Lunghezza da posizione iniziale nel codice inserito
047600070411w   1c                   when      V1Clm2    = *zeros
047700070411     c                   seton                                        285390
047800070411     c                   eval      V1Dmsg    = $Msg( 9)
047900070411w   1c                   when      V1Clm2    < *zeros
048000070411     c                   seton                                        285390
048100070411     c                   eval      V1Dmsg    = $Msg(10)
048200070508w   1c                   when      V1Clm2    > C_StrLenMax
048300070508     c                   seton                                        285390
048400070508     c                   eval      V1Dmsg    = $Msg(11)
048500070411      * Conformità tra lunghezza-matricola e lunghezza-sottostringa
048600070411     c                   when      V1Clm1    < (V1Cim2 + V1Clm2 - 1)
048700070411     c                   seton                                        285390
048800070508     c                   eval      V1Dmsg    = $Msg(12)
048900070411      * Numero totale di matricole da inserire obbligatorio
049000070411w   1c                   when      V1Cnmi    = *zeros
049100070411     c                   seton                                        285490
049200070508     c                   eval      V1Dmsg    = $Msg(13)
049300070411w   1c                   when      V1Cnmi    < *zeros
049400070411     c                   seton                                        285490
049500070508     c                   eval      V1Dmsg    = $Msg(14)
049600070411      * Descrizione obbligatoria
049700070411w   1c                   when      V1Cdes    = *blanks
049800070411     c                   seton                                        285590
049900070508     c                   eval      V1Dmsg    = $Msg(15)
050000070411e   1c                   endsl
050100070411      *
050200070411     c                   ENDSR
050300070411      *
050400070411      *---------------------------------------------------------------*
050500070411      *?Gestione tasto funzionale F03 da videata D01                 ?*
050600070411      *---------------------------------------------------------------*
050700070411     c     F03D01        BEGSR
050800070411      *
050900070411      * Uscita dal pgm
051000070417     c                   eval      DUJret    = '1'
051100070411     c                   eval      $Fine     = *on
051200070411      *
051300070411     c                   ENDSR
051400070411      *
051500070411      *---------------------------------------------------------------*
051600070411      *?Gestione tasto funzionale F06 da videata D01                 ?*
051700070411      *---------------------------------------------------------------*
051800070411     c     F06D01        BEGSR
051900070411      *
052000070411      * Aggiornamento numeratore (se immissione)
052100070417if  1c                   IF        DUJopz    = '1'
052200070508     c                   exsr      RepNumFree
052300070411if  2c                   if        *in90
052400070411     c                   leavesr
052500070411e   2c                   endif
052600070411e   1c                   ENDIF
052700070411      *
052800070508      * Aggiornamento trk "1" nel file WFAIM00F
052900070508     c                   exsr      RepRecWFAIM
053000070518if  1c                   if        DUJopz   <> '6'
053100070508     c                   exsr      xLockWFAIM
053200070518x   1c                   else
053300070518     c                   exsr      Inc_WFAIM
053400070518e   1c                   endif
053500070411      *
053600070411     c                   ENDSR
053700070411      *
053800070411      *---------------------------------------------------------------*
053900070411      *?Gestione tasto funzionale F12 da videata D01                 ?*
054000070411      *---------------------------------------------------------------*
054100070411     c     F12D01        BEGSR
054200070411      *
054300070411      * Uscita dal pgm
054400070417     c                   eval      DUJret    = '2'
054500070411     c                   eval      $Fine     = *on
054600070411      *
054700070411     c                   ENDSR
054800070412      *
054900070412      *---------------------------------------------------------------*
055000070412      *?Gestione videata W01                                         ?*
055100070412      *---------------------------------------------------------------*
055200070412     c     GesW01        BEGSR
055300070412      *
055400070412      * Inizializzazione della videata
055500070412if  1c                   if        $InzW01   = *on
055600070417     c                   clear                   SDUJW01
055700070417     c                   eval      W1Cnum    = DUJnum
055800070412     c                   eval      $InzW01   = *off
055900070412e   1c                   endif
056000070412      *
056100070412      * Emissione della window di conferma
056200070417     c                   exfmt     SDUJW01
056300070412      *
056400070412sel 1c                   SELECT
056500070412      * F12 = Ritorno
056600070412w   1c                   WHEN      *inKL
056700070412     c                   exsr      F12D01
056800070412      * F6 = Conferma
056900070412w   1c                   WHEN      *inKF
057000070508     c                   exsr      Dlt_WFAIM
057100070412e   1c                   ENDSL
057200070412      *
057300070412     c                   ENDSR
057400070910      *
057500070910      *---------------------------------------------------------------*
057600070910      *?Gestione videata W02                                         ?*
057700070910      *---------------------------------------------------------------*
057800070910     c     GesW02        BEGSR
057900070910      *
058000070910      * Inizializzazione della videata
058100070910if  1c                   if        $InzW01   = *on
058200070910     c                   clear                   SDUJW02
058300070910     c                   eval      W1Cnum    = DUJnum
058400070910     c                   eval      $InzW01   = *off
058500070910e   1c                   endif
058600070910      *
058700070910      * Emissione della window di conferma
058800070910     c                   exfmt     SDUJW02
058900070910      *
059000070910sel 1c                   SELECT
059100070910      * F12 = Ritorno
059200070910w   1c                   WHEN      *inKL
059300070910     c                   exsr      F12D01
059400070910      * F6 = Conferma
059500070910w   1c                   WHEN      *inKF
059600070910     c                   exsr      xLockWFAIM
059700070910e   1c                   ENDSL
059800070910      *
059900070910     c                   ENDSR
060000070411      *
060100070411      *---------------------------------------------------------------*
060200070508      *?Reperimento nuovo numero progressivo                         ?*
060300070411      *---------------------------------------------------------------*
060400070508     c     RepNumFree    BEGSR
060500070411      *
060600070508     c     *hival        setgt     WFAIM000
060700070508     c                   readp(n)  WFAIM000
060800070411      *
060900070508if  1c                   if        %eof(WFAIM01L)
061000070508     c                   eval      DUJnum    = 1
061100070508x   1c                   else
061200070508     c                   eval      DUJnum    = WIMnum + 1
061300070411e   1c                   endif
061400070411      *
061500070411     c                   ENDSR
061600070412      *
061700070412      *---------------------------------------------------------------*
061800070508      *?Reperimento record trk "1" del file WFAIM00F                 ?*
061900070412      *---------------------------------------------------------------*
062000070508     c     RepRecWFAIM   BEGSR
062100070412      *
062200070412      * Reperimento record per aggiornamento
062300070508      * (anche se inserimento: nuovo numero DUJNUM già reperito)
062400070508     c                   eval      WIMnum    = DUJnum
062500070518     c                   if        DUJopz   <> '6'
062600070508     c                   eval      WIMtrk    = '1'
062700070518     c                   else
062800070518     c                   eval      WIMtrk    = '3'
062900070518     c                   endif
063000070508     c     K02WIM01      chain     WFAIM000
063100070412      *
063200070412      * Impostazione nuovi dati
063300070412      *
063400070508if  1c                   IF        NOT  %found(WFAIM01L)
063500070508     c                   clear                   dWIMuni1
063600070508     c                   clear                   WFAIM000
063700070508     c                   z-add     DUJnum        WIMnum
063800070508     c                   eval      WIMtrk    = '1'
063900070412x   1c                   ELSE
064000070508     c                   movel     WIMuni        dWIMuni1
064100070412e   1c                   ENDIF
064200070412      *
064300070508     c                   eval      §WIM1usp  = KNMUS
064400070508     c                   eval      §WIM1des  = V1Cdes
064500070508     c                   eval      §WIM1lm1  = V1Clm1
064600070508     c                   eval      §WIM1im2  = V1Cim2
064700070508     c                   eval      §WIM1lm2  = V1Clm2
064800070508     c                   eval      §WIM1nmi  = V1Cnmi
064900070508     c                   movel     dWIMuni1      WIMuni
065000070412      *
065100070412     c                   ENDSR
065200070411      *
065300070411      *---------------------------------------------------------------*
065400070508      *?Aggiornamento record trk "1" del file WFAIM00F               ?*
065500070411      *---------------------------------------------------------------*
065600070508     c     xLockWFAIM    BEGSR
065700070412      *
065800070418sel 1c                   select
065900070418      * Richiesta Disallocazione
066000070910w   1c                   when           DUJopz    = '8'
066100070910     c                             or   DUJopz    = 'S'
066200070508     c                   eval      WIMprg    = 1
066300070508     c                   eval      §WIM1uin  = *blanks
066400070508     c                   movel     dWIMuni1      WIMuni
066500070418      * Richiesta Allocazione x Conferma (inserimento in UNANA00F)
066600070418w   1c                   when      DUJopz    = '9'
066700070508     c                   eval      WIMprg    = *zeros
066800070508     c                   eval      §WIM1uin  = KNMUS
066900070508     c                   movel     dWIMuni1      WIMuni
067000070418      * Richiesta Allocazione
067100070418x   1c                   other
067200070508     c                   eval      WIMprg    = *zeros
067300070531     c                   eval      §WIM1usp  = KNMUS
067400070531     c                   movel     dWIMuni1      WIMuni
067500070418     c                   endsl
067600070411      *
067700070411      * Scrittura/Aggiornamento record
067800070508if  1c                   if        %found(WFAIM01L)
067900070411      *                  __________________
068000070508     c                   UPDATE    WFAIM000
068100070411      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
068200070411x   1c                   else
068300070411      *                  __________________
068400070508     c                   WRITE     WFAIM000
068500070411      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
068600070411e   1c                   endif
068700070412      *
068800070412      * Uscita dal pgm
068900070417     c                   eval      DUJret    = *off
069000070412     c                   eval      $Fine     = *on
069100070411      *
069200070411     c                   ENDSR
069300070517      *
069400070517      *---------------------------------------------------------------*
069500070517      *?Cancellazione totale di un "inserimento multiplo unità"      ?*
069600070517      *---------------------------------------------------------------*
069700070517     c     Dlt_WFAIM     BEGSR
069800070517      *
069900070517     c     DUJnum        setll     WFAIM000
070000070517     c     DUJnum        reade     WFAIM000
070100070517      *
070200070517do  1c                   DOW       NOT  %eof(WFAIM01L)
070300070517      *                  __________________
070400070517     c                   DELETE    WFAIM000
070500070517      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
070600070517     c     DUJnum        reade     WFAIM000
070700070517e   1c                   ENDDO
070800070517      *
070900070517      * Uscita dal pgm
071000070517     c                   eval      DUJret    = *off
071100070517     c                   eval      $Fine     = *on
071200070517      *
071300070517     c                   ENDSR
071400070411      *
071500070411      *---------------------------------------------------------------*
071600070518      *?Impostazione "inserimento multiplo unità" come Già Confermato?*
071700070411      *---------------------------------------------------------------*
071800070518     c     Igc_WFAIM     BEGSR
071900070411      *
072000070517      * Elaborazione testata   (trk "1")
072100070508     c                   eval      WIMnum    = DUJnum
072200070508     c                   eval      WIMtrk    = '1'
072300070508     c     K02WIM01      chain     WFAIM000
072400070508if  1c                   if        %found(WFAIM01L)
072500070517      *
072600070517     c                   eval      WIMtrk    = '3'
072700070411      *                  __________________
072800070517     c                   UPDATE    WFAIM000
072900070411      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
073000070411e   1c                   endif
073100070411      *
073200070517      * Elaborazione dettaglio (trk "2") esistente
073300070508     c                   eval      WIMnum    = DUJnum
073400070518     c                   eval      W_trk     = '2'
073500070518     c     K02WIM01w     setll     WFAIM000
073600070518     c     K02WIM01w     reade     WFAIM000
073700070508do  1c                   DOW       NOT  %eof(WFAIM01L)
073800070517      *
073900070517     c                   eval      WIMtrk    = '4'
074000070411      *                  __________________
074100070517     c                   UPDATE    WFAIM000
074200070411      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
074300070518     c     K02WIM01w     reade     WFAIM000
074400070411e   1c                   ENDDO
074500070412      *
074600070412      * Uscita dal pgm
074700070417     c                   eval      DUJret    = *off
074800070412     c                   eval      $Fine     = *on
074900070411      *
075000070411     c                   ENDSR
075100070518      *
075200070518      *---------------------------------------------------------------*
075300070518      *?Impostazione "inserimento multiplo unità" come Non Confermato?*
075400070518      *---------------------------------------------------------------*
075500070518     c     Inc_WFAIM     BEGSR
075600070518      *
075700070518      * Elaborazione testata   (trk "3")
075800070518     c                   eval      WIMnum    = DUJnum
075900070518     c                   eval      WIMtrk    = '3'
076000070518     c     K02WIM01      chain     WFAIM000
076100070518if  1c                   if        %found(WFAIM01L)
076200070518      *
076300070518     c                   eval      WIMtrk    = '1'
076400070518     c                   eval      WIMprg    = 1
076500070518      *                  __________________
076600070518     c                   UPDATE    WFAIM000
076700070518      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
076800070518e   1c                   endif
076900070518      *
077000070518      * Elaborazione dettaglio (trk "4") esistente
077100070518     c                   eval      WIMnum    = DUJnum
077200070518     c                   eval      W_trk     = '4'
077300070518     c     K02WIM01w     setll     WFAIM000
077400070518     c     K02WIM01w     reade     WFAIM000
077500070518do  1c                   DOW       NOT  %eof(WFAIM01L)
077600070518      *
077700070518     c                   eval      WIMtrk    = '2'
077800070518      *                  __________________
077900070518     c                   UPDATE    WFAIM000
078000070518      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
078100070518     c     K02WIM01w     reade     WFAIM000
078200070518e   1c                   ENDDO
078300070518      *
078400070518      * Uscita dal pgm
078500070518     c                   eval      DUJret    = *off
078600070518     c                   eval      $Fine     = *on
078700070518      *
078800070518     c                   ENDSR
078900070411
079000070411** - $MSG -------------------------------------------------------------------*
079100070509"Inserimento multiplo unità" &IMU  non trovato nell'archivio                    1
079200070509"Inserimento multiplo unità" &IMU  in uso dall'utente                           2
079300070509"Inserimento multiplo unità" &IMU  NON più "bloccato"                           3
079400070411Lunghezza matricola obbligatoria                                                4
079500070508Lunghezza matricola errata (min 1 - max 50)                                     5
079600070411Posizione iniziale nel codice inserito obbligatoria                             6
079700070411Posizione iniziale nel codice inserito errata                                   7
079800070411Posizione iniziale nel codice OLTRE la lunghezza della matricola                8
079900070411Lunghezza da posizione iniziale nel codice inserito obbligatoria                9
080000070411Lunghezza da posizione iniziale nel codice inserito errata                     10
080100070508Lunghezza della sottostringa da considerare errata (min 1 - max 15)            11
080200070508Lunghezza della sottostringa OLTRE la lunghezza della matricola                12
080300070508Numero totale di matricole da inserire obbligatorio                            13
080400070508Numero totale di matricole da inserire errato                                  14
080500070508Descrizione obbligatoria                                                       15
