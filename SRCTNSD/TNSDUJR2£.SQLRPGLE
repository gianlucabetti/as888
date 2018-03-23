000100070412      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE)
000200070412      *===============================================================*
000300070509      *?TNSDUJR2 - Inserimento multiplo unità EDP - trk "2" pf WFAIM ?*
000400070412      *===============================================================*
000500070605      *A V V E R T E N Z E :?                                       ?*
000600070605      * Occorre? ricompilare ANCHE ?il pgm.?TNSDUJR3? se ?si modifica *
000700070605      * il dspf TNSDUJD2.                                             *
000800070605      *===============================================================*
000900070503      *?N O T E :                                                    ?*
001000070503      * Valori ammessi per li campo DUJOPZ?ricevuto?:                 *
001100070606      * · 'A'   -> immissione nuovo inserimento multiplo unità        *
001200070606      *            con il subfile già caricato                        *
001300070606      *            (richiamato da pgm. TNSDUJR3)                      *
001400070503      * · '1'   -> immissione nuovo inserimento multiplo unità        *
001500070503      *            (richiamato da pgm. TNSDUJR)                       *
001600070503      * · '4'   -> cancellazione singolo record trk. "2"              *
001700070517      *            => imposta trk. "4" nel record (appena elaborato)  *
001800070503      *            (richiamato da pgm. TNSDUKR)                       *
001900070518      * · '5'   -> visualizzazione  inserimento multiplo unità        *
002000070518      *            (richiamato da pgm. TNSDUJR)                       *
002100070503      * · altro -> manutenzione     inserimento multiplo unità        *
002200070503      *            (richiamato da pgm. TNSDUJR)                       *
002300070503      * Valori gestiti per li campo DUJOPZ?passato?a TNSDUJR1:        *
002400070503      * · '0'   -> reperimento  record trk. "1"                       *
002500070503      * · '2'   -> manutenzione record trk. "1"                       *
002600070503      *===============================================================*
002700070412
002800070412     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002900070412
003000070412      *---------------------------------------------------------------*
003100070412
003200070509     fWFAIM01L  Uf A e           k disk
003300070412      *
003400070417     fTNSDUJD2  cf   e             workstn
003500070417     f                                     sfile(SDUJS01:S01nrr)
003600070412     f                                     infds(DSFMT)
003700070412
003800070412      *---------------------------------------------------------------*
003900070412
004000070412      *
004100070412      *?Costanti  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
004200070412      *
004300070412      * - Tasti di funzione
004400070412     d C_RollUp        c                   const(x'F5')
004500070412      * - Nr. di righe del sfl (SFLPAG)
004600070412     d C_SflPag        c                   const(16)
004700070412      *
004800070412      *?Schiere - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
004900070412      *
005000070412      * - Messaggi di errore
005100081031     d $Msg            s             78    dim(16)   ctdata  perrcd(1)
005200070412      * - Indicatori per abilitazione dei tasti funzionali (F1-F24)
005300070412     d $Fxx            s              1    dim(24)   inz(*off)
005400070412      * - Codici matricola già inseriti nel subfile
005500070412     d $Matr           s                   dim(9999) inz  like(S1Cmat)
005600070412      * - Flags numerici matricola "annullata" nel subfile (per NRR)
005700070412     d $MAnn           s              1  0 dim(9999) inz
005800070412      *
005900070412      *?Ds  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
006000070412      *
006100070412      * - Parametri
006200070412     d KPJBA         e ds
006300070417     d TNSDUJds      e ds                  inz
006400070412      *
006500070412      * - Parametri x Controllo profilo utenti
006600070606     d TIBS34ds      e ds                  inz
006700070412      * - Ds di riferimento al file esterno AZUTE00F
006800070412     d AZUTEds       e ds                  extname(AZUTE00F)
006900070412      * - Ds per dati organigramma
007000070606     d dDatiUte      e ds
007100070412      *
007200070509      * DS campo WIMUNI file WFAIM00F trk "1"
007300070509     d dWIMuni1      e ds                  inz
007400070412      *
007500070509      * DS file WFAIM00F per SQL
007600070509     d WFAIMds       e ds                  inz  extname(WFAIM00F)
007700070412     d                                     prefix(SQL:3)
007800070412      *
007900070412     d Status         sds           333
008000070412     d   SDSpgm          *proc
008100070412      *
008200070412     d dsFMT           ds           512
008300070412     d  £Tasto               369    369
008400070412      *
008500070412      *?Variabili - - - - - - - - - - - - - - - - - - - - - - - - - -?*
008600070412      *
008700070412      * - Flags
008800070412     d $EoF            s              1    inz(*off)
008900070412     d $Fine           s              1    inz(*off)
009000070412     d $InzS01         s              1    inz(*on)
009100070412     d $InzW01         s              1    inz(*on)
009200070412     d $Found          s              1    inz(*off)
009300070412     d $Err            s              1    inz(*off)
009400070412      *
009500070412     d $Video          s              2    inz('S1')
009600070412      *
009700070412      * - Indici di schiera
009800070412     d yy              s              3  0 inz
009900070412      *
010000070412      * - Variabili riferite al data base o al display file
010100070412     d S01nrr          s                   like(C01rcd)   inz
010200070412     d MAXnrr          s                   like(C01rcd)   inz
010300070412     d wMatr           s                   like(S1Cmat)   inz
010400070412     d wMatr§          s                   like(S1Cmat)   inz
010500070412     d wVideo          s                   like($Video)   inz
010600070509     d WWWprg          s                   like(WIMprg)   inz
010700070620     d WrecTrk2        s                   like(WIMprg)   inz
010800070521     d WrecTrk4        s                   like(WIMprg)   inz
010900070417     d SaveU0ds        s                   like(TNSDUJds) inz
011000070412      *
011100070412      * - Variabili definite a programma
011200070412     d wPag            s              4  0 inz
011300070412     d wRig            s              3  0 inz
011400070412     d wErr            s              3  0 inz
011500070518     d*W_SflPag***     s              3  0 inz
011600070412      *
011700070412      *?Key-List  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
011800070412      *
011900070509      * - WFAIM01L
012000070509     c     K02WIM01      klist
012100070509     c                   kfld                    WIMnum
012200070509     c                   kfld                    WIMtrk
012300070509     c     K03WIM01      klist
012400070509     c                   kfld                    WIMnum
012500070509     c                   kfld                    WIMtrk
012600070509     c                   kfld                    WIMprg
012700070412
012800070412      *---------------------------------------------------------------*
012900070412      *?RIEPILOGO INDICATORI                                         ?*
013000070412      *---------------------------------------------------------------*
013100070412      * 25    - Comodo                                                *
013200070416      * 28    - Emissione del messaggio di errore a video             *
013300070416      * 30    - Pulizia      del subfile S01 (sflclr, NO sfldsp)      *
013400070416      * 31    - NO emissione del subfile S01 (sflclr, NO sfldspctl)   *
013500070412      * 32    - Record modificato nel subfile S01 (sflnxtchg)         *
013600070416      * 33    - Fine dati         nel subfile S01 (sflend)            *
013700070412      * 41    - S01: Evidenziazione S1CMAT in (RI)                    *
013800070518      * 45    - S01: Protezione campi I/O  per sola visualizzazione   *
013900081031      * 46    - C01: Protezione campi I/O  per immissione NON manuale *
014000070412      * 50    - S01: Posizionamento cursore sul campo "Opzione"       *
014100070412      * 90    - Generico di errore                                    *
014200070412      *---------------------------------------------------------------*
014300070412
014400070412     c     *Entry        plist
014500070412     c                   parm                    KPJBA
014600070412      *
014700070412      * Operazioni Iniziali
014800070412     c                   exsr      RoutInz
014900070412      *
015000070412      * Gestione Video
015100070412do  1c                   DOW       $Fine     = *off
015200070412cas 2c     $Video        caseq     'S1'          GesS01
015300070412cas 2c     $Video        caseq     'W1'          GesW01
015400070412e   2c                   endcs
015500070412e   1c                   ENDDO
015600070412      *
015700070412      * Fine
015800070509     c                   movel     dWIMuni1      DUJuni
015900070417     c                   movel(p)  TNSDUJds      KPJBU
016000070412     c                   eval      *inLR     = *on
016100070412      *
016200070412      *---------------------------------------------------------------*
016300070412      *?Operazioni Iniziali                                          ?*
016400070412      *---------------------------------------------------------------*
016500070412     c     RoutInz       BEGSR
016600070412      *
016700070412     c/exec sql
016800070412     c+     set  option  DYNUSRPRF = *OWNER,
016900070412     c+                  CLOSQLCSR = *ENDMOD
017000070412     c/end-exec
017100070412      *
017200070412      * Reperimento dati job
017300070412     c                   exsr      DatiJob
017400070412      *
017500070412     c                   reset                   $Fine
017600070412     c                   reset                   $InzS01
017700070412     c                   reset                   $InzW01
017800070412      *
017900070412      * Impostazione iniziale dei parametri i/o
018000070417     c                   movel     KPJBU         TNSDUJds
018100070417     c                   eval      DUJret    = *off
018200070417     c                   eval      DUJerr    = *off
018300070417     c                   eval      DUJmsg    = *blanks
018400070509      *
018500070509      * "Intestazione" del subfile-control
018600070509     c                   movel     SDSpgm        V1Tpgm
018700070518sel 1c                   select
018800070606w   1c                   when           DUJopz  = '1'
018900070606     c                             or   DUJopz  = 'A'
019000070509     c                   eval      V1Ttxt    = '     '
019100070509     c                                       + 'I N S E R I M E N T O'
019200070518w   1c                   when      DUJopz    = '5'
019300070518     c                   eval      V1Ttxt    = ' '
019400070518     c                                       + 'V I S U A L I Z Z A +
019500070518     c                                          Z I O N E'
019600070518x   1c                   other
019700070509     c                   eval      V1Ttxt    = '        '
019800070509     c                                       + 'M O D I F I C A'
019900070518e   1c                   endsl
020000070412      *
020100070412      * CONTROLLO PARAMETRI RICEVUTI
020200070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
020300070412sel 1c                   select
020400070503w   1c                   when           DUJopz   <> '0'
020500070503     c                             and  DUJopz   <> '1'
020600070503     c                             and  DUJopz   <> '2'
020700070503     c                             and  DUJopz   <> '4'
020800070518     c                             and  DUJopz   <> '5'
020900070503     c                             and  DUJopz   <> '7'
021000070503     c                             and  DUJopz   <> '8'
021100070503     c                             and  DUJopz   <> '9'
021200070606     c                             and  DUJopz   <> 'A'
021300070503     c                             and  DUJopz   <> 'D'
021400070417     c                   eval      DUJerr    = *on
021500070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
021600070412     c                   eval      $Fine     = *on
021700070412     c                   leavesr
021800070417w   1c                   when      DUJnum    = *zeros
021900070417     c                   eval      DUJerr    = *on
022000070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
022100070412     c                   eval      $Fine     = *on
022200070412     c                   leavesr
022300070423w   1c                   when           DUJopz    = '4'
022400070423     c                             and (DUJnum   <= *zeros
022500070423     c                              or  DUJprg   <= *zeros)
022600070423     c                   eval      DUJerr    = *on
022700070423     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
022800070423     c                   eval      $Fine     = *on
022900070423     c                   leavesr
023000070412e   1c                   endsl
023100070412      *
023200070509      * Reperimento dati del trk "1" del file WFAIM01L
023300070412      *   (SE NON ricevuto come parametro)
023400070412     c                   exsr      Rep_Trk_1
023500070523if  1c***                if             DUJerr   <> *off
023600070523     c***                          or   DUJret   <> *off
023700070523if  1c                   if            (*in28     = *on
023800070523     c                             or   V1Dmsg   <> *blanks)
023900070523     c                             and  DUJopz    = '5'
024000070523     c                   eval      DUJerr    = *on
024100070523     c                   eval      DUJmsg    = V1Dmsg
024200070523     c                   eval      $Fine     = *on
024300070423     c                   leavesr
024400070423e   1c                   endif
024500070606      *
024600070606      * GESTIONE OPZIONI RICEVUTE
024700070606      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
024800070606sel 1c                   SELECT
024900070606      * - 4=Cancellazione singolo record trk. "2"
025000070606      *   (SENZA visualizzare nulla)
025100070606w   1c                   WHEN      DUJopz    = '4'
025200070606     c                   exsr      Dlt_WFAIM_2
025300070606     c                   eval      $Fine     = *on
025400070606     c                   leavesr
025500070606      * - A=Immissione nuovo inserimento multiplo unità
025600070606      *     da sfl già caricato
025700070606w   1c                   WHEN      DUJopz    = 'A'
025800070606     c                   eval      $Video    = 'S1'
025900070606     c                   eval      $InzS01   = *off
026000070607     c                   eval      MAXnrr    = DUJprg
026100070607     c                   eval      C1Cprg    = DUJprg
026200070606     c                   exsr      Upd_Txt_C01
026300070606     c                   exsr      CtrS01
026400070607if  2c                   if        NOT  *in90
026500070607     c                   eval      C1Cprg    = MAXnrr
026600070606     c                   exsr      F06S01
026700070606e   2c                   endif
026800070606e   1c                   ENDSL
026900070412      *
027000070412     c                   ENDSR
027100070412      *
027200070412      *---------------------------------------------------------------*
027300070412      *?Reperimento Dati del job (Utente/Operativi)                  ?*
027400070412      *---------------------------------------------------------------*
027500070412     c     DatiJob       BEGSR
027600070412      *
027700070412     c     *dtaara       define    §azute        azuteds
027800070412     c     *dtaara       define    §datiute      ddatiute
027900070412      *
028000070412     c                   in(E)     *dtaara
028100070412     c                   IF        %ERROR or RSUT = *blanks
028200070412     c                   clear                   Tibs34Ds
028300070412     c                   call      'TIBS34R'
028400070412     c                   parm                    Tibs34Ds
028500070412     c                   in        *dtaara
028600070412     c                   ENDIF
028700070412      *
028800070412     c                   ENDSR
028900070412      *
029000070412      *---------------------------------------------------------------*
029100070509      *?Reperimento Dati dal trk "1" del file WFAIM00F               ?*
029200070412      *---------------------------------------------------------------*
029300070412     c     Rep_Trk_1     BEGSR
029400070412      *
029500070509     c                   clear                   dWIMuni1
029600070412      *
029700070412      * Se ricevuto come parametro (come dovrebbe essere): basta quello
029800070417if  1c                   if        %subst(DUJuni:1:40) <> *blanks
029900070509     c                   movel     DUJuni        dWIMuni1
030000070412     c                   leavesr
030100070412e   1c                   endif
030200070412      *
030300070509      * Altrimenti: si reperiscono i dati dal trk. "1" del file WFAIM
030400070417      *             (richiamando il pgm. TNSDUJR1)
030500070417     c                   movel     TNSDUJds      SaveU0ds
030600070417     c                   eval      DUJopz    = '0'
030700070417     c                   exsr      Call_TNSDUJ1
030800070417     c                   movel     SaveU0ds      TNSDUJds
030900070518     c                   movel     dWIMuni1      DUJuni
031000070412      *
031100070412     c                   ENDSR
031200070412      *
031300070412      *---------------------------------------------------------------*
031400070412      *?Gestione videata S01                                         ?*
031500070412      *---------------------------------------------------------------*
031600070412     c     GesS01        BEGSR
031700070412      *
031800070412      * Inizializzazione della videata
031900070412if  1c                   if        $InzS01   = *on
032000070412     c                   exsr      InzS01
032100070412     c                   eval      $InzS01   = *off
032200070412e   1c                   endif
032300070412      *
032400070412      * Posizionamento del cursore
032500070412      * => NON nel sfl (se non ci sono errori) ma nel sflctl
032600070412      *
032700070412      * Scrittura di testata e riga tasti funzionali abilitati
032800070417     c                   write     SDUJT01
032900070417     c                   write     SDUJZ01
033000070412      *
033100070412      * Visualizzazione (eventuale) del msg per la mancanza di dati
033200070412if  1c                   if        C01rcd   <= *zeros
033300070417     c                   write     SDUJD00
033400070412e   1c                   endif
033500070412      *
033600070412      * Emissione della videata
033700070417     c                   exfmt     SDUJC01
033800070412if  1c                   if        C01csr    > *zeros
033900070412     c                   eval      C01rcd    = C01csr
034000070412e   1c                   endif
034100070412     c                   setoff                                       28  90
034200070412     c                   clear                   V1Dmsg
034300070412      *
034400070412sel 1c                   SELECT
034500070412      * F3=Fine
034600070412w   1c                   WHEN      *inKC
034700070412     c                   exsr      F03S01
034800070417      * F8=Parametri di controllo (TNSDUJR1)
034900070412w   1c                   WHEN      *inKH
035000070518     c                             and  DUJopz <> '5'
035100070412     c                   exsr      F08S01
035200081031      * F10=Gestione Prefisso/Suffisso Matricola
035300081031w   1c                   WHEN      *inKJ
035400081031     c                   eval      *in46     = *on
035500070412      * F12=Ritorno
035600070412w   1c                   WHEN      *inKL
035700070412     c                   exsr      F12S01
035800070518      * Richiesta SOLA visualizzazione
035900070620w   1c                   WHEN      DUJopz    = '5'
036000070412      * Invio / F6=Conferma
036100070412x   1c                   OTHER
036200081031      * - Controllo Prefisso/Suffisso della Matricola
036300081103     c                   eval      *in51     = *off
036400081103     c                   eval      *in52     = *off
036500081031sel 2c                   select
036600081031w   2c                   when      C1Cpsf    = *blanks
036700081031     c                   clear                   C1Cpsc
036800081031w   2c                   when            C1Cpsf   <> 'P'
036900081031     c                             and   C1Cpsf   <> 'S'
037000081031     c                   seton                                        285190
037100081031     c                   eval      V1Dmsg    = $Msg(15)
037200081031     c                   leavesr
037300081031w   2c                   when      C1Cpsc    = *blanks
037400081031     c                   seton                                        285290
037500081031     c                   if        C1Cpsf    = 'P'
037600081103     c                   eval      V1Dmsg    = %trimr( $Msg(16) )
037700081031     c                                       + ' prefisso'
037800081031     c                   else
037900081103     c                   eval      V1Dmsg    = %trimr( $Msg(16) )
038000081031     c                                       + ' suffisso'
038100081031     c                   endif
038200081031     c                   leavesr
038300081031e   2c                   endsl
038400081103     c                   eval      *in46     = *off
038500070412      * - Gestione opzioni inserite
038600070518if  2c                   if        S01nrr    > *zeros
038700070412     c                   exsr      OpzS01
038800070412e   2c                   endif
038900070412      * - Controllo dati inseriti nel subfile-control
039000070412      *   (solo per evidenziarne eventuali anomalie)
039100070412if  2c                   IF        C1Cmat   <> *blanks
039200070412     c                   exsr      CtrC01
039300070412      * - Inserimento record nel subfile
039400070412if  3c                   if        wErr      = *zeros
039500070412     c                   exsr      WrtS01
039600070412e   3c                   endif
039700070412     c                   clear                   C1Cmat
039800070412e   2c                   ENDIF
039900070412      * - Riconteggio delle matricole confermabili inserite
040000070412     c                   eval      C1Cprg    = MAXnrr - %xfoot($MAnn)
040100070412      * - F6=Conferma
040200070412if  2c                   If        *inKF  and  NOT *in90
040300070412     c                   exsr      CtrS01
040400070412if  3c                   if        *in90
040500070412     c                   leavesr
040600070412e   3c                   endif
040700070412     c                   exsr      F06S01
040800070412e   2c                   EndIf
040900070412e   1c                   ENDSL
041000070412      *
041100070412     c                   ENDSR
041200070412      *
041300070412      *---------------------------------------------------------------*
041400070412      *?Inizializzazione videata S01                                 ?*
041500070412      *---------------------------------------------------------------*
041600070412     c     InzS01        BEGSR
041700070412      *
041800070412      * "Intestazione" del subfile-control
041900070509     c                   clear                   SDUJC01
042000070509     c                   exsr      Upd_Txt_C01
042100070412      *
042200070412      * Pulizia subfile
042300070412     c                   seton                                        3031
042400070417     c                   write     SDUJC01
042500070412     c                   setoff                                         3133
042600070412      *
042700070412     c                   reset                   $EoF
042800070518     c***                clear                   W_SflPag
042900070412     c***                clear                   C01csr
043000070412     c                   clear                   C01rcd
043100070412     c                   clear                   S01nrr
043200070412     c                   clear                   V1Dmsg
043300070412     c                   setoff                                       28  90
043400081031     c                   eval      *in46     = *off
043500070412     c                   movea     *zeros        *in(50)
043600070412      *
043700070412      * Caricamento eventuali dati da presentare nel sfl
043800070412     c                   clear                   $MAnn
043900070412     c                   clear                   $Matr
044000070412     c                   clear                   MAXnrr
044100070620     c                   clear                   WrecTrk2
044200070620     c                   clear                   WrecTrk4
044300070509     c                   eval      WIMnum    = DUJnum
044400070509     c                   eval      WIMtrk    = '2'
044500070509     c     K02WIM01      setll     WFAIM000
044600070509     c     K02WIM01      reade(n)  WFAIM000
044700070509     c                   eval      $EoF      = (%eof(WFAIM01L))
044800070412     c                   exsr      RollUpS01
044900070620      *
045000070620      * Conteggio dei records di dettaglio già inseriti (trk. "2")
045100070620     c                   eval      WrecTrk2  = S01nrr
045200070518      *
045300070518      * Se "visualizzazione": si caricano anche le matricole già
045400070518      *                       confermate
045500070518     c                   if        DUJopz   <> '5'
045600070518     c                   leavesr
045700070518     c                   endif
045800070518      *
045900070518     c                   reset                   $EoF
046000070518     c                   eval      WIMnum    = DUJnum
046100070518     c                   eval      WIMtrk    = '4'
046200070518     c     K02WIM01      setll     WFAIM000
046300070518     c     K02WIM01      reade(n)  WFAIM000
046400070518     c                   eval      $EoF      = (%eof(WFAIM01L))
046500070518     c                   exsr      RollUpS01
046600070521      *
046700070521      * Conteggio dei records di dettaglio già confermati (trk. "4")
046800070620     c                   eval      WrecTrk4  = S01nrr - WrecTrk2
046900070412      *
047000070412     c                   ENDSR
047100070412      *
047200070412      *---------------------------------------------------------------*
047300070412      *?Caricamento pagina successiva di dati S01                    ?*
047400070412      *---------------------------------------------------------------*
047500070412     c     RollUpS01     BEGSR
047600070412      *
047700070518     c***                eval      S01nrr    = (W_SflPag * C_SflPag)
047800070518     c***                add       1             W_SflPag
047900070412     c                   eval      *in32     = *off
048000070412      *
048100070412      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
048200070412do  1c                   doW            $EoF   = *off
048300070412     c*** carica tutto:            and  S01nrr < W_SflPag * C_SflPag
048400070412     c                   exsr      CarS01
048500070509     c     K02WIM01      reade     WFAIM000
048600070509     c                   eval      $EoF      = (%eof(WFAIM01L))
048700070412e   1c                   enddo
048800070412      *
048900070412      * Visualizzazione totale unità già inserite
049000070412     c                   eval      C1Cprg    = MAXnrr
049100070412      *
049200070412      * Visualizzazione del SFL (se ci sono dati)
049300070412     c                   eval      *in30     = (S01nrr = *zeros)
049400070412      *
049500070412      * Attivazione (eventuale) del SFLEND
049600070412     c                   eval      *in33     = $EoF
049700070412      *
049800070412      * Posizionamento cursore al 1° rcd della pagina
049900070412if  1c                   if        S01nrr    > *zeros
050000070412     c     S01nrr        div       C_SflPag      wPag
050100070412     c                   mvr                     wRig
050200070412     c                   eval      C01rcd    = wPag * C_SflPag
050300070412     c                   if        wRig      > *zeros
050400070412     c                   eval      C01rcd    = C01rcd + 1
050500070412     c                   else
050600070412     c                   eval      C01rcd    = C01rcd - C_SflPag + 1
050700070412     c                   endif
050800070412x   1c                   else
050900070412     c                   clear                   C01rcd
051000070412e   1c                   endif
051100070412      *
051200070412     c                   ENDSR
051300070412      *
051400070412      *---------------------------------------------------------------*
051500070509      *?Caricamento dati da file WFAIM00F in subfile S01             ?*
051600070412      *---------------------------------------------------------------*
051700070412     c     CarS01        BEGSR
051800070412      *
051900070417     c                   clear                   SDUJS01
052000070412      *
052100070412      * Controlli preliminari:
052200070412      * SE matricola già inserita => uscita (NO caricamento record)
052300070412     c                   clear                   wErr
052400070509     c                   eval      wMatr     = WIMuni
052500070412     c                   exsr      CtrlMAT_1
052600070521if  1c                   if             wErr    > *zeros
052700070521     c                             and  DUJopz <> '5'
052800070412     c                   leavesr
052900070412e   1c                   endif
053000070412      * ALTRIMENTI => gestione codice matricola - anche se anomalo
053100070412      *
053200070412      * Impostazione campi del subfile
053300070509     c                   eval      H1Cprg    = WIMprg
053400070509     c                   eval      S1Cmat    = WIMuni
053500070521if  1c                   if        DUJopz   <> '5'
053600070412     c                   eval      S1Cprg    = MAXnrr + 1
053700070521x   1c                   else
053800070521     c                   eval      S1Cprg    = WIMprg
053900070521e   1c                   endif
054000070518if  1c                   if        WIMtrk    > '2'
054100070518     c                   eval      S1Dnot    = 'Confermato'
054200070518e   1c                   endif
054300070518      *
054400070518      * Impostazione indicatori
054500070518     c                   eval      *in45     = (DUJopz = '5')
054600070518     c                   eval      *in41     = (WIMtrk > '2')
054700070412      *
054800070412      * Scrittura record subfile
054900070412     c                   add       1             S01nrr
055000070417     c                   write     SDUJS01
055100070412     c                   eval      MAXnrr    = S01nrr
055200070412      *
055300070412      * Intabellamento codice matricola appena inserito
055400070412     c                   eval      $Matr(S01nrr) = wMatr
055500070412      *
055600070412     c                   ENDSR
055700070412      *
055800070412      *---------------------------------------------------------------*
055900070412      *?Controllo dati in subfile control C01                        ?*
056000070412      *---------------------------------------------------------------*
056100070412     c     CtrC01        BEGSR
056200070412      *
056300070412      * Controllo matricola:
056400070412      * SE matricola già inserita => uscita (NO caricamento record)
056500070412     c                   clear                   wErr
056600081031sel 1c                   select
056700081031w   1c                   when      C1Cpsf    = *blanks
056800070412     c                   eval      wMatr     = C1Cmat
056900081031w   1c                   when      C1Cpsf    = 'P'
057000081031     c                   eval      wMatr     = %trimr(C1Cpsc)
057100081031     c                                       + %trimr(C1Cmat)
057200081031w   1c                   when      C1Cpsf    = 'S'
057300081031     c                   eval      wMatr     = %trimr(C1Cmat)
057400081031     c                                       + %trimr(C1Cpsc)
057500081031x   1c                   other
057600081031     c                   eval      wMatr     = C1Cmat
057700081031e   1c                   endsl
057800070412     c                   exsr      CtrlMAT_1
057900070412if  1c                   if        wErr      > *zeros
058000070412     c                   leavesr
058100070412e   1c                   endif
058200070412      *
058300070412      * ALTRIMENTI => gestione codice matricola - anche se anomalo...
058400070412      *               (in seguito...)
058500070412      *
058600070412     c                   ENDSR
058700070412      *
058800070412      *---------------------------------------------------------------*
058900070412      *?Caricamento dati appena inseriti da C01 in subfile S01       ?*
059000070412      *---------------------------------------------------------------*
059100070412     c     WrtS01        BEGSR
059200070412      *
059300070417     c                   clear                   SDUJS01
059400070412      *
059500070412      * Impostazione campi
059600070412     c*** già così:      clear                   H1Cprg
059700081031     c*** Pref/Suff ?    eval      S1Cmat    = C1Cmat
059800081031     c                   eval      S1Cmat    = wMatr
059900070412     c                   eval      S1Cprg    = MAXnrr + 1
060000070412      *
060100070412      * Scrittura record subfile
060200070412      * · Meglio "ripristinare" S01NRR: potrebbe essere impostato a
060300070412      *   quello di un subfile-record appena annullato/ripristinato...
060400070412     c                   eval      S01nrr    = MAXnrr
060500070412     c                   add       1             S01nrr
060600070417     c                   write     SDUJS01
060700070412     c                   eval      MAXnrr    = S01nrr
060800070412      *
060900070412      * Intabellamento codice matricola
061000070412     c                   eval      $Matr(S01nrr) = wMatr
061100070412      *
061200070412      * Visualizzazione del SFL (...visto che ci sono dati!)
061300070412     c***                eval      *in30     = (S01nrr = *zeros)
061400070412     c                   eval      *in30     = *off
061500070412      *
061600070412      * Attivazione (eventuale) del SFLEND
061700070412     c*** (sempre *on)   eval      *in33     = $EoF
061800070412      *
061900070412      * Posizionamento cursore al 1° rcd della pagina
062000070412if  1c                   if        S01nrr    > *zeros
062100070412     c     S01nrr        div       C_SflPag      wPag
062200070412     c                   mvr                     wRig
062300070412     c                   eval      C01rcd    = wPag * C_SflPag
062400070412     c                   if        wRig      > *zeros
062500070412     c                   eval      C01rcd    = C01rcd + 1
062600070412     c                   else
062700070412     c                   eval      C01rcd    = C01rcd - C_SflPag + 1
062800070412     c                   endif
062900070412x   1c                   else
063000070412     c                   clear                   C01rcd
063100070412e   1c                   endif
063200070412      *
063300070412     c                   ENDSR
063400070412      *
063500070412      *---------------------------------------------------------------*
063600070412      *?Gestione opzioni subfile S01                                 ?*
063700070412      *---------------------------------------------------------------*
063800070412     c     OpzS01        BEGSR
063900070509      *
064000070509      * "A" di annullamento / "F" di forzatura rimangono impostate nel
064100070509      * subfile; non serve fare altro, perchè in fase di conferma verrà
064200070509      * gestita questa opzione:
064300070509      * x "A" il barcode in esame NON verrà riportato in WFAIM00F;
064400070509      * x "F" il barcode in esame verrà riportato in WFAIM00F, anche
064500070509      *       se formalmente scorretto.?=> "F" NON PIÙ GESTITA !!!?
064600070412      *
064700070412      * Ciclo di lettura del subfile
064800070417     c                   readc     SDUJS01
064900070412      *
065000070417do  1c                   DOW       NOT %eof(TNSDUJD2)
065100070412      *
065200070412     c                   eval      *in32     = *off
065300070412     c                   movea     *zeros        *in(50)
065400070412     c                   z-add     S01nrr        C01rcd
065500070412      *
065600070412sel 2c                   SELECT
065700070412      * - Nessuna opzione
065800070412w   2c                   WHEN      S1Copz    = *blanks
065900070412     c                   clear                   $MAnn(S01nrr)
066000070412      * - Opz. A=Annullamento
066100070412w   2c                   WHEN      S1Copz    = 'A'
066200070412     c                   z-add     1             $MAnn(S01nrr)
066300070412      * - Opz. F=Forzatura
066400070509w   2c***                WHEN      S1Copz    = 'F'
066500070509     c***                clear                   $MAnn(S01nrr)
066600070412      * - Opz. errata
066700070412x   2c                   OTHER
066800070412     c                   seton                                        285090
066900070412     c                   eval      V1Dmsg    = $Msg( 1)
067000070412e   2c                   ENDSL
067100070412      *
067200070412      * > Aggiornamento sfl (senza?MAI?pulire il campo opzione)
067300070412sel 2c                   select
067400070412w   2c                   when      *in28
067500070412     c                   eval      *in32     = *on
067600070412w   2c                   when      *in90     = *on
067700070412e   2c                   endsl
067800070417     c                   update    SDUJS01
067900070412if  2c                   if        *in28  OR  *in90
068000070412     c                   leave
068100070412e   2c                   endif
068200070412      *
068300070417     c                   readc     SDUJS01
068400070412      *
068500070412e   1c                   ENDDO
068600070412      *
068700070412      * Ripristino del numero relativo di record già raggiunto nel sfl
068800070412     c*** non qui:       eval      S01nrr    = MAXnrr
068900070412      *
069000070412     c                   ENDSR
069100070412      *
069200070412      *---------------------------------------------------------------*
069300070412      *?Controllo dati in TUTTI i records del subfile S01 (per F6)   ?*
069400070412      *---------------------------------------------------------------*
069500070412     c     CtrS01        BEGSR
069600070412      *
069700070412     c                   movea     *zeros        *in(50)
069800070412      *
069900070412      * Ciclo di lettura subfile S01
070000070412do  1c                   DO        MAXnrr        S01nrr
070100070412      *
070200070417     c     S01nrr        chain     SDUJS01
070300070417if  2c                   if        NOT %found(TNSDUJD2)
070400070606     c                   eval      MAXnrr    = S01nrr - 1
070500070412     c                   leave
070600070412e   2c                   endif
070700070412      *
070800070412      * - Elaborazione dei soli records NON "annullati"
070900070412if  2c                   if        S1Copz    = 'A'
071000070412     c                   eval      $MAnn(S01nrr) = 1
071100070412     c                   iter
071200070412e   2c                   endif
071300070412     c                   clear                   $MAnn(S01nrr)
071400070412      *
071500070509      * - Controllo barcode -SE?NON?chiesta "forzatura"
071600070412     c                   clear                   wErr
071700070518if  2c***                IF        S1Copz   <> 'F'
071800070412     c                   eval      wMatr     = S1Cmat
071900070412     c                   exsr      CtrlMAT_2
072000070412     c                   eval      *in28     = (wErr > *zero)
072100070518e   2c***                ENDIF
072200070412      *
072300070412      * > Emissione eventuale messaggio di errore
072400070412      *   (senza MAI pulire il campo opzione)
072500070412if  2c                   IF        *in28
072600070412     c                   seton                                        50  90
072700070412     c                   eval      *in32     = *on
072800070412if  3c                   if        wErr      > *zeros
072900070412sel 4c                   select
073000070413w   4c                   when      wErr      = 3
073100070412     c                   eval      V1Dmsg    = %replace(
073200070509     c                                           %editc(§WIM1lm1:'Z'):
073300070413     c                                           $Msg( 3):
073400070413     c                                           %scan('&N':$Msg( 3)) )
073500070413w   4c                   when      wErr      = 5
073600070412     c                   eval      V1Dmsg    = %trimr($Msg(wErr))
073700070412     c                                       + %editc(SQLnum:'Z')
073800070412x   4c                   other
073900070412     c                   eval      V1Dmsg    = $Msg(wErr)
074000070412e   4c                   endsl
074100070412e   3c                   endif
074200070412     c                   z-add     S01nrr        C01rcd
074300070412e   2c                   ENDIF
074400070412      *
074500070417     c                   update    SDUJS01
074600070412if  2c                   if        *in28  OR  *in90
074700070412     c                   leave
074800070412e   2c                   endif
074900070412      *
075000070412e   1c                   ENDDO
075100070412      *
075200070412     c                   eval      C1Cprg    = MAXnrr - %xfoot($MAnn)
075300070412if  1c                   if        *in28  OR  *in90
075400070412     c                   leavesr
075500070412e   1c                   endif
075600070412      *
075700070412      * Verifica se tot. matric. da inserire <> n° matric. confermate
075800070521      *   (conteggiando anche quelle eventualmente già confermate -
075900070521      *    vedi trk. "3"/"4")
076000070521if  1c***                if        §WIM1nmi <> C1Cprg
076100070521if  1c                   if        §WIM1nmi <> (C1Cprg + WrecTrk4)
076200070412     c                   eval      *in90     = *on
076300070413     c                   eval      wErr      = 6
076400070412     c                   eval      $Video    = 'W1'
076500070412     c                   eval      $InzW01   = *on
076600070412e   1c                   endif
076700070412      *
076800070412     c                   ENDSR
076900070412      *
077000070412      *---------------------------------------------------------------*
077100070412      *?Gestione tasto funzionale F03 da videata S01                 ?*
077200070412      *---------------------------------------------------------------*
077300070412     c     F03S01        BEGSR
077400070620      *
077500070620     c                   clear                   WWWprg
077600070620if  1c                   if        WrecTrk2  > *zeros
077700070620     c                   eval      WWWprg    = %xfoot( %subarr(
077800070620     c                                           $MAnn:1:WrecTrk2 ))
077900070620e   1c                   endif
078000070412      *
078100070412      * Chiusura del programma
078200070620if  1c                   if             DUJopz  = '4'
078300070620     c                             or   DUJopz  = '5'
078400070620     c                             or  (C1Cprg  = WrecTrk2
078500070620     c                              and WWWprg  = *zeros)
078600070620     c                             or   $Video  = 'W1'
078700070620      *
078800070417     c                   eval      DUJret    = '1'
078900070412     c                   eval      $Fine     = *on
079000070620      *
079100070620x   1c                   else
079200070620      *
079300070620      * ...chiedendone PRIMA la conferma
079400070620     c                   eval      *in90     = *on
079500070620     c                   eval      wErr      = -03
079600070620     c                   eval      $Video    = 'W1'
079700070620     c                   eval      $InzW01   = *on
079800070620      *
079900070620e   1c                   endif
080000070412      *
080100070412     c                   ENDSR
080200070412      *
080300070412      *---------------------------------------------------------------*
080400070423      *?Gestione tasto funzionale F06 da videata S01                 ?*
080500070412      *---------------------------------------------------------------*
080600070412     c     F06S01        BEGSR
080700070412      *
080800070517      * Ciclo di cancellazione degli eventuali record "2" pre-esistenti
080900070509     c                   eval      WIMnum    = DUJnum
081000070509     c                   eval      WIMtrk    = '2'
081100070509     c                   exsr      Dlt_WFAIM
081200070412      *
081300070412      * Ciclo di lettura subfile S01
081400070412     c                   clear                   WWWprg
081500070412do  1c                   DO        MAXnrr        S01nrr
081600070412      *
081700070417     c     S01nrr        chain     SDUJS01
081800070417if  2c                   if        NOT %found(TNSDUJD2)
081900070412     c                   leave
082000070412e   2c                   endif
082100070412      *
082200070412      * - Registrazione dei soli records NON annullati
082300070412if  2c                   if        S1Copz   <> 'A'
082400070509     c                   exsr      Wrt_WFAIM
082500070412e   2c                   endif
082600070412      *
082700070412e   1c                   ENDDO
082800070412      *
082900070412      * Chiusura del programma
083000070412     c                   eval      $Fine     = *on
083100070412      *
083200070412     c                   ENDSR
083300070412      *
083400070412      *---------------------------------------------------------------*
083500070412      *?Gestione tasto funzionale F08 da videata S01                 ?*
083600070412      *---------------------------------------------------------------*
083700070412     c     F08S01        BEGSR
083800070412      *
083900070412      * "Salvataggio" parametri ricevuti
084000070417     c                   movel     TNSDUJds      SaveU0ds
084100070412      *
084200070417      * Richiamo pgm. TNSDUJR1
084300070417     c                   eval      DUJopz    = '2'
084400070417     c                   exsr      Call_TNSDUJ1
084500070413      *
084600070417if  1c                   if        DUJret    = '1'
084700070413     c                   eval      $Fine     = *on
084800070413e   1c                   endif
084900070412      *
085000070412      * "Ripristino" parametri ricevuti
085100070417     c                   movel     SaveU0ds      TNSDUJds
085200070413if  1c                   if        $Fine     = *on
085300070417     c                   eval      DUJret    = '2'
085400070413e   1c                   endif
085500070412      *
085600070412     c                   ENDSR
085700070412      *
085800070412      *---------------------------------------------------------------*
085900070412      *?Gestione tasto funzionale F12 da videata S01                 ?*
086000070412      *---------------------------------------------------------------*
086100070412     c     F12S01        BEGSR
086200070620      *
086300070620     c                   clear                   WWWprg
086400070620if  1c                   if        WrecTrk2  > *zeros
086500070620     c                   eval      WWWprg    = %xfoot( %subarr(
086600070620     c                                           $MAnn:1:WrecTrk2 ))
086700070620e   1c                   endif
086800070412      *
086900070412      * Chiusura del programma
087000070620if  1c                   if             DUJopz  = '4'
087100070620     c                             or   DUJopz  = '5'
087200070620     c                             or  (C1Cprg  = WrecTrk2
087300070620     c                              and WWWprg  = *zeros)
087400070620     c                             or   $Video  = 'W1'
087500070620      *
087600070417     c                   eval      DUJret    = '2'
087700070412     c                   eval      $Fine     = *on
087800070620      *
087900070620x   1c                   else
088000070620      *
088100070620      * ...chiedendone PRIMA la conferma
088200070620     c                   eval      *in90     = *on
088300070620     c                   eval      wErr      = -12
088400070620     c                   eval      $Video    = 'W1'
088500070620     c                   eval      $InzW01   = *on
088600070620      *
088700070620e   1c                   endif
088800070412      *
088900070412     c                   ENDSR
089000070412      *
089100070412      *---------------------------------------------------------------*
089200070412      *?Gestione videata W01                                         ?*
089300070412      *---------------------------------------------------------------*
089400070412     c     GesW01        BEGSR
089500070412      *
089600070412      * Inizializzazione della videata
089700070412     c                   if        $InzW01   = *on
089800070412     c                   exsr      InzW01
089900070412     c                   eval      $InzW01   = *off
090000070412     c                   endif
090100070412      *
090200070412      * Emissione della window
090300070417     c                   exfmt     SDUJW01
090400070412      *
090500070412sel 1c                   select
090600070412      * F12 = Ritorno
090700070412w   1c                   when      *inKL
090800070620     c                   eval      $Video    = 'S1'
090900070412      * F6 = Conferma
091000070412w   1c                   when      *inKF
091100070620sel 2c                   select
091200070620w   2c                   when      wErr      > *zeros
091300070412     c                   eval      $Video    = 'S1'
091400070412     c                   exsr      F06S01
091500070620w   2c                   when      wErr      = -03
091600070620     c                   exsr      F03S01
091700070620w   2c                   when      wErr      = -12
091800070620     c                   exsr      F12S01
091900070620e   2c                   endsl
092000070412e   1c                   endsl
092100070412      *
092200070412     c                   ENDSR
092300070412      *
092400070412      *---------------------------------------------------------------*
092500070412      *?Inizializzazione videata W01                                 ?*
092600070412      *---------------------------------------------------------------*
092700070412     c     InzW01        BEGSR
092800070412      *
092900070417     c                   clear                   SDUJW01
093000070412      *
093100070412sel 1c                   SELECT
093200070620      * ----------------
093300070620      * Completamento messaggio  6/ 7/ 8:
093400070620      *   richiede numeri di 4/0: si utilizza?temporaneamente?S01nrr !
093500070413w   1c                   WHEN      wErr      = 6
093600070412     c                   eval      S01nrr    = C1Cprg
093700070412     c                   eval      W1Ctx1    = %replace(
093800070412     c                                           %editc(S01nrr:'3'):
093900070412     c                                           $Msg(wErr):
094000070412     c                                           %scan('&NMI':
094100070412     c                                                 $Msg(wErr) ))
094200070412     c                   eval      W1Ctx2    = %replace(
094300070509     c                                           %editc(§WIM1nmi:'3'):
094400070412     c                                           $Msg(wErr+1):
094500070412     c                                           %scan('&NMP':
094600070412     c                                                 $Msg(wErr+1) ))
094700070412     c                   eval      W1Ctx3    = $Msg(wErr+2)
094800070412     c                   eval      S01nrr    = MAXnrr
094900070620      * ----------------
095000070620      * Completamento messaggio 11:
095100070620      *   richiede numeri di 4/0: si utilizza?temporaneamente?S01nrr !
095200070620w   1c                   WHEN           wErr    < *zeros
095300070620     c                             and (*inKC   = *on
095400070620     c                              or  *inKL   = *on)
095500070620     c                   clear                   WWWprg
095600070620if  2c                   if        WrecTrk2  > *zeros
095700070620     c                   eval      WWWprg    = %xfoot( %subarr(
095800070620     c                                           $MAnn:1:WrecTrk2 ))
095900070620e   2c                   endif
096000070620sel 2c                   Select
096100070620      * - Annullati records già presenti
096200070620w   2c                   When      WWWprg  > *zeros
096300070620     c                   eval      S01nrr    = WWWprg
096400070620     c                   eval      W1Ctx1    = %replace(
096500070620     c                                           %editc(S01nrr:'3'):
096600070620     c                                           $Msg(10):
096700070620     c                                           %scan('&NMA':
096800070620     c                                                 $Msg(10) ))
096900070620      * - - ...ed inserite altre matricole (NON annullate)
097000070620if  3c                   if        *zeros    < MAXnrr - WrecTrk2
097100070620     c                                       - %xfoot( %subarr(
097200070620     c                                           $MAnn:WrecTrk2+1 ))
097300070620     c                   eval      S01nrr    = MAXnrr - WrecTrk2
097400070620     c                                       - %xfoot( %subarr(
097500070620     c                                           $MAnn:WrecTrk2+1 ))
097600070620     c                   eval      W1Ctx2    = %replace(
097700070620     c                                           %editc(S01nrr:'3'):
097800070620     c                                           $Msg(11):
097900070620     c                                           %scan('&NMI':
098000070620     c                                                 $Msg(11) ))
098100070620e   3c                   endif
098200070620      * - Inserita 1 matricola
098300070620w   2c                   When      1         = C1Cprg - WrecTrk2
098400070620     c                   eval      W1Ctx1    = $Msg(12)
098500070620      * - Inserita altre matricole
098600070620x   2c                   Other
098700070620     c                   eval      S01nrr    = C1Cprg - WrecTrk2
098800070620     c                   eval      W1Ctx1    = %replace(
098900070620     c                                           %editc(S01nrr:'3'):
099000070620     c                                           $Msg(13):
099100070620     c                                           %scan('&NMI':
099200070620     c                                                 $Msg(13) ))
099300070620e   2c                   EndSl
099400070620if  2c                   if        W1Ctx2    = *blanks
099500070620     c                   eval      W1Ctx2    = $Msg(14)
099600070620x   2c                   else
099700070620     c                   eval      W1Ctx3    = $Msg(14)
099800070620e   2c                   endif
099900070620     c                   eval      S01nrr    = MAXnrr
100000070620      * ----------------
100100070412e   1c                   ENDSL
100200070412      *
100300070412     c                   ENDSR
100400070412      *
100500070412      *---------------------------------------------------------------*
100600070509      *?Richiamo del pgm. TNSDUJR1 (gestione trk. "1" file WFAIM00F) ?*
100700070412      *---------------------------------------------------------------*
100800070417     c     Call_TNSDUJ1  BEGSR
100900070412      *
101000070417     c*** già impostato: eval      DUJopz    = ?
101100070412      *
101200070417      * Richiamo pgm. TNSDUJR1
101300070417     c                   movel(p)  TNSDUJds      KPJBU
101400070417     c                   call      'TNSDUJR1'
101500070412     c                   parm                    KPJBA
101600070417     c                   movel     KPJBU         TNSDUJds
101700070412      *
101800070412sel 1c                   SELECT
101900070412      * Ritorno normale
102000070417w   1c                   WHEN           DUJret  = *off
102100070417     c                             and  DUJerr  = *off
102200070509     c                   exsr      Upd_Txt_C01
102300070412      * Ritorno con errore
102400070417w   1c                   WHEN      DUJerr    = *on
102500070412     c                   seton                                        28  90
102600070417     c                   eval      V1Dmsg    = DUJmsg
102700070412      * Ritorno con F3
102800070620      * (forzo "F12" per non far chiudere anche TNSDUJR e tornare a
102900070620      *  menù)
103000070417w   1c                   WHEN      DUJret    = '1'
103100070412     c                   eval      $Fine     = *on
103200070412      * Ritorno con F12
103300070417w   1c                   WHEN      DUJret    = '2'
103400070412      *
103500070412e   1c                   ENDSL
103600070412      *
103700070412     c                   ENDSR
103800070509      *
103900070509      *---------------------------------------------------------------*
104000070509      *?Aggiornamento testata in C01 con nuovi dati restituiti da R1 ?*
104100070509      *---------------------------------------------------------------*
104200070509     c     Upd_Txt_C01   BEGSR
104300070509      *
104400070509     c                   movel     DUJuni        dWIMuni1
104500070509      *
104600070509     c                   eval      C1Dnum    = DUJnum
104700070509     c                   eval      C1Ddes    = §WIM1des
104800070509     c                   eval      C1Dnmi    = §WIM1nmi
104900070518      *
105000070518     c                   eval      *in45     = (DUJopz = '5')
105100070509      *
105200070509     c                   ENDSR
105300070412      *
105400070412      *---------------------------------------------------------------*
105500070412      *?Controllo singolo codice matricola inserito (1)              ?*
105600070412      *---------------------------------------------------------------*
105700070412     c     CtrlMAT_1     BEGSR
105800070412      *
105900070412      *?Matricola (estesa) già inserita nel subfile?
106000070412     c     wMatr         lookup    $Matr                                  25
106100070412     c                   if        *in25
106200070413     c                   eval      wErr      = 2
106300070412     c                   leavesr
106400070412     c                   endif
106500070412      *
106600070412     c                   ENDSR
106700070412      *
106800070412      *---------------------------------------------------------------*
106900070412      *?Controllo singolo codice matricola inserito (2)              ?*
107000070412      *---------------------------------------------------------------*
107100070412     c     CtrlMAT_2     BEGSR
107200070412      *
107300070412      * Lunghezza matricola errata
107400070509     c                   if             §WIM1lm1 > *zeros
107500070509     c                             and  %len(%trimr(wMatr)) <> §WIM1lm1
107600070413     c                   eval      wErr      = 3
107700070412     c                   leavesr
107800070412     c                   endif
107900070412      *
108000070412      * Matricola (sottostringa selezionata) già inserita nel subfile
108100070412     c                   eval      yy        = 1
108200070412do  1c                   DOW       yy        < S01nrr
108300070412sel 2c                   select
108400070412w   2c                   when      $Matr(yy) = *blanks
108500070412     c                   leave
108600070412w   2c                   when      $MAnn(yy) > *zeros
108700070509w   2c                   when      %subst(wMatr    :§WIM1im2:§WIM1lm2) =
108800070509     c                             %subst($Matr(yy):§WIM1im2:§WIM1lm2)
108900070413     c                   eval      wErr      = 4
109000070412     c                   leavesr
109100070412e   2c                   endsl
109200070412     c                   add       1             yy
109300070412e   1c                   ENDDO
109400070412      *
109500070509      * Matricola (sottostringa selez.) già inserita nel file WFAIM00F
109600070412     c                   eval      wMatr§    = '%' +
109700070509     c                                  %subst(wMatr:§WIM1im2:§WIM1lm2)
109800070412     c                                       + '%'
109900070412     c                   exsr      OpenCursor
110000070412     c                   exsr      ReadCursor
110100070412     c                   exsr      CloseCursor
110200070412if  1c                   if        $Found    = *on
110300070413     c                   eval      wErr      = 5
110400070412     c                   leavesr
110500070412e   1c                   endif
110600070412      *
110700070412     c                   ENDSR
110800070412      *
110900070412      *****************************************************************
111000070412      **?Apertura cursore.                                          ?**
111100070412      *****************************************************************
111200070412     c     OpenCursor    BEGSR
111300070412      *
111400070412     c/exec sql
111500070412     c+    declare A1 cursor for
111600070412     c+    select  *
111700070509     c+    from    WFAIM00F
111800070509     c+    where   WIMtrk =    '2'
111900070509     c+      and   WIMnum <>   :WIMnum
112000070509     c+      and   WIMuni like :wMatr§
112100070412     c+    for     fetch  only
112200070412     c/end-exec
112300070412      *
112400070412     c/exec sql
112500070412     c+    open    A1
112600070412     c/end-exec
112700070412      *
112800070412     c                   ENDSR
112900070412      *
113000070412      *****************************************************************
113100070412      **?Lettura cursore.                                           ?**
113200070412      *****************************************************************
113300070412     c     ReadCursor    BEGSR
113400070412      *
113500070509     c                   clear                   WFAIMds
113600070412      *
113700070412     c/exec sql
113800070509     c+    fetch   next from A1 into :WFAIMds
113900070412     c/end-exec
114000070412      *
114100070412sel 1c                   select
114200070412w   1c                   when      SQLcod = 100
114300070412     c                   eval      $Found = *off
114400070412w   1c                   when      SQLcod < 0
114500070412     c                   eval      $Err   = *on
114600070412     c                   eval      $Found = *off
114700070412x   1c                   other
114800070412     c                   eval      $Found = *on
114900070412e   1c                   endsl
115000070412      *
115100070412     c                   ENDSR
115200070412      *
115300070412      *****************************************************************
115400070412      **?Chiusura cursore.                                          ?**
115500070412      *****************************************************************
115600070412     c     CloseCursor   BEGSR
115700070412      *
115800070412     c/exec sql
115900070412     c+    close   A1
116000070412     c/end-exec
116100070412      *
116200070412     c                   ENDSR
116300070423      *
116400070423      *---------------------------------------------------------------*
116500070423      *?Cancellazione sing. record di un "inserimento multiplo unità"?*
116600070423      *---------------------------------------------------------------*
116700070509     c     Dlt_WFAIM_2   BEGSR
116800070423      *
116900070517      * Cancellazione "logica" di un singolo rec. trk "2"
117000070509     c                   eval      WIMnum    = DUJnum
117100070509     c                   eval      WIMtrk    = '2'
117200070509     c                   eval      WIMprg    = DUJprg
117300070509     c     K03WIM01      chain     WFAIM000
117400070509do  1c                   if        %found(WFAIM01L)
117500070517      *
117600070517     c                   eval      WIMtrk    = '4'
117700070423      *                  __________________
117800070517     c                   UPDATE    WFAIM000
117900070423      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
118000070507x   1c                   else
118100070507     c                   eval      DUJerr  = *on
118200070507     c                   eval      DUJmsg    = %trimr( %replace(
118300070507     c                                           %editc(DUJprg:'Z'):
118400070507     c                                           $Msg( 9):
118500070507     c                                           %scan('&PRGR':
118600070507     c                                                 $Msg( 9)) ))
118700070507     c                                       + %editc(DUJprg:'K')
118800070423e   1c                   endif
118900070423      *
119000070423     c                   ENDSR
119100070412      *
119200070412      *---------------------------------------------------------------*
119300070412      *?Cancellazione dettaglio di un "inserimento multiplo unità"   ?*
119400070412      *---------------------------------------------------------------*
119500070509     c     Dlt_WFAIM     BEGSR
119600070412      *
119700070412      * Ciclo di cancellazione degli eventuali rec. trk "2" esistenti
119800070412      *   (con chiavi di lettura già impostate)
119900070509     c     K02WIM01      setll     WFAIM000
120000070509     c     K02WIM01      reade     WFAIM000
120100070509do  1c                   DOW       NOT  %eof(WFAIM01L)
120200070412      *                  __________________
120300070509     c                   DELETE    WFAIM000
120400070412      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
120500070509     c     K02WIM01      reade     WFAIM000
120600070412e   1c                   ENDDO
120700070412      *
120800070412     c                   ENDSR
120900070412      *
121000070412      *---------------------------------------------------------------*
121100070509      *?Scrittura singolo record trk "2" del file WFAIM00F           ?*
121200070412      *---------------------------------------------------------------*
121300070509     c     Wrt_WFAIM     BEGSR
121400070412      *
121500070412     c                   add       1             WWWprg
121600070412      *
121700070509     c                   clear                   WFAIM000
121800070412      *
121900070509     c                   eval      WIMnum    = DUJnum
122000070509     c                   eval      WIMtrk    = '2'
122100070509     c                   eval      WIMprg    = WWWprg
122200070509     c                   eval      WIMuni    = S1Cmat
122300070412      *                  __________________
122400070509     c                   WRITE     WFAIM000
122500070412      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
122600070412     c                   ENDSR
122700070509
122800070509** - $MSG ....+....2....+....3....+....4....+....5....+....6....+....7....+..*
122900070413Opzione errata                                                                 1
123000070413Matricola già inserita                                                         2
123100070413Matricola di lunghezza errata: deve essere di &N caratteri                     3
123200070413Matricola (sottostringa selezionata) già inserita nel subfile                  4
123300070413Matricola (sottostringa selezionata) reperita nell'inserimento n°              5
123400070509Inserite n° &NMI unità.                 |                                      6
123500070509Previste n° &NMP unità.                 |                                      7
123600070509»»»»»»»»»»»»»»» Premere F6 per forzare. |                                      8
123700070507Record n°&PRGR NON trovato per l'inserimento multiplo n°                       9
123800070620Le &NMA unità annullate andranno perse  |                                     10
123900070620e così pure le &NMI unità inserite.     |                                     11
124000070620La singola unità inserita andrà persa...|                                     12
124100070620Le &NMI unità inserite andranno perse...|                                     13
124200070620Premere F6 per confermare l'uscita.     |                                     14
124300081031Valori ammessi: "P" per prefisso, "S" per suffisso, "_" per nulla...          15
124400081031Inserire i caratteri da usare come                                            16
