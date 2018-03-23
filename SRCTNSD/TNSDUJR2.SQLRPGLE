000100130919      *PARMS DBGVIEW(*SOURCE)
000200130919     /*PRM  dbgview(*source)
000300130919     /*END
000400070412      *===============================================================*
000500070509      *?TNSDUJR2 - Inserimento multiplo unità EDP - trk "2" pf WFAIM ?*
000600070412      *===============================================================*
000700070605      *A V V E R T E N Z E :?                                       ?*
000800070605      * Occorre? ricompilare ANCHE ?il pgm.?TNSDUJR3? se ?si modifica *
000900070605      * il dspf TNSDUJD2.                                             *
001000070605      *===============================================================*
001100070503      *?N O T E :                                                    ?*
001200070503      * Valori ammessi per li campo DUJOPZ?ricevuto?:                 *
001300070606      * · 'A'   -> immissione nuovo inserimento multiplo unità        *
001400070606      *            con il subfile già caricato                        *
001500070606      *            (richiamato da pgm. TNSDUJR3)                      *
001600070503      * · '1'   -> immissione nuovo inserimento multiplo unità        *
001700070503      *            (richiamato da pgm. TNSDUJR)                       *
001800070503      * · '4'   -> cancellazione singolo record trk. "2"              *
001900070517      *            => imposta trk. "4" nel record (appena elaborato)  *
002000070503      *            (richiamato da pgm. TNSDUKR)                       *
002100070518      * · '5'   -> visualizzazione  inserimento multiplo unità        *
002200070518      *            (richiamato da pgm. TNSDUJR)                       *
002300070503      * · altro -> manutenzione     inserimento multiplo unità        *
002400070503      *            (richiamato da pgm. TNSDUJR)                       *
002500070503      * Valori gestiti per li campo DUJOPZ?passato?a TNSDUJR1:        *
002600070503      * · '0'   -> reperimento  record trk. "1"                       *
002700070503      * · '2'   -> manutenzione record trk. "1"                       *
002800070503      *===============================================================*
002900070412
003000070412     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003100070412
003200070412      *---------------------------------------------------------------*
003300070412
003400070509     fWFAIM01L  Uf A e           k disk
003500070412      *
003600070417     fTNSDUJD2  cf   e             workstn
003700070417     f                                     sfile(SDUJS01:S01nrr)
003800070412     f                                     infds(DSFMT)
003900070412
004000070412      *---------------------------------------------------------------*
004100070412
004200070412      *
004300070412      *?Costanti  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
004400070412      *
004500070412      * - Tasti di funzione
004600070412     d C_RollUp        c                   const(x'F5')
004700070412      * - Nr. di righe del sfl (SFLPAG)
004800070412     d C_SflPag        c                   const(16)
004900070412      *
005000070412      *?Schiere - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
005100070412      *
005200070412      * - Messaggi di errore
005300081031     d $Msg            s             78    dim(16)   ctdata  perrcd(1)
005400070412      * - Indicatori per abilitazione dei tasti funzionali (F1-F24)
005500070412     d $Fxx            s              1    dim(24)   inz(*off)
005600070412      * - Codici matricola già inseriti nel subfile
005700070412     d $Matr           s                   dim(9999) inz  like(S1Cmat)
005800070412      * - Flags numerici matricola "annullata" nel subfile (per NRR)
005900070412     d $MAnn           s              1  0 dim(9999) inz
006000070412      *
006100070412      *?Ds  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
006200070412      *
006300070412      * - Parametri
006400070412     d KPJBA         e ds
006500070417     d TNSDUJds      e ds                  inz
006600070412      *
006700070412      * - Parametri x Controllo profilo utenti
006800070606     d TIBS34ds      e ds                  inz
006900070412      * - Ds di riferimento al file esterno AZUTE00F
007000070412     d AZUTEds       e ds                  extname(AZUTE00F)
007100070412      * - Ds per dati organigramma
007200070606     d dDatiUte      e ds
007300070412      *
007400130919      * DS campo WIMUNI file WFAIM00F trk "1" prg 01
007500070509     d dWIMuni1      e ds                  inz
007600130919      * DS campo WIMUNI file WFAIM00F trk "1" prg 02
007700130919     d dWIMuni12     e ds                  inz
007800070412      *
007900070509      * DS file WFAIM00F per SQL
008000070509     d WFAIMds       e ds                  inz  extname(WFAIM00F)
008100070412     d                                     prefix(SQL:3)
008200070412      *
008300070412     d Status         sds           333
008400070412     d   SDSpgm          *proc
008500070412      *
008600070412     d dsFMT           ds           512
008700070412     d  £Tasto               369    369
008800070412      *
008900070412      *?Variabili - - - - - - - - - - - - - - - - - - - - - - - - - -?*
009000070412      *
009100070412      * - Flags
009200070412     d $EoF            s              1    inz(*off)
009300070412     d $Fine           s              1    inz(*off)
009400070412     d $InzS01         s              1    inz(*on)
009500070412     d $InzW01         s              1    inz(*on)
009600070412     d $Found          s              1    inz(*off)
009700070412     d $Err            s              1    inz(*off)
009800070412      *
009900070412     d $Video          s              2    inz('S1')
010000070412      *
010100070412      * - Indici di schiera
010200070412     d yy              s              3  0 inz
010300070412      *
010400070412      * - Variabili riferite al data base o al display file
010500070412     d S01nrr          s                   like(C01rcd)   inz
010600070412     d MAXnrr          s                   like(C01rcd)   inz
010700070412     d wMatr           s                   like(S1Cmat)   inz
010800070412     d wMatr§          s                   like(S1Cmat)   inz
010900070412     d wVideo          s                   like($Video)   inz
011000070509     d WWWprg          s                   like(WIMprg)   inz
011100070620     d WrecTrk2        s                   like(WIMprg)   inz
011200070521     d WrecTrk4        s                   like(WIMprg)   inz
011300070417     d SaveU0ds        s                   like(TNSDUJds) inz
011400070412      *
011500070412      * - Variabili definite a programma
011600070412     d wPag            s              4  0 inz
011700070412     d wRig            s              3  0 inz
011800070412     d wErr            s              3  0 inz
011900070518     d*W_SflPag***     s              3  0 inz
012000070412      *
012100070412      *?Key-List  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
012200070412      *
012300070509      * - WFAIM01L
012400070509     c     K02WIM01      klist
012500070509     c                   kfld                    WIMnum
012600070509     c                   kfld                    WIMtrk
012700070509     c     K03WIM01      klist
012800070509     c                   kfld                    WIMnum
012900070509     c                   kfld                    WIMtrk
013000070509     c                   kfld                    WIMprg
013100070412
013200070412      *---------------------------------------------------------------*
013300070412      *?RIEPILOGO INDICATORI                                         ?*
013400070412      *---------------------------------------------------------------*
013500070412      * 25    - Comodo                                                *
013600070416      * 28    - Emissione del messaggio di errore a video             *
013700070416      * 30    - Pulizia      del subfile S01 (sflclr, NO sfldsp)      *
013800070416      * 31    - NO emissione del subfile S01 (sflclr, NO sfldspctl)   *
013900070412      * 32    - Record modificato nel subfile S01 (sflnxtchg)         *
014000070416      * 33    - Fine dati         nel subfile S01 (sflend)            *
014100070412      * 41    - S01: Evidenziazione S1CMAT in (RI)                    *
014200070518      * 45    - S01: Protezione campi I/O  per sola visualizzazione   *
014300081031      * 46    - C01: Protezione campi I/O  per immissione NON manuale *
014400070412      * 50    - S01: Posizionamento cursore sul campo "Opzione"       *
014500070412      * 90    - Generico di errore                                    *
014600070412      *---------------------------------------------------------------*
014700070412
014800070412     c     *Entry        plist
014900070412     c                   parm                    KPJBA
015000070412      *
015100070412      * Operazioni Iniziali
015200070412     c                   exsr      RoutInz
015300070412      *
015400070412      * Gestione Video
015500070412do  1c                   DOW       $Fine     = *off
015600070412cas 2c     $Video        caseq     'S1'          GesS01
015700070412cas 2c     $Video        caseq     'W1'          GesW01
015800070412e   2c                   endcs
015900070412e   1c                   ENDDO
016000070412      *
016100070412      * Fine
016200130923     c                   movel(p)  dWIMuni1      DUJuni1
016300130923     c                   movel(p)  dWIMuni12     DUJuni2
016400070417     c                   movel(p)  TNSDUJds      KPJBU
016500070412     c                   eval      *inLR     = *on
016600070412      *
016700070412      *---------------------------------------------------------------*
016800070412      *?Operazioni Iniziali                                          ?*
016900070412      *---------------------------------------------------------------*
017000070412     c     RoutInz       BEGSR
017100070412      *
017200070412     c/exec sql
017300070412     c+     set  option  DYNUSRPRF = *OWNER,
017400070412     c+                  CLOSQLCSR = *ENDMOD
017500070412     c/end-exec
017600070412      *
017700070412      * Reperimento dati job
017800070412     c                   exsr      DatiJob
017900070412      *
018000070412     c                   reset                   $Fine
018100070412     c                   reset                   $InzS01
018200070412     c                   reset                   $InzW01
018300070412      *
018400070412      * Impostazione iniziale dei parametri i/o
018500070417     c                   movel     KPJBU         TNSDUJds
018600070417     c                   eval      DUJret    = *off
018700070417     c                   eval      DUJerr    = *off
018800070417     c                   eval      DUJmsg    = *blanks
018900070509      *
019000070509      * "Intestazione" del subfile-control
019100070509     c                   movel     SDSpgm        V1Tpgm
019200070518sel 1c                   select
019300070606w   1c                   when           DUJopz  = '1'
019400070606     c                             or   DUJopz  = 'A'
019500070509     c                   eval      V1Ttxt    = '     '
019600070509     c                                       + 'I N S E R I M E N T O'
019700070518w   1c                   when      DUJopz    = '5'
019800070518     c                   eval      V1Ttxt    = ' '
019900070518     c                                       + 'V I S U A L I Z Z A +
020000070518     c                                          Z I O N E'
020100070518x   1c                   other
020200070509     c                   eval      V1Ttxt    = '        '
020300070509     c                                       + 'M O D I F I C A'
020400070518e   1c                   endsl
020500070412      *
020600070412      * CONTROLLO PARAMETRI RICEVUTI
020700070412      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
020800070412sel 1c                   select
020900070503w   1c                   when           DUJopz   <> '0'
021000070503     c                             and  DUJopz   <> '1'
021100070503     c                             and  DUJopz   <> '2'
021200070503     c                             and  DUJopz   <> '4'
021300070518     c                             and  DUJopz   <> '5'
021400070503     c                             and  DUJopz   <> '7'
021500070503     c                             and  DUJopz   <> '8'
021600070503     c                             and  DUJopz   <> '9'
021700070606     c                             and  DUJopz   <> 'A'
021800070503     c                             and  DUJopz   <> 'D'
021900070417     c                   eval      DUJerr    = *on
022000070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
022100070412     c                   eval      $Fine     = *on
022200070412     c                   leavesr
022300070417w   1c                   when      DUJnum    = *zeros
022400070417     c                   eval      DUJerr    = *on
022500070417     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
022600070412     c                   eval      $Fine     = *on
022700070412     c                   leavesr
022800070423w   1c                   when           DUJopz    = '4'
022900070423     c                             and (DUJnum   <= *zeros
023000070423     c                              or  DUJprg   <= *zeros)
023100070423     c                   eval      DUJerr    = *on
023200070423     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
023300070423     c                   eval      $Fine     = *on
023400070423     c                   leavesr
023500070412e   1c                   endsl
023600070412      *
023700070509      * Reperimento dati del trk "1" del file WFAIM01L
023800070412      *   (SE NON ricevuto come parametro)
023900070412     c                   exsr      Rep_Trk_1
024000070523if  1c***                if             DUJerr   <> *off
024100070523     c***                          or   DUJret   <> *off
024200070523if  1c                   if            (*in28     = *on
024300070523     c                             or   V1Dmsg   <> *blanks)
024400070523     c                             and  DUJopz    = '5'
024500070523     c                   eval      DUJerr    = *on
024600070523     c                   eval      DUJmsg    = V1Dmsg
024700070523     c                   eval      $Fine     = *on
024800070423     c                   leavesr
024900070423e   1c                   endif
025000070606      *
025100070606      * GESTIONE OPZIONI RICEVUTE
025200070606      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
025300070606sel 1c                   SELECT
025400070606      * - 4=Cancellazione singolo record trk. "2"
025500070606      *   (SENZA visualizzare nulla)
025600070606w   1c                   WHEN      DUJopz    = '4'
025700070606     c                   exsr      Dlt_WFAIM_2
025800070606     c                   eval      $Fine     = *on
025900070606     c                   leavesr
026000070606      * - A=Immissione nuovo inserimento multiplo unità
026100070606      *     da sfl già caricato
026200070606w   1c                   WHEN      DUJopz    = 'A'
026300070606     c                   eval      $Video    = 'S1'
026400070606     c                   eval      $InzS01   = *off
026500070607     c                   eval      MAXnrr    = DUJprg
026600070607     c                   eval      C1Cprg    = DUJprg
026700070606     c                   exsr      Upd_Txt_C01
026800070606     c                   exsr      CtrS01
026900070607if  2c                   if        NOT  *in90
027000070607     c                   eval      C1Cprg    = MAXnrr
027100070606     c                   exsr      F06S01
027200070606e   2c                   endif
027300070606e   1c                   ENDSL
027400070412      *
027500070412     c                   ENDSR
027600070412      *
027700070412      *---------------------------------------------------------------*
027800070412      *?Reperimento Dati del job (Utente/Operativi)                  ?*
027900070412      *---------------------------------------------------------------*
028000070412     c     DatiJob       BEGSR
028100070412      *
028200070412     c     *dtaara       define    §azute        azuteds
028300070412     c     *dtaara       define    §datiute      ddatiute
028400070412      *
028500070412     c                   in(E)     *dtaara
028600070412     c                   IF        %ERROR or RSUT = *blanks
028700070412     c                   clear                   Tibs34Ds
028800070412     c                   call      'TIBS34R'
028900070412     c                   parm                    Tibs34Ds
029000070412     c                   in        *dtaara
029100070412     c                   ENDIF
029200070412      *
029300070412     c                   ENDSR
029400070412      *
029500070412      *---------------------------------------------------------------*
029600070509      *?Reperimento Dati dal trk "1" del file WFAIM00F               ?*
029700070412      *---------------------------------------------------------------*
029800070412     c     Rep_Trk_1     BEGSR
029900070412      *
030000070509     c                   clear                   dWIMuni1
030100070412      *
030200070412      * Se ricevuto come parametro (come dovrebbe essere): basta quello
030300130916if  1c                   if        %subst(DUJuni1:1:40) <> *blanks
030400130916     c                   movel     DUJuni1       dWIMuni1
030500130916     c                   movel     DUJuni2       dWIMuni12
030600070412     c                   leavesr
030700070412e   1c                   endif
030800070412      *
030900070509      * Altrimenti: si reperiscono i dati dal trk. "1" del file WFAIM
031000070417      *             (richiamando il pgm. TNSDUJR1)
031100070417     c                   movel     TNSDUJds      SaveU0ds
031200070417     c                   eval      DUJopz    = '0'
031300070417     c                   exsr      Call_TNSDUJ1
031400070417     c                   movel     SaveU0ds      TNSDUJds
031500130923     c                   movel(p)  dWIMuni1      DUJuni1
031600130923     c                   movel(p)  dWIMuni12     DUJuni2
031700070412      *
031800070412     c                   ENDSR
031900070412      *
032000070412      *---------------------------------------------------------------*
032100070412      *?Gestione videata S01                                         ?*
032200070412      *---------------------------------------------------------------*
032300070412     c     GesS01        BEGSR
032400070412      *
032500070412      * Inizializzazione della videata
032600070412if  1c                   if        $InzS01   = *on
032700070412     c                   exsr      InzS01
032800070412     c                   eval      $InzS01   = *off
032900070412e   1c                   endif
033000070412      *
033100070412      * Posizionamento del cursore
033200070412      * => NON nel sfl (se non ci sono errori) ma nel sflctl
033300070412      *
033400070412      * Scrittura di testata e riga tasti funzionali abilitati
033500070417     c                   write     SDUJT01
033600070417     c                   write     SDUJZ01
033700070412      *
033800070412      * Visualizzazione (eventuale) del msg per la mancanza di dati
033900070412if  1c                   if        C01rcd   <= *zeros
034000070417     c                   write     SDUJD00
034100070412e   1c                   endif
034200070412      *
034300070412      * Emissione della videata
034400070417     c                   exfmt     SDUJC01
034500070412if  1c                   if        C01csr    > *zeros
034600070412     c                   eval      C01rcd    = C01csr
034700070412e   1c                   endif
034800070412     c                   setoff                                       28  90
034900070412     c                   clear                   V1Dmsg
035000070412      *
035100070412sel 1c                   SELECT
035200070412      * F3=Fine
035300070412w   1c                   WHEN      *inKC
035400070412     c                   exsr      F03S01
035500070417      * F8=Parametri di controllo (TNSDUJR1)
035600070412w   1c                   WHEN      *inKH
035700070518     c                             and  DUJopz <> '5'
035800070412     c                   exsr      F08S01
035900081031      * F10=Gestione Prefisso/Suffisso Matricola
036000081031w   1c                   WHEN      *inKJ
036100081031     c                   eval      *in46     = *on
036200070412      * F12=Ritorno
036300070412w   1c                   WHEN      *inKL
036400070412     c                   exsr      F12S01
036500070518      * Richiesta SOLA visualizzazione
036600070620w   1c                   WHEN      DUJopz    = '5'
036700070412      * Invio / F6=Conferma
036800070412x   1c                   OTHER
036900081031      * - Controllo Prefisso/Suffisso della Matricola
037000081103     c                   eval      *in51     = *off
037100081103     c                   eval      *in52     = *off
037200081031sel 2c                   select
037300081031w   2c                   when      C1Cpsf    = *blanks
037400081031     c                   clear                   C1Cpsc
037500081031w   2c                   when            C1Cpsf   <> 'P'
037600081031     c                             and   C1Cpsf   <> 'S'
037700081031     c                   seton                                        285190
037800081031     c                   eval      V1Dmsg    = $Msg(15)
037900081031     c                   leavesr
038000081031w   2c                   when      C1Cpsc    = *blanks
038100081031     c                   seton                                        285290
038200081031     c                   if        C1Cpsf    = 'P'
038300081103     c                   eval      V1Dmsg    = %trimr( $Msg(16) )
038400081031     c                                       + ' prefisso'
038500081031     c                   else
038600081103     c                   eval      V1Dmsg    = %trimr( $Msg(16) )
038700081031     c                                       + ' suffisso'
038800081031     c                   endif
038900081031     c                   leavesr
039000081031e   2c                   endsl
039100081103     c                   eval      *in46     = *off
039200070412      * - Gestione opzioni inserite
039300070518if  2c                   if        S01nrr    > *zeros
039400070412     c                   exsr      OpzS01
039500070412e   2c                   endif
039600070412      * - Controllo dati inseriti nel subfile-control
039700070412      *   (solo per evidenziarne eventuali anomalie)
039800070412if  2c                   IF        C1Cmat   <> *blanks
039900070412     c                   exsr      CtrC01
040000070412      * - Inserimento record nel subfile
040100070412if  3c                   if        wErr      = *zeros
040200070412     c                   exsr      WrtS01
040300070412e   3c                   endif
040400070412     c                   clear                   C1Cmat
040500070412e   2c                   ENDIF
040600070412      * - Riconteggio delle matricole confermabili inserite
040700070412     c                   eval      C1Cprg    = MAXnrr - %xfoot($MAnn)
040800070412      * - F6=Conferma
040900070412if  2c                   If        *inKF  and  NOT *in90
041000070412     c                   exsr      CtrS01
041100070412if  3c                   if        *in90
041200070412     c                   leavesr
041300070412e   3c                   endif
041400070412     c                   exsr      F06S01
041500070412e   2c                   EndIf
041600070412e   1c                   ENDSL
041700070412      *
041800070412     c                   ENDSR
041900070412      *
042000070412      *---------------------------------------------------------------*
042100070412      *?Inizializzazione videata S01                                 ?*
042200070412      *---------------------------------------------------------------*
042300070412     c     InzS01        BEGSR
042400070412      *
042500070412      * "Intestazione" del subfile-control
042600070509     c                   clear                   SDUJC01
042700070509     c                   exsr      Upd_Txt_C01
042800070412      *
042900070412      * Pulizia subfile
043000070412     c                   seton                                        3031
043100070417     c                   write     SDUJC01
043200070412     c                   setoff                                         3133
043300070412      *
043400070412     c                   reset                   $EoF
043500070518     c***                clear                   W_SflPag
043600070412     c***                clear                   C01csr
043700070412     c                   clear                   C01rcd
043800070412     c                   clear                   S01nrr
043900070412     c                   clear                   V1Dmsg
044000070412     c                   setoff                                       28  90
044100081031     c                   eval      *in46     = *off
044200070412     c                   movea     *zeros        *in(50)
044300070412      *
044400070412      * Caricamento eventuali dati da presentare nel sfl
044500070412     c                   clear                   $MAnn
044600070412     c                   clear                   $Matr
044700070412     c                   clear                   MAXnrr
044800070620     c                   clear                   WrecTrk2
044900070620     c                   clear                   WrecTrk4
045000070509     c                   eval      WIMnum    = DUJnum
045100070509     c                   eval      WIMtrk    = '2'
045200070509     c     K02WIM01      setll     WFAIM000
045300070509     c     K02WIM01      reade(n)  WFAIM000
045400070509     c                   eval      $EoF      = (%eof(WFAIM01L))
045500070412     c                   exsr      RollUpS01
045600070620      *
045700070620      * Conteggio dei records di dettaglio già inseriti (trk. "2")
045800070620     c                   eval      WrecTrk2  = S01nrr
045900070518      *
046000070518      * Se "visualizzazione": si caricano anche le matricole già
046100070518      *                       confermate
046200070518     c                   if        DUJopz   <> '5'
046300070518     c                   leavesr
046400070518     c                   endif
046500070518      *
046600070518     c                   reset                   $EoF
046700070518     c                   eval      WIMnum    = DUJnum
046800070518     c                   eval      WIMtrk    = '4'
046900070518     c     K02WIM01      setll     WFAIM000
047000070518     c     K02WIM01      reade(n)  WFAIM000
047100070518     c                   eval      $EoF      = (%eof(WFAIM01L))
047200070518     c                   exsr      RollUpS01
047300070521      *
047400070521      * Conteggio dei records di dettaglio già confermati (trk. "4")
047500070620     c                   eval      WrecTrk4  = S01nrr - WrecTrk2
047600070412      *
047700070412     c                   ENDSR
047800070412      *
047900070412      *---------------------------------------------------------------*
048000070412      *?Caricamento pagina successiva di dati S01                    ?*
048100070412      *---------------------------------------------------------------*
048200070412     c     RollUpS01     BEGSR
048300070412      *
048400070518     c***                eval      S01nrr    = (W_SflPag * C_SflPag)
048500070518     c***                add       1             W_SflPag
048600070412     c                   eval      *in32     = *off
048700070412      *
048800070412      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
048900070412do  1c                   doW            $EoF   = *off
049000070412     c*** carica tutto:            and  S01nrr < W_SflPag * C_SflPag
049100070412     c                   exsr      CarS01
049200070509     c     K02WIM01      reade     WFAIM000
049300070509     c                   eval      $EoF      = (%eof(WFAIM01L))
049400070412e   1c                   enddo
049500070412      *
049600070412      * Visualizzazione totale unità già inserite
049700070412     c                   eval      C1Cprg    = MAXnrr
049800070412      *
049900070412      * Visualizzazione del SFL (se ci sono dati)
050000070412     c                   eval      *in30     = (S01nrr = *zeros)
050100070412      *
050200070412      * Attivazione (eventuale) del SFLEND
050300070412     c                   eval      *in33     = $EoF
050400070412      *
050500070412      * Posizionamento cursore al 1° rcd della pagina
050600070412if  1c                   if        S01nrr    > *zeros
050700070412     c     S01nrr        div       C_SflPag      wPag
050800070412     c                   mvr                     wRig
050900070412     c                   eval      C01rcd    = wPag * C_SflPag
051000070412     c                   if        wRig      > *zeros
051100070412     c                   eval      C01rcd    = C01rcd + 1
051200070412     c                   else
051300070412     c                   eval      C01rcd    = C01rcd - C_SflPag + 1
051400070412     c                   endif
051500070412x   1c                   else
051600070412     c                   clear                   C01rcd
051700070412e   1c                   endif
051800070412      *
051900070412     c                   ENDSR
052000070412      *
052100070412      *---------------------------------------------------------------*
052200070509      *?Caricamento dati da file WFAIM00F in subfile S01             ?*
052300070412      *---------------------------------------------------------------*
052400070412     c     CarS01        BEGSR
052500070412      *
052600070417     c                   clear                   SDUJS01
052700070412      *
052800070412      * Controlli preliminari:
052900070412      * SE matricola già inserita => uscita (NO caricamento record)
053000070412     c                   clear                   wErr
053100070509     c                   eval      wMatr     = WIMuni
053200070412     c                   exsr      CtrlMAT_1
053300070521if  1c                   if             wErr    > *zeros
053400070521     c                             and  DUJopz <> '5'
053500070412     c                   leavesr
053600070412e   1c                   endif
053700070412      * ALTRIMENTI => gestione codice matricola - anche se anomalo
053800070412      *
053900070412      * Impostazione campi del subfile
054000070509     c                   eval      H1Cprg    = WIMprg
054100070509     c                   eval      S1Cmat    = WIMuni
054200070521if  1c                   if        DUJopz   <> '5'
054300070412     c                   eval      S1Cprg    = MAXnrr + 1
054400070521x   1c                   else
054500070521     c                   eval      S1Cprg    = WIMprg
054600070521e   1c                   endif
054700070518if  1c                   if        WIMtrk    > '2'
054800070518     c                   eval      S1Dnot    = 'Confermato'
054900070518e   1c                   endif
055000070518      *
055100070518      * Impostazione indicatori
055200070518     c                   eval      *in45     = (DUJopz = '5')
055300070518     c                   eval      *in41     = (WIMtrk > '2')
055400070412      *
055500070412      * Scrittura record subfile
055600070412     c                   add       1             S01nrr
055700070417     c                   write     SDUJS01
055800070412     c                   eval      MAXnrr    = S01nrr
055900070412      *
056000070412      * Intabellamento codice matricola appena inserito
056100070412     c                   eval      $Matr(S01nrr) = wMatr
056200070412      *
056300070412     c                   ENDSR
056400070412      *
056500070412      *---------------------------------------------------------------*
056600070412      *?Controllo dati in subfile control C01                        ?*
056700070412      *---------------------------------------------------------------*
056800070412     c     CtrC01        BEGSR
056900070412      *
057000070412      * Controllo matricola:
057100070412      * SE matricola già inserita => uscita (NO caricamento record)
057200070412     c                   clear                   wErr
057300081031sel 1c                   select
057400081031w   1c                   when      C1Cpsf    = *blanks
057500070412     c                   eval      wMatr     = C1Cmat
057600081031w   1c                   when      C1Cpsf    = 'P'
057700081031     c                   eval      wMatr     = %trimr(C1Cpsc)
057800081031     c                                       + %trimr(C1Cmat)
057900081031w   1c                   when      C1Cpsf    = 'S'
058000081031     c                   eval      wMatr     = %trimr(C1Cmat)
058100081031     c                                       + %trimr(C1Cpsc)
058200081031x   1c                   other
058300081031     c                   eval      wMatr     = C1Cmat
058400081031e   1c                   endsl
058500070412     c                   exsr      CtrlMAT_1
058600070412if  1c                   if        wErr      > *zeros
058700070412     c                   leavesr
058800070412e   1c                   endif
058900070412      *
059000070412      * ALTRIMENTI => gestione codice matricola - anche se anomalo...
059100070412      *               (in seguito...)
059200070412      *
059300070412     c                   ENDSR
059400070412      *
059500070412      *---------------------------------------------------------------*
059600070412      *?Caricamento dati appena inseriti da C01 in subfile S01       ?*
059700070412      *---------------------------------------------------------------*
059800070412     c     WrtS01        BEGSR
059900070412      *
060000070417     c                   clear                   SDUJS01
060100070412      *
060200070412      * Impostazione campi
060300070412     c*** già così:      clear                   H1Cprg
060400081031     c*** Pref/Suff ?    eval      S1Cmat    = C1Cmat
060500081031     c                   eval      S1Cmat    = wMatr
060600070412     c                   eval      S1Cprg    = MAXnrr + 1
060700070412      *
060800070412      * Scrittura record subfile
060900070412      * · Meglio "ripristinare" S01NRR: potrebbe essere impostato a
061000070412      *   quello di un subfile-record appena annullato/ripristinato...
061100070412     c                   eval      S01nrr    = MAXnrr
061200070412     c                   add       1             S01nrr
061300070417     c                   write     SDUJS01
061400070412     c                   eval      MAXnrr    = S01nrr
061500070412      *
061600070412      * Intabellamento codice matricola
061700070412     c                   eval      $Matr(S01nrr) = wMatr
061800070412      *
061900070412      * Visualizzazione del SFL (...visto che ci sono dati!)
062000070412     c***                eval      *in30     = (S01nrr = *zeros)
062100070412     c                   eval      *in30     = *off
062200070412      *
062300070412      * Attivazione (eventuale) del SFLEND
062400070412     c*** (sempre *on)   eval      *in33     = $EoF
062500070412      *
062600070412      * Posizionamento cursore al 1° rcd della pagina
062700070412if  1c                   if        S01nrr    > *zeros
062800070412     c     S01nrr        div       C_SflPag      wPag
062900070412     c                   mvr                     wRig
063000070412     c                   eval      C01rcd    = wPag * C_SflPag
063100070412     c                   if        wRig      > *zeros
063200070412     c                   eval      C01rcd    = C01rcd + 1
063300070412     c                   else
063400070412     c                   eval      C01rcd    = C01rcd - C_SflPag + 1
063500070412     c                   endif
063600070412x   1c                   else
063700070412     c                   clear                   C01rcd
063800070412e   1c                   endif
063900070412      *
064000070412     c                   ENDSR
064100070412      *
064200070412      *---------------------------------------------------------------*
064300070412      *?Gestione opzioni subfile S01                                 ?*
064400070412      *---------------------------------------------------------------*
064500070412     c     OpzS01        BEGSR
064600070509      *
064700070509      * "A" di annullamento / "F" di forzatura rimangono impostate nel
064800070509      * subfile; non serve fare altro, perchè in fase di conferma verrà
064900070509      * gestita questa opzione:
065000070509      * x "A" il barcode in esame NON verrà riportato in WFAIM00F;
065100070509      * x "F" il barcode in esame verrà riportato in WFAIM00F, anche
065200070509      *       se formalmente scorretto.?=> "F" NON PIÙ GESTITA !!!?
065300070412      *
065400070412      * Ciclo di lettura del subfile
065500070417     c                   readc     SDUJS01
065600070412      *
065700070417do  1c                   DOW       NOT %eof(TNSDUJD2)
065800070412      *
065900070412     c                   eval      *in32     = *off
066000070412     c                   movea     *zeros        *in(50)
066100070412     c                   z-add     S01nrr        C01rcd
066200070412      *
066300070412sel 2c                   SELECT
066400070412      * - Nessuna opzione
066500070412w   2c                   WHEN      S1Copz    = *blanks
066600070412     c                   clear                   $MAnn(S01nrr)
066700070412      * - Opz. A=Annullamento
066800070412w   2c                   WHEN      S1Copz    = 'A'
066900070412     c                   z-add     1             $MAnn(S01nrr)
067000070412      * - Opz. F=Forzatura
067100070509w   2c***                WHEN      S1Copz    = 'F'
067200070509     c***                clear                   $MAnn(S01nrr)
067300070412      * - Opz. errata
067400070412x   2c                   OTHER
067500070412     c                   seton                                        285090
067600070412     c                   eval      V1Dmsg    = $Msg( 1)
067700070412e   2c                   ENDSL
067800070412      *
067900070412      * > Aggiornamento sfl (senza?MAI?pulire il campo opzione)
068000070412sel 2c                   select
068100070412w   2c                   when      *in28
068200070412     c                   eval      *in32     = *on
068300070412w   2c                   when      *in90     = *on
068400070412e   2c                   endsl
068500070417     c                   update    SDUJS01
068600070412if  2c                   if        *in28  OR  *in90
068700070412     c                   leave
068800070412e   2c                   endif
068900070412      *
069000070417     c                   readc     SDUJS01
069100070412      *
069200070412e   1c                   ENDDO
069300070412      *
069400070412      * Ripristino del numero relativo di record già raggiunto nel sfl
069500070412     c*** non qui:       eval      S01nrr    = MAXnrr
069600070412      *
069700070412     c                   ENDSR
069800070412      *
069900070412      *---------------------------------------------------------------*
070000070412      *?Controllo dati in TUTTI i records del subfile S01 (per F6)   ?*
070100070412      *---------------------------------------------------------------*
070200070412     c     CtrS01        BEGSR
070300070412      *
070400070412     c                   movea     *zeros        *in(50)
070500070412      *
070600070412      * Ciclo di lettura subfile S01
070700070412do  1c                   DO        MAXnrr        S01nrr
070800070412      *
070900070417     c     S01nrr        chain     SDUJS01
071000070417if  2c                   if        NOT %found(TNSDUJD2)
071100070606     c                   eval      MAXnrr    = S01nrr - 1
071200070412     c                   leave
071300070412e   2c                   endif
071400070412      *
071500070412      * - Elaborazione dei soli records NON "annullati"
071600070412if  2c                   if        S1Copz    = 'A'
071700070412     c                   eval      $MAnn(S01nrr) = 1
071800070412     c                   iter
071900070412e   2c                   endif
072000070412     c                   clear                   $MAnn(S01nrr)
072100070412      *
072200070509      * - Controllo barcode -SE?NON?chiesta "forzatura"
072300070412     c                   clear                   wErr
072400070518if  2c***                IF        S1Copz   <> 'F'
072500070412     c                   eval      wMatr     = S1Cmat
072600070412     c                   exsr      CtrlMAT_2
072700070412     c                   eval      *in28     = (wErr > *zero)
072800070518e   2c***                ENDIF
072900070412      *
073000070412      * > Emissione eventuale messaggio di errore
073100070412      *   (senza MAI pulire il campo opzione)
073200070412if  2c                   IF        *in28
073300070412     c                   seton                                        50  90
073400070412     c                   eval      *in32     = *on
073500070412if  3c                   if        wErr      > *zeros
073600070412sel 4c                   select
073700070413w   4c                   when      wErr      = 3
073800070412     c                   eval      V1Dmsg    = %replace(
073900070509     c                                           %editc(§WIM1lm1:'Z'):
074000070413     c                                           $Msg( 3):
074100070413     c                                           %scan('&N':$Msg( 3)) )
074200070413w   4c                   when      wErr      = 5
074300070412     c                   eval      V1Dmsg    = %trimr($Msg(wErr))
074400070412     c                                       + %editc(SQLnum:'Z')
074500070412x   4c                   other
074600070412     c                   eval      V1Dmsg    = $Msg(wErr)
074700070412e   4c                   endsl
074800070412e   3c                   endif
074900070412     c                   z-add     S01nrr        C01rcd
075000070412e   2c                   ENDIF
075100070412      *
075200070417     c                   update    SDUJS01
075300070412if  2c                   if        *in28  OR  *in90
075400070412     c                   leave
075500070412e   2c                   endif
075600070412      *
075700070412e   1c                   ENDDO
075800070412      *
075900070412     c                   eval      C1Cprg    = MAXnrr - %xfoot($MAnn)
076000070412if  1c                   if        *in28  OR  *in90
076100070412     c                   leavesr
076200070412e   1c                   endif
076300070412      *
076400070412      * Verifica se tot. matric. da inserire <> n° matric. confermate
076500070521      *   (conteggiando anche quelle eventualmente già confermate -
076600070521      *    vedi trk. "3"/"4")
076700070521if  1c***                if        §WIM1nmi <> C1Cprg
076800070521if  1c                   if        §WIM1nmi <> (C1Cprg + WrecTrk4)
076900070412     c                   eval      *in90     = *on
077000070413     c                   eval      wErr      = 6
077100070412     c                   eval      $Video    = 'W1'
077200070412     c                   eval      $InzW01   = *on
077300070412e   1c                   endif
077400070412      *
077500070412     c                   ENDSR
077600070412      *
077700070412      *---------------------------------------------------------------*
077800070412      *?Gestione tasto funzionale F03 da videata S01                 ?*
077900070412      *---------------------------------------------------------------*
078000070412     c     F03S01        BEGSR
078100070620      *
078200070620     c                   clear                   WWWprg
078300070620if  1c                   if        WrecTrk2  > *zeros
078400070620     c                   eval      WWWprg    = %xfoot( %subarr(
078500070620     c                                           $MAnn:1:WrecTrk2 ))
078600070620e   1c                   endif
078700070412      *
078800070412      * Chiusura del programma
078900070620if  1c                   if             DUJopz  = '4'
079000070620     c                             or   DUJopz  = '5'
079100070620     c                             or  (C1Cprg  = WrecTrk2
079200070620     c                              and WWWprg  = *zeros)
079300070620     c                             or   $Video  = 'W1'
079400070620      *
079500070417     c                   eval      DUJret    = '1'
079600070412     c                   eval      $Fine     = *on
079700070620      *
079800070620x   1c                   else
079900070620      *
080000070620      * ...chiedendone PRIMA la conferma
080100070620     c                   eval      *in90     = *on
080200070620     c                   eval      wErr      = -03
080300070620     c                   eval      $Video    = 'W1'
080400070620     c                   eval      $InzW01   = *on
080500070620      *
080600070620e   1c                   endif
080700070412      *
080800070412     c                   ENDSR
080900070412      *
081000070412      *---------------------------------------------------------------*
081100070423      *?Gestione tasto funzionale F06 da videata S01                 ?*
081200070412      *---------------------------------------------------------------*
081300070412     c     F06S01        BEGSR
081400070412      *
081500070517      * Ciclo di cancellazione degli eventuali record "2" pre-esistenti
081600070509     c                   eval      WIMnum    = DUJnum
081700070509     c                   eval      WIMtrk    = '2'
081800070509     c                   exsr      Dlt_WFAIM
081900070412      *
082000070412      * Ciclo di lettura subfile S01
082100070412     c                   clear                   WWWprg
082200070412do  1c                   DO        MAXnrr        S01nrr
082300070412      *
082400070417     c     S01nrr        chain     SDUJS01
082500070417if  2c                   if        NOT %found(TNSDUJD2)
082600070412     c                   leave
082700070412e   2c                   endif
082800070412      *
082900070412      * - Registrazione dei soli records NON annullati
083000070412if  2c                   if        S1Copz   <> 'A'
083100070509     c                   exsr      Wrt_WFAIM
083200070412e   2c                   endif
083300070412      *
083400070412e   1c                   ENDDO
083500070412      *
083600070412      * Chiusura del programma
083700070412     c                   eval      $Fine     = *on
083800070412      *
083900070412     c                   ENDSR
084000070412      *
084100070412      *---------------------------------------------------------------*
084200070412      *?Gestione tasto funzionale F08 da videata S01                 ?*
084300070412      *---------------------------------------------------------------*
084400070412     c     F08S01        BEGSR
084500070412      *
084600070412      * "Salvataggio" parametri ricevuti
084700070417     c                   movel     TNSDUJds      SaveU0ds
084800070412      *
084900070417      * Richiamo pgm. TNSDUJR1
085000070417     c                   eval      DUJopz    = '2'
085100070417     c                   exsr      Call_TNSDUJ1
085200070413      *
085300070417if  1c                   if        DUJret    = '1'
085400070413     c                   eval      $Fine     = *on
085500070413e   1c                   endif
085600070412      *
085700070412      * "Ripristino" parametri ricevuti
085800070417     c                   movel     SaveU0ds      TNSDUJds
085900070413if  1c                   if        $Fine     = *on
086000070417     c                   eval      DUJret    = '2'
086100070413e   1c                   endif
086200070412      *
086300070412     c                   ENDSR
086400070412      *
086500070412      *---------------------------------------------------------------*
086600070412      *?Gestione tasto funzionale F12 da videata S01                 ?*
086700070412      *---------------------------------------------------------------*
086800070412     c     F12S01        BEGSR
086900070620      *
087000070620     c                   clear                   WWWprg
087100070620if  1c                   if        WrecTrk2  > *zeros
087200070620     c                   eval      WWWprg    = %xfoot( %subarr(
087300070620     c                                           $MAnn:1:WrecTrk2 ))
087400070620e   1c                   endif
087500070412      *
087600070412      * Chiusura del programma
087700070620if  1c                   if             DUJopz  = '4'
087800070620     c                             or   DUJopz  = '5'
087900070620     c                             or  (C1Cprg  = WrecTrk2
088000070620     c                              and WWWprg  = *zeros)
088100070620     c                             or   $Video  = 'W1'
088200070620      *
088300070417     c                   eval      DUJret    = '2'
088400070412     c                   eval      $Fine     = *on
088500070620      *
088600070620x   1c                   else
088700070620      *
088800070620      * ...chiedendone PRIMA la conferma
088900070620     c                   eval      *in90     = *on
089000070620     c                   eval      wErr      = -12
089100070620     c                   eval      $Video    = 'W1'
089200070620     c                   eval      $InzW01   = *on
089300070620      *
089400070620e   1c                   endif
089500070412      *
089600070412     c                   ENDSR
089700070412      *
089800070412      *---------------------------------------------------------------*
089900070412      *?Gestione videata W01                                         ?*
090000070412      *---------------------------------------------------------------*
090100070412     c     GesW01        BEGSR
090200070412      *
090300070412      * Inizializzazione della videata
090400070412     c                   if        $InzW01   = *on
090500070412     c                   exsr      InzW01
090600070412     c                   eval      $InzW01   = *off
090700070412     c                   endif
090800070412      *
090900070412      * Emissione della window
091000070417     c                   exfmt     SDUJW01
091100070412      *
091200070412sel 1c                   select
091300070412      * F12 = Ritorno
091400070412w   1c                   when      *inKL
091500070620     c                   eval      $Video    = 'S1'
091600070412      * F6 = Conferma
091700070412w   1c                   when      *inKF
091800070620sel 2c                   select
091900070620w   2c                   when      wErr      > *zeros
092000070412     c                   eval      $Video    = 'S1'
092100070412     c                   exsr      F06S01
092200070620w   2c                   when      wErr      = -03
092300070620     c                   exsr      F03S01
092400070620w   2c                   when      wErr      = -12
092500070620     c                   exsr      F12S01
092600070620e   2c                   endsl
092700070412e   1c                   endsl
092800070412      *
092900070412     c                   ENDSR
093000070412      *
093100070412      *---------------------------------------------------------------*
093200070412      *?Inizializzazione videata W01                                 ?*
093300070412      *---------------------------------------------------------------*
093400070412     c     InzW01        BEGSR
093500070412      *
093600070417     c                   clear                   SDUJW01
093700070412      *
093800070412sel 1c                   SELECT
093900070620      * ----------------
094000070620      * Completamento messaggio  6/ 7/ 8:
094100070620      *   richiede numeri di 4/0: si utilizza?temporaneamente?S01nrr !
094200070413w   1c                   WHEN      wErr      = 6
094300070412     c                   eval      S01nrr    = C1Cprg
094400070412     c                   eval      W1Ctx1    = %replace(
094500070412     c                                           %editc(S01nrr:'3'):
094600070412     c                                           $Msg(wErr):
094700070412     c                                           %scan('&NMI':
094800070412     c                                                 $Msg(wErr) ))
094900070412     c                   eval      W1Ctx2    = %replace(
095000070509     c                                           %editc(§WIM1nmi:'3'):
095100070412     c                                           $Msg(wErr+1):
095200070412     c                                           %scan('&NMP':
095300070412     c                                                 $Msg(wErr+1) ))
095400070412     c                   eval      W1Ctx3    = $Msg(wErr+2)
095500070412     c                   eval      S01nrr    = MAXnrr
095600070620      * ----------------
095700070620      * Completamento messaggio 11:
095800070620      *   richiede numeri di 4/0: si utilizza?temporaneamente?S01nrr !
095900070620w   1c                   WHEN           wErr    < *zeros
096000070620     c                             and (*inKC   = *on
096100070620     c                              or  *inKL   = *on)
096200070620     c                   clear                   WWWprg
096300070620if  2c                   if        WrecTrk2  > *zeros
096400070620     c                   eval      WWWprg    = %xfoot( %subarr(
096500070620     c                                           $MAnn:1:WrecTrk2 ))
096600070620e   2c                   endif
096700070620sel 2c                   Select
096800070620      * - Annullati records già presenti
096900070620w   2c                   When      WWWprg  > *zeros
097000070620     c                   eval      S01nrr    = WWWprg
097100070620     c                   eval      W1Ctx1    = %replace(
097200070620     c                                           %editc(S01nrr:'3'):
097300070620     c                                           $Msg(10):
097400070620     c                                           %scan('&NMA':
097500070620     c                                                 $Msg(10) ))
097600070620      * - - ...ed inserite altre matricole (NON annullate)
097700070620if  3c                   if        *zeros    < MAXnrr - WrecTrk2
097800070620     c                                       - %xfoot( %subarr(
097900070620     c                                           $MAnn:WrecTrk2+1 ))
098000070620     c                   eval      S01nrr    = MAXnrr - WrecTrk2
098100070620     c                                       - %xfoot( %subarr(
098200070620     c                                           $MAnn:WrecTrk2+1 ))
098300070620     c                   eval      W1Ctx2    = %replace(
098400070620     c                                           %editc(S01nrr:'3'):
098500070620     c                                           $Msg(11):
098600070620     c                                           %scan('&NMI':
098700070620     c                                                 $Msg(11) ))
098800070620e   3c                   endif
098900070620      * - Inserita 1 matricola
099000070620w   2c                   When      1         = C1Cprg - WrecTrk2
099100070620     c                   eval      W1Ctx1    = $Msg(12)
099200070620      * - Inserita altre matricole
099300070620x   2c                   Other
099400070620     c                   eval      S01nrr    = C1Cprg - WrecTrk2
099500070620     c                   eval      W1Ctx1    = %replace(
099600070620     c                                           %editc(S01nrr:'3'):
099700070620     c                                           $Msg(13):
099800070620     c                                           %scan('&NMI':
099900070620     c                                                 $Msg(13) ))
100000070620e   2c                   EndSl
100100070620if  2c                   if        W1Ctx2    = *blanks
100200070620     c                   eval      W1Ctx2    = $Msg(14)
100300070620x   2c                   else
100400070620     c                   eval      W1Ctx3    = $Msg(14)
100500070620e   2c                   endif
100600070620     c                   eval      S01nrr    = MAXnrr
100700070620      * ----------------
100800070412e   1c                   ENDSL
100900070412      *
101000070412     c                   ENDSR
101100070412      *
101200070412      *---------------------------------------------------------------*
101300070509      *?Richiamo del pgm. TNSDUJR1 (gestione trk. "1" file WFAIM00F) ?*
101400070412      *---------------------------------------------------------------*
101500070417     c     Call_TNSDUJ1  BEGSR
101600070412      *
101700070417     c*** già impostato: eval      DUJopz    = ?
101800070412      *
101900070417      * Richiamo pgm. TNSDUJR1
102000070417     c                   movel(p)  TNSDUJds      KPJBU
102100070417     c                   call      'TNSDUJR1'
102200070412     c                   parm                    KPJBA
102300070417     c                   movel     KPJBU         TNSDUJds
102400070412      *
102500070412sel 1c                   SELECT
102600070412      * Ritorno normale
102700070417w   1c                   WHEN           DUJret  = *off
102800070417     c                             and  DUJerr  = *off
102900070509     c                   exsr      Upd_Txt_C01
103000070412      * Ritorno con errore
103100070417w   1c                   WHEN      DUJerr    = *on
103200070412     c                   seton                                        28  90
103300070417     c                   eval      V1Dmsg    = DUJmsg
103400070412      * Ritorno con F3
103500070620      * (forzo "F12" per non far chiudere anche TNSDUJR e tornare a
103600070620      *  menù)
103700070417w   1c                   WHEN      DUJret    = '1'
103800070412     c                   eval      $Fine     = *on
103900070412      * Ritorno con F12
104000070417w   1c                   WHEN      DUJret    = '2'
104100070412      *
104200070412e   1c                   ENDSL
104300070412      *
104400070412     c                   ENDSR
104500070509      *
104600070509      *---------------------------------------------------------------*
104700070509      *?Aggiornamento testata in C01 con nuovi dati restituiti da R1 ?*
104800070509      *---------------------------------------------------------------*
104900070509     c     Upd_Txt_C01   BEGSR
105000070509      *
105100130916     c                   movel     DUJuni1       dWIMuni1
105200130916     c                   movel     DUJuni2       dWIMuni12
105300070509      *
105400070509     c                   eval      C1Dnum    = DUJnum
105500130916     c                   eval      C1Ddes    = §WIM1des + §WIM12des
105600070509     c                   eval      C1Dnmi    = §WIM1nmi
105700070518      *
105800070518     c                   eval      *in45     = (DUJopz = '5')
105900070509      *
106000070509     c                   ENDSR
106100070412      *
106200070412      *---------------------------------------------------------------*
106300070412      *?Controllo singolo codice matricola inserito (1)              ?*
106400070412      *---------------------------------------------------------------*
106500070412     c     CtrlMAT_1     BEGSR
106600070412      *
106700070412      *?Matricola (estesa) già inserita nel subfile?
106800070412     c     wMatr         lookup    $Matr                                  25
106900070412     c                   if        *in25
107000070413     c                   eval      wErr      = 2
107100070412     c                   leavesr
107200070412     c                   endif
107300070412      *
107400070412     c                   ENDSR
107500070412      *
107600070412      *---------------------------------------------------------------*
107700070412      *?Controllo singolo codice matricola inserito (2)              ?*
107800070412      *---------------------------------------------------------------*
107900070412     c     CtrlMAT_2     BEGSR
108000070412      *
108100070412      * Lunghezza matricola errata
108200070509     c                   if             §WIM1lm1 > *zeros
108300070509     c                             and  %len(%trimr(wMatr)) <> §WIM1lm1
108400070413     c                   eval      wErr      = 3
108500070412     c                   leavesr
108600070412     c                   endif
108700070412      *
108800070412      * Matricola (sottostringa selezionata) già inserita nel subfile
108900070412     c                   eval      yy        = 1
109000070412do  1c                   DOW       yy        < S01nrr
109100070412sel 2c                   select
109200070412w   2c                   when      $Matr(yy) = *blanks
109300070412     c                   leave
109400070412w   2c                   when      $MAnn(yy) > *zeros
109500070509w   2c                   when      %subst(wMatr    :§WIM1im2:§WIM1lm2) =
109600070509     c                             %subst($Matr(yy):§WIM1im2:§WIM1lm2)
109700070413     c                   eval      wErr      = 4
109800070412     c                   leavesr
109900070412e   2c                   endsl
110000070412     c                   add       1             yy
110100070412e   1c                   ENDDO
110200070412      *
110300070509      * Matricola (sottostringa selez.) già inserita nel file WFAIM00F
110400070412     c                   eval      wMatr§    = '%' +
110500070509     c                                  %subst(wMatr:§WIM1im2:§WIM1lm2)
110600070412     c                                       + '%'
110700070412     c                   exsr      OpenCursor
110800070412     c                   exsr      ReadCursor
110900070412     c                   exsr      CloseCursor
111000070412if  1c                   if        $Found    = *on
111100070413     c                   eval      wErr      = 5
111200070412     c                   leavesr
111300070412e   1c                   endif
111400070412      *
111500070412     c                   ENDSR
111600070412      *
111700070412      *****************************************************************
111800070412      **?Apertura cursore.                                          ?**
111900070412      *****************************************************************
112000070412     c     OpenCursor    BEGSR
112100070412      *
112200070412     c/exec sql
112300070412     c+    declare A1 cursor for
112400070412     c+    select  *
112500070509     c+    from    WFAIM00F
112600070509     c+    where   WIMtrk =    '2'
112700070509     c+      and   WIMnum <>   :WIMnum
112800070509     c+      and   WIMuni like :wMatr§
112900070412     c+    for     fetch  only
113000070412     c/end-exec
113100070412      *
113200070412     c/exec sql
113300070412     c+    open    A1
113400070412     c/end-exec
113500070412      *
113600070412     c                   ENDSR
113700070412      *
113800070412      *****************************************************************
113900070412      **?Lettura cursore.                                           ?**
114000070412      *****************************************************************
114100070412     c     ReadCursor    BEGSR
114200070412      *
114300070509     c                   clear                   WFAIMds
114400070412      *
114500070412     c/exec sql
114600070509     c+    fetch   next from A1 into :WFAIMds
114700070412     c/end-exec
114800070412      *
114900070412sel 1c                   select
115000070412w   1c                   when      SQLcod = 100
115100070412     c                   eval      $Found = *off
115200070412w   1c                   when      SQLcod < 0
115300070412     c                   eval      $Err   = *on
115400070412     c                   eval      $Found = *off
115500070412x   1c                   other
115600070412     c                   eval      $Found = *on
115700070412e   1c                   endsl
115800070412      *
115900070412     c                   ENDSR
116000070412      *
116100070412      *****************************************************************
116200070412      **?Chiusura cursore.                                          ?**
116300070412      *****************************************************************
116400070412     c     CloseCursor   BEGSR
116500070412      *
116600070412     c/exec sql
116700070412     c+    close   A1
116800070412     c/end-exec
116900070412      *
117000070412     c                   ENDSR
117100070423      *
117200070423      *---------------------------------------------------------------*
117300070423      *?Cancellazione sing. record di un "inserimento multiplo unità"?*
117400070423      *---------------------------------------------------------------*
117500070509     c     Dlt_WFAIM_2   BEGSR
117600070423      *
117700070517      * Cancellazione "logica" di un singolo rec. trk "2"
117800070509     c                   eval      WIMnum    = DUJnum
117900070509     c                   eval      WIMtrk    = '2'
118000070509     c                   eval      WIMprg    = DUJprg
118100070509     c     K03WIM01      chain     WFAIM000
118200070509do  1c                   if        %found(WFAIM01L)
118300070517      *
118400070517     c                   eval      WIMtrk    = '4'
118500070423      *                  __________________
118600070517     c                   UPDATE    WFAIM000
118700070423      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
118800070507x   1c                   else
118900070507     c                   eval      DUJerr  = *on
119000070507     c                   eval      DUJmsg    = %trimr( %replace(
119100070507     c                                           %editc(DUJprg:'Z'):
119200070507     c                                           $Msg( 9):
119300070507     c                                           %scan('&PRGR':
119400070507     c                                                 $Msg( 9)) ))
119500070507     c                                       + %editc(DUJprg:'K')
119600070423e   1c                   endif
119700070423      *
119800070423     c                   ENDSR
119900070412      *
120000070412      *---------------------------------------------------------------*
120100070412      *?Cancellazione dettaglio di un "inserimento multiplo unità"   ?*
120200070412      *---------------------------------------------------------------*
120300070509     c     Dlt_WFAIM     BEGSR
120400070412      *
120500070412      * Ciclo di cancellazione degli eventuali rec. trk "2" esistenti
120600070412      *   (con chiavi di lettura già impostate)
120700070509     c     K02WIM01      setll     WFAIM000
120800070509     c     K02WIM01      reade     WFAIM000
120900070509do  1c                   DOW       NOT  %eof(WFAIM01L)
121000070412      *                  __________________
121100070509     c                   DELETE    WFAIM000
121200070412      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
121300070509     c     K02WIM01      reade     WFAIM000
121400070412e   1c                   ENDDO
121500070412      *
121600070412     c                   ENDSR
121700070412      *
121800070412      *---------------------------------------------------------------*
121900070509      *?Scrittura singolo record trk "2" del file WFAIM00F           ?*
122000070412      *---------------------------------------------------------------*
122100070509     c     Wrt_WFAIM     BEGSR
122200070412      *
122300070412     c                   add       1             WWWprg
122400070412      *
122500070509     c                   clear                   WFAIM000
122600070412      *
122700070509     c                   eval      WIMnum    = DUJnum
122800070509     c                   eval      WIMtrk    = '2'
122900070509     c                   eval      WIMprg    = WWWprg
123000070509     c                   eval      WIMuni    = S1Cmat
123100070412      *                  __________________
123200070509     c                   WRITE     WFAIM000
123300070412      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
123400070412     c                   ENDSR
123500070509
123600070509** - $MSG ....+....2....+....3....+....4....+....5....+....6....+....7....+..*
123700070413Opzione errata                                                                 1
123800070413Matricola già inserita                                                         2
123900070413Matricola di lunghezza errata: deve essere di &N caratteri                     3
124000070413Matricola (sottostringa selezionata) già inserita nel subfile                  4
124100070413Matricola (sottostringa selezionata) reperita nell'inserimento n°              5
124200070509Inserite n° &NMI unità.                 |                                      6
124300070509Previste n° &NMP unità.                 |                                      7
124400070509»»»»»»»»»»»»»»» Premere F6 per forzare. |                                      8
124500070507Record n°&PRGR NON trovato per l'inserimento multiplo n°                       9
124600070620Le &NMA unità annullate andranno perse  |                                     10
124700070620e così pure le &NMI unità inserite.     |                                     11
124800070620La singola unità inserita andrà persa...|                                     12
124900070620Le &NMI unità inserite andranno perse...|                                     13
125000070620Premere F6 per confermare l'uscita.     |                                     14
125100081031Valori ammessi: "P" per prefisso, "S" per suffisso, "_" per nulla...          15
125200081031Inserire i caratteri da usare come                                            16
