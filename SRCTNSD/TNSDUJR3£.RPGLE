000100070601      *PARMS DBGVIEW(*SOURCE)
000200070601      *===============================================================*
000300070606      *?TNSDUJR3 - Inserimento multiplo unità EDP - trk "1" pf WFAIM ?*
000400070601      *===============================================================*
000500070606      *?N O T E :                                                    ?*
000600070601      * · DUJOPZ = '2' -> spostamento barcode da WFAIM20F a WFAIM00F  *
000700070601      * ·   "    = '4' -> cancellazione   records da WFAIM20F         *
000800070601      * ·   "    = '5' -> visualizzazione records di WFAIM20F         *
000900070601      *===============================================================*
001000070601
001100070601     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001200070601
001300070601      *---------------------------------------------------------------*
001400070601
001500070606     fWFAIM20F  if   e           k disk    usropn
001600070606     fWFAIM01L  if   e           k disk
001700070601      *
001800070606     fTNSDUJD2  cf   e             workstn usropn
001900070607     f                                     ignore(SDUJW01 :
002000070607     f                                            SDUJASS)
002100070604     f                                     sfile(SDUJS01:S01nrr)
002200070607     fTNSDUJD3  cf   e             workstn usropn
002300070601
002400070601      *---------------------------------------------------------------*
002500070601
002600070601      *
002700070601      *?Costanti  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
002800070601      *
002900070608      * - Comandi eseguibili
003000070608     d C_DspLNK_a      c                   const('DSPLNK +
003100070608     d                                           obj(''')
003200070608     d C_DspLNK_b      c                   const(''') +
003300070608     d                                           output(*) +
003400070608     d                                           objtype(*all)')
003500070608     d C_DirAS888      c                   const('/csv/*')
003600070608     d C_IFSas888      c                   const('\\CSV\')
003700070611     d C_DirSETRAS     c                   const('/tmp/*')
003800070611     d C_IFSsetras     c                   const('\\tmp\')
003900070606     d C_AlcWFAIM20    c                   const('ALCOBJ +
004000070606     d                                     obj((*libl/WFAIM20F +
004100070606     d                                          *file +
004200070606     d                                          *exclRD)) +
004300070606     d                                     wait(1) +
004400070606     d                                     scope(*job)')
004500070606     d C_DlcWFAIM20    c                   const('DLCOBJ +
004600070606     d                                     obj((*libl/WFAIM20F +
004700070606     d                                          *file +
004800070606     d                                          *exclRD)) +
004900070606     d                                     scope(*job)')
005000070606     d C_OvrShareD2    c                   const('OVRDBF +
005100070606     d                                     file(TNSDUJD2) +
005200070606     d                                     ovrscope(*actgrpdfn) +
005300070606     d                                     share(*YES)')
005400070601      *
005500070601      *?Schiere - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
005600070601      *
005700070604      * - Messaggi di errore
005800070608     d $Msg            s             78    dim( 6)   ctdata  perrcd(1)
005900070604      * - Codici matricola già inseriti nel subfile
006000070604     d $Matr           s                   dim(9999) inz  like(S1Cmat)
006100070601      *
006200070601      *?Ds  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
006300070601      *
006400070601      * - Parametri
006500070601     d KPJBA         e ds
006600070601     d TNSDUJds      e ds                  inz
006700070601      *
006800070601      * - Parametri x Controllo profilo utenti
006900070606     d TIBS34ds      e ds                  inz
007000070601      * - Ds di riferimento al file esterno AZUTE00F
007100070601     d AZUTEds       e ds                  extname(AZUTE00F)
007200070601      * - Ds per dati organigramma
007300070606     d dDatiUte      e ds
007400070606      *
007500070606      * DS campo WIMUNI file WFAIM00F trk "1"
007600070606     d dWIMuni1      e ds                  inz
007700070601      *
007800070601     d Status         sds           333
007900070601     d   SDSpgm          *proc
008000070601      *
008100070601      *?Variabili - - - - - - - - - - - - - - - - - - - - - - - - - -?*
008200070601      *
008300070601      * - Flags
008400070606     d $AlcWIM20       s              1    inz(*off)
008500070601     d $Fine           s              1    inz(*off)
008600070604     d $EoF            s              1    inz(*off)
008700070604     d $InzW01         s              1    inz(*on)
008800070604     d $InzS01         s              1    inz(*on)
008900070601      *
009000070601      * - Variabili riferite al data base o al display file
009100070601     d SavOpz          s                   like(DUJopz)   inz
009200070604      * - Variabili già presenti in TNSDUJR2
009300070604     d S01nrr          s                   like(C01rcd)   inz
009400070604     d MAXnrr          s                   like(C01rcd)   inz
009500070604      *
009600070604      * - Parametri per pgm. TNSDUJC = Riorganizzazione WF
009700070604     d DUJfclr         s              1    inz('2')
009800070606      *
009900070606      * - Parametri per pgm. TNSDUJC3 = Ricezione dati da PC
010000070608     d DUJ3dir         s             96    inz
010100070607     d DUJ3csv         s             32    inz
010200070608     d DUJ3err         s              1    inz(*off)
010300070606      *
010400070606      * - Parametri per QCMDEXC
010500070608     d Qcmd            s            256    inz
010600070606     d Qlen            s             15  5 inz(%len(Qcmd))
010700070601      *
010800070601      *?Key-List  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
010900070601      *
011000070604      * - WFAIM01L
011100070604     c     K02WIM01      klist
011200070604     c                   kfld                    WIMnum
011300070604     c                   kfld                    WIMtrk
011400070601
011500070601      *---------------------------------------------------------------*
011600070601      *?RIEPILOGO INDICATORI                                         ?*
011700070601      *---------------------------------------------------------------*
011800070601      * 25    - Comodo                                                *
011900070601      * 90    - Generico di errore                                    *
012000070601      *---------------------------------------------------------------*
012100070601
012200070601     c     *Entry        plist
012300070601     c                   parm                    KPJBA
012400070601      *
012500070601      * Operazioni Iniziali
012600070601     c                   exsr      RoutInz
012700070601      *
012800070604      * Elaborazione singola opzione ricevuta dal chiamante TNSDUJR
012900070601do  1c                   DOW       $Fine     = *off
013000070606cas 2c     DUJopz        caseq     '1'           From_PC
013100070606cas 2c     DUJopz        caseq     '2'           OpzS01_2
013200070606cas 2c     DUJopz        caseq     '4'           OpzS01_4
013300070606cas 2c     DUJopz        caseq     '5'           OpzS01_5
013400070601e   2c                   endcs
013500070601e   1c                   ENDDO
013600070608      *
013700070608      * Operazioni FInali
013800070608     c                   exsr      RoutEnd
013900070604      *
014000070601      * Fine
014100070604     c                   movel(p)  TNSDUJds      KPJBU
014200070604     c                   eval      *inLR     = *on
014300070601      *
014400070601      *---------------------------------------------------------------*
014500070601      *?Operazioni Iniziali                                          ?*
014600070601      *---------------------------------------------------------------*
014700070601     c     RoutInz       BEGSR
014800070601      *
014900070601      * Reperimento dati job
015000070601     c                   exsr      DatiJob
015100070601      *
015200070601     c                   reset                   $Fine
015300070601      *
015400070601      * Impostazione iniziale dei parametri i/o
015500070601     c                   movel     KPJBU         TNSDUJds
015600070601     c                   clear                   DUJnum
015700070601     c                   clear                   DUJprg
015800070601     c                   clear                   DUJuni
015900070601     c                   eval      DUJret    = *off
016000070601     c                   eval      DUJerr    = *off
016100070601     c                   eval      DUJmsg    = *blanks
016200070601      *
016300070601     c                   eval      SavOpz    = DUJopz
016400070601      *
016500070601      * CONTROLLO PARAMETRI RICEVUTI
016600070601      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
016700070606if  1c                   if             DUJopz   <> '1'
016800070606     c                             and  DUJopz   <> '2'
016900070601     c                             and  DUJopz   <> '4'
017000070601     c                             and  DUJopz   <> '5'
017100070601     c                   eval      DUJerr    = *on
017200070601     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
017300070601     c                   eval      $Fine     = *on
017400070601     c                   leavesr
017500070601e   1c                   endif
017600070606      *
017700070606      * Allocazione archivio WFAIM20F
017800070606      *   (se richiesta conferma o cancellazione)
017900070606if  1c                   IF             DUJopz  = '2'
018000070606     c                             or   DUJopz  = '4'
018100070606     c                   call(e)   'QCMDEXC'
018200070606     c                   parm      C_AlcWFAIM20  Qcmd
018300070606     c                   parm                    Qlen
018400070606if  2c                   if        %error
018500070606     c                   eval      DUJerr    = *on
018600070606     c                   eval      DUJmsg    = $Msg( 1)
018700070606     c                   eval      $Fine     = *on
018800070606     c                   leavesr
018900070606e   2c                   endif
019000070606     c                   eval      $AlcWIM20 = (NOT %error)
019100070606e   1c                   ENDIF
019200070604      *
019300070606      * Apertura archivio WFAIM20F
019400070606      *   (se richiesta conferma o visualizzazione)
019500070606if  1c                   if             DUJopz  = '2'
019600070606     c                             or   DUJopz  = '5'
019700070604     c                   open      WFAIM20F
019800070606e   1c                   endif
019900070601      *
020000070601     c                   ENDSR
020100070601      *
020200070601      *---------------------------------------------------------------*
020300070601      *?Reperimento Dati del job (Utente/Operativi)                  ?*
020400070601      *---------------------------------------------------------------*
020500070601     c     DatiJob       BEGSR
020600070601      *
020700070601     c     *dtaara       define    §azute        azuteds
020800070601     c     *dtaara       define    §datiute      ddatiute
020900070601      *
021000070601     c                   in(E)     *dtaara
021100070601     c                   IF        %ERROR or RSUT = *blanks
021200070601     c                   clear                   Tibs34Ds
021300070601     c                   call      'TIBS34R'
021400070601     c                   parm                    Tibs34Ds
021500070601     c                   in        *dtaara
021600070601     c                   ENDIF
021700070601      *
021800070601     c                   ENDSR
021900070606      *
022000070606      *---------------------------------------------------------------*
022100070606      *?Trasferimento dati da PC ad AS/400                           ?*
022200070606      *---------------------------------------------------------------*
022300070606     c     From_PC       BEGSR
022400070606      *
022500070607      * Richiesta conferma di copia dati da file PC in WFAIM20F
022600070606      *
022700070607if  1c                   if        NOT  %open(TNSDUJD3)
022800070607     c                   open      TNSDUJD3
022900070607e   1c                   endif
023000070606      *
023100070606      * - Inizializzazione window
023200070607if  1c                   if        $InzW01   = *on
023300070607     c                   clear                   SDUJW01
023400070608     c                   exsr      DftW01
023500070607     c                   eval      $InzW01   = *off
023600070607e   1c                   endif
023700070606      *
023800070607      * - Emissione window per richiesta nome file da ricevere
023900070607     c                   exfmt     SDUJW01
024000070608     c                   clear                   V1Dmsg
024100070608     c                   setoff                                       28  90
024200070608     c                   movea     *zeros        *in(50)
024300070606      *
024400070606sel 1c                   select
024500070608      * - F4=Ricerca  (NON eseguire ricezione)
024600070608w   1c                   when      *inKD
024700070608     c                   exsr      DspLNK
024800070606      * - F12=Ritorno (NON eseguire ricezione)
024900070606w   1c                   when      *inKL
025000070608     c                   eval      DUJret    = '2'
025100070608     c                   eval      $Fine     = *on
025200070608      * - Invio / F6 => Controlli
025300070608x   1c                   other
025400070608     c                   exsr      DftW01
025500070606      * - F6=Conferma (eseguire ricezione)
025600070608if  2c                   if        *inKF
025700070608     c                   exsr      Call_TNSDUJ3C
025800070608e   2c                   endif
025900070606e   1c                   endsl
026000070606      *
026100070606     c                   ENDSR
026200070608      *
026300070608      *---------------------------------------------------------------*
026400070608      *?Visualizzazione collegamenti degli oggetti                   ?*
026500070608      *---------------------------------------------------------------*
026600070608     c     DspLNK        BEGSR
026700070608      *
026800070608     c                   if        %subst(KNSIF:7:1) = 'P'
026900070608     c                   eval      Qcmd      = C_DspLNK_a
027000070608     c                                       + %trimr(C_DirAS888)
027100070608     c                                       + C_DspLNK_b
027200070608     c                   else
027300070608     c                   eval      Qcmd      = C_DspLNK_a
027400070608     c                                       + %trimr(C_DirSETRAS)
027500070608     c                                       + C_DspLNK_b
027600070608     c                   endif
027700070608      *
027800070608     c                   call      'QCMDEXC'                            90
027900070608     c                   parm                    Qcmd
028000070608     c                   parm                    Qlen
028100070608      *
028200070608     c                   if        *in90
028300070608     c                   seton                                        2850
028400070608     c                   eval      V1Dmsg    = $Msg( 5)
028500070608     c                   endif
028600070608      *
028700070608     c                   ENDSR
028800070608      *
028900070608      *---------------------------------------------------------------*
029000070608      *?Visualizzazione collegamenti degli oggetti                   ?*
029100070608      *---------------------------------------------------------------*
029200070608     c     Call_TNSDUJ3C BEGSR
029300070608      *
029400070608     c                   if        %subst(KNSIF:7:1) = 'P'
029500070608     c                   eval      DUJ3dir   = C_IFSas888
029600070608     c                   else
029700070608     c                   eval      DUJ3dir   = C_IFSsetras
029800070608     c                   endif
029900070608      *
030000070608     c                   call      'TNSDUJC3'
030100070608     c                   parm                    DUJ3dir
030200070608     c                   parm      W1Cfpc        DUJ3csv
030300070608     c                   parm      *off          DUJ3err
030400070608      *
030500070608if  1c                   if        DUJ3err   = *on
030600070608     c                   seton                                        2850
030700070608     c                   eval      V1Dmsg    = $Msg( 6)
030800070608x   1c                   else
030900070608     c                   eval      $Fine     = *on
031000070608e   1c                   endif
031100070608      *
031200070608     c                   ENDSR
031300070608      *
031400070608      *---------------------------------------------------------------*
031500070608      *?Impostazione dei valori di default in W01 (se mancanti)      ?*
031600070608      *---------------------------------------------------------------*
031700070608     c     DftW01        BEGSR
031800070608      *
031900070608      * Directory in cui è posto il file da trasmettere
032000070608if  1c                   if        W1Ddir    = *blanks
032100070608if  2c                   if        %subst(KNSIF:7:1) = 'P'
032200070608     c                   eval      W1Ddir    = C_DirAS888
032300070608x   2c                   else
032400070608     c                   eval      W1Ddir    = C_DirSETRAS
032500070608e   2c                   endif
032600070608e   1c                   endif
032700070608      *
032800070608      * Nome File da trasmettere da PC as AS/400
032900070608if  1c                   if             W1Cfpc  = *blanks
033000070608     c                             and  $InzW01 = *off
033100070608     c                   eval      W1Cfpc    = 'Barcode.csv'
033200070608e   1c                   endif
033300070608      *
033400070608     c                   ENDSR
033500070601      *
033600070601      *---------------------------------------------------------------*
033700070601      *?Passaggio barcode da WFAIM20F a WFAIM00F                     ?*
033800070601      *---------------------------------------------------------------*
033900070606     c     OpzS01_2      BEGSR
034000070601      *
034100070606      *?VIENE UTILIZZATO IL DSPF TNSDUJD2?
034200070606      *
034300070607     c                   eval      $Fine     = *on
034400070607      *
034500070601      * Nuovo inserimento multiplo unità:
034600070601      * 1) Creazione + "Blocco" dell'inserimento multiplo unità (trk 1)
034700070606      * 2) Caricamento subfile SDUJS01 con i dati di WFAIM20F
034800070606      * 3) Manutenzione dell'inserimento multiplo unità         (trk.2)
034900070606      * 4) "Sblocco" dell'inserimento multiplo unità            (trk.1)
035000070606      *
035100070604      * 1) Gestione rec. trk "1" per
035200070604      *    Inserimento e "Blocco" dell'inserimento multiplo unità
035300070604     c                   eval      DUJopz    = '1'
035400070601     c                   exsr      Call_TNSDUJ1
035500070601if  1c                   if             DUJerr <> *off
035600070601     c                             or   DUJret <> *off
035700070601     c                   leavesr
035800070601e   1c                   endif
035900070606     c                   eval      DUJopz    = SavOpz
036000070601      *
036100070606      * 2) Inizializzazione subfile per chiamata al pgm. TNSDUJR2
036200070606     c                   exsr      InzS01
036300070606      *    - Ritorno con errore se nessun record è stato caricato
036400070606      *      (dopo la cancellazione del trk "1", creato al punto 1)
036500070606if  1c                   if        MAXnrr    = *zeros
036600070606     c                   eval      DUJopz    = 'd'
036700070606     c                   exsr      Call_TNSDUJ1
036800070606     c                   eval      DUJerr    = *on
036900070606     c                   eval      DUJmsg    = $Msg( 1)
037000070606     c                   leavesr
037100070606e   1c                   endif
037200070601      *
037300070604      * 3) Gestione rec. trk "2" per
037400070605      *    Immissione/Conferma dell'inserimento multiplo unità
037500070606     c                   eval      DUJopz    = 'A'
037600070607     c                   eval      DUJprg    = MAXnrr
037700070601     c                   exsr      Call_TNSDUJ2
037800070605      *
037900070605      * 4) "Sblocco" dell'inserimento multiplo unità al termine
038000070606if  1c                   IF             DUJret  = *off
038100070604     c                             and  DUJerr  = *off
038200070606      *    - Gestione rec. trk "1" per
038300070606      *      "Sblocco" dell'inserimento multiplo unità
038400070604     c                   eval      DUJopz    = '8'
038500070604     c                   exsr      Call_TNSDUJ1
038600070606      *    - Cancellazione records da WFAIM20F
038700070606     c                   close     WFAIM20F
038800070606     c                   call      'TNSDUJC'
038900070606     c                   parm      '2'           DUJfclr
039000070606x   1c                   ELSE
039100070606      *    - Cancellazione del trk "1", creato al punto 1
039200070606     c                   eval      DUJopz    = 'd'
039300070606     c                   exsr      Call_TNSDUJ1
039400070606e   1c                   ENDIF
039500070601      *
039600070601     c                   ENDSR
039700070601      *
039800070601      *---------------------------------------------------------------*
039900070601      *?Cancellazione barcode da WFAIM20F                            ?*
040000070601      *---------------------------------------------------------------*
040100070606     c     OpzS01_4      BEGSR
040200070601      *
040300070607      *?VIENE UTILIZZATO IL DSPF TNSDUJD3?
040400070606      *
040500070607if  1c                   if        NOT  %open(TNSDUJD3)
040600070607     c                   open      TNSDUJD3
040700070607e   1c                   endif
040800070607      *
040900070601      * Inizializzazione della videata
041000070607      *   (NON ha campi di I/O !!!)
041100070601      *
041200070601      * Emissione della window di conferma
041300070607     c                   exfmt     SDUJW02
041400070604     c                   setoff                                       28  90
041500070604     c                   clear                   V1Dmsg
041600070604      *
041700070604sel 1c                   SELECT
041800070601      * - F12 = Ritorno
041900070604w   1c                   WHEN      *inKL
042000070604     c                   eval      DUJret    = '2'
042100070604     c                   eval      $Fine     = *on
042200070601      * - F6 = Conferma
042300070604w   1c                   WHEN      *inKF
042400070604      *   Cancellazione records da WFAIM20F
042500070606     c*** mai aperto:    close     WFAIM20F
042600070604     c                   call      'TNSDUJC'
042700070604     c                   parm      '2'           DUJfclr
042800070604     c                   eval      $Fine     = *on
042900070604      *
043000070604e   1c                   ENDSL
043100070601      *
043200070601     c                   ENDSR
043300070601      *
043400070601      *---------------------------------------------------------------*
043500070601      *?Visualizzazione barcode di WFAIM20F                          ?*
043600070601      *---------------------------------------------------------------*
043700070606     c     OpzS01_5      BEGSR
043800070601      *
043900070606      *?VIENE UTILIZZATO IL DSPF TNSDUJD2?
044000070606      *
044100070604      * Inizializzazione della videata
044200070604      *
044300070604if  1c                   if        $InzS01   = *on
044400070604     c                   exsr      InzS01
044500070604     c                   eval      $InzS01   = *off
044600070604e   1c                   endif
044700070604      *
044800070604      * Scrittura di testata e riga tasti funzionali abilitati
044900070604     c                   write     SDUJT01
045000070604     c                   write     SDUJZ01
045100070604      *
045200070604      * Visualizzazione (eventuale) del msg per la mancanza di dati
045300070604if  1c                   if        C01rcd   <= *zeros
045400070604     c                   write     SDUJD00
045500070604e   1c                   endif
045600070604      *
045700070604      * Emissione della videata
045800070604     c                   exfmt     SDUJC01
045900070604     c                   setoff                                       28  90
046000070604     c                   clear                   V1Dmsg
046100070604if  1c                   if        C01csr    > *zeros
046200070604     c                   eval      C01rcd    = C01csr
046300070604e   1c                   endif
046400070604      *
046500070604sel 1c                   SELECT
046600070604      * F3=Fine
046700070604w   1c                   WHEN      *inKC
046800070604     c                   eval      DUJret    = '1'
046900070604     c                   eval      $Fine     = *on
047000070604      * F12=Ritorno
047100070604w   1c                   WHEN      *inKL
047200070604     c                   eval      DUJret    = '2'
047300070604     c                   eval      $Fine     = *on
047400070604e   1c                   ENDSL
047500070601      *
047600070601     c                   ENDSR
047700070604      *
047800070604      *---------------------------------------------------------------*
047900070606      *?Inizializzazione videata S01 del display-file TNSDUJD2       ?*
048000070604      *---------------------------------------------------------------*
048100070604     c     InzS01        BEGSR
048200070604      *
048300070606      *?VIENE UTILIZZATO IL DSPF TNSDUJD2?
048400070606      *
048500070606      * Override al dispaly-file per abilitare la condivisione al sfl
048600070607     c                   call      'QCMDEXC'
048700070607     c                   parm      C_OvrShareD2  Qcmd
048800070607     c                   parm                    Qlen
048900070606     c                   open      TNSDUJD2
049000070606      *
049100070606      * "Intestazione" testata  (qui gestita se opz. 5=Visualizzazione)
049200070606if  1c                   if        DUJopz    = '5'
049300070604     c                   eval      V1Tpgm    = SDSpgm
049400070604     c                   eval      V1Ttxt    = ' '
049500070604     c                                       + 'V I S U A L I Z Z A +
049600070604     c                                          Z I O N E'
049700070606e   1c                   endif
049800070604      *
049900070606      * "Intestazione" del subfile-control (gestito dal pgm. TNSDUJR2)
050000070604     c                   clear                   SDUJC01
050100070606if  1c                   if        DUJopz    = '2'
050200070606     c                   movel     DUJuni        dWIMuni1
050300070606     c                   eval      C1Dnum    = DUJnum
050400070606     c                   eval      C1Ddes    = §WIM1des
050500070606     c                   eval      C1Dnmi    = §WIM1nmi
050600070606e   1c                   endif
050700070604      *
050800070604      * Pulizia subfile
050900070604     c                   seton                                        3031
051000070604     c                   write     SDUJC01
051100070604     c                   setoff                                         3133
051200070604      *
051300070604     c                   reset                   $EoF
051400070604     c***                clear                   W_SflPag
051500070604     c***                clear                   C01csr
051600070604     c                   clear                   C01rcd
051700070604     c                   clear                   S01nrr
051800070604     c                   clear                   V1Dmsg
051900070604     c                   setoff                                       28  90
052000070604     c                   movea     *zeros        *in(50)
052100070604      *
052200070604      * Caricamento eventuali dati da presentare nel sfl
052300070604     c                   clear                   $Matr
052400070604     c                   clear                   MAXnrr
052500070605     c                   read      WFAIM200
052600070604     c                   eval      $EoF      = (%eof(WFAIM20F))
052700070604     c                   exsr      RollUpS01
052800070605      *
052900070605      * Reimpostazione condizioni ai vari DSPATR...
053000070606     c                   eval      *in45     = (DUJopz = '5')
053100070604      *
053200070604      * Posizionamento cursore al 1° rcd del sfl
053300070604if  1c                   if        S01nrr    > *zeros
053400070604     c                   eval      C01rcd    = 1
053500070604x   1c                   else
053600070604     c                   clear                   C01rcd
053700070604e   1c                   endif
053800070604      *
053900070604     c                   ENDSR
054000070604      *
054100070604      *---------------------------------------------------------------*
054200070604      *?Caricamento dati nel subfile S01                             ?*
054300070604      *---------------------------------------------------------------*
054400070604     c     RollUpS01     BEGSR
054500070604      *
054600070604     c***                eval      S01nrr    = (W_SflPag * C_SflPag)
054700070604     c***                add       1             W_SflPag
054800070604     c                   eval      *in32     = *off
054900070604      *
055000070604      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
055100070604do  1c                   doW            $EoF   = *off
055200070604     c*** carica tutto:            and  S01nrr < W_SflPag * C_SflPag
055300070604     c                   exsr      CarS01
055400070605     c                   read      WFAIM200
055500070604     c                   eval      $EoF      = (%eof(WFAIM20F))
055600070604e   1c                   enddo
055700070604      *
055800070604      * Visualizzazione totale unità già inserite
055900070604     c                   eval      C1Cprg    = MAXnrr
056000070604      *
056100070604      * Visualizzazione del SFL (se ci sono dati)
056200070604     c                   eval      *in30     = (S01nrr = *zeros)
056300070604      *
056400070604      * Attivazione (eventuale) del SFLEND
056500070604     c                   eval      *in33     = $EoF
056600070604      *
056700070604     c                   ENDSR
056800070604      *
056900070604      *---------------------------------------------------------------*
057000070604      *?Caricamento singolo rec. dati da file WFAIM00F in subfile S01?*
057100070604      *---------------------------------------------------------------*
057200070604     c     CarS01        BEGSR
057300070604      *
057400070604     c                   clear                   SDUJS01
057500070604      *
057600070604      * Controlli preliminari:
057700070604      * SE matricola già inserita => uscita (NO caricamento record)
057800070605      * ...Ammesso che NON sia in 5=Visualizzazione
057900070604     c     WIMbcd        lookup    $Matr                                  25
058000070604if  1c                   if        *in25
058100070605     c                             and  DUJopz <> '5'
058200070604     c                   leavesr
058300070604e   1c                   endif
058400070604      * ALTRIMENTI => gestione codice matricola - anche se anomalo
058500070604      *
058600070604      * Impostazione campi del subfile
058700070604     c                   eval      S1Cmat    = WIMbcd
058800070604     c                   eval      S1Cprg    = MAXnrr + 1
058900070604      *
059000070604      * Impostazione indicatori
059100070604     c                   eval      *in41     = *off
059200070606     c                   eval      *in45     = (DUJopz = '5')
059300070604      *
059400070604      * Scrittura record subfile
059500070604     c                   add       1             S01nrr
059600070604     c                   write     SDUJS01
059700070604     c                   eval      MAXnrr    = S01nrr
059800070604      *
059900070604      * Intabellamento codice matricola appena inserito
060000070604     c                   eval      $Matr(S01nrr) = WIMbcd
060100070604      *
060200070604     c                   ENDSR
060300070601      *
060400070601      *---------------------------------------------------------------*
060500070601      *?Richiamo del pgm. TNSDUJR1 (gestione trk. "1" file WFAIM00F) ?*
060600070601      *---------------------------------------------------------------*
060700070601     c     Call_TNSDUJ1  BEGSR
060800070601      *
060900070601     c                   movel(p)  TNSDUJds      KPJBU
061000070601     c                   call      'TNSDUJR1'
061100070601     c                   parm                    KPJBA
061200070601     c                   movel     KPJBU         TNSDUJds
061300070601      *
061400070601sel 1c                   SELECT
061500070601      * Ritorno normale
061600070601w   1c                   WHEN           DUJret  = *off
061700070601     c                             and  DUJerr  = *off
061800070606if  2c                   if        %subst(DUJuni:1:40) <> *blanks
061900070606     c                   movel     DUJuni        dWIMuni1
062000070606e   2c                   endif
062100070601      * Ritorno con errore
062200070601w   1c                   WHEN      DUJerr    = *on
062300070608     c*** sarà comunque: eval      $Fine     = *on
062400070601      * Ritorno con F3
062500070601w   1c                   WHEN      DUJret    = '1'
062600070608     c*** sarà comunque: eval      $Fine     = *on
062700070601      * Ritorno con F12
062800070601w   1c                   WHEN      DUJret    = '2'
062900070608     c*** sarà comunque: eval      $Fine     = *on
063000070601      *
063100070601e   1c                   ENDSL
063200070601      *
063300070601     c                   ENDSR
063400070601      *
063500070601      *---------------------------------------------------------------*
063600070601      *?Richiamo del pgm. TNSDUJR2 (gestione trk. "2" file WFAIM00F) ?*
063700070601      *---------------------------------------------------------------*
063800070601     c     Call_TNSDUJ2  BEGSR
063900070601      *
064000070601      * Gestione rec. trk "2" per
064100070601      * Immissione dell'inserimento multiplo unità
064200070601     c                   movel(p)  TNSDUJds      KPJBU
064300070601     c                   call      'TNSDUJR2'
064400070601     c                   parm                    KPJBA
064500070601     c                   movel     KPJBU         TNSDUJds
064600070601      *
064700070601sel 1c                   SELECT
064800070601      * - Ritorno normale
064900070601w   1c                   WHEN           DUJret  = *off
065000070601     c                             and  DUJerr  = *off
065100070608     c*** inutile:       leavesr
065200070601      * - Ritorno con errore
065300070601w   1c                   WHEN      DUJerr    = *on
065400070608     c*** sarà comunque: eval      $Fine     = *on
065500070601      * - Ritorno con F3
065600070601w   1c                   WHEN      DUJret    = '1'
065700070608     c*** sarà comunque: eval      $Fine     = *on
065800070601      * - Ritorno con F12
065900070601w   1c                   WHEN      DUJret    = '2'
066000070608     c*** sarà comunque: eval      $Fine     = *on
066100070601      *
066200070601     c                   ENDSL
066300070604      *
066400070604     c                   ENDSR
066500070608      *
066600070608      *---------------------------------------------------------------*
066700070608      *?Operazioni Finali                                            ?*
066800070608      *---------------------------------------------------------------*
066900070608     c     RoutEnd       BEGSR
067000070608      *
067100070608      * Chiusura archivi
067200070608      *
067300070608if  1c                   if        %open(TNSDUJD3)
067400070608     c                   close     TNSDUJD3
067500070608e   1c                   endif
067600070608if  1c                   if        %open(TNSDUJD2)
067700070608     c                   close     TNSDUJD2
067800070608e   1c                   endif
067900070608      *
068000070608      * Disallocazione workfile WFAIM20F
068100070608if  1c                   IF        $AlcWIM20 = *on
068200070608if  2c                   if        %open(WFAIM20F)
068300070608     c                   close     WFAIM20F
068400070608e   2c                   endif
068500070608     c                   call(e)   'QCMDEXC'
068600070608     c                   parm      C_DlcWFAIM20  Qcmd
068700070608     c                   parm                    Qlen
068800070608e   1c                   ENDIF
068900070608      *
069000070608     c                   ENDSR
069100070604      *
069200070604** - $Msg ....+....2....+....3....+....4....+....5....+....6....+....7....+...
069300070606Dati già in uso da altro lavoro...                                              1
069400070606Non trovati record nel workfile WFAIM20F                                        2
069500070606             Premere F6 per             |                                       3
069600070606 confermare la ricezione di dati da PC. |                                       4
069700070608Rilevati errori nel tentativo di visualizzare i dati della directory impostata  5
069800070608Rilevati errori nella trasmissione del file da PC su AS/400...                  6
