000100070601      *PARMS DBGVIEW(*SOURCE)
000200130919     /*PRM  dbgview(*source)
000300130919     /*END
000400070601      *===============================================================*
000500070606      *?TNSDUJR3 - Inserimento multiplo unità EDP - trk "1" pf WFAIM ?*
000600070601      *===============================================================*
000700070606      *?N O T E :                                                    ?*
000800070601      * · DUJOPZ = '2' -> spostamento barcode da WFAIM20F a WFAIM00F  *
000900070601      * ·   "    = '4' -> cancellazione   records da WFAIM20F         *
001000070601      * ·   "    = '5' -> visualizzazione records di WFAIM20F         *
001100070601      *===============================================================*
001200070601
001300070601     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001400070601
001500070601      *---------------------------------------------------------------*
001600070601
001700070606     fWFAIM20F  if   e           k disk    usropn
001800070606     fWFAIM01L  if   e           k disk
001900070601      *
002000070606     fTNSDUJD2  cf   e             workstn usropn
002100070607     f                                     ignore(SDUJW01 :
002200070607     f                                            SDUJASS)
002300070604     f                                     sfile(SDUJS01:S01nrr)
002400070607     fTNSDUJD3  cf   e             workstn usropn
002500070601
002600070601      *---------------------------------------------------------------*
002700070601
002800070601      *
002900070601      *?Costanti  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
003000070601      *
003100070608      * - Comandi eseguibili
003200070608     d C_DspLNK_a      c                   const('DSPLNK +
003300070608     d                                           obj(''')
003400070608     d C_DspLNK_b      c                   const(''') +
003500070608     d                                           output(*) +
003600070608     d                                           objtype(*all)')
003700070608     d C_DirAS888      c                   const('/csv/*')
003800070608     d C_IFSas888      c                   const('\\CSV\')
003900070611     d C_DirSETRAS     c                   const('/tmp/*')
004000070611     d C_IFSsetras     c                   const('\\tmp\')
004100070606     d C_AlcWFAIM20    c                   const('ALCOBJ +
004200070606     d                                     obj((*libl/WFAIM20F +
004300070606     d                                          *file +
004400070606     d                                          *exclRD)) +
004500070606     d                                     wait(1) +
004600070606     d                                     scope(*job)')
004700070606     d C_DlcWFAIM20    c                   const('DLCOBJ +
004800070606     d                                     obj((*libl/WFAIM20F +
004900070606     d                                          *file +
005000070606     d                                          *exclRD)) +
005100070606     d                                     scope(*job)')
005200070606     d C_OvrShareD2    c                   const('OVRDBF +
005300070606     d                                     file(TNSDUJD2) +
005400070606     d                                     ovrscope(*actgrpdfn) +
005500070606     d                                     share(*YES)')
005600070601      *
005700070601      *?Schiere - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
005800070601      *
005900070604      * - Messaggi di errore
006000070608     d $Msg            s             78    dim( 6)   ctdata  perrcd(1)
006100070604      * - Codici matricola già inseriti nel subfile
006200070604     d $Matr           s                   dim(9999) inz  like(S1Cmat)
006300070601      *
006400070601      *?Ds  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
006500070601      *
006600070601      * - Parametri
006700070601     d KPJBA         e ds
006800070601     d TNSDUJds      e ds                  inz
006900070601      *
007000070601      * - Parametri x Controllo profilo utenti
007100070606     d TIBS34ds      e ds                  inz
007200070601      * - Ds di riferimento al file esterno AZUTE00F
007300070601     d AZUTEds       e ds                  extname(AZUTE00F)
007400070601      * - Ds per dati organigramma
007500070606     d dDatiUte      e ds
007600070606      *
007700130916      * DS campo WIMUNI file WFAIM00F trk "1" prg 00/01
007800070606     d dWIMuni1      e ds                  inz
007900130916      *
008000130916      * DS campo WIMUNI file WFAIM00F trk "1" prg 02
008100130916     d dWIMuni12     e ds                  inz
008200070601      *
008300070601     d Status         sds           333
008400070601     d   SDSpgm          *proc
008500070601      *
008600070601      *?Variabili - - - - - - - - - - - - - - - - - - - - - - - - - -?*
008700070601      *
008800070601      * - Flags
008900070606     d $AlcWIM20       s              1    inz(*off)
009000070601     d $Fine           s              1    inz(*off)
009100070604     d $EoF            s              1    inz(*off)
009200070604     d $InzW01         s              1    inz(*on)
009300070604     d $InzS01         s              1    inz(*on)
009400070601      *
009500070601      * - Variabili riferite al data base o al display file
009600070601     d SavOpz          s                   like(DUJopz)   inz
009700070604      * - Variabili già presenti in TNSDUJR2
009800070604     d S01nrr          s                   like(C01rcd)   inz
009900070604     d MAXnrr          s                   like(C01rcd)   inz
010000070604      *
010100070604      * - Parametri per pgm. TNSDUJC = Riorganizzazione WF
010200070604     d DUJfclr         s              1    inz('2')
010300070606      *
010400070606      * - Parametri per pgm. TNSDUJC3 = Ricezione dati da PC
010500070608     d DUJ3dir         s             96    inz
010600070607     d DUJ3csv         s             32    inz
010700070608     d DUJ3err         s              1    inz(*off)
010800070606      *
010900070606      * - Parametri per QCMDEXC
011000070608     d Qcmd            s            256    inz
011100070606     d Qlen            s             15  5 inz(%len(Qcmd))
011200070601      *
011300070601      *?Key-List  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
011400070601      *
011500070604      * - WFAIM01L
011600070604     c     K02WIM01      klist
011700070604     c                   kfld                    WIMnum
011800070604     c                   kfld                    WIMtrk
011900070601
012000070601      *---------------------------------------------------------------*
012100070601      *?RIEPILOGO INDICATORI                                         ?*
012200070601      *---------------------------------------------------------------*
012300070601      * 25    - Comodo                                                *
012400070601      * 90    - Generico di errore                                    *
012500070601      *---------------------------------------------------------------*
012600070601
012700070601     c     *Entry        plist
012800070601     c                   parm                    KPJBA
012900070601      *
013000070601      * Operazioni Iniziali
013100070601     c                   exsr      RoutInz
013200070601      *
013300070604      * Elaborazione singola opzione ricevuta dal chiamante TNSDUJR
013400070601do  1c                   DOW       $Fine     = *off
013500070606cas 2c     DUJopz        caseq     '1'           From_PC
013600070606cas 2c     DUJopz        caseq     '2'           OpzS01_2
013700070606cas 2c     DUJopz        caseq     '4'           OpzS01_4
013800070606cas 2c     DUJopz        caseq     '5'           OpzS01_5
013900070601e   2c                   endcs
014000070601e   1c                   ENDDO
014100070608      *
014200070608      * Operazioni FInali
014300070608     c                   exsr      RoutEnd
014400070604      *
014500070601      * Fine
014600070604     c                   movel(p)  TNSDUJds      KPJBU
014700070604     c                   eval      *inLR     = *on
014800070601      *
014900070601      *---------------------------------------------------------------*
015000070601      *?Operazioni Iniziali                                          ?*
015100070601      *---------------------------------------------------------------*
015200070601     c     RoutInz       BEGSR
015300070601      *
015400070601      * Reperimento dati job
015500070601     c                   exsr      DatiJob
015600070601      *
015700070601     c                   reset                   $Fine
015800070601      *
015900070601      * Impostazione iniziale dei parametri i/o
016000070601     c                   movel     KPJBU         TNSDUJds
016100070601     c                   clear                   DUJnum
016200070601     c                   clear                   DUJprg
016300130916     c                   clear                   DUJuni1
016400130916     c                   clear                   DUJuni2
016500070601     c                   eval      DUJret    = *off
016600070601     c                   eval      DUJerr    = *off
016700070601     c                   eval      DUJmsg    = *blanks
016800070601      *
016900070601     c                   eval      SavOpz    = DUJopz
017000070601      *
017100070601      * CONTROLLO PARAMETRI RICEVUTI
017200070601      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
017300070606if  1c                   if             DUJopz   <> '1'
017400070606     c                             and  DUJopz   <> '2'
017500070601     c                             and  DUJopz   <> '4'
017600070601     c                             and  DUJopz   <> '5'
017700070601     c                   eval      DUJerr    = *on
017800070601     c***                eval      DUJmsg    = 'PARAMETRI ERRATI'
017900070601     c                   eval      $Fine     = *on
018000070601     c                   leavesr
018100070601e   1c                   endif
018200070606      *
018300070606      * Allocazione archivio WFAIM20F
018400070606      *   (se richiesta conferma o cancellazione)
018500070606if  1c                   IF             DUJopz  = '2'
018600070606     c                             or   DUJopz  = '4'
018700070606     c                   call(e)   'QCMDEXC'
018800070606     c                   parm      C_AlcWFAIM20  Qcmd
018900070606     c                   parm                    Qlen
019000070606if  2c                   if        %error
019100070606     c                   eval      DUJerr    = *on
019200070606     c                   eval      DUJmsg    = $Msg( 1)
019300070606     c                   eval      $Fine     = *on
019400070606     c                   leavesr
019500070606e   2c                   endif
019600070606     c                   eval      $AlcWIM20 = (NOT %error)
019700070606e   1c                   ENDIF
019800070604      *
019900070606      * Apertura archivio WFAIM20F
020000070606      *   (se richiesta conferma o visualizzazione)
020100070606if  1c                   if             DUJopz  = '2'
020200070606     c                             or   DUJopz  = '5'
020300070604     c                   open      WFAIM20F
020400070606e   1c                   endif
020500070601      *
020600070601     c                   ENDSR
020700070601      *
020800070601      *---------------------------------------------------------------*
020900070601      *?Reperimento Dati del job (Utente/Operativi)                  ?*
021000070601      *---------------------------------------------------------------*
021100070601     c     DatiJob       BEGSR
021200070601      *
021300070601     c     *dtaara       define    §azute        azuteds
021400070601     c     *dtaara       define    §datiute      ddatiute
021500070601      *
021600070601     c                   in(E)     *dtaara
021700070601     c                   IF        %ERROR or RSUT = *blanks
021800070601     c                   clear                   Tibs34Ds
021900070601     c                   call      'TIBS34R'
022000070601     c                   parm                    Tibs34Ds
022100070601     c                   in        *dtaara
022200070601     c                   ENDIF
022300070601      *
022400070601     c                   ENDSR
022500070606      *
022600070606      *---------------------------------------------------------------*
022700070606      *?Trasferimento dati da PC ad AS/400                           ?*
022800070606      *---------------------------------------------------------------*
022900070606     c     From_PC       BEGSR
023000070606      *
023100070607      * Richiesta conferma di copia dati da file PC in WFAIM20F
023200070606      *
023300070607if  1c                   if        NOT  %open(TNSDUJD3)
023400070607     c                   open      TNSDUJD3
023500070607e   1c                   endif
023600070606      *
023700070606      * - Inizializzazione window
023800070607if  1c                   if        $InzW01   = *on
023900070607     c                   clear                   SDUJW01
024000070608     c                   exsr      DftW01
024100070607     c                   eval      $InzW01   = *off
024200070607e   1c                   endif
024300070606      *
024400070607      * - Emissione window per richiesta nome file da ricevere
024500070607     c                   exfmt     SDUJW01
024600070608     c                   clear                   V1Dmsg
024700070608     c                   setoff                                       28  90
024800070608     c                   movea     *zeros        *in(50)
024900070606      *
025000070606sel 1c                   select
025100070608      * - F4=Ricerca  (NON eseguire ricezione)
025200070608w   1c                   when      *inKD
025300070608     c                   exsr      DspLNK
025400070606      * - F12=Ritorno (NON eseguire ricezione)
025500070606w   1c                   when      *inKL
025600070608     c                   eval      DUJret    = '2'
025700070608     c                   eval      $Fine     = *on
025800070608      * - Invio / F6 => Controlli
025900070608x   1c                   other
026000070608     c                   exsr      DftW01
026100070606      * - F6=Conferma (eseguire ricezione)
026200070608if  2c                   if        *inKF
026300070608     c                   exsr      Call_TNSDUJ3C
026400070608e   2c                   endif
026500070606e   1c                   endsl
026600070606      *
026700070606     c                   ENDSR
026800070608      *
026900070608      *---------------------------------------------------------------*
027000070608      *?Visualizzazione collegamenti degli oggetti                   ?*
027100070608      *---------------------------------------------------------------*
027200070608     c     DspLNK        BEGSR
027300070608      *
027400070608     c                   if        %subst(KNSIF:7:1) = 'P'
027500070608     c                   eval      Qcmd      = C_DspLNK_a
027600070608     c                                       + %trimr(C_DirAS888)
027700070608     c                                       + C_DspLNK_b
027800070608     c                   else
027900070608     c                   eval      Qcmd      = C_DspLNK_a
028000070608     c                                       + %trimr(C_DirSETRAS)
028100070608     c                                       + C_DspLNK_b
028200070608     c                   endif
028300070608      *
028400070608     c                   call      'QCMDEXC'                            90
028500070608     c                   parm                    Qcmd
028600070608     c                   parm                    Qlen
028700070608      *
028800070608     c                   if        *in90
028900070608     c                   seton                                        2850
029000070608     c                   eval      V1Dmsg    = $Msg( 5)
029100070608     c                   endif
029200070608      *
029300070608     c                   ENDSR
029400070608      *
029500070608      *---------------------------------------------------------------*
029600070608      *?Visualizzazione collegamenti degli oggetti                   ?*
029700070608      *---------------------------------------------------------------*
029800070608     c     Call_TNSDUJ3C BEGSR
029900070608      *
030000070608     c                   if        %subst(KNSIF:7:1) = 'P'
030100070608     c                   eval      DUJ3dir   = C_IFSas888
030200070608     c                   else
030300070608     c                   eval      DUJ3dir   = C_IFSsetras
030400070608     c                   endif
030500070608      *
030600070608     c                   call      'TNSDUJC3'
030700070608     c                   parm                    DUJ3dir
030800070608     c                   parm      W1Cfpc        DUJ3csv
030900070608     c                   parm      *off          DUJ3err
031000070608      *
031100070608if  1c                   if        DUJ3err   = *on
031200070608     c                   seton                                        2850
031300070608     c                   eval      V1Dmsg    = $Msg( 6)
031400070608x   1c                   else
031500070608     c                   eval      $Fine     = *on
031600070608e   1c                   endif
031700070608      *
031800070608     c                   ENDSR
031900070608      *
032000070608      *---------------------------------------------------------------*
032100070608      *?Impostazione dei valori di default in W01 (se mancanti)      ?*
032200070608      *---------------------------------------------------------------*
032300070608     c     DftW01        BEGSR
032400070608      *
032500070608      * Directory in cui è posto il file da trasmettere
032600070608if  1c                   if        W1Ddir    = *blanks
032700070608if  2c                   if        %subst(KNSIF:7:1) = 'P'
032800070608     c                   eval      W1Ddir    = C_DirAS888
032900070608x   2c                   else
033000070608     c                   eval      W1Ddir    = C_DirSETRAS
033100070608e   2c                   endif
033200070608e   1c                   endif
033300070608      *
033400070608      * Nome File da trasmettere da PC as AS/400
033500070608if  1c                   if             W1Cfpc  = *blanks
033600070608     c                             and  $InzW01 = *off
033700070608     c                   eval      W1Cfpc    = 'Barcode.csv'
033800070608e   1c                   endif
033900070608      *
034000070608     c                   ENDSR
034100070601      *
034200070601      *---------------------------------------------------------------*
034300070601      *?Passaggio barcode da WFAIM20F a WFAIM00F                     ?*
034400070601      *---------------------------------------------------------------*
034500070606     c     OpzS01_2      BEGSR
034600070601      *
034700070606      *?VIENE UTILIZZATO IL DSPF TNSDUJD2?
034800070606      *
034900070607     c                   eval      $Fine     = *on
035000070607      *
035100070601      * Nuovo inserimento multiplo unità:
035200070601      * 1) Creazione + "Blocco" dell'inserimento multiplo unità (trk 1)
035300070606      * 2) Caricamento subfile SDUJS01 con i dati di WFAIM20F
035400070606      * 3) Manutenzione dell'inserimento multiplo unità         (trk.2)
035500070606      * 4) "Sblocco" dell'inserimento multiplo unità            (trk.1)
035600070606      *
035700070604      * 1) Gestione rec. trk "1" per
035800070604      *    Inserimento e "Blocco" dell'inserimento multiplo unità
035900070604     c                   eval      DUJopz    = '1'
036000070601     c                   exsr      Call_TNSDUJ1
036100070601if  1c                   if             DUJerr <> *off
036200070601     c                             or   DUJret <> *off
036300070601     c                   leavesr
036400070601e   1c                   endif
036500070606     c                   eval      DUJopz    = SavOpz
036600070601      *
036700070606      * 2) Inizializzazione subfile per chiamata al pgm. TNSDUJR2
036800070606     c                   exsr      InzS01
036900070606      *    - Ritorno con errore se nessun record è stato caricato
037000070606      *      (dopo la cancellazione del trk "1", creato al punto 1)
037100070606if  1c                   if        MAXnrr    = *zeros
037200070606     c                   eval      DUJopz    = 'd'
037300070606     c                   exsr      Call_TNSDUJ1
037400070606     c                   eval      DUJerr    = *on
037500070606     c                   eval      DUJmsg    = $Msg( 1)
037600070606     c                   leavesr
037700070606e   1c                   endif
037800070601      *
037900070604      * 3) Gestione rec. trk "2" per
038000070605      *    Immissione/Conferma dell'inserimento multiplo unità
038100070606     c                   eval      DUJopz    = 'A'
038200070607     c                   eval      DUJprg    = MAXnrr
038300070601     c                   exsr      Call_TNSDUJ2
038400070605      *
038500070605      * 4) "Sblocco" dell'inserimento multiplo unità al termine
038600070606if  1c                   IF             DUJret  = *off
038700070604     c                             and  DUJerr  = *off
038800070606      *    - Gestione rec. trk "1" per
038900070606      *      "Sblocco" dell'inserimento multiplo unità
039000070604     c                   eval      DUJopz    = '8'
039100070604     c                   exsr      Call_TNSDUJ1
039200070606      *    - Cancellazione records da WFAIM20F
039300070606     c                   close     WFAIM20F
039400070606     c                   call      'TNSDUJC'
039500070606     c                   parm      '2'           DUJfclr
039600070606x   1c                   ELSE
039700070606      *    - Cancellazione del trk "1", creato al punto 1
039800070606     c                   eval      DUJopz    = 'd'
039900070606     c                   exsr      Call_TNSDUJ1
040000070606e   1c                   ENDIF
040100070601      *
040200070601     c                   ENDSR
040300070601      *
040400070601      *---------------------------------------------------------------*
040500070601      *?Cancellazione barcode da WFAIM20F                            ?*
040600070601      *---------------------------------------------------------------*
040700070606     c     OpzS01_4      BEGSR
040800070601      *
040900070607      *?VIENE UTILIZZATO IL DSPF TNSDUJD3?
041000070606      *
041100070607if  1c                   if        NOT  %open(TNSDUJD3)
041200070607     c                   open      TNSDUJD3
041300070607e   1c                   endif
041400070607      *
041500070601      * Inizializzazione della videata
041600070607      *   (NON ha campi di I/O !!!)
041700070601      *
041800070601      * Emissione della window di conferma
041900070607     c                   exfmt     SDUJW02
042000070604     c                   setoff                                       28  90
042100070604     c                   clear                   V1Dmsg
042200070604      *
042300070604sel 1c                   SELECT
042400070601      * - F12 = Ritorno
042500070604w   1c                   WHEN      *inKL
042600070604     c                   eval      DUJret    = '2'
042700070604     c                   eval      $Fine     = *on
042800070601      * - F6 = Conferma
042900070604w   1c                   WHEN      *inKF
043000070604      *   Cancellazione records da WFAIM20F
043100070606     c*** mai aperto:    close     WFAIM20F
043200070604     c                   call      'TNSDUJC'
043300070604     c                   parm      '2'           DUJfclr
043400070604     c                   eval      $Fine     = *on
043500070604      *
043600070604e   1c                   ENDSL
043700070601      *
043800070601     c                   ENDSR
043900070601      *
044000070601      *---------------------------------------------------------------*
044100070601      *?Visualizzazione barcode di WFAIM20F                          ?*
044200070601      *---------------------------------------------------------------*
044300070606     c     OpzS01_5      BEGSR
044400070601      *
044500070606      *?VIENE UTILIZZATO IL DSPF TNSDUJD2?
044600070606      *
044700070604      * Inizializzazione della videata
044800070604      *
044900070604if  1c                   if        $InzS01   = *on
045000070604     c                   exsr      InzS01
045100070604     c                   eval      $InzS01   = *off
045200070604e   1c                   endif
045300070604      *
045400070604      * Scrittura di testata e riga tasti funzionali abilitati
045500070604     c                   write     SDUJT01
045600070604     c                   write     SDUJZ01
045700070604      *
045800070604      * Visualizzazione (eventuale) del msg per la mancanza di dati
045900070604if  1c                   if        C01rcd   <= *zeros
046000070604     c                   write     SDUJD00
046100070604e   1c                   endif
046200070604      *
046300070604      * Emissione della videata
046400070604     c                   exfmt     SDUJC01
046500070604     c                   setoff                                       28  90
046600070604     c                   clear                   V1Dmsg
046700070604if  1c                   if        C01csr    > *zeros
046800070604     c                   eval      C01rcd    = C01csr
046900070604e   1c                   endif
047000070604      *
047100070604sel 1c                   SELECT
047200070604      * F3=Fine
047300070604w   1c                   WHEN      *inKC
047400070604     c                   eval      DUJret    = '1'
047500070604     c                   eval      $Fine     = *on
047600070604      * F12=Ritorno
047700070604w   1c                   WHEN      *inKL
047800070604     c                   eval      DUJret    = '2'
047900070604     c                   eval      $Fine     = *on
048000070604e   1c                   ENDSL
048100070601      *
048200070601     c                   ENDSR
048300070604      *
048400070604      *---------------------------------------------------------------*
048500070606      *?Inizializzazione videata S01 del display-file TNSDUJD2       ?*
048600070604      *---------------------------------------------------------------*
048700070604     c     InzS01        BEGSR
048800070604      *
048900070606      *?VIENE UTILIZZATO IL DSPF TNSDUJD2?
049000070606      *
049100070606      * Override al dispaly-file per abilitare la condivisione al sfl
049200070607     c                   call      'QCMDEXC'
049300070607     c                   parm      C_OvrShareD2  Qcmd
049400070607     c                   parm                    Qlen
049500070606     c                   open      TNSDUJD2
049600070606      *
049700070606      * "Intestazione" testata  (qui gestita se opz. 5=Visualizzazione)
049800070606if  1c                   if        DUJopz    = '5'
049900070604     c                   eval      V1Tpgm    = SDSpgm
050000070604     c                   eval      V1Ttxt    = ' '
050100070604     c                                       + 'V I S U A L I Z Z A +
050200070604     c                                          Z I O N E'
050300070606e   1c                   endif
050400070604      *
050500070606      * "Intestazione" del subfile-control (gestito dal pgm. TNSDUJR2)
050600070604     c                   clear                   SDUJC01
050700070606if  1c                   if        DUJopz    = '2'
050800130916     c                   movel     DUJuni1       dWIMuni1
050900130916     c                   movel     DUJuni2       dWIMuni12
051000070606     c                   eval      C1Dnum    = DUJnum
051100130919     c                   eval      C1Ddes    = §WIM1des + §WIM12des
051200070606     c                   eval      C1Dnmi    = §WIM1nmi
051300070606e   1c                   endif
051400070604      *
051500070604      * Pulizia subfile
051600070604     c                   seton                                        3031
051700070604     c                   write     SDUJC01
051800070604     c                   setoff                                         3133
051900070604      *
052000070604     c                   reset                   $EoF
052100070604     c***                clear                   W_SflPag
052200070604     c***                clear                   C01csr
052300070604     c                   clear                   C01rcd
052400070604     c                   clear                   S01nrr
052500070604     c                   clear                   V1Dmsg
052600070604     c                   setoff                                       28  90
052700070604     c                   movea     *zeros        *in(50)
052800070604      *
052900070604      * Caricamento eventuali dati da presentare nel sfl
053000070604     c                   clear                   $Matr
053100070604     c                   clear                   MAXnrr
053200070605     c                   read      WFAIM200
053300070604     c                   eval      $EoF      = (%eof(WFAIM20F))
053400070604     c                   exsr      RollUpS01
053500070605      *
053600070605      * Reimpostazione condizioni ai vari DSPATR...
053700070606     c                   eval      *in45     = (DUJopz = '5')
053800070604      *
053900070604      * Posizionamento cursore al 1° rcd del sfl
054000070604if  1c                   if        S01nrr    > *zeros
054100070604     c                   eval      C01rcd    = 1
054200070604x   1c                   else
054300070604     c                   clear                   C01rcd
054400070604e   1c                   endif
054500070604      *
054600070604     c                   ENDSR
054700070604      *
054800070604      *---------------------------------------------------------------*
054900070604      *?Caricamento dati nel subfile S01                             ?*
055000070604      *---------------------------------------------------------------*
055100070604     c     RollUpS01     BEGSR
055200070604      *
055300070604     c***                eval      S01nrr    = (W_SflPag * C_SflPag)
055400070604     c***                add       1             W_SflPag
055500070604     c                   eval      *in32     = *off
055600070604      *
055700070604      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
055800070604do  1c                   doW            $EoF   = *off
055900070604     c*** carica tutto:            and  S01nrr < W_SflPag * C_SflPag
056000070604     c                   exsr      CarS01
056100070605     c                   read      WFAIM200
056200070604     c                   eval      $EoF      = (%eof(WFAIM20F))
056300070604e   1c                   enddo
056400070604      *
056500070604      * Visualizzazione totale unità già inserite
056600070604     c                   eval      C1Cprg    = MAXnrr
056700070604      *
056800070604      * Visualizzazione del SFL (se ci sono dati)
056900070604     c                   eval      *in30     = (S01nrr = *zeros)
057000070604      *
057100070604      * Attivazione (eventuale) del SFLEND
057200070604     c                   eval      *in33     = $EoF
057300070604      *
057400070604     c                   ENDSR
057500070604      *
057600070604      *---------------------------------------------------------------*
057700070604      *?Caricamento singolo rec. dati da file WFAIM00F in subfile S01?*
057800070604      *---------------------------------------------------------------*
057900070604     c     CarS01        BEGSR
058000070604      *
058100070604     c                   clear                   SDUJS01
058200070604      *
058300070604      * Controlli preliminari:
058400070604      * SE matricola già inserita => uscita (NO caricamento record)
058500070605      * ...Ammesso che NON sia in 5=Visualizzazione
058600070604     c     WIMbcd        lookup    $Matr                                  25
058700070604if  1c                   if        *in25
058800070605     c                             and  DUJopz <> '5'
058900070604     c                   leavesr
059000070604e   1c                   endif
059100070604      * ALTRIMENTI => gestione codice matricola - anche se anomalo
059200070604      *
059300070604      * Impostazione campi del subfile
059400070604     c                   eval      S1Cmat    = WIMbcd
059500070604     c                   eval      S1Cprg    = MAXnrr + 1
059600070604      *
059700070604      * Impostazione indicatori
059800070604     c                   eval      *in41     = *off
059900070606     c                   eval      *in45     = (DUJopz = '5')
060000070604      *
060100070604      * Scrittura record subfile
060200070604     c                   add       1             S01nrr
060300070604     c                   write     SDUJS01
060400070604     c                   eval      MAXnrr    = S01nrr
060500070604      *
060600070604      * Intabellamento codice matricola appena inserito
060700070604     c                   eval      $Matr(S01nrr) = WIMbcd
060800070604      *
060900070604     c                   ENDSR
061000070601      *
061100070601      *---------------------------------------------------------------*
061200070601      *?Richiamo del pgm. TNSDUJR1 (gestione trk. "1" file WFAIM00F) ?*
061300070601      *---------------------------------------------------------------*
061400070601     c     Call_TNSDUJ1  BEGSR
061500070601      *
061600070601     c                   movel(p)  TNSDUJds      KPJBU
061700070601     c                   call      'TNSDUJR1'
061800070601     c                   parm                    KPJBA
061900070601     c                   movel     KPJBU         TNSDUJds
062000070601      *
062100070601sel 1c                   SELECT
062200070601      * Ritorno normale
062300070601w   1c                   WHEN           DUJret  = *off
062400070601     c                             and  DUJerr  = *off
062500130916if  2c                   if        %subst(DUJuni1:1:40) <> *blanks
062600130916     c                   movel     DUJuni1       dWIMuni1
062700130916     c                   movel     DUJuni2       dWIMuni12
062800070606e   2c                   endif
062900070601      * Ritorno con errore
063000070601w   1c                   WHEN      DUJerr    = *on
063100070608     c*** sarà comunque: eval      $Fine     = *on
063200070601      * Ritorno con F3
063300070601w   1c                   WHEN      DUJret    = '1'
063400070608     c*** sarà comunque: eval      $Fine     = *on
063500070601      * Ritorno con F12
063600070601w   1c                   WHEN      DUJret    = '2'
063700070608     c*** sarà comunque: eval      $Fine     = *on
063800070601      *
063900070601e   1c                   ENDSL
064000070601      *
064100070601     c                   ENDSR
064200070601      *
064300070601      *---------------------------------------------------------------*
064400070601      *?Richiamo del pgm. TNSDUJR2 (gestione trk. "2" file WFAIM00F) ?*
064500070601      *---------------------------------------------------------------*
064600070601     c     Call_TNSDUJ2  BEGSR
064700070601      *
064800070601      * Gestione rec. trk "2" per
064900070601      * Immissione dell'inserimento multiplo unità
065000070601     c                   movel(p)  TNSDUJds      KPJBU
065100070601     c                   call      'TNSDUJR2'
065200070601     c                   parm                    KPJBA
065300070601     c                   movel     KPJBU         TNSDUJds
065400070601      *
065500070601sel 1c                   SELECT
065600070601      * - Ritorno normale
065700070601w   1c                   WHEN           DUJret  = *off
065800070601     c                             and  DUJerr  = *off
065900070608     c*** inutile:       leavesr
066000070601      * - Ritorno con errore
066100070601w   1c                   WHEN      DUJerr    = *on
066200070608     c*** sarà comunque: eval      $Fine     = *on
066300070601      * - Ritorno con F3
066400070601w   1c                   WHEN      DUJret    = '1'
066500070608     c*** sarà comunque: eval      $Fine     = *on
066600070601      * - Ritorno con F12
066700070601w   1c                   WHEN      DUJret    = '2'
066800070608     c*** sarà comunque: eval      $Fine     = *on
066900070601      *
067000070601     c                   ENDSL
067100070604      *
067200070604     c                   ENDSR
067300070608      *
067400070608      *---------------------------------------------------------------*
067500070608      *?Operazioni Finali                                            ?*
067600070608      *---------------------------------------------------------------*
067700070608     c     RoutEnd       BEGSR
067800070608      *
067900070608      * Chiusura archivi
068000070608      *
068100070608if  1c                   if        %open(TNSDUJD3)
068200070608     c                   close     TNSDUJD3
068300070608e   1c                   endif
068400070608if  1c                   if        %open(TNSDUJD2)
068500070608     c                   close     TNSDUJD2
068600070608e   1c                   endif
068700070608      *
068800070608      * Disallocazione workfile WFAIM20F
068900070608if  1c                   IF        $AlcWIM20 = *on
069000070608if  2c                   if        %open(WFAIM20F)
069100070608     c                   close     WFAIM20F
069200070608e   2c                   endif
069300070608     c                   call(e)   'QCMDEXC'
069400070608     c                   parm      C_DlcWFAIM20  Qcmd
069500070608     c                   parm                    Qlen
069600070608e   1c                   ENDIF
069700070608      *
069800070608     c                   ENDSR
069900070604      *
070000070604** - $Msg ....+....2....+....3....+....4....+....5....+....6....+....7....+...
070100070606Dati già in uso da altro lavoro...                                              1
070200070606Non trovati record nel workfile WFAIM20F                                        2
070300070606             Premere F6 per             |                                       3
070400070606 confermare la ricezione di dati da PC. |                                       4
070500070608Rilevati errori nel tentativo di visualizzare i dati della directory impostata  5
070600070608Rilevati errori nella trasmissione del file da PC su AS/400...                  6
