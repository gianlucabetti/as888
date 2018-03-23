000100070411      *PARMS DBGVIEW(*SOURCE)
000200070129      *===============================================================*
000300070417      *?TNSDUJR - Inserimento multiplo unità EDP                     ?*
000400070129      *===============================================================*
000500070129
000600070129     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700070129
000800070129      *---------------------------------------------------------------*
000900070129
001000070605     fWFAIM01L  if   e           k disk    usropn
001100070604     fWFAIM20F  if   e           k disk    usropn
001200070129      *
001300070417     fTNSDUJD   cf   e             workstn
001400070417     f                                     sfile(SDUJS01:S01nrr)
001500070129     f                                     infds(DSFMT)
001600070129
001700070129      *---------------------------------------------------------------*
001800070129
001900070403      *
002000070410      *?Costanti  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
002100070129      *
002200070129      * - Tasti di funzione
002300070405     d C_RollUp        c                   const(x'F5')
002400070129      * - Nr. di righe del sfl (SFLPAG)
002500070411     d C_SflPag        c                   const(16)
002600070129      *
002700070410      *?Schiere - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
002800070129      *
002900070129      * - Messaggi di errore
003000070604     d $Msg            s             78    dim( 3)   ctdata  perrcd(1)
003100070129      *
003200070410      *?Ds  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
003300070129      *
003400070129     d KPJBA         e ds
003500070129      * - Parametri x Controllo profilo utenti
003600070416     d TIBS34ds      e ds                  inz
003700070129      * - Ds di riferimento al file esterno AZUTE00F
003800070129     d AZUTEds       e ds                  extname(AZUTE00F)
003900070129      * - Ds per dati organigramma
004000070416     d dDatiUte      e ds
004100070129      *
004200070508      * DS campo WIMUNI file WFAIM00F trk "1"
004300070508     d dWIMuni1      e ds                  inz
004400070411      *
004500070417      * Parametri per pgm. TNSDUJR1/R2
004600070417     d TNSDUJds      e ds                  inz
004700070129      *
004800070129     d Status         sds           333
004900070129     d   SDSpgm          *proc
005000070129      *
005100070129     d dsFMT           ds           512
005200070129     d  £Tasto               369    369
005300070129      *
005400070410      *?Variabili - - - - - - - - - - - - - - - - - - - - - - - - - -?*
005500070129      *
005600070129      * - Flags
005700070411     d $EoF            s              1    inz(*off)
005800070601     d $20F            s              1    inz(*off)
005900070129     d $Fine           s              1    inz(*off)
006000070521     d $RgzPfm         s              1    inz(*off)
006100070129     d $InzS01         s              1    inz(*on)
006200070129      *
006300070129      * - Indici di schiera
006400070129      *
006500070129      * - Variabili riferite al data base o al display file
006600070129     d S01nrr          s                   like(C01rcd) inz
006700070601     d wRec20F         s                   like(S1Cnmi) inz
006800070201      *
006900070129      * - Variabili definite a programma
007000070129     d wPag            s              4  0 inz
007100070129     d wRig            s              3  0 inz
007200070411     d W_SflPag        s              3  0 inz
007300070604      *
007400070604      * - Parametri per pgm. TNSDUJC = Riorganizzazione WF
007500070604     d DUJfclr         s              1    inz('0')
007600070129      *
007700070410      *?Key-List  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
007800070129      *
007900070508     c     K02WIM01      klist
008000070508     c                   kfld                    WIMnum
008100070508     c                   kfld                    WIMtrk
008200070129
008300070129      *---------------------------------------------------------------*
008400070410      *?RIEPILOGO INDICATORI                                         ?*
008500070129      *---------------------------------------------------------------*
008600070416      * 06    - Abilitazione F6=Conferma in C01/S01                   *
008700070503      * 12    - Abilitazione F12=Ritorno in C01/S01                   *
008800070410      * 25    - Comodo                                                *
008900070416      * 28    - Emissione del messaggio di errore a video             *
009000070416      * 30    - Pulizia      del subfile S01 (sflclr, NO sfldsp)      *
009100070416      * 31    - NO emissione del subfile S01 (sflclr, NO sfldspctl)   *
009200070129      * 32    - Record modificato nel subfile S01 (sflnxtchg)         *
009300070416      * 33    - Fine dati         nel subfile S01 (sflend)            *
009400070601      * 41    - S01: Evidenziazione sflrec con DSPATR(RI)             *
009500070601      * 42    - S01: Evidenziazione sflrec con DSPATR(HI BL)          *
009600070601      * 43    - S01: Evidenziazione sflrec con COLOR(PNK)             *
009700070411      * 50    - S01: Posizionamento cursore sul campo "Opzione"       *
009800070129      * 90    - Generico di errore                                    *
009900070129      *---------------------------------------------------------------*
010000070129
010100070129     c     *Entry        plist
010200070129     c                   parm                    KPJBA
010300070129      *
010400070129      * Operazioni Iniziali
010500070129     c                   exsr      RoutInz
010600070129      *
010700070129      * Gestione Video
010800070411      * - SubFile x visualizzazione   trk "1" già inseriti (testata)
010900070129do  1c                   DOW       $Fine     = *off
011000070518     c                   exsr      GesS01
011100070129e   1c                   ENDDO
011200070518      *
011300070521      * "Riorganizzazione" work-file (se "modificato")
011400070521if  1c                   if        $RgzPfm   = *on
011500070518     c                   close     WFAIM01L
011600070523     c                   call      'TNSDUJC'
011700070604     c                   parm      '0'           DUJfclr
011800070518e   1c                   endif
011900070129      *
012000070129      * Fine
012100070417     c                   movel(p)  TNSDUJds      KPJBU
012200070129     c                   movel     *on           *inLR
012300070129      *
012400070129      *---------------------------------------------------------------*
012500070410      *?Operazioni Iniziali                                          ?*
012600070129      *---------------------------------------------------------------*
012700070129     c     RoutInz       BEGSR
012800070129      *
012900070130      * Reperimento dati job
013000070129     c                   exsr      DatiJob
013100070129      *
013200070129     c                   movel     SDSpgm        V1Tpgm
013300070416      *
013400070416      * Ricezione parametri
013500070417     c                   clear                   TNSDUJds
013600070417if  1c                   if        KPJBU    <> *blanks
013700070417     c                   movel     KPJBU         TNSDUJds
013800070417if  2c                   if        DUJopz    = '1'
013900070417     c                   clear                   DUJnum
014000070417e   2c                   endif
014100070417e   1c                   endif
014200070417     c                   eval      DUJuni    = *blanks
014300070417     c                   eval      DUJret    = *off
014400070417     c                   eval      DUJerr    = *off
014500070417     c                   eval      DUJmsg    = *blanks
014600070417      *
014700070417     c                   eval      *in06     = (DUJopz <> '1')
014800070503     c                   eval      *in12     = (DUJopz  = '1')
014900070605      *
015000070605     c                   open      WFAIM01L
015100070129      *
015200070129     c                   ENDSR
015300070129      *
015400070129      *---------------------------------------------------------------*
015500070410      *?Reperimento Dati del job (Utente/Operativi)                  ?*
015600070129      *---------------------------------------------------------------*
015700070129     c     DatiJob       BEGSR
015800070129      *
015900070129     c     *dtaara       define    §azute        azuteds
016000070129     c     *dtaara       define    §datiute      ddatiute
016100070129      *
016200070129     c                   in(E)     *dtaara
016300070129     c                   IF        %ERROR or RSUT = *blanks
016400070129     c                   clear                   Tibs34Ds
016500070129     c                   call      'TIBS34R'
016600070129     c                   parm                    Tibs34Ds
016700070129     c                   in        *dtaara
016800070129     c                   ENDIF
016900070129      *
017000070129     c                   ENDSR
017100070129      *
017200070129      *---------------------------------------------------------------*
017300070410      *?Gestione videata S01                                         ?*
017400070129      *---------------------------------------------------------------*
017500070129     c     GesS01        BEGSR
017600070129      *
017700070130      * Inizializzazione della videata
017800070129if  1c                   if        $InzS01   = *on
017900070129     c                   exsr      InzS01
018000070129     c                   eval      $InzS01   = *off
018100070129e   1c                   endif
018200070129      *
018300070130      * Scrittura di testata e riga tasti funzionali abilitati
018400070417     c                   write     SDUJT01
018500070417     c                   write     SDUJZ01
018600070129      *
018700070130      * Visualizzazione (eventuale) del msg per la mancanza di dati
018800070403if  1c                   if        C01rcd   <= *zeros
018900070417     c                   write     SDUJD00
019000070129e   1c                   endif
019100070129      *
019200070130      * Emissione della videata
019300070417     c                   exfmt     SDUJC01
019400070405if  1c                   if        C01csr    > *zeros
019500070405     c                   eval      C01rcd    = C01csr
019600070405e   1c                   endif
019700070129     c                   setoff                                       28  90
019800070129     c                   clear                   V1Dmsg
019900070129      *
020000070129sel 1c                   SELECT
020100070129      * F3=Fine
020200070129w   1c                   WHEN      *inKC
020300070129     c                   exsr      F03S01
020400070129      * F5=Rivisualizzazione
020500070129w   1c                   WHEN      *inKE
020600070129     c                   exsr      F05S01
020700070129      * F6=Nuovo inserimento
020800070129w   1c                   WHEN      *inKF
020900070129     c                   exsr      F06S01
021000070503      * F12=Ritorno
021100070503w   1c                   WHEN      *inKL
021200070503     c                   exsr      F12S01
021300070607      * F13=Ricezione dati da PC
021400070607w   1c                   WHEN      *inKM
021500070607     c                   exsr      F13S01
021600070129      * Roll-UP
021700070405w   1c                   WHEN      £Tasto    = C_RollUp
021800070129     c                   exsr      RollUpS01
021900070129      * NO rec.
022000070410w   1c                   WHEN      S01nrr   <= *zeros
022100070129      *
022200070129      * Gestione opzioni inserite
022300070129x   1c                   OTHER
022400070129     c                   exsr      OpzS01
022500070129e   1c                   ENDSL
022600070129      *
022700070129     c                   ENDSR
022800070129      *
022900070129      *---------------------------------------------------------------*
023000070410      *?Inizializzazione videata S01                                 ?*
023100070129      *---------------------------------------------------------------*
023200070129     c     InzS01        BEGSR
023300070129      *
023400070130      * "Intestazione" del subfile-control
023500070416     c                   clear                   C1Dtxt
023600070130      *
023700070129      * Pulizia subfile
023800070129     c                   seton                                        3031
023900070417     c                   write     SDUJC01
024000070129     c                   setoff                                         3133
024100070129      *
024200070601     c                   reset                   $20F
024300070411     c                   reset                   $EoF
024400070411     c                   clear                   W_SflPag
024500070405     c***                clear                   C01csr
024600070129     c                   clear                   C01rcd
024700070129     c                   clear                   S01nrr
024800070129     c                   clear                   V1Dmsg
024900070129     c                   setoff                                       28  90
025000070129     c                   movea     *zeros        *in(50)
025100070601      *
025200070605      * Verifica esistenza dati da file PC (.csv)
025300070601      *   (se NON fase di selezione per conferma)
025400070605      *   (& se NON file già allocato)
025500070601if  1c                   if        *in06
025600070605     c                   open(e)   WFAIM20F
025700070605     c                   if        NOT %error
025800070601     c                   exsr      CheckWFAIM20F
025900070604     c                   close     WFAIM20F
026000070605     c                   endif
026100070601e   1c                   endif
026200070129      *
026300070129      * Caricamento 1ª pagina dei dati da presentare nel sfl
026400070508     c     *loval        setll     WFAIM000
026500070508     c                   read      WFAIM000
026600070508     c                   eval      $EoF      = (%eof(WFAIM01L))
026700070601sel 1c                   select
026800070601w   1c                   when      $EoF      = *off
026900070416     c                   eval      C1Dtxt    = 'Rilevati precedenti in+
027000070411     c                                         serimenti non ancora co+
027100070411     c                                         nfermati:'
027200070129     c                   exsr      RollUpS01
027300070601w   1c                   when      $20F      = *on
027400070605     c                   exsr      CarS01csv
027500070601     c                   eval      *in30     = (S01nrr = *zeros)
027600070601     c                   eval      *in33     = *on
027700070601     c                   eval      C01rcd    = S01nrr
027800070601e   1c                   endsl
027900070129      *
028000070129     c                   ENDSR
028100070601      *
028200070601      *---------------------------------------------------------------*
028300070601      *?Caricamento pagina successiva di dati S01                    ?*
028400070601      *---------------------------------------------------------------*
028500070601     c     CheckWFAIM20F BEGSR
028600070601      *
028700070601      * Verifica esistenza dati
028800070601     c                   read      WFAIM200
028900070601if  1c                   if        %eof(WFAIM20F)
029000070601     c                   leavesr
029100070601e   1c                   endif
029200070601      *
029300070601      * Conteggio barcode da inserire
029400070601     c                   clear                   wRec20F
029500070601do  1c                   dow       NOT  %eof(WFAIM20F)
029600070601if  2c                   if        WIMbcd   <> *blanks
029700070601     c                   add       1             wRec20F
029800070601e   2c                   endif
029900070601     c                   read      WFAIM200
030000070601e   1c                   enddo
030100070601      *
030200070605      * Impostazione segnalazione di esistenza dati di file PC (.csv)
030300070601      *   da confermare
030400070601     c                   eval      $20F      = (wRec20F > *zeros)
030500070601      *
030600070601      * Impostazione testata
030700070601if  1c                   if            $20F    = *on
030800070601     c                             and C1Dtxt  = *blanks
030900070605     c                   eval      C1Dtxt    = 'Rilevati barcode da fi+
031000070605     c                                         le PC (.csv):'
031100070605     c***                eval      C1Dtxt    = 'Rilevato file PC (.csv+
031200070605     c***                                      ) di BARCODE non ancora+
031300070605     c***                                       confermati:'
031400070601e   1c                   endif
031500070601      *
031600070601     c                   ENDSR
031700070129      *
031800070129      *---------------------------------------------------------------*
031900070410      *?Caricamento pagina successiva di dati S01                    ?*
032000070129      *---------------------------------------------------------------*
032100070129     c     RollUpS01     BEGSR
032200070129      *
032300070411     c                   eval      S01nrr    = (W_SflPag * C_SflPag)
032400070411     c                   add       1             W_SflPag
032500070129     c                   eval      *in32     = *off
032600070601      *
032700070605      * Seganazione di barcode nel file WFAIM20F da file PC (.csv)
032800070601if  1c                   if             $20F   = *on
032900070601     c                             and  S01nrr = *zeros
033000070605     c                   exsr      CarS01csv
033100070601e   1c                   endif
033200070129      *
033300070129      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
033400070411do  1c                   doW            $EoF   = *off
033500070411     c                             and  S01nrr < W_SflPag * C_SflPag
033600070129     c                   exsr      CarS01
033700070517     c     WIMnum        setgt     WFAIM000
033800070508     c                   read      WFAIM000
033900070508     c                   eval      $EoF      = (%eof(WFAIM01L))
034000070129e   1c                   enddo
034100070129      *
034200070129      * Visualizzazione del SFL (se ci sono dati)
034300070129     c                   eval      *in30     = (S01nrr = *zeros)
034400070129      *
034500070129      * Attivazione (eventuale) del SFLEND
034600070601     c                   eval      *in33     = $EoF
034700070129      *
034800070129      * Posizionamento cursore al 1° rcd della pagina
034900070129if  1c                   if        S01nrr    > *zeros
035000070411     c     S01nrr        div       C_SflPag      wPag
035100070129     c                   mvr                     wRig
035200070411     c                   eval      C01rcd    = wPag * C_SflPag
035300070129     c                   if        wRig      > *zeros
035400070129     c                   eval      C01rcd    = C01rcd + 1
035500070129     c                   else
035600070411     c                   eval      C01rcd    = C01rcd - C_SflPag + 1
035700070129     c                   endif
035800070129x   1c                   else
035900070129     c                   clear                   C01rcd
036000070129e   1c                   endif
036100070129      *
036200070129     c                   ENDSR
036300070601      *
036400070601      *---------------------------------------------------------------*
036500070601      *?Caricamento dati nel subfile S01 da barcode del file WFAIM20F?*
036600070601      *---------------------------------------------------------------*
036700070605     c     CarS01csv     BEGSR
036800070601      *
036900070601      * Caricamento dati nel record del sfl
037000070601     c                   clear                   SDUJS01
037100070601     c                   eval      H1in41    = *off
037200070601     c                   eval      H1in42    = *off
037300070601     c                   eval      H1in43    = *on
037400070601     c***                eval      S1Cnum    = *hival
037500070601     c***                eval      S1Cdes    = *blanks
037600070601     c***                eval      S1Cnmi    = *zeros
037700070601     c***                eval      S1Cute    = *blanks
037800070601sel 1c                   select
037900070601w   1c                   when      H1in42    = *on
038000070605     c                   eval      S1Cnot    = 'Confermato       '
038100070601w   1c                   when      H1in41    = *on
038200070605     c                   eval      S1Cnot    = 'In uso...        '
038300070601w   1c                   when      H1in43    = *on
038400070605     c                   eval      S1Cnot    = 'file PC (.csv)   '
038500070601e   1c                   endsl
038600070605     c                   eval      S1Dtxt    = 'Dati derivanti da docu+
038700070606     c                                         mento su PC (.CSV)  -  +
038800070606     c                                         da elaborare con opz. 2'
038900070601      * - Dati di "totale"
039000070601     c                   eval      S1Cnma    = wRec20F
039100070601      *
039200070601      * Impostazione indicatori
039300070601     c                   eval      *in41     = (H1in41 = *on)
039400070601     c                   eval      *in42     = (H1in42 = *on)
039500070601     c                   eval      *in43     = (H1in43 = *on)
039600070601      *
039700070601      * Scrittura record subfile
039800070601     c                   add       1             S01nrr
039900070601     c                   write     SDUJS01
040000070601      *
040100070601     c                   ENDSR
040200070129      *
040300070129      *---------------------------------------------------------------*
040400070410      *?Caricamento dati in subfile S01                              ?*
040500070129      *---------------------------------------------------------------*
040600070129     c     CarS01        BEGSR
040700070129      *
040800070517sel 1c                   select
040900070517w   1c                   when           WIMtrk = '1'
041000070517     c                             OR  (WIMtrk = '3'
041100070517     c                             and  *in06  = *on)
041200070508     c                   movel     WIMuni        dWIMuni1
041300070517w   1c                   when           WIMtrk = '2'
041400070517     c                             OR  (WIMtrk = '4'
041500070517     c                             and  *in06  = *on)
041600070508     c                   clear                   dWIMuni1
041700070508     c                   eval      §WIM1des  = *all'? '
041800070508     c                   eval      §WIM1usp  = *all'? '
041900070517x   1c                   other
042000070517     c                   leavesr
042100070518e   1c                   endsl
042200070129      *
042300070129      * Caricamento dati nel record del sfl
042400070417     c                   clear                   SDUJS01
042500070508     c                   eval      H1in41    = (WIMprg = *zeros)
042600070517     c                   eval      H1in42    = (WIMtrk > '2')
042700070601     c                   eval      H1in43    = *off
042800070508     c                   eval      S1Cnum    = WIMnum
042900070508     c                   eval      S1Cdes    = §WIM1des
043000070508     c                   eval      S1Cnmi    = §WIM1nmi
043100070508if  1c                   if        §WIM1uin <> *blanks
043200070508     c                   eval      S1Cute    = §WIM1uin
043300070508x   1c                   else
043400070508     c                   eval      S1Cute    = §WIM1usp
043500070508e   1c                   endif
043600070517sel 1c                   select
043700070517w   1c                   when      H1in42    = *on
043800070605     c                   eval      S1Cnot    = 'Confermato       '
043900070518w   1c                   when      H1in41    = *on
044000070605     c                   eval      S1Cnot    = 'In uso...        '
044100070601w   1c                   when      H1in43    = *on
044200070605     c                   eval      S1Cnot    = 'file PC (.csv)   '
044300070517e   1c                   endsl
044400070606     c                   eval      S1Dtxt    = 'Lunghezza: '
044500070605     c                                       + %editc(§WIM1lm1:'Z')
044600070606     c                                       + '  dalla posizione '
044700070605     c                                       + %editc(§WIM1im2:'Z')
044800070606     c                                       + '  per '
044900070605     c                                       + %editc(§WIM1lm2:'Z')
045000070605     c                                       + ' caratteri'
045100070517      * - Dati di "totale"
045200070517sel 1c                   select
045300070517w   1c                   when      WIMtrk    = '1'
045400070517     c                   eval      WIMtrk    = '2'
045500070517     c     K02WIM01      setll     WFAIM000
045600070517     c     K02WIM01      reade     WFAIM000
045700070517w   1c                   when      WIMtrk    = '3'
045800070517     c                   eval      WIMtrk    = '4'
045900070517     c     K02WIM01      setll     WFAIM000
046000070517     c     K02WIM01      reade     WFAIM000
046100070517e   1c                   endsl
046200070517do  1c                   dow       NOT  %eof(WFAIM01L)
046300070517     c                   add       1             S1Cnma
046400070517     c     K02WIM01      reade     WFAIM000
046500070517e   1c                   enddo
046600070129      *
046700070129      * Impostazione indicatori
046800070508     c                   eval      *in41     = (H1in41 = *on)
046900070517     c                   eval      *in42     = (H1in42 = *on)
047000070601     c                   eval      *in43     = (H1in43 = *on)
047100070129      *
047200070129      * Scrittura record subfile
047300070129     c                   add       1             S01nrr
047400070417     c                   write     SDUJS01
047500070129      *
047600070129     c                   ENDSR
047700070129      *
047800070129      *---------------------------------------------------------------*
047900070410      *?Gestione tasto funzionale F03 da videata S01                 ?*
048000070129      *---------------------------------------------------------------*
048100070129     c     F03S01        BEGSR
048200070129      *
048300070130      * Chiusura del programma
048400070417     c                   eval      DUJret    = '1'
048500070130     c                   eval      $Fine     = *on
048600070129      *
048700070129     c                   ENDSR
048800070129      *
048900070129      *---------------------------------------------------------------*
049000070410      *?Gestione tasto funzionale F05 da videata S01                 ?*
049100070129      *---------------------------------------------------------------*
049200070129     c     F05S01        BEGSR
049300070129      *
049400070130      * Ricarica del subfile
049500070130     c                   eval      $InzS01   = *on
049600070129      *
049700070129     c                   ENDSR
049800070129      *
049900070129      *---------------------------------------------------------------*
050000070410      *?Gestione tasto funzionale F06 da videata S01                 ?*
050100070129      *---------------------------------------------------------------*
050200070129     c     F06S01        BEGSR
050300070129      *
050400070129      * Nuovo inserimento multiplo unità
050500070412      *
050600070412      * 1) Creazione + "Blocco" dell'inserimento multiplo unità (trk 1)
050700070412      * 2) Manutenzione dell'inserimento multiplo unità         (trk.2)
050800070412      * 3) "Sblocco" dell'inserimento multiplo unità            (trk.1)
050900070417     c                   clear                   TNSDUJds
051000070417     c                   eval      DUJopz    = '1'
051100070417     c                   exsr      Call_TNSDUJ2
051200070417     c                   if             DUJerr <> *off
051300070417     c                             or   DUJret <> *off
051400070412     c                   leavesr
051500070412     c                   endif
051600070129      *
051700070129     c                   ENDSR
051800070503      *
051900070503      *---------------------------------------------------------------*
052000070503      *?Gestione tasto funzionale F12 da videata S01                 ?*
052100070503      *---------------------------------------------------------------*
052200070503     c     F12S01        BEGSR
052300070503      *
052400070503      * Chiusura del programma
052500070503     c                   eval      DUJret    = '2'
052600070503     c                   eval      $Fine     = *on
052700070503      *
052800070503     c                   ENDSR
052900070607      *
053000070607      *---------------------------------------------------------------*
053100070607      *?Gestione tasto funzionale F13 da videata S01                 ?*
053200070607      *---------------------------------------------------------------*
053300070607     c     F13S01        BEGSR
053400070607      *
053500070607     c                   close     WFAIM01L
053600070607      *
053700070607     c                   eval      DUJopz    = '1'
053800070607      *
053900070607     c                   movel     TNSDUJds      KPJBU
054000070607     c                   call      'TNSDUJR3'
054100070607     c                   parm                    KPJBA
054200070607     c                   movel     KPJBU         TNSDUJds
054300070607      *
054400070607sel 1c                   select
054500070607w   1c                   when      DUJerr   <> *off
054600070607     c                   seton                                        28  90
054700070607     c                   eval      V1Dmsg    = DUJmsg
054800070607w   1c                   when      DUJret    = *zeros
054900070607     c                   eval      $InzS01   = *on
055000070607e   1c                   endsl
055100070607      *
055200070607     c                   open      WFAIM01L
055300070607      *
055400070607     c                   ENDSR
055500070129      *
055600070129      *---------------------------------------------------------------*
055700070410      *?Controllo opzioni subfile S01                                ?*
055800070129      *---------------------------------------------------------------*
055900070129     c     OpzS01        BEGSR
056000070129      *
056100070417     c                   readc     SDUJS01
056200070129      *
056300070417do  1c                   DOW       NOT %eof(TNSDUJD)
056400070129      *
056500070129     c                   eval      *in32     = *off
056600070129     c                   movea     *zeros        *in(50)
056700070129     c                   z-add     S01nrr        C01rcd
056800070129      *
056900070129sel 2c                   SELECT
057000070129      * - Nessuna opzione
057100070129w   2c                   WHEN      S1Copz    = *blanks
057200070416      * - Opz. 1=Conferma
057300070416w   2c                   WHEN      S1Copz    = '1'
057400070416     c                             and *in06 = *off
057500070518     c                   if        H1in42    = *off
057600070517     c                   exsr      OpzS01_1
057700070518     c                   else
057800070518     c                   seton                                        285090
057900070518     c                   eval      V1Dmsg    = $Msg( 2)
058000070518     c                   endif
058100070405      * - Opz. 2=Gestione
058200070405w   2c                   WHEN      S1Copz    = '2'
058300070416     c                             and *in06 = *on
058400070604sel 3c                   select
058500070605      *   - da file PC (.csv)
058600070604w   3c                   when      H1in43    = *on
058700070604     c                   exsr      Call_TNSDUJ3
058800070604      *   - da confermare
058900070604w   3c                   when      H1in42    = *off
059000070517     c                   exsr      OpzS01_2
059100070604      *   - già confermato
059200070604x   3c                   other
059300070518     c                   seton                                        285090
059400070518     c                   eval      V1Dmsg    = $Msg( 2)
059500070604e   3c                   endsl
059600070404      * - Opz. 4=Cancellazione
059700070404w   2c                   WHEN      S1Copz    = '4'
059800070416     c                             and *in06 = *on
059900070605      *   - da file PC (.csv)
060000070604if  3c                   if        H1in43    = *on
060100070604     c                   exsr      Call_TNSDUJ3
060200070604x   3c                   else
060300070517     c                   exsr      OpzS01_4
060400070604e   3c                   endif
060500070518      * - Opz. 5=Visualizzazione
060600070518w   2c                   WHEN      S1Copz    = '5'
060700070605      *   - da file PC (.csv)
060800070604if  3c                   if        H1in43    = *on
060900070604     c                   exsr      Call_TNSDUJ3
061000070604x   3c                   else
061100070518     c                   exsr      OpzS01_5
061200070604e   3c                   endif
061300070517      * - Opz. 6=RI-attivazione
061400070517w   2c                   WHEN      S1Copz    = '6'
061500070517     c                             and *in06 = *on
061600070604if  3c                   if             H1in42  = *off
061700070604     c                             or   H1in43  = *on
061800070518     c                   seton                                        285090
061900070518     c                   eval      V1Dmsg    = $Msg( 3)
062000070604x   3c                   else
062100070606     c                   exsr      OpzS01_68
062200070604e   3c                   endif
062300070412      * - Opz. 8=Sblocco
062400070517w   2c                   WHEN           S1Copz  = '8'
062500070518     c                             and  H1in42  = *off
062600070601     c                             and  H1in43  = *off
062700070910     c***                          and  %subst(KNMUS:1:3) = 'EDP'
062800070910     c                   exsr      OpzS01_68
062900070129x   2c                   OTHER
063000070403     c                   seton                                        285090
063100070129     c                   eval      V1Dmsg    = $Msg( 1)
063200070129e   2c                   ENDSL
063300070129      *
063400070129      * Reimpostazione indicatori per DSPATR
063500070508     c                   eval      *in41     = (H1in41 = *on)
063600070517     c                   eval      *in42     = (H1in42 = *on)
063700070601     c                   eval      *in43     = (H1in43 = *on)
063800070129      *
063900070129      * Aggiornamento sfl
064000070129sel 2c                   select
064100070129w   2c                   when      *in28
064200070129     c                   eval      *in32     = *on
064300070129w   2c                   when      *in90     = *on
064400070129     c                   clear                   S1Copz
064500070129x   2c                   other
064600070129     c                   clear                   S1Copz
064700070129e   2c                   endsl
064800070417     c                   UPDATE    SDUJS01
064900070129if  2c                   if        *in28  OR  *in90
065000070129     c                   leave
065100070129e   2c                   endif
065200070129      *
065300070417     c                   readc     SDUJS01
065400070129      *
065500070129e   1c                   ENDDO
065600070129      *
065700070129     c                   ENDSR
065800070416      *
065900070416      *---------------------------------------------------------------*
066000070416      *?Conferma del singolo "inserimento multiplo unità"            ?*
066100070416      *---------------------------------------------------------------*
066200070517     c     OpzS01_1      BEGSR
066300070424      *
066400070424      * RIaggancio del record trk. "1" per reperire tutti i dati
066500070508     c                   eval      WIMnum    = S1Cnum
066600070508     c                   eval      WIMtrk    = '1'
066700070508     c     K02WIM01      chain     WFAIM000
066800070416      *
066900070417      * Restituzione parametri selezionati
067000070508if  1c                   if        %found(WFAIM01L)
067100070508     c                   eval      DUJnum    = WIMnum
067200070508     c                   eval      DUJuni    = WIMuni
067300070424e   1c                   endif
067400070417      *
067500070417      * Chiusura del programma
067600070417     c                   eval      $Fine     = *on
067700070417     c                   eval      *in90     = *on
067800070416      *
067900070416     c                   ENDSR
068000070129      *
068100070129      *---------------------------------------------------------------*
068200070410      *?Gestione del singolo "inserimento multiplo unità"            ?*
068300070129      *---------------------------------------------------------------*
068400070517     c     OpzS01_2      BEGSR
068500070404      *
068600070412      * 1) "Blocco" del singolo inserimento multiplo unità      (trk 1)
068700070412      * 2) Manutenzione del singolo inserimento multiplo unità  (trk.2)
068800070412      * 3) "Sblocco" del singolo inserimento multiplo unità     (trk.1)
068900070417     c                   clear                   TNSDUJds
069000070417     c                   eval      DUJopz    = '7'
069100070417     c                   eval      DUJnum    = S1Cnum
069200070417     c                   exsr      Call_TNSDUJ2
069300070129      *
069400070129     c                   ENDSR
069500070404      *
069600070404      *---------------------------------------------------------------*
069700070410      *?Cancellazione del singolo "inserimento multiplo unità"       ?*
069800070404      *---------------------------------------------------------------*
069900070517     c     OpzS01_4      BEGSR
070000070404      *
070100070605      * 1) "Blocco" del singolo inserimento multiplo unità      (trk 1)
070200070605     c                   clear                   TNSDUJds
070300070605     c                   eval      DUJopz    = '7'
070400070605     c                   eval      DUJnum    = S1Cnum
070500070605     c                   exsr      Call_TNSDUJ1
070600070605if  1c                   if             DUJerr <> *off
070700070605     c                             or   DUJret <> *off
070800070605     c                   leavesr
070900070605e   1c                   endif
071000070605      *
071100070605      * 2) Cancellazione del singolo inserimento multiplo unità
071200070417     c                   clear                   TNSDUJds
071300070606     c***                eval      DUJopz    = '4'
071400070606     c                   eval      DUJopz    = S1Copz
071500070417     c                   eval      DUJnum    = S1Cnum
071600070417     c                   exsr      Call_TNSDUJ1
071700070605if  1c                   if             DUJerr <> *off
071800070605     c                             or   DUJret <> *off
071900070605     c                   clear                   TNSDUJds
072000070605     c                   eval      DUJopz    = '8'
072100070605     c                   eval      DUJnum    = S1Cnum
072200070605     c                   exsr      Call_TNSDUJ1
072300070605e   1c                   endif
072400070404      *
072500070404     c                   ENDSR
072600070518      *
072700070518      *---------------------------------------------------------------*
072800070518      *?Visualizzazione del singolo "inserimento multiplo unità"     ?*
072900070518      *---------------------------------------------------------------*
073000070606     c     OpzS01_5      BEGSR
073100070518      *
073200070518     c                   clear                   TNSDUJds
073300070606     c***                eval      DUJopz    = '5'
073400070606     c                   eval      DUJopz    = S1Copz
073500070518     c                   eval      DUJnum    = S1Cnum
073600070518     c                   exsr      Call_TNSDUJ2
073700070518      *
073800070518     c                   ENDSR
073900070517      *
074000070517      *---------------------------------------------------------------*
074100070517      *?RI-attivazione del singolo "inserimento multiplo unità"      ?*
074200070910      *?"Sblocco" del singolo "inserimento multiplo unità"           ?*
074300070517      *---------------------------------------------------------------*
074400070606     c     OpzS01_68     BEGSR
074500070517      *
074600070517     c                   clear                   TNSDUJds
074700070910sel 1c                   select
074800070606      * RI-attivazione del singolo "inserimento multiplo unità"
074900070910w   1c                   when      S1Copz    = '6'
075000070910     c                   eval      DUJopz    = '6'
075100070606      * "Sblocco" del singolo "inserimento multiplo unità" x usr EDP*
075200070910w   1c                   when      S1Copz    = '8'
075300070606     c***                eval      DUJopz    = '8'
075400070910     c                   eval      DUJopz    = 'S'
075500070910e   1c                   endsl
075600070910     c***                eval      DUJopz    = S1Copz
075700070517     c                   eval      DUJnum    = S1Cnum
075800070517     c                   exsr      Call_TNSDUJ1
075900070517      *
076000070517     c                   ENDSR
076100070411      *
076200070411      *---------------------------------------------------------------*
076300070508      *?Richiamo del pgm. TNSDUJR1 (gestione trk. "1" file WFAIM00F) ?*
076400070411      *---------------------------------------------------------------*
076500070417     c     Call_TNSDUJ1  BEGSR
076600070411      *
076700070417     c                   movel(p)  TNSDUJds      KPJBU
076800070417     c                   call      'TNSDUJR1'
076900070411     c                   parm                    KPJBA
077000070417     c                   movel     KPJBU         TNSDUJds
077100070411      *
077200070411sel 1c                   SELECT
077300070411      * Ritorno normale
077400070417w   1c                   WHEN           DUJret  = *off
077500070417     c                             and  DUJerr  = *off
077600070411     c                   eval      $InzS01   = *on
077700070521if  2c                   if        S1Copz    = '4'
077800070521     c                   eval      $RgzPfm   = *on
077900070521e   2c                   endif
078000070411      * Ritorno con errore
078100070417w   1c                   WHEN      DUJerr    = *on
078200070411     c                   seton                                        28  90
078300070417     c                   eval      V1Dmsg    = DUJmsg
078400070411      * Ritorno con F3
078500070417w   1c                   WHEN      DUJret    = '1'
078600070416     c***                eval      $Fine     = *on
078700070416     c                   eval      $InzS01   = *off
078800070416     c                   eval      *in90     = *on
078900070411      * Ritorno con F12
079000070417w   1c                   WHEN      DUJret    = '2'
079100070412      *
079200070411e   1c                   ENDSL
079300070411      *
079400070411     c                   ENDSR
079500070411      *
079600070411      *---------------------------------------------------------------*
079700070508      *?Richiamo del pgm. TNSDUJR2 (gestione trk. "2" file WFAIM00F) ?*
079800070508      *---------------------------------------------------------------*
079900070417     c     Call_TNSDUJ2  BEGSR
080000070412      *
080100070412      * Gestione rec. trk "1" per
080200070412      * Inserimento e/o "Blocco" dell'inserimento multiplo unità
080300070518      * (SE NON VISUALIZZAZIONE)
080400070518if  1c                   IF        DUJopz   <> '5'
080500070417     c                   exsr      Call_TNSDUJ1
080600070518if  2c                   if             DUJerr <> *off
080700070417     c                             or   DUJret <> *off
080800070412     c                   leavesr
080900070518e   2c                   endif
081000070518e   1c                   ENDIF
081100070411      *
081200070412      * Gestione rec. trk "2" per
081300070412      * Manutenzione dell'inserimento multiplo unità
081400070417     c                   movel(p)  TNSDUJds      KPJBU
081500070417     c                   call      'TNSDUJR2'
081600070411     c                   parm                    KPJBA
081700070417     c                   movel     KPJBU         TNSDUJds
081800070411      *
081900070411sel 1c                   SELECT
082000070412      * - Ritorno normale
082100070417w   1c                   WHEN           DUJret  = *off
082200070417     c                             and  DUJerr  = *off
082300070518     c                             and  DUJopz <> '5'
082400070411     c                   eval      $InzS01   = *on
082500070521if  2c                   if             S1Copz  = '2'
082600070521     c                             or   S1Copz  = '6'
082700070521     c                   eval      $RgzPfm   = *on
082800070521e   2c                   endif
082900070412      * - Ritorno con errore
083000070417w   1c                   WHEN      DUJerr    = *on
083100070411     c                   seton                                        28  90
083200070417     c                   eval      V1Dmsg    = DUJmsg
083300070412      * - Ritorno con F3
083400070417w   1c                   WHEN      DUJret    = '1'
083500070416     c***                eval      $Fine     = *on
083600070416     c                   eval      *in90     = *on
083700070412      * - Ritorno con F12
083800070417w   1c                   WHEN      DUJret    = '2'
083900070412      *
084000070411     c                   ENDSL
084100070518      *
084200070518      * Se richiesta solo visualizzazione (opzione 5): esce
084300070518if  1c                   if        DUJopz    = '5'
084400070518     c                   leavesr
084500070518e   1c                   endif
084600070412      *
084700070412      * ...Anche in caso di errore (che mi sembra comunque IMPOSSIBILE)
084800070412      *   e soprattutto in caso di F3/F12, è da sbloccare l'inserimento
084900070518      *   multiplo unità al trk "1" !!!
085000070412      *
085100070412      * Gestione rec. trk "1" per
085200070412      * "Sblocco" dell'inserimento multiplo unità
085300070417     c                   eval      DUJopz    = '8'
085400070417     c                   exsr      Call_TNSDUJ1
085500070416      *
085600070416if  1c                   if        *in28  or  *in90
085700070416     c                   eval      $InzS01   = *off
085800070416e   1c                   endif
085900070411      *
086000070411     c                   ENDSR
086100070601      *
086200070601      *---------------------------------------------------------------*
086300070605      *?Richiamo del pgm. TNSDUJR3 (gest. file PC .CSV in WFAIM20F)  ?*
086400070601      *---------------------------------------------------------------*
086500070601     c     Call_TNSDUJ3  BEGSR
086600070601      *
086700070601     c                   clear                   TNSDUJds
086800070601     c                   eval      DUJopz    = S1Copz
086900070601      *
087000070601     c                   movel(p)  TNSDUJds      KPJBU
087100070601     c                   call      'TNSDUJR3'
087200070601     c                   parm                    KPJBA
087300070601     c                   movel     KPJBU         TNSDUJds
087400070601      *
087500070601sel 1c                   SELECT
087600070601      * Ritorno normale
087700070601w   1c                   WHEN           DUJret  = *off
087800070601     c                             and  DUJerr  = *off
087900070601     c                   eval      $InzS01   = *on
088000070601      * Ritorno con errore
088100070601w   1c                   WHEN      DUJerr    = *on
088200070601     c                   seton                                        28  90
088300070601     c                   eval      V1Dmsg    = DUJmsg
088400070601      * Ritorno con F3
088500070601w   1c                   WHEN      DUJret    = '1'
088600070604     c                   eval      $RgzPfm   = *on
088700070604     c                   eval      $Fine     = *on
088800070604     c                   eval      *in90     = *on
088900070601      * Ritorno con F12
089000070601w   1c                   WHEN      DUJret    = '2'
089100070604     c                   eval      $RgzPfm   = *on
089200070601      *
089300070601e   1c                   ENDSL
089400070601      *
089500070601     c                   ENDSR
089600070129
089700070201** - $MSG -------------------------------------------------------------------*
089800070129Opzione errata                                                                  1
089900070518Opzione errata per un "Inserimento Multiplo Unità" già confermato               2
090000070518Opzione errata per un "Inserimento Multiplo Unità" da confermare                3
