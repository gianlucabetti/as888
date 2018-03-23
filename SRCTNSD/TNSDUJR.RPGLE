000100070411      *PARMS DBGVIEW(*SOURCE)
000200130919     /*PRM  dbgview(*source)
000300130919     /*END
000400070129      *===============================================================*
000500070417      *?TNSDUJR - Inserimento multiplo unità EDP                     ?*
000600070129      *===============================================================*
000700070129
000800070129     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000900070129
001000070129      *---------------------------------------------------------------*
001100070129
001200070605     fWFAIM01L  if   e           k disk    usropn
001300070604     fWFAIM20F  if   e           k disk    usropn
001400070129      *
001500070417     fTNSDUJD   cf   e             workstn
001600070417     f                                     sfile(SDUJS01:S01nrr)
001700070129     f                                     infds(DSFMT)
001800070129
001900070129      *---------------------------------------------------------------*
002000070129
002100070403      *
002200070410      *?Costanti  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
002300070129      *
002400070129      * - Tasti di funzione
002500070405     d C_RollUp        c                   const(x'F5')
002600070129      * - Nr. di righe del sfl (SFLPAG)
002700070411     d C_SflPag        c                   const(16)
002800070129      *
002900070410      *?Schiere - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
003000070129      *
003100070129      * - Messaggi di errore
003200070604     d $Msg            s             78    dim( 3)   ctdata  perrcd(1)
003300070129      *
003400070410      *?Ds  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
003500070129      *
003600070129     d KPJBA         e ds
003700070129      * - Parametri x Controllo profilo utenti
003800070416     d TIBS34ds      e ds                  inz
003900070129      * - Ds di riferimento al file esterno AZUTE00F
004000070129     d AZUTEds       e ds                  extname(AZUTE00F)
004100070129      * - Ds per dati organigramma
004200070416     d dDatiUte      e ds
004300070129      *
004400130923      * DS campo WIMUNI file WFAIM00F trk "1" pr 01
004500070508     d dWIMuni1      e ds                  inz
004600130923      * DS campo WIMUNI file WFAIM00F trk "1" prg 02
004700130923     d dWIMuni12     e ds                  inz
004800070411      *
004900070417      * Parametri per pgm. TNSDUJR1/R2
005000070417     d TNSDUJds      e ds                  inz
005100070129      *
005200070129     d Status         sds           333
005300070129     d   SDSpgm          *proc
005400070129      *
005500070129     d dsFMT           ds           512
005600070129     d  £Tasto               369    369
005700070129      *
005800070410      *?Variabili - - - - - - - - - - - - - - - - - - - - - - - - - -?*
005900070129      *
006000070129      * - Flags
006100070411     d $EoF            s              1    inz(*off)
006200070601     d $20F            s              1    inz(*off)
006300070129     d $Fine           s              1    inz(*off)
006400070521     d $RgzPfm         s              1    inz(*off)
006500070129     d $InzS01         s              1    inz(*on)
006600070129      *
006700070129      * - Indici di schiera
006800070129      *
006900070129      * - Variabili riferite al data base o al display file
007000070129     d S01nrr          s                   like(C01rcd) inz
007100070601     d wRec20F         s                   like(S1Cnmi) inz
007200070201      *
007300070129      * - Variabili definite a programma
007400070129     d wPag            s              4  0 inz
007500070129     d wRig            s              3  0 inz
007600070411     d W_SflPag        s              3  0 inz
007700070604      *
007800070604      * - Parametri per pgm. TNSDUJC = Riorganizzazione WF
007900070604     d DUJfclr         s              1    inz('0')
008000070129      *
008100070410      *?Key-List  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
008200070129      *
008300070508     c     K02WIM01      klist
008400070508     c                   kfld                    WIMnum
008500070508     c                   kfld                    WIMtrk
008600130919     c     K03WIM01      klist
008700130919     c                   kfld                    WIMnum
008800130919     c                   kfld                    WIMtrk
008900130919     c                   kfld                    WIMprg
009000070129
009100070129      *---------------------------------------------------------------*
009200070410      *?RIEPILOGO INDICATORI                                         ?*
009300070129      *---------------------------------------------------------------*
009400070416      * 06    - Abilitazione F6=Conferma in C01/S01                   *
009500070503      * 12    - Abilitazione F12=Ritorno in C01/S01                   *
009600070410      * 25    - Comodo                                                *
009700070416      * 28    - Emissione del messaggio di errore a video             *
009800070416      * 30    - Pulizia      del subfile S01 (sflclr, NO sfldsp)      *
009900070416      * 31    - NO emissione del subfile S01 (sflclr, NO sfldspctl)   *
010000070129      * 32    - Record modificato nel subfile S01 (sflnxtchg)         *
010100070416      * 33    - Fine dati         nel subfile S01 (sflend)            *
010200070601      * 41    - S01: Evidenziazione sflrec con DSPATR(RI)             *
010300070601      * 42    - S01: Evidenziazione sflrec con DSPATR(HI BL)          *
010400070601      * 43    - S01: Evidenziazione sflrec con COLOR(PNK)             *
010500070411      * 50    - S01: Posizionamento cursore sul campo "Opzione"       *
010600070129      * 90    - Generico di errore                                    *
010700070129      *---------------------------------------------------------------*
010800070129
010900070129     c     *Entry        plist
011000070129     c                   parm                    KPJBA
011100070129      *
011200070129      * Operazioni Iniziali
011300070129     c                   exsr      RoutInz
011400070129      *
011500070129      * Gestione Video
011600070411      * - SubFile x visualizzazione   trk "1" già inseriti (testata)
011700070129do  1c                   DOW       $Fine     = *off
011800070518     c                   exsr      GesS01
011900070129e   1c                   ENDDO
012000070518      *
012100070521      * "Riorganizzazione" work-file (se "modificato")
012200070521if  1c                   if        $RgzPfm   = *on
012300070518     c                   close     WFAIM01L
012400070523     c                   call      'TNSDUJC'
012500070604     c                   parm      '0'           DUJfclr
012600070518e   1c                   endif
012700070129      *
012800070129      * Fine
012900070417     c                   movel(p)  TNSDUJds      KPJBU
013000070129     c                   movel     *on           *inLR
013100070129      *
013200070129      *---------------------------------------------------------------*
013300070410      *?Operazioni Iniziali                                          ?*
013400070129      *---------------------------------------------------------------*
013500070129     c     RoutInz       BEGSR
013600070129      *
013700070130      * Reperimento dati job
013800070129     c                   exsr      DatiJob
013900070129      *
014000070129     c                   movel     SDSpgm        V1Tpgm
014100070416      *
014200070416      * Ricezione parametri
014300070417     c                   clear                   TNSDUJds
014400070417if  1c                   if        KPJBU    <> *blanks
014500070417     c                   movel     KPJBU         TNSDUJds
014600070417if  2c                   if        DUJopz    = '1'
014700070417     c                   clear                   DUJnum
014800070417e   2c                   endif
014900070417e   1c                   endif
015000130916     c                   eval      DUJuni1   = *blanks
015100130916     c                   eval      DUJuni2   = *blanks
015200070417     c                   eval      DUJret    = *off
015300070417     c                   eval      DUJerr    = *off
015400070417     c                   eval      DUJmsg    = *blanks
015500070417      *
015600070417     c                   eval      *in06     = (DUJopz <> '1')
015700070503     c                   eval      *in12     = (DUJopz  = '1')
015800070605      *
015900070605     c                   open      WFAIM01L
016000070129      *
016100070129     c                   ENDSR
016200070129      *
016300070129      *---------------------------------------------------------------*
016400070410      *?Reperimento Dati del job (Utente/Operativi)                  ?*
016500070129      *---------------------------------------------------------------*
016600070129     c     DatiJob       BEGSR
016700070129      *
016800070129     c     *dtaara       define    §azute        azuteds
016900070129     c     *dtaara       define    §datiute      ddatiute
017000070129      *
017100070129     c                   in(E)     *dtaara
017200070129     c                   IF        %ERROR or RSUT = *blanks
017300070129     c                   clear                   Tibs34Ds
017400070129     c                   call      'TIBS34R'
017500070129     c                   parm                    Tibs34Ds
017600070129     c                   in        *dtaara
017700070129     c                   ENDIF
017800070129      *
017900070129     c                   ENDSR
018000070129      *
018100070129      *---------------------------------------------------------------*
018200070410      *?Gestione videata S01                                         ?*
018300070129      *---------------------------------------------------------------*
018400070129     c     GesS01        BEGSR
018500070129      *
018600070130      * Inizializzazione della videata
018700070129if  1c                   if        $InzS01   = *on
018800070129     c                   exsr      InzS01
018900070129     c                   eval      $InzS01   = *off
019000070129e   1c                   endif
019100070129      *
019200070130      * Scrittura di testata e riga tasti funzionali abilitati
019300070417     c                   write     SDUJT01
019400070417     c                   write     SDUJZ01
019500070129      *
019600070130      * Visualizzazione (eventuale) del msg per la mancanza di dati
019700070403if  1c                   if        C01rcd   <= *zeros
019800070417     c                   write     SDUJD00
019900070129e   1c                   endif
020000070129      *
020100070130      * Emissione della videata
020200070417     c                   exfmt     SDUJC01
020300070405if  1c                   if        C01csr    > *zeros
020400070405     c                   eval      C01rcd    = C01csr
020500070405e   1c                   endif
020600070129     c                   setoff                                       28  90
020700070129     c                   clear                   V1Dmsg
020800070129      *
020900070129sel 1c                   SELECT
021000070129      * F3=Fine
021100070129w   1c                   WHEN      *inKC
021200070129     c                   exsr      F03S01
021300070129      * F5=Rivisualizzazione
021400070129w   1c                   WHEN      *inKE
021500070129     c                   exsr      F05S01
021600070129      * F6=Nuovo inserimento
021700070129w   1c                   WHEN      *inKF
021800070129     c                   exsr      F06S01
021900070503      * F12=Ritorno
022000070503w   1c                   WHEN      *inKL
022100070503     c                   exsr      F12S01
022200070607      * F13=Ricezione dati da PC
022300070607w   1c                   WHEN      *inKM
022400070607     c                   exsr      F13S01
022500070129      * Roll-UP
022600070405w   1c                   WHEN      £Tasto    = C_RollUp
022700070129     c                   exsr      RollUpS01
022800070129      * NO rec.
022900070410w   1c                   WHEN      S01nrr   <= *zeros
023000070129      *
023100070129      * Gestione opzioni inserite
023200070129x   1c                   OTHER
023300070129     c                   exsr      OpzS01
023400070129e   1c                   ENDSL
023500070129      *
023600070129     c                   ENDSR
023700070129      *
023800070129      *---------------------------------------------------------------*
023900070410      *?Inizializzazione videata S01                                 ?*
024000070129      *---------------------------------------------------------------*
024100070129     c     InzS01        BEGSR
024200070129      *
024300070130      * "Intestazione" del subfile-control
024400070416     c                   clear                   C1Dtxt
024500070130      *
024600070129      * Pulizia subfile
024700070129     c                   seton                                        3031
024800070417     c                   write     SDUJC01
024900070129     c                   setoff                                         3133
025000070129      *
025100070601     c                   reset                   $20F
025200070411     c                   reset                   $EoF
025300070411     c                   clear                   W_SflPag
025400070405     c***                clear                   C01csr
025500070129     c                   clear                   C01rcd
025600070129     c                   clear                   S01nrr
025700070129     c                   clear                   V1Dmsg
025800070129     c                   setoff                                       28  90
025900070129     c                   movea     *zeros        *in(50)
026000070601      *
026100070605      * Verifica esistenza dati da file PC (.csv)
026200070601      *   (se NON fase di selezione per conferma)
026300070605      *   (& se NON file già allocato)
026400070601if  1c                   if        *in06
026500070605     c                   open(e)   WFAIM20F
026600070605     c                   if        NOT %error
026700070601     c                   exsr      CheckWFAIM20F
026800070604     c                   close     WFAIM20F
026900070605     c                   endif
027000070601e   1c                   endif
027100070129      *
027200070129      * Caricamento 1ª pagina dei dati da presentare nel sfl
027300070508     c     *loval        setll     WFAIM000
027400070508     c                   read      WFAIM000
027500070508     c                   eval      $EoF      = (%eof(WFAIM01L))
027600070601sel 1c                   select
027700070601w   1c                   when      $EoF      = *off
027800070416     c                   eval      C1Dtxt    = 'Rilevati precedenti in+
027900070411     c                                         serimenti non ancora co+
028000070411     c                                         nfermati:'
028100070129     c                   exsr      RollUpS01
028200070601w   1c                   when      $20F      = *on
028300070605     c                   exsr      CarS01csv
028400070601     c                   eval      *in30     = (S01nrr = *zeros)
028500070601     c                   eval      *in33     = *on
028600070601     c                   eval      C01rcd    = S01nrr
028700070601e   1c                   endsl
028800070129      *
028900070129     c                   ENDSR
029000070601      *
029100070601      *---------------------------------------------------------------*
029200070601      *?Caricamento pagina successiva di dati S01                    ?*
029300070601      *---------------------------------------------------------------*
029400070601     c     CheckWFAIM20F BEGSR
029500070601      *
029600070601      * Verifica esistenza dati
029700070601     c                   read      WFAIM200
029800070601if  1c                   if        %eof(WFAIM20F)
029900070601     c                   leavesr
030000070601e   1c                   endif
030100070601      *
030200070601      * Conteggio barcode da inserire
030300070601     c                   clear                   wRec20F
030400070601do  1c                   dow       NOT  %eof(WFAIM20F)
030500070601if  2c                   if        WIMbcd   <> *blanks
030600070601     c                   add       1             wRec20F
030700070601e   2c                   endif
030800070601     c                   read      WFAIM200
030900070601e   1c                   enddo
031000070601      *
031100070605      * Impostazione segnalazione di esistenza dati di file PC (.csv)
031200070601      *   da confermare
031300070601     c                   eval      $20F      = (wRec20F > *zeros)
031400070601      *
031500070601      * Impostazione testata
031600070601if  1c                   if            $20F    = *on
031700070601     c                             and C1Dtxt  = *blanks
031800070605     c                   eval      C1Dtxt    = 'Rilevati barcode da fi+
031900070605     c                                         le PC (.csv):'
032000070605     c***                eval      C1Dtxt    = 'Rilevato file PC (.csv+
032100070605     c***                                      ) di BARCODE non ancora+
032200070605     c***                                       confermati:'
032300070601e   1c                   endif
032400070601      *
032500070601     c                   ENDSR
032600070129      *
032700070129      *---------------------------------------------------------------*
032800070410      *?Caricamento pagina successiva di dati S01                    ?*
032900070129      *---------------------------------------------------------------*
033000070129     c     RollUpS01     BEGSR
033100070129      *
033200070411     c                   eval      S01nrr    = (W_SflPag * C_SflPag)
033300070411     c                   add       1             W_SflPag
033400070129     c                   eval      *in32     = *off
033500070601      *
033600070605      * Seganazione di barcode nel file WFAIM20F da file PC (.csv)
033700070601if  1c                   if             $20F   = *on
033800070601     c                             and  S01nrr = *zeros
033900070605     c                   exsr      CarS01csv
034000070601e   1c                   endif
034100070129      *
034200070129      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
034300070411do  1c                   doW            $EoF   = *off
034400070411     c                             and  S01nrr < W_SflPag * C_SflPag
034500070129     c                   exsr      CarS01
034600070517     c     WIMnum        setgt     WFAIM000
034700070508     c                   read      WFAIM000
034800070508     c                   eval      $EoF      = (%eof(WFAIM01L))
034900070129e   1c                   enddo
035000070129      *
035100070129      * Visualizzazione del SFL (se ci sono dati)
035200070129     c                   eval      *in30     = (S01nrr = *zeros)
035300070129      *
035400070129      * Attivazione (eventuale) del SFLEND
035500070601     c                   eval      *in33     = $EoF
035600070129      *
035700070129      * Posizionamento cursore al 1° rcd della pagina
035800070129if  1c                   if        S01nrr    > *zeros
035900070411     c     S01nrr        div       C_SflPag      wPag
036000070129     c                   mvr                     wRig
036100070411     c                   eval      C01rcd    = wPag * C_SflPag
036200070129     c                   if        wRig      > *zeros
036300070129     c                   eval      C01rcd    = C01rcd + 1
036400070129     c                   else
036500070411     c                   eval      C01rcd    = C01rcd - C_SflPag + 1
036600070129     c                   endif
036700070129x   1c                   else
036800070129     c                   clear                   C01rcd
036900070129e   1c                   endif
037000070129      *
037100070129     c                   ENDSR
037200070601      *
037300070601      *---------------------------------------------------------------*
037400070601      *?Caricamento dati nel subfile S01 da barcode del file WFAIM20F?*
037500070601      *---------------------------------------------------------------*
037600070605     c     CarS01csv     BEGSR
037700070601      *
037800070601      * Caricamento dati nel record del sfl
037900070601     c                   clear                   SDUJS01
038000070601     c                   eval      H1in41    = *off
038100070601     c                   eval      H1in42    = *off
038200070601     c                   eval      H1in43    = *on
038300070601     c***                eval      S1Cnum    = *hival
038400070601     c***                eval      S1Cdes    = *blanks
038500070601     c***                eval      S1Cnmi    = *zeros
038600070601     c***                eval      S1Cute    = *blanks
038700070601sel 1c                   select
038800070601w   1c                   when      H1in42    = *on
038900130923     c                   eval      S1Cnot    = 'Confermato  '
039000070601w   1c                   when      H1in41    = *on
039100130923     c                   eval      S1Cnot    = 'In uso...   '
039200070601w   1c                   when      H1in43    = *on
039300130923     c                   eval      S1Cnot    = 'file PC .csv'
039400070601e   1c                   endsl
039500070605     c                   eval      S1Dtxt    = 'Dati derivanti da docu+
039600070606     c                                         mento su PC (.CSV)  -  +
039700070606     c                                         da elaborare con opz. 2'
039800070601      * - Dati di "totale"
039900070601     c                   eval      S1Cnma    = wRec20F
040000070601      *
040100070601      * Impostazione indicatori
040200070601     c                   eval      *in41     = (H1in41 = *on)
040300070601     c                   eval      *in42     = (H1in42 = *on)
040400070601     c                   eval      *in43     = (H1in43 = *on)
040500070601      *
040600070601      * Scrittura record subfile
040700070601     c                   add       1             S01nrr
040800070601     c                   write     SDUJS01
040900070601      *
041000070601     c                   ENDSR
041100070129      *
041200070129      *---------------------------------------------------------------*
041300070410      *?Caricamento dati in subfile S01                              ?*
041400070129      *---------------------------------------------------------------*
041500070129     c     CarS01        BEGSR
041600070129      *
041700130923     c                   clear                   dWIMuni1
041800130923     c                   clear                   dWIMuni12
041900070517sel 1c                   select
042000070517w   1c                   when           WIMtrk = '1'
042100070517     c                             OR  (WIMtrk = '3'
042200070517     c                             and  *in06  = *on)
042300130923     c                   if        WIMprg < 2
042400070508     c                   movel     WIMuni        dWIMuni1
042500130923     c                   else
042600130923     c                   eval      §WIM1des  = *all'? '
042700130923     c                   eval      §WIM1usp  = *all'? '
042800130923     c                   movel     WIMuni        dWIMuni12
042900130923     c                   endif
043000070517w   1c                   when           WIMtrk = '2'
043100070517     c                             OR  (WIMtrk = '4'
043200070517     c                             and  *in06  = *on)
043300070508     c                   eval      §WIM1des  = *all'? '
043400070508     c                   eval      §WIM1usp  = *all'? '
043500070517x   1c                   other
043600070517     c                   leavesr
043700070518e   1c                   endsl
043800070129      *
043900070129      * Caricamento dati nel record del sfl
044000070417     c                   clear                   SDUJS01
044100130923     c                   eval      H1in41    = (WIMtrk = '1'  and
044200130923     c                                          WIMprg = *zeros)
044300070517     c                   eval      H1in42    = (WIMtrk > '2')
044400070601     c                   eval      H1in43    = *off
044500130923if  1c                   if            (WIMtrk = '1'  OR
044600130923     c                                 (WIMtrk = '3'  and  *in06  = *on))
044700130923     c                             and WIMprg < 2
044800130923     c     K02WIM01      reade     WFAIM000
044900130923if  2c                   if        NOT %eof(WFAIM01L) and  WIMprg = 2
045000130923     c                   movel     WIMuni        dWIMuni12
045100130923e   2c                   endif
045200130923e   1c                   endif
045300070508     c                   eval      S1Cnum    = WIMnum
045400130923     c                   eval      S1Cdes    = §WIM1des + §WIM12des
045500070508     c                   eval      S1Cnmi    = §WIM1nmi
045600070508if  1c                   if        §WIM1uin <> *blanks
045700070508     c                   eval      S1Cute    = §WIM1uin
045800070508x   1c                   else
045900070508     c                   eval      S1Cute    = §WIM1usp
046000070508e   1c                   endif
046100070517sel 1c                   select
046200070517w   1c                   when      H1in42    = *on
046300130923     c                   eval      S1Cnot    = 'Confermato  '
046400070518w   1c                   when      H1in41    = *on
046500130923     c                   eval      S1Cnot    = 'In uso...   '
046600070601w   1c                   when      H1in43    = *on
046700130923     c                   eval      S1Cnot    = 'file PC .csv'
046800070517e   1c                   endsl
046900070606     c                   eval      S1Dtxt    = 'Lunghezza: '
047000070605     c                                       + %editc(§WIM1lm1:'Z')
047100070606     c                                       + '  dalla posizione '
047200070605     c                                       + %editc(§WIM1im2:'Z')
047300070606     c                                       + '  per '
047400070605     c                                       + %editc(§WIM1lm2:'Z')
047500070605     c                                       + ' caratteri'
047600070517      * - Dati di "totale"
047700070517sel 1c                   select
047800070517w   1c                   when      WIMtrk    = '1'
047900070517     c                   eval      WIMtrk    = '2'
048000070517     c     K02WIM01      setll     WFAIM000
048100070517     c     K02WIM01      reade     WFAIM000
048200070517w   1c                   when      WIMtrk    = '3'
048300070517     c                   eval      WIMtrk    = '4'
048400070517     c     K02WIM01      setll     WFAIM000
048500070517     c     K02WIM01      reade     WFAIM000
048600070517e   1c                   endsl
048700070517do  1c                   dow       NOT  %eof(WFAIM01L)
048800070517     c                   add       1             S1Cnma
048900070517     c     K02WIM01      reade     WFAIM000
049000070517e   1c                   enddo
049100070129      *
049200070129      * Impostazione indicatori
049300070508     c                   eval      *in41     = (H1in41 = *on)
049400070517     c                   eval      *in42     = (H1in42 = *on)
049500070601     c                   eval      *in43     = (H1in43 = *on)
049600070129      *
049700070129      * Scrittura record subfile
049800070129     c                   add       1             S01nrr
049900070417     c                   write     SDUJS01
050000070129      *
050100070129     c                   ENDSR
050200070129      *
050300070129      *---------------------------------------------------------------*
050400070410      *?Gestione tasto funzionale F03 da videata S01                 ?*
050500070129      *---------------------------------------------------------------*
050600070129     c     F03S01        BEGSR
050700070129      *
050800070130      * Chiusura del programma
050900070417     c                   eval      DUJret    = '1'
051000070130     c                   eval      $Fine     = *on
051100070129      *
051200070129     c                   ENDSR
051300070129      *
051400070129      *---------------------------------------------------------------*
051500070410      *?Gestione tasto funzionale F05 da videata S01                 ?*
051600070129      *---------------------------------------------------------------*
051700070129     c     F05S01        BEGSR
051800070129      *
051900070130      * Ricarica del subfile
052000070130     c                   eval      $InzS01   = *on
052100070129      *
052200070129     c                   ENDSR
052300070129      *
052400070129      *---------------------------------------------------------------*
052500070410      *?Gestione tasto funzionale F06 da videata S01                 ?*
052600070129      *---------------------------------------------------------------*
052700070129     c     F06S01        BEGSR
052800070129      *
052900070129      * Nuovo inserimento multiplo unità
053000070412      *
053100070412      * 1) Creazione + "Blocco" dell'inserimento multiplo unità (trk 1)
053200070412      * 2) Manutenzione dell'inserimento multiplo unità         (trk.2)
053300070412      * 3) "Sblocco" dell'inserimento multiplo unità            (trk.1)
053400070417     c                   clear                   TNSDUJds
053500070417     c                   eval      DUJopz    = '1'
053600070417     c                   exsr      Call_TNSDUJ2
053700070417     c                   if             DUJerr <> *off
053800070417     c                             or   DUJret <> *off
053900070412     c                   leavesr
054000070412     c                   endif
054100070129      *
054200070129     c                   ENDSR
054300070503      *
054400070503      *---------------------------------------------------------------*
054500070503      *?Gestione tasto funzionale F12 da videata S01                 ?*
054600070503      *---------------------------------------------------------------*
054700070503     c     F12S01        BEGSR
054800070503      *
054900070503      * Chiusura del programma
055000070503     c                   eval      DUJret    = '2'
055100070503     c                   eval      $Fine     = *on
055200070503      *
055300070503     c                   ENDSR
055400070607      *
055500070607      *---------------------------------------------------------------*
055600070607      *?Gestione tasto funzionale F13 da videata S01                 ?*
055700070607      *---------------------------------------------------------------*
055800070607     c     F13S01        BEGSR
055900070607      *
056000070607     c                   close     WFAIM01L
056100070607      *
056200070607     c                   eval      DUJopz    = '1'
056300070607      *
056400070607     c                   movel     TNSDUJds      KPJBU
056500070607     c                   call      'TNSDUJR3'
056600070607     c                   parm                    KPJBA
056700070607     c                   movel     KPJBU         TNSDUJds
056800070607      *
056900070607sel 1c                   select
057000070607w   1c                   when      DUJerr   <> *off
057100070607     c                   seton                                        28  90
057200070607     c                   eval      V1Dmsg    = DUJmsg
057300070607w   1c                   when      DUJret    = *zeros
057400070607     c                   eval      $InzS01   = *on
057500070607e   1c                   endsl
057600070607      *
057700070607     c                   open      WFAIM01L
057800070607      *
057900070607     c                   ENDSR
058000070129      *
058100070129      *---------------------------------------------------------------*
058200070410      *?Controllo opzioni subfile S01                                ?*
058300070129      *---------------------------------------------------------------*
058400070129     c     OpzS01        BEGSR
058500070129      *
058600070417     c                   readc     SDUJS01
058700070129      *
058800070417do  1c                   DOW       NOT %eof(TNSDUJD)
058900070129      *
059000070129     c                   eval      *in32     = *off
059100070129     c                   movea     *zeros        *in(50)
059200070129     c                   z-add     S01nrr        C01rcd
059300070129      *
059400070129sel 2c                   SELECT
059500070129      * - Nessuna opzione
059600070129w   2c                   WHEN      S1Copz    = *blanks
059700070416      * - Opz. 1=Conferma
059800070416w   2c                   WHEN      S1Copz    = '1'
059900070416     c                             and *in06 = *off
060000070518     c                   if        H1in42    = *off
060100070517     c                   exsr      OpzS01_1
060200070518     c                   else
060300070518     c                   seton                                        285090
060400070518     c                   eval      V1Dmsg    = $Msg( 2)
060500070518     c                   endif
060600070405      * - Opz. 2=Gestione
060700070405w   2c                   WHEN      S1Copz    = '2'
060800070416     c                             and *in06 = *on
060900070604sel 3c                   select
061000070605      *   - da file PC (.csv)
061100070604w   3c                   when      H1in43    = *on
061200070604     c                   exsr      Call_TNSDUJ3
061300070604      *   - da confermare
061400070604w   3c                   when      H1in42    = *off
061500070517     c                   exsr      OpzS01_2
061600070604      *   - già confermato
061700070604x   3c                   other
061800070518     c                   seton                                        285090
061900070518     c                   eval      V1Dmsg    = $Msg( 2)
062000070604e   3c                   endsl
062100070404      * - Opz. 4=Cancellazione
062200070404w   2c                   WHEN      S1Copz    = '4'
062300070416     c                             and *in06 = *on
062400070605      *   - da file PC (.csv)
062500070604if  3c                   if        H1in43    = *on
062600070604     c                   exsr      Call_TNSDUJ3
062700070604x   3c                   else
062800070517     c                   exsr      OpzS01_4
062900070604e   3c                   endif
063000070518      * - Opz. 5=Visualizzazione
063100070518w   2c                   WHEN      S1Copz    = '5'
063200070605      *   - da file PC (.csv)
063300070604if  3c                   if        H1in43    = *on
063400070604     c                   exsr      Call_TNSDUJ3
063500070604x   3c                   else
063600070518     c                   exsr      OpzS01_5
063700070604e   3c                   endif
063800070517      * - Opz. 6=RI-attivazione
063900070517w   2c                   WHEN      S1Copz    = '6'
064000070517     c                             and *in06 = *on
064100070604if  3c                   if             H1in42  = *off
064200070604     c                             or   H1in43  = *on
064300070518     c                   seton                                        285090
064400070518     c                   eval      V1Dmsg    = $Msg( 3)
064500070604x   3c                   else
064600070606     c                   exsr      OpzS01_68
064700070604e   3c                   endif
064800070412      * - Opz. 8=Sblocco
064900070517w   2c                   WHEN           S1Copz  = '8'
065000070518     c                             and  H1in42  = *off
065100070601     c                             and  H1in43  = *off
065200070910     c***                          and  %subst(KNMUS:1:3) = 'EDP'
065300070910     c                   exsr      OpzS01_68
065400070129x   2c                   OTHER
065500070403     c                   seton                                        285090
065600070129     c                   eval      V1Dmsg    = $Msg( 1)
065700070129e   2c                   ENDSL
065800070129      *
065900070129      * Reimpostazione indicatori per DSPATR
066000070508     c                   eval      *in41     = (H1in41 = *on)
066100070517     c                   eval      *in42     = (H1in42 = *on)
066200070601     c                   eval      *in43     = (H1in43 = *on)
066300070129      *
066400070129      * Aggiornamento sfl
066500070129sel 2c                   select
066600070129w   2c                   when      *in28
066700070129     c                   eval      *in32     = *on
066800070129w   2c                   when      *in90     = *on
066900070129     c                   clear                   S1Copz
067000070129x   2c                   other
067100070129     c                   clear                   S1Copz
067200070129e   2c                   endsl
067300070417     c                   UPDATE    SDUJS01
067400070129if  2c                   if        *in28  OR  *in90
067500070129     c                   leave
067600070129e   2c                   endif
067700070129      *
067800070417     c                   readc     SDUJS01
067900070129      *
068000070129e   1c                   ENDDO
068100070129      *
068200070129     c                   ENDSR
068300070416      *
068400070416      *---------------------------------------------------------------*
068500070416      *?Conferma del singolo "inserimento multiplo unità"            ?*
068600070416      *---------------------------------------------------------------*
068700070517     c     OpzS01_1      BEGSR
068800070424      *
068900130916      * RIaggancio del record trk. "1" prg 01 per reperire tutti i dati
069000070508     c                   eval      WIMnum    = S1Cnum
069100070508     c                   eval      WIMtrk    = '1'
069200070508     c     K02WIM01      chain     WFAIM000
069300070416      *
069400070417      * Restituzione parametri selezionati
069500070508if  1c                   if        %found(WFAIM01L)
069600070508     c                   eval      DUJnum    = WIMnum
069700130916     c                   eval      DUJuni1   = WIMuni
069800070424e   1c                   endif
069900130916      *
070000130916      * RIaggancio del record trk. "1" prg 02 per reperire tutti i dati
070100130916     c*// già fatto:     eval      WIMnum    = S1Cnum
070200130916     c*// già fatto:     eval      WIMtrk    = '1'
070300130916     c                   eval      WIMprg    = 2
070400130919     c     K03WIM01      chain     WFAIM000
070500130916      *
070600130916      * Restituzione parametri selezionati
070700130916if  1c                   if        %found(WFAIM01L)
070800130916     c                   eval      DUJuni2   = WIMuni
070900130916e   1c                   endif
071000070417      *
071100070417      * Chiusura del programma
071200070417     c                   eval      $Fine     = *on
071300070417     c                   eval      *in90     = *on
071400070416      *
071500070416     c                   ENDSR
071600070129      *
071700070129      *---------------------------------------------------------------*
071800070410      *?Gestione del singolo "inserimento multiplo unità"            ?*
071900070129      *---------------------------------------------------------------*
072000070517     c     OpzS01_2      BEGSR
072100070404      *
072200070412      * 1) "Blocco" del singolo inserimento multiplo unità      (trk 1)
072300070412      * 2) Manutenzione del singolo inserimento multiplo unità  (trk.2)
072400070412      * 3) "Sblocco" del singolo inserimento multiplo unità     (trk.1)
072500070417     c                   clear                   TNSDUJds
072600070417     c                   eval      DUJopz    = '7'
072700070417     c                   eval      DUJnum    = S1Cnum
072800070417     c                   exsr      Call_TNSDUJ2
072900070129      *
073000070129     c                   ENDSR
073100070404      *
073200070404      *---------------------------------------------------------------*
073300070410      *?Cancellazione del singolo "inserimento multiplo unità"       ?*
073400070404      *---------------------------------------------------------------*
073500070517     c     OpzS01_4      BEGSR
073600070404      *
073700070605      * 1) "Blocco" del singolo inserimento multiplo unità      (trk 1)
073800070605     c                   clear                   TNSDUJds
073900070605     c                   eval      DUJopz    = '7'
074000070605     c                   eval      DUJnum    = S1Cnum
074100070605     c                   exsr      Call_TNSDUJ1
074200070605if  1c                   if             DUJerr <> *off
074300070605     c                             or   DUJret <> *off
074400070605     c                   leavesr
074500070605e   1c                   endif
074600070605      *
074700070605      * 2) Cancellazione del singolo inserimento multiplo unità
074800070417     c                   clear                   TNSDUJds
074900070606     c***                eval      DUJopz    = '4'
075000070606     c                   eval      DUJopz    = S1Copz
075100070417     c                   eval      DUJnum    = S1Cnum
075200070417     c                   exsr      Call_TNSDUJ1
075300070605if  1c                   if             DUJerr <> *off
075400070605     c                             or   DUJret <> *off
075500070605     c                   clear                   TNSDUJds
075600070605     c                   eval      DUJopz    = '8'
075700070605     c                   eval      DUJnum    = S1Cnum
075800070605     c                   exsr      Call_TNSDUJ1
075900070605e   1c                   endif
076000070404      *
076100070404     c                   ENDSR
076200070518      *
076300070518      *---------------------------------------------------------------*
076400070518      *?Visualizzazione del singolo "inserimento multiplo unità"     ?*
076500070518      *---------------------------------------------------------------*
076600070606     c     OpzS01_5      BEGSR
076700070518      *
076800070518     c                   clear                   TNSDUJds
076900070606     c***                eval      DUJopz    = '5'
077000070606     c                   eval      DUJopz    = S1Copz
077100070518     c                   eval      DUJnum    = S1Cnum
077200070518     c                   exsr      Call_TNSDUJ2
077300070518      *
077400070518     c                   ENDSR
077500070517      *
077600070517      *---------------------------------------------------------------*
077700070517      *?RI-attivazione del singolo "inserimento multiplo unità"      ?*
077800070910      *?"Sblocco" del singolo "inserimento multiplo unità"           ?*
077900070517      *---------------------------------------------------------------*
078000070606     c     OpzS01_68     BEGSR
078100070517      *
078200070517     c                   clear                   TNSDUJds
078300070910sel 1c                   select
078400070606      * RI-attivazione del singolo "inserimento multiplo unità"
078500070910w   1c                   when      S1Copz    = '6'
078600070910     c                   eval      DUJopz    = '6'
078700070606      * "Sblocco" del singolo "inserimento multiplo unità" x usr EDP*
078800070910w   1c                   when      S1Copz    = '8'
078900070606     c***                eval      DUJopz    = '8'
079000070910     c                   eval      DUJopz    = 'S'
079100070910e   1c                   endsl
079200070910     c***                eval      DUJopz    = S1Copz
079300070517     c                   eval      DUJnum    = S1Cnum
079400070517     c                   exsr      Call_TNSDUJ1
079500070517      *
079600070517     c                   ENDSR
079700070411      *
079800070411      *---------------------------------------------------------------*
079900070508      *?Richiamo del pgm. TNSDUJR1 (gestione trk. "1" file WFAIM00F) ?*
080000070411      *---------------------------------------------------------------*
080100070417     c     Call_TNSDUJ1  BEGSR
080200070411      *
080300070417     c                   movel(p)  TNSDUJds      KPJBU
080400070417     c                   call      'TNSDUJR1'
080500070411     c                   parm                    KPJBA
080600070417     c                   movel     KPJBU         TNSDUJds
080700070411      *
080800070411sel 1c                   SELECT
080900070411      * Ritorno normale
081000070417w   1c                   WHEN           DUJret  = *off
081100070417     c                             and  DUJerr  = *off
081200070411     c                   eval      $InzS01   = *on
081300070521if  2c                   if        S1Copz    = '4'
081400070521     c                   eval      $RgzPfm   = *on
081500070521e   2c                   endif
081600070411      * Ritorno con errore
081700070417w   1c                   WHEN      DUJerr    = *on
081800070411     c                   seton                                        28  90
081900070417     c                   eval      V1Dmsg    = DUJmsg
082000070411      * Ritorno con F3
082100070417w   1c                   WHEN      DUJret    = '1'
082200070416     c***                eval      $Fine     = *on
082300070416     c                   eval      $InzS01   = *off
082400070416     c                   eval      *in90     = *on
082500070411      * Ritorno con F12
082600070417w   1c                   WHEN      DUJret    = '2'
082700070412      *
082800070411e   1c                   ENDSL
082900070411      *
083000070411     c                   ENDSR
083100070411      *
083200070411      *---------------------------------------------------------------*
083300070508      *?Richiamo del pgm. TNSDUJR2 (gestione trk. "2" file WFAIM00F) ?*
083400070508      *---------------------------------------------------------------*
083500070417     c     Call_TNSDUJ2  BEGSR
083600070412      *
083700070412      * Gestione rec. trk "1" per
083800070412      * Inserimento e/o "Blocco" dell'inserimento multiplo unità
083900070518      * (SE NON VISUALIZZAZIONE)
084000070518if  1c                   IF        DUJopz   <> '5'
084100070417     c                   exsr      Call_TNSDUJ1
084200070518if  2c                   if             DUJerr <> *off
084300070417     c                             or   DUJret <> *off
084400070412     c                   leavesr
084500070518e   2c                   endif
084600070518e   1c                   ENDIF
084700070411      *
084800070412      * Gestione rec. trk "2" per
084900070412      * Manutenzione dell'inserimento multiplo unità
085000070417     c                   movel(p)  TNSDUJds      KPJBU
085100070417     c                   call      'TNSDUJR2'
085200070411     c                   parm                    KPJBA
085300070417     c                   movel     KPJBU         TNSDUJds
085400070411      *
085500070411sel 1c                   SELECT
085600070412      * - Ritorno normale
085700070417w   1c                   WHEN           DUJret  = *off
085800070417     c                             and  DUJerr  = *off
085900070518     c                             and  DUJopz <> '5'
086000070411     c                   eval      $InzS01   = *on
086100070521if  2c                   if             S1Copz  = '2'
086200070521     c                             or   S1Copz  = '6'
086300070521     c                   eval      $RgzPfm   = *on
086400070521e   2c                   endif
086500070412      * - Ritorno con errore
086600070417w   1c                   WHEN      DUJerr    = *on
086700070411     c                   seton                                        28  90
086800070417     c                   eval      V1Dmsg    = DUJmsg
086900070412      * - Ritorno con F3
087000070417w   1c                   WHEN      DUJret    = '1'
087100070416     c***                eval      $Fine     = *on
087200070416     c                   eval      *in90     = *on
087300070412      * - Ritorno con F12
087400070417w   1c                   WHEN      DUJret    = '2'
087500070412      *
087600070411     c                   ENDSL
087700070518      *
087800070518      * Se richiesta solo visualizzazione (opzione 5): esce
087900070518if  1c                   if        DUJopz    = '5'
088000070518     c                   leavesr
088100070518e   1c                   endif
088200070412      *
088300070412      * ...Anche in caso di errore (che mi sembra comunque IMPOSSIBILE)
088400070412      *   e soprattutto in caso di F3/F12, è da sbloccare l'inserimento
088500070518      *   multiplo unità al trk "1" !!!
088600070412      *
088700070412      * Gestione rec. trk "1" per
088800070412      * "Sblocco" dell'inserimento multiplo unità
088900070417     c                   eval      DUJopz    = '8'
089000070417     c                   exsr      Call_TNSDUJ1
089100070416      *
089200070416if  1c                   if        *in28  or  *in90
089300070416     c                   eval      $InzS01   = *off
089400070416e   1c                   endif
089500070411      *
089600070411     c                   ENDSR
089700070601      *
089800070601      *---------------------------------------------------------------*
089900070605      *?Richiamo del pgm. TNSDUJR3 (gest. file PC .CSV in WFAIM20F)  ?*
090000070601      *---------------------------------------------------------------*
090100070601     c     Call_TNSDUJ3  BEGSR
090200070601      *
090300070601     c                   clear                   TNSDUJds
090400070601     c                   eval      DUJopz    = S1Copz
090500070601      *
090600070601     c                   movel(p)  TNSDUJds      KPJBU
090700070601     c                   call      'TNSDUJR3'
090800070601     c                   parm                    KPJBA
090900070601     c                   movel     KPJBU         TNSDUJds
091000070601      *
091100070601sel 1c                   SELECT
091200070601      * Ritorno normale
091300070601w   1c                   WHEN           DUJret  = *off
091400070601     c                             and  DUJerr  = *off
091500070601     c                   eval      $InzS01   = *on
091600070601      * Ritorno con errore
091700070601w   1c                   WHEN      DUJerr    = *on
091800070601     c                   seton                                        28  90
091900070601     c                   eval      V1Dmsg    = DUJmsg
092000070601      * Ritorno con F3
092100070601w   1c                   WHEN      DUJret    = '1'
092200070604     c                   eval      $RgzPfm   = *on
092300070604     c                   eval      $Fine     = *on
092400070604     c                   eval      *in90     = *on
092500070601      * Ritorno con F12
092600070601w   1c                   WHEN      DUJret    = '2'
092700070604     c                   eval      $RgzPfm   = *on
092800070601      *
092900070601e   1c                   ENDSL
093000070601      *
093100070601     c                   ENDSR
093200070129
093300070201** - $MSG -------------------------------------------------------------------*
093400070129Opzione errata                                                                  1
093500070518Opzione errata per un "Inserimento Multiplo Unità" già confermato               2
093600070518Opzione errata per un "Inserimento Multiplo Unità" da confermare                3
