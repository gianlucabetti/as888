000100060928      *---------------------------------------------------------------*
000200060928      * Passaggio dati VdL al tabellone                               *
000300060928      *---------------------------------------------------------------*
000400060928
000500060928     h decedit('0,') datedit(*dmy/)
000600060928
000700060928      *---------------------------------------------------------------*
000800060928
000900060928     fTNTBE01L  if   e           k disk
001000080422     fFNMSA12L  if   e           k disk
001100060928      *
001200080422     fFNMSS11L  Uf A e           k disk
001300060928
001400060928      *---------------------------------------------------------------*
001500060928
001600060928      *
001700060928      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
001800060928      *
001900060928      *
002000060928      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
002100060928      * - P.O. (da applicazioni per macchinone)
002200060928     d $MTP            s              1    dim(100)
002300060928      *
002400060928      * - Media colli/minuto da tab. MDT
002500060928     d $MDT            s              2  0 dim( 10) inz                         *media lampeggio x b
002600060929      *
002700060929      * - Schiere per "semplificare" il calcolo su campi simili...
002800060929      * - - Colli per entrata
002900060929     d $EXX            s                   like(MSSe01)     dim(20)  inz
003000060929      * - - Minuti lavorati per entrata
003100060929     d $MXX            s                   like(MSSm01)     dim(20)  inz
003200060929      * - - Colli per uscita
003300060929     d $UXX            s                   like(MSSu01)     dim(60)  inz
003400060929      * - - Date lettura per entrata
003500060929     d $DTL            s                   like(MSAdtl)     dim(20)  inz
003600060929      * - - Orari lettura per entrata
003700060929     d $ORL            s                   like(MSAorl)     dim(20)  inz
003800060929      * - - Colli ultimo minuto per entrata - CORRENTE
003900060929     d $MSApep         s                   like(MSApep001)  dim(20)  inz
004000060929      * - - Colli ultimo minuto per uscita  - CORRENTE
004100060929     d $MSApad         s                   like(MSApad001)  dim(60)  inz
004200060928      *
004300060928      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
004400060928      *
004500060928      * - Parametri per richiamo al pgm di controllo profilo utenti
004600060928     d TIBS34ds      e ds
004700060928      * - Ds di riferimento al file esterno AzUte00F
004800060928     d AZUTEds       e ds                  extname(AZUTE00F)
004900060928      * - Ds per dati organigramma
005000060928     d DDatiUte      e ds
005100060928      *
005200060928      * - Dati tabellone
005300060928     d dMDT          e ds                  inz
005400060928      *
005500060928      * - Parametri in INPUT
005600060928     d Param           ds                                                       *DS parametri
005700081201     d  PARfil                        3s 0                                      - filiale
005800081201     d  PARdata                       8s 0                                      - data
005900081201     d  PARnpg                        1s 0                                      - categoria FV
006000060928      *
006100060928      * - Controlla data
006200060928     d WLBdat          ds                  inz
006300060928     d  G08dat                        8  0 inz
006400060928     d  G08inv                        8  0 inz
006500060928     d  G08err                        1    inz('3')
006600060928     d  G08tgi                        5  0 inz
006700060928      * - Calcolo data
006800060928     d WGIdat          ds                  inz
006900060928     d  GIOdat                        8  0 inz
007000060928     d  GIOinv                        8  0 inz
007100060928     d  GIOtgi                        5  0 inz
007200060928      *
007300060928      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
007400060928      *
007500060928      * - Flags booleani
007600060928     d $First          s              1    inz(*on)                             *prima lettura
007700060928      *
007800060928      * - Indici di schiera
007900060928     d XX              s              3  0 inz
008000060928     d YY              s              3  0 inz
008100060928      *
008200060928      * - Campi chiave riferiti al data base
008300060928     d KSAfil          s                   like(MSAfil)  inz
008400060928     d KSAapl          s                   like(MSAapl)  inz
008500060928     d KSAdfv          s                   like(MSAdfv)  inz
008600060928     d KSAnpg          s                   like(MSAnpg)  inz
008700060928     d KSAdtl          s                   like(MSAdtl)  inz
008800060928     d KSAorl          s                   like(MSAorl)  inz
008900060928     d KSSapl          s                   like(MSSapl)  inz
009000060928     d KSSfil          s                   like(MSSfil)  inz
009100060928     d KSSdfv          s                   like(MSSdfv)  inz
009200060928     d KSSnpg          s                   like(MSSnpg)  inz
009300060928      *
009400060928      * - Altre avriabili riferite al data base
009500060928     d wTabApl         s                   like(MSAapl)  inz
009600060928     d wDFVpr          s                   like(MSAdfv)  inz                    *di lavoro
009700060928     d wDFVgg          s                   like(MSAdfv)  inz
009800060928     d DEPdtlin        s                   like(MSAdtl)  inz                    *dep data inizio
009900060928     d DEPorlin        s                   like(MSAorl)  inz                    *dep ora  inizio
010000060928     d DEPdtlfi        s                   like(MSAdtl)  inz                    *dep data fine
010100060928     d DEPorlfi        s                   like(MSAorl)  inz                    *dep ora  fine
010200060928     d DEPdfv          s                   like(MSSdfv)  inz
010300060928     d DEPnpg          s                   like(MSSnpg)  inz
010400060928     d SAVdfv          s                   like(MSAdfv)  inz                    *data foglio preced.
010500060928      *
010600070222      * - Variabili per conteggio dati da aggiornare in FLTR800F
010700060928     d mNCL            s              3  0 inz                                  *media colli/mm tot
010800060928     d mPSP            s              3  0 inz                                  *% scostamento g pre
010900060928      * - Variabili per conteggio dati da aggiornare in FNMSS00F
011000060928     d mMLAV           s                   like(MSStlm)  inz                    *mm lavorati tot
011100060928     d mPesInd         s                   like(MSScla)  inz                    *caricati da
011200060928     d mPusSor         s                   like(MSSsor)  inz                    *smistati da
011300060928     d mOdsDna         s                   like(MSSdna)  inz                    *of x uscita piena
011400060928     d mErrMac         s                   like(MSSerr)  inz                    *of x errori machine
011500060928     d mPS2dok         s                   like(MSSps2)  inz                    *letti da pistola
011600060928     d mCSD            s                   like(MSScsd)  inz                    *letti da scanner
011700060928     d mCCV            s                   like(MSSccv)  inz                    *letti con volume
011800060928     d mCCP            s                   like(MSSccp)  inz                    *letti con peso
011900060928     d mNLP            s                   like(MSSnlp)  inz                    *non letti pistola
012000060928     d mNLS            s                   like(MSSnls)  inz                    *non letti scanner
012100060928     d mXnoMis         s                   like(MSSnmi)  inz                    *C:non misurati
012200060928     d mXnoPes         s                   like(MSSnpe)  inz                    *C:non pesati
012300060928      * - - CORRENTE - progressivo minuto
012400060929     d mPNC            s                   like(MSSpnc)  inz                    *progressivo colli
012500060928      * - - PRECEDENTE - progressivo minuto
012600060929     d mPNCpr          s                   like(MSSpnc)  inz                    *progressivo colli
012700060928      *
012800060928      * - Campi di comodo
012900060928     d wOraPenUlt      s                   like(MSAorl) inz                     *ora ultimo mm
013000060928     d wDatOraLav      s             12  0 inz                                  *data+ora lavoro
013100060928     d wDatOraUlt      s             12  0 inz                                  *data+ora ultimo mm
013200060928     d wDatCor         s              8  0 inz                                  *data corrente
013300060928     d wOraCor         s              4  0 inz                                  *ora  corrente
013400060928     d n14             s             14  0 inz                                  *numerico 14
013500060928      *
013600060928      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
013700060928      *
013800080422      * - FNMSA12L
013900060928     c     K06MSA02      klist
014000060928     c                   kfld                    KSAapl
014100060928     c                   kfld                    KSAfil
014200060928     c                   kfld                    KSAdfv
014300060928     c                   kfld                    KSAnpg
014400060928     c                   kfld                    KSAdtl
014500060928     c                   kfld                    KSAorl
014600060928     c     K02MSA02      klist
014700060928     c                   kfld                    KSAapl
014800060928     c                   kfld                    KSAfil
014900060928     c     K04MSA02      klist
015000060928     c                   kfld                    KSAapl
015100060928     c                   kfld                    KSAfil
015200060928     c                   kfld                    KSAdfv
015300060928     c                   kfld                    KSAnpg
015400080422      * - FNMSS11L
015500060928     c     K04MSS01      klist
015600060928     c                   kfld                    KSSapl
015700060928     c                   kfld                    KSSfil
015800060928     c                   kfld                    KSSdfv
015900060928     c                   kfld                    KSSnpg
016000060928      * - TNTBE01L
016100060928     c     K02TBE01      klist
016200060928     c                   kfld                    TBEcod
016300060928     c                   kfld                    TBEke1
016400060928
016500060928      *===============================================================*
016600060928      *?  I N D I C A T O R I                                        ?*
016700060928      *---------------------------------------------------------------*
016800060928      *  10 - Comodo                                                  *
016900060928      *===============================================================*
017000060928
017100060928     c     *Entry        plist
017200060928     c                   parm                    Param
017300060928      *
017400060928      * Ciclo per ogni tipo applicazione caricato in schiera
017500060928     c                   eval      YY       =  1
017600060928do  1c                   DOW       $MTP(yy) <> *blanks
017700060928      *
017800060928     c                   movel     $MTP(yy)      wTabApl
017900060928      *
018000060928      * - Legge l'ULTIMO record per sapere qual'è l'ultima lavorazione
018100060929     c                   eval      KSAapl  = wTabApl
018200060929     c                   eval      KSAfil  = PARfil
018300081201     c                   eval      KSAdfv  = PARdata
018400081201     c                   eval      KSAnpg  = PARnpg
018500060929     c                   eval      KSAdtl  = *hival
018600060929     c                   eval      KSAorl  = *hival
018700080422     c     K06MSA02      setgt     FNMSA000
018800080422     c     K02MSA02      readpe    FNMSA000
018900060929if  2c                   if        NOT  %eof(FNMSA12L)
019000060929     c                   exsr      Elab_FNMSA_1
019100060929e   2c                   endif
019200060928      *
019300060928     c                   add       1             yy
019400060928      *
019500060928e   1c                   ENDDO
019600060928      *
019700060928      * Fine
019800060928     c                   eval      *inRT   = *on
019900060928
020000060928      *---------------------------------------------------------------*
020100060928      *?Operazioni iniziali                                          ?*
020200060928      *---------------------------------------------------------------*
020300060928     c     *InzSR        BEGSR
020400060928      *
020500060928      * Reperimento dati job
020600060928     c                   exsr      DatiJob
020700060928      *
020800060928      * Caricamento tabelle occorrenti
020900060928     c                   exsr      CarTAB
021000060928      *
021100060928     c                   ENDSR
021200060928
021300060928      *---------------------------------------------------------------*
021400060928      *?Reperimento dati del job (Utente/Operativi)                  ?*
021500060928      *---------------------------------------------------------------*
021600060928     c     DatiJob       BEGSR
021700060928      *
021800060928     c     *dtaara       define    §azute        AZUTEds
021900060928     c     *dtaara       define    §datiute      dDatiUte
022000060928      *
022100060928     c                   in(E)     *dtaara
022200060928     c                   if        %error or RSUT = *blanks
022300060928     c                   clear                   TIBS34ds
022400060928     c                   call      'TIBS34R'
022500060928     c                   parm                    TIBS34ds
022600060928     c                   in        *dtaara
022700060928     c                   endif
022800060928      *
022900060928     c                   ENDSR
023000060928
023100060928      *---------------------------------------------------------------*
023200060928      *?Caricamento tabelle occorrenti                               ?*
023300060928      *---------------------------------------------------------------*
023400060928     c     CarTAB        BEGSR
023500060928      *
023600060928      * Dati tabellone
023700060928     c                   z-add     11            $MDT                           *media lampeggio
023800060928     c                   eval      TBEcod  = 'MDT'
023900060928     c                   movel(p)  PARfil        TBEke1
024000060928     c     K02TBE01      setll     TNTBE000
024100060928     c     K02TBE01      reade     TNTBE000
024200060928do  1c                   dow       NOT  %eof(TNTBE01L)
024300060928if  2c                   if        TBEatb  = *blanks
024400060928     c                   movel     TBEuni        dMDT
024500060928     c                   movel     TBEke2        xx
024600060928     c                   eval      $MDT(xx)  = §MDTmcl                          *media lampeggio
024700060928e   2c                   endif
024800060928     c     K02TBE01      reade     TNTBE000
024900060928e   1c                   enddo
025000060928      *
025100060928      * Tipo applicazione
025200060928      * - Leggo la tabella MTP e carico in schiera gli elementi
025300060928     c                   clear                   xx
025400060928     c                   clear                   $MTP
025500060928     c                   eval      TBEcod  = 'MTP'
025600060928     c     TBEcod        setll     TNTBE000
025700060928     c     TBEcod        reade     TNTBE000
025800060928do  1c                   dow       NOT  %eof(TNTBE01L)
025900060928if  2c                   if        TBEatb  = *blanks
026000060928     c                   add       1             xx
026100060928     c                   movel     TBEke1        $MTP(xx)
026200060928e   2c                   endif
026300060928     c     TBEcod        reade     TNTBE000
026400060928e   1c                   enddo
026500060928      *
026600060928     c                   ENDSR
026700060928
026800060928      *---------------------------------------------------------------*
026900060928      *?Azzeramento dei campi di memorizzazione -PRECEDENTE-         ?*
027000060928      *---------------------------------------------------------------*
027100060928     c     sr_AzzMSApr   BEGSR
027200060928      *
027300060928      * Progressivo numero colli
027400060929     c                   clear                   mPNCpr                         *progressivo colli
027500060928      *
027600060928     c                   ENDSR
027700060928
027800060928      *---------------------------------------------------------------*
027900060928      *?Azzeramento dei campi di memorizzazione -CORRENTE-           ?*
028000060928      *---------------------------------------------------------------*
028100060928     c     sr_AzzMSA     BEGSR
028200060928      *
028300060928     c                   eval      $First = *on                                 *prima lettura
028400060928      *
028500060928     c                   clear                   DEPdtlin                       *data inizio
028600060928     c                   clear                   DEPorlin                       *ora  inizio
028700060928     c                   clear                   DEPdtlfi                       *data fine
028800060928     c                   clear                   DEPorlfi                       *ora  fine
028900060928     c                   clear                   wDFVgg                         *gg data foglio
029000060928     c                   clear                   wDFVpr                         *data gg precedente
029100060929     c                   clear                   mPNC                           *progressivo colli
029200060928     c                   clear                   $EXX                           *progressivi entrate
029300060928     c                   clear                   $UXX                           *progressivi uscite
029400060928     c                   clear                   mMLAV                          *mm lavorati
029500060928     c                   clear                   $MXX                           *mm lavorati x entr.
029600060928     c                   clear                   $DTL                           *Date letture entr.
029700060928     c                   clear                   $ORL                           *Ora  letture entr.
029800060928     c                   clear                   mNCL                           *media colli/mm tot
029900060928     c                   clear                   mPSP                           *% scostamento g pre
030000060928     c                   clear                   mPesInd                        *caricati da
030100060928     c                   clear                   mPusSor                        *smistati da
030200060928     c                   clear                   mPS2dok                        *letti da pistola
030300060928     c                   clear                   mCSD                           *letti da scanner
030400060928     c                   clear                   mCCV                           *letti con volume
030500060928     c                   clear                   mCCP                           *letti con peso
030600060928     c                   clear                   mNLS                           *non letti scanner
030700060928     c                   clear                   mNLP                           *non letti pistola
030800060928     c                   clear                   mXnoMis                        *C:non misurati
030900060928     c                   clear                   mXnoPes                        *C:non pesati
031000060928     c                   clear                   mOdsDna                        *of x uscita piena
031100060928     c                   clear                   mErrMac                        *of x errori machine
031200060929     c                   clear                   $MSApep                        *colli ultimo min E.
031300060929     c                   clear                   $MSApad                        *colli ultimo min U.
031400060928      *
031500060928     c                   ENDSR
031600060929
031700060929      *---------------------------------------------------------------*
031800060929      *?Ciclo di elaborazione del singolo record dal file FNMSA10F   ?*
031900060929      *---------------------------------------------------------------*
032000060929     c     Elab_FNMSA_1  BEGSR
032100060929      *
032200060929      * Memorizza data + ora dell'ultimo minuto lavorato
032300080422     c                   movel     MSAdtl        wDatOraUlt                     *data+ora ultimo mm
032400080422     c                   move      MSAorl        wDatOraUlt
032500060929      *
032600060929      * Imposta la chiave dell'ultima lettura
032700080422     c                   eval      KSAapl  = MSAapl
032800080422     c                   eval      KSAfil  = MSAfil
032900080422     c                   eval      KSAdfv  = MSAdfv
033000080422     c                   eval      KSAnpg  = MSAnpg
033100060929      *
033200060929      * Imposta la chiave dell'ultima lettura per file saldi
033300060929     c                   eval      DEPdfv  = KSAdfv                             *data foglio
033400060929     c                   eval      DEPnpg  = KSAnpg                             *gipo foglio
033500060929      *
033600060929     c                   clear                   wOraPenult
033700060929      *
033800060929      * Legge il penultimo minuto perchè è sicuramente "completo"
033900060929      * (sono i dati relativi a questo minuto che emette sul tabellone)
034000080422     c     K04MSA02      readPE    FNMSA000
034100060929if  1c                   if        %eof(FNMSA12L)
034200060929     c                   leavesr
034300060929e   1c                   endif
034400060929      *
034500060929      * Memorizza data + ora e ora del penultimo minuto lavorato
034600080422     c                   eval      wOraPenult = MSAorl                          *ora ultimo mm
034700060929      *
034800060929      * Elabora giorno precedente per tabellone
034900060929     c                   exsr      ElabIeri_1
035000060929      *
035100060929      * Elabora oggi per tabellone e file saldi e scrive FLTR8
035200060929     c                   exsr      ElabOggi_1
035300060929      *
035400060929      * Scrive i saldi
035500060929     c                   exsr      Wrt_FNMSS_1
035600060929      *
035700060929     c                   ENDSR
035800060929
035900060929      *---------------------------------------------------------------*
036000060929      *?Elaboro dati del giorno precedente                           ?*
036100060929      *---------------------------------------------------------------*
036200060929     c     ElabIeri_1    BEGSR
036300060929      *
036400060929      * Memorizza la data foglio
036500060929     c                   eval      SAVdfv  = KSAdfv                             *data foglio preced.
036600060929      *
036700060929      * Calcola il giorno precedente utile alla data foglio trovata
036800060929     c                   reset                   WLBdat
036900060929     c                   eval      G08inv  = KSAdfv
037000060929     c                   call      'XSRDA8'
037100060929     c                   parm                    WLBdat
037200060929     c                   eval      wDFVgg  = G08tgi                             *gg data richiesta
037300060929      *
037400060929      * Ciclo per trovare il primo giorno precedente nel quale il
037500060929      *   macchinone ha lavorato
037600060929     c                   eval      xx      = 1
037700060929do  1c                   DOW       xx     <= 10
037800060929     c                   clear                   WGIdat
037900060929     c                   eval      GIOtgi  = wDFVgg - xx                        *gg data - "X" gg
038000060929     c                   call      'XSRGI8'
038100060929     c                   parm                    WGIdat
038200060929     c                   eval      wDFVpr  = GIOinv                             *data gg precedente
038300060929     c                   eval      KSAdfv  = wDFVpr                             *data foglio preced.
038400080422     c     K04MSA02      setll     FNMSA000
038500060929if  2c                   if        %equal(FNMSA12L)                             *trovati dati
038600060929     c                   leave                                                  *esce dal ciclo
038700060929e   2c                   endif
038800060929     c                   add       1             xx                             *incrementa gg meno
038900060929e   1c                   ENDDO
039000060929      *
039100060929      * Azzera i campi...
039200060929     c                   exsr      sr_AzzMSApr
039300060929      *
039400060929      * Legge tutti i dati della lavorazione -PRECEDENTE-
039500080422     c     K04MSA02      setll     FNMSA000
039600080422     c     K04MSA02      reade     FNMSA000
039700060929do  1c                   DOW       NOT  %eof(FNMSA12L)
039800080422if  2c                   if        MSAorl <= wOraPenult                         *fino all'ultimo mm
039900060929     c                   exsr      sr_MemMSApr_1
040000060929e   2c                   endif
040100080422     c     K04MSA02      reade     FNMSA000
040200060929e   1c                   ENDDO
040300060929      *
040400060929      * Repimposta la data del foglio "originale"
040500060929     c                   eval      KSAdfv  = SAVdfv                             *Data foglio preced.
040600060929      *
040700060929      * Reperisce la data + ora corrente
040800060929     c                   time                    n14                            *Ora (6)+ Data (8)
040900060929     c                   movel     n14           wOraCor                        *Ora (4)
041000060929     c                   clear                   WLBdat
041100060929     c                   move      n14           G08dat                         *Data (8) in g/m/a
041200060929     c                   eval      G08err  = *off
041300060929     c                   call      'XSRDA8'
041400060929     c                   parm                    WLBdat
041500060929     c                   eval      wDatCor = G08inv                             *Data corrente
041600060929      *
041700060929     c                   ENDSR
041800060929
041900060929      *---------------------------------------------------------------*
042000060929      *?Elaboro dati del giorno corrente                             ?*
042100060929      *---------------------------------------------------------------*
042200060929     c     ElabOggi_1    BEGSR
042300060929      *
042400060929      * Azzera i campi...
042500060929     c                   exsr      sr_AzzMSA
042600060929      *
042700060929      * Legge tutti i dati della lavorazione -CORRENTE-
042800080422     c     K04MSA02      setll     FNMSA000                                     *ultimo
042900080422     c     K04MSA02      reade     FNMSA000
043000060929      *
043100060929do  1c                   DOW       NOT  %eof(FNMSA12L)
043200060929      *
043300080422     c                   movel     MSAdtl        wDatOraLav                     *data+ora
043400080422     c                   move      MSAorl        wDatOraLav
043500060929if  2c                   if        wDatOraLav <= wDatOraUlt                     *fino all'ultimo mm
043600060929     c                   exsr      sr_MemMSA_1
043700060929e   2c                   endif
043800060929      *
043900080422     c     K04MSA02      reade     FNMSA000
044000060929      *
044100060929e   1c                   ENDDO
044200060929      *
044300060929     c                   ENDSR
044400060929
044500060929      *---------------------------------------------------------------*
044600060929      *?Memorizzazione dei dati del file -PRECEDENTE-                ?*
044700060929      *---------------------------------------------------------------*
044800060929     c     sr_MemMSApr_1 BEGSR
044900060929      *
045000060929      * Colli lavorati TOTALI  (compresa baia 20!)
045100060929     c                   eval      mPNCpr   = mPNCpr
045200080422     c                                      + MSApep001
045300080422     c                                      + MSApep002
045400080422     c                                      + MSApep003
045500080422     c                                      + MSApep004
045600080422     c                                      + MSApep005
045700080422     c                                      + MSApep006
045800080422     c                                      + MSApep007
045900080422     c                                      + MSApep008
046000080422     c                                      + MSApep009
046100080422     c                                      + MSApep010
046200080422     c                                      + MSApep011
046300080422     c                                      + MSApep012
046400080422     c                                      + MSApep013
046500080422     c                                      + MSApep014
046600080422     c                                      + MSApep015
046700080422     c                                      + MSApep016
046800080422     c                                      + MSApep017
046900080422     c                                      + MSApep018
047000080422     c                                      + MSApep019
047100080422     c                                      + MSApep020
047200060929      *
047300060929     c                   ENDSR
047400060929
047500060929      *---------------------------------------------------------------*
047600060929      *?Memorizzazione dei dati del file -CORRENTE-                  ?*
047700060929      *---------------------------------------------------------------*
047800060929     c     sr_MemMSA_1   BEGSR
047900060929      *
048000060929      * Prima data + ora lettura presente
048100060929if  1c                   if        $First   = *on                               *SI prima lettura
048200080422     c                   eval      DEPdtlin = MSAdtl                            *data inizio
048300080422     c                   eval      DEPorlin = MSAorl                            *ora  inizio
048400060929     c                   eval      $First   = *off                              *NO prima lettura
048500060929e   1c                   endif
048600060929      *
048700060929      * Ultima data + ora lettura presente
048800080422     c                   eval      DEPdtlfi = MSAdtl                            *data fine
048900080422     c                   eval      DEPorlfi = MSAorl                            *ora  fine
049000060929      *
049100060929      * Colli ULTIMO MINUTO per tabellone
049200060929     c*** superfluo:     clear                   $MSApep
049300080422     c                   eval      $MSApep( 1) = MSApep001
049400080422     c                   eval      $MSApep( 2) = MSApep002
049500080422     c                   eval      $MSApep( 3) = MSApep003
049600080422     c                   eval      $MSApep( 4) = MSApep004
049700080422     c                   eval      $MSApep( 5) = MSApep005
049800080422     c                   eval      $MSApep( 6) = MSApep006
049900080422     c                   eval      $MSApep( 7) = MSApep007
050000080422     c                   eval      $MSApep( 8) = MSApep008
050100080422     c                   eval      $MSApep( 9) = MSApep009
050200080422     c                   eval      $MSApep(10) = MSApep010
050300080422     c                   eval      $MSApep(11) = MSApep011
050400080422     c                   eval      $MSApep(12) = MSApep012
050500080422     c                   eval      $MSApep(13) = MSApep013
050600080422     c                   eval      $MSApep(14) = MSApep014
050700080422     c                   eval      $MSApep(15) = MSApep015
050800080422     c                   eval      $MSApep(16) = MSApep016
050900080422     c                   eval      $MSApep(17) = MSApep017
051000080422     c                   eval      $MSApep(18) = MSApep018
051100080422     c                   eval      $MSApep(19) = MSApep019
051200080422     c                   eval      $MSApep(20) = MSApep020
051300060929      *
051400060929      * Colli e minuti lavorati TOTALI
051500060929      * - progressivo colli  (compresa baia 20!)
051600060929     c                   clear                   xx
051700060929do  1c                   dow       xx       < %elem($MSApep)
051800060929     c                   add       1             xx
051900060929     c                   add       $MSApep(xx)   mPNC
052000060929e   1c                   enddo
052100060929      * - Progressivo minuti
052200060929     c                   add       1             mMLAV
052300060929      *
052400060929      * Colli e minuti lavorati ENTRATE
052500060929     c                   clear                   xx
052600060929     c                   DOW       xx       < %elem($MSApep)
052700060929     c                   add       1             xx
052800060929if  1c                   if        $MSApep(xx) > *zeros
052900060929     c                   add       1             $MXX(xx)                       *mm entrata
053000060929     c                   add       $MSApep(xx)   $EXX(xx)                       *progressivo  ent
053100080422     c                   eval      $DTL(xx) = MSAdtl                            *data lettura ent
053200080422     c                   eval      $ORL(xx) = MSAorl                            *ora  lettura ent
053300060929e   1c                   endif
053400060929     c                   ENDDO
053500060929      *
053600060929      * Colli USCITE
053700060929     c*** superfluo:     clear                   $MSApad
053800080422     c                   eval      $MSApad( 1) = MSApad001
053900080422     c                   eval      $MSApad( 2) = MSApad002
054000080422     c                   eval      $MSApad( 3) = MSApad003
054100080422     c                   eval      $MSApad( 4) = MSApad004
054200080422     c                   eval      $MSApad( 5) = MSApad005
054300080422     c                   eval      $MSApad( 6) = MSApad006
054400080422     c                   eval      $MSApad( 7) = MSApad007
054500080422     c                   eval      $MSApad( 8) = MSApad008
054600080422     c                   eval      $MSApad( 9) = MSApad009
054700080422     c                   eval      $MSApad(10) = MSApad010
054800080422     c                   eval      $MSApad(11) = MSApad011
054900080422     c                   eval      $MSApad(12) = MSApad012
055000080422     c                   eval      $MSApad(13) = MSApad013
055100080422     c                   eval      $MSApad(14) = MSApad014
055200080422     c                   eval      $MSApad(15) = MSApad015
055300080422     c                   eval      $MSApad(16) = MSApad016
055400080422     c                   eval      $MSApad(17) = MSApad017
055500080422     c                   eval      $MSApad(18) = MSApad018
055600080422     c                   eval      $MSApad(19) = MSApad019
055700080422     c                   eval      $MSApad(20) = MSApad020
055800080422     c                   eval      $MSApad(21) = MSApad021
055900080422     c                   eval      $MSApad(22) = MSApad022
056000080422     c                   eval      $MSApad(23) = MSApad023
056100080422     c                   eval      $MSApad(24) = MSApad024
056200080422     c                   eval      $MSApad(25) = MSApad025
056300080422     c                   eval      $MSApad(26) = MSApad026
056400080422     c                   eval      $MSApad(27) = MSApad027
056500080422     c                   eval      $MSApad(28) = MSApad028
056600080422     c                   eval      $MSApad(29) = MSApad029
056700080422     c                   eval      $MSApad(30) = MSApad030
056800080422     c                   eval      $MSApad(31) = MSApad031
056900080422     c                   eval      $MSApad(32) = MSApad032
057000080422     c                   eval      $MSApad(33) = MSApad033
057100080422     c                   eval      $MSApad(34) = MSApad034
057200080422     c                   eval      $MSApad(35) = MSApad035
057300080422     c                   eval      $MSApad(36) = MSApad036
057400080422     c                   eval      $MSApad(37) = MSApad037
057500080422     c                   eval      $MSApad(38) = MSApad038
057600080422     c                   eval      $MSApad(39) = MSApad039
057700080422     c                   eval      $MSApad(40) = MSApad040
057800080422     c                   eval      $MSApad(41) = MSApad041
057900080422     c                   eval      $MSApad(42) = MSApad042
058000080422     c                   eval      $MSApad(43) = MSApad043
058100080422     c                   eval      $MSApad(44) = MSApad044
058200080422     c                   eval      $MSApad(45) = MSApad045
058300080422     c                   eval      $MSApad(46) = MSApad046
058400080422     c                   eval      $MSApad(47) = MSApad047
058500080422     c                   eval      $MSApad(48) = MSApad048
058600080422     c                   eval      $MSApad(49) = MSApad049
058700080422     c                   eval      $MSApad(50) = MSApad050
058800080422     c                   eval      $MSApad(51) = MSApad051
058900080422     c                   eval      $MSApad(52) = MSApad052
059000080422     c                   eval      $MSApad(53) = MSApad053
059100080422     c                   eval      $MSApad(54) = MSApad054
059200080422     c                   eval      $MSApad(55) = MSApad055
059300080422     c                   eval      $MSApad(56) = MSApad056
059400080422     c                   eval      $MSApad(57) = MSApad057
059500080422     c                   eval      $MSApad(58) = MSApad058
059600080422     c                   eval      $MSApad(59) = MSApad059
059700080422     c                   eval      $MSApad(60) = MSApad060
059800060929     c                   add       $MSApad       $UXX                           *progr.colli x usc.
059900060929      *
060000060929      * Dati statistici
060100080422     c                   add       MSApesind     mPesInd                        *caricati
060200080422     c                   add       MSApussor     mPusSor                        *smistati
060300080422     c                   add       MSAps2dok     mPs2Dok                        *letti da pistola
060400080422     c                   add       MSAcsd        mCSD                           *letti da scanner
060500080422     c                   add       MSAccv        mCCV                           *letti con volume
060600080422     c                   add       MSAccp        mCCP                           *letti con peso
060700080422     c                   add       MSAnls        mNLS                           *non letti scanner
060800080422     c                   add       MSAnlp        mNLP                           *non letti pistola
060900060929      * C: non misurati
061000060929     c                   eval      mXnoMis  = mXnoMis                           *C: non misurati
061100080422     c                                      + (MSApussor - MSAccv)
061200060929      * C: non pesati
061300060929     c                   eval      mXnoPes  = mXnoPes                           *C: non pesati
061400080422     c                                      + (MSApussor - MSAccp)
061500060929      * - OF x uscita piena
061600080422     c                   add       MSAodsdna     mOdsDna                        · TR4ods = '3'
061700060929      * - OF x errori macchina
061800060929     c                   eval      mErrMac  = mErrMac
061900080422     c                                      + MSAodsdnr                         + TR4ods = '2'
062000080422     c                                      + MSAodsftd                         + TR4ods = '4'
062100080422     c                                      + MSAodsids                         + TR4ods = '5'
062200080422     c                                      + MSAodsuds                         + TR4ods = '6'
062300080422     c                                      + MSAodsdne                         + TR4ods = '7'
062400080422     c                                      + MSAodsciv                         + TR4ods = '8'
062500080422     c                                      + MSAodsdnq                         + TR4ods = '9'
062600080422     c*** NO "unknown state":               + MSAodsust                         + TR4ods = '0'
062700060929      *
062800060929     c                   ENDSR
062900060929
063000060929      *---------------------------------------------------------------*
063100060929      *?Scrittura del file per le statistiche                        ?*
063200060929      *---------------------------------------------------------------*
063300060929     c     Wrt_FNMSS_1   BEGSR
063400060929      *
063500080422     c                   clear                   FNMSS000
063600060929      *
063700060929      * Reperisce il record da aggiornare
063800080422     c                   eval      KSSapl   = MSAapl                            *tipo applicazione
063900060929     c                   eval      KSSfil   = PARfil                            *p.o.
064000060929     c                   eval      KSSdfv   = DEPdfv                            *data foglio
064100060929     c                   eval      KSSnpg   = DEPnpg                            *categoria foglio
064200080422     c     K04MSS01      chain     FNMSS000
064300060929      *
064400060929      * Imposta i campi
064500080422     c                   eval      MSSpnc   = mPNC                              *progressivo colli
064600080422     c                   eval      MSStlm   = mMLAV                             *mm lavorati
064700060929      * - colli per entrata
064800080422     c                   eval      MSSe01   = $EXX( 1)
064900080422     c                   eval      MSSe02   = $EXX( 2)
065000080422     c                   eval      MSSe03   = $EXX( 3)
065100080422     c                   eval      MSSe04   = $EXX( 4)
065200080422     c                   eval      MSSe05   = $EXX( 5)
065300080422     c                   eval      MSSe06   = $EXX( 6)
065400080422     c                   eval      MSSe07   = $EXX( 7)
065500080422     c                   eval      MSSe08   = $EXX( 8)
065600080422     c                   eval      MSSe09   = $EXX( 9)
065700080422     c                   eval      MSSe10   = $EXX(10)
065800080422     c                   eval      MSSe11   = $EXX(11)
065900080422     c                   eval      MSSe12   = $EXX(12)
066000080422     c                   eval      MSSe13   = $EXX(13)
066100080422     c                   eval      MSSe14   = $EXX(14)
066200080422     c                   eval      MSSe15   = $EXX(15)
066300080422     c                   eval      MSSe16   = $EXX(16)
066400080422     c                   eval      MSSe17   = $EXX(17)
066500080422     c                   eval      MSSe18   = $EXX(18)
066600080422     c                   eval      MSSe19   = $EXX(19)
066700080422     c                   eval      MSSe20   = $EXX(20)
066800060929      * - minuti per entrata
066900080422     c                   eval      MSSm01   = $MXX( 1)
067000080422     c                   eval      MSSm02   = $MXX( 2)
067100080422     c                   eval      MSSm03   = $MXX( 3)
067200080422     c                   eval      MSSm04   = $MXX( 4)
067300080422     c                   eval      MSSm05   = $MXX( 5)
067400080422     c                   eval      MSSm06   = $MXX( 6)
067500080422     c                   eval      MSSm07   = $MXX( 7)
067600080422     c                   eval      MSSm08   = $MXX( 8)
067700080422     c                   eval      MSSm09   = $MXX( 9)
067800080422     c                   eval      MSSm10   = $MXX(10)
067900080422     c                   eval      MSSm11   = $MXX(11)
068000080422     c                   eval      MSSm12   = $MXX(12)
068100080422     c                   eval      MSSm13   = $MXX(13)
068200080422     c                   eval      MSSm14   = $MXX(14)
068300080422     c                   eval      MSSm15   = $MXX(15)
068400080422     c                   eval      MSSm16   = $MXX(16)
068500080422     c                   eval      MSSm17   = $MXX(17)
068600080422     c                   eval      MSSm18   = $MXX(18)
068700080422     c                   eval      MSSm19   = $MXX(19)
068800080422     c                   eval      MSSm20   = $MXX(20)
068900060929      * Colli per uscita
069000080422     c                   eval      MSSu01   = $UXX( 1)
069100080422     c                   eval      MSSu02   = $UXX( 2)
069200080422     c                   eval      MSSu03   = $UXX( 3)
069300080422     c                   eval      MSSu04   = $UXX( 4)
069400080422     c                   eval      MSSu05   = $UXX( 5)
069500080422     c                   eval      MSSu06   = $UXX( 6)
069600080422     c                   eval      MSSu07   = $UXX( 7)
069700080422     c                   eval      MSSu08   = $UXX( 8)
069800080422     c                   eval      MSSu09   = $UXX( 9)
069900080422     c                   eval      MSSu10   = $UXX(10)
070000080422     c                   eval      MSSu11   = $UXX(11)
070100080422     c                   eval      MSSu12   = $UXX(12)
070200080422     c                   eval      MSSu13   = $UXX(13)
070300080422     c                   eval      MSSu14   = $UXX(14)
070400080422     c                   eval      MSSu15   = $UXX(15)
070500080422     c                   eval      MSSu16   = $UXX(16)
070600080422     c                   eval      MSSu17   = $UXX(17)
070700080422     c                   eval      MSSu18   = $UXX(18)
070800080422     c                   eval      MSSu19   = $UXX(19)
070900080422     c                   eval      MSSu20   = $UXX(20)
071000080422     c                   eval      MSSu21   = $UXX(21)
071100080422     c                   eval      MSSu22   = $UXX(22)
071200080422     c                   eval      MSSu23   = $UXX(23)
071300080422     c                   eval      MSSu24   = $UXX(24)
071400080422     c                   eval      MSSu25   = $UXX(25)
071500080422     c                   eval      MSSu26   = $UXX(26)
071600080422     c                   eval      MSSu27   = $UXX(27)
071700080422     c                   eval      MSSu28   = $UXX(28)
071800080422     c                   eval      MSSu29   = $UXX(29)
071900080422     c                   eval      MSSu30   = $UXX(30)
072000080422     c                   eval      MSSu31   = $UXX(31)
072100080422     c                   eval      MSSu32   = $UXX(32)
072200080422     c                   eval      MSSu33   = $UXX(33)
072300080422     c                   eval      MSSu34   = $UXX(34)
072400080422     c                   eval      MSSu35   = $UXX(35)
072500080422     c                   eval      MSSu36   = $UXX(36)
072600080422     c                   eval      MSSu37   = $UXX(37)
072700080422     c                   eval      MSSu38   = $UXX(38)
072800080422     c                   eval      MSSu39   = $UXX(39)
072900080422     c                   eval      MSSu40   = $UXX(40)
073000080422     c                   eval      MSSu41   = $UXX(41)
073100080422     c                   eval      MSSu42   = $UXX(42)
073200080422     c                   eval      MSSu43   = $UXX(43)
073300080422     c                   eval      MSSu44   = $UXX(44)
073400080422     c                   eval      MSSu45   = $UXX(45)
073500080422     c                   eval      MSSu46   = $UXX(46)
073600080422     c                   eval      MSSu47   = $UXX(47)
073700080422     c                   eval      MSSu48   = $UXX(48)
073800080422     c                   eval      MSSu49   = $UXX(49)
073900080422     c                   eval      MSSu50   = $UXX(50)
074000080422     c                   eval      MSSu51   = $UXX(51)
074100080422     c                   eval      MSSu52   = $UXX(52)
074200080422     c                   eval      MSSu53   = $UXX(53)
074300080422     c                   eval      MSSu54   = $UXX(54)
074400080422     c                   eval      MSSu55   = $UXX(55)
074500080422     c                   eval      MSSu56   = $UXX(56)
074600080422     c                   eval      MSSu57   = $UXX(57)
074700080422     c                   eval      MSSu58   = $UXX(58)
074800080422     c                   eval      MSSu59   = $UXX(59)
074900080422     c                   eval      MSSu60   = $UXX(60)
075000060929      *
075100080422     c                   eval      MSScla   = mPesInd                           *caricati da
075200080422     c                   eval      MSSsor   = mPusSor                           *smistati da
075300080422     c                   eval      MSSdna   = mOdsDna                           *of x uscita piena
075400080422     c                   eval      MSSerr   = mErrMac                           *of x errori macchin
075500080422     c                   eval      MSSps2   = mPs2Dok                           *letti da pistaola
075600080422     c                   eval      MSScsd   = mCSD                              *letti da scanner
075700080422     c                   eval      MSSccv   = mCCV                              *letti con volume
075800080422     c                   eval      MSSccp   = mCCP                              *letti con peso
075900080422     c                   eval      MSSnlp   = mNLP                              *non letti pistola
076000080422     c                   eval      MSSnls   = mNLS                              *non letti scanner
076100080422     c                   eval      MSSnpe   = mXnoPes                           *C:non pesati
076200080422     c                   eval      MSSnmi   = mXnoMis                           *C:non misurati
076300060929      *
076400060929      * Scrive
076500060929if  1c                   if        NOT  %found(FNMSS11L)                        *inesistente
076600080422     c                   eval      MSSapl   = KSSapl                            *tipo applicazione
076700080422     c                   eval      MSSfil   = KSSfil                            *p.o.
076800080422     c                   eval      MSSdfv   = KSSdfv                            *data foglio
076900080422     c                   eval      MSSnpg   = KSSnpg                            *categoria foglio
077000060929      *                  __________________
077100080422     c                   WRITE     FNMSS000                                     *INSERIMENTO
077200060929      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
077300060929x   1c                   else                                                   *esistente
077400060929      *                  __________________
077500080422     c                   UPDATE    FNMSS000                                     *MODIFICA
077600060929      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
077700060929e   1c                   endif
077800060929      *
077900060929     c                   ENDSR
