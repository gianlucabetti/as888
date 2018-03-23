000100161214     /*END
000200060928      *---------------------------------------------------------------*
000300060928      * Passaggio dati VdL al tabellone                               *
000400060928      *---------------------------------------------------------------*
000500060928
000600060928     h decedit('0,') datedit(*dmy/)
000700060928
000800060928      *---------------------------------------------------------------*
000900060928
001000060928     fTNTBE01L  if   e           k disk
001100080422     fFNMSA12L  if   e           k disk
001200161214     fFNMSA02L  if   e           k disk    rename( FNMSA000 : FNMSA002 )
001300161214     f                                     prefix( SA0 : 3 )
001400060928      *
001500060928     fFLTR801L  Uf A e           k disk
001600080422     fFNMSS11L  Uf A e           k disk
001700161214     fFNMSS01L  Uf A e           k disk    rename( FNMSS000 : FNMSS001 )
001800161214     f                                     prefix( SS0 : 3 )
001900060928
002000060928      *---------------------------------------------------------------*
002100060928
002200060928      *
002300060928      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
002400060928      *
002500060928      *
002600060928      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
002700060928      * - P.O. (da applicazioni per macchinone)
002800060928     d $MTP            s              1    dim(100)
002900060928      *
003000060928      * - Media colli/minuto da tab. MDT
003100060928     d $MDT            s              2  0 dim( 10) inz                         *media lampeggio x b
003200060929      *
003300060929      * - Schiere per "semplificare" il calcolo su campi simili...
003400060929      * - - Colli per entrata
003500060929     d $EXX            s                   like(MSSe01)     dim(20)  inz
003600060929      * - - Minuti lavorati per entrata
003700060929     d $MXX            s                   like(MSSm01)     dim(20)  inz
003800060929      * - - Colli per uscita
003900060929     d $UXX            s                   like(MSSu01)     dim(60)  inz
004000060929      * - - Date lettura per entrata
004100060929     d $DTL            s                   like(MSAdtl)     dim(20)  inz
004200060929      * - - Orari lettura per entrata
004300060929     d $ORL            s                   like(MSAorl)     dim(20)  inz
004400060929      * - - Colli ultimo minuto per entrata - CORRENTE
004500060929     d $MSApep         s                   like(MSApep001)  dim(20)  inz
004600060929      * - - Colli ultimo minuto per uscita  - CORRENTE
004700060929     d $MSApad         s                   like(MSApad001)  dim(60)  inz
004800060929      * - - Media per entrata
004900060929     d $TR8M           s              2  0 dim(20)  inz
005000060928      *
005100060928      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
005200060928      *
005300060928      * - Parametri per richiamo al pgm di controllo profilo utenti
005400060928     d TIBS34ds      e ds
005500060928      * - Ds di riferimento al file esterno AzUte00F
005600060928     d AZUTEds       e ds                  extname(AZUTE00F)
005700060928      * - Ds per dati organigramma
005800060928     d DDatiUte      e ds
005900060928      *
006000060928      * - Dati tabellone
006100060928     d dMDT          e ds                  inz
006200060928      *
006300060928      * - Parametri in INPUT
006400060928     d Param           ds                                                       *DS parametri
006500060928     d  PARfil                 1      3  0                                      - filiale
006600060928     d  PARapl                 4      4                                         - applicazione
006700060928      *
006800060928      * - Controlla data
006900060928     d WLBdat          ds                  inz
007000060928     d  G08dat                        8  0 inz
007100060928     d  G08inv                        8  0 inz
007200060928     d  G08err                        1    inz('3')
007300060928     d  G08tgi                        5  0 inz
007400060928      * - Calcolo data
007500060928     d WGIdat          ds                  inz
007600060928     d  GIOdat                        8  0 inz
007700060928     d  GIOinv                        8  0 inz
007800060928     d  GIOtgi                        5  0 inz
007900060928      *
008000060928      * - Variabili "temporali"
008100060928     d                 ds                  inz
008200060928     d wTempo                          z   inz(z'1901-01-01-01.00.00.000000')
008300060928     d  wData                          d   datfmt(*iso) overlay(wTempo)
008400060928     d  wOra                           t   timfmt(*iso) overlay(wTempo : 12)
008500060928      *
008600060928      * - di comodo (per "raggruppare" campi legati)
008700060928      * - - Flags lampeggio in FLTR800F
008800060929     d                 ds                  inz
008900060928     d   TR8b01                            inz
009000060928     d   TR8b02                            inz
009100060928     d   TR8b03                            inz
009200060928     d   TR8b04                            inz
009300060928     d   TR8b05                            inz
009400060928     d   TR8b06                            inz
009500060928     d   TR8b07                            inz
009600060928     d   TR8b08                            inz
009700060928     d   TR8b09                            inz
009800060928     d   TR8b10                            inz
009900060928     d  $TR8b                  1     10    dim(10)       inz
010000060928      *
010100060928      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
010200060928      *
010300060928      * - Flags booleani
010400060928     d $First          s              1    inz(*on)                             *prima lettura
010500060928      *
010600060928      * - Indici di schiera
010700060928     d XX              s              3  0 inz
010800060928     d YY              s              3  0 inz
010900060928      *
011000060928      * - Campi chiave riferiti al data base
011100060928     d KSAfil          s                   like(MSAfil)  inz
011200060928     d KSAapl          s                   like(MSAapl)  inz
011300060928     d KSAdfv          s                   like(MSAdfv)  inz
011400060928     d KSAnpg          s                   like(MSAnpg)  inz
011500060928     d KSAdtl          s                   like(MSAdtl)  inz
011600060928     d KSAorl          s                   like(MSAorl)  inz
011700161214     d*//· KSAimp          s                   like(SA0imp)  inz
011800161214     d*//· KSAlav          s                   like(SA0lav)  inz
011900060928     d KR8fil          s                   like(TR8fil)  inz
012000060928     d KR8tip          s                   like(TR8tip)  inz
012100060928     d KSSapl          s                   like(MSSapl)  inz
012200060928     d KSSfil          s                   like(MSSfil)  inz
012300060928     d KSSdfv          s                   like(MSSdfv)  inz
012400060928     d KSSnpg          s                   like(MSSnpg)  inz
012500161214     d*//· KSSimp          s                   like(SS0imp)  inz
012600161214     d*//· KSSlav          s                   like(SS0lav)  inz
012700060928      *
012800060928      * - Altre avriabili riferite al data base
012900060928     d wTabApl         s                   like(MSAapl)  inz
013000060928     d wDFVpr          s                   like(MSAdfv)  inz                    *di lavoro
013100060928     d wDFVgg          s                   like(MSAdfv)  inz
013200060928     d DEPdtlin        s                   like(MSAdtl)  inz                    *dep data inizio
013300060928     d DEPorlin        s                   like(MSAorl)  inz                    *dep ora  inizio
013400060928     d DEPdtlfi        s                   like(MSAdtl)  inz                    *dep data fine
013500060928     d DEPorlfi        s                   like(MSAorl)  inz                    *dep ora  fine
013600060928     d DEPdfv          s                   like(MSSdfv)  inz
013700060928     d DEPnpg          s                   like(MSSnpg)  inz
013800161214     d DEPimp          s                   like(SS0imp)  inz
013900161214     d DEPlav          s                   like(SS0lav)  inz
014000060928     d SAVdfv          s                   like(MSAdfv)  inz                    *data foglio preced.
014100060928      *
014200070222      * - Variabili per conteggio dati da aggiornare in FLTR800F
014300060928     d mNCL            s              3  0 inz                                  *media colli/mm tot
014400060928     d mPSP            s              3  0 inz                                  *% scostamento g pre
014500060928      * - Variabili per conteggio dati da aggiornare in FNMSS00F
014600060928     d mMLAV           s                   like(MSStlm)  inz                    *mm lavorati tot
014700060928     d mPesInd         s                   like(MSScla)  inz                    *caricati da
014800060928     d mPusSor         s                   like(MSSsor)  inz                    *smistati da
014900060928     d mOdsDna         s                   like(MSSdna)  inz                    *of x uscita piena
015000060928     d mErrMac         s                   like(MSSerr)  inz                    *of x errori machine
015100060928     d mPS2dok         s                   like(MSSps2)  inz                    *letti da pistola
015200060928     d mCSD            s                   like(MSScsd)  inz                    *letti da scanner
015300161214     d mPWF            s                   like(SS0pwf)  inz                    *letti da pist.Wi-Fi
015400060928     d mCCV            s                   like(MSSccv)  inz                    *letti con volume
015500060928     d mCCP            s                   like(MSSccp)  inz                    *letti con peso
015600060928     d mNLP            s                   like(MSSnlp)  inz                    *non letti pistola
015700060928     d mNLS            s                   like(MSSnls)  inz                    *non letti scanner
015800060928     d mXnoMis         s                   like(MSSnmi)  inz                    *C:non misurati
015900060928     d mXnoPes         s                   like(MSSnpe)  inz                    *C:non pesati
016000060928      * - - CORRENTE - progressivo minuto
016100060929     d mPNC            s                   like(MSSpnc)  inz                    *progressivo colli
016200060928      * - - PRECEDENTE - progressivo minuto
016300060929     d mPNCpr          s                   like(MSSpnc)  inz                    *progressivo colli
016400060928      *
016500060928      * - Campi di comodo
016600060928     d wOraPenUlt      s                   like(MSAorl) inz                     *ora ultimo mm
016700060928     d wDatOraCor      s             12  0 inz                                  *data+ora corr + 10m
016800060928     d wDatOraLav      s             12  0 inz                                  *data+ora lavoro
016900060928     d wDatOraUlt      s             12  0 inz                                  *data+ora ultimo mm
017000060928     d wDatOraPenult   s             12  0 inz                                  *data+ora penult mm
017100060928     d wDatCor         s              8  0 inz                                  *data corrente
017200060928     d wOraCor         s              4  0 inz                                  *ora  corrente
017300060928     d §PNCatt         s             11  3 inz                                  *prg colli attuale
017400060928     d §PNCpre         s             11  3 inz                                  *prg colli preced
017500060928     d §PNCpsp         s              3  0 inz                                  *% scostamento
017600060928     d §Gap            s             18  6 inz
017700060928     d n14             s             14  0 inz                                  *numerico 14
017800061002     d w0080           s              8  0 inz                                  *numerico 8
017900061002     d w0060           s              6  0 inz                                  *numerico 6
018000061002     d w0040           s              4  0 inz                                  *numerico 4
018100061002     d w0030           s              3  0 inz                                  *numerico 3
018200061002     d w003a           s              3    inz                                  *di lavoro
018300060928      *
018400060928      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
018500060928      *
018600161214      * - FNMSA12L e FNMSA02L
018700060928     c     K06MSA02      klist
018800060928     c                   kfld                    KSAapl
018900060928     c                   kfld                    KSAfil
019000060928     c                   kfld                    KSAdfv
019100060928     c                   kfld                    KSAnpg
019200060928     c                   kfld                    KSAdtl
019300060928     c                   kfld                    KSAorl
019400060928     c     K02MSA02      klist
019500060928     c                   kfld                    KSAapl
019600060928     c                   kfld                    KSAfil
019700060928     c     K04MSA02      klist
019800060928     c                   kfld                    KSAapl
019900060928     c                   kfld                    KSAfil
020000060928     c                   kfld                    KSAdfv
020100060928     c                   kfld                    KSAnpg
020200161214      * - FNMSS11L e FNMSS01L
020300060928     c     K04MSS01      klist
020400060928     c                   kfld                    KSSapl
020500060928     c                   kfld                    KSSfil
020600060928     c                   kfld                    KSSdfv
020700060928     c                   kfld                    KSSnpg
020800060928      * - FLTR801L
020900060928     c     K02TR801      klist
021000060928     c                   kfld                    KR8fil
021100060928     c                   kfld                    KR8tip
021200060928      * - TNTBE01L
021300060928     c     K02TBE01      klist
021400060928     c                   kfld                    TBEcod
021500060928     c                   kfld                    TBEke1
021600060928
021700060928      *===============================================================*
021800060928      *?  I N D I C A T O R I                                        ?*
021900060928      *---------------------------------------------------------------*
022000060928      *  10 - Comodo                                                  *
022100060928      *===============================================================*
022200060928
022300060928     c     *Entry        plist
022400060928     c                   parm                    Param
022500060928      *
022600060928      * Ciclo per ogni tipo applicazione caricato in schiera
022700060928     c                   eval      YY       =  1
022800060928do  1c                   DOW       $MTP(yy) <> *blanks
022900060928      *
023000060928     c                   movel     $MTP(yy)      wTabApl
023100060928      *
023200060928      * - Legge l'ULTIMO record per sapere qual'è l'ultima lavorazione
023300060929     c                   eval      KSAapl  = wTabApl
023400060929     c                   eval      KSAfil  = PARfil
023500060929     c                   eval      KSAdfv  = *hival
023600060929     c                   eval      KSAnpg  = *hival
023700060929     c                   eval      KSAdtl  = *hival
023800060929     c                   eval      KSAorl  = *hival
023900080422     c     K06MSA02      setgt     FNMSA000
024000080422     c     K02MSA02      readpe    FNMSA000
024100060929if  2c                   if        NOT  %eof(FNMSA12L)
024200060929     c                   exsr      Elab_FNMSA_1
024300060929e   2c                   endif
024400161214      *
024500161214      * -?...Altro giro per FNMSA00F => FNMSS00F?
024600161214     c                   eval      KSAapl  = wTabApl
024700161214     c                   eval      KSAfil  = PARfil
024800161214     c                   eval      KSAdfv  = *hival
024900161214     c                   eval      KSAnpg  = *hival
025000161214     c                   eval      KSAdtl  = *hival
025100161214     c                   eval      KSAorl  = *hival
025200161214     c     K06MSA02      setgt     FNMSA002
025300161214     c     K02MSA02      readpe    FNMSA002
025400161214if  2c                   if        NOT  %eof(FNMSA02L)
025500161214     c                   exsr      Elab_FNMSA
025600161214e   2c                   endif
025700060928      *
025800060928     c                   add       1             yy
025900060928      *
026000060928e   1c                   ENDDO
026100060928      *
026200060928      * Fine
026300060928     c                   eval      *inRT   = *on
026400060928
026500060928      *---------------------------------------------------------------*
026600060928      *?Operazioni iniziali                                          ?*
026700060928      *---------------------------------------------------------------*
026800060928     c     *InzSR        BEGSR
026900060928      *
027000060928      * Reperimento dati job
027100060928     c                   exsr      DatiJob
027200060928      *
027300060928      * Caricamento tabelle occorrenti
027400060928     c                   exsr      CarTAB
027500060928      *
027600060928     c                   ENDSR
027700060928
027800060928      *---------------------------------------------------------------*
027900060928      *?Reperimento dati del job (Utente/Operativi)                  ?*
028000060928      *---------------------------------------------------------------*
028100060928     c     DatiJob       BEGSR
028200060928      *
028300060928     c     *dtaara       define    §azute        AZUTEds
028400060928     c     *dtaara       define    §datiute      dDatiUte
028500060928      *
028600060928     c                   in(E)     *dtaara
028700060928     c                   if        %error or RSUT = *blanks
028800060928     c                   clear                   TIBS34ds
028900060928     c                   call      'TIBS34R'
029000060928     c                   parm                    TIBS34ds
029100060928     c                   in        *dtaara
029200060928     c                   endif
029300060928      *
029400060928     c                   ENDSR
029500060928
029600060928      *---------------------------------------------------------------*
029700060928      *?Caricamento tabelle occorrenti                               ?*
029800060928      *---------------------------------------------------------------*
029900060928     c     CarTAB        BEGSR
030000060928      *
030100160705      * Dati tabellone della Filiale
030200060928     c                   eval      TBEcod  = 'MDT'
030300060928     c                   movel(p)  PARfil        TBEke1
030400060928     c     K02TBE01      setll     TNTBE000
030500060928     c     K02TBE01      reade     TNTBE000
030600060928do  1c                   dow       NOT  %eof(TNTBE01L)
030700060928if  2c                   if        TBEatb  = *blanks
030800060928     c                   movel     TBEuni        dMDT
030900060928     c                   movel     TBEke2        xx
031000060928     c                   eval      $MDT(xx)  = §MDTmcl                          *media lampeggio
031100060928e   2c                   endif
031200060928     c     K02TBE01      reade     TNTBE000
031300060928e   1c                   enddo
031400160707      *
031500160707      * Dati tabellone di Default
031600160707if  1c                   If        %xfoot( $MDT ) = *zero
031700160707     c                   eval      TBEcod  = 'MDT'
031800160707     c                   movel(p)  '999'         TBEke1
031900160707     c     K02TBE01      setll     TNTBE000
032000160707     c     K02TBE01      reade     TNTBE000
032100160707do  2c                   dow       NOT  %eof(TNTBE01L)
032200160707if  3c                   if        TBEatb  = *blanks
032300160707     c                   movel     TBEuni        dMDT
032400160707     c                   movel     TBEke2        xx
032500160707     c                   eval      $MDT(xx)  = §MDTmcl                          *media lampeggio
032600160707e   3c                   endif
032700160707     c     K02TBE01      reade     TNTBE000
032800160707e   2c                   enddo
032900160707e   1c                   EndIf
033000060928      *
033100060928      * Tipo applicazione
033200060928      * - Leggo la tabella MTP e carico in schiera gli elementi
033300060928     c                   clear                   xx
033400060928     c                   clear                   $MTP
033500060928     c                   eval      TBEcod  = 'MTP'
033600060928     c     TBEcod        setll     TNTBE000
033700060928     c     TBEcod        reade     TNTBE000
033800060928do  1c                   dow       NOT  %eof(TNTBE01L)
033900060928if  2c                   if        TBEatb  = *blanks
034000060928     c                   add       1             xx
034100060928     c                   movel     TBEke1        $MTP(xx)
034200060928e   2c                   endif
034300060928     c     TBEcod        reade     TNTBE000
034400060928e   1c                   enddo
034500060928      *
034600060928     c                   ENDSR
034700060928
034800060928      *---------------------------------------------------------------*
034900060928      *?Azzeramento dei campi di memorizzazione -PRECEDENTE-         ?*
035000060928      *---------------------------------------------------------------*
035100060928     c     sr_AzzMSApr   BEGSR
035200060928      *
035300060928      * Progressivo numero colli
035400060929     c                   clear                   mPNCpr                         *progressivo colli
035500060928      *
035600060928     c                   ENDSR
035700060928
035800060928      *---------------------------------------------------------------*
035900060928      *?Azzeramento dei campi di memorizzazione -CORRENTE-           ?*
036000060928      *---------------------------------------------------------------*
036100060928     c     sr_AzzMSA     BEGSR
036200060928      *
036300060928     c                   eval      $First = *on                                 *prima lettura
036400060928      *
036500060928     c                   clear                   DEPdtlin                       *data inizio
036600060928     c                   clear                   DEPorlin                       *ora  inizio
036700060928     c                   clear                   DEPdtlfi                       *data fine
036800060928     c                   clear                   DEPorlfi                       *ora  fine
036900060928     c                   clear                   wDFVgg                         *gg data foglio
037000060928     c                   clear                   wDFVpr                         *data gg precedente
037100060929     c                   clear                   mPNC                           *progressivo colli
037200060928     c                   clear                   $EXX                           *progressivi entrate
037300060928     c                   clear                   $UXX                           *progressivi uscite
037400060928     c                   clear                   mMLAV                          *mm lavorati
037500060928     c                   clear                   $MXX                           *mm lavorati x entr.
037600060928     c                   clear                   $DTL                           *Date letture entr.
037700060928     c                   clear                   $ORL                           *Ora  letture entr.
037800060928     c                   clear                   mNCL                           *media colli/mm tot
037900060928     c                   clear                   $TR8M                          *media x entrata
038000060928     c                   clear                   mPSP                           *% scostamento g pre
038100060928     c                   clear                   mPesInd                        *caricati da
038200060928     c                   clear                   mPusSor                        *smistati da
038300060928     c                   clear                   mPS2dok                        *letti da pistola
038400060928     c                   clear                   mCSD                           *letti da scanner
038500161214     c                   clear                   mPWF                           *letti da pist.Wi-Fi
038600060928     c                   clear                   mCCV                           *letti con volume
038700060928     c                   clear                   mCCP                           *letti con peso
038800060928     c                   clear                   mNLS                           *non letti scanner
038900060928     c                   clear                   mNLP                           *non letti pistola
039000060928     c                   clear                   mXnoMis                        *C:non misurati
039100060928     c                   clear                   mXnoPes                        *C:non pesati
039200060928     c                   clear                   mOdsDna                        *of x uscita piena
039300060928     c                   clear                   mErrMac                        *of x errori machine
039400060929     c                   clear                   $MSApep                        *colli ultimo min E.
039500060929     c                   clear                   $MSApad                        *colli ultimo min U.
039600060928      *
039700060928     c                   ENDSR
039800060928
039900060928      *---------------------------------------------------------------*
040000060928      *?Sottrazione 10 minuti alla data + ora corrente               ?*
040100060928      *---------------------------------------------------------------*
040200060928     c     sr_SubMin     BEGSR
040300060928      *
040400061002     c                   clear                   w0060
040500061002     c                   movel     w0040         w0060
040600061002     c     *iso          move      w0080         wData
040700061002     c     *iso          move      w0060         wOra
040800060928     c                   subdur    10:*mn        wTempo
040900061002     c     *iso          move      wData         w0080
041000061002     c     *iso          move      wOra          w0060
041100061002     c                   movel     w0060         w0040
041200060928      *
041300060928     c                   ENDSR
041400060928
041500060928      *---------------------------------------------------------------*
041600060928      *?Calcolo dei flag "lampeggi"                                  ?*
041700060928      *---------------------------------------------------------------*
041800060928     c     CtrBlink      BEGSR
041900060928      *
042000060928     c                   clear                   xx
042100060928      *
042200060928do  1c                   DOW       xx        < %elem($TR8b)
042300060928      *
042400060928     c                   add       1             xx
042500060929if  2c                   if             $TR8m(xx) > *zeros
042600060929     c                             and  $TR8m(xx) < $MDT(xx)
042700060928      *
042800060928     c                   movel     $DTL(xx)      wDatOraLav
042900060928     c                   move      $ORL(xx)      wDatOraLav
043000060928if  3c                   if        wDatOraLav   >= wDatOraCor
043100060928     c                   eval      $TR8b(xx) = 'S'                              *LAMP media ent
043200060928e   3c                   endif
043300060928      *
043400060928e   2c                   endif
043500060928      *
043600060928e   1c                   ENDDO
043700060928      *
043800060928     c                   ENDSR
043900060928
044000060928      *---------------------------------------------------------------*
044100060928      *?calcola la percentuale di scostamento                        ?*
044200060929      *?   INPUT  --> §PNCatt - > "X" attuali                        ?*
044300060929      *?              §PNCpre - > "X" precedenti                     ?*
044400060929      *?   OUTPUT --> §PNCpsp - > % di scostamento                   ?*
044500060928      *---------------------------------------------------------------*
044600060928     c     Scostam       BEGSR
044700060928      *
044800060928      * (attuali - precedenti) = gap
044900060928      * (gap x 100) : attuali  = % scostamento
045000060928      *
045100060928      * NB: se l'attuale è uguale a zero, lo imposta a uno per non dare errore
045200060928if  1c                   if        §PNCatt  = *zeros
045300060928     c                   eval      §PNCatt  = 1
045400060928e   1C                   endif
045500060928      *
045600060928      * Calcola la differenza in termini non assoluti: percentuali
045700060928      *   (attuali - precedenti) * 100 / attuali
045800060928     c                   eval      §Gap  = ((§PNCatt - §PNCpre) * 100)
045900060928     c                                     / §PNCatt
046000060928      *
046100060928     c                   z-add     §Gap          §PNCpsp                        *% scostamento
046200060928      * Se la % scostamento è più di 3 cifre -> la sistema
046300060928sel 1c                   select
046400060928w   1c                   when      §Gap >= +1000
046500060928     c                   z-add     999           §PNCpsp
046600060928w   1c                   when      §Gap <= -1000
046700060928     c                   z-sub     999           §PNCpsp
046800060928e   1c                   endsl
046900060928      *
047000060928     c                   ENDSR
047100060929
047200060929      *---------------------------------------------------------------*
047300060929      *?Ciclo di elaborazione del singolo record dal file FNMSA10F   ?*
047400060929      *---------------------------------------------------------------*
047500060929     c     Elab_FNMSA_1  BEGSR
047600060929      *
047700060929      * Memorizza data + ora dell'ultimo minuto lavorato
047800080422     c                   movel     MSAdtl        wDatOraUlt                     *data+ora ultimo mm
047900080422     c                   move      MSAorl        wDatOraUlt
048000060929      *
048100060929      * Imposta la chiave dell'ultima lettura
048200080422     c                   eval      KSAapl  = MSAapl
048300080422     c                   eval      KSAfil  = MSAfil
048400080422     c                   eval      KSAdfv  = MSAdfv
048500080422     c                   eval      KSAnpg  = MSAnpg
048600060929      *
048700060929      * Imposta la chiave dell'ultima lettura per file saldi
048800060929     c                   eval      DEPdfv  = KSAdfv                             *data foglio
048900060929     c                   eval      DEPnpg  = KSAnpg                             *gipo foglio
049000060929      *
049100060929     c                   clear                   wDatOraPenult
049200060929     c                   clear                   wOraPenult
049300060929      *
049400060929      * Legge il penultimo minuto perchè è sicuramente "completo"
049500060929      * (sono i dati relativi a questo minuto che emette sul tabellone)
049600080422     c     K04MSA02      readPE    FNMSA000
049700060929if  1c                   if        %eof(FNMSA12L)
049800060929     c                   leavesr
049900060929e   1c                   endif
050000060929      *
050100060929      * Memorizza data + ora e ora del penultimo minuto lavorato
050200080422     c                   movel     MSAdtl        wDatOraPenult                  *data+ora penult mm
050300080422     c                   move      MSAorl        wDatOraPenult
050400080422     c                   eval      wOraPenult = MSAorl                          *ora ultimo mm
050500060929      *
050600060929      * Elabora giorno precedente per tabellone
050700060929     c                   exsr      ElabIeri_1
050800060929      *
050900060929      * Elabora oggi per tabellone e file saldi e scrive FLTR8
051000060929     c                   exsr      ElabOggi_1
051100060929      *
051200060929      * Scrive i saldi
051300060929     c                   exsr      Wrt_FNMSS_1
051400060929      *
051500060929     c                   ENDSR
051600060929
051700060929      *---------------------------------------------------------------*
051800060929      *?Elaboro dati del giorno precedente                           ?*
051900060929      *---------------------------------------------------------------*
052000060929     c     ElabIeri_1    BEGSR
052100060929      *
052200060929      * Memorizza la data foglio
052300060929     c                   eval      SAVdfv  = KSAdfv                             *data foglio preced.
052400060929      *
052500060929      * Calcola il giorno precedente utile alla data foglio trovata
052600060929     c                   reset                   WLBdat
052700060929     c                   eval      G08inv  = KSAdfv
052800060929     c                   call      'XSRDA8'
052900060929     c                   parm                    WLBdat
053000060929     c                   eval      wDFVgg  = G08tgi                             *gg data richiesta
053100060929      *
053200060929      * Ciclo per trovare il primo giorno precedente nel quale il
053300060929      *   macchinone ha lavorato
053400060929     c                   eval      xx      = 1
053500060929do  1c                   DOW       xx     <= 10
053600060929     c                   clear                   WGIdat
053700060929     c                   eval      GIOtgi  = wDFVgg - xx                        *gg data - "X" gg
053800060929     c                   call      'XSRGI8'
053900060929     c                   parm                    WGIdat
054000060929     c                   eval      wDFVpr  = GIOinv                             *data gg precedente
054100060929     c                   eval      KSAdfv  = wDFVpr                             *data foglio preced.
054200080422     c     K04MSA02      setll     FNMSA000
054300060929if  2c                   if        %equal(FNMSA12L)                             *trovati dati
054400060929     c                   leave                                                  *esce dal ciclo
054500060929e   2c                   endif
054600060929     c                   add       1             xx                             *incrementa gg meno
054700060929e   1c                   ENDDO
054800060929      *
054900060929      * Azzera i campi...
055000060929     c                   exsr      sr_AzzMSApr
055100060929      *
055200060929      * Legge tutti i dati della lavorazione -PRECEDENTE-
055300080422     c     K04MSA02      setll     FNMSA000
055400080422     c     K04MSA02      reade     FNMSA000
055500060929do  1c                   DOW       NOT  %eof(FNMSA12L)
055600080422if  2c                   if        MSAorl <= wOraPenult                         *fino all'ultimo mm
055700060929     c                   exsr      sr_MemMSApr_1
055800060929e   2c                   endif
055900080422     c     K04MSA02      reade     FNMSA000
056000060929e   1c                   ENDDO
056100060929      *
056200060929      * Repimposta la data del foglio "originale"
056300060929     c                   eval      KSAdfv  = SAVdfv                             *Data foglio preced.
056400060929      *
056500060929      * Reperisce la data + ora corrente
056600060929     c                   time                    n14                            *Ora (6)+ Data (8)
056700060929     c                   movel     n14           wOraCor                        *Ora (4)
056800060929     c                   clear                   WLBdat
056900060929     c                   move      n14           G08dat                         *Data (8) in g/m/a
057000060929     c                   eval      G08err  = *off
057100060929     c                   call      'XSRDA8'
057200060929     c                   parm                    WLBdat
057300060929     c                   eval      wDatCor = G08inv                             *Data corrente
057400060929      *
057500060929      * Sottrae 10 mm dalla data + ora corrente
057600061002     c                   eval      w0080   = wDatCor
057700061002     c                   eval      w0040   = wOraCor
057800060929     c                   exsr      sr_SubMin
057900061002     c                   movel     w0080         wDatOraCor
058000061002     c                   move      w0040         wDatOraCor
058100060929      *
058200060929     c                   ENDSR
058300060929
058400060929      *---------------------------------------------------------------*
058500060929      *?Elaboro dati del giorno corrente                             ?*
058600060929      *---------------------------------------------------------------*
058700060929     c     ElabOggi_1    BEGSR
058800060929      *
058900060929      * Azzera i campi...
059000060929     c                   exsr      sr_AzzMSA
059100060929      *
059200060929      * Legge tutti i dati della lavorazione -CORRENTE-
059300080422     c     K04MSA02      setll     FNMSA000                                     *ultimo
059400080422     c     K04MSA02      reade     FNMSA000
059500060929      *
059600060929do  1c                   DOW       NOT  %eof(FNMSA12L)
059700060929      *
059800080422     c                   movel     MSAdtl        wDatOraLav                     *data+ora
059900080422     c                   move      MSAorl        wDatOraLav
060000060929if  2c                   if        wDatOraLav <= wDatOraUlt                     *fino all'ultimo mm
060100060929     c                   exsr      sr_MemMSA_1
060200060929e   2c                   endif
060300060929      *
060400060929      * Quando arriva al penultimo minuto:
060500060929      *   scrive il file per il tabellone solo se tipo applicazione
060600160707      *   letto corrisponde a quello passato a pgm ("V" = VDL)
060700070222if  2c                   if             wTabApl    = PARapl
060800070222     c                             and  wDatOraLav = wDatOraPenult              *fino all'ultimo mm
060900070222     c                   exsr      Wrt_FLTR8_1
061000070222e   2c                   endif
061100060929      *
061200080422     c     K04MSA02      reade     FNMSA000
061300060929      *
061400060929e   1c                   ENDDO
061500060929      *
061600060929     c                   ENDSR
061700060929
061800060929      *---------------------------------------------------------------*
061900060929      *?Memorizzazione dei dati del file -PRECEDENTE-                ?*
062000060929      *---------------------------------------------------------------*
062100060929     c     sr_MemMSApr_1 BEGSR
062200060929      *
062300060929      * Colli lavorati TOTALI  (compresa baia 20!)
062400060929     c                   eval      mPNCpr   = mPNCpr
062500080422     c                                      + MSApep001
062600080422     c                                      + MSApep002
062700080422     c                                      + MSApep003
062800080422     c                                      + MSApep004
062900080422     c                                      + MSApep005
063000080422     c                                      + MSApep006
063100080422     c                                      + MSApep007
063200080422     c                                      + MSApep008
063300080422     c                                      + MSApep009
063400080422     c                                      + MSApep010
063500080422     c                                      + MSApep011
063600080422     c                                      + MSApep012
063700080422     c                                      + MSApep013
063800080422     c                                      + MSApep014
063900080422     c                                      + MSApep015
064000080422     c                                      + MSApep016
064100080422     c                                      + MSApep017
064200080422     c                                      + MSApep018
064300080422     c                                      + MSApep019
064400080422     c                                      + MSApep020
064500060929      *
064600060929     c                   ENDSR
064700060929
064800060929      *---------------------------------------------------------------*
064900060929      *?Memorizzazione dei dati del file -CORRENTE-                  ?*
065000060929      *---------------------------------------------------------------*
065100060929     c     sr_MemMSA_1   BEGSR
065200060929      *
065300060929      * Prima data + ora lettura presente
065400060929if  1c                   if        $First   = *on                               *SI prima lettura
065500080422     c                   eval      DEPdtlin = MSAdtl                            *data inizio
065600080422     c                   eval      DEPorlin = MSAorl                            *ora  inizio
065700060929     c                   eval      $First   = *off                              *NO prima lettura
065800060929e   1c                   endif
065900060929      *
066000060929      * Ultima data + ora lettura presente
066100080422     c                   eval      DEPdtlfi = MSAdtl                            *data fine
066200080422     c                   eval      DEPorlfi = MSAorl                            *ora  fine
066300060929      *
066400060929      * Colli ULTIMO MINUTO per tabellone
066500060929     c*** superfluo:     clear                   $MSApep
066600080422     c                   eval      $MSApep( 1) = MSApep001
066700080422     c                   eval      $MSApep( 2) = MSApep002
066800080422     c                   eval      $MSApep( 3) = MSApep003
066900080422     c                   eval      $MSApep( 4) = MSApep004
067000080422     c                   eval      $MSApep( 5) = MSApep005
067100080422     c                   eval      $MSApep( 6) = MSApep006
067200080422     c                   eval      $MSApep( 7) = MSApep007
067300080422     c                   eval      $MSApep( 8) = MSApep008
067400080422     c                   eval      $MSApep( 9) = MSApep009
067500080422     c                   eval      $MSApep(10) = MSApep010
067600080422     c                   eval      $MSApep(11) = MSApep011
067700080422     c                   eval      $MSApep(12) = MSApep012
067800080422     c                   eval      $MSApep(13) = MSApep013
067900080422     c                   eval      $MSApep(14) = MSApep014
068000080422     c                   eval      $MSApep(15) = MSApep015
068100080422     c                   eval      $MSApep(16) = MSApep016
068200080422     c                   eval      $MSApep(17) = MSApep017
068300080422     c                   eval      $MSApep(18) = MSApep018
068400080422     c                   eval      $MSApep(19) = MSApep019
068500080422     c                   eval      $MSApep(20) = MSApep020
068600060929      *
068700060929      * Colli e minuti lavorati TOTALI
068800060929      * - progressivo colli  (compresa baia 20!)
068900060929     c                   clear                   xx
069000060929do  1c                   dow       xx       < %elem($MSApep)
069100060929     c                   add       1             xx
069200060929     c                   add       $MSApep(xx)   mPNC
069300060929e   1c                   enddo
069400060929      * - Progressivo minuti
069500060929     c                   add       1             mMLAV
069600060929      *
069700060929      * Colli e minuti lavorati ENTRATE
069800060929     c                   clear                   xx
069900060929     c                   DOW       xx       < %elem($MSApep)
070000060929     c                   add       1             xx
070100060929if  1c                   if        $MSApep(xx) > *zeros
070200060929     c                   add       1             $MXX(xx)                       *mm entrata
070300060929     c                   add       $MSApep(xx)   $EXX(xx)                       *progressivo  ent
070400080422     c                   eval      $DTL(xx) = MSAdtl                            *data lettura ent
070500080422     c                   eval      $ORL(xx) = MSAorl                            *ora  lettura ent
070600060929e   1c                   endif
070700060929     c                   ENDDO
070800060929      *
070900060929      * Colli USCITE
071000060929     c*** superfluo:     clear                   $MSApad
071100080422     c                   eval      $MSApad( 1) = MSApad001
071200080422     c                   eval      $MSApad( 2) = MSApad002
071300080422     c                   eval      $MSApad( 3) = MSApad003
071400080422     c                   eval      $MSApad( 4) = MSApad004
071500080422     c                   eval      $MSApad( 5) = MSApad005
071600080422     c                   eval      $MSApad( 6) = MSApad006
071700080422     c                   eval      $MSApad( 7) = MSApad007
071800080422     c                   eval      $MSApad( 8) = MSApad008
071900080422     c                   eval      $MSApad( 9) = MSApad009
072000080422     c                   eval      $MSApad(10) = MSApad010
072100080422     c                   eval      $MSApad(11) = MSApad011
072200080422     c                   eval      $MSApad(12) = MSApad012
072300080422     c                   eval      $MSApad(13) = MSApad013
072400080422     c                   eval      $MSApad(14) = MSApad014
072500080422     c                   eval      $MSApad(15) = MSApad015
072600080422     c                   eval      $MSApad(16) = MSApad016
072700080422     c                   eval      $MSApad(17) = MSApad017
072800080422     c                   eval      $MSApad(18) = MSApad018
072900080422     c                   eval      $MSApad(19) = MSApad019
073000080422     c                   eval      $MSApad(20) = MSApad020
073100080422     c                   eval      $MSApad(21) = MSApad021
073200080422     c                   eval      $MSApad(22) = MSApad022
073300080422     c                   eval      $MSApad(23) = MSApad023
073400080422     c                   eval      $MSApad(24) = MSApad024
073500080422     c                   eval      $MSApad(25) = MSApad025
073600080422     c                   eval      $MSApad(26) = MSApad026
073700080422     c                   eval      $MSApad(27) = MSApad027
073800080422     c                   eval      $MSApad(28) = MSApad028
073900080422     c                   eval      $MSApad(29) = MSApad029
074000080422     c                   eval      $MSApad(30) = MSApad030
074100080422     c                   eval      $MSApad(31) = MSApad031
074200080422     c                   eval      $MSApad(32) = MSApad032
074300080422     c                   eval      $MSApad(33) = MSApad033
074400080422     c                   eval      $MSApad(34) = MSApad034
074500080422     c                   eval      $MSApad(35) = MSApad035
074600080422     c                   eval      $MSApad(36) = MSApad036
074700080422     c                   eval      $MSApad(37) = MSApad037
074800080422     c                   eval      $MSApad(38) = MSApad038
074900080422     c                   eval      $MSApad(39) = MSApad039
075000080422     c                   eval      $MSApad(40) = MSApad040
075100080422     c                   eval      $MSApad(41) = MSApad041
075200080422     c                   eval      $MSApad(42) = MSApad042
075300080422     c                   eval      $MSApad(43) = MSApad043
075400080422     c                   eval      $MSApad(44) = MSApad044
075500080422     c                   eval      $MSApad(45) = MSApad045
075600080422     c                   eval      $MSApad(46) = MSApad046
075700080422     c                   eval      $MSApad(47) = MSApad047
075800080422     c                   eval      $MSApad(48) = MSApad048
075900080422     c                   eval      $MSApad(49) = MSApad049
076000080422     c                   eval      $MSApad(50) = MSApad050
076100080422     c                   eval      $MSApad(51) = MSApad051
076200080422     c                   eval      $MSApad(52) = MSApad052
076300080422     c                   eval      $MSApad(53) = MSApad053
076400080422     c                   eval      $MSApad(54) = MSApad054
076500080422     c                   eval      $MSApad(55) = MSApad055
076600080422     c                   eval      $MSApad(56) = MSApad056
076700080422     c                   eval      $MSApad(57) = MSApad057
076800080422     c                   eval      $MSApad(58) = MSApad058
076900080422     c                   eval      $MSApad(59) = MSApad059
077000080422     c                   eval      $MSApad(60) = MSApad060
077100060929     c                   add       $MSApad       $UXX                           *progr.colli x usc.
077200060929      *
077300060929      * Dati statistici
077400080422     c                   add       MSApesind     mPesInd                        *caricati
077500080422     c                   add       MSApussor     mPusSor                        *smistati
077600080422     c                   add       MSAps2dok     mPs2Dok                        *letti da pistola
077700080422     c                   add       MSAcsd        mCSD                           *letti da scanner
077800080422     c                   add       MSAccv        mCCV                           *letti con volume
077900080422     c                   add       MSAccp        mCCP                           *letti con peso
078000080422     c                   add       MSAnls        mNLS                           *non letti scanner
078100080422     c                   add       MSAnlp        mNLP                           *non letti pistola
078200060929      * C: non misurati
078300060929     c                   eval      mXnoMis  = mXnoMis                           *C: non misurati
078400080422     c                                      + (MSApussor - MSAccv)
078500060929      * C: non pesati
078600060929     c                   eval      mXnoPes  = mXnoPes                           *C: non pesati
078700080422     c                                      + (MSApussor - MSAccp)
078800060929      * - OF x uscita piena
078900080422     c                   add       MSAodsdna     mOdsDna                        · TR4ods = '3'
079000060929      * - OF x errori macchina
079100060929     c                   eval      mErrMac  = mErrMac
079200080422     c                                      + MSAodsdnr                         + TR4ods = '2'
079300080422     c                                      + MSAodsftd                         + TR4ods = '4'
079400080422     c                                      + MSAodsids                         + TR4ods = '5'
079500080422     c                                      + MSAodsuds                         + TR4ods = '6'
079600080422     c                                      + MSAodsdne                         + TR4ods = '7'
079700080422     c                                      + MSAodsciv                         + TR4ods = '8'
079800080422     c                                      + MSAodsdnq                         + TR4ods = '9'
079900080422     c*** NO "unknown state":               + MSAodsust                         + TR4ods = '0'
080000060929      *
080100060929     c                   ENDSR
080200060929
080300060929      *---------------------------------------------------------------*
080400060929      *?Scrittura del file per le statistiche                        ?*
080500060929      *---------------------------------------------------------------*
080600060929     c     Wrt_FNMSS_1   BEGSR
080700060929      *
080800080422     c                   clear                   FNMSS000
080900060929      *
081000060929      * Reperisce il record da aggiornare
081100080422     c                   eval      KSSapl   = MSAapl                            *tipo applicazione
081200060929     c                   eval      KSSfil   = PARfil                            *p.o.
081300060929     c                   eval      KSSdfv   = DEPdfv                            *data foglio
081400060929     c                   eval      KSSnpg   = DEPnpg                            *categoria foglio
081500080422     c     K04MSS01      chain     FNMSS000
081600060929      *
081700060929      * Imposta i campi
081800080422     c                   eval      MSSpnc   = mPNC                              *progressivo colli
081900080422     c                   eval      MSStlm   = mMLAV                             *mm lavorati
082000060929      * - colli per entrata
082100080422     c                   eval      MSSe01   = $EXX( 1)
082200080422     c                   eval      MSSe02   = $EXX( 2)
082300080422     c                   eval      MSSe03   = $EXX( 3)
082400080422     c                   eval      MSSe04   = $EXX( 4)
082500080422     c                   eval      MSSe05   = $EXX( 5)
082600080422     c                   eval      MSSe06   = $EXX( 6)
082700080422     c                   eval      MSSe07   = $EXX( 7)
082800080422     c                   eval      MSSe08   = $EXX( 8)
082900080422     c                   eval      MSSe09   = $EXX( 9)
083000080422     c                   eval      MSSe10   = $EXX(10)
083100080422     c                   eval      MSSe11   = $EXX(11)
083200080422     c                   eval      MSSe12   = $EXX(12)
083300080422     c                   eval      MSSe13   = $EXX(13)
083400080422     c                   eval      MSSe14   = $EXX(14)
083500080422     c                   eval      MSSe15   = $EXX(15)
083600080422     c                   eval      MSSe16   = $EXX(16)
083700080422     c                   eval      MSSe17   = $EXX(17)
083800080422     c                   eval      MSSe18   = $EXX(18)
083900080422     c                   eval      MSSe19   = $EXX(19)
084000080422     c                   eval      MSSe20   = $EXX(20)
084100060929      * - minuti per entrata
084200080422     c                   eval      MSSm01   = $MXX( 1)
084300080422     c                   eval      MSSm02   = $MXX( 2)
084400080422     c                   eval      MSSm03   = $MXX( 3)
084500080422     c                   eval      MSSm04   = $MXX( 4)
084600080422     c                   eval      MSSm05   = $MXX( 5)
084700080422     c                   eval      MSSm06   = $MXX( 6)
084800080422     c                   eval      MSSm07   = $MXX( 7)
084900080422     c                   eval      MSSm08   = $MXX( 8)
085000080422     c                   eval      MSSm09   = $MXX( 9)
085100080422     c                   eval      MSSm10   = $MXX(10)
085200080422     c                   eval      MSSm11   = $MXX(11)
085300080422     c                   eval      MSSm12   = $MXX(12)
085400080422     c                   eval      MSSm13   = $MXX(13)
085500080422     c                   eval      MSSm14   = $MXX(14)
085600080422     c                   eval      MSSm15   = $MXX(15)
085700080422     c                   eval      MSSm16   = $MXX(16)
085800080422     c                   eval      MSSm17   = $MXX(17)
085900080422     c                   eval      MSSm18   = $MXX(18)
086000080422     c                   eval      MSSm19   = $MXX(19)
086100080422     c                   eval      MSSm20   = $MXX(20)
086200060929      * Colli per uscita
086300080422     c                   eval      MSSu01   = $UXX( 1)
086400080422     c                   eval      MSSu02   = $UXX( 2)
086500080422     c                   eval      MSSu03   = $UXX( 3)
086600080422     c                   eval      MSSu04   = $UXX( 4)
086700080422     c                   eval      MSSu05   = $UXX( 5)
086800080422     c                   eval      MSSu06   = $UXX( 6)
086900080422     c                   eval      MSSu07   = $UXX( 7)
087000080422     c                   eval      MSSu08   = $UXX( 8)
087100080422     c                   eval      MSSu09   = $UXX( 9)
087200080422     c                   eval      MSSu10   = $UXX(10)
087300080422     c                   eval      MSSu11   = $UXX(11)
087400080422     c                   eval      MSSu12   = $UXX(12)
087500080422     c                   eval      MSSu13   = $UXX(13)
087600080422     c                   eval      MSSu14   = $UXX(14)
087700080422     c                   eval      MSSu15   = $UXX(15)
087800080422     c                   eval      MSSu16   = $UXX(16)
087900080422     c                   eval      MSSu17   = $UXX(17)
088000080422     c                   eval      MSSu18   = $UXX(18)
088100080422     c                   eval      MSSu19   = $UXX(19)
088200080422     c                   eval      MSSu20   = $UXX(20)
088300080422     c                   eval      MSSu21   = $UXX(21)
088400080422     c                   eval      MSSu22   = $UXX(22)
088500080422     c                   eval      MSSu23   = $UXX(23)
088600080422     c                   eval      MSSu24   = $UXX(24)
088700080422     c                   eval      MSSu25   = $UXX(25)
088800080422     c                   eval      MSSu26   = $UXX(26)
088900080422     c                   eval      MSSu27   = $UXX(27)
089000080422     c                   eval      MSSu28   = $UXX(28)
089100080422     c                   eval      MSSu29   = $UXX(29)
089200080422     c                   eval      MSSu30   = $UXX(30)
089300080422     c                   eval      MSSu31   = $UXX(31)
089400080422     c                   eval      MSSu32   = $UXX(32)
089500080422     c                   eval      MSSu33   = $UXX(33)
089600080422     c                   eval      MSSu34   = $UXX(34)
089700080422     c                   eval      MSSu35   = $UXX(35)
089800080422     c                   eval      MSSu36   = $UXX(36)
089900080422     c                   eval      MSSu37   = $UXX(37)
090000080422     c                   eval      MSSu38   = $UXX(38)
090100080422     c                   eval      MSSu39   = $UXX(39)
090200080422     c                   eval      MSSu40   = $UXX(40)
090300080422     c                   eval      MSSu41   = $UXX(41)
090400080422     c                   eval      MSSu42   = $UXX(42)
090500080422     c                   eval      MSSu43   = $UXX(43)
090600080422     c                   eval      MSSu44   = $UXX(44)
090700080422     c                   eval      MSSu45   = $UXX(45)
090800080422     c                   eval      MSSu46   = $UXX(46)
090900080422     c                   eval      MSSu47   = $UXX(47)
091000080422     c                   eval      MSSu48   = $UXX(48)
091100080422     c                   eval      MSSu49   = $UXX(49)
091200080422     c                   eval      MSSu50   = $UXX(50)
091300080422     c                   eval      MSSu51   = $UXX(51)
091400080422     c                   eval      MSSu52   = $UXX(52)
091500080422     c                   eval      MSSu53   = $UXX(53)
091600080422     c                   eval      MSSu54   = $UXX(54)
091700080422     c                   eval      MSSu55   = $UXX(55)
091800080422     c                   eval      MSSu56   = $UXX(56)
091900080422     c                   eval      MSSu57   = $UXX(57)
092000080422     c                   eval      MSSu58   = $UXX(58)
092100080422     c                   eval      MSSu59   = $UXX(59)
092200080422     c                   eval      MSSu60   = $UXX(60)
092300060929      *
092400080422     c                   eval      MSScla   = mPesInd                           *caricati da
092500080422     c                   eval      MSSsor   = mPusSor                           *smistati da
092600080422     c                   eval      MSSdna   = mOdsDna                           *of x uscita piena
092700080422     c                   eval      MSSerr   = mErrMac                           *of x errori macchin
092800080422     c                   eval      MSSps2   = mPs2Dok                           *letti da pistaola
092900080422     c                   eval      MSScsd   = mCSD                              *letti da scanner
093000080422     c                   eval      MSSccv   = mCCV                              *letti con volume
093100080422     c                   eval      MSSccp   = mCCP                              *letti con peso
093200080422     c                   eval      MSSnlp   = mNLP                              *non letti pistola
093300080422     c                   eval      MSSnls   = mNLS                              *non letti scanner
093400080422     c                   eval      MSSnpe   = mXnoPes                           *C:non pesati
093500080422     c                   eval      MSSnmi   = mXnoMis                           *C:non misurati
093600060929      *
093700060929      * Scrive
093800060929if  1c                   if        NOT  %found(FNMSS11L)                        *inesistente
093900080422     c                   eval      MSSapl   = KSSapl                            *tipo applicazione
094000080422     c                   eval      MSSfil   = KSSfil                            *p.o.
094100080422     c                   eval      MSSdfv   = KSSdfv                            *data foglio
094200080422     c                   eval      MSSnpg   = KSSnpg                            *categoria foglio
094300060929      *                  __________________
094400080422     c                   WRITE     FNMSS000                                     *INSERIMENTO
094500060929      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
094600060929x   1c                   else                                                   *esistente
094700060929      *                  __________________
094800080422     c                   UPDATE    FNMSS000                                     *MODIFICA
094900060929      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
095000060929e   1c                   endif
095100060929      *
095200060929     c                   ENDSR
095300161214
095400161214      *---------------------------------------------------------------*
095500161214      *?Ciclo di elaborazione del singolo record dal file FNMSA00F   ?*
095600161214      *---------------------------------------------------------------*
095700161214     c     Elab_FNMSA    BEGSR
095800161214      *
095900161214      * Memorizza data + ora dell'ultimo minuto lavorato
096000161214     c                   movel     SA0dtl        wDatOraUlt                     *data+ora ultimo mm
096100161214     c                   move      SA0orl        wDatOraUlt
096200161214      *
096300161214      * Imposta la chiave dell'ultima lettura
096400161214     c*//·               eval      KSAimp  = SA0imp
096500161214     c                   eval      KSAapl  = SA0apl
096600161214     c                   eval      KSAfil  = SA0fil
096700161214     c                   eval      KSAdfv  = SA0dfv
096800161214     c                   eval      KSAnpg  = SA0npg
096900161214     c*//·               eval      KSAlav  = SA0lav
097000161214      *
097100161214      * Imposta la chiave dell'ultima lettura per file saldi
097200161214     c*//·               eval      DEPimp  = KSAimp                             *tipo impianto
097300161214     c*//·               eval      DEPlav  = KSAlav                             *tipo lavorazione
097400161214     c                   eval      DEPdfv  = KSAdfv                             *data foglio
097500161214     c                   eval      DEPnpg  = KSAnpg                             *categoria foglio
097600161214      *
097700161214     c                   clear                   wDatOraPenult
097800161214     c                   clear                   wOraPenult
097900161214      *
098000161214      * Legge il penultimo minuto perchè è sicuramente "completo"
098100161214      * (sono i dati relativi a questo minuto che emette sul tabellone)
098200161214     c     K04MSA02      readPE    FNMSA002
098300161214if  1c                   if        %eof(FNMSA02L)
098400161214     c                   leavesr
098500161214e   1c                   endif
098600161214      *
098700161214      * Memorizza data + ora e ora del penultimo minuto lavorato
098800161215     c                   movel     SA0dtl        wDatOraPenult                  *data+ora penult mm
098900161215     c                   move      SA0orl        wDatOraPenult
099000161215     c                   eval      wOraPenult = SA0orl                          *ora ultimo mm
099100161214      *
099200161214      * Elabora giorno precedente per tabellone
099300161214     c                   exsr      ElabIeri
099400161214      *
099500161214      * Elabora oggi per tabellone e file saldi e scrive FLTR8
099600161214     c                   exsr      ElabOggi
099700161214      *
099800161214      * Scrive i saldi
099900161214     c                   exsr      Wrt_FNMSS
100000161214      *
100100161214     c                   ENDSR
100200161214
100300161214      *---------------------------------------------------------------*
100400161214      *?Elaboro dati del giorno precedente                           ?*
100500161214      *---------------------------------------------------------------*
100600161214     c     ElabIeri      BEGSR
100700161214      *
100800161214      * Memorizza la data foglio
100900161214     c                   eval      SAVdfv  = KSAdfv                             *data foglio preced.
101000161214      *
101100161214      * Calcola il giorno precedente utile alla data foglio trovata
101200161214     c                   reset                   WLBdat
101300161214     c                   eval      G08inv  = KSAdfv
101400161214     c                   call      'XSRDA8'
101500161214     c                   parm                    WLBdat
101600161214     c                   eval      wDFVgg  = G08tgi                             *gg data richiesta
101700161214      *
101800161214      * Ciclo per trovare il primo giorno precedente nel quale il
101900161214      *   macchinone ha lavorato
102000161214     c                   eval      xx      = 1
102100161214do  1c                   DOW       xx     <= 10
102200161214     c                   clear                   WGIdat
102300161214     c                   eval      GIOtgi  = wDFVgg - xx                        *gg data - "X" gg
102400161214     c                   call      'XSRGI8'
102500161214     c                   parm                    WGIdat
102600161214     c                   eval      wDFVpr  = GIOinv                             *data gg precedente
102700161214     c                   eval      KSAdfv  = wDFVpr                             *data foglio preced.
102800161214     c     K04MSA02      setll     FNMSA002
102900161214if  2c                   if        %equal(FNMSA02L)                             *trovati dati
103000161214     c                   leave                                                  *esce dal ciclo
103100161214e   2c                   endif
103200161214     c                   add       1             xx                             *incrementa gg meno
103300161214e   1c                   ENDDO
103400161214      *
103500161214      * Azzera i campi...
103600161214     c                   exsr      sr_AzzMSApr
103700161214      *
103800161214      * Legge tutti i dati della lavorazione -PRECEDENTE-
103900161214     c     K04MSA02      setll     FNMSA002
104000161214     c     K04MSA02      reade     FNMSA002
104100161214do  1c                   DOW       NOT  %eof(FNMSA02L)
104200161215if  2c                   if        SA0orl <= wOraPenult                         *fino all'ultimo mm
104300161214     c                   exsr      sr_MemMSApr
104400161214e   2c                   endif
104500161214     c     K04MSA02      reade     FNMSA002
104600161214e   1c                   ENDDO
104700161214      *
104800161214      * Repimposta la data del foglio "originale"
104900161214     c                   eval      KSAdfv  = SAVdfv                             *Data foglio preced.
105000161214      *
105100161214      * Reperisce la data + ora corrente
105200161214     c                   time                    n14                            *Ora (6)+ Data (8)
105300161214     c                   movel     n14           wOraCor                        *Ora (4)
105400161214     c                   clear                   WLBdat
105500161214     c                   move      n14           G08dat                         *Data (8) in g/m/a
105600161214     c                   eval      G08err  = *off
105700161214     c                   call      'XSRDA8'
105800161214     c                   parm                    WLBdat
105900161214     c                   eval      wDatCor = G08inv                             *Data corrente
106000161214      *
106100161214      * Sottrae 10 mm dalla data + ora corrente
106200161214     c                   eval      w0080   = wDatCor
106300161214     c                   eval      w0040   = wOraCor
106400161214     c                   exsr      sr_SubMin
106500161214     c                   movel     w0080         wDatOraCor
106600161214     c                   move      w0040         wDatOraCor
106700161214      *
106800161214     c                   ENDSR
106900161214
107000161214      *---------------------------------------------------------------*
107100161214      *?Elaboro dati del giorno corrente                             ?*
107200161214      *---------------------------------------------------------------*
107300161214     c     ElabOggi      BEGSR
107400161214      *
107500161214      * Azzera i campi...
107600161214     c                   exsr      sr_AzzMSA
107700161214      *
107800161214      * Legge tutti i dati della lavorazione -CORRENTE-
107900161214     c     K04MSA02      setll     FNMSA002                                     *ultimo
108000161214     c     K04MSA02      reade     FNMSA002
108100161214      *
108200161214do  1c                   DOW       NOT  %eof(FNMSA02L)
108300161214      *
108400161214     c                   movel     SA0dtl        wDatOraLav                     *data+ora
108500161214     c                   move      SA0orl        wDatOraLav
108600161214if  2c                   if        wDatOraLav <= wDatOraUlt                     *fino all'ultimo mm
108700161214     c                   exsr      sr_MemMSA
108800161214e   2c                   endif
108900161214      *
109000161214      * Quando arriva al penultimo minuto:
109100161214      *   scrive il file per il tabellone solo se tipo applicazione
109200161214      *   letto corrisponde a quello passato a pgm ("V" = VDL)
109300161214      * -?FLTR8 è già stato aggiornato nella subr. "ElabOggi_1"?
109400161214if  2c*//                if             wTabApl    = PARapl
109500161214     c*//                          and  wDatOraLav = wDatOraPenult              *fino all'ultimo mm
109600161214     c*//                exsr      Wrt_FLTR8
109700161214e   2c*//                endif
109800161214      *
109900161214     c     K04MSA02      reade     FNMSA002
110000161214      *
110100161214e   1c                   ENDDO
110200161214      *
110300161214     c                   ENDSR
110400161214
110500161214      *---------------------------------------------------------------*
110600161214      *?Memorizzazione dei dati del file -PRECEDENTE-                ?*
110700161214      *---------------------------------------------------------------*
110800161214     c     sr_MemMSApr   BEGSR
110900161214      *
111000161214      * Colli lavorati TOTALI  (compresa baia 20!)
111100161214     c                   eval      mPNCpr  += ( SA0pep001 +
111200161214     c                                          SA0pep002 +
111300161214     c                                          SA0pep003 +
111400161214     c                                          SA0pep004 +
111500161214     c                                          SA0pep005 +
111600161214     c                                          SA0pep006 +
111700161214     c                                          SA0pep007 +
111800161214     c                                          SA0pep008 +
111900161214     c                                          SA0pep009 +
112000161214     c                                          SA0pep010 +
112100161214     c                                          SA0pep011 +
112200161214     c                                          SA0pep012 +
112300161214     c                                          SA0pep013 +
112400161214     c                                          SA0pep014 +
112500161214     c                                          SA0pep015 +
112600161214     c                                          SA0pep016 +
112700161214     c                                          SA0pep017 +
112800161214     c                                          SA0pep018 +
112900161214     c                                          SA0pep019 +
113000161214     c                                          SA0pep020 )
113100161214      *
113200161214     c                   ENDSR
113300161214
113400161214      *---------------------------------------------------------------*
113500161214      *?Memorizzazione dei dati del file -CORRENTE-                  ?*
113600161214      *---------------------------------------------------------------*
113700161214     c     sr_MemMSA     BEGSR
113800161214      *
113900161214      * · Prima data + ora lettura presente
114000161214if  1c                   if        $First   = *on                               *SI prima lettura
114100161214     c                   eval      DEPdtlin = SA0dtl                            *data inizio
114200161214     c                   eval      DEPorlin = SA0orl                            *ora  inizio
114300161214     c                   eval      $First   = *off                              *NO prima lettura
114400161214e   1c                   endif
114500161214      *
114600161214      * · Ultima data + ora lettura presente
114700161214     c                   eval      DEPdtlfi = SA0dtl                            *data fine
114800161214     c                   eval      DEPorlfi = SA0orl                            *ora  fine
114900161214      *
115000161214      * · Colli ULTIMO MINUTO per tabellone
115100161214     c*** superfluo:     clear                   $MSApep
115200161214     c                   eval      $MSApep( 1) = SA0pep001
115300161214     c                   eval      $MSApep( 2) = SA0pep002
115400161214     c                   eval      $MSApep( 3) = SA0pep003
115500161214     c                   eval      $MSApep( 4) = SA0pep004
115600161214     c                   eval      $MSApep( 5) = SA0pep005
115700161214     c                   eval      $MSApep( 6) = SA0pep006
115800161214     c                   eval      $MSApep( 7) = SA0pep007
115900161214     c                   eval      $MSApep( 8) = SA0pep008
116000161214     c                   eval      $MSApep( 9) = SA0pep009
116100161214     c                   eval      $MSApep(10) = SA0pep010
116200161214     c                   eval      $MSApep(11) = SA0pep011
116300161214     c                   eval      $MSApep(12) = SA0pep012
116400161214     c                   eval      $MSApep(13) = SA0pep013
116500161214     c                   eval      $MSApep(14) = SA0pep014
116600161214     c                   eval      $MSApep(15) = SA0pep015
116700161214     c                   eval      $MSApep(16) = SA0pep016
116800161214     c                   eval      $MSApep(17) = SA0pep017
116900161214     c                   eval      $MSApep(18) = SA0pep018
117000161214     c                   eval      $MSApep(19) = SA0pep019
117100161214     c                   eval      $MSApep(20) = SA0pep020
117200161214      *
117300161214      * · Colli e minuti lavorati TOTALI
117400161214      * · · progressivo colli  (compresa baia 20!)
117500161214     c                   clear                   xx
117600161214do  1c                   dow       xx       < %elem($MSApep)
117700161214     c                   add       1             xx
117800161214     c                   add       $MSApep(xx)   mPNC
117900161214e   1c                   enddo
118000161214      * · · progressivo minuti
118100161214     c                   add       1             mMLAV
118200161214      *
118300161214      * · Colli e minuti lavorati ENTRATE
118400161214     c                   clear                   xx
118500161214     c                   DOW       xx       < %elem($MSApep)
118600161214     c                   add       1             xx
118700161214if  1c                   if        $MSApep(xx) > *zeros
118800161214     c                   add       1             $MXX(xx)                       *mm entrata
118900161214     c                   add       $MSApep(xx)   $EXX(xx)                       *progressivo  ent
119000161214     c                   eval      $DTL(xx) = SA0dtl                            *data lettura ent
119100161214     c                   eval      $ORL(xx) = SA0orl                            *ora  lettura ent
119200161214e   1c                   endif
119300161214     c                   ENDDO
119400161214      *
119500161214      * · Colli USCITE
119600161214     c*** superfluo:     clear                   $MSApad
119700161214     c                   eval      $MSApad( 1) = SA0pad001
119800161214     c                   eval      $MSApad( 2) = SA0pad002
119900161214     c                   eval      $MSApad( 3) = SA0pad003
120000161214     c                   eval      $MSApad( 4) = SA0pad004
120100161214     c                   eval      $MSApad( 5) = SA0pad005
120200161214     c                   eval      $MSApad( 6) = SA0pad006
120300161214     c                   eval      $MSApad( 7) = SA0pad007
120400161214     c                   eval      $MSApad( 8) = SA0pad008
120500161214     c                   eval      $MSApad( 9) = SA0pad009
120600161214     c                   eval      $MSApad(10) = SA0pad010
120700161214     c                   eval      $MSApad(11) = SA0pad011
120800161214     c                   eval      $MSApad(12) = SA0pad012
120900161214     c                   eval      $MSApad(13) = SA0pad013
121000161214     c                   eval      $MSApad(14) = SA0pad014
121100161214     c                   eval      $MSApad(15) = SA0pad015
121200161214     c                   eval      $MSApad(16) = SA0pad016
121300161214     c                   eval      $MSApad(17) = SA0pad017
121400161214     c                   eval      $MSApad(18) = SA0pad018
121500161214     c                   eval      $MSApad(19) = SA0pad019
121600161214     c                   eval      $MSApad(20) = SA0pad020
121700161214     c                   eval      $MSApad(21) = SA0pad021
121800161214     c                   eval      $MSApad(22) = SA0pad022
121900161214     c                   eval      $MSApad(23) = SA0pad023
122000161214     c                   eval      $MSApad(24) = SA0pad024
122100161214     c                   eval      $MSApad(25) = SA0pad025
122200161214     c                   eval      $MSApad(26) = SA0pad026
122300161214     c                   eval      $MSApad(27) = SA0pad027
122400161214     c                   eval      $MSApad(28) = SA0pad028
122500161214     c                   eval      $MSApad(29) = SA0pad029
122600161214     c                   eval      $MSApad(30) = SA0pad030
122700161214     c                   eval      $MSApad(31) = SA0pad031
122800161214     c                   eval      $MSApad(32) = SA0pad032
122900161214     c                   eval      $MSApad(33) = SA0pad033
123000161214     c                   eval      $MSApad(34) = SA0pad034
123100161214     c                   eval      $MSApad(35) = SA0pad035
123200161214     c                   eval      $MSApad(36) = SA0pad036
123300161214     c                   eval      $MSApad(37) = SA0pad037
123400161214     c                   eval      $MSApad(38) = SA0pad038
123500161214     c                   eval      $MSApad(39) = SA0pad039
123600161214     c                   eval      $MSApad(40) = SA0pad040
123700161214     c                   eval      $MSApad(41) = SA0pad041
123800161214     c                   eval      $MSApad(42) = SA0pad042
123900161214     c                   eval      $MSApad(43) = SA0pad043
124000161214     c                   eval      $MSApad(44) = SA0pad044
124100161214     c                   eval      $MSApad(45) = SA0pad045
124200161214     c                   eval      $MSApad(46) = SA0pad046
124300161214     c                   eval      $MSApad(47) = SA0pad047
124400161214     c                   eval      $MSApad(48) = SA0pad048
124500161214     c                   eval      $MSApad(49) = SA0pad049
124600161214     c                   eval      $MSApad(50) = SA0pad050
124700161214     c                   eval      $MSApad(51) = SA0pad051
124800161214     c                   eval      $MSApad(52) = SA0pad052
124900161214     c                   eval      $MSApad(53) = SA0pad053
125000161214     c                   eval      $MSApad(54) = SA0pad054
125100161214     c                   eval      $MSApad(55) = SA0pad055
125200161214     c                   eval      $MSApad(56) = SA0pad056
125300161214     c                   eval      $MSApad(57) = SA0pad057
125400161214     c                   eval      $MSApad(58) = SA0pad058
125500161214     c                   eval      $MSApad(59) = SA0pad059
125600161214     c                   eval      $MSApad(60) = SA0pad060
125700161214     c                   add       $MSApad       $UXX                           *progr.colli x usc.
125800161214      *
125900161214      * · Dati statistici
126000161214     c                   add       SA0pesind     mPesInd                        *caricati
126100161214     c                   add       SA0pussor     mPusSor                        *smistati
126200161214     c                   add       SA0ps2dok     mPs2Dok                        *letti da pistola
126300161214     c                   add       SA0csd        mCSD                           *letti da scanner
126400161214     c                   add       SA0pwf        mPWF                           *letti da pist.Wi-Fi
126500161214     c                   add       SA0ccv        mCCV                           *letti con volume
126600161214     c                   add       SA0ccp        mCCP                           *letti con peso
126700161214     c                   add       SA0nls        mNLS                           *non letti scanner
126800161214     c                   add       SA0nlp        mNLP                           *non letti pistola
126900161214      * · C: non misurati
127000161214     c                   eval      mXnoMis += (SA0pussor - SA0ccv)              *C: non misurati
127100161214      * · C: non pesati
127200161214     c                   eval      mXnoPes += (SA0pussor - SA0ccp)              *C: non pesati
127300161214      * · OF x uscita piena
127400161214     c                   add       SA0odsdna     mOdsDna                        · TR4ods = '3'
127500161214      * · OF x errori macchina
127600161214     c                   eval      mErrMac += ( SA0odsdnr +                     + TR4ods = '2'
127700161214     c                                          SA0odsftd +                     + TR4ods = '4'
127800161214     c                                          SA0odsids +                     + TR4ods = '5'
127900161214     c                                          SA0odsuds +                     + TR4ods = '6'
128000161214     c                                          SA0odsdne +                     + TR4ods = '7'
128100161214     c                                          SA0odsciv +                     + TR4ods = '8'
128200161214     c                                          SA0odsdnq )                     + TR4ods = '9'
128300161214     c*** NO "unknown state":                 + SA0odsust                       + TR4ods = '0'
128400161214      *
128500161214     c                   ENDSR
128600161214
128700161214      *---------------------------------------------------------------*
128800161214      *?Scrittura del file per le statistiche                        ?*
128900161214      *---------------------------------------------------------------*
129000161214     c     Wrt_FNMSS     BEGSR
129100161214      *
129200161214     c                   clear                   FNMSS001
129300161214      *
129400161214      * Reperisce il record da aggiornare
129500161214     c*//·               eval      KSSimp   = DEPimp                            *tipo impianto
129600161214     c                   eval      KSSapl   = SA0apl                            *tipo applicazione
129700161214     c                   eval      KSSfil   = PARfil                            *p.o.
129800161214     c                   eval      KSSdfv   = DEPdfv                            *data foglio
129900161214     c                   eval      KSSnpg   = DEPnpg                            *categoria foglio
130000161214     c*//·               eval      KSSlav   = DEPlav                            *tipo lavorazione
130100161214     c     K04MSS01      chain     FNMSS001
130200161214      *
130300161214      * Imposta i campi
130400161214     c                   eval      SS0pnc   = mPNC                              *progressivo colli
130500161214     c                   eval      SS0tlm   = mMLAV                             *mm lavorati
130600161214      * - colli per entrata
130700161214     c                   eval      SS0e01   = $EXX( 1)
130800161214     c                   eval      SS0e02   = $EXX( 2)
130900161214     c                   eval      SS0e03   = $EXX( 3)
131000161214     c                   eval      SS0e04   = $EXX( 4)
131100161214     c                   eval      SS0e05   = $EXX( 5)
131200161214     c                   eval      SS0e06   = $EXX( 6)
131300161214     c                   eval      SS0e07   = $EXX( 7)
131400161214     c                   eval      SS0e08   = $EXX( 8)
131500161214     c                   eval      SS0e09   = $EXX( 9)
131600161214     c                   eval      SS0e10   = $EXX(10)
131700161214     c                   eval      SS0e11   = $EXX(11)
131800161214     c                   eval      SS0e12   = $EXX(12)
131900161214     c                   eval      SS0e13   = $EXX(13)
132000161214     c                   eval      SS0e14   = $EXX(14)
132100161214     c                   eval      SS0e15   = $EXX(15)
132200161214     c                   eval      SS0e16   = $EXX(16)
132300161214     c                   eval      SS0e17   = $EXX(17)
132400161214     c                   eval      SS0e18   = $EXX(18)
132500161214     c                   eval      SS0e19   = $EXX(19)
132600161214     c                   eval      SS0e20   = $EXX(20)
132700161214      * - minuti per entrata
132800161214     c                   eval      SS0m01   = $MXX( 1)
132900161214     c                   eval      SS0m02   = $MXX( 2)
133000161214     c                   eval      SS0m03   = $MXX( 3)
133100161214     c                   eval      SS0m04   = $MXX( 4)
133200161214     c                   eval      SS0m05   = $MXX( 5)
133300161214     c                   eval      SS0m06   = $MXX( 6)
133400161214     c                   eval      SS0m07   = $MXX( 7)
133500161214     c                   eval      SS0m08   = $MXX( 8)
133600161214     c                   eval      SS0m09   = $MXX( 9)
133700161214     c                   eval      SS0m10   = $MXX(10)
133800161214     c                   eval      SS0m11   = $MXX(11)
133900161214     c                   eval      SS0m12   = $MXX(12)
134000161214     c                   eval      SS0m13   = $MXX(13)
134100161214     c                   eval      SS0m14   = $MXX(14)
134200161214     c                   eval      SS0m15   = $MXX(15)
134300161214     c                   eval      SS0m16   = $MXX(16)
134400161214     c                   eval      SS0m17   = $MXX(17)
134500161214     c                   eval      SS0m18   = $MXX(18)
134600161214     c                   eval      SS0m19   = $MXX(19)
134700161214     c                   eval      SS0m20   = $MXX(20)
134800161214      * Colli per uscita
134900161214     c                   eval      SS0u01   = $UXX( 1)
135000161214     c                   eval      SS0u02   = $UXX( 2)
135100161214     c                   eval      SS0u03   = $UXX( 3)
135200161214     c                   eval      SS0u04   = $UXX( 4)
135300161214     c                   eval      SS0u05   = $UXX( 5)
135400161214     c                   eval      SS0u06   = $UXX( 6)
135500161214     c                   eval      SS0u07   = $UXX( 7)
135600161214     c                   eval      SS0u08   = $UXX( 8)
135700161214     c                   eval      SS0u09   = $UXX( 9)
135800161214     c                   eval      SS0u10   = $UXX(10)
135900161214     c                   eval      SS0u11   = $UXX(11)
136000161214     c                   eval      SS0u12   = $UXX(12)
136100161214     c                   eval      SS0u13   = $UXX(13)
136200161214     c                   eval      SS0u14   = $UXX(14)
136300161214     c                   eval      SS0u15   = $UXX(15)
136400161214     c                   eval      SS0u16   = $UXX(16)
136500161214     c                   eval      SS0u17   = $UXX(17)
136600161214     c                   eval      SS0u18   = $UXX(18)
136700161214     c                   eval      SS0u19   = $UXX(19)
136800161214     c                   eval      SS0u20   = $UXX(20)
136900161214     c                   eval      SS0u21   = $UXX(21)
137000161214     c                   eval      SS0u22   = $UXX(22)
137100161214     c                   eval      SS0u23   = $UXX(23)
137200161214     c                   eval      SS0u24   = $UXX(24)
137300161214     c                   eval      SS0u25   = $UXX(25)
137400161214     c                   eval      SS0u26   = $UXX(26)
137500161214     c                   eval      SS0u27   = $UXX(27)
137600161214     c                   eval      SS0u28   = $UXX(28)
137700161214     c                   eval      SS0u29   = $UXX(29)
137800161214     c                   eval      SS0u30   = $UXX(30)
137900161214     c                   eval      SS0u31   = $UXX(31)
138000161214     c                   eval      SS0u32   = $UXX(32)
138100161214     c                   eval      SS0u33   = $UXX(33)
138200161214     c                   eval      SS0u34   = $UXX(34)
138300161214     c                   eval      SS0u35   = $UXX(35)
138400161214     c                   eval      SS0u36   = $UXX(36)
138500161214     c                   eval      SS0u37   = $UXX(37)
138600161214     c                   eval      SS0u38   = $UXX(38)
138700161214     c                   eval      SS0u39   = $UXX(39)
138800161214     c                   eval      SS0u40   = $UXX(40)
138900161214     c                   eval      SS0u41   = $UXX(41)
139000161214     c                   eval      SS0u42   = $UXX(42)
139100161214     c                   eval      SS0u43   = $UXX(43)
139200161214     c                   eval      SS0u44   = $UXX(44)
139300161214     c                   eval      SS0u45   = $UXX(45)
139400161214     c                   eval      SS0u46   = $UXX(46)
139500161214     c                   eval      SS0u47   = $UXX(47)
139600161214     c                   eval      SS0u48   = $UXX(48)
139700161214     c                   eval      SS0u49   = $UXX(49)
139800161214     c                   eval      SS0u50   = $UXX(50)
139900161214     c                   eval      SS0u51   = $UXX(51)
140000161214     c                   eval      SS0u52   = $UXX(52)
140100161214     c                   eval      SS0u53   = $UXX(53)
140200161214     c                   eval      SS0u54   = $UXX(54)
140300161214     c                   eval      SS0u55   = $UXX(55)
140400161214     c                   eval      SS0u56   = $UXX(56)
140500161214     c                   eval      SS0u57   = $UXX(57)
140600161214     c                   eval      SS0u58   = $UXX(58)
140700161214     c                   eval      SS0u59   = $UXX(59)
140800161214     c                   eval      SS0u60   = $UXX(60)
140900161214      *
141000161214     c                   eval      SS0cla   = mPesInd                           *caricati da
141100161214     c                   eval      SS0sor   = mPusSor                           *smistati da
141200161214     c                   eval      SS0dna   = mOdsDna                           *of x uscita piena
141300161214     c                   eval      SS0err   = mErrMac                           *of x errori macchin
141400161214     c                   eval      SS0ps2   = mPs2Dok                           *letti da pistola
141500161214     c                   eval      SS0csd   = mCSD                              *letti da scanner
141600161214     c                   eval      SS0ccv   = mCCV                              *letti con volume
141700161214     c                   eval      SS0ccp   = mCCP                              *letti con peso
141800161214     c                   eval      SS0nlp   = mNLP                              *non letti pistola
141900161214     c                   eval      SS0nls   = mNLS                              *non letti scanner
142000161214     c                   eval      SS0npe   = mXnoPes                           *C:non pesati
142100161214     c                   eval      SS0nmi   = mXnoMis                           *C:non misurati
142200161214     c                   eval      SS0pwf   = mPWF                              *letti da pist.Wi-Fi
142300161214      *
142400161214      * Scrive
142500161214if  1c                   if        NOT  %found(FNMSS01L)                        *inesistente
142600161214     c*//·               eval      SS0apl   = KSSimp                            *tipo applicazione
142700161214     c                   eval      SS0apl   = KSSapl                            *tipo applicazione
142800161214     c                   eval      SS0fil   = KSSfil                            *filiale
142900161214     c                   eval      SS0dfv   = KSSdfv                            *data foglio
143000161214     c                   eval      SS0npg   = KSSnpg                            *categoria foglio
143100161214     c*//·               eval      SS0npg   = DEPnpg                            *categoria foglio
143200161214     c*//·               eval      SS0imp   = KSSimp                            *tipo impianto
143300161214     c*//·               eval      SS0lav   = KSSlav                            *tipo lavorazione
143400161214      *                  __________________
143500161214     c                   WRITE     FNMSS001                                     *INSERIMENTO
143600161214      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
143700161214x   1c                   else                                                   *esistente
143800161214      *                  __________________
143900161214     c                   UPDATE    FNMSS001                                     *MODIFICA
144000161214      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
144100161214e   1c                   endif
144200161214      *
144300161214     c                   ENDSR
144400070222
144500070222      *---------------------------------------------------------------*
144600070222      *?Scrittura del file per il tabellone                          ?*
144700070222      *---------------------------------------------------------------*
144800070222     c     Wrt_FLTR8_1   BEGSR
144900070222      *
145000070222      * Imposta i colli lavorati nel minuto
145100070222     c                   z-add     $MSApep       $TR8M
145200070222      * - Totale dei colli lavorati nel minuto
145300070222      *   (compresa baia 20!)
145400070222     c                   clear                   mNCL
145500070222     c                   clear                   xx
145600070222do  1c                   dow       xx       < %elem($MSApep)
145700070222     c                   add       1             xx
145800070222     c                   add       $MSApep(xx)   mNCL
145900070222e   1c                   enddo
146000070222      *
146100070222      * Calcola la % di scostamento sulla lavorazione precedente del
146200070222      *   progressivo colli
146300070222if  1c                   if             mPNC     > *zeros
146400070222     c                             or   mPNCpr   > *zeros
146500070222     c                   z-add     mPNC          §PNCatt                        *prg colli attuali
146600070222     c                   z-add     mPNCpr        §PNCpre                        *prg colli preced
146700070222     c                   z-add     *zeros        §PNCpsp                        *% scostamento
146800070222     c                   exsr      scostam                                      *CALCOLA PERCENTUALE
146900070222     c                   z-add     §PNCpsp       mPSP                           *% scostamento
147000070222e   1c                   endif
147100070222      *
147200070222      * Aggancia il record da aggiornare
147300070222     c                   clear                   FLTR8000                       *tabellone
147400070222     c                   movel     PARfil        KR8fil                         *filiale
147500070222     c                   clear                   KR8tip                         *tipo standard
147600070222     c     K02TR801      chain     FLTR8000
147700070222      *
147800070222      * Imposta i campi
147900160707if  1c                   if        NOT  %found(FLTR801L)                        *inesistente
148000160707     c                   clear                   TR8orl
148100160707     c                   clear                   TR8pnc
148200160707     c                   endif
148300160707     c                   if        TR8orl  <> %editc( DEPorlfi : 'X' )  or
148400160707     c                             TR8pnc  <> %subst(
148500160707     c                                        %editc( mPNC : 'X' ) : 2 )
148600160707     c                   eval      TR8time  = %timestamp()                      *data/ora ins./mod.
148700160707     c                   endif
148800160707      *
148900070222     c                   move      DEPorlfi      TR8orl                         *ora lettura
149000070222     c                   move      mPNC          TR8pnc                         *progressivo colli
149100070222      *
149200070222     c                   eval      w0030    = %abs(mPSP)                        *v.assoluto %
149300070222     c                   movel     w0030         w003a
149400070222if  1c                   if        mPSP    >= 0                                 *% positiva
149500070222     c                   eval      TR8psp   = '+' + w003a                       *v.assoluto + segno
149600070222x   1c                   else                                                   *% negativa
149700070222     c                   eval      TR8psp   = '-' + w003a                       *v.assoluto + segno
149800070222e   1c                   endif
149900070222     c                   move      mNCL          TR8ncl                         *media colli/mm tot
150000070222     c                   move      $TR8m( 1)     TR8m01                         *colli ultimo mm E01
150100070222     c                   move      $TR8m( 2)     TR8m02                         *colli ultimo mm E02
150200070222     c                   move      $TR8m( 3)     TR8m03                         *colli ultimo mm E03
150300070222     c                   move      $TR8m( 4)     TR8m04                         *colli ultimo mm E04
150400070222     c                   move      $TR8m( 5)     TR8m05                         *colli ultimo mm E05
150500070222     c                   move      $TR8m( 6)     TR8m06                         *colli ultimo mm E06
150600070222     c                   move      $TR8m( 7)     TR8m07                         *colli ultimo mm E07
150700070222     c                   move      $TR8m( 8)     TR8m08                         *colli ultimo mm E08
150800070222     c                   move      $TR8m( 9)     TR8m09                         *colli ultimo mm E09
150900070222     c                   move      $TR8m(10)     TR8m10                         *colli ultimo mm E10
151000070222      *
151100070222      * Calcola i flag lampeggi
151200070222     c                   clear                   TR8brl                         *LAMP ora lettura
151300070222     c                   clear                   TR8bnc                         *LAMP prg colli
151400070222     c                   clear                   TR8bsp                         *LAMP % scostamento
151500070222     c                   clear                   TR8bcl                         *LAMP media tot
151600070222     c                   clear                   $TR8b                          *LAMP media entr.
151700070222     c                   exsr      CtrBlink                                     *calcola lampeggi
151800070222      * Scrive
151900070222if  1c                   if        NOT  %found(FLTR801L)                        *inesistente
152000070222     c                   movel     KR8fil        TR8fil                         *filiale
152100070222     c                   movel     KR8tip        TR8tip                         *tipo standard
152200070222      *                  __________________
152300070222     c                   WRITE     FLTR8000                                     *INSERIMENTO
152400070222      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
152500070222x   1c                   else                                                   *esistente
152600070222      *                  __________________
152700070222     c                   UPDATE    FLTR8000                                     *MODIFICA
152800070222      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
152900070222e   1c                   endif
153000070222      *
153100070222     c                   ENDSR
