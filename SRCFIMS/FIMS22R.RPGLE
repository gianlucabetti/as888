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
001100170215     fFNMSA02L  if   e           k disk
001200060928      *
001300060928     fFLTR801L  Uf A e           k disk
001400170215      *
001500170215     fFNMSS01L  Uf A e           k disk
001600060928
001700060928      *---------------------------------------------------------------*
001800060928
001900060928      *
002000060928      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
002100060928      *
002200060928      *
002300060928      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
002400060928      * - P.O. (da applicazioni per macchinone)
002500060928     d $MTP            s              1    dim(100)
002600060928      *
002700060928      * - Media colli/minuto da tab. MDT
002800060928     d $MDT            s              2  0 dim( 10) inz                         *media lampeggio x b
002900060929      *
003000060929      * - Schiere per "semplificare" il calcolo su campi simili...
003100060929      * - - Colli per entrata
003200060929     d $EXX            s                   like(MSSe01)     dim(20)  inz
003300060929      * - - Minuti lavorati per entrata
003400060929     d $MXX            s                   like(MSSm01)     dim(20)  inz
003500060929      * - - Colli per uscita
003600060929     d $UXX            s                   like(MSSu01)     dim(60)  inz
003700060929      * - - Date lettura per entrata
003800060929     d $DTL            s                   like(MSAdtl)     dim(20)  inz
003900060929      * - - Orari lettura per entrata
004000060929     d $ORL            s                   like(MSAorl)     dim(20)  inz
004100060929      * - - Colli ultimo minuto per entrata - CORRENTE
004200060929     d $MSApep         s                   like(MSApep001)  dim(20)  inz
004300060929      * - - Colli ultimo minuto per uscita  - CORRENTE
004400060929     d $MSApad         s                   like(MSApad001)  dim(60)  inz
004500060929      * - - Media per entrata
004600060929     d $TR8M           s              2  0 dim(20)  inz
004700060928      *
004800060928      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
004900060928      *
005000060928      * - Parametri per richiamo al pgm di controllo profilo utenti
005100060928     d TIBS34ds      e ds
005200060928      * - Ds di riferimento al file esterno AzUte00F
005300060928     d AZUTEds       e ds                  extname(AZUTE00F)
005400060928      * - Ds per dati organigramma
005500060928     d DDatiUte      e ds
005600060928      *
005700060928      * - Dati tabellone
005800060928     d dMDT          e ds                  inz
005900060928      *
006000060928      * - Parametri in INPUT
006100060928     d Param           ds                                                       *DS parametri
006200060928     d  PARfil                 1      3  0                                      - filiale
006300060928     d  PARapl                 4      4                                         - applicazione
006400060928      *
006500060928      * - Controlla data
006600060928     d WLBdat          ds                  inz
006700060928     d  G08dat                        8  0 inz
006800060928     d  G08inv                        8  0 inz
006900060928     d  G08err                        1    inz('3')
007000060928     d  G08tgi                        5  0 inz
007100060928      * - Calcolo data
007200060928     d WGIdat          ds                  inz
007300060928     d  GIOdat                        8  0 inz
007400060928     d  GIOinv                        8  0 inz
007500060928     d  GIOtgi                        5  0 inz
007600060928      *
007700060928      * - Variabili "temporali"
007800060928     d                 ds                  inz
007900060928     d wTempo                          z   inz(z'1901-01-01-01.00.00.000000')
008000060928     d  wData                          d   datfmt(*iso) overlay(wTempo)
008100060928     d  wOra                           t   timfmt(*iso) overlay(wTempo : 12)
008200060928      *
008300060928      * - di comodo (per "raggruppare" campi legati)
008400060928      * - - Flags lampeggio in FLTR800F
008500060929     d                 ds                  inz
008600060928     d   TR8b01                            inz
008700060928     d   TR8b02                            inz
008800060928     d   TR8b03                            inz
008900060928     d   TR8b04                            inz
009000060928     d   TR8b05                            inz
009100060928     d   TR8b06                            inz
009200060928     d   TR8b07                            inz
009300060928     d   TR8b08                            inz
009400060928     d   TR8b09                            inz
009500060928     d   TR8b10                            inz
009600060928     d  $TR8b                  1     10    dim(10)       inz
009700060928      *
009800060928      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
009900060928      *
010000060928      * - Flags booleani
010100060928     d $First          s              1    inz(*on)                             *prima lettura
010200060928      *
010300060928      * - Indici di schiera
010400060928     d XX              s              3  0 inz
010500060928     d YY              s              3  0 inz
010600060928      *
010700060928      * - Campi chiave riferiti al data base
010800060928     d KSAfil          s                   like(MSAfil)  inz
010900060928     d KSAapl          s                   like(MSAapl)  inz
011000060928     d KSAdfv          s                   like(MSAdfv)  inz
011100060928     d KSAnpg          s                   like(MSAnpg)  inz
011200060928     d KSAdtl          s                   like(MSAdtl)  inz
011300060928     d KSAorl          s                   like(MSAorl)  inz
011400060928     d KR8fil          s                   like(TR8fil)  inz
011500060928     d KR8tip          s                   like(TR8tip)  inz
011600060928     d KSSapl          s                   like(MSSapl)  inz
011700060928     d KSSfil          s                   like(MSSfil)  inz
011800060928     d KSSdfv          s                   like(MSSdfv)  inz
011900060928     d KSSnpg          s                   like(MSSnpg)  inz
012000060928      *
012100060928      * - Altre avriabili riferite al data base
012200060928     d wTabApl         s                   like(MSAapl)  inz
012300060928     d wDFVpr          s                   like(MSAdfv)  inz                    *di lavoro
012400060928     d wDFVgg          s                   like(MSAdfv)  inz
012500060928     d DEPdtlin        s                   like(MSAdtl)  inz                    *dep data inizio
012600060928     d DEPorlin        s                   like(MSAorl)  inz                    *dep ora  inizio
012700060928     d DEPdtlfi        s                   like(MSAdtl)  inz                    *dep data fine
012800060928     d DEPorlfi        s                   like(MSAorl)  inz                    *dep ora  fine
012900060928     d DEPdfv          s                   like(MSSdfv)  inz
013000060928     d DEPnpg          s                   like(MSSnpg)  inz
013100060928     d SAVdfv          s                   like(MSAdfv)  inz                    *data foglio preced.
013200060928      *
013300070222      * - Variabili per conteggio dati da aggiornare in FLTR800F
013400060928     d mNCL            s              3  0 inz                                  *media colli/mm tot
013500060928     d mPSP            s              3  0 inz                                  *% scostamento g pre
013600060928      * - Variabili per conteggio dati da aggiornare in FNMSS00F
013700060928     d mMLAV           s                   like(MSStlm)  inz                    *mm lavorati tot
013800060928     d mPesInd         s                   like(MSScla)  inz                    *caricati da
013900060928     d mPusSor         s                   like(MSSsor)  inz                    *smistati da
014000060928     d mOdsDna         s                   like(MSSdna)  inz                    *of x uscita piena
014100060928     d mErrMac         s                   like(MSSerr)  inz                    *of x errori machine
014200060928     d mPS2dok         s                   like(MSSps2)  inz                    *letti da pistola
014300060928     d mCSD            s                   like(MSScsd)  inz                    *letti da scanner
014400170215     d mPWF            s                   like(MSSpwf)  inz                    *letti da pist.Wi-Fi
014500060928     d mCCV            s                   like(MSSccv)  inz                    *letti con volume
014600060928     d mCCP            s                   like(MSSccp)  inz                    *letti con peso
014700060928     d mNLP            s                   like(MSSnlp)  inz                    *non letti pistola
014800060928     d mNLS            s                   like(MSSnls)  inz                    *non letti scanner
014900060928     d mXnoMis         s                   like(MSSnmi)  inz                    *C:non misurati
015000060928     d mXnoPes         s                   like(MSSnpe)  inz                    *C:non pesati
015100060928      * - - CORRENTE - progressivo minuto
015200060929     d mPNC            s                   like(MSSpnc)  inz                    *progressivo colli
015300060928      * - - PRECEDENTE - progressivo minuto
015400060929     d mPNCpr          s                   like(MSSpnc)  inz                    *progressivo colli
015500060928      *
015600060928      * - Campi di comodo
015700060928     d wOraPenUlt      s                   like(MSAorl) inz                     *ora ultimo mm
015800060928     d wDatOraCor      s             12  0 inz                                  *data+ora corr + 10m
015900060928     d wDatOraLav      s             12  0 inz                                  *data+ora lavoro
016000060928     d wDatOraUlt      s             12  0 inz                                  *data+ora ultimo mm
016100060928     d wDatOraPenult   s             12  0 inz                                  *data+ora penult mm
016200060928     d wDatCor         s              8  0 inz                                  *data corrente
016300060928     d wOraCor         s              4  0 inz                                  *ora  corrente
016400060928     d §PNCatt         s             11  3 inz                                  *prg colli attuale
016500060928     d §PNCpre         s             11  3 inz                                  *prg colli preced
016600060928     d §PNCpsp         s              3  0 inz                                  *% scostamento
016700060928     d §Gap            s             18  6 inz
016800060928     d n14             s             14  0 inz                                  *numerico 14
016900061002     d w0080           s              8  0 inz                                  *numerico 8
017000061002     d w0060           s              6  0 inz                                  *numerico 6
017100061002     d w0040           s              4  0 inz                                  *numerico 4
017200061002     d w0030           s              3  0 inz                                  *numerico 3
017300061002     d w003a           s              3    inz                                  *di lavoro
017400060928      *
017500060928      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
017600060928      *
017700170215      * - FNMSA02L
017800060928     c     K06MSA02      klist
017900060928     c                   kfld                    KSAapl
018000060928     c                   kfld                    KSAfil
018100060928     c                   kfld                    KSAdfv
018200060928     c                   kfld                    KSAnpg
018300060928     c                   kfld                    KSAdtl
018400060928     c                   kfld                    KSAorl
018500060928     c     K02MSA02      klist
018600060928     c                   kfld                    KSAapl
018700060928     c                   kfld                    KSAfil
018800060928     c     K04MSA02      klist
018900060928     c                   kfld                    KSAapl
019000060928     c                   kfld                    KSAfil
019100060928     c                   kfld                    KSAdfv
019200060928     c                   kfld                    KSAnpg
019300170215      * - FNMSS01L
019400060928     c     K04MSS01      klist
019500060928     c                   kfld                    KSSapl
019600060928     c                   kfld                    KSSfil
019700060928     c                   kfld                    KSSdfv
019800060928     c                   kfld                    KSSnpg
019900060928      * - FLTR801L
020000060928     c     K02TR801      klist
020100060928     c                   kfld                    KR8fil
020200060928     c                   kfld                    KR8tip
020300060928      * - TNTBE01L
020400060928     c     K02TBE01      klist
020500060928     c                   kfld                    TBEcod
020600060928     c                   kfld                    TBEke1
020700060928
020800060928      *===============================================================*
020900060928      *?  I N D I C A T O R I                                        ?*
021000060928      *---------------------------------------------------------------*
021100060928      *  10 - Comodo                                                  *
021200060928      *===============================================================*
021300060928
021400060928     c     *Entry        plist
021500060928     c                   parm                    Param
021600060928      *
021700060928      * Ciclo per ogni tipo applicazione caricato in schiera
021800060928     c                   eval      YY       =  1
021900060928do  1c                   DOW       $MTP(yy) <> *blanks
022000060928      *
022100060928     c                   movel     $MTP(yy)      wTabApl
022200060928      *
022300060928      * - Legge l'ULTIMO record per sapere qual'è l'ultima lavorazione
022400060929     c                   eval      KSAapl  = wTabApl
022500060929     c                   eval      KSAfil  = PARfil
022600060929     c                   eval      KSAdfv  = *hival
022700060929     c                   eval      KSAnpg  = *hival
022800060929     c                   eval      KSAdtl  = *hival
022900060929     c                   eval      KSAorl  = *hival
023000080422     c     K06MSA02      setgt     FNMSA000
023100080422     c     K02MSA02      readpe    FNMSA000
023200170215if  2c                   if        NOT  %eof(FNMSA02L)
023300170215     c                   exsr      Elab_FNMSA
023400060929e   2c                   endif
023500060928      *
023600060928     c                   add       1             yy
023700060928      *
023800060928e   1c                   ENDDO
023900060928      *
024000060928      * Fine
024100060928     c                   eval      *inRT   = *on
024200060928
024300060928      *---------------------------------------------------------------*
024400060928      *?Operazioni iniziali                                          ?*
024500060928      *---------------------------------------------------------------*
024600060928     c     *InzSR        BEGSR
024700060928      *
024800060928      * Reperimento dati job
024900060928     c                   exsr      DatiJob
025000060928      *
025100060928      * Caricamento tabelle occorrenti
025200060928     c                   exsr      CarTAB
025300060928      *
025400060928     c                   ENDSR
025500060928
025600060928      *---------------------------------------------------------------*
025700060928      *?Reperimento dati del job (Utente/Operativi)                  ?*
025800060928      *---------------------------------------------------------------*
025900060928     c     DatiJob       BEGSR
026000060928      *
026100060928     c     *dtaara       define    §azute        AZUTEds
026200060928     c     *dtaara       define    §datiute      dDatiUte
026300060928      *
026400060928     c                   in(E)     *dtaara
026500060928     c                   if        %error or RSUT = *blanks
026600060928     c                   clear                   TIBS34ds
026700060928     c                   call      'TIBS34R'
026800060928     c                   parm                    TIBS34ds
026900060928     c                   in        *dtaara
027000060928     c                   endif
027100060928      *
027200060928     c                   ENDSR
027300060928
027400060928      *---------------------------------------------------------------*
027500060928      *?Caricamento tabelle occorrenti                               ?*
027600060928      *---------------------------------------------------------------*
027700060928     c     CarTAB        BEGSR
027800060928      *
027900160705      * Dati tabellone della Filiale
028000060928     c                   eval      TBEcod  = 'MDT'
028100060928     c                   movel(p)  PARfil        TBEke1
028200060928     c     K02TBE01      setll     TNTBE000
028300060928     c     K02TBE01      reade     TNTBE000
028400060928do  1c                   dow       NOT  %eof(TNTBE01L)
028500060928if  2c                   if        TBEatb  = *blanks
028600060928     c                   movel     TBEuni        dMDT
028700060928     c                   movel     TBEke2        xx
028800060928     c                   eval      $MDT(xx)  = §MDTmcl                          *media lampeggio
028900060928e   2c                   endif
029000060928     c     K02TBE01      reade     TNTBE000
029100060928e   1c                   enddo
029200160707      *
029300160707      * Dati tabellone di Default
029400160707if  1c                   If        %xfoot( $MDT ) = *zero
029500160707     c                   eval      TBEcod  = 'MDT'
029600160707     c                   movel(p)  '999'         TBEke1
029700160707     c     K02TBE01      setll     TNTBE000
029800160707     c     K02TBE01      reade     TNTBE000
029900160707do  2c                   dow       NOT  %eof(TNTBE01L)
030000160707if  3c                   if        TBEatb  = *blanks
030100160707     c                   movel     TBEuni        dMDT
030200160707     c                   movel     TBEke2        xx
030300160707     c                   eval      $MDT(xx)  = §MDTmcl                          *media lampeggio
030400160707e   3c                   endif
030500160707     c     K02TBE01      reade     TNTBE000
030600160707e   2c                   enddo
030700160707e   1c                   EndIf
030800060928      *
030900060928      * Tipo applicazione
031000060928      * - Leggo la tabella MTP e carico in schiera gli elementi
031100060928     c                   clear                   xx
031200060928     c                   clear                   $MTP
031300060928     c                   eval      TBEcod  = 'MTP'
031400060928     c     TBEcod        setll     TNTBE000
031500060928     c     TBEcod        reade     TNTBE000
031600060928do  1c                   dow       NOT  %eof(TNTBE01L)
031700060928if  2c                   if        TBEatb  = *blanks
031800060928     c                   add       1             xx
031900060928     c                   movel     TBEke1        $MTP(xx)
032000060928e   2c                   endif
032100060928     c     TBEcod        reade     TNTBE000
032200060928e   1c                   enddo
032300060928      *
032400060928     c                   ENDSR
032500060928
032600060928      *---------------------------------------------------------------*
032700060928      *?Azzeramento dei campi di memorizzazione -PRECEDENTE-         ?*
032800060928      *---------------------------------------------------------------*
032900060928     c     sr_AzzMSApr   BEGSR
033000060928      *
033100060928      * Progressivo numero colli
033200060929     c                   clear                   mPNCpr                         *progressivo colli
033300060928      *
033400060928     c                   ENDSR
033500060928
033600060928      *---------------------------------------------------------------*
033700060928      *?Azzeramento dei campi di memorizzazione -CORRENTE-           ?*
033800060928      *---------------------------------------------------------------*
033900060928     c     sr_AzzMSA     BEGSR
034000060928      *
034100060928     c                   eval      $First = *on                                 *prima lettura
034200060928      *
034300060928     c                   clear                   DEPdtlin                       *data inizio
034400060928     c                   clear                   DEPorlin                       *ora  inizio
034500060928     c                   clear                   DEPdtlfi                       *data fine
034600060928     c                   clear                   DEPorlfi                       *ora  fine
034700060928     c                   clear                   wDFVgg                         *gg data foglio
034800060928     c                   clear                   wDFVpr                         *data gg precedente
034900060929     c                   clear                   mPNC                           *progressivo colli
035000060928     c                   clear                   $EXX                           *progressivi entrate
035100060928     c                   clear                   $UXX                           *progressivi uscite
035200060928     c                   clear                   mMLAV                          *mm lavorati
035300060928     c                   clear                   $MXX                           *mm lavorati x entr.
035400060928     c                   clear                   $DTL                           *Date letture entr.
035500060928     c                   clear                   $ORL                           *Ora  letture entr.
035600060928     c                   clear                   mNCL                           *media colli/mm tot
035700060928     c                   clear                   $TR8M                          *media x entrata
035800060928     c                   clear                   mPSP                           *% scostamento g pre
035900060928     c                   clear                   mPesInd                        *caricati da
036000060928     c                   clear                   mPusSor                        *smistati da
036100060928     c                   clear                   mPS2dok                        *letti da pistola
036200060928     c                   clear                   mCSD                           *letti da scanner
036300161214     c                   clear                   mPWF                           *letti da pist.Wi-Fi
036400060928     c                   clear                   mCCV                           *letti con volume
036500060928     c                   clear                   mCCP                           *letti con peso
036600060928     c                   clear                   mNLS                           *non letti scanner
036700060928     c                   clear                   mNLP                           *non letti pistola
036800060928     c                   clear                   mXnoMis                        *C:non misurati
036900060928     c                   clear                   mXnoPes                        *C:non pesati
037000060928     c                   clear                   mOdsDna                        *of x uscita piena
037100060928     c                   clear                   mErrMac                        *of x errori machine
037200060929     c                   clear                   $MSApep                        *colli ultimo min E.
037300060929     c                   clear                   $MSApad                        *colli ultimo min U.
037400060928      *
037500060928     c                   ENDSR
037600060928
037700060928      *---------------------------------------------------------------*
037800060928      *?Sottrazione 10 minuti alla data + ora corrente               ?*
037900060928      *---------------------------------------------------------------*
038000060928     c     sr_SubMin     BEGSR
038100060928      *
038200061002     c                   clear                   w0060
038300061002     c                   movel     w0040         w0060
038400061002     c     *iso          move      w0080         wData
038500061002     c     *iso          move      w0060         wOra
038600060928     c                   subdur    10:*mn        wTempo
038700061002     c     *iso          move      wData         w0080
038800061002     c     *iso          move      wOra          w0060
038900061002     c                   movel     w0060         w0040
039000060928      *
039100060928     c                   ENDSR
039200060928
039300060928      *---------------------------------------------------------------*
039400060928      *?Calcolo dei flag "lampeggi"                                  ?*
039500060928      *---------------------------------------------------------------*
039600060928     c     CtrBlink      BEGSR
039700060928      *
039800060928     c                   clear                   xx
039900060928      *
040000060928do  1c                   DOW       xx        < %elem($TR8b)
040100060928      *
040200060928     c                   add       1             xx
040300060929if  2c                   if             $TR8m(xx) > *zeros
040400060929     c                             and  $TR8m(xx) < $MDT(xx)
040500060928      *
040600060928     c                   movel     $DTL(xx)      wDatOraLav
040700060928     c                   move      $ORL(xx)      wDatOraLav
040800060928if  3c                   if        wDatOraLav   >= wDatOraCor
040900060928     c                   eval      $TR8b(xx) = 'S'                              *LAMP media ent
041000060928e   3c                   endif
041100060928      *
041200060928e   2c                   endif
041300060928      *
041400060928e   1c                   ENDDO
041500060928      *
041600060928     c                   ENDSR
041700060928
041800060928      *---------------------------------------------------------------*
041900060928      *?calcola la percentuale di scostamento                        ?*
042000060929      *?   INPUT  --> §PNCatt - > "X" attuali                        ?*
042100060929      *?              §PNCpre - > "X" precedenti                     ?*
042200060929      *?   OUTPUT --> §PNCpsp - > % di scostamento                   ?*
042300060928      *---------------------------------------------------------------*
042400060928     c     Scostam       BEGSR
042500060928      *
042600060928      * (attuali - precedenti) = gap
042700060928      * (gap x 100) : attuali  = % scostamento
042800060928      *
042900060928      * NB: se l'attuale è uguale a zero, lo imposta a uno per non dare errore
043000060928if  1c                   if        §PNCatt  = *zeros
043100060928     c                   eval      §PNCatt  = 1
043200060928e   1C                   endif
043300060928      *
043400060928      * Calcola la differenza in termini non assoluti: percentuali
043500060928      *   (attuali - precedenti) * 100 / attuali
043600060928     c                   eval      §Gap  = ((§PNCatt - §PNCpre) * 100)
043700060928     c                                     / §PNCatt
043800060928      *
043900060928     c                   z-add     §Gap          §PNCpsp                        *% scostamento
044000060928      * Se la % scostamento è più di 3 cifre -> la sistema
044100060928sel 1c                   select
044200060928w   1c                   when      §Gap >= +1000
044300060928     c                   z-add     999           §PNCpsp
044400060928w   1c                   when      §Gap <= -1000
044500060928     c                   z-sub     999           §PNCpsp
044600060928e   1c                   endsl
044700060928      *
044800060928     c                   ENDSR
044900060929
045000060929      *---------------------------------------------------------------*
045100170215      *?Ciclo di elaborazione del singolo record dal file FNMSA00F   ?*
045200060929      *---------------------------------------------------------------*
045300170215     c     Elab_FNMSA    BEGSR
045400060929      *
045500060929      * Memorizza data + ora dell'ultimo minuto lavorato
045600080422     c                   movel     MSAdtl        wDatOraUlt                     *data+ora ultimo mm
045700080422     c                   move      MSAorl        wDatOraUlt
045800060929      *
045900060929      * Imposta la chiave dell'ultima lettura
046000080422     c                   eval      KSAapl  = MSAapl
046100080422     c                   eval      KSAfil  = MSAfil
046200080422     c                   eval      KSAdfv  = MSAdfv
046300080422     c                   eval      KSAnpg  = MSAnpg
046400060929      *
046500060929      * Imposta la chiave dell'ultima lettura per file saldi
046600060929     c                   eval      DEPdfv  = KSAdfv                             *data foglio
046700060929     c                   eval      DEPnpg  = KSAnpg                             *gipo foglio
046800060929      *
046900060929     c                   clear                   wDatOraPenult
047000060929     c                   clear                   wOraPenult
047100060929      *
047200060929      * Legge il penultimo minuto perchè è sicuramente "completo"
047300060929      * (sono i dati relativi a questo minuto che emette sul tabellone)
047400080422     c     K04MSA02      readPE    FNMSA000
047500170215if  1c                   if        %eof(FNMSA02L)
047600060929     c                   leavesr
047700060929e   1c                   endif
047800060929      *
047900060929      * Memorizza data + ora e ora del penultimo minuto lavorato
048000080422     c                   movel     MSAdtl        wDatOraPenult                  *data+ora penult mm
048100080422     c                   move      MSAorl        wDatOraPenult
048200080422     c                   eval      wOraPenult = MSAorl                          *ora ultimo mm
048300060929      *
048400060929      * Elabora giorno precedente per tabellone
048500170215     c                   exsr      ElabIeri
048600060929      *
048700060929      * Elabora oggi per tabellone e file saldi e scrive FLTR8
048800170215     c                   exsr      ElabOggi
048900060929      *
049000060929      * Scrive i saldi
049100170215     c                   exsr      Wrt_FNMSS
049200060929      *
049300060929     c                   ENDSR
049400060929
049500060929      *---------------------------------------------------------------*
049600060929      *?Elaboro dati del giorno precedente                           ?*
049700060929      *---------------------------------------------------------------*
049800170215     c     ElabIeri      BEGSR
049900060929      *
050000060929      * Memorizza la data foglio
050100060929     c                   eval      SAVdfv  = KSAdfv                             *data foglio preced.
050200060929      *
050300060929      * Calcola il giorno precedente utile alla data foglio trovata
050400060929     c                   reset                   WLBdat
050500060929     c                   eval      G08inv  = KSAdfv
050600060929     c                   call      'XSRDA8'
050700060929     c                   parm                    WLBdat
050800060929     c                   eval      wDFVgg  = G08tgi                             *gg data richiesta
050900060929      *
051000060929      * Ciclo per trovare il primo giorno precedente nel quale il
051100060929      *   macchinone ha lavorato
051200060929     c                   eval      xx      = 1
051300060929do  1c                   DOW       xx     <= 10
051400060929     c                   clear                   WGIdat
051500060929     c                   eval      GIOtgi  = wDFVgg - xx                        *gg data - "X" gg
051600060929     c                   call      'XSRGI8'
051700060929     c                   parm                    WGIdat
051800060929     c                   eval      wDFVpr  = GIOinv                             *data gg precedente
051900060929     c                   eval      KSAdfv  = wDFVpr                             *data foglio preced.
052000080422     c     K04MSA02      setll     FNMSA000
052100170215if  2c                   if        %equal(FNMSA02L)                             *trovati dati
052200060929     c                   leave                                                  *esce dal ciclo
052300060929e   2c                   endif
052400060929     c                   add       1             xx                             *incrementa gg meno
052500060929e   1c                   ENDDO
052600060929      *
052700060929      * Azzera i campi...
052800060929     c                   exsr      sr_AzzMSApr
052900060929      *
053000060929      * Legge tutti i dati della lavorazione -PRECEDENTE-
053100080422     c     K04MSA02      setll     FNMSA000
053200080422     c     K04MSA02      reade     FNMSA000
053300170215do  1c                   DOW       NOT  %eof(FNMSA02L)
053400080422if  2c                   if        MSAorl <= wOraPenult                         *fino all'ultimo mm
053500170215     c                   exsr      sr_MemMSApr
053600060929e   2c                   endif
053700080422     c     K04MSA02      reade     FNMSA000
053800060929e   1c                   ENDDO
053900060929      *
054000060929      * Repimposta la data del foglio "originale"
054100060929     c                   eval      KSAdfv  = SAVdfv                             *Data foglio preced.
054200060929      *
054300060929      * Reperisce la data + ora corrente
054400060929     c                   time                    n14                            *Ora (6)+ Data (8)
054500060929     c                   movel     n14           wOraCor                        *Ora (4)
054600060929     c                   clear                   WLBdat
054700060929     c                   move      n14           G08dat                         *Data (8) in g/m/a
054800060929     c                   eval      G08err  = *off
054900060929     c                   call      'XSRDA8'
055000060929     c                   parm                    WLBdat
055100060929     c                   eval      wDatCor = G08inv                             *Data corrente
055200060929      *
055300060929      * Sottrae 10 mm dalla data + ora corrente
055400061002     c                   eval      w0080   = wDatCor
055500061002     c                   eval      w0040   = wOraCor
055600060929     c                   exsr      sr_SubMin
055700061002     c                   movel     w0080         wDatOraCor
055800061002     c                   move      w0040         wDatOraCor
055900060929      *
056000060929     c                   ENDSR
056100060929
056200060929      *---------------------------------------------------------------*
056300060929      *?Elaboro dati del giorno corrente                             ?*
056400060929      *---------------------------------------------------------------*
056500170215     c     ElabOggi      BEGSR
056600060929      *
056700060929      * Azzera i campi...
056800060929     c                   exsr      sr_AzzMSA
056900060929      *
057000060929      * Legge tutti i dati della lavorazione -CORRENTE-
057100080422     c     K04MSA02      setll     FNMSA000                                     *ultimo
057200080422     c     K04MSA02      reade     FNMSA000
057300060929      *
057400170215do  1c                   DOW       NOT  %eof(FNMSA02L)
057500060929      *
057600080422     c                   movel     MSAdtl        wDatOraLav                     *data+ora
057700080422     c                   move      MSAorl        wDatOraLav
057800060929if  2c                   if        wDatOraLav <= wDatOraUlt                     *fino all'ultimo mm
057900170215     c                   exsr      sr_MemMSA
058000060929e   2c                   endif
058100060929      *
058200060929      * Quando arriva al penultimo minuto:
058300060929      *   scrive il file per il tabellone solo se tipo applicazione
058400160707      *   letto corrisponde a quello passato a pgm ("V" = VDL)
058500070222if  2c                   if             wTabApl    = PARapl
058600070222     c                             and  wDatOraLav = wDatOraPenult              *fino all'ultimo mm
058700170215     c                   exsr      Wrt_FLTR8
058800070222e   2c                   endif
058900060929      *
059000080422     c     K04MSA02      reade     FNMSA000
059100060929      *
059200060929e   1c                   ENDDO
059300060929      *
059400060929     c                   ENDSR
059500060929
059600060929      *---------------------------------------------------------------*
059700060929      *?Memorizzazione dei dati del file -PRECEDENTE-                ?*
059800060929      *---------------------------------------------------------------*
059900170215     c     sr_MemMSApr   BEGSR
060000060929      *
060100060929      * Colli lavorati TOTALI  (compresa baia 20!)
060200060929     c                   eval      mPNCpr   = mPNCpr
060300080422     c                                      + MSApep001
060400080422     c                                      + MSApep002
060500080422     c                                      + MSApep003
060600080422     c                                      + MSApep004
060700080422     c                                      + MSApep005
060800080422     c                                      + MSApep006
060900080422     c                                      + MSApep007
061000080422     c                                      + MSApep008
061100080422     c                                      + MSApep009
061200080422     c                                      + MSApep010
061300080422     c                                      + MSApep011
061400080422     c                                      + MSApep012
061500080422     c                                      + MSApep013
061600080422     c                                      + MSApep014
061700080422     c                                      + MSApep015
061800080422     c                                      + MSApep016
061900080422     c                                      + MSApep017
062000080422     c                                      + MSApep018
062100080422     c                                      + MSApep019
062200080422     c                                      + MSApep020
062300060929      *
062400060929     c                   ENDSR
062500060929
062600060929      *---------------------------------------------------------------*
062700060929      *?Memorizzazione dei dati del file -CORRENTE-                  ?*
062800060929      *---------------------------------------------------------------*
062900170215     c     sr_MemMSA     BEGSR
063000060929      *
063100060929      * Prima data + ora lettura presente
063200060929if  1c                   if        $First   = *on                               *SI prima lettura
063300080422     c                   eval      DEPdtlin = MSAdtl                            *data inizio
063400080422     c                   eval      DEPorlin = MSAorl                            *ora  inizio
063500060929     c                   eval      $First   = *off                              *NO prima lettura
063600060929e   1c                   endif
063700060929      *
063800060929      * Ultima data + ora lettura presente
063900080422     c                   eval      DEPdtlfi = MSAdtl                            *data fine
064000080422     c                   eval      DEPorlfi = MSAorl                            *ora  fine
064100060929      *
064200060929      * Colli ULTIMO MINUTO per tabellone
064300060929     c*** superfluo:     clear                   $MSApep
064400080422     c                   eval      $MSApep( 1) = MSApep001
064500080422     c                   eval      $MSApep( 2) = MSApep002
064600080422     c                   eval      $MSApep( 3) = MSApep003
064700080422     c                   eval      $MSApep( 4) = MSApep004
064800080422     c                   eval      $MSApep( 5) = MSApep005
064900080422     c                   eval      $MSApep( 6) = MSApep006
065000080422     c                   eval      $MSApep( 7) = MSApep007
065100080422     c                   eval      $MSApep( 8) = MSApep008
065200080422     c                   eval      $MSApep( 9) = MSApep009
065300080422     c                   eval      $MSApep(10) = MSApep010
065400080422     c                   eval      $MSApep(11) = MSApep011
065500080422     c                   eval      $MSApep(12) = MSApep012
065600080422     c                   eval      $MSApep(13) = MSApep013
065700080422     c                   eval      $MSApep(14) = MSApep014
065800080422     c                   eval      $MSApep(15) = MSApep015
065900080422     c                   eval      $MSApep(16) = MSApep016
066000080422     c                   eval      $MSApep(17) = MSApep017
066100080422     c                   eval      $MSApep(18) = MSApep018
066200080422     c                   eval      $MSApep(19) = MSApep019
066300080422     c                   eval      $MSApep(20) = MSApep020
066400060929      *
066500060929      * Colli e minuti lavorati TOTALI
066600060929      * - progressivo colli  (compresa baia 20!)
066700060929     c                   clear                   xx
066800060929do  1c                   dow       xx       < %elem($MSApep)
066900060929     c                   add       1             xx
067000060929     c                   add       $MSApep(xx)   mPNC
067100060929e   1c                   enddo
067200060929      * - Progressivo minuti
067300060929     c                   add       1             mMLAV
067400060929      *
067500060929      * Colli e minuti lavorati ENTRATE
067600060929     c                   clear                   xx
067700060929     c                   DOW       xx       < %elem($MSApep)
067800060929     c                   add       1             xx
067900060929if  1c                   if        $MSApep(xx) > *zeros
068000060929     c                   add       1             $MXX(xx)                       *mm entrata
068100060929     c                   add       $MSApep(xx)   $EXX(xx)                       *progressivo  ent
068200080422     c                   eval      $DTL(xx) = MSAdtl                            *data lettura ent
068300080422     c                   eval      $ORL(xx) = MSAorl                            *ora  lettura ent
068400060929e   1c                   endif
068500060929     c                   ENDDO
068600060929      *
068700060929      * Colli USCITE
068800060929     c*** superfluo:     clear                   $MSApad
068900080422     c                   eval      $MSApad( 1) = MSApad001
069000080422     c                   eval      $MSApad( 2) = MSApad002
069100080422     c                   eval      $MSApad( 3) = MSApad003
069200080422     c                   eval      $MSApad( 4) = MSApad004
069300080422     c                   eval      $MSApad( 5) = MSApad005
069400080422     c                   eval      $MSApad( 6) = MSApad006
069500080422     c                   eval      $MSApad( 7) = MSApad007
069600080422     c                   eval      $MSApad( 8) = MSApad008
069700080422     c                   eval      $MSApad( 9) = MSApad009
069800080422     c                   eval      $MSApad(10) = MSApad010
069900080422     c                   eval      $MSApad(11) = MSApad011
070000080422     c                   eval      $MSApad(12) = MSApad012
070100080422     c                   eval      $MSApad(13) = MSApad013
070200080422     c                   eval      $MSApad(14) = MSApad014
070300080422     c                   eval      $MSApad(15) = MSApad015
070400080422     c                   eval      $MSApad(16) = MSApad016
070500080422     c                   eval      $MSApad(17) = MSApad017
070600080422     c                   eval      $MSApad(18) = MSApad018
070700080422     c                   eval      $MSApad(19) = MSApad019
070800080422     c                   eval      $MSApad(20) = MSApad020
070900080422     c                   eval      $MSApad(21) = MSApad021
071000080422     c                   eval      $MSApad(22) = MSApad022
071100080422     c                   eval      $MSApad(23) = MSApad023
071200080422     c                   eval      $MSApad(24) = MSApad024
071300080422     c                   eval      $MSApad(25) = MSApad025
071400080422     c                   eval      $MSApad(26) = MSApad026
071500080422     c                   eval      $MSApad(27) = MSApad027
071600080422     c                   eval      $MSApad(28) = MSApad028
071700080422     c                   eval      $MSApad(29) = MSApad029
071800080422     c                   eval      $MSApad(30) = MSApad030
071900080422     c                   eval      $MSApad(31) = MSApad031
072000080422     c                   eval      $MSApad(32) = MSApad032
072100080422     c                   eval      $MSApad(33) = MSApad033
072200080422     c                   eval      $MSApad(34) = MSApad034
072300080422     c                   eval      $MSApad(35) = MSApad035
072400080422     c                   eval      $MSApad(36) = MSApad036
072500080422     c                   eval      $MSApad(37) = MSApad037
072600080422     c                   eval      $MSApad(38) = MSApad038
072700080422     c                   eval      $MSApad(39) = MSApad039
072800080422     c                   eval      $MSApad(40) = MSApad040
072900080422     c                   eval      $MSApad(41) = MSApad041
073000080422     c                   eval      $MSApad(42) = MSApad042
073100080422     c                   eval      $MSApad(43) = MSApad043
073200080422     c                   eval      $MSApad(44) = MSApad044
073300080422     c                   eval      $MSApad(45) = MSApad045
073400080422     c                   eval      $MSApad(46) = MSApad046
073500080422     c                   eval      $MSApad(47) = MSApad047
073600080422     c                   eval      $MSApad(48) = MSApad048
073700080422     c                   eval      $MSApad(49) = MSApad049
073800080422     c                   eval      $MSApad(50) = MSApad050
073900080422     c                   eval      $MSApad(51) = MSApad051
074000080422     c                   eval      $MSApad(52) = MSApad052
074100080422     c                   eval      $MSApad(53) = MSApad053
074200080422     c                   eval      $MSApad(54) = MSApad054
074300080422     c                   eval      $MSApad(55) = MSApad055
074400080422     c                   eval      $MSApad(56) = MSApad056
074500080422     c                   eval      $MSApad(57) = MSApad057
074600080422     c                   eval      $MSApad(58) = MSApad058
074700080422     c                   eval      $MSApad(59) = MSApad059
074800080422     c                   eval      $MSApad(60) = MSApad060
074900060929     c                   add       $MSApad       $UXX                           *progr.colli x usc.
075000060929      *
075100060929      * Dati statistici
075200080422     c                   add       MSApesind     mPesInd                        *caricati
075300080422     c                   add       MSApussor     mPusSor                        *smistati
075400080422     c                   add       MSAps2dok     mPs2Dok                        *letti da pistola
075500080422     c                   add       MSAcsd        mCSD                           *letti da scanner
075600170215     c                   add       MSApwf        mPWF                           *letti da pist.WiFi
075700080422     c                   add       MSAccv        mCCV                           *letti con volume
075800080422     c                   add       MSAccp        mCCP                           *letti con peso
075900080422     c                   add       MSAnls        mNLS                           *non letti scanner
076000080422     c                   add       MSAnlp        mNLP                           *non letti pistola
076100060929      * C: non misurati
076200060929     c                   eval      mXnoMis  = mXnoMis                           *C: non misurati
076300080422     c                                      + (MSApussor - MSAccv)
076400060929      * C: non pesati
076500060929     c                   eval      mXnoPes  = mXnoPes                           *C: non pesati
076600080422     c                                      + (MSApussor - MSAccp)
076700060929      * - OF x uscita piena
076800080422     c                   add       MSAodsdna     mOdsDna                        · TR4ods = '3'
076900060929      * - OF x errori macchina
077000060929     c                   eval      mErrMac  = mErrMac
077100080422     c                                      + MSAodsdnr                         + TR4ods = '2'
077200080422     c                                      + MSAodsftd                         + TR4ods = '4'
077300080422     c                                      + MSAodsids                         + TR4ods = '5'
077400080422     c                                      + MSAodsuds                         + TR4ods = '6'
077500080422     c                                      + MSAodsdne                         + TR4ods = '7'
077600080422     c                                      + MSAodsciv                         + TR4ods = '8'
077700080422     c                                      + MSAodsdnq                         + TR4ods = '9'
077800080422     c*** NO "unknown state":               + MSAodsust                         + TR4ods = '0'
077900060929      *
078000060929     c                   ENDSR
078100060929
078200060929      *---------------------------------------------------------------*
078300060929      *?Scrittura del file per le statistiche                        ?*
078400060929      *---------------------------------------------------------------*
078500170215     c     Wrt_FNMSS     BEGSR
078600060929      *
078700080422     c                   clear                   FNMSS000
078800060929      *
078900060929      * Reperisce il record da aggiornare
079000080422     c                   eval      KSSapl   = MSAapl                            *tipo applicazione
079100060929     c                   eval      KSSfil   = PARfil                            *p.o.
079200060929     c                   eval      KSSdfv   = DEPdfv                            *data foglio
079300060929     c                   eval      KSSnpg   = DEPnpg                            *categoria foglio
079400080422     c     K04MSS01      chain     FNMSS000
079500060929      *
079600060929      * Imposta i campi
079700080422     c                   eval      MSSpnc   = mPNC                              *progressivo colli
079800080422     c                   eval      MSStlm   = mMLAV                             *mm lavorati
079900060929      * - colli per entrata
080000080422     c                   eval      MSSe01   = $EXX( 1)
080100080422     c                   eval      MSSe02   = $EXX( 2)
080200080422     c                   eval      MSSe03   = $EXX( 3)
080300080422     c                   eval      MSSe04   = $EXX( 4)
080400080422     c                   eval      MSSe05   = $EXX( 5)
080500080422     c                   eval      MSSe06   = $EXX( 6)
080600080422     c                   eval      MSSe07   = $EXX( 7)
080700080422     c                   eval      MSSe08   = $EXX( 8)
080800080422     c                   eval      MSSe09   = $EXX( 9)
080900080422     c                   eval      MSSe10   = $EXX(10)
081000080422     c                   eval      MSSe11   = $EXX(11)
081100080422     c                   eval      MSSe12   = $EXX(12)
081200080422     c                   eval      MSSe13   = $EXX(13)
081300080422     c                   eval      MSSe14   = $EXX(14)
081400080422     c                   eval      MSSe15   = $EXX(15)
081500080422     c                   eval      MSSe16   = $EXX(16)
081600080422     c                   eval      MSSe17   = $EXX(17)
081700080422     c                   eval      MSSe18   = $EXX(18)
081800080422     c                   eval      MSSe19   = $EXX(19)
081900080422     c                   eval      MSSe20   = $EXX(20)
082000060929      * - minuti per entrata
082100080422     c                   eval      MSSm01   = $MXX( 1)
082200080422     c                   eval      MSSm02   = $MXX( 2)
082300080422     c                   eval      MSSm03   = $MXX( 3)
082400080422     c                   eval      MSSm04   = $MXX( 4)
082500080422     c                   eval      MSSm05   = $MXX( 5)
082600080422     c                   eval      MSSm06   = $MXX( 6)
082700080422     c                   eval      MSSm07   = $MXX( 7)
082800080422     c                   eval      MSSm08   = $MXX( 8)
082900080422     c                   eval      MSSm09   = $MXX( 9)
083000080422     c                   eval      MSSm10   = $MXX(10)
083100080422     c                   eval      MSSm11   = $MXX(11)
083200080422     c                   eval      MSSm12   = $MXX(12)
083300080422     c                   eval      MSSm13   = $MXX(13)
083400080422     c                   eval      MSSm14   = $MXX(14)
083500080422     c                   eval      MSSm15   = $MXX(15)
083600080422     c                   eval      MSSm16   = $MXX(16)
083700080422     c                   eval      MSSm17   = $MXX(17)
083800080422     c                   eval      MSSm18   = $MXX(18)
083900080422     c                   eval      MSSm19   = $MXX(19)
084000080422     c                   eval      MSSm20   = $MXX(20)
084100060929      * Colli per uscita
084200080422     c                   eval      MSSu01   = $UXX( 1)
084300080422     c                   eval      MSSu02   = $UXX( 2)
084400080422     c                   eval      MSSu03   = $UXX( 3)
084500080422     c                   eval      MSSu04   = $UXX( 4)
084600080422     c                   eval      MSSu05   = $UXX( 5)
084700080422     c                   eval      MSSu06   = $UXX( 6)
084800080422     c                   eval      MSSu07   = $UXX( 7)
084900080422     c                   eval      MSSu08   = $UXX( 8)
085000080422     c                   eval      MSSu09   = $UXX( 9)
085100080422     c                   eval      MSSu10   = $UXX(10)
085200080422     c                   eval      MSSu11   = $UXX(11)
085300080422     c                   eval      MSSu12   = $UXX(12)
085400080422     c                   eval      MSSu13   = $UXX(13)
085500080422     c                   eval      MSSu14   = $UXX(14)
085600080422     c                   eval      MSSu15   = $UXX(15)
085700080422     c                   eval      MSSu16   = $UXX(16)
085800080422     c                   eval      MSSu17   = $UXX(17)
085900080422     c                   eval      MSSu18   = $UXX(18)
086000080422     c                   eval      MSSu19   = $UXX(19)
086100080422     c                   eval      MSSu20   = $UXX(20)
086200080422     c                   eval      MSSu21   = $UXX(21)
086300080422     c                   eval      MSSu22   = $UXX(22)
086400080422     c                   eval      MSSu23   = $UXX(23)
086500080422     c                   eval      MSSu24   = $UXX(24)
086600080422     c                   eval      MSSu25   = $UXX(25)
086700080422     c                   eval      MSSu26   = $UXX(26)
086800080422     c                   eval      MSSu27   = $UXX(27)
086900080422     c                   eval      MSSu28   = $UXX(28)
087000080422     c                   eval      MSSu29   = $UXX(29)
087100080422     c                   eval      MSSu30   = $UXX(30)
087200080422     c                   eval      MSSu31   = $UXX(31)
087300080422     c                   eval      MSSu32   = $UXX(32)
087400080422     c                   eval      MSSu33   = $UXX(33)
087500080422     c                   eval      MSSu34   = $UXX(34)
087600080422     c                   eval      MSSu35   = $UXX(35)
087700080422     c                   eval      MSSu36   = $UXX(36)
087800080422     c                   eval      MSSu37   = $UXX(37)
087900080422     c                   eval      MSSu38   = $UXX(38)
088000080422     c                   eval      MSSu39   = $UXX(39)
088100080422     c                   eval      MSSu40   = $UXX(40)
088200080422     c                   eval      MSSu41   = $UXX(41)
088300080422     c                   eval      MSSu42   = $UXX(42)
088400080422     c                   eval      MSSu43   = $UXX(43)
088500080422     c                   eval      MSSu44   = $UXX(44)
088600080422     c                   eval      MSSu45   = $UXX(45)
088700080422     c                   eval      MSSu46   = $UXX(46)
088800080422     c                   eval      MSSu47   = $UXX(47)
088900080422     c                   eval      MSSu48   = $UXX(48)
089000080422     c                   eval      MSSu49   = $UXX(49)
089100080422     c                   eval      MSSu50   = $UXX(50)
089200080422     c                   eval      MSSu51   = $UXX(51)
089300080422     c                   eval      MSSu52   = $UXX(52)
089400080422     c                   eval      MSSu53   = $UXX(53)
089500080422     c                   eval      MSSu54   = $UXX(54)
089600080422     c                   eval      MSSu55   = $UXX(55)
089700080422     c                   eval      MSSu56   = $UXX(56)
089800080422     c                   eval      MSSu57   = $UXX(57)
089900080422     c                   eval      MSSu58   = $UXX(58)
090000080422     c                   eval      MSSu59   = $UXX(59)
090100080422     c                   eval      MSSu60   = $UXX(60)
090200060929      *
090300080422     c                   eval      MSScla   = mPesInd                           *caricati da
090400080422     c                   eval      MSSsor   = mPusSor                           *smistati da
090500080422     c                   eval      MSSdna   = mOdsDna                           *of x uscita piena
090600080422     c                   eval      MSSerr   = mErrMac                           *of x errori macchin
090700080422     c                   eval      MSSps2   = mPs2Dok                           *letti da pistaola
090800080422     c                   eval      MSScsd   = mCSD                              *letti da scanner
090900080422     c                   eval      MSSccv   = mCCV                              *letti con volume
091000080422     c                   eval      MSSccp   = mCCP                              *letti con peso
091100080422     c                   eval      MSSnlp   = mNLP                              *non letti pistola
091200080422     c                   eval      MSSnls   = mNLS                              *non letti scanner
091300080422     c                   eval      MSSnpe   = mXnoPes                           *C:non pesati
091400080422     c                   eval      MSSnmi   = mXnoMis                           *C:non misurati
091500170215     c                   eval      MSSpwf   = mPWF                              *letti da pist.Wi-Fi
091600060929      *
091700060929      * Scrive
091800170215if  1c                   if        NOT  %found(FNMSS01L)                        *inesistente
091900080422     c                   eval      MSSapl   = KSSapl                            *tipo applicazione
092000080422     c                   eval      MSSfil   = KSSfil                            *p.o.
092100080422     c                   eval      MSSdfv   = KSSdfv                            *data foglio
092200080422     c                   eval      MSSnpg   = KSSnpg                            *categoria foglio
092300060929      *                  __________________
092400080422     c                   WRITE     FNMSS000                                     *INSERIMENTO
092500060929      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
092600060929x   1c                   else                                                   *esistente
092700060929      *                  __________________
092800080422     c                   UPDATE    FNMSS000                                     *MODIFICA
092900060929      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
093000060929e   1c                   endif
093100060929      *
093200060929     c                   ENDSR
093300070222
093400070222      *---------------------------------------------------------------*
093500070222      *?Scrittura del file per il tabellone                          ?*
093600070222      *---------------------------------------------------------------*
093700170215     c     Wrt_FLTR8     BEGSR
093800070222      *
093900070222      * Imposta i colli lavorati nel minuto
094000070222     c                   z-add     $MSApep       $TR8M
094100070222      * - Totale dei colli lavorati nel minuto
094200070222      *   (compresa baia 20!)
094300070222     c                   clear                   mNCL
094400070222     c                   clear                   xx
094500070222do  1c                   dow       xx       < %elem($MSApep)
094600070222     c                   add       1             xx
094700070222     c                   add       $MSApep(xx)   mNCL
094800070222e   1c                   enddo
094900070222      *
095000070222      * Calcola la % di scostamento sulla lavorazione precedente del
095100070222      *   progressivo colli
095200070222if  1c                   if             mPNC     > *zeros
095300070222     c                             or   mPNCpr   > *zeros
095400070222     c                   z-add     mPNC          §PNCatt                        *prg colli attuali
095500070222     c                   z-add     mPNCpr        §PNCpre                        *prg colli preced
095600070222     c                   z-add     *zeros        §PNCpsp                        *% scostamento
095700070222     c                   exsr      scostam                                      *CALCOLA PERCENTUALE
095800070222     c                   z-add     §PNCpsp       mPSP                           *% scostamento
095900070222e   1c                   endif
096000070222      *
096100070222      * Aggancia il record da aggiornare
096200070222     c                   clear                   FLTR8000                       *tabellone
096300070222     c                   movel     PARfil        KR8fil                         *filiale
096400070222     c                   clear                   KR8tip                         *tipo standard
096500070222     c     K02TR801      chain     FLTR8000
096600070222      *
096700070222      * Imposta i campi
096800160707if  1c                   if        NOT  %found(FLTR801L)                        *inesistente
096900160707     c                   clear                   TR8orl
097000160707     c                   clear                   TR8pnc
097100160707     c                   endif
097200160707     c                   if        TR8orl  <> %editc( DEPorlfi : 'X' )  or
097300160707     c                             TR8pnc  <> %subst(
097400160707     c                                        %editc( mPNC : 'X' ) : 2 )
097500160707     c                   eval      TR8time  = %timestamp()                      *data/ora ins./mod.
097600160707     c                   endif
097700160707      *
097800070222     c                   move      DEPorlfi      TR8orl                         *ora lettura
097900070222     c                   move      mPNC          TR8pnc                         *progressivo colli
098000070222      *
098100070222     c                   eval      w0030    = %abs(mPSP)                        *v.assoluto %
098200070222     c                   movel     w0030         w003a
098300070222if  1c                   if        mPSP    >= 0                                 *% positiva
098400070222     c                   eval      TR8psp   = '+' + w003a                       *v.assoluto + segno
098500070222x   1c                   else                                                   *% negativa
098600070222     c                   eval      TR8psp   = '-' + w003a                       *v.assoluto + segno
098700070222e   1c                   endif
098800070222     c                   move      mNCL          TR8ncl                         *media colli/mm tot
098900070222     c                   move      $TR8m( 1)     TR8m01                         *colli ultimo mm E01
099000070222     c                   move      $TR8m( 2)     TR8m02                         *colli ultimo mm E02
099100070222     c                   move      $TR8m( 3)     TR8m03                         *colli ultimo mm E03
099200070222     c                   move      $TR8m( 4)     TR8m04                         *colli ultimo mm E04
099300070222     c                   move      $TR8m( 5)     TR8m05                         *colli ultimo mm E05
099400070222     c                   move      $TR8m( 6)     TR8m06                         *colli ultimo mm E06
099500070222     c                   move      $TR8m( 7)     TR8m07                         *colli ultimo mm E07
099600070222     c                   move      $TR8m( 8)     TR8m08                         *colli ultimo mm E08
099700070222     c                   move      $TR8m( 9)     TR8m09                         *colli ultimo mm E09
099800070222     c                   move      $TR8m(10)     TR8m10                         *colli ultimo mm E10
099900070222      *
100000070222      * Calcola i flag lampeggi
100100070222     c                   clear                   TR8brl                         *LAMP ora lettura
100200070222     c                   clear                   TR8bnc                         *LAMP prg colli
100300070222     c                   clear                   TR8bsp                         *LAMP % scostamento
100400070222     c                   clear                   TR8bcl                         *LAMP media tot
100500070222     c                   clear                   $TR8b                          *LAMP media entr.
100600070222     c                   exsr      CtrBlink                                     *calcola lampeggi
100700070222      * Scrive
100800070222if  1c                   if        NOT  %found(FLTR801L)                        *inesistente
100900070222     c                   movel     KR8fil        TR8fil                         *filiale
101000070222     c                   movel     KR8tip        TR8tip                         *tipo standard
101100070222      *                  __________________
101200070222     c                   WRITE     FLTR8000                                     *INSERIMENTO
101300070222      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
101400070222x   1c                   else                                                   *esistente
101500070222      *                  __________________
101600070222     c                   UPDATE    FLTR8000                                     *MODIFICA
101700070222      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
101800070222e   1c                   endif
101900070222      *
102000070222     c                   ENDSR
