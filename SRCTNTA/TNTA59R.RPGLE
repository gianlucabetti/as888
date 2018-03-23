000100060707      *---------------------------------------------------------------*
000200060707      *? Stampa lettera  "Mandato ad assicurare per conto - AC BASE" ?*
000300060707      *---------------------------------------------------------------*
000400060804      *?NOTE:?                                                        *
000500060804      *?Parametri  richiesti  nella ds FNLV45DS:?                     *
000600060804      * · I45NRV (numero visita) / I45KSC (codice cliente)            *
000700060804      * · I45AST (ambiente  - valori gestiti: V=Visite, C=Clienti)    *
000800060804      * · I45CTR (codice      tariffa)                                *
000900060804      * · I45PRG (progressivo tariffa)                                *
001000060804      *?Parametri facoltativi nella ds FNLV45DS:?                     *
001100060804      * · I45APP (p.o.      - se non impostato: assunto DUTPOU)       *
001200060804      * · I45LIN (lingua    - se non impostato: assunto I=Italiano)   *
001300060804      *---------------------------------------------------------------*
001400060707
001500060707     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600060707
001700060707      *---------------------------------------------------------------*
001800060707
001900070208     fTNTAM01L  if   e           k disk    usropn
002000070208     fTNOFM01L  if   e           k disk    usropn
002100070208     f                                     rename(TNTAM000:TNOFM000)
002200070208     fTITPT01L  if   e           k disk    usropn
002300070208     fTIOPT01L  if   e           k disk    usropn
002400070208     f                                     rename(TITPT000:TIOPT000)
002500070208     fTITPD01L  if   e           k disk    usropn
002600070208     fTIOPD01L  if   e           k disk    usropn
002700070208     f                                     rename(TITPD000:TIOPD000)
002800091022     fTivis05l  if   e           k disk    usropn
002900070208     fTFACO00F  if   e           k disk    usropn
003000070208     f                                     rename(CNACO000:TFACO000)
003100070208     fTFIND00F  if   e           k disk    usropn
003200070208     f                                     rename(CNIND000:TFIND000)
003300060724     fAZORG01L  if   e           k disk
003400060724      *
003500070208     fTNTA59P   o    e             printer usropn
003600060707
003700060707      *---------------------------------------------------------------*
003800060707
003900060707      *
004000060707      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *?
004100060707      *
004200060707      *
004300060707      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *?
004400060707      *
004500070208     d $Cmd            s             80      dim( 1) ctdata perrcd(1)
004600060707      *
004700060707      *?  D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *?
004800060707      *
004900060707      * Parametri
005000060724     d KPJBA         e ds
005100060724     d FNLV45ds      e ds
005200060707      *
005300060707      * - Parametri x Controllo profilo utenti
005400060707     d TIBS34ds      e ds
005500060707      * - Ds di riferimento al file esterno AZUTE00F
005600060707     d AZUTEds       e ds                  extname(AZUTE00F)
005700060707      * - Ds per dati organigramma
005800060707     d dDATIUTE      e ds
005900060707      *
006000060707      * Decodifica cliente
006100060707     d TIBS69ds      e ds                  inz
006200060707     d  I69tla       e                     inz('L')
006300060707     d DS_cnaco      e ds                  extname(CNACO00F) inz
006400060707     d DS_cnind      e ds                  extname(CNIND00F) inz
006500060707     d DS_cnclp      e ds                  extname(CNCLP00F) inz
006600060707     d DS_fncls      e ds                  extname(FNCLS00F) inz
006700060707      *
006800060707     d Status         sds
006900060707     d  SDSpgm           *proc
007000060707     d  SDSprm           *parms
007100060724      *
007200060724     d WLBdat          ds                  inz
007300060724     d  G02dat                 1      8  0 inz
007400060724     d  G02inv                 9     16  0 inz
007500060724     d  G02err                17     17    inz('3')
007600060724     d  G02tgi                18     22  0 inz
007700060707      *
007800060707      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *?
007900060707      *
008000060724      * - Flags
008100060724     d $Err            s              1    inz(*off)
008200060724      * - Parametri per QCMDEXC
008300070208     d Qcmd            s             80    inz
008400060724     d Qlen            s             15  5 inz(%len(Qcmd))
008500060724      * - Campi di comodo
008600070208     d WWWksc          s                   like(I45ksc)   inz
008700060725     d WWWftc          s                   like(TPDftc)   inz('d ')
008800091022
008900091022     itivis000
009000091022     i              visesi                      wisesi
009100091022
009200060724      *
009300060724      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *?
009400060724      *
009500060918      * > TNTAM01L
009600060724     c     K03TAM01      klist
009700070208     c                   kfld                    WWWksc
009800060724     c                   kfld                    I45ctr
009900060724     c                   kfld                    I45prg
010000080709      * > TNOFM01L
010100080709     c     K03OFM01      klist
010200080709     c                   kfld                    I45nrv
010300080709     c                   kfld                    I45ctr
010400080709     c                   kfld                    I45prg
010500060918      * > TITPT01L / TITPD01L
010600060725     c     K04TPx01      klist
010700070208     c                   kfld                    WWWksc
010800060725     c                   kfld                    I45ctr
010900060725     c                   kfld                    I45prg
011000060725     c                   kfld                    WWWftc
011100080709      * > TIOPT01L / TIOPD01L
011200080709     c     K04OPx01      klist
011300080709     c                   kfld                    I45nrv
011400080709     c                   kfld                    I45ctr
011500080709     c                   kfld                    I45prg
011600080709     c                   kfld                    WWWftc
011700070208      * > TFACO00F / TFIND00F
011800070208     c     K03ACO01      klist
011900070208     c                   kfld                    ACOkut
012000070208     c                   kfld                    I45kcc
012100070208     c                   kfld                    WWWksc
012200060707
012300060707      *===============================================================*
012400060707      *?RIEPILOGO INDICATORI                                         ?*
012500060707      *---------------------------------------------------------------*
012600060707      *       -                                                       *
012700060707      *===============================================================*
012800060707
012900060707     c     *Entry        plist
013000060724     c                   parm                    KPJBA
013100060724     c                   parm                    FNLV45ds
013200060803      *
013300060803      * Sistemazione eventuali parametri mancanti:
013400110103     c**                 if             I45ast <> 'V'
013500110103     c                   if             I45ast <> 'C'
013600091022     c                             and  I45ast <> 'T'
013700070208     c                   eval      $Err   =  *on
013800070208     c                   goto      Fine
013900070208     c                   endif
014000060803      * - P.O. di appartenenza
014100060803     c                   if        I45app =  *zeros
014200060803     c                   eval      I45app =  DUTpou
014300060803     c                   endif
014400070208      * - CapoConto
014500070208     c                   if        I45kcc =  *zeros
014600070208     c                   eval      I45kcc =  DUTkci
014700070208     c                   endif
014800070208      * - Numero copie di stampa
014900070208     c                   if        I45nco <= *zeros
015000070208     c                   eval      I45nco =  1
015100070208     c                   endif
015200070208      * - Lingua
015300060803     c                   if        I45lin =  *blanks
015400060803     c                   eval      I45lin =  'I'
015500060803     c                   endif
015600060724      *
015700060724     c                   reset                   $Err
015800070208      *
015900070208      * Override al PrtF
016000070208     c                   exsr      sr_OvrPrtF
016100060707      *
016200060707      * Pulizia del PRTF
016300060707     c                   clear                   TA59ACBAS1
016400060804     c                   clear                   TA59ACBAS2
016500070122     c                   clear                   TA59FIRMAR
016600060707      *
016700060707      * Impostazione dei campi in stampa
016800060707     c                   exsr      Rie_Out
016900060918     c     $Err          cabeq     *on           Fine
017000060707      *
017100060724      * Stampa
017200060731sel 1c                   select
017300060731      * - Italiano
017400061120w   1c                   when           I45lin = '1'
017500061120     c                             or   I45lin = 'I'
017600061120     c                             or   I45lin = *blanks
017700070202     c                   eval      §POF   = 8,90
017800060724     c                   write     TA59ACBAS1
017900060724     c                   write     TA59ACBAS2
018000070122     c                   write     TA59FIRMAR
018100060731      * - Inglese
018200060731w   1c                   when      I45lin = '2'
018300070205     c                   eval      §POF   = 9,15
018400070205     c                   write     TA59ACBA21
018500070205     c                   write     TA59ACBA22
018600070122     c                   write     TA59FIRMAR
018700060731      * - Francese
018800060731w   1c                   when      I45lin = '3'
018900070205     c                   eval      §POF   = 9,65
019000070205     c                   write     TA59ACBA31
019100070205     c                   write     TA59ACBA32
019200070122     c                   write     TA59FIRMAR
019300061120      * - Tedesco
019400060731w   1c                   when      I45lin = '4'
019500070205     c                   eval      §POF   = 10,00
019600070205     c                   write     TA59ACBA41
019700070205     c                   write     TA59ACBA42
019800070122     c                   write     TA59FIRMAR
019900060731      *
020000060731e   1c                   endsl
020100060707      *
020200060707      * Fine
020300060724     c     Fine          tag
020400060707     c                   eval      *inLR  = *on
020500060707
020600060707      *---------------------------------------------------------------*
020700060707      *?OPERAZIONI INIZIALI                                          ?*
020800060707      *---------------------------------------------------------------*
020900060707     c     *InzSR        BEGSR
021000060707      *
021100060707      * Reperisco dati societari
021200060707     c     *dtaara       define    §azute        AZUTEds
021300060707     c     *dtaara       define    §datiute      dDATIUTE
021400060707     c                   in(E)     *dtaara
021500060707     c                   if        %ERROR or RSUT = *blanks
021600060707     c                   clear                   Tibs34Ds
021700060707     c                   call      'TIBS34R'
021800060707     c                   parm                    Tibs34Ds
021900060707     c                   in        *dtaara
022000060707     c                   endif
022100060707      *
022200060707     c                   ENDSR
022300070208
022400070208      *---------------------------------------------------------------*
022500070208      *?OVERRIDE AL PRINTER FILE                                     ?*
022600070208      *---------------------------------------------------------------*
022700070208     c     sr_OvrPrtF    BEGSR
022800070208      *
022900070208     c                   eval      Qcmd = %trimr($Cmd(1))
023000070208     c                                  + %trim(%editc(I45nco:'Z'))
023100070208     c                                  + ')'
023200070208      *
023300070208     c                   call      'QCMDEXC'
023400070208     c                   parm                    Qcmd
023500070208     c                   parm                    Qlen
023600070208      *
023700070208     c                   open      TNTA59P
023800070208      *
023900070208     c                   ENDSR
024000060707
024100060707      *---------------------------------------------------------------*
024200060707      *?IMPOSTAZIONE DEI CAMPI IN STAMPA                             ?*
024300060707      *---------------------------------------------------------------*
024400060707     c     Rie_Out       BEGSR
024500070208      *
024600070208      * Reperisco dati di tariffa / offerta
024700110103     c**                 if        I45ast = 'V' or I45ast = 'T'
024800110103     c                   if        I45ast = 'T'
024900070208     c                   exsr      Rep_Offerta
025000070208     c                   else
025100070208     c                   exsr      Rep_Tariffa
025200070208     c                   endif
025300060724      *
025400060728      * Decodifica P.O.
025500060728     c     I45app        chain     AZORG
025600060728     c                   if        NOT  %found(AZORG01L)
025700060728     c                             or   ORGfva <> *blanks
025800060918     c***                clear                   ORGdes
025900060918     c                   eval      $Err   =  *on
026000060918     c                   leavesr
026100060728     c                   endif
026200060707      *
026300060804      * "TESTATA" LETTERA - - - - - - - - - - - - - - - - - - - - - - *
026400060804      *
026500060804      * Impostazione dati del cliente
026600060707     c                   eval      MACrsc =  ACOrag
026700060707     c                   eval      MACinc =  INDvia
026800061218     ***c                   eval      MACcac =  %trim(%editw(INDcap:'0     '))
026900061218     ***c                   eval      MACloc =  INDcit
027000061218     ***c                   if        INDprv <> *blanks
027100061218     ***c*** no parentesi:  eval      MACpnc =  '(' + INDprv + ')'
027200061218     ***c                   eval      MACpnc =  INDprv
027300061218     ***c                   else
027400061218     ***c                   eval      MACpnc =  INDsta
027500061218     ***c                   endif
027600061218     c                   eval      MACclc =  %trim(%editw(INDcap:'0     '))
027700061218     c                                    +  '  '
027800061218     c                                    +  INDcit
027900061218     c                   if        INDprv <> *blanks
028000061218     c                   eval      MACclc =  %trim(MACclc)
028100061218     c                                    +  '  '
028200061218     c                                    +  INDprv
028300061218     c                   else
028400061218     c                   eval      MACclc =  %trim(MACclc)
028500061218     c                                    +  '  '
028600061218     c                                    +  INDsta
028700061218     c                   endif
028800060707      *
028900060707      * Decodifica P.O.
029000060724     c                   eval      MAClpo =  ORGdes
029100060804      *
029200060804      * "CORPO" LETTERA - - - - - - - - - - - - - - - - - - - - - - - *
029300060707      *
029400060724      * Impostazione data decorrenza tariffa
029500060724     c                   reset                   WLBdat
029600060724     c                   eval      G02inv = TAMddt
029700060724     c*** già così:      eval      G02err = '3'
029800060724     c                   call      'XSRDA8'
029900060724     c                   parm                    WLBdat
030000060724     c                   eval      MACdta = G02dat
030100060707      *
030200060724      * Impostazione COSTO (importo tariffa)
030300070205     c                   if        I45lin = '3'
030400070205     c                   eval      MACcos =  %trim(%editc(TPDitr:'N'))
030500070205     c                                    +  ' euros/Kg'
030600070205     c                   else
030700060725     c                   eval      MACcos =  %trim(%editc(TPDitr:'N'))
030800060725     c                                    +  ' Euro/Kg'
030900070205     c                   endif
031000060707      *
031100060707      * Impostazione LIMITE DI RISARCIMENTO
031200060728     c                   if             TPTvvm =  'EUR'
031300060728     c                             or   TPTvvm =  *blanks
031400070205     c                   if        I45lin = '3'
031500070205     c                   eval      MAClim =  %trim(%editc(TPTvlm:'N'))
031600070205     c                                    +  ' euros/Kg'
031700070205     c                   else
031800060725     c                   eval      MAClim =  %trim(%editc(TPTvlm:'N'))
031900060725     c                                    +  ' Euro/Kg'
032000070205     c                   endif
032100060725     c                   else
032200060725     c                   eval      MAClim =  %trim(%editc(TPTvlm:'N'))
032300060728     c                                    +  ' '
032400060728     c                                    +  %trim(TPTvvm) + '/kg'
032500060725     c                   endif
032600070122      *
032700070122      * FIRMA "Per accettazione del mandato (Resp. Bartolini)"  - - - *
032800070122      * (per ora "chiodata" a quella di Marinoni Davide)
032900070122     c                   eval      §MACfir = 'FIRMAMAD'
033000060707      *
033100060707     c                   ENDSR
033200070208
033300070208      *---------------------------------------------------------------*
033400070208      *?Reperimento dati da tariffa                                  ?*
033500070208      *---------------------------------------------------------------*
033600070208     c     Rep_Tariffa   BEGSR
033700070208      *
033800070208      * Impostazione codice cliente da utilizzare
033900070208     c                   eval      WWWksc = I45ksc
034000070208      *
034100070208      * Apertura archivi
034200070208     c                   open      TNTAM01L
034300070208     c                   open      TITPT01L
034400070208     c                   open      TITPD01L
034500070208      *
034600070208      * Decodifica cliente
034700070208     c                   exsr      Decod_Cli
034800070208      *
034900070208      * Reperimento data decorrenza tariffa
035000070208     c     K03TAM01      chain     TNTAM000
035100070208     c                   if        NOT  %found(TNTAM01L)
035200070208     c                             or   TAMatb <> *blanks
035300070208     c***                clear                   TAMddt
035400070208     c                   eval      $Err   =  *on
035500070208     c                   leavesr
035600070208     c                   endif
035700070208      *
035800070208      * Reperimento LIMITE DI RISARCIMENTO
035900070208     c     K04TPx01      chain     TITPT000
036000070208     c                   if        NOT  %found(TITPT01L)
036100070208     c                             or   TPTatb <> *blanks
036200070208     c***                clear                   TPTvlm
036300070208     c***                clear                   TPTvvm
036400070208     c                   eval      $Err   =  *on
036500070208     c                   leavesr
036600070208     c                   endif
036700070208      *
036800070208      * Reperimento COSTO (importo tariffa)
036900070208     c     K04TPx01      chain     TITPD000
037000070208     c                   if        NOT  %found(TITPD01L)
037100070208     c                             or   TPDatb <> *blanks
037200070208     c***                clear                   TPDitr
037300070208     c                   eval      $Err   =  *on
037400070208     c                   leavesr
037500070208     c                   endif
037600070208      *
037700070208     c                   ENDSR
037800070208
037900070208      *---------------------------------------------------------------*
038000070208      *?Reperimento dati da offerta                                  ?*
038100070208      *---------------------------------------------------------------*
038200070208     c     Rep_Offerta   BEGSR
038300070208      *
038400070208      * Apertura archivi
038500110103     c**                 if        I45ast = 'V'
038600110103     c**                 open      TNVIS05L
038700110103     c**                 endif
038800091022     c                   if        I45ast = 'T'
038900091022     c                   open      Tivis05l
039000091022     c                   endif
039100070208     c                   open      TFACO00F
039200070208     c                   open      TFIND00F
039300070208     c                   open      TNOFM01L
039400070208     c                   open      TIOPT01L
039500070208     c                   open      TIOPD01L
039600070208      *
039700070208      * Impostazione iniziale del codice utente (fisso)
039800070208     c                   eval      ACOkut = 1
039900070208      *
040000070208      * Reperimento cliente da visita
040100110103     c**                 if        i45ast = 'V'
040200110103     c**   I45nrv        chain     TNVIS000
040300110103     c**                 endif
040400091022     c                   if        i45ast = 'T'
040500091022     c     I45nrv        chain     TiVIS000
040600091022     c                   endif
040700091022     c                   if        NOT  %found
040800070208     c                   eval      $Err   = *on
040900070208     c                   leavesr
041000070208     c                   endif
041100070208      *
041200070208      * Decodifica cliente
041300070208      * (se cliente nuovo leggo nel file anagrafica visite)
041400070208     c                   eval      WWWksc = VISnrv
041500070208     c     K03ACO01      chain     TFACO000
041600070208     c                   if        %found(TFACO00F)
041700070208     c     K03ACO01      chain     TFIND000
041800070208     c                   endif
041900070208      * (se non trovato cerco nell'anagrafica clienti)
042000070208     c                   if        NOT  %found(TFACO00F)  or
042100070208     c                             NOT  %found(TFIND00F)
042200070208     c                   eval      WWWksc = VISksc
042300070208     c                   exsr      Decod_Cli
042400070208     c                   endif
042500070208      *
042600070208      * Reperimento data decorrenza tariffa
042700080709     c     K03OFM01      chain     TNOFM000
042800070208     c                   if        NOT  %found(TNOFM01L)
042900070208     c                             or   TAMatb <> *blanks
043000070208     c***                clear                   TAMddt
043100070208     c                   eval      $Err   =  *on
043200070208     c                   leavesr
043300070208     c                   endif
043400070208      *
043500070208      * Reperimento LIMITE DI RISARCIMENTO
043600080709     c     K04OPx01      chain     TIOPT000
043700070208     c                   if        NOT  %found(TIOPT01L)
043800070208     c                             or   TPTatb <> *blanks
043900070208     c***                clear                   TPTvlm
044000070208     c***                clear                   TPTvvm
044100070208     c                   eval      $Err   =  *on
044200070208     c                   leavesr
044300070208     c                   endif
044400070208      *
044500070208      * Reperimento COSTO (importo tariffa)
044600080709     c     K04OPx01      chain     TIOPD000
044700070208     c                   if        NOT  %found(TIOPD01L)
044800070208     c                             or   TPDatb <> *blanks
044900070208     c***                clear                   TPDitr
045000070208     c                   eval      $Err   =  *on
045100070208     c                   leavesr
045200070208     c                   endif
045300070208      *
045400070208     c                   ENDSR
045500070208
045600070208      *---------------------------------------------------------------*
045700070208      *?Decodifica cliente da CNACO/CNIND/CNCLP/FNCLS                ?*
045800070208      *---------------------------------------------------------------*
045900070208     c     Decod_Cli     BEGSR
046000070208      *
046100070208     c                   reset                   TIBS69ds
046200070208     c                   clear                   ds_CNACO
046300070208     c                   clear                   ds_CNIND
046400070208     c                   clear                   ds_CNCLP
046500070208     c                   clear                   ds_FNCLS
046600070208     c                   eval      I69kac =  WWWksc
046700070208     c                   eval      I69kin =  WWWksc
046800070208     c                   call      'TIBS69R'
046900070208     c                   parm                    TIBS69ds
047000070208     c                   parm                    ds_CNACO
047100070208     c                   parm                    ds_CNIND
047200070208     c                   parm                    ds_CNCLP
047300070208     c                   parm                    ds_FNCLS
047400070208     c***                if        O69err =  *on
047500070208     c*** NO !!!         eval      $Err   =  *on
047600070208     c*** NO !!!         leavesr
047700070208     c***                endif
047800070208      *
047900070208     c                   ENDSR
048000070208
048100070208** - $Cmd
048200070208OVRPRTF FILE(TNTA59P) COPIES(
