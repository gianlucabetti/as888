000100060707      *---------------------------------------------------------------*
000200060918      *? Stampa lettera  "Mandato ad assicurare per conto - AC PLUS" ?*
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
001900060724     fTNTAM01L  if   e           k disk
002000060725     fTITPD01L  if   e           k disk
002100060725     fTITPT01L  if   e           k disk
002200060724     fAZORG01L  if   e           k disk
002300060918     fTABEL00F  if   e           k disk
002400060724      *
002500060918     fTNTA59P2  o    e             printer
002600060707
002700060707      *---------------------------------------------------------------*
002800060707
002900060707      *
003000060707      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *?
003100060707      *
003200060707      *
003300060707      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *?
003400060707      *
003500060707      *
003600060707      *?  D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *?
003700060707      *
003800060707      * Parametri
003900060724     d KPJBA         e ds
004000060724     d FNLV45ds      e ds
004100060707      *
004200060707      * - Parametri x Controllo profilo utenti
004300060707     d TIBS34ds      e ds
004400060707      * - Ds di riferimento al file esterno AZUTE00F
004500060707     d AZUTEds       e ds                  extname(AZUTE00F)
004600060707      * - Ds per dati organigramma
004700060707     d dDATIUTE      e ds
004800060707      *
004900060707      * Decodifica cliente
005000060707     d TIBS69ds      e ds                  inz
005100060707     d  I69tla       e                     inz('L')
005200060707     d DS_cnaco      e ds                  extname(CNACO00F) inz
005300060707     d DS_cnind      e ds                  extname(CNIND00F) inz
005400060707     d DS_cnclp      e ds                  extname(CNCLP00F) inz
005500060707     d DS_fncls      e ds                  extname(FNCLS00F) inz
005600060918      *
005700060918      * Tab. "TR" = Tipi Tariffa
005800060918     d dsTR          e ds                  inz
005900060707      *
006000060707     d Status         sds
006100060707     d  SDSpgm           *proc
006200060707     d  SDSprm           *parms
006300060724      *
006400060724     d WLBdat          ds                  inz
006500060724     d  G02dat                 1      8  0 inz
006600060724     d  G02inv                 9     16  0 inz
006700060724     d  G02err                17     17    inz('3')
006800060724     d  G02tgi                18     22  0 inz
006900060707      *
007000060707      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *?
007100060707      *
007200060724      * - Flags
007300060724     d $Err            s              1    inz(*off)
007400060724      * - Parametri per QCMDEXC
007500060724     d Qcmd            s            395    inz
007600060724     d Qlen            s             15  5 inz(%len(Qcmd))
007700060724      * - Campi di comodo
007800060724     d WWWksc          s                   like(I45ksc)   inz
007900060918     d WWWftc          s                   like(TPDftc)   inz('C ')
008000060724      *
008100060724      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *?
008200060724      *
008300060918      * > TNTAM01L
008400060724     c     K03TAM01      klist
008500060724     c                   kfld                    WWWksc
008600060724     c                   kfld                    I45ctr
008700060724     c                   kfld                    I45prg
008800060918      * > TITPT01L / TITPD01L
008900060725     c     K04TPx01      klist
009000060725     c                   kfld                    WWWksc
009100060725     c                   kfld                    I45ctr
009200060725     c                   kfld                    I45prg
009300060725     c                   kfld                    WWWftc
009400060918      * > TABEL00F
009500060918     c     K03TABEL      klist
009600060918     c                   kfld                    TBLkut
009700060918     c                   kfld                    TBLcod
009800060918     c                   kfld                    TBLkey
009900060707
010000060707      *===============================================================*
010100060707      *?RIEPILOGO INDICATORI                                         ?*
010200060707      *---------------------------------------------------------------*
010300060707      *       -                                                       *
010400060707      *===============================================================*
010500060707
010600060707     c     *Entry        plist
010700060724     c                   parm                    KPJBA
010800060724     c                   parm                    FNLV45ds
010900060918      *
011000060918      * Impostazione campi "fissi"
011100060918     c                   eval      TBLkut =  1
011200060918     c                   eval      TBLcod =  'TR'
011300060803      *
011400060803      * Sistemazione eventuali parametri mancanti:
011500060803      * - P.O. di appartenenza
011600060803     c                   if        I45app =  *zeros
011700060803     c                   eval      I45app =  DUTpou
011800060803     c                   endif
011900060803      * - Lingua
012000060803     c                   if        I45lin =  *blanks
012100060803     c                   eval      I45lin =  'I'
012200060803     c                   endif
012300060724      *
012400060724     c                   reset                   $Err
012500060724      *
012600060724      * Impostazione iniziale del codice da utilizzare
012700060724sel 1c                   select
012800060724w   1c                   when      I45ast =  'C'
012900060724     c                   eval      WWWksc =  I45ksc
013000060724w   1c                   when      I45ast =  'V'
013100060724     c                   eval      WWWksc =  I45nrv
013200060724x   1c                   other
013300060724     c                   eval      $Err   =  *on
013400060724e   1c                   endsl
013500060724     c     $Err          cabeq     *on           Fine
013600060707      *
013700060707      * Pulizia del PRTF
013800060918     c                   clear                   TA59ACPLU1
013900060918     c                   clear                   TA59ACPLU2
014000060918     c*** no campi:      clear                   TA59ACPLU3
014100060918     c                   clear                   TA59ACPLU4
014200060918     c*** no campi:      clear                   TA59ACPLU5
014300060707      *
014400060707      * Impostazione dei campi in stampa
014500060707     c                   exsr      Rie_Out
014600060918     c     $Err          cabeq     *on           Fine
014700060707      *
014800060724      * Stampa
014900060731sel 1c                   select
015000060731      * - Italiano
015100061120w   1c                   when           I45lin = '1'
015200061120     c                             or   I45lin = 'I'
015300061120     c                             or   I45lin = *blanks
015400060918     c                   write     TA59ACPLU1
015500060918     c                   write     TA59ACPLU2
015600060918if  2c                   if        TPTvlm =  *zeros
015700060918     c                   write     TA59ACPLU3
015800060918x   2c                   else
015900060918     c                   write     TA59ACPLU4
016000060918e   2c                   endif
016100060918     c                   write     TA59ACPLU5
016200060731      * - Inglese
016300060918w   1c                   when      I45lin =  '2'
016400060918     c***                write     TA59ACPL21
016500060918     c***                write     TA59ACPL22
016600060918if  2c***                if        TPTvlm =  *zeros
016700060918     c***                write     TA59ACPL23
016800060918x   2c***                else
016900060918     c***                write     TA59ACPL24
017000060918e   2c***                endif
017100060918     c***                write     TA59ACPL25
017200060731      * - Francese
017300060918w   1c                   when      I45lin =  '3'
017400060918     c***                write     TA59ACPL31
017500060918     c***                write     TA59ACPL32
017600060918if  2c***                if        TPTvlm =  *zeros
017700060918     c***                write     TA59ACPL33
017800060918x   2c***                else
017900060918     c***                write     TA59ACPL34
018000060918e   2c***                endif
018100060918     c***                write     TA59ACPL35
018200061120      * - Tedesco
018300060918w   1c                   when      I45lin =  '4'
018400060918     c***                write     TA59ACPL41
018500060918     c***                write     TA59ACPL42
018600060918if  2c***                if        TPTvlm =  *zeros
018700060918     c***                write     TA59ACPL43
018800060918x   2c***                else
018900060918     c***                write     TA59ACPL44
019000060918e   2c***                endif
019100060918     c***                write     TA59ACPL45
019200060731      *
019300060731e   1c                   endsl
019400060707      *
019500060707      * Fine
019600060724     c     Fine          tag
019700060918     c                   eval      *inLR  =  *on
019800060707
019900060707      *---------------------------------------------------------------*
020000060707      *?OPERAZIONI INIZIALI                                          ?*
020100060707      *---------------------------------------------------------------*
020200060707     c     *InzSR        BEGSR
020300060707      *
020400060707      * Reperisco dati societari
020500060707     c     *dtaara       define    §azute        AZUTEds
020600060707     c     *dtaara       define    §datiute      dDATIUTE
020700060707     c                   in(E)     *dtaara
020800060707     c                   if        %ERROR or RSUT = *blanks
020900060707     c                   clear                   Tibs34Ds
021000060707     c                   call      'TIBS34R'
021100060707     c                   parm                    Tibs34Ds
021200060707     c                   in        *dtaara
021300060707     c                   endif
021400060707      *
021500060707     c                   ENDSR
021600060707
021700060707      *---------------------------------------------------------------*
021800060707      *?IMPOSTAZIONE DEI CAMPI IN STAMPA                             ?*
021900060707      *---------------------------------------------------------------*
022000060707     c     Rie_Out       BEGSR
022100060707      *
022200060707      * Decodifica cliente
022300060707     c                   reset                   TIBS69ds
022400060707     c                   clear                   ds_CNACO
022500060707     c                   clear                   ds_CNIND
022600060707     c                   clear                   ds_CNCLP
022700060707     c                   clear                   ds_FNCLS
022800060724     c                   eval      I69kac =  WWWksc
022900060724     c                   eval      I69kin =  WWWksc
023000060707     c                   call      'TIBS69R'
023100060707     c                   parm                    TIBS69ds
023200060707     c                   parm                    ds_CNACO
023300060707     c                   parm                    ds_CNIND
023400060707     c                   parm                    ds_CNCLP
023500060707     c                   parm                    ds_FNCLS
023600060918     c***                if        O69err =  *on
023700060918     c*** NO !!!         eval      $Err   =  *on
023800060918     c***                leavesr
023900060918     c***                endif
024000060724      *
024100060728      * Decodifica P.O.
024200060728     c     I45app        chain     AZORG
024300060728     c                   if        NOT  %found(AZORG01L)
024400060728     c                             or   ORGfva <> *blanks
024500060918     c***                clear                   ORGdes
024600060918     c                   eval      $Err   =  *on
024700060918     c                   leavesr
024800060728     c                   endif
024900060728      *
025000060728      * Reperimento data decorrenza tariffa
025100060728     c     K03TAM01      chain     TNTAM000
025200060728     c                   if        NOT  %found(TNTAM01L)
025300060728     c                             or   TAMatb <> *blanks
025400060918     c***                clear                   TAMddt
025500060918     c                   eval      $Err   =  *on
025600060918     c                   leavesr
025700060728     c                   endif
025800060728      *
025900060728      * Reperimento COSTO (importo tariffa)
026000060728     c     K04TPx01      chain     TITPD000
026100060728     c                   if        NOT  %found(TITPD01L)
026200060728     c                             or   TPDatb <> *blanks
026300060918     c***                clear                   TPDitr
026400060918     c                   eval      $Err   =  *on
026500060918     c                   leavesr
026600060728     c                   endif
026700060728      *
026800060728      * Reperimento LIMITE DI RISARCIMENTO
026900060728     c     K04TPx01      chain     TITPT000
027000060728     c                   if        NOT  %found(TITPT01L)
027100060728     c                             or   TPTatb <> *blanks
027200060918     c***                clear                   TPTtma
027300060918     c***                clear                   TPTvlm
027400060918     c***                clear                   TPTvvm
027500060918     c                   eval      $Err   =  *on
027600060918     c                   leavesr
027700060728     c                   endif
027800060918      * - tipo mandato NON reale
027900060918     c                   if        TPTtma <> *blanks
028000060918     c                   eval      $Err   =  *on
028100060918     c                   leavesr
028200060918     c                   endif
028300060918      *
028400060918      * Tipo Tariffa (se valore merce > *zero)
028500060918     c                   clear                   dsTR
028600060918     c                   IF        TPTvlm <> *zeros
028700060918     c                   movel(p)  TPTtpg        TBLkey
028800060918     c     K03TABEL      chain     TABEL
028900060918     c                   if        NOT  %found(TABEL00F)
029000060918     c                             or   TBLflg <> *blanks
029100060918     c                   eval      $Err   =  *on
029200060918     c                   leavesr
029300060918     c                   endif
029400060918     c                   movel     TBLuni        dsTR
029500060918     c                   ENDIF
029600060707      *
029700060804      * "TESTATA" LETTERA - - - - - - - - - - - - - - - - - - - - - - *
029800060804      *
029900060804      * Impostazione dati del cliente
030000060707     c                   eval      MACrsc =  ACOrag
030100060707     c                   eval      MACinc =  INDvia
030200060707     c                   eval      MACcac =  %trim(%editw(INDcap:'0     '))
030300060707     c                   eval      MACloc =  INDcit
030400060707     c                   if        INDprv <> *blanks
030500060918     c*** no parentesi:  eval      MACpnc =  '(' + INDprv + ')'
030600060918     c                   eval      MACpnc =  INDprv
030700060707     c                   else
030800060707     c                   eval      MACpnc =  INDsta
030900060707     c                   endif
031000060707      *
031100060707      * Decodifica P.O.
031200060724     c                   eval      MAClpo =  ORGdes
031300060804      *
031400060804      * "CORPO" LETTERA - - - - - - - - - - - - - - - - - - - - - - - *
031500060707      *
031600060724      * Impostazione data decorrenza tariffa
031700060724     c                   reset                   WLBdat
031800060918     c                   eval      G02inv =  TAMddt
031900060918     c*** già così:      eval      G02err =  '3'
032000060724     c                   call      'XSRDA8'
032100060724     c                   parm                    WLBdat
032200060918     c                   eval      MACdta =  G02dat
032300060918      *
032400060918      * Codice cliente
032500060918     c                   eval      MACksc =  WWWksc
032600060918      *
032700060918      * Codice tariffa
032800060918     c                   eval      MACctr =  I45ctr
032900060918      *
033000060918      * Tipo applicazione
033100060918     c                   select
033200060918     c                   when      TPTapl =  'P'
033300060918     c                   eval      MACapl =  'in partenza    '
033400060918     c                   when      TPTapl =  'A'
033500060918     c                   eval      MACapl =  'in arrivo      '
033600060918     c                   when      TPTapl =  *blanks
033700060918     c                   eval      MACapl =  'tutte          '
033800060918     c                   endsl
033900060707      *
034000060707      * Impostazione LIMITE DI RISARCIMENTO
034100060918     c                   IF        TPTvlm <> *zeros
034200060918      *
034300060728     c                   if             TPTvvm =  'EUR'
034400060728     c                             or   TPTvvm =  *blanks
034500060918     c                   eval      MAClim =  'Euro'
034600060918     c                                    +  ' '
034700060918     c                                    +  %trim(%editc(TPTvlm:'N'))
034800060725     c                   else
034900060918     c                   eval      MAClim =  TPTvvm
035000060918     c                                    +  ' '
035100060918     c                                    +  %trim(%editc(TPTvlm:'N'))
035200060725     c                   endif
035300060918     c                   eval      MACdtr =  §TRde3
035400060918      *
035500060918     c                   ENDIF
035600060707      *
035700060707     c                   ENDSR
