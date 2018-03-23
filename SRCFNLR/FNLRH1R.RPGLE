000100070724      * FNLRH1R *-----------------------------------------------------*
000200070724      *?Lista spedizioni EXPORT in arrivo                     -BATCH-?*
000300070719      *---------------------------------------------------------------*
000400070719
000500070723     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600070719
000700070719      *---------------------------------------------------------------*
000800070719
000900070719     fFNARB54L  if   e           k disk
001000070720      *
001100070725     fFNLBL01L  if   e           k disk
001200070725     fFNART01L  if   e           k disk
001300070720     fAZCLN01L  if   e           k disk
001400070720     fTABEL00F  if   e           k disk
001500070720     fAZORG01L  if   e           k disk
001600070719      *
001700070720     fPRTF198   o    f  198        printer oflind(*inOF)
001800070719
001900070719      *---------------------------------------------------------------*
002000070719
002100070719      *
002200070719      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002300070719      *
002400070720      *
002500070719      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002600070719      *
002700070720      * - Testate in stampa
002800070720     d $Txt            s             66    dim( 9)  ctdata  perrcd(1)
002900070725     d $Psgn           s             10    dim(18)  inz
003000070719      *
003100070719      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
003200070719      *
003300070720      * - Parametri ricevuti
003400070719     d KPJBA         e ds
003500070724     d FNLRH1ds      e ds                  inz
003600070719      *
003700070719      * - Parametri x Controllo profilo utenti
003800070720     d TIBS34ds      e ds                  inz
003900070719      * - Ds di riferimento al file esterno AZUTE00F
004000070720     d AZUTEds       e ds                  inz  extname(AZUTE00F)
004100070719      * - Ds per dati organigramma
004200070720     d dDatiUte      e ds                  inz
004300070720      *
004400070724      * - Aggiunta/Sottrazione di n giorni lavorativi ad una data
004500070720     d TRUL52ds      e ds                  inz
004600070720     d   D52gio      e                     inz(1)
004700070720     d   D52sgn      e                     inz('-')
004800070719      *
004900070719      * - Reperimento Terminal
005000070719     d FNLV54ds      e ds                  inz
005100070719     d  I54tla       e                     inz('L')
005200070719     d  I54tpt       e                     inz('P')
005300070720      *
005400070720      * - Reperimento dati foglio da spunta
005500070720     d FNLV53ds      e ds                  inz
005600070719      *
005700070719      * - Caricamento schiera filiali
005800070719     d TRUL06ds      e ds                  inz
005900070720     d  $£1                    1     90  0 inz  dim(30)                         P.O. COMODO
006000070719      *
006100070720      * - Tabella "1Y" = Simboli calendario
006200070719     d ds1Y          e ds                  inz
006300070725      * - Tabella "3Y" = Codici bolla
006400070725     d ds3A          e ds                  inz
006500070720      *
006600070720      * - Descrizione 143 dell'organigramma
006700070720     d Og143         e ds                  inz
006800070719      *
006900070719     d Status         sds
007000070719     d  SDSpgm           *proc
007100070720      *
007200070720      * - Calendario Bartolini:
007300070720      * - - Flags festività mattino
007400070720     d CLNmat          ds
007500070720     d  $MAT                   1     31    dim(31)
007600070725     *** * - - Flags festività pomeriggio
007700070725     ***d CLNpom          ds
007800070725     ***d  $POM                   1     31    dim(31)
007900070720      *
008000070720      * - Parametri per pgm. controllo/inversione data
008100070720     d WLBdat          ds                  inz
008200070720     d  G08dat                        8  0 inz
008300070720     d  G08inv                        8  0 inz
008400070720     d  G08err                        1    inz('3')
008500070720     d  G08tgi                        5  0 inz
008600070719      *
008700070719      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
008800070719      *
008900070719      * - Flags booleani
009000070719     d $RecOK          s              1    inz(*off)
009100070725     d $Mamma          s              1    inz(*off)
009200070725     d $Full           s              1    inz(*off)
009300070725      *
009400070725      * - Indici di schiera
009500070725     d xx              s              3  0 inz
009600070720      *
009700070720      * - Variabili per controllo rutture
009800070720     d SAVlna          s                   inz  like(ARBlna)
009900070720     d SAVtfp          s                   inz  like(ARBtfp)
010000070720     d SAVnfv          s                   inz  like(ARBnfv)
010100070720      *
010200070720      * - Variabili di stampa
010300070725     d WPtime          s              6  0 inz
010400070723     d WPdfa           s              8  0 inz
010500070720     d WPdsp           s              8  0 inz
010600070725     d WPsgn1          s             60    inz
010700070725     d WPsgn2          s             60    inz
010800070725     d WPsgn3          s             60    inz
010900070725     d WPsgn4          s             60    inz
011000070719      *
011100070719      * - Variabili di comodo
011200070723     d FGSogas         s                   inz  like(§OGas)
011300070723     d wDtaSpeLim      s                   inz  like(D52dfi)
011400070724     d w_Dfa           s                   inz  like(DH1dfa)
011500070719     d wData_Iso       s               d   inz  datfmt(*iso)
011600070720     d wGiorno         s              2  0 inz
011700070719      *
011800070719      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
011900070719      *
012000070719      * - FNARB54L
012100070720     c     k02arb54      klist
012200070724     c                   kfld                    DH1lna
012300070723     c                   kfld                    ARBdcm
012400070725      * - FIART01L
012500070725     c     k04art01a     klist
012600070725     c                   kfld                    ARBaas
012700070725     c                   kfld                    ARBlnp
012800070725     c                   kfld                    ARBnrs
012900070725     c                   kfld                    ARBnsp
013000070725     c     k04art01b     klist
013100070725     c                   kfld                    LBLaap
013200070725     c                   kfld                    LBLlpp
013300070725     c                   kfld                    LBLnrp
013400070725     c                   kfld                    LBLnsp
013500070720      * - AZCLN01L
013600070720     c     k04cln01      klist
013700070720     c                   kfld                    CLNtfp
013800070720     c                   kfld                    CLNtfa
013900070720     c                   kfld                    CLNann
014000070720     c                   kfld                    CLNmes
014100070720      * - TABEL00F
014200070720     c     k03tabel      klist
014300070720     c                   kfld                    TBLkut
014400070720     c                   kfld                    TBLcod
014500070720     c                   kfld                    TBLkey
014600070719
014700070719      *---------------------------------------------------------------*
014800070719      *? RIEPILOGO INDICATORI                                        ?*
014900070719      *---------------------------------------------------------------*
015000070719      * 10   - Comodo                                                 *
015100070719      *---------------------------------------------------------------*
015200070719
015300070719     c     *Entry        plist
015400070719     c                   parm                    KPJBA
015500070719      *
015600070719      * Operazioni iniziali
015700070719     c                   exsr      sr_Start
015800070719      *
015900070719      * Ciclo di elaborazione
016000070723      * x bolle da consegnare
016100070723     c                   clear                   ARBdcm
016200070720     c     k02arb54      setll     FNARB000
016300070720     c     k02arb54      reade     FNARB000
016400070720      *
016500070719do  1c                   dow       NOT  %eof(FNARB54L)
016600070720      *
016700070719     c                   exsr      Elab_FNARB
016800070720     c     k02arb54      reade     FNARB000
016900070720      *
017000070719e   1c                   enddo
017100070719      *
017200070719      * Fine
017300070719     c                   exsr      sr_End
017400070719     c                   eval      *inLR   = *on
017500070719      *
017600070719      *---------------------------------------------------------------*
017700070719      *?Operazioni iniziali                                          ?*
017800070719      *---------------------------------------------------------------*
017900070719     c     sr_Start      BEGSR
018000070719      *
018100070724     c                   movel     KPJBU         FNLRH1ds
018200070719      *
018300070719      * Reperimento dati job
018400070719     c     *dtaara       define    §azute        AZUTEds
018500070719     c     *dtaara       define    §datiute      dDATIUTE
018600070719      *
018700070719     c                   in(E)     *dtaara
018800070719     c                   IF        %ERROR or RSUT = *blanks
018900070719     c                   clear                   Tibs34Ds
019000070719     c                   call      'TIBS34R'
019100070719     c                   parm                    Tibs34Ds
019200070719     c                   in        *dtaara
019300070719     c                   ENDIF
019400070720      *
019500070720      * Impostazione campi fissi
019600070720     c                   eval      TBLkut  = 1
019700070720     c                   time                    WPtime
019800070720      *
019900070724      * - Aggiunta/Sottrazione (D52SGN = "-") di n giorni lavorativi
020000070724      *   (D52GIO = 1) alla data del foglio arrivi (D52DIN = DH1DFA),
020100070724      *   per ottenere la data spedizione limite (D52DFI),
020200070724      *   corrispondente al giorno lavorativo precedente la data del
020300070724      *   foglio arrivi.
020400070720     c                   reset                   TRUL52ds
020500070724     c                   move      DH1dfa        D52din
020600070720     c                   call      'TRUL52R '
020700070720     c                   parm                    TRUL52ds
020800070723     c                   move      D52dfi        wDtaSpeLim
020900070719      *
021000070719      * Caricamento tabella filiali gestite £1
021100070719      * - reperimento terminal-partenza della linea di arrivo
021200070719     c                   reset                   FNLV54ds
021300070724     c                   eval      I54lin = DH1fgs
021400070724     c                   eval      I54drf = DH1dfa
021500070719     c                   call      'FNLV54R'
021600070719     c                   parm                    FNLV54ds
021700070724     c                   move      O54aslin      FGSogas
021800070719      * - caricamento schiera filiali
021900070719     c                   clear                   TRUL06ds
022000070719     c                   eval      D06cod = '£1'
022100070719     c                   movel(p)  O54tfp        D06key
022200070719     c                   movel(p)  TRUL06ds      KPJBU
022300070719     c                   call      'TRUL06R'
022400070719     c                   parm                    KPJBA
022500070719     c                   movel     KPJBU         TRUL06ds
022600070720      *
022700070720      * Reperimento tab. "1Y"
022800070720     c                   clear                   ds1Y
022900070720     c                   clear                   CLNtfp
023000070724     c                   z-add     DH1lna        CLNtfa
023100070724     c     *iso          move      DH1dfa        wData_Iso
023200070720     c                   extrct    wData_Iso:*y  CLNann
023300070720     c                   extrct    wData_Iso:*m  CLNmes
023400070720     c                   extrct    wData_Iso:*d  wGiorno
023500070720     c     k04cln01      chain     AZCLN000
023600070720if  1c                   IF             %found(AZCLN01L)
023700070725     ***c                             and  CLNatb  = *blanks
023800070725     ***c                             and ($MAT(wGiorno) <> *blanks
023900070725     ***c                              or  $POM(wGiorno) <> *blanks)
024000070725     c                             and  $MAT(wGiorno) <> *blanks
024100070720     c                   movel     '1Y'          TBLcod
024200070720     c                   movel(p)  $MAT(wGiorno) TBLkey
024300070720     c     k03tabel      chain     TABEL
024400070720if  2c                   if        %found(TABEL00F)
024500070720     c                   movel     TBLuni        ds1Y
024600070720e   2c                   endif
024700070720e   1c                   ENDIF
024800070720      *
024900070720      * Stampa 1ª testata
025000070720     c                   reset                   WLBdat
025100070724     c                   eval      G08inv  =  DH1dfa
025200070720     c*** già così:      eval      G08err  =  '3'
025300070720     c                   call      'XSRDA8'
025400070720     c                   parm                    WLBdat
025500070723     c                   eval      WPdfa   =  G08dat
025600070720     c                   except    PRTtxt
025700070724     c                   eval      *inOF   = *off
025800070719      *
025900070719     c                   ENDSR
026000070719      *
026100070719      *---------------------------------------------------------------*
026200070719      *?Operazioni finali                                            ?*
026300070719      *---------------------------------------------------------------*
026400070719     c     sr_End        BEGSR
026500070719      *
026600070720     c                   except    PRTend
026700070720      *
026800070719     c                   clear                   FNLV53ds
026900070719     c                   eval      D53tla  = 'C'
027000070719     c                   call      'FNLV53R'
027100070719     c                   parm                    FNLV53ds
027200070719      *
027300070719     c                   ENDSR
027400070719      *
027500070719      *---------------------------------------------------------------*
027600070719      *?Elaborazione singola spedizione                              ?*
027700070719      *---------------------------------------------------------------*
027800070719     c     Elab_FNARB    BEGSR
027900070719      *
028000070719     c                   eval      $RecOK  = *off
028100070719      *
028200070720      * Selezione record
028300070719     c                   exsr      sr_Selez
028400070720     c                   if        $RecOK  = *off
028500070720     c                   leavesr
028600070720     c                   endif
028700070720      *
028800070720      * Stampa record selezionato
028900070720     c                   exsr      sr_Stampa
029000070719      *
029100070719     c                   ENDSR
029200070719      *
029300070719      *---------------------------------------------------------------*
029400070719      *?Selezione singolo record da file FNARB                       ?*
029500070719      *---------------------------------------------------------------*
029600070719     c     sr_Selez      BEGSR
029700070719      *
029800070719     c                   eval      *in10   = *off
029900070720     c     ARBlnp        lookup    $£1                                    10
030000070719      *
030100070719sel 1c                   SELECT
030200070719      *
030300070720      *?Zona esterna al range richiesto?
030400070724w   1c                   WHEN           ARBznc  < DH1znd
030500070724     c                             or   ARBznc  > DH1zna
030600070719      *
030700070720      *?BOLLA LOCALE:?
030800070720w   1c                   WHEN      *in10
030900070724      * -?SOLO SE?la data spedizione coincide con la data spedizione
031000070724      *   limite, calcolata come giorno lavorativo precedente la data
031100070724      *   del foglio arrivi
031200070724if  2c                   if        (ARBaas*10000+ARBmgs) = wDtaSpeLim
031300070720     c                   exsr      sr_CtrTFA
031400070720e   2c                   endif
031500070719      *
031600070720      *?BOLLA NON LOCALE:?
031700070719w   1c                   OTHER
031800070719      * - richiama FNLV53R per cercare l'esatto foglio di abbinamento
031900070719      *   (solo a rottura di LNA, TFP, NFV)
032000070723if  2c     arblna        ifne      savlna
032100070723     c     arbtfp        orne      savtfp
032200070723     c     arbnfv        orne      savnfv
032300070723     c                   z-add     arblna        savlna
032400070723     c                   z-add     arbtfp        savtfp
032500070723     c                   z-add     arbnfv        savnfv
032600070719     c                   clear                   FNLV53ds
032700070723     c                   movel     'A'           d53tfo
032800070723     c                   z-add     simfel        d53fel
032900070723     c                   z-add     arbnfv        d53nfv
033000070724     c                   move      arbtfp        d53fgs
033100070723     c                   movel     'B'           d53abb
033200070723     c                   z-add     arblna        d53lna
033300070723     c                   z-add     arbtfa        d53tfa
033400070723     c                   call      'FNLV53R'
033500070723     c                   parm                    fnlv53ds
033600070723e   2c                   endif
033700070723      *
033800070724      * - bolla partita su foglio viaggio abbinato a foglio arrivi richiesto
033900070724      *   e filiale invio foglio = FGS
034000070719      *   => bolla da elencare
034100070724w   2c     d53err        Ifeq      *blanks
034200070724     c     d53nfa        andeq     DH1nfa
034300070724     c     d53lai        andeq     DH1fgs
034400070724     c     d53dfa        andle     DH1dfa
034500070719     c                   eval      $RecOK  = *on
034600070724e   2c                   endif
034700070719      *
034800070719e   1c                   ENDSL
034900070719      *
035000070719     c                   ENDSR
035100070720      *
035200070720      *---------------------------------------------------------------*
035300070720      *?Controllo Terminal di Arrivo                                 ?*ta
035400070720      *---------------------------------------------------------------*ta
035500070720     c     sr_CtrTFA     BEGSR
035600070720      *
035700070724if  3c     arbtfa        ifeq      DH1fgs
035800070720     c                   eval      $RecOK  = *on
035900070723x   3c                   else
036000070720     c     ARBtfa        lookup    $£1                                    10
036100070723if  4c     *in10         ifeq      *off
036200070724     c     arblna        andeq     DH1fgs
036300070723     c     arbtfa        chain     Azorg01l
036400070723if  5c                   if        %found(Azorg01l)
036500070724     c                   movel     Orgde3        Og143
036600070720x   5c                   else
036700070723     c                   clear                   §ogas
036800070720e   5c                   endif
036900070723if  5c     §ogas         ifne      fgsogas
037000070720     c                   eval      $RecOK  = *on
037100070720e   5c                   endif
037200070720e   4c                   endif
037300070723e   3c                   endif
037400070720      *
037500070720     c                   ENDSR
037600070719      *
037700070720      *---------------------------------------------------------------*
037800070720      *?Stampa singolo record da file FNARB                          ?*
037900070720      *---------------------------------------------------------------*
038000070720     c     sr_Stampa     BEGSR
038100070725      *
038200070725      * Caricamento elenco segnacolli
038300070725     c                   clear                   xx
038400070725     c                   clear                   $Psgn
038500070725     c                   clear                   WPsgn1
038600070725     c                   clear                   WPsgn2
038700070725     c                   clear                   WPsgn3
038800070725     c                   exsr      sr_SgnCl
038900070720      *
039000070720     c                   reset                   WLBdat
039100070725     c                   eval      G08inv  = (ARBaas*10000)+ARBmgs
039200070725     c*** già così:      eval      G08err  = '3'
039300070720     c                   call      'XSRDA8'
039400070720     c                   parm                    WLBdat
039500070725     c                   eval      WPdsp   = G08dat
039600070720      *
039700070720     c                   if        *inOF
039800070720     c                   except    PRTtxt
039900070720     c                   eval      *inOF   = *off
040000070720     c                   endif
040100070720      *
040200070720     c                   except    PRTdet
040300070725      *
040400070725      * Test completamento stampa elenco segnacolli
040500070725if  1c                   if        %eof(FNART01L)
040600070725     c                   leavesr
040700070725e   1c                   endif
040800070725      * Completamento stampa elenco segnacolli (se ce ne sono altri)
040900070725     c                   clear                   xx
041000070725     c                   clear                   $Psgn
041100070725     c                   clear                   WPsgn4
041200070725do  1c                   DOW       NOT  %eof(FNART01L)
041300070725     c                   add       1             xx
041400070725     c                   eval      $Psgn(xx) = %editc(ARTnsc:'K')
041500070725if  2c                   if        xx        = 6
041600070725if  3c                   if        *inOF
041700070725     c                   except    PRTtxt
041800070725     c                   eval      *inOF   = *off
041900070725e   3c                   endif
042000070725     c                   movea     $Psgn         WPsgn4
042100070725     c                   except    PRTsgn
042200070725     c                   clear                   xx
042300070725     c                   clear                   $Psgn
042400070725     c                   clear                   WPsgn4
042500070725e   2c                   endif
042600070725if  2c                   if        $Mamma    = *off
042700070725     c     k04art01a     reade     FNART000
042800070725x   2c                   else
042900070725     c     k04art01b     setll     FNART000
043000070725e   2c                   endif
043100070725e   1c                   ENDDO
043200070725      *
043300070725if  1c                   if        %eof(FNART01L)
043400070725     c                             and  xx > 1
043500070725     c                   movea     $Psgn         WPsgn4
043600070725     c                   except    PRTsgn
043700070725e   1c                   endif
043800070720      *
043900070720     c                   ENDSR
044000070725      *
044100070725      *---------------------------------------------------------------*
044200070725      *?Caricamento elenco dei segnacolli                            ?*
044300070725      *---------------------------------------------------------------*
044400070725     c     sr_SgnCl      BEGSR
044500070725      *
044600070725      * Reperimento tab. "3A" = Codici bolla
044700070725      *   per verificare se bolla "figlia"
044800070725     c                   clear                   ds3A
044900070725     c                   movel     '3A'          TBLcod
045000070725     c                   movel(p)  ARBcbo        TBLkey
045100070725     c     k03tabel      chain     TABEL
045200070725if  1c                   if        %found(TABEL00F)
045300070725     c                   movel     TBLuni        ds3A
045400070725e   1c                   endif
045500070725     c                   eval      $Mamma  = (§3Abcm = 'S')
045600070725      *
045700070725      * Se bolla con MAMMA: prendere segnacolli da bolla MAMMA
045800070725if  1c                   if        $Mamma  = *on
045900070725     c     k04art01a     chain     FNLBL000
046000070725if  2c                   if        NOT  %found(FNLBL01L)
046100070725     c                   eval      WPsgn1  = 'NON trovata bolla MAMMA!'
046200070725     c                   leavesr
046300070725e   2c                   endif
046400070725e   1c                   endif
046500070725      *
046600070725      * Ciclo di lettura per il recupero di tutti i numeri segnacollo
046700070725if  1c                   if        $Mamma  = *off
046800070725     c     k04art01a     setll     FNART000
046900070725     c     k04art01a     reade     FNART000
047000070725x   1c                   else
047100070725     c     k04art01b     setll     FNART000
047200070725     c     k04art01b     reade     FNART000
047300070725e   1c                   endif
047400070725      *
047500070725     c                   eval      $Full     = *off
047600070725do  1c                   DOW       NOT  %eof(FNART01L)
047700070725     c                             and  $Full   = *off
047800070725     c                   add       1             xx
047900070725     c                   eval      $Psgn(xx) = %editc(ARTnsc:'K')
048000070725     c                   eval      $Full     = (xx = %elem($Psgn))
048100070725if  2c                   if        $Mamma    = *off
048200070725     c     k04art01a     reade     FNART000
048300070725x   2c                   else
048400070725     c     k04art01b     setll     FNART000
048500070725e   2c                   endif
048600070725e   1c                   ENDDO
048700070725      *
048800070725      * Impostazione dei campi di stampa con la schiera:
048900070725      *   3 campi -- 6 elementi x campo
049000070725      *   oltre i 18 segnacolli: sempre solo WPsgn4 (dopo...)
049100070725     c                   movea     $Psgn( 1)     WPsgn1
049200070725     c                   movea     $Psgn( 7)     WPsgn2
049300070725     c                   movea     $Psgn(13)     WPsgn3
049400070725      *
049500070725     c                   ENDSR
049600070720
049700070720      *?SPOOL DI STAMPA ---------------------------------------------?*
049800070720
049900070720     oPRTF198   e            PRTtxt            2
050000070720     o                       RSUT
050100070720     o                       KNMUS            +   3
050200070720     o                       SDSpgm           +   3
050300070724     o                                        +  10 'ELENCO SPEDIZIONI-
050400070724     o                                               EXPORT IN ARRIVO'
050500070724     o                       *date         y  +  10
050600070720     o                       WPtime           +   3 '0 :  :  '
050700070720     o                                        +   3 'Pag.'
050800070720     o                       Page          z  +   0
050900070725     o          e            PRTtxt      0
051000070725     o                                        +  56 'ELENCO SPEDIZIONI-
051100070725     o                                               EXPORT IN ARRIVO'
051200070720      *
051300070720     o          e            PRTtxt      2
051400070724     o                                              'Foglio arrivi:'
051500070725     o                       DH1nfa        z  +   1
051600070720     o                                        +   1 'del'
051700070723     o                       WPdfa         y  +   1
051800070724     o                                        +   5 'Linea di arrivo:'
051900070724     o                       DH1lna           +   1 '0   '
052000070720     o                                        +   5 'Zone consegna: da-
052100070720     o                                              lla'
052200070724     o                       DH1znd           +   1 '0  '
052300070720     o                                        +   1 'alla'
052400070724     o                       DH1zna           +   1 '0  '
052500070725     o          e            PRTtxt      0
052600070725     o                                              'Foglio arrivi:'
052700070725     o                       DH1nfa           +   1
052800070725     o                                        +   1 'del'
052900070725     o                       WPdfa         y  +   1
053000070725     o                                        +   5 'Linea di arrivo:'
053100070725     o                       DH1lna           +   1 '0   '
053200070725     o                                        +   5 'Zone consegna: da-
053300070725     o                                              lla'
053400070725     o                       DH1znd           +   1 '0  '
053500070725     o                                        +   1 'alla'
053600070725     o                       DH1zna           +   1 '0  '
053700070720      *
053800070720     o          e            PRTtxt      2
053900070720     o                       $Txt( 1)
054000070723     o                       $Txt( 4)         +   0
054100070720     o                       $Txt( 7)         +   0
054200070725     o          e            PRTtxt      0
054300070725     o                       $Txt( 1)
054400070725     o                       $Txt( 4)         +   0
054500070725     o                       $Txt( 7)         +   0
054600070724     o          e            PRTtxt      1
054700070720     o                       $Txt( 2)
054800070720     o                       $Txt( 5)         +   0
054900070720     o                       $Txt( 8)         +   0
055000070725     o          e            PRTtxt      0
055100070725     o                       $Txt( 2)
055200070725     o                       $Txt( 5)         +   0
055300070725     o                       $Txt( 8)         +   0
055400070720      *
055500070724     o          e            PRTdet      2
055600070720     o                       ARBlnp
055700070724     o                       ARBnrs        z  +   1
055800070724     o                       ARBnsp        z  +   1
055900070720     o                       WPdsp         y  +   2
056000070724     o                       ARBcbo           +   6
056100070724     o                       ARBrsm           +   5
056200070720     o                       ARBrsd           +   2
056300070724     o                       ARBncl        j  +   1
056400070720     o                       ARBpkf        j  +   0
056500070720     o                       ARBvlb        j  +   1
056600070725     o                       WPsgn1           +   1
056700070724     o          e            PRTdet      1
056800070724     o                       ARBind           +  76
056900070725     o                       WPsgn2           +  27
057000070724     o          e            PRTdet      1
057100070724     o                       ARBcad           +  76
057200070725     o                       ARBlod           +   0
057300070725     o***                    ARBprd           +   1
057400070724     o                       ARBnzd           +   1
057500070725     o                       WPsgn3           +  14
057600070725     o          e            PRTsgn      1
057700070725     o                       WPsgn4           + 138
057800070720      *
057900070720     o          e            PRTend      2
058000070720     o                                        +  70 '***  Fine Lista  -
058100070720     o                                              ***'
058200070725     o          e            PRTend      0
058300070725     o                                        +  70 '***  Fine Lista  -
058400070725     o                                              ***'
058500070720
058600070720      *?SCHIERE A TEPO DI COMPILAZIONE ------------------------------?*
058700070720
058800070720** - $Txt -------------------------------------------------------*
058900070720  Spedizione        del     Cod.Bolla  Mittente                       1
059000070720--------------------------  ---------  ---------------------------    2
059100070720123 12 1234567  12/34/5678     12      123456789012345678901234567
059200070720          Destinatario                         Colli   Peso kg  Vo    4
059300070720--------  -----------------------------------  -----  --------  --    5
05940007072089012345  12345678901234567890123456789012345 12.345-123.456,7- 12
059500070725lume  Elenco segnacolli                                               7
059600070725----  ------------------------------------------------------------    8
0597000707253,45- 1.234.567-1.234.567-1.234.567-1.234.567-1.234.567-1.234.567-
