000100070104      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT) DBGVIEW(*SOURCE)
000200050811      *===============================================================*
000300051025      *?FIDN8BR * Lista Recupero C.A. affidate                       ?*
000400050811      *===============================================================*
000500050811
000600050811     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000601070104     h alwnull(*inputonly)
000700050811
000800050811      *---------------------------------------------------------------*
000900050811
001100050811     fTITAS30C  if   e           k disk
001200050811     fFNDCL01L  if   e           k disk
001300050811     fFNDET01L  if   e           k disk
001400050811     fFNDRA01L  if   e           k disk
001500050811      *
001600050811     fPRTF198   o    f  198        printer usropn
001700050811     f                                     oflind(*inOF)
001800050811
001900050811      *---------------------------------------------------------------*
002000050811
002100050812      *
002200050812      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002300050812      *
002400050812      * - Fasi C.A. fissate
002600050812     d C_DataLimite    c                   const(20050418)
002700050812      *
002800050811      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002900050811      *
003000050812      * - Causali Chiusura C.A. - di Pagamento
003100050812     d $CCHpag         s                   like(DCTcch)
003200050812     d                                     dim(10) ctdata  perrcd(10)
003300050811      * - Comandi da eseguire
003400050811     d $Cmd            s                   like(Qcmd)
003500050811     d                                     dim(02) ctdata perrcd(1)
003600050811      * - Testate (con esempio di stampa)
003700050914     d $Txt            s             66    dim(15) ctdata perrcd(1)
003800050811      *
003900050811      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004000050811      *
004100050811      * - Parametri
004200050811     d KPJBA         e ds
004300050831     d FIDN8Bds      e ds                  inz
004400050811      *
004500050811      * - Parametri x Controllo profilo utenti
004600050811     d TIBS34ds      e ds                  inz
004700050811      * - Ds di riferimento al file esterno AZUTE00F
004800050811     d AZUTEds       e ds                  extname(AZUTE00F)
004900050811      * - Ds per dati organigramma
005000050811     d DDatiUte      e ds
005100051121      * - Ds per DCTFLO
005200051121     d DDCT01        e ds
005201070104      * - Ds definizione file lettura FNDCT
005202070104     d Fndctds       e ds                  extname(FNDCT00F)
005300050811      *
005400050811      * - Parametri x Interrogazione tabelle
005500050811     d TIBS02DS      e ds                  inz
005600050811     d   T02mod      e                     inz('C')
005700050811     d   T02cod      e                     inz('TAD')
005800050811      * - Tab. "TAD"
005900050811     d dTAD          e ds                  inz
006000050811      *
006100050811     d Status         sds           333
006200050811     d   SDSpgm          *proc
006300050811      *
006400050811      * - Controllo/Inversione date
006500050811     d WLBdat          ds                  inz
006600050811     d   G08dat                       8  0 inz
006700050811     d   G08inv                       8  0 inz
006800050811     d   G08err                       1    inz('3')
006900050811     d   G08tgi                       5  0 inz
007000050811      *
007100050811      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
007200050811      *
007300050812      * - Flags
007400050811     d $OK             s              1    inz(*off)
007401070104     d $EOF            s              1    inz(*off)
007500050811      * - Parametri per QCMDEXC
007600050811     d Qcmd            s             80    inz
007700050811     d Qlen            s             15  5 inz(80)
007800050811      * - Campi di comodo
007900050811     d wDate           s              8  0 inz
008000050811     d wTime           s              6  0 inz
008100050811     d Data_Eur        s               d   inz  datfmt(*eur)
008200050812     d wTAD            s                   inz  like(DCTtad)
008300050811      * - Campi di stampa
008400050905     d pDCI            s              8  0 inz
008500050905     d pDCF            s              8  0 inz
008600050811     d pDCA            s              8  0 inz
008700050811     d pDSP            s              8  0 inz
008800050811     d pDEV            s              8  0 inz
008900050811     d pDRR            s              8  0 inz
009000050811     d pDVR            s              8  0 inz
009100050811     d pIPL            s              9  2 inz
009200050811     d pIPR            s              9  2 inz
009300050811     d pIRI            s              9  2 inz
009400050914     d pIPLt           s              9  2 inz
009500050914     d pIPRt           s              9  2 inz
009600050914     d pIRIt           s              9  2 inz
009700050811     d pTAD            s             25    inz
009800050811      *
009900050811      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
010000050811      *
010100050811      * - TITAS30C
010200050811     c     K04TAS30      klist
010300050811     c                   kfld                    DCTaas
010400050811     c                   kfld                    DCTlnp
010500050811     c                   kfld                    DCTnrs
010600050811     c                   kfld                    DCTnsp
010700050811      * - FNDET01L
010800050811     c     K02DET01      klist
010900050811     c                   kfld                    DCTaae
011000050811     c                   kfld                    DCTnev
011100050811      * - FNDRA01L / FNDCL01L
011200050811     c     K03DRA01      klist
011300050811     c                   kfld                    DCTaac
011400050811     c                   kfld                    DCTfil
011500050811     c                   kfld                    DCTnca
011600050811
011700050811      *---------------------------------------------------------------*
011800050811      *  RIEPILOGO INDICATORI                                         *
011900050811      *---------------------------------------------------------------*
012000050811      * 10    - Comodo                                                *
012100050811      *---------------------------------------------------------------*
012200050811
012300050811     c     *Entry        plist
012400050811     c                   parm                    KPJBA
012500050831     c                   movel     KPJBU         FIDN8Bds
012600050811      *
012700050811      * Operazioni iniziali
012800050811     c                   exsr      OpeIni
012801070104      *
012802070104     c                   exsr      OpenCursor
012803070104      *
012804070104     c                   dou       $Eof  = *on
012805070104     c                   exsr      ReadCursor
012806070104      *
012811070104      *
012812070104     c                   enddo
012813070104      *
012814070104     c                   exsr      CloseCursor
012815070104      *
015700050811      *
015800050811      * Operazioni finali
015900050811     c                   exsr      OpeFin
016000050811      *
016100050811      * Fine
016200050811     c                   movel     *on           *inLR
016300050811      *
016400050811      *---------------------------------------------------------------*
016500050811      * Operazioni Iniziali                                           *
016600050811      *---------------------------------------------------------------*
016700050811     c     OpeIni        BEGSR
016701070104     C
016702070104     C/EXEC SQL
016703070104     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
016704070104     C/END-EXEC
016800050811      *
016900050811      * Reperimento Dati del job (Utente/Operativi)
017000050811     c                   exsr      DatiJob
017100050811      *
017200050811      * Override al prtf
017300050811     c                   call      'QCMDEXC'
017400050811     c                   parm      $Cmd(1)       Qcmd
017500050811     c                   parm                    Qlen
017600050811      *
017700050811      * Apertura prtf
017800050811     c                   open      PRTF198
017900050811      *
018000050811      * Stampa della testata iniziale
018100050811     c                   move      *date         Data_eur
018200050811     c     *eur          movel     Data_eur      wDate
018300050811     c                   time                    wTime
018400050811      *
018500050905      * - impostazione date limite in formato gg/mm/aaaa
018600050905if  1c                   if        D8Bdci   > *zeros
018700050811     c                   reset                   WLBdat
018800050905     c                   eval      G08inv   = D8Bdci
018900050811     c                   call      'XSRDA8'
019000050811     c                   parm                    WLBdat
019100050811if  2c                   if        G08err   = *off
019200050905     c                   eval      pDCI     = G08dat
019300050811e   2c                   endif
019400050811e   1c                   endif
019500050905if  1c                   if        D8Bdcf   > *zeros
019600050905     c                   reset                   WLBdat
019700050905     c                   eval      G08inv   = D8Bdcf
019800050905     c                   call      'XSRDA8'
019900050905     c                   parm                    WLBdat
020000050905if  2c                   if        G08err   = *off
020100050905     c                   eval      pDCF     = G08dat
020200050905e   2c                   endif
020300050905e   1c                   endif
020400050905if  1c                   if        D8Bdcf   = *zeros
020500050905     c                   eval      D8Bdcf   = D8Bdci
020600050905e   1c                   endif
020700050914      *
020800050914      * Pulizia dei campi di totale
020900050914     c                   clear                   pIPLt
021000050914     c                   clear                   pIPRt
021100050914     c                   clear                   pIRIt
021200050811      *
021300050812      * Stampa parametri ricevuti e inizio lista
021400050811     c                   time                    Wtime
021500050812     c                   except    PRTtx1
021600050812     c                   except    PRTpar
021700050812     c                   except    PRTtx1
021800050812     c                   except    PRTtx2
021900050811     c                   eval      *inOF    = *off
022000050811      *
022100050811     c                   ENDSR
022200050811      *
022300050811      *---------------------------------------------------------------*
022400050811      * Reperimento Dati del job (Utente/Operativi)                   *
022500050811      *---------------------------------------------------------------*
022600050811     c     DatiJob       BEGSR
022700050811      *
022800050811     c     *dtaara       define    §azute        azuteds
022900050811     c     *dtaara       define    §datiute      ddatiute
023000050811      *
023100050811     c                   in(E)     *dtaara
023200050811     c                   IF        %ERROR or RSUT = *blanks
023300050811     c                   clear                   Tibs34Ds
023400050811     c                   call      'TIBS34R'
023500050811     c                   parm                    Tibs34Ds
023600050811     c                   in        *dtaara
023700050811     c                   ENDIF
023800050811      *
023900050811     c                   ENDSR
024000050811      *
024001070104      *****************************************************************
024002070104      **?Apertura cursore.                                          ?**
024003070104      *****************************************************************
024004070104     c     OpenCursor    BEGSR
024005070104      *
024006070104     c/exec sql
024007070104     c+ declare A1 cursor for
024008070104     c+ select  *
024009070104     c+ from    FNDCT00F
024010070104     c+ where   DCTatb = ' '
024012070104     c+   and   substr(DCTflo, 13, 1) = 'A'
024013070104     c+   and   DCTfca = 900
024014070104     c+   and   DCTnev = 0
024015070104     c+   and   DCTdch between :D8Bdci and :D8Bdcf
024016070104     c+ order   by     DCTdch, DCTfil, DCTaac, DCTnca
024017070104     c+ for     FETCH  only
024018070104     c/end-exec
024019070104      *
024020070104     c/exec sql
024021070104     c+ open A1
024022070104     c/end-exec
024023070104      *
024024070104     c                   ENDSR
024025070104      *
024026070104      *****************************************************************
024027070104      **?Chiusura cursore.                                          ?**
024028070104      *****************************************************************
024029070104     c     CloseCursor   BEGSR
024030070104      *
024031070104     c/exec sql
024032070104     c+ close A1
024033070104     c/end-exec
024034070104      *
024035070104     c                   ENDSR
024036070104      *
024037070104      *****************************************************************
024038070104      **?Lettura cursore.                                           ?**
024039070104      *****************************************************************
024040070104     c     ReadCursor    BEGSR
024041070104      *
024042070104     c                   clear                   FNDCTds
024043070104      *
024044070104     c/exec sql
024045070104     c+ fetch next from A1 into :FNDCTds
024046070104     c/end-exec
024047070104      *
024048070104sel 1c                   select
024049070104w   1c                   when      SQLcod = 100
024050070104     c                   eval      $EoF   = *on
024051070104w   1c                   when      SQLcod < 0
024053070104     c                   eval      $Eof   = *on
024054070104x   1c                   other
024055070104     c                   exsr      AltriDati
024056070104if  3c                   if        $OK      = *on
024057070104     c                   exsr      Stampa
024058070104e   3c                   endif
024060070104e   1c                   endsl
024061070104      *
024062070104     c                   ENDSR
024063070104      *
024100050811      *---------------------------------------------------------------*
024200050811      * Reperimento altri dati                                        *
024300050811      *---------------------------------------------------------------*
024400050811     c     AltriDati     BEGSR
024500050811      *
024600050811     c                   reset                   $OK
024700050812      *
024800050812      * Altre selezioni sulla C.A.:
024900050812      * - C.A. chiusa con causale di pagamento
025000050812     c                   eval      *in10    = *off
025100050812     c     DCTcch        lookup    $CCHpag                                10
025200050812if  1c                   if        NOT *in10
025300050812     c                   leavesr
025400050812e   1c                   endif
025500050811      *
025600050811      * Aggancio file TITAS30C
025700050811     c     K04TAS30      chain     TITAS30C
025800050811if  1c                   if        NOT %found(TITAS30C)
025900050811     c                   leavesr
026000050811e   1c                   endif
026100050811      *
026200050811      * Aggancio file FNDCL01L
026300050811     c     K03DRA01      chain     FNDCL000
026400050811if  1c                   if        NOT %found(FNDCL01L)
026500050811     c                             or  DCLatb <> *blanks
026600050811     c                             or  DCLfic <> 'C'
026700050811     c                   leavesr
026800050811e   1c                   endif
026900050811      *
027000050811      * Aggancio file FNDRA01L
027100050811     c     K03DRA01      chain     FNDRA000
027200050811if  1c                   if        NOT %found(FNDRA01L)
027300050811     c                             or  DRAatb <> *blanks
027400050811     c                   clear                   DRAdrr
027500050811     c                   clear                   DRAdvr
027600050811     c                   clear                   DRAdir
027700050811     c                   clear                   DRAiri
027800050812     c                   clear                   DRAesc
027900050811e   1c                   endif
028000050812      * - C.A. esclusa
028100050812if  1c                   if        DRAesc   = 'E'
028200050812     c                   leavesr
028300050812e   1c                   endif
028400050812      *
028500050812      * Selezioni a video
028600050812sel 1c                   select
028700051025      * - C.A. recuperate
028800051011w   1c                   when           D8Bse3 = *blanks
028900050812     c                             and  DRAiri > *zeros
029000050812     c                   leavesr
029100051011w   1c                   when           D8Bse3 = 'S'
029200050915     c                             and  DRAiri = *zeros
029300050915     c                   leavesr
029400051025      * - C.A. per le quali è già stato chiesto il recupero
029500051011w   1c                   when           D8Bse1 = 'S'
029600051011     c                             and  D8Bse2 = *blanks
029700050812     c                             and  DRAdrr > *zeros
029800050812     c                   leavesr
029900051025      * - C.A. per le quali si deve ancora chiedere il recupero
030000051011w   1c                   when           D8Bse1 = *blanks
030100051011     c                             and  D8Bse2 = 'S'
030200050812     c                             and  DRAdrr = *zeros
030300050812     c                   leavesr
030400050812e   1c                   endsl
030500050811      *
030600050811     c                   eval      $OK      = *on
030700050811      *
030800050811     c                   ENDSR
030900050811      *
031000050811      *---------------------------------------------------------------*
031100050811      * Stampa elenco C.A.                                            *
031200050811      *---------------------------------------------------------------*
031300050811     c     Stampa        BEGSR
031400050812      *
031500051017     c                   clear                   pTAD
031600050914     c                   clear                   pDCI
031700050914     c                   clear                   pDCF
031800050914     c                   clear                   pDCA
031900050914     c                   clear                   pDSP
032000050914     c                   clear                   pDEV
032100050914     c                   clear                   pDRR
032200050914     c                   clear                   pDVR
032300050914     c                   clear                   pIPL
032400050914     c                   clear                   pIPR
032500050914     c                   clear                   pIRI
032600051017      *
032700051017      * Decodifico Tipo Anomalia
032800051017     c                   eval      wTAD     = DCTtad
032900051017     c                   exsr      DecodTAD
033000050811      *
033100050811      * Inversione date
033200050811      * - data C.A.
033300050811     c                   reset                   WLBdat
033400050811     c                   eval      G08inv   = DCTaac * 10000 + DCTmgc
033500050811     c                   call      'XSRDA8'
033600050811     c                   parm                    WLBdat
033700050811if  1c                   if        G08err   = *off
033800050811     c                   eval      pDCA     = G08dat
033900050811e   1c                   endif
034000050811      * - data Spedizione
034100050811     c                   reset                   WLBdat
034200050812     c                   eval      G08inv   = DCTaas * 10000 + TASmgs
034300050811     c                   call      'XSRDA8'
034400050811     c                   parm                    WLBdat
034500050811if  1c                   if        G08err   = *off
034600050811     c                   eval      pDSP     = G08dat
034700050811e   1c                   endif
034800050811      * - data Evento
034900050811if  1c                   if        DCTnev   > *zeros
035000050811     c                   reset                   WLBdat
035100050811     c                   eval      G08inv   = DCTaae * 10000 + DETmge
035200050811     c                   call      'XSRDA8'
035300050811     c                   parm                    WLBdat
035400050811if  2c                   if        G08err   = *off
035500050811     c                   eval      pDEV     = G08dat
035600050811e   2c                   endif
035700050811e   1c                   endif
035800051025      * - data Richiesta Recupero
035900050811if  1c                   if        DRAdrr   > *zeros
036000050811     c                   reset                   WLBdat
036100050811     c                   eval      G08inv   = DRAdrr
036200050811     c                   call      'XSRDA8'
036300050811     c                   parm                    WLBdat
036400050811if  2c                   if        G08err   = *off
036500050811     c                   eval      pDRR     = G08dat
036600050811e   2c                   endif
036700050811e   1c                   endif
036800051025      * - data Ricezione Recupero (Valuta)
036900050811if  1c                   if        DRAdvr   > *zeros
037000050811     c                   reset                   WLBdat
037100050811     c                   eval      G08inv   = DRAdvr
037200050811     c                   call      'XSRDA8'
037300050811     c                   parm                    WLBdat
037400050811if  2c                   if        G08err   = *off
037500050811     c                   eval      pDVR     = G08dat
037600050811e   2c                   endif
037700050811e   1c                   endif
037800050811      *
037900050811      * Impostazione campi importo (di misura inferiore in stampa)
038000050811     c                   z-add     DCLipl        pIPL
038100050914     c                   add       DCLipl        pIPLt
038200050812if  1c                   if        DRAdrr   > *zeros
038300050811     c                   z-add     DCLipl        pIPR
038400050914     c                   add       DCLipl        pIPRt
038500050812e   1c                   endif
038600050811     c                   z-add     DRAiri        pIRI
038700050914     c                   add       DRAiri        pIRIt
038800050811      *
038900050811      *
039000050811      * Stampa
039100050811     c                   if        *inOF    = *on
039200050812     c                   except    PRTtx1
039300050812     c                   except    PRTtx2
039400050811     c                   eval      *inOF    = *off
039500050811     c                   endif
039600050811     c                   except    PRTdet
039700050811      *
039800050811     c                   ENDSR
039900050811      *
040000050811      *---------------------------------------------------------------*
040100050811      * Decodifica / Controllo TIPO ANOMALIA                          *
040200050811      *---------------------------------------------------------------*
040300050811     c     DecodTAD      BEGSR
040400050811      *
040500050811     c                   clear                   dTAD
040600050811     c                   reset                   TIBS02ds
040700050811     c                   movel     KNSIF         T02sif
040800050812     c                   movel(p)  wTAD          T02ke1
040900050811     c                   call      'TIBS02R'
041000050811     c                   parm                    KPJBA
041100050811     c                   parm                    TIBS02ds
041200050811     c                   if        T02err   = *blanks
041300050811     c                   movel     T02uni        dTAD
041400050811     c                   endif
041500050812      *
041600051017if  1c***                if        §TADevfo =  'S'
041700051017     c***                ...       ...
041800051017e   1c***                endif
041900050905      *
042000050905     c                   movel     §TADdesc      pTAD
042100050811      *
042200050811     c                   ENDSR
042300050811      *
042400050811      *---------------------------------------------------------------*
042500050811      * Operazioni Finali                                             *
042600050811      *---------------------------------------------------------------*
042700050811     c     OpeFin        BEGSR
042800050811      *
042900050811      * Stampa fine lista
043000050914if  1c                   if            pIPLt   = *zeros
043100050914     c                             and pIPRt   = *zeros
043200050914     c                             and pIRIt   = *zeros
043300050811     c                   except    PRTend
043400050914x   1c                   else
043500050914     c                   except    PRTtot
043600050914e   1c                   endif
043700050811      *
043800050811      * Chiusura prtf
043900050811     c                   close     PRTF198
044000050811      *
044100050811      * Cancellazione override al prtf
044200050811     c                   call      'QCMDEXC'
044300050811     c                   parm      $Cmd(2)       Qcmd
044400050811     c                   parm                    Qlen
044500050811      *
044600050811      * Chiusura pgm. TIBS02R
044700050811     c                   clear                   TIBS02ds
044800050811     c                   eval      T02tla   = 'C'
044900050811     c                   call      'TIBS02R'
045000050811     c                   parm                    KPJBA
045100050811     c                   parm                    TIBS02ds
045200050811      *
045300050811     c                   ENDSR
045400050811
045500050811      *---------------------------------------------------------------*
045600050811
045700050812     oPRTF198   e            PRTtx1            2
045800050811     o                       RSUT
045900051025     o                                           90 'LISTA RECUPERO C.-
046000051025     o                                              A. AFFIDATE'
046100050811     o                       SDSpgm             140
046200050811     o                       *date         y  +   5
046300050811     o                       Wtime            +   5 '  :  :  '
046400050811     o                                        +   5 'Pag.'
046500050811     o                       Page             +   0
046600050812     o          e            PRTtx1      0
046700051025     o                                           90 'LISTA RECUPERO C.-
046800051025     o                                              A. AFFIDATE'
046900050812      *
047000050812     o          e            PRTtx2      2
047100050811     o                       $TXT(1)
047200050812     o                       $TXT(5)          +   0
047300050812     o                       $TXT(9)          +   0
047400050812     o          e            PRTtx2      1
047500050812     o                       $TXT(2)
047600050812     o                       $TXT(6)          +   0
047700050812     o                       $TXT(10)         +   0
047800050812     o          e            PRTtx2      1  1
047900050812     o                       $TXT(3)
048000050812     o                       $TXT(7)          +   0
048100050812     o                       $TXT(11)         +   0
048200050812      *
048300050812     o          e            PRTpar      5
048400050812     o                                              'PARAMETRI DI LANC-
048500050812     o                                              IO:'
048600050915     o                                        +   5 'Periodo chiusura -
048700050905     o                                              C.A. .............-
048800050915     o                                              .....:'
048900050905     o                                        +   2 'dal'
049000050905     o                       pDCI          y  +   2
049100050905     o                                        +   2 'al'
049200050905     o                       pDCF          y  +   2
049300050812     o          e            PRTpar      0
049400051011     o                                              '_________________-
049500050812     o                                              ___'
049600051011     o          e            PRTpar      5
049700051011     o                                        +  25 'C.A. con richiest-
049800051025     o                                              a recupero da eseg-
049900051011     o                                              uire :'
050000051011     o                       D8Bse1           +   2
050100051011     o                                        +  10 '(S=Sì)'
050200050812     o          e            PRTpar      2
050300051025     o                                        +  25 'C.A. in attesa di-
050400051025     o                                               recupero ........-
050500051025     o                                              .....:'
050600051011     o                       D8Bse2           +   2
050700050812     o                                        +  10 '(S=Sì)'
050800051011     o          e            PRTpar      2
050900051025     o                                        +  25 'C.A. recuperate .-
051000051025     o                                              ..................-
051100051011     o                                              .....:'
051200051011     o                       D8Bse3           +   2
051300051011     o                                        +  10 '(S=Sì)'
051400050811      *
051500050811     o          e            PRTdet      1
051600050811     o                       DCTfil
051700050811     o                       DCTnca        z  +   1
051800050811     o                       pDCA          y  +   2
051900050811     o                       DCTprn        z  +   3
052000050811     o                       DCTpra        z  +   1
052100050811     o                       DCTlnp        z  +   3
052200050811     o                       DCTnrs        z  +   1
052300050811     o                       DCTnsp        z  +   1
052400050811     o                       pDSP          y  +   3
052500051017     o                       DCTtad           +   3
052600050811     o                       pTAD             +   1
052700050811     o                       pDRR             +   3 '  /  /    '
052800050811     o                       pDVR             +   3 '  /  /    '
052900050811     o                       pIPL          k  +   3
053000050811     o                       pIPR          k  +   2
053100050811     o                       pIRI          k  +   2
053200050914      *
053300050914     o          e            PRTtot      2
053400050914     o                       $TXT(13)
053500050914     o                       $TXT(14)         +   0
053600050914     o                       $TXT(15)         +   0
053700051021     o                       pIPLt         k    141
053800050914     o                       pIPRt         k  +   2
053900050914     o                       pIRIt         k  +   2
054000050811      *
054100050811     o          e            PRTend      2
054200050811     o                                           80 '***   Fine Lista   ***'
054300050811     o          e            PRTend      0
054400050811     o                                           80 '***   Fine Lista   ***'
054500050811
054600050811      *---------------------------------------------------------------*
054700050811
054800050812**   $CCHpag
05490005102518
055000050811**   $CMD ---------------------------------------------------------------------*
055100051025OVRPRTF FILE(PRTF198) USRDTA('RecuperoCA')
055200050811DLTOVR  FILE(PRTF198)
055300050811**   $TXT ....+....2....+....3....+....4....+....5....+....6....+*
055400050811*------- C. A. -------*                  *-- S P E D I Z I O N E -
055500050811     Numero     del       Num. Pratica       Spedizione      del
055600050914------------------------------------------------------------------
055700050811123 1234567  12/34/5678   1234567 2005   123 12 1234567   12/34/56
055800051025-*                                  Data Rich.   Data Ricez      L
055900051025     Tipo Anomalia                   Recupero      Recupero
056000050914------------------------------------------------------------------
05610005101778   12 1234567890123456789012345   12/34/5678   12/34/5678   1.23
056200051025iquidato       Recupero        Importo
056300051025 Cliente      Richiesto     Recuperato
056400051017--------------------------------------
0565000510174.567,89-  1.234.567,89-  1.234.567,89-
056600050914
056700051021     T O T A L E   . . . . . . . . . . . . . . . . . . . . .  1.23
0568000510174.567,89-  1.234.567,89-  1.234.567,89-
