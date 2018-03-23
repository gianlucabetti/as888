000100060301      *PARMS CLOSQLCSR(*ENDMOD) DYNUSRPRF(*OWNER) DBGVIEW(*SOURCE)
000200060224      *===============================================================*
000300060224      *?Controllo instradamenti in P.O. - Esecuzione batch           ?*
000400060224      *===============================================================*
000500060224      *
000600060224     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700060224      *
000800060224      *?A R C H I V I?------------------------------------------------*
000900060224      *
001000060224     fTABEL00F  if   e           k disk
001100060224     fTNTBE01L  if   e           k disk
001200060224     fAZORG01L  if   e           k disk
001300060224      *
001400060224     fWFSI760F  Uf A e             disk    usropn
001500060224      *
001600060224     fPRTF198   O    f  198        printer oflind(*inOF)
001700060224      *
001800060224      *?C O S T A N T I?----------------------------------------------*
001900060224      *
002000060303     d C_Select        c                   const('SELECT')
002100060301     d C_Union         c                   const('UNION')
002200060303     d C_OrderBy       c                   const('ORDER BY +
002300060303     d                                            SQLlnp, +
002400060309     d                                            SQLksc, +
002500060303     d                                            SQLnrs, +
002600060322     d                                            SQLaas, +
002700060303     d                                            SQLnsp')
002800060301     d C_ForFetch      c                   const('FOR FETCH ONLY')
002900060224      *
003000060224      *?S C H I E R E?------------------------------------------------*
003100060224      *
003200060303     d $Sql_Fld1       s              9    dim(26)  ctdata perrcd(1)            Campi da selezionare
003300060303     d $Sql_Fld2       s             10    dim(26)  alt($Sql_Fld1)              Campi da selezionare
003400060303     d $Sql_BLP        s             78    dim(10)  ctdata perrcd(1)            Selezioni da FNBLP
003500060303     d $Sql_ARB        s             78    dim(10)  ctdata perrcd(1)            Selezioni da FNARB
003600060303     d $Sql_TAS        s             78    dim(10)  ctdata perrcd(1)            Selezione da TITAS
003700060303     d $Sql_VAB        s             78    dim(10)  ctdata perrcd(1)            Selezioni da FIVAB
003800060224     d $Txt            s             66    dim(32)  ctdata perrcd(1)            *righe di stampa
003900060303     d $Decod          s             66    dim(10)  ctdata perrcd(1)            *decodifiche
004000060224      * - P.O. in £1
004100060224     d £1              s              3  0 dim(250) inz
004200060224      * - Dati da tab. "3A" = Codici bolla
004300060303     d $3A             s                   dim( 50) like(SQLcbo)  inz           *codice bolla
004400060302     d $3Atb1          s                   dim( 50) like(§3Atb1)  inz           *tipo bolla 1
004500060302     d $3Atb2          s                   dim( 50) like(§3Atb2)  inz           *tipo bolla 2
004600060302     d $3Afca          s                   dim( 50) like(§3Afca)  inz           *c/assegno
004700060224      * - Dati dei P.O.
004800060302     d $PO             s                   dim(999) like(ORGfil)  inz           *p.o.
004900060302     d $POfie          s                   dim(999) like(§NTWfie) inz           *flag italia/estero
005000060302     d $POntw          s                   dim(999) like(§OGntw)  inz           *network P.O.
005100060302     d $POfed          s                   dim(999) like(ORGfil)  inz           *p.o. FedEx
005200060302     d $POxin          s              5    dim(999)               inz           *"nnn, "
005300060224      *
005400060224      *?S T R U T T U R E   D A T I?----------------------------------*
005500060224      *
005600060224     d KPJBA         e ds
005700060224      *
005800060224      * - Parametri x Controllo profilo utenti
005900060306     d TIBS34ds      e ds                  inz
006000060224      * - Ds di riferimento al file esterno AZUTE00F
006100060306     d AZUTEds       e ds                  inz  extname(AZUTE00F)
006200060224      * - Ds per dati organigramma
006300060306     d DDatiUte      e ds                  inz
006400060302      *
006500060224      * - Parametri x Caricamento £1
006600060224     d TRUL06ds      e ds                  inz
006700060224     d   D06cod      e                     inz('PP')
006800060224     d   D06tla      e                     inz('L')
006900060224     d   Lin                   1     90  0 inz  dim(30)
007000060224      * - Parametri x Reperimento P.O. in gestione
007100060224     d TRUL26ds      e ds                  inz
007200060224     d  I26tla       e                     inz('L')
007300060224     d  I26tpt       e                     inz('P')
007400060224     d  SKpo                  11    760  0 inz  dim(250)
007500060224     d  SKpot                761   1510  0 inz  dim(250)
007600060224      *
007700060224      * - Parametri x Reperimento terminals
007800060224     d FNLV55ds      e ds                  inz
007900060224      *
008000060224      * - Parametri x Controllo CAP
008100060224     d TISI95ds      e ds                  inz
008200060224      *
008300060224      * - Parametri x Decodifica anagrafica clienti
008400060224     d TIBS69ds      e ds                  inz
008500060224     d DS_cnaco      e ds                  inz  extname(CNACO00F)
008600060224     d DS_cnind      e ds                  inz  extname(CNIND00F)
008700060224     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
008800060224     d DS_fncls      e ds                  inz  extname(FNCLS00F)
008900060224      *
009000060224      * - Descrizione 143 dell'organigramma
009100060224     d Og143         e ds                  inz
009200060224      *
009300060224      * - Tabella "NTW" = Network Aziendali
009400060224     d dNTW          e ds                  inz
009500060224      * - Tabella "1B"  = Codici Trattamento Merce
009600060224     d ds1B          e ds                  inz
009700060224      * - Tabella "3A"  = Codici Bolla
009800060224     d ds3A          e ds                  inz
009900060224      * - Tabella "3C"  = Clienti con dischetto
010000060224     d ds3C          e ds                  inz
010100060303      *
010200060303      * - DS di comodo per definizione campi da estrarre via SQL
010300060303     d FNBLPds       e ds                  inz  extname(FNBLP00F)
010400060420      *
010500060420      *
010600060420     d Status         sds
010700060420     d  SDSpgm           *proc
010800060303      *
010900060303      * - Dati estratti via SQL
011000060303     d ds_SQL          ds                  inz
011100060303     d   SQLaas                            inz  like(BLPaas)
011200060303     d   SQLmgs                            inz  like(BLPmgs)
011300060303     d   SQLlnp                            inz  like(BLPlnp)
011400060303     d   SQLnrs                            inz  like(BLPnrs)
011500060303     d   SQLnsp                            inz  like(BLPnsp)
011600060303     d   SQLcbo                            inz  like(BLPcbo)
011700060303     d   SQLksc                            inz  like(BLPksc)
011800060303     d   SQLcad                            inz  like(BLPcad)
011900060303     d   SQLlod                            inz  like(BLPlod)
012000060303     d   SQLprd                            inz  like(BLPprd)
012100060303     d   SQLnzd                            inz  like(BLPnzd)
012200060303     d   SQLtfp                            inz  like(BLPtfp)
012300060303     d   SQLtsp                            inz  like(BLPtsp)
012400060303     d   SQLtc1                            inz  like(BLPtc1)
012500060303     d   SQLtc2                            inz  like(BLPtc2)
012600060303     d   SQLffd                            inz  like(BLPffd)
012700060303     d   SQLncl                            inz  like(BLPncl)
012800060303     d   SQLpkb                            inz  like(BLPpkb)
012900060303     d   SQLvlb                            inz  like(BLPvlb)
013000060303     d   SQLlna                            inz  like(BLPlna)
013100060303     d   SQLznc                            inz  like(BLPznc)
013200060303     d   SQLrmn                            inz  like(BLPrmn)
013300060303     d   SQLrma                            inz  like(BLPrma)
013400060303     d   SQLfvb                            inz  like(BLPfvb)
013500060303     d   SQLfgs                       3  0 inz                                  -p.o. gestione
013600060303     d   SQLcca                       1    inz                                  -cod.cons.anomala
013700060303      *
013800060224      * - Parametri ricevuti da pgm. TISI75R
013900060224     d TISI76ds        ds                  inz
014000060224     d   P76pav                       1    inz                                  -tipo bolla
014100060224     d   P76ds1                       8  0 inz                                  -data iniziale
014200060224     d   P76ds2                       8  0 inz                                  -data finale
014300060224     d   P76ksc                       7  0 inz                                  -cliente
014400060302     d   P76lnp                       3  0 inz                                  -p.o. partenza
014500060302     d   P76lna                       3  0 inz                                  -p.o. arrivo
014600060224     d   P76fns                       1    inz                                  -bolle con serie
014700060302     d   P76fel                       1    inz                                  -errori di p.o.
014800060224     d   P76nrs                       2  0 inz                                  -serie
014900060224     d   P76tot                       1    inz                                  -solo totali
015000061017     d   P76obs                       1    inz                                  -solo totali
015100060303      *
015200060303      * - Controllo/Inversione date
015300060303     d WLBdat          ds                  inz
015400060303     d   G08dat                       8  0 inz
015500060303     d   G08inv                       8  0 inz
015600060303     d   G08err                       1    inz('3')
015700060303     d   G08tgi                       5  0 inz
015800060224      *
015900060224      *?V A R I A B I L I?--------------------------------------------*
016000060224      *
016100060224      * - Flags
016200060303     d $EoF            s              1    inz(*off)
016300060224     d $Err            s              1    inz(*off)
016400060224     d $RecOK          s              1    inz(*off)
016500060224     d $PT             s              1    inz(*off)
016600060303      *
016700060303      * - Parametri per SQL
016800061018     d wSQL            s           7500    inz
016900061018     d wSQLds          ds                  inz
017000061018     d   WWWsql1                    500    inz
017100061018     d   WWWsql2                    500    inz
017200061018     d   WWWsql3                    500    inz
017300061018     d   WWWsql4                    500    inz
017400061018     d   WWWsql5                    500    inz
017500061018     d   WWWsql6                    500    inz
017600061018     d   WWWsql7                    500    inz
017700061018     d   WWWsql8                    500    inz
017800061018     d   WWWsql9                    500    inz
017900061018     d   WWWsql10                   500    inz
018000061018     d   WWWsql11                   500    inz
018100061018     d   WWWsql12                   500    inz
018200061018     d   WWWsql13                   500    inz
018300061018     d   WWWsql14                   500    inz
018400061018     d   WWWsql15                   500    inz
018500060224      *
018600060224      * - Indici di schiera
018700060224     d xx              s              3  0 inz
018800060224     d xy              s              3  0 inz
018900060302      *
019000060302      * - Campi per test livelli di rottura
019100060307     d SAVlnp          s                   like(SQLlnp)  inz(*loval)
019200060307     d SAVnrs          s                   like(SQLnrs)  inz(*loval)
019300060307     d SAVksc          s                   like(SQLksc)  inz(*loval)
019400060224      *
019500060224      * - Campi in stampa
019600060224     d WPPpav          s             10    inz
019700060303     d WPPds1          s                   inz  like(I95dat)
019800060303     d WPPds2          s                   inz  like(I95dat)
019900060306     d WPPksc          s             30    inz
020000060306     d WPPlnp          s             30    inz
020100060306     d WPPlna          s             30    inz
020200140910     d WTSRag          s             20    inz
020300060303     d WPDftc          s                   inz  like(BLPtc1)                    *consegna particolar
020400060303     d WPDdat          s                   inz  like(I95dat)                    *data spedizione
020500060302     d WPDxla          s                   inz  like(O95cla)                    *lna standard
020600060302     d WPDxzc          s                   inz  like(O95czc)                    *znc standard
020700060302     d WPDxls          s                   inz  like(O95cls)                    *lna sotto
020800060302     d WPDxzs          s                   inz  like(O95czs)                    *znc sotto
020900060302     d WPDxlo          s                   inz  like(O95clo)                    *lna oltre
021000061018     d WPDxzo          s                   inz  like(O95czo)                    *znc oltre
021100060303     d WPDxzf          s                   inz  like(O95czf)                    *znc f.deposito
021200060303     d WPDfca          s                   inz  like(I95fca)                    *S=C/Assegno
021300061018     d WPDobs          s              1    inz
021400060302     d WPDlid          s             10    inz                                  *livello scelta
021500060302     d WPDlil          s             10    inz                                  *livello lna/znc
021600060303     d WPDloc1         s             25    inz                                  *località (1ª parte)
021700060303     d WPDloc2         s             10    inz                                  *località (2ª parte)
021800060302     d WPDtxt          s            198    inz                                  *testo generico
021900061018     d WPDtxtobs       s             67    inz                                  *testo generico
022000060302     d WPEmsg1         s             80    inz                                  *messaggio di errore
022100060302     d WPEmsg2         s             80    inz                                  *messaggio di errore
022200060302     d WPDverCap       s             15    inz
022300060302     d WPDgesPkb       s             15    inz
022400060302     d WPDsegSeq       s             15    inz
022500060302      *
022600060302      * - Campi d Totale
022700060302     d wConta          s              7  0 inz
022800060302     d wContaPo        s              7  0 inz
022900060302     d wContaRot       s              7  0 inz
023000060302     d wContaTot       s              7  0 inz
023100060302     d wContaPoTot     s              7  0 inz
023200060302     d wContaRotTot    s              7  0 inz
023300060302     d wColli          s              7  0 inz
023400060302     d wColliPo        s              7  0 inz
023500060302     d wColliRot       s              7  0 inz
023600060302     d wColliTot       s              7  0 inz
023700060302     d wColliPoTot     s              7  0 inz
023800060302     d wColliRotTot    s              7  0 inz
023900060302     d wContaDir       s              7  0 inz
024000060302     d wContaDirTot    s              7  0 inz
024100060302     d wSup            s              7  0 inz
024200060302     d wSupPo          s              7  0 inz
024300060302     d wSupRot         s              7  0 inz
024400060302     d wSupTot         s              7  0 inz
024500060302     d wSupPoTot       s              7  0 inz
024600060302     d wSupRotTot      s              7  0 inz
024700061017     d wContaob        s              7  0 inz
024800061017     d wContaPoob      s              7  0 inz
024900061017     d wContaRotob     s              7  0 inz
025000061017     d wColliob        s              7  0 inz
025100061017     d wColliPoob      s              7  0 inz
025200061017     d wColliRotob     s              7  0 inz
025300060302      *
025400060302      * - Campi di comodo
025500060302     d WWWnrs          s                   inz  like(SQLnrs)
025600060302     d WWWtpo          s                   inz  like(I95tpo)
025700060303     d WPOxin          s           5000    inz                                  "nnn, "
025800060303     d Wxxx            s              3    inz                                  BLP/ARB/TAS/VAB
025900060420     d Wtime           s              6  0 inz
026000060224      *
026100060224      *?K E Y - L I S T?----------------------------------------------*
026200060224      *
026300060224      * - TABEL00F
026400060224     c     K02TABEL      klist
026500060224     c                   kfld                    TBLkut
026600060224     c                   kfld                    TBLcod
026700060224     c     K03TABEL      klist
026800060224     c                   kfld                    TBLkut
026900060224     c                   kfld                    TBLcod
027000060224     c                   kfld                    TBLkey
027100060224      * - TNTBE01L
027200060224     c     K02TBE01      klist
027300060224     c                   kfld                    TBEcod
027400060224     c                   kfld                    TBEke1
027500060224      *
027600060224      *===============================================================*
027700060224      *                                                               *
027800060224      *?I N D I C A T O R I?------------------------------------------*
027900060224      *                                                               *
028000060224      *  10     - Comodo                                              *
028100060224      *                                                               *
028200060224      *===============================================================*
028300060224      *
028400060224     c     *Entry        plist
028500060224     c                   parm                    KPJBA
028600060224     c                   movel     KPJBU         TISI76ds
028700060224      *
028800060224      * Operazioni iniziali
028900060224     c                   exsr      RoutInz
029000060224      *
029100060224      * Ciclo di elaborazione fino a fine file
029200060303do  1c                   DOW       $EoF    = *off
029300060224      *
029400060302     c                   exsr      ReadCursor
029500060302     c                   if        $RecOK  =  *on
029600060302     c                   exsr      sr_Print
029700060302     c                   endif
029800060224      *
029900060302e   1c                   ENDDO                                                  *rottura serie
030000060224      *
030100060224      * Operazioni finali
030200060224     c                   exsr      RoutFin
030300060224      *
030400060224      * Fine
030500060224     c                   eval      *inLR = *on
030600060224      *
030700060224      *?S U B R O U T I N E S?----------------------------------------*
030800060224      *
030900060224      *---------------------------------------------------------------*
031000060224      *?Operazioni iniziali                                          ?*
031100060224      *---------------------------------------------------------------*
031200060224     c     RoutInz       BEGSR
031300060302      *
031400060302      * Impostazione iniziale dati fissi
031500060420     c                   time                    wTime
031600060302     c                   eval      TBLkut  = 1
031700060224      *
031800060224      * Reperimento dati utente
031900060224     c                   exsr      RepDatiUte
032000060224      *
032100060224      * Considerazioni iniziali sui parametri di lancio
032200060302     c*** già cosi:      clear                   WWWnrs
032300060303     c                   if        P76fns  = 'S'
032400060302     c                   eval      WWWnrs  = 1
032500060224     c                   endif
032600060302      *
032700060302      * Caricamento delle tabelle occorrenti
032800060302     c                   exsr      sr_CarTAB
032900060303      *
033000060303      * Sistemazione schiere per estrazione dati via SQL
033100060303     c                   exsr      sr_SchSQL
033200060302      *
033300060302      * Preparazione stringa SQL da eseguire
033400060302     c                   exsr      sr_PrepSQL
033500060302      *
033600060302      * Apertura archivi
033700060302     c                   exsr      sr_Open
033800060302      *
033900060302      * Stampa dei parametri di lancio
034000060302     c                   exsr      sr_PrtParm
034100060302      *
034200060302     c                   ENDSR
034300060302      *
034400060302      *---------------------------------------------------------------*
034500060302      *?Reperimento Dati Utente                                      ?*
034600060302      *---------------------------------------------------------------*
034700060302     c     RepDatiUte    BEGSR
034800060302      *
034900060302     c     *dtaara       define    §azute        azuteds
035000060302     c     *dtaara       define    §datiute      ddatiute
035100060302     c                   in(e)     *dtaara
035200060302if  1c                   if        %error
035300060302     c                   eval      I34tla = 'L'
035400060302     c                   call      'TIBS34R'
035500060302     c                   parm                    TIBS34ds
035600060302     c                   in        *dtaara
035700060302e   1c                   endif
035800060302      *
035900060302     c                   ENDSR
036000060302      *
036100060302      *---------------------------------------------------------------*
036200060302      *?Caricamento schiere da tabelle                               ?*
036300060302      *---------------------------------------------------------------*
036400060302     c     sr_CarTAB     BEGSR
036500060224      *
036600060224      * Caricamento tab. "3A" = Codici Bolla
036700060224      *   (selezione delle sole bolle normali)
036800060224     c                   clear                   xx
036900060224     c                   eval      TBLkut  = 1
037000060224     c                   eval      TBLcod  = '3A'
037100060224     c     K02TABEL      setll     TABEL
037200060224     c     K02TABEL      reade     TABEL
037300060224do  1c                   DOW       NOT %eof(TABEL00F)
037400060224if  2c                   if        TBLflg  = *blanks
037500060303     c                   movel     TBLuni        ds3A
037600060224if  3c                   if        §3Arbl  = 'N'
037700060224     c                   add       1             xx
037800060224     c                   movel     TBLkey        $3A(xx)
037900060224     c                   movel     §3Atb1        $3Atb1(xx)
038000060224     c                   movel     §3Atb2        $3Atb2(xx)
038100060224     c                   z-add     §3Afca        $3Afca(xx)
038200060224e   3c                   endif
038300060224e   2c                   endif
038400060224     c     K02tabel      reade     TABEL
038500060224e   1c                   ENDDO
038600060224      *
038700060224      * Caricamento P.O.
038800060224     c                   clear                   xx
038900060224     c                   clear                   xy
039000060224     c     *loval        setll     AZORG
039100060224     c                   read      AZORG
039200060224do  1c                   DOW       NOT %eof(AZORG01L)
039300060224if  2c                   if            ORGfva  = *blanks                        *valido
039400060224     c                             and ORGfag <> 'V'                            *no varie
039500060224     C                   movel     ORGde3        Og143
039600060224     c                   add       1             xx
039700060224     c                   eval      $PO(xx)    = ORGfil
039800060224     c                   eval      $POntw(xx) = §OGntw
039900060224      * - Test del network
040000060224      *   x stabilire se si tratta di p.o. Italia o Estero
040100060224     c                   clear                   dNTW
040200060306     c                   eval      TBEcod  = 'NTW'
040300060306     c                   movel(p)  §OGntw        TBEke1
040400060224     c     K02TBE01      chain     TNTBE000
040500060224if  3c                   if        %found(TNTBE01L)
040600060303     c                   movel     TBEuni        dNTW
040700060224e   3c                   endif
040800060224if  3c                   if        §NTWfie = *blanks
040900060224     c                   eval      $POfie(xx) = 'I'                             *flag Italia
041000060224x   3c                   else
041100060224     c                   eval      $POfie(xx) = 'E'                             *flag Estero
041200060224e   3c                   endif
041300060224      * - Memorizzazione dei p.o. FedEx
041400060224if  3c                   if        §OGntw  = 'FED'
041500060224     c                   add       1             xy
041600060224     c                   eval      $POfed(xy) = ORGfil
041700060224e   3c                   endif
041800060224e   2c                   endif
041900060224     c                   read      AZORG
042000060224e   1c                   ENDDO
042100060302      *
042200060302do  1c                   do        xx            xy
042300060302     c                   movel     $PO(xy)       $POxin(xy)
042400060302if  2c                   if        xy      < xx
042500060302     c                   move      ', '          $POxin(xy)
042600060302e   2c                   endif
042700060302e   1c                   enddo
042800060303     c                   movea     $POxin        WPOxin
042900060224      *
043000060224      * Caricamento dei P.O. in gestione
043100060224      * - p.o.
043200060224if  1c                   if        DUTlpo <> 'S'
043300060224     c                   reset                   TRUL06ds
043400060224     c                   movel(p)  DUTpou        D06key
043500060224     c                   movel     TRUL06ds      KPJBU
043600060224     c                   call      'TRUL06R'
043700060224     c                   parm                    KPJBA
043800060224     c                   movel     KPJBU         TRUL06ds
043900060224     c                   movea     Lin           £1
044000060224      * - Sede
044100060224x   1c                   else
044200060224     c                   reset                   TRUL26ds
044300060224     c                   call      'TRUL26R'
044400060224     c                   parm                    TRUL26ds
044500060302     c                   movea     SKpo          £1
044600060224e   1c                   endif
044700060224      *
044800060224      * Se non sono in sede: includo/escludo solo bolle POSTE.
044900060224if  1c                   IF        DUTlpo <> 'S'
045000060302     c                   clear                   xx
045100060302do  2c                   DOW       xx      < %elem(£1)
045200060302     c                   eval      xx      = xx + 1
045300060224     c                   eval      xy      = 1
045400060224     c     £1(xx)        lookup    $PO(xy)                                10
045500060224      * - Se esiste P.O. (è altamente imporobabile che non esista
045600060224      *   ma lo gestisco ugualmente)
045700060224if  3c                   if        *in10
045800060224     c                   eval      $PT     = ($POntw(xy) = 'PPT')
045900060224x   3c                   else
046000060224      * - Se non esiste il P.O. statisticamente assumo che il P.O.
046100060224      *   NON sia del network POSTE
046200060224     c                   eval      $PT     = *off
046300060224e   3c                   endif
046400060224      * - Includo/Escludo...
046500060224sel 3c                   select
046600060224w   3c                   when          DUTntw  = 'PPT'
046700060224     c                             and $PT     = *off
046800060224     c                   clear                   £1(xx)
046900060224w   3c                   when          DUTntw <> 'PPT'
047000060224     c                             and $PT     = *on
047100060224     c                   clear                   £1(xx)
047200060224e   3c                   endsl
047300060224e   2c                   ENDDO
047400060224e   1c                   ENDIF
047500060302      *
047600060302     c                   ENDSR
047700060303      *
047800060303      *---------------------------------------------------------------*
047900060303      *?Sistemazione schiere per estrazione dati via SQL             ?*
048000060303      *---------------------------------------------------------------*
048100060303     c     sr_SchSQL     BEGSR
048200060303      *
048300060303      * Spiegazione:
048400060303      * La schiera $Sql_Fld1 contiene le lettere "finali" dei campi da
048500060303      *   estrarre dai vari archivi.
048600060303      * Ogni suo elemento è per questo "suddiviso" in 3 campi:
048700060303      * - i primi 3 caratteri    definiscono il campo dei file BLP/ARB;
048800060303      * - i 3 caratteri in mezzo definiscono il campo nel file TITAS;
048900060303      * - gli ultimi 3 caratteri definiscono il campo nel file FIVAB;
049000060303      * (le lettere iniziali BLP/ARB/TAS/VAB verranno poi impostate).
049100060303      * Nella schiera a tempo di compilazione sono impostate SOLO le
049200060303      *   eccezioni di almeno uno dei 2 archivi TAS/VAB: adesso imposto
049300060303      *   i default (dove mancanti).
049400060306      * N.B. - *blank significa che il campo da considerare ha lo
049500060306      * ¯¯¯¯   stesso nome del 1° (da FNBLP/FNARB);
049600060306      *      - "---"  significa cge il campo non è da considerare per
049700060306      *        il file in esame.
049800060303      *
049900060303     c                   eval      xx      = 1
050000060303do  1c                   dow       xx     <= %elem($Sql_Fld1)
050100060303      *
050200060303if  2c                   if        $Sql_Fld1(xx)        <> *blanks
050300060306      *
050400060306      * - dati da FNBLP/FNARB
050500060306      *   (solo esclusioni)
050600060306if  3c                   if        %subst($Sql_Fld1(xx):1:3) = *all'-'
050700060306     c                   eval      %subst($Sql_Fld1(xx):1:3) = *blanks
050800060306e   3c                   endif
050900060303      *
051000060303      * - dati da TITAS
051100060303sel 3c                   select
051200060303w   3c                   when      %subst($Sql_Fld1(xx):4:3) = *blanks
051300060303     c                   eval      %subst($Sql_Fld1(xx):4:3) =
051400060303     c                             %subst($Sql_Fld1(xx):1:3)
051500060303w   3c                   when      %subst($Sql_Fld1(xx):4:3) = *all'-'
051600060303     c                   eval      %subst($Sql_Fld1(xx):4:3) = *blanks
051700060303e   3c                   endsl
051800060303      *
051900060303      * - dati da FIVAB
052000060303sel 3c                   select
052100060303w   3c                   when      %subst($Sql_Fld1(xx):7:3) = *blanks
052200060303     c                   eval      %subst($Sql_Fld1(xx):7:3) =
052300060303     c                             %subst($Sql_Fld1(xx):1:3)
052400060303w   3c                   when      %subst($Sql_Fld1(xx):7:3) = *all'-'
052500060303     c                   eval      %subst($Sql_Fld1(xx):7:3) = *blanks
052600060303e   3c                   endsl
052700060303      *
052800060303e   2c                   endif
052900060303      *
053000060303     c                   eval      xx      = xx + 1
053100060303e   1c                   enddo
053200060303      *
053300060303     c                   ENDSR
053400060302      *
053500060302      *---------------------------------------------------------------*
053600060302      *?Preparazione stringa SQL da eseguire e Apertura cursore      ?*
053700060302      *---------------------------------------------------------------*
053800060302     c     sr_PrepSQL    BEGSR
053900060302      *
054000060303     c                   clear                   wSQL
054100060306     c                   eval      wSQL    = C_Select
054200060302      *
054300060303      * Dichiarazione SQL
054400060301sel 1c                   select
054500060302      *
054600060301      * - Bolle Partenza (da FNBLP00F)
054700060301w   1c                   when      P76pav  = 'P'
054800060301     c                   exsr      PrepSQL_BLP
054900060302      *
055000060301      * - Bolle Arrivo . (da FNARB00F)
055100060301w   1c                   when      P76pav  = 'A'
055200060301     c                   exsr      PrepSQL_ARB
055300060302      *
055400060301      * - Bolle Sede ... (da TITAS30C)
055500060301w   1c                   when      P76pav  = 'S'
055600060301     c                   exsr      PrepSQL_TAS
055700060302      *
055800060301      * - Dkt .......... (da FIVAB00F)
055900060301w   1c                   when      P76pav  = 'D'
056000060301     c                   exsr      PrepSQL_VAB
056100060301      *
056200060301e   1c                   endsl
056300060303      *
056400060303     c                   eval      wSQL    =  %trim(wSQL)
056500060303      *?order by?
056600060303     c                                     +  ' '
056700060303     c                                     +  C_OrderBy
056800060303      *?for fatch only?
056900060303     c                                     +  ' '
057000060303     c                                     +  C_ForFetch
057100060303      *
057200060303      * Apertura cursore
057300061018     c                   movel     wSQL          wSQLds
057400060303     c                   exsr      OpenCursor
057500060224      *
057600060224     c                   ENDSR
057700060301      *
057800060301      *---------------------------------------------------------------*
057900060301      *?Preparazione stringa SQL x bolle partenza                    ?*
058000060301      *---------------------------------------------------------------*
058100060301     c     PrepSQL_BLP   BEGSR
058200060301      *
058300060303     c                   eval      Wxxx    = 'BLP'
058400060303     c                   eval      xx      = 1
058500060303     c                   exsr      PrepSQL_Sel
058600060303      *
058700060303     c                   eval      wSQL    = %trim(wSQL)
058800060302     c                                     + ' '
058900060306     c                                     + %trim($Sql_BLP( 1))
059000060303     c                                     + ' '
059100060302     c                                     + %trim($Sql_BLP( 2))
059200060302      * - lnp
059300060302     c                                     + ' '
059400060303     c                                     + %trim($Sql_BLP( 3))
059500060303     c                                     + %trim(WPOxin) + ')'
059600060302      * - lna
059700060302     c                                     + ' '
059800060303     c                                     + %trim($Sql_BLP( 4))
059900060303     c                                     + %trim(WPOxin) + ')'
060000060302      * - data spedizione (aaaammgg)
060100060302     c                                     + ' '
060200060303     c                                     + %trim($Sql_BLP( 5))
060300060302     c                                     + ' '
060400060303     c                                     + %char(P76ds1)
060500060302     c                                     + ' and '
060600060303     c                                     + %char(P76ds2)
060700060302      * - codice cliente specifico
060800060302     c                   if        P76ksc <> *zeros
060900060302     c                   eval      wSQL    = %trim(wSQL)
061000060302     c                                     + ' '
061100060303     c                                     + %trim($Sql_BLP( 6))
061200060302     c                                     + ' '
061300060303     c                                     + %char(P76ksc)
061400060302     c                   endif
061500060302      * - p.o.partenza specifico
061600060302     c                   if        P76lnp <> *zeros
061700060302     c                   eval      wSQL    = %trim(wSQL)
061800060302     c                                     + ' '
061900060303     c                                     + %trim($Sql_BLP( 7))
062000060302     c                                     + ' '
062100060303     c                                     + %char(P76lnp)
062200060302     c                   endif
062300060302      * - p.o.arrivo specifico
062400060302     c                   if        P76lna <> *zeros
062500060302     c                   eval      wSQL    = %trim(wSQL)
062600060302     c                                     + ' '
062700060303     c                                     + %trim($Sql_BLP( 8))
062800060302     c                                     + ' '
062900060303     c                                     + %char(P76lna)
063000060302     c                   endif
063100060302      * - serie specifica
063200060302     c                   if        P76nrs <> *zeros
063300060302     c                   eval      wSQL    = %trim(wSQL)
063400060302     c                                     + ' '
063500060303     c                                     + %trim($Sql_BLP( 9))
063600060302     c                                     + ' '
063700060303     c                                     + %char(P76nrs)
063800060302     c                   endif
063900060420      * - solo ldv con serie
064000060302     c                   if        P76fns  = 'S'
064100060302     c                   eval      wSQL    = %trim(wSQL)
064200060302     c                                     + ' '
064300060303     c                                     + %trim($Sql_BLP(10))
064400060302     c                   endif
064500060301      *
064600060301     c                   ENDSR
064700060301      *
064800060301      *---------------------------------------------------------------*
064900060301      *?Preparazione stringa SQL x bolle arrivo                      ?*
065000060301      *---------------------------------------------------------------*
065100060301     c     PrepSQL_ARB   BEGSR
065200060301      *
065300060303     c                   eval      Wxxx    = 'ARB'
065400060303     c                   eval      xx      = 1
065500060303     c                   exsr      PrepSQL_Sel
065600060303      *
065700060303     c                   eval      wSQL    = %trim(wSQL)
065800060303     c                                     + ' '
065900060303     c                                     + %trim($Sql_ARB( 1))
066000060302     c                                     + ' '
066100060302     c                                     + %trim($Sql_ARB( 2))
066200060302      * - lnp
066300060302     c                                     + ' '
066400060303     c                                     + %trim($Sql_ARB( 3))
066500060303     c                                     + %trim(WPOxin) + ')'
066600060302      * - lna
066700060302     c                                     + ' '
066800060303     c                                     + %trim($Sql_ARB( 4))
066900060303     c                                     + %trim(WPOxin) + ')'
067000060302      * - data spedizione (aaaammgg)
067100060302     c                                     + ' '
067200060303     c                                     + %trim($Sql_ARB( 5))
067300060302     c                                     + ' '
067400060303     c                                     + %char(P76ds1)
067500060302     c                                     + ' and '
067600060303     c                                     + %char(P76ds2)
067700060302      * - codice cliente specifico
067800060302     c                   if        P76ksc <> *zeros
067900060302     c                   eval      wSQL    = %trim(wSQL)
068000060302     c                                     + ' '
068100060303     c                                     + %trim($Sql_ARB( 6))
068200060302     c                                     + ' '
068300060303     c                                     + %char(P76ksc)
068400060302     c                   endif
068500060302      * - p.o.partenza specifico
068600060302     c                   if        P76lnp <> *zeros
068700060302     c                   eval      wSQL    = %trim(wSQL)
068800060302     c                                     + ' '
068900060303     c                                     + %trim($Sql_ARB( 7))
069000060302     c                                     + ' '
069100060303     c                                     + %char(P76lnp)
069200060302     c                   endif
069300060302      * - p.o.arrivo specifico
069400060302     c                   if        P76lna <> *zeros
069500060302     c                   eval      wSQL    = %trim(wSQL)
069600060302     c                                     + ' '
069700060303     c                                     + %trim($Sql_ARB( 8))
069800060302     c                                     + ' '
069900060303     c                                     + %char(P76lna)
070000060302     c                   endif
070100060302      * - serie specifica
070200060302     c                   if        P76nrs <> *zeros
070300060302     c                   eval      wSQL    = %trim(wSQL)
070400060302     c                                     + ' '
070500060303     c                                     + %trim($Sql_ARB( 9))
070600060302     c                                     + ' '
070700060303     c                                     + %char(P76nrs)
070800060302     c                   endif
070900060420      * - solo ldv con serie
071000060302     c                   if        P76fns  = 'S'
071100060302     c                   eval      wSQL    = %trim(wSQL)
071200060302     c                                     + ' '
071300060303     c                                     + %trim($Sql_ARB(10))
071400060302     c                   endif
071500060301      *
071600060301     c                   ENDSR
071700060301      *
071800060301      *---------------------------------------------------------------*
071900060301      *?Preparazione stringa SQL x bolle sede                        ?*
072000060301      *---------------------------------------------------------------*
072100060301     c     PrepSQL_TAS   BEGSR
072200060301      *
072300060302      *?TITAS00F?
072400060303     c                   eval      Wxxx    = 'TAS'
072500060303     c                   eval      xx      = 2
072600060303     c                   exsr      PrepSQL_Sel
072700060303      *
072800060303     c                   eval      wSQL    = %trim(wSQL)
072900060303     c                                     + ' '
073000060303     c                                     + %trim($Sql_TAS( 1))
073100060302     c                                     + ' '
073200060302     c                                     + %trim($Sql_TAS( 2))
073300060303     c                   exsr      Add_SelTAS
073400060303      *
073500060307      * Union + Select
073600060307     c                   eval      wSQL    = %trim(wSQL)
073700060307     c                                     + ' '
073800060307     c                                     + C_Union
073900060307     c                                     + ' '
074000060307     c                                     + C_Select
074100060302      *
074200060302      *?TITAS10F?
074300060303     c                   eval      Wxxx    = 'TAS'
074400060303     c                   eval      xx      = 2
074500060303     c                   exsr      PrepSQL_Sel
074600060303      *
074700060303     c                   eval      wSQL    =  %trim(wSQL)
074800060303     c                                     + ' '
074900060303     c                                     + 'from TITAS10F'
075000060303     c                                     + ' '
075100060303     c                                     + %trim($Sql_TAS( 2))
075200060303     c                   exsr      Add_SelTAS
075300060303      *
075400060303      * Union
075500060303     c***                eval      wSQL    =  %trim(wSQL)
075600060303     c***                                  +  ' '
075700060303     c***                                  +  C_Union
075800060302      *
075900060302      *?TITASP0F?  (no)
076000060303     c***                eval      Wxxx    = 'TAS'
076100060303     c***                eval      xx      = 2
076200060303     c***                exsr      PrepSQL_Sel
076300060303      ****
076400060303     c***                eval      wSQL    =  %trim(wSQL)
076500060303     c***                                  + ' '
076600060303     c***                                  + 'from TITASP0F'
076700060303     c***                                  + ' '
076800060303     c***                                  + %trim($Sql_TAS( 2))
076900060303     c***                exsr      Add_SelTAS
077000060301      *
077100060301     c                   ENDSR
077200060303      *
077300060303      *---------------------------------------------------------------*
077400060303      *?Aggiunta selezione record in stringa SQL x bolle sede        ?*
077500060303      *---------------------------------------------------------------*
077600060303     c     Add_SelTAS    BEGSR
077700060303      *
077800060303     c                   eval      wSQL    = %trim(wSQL)
077900060303      * - lnp
078000060303     c                                     + ' '
078100060303     c                                     + %trim($Sql_TAS( 3))
078200060303     c                                     + %trim(WPOxin) + ')'
078300060303      * - lna
078400060303     c                                     + ' '
078500060303     c                                     + %trim($Sql_TAS( 4))
078600060303     c                                     + %trim(WPOxin) + ')'
078700060303      * - data spedizione (aaaammgg)
078800060303     c                                     + ' '
078900060303     c                                     + %trim($Sql_TAS( 5))
079000060303     c                                     + ' '
079100060303     c                                     + %char(P76ds1)
079200060303     c                                     + ' and '
079300060303     c                                     + %char(P76ds2)
079400060303      * - codice cliente specifico
079500060303     c                   if        P76ksc <> *zeros
079600060303     c                   eval      wSQL    = %trim(wSQL)
079700060303     c                                     + ' '
079800060303     c                                     + %trim($Sql_TAS( 6))
079900060303     c                                     + ' '
080000060303     c                                     + %char(P76ksc)
080100060303     c                   endif
080200060303      * - p.o.partenza specifico
080300060303     c                   if        P76lnp <> *zeros
080400060303     c                   eval      wSQL    = %trim(wSQL)
080500060303     c                                     + ' '
080600060303     c                                     + %trim($Sql_TAS( 7))
080700060303     c                                     + ' '
080800060303     c                                     + %char(P76lnp)
080900060303     c                   endif
081000060303      * - p.o.arrivo specifico
081100060303     c                   if        P76lna <> *zeros
081200060303     c                   eval      wSQL    = %trim(wSQL)
081300060303     c                                     + ' '
081400060303     c                                     + %trim($Sql_TAS( 8))
081500060303     c                                     + ' '
081600060303     c                                     + %char(P76lna)
081700060303     c                   endif
081800060303      * - serie specifica
081900060303     c                   if        P76nrs <> *zeros
082000060303     c                   eval      wSQL    = %trim(wSQL)
082100060303     c                                     + ' '
082200060303     c                                     + %trim($Sql_TAS( 9))
082300060303     c                                     + ' '
082400060303     c                                     + %char(P76nrs)
082500060303     c                   endif
082600060420      * - solo ldv con serie
082700060303     c                   if        P76fns  = 'S'
082800060303     c                   eval      wSQL    = %trim(wSQL)
082900060303     c                                     + ' '
083000060303     c                                     + %trim($Sql_TAS(10))
083100060303     c                   endif
083200060303      *
083300060303     c                   ENDSR
083400060301      *
083500060301      *---------------------------------------------------------------*
083600060301      *?Preparazione stringa SQL x bolle partenza da cliente         ?*
083700060301      *---------------------------------------------------------------*
083800060301     c     PrepSQL_VAB   BEGSR
083900060301      *
084000060303     c                   eval      Wxxx    = 'VAB'
084100060303     c                   eval      xx      = 3
084200060303     c                   exsr      PrepSQL_Sel
084300060303      *
084400060303     c                   eval      wSQL    = %trim(wSQL)
084500060303     c                                     + ' '
084600060303     c                                     + %trim($Sql_VAB( 1))
084700060302     c                                     + ' '
084800060302     c                                     + %trim($Sql_VAB( 2))
084900060302      * - lnp
085000060302     c                                     + ' '
085100060303     c                                     + %trim($Sql_VAB( 3))
085200060309     c                                     + %trim(WPOxin) + ', 000)'
085300060302      * - lna
085400060302     c                                     + ' '
085500060303     c                                     + %trim($Sql_VAB( 4))
085600060309     c                                     + %trim(WPOxin) + ', 000)'
085700060302      * - data spedizione (aaaammgg)
085800060302     c                                     + ' '
085900060303     c                                     + %trim($Sql_VAB( 5))
086000060302     c                                     + ' '
086100060303     c                                     + %char(P76ds1)
086200060302     c                                     + ' and '
086300060303     c                                     + %char(P76ds2)
086400060302      * - codice cliente specifico
086500060302     c                   if        P76ksc <> *zeros
086600060302     c                   eval      wSQL    = %trim(wSQL)
086700060302     c                                     + ' '
086800060303     c                                     + %trim($Sql_VAB( 6))
086900060302     c                                     + ' '
087000060303     c                                     + %char(P76ksc)
087100060302     c                   endif
087200060302      * - p.o.partenza specifico
087300060302     c                   if        P76lnp <> *zeros
087400060302     c                   eval      wSQL    = %trim(wSQL)
087500060302     c                                     + ' '
087600060303     c                                     + %trim($Sql_VAB( 7))
087700060302     c                                     + ' '
087800060303     c                                     + %char(P76lnp)
087900060302     c                   endif
088000060302      * - p.o.arrivo specifico
088100060302     c                   if        P76lna <> *zeros
088200060302     c                   eval      wSQL    = %trim(wSQL)
088300060302     c                                     + ' '
088400060303     c                                     + %trim($Sql_VAB( 8))
088500060302     c                                     + ' '
088600060303     c                                     + %char(P76lna)
088700060302     c                   endif
088800060302      * - serie specifica
088900060302     c                   if        P76nrs <> *zeros
089000060302     c                   eval      wSQL    = %trim(wSQL)
089100060302     c                                     + ' '
089200060303     c                                     + %trim($Sql_VAB( 9))
089300060302     c                                     + ' '
089400060303     c                                     + %char(P76nrs)
089500060302     c                   endif
089600060420      * - solo ldv con serie
089700060302     c                   if        P76fns  = 'S'
089800060302     c                   eval      wSQL    = %trim(wSQL)
089900060302     c                                     + ' '
090000060303     c                                     + %trim($Sql_VAB(10))
090100060302     c                   endif
090200060301      *
090300060301     c                   ENDSR
090400060303      *
090500060303      *---------------------------------------------------------------*
090600060303      *?Selezione dei campi dal record del file specifico            ?*
090700060303      *---------------------------------------------------------------*
090800060303     c     PrepSQL_Sel   BEGSR
090900060303      *
091000060303      * N.B.1: La lunghezza dei campi è CHIODATA a 6 caratteri
091100060303      * ¯¯¯¯¯  (3 iniziali - identificativi del file -
091200060303      *         3 finali   - identificativi del campo)
091300060303      *        SE si devessero elaborare campi con nomi più lunghi
091400060303      *        occorrerà rivedere la gestione della schiera $Sql_Fld1!
091500060303      *
091600060303      * N.B.2: La virgola NON deve essere inserita nell'ultimo elemento
091700060303      * ¯¯¯¯¯  della schiera $Sql_Fld2 !!!
091800060303      *
091900060303     c                   eval      xx      = (xx - 1) * 3 + 1
092000060303     c                   eval      xy      = 1
092100060303      *
092200060303do  1c                   dow       xy     <= %elem($Sql_Fld1)
092300060303      *
092400060306if  2c                   if        %subst($Sql_Fld1(xy):xx:3) <> *blanks
092500060303      *
092600060303if  3c                   if        xy      = 1
092700060303     c                   eval      wSQL    = %trim(wSQL)
092800060303     c                                     + ' '
092900060303     c                                     + Wxxx
093000060303     c                                     + %subst($Sql_Fld1(xy):xx:3)
093100060303     c                                     + $Sql_Fld2(xy)
093200060303x   3c                   else
093300060303     c                   eval      wSQL    = %trim(wSQL)
093400060303     c                                     + ','
093500060303     c                                     + Wxxx
093600060303     c                                     + %subst($Sql_Fld1(xy):xx:3)
093700060303     c                                     + $Sql_Fld2(xy)
093800060303e   3c                   endif
093900060303      *
094000060307x   2c                   else
094100060307      *
094200060307sel 3c                   select
094300060307      * xxxRMA (15 alfanum): no in TITAS
094400060307w   3c                   when      xy      = 23                                 RMA: no TAS
094500060307     c                   eval      wSQL    = %trim(wSQL)
094600060307     c                                     + ',''               '''             15 alfanum.
094700060307     c                                     + $Sql_Fld2(xy)
094800060307      * xxxFVB (01 alfanum): no in FIVAB
094900060307w   3c                   when      xy      = 24                                 FVB: no VAB
095000060307     c                   eval      wSQL    = %trim(wSQL)
095100060307     c                                     + ','' '''                           01 alfanum.
095200060307     c                                     + $Sql_Fld2(xy)
095300060307      * xxxFGS (03 / 0 num): no in FNBLP/FNARB e TITAS
095400060322w   3c***                when      xy      = 25                                 FGS: no BLP/ARB/TAS
095500060322     c***                eval      wSQL    = %trim(wSQL)
095600060322     c***                                  + ',''000'''                         03 num.
095700060322     c***                                  + $Sql_Fld2(xy)
095800060307      * xxxCCA (01 alfanum): no in FNBLP/FNARB e FIVAB
095900060307w   3c                   when      xy      = 26                                 CCA: no BLP/ARB/VAB
096000060307     c                   eval      wSQL    = %trim(wSQL)
096100060322     c                                     + ','' '''                           01 alfanum.
096200060307     c                                     + $Sql_Fld2(xy)
096300060307e   3c                   endsl
096400060307      *
096500060303e   2c                   endif
096600060303      *
096700060303     c                   eval      xy      = xy + 1
096800060303e   1c                   enddo
096900060303      *
097000060303     c                   ENDSR
097100060301      *
097200060301      *---------------------------------------------------------------*
097300060303      *?Dichiarazione e Apertura cursore                             ?*
097400060301      *---------------------------------------------------------------*
097500060303     c     OpenCursor    BEGSR
097600060301      *
097700060301      * Dichiarazione cursore
097800060301      *
097900060301     c/exec sql
098000060301     c+     PREPARE S1 FROM :wSQL
098100060301     c/end-exec
098200060301     c*
098300060301     c/exec sql
098400060301     c+     DECLARE C1 CURSOR FOR S1
098500060301     c/end-exec
098600060301      *
098700060301      * Apertura del cursore
098800060301      *
098900060301     c/exec sql
099000060301     c+     OPEN C1
099100060301     c/end-exec
099200060301      *
099300060301     c                   ENDSR
099400060302      *
099500060302      *---------------------------------------------------------------*
099600060302      *?Apertura archivi                                             ?*
099700060302      *---------------------------------------------------------------*
099800060302     c     sr_Open       BEGSR
099900060302      *
100000060302      * Bolle Sede
100100060302if  1c                   if        P76pav  = 'S'
100200060302      * - cancellazione wrk-file
100300060302     c/exec SQL
100400060302     c+   DELETE from WFSI760F
100500060302     c/end-exec
100600060302     c                   open      WFSI760F
100700060302e   1c                   endif
100800060302      *
100900060302     c                   ENDSR
101000060302      *
101100060302      *---------------------------------------------------------------*
101200060302      *?Stampa pagina iniziale con i parametri di lancio             ?*
101300060302      *---------------------------------------------------------------*
101400060302     c     sr_PrtParm    BEGSR
101500060302      *
101600060302      * Decodifica parametri
101700060302      * - Tipo P.O.
101800060302sel 1c                   select
101900060302w   1c                   when      P76pav  = 'P'
102000060302     c                   eval      WPPpav  = 'Partenza'
102100060302w   1c                   when      P76pav  = 'A'
102200060302     c                   eval      WPPpav  = 'Arrivo'
102300060302if  1c                   when      P76pav  = 'D'
102400060302     c                   eval      WPPpav  = 'Dkt'
102500060302if  1c                   when      P76pav  = 'S'
102600060302     c                   eval      WPPpav  = 'Sede'
102700060302e   1c                   endsl
102800060302      * - Inversione date
102900060302     c                   reset                   WLBdat
103000060302     c                   eval      G08inv  = P76ds1
103100060302     c                   call      'XSRDA8'
103200060302     c                   parm                    WLBdat
103300060302     c                   eval      WPPds1  = G08dat
103400060302     c                   reset                   WLBdat
103500060302     c                   eval      G08inv  = P76ds2
103600060302     c                   call      'XSRDA8'
103700060302     c                   parm                    WLBdat
103800060302     c                   eval      WPPds2  = G08dat
103900060302      * - Decodifica cliente
104000060302if  1c                   if        P76ksc <> *zeros
104100060302     c                   clear                   TIBS69ds
104200060302     c                   eval      I69sif  = KNSIF
104300060302     c                   eval      I69kac  = P76ksc
104400060302     c                   clear                   DS_cnaco
104500060302     c                   clear                   DS_cnind
104600060302     c                   clear                   DS_cnclp
104700060302     c                   clear                   DS_fncls
104800060302     c                   call      'TIBS69R'
104900060302     c                   parm                    TIBS69ds
105000060302     c                   parm                    DS_cnaco
105100060302     c                   parm                    DS_cnind
105200060302     c                   parm                    DS_cnclp
105300060302     c                   parm                    DS_fncls
105400060302if  2c                   if        O69err <> *on
105500060302     c                   movel     ACOrag        WPPksc
105600060302e   2c                   endif
105700060302e   1c                   endif
105800060302      * - Decodifica Lnp
105900060302if  1c                   if        P76lnp <> *zeros
106000060302     c     P76lnp        chain     AZORG
106100060302if  2c                   if        %found(AZORG01L)
106200060302     c                   movel     ORGdes        WPPlnp
106300060302e   2c                   endif
106400060302e   1c                   endif
106500060302      * - Decodifica Lna
106600060302if  1c                   if        P76lna <> *zeros
106700060302     c     P76lna        chain     AZORG
106800060302if  2c                   if        %found(AZORG01L)
106900060302     c                   movel     ORGdes        WPPlna
107000060302e   2c                   endif
107100060302e   1c                   endif
107200060302      * Stampa
107300060302     c                   except    PRTtxt
107400060302     c                   except    PRTprm
107500060302      *
107600060308     c***                eval      *inOF   = *on
107700060308     c                   except    PRTtxt                                       *testata
107800060308     c                   except    PRTtx2                                       *intestazione
107900060308     c                   eval      *inOF   = *off
108000060302      *
108100060302     c                   ENDSR
108200060301      *
108300060301      *---------------------------------------------------------------*
108400060301      *?Lettura cursore                                              ?*
108500060301      *---------------------------------------------------------------*
108600060301     c     ReadCursor    BEGSR
108700060301      *
108800060303     c                   reset                   $RecOK
108900060303     c                   clear                   ds_SQL
109000060302      *
109100060301     c/exec sql
109200060303     c+     FETCH NEXT FROM C1 INTO :ds_SQL
109300060301     c/end-exec
109400060301      *
109500060301sel 1c                   select
109600060301w   1c                   when      SQLcod  =  100
109700060301     c                   eval      $EoF    =  *on
109800060301w   1c                   when      SQLcod  <  *zeros
109900060301     c                   eval      $Err    =  *on
110000060301     c                   eval      $Eof    =  *on
110100060301x   1c                   other
110200060301     c                   exsr      Selez_Rec
110300060301e   1c                   endsl
110400060301      *
110500060301     c                   ENDSR
110600060301      *
110700060301      *---------------------------------------------------------------*
110800060301      *?Selezioni ulteriori                                          ?*
110900060301      *---------------------------------------------------------------*
111000060301     c     Selez_Rec     BEGSR
111100060301      *
111200060303     c*** già fatto:     reset                   $RecOK
111300060302      *
111400060302      * Esclusione P.O. arrivo bolle estero
111500060302     c                   eval      xy      = 1
111600060302     c     SQLlna        lookup    $PO(xy)                                10
111700060303if  1c                   if            NOT *in10                                *non trovata o
111800060303     c                             or  $POfie(xy) = 'E'                          *filiale estera
111900060302     c                   leavesr                                                *record non valido
112000060302e   1c                   endif
112100060302      *
112200060302      * Esclusioni rispetto alla gestione utenti
112300060302if  1c                   if            DUTlpo <> 'S'
112400060302     c                             and P76pav  = 'D'                            DISCHETTO
112500060302     c     SQLfgs        lookup    £1                                     10
112600060302if  2c                   if        NOT *in10
112700060302     c                   leavesr                                                *record non valido
112800060302e   2c                   endif
112900060302e   1c                   endif
113000060302      *
113100060302      * Se P.O. partenza POSTE: il terminal è se stessa
113200060302     c                   eval      xy      = 1
113300060302     c     SQLlnp        lookup    $PO(xy)                                10
113400060302if  1c                   if        NOT *in10                                    *no trovato
113500060302     c                   leavesr                                                *record non valido
113600060302x   1c                   else                                                   *trovato
113700060302if  2c                   if        $POntw(xy)  = 'PPT'                          *filiale ntw POSTE
113800060302     c                   eval      SQLtfp  = SQLlnp                             *terminal = p.o.
113900060302e   2c                   endif
114000060302e   1c                   endif
114100060302      *
114200060302      * Record OK
114300060302     c                   eval      $RecOK  = *on
114400060301      *
114500060301     c                   ENDSR
114600060302      *
114700060302      *---------------------------------------------------------------*
114800060303      *?Stampa                                                       ?*
114900060302      *---------------------------------------------------------------*
115000060302     c     sr_Print      BEGSR
115100060302      *
115200060302      * Stampa totali a rottura
115300060302sel 1c                   select
115400060302      * - P.O. partenza
115500060302w   1c                   when      SQLlnp <> SAVlnp
115600060302     c                   exsr      sr_TotNRS
115700060302     c                   exsr      sr_TotLNP
115800060309      * - Serie / Cliente
115900060309w   1c                   when          SQLnrs <> SAVnrs
116000060309     c                             or  SQLksc <> SAVksc
116100060302     c                   exsr      sr_TotNRS
116200060302e   1c                   endsl
116300060302      *
116400060302      * Elaborazione dettaglio
116500060302if  1c                   if            P76pav <> 'D'                            NO DISCHETTO
116600060302     c                             and SQLnrs <> *zeros                         NO disk B
116700060302     c                             and SQLfvb <> 'R'                            NO volume reale
116800060302     c                   clear                   SQLvlb
116900060302e   1c                   endif
117000060302      *
117100060302if  1c                   if        P76pav  = 'D'                                DISCHETTO
117200060302     c                   clear                   FNLV55ds
117300060302     c                   eval      D55tpt  = 'P'
117400060302     c                   eval      D55lin  = SQLlnp
117500060303     c                   eval      D55drf  = SQLaas * 10000 + SQLmgs            *data di riferimento
117600060302     c                   call      'FNLV55R'
117700060302     c                   parm                    FNLV55ds
117800060302if  2c                   if        D55err <> *blanks
117900060302     c                   eval      D55tfp  = SQLlnp
118000060302e   2c                   endif
118100060302     c                   eval      SQLtfp  = D55tfp                             *terminal partenza
118200060302e   1c                   endif
118300060302      *
118400060302      * Controlla l'instradamento e stampa eventuali differenze
118500060302     c                   exsr      sr_ChkINS
118600060302      *
118700060302     c                   ENDSR
118800060302      *
118900060302      *---------------------------------------------------------------*
119000060302      *?Rottura di livello: Numero Serie / Cliente                   ?*
119100060302      *---------------------------------------------------------------*
119200060302     c     sr_TotNRS     BEGSR
119300060302      *
119400060302      * Stampa i totali
119500060307if  1c                   IF        SAVksc <> *loval
119600060302     c                   exsr      sr_RepInfo
119700060302     c                   eval      WPDtxt  = ' TOTALE '
119800060302     c                                     + %editw(SAVksc:'   .    ')
119900060302     c                                     + ' '
120000140910     c                                     + WTSRag
120100060302     c                                     + ' Lnp: '
120200060302     c                                     + %editc(SAVlnp:'1')
120300060302     c                                     + ' Nrs: '
120400060302     c                                     + %editc(SAVnrs:'1')
120500061017     c                                     + WPDgesPkb
120600060303     c                                     + WPDsegSeq
120700060303     c                                     + WPDverCap
120800060302     c                                     + ' Sped:'
120900060302     c                                     + %editc(wContaRotTot:'2')
121000060302     c                                     + ' Colli:'
121100060413     c                                     + %editc(wColliRotTot:'2')
121200060302     c                                     + ' di cui errate Sped:'
121300060302     c                                     + %editc(wContaRot:'2')
121400060302     c                                     + ' Colli:'
121500060302     c                                     + %editc(wColliRot:'2')
121600060302     c                                     + ' a Sup.:'
121700060302     c                                     + %editc(wSupRot:'2')
121800060322if  1c                   if        *inOF                                        *fine pagina
121900060322     c                   except    PRTtxt                                       *testata
122000060322     c                   except    PRTtx2                                       *intestazione
122100060322     c                   eval      *inOF   = *off
122200060322e   1c                   endif
122300060302     c                   except    PRTtot                                       *totali
122400061018     c                   if        p76obs='S'
122500061018     c                   eval      WPDtxtobs  = 'di cui Sped. Obsol:'
122600061018     c                                        + %editc(wContaRotob:'2')
122700061018     c                                     + ' Colli:'
122800061018     c                                     + %editc(wColliRotob:'2')
122900061018     c                   except    PRTtotobs                                    *totali
123000061018     c                   endif
123100060302     c                   except    PRTnul                                       *riga vuota
123200060302     c                   if        P76pav  = 'S'
123300060302     c                   exsr      sr_WriteWF
123400060302     c                   endif
123500060302e   1c                   ENDIF
123600060302      *
123700060302      * Memorizza la nuova serie in un deposito
123800060302     c                   eval      SAVnrs  = SQLnrs
123900060302     c                   eval      SAVksc  = SQLksc
124000060302      *
124100060302      * Azzera i totali
124200060302     c                   clear                   wContaRot
124300060302     c                   clear                   wContaRotTot
124400060302     c                   clear                   wColliRot
124500060302     c                   clear                   wColliRotTot
124600060302     c                   clear                   wContaDir
124700060302     c                   clear                   wContaDirTot
124800060302     c                   clear                   wSupRot
124900060302     c                   clear                   wSupRotTot
125000061018     c                   clear                   wcontarotob
125100061018     c                   clear                   wcollirotob
125200060302      *
125300060302     c                   ENDSR
125400060302      *
125500060302      *---------------------------------------------------------------*
125600060302      *?Rottura di livello: P.O. di Partenza                         ?*
125700060302      *---------------------------------------------------------------*
125800060302     c     sr_TotLNP     BEGSR
125900060302      *
126000060302      * Stampa i totali
126100060307if  1c                   IF        SAVlnp <> *loval
126200060302     c                   eval      WPDtxt  = ' TOTALE P.O.            '
126300060302     c                                     + '                      '
126400060302     c                                     + ' Sped:'
126500060302     c                                     + %editc(wContaPoTot:'2')
126600060302     c                                     + ' Colli:'
126700060302     c                                     + %editc(wColliPoTot:'2')
126800060302     c                                     + ' di cui errate Sped:'
126900060302     c                                     + %editc(wContaPo:'2')
127000060302     c                                     + ' Colli:'
127100060302     c                                     + %editc(wColliPo:'2')
127200060302     c                                     + ' a Sup.:'
127300060302     c                                     + %editc(wSupPo:'2')
127400061018     c                   if        p76obs ='S'
127500061018     c                   eval      WPDtxt  = %trim(wpdtxt)
127600061018     c                                     + ' di cui Sped. Obs:'
127700061018     c                                     + %editc(wContaPoob:'2')
127800061018     c                                     + ' Colli:'
127900061018     c                                     + %editc(wColliPoob:'2')
128000061018     c                   endif
128100060302     c                   except    PRTtot                                       *totali
128200060302     c                   except    PRTnul                                       *riga vuota
128300060308      * - Imposto la condizione per il salto pagina (e stampa testata)
128400060308     c                   if            P76pav  = 'P'
128500060308     c                             or  P76pav  = 'A'
128600060308     c                             or  P76pav  = 'S'
128700060308     c                   eval      *inOF   = *on
128800060308     c                   endif
128900060302e   1c                   ENDIF
129000060302      *
129100060302      * Memorizza la nuova linea in un deposito
129200060302     c                   eval      SAVlnp  = SQLlnp
129300060302      *
129400060302      * Azzera i totali
129500060302     c                   clear                   wContaPo
129600060302     c                   clear                   wContaPoTot
129700060302     c                   clear                   wColliPo
129800060302     c                   clear                   wColliPoTot
129900060302     c                   clear                   wSupPo
130000060302     c                   clear                   wSupPoTot
130100061018     c                   clear                   wContaPoob
130200061018     c                   clear                   wColliPoob
130300060302      *
130400060302     c                   ENDSR
130500060302      *
130600060302      *---------------------------------------------------------------*
130700060302      *?Rottura di livello: Finale                                   ?*
130800060302      *---------------------------------------------------------------*
130900060302     c     sr_TotGEN     BEGSR
131000060302      *
131100060302      * Stampa il riepilogo dei dati
131200060302     c                   eval      WPDtxt  = ' TOTALE GENERALE        '
131300060302     c                                     + '                      '
131400060302     c                                     + ' Sped:'
131500060302     c                                     + %editc(wContaTot:'2')
131600060302     c                                     + ' Colli:'
131700060302     c                                     + %editc(wColliTot:'2')
131800060302     c                                     + ' di cui errate Sped:'
131900060302     c                                     + %editc(wConta:'2')
132000060302     c                                     + ' Colli:'
132100060302     c                                     + %editc(wColli:'2')
132200060302     c                                     + ' a Sup.:'
132300060302     c                                     + %editc(wSup:'2')
132400061018     c                   if        p76obs ='S'
132500061018     c                   eval      WPDtxt  = %trim(wpdtxt)
132600061018     c                                     + ' di cui Sped. Obs:'
132700061018     c                                     + %editc(wContaob:'2')
132800061018     c                                     + ' Colli:'
132900061018     c                                     + %editc(wColliob:'2')
133000061018     c                   endif
133100060302     c                   except    PRTnul                                       *riga vuota
133200060302     c                   except    PRTtot                                       *totali
133300060302     c                   except    PRTend                                       *chiusura
133400060302      *
133500060302     c                   ENDSR
133600060302      *
133700060302      *---------------------------------------------------------------*
133800060302      *?Controlla l'instradamento e Stampa eventuali differenze      ?*
133900060302      *---------------------------------------------------------------*
134000060302     c     sr_ChkINS     BEGSR
134100060302      *
134200060302     c                   eval      $Err    = *off                               *errore
134300060306      *
134400060306      * Pulisce i campi di decodifica/descrizione
134500060306     c                   clear                   WPDftc                         *consegna particolar
134600060306     c                   clear                   WPDdat                         *data spedizione
134700060306     c                   clear                   WPDxla                         *lna standard
134800060306     c                   clear                   WPDxzc                         *znc standard
134900060306     c                   clear                   WPDxls                         *lna sotto
135000060306     c                   clear                   WPDxzs                         *znc sotto
135100060306     c                   clear                   WPDxlo                         *lna oltre
135200060306     c                   clear                   WPDxzo                         *znc oltre
135300060306     c                   clear                   WPDxzf                         *znc f.deposito
135400060306     c                   clear                   WPDfca                         *S=C/Assegno
135500060306     c                   clear                   WPDlid                         *livello scelta
135600060306     c                   clear                   WPDlil                         *livello lna/znc
135700060306     c                   clear                   WPDloc1                        *località (1ª parte)
135800060306     c                   clear                   WPDloc2                        *località (2ª parte)
135900060306     c                   clear                   WPDtxt                         *testo generico
136000061018     c                   clear                   WPDtxtobs                      *testo generico
136100060306     c                   clear                   WPEmsg1                        *messaggio di errore
136200060306     c                   clear                   WPEmsg2                        *messaggio di errore
136300060306     c                   clear                   WPDverCap
136400060306     c                   clear                   WPDgesPkb
136500060306     c                   clear                   WPDsegSeq
136600061017     c                   clear                   WPDobs
136700060302      *
136800060302      * Reperisce il Tipo Porto e l'esistenza del Contrassegno
136900060302     c                   exsr      sr_ChkCBO
137000060302      *
137100060302      * Lancia il programma di calcolo instradamento
137200060302     c                   clear                   TISI95ds                       *azzera i parametri
137300060302     c                   eval      I95tcn  = '7'                                *tipo controllo
137400060303     c                   eval      I95nar  = SQLnzd                             *nazione
137500060303     c                   eval      I95cap  = SQLcad                             *cap
137600060303     c                   eval      I95loc  = SQLlod                             *località
137700060302     c                   eval      I95prv  = SQLprd                             *provincia
137800060303     c                   eval      I95dat  = SQLaas * 10000 + SQLmgs            *data di riferimento
137900060303     c                   eval      I95tpo  = WWWtpo                             *tipo porto
138000060303     c                   eval      I95fca  = WPDfca                             *contrassegno
138100060302     c                   eval      I95ffd  = SQLffd                             *fermo deposito
138200060302     c                   eval      I95tsp  = SQLtsp                             *tipo servizio
138300060303     c                   eval      I95tfp  = SQLtfp                             *terminal partenza
138400060302     c                   eval      I95fre  = 'S'                                *controllo eccezioni
138500060302     c                   eval      I95lkg  = SQLpkb                             *peso bollettato
138600060302     c                   eval      I95lmc  = SQLvlb                             *volume bollettato
138700060306     c                   eval      I95fi2  = SQLtc1 + SQLtc2                    *consegne particolar
138800060302      *
138900060302      * Verifica se P.O.arrivo è P.O. FedEx
139000060302if  1c                   if        SQLlna  > *zeros
139100060302     c     SQLlna        lookup    $POfed                                 10
139200060302if  2c                   if        *in10
139300060302     c                   eval      I95fi1  = 'S'
139400060302e   2c                   endif
139500060302e   1c                   endif
139600060302      *
139700060302     c                   call      'TISI95R'
139800060302     c                   parm                    TISI95ds
139900060302      *
140000060302      * Controlla instradamento calcolato con quello scritto sulla
140100060302      *   bolla
140200061017     c                   if        o95lna<>sqllna or
140300061017     c                             (p76obs='S' and
140400061017     c                             (o95lf1='O' or o95cf1='O')) or
140500061017     c                             (p76fel='N' and
140600061017     c                             (O95znc <> SQLznc
140700061017     c                             or  O95cap <> SQLcad
140800061017     c                             or (O95prv <> SQLprd
140900061017     c                             and O95pra <> SQLprd)))
141000061017     c                   eval      $Err     = *on                               *errore
141100061017     c                   exsr      sr_PrtERR                                    *stampa errore
141200061017     c                   endif
141300060302      *
141400060302      * Effettua sempre il conteggio del totale spedizioni e colli
141500060302      *   di tutte le spedizioni
141600060302     c                   add       1             wContaTot
141700060302     c                   add       1             wContaPoTot
141800060302     c                   add       1             wContaRotTot
141900060302     c                   add       SQLncl        wColliTot
142000060302     c                   add       SQLncl        wColliPoTot
142100060302     c                   add       SQLncl        wColliRotTot
142200060302      *
142300060302      * Controlla se la bolla è dirottata
142400060302if  1c                   if            P76pav  = 'S'                            NO DISCHETTO
142500060302     c                             and SQLcca  = '1'
142600060302     c                   add       1             wContaDirTot
142700060302e   1c                   endif
142800060302      *
142900060302      * Controlla se la bolla è destinata a supermercato
143000060302if  1c                   if            SQLtc1 = 'S'
143100060302     c                             or  SQLtc2 = 'S'
143200060302     c                   if        SQLtc1  = 'S'
143300060303     c                   eval      WPDftc  = SQLtc1
143400060302     c                   endif
143500060302     c                   if        SQLtc2  = 'S'
143600060303     c                   eval      WPDftc  = SQLtc2
143700060302     c                   endif
143800060302     c                   add       1             wSupTot
143900060302     c                   add       1             wSupPoTot
144000060302     c                   add       1             wSupRotTot
144100060302e   1c                   endif
144200060302      *
144300060302     c                   ENDSR
144400060302      *
144500060302      *---------------------------------------------------------------*
144600060302      *?Reperisce il Tipo Porto e l'Esistenza del Contrassegno       ?*
144700060302      *---------------------------------------------------------------*
144800060302     c     sr_ChkCBO     BEGSR
144900060302      *
145000060302     c                   eval      WWWtpo  = 'F'
145100060303     c                   eval      WPDfca  = 'N'
145200060302      *
145300060302     c                   eval      xy      = 1
145400060303     c     SQLcbo        lookup    $3A(xy)                                10
145500060302      *
145600060302if  1c                   if        *in10
145700060302      * - Tipo Porto
145800060302if  2c                   if            $3Atb1(xy) = 'A'
145900060302     c                             or  $3Atb2(xy) = 'A'
146000060302     c                   eval      WWWtpo  = 'A'                                *tipo porto
146100060302e   2c                   endif
146200060302      * - Flag C/Assegno
146300060303if  2c                   if        $3Afca(xy)    <> *zeros
146400060303     c                   eval      WPDfca  = 'S'                                *c/assegno
146500060302e   2c                   endif
146600060302      *
146700060302e   1c                   endif
146800060302      *
146900060302     c                   ENDSR
147000060302      *
147100060302      *---------------------------------------------------------------*
147200060302      *?Reperimento informazioni anagrafiche sulla Serie             ?*
147300060302      *---------------------------------------------------------------*
147400060302     c     sr_RepInfo    BEGSR
147500060302      *
147600060302     c                   clear                   ds3C
147700060302     c                   clear                   ds1B
147800060302      *
147900060302     c                   clear                   WPDgesPkb
148000060302     c                   clear                   WPDsegSeq
148100060302     c                   clear                   WPDverCap
148200060302      *
148300060302      * - tabella "3C" = Clienti con DiskC
148400060302     c*** già così       eval      TBLkut  = 1
148500060302     c                   eval      TBLcod  = '3C'
148600060302     c                   movel(p)  SAVksc        TBLkey
148700060302     c     K03TABEL      chain     TABEL
148800060302if  1c                   if            %found(TABEL00F)
148900060302     c                             and TBLflg = *blanks
149000060302     c                   movel     TBLuni        ds3C
149100060302      * - tabella "1B" = Codici Trattamento Merce
149200060302     c*** già così       eval      TBLkut  = 1
149300060302     c                   eval      TBLcod  = '1B'
149400060302     c                   movel(p)  §3Cctm        TBLkey
149500060302     c     K03TABEL      chain     TABEL
149600060302if  cc                   if            %found(TABEL00F)
149700060302     c                             and TBLflg = *blanks
149800060302     c                   movel     TBLuni        ds1B
149900060302e   2c                   endif
150000060302e   1c                   endif
150100060302      *
150200060302      * Considerazioni
150300060302if  1c                   if        §3Cpes  = 'N'
150400060302     c                   eval      WPDgesPkb  = ' NON gest. peso'
150500060302e   1c                   endif
150600060302      *
150700060302sel 1c                   select
150800060302w   1c                   when      §3Cnwc  = 'L'
150900060302     c                   eval      WPDverCap  = ' Vers. capp.: 3'
151000060302w   1c                   when      §3Cnwc  = 'S'
151100060302     c                   eval      WPDverCap  = ' Vers. capp.: 2'
151200060302w   1c                   when      §3Cnwc  = *blanks
151300060302     c                   eval      WPDverCap  = ' Vers. capp.: 1'
151400060302e   1c                   endsl
151500060302      *
151600060302if  1c                   if        §1Bfg7  = 'S'
151700060302     c                   eval      WPDsegSeq  = ' Segn. NON seq.'
151800060302e   1c                   endif
151900140910      *
152000140910     c                   clear                   TIBS69ds
152100140910     c                   eval      I69sif  = KNSIF
152200140910     c                   eval      I69kac  = savksc
152300140910     c                   clear                   DS_cnaco
152400140910     c                   clear                   DS_cnind
152500140910     c                   clear                   DS_cnclp
152600140910     c                   clear                   DS_fncls
152700140910     c                   call      'TIBS69R'
152800140910     c                   parm                    TIBS69ds
152900140910     c                   parm                    DS_cnaco
153000140910     c                   parm                    DS_cnind
153100140910     c                   parm                    DS_cnclp
153200140910     c                   parm                    DS_fncls
153300140910if  2c                   if        O69err <> *on
153400140910     c                   movel     ACOrag        WTSRag
153500140910e   2c                   endif
153600060302      *
153700060302     c                   ENDSR
153800060302      *
153900060302      *---------------------------------------------------------------*
154000060302      *?Stampa Errore                                                ?*
154100060302      *---------------------------------------------------------------*
154200060302     c     sr_PrtERR     BEGSR
154300060302      *
154400060302      * Messaggio di errore
154500060302     c***                movel     O95msg        WPEmsg1
154600060302     c                   eval      WPEmsg1 = %subst(O95msg:1:75)
154700060302      * Data spedizione
154800060302     c                   reset                   WLBdat
154900060303     c                   eval      G08inv  = SQLaas * 10000 + SQLmgs            *data spedizione
155000060302     c                   call      'XSRDA8'
155100060302     c                   parm                    WLBdat
155200060302     c                   eval      WPDdat  = G08dat                             *data spedizione
155300060302      * Livello scelta
155400060302if  1c                   if           (O95ela > *zeros
155500060302     c                             or  O95ezc > *zeros
155600060302     c                             or  O95elo > *zeros
155700060302     c                             or  O95ezo > *zeros
155800060302     c                             or  O95els > *zeros
155900060302     c                             or  O95ezs > *zeros)
156000060302     c                             and O95lid = '3'
156100060302     c                   movel     '5'           O95lid                         *forzato
156200060302e   1c                   endif
156300060302sel 1c                   select
156400060302w   1c                   when      O95lid  = '1'
156500060302     c                   movel     $Decod(1)     WPDlid
156600060302w   1c                   when      O95lid  = '2'
156700060302     c                   movel     $Decod(2)     WPDlid
156800060302w   1c                   when      O95lid  = '3'
156900060302     c                   movel     $Decod(3)     WPDlid
157000060302w   1c                   when      O95lid  = '4'
157100060302     c                   movel     $Decod(4)     WPDlid
157200060302w   1c                   when      O95lid  = '5'
157300060302     c                   movel     $Decod(5)     WPDlid
157400060302e   1c                   endsl
157500060302      * Livello lna/zc
157600060302sel 1c                   select
157700060302w   1c                   when          O95lil  = *blanks
157800060302     c                             and O95lna  > *zeros                         *se lna=0 NO scritta
157900060302     c                   movel     $Decod(6)     WPDlil
158000060302w   1c                   when      O95lil  = 'S'
158100060302     c                   movel     $Decod(7)     WPDlil
158200060302w   1c                   when      O95lil  = 'O'
158300060302     c                   movel     $Decod(8)     WPDlil
158400060302w   1c                   when      O95lil  = 'F'
158500060302     c                   movel     $Decod(9)     WPDlil
158600060302e   1c                   endsl
158700060302      *
158800060302if  1c                   if        O95cla  > *zeros                             *CAP pieni
158900060302     c                   eval      WPDxla  = O95cla                             *lna standard
159000060302     c                   eval      WPDxzc  = O95czc                             *znc standard
159100060302     c                   eval      WPDxls  = O95cls                             *lna sotto
159200060302     c                   eval      WPDxzs  = O95czs                             *znc sotto
159300060302     c                   eval      WPDxlo  = O95clo                             *lna oltre
159400060302     c                   eval      WPDxzo  = O95czo                             *znc oltre
159500060302     c                   eval      WPDxzf  = O95czf                             *znc f.deposito
159600060302e   1c                   endif
159700060302      *
159800060302if  1c                   if        O95lla > *zeros                              *LOCALITA' pieni
159900060302     c                   eval      WPDxla  = O95lla                             *lna standard
160000060302     c                   eval      WPDxzc  = O95lzc                             *znc standard
160100060302     c                   eval      WPDxls  = O95lls                             *lna sotto
160200060302     c                   eval      WPDxzs  = O95lzs                             *znc sotto
160300060302     c                   eval      WPDxlo  = O95llo                             *lna oltre
160400060302     c                   eval      WPDxzo  = O95lzo                             *znc oltre
160500060302     c                   eval      WPDxzf  = O95lzf                             *znc f.deposito
160600060302e   1c                   endif
160700060302      *
160800060302if  1c                   if        o95ela > *zeros                              *ECCEZIONI pieni
160900060302     c                   eval      WPDxla  = O95ela                             *lna standard
161000060302     c                   eval      WPDxzc  = O95ezc                             *znc standard
161100060302     c                   eval      WPDxls  = O95els                             *lna sotto
161200060302     c                   eval      WPDxzs  = O95ezs                             *znc sotto
161300060302     c                   eval      WPDxlo  = O95elo                             *lna oltre
161400060302     c                   eval      WPDxzo  = O95ezo                             *znc oltre
161500060303     c*** no questo:     eval      WPDxzf  = O95ezf                             *znc f.deposito
161600060302e   1c                   endif
161700060302      *
161800060302      * Località
161900060303     c                   eval      WPDloc1 = %subst(SQLlod:01:25)
162000060303     c                   eval      WPDloc2 = %subst(SQLlod:26:10)
162100061018     c* obsoleto
162200061018     c                   clear                   wpdobs
162300061018     c                   select
162400061018     c                   when      o95cf1='O'
162500061018     c                   eval      wpdobs='C'
162600061018     c                   when      o95lf1='O'
162700061018     c                   eval      wpdobs='L'
162800061018     c                   endsl
162900060302      *
163000060302      *
163100060302      * Incremento totali
163200061017     c                   if        o95lna<>sqllna or
163300061017     c                             (p76fel='N' and
163400061017     c                             (O95znc <> SQLznc
163500061017     c                             or  O95cap <> SQLcad
163600061017     c                             or (O95prv <> SQLprd
163700061017     c                             and O95pra <> SQLprd)))
163800060302     c                   add       1             wConta
163900060302     c                   add       1             wContaPo
164000060302     c                   add       1             wContaRot
164100060302     c                   add       SQLncl        wColli
164200060302     c                   add       SQLncl        wColliPo
164300060302     c                   add       SQLncl        wColliRot
164400061017     c                   endif
164500061017     c                   if        p76obs='S' and
164600061017     c                             (o95lf1='O' or o95cf1='O')
164700061017     c                   add       1             wContaob
164800061017     c                   add       1             wContaPoob
164900061017     c                   add       1             wContaRotob
165000061017     c                   add       SQLncl        wColliob
165100061017     c                   add       SQLncl        wColliPoob
165200061017     c                   add       SQLncl        wColliRotob
165300061017     c                   endif
165400060302      *
165500060302      * Controlla se la bolla è dirottata
165600060303if  1c                   if            P76pav  = 'S'
165700060303     c                             and SQLcca  = '1'
165800060302     c                   add       1             wContaDir
165900060302e   1c                   endif
166000060302      *
166100060302      * Controlla se la bolla è destinata a supermercato
166200060302if  1c                   IF            SQLtc1  = 'S'
166300060302     c                             or  SQLtc2  = 'S'
166400060302if  2c                   if        SQLtc1  = 'S'
166500060302     c                   eval      WPDftc  = SQLtc1
166600060302e   2c                   endif
166700060302if  2c                   if        SQLtc2  = 'S'
166800060302     c                   eval      WPDftc  = SQLtc2
166900060302e   2c                   endif
167000060302     c                   add       1             wSup
167100060302     c                   add       1             wSupPo
167200060302     c                   add       1             wSupRot
167300060302e   1c                   ENDIF
167400060414      *
167500060414      *
167600060414      * Stampa
167700060420if  1c                   IF        P76tot  = 'N'
167800060420if  2c                   if        *inOF                                        *fine pagina
167900060414     c                   except    PRTtxt                                       *testata
168000060414     c                   except    PRTtx2                                       *intestazione
168100060414     c                   eval      *inOF   = *off
168200060420e   2c                   endif
168300060303      *                  ________________
168400060302     c                   except    PRTdet                                       *dettaglio
168500060303      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
168600060420e   1c                   ENDIF
168700060302      *
168800060414      * (Verifico eventuali incongruenze tra CAP e PROVINCIA)
168900060302if  1c                   IF        O95lia <> '3'
169000060302      *
169100060420if  2c                   If        P76tot  = 'N'
169200060302     c                   movel(p)  $Decod(10)    WPEmsg2
169300060420if  3c                   if        *inOF                                        *fine pagina
169400060302     c                   except    PRTtxt                                       *testata
169500060302     c                   except    PRTtx2                                       *intestazione
169600060302     c                   eval      *inOF   = *off
169700060420e   3c                   endif
169800060302     c                   except    PRTerr2                                      *errore
169900060420e   2c                   EndIf
170000060302      *
170100060302x   1c                   ELSE
170200060302      *
170300060420if  2c                   If           (O95lna  = *zeros
170400060420     c                             or  O95err <> *blanks)
170500060420     c                             and P76tot  = 'N'
170600060420if  3c                   if        *inOF                                        *fine pagina
170700060420     c                   except    PRTtxt                                       *testata
170800060420     c                   except    PRTtx2                                       *intestazione
170900060420     c                   eval      *inOF   = *off
171000060420e   3c                   endif
171100060302     c                   except    PRTerr1                                      *errore
171200060420e   2c                   EndIf
171300060302      *
171400060302e   1c                   ENDIF
171500060302      *
171600060302     c                   ENDSR
171700060302      *
171800060302      *---------------------------------------------------------------*
171900060302      *?Scrittura singolo record nel work-file                       ?*
172000060302      *---------------------------------------------------------------*
172100060414     c     sr_WriteWF    BEGSR
172200060302      *
172300060302      * Reperisco i dati di classificazione geografica del cliente
172400060303     c                   movel     SAVksc        SI76fil
172500060302     c     SI76fil       chain     AZORG
172600060302     c                   if        %found(AZORG01L)
172700060303     c                   eval      SI76dis  = ORGfl3
172800060303     c                   eval      SI76ara  = ORGcar
172900060302     c                   endif
173000060303     c                   eval      SI76ksc  = SAVksc
173100140910     c                   eval      SI76rag  = WTSRag
173200060302     c                   eval      SI76lnp  = SAVlnp
173300060302     c                   eval      SI76nrs  = SAVnrs
173400060302     c                   eval      SI76gpb  = WPDgesPkb
173500060302     c                   eval      SI76gss  = WPDsegSeq
173600060302     c                   eval      SI76ver  = WPDverCap
173700060302     c                   eval      SI76spet = wContaRotTot
173800060302     c                   eval      SI76colt = wColliRotTot
173900060302     c                   eval      SI76spee = wContaRot
174000060302     c                   eval      SI76cole = wColliRot
174100060302     c                   eval      SI76dirt = wContaDirTot
174200060302     c                   eval      SI76dire = wContaDir
174300060302     c                   eval      SI76supt = wSupRotTot
174400060302     c                   eval      SI76supe = wSupRot
174500061018     c                   eval      SI76speo = wContaRotob
174600061018     c                   eval      SI76colo = wColliRotob
174700060302      *                  __________________
174800060302     c                   WRITE     WFSI7600
174900060303      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
175000060302     c                   ENDSR
175100060302      *
175200060302      *---------------------------------------------------------------*
175300060302      *?Chiusura archivi                                             ?*
175400060302      *---------------------------------------------------------------*
175500060302     c     sr_Close      BEGSR
175600060302      *
175700060302      * WorkFile Bolle Sede
175800060302if  1c                   if        P76pav  = 'S'
175900060302     c                   close     WFSI760F
176000060302e   1c                   endif
176100060302      *
176200060302     c                   ENDSR
176300060302      *
176400060302      *---------------------------------------------------------------*
176500060302      *?Operazioni finali                                            ?*
176600060302      *---------------------------------------------------------------*
176700060302     c     RoutFin       BEGSR
176800060302      *
176900060302      * Stampa totale ultima serie elaborata
177000060302     c                   exsr      sr_TotNRS
177100060302      *
177200060302      * Stampa totale ultimo P.O. di partenza elaborato
177300060302     c                   exsr      sr_TotLNP
177400060302      *
177500060302      * Stampa riepilogo dei dati
177600060302     c                   exsr      sr_TotGEN
177700060302      *
177800060302      * Operazioni di chiusura
177900060302     c                   exsr      sr_Close
178000060302      *
178100060302      * Chiusura programmi
178200060302     c                   clear                   TISI95ds
178300060303     c                   eval      I95tla  = 'C'
178400060302     c                   call      'TISI95R'
178500060302     c                   parm                    TISI95ds
178600060302      *
178700060302     c                   clear                   TIBS69ds
178800060302     c                   clear                   DS_cnaco
178900060302     c                   clear                   DS_cnind
179000060302     c                   clear                   DS_cnclp
179100060302     c                   clear                   DS_fncls
179200060302     c                   eval      I69tla  = 'C'
179300060302     c                   call      'TIBS69R'
179400060302     c                   parm                    TIBS69ds
179500060302     c                   parm                    DS_cnaco
179600060302     c                   parm                    DS_cnind
179700060302     c                   parm                    DS_cnclp
179800060302     c                   parm                    DS_fncls
179900060302      *
180000060302     c                   clear                   FNLV55ds
180100060302     c                   eval      D55tla  = 'C'
180200060302     c                   call      'FNLV55R'
180300060302     c                   parm                    FNLV55ds
180400060302      *
180500060302     c                   ENDSR
180600060224      *
180700060224      *?O U T P U T?--------------------------------------------------*
180800060224      *
180900060224      * Testata iniziale
181000060224     oPRTF198   e            PRTtxt            2
181100060224     o                       $Txt(1)             66
181200060224     o                       $Txt(2)            132
181300060224     o                       $Txt(3)            198
181400060224     o                       RSUT                20
181500060420     o                       SDSpgm              55
181600060420     o                       *date         y    150
181700060420     o                       wTime              + 5 '0 :  :  '
181800060306     o                       Page          z    198
181900060224      * Parametri di lancio
182000060420     o          e            PRTprm      3
182100060224     o                       $Txt(14)           132
182200060420     o          e            PRTprm      2
182300060224     o                       $Txt(15)           132
182400060224     o                       P76pav              94
182500060306     o                       WPPpav             111
182600060420     o          e            PRTprm      2
182700060224     o                       $Txt(16)           132
182800060224     o                       WPPds1        8    103
182900060224     o                       WPPds2        8    118
183000060224     o          e            PRTprm      1
183100060420     o                       $Txt(17)           132
183200060306     o                       P76ksc        z    100
183300060224     o                       WPPksc             131
183400060224     o          e            PRTprm      1
183500060420     o                       $Txt(18)           132
183600060306     o                       P76lnp        z     96
183700060306     o                       WPPlnp             131
183800060224     o          e            PRTprm      1
183900060420     o                       $Txt(19)           132
184000060306     o                       P76lna        z     96
184100060306     o                       WPPlna             131
184200060224     o          e            PRTprm      1
184300060420     o                       $Txt(20)           132
184400060306     o                       P76nrs        z     95
184500060420     o          e            PRTprm      1
184600060420     o                       $Txt(21)           132
184700060420     o                       P76fns              94
184800060414     o          e            PRTprm      1
184900060414     o                       $Txt(22)           132
185000060414     o                       P76fel              94
185100060414     o          e            PRTprm      1
185200060414     o                       $Txt(23)           132
185300060414     o                       P76tot              94
185400061018     o          e            PRTprm      1
185500061018     o                       $Txt(24)           132
185600061018     o                       P76obs              94
185700060224      * Testata del dettaglio
185800060224     o          e            PRTtx2      1
185900060224     o                       $Txt(4)             66
186000060224     o                       $Txt(4)            132
186100060224     o                       $Txt(4)            198
186200060224     o          e            PRTtx2      1
186300060224     o                       $Txt(5)             66
186400060224     o                       $Txt(8)            132
186500060224     o                       $Txt(11)           198
186600060224     o          e            PRTtx2      1
186700060224     o                       $Txt(6)             66
186800060224     o                       $Txt(9)            132
186900060224     o                       $Txt(12)           198
187000060224     o          e            PRTtx2      1
187100060224     o                       $Txt(4)             66
187200060224     o                       $Txt(4)            132
187300060224     o                       $Txt(4)            198
187400060224      * Dettaglio
187500060224     o          e            PRTdet      1
187600060303     o                       WPDdat        y     10
187700060303     o                       SQLlnp              14
187800060303     o                       SQLnrs        z     17
187900060303     o                       SQLnsp        z     25
188000060303     o                       SQLcbo              28
188100060303     o                       SQLksc        z     36
188200060303     o                       SQLnzd              40
188300060303     o                       SQLcad              50
188400060303     o                       WPDloc1             77
188500060303     o                       SQLprd              80
188600060303     o                       SQLtfp              84
188700060303     o                       SQLtsp              87
188800060303     o                       WPDftc              89
188900060303     o                       SQLffd              92
189000060303     o                       WPDfca              95
189100060303     o                       SQLpkb        4    103
189200060303     o                       SQLvlb        4    110
189300060303     o                       SQLlna             114
189400060303     o                       SQLznc             117
189500061017     o                                          121 '-->'
189600061017     o                       O95lna             125
189700061017     o                       O95znc             128
189800061017     o                       WPDlid             139
189900060303     o                       WPDlil             150
190000061018     o                       WPDxla             155
190100061018     o                       WPDxzc             158
190200061018     o                       WPDxls             163
190300061018     o                       WPDxzs             166
190400061018     o                       WPDxlo             170
190500061018     o                       WPDxzo             173
190600061018     o                       WPDxzf             177
190700061018     o                       SQLrmn        z    194
190800061018     o                       wpdobs             197
190900060302      * Segnalazione errore (1)
191000060302     o          e            PRTerr1     1
191100060302     o                       WPEmsg1            182
191200060302     o                       SQLrma             198
191300060302      * Segnalazione errore (2)
191400060302     o          E            PRTerr2     1
191500060307     o                       WPDloc2             62
191600060302     o                       WPEmsg2            191
191700060302      * Riga vuota
191800060302     o          e            PRTnul      2
191900060224      * Totale
192000060302     o          E            PRTtot      1
192100060302     o                       WPDtxt             198
192200061018     o          E            PRTtotobs   1
192300061018     o                       WPDtxtobs          198
192400060302      * Fine stampa
192500060302     o          e            PRTend      2
192600060224     o                       $Txt(26)           132
192700060224      *
192800060224      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?*
192900060224      *
193000060303** - $Sql_Fld1/$Sql_Fld2                Note (eccezioni)
193100060303aas       as SQLaas        1
193200060303mgs       as SQLmgs        2
193300060303lnp       as SQLlnp        3
193400060303nrs       as SQLnrs        4
193500060303nsp       as SQLnsp        5
193600060303cbo       as SQLcbo        6
193700060303kscccmccm as SQLksc        7            TASccm, VABccm
193800060303cad       as SQLcad        8
193900060303lod       as SQLlod        9
194000060303prd       as SQLprd       10
194100060303nzd       as SQLnzd       11
194200060303tfptfplnp as SQLtfp       12                    VABlnp...
194300060303tsp       as SQLtsp       13
194400060303tc1ftctc1 as SQLtc1       14            TASftc
194500060303tc2       as SQLtc2       15
194600060303ffd       as SQLffd       16
194700060303ncl       as SQLncl       17
194800060303pkb       as SQLpkb       18
194900060303vlb       as SQLvlb       19
195000060303lna       as SQLlna       20
195100060303znc       as SQLznc       21
195200060303rmn       as SQLrmn       22
195300060303rma---    as SQLrma       23            no TITAS
195400060303fvb   --- as SQLfvb       24            no FIVAB
195500060322lnp   fgs as SQLfgs       25            no FNBLP/FNARB/TITAS
195600060303---cca--- as SQLcca       26            no FNBLP/FNARB/FIVAB
195700060303** - $Sql_BLP ---------------------------------------------------------------*
195800060309from   FNBLP00F                                                                1  ex FNBLP01L
195900070222where  BLPffd <> 'M'                                                           2
196000060303   and BLPlnp in (                                                             3
196100060303   and BLPlna in (                                                             4
196200060303   and (BLPaas*10000+BLPmgs) between                                           5
196300060303   and BLPksc =                                                                6
196400060303   and BLPlnp =                                                                7
196500060303   and BLPlna =                                                                8
196600060303   and BLPnrs =                                                                9
196700060303   and BLPnrs <> 0                                                            10
196800060303** - $Sql_ARB ---------------------------------------------------------------*
196900060309from   FNARB00F                                                                1  ex FNARB01L
197000070222where  ARBffd <> 'M'                                                           2
197100060303   and ARBlnp in (                                                             3
197200060303   and ARBlna in (                                                             4
197300060303   and (ARBaas*10000+ARBmgs) between                                           5
197400060303   and ARBksc =                                                                6
197500060303   and ARBlnp =                                                                7
197600060303   and ARBlna =                                                                8
197700060303   and ARBnrs =                                                                9
197800060303   and ARBnrs <> 0                                                            10
197900060303** - $Sql_TAS ---------------------------------------------------------------*
198000060303from   TITAS00F                                                                1
198100060303where  TASffd <> 'M'                                                           2
198200060303   and TASlnp in (                                                             3
198300060303   and TASlna in (                                                             4
198400060303   and (TASaas*10000+TASmgs) between                                           5
198500060303   and TASccm =                                                                6
198600060303   and TASlnp =                                                                7
198700060303   and TASlna =                                                                8
198800060303   and TASnrs =                                                                9
198900060303   and TASnrs <> 0                                                            10
199000060303** - $Sql_VAB ---------------------------------------------------------------*
199100060309from   FIVAB00F                                                                1  ex FIVAB04L
199200060303where  VABatb =  ' '  and  VABffd <> 'M'                                       2
199300060303   and VABlnp in (                                                             3
199400060303   and VABlna in (                                                             4
199500060303   and (VABaas*10000+VABmgs) between                                           5
199600060303   and VABccm =                                                                6
199700060303   and VABlnp =                                                                7
199800060303   and VABlna =                                                                8
199900060303   and VABnrs =                                                                9
200000060303   and VABnrs <> 0                                                            10
200100060224** - $Txt -------------------------------------------------------*
200200060420xxxxxxxxxxxxxxxxxxxx                         xxxxxxxxxx              1
200300060224            ***   Errori  di instradamento   ***                     2
200400060420        99/99/9999     99:99:99                          Pag. 9999   3
200500060224------------------------------------------------------------------   4
200600060224Instradamento bolla                                località [01-20   5
200700060224Data       lnp spedizione cb cliente naz cap       località [21-35   6
20080006022499/99/9999 999 99 9999999 xx 9999999 xxx xxxxxxxxx xxxxxxxxxxxxxxx   7
200900061017]                                                       Instradame   8
201000061017]           pv tfp ts s fd ca  peso   volume lna zc --> lna zc       9
201100061017xxxxxxxxxxx xx 999  x x  x  x999999,9 99,999 999 99     999 99 xxx  10
201200061017nto calcolato     |Standard| Sotto| Oltre|FDep| Rifer.Mittente|Obs  11
201300061017                  | lna zc |lna zc|lna zc| zc |numerico / alfa|c/l  12
201400061017xxxxxxx xxxxxxxxxx  999 99  999 99 999 99  99  xxxxxxxxxxxxxxx  X   13
201500060224            ***     Parametri  di lancio     ***                    14
201600060306Selezione bolle .........: x       xxxxxxxxxx                       15
201700060224Data spedizione .........: 99/99/9999  al 99/99/9999                16
201800060420Cliente .................: 9999999 xxxxxxxxxxxxxxxxxxxxxxxxx        17
201900060420P.O. di partenza ........: 999     xxxxxxxxxxxxxxxxxxxxxxxxx        18
202000060420P.O. di arrivo ..........: 999     xxxxxxxxxxxxxxxxxxxxxxxxx        19
202100060420Serie  ..................: 99                                       20
202200060420Solo bolle con serie ....: x                                        21
202300060414Solo errori P.O. arrivo .: x                                        22
202400060414Solo totali .............: x                                        23
202500061018Anche bolle cap/loc obsol: x                                        24
202600060224                                                                    25
202700060224            **  F i n e    p r o s p e t t o  **                    26
202800060224  TOTALE GENERALE spedizioni con errori di instradamento:           27
202900060224  TOTALE GENERALE spedizioni:                                       28
203000060224  per colli:                                                        29
203100060224  Cliente gestisce peso:                                            30
203200060224  Segnacolli seq.:                                                  31
203300060224  Versione cappario:                                                32
203400060303** - $Decod (decodifiche in stampa) -----------------------------*
203500060224CAP GEN                                                              1
203600060224CAP                                                                  2
203700060224LOCALITA'                                                            3
203800060224SINONIMI                                                             4
203900060224ECCEZIONI                                                            5
204000060224STANDARD                                                             6
204100060224SOTTO                                                                7
204200060224OLTRE                                                                8
204300060224F.DEPOSITO                                                           9
204400060224PROVINCIA E/O CAP INDICATI INCONGRUENTI E/O LOCALITA' NON TROVATA   10
