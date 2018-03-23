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
020200060303     d WPDftc          s                   inz  like(BLPtc1)                    *consegna particolar
020300060303     d WPDdat          s                   inz  like(I95dat)                    *data spedizione
020400060302     d WPDxla          s                   inz  like(O95cla)                    *lna standard
020500060302     d WPDxzc          s                   inz  like(O95czc)                    *znc standard
020600060302     d WPDxls          s                   inz  like(O95cls)                    *lna sotto
020700060302     d WPDxzs          s                   inz  like(O95czs)                    *znc sotto
020800060302     d WPDxlo          s                   inz  like(O95clo)                    *lna oltre
020900061018     d WPDxzo          s                   inz  like(O95czo)                    *znc oltre
021000060303     d WPDxzf          s                   inz  like(O95czf)                    *znc f.deposito
021100060303     d WPDfca          s                   inz  like(I95fca)                    *S=C/Assegno
021200061018     d WPDobs          s              1    inz
021300060302     d WPDlid          s             10    inz                                  *livello scelta
021400060302     d WPDlil          s             10    inz                                  *livello lna/znc
021500060303     d WPDloc1         s             25    inz                                  *località (1ª parte)
021600060303     d WPDloc2         s             10    inz                                  *località (2ª parte)
021700060302     d WPDtxt          s            198    inz                                  *testo generico
021800061018     d WPDtxtobs       s             67    inz                                  *testo generico
021900060302     d WPEmsg1         s             80    inz                                  *messaggio di errore
022000060302     d WPEmsg2         s             80    inz                                  *messaggio di errore
022100060302     d WPDverCap       s             15    inz
022200060302     d WPDgesPkb       s             15    inz
022300060302     d WPDsegSeq       s             15    inz
022400060302      *
022500060302      * - Campi d Totale
022600060302     d wConta          s              7  0 inz
022700060302     d wContaPo        s              7  0 inz
022800060302     d wContaRot       s              7  0 inz
022900060302     d wContaTot       s              7  0 inz
023000060302     d wContaPoTot     s              7  0 inz
023100060302     d wContaRotTot    s              7  0 inz
023200060302     d wColli          s              7  0 inz
023300060302     d wColliPo        s              7  0 inz
023400060302     d wColliRot       s              7  0 inz
023500060302     d wColliTot       s              7  0 inz
023600060302     d wColliPoTot     s              7  0 inz
023700060302     d wColliRotTot    s              7  0 inz
023800060302     d wContaDir       s              7  0 inz
023900060302     d wContaDirTot    s              7  0 inz
024000060302     d wSup            s              7  0 inz
024100060302     d wSupPo          s              7  0 inz
024200060302     d wSupRot         s              7  0 inz
024300060302     d wSupTot         s              7  0 inz
024400060302     d wSupPoTot       s              7  0 inz
024500060302     d wSupRotTot      s              7  0 inz
024600061017     d wContaob        s              7  0 inz
024700061017     d wContaPoob      s              7  0 inz
024800061017     d wContaRotob     s              7  0 inz
024900061017     d wColliob        s              7  0 inz
025000061017     d wColliPoob      s              7  0 inz
025100061017     d wColliRotob     s              7  0 inz
025200060302      *
025300060302      * - Campi di comodo
025400060302     d WWWnrs          s                   inz  like(SQLnrs)
025500060302     d WWWtpo          s                   inz  like(I95tpo)
025600060303     d WPOxin          s           5000    inz                                  "nnn, "
025700060303     d Wxxx            s              3    inz                                  BLP/ARB/TAS/VAB
025800060420     d Wtime           s              6  0 inz
025900060224      *
026000060224      *?K E Y - L I S T?----------------------------------------------*
026100060224      *
026200060224      * - TABEL00F
026300060224     c     K02TABEL      klist
026400060224     c                   kfld                    TBLkut
026500060224     c                   kfld                    TBLcod
026600060224     c     K03TABEL      klist
026700060224     c                   kfld                    TBLkut
026800060224     c                   kfld                    TBLcod
026900060224     c                   kfld                    TBLkey
027000060224      * - TNTBE01L
027100060224     c     K02TBE01      klist
027200060224     c                   kfld                    TBEcod
027300060224     c                   kfld                    TBEke1
027400060224      *
027500060224      *===============================================================*
027600060224      *                                                               *
027700060224      *?I N D I C A T O R I?------------------------------------------*
027800060224      *                                                               *
027900060224      *  10     - Comodo                                              *
028000060224      *                                                               *
028100060224      *===============================================================*
028200060224      *
028300060224     c     *Entry        plist
028400060224     c                   parm                    KPJBA
028500060224     c                   movel     KPJBU         TISI76ds
028600060224      *
028700060224      * Operazioni iniziali
028800060224     c                   exsr      RoutInz
028900060224      *
029000060224      * Ciclo di elaborazione fino a fine file
029100060303do  1c                   DOW       $EoF    = *off
029200060224      *
029300060302     c                   exsr      ReadCursor
029400060302     c                   if        $RecOK  =  *on
029500060302     c                   exsr      sr_Print
029600060302     c                   endif
029700060224      *
029800060302e   1c                   ENDDO                                                  *rottura serie
029900060224      *
030000060224      * Operazioni finali
030100060224     c                   exsr      RoutFin
030200060224      *
030300060224      * Fine
030400060224     c                   eval      *inLR = *on
030500060224      *
030600060224      *?S U B R O U T I N E S?----------------------------------------*
030700060224      *
030800060224      *---------------------------------------------------------------*
030900060224      *?Operazioni iniziali                                          ?*
031000060224      *---------------------------------------------------------------*
031100060224     c     RoutInz       BEGSR
031200060302      *
031300060302      * Impostazione iniziale dati fissi
031400060420     c                   time                    wTime
031500060302     c                   eval      TBLkut  = 1
031600060224      *
031700060224      * Reperimento dati utente
031800060224     c                   exsr      RepDatiUte
031900060224      *
032000060224      * Considerazioni iniziali sui parametri di lancio
032100060302     c*** già cosi:      clear                   WWWnrs
032200060303     c                   if        P76fns  = 'S'
032300060302     c                   eval      WWWnrs  = 1
032400060224     c                   endif
032500060302      *
032600060302      * Caricamento delle tabelle occorrenti
032700060302     c                   exsr      sr_CarTAB
032800060303      *
032900060303      * Sistemazione schiere per estrazione dati via SQL
033000060303     c                   exsr      sr_SchSQL
033100060302      *
033200060302      * Preparazione stringa SQL da eseguire
033300060302     c                   exsr      sr_PrepSQL
033400060302      *
033500060302      * Apertura archivi
033600060302     c                   exsr      sr_Open
033700060302      *
033800060302      * Stampa dei parametri di lancio
033900060302     c                   exsr      sr_PrtParm
034000060302      *
034100060302     c                   ENDSR
034200060302      *
034300060302      *---------------------------------------------------------------*
034400060302      *?Reperimento Dati Utente                                      ?*
034500060302      *---------------------------------------------------------------*
034600060302     c     RepDatiUte    BEGSR
034700060302      *
034800060302     c     *dtaara       define    §azute        azuteds
034900060302     c     *dtaara       define    §datiute      ddatiute
035000060302     c                   in(e)     *dtaara
035100060302if  1c                   if        %error
035200060302     c                   eval      I34tla = 'L'
035300060302     c                   call      'TIBS34R'
035400060302     c                   parm                    TIBS34ds
035500060302     c                   in        *dtaara
035600060302e   1c                   endif
035700060302      *
035800060302     c                   ENDSR
035900060302      *
036000060302      *---------------------------------------------------------------*
036100060302      *?Caricamento schiere da tabelle                               ?*
036200060302      *---------------------------------------------------------------*
036300060302     c     sr_CarTAB     BEGSR
036400060224      *
036500060224      * Caricamento tab. "3A" = Codici Bolla
036600060224      *   (selezione delle sole bolle normali)
036700060224     c                   clear                   xx
036800060224     c                   eval      TBLkut  = 1
036900060224     c                   eval      TBLcod  = '3A'
037000060224     c     K02TABEL      setll     TABEL
037100060224     c     K02TABEL      reade     TABEL
037200060224do  1c                   DOW       NOT %eof(TABEL00F)
037300060224if  2c                   if        TBLflg  = *blanks
037400060303     c                   movel     TBLuni        ds3A
037500060224if  3c                   if        §3Arbl  = 'N'
037600060224     c                   add       1             xx
037700060224     c                   movel     TBLkey        $3A(xx)
037800060224     c                   movel     §3Atb1        $3Atb1(xx)
037900060224     c                   movel     §3Atb2        $3Atb2(xx)
038000060224     c                   z-add     §3Afca        $3Afca(xx)
038100060224e   3c                   endif
038200060224e   2c                   endif
038300060224     c     K02tabel      reade     TABEL
038400060224e   1c                   ENDDO
038500060224      *
038600060224      * Caricamento P.O.
038700060224     c                   clear                   xx
038800060224     c                   clear                   xy
038900060224     c     *loval        setll     AZORG
039000060224     c                   read      AZORG
039100060224do  1c                   DOW       NOT %eof(AZORG01L)
039200060224if  2c                   if            ORGfva  = *blanks                        *valido
039300060224     c                             and ORGfag <> 'V'                            *no varie
039400060224     C                   movel     ORGde3        Og143
039500060224     c                   add       1             xx
039600060224     c                   eval      $PO(xx)    = ORGfil
039700060224     c                   eval      $POntw(xx) = §OGntw
039800060224      * - Test del network
039900060224      *   x stabilire se si tratta di p.o. Italia o Estero
040000060224     c                   clear                   dNTW
040100060306     c                   eval      TBEcod  = 'NTW'
040200060306     c                   movel(p)  §OGntw        TBEke1
040300060224     c     K02TBE01      chain     TNTBE000
040400060224if  3c                   if        %found(TNTBE01L)
040500060303     c                   movel     TBEuni        dNTW
040600060224e   3c                   endif
040700060224if  3c                   if        §NTWfie = *blanks
040800060224     c                   eval      $POfie(xx) = 'I'                             *flag Italia
040900060224x   3c                   else
041000060224     c                   eval      $POfie(xx) = 'E'                             *flag Estero
041100060224e   3c                   endif
041500060224      * - Memorizzazione dei p.o. FedEx
041600060224if  3c                   if        §OGntw  = 'FED'
041700060224     c                   add       1             xy
041800060224     c                   eval      $POfed(xy) = ORGfil
041900060224e   3c                   endif
042000060224e   2c                   endif
042100060224     c                   read      AZORG
042200060224e   1c                   ENDDO
042300060302      *
042400060302do  1c                   do        xx            xy
042500060302     c                   movel     $PO(xy)       $POxin(xy)
042600060302if  2c                   if        xy      < xx
042700060302     c                   move      ', '          $POxin(xy)
042800060302e   2c                   endif
042900060302e   1c                   enddo
043000060303     c                   movea     $POxin        WPOxin
043100060224      *
043200060224      * Caricamento dei P.O. in gestione
043300060224      * - p.o.
043400060224if  1c                   if        DUTlpo <> 'S'
043500060224     c                   reset                   TRUL06ds
043600060224     c                   movel(p)  DUTpou        D06key
043700060224     c                   movel     TRUL06ds      KPJBU
043800060224     c                   call      'TRUL06R'
043900060224     c                   parm                    KPJBA
044000060224     c                   movel     KPJBU         TRUL06ds
044100060224     c                   movea     Lin           £1
044200060224      * - Sede
044300060224x   1c                   else
044400060224     c                   reset                   TRUL26ds
044500060224     c                   call      'TRUL26R'
044600060224     c                   parm                    TRUL26ds
044700060302     c                   movea     SKpo          £1
044800060224e   1c                   endif
044900060224      *
045000060224      * Se non sono in sede: includo/escludo solo bolle POSTE.
045100060224if  1c                   IF        DUTlpo <> 'S'
045200060302     c                   clear                   xx
045300060302do  2c                   DOW       xx      < %elem(£1)
045400060302     c                   eval      xx      = xx + 1
045500060224     c                   eval      xy      = 1
045600060224     c     £1(xx)        lookup    $PO(xy)                                10
045700060224      * - Se esiste P.O. (è altamente imporobabile che non esista
045800060224      *   ma lo gestisco ugualmente)
045900060224if  3c                   if        *in10
046000060224     c                   eval      $PT     = ($POntw(xy) = 'PPT')
046100060224x   3c                   else
046200060224      * - Se non esiste il P.O. statisticamente assumo che il P.O.
046300060224      *   NON sia del network POSTE
046400060224     c                   eval      $PT     = *off
046500060224e   3c                   endif
046600060224      * - Includo/Escludo...
046700060224sel 3c                   select
046800060224w   3c                   when          DUTntw  = 'PPT'
046900060224     c                             and $PT     = *off
047000060224     c                   clear                   £1(xx)
047100060224w   3c                   when          DUTntw <> 'PPT'
047200060224     c                             and $PT     = *on
047300060224     c                   clear                   £1(xx)
047400060224e   3c                   endsl
047500060224e   2c                   ENDDO
047600060224e   1c                   ENDIF
047700060302      *
047800060302     c                   ENDSR
047900060303      *
048000060303      *---------------------------------------------------------------*
048100060303      *?Sistemazione schiere per estrazione dati via SQL             ?*
048200060303      *---------------------------------------------------------------*
048300060303     c     sr_SchSQL     BEGSR
048400060303      *
048500060303      * Spiegazione:
048600060303      * La schiera $Sql_Fld1 contiene le lettere "finali" dei campi da
048700060303      *   estrarre dai vari archivi.
048800060303      * Ogni suo elemento è per questo "suddiviso" in 3 campi:
048900060303      * - i primi 3 caratteri    definiscono il campo dei file BLP/ARB;
049000060303      * - i 3 caratteri in mezzo definiscono il campo nel file TITAS;
049100060303      * - gli ultimi 3 caratteri definiscono il campo nel file FIVAB;
049200060303      * (le lettere iniziali BLP/ARB/TAS/VAB verranno poi impostate).
049300060303      * Nella schiera a tempo di compilazione sono impostate SOLO le
049400060303      *   eccezioni di almeno uno dei 2 archivi TAS/VAB: adesso imposto
049500060303      *   i default (dove mancanti).
049600060306      * N.B. - *blank significa che il campo da considerare ha lo
049700060306      * ¯¯¯¯   stesso nome del 1° (da FNBLP/FNARB);
049800060306      *      - "---"  significa cge il campo non è da considerare per
049900060306      *        il file in esame.
050000060303      *
050100060303     c                   eval      xx      = 1
050200060303do  1c                   dow       xx     <= %elem($Sql_Fld1)
050300060303      *
050400060303if  2c                   if        $Sql_Fld1(xx)        <> *blanks
050500060306      *
050600060306      * - dati da FNBLP/FNARB
050700060306      *   (solo esclusioni)
050800060306if  3c                   if        %subst($Sql_Fld1(xx):1:3) = *all'-'
050900060306     c                   eval      %subst($Sql_Fld1(xx):1:3) = *blanks
051000060306e   3c                   endif
051100060303      *
051200060303      * - dati da TITAS
051300060303sel 3c                   select
051400060303w   3c                   when      %subst($Sql_Fld1(xx):4:3) = *blanks
051500060303     c                   eval      %subst($Sql_Fld1(xx):4:3) =
051600060303     c                             %subst($Sql_Fld1(xx):1:3)
051700060303w   3c                   when      %subst($Sql_Fld1(xx):4:3) = *all'-'
051800060303     c                   eval      %subst($Sql_Fld1(xx):4:3) = *blanks
051900060303e   3c                   endsl
052000060303      *
052100060303      * - dati da FIVAB
052200060303sel 3c                   select
052300060303w   3c                   when      %subst($Sql_Fld1(xx):7:3) = *blanks
052400060303     c                   eval      %subst($Sql_Fld1(xx):7:3) =
052500060303     c                             %subst($Sql_Fld1(xx):1:3)
052600060303w   3c                   when      %subst($Sql_Fld1(xx):7:3) = *all'-'
052700060303     c                   eval      %subst($Sql_Fld1(xx):7:3) = *blanks
052800060303e   3c                   endsl
052900060303      *
053000060303e   2c                   endif
053100060303      *
053200060303     c                   eval      xx      = xx + 1
053300060303e   1c                   enddo
053400060303      *
053500060303     c                   ENDSR
053600060302      *
053700060302      *---------------------------------------------------------------*
053800060302      *?Preparazione stringa SQL da eseguire e Apertura cursore      ?*
053900060302      *---------------------------------------------------------------*
054000060302     c     sr_PrepSQL    BEGSR
054100060302      *
054200060303     c                   clear                   wSQL
054300060306     c                   eval      wSQL    = C_Select
054400060302      *
054500060303      * Dichiarazione SQL
054600060301sel 1c                   select
054700060302      *
054800060301      * - Bolle Partenza (da FNBLP00F)
054900060301w   1c                   when      P76pav  = 'P'
055000060301     c                   exsr      PrepSQL_BLP
055100060302      *
055200060301      * - Bolle Arrivo . (da FNARB00F)
055300060301w   1c                   when      P76pav  = 'A'
055400060301     c                   exsr      PrepSQL_ARB
055500060302      *
055600060301      * - Bolle Sede ... (da TITAS30C)
055700060301w   1c                   when      P76pav  = 'S'
055800060301     c                   exsr      PrepSQL_TAS
055900060302      *
056000060301      * - Dkt .......... (da FIVAB00F)
056100060301w   1c                   when      P76pav  = 'D'
056200060301     c                   exsr      PrepSQL_VAB
056300060301      *
056400060301e   1c                   endsl
056500060303      *
056600060303     c                   eval      wSQL    =  %trim(wSQL)
056700060303      *?order by?
056800060303     c                                     +  ' '
056900060303     c                                     +  C_OrderBy
057000060303      *?for fatch only?
057100060303     c                                     +  ' '
057200060303     c                                     +  C_ForFetch
057300060303      *
057400060303      * Apertura cursore
057500061018     c                   movel     wSQL          wSQLds
057600060303     c                   exsr      OpenCursor
057700060224      *
057800060224     c                   ENDSR
057900060301      *
058000060301      *---------------------------------------------------------------*
058100060301      *?Preparazione stringa SQL x bolle partenza                    ?*
058200060301      *---------------------------------------------------------------*
058300060301     c     PrepSQL_BLP   BEGSR
058400060301      *
058500060303     c                   eval      Wxxx    = 'BLP'
058600060303     c                   eval      xx      = 1
058700060303     c                   exsr      PrepSQL_Sel
058800060303      *
058900060303     c                   eval      wSQL    = %trim(wSQL)
059000060302     c                                     + ' '
059100060306     c                                     + %trim($Sql_BLP( 1))
059200060303     c                                     + ' '
059300060302     c                                     + %trim($Sql_BLP( 2))
059400060302      * - lnp
059500060302     c                                     + ' '
059600060303     c                                     + %trim($Sql_BLP( 3))
059700060303     c                                     + %trim(WPOxin) + ')'
059800060302      * - lna
059900060302     c                                     + ' '
060000060303     c                                     + %trim($Sql_BLP( 4))
060100060303     c                                     + %trim(WPOxin) + ')'
060200060302      * - data spedizione (aaaammgg)
060300060302     c                                     + ' '
060400060303     c                                     + %trim($Sql_BLP( 5))
060500060302     c                                     + ' '
060600060303     c                                     + %char(P76ds1)
060700060302     c                                     + ' and '
060800060303     c                                     + %char(P76ds2)
060900060302      * - codice cliente specifico
061000060302     c                   if        P76ksc <> *zeros
061100060302     c                   eval      wSQL    = %trim(wSQL)
061200060302     c                                     + ' '
061300060303     c                                     + %trim($Sql_BLP( 6))
061400060302     c                                     + ' '
061500060303     c                                     + %char(P76ksc)
061600060302     c                   endif
061700060302      * - p.o.partenza specifico
061800060302     c                   if        P76lnp <> *zeros
061900060302     c                   eval      wSQL    = %trim(wSQL)
062000060302     c                                     + ' '
062100060303     c                                     + %trim($Sql_BLP( 7))
062200060302     c                                     + ' '
062300060303     c                                     + %char(P76lnp)
062400060302     c                   endif
062500060302      * - p.o.arrivo specifico
062600060302     c                   if        P76lna <> *zeros
062700060302     c                   eval      wSQL    = %trim(wSQL)
062800060302     c                                     + ' '
062900060303     c                                     + %trim($Sql_BLP( 8))
063000060302     c                                     + ' '
063100060303     c                                     + %char(P76lna)
063200060302     c                   endif
063300060302      * - serie specifica
063400060302     c                   if        P76nrs <> *zeros
063500060302     c                   eval      wSQL    = %trim(wSQL)
063600060302     c                                     + ' '
063700060303     c                                     + %trim($Sql_BLP( 9))
063800060302     c                                     + ' '
063900060303     c                                     + %char(P76nrs)
064000060302     c                   endif
064100060420      * - solo ldv con serie
064200060302     c                   if        P76fns  = 'S'
064300060302     c                   eval      wSQL    = %trim(wSQL)
064400060302     c                                     + ' '
064500060303     c                                     + %trim($Sql_BLP(10))
064600060302     c                   endif
064700060301      *
064800060301     c                   ENDSR
064900060301      *
065000060301      *---------------------------------------------------------------*
065100060301      *?Preparazione stringa SQL x bolle arrivo                      ?*
065200060301      *---------------------------------------------------------------*
065300060301     c     PrepSQL_ARB   BEGSR
065400060301      *
065500060303     c                   eval      Wxxx    = 'ARB'
065600060303     c                   eval      xx      = 1
065700060303     c                   exsr      PrepSQL_Sel
065800060303      *
065900060303     c                   eval      wSQL    = %trim(wSQL)
066000060303     c                                     + ' '
066100060303     c                                     + %trim($Sql_ARB( 1))
066200060302     c                                     + ' '
066300060302     c                                     + %trim($Sql_ARB( 2))
066400060302      * - lnp
066500060302     c                                     + ' '
066600060303     c                                     + %trim($Sql_ARB( 3))
066700060303     c                                     + %trim(WPOxin) + ')'
066800060302      * - lna
066900060302     c                                     + ' '
067000060303     c                                     + %trim($Sql_ARB( 4))
067100060303     c                                     + %trim(WPOxin) + ')'
067200060302      * - data spedizione (aaaammgg)
067300060302     c                                     + ' '
067400060303     c                                     + %trim($Sql_ARB( 5))
067500060302     c                                     + ' '
067600060303     c                                     + %char(P76ds1)
067700060302     c                                     + ' and '
067800060303     c                                     + %char(P76ds2)
067900060302      * - codice cliente specifico
068000060302     c                   if        P76ksc <> *zeros
068100060302     c                   eval      wSQL    = %trim(wSQL)
068200060302     c                                     + ' '
068300060303     c                                     + %trim($Sql_ARB( 6))
068400060302     c                                     + ' '
068500060303     c                                     + %char(P76ksc)
068600060302     c                   endif
068700060302      * - p.o.partenza specifico
068800060302     c                   if        P76lnp <> *zeros
068900060302     c                   eval      wSQL    = %trim(wSQL)
069000060302     c                                     + ' '
069100060303     c                                     + %trim($Sql_ARB( 7))
069200060302     c                                     + ' '
069300060303     c                                     + %char(P76lnp)
069400060302     c                   endif
069500060302      * - p.o.arrivo specifico
069600060302     c                   if        P76lna <> *zeros
069700060302     c                   eval      wSQL    = %trim(wSQL)
069800060302     c                                     + ' '
069900060303     c                                     + %trim($Sql_ARB( 8))
070000060302     c                                     + ' '
070100060303     c                                     + %char(P76lna)
070200060302     c                   endif
070300060302      * - serie specifica
070400060302     c                   if        P76nrs <> *zeros
070500060302     c                   eval      wSQL    = %trim(wSQL)
070600060302     c                                     + ' '
070700060303     c                                     + %trim($Sql_ARB( 9))
070800060302     c                                     + ' '
070900060303     c                                     + %char(P76nrs)
071000060302     c                   endif
071100060420      * - solo ldv con serie
071200060302     c                   if        P76fns  = 'S'
071300060302     c                   eval      wSQL    = %trim(wSQL)
071400060302     c                                     + ' '
071500060303     c                                     + %trim($Sql_ARB(10))
071600060302     c                   endif
071700060301      *
071800060301     c                   ENDSR
071900060301      *
072000060301      *---------------------------------------------------------------*
072100060301      *?Preparazione stringa SQL x bolle sede                        ?*
072200060301      *---------------------------------------------------------------*
072300060301     c     PrepSQL_TAS   BEGSR
072400060301      *
072500060302      *?TITAS00F?
072600060303     c                   eval      Wxxx    = 'TAS'
072700060303     c                   eval      xx      = 2
072800060303     c                   exsr      PrepSQL_Sel
072900060303      *
073000060303     c                   eval      wSQL    = %trim(wSQL)
073100060303     c                                     + ' '
073200060303     c                                     + %trim($Sql_TAS( 1))
073300060302     c                                     + ' '
073400060302     c                                     + %trim($Sql_TAS( 2))
073500060303     c                   exsr      Add_SelTAS
073600060303      *
073700060307      * Union + Select
073800060307     c                   eval      wSQL    = %trim(wSQL)
073900060307     c                                     + ' '
074000060307     c                                     + C_Union
074100060307     c                                     + ' '
074200060307     c                                     + C_Select
074300060302      *
074400060302      *?TITAS10F?
074500060303     c                   eval      Wxxx    = 'TAS'
074600060303     c                   eval      xx      = 2
074700060303     c                   exsr      PrepSQL_Sel
074800060303      *
074900060303     c                   eval      wSQL    =  %trim(wSQL)
075000060303     c                                     + ' '
075100060303     c                                     + 'from TITAS10F'
075200060303     c                                     + ' '
075300060303     c                                     + %trim($Sql_TAS( 2))
075400060303     c                   exsr      Add_SelTAS
075500060303      *
075600060303      * Union
075700060303     c***                eval      wSQL    =  %trim(wSQL)
075800060303     c***                                  +  ' '
075900060303     c***                                  +  C_Union
076000060302      *
076100060302      *?TITASP0F?  (no)
076200060303     c***                eval      Wxxx    = 'TAS'
076300060303     c***                eval      xx      = 2
076400060303     c***                exsr      PrepSQL_Sel
076500060303      ****
076600060303     c***                eval      wSQL    =  %trim(wSQL)
076700060303     c***                                  + ' '
076800060303     c***                                  + 'from TITASP0F'
076900060303     c***                                  + ' '
077000060303     c***                                  + %trim($Sql_TAS( 2))
077100060303     c***                exsr      Add_SelTAS
077200060301      *
077300060301     c                   ENDSR
077400060303      *
077500060303      *---------------------------------------------------------------*
077600060303      *?Aggiunta selezione record in stringa SQL x bolle sede        ?*
077700060303      *---------------------------------------------------------------*
077800060303     c     Add_SelTAS    BEGSR
077900060303      *
078000060303     c                   eval      wSQL    = %trim(wSQL)
078100060303      * - lnp
078200060303     c                                     + ' '
078300060303     c                                     + %trim($Sql_TAS( 3))
078400060303     c                                     + %trim(WPOxin) + ')'
078500060303      * - lna
078600060303     c                                     + ' '
078700060303     c                                     + %trim($Sql_TAS( 4))
078800060303     c                                     + %trim(WPOxin) + ')'
078900060303      * - data spedizione (aaaammgg)
079000060303     c                                     + ' '
079100060303     c                                     + %trim($Sql_TAS( 5))
079200060303     c                                     + ' '
079300060303     c                                     + %char(P76ds1)
079400060303     c                                     + ' and '
079500060303     c                                     + %char(P76ds2)
079600060303      * - codice cliente specifico
079700060303     c                   if        P76ksc <> *zeros
079800060303     c                   eval      wSQL    = %trim(wSQL)
079900060303     c                                     + ' '
080000060303     c                                     + %trim($Sql_TAS( 6))
080100060303     c                                     + ' '
080200060303     c                                     + %char(P76ksc)
080300060303     c                   endif
080400060303      * - p.o.partenza specifico
080500060303     c                   if        P76lnp <> *zeros
080600060303     c                   eval      wSQL    = %trim(wSQL)
080700060303     c                                     + ' '
080800060303     c                                     + %trim($Sql_TAS( 7))
080900060303     c                                     + ' '
081000060303     c                                     + %char(P76lnp)
081100060303     c                   endif
081200060303      * - p.o.arrivo specifico
081300060303     c                   if        P76lna <> *zeros
081400060303     c                   eval      wSQL    = %trim(wSQL)
081500060303     c                                     + ' '
081600060303     c                                     + %trim($Sql_TAS( 8))
081700060303     c                                     + ' '
081800060303     c                                     + %char(P76lna)
081900060303     c                   endif
082000060303      * - serie specifica
082100060303     c                   if        P76nrs <> *zeros
082200060303     c                   eval      wSQL    = %trim(wSQL)
082300060303     c                                     + ' '
082400060303     c                                     + %trim($Sql_TAS( 9))
082500060303     c                                     + ' '
082600060303     c                                     + %char(P76nrs)
082700060303     c                   endif
082800060420      * - solo ldv con serie
082900060303     c                   if        P76fns  = 'S'
083000060303     c                   eval      wSQL    = %trim(wSQL)
083100060303     c                                     + ' '
083200060303     c                                     + %trim($Sql_TAS(10))
083300060303     c                   endif
083400060303      *
083500060303     c                   ENDSR
083600060301      *
083700060301      *---------------------------------------------------------------*
083800060301      *?Preparazione stringa SQL x bolle partenza da cliente         ?*
083900060301      *---------------------------------------------------------------*
084000060301     c     PrepSQL_VAB   BEGSR
084100060301      *
084200060303     c                   eval      Wxxx    = 'VAB'
084300060303     c                   eval      xx      = 3
084400060303     c                   exsr      PrepSQL_Sel
084500060303      *
084600060303     c                   eval      wSQL    = %trim(wSQL)
084700060303     c                                     + ' '
084800060303     c                                     + %trim($Sql_VAB( 1))
084900060302     c                                     + ' '
085000060302     c                                     + %trim($Sql_VAB( 2))
085100060302      * - lnp
085200060302     c                                     + ' '
085300060303     c                                     + %trim($Sql_VAB( 3))
085400060309     c                                     + %trim(WPOxin) + ', 000)'
085500060302      * - lna
085600060302     c                                     + ' '
085700060303     c                                     + %trim($Sql_VAB( 4))
085800060309     c                                     + %trim(WPOxin) + ', 000)'
085900060302      * - data spedizione (aaaammgg)
086000060302     c                                     + ' '
086100060303     c                                     + %trim($Sql_VAB( 5))
086200060302     c                                     + ' '
086300060303     c                                     + %char(P76ds1)
086400060302     c                                     + ' and '
086500060303     c                                     + %char(P76ds2)
086600060302      * - codice cliente specifico
086700060302     c                   if        P76ksc <> *zeros
086800060302     c                   eval      wSQL    = %trim(wSQL)
086900060302     c                                     + ' '
087000060303     c                                     + %trim($Sql_VAB( 6))
087100060302     c                                     + ' '
087200060303     c                                     + %char(P76ksc)
087300060302     c                   endif
087400060302      * - p.o.partenza specifico
087500060302     c                   if        P76lnp <> *zeros
087600060302     c                   eval      wSQL    = %trim(wSQL)
087700060302     c                                     + ' '
087800060303     c                                     + %trim($Sql_VAB( 7))
087900060302     c                                     + ' '
088000060303     c                                     + %char(P76lnp)
088100060302     c                   endif
088200060302      * - p.o.arrivo specifico
088300060302     c                   if        P76lna <> *zeros
088400060302     c                   eval      wSQL    = %trim(wSQL)
088500060302     c                                     + ' '
088600060303     c                                     + %trim($Sql_VAB( 8))
088700060302     c                                     + ' '
088800060303     c                                     + %char(P76lna)
088900060302     c                   endif
089000060302      * - serie specifica
089100060302     c                   if        P76nrs <> *zeros
089200060302     c                   eval      wSQL    = %trim(wSQL)
089300060302     c                                     + ' '
089400060303     c                                     + %trim($Sql_VAB( 9))
089500060302     c                                     + ' '
089600060303     c                                     + %char(P76nrs)
089700060302     c                   endif
089800060420      * - solo ldv con serie
089900060302     c                   if        P76fns  = 'S'
090000060302     c                   eval      wSQL    = %trim(wSQL)
090100060302     c                                     + ' '
090200060303     c                                     + %trim($Sql_VAB(10))
090300060302     c                   endif
090400060301      *
090500060301     c                   ENDSR
090600060303      *
090700060303      *---------------------------------------------------------------*
090800060303      *?Selezione dei campi dal record del file specifico            ?*
090900060303      *---------------------------------------------------------------*
091000060303     c     PrepSQL_Sel   BEGSR
091100060303      *
091200060303      * N.B.1: La lunghezza dei campi è CHIODATA a 6 caratteri
091300060303      * ¯¯¯¯¯  (3 iniziali - identificativi del file -
091400060303      *         3 finali   - identificativi del campo)
091500060303      *        SE si devessero elaborare campi con nomi più lunghi
091600060303      *        occorrerà rivedere la gestione della schiera $Sql_Fld1!
091700060303      *
091800060303      * N.B.2: La virgola NON deve essere inserita nell'ultimo elemento
091900060303      * ¯¯¯¯¯  della schiera $Sql_Fld2 !!!
092000060303      *
092100060303     c                   eval      xx      = (xx - 1) * 3 + 1
092200060303     c                   eval      xy      = 1
092300060303      *
092400060303do  1c                   dow       xy     <= %elem($Sql_Fld1)
092500060303      *
092600060306if  2c                   if        %subst($Sql_Fld1(xy):xx:3) <> *blanks
092700060303      *
092800060303if  3c                   if        xy      = 1
092900060303     c                   eval      wSQL    = %trim(wSQL)
093000060303     c                                     + ' '
093100060303     c                                     + Wxxx
093200060303     c                                     + %subst($Sql_Fld1(xy):xx:3)
093300060303     c                                     + $Sql_Fld2(xy)
093400060303x   3c                   else
093500060303     c                   eval      wSQL    = %trim(wSQL)
093600060303     c                                     + ','
093700060303     c                                     + Wxxx
093800060303     c                                     + %subst($Sql_Fld1(xy):xx:3)
093900060303     c                                     + $Sql_Fld2(xy)
094000060303e   3c                   endif
094100060303      *
094200060307x   2c                   else
094300060307      *
094400060307sel 3c                   select
094500060307      * xxxRMA (15 alfanum): no in TITAS
094600060307w   3c                   when      xy      = 23                                 RMA: no TAS
094700060307     c                   eval      wSQL    = %trim(wSQL)
094800060307     c                                     + ',''               '''             15 alfanum.
094900060307     c                                     + $Sql_Fld2(xy)
095000060307      * xxxFVB (01 alfanum): no in FIVAB
095100060307w   3c                   when      xy      = 24                                 FVB: no VAB
095200060307     c                   eval      wSQL    = %trim(wSQL)
095300060307     c                                     + ','' '''                           01 alfanum.
095400060307     c                                     + $Sql_Fld2(xy)
095500060307      * xxxFGS (03 / 0 num): no in FNBLP/FNARB e TITAS
095600060322w   3c***                when      xy      = 25                                 FGS: no BLP/ARB/TAS
095700060322     c***                eval      wSQL    = %trim(wSQL)
095800060322     c***                                  + ',''000'''                         03 num.
095900060322     c***                                  + $Sql_Fld2(xy)
096000060307      * xxxCCA (01 alfanum): no in FNBLP/FNARB e FIVAB
096100060307w   3c                   when      xy      = 26                                 CCA: no BLP/ARB/VAB
096200060307     c                   eval      wSQL    = %trim(wSQL)
096300060322     c                                     + ','' '''                           01 alfanum.
096400060307     c                                     + $Sql_Fld2(xy)
096500060307e   3c                   endsl
096600060307      *
096700060303e   2c                   endif
096800060303      *
096900060303     c                   eval      xy      = xy + 1
097000060303e   1c                   enddo
097100060303      *
097200060303     c                   ENDSR
097300060301      *
097400060301      *---------------------------------------------------------------*
097500060303      *?Dichiarazione e Apertura cursore                             ?*
097600060301      *---------------------------------------------------------------*
097700060303     c     OpenCursor    BEGSR
097800060301      *
097900060301      * Dichiarazione cursore
098000060301      *
098100060301     c/exec sql
098200060301     c+     PREPARE S1 FROM :wSQL
098300060301     c/end-exec
098400060301     c*
098500060301     c/exec sql
098600060301     c+     DECLARE C1 CURSOR FOR S1
098700060301     c/end-exec
098800060301      *
098900060301      * Apertura del cursore
099000060301      *
099100060301     c/exec sql
099200060301     c+     OPEN C1
099300060301     c/end-exec
099400060301      *
099500060301     c                   ENDSR
099600060302      *
099700060302      *---------------------------------------------------------------*
099800060302      *?Apertura archivi                                             ?*
099900060302      *---------------------------------------------------------------*
100000060302     c     sr_Open       BEGSR
100100060302      *
100200060302      * Bolle Sede
100300060302if  1c                   if        P76pav  = 'S'
100400060302      * - cancellazione wrk-file
100500060302     c/exec SQL
100600060302     c+   DELETE from WFSI760F
100700060302     c/end-exec
100800060302     c                   open      WFSI760F
100900060302e   1c                   endif
101000060302      *
101100060302     c                   ENDSR
101200060302      *
101300060302      *---------------------------------------------------------------*
101400060302      *?Stampa pagina iniziale con i parametri di lancio             ?*
101500060302      *---------------------------------------------------------------*
101600060302     c     sr_PrtParm    BEGSR
101700060302      *
101800060302      * Decodifica parametri
101900060302      * - Tipo P.O.
102000060302sel 1c                   select
102100060302w   1c                   when      P76pav  = 'P'
102200060302     c                   eval      WPPpav  = 'Partenza'
102300060302w   1c                   when      P76pav  = 'A'
102400060302     c                   eval      WPPpav  = 'Arrivo'
102500060302if  1c                   when      P76pav  = 'D'
102600060302     c                   eval      WPPpav  = 'Dkt'
102700060302if  1c                   when      P76pav  = 'S'
102800060302     c                   eval      WPPpav  = 'Sede'
102900060302e   1c                   endsl
103000060302      * - Inversione date
103100060302     c                   reset                   WLBdat
103200060302     c                   eval      G08inv  = P76ds1
103300060302     c                   call      'XSRDA8'
103400060302     c                   parm                    WLBdat
103500060302     c                   eval      WPPds1  = G08dat
103600060302     c                   reset                   WLBdat
103700060302     c                   eval      G08inv  = P76ds2
103800060302     c                   call      'XSRDA8'
103900060302     c                   parm                    WLBdat
104000060302     c                   eval      WPPds2  = G08dat
104100060302      * - Decodifica cliente
104200060302if  1c                   if        P76ksc <> *zeros
104300060302     c                   clear                   TIBS69ds
104400060302     c                   eval      I69sif  = KNSIF
104500060302     c                   eval      I69kac  = P76ksc
104600060302     c                   clear                   DS_cnaco
104700060302     c                   clear                   DS_cnind
104800060302     c                   clear                   DS_cnclp
104900060302     c                   clear                   DS_fncls
105000060302     c                   call      'TIBS69R'
105100060302     c                   parm                    TIBS69ds
105200060302     c                   parm                    DS_cnaco
105300060302     c                   parm                    DS_cnind
105400060302     c                   parm                    DS_cnclp
105500060302     c                   parm                    DS_fncls
105600060302if  2c                   if        O69err <> *on
105700060302     c                   movel     ACOrag        WPPksc
105800060302e   2c                   endif
105900060302e   1c                   endif
106000060302      * - Decodifica Lnp
106100060302if  1c                   if        P76lnp <> *zeros
106200060302     c     P76lnp        chain     AZORG
106300060302if  2c                   if        %found(AZORG01L)
106400060302     c                   movel     ORGdes        WPPlnp
106500060302e   2c                   endif
106600060302e   1c                   endif
106700060302      * - Decodifica Lna
106800060302if  1c                   if        P76lna <> *zeros
106900060302     c     P76lna        chain     AZORG
107000060302if  2c                   if        %found(AZORG01L)
107100060302     c                   movel     ORGdes        WPPlna
107200060302e   2c                   endif
107300060302e   1c                   endif
107400060302      * Stampa
107500060302     c                   except    PRTtxt
107600060302     c                   except    PRTprm
107700060302      *
107800060308     c***                eval      *inOF   = *on
107900060308     c                   except    PRTtxt                                       *testata
108000060308     c                   except    PRTtx2                                       *intestazione
108100060308     c                   eval      *inOF   = *off
108200060302      *
108300060302     c                   ENDSR
108400060301      *
108500060301      *---------------------------------------------------------------*
108600060301      *?Lettura cursore                                              ?*
108700060301      *---------------------------------------------------------------*
108800060301     c     ReadCursor    BEGSR
108900060301      *
109000060303     c                   reset                   $RecOK
109100060303     c                   clear                   ds_SQL
109200060302      *
109300060301     c/exec sql
109400060303     c+     FETCH NEXT FROM C1 INTO :ds_SQL
109500060301     c/end-exec
109600060301      *
109700060301sel 1c                   select
109800060301w   1c                   when      SQLcod  =  100
109900060301     c                   eval      $EoF    =  *on
110000060301w   1c                   when      SQLcod  <  *zeros
110100060301     c                   eval      $Err    =  *on
110200060301     c                   eval      $Eof    =  *on
110300060301x   1c                   other
110400060301     c                   exsr      Selez_Rec
110500060301e   1c                   endsl
110600060301      *
110700060301     c                   ENDSR
110800060301      *
110900060301      *---------------------------------------------------------------*
111000060301      *?Selezioni ulteriori                                          ?*
111100060301      *---------------------------------------------------------------*
111200060301     c     Selez_Rec     BEGSR
111300060301      *
111400060303     c*** già fatto:     reset                   $RecOK
111500060302      *
111600060302      * Esclusione P.O. arrivo bolle estero
111700060302     c                   eval      xy      = 1
111800060302     c     SQLlna        lookup    $PO(xy)                                10
111900060303if  1c                   if            NOT *in10                                *non trovata o
112000060303     c                             or  $POfie(xy) = 'E'                          *filiale estera
112100060302     c                   leavesr                                                *record non valido
112200060302e   1c                   endif
112300060302      *
112400060302      * Esclusioni rispetto alla gestione utenti
112500060302if  1c                   if            DUTlpo <> 'S'
112600060302     c                             and P76pav  = 'D'                            DISCHETTO
112700060302     c     SQLfgs        lookup    £1                                     10
112800060302if  2c                   if        NOT *in10
112900060302     c                   leavesr                                                *record non valido
113000060302e   2c                   endif
113100060302e   1c                   endif
113200060302      *
113300060302      * Se P.O. partenza POSTE: il terminal è se stessa
113400060302     c                   eval      xy      = 1
113500060302     c     SQLlnp        lookup    $PO(xy)                                10
113600060302if  1c                   if        NOT *in10                                    *no trovato
113700060302     c                   leavesr                                                *record non valido
113800060302x   1c                   else                                                   *trovato
113900060302if  2c                   if        $POntw(xy)  = 'PPT'                          *filiale ntw POSTE
114000060302     c                   eval      SQLtfp  = SQLlnp                             *terminal = p.o.
114100060302e   2c                   endif
114200060302e   1c                   endif
114300060302      *
114400060302      * Record OK
114500060302     c                   eval      $RecOK  = *on
114600060301      *
114700060301     c                   ENDSR
114800060302      *
114900060302      *---------------------------------------------------------------*
115000060303      *?Stampa                                                       ?*
115100060302      *---------------------------------------------------------------*
115200060302     c     sr_Print      BEGSR
115300060302      *
115400060302      * Stampa totali a rottura
115500060302sel 1c                   select
115600060302      * - P.O. partenza
115700060302w   1c                   when      SQLlnp <> SAVlnp
115800060302     c                   exsr      sr_TotNRS
115900060302     c                   exsr      sr_TotLNP
116000060309      * - Serie / Cliente
116100060309w   1c                   when          SQLnrs <> SAVnrs
116200060309     c                             or  SQLksc <> SAVksc
116300060302     c                   exsr      sr_TotNRS
116400060302e   1c                   endsl
116500060302      *
116600060302      * Elaborazione dettaglio
116700060302if  1c                   if            P76pav <> 'D'                            NO DISCHETTO
116800060302     c                             and SQLnrs <> *zeros                         NO disk B
116900060302     c                             and SQLfvb <> 'R'                            NO volume reale
117000060302     c                   clear                   SQLvlb
117100060302e   1c                   endif
117200060302      *
117300060302if  1c                   if        P76pav  = 'D'                                DISCHETTO
117400060302     c                   clear                   FNLV55ds
117500060302     c                   eval      D55tpt  = 'P'
117600060302     c                   eval      D55lin  = SQLlnp
117700060303     c                   eval      D55drf  = SQLaas * 10000 + SQLmgs            *data di riferimento
117800060302     c                   call      'FNLV55R'
117900060302     c                   parm                    FNLV55ds
118000060302if  2c                   if        D55err <> *blanks
118100060302     c                   eval      D55tfp  = SQLlnp
118200060302e   2c                   endif
118300060302     c                   eval      SQLtfp  = D55tfp                             *terminal partenza
118400060302e   1c                   endif
118500060302      *
118600060302      * Controlla l'instradamento e stampa eventuali differenze
118700060302     c                   exsr      sr_ChkINS
118800060302      *
118900060302     c                   ENDSR
119000060302      *
119100060302      *---------------------------------------------------------------*
119200060302      *?Rottura di livello: Numero Serie / Cliente                   ?*
119300060302      *---------------------------------------------------------------*
119400060302     c     sr_TotNRS     BEGSR
119500060302      *
119600060302      * Stampa i totali
119700060307if  1c                   IF        SAVksc <> *loval
119800060302     c                   exsr      sr_RepInfo
119900060302     c                   eval      WPDtxt  = ' TOTALE '
120000060302     c                                     + %editw(SAVksc:'   .    ')
120100060302     c                                     + ' '
120200060302     c                                     + §3Crag
120300060302     c                                     + ' Lnp: '
120400060302     c                                     + %editc(SAVlnp:'1')
120500060302     c                                     + ' Nrs: '
120600060302     c                                     + %editc(SAVnrs:'1')
120700061017     c                                     + WPDgesPkb
120800060303     c                                     + WPDsegSeq
120900060303     c                                     + WPDverCap
121000060302     c                                     + ' Sped:'
121100060302     c                                     + %editc(wContaRotTot:'2')
121200060302     c                                     + ' Colli:'
121300060413     c                                     + %editc(wColliRotTot:'2')
121400060302     c                                     + ' di cui errate Sped:'
121500060302     c                                     + %editc(wContaRot:'2')
121600060302     c                                     + ' Colli:'
121700060302     c                                     + %editc(wColliRot:'2')
121800060302     c                                     + ' a Sup.:'
121900060302     c                                     + %editc(wSupRot:'2')
122000060322if  1c                   if        *inOF                                        *fine pagina
122100060322     c                   except    PRTtxt                                       *testata
122200060322     c                   except    PRTtx2                                       *intestazione
122300060322     c                   eval      *inOF   = *off
122400060322e   1c                   endif
122500060302     c                   except    PRTtot                                       *totali
122600061018     c                   if        p76obs='S'
122700061018     c                   eval      WPDtxtobs  = 'di cui Sped. Obsol:'
122800061018     c                                        + %editc(wContaRotob:'2')
122900061018     c                                     + ' Colli:'
123000061018     c                                     + %editc(wColliRotob:'2')
123100061018     c                   except    PRTtotobs                                    *totali
123200061018     c                   endif
123300060302     c                   except    PRTnul                                       *riga vuota
123400060302     c                   if        P76pav  = 'S'
123500060302     c                   exsr      sr_WriteWF
123600060302     c                   endif
123700060302e   1c                   ENDIF
123800060302      *
123900060302      * Memorizza la nuova serie in un deposito
124000060302     c                   eval      SAVnrs  = SQLnrs
124100060302     c                   eval      SAVksc  = SQLksc
124200060302      *
124300060302      * Azzera i totali
124400060302     c                   clear                   wContaRot
124500060302     c                   clear                   wContaRotTot
124600060302     c                   clear                   wColliRot
124700060302     c                   clear                   wColliRotTot
124800060302     c                   clear                   wContaDir
124900060302     c                   clear                   wContaDirTot
125000060302     c                   clear                   wSupRot
125100060302     c                   clear                   wSupRotTot
125200061018     c                   clear                   wcontarotob
125300061018     c                   clear                   wcollirotob
125400060302      *
125500060302     c                   ENDSR
125600060302      *
125700060302      *---------------------------------------------------------------*
125800060302      *?Rottura di livello: P.O. di Partenza                         ?*
125900060302      *---------------------------------------------------------------*
126000060302     c     sr_TotLNP     BEGSR
126100060302      *
126200060302      * Stampa i totali
126300060307if  1c                   IF        SAVlnp <> *loval
126400060302     c                   eval      WPDtxt  = ' TOTALE P.O.            '
126500060302     c                                     + '                      '
126600060302     c                                     + ' Sped:'
126700060302     c                                     + %editc(wContaPoTot:'2')
126800060302     c                                     + ' Colli:'
126900060302     c                                     + %editc(wColliPoTot:'2')
127000060302     c                                     + ' di cui errate Sped:'
127100060302     c                                     + %editc(wContaPo:'2')
127200060302     c                                     + ' Colli:'
127300060302     c                                     + %editc(wColliPo:'2')
127400060302     c                                     + ' a Sup.:'
127500060302     c                                     + %editc(wSupPo:'2')
127600061018     c                   if        p76obs ='S'
127700061018     c                   eval      WPDtxt  = %trim(wpdtxt)
127800061018     c                                     + ' di cui Sped. Obs:'
127900061018     c                                     + %editc(wContaPoob:'2')
128000061018     c                                     + ' Colli:'
128100061018     c                                     + %editc(wColliPoob:'2')
128200061018     c                   endif
128300060302     c                   except    PRTtot                                       *totali
128400060302     c                   except    PRTnul                                       *riga vuota
128500060308      * - Imposto la condizione per il salto pagina (e stampa testata)
128600060308     c                   if            P76pav  = 'P'
128700060308     c                             or  P76pav  = 'A'
128800060308     c                             or  P76pav  = 'S'
128900060308     c                   eval      *inOF   = *on
129000060308     c                   endif
129100060302e   1c                   ENDIF
129200060302      *
129300060302      * Memorizza la nuova linea in un deposito
129400060302     c                   eval      SAVlnp  = SQLlnp
129500060302      *
129600060302      * Azzera i totali
129700060302     c                   clear                   wContaPo
129800060302     c                   clear                   wContaPoTot
129900060302     c                   clear                   wColliPo
130000060302     c                   clear                   wColliPoTot
130100060302     c                   clear                   wSupPo
130200060302     c                   clear                   wSupPoTot
130300061018     c                   clear                   wContaPoob
130400061018     c                   clear                   wColliPoob
130500060302      *
130600060302     c                   ENDSR
130700060302      *
130800060302      *---------------------------------------------------------------*
130900060302      *?Rottura di livello: Finale                                   ?*
131000060302      *---------------------------------------------------------------*
131100060302     c     sr_TotGEN     BEGSR
131200060302      *
131300060302      * Stampa il riepilogo dei dati
131400060302     c                   eval      WPDtxt  = ' TOTALE GENERALE        '
131500060302     c                                     + '                      '
131600060302     c                                     + ' Sped:'
131700060302     c                                     + %editc(wContaTot:'2')
131800060302     c                                     + ' Colli:'
131900060302     c                                     + %editc(wColliTot:'2')
132000060302     c                                     + ' di cui errate Sped:'
132100060302     c                                     + %editc(wConta:'2')
132200060302     c                                     + ' Colli:'
132300060302     c                                     + %editc(wColli:'2')
132400060302     c                                     + ' a Sup.:'
132500060302     c                                     + %editc(wSup:'2')
132600061018     c                   if        p76obs ='S'
132700061018     c                   eval      WPDtxt  = %trim(wpdtxt)
132800061018     c                                     + ' di cui Sped. Obs:'
132900061018     c                                     + %editc(wContaob:'2')
133000061018     c                                     + ' Colli:'
133100061018     c                                     + %editc(wColliob:'2')
133200061018     c                   endif
133300060302     c                   except    PRTnul                                       *riga vuota
133400060302     c                   except    PRTtot                                       *totali
133500060302     c                   except    PRTend                                       *chiusura
133600060302      *
133700060302     c                   ENDSR
133800060302      *
133900060302      *---------------------------------------------------------------*
134000060302      *?Controlla l'instradamento e Stampa eventuali differenze      ?*
134100060302      *---------------------------------------------------------------*
134200060302     c     sr_ChkINS     BEGSR
134300060302      *
134400060302     c                   eval      $Err    = *off                               *errore
134500060306      *
134600060306      * Pulisce i campi di decodifica/descrizione
134700060306     c                   clear                   WPDftc                         *consegna particolar
134800060306     c                   clear                   WPDdat                         *data spedizione
134900060306     c                   clear                   WPDxla                         *lna standard
135000060306     c                   clear                   WPDxzc                         *znc standard
135100060306     c                   clear                   WPDxls                         *lna sotto
135200060306     c                   clear                   WPDxzs                         *znc sotto
135300060306     c                   clear                   WPDxlo                         *lna oltre
135400060306     c                   clear                   WPDxzo                         *znc oltre
135500060306     c                   clear                   WPDxzf                         *znc f.deposito
135600060306     c                   clear                   WPDfca                         *S=C/Assegno
135700060306     c                   clear                   WPDlid                         *livello scelta
135800060306     c                   clear                   WPDlil                         *livello lna/znc
135900060306     c                   clear                   WPDloc1                        *località (1ª parte)
136000060306     c                   clear                   WPDloc2                        *località (2ª parte)
136100060306     c                   clear                   WPDtxt                         *testo generico
136200061018     c                   clear                   WPDtxtobs                      *testo generico
136300060306     c                   clear                   WPEmsg1                        *messaggio di errore
136400060306     c                   clear                   WPEmsg2                        *messaggio di errore
136500060306     c                   clear                   WPDverCap
136600060306     c                   clear                   WPDgesPkb
136700060306     c                   clear                   WPDsegSeq
136800061017     c                   clear                   WPDobs
136900060302      *
137000060302      * Reperisce il Tipo Porto e l'esistenza del Contrassegno
137100060302     c                   exsr      sr_ChkCBO
137200060302      *
137300060302      * Lancia il programma di calcolo instradamento
137400060302     c                   clear                   TISI95ds                       *azzera i parametri
137500060302     c                   eval      I95tcn  = '7'                                *tipo controllo
137600060303     c                   eval      I95nar  = SQLnzd                             *nazione
137700060303     c                   eval      I95cap  = SQLcad                             *cap
137800060303     c                   eval      I95loc  = SQLlod                             *località
137900060302     c                   eval      I95prv  = SQLprd                             *provincia
138000060303     c                   eval      I95dat  = SQLaas * 10000 + SQLmgs            *data di riferimento
138100060303     c                   eval      I95tpo  = WWWtpo                             *tipo porto
138200060303     c                   eval      I95fca  = WPDfca                             *contrassegno
138300060302     c                   eval      I95ffd  = SQLffd                             *fermo deposito
138400060302     c                   eval      I95tsp  = SQLtsp                             *tipo servizio
138500060303     c                   eval      I95tfp  = SQLtfp                             *terminal partenza
138600060302     c                   eval      I95fre  = 'S'                                *controllo eccezioni
138700060302     c                   eval      I95lkg  = SQLpkb                             *peso bollettato
138800060302     c                   eval      I95lmc  = SQLvlb                             *volume bollettato
138900060306     c                   eval      I95fi2  = SQLtc1 + SQLtc2                    *consegne particolar
139000060302      *
139100060302      * Verifica se P.O.arrivo è P.O. FedEx
139200060302if  1c                   if        SQLlna  > *zeros
139300060302     c     SQLlna        lookup    $POfed                                 10
139400060302if  2c                   if        *in10
139500060302     c                   eval      I95fi1  = 'S'
139600060302e   2c                   endif
139700060302e   1c                   endif
139800060302      *
139900060302     c                   call      'TISI95R'
140000060302     c                   parm                    TISI95ds
140100060302      *
140200060302      * Controlla instradamento calcolato con quello scritto sulla
140300060302      *   bolla
140400061017     c                   if        o95lna<>sqllna or
140500061017     c                             (p76obs='S' and
140600061017     c                             (o95lf1='O' or o95cf1='O')) or
140700061017     c                             (p76fel='N' and
140800061017     c                             (O95znc <> SQLznc
140900061017     c                             or  O95cap <> SQLcad
141000061017     c                             or (O95prv <> SQLprd
141100061017     c                             and O95pra <> SQLprd)))
141200061017     c                   eval      $Err     = *on                               *errore
141300061017     c                   exsr      sr_PrtERR                                    *stampa errore
141400061017     c                   endif
141500060302      *
141600060302      * Effettua sempre il conteggio del totale spedizioni e colli
141700060302      *   di tutte le spedizioni
141800060302     c                   add       1             wContaTot
141900060302     c                   add       1             wContaPoTot
142000060302     c                   add       1             wContaRotTot
142100060302     c                   add       SQLncl        wColliTot
142200060302     c                   add       SQLncl        wColliPoTot
142300060302     c                   add       SQLncl        wColliRotTot
142400060302      *
142500060302      * Controlla se la bolla è dirottata
142600060302if  1c                   if            P76pav  = 'S'                            NO DISCHETTO
142700060302     c                             and SQLcca  = '1'
142800060302     c                   add       1             wContaDirTot
142900060302e   1c                   endif
143000060302      *
143100060302      * Controlla se la bolla è destinata a supermercato
143200060302if  1c                   if            SQLtc1 = 'S'
143300060302     c                             or  SQLtc2 = 'S'
143400060302     c                   if        SQLtc1  = 'S'
143500060303     c                   eval      WPDftc  = SQLtc1
143600060302     c                   endif
143700060302     c                   if        SQLtc2  = 'S'
143800060303     c                   eval      WPDftc  = SQLtc2
143900060302     c                   endif
144000060302     c                   add       1             wSupTot
144100060302     c                   add       1             wSupPoTot
144200060302     c                   add       1             wSupRotTot
144300060302e   1c                   endif
144400060302      *
144500060302     c                   ENDSR
144600060302      *
144700060302      *---------------------------------------------------------------*
144800060302      *?Reperisce il Tipo Porto e l'Esistenza del Contrassegno       ?*
144900060302      *---------------------------------------------------------------*
145000060302     c     sr_ChkCBO     BEGSR
145100060302      *
145200060302     c                   eval      WWWtpo  = 'F'
145300060303     c                   eval      WPDfca  = 'N'
145400060302      *
145500060302     c                   eval      xy      = 1
145600060303     c     SQLcbo        lookup    $3A(xy)                                10
145700060302      *
145800060302if  1c                   if        *in10
145900060302      * - Tipo Porto
146000060302if  2c                   if            $3Atb1(xy) = 'A'
146100060302     c                             or  $3Atb2(xy) = 'A'
146200060302     c                   eval      WWWtpo  = 'A'                                *tipo porto
146300060302e   2c                   endif
146400060302      * - Flag C/Assegno
146500060303if  2c                   if        $3Afca(xy)    <> *zeros
146600060303     c                   eval      WPDfca  = 'S'                                *c/assegno
146700060302e   2c                   endif
146800060302      *
146900060302e   1c                   endif
147000060302      *
147100060302     c                   ENDSR
147200060302      *
147300060302      *---------------------------------------------------------------*
147400060302      *?Reperimento informazioni anagrafiche sulla Serie             ?*
147500060302      *---------------------------------------------------------------*
147600060302     c     sr_RepInfo    BEGSR
147700060302      *
147800060302     c                   clear                   ds3C
147900060302     c                   clear                   ds1B
148000060302      *
148100060302     c                   clear                   WPDgesPkb
148200060302     c                   clear                   WPDsegSeq
148300060302     c                   clear                   WPDverCap
148400060302      *
148500060302      * - tabella "3C" = Clienti con DiskC
148600060302     c*** già così       eval      TBLkut  = 1
148700060302     c                   eval      TBLcod  = '3C'
148800060302     c                   movel(p)  SAVksc        TBLkey
148900060302     c     K03TABEL      chain     TABEL
149000060302if  1c                   if            %found(TABEL00F)
149100060302     c                             and TBLflg = *blanks
149200060302     c                   movel     TBLuni        ds3C
149300060302      * - tabella "1B" = Codici Trattamento Merce
149400060302     c*** già così       eval      TBLkut  = 1
149500060302     c                   eval      TBLcod  = '1B'
149600060302     c                   movel(p)  §3Cctm        TBLkey
149700060302     c     K03TABEL      chain     TABEL
149800060302if  cc                   if            %found(TABEL00F)
149900060302     c                             and TBLflg = *blanks
150000060302     c                   movel     TBLuni        ds1B
150100060302e   2c                   endif
150200060302e   1c                   endif
150300060302      *
150400060302      * Considerazioni
150500060302if  1c                   if        §3Cpes  = 'N'
150600060302     c                   eval      WPDgesPkb  = ' NON gest. peso'
150700060302e   1c                   endif
150800060302      *
150900060302sel 1c                   select
151000060302w   1c                   when      §3Cnwc  = 'L'
151100060302     c                   eval      WPDverCap  = ' Vers. capp.: 3'
151200060302w   1c                   when      §3Cnwc  = 'S'
151300060302     c                   eval      WPDverCap  = ' Vers. capp.: 2'
151400060302w   1c                   when      §3Cnwc  = *blanks
151500060302     c                   eval      WPDverCap  = ' Vers. capp.: 1'
151600060302e   1c                   endsl
151700060302      *
151800060302if  1c                   if        §1Bfg7  = 'S'
151900060302     c                   eval      WPDsegSeq  = ' Segn. NON seq.'
152000060302e   1c                   endif
152100060302      *
152200060302     c                   ENDSR
152300060302      *
152400060302      *---------------------------------------------------------------*
152500060302      *?Stampa Errore                                                ?*
152600060302      *---------------------------------------------------------------*
152700060302     c     sr_PrtERR     BEGSR
152800060302      *
152900060302      * Messaggio di errore
153000060302     c***                movel     O95msg        WPEmsg1
153100060302     c                   eval      WPEmsg1 = %subst(O95msg:1:75)
153200060302      * Data spedizione
153300060302     c                   reset                   WLBdat
153400060303     c                   eval      G08inv  = SQLaas * 10000 + SQLmgs            *data spedizione
153500060302     c                   call      'XSRDA8'
153600060302     c                   parm                    WLBdat
153700060302     c                   eval      WPDdat  = G08dat                             *data spedizione
153800060302      * Livello scelta
153900060302if  1c                   if           (O95ela > *zeros
154000060302     c                             or  O95ezc > *zeros
154100060302     c                             or  O95elo > *zeros
154200060302     c                             or  O95ezo > *zeros
154300060302     c                             or  O95els > *zeros
154400060302     c                             or  O95ezs > *zeros)
154500060302     c                             and O95lid = '3'
154600060302     c                   movel     '5'           O95lid                         *forzato
154700060302e   1c                   endif
154800060302sel 1c                   select
154900060302w   1c                   when      O95lid  = '1'
155000060302     c                   movel     $Decod(1)     WPDlid
155100060302w   1c                   when      O95lid  = '2'
155200060302     c                   movel     $Decod(2)     WPDlid
155300060302w   1c                   when      O95lid  = '3'
155400060302     c                   movel     $Decod(3)     WPDlid
155500060302w   1c                   when      O95lid  = '4'
155600060302     c                   movel     $Decod(4)     WPDlid
155700060302w   1c                   when      O95lid  = '5'
155800060302     c                   movel     $Decod(5)     WPDlid
155900060302e   1c                   endsl
156000060302      * Livello lna/zc
156100060302sel 1c                   select
156200060302w   1c                   when          O95lil  = *blanks
156300060302     c                             and O95lna  > *zeros                         *se lna=0 NO scritta
156400060302     c                   movel     $Decod(6)     WPDlil
156500060302w   1c                   when      O95lil  = 'S'
156600060302     c                   movel     $Decod(7)     WPDlil
156700060302w   1c                   when      O95lil  = 'O'
156800060302     c                   movel     $Decod(8)     WPDlil
156900060302w   1c                   when      O95lil  = 'F'
157000060302     c                   movel     $Decod(9)     WPDlil
157100060302e   1c                   endsl
157200060302      *
157300060302if  1c                   if        O95cla  > *zeros                             *CAP pieni
157400060302     c                   eval      WPDxla  = O95cla                             *lna standard
157500060302     c                   eval      WPDxzc  = O95czc                             *znc standard
157600060302     c                   eval      WPDxls  = O95cls                             *lna sotto
157700060302     c                   eval      WPDxzs  = O95czs                             *znc sotto
157800060302     c                   eval      WPDxlo  = O95clo                             *lna oltre
157900060302     c                   eval      WPDxzo  = O95czo                             *znc oltre
158000060302     c                   eval      WPDxzf  = O95czf                             *znc f.deposito
158100060302e   1c                   endif
158200060302      *
158300060302if  1c                   if        O95lla > *zeros                              *LOCALITA' pieni
158400060302     c                   eval      WPDxla  = O95lla                             *lna standard
158500060302     c                   eval      WPDxzc  = O95lzc                             *znc standard
158600060302     c                   eval      WPDxls  = O95lls                             *lna sotto
158700060302     c                   eval      WPDxzs  = O95lzs                             *znc sotto
158800060302     c                   eval      WPDxlo  = O95llo                             *lna oltre
158900060302     c                   eval      WPDxzo  = O95lzo                             *znc oltre
159000060302     c                   eval      WPDxzf  = O95lzf                             *znc f.deposito
159100060302e   1c                   endif
159200060302      *
159300060302if  1c                   if        o95ela > *zeros                              *ECCEZIONI pieni
159400060302     c                   eval      WPDxla  = O95ela                             *lna standard
159500060302     c                   eval      WPDxzc  = O95ezc                             *znc standard
159600060302     c                   eval      WPDxls  = O95els                             *lna sotto
159700060302     c                   eval      WPDxzs  = O95ezs                             *znc sotto
159800060302     c                   eval      WPDxlo  = O95elo                             *lna oltre
159900060302     c                   eval      WPDxzo  = O95ezo                             *znc oltre
160000060303     c*** no questo:     eval      WPDxzf  = O95ezf                             *znc f.deposito
160100060302e   1c                   endif
160200060302      *
160300060302      * Località
160400060303     c                   eval      WPDloc1 = %subst(SQLlod:01:25)
160500060303     c                   eval      WPDloc2 = %subst(SQLlod:26:10)
160600061018     c* obsoleto
160700061018     c                   clear                   wpdobs
160800061018     c                   select
160900061018     c                   when      o95cf1='O'
161000061018     c                   eval      wpdobs='C'
161100061018     c                   when      o95lf1='O'
161200061018     c                   eval      wpdobs='L'
161300061018     c                   endsl
161400060302      *
161500060302      *
161600060302      * Incremento totali
161700061017     c                   if        o95lna<>sqllna or
161800061017     c                             (p76fel='N' and
161900061017     c                             (O95znc <> SQLznc
162000061017     c                             or  O95cap <> SQLcad
162100061017     c                             or (O95prv <> SQLprd
162200061017     c                             and O95pra <> SQLprd)))
162300060302     c                   add       1             wConta
162400060302     c                   add       1             wContaPo
162500060302     c                   add       1             wContaRot
162600060302     c                   add       SQLncl        wColli
162700060302     c                   add       SQLncl        wColliPo
162800060302     c                   add       SQLncl        wColliRot
162900061017     c                   endif
163000061017     c                   if        p76obs='S' and
163100061017     c                             (o95lf1='O' or o95cf1='O')
163200061017     c                   add       1             wContaob
163300061017     c                   add       1             wContaPoob
163400061017     c                   add       1             wContaRotob
163500061017     c                   add       SQLncl        wColliob
163600061017     c                   add       SQLncl        wColliPoob
163700061017     c                   add       SQLncl        wColliRotob
163800061017     c                   endif
163900060302      *
164000060302      * Controlla se la bolla è dirottata
164100060303if  1c                   if            P76pav  = 'S'
164200060303     c                             and SQLcca  = '1'
164300060302     c                   add       1             wContaDir
164400060302e   1c                   endif
164500060302      *
164600060302      * Controlla se la bolla è destinata a supermercato
164700060302if  1c                   IF            SQLtc1  = 'S'
164800060302     c                             or  SQLtc2  = 'S'
164900060302if  2c                   if        SQLtc1  = 'S'
165000060302     c                   eval      WPDftc  = SQLtc1
165100060302e   2c                   endif
165200060302if  2c                   if        SQLtc2  = 'S'
165300060302     c                   eval      WPDftc  = SQLtc2
165400060302e   2c                   endif
165500060302     c                   add       1             wSup
165600060302     c                   add       1             wSupPo
165700060302     c                   add       1             wSupRot
165800060302e   1c                   ENDIF
165900060414      *
166000060414      *
166100060414      * Stampa
166200060420if  1c                   IF        P76tot  = 'N'
166300060420if  2c                   if        *inOF                                        *fine pagina
166400060414     c                   except    PRTtxt                                       *testata
166500060414     c                   except    PRTtx2                                       *intestazione
166600060414     c                   eval      *inOF   = *off
166700060420e   2c                   endif
166800060303      *                  ________________
166900060302     c                   except    PRTdet                                       *dettaglio
167000060303      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
167100060420e   1c                   ENDIF
167200060302      *
167300060414      * (Verifico eventuali incongruenze tra CAP e PROVINCIA)
167400060302if  1c                   IF        O95lia <> '3'
167500060302      *
167600060420if  2c                   If        P76tot  = 'N'
167700060302     c                   movel(p)  $Decod(10)    WPEmsg2
167800060420if  3c                   if        *inOF                                        *fine pagina
167900060302     c                   except    PRTtxt                                       *testata
168000060302     c                   except    PRTtx2                                       *intestazione
168100060302     c                   eval      *inOF   = *off
168200060420e   3c                   endif
168300060302     c                   except    PRTerr2                                      *errore
168400060420e   2c                   EndIf
168500060302      *
168600060302x   1c                   ELSE
168700060302      *
168800060420if  2c                   If           (O95lna  = *zeros
168900060420     c                             or  O95err <> *blanks)
169000060420     c                             and P76tot  = 'N'
169100060420if  3c                   if        *inOF                                        *fine pagina
169200060420     c                   except    PRTtxt                                       *testata
169300060420     c                   except    PRTtx2                                       *intestazione
169400060420     c                   eval      *inOF   = *off
169500060420e   3c                   endif
169600060302     c                   except    PRTerr1                                      *errore
169700060420e   2c                   EndIf
169800060302      *
169900060302e   1c                   ENDIF
170000060302      *
170100060302     c                   ENDSR
170200060302      *
170300060302      *---------------------------------------------------------------*
170400060302      *?Scrittura singolo record nel work-file                       ?*
170500060302      *---------------------------------------------------------------*
170600060414     c     sr_WriteWF    BEGSR
170700060302      *
170800060302      * Reperisco i dati di classificazione geografica del cliente
170900060303     c                   movel     SAVksc        SI76fil
171000060302     c     SI76fil       chain     AZORG
171100060302     c                   if        %found(AZORG01L)
171200060303     c                   eval      SI76dis  = ORGfl3
171300060303     c                   eval      SI76ara  = ORGcar
171400060302     c                   endif
171500060303     c                   eval      SI76ksc  = SAVksc
171600060302     c                   eval      SI76rag  = §3Crag
171700060302     c                   eval      SI76lnp  = SAVlnp
171800060302     c                   eval      SI76nrs  = SAVnrs
171900060302     c                   eval      SI76gpb  = WPDgesPkb
172000060302     c                   eval      SI76gss  = WPDsegSeq
172100060302     c                   eval      SI76ver  = WPDverCap
172200060302     c                   eval      SI76spet = wContaRotTot
172300060302     c                   eval      SI76colt = wColliRotTot
172400060302     c                   eval      SI76spee = wContaRot
172500060302     c                   eval      SI76cole = wColliRot
172600060302     c                   eval      SI76dirt = wContaDirTot
172700060302     c                   eval      SI76dire = wContaDir
172800060302     c                   eval      SI76supt = wSupRotTot
172900060302     c                   eval      SI76supe = wSupRot
173000061018     c                   eval      SI76speo = wContaRotob
173100061018     c                   eval      SI76colo = wColliRotob
173200060302      *                  __________________
173300060302     c                   WRITE     WFSI7600
173400060303      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
173500060302     c                   ENDSR
173600060302      *
173700060302      *---------------------------------------------------------------*
173800060302      *?Chiusura archivi                                             ?*
173900060302      *---------------------------------------------------------------*
174000060302     c     sr_Close      BEGSR
174100060302      *
174200060302      * WorkFile Bolle Sede
174300060302if  1c                   if        P76pav  = 'S'
174400060302     c                   close     WFSI760F
174500060302e   1c                   endif
174600060302      *
174700060302     c                   ENDSR
174800060302      *
174900060302      *---------------------------------------------------------------*
175000060302      *?Operazioni finali                                            ?*
175100060302      *---------------------------------------------------------------*
175200060302     c     RoutFin       BEGSR
175300060302      *
175400060302      * Stampa totale ultima serie elaborata
175500060302     c                   exsr      sr_TotNRS
175600060302      *
175700060302      * Stampa totale ultimo P.O. di partenza elaborato
175800060302     c                   exsr      sr_TotLNP
175900060302      *
176000060302      * Stampa riepilogo dei dati
176100060302     c                   exsr      sr_TotGEN
176200060302      *
176300060302      * Operazioni di chiusura
176400060302     c                   exsr      sr_Close
176500060302      *
176600060302      * Chiusura programmi
176700060302     c                   clear                   TISI95ds
176800060303     c                   eval      I95tla  = 'C'
176900060302     c                   call      'TISI95R'
177000060302     c                   parm                    TISI95ds
177100060302      *
177200060302     c                   clear                   TIBS69ds
177300060302     c                   clear                   DS_cnaco
177400060302     c                   clear                   DS_cnind
177500060302     c                   clear                   DS_cnclp
177600060302     c                   clear                   DS_fncls
177700060302     c                   eval      I69tla  = 'C'
177800060302     c                   call      'TIBS69R'
177900060302     c                   parm                    TIBS69ds
178000060302     c                   parm                    DS_cnaco
178100060302     c                   parm                    DS_cnind
178200060302     c                   parm                    DS_cnclp
178300060302     c                   parm                    DS_fncls
178400060302      *
178500060302     c                   clear                   FNLV55ds
178600060302     c                   eval      D55tla  = 'C'
178700060302     c                   call      'FNLV55R'
178800060302     c                   parm                    FNLV55ds
178900060302      *
179000060302     c                   ENDSR
179100060224      *
179200060224      *?O U T P U T?--------------------------------------------------*
179300060224      *
179400060224      * Testata iniziale
179500060224     oPRTF198   e            PRTtxt            2
179600060224     o                       $Txt(1)             66
179700060224     o                       $Txt(2)            132
179800060224     o                       $Txt(3)            198
179900060224     o                       RSUT                20
180000060420     o                       SDSpgm              55
180100060420     o                       *date         y    150
180200060420     o                       wTime              + 5 '0 :  :  '
180300060306     o                       Page          z    198
180400060224      * Parametri di lancio
180500060420     o          e            PRTprm      3
180600060224     o                       $Txt(14)           132
180700060420     o          e            PRTprm      2
180800060224     o                       $Txt(15)           132
180900060224     o                       P76pav              94
181000060306     o                       WPPpav             111
181100060420     o          e            PRTprm      2
181200060224     o                       $Txt(16)           132
181300060224     o                       WPPds1        8    103
181400060224     o                       WPPds2        8    118
181500060224     o          e            PRTprm      1
181600060420     o                       $Txt(17)           132
181700060306     o                       P76ksc        z    100
181800060224     o                       WPPksc             131
181900060224     o          e            PRTprm      1
182000060420     o                       $Txt(18)           132
182100060306     o                       P76lnp        z     96
182200060306     o                       WPPlnp             131
182300060224     o          e            PRTprm      1
182400060420     o                       $Txt(19)           132
182500060306     o                       P76lna        z     96
182600060306     o                       WPPlna             131
182700060224     o          e            PRTprm      1
182800060420     o                       $Txt(20)           132
182900060306     o                       P76nrs        z     95
183000060420     o          e            PRTprm      1
183100060420     o                       $Txt(21)           132
183200060420     o                       P76fns              94
183300060414     o          e            PRTprm      1
183400060414     o                       $Txt(22)           132
183500060414     o                       P76fel              94
183600060414     o          e            PRTprm      1
183700060414     o                       $Txt(23)           132
183800060414     o                       P76tot              94
183900061018     o          e            PRTprm      1
184000061018     o                       $Txt(24)           132
184100061018     o                       P76obs              94
184200060224      * Testata del dettaglio
184300060224     o          e            PRTtx2      1
184400060224     o                       $Txt(4)             66
184500060224     o                       $Txt(4)            132
184600060224     o                       $Txt(4)            198
184700060224     o          e            PRTtx2      1
184800060224     o                       $Txt(5)             66
184900060224     o                       $Txt(8)            132
185000060224     o                       $Txt(11)           198
185100060224     o          e            PRTtx2      1
185200060224     o                       $Txt(6)             66
185300060224     o                       $Txt(9)            132
185400060224     o                       $Txt(12)           198
185500060224     o          e            PRTtx2      1
185600060224     o                       $Txt(4)             66
185700060224     o                       $Txt(4)            132
185800060224     o                       $Txt(4)            198
185900060224      * Dettaglio
186000060224     o          e            PRTdet      1
186100060303     o                       WPDdat        y     10
186200060303     o                       SQLlnp              14
186300060303     o                       SQLnrs        z     17
186400060303     o                       SQLnsp        z     25
186500060303     o                       SQLcbo              28
186600060303     o                       SQLksc        z     36
186700060303     o                       SQLnzd              40
186800060303     o                       SQLcad              50
186900060303     o                       WPDloc1             77
187000060303     o                       SQLprd              80
187100060303     o                       SQLtfp              84
187200060303     o                       SQLtsp              87
187300060303     o                       WPDftc              89
187400060303     o                       SQLffd              92
187500060303     o                       WPDfca              95
187600060303     o                       SQLpkb        4    103
187700060303     o                       SQLvlb        4    110
187800060303     o                       SQLlna             114
187900060303     o                       SQLznc             117
188000061017     o                                          121 '-->'
188100061017     o                       O95lna             125
188200061017     o                       O95znc             128
188300061017     o                       WPDlid             139
188400060303     o                       WPDlil             150
188500061018     o                       WPDxla             155
188600061018     o                       WPDxzc             158
188700061018     o                       WPDxls             163
188800061018     o                       WPDxzs             166
188900061018     o                       WPDxlo             170
189000061018     o                       WPDxzo             173
189100061018     o                       WPDxzf             177
189200061018     o                       SQLrmn        z    194
189300061018     o                       wpdobs             197
189400060302      * Segnalazione errore (1)
189500060302     o          e            PRTerr1     1
189600060302     o                       WPEmsg1            182
189700060302     o                       SQLrma             198
189800060302      * Segnalazione errore (2)
189900060302     o          E            PRTerr2     1
190000060307     o                       WPDloc2             62
190100060302     o                       WPEmsg2            191
190200060302      * Riga vuota
190300060302     o          e            PRTnul      2
190400060224      * Totale
190500060302     o          E            PRTtot      1
190600060302     o                       WPDtxt             198
190700061018     o          E            PRTtotobs   1
190800061018     o                       WPDtxtobs          198
190900060302      * Fine stampa
191000060302     o          e            PRTend      2
191100060224     o                       $Txt(26)           132
191200060224      *
191300060224      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?*
191400060224      *
191500060303** - $Sql_Fld1/$Sql_Fld2                Note (eccezioni)
191600060303aas       as SQLaas        1
191700060303mgs       as SQLmgs        2
191800060303lnp       as SQLlnp        3
191900060303nrs       as SQLnrs        4
192000060303nsp       as SQLnsp        5
192100060303cbo       as SQLcbo        6
192200060303kscccmccm as SQLksc        7            TASccm, VABccm
192300060303cad       as SQLcad        8
192400060303lod       as SQLlod        9
192500060303prd       as SQLprd       10
192600060303nzd       as SQLnzd       11
192700060303tfptfplnp as SQLtfp       12                    VABlnp...
192800060303tsp       as SQLtsp       13
192900060303tc1ftctc1 as SQLtc1       14            TASftc
193000060303tc2       as SQLtc2       15
193100060303ffd       as SQLffd       16
193200060303ncl       as SQLncl       17
193300060303pkb       as SQLpkb       18
193400060303vlb       as SQLvlb       19
193500060303lna       as SQLlna       20
193600060303znc       as SQLznc       21
193700060303rmn       as SQLrmn       22
193800060303rma---    as SQLrma       23            no TITAS
193900060303fvb   --- as SQLfvb       24            no FIVAB
194000060322lnp   fgs as SQLfgs       25            no FNBLP/FNARB/TITAS
194100060303---cca--- as SQLcca       26            no FNBLP/FNARB/FIVAB
194200060303** - $Sql_BLP ---------------------------------------------------------------*
194300060309from   FNBLP00F                                                                1  ex FNBLP01L
194400070222where  BLPffd <> 'M'                                                           2
194500060303   and BLPlnp in (                                                             3
194600060303   and BLPlna in (                                                             4
194700060303   and (BLPaas*10000+BLPmgs) between                                           5
194800060303   and BLPksc =                                                                6
194900060303   and BLPlnp =                                                                7
195000060303   and BLPlna =                                                                8
195100060303   and BLPnrs =                                                                9
195200060303   and BLPnrs <> 0                                                            10
195300060303** - $Sql_ARB ---------------------------------------------------------------*
195400060309from   FNARB00F                                                                1  ex FNARB01L
195500070222where  ARBffd <> 'M'                                                           2
195600060303   and ARBlnp in (                                                             3
195700060303   and ARBlna in (                                                             4
195800060303   and (ARBaas*10000+ARBmgs) between                                           5
195900060303   and ARBksc =                                                                6
196000060303   and ARBlnp =                                                                7
196100060303   and ARBlna =                                                                8
196200060303   and ARBnrs =                                                                9
196300060303   and ARBnrs <> 0                                                            10
196400060303** - $Sql_TAS ---------------------------------------------------------------*
196500060303from   TITAS00F                                                                1
196600060303where  TASffd <> 'M'                                                           2
196700060303   and TASlnp in (                                                             3
196800060303   and TASlna in (                                                             4
196900060303   and (TASaas*10000+TASmgs) between                                           5
197000060303   and TASccm =                                                                6
197100060303   and TASlnp =                                                                7
197200060303   and TASlna =                                                                8
197300060303   and TASnrs =                                                                9
197400060303   and TASnrs <> 0                                                            10
197500060303** - $Sql_VAB ---------------------------------------------------------------*
197600060309from   FIVAB00F                                                                1  ex FIVAB04L
197700060303where  VABatb =  ' '  and  VABffd <> 'M'                                       2
197800060303   and VABlnp in (                                                             3
197900060303   and VABlna in (                                                             4
198000060303   and (VABaas*10000+VABmgs) between                                           5
198100060303   and VABccm =                                                                6
198200060303   and VABlnp =                                                                7
198300060303   and VABlna =                                                                8
198400060303   and VABnrs =                                                                9
198500060303   and VABnrs <> 0                                                            10
198600060224** - $Txt -------------------------------------------------------*
198700060420xxxxxxxxxxxxxxxxxxxx                         xxxxxxxxxx              1
198800060224            ***   Errori  di instradamento   ***                     2
198900060420        99/99/9999     99:99:99                          Pag. 9999   3
199000060224------------------------------------------------------------------   4
199100060224Instradamento bolla                                località [01-20   5
199200060224Data       lnp spedizione cb cliente naz cap       località [21-35   6
19930006022499/99/9999 999 99 9999999 xx 9999999 xxx xxxxxxxxx xxxxxxxxxxxxxxx   7
199400061017]                                                       Instradame   8
199500061017]           pv tfp ts s fd ca  peso   volume lna zc --> lna zc       9
199600061017xxxxxxxxxxx xx 999  x x  x  x999999,9 99,999 999 99     999 99 xxx  10
199700061017nto calcolato     |Standard| Sotto| Oltre|FDep| Rifer.Mittente|Obs  11
199800061017                  | lna zc |lna zc|lna zc| zc |numerico / alfa|c/l  12
199900061017xxxxxxx xxxxxxxxxx  999 99  999 99 999 99  99  xxxxxxxxxxxxxxx  X   13
200000060224            ***     Parametri  di lancio     ***                    14
200100060306Selezione bolle .........: x       xxxxxxxxxx                       15
200200060224Data spedizione .........: 99/99/9999  al 99/99/9999                16
200300060420Cliente .................: 9999999 xxxxxxxxxxxxxxxxxxxxxxxxx        17
200400060420P.O. di partenza ........: 999     xxxxxxxxxxxxxxxxxxxxxxxxx        18
200500060420P.O. di arrivo ..........: 999     xxxxxxxxxxxxxxxxxxxxxxxxx        19
200600060420Serie  ..................: 99                                       20
200700060420Solo bolle con serie ....: x                                        21
200800060414Solo errori P.O. arrivo .: x                                        22
200900060414Solo totali .............: x                                        23
201000061018Anche bolle cap/loc obsol: x                                        24
201100060224                                                                    25
201200060224            **  F i n e    p r o s p e t t o  **                    26
201300060224  TOTALE GENERALE spedizioni con errori di instradamento:           27
201400060224  TOTALE GENERALE spedizioni:                                       28
201500060224  per colli:                                                        29
201600060224  Cliente gestisce peso:                                            30
201700060224  Segnacolli seq.:                                                  31
201800060224  Versione cappario:                                                32
201900060303** - $Decod (decodifiche in stampa) -----------------------------*
202000060224CAP GEN                                                              1
202100060224CAP                                                                  2
202200060224LOCALITA'                                                            3
202300060224SINONIMI                                                             4
202400060224ECCEZIONI                                                            5
202500060224STANDARD                                                             6
202600060224SOTTO                                                                7
202700060224OLTRE                                                                8
202800060224F.DEPOSITO                                                           9
202900060224PROVINCIA E/O CAP INDICATI INCONGRUENTI E/O LOCALITA' NON TROVATA   10
