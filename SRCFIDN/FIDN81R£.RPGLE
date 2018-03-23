000100050406     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050406
000300050406      *------------------------------------------------------------------------*
000400050406      *
000500060426      *  Crea work files statistica danni per bilancio
000600050406      *
000700050406      *------------------------------------------------------------------------*
000800050406      *
000900050406      *  FASI C.A. FISSATE A PROGRAMMA
001000050406      *------------------------------------------------------------------------*
001100060426      * 100 = Conferma CA                                   WF100
001200050406      * 700 = Progetto di liquidazione                      WF700
001300050406      * 900 = Chiusura CA                                   WF900
001400050406
001500050406
001600050406      *------------------------------------------------------------------------*
001700050406
001800050406     fFndct01l  if   e           k Disk
001900050406     fTitas30c  if   e           k Disk
002000050406     fFndcl01l  if   e           k Disk
002100060426     fTntbe01l  if   e           k Disk
002200060427     fWfdab10f  o    e             Disk    prefix(w1_) rename(wfdab000:wfdabw1)
002300060427     fWfdab20f  o    e             Disk    prefix(w2_) rename(wfdab000:wfdabw2)
002400060427     fWfdab30f  o    e             Disk    prefix(w3_) rename(wfdab000:wfdabw3)
002500060427     fWfdab40f  o    e             Disk    prefix(w4_) rename(wfdab000:wfdabw4)
002600050406
002700050406      *------------------------------------------------------------------------*
002800050406      *  RIEPILOGO INDICATORI
002900050406      *------------------------------------------------------------------------*
003000050406
003100060426      *------------------------------------------------------------------------*
003200060426      *   S C H I E R E
003300060426      *------------------------------------------------------------------------*
003400060426
003500060428     d Cch             s              2    Dim(20)                              causali chiusura pag
003600060426
003700050406      *------------------------------------------------------------------------*
003800050406      *   V A R I A B I L I
003900050406      *------------------------------------------------------------------------*
004000060426
004100050406     d wfase           s              1    inz('0')
004200050407     d wfpr            s                   like(DctFpr)
004300050406     d wspe            s              1    inz('0')
004400060427     d wf100           s                   like(DctFca) inz(100)
004500050406     d wf700           s                   like(DctFca) inz(700)
004600050406     d wf900           s                   like(DctFca) inz(900)
004700060426     d Ktbe            s                   like(tbecod) inz('CCH')
004800060427     d Kdct            s                   like(dctaac)
004900050406
005000060426     d k               s              2  0
005100050406      *------------------------------------------------------------------------*
005200050406      *   D S   I N T E R N E / E S T E R N E
005300050406      *------------------------------------------------------------------------*
005400050406     d                 ds
005500050406     d  dctaac                 1      4  0
005600050406     d  dctmgc                 5      8  0
005700050406     d  dataca                 1      8  0
005800050406
005900050406     d                 ds
006000050406     d  tasaas                 1      4  0
006100050406     d  tasmgs                 5      8  0
006200050406     d  datasp                 1      8  0
006300050406
006400050406     d fidn80ds      e ds
006500060427
006600060427     d wfdabds       e ds                  extname(wfdab10f)
006700060427     d wfdabds1      e ds                  extname(wfdab10f) prefix(W1_)
006800060427     d wfdabds2      e ds                  extname(wfdab10f) prefix(W2_)
006900060427     d wfdabds3      e ds                  extname(wfdab10f) prefix(W3_)
007000060427     d wfdabds4      e ds                  extname(wfdab10f) prefix(W4_)
007100060428     d fndclds       e ds                  extname(fndcl00f)
007200050406     d ddct01        e ds
007300060426     d dcch          e ds
007400050406     d kpjba         e ds
007500050406
007600050406      *------------------------------------------------------------------------*
007700050406
007800060427
007900060427      * caricamento tabella causali chiusura
008000060427
008100060427     c                   exsr      SR_Carcau
008200060427
008300060427      * preparo la chiave per la letura delle CA con anno di apertura
008400060427
008500060427     c                   movel     d80cad        kdct
008600060427      * Mi posiziono con anno CA
008700060427     c     kdct          setll     Fndct01l
008800060427
008900060427     c                   Do        *hival
009000060427     c                   read      Fndct01l
009100060427      *
009200060427     c                   If        %eof(Fndct01l)
009300060427     c                   leave
009400050406     c                   EndIf
009500050406      * controllo la data apertura c.a.
009600050406     c                   If        dataca < d80cad or
009700050406     c                             dataca > d80caa
009800050406     c                   Iter
009900050406     c                   EndIf
010000050406
010100050406     c                   Eval      ddct01 = DctFlo
010200050406      * controllo il tipo gestione
010300050407     c                   Eval      wfpr = dctfpr
010400050407     c                   If        §dctpfra <> *Blanks
010500050407     c                   Eval      wfpr = §dctpfra
010600050406     c                   EndIf
010700050406
010800050406      * Aggancio la bolla
010900050406     c                   Eval      wspe = *Off
011000060428     c     kTitas        Chain     Titas30c
011100060428
011200060428     c                   If        %found(Titas30c)  and
011300050406      * controllo la data spedizione
011400060428     c                             (datasp >= d80spd and
011500060428     c                             datasp <= d80spa)
011600050406     c                   Eval      wspe = *On
011700060428     c                   Endif
011800050406
011900050406     c                   If        wspe = *Off
012000050406     c                   Iter
012100050406     c                   EndIf
012200050406
012300050406      * Ok c.a. giusta scrivo il file
012400050207
012500060427     c                   Clear                   wfdabds
012600050406     c                   Eval      dabdte = *date
012700050406     c                   Eval      dabute = knmus
012800050406     c                   Eval      dabaas = dctaas
012900050406     c                   Eval      dablnp = dctlnp
013000050406     c                   Eval      dabnrs = dctnrs
013100050406     c                   Eval      dabnsp = dctnsp
013200050406     c                   Eval      dabdsp = datasp
013300050406     c                   Eval      dabaac = dctaac
013400050406     c                   Eval      dabfil = dctfil
013500050406     c                   Eval      dabnca = dctnca
013600050406     c                   Eval      dabdca = dataca
013700050406     c                   Eval      dabfpr = dctfpr
013800050407     c                   If        §dctpfra <> *Blanks
013900050406     c                   Eval      dabfpr = §dctpfra
014000050406     c                   EndIf
014100060427
014200060427     c                   Eval      dabfca = dctfca
014300050406      * Aggancio il file per gli importi
014400050406     c     kFndct        Chain     Fndcl01l
014500050406      * deve esserci per forza ma non si sa mai
014600050406     c                   If        Not %Found(Fndcl01l)
014700060428     c                   clear                   fndclds
014800050406     c                   EndIf
014900060427
015000060427      * importo liquidato
015100060427     c                   Eval      dabipl = dclipl
015200060427      * se causale di chiusura = 13 verifico che cosa c'è di valorizzato tra i vari importi
015300060427      * importo liquidato
015400060427      * importo trattenuto
015500060427      * importo richiesto
015600060427      * importo massimo risarcibile
015700060427     c                   If        dctcch = '13'
015800060427
015900060427     c                   SELECT
016000060427
016100060427     c                   When      dclipl > 0
016200060427     c                   Eval      dabipl = dclipl
016300060427     c                   When      dclipt > 0
016400060427     c                   Eval      dabipl = dclipt
016500060427     c                   When      dclipr > 0
016600060427     c                   Eval      dabipl = dclipr
016700060427     c                   other
016800060427     c                   Eval      dabipl = dctipv
016900060427
017000060427     c                   Endsl
017100060427     c                   Endif
017200050406
017300050406     c                   Eval      dabdch = dctdch
017400050406     c                   Eval      dabcch = dctcch
017500050406     c                   Eval      dabipv = dctipv
017600060427
017700060427      * verifico che file devo scrivere
017800060427
017900060427      * verifico se ca chiusa con causali di pagamento
018000060427     c                   If        dctcch <> *blanks
018100060428     c     dctcch        lookup    cch                                    30
018200060427     c                   if        %found
018300060427      * CA pagamento
018400060427     c                   If        D80ab2 = 'X'
018500060427     c                   movel     wfdabds       wfdabds2
018600060427
018700060427     c                   Write     wfdabw2
018800060427     c                   iter
018900060427     c                   endif
019000060427
019100060427     c                   else
019200060427      * CA chiuse no pagamento
019300060427     c                   If        D80ab1 = 'X'
019400060427     c                   movel     wfdabds       wfdabds1
019500060427
019600060427     c                   Write     wfdabw1
019700060427     c                   iter
019800060427     c                   endif
019900060427     c                   endif
020000050406
020100060427     c                   else
020200060427      * CA Aperte
020300060427
020400060427      * verifico la fase
020500060427     c                   If        dctfca = 700
020600060427     c                   If        D80ab3 = 'X'
020700060427     c                   movel     wfdabds       wfdabds3
020800060427
020900060427     c                   Write     wfdabw3
021000060427     c                   iter
021100060427     c                   endif
021200060427     c                   endif
021300060427      * fase tra 100 e 700
021400060428     c                   if         dctfca > 99  and dctfca < 700
021500060427     c                   If        D80ab4 = 'X'
021600060427     c                   movel     wfdabds       wfdabds4
021700060427
021800060427     c                   Write     wfdabw4
021900060427     c                   iter
022000060427     c                   endif
022100060427     c                   endif
022200060427
022300060427     c                   endif
022400060427
022500050406     c                   EndDo
022600050406
022700050406     c                   Eval      *InLr = *On
022800050406
022900060426      *------------------------------------------------------------------------*
023000060426      * Caricamento  tabelle causali di chiusura pagamento
023100060426      *------------------------------------------------------------------------*
023200060426     c     SR_Carcau     BegSr
023300060426
023400060426     c                   clear                   K
023500060426     c     Ktbe          Setll     Tntbe01l
023600060426     c                   do        *Hival
023700060426     c     Ktbe          Reade     Tntbe01l
023800060426
023900060426    1c                   If        %Eof(Tntbe01l)
024000060426     c                   Leave
024100060426     c                   Endif
024200060426
024300060426     c                   If        Tbeatb <> ' '
024400060426     c                   Iter
024500060426     c                   Endif
024600060426
024700060426     c                   Movel     Tbeuni        Dcch
024800060426      * Se il flag di cauasle per pagare carico in schiera
024900060426    3c                   If        §cchpaga = 'S'
025000060426     c                   Add       1             K
025100060426     c                   Movel     Tbeke1        cch(K)
025200060426    3c                   Endif
025300060426
025400060426    2c                   Enddo
025500060426
025600060427      * aggiungo la 07 pagamento Consuldanni
025700060427     c                   add       1             K
025800060427     c                   movel     '07'          cch(k)
025900060427      * aggiungo la 12 C.A. trattenuta dal cliente
026000060427     c                   add       1             K
026100060427     c                   movel     '12'          cch(k)
026200060427      * aggiungo la 13 C.A. trattenuta dal cliente
026300060427     c                   add       1             K
026400060427     c                   movel     '13'          cch(k)
026500060427
026600060426     c                   Endsr
026700050406      *------------------------------------------------------------------------*
026800050406      * ROUTINE INIZIALE
026900050406      *------------------------------------------------------------------------*
027000050406     c     *Inzsr        BegSr
027100050406
027200050406     c     *Entry        Plist
027300050406     c                   Parm                    kpjba
027400050406
027500050406     c                   Movel     kpjbu         fidn80ds
027600060427
027700060427     c     kFndct        Klist
027800060427     c                   Kfld                    dctaac
027900060427     c                   Kfld                    dctfil
028000060427     c                   Kfld                    dctnca
028100050406
028200050406     c     kTitas        Klist
028300050406     c                   Kfld                    dctaas
028400050406     c                   Kfld                    dctlnp
028500050406     c                   Kfld                    dctnrs
028600050406     c                   Kfld                    dctnsp
028700050406
028800050406     c                   EndSr
