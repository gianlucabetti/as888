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
002600080325     fwfdab50f  o    e             disk    prefix(w5_) rename(wfdab000:wfdabw5)
002700050406
002800050406      *------------------------------------------------------------------------*
002900050406      *  RIEPILOGO INDICATORI
003000050406      *------------------------------------------------------------------------*
003100050406
003200060426      *------------------------------------------------------------------------*
003300060426      *   S C H I E R E
003400060426      *------------------------------------------------------------------------*
003500060426
003600060428     d Cch             s              2    Dim(20)                              causali chiusura pag
003700060426
003800050406      *------------------------------------------------------------------------*
003900050406      *   V A R I A B I L I
004000050406      *------------------------------------------------------------------------*
004100060426
004200050406     d wfase           s              1    inz('0')
004300050407     d wfpr            s                   like(DctFpr)
004400050406     d wspe            s              1    inz('0')
004500060427     d wf100           s                   like(DctFca) inz(100)
004600050406     d wf700           s                   like(DctFca) inz(700)
004700050406     d wf900           s                   like(DctFca) inz(900)
004800060426     d Ktbe            s                   like(tbecod) inz('CCH')
004900060427     d Kdct            s                   like(dctaac)
005000050406
005100060426     d k               s              2  0
005200050406      *------------------------------------------------------------------------*
005300050406      *   D S   I N T E R N E / E S T E R N E
005400050406      *------------------------------------------------------------------------*
005500050406     d                 ds
005600050406     d  dctaac                 1      4  0
005700050406     d  dctmgc                 5      8  0
005800050406     d  dataca                 1      8  0
005900050406
006000050406     d                 ds
006100050406     d  tasaas                 1      4  0
006200050406     d  tasmgs                 5      8  0
006300050406     d  datasp                 1      8  0
006400050406
006500050406     d fidn80ds      e ds
006600060427
006700060427     d wfdabds       e ds                  extname(wfdab10f)
006800060427     d wfdabds1      e ds                  extname(wfdab10f) prefix(W1_)
006900060427     d wfdabds2      e ds                  extname(wfdab10f) prefix(W2_)
007000060427     d wfdabds3      e ds                  extname(wfdab10f) prefix(W3_)
007100060427     d wfdabds4      e ds                  extname(wfdab10f) prefix(W4_)
007200080325     d wfdabds5      e ds                  extname(wfdab10f) prefix(W5_)
007300060428     d fndclds       e ds                  extname(fndcl00f)
007400050406     d ddct01        e ds
007500060426     d dcch          e ds
007600050406     d kpjba         e ds
007700050406
007800050406      *------------------------------------------------------------------------*
007900050406
008000060427
008100060427      * caricamento tabella causali chiusura
008200060427
008300060427     c                   exsr      SR_Carcau
008400060427
008500060427      * preparo la chiave per la letura delle CA con anno di apertura
008600060427
008700060427     c                   movel     d80cad        kdct
008800060427      * Mi posiziono con anno CA
008900060427     c     kdct          setll     Fndct01l
009000060427
009100060427     c                   Do        *hival
009200060427     c                   read      Fndct01l
009300060427      *
009400060427     c                   If        %eof(Fndct01l)
009500060427     c                   leave
009600050406     c                   EndIf
009700050406      * controllo la data apertura c.a.
009800050406     c                   If        dataca < d80cad or
009900050406     c                             dataca > d80caa
010000050406     c                   Iter
010100050406     c                   EndIf
010200050406
010300050406     c                   Eval      ddct01 = DctFlo
010400050406      * controllo il tipo gestione
010500050407     c                   Eval      wfpr = dctfpr
010600050407     c                   If        §dctpfra <> *Blanks
010700050407     c                   Eval      wfpr = §dctpfra
010800050406     c                   EndIf
010900050406
011000050406      * Aggancio la bolla
011100050406     c                   Eval      wspe = *Off
011200060428     c     kTitas        Chain     Titas30c
011300060428
011400060428     c                   If        %found(Titas30c)  and
011500050406      * controllo la data spedizione
011600060428     c                             (datasp >= d80spd and
011700060428     c                             datasp <= d80spa)
011800050406     c                   Eval      wspe = *On
011900060428     c                   Endif
012000050406
012100050406     c                   If        wspe = *Off
012200050406     c                   Iter
012300050406     c                   EndIf
012400050406
012500050406      * Ok c.a. giusta scrivo il file
012600050207
012700060427     c                   Clear                   wfdabds
012800050406     c                   Eval      dabdte = *date
012900050406     c                   Eval      dabute = knmus
013000050406     c                   Eval      dabaas = dctaas
013100050406     c                   Eval      dablnp = dctlnp
013200050406     c                   Eval      dabnrs = dctnrs
013300050406     c                   Eval      dabnsp = dctnsp
013400050406     c                   Eval      dabdsp = datasp
013500050406     c                   Eval      dabaac = dctaac
013600050406     c                   Eval      dabfil = dctfil
013700050406     c                   Eval      dabnca = dctnca
013800050406     c                   Eval      dabdca = dataca
013900050406     c                   Eval      dabfpr = dctfpr
014000050407     c                   If        §dctpfra <> *Blanks
014100050406     c                   Eval      dabfpr = §dctpfra
014200050406     c                   EndIf
014300060427
014400060427     c                   Eval      dabfca = dctfca
014500050406      * Aggancio il file per gli importi
014600050406     c     kFndct        Chain     Fndcl01l
014700050406      * deve esserci per forza ma non si sa mai
014800050406     c                   If        Not %Found(Fndcl01l)
014900060428     c                   clear                   fndclds
015000050406     c                   EndIf
015100060427
015200060427      * importo liquidato
015300060427     c                   Eval      dabipl = dclipl
015400060427      * se causale di chiusura = 13 verifico che cosa c'è di valorizzato tra i vari importi
015500060427      * importo liquidato
015600060427      * importo trattenuto
015700060427      * importo richiesto
015800060427      * importo massimo risarcibile
015900060427     c                   If        dctcch = '13'
016000060427
016100060427     c                   SELECT
016200060427
016300060427     c                   When      dclipl > 0
016400060427     c                   Eval      dabipl = dclipl
016500060427     c                   When      dclipt > 0
016600060427     c                   Eval      dabipl = dclipt
016700060427     c                   When      dclipr > 0
016800060427     c                   Eval      dabipl = dclipr
016900060427     c                   other
017000060427     c                   Eval      dabipl = dctipv
017100060427
017200060427     c                   Endsl
017300060427     c                   Endif
017400050406
017500050406     c                   Eval      dabdch = dctdch
017600050406     c                   Eval      dabcch = dctcch
017700050406     c                   Eval      dabipv = dctipv
017800060427
017900060427      * verifico che file devo scrivere
018000060427
018100060427      * verifico se ca chiusa con causali di pagamento
018200060427     c                   If        dctcch <> *blanks
018300060428     c     dctcch        lookup    cch                                    30
018400060427     c                   if        %found
018500060427      * CA pagamento
018600060427     c                   If        D80ab2 = 'X'
018700060427     c                   movel     wfdabds       wfdabds2
018800060427
018900060427     c                   Write     wfdabw2
019000060427     c                   iter
019100060427     c                   endif
019200060427
019300060427     c                   else
019400060427      * CA chiuse no pagamento
019500060427     c                   If        D80ab1 = 'X'
019600060427     c                   movel     wfdabds       wfdabds1
019700060427
019800060427     c                   Write     wfdabw1
019900060427     c                   iter
020000060427     c                   endif
020100060427     c                   endif
020200080325      * CA chiuse con saldo a 0
020300080325     c                   if        d80ab5 = 'X' and dabipl = 0
020400080325     c                   movel     wfdabds       wfdabds5
020500080325     c                   Write     wfdabw5
020600080325     c                   iter
020700080325     c                   endif
020800050406
020900060427     c                   else
021000060427      * CA Aperte
021100060427
021200060427      * verifico la fase
021300060427     c                   If        dctfca = 700
021400060427     c                   If        D80ab3 = 'X'
021500060427     c                   movel     wfdabds       wfdabds3
021600060427
021700060427     c                   Write     wfdabw3
021800060427     c                   iter
021900060427     c                   endif
022000060427     c                   endif
022100060427      * fase tra 100 e 700
022200060428     c                   if         dctfca > 99  and dctfca < 700
022300060427     c                   If        D80ab4 = 'X'
022400060427     c                   movel     wfdabds       wfdabds4
022500060427
022600060427     c                   Write     wfdabw4
022700060427     c                   iter
022800060427     c                   endif
022900060427     c                   endif
023000060427
023100060427     c                   endif
023200060427
023300050406     c                   EndDo
023400050406
023500050406     c                   Eval      *InLr = *On
023600050406
023700060426      *------------------------------------------------------------------------*
023800060426      * Caricamento  tabelle causali di chiusura pagamento
023900060426      *------------------------------------------------------------------------*
024000060426     c     SR_Carcau     BegSr
024100060426
024200060426     c                   clear                   K
024300060426     c     Ktbe          Setll     Tntbe01l
024400060426     c                   do        *Hival
024500060426     c     Ktbe          Reade     Tntbe01l
024600060426
024700060426    1c                   If        %Eof(Tntbe01l)
024800060426     c                   Leave
024900060426     c                   Endif
025000060426
025100060426     c                   If        Tbeatb <> ' '
025200060426     c                   Iter
025300060426     c                   Endif
025400060426
025500060426     c                   Movel     Tbeuni        Dcch
025600060426      * Se il flag di cauasle per pagare carico in schiera
025700060426    3c                   If        §cchpaga = 'S'
025800060426     c                   Add       1             K
025900060426     c                   Movel     Tbeke1        cch(K)
026000060426    3c                   Endif
026100060426
026200060426    2c                   Enddo
026300060426
026400060427      * aggiungo la 07 pagamento Consuldanni
026500060427     c                   add       1             K
026600060427     c                   movel     '07'          cch(k)
026700060427      * aggiungo la 12 C.A. trattenuta dal cliente
026800060427     c                   add       1             K
026900060427     c                   movel     '12'          cch(k)
027000060427      * aggiungo la 13 C.A. trattenuta dal cliente
027100060427     c                   add       1             K
027200060427     c                   movel     '13'          cch(k)
027300060427
027400060426     c                   Endsr
027500050406      *------------------------------------------------------------------------*
027600050406      * ROUTINE INIZIALE
027700050406      *------------------------------------------------------------------------*
027800050406     c     *Inzsr        BegSr
027900050406
028000050406     c     *Entry        Plist
028100050406     c                   Parm                    kpjba
028200050406
028300050406     c                   Movel     kpjbu         fidn80ds
028400060427
028500060427     c     kFndct        Klist
028600060427     c                   Kfld                    dctaac
028700060427     c                   Kfld                    dctfil
028800060427     c                   Kfld                    dctnca
028900050406
029000050406     c     kTitas        Klist
029100050406     c                   Kfld                    dctaas
029200050406     c                   Kfld                    dctlnp
029300050406     c                   Kfld                    dctnrs
029400050406     c                   Kfld                    dctnsp
029500050406
029600050406     c                   EndSr
