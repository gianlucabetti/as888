000100040119      *===============================================================*
000200160418      * Tnsf22r    * Tassazione bolle con peso / volume desunto
000300040119      *===============================================================*
000400040119
000500150908     h decedit('0,') datedit(*ymd/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000150713     Fsipes01l  if   e           k disk
001001160418     Fsivlm01l  if   e           k disk
001100150904     Ftntbe01l  if   e           k disk
001200150925     FAzorg01l  if   e           k disk
001300040119      *
001400050526     d kpjba         e ds
001500150904      ** DS  VARIABILI Per calcolo peso desunto
001600150904     d dvpd          e ds
001700150902      ** DS  TNSF20R
001800150902     d dtas          e ds
001900060926     d Dtas01        e ds
002000050607      ** DS Calcolo tassazione  - Varie
002100050607     d Dtasv         e ds
002200150908     d Dtaspes       e ds
002300150925      ** DS campo ORGDE3 di AZORG00F
002400150925     d OG143         e ds
002500150907
002600150907      * campo di comodo
002700150904     d                 ds
002800150908     d Mese                    3      4  0
002900150908     d Anno                    5      8  0
003000150908     d dataamg                 1      8  0
003100150904
003200150902      ** campi di salvataggio  originale di titas prima della tassazione
003300150907     d W_Dtas          s                   like(dtas)
003400150907     d W_Dtasv         s                   like(dtasv)
003500040331      *
003600150902      ** campi di salvataggio tassazione con peso originale di titas
003700150902     d O_Dtas          s                   like(dtas)
003800150907     d O_Dtasv         s                   like(dtasv)
003801160418      *
003802160418      ** campi di salvataggio tassazione con peso desunto
003803160418     d D_Dtas          s                   like(dtas)
003804160418     d D_Dtasv         s                   like(dtasv)
003900040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
004000050526      *
004100040331      * - Campi di comodo
004200150713     d Flg_pes         s              1                                         flag di scarto x PES
004201160420     d Flg_vlm         s              1                                         flag di scarto x VLM
004300150713     d Colli_anal      s              5  2                                      % colli analizzati
004301160419     d Colli_anal_Vlm  s              5  2                                      % colli analizz. VLM
004400150713     d Scost_peso      s              7  2                                      % scostamento peso
004401160420     d Scost_Vlm       s              7  2                                      % scostamento Volume
004500150713     D tipo_tar        s              1  0
004600150911     D R_taspkf        s                   like(taspkf)                         Peso desunto
004601160420     D R_tasvlf        s                   like(tasvlf)                         Volume desunto
004700150911     D O_taspkf        s                   like(taspkf)                         Peso di titas
004701160428     D O_tasvlf        s                   like(tasvlf)                         Volume di titas
004800150911     D O_tasimv        s                   like(tasimv)                         imv pkf di titas
004900150908     D Dtastat         s               d   datfmt(*iso)
005000150904     D dataiso         s               d   datfmt(*iso)
005100150904     D dataeur         s               d   datfmt(*eur)
005200150904     D xx              s              3  0
005300150929     D yy              s              3  0
005400150928     D kk              s              4  0
005500150904     D ktbecod         s                   like(tbecod) inz('VPD')
005600150925     D k_orgfil        s                   like(orgfil)
005700150904      *  - Flags booleani
005800150904     d $EoF            s               n   inz(*off)
005900150904     d $Fine           s               n   inz(*off)
006000060914      *
006100060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
006200150904      * date decorrenza varibili calcolo peso desunto  e variabili calcolo
006300150929     D SDtasped        S              8s 0 DIM(50)                              Data spedizione
006400150929     D ODtasped        S              8s 0 DIM(50) ascend                       Data spedizione
006500150904     D SVpd            S                   DIM(50) like(Dvpd)                   variab.peso desunto
006600150904
006700150928      * filiali estere per esclusione clienti esteri da calcolo peso desunto
006800150928     D FilEst          S              3  0 DIM(400)                             Filiali ESTERE
006900150910
007000150910     c     *entry        plist
007100150910     c                   parm                    kpjba
007200150910     c                   parm                    dtas
007300150910     c                   parm                    dtasv
007400150910     c                   parm                    dtaspes
007500150910
007600150910      * salvataggio dei parametri passati da utilizzare nelle seguenti tassazioni
007700150910     c                   movel     dtas          W_dtas
007800150910     c                   movel     dtasv         W_dtasv
007801160420      * pulizia dei campi di comodo per memorizzare peso e volume desunti
007802160420     c                   clear                   R_taspkf
007803160420     c                   clear                   R_tasvlf
007900150925
008000150925      * verifico la data di tassazione della ds DTASPES
008100150925      * se non valorizzato imposto udate
008200150925     c                   if        taspdtt = 0
008300150925     c                   eval      taspdtt = *date
008400150925     c                   endif
008500150904      * Se Tipo lancio 'C' --> solo chiusura file
008600150908     c                   if        tasptla <> 'C'
008700120321
008800150904      * eseguo la tassazione
008900150904     c                   exsr      Sr_Sf20
008901160608      * verifico se è stata effettuata la tassazione con il volume vdl sul volume reale
008902160608      * se flag volume da fatturare era "R" e poi è diventato "T" o "Z" ricalcolo
008903160608      * senza volume VDL
008904160608    1c                   if        taserr = ' '  and tasfvf = 'R' and
008905160608     c                             tasfvt <> 'R'
008906160608     c                   eval      O_dtas = dtas
008907160608     c                   eval      O_dtasv = dtasv
008908160608     c                   eval      O_tasimv = tasimv
008909160608     c                   eval      dtas =  W_dtas
008910160608     c                   eval      dtasv = W_dtasv
008911160608      * pulisco il volume VDL ed il numero dei colli volume rilevati dal VDL
008912160608     c                   clear                   tasncr
008913160608     c                   clear                   tasvlc
008914160608      * ritasso
008915160608     c                   exsr      Sr_sf20
008916160608      * se non ci sono errori memorizzo l'imponibile nella DTASPES
008917160608    2c                   if        taserr = ' '
008918160608     c                   eval      taspimvvr = tasimv
008919160608      * se imponibile con volume VDL è diverso dall'imponibile con volume reale memorizzo una "S"
008920160608      * che indica che il volume VDL ha modificato la tassazione
008921160608    3c                   if        tasimv <> O_tasimv
008922160608     c                   eval      tasvlmvdl = 'S'
008923160608    3c                   endif
008924160608      * se imponibile con volume VDL è uguale all'imponibile con volume reale memorizzo una "X"
008925160608      * che indica che il volume VDL non ha modificato la tassazione
008926160608    3c                   if        tasimv = O_tasimv
008927160608     c                   eval      tasvlmvdl = 'X'
008928160608    3c                   endif
008929160608     c                   clear                   O_tasimv
008930160608     c                   eval      dtas = O_dtas
008931160608     c                   eval      dtasv = O_dtasv
008932160608    2c                   endif
008933160608
008934160608    1c                   endif
008935160608     c
009000150902      * verifico se la spedizione che devo tassare ha le caratteristiche per applicare
009100160418      * il peso e volume desunto aumentandolo della perentuale di inaffidabilità
009200160418      * rilevata dalle statistiche
009300150904
009400150904     c                   movel     tasctr        tipo_tar
009500150925      * verifico il network del cliente
009600150925     c                   movel     tasksc        K_orgfil
009700150902
009800150904     c                   Select
009900150904
010000150907      * NON ELABORO  :
010100150907      * se tipo bolla non è F1
010200150904     c                   When      tastbl <>  'F1' or
010300150904      * se bolla con almeno un collo rilevato da VDL
010400150904     c                             tasncp <>  0    or
010500150904      * se bolla con peso bollettato diverso dal peso fatturato
010600160607     c                             taspkb <>  taspkf  or
010700150904      * se tariffa  non è a peso
010800150907     c                             (tipo_tar <> 0 AND tipo_tar <> 3) or
010900150907      * se tariffa cliente ha tipo applicazione volume "MAI VDL"
011000150922     c                             tastap = 'M'  or
011100150925      * se tariffa cliente ha tipo applicazione peso desunto Uguale a NO
011200150925     c                             tasepd =  'N' or
011300150928      * se cliente estero
011400150928     c                             %lookup(K_orgfil:FilEst)>0   or
011401151028      * se spedizione  < di 1
011402151028     c                             taspkf <= 1                  or
011500150907      * se è già un manca tariffa
011600150907     c                             taserr <> ' '
011700150904      *
011800150904
011900150904     c                   Other
011901160420      * verifica e preparazioni basi di calcolo valori desunti
011902160420     c                   exsr      Sr_Preparo
011903160419      * se spedizione  <= a 300 kg cerco se posso calcolare il peso desunto
011904160419     c                   If        taspkf <= 300
012000160419     c                   exsr      Sr_Elab_peso
012001160419     c                   Endif
012002160419      * verifico se spedizione valida per calcolo del volume desunto
012003160419      *
012004160607      * flag volume fatturato = a K/P/C (dal 7/6/16 anche sul Reale)
012005160607     c                   If        (tasfvt = 'K' or tasfvt = 'P' or
012006160607     c                             tasfvt = 'C' or tasfvt = 'R')   and
012007160419      * Non sono stati rilevati volumi sul vdl
012100160607     c                             tasncr = 0 and
012101160419      * il volume da fatturare non è superiore a 1,5 Mc
012102160419     c                             tasvlf <= 1,5
012103160419      * posso calcolare il volume desunto
012104160419     c                   exsr      Sr_Elab_vol
012105160419     c                   Endif
012106160420      * eseguo la tassazione con i nuovi valori di peso e volume
012107160420     c                   Exsr      Sr_Tassa
012108160420
012200150904     c                   Endsl
012300150904
012400150904     C                   Endif
012500120323
012600150904      * FINE
012700150904     c                   if        TAStla <> *blanks
012800150904     c                   eval      *inlr = *on
012900150909     c                   movel     'C'           tastla
013000150909     C                   CALL      'TNSF20R'
013100150909     C                   PARM                    KPJBA
013200150909     C                   PARM                    DTAS
013300150909     C                   PARM                    DTASV
013400150904     c                   else
013500150904     c                   eval      *inrt = *on
013600150904     c                   endif
013700150904
013701160420      **-------------------------------------------------------------**
013702160420      ** Verifica e preparazioni basi di calcolo valori desunti      **
013703160420      **-------------------------------------------------------------**
013704160420     c     Sr_Preparo    BEGSR
013800120321      *
013801160420      * mi salvo le ds della tassazione con peso originale bolla
013802160420     c                   eval      O_dtas = dtas
013803160420     c                   eval      O_dtasv = dtasv
013804160420     c                   eval      O_tasimv = tasimv
013805160420     c                   eval      O_taspkf = taspkf
013806160420     c                   eval      O_tasvlf = tasvlf
013807160420      *
013808160420      * imposto i flag del controllo file statistiche
013809160420     c                   eval      flg_pes = 'N'                                No file pes
013810160420     c                   eval      flg_vlm = 'N'                                No file VLM
013811160420      *
013812160420      * cerco le variabili peso in vigore rispetto alla data di tassazione passata nella DTASPES
013813160420      *
013814160420     c                   z-add     1             xx
013815160420     c                   eval      xx=%lookuple(taspdtt:Odtasped)
013816160420     c                   if        xx > 0
013817160420     c                   eval      yy= %lookup(Odtasped(xx):Sdtasped)
013818160420     c                   if        yy > 0
013819160420     c                   eval      dvpd  =  Svpd(yy)
013820160420     c                   mult      -1            §vpdscoda1
013821160420     c                   mult      -1            §vpdscoa1
013822160420     c                   mult      -1            §vpdscoda2
013823160420     c                   mult      -1            §vpdscoa2
013824160420     c                   endif
013825160420     c                   endif
013826160420      *
013827160420      * non trovate variabili in tabella legate alla data spedizione esco dal programma
013828160420     c                   if        xx = 0 or yy = 0
013829160420     c                   eval      tasperr = '1'
013830160420     c                   eval      taspmsg = 'Non trovata schiera VPD'
013831160420     c                   exsr      Sr_Fine
013832160420     c                   leavesr
013833160420     c                   endif
013834160420
013835160420     c                   Endsr
013900120321      **-------------------------------------------------------------**
014000160420      ** Elabora ogni singola bolla calcolo peso desunto             **
014100120321      **-------------------------------------------------------------**
014200160419     c     sr_Elab_Peso  BEGSR
014300150907      *
014302160420     c                   clear                   taspimvd
014303160420     c                   clear                   taspdes
017100150713      * aggancio SIPES
017200150713     c                   exsr      SR_ctrpesVDL
017300150907      * se non ci sono errori ricalcolo con il peso desunto
017400160420    1c                   if        Flg_pes = ' '
017500150907     c                   eval      R_taspkf =
017600150907     c                              (taspkf+((taspkf*Scost_peso*-1)/100))
017700150907      * se il peso ricalcolato è diverso da quello originale ricalcolo
017800160420    2c                   if        R_taspkf  > taspkf
017900150907     c                   eval      dtas = W_dtas
018000150907     c                   eval      dtasv = W_dtasv
018100150907     c                   eval      taspkf = R_taspkf
018200150907      * ritasso
018300150907     c                   exsr      sr_sf20
019701160420      * mi salvo imponibile con peso desunto da memorizzare in dtaspes
019801160420     c                   eval      taspimvd = tasimv
020700150908
020800160420    2c                   Endif
020900150908      * Non trovata statistica o statistica non giusta per i parametri
021000160420   x1c                   Else
021100150908     c                   eval      tasperr = '1'
021200160420    1c                   endif
021500120314      *
021600120323     c                   endsr
021700150904      **-------------------------------------------------------------**
021800150904      ** Controllo statistica pesi VDL                               **
021900150904      **-------------------------------------------------------------**
022000150904     c     sr_ctrpesVDL  BEGSR
022100150904      *
022300150904     c
022400151022      * imposto anno e mese ricerca statistica se non trovato si
022600150904      * cerca indietro fino ad un massimo di mesi stabilito nella tabellla delle variabili calcolo
022700150904      * peso desunto
022800150929     c                   move      tasdsp        dtastat
022900150907     c                   do        §vpdmesi
023000150904     c                   move      Dtastat       dataeur
023100150908     c                   move      dataeur       dataamg
023700150904      * aggancio la statistica
023800150904     c     Kpes          chain     Sipes01l
023900150904     c                   if        %found(Sipes01l)
024000150908     c                   eval      flg_pes = ' '                                Si file pes
024100150904     c                   leave
024200150904     c                   endif
024300150929     c                   subdur    1:*M          Dtastat
024400150904     c                   enddo
024500150904     c
024600150904      * se tutto bene
024700150904     c                   If        not %found(Sipes01l)
024800150904     c                   eval      flg_pes = 'N'                                No file pes
024900150904     c                   clear                   Colli_anal
025000150904     c                   clear                   Scost_peso
025100150904     c                   clear                   pesnsp
025200150904     c                   leavesr
025300150904     c                   endif
025400150909
025500150909      * controllo se la statistica pesi è valida per il calcolo del peso desunto
025600150909
025700150909      * calcolo % colli analizzato
025800150909     c                   eval      Colli_Anal = (pesncv*100)/pestcl
025900150909      * calcolo % scostamento pesi
026000150909     c                   eval      Scost_Peso = ((pespsb-pespsc)*100)/pespsb
026100150909
026200150909     c                   select
026300150904      * verifico numero spedizioni rilevate il  mese precedente
026400150909     c                   when      Pesnsp < §vpdnsped
026500150904     c                   eval      flg_pes = 'S'                                poche spedizioni
026600150930      * verifico se analizzate più del 50% dei colli bollettati
026700150930     c                   When      Colli_anal <= §vpdcolli                      pochi colli analizz.
026800150904     c                   eval      flg_pes = 'C'                                pochi colli
026900150907      * verifico se scostamento peso è nei range impostati in tabella in base al peso della
027000150907      * spedizione
027100150907
027200150907      * 1° range di pesi
027300150909     c                   When      taspkf <= §vpdpkal1 and taspkf >= §vpdpkda1
027400150907     c                             and (Scost_Peso < §vpdscoda1 or              Scostamento pesi
027500150907     c                             Scost_peso > §vpdscoa1)
027600150907     c                   eval      flg_pes = '1'                                scostamento 1° range
027700150907      * 2° range di pesi
027800150909     c                   when      taspkf <= §vpdpkal2 and taspkf >= §vpdpkda2
027900150907     c                             and (Scost_Peso < §vpdscoda2 or              Scostamento pesi
028000150907     c                             Scost_peso > §vpdscoa2)
028100150907     c                   eval      flg_pes = '2'                                scostamento 2°range
028200150909     c                   endsl
028300150904      *
028400150904      *
028500150904     c                   endsr
028600150907      *
028601160420      *
028602160420      **-------------------------------------------------------------**
028603160420      ** Elabora ogni singola bolla calcolo volume desunto           **
028604160420      **-------------------------------------------------------------**
028605160420     c     sr_Elab_Vol   BEGSR
028607160420      *
028609160420     c
028637160420      * aggancio SIVLM
028638160420     c                   exsr      SR_ctrvlmVDL
028639160420      * se non ci sono errori ricalcolo con il peso desunto
028640160420    1c                   if        Flg_vlm = ' '
028641160420     c                   eval      R_tasvlf =
028642160420     c                              (tasvlf+((tasvlf*Scost_vlm*-1)/100))
028685160420      * Non trovata statistica o statistica non giusta per i parametri
028686160420   x1c                   Else
028687160420     c                   eval      tasperr = '1'
028688160420
028689160420    1c                   endif
028690160420      *
028692160420     c                   endsr
028693160419      **-------------------------------------------------------------**
028694160419      ** Controllo statistica Volumi VDL                             **
028695160419      **-------------------------------------------------------------**
028696160420     c     sr_ctrvlmVDL  BEGSR
028697160419      *
028698160420     c                   eval      flg_vlm = 'N'                                No file VLM
028699160419     c
028700160419      * imposto anno e mese ricerca statistica se non trovato si
028701160419      * cerca indietro fino ad un massimo di mesi stabilito nella tabellla delle variabili calcolo
028702160419      * volume desunto
028703160419     c                   move      tasdsp        dtastat
028704160419     c                   do        §vpdmesi
028705160419     c                   move      Dtastat       dataeur
028706160419     c                   move      dataeur       dataamg
028707160419      * aggancio la statistica
028708160419     c     Kpes          chain     Sivlm01l
028709160419     c                   if        %found(Sivlm01l)
028710160420     c                   eval      flg_vlm = ' '                                Si file VLM
028711160419     c                   leave
028712160419     c                   endif
028713160419     c                   subdur    1:*M          Dtastat
028714160419     c                   enddo
028715160419     c
028716160419      * se tutto bene
028717160419     c                   If        not %found(Sivlm01l)
028718160420     c                   eval      flg_vlm = 'N'                                No file VLM
028719160419     c                   clear                   Colli_anal_Vlm
028720160420     c                   clear                   Scost_vlm
028721160419     c                   clear                   vlmnsp
028722160419     c                   leavesr
028723160419     c                   endif
028724160419
028725160419      * controllo se la statistica pesi è valida per il calcolo del peso desunto
028726160419
028727160419      * calcolo % colli analizzato Volume
028728160419     c                   eval      Colli_Anal_Vlm = (vlmncr*100)/vlmtcl
028729160419      * calcolo % scostamento pesi
028730160420     c                   eval      Scost_Vlm = ((vlmvlb-vlmvlc)*100)/vlmvlb
028731160419
028732160419     c                   select
028733160419      * verifico numero spedizioni rilevate il  mese precedente
028734160420     c                   when      Vlmnsp < §vpdnsped
028735160420     c                   eval      flg_vlm = 'S'                                poche spedizioni
028736160420      * verifico se analizzate più del 40% dei colli bollettati
028737160419     c                   When      Colli_anal <= §vpdcolli                      pochi colli analizz.
028738160420     c                   eval      flg_vlm = 'C'                                pochi colli
028739160420      * verifico se scostamento volume è maggire di -100 impostato a programmma
028740160428     c                   When      Scost_vlm < -100 or Scost_vlm > 0            Scostamento volumi
028741160420     c                   eval      flg_vlm = '1'
028742160419      *
028743160420     c                   endsl
028744160419      *
028745160419     c                   endsr
028746160420      *
028747160420      **-------------------------------------------------------------**
028748160420      ** Eseguo la tassazione con i nuovi valori di peso e volume    **
028749160420      **-------------------------------------------------------------**
028750160420     c     sr_Tassa      BEGSR
028751160420
028752160420      * qui mi trovo che potrei tassare con il peso e volume desunto o
028753160420      * solo peso desunto o solo i volume desunto
028754160420
028755160420      * se il flag relativo al peso e al volume sono entrambi diversi da  blank
028756160420      * non ci sono stati elementi validi per ricalcolare con valori DESUNTI
028757160421      * quindi restituisco la ds di tassazione originale
028758160420     c                   if        flg_pes <> ' ' and flg_vlm <> ' '
028759160420     c                   exsr      Sr_fine
028760160422     c                   leavesr
028761160420     c                   endif
028762160420      * imposto i dati nella DTAS con i valori originali
028763160420     c                   eval      dtas = W_dtas
028764160420     c                   eval      dtasv = W_dtasv
028765160420      * se il volume ricalcolato è diverso da quello originale imposto nella DS
028766160428     c                   if        R_tasvlf  > tasvlf
028767160420     c                   eval      tasvlf = R_tasvlf
028768160420     c                   endif
028769160420      * se il peso ricalcolato è diverso zeros da quello originale imposto nella DS
028770160420     c                   if        R_taspkf > 0
028771160420     c                   eval      taspkf = R_taspkf
028772160420     c                   endif
028773160420      * ritasso
028774160420     c                   exsr      sr_sf20
028775160421      * dopo la ritassazione sostituisco i valori del TASPKF e TASVLF
028776160420      * con quelli originali di Titas ed imposto nel campo peso cml al netto della tara
028777160420      * il peso desunto se l'ho utilizzato da memorizzare su fiar5 o stampare in fattura
028780160421     c
028781160422    1c                   if        R_taspkf  > O_taspkf
028782160420     c                   eval      taspkcn = taspkf
028783160420     c                   eval      tasspc = 'D'
028784160420     c                   eval      taspkf = O_taspkf
028785160422    1c                   endif
028786160422    1c                   if        R_tasvlf  > O_tasvlf
028787160420     c                   eval      tasvlf = O_tasvlf
028788160608     c                   eval      tasfvfd = 'D'                                servira' ancora
028789160608     c                   eval      tasfvt  = 'D'
028790160608     c                   eval      tasvlt  = R_tasvlf
028791160422    1c                   endif
028792160421      *
028793160422    1c                   if        taserr <> ' '
028794160420     c                   eval      tasperr = '1'
028795160420     c                   eval      taspmsg = 'Manca tariffa con nuovo Peso'  +
028796160420     c                             %editc(taspkcn:'4')
028797160420     c                   exsr      Sr_Fine
028798160420     c                   leavesr
028799160422    1c                   endif
028800160420      * verifico l'imponibile  se maggiorato applico il nuovo peso altrimenti mantengo il
028801160420      * vecchio  e  la (Bocchi 28/10/15)
028802160420      * differenza tra imponibile del peso desunto - imponibile no peso desunto è > di 0,25
028803160422    1c                   if        tasimv <= O_tasimv or
028804160420     c                             ((tasimv-O_tasimv) <= 0,25)
028805160420     c                   eval      dtas = O_dtas
028806160420     c                   eval      dtasv = O_dtasv
028807160421     c                   eval      taspdes = 'N'                                flag peso desunto
028808160421     c                   eval      tasvdes = 'N'                                flag vol. desunto
028809160422   x1c                   else
028810160422    2c                   if        tasspc = 'D'
028811160421     c                   eval      taspdes = 'S'                                flag peso desunto
028812160422    2c                   endif
028813160422    3c                   if        R_tasvlf > o_tasvlf
028814160421     c                   eval      tasvdes = 'S'                                flag vol. desunto
028815160422    2c                   endif
028816160422    1c                   endif
028817160420
028818160422     c                   exsr      Sr_Fine
028819160421      *
028820160421     c                   endsr
028846160419      *
028847050526      **-------------------------------------------------------------**
028848150904      ** Richiamo pgm tassaz.  tnsf20r                               **
028900050526      **-------------------------------------------------------------**
029000050526     c     sr_sf20       BEGSR
029100050526      *
029200150904     c                   call      'TNSF20R'
029300150908     c                   parm                    Kpjba
029400150904     c                   parm                    dtas
029500150904     c                   parm                    dtasv
029600150904
029700050607     c     e_sf20        endsr
029800150908      *
029900150908      **-------------------------------------------------------------**
030000150908      ** Fine programma memorizzazione dati                          **
030100150908      **-------------------------------------------------------------**
030200150908     c     sr_Fine       BEGSR
030300160421      * valorizzo i dati nella ds di comodo per il peso
030400150908     c                   if        flg_pes = 'N'
030500160421     c                   eval      taspmsg = 'Non trovato record SIPES'
030600150909     c                   clear                   TASPPKD
030700150908     c                   clear                   TASPANNO
030800150908     c                   clear                   TASPMESE
030900150908     c                   clear                   TASPCOLLI
031000150908     c                   clear                   TASPSCOST
031100150908     c                   clear                   TASPNSP
031200150909     c                   clear                   TASPIMV
031201160420     c                   clear                   TASPIMVD
031300150908     c                   endif
031400150908      *
031500150908     c                   if        flg_pes = 'S'
031600150908     c                   eval      taspmsg = 'Poche spedizioni rilevate'
031700150909     c                   clear                   TASPPKD
031800150908     c                   movel     pesann        TASPANNO
031900150908     c                   movel     pesmes        TASPMESE
032000150908     c                   movel     Colli_anal    TASPCOLLI
032100150908     c                   movel     Scost_peso    TASPSCOST
032200150908     c                   movel     pesnsp        TASPNSP
032300150909     c                   clear                   TASPIMV
032301160420     c                   clear                   TASPIMVD
032400150908     c                   endif
032401160201      *
032402160201     c                   if        flg_pes = 'C'
032403160201     c                   eval      taspmsg = '% colli analizzato basso'
032404160201     c                   clear                   TASPPKD
032405160201     c                   movel     pesann        TASPANNO
032406160201     c                   movel     pesmes        TASPMESE
032407160201     c                   movel     Colli_anal    TASPCOLLI
032408160201     c                   movel     Scost_peso    TASPSCOST
032409160201     c                   movel     pesnsp        TASPNSP
032410160201     c                   clear                   TASPIMV
032411160420     c                   clear                   TASPIMVD
032412160201     c                   endif
032500150908      *
032600150908     c                   if        flg_pes = '1'
032700150908     c                   eval      taspmsg = 'Range % scostamento 1 non valido'
032800150909     c                   clear                   TASPPKD
032900150908     c                   movel     pesann        TASPANNO
033000150908     c                   movel     pesmes        TASPMESE
033100150908     c                   movel     Colli_anal    TASPCOLLI
033200150908     c                   movel     Scost_peso    TASPSCOST
033300150908     c                   movel     pesnsp        TASPNSP
033400150909     c                   clear                   TASPIMV
033401160420     c                   clear                   TASPIMVD
033500150908     c                   endif
033600150908      *
033700150908     c                   if        flg_pes = '2'
033800150908     c                   eval      taspmsg = 'Range % scostamento 2 non valido'
033900150909     c                   clear                   TASPPKD
034000150908     c                   movel     pesann        TASPANNO
034100150908     c                   movel     pesmes        TASPMESE
034200150908     c                   movel     Colli_anal    TASPCOLLI
034300150908     c                   movel     Scost_peso    TASPSCOST
034400150908     c                   movel     pesnsp        TASPNSP
034500150909     c                   clear                   TASPIMV
034501160420     c                   clear                   TASPIMVD
034600150908     c                   endif
034601160421      * valorizzo i dati nella ds di comodo per il volume
034602160421     c                   if        flg_vlm = 'N'
034603160421     c                   eval      taspmsg = %trimr(taspmsg) +
034604160421     c                             ' Non trovato record SIVLM'
034605160421     c                   clear                   TASVVLD
034606160421     c                   clear                   TASVANNO
034607160421     c                   clear                   TASVMESE
034608160421     c                   clear                   TASVCOLLI
034609160421     c                   clear                   TASVSCOST
034610160421     c                   clear                   TASVNSP
034611160421     c                   clear                   TASPIMV
034613160421     c                   endif
034614160421      *
034615160421     c                   if        flg_VLM = 'S'
034616160421     c                   eval      taspmsg = %trimr(taspmsg) +
034617160428     c                                       ' Poche spedizioni rilevate SIVLM'
034618160421     c                   clear                   TASVVLD
034619160421     c                   movel     VLMann        TASVANNO
034620160421     c                   movel     VLMmes        TASVMESE
034621160421     c                   movel     Colli_anal_VLMTASVCOLLI
034622160421     c                   movel     Scost_VLM     TASVSCOST
034623160421     c                   movel     VLMnsp        TASVNSP
034624160421     c                   clear                   TASPIMV
034626160421     c                   endif
034627160421      *
034628160421     c                   if        flg_VLM = 'C'
034629160421     c                   eval      taspmsg =  %trimr(taspmsg) +
034630160428     c                                       ' % colli analizzato basso SIVLM'
034631160421     c                   clear                   TASVVLD
034632160421     c                   movel     VLMann        TASVANNO
034633160421     c                   movel     VLMmes        TASVMESE
034634160421     c                   movel     Colli_anal_VLMTASVCOLLI
034635160421     c                   movel     Scost_VLM     TASVSCOST
034636160421     c                   movel     VLMnsp        TASVNSP
034637160421     c                   clear                   TASPIMV
034646160421     c                   endif
034647160421      *
034648160421     c                   if        flg_VLM = '1'
034649160421     c                   eval      taspmsg =  %trimr(taspmsg) +
034650160428     c                                       ' % scostamento  non valido SIVLM'
034651160421     c                   clear                   TASVVLD
034652160421     c                   movel     VLMann        TASVANNO
034653160421     c                   movel     VLMmes        TASVMESE
034654160421     c                   movel     Colli_anal_VLMTASVCOLLI
034655160421     c                   movel     Scost_VLM     TASVSCOST
034656160421     c                   movel     VLMnsp        TASVNSP
034657160421     c                   clear                   TASPIMV
034666160421     c                   endif
034667160421      *
034700150908      *
034800160422     c                   If        flg_pes = ' '
034900150909     c                   movel     R_taspkf      TASPPKD
035000150908     c                   movel     pesann        TASPANNO
035100150908     c                   movel     pesmes        TASPMESE
035200150908     c                   movel     Colli_anal    TASPCOLLI
035300150908     c                   movel     Scost_peso    TASPSCOST
035400150908     c                   movel     pesnsp        TASPNSP
035500150909     c                   movel     O_tasimv      TASPIMV
035501160422     c                   endif
035502160422      *
035503160422      *
035504160422     c                   If        flg_vlm = ' '
035505160421     c                   movel     R_tasvlf      TASVVLD
035506160421     c                   movel     VLMann        TASVANNO
035507160421     c                   movel     VLMmes        TASVMESE
035508160421     c                   movel     Colli_anal_VLMTASVCOLLI
035509160421     c                   movel     Scost_VLM     TASVSCOST
035510160421     c                   movel     VLMnsp        TASVNSP
035511160422     c                   movel     O_tasimv      TASPIMV
035600150908     c                   endif
035700150908      *
035800150908     c                   Endsr
035900120323      *
036000050609      *************************************************************************
036100050609      * Operazioni iniziali
036200050609      *************************************************************************
036300050609     C     *inzsr        BEGSR
036400050609     c*
036500150713      * Chiave file statistica pesi VDL
036600150713     c     Kpes          Klist
036700150902     c                   kfld                    Tasksc
036800150713     c                   kfld                    Anno
036900150713     c                   kfld                    Mese
037000150902     c                   kfld                    Tasctr
037100150904
037200150914     c                   movel     *all'9'       sdtasped
037300150902      *
037400150902      * recupero tabella valori variabili da utilizzare nell'analisi dei dati della statistica PESI
037500150908     c     Ktbecod       setll     tntbe01l
037600150904     c     ktbecod       reade     tntbe01l
037700150904     c                   dow       not %eof(tntbe01l)
037800150904     c                   if        xx < 50
037900150904     c                   add       1             xx
038000150908     c                   eval      SDtasped(xx)= %dech(%subst(tbeKe1:1:8):8:0)
038100150908     c                   eval      SVpd(xx) = tbeuni
038200150904     c                   endif
038300150904     c     ktbecod       reade     tntbe01l
038400150904     c                   enddo
038500150929
038600150929     c                   movea     Sdtasped      Odtasped
038700150929
038800150929     c                   sorta     Odtasped
038900150928
039000150928      * Carico le filiali Estere per escludere clienti esteri dal calcolo peso desunto
039100150928     c                   clear                   kk
039200150928     c     *loval        setll     AZORG01L
039300150928     c                   read      AZORG01L
039400150928    1c                   DOW       not %eof(azorg01l)
039500150928
039600150928      * Filiale o Agenzia
039700150928    2c                   If        (orgfag = 'F' or orgfag = 'A') and
039800150928      * Annullato
039900150928    3c                             orgfva =  ' '
040000150928
040100150928    4c                   movel     ORGde3        OG143
040200150928      * Filiale estero
040300150928    4c                   If        §OGNtw = 'EEX' OR  §OGNtw = 'EUP' OR
040400150928     c                             §OGNtw = 'DPD' OR  §OGNtw = 'FED'
040500150928     c                   Add       1             KK
040600150928     c                   Movel     ORGFil        FilEst(KK)
040700150928    4c                   Endif
040800150928
040900150928    3c                   Endif
041000150928      *
041100150928     c                   Read      Azorg01l
041200150928    1c                   Enddo
041300060915
041400050609     c                   endsr
