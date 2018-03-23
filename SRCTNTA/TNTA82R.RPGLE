000100050103     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050103
000300050103      *------------------------------------------------------------------------*
000400050103      *                                                                        *
000500050103      *                 ABILITAZIONE CLIENTI               ?                   *
000600050103      *                                                                        *
000700050103      *------------------------------------------------------------------------*
000800050103
000900101007     fTiabl01l  if   e           k disk
001000050209     fTivss02l  uf   e           k disk
001100050110     fTikun00f  o    e             disk
001200050104     fTnta82D   cf   e             workstn sfile(ta8201s:nrr)
001300050103
001400050103      *------------------------------------------------------------------------*
001500050103      *  RIEPILOGO INDICATORI
001600050103      *------------------------------------------------------------------------*
001700050103      * 01 - Pgm richiamato
001800050103      * 02 - Utente non abilitato - esce dal pgm
001900050104      * 03 - Codice cliente inserito è papà
002000050111      * 04 - Codice abilitato proteggo i campi nominativo e e-mail
002100050111      * 05 - Codice con abilitazioni scaduta
002200050104      * 06 - Proteggo figlio nel subfile caricato in automatico
002300050105      * 07 - Proteggo il codice papà
002400050110      * 08 - Aggiunto codice figlio nel subfile
002500050309      * 09 - Cliente da abilitare nuovo
002600101019      * 10 - F8 = Reinvio password abilitato
002700050103      * 20 - ON   SFLDSP
002800050103      * 21 - ON   SFLDSPCTL
002900050103      * 21 - OFF  SFLCLR
003000050103      * 28 - ERRORE GENERICO DSPF
003100050111      * 30 - Lettura SFL
003200050111      * 31 - Comodo
003300050308      * 32 - Comodo
003400050103      * 40 - Errore codice cliente
003500050104      * 41 - Errore responsabile
003600050104      * 42 - Errore e-mail
003700050104      * 43 - Errore cliente nel subfile
003800050105      * 44 - Errore papà
003900050111      * 45 - Posizionamento cursore codice cliente
004000050112      * 90 - Riemissione videata
004100050103      *------------------------------------------------------------------------*
004200050103
004300050103      *   V A R I A B I L I
004400100408     d API             s              1    inz('''')
004500050110     d dataiso         s               d   datfmt(*iso)
004600050104     d divis           s              3  0
004700050104     d nrr             s              4  0
004800050104     d resto           s              3  0
004900050308     d savnrr          s              4  0
005000050104     d xx              s              3  0
005100050110     d wabi            s                   like(UteAut)
005200050114     d was888          s              1    inz('0')
005300050110     d wdataf          s              8  0
005400050110     d wdatai          s              8  0
005500050112     d wksu            s                   like(v1cksu) inz
005600050104     d wnrr            s                   like(nrr)
005700100408     d wpos            s              3  0
005800050112     d wtibs69         s              1    inz('0')
005900050110     d w003a           s              3
006000050308     d w007a           s              7
006100050104     d w0070           s              7  0
006200050104     d w008a           s              8
006300050111     d w0110           s             11  0
006400101006     d $Fine           s               n   inz(*off)
006500120126     d w_Conta         s              3  0
006600120126     d w_VSSsun        s                   like(VSSsun)
006700050103
006800050103      *   S C H I E R E
006900050104     d ksa             s              4    Dim(30)
007000050104     d ksc             s              7  0 Dim(30)
007100050110     d msg             s             78    Dim(20) ctdata perrcd(1)
007200050104     d skfig           s             11  0 Dim(500) inz(*Zeros)
007300050103     d skpog           s              3    Dim(250) inz(*Zeros)
007400050114     d sksfl           s              8  0 Dim(501) inz(*Zeros)
007500050103
007600050103      *   D S   I N T E R N E / E S T E R N E
007700050104     d                 ds
007800050104     d dsksn                   1      4p 0
007900050104     d dsksa                   1      4
008000050104
008100050103     d Param           ds
008200050103     d  parflg                 1      1
008300050103     d  parcli                 2      8
008400050103
008500050103     d                sds
008600050103     d  Vtcpgm                 1     10
008700130412     d  SDSuser              254    263
008800050103
008900050103     d Azuteds       e ds                  Extname(Azute00f)
009000050103     d dDatiute      e ds
009100050103     d dLat          e ds
009200050103     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
009300050103     d ds_cnind      e ds                  inz  extname(Cnind00f)
009400050103     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
009500050103     d ds_fncls      e ds                  inz  extname(Fncls00f)
009600050103     d dsemail       e ds
009700050110     d dute01        e ds
009800050103     d Kpjba         e ds
009900050103     d Tibs02ds      e ds
010000050104     d Tibs10ds      e ds
010100050104     d  skfigli               21   5520  0 dim(500)
010200050103     d Tibs34ds      e ds
010300050103     d Tibs69ds      e ds
010400050111     d Tis730ds      e ds
010500050103     d Trul31ds      e ds
010600050104     d Xclirds       e ds
010700101006
010800101006      //---------------------------------------------------------------
010900101006      //?Definizione procedure usate.
011000101006      //---------------------------------------------------------------
011100101006     d tis730r         pr                  extpgm('TIS730R')
011200101006     d  kpjba                              likeds(kpjba)
011300101006     d  tis730ds                           likeds(tis730ds)
011400101006
011500101006      //---------------------------------------------------------------
011600050103      *   C O S T A N T I
011700050103      * titolo videata (lunghezza massima 36)
011800050103     d VtcTit          C                   CONST('ABILITAZIONE CLIENTI SERVIZI -
011900050103     d                                     ON LINE')
012000050103
012100050103      *------------------------------------------------------------------------*
012200050103
012300050103      * Se pgm richiamato salto la prima videata
012400050103     c   01              ExSr      Sr_Car02d
012500050103     c   01              Goto      Inz02
012600050111
012700050111      * Gestione da Menù
012800050111     c     Inz01         Tag
012900050103
013000050308     c  n28
013100050308     cann90              Clear                   vidksu
013200050308     c  n90              Clear                   vddksu
013300050316     c                   Eval      *In07 = *Off
013400050103
013500050103     c                   Exfmt     Ta8201d
013600050104     c                   Eval      *In28 = *Off
013700050104     c                   Clear                   vidmsg
013800050103      * F3=Fine
013900050103     c                   If        *InKc or
014000050103      * 02=utente non abilitato
014100050103     c                             *In02
014200050103     c                   Goto      Fine
014300050103     c                   EndIf
014400050103
014500050103      * Controllo
014600050103     c                   ExSr      Sr_Ctr01d
014700050111     c   28
014800050111     cor 90              Goto      Inz01
014900050111     c                   Eval      wksu = vidksu
015000050104      * Ulteriori controlli sul codice inserito
015100050104      * se papà o figlio se abilitato lui o il suo papà
015200050111     c     Carsfl        Tag
015300050104     c                   ExSr      Sr_Ctrksu
015400050104      * Pulisco subfile
015500050103     c                   ExSr      Sr_Pulsfl
015600050103      * Carico il subfile
015700050103     c                   ExSr      Sr_Carsfl
015800050104
015900050104      * Se cliente già abilitato o scaduto emetto msg
016000050309     c                   If        *In04 and Not *In05 and Not *In90
016100050309     c                   Eval      *In28 = *On
016200050309     c                   Eval      vidmsg = msg(04)
016300050111     c   07              Eval      *In44 = *On
016400050111     c  n07              Eval      *In45 = *On
016500050104     c                   EndIf
016600050104     c                   If        *In05
016700050104     c                   Eval      *In28 = *On
016800050104     c                   Eval      vidmsg = msg(05)
016900050111     c   07              Eval      *In44 = *On
017000050111     c  n07              Eval      *In45 = *On
017100050104     c                   EndIf
017200050104
017300050111     c     Emisfl        Tag
017400050104      * Emetto il Subfile
017500050104     c                   ExSr      Sr_Emisfl
017600050104      * F3=Fine
017700050104     c   kc              Goto      Fine
017800050104      * F12 - Torno alla videata precedente
017900050104     c                   If        *InKl
018000050104     c                   Eval      *In28 = *Off
018100050104     c                   Clear                   vidmsg
018200050104     c                   Goto      Inz01
018300050104     c                   EndIf
018400050104      * Controllo dati
018500050104     c                   ExSr      Sr_CtrSfl
018600050111     c   90              Goto      Carsfl
018700101006     c                   If        *in28 or (not *inkf and
018800101006     c                             not *inkh)
018900101006     c                   Goto      Emisfl
019000101006     c                   endif
019100101006      * Reinvio password
019200101006     c                   If        *inkh
019300101006     c                   ExSr      Sr_Password
019400101006     c   90              Goto      Emisfl
019500101006     c                   endif
019600050110      * Conferma
019700050110     c   kf              ExSr      Sr_Conferma
019800050111     c   28              Goto      Emisfl
019900050104
020000050111      * Gestione da Offerta
020100050103     c     Inz02         Tag
020200050111if  1c                   If        *In01
020300050111do  2c   90              Do
020400050316     c                   Eval      *In07 = *Off
020500050111     c                   ExSr      Sr_Ctrksu
020600050111      * Se cliente scaduto emetto msg
020700050111     c                   If        *In05
020800050111     c                   Eval      *In28 = *On
020900050111     c                   Eval      vidmsg = msg(05)
021000050111     c                   Eval      *In44 = *On
021100050111     c                   EndIf
021200050111e   2c                   EndDo
021300050111
021400050112     c                   Exfmt     Ta8202d
021500050103     c                   Eval      *In28 = *Off
021600050111
021700050111     c                   ExSr      Sr_Ctr02d
021800050111     c   28
021900050112     cor 90
022000050112     cornkf              Goto      Inz02
022100050111
022200050111     c   kf              ExSr      Sr_Conferma
022300050111     c   28              Goto      Inz02
022400050111
022500050111e   1c                   EndIf
022600050103
022700050103     c     Fine          Tag
022800050112
022900050112      * Chiude i file dei pgm richiamati
023000050112     c                   If        wtibs69 = *On
023100050112     c                   Eval      i69tla = 'C'
023200050112     c                   Call      'TIBS69R'
023300050112     c                   Parm                    Tibs69ds
023400050112     c                   EndIf
023500050103
023600050103     c                   Eval      *InLr = *On
023700050103
023800050103      *------------------------------------------------------------------------*
023900050111      * CONTROLLO LA VIDEATA PER GESTIONE DA MENU
024000050103      *------------------------------------------------------------------------*
024100050103     c     Sr_Ctr01d     BegSr
024200050104
024300050104     c                   Clear                   vddksu
024400050111     c                   Eval      *In90 = *Off
024500050309     c                   Eval      *In09 = *Off
024600050103
024700050104      * Codice obbligatorio
024800050104if  1c                   If        vidksu = *Zeros or vidksu = *Blanks
024900050103     c                   Eval      *In28 = *On
025000050103     c                   Eval      *In40 = *On
025100050104     c                   Eval      vidmsg = msg(02)
025200050103     c                   Leavesr
025300050103e   1c                   EndIf
025400050104
025500050104if  1c                   If        vidksu > '09999999'
025600050104     c                   Eval      *In28 = *On
025700050104     c                   Eval      *In40 = *On
025800050104     c                   Eval      vidmsg = msg(03)
025900050104     c                   Leavesr
026000050104e   1c                   EndIf
026100050104
026200050104      * Ricerca
026300050310      * forzo autorizzazione AZ in modo da visualizzare tutti i clienti
026400050111     c     '?'           Scan      vidksu                                 31
026500050111if  1c                   If        *In31
026600050104     c                   Clear                   Xclirds
026700050104     c                   Eval      DxcCap = DutKci
026800050310     c                   Eval      DxcAut = 'AZ'
026900050104     c                   Call      'XCLIR'
027000050104     c                   Parm                    Xclirds
027100050104     c                   Movea     DxcKsc        ksa
027200050104     c                   move      ksa(1)        dsksa
027300050104     c                   z-add     dsksn         ksc(1)
027400050104     c                   Move      *all'0'       vidksu
027500050104     c                   Move      ksc(1)        vidksu
027600050111     c                   Eval      *In90 = *On
027700050104e   1c                   EndIf
027800050104
027900050104      * Solo codici numerici
028000050104if  1c                   If        vidksu < *Zeros
028100050104     c                   Eval      *In28 = *On
028200050104     c                   Eval      *In40 = *On
028300050104     c                   Eval      vidmsg = msg(03)
028400050104     c                   Leavesr
028500050104e   1c                   EndIf
028600050103
028700050104      * Controllo esistenza codice
028800050104     c                   Move      vidksu        w0070
028900050104     c                   ExSr      Sr_ChkCli
029000050104if  1c                   If        O69err = *On
029100050104     c                   Eval      *In28 = *On
029200050104     c                   Eval      *In40 = *On
029300050104     c                   Eval      vidmsg = msg(03)
029400050104     c                   Leavesr
029500050104e   1c                   EndIf
029600050104     c                   Movel     AcoRag        vddksu
029700050110
029800050110      * Codice gestibile dall'utente
029900050308     c**!!!              Eval      w003a = %subst(vidksu:2:3)
030000050308     c**!!!w003a         Lookup    skpog                                  31
030100050308if  1c**!!!              If        Not *In31
030200050308     c**!!!              Eval      *In28 = *On
030300050308     c**!!!              Eval      *In40 = *On
030400050308     c**!!!              Eval      vidmsg = msg(13)
030500050308     c**!!!              Leavesr
030600050308e   1c**!!!              EndIf
030700050103
030800050103     c                   EndSr
030900050104
031000050104      *------------------------------------------------------------------------*
031100050104      * CONTROLLO IL CODICE INSERITO ABILITAZIONI PAPA/FIGLIO
031200050104      *------------------------------------------------------------------------*
031300050104     c     Sr_CtrKsu     BegSr
031400050104
031500050104     c                   Eval      *In03 = *Off
031600050104     c                   Eval      *In04 = *Off
031700050104     c                   Eval      *In05 = *Off
031800050111
031900050111      * se gestione da menù
032000050111if  0c                   If        Not *In01
032100050111     c  n90              Clear                   v1ccli
032200050111     c  n90              Clear                   v1dcli
032300050111     c  n90              Clear                   v1cksu
032400050111     c  n90              Clear                   v1dksu
032500050104     c                   Clear                   v1crsp
032600050104     c                   Clear                   v1cem1
032700050104     c                   Clear                   v1cem2
032800050104     c                   Clear                   skfig
032900050104
033000050104      * controllo se il codice inserito è un papà
033100050104     c                   Clear                   Tibs10ds
033200050104     c                   Eval      d10tle = 'WW'
033300050104     c                   Eval      d10paf = 'F'
033400050111     c                   move      wksu          d10cod
033500050104     c                   Eval      d10drf = *date
033600050104     c                   Call      'TIBS10R'
033700050104     c                   Parm                    Tibs10ds
033800050104if  1c                   If        d10err = *Blanks
033900050111     c  n90              Eval      *In03 = *On
034000050105     c                   Eval      *In07 = *On
034100050104     c                   Move      *all'0'       v1cksu
034200050104     c                   Move      d10cop        v1cksu
034300050104     c                   Movea     skfigli       skfig
034400050104   x1c                   Else
034500050104      * controllo se il codice inserito è un figlio
034600050104     c                   Clear                   Tibs10ds
034700050104     c                   Eval      d10tle = 'WW'
034800050104     c                   Eval      d10paf = 'P'
034900050111     c                   Move      wksu          d10cod
035000050104     c                   Eval      d10drf = *date
035100050104     c                   Call      'TIBS10R'
035200050104     c                   Parm                    Tibs10ds
035300050104if  2c                   If        d10err = *Blanks
035400050105     c                   Eval      *In07 = *On
035500050104     c                   Move      *all'0'       v1cksu
035600050104     c                   Move      d10cop        v1cksu
035700050104     c                   Move      vidksu        v1ccli
035800050104     c                   Movea     skfigli       skfig
035900050104   x2c                   Else
036000050110      * non è papà e non è figlio
036100050105     c                   Move      vidksu        v1ccli
036200050104e   2c                   EndIf
036300050104e   1c                   EndIf
036400050111      * se gestione da offerta con papà inserito
036500050111   x0c                   Else
036600050111     c                   Eval      *In07 = *On
036700050111e   0c                   EndIf
036800050111
036900050104      * decodifico il figlio e il papà
037000050105if  1c                   If        v1ccli > *Zeros
037100050104     c                   Move      v1ccli        w0070
037200050104     c                   ExSr      Sr_ChkCli
037300050105if  2c                   If        O69err <> *On
037400050104     c                   Movel     AcoRag        v1dcli
037500050105e   2c                   EndIf
037600050105e   1c                   EndIf
037700050105if  1c                   If        v1cksu > *Zeros
037800050104     c                   Move      v1cksu        w0070
037900050104     c                   ExSr      Sr_ChkCli
038000050105if  2c                   If        O69err <> *On
038100050104     c                   Movel     AcoRag        v1dksu
038200050105e   2c                   EndIf
038300050105e   1c                   EndIf
038400050104
038500050104      * Controllo se il papà è già abilitato ai servizi ON LINE
038600050111      * oppure se il codice inserito è già abilitato
038700050104     c                   Move      *all'0'       vssksu
038800050111     c   07              Move      v1cksu        vssksu
038900050111     c  n07              Move      v1ccli        vssksu
039000050104     c                   Eval      vssisv = 'TT'
039100050110     c     kTivss        Chain(n)  Tivss02l
039200050104if  1c                   If        %Found(Tivss02l)
039300050104     c                   Eval      *In04 = *On
039400050104      * cerco il nominativo e l'e-mail
039500050104     c                   Eval      abltip = 'SG1'
039600050104     c     kTiabl        Chain     Tiabl01l
039700050104if  2c                   If        %Found(Tiabl01l)
039800050104     c                   Eval      v1crsp = AblRsp
039900050104     c                   Eval      v1cem1 = %subst(Ableml:1:49)
040000050104     c                   Eval      v1cem2 = %subst(Ableml:50:49)
040100050104e   2c                   EndIf
040200050104      * controllo se abilitazione ancora valida
040300050104     c                   Eval      *In05 =(*date > vssdsc)
040400050309      * se non è papà ma è già abilitato deve diventare papà
040500050309if  2c                   If        Not *In07
040600050309     c                   Eval      *In03 = *On
040700050309     c                   Eval      *In07 = *On
040800050309     c                   Move      v1ccli        v1cksu
040900050309     c                   Move      v1dcli        v1dksu
041000050309     c                   Clear                   v1ccli
041100050309     c                   Clear                   v1dcli
041200050309e   2c                   EndIf
041300050309   x1c                   Else
041400050309      * codice non abilitato e non papà
041500050309     c                   Eval      *In09 = *On
041600050104e   1c                   EndIf
041700050104
041800050104     c                   EndSr
041900050103
042000050103      *------------------------------------------------------------------------*
042100050103      * PULISCO IL SUBFILE
042200050103      *------------------------------------------------------------------------*
042300050103     c     Sr_Pulsfl     BegSr
042400050103
042500050103     c                   Clear                   nrr
042600050308     c                   Clear                   savnrr
042700050110     c                   Eval      *In20 = *Off
042800050110     c                   Eval      *In21 = *Off
042900050103     c                   Write     ta8201c
043000050103     c                   Eval      *In20 = *On
043100050103     c                   Eval      *In21 = *On
043200050103
043300050103     c                   EndSr
043400050104
043500050103      *------------------------------------------------------------------------*
043600050103      * CARICO IL SUBFILE
043700050103      *------------------------------------------------------------------------*
043800050103     c     Sr_Carsfl     BegSr
043900050103
044000050316      * se il codice immesso non è abilitato ed ho inserito un padre
044100050316      * carico il codice immesso nel subfile dei figli
044200050316     c                   If        *In09 and v1cksu <> *Blanks and
044300050316     c                             v1cksu <> *Zeros
044400101230     c                             and not *in07
044500050316     c                   Add       1             Nrr
044600050316     c     Nrr           Chain     ta8201s                            30
044700050316     c                   Move      v1ccli        v1scod
044800050316     c                   Move      v1scod        w0070
044900050316     c                   ExSr      Sr_ChkCli
045000050316     c                   Movel     AcoRag        v1sdes
045100050316     c  n30              Update    ta8201s
045200050316     c   30              Write     ta8201s
045300050316     c                   EndIf
045400050104      * Carico i figli
045500050309do  1c     2             Do        500           xx
045600050104     c                   Eval      *In06 = *Off
045700050104     c                   Clear                   wprot
045800050104     c                   Clear                   v1scod
045900050104     c                   Clear                   v1sdes
046000050104if  2c                   If        skfig(xx) = *Zeros
046100050104     c                   Leave
046200050104e   2c                   EndIf
046300050110     c                   Add       1             Nrr
046400050110     c     Nrr           Chain     ta8201s                            30
046500050104     c                   Eval      *In06 = *On
046600050104     c                   Eval      wprot = 'S'
046700050104     c                   Move      skfig(xx)     v1scod
046800050104     c                   Move      v1scod        w0070
046900050104     c                   ExSr      Sr_ChkCli
047000050104     c                   Movel     AcoRag        v1sdes
047100050110     c  n30              Update    ta8201s
047200050110     c   30              Write     ta8201s
047300050104e   1c                   EndDo
047400050104
047500050104     c                   EndSr
047600050104
047700050104      *------------------------------------------------------------------------*
047800050104      * EMETTO IL SUBFILE
047900050104      *------------------------------------------------------------------------*
048000050104     c     Sr_Emisfl     BegSr
048100101019
048200101019      * Controllo se posso abilitare il tasto F8=Reinvio Password
048300101019      * da abilitazioni sul profilo
048400101019     c                   eval      *in10 = *off
048500101019     c                   IF        §uteinvpwd = 'S' and *in07
048600101019      * e se codice padre gestibile dall'utente
048700101019     c                   Eval      w003a = %subst(v1cksu:2:3)
048800101019     c     w003a         Lookup    skpog                                  31
048900101019if  3c                   If        *In31
049000101019     c                   eval      *in10 = *on
049100101019     c                   endif
049200101019     c                   ENDIF
049300050104
049400050104     c                   Eval      rec = 1
049500050308     c                   If        savnrr > 0
049600050308     c                   Eval      rec = savnrr
049700050308     c                   EndIf
049800050104
049900050104     c                   Write     ta8201z
050000050104     c                   Exfmt     ta8201c
050100050104
050200050104     c                   Eval      *In28 = *Off
050300050104     c                   Clear                   vidmsg
050400050104
050500050104     c                   EndSr
050600050104
050700050104      *------------------------------------------------------------------------*
050800050104      * CONTROLLO I DATI DEL SUBFILE
050900050104      *------------------------------------------------------------------------*
051000150204     c     Sr_CtrSfl     BegSr
051100050104
051200050110     c                   Eval      *In08 = *Off
051300050104     c                   Eval      *In41 = *Off
051400050104     c                   Eval      *In42 = *Off
051500050105     c                   Eval      *In44 = *Off
051600050111     c                   Eval      *In45 = *Off
051700050111     c                   Eval      *In90 = *Off
051800050104     c                   Clear                   nrr
051900050114     c                   Clear                   sksfl
052000050309
052100050309      * Emetto subito il messaggio bloccante se cliente con abilitazione
052200050309      * scaduta
052300050309     c                   If        *In05
052400050309     c                   Eval      *In28 = *On
052500050309     c   07              Eval      *In44 = *On
052600050309     c  n07              Eval      *In45 = *On
052700050309     c                   Eval      vidmsg = msg(05)
052800050309     c                   Leavesr
052900050309     c                   EndIf
053000050104
053100050104      * Dati del control
053200050105
053300050105      * Codice papà
053400050111if  1c                   If        Not *In07 and v1cksu <> *Zeros and
053500050111     c                             v1cksu <> *Blanks
053600050111if  2c                   If        v1cksu > '09999999'
053700050105     c                   Eval      *In28 = *On
053800050105     c                   Eval      *In44 = *On
053900050105     c                   Eval      vidmsg = msg(03)
054000050105     c                   Leavesr
054100050105e   2c                   EndIf
054200050111      * Ricerca
054300050310      * forzo autorizzazione AZ in modo da visualizzare tutti i clienti
054400050111     c     '?'           Scan      v1cksu                                 31
054500050111if  2c                   If        *In31
054600050111     c                   Clear                   Xclirds
054700050111     c                   Eval      DxcCap = DutKci
054800050310     c                   Eval      DxcAut = 'AZ'
054900050111     c                   Call      'XCLIR'
055000050111     c                   Parm                    Xclirds
055100050111     c                   Movea     DxcKsc        ksa
055200050111     c                   move      ksa(1)        dsksa
055300050111     c                   z-add     dsksn         ksc(1)
055400050111     c                   Move      *all'0'       v1cksu
055500050111     c                   Move      ksc(1)        v1cksu
055600050111     c                   Eval      *In44 = *On
055700050111e   2c                   EndIf
055800050111      * Solo codici numerici
055900050111if  2c                   If        v1cksu < *Zeros
056000050111     c                   Eval      *In28 = *On
056100050111     c                   Eval      *In44 = *On
056200050111     c                   Eval      vidmsg = msg(03)
056300050111     c                   Leavesr
056400050111e   2c                   EndIf
056500050105      * Controllo esistenza codice
056600050111if  2c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
056700050105     c                   Move      v1cksu        w0070
056800050105     c                   ExSr      Sr_ChkCli
056900050111if  3c                   If        O69err = *On
057000050105     c                   Eval      *In28 = *On
057100050105     c                   Eval      *In44 = *On
057200050105     c                   Eval      vidmsg = msg(03)
057300050105     c                   Leavesr
057400050111e   3c                   EndIf
057500050105     c                   Movel     AcoRag        v1dksu
057600050308      * Se il figlio è gestibile dall'utente posso immettere qualsiasi papà
057700050308      * altrimenti posso immettere solo papà gestiti dall'utente
057800050308     c                   Move      v1ccli        w007a
057900050308     c                   Eval      w003a = %subst(w007a:1:3)
058000050308     c     w003a         Lookup    skpog                                  31
058100050308if 2ac                   If        Not *In31
058200050110      * Codice gestibile dall'utente
058300050111     c                   Eval      w003a = %subst(v1cksu:2:3)
058400050110     c     w003a         Lookup    skpog                                  31
058500050111if  3c                   If        Not *In31
058600050110     c                   Eval      *In28 = *On
058700050110     c                   Eval      *In44 = *On
058800050110     c                   Eval      vidmsg = msg(13)
058900050110     c                   Leavesr
059000050111e   3c                   EndIf
059100050308e  2ac                   EndIf
059200050105      * non deve essere figlio di altro papà
059300050105     c                   Clear                   Tibs10ds
059400050105     c                   Eval      d10tle = 'WW'
059500050105     c                   Eval      d10paf = 'P'
059600050105     c                   Move      v1cksu        d10cod
059700050105     c                   Eval      d10drf = *date
059800050105     c                   Call      'TIBS10R'
059900050105     c                   Parm                    Tibs10ds
060000050111if  3c                   If        d10err = *Blanks
060100050105     c                   Eval      *In28 = *On
060200050105     c                   Eval      *In44 = *On
060300050105     c                   Eval      vidmsg = msg(09)
060400050105     c                   Move      d10cop        w008a
060500050105     c                   Eval      %subst(vidmsg:36:8) = w008a
060600050105     c                   Leavesr
060700050111e   3c                   EndIf
060800050105      * deve essere già abilitato ai servizi ON LINE
060900050105     c                   Move      *all'0'       vssksu
061000050105     c                   Move      v1cksu        vssksu
061100050105     c                   Eval      vssisv = 'TT'
061200050110     c     kTivss        Chain(n)  Tivss02l
061300050111if  3c                   If        Not %Found(Tivss02l)
061400050105     c                   Eval      *In28 = *On
061500050105     c                   Eval      *In44 = *On
061600050105     c                   Eval      vidmsg = msg(11)
061700050105     c                   Leavesr
061800050111e   3c                   EndIf
061900050111     c                   Eval      wksu = v1cksu
062000050111     c                   Eval      *In90 = *On
062100050316     c                   Eval      *In07 = *On
062200050111     c                   Leavesr
062300050111e   2c                   EndIf
062400050308   x1c                   Else
062500050309      * se papà non inserito e voglio abilitare il codice questo deve essere
062600050309      * gestibile dall'utente
062700050309if  2c                   If        Not *In07
062800050309     c                   Clear                   v1cksu
062900050309     c                   Clear                   v1dksu
063000050309     c                   Move      v1ccli        w007a
063100050309     c                   Eval      w003a = %subst(w007a:1:3)
063200050309     c     w003a         Lookup    skpog                                  31
063300050309if  3c                   If        Not *In31
063400050309     c                   Eval      *In28 = *On
063500050309     c                   Eval      *In45 = *On
063600050309     c                   Eval      vidmsg = msg(13)
063700050309     c                   Leavesr
063800050309e   3c                   EndIf
063900050309e   2c                   EndIf
064000050105e   1c                   EndIf
064100050105      * Responsabile/E-mail
064200050104if  1c                   If        Not *In04
064300050105      * Responsabile
064400050104if  2c                   If        v1crsp = *Blanks
064500050104     c                   Eval      *In28 = *On
064600050104     c                   Eval      *In41 = *On
064700050104     c                   Eval      vidmsg = msg(06)
064800050104     c                   Leavesr
064900050104e   2c                   EndIf
065000050105      * E-mail
065100050104if  2c                   If        v1cem1 = *Blanks and v1cem2 = *Blanks
065200050104     c                   Eval      *In28 = *On
065300050104     c                   Eval      *In42 = *On
065400050104     c                   Eval      vidmsg = msg(07)
065500050104     c                   Leavesr
065600050104e   2c                   EndIf
065700050104     c                   Clear                   dsemail
065800050104     c                   Eval      §emlindi = v1cem1 + v1cem2
065900050104     c                   Call      'XEMAIL'
066000050104     c                   Parm                    dsemail
066100050104if  2c                   If        §emlerro = '1'
066200050104     c                   Eval      *In28 = *On
066300050104     c                   Eval      *In42 = *On
066400050104     c                   Eval      vidmsg = msg(08)
066500050104     c                   Leavesr
066600050104e   2c                   EndIf
066700050104     c                   Eval      v1cem1 = %subst(§emlindo:1:49)
066800050104     c                   Eval      v1cem2 = %subst(§emlindo:50:49)
066900050104e   1c                   EndIf
067000050104
067100050104      * Dati del subfile
067200050104do  1c                   Do        *Hival
067300050104     c                   Eval      nrr = nrr +1
067400050104     c     nrr           Chain     ta8201s                            30
067500050104     c                   If        *In30
067600050104     c                   Leave
067700050104     c                   EndIf
067800050104
067900050111     c                   Eval      *In06 = *Off
068000050104     c                   Eval      *In28 = *Off
068100050104     c                   Eval      *In43 = *Off
068200050104
068300050104      * se non è un codice protetto
068400050104if  2c                   If        wprot = *Blanks
068500050104     c                   Clear                   v1sdes
068600050104      * Controllo il cliente inserito
068700050104if  3c                   If        v1scod > *Zeros
068800091029      * se uguale al codice che si deve abilitare e non c'è il codice padre
068900091029      * devo dare errore, altrimenti il pgm scrive un legame con il codice
069000091029      * padre a 0
069100091029     c                   if        v1scod = v1ccli and
069200091029     c                             (v1cksu = *zeros or v1cksu = *blanks)
069300091029     c                   Eval      *In28 = *On
069400091029     c                   Eval      *In43 = *On
069500091029     c                   Eval      vidmsg = msg(16)
069600091029     c                   Z-add     nrr           savnrr
069700091029     c                   Update    ta8201s
069800091029     c                   Leavesr
069900091029     c                   endif
070000140908      * solo 7 cifre
070100140908if  1c                   If        v1scod > 9999999
070200140908     c                   Eval      *In28 = *On
070300140908     c                   Eval      *In43 = *On
070400140908     c                   Eval      vidmsg = msg(03)
070500140908     c                   Z-add     nrr           savnrr
070600140908     c                   Update    ta8201s
070700140908     c                   Leavesr
070800140908e   1c                   EndIf
070900091029      * controllo in anagrafica
071000050104     c                   Move      v1scod        w0070
071100050104     c                   ExSr      Sr_ChkCli
071200050104if  4c                   If        O69err = *On
071300050104     c                   Eval      *In28 = *On
071400050104     c                   Eval      *In43 = *On
071500050104     c                   Eval      vidmsg = msg(03)
071600050308     c                   Z-add     nrr           savnrr
071700050104     c                   Update    ta8201s
071800050114     c                   Leavesr
071900050104e   4c                   EndIf
072000050104     c                   Movel     AcoRag        v1sdes
072100050308
072200050308      * Se il papà è gestibile dall'utente posso immettere qualsiasi figlio
072300050308      * altrimenti posso immettere solo figli gestiti dall'utente
072400050308     c                   Eval      w003a = %subst(v1cksu:2:3)
072500050308     c     w003a         Lookup    skpog                                  31
072600050308if 3ac                   If        Not *In31
072700050110      * Codice gestibile dall'utente
072800050111     c                   Move      v1scod        w008a
072900050111     c                   Eval      w003a = %subst(w008a:2:3)
073000050110     c     w003a         Lookup    skpog                                  31
073100050110if  4c                   If        Not *In31
073200050110     c                   Eval      *In28 = *On
073300050110     c                   Eval      *In43 = *On
073400050110     c                   Eval      vidmsg = msg(13)
073500050308     c                   Z-add     nrr           savnrr
073600050110     c                   Update    ta8201s
073700050114     c                   Leavesr
073800050110e   4c                   EndIf
073900050308e  3ac                   EndIf
074000050110      * non deve essere già caricato nella sk dei figli
074100050110     c                   Z-add     v1scod        w0110
074200050110     c     w0110         Lookup    skfig                                  31
074300050110if  4c                   If        *In31
074400050110     c                   Eval      *In28 = *On
074500050110     c                   Eval      *In43 = *On
074600050110     c                   Eval      vidmsg = msg(12)
074700050308     c                   Z-add     nrr           savnrr
074800050110     c                   Update    ta8201s
074900050114     c                   Leavesr
075000050110e   4c                   EndIf
075100050114      * non deve essere già stato inserito prima nel sfl
075200050114     c     v1scod        Lookup    sksfl                                  31
075300050114if  4c                   If        *In31
075400050114     c                   Eval      *In28 = *On
075500050114     c                   Eval      *In43 = *On
075600050114     c                   Eval      vidmsg = msg(12)
075700050308     c                   Z-add     nrr           savnrr
075800050114     c                   Update    ta8201s
075900050114     c                   Leavesr
076000050114e   4c                   EndIf
076100050104      * non deve essere già figlio di un altro papà
076200050104     c                   Clear                   Tibs10ds
076300050104     c                   Eval      d10tle = 'WW'
076400050104     c                   Eval      d10paf = 'P'
076500050104     c                   Move      v1scod        d10cod
076600050104     c                   Eval      d10drf = *date
076700050104     c                   Call      'TIBS10R'
076800050104     c                   Parm                    Tibs10ds
076900050104if  4c                   If        d10err = *Blanks
077000050104     c                   Eval      *In28 = *On
077100050104     c                   Eval      *In43 = *On
077200050104     c                   Eval      vidmsg = msg(09)
077300050104     c                   Move      d10cop        w008a
077400050104     c                   Eval      %subst(vidmsg:36:8) = w008a
077500050308     c                   Z-add     nrr           savnrr
077600050104     c                   Update    ta8201s
077700050114     c                   Leavesr
077800050104e   4c                   EndIf
077900050104      * non deve essere già papà
078000050104     c                   Clear                   Tibs10ds
078100050104     c                   Eval      d10tle = 'WW'
078200050104     c                   Eval      d10paf = 'F'
078300050104     c                   move      v1scod        d10cod
078400050104     c                   Eval      d10drf = *date
078500050104     c                   Call      'TIBS10R'
078600050104     c                   Parm                    Tibs10ds
078700050104if  4c                   If        d10err = *Blanks
078800050104     c                   Eval      *In28 = *On
078900050104     c                   Eval      *In43 = *On
079000050104     c                   Eval      vidmsg = msg(10)
079100050308     c                   Z-add     nrr           savnrr
079200050104     c                   Update    ta8201s
079300050114     c                   Leavesr
079400050104e   4c                   EndIf
079500150116
079600150204     C* il controllo successsivo da bloccante diventa warning
079700150204     C* perché devo poter dire che al cliente
079800150204     C* padre è associato anche un altro cliente anche se già abilitato
079900150204     C* ma di questo avverto l'utente
080000150204     C* quindi il test lo faccio se:
080100150204     C* - il cliente non è già stato controllato
080200150204     C                   IF        V1SCod <> w1SCod
080300150204     c                   Move      *all'0'       vssksu
080400150204     c                   Move      v1scod        vssksu
080500150204     c                   EVAL      w1scod = v1scod
080600150204     c                   Eval      vssisv = 'TT'
080700150204     c     kTivss        Chain(n)  Tivss02l
080800150204if  4c                   If        %Found(Tivss02l)
080900150204     c                   Eval      *In28 = *On
081000150204     c                   Eval      *In43 = *On
081100150204     c                   Eval      vidmsg = msg(19)
081200150204     c                   Z-add     nrr           savnrr
081300150204     c                   Update    ta8201s
081400150204     c                   Leavesr
081500150204e   4c                   EndIf
081600150204e   4C                   EndIf
081700050114      * carico il nuovo codice in una sk di appoggio x controllare che non
081800050114      * siano immessi dei codici doppi
081900050114     c                   Eval      xx = 1
082000050114     c     *Zeros        Lookup    sksfl(xx)                              31
082100050114if  4c                   If        *In31
082200050114     c                   Eval      sksfl(xx) = v1scod
082300050114e   4c                   EndIf
082400050110     c                   Eval      *In08 = *On
082500050104e   3c                   EndIf
082600050104e   2c                   EndIf
082700050110      * se era protetto lo proteggo di nuovo
082800050110if  2c                   If        wprot = 'S'
082900050110     c                   Eval      *In06 = *On
083000050110e   2c                   EndIf
083100050104
083200050308     c                   Z-add     nrr           rec
083300050104     c                   Update    ta8201s
083400050104
083500050104e   1c                   EndDo
083600050111
083700050111      * Il papà è abilitato ma non sono stati aggiunti dei nuovi figli
083800050111      * errore
083900101006      * e non è stato dato F8=Reinvio Password
084000050309if  1c                   If        *In04 and Not *In08 and Not *In05
084100101006     c                             and not *inkh
084200050111     c                   Eval      *In28 = *On
084300050111     c                   Eval      *In44 = *On
084400050111     c                   Eval      vidmsg = msg(14)
084500050111     c                   Leavesr
084600050111e   1c                   EndIf
084700050104
084800101007      * Il papà è abilitato sono stati aggiunti dei nuovi figli
084900101007      * ma è stato dato F8=Reinvio Password
085000101007      * errore perchè prima si deve fare F6=Conferma
085100101007if  1c                   If        *In04 and *In08 and Not *In05
085200101007     c                             and *inkh
085300101007     c                   Eval      *In28 = *On
085400101007     c                   Eval      *In44 = *On
085500101007     c                   Eval      vidmsg = msg(17)
085600101007     c                   Leavesr
085700101007e   1c                   EndIf
085800101007
085900050104     c                   EndSr
086000050104
086100050104      *------------------------------------------------------------------------*
086200050104      * CONTROLLO IL CLIENTE
086300050104      *------------------------------------------------------------------------*
086400050104     c     Sr_Chkcli     BegSr
086500050104
086600050104     c                   Clear                   Tibs69ds
086700050104     c                   Move      w0070         I69kac
086800050104     c                   Call      'TIBS69R'
086900050112     c                   Parm                    Tibs69ds
087000050104     c                   Parm                    ds_cnaco
087100050104     c                   Parm                    ds_cnind
087200050104     c                   Parm                    ds_cnclp
087300050104     c                   Parm                    ds_fncls
087400050112     c                   Eval      wtibs69 = *On
087500050104
087600050104     c                   EndSr
087700041117
087800050103      *------------------------------------------------------------------------*
087900050111      * CARICA DATI NELLA VIDEATA PER GESTIONE DA OFFERTA
088000050103      *------------------------------------------------------------------------*
088100050103     c     Sr_Car02d     BegSr
088200050103
088300050111     c                   Move      parcli        v1ccli
088400050111     c                   Clear                   v1dcli
088500050111     c                   Clear                   v1crsp
088600050111     c                   Clear                   v1cem1
088700050111     c                   Clear                   v1cem2
088800050103     c                   Eval      V1cAbi = 'S'
088900050111     c                   Clear                   v1cksu
089000050104     c                   Clear                   v1dksu
089100050103
089200050111     c                   Move      v1ccli        w0070
089300050111     c                   ExSr      Sr_ChkCli
089400050104if  1c                   If        O69err <> *On
089500050111     c                   Movel     ACOrag        v1dcli
089600050103e   1c                   EndIf
089700050103
089800050103     c                   EndSr
089900050111
090000050111      *------------------------------------------------------------------------*
090100050111      * CONTROLLO LA VIDEATA PER GESTIONE DA OFFERTA
090200050111      *------------------------------------------------------------------------*
090300050111     c     Sr_Ctr02d     BegSr
090400050111
090500050111     c                   Eval      *In41 = *Off
090600050111     c                   Eval      *In42 = *Off
090700050112     c                   Eval      *In43 = *Off
090800050111     c                   Eval      *In44 = *Off
090900050111     c                   Eval      *In90 = *Off
091000050111
091100050111      * Se immesso che abilito il cliente
091200050111if  1c                   If        v1cabi = 'S'
091300050112      * Il codice non deve essere già figlio di un altro papà
091400050112     c                   Clear                   Tibs10ds
091500050112     c                   Eval      d10tle = 'WW'
091600050112     c                   Eval      d10paf = 'P'
091700050112     c                   Move      v1ccli        d10cod
091800050112     c                   Eval      d10drf = *date
091900050112     c                   Call      'TIBS10R'
092000050112     c                   Parm                    Tibs10ds
092100050112if  2c                   If        d10err = *Blanks
092200050112     c                   Eval      *In28 = *On
092300050112     c                   Eval      *In43 = *On
092400050112     c                   Eval      vidmsg = msg(09)
092500050112     c                   Move      d10cop        w008a
092600050112     c                   Eval      %subst(vidmsg:36:8) = w008a
092700050112     c                   Leavesr
092800050112e   2c                   EndIf
092900050111      * Codice Papà inserito
093000050111if  2c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
093100050111
093200050111if  3c                   If        vidksu > '09999999'
093300050111     c                   Eval      *In28 = *On
093400050111     c                   Eval      *In44 = *On
093500050111     c                   Eval      vidmsg = msg(03)
093600050111     c                   Leavesr
093700050111e   3c                   EndIf
093800050111      * Ricerca
093900050310      * forzo autorizzazione AZ in modo da visualizzare tutti i clienti
094000050111     c     '?'           Scan      v1cksu                                 31
094100050111if  3c                   If        *In31
094200050111     c                   Clear                   Xclirds
094300050111     c                   Eval      DxcCap = DutKci
094400050310     c                   Eval      DxcAut = 'AZ'
094500050111     c                   Call      'XCLIR'
094600050111     c                   Parm                    Xclirds
094700050111     c                   Movea     DxcKsc        ksa
094800050111     c                   move      ksa(1)        dsksa
094900050111     c                   z-add     dsksn         ksc(1)
095000050111     c                   Move      *all'0'       v1cksu
095100050111     c                   Move      ksc(1)        v1cksu
095200050111     c                   Eval      *In44 = *On
095300050111e   3c                   EndIf
095400050111      * Solo codici numerici
095500050111if  3c                   If        v1cksu < *Zeros
095600050111     c                   Eval      *In28 = *On
095700050111     c                   Eval      *In44 = *On
095800050111     c                   Eval      vidmsg = msg(03)
095900050111     c                   Leavesr
096000050111e   3c                   EndIf
096100050111      * Controllo esistenza codice
096200050111if  3c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
096300050111     c                   Move      v1cksu        w0070
096400050111     c                   ExSr      Sr_ChkCli
096500050111if  4c                   If        O69err = *On
096600050111     c                   Eval      *In28 = *On
096700050111     c                   Eval      *In44 = *On
096800050111     c                   Eval      vidmsg = msg(03)
096900050111     c                   Leavesr
097000050111e   4c                   EndIf
097100050111     c                   Movel     AcoRag        v1dksu
097200050111      * Codice gestibile dall'utente
097300050308     c**!!!              Eval      w003a = %subst(v1cksu:2:3)
097400050308     c**!!!w003a         Lookup    skpog                                  31
097500050308if  4c**!!!              If        Not *In31
097600050308     c**!!!              Eval      *In28 = *On
097700050308     c**!!!              Eval      *In44 = *On
097800050308     c**!!!              Eval      vidmsg = msg(13)
097900050308     c**!!!              Leavesr
098000050308e   4c**!!!              EndIf
098100050111      * non deve essere figlio di altro papà
098200050111     c                   Clear                   Tibs10ds
098300050111     c                   Eval      d10tle = 'WW'
098400050111     c                   Eval      d10paf = 'P'
098500050111     c                   Move      v1cksu        d10cod
098600050111     c                   Eval      d10drf = *date
098700050111     c                   Call      'TIBS10R'
098800050111     c                   Parm                    Tibs10ds
098900050111if  4c                   If        d10err = *Blanks
099000050111     c                   Eval      *In28 = *On
099100050111     c                   Eval      *In44 = *On
099200050111     c                   Eval      vidmsg = msg(09)
099300050111     c                   Move      d10cop        w008a
099400050111     c                   Eval      %subst(vidmsg:36:8) = w008a
099500050111     c                   Leavesr
099600050111e   4c                   EndIf
099700050111      * deve essere già abilitato ai servizi ON LINE
099800050111     c                   Move      *all'0'       vssksu
099900050111     c                   Move      v1cksu        vssksu
100000050111     c                   Eval      vssisv = 'TT'
100100050111     c     kTivss        Chain(n)  Tivss02l
100200050111if  4c                   If        Not %Found(Tivss02l)
100300050111     c                   Eval      *In28 = *On
100400050111     c                   Eval      *In44 = *On
100500050111     c                   Eval      vidmsg = msg(11)
100600050111     c                   Leavesr
100700050111e   4c                   EndIf
100800050316if  4c**!!!              If        wksu = *Blanks or wksu = *Zeros
100900050316     c                   If        wksu <> v1cksu
101000050111     c                   Eval      wksu = v1cksu
101100050111     c                   Eval      *In90 = *On
101200050112e   4c                   EndIf
101300050111     c                   Leavesr
101400050111e   3c                   EndIf
101500050316      * se il codice padre non è inserito ma prima ne avevo inserito uno devo
101600050316      * pulire i dati del responsabile e dell'e-mail
101700050316   x2c                   Else
101800050316if  3c                   If        wksu <> *Zeros and wksu <> *Blanks
101900050316     c                   Clear                   wksu
102000050316     c                   Clear                   v1cksu
102100050316     c                   Clear                   v1dksu
102200050316     c                   Clear                   v1crsp
102300050316     c                   Clear                   v1cem1
102400050316     c                   Clear                   v1cem2
102500050316     c                   Eval      *In04 = *Off
102600091029     c                   Eval      *In07 = *Off
102700050316    3c                   EndIf
102800050111e   2c                   EndIf
102900050111      * Responsabile
103000050111if  2c                   If        v1crsp = *Blanks
103100050111     c                   Eval      *In28 = *On
103200050111     c                   Eval      *In41 = *On
103300050111     c                   Eval      vidmsg = msg(06)
103400050111     c                   Leavesr
103500050111e   2c                   EndIf
103600050111      * E-mail
103700050111if  2c                   If        v1cem1 = *Blanks and v1cem2 = *Blanks
103800050111     c                   Eval      *In28 = *On
103900050111     c                   Eval      *In42 = *On
104000050111     c                   Eval      vidmsg = msg(07)
104100050111     c                   Leavesr
104200050111e   2c                   EndIf
104300050111     c                   Clear                   dsemail
104400050111     c                   Eval      §emlindi = v1cem1 + v1cem2
104500050111     c                   Call      'XEMAIL'
104600050111     c                   Parm                    dsemail
104700050111if  2c                   If        §emlerro = '1'
104800050111     c                   Eval      *In28 = *On
104900050111     c                   Eval      *In42 = *On
105000050111     c                   Eval      vidmsg = msg(08)
105100050111     c                   Leavesr
105200050111e   2c                   EndIf
105300050111     c                   Eval      v1cem1 = %subst(§emlindo:1:49)
105400050111     c                   Eval      v1cem2 = %subst(§emlindo:50:49)
105500050111e   1c                   EndIf
105600050111
105700050111     c                   EndSr
105800101006
105900101006      /free
106000101006       //--------------------------------------------------------------
106100101006       //?Reinvio password a cliente abilitato.
106200101006       //--------------------------------------------------------------
106300101006       BEGSR sr_Password;
106400101006
106500120126       //?Come prima cosa verifico che non ci siano più abilitazioni per
106600120126       //?lo stesso unificante
106700120126         clear w_Conta;
106800120126         clear w_VSSsun;
106900120126         setll (VSSksu : VSSisv) TIVSS02L;
107000120126         reade(n) (VSSksu : VSSisv) TIVSS02L;
107100120126         DOW  not %eof(TIVSS02L);
107200120126           IF  w_VSSsun <> VSSsun;
107300120126             w_VSSsun = VSSsun;
107400120126             w_Conta += 1;
107500120126           ENDIF;
107600120126           reade(n) (VSSksu : VSSisv) TIVSS02L;
107700120126         ENDDO;
107800120126         IF  w_Conta > 1;
107900120126           *in28 = *on;
108000120126           *in90 = *on;
108100120126           VIDmsg = msg(18);
108200120126           leavesr;
108300120126         ENDIF;
108400120126
108500101006         $Fine = *off;
108600101019         w03cem1 = v1cem1;
108700101019         w03cem2 = v1cem2;
108800101019         w03crsp = v1crsp;
108900101006       //?Emetto la window
109000101006         DOW  not $Fine;
109100101006           exfmt TA8203W;
109200101006           *in28 = *off;
109300101006           clear w03msg;
109400101006           SELECT;
109500101006             WHEN  *inkf;
109600101018               exsr CtrW03;
109700101018               IF  *in28;
109800101018                 iter;
109900101018               ENDIF;
110000101006               exsr sr_newpsw;
110100101006               IF  not *in28;
110200101006                 $Fine = *on;
110300101006               ENDIF;
110400101006             WHEN  *inkl;
110500101006               *in90 = *on;
110600101006               $Fine = *on;
110700101018             OTHER;
110800101018               exsr CtrW03;
110900101006           ENDSL;
111000101006         ENDDO;
111100101006
111200101006       ENDSR;
111300101018
111400101018       //--------------------------------------------------------------
111500101018       //?Controllo i dati della window.
111600101018       //--------------------------------------------------------------
111700101018       BEGSR CtrW03;
111800101019
111900101019         *in41 = *off;
112000101019         *in42 = *off;
112100101018
112200101018       // E-mail
112300101019         IF  w03cem1 = *blanks and w03cem2 = *blanks;
112400101018           *in28 = *on;
112500101018           *in42 = *on;
112600101018           w03msg = msg(07);
112700101018           leavesr;
112800101018         ENDIF;
112900101018      /end-free
113000101018     c                   Clear                   dsemail
113100101019     c                   Eval      §emlindi = w03cem1 + w03cem2
113200101018     c                   Call      'XEMAIL'
113300101018     c                   Parm                    dsemail
113400101018if  2c                   If        §emlerro = '1'
113500101018     c                   Eval      *In28 = *On
113600101018     c                   Eval      *In42 = *On
113700101018     c                   Eval      w03msg = msg(08)
113800101018     c                   Leavesr
113900101018e   2c                   EndIf
114000101018      /free
114100101019       w03cem1 = %subst(§emlindo:1:49);
114200101019       w03cem2 = %subst(§emlindo:50:49);
114300101019
114400101019       // Responsabile
114500101019         IF  w03crsp = *blanks;
114600101019           *in28 = *on;
114700101019           *in41 = *on;
114800101019           w03msg = msg(06);
114900101019           leavesr;
115000101019         ENDIF;
115100101018
115200101018       ENDSR;
115300101006      /end-free
115400041117
115500041118      *------------------------------------------------------------------------*
115600001011      * F6 - Conferma
115700050110      *------------------------------------------------------------------------*
115800050110     c     Sr_Conferma   BegSr
115900050112
116000050112     c                   Eval      *In90 = *Off
116100100408
116200100408      /free
116300100408       //?Devo togliere gli apici dal campo V1CRSP
116400100408       //?richiesto da Fabrizio
116500100408       clear wpos;
116600100408       wpos = %scan(api:v1crsp);
116700100408       DOW  wpos > 0;
116800100408         v1crsp = %replace(' ':v1crsp:wpos);
116900100408         clear wpos;
117000100408         wpos = %scan(api:v1crsp);
117100100408       ENDDO;
117200100408      /end-free
117300050110
117400050110      * --> Pgm non richiamato
117500050110if  1c                   If        Not *In01
117600050110
117700050110      * Papà già abilitato e sono stati inseriti dei nuovi figli
117800050111if  2c                   If        *In04 and *In08
117900050110      * Aggiorno il legame WW su TIVSS se non aveva già dei figli
118000050110if  3c                   If        skfig(01) = *Zeros
118100050110     c                   Move      *all'0'       vssksu
118200050111     c   07              Move      v1cksu        vssksu
118300050111     c  n07              Move      v1ccli        vssksu
118400050110     c                   Eval      vssisv = 'TT'
118500050110     c     kTivss        Chain     Tivss02l
118600050110if  4c                   If        %Found(Tivss02l)
118700050110     c                   Eval      vsstle = 'WW'
118800050110     c                   Update    Tivss000
118900050110e   4c                   EndIf
119000050110e   3c                   EndIf
119100050110      * Scrivo i nuovi legami su TIKUN
119200050111     c                   Clear                   nrr
119300050110do  3c                   Do        *Hival
119400050110     c                   Eval      nrr = nrr +1
119500050110     c     nrr           Chain     ta8201s                            30
119600050110     c                   If        *In30
119700050110     c                   Leave
119800050110     c                   EndIf
119900050110      * Se non è un codice protetto
120000050110if  4c                   If        wprot = *Blanks and v1scod <> *Zeros
120100050110      * Scrivo TIKUN
120200050110     c                   Clear                   Tikun000
120300050111     c   07              Move      v1cksu        kuncop
120400050111     c  n07              Move      v1ccli        kuncop
120500050110     c                   Move      v1scod        kuncof
120600050110     c                   Eval      kuntle = 'WW'
120700050110     c                   Eval      kundde = wdatai
120800050110     c                   Eval      kundsc = wdataf
120900130412     C                   EVAL      KUNDIM = %dec(%date())
121000130412     C                   EVAL      KUNDUV = %dec(%date())
121100130412     C                   EVAL      KUNPUV = SDSUser
121200050110     c                   Write     Tikun000
121300050110e   4c                   EndIf
121400050110e   3c                   EndDo
121500050110e   2c                   EndIf
121600050111      * Codice nuovo da abilitare
121700050114if  2c                   If        Not *In04 and was888 = *Off
121800050110      * Abilito il codice richiamando il TIS730R
121900050110     c                   Clear                   Tis730ds
122000050110     c                   Eval      s30ric = 'S'
122100101006     c                   eval      s30opz = 'II'
122200101230     c   07              move      v1cksu        s30ksu
122300101230     c  n07              Eval      s30ksu = v1ccli
122400050110     c                   Eval      s30rsp = v1crsp
122500050110     c                   Eval      s30eml = v1cem1 + v1cem2
122600050110     c                   Eval      s30isv = 'TT'
122700050119     c                   Eval      s30dti = *date
122800050110     c                   Eval      s30dtf = wdataf
122900060905      * controllo chiodato perchè è successo che sia stato abilitato un cliente
123000060905      * con codice zero
123100060905      * non so se è colpa mia o di Gurro!!!!
123200060905     c                   if        s30ksu = 0
123300060905     c                   eval      vidmsg = 'Manca codice cliente telefonare al-
123400060905     c                                       CED in Sede!!'
123500060905     c                   eval      *in28 = *on
123600060905     c                   leavesr
123700060905     c                   endif
123800050110     c                   Call      'TIS730R'
123900050110     c                   Parm                    Kpjba
124000050110     c                   Parm                    Tis730ds
124100050112if  3c                   If        s30err <> *Blanks
124200101006if  4c                   If        s30err = '08'
124300050111     c                   Eval      vidmsg = s30msg
124400050112   x4c                   Else
124500050112     c                   Eval      vidmsg = msg(15)
124600050112e   4c                   EndIf
124700050111     c                   Eval      *In28 = *On
124800050111     c                   Leavesr
124900050112e   3c                   EndIf
125000110223      * Se sono stati inseriti dei figli
125100110223if  3c                   If        *In08
125200050110     c                   Move      *all'0'       vssksu
125300101230     c   07              Move      v1cksu        vssksu
125400101230     c  n07              Move      v1ccli        vssksu
125500050110     c                   Eval      vssisv = 'TT'
125600050110     c     kTivss        Chain     Tivss02l
125700050110if  4c                   If        %Found(Tivss02l)
125800050110     c                   Eval      vsstle = 'WW'
125900050110     c                   Update    Tivss000
126000050110e   4c                   EndIf
126100050110      * Scrivo i nuovi legami su TIKUN
126200050111     c                   Clear                   nrr
126300050111do  4c                   Do        *Hival
126400050110     c                   Eval      nrr = nrr +1
126500050110     c     nrr           Chain     ta8201s                            30
126600050110     c                   If        *In30
126700050110     c                   Leave
126800050110     c                   EndIf
126900101230if  5c**!!!              If        v1scod <> *Zeros
127000101230if  4c                   If        wprot = *Blanks and v1scod <> *Zeros
127100050110      * Scrivo TIKUN
127200050110     c                   Clear                   Tikun000
127300101230     c   07              Move      v1cksu        kuncop
127400101230     c  n07              Move      v1ccli        kuncop
127500101230     c**!!!              Move      v1ccli        kuncop
127600050110     c                   Move      v1scod        kuncof
127700050110     c                   Eval      kuntle = 'WW'
127800050110     c                   Eval      kundde = wdatai
127900050110     c                   Eval      kundsc = wdataf
128000130412     C                   EVAL      KUNDIM = %dec(%date())
128100130412     C                   EVAL      KUNDUV = %dec(%date())
128200130412     C                   EVAL      KUNPUV = SDSUser
128300050110     c                   Write     Tikun000
128400050111e   5c                   EndIf
128500050111e   4c                   EndDo
128600101230e   3c                   EndIf
128700050111e   2c                   EndIf
128800050110
128900050111     c                   Goto      Inz01
129000050110e   1c                   EndIf
129100050110
129200050111      * --> Pgm richiamato
129300050111if  1c                   If        *In01
129400050112      * Richiesta l'abilitazione del cliente
129500050111if  2c                   If        v1cabi = 'S'
129600050111      * Immesso il papà
129700050111if  3c                   If        *In07
129800050112      * Aggiorno il legame WW su TIVSS
129900050111     c                   Move      *all'0'       vssksu
130000050112     c                   Move      v1cksu        vssksu
130100050111     c                   Eval      vssisv = 'TT'
130200050111     c     kTivss        Chain     Tivss02l
130300050111if  4c                   If        %Found(Tivss02l)
130400050111     c                   Eval      vsstle = 'WW'
130500050111     c                   Update    Tivss000
130600050111e   4c                   EndIf
130700050111      * Scrivo i nuovi legami su TIKUN
130800050111      * Scrivo TIKUN
130900050111     c                   Clear                   Tikun000
131000050111     c                   Move      v1cksu        kuncop
131100050111     c                   Move      v1ccli        kuncof
131200050111     c                   Eval      kuntle = 'WW'
131300050111     c                   Eval      kundde = wdatai
131400050111     c                   Eval      kundsc = wdataf
131500130412     C                   EVAL      KUNDIM = %dec(%date())
131600130412     C                   EVAL      KUNDUV = %dec(%date())
131700130412     C                   EVAL      KUNPUV = SDSUser
131800050111     c                   Write     Tikun000
131900050111e   3c                   EndIf
132000050111      * Codice nuovo da abilitare
132100050114if  3c                   If        Not *In07 and was888 = *Off
132200050111      * Abilito il codice richiamando il TIS730R
132300050111     c                   Clear                   Tis730ds
132400050111     c                   Eval      s30ric = 'S'
132500101006     c                   eval      s30opz = 'II'
132600050111     c                   Eval      s30ksu = v1ccli
132700050111     c                   Eval      s30rsp = v1crsp
132800050111     c                   Eval      s30eml = v1cem1 + v1cem2
132900050111     c                   Eval      s30isv = 'TT'
133000050119     c                   Eval      s30dti = *date
133100050111     c                   Eval      s30dtf = wdataf
133200050111     c                   Call      'TIS730R'
133300050111     c                   Parm                    Kpjba
133400050111     c                   Parm                    Tis730ds
133500050112if  4c                   If        s30err <> *Blanks
133600101006if  5c                   If        s30err = '08'
133700050112     c                   Eval      vidmsg = s30msg
133800050112   x5c                   Else
133900050112     c                   Eval      vidmsg = msg(15)
134000050112e   5c                   EndIf
134100050111     c                   Eval      *In28 = *On
134200050111     c                   Leavesr
134300050112e   4c                   EndIf
134400050111e   3c                   EndIf
134500050111e   2c                   EndIf
134600050111     c                   Goto      Fine
134700050111e   1c                   EndIf
134800050110
134900050110     c                   EndSr
135000101006
135100101006      /free
135200101006       //--------------------------------------------------------------
135300101006       //?Nuova password a cliente abilitato.
135400101006       //--------------------------------------------------------------
135500101006       BEGSR sr_newpsw;
135600101018
135700101018       //?Devo togliere gli apici dal campo V1CRSP
135800101018       //?richiesto da Fabrizio
135900101018       clear wpos;
136000101019       wpos = %scan(api:w03crsp);
136100101018       DOW  wpos > 0;
136200101019         w03crsp = %replace(' ':w03crsp:wpos);
136300101018         clear wpos;
136400101019         wpos = %scan(api:w03crsp);
136500101018       ENDDO;
136600101006
136700101006       //?Richiamo pgm per creare nuova password
136800101006       //?solo se sono su sistema di produzione
136900101006         IF  was888 = *off;
137000101006           clear TIS730DS;
137100101006           S30ric = 'S';
137200101018           S30opz = 'UU';
137300101006           S30sun = VSSsun;
137400101006           S30ksu = %dec(VSSksu:8:0);
137500101019           S30rsp = w03crsp;
137600101019           S30eml = w03cem1 + w03cem2;
137700101006           S30isv = 'TT';
137800101006           S30dti = VSSdde;
137900101006           S30dtf = VSSdsc;
138000101006           tis730r (kpjba : TIS730DS);
138100101006           SELECT;
138200101006             WHEN  S30err = '16';
138300101006             WHEN  S30err = '08';
138400101006               w03msg = S30msg;
138500101006               *in28 = *on;
138600101006             WHEN  S30err <> *blanks;
138700101006               w03msg = msg(15);
138800101006               *in28 = *on;
138900101006           ENDSL;
139000101006         ENDIF;
139100101006
139200101006       ENDSR;
139300101006      /end-free
139400050110
139500050110      *------------------------------------------------------------------------*
139600050110      * ROUTINE INIZIALE
139700050110      *------------------------------------------------------------------------*
139800050110     c     *Inzsr        BegSr
139900050110
140000050110     c     *Entry        Plist
140100050110     c                   Parm                    Kpjba
140200050103     c                   Eval      Param = Kpjbu
140300050103
140400050103     c                   Eval      *In01 = (parflg <> *Blanks)
140500050110
140600050110     c     *dtaara       define    §azute        azuteds
140700050110     c     *dtaara       define    §datiute      ddatiute
140800050110     c                   in(E)     *dtaara
140900050110     c                   If        %error  or RSUT = *blanks
141000050110     c                   Clear                   Tibs34ds
141100050110     c                   Call      'TIBS34R'
141200050110     c                   Parm                    Tibs34ds
141300050110     c                   In        *dtaara
141400050110     c                   EndIf
141500050110
141600050110     c                   Clear                   wabi
141700050110     c                   Clear                   dLat
141800050110
141900050110      * Verifica errori e autorità profilo
142000050110s   1c                   Select
142100050110      * se ho errori nei dati utente esco dal pgm
142200050110w   1c                   When      DutErr = 'E'
142300050110     c                   GoTo      Fine
142400050110      * se non c'è l'abilitazione
142500050110      * --> se 1° livello, abilitazioni al terminal
142600050110      *     se 2° livello, abilitazioni al punto operativo
142700050110      *     se sede è impossibile ma se capita mando a fine il pgm
142800050110w   1c                   When      UteAut = *Blanks
142900050110if  2c                   If        DutLpo = '1'
143000050110     c                   Eval      wabi   = 'TP'
143100050110e   2c                   EndIf
143200050110if  2c                   If        DutLpo = '2'
143300050110     c                   Eval      wabi   = 'PO'
143400050110e   2c                   EndIf
143500050110if  2c                   If        DutLpo = 'S'
143600050110     c                   GoTo      Fine
143700050110e   2c                   EndIf
143800050110      * carica le abilitazioni del profilo
143900050110x   1c                   Other
144000050110     c                   Movel     UteFaf        Dute01
144100050110if  2c                   If        §UteCli <> *Blanks
144200050110     c                   Eval      wabi = §UteCli
144300050110   x2c                   Else
144400050110     c                   Eval      wabi = UteAut
144500050110e   2c                   EndIf
144600050110e   1c                   EndSl
144700050110
144800050110      * controllo se ok l'abilitazione dell'utente
144900050110     c                   Clear                   Tibs02ds
145000050110     c                   Eval      T02Mod = 'C'
145100050110     c                   Eval      T02Sif = knsif
145200050110     c                   Eval      T02Cod = 'LAT'
145300050110     c                   Movel(p)  wabi          T02Ke1
145400050110     c                   Call      'TIBS02R'
145500050110     c                   Parm                    kpjba
145600050110     c                   Parm                    Tibs02ds
145700050110if  1c                   If        T02Err = *Blanks
145800050110     c                   Eval      dLat = T02Uni
145900050110e   1c                   EndIf
146000050110      * errore o non abilitato
146100050110if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
146200050103     c                   Eval      *In02 = *On
146300050110     c                   Eval      *In28 = *On
146400050104     c                   Eval      vidmsg = msg(01)
146500050110e   1c                   EndIf
146600050110
146700050110      * Reperimento dei P.O. gestibili dall'utente
146800050110if  1c                   If        Not *In28
146900050110     c                   Clear                   Trul31ds
147000050110     c                   Eval      I31abi = wabi
147100050110     c                   Eval      I31cdi = DUTdis
147200050110     c                   Eval      I31car = DUTare
147300050110     c                   Eval      I31cpo = DUTpou
147400050110     c                   Call      'TRUL31R'
147500050110     c                   Parm                    KPJBA
147600050110     c                   Parm                    Trul31ds
147700050110if  2c                   If        O31pog > *zeros
147800050110     c                   Movea     O31pog        skpog
147900050110x   2c                   Else
148000050103     c                   Eval      *In02 = *On
148100050110     c                   Eval      *In28 = *On
148200050104     c                   Eval      vidmsg = msg(01)
148300050110e   2c                   EndIf
148400050110e   1c                   EndIf
148500101018
148600050110
148700050110      * Se è ambiente prova GAITRAGRPS
148800050110     c                   If        %subst(knsif:7:1) = 'P'
148900050114     c                   Eval      was888 = *On
149000050110      * Se non è ambiente prova GAITRAGRU
149100050110     c                   Else
149200050114     c                   Eval      was888 = *Off
149300050110     c                   EndIf
149400050110
149500050110      * Calcolo oggi -60 gg.
149600050110     c     *iso          Move      *date         dataiso
149700050110     c                   subdur    60:*d         dataiso
149800050110     c                   Move      dataiso       wdatai
149900050110     c                   Move      20391231      wdataf
150000050110
150100050110      * KLIST
150200050104     c     kTiabl        klist
150300050104     c                   kfld                    vsssun
150400050104     c                   kfld                    vssisv
150500050104     c                   kfld                    abltip
150600050104
150700050104     c     kTivss        klist
150800050104     c                   kfld                    vssksu
150900050104     c                   kfld                    vssisv
151000050110
151100050110     c                   EndSr
151200050110      *------------------------------------------------------------------------*
151300050110
151400050110** MSG  Lungh. 78                                                            *
151500050110Utente non autorizzato all'Abilitazione clienti ai servizi ON LINE            01
151600050110Immettere il Cliente                                                          02
151700050103Codice Cliente errato                                                         03
151800150204ATTENZIONE!! Cliente già abilitato                                            04
151900050309ATTENZIONE!! Cliente con abilitazione scaduta avvisare il CED!!!!!            05
152000050309Immettere il Responsabile password                                            06
152100050309Immettere l'indirizzo e-mail x invio password                                 07
152200050104Indirizzo e-mail errato                                                       08
152300050104Il codice inserito è già figlio di                                            09
152400050105Il codice inserito è un padre                                                 10
152500050105Il codice padre non è abilitato ai servizi ON LINE                            11
152600050110Codice già presente                                                           12
152700050110Codice Cliente non gestibile dall'utente                                      13
152800050111ATTENZIONE!! Non sono state effettuate modifiche                              14
152900050112ATTENZIONE!! Non è possibile abilitare il cliente... RIPROVARE PIU' TARDI     15
153000091029Codice figlio uguale al codice da abilitare                                   16
153100101007ATTENZIONE!! Modifiche in sospeso, prima fare F6=Conferma                     17
153200120126Presenti più abilitazioni sullo stesso cliente. CONTATTARE IL CED!!!          18
153300150204Cliente già abilitato, ENTER per continuare                                   19
