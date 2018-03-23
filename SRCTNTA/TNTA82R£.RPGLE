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
051000050104     c     Sr_CtrSfl     BegSr
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
079500050105      * Non deve essere già abilitato
079600050105     c                   Move      *all'0'       vssksu
079700050105     c                   Move      v1scod        vssksu
079800050105     c                   Eval      vssisv = 'TT'
079900050110     c     kTivss        Chain(n)  Tivss02l
080000050105if  4c                   If        %Found(Tivss02l)
080100050105     c                   Eval      *In28 = *On
080200050105     c                   Eval      *In43 = *On
080300050105     c                   Eval      vidmsg = msg(04)
080400050308     c                   Z-add     nrr           savnrr
080500050105     c                   Update    ta8201s
080600050114     c                   Leavesr
080700050105e   4c                   EndIf
080800050114      * carico il nuovo codice in una sk di appoggio x controllare che non
080900050114      * siano immessi dei codici doppi
081000050114     c                   Eval      xx = 1
081100050114     c     *Zeros        Lookup    sksfl(xx)                              31
081200050114if  4c                   If        *In31
081300050114     c                   Eval      sksfl(xx) = v1scod
081400050114e   4c                   EndIf
081500050110     c                   Eval      *In08 = *On
081600050104e   3c                   EndIf
081700050104e   2c                   EndIf
081800050110      * se era protetto lo proteggo di nuovo
081900050110if  2c                   If        wprot = 'S'
082000050110     c                   Eval      *In06 = *On
082100050110e   2c                   EndIf
082200050104
082300050308     c                   Z-add     nrr           rec
082400050104     c                   Update    ta8201s
082500050104
082600050104e   1c                   EndDo
082700050111
082800050111      * Il papà è abilitato ma non sono stati aggiunti dei nuovi figli
082900050111      * errore
083000101006      * e non è stato dato F8=Reinvio Password
083100050309if  1c                   If        *In04 and Not *In08 and Not *In05
083200101006     c                             and not *inkh
083300050111     c                   Eval      *In28 = *On
083400050111     c                   Eval      *In44 = *On
083500050111     c                   Eval      vidmsg = msg(14)
083600050111     c                   Leavesr
083700050111e   1c                   EndIf
083800050104
083900101007      * Il papà è abilitato sono stati aggiunti dei nuovi figli
084000101007      * ma è stato dato F8=Reinvio Password
084100101007      * errore perchè prima si deve fare F6=Conferma
084200101007if  1c                   If        *In04 and *In08 and Not *In05
084300101007     c                             and *inkh
084400101007     c                   Eval      *In28 = *On
084500101007     c                   Eval      *In44 = *On
084600101007     c                   Eval      vidmsg = msg(17)
084700101007     c                   Leavesr
084800101007e   1c                   EndIf
084900101007
085000050104     c                   EndSr
085100050104
085200050104      *------------------------------------------------------------------------*
085300050104      * CONTROLLO IL CLIENTE
085400050104      *------------------------------------------------------------------------*
085500050104     c     Sr_Chkcli     BegSr
085600050104
085700050104     c                   Clear                   Tibs69ds
085800050104     c                   Move      w0070         I69kac
085900050104     c                   Call      'TIBS69R'
086000050112     c                   Parm                    Tibs69ds
086100050104     c                   Parm                    ds_cnaco
086200050104     c                   Parm                    ds_cnind
086300050104     c                   Parm                    ds_cnclp
086400050104     c                   Parm                    ds_fncls
086500050112     c                   Eval      wtibs69 = *On
086600050104
086700050104     c                   EndSr
086800041117
086900050103      *------------------------------------------------------------------------*
087000050111      * CARICA DATI NELLA VIDEATA PER GESTIONE DA OFFERTA
087100050103      *------------------------------------------------------------------------*
087200050103     c     Sr_Car02d     BegSr
087300050103
087400050111     c                   Move      parcli        v1ccli
087500050111     c                   Clear                   v1dcli
087600050111     c                   Clear                   v1crsp
087700050111     c                   Clear                   v1cem1
087800050111     c                   Clear                   v1cem2
087900050103     c                   Eval      V1cAbi = 'S'
088000050111     c                   Clear                   v1cksu
088100050104     c                   Clear                   v1dksu
088200050103
088300050111     c                   Move      v1ccli        w0070
088400050111     c                   ExSr      Sr_ChkCli
088500050104if  1c                   If        O69err <> *On
088600050111     c                   Movel     ACOrag        v1dcli
088700050103e   1c                   EndIf
088800050103
088900050103     c                   EndSr
089000050111
089100050111      *------------------------------------------------------------------------*
089200050111      * CONTROLLO LA VIDEATA PER GESTIONE DA OFFERTA
089300050111      *------------------------------------------------------------------------*
089400050111     c     Sr_Ctr02d     BegSr
089500050111
089600050111     c                   Eval      *In41 = *Off
089700050111     c                   Eval      *In42 = *Off
089800050112     c                   Eval      *In43 = *Off
089900050111     c                   Eval      *In44 = *Off
090000050111     c                   Eval      *In90 = *Off
090100050111
090200050111      * Se immesso che abilito il cliente
090300050111if  1c                   If        v1cabi = 'S'
090400050112      * Il codice non deve essere già figlio di un altro papà
090500050112     c                   Clear                   Tibs10ds
090600050112     c                   Eval      d10tle = 'WW'
090700050112     c                   Eval      d10paf = 'P'
090800050112     c                   Move      v1ccli        d10cod
090900050112     c                   Eval      d10drf = *date
091000050112     c                   Call      'TIBS10R'
091100050112     c                   Parm                    Tibs10ds
091200050112if  2c                   If        d10err = *Blanks
091300050112     c                   Eval      *In28 = *On
091400050112     c                   Eval      *In43 = *On
091500050112     c                   Eval      vidmsg = msg(09)
091600050112     c                   Move      d10cop        w008a
091700050112     c                   Eval      %subst(vidmsg:36:8) = w008a
091800050112     c                   Leavesr
091900050112e   2c                   EndIf
092000050111      * Codice Papà inserito
092100050111if  2c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
092200050111
092300050111if  3c                   If        vidksu > '09999999'
092400050111     c                   Eval      *In28 = *On
092500050111     c                   Eval      *In44 = *On
092600050111     c                   Eval      vidmsg = msg(03)
092700050111     c                   Leavesr
092800050111e   3c                   EndIf
092900050111      * Ricerca
093000050310      * forzo autorizzazione AZ in modo da visualizzare tutti i clienti
093100050111     c     '?'           Scan      v1cksu                                 31
093200050111if  3c                   If        *In31
093300050111     c                   Clear                   Xclirds
093400050111     c                   Eval      DxcCap = DutKci
093500050310     c                   Eval      DxcAut = 'AZ'
093600050111     c                   Call      'XCLIR'
093700050111     c                   Parm                    Xclirds
093800050111     c                   Movea     DxcKsc        ksa
093900050111     c                   move      ksa(1)        dsksa
094000050111     c                   z-add     dsksn         ksc(1)
094100050111     c                   Move      *all'0'       v1cksu
094200050111     c                   Move      ksc(1)        v1cksu
094300050111     c                   Eval      *In44 = *On
094400050111e   3c                   EndIf
094500050111      * Solo codici numerici
094600050111if  3c                   If        v1cksu < *Zeros
094700050111     c                   Eval      *In28 = *On
094800050111     c                   Eval      *In44 = *On
094900050111     c                   Eval      vidmsg = msg(03)
095000050111     c                   Leavesr
095100050111e   3c                   EndIf
095200050111      * Controllo esistenza codice
095300050111if  3c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
095400050111     c                   Move      v1cksu        w0070
095500050111     c                   ExSr      Sr_ChkCli
095600050111if  4c                   If        O69err = *On
095700050111     c                   Eval      *In28 = *On
095800050111     c                   Eval      *In44 = *On
095900050111     c                   Eval      vidmsg = msg(03)
096000050111     c                   Leavesr
096100050111e   4c                   EndIf
096200050111     c                   Movel     AcoRag        v1dksu
096300050111      * Codice gestibile dall'utente
096400050308     c**!!!              Eval      w003a = %subst(v1cksu:2:3)
096500050308     c**!!!w003a         Lookup    skpog                                  31
096600050308if  4c**!!!              If        Not *In31
096700050308     c**!!!              Eval      *In28 = *On
096800050308     c**!!!              Eval      *In44 = *On
096900050308     c**!!!              Eval      vidmsg = msg(13)
097000050308     c**!!!              Leavesr
097100050308e   4c**!!!              EndIf
097200050111      * non deve essere figlio di altro papà
097300050111     c                   Clear                   Tibs10ds
097400050111     c                   Eval      d10tle = 'WW'
097500050111     c                   Eval      d10paf = 'P'
097600050111     c                   Move      v1cksu        d10cod
097700050111     c                   Eval      d10drf = *date
097800050111     c                   Call      'TIBS10R'
097900050111     c                   Parm                    Tibs10ds
098000050111if  4c                   If        d10err = *Blanks
098100050111     c                   Eval      *In28 = *On
098200050111     c                   Eval      *In44 = *On
098300050111     c                   Eval      vidmsg = msg(09)
098400050111     c                   Move      d10cop        w008a
098500050111     c                   Eval      %subst(vidmsg:36:8) = w008a
098600050111     c                   Leavesr
098700050111e   4c                   EndIf
098800050111      * deve essere già abilitato ai servizi ON LINE
098900050111     c                   Move      *all'0'       vssksu
099000050111     c                   Move      v1cksu        vssksu
099100050111     c                   Eval      vssisv = 'TT'
099200050111     c     kTivss        Chain(n)  Tivss02l
099300050111if  4c                   If        Not %Found(Tivss02l)
099400050111     c                   Eval      *In28 = *On
099500050111     c                   Eval      *In44 = *On
099600050111     c                   Eval      vidmsg = msg(11)
099700050111     c                   Leavesr
099800050111e   4c                   EndIf
099900050316if  4c**!!!              If        wksu = *Blanks or wksu = *Zeros
100000050316     c                   If        wksu <> v1cksu
100100050111     c                   Eval      wksu = v1cksu
100200050111     c                   Eval      *In90 = *On
100300050112e   4c                   EndIf
100400050111     c                   Leavesr
100500050111e   3c                   EndIf
100600050316      * se il codice padre non è inserito ma prima ne avevo inserito uno devo
100700050316      * pulire i dati del responsabile e dell'e-mail
100800050316   x2c                   Else
100900050316if  3c                   If        wksu <> *Zeros and wksu <> *Blanks
101000050316     c                   Clear                   wksu
101100050316     c                   Clear                   v1cksu
101200050316     c                   Clear                   v1dksu
101300050316     c                   Clear                   v1crsp
101400050316     c                   Clear                   v1cem1
101500050316     c                   Clear                   v1cem2
101600050316     c                   Eval      *In04 = *Off
101700091029     c                   Eval      *In07 = *Off
101800050316    3c                   EndIf
101900050111e   2c                   EndIf
102000050111      * Responsabile
102100050111if  2c                   If        v1crsp = *Blanks
102200050111     c                   Eval      *In28 = *On
102300050111     c                   Eval      *In41 = *On
102400050111     c                   Eval      vidmsg = msg(06)
102500050111     c                   Leavesr
102600050111e   2c                   EndIf
102700050111      * E-mail
102800050111if  2c                   If        v1cem1 = *Blanks and v1cem2 = *Blanks
102900050111     c                   Eval      *In28 = *On
103000050111     c                   Eval      *In42 = *On
103100050111     c                   Eval      vidmsg = msg(07)
103200050111     c                   Leavesr
103300050111e   2c                   EndIf
103400050111     c                   Clear                   dsemail
103500050111     c                   Eval      §emlindi = v1cem1 + v1cem2
103600050111     c                   Call      'XEMAIL'
103700050111     c                   Parm                    dsemail
103800050111if  2c                   If        §emlerro = '1'
103900050111     c                   Eval      *In28 = *On
104000050111     c                   Eval      *In42 = *On
104100050111     c                   Eval      vidmsg = msg(08)
104200050111     c                   Leavesr
104300050111e   2c                   EndIf
104400050111     c                   Eval      v1cem1 = %subst(§emlindo:1:49)
104500050111     c                   Eval      v1cem2 = %subst(§emlindo:50:49)
104600050111e   1c                   EndIf
104700050111
104800050111     c                   EndSr
104900101006
105000101006      /free
105100101006       //--------------------------------------------------------------
105200101006       //?Reinvio password a cliente abilitato.
105300101006       //--------------------------------------------------------------
105400101006       BEGSR sr_Password;
105500101006
105600120126       //?Come prima cosa verifico che non ci siano più abilitazioni per
105700120126       //?lo stesso unificante
105800120126         clear w_Conta;
105900120126         clear w_VSSsun;
106000120126         setll (VSSksu : VSSisv) TIVSS02L;
106100120126         reade(n) (VSSksu : VSSisv) TIVSS02L;
106200120126         DOW  not %eof(TIVSS02L);
106300120126           IF  w_VSSsun <> VSSsun;
106400120126             w_VSSsun = VSSsun;
106500120126             w_Conta += 1;
106600120126           ENDIF;
106700120126           reade(n) (VSSksu : VSSisv) TIVSS02L;
106800120126         ENDDO;
106900120126         IF  w_Conta > 1;
107000120126           *in28 = *on;
107100120126           *in90 = *on;
107200120126           VIDmsg = msg(18);
107300120126           leavesr;
107400120126         ENDIF;
107500120126
107600101006         $Fine = *off;
107700101019         w03cem1 = v1cem1;
107800101019         w03cem2 = v1cem2;
107900101019         w03crsp = v1crsp;
108000101006       //?Emetto la window
108100101006         DOW  not $Fine;
108200101006           exfmt TA8203W;
108300101006           *in28 = *off;
108400101006           clear w03msg;
108500101006           SELECT;
108600101006             WHEN  *inkf;
108700101018               exsr CtrW03;
108800101018               IF  *in28;
108900101018                 iter;
109000101018               ENDIF;
109100101006               exsr sr_newpsw;
109200101006               IF  not *in28;
109300101006                 $Fine = *on;
109400101006               ENDIF;
109500101006             WHEN  *inkl;
109600101006               *in90 = *on;
109700101006               $Fine = *on;
109800101018             OTHER;
109900101018               exsr CtrW03;
110000101006           ENDSL;
110100101006         ENDDO;
110200101006
110300101006       ENDSR;
110400101018
110500101018       //--------------------------------------------------------------
110600101018       //?Controllo i dati della window.
110700101018       //--------------------------------------------------------------
110800101018       BEGSR CtrW03;
110900101019
111000101019         *in41 = *off;
111100101019         *in42 = *off;
111200101018
111300101018       // E-mail
111400101019         IF  w03cem1 = *blanks and w03cem2 = *blanks;
111500101018           *in28 = *on;
111600101018           *in42 = *on;
111700101018           w03msg = msg(07);
111800101018           leavesr;
111900101018         ENDIF;
112000101018      /end-free
112100101018     c                   Clear                   dsemail
112200101019     c                   Eval      §emlindi = w03cem1 + w03cem2
112300101018     c                   Call      'XEMAIL'
112400101018     c                   Parm                    dsemail
112500101018if  2c                   If        §emlerro = '1'
112600101018     c                   Eval      *In28 = *On
112700101018     c                   Eval      *In42 = *On
112800101018     c                   Eval      w03msg = msg(08)
112900101018     c                   Leavesr
113000101018e   2c                   EndIf
113100101018      /free
113200101019       w03cem1 = %subst(§emlindo:1:49);
113300101019       w03cem2 = %subst(§emlindo:50:49);
113400101019
113500101019       // Responsabile
113600101019         IF  w03crsp = *blanks;
113700101019           *in28 = *on;
113800101019           *in41 = *on;
113900101019           w03msg = msg(06);
114000101019           leavesr;
114100101019         ENDIF;
114200101018
114300101018       ENDSR;
114400101006      /end-free
114500041117
114600041118      *------------------------------------------------------------------------*
114700001011      * F6 - Conferma
114800050110      *------------------------------------------------------------------------*
114900050110     c     Sr_Conferma   BegSr
115000050112
115100050112     c                   Eval      *In90 = *Off
115200100408
115300100408      /free
115400100408       //?Devo togliere gli apici dal campo V1CRSP
115500100408       //?richiesto da Fabrizio
115600100408       clear wpos;
115700100408       wpos = %scan(api:v1crsp);
115800100408       DOW  wpos > 0;
115900100408         v1crsp = %replace(' ':v1crsp:wpos);
116000100408         clear wpos;
116100100408         wpos = %scan(api:v1crsp);
116200100408       ENDDO;
116300100408      /end-free
116400050110
116500050110      * --> Pgm non richiamato
116600050110if  1c                   If        Not *In01
116700050110
116800050110      * Papà già abilitato e sono stati inseriti dei nuovi figli
116900050111if  2c                   If        *In04 and *In08
117000050110      * Aggiorno il legame WW su TIVSS se non aveva già dei figli
117100050110if  3c                   If        skfig(01) = *Zeros
117200050110     c                   Move      *all'0'       vssksu
117300050111     c   07              Move      v1cksu        vssksu
117400050111     c  n07              Move      v1ccli        vssksu
117500050110     c                   Eval      vssisv = 'TT'
117600050110     c     kTivss        Chain     Tivss02l
117700050110if  4c                   If        %Found(Tivss02l)
117800050110     c                   Eval      vsstle = 'WW'
117900050110     c                   Update    Tivss000
118000050110e   4c                   EndIf
118100050110e   3c                   EndIf
118200050110      * Scrivo i nuovi legami su TIKUN
118300050111     c                   Clear                   nrr
118400050110do  3c                   Do        *Hival
118500050110     c                   Eval      nrr = nrr +1
118600050110     c     nrr           Chain     ta8201s                            30
118700050110     c                   If        *In30
118800050110     c                   Leave
118900050110     c                   EndIf
119000050110      * Se non è un codice protetto
119100050110if  4c                   If        wprot = *Blanks and v1scod <> *Zeros
119200050110      * Scrivo TIKUN
119300050110     c                   Clear                   Tikun000
119400050111     c   07              Move      v1cksu        kuncop
119500050111     c  n07              Move      v1ccli        kuncop
119600050110     c                   Move      v1scod        kuncof
119700050110     c                   Eval      kuntle = 'WW'
119800050110     c                   Eval      kundde = wdatai
119900050110     c                   Eval      kundsc = wdataf
120000130412     C                   EVAL      KUNDIM = %dec(%date())
120100130412     C                   EVAL      KUNDUV = %dec(%date())
120200130412     C                   EVAL      KUNPUV = SDSUser
120300050110     c                   Write     Tikun000
120400050110e   4c                   EndIf
120500050110e   3c                   EndDo
120600050110e   2c                   EndIf
120700050111      * Codice nuovo da abilitare
120800050114if  2c                   If        Not *In04 and was888 = *Off
120900050110      * Abilito il codice richiamando il TIS730R
121000050110     c                   Clear                   Tis730ds
121100050110     c                   Eval      s30ric = 'S'
121200101006     c                   eval      s30opz = 'II'
121300101230     c   07              move      v1cksu        s30ksu
121400101230     c  n07              Eval      s30ksu = v1ccli
121500050110     c                   Eval      s30rsp = v1crsp
121600050110     c                   Eval      s30eml = v1cem1 + v1cem2
121700050110     c                   Eval      s30isv = 'TT'
121800050119     c                   Eval      s30dti = *date
121900050110     c                   Eval      s30dtf = wdataf
122000060905      * controllo chiodato perchè è successo che sia stato abilitato un cliente
122100060905      * con codice zero
122200060905      * non so se è colpa mia o di Gurro!!!!
122300060905     c                   if        s30ksu = 0
122400060905     c                   eval      vidmsg = 'Manca codice cliente telefonare al-
122500060905     c                                       CED in Sede!!'
122600060905     c                   eval      *in28 = *on
122700060905     c                   leavesr
122800060905     c                   endif
122900050110     c                   Call      'TIS730R'
123000050110     c                   Parm                    Kpjba
123100050110     c                   Parm                    Tis730ds
123200050112if  3c                   If        s30err <> *Blanks
123300101006if  4c                   If        s30err = '08'
123400050111     c                   Eval      vidmsg = s30msg
123500050112   x4c                   Else
123600050112     c                   Eval      vidmsg = msg(15)
123700050112e   4c                   EndIf
123800050111     c                   Eval      *In28 = *On
123900050111     c                   Leavesr
124000050112e   3c                   EndIf
124100110223      * Se sono stati inseriti dei figli
124200110223if  3c                   If        *In08
124300050110     c                   Move      *all'0'       vssksu
124400101230     c   07              Move      v1cksu        vssksu
124500101230     c  n07              Move      v1ccli        vssksu
124600050110     c                   Eval      vssisv = 'TT'
124700050110     c     kTivss        Chain     Tivss02l
124800050110if  4c                   If        %Found(Tivss02l)
124900050110     c                   Eval      vsstle = 'WW'
125000050110     c                   Update    Tivss000
125100050110e   4c                   EndIf
125200050110      * Scrivo i nuovi legami su TIKUN
125300050111     c                   Clear                   nrr
125400050111do  4c                   Do        *Hival
125500050110     c                   Eval      nrr = nrr +1
125600050110     c     nrr           Chain     ta8201s                            30
125700050110     c                   If        *In30
125800050110     c                   Leave
125900050110     c                   EndIf
126000101230if  5c**!!!              If        v1scod <> *Zeros
126100101230if  4c                   If        wprot = *Blanks and v1scod <> *Zeros
126200050110      * Scrivo TIKUN
126300050110     c                   Clear                   Tikun000
126400101230     c   07              Move      v1cksu        kuncop
126500101230     c  n07              Move      v1ccli        kuncop
126600101230     c**!!!              Move      v1ccli        kuncop
126700050110     c                   Move      v1scod        kuncof
126800050110     c                   Eval      kuntle = 'WW'
126900050110     c                   Eval      kundde = wdatai
127000050110     c                   Eval      kundsc = wdataf
127100130412     C                   EVAL      KUNDIM = %dec(%date())
127200130412     C                   EVAL      KUNDUV = %dec(%date())
127300130412     C                   EVAL      KUNPUV = SDSUser
127400050110     c                   Write     Tikun000
127500050111e   5c                   EndIf
127600050111e   4c                   EndDo
127700101230e   3c                   EndIf
127800050111e   2c                   EndIf
127900050110
128000050111     c                   Goto      Inz01
128100050110e   1c                   EndIf
128200050110
128300050111      * --> Pgm richiamato
128400050111if  1c                   If        *In01
128500050112      * Richiesta l'abilitazione del cliente
128600050111if  2c                   If        v1cabi = 'S'
128700050111      * Immesso il papà
128800050111if  3c                   If        *In07
128900050112      * Aggiorno il legame WW su TIVSS
129000050111     c                   Move      *all'0'       vssksu
129100050112     c                   Move      v1cksu        vssksu
129200050111     c                   Eval      vssisv = 'TT'
129300050111     c     kTivss        Chain     Tivss02l
129400050111if  4c                   If        %Found(Tivss02l)
129500050111     c                   Eval      vsstle = 'WW'
129600050111     c                   Update    Tivss000
129700050111e   4c                   EndIf
129800050111      * Scrivo i nuovi legami su TIKUN
129900050111      * Scrivo TIKUN
130000050111     c                   Clear                   Tikun000
130100050111     c                   Move      v1cksu        kuncop
130200050111     c                   Move      v1ccli        kuncof
130300050111     c                   Eval      kuntle = 'WW'
130400050111     c                   Eval      kundde = wdatai
130500050111     c                   Eval      kundsc = wdataf
130600130412     C                   EVAL      KUNDIM = %dec(%date())
130700130412     C                   EVAL      KUNDUV = %dec(%date())
130800130412     C                   EVAL      KUNPUV = SDSUser
130900050111     c                   Write     Tikun000
131000050111e   3c                   EndIf
131100050111      * Codice nuovo da abilitare
131200050114if  3c                   If        Not *In07 and was888 = *Off
131300050111      * Abilito il codice richiamando il TIS730R
131400050111     c                   Clear                   Tis730ds
131500050111     c                   Eval      s30ric = 'S'
131600101006     c                   eval      s30opz = 'II'
131700050111     c                   Eval      s30ksu = v1ccli
131800050111     c                   Eval      s30rsp = v1crsp
131900050111     c                   Eval      s30eml = v1cem1 + v1cem2
132000050111     c                   Eval      s30isv = 'TT'
132100050119     c                   Eval      s30dti = *date
132200050111     c                   Eval      s30dtf = wdataf
132300050111     c                   Call      'TIS730R'
132400050111     c                   Parm                    Kpjba
132500050111     c                   Parm                    Tis730ds
132600050112if  4c                   If        s30err <> *Blanks
132700101006if  5c                   If        s30err = '08'
132800050112     c                   Eval      vidmsg = s30msg
132900050112   x5c                   Else
133000050112     c                   Eval      vidmsg = msg(15)
133100050112e   5c                   EndIf
133200050111     c                   Eval      *In28 = *On
133300050111     c                   Leavesr
133400050112e   4c                   EndIf
133500050111e   3c                   EndIf
133600050111e   2c                   EndIf
133700050111     c                   Goto      Fine
133800050111e   1c                   EndIf
133900050110
134000050110     c                   EndSr
134100101006
134200101006      /free
134300101006       //--------------------------------------------------------------
134400101006       //?Nuova password a cliente abilitato.
134500101006       //--------------------------------------------------------------
134600101006       BEGSR sr_newpsw;
134700101018
134800101018       //?Devo togliere gli apici dal campo V1CRSP
134900101018       //?richiesto da Fabrizio
135000101018       clear wpos;
135100101019       wpos = %scan(api:w03crsp);
135200101018       DOW  wpos > 0;
135300101019         w03crsp = %replace(' ':w03crsp:wpos);
135400101018         clear wpos;
135500101019         wpos = %scan(api:w03crsp);
135600101018       ENDDO;
135700101006
135800101006       //?Richiamo pgm per creare nuova password
135900101006       //?solo se sono su sistema di produzione
136000101006         IF  was888 = *off;
136100101006           clear TIS730DS;
136200101006           S30ric = 'S';
136300101018           S30opz = 'UU';
136400101006           S30sun = VSSsun;
136500101006           S30ksu = %dec(VSSksu:8:0);
136600101019           S30rsp = w03crsp;
136700101019           S30eml = w03cem1 + w03cem2;
136800101006           S30isv = 'TT';
136900101006           S30dti = VSSdde;
137000101006           S30dtf = VSSdsc;
137100101006           tis730r (kpjba : TIS730DS);
137200101006           SELECT;
137300101006             WHEN  S30err = '16';
137400101006             WHEN  S30err = '08';
137500101006               w03msg = S30msg;
137600101006               *in28 = *on;
137700101006             WHEN  S30err <> *blanks;
137800101006               w03msg = msg(15);
137900101006               *in28 = *on;
138000101006           ENDSL;
138100101006         ENDIF;
138200101006
138300101006       ENDSR;
138400101006      /end-free
138500050110
138600050110      *------------------------------------------------------------------------*
138700050110      * ROUTINE INIZIALE
138800050110      *------------------------------------------------------------------------*
138900050110     c     *Inzsr        BegSr
139000050110
139100050110     c     *Entry        Plist
139200050110     c                   Parm                    Kpjba
139300050103     c                   Eval      Param = Kpjbu
139400050103
139500050103     c                   Eval      *In01 = (parflg <> *Blanks)
139600050110
139700050110     c     *dtaara       define    §azute        azuteds
139800050110     c     *dtaara       define    §datiute      ddatiute
139900050110     c                   in(E)     *dtaara
140000050110     c                   If        %error  or RSUT = *blanks
140100050110     c                   Clear                   Tibs34ds
140200050110     c                   Call      'TIBS34R'
140300050110     c                   Parm                    Tibs34ds
140400050110     c                   In        *dtaara
140500050110     c                   EndIf
140600050110
140700050110     c                   Clear                   wabi
140800050110     c                   Clear                   dLat
140900050110
141000050110      * Verifica errori e autorità profilo
141100050110s   1c                   Select
141200050110      * se ho errori nei dati utente esco dal pgm
141300050110w   1c                   When      DutErr = 'E'
141400050110     c                   GoTo      Fine
141500050110      * se non c'è l'abilitazione
141600050110      * --> se 1° livello, abilitazioni al terminal
141700050110      *     se 2° livello, abilitazioni al punto operativo
141800050110      *     se sede è impossibile ma se capita mando a fine il pgm
141900050110w   1c                   When      UteAut = *Blanks
142000050110if  2c                   If        DutLpo = '1'
142100050110     c                   Eval      wabi   = 'TP'
142200050110e   2c                   EndIf
142300050110if  2c                   If        DutLpo = '2'
142400050110     c                   Eval      wabi   = 'PO'
142500050110e   2c                   EndIf
142600050110if  2c                   If        DutLpo = 'S'
142700050110     c                   GoTo      Fine
142800050110e   2c                   EndIf
142900050110      * carica le abilitazioni del profilo
143000050110x   1c                   Other
143100050110     c                   Movel     UteFaf        Dute01
143200050110if  2c                   If        §UteCli <> *Blanks
143300050110     c                   Eval      wabi = §UteCli
143400050110   x2c                   Else
143500050110     c                   Eval      wabi = UteAut
143600050110e   2c                   EndIf
143700050110e   1c                   EndSl
143800050110
143900050110      * controllo se ok l'abilitazione dell'utente
144000050110     c                   Clear                   Tibs02ds
144100050110     c                   Eval      T02Mod = 'C'
144200050110     c                   Eval      T02Sif = knsif
144300050110     c                   Eval      T02Cod = 'LAT'
144400050110     c                   Movel(p)  wabi          T02Ke1
144500050110     c                   Call      'TIBS02R'
144600050110     c                   Parm                    kpjba
144700050110     c                   Parm                    Tibs02ds
144800050110if  1c                   If        T02Err = *Blanks
144900050110     c                   Eval      dLat = T02Uni
145000050110e   1c                   EndIf
145100050110      * errore o non abilitato
145200050110if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
145300050103     c                   Eval      *In02 = *On
145400050110     c                   Eval      *In28 = *On
145500050104     c                   Eval      vidmsg = msg(01)
145600050110e   1c                   EndIf
145700050110
145800050110      * Reperimento dei P.O. gestibili dall'utente
145900050110if  1c                   If        Not *In28
146000050110     c                   Clear                   Trul31ds
146100050110     c                   Eval      I31abi = wabi
146200050110     c                   Eval      I31cdi = DUTdis
146300050110     c                   Eval      I31car = DUTare
146400050110     c                   Eval      I31cpo = DUTpou
146500050110     c                   Call      'TRUL31R'
146600050110     c                   Parm                    KPJBA
146700050110     c                   Parm                    Trul31ds
146800050110if  2c                   If        O31pog > *zeros
146900050110     c                   Movea     O31pog        skpog
147000050110x   2c                   Else
147100050103     c                   Eval      *In02 = *On
147200050110     c                   Eval      *In28 = *On
147300050104     c                   Eval      vidmsg = msg(01)
147400050110e   2c                   EndIf
147500050110e   1c                   EndIf
147600101018
147700050110
147800050110      * Se è ambiente prova GAITRAGRPS
147900050110     c                   If        %subst(knsif:7:1) = 'P'
148000050114     c                   Eval      was888 = *On
148100050110      * Se non è ambiente prova GAITRAGRU
148200050110     c                   Else
148300050114     c                   Eval      was888 = *Off
148400050110     c                   EndIf
148500050110
148600050110      * Calcolo oggi -60 gg.
148700050110     c     *iso          Move      *date         dataiso
148800050110     c                   subdur    60:*d         dataiso
148900050110     c                   Move      dataiso       wdatai
149000050110     c                   Move      20391231      wdataf
149100050110
149200050110      * KLIST
149300050104     c     kTiabl        klist
149400050104     c                   kfld                    vsssun
149500050104     c                   kfld                    vssisv
149600050104     c                   kfld                    abltip
149700050104
149800050104     c     kTivss        klist
149900050104     c                   kfld                    vssksu
150000050104     c                   kfld                    vssisv
150100050110
150200050110     c                   EndSr
150300050110      *------------------------------------------------------------------------*
150400050110
150500050110** MSG  Lungh. 78                                                            *
150600050110Utente non autorizzato all'Abilitazione clienti ai servizi ON LINE            01
150700050110Immettere il Cliente                                                          02
150800050103Codice Cliente errato                                                         03
150900050104ATTENZIONE!! Cliente già abilitato                                            04
151000050309ATTENZIONE!! Cliente con abilitazione scaduta avvisare il CED!!!!!            05
151100050309Immettere il Responsabile password                                            06
151200050309Immettere l'indirizzo e-mail x invio password                                 07
151300050104Indirizzo e-mail errato                                                       08
151400050104Il codice inserito è già figlio di                                            09
151500050105Il codice inserito è un padre                                                 10
151600050105Il codice padre non è abilitato ai servizi ON LINE                            11
151700050110Codice già presente                                                           12
151800050110Codice Cliente non gestibile dall'utente                                      13
151900050111ATTENZIONE!! Non sono state effettuate modifiche                              14
152000050112ATTENZIONE!! Non è possibile abilitare il cliente... RIPROVARE PIU' TARDI     15
152100091029Codice figlio uguale al codice da abilitare                                   16
152200101007ATTENZIONE!! Modifiche in sospeso, prima fare F6=Conferma                     17
152300120126Presenti più abilitazioni sullo stesso cliente. CONTATTARE IL CED!!!          18
