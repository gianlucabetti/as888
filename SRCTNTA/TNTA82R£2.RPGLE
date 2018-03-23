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
070000091029      * controllo in anagrafica
070100050104     c                   Move      v1scod        w0070
070200050104     c                   ExSr      Sr_ChkCli
070300050104if  4c                   If        O69err = *On
070400050104     c                   Eval      *In28 = *On
070500050104     c                   Eval      *In43 = *On
070600050104     c                   Eval      vidmsg = msg(03)
070700050308     c                   Z-add     nrr           savnrr
070800050104     c                   Update    ta8201s
070900050114     c                   Leavesr
071000050104e   4c                   EndIf
071100050104     c                   Movel     AcoRag        v1sdes
071200050308
071300050308      * Se il papà è gestibile dall'utente posso immettere qualsiasi figlio
071400050308      * altrimenti posso immettere solo figli gestiti dall'utente
071500050308     c                   Eval      w003a = %subst(v1cksu:2:3)
071600050308     c     w003a         Lookup    skpog                                  31
071700050308if 3ac                   If        Not *In31
071800050110      * Codice gestibile dall'utente
071900050111     c                   Move      v1scod        w008a
072000050111     c                   Eval      w003a = %subst(w008a:2:3)
072100050110     c     w003a         Lookup    skpog                                  31
072200050110if  4c                   If        Not *In31
072300050110     c                   Eval      *In28 = *On
072400050110     c                   Eval      *In43 = *On
072500050110     c                   Eval      vidmsg = msg(13)
072600050308     c                   Z-add     nrr           savnrr
072700050110     c                   Update    ta8201s
072800050114     c                   Leavesr
072900050110e   4c                   EndIf
073000050308e  3ac                   EndIf
073100050110      * non deve essere già caricato nella sk dei figli
073200050110     c                   Z-add     v1scod        w0110
073300050110     c     w0110         Lookup    skfig                                  31
073400050110if  4c                   If        *In31
073500050110     c                   Eval      *In28 = *On
073600050110     c                   Eval      *In43 = *On
073700050110     c                   Eval      vidmsg = msg(12)
073800050308     c                   Z-add     nrr           savnrr
073900050110     c                   Update    ta8201s
074000050114     c                   Leavesr
074100050110e   4c                   EndIf
074200050114      * non deve essere già stato inserito prima nel sfl
074300050114     c     v1scod        Lookup    sksfl                                  31
074400050114if  4c                   If        *In31
074500050114     c                   Eval      *In28 = *On
074600050114     c                   Eval      *In43 = *On
074700050114     c                   Eval      vidmsg = msg(12)
074800050308     c                   Z-add     nrr           savnrr
074900050114     c                   Update    ta8201s
075000050114     c                   Leavesr
075100050114e   4c                   EndIf
075200050104      * non deve essere già figlio di un altro papà
075300050104     c                   Clear                   Tibs10ds
075400050104     c                   Eval      d10tle = 'WW'
075500050104     c                   Eval      d10paf = 'P'
075600050104     c                   Move      v1scod        d10cod
075700050104     c                   Eval      d10drf = *date
075800050104     c                   Call      'TIBS10R'
075900050104     c                   Parm                    Tibs10ds
076000050104if  4c                   If        d10err = *Blanks
076100050104     c                   Eval      *In28 = *On
076200050104     c                   Eval      *In43 = *On
076300050104     c                   Eval      vidmsg = msg(09)
076400050104     c                   Move      d10cop        w008a
076500050104     c                   Eval      %subst(vidmsg:36:8) = w008a
076600050308     c                   Z-add     nrr           savnrr
076700050104     c                   Update    ta8201s
076800050114     c                   Leavesr
076900050104e   4c                   EndIf
077000050104      * non deve essere già papà
077100050104     c                   Clear                   Tibs10ds
077200050104     c                   Eval      d10tle = 'WW'
077300050104     c                   Eval      d10paf = 'F'
077400050104     c                   move      v1scod        d10cod
077500050104     c                   Eval      d10drf = *date
077600050104     c                   Call      'TIBS10R'
077700050104     c                   Parm                    Tibs10ds
077800050104if  4c                   If        d10err = *Blanks
077900050104     c                   Eval      *In28 = *On
078000050104     c                   Eval      *In43 = *On
078100050104     c                   Eval      vidmsg = msg(10)
078200050308     c                   Z-add     nrr           savnrr
078300050104     c                   Update    ta8201s
078400050114     c                   Leavesr
078500050104e   4c                   EndIf
078600050105      * Non deve essere già abilitato
078700050105     c                   Move      *all'0'       vssksu
078800050105     c                   Move      v1scod        vssksu
078900050105     c                   Eval      vssisv = 'TT'
079000050110     c     kTivss        Chain(n)  Tivss02l
079100050105if  4c                   If        %Found(Tivss02l)
079200050105     c                   Eval      *In28 = *On
079300050105     c                   Eval      *In43 = *On
079400050105     c                   Eval      vidmsg = msg(04)
079500050308     c                   Z-add     nrr           savnrr
079600050105     c                   Update    ta8201s
079700050114     c                   Leavesr
079800050105e   4c                   EndIf
079900050114      * carico il nuovo codice in una sk di appoggio x controllare che non
080000050114      * siano immessi dei codici doppi
080100050114     c                   Eval      xx = 1
080200050114     c     *Zeros        Lookup    sksfl(xx)                              31
080300050114if  4c                   If        *In31
080400050114     c                   Eval      sksfl(xx) = v1scod
080500050114e   4c                   EndIf
080600050110     c                   Eval      *In08 = *On
080700050104e   3c                   EndIf
080800050104e   2c                   EndIf
080900050110      * se era protetto lo proteggo di nuovo
081000050110if  2c                   If        wprot = 'S'
081100050110     c                   Eval      *In06 = *On
081200050110e   2c                   EndIf
081300050104
081400050308     c                   Z-add     nrr           rec
081500050104     c                   Update    ta8201s
081600050104
081700050104e   1c                   EndDo
081800050111
081900050111      * Il papà è abilitato ma non sono stati aggiunti dei nuovi figli
082000050111      * errore
082100101006      * e non è stato dato F8=Reinvio Password
082200050309if  1c                   If        *In04 and Not *In08 and Not *In05
082300101006     c                             and not *inkh
082400050111     c                   Eval      *In28 = *On
082500050111     c                   Eval      *In44 = *On
082600050111     c                   Eval      vidmsg = msg(14)
082700050111     c                   Leavesr
082800050111e   1c                   EndIf
082900050104
083000101007      * Il papà è abilitato sono stati aggiunti dei nuovi figli
083100101007      * ma è stato dato F8=Reinvio Password
083200101007      * errore perchè prima si deve fare F6=Conferma
083300101007if  1c                   If        *In04 and *In08 and Not *In05
083400101007     c                             and *inkh
083500101007     c                   Eval      *In28 = *On
083600101007     c                   Eval      *In44 = *On
083700101007     c                   Eval      vidmsg = msg(17)
083800101007     c                   Leavesr
083900101007e   1c                   EndIf
084000101007
084100050104     c                   EndSr
084200050104
084300050104      *------------------------------------------------------------------------*
084400050104      * CONTROLLO IL CLIENTE
084500050104      *------------------------------------------------------------------------*
084600050104     c     Sr_Chkcli     BegSr
084700050104
084800050104     c                   Clear                   Tibs69ds
084900050104     c                   Move      w0070         I69kac
085000050104     c                   Call      'TIBS69R'
085100050112     c                   Parm                    Tibs69ds
085200050104     c                   Parm                    ds_cnaco
085300050104     c                   Parm                    ds_cnind
085400050104     c                   Parm                    ds_cnclp
085500050104     c                   Parm                    ds_fncls
085600050112     c                   Eval      wtibs69 = *On
085700050104
085800050104     c                   EndSr
085900041117
086000050103      *------------------------------------------------------------------------*
086100050111      * CARICA DATI NELLA VIDEATA PER GESTIONE DA OFFERTA
086200050103      *------------------------------------------------------------------------*
086300050103     c     Sr_Car02d     BegSr
086400050103
086500050111     c                   Move      parcli        v1ccli
086600050111     c                   Clear                   v1dcli
086700050111     c                   Clear                   v1crsp
086800050111     c                   Clear                   v1cem1
086900050111     c                   Clear                   v1cem2
087000050103     c                   Eval      V1cAbi = 'S'
087100050111     c                   Clear                   v1cksu
087200050104     c                   Clear                   v1dksu
087300050103
087400050111     c                   Move      v1ccli        w0070
087500050111     c                   ExSr      Sr_ChkCli
087600050104if  1c                   If        O69err <> *On
087700050111     c                   Movel     ACOrag        v1dcli
087800050103e   1c                   EndIf
087900050103
088000050103     c                   EndSr
088100050111
088200050111      *------------------------------------------------------------------------*
088300050111      * CONTROLLO LA VIDEATA PER GESTIONE DA OFFERTA
088400050111      *------------------------------------------------------------------------*
088500050111     c     Sr_Ctr02d     BegSr
088600050111
088700050111     c                   Eval      *In41 = *Off
088800050111     c                   Eval      *In42 = *Off
088900050112     c                   Eval      *In43 = *Off
089000050111     c                   Eval      *In44 = *Off
089100050111     c                   Eval      *In90 = *Off
089200050111
089300050111      * Se immesso che abilito il cliente
089400050111if  1c                   If        v1cabi = 'S'
089500050112      * Il codice non deve essere già figlio di un altro papà
089600050112     c                   Clear                   Tibs10ds
089700050112     c                   Eval      d10tle = 'WW'
089800050112     c                   Eval      d10paf = 'P'
089900050112     c                   Move      v1ccli        d10cod
090000050112     c                   Eval      d10drf = *date
090100050112     c                   Call      'TIBS10R'
090200050112     c                   Parm                    Tibs10ds
090300050112if  2c                   If        d10err = *Blanks
090400050112     c                   Eval      *In28 = *On
090500050112     c                   Eval      *In43 = *On
090600050112     c                   Eval      vidmsg = msg(09)
090700050112     c                   Move      d10cop        w008a
090800050112     c                   Eval      %subst(vidmsg:36:8) = w008a
090900050112     c                   Leavesr
091000050112e   2c                   EndIf
091100050111      * Codice Papà inserito
091200050111if  2c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
091300050111
091400050111if  3c                   If        vidksu > '09999999'
091500050111     c                   Eval      *In28 = *On
091600050111     c                   Eval      *In44 = *On
091700050111     c                   Eval      vidmsg = msg(03)
091800050111     c                   Leavesr
091900050111e   3c                   EndIf
092000050111      * Ricerca
092100050310      * forzo autorizzazione AZ in modo da visualizzare tutti i clienti
092200050111     c     '?'           Scan      v1cksu                                 31
092300050111if  3c                   If        *In31
092400050111     c                   Clear                   Xclirds
092500050111     c                   Eval      DxcCap = DutKci
092600050310     c                   Eval      DxcAut = 'AZ'
092700050111     c                   Call      'XCLIR'
092800050111     c                   Parm                    Xclirds
092900050111     c                   Movea     DxcKsc        ksa
093000050111     c                   move      ksa(1)        dsksa
093100050111     c                   z-add     dsksn         ksc(1)
093200050111     c                   Move      *all'0'       v1cksu
093300050111     c                   Move      ksc(1)        v1cksu
093400050111     c                   Eval      *In44 = *On
093500050111e   3c                   EndIf
093600050111      * Solo codici numerici
093700050111if  3c                   If        v1cksu < *Zeros
093800050111     c                   Eval      *In28 = *On
093900050111     c                   Eval      *In44 = *On
094000050111     c                   Eval      vidmsg = msg(03)
094100050111     c                   Leavesr
094200050111e   3c                   EndIf
094300050111      * Controllo esistenza codice
094400050111if  3c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
094500050111     c                   Move      v1cksu        w0070
094600050111     c                   ExSr      Sr_ChkCli
094700050111if  4c                   If        O69err = *On
094800050111     c                   Eval      *In28 = *On
094900050111     c                   Eval      *In44 = *On
095000050111     c                   Eval      vidmsg = msg(03)
095100050111     c                   Leavesr
095200050111e   4c                   EndIf
095300050111     c                   Movel     AcoRag        v1dksu
095400050111      * Codice gestibile dall'utente
095500050308     c**!!!              Eval      w003a = %subst(v1cksu:2:3)
095600050308     c**!!!w003a         Lookup    skpog                                  31
095700050308if  4c**!!!              If        Not *In31
095800050308     c**!!!              Eval      *In28 = *On
095900050308     c**!!!              Eval      *In44 = *On
096000050308     c**!!!              Eval      vidmsg = msg(13)
096100050308     c**!!!              Leavesr
096200050308e   4c**!!!              EndIf
096300050111      * non deve essere figlio di altro papà
096400050111     c                   Clear                   Tibs10ds
096500050111     c                   Eval      d10tle = 'WW'
096600050111     c                   Eval      d10paf = 'P'
096700050111     c                   Move      v1cksu        d10cod
096800050111     c                   Eval      d10drf = *date
096900050111     c                   Call      'TIBS10R'
097000050111     c                   Parm                    Tibs10ds
097100050111if  4c                   If        d10err = *Blanks
097200050111     c                   Eval      *In28 = *On
097300050111     c                   Eval      *In44 = *On
097400050111     c                   Eval      vidmsg = msg(09)
097500050111     c                   Move      d10cop        w008a
097600050111     c                   Eval      %subst(vidmsg:36:8) = w008a
097700050111     c                   Leavesr
097800050111e   4c                   EndIf
097900050111      * deve essere già abilitato ai servizi ON LINE
098000050111     c                   Move      *all'0'       vssksu
098100050111     c                   Move      v1cksu        vssksu
098200050111     c                   Eval      vssisv = 'TT'
098300050111     c     kTivss        Chain(n)  Tivss02l
098400050111if  4c                   If        Not %Found(Tivss02l)
098500050111     c                   Eval      *In28 = *On
098600050111     c                   Eval      *In44 = *On
098700050111     c                   Eval      vidmsg = msg(11)
098800050111     c                   Leavesr
098900050111e   4c                   EndIf
099000050316if  4c**!!!              If        wksu = *Blanks or wksu = *Zeros
099100050316     c                   If        wksu <> v1cksu
099200050111     c                   Eval      wksu = v1cksu
099300050111     c                   Eval      *In90 = *On
099400050112e   4c                   EndIf
099500050111     c                   Leavesr
099600050111e   3c                   EndIf
099700050316      * se il codice padre non è inserito ma prima ne avevo inserito uno devo
099800050316      * pulire i dati del responsabile e dell'e-mail
099900050316   x2c                   Else
100000050316if  3c                   If        wksu <> *Zeros and wksu <> *Blanks
100100050316     c                   Clear                   wksu
100200050316     c                   Clear                   v1cksu
100300050316     c                   Clear                   v1dksu
100400050316     c                   Clear                   v1crsp
100500050316     c                   Clear                   v1cem1
100600050316     c                   Clear                   v1cem2
100700050316     c                   Eval      *In04 = *Off
100800091029     c                   Eval      *In07 = *Off
100900050316    3c                   EndIf
101000050111e   2c                   EndIf
101100050111      * Responsabile
101200050111if  2c                   If        v1crsp = *Blanks
101300050111     c                   Eval      *In28 = *On
101400050111     c                   Eval      *In41 = *On
101500050111     c                   Eval      vidmsg = msg(06)
101600050111     c                   Leavesr
101700050111e   2c                   EndIf
101800050111      * E-mail
101900050111if  2c                   If        v1cem1 = *Blanks and v1cem2 = *Blanks
102000050111     c                   Eval      *In28 = *On
102100050111     c                   Eval      *In42 = *On
102200050111     c                   Eval      vidmsg = msg(07)
102300050111     c                   Leavesr
102400050111e   2c                   EndIf
102500050111     c                   Clear                   dsemail
102600050111     c                   Eval      §emlindi = v1cem1 + v1cem2
102700050111     c                   Call      'XEMAIL'
102800050111     c                   Parm                    dsemail
102900050111if  2c                   If        §emlerro = '1'
103000050111     c                   Eval      *In28 = *On
103100050111     c                   Eval      *In42 = *On
103200050111     c                   Eval      vidmsg = msg(08)
103300050111     c                   Leavesr
103400050111e   2c                   EndIf
103500050111     c                   Eval      v1cem1 = %subst(§emlindo:1:49)
103600050111     c                   Eval      v1cem2 = %subst(§emlindo:50:49)
103700050111e   1c                   EndIf
103800050111
103900050111     c                   EndSr
104000101006
104100101006      /free
104200101006       //--------------------------------------------------------------
104300101006       //?Reinvio password a cliente abilitato.
104400101006       //--------------------------------------------------------------
104500101006       BEGSR sr_Password;
104600101006
104700120126       //?Come prima cosa verifico che non ci siano più abilitazioni per
104800120126       //?lo stesso unificante
104900120126         clear w_Conta;
105000120126         clear w_VSSsun;
105100120126         setll (VSSksu : VSSisv) TIVSS02L;
105200120126         reade(n) (VSSksu : VSSisv) TIVSS02L;
105300120126         DOW  not %eof(TIVSS02L);
105400120126           IF  w_VSSsun <> VSSsun;
105500120126             w_VSSsun = VSSsun;
105600120126             w_Conta += 1;
105700120126           ENDIF;
105800120126           reade(n) (VSSksu : VSSisv) TIVSS02L;
105900120126         ENDDO;
106000120126         IF  w_Conta > 1;
106100120126           *in28 = *on;
106200120126           *in90 = *on;
106300120126           VIDmsg = msg(18);
106400120126           leavesr;
106500120126         ENDIF;
106600120126
106700101006         $Fine = *off;
106800101019         w03cem1 = v1cem1;
106900101019         w03cem2 = v1cem2;
107000101019         w03crsp = v1crsp;
107100101006       //?Emetto la window
107200101006         DOW  not $Fine;
107300101006           exfmt TA8203W;
107400101006           *in28 = *off;
107500101006           clear w03msg;
107600101006           SELECT;
107700101006             WHEN  *inkf;
107800101018               exsr CtrW03;
107900101018               IF  *in28;
108000101018                 iter;
108100101018               ENDIF;
108200101006               exsr sr_newpsw;
108300101006               IF  not *in28;
108400101006                 $Fine = *on;
108500101006               ENDIF;
108600101006             WHEN  *inkl;
108700101006               *in90 = *on;
108800101006               $Fine = *on;
108900101018             OTHER;
109000101018               exsr CtrW03;
109100101006           ENDSL;
109200101006         ENDDO;
109300101006
109400101006       ENDSR;
109500101018
109600101018       //--------------------------------------------------------------
109700101018       //?Controllo i dati della window.
109800101018       //--------------------------------------------------------------
109900101018       BEGSR CtrW03;
110000101019
110100101019         *in41 = *off;
110200101019         *in42 = *off;
110300101018
110400101018       // E-mail
110500101019         IF  w03cem1 = *blanks and w03cem2 = *blanks;
110600101018           *in28 = *on;
110700101018           *in42 = *on;
110800101018           w03msg = msg(07);
110900101018           leavesr;
111000101018         ENDIF;
111100101018      /end-free
111200101018     c                   Clear                   dsemail
111300101019     c                   Eval      §emlindi = w03cem1 + w03cem2
111400101018     c                   Call      'XEMAIL'
111500101018     c                   Parm                    dsemail
111600101018if  2c                   If        §emlerro = '1'
111700101018     c                   Eval      *In28 = *On
111800101018     c                   Eval      *In42 = *On
111900101018     c                   Eval      w03msg = msg(08)
112000101018     c                   Leavesr
112100101018e   2c                   EndIf
112200101018      /free
112300101019       w03cem1 = %subst(§emlindo:1:49);
112400101019       w03cem2 = %subst(§emlindo:50:49);
112500101019
112600101019       // Responsabile
112700101019         IF  w03crsp = *blanks;
112800101019           *in28 = *on;
112900101019           *in41 = *on;
113000101019           w03msg = msg(06);
113100101019           leavesr;
113200101019         ENDIF;
113300101018
113400101018       ENDSR;
113500101006      /end-free
113600041117
113700041118      *------------------------------------------------------------------------*
113800001011      * F6 - Conferma
113900050110      *------------------------------------------------------------------------*
114000050110     c     Sr_Conferma   BegSr
114100050112
114200050112     c                   Eval      *In90 = *Off
114300100408
114400100408      /free
114500100408       //?Devo togliere gli apici dal campo V1CRSP
114600100408       //?richiesto da Fabrizio
114700100408       clear wpos;
114800100408       wpos = %scan(api:v1crsp);
114900100408       DOW  wpos > 0;
115000100408         v1crsp = %replace(' ':v1crsp:wpos);
115100100408         clear wpos;
115200100408         wpos = %scan(api:v1crsp);
115300100408       ENDDO;
115400100408      /end-free
115500050110
115600050110      * --> Pgm non richiamato
115700050110if  1c                   If        Not *In01
115800050110
115900050110      * Papà già abilitato e sono stati inseriti dei nuovi figli
116000050111if  2c                   If        *In04 and *In08
116100050110      * Aggiorno il legame WW su TIVSS se non aveva già dei figli
116200050110if  3c                   If        skfig(01) = *Zeros
116300050110     c                   Move      *all'0'       vssksu
116400050111     c   07              Move      v1cksu        vssksu
116500050111     c  n07              Move      v1ccli        vssksu
116600050110     c                   Eval      vssisv = 'TT'
116700050110     c     kTivss        Chain     Tivss02l
116800050110if  4c                   If        %Found(Tivss02l)
116900050110     c                   Eval      vsstle = 'WW'
117000050110     c                   Update    Tivss000
117100050110e   4c                   EndIf
117200050110e   3c                   EndIf
117300050110      * Scrivo i nuovi legami su TIKUN
117400050111     c                   Clear                   nrr
117500050110do  3c                   Do        *Hival
117600050110     c                   Eval      nrr = nrr +1
117700050110     c     nrr           Chain     ta8201s                            30
117800050110     c                   If        *In30
117900050110     c                   Leave
118000050110     c                   EndIf
118100050110      * Se non è un codice protetto
118200050110if  4c                   If        wprot = *Blanks and v1scod <> *Zeros
118300050110      * Scrivo TIKUN
118400050110     c                   Clear                   Tikun000
118500050111     c   07              Move      v1cksu        kuncop
118600050111     c  n07              Move      v1ccli        kuncop
118700050110     c                   Move      v1scod        kuncof
118800050110     c                   Eval      kuntle = 'WW'
118900050110     c                   Eval      kundde = wdatai
119000050110     c                   Eval      kundsc = wdataf
119100130412     C                   EVAL      KUNDIM = %dec(%date())
119200130412     C                   EVAL      KUNDUV = %dec(%date())
119300130412     C                   EVAL      KUNPUV = SDSUser
119400050110     c                   Write     Tikun000
119500050110e   4c                   EndIf
119600050110e   3c                   EndDo
119700050110e   2c                   EndIf
119800050111      * Codice nuovo da abilitare
119900050114if  2c                   If        Not *In04 and was888 = *Off
120000050110      * Abilito il codice richiamando il TIS730R
120100050110     c                   Clear                   Tis730ds
120200050110     c                   Eval      s30ric = 'S'
120300101006     c                   eval      s30opz = 'II'
120400101230     c   07              move      v1cksu        s30ksu
120500101230     c  n07              Eval      s30ksu = v1ccli
120600050110     c                   Eval      s30rsp = v1crsp
120700050110     c                   Eval      s30eml = v1cem1 + v1cem2
120800050110     c                   Eval      s30isv = 'TT'
120900050119     c                   Eval      s30dti = *date
121000050110     c                   Eval      s30dtf = wdataf
121100060905      * controllo chiodato perchè è successo che sia stato abilitato un cliente
121200060905      * con codice zero
121300060905      * non so se è colpa mia o di Gurro!!!!
121400060905     c                   if        s30ksu = 0
121500060905     c                   eval      vidmsg = 'Manca codice cliente telefonare al-
121600060905     c                                       CED in Sede!!'
121700060905     c                   eval      *in28 = *on
121800060905     c                   leavesr
121900060905     c                   endif
122000050110     c                   Call      'TIS730R'
122100050110     c                   Parm                    Kpjba
122200050110     c                   Parm                    Tis730ds
122300050112if  3c                   If        s30err <> *Blanks
122400101006if  4c                   If        s30err = '08'
122500050111     c                   Eval      vidmsg = s30msg
122600050112   x4c                   Else
122700050112     c                   Eval      vidmsg = msg(15)
122800050112e   4c                   EndIf
122900050111     c                   Eval      *In28 = *On
123000050111     c                   Leavesr
123100050112e   3c                   EndIf
123200110223      * Se sono stati inseriti dei figli
123300110223if  3c                   If        *In08
123400050110     c                   Move      *all'0'       vssksu
123500101230     c   07              Move      v1cksu        vssksu
123600101230     c  n07              Move      v1ccli        vssksu
123700050110     c                   Eval      vssisv = 'TT'
123800050110     c     kTivss        Chain     Tivss02l
123900050110if  4c                   If        %Found(Tivss02l)
124000050110     c                   Eval      vsstle = 'WW'
124100050110     c                   Update    Tivss000
124200050110e   4c                   EndIf
124300050110      * Scrivo i nuovi legami su TIKUN
124400050111     c                   Clear                   nrr
124500050111do  4c                   Do        *Hival
124600050110     c                   Eval      nrr = nrr +1
124700050110     c     nrr           Chain     ta8201s                            30
124800050110     c                   If        *In30
124900050110     c                   Leave
125000050110     c                   EndIf
125100101230if  5c**!!!              If        v1scod <> *Zeros
125200101230if  4c                   If        wprot = *Blanks and v1scod <> *Zeros
125300050110      * Scrivo TIKUN
125400050110     c                   Clear                   Tikun000
125500101230     c   07              Move      v1cksu        kuncop
125600101230     c  n07              Move      v1ccli        kuncop
125700101230     c**!!!              Move      v1ccli        kuncop
125800050110     c                   Move      v1scod        kuncof
125900050110     c                   Eval      kuntle = 'WW'
126000050110     c                   Eval      kundde = wdatai
126100050110     c                   Eval      kundsc = wdataf
126200130412     C                   EVAL      KUNDIM = %dec(%date())
126300130412     C                   EVAL      KUNDUV = %dec(%date())
126400130412     C                   EVAL      KUNPUV = SDSUser
126500050110     c                   Write     Tikun000
126600050111e   5c                   EndIf
126700050111e   4c                   EndDo
126800101230e   3c                   EndIf
126900050111e   2c                   EndIf
127000050110
127100050111     c                   Goto      Inz01
127200050110e   1c                   EndIf
127300050110
127400050111      * --> Pgm richiamato
127500050111if  1c                   If        *In01
127600050112      * Richiesta l'abilitazione del cliente
127700050111if  2c                   If        v1cabi = 'S'
127800050111      * Immesso il papà
127900050111if  3c                   If        *In07
128000050112      * Aggiorno il legame WW su TIVSS
128100050111     c                   Move      *all'0'       vssksu
128200050112     c                   Move      v1cksu        vssksu
128300050111     c                   Eval      vssisv = 'TT'
128400050111     c     kTivss        Chain     Tivss02l
128500050111if  4c                   If        %Found(Tivss02l)
128600050111     c                   Eval      vsstle = 'WW'
128700050111     c                   Update    Tivss000
128800050111e   4c                   EndIf
128900050111      * Scrivo i nuovi legami su TIKUN
129000050111      * Scrivo TIKUN
129100050111     c                   Clear                   Tikun000
129200050111     c                   Move      v1cksu        kuncop
129300050111     c                   Move      v1ccli        kuncof
129400050111     c                   Eval      kuntle = 'WW'
129500050111     c                   Eval      kundde = wdatai
129600050111     c                   Eval      kundsc = wdataf
129700130412     C                   EVAL      KUNDIM = %dec(%date())
129800130412     C                   EVAL      KUNDUV = %dec(%date())
129900130412     C                   EVAL      KUNPUV = SDSUser
130000050111     c                   Write     Tikun000
130100050111e   3c                   EndIf
130200050111      * Codice nuovo da abilitare
130300050114if  3c                   If        Not *In07 and was888 = *Off
130400050111      * Abilito il codice richiamando il TIS730R
130500050111     c                   Clear                   Tis730ds
130600050111     c                   Eval      s30ric = 'S'
130700101006     c                   eval      s30opz = 'II'
130800050111     c                   Eval      s30ksu = v1ccli
130900050111     c                   Eval      s30rsp = v1crsp
131000050111     c                   Eval      s30eml = v1cem1 + v1cem2
131100050111     c                   Eval      s30isv = 'TT'
131200050119     c                   Eval      s30dti = *date
131300050111     c                   Eval      s30dtf = wdataf
131400050111     c                   Call      'TIS730R'
131500050111     c                   Parm                    Kpjba
131600050111     c                   Parm                    Tis730ds
131700050112if  4c                   If        s30err <> *Blanks
131800101006if  5c                   If        s30err = '08'
131900050112     c                   Eval      vidmsg = s30msg
132000050112   x5c                   Else
132100050112     c                   Eval      vidmsg = msg(15)
132200050112e   5c                   EndIf
132300050111     c                   Eval      *In28 = *On
132400050111     c                   Leavesr
132500050112e   4c                   EndIf
132600050111e   3c                   EndIf
132700050111e   2c                   EndIf
132800050111     c                   Goto      Fine
132900050111e   1c                   EndIf
133000050110
133100050110     c                   EndSr
133200101006
133300101006      /free
133400101006       //--------------------------------------------------------------
133500101006       //?Nuova password a cliente abilitato.
133600101006       //--------------------------------------------------------------
133700101006       BEGSR sr_newpsw;
133800101018
133900101018       //?Devo togliere gli apici dal campo V1CRSP
134000101018       //?richiesto da Fabrizio
134100101018       clear wpos;
134200101019       wpos = %scan(api:w03crsp);
134300101018       DOW  wpos > 0;
134400101019         w03crsp = %replace(' ':w03crsp:wpos);
134500101018         clear wpos;
134600101019         wpos = %scan(api:w03crsp);
134700101018       ENDDO;
134800101006
134900101006       //?Richiamo pgm per creare nuova password
135000101006       //?solo se sono su sistema di produzione
135100101006         IF  was888 = *off;
135200101006           clear TIS730DS;
135300101006           S30ric = 'S';
135400101018           S30opz = 'UU';
135500101006           S30sun = VSSsun;
135600101006           S30ksu = %dec(VSSksu:8:0);
135700101019           S30rsp = w03crsp;
135800101019           S30eml = w03cem1 + w03cem2;
135900101006           S30isv = 'TT';
136000101006           S30dti = VSSdde;
136100101006           S30dtf = VSSdsc;
136200101006           tis730r (kpjba : TIS730DS);
136300101006           SELECT;
136400101006             WHEN  S30err = '16';
136500101006             WHEN  S30err = '08';
136600101006               w03msg = S30msg;
136700101006               *in28 = *on;
136800101006             WHEN  S30err <> *blanks;
136900101006               w03msg = msg(15);
137000101006               *in28 = *on;
137100101006           ENDSL;
137200101006         ENDIF;
137300101006
137400101006       ENDSR;
137500101006      /end-free
137600050110
137700050110      *------------------------------------------------------------------------*
137800050110      * ROUTINE INIZIALE
137900050110      *------------------------------------------------------------------------*
138000050110     c     *Inzsr        BegSr
138100050110
138200050110     c     *Entry        Plist
138300050110     c                   Parm                    Kpjba
138400050103     c                   Eval      Param = Kpjbu
138500050103
138600050103     c                   Eval      *In01 = (parflg <> *Blanks)
138700050110
138800050110     c     *dtaara       define    §azute        azuteds
138900050110     c     *dtaara       define    §datiute      ddatiute
139000050110     c                   in(E)     *dtaara
139100050110     c                   If        %error  or RSUT = *blanks
139200050110     c                   Clear                   Tibs34ds
139300050110     c                   Call      'TIBS34R'
139400050110     c                   Parm                    Tibs34ds
139500050110     c                   In        *dtaara
139600050110     c                   EndIf
139700050110
139800050110     c                   Clear                   wabi
139900050110     c                   Clear                   dLat
140000050110
140100050110      * Verifica errori e autorità profilo
140200050110s   1c                   Select
140300050110      * se ho errori nei dati utente esco dal pgm
140400050110w   1c                   When      DutErr = 'E'
140500050110     c                   GoTo      Fine
140600050110      * se non c'è l'abilitazione
140700050110      * --> se 1° livello, abilitazioni al terminal
140800050110      *     se 2° livello, abilitazioni al punto operativo
140900050110      *     se sede è impossibile ma se capita mando a fine il pgm
141000050110w   1c                   When      UteAut = *Blanks
141100050110if  2c                   If        DutLpo = '1'
141200050110     c                   Eval      wabi   = 'TP'
141300050110e   2c                   EndIf
141400050110if  2c                   If        DutLpo = '2'
141500050110     c                   Eval      wabi   = 'PO'
141600050110e   2c                   EndIf
141700050110if  2c                   If        DutLpo = 'S'
141800050110     c                   GoTo      Fine
141900050110e   2c                   EndIf
142000050110      * carica le abilitazioni del profilo
142100050110x   1c                   Other
142200050110     c                   Movel     UteFaf        Dute01
142300050110if  2c                   If        §UteCli <> *Blanks
142400050110     c                   Eval      wabi = §UteCli
142500050110   x2c                   Else
142600050110     c                   Eval      wabi = UteAut
142700050110e   2c                   EndIf
142800050110e   1c                   EndSl
142900050110
143000050110      * controllo se ok l'abilitazione dell'utente
143100050110     c                   Clear                   Tibs02ds
143200050110     c                   Eval      T02Mod = 'C'
143300050110     c                   Eval      T02Sif = knsif
143400050110     c                   Eval      T02Cod = 'LAT'
143500050110     c                   Movel(p)  wabi          T02Ke1
143600050110     c                   Call      'TIBS02R'
143700050110     c                   Parm                    kpjba
143800050110     c                   Parm                    Tibs02ds
143900050110if  1c                   If        T02Err = *Blanks
144000050110     c                   Eval      dLat = T02Uni
144100050110e   1c                   EndIf
144200050110      * errore o non abilitato
144300050110if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
144400050103     c                   Eval      *In02 = *On
144500050110     c                   Eval      *In28 = *On
144600050104     c                   Eval      vidmsg = msg(01)
144700050110e   1c                   EndIf
144800050110
144900050110      * Reperimento dei P.O. gestibili dall'utente
145000050110if  1c                   If        Not *In28
145100050110     c                   Clear                   Trul31ds
145200050110     c                   Eval      I31abi = wabi
145300050110     c                   Eval      I31cdi = DUTdis
145400050110     c                   Eval      I31car = DUTare
145500050110     c                   Eval      I31cpo = DUTpou
145600050110     c                   Call      'TRUL31R'
145700050110     c                   Parm                    KPJBA
145800050110     c                   Parm                    Trul31ds
145900050110if  2c                   If        O31pog > *zeros
146000050110     c                   Movea     O31pog        skpog
146100050110x   2c                   Else
146200050103     c                   Eval      *In02 = *On
146300050110     c                   Eval      *In28 = *On
146400050104     c                   Eval      vidmsg = msg(01)
146500050110e   2c                   EndIf
146600050110e   1c                   EndIf
146700101018
146800050110
146900050110      * Se è ambiente prova GAITRAGRPS
147000050110     c                   If        %subst(knsif:7:1) = 'P'
147100050114     c                   Eval      was888 = *On
147200050110      * Se non è ambiente prova GAITRAGRU
147300050110     c                   Else
147400050114     c                   Eval      was888 = *Off
147500050110     c                   EndIf
147600050110
147700050110      * Calcolo oggi -60 gg.
147800050110     c     *iso          Move      *date         dataiso
147900050110     c                   subdur    60:*d         dataiso
148000050110     c                   Move      dataiso       wdatai
148100050110     c                   Move      20391231      wdataf
148200050110
148300050110      * KLIST
148400050104     c     kTiabl        klist
148500050104     c                   kfld                    vsssun
148600050104     c                   kfld                    vssisv
148700050104     c                   kfld                    abltip
148800050104
148900050104     c     kTivss        klist
149000050104     c                   kfld                    vssksu
149100050104     c                   kfld                    vssisv
149200050110
149300050110     c                   EndSr
149400050110      *------------------------------------------------------------------------*
149500050110
149600050110** MSG  Lungh. 78                                                            *
149700050110Utente non autorizzato all'Abilitazione clienti ai servizi ON LINE            01
149800050110Immettere il Cliente                                                          02
149900050103Codice Cliente errato                                                         03
150000050104ATTENZIONE!! Cliente già abilitato                                            04
150100050309ATTENZIONE!! Cliente con abilitazione scaduta avvisare il CED!!!!!            05
150200050309Immettere il Responsabile password                                            06
150300050309Immettere l'indirizzo e-mail x invio password                                 07
150400050104Indirizzo e-mail errato                                                       08
150500050104Il codice inserito è già figlio di                                            09
150600050105Il codice inserito è un padre                                                 10
150700050105Il codice padre non è abilitato ai servizi ON LINE                            11
150800050110Codice già presente                                                           12
150900050110Codice Cliente non gestibile dall'utente                                      13
151000050111ATTENZIONE!! Non sono state effettuate modifiche                              14
151100050112ATTENZIONE!! Non è possibile abilitare il cliente... RIPROVARE PIU' TARDI     15
151200091029Codice figlio uguale al codice da abilitare                                   16
151300101007ATTENZIONE!! Modifiche in sospeso, prima fare F6=Conferma                     17
151400120126Presenti più abilitazioni sullo stesso cliente. CONTATTARE IL CED!!!          18
