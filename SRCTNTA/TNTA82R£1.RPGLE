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
008700050103
008800050103     d Azuteds       e ds                  Extname(Azute00f)
008900050103     d dDatiute      e ds
009000050103     d dLat          e ds
009100050103     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
009200050103     d ds_cnind      e ds                  inz  extname(Cnind00f)
009300050103     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
009400050103     d ds_fncls      e ds                  inz  extname(Fncls00f)
009500050103     d dsemail       e ds
009600050110     d dute01        e ds
009700050103     d Kpjba         e ds
009800050103     d Tibs02ds      e ds
009900050104     d Tibs10ds      e ds
010000050104     d  skfigli               21   5520  0 dim(500)
010100050103     d Tibs34ds      e ds
010200050103     d Tibs69ds      e ds
010300050111     d Tis730ds      e ds
010400050103     d Trul31ds      e ds
010500050104     d Xclirds       e ds
010600101006
010700101006      //---------------------------------------------------------------
010800101006      //?Definizione procedure usate.
010900101006      //---------------------------------------------------------------
011000101006     d tis730r         pr                  extpgm('TIS730R')
011100101006     d  kpjba                              likeds(kpjba)
011200101006     d  tis730ds                           likeds(tis730ds)
011300101006
011400101006      //---------------------------------------------------------------
011500050103      *   C O S T A N T I
011600050103      * titolo videata (lunghezza massima 36)
011700050103     d VtcTit          C                   CONST('ABILITAZIONE CLIENTI SERVIZI -
011800050103     d                                     ON LINE')
011900050103
012000050103      *------------------------------------------------------------------------*
012100050103
012200050103      * Se pgm richiamato salto la prima videata
012300050103     c   01              ExSr      Sr_Car02d
012400050103     c   01              Goto      Inz02
012500050111
012600050111      * Gestione da Menù
012700050111     c     Inz01         Tag
012800050103
012900050308     c  n28
013000050308     cann90              Clear                   vidksu
013100050308     c  n90              Clear                   vddksu
013200050316     c                   Eval      *In07 = *Off
013300050103
013400050103     c                   Exfmt     Ta8201d
013500050104     c                   Eval      *In28 = *Off
013600050104     c                   Clear                   vidmsg
013700050103      * F3=Fine
013800050103     c                   If        *InKc or
013900050103      * 02=utente non abilitato
014000050103     c                             *In02
014100050103     c                   Goto      Fine
014200050103     c                   EndIf
014300050103
014400050103      * Controllo
014500050103     c                   ExSr      Sr_Ctr01d
014600050111     c   28
014700050111     cor 90              Goto      Inz01
014800050111     c                   Eval      wksu = vidksu
014900050104      * Ulteriori controlli sul codice inserito
015000050104      * se papà o figlio se abilitato lui o il suo papà
015100050111     c     Carsfl        Tag
015200050104     c                   ExSr      Sr_Ctrksu
015300050104      * Pulisco subfile
015400050103     c                   ExSr      Sr_Pulsfl
015500050103      * Carico il subfile
015600050103     c                   ExSr      Sr_Carsfl
015700050104
015800050104      * Se cliente già abilitato o scaduto emetto msg
015900050309     c                   If        *In04 and Not *In05 and Not *In90
016000050309     c                   Eval      *In28 = *On
016100050309     c                   Eval      vidmsg = msg(04)
016200050111     c   07              Eval      *In44 = *On
016300050111     c  n07              Eval      *In45 = *On
016400050104     c                   EndIf
016500050104     c                   If        *In05
016600050104     c                   Eval      *In28 = *On
016700050104     c                   Eval      vidmsg = msg(05)
016800050111     c   07              Eval      *In44 = *On
016900050111     c  n07              Eval      *In45 = *On
017000050104     c                   EndIf
017100050104
017200050111     c     Emisfl        Tag
017300050104      * Emetto il Subfile
017400050104     c                   ExSr      Sr_Emisfl
017500050104      * F3=Fine
017600050104     c   kc              Goto      Fine
017700050104      * F12 - Torno alla videata precedente
017800050104     c                   If        *InKl
017900050104     c                   Eval      *In28 = *Off
018000050104     c                   Clear                   vidmsg
018100050104     c                   Goto      Inz01
018200050104     c                   EndIf
018300050104      * Controllo dati
018400050104     c                   ExSr      Sr_CtrSfl
018500050111     c   90              Goto      Carsfl
018600101006     c                   If        *in28 or (not *inkf and
018700101006     c                             not *inkh)
018800101006     c                   Goto      Emisfl
018900101006     c                   endif
019000101006      * Reinvio password
019100101006     c                   If        *inkh
019200101006     c                   ExSr      Sr_Password
019300101006     c   90              Goto      Emisfl
019400101006     c                   endif
019500050110      * Conferma
019600050110     c   kf              ExSr      Sr_Conferma
019700050111     c   28              Goto      Emisfl
019800050104
019900050111      * Gestione da Offerta
020000050103     c     Inz02         Tag
020100050111if  1c                   If        *In01
020200050111do  2c   90              Do
020300050316     c                   Eval      *In07 = *Off
020400050111     c                   ExSr      Sr_Ctrksu
020500050111      * Se cliente scaduto emetto msg
020600050111     c                   If        *In05
020700050111     c                   Eval      *In28 = *On
020800050111     c                   Eval      vidmsg = msg(05)
020900050111     c                   Eval      *In44 = *On
021000050111     c                   EndIf
021100050111e   2c                   EndDo
021200050111
021300050112     c                   Exfmt     Ta8202d
021400050103     c                   Eval      *In28 = *Off
021500050111
021600050111     c                   ExSr      Sr_Ctr02d
021700050111     c   28
021800050112     cor 90
021900050112     cornkf              Goto      Inz02
022000050111
022100050111     c   kf              ExSr      Sr_Conferma
022200050111     c   28              Goto      Inz02
022300050111
022400050111e   1c                   EndIf
022500050103
022600050103     c     Fine          Tag
022700050112
022800050112      * Chiude i file dei pgm richiamati
022900050112     c                   If        wtibs69 = *On
023000050112     c                   Eval      i69tla = 'C'
023100050112     c                   Call      'TIBS69R'
023200050112     c                   Parm                    Tibs69ds
023300050112     c                   EndIf
023400050103
023500050103     c                   Eval      *InLr = *On
023600050103
023700050103      *------------------------------------------------------------------------*
023800050111      * CONTROLLO LA VIDEATA PER GESTIONE DA MENU
023900050103      *------------------------------------------------------------------------*
024000050103     c     Sr_Ctr01d     BegSr
024100050104
024200050104     c                   Clear                   vddksu
024300050111     c                   Eval      *In90 = *Off
024400050309     c                   Eval      *In09 = *Off
024500050103
024600050104      * Codice obbligatorio
024700050104if  1c                   If        vidksu = *Zeros or vidksu = *Blanks
024800050103     c                   Eval      *In28 = *On
024900050103     c                   Eval      *In40 = *On
025000050104     c                   Eval      vidmsg = msg(02)
025100050103     c                   Leavesr
025200050103e   1c                   EndIf
025300050104
025400050104if  1c                   If        vidksu > '09999999'
025500050104     c                   Eval      *In28 = *On
025600050104     c                   Eval      *In40 = *On
025700050104     c                   Eval      vidmsg = msg(03)
025800050104     c                   Leavesr
025900050104e   1c                   EndIf
026000050104
026100050104      * Ricerca
026200050310      * forzo autorizzazione AZ in modo da visualizzare tutti i clienti
026300050111     c     '?'           Scan      vidksu                                 31
026400050111if  1c                   If        *In31
026500050104     c                   Clear                   Xclirds
026600050104     c                   Eval      DxcCap = DutKci
026700050310     c                   Eval      DxcAut = 'AZ'
026800050104     c                   Call      'XCLIR'
026900050104     c                   Parm                    Xclirds
027000050104     c                   Movea     DxcKsc        ksa
027100050104     c                   move      ksa(1)        dsksa
027200050104     c                   z-add     dsksn         ksc(1)
027300050104     c                   Move      *all'0'       vidksu
027400050104     c                   Move      ksc(1)        vidksu
027500050111     c                   Eval      *In90 = *On
027600050104e   1c                   EndIf
027700050104
027800050104      * Solo codici numerici
027900050104if  1c                   If        vidksu < *Zeros
028000050104     c                   Eval      *In28 = *On
028100050104     c                   Eval      *In40 = *On
028200050104     c                   Eval      vidmsg = msg(03)
028300050104     c                   Leavesr
028400050104e   1c                   EndIf
028500050103
028600050104      * Controllo esistenza codice
028700050104     c                   Move      vidksu        w0070
028800050104     c                   ExSr      Sr_ChkCli
028900050104if  1c                   If        O69err = *On
029000050104     c                   Eval      *In28 = *On
029100050104     c                   Eval      *In40 = *On
029200050104     c                   Eval      vidmsg = msg(03)
029300050104     c                   Leavesr
029400050104e   1c                   EndIf
029500050104     c                   Movel     AcoRag        vddksu
029600050110
029700050110      * Codice gestibile dall'utente
029800050308     c**!!!              Eval      w003a = %subst(vidksu:2:3)
029900050308     c**!!!w003a         Lookup    skpog                                  31
030000050308if  1c**!!!              If        Not *In31
030100050308     c**!!!              Eval      *In28 = *On
030200050308     c**!!!              Eval      *In40 = *On
030300050308     c**!!!              Eval      vidmsg = msg(13)
030400050308     c**!!!              Leavesr
030500050308e   1c**!!!              EndIf
030600050103
030700050103     c                   EndSr
030800050104
030900050104      *------------------------------------------------------------------------*
031000050104      * CONTROLLO IL CODICE INSERITO ABILITAZIONI PAPA/FIGLIO
031100050104      *------------------------------------------------------------------------*
031200050104     c     Sr_CtrKsu     BegSr
031300050104
031400050104     c                   Eval      *In03 = *Off
031500050104     c                   Eval      *In04 = *Off
031600050104     c                   Eval      *In05 = *Off
031700050111
031800050111      * se gestione da menù
031900050111if  0c                   If        Not *In01
032000050111     c  n90              Clear                   v1ccli
032100050111     c  n90              Clear                   v1dcli
032200050111     c  n90              Clear                   v1cksu
032300050111     c  n90              Clear                   v1dksu
032400050104     c                   Clear                   v1crsp
032500050104     c                   Clear                   v1cem1
032600050104     c                   Clear                   v1cem2
032700050104     c                   Clear                   skfig
032800050104
032900050104      * controllo se il codice inserito è un papà
033000050104     c                   Clear                   Tibs10ds
033100050104     c                   Eval      d10tle = 'WW'
033200050104     c                   Eval      d10paf = 'F'
033300050111     c                   move      wksu          d10cod
033400050104     c                   Eval      d10drf = *date
033500050104     c                   Call      'TIBS10R'
033600050104     c                   Parm                    Tibs10ds
033700050104if  1c                   If        d10err = *Blanks
033800050111     c  n90              Eval      *In03 = *On
033900050105     c                   Eval      *In07 = *On
034000050104     c                   Move      *all'0'       v1cksu
034100050104     c                   Move      d10cop        v1cksu
034200050104     c                   Movea     skfigli       skfig
034300050104   x1c                   Else
034400050104      * controllo se il codice inserito è un figlio
034500050104     c                   Clear                   Tibs10ds
034600050104     c                   Eval      d10tle = 'WW'
034700050104     c                   Eval      d10paf = 'P'
034800050111     c                   Move      wksu          d10cod
034900050104     c                   Eval      d10drf = *date
035000050104     c                   Call      'TIBS10R'
035100050104     c                   Parm                    Tibs10ds
035200050104if  2c                   If        d10err = *Blanks
035300050105     c                   Eval      *In07 = *On
035400050104     c                   Move      *all'0'       v1cksu
035500050104     c                   Move      d10cop        v1cksu
035600050104     c                   Move      vidksu        v1ccli
035700050104     c                   Movea     skfigli       skfig
035800050104   x2c                   Else
035900050110      * non è papà e non è figlio
036000050105     c                   Move      vidksu        v1ccli
036100050104e   2c                   EndIf
036200050104e   1c                   EndIf
036300050111      * se gestione da offerta con papà inserito
036400050111   x0c                   Else
036500050111     c                   Eval      *In07 = *On
036600050111e   0c                   EndIf
036700050111
036800050104      * decodifico il figlio e il papà
036900050105if  1c                   If        v1ccli > *Zeros
037000050104     c                   Move      v1ccli        w0070
037100050104     c                   ExSr      Sr_ChkCli
037200050105if  2c                   If        O69err <> *On
037300050104     c                   Movel     AcoRag        v1dcli
037400050105e   2c                   EndIf
037500050105e   1c                   EndIf
037600050105if  1c                   If        v1cksu > *Zeros
037700050104     c                   Move      v1cksu        w0070
037800050104     c                   ExSr      Sr_ChkCli
037900050105if  2c                   If        O69err <> *On
038000050104     c                   Movel     AcoRag        v1dksu
038100050105e   2c                   EndIf
038200050105e   1c                   EndIf
038300050104
038400050104      * Controllo se il papà è già abilitato ai servizi ON LINE
038500050111      * oppure se il codice inserito è già abilitato
038600050104     c                   Move      *all'0'       vssksu
038700050111     c   07              Move      v1cksu        vssksu
038800050111     c  n07              Move      v1ccli        vssksu
038900050104     c                   Eval      vssisv = 'TT'
039000050110     c     kTivss        Chain(n)  Tivss02l
039100050104if  1c                   If        %Found(Tivss02l)
039200050104     c                   Eval      *In04 = *On
039300050104      * cerco il nominativo e l'e-mail
039400050104     c                   Eval      abltip = 'SG1'
039500050104     c     kTiabl        Chain     Tiabl01l
039600050104if  2c                   If        %Found(Tiabl01l)
039700050104     c                   Eval      v1crsp = AblRsp
039800050104     c                   Eval      v1cem1 = %subst(Ableml:1:49)
039900050104     c                   Eval      v1cem2 = %subst(Ableml:50:49)
040000050104e   2c                   EndIf
040100050104      * controllo se abilitazione ancora valida
040200050104     c                   Eval      *In05 =(*date > vssdsc)
040300050309      * se non è papà ma è già abilitato deve diventare papà
040400050309if  2c                   If        Not *In07
040500050309     c                   Eval      *In03 = *On
040600050309     c                   Eval      *In07 = *On
040700050309     c                   Move      v1ccli        v1cksu
040800050309     c                   Move      v1dcli        v1dksu
040900050309     c                   Clear                   v1ccli
041000050309     c                   Clear                   v1dcli
041100050309e   2c                   EndIf
041200050309   x1c                   Else
041300050309      * codice non abilitato e non papà
041400050309     c                   Eval      *In09 = *On
041500050104e   1c                   EndIf
041600050104
041700050104     c                   EndSr
041800050103
041900050103      *------------------------------------------------------------------------*
042000050103      * PULISCO IL SUBFILE
042100050103      *------------------------------------------------------------------------*
042200050103     c     Sr_Pulsfl     BegSr
042300050103
042400050103     c                   Clear                   nrr
042500050308     c                   Clear                   savnrr
042600050110     c                   Eval      *In20 = *Off
042700050110     c                   Eval      *In21 = *Off
042800050103     c                   Write     ta8201c
042900050103     c                   Eval      *In20 = *On
043000050103     c                   Eval      *In21 = *On
043100050103
043200050103     c                   EndSr
043300050104
043400050103      *------------------------------------------------------------------------*
043500050103      * CARICO IL SUBFILE
043600050103      *------------------------------------------------------------------------*
043700050103     c     Sr_Carsfl     BegSr
043800050103
043900050316      * se il codice immesso non è abilitato ed ho inserito un padre
044000050316      * carico il codice immesso nel subfile dei figli
044100050316     c                   If        *In09 and v1cksu <> *Blanks and
044200050316     c                             v1cksu <> *Zeros
044300101230     c                             and not *in07
044400050316     c                   Add       1             Nrr
044500050316     c     Nrr           Chain     ta8201s                            30
044600050316     c                   Move      v1ccli        v1scod
044700050316     c                   Move      v1scod        w0070
044800050316     c                   ExSr      Sr_ChkCli
044900050316     c                   Movel     AcoRag        v1sdes
045000050316     c  n30              Update    ta8201s
045100050316     c   30              Write     ta8201s
045200050316     c                   EndIf
045300050104      * Carico i figli
045400050309do  1c     2             Do        500           xx
045500050104     c                   Eval      *In06 = *Off
045600050104     c                   Clear                   wprot
045700050104     c                   Clear                   v1scod
045800050104     c                   Clear                   v1sdes
045900050104if  2c                   If        skfig(xx) = *Zeros
046000050104     c                   Leave
046100050104e   2c                   EndIf
046200050110     c                   Add       1             Nrr
046300050110     c     Nrr           Chain     ta8201s                            30
046400050104     c                   Eval      *In06 = *On
046500050104     c                   Eval      wprot = 'S'
046600050104     c                   Move      skfig(xx)     v1scod
046700050104     c                   Move      v1scod        w0070
046800050104     c                   ExSr      Sr_ChkCli
046900050104     c                   Movel     AcoRag        v1sdes
047000050110     c  n30              Update    ta8201s
047100050110     c   30              Write     ta8201s
047200050104e   1c                   EndDo
047300050104
047400050104     c                   EndSr
047500050104
047600050104      *------------------------------------------------------------------------*
047700050104      * EMETTO IL SUBFILE
047800050104      *------------------------------------------------------------------------*
047900050104     c     Sr_Emisfl     BegSr
048000101019
048100101019      * Controllo se posso abilitare il tasto F8=Reinvio Password
048200101019      * da abilitazioni sul profilo
048300101019     c                   eval      *in10 = *off
048400101019     c                   IF        §uteinvpwd = 'S' and *in07
048500101019      * e se codice padre gestibile dall'utente
048600101019     c                   Eval      w003a = %subst(v1cksu:2:3)
048700101019     c     w003a         Lookup    skpog                                  31
048800101019if  3c                   If        *In31
048900101019     c                   eval      *in10 = *on
049000101019     c                   endif
049100101019     c                   ENDIF
049200050104
049300050104     c                   Eval      rec = 1
049400050308     c                   If        savnrr > 0
049500050308     c                   Eval      rec = savnrr
049600050308     c                   EndIf
049700050104
049800050104     c                   Write     ta8201z
049900050104     c                   Exfmt     ta8201c
050000050104
050100050104     c                   Eval      *In28 = *Off
050200050104     c                   Clear                   vidmsg
050300050104
050400050104     c                   EndSr
050500050104
050600050104      *------------------------------------------------------------------------*
050700050104      * CONTROLLO I DATI DEL SUBFILE
050800050104      *------------------------------------------------------------------------*
050900050104     c     Sr_CtrSfl     BegSr
051000050104
051100050110     c                   Eval      *In08 = *Off
051200050104     c                   Eval      *In41 = *Off
051300050104     c                   Eval      *In42 = *Off
051400050105     c                   Eval      *In44 = *Off
051500050111     c                   Eval      *In45 = *Off
051600050111     c                   Eval      *In90 = *Off
051700050104     c                   Clear                   nrr
051800050114     c                   Clear                   sksfl
051900050309
052000050309      * Emetto subito il messaggio bloccante se cliente con abilitazione
052100050309      * scaduta
052200050309     c                   If        *In05
052300050309     c                   Eval      *In28 = *On
052400050309     c   07              Eval      *In44 = *On
052500050309     c  n07              Eval      *In45 = *On
052600050309     c                   Eval      vidmsg = msg(05)
052700050309     c                   Leavesr
052800050309     c                   EndIf
052900050104
053000050104      * Dati del control
053100050105
053200050105      * Codice papà
053300050111if  1c                   If        Not *In07 and v1cksu <> *Zeros and
053400050111     c                             v1cksu <> *Blanks
053500050111if  2c                   If        v1cksu > '09999999'
053600050105     c                   Eval      *In28 = *On
053700050105     c                   Eval      *In44 = *On
053800050105     c                   Eval      vidmsg = msg(03)
053900050105     c                   Leavesr
054000050105e   2c                   EndIf
054100050111      * Ricerca
054200050310      * forzo autorizzazione AZ in modo da visualizzare tutti i clienti
054300050111     c     '?'           Scan      v1cksu                                 31
054400050111if  2c                   If        *In31
054500050111     c                   Clear                   Xclirds
054600050111     c                   Eval      DxcCap = DutKci
054700050310     c                   Eval      DxcAut = 'AZ'
054800050111     c                   Call      'XCLIR'
054900050111     c                   Parm                    Xclirds
055000050111     c                   Movea     DxcKsc        ksa
055100050111     c                   move      ksa(1)        dsksa
055200050111     c                   z-add     dsksn         ksc(1)
055300050111     c                   Move      *all'0'       v1cksu
055400050111     c                   Move      ksc(1)        v1cksu
055500050111     c                   Eval      *In44 = *On
055600050111e   2c                   EndIf
055700050111      * Solo codici numerici
055800050111if  2c                   If        v1cksu < *Zeros
055900050111     c                   Eval      *In28 = *On
056000050111     c                   Eval      *In44 = *On
056100050111     c                   Eval      vidmsg = msg(03)
056200050111     c                   Leavesr
056300050111e   2c                   EndIf
056400050105      * Controllo esistenza codice
056500050111if  2c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
056600050105     c                   Move      v1cksu        w0070
056700050105     c                   ExSr      Sr_ChkCli
056800050111if  3c                   If        O69err = *On
056900050105     c                   Eval      *In28 = *On
057000050105     c                   Eval      *In44 = *On
057100050105     c                   Eval      vidmsg = msg(03)
057200050105     c                   Leavesr
057300050111e   3c                   EndIf
057400050105     c                   Movel     AcoRag        v1dksu
057500050308      * Se il figlio è gestibile dall'utente posso immettere qualsiasi papà
057600050308      * altrimenti posso immettere solo papà gestiti dall'utente
057700050308     c                   Move      v1ccli        w007a
057800050308     c                   Eval      w003a = %subst(w007a:1:3)
057900050308     c     w003a         Lookup    skpog                                  31
058000050308if 2ac                   If        Not *In31
058100050110      * Codice gestibile dall'utente
058200050111     c                   Eval      w003a = %subst(v1cksu:2:3)
058300050110     c     w003a         Lookup    skpog                                  31
058400050111if  3c                   If        Not *In31
058500050110     c                   Eval      *In28 = *On
058600050110     c                   Eval      *In44 = *On
058700050110     c                   Eval      vidmsg = msg(13)
058800050110     c                   Leavesr
058900050111e   3c                   EndIf
059000050308e  2ac                   EndIf
059100050105      * non deve essere figlio di altro papà
059200050105     c                   Clear                   Tibs10ds
059300050105     c                   Eval      d10tle = 'WW'
059400050105     c                   Eval      d10paf = 'P'
059500050105     c                   Move      v1cksu        d10cod
059600050105     c                   Eval      d10drf = *date
059700050105     c                   Call      'TIBS10R'
059800050105     c                   Parm                    Tibs10ds
059900050111if  3c                   If        d10err = *Blanks
060000050105     c                   Eval      *In28 = *On
060100050105     c                   Eval      *In44 = *On
060200050105     c                   Eval      vidmsg = msg(09)
060300050105     c                   Move      d10cop        w008a
060400050105     c                   Eval      %subst(vidmsg:36:8) = w008a
060500050105     c                   Leavesr
060600050111e   3c                   EndIf
060700050105      * deve essere già abilitato ai servizi ON LINE
060800050105     c                   Move      *all'0'       vssksu
060900050105     c                   Move      v1cksu        vssksu
061000050105     c                   Eval      vssisv = 'TT'
061100050110     c     kTivss        Chain(n)  Tivss02l
061200050111if  3c                   If        Not %Found(Tivss02l)
061300050105     c                   Eval      *In28 = *On
061400050105     c                   Eval      *In44 = *On
061500050105     c                   Eval      vidmsg = msg(11)
061600050105     c                   Leavesr
061700050111e   3c                   EndIf
061800050111     c                   Eval      wksu = v1cksu
061900050111     c                   Eval      *In90 = *On
062000050316     c                   Eval      *In07 = *On
062100050111     c                   Leavesr
062200050111e   2c                   EndIf
062300050308   x1c                   Else
062400050309      * se papà non inserito e voglio abilitare il codice questo deve essere
062500050309      * gestibile dall'utente
062600050309if  2c                   If        Not *In07
062700050309     c                   Clear                   v1cksu
062800050309     c                   Clear                   v1dksu
062900050309     c                   Move      v1ccli        w007a
063000050309     c                   Eval      w003a = %subst(w007a:1:3)
063100050309     c     w003a         Lookup    skpog                                  31
063200050309if  3c                   If        Not *In31
063300050309     c                   Eval      *In28 = *On
063400050309     c                   Eval      *In45 = *On
063500050309     c                   Eval      vidmsg = msg(13)
063600050309     c                   Leavesr
063700050309e   3c                   EndIf
063800050309e   2c                   EndIf
063900050105e   1c                   EndIf
064000050105      * Responsabile/E-mail
064100050104if  1c                   If        Not *In04
064200050105      * Responsabile
064300050104if  2c                   If        v1crsp = *Blanks
064400050104     c                   Eval      *In28 = *On
064500050104     c                   Eval      *In41 = *On
064600050104     c                   Eval      vidmsg = msg(06)
064700050104     c                   Leavesr
064800050104e   2c                   EndIf
064900050105      * E-mail
065000050104if  2c                   If        v1cem1 = *Blanks and v1cem2 = *Blanks
065100050104     c                   Eval      *In28 = *On
065200050104     c                   Eval      *In42 = *On
065300050104     c                   Eval      vidmsg = msg(07)
065400050104     c                   Leavesr
065500050104e   2c                   EndIf
065600050104     c                   Clear                   dsemail
065700050104     c                   Eval      §emlindi = v1cem1 + v1cem2
065800050104     c                   Call      'XEMAIL'
065900050104     c                   Parm                    dsemail
066000050104if  2c                   If        §emlerro = '1'
066100050104     c                   Eval      *In28 = *On
066200050104     c                   Eval      *In42 = *On
066300050104     c                   Eval      vidmsg = msg(08)
066400050104     c                   Leavesr
066500050104e   2c                   EndIf
066600050104     c                   Eval      v1cem1 = %subst(§emlindo:1:49)
066700050104     c                   Eval      v1cem2 = %subst(§emlindo:50:49)
066800050104e   1c                   EndIf
066900050104
067000050104      * Dati del subfile
067100050104do  1c                   Do        *Hival
067200050104     c                   Eval      nrr = nrr +1
067300050104     c     nrr           Chain     ta8201s                            30
067400050104     c                   If        *In30
067500050104     c                   Leave
067600050104     c                   EndIf
067700050104
067800050111     c                   Eval      *In06 = *Off
067900050104     c                   Eval      *In28 = *Off
068000050104     c                   Eval      *In43 = *Off
068100050104
068200050104      * se non è un codice protetto
068300050104if  2c                   If        wprot = *Blanks
068400050104     c                   Clear                   v1sdes
068500050104      * Controllo il cliente inserito
068600050104if  3c                   If        v1scod > *Zeros
068700091029      * se uguale al codice che si deve abilitare e non c'è il codice padre
068800091029      * devo dare errore, altrimenti il pgm scrive un legame con il codice
068900091029      * padre a 0
069000091029     c                   if        v1scod = v1ccli and
069100091029     c                             (v1cksu = *zeros or v1cksu = *blanks)
069200091029     c                   Eval      *In28 = *On
069300091029     c                   Eval      *In43 = *On
069400091029     c                   Eval      vidmsg = msg(16)
069500091029     c                   Z-add     nrr           savnrr
069600091029     c                   Update    ta8201s
069700091029     c                   Leavesr
069800091029     c                   endif
069900091029      * controllo in anagrafica
070000050104     c                   Move      v1scod        w0070
070100050104     c                   ExSr      Sr_ChkCli
070200050104if  4c                   If        O69err = *On
070300050104     c                   Eval      *In28 = *On
070400050104     c                   Eval      *In43 = *On
070500050104     c                   Eval      vidmsg = msg(03)
070600050308     c                   Z-add     nrr           savnrr
070700050104     c                   Update    ta8201s
070800050114     c                   Leavesr
070900050104e   4c                   EndIf
071000050104     c                   Movel     AcoRag        v1sdes
071100050308
071200050308      * Se il papà è gestibile dall'utente posso immettere qualsiasi figlio
071300050308      * altrimenti posso immettere solo figli gestiti dall'utente
071400050308     c                   Eval      w003a = %subst(v1cksu:2:3)
071500050308     c     w003a         Lookup    skpog                                  31
071600050308if 3ac                   If        Not *In31
071700050110      * Codice gestibile dall'utente
071800050111     c                   Move      v1scod        w008a
071900050111     c                   Eval      w003a = %subst(w008a:2:3)
072000050110     c     w003a         Lookup    skpog                                  31
072100050110if  4c                   If        Not *In31
072200050110     c                   Eval      *In28 = *On
072300050110     c                   Eval      *In43 = *On
072400050110     c                   Eval      vidmsg = msg(13)
072500050308     c                   Z-add     nrr           savnrr
072600050110     c                   Update    ta8201s
072700050114     c                   Leavesr
072800050110e   4c                   EndIf
072900050308e  3ac                   EndIf
073000050110      * non deve essere già caricato nella sk dei figli
073100050110     c                   Z-add     v1scod        w0110
073200050110     c     w0110         Lookup    skfig                                  31
073300050110if  4c                   If        *In31
073400050110     c                   Eval      *In28 = *On
073500050110     c                   Eval      *In43 = *On
073600050110     c                   Eval      vidmsg = msg(12)
073700050308     c                   Z-add     nrr           savnrr
073800050110     c                   Update    ta8201s
073900050114     c                   Leavesr
074000050110e   4c                   EndIf
074100050114      * non deve essere già stato inserito prima nel sfl
074200050114     c     v1scod        Lookup    sksfl                                  31
074300050114if  4c                   If        *In31
074400050114     c                   Eval      *In28 = *On
074500050114     c                   Eval      *In43 = *On
074600050114     c                   Eval      vidmsg = msg(12)
074700050308     c                   Z-add     nrr           savnrr
074800050114     c                   Update    ta8201s
074900050114     c                   Leavesr
075000050114e   4c                   EndIf
075100050104      * non deve essere già figlio di un altro papà
075200050104     c                   Clear                   Tibs10ds
075300050104     c                   Eval      d10tle = 'WW'
075400050104     c                   Eval      d10paf = 'P'
075500050104     c                   Move      v1scod        d10cod
075600050104     c                   Eval      d10drf = *date
075700050104     c                   Call      'TIBS10R'
075800050104     c                   Parm                    Tibs10ds
075900050104if  4c                   If        d10err = *Blanks
076000050104     c                   Eval      *In28 = *On
076100050104     c                   Eval      *In43 = *On
076200050104     c                   Eval      vidmsg = msg(09)
076300050104     c                   Move      d10cop        w008a
076400050104     c                   Eval      %subst(vidmsg:36:8) = w008a
076500050308     c                   Z-add     nrr           savnrr
076600050104     c                   Update    ta8201s
076700050114     c                   Leavesr
076800050104e   4c                   EndIf
076900050104      * non deve essere già papà
077000050104     c                   Clear                   Tibs10ds
077100050104     c                   Eval      d10tle = 'WW'
077200050104     c                   Eval      d10paf = 'F'
077300050104     c                   move      v1scod        d10cod
077400050104     c                   Eval      d10drf = *date
077500050104     c                   Call      'TIBS10R'
077600050104     c                   Parm                    Tibs10ds
077700050104if  4c                   If        d10err = *Blanks
077800050104     c                   Eval      *In28 = *On
077900050104     c                   Eval      *In43 = *On
078000050104     c                   Eval      vidmsg = msg(10)
078100050308     c                   Z-add     nrr           savnrr
078200050104     c                   Update    ta8201s
078300050114     c                   Leavesr
078400050104e   4c                   EndIf
078500050105      * Non deve essere già abilitato
078600050105     c                   Move      *all'0'       vssksu
078700050105     c                   Move      v1scod        vssksu
078800050105     c                   Eval      vssisv = 'TT'
078900050110     c     kTivss        Chain(n)  Tivss02l
079000050105if  4c                   If        %Found(Tivss02l)
079100050105     c                   Eval      *In28 = *On
079200050105     c                   Eval      *In43 = *On
079300050105     c                   Eval      vidmsg = msg(04)
079400050308     c                   Z-add     nrr           savnrr
079500050105     c                   Update    ta8201s
079600050114     c                   Leavesr
079700050105e   4c                   EndIf
079800050114      * carico il nuovo codice in una sk di appoggio x controllare che non
079900050114      * siano immessi dei codici doppi
080000050114     c                   Eval      xx = 1
080100050114     c     *Zeros        Lookup    sksfl(xx)                              31
080200050114if  4c                   If        *In31
080300050114     c                   Eval      sksfl(xx) = v1scod
080400050114e   4c                   EndIf
080500050110     c                   Eval      *In08 = *On
080600050104e   3c                   EndIf
080700050104e   2c                   EndIf
080800050110      * se era protetto lo proteggo di nuovo
080900050110if  2c                   If        wprot = 'S'
081000050110     c                   Eval      *In06 = *On
081100050110e   2c                   EndIf
081200050104
081300050308     c                   Z-add     nrr           rec
081400050104     c                   Update    ta8201s
081500050104
081600050104e   1c                   EndDo
081700050111
081800050111      * Il papà è abilitato ma non sono stati aggiunti dei nuovi figli
081900050111      * errore
082000101006      * e non è stato dato F8=Reinvio Password
082100050309if  1c                   If        *In04 and Not *In08 and Not *In05
082200101006     c                             and not *inkh
082300050111     c                   Eval      *In28 = *On
082400050111     c                   Eval      *In44 = *On
082500050111     c                   Eval      vidmsg = msg(14)
082600050111     c                   Leavesr
082700050111e   1c                   EndIf
082800050104
082900101007      * Il papà è abilitato sono stati aggiunti dei nuovi figli
083000101007      * ma è stato dato F8=Reinvio Password
083100101007      * errore perchè prima si deve fare F6=Conferma
083200101007if  1c                   If        *In04 and *In08 and Not *In05
083300101007     c                             and *inkh
083400101007     c                   Eval      *In28 = *On
083500101007     c                   Eval      *In44 = *On
083600101007     c                   Eval      vidmsg = msg(17)
083700101007     c                   Leavesr
083800101007e   1c                   EndIf
083900101007
084000050104     c                   EndSr
084100050104
084200050104      *------------------------------------------------------------------------*
084300050104      * CONTROLLO IL CLIENTE
084400050104      *------------------------------------------------------------------------*
084500050104     c     Sr_Chkcli     BegSr
084600050104
084700050104     c                   Clear                   Tibs69ds
084800050104     c                   Move      w0070         I69kac
084900050104     c                   Call      'TIBS69R'
085000050112     c                   Parm                    Tibs69ds
085100050104     c                   Parm                    ds_cnaco
085200050104     c                   Parm                    ds_cnind
085300050104     c                   Parm                    ds_cnclp
085400050104     c                   Parm                    ds_fncls
085500050112     c                   Eval      wtibs69 = *On
085600050104
085700050104     c                   EndSr
085800041117
085900050103      *------------------------------------------------------------------------*
086000050111      * CARICA DATI NELLA VIDEATA PER GESTIONE DA OFFERTA
086100050103      *------------------------------------------------------------------------*
086200050103     c     Sr_Car02d     BegSr
086300050103
086400050111     c                   Move      parcli        v1ccli
086500050111     c                   Clear                   v1dcli
086600050111     c                   Clear                   v1crsp
086700050111     c                   Clear                   v1cem1
086800050111     c                   Clear                   v1cem2
086900050103     c                   Eval      V1cAbi = 'S'
087000050111     c                   Clear                   v1cksu
087100050104     c                   Clear                   v1dksu
087200050103
087300050111     c                   Move      v1ccli        w0070
087400050111     c                   ExSr      Sr_ChkCli
087500050104if  1c                   If        O69err <> *On
087600050111     c                   Movel     ACOrag        v1dcli
087700050103e   1c                   EndIf
087800050103
087900050103     c                   EndSr
088000050111
088100050111      *------------------------------------------------------------------------*
088200050111      * CONTROLLO LA VIDEATA PER GESTIONE DA OFFERTA
088300050111      *------------------------------------------------------------------------*
088400050111     c     Sr_Ctr02d     BegSr
088500050111
088600050111     c                   Eval      *In41 = *Off
088700050111     c                   Eval      *In42 = *Off
088800050112     c                   Eval      *In43 = *Off
088900050111     c                   Eval      *In44 = *Off
089000050111     c                   Eval      *In90 = *Off
089100050111
089200050111      * Se immesso che abilito il cliente
089300050111if  1c                   If        v1cabi = 'S'
089400050112      * Il codice non deve essere già figlio di un altro papà
089500050112     c                   Clear                   Tibs10ds
089600050112     c                   Eval      d10tle = 'WW'
089700050112     c                   Eval      d10paf = 'P'
089800050112     c                   Move      v1ccli        d10cod
089900050112     c                   Eval      d10drf = *date
090000050112     c                   Call      'TIBS10R'
090100050112     c                   Parm                    Tibs10ds
090200050112if  2c                   If        d10err = *Blanks
090300050112     c                   Eval      *In28 = *On
090400050112     c                   Eval      *In43 = *On
090500050112     c                   Eval      vidmsg = msg(09)
090600050112     c                   Move      d10cop        w008a
090700050112     c                   Eval      %subst(vidmsg:36:8) = w008a
090800050112     c                   Leavesr
090900050112e   2c                   EndIf
091000050111      * Codice Papà inserito
091100050111if  2c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
091200050111
091300050111if  3c                   If        vidksu > '09999999'
091400050111     c                   Eval      *In28 = *On
091500050111     c                   Eval      *In44 = *On
091600050111     c                   Eval      vidmsg = msg(03)
091700050111     c                   Leavesr
091800050111e   3c                   EndIf
091900050111      * Ricerca
092000050310      * forzo autorizzazione AZ in modo da visualizzare tutti i clienti
092100050111     c     '?'           Scan      v1cksu                                 31
092200050111if  3c                   If        *In31
092300050111     c                   Clear                   Xclirds
092400050111     c                   Eval      DxcCap = DutKci
092500050310     c                   Eval      DxcAut = 'AZ'
092600050111     c                   Call      'XCLIR'
092700050111     c                   Parm                    Xclirds
092800050111     c                   Movea     DxcKsc        ksa
092900050111     c                   move      ksa(1)        dsksa
093000050111     c                   z-add     dsksn         ksc(1)
093100050111     c                   Move      *all'0'       v1cksu
093200050111     c                   Move      ksc(1)        v1cksu
093300050111     c                   Eval      *In44 = *On
093400050111e   3c                   EndIf
093500050111      * Solo codici numerici
093600050111if  3c                   If        v1cksu < *Zeros
093700050111     c                   Eval      *In28 = *On
093800050111     c                   Eval      *In44 = *On
093900050111     c                   Eval      vidmsg = msg(03)
094000050111     c                   Leavesr
094100050111e   3c                   EndIf
094200050111      * Controllo esistenza codice
094300050111if  3c                   If        v1cksu <> *Zeros and v1cksu <> *Blanks
094400050111     c                   Move      v1cksu        w0070
094500050111     c                   ExSr      Sr_ChkCli
094600050111if  4c                   If        O69err = *On
094700050111     c                   Eval      *In28 = *On
094800050111     c                   Eval      *In44 = *On
094900050111     c                   Eval      vidmsg = msg(03)
095000050111     c                   Leavesr
095100050111e   4c                   EndIf
095200050111     c                   Movel     AcoRag        v1dksu
095300050111      * Codice gestibile dall'utente
095400050308     c**!!!              Eval      w003a = %subst(v1cksu:2:3)
095500050308     c**!!!w003a         Lookup    skpog                                  31
095600050308if  4c**!!!              If        Not *In31
095700050308     c**!!!              Eval      *In28 = *On
095800050308     c**!!!              Eval      *In44 = *On
095900050308     c**!!!              Eval      vidmsg = msg(13)
096000050308     c**!!!              Leavesr
096100050308e   4c**!!!              EndIf
096200050111      * non deve essere figlio di altro papà
096300050111     c                   Clear                   Tibs10ds
096400050111     c                   Eval      d10tle = 'WW'
096500050111     c                   Eval      d10paf = 'P'
096600050111     c                   Move      v1cksu        d10cod
096700050111     c                   Eval      d10drf = *date
096800050111     c                   Call      'TIBS10R'
096900050111     c                   Parm                    Tibs10ds
097000050111if  4c                   If        d10err = *Blanks
097100050111     c                   Eval      *In28 = *On
097200050111     c                   Eval      *In44 = *On
097300050111     c                   Eval      vidmsg = msg(09)
097400050111     c                   Move      d10cop        w008a
097500050111     c                   Eval      %subst(vidmsg:36:8) = w008a
097600050111     c                   Leavesr
097700050111e   4c                   EndIf
097800050111      * deve essere già abilitato ai servizi ON LINE
097900050111     c                   Move      *all'0'       vssksu
098000050111     c                   Move      v1cksu        vssksu
098100050111     c                   Eval      vssisv = 'TT'
098200050111     c     kTivss        Chain(n)  Tivss02l
098300050111if  4c                   If        Not %Found(Tivss02l)
098400050111     c                   Eval      *In28 = *On
098500050111     c                   Eval      *In44 = *On
098600050111     c                   Eval      vidmsg = msg(11)
098700050111     c                   Leavesr
098800050111e   4c                   EndIf
098900050316if  4c**!!!              If        wksu = *Blanks or wksu = *Zeros
099000050316     c                   If        wksu <> v1cksu
099100050111     c                   Eval      wksu = v1cksu
099200050111     c                   Eval      *In90 = *On
099300050112e   4c                   EndIf
099400050111     c                   Leavesr
099500050111e   3c                   EndIf
099600050316      * se il codice padre non è inserito ma prima ne avevo inserito uno devo
099700050316      * pulire i dati del responsabile e dell'e-mail
099800050316   x2c                   Else
099900050316if  3c                   If        wksu <> *Zeros and wksu <> *Blanks
100000050316     c                   Clear                   wksu
100100050316     c                   Clear                   v1cksu
100200050316     c                   Clear                   v1dksu
100300050316     c                   Clear                   v1crsp
100400050316     c                   Clear                   v1cem1
100500050316     c                   Clear                   v1cem2
100600050316     c                   Eval      *In04 = *Off
100700091029     c                   Eval      *In07 = *Off
100800050316    3c                   EndIf
100900050111e   2c                   EndIf
101000050111      * Responsabile
101100050111if  2c                   If        v1crsp = *Blanks
101200050111     c                   Eval      *In28 = *On
101300050111     c                   Eval      *In41 = *On
101400050111     c                   Eval      vidmsg = msg(06)
101500050111     c                   Leavesr
101600050111e   2c                   EndIf
101700050111      * E-mail
101800050111if  2c                   If        v1cem1 = *Blanks and v1cem2 = *Blanks
101900050111     c                   Eval      *In28 = *On
102000050111     c                   Eval      *In42 = *On
102100050111     c                   Eval      vidmsg = msg(07)
102200050111     c                   Leavesr
102300050111e   2c                   EndIf
102400050111     c                   Clear                   dsemail
102500050111     c                   Eval      §emlindi = v1cem1 + v1cem2
102600050111     c                   Call      'XEMAIL'
102700050111     c                   Parm                    dsemail
102800050111if  2c                   If        §emlerro = '1'
102900050111     c                   Eval      *In28 = *On
103000050111     c                   Eval      *In42 = *On
103100050111     c                   Eval      vidmsg = msg(08)
103200050111     c                   Leavesr
103300050111e   2c                   EndIf
103400050111     c                   Eval      v1cem1 = %subst(§emlindo:1:49)
103500050111     c                   Eval      v1cem2 = %subst(§emlindo:50:49)
103600050111e   1c                   EndIf
103700050111
103800050111     c                   EndSr
103900101006
104000101006      /free
104100101006       //--------------------------------------------------------------
104200101006       //?Reinvio password a cliente abilitato.
104300101006       //--------------------------------------------------------------
104400101006       BEGSR sr_Password;
104500101006
104600120126       //?Come prima cosa verifico che non ci siano più abilitazioni per
104700120126       //?lo stesso unificante
104800120126         clear w_Conta;
104900120126         clear w_VSSsun;
105000120126         setll (VSSksu : VSSisv) TIVSS02L;
105100120126         reade(n) (VSSksu : VSSisv) TIVSS02L;
105200120126         DOW  not %eof(TIVSS02L);
105300120126           IF  w_VSSsun <> VSSsun;
105400120126             w_VSSsun = VSSsun;
105500120126             w_Conta += 1;
105600120126           ENDIF;
105700120126           reade(n) (VSSksu : VSSisv) TIVSS02L;
105800120126         ENDDO;
105900120126         IF  w_Conta > 1;
106000120126           *in28 = *on;
106100120126           *in90 = *on;
106200120126           VIDmsg = msg(18);
106300120126           leavesr;
106400120126         ENDIF;
106500120126
106600101006         $Fine = *off;
106700101019         w03cem1 = v1cem1;
106800101019         w03cem2 = v1cem2;
106900101019         w03crsp = v1crsp;
107000101006       //?Emetto la window
107100101006         DOW  not $Fine;
107200101006           exfmt TA8203W;
107300101006           *in28 = *off;
107400101006           clear w03msg;
107500101006           SELECT;
107600101006             WHEN  *inkf;
107700101018               exsr CtrW03;
107800101018               IF  *in28;
107900101018                 iter;
108000101018               ENDIF;
108100101006               exsr sr_newpsw;
108200101006               IF  not *in28;
108300101006                 $Fine = *on;
108400101006               ENDIF;
108500101006             WHEN  *inkl;
108600101006               *in90 = *on;
108700101006               $Fine = *on;
108800101018             OTHER;
108900101018               exsr CtrW03;
109000101006           ENDSL;
109100101006         ENDDO;
109200101006
109300101006       ENDSR;
109400101018
109500101018       //--------------------------------------------------------------
109600101018       //?Controllo i dati della window.
109700101018       //--------------------------------------------------------------
109800101018       BEGSR CtrW03;
109900101019
110000101019         *in41 = *off;
110100101019         *in42 = *off;
110200101018
110300101018       // E-mail
110400101019         IF  w03cem1 = *blanks and w03cem2 = *blanks;
110500101018           *in28 = *on;
110600101018           *in42 = *on;
110700101018           w03msg = msg(07);
110800101018           leavesr;
110900101018         ENDIF;
111000101018      /end-free
111100101018     c                   Clear                   dsemail
111200101019     c                   Eval      §emlindi = w03cem1 + w03cem2
111300101018     c                   Call      'XEMAIL'
111400101018     c                   Parm                    dsemail
111500101018if  2c                   If        §emlerro = '1'
111600101018     c                   Eval      *In28 = *On
111700101018     c                   Eval      *In42 = *On
111800101018     c                   Eval      w03msg = msg(08)
111900101018     c                   Leavesr
112000101018e   2c                   EndIf
112100101018      /free
112200101019       w03cem1 = %subst(§emlindo:1:49);
112300101019       w03cem2 = %subst(§emlindo:50:49);
112400101019
112500101019       // Responsabile
112600101019         IF  w03crsp = *blanks;
112700101019           *in28 = *on;
112800101019           *in41 = *on;
112900101019           w03msg = msg(06);
113000101019           leavesr;
113100101019         ENDIF;
113200101018
113300101018       ENDSR;
113400101006      /end-free
113500041117
113600041118      *------------------------------------------------------------------------*
113700001011      * F6 - Conferma
113800050110      *------------------------------------------------------------------------*
113900050110     c     Sr_Conferma   BegSr
114000050112
114100050112     c                   Eval      *In90 = *Off
114200100408
114300100408      /free
114400100408       //?Devo togliere gli apici dal campo V1CRSP
114500100408       //?richiesto da Fabrizio
114600100408       clear wpos;
114700100408       wpos = %scan(api:v1crsp);
114800100408       DOW  wpos > 0;
114900100408         v1crsp = %replace(' ':v1crsp:wpos);
115000100408         clear wpos;
115100100408         wpos = %scan(api:v1crsp);
115200100408       ENDDO;
115300100408      /end-free
115400050110
115500050110      * --> Pgm non richiamato
115600050110if  1c                   If        Not *In01
115700050110
115800050110      * Papà già abilitato e sono stati inseriti dei nuovi figli
115900050111if  2c                   If        *In04 and *In08
116000050110      * Aggiorno il legame WW su TIVSS se non aveva già dei figli
116100050110if  3c                   If        skfig(01) = *Zeros
116200050110     c                   Move      *all'0'       vssksu
116300050111     c   07              Move      v1cksu        vssksu
116400050111     c  n07              Move      v1ccli        vssksu
116500050110     c                   Eval      vssisv = 'TT'
116600050110     c     kTivss        Chain     Tivss02l
116700050110if  4c                   If        %Found(Tivss02l)
116800050110     c                   Eval      vsstle = 'WW'
116900050110     c                   Update    Tivss000
117000050110e   4c                   EndIf
117100050110e   3c                   EndIf
117200050110      * Scrivo i nuovi legami su TIKUN
117300050111     c                   Clear                   nrr
117400050110do  3c                   Do        *Hival
117500050110     c                   Eval      nrr = nrr +1
117600050110     c     nrr           Chain     ta8201s                            30
117700050110     c                   If        *In30
117800050110     c                   Leave
117900050110     c                   EndIf
118000050110      * Se non è un codice protetto
118100050110if  4c                   If        wprot = *Blanks and v1scod <> *Zeros
118200050110      * Scrivo TIKUN
118300050110     c                   Clear                   Tikun000
118400050111     c   07              Move      v1cksu        kuncop
118500050111     c  n07              Move      v1ccli        kuncop
118600050110     c                   Move      v1scod        kuncof
118700050110     c                   Eval      kuntle = 'WW'
118800050110     c                   Eval      kundde = wdatai
118900050110     c                   Eval      kundsc = wdataf
119000050110     c                   Move      *Date         kundtr
119100050110     c                   Write     Tikun000
119200050110e   4c                   EndIf
119300050110e   3c                   EndDo
119400050110e   2c                   EndIf
119500050111      * Codice nuovo da abilitare
119600050114if  2c                   If        Not *In04 and was888 = *Off
119700050110      * Abilito il codice richiamando il TIS730R
119800050110     c                   Clear                   Tis730ds
119900050110     c                   Eval      s30ric = 'S'
120000101006     c                   eval      s30opz = 'II'
120100101230     c   07              move      v1cksu        s30ksu
120200101230     c  n07              Eval      s30ksu = v1ccli
120300050110     c                   Eval      s30rsp = v1crsp
120400050110     c                   Eval      s30eml = v1cem1 + v1cem2
120500050110     c                   Eval      s30isv = 'TT'
120600050119     c                   Eval      s30dti = *date
120700050110     c                   Eval      s30dtf = wdataf
120800060905      * controllo chiodato perchè è successo che sia stato abilitato un cliente
120900060905      * con codice zero
121000060905      * non so se è colpa mia o di Gurro!!!!
121100060905     c                   if        s30ksu = 0
121200060905     c                   eval      vidmsg = 'Manca codice cliente telefonare al-
121300060905     c                                       CED in Sede!!'
121400060905     c                   eval      *in28 = *on
121500060905     c                   leavesr
121600060905     c                   endif
121700050110     c                   Call      'TIS730R'
121800050110     c                   Parm                    Kpjba
121900050110     c                   Parm                    Tis730ds
122000050112if  3c                   If        s30err <> *Blanks
122100101006if  4c                   If        s30err = '08'
122200050111     c                   Eval      vidmsg = s30msg
122300050112   x4c                   Else
122400050112     c                   Eval      vidmsg = msg(15)
122500050112e   4c                   EndIf
122600050111     c                   Eval      *In28 = *On
122700050111     c                   Leavesr
122800050112e   3c                   EndIf
122900110223      * Se sono stati inseriti dei figli
123000110223if  3c                   If        *In08
123100050110     c                   Move      *all'0'       vssksu
123200101230     c   07              Move      v1cksu        vssksu
123300101230     c  n07              Move      v1ccli        vssksu
123400050110     c                   Eval      vssisv = 'TT'
123500050110     c     kTivss        Chain     Tivss02l
123600050110if  4c                   If        %Found(Tivss02l)
123700050110     c                   Eval      vsstle = 'WW'
123800050110     c                   Update    Tivss000
123900050110e   4c                   EndIf
124000050110      * Scrivo i nuovi legami su TIKUN
124100050111     c                   Clear                   nrr
124200050111do  4c                   Do        *Hival
124300050110     c                   Eval      nrr = nrr +1
124400050110     c     nrr           Chain     ta8201s                            30
124500050110     c                   If        *In30
124600050110     c                   Leave
124700050110     c                   EndIf
124800101230if  5c**!!!              If        v1scod <> *Zeros
124900101230if  4c                   If        wprot = *Blanks and v1scod <> *Zeros
125000050110      * Scrivo TIKUN
125100050110     c                   Clear                   Tikun000
125200101230     c   07              Move      v1cksu        kuncop
125300101230     c  n07              Move      v1ccli        kuncop
125400101230     c**!!!              Move      v1ccli        kuncop
125500050110     c                   Move      v1scod        kuncof
125600050110     c                   Eval      kuntle = 'WW'
125700050110     c                   Eval      kundde = wdatai
125800050110     c                   Eval      kundsc = wdataf
125900050110     c                   Move      *Date         kundtr
126000050110     c                   Write     Tikun000
126100050111e   5c                   EndIf
126200050111e   4c                   EndDo
126300101230e   3c                   EndIf
126400050111e   2c                   EndIf
126500050110
126600050111     c                   Goto      Inz01
126700050110e   1c                   EndIf
126800050110
126900050111      * --> Pgm richiamato
127000050111if  1c                   If        *In01
127100050112      * Richiesta l'abilitazione del cliente
127200050111if  2c                   If        v1cabi = 'S'
127300050111      * Immesso il papà
127400050111if  3c                   If        *In07
127500050112      * Aggiorno il legame WW su TIVSS
127600050111     c                   Move      *all'0'       vssksu
127700050112     c                   Move      v1cksu        vssksu
127800050111     c                   Eval      vssisv = 'TT'
127900050111     c     kTivss        Chain     Tivss02l
128000050111if  4c                   If        %Found(Tivss02l)
128100050111     c                   Eval      vsstle = 'WW'
128200050111     c                   Update    Tivss000
128300050111e   4c                   EndIf
128400050111      * Scrivo i nuovi legami su TIKUN
128500050111      * Scrivo TIKUN
128600050111     c                   Clear                   Tikun000
128700050111     c                   Move      v1cksu        kuncop
128800050111     c                   Move      v1ccli        kuncof
128900050111     c                   Eval      kuntle = 'WW'
129000050111     c                   Eval      kundde = wdatai
129100050111     c                   Eval      kundsc = wdataf
129200050111     c                   Move      *Date         kundtr
129300050111     c                   Write     Tikun000
129400050111e   3c                   EndIf
129500050111      * Codice nuovo da abilitare
129600050114if  3c                   If        Not *In07 and was888 = *Off
129700050111      * Abilito il codice richiamando il TIS730R
129800050111     c                   Clear                   Tis730ds
129900050111     c                   Eval      s30ric = 'S'
130000101006     c                   eval      s30opz = 'II'
130100050111     c                   Eval      s30ksu = v1ccli
130200050111     c                   Eval      s30rsp = v1crsp
130300050111     c                   Eval      s30eml = v1cem1 + v1cem2
130400050111     c                   Eval      s30isv = 'TT'
130500050119     c                   Eval      s30dti = *date
130600050111     c                   Eval      s30dtf = wdataf
130700050111     c                   Call      'TIS730R'
130800050111     c                   Parm                    Kpjba
130900050111     c                   Parm                    Tis730ds
131000050112if  4c                   If        s30err <> *Blanks
131100101006if  5c                   If        s30err = '08'
131200050112     c                   Eval      vidmsg = s30msg
131300050112   x5c                   Else
131400050112     c                   Eval      vidmsg = msg(15)
131500050112e   5c                   EndIf
131600050111     c                   Eval      *In28 = *On
131700050111     c                   Leavesr
131800050112e   4c                   EndIf
131900050111e   3c                   EndIf
132000050111e   2c                   EndIf
132100050111     c                   Goto      Fine
132200050111e   1c                   EndIf
132300050110
132400050110     c                   EndSr
132500101006
132600101006      /free
132700101006       //--------------------------------------------------------------
132800101006       //?Nuova password a cliente abilitato.
132900101006       //--------------------------------------------------------------
133000101006       BEGSR sr_newpsw;
133100101018
133200101018       //?Devo togliere gli apici dal campo V1CRSP
133300101018       //?richiesto da Fabrizio
133400101018       clear wpos;
133500101019       wpos = %scan(api:w03crsp);
133600101018       DOW  wpos > 0;
133700101019         w03crsp = %replace(' ':w03crsp:wpos);
133800101018         clear wpos;
133900101019         wpos = %scan(api:w03crsp);
134000101018       ENDDO;
134100101006
134200101006       //?Richiamo pgm per creare nuova password
134300101006       //?solo se sono su sistema di produzione
134400101006         IF  was888 = *off;
134500101006           clear TIS730DS;
134600101006           S30ric = 'S';
134700101018           S30opz = 'UU';
134800101006           S30sun = VSSsun;
134900101006           S30ksu = %dec(VSSksu:8:0);
135000101019           S30rsp = w03crsp;
135100101019           S30eml = w03cem1 + w03cem2;
135200101006           S30isv = 'TT';
135300101006           S30dti = VSSdde;
135400101006           S30dtf = VSSdsc;
135500101006           tis730r (kpjba : TIS730DS);
135600101006           SELECT;
135700101006             WHEN  S30err = '16';
135800101006             WHEN  S30err = '08';
135900101006               w03msg = S30msg;
136000101006               *in28 = *on;
136100101006             WHEN  S30err <> *blanks;
136200101006               w03msg = msg(15);
136300101006               *in28 = *on;
136400101006           ENDSL;
136500101006         ENDIF;
136600101006
136700101006       ENDSR;
136800101006      /end-free
136900050110
137000050110      *------------------------------------------------------------------------*
137100050110      * ROUTINE INIZIALE
137200050110      *------------------------------------------------------------------------*
137300050110     c     *Inzsr        BegSr
137400050110
137500050110     c     *Entry        Plist
137600050110     c                   Parm                    Kpjba
137700050103     c                   Eval      Param = Kpjbu
137800050103
137900050103     c                   Eval      *In01 = (parflg <> *Blanks)
138000050110
138100050110     c     *dtaara       define    §azute        azuteds
138200050110     c     *dtaara       define    §datiute      ddatiute
138300050110     c                   in(E)     *dtaara
138400050110     c                   If        %error  or RSUT = *blanks
138500050110     c                   Clear                   Tibs34ds
138600050110     c                   Call      'TIBS34R'
138700050110     c                   Parm                    Tibs34ds
138800050110     c                   In        *dtaara
138900050110     c                   EndIf
139000050110
139100050110     c                   Clear                   wabi
139200050110     c                   Clear                   dLat
139300050110
139400050110      * Verifica errori e autorità profilo
139500050110s   1c                   Select
139600050110      * se ho errori nei dati utente esco dal pgm
139700050110w   1c                   When      DutErr = 'E'
139800050110     c                   GoTo      Fine
139900050110      * se non c'è l'abilitazione
140000050110      * --> se 1° livello, abilitazioni al terminal
140100050110      *     se 2° livello, abilitazioni al punto operativo
140200050110      *     se sede è impossibile ma se capita mando a fine il pgm
140300050110w   1c                   When      UteAut = *Blanks
140400050110if  2c                   If        DutLpo = '1'
140500050110     c                   Eval      wabi   = 'TP'
140600050110e   2c                   EndIf
140700050110if  2c                   If        DutLpo = '2'
140800050110     c                   Eval      wabi   = 'PO'
140900050110e   2c                   EndIf
141000050110if  2c                   If        DutLpo = 'S'
141100050110     c                   GoTo      Fine
141200050110e   2c                   EndIf
141300050110      * carica le abilitazioni del profilo
141400050110x   1c                   Other
141500050110     c                   Movel     UteFaf        Dute01
141600050110if  2c                   If        §UteCli <> *Blanks
141700050110     c                   Eval      wabi = §UteCli
141800050110   x2c                   Else
141900050110     c                   Eval      wabi = UteAut
142000050110e   2c                   EndIf
142100050110e   1c                   EndSl
142200050110
142300050110      * controllo se ok l'abilitazione dell'utente
142400050110     c                   Clear                   Tibs02ds
142500050110     c                   Eval      T02Mod = 'C'
142600050110     c                   Eval      T02Sif = knsif
142700050110     c                   Eval      T02Cod = 'LAT'
142800050110     c                   Movel(p)  wabi          T02Ke1
142900050110     c                   Call      'TIBS02R'
143000050110     c                   Parm                    kpjba
143100050110     c                   Parm                    Tibs02ds
143200050110if  1c                   If        T02Err = *Blanks
143300050110     c                   Eval      dLat = T02Uni
143400050110e   1c                   EndIf
143500050110      * errore o non abilitato
143600050110if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
143700050103     c                   Eval      *In02 = *On
143800050110     c                   Eval      *In28 = *On
143900050104     c                   Eval      vidmsg = msg(01)
144000050110e   1c                   EndIf
144100050110
144200050110      * Reperimento dei P.O. gestibili dall'utente
144300050110if  1c                   If        Not *In28
144400050110     c                   Clear                   Trul31ds
144500050110     c                   Eval      I31abi = wabi
144600050110     c                   Eval      I31cdi = DUTdis
144700050110     c                   Eval      I31car = DUTare
144800050110     c                   Eval      I31cpo = DUTpou
144900050110     c                   Call      'TRUL31R'
145000050110     c                   Parm                    KPJBA
145100050110     c                   Parm                    Trul31ds
145200050110if  2c                   If        O31pog > *zeros
145300050110     c                   Movea     O31pog        skpog
145400050110x   2c                   Else
145500050103     c                   Eval      *In02 = *On
145600050110     c                   Eval      *In28 = *On
145700050104     c                   Eval      vidmsg = msg(01)
145800050110e   2c                   EndIf
145900050110e   1c                   EndIf
146000101018
146100050110
146200050110      * Se è ambiente prova GAITRAGRPS
146300050110     c                   If        %subst(knsif:7:1) = 'P'
146400050114     c                   Eval      was888 = *On
146500050110      * Se non è ambiente prova GAITRAGRU
146600050110     c                   Else
146700050114     c                   Eval      was888 = *Off
146800050110     c                   EndIf
146900050110
147000050110      * Calcolo oggi -60 gg.
147100050110     c     *iso          Move      *date         dataiso
147200050110     c                   subdur    60:*d         dataiso
147300050110     c                   Move      dataiso       wdatai
147400050110     c                   Move      20391231      wdataf
147500050110
147600050110      * KLIST
147700050104     c     kTiabl        klist
147800050104     c                   kfld                    vsssun
147900050104     c                   kfld                    vssisv
148000050104     c                   kfld                    abltip
148100050104
148200050104     c     kTivss        klist
148300050104     c                   kfld                    vssksu
148400050104     c                   kfld                    vssisv
148500050110
148600050110     c                   EndSr
148700050110      *------------------------------------------------------------------------*
148800050110
148900050110** MSG  Lungh. 78                                                            *
149000050110Utente non autorizzato all'Abilitazione clienti ai servizi ON LINE            01
149100050110Immettere il Cliente                                                          02
149200050103Codice Cliente errato                                                         03
149300050104ATTENZIONE!! Cliente già abilitato                                            04
149400050309ATTENZIONE!! Cliente con abilitazione scaduta avvisare il CED!!!!!            05
149500050309Immettere il Responsabile password                                            06
149600050309Immettere l'indirizzo e-mail x invio password                                 07
149700050104Indirizzo e-mail errato                                                       08
149800050104Il codice inserito è già figlio di                                            09
149900050105Il codice inserito è un padre                                                 10
150000050105Il codice padre non è abilitato ai servizi ON LINE                            11
150100050110Codice già presente                                                           12
150200050110Codice Cliente non gestibile dall'utente                                      13
150300050111ATTENZIONE!! Non sono state effettuate modifiche                              14
150400050112ATTENZIONE!! Non è possibile abilitare il cliente... RIPROVARE PIU' TARDI     15
150500091029Codice figlio uguale al codice da abilitare                                   16
150600101007ATTENZIONE!! Modifiche in sospeso, prima fare F6=Conferma                     17
150700120126Presenti più abilitazioni sullo stesso cliente. CONTATTARE IL CED!!!          18
