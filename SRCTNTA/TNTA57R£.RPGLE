000100060317     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060317
000300060317      *------------------------------------------------------------------------*
000400060317      *
000500060317      *  Stato avanzamento progetto AC BASE
000600060317      *
000700060317      *------------------------------------------------------------------------*
000800060317
000900060320     fazorg01l  if   e           k disk
001000060320     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001100060317     ftnta57d   cf   e             workstn
001200060317
001300060317      *------------------------------------------------------------------------*
001400060317      *  RIEPILOGO INDICATORI
001500060317      *------------------------------------------------------------------------*
001600060320      * 08 - utente non abilitato
001700060320      * 12 - utente di sede
001800060320      * 28 - errore generico
001900060320      * 30 - comodo
002000060320      * 40 - errore p.o.
002100060321      * 41 - errore selezioni
002200060320      * 46 - autorizzazione distretto
002300060320      * 47/56 - errore sulle aree
002400060317      *------------------------------------------------------------------------*
002500060317
002600060317      *------------------------------------------------------------------------*
002700060317      *   V A R I A B I L I
002800060317      *------------------------------------------------------------------------*
002900060320     d wabi            s                   like(uteaut)
003000060320     d w003a           s              3
003100060320     d xx              s              3  0
003200060320     d yy              s              3  0
003300060317
003400060317      *------------------------------------------------------------------------*
003500060317      *   S C H I E R E
003600060317      *------------------------------------------------------------------------*
003700070531     d car             s              3    dim(20)
003800060320     d msg             s             78    dim(10) ctdata perrcd(1)
003900060320     d skpog           s              3    dim(250) inz(*zeros)
004000060317
004100060317      *------------------------------------------------------------------------*
004200060317      *   D S   I N T E R N E / E S T E R N E
004300060317      *------------------------------------------------------------------------*
004400060317     d parm01          ds
004500060321     d  parfil                 1      3  0
004600060321     d  parcar                 4     33    dim(10)
004700060321     d  parsc01               34     34
004800060321     d  parsc02               35     35
004900060321     d  parsc03               36     36
005000060321     d  parsc04               37     37
005100060323     d  parfile               38     38
005200060323     d  parord                39     39
005300060323     d  parabi                40     41
005400060320
005500060320     d ds_car          ds
005600060320     d  v1car1
005700060320     d  v1car2
005800060320     d  v1car3
005900060320     d  v1car4
006000060320     d  v1car5
006100060320     d  v1car6
006200060320     d  v1car7
006300060320     d  v1car8
006400060320     d  v1car9
006500060320     d  v1car0
006600060317
006700060320     d azuteds       e ds                  extname(azute00f)
006800060317     d ddatiute      e ds
006900060320     d dlat          e ds
007000060320     d dute01        e ds
007100060317     d kpjba         e ds
007200060320     d tibs02ds      e ds
007300060317     d tibs34ds      e ds
007400060320     d trul31ds      e ds
007500060317
007600060317     d                sds
007700060317     d  vtcpgm                 1     10
007800050408      *------------------------------------------------------------------------*
007900050408
008000060320      * pulizia videata
008100060320     c                   exsr      sr_pulvid
008200050408
008300060320      * emissione videata
008400060320     c                   do        *hival
008500060320     c                   exfmt     ta57d01
008600050408
008700060320      * f3=fine
008800060320     c   kc              leave
008900050408
009000060320     c                   setoff                                       28
009100050408
009200060320      * controlli videata
009300060320     c                   exsr      sr_contr
009400060320     c   28              iter
009500050408
009600060320      * f6=conferma
009700060320     c                   if        *inkf = *on
009800060320     c                   exsr      sr_conferma
009900060320     c                   leave
010000060320     c                   endif
010100050408
010200060320     c                   enddo
010300050408
010400060320     c                   eval      *inlr = *on
010500060320
010600050408      *------------------------------------------------------------------------*
010700050408      * PULIZIA VIDEATA
010800050408      *------------------------------------------------------------------------*
010900060320     c     sr_pulvid     begsr
011000050408
011100060320     c                   clear                   v1cfil
011200060320     c                   eval      v1dfil = 'Tutti'
011300060320     c                   clear                   ds_car
011400060320
011500060320     c                   select
011600060320      * se abilitazione distretto faccio vedere le aree gestite
011700060320     c                   when      wabi = 'DI'
011800060320     c                   exsr      sr_cararee
011900060320     c                   eval      *in46 = *on
012000060320
012100060320      * imposto il p.o. da elaborare
012200060320     c                   when      not *in12
012300060320     c                   movel     dutpou        v1cfil
012400060320     c     v1cfil        chain     azorg01l
012500060320     c                   if        %found(azorg01l) and orgfva = *blanks
012600060320     c                   eval      v1dfil = orgdes
012700060320     c                   endif
012800060320     c                   endsl
012900060320
013000060320     c                   clear                   v1csc1
013100060320     c                   clear                   v1csc2
013200060320     c                   clear                   v1csc3
013300060320     c                   clear                   v1csc4
013400060320
013500060320     c                   eval      v1ctpel = 'S'
013600060322     c                   eval      v1cord = 'C'
013700050408
013800060320     c                   endsr
013900060320
014000060320      *---------------------------------------------------------------*
014100060320      * CARICO LE AREE IN GESTIONE AL DISTRETTO
014200060320      *---------------------------------------------------------------*
014300060320     c     sr_cararee    begsr
014400060320
014500060320     c                   clear                   car
014600060320
014700060320     c     dutdis        setll     azorg02l
014800060320     c                   do        *hival
014900060320     c     dutdis        reade     azorg02l
015000060320     c                   if        %eof(azorg02l)
015100060320     c                   leave
015200060320     c                   endif
015300060320     c                   if        orgfva <> *blanks or
015400060320     c                             (orgfag <> 'A' and orgfag <> 'F')
015500060320     c                   iter
015600060320     c                   endif
015700060320
015800060320     c                   move      orgcar        w003a
015900060320     c     w003a         lookup    car                                    30
016000060320     c                   if        not *in30
016100060320     c                   eval      xx = 1
016200060320     c     *blanks       lookup    car(xx)                                30
016300060320     c                   if        *in30
016400060320     c                   eval      car(xx) = w003a
016500060320     c                   endif
016600060320     c                   endif
016700060320
016800060320     c                   enddo
016900060320
017000060320     c                   eval      v1car1 = car(1)
017100060320     c                   eval      v1car2 = car(2)
017200060320     c                   eval      v1car3 = car(3)
017300060320     c                   eval      v1car4 = car(4)
017400060320     c                   eval      v1car5 = car(5)
017500060320     c                   eval      v1car6 = car(6)
017600060320     c                   eval      v1car7 = car(7)
017700060320     c                   eval      v1car8 = car(8)
017800060320     c                   eval      v1car9 = car(9)
017900060320     c                   eval      v1car0 = car(10)
018000060320
018100060320     c                   endsr
018200060320
018300050408      *------------------------------------------------------------------------*
018400050408      * CONTROLLI VIDEO
018500050408      *------------------------------------------------------------------------*
018600060320     c     sr_contr      begsr
018700060320
018800060320      * se utente non abilitato riemetto la videata con l'errore
018900060320     c                   if        *in08
019000060320     c                   eval      *in28 = *on
019100060320     c                   leavesr
019200060320     c                   endif
019300050408
019400060320      * reset indicatori
019500060321     c                   setoff                                       404147
019600060320     c                   setoff                                       484950
019700060320     c                   setoff                                       515253
019800060320     c                   setoff                                       545556
019900060320
020000060320      * controllo il p.o. se immesso
020100060320     c                   if        v1cfil > *zeros
020200060320     c                   move      v1cfil        w003a
020300060320      * controllo se il p.o. è valido
020400060320     c     v1cfil        chain     azorg01l
020500060320     c                   if        not %found(azorg01l) or orgfva <> *blanks
020600060320     c                   eval      *in28 = *on
020700060320     c                   eval      *in40 = *on
020800060320     c                   eval      v1cmsg = msg(03)
020900060320     c                   leavesr
021000060320     c                   endif
021100060320     c                   if        orgfag <> 'A' and orgfag <> 'F'
021200060320     c                   eval      *in28 = *on
021300060320     c                   eval      *in40 = *on
021400060320     c                   eval      v1cmsg = msg(03)
021500060320     c                   leavesr
021600060320     c                   endif
021700060320      * controllo se il p.o. è in gestione
021800060320     c     w003a         lookup    skpog                                  30
021900060320     c                   if        not *in30
022000060320     c                   eval      *in28 = *on
022100060320     c                   eval      *in40 = *on
022200060320     c                   eval      v1cmsg = msg(02)
022300060320     c                   leavesr
022400060320     c                   endif
022500060321     c                   eval      v1dfil = orgdes
022600060320     c                   else
022700060320     c                   eval      v1dfil ='Tutti'
022800060320     c                   endif
022900060320
023000060320      * se immesso il p.o. non posso immettere le aree e viceversa
023100060320     c                   if        v1cfil <> *zeros and ds_car <> *blanks
023200060320     c                   eval      *in28 = *on
023300060320     c                   eval      *in40 = *on
023400060320     c                   eval      *in47 = *on
023500060320     c                   eval      v1cmsg = msg(04)
023600060320     c                   leavesr
023700060320     c                   endif
023800060320
023900060320      * controllo le aree immesse
024000060321     c                   movea     ds_car        parcar
024100060320      * Ricerca
024200060320     c                   do        10            xx
024300060320     c                   eval      yy = (46 + xx)
024400060321     c                   if        parcar(xx) <> *blanks
024500060321     c     parcar(xx)    lookup    car                                    30
024600060320     c                   if        not *in30
024700060320     c                   eval      v1cmsg = msg(05)
024800060320     c                   eval      *in(yy) = *on
024900060320     c                   eval      *in28 = *on
025000060320     c                   endif
025100060320     c                   endif
025200060320     c                   enddo
025300060320
025400060321     c                   move      parcar(1)     v1car1
025500060321     c                   move      parcar(2)     v1car2
025600060321     c                   move      parcar(3)     v1car3
025700060321     c                   move      parcar(4)     v1car4
025800060321     c                   move      parcar(5)     v1car5
025900060321     c                   move      parcar(6)     v1car6
026000060321     c                   move      parcar(7)     v1car7
026100060321     c                   move      parcar(8)     v1car8
026200060321     c                   move      parcar(9)     v1car9
026300060321     c                   move      parcar(10)    v1car0
026400060320     c   28              leavesr
026500060321
026600060321      * almeno una scelta deve essere fatta
026700060321     c                   if        v1csc1 = *blanks and v1csc2 = *blanks and
026800060323     c                             v1csc3 = *blanks and v1csc4 = *blanks
026900060321     c                   eval      *in28 = *on
027000060321     c                   eval      *in41 = *on
027100060321     c                   eval      v1cmsg = msg(06)
027200060321     c                   leavesr
027300060321     c                   endif
027400050408
027500060320     c                   endsr
027600060320
027700050408      *------------------------------------------------------------------------*
027800060320      * RICHIAMA IL PROGRAMMA BATCH
027900050408      *------------------------------------------------------------------------*
028000060320     c     sr_conferma   begsr
028100050408
028200060321     c                   eval      parfil = v1cfil
028300060321     c                   eval      parsc01 = v1csc1
028400060321     c                   eval      parsc02 = v1csc2
028500060321     c                   eval      parsc03 = v1csc3
028600060321     c                   eval      parsc04 = v1csc4
028700060321     c                   eval      parfile = v1ctpel
028800060323     c                   eval      parord = v1cord
028900060321     c                   eval      parabi = wabi
029000060320     c                   eval      kpjbu = parm01
029100060320     c                   eval      kcoaz = 'TA58'
029200060321     c                   call      'BCH10'
029300060321     c*****              call      'TNTA58R'
029400060320     c                   parm                    kpjba
029500050408
029600060320     c                   endsr
029700050408      *------------------------------------------------------------------------*
029800050408      * ROUTINE INIZIALE
029900050408      *------------------------------------------------------------------------*
030000060320     c     *inzsr        begsr
030100050408
030200060320     c     *entry        plist
030300060320     c                   parm                    kpjba
030400050510
030500050510     c     *dtaara       define    §azute        azuteds
030600050510     c     *dtaara       define    §datiute      ddatiute
030700050510
030800060320     c                   in(e)     *dtaara
030900060320     c                   if        %error or rsut = *blanks
031000060320     c                   clear                   tibs34ds
031100050510     c                   call      'TIBS34R'
031200060320     c                   parm                    tibs34ds
031300050510     c                   in        *dtaara
031400060320     c                   endif
031500060320
031600060320     c                   clear                   wabi
031700060320     c                   clear                   dlat
031800060320
031900060320      * verifica errori e autorità profilo
032000060320s   1c                   select
032100060320      * se ho errori nei dati utente esco dal pgm
032200060320w   1c                   when      duterr = 'E'
032300060320     c                   eval      *in08 = *on
032400060320     c                   eval      *in28 = *on
032500060320     c                   eval      v1cmsg = msg(01)
032600060320     c                   leavesr
032700060320      * se non c'è l'abilitazione
032800060320      * --> se 1° livello, abilitazioni al terminal
032900060320      *     se 2° livello, abilitazioni al punto operativo
033000060320      *     se sede è impossibile ma se capita mando a fine il pgm
033100060320w   1c                   when      uteaut = *Blanks
033200060320if  2c                   if        dutlpo = '1'
033300060320     c                   eval      wabi   = 'TP'
033400060320e   2c                   endif
033500060320if  2c                   if        dutlpo = '2'
033600060320     c                   eval      wabi   = 'PO'
033700060320e   2c                   endif
033800060320if  2c                   if        dutlpo = 'S'
033900060320     c                   eval      wabi   = 'AZ'
034000060320e   2c                   endif
034100060320      * carica le abilitazioni del profilo
034200060320x   1c                   other
034300060320     c                   movel     utefaf        dute01
034400060320if  2c                   if        §utegtc <> *Blanks
034500060320     c                   eval      wabi = §utegtc
034600060320   x2c                   else
034700060320     c                   eval      wabi = uteaut
034800060320e   2c                   endif
034900060320e   1c                   endsl
035000060320
035100060320      * controllo se ok l'abilitazione dell'utente
035200060320     c                   clear                   tibs02ds
035300060320     c                   eval      t02mod = 'C'
035400060320     c                   eval      t02sif = knsif
035500060320     c                   eval      t02cod = 'LAT'
035600060320     c                   movel(p)  wabi          t02ke1
035700060320     c                   call      'TIBS02R'
035800060320     c                   parm                    kpjba
035900060320     c                   parm                    Tibs02ds
036000060320     c                   if        t02err = *blanks
036100060320     c                   eval      dlat = t02uni
036200060320     c                   endif
036300060320      * errore o non abilitato
036400060320     c                   if        t02err <> *blanks or §latabi = 'S'
036500060320     c                   eval      *in08 = *on
036600060320     c                   eval      *in28 = *on
036700060320     c                   eval      v1cmsg = msg(01)
036800060320     c                   leavesr
036900060320     c                   endif
037000060320
037100060320      * reperimento dei p.o. gestibili dall'utente
037200060320     c                   clear                   trul31ds
037300060320     c                   eval      i31abi = wabi
037400060320     c                   eval      i31cdi = dutdis
037500060320     c                   eval      i31car = dutare
037600060320     c                   eval      i31cpo = dutpou
037700060320     c                   call      'TRUL31R'
037800060320     c                   parm                    kpjba
037900060320     c                   parm                    trul31ds
038000060320     c                   if        o31pog > *zeros
038100060320     c                   movea     o31pog        skpog
038200060320     c                   else
038300060320     c                   eval      *in08 = *on
038400060320     c                   eval      *in28 = *on
038500060320     c                   eval      v1cmsg = msg(01)
038600060320     c                   leavesr
038700060320     c                   endif
038800060320
038900060320     c                   eval      *in12 = (simfel = *zeros)
039000050408
039100060320     c                   endsr
039200050408      *
039300060320** MSG  Lungh. 78                                                            *
039400060320Utente non autorizzato alla funzione                                          01
039500060320Punto operativo in gestione ad altro p.o.                                     02
039600060320Punto operativo errato                                                        03
039700060320Punto Operativo valorizzato non compatibile con aree selezionate              04
039800060320Codice Area non in gestione all'utente                                        05
039900060321Effettuare almeno una scelta                                                  06
