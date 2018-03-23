000100060317     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060317
000300060317      *------------------------------------------------------------------------*
000400060317      *
000500060927      *  Pulizia Tariffe Obsolete
000600060317      *
000700060317      *------------------------------------------------------------------------*
000800060317
000900060320     fazorg01l  if   e           k disk
001000060320     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001100060927     ftnta92d   cf   e             workstn
001200060317
001300060317      *------------------------------------------------------------------------*
001400060317      *  RIEPILOGO INDICATORI
001500060317      *------------------------------------------------------------------------*
001600060320      * 08 - utente non abilitato
001700060320      * 12 - utente di sede
001800060320      * 28 - errore generico
001900060320      * 30 - comodo
002000060320      * 40 - errore p.o.
002100060927      * 42 - Data errata
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
004700060927     d  pardus                34     41  0
004800060927     d  parfile               42     42
004900060929     d  parabi                43     44
005000060320
005100060320     d ds_car          ds
005200060320     d  v1car1
005300060320     d  v1car2
005400060320     d  v1car3
005500060320     d  v1car4
005600060320     d  v1car5
005700060320     d  v1car6
005800060320     d  v1car7
005900060320     d  v1car8
006000060320     d  v1car9
006100060320     d  v1car0
006200060927      * Parametri per controllo data e transform in giorni - XSRDA8 -
006300060927     d WLBdat          ds
006400060927     d  G02dat                        8  0
006500060927     d  G02inv                        8  0
006600060927     d  G02err                        1
006700060927     d  G02tgi                        5  0
006800060317
006900060320     d azuteds       e ds                  extname(azute00f)
007000060317     d ddatiute      e ds
007100060320     d dlat          e ds
007200060320     d dute01        e ds
007300060317     d kpjba         e ds
007400060320     d tibs02ds      e ds
007500060317     d tibs34ds      e ds
007600060320     d trul31ds      e ds
007700060317
007800060317     d                sds
007900060317     d  vtcpgm                 1     10
008000050408      *------------------------------------------------------------------------*
008100050408
008200060320      * pulizia videata
008300060320     c                   exsr      sr_pulvid
008400050408
008500060320      * emissione videata
008600060320     c                   do        *hival
008700060927     c                   exfmt     ta92d01
008800050408
008900060320      * f3=fine
009000060320     c   kc              leave
009100050408
009200060320     c                   setoff                                       28
009300050408
009400060320      * controlli videata
009500060320     c                   exsr      sr_contr
009600060320     c   28              iter
009700050408
009800060320      * f6=conferma
009900060320     c                   if        *inkf = *on
010000060320     c                   exsr      sr_conferma
010100060320     c                   leave
010200060320     c                   endif
010300050408
010400060320     c                   enddo
010500050408
010600060320     c                   eval      *inlr = *on
010700060320
010800050408      *------------------------------------------------------------------------*
010900050408      * PULIZIA VIDEATA
011000050408      *------------------------------------------------------------------------*
011100060320     c     sr_pulvid     begsr
011200050408
011300060320     c                   clear                   v1cfil
011400060320     c                   eval      v1dfil = 'Tutti'
011500060320     c                   clear                   ds_car
011600060320
011700060320     c                   select
011800060320      * se abilitazione distretto faccio vedere le aree gestite
011900060929     c                   when      wabi = 'DI' or wabi = 'AZ'
012000060320     c                   exsr      sr_cararee
012100060320     c                   eval      *in46 = *on
012200060320
012300060320      * imposto il p.o. da elaborare
012400060320     c                   when      not *in12
012500060320     c                   movel     dutpou        v1cfil
012600060320     c     v1cfil        chain     azorg01l
012700060320     c                   if        %found(azorg01l) and orgfva = *blanks
012800060320     c                   eval      v1dfil = orgdes
012900060320     c                   endif
013000060320     c                   endsl
013100060320
013200050408
013300060320     c                   endsr
013400060320
013500060320      *---------------------------------------------------------------*
013600060320      * CARICO LE AREE IN GESTIONE AL DISTRETTO
013700060320      *---------------------------------------------------------------*
013800060320     c     sr_cararee    begsr
013900060320
014000060320     c                   clear                   car
014100060320
014200060320     c     dutdis        setll     azorg02l
014300060320     c                   do        *hival
014400060320     c     dutdis        reade     azorg02l
014500060320     c                   if        %eof(azorg02l)
014600060320     c                   leave
014700060320     c                   endif
014800060320     c                   if        orgfva <> *blanks or
014900060320     c                             (orgfag <> 'A' and orgfag <> 'F')
015000060320     c                   iter
015100060320     c                   endif
015200060320
015300060320     c                   move      orgcar        w003a
015400060320     c     w003a         lookup    car                                    30
015500060320     c                   if        not *in30
015600060320     c                   eval      xx = 1
015700060320     c     *blanks       lookup    car(xx)                                30
015800060320     c                   if        *in30
015900060320     c                   eval      car(xx) = w003a
016000060320     c                   endif
016100060320     c                   endif
016200060320
016300060320     c                   enddo
016400060320
016500060320     c                   eval      v1car1 = car(1)
016600060320     c                   eval      v1car2 = car(2)
016700060320     c                   eval      v1car3 = car(3)
016800060320     c                   eval      v1car4 = car(4)
016900060320     c                   eval      v1car5 = car(5)
017000060320     c                   eval      v1car6 = car(6)
017100060320     c                   eval      v1car7 = car(7)
017200060320     c                   eval      v1car8 = car(8)
017300060320     c                   eval      v1car9 = car(9)
017400060320     c                   eval      v1car0 = car(10)
017500060320
017600060320     c                   endsr
017700060320
017800050408      *------------------------------------------------------------------------*
017900050408      * CONTROLLI VIDEO
018000050408      *------------------------------------------------------------------------*
018100060320     c     sr_contr      begsr
018200060320
018300060320      * se utente non abilitato riemetto la videata con l'errore
018400060320     c                   if        *in08
018500060320     c                   eval      *in28 = *on
018600060320     c                   leavesr
018700060320     c                   endif
018800050408
018900060320      * reset indicatori
019000060927     c                   setoff                                       404247
019100060320     c                   setoff                                       484950
019200060320     c                   setoff                                       515253
019300060320     c                   setoff                                       545556
019400060320
019500060320      * controllo il p.o. se immesso
019600060320     c                   if        v1cfil > *zeros
019700060320     c                   move      v1cfil        w003a
019800060320      * controllo se il p.o. è valido
019900060320     c     v1cfil        chain     azorg01l
020000060320     c                   if        not %found(azorg01l) or orgfva <> *blanks
020100060320     c                   eval      *in28 = *on
020200060320     c                   eval      *in40 = *on
020300060320     c                   eval      v1cmsg = msg(03)
020400060320     c                   leavesr
020500060320     c                   endif
020600060320     c                   if        orgfag <> 'A' and orgfag <> 'F'
020700060320     c                   eval      *in28 = *on
020800060320     c                   eval      *in40 = *on
020900060320     c                   eval      v1cmsg = msg(03)
021000060320     c                   leavesr
021100060320     c                   endif
021200060320      * controllo se il p.o. è in gestione
021300060320     c     w003a         lookup    skpog                                  30
021400060320     c                   if        not *in30
021500060320     c                   eval      *in28 = *on
021600060320     c                   eval      *in40 = *on
021700060320     c                   eval      v1cmsg = msg(02)
021800060320     c                   leavesr
021900060320     c                   endif
022000060321     c                   eval      v1dfil = orgdes
022100060320     c                   else
022200060320     c                   eval      v1dfil ='Tutti'
022300060320     c                   endif
022400060320
022500060320      * se immesso il p.o. non posso immettere le aree e viceversa
022600060320     c                   if        v1cfil <> *zeros and ds_car <> *blanks
022700060320     c                   eval      *in28 = *on
022800060320     c                   eval      *in40 = *on
022900060320     c                   eval      *in47 = *on
023000060320     c                   eval      v1cmsg = msg(04)
023100060320     c                   leavesr
023200060320     c                   endif
023300060320
023400060320      * controllo le aree immesse
023500060321     c                   movea     ds_car        parcar
023600060320      * Ricerca
023700060320     c                   do        10            xx
023800060320     c                   eval      yy = (46 + xx)
023900060321     c                   if        parcar(xx) <> *blanks
024000060321     c     parcar(xx)    lookup    car                                    30
024100060320     c                   if        not *in30
024200060320     c                   eval      v1cmsg = msg(05)
024300060320     c                   eval      *in(yy) = *on
024400060320     c                   eval      *in28 = *on
024500060320     c                   endif
024600060320     c                   endif
024700060320     c                   enddo
024800060320
024900060321     c                   move      parcar(1)     v1car1
025000060321     c                   move      parcar(2)     v1car2
025100060321     c                   move      parcar(3)     v1car3
025200060321     c                   move      parcar(4)     v1car4
025300060321     c                   move      parcar(5)     v1car5
025400060321     c                   move      parcar(6)     v1car6
025500060321     c                   move      parcar(7)     v1car7
025600060321     c                   move      parcar(8)     v1car8
025700060321     c                   move      parcar(9)     v1car9
025800060321     c                   move      parcar(10)    v1car0
025900060320     c   28              leavesr
026000060927
026100060927      * controllo data ultima spediizone fatturata
026200050408
026300060927     c                   clear                   wlbdat
026400060927     c                   z-add     v1cdus        g02dat
026500060927     c                   clear                   g02err
026600060927     c                   call      'XSRDA8'
026700060927     c                   parm                    wlbdat
026800060927     c                   If        g02err = '1'
026900060927     c                   eval      v1cmsg = msg(06)
027000060927     c                   eval      *in42   = *on
027100060927     c                   eval      *in28 = *on
027200060927     c                   endif
027300060927     c                   z-add     g02inv        pardus
027400060927
027500060927     c                   z-add     g02dat        v1cdus
027600060927
027700060927     c                   endsr
027800060320
027900050408      *------------------------------------------------------------------------*
028000060320      * RICHIAMA IL PROGRAMMA BATCH
028100050408      *------------------------------------------------------------------------*
028200060320     c     sr_conferma   begsr
028300050408
028400060929     c                   eval      parfil = v1cfil
028500061002     c                   eval      parfile = 'F'
028600060929     c                   eval      parabi = wabi
028700060320     c                   eval      kpjbu = parm01
028800060927     c                   eval      kcoaz = 'TA93'
028900061003     c                   call      'BCH10'
029000061003     c******             call      'TNTA93R'
029100060320     c                   parm                    kpjba
029200050408
029300060320     c                   endsr
029400050408      *------------------------------------------------------------------------*
029500050408      * ROUTINE INIZIALE
029600050408      *------------------------------------------------------------------------*
029700060320     c     *inzsr        begsr
029800050408
029900060320     c     *entry        plist
030000060320     c                   parm                    kpjba
030100050510
030200050510     c     *dtaara       define    §azute        azuteds
030300050510     c     *dtaara       define    §datiute      ddatiute
030400050510
030500060320     c                   in(e)     *dtaara
030600060320     c                   if        %error or rsut = *blanks
030700060320     c                   clear                   tibs34ds
030800050510     c                   call      'TIBS34R'
030900060320     c                   parm                    tibs34ds
031000050510     c                   in        *dtaara
031100060320     c                   endif
031200060320
031300060320     c                   clear                   wabi
031400060320     c                   clear                   dlat
031500060320
031600060320      * verifica errori e autorità profilo
031700060320s   1c                   select
031800060320      * se ho errori nei dati utente esco dal pgm
031900060320w   1c                   when      duterr = 'E'
032000060320     c                   eval      *in08 = *on
032100060320     c                   eval      *in28 = *on
032200060320     c                   eval      v1cmsg = msg(01)
032300060320     c                   leavesr
032400060320      * se non c'è l'abilitazione
032500060320      * --> se 1° livello, abilitazioni al terminal
032600060320      *     se 2° livello, abilitazioni al punto operativo
032700060320      *     se sede è impossibile ma se capita mando a fine il pgm
032800060320w   1c                   when      uteaut = *Blanks
032900060320if  2c                   if        dutlpo = '1'
033000060320     c                   eval      wabi   = 'TP'
033100060320e   2c                   endif
033200060320if  2c                   if        dutlpo = '2'
033300060320     c                   eval      wabi   = 'PO'
033400060320e   2c                   endif
033500060320if  2c                   if        dutlpo = 'S'
033600060320     c                   eval      wabi   = 'AZ'
033700060320e   2c                   endif
033800060320      * carica le abilitazioni del profilo
033900060320x   1c                   other
034000060320     c                   movel     utefaf        dute01
034100060320if  2c                   if        §utegtc <> *Blanks
034200060320     c                   eval      wabi = §utegtc
034300060320   x2c                   else
034400060320     c                   eval      wabi = uteaut
034500060320e   2c                   endif
034600060320e   1c                   endsl
034700060320
034800060320      * controllo se ok l'abilitazione dell'utente
034900060320     c                   clear                   tibs02ds
035000060320     c                   eval      t02mod = 'C'
035100060320     c                   eval      t02sif = knsif
035200060320     c                   eval      t02cod = 'LAT'
035300060320     c                   movel(p)  wabi          t02ke1
035400060320     c                   call      'TIBS02R'
035500060320     c                   parm                    kpjba
035600060320     c                   parm                    Tibs02ds
035700060320     c                   if        t02err = *blanks
035800060320     c                   eval      dlat = t02uni
035900060320     c                   endif
036000060320      * errore o non abilitato
036100060320     c                   if        t02err <> *blanks or §latabi = 'S'
036200060320     c                   eval      *in08 = *on
036300060320     c                   eval      *in28 = *on
036400060320     c                   eval      v1cmsg = msg(01)
036500060320     c                   leavesr
036600060320     c                   endif
036700060320
036800060320      * reperimento dei p.o. gestibili dall'utente
036900060320     c                   clear                   trul31ds
037000060320     c                   eval      i31abi = wabi
037100060320     c                   eval      i31cdi = dutdis
037200060320     c                   eval      i31car = dutare
037300060320     c                   eval      i31cpo = dutpou
037400060320     c                   call      'TRUL31R'
037500060320     c                   parm                    kpjba
037600060320     c                   parm                    trul31ds
037700060320     c                   if        o31pog > *zeros
037800060320     c                   movea     o31pog        skpog
037900060320     c                   else
038000060320     c                   eval      *in08 = *on
038100060320     c                   eval      *in28 = *on
038200060320     c                   eval      v1cmsg = msg(01)
038300060320     c                   leavesr
038400060320     c                   endif
038500060320
038600060320     c                   eval      *in12 = (simfel = *zeros)
038700050408
038800060320     c                   endsr
038900050408      *
039000060320** MSG  Lungh. 78                                                            *
039100060320Utente non autorizzato alla funzione                                          01
039200060320Punto operativo in gestione ad altro p.o.                                     02
039300060320Punto operativo errato                                                        03
039400060320Punto Operativo valorizzato non compatibile con aree selezionate              04
039500060320Codice Area non in gestione all'utente                                        05
039600060927Data Errata                                                                   06
