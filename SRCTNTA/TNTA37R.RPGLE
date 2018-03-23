000100060317     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060317
000300060317      *------------------------------------------------------------------------*
000400060317      *
000500060317      *  Stato avanzamento progetto AC BASE
000600060317      *
000700060317      *------------------------------------------------------------------------*
000800060320     fazorg01l  if   e           k disk
000900060320     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001000111122     ftnta37d   cf   e             workstn
001100060317
001200060317      *------------------------------------------------------------------------*
001300060317      *  RIEPILOGO INDICATORI
001400060317      *------------------------------------------------------------------------*
001500060320      * 08 - utente non abilitato
001600060320      * 12 - utente di sede
001700060320      * 28 - errore generico
001800060320      * 30 - comodo
001900060320      * 40 - errore p.o.
002000060321      * 41 - errore selezioni
002100060320      * 46 - autorizzazione distretto
002200060320      * 47/56 - errore sulle aree
002300060317      *------------------------------------------------------------------------*
002400060317
002500060317      *------------------------------------------------------------------------*
002600060317      *   V A R I A B I L I
002700060317      *------------------------------------------------------------------------*
002800090708     d wabi            s                   like(uteaut)
002900060320     d w003a           s              3
003000060320     d xx              s              3  0
003100060320     d yy              s              3  0
003200060317
003300060317      *------------------------------------------------------------------------*
003400060317      *   S C H I E R E
003500060317      *------------------------------------------------------------------------*
003600070531     d car             s              3    dim(20)
003700060320     d msg             s             78    dim(10) ctdata perrcd(1)
003800060317
003900060317      *------------------------------------------------------------------------*
004000060317      *   D S   I N T E R N E / E S T E R N E
004100060317      *------------------------------------------------------------------------*
004200060317     d parm01          ds
004300060321     d  parfil                 1      3  0
004400060321     d  parcar                 4     33    dim(10)
004500060321     d  parsc01               34     34
004600060321     d  parsc02               35     35
004700060321     d  parsc03               36     36
004800060321     d  parsc04               37     37
004900060323     d  parfile               38     38
005000060323     d  parord                39     39
005100060323     d  parabi                40     41
005200060320
005300060320     d ds_car          ds
005400060320     d  v1car1
005500060320     d  v1car2
005600060320     d  v1car3
005700060320     d  v1car4
005800060320     d  v1car5
005900060320     d  v1car6
006000060320     d  v1car7
006100060320     d  v1car8
006200060320     d  v1car9
006300060320     d  v1car0
006400060317
006500060320     d azuteds       e ds                  extname(azute00f)
006600060317     d ddatiute      e ds
006700060317     d kpjba         e ds
006800060320     d tibs02ds      e ds
006900060317     d tibs34ds      e ds
007000090708     d tntaa1ds      e ds
007100060317
007200060317     d                sds
007300060317     d  vtcpgm                 1     10
007400050408      *------------------------------------------------------------------------*
007500050408
007600060320      * pulizia videata
007700060320     c                   exsr      sr_pulvid
007800050408
007900060320      * emissione videata
008000060320     c                   do        *hival
008100060320     c                   exfmt     ta57d01
008200050408
008300060320      * f3=fine
008400090708     c                   if        *inkc or *in08
008500090708     c                   leave
008600090708     c                   endif
008700050408
008800060320     c                   setoff                                       28
008900050408
009000060320      * controlli videata
009100060320     c                   exsr      sr_contr
009200060320     c   28              iter
009300050408
009400060320      * f6=conferma
009500060320     c                   if        *inkf = *on
009600060320     c                   exsr      sr_conferma
009700060320     c                   leave
009800060320     c                   endif
009900050408
010000060320     c                   enddo
010100090708     C*
010200090708     C                   clear                   tntaa1ds
010300090708     C                   movel     'C'           Itaa1Tla
010400090708     C                   movel(p)  tntaa1ds      kpjbu
010500090708     C                   CALL      'TNTAA1C'
010600090708     C                   PARM                    KPJBA
010700090708     c
010800060320     c                   eval      *inlr = *on
010900060320
011000050408      *------------------------------------------------------------------------*
011100050408      * PULIZIA VIDEATA
011200050408      *------------------------------------------------------------------------*
011300060320     c     sr_pulvid     begsr
011400050408
011500060320     c                   clear                   v1cfil
011600060320     c                   eval      v1dfil = 'Tutti'
011700060320     c                   clear                   ds_car
011800060320
011900060320     c                   select
012000060320      * se abilitazione distretto faccio vedere le aree gestite
012100060320     c                   when      wabi = 'DI'
012200060320     c                   exsr      sr_cararee
012300060320     c                   eval      *in46 = *on
012400060320
012500060320      * imposto il p.o. da elaborare
012600060320     c                   when      not *in12
012700060320     c                   movel     dutpou        v1cfil
012800060320     c     v1cfil        chain     azorg01l
012900060320     c                   if        %found(azorg01l) and orgfva = *blanks
013000060320     c                   eval      v1dfil = orgdes
013100060320     c                   endif
013200060320     c                   endsl
013300060320
013400060320     c                   clear                   v1csc1
013500060320     c                   clear                   v1csc2
013600060320     c                   clear                   v1csc3
013700060320     c                   clear                   v1csc4
013800060320
013900060320     c                   eval      v1ctpel = 'S'
014000060322     c                   eval      v1cord = 'C'
014100050408
014200060320     c                   endsr
014300060320
014400060320      *---------------------------------------------------------------*
014500060320      * CARICO LE AREE IN GESTIONE AL DISTRETTO
014600060320      *---------------------------------------------------------------*
014700060320     c     sr_cararee    begsr
014800060320
014900060320     c                   clear                   car
015000060320
015100060320     c     dutdis        setll     azorg02l
015200060320     c                   do        *hival
015300060320     c     dutdis        reade     azorg02l
015400060320     c                   if        %eof(azorg02l)
015500060320     c                   leave
015600060320     c                   endif
015700060320     c                   if        orgfva <> *blanks or
015800060320     c                             (orgfag <> 'A' and orgfag <> 'F')
015900060320     c                   iter
016000060320     c                   endif
016100060320
016200060320     c                   move      orgcar        w003a
016300060320     c     w003a         lookup    car                                    30
016400060320     c                   if        not *in30
016500060320     c                   eval      xx = 1
016600060320     c     *blanks       lookup    car(xx)                                30
016700060320     c                   if        *in30
016800060320     c                   eval      car(xx) = w003a
016900060320     c                   endif
017000060320     c                   endif
017100060320
017200060320     c                   enddo
017300060320
017400060320     c                   eval      v1car1 = car(1)
017500060320     c                   eval      v1car2 = car(2)
017600060320     c                   eval      v1car3 = car(3)
017700060320     c                   eval      v1car4 = car(4)
017800060320     c                   eval      v1car5 = car(5)
017900060320     c                   eval      v1car6 = car(6)
018000060320     c                   eval      v1car7 = car(7)
018100060320     c                   eval      v1car8 = car(8)
018200060320     c                   eval      v1car9 = car(9)
018300060320     c                   eval      v1car0 = car(10)
018400060320
018500060320     c                   endsr
018600060320
018700050408      *------------------------------------------------------------------------*
018800050408      * CONTROLLI VIDEO
018900050408      *------------------------------------------------------------------------*
019000060320     c     sr_contr      begsr
019100060320
019200060320      * se utente non abilitato riemetto la videata con l'errore
019300060320     c                   if        *in08
019400060320     c                   eval      *in28 = *on
019500060320     c                   leavesr
019600060320     c                   endif
019700050408
019800060320      * reset indicatori
019900060321     c                   setoff                                       404147
020000060320     c                   setoff                                       484950
020100060320     c                   setoff                                       515253
020200060320     c                   setoff                                       545556
020300060320
020400060320      * controllo il p.o. se immesso
020500060320     c                   if        v1cfil > *zeros
020600060320      * controllo se il p.o. è valido
020700060320     c     v1cfil        chain     azorg01l
020800060320     c                   if        not %found(azorg01l) or orgfva <> *blanks
020900060320     c                   eval      *in28 = *on
021000060320     c                   eval      *in40 = *on
021100060320     c                   eval      v1cmsg = msg(03)
021200060320     c                   leavesr
021300060320     c                   endif
021400060320     c                   if        orgfag <> 'A' and orgfag <> 'F'
021500060320     c                   eval      *in28 = *on
021600060320     c                   eval      *in40 = *on
021700060320     c                   eval      v1cmsg = msg(03)
021800060320     c                   leavesr
021900060320     c                   endif
022000090708     c
022100090708     c* controllo abilitaizoni utente
022200090708     c                   clear                   tntaa1ds
022300090708     c                   eval      itaa1caut='§utegtc'
022400090708     c                   movel     v1cfil        itaa1fil
022500090708     c                   clear                   kpjbu
022600090708     c                   movel     tntaa1ds      kpjbu
022700090708     c                   call      'TNTAA1C'
022800090708     c                   parm                    kpjba
022900090708     c                   movel     kpjbu         tntaa1ds
023000090708     c
023100090708    2c                   if        otaa1fabi='N'
023200090708     c
023300060320      * controllo se il p.o. è in gestione
023400090708     c**   w003a         lookup    skpog                                  30
023500090708     c**                 if        not *in30
023600090708     c
023700060320     c                   eval      *in28 = *on
023800060320     c                   eval      *in40 = *on
023900060320     c                   eval      v1cmsg = msg(02)
024000060320     c                   leavesr
024100060320     c                   endif
024200060321     c                   eval      v1dfil = orgdes
024300060320     c                   else
024400060320     c                   eval      v1dfil ='Tutti'
024500060320     c                   endif
024600060320
024700060320      * se immesso il p.o. non posso immettere le aree e viceversa
024800060320     c                   if        v1cfil <> *zeros and ds_car <> *blanks
024900060320     c                   eval      *in28 = *on
025000060320     c                   eval      *in40 = *on
025100060320     c                   eval      *in47 = *on
025200060320     c                   eval      v1cmsg = msg(04)
025300060320     c                   leavesr
025400060320     c                   endif
025500060320
025600060320      * controllo le aree immesse
025700060321     c                   movea     ds_car        parcar
025800060320      * Ricerca
025900060320     c                   do        10            xx
026000060320     c                   eval      yy = (46 + xx)
026100060321     c                   if        parcar(xx) <> *blanks
026200060321     c     parcar(xx)    lookup    car                                    30
026300060320     c                   if        not *in30
026400060320     c                   eval      v1cmsg = msg(05)
026500060320     c                   eval      *in(yy) = *on
026600060320     c                   eval      *in28 = *on
026700060320     c                   endif
026800060320     c                   endif
026900060320     c                   enddo
027000060320
027100060321     c                   move      parcar(1)     v1car1
027200060321     c                   move      parcar(2)     v1car2
027300060321     c                   move      parcar(3)     v1car3
027400060321     c                   move      parcar(4)     v1car4
027500060321     c                   move      parcar(5)     v1car5
027600060321     c                   move      parcar(6)     v1car6
027700060321     c                   move      parcar(7)     v1car7
027800060321     c                   move      parcar(8)     v1car8
027900060321     c                   move      parcar(9)     v1car9
028000060321     c                   move      parcar(10)    v1car0
028100060320     c   28              leavesr
028200060321
028300060321      * almeno una scelta deve essere fatta
028400060321     c                   if        v1csc1 = *blanks and v1csc2 = *blanks and
028500060323     c                             v1csc3 = *blanks and v1csc4 = *blanks
028600090708     c                   eval      *in28 = *on
028700060321     c                   eval      *in41 = *on
028800060321     c                   eval      v1cmsg = msg(06)
028900060321     c                   leavesr
029000060321     c                   endif
029100050408
029200060320     c                   endsr
029300060320
029400050408      *------------------------------------------------------------------------*
029500060320      * RICHIAMA IL PROGRAMMA BATCH
029600050408      *------------------------------------------------------------------------*
029700060320     c     sr_conferma   begsr
029800050408
029900060321     c                   eval      parfil = v1cfil
030000060321     c                   eval      parsc01 = v1csc1
030100060321     c                   eval      parsc02 = v1csc2
030200060321     c                   eval      parsc03 = v1csc3
030300060321     c                   eval      parsc04 = v1csc4
030400060321     c                   eval      parfile = v1ctpel
030500060323     c                   eval      parord = v1cord
030600060321     c                   eval      parabi = wabi
030700060320     c                   eval      kpjbu = parm01
030800111122     c****               eval      kcoaz = 'TA58'
030900111122     c****               call      'BCH10'
031000111122     c                   call      'TNTA38R'
031100060320     c                   parm                    kpjba
031200050408
031300060320     c                   endsr
031400050408      *------------------------------------------------------------------------*
031500050408      * ROUTINE INIZIALE
031600050408      *------------------------------------------------------------------------*
031700060320     c     *inzsr        begsr
031800050408
031900060320     c     *entry        plist
032000060320     c                   parm                    kpjba
032100050510
032200050510     c     *dtaara       define    §azute        azuteds
032300050510     c     *dtaara       define    §datiute      ddatiute
032400050510
032500060320     c                   in(e)     *dtaara
032600060320     c                   if        %error or rsut = *blanks
032700060320     c                   clear                   tibs34ds
032800050510     c                   call      'TIBS34R'
032900060320     c                   parm                    tibs34ds
033000050510     c                   in        *dtaara
033100060320     c                   endif
033200090708
033300090708     c* Richiamo solo per avere codice di abilitazione
033400090708     c                   clear                   tntaa1ds
033500090708     c                   eval      itaa1caut='§utegtc'
033600090708     c                   clear                   kpjbu
033700090708     c                   movel     tntaa1ds      kpjbu
033800090708     c                   call      'TNTAA1C'
033900090708     c                   parm                    kpjba
034000090708     c                   movel     kpjbu         tntaa1ds
034100090708     C* UTENTE NON abilitato
034200090708     c                   if        otaa1fabi='N'
034300090708     c                   eval      *in08 = *on
034400090708     c                   eval      *in28 = *on
034500090708     c                   eval      v1cmsg = msg(01)
034600090708     c                   endif
034700090708     c
034800090708     c                   eval      wabi=otaa1cabi
034900060320
035000060320     c                   eval      *in12 = (simfel = *zeros)
035100050408
035200060320     c                   endsr
035300050408      *
035400060320** MSG  Lungh. 78                                                            *
035500090708UTENTE NON ABILITATO ALLA GESTIONE TARIFFE/OFFERTE                            01
035600090708Filiale non in gestione all'utente                                            02
035700090708Filiale  errata                                                               03
035800090708filiale immessa  non compatibile con aree selezionate                         04
035900060320Codice Area non in gestione all'utente                                        05
036000060321Effettuare almeno una scelta                                                  06
