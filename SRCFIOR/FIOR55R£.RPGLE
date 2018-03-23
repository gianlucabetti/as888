000100071204     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000200061227
000300061227      *------------------------------------------------------------------------*
000400061227      *
000500061227      *          PREPARAZIONE DATI PER PDA AUT
000600061227      *
000700061227      *------------------------------------------------------------------------*
000800061227
000900061227     ffior55d   cf   e             workstn
001000061227     fazorg01l  if   e           k disk
001100071204     ffidst09l  if   e           k disk
001200071204     ffidst08l  if   e           k disk    rename(fidst000:fidst08)
001300061227
001400061227      *------------------------------------------------------------------------*
001500061227      *  RIEPILOGO INDICATORI
001600061227      *------------------------------------------------------------------------*
001700071204      * 01 - cambio data
001800070118      * 04 - utente EDP
001900061227      * 05 - sproteggo p.o. in gestione
002000061227      * 06 - non abilito f18
002100061227      * 28 - errore generico
002200061227      * 30 - comodo
002300061227      * 40 - errore p.o. in gestione
002400061227      * 41 - errore data distinta
002500071204      * 42 - errore num. distinta dal
002600071204      * 43 - errore num. distinta al
002700080715      * 44 - errore dati da preparare
002800061227      *------------------------------------------------------------------------*
002900061227
003000061227      *   V A R I A B I L I
003100061227     d dataiso         s               d   datfmt(*iso)
003200061227     d dataeur         s               d   datfmt(*eur)
003300071204     d data_oggi       s              8  0
003400071204     d kdstnpg         s                   like(dstnpg) inz(4)
003500071206     d xx              s              3  0
003600061227     d wdata           s              8  0
003700070124     d wforza          s              1    inz(*off)
003800071204     d wndcdal         s                   like(dstnfv)
003900120521     d savfgs          s                   like(v1cfgs)
004000120521     d savddc          s                   like(v1cddc)
004100120521     d savndcda        s                   like(v1ndcda)
004200120521     d savndcal        s                   like(v1ndcal)
004300071206     d $fv6            s              6p 0 dim(600)
004400130621
004500130621     d wabi            s                   like(UTEaut)
004600130621     d wEsci           s               n   inz(*off)
004700130621     d wErrGrave       s               n   inz(*off)
004800130621     d w003a           s              3
004900061227
005000061227      *   S C H I E R E
005100130621     d**** l1              s              3  0 dim(30)
005200130621     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
005300110201     d msg             s             78    dim(20) ctdata perrcd(1)
005400061227
005500061227      *   D S   I N T E R N E / E S T E R N E
005600061227     d wlbdat          ds                  inz
005700061227     d  g02dat                 1      8  0
005800061227     d  g02inv                 9     16  0
005900061227     d  g02err                17     17
006000061227     d  g02tgi                18     22  0
006100061227
006200061227     d azuteds       e ds                  extname(azute00f)
006300061227     d ddatiute      e ds
006400071204     d fidg31ds      e ds
006500080715     d fidg41ds      e ds
006600070115     d fior56ds      e ds
006601160504     d ddstflr       e ds
006700080519     d fior90ds      e ds
006800061227     d kpjba         e ds
006900061227     d og148         e ds
007000080201     d og150         e ds
007100070118     d tibs02ds      e ds
007200061227     d tibs34ds      e ds                  inz
007300130621     d**** trul06ds      e ds                  extname(trul06ds)
007400130621     d****  lin                    1     90  0 dim(30)
007500130621     d TRUL31DS      e ds
007600130621
007700130621     d dLAT          e ds
007800130621     d dUTE01        e ds
007900061227
008000061227     d                sds
008100061227     d  vtcpgm                 1     10
008200071204
008300071204      *   C O S T A N T I
008400071204     d digitn          c                   const('1234567890')
008500061227
008600061227      *------------------------------------------------------------------------*
008700061227
008800061227      * gestione videata
008900061227     c                   do        *hival
009000130621
009100130621      /free
009200130621       //?Se utente non autorizzato alla funzione esco
009300130621         IF  wErrGrave;
009400130621           *in28 = *on;
009500130621           wEsci = *on;
009600130621           V1Cmsg = 'Utente non autorizzato alla funzione';
009700130621         ENDIF;
009800130621      /end-free
009900061227     c                   exfmt     or55d01
010000130621
010100061227     c                   setoff                                       284041
010200080715     c                   setoff                                       44
010300130621
010400130621      /free
010500130621         IF  wEsci;
010600130621           leave;
010700130621         ENDIF;
010800130621      /end-free
010900120521     c                   if        V1CFGS <> savfgs or
011000120521     c                             V1CDDC <> savddc or
011100120521     c                             (V1NDCDA<> savndcda AND
011200120521     c                             %scan('?':v1ndcda)<=0) OR
011300120521     c                             V1NDCAL<> savndcal
011400120521     c                   setoff                                       45
011500120521     c                   eval      savfgs = v1cfgs
011600120521     c                   eval      savddc = v1cddc
011700120521     c                   eval      savndcda = v1ndcda
011800120521     c                   eval      savndcal = v1ndcal
011900120521     c                   end
012000071204      * f1=cambio data
012100071204     c   ka              eval      *in01 = *on
012200071204     c   ka              eval      *in41 = *on
012300071204     c   ka              iter
012400071204
012500061227      * f3=fine
012600061227     c   kc              leave
012700061227
012800061227      * f18=cambio p.p. in gestione
012900061227     c   ks              eval      *in05 = *on
013000061227     c   ks              iter
013100061227
013200061227      * controllo p.o. in gestione
013300061227     c                   exsr      sr_contrfgs
013400061227     c  n28              eval      *in05 = *off
013500061227     c   28              iter
013600080715
013700080715      * controllo il tipo preparazione
013800080715     c                   if        v1ctipo = *blanks
013900080715     c                   seton                                        2844
014000080715     c                   eval      v1cmsg = msg(15)
014100080715     c                   iter
014200080715     c                   endif
014300110201      * controllo il tipo preparazione congruente con abilitazione filiale
014400110201     c                   if        v1ctipo = 'R' and §ogpdaorm = *blanks
014500110201     c                   seton                                        2844
014600110201     c                   eval      v1cmsg = msg(16)
014700110201     c                   eval      v1cmsg = %trim(v1cmsg) + ' Ritiri'
014800110201     c                   iter
014900110201     c                   endif
015000110201     c                   if        v1ctipo = 'C' and §ogpdacon = *blanks
015100110201     c                   seton                                        2844
015200110201     c                   eval      v1cmsg = msg(16)
015300110201     c                   eval      v1cmsg = %trim(v1cmsg) + ' Consegne'
015400110201     c                   iter
015500110201     c                   endif
015600110201     c                   if        v1ctipo = 'E' and
015700110201     c                             (§ogpdaorm = *blanks or §ogpdacon = *blanks)
015800110201     c                   seton                                        2844
015900110201     c                   eval      v1cmsg = msg(17)
016000110201     c                   IF        §ogpdaorm <> *blanks
016100110201     c                   eval      v1cmsg = %trim(v1cmsg) + ' Ritiri'
016200110201     c                   endif
016300110201     c                   IF        §ogpdacon <> *blanks
016400110201     c                   eval      v1cmsg = %trim(v1cmsg) + ' Consegne'
016500110201     c                   endif
016600110201     c                   iter
016700110201     c                   endif
016800061227
016900071205     c                   exsr      sr_contrddc
017000070118     c   28              iter
017100071204
017200071204     c                   exsr      sr_contrndc
017300071204     c   28              iter
017400061227
017500061227      * f6=conferma
017600061227     c                   if        *inkf
017700080715      * Ritiro o Entrambi
017800080715     c                   if        v1ctipo = 'R' or v1ctipo = 'E'
017900070115     c                   clear                   fior56ds
018000070119     c                   eval      or56tla = 'T'
018100070115     c                   movel     v1cfgs        or56fgs
018200070115     c                   movel     wdata         or56ddc
018300071204     c                   movel     wndcdal       or56ndcd
018400071204     c                   movel     v1ndcal       or56ndca
018500080201     c                   eval      or56tdis = v1tdis
018600080201     c                   eval      or56tusc = v1tusc
018700140120     c                   eval      OR56cmd = 'I'
018800070124     c                   eval      kpjbu = fior56ds
018900080212     c                   call      'FIOR56R'
019000061227     c                   parm                    kpjba
019100070124     c                   parm                    fior56ds
019200061227     c                   endif
019300080715      * Consegne o Entrambi
019400080715     c                   if        v1ctipo = 'C' or v1ctipo = 'E'
019500080715     c                   clear                   fidg41ds
019600080715     c                   eval      co41tla = 'T'
019700080715     c                   movel     v1cfgs        co41fgs
019800080715     c                   movel     wdata         co41ddc
019900080715     c                   movel     wndcdal       co41ndcd
020000080715     c                   movel     v1ndcal       co41ndca
020100080715     c                   eval      co41tdis = v1tdis
020200080715     c                   eval      co41tusc = v1tusc
020201160504     c                   eval      co41tur1 = v1tur1
020202160504     c                   eval      co41tur2 = v1tur2
020300080715     c                   eval      kpjbu = fidg41ds
020400080715     c                   call      'FIDG41R'
020500080715     c                   parm                    kpjba
020600080715     c                   parm                    fidg41ds
020700080715     c                   endif
020800080715
020900080715     c                   leave
021000080715     c                   endif
021100061227
021200061227     c                   enddo
021300061227
021400061227     c                   eval      *inlr = *on
021500061227
021600061227      *------------------------------------------------------------------------*
021700061227      * CONTROLLO IL P.O. IN IN GESTIONE
021800061227      *------------------------------------------------------------------------*
021900061227     c     sr_contrfgs   begsr
022000061227
022100061227     c                   setoff                                       40
022200061227     c                   clear                   v1dfgs
022300061227     c                   clear                   og148
022400080201     c                   clear                   og150
022500061227
022600130621     c**** v1cfgs        lookup    l1                                     30
022700130621     c                   move      V1Cfgs        w003a
022800130621     c     w003a         lookup    SKpog                                  30
022900061227     c                   if        not *in30
023000061227     c                   seton                                        2840
023100061227     c                   eval      v1cmsg = msg(01)
023200061227     c                   leavesr
023300061227     c                   endif
023400061227
023500061227      * decodifica p.o. in gestione
023600061227     c     v1cfgs        chain     azorg01L
023700061227     c                   if        %found(azorg01l) and orgfva = *blanks
023800061227     c                   eval      v1dfgs = orgdes
023900061227     c                   eval      og148 = orgde8
024000080201     c                   eval      og150 = orgdf0
024100061227     c                   endif
024200110201      * PDA attivo
024300110201     c                   if        §ogpdaorm = *blanks and
024400110201     c                             §ogpdacon = *blanks
024500061227     c                   seton                                        2840
024600061227     c                   eval      v1cmsg = msg(03)
024700061227     c                   leavesr
024800061227     c                   endif
024900080201      * visualizzo picking messaggerie se filiale abilitata
025000080201     c                   eval      *in10 = (§ogpkm ='S')
025001160503      * visualizzo turno
025002160503     c                   eval      *in11 = (§ogturno ='S')
025100061227
025200061227     c                   endsr
025300061227
025400061227      *------------------------------------------------------------------------*
025500061227      * CONTROLLO LA DATA DISTINTA
025600061227      *------------------------------------------------------------------------*
025700061227     c     sr_contrddc   begsr
025800061227
025900160503     c                   setoff                                       4142
026000061227
026100061227     c                   if        v1cddc = *zeros
026200061227     c                   seton                                        2841
026300061227     c                   eval      v1cmsg = msg(02)
026400061227     c                   leavesr
026500061227     c                   endif
026600061227
026700061227     c                   clear                   wdata
026800061227     c                   clear                   wlbdat
026900061227     c                   z-add     v1cddc        g02dat
027000061227     c                   call      'XSRDA8'
027100061227     c                   parm                    wlbdat
027200061227     c                   if        g02err = '1'
027300061227     c                   eval      *in41 = *ON
027400061227     c                   eval      *in28 = *ON
027500061227     c                   eval      v1cmsg = msg(02)
027600061227     c                   leavesr
027700061227     c                   endif
027800071204
027900071204     c                   z-add     g02dat        v1cddc
028000071204     c                   z-add     g02inv        wdata
028100071204
028200071204      * se utente NO EDP la data non può essere inferiore alla data del giorno
028300071204     c                   if        not *in04 and wdata < data_oggi
028400071205     c                   eval      *in41 = *on
028500071205     c                   eval      *in28 = *on
028600071204     c                   eval      v1cmsg = msg(05)
028700071204     c                   leavesr
028800071204     c                   endif
028900061227
029000061227     c                   endsr
029100071204
029200071204      *------------------------------------------------------------------------*
029300071204      * CONTROLLO NUMERO DISTINTA
029400071204      *------------------------------------------------------------------------*
029500071204     c     sr_contrndc   begsr
029600071204
029700160504     c                   setoff                                       424347
029800071204
029900120521     c     kfidst08      setll     fidst08l                               46
030000120521     c                   if        not *in46
030100120521     c                   eval      *in28 = *on
030200120521     c                   eval      *in42 = *on
030300120521     c                   eval      v1cmsg = msg(06)
030400120521     c                   leavesr
030500120522     c                   end
030600071204
030700071204      * numero distinta dal
030800071204     c                   if        v1ndcda <> *blanks
030900071204
031000071204     c                   if        v1ndcda = *zeros
031100071204     c                   clear                   v1ndcda
031200071204     c                   endif
031300071204      * ricerca
031400071205     c                   if        %scan('?':v1ndcda) > *zeros
031500071204     c                   clear                   fidg31ds
031600071204     c                   eval      fdgfgs = v1cfgs
031601160504     c                   eval      fdgTDIS = v1TDIS
031602160504     c                   eval      fdgTUR1 = v1TUR1
031603160504     c                   eval      fdgTUR2 = v1TUR2
031700071204     c                   eval      fdggda = wdata
031800071204     c                   eval      fdggal = wdata
031900071204     c                   eval      fdgric = 'S'
032000071204     c                   eval      fdgtfv = '2'
032100071204     c                   eval      fdgtpv = 'P'
032200071204     c                   eval      fdgopz = '1'
032300071204     c                   eval      fdgnf1 = wndcdal
032400080715      * se richiesto solo ritiri
032500080715     c                   if        v1ctipo = 'R'
032600080715     c                   eval      fdgtpop = 'R'
032700080715      * se no passo consegne
032800080715     c                   else
032900080715     c                   eval      fdgtpop = 'C'
033000080715     c                   endif
033100071206     c                   clear                   $fv6
033200071206      * impostato a tutti 99999
033300071206     c                   do        600           xx
033400071206     c                   z-add     999999        $fv6(xx)
033500071206     c                   enddo
033600071204     c                   eval      kpjbu = fidg31ds
033700071204     c                   call      'FIDG31R'
033800071204     c                   parm                    KPJBA
033900071206     c                   parm                    $fv6
034000071204     c                   eval      fidg31ds = kpjbu
034100071204     c                   if        fdgnfv = *zeros
034200071204     c                   clear                   fidg31ds
034300080910     c                   clear                   v1ndcda
034400071204     c                   else
034500071204     c                   move      fdgnfv        v1ndcda
034600071204     c                   endif
034700071204     c                   endif
034800071205      * controllo validità
034900080910     c                   if        v1ndcda > *blanks and v1ndcda <> *zeros
035000071204     c                   move      v1ndcda       wndcdal
035100071204     c                   eval      dstnfv = wndcdal
035200071204     c     kfidst        chain     fidst09l
035300071204     c                   select
035400071204     c                   when      not %found(fidst09l) or dstatb <> *blanks
035500071204     c                   eval      *in28 = *on
035600071204     c                   eval      *in42 = *on
035700071204     c                   eval      v1cmsg = msg(08)
035800071204     c                   leavesr
035900071204     c                   when      dstfcf = 'S'
036000071204     c                   eval      *in28 = *on
036100071204     c                   eval      *in42 = *on
036200071204     c                   eval      v1cmsg = msg(09)
036300071204     c                   leavesr
036400071204     c                   when      dstdfv <> wdata
036500071204     c                   eval      *in28 = *on
036600071204     c                   eval      *in42 = *on
036700071204     c                   eval      v1cmsg = msg(06)
036800071204     c                   leavesr
038100071204     c                   other
038101160503     c                   eval      ddstflr = dstflr
038200071204     c                   if        v1ndcal = *zeros
038300071204     c                   move      v1ndcda       v1ndcal
038400120522     c                   move      v1ndcda       savndcal
038500071204     c                   endif
038600071204     c                   endsl
038700080910     c                   endif
038800071204     c                   endif
038900071204
039000071205      * numero distinta al
039100071204     c                   if        v1ndcal <= *zeros
039200071204     c                   eval      *in28 = *on
039300071204     c                   eval      *in43 = *on
039400071204     c                   eval      v1cmsg = msg(10)
039500071204     c                   leavesr
039600071204     c                   endif
039700071204
039800071205      * range incompatibile
039900071204     c                   if        wndcdal > v1ndcal
040000071204     c                   eval      *in28 = *on
040100071204     c                   eval      *in43 = *on
040200071204     c                   eval      v1cmsg = msg(11)
040300071204     c                   leavesr
040400071204     c                   endif
040500080519
040600080715      * se richiesta una sola distinta
040700080715     c                   if        wndcdal = v1ndcal
040800080715
040900080715      * se richiesta la preparazione ORM o ENTRAMBI
041000080715      * controllo se c'è già la fase RCH o RQE
041100080519      * in questo caso non si possono preparare i dati per il PDA
041200080715     c                   if        v1ctipo = 'R' or v1ctipo = 'E'
041300080519      * fase RCH
041400080519     c                   clear                   fior90ds
041500080519     c                   eval      or90tla = 'C'
041600080519     c                   eval      or90fas = 'RCH'
041700080519     c                   eval      or90fgs = v1cfgs
041800080519     c                   eval      or90ndc = wndcdal
041900080519     c                   call      'FIOR90R'
042000080519     c                   parm                    kpjba
042100080519     c                   parm                    fior90ds
042200080519      * fase RCH presente errore
042300080519     c                   if        or90ndcok <> *blanks
042400080519     c                   eval      *in28 = *on
042500080519     c                   eval      *in43 = *on
042601160503     c                   eval      v1cmsg = msg(12)
042700080519     c                   leavesr
042800080519     c                   endif
042900080519      * se non trovo la fase 'RCH' provo con la fase 'RQE'
043000080519     c                   clear                   fior90ds
043100080519     c                   eval      or90tla = 'C'
043200080519     c                   eval      or90fas = 'RQE'
043300080519     c                   eval      or90fgs = v1cfgs
043400080519     c                   eval      or90ndc = wndcdal
043500080519     c                   call      'FIOR90R'
043600080519     c                   parm                    kpjba
043700080519     c                   parm                    fior90ds
043800080519      * fase RCH presente errore
043900080519     c                   if        or90ndcok <> *blanks
044000080519     c                   eval      *in28 = *on
044100080519     c                   eval      *in43 = *on
044200080519     c                   eval      v1cmsg = msg(13)
044300080519     c                   leavesr
044400080519     c                   endif
044500080519     c                   endif
044600080715      * se richiesta la preparazione CONSEGNE o ENTRAMBI
044700080715      * verifico esistenza fase PDC
044800080715     c                   if        v1ctipo = 'C' or v1ctipo = 'E'
044900080715     c                   clear                   fior90ds
045000080715     c                   eval      or90tla = 'C'
045100080715     c                   eval      or90fas = 'PDC'
045200080715     c                   eval      or90fgs = v1cfgs
045300080715     c                   eval      or90ndc = wndcdal
045400080715     c                   call      'FIOR90R'
045500080715     c                   parm                    kpjba
045600080715     c                   parm                    fior90ds
045700080715      * fase PDC presente errore
045800080715     c                   if        or90ndcok <> *blanks
045900080715     c                   eval      *in28 = *on
046000080715     c                   eval      *in43 = *on
046100080715     c                   eval      v1cmsg = msg(04)
046200080715     c                   leavesr
046300080715     c                   endif
046301160504     c* controlla il tipo distinta
046302160504     c                   if        *in10 and
046305160504     c                             (v1tdis = 'P' and dsttpm <>' ') or
046306160504     c                             (v1tdis = 'M' and dsttpm <>'M')
046307160504     C                   SETON                                        414228
046308160504     c                   eval      v1cmsg = msg(20)
046309160504     C                   leavesr
046310160504     c                   end
046311160503     c* controlla il turno
046312160503     c                   if        *in11
046313160504     c                             and (v1tur1 = ' ' or v1tur2 = ' ')
046314160503     c                             and dsttpm= ' '
046315160503     c                   select
046316160503     c                   when      (v1tur1 = '1' or
046317160503     c                              v1tur2 = '1') and
046318160505     c                             §dstturno <> ' '
046319160503     C                   SETON                                        28
046320160503     c                   when      (v1tur1 = '2' or
046321160503     c                              v1tur2 = '2') and
046322160505     c                             §dstturno <> '2'
046323160503     C                   SETON                                        28
046324160503     c                   endsl
046325160503     c                   if        *in28
046326160504     c                   seton                                        47
046327160503     c                   eval      v1cmsg = msg(19)
046328160504     C                   leavesr
046329160503     c                   end
046330160503     c                   end
046400080715     c                   endif
046401160503     c*
046500120522     c                   IF        not *in45 and %found(fidst09l)
046600120522     c                             and v1ctipo<>'R'
046700120522     c                             and (dstpda = 'E' or dstpda = 'C')
046800120523     c                   if        dstnbla > 200
046900120522     c                   eval      *in28 = *on
047000120522     c                   eval      *in45 = *on
047100120522     c                   eval      v1cmsg = msg(18)
047200120522     c                   leavesr
047300120522     c                   end
047400120522     c                   end
047500120522     c* più distinte, controllo che non ce ne siano con + di 200 sped.
047600120522     c                   else
047700120522     c                   if        not *in45 and
047800120522     c                             v1ctipo<>'R'
047900120522     c     kfidst08      setll     fidst08l
048000120522     c                   do        *hival
048100120522     c     kfidst08      reade     fidst08l
048200120522     c                   if        %eof(fidst08l)
048300120522     c                   leave
048400120522     c                   end
048500120522     c                   if        DSTPDA = 'N' or dstpda = 'O'
048600120522     c                   ITER
048700120522     c                   end
048800120522     c                   if        DSTNFV <
048900120522     c                             wndcdal
049000120522     c                   ITER
049100120522     c                   end
049200120522     c                   IF        DSTNFV > v1ndcal
049300120522     c                   leave
049400120522     c                   end
049500120523     c                   if        dstnbla > 200
049600120522     c                   eval      *in28 = *on
049700120522     c                   eval      *in45 = *on
049800120522     c                   eval      v1cmsg = msg(18)
049900120522     c                   leavesr
050000120522     c                   endif
050100120522     c                   enddo
050202160504     c                   endif
050300080715
050400080715     c                   endif
050401160505     c* controllo turno
050402160505     c                   if        v1ctipo = 'C' or v1ctipo = 'E'
050403160505     c                   if        *in11 and v1tur1 = ' ' and
050404160505     C                             v1tur2= ' '  and
050405160505     C                             (v1tdis = 'P' or v1tdis= ' ')
050406160505     c                   eval      v1cmsg = msg(19)
050407160505     C                   SETON                                        4728
050408160505     C                   leavesr
050409160505     C                   ENDIF
050410160505     c                   endif
050411160505     C*
050500071204
050600071204     c                   endsr
050700061227
050800061227      *------------------------------------------------------------------------*
050900061227      * ROUTINE INIZIALE
051000061227      *------------------------------------------------------------------------*
051100061227     c     *inzsr        begsr
051200061227
051300061227     c     *entry        plist
051400061227     c                   parm                    kpjba
051500061227
051600061227     c     *dtaara       define    §azute        azuteds
051700061227     c     *dtaara       define    §datiute      ddatiute
051800061227     c                   in(e)     *dtaara
051900061227     c                   if        %error  or rsut = *blanks
052000061227     c                   clear                   tibs34ds
052100061227     c                   call      'TIBS34R'
052200061227     c                   parm                    tibs34ds
052300061227     c                   in        *dtaara
052400061227     c                   endif
052500130621
052600130621      * controllo abilitazione utente
052700130621     c                   clear                   wabi
052800130621     c                   clear                   dLAT
052900130621     c                   eval      dUTE01 = UTEfaf
053000130621
053100130621      * Verifica errori e autorità profilo
053200130621     c                   SELECT
053300130621      * se ho errori nei dati utente esco dal pgm
053400130621     c                   WHEN      DUTerr = 'E'
053500130621     c                   eval      wErrGrave = *on
053600130621     c                   leavesr
053700130621      * carica abilitazioni del profilo
053800130621     c                   WHEN      §UTEorm <> *blanks
053900130621     c                   eval      wabi = §UTEorm
054000130621     c                   WHEN      UTEaut <> *blanks
054100130621     c                   eval      wabi = UTEaut
054200130621      * se non c'è l'abilitazione
054300130621      * --> se 1° livello, abilitazioni al terminal
054400130621      *     se 2° livello, abilitazioni al punto operativo
054500130621      *     se sede no abilitazioni
054600130621     c                   OTHER
054700130621     c                   IF        DUTlpo = '1'
054800130621     c                   eval      wabi   = 'TP'
054900130621     c                   ENDIF
055000130621     c                   IF        DUTlpo = '2'
055100130621     c                   eval      wabi   = 'PO'
055200130621     c                   ENDIF
055300130621     c                   ENDSL
055400130621
055500130621      * controllo se ok l'abilitazione dell'utente
055600130621     c                   clear                   TIBS02DS
055700130621     c                   eval      T02mod = 'C'
055800130621     c                   eval      T02sif = knsif
055900130621     c                   eval      T02cod = 'LAT'
056000130621     c                   eval      T02ke1 = wabi
056100130621     c                   Call      'TIBS02R'
056200130621     c                   Parm                    KPJBA
056300130621     c                   Parm                    TIBS02DS
056400130621     c                   IF        T02err <> *blanks or §LATabi = 'S'
056500130621     c                   eval      wErrGrave = *on
056600130621     c                   leavesr
056700130621     c                   ENDIF
056800130621     c                   eval      dLAT = T02uni
056900130621
057000130621      * Reperimento delle filiali gestibili dall'utente
057100130621     c                   clear                   TRUL31DS
057200130621     c                   eval      I31abi = wabi
057300130621     c                   eval      I31cdi = DUTdis
057400130621     c                   eval      I31car = DUTare
057500130621     c                   eval      I31cpo = DUTpou
057600130621     c                   call      'TRUL31R'
057700130621     c                   parm                    KPJBA
057800130621     c                   parm                    TRUL31DS
057900130621     c                   IF        O31pog > *zeros
058000130621     c                   movea     O31pog        SKpog
058100130621     c                   ELSE
058200130621     c                   eval      wErrGrave = *on
058300130621     c                   leavesr
058400130621     c                   ENDIF
058500061227
058600070118      * controllo se utente EDP
058700070118     c                   eval      *in04 = (%subst(knmus:1:3) = 'EDP')
058800061227      * imposto il p.o. in gestione
058900061227     c                   eval      v1cfgs = dutpou
059000061227      * decodifico p.o. gestione
059100061227     c                   eval      v1dfgs = dutdpo
059301160503     c                   eval      v1tur1 = '1'
059302160503     c                   eval      v1tur2 = '2'
059400061227
059500061227      * imposto la data del giorno
059600061227     c     *iso          move      *date         dataiso
059700061227     c                   move      dataiso       dataeur
059800061227     c                   move      dataeur       v1cddc
059900061227     c                   move      dataiso       wdata
060000071204     c                   eval      data_oggi = wdata
061100061227
061200061227      * controllo se PDA attivo sul p.o. in gestione
061300061227     c                   clear                   og148
061400080201     c                   clear                   og150
061500061227     c     v1cfgs        chain     azorg01L
061600061227     c                   if        %found(azorg01l) and orgfva = *blanks
061700061227     c                   eval      og148 = orgde8
061800080201     c                   eval      og150 = orgdf0
061900061227     c                   endif
062000110201     c                   if        §ogpdaorm = *blanks and
062100110201     c                             §ogpdacon = *blanks
062200061227     c                   seton                                        2840
062300061227     c                   eval      v1cmsg = msg(03)
062400061227     c                   endif
062500080201      * picking messaggerie
062600080201     c                   eval      *in10 = (§ogpkm ='S')
062601160503      * turno
062602160503     c                   eval      *in11 = (§ogturno ='S')
062700071204
062800071204      * klist
062900071204     c     kfidst        klist
063000071204     c                   kfld                    v1cfgs
063100071204     c                   kfld                    kdstnpg
063200071204     c                   kfld                    dstnfv
063300071204
063400071204     c     kfidst08      klist
063500071204     c                   kfld                    v1cfgs
063600071204     c                   kfld                    kdstnpg
063700071204     c                   kfld                    wdata
063800061227
063900061227     c                   endsr
064000061227
064100061227** MSG  Lungh. 78                                                            *
064200070419Filiale gestione errata                                                       01
064300061227Data errata                                                                   02
064400070419PDA non attivo per la filiale gestione                                        03
064500070118L'estrazione dei dati è già stata elaborata                                   04
064600071204Data antecedente alla data odierna                                            05
064700071204Non trovate distinte entro i limiti indicati                                  06
064800071204Numero distinta errato: contiene caratteri NON numerici                       07
064900071204Numero distinta inesistente o annullato                                       08
065000071204Distinta già chiusa                                                           09
065100071204Numero distinta obbligatorio                                                  10
065200071204Limiti incompatibili: numero distinta al minore del numero distinta dal       11
065300080519Per la distinta selezionata è già stata eseguita la chiusura da PDA           12
065400080519Per la distinta selezionata è già stata eseguita la quadratura distinta       13
065500080715Distinta non valida per elaborazione dati PDA                                 14
065600080715Tipo preparazione dati obbligatorio                                           15
065700110201Filiale non attiva alla procedura PDA                                         16
065800110201Filiale attiva SOLO alla procedura PDA                                        17
065900120522Ci sono distinte con più di 200 Spediz.!! Il PDA potrebbe dare errore         18
066000160504Distinta/e non congruente/i con il turno                                      19
066100160504Distinta non congruente con il tipo distinta                                  20
