000100071214     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000200070209
000300070209      *------------------------------------------------------------------------*
000400070209      *
000500070209      *                 STAMPA PROSPETTO ORM               ?
000600070209      *
000700070209      *------------------------------------------------------------------------*
000800070209     fazorg01l  if   e           k disk
000900071214     ffidst09l  if   e           k disk
001000071214     ffidst08l  if   e           k disk    rename(fidst000:fidst08)
001100070209     ffior09d   cf   e             workstn
001200070209
001300070209      *------------------------------------------------------------------------*
001400070209      *  RIEPILOGO INDICATORI
001500070209      *------------------------------------------------------------------------*
001600070209      * 04 - utente EDP
001700070209      * 05 - sproteggo p.o. in gestione
001800070209      * 06 - non abilito f18
001900080402      * 10 - filiale abilitata al picking messaggerie
002000080402      * 11 - filiale con distinte mattino e pomeriggio
002100070209      * 28 - errore generico
002200070209      * 30 - comodo
002300070209      * 40 - errore p.o. in gestione
002400071214      * 41 - errore distinta da
002500071214      * 42 - errore distinta al
002600071214      * 43 - errore data distinta
002700080402      * 47 - errore tipo uscita
002800070209      *------------------------------------------------------------------------*
002900070209
003000070209      *   V A R I A B I L I
003100071214     d dataiso         s               d   datfmt(*iso)
003200070223     d dataeur         s               d   datfmt(*eur)
003300071214     d data_oggi       s              8  0
003400071214     d kdstnpg         s                   like(dstnpg) inz(4)
003500071214     d xx              s              3  0
003600071214     d wdata           s              8  0
003700071214     d wndcdal         s                   like(dstnfv)
003800071214     d $fv6            s              6p 0 dim(600)
003900130619
004000130619     d wabi            s                   like(UTEaut)
004100130619     d wEsci           s               n   inz(*off)
004200130619     d wErrGrave       s               n   inz(*off)
004300130619     d w003a           s              3
004400070209
004500070209      *   S C H I E R E
004600130619     d****** l1              s              3  0 dim(30)
004700130619     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
004800071214     d msg             s             78    dim(15) ctdata perrcd(1)
004900070209
005000070209      *   D S   I N T E R N E / E S T E R N E
005100071214     d wlbdat          ds                  inz
005200071214     d  g02dat                 1      8  0
005300071214     d  g02inv                 9     16  0
005400071214     d  g02err                17     17
005500071214     d  g02tgi                18     22  0
005600071214
005700070209     d azuteds       e ds                  extname(azute00f)
005800070209     d ddatiute      e ds
005900080430     d fior47ds      e ds
006000070209     d fior24ds      e ds
006100080402     d fior90ds      e ds
006200070209     d kpjba         e ds
006300070223     d og148         e ds
006400080402     d og147         e ds
006500080402     d og150         e ds
006600080402     d ddstflr       e ds
006700070209     d tibs34ds      e ds
006800130619     d******* trul06ds      e ds                  extname(trul06ds)
006900130619     d*******  lin                    1     90  0 dim(30)
007000130619
007100130619     d dLAT          e ds
007200130619     d dUTE01        e ds
007300130619
007400130619     d TIBS02DS      e ds
007500130619     d TRUL31DS      e ds
007600070209
007700070209     d                sds
007800070209     d  vtcpgm                 1     10
007900071214
008000071214      *   C O S T A N T I
008100071214     d digitn          c                   const('1234567890')
008200070209
008300070209      *------------------------------------------------------------------------*
008400070209
008500070209      * gestione videata
008600070209     c                   do        *hival
008700130619
008800130619      /free
008900130619       //?Se utente non autorizzato alla funzione esco
009000130619         IF  wErrGrave;
009100130619           *in28 = *on;
009200130619           wEsci = *on;
009300130619           V1Cmsg = 'Utente non autorizzato alla funzione';
009400130619         ENDIF;
009500130619      /end-free
009600130619
009700070209     c                   exfmt     or09d01
009800070209     c                   setoff                                       284041
009900080402     c                   setoff                                       424347
010000130619
010100130619      /free
010200130619         IF  wEsci;
010300130619           leave;
010400130619         ENDIF;
010500130619      /end-free
010600070209
010700070209      * f3=fine
010800070209     c   kc              leave
010900070209
011000070209      * f18=cambio p.p. in gestione
011100070209     c   ks              eval      *in05 = *on
011200070209     c   ks              iter
011300070209
011400070209      * controllo p.o. in gestione
011500070209     c                   exsr      sr_contrfgs
011600080402      *
011700080402     c                   if        *in05 and not *in28
011800080402     c                   eval      *in05 = *off
011900080402     c                   iter
012000080402     c                   endif
012100080402      *
012200070209     c   28              iter
012300070209
012400071214     c                   exsr      sr_contrddc
012500071214     c   28              iter
012600080402
012700080402     c                   exsr      sr_contrtdc
012800080402     c   28              iter
012900071214
013000070209     c                   exsr      sr_contrndc
013100070209     c   28              iter
013200071214
013300070209      * f6=conferma
013400070209     c                   if        *inkf
013500070209     c                   clear                   fior24ds
013600070209     c                   eval      i24fgs = v1cfgs
013700071214     c                   movel     wdata         i24data
013800071214     c                   move      wndcdal       i24ndcda
013900071214     c                   move      v1ndcal       i24ndcal
014000080402     c                   move      v1cfpp        i24fpp
014100080402     c                   move      v1ctpm        i24tpm
014200070209     c                   eval      kpjbu = fior24ds
014300080509     c                   eval      kcoaz = 'OR24'
014400080509     c                   call      'BCH10'
014500080509     c****               call      'FIOR24R'
014600070209     c                   parm                    kpjba
014700070209     c                   leave
014800070209     c                   endif
014900070209
015000070209     c                   enddo
015100070209
015200070209     c                   eval      *inlr = *on
015300070209
015400070209      *------------------------------------------------------------------------*
015500070209      * CONTROLLO IL P.O. IN IN GESTIONE
015600070209      *------------------------------------------------------------------------*
015700070209     c     sr_contrfgs   begsr
015800070209
015900070209     c                   setoff                                       40
016000070209     c                   clear                   v1dfgs
016100070223     c                   clear                   og148
016200080402     c                   clear                   og147
016300080402     c                   clear                   og150
016400070209
016500130619     c**** v1cfgs        lookup    l1                                     30
016600130619     c                   move      V1Cfgs        w003a
016700130619     c     w003a         lookup    SKpog                                  30
016800070209     c                   if        not *in30
016900070209     c                   seton                                        2840
017000070209     c                   eval      v1cmsg = msg(01)
017100070209     c                   leavesr
017200070209     c                   endif
017300070209
017400070209      * decodifica p.o. in gestione
017500070209     c     v1cfgs        chain     azorg01L
017600070209     c                   if        %found(azorg01l) and orgfva = *blanks
017700070209     c                   eval      v1dfgs = orgdes
017800070223     c                   eval      og148 = orgde8
017900080402     c                   eval      og147 = orgde7
018000080402     c                   eval      og150 = orgdf0
018100070209     c                   endif
018200070223      * PDA attivo
018300080213     c                   if        §ogpdaorm = *blanks
018400070223     c                   eval      *in28 = *on
018500070223     c                   eval      *in40 = *on
018600070223     c                   eval      v1cmsg = msg(04)
018700070223     c                   leavesr
018800070223     c                   endif
018900080402      * verifico se P.O. abilitato al picking messaggerie
019000080402     c                   eval      *in10 = (§ogpkm ='S')
019100080402
019200080402      * verifico se P.O. abilitato alla richiesta del tipo uscita distinta
019300080402     c                   eval      *in11 = (§ogfrtuo ='S')
019400080402
019500070209
019600070209     c                   endsr
019700071214
019800071214      *------------------------------------------------------------------------*
019900071214      * CONTROLLO LA DATA DISTINTA
020000071214      *------------------------------------------------------------------------*
020100071214     c     sr_contrddc   begsr
020200071214
020300071214     c                   setoff                                       43
020400071214
020500071214     c                   if        v1cddc = *zeros
020600071214     c                   seton                                        2843
020700071214     c                   eval      v1cmsg = msg(10)
020800071214     c                   leavesr
020900071214     c                   endif
021000071214
021100071214     c                   clear                   wdata
021200071214     c                   clear                   wlbdat
021300071214     c                   z-add     v1cddc        g02dat
021400071214     c                   call      'XSRDA8'
021500071214     c                   parm                    wlbdat
021600071214     c                   if        g02err = '1'
021700071214     c                   eval      *in43 = *ON
021800071214     c                   eval      *in28 = *ON
021900071214     c                   eval      v1cmsg = msg(10)
022000071214     c                   leavesr
022100071214     c                   endif
022200071214
022300071214     c                   z-add     g02dat        v1cddc
022400071214     c                   z-add     g02inv        wdata
022500071214
022600071214
022700071214     c                   endsr
022800070209
022900080402      *------------------------------------------------------------------------*
023000080402      * CONTROLLO TIPOLOGIA DISTINTE
023100080402      *------------------------------------------------------------------------*
023200080402     c     sr_contrtdc   begsr
023300080402
023400080402     c                   setoff                                       47
023500080402      * se abilitato in organigramma matt/pom. obbligo l'inserimento
023600080402      * del valore
023700080402     c                   if        §OGfrtuo = 'S' and  v1cfpp = ' '
023800080402     c                   seton                                        4728
023900080402     c                   eval      v1cmsg = msg(13)
024000080402     c                   end
024100080402
024200080402     c                   endsr
024300080402
024400070209      *------------------------------------------------------------------------*
024500070209      * CONTROLLO LA DISTINTA
024600070209      *------------------------------------------------------------------------*
024700070209     c     sr_contrndc   begsr
024800070209
024900071214     c                   setoff                                       4142
025000071214
025100071214     c     kfidst08      chain     fidst08l
025200071214     c                   if        not %found(fidst08l)
025300071214     c                   eval      *in28 = *on
025400071214     c                   eval      *in41 = *on
025500071214     c                   eval      v1cmsg = msg(12)
025600071214     c                   leavesr
025700071214     c                   endif
025800071214
025900071214      * numero distinta dal
026000071214     c                   if        v1ndcda <> *blanks
026100071214
026200071214     c                   if        v1ndcda = *zeros
026300071214     c                   clear                   v1ndcda
026400071214     c                   endif
026500071214      * ricerca
026600071214     c                   if        %scan('?':v1ndcda) > *zeros
026700080430     c                   clear                   fior47ds
026800080430     c                   eval      or47fgsi = v1cfgs
026900080430     c                   eval      or47dfvi = wdata
027000080430     c                   eval      or47fppi = v1cfpp
027100080430     c                   eval      or47tpmi = v1ctpm
027200080430     c                   eval      kpjbu = fior47ds
027300080430     c                   call      'FIOR47R'
027400071214     c                   parm                    KPJBA
027500080430     c                   eval      fior47ds = kpjbu
027600080430     c                   if        or47nfvo= *zeros
027700080430     c                   clear                   fior47ds
027800071214     c                   else
027900080430     c                   move      or47nfvo      v1ndcda
028000071214     c                   endif
028100071214     c                   endif
028200071214      * controllo validità
028300071214     c                   if        %check(digitn:v1ndcda) = *zeros
028400071214     c                   move      v1ndcda       wndcdal
028500071214     c                   eval      dstnfv = wndcdal
028600071214     c     kfidst        chain     fidst09l
028700071214     c                   endif
028800071214     c                   select
028900071214     c                   when      %check(digitn:v1ndcda) > *zeros
029000071214     c                   eval      *in28 = *on
029100071214     c                   eval      *in41 = *on
029200071214     c                   eval      v1cmsg = msg(05)
029300071214     c                   leavesr
029400071214     c                   when      not %found(fidst09l) or dstatb <> *blanks
029500071214     c                   eval      *in28 = *on
029600071214     c                   eval      *in41 = *on
029700071214     c                   eval      v1cmsg = msg(06)
029800071214     c                   leavesr
029900071214     c                   when      dstdfv <> wdata
030000071214     c                   eval      *in28 = *on
030100071214     c                   eval      *in42 = *on
030200071214     c                   eval      v1cmsg = msg(12)
030300071214     c                   leavesr
030400071214     c                   other
030500071214     c                   if        v1ndcal = *zeros
030600071214     c                   move      v1ndcda       v1ndcal
030700071214     c                   endif
030800071214     c                   endsl
030900071214     c                   endif
031000071214
031100071214      * numero distinta al
031200071214     c                   if        v1ndcal <= *zeros
031300071214     c                   eval      *in28 = *on
031400071214     c                   eval      *in42 = *on
031500071214     c                   eval      v1cmsg = msg(08)
031600071214     c                   leavesr
031700071214     c                   endif
031800071214
031900071214      * range incompatibile
032000071214     c                   if        wndcdal > v1ndcal
032100071214     c                   eval      *in28 = *on
032200071214     c                   eval      *in42 = *on
032300071214     c                   eval      v1cmsg = msg(09)
032400071214     c                   leavesr
032500071214     c                   endif
032600080402
032700080402      * se selezionata una sola distinta verifico se compatibile con le scelte del video
032800080402     c                   if        wndcdal = v1ndcal
032900080402      * tipo uscita
033000080402     c                   If        v1cfpp <> ' ' and dstfpp <> v1cfpp
033100080402     c                   eval      *in28 = *on
033200080402     c                   eval      *in42 = *on
033300080402     c                   eval      v1cmsg = msg(07)
033400080402     c                   leavesr
033500080402     c                   endif
033600080402      * parcel /messaggerie
033700080402     c                   if        (v1ctpm = 'P' and dsttpm <> ' ') or
033800080402     c                             (v1ctpm = 'M' and dsttpm <> 'M')
033900080402     c                   eval      *in28 = *on
034000080402     c                   eval      *in42 = *on
034100080402     c                   eval      v1cmsg = msg(07)
034200080402     c                   leavesr
034300080402     c                   endif
034400080402
034500080402     c                   eval      ddstflr = dstflr
034600080404      * verifico se distinta con dati su pda
034700080404     c                   If        (dstpda <> 'O' and dstpda <> 'E')
034800080402     c                   eval      *in28 = *on
034900080402     c                   eval      *in42 = *on
035000080402     c                   eval      v1cmsg = msg(11)
035100080402     c                   leavesr
035200080402     c                   endif
035300080402
035400080402      * verifico se quadrata controllando fasi RCH nel file fidsf
035500080402
035600080402     c                   clear                   fior90ds
035700080402     c                   eval      or90tla = 'C'
035800080402     c                   eval      or90fas = 'RCH'
035900080402     c                   eval      or90fgs = dstfgs
036000080402     c                   eval      or90ndc = wndcdal
036100080402     c                   call      'FIOR90R'
036200080402     c                   parm                    kpjba
036300080402     c                   parm                    fior90ds
036400080402
036500080402     c                   if        or90ndcok <> '1' and or90err <> ' '
036600080402     c                   eval      *in28 = *on
036700080402     c                   eval      *in42 = *on
036800080402     c                   eval      v1cmsg = msg(14)
036900080402     c                   leavesr
037000080402     c                   endif
037100080402
037200080402     c                   endif
037300080402
037400070209     c                   endsr
037500070209
037600070209      *------------------------------------------------------------------------*
037700070209      * ROUTINE INIZIALE
037800070209      *------------------------------------------------------------------------*
037900070209     c     *inzsr        begsr
038000070209
038100070209     c     *entry        plist
038200070209     c                   parm                    kpjba
038300070209
038400070209     c     *dtaara       define    §azute        azuteds
038500070209     c     *dtaara       define    §datiute      ddatiute
038600070209     c                   in(e)     *dtaara
038700070209     c                   if        %error or rsut = *blanks
038800070209     c                   clear                   tibs34ds
038900070209     c                   call      'TIBS34R'
039000070209     c                   parm                    tibs34ds
039100070209     c                   in        *dtaara
039200070209     c                   endif
039300130619
039400130619      * controllo abilitazione utente
039500130619     c                   clear                   wabi
039600130619     c                   clear                   dLAT
039700130619     c                   eval      dUTE01 = UTEfaf
039800130619
039900130619      * Verifica errori e autorità profilo
040000130619     c                   SELECT
040100130619      * se ho errori nei dati utente esco dal pgm
040200130619     c                   WHEN      DUTerr = 'E'
040300130619     c                   eval      wErrGrave = *on
040400130619     c                   leavesr
040500130619      * carica abilitazioni del profilo
040600130619     c                   WHEN      §UTEorm <> *blanks
040700130619     c                   eval      wabi = §UTEorm
040800130619     c                   WHEN      UTEaut <> *blanks
040900130619     c                   eval      wabi = UTEaut
041000130619      * se non c'è l'abilitazione
041100130619      * --> se 1° livello, abilitazioni al terminal
041200130619      *     se 2° livello, abilitazioni al punto operativo
041300130619      *     se sede no abilitazioni
041400130619     c                   OTHER
041500130619     c                   IF        DUTlpo = '1'
041600130619     c                   eval      wabi   = 'TP'
041700130619     c                   ENDIF
041800130619     c                   IF        DUTlpo = '2'
041900130619     c                   eval      wabi   = 'PO'
042000130619     c                   ENDIF
042100130619     c                   ENDSL
042200130619
042300130619      * controllo se ok l'abilitazione dell'utente
042400130619     c                   clear                   TIBS02DS
042500130619     c                   eval      T02mod = 'C'
042600130619     c                   eval      T02sif = knsif
042700130619     c                   eval      T02cod = 'LAT'
042800130619     c                   eval      T02ke1 = wabi
042900130619     c                   Call      'TIBS02R'
043000130619     c                   Parm                    KPJBA
043100130619     c                   Parm                    TIBS02DS
043200130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
043300130619     c                   eval      wErrGrave = *on
043400130619     c                   leavesr
043500130619     c                   ENDIF
043600130619     c                   eval      dLAT = T02uni
043700130619
043800130619      * Reperimento delle filiali gestibili dall'utente
043900130619     c                   clear                   TRUL31DS
044000130619     c                   eval      I31abi = wabi
044100130619     c                   eval      I31cdi = DUTdis
044200130619     c                   eval      I31car = DUTare
044300130619     c                   eval      I31cpo = DUTpou
044400130619     c                   call      'TRUL31R'
044500130619     c                   parm                    KPJBA
044600130619     c                   parm                    TRUL31DS
044700130619     c                   IF        O31pog > *zeros
044800130619     c                   movea     O31pog        SKpog
044900130619     c                   ELSE
045000130619     c                   eval      wErrGrave = *on
045100130619     c                   leavesr
045200130619     c                   ENDIF
045300071214
045400071214      * controllo se utente EDP
045500071214     c                   eval      *in04 = (%subst(knmus:1:3) = 'EDP')
045600070209      * imposto il p.o. in gestione
045700070209     c                   eval      v1cfgs = dutpou
045800070209      * decodifico p.o. gestione
045900070209     c                   eval      v1dfgs = dutdpo
046000070209      * abilito f18
046100130619     c***********        eval      *in06 = (dutlpo = '2' or dutlpo = *blanks)
046200070209
046300070209      * carico schiera p.o. gestiti
046400130619     c*****              clear                   trul06ds
046500130619     c*****              eval      d06cod = '£1'
046600130619     c*****              movel     simfel        d06key
046700130619     c*****              eval      kpjbu = trul06ds
046800130619     c*****              call      'TRUL06R'
046900130619     c*****              parm                    kpjba
047000130619     c*****              eval      trul06ds = kpjbu
047100130619     c*****              movel     lin           l1
047200070223
047300070223      * controllo se PDA attivo sul p.o. in gestione
047400070223     c                   clear                   og148
047500100505     c                   clear                   og147
047600100505     c                   clear                   og150
047700070223     c     v1cfgs        chain     azorg01L
047800070223     c                   if        %found(azorg01l) and orgfva = *blanks
047900070223     c                   eval      og148 = orgde8
048000100505     c                   eval      og147 = orgde7
048100100505     c                   eval      og150 = orgdf0
048200070223     c                   endif
048300080213     c                   if        §ogpdaorm = *blanks
048400070223     c                   eval      *in28 = *on
048500070223     c                   eval      *in40 = *on
048600070223     c                   eval      v1cmsg = msg(04)
048700070223     c                   endif
048800100505      * verifico se P.O. abilitato al picking messaggerie
048900100505     c                   eval      *in10 = (§ogpkm ='S')
049000100505
049100100505      * verifico se P.O. abilitato alla richiesta del tipo uscita distinta
049200100505     c                   eval      *in11 = (§ogfrtuo ='S')
049300070223
049400071214      * imposto la data del giorno
049500071214     c     *iso          move      *date         dataiso
049600071214     c                   move      dataiso       dataeur
049700071214     c                   move      dataeur       v1cddc
049800071214     c                   move      dataiso       wdata
049900071214     c                   eval      data_oggi = wdata
050000070223
050100071214      * klist
050200071214     c     kfidst        klist
050300071214     c                   kfld                    v1cfgs
050400071214     c                   kfld                    kdstnpg
050500071214     c                   kfld                    dstnfv
050600071214
050700071214     c     kfidst08      klist
050800071214     c                   kfld                    v1cfgs
050900071214     c                   kfld                    kdstnpg
051000071214     c                   kfld                    wdata
051100070209
051200070209     c                   endsr
051300070209
051400070209** MSG  Lungh. 78                                                            *
051500070418Filiale in gestione errata                                                    01
051600070209Immettere la distinta                                                         02
051700070209Distinta errata                                                               03
051800070418PDA non attivo per la filiale in gestione                                     04
051900071214Numero distinta errato: contiene caratteri NON numerici                       05
052000071214Numero distinta inesistente o annullato                                       06
052100080402Distinta non compatibile con tipo prestazione selezionato                     07
052200071214Numero distinta obbligatorio                                                  08
052300071214Limiti incompatibili: numero distinta al minore del numero distinta dal       09
052400071214Data errata                                                                   10
052500080402Distinta errata Distinta non per PDA                                          11
052600071214Non trovate distinte entro i limiti indicati                                  12
052700080402Obbligatorio scegliere il tipo uscita                                         13
052800080402Distinta errata Distinta non quadrata                                         14
