000100071214     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000200070209
000300070209      *------------------------------------------------------------------------*
000400070209      *
000500070209      *                 STAMPA PROSPETTO ORM               ?
000600070209      *
000700070209      *------------------------------------------------------------------------*
000800070209     fazorg01l  if   e           k disk
000900140806     ffirpt01l  if   e           k disk
001000140806     ffirpt02l  if   e           k disk    rename(firpt000:firpt02)
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
003400071214     d xx              s              3  0
003500071214     d wdata           s              8  0
003600140806     d wndcdal         s                   like(rptnfv)
003700071214     d $fv6            s              6p 0 dim(600)
003800130619
003900130619     d wabi            s                   like(UTEaut)
004000130619     d wEsci           s               n   inz(*off)
004100130619     d wErrGrave       s               n   inz(*off)
004200130619     d w003a           s              3
004300070209
004400070209      *   S C H I E R E
004500130619     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
004600071214     d msg             s             78    dim(15) ctdata perrcd(1)
004700070209
004800070209      *   D S   I N T E R N E / E S T E R N E
004900071214     d wlbdat          ds                  inz
005000071214     d  g02dat                 1      8  0
005100071214     d  g02inv                 9     16  0
005200071214     d  g02err                17     17
005300071214     d  g02tgi                18     22  0
005400071214
005500070209     d azuteds       e ds                  extname(azute00f)
005600070209     d ddatiute      e ds
005700080430     d fior47ds      e ds
005800070209     d fior24ds      e ds
005900080402     d fior90ds      e ds
006000070209     d kpjba         e ds
006100070223     d og148         e ds
006200080402     d og147         e ds
006300080402     d og150         e ds
006400070209     d tibs34ds      e ds
006500130619
006600130619     d dLAT          e ds
006700130619     d dUTE01        e ds
006800130619
006900130619     d TIBS02DS      e ds
007000130619     d TRUL31DS      e ds
007100070209
007200070209     d                sds
007300070209     d  vtcpgm                 1     10
007400071214
007500071214      *   C O S T A N T I
007600071214     d digitn          c                   const('1234567890')
007700070209
007800070209      *------------------------------------------------------------------------*
007900070209
008000070209      * gestione videata
008100070209     c                   do        *hival
008200130619
008300130619      /free
008400130619       //?Se utente non autorizzato alla funzione esco
008500130619         IF  wErrGrave;
008600130619           *in28 = *on;
008700130619           wEsci = *on;
008800130619           V1Cmsg = 'Utente non autorizzato alla funzione';
008900130619         ENDIF;
009000130619      /end-free
009100130619
009200070209     c                   exfmt     or09d01
009300070209     c                   setoff                                       284041
009400080402     c                   setoff                                       424347
009500130619
009600130619      /free
009700130619         IF  wEsci;
009800130619           leave;
009900130619         ENDIF;
010000130619      /end-free
010100070209
010200070209      * f3=fine
010300070209     c   kc              leave
010400070209
010500070209      * f18=cambio p.p. in gestione
010600070209     c   ks              eval      *in05 = *on
010700070209     c   ks              iter
010800070209
010900070209      * controllo p.o. in gestione
011000070209     c                   exsr      sr_contrfgs
011100080402      *
011200080402     c                   if        *in05 and not *in28
011300080402     c                   eval      *in05 = *off
011400080402     c                   iter
011500080402     c                   endif
011600080402      *
011700070209     c   28              iter
011800070209
011900071214     c                   exsr      sr_contrddc
012000071214     c   28              iter
012100080402
012200080402     c                   exsr      sr_contrtdc
012300080402     c   28              iter
012400071214
012500070209     c                   exsr      sr_contrndc
012600070209     c   28              iter
012700071214
012800070209      * f6=conferma
012900070209     c                   if        *inkf
013000070209     c                   clear                   fior24ds
013100070209     c                   eval      i24fgs = v1cfgs
013200071214     c                   movel     wdata         i24data
013300071214     c                   move      wndcdal       i24ndcda
013400071214     c                   move      v1ndcal       i24ndcal
013500080402     c                   move      v1cfpp        i24fpp
013600080402     c                   move      v1ctpm        i24tpm
013700070209     c                   eval      kpjbu = fior24ds
013800080509     c                   eval      kcoaz = 'OR24'
013900080509     c                   call      'BCH10'
014000070209     c                   parm                    kpjba
014100070209     c                   leave
014200070209     c                   endif
014300070209
014400070209     c                   enddo
014500070209
014600070209     c                   eval      *inlr = *on
014700070209
014800070209      *------------------------------------------------------------------------*
014900070209      * CONTROLLO IL P.O. IN IN GESTIONE
015000070209      *------------------------------------------------------------------------*
015100070209     c     sr_contrfgs   begsr
015200070209
015300070209     c                   setoff                                       40
015400070209     c                   clear                   v1dfgs
015500070223     c                   clear                   og148
015600080402     c                   clear                   og147
015700080402     c                   clear                   og150
015800070209
015900130619     c                   move      V1Cfgs        w003a
016000130619     c     w003a         lookup    SKpog                                  30
016100070209     c                   if        not *in30
016200070209     c                   seton                                        2840
016300070209     c                   eval      v1cmsg = msg(01)
016400070209     c                   leavesr
016500070209     c                   endif
016600070209
016700070209      * decodifica p.o. in gestione
016800070209     c     v1cfgs        chain     azorg01L
016900070209     c                   if        %found(azorg01l) and orgfva = *blanks
017000070209     c                   eval      v1dfgs = orgdes
017100070223     c                   eval      og148 = orgde8
017200080402     c                   eval      og147 = orgde7
017300080402     c                   eval      og150 = orgdf0
017400070209     c                   endif
017500070223      * PDA attivo
017600080213     c                   if        §ogpdaorm = *blanks
017700070223     c                   eval      *in28 = *on
017800070223     c                   eval      *in40 = *on
017900070223     c                   eval      v1cmsg = msg(04)
018000070223     c                   leavesr
018100070223     c                   endif
018200080402      * verifico se P.O. abilitato al picking messaggerie
018300080402     c                   eval      *in10 = (§ogpkm ='S')
018400080402
018500080402      * verifico se P.O. abilitato alla richiesta del tipo uscita distinta
018600080402     c                   eval      *in11 = (§ogfrtuo ='S')
018700080402
018800070209
018900070209     c                   endsr
019000071214
019100071214      *------------------------------------------------------------------------*
019200071214      * CONTROLLO LA DATA DISTINTA
019300071214      *------------------------------------------------------------------------*
019400071214     c     sr_contrddc   begsr
019500071214
019600071214     c                   setoff                                       43
019700071214
019800071214     c                   if        v1cddc = *zeros
019900071214     c                   seton                                        2843
020000071214     c                   eval      v1cmsg = msg(10)
020100071214     c                   leavesr
020200071214     c                   endif
020300071214
020400071214     c                   clear                   wdata
020500071214     c                   clear                   wlbdat
020600071214     c                   z-add     v1cddc        g02dat
020700071214     c                   call      'XSRDA8'
020800071214     c                   parm                    wlbdat
020900071214     c                   if        g02err = '1'
021000071214     c                   eval      *in43 = *ON
021100071214     c                   eval      *in28 = *ON
021200071214     c                   eval      v1cmsg = msg(10)
021300071214     c                   leavesr
021400071214     c                   endif
021500071214
021600071214     c                   z-add     g02dat        v1cddc
021700071214     c                   z-add     g02inv        wdata
021800071214
021900071214
022000071214     c                   endsr
022100070209
022200080402      *------------------------------------------------------------------------*
022300080402      * CONTROLLO TIPOLOGIA DISTINTE
022400080402      *------------------------------------------------------------------------*
022500080402     c     sr_contrtdc   begsr
022600080402
022700080402     c                   setoff                                       47
022800080402      * se abilitato in organigramma matt/pom. obbligo l'inserimento
022900080402      * del valore
023000080402     c                   if        §OGfrtuo = 'S' and  v1cfpp = ' '
023100080402     c                   seton                                        4728
023200080402     c                   eval      v1cmsg = msg(13)
023300080402     c                   end
023400080402
023500080402     c                   endsr
023600080402
023700070209      *------------------------------------------------------------------------*
023800070209      * CONTROLLO LA DISTINTA
023900070209      *------------------------------------------------------------------------*
024000070209     c     sr_contrndc   begsr
024100070209
024200071214     c                   setoff                                       4142
024300071214
024400140806     c     kfirpt02      chain     firpt02l
024500140806     c                   if        not %found(firpt02l)
024600071214     c                   eval      *in28 = *on
024700071214     c                   eval      *in41 = *on
024800071214     c                   eval      v1cmsg = msg(12)
024900071214     c                   leavesr
025000071214     c                   endif
025100071214
025200071214      * numero distinta dal
025300071214     c                   if        v1ndcda <> *blanks
025400071214
025500071214     c                   if        v1ndcda = *zeros
025600071214     c                   clear                   v1ndcda
025700071214     c                   endif
025800071214      * ricerca
025900071214     c                   if        %scan('?':v1ndcda) > *zeros
026000080430     c                   clear                   fior47ds
026100080430     c                   eval      or47fgsi = v1cfgs
026200080430     c                   eval      or47dfvi = wdata
026300080430     c                   eval      or47fppi = v1cfpp
026400080430     c                   eval      or47tpmi = v1ctpm
026500080430     c                   eval      kpjbu = fior47ds
026600080430     c                   call      'FIOR47R'
026700071214     c                   parm                    KPJBA
026800080430     c                   eval      fior47ds = kpjbu
026900080430     c                   if        or47nfvo= *zeros
027000080430     c                   clear                   fior47ds
027100071214     c                   else
027200080430     c                   move      or47nfvo      v1ndcda
027300071214     c                   endif
027400071214     c                   endif
027500071214      * controllo validità
027600071214     c                   if        %check(digitn:v1ndcda) = *zeros
027700071214     c                   move      v1ndcda       wndcdal
027800140806     c                   eval      rptnfv = wndcdal
027900140806     c     kfirpt        chain     firpt01l
028000071214     c                   endif
028100071214     c                   select
028200071214     c                   when      %check(digitn:v1ndcda) > *zeros
028300071214     c                   eval      *in28 = *on
028400071214     c                   eval      *in41 = *on
028500071214     c                   eval      v1cmsg = msg(05)
028600071214     c                   leavesr
028700140806     c                   when      not %found(firpt01l) or rptatb <> *blanks
028800071214     c                   eval      *in28 = *on
028900071214     c                   eval      *in41 = *on
029000071214     c                   eval      v1cmsg = msg(06)
029100071214     c                   leavesr
029200140806     c                   when      rptdfv <> wdata
029300071214     c                   eval      *in28 = *on
029400071214     c                   eval      *in42 = *on
029500071214     c                   eval      v1cmsg = msg(12)
029600071214     c                   leavesr
029700071214     c                   other
029800071214     c                   if        v1ndcal = *zeros
029900071214     c                   move      v1ndcda       v1ndcal
030000071214     c                   endif
030100071214     c                   endsl
030200071214     c                   endif
030300071214
030400071214      * numero distinta al
030500071214     c                   if        v1ndcal <= *zeros
030600071214     c                   eval      *in28 = *on
030700071214     c                   eval      *in42 = *on
030800071214     c                   eval      v1cmsg = msg(08)
030900071214     c                   leavesr
031000071214     c                   endif
031100071214
031200071214      * range incompatibile
031300071214     c                   if        wndcdal > v1ndcal
031400071214     c                   eval      *in28 = *on
031500071214     c                   eval      *in42 = *on
031600071214     c                   eval      v1cmsg = msg(09)
031700071214     c                   leavesr
031800071214     c                   endif
031900080402
032000080402      * se selezionata una sola distinta verifico se compatibile con le scelte del video
032100080402     c                   if        wndcdal = v1ndcal
032200080402      * tipo uscita
032300140806     c                   If        v1cfpp <> ' ' and rptfpp <> v1cfpp
032400080402     c                   eval      *in28 = *on
032500080402     c                   eval      *in42 = *on
032600080402     c                   eval      v1cmsg = msg(07)
032700080402     c                   leavesr
032800080402     c                   endif
032900080402      * parcel /messaggerie
033000140806     c                   if        (v1ctpm = 'P' and rpttpm <> ' ') or
033100140806     c                             (v1ctpm = 'M' and rpttpm <> 'M')
033200080402     c                   eval      *in28 = *on
033300080402     c                   eval      *in42 = *on
033400080402     c                   eval      v1cmsg = msg(07)
033500080402     c                   leavesr
033600080402     c                   endif
033700080402
033800080402     c                   endif
033900080402
034000070209     c                   endsr
034100070209
034200070209      *------------------------------------------------------------------------*
034300070209      * ROUTINE INIZIALE
034400070209      *------------------------------------------------------------------------*
034500070209     c     *inzsr        begsr
034600070209
034700070209     c     *entry        plist
034800070209     c                   parm                    kpjba
034900070209
035000070209     c     *dtaara       define    §azute        azuteds
035100070209     c     *dtaara       define    §datiute      ddatiute
035200070209     c                   in(e)     *dtaara
035300070209     c                   if        %error or rsut = *blanks
035400070209     c                   clear                   tibs34ds
035500070209     c                   call      'TIBS34R'
035600070209     c                   parm                    tibs34ds
035700070209     c                   in        *dtaara
035800070209     c                   endif
035900130619
036000130619      * controllo abilitazione utente
036100130619     c                   clear                   wabi
036200130619     c                   clear                   dLAT
036300130619     c                   eval      dUTE01 = UTEfaf
036400130619
036500130619      * Verifica errori e autorità profilo
036600130619     c                   SELECT
036700130619      * se ho errori nei dati utente esco dal pgm
036800130619     c                   WHEN      DUTerr = 'E'
036900130619     c                   eval      wErrGrave = *on
037000130619     c                   leavesr
037100130619      * carica abilitazioni del profilo
037200130619     c                   WHEN      §UTEorm <> *blanks
037300130619     c                   eval      wabi = §UTEorm
037400130619     c                   WHEN      UTEaut <> *blanks
037500130619     c                   eval      wabi = UTEaut
037600130619      * se non c'è l'abilitazione
037700130619      * --> se 1° livello, abilitazioni al terminal
037800130619      *     se 2° livello, abilitazioni al punto operativo
037900130619      *     se sede no abilitazioni
038000130619     c                   OTHER
038100130619     c                   IF        DUTlpo = '1'
038200130619     c                   eval      wabi   = 'TP'
038300130619     c                   ENDIF
038400130619     c                   IF        DUTlpo = '2'
038500130619     c                   eval      wabi   = 'PO'
038600130619     c                   ENDIF
038700130619     c                   ENDSL
038800130619
038900130619      * controllo se ok l'abilitazione dell'utente
039000130619     c                   clear                   TIBS02DS
039100130619     c                   eval      T02mod = 'C'
039200130619     c                   eval      T02sif = knsif
039300130619     c                   eval      T02cod = 'LAT'
039400130619     c                   eval      T02ke1 = wabi
039500130619     c                   Call      'TIBS02R'
039600130619     c                   Parm                    KPJBA
039700130619     c                   Parm                    TIBS02DS
039800130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
039900130619     c                   eval      wErrGrave = *on
040000130619     c                   leavesr
040100130619     c                   ENDIF
040200130619     c                   eval      dLAT = T02uni
040300130619
040400130619      * Reperimento delle filiali gestibili dall'utente
040500130619     c                   clear                   TRUL31DS
040600130619     c                   eval      I31abi = wabi
040700130619     c                   eval      I31cdi = DUTdis
040800130619     c                   eval      I31car = DUTare
040900130619     c                   eval      I31cpo = DUTpou
041000130619     c                   call      'TRUL31R'
041100130619     c                   parm                    KPJBA
041200130619     c                   parm                    TRUL31DS
041300130619     c                   IF        O31pog > *zeros
041400130619     c                   movea     O31pog        SKpog
041500130619     c                   ELSE
041600130619     c                   eval      wErrGrave = *on
041700130619     c                   leavesr
041800130619     c                   ENDIF
041900071214
042000071214      * controllo se utente EDP
042100071214     c                   eval      *in04 = (%subst(knmus:1:3) = 'EDP')
042200070209      * imposto il p.o. in gestione
042300070209     c                   eval      v1cfgs = dutpou
042400070209      * decodifico p.o. gestione
042500070209     c                   eval      v1dfgs = dutdpo
042600070223
042700070223      * controllo se PDA attivo sul p.o. in gestione
042800070223     c                   clear                   og148
042900100505     c                   clear                   og147
043000100505     c                   clear                   og150
043100070223     c     v1cfgs        chain     azorg01L
043200070223     c                   if        %found(azorg01l) and orgfva = *blanks
043300070223     c                   eval      og148 = orgde8
043400100505     c                   eval      og147 = orgde7
043500100505     c                   eval      og150 = orgdf0
043600070223     c                   endif
043700080213     c                   if        §ogpdaorm = *blanks
043800070223     c                   eval      *in28 = *on
043900070223     c                   eval      *in40 = *on
044000070223     c                   eval      v1cmsg = msg(04)
044100070223     c                   endif
044200100505      * verifico se P.O. abilitato al picking messaggerie
044300100505     c                   eval      *in10 = (§ogpkm ='S')
044400100505
044500100505      * verifico se P.O. abilitato alla richiesta del tipo uscita distinta
044600100505     c                   eval      *in11 = (§ogfrtuo ='S')
044700070223
044800071214      * imposto la data del giorno
044900071214     c     *iso          move      *date         dataiso
045000071214     c                   move      dataiso       dataeur
045100071214     c                   move      dataeur       v1cddc
045200071214     c                   move      dataiso       wdata
045300071214     c                   eval      data_oggi = wdata
045400070223
045500071214      * klist
045600140806     c     kfirpt        klist
045700071214     c                   kfld                    v1cfgs
045800140806     c                   kfld                    rptnfv
045900071214
046000140806     c     kfirpt02      klist
046100071214     c                   kfld                    v1cfgs
046200071214     c                   kfld                    wdata
046300070209
046400070209     c                   endsr
046500070209
046600070209** MSG  Lungh. 78                                                            *
046700070418Filiale in gestione errata                                                    01
046800140806Libero.......                                                                 02
046900140806Libero.......                                                                 03
047000070418PDA non attivo per la filiale in gestione                                     04
047100071214Numero distinta errato: contiene caratteri NON numerici                       05
047200071214Numero distinta inesistente o annullato                                       06
047300080402Distinta non compatibile con tipo prestazione selezionato                     07
047400071214Numero distinta obbligatorio                                                  08
047500071214Limiti incompatibili: numero distinta al minore del numero distinta dal       09
047600071214Data errata                                                                   10
047700140806Libero.......                                                                 11
047800071214Non trovate distinte entro i limiti indicati                                  12
047900080402Obbligatorio scegliere il tipo uscita                                         13
048000140806Libero.......                                                                 14
