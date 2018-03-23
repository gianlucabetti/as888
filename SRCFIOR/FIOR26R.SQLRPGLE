000100070212     H DECEDIT('0,') DATEDIT(*yMd.) option(*nodebugio)
000200001113
000300010206      ****************************************************************
000400010206      *                                                              *
000500010206      *        QUADRATURA   O R M                                    *
000600010206      *                                                              *
000700010206      ****************************************************************
000800010206
000900010206      ****************************************************************
001000010206      *  RIEPILOGO INDICATORI
001100010206      ****************************************************************
001200010206
001300070125      * 01 - P.o. gestione abilitato a PDA
001400070212      * 02 - richieste tutte le distinte
001500080415      * 03 - distinta senza fase "RCH" su FIDSF
001600010206      * 05 - SPROTEGGO P.O. RITIRO
001700010206      * 06 - NON ABILITO TASTO FUNZIONALE F18
001800070926      * 07 - P.o. gestione abilitato alla procedura automazione ORM
001900010206      * 20 - GESTIONE SUBFILE
002000010206      * 21 - GESTIONE SUBFILE
002100010206      * 30 - Comodo
002200010206      * 40 - ERRORE P.o. ritiro
002300010206      * 41 - ERRORE Nr.Distinta
002400010206      * 43 - ERRORE Causale
002500010206      * 44 - ERRORE ORM allocato
002600010208      * 45 - ERRORE ORM ha già la bolla
002700010530      * 46 - ORM in fase 900
002800010529      * 47 - ERRORE scelta
002900011015      * 48 - ORM già chiuso non si può richiudere
003000070206      * 49 - Distinta passata a PDA ma ORM in fase 400
003100010206      * 90 - Riemissione videata
003200010206
003300010206      *--------------------------------------------------------------*
003400001113
003500001113     FAZORG01L  IF   E           K DISK
003600021202     FFIAPD01L  IF   E           K DISK
003700040406     fFnorf01l  if a e           k disk
003800010206     FFNORM01L  UF   E           K DISK
003900010206     FFNORM06L  IF   E           K DISK    RENAME(FNORM000:FNORM6)
004000071011     ffnorg01l  uf   e           k disk
004100060214     FFIAR404L  IF   E           K DISK
004200020702     ffnacr01l  if   e           k disk
004300041012     fFiqdt01L  if   e           k disk
004400050225     fFnarb78l  if   e           k disk
004500120613     ffidst01l  if   e           k disk
004600080415     ffiplg01l  if   e           k disk
004700140611     fFIRPT01L  if a e           k disk
004800010206     FFIOR26D   CF   E             WORKSTN SFILE(FIOr26s:nrr)
004900080526     fprtemail  o    f  132        printer  oflind(*inof)  usropn
005000001113
005100010206      *------------------------------------------------------------------------*
005200080526
005300080526      * dati per invio mail
005400080526     d lenght          s             15  5
005500080526     d o_testo         s            132    inz
005600080526     d qcmd            s            512    inz
005700080526     d $invio          s              1n   inz(*off)
005800080526     d $okmra          s              1n   inz(*off)
005900080526     d dmradan       e ds                  inz
006000080526     d trtcm1ds      e ds                  inz
006100080526      *    .§cm1mitt = Indirizzo e-mail del mittente
006200120302     d   §cm1mitt    e                     inz('ced@brt.it')
006300080526      *    ·§cm1dst  = Indirizzo e-mail del destinatario
006400120302     d   §cm1dst     e                     inz('monica.beghelli@brt.it')
006500080526      *    ·§cm1tips = Tipo lettera via e-mail:
006600080526      *     "LET" = testo allegato in corpo con logo
006700080526      *             (richiede righe libere iniziali per il logo)
006800080526      *     "COR" = testo integrato senza logo
006900080526      *             (non consente né UNDERLINE né HIGHLIGHT)
007000080526     d   §cm1tips    e                     inz('COR')
007100080526      *    ·§cm1po   = Filiale
007200080526     d   §cm1po      e                     inz('046')
007300080526      *    ·§cm1var  = Oggetto e-mail
007400080526     d   §cm1var     e                     inz('*OBJM*+
007500080526     d                                     Fase RQE non scritta')
007600080526      *    ·§cm1sts  = Stato
007700080526     d   §cm1sts     e                     inz(*off)
007800080526      *    ·§cm1idp  = Id processo
007900080526     d   §cm1idp     e                     inz('2')
008000080526
008100080526     d c_sede          c                   const('046')
008200080526      *  - comando di override al prtf
008300080526     d c_cmdovrprtf    c                   const('OVRPRTF +
008400080526     d                                           file(PRTEMAIL) +
008500080526     d                                           pagesize(66 132) +
008600080526     d                                           lpi(6) cpi(10) +
008700080526     d                                           ovrscope(*actgrpdfn) +
008800080526     d                                           ')
008900080526     d c_cmddltovr     c                   const('DLTOVR +
009000080526     d                                            file(PRTEMAIL) +
009100080526     d                                            lvl(*actgrpdfn)')
009200010206
009300120613     D kdstnpg         S                   LIKE(dstnpg)
009400120613     D kdstnfv         S                   LIKE(dstnfv)
009500120613     D kdstfgs         S                   LIKE(dstfgs)
009600010206
009700060516     d keypor          s                   like(ormpor)
009800010206     D keyndc          S                   LIKE(ORMndc)
009900010208
010000060214     D kar4trc         S                   LIKE(ar4trc)
010100060214     D kar4n14         S                   LIKE(ar4n14)
010200021202
010300021202     D kpdpdr          S                   LIKE(APDpdr)
010400021202     D kpdtip          S                   LIKE(APDtip)
010500041012
010600041012     d kqdtnfv         s                   like(qdtnfv)
010700080424     d kqdtddc         s                   like(qdtddc)
010800080424     d kqdtpdr         s                   like(qdtpdr)
010900070206     d ktiporcd        s                   like(plgtiporcd)
011000070215     d ktipappl        s                   like(plgtipappl)
011100010530
011200010206     D §kpjbu          S                   LIKE(kpjbu)
011300010206
011400020523     d sav_lpksc       s                   like(ormksc)
011500040406     d sav_OrfFar      s                   like(OrfFar)
011600040406     d sav_CmrCau      s                   like(OrfCar)
011700040909     d savndc          s                   like(OrmNdc)
011800040909     d savddc          s                   like(OrmDdc)
011900040406
012000040406     d kOrfPoe         s                   like(OrfPoe)
012100040406     d kOrfNsr         s                   like(OrfNsr)
012200040406     d kOrfNor         s                   like(OrfNor)
012300040406     d kOrfNrv         s                   like(OrfNrv)
012400040406     d kOrfDae         s                   like(OrfDae)
012500040406     d kOrfOre         s                   like(OrfOre)
012600040406     d kOrfFar         s                   like(OrfFar)
012700050225
012800050225     d karbifp         s                   Like(arbifp)
012900050225     d karbndc         s                   Like(arbndc)
013000020523
013100020523     d conta           s              2  0
013200070212     d dataiso         s               d   datfmt(*iso)
013300070212     d datascad        s              8  0
013400020523     d okbolla         s              1
013500070212     d savormndc       s                   like(ormndc)
013600070302     d wdelpda         s              1    inz(*off)
013700020529     d wnobolla        s              1    inz('0')
013800070302     d wokpda          s              1    inz(*off)
013900070302     d wokrch          s              1    inz(*off)
014000040406     d wtenta          s              3  0
014100040406     d wconta          s              3  0
014200040406     d wchiudi         s              1    inz('0')
014300090402     d wfase           s              3
014400070212     d wgiorni         s              3  0
014500080415     d wndc            s                   like(ormndc)
014600060516     d xx              s              3  0
014700070207     d yy              s              3  0
014800080723     d $fv6            s              6p 0 dim(600)
014900081125     d $noadd          s              1    inz('0')
015000100409     D $GIRO           S              1  0 INZ(0)
015100130620
015200130620     d wabi            s                   like(UTEaut)
015300130620     d wEsci           s               n   inz(*off)
015400130620     d wErrGrave       s               n   inz(*off)
015500130620     d w003a           s              3
015600020523
015700020523     D lung            S             15  5
015800020523     D comman          S            110
015900020523
016000010206     D flgkf           S              1
016100010206     D W0140           S             14  0
016200010206     D Wdtgio          S              8  0
016300010206     D dateu           S              8  0
016400070926     d data_org        s              8  0 inz
016500010206
016600010206      *   S C H I E R E
016700070212     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             MSG VIDEO
016800130620     D**** L1              S              3  0 DIM(30)                              P.O. Gestiti £1
016900001113     D L6              S              3  0 DIM(30)                              P.O. Gestiti £6
017000130620     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
017100030307     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
017200030623      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
017300030623     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
017400030623     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
017500071214     d skddc           s              8  0 dim(100)
017600070207     d skndc           s              6  0 dim(100)
017700080424     d skpdr           s              7  0 dim(100)
017800010119
017900010206      *   D S   I N T E R N E / E S T E R N E
018000010206
018100010206     D WLBDAT          DS                  INZ
018200010206     D  G02DAT                 1      8  0
018300010206     D  G02INV                 9     16  0
018400010206     D  G02ERR                17     17
018500010206     D  G02TGI                18     22  0
018600020523
018700020524     D                 DS
018800020523     d  ormcra1                1      3  0
018900020523     d  ormcra2                4      7  0
019000020523     d  ormcra3                8     10  0
019100020523     d  ds_ormcra              1     10  0
019200010206
019300020524     D                 DS
019400020523     d  ormcor1                1      3  0
019500020523     d  ormcor2                4      7  0
019600020523     d  ormcor3                8     10  0
019700020523     d  ds_ormcor              1     10  0
019800020523
019900020524     D                 DS
020000020523     d  ormcrc1                1      3  0
020100020523     d  ormcrc2                4      7  0
020200020523     d  ormcrc3                8     10  0
020300020523     d  ds_ormcrc              1     10  0
020400020523
020500020523     D                 DS
020600020523     D  ds_poe                 1      3  0
020700020523     D  ds_nsr                 4      5  0
020800020523     D  ds_nor                 6     12  0
020900020523     D  ds_nrv                13     14  0
021000020523     D  ds_keyorm              1     14  0
021100020523
021200001116     D Parm01          DS
021300001116     D  ppoe                          3  0
021400001116     D  pnor                          7  0
021500001116     D  pnsr                          2  0
021600001116     D  pnrv                          2  0
021700001116     D  psce                          1
021800001116     D  pfgs                          3  0
021900001116     D  ppor                          3  0
022000001116     D  pdtr                          8  0
022100001116     D  pmdb                         10
022200001116     D  pprb                         10
022300001116     D  pdts                          8  0
022400001116     D  prmp                          1
022500010130     D  pbrc                          1
022600010130     D  pref                          2
022700040406     D  pmio                          1
022800041014     d  pndc                          6  0
022900041014     d  pddc                          8  0
023000120613     d  pdstfgs                       3  0
023100010206
023200010206      * Scomposizione Descrizione Distinta
023300010206     D                 DS
023400010206     D  v1dndc                 1     40
023500010206     D  wdel                   1      4
023600010206     D  wggg                   5      6
023700010206     D  wbarra1                7      7
023800010206     D  wmmm                   8      9
023900010206     D  wbarra2               10     10
024000010206     D  waaa                  11     15
024100010206     D  wpdr                  16     23
024200010206     D  wdescr                24     40
024300070206
024400070206     d                 ds
024500070206     d kiddoc                  1     16
024600070206     d ormpoe                  1      3  0
024700070206     d ormnsr                  4      5  0
024800070206     d ormnor                  6     12  0
024900070206     d ormnrv                 13     14  0
025000070206     d wblk                   15     16    inz('  ')
025100001113
025200020523     D DTASV         E DS
025300020523     D DBLP          E DS
025400030623     D TRUL90DS      E DS
025500010301     D DCMR          E DS
025600081029     d a_dcmr        e ds                  extname(dcmr) prefix(a_)
025700010206     D FIOR06DS      E DS
025800080319     d fior06vds     e ds
025900010206     D TIBS02DS      E DS
026000010206     D TRUL06DS      E DS
026100080723     D  LIN                    1     90  0 DIM(30)
026200040406     d Fior05ds      e ds
026300080327     d fior90ds      e ds
026400041012      * DS  pgm fnlrf8r x gestione rientri
026500041012     D fnlrf8ds      E DS
026600060516     d fnlv55ds      e ds
026700070125     d og148         e ds
026800070926     d og147         e ds
026900070206     d fiprcresds    e ds
027000080317     d fior56ds      e ds
027100070212     d dsdp          e ds
027200070711     d dfar          e ds
027300080314     d dorg01        e ds
027400080319     d ddstflr       e ds
027500080723     d fidg31ds      e ds
027600080723     d azuteds       e ds                  extname(azute00f)
027700080723     d ddatiute      e ds
027800080723     d tibs34ds      e ds
027900081029     d dorm01        e ds
028000081125     d dorf01        e ds
028100130620
028200130620     d dLAT          e ds
028300130620     d dUTE01        e ds
028400130620     d TRUL31DS      e ds
028500010206
028600010206     D KPJBA         E DS
028700010206
028800010206     D                SDS
028900010206     D  VTCPGM                 1     10
029000140611
029100140611      //---------------------------------------------------------------
029200140611      //?Definizione key-list.
029300140611      //---------------------------------------------------------------
029400140611       // - File FIRPT01L
029500140611     d k03firpt      e ds                  extname(FIRPT01L:*key)
029600140611     d                                     prefix(k_)
029700140611     d                                     inz
029800001113
029900010206      *------------------------------------------------------------------------*
030000001113
030100010206
030200010206      * Emissione 1° Videata
030300010206
030400010206     C                   do        *hival
030500010206      * Pulizia 1° Videata
030600010206     C                   if        *in28 = *off and *in90 = *off
030700010206     C                   exsr      Sr_Pulvid1
030800010206     C                   endif
030900100409
031000100409     C                   CLEAR                   $GIRO
031100130620
031200130620      /free
031300130620       //?Se utente non autorizzato alla funzione esco
031400130620         IF  wErrGrave;
031500130620           *in28 = *on;
031600130620           wEsci = *on;
031700130620           V1Cmsg = 'Utente non autorizzato alla funzione';
031800130620         ENDIF;
031900130620      /end-free
032000010206
032100010206     C                   exfmt     fior261
032200130620      /free
032300130620         IF  wEsci;
032400130620           leave;
032500130620         ENDIF;
032600130620      /end-free
032700010206
032800010206      * F3=Fine
032900010206     C   kc              leave
033000010206
033100010206     C                   setoff                                       2890
033200010206
033300010206      * F18=Cambio P.O. in gestione
033400010206     C   ks              eval      *in05 = *on
033500010206     C   ks              iter
033600010206
033700010206      * 05 on --> controllo P.O. in gestione
033800010206     C                   if        *in05 = *on
033900010206     C                   exsr      Sr_Contrpo
034000020612     C  n28              eval      *in05 = *off
034100020612     C   28              iter
034200010206     C                   endif
034300010206
034400010206      * Controlli 1° videata
034500010206     C                   exsr      Sr_Contr01
034600070208     C   28
034700070208     Cor 90              iter
034800070212
034900070302      * se p.o. abilitato al PDA, distinta abilitata allo scarico su PDA, è stata inserita
035000070302      * una distinta, ma non c'è il rcd RCH e non è stata fatta la forzatura per quadrare
035100070302      * torno al'inizio....
035200070302     c                   if        *in01 and wokpda = *on and
035300070302     c                             v1ndc <> *blanks and v1ndc <> *zeros and
035400070302     c                             wokrch = *off and wdelpda = *off
035500070212     c                   iter
035600070212     c                   endif
035700100409
035800100409     C                   DO        *HIVAL
035900010206
036000070205      * Gestione subfile di tutte le distinte
036100070205     c                   if        v1allndc = 'SI'
036200070205     c                   exsr      sr_allndc
036300070206     c                   eval      *in28 = *off
036400070205     c                   else
036500070205      * Gestione subfile x distinta
036600010206     C                   exsr      Sr_Gessfl
036700010206     C                   setoff                                       28
036800120613     c                   move      dstdfv        skddc(1)
036900070207     c                   move      v1ndc         skndc(1)
037000080424     c                   move      dstpdr        skpdr(1)
037100070205     c                   endif
037200100409
037300100415     C                   IF        ($GIRO > 0 AND NRR = 0) OR *INKL
037400100409     C                   LEAVE
037500100409     C                   ENDIF
037600100409
037700100409     C                   ENDDO
037800041013
037900070207      * per ogni distinta quadrata
038000080404    1c                   if        flgkf = '1'
038100080404    2c                   do        100           yy
038200080404    3c                   if        skndc(yy) <> *zeros
038300041013      * Richiamo Fnlrf8r x gestione rientri
038400041013     c                   ExSr      Sr_Rientri
038500070302      * p.o. gestione abilitato allo scarico PDA e distinta abilitata allo scarico su PDA
038600080404    4c                   if        *in01 and wokpda = *on
038700080424      * scrivo la fase 'RQE' sulla distinta
038800080424     c                   exsr      sr_fase
038900080424      * se ho forzato la quadratura devo
039000080424      * annullare i dati su FIPDO della distinta forzata
039100140611      * se non è già stata scritta, scrivere la testata per il prospetto ORM
039200070212      * la forzatura viene fatta solo se richiesta una distinta quindi quando
039300070212      * arrivo a questo punto ho tutti i dati che mi servono della distinta forzata
039400080404    5c                   if        wdelpda = *on
039500080319      * annullo i dati inviati al PDA
039600080404     c                   clear                   fior56ds
039700080404     c                   eval      or56tla = 'Q'
039800080314     c                   exsr      sr_aggpdo
039900140611      /free
040000140611       //?Scrivo la testata del prospetto ORM se non è già presente
040100140611         kdstnpg = 4;
040200140611         kdstnfv = skndc(yy);
040300140611         kdstfgs = V1Cpor;
040400140611         chain (kdstnpg:kdstnfv:kdstfgs) FIDST01L;
040500140611         IF  not %found(FIDST01L);
040600140611           clear DSTfpp;
040700140611           clear DSTtpm;
040800140611         ENDIF;
040900140611         k_RPTfgs = V1Cpor;
041000140611         k_RPTnfv = skndc(yy);
041100140611         k_RPTpdr = skpdr(yy);
041200140611         chain %kds(k03firpt) FIRPT01L;
041300140611         IF  not %found(FIRPT01L);
041400140611           RPTfgs = k_RPTfgs;
041500140611           RPTnfv = k_RPTnfv;
041600140611           RPTpdr = k_RPTpdr;
041700140611           RPTdfv = skddc(yy);
041800140611           RPTfpp = DSTfpp;
041900140611           RPTtpm = DSTtpm;
042000140611           write FIRPT000;
042100140611         ENDIF;
042200140611      /end-free
042300140611
042400080404    5c                   endif
042500080327      * se filiale gestione non abilitata allo scarico PDA
042600080327      * o se distinta non abilitata allo scarico PDA
042700080415      * scrivo la fase 'RQE' sulla distinta
042800080327      * in questo modo non si potranno più assegnare ORM a distinte già quadrate anche se non
042900080327      * è previsto lo scarico a PDA
043000080404   x4c                   else
043100080519      * solo se procedura GEO ORM attiva
043200080519     c                   if        *in07
043300080415      * scrivo la fase 'RQE' sulla distinta
043400080519     c                   exsr      sr_fase
043500080404      * se la distinta è in test cancello i dati su FIPDO e FIPRO
043600080404      * imposto solo la distinta tanto gli altri dati sono già impostati da prima
043700080404     c                   eval       keyndc = skndc(yy)
043800101214     c                   eval       keypor = v1cpor
043900080404     c                   clear                   ddstflr
044000080404     c     kfidst        chain     fidst01l
044100080404    5c                   if        %found(fidst01l)
044200120613     c                   eval      ddstflr = dstflr
044300080404      * attiva su PDA
044400120613    6c                   if        (dstpda = 'O' or dstpda = 'E') and
044500080404      * in TEST
044600080404     c                             (§dsttstpda = 'O' or §dsttstpda = 'E')
044700080404     c                   clear                   fior56ds
044800080404     c                   eval      or56tla = 'D'
044900080404     c                   exsr      sr_aggpdo
045000080404    6c                   endif
045100080404    5c                   endif
045200080519     c                   endif
045300080519
045400080404    4c                   endif
045500080526
045600080526      * controllo se c'è la fase RQE
045700080526     c                   clear                   fior90ds
045800080526     c                   eval      or90tla = 'C'
045900080526     c                   eval      or90fas = 'RQE'
046000080526     c                   eval      or90fgs = v1cpor
046100080526     c                   eval      or90ndc = skndc(yy)
046200080526     c                   call      'FIOR90R'
046300080526     c                   parm                    kpjba
046400080526     c                   parm                    fior90ds
046500080526      * manca la fase mando mail
046600080526     c                   if        or90ndcok = *blanks and *in07
046700080526     c                   exsr      sr_mail
046800080526     c                   endif
046900080526
047000080404    3c                   endif
047100080404    2c                   enddo
047200080404    1c                   endif
047300010206
047400010206     C                   enddo
047500080526
047600080526     c                   exsr      sr_end
047700010206
047800010206     C                   clear                   fior06ds
047900010206     C                   movel     'C'           i06tla
048000010206     C                   call      'FIOR06R'
048100010206     C                   parm                    kpjba
048200010206     C                   parm                    fior06ds
048300010206
048400010206     C                   eval      *inlr = *on
048500010206      *****************************************************************
048600010206      * PULIZIA 1° VIDEATA
048700010206      *****************************************************************
048800010206     C     Sr_Pulvid1    BEGSR
048900010206
049000010206     C                   clear                   v1ndc
049100010206     C                   clear                   v1dndc
049200070205     c                   clear                   v1allndc
049300071214     c                   clear                   skddc
049400070207     c                   clear                   skndc
049500080424     c                   clear                   skpdr
049600070302     c                   eval      wdelpda = *off
049700070302     c                   eval      wokrch = *off
049800070302     c                   eval      wokpda = *off
049900010206
050000010206     C                   endsr
050100010206      **********************************************************************
050200010206      * CONTROLLO IL P.O. DI RITIRO
050300010206      **********************************************************************
050400010206     C     Sr_Contrpo    begsr
050500010206
050600010206     C                   setoff                                       4041
050700010206     C                   clear                   v1dpor
050800070125     c                   clear                   og148
050900070926     c                   clear                   og147
051000010206
051100130620     C**** v1cpor        lookup    L1                                     30
051200130620     c                   move      V1Cpor        w003a
051300130620     c     w003a         lookup    SKpog                                  30
051400010206     C     *in30         ifeq      *off
051500010206     C                   seton                                        40  28
051600010206     C                   movel     msg(1)        v1cmsg
051700010206     C                   else
051800010206
051900010206      * Decodifica
052000010206     C     v1cpor        chain     azorg01L
052100010206     C                   if        %found(azorg01l) and ORGfva = *blanks
052200010206     C                   movel     orgDES        v1dpor
052300070125     c                   eval      og148 = orgde8
052400070926     c                   eval      og147 = orgde7
052500010206     C                   else
052600010206     C                   seton                                        40  28
052700010206     C                   movel     msg(1)        v1cmsg
052800010206     C                   endif
052900010207
053000010207      * Devo ricaricare la £6
053100010207     C                   CLEAR                   trul06ds
053200010207     C                   MOVE      '£6'          D06COD
053300010207     C                   MOVEL     v1cpor        D06KEY
053400010207     C                   MOVEL     trul06ds      KPJBU
053500010207     C                   CALL      'TRUL06R'
053600010207     C                   PARM                    KPJBA
053700010207     C                   MOVEL     KPJBU         trul06ds
053800010207     C                   MOVEA     LIN           L6
053900010206     C                   endif
054000070125
054100070125      * p.o. gestione con abilitazione scarico dati per PDA
054200070125     c                   eval      *in01 = (§ogpdaorm = 'S')
054300070926      * data partenza procedura in organigramma
054400071026     c                   eval      *in07 = *off
054500071026     c                   if        §ogddao > *zeros and §ogcgio = 'S'
054600070926     c                   move      §ogddao       data_org
054700071026      * controllo se attiva
054800071026     c                   if        dateu >= data_org
054900071026     c                   eval      *in07 = *on
055000071026     c                   endif
055100071026     c                   endif
055200010206
055300010206     C                   endsr
055400010206      **********************************************************************
055500010206      * CONTROLLI VIDEO 1
055600010206      **********************************************************************
055700010206     C     Sr_Contr01    begsr
055800010206
055900010206      * Reset indicatori
056000010206
056100070212     C                   setoff                                       410203
056200070212
056300070212      * recupero la password del p.o. gestione
056400070223      * se p.o. abilitato al PDA
056500070223     c                   if        *in01
056600070212     c                   clear                   tibs02ds
056700070212     c                   clear                   dsdp
056800070212     c                   eval      t02mod = 'C'
056900070212     c                   eval      t02sif = knsif
057000070212     c                   eval      t02cod = 'SDP'
057100070212     c                   movel(p)  v1cpor        t02ke1
057200070212     c                   call      'TIBS02R'
057300070212     c                   parm                    kpjba
057400070212     c                   parm                    tibs02ds
057500070212     c                   if        t02err <> *blanks
057600070212     c                   eval      v1cmsg = msg(16)
057700070212     c                   eval      *in28 = *on
057800070212     c                   eval      *in41 = *on
057900070212     c                   leavesr
058000070212     c                   endif
058100070212     c                   eval      dsdp = t02uni
058200070223     c                   endif
058300070205
058400070205      * non posso richiedere una distinta e tutte le distinte
058500070205     c                   if        v1allndc = 'SI' and v1ndc <> *zeros and
058600070205     c                             v1ndc <> *blanks
058700070205     c                   eval      v1cmsg = msg(12)
058800070205     c                   eval      *in28 = *on
058900070205     c                   eval      *in41 = *on
059000070205     c                   leavesr
059100070205     c                   endif
059200010206
059300010206      * Ricerca e controllo numero distinta
059400010206
059500070205     c                   if        v1allndc = *blanks
059600010206     C                   exsr      Sr_Contrndc
059700010206     C   90
059800010206     Cor 28              goto      endsrcontr
059900070205     c                   endif
060000070206
060100070206      * richieste tutte le distinte del PDA accendo indicatore
060200070207     c                   if        v1allndc = 'SI'
060300070207     c                   eval      *in02 = *on
060400070207     c                   endif
060500010206
060600010206     C     endsrcontr    endsr
060700010206      **********************************************************************
060800010206      * CONTROLLO   * Numero distinta *
060900010206      **********************************************************************
061000010206     C     Sr_Contrndc   begsr
061100010206
061200080723     c                   movel     *date         dataiso
061300080723     c                   subdur    1:*m          dataiso
061400080723
061500010206      *    Ricerco Distinta
061600010206
061700080723     c                   if        %scan('?':v1ndc) > *zeros
061800080723     c                   clear                   v1dndc
061900080723     c                   clear                   v1ndc
062000080723     c                   clear                   fidg31ds
062100080723     c                   eval      fdgfgs = v1cpor
062200080723     c                   move      dataiso       fdggda
062300080723     c                   eval      fdggal = *date
062400080724     c                   eval      fdgtfv = '2'
062500080723     c                   eval      fdgnpg = 4
062600080723     c                   eval      fdgric = 'S'
062700080723     c                   eval      fdgtpv = 'T'
062800080723      * esclude le distinte con fase RQE
062900080723     c                   eval      fdgiefas = 'E'
063000080723     c                   eval      fdgfas02 = 'RQE'
063100080723     c                   clear                   $fv6
063200080723      * impostato a tutti 99999
063300080723     c                   do        600           xx
063400080723     c                   z-add     999999        $fv6(xx)
063500080723     c                   enddo
063600080723     c                   eval      fdgtpop = 'R'
063700080723     c                   eval      kpjbu = fidg31ds
063800080723     c                   call      'FIDG31R'
063900080723     c                   parm                    kpjba
064000080723     c                   parm                    $fv6
064100080723     c                   eval      fidg31ds = kpjbu
064200080723      * se torno dal pgm senza aver selezionato niente pulisco i dati della DS
064300080723     c                   if        fdgnfv = *zeros
064400080723     c                   clear                   fidg31ds
064500080723     c                   else
064600080723     c                   eval      v1ndc = *all'0'
064700080723     c                   move      fdgnfv        v1ndc
064800080723     c                   endif
064900080723     c                   if        v1ndc = *zeros
065000080723     c                   clear                   v1ndc
065100080723     c                   endif
065200010206     C     v1ndc         ifne      *blanks
065300120613     c                   clear                   ddstflr
065400120613     C                   move      '4'           kdstnpg
065500120613     C                   move      v1ndc         kdstnfv
065600120613     C                   move      v1cpor        kdstfgs
065700120613     C     kfidst        chain     fidst01l
065800120613     c                   if        not %found(fidst01l)
065900060516     c                   clear                   fnlv55ds
066000060516     c                   eval      d55tpt = '6'
066100060516     c                   move      v1cpor        d55lin
066200060516     c                   eval      d55drf = dateu
066300060516     c                   call      'FNLV55R'
066400060516     c                   parm                    fnlv55ds
066500060516     c                   if        d55err = *blanks and d55tfa <> d55lin
066600120613     c                   move      d55tfa        kdstfgs
066700120613     c     kfidst        chain     fidst01l
066800060516     c                   endif
066900060516     c                   endif
067000120613     C                   if        %found(fidst01l)
067100010206     C                   exsr      Sr_Descriz
067200010206     C                   endif
067300010206     C                   seton                                        90  41
067400010206     C                   goto      endsrcndc
067500010206     C                   end
067600080723     c                   endif
067700010206
067800010206      *    Controllo numero Distinta
067900010206
068000010206     C                   clear                   v1dndc
068100010206     C     v1ndc         ifeq      *all'0'
068200010206     C                   clear                   v1dndc
068300070125     c                   clear                   v1ndc
068400010206     C                   endif
068500010206     C     v1ndc         ifne      *blanks
068600010206     C                   testn                   v1ndc                30
068700010206     C   30              move      v1ndc         w001a             1
068800010206     C   30              eval      *in30 = (W001A >= '0')
068900010206     C                   if        *IN30 = *off
069000010206     C                   movel     msg(2)        v1cmsg
069100010206     C                   seton                                        4128
069200010206     C                   goto      endsrcndc
069300010206     C                   endif
069400120613     c                   clear                   ddstflr
069500120613     C                   move      '4'           kdstnpg
069600120613     C                   move      v1ndc         kdstnfv
069700120613     C                   move      v1cpor        kdstfgs
069800120613     C     kfidst        chain     fidst01l
069900120613     c                   if        not %found(fidst01l)
070000060516     c                   clear                   fnlv55ds
070100060516     c                   eval      d55tpt = '6'
070200060516     c                   move      v1cpor        d55lin
070300060516     c                   eval      d55drf = dateu
070400060516     c                   call      'FNLV55R'
070500060516     c                   parm                    fnlv55ds
070600060516     c                   if        d55err = *blanks and d55tfa <> d55lin
070700120613     c                   move      d55tfa        kdstfgs
070800120613     c     kfidst        chain     fidst01l
070900060516     c                   endif
071000060516     c                   endif
071100120613     C                   if        %found(fidst01l)
071200120613     C     dstatb        ifne      *blanks
071300010206     C                   eval      *in28 = *on
071400010206     C                   eval      *in41 = *on
071500010206     C                   movel     msg(2)        v1cmsg
071600010206     C                   goto      endsrcndc
071700010206     C                   else
071800010206     C                   exsr      Sr_Descriz
071900010206     C                   endif
072000010206     C                   else
072100010206     C                   eval      *in28 = *on
072200010206     C                   eval      *in41 = *on
072300010206     C                   movel     msg(2)        v1cmsg
072400010206     C                   goto      endsrcndc
072500010206     C                   endif
072600010206     C                   else
072700010206     C                   eval      *in28 = *on
072800010206     C                   eval      *in41 = *on
072900010206     C                   movel     msg(4)        v1cmsg
073000010206     C                   goto      endsrcndc
073100010206     C                   endif
073200130923
073300130923      * se la distinta che sto cercando di quadrare ha data maggiore di oggi errore
073400130923     c                   IF        DSTdfv > dateu
073500130923     c                   eval      *in28 = *on
073600130923     c                   eval      *in41 = *on
073700130923     c                   eval      V1Cmsg = 'La distinta è del xx/xx/xxxx non +
073800130923     c                             può ancora essere quadrata'
073900130923     c                   eval      %subst(V1Cmsg:19:10) =
074000130923     c                             %subst(%editc(DSTdfv:'X'):7:2) + '/' +
074100130923     c                             %subst(%editc(DSTdfv:'X'):5:2) + '/' +
074200130923     c                             %subst(%editc(DSTdfv:'X'):1:4)
074300130923     c                   leavesr
074400130923     c                   ENDIF
074500090402
074600090402      * controllo se c'è la fase 'RQE' sulla distinta
074700090402     c                   movel     v1ndc         wndc
074800090402     c                   eval      wfase = 'RQE'
074900090402     c                   exsr      sr_ctrfase
075000090402      * se trova la fase distinta già quadrata errore
075100090402     c                   if        or90ndcok <> *blanks
075200090402     c                   eval      *in28 = *on
075300090402     c                   eval      *in41 = *on
075400090402     c                   movel     msg(20)       v1cmsg
075500090402     c                   leavesr
075600090402     c                   endif
075700070206
075800070302      * se p.o. abilitato al PDA
075900070212     c                   if        *in01
076000070302      * controllo se la distinta è una distinta abilitata allo scarico su PDA
076100080319      * ma non in test
076200120613     c                   eval      ddstflr = dstflr
076300120613     c                   if        (dstpda = 'O' or dstpda = 'E') and
076400080319     c                             §dsttstpda <> 'O' and §dsttstpda <> 'E'
076500070302     c                   eval      wokpda = *on
076600090402
076700080415      * controllo se c'è la fase 'RCH' sulla distinta
076800090402     c                   movel     v1ndc         wndc
076900090402     c                   eval      wfase = 'RCH'
077000080415     c                   exsr      sr_ctrfase
077100080415      * se non trova la fase devo emettere una videata per richiedere la
077200080415      * password per forzare la quadratura
077300090402     c                   if        or90ndcok = *blanks
077400070212     c                   exsr      sr_deldapda
077500070302     c                   else
077600070302     c                   eval      wokrch = *on
077700070212     c                   endif
077800080319     c                   endif
077900070302     c                   endif
078000010206
078100010206     C     endsrcndc     endsr
078200010206      **********************************************************************
078300010206      * Recupero Descrizione Distinta
078400010206      **********************************************************************
078500010206     C     Sr_Descriz    begsr
078600010206
078700021202     C                   movel     DSTpdr        kpdpdr
078800021202     C                   move      'A'           kpdtip
078900021202     C     kfapd         chain     fiapd01L
079000021203     C                   if        not%found(fiapd01l)
079100010206     C                   clear                   apdrsc
079200010206     C                   endif
079300010206
079400010206     C                   movel     'DEL '        wdel
079500120613     C                   move      dstdfv        wggg
079600010206     C                   move      '/'           wbarra1
079700010206     C                   move      '/'           wbarra2
079800120613     C                   move      dstdfv        wa4               4
079900010206     C                   movel     wa4           wmmm
080000120613     C                   movel     dstdfv        wa4               4
080100010206     C                   movel(p)  wa4           waaa
080200010206     C     dstpdr        ifne      0
080300010206     C                   movel(p)  dstpdr        wpdr
080400010206     C                   else
080500010206     C                   clear                   wpdr
080600010206     C                   endif
080700010206     C                   movel     apdrsc        wdescr
080800010206
080900010206     C                   endsr
081000070207
081100070207      *--------------------------------------------------------------------*
081200070207      * CANCELLO LA DISTINTA DA PDA
081300070207      *--------------------------------------------------------------------*
081400070207     c     sr_deldapda   begsr
081500070207
081600070207     c                   do        *hival
081700070207     c                   exfmt     fior26w
081800070207     c                   setoff                                       28
081900070207     c                   clear                   wfcmsg
082000070208     c   kl              leave
082100070207      * PASSWORD
082200070207     c                   if        wpassw = *blanks
082300070207     c                   eval      *in28 = *On
082400070207     c                   eval      wfcmsg = msg(15)
082500070207     c                   iter
082600070207     c                   endif
082700070207      * controllo se esatta
082800070212     c                   if        wpassw <> §sdppwd
082900070207     c                   eval      *in28 = *On
083000070207     c                   eval      wfcmsg = msg(14)
083100070207     c                   iter
083200070207     c                   endif
083300070212      * Controllo la validità
083400070212      * Data immissione/variazione password
083500070212     c                   clear                   wlbdat
083600070212     c                   z-add     §sdpdta       g02dat
083700070212     c                   call      'XSRDA8'
083800070212     c                   parm                    wlbdat
083900070212     c                   movel     g02inv        dataiso
084000070212      * Scadenza password
084100070212     c                   clear                   tibs02ds
084200070212     c                   eval      t02mod = 'C'
084300070212     c                   eval      t02sif = knsif
084400070212     c                   eval      t02cod = 'SDP'
084500070212     c                   eval      t02ke1 = 'PSW'
084600070212     c                   call      'TIBS02R'
084700070212     c                   parm                    kpjba
084800070212     c                   parm                    tibs02ds
084900070212     c                   if        t02err <> *blanks
085000070212     c                   eval      *in28 = *On
085100070212     c                   eval      wfcmsg = msg(17)
085200070212     c                   iter
085300070212     c                   endif
085400070212     c                   movel     t02uni        wgiorni
085500070212     c                   adddur    wgiorni:*d    dataiso
085600070212     c                   move      dataiso       datascad
085700070212     c                   if        dateu > dataScad
085800070212     c                   eval      *In28 = *On
085900070212     c                   eval      wfcmsg = msg(18)
086000070212     c                   iter
086100070212     c                   endif
086200070208     c   kl
086300070208     cor kf              leave
086400070207     c                   enddo
086500070207
086600070208     c                   if        *inkf
086700070302     c                   eval      wdelpda = *on
086800070207     c                   endif
086900070207
087000070207     c                   endsr
087100070207
087200010206      **********************************************************************
087300010206      * GESTIONE SUBFILE
087400010206      **********************************************************************
087500010206     C     Sr_Gessfl     begsr
087600010206
087700010208     C                   do        *hival
087800010206     C                   z-add     1             recsf
087900010206     C                   setoff                                       47
088000010518     C                   eval      *in45 = *off
088100010206      * Pulisco subfile
088200010206     C                   z-add     *zeros        nrr               4 0
088300010206     C                   eval      *in21 = *off
088400010206     C                   write     fior26c
088500010206     C                   eval      *in20 = *on
088600010206     C                   eval      *in21 = *on
088700060516
088800060516     c                   do        30            xx
088900060516     c                   if        l6(xx) = *zeros
089000060516     c                   leave
089100060516     c                   endif
089200010206
089300010206      * Carico il subfile
089400060516     c                   move      l6(xx)        keypor
089500010206     C                   movel     v1ndc         keyndc
089600010206     C     kfnorm        setll     fnorm06l
089700010206     C                   do        *hival
089800010206     C     kfnorm        reade     fnorm06l
089900010206
090000010206     C                   if        %eof
090100010206     C                   leave
090200010206     C                   endif
090300010206
090400010206      * solo 8000 record xchè il file video è definito così
090500010206     C                   if        nrr > 8000
090600010206     C                   leave
090700010206     C                   endif
090800010206
090900070206      * solo gli orm in fase 400 o 410 o 420
091000070207     C                   if        ormfao <> 400 and ormfao <> 410 and
091100070206     c                             ormfao <> 420
091200010206     C                   iter
091300010206     C                   endif
091400070212
091500070212      * sto caricando una sola distinta quindi spengo il 50
091600070212      * così ho tutti i campi del subfile visibili
091700070212     c                   eval      *in50 = *off
091800010206
091900010206     C                   exsr      Sr_Carsfl
092000010206     C                   enddo
092100060516
092200060516     c                   enddo
092300100409
092400100409     C                   IF        $GIRO > 0  AND NRR = 0
092500100409     C                   LEAVESR
092600100409     C                   ENDIF
092700010220
092800010220     C     emettosf      tag
092900100409
093000100409     C                   EVAL      $GIRO = 1
093100010206
093200010206      * Se non ho caricato emetto una videata di segnalazione.
093300010206     C                   if        nrr = *zeros
093400010206     C                   exfmt     fior263
093500010206     C                   eval      *in90 = *on
093600070423     C                   exsr      Sr_Pulvid1
093700010206     C                   endif
093800010206
093900010206      * Se ho caricato emetto il subfile
094000010206     C                   if        nrr > *zeros
094100010206     C                   write     fior262
094200010206     C                   exfmt     fior26c
094300010206     C                   endif
094400010206
094500010206      * F6=Conferma Totale
094600010206     C                   if        *inkf = *on
094700010206     C                   move      '1'           flgkf
094800010206     C                   else
094900010206     C                   clear                   flgkf
095000010206     C                   endif
095100010206      * F12=Ritorno
095200010206     C   kl              leave
095300010206
095400010206      *    Leggo Scelte Effettuate dall'utente
095500010206
095600010206     C                   if        nrr >*zeros
095700010206
095800010206     C                   eval      nrr = *zeros
095900010206     C                   do        *hival
096000010206     C                   eval      nrr = nrr +1
096100010206     C     nrr           chain     fior26s                            30
096200010206     C                   if        *in30 = *on
096300010206     C                   leave
096400010206     C                   endif
096500010206
096600010206     C                   setoff                                       28  47
096700010302     C                   clear                   dcmr
096800010206
096900010206      * Se immessa una causale questa deve esistere in tabella CMR
097000010206     C                   if        vs1cau <> *blanks
097100010206     C                   exsr      Sr_Contrcau
097200010206     C                   z-add     nrr           recsf
097300010206     C                   if        *in43 = *on
097400010206     C                   seton                                        28  47
097500010206     C                   update    fior26s
097600010206     C                   goto      emettosf
097700010206     C                   endif
097800010518      * Se orm ha già una bolla non posso dichiararlo non fatto
097900010411      * se non è un prepagato
098000010411     C                   if        vs1tor <> 'P'
098100010208     C                   exsr      Sr_Bolla
098200010518     C                   if        *in45 = *on
098300010208     C                   seton                                        28  47
098400010208     C                   update    fior26s
098500010208     C                   goto      emettosf
098600010208     C                   endif
098700010411     C                   endif
098800010206     C                   endif
098900010206      * Note
099000010206     C                   if        vs1not <> *blanks
099100010206     C                   z-add     nrr           recsf
099200010302     C                   clear                   fior06ds
099300010302     C                   movel     vs1fao        i06far
099400080410     c                   movel     'V'           i06flm
099500010301     C                   exsr      Sr_Note
099600010208     C                   move      *blanks       vs1not
099700050427     c                   If        flgkf <> '1'
099800010208     C                   update    fior26s
099900050427     C                   goto      emettosf
100000050427     c                   EndIf
100100010206     C                   endif
100200010206
100300010206      * F6 conferma totale del subfile
100400060309      * confermo orm se non è ancora stato confermato
100500060309     C                   if        flgkf = '1' and vs1okorm = *blanks
100600060309     C                   z-add     nrr           recsf
100700010206     C                   exsr      Sr_Conferma
100800011015     C                   if        *in44 = *on or
100900020524     C                             *in48 = *on or *in46
101000010206     C                   seton                                        28  47
101100010206     C                   update    fior26s
101200010606     C                   leave
101300010206     C                   endif
101400010301      * Se la causale prevede le note queste sono obbligatorie
101500010301     C                   if        vs1cau <> *blanks
101600020529     c                             and okbolla = *blanks
101700010301     C                   clear                   dcmr
101800010301     C                   clear                   TIBS02DS
101900010301     C                   movel     'C'           T02mod
102000010301     C                   movel     knsif         t02sif
102100010301     C                   movel     'CMR'         t02cod
102200010301     C                   movel(p)  vs1cau        T02ke1
102300010301     C                   call      'TIBS02R'
102400010301     C                   parm                    KPJBA
102500010301     C                   parm                    TIBS02DS
102600010301     C                   movel     t02ke1        vs1cau
102700010301     C                   movel     t02uni        dcmr
102800040413      * salvo i dati che mi possono servire x la chiusura ORM
102900040413     c                   Eval      wtenta = d§cmrnca
103000040413     c                   Move      d§cmrcca      sav_cmrcau
103100010302     C                   if        d§cmrnot <> *blanks
103200010302     C                   do        *hival
103300010302     C                   clear                   fior06ds
103400010302     C                   movel     d§cmrfar      i06far
103500080410     c                   movel     'V'           i06flm
103600080410     c                   if        *in01 and wokpda = *on and wokrch = *on
103700080410     c                   movel     'P'           i06flm
103800080410     c                   endif
103900010301     C                   exsr      Sr_Note
104000010301     C                   if        o06f03 = 'S' or o06f12 = 'S'
104100010301     C                   iter
104200010301     C                   endif
104300010301     C                   if        o06dai <> ormdfo
104400010301     C                   iter
104500010301     C                   endif
104600010301     C                   if        o06ori < ormofo
104700010301     C                   iter
104800010301     C                   endif
104900010301     C                   if        o06prg = 0
105000010301     C                   iter
105100010301     C                   endif
105200010301     C                   leave
105300010301     C                   enddo
105400010302     C                   endif
105500010301     C                   endif
105600040406
105700040406      * se ho quadrato con causale controllo quanti tentativi posso fare prima di chiudere
105800040406      * l'ORM in automatico
105900040413     c                   If        vs1cau <> *Blanks and d§cmrnca <> *Zeros
106000040406     c                             and okbolla = *Blanks
106100040406     c                   ExSr      Sr_ContrFase
106200040406     c                   If        wchiudi = *On
106300040406     c                   ExSr      Sr_Chiudi
106400040406     c                   EndIf
106500040406     c                   EndIf
106600010301
106700060309      * se arrivo qua vuol dire che l'orm è stato avanzato quindi aggiorno il flag
106800060309     c                   eval      vs1okorm = '1'
106900010206     C                   endif
107000060309
107100010206     C                   update    fior26s
107200010206
107300010206     C                   enddo
107400010206     C                   endif
107500010206
107600010208     C                   if        flgkf = *blanks
107700010206     C                   goto      emettosf
107800010529     C                   else
107900011015     C                   if        *in44 = *on or
108000020524     C                             *in48 = *on or *in46
108100010606     C                   iter
108200010606     C                   else
108300020529      * se non ho scritto una bolla riemetto il subfile
108400020529     c                   if        wnobolla = *on
108500020529     c                   eval      wnobolla = *off
108600080520     c                   iter
108700020529     C                   else
108800010529     C                   leave
108900020529     c                   endif
109000010606     C                   endif
109100010208     C                   endif
109200020529
109300010206
109400010208     C                   enddo
109500010206
109600010206     C                   endsr
109700070207
109800070205      *--------------------------------------------------------------------*
109900070206      * GESTIONE SUBFILE DI TUTTE LE DISTINTE PASSATE A PDA
110000070206      *--------------------------------------------------------------------*
110100070206     c     sr_allndc     begsr
110200070205
110300070206     c                   do        *hival
110400070206     c                   z-add     1             recsf
110500070206     c                   setoff                                       47
110600070206     c                   eval      *in45 = *off
110700081106     c                   clear                   savormndc
110800070206      * pulisco subfile
110900070206     c                   z-add     *zeros        nrr               4 0
111000070206     c                   eval      *in21 = *off
111100070206     c                   write     fior26c
111200070206     c                   eval      *in20 = *on
111300070206     c                   eval      *in21 = *on
111400070205
111500070205     c                   do        30            xx
111600070205     c                   if        l6(xx) = *zeros
111700070205     c                   leave
111800070205     c                   endif
111900070205
112000070206      * carico il subfile
112100070205     c                   move      l6(xx)        keypor
112200070206     c     keypor        setll     fnorm06l
112300070206     c                   do        *hival
112400070206     c     keypor        reade     fnorm06l
112500070205
112600070206     c                   if        %eof
112700070206     c                   leave
112800070206     c                   endif
112900070205
113000070205      * solo 8000 record xchè il file video è definito così
113100070206     c                   if        nrr > 8000
113200070206     c                   leave
113300070206     c                   endif
113400070206
113500070206      * solo gli orm in fase 400 - 410 - 420
113600070207     c                   if        ormfao <> 400 and ormfao <> 410 and
113700070206     c                             ormfao <> 420
113800070206     c                   iter
113900070206     c                   endif
114000070206
114100080415      * controllo se la distinta ha la fase 'RCH'
114200080415     c                   eval      wndc = ormndc
114300090402     c                   eval      wfase = 'RCH'
114400080415     c                   exsr      sr_ctrfase
114500080415      * se non trova la fase salto la distinta
114600080415     c                   if        or90ndcok = *blanks
114700070206     c                   iter
114800070206     c                   endif
114900080404
115000080404      * controllo se la distinta è una distinta abilitata allo scarico su PDA
115100080404      * ma non in test
115200120613     C                   move      '4'           kdstnpg
115300120613     c                   eval      kdstnfv = ormndc
115400120613     c                   eval      kdstfgs = keypor
115500080404     c                   clear                   ddstflr
115600080404     c     kfidst        chain     fidst01l
115700080404     c                   if        %found(fidst01l)
115800120613     c                   eval      ddstflr = dstflr
115900080404      * non attiva con PDA
116000120613     c                   if        dstpda <> 'O' and dstpda <> 'E'
116100080404     c                   iter
116200080404     c                   endif
116300080404      * in test
116400080404     c                   if        §dsttstpda = 'O' or §dsttstpda = 'E'
116500080404     c                   iter
116600080404     c                   endif
116700080404     c                   endif
116800070212
116900070212      * per ogni distinta devo prima emettere una riga con i dati della distinta
117000070212      * poi i rcd relativi alla distinta
117100070212     c                   if        ormndc <> savormndc
117200070212     c                   exsr      sr_carsfld
117300070212     c                   eval      savormndc = ormndc
117400080404     c                   eval      *in50 = *off
117500080404     C                   exsr      Sr_Carsfl
117600070212     c                   else
117700070212     c                   eval      *in50 = *off
117800070205     C                   exsr      Sr_Carsfl
117900070212     c                   endif
118000070205     C                   enddo
118100070205
118200070205     c                   enddo
118300100409
118400100409     C                   IF        $GIRO > 0  AND NRR = 0
118500100409     C                   LEAVESR
118600100409     C                   ENDIF
118700070205
118800070206     C     emettosfall   tag
118900100409
119000100409     C                   EVAL      $GIRO = 1
119100070205
119200070205      * Se non ho caricato emetto una videata di segnalazione.
119300070205     C                   if        nrr = *zeros
119400070205     C                   exfmt     fior263
119500070205     C                   eval      *in90 = *on
119600070205     C                   endif
119700070205
119800070205      * Se ho caricato emetto il subfile
119900070205     C                   if        nrr > *zeros
120000070205     C                   write     fior262
120100070205     C                   exfmt     fior26c
120200070205     C                   endif
120300070205
120400070205      * F6=Conferma Totale
120500070205     C                   if        *inkf = *on
120600070205     C                   move      '1'           flgkf
120700070205     C                   else
120800070205     C                   clear                   flgkf
120900070205     C                   endif
121000070205      * F12=Ritorno
121100070205     C   kl              leave
121200070205
121300070205      *    Leggo Scelte Effettuate dall'utente
121400070205
121500070205     C                   if        nrr >*zeros
121600070205
121700070205     C                   eval      nrr = *zeros
121800070205     C                   do        *hival
121900070205     C                   eval      nrr = nrr +1
122000070205     C     nrr           chain     fior26s                            30
122100070205     C                   if        *in30 = *on
122200070205     C                   leave
122300070205     C                   endif
122400080218
122500080218      * se riga di descrizione distinta leggo record sucessivo
122600080218     c                   if        vs1in50 = *on
122700080218     c                   iter
122800080218     c                   endif
122900070205
123000070205     C                   setoff                                       28  47
123100070205     C                   clear                   dcmr
123200070205
123300070205      * Se immessa una causale questa deve esistere in tabella CMR
123400070205     C                   if        vs1cau <> *blanks
123500070205     C                   exsr      Sr_Contrcau
123600070205     C                   z-add     nrr           recsf
123700070205     C                   if        *in43 = *on
123800070205     C                   seton                                        28  47
123900080117     c                   eval      *in49 = vs1in49
124000080117     c                   eval      *in50 = vs1in50
124100070205     C                   update    fior26s
124200070206     C                   goto      emettosfall
124300070205     C                   endif
124400070205      * Se orm ha già una bolla non posso dichiararlo non fatto
124500070205      * se non è un prepagato
124600070205     C                   if        vs1tor <> 'P'
124700070205     C                   exsr      Sr_Bolla
124800070205     C                   if        *in45 = *on
124900070205     C                   seton                                        28  47
125000080117     c                   eval      *in49 = vs1in49
125100080117     c                   eval      *in50 = vs1in50
125200070205     C                   update    fior26s
125300070206     C                   goto      emettosfall
125400070205     C                   endif
125500070205     C                   endif
125600070205     C                   endif
125700070205      * Note
125800070205     C                   if        vs1not <> *blanks
125900070205     C                   z-add     nrr           recsf
126000070205     C                   clear                   fior06ds
126100070205     C                   movel     vs1fao        i06far
126200080410     c                   movel     'V'           i06flm
126300070205     C                   exsr      Sr_Note
126400070205     C                   move      *blanks       vs1not
126500070205     c                   If        flgkf <> '1'
126600080117     c                   eval      *in49 = vs1in49
126700080117     c                   eval      *in50 = vs1in50
126800070205     C                   update    fior26s
126900070206     C                   goto      emettosfall
127000070205     c                   EndIf
127100070205     C                   endif
127200070205
127300070205      * F6 conferma totale del subfile
127400070205      * confermo orm se non è ancora stato confermato
127500070205     C                   if        flgkf = '1' and vs1okorm = *blanks
127600070205     C                   z-add     nrr           recsf
127700070205     C                   exsr      Sr_Conferma
127800070205     C                   if        *in44 = *on or
127900070205     C                             *in48 = *on or *in46
128000070205     C                   seton                                        28  47
128100080117     c                   eval      *in49 = vs1in49
128200080117     c                   eval      *in50 = vs1in50
128300070205     C                   update    fior26s
128400070205     C                   leave
128500070205     C                   endif
128600070205      * Se la causale prevede le note queste sono obbligatorie
128700070205     C                   if        vs1cau <> *blanks
128800070205     c                             and okbolla = *blanks
128900070205     C                   clear                   dcmr
129000070205     C                   clear                   TIBS02DS
129100070205     C                   movel     'C'           T02mod
129200070205     C                   movel     knsif         t02sif
129300070205     C                   movel     'CMR'         t02cod
129400070205     C                   movel(p)  vs1cau        T02ke1
129500070205     C                   call      'TIBS02R'
129600070205     C                   parm                    KPJBA
129700070205     C                   parm                    TIBS02DS
129800070205     C                   movel     t02ke1        vs1cau
129900070205     C                   movel     t02uni        dcmr
130000070205      * salvo i dati che mi possono servire x la chiusura ORM
130100070205     c                   Eval      wtenta = d§cmrnca
130200070205     c                   Move      d§cmrcca      sav_cmrcau
130300070205     C                   if        d§cmrnot <> *blanks
130400070205     C                   do        *hival
130500070205     C                   clear                   fior06ds
130600070205     C                   movel     d§cmrfar      i06far
130700080410     c                   movel     'P'           i06flm
130800070205     C                   exsr      Sr_Note
130900070205     C                   if        o06f03 = 'S' or o06f12 = 'S'
131000070205     C                   iter
131100070205     C                   endif
131200070205     C                   if        o06dai <> ormdfo
131300070205     C                   iter
131400070205     C                   endif
131500070205     C                   if        o06ori < ormofo
131600070205     C                   iter
131700070205     C                   endif
131800070205     C                   if        o06prg = 0
131900070205     C                   iter
132000070205     C                   endif
132100070205     C                   leave
132200070205     C                   enddo
132300070205     C                   endif
132400070205     C                   endif
132500070205
132600070205      * se ho quadrato con causale controllo quanti tentativi posso fare prima di chiudere
132700070205      * l'ORM in automatico
132800070205     c                   If        vs1cau <> *Blanks and d§cmrnca <> *Zeros
132900070205     c                             and okbolla = *Blanks
133000070205     c                   ExSr      Sr_ContrFase
133100070205     c                   If        wchiudi = *On
133200070205     c                   ExSr      Sr_Chiudi
133300070205     c                   EndIf
133400070205     c                   EndIf
133500070205
133600070205      * se arrivo qua vuol dire che l'orm è stato avanzato quindi aggiorno il flag
133700070205     c                   eval      vs1okorm = '1'
133800070205     C                   endif
133900070205
134000080117     c                   eval      *in49 = vs1in49
134100080117     c                   eval      *in50 = vs1in50
134200070205     C                   update    fior26s
134300070205
134400070205     C                   enddo
134500070205     C                   endif
134600070205
134700070205     C                   if        flgkf = *blanks
134800070206     C                   goto      emettosfall
134900070205     C                   else
135000070205     C                   if        *in44 = *on or
135100070205     C                             *in48 = *on or *in46
135200070205     C                   iter
135300070205     C                   else
135400070205      * se non ho scritto una bolla riemetto il subfile
135500070205     c                   if        wnobolla = *on
135600070205     c                   eval      wnobolla = *off
135700080520     c                   iter
135800070205     C                   else
135900070205     C                   leave
136000070205     c                   endif
136100070205     C                   endif
136200070205     C                   endif
136300070205
136400070205     C                   enddo
136500070205
136600070205     C                   endsr
136700070207
136800010206      **********************************************************************
136900010206      * CARICO SUBFILE
137000010206      **********************************************************************
137100010206     C     Sr_Carsfl     begsr
137200010206
137300070207     c                   eval      *in49 = *off
137400060309     c                   clear                   vs1okorm
137500010206     C                   clear                   vs1cau
137600010206     C                   clear                   vs1not
137700010206      * Inversione data ORM
137800010206     C                   z-add     ORMdao        G02inv
137900010206     C                   movel     '3'           G02err
138000010206     C                   call      'XSRDA8'
138100010206     C                   parm                    wlbdat
138200010206     C                   movel     G02dat        vs1dao
138300070212     C                   eval      vs1tor = ORMtor
138400070212     C                   eval      vs1poe = ORMpoe
138500070212     C                   eval      vs1nsr = ORMnsr
138600070212     C                   eval      vs1nor = ORMnor
138700070212     C                   eval      vs1nrv = ORMnrv
138800140213
138900140213     c                   eval      vs1pdr = dstpdr
139000140213     c                   eval      vs1fpp = dstfpp
139100070212
139200070212      * imposto la riga da emettere a video
139300070212     c                   eval      vs1riga =
139400070212     c                             %editc(%dec(ormpoe):'X') + ' ' +
139500070212     c                             %editc(%dec(ormnsr):'Z') + ' ' +
139600070212     c                             %editc(%dec(ormnor):'Z') + ' ' +
139700070212     c                             %editc(%dec(ormnrv):'Z') + ' ' +
139800130417     c                             %subst(ormrsr:1:25) + ' ' +
139900070212     c                             %subst(%editc(%dec(ormdar):'X'):7:2) +
140000070212     c                             '/' +
140100070212     c                             %subst(%editc(%dec(ormdar):'X'):5:2) +
140200070212     c                             '/' +
140300070212     c                             %subst(%editc(%dec(ormdar):'X'):1:4) +
140400070212     c                             ' ' +
140500070212     c                             %subst(%editc(%dec(ormorr):'Z'):1:2) +
140600070212     c                             ':' +
140700070212     c                             %subst(%editc(%dec(ormorr):'X'):3:2)
140800070212     C                   eval      vs1fao = ORMfao
140900070212     C                   eval      vs1por = ORMpor
141000070206
141100070206      * se distinta scaricata su PDA recupero i dati dal file FIPLG
141200070302     c                   if        (*in01 and wokpda = *on and wokrch = *on)
141300070302     c                             or *in02
141400070206     c                   eval      ktiporcd = 'RES'
141500070529     c                   eval      ktipappl = 'R'
141600070206     c                   eval      keyndc = ormndc
141700070206     c     kfiplg01      setll     fiplg01l
141800070206     c                   do        *hival
141900070206     c     kfiplg01      reade     fiplg01l
142000070206     c                   if        %eof(fiplg01l)
142100070206     c                   leave
142200070206     c                   endif
142300070206     c                   eval      fiprcresds = plgdati
142400070206     c                   eval      vs1cau = §rescmr
142500070206     c                   enddo
142600070206      * se fase ancora 400 la metto in RI così l'utente nota che non ci sono
142700070206      * stati esiti per questo ORM
142800070207     c                   if        ormfao = 400
142900070207     c                   eval      *in49 = *on
143000070206      * inoltre forzo la causale = a 'XX' (non esiste in tabella così si ferma)
143100070206     c                   eval      vs1cau = 'XX'
143200070207     c                   endif
143300070206     c                   endif
143400140611      * mi salvo la distinta in schiera
143500070207     c                   eval      yy = 1
143600070207     c     ormndc        lookup    skndc(yy)                              30
143700070207     c                   if        not *in30
143800070207     c     *zeros        lookup    skndc(yy)                              30
143900070207     c                   if        *in30
144000070207     c                   eval      skndc(yy) = ormndc
144100071214     c                   eval      skddc(yy) = ormddc
144200140213     c                   eval      skpdr(yy) = dstpdr
144300070207     c                   endif
144400070207     c                   endif
144500080117
144600080117      * mi salvo gli indicatori di visualizzazione riga
144700080117     c                   eval      vs1in49 = *in49
144800080117     c                   eval      vs1in50 = *in50
144900010206
145000010206     C                   add       1             nrr
145100010206     C                   write     fior26s
145200010206
145300010206     C                   endsr
145400070212
145500070212      *-------------------------------------------------------------------------------*
145600070212      * CARICO SUBFILE - riga distinta per separare nel caso di allndc = 'S'
145700070212      *-------------------------------------------------------------------------------*
145800070212     c     sr_carsfld    begsr
145900070212
146000070212      * per prima cosa accendo l'indicatore 50 così non sono disponibili i campi della
146100070212      * riga di dettaglio
146200070212     c                   eval      *in50 = *on
146300070212
146400070212      * poi pulisco i campi di dettaglio
146500070212     c                   clear                   vs1okorm
146600070212     c                   clear                   vs1cau
146700070212     c                   clear                   vs1not
146800070212     c                   clear                   vs1nsr
146900070212     c                   clear                   vs1nor
147000070212     c                   clear                   vs1nrv
147100070212     c                   clear                   vs1fao
147200070212     c                   clear                   vs1por
147300070212     c                   clear                   vs1tor
147400070212     c                   clear                   vs1dao
147500140213     c                   clear                   vs1pdr
147600140213     c                   clear                   vs1fpp
147700070212      * spengo indicatore di visualizzazione
147800070212     c                   eval      *in49 = *off
147900070212
148000070212      * ora cerco i dati relativi alla distinta e compongo la riga del subfile
148100070212     c                   clear                   vs1riga
148200070212     c                   eval      kpdpdr = dstpdr
148300070212     c                   eval      kpdtip = 'A'
148400070212     c     kfapd         chain     fiapd01L
148500070212     c                   if        %found(fiapd01l)
148600070212     c                   eval      vs1riga = 'Distinta ' +
148700070212     c                             %editc(%dec(dstnfv):'X') + ' ' +
148800070212     c                             'del ' +
148900120613     c                             %subst(%editc(%dec(dstdfv):'X'):7:2) +
149000070212     c                             '/' +
149100120613     c                             %subst(%editc(%dec(dstdfv):'X'):5:2) +
149200070212     c                             '/' +
149300120613     c                             %subst(%editc(%dec(dstdfv):'X'):1:4) +
149400070212     c                             ' ' + 'Aut. ' +
149500070212     c                             %editc(%dec(dstpdr):'X') + ' ' +
149600070212     c                             apdrsc
149700070212     c                   endif
149800080117
149900080117      * mi salvo gli indicatori di visualizzazione riga
150000080117     c                   eval      vs1in49 = *in49
150100080117     c                   eval      vs1in50 = *in50
150200070212
150300070212     c                   add       1             nrr
150400070212     c                   write     fior26s
150500070212
150600070212     c                   endsr
150700010206      **********************************************************************
150800010206      * CONTROLLO  * Causale mancato ritiro *
150900010206      **********************************************************************
151000010206     C     Sr_Contrcau   BEGSR
151100010206
151200010206     C                   eval      *in43 = *off
151300010206
151400010206     C     '?'           scan      vs1cau                                 30
151500010206     C     *in30         ifeq      *on
151600010301     C                   clear                   dcmr
151700010206     C                   clear                   TIBS02DS
151800010206     C                   movel     'R'           t02mod
151900010206     C                   movel     knsif         t02sif
152000010206     C                   movel     'CMR'         t02cod
152100010206     C                   call      'TIBS02R'
152200010206     C                   parm                    KPJBA
152300010206     C                   parm                    TIBS02DS
152400010206     C                   movel     T02ke1        vs1cau
152500010206     C                   endif
152600010206
152700010206      * Controllo Causale
152800010206     C                   if        vs1cau <> *blanks
152900010301     C                   clear                   dcmr
153000010206     C                   clear                   TIBS02DS
153100010206     C                   movel     'C'           T02mod
153200010206     C                   movel     knsif         t02sif
153300010206     C                   movel     'CMR'         t02cod
153400010206     C                   movel(p)  vs1cau        T02ke1
153500010206     C                   call      'TIBS02R'
153600010206     C                   parm                    KPJBA
153700010206     C                   parm                    TIBS02DS
153800010206     C                   if        t02err <> *blanks
153900010206     C                   movel     msg(5)        vs1msg
154000010206     C                   eval      *in43 = *on
154100010206     C                   goto      endsrcau
154200010206     C                   endif
154300010206     C                   movel     t02ke1        vs1cau
154400010301     C                   movel     t02uni        dcmr
154500010302     C                   if        d§cmrnoq <> *blanks
154600010305     C                   movel     msg(8)        vs1msg
154700010302     C                   eval      *in43 = *on
154800010302     C                   goto      endsrcau
154900010302     C                   endif
155000010206     C                   endif
155100010206
155200010206     C     endsrcau      endsr
155300010208      **********************************************************************
155400010208      * CONTROLLO  * Orm / bolla *
155500010208      **********************************************************************
155600010208     C     Sr_Bolla      BEGSR
155700081106
155800081106     c                   eval      *in45 = *off
155900010208
156000010208     C                   clear                   worm             14
156100010208     C                   move      vs1poe        wpoe              3
156200010208     C                   move      vs1nsr        wnsr              2
156300010208     C                   move      vs1nor        wnor              7
156400010208     C                   move      vs1nrv        wnrv              2
156500010208     C                   eval      worm = wpoe + wnsr
156600010208     C                             + wnor + wnrv
156700060214     C                   movel     worm          kar4n14
156800060214     C     kfiar4        chain     fiar404l
156900060214     C                   if        %found(fiar404l)
157000010208     C                   movel     msg(7)        vs1msg
157100010518     C                   eval      *in45 = *on
157200010208     C                   goto      endsrbolla
157300010208     C                   endif
157400010208
157500010208     C     endsrbolla    endsr
157600010206      **********************************************************************
157700010206      * RICHIAMA IL PROGRAMMA DELLE NOTE
157800010206      **********************************************************************
157900010206     C     Sr_Note       BEGSR
158000010206
158100010206     C                   clear                   parm01
158200010206     C                   movel     v1cpor        pfgs
158300010206     C                   movel     vs1dao        i06dta
158400010206     C                   z-add     vs1poe        i06poe
158500010206     C                   z-add     vs1nor        i06nor
158600010206     C                   z-add     vs1nsr        i06nsr
158700010206     C                   z-add     vs1nrv        i06nrv
158800010206     C                   eval      i06dai = dateu
158900010206     C                   TIME                    W0140
159000010206     C                   movel     w0140         I06ori
159100080319      * devo impostare la causale nelle note così viene visualizzata dal
159200080319      * pgm di gestione note
159300080319     c                   clear                   fior06vds
159400080319     c                   if        vs1cau <> *blanks
159500080319     c                   eval      v06car = vs1cau
159600080319     c                   eval      v06dcar = d§cmrdes
159700080319     c                   endif
159800010206
159900010206     C                   movel     kpjbu         §kpjbu
160000010206     C                   movel     parm01        kpjbu
160100010206     C                   CALL      'FIOR06R'
160200010206     C                   PARM                    kpjba
160300010206     C                   parm                    fior06ds
160400080319     C                   parm                    fior06vds
160500010206     C                   movel     kpjbu         parm01
160600010206     C                   movel     §kpjbu        kpjbu
160700010206
160800010206     C                   endsr
160900010206      **********************************************************************
161000010206      * CONFERMA
161100010206      **********************************************************************
161200010206     C     Sr_Conferma   BEGSR
161300010206
161400010206     C                   eval      *in44 = *off
161500010530     C                   eval      *in46 = *off
161600011015     C                   eval      *in48 = *off
161700010530
161800010530      * Controllo che orm non sia allocato
161900010530     C     kfnorm1       chain(e)  fnorm01l
162000010530     C                   if        %error
162100010530     C                   movel     msg(6)        vs1msg
162200010530     C                   eval      *in44 = *on
162300010530     C                   goto      endsrconf
162400010530     C                   endif
162500010530
162600010530      * Se orm è in fase di bollettato (900) non devo aggiornare
162700020524     C                   if        ormfao = 900
162800020524     C                   unlock    fnorm01l
162900020524     C                   movel     msg(11)       vs1msg
163000020524     C                   eval      *in46 = *on
163100020524     C                   goto      endsrconf
163200020524     c                   endif
163300011015
163400011015      * Se orm è chiuso e sto cercando di chiuderlo di nuovo devo dare errore
163500030901     C                   if        (ormfao = 999 or OrmFao = 910) and
163600030901     C                             (d§cmrfar = '999' or d§cmrfar = '910')
163700011015     C                   movel     msg(10)       vs1msg
163800011015     C                   eval      *in48 = *on
163900011015     C                   goto      endsrconf
164000011015     C                   endif
164100040406
164200040406     c                   Eval      kOrfPoe = OrmPoe
164300040406     c                   Eval      kOrfNsr = OrmNsr
164400040406     c                   Eval      kOrfNor = OrmNor
164500040406     c                   Eval      kOrfNrv = OrmNrv
164600040406     c                   Eval      kOrfDae = OrmDfo
164700040406     c                   Eval      kOrfOre = OrmOfo
164800040406     c                   Eval      kOrfFar = OrmFao
164900020524
165000020524     c                   eval      ds_ormcra = ormcra
165100020524     c                   eval      ds_ormcor = ormcor
165200020524     c                   eval      ds_ormcrc = ormcrc
165300010206
165400010206      * RITIRATI
165500010206     C                   if        vs1cau = *blanks
165600010206      * Aggiorno ORM e scrivo fase
165700010529     C                   time                    w0140
165800010529     C                   move      w0140         wdtgio
165900010529     C                   z-add     wdtgio        G02DAT
166000010529     C                   clear                   G02ERR
166100010529     C                   call      'XSRDA8'
166200010529     C                   parm                    WLBDAT
166300010529     C                   z-add     g02inv        ormdfo
166400010206     C                   movel     w0140         ormofo
166500010206     C                   z-add     600           ormfao
166600040909     C                   eval      ormdtt = g02inv
166700070711      * imposto ormeti
166800070711     c                   clear                   dfar
166900070711     c                   clear                   tibs02ds
167000070711     c                   eval      t02mod = 'C'
167100070711     c                   eval      t02sif = knsif
167200070711     c                   eval      t02cod = 'FAR'
167300070711     c                   movel(p)  ormfao        t02ke1
167400070711     c                   call      'TIBS02R'
167500070711     c                   parm                    kpjba
167600070711     c                   parm                    tibs02ds
167700070711     c                   eval      dfar = t02uni
167800070711     c                   eval      ormeti = d§farass
167900010206     C                   update    fnorm000
168000010206
168100040909     c                   Clear                   Fnorf000
168200010206     C                   eval      orfpoe = ormpoe
168300010206     C                   eval      orfnsr = ormnsr
168400010206     C                   eval      orfnor = ormnor
168500010206     C                   eval      orfnrv = ormnrv
168600010406     C                   eval      orfpog = ormpor
168700010206     C                   eval      orfdae = ormdfo
168800010206     C                   eval      orfore = ormofo
168900010206     C                   eval      orffar = ormfao
169000010206     C                   eval      orfcar = *blanks
169100010206     C                   eval      orfpue = KNMUS
169200060516     c                   move      v1cpor        orffgs
169300040909     c                   Eval      OrfNdc = OrmNdc
169400040909     c                   Eval      OrfDdc = OrmDdc
169500010206     C                   write     fnorf000
169600010206      * MANCATO RITIRO
169700010206     C                   else
169800010529
169900020529     c                   clear                   okbolla
170000020523      * la causale richiesta genera addebito e l'orm è in fase 400
170100020523     c                   if        d§cmradd <> *blanks
170200020523     c                   exsr      sr_contradd
170300020523      * se f12 o f03 da immissione bolla riemetto la videata
170400020523     c                   if        okbolla = '1'
170500020529     c                   eval      wnobolla = *on
170600020523     C                   goto      endsrconf
170700020523     c                   endif
170800020523     c                   endif
170900020523
171000040909      * salvo i campi della distinta da riportare sulla fase
171100040909     c                   eval      savndc = ormndc
171200040909     c                   eval      savddc = ormddc
171300040909
171400010529     C                   time                    w0140
171500010529     C                   move      w0140         wdtgio
171600010529     C                   z-add     wdtgio        G02DAT
171700010529     C                   clear                   G02ERR
171800010529     C                   call      'XSRDA8'
171900010529     C                   parm                    WLBDAT
172000010529     C                   z-add     g02inv        ormdfo
172100010206     C                   movel     w0140         ormofo
172200010301     C                   movel     d§cmrfar      ormfao
172300010206     C                   clear                   ormtap
172400010206     C                   clear                   ormnpg
172500010206     C                   clear                   ormndc
172600010206     C                   clear                   ormddc
172700040909     C                   eval      ormdtt = g02inv
172800070711      * imposto ormeti
172900070711     c                   clear                   dfar
173000070711     c                   clear                   tibs02ds
173100070711     c                   eval      t02mod = 'C'
173200070711     c                   eval      t02sif = knsif
173300070711     c                   eval      t02cod = 'FAR'
173400070711     c                   movel(p)  ormfao        t02ke1
173500070711     c                   call      'TIBS02R'
173600070711     c                   parm                    kpjba
173700070711     c                   parm                    tibs02ds
173800070711     c                   eval      dfar = t02uni
173900070711     c                   eval      ormeti = d§farass
174000070926      * se ORM ancora assegnabile pulisco la data di stampa
174100070926     c                   if        ormeti <> *blanks and *in07
174200070926     c                   clear                   ormdst
174300071011      * devo pulire anche i dati su fnorg
174400071011     c     kfnorm1       chain     fnorg01l
174500071011    3c                   if        %found(fnorg01l)
174600080314     c                   eval      dorg01 = orgflo
174700071011     c                   eval      orgndc = 0
174800071011     c                   eval      orgpdc = 0
174900071011     c                   eval      orgfgs = 0
175000071011     c                   eval      orgddc = 0
175100071011     c                   eval      orgnftl= 0
175200071011     c                   eval      orgslo = 0
175300071011     c                   eval      orgdtvdis= 0
175400071011     c                   eval      orghvdis= 0
175500071026     c                   eval      orgfao = 0
175600071026     c                   eval      orgdfo = 0
175700071026     c                   eval      orgofo = 0
175800080314      * pulisco il flag di ORM dato a PDA nel caso di mancato ritiro anche se si chiude l'ORM
175900080314     c                   clear                   §orgpda
176000080314     c                   eval      orgflo = dorg01
176100071011     c                   update    fnorg000
176200071011    3c                   endif
176300070926     c                   endif
176400010206     C                   update    fnorm000
176500010206
176600081125     c                   clear                   dorf01
176700040909     c                   Clear                   Fnorf000
176800010206     C                   eval      orfpoe = ormpoe
176900010206     C                   eval      orfnsr = ormnsr
177000010206     C                   eval      orfnor = ormnor
177100010206     C                   eval      orfnrv = ormnrv
177200010406     C                   eval      orfpog = ormpor
177300010206     C                   eval      orfdae = ormdfo
177400010206     C                   eval      orfore = ormofo
177500010206     C                   eval      orffar = ormfao
177600010206     C                   eval      orfcar = vs1cau
177700010206     C                   eval      orfpue = KNMUS
177800060516     c                   move      v1cpor        orffgs
177900040909     c                   Eval      OrfNdc = savndc
178000040909     c                   Eval      OrfDdc = savddc
178100081125      * imposto se non ho fatto l'ulteriore addebito
178200081125     c                   if        $noadd = *on
178300081125     c                   eval      §orfadd = 'S'
178400081125     c                   endif
178500081125     c                   eval      orfflo = dorf01
178600010206     C                   write     fnorf000
178700010206
178800010206     C                   endif
178900010206
179000010206     C     endsrconf     endsr
179100020523      **********************************************************************
179200020523      * Controllo se devo generare la bolla di addebito
179300020523      **********************************************************************
179400020523     c     sr_contradd   begsr
179500081125
179600081125     c                   eval      $noadd = *off
179700081125
179800081125      * controllo che l'ORM non abbia già avuto una causale che genera addebito
179900081125      * in questo caso faccio solo un addebito
180000081125     c     kfnorm1       setll     fnorf01l
180100081125     c                   do        *hival
180200081125     c     kfnorm1       reade     fnorf01l
180300081125     c                   if        %eof (fnorf01l)
180400081125     c                   leave
180500081125     c                   endif
180600081125      * senza causale rileggo
180700081125     c                   if        orfcar = *blanks
180800081125     c                   iter
180900081125     c                   endif
181000081125      * fase da PDA rileggo
181100081125     c                   if        orffar = 410
181200081125     c                   iter
181300081125     c                   endif
181400081125     c                   clear                   a_dcmr
181500081125     c                   clear                   tibs02ds
181600081125     c                   eval      t02mod = 'C'
181700081125     c                   eval      t02sif = knsif
181800081125     c                   eval      t02cod = 'CMR'
181900081125     c                   eval      t02ke1 = orfcar
182000081125     c                   call      'TIBS02R'
182100081125     c                   parm                    kpjba
182200081125     c                   parm                    tibs02ds
182300081125     c                   eval      a_dcmr = t02uni
182400081125      * se causale che genera bolla vado avanti senza generare l'addebito per la seconda volta
182500081125     c                   if        a_d§cmradd = 'S'
182600081125     c                   eval      $noadd = *on
182700081125     c                   leavesr
182800081125     c                   endif
182900081125     c                   enddo
183000020523
183100020523     c                   clear                   sav_lpksc
183200020523
183300020523      * se cod.ordinante codificato addebito
183400020523 b1  c                   if        ormcor <> *zeros and ormcor2 <> 8888
183500020523     c                             and ormcor2 <> 9999
183600020523     c                   clear                   dblp
183700090505     c                   movel     ormcor        §lpksca
183800020702      * se c'è imposto il codice bollettazione
183900020702     c     ormcor        chain     fnacr01l
184000020702     c                   if        %found(fnacr01l) and acratb = *blanks
184100020702     c                             and acrksc <> *zeros
184200090505     c                   move      acrksc        §lpksca
184300090505     c                   if        acrccc<>999
184400090505     c                   move      acrccc        §lpctra
184500020702     c                   endif
184600090505     c                   endif
184700020523     c                   eval      §lprsd = ormrso
184800020523     c                   eval      §lpind = ormino
184900020523     c                   eval      §lpcad = ormcao
185000020523     c                   eval      §lplod = ormloo
185100020523     c                   eval      §lpprd = ormpro
185200020523     c                   eval      §lpnzd = ormnao
185300020523     c                   exsr      sr_wrtbolla
185400020523 x1  c                   else
185500020523      * se non inserito l'ordinante
185600020523 b2  c                   if        ormrso = *blanks
185700020523      * orm non commissionato
185800020523 b3  c                   if        ormpoe = ormpor
185900020523      * se cod.mittente codificato addebito
186000020523 b4  c                   if        ormcra <> *zeros and ormcra2 <> 8888
186100020523     c                             and ormcra2 <> 9999
186200020523     c                   clear                   dblp
186300090505     c                   movel     ormcra        §lpksca
186400020702      * se c'è imposto il codice bollettazione
186500020702     c     ormcra        chain     fnacr01l
186600020702     c                   if        %found(fnacr01l) and acratb = *blanks
186700020702     c                             and acrksc <> *zeros
186800090505     c                   move      acrksc        §lpksca
186900090505     c                   if        acrccc<>999
187000090505     c                   move      acrccc        §lpctra
187100020702     c                   endif
187200090505     c                   endif
187300020523     c                   eval      §lprsd = ormrsr
187400020523     c                   eval      §lpind = orminr
187500020523     c                   eval      §lpcad = ormcar
187600020523     c                   eval      §lplod = ormlor
187700020523     c                   eval      §lpprd = ormprr
187800020523     c                   eval      §lpnzd = ormnar
187900020523     c                   exsr      sr_wrtbolla
188000020523      * se cod.mittente non codificato
188100020523 x4  c                   else
188200020523      * se cod.destinatario codificato addebito
188300020523 b5  c                   if        ormcrc <> *zeros and ormcrc2 <> 8888
188400020523     c                             and ormcrc2 <> 9999
188500020523     c                   clear                   dblp
188600090505     c                   movel     ormcrc        §lpksca
188700020702      * se c'è imposto il codice bollettazione
188800020702     c     ormcrc        chain     fnacr01l
188900020702     c                   if        %found(fnacr01l) and acratb = *blanks
189000020702     c                             and acrksc <> *zeros
189100090505     c                   move      acrksc        §lpksca
189200090505     c                   if        acrccc<>999
189300090505     c                   move      acrccc        §lpctra
189400020702     c                   endif
189500090505     c                   endif
189600020523     c                   eval      §lprsd = ormrsc
189700020523     c                   eval      §lpind = orminc
189800020523     c                   eval      §lpcad = ormcac
189900020523     c                   eval      §lplod = ormloc
190000020523     c                   eval      §lpprd = ormprc
190100020523     c                   eval      §lpnzd = ormnac
190200020523     c                   exsr      sr_wrtbolla
190300020523 e5  c                   endif
190400020523 e4  c                   endif
190500020523      * orm commissionato
190600020523 x3  c                   else
190700020523      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
190800020523 b4  c                   if        ormcrc <> *zeros and ormcrc2 <> 8888
190900020523     c                             and ormcrc2 <> 9999  and ormcrc1 = ormpoe
191000020523     c                   clear                   dblp
191100090505     c                   movel     ormcrc        §lpksca
191200020702      * se c'è imposto il codice bollettazione
191300020702     c     ormcrc        chain     fnacr01l
191400020702     c                   if        %found(fnacr01l) and acratb = *blanks
191500020702     c                             and acrksc <> *zeros
191600090505     c                   move      acrksc        §lpksca
191700090505     c                   if        acrccc<>999
191800090505     c                   move      acrccc        §lpctra
191900020702     c                   endif
192000090505     c                   endif
192100020523     c                   eval      §lprsd = ormrsc
192200020523     c                   eval      §lpind = orminc
192300020523     c                   eval      §lpcad = ormcac
192400020523     c                   eval      §lplod = ormloc
192500020523     c                   eval      §lpprd = ormprc
192600020523     c                   eval      §lpnzd = ormnac
192700020523     c                   exsr      sr_wrtbolla
192800020523      * se non codificato o p.o. differenti
192900020523 x4  c                   else
193000020523      * se cod.mittente codificato addebito
193100020523 b5  c                   if        ormcra <> *zeros and ormcra2 <> 8888
193200020523     c                             and ormcra2 <> 9999
193300020523     c                   clear                   dblp
193400090505     c                   movel     ormcra        §lpksca
193500020702      * se c'è imposto il codice bollettazione
193600020702     c     ormcra        chain     fnacr01l
193700020702     c                   if        %found(fnacr01l) and acratb = *blanks
193800020702     c                             and acrksc <> *zeros
193900090505     c                   move      acrksc        §lpksca
194000090505     c                   if        acrccc<>999
194100090505     c                   move      acrccc        §lpctra
194200020702     c                   endif
194300090505     c                   endif
194400020523     c                   eval      §lprsd = ormrsr
194500020523     c                   eval      §lpind = orminr
194600020523     c                   eval      §lpcad = ormcar
194700020523     c                   eval      §lplod = ormlor
194800020523     c                   eval      §lpprd = ormprr
194900020523     c                   eval      §lpnzd = ormnar
195000020523     c                   exsr      sr_wrtbolla
195100020523 e5  c                   endif
195200020523 e4  c                   endif
195300020523 e3  c                   endif
195400020523 e2  c                   endif
195500020523 e1  c                   endif
195600020523
195700020523     c                   endsr
195800020523      **********************************************************************
195900020523      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
196000020523      **********************************************************************
196100020523     c     sr_wrtbolla   begsr
196200020523
196300020523      * richiamo il programma per la scelta delle stampanti
196400020523     c                   exsr      sr_scestp
196500020524     c   92              eval      okbolla = '1'
196600020523     c   92              goto      endwrtbolla
196700020523
196800020523     c                   clear                   kpjbu
196900020523     c                   clear                   dtasv
197000020523
197100020523     c                   eval      §lpfpr = 'T'
197200020523     c                   movel     ormpor        §lpccn
197300020523     c                   move      9999          §lpccn
197400020523     c                   movel     dateu         §lpaas
197500020523     c                   move      dateu         §lpmgs
197600020523     c                   eval      §lplnp = ormpor
197700020523     c                   eval      §lpcbo = 'FY'
197800020523     c                   eval      §lpsop = '+'
197900020530     c                   eval      §lplna = ormpoe
198000020523     c                   eval      §lprsm = ormrsr
198100020523     c                   eval      §lpinm = orminr
198200020523     c                   eval      §lpcam = ormcar
198300020523     c                   eval      §lplom = ormlor
198400020523     c                   eval      §lpprm = ormprr
198500020523     c                   eval      §lpnzm = ormnar
198600020523
198700020523 b1  c                   if        ormncl <> 0
198800020523     c                   eval      §lpncl = ormncl
198900020523 x1  c                   else
199000020523     c                   eval      §lpncl = 1
199100020523 e1  c                   endif
199200020523 b1  c                   if        ormpkg <> 0
199300020523     c                   eval      §lppkb = ormpkg
199400020523 x1  c                   else
199500020523 b2  c                   if        ormvlm <> 0
199600020523     c     ormvlm        mult      200           §lppkb
199700020523 x2  c                   else
199800020523 b3  c                   if        ormbnc <> 0
199900020523     c     ormbnc        mult      200           §lppkb
200000020523 e3  c                   endif
200100020523 e2  c                   endif
200200020523 e1  c                   endif
200300020523
200400020523     c                   clear                   ds_keyorm
200500020523     c                   eval      ds_poe = ORMpoe
200600020523     c                   eval      ds_nsr = ORMnsr
200700020523     c                   eval      ds_nor = ORMnor
200800020523     c                   eval      ds_nrv = ORMnrv
200900041201     c                   move      ds_keyorm     §lprmn
201000020523
201100020523     c                   eval      §lprma = ormrfa
201200020523     c                   eval      §lprmo = vs1cau + '-' + d§cmrdes1
201300020523     c                   eval      §lpctm = '4F'
201400020523     c                   movel     dateu         §lpdrt
201500140213     c                   eval      §lppdr = vs1pdr
201600140213     c                   eval      §lpfpp = vs1fpp
201700020523
201800020523
201900020523     C                   call      'FNLS01R'
202000020523     C                   parm                    kpjba
202100020523     C                   parm                    dblp
202200020523     C                   parm                    dtasv
202300030623     c                   Parm                    Trul90ds
202400020523
202500020523     C                   if        §lpfpr = '4' or §lpfpr = '3'
202600020523     C                             or §lpfpr = '5'
202700020523     C                   eval      okbolla = '1'
202800020523     C                   endif
202900020523
203000020523     c     endwrtbolla   endsr
203100020523      **********************************************************************
203200020523      * scelta stampante per immissione bolla
203300020523      **********************************************************************
203400020523     c     sr_scestp     begsr
203500020523
203600020523     C                   clear                   kpjbu
203700020523     c                   setoff                                       92
203800020523
203900020523      * Richiesta stampanti
204000020523     C                   if        conta = *zeros
204100030623     C                   clear                   trul90ds
204200030623     C                   movel     'S'           D90rse
204300030623     C                   movel     'S'           D90rsb
204400030623     C                   call      'TRUL90R'
204500020523     C                   parm                    kpjba
204600030623     C                   Parm                    trul90ds
204700020523      * F3=Fine
204800030623     C                   if        D90f3 = '1'
204900020523     C                   eval      *in92 = *on
205000020523     C                   goto      endstp
205100020523     C                   endif
205200030307
205300030307     C                   z-add     103           lung
205400020523
205500020523      * Ovrprtf Segnacolli
205600030623     C                   movea     D90pre        cm3(28)
205700030623     C                   movea     D90mde        cm3(50)
205800020523     C                   clear                   comman
205900020523     C                   movea     cm3           comman
206000020523     C                   CALL      'QCMDEXC'                            92
206100020523     C                   PARM                    comman
206200020523     C                   PARM                    lung
206300020523     C   92              goto      endstp
206400030623
206500030623      * Bolle - Nuovo modulo
206600030623
206700030623     c                   Z-Add     110           lung
206800030623
206900030623      * Ovrprtf Bolle A4
207000030623     c                   Movea     D90prb4       cma4(30)
207100030623     c                   Movea     D90mdb4       cma4(52)
207200030623     c                   Clear                   Comman
207300030623     c                   Movea     cmA4          Comman
207400030623     c                   Call      'QCMDEXC'                            92
207500030623     c                   Parm                    Comman
207600030623     c                   Parm                    Lung
207700030623     c   92              goto      endstp
207800030623
207900030623      * Ovrprtf Bolle A5
208000030623     c                   Movea     D90prb5       cmA5(30)
208100030623     c                   Movea     D90mdb5       cmA5(52)
208200030623     c                   Clear                   Comman
208300030623     c                   Movea     cmA5          Comman
208400030623     c                   Call      'QCMDEXC'                            92
208500030623     c                   Parm                    Comman
208600030623     c                   Parm                    Lung
208700030623     c   92              goto      endstp
208800020523
208900020523     C                   eval      conta = conta +1
209000020523     C                   endif
209100020523
209200020523     c     endstp        endsr
209300040406      *-------------------------------------------------------------------------------*
209400040406      * Controllo se devo chiudere l'ORM in automatico in base i tentativi fatti
209500040406      *-------------------------------------------------------------------------------*
209600040406     c     Sr_ContrFase  BegSr
209700040406
209800040406     c                   Eval      wchiudi = *Off
209900040406     c                   Clear                   wconta
210000081007     c                   clear                   Sav_OrfFar
210100040406      * Mi posiziono sull'ultima fase dell'ORM
210200040406     c     kOrf          Setll     Fnorf01l
210300040406     c                   Do        *Hival
210400040406      * Torno indietro a leggere
210500040406     c     kFnorm1       Readpe    Fnorf01l
210600040406      * Fine file
210700040406     c                   If        %Eof(Fnorf01l)
210800040406     c                   Leave
210900040406     c                   EndIf
211000040406      * se stessa fase torno a leggere
211100040406     c                   If        OrfFar = Sav_OrfFar
211200040406     c                   Iter
211300040406     c                   EndIf
211400040406     c                   Eval      sav_OrfFar = OrfFar
211500040406      * fase senza causale
211600040406     c                   If        OrfCar = *Blanks
211700040406     c                   Iter
211800040406     c                   EndIf
211900040406      * controllo la causale della fase
212000040406     c                   Clear                   dCmr
212100040406     c                   Clear                   Tibs02ds
212200040406     c                   Eval      T02Mod = 'C'
212300040406     c                   Eval      T02Sif = knsif
212400040406     c                   Eval      T02Cod = 'CMR'
212500040406     c                   Eval      T02Ke1 = OrfCar
212600040406     c                   Call      'TIBS02R'
212700040406     c                   Parm                    kpjba
212800040406     c                   Parm                    Tibs02ds
212900040406     c                   Eval      dCmr = T02Uni
213000040413      * se non è da  conteggiare torno a leggere
213100040413     c                   If        d§cmrccc = *Blanks
213200040406     c                   Iter
213300040406     c                   EndIf
213400040406
213500040406      * Conto
213600040406     c                   add       1             wconta
213700040406      * se raggiunto il max dei tentativi chiudo l'ORM
213800040406     c                   If        wconta >= wtenta
213900040406     c                   Eval      wchiudi = *On
214000040406     c                   Leave
214100040406     c                   EndIf
214200040406     c                   EndDo
214300040406
214400040406     c                   EndSr
214500040406      *-------------------------------------------------------------------------------*
214600040406      * Chiamo la manutenzione ORM x proporre la chiusura
214700040406      *-------------------------------------------------------------------------------*
214800040406     c     Sr_Chiudi     BegSr
214900040406
215000040406     c                   Clear                   Fior05ds
215100040406     c                   Eval      §rmtla = 'C'
215200040406     c                   Eval      §rmfpr = 'P'
215300040406     c                   Eval      §rmpoe = OrmPoe
215400040406     c                   Eval      §rmnsr = OrmNsr
215500040406     c                   Eval      §rmnor = OrmNor
215600040406     c                   Eval      §rmnrv = OrmNrv
215700040406     c                   Eval      §rmtor = OrmTor
215800040406     c                   Eval      §rmcor = OrmCor
215900040406     c                   Eval      §rmrso = OrmRso
216000040406     c                   Eval      §rmino = OrmIno
216100040406     c                   Eval      §rmcao = OrmCao
216200040406     c                   Eval      §rmloo = OrmLoo
216300040406     c                   Eval      §rmpro = OrmPro
216400040406     c                   Eval      §rmnao = OrmNao
216500040406     c                   Eval      §rmcra = OrmCra
216600040406     c                   Eval      §rmrsr = OrmRsr
216700040406     c                   Eval      §rminr = OrmInr
216800040406     c                   Eval      §rmcar = OrmCar
216900040406     c                   Eval      §rmlor = OrmLor
217000040406     c                   Eval      §rmprr = OrmPrr
217100040406     c                   Eval      §rmnar = OrmNar
217200040406     c                   Eval      §rmrer = OrmRer
217300040406     c                   Eval      §rmter = OrmTer
217400040406     c                   Eval      §rmcrc = OrmCrc
217500040406     c                   Eval      §rmrsc = OrmRsc
217600040406     c                   Eval      §rminc = OrmInc
217700040406     c                   Eval      §rmcac = OrmCac
217800040406     c                   Eval      §rmloc = OrmLoc
217900040406     c                   Eval      §rmprc = OrmPrc
218000040406     c                   Eval      §rmnac = OrmNac
218100040406     c                   Eval      §rmffd = OrmFfd
218200040406     c                   Eval      §rmdar = OrmDar
218300040406     c                   Eval      §rmorr = OrmOrr
218400040406     c                   Eval      §rmnam = OrmNam
218500040406     c                   Eval      §rmncl = OrmNcl
218600040406     c                   Eval      §rmpkg = OrmPkg
218700040406     c                   Eval      §rmvlm = OrmVlm
218800040406     c                   Eval      §rmbnc = OrmBnc
218900040406     c                   Eval      §rmblc = OrmBlc
219000040406     c                   Eval      §rmatt = OrmAtt
219100040406     c                   Eval      §rmmtc = OrmMtc
219200040406     c                   Eval      §rmpag = OrmPag
219300040406     c                   Eval      §rmksc = OrmKsc
219400040406     c                   Eval      §rmctr = OrmCtr
219500040406     c                   Eval      §rmpor = OrmPor
219600040406     c                   Eval      §rmzor = OrmZor
219700040406     c                   Eval      §rmpoc = OrmPoc
219800040406     c                   Eval      §rmno1 = OrmNo1
219900040406     c                   Eval      §rmno2 = OrmNo2
220000040406     c                   Eval      §rmcst = OrmCst
220100040406     c                   Eval      §rmvcs = OrmVcs
220200040406     c                   Eval      §rmfcs = OrmFcs
220300040406     c                   Eval      §rmddt = OrmDdt
220400040406     c                   Eval      §rmsto = OrmSto
220500040406     c                   Eval      §rmrfa = OrmRfa
220600040406     c                   Eval      §rmspi = OrmSpi
220700040406     c                   Eval      §rmcau = Sav_CmrCau
220800040406     c                   Eval      §rmflo = OrmFlo
220900040406     c                   Eval      §rmflv = 'B'
221000040406      * Passo alcuni parametri anche nella parm01
221100040406     c                   Clear                   parm01
221200040406     c                   eval      pmio = 'S'
221300041014     c                   Eval      pndc = savndc
221400041014     c                   Eval      pddc = savddc
221500120613     c                   eval      pdstfgs = v1cpor
221600040406     c                   Eval      kpjbu = parm01
221700040406
221800040406     c                   Call      'FIOR05R'
221900040406     c                   Parm                    kpjba
222000040406     c                   Parm                    Fior05ds
222100040406
222200040406     c                   EndSr
222300041012      *-------------------------------------------------------------------------------*
222400041012      * Richiamo Fnlrf8r x gestione rientri
222500041012      *-------------------------------------------------------------------------------*
222600041012     c     Sr_Rientri    BegSr
222700041012      * Richiamo Fnlrf8r x gestione rientri
222800041012      * se presente la distinta nel file dei rientri
222900120613     c                   eval      kdstfgs = v1cpor
223000070207     c                   move      skndc(yy)     kqdtnfv
223100080424     c                   move      skddc(yy)     kqdtddc
223200080424     c                   move      skpdr(yy)     kqdtpdr
223300041012     c     kQdt01        setll     Fiqdt01l
223400041012     c                   If        %Equal(Fiqdt01l)
223500120613     c                   Z-Add     kdstfgs       karbifp
223600070207     c                   move      skndc(yy)     karbndc
223700050225     c     karb78        setll     fnarb78l
223800050225     c                   If        Not %Equal(Fnarb78l)
223900041012     c                   clear                   fnlrf8ds
224000041012     c                   z-add     kqdtnfv       rf8dis
224100120613     c                   z-add     kdstfgs       rf8fgs
224200041012     c                   movel(p)  fnlrf8ds      kpjbu
224300041012     c                   call      'FNLRF8R'
224400041012     c                   parm                    kpjba
224500050225     c                   endif
224600041012     c                   endif
224700041012
224800041012     c                   EndSr
224900080314
225000080314      *--------------------------------------------------------------------*
225100080319      * Annullo i dati su FIPDO
225200080314      *--------------------------------------------------------------------*
225300080314     c     sr_aggpdo     begsr
225400080314
225500080314     c                   eval      or56fgs = v1cpor
225600080404     c                   eval      or56ddc = skddc(yy)
225700080404     c                   eval      or56ndcd = skndc(yy)
225800080404     c                   eval      or56ndca = skndc(yy)
225900080314     c                   call      'FIOR56R'
226000080314     c                   parm                    kpjbu
226100080314     c                   parm                    fior56ds
226200080314      * il pgm torna degli errori, ma dato che ho già fatto qua tutti i controlli
226300080314      * del caso, non me ne preoccupo
226400080314
226500080314     c                   endsr
226600080415
226700080415      *-------------------------------------------------------------------------------*
226800080415      * Controllo se esiste la fase RCH sulla distinta
226900080415      *-------------------------------------------------------------------------------*
227000080415     c     sr_ctrfase    begsr
227100080415
227200080415     c                   clear                   fior90ds
227300080415     c                   eval      or90tla = 'C'
227400090402     c                   eval      or90fas = wfase
227500080415     c                   eval      or90fgs = v1cpor
227600080415     c                   eval      or90ndc = wndc
227700080415     c                   call      'FIOR90R'
227800080415     c                   parm                    kpjba
227900080415     c                   parm                    fior90ds
228000080415
228100080415     c                   endsr
228200080314
228300080327      *-------------------------------------------------------------------------------*
228400080415      * Scrivo la fase RQE sulla distinta
228500080327      *-------------------------------------------------------------------------------*
228600080327     c     sr_fase       begsr
228700080327
228800080415     c                   clear                   fior90ds
228900080415     c                   eval      or90tla = 'W'
229000080415     c                   eval      or90fas = 'RQE'
229100080327     c                   eval      or90fgs = v1cpor
229200080327     c                   eval      or90ndc = skndc(yy)
229300080327     c                   eval      or90flgins = 'A'
229400080327     c                   call      'FIOR90R'
229500080327     c                   parm                    kpjba
229600080327     c                   parm                    fior90ds
229700080327      * non controllo l'errore di ritorno tanto sono alla fine del PGM
229800080327      * al massimo non ho scritto la fase
229900080327
230000080327     c                   endsr
230100080526
230200080526      *------------------------------------------------------------------------*
230300080526      * ROUTINE PER INVIARE MAIL SE ERRORE CODICE SPC
230400080526      *------------------------------------------------------------------------*
230500080526     c     sr_mail       begsr
230600080526
230700080526      * Reperimento tab. "MRA" se non già recuperata
230800080526     c                   if        not $okmra
230900080526     c                   clear                   dmradan
231000080526     c                   clear                   tibs02ds
231100080526     c                   eval      t02mod = 'C'
231200080526     c                   eval      t02sif = knsif
231300080526     c                   eval      t02cod = 'MRA'
231400080526     c                   eval      t02ke1 = 'FIOR90R'
231500080526     c                   call      'TIBS02R'
231600080526     c                   parm                    kpjba
231700080526     c                   parm                    tibs02ds
231800080526     c                   if        t02err = *blanks
231900080526     c                   eval      dmradan = t02uni
232000080526     c                   eval      $okmra = *on
232100080526     c                   endif
232200080526     c                   endif
232300080526
232400080526      * Override al file di stampa ed apertura dello stesso
232500080526     c                   if        not %open(prtemail)
232600080526     c                   exsr      sr_openprtf
232700080526     c                   endif
232800080526
232900080526     c                   eval      o_testo = 'Fase RQE non scritta per la +
233000080526     c                                        distinta ' +
233100080526     c                                        %editc(v1cpor:'X') + '-' +
233200080526     c                                        %editc(skndc(yy):'X') +
233300080526     c                                        ' Fior26r'
233400080526     c                   except    prtdet
233500080526
233600080526     c                   endsr
233700080526
233800080526      *------------------------------------------------------------------------*
233900080526      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
234000080526      *  + STAMPA INIZIO MAIL
234100080526      *------------------------------------------------------------------------*
234200080526     c     sr_openprtf   begsr
234300080526
234400080526      * Override al file di stampa
234500080526     c                   exsr      sr_override
234600080526
234700080526     c                   open      prtemail
234800080526
234900080526      * Stampa una testata se NON è richiesta la e-mail
235000080526     c                   if        §mradreg = *blanks
235100080526     c                   eval      o_testo = knmus + ' - ' + 'FIOR90R'
235200080526     c                             + ' - ' + %editc( *date : 'Y' )
235300080526     c                             + ' - *REM* ' + %subst(§cm1var : 7 : 70)
235400080526     c                   except    prtdet
235500080526     c                   clear                   o_testo
235600080526     c                   except    prtdet
235700080526     c                   except    prtdet
235800080526     c                   endif
235900080526
236000080526      * Stampa testo iniziale
236100080526     c                   eval      o_testo = 'Fase RQE non scritta'
236200080526     c                   except    prtdet
236300080526
236400080526      * Stampa una riga vuota
236500080526     c                   clear                   o_testo
236600080526     c                   except    prtdet
236700080526
236800080526     c                   endsr
236900080526
237000080526      *------------------------------------------------------------------------*
237100080526      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
237200080526      *  + STAMPA INIZIO MAIL
237300080526      *------------------------------------------------------------------------*
237400080526     c     sr_override   begsr
237500080526
237600080526     c                   reset                   $invio
237700080526     c                   reset                   trtcm1ds
237800080526
237900080526     c                   if        §mradreg <> *blanks
238000080526     c                   eval      §cm1mitt = %trim(§mradmitt)
238100080526     c                   eval      §cm1dst  = %trim(§mraddest)
238200080526     c                   eval      §cm1tips = §mradreg
238300080526     c                   eval      §cm1po   = c_sede
238400080526     c                   eval      §cm1var  = '*OBJM*' + §mraddes
238500080526     c                   eval      §cm1idp  = §mradidpro
238600080526     c                   eval      qcmd = c_cmdovrprtf
238700080526     c                             + ' outq(' + %trim(§mradoutqi) + ')'
238800080526     c                             + ' usrdfndta(''' + trtcm1ds + ''')'
238900080526     c                   else
239000080526     c                   eval      qcmd = c_cmdovrprtf
239100080526     c                   endif
239200080526
239300080526     c                   eval      lenght = %size(qcmd)
239400080526     c                   call(e)   'QCMDEXC'
239500080526     c                   parm                    qcmd
239600080526     c                   parm                    lenght
239700080526
239800080526     c                   if        §mradreg <> *blanks and not %error
239900080526     c                   eval      $invio = *on
240000080526     c                   endif
240100080526
240200080526     c                   endsr
240300080526
240400080526      *------------------------------------------------------------------------*
240500080526      * ROUTINE FINALE
240600080526      *------------------------------------------------------------------------*
240700080526     c     sr_end        begsr
240800080526
240900080526     c                   if        %open(prtemail)
241000080526
241100080526      * Chiusura dello spool?
241200080526     c                   clear                   o_testo
241300080526     c                   except    prtdet
241400080526     c                   eval      o_testo = '***   Fine Lista   ***'
241500080526     c                   except    prtdet
241600080526
241700080526     c                   close     prtemail
241800080526
241900080526      * Eliminazione overflow?
242000080526     c                   eval      lenght = %size(qcmd)
242100080526     c                   eval      qcmd = c_cmddltovr
242200080526     c                   call(e)   'QCMDEXC'
242300080526     c                   parm                    qcmd
242400080526     c                   parm                    lenght
242500080526
242600080526     c                   endif
242700080526
242800080526     c                   endsr
242900080526
243000070207
243100010206      *****************************************************************
243200010206      * ROUTINE INIZIALE
243300010206      *****************************************************************
243400010206     C     *INZSR        BEGSR
243500010206
243600010206     C     *ENTRY        PLIST
243700010206     C                   PARM                    KPJBA
243800010206
243900010206     C                   eval      vtctit = '*  Quadratura Ordini '
244000010206     C                             + 'Ritiri Merce  *'
244100010206
244200080723     c     *dtaara       define    §azute        azuteds
244300080723     c     *dtaara       define    §datiute      ddatiute
244400080723     c                   in(e)     *dtaara
244500080723     c                   if        %error or rsut = *blanks
244600080723     c                   clear                   tibs34ds
244700080723     c                   call      'TIBS34R'
244800080723     c                   parm                    tibs34ds
244900080723     c                   in        *dtaara
245000080723     c                   endif
245100130620
245200130620      * controllo abilitazione utente
245300130620     c                   clear                   wabi
245400130620     c                   clear                   dLAT
245500130620     c                   eval      dUTE01 = UTEfaf
245600130620
245700130620      * Verifica errori e autorità profilo
245800130620     c                   SELECT
245900130620      * se ho errori nei dati utente esco dal pgm
246000130620     c                   WHEN      DUTerr = 'E'
246100130620     c                   eval      wErrGrave = *on
246200130620     c                   leavesr
246300130620      * carica abilitazioni del profilo
246400130620     c                   WHEN      §UTEorm <> *blanks
246500130620     c                   eval      wabi = §UTEorm
246600130620     c                   WHEN      UTEaut <> *blanks
246700130620     c                   eval      wabi = UTEaut
246800130620      * se non c'è l'abilitazione
246900130620      * --> se 1° livello, abilitazioni al terminal
247000130620      *     se 2° livello, abilitazioni al punto operativo
247100130620      *     se sede no abilitazioni
247200130620     c                   OTHER
247300130620     c                   IF        DUTlpo = '1'
247400130620     c                   eval      wabi   = 'TP'
247500130620     c                   ENDIF
247600130620     c                   IF        DUTlpo = '2'
247700130620     c                   eval      wabi   = 'PO'
247800130620     c                   ENDIF
247900130620     c                   ENDSL
248000130620
248100130620      * controllo se ok l'abilitazione dell'utente
248200130620     c                   clear                   TIBS02DS
248300130620     c                   eval      T02mod = 'C'
248400130620     c                   eval      T02sif = knsif
248500130620     c                   eval      T02cod = 'LAT'
248600130620     c                   eval      T02ke1 = wabi
248700130620     c                   Call      'TIBS02R'
248800130620     c                   Parm                    KPJBA
248900130620     c                   Parm                    TIBS02DS
249000130620     c                   IF        T02err <> *blanks or §LATabi = 'S'
249100130620     c                   eval      wErrGrave = *on
249200130620     c                   leavesr
249300130620     c                   ENDIF
249400130620     c                   eval      dLAT = T02uni
249500130620
249600130620      * Reperimento delle filiali gestibili dall'utente
249700130620     c                   clear                   TRUL31DS
249800130620     c                   eval      I31abi = wabi
249900130620     c                   eval      I31cdi = DUTdis
250000130620     c                   eval      I31car = DUTare
250100130620     c                   eval      I31cpo = DUTpou
250200130620     c                   call      'TRUL31R'
250300130620     c                   parm                    KPJBA
250400130620     c                   parm                    TRUL31DS
250500130620     c                   IF        O31pog > *zeros
250600130620     c                   movea     O31pog        SKpog
250700130620     c                   ELSE
250800130620     c                   eval      wErrGrave = *on
250900130620     c                   leavesr
251000130620     c                   ENDIF
251100010206
251200010206      * IMPOSTO IL P.O. DI GESTIONE
251300130620     C*****              if        dutlpo = '2' or dutlpo = *blanks
251400080723     C                   movel     dutpou        V1Cpor
251500130620     C*****              eval      *in06 = *on
251600130620     C*****              else
251700130620     C*****              movel     SIMFEL        V1Cpor
251800130620     C*****              endif
251900010206      * Carico schiera P.O. gestiti
252000130620     C*****              CLEAR                   TRUL06ds
252100130620     C*****              MOVE      '£1'          D06COD
252200130620     C*****              MOVEL     SIMFEL        D06KEY
252300130620     C*****              MOVEL     TRUL06DS      KPJBU
252400130620     C*****              CALL      'TRUL06R'
252500130620     C*****              PARM                    KPJBA
252600130620     C*****              MOVEL     KPJBU         TRUL06DS
252700130620     C*****              MOVEA     LIN           L1
252800010206      * Carico £6
252900010206     C                   CLEAR                   trul06ds
253000010206     C                   MOVE      '£6'          D06COD
253100010206     C                   MOVEL     v1cpor        D06KEY
253200010206     C                   MOVEL     trul06ds      KPJBU
253300010206     C                   CALL      'TRUL06R'
253400010206     C                   PARM                    KPJBA
253500010206     C                   MOVEL     KPJBU         trul06ds
253600010206     C                   MOVEA     LIN           L6
253700010206
253800010206      * Decodifico P.o. gestione
253900010206     C     v1cpor        chain     azorg01L
254000010206     C                   if        %found(azorg01l)
254100010206     C                   movel     orgDES        v1dpor
254200070125     c                   eval      og148 = orgde8
254300070926     c                   eval      og147 = orgde7
254400010206     C                   else
254500010206     C                   clear                   v1dpor
254600070125     c                   clear                   og148
254700070926     c                   clear                   og147
254800010206     C                   endif
254900010206
255000010206      * reperisco data e ora
255100010206     C                   TIME                    W0140
255200010206      * UDATE IN GGMMAAAA
255300010206     C                   MOVE      W0140         WDTGIO
255400010206      * UDATE IN AAAAMMGG
255500010206     C                   Z-ADD     WDTGIO        G02DAT
255600010206     C                   MOVEL     *BLANK        G02ERR
255700010206     C                   CALL      'XSRDA8'
255800010206     C                   PARM                    WLBDAT
255900010206     C                   MOVEL     G02INV        DATEU
256000071026
256100071026      * p.o. gestione con abilitazione scarico dati per PDA
256200071026     c                   eval      *in01 = (§ogpdaorm = 'S')
256300071026      * data partenza procedura in organigramma
256400071026     c                   eval      *in07 = *off
256500071026     c                   if        §ogddao > *zeros and §ogcgio = 'S'
256600071026     c                   move      §ogddao       data_org
256700071026      * controllo se attiva
256800071026     c                   if        dateu >= data_org
256900071026     c                   eval      *in07 = *on
257000071026     c                   endif
257100071026     c                   endif
257200010206
257300120613     C     kfidst        klist
257400120613     C                   kfld                    kdstnpg
257500120613     C                   kfld                    kdstnfv
257600120613     C                   kfld                    kdstfgs
257700010206
257800010206     C     kfnorm        klist
257900060516     C                   kfld                    keypor
258000010206     C                   kfld                    keyndc
258100010206
258200010206     C     kfnorm1       klist
258300010206     C                   kfld                    vs1poe
258400010206     C                   kfld                    vs1nsr
258500010206     C                   kfld                    vs1nor
258600010206     C                   kfld                    vs1nrv
258700010208
258800060214     C     kfiar4        klist
258900060214     C                   kfld                    Kar4trc
259000060214     C                   kfld                    kar4n14
259100060214     C                   movel     'M'           kar4trc
259200021202
259300021202     C     Kfapd         klist
259400021202     C                   kfld                    kpdtip
259500021202     C                   kfld                    kpdpdr
259600040406
259700040406     c     kOrf          klist
259800040406     c                   kfld                    kOrfPoe
259900040406     c                   kfld                    kOrfNsr
260000040406     c                   kfld                    kOrfNor
260100040406     c                   kfld                    kOrfNrv
260200040406     c                   kfld                    kOrfDae
260300040406     c                   kfld                    kOrfOre
260400040406     c                   kfld                    kOrfFar
260500041012
260600041012     c     kQdt01        klist
260700120613     c                   kfld                    kdstfgs
260800041012     c                   kfld                    kqdtnfv
260900080424     c                   kfld                    kqdtddc
261000080424     c                   kfld                    kqdtpdr
261100050225
261200050225     c     Karb78        Klist
261300050225     c                   Kfld                    Karbifp
261400050225     c                   Kfld                    Karbndc
261500070206
261600070206     c     kfiplg01      klist
261700070206     c                   Kfld                    ktiporcd
261800070206     c                   kfld                    keypor
261900070206     c                   kfld                    keyndc
262000070215     c                   Kfld                    ktipappl
262100070206     c                   kfld                    kiddoc
262200010206
262300010206     C                   endsr
262400080526
262500080526     oprtemail  e            prtdet      1
262600080526     o                       o_testo
262700010206      *---------------------------------------------------------------------------------------------
262800001113** MSG
262900070418Filiale gestione errata                                                              1
263000010206Numero Distinta Errato                                                               2
263100010206Distinta non aperta                                                                  3
263200010206Immettere Numero Distinta                                                            4
263300010206Causale Errata                                                                       5
263400010206ORM allocato da altro utente                                                         6
263500010628L'ORM è stato bollettato: non si può dichiarare NON FATTO                            7
263600010305Causale non gestibile                                                                8
263700010529ORM in fase errata per dichiararlo NON FATTO                                         9
263800011015ORM già chiuso non è possibile richiuderlo                                          10
263900020524ATTENZIONE!! L'ORM è in fase 900. Non si può quadrare                               11
264000070125Effettuare un'unica scelta: o tutte le distinte o una distinta                      12
264100070206La distinta deve essere prima chiusa sul PDA, poi può essere quadrata               13
264200070207Password errata                                                                     14
264300070207Immettere la password                                                               15
264400070212Manca tabella di controllo password. TEL CED SEDE !!!!!                             16
264500070212Manca tabella di controllo scadenza password. TEL CED SEDE !!!!!                    17
264600070212Password scaduta                                                                    18
264700081030ORM Commissionato. Non si può usare più di 1 causale con resp. al cliente           19
264800090402Distinta già quadrata                                                               20
264900020523** CM3
265000030307OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
265100030307 SHARE(*YES)
265200030623** cmA4
265300030623OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
265400030623 SHARE(*YES)          USRDTA('BolAdd_A4')
265500030623**  cmA5
265600030623OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
265700030623 SHARE(*YES)          USRDTA('BolAdd_A5')
