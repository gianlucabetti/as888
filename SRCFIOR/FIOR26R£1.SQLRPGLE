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
004700010206     FFIOR26D   CF   E             WORKSTN SFILE(FIOr26s:nrr)
004800080526     fprtemail  o    f  132        printer  oflind(*inof)  usropn
004900001113
005000010206      *------------------------------------------------------------------------*
005100080526
005200080526      * dati per invio mail
005300080526     d lenght          s             15  5
005400080526     d o_testo         s            132    inz
005500080526     d qcmd            s            512    inz
005600080526     d $invio          s              1n   inz(*off)
005700080526     d $okmra          s              1n   inz(*off)
005800080526     d dmradan       e ds                  inz
005900080526     d trtcm1ds      e ds                  inz
006000080526      *    .§cm1mitt = Indirizzo e-mail del mittente
006100120302     d   §cm1mitt    e                     inz('ced@brt.it')
006200080526      *    ·§cm1dst  = Indirizzo e-mail del destinatario
006300120302     d   §cm1dst     e                     inz('monica.beghelli@brt.it')
006400080526      *    ·§cm1tips = Tipo lettera via e-mail:
006500080526      *     "LET" = testo allegato in corpo con logo
006600080526      *             (richiede righe libere iniziali per il logo)
006700080526      *     "COR" = testo integrato senza logo
006800080526      *             (non consente né UNDERLINE né HIGHLIGHT)
006900080526     d   §cm1tips    e                     inz('COR')
007000080526      *    ·§cm1po   = Filiale
007100080526     d   §cm1po      e                     inz('046')
007200080526      *    ·§cm1var  = Oggetto e-mail
007300080526     d   §cm1var     e                     inz('*OBJM*+
007400080526     d                                     Fase RQE non scritta')
007500080526      *    ·§cm1sts  = Stato
007600080526     d   §cm1sts     e                     inz(*off)
007700080526      *    ·§cm1idp  = Id processo
007800080526     d   §cm1idp     e                     inz('2')
007900080526
008000080526     d c_sede          c                   const('046')
008100080526      *  - comando di override al prtf
008200080526     d c_cmdovrprtf    c                   const('OVRPRTF +
008300080526     d                                           file(PRTEMAIL) +
008400080526     d                                           pagesize(66 132) +
008500080526     d                                           lpi(6) cpi(10) +
008600080526     d                                           ovrscope(*actgrpdfn) +
008700080526     d                                           ')
008800080526     d c_cmddltovr     c                   const('DLTOVR +
008900080526     d                                            file(PRTEMAIL) +
009000080526     d                                            lvl(*actgrpdfn)')
009100010206
009200120613     D kdstnpg         S                   LIKE(dstnpg)
009300120613     D kdstnfv         S                   LIKE(dstnfv)
009400120613     D kdstfgs         S                   LIKE(dstfgs)
009500010206
009600060516     d keypor          s                   like(ormpor)
009700010206     D keyndc          S                   LIKE(ORMndc)
009800010208
009900060214     D kar4trc         S                   LIKE(ar4trc)
010000060214     D kar4n14         S                   LIKE(ar4n14)
010100021202
010200021202     D kpdpdr          S                   LIKE(APDpdr)
010300021202     D kpdtip          S                   LIKE(APDtip)
010400041012
010500041012     d kqdtnfv         s                   like(qdtnfv)
010600080424     d kqdtddc         s                   like(qdtddc)
010700080424     d kqdtpdr         s                   like(qdtpdr)
010800070206     d ktiporcd        s                   like(plgtiporcd)
010900070215     d ktipappl        s                   like(plgtipappl)
011000010530
011100010206     D §kpjbu          S                   LIKE(kpjbu)
011200010206
011300020523     d sav_lpksc       s                   like(ormksc)
011400040406     d sav_OrfFar      s                   like(OrfFar)
011500040406     d sav_CmrCau      s                   like(OrfCar)
011600040909     d savndc          s                   like(OrmNdc)
011700040909     d savddc          s                   like(OrmDdc)
011800040406
011900040406     d kOrfPoe         s                   like(OrfPoe)
012000040406     d kOrfNsr         s                   like(OrfNsr)
012100040406     d kOrfNor         s                   like(OrfNor)
012200040406     d kOrfNrv         s                   like(OrfNrv)
012300040406     d kOrfDae         s                   like(OrfDae)
012400040406     d kOrfOre         s                   like(OrfOre)
012500040406     d kOrfFar         s                   like(OrfFar)
012600050225
012700050225     d karbifp         s                   Like(arbifp)
012800050225     d karbndc         s                   Like(arbndc)
012900020523
013000020523     d conta           s              2  0
013100070212     d dataiso         s               d   datfmt(*iso)
013200070212     d datascad        s              8  0
013300020523     d okbolla         s              1
013400070212     d savormndc       s                   like(ormndc)
013500070302     d wdelpda         s              1    inz(*off)
013600020529     d wnobolla        s              1    inz('0')
013700070302     d wokpda          s              1    inz(*off)
013800070302     d wokrch          s              1    inz(*off)
013900040406     d wtenta          s              3  0
014000040406     d wconta          s              3  0
014100040406     d wchiudi         s              1    inz('0')
014200090402     d wfase           s              3
014300070212     d wgiorni         s              3  0
014400080415     d wndc            s                   like(ormndc)
014500060516     d xx              s              3  0
014600070207     d yy              s              3  0
014700080723     d $fv6            s              6p 0 dim(600)
014800081125     d $noadd          s              1    inz('0')
014900100409     D $GIRO           S              1  0 INZ(0)
015000130620
015100130620     d wabi            s                   like(UTEaut)
015200130620     d wEsci           s               n   inz(*off)
015300130620     d wErrGrave       s               n   inz(*off)
015400130620     d w003a           s              3
015500020523
015600020523     D lung            S             15  5
015700020523     D comman          S            110
015800020523
015900010206     D flgkf           S              1
016000010206     D W0140           S             14  0
016100010206     D Wdtgio          S              8  0
016200010206     D dateu           S              8  0
016300070926     d data_org        s              8  0 inz
016400010206
016500010206      *   S C H I E R E
016600070212     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             MSG VIDEO
016700130620     D**** L1              S              3  0 DIM(30)                              P.O. Gestiti £1
016800001113     D L6              S              3  0 DIM(30)                              P.O. Gestiti £6
016900130620     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
017000030307     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
017100030623      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
017200030623     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
017300030623     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
017400071214     d skddc           s              8  0 dim(100)
017500070207     d skndc           s              6  0 dim(100)
017600080424     d skpdr           s              7  0 dim(100)
017700010119
017800010206      *   D S   I N T E R N E / E S T E R N E
017900010206
018000010206     D WLBDAT          DS                  INZ
018100010206     D  G02DAT                 1      8  0
018200010206     D  G02INV                 9     16  0
018300010206     D  G02ERR                17     17
018400010206     D  G02TGI                18     22  0
018500020523
018600020524     D                 DS
018700020523     d  ormcra1                1      3  0
018800020523     d  ormcra2                4      7  0
018900020523     d  ormcra3                8     10  0
019000020523     d  ds_ormcra              1     10  0
019100010206
019200020524     D                 DS
019300020523     d  ormcor1                1      3  0
019400020523     d  ormcor2                4      7  0
019500020523     d  ormcor3                8     10  0
019600020523     d  ds_ormcor              1     10  0
019700020523
019800020524     D                 DS
019900020523     d  ormcrc1                1      3  0
020000020523     d  ormcrc2                4      7  0
020100020523     d  ormcrc3                8     10  0
020200020523     d  ds_ormcrc              1     10  0
020300020523
020400020523     D                 DS
020500020523     D  ds_poe                 1      3  0
020600020523     D  ds_nsr                 4      5  0
020700020523     D  ds_nor                 6     12  0
020800020523     D  ds_nrv                13     14  0
020900020523     D  ds_keyorm              1     14  0
021000020523
021100001116     D Parm01          DS
021200001116     D  ppoe                          3  0
021300001116     D  pnor                          7  0
021400001116     D  pnsr                          2  0
021500001116     D  pnrv                          2  0
021600001116     D  psce                          1
021700001116     D  pfgs                          3  0
021800001116     D  ppor                          3  0
021900001116     D  pdtr                          8  0
022000001116     D  pmdb                         10
022100001116     D  pprb                         10
022200001116     D  pdts                          8  0
022300001116     D  prmp                          1
022400010130     D  pbrc                          1
022500010130     D  pref                          2
022600040406     D  pmio                          1
022700041014     d  pndc                          6  0
022800041014     d  pddc                          8  0
022900120613     d  pdstfgs                       3  0
023000010206
023100010206      * Scomposizione Descrizione Distinta
023200010206     D                 DS
023300010206     D  v1dndc                 1     40
023400010206     D  wdel                   1      4
023500010206     D  wggg                   5      6
023600010206     D  wbarra1                7      7
023700010206     D  wmmm                   8      9
023800010206     D  wbarra2               10     10
023900010206     D  waaa                  11     15
024000010206     D  wpdr                  16     23
024100010206     D  wdescr                24     40
024200070206
024300070206     d                 ds
024400070206     d kiddoc                  1     16
024500070206     d ormpoe                  1      3  0
024600070206     d ormnsr                  4      5  0
024700070206     d ormnor                  6     12  0
024800070206     d ormnrv                 13     14  0
024900070206     d wblk                   15     16    inz('  ')
025000001113
025100020523     D DTASV         E DS
025200020523     D DBLP          E DS
025300030623     D TRUL90DS      E DS
025400010301     D DCMR          E DS
025500081029     d a_dcmr        e ds                  extname(dcmr) prefix(a_)
025600010206     D FIOR06DS      E DS
025700080319     d fior06vds     e ds
025800010206     D TIBS02DS      E DS
025900010206     D TRUL06DS      E DS
026000080723     D  LIN                    1     90  0 DIM(30)
026100040406     d Fior05ds      e ds
026200080327     d fior90ds      e ds
026300041012      * DS  pgm fnlrf8r x gestione rientri
026400041012     D fnlrf8ds      E DS
026500060516     d fnlv55ds      e ds
026600070125     d og148         e ds
026700070926     d og147         e ds
026800070206     d fiprcresds    e ds
026900070207     d fior24ds      e ds
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
029000001113
029100010206      *------------------------------------------------------------------------*
029200001113
029300010206
029400010206      * Emissione 1° Videata
029500010206
029600010206     C                   do        *hival
029700010206      * Pulizia 1° Videata
029800010206     C                   if        *in28 = *off and *in90 = *off
029900010206     C                   exsr      Sr_Pulvid1
030000010206     C                   endif
030100100409
030200100409     C                   CLEAR                   $GIRO
030300130620
030400130620      /free
030500130620       //?Se utente non autorizzato alla funzione esco
030600130620         IF  wErrGrave;
030700130620           *in28 = *on;
030800130620           wEsci = *on;
030900130620           V1Cmsg = 'Utente non autorizzato alla funzione';
031000130620         ENDIF;
031100130620      /end-free
031200010206
031300010206     C                   exfmt     fior261
031400130620      /free
031500130620         IF  wEsci;
031600130620           leave;
031700130620         ENDIF;
031800130620      /end-free
031900010206
032000010206      * F3=Fine
032100010206     C   kc              leave
032200010206
032300010206     C                   setoff                                       2890
032400010206
032500010206      * F18=Cambio P.O. in gestione
032600010206     C   ks              eval      *in05 = *on
032700010206     C   ks              iter
032800010206
032900010206      * 05 on --> controllo P.O. in gestione
033000010206     C                   if        *in05 = *on
033100010206     C                   exsr      Sr_Contrpo
033200020612     C  n28              eval      *in05 = *off
033300020612     C   28              iter
033400010206     C                   endif
033500010206
033600010206      * Controlli 1° videata
033700010206     C                   exsr      Sr_Contr01
033800070208     C   28
033900070208     Cor 90              iter
034000070212
034100070302      * se p.o. abilitato al PDA, distinta abilitata allo scarico su PDA, è stata inserita
034200070302      * una distinta, ma non c'è il rcd RCH e non è stata fatta la forzatura per quadrare
034300070302      * torno al'inizio....
034400070302     c                   if        *in01 and wokpda = *on and
034500070302     c                             v1ndc <> *blanks and v1ndc <> *zeros and
034600070302     c                             wokrch = *off and wdelpda = *off
034700070212     c                   iter
034800070212     c                   endif
034900100409
035000100409     C                   DO        *HIVAL
035100010206
035200070205      * Gestione subfile di tutte le distinte
035300070205     c                   if        v1allndc = 'SI'
035400070205     c                   exsr      sr_allndc
035500070206     c                   eval      *in28 = *off
035600070205     c                   else
035700070205      * Gestione subfile x distinta
035800010206     C                   exsr      Sr_Gessfl
035900010206     C                   setoff                                       28
036000120613     c                   move      dstdfv        skddc(1)
036100070207     c                   move      v1ndc         skndc(1)
036200080424     c                   move      dstpdr        skpdr(1)
036300070205     c                   endif
036400100409
036500100415     C                   IF        ($GIRO > 0 AND NRR = 0) OR *INKL
036600100409     C                   LEAVE
036700100409     C                   ENDIF
036800100409
036900100409     C                   ENDDO
037000041013
037100070207      * per ogni distinta quadrata
037200080404    1c                   if        flgkf = '1'
037300080404    2c                   do        100           yy
037400080404    3c                   if        skndc(yy) <> *zeros
037500041013      * Richiamo Fnlrf8r x gestione rientri
037600041013     c                   ExSr      Sr_Rientri
037700070302      * p.o. gestione abilitato allo scarico PDA e distinta abilitata allo scarico su PDA
037800080404    4c                   if        *in01 and wokpda = *on
037900080424      * scrivo la fase 'RQE' sulla distinta
038000080424     c                   exsr      sr_fase
038100080424      * se ho forzato la quadratura devo
038200080424      * annullare i dati su FIPDO della distinta forzata
038300070212      * la forzatura viene fatta solo se richiesta una distinta quindi quando
038400070212      * arrivo a questo punto ho tutti i dati che mi servono della distinta forzata
038500080404    5c                   if        wdelpda = *on
038600080319      * annullo i dati inviati al PDA
038700080404     c                   clear                   fior56ds
038800080404     c                   eval      or56tla = 'Q'
038900080314     c                   exsr      sr_aggpdo
039000080404    5c                   endif
039100080327      * se filiale gestione non abilitata allo scarico PDA
039200080327      * o se distinta non abilitata allo scarico PDA
039300080415      * scrivo la fase 'RQE' sulla distinta
039400080327      * in questo modo non si potranno più assegnare ORM a distinte già quadrate anche se non
039500080327      * è previsto lo scarico a PDA
039600080404   x4c                   else
039700080519      * solo se procedura GEO ORM attiva
039800080519     c                   if        *in07
039900080415      * scrivo la fase 'RQE' sulla distinta
040000080519     c                   exsr      sr_fase
040100080404      * se la distinta è in test cancello i dati su FIPDO e FIPRO
040200080404      * imposto solo la distinta tanto gli altri dati sono già impostati da prima
040300080404     c                   eval       keyndc = skndc(yy)
040400101214     c                   eval       keypor = v1cpor
040500080404     c                   clear                   ddstflr
040600080404     c     kfidst        chain     fidst01l
040700080404    5c                   if        %found(fidst01l)
040800120613     c                   eval      ddstflr = dstflr
040900080404      * attiva su PDA
041000120613    6c                   if        (dstpda = 'O' or dstpda = 'E') and
041100080404      * in TEST
041200080404     c                             (§dsttstpda = 'O' or §dsttstpda = 'E')
041300080404     c                   clear                   fior56ds
041400080404     c                   eval      or56tla = 'D'
041500080404     c                   exsr      sr_aggpdo
041600080404    6c                   endif
041700080404    5c                   endif
041800080519     c                   endif
041900080519
042000080404    4c                   endif
042100080526
042200080526      * controllo se c'è la fase RQE
042300080526     c                   clear                   fior90ds
042400080526     c                   eval      or90tla = 'C'
042500080526     c                   eval      or90fas = 'RQE'
042600080526     c                   eval      or90fgs = v1cpor
042700080526     c                   eval      or90ndc = skndc(yy)
042800080526     c                   call      'FIOR90R'
042900080526     c                   parm                    kpjba
043000080526     c                   parm                    fior90ds
043100080526      * manca la fase mando mail
043200080526     c                   if        or90ndcok = *blanks and *in07
043300080526     c                   exsr      sr_mail
043400080526     c                   endif
043500080526
043600080404    3c                   endif
043700080404    2c                   enddo
043800080404    1c                   endif
043900010206
044000010206     C                   enddo
044100080526
044200080526     c                   exsr      sr_end
044300010206
044400010206     C                   clear                   fior06ds
044500010206     C                   movel     'C'           i06tla
044600010206     C                   call      'FIOR06R'
044700010206     C                   parm                    kpjba
044800010206     C                   parm                    fior06ds
044900010206
045000010206     C                   eval      *inlr = *on
045100010206      *****************************************************************
045200010206      * PULIZIA 1° VIDEATA
045300010206      *****************************************************************
045400010206     C     Sr_Pulvid1    BEGSR
045500010206
045600010206     C                   clear                   v1ndc
045700010206     C                   clear                   v1dndc
045800070205     c                   clear                   v1allndc
045900071214     c                   clear                   skddc
046000070207     c                   clear                   skndc
046100080424     c                   clear                   skpdr
046200070302     c                   eval      wdelpda = *off
046300070302     c                   eval      wokrch = *off
046400070302     c                   eval      wokpda = *off
046500010206
046600010206     C                   endsr
046700010206      **********************************************************************
046800010206      * CONTROLLO IL P.O. DI RITIRO
046900010206      **********************************************************************
047000010206     C     Sr_Contrpo    begsr
047100010206
047200010206     C                   setoff                                       4041
047300010206     C                   clear                   v1dpor
047400070125     c                   clear                   og148
047500070926     c                   clear                   og147
047600010206
047700130620     C**** v1cpor        lookup    L1                                     30
047800130620     c                   move      V1Cpor        w003a
047900130620     c     w003a         lookup    SKpog                                  30
048000010206     C     *in30         ifeq      *off
048100010206     C                   seton                                        40  28
048200010206     C                   movel     msg(1)        v1cmsg
048300010206     C                   else
048400010206
048500010206      * Decodifica
048600010206     C     v1cpor        chain     azorg01L
048700010206     C                   if        %found(azorg01l) and ORGfva = *blanks
048800010206     C                   movel     orgDES        v1dpor
048900070125     c                   eval      og148 = orgde8
049000070926     c                   eval      og147 = orgde7
049100010206     C                   else
049200010206     C                   seton                                        40  28
049300010206     C                   movel     msg(1)        v1cmsg
049400010206     C                   endif
049500010207
049600010207      * Devo ricaricare la £6
049700010207     C                   CLEAR                   trul06ds
049800010207     C                   MOVE      '£6'          D06COD
049900010207     C                   MOVEL     v1cpor        D06KEY
050000010207     C                   MOVEL     trul06ds      KPJBU
050100010207     C                   CALL      'TRUL06R'
050200010207     C                   PARM                    KPJBA
050300010207     C                   MOVEL     KPJBU         trul06ds
050400010207     C                   MOVEA     LIN           L6
050500010206     C                   endif
050600070125
050700070125      * p.o. gestione con abilitazione scarico dati per PDA
050800070125     c                   eval      *in01 = (§ogpdaorm = 'S')
050900070926      * data partenza procedura in organigramma
051000071026     c                   eval      *in07 = *off
051100071026     c                   if        §ogddao > *zeros and §ogcgio = 'S'
051200070926     c                   move      §ogddao       data_org
051300071026      * controllo se attiva
051400071026     c                   if        dateu >= data_org
051500071026     c                   eval      *in07 = *on
051600071026     c                   endif
051700071026     c                   endif
051800010206
051900010206     C                   endsr
052000010206      **********************************************************************
052100010206      * CONTROLLI VIDEO 1
052200010206      **********************************************************************
052300010206     C     Sr_Contr01    begsr
052400010206
052500010206      * Reset indicatori
052600010206
052700070212     C                   setoff                                       410203
052800070212
052900070212      * recupero la password del p.o. gestione
053000070223      * se p.o. abilitato al PDA
053100070223     c                   if        *in01
053200070212     c                   clear                   tibs02ds
053300070212     c                   clear                   dsdp
053400070212     c                   eval      t02mod = 'C'
053500070212     c                   eval      t02sif = knsif
053600070212     c                   eval      t02cod = 'SDP'
053700070212     c                   movel(p)  v1cpor        t02ke1
053800070212     c                   call      'TIBS02R'
053900070212     c                   parm                    kpjba
054000070212     c                   parm                    tibs02ds
054100070212     c                   if        t02err <> *blanks
054200070212     c                   eval      v1cmsg = msg(16)
054300070212     c                   eval      *in28 = *on
054400070212     c                   eval      *in41 = *on
054500070212     c                   leavesr
054600070212     c                   endif
054700070212     c                   eval      dsdp = t02uni
054800070223     c                   endif
054900070205
055000070205      * non posso richiedere una distinta e tutte le distinte
055100070205     c                   if        v1allndc = 'SI' and v1ndc <> *zeros and
055200070205     c                             v1ndc <> *blanks
055300070205     c                   eval      v1cmsg = msg(12)
055400070205     c                   eval      *in28 = *on
055500070205     c                   eval      *in41 = *on
055600070205     c                   leavesr
055700070205     c                   endif
055800010206
055900010206      * Ricerca e controllo numero distinta
056000010206
056100070205     c                   if        v1allndc = *blanks
056200010206     C                   exsr      Sr_Contrndc
056300010206     C   90
056400010206     Cor 28              goto      endsrcontr
056500070205     c                   endif
056600070206
056700070206      * richieste tutte le distinte del PDA accendo indicatore
056800070207     c                   if        v1allndc = 'SI'
056900070207     c                   eval      *in02 = *on
057000070207     c                   endif
057100010206
057200010206     C     endsrcontr    endsr
057300010206      **********************************************************************
057400010206      * CONTROLLO   * Numero distinta *
057500010206      **********************************************************************
057600010206     C     Sr_Contrndc   begsr
057700010206
057800080723     c                   movel     *date         dataiso
057900080723     c                   subdur    1:*m          dataiso
058000080723
058100010206      *    Ricerco Distinta
058200010206
058300080723     c                   if        %scan('?':v1ndc) > *zeros
058400080723     c                   clear                   v1dndc
058500080723     c                   clear                   v1ndc
058600080723     c                   clear                   fidg31ds
058700080723     c                   eval      fdgfgs = v1cpor
058800080723     c                   move      dataiso       fdggda
058900080723     c                   eval      fdggal = *date
059000080724     c                   eval      fdgtfv = '2'
059100080723     c                   eval      fdgnpg = 4
059200080723     c                   eval      fdgric = 'S'
059300080723     c                   eval      fdgtpv = 'T'
059400080723      * esclude le distinte con fase RQE
059500080723     c                   eval      fdgiefas = 'E'
059600080723     c                   eval      fdgfas02 = 'RQE'
059700080723     c                   clear                   $fv6
059800080723      * impostato a tutti 99999
059900080723     c                   do        600           xx
060000080723     c                   z-add     999999        $fv6(xx)
060100080723     c                   enddo
060200080723     c                   eval      fdgtpop = 'R'
060300080723     c                   eval      kpjbu = fidg31ds
060400080723     c                   call      'FIDG31R'
060500080723     c                   parm                    kpjba
060600080723     c                   parm                    $fv6
060700080723     c                   eval      fidg31ds = kpjbu
060800080723      * se torno dal pgm senza aver selezionato niente pulisco i dati della DS
060900080723     c                   if        fdgnfv = *zeros
061000080723     c                   clear                   fidg31ds
061100080723     c                   else
061200080723     c                   eval      v1ndc = *all'0'
061300080723     c                   move      fdgnfv        v1ndc
061400080723     c                   endif
061500080723     c                   if        v1ndc = *zeros
061600080723     c                   clear                   v1ndc
061700080723     c                   endif
061800010206     C     v1ndc         ifne      *blanks
061900120613     c                   clear                   ddstflr
062000120613     C                   move      '4'           kdstnpg
062100120613     C                   move      v1ndc         kdstnfv
062200120613     C                   move      v1cpor        kdstfgs
062300120613     C     kfidst        chain     fidst01l
062400120613     c                   if        not %found(fidst01l)
062500060516     c                   clear                   fnlv55ds
062600060516     c                   eval      d55tpt = '6'
062700060516     c                   move      v1cpor        d55lin
062800060516     c                   eval      d55drf = dateu
062900060516     c                   call      'FNLV55R'
063000060516     c                   parm                    fnlv55ds
063100060516     c                   if        d55err = *blanks and d55tfa <> d55lin
063200120613     c                   move      d55tfa        kdstfgs
063300120613     c     kfidst        chain     fidst01l
063400060516     c                   endif
063500060516     c                   endif
063600120613     C                   if        %found(fidst01l)
063700010206     C                   exsr      Sr_Descriz
063800010206     C                   endif
063900010206     C                   seton                                        90  41
064000010206     C                   goto      endsrcndc
064100010206     C                   end
064200080723     c                   endif
064300010206
064400010206      *    Controllo numero Distinta
064500010206
064600010206     C                   clear                   v1dndc
064700010206     C     v1ndc         ifeq      *all'0'
064800010206     C                   clear                   v1dndc
064900070125     c                   clear                   v1ndc
065000010206     C                   endif
065100010206     C     v1ndc         ifne      *blanks
065200010206     C                   testn                   v1ndc                30
065300010206     C   30              move      v1ndc         w001a             1
065400010206     C   30              eval      *in30 = (W001A >= '0')
065500010206     C                   if        *IN30 = *off
065600010206     C                   movel     msg(2)        v1cmsg
065700010206     C                   seton                                        4128
065800010206     C                   goto      endsrcndc
065900010206     C                   endif
066000120613     c                   clear                   ddstflr
066100120613     C                   move      '4'           kdstnpg
066200120613     C                   move      v1ndc         kdstnfv
066300120613     C                   move      v1cpor        kdstfgs
066400120613     C     kfidst        chain     fidst01l
066500120613     c                   if        not %found(fidst01l)
066600060516     c                   clear                   fnlv55ds
066700060516     c                   eval      d55tpt = '6'
066800060516     c                   move      v1cpor        d55lin
066900060516     c                   eval      d55drf = dateu
067000060516     c                   call      'FNLV55R'
067100060516     c                   parm                    fnlv55ds
067200060516     c                   if        d55err = *blanks and d55tfa <> d55lin
067300120613     c                   move      d55tfa        kdstfgs
067400120613     c     kfidst        chain     fidst01l
067500060516     c                   endif
067600060516     c                   endif
067700120613     C                   if        %found(fidst01l)
067800120613     C     dstatb        ifne      *blanks
067900010206     C                   eval      *in28 = *on
068000010206     C                   eval      *in41 = *on
068100010206     C                   movel     msg(2)        v1cmsg
068200010206     C                   goto      endsrcndc
068300010206     C                   else
068400010206     C                   exsr      Sr_Descriz
068500010206     C                   endif
068600010206     C                   else
068700010206     C                   eval      *in28 = *on
068800010206     C                   eval      *in41 = *on
068900010206     C                   movel     msg(2)        v1cmsg
069000010206     C                   goto      endsrcndc
069100010206     C                   endif
069200010206     C                   else
069300010206     C                   eval      *in28 = *on
069400010206     C                   eval      *in41 = *on
069500010206     C                   movel     msg(4)        v1cmsg
069600010206     C                   goto      endsrcndc
069700010206     C                   endif
069800130923
069900130923      * se la distinta che sto cercando di quadrare ha data maggiore di oggi errore
070000130923     c                   IF        DSTdfv > dateu
070100130923     c                   eval      *in28 = *on
070200130923     c                   eval      *in41 = *on
070300130923     c                   eval      V1Cmsg = 'La distinta è del xx/xx/xxxx non +
070400130923     c                             può ancora essere quadrata'
070500130923     c                   eval      %subst(V1Cmsg:19:10) =
070600130923     c                             %subst(%editc(DSTdfv:'X'):7:2) + '/' +
070700130923     c                             %subst(%editc(DSTdfv:'X'):5:2) + '/' +
070800130923     c                             %subst(%editc(DSTdfv:'X'):1:4)
070900130923     c                   leavesr
071000130923     c                   ENDIF
071100090402
071200090402      * controllo se c'è la fase 'RQE' sulla distinta
071300090402     c                   movel     v1ndc         wndc
071400090402     c                   eval      wfase = 'RQE'
071500090402     c                   exsr      sr_ctrfase
071600090402      * se trova la fase distinta già quadrata errore
071700090402     c                   if        or90ndcok <> *blanks
071800090402     c                   eval      *in28 = *on
071900090402     c                   eval      *in41 = *on
072000090402     c                   movel     msg(20)       v1cmsg
072100090402     c                   leavesr
072200090402     c                   endif
072300070206
072400070302      * se p.o. abilitato al PDA
072500070212     c                   if        *in01
072600070302      * controllo se la distinta è una distinta abilitata allo scarico su PDA
072700080319      * ma non in test
072800120613     c                   eval      ddstflr = dstflr
072900120613     c                   if        (dstpda = 'O' or dstpda = 'E') and
073000080319     c                             §dsttstpda <> 'O' and §dsttstpda <> 'E'
073100070302     c                   eval      wokpda = *on
073200090402
073300080415      * controllo se c'è la fase 'RCH' sulla distinta
073400090402     c                   movel     v1ndc         wndc
073500090402     c                   eval      wfase = 'RCH'
073600080415     c                   exsr      sr_ctrfase
073700080415      * se non trova la fase devo emettere una videata per richiedere la
073800080415      * password per forzare la quadratura
073900090402     c                   if        or90ndcok = *blanks
074000070212     c                   exsr      sr_deldapda
074100070302     c                   else
074200070302     c                   eval      wokrch = *on
074300070212     c                   endif
074400080319     c                   endif
074500070302     c                   endif
074600010206
074700010206     C     endsrcndc     endsr
074800010206      **********************************************************************
074900010206      * Recupero Descrizione Distinta
075000010206      **********************************************************************
075100010206     C     Sr_Descriz    begsr
075200010206
075300021202     C                   movel     DSTpdr        kpdpdr
075400021202     C                   move      'A'           kpdtip
075500021202     C     kfapd         chain     fiapd01L
075600021203     C                   if        not%found(fiapd01l)
075700010206     C                   clear                   apdrsc
075800010206     C                   endif
075900010206
076000010206     C                   movel     'DEL '        wdel
076100120613     C                   move      dstdfv        wggg
076200010206     C                   move      '/'           wbarra1
076300010206     C                   move      '/'           wbarra2
076400120613     C                   move      dstdfv        wa4               4
076500010206     C                   movel     wa4           wmmm
076600120613     C                   movel     dstdfv        wa4               4
076700010206     C                   movel(p)  wa4           waaa
076800010206     C     dstpdr        ifne      0
076900010206     C                   movel(p)  dstpdr        wpdr
077000010206     C                   else
077100010206     C                   clear                   wpdr
077200010206     C                   endif
077300010206     C                   movel     apdrsc        wdescr
077400010206
077500010206     C                   endsr
077600070207
077700070207      *--------------------------------------------------------------------*
077800070207      * CANCELLO LA DISTINTA DA PDA
077900070207      *--------------------------------------------------------------------*
078000070207     c     sr_deldapda   begsr
078100070207
078200070207     c                   do        *hival
078300070207     c                   exfmt     fior26w
078400070207     c                   setoff                                       28
078500070207     c                   clear                   wfcmsg
078600070208     c   kl              leave
078700070207      * PASSWORD
078800070207     c                   if        wpassw = *blanks
078900070207     c                   eval      *in28 = *On
079000070207     c                   eval      wfcmsg = msg(15)
079100070207     c                   iter
079200070207     c                   endif
079300070207      * controllo se esatta
079400070212     c                   if        wpassw <> §sdppwd
079500070207     c                   eval      *in28 = *On
079600070207     c                   eval      wfcmsg = msg(14)
079700070207     c                   iter
079800070207     c                   endif
079900070212      * Controllo la validità
080000070212      * Data immissione/variazione password
080100070212     c                   clear                   wlbdat
080200070212     c                   z-add     §sdpdta       g02dat
080300070212     c                   call      'XSRDA8'
080400070212     c                   parm                    wlbdat
080500070212     c                   movel     g02inv        dataiso
080600070212      * Scadenza password
080700070212     c                   clear                   tibs02ds
080800070212     c                   eval      t02mod = 'C'
080900070212     c                   eval      t02sif = knsif
081000070212     c                   eval      t02cod = 'SDP'
081100070212     c                   eval      t02ke1 = 'PSW'
081200070212     c                   call      'TIBS02R'
081300070212     c                   parm                    kpjba
081400070212     c                   parm                    tibs02ds
081500070212     c                   if        t02err <> *blanks
081600070212     c                   eval      *in28 = *On
081700070212     c                   eval      wfcmsg = msg(17)
081800070212     c                   iter
081900070212     c                   endif
082000070212     c                   movel     t02uni        wgiorni
082100070212     c                   adddur    wgiorni:*d    dataiso
082200070212     c                   move      dataiso       datascad
082300070212     c                   if        dateu > dataScad
082400070212     c                   eval      *In28 = *On
082500070212     c                   eval      wfcmsg = msg(18)
082600070212     c                   iter
082700070212     c                   endif
082800070208     c   kl
082900070208     cor kf              leave
083000070207     c                   enddo
083100070207
083200070208     c                   if        *inkf
083300070302     c                   eval      wdelpda = *on
083400070207     c                   endif
083500070207
083600070207     c                   endsr
083700070207
083800010206      **********************************************************************
083900010206      * GESTIONE SUBFILE
084000010206      **********************************************************************
084100010206     C     Sr_Gessfl     begsr
084200010206
084300010208     C                   do        *hival
084400010206     C                   z-add     1             recsf
084500010206     C                   setoff                                       47
084600010518     C                   eval      *in45 = *off
084700010206      * Pulisco subfile
084800010206     C                   z-add     *zeros        nrr               4 0
084900010206     C                   eval      *in21 = *off
085000010206     C                   write     fior26c
085100010206     C                   eval      *in20 = *on
085200010206     C                   eval      *in21 = *on
085300060516
085400060516     c                   do        30            xx
085500060516     c                   if        l6(xx) = *zeros
085600060516     c                   leave
085700060516     c                   endif
085800010206
085900010206      * Carico il subfile
086000060516     c                   move      l6(xx)        keypor
086100010206     C                   movel     v1ndc         keyndc
086200010206     C     kfnorm        setll     fnorm06l
086300010206     C                   do        *hival
086400010206     C     kfnorm        reade     fnorm06l
086500010206
086600010206     C                   if        %eof
086700010206     C                   leave
086800010206     C                   endif
086900010206
087000010206      * solo 8000 record xchè il file video è definito così
087100010206     C                   if        nrr > 8000
087200010206     C                   leave
087300010206     C                   endif
087400010206
087500070206      * solo gli orm in fase 400 o 410 o 420
087600070207     C                   if        ormfao <> 400 and ormfao <> 410 and
087700070206     c                             ormfao <> 420
087800010206     C                   iter
087900010206     C                   endif
088000070212
088100070212      * sto caricando una sola distinta quindi spengo il 50
088200070212      * così ho tutti i campi del subfile visibili
088300070212     c                   eval      *in50 = *off
088400010206
088500010206     C                   exsr      Sr_Carsfl
088600010206     C                   enddo
088700060516
088800060516     c                   enddo
088900100409
089000100409     C                   IF        $GIRO > 0  AND NRR = 0
089100100409     C                   LEAVESR
089200100409     C                   ENDIF
089300010220
089400010220     C     emettosf      tag
089500100409
089600100409     C                   EVAL      $GIRO = 1
089700010206
089800010206      * Se non ho caricato emetto una videata di segnalazione.
089900010206     C                   if        nrr = *zeros
090000010206     C                   exfmt     fior263
090100010206     C                   eval      *in90 = *on
090200070423     C                   exsr      Sr_Pulvid1
090300010206     C                   endif
090400010206
090500010206      * Se ho caricato emetto il subfile
090600010206     C                   if        nrr > *zeros
090700010206     C                   write     fior262
090800010206     C                   exfmt     fior26c
090900010206     C                   endif
091000010206
091100010206      * F6=Conferma Totale
091200010206     C                   if        *inkf = *on
091300010206     C                   move      '1'           flgkf
091400010206     C                   else
091500010206     C                   clear                   flgkf
091600010206     C                   endif
091700010206      * F12=Ritorno
091800010206     C   kl              leave
091900010206
092000010206      *    Leggo Scelte Effettuate dall'utente
092100010206
092200010206     C                   if        nrr >*zeros
092300010206
092400010206     C                   eval      nrr = *zeros
092500010206     C                   do        *hival
092600010206     C                   eval      nrr = nrr +1
092700010206     C     nrr           chain     fior26s                            30
092800010206     C                   if        *in30 = *on
092900010206     C                   leave
093000010206     C                   endif
093100010206
093200010206     C                   setoff                                       28  47
093300010302     C                   clear                   dcmr
093400010206
093500010206      * Se immessa una causale questa deve esistere in tabella CMR
093600010206     C                   if        vs1cau <> *blanks
093700010206     C                   exsr      Sr_Contrcau
093800010206     C                   z-add     nrr           recsf
093900010206     C                   if        *in43 = *on
094000010206     C                   seton                                        28  47
094100010206     C                   update    fior26s
094200010206     C                   goto      emettosf
094300010206     C                   endif
094400010518      * Se orm ha già una bolla non posso dichiararlo non fatto
094500010411      * se non è un prepagato
094600010411     C                   if        vs1tor <> 'P'
094700010208     C                   exsr      Sr_Bolla
094800010518     C                   if        *in45 = *on
094900010208     C                   seton                                        28  47
095000010208     C                   update    fior26s
095100010208     C                   goto      emettosf
095200010208     C                   endif
095300010411     C                   endif
095400010206     C                   endif
095500010206      * Note
095600010206     C                   if        vs1not <> *blanks
095700010206     C                   z-add     nrr           recsf
095800010302     C                   clear                   fior06ds
095900010302     C                   movel     vs1fao        i06far
096000080410     c                   movel     'V'           i06flm
096100010301     C                   exsr      Sr_Note
096200010208     C                   move      *blanks       vs1not
096300050427     c                   If        flgkf <> '1'
096400010208     C                   update    fior26s
096500050427     C                   goto      emettosf
096600050427     c                   EndIf
096700010206     C                   endif
096800010206
096900010206      * F6 conferma totale del subfile
097000060309      * confermo orm se non è ancora stato confermato
097100060309     C                   if        flgkf = '1' and vs1okorm = *blanks
097200060309     C                   z-add     nrr           recsf
097300010206     C                   exsr      Sr_Conferma
097400011015     C                   if        *in44 = *on or
097500020524     C                             *in48 = *on or *in46
097600010206     C                   seton                                        28  47
097700010206     C                   update    fior26s
097800010606     C                   leave
097900010206     C                   endif
098000010301      * Se la causale prevede le note queste sono obbligatorie
098100010301     C                   if        vs1cau <> *blanks
098200020529     c                             and okbolla = *blanks
098300010301     C                   clear                   dcmr
098400010301     C                   clear                   TIBS02DS
098500010301     C                   movel     'C'           T02mod
098600010301     C                   movel     knsif         t02sif
098700010301     C                   movel     'CMR'         t02cod
098800010301     C                   movel(p)  vs1cau        T02ke1
098900010301     C                   call      'TIBS02R'
099000010301     C                   parm                    KPJBA
099100010301     C                   parm                    TIBS02DS
099200010301     C                   movel     t02ke1        vs1cau
099300010301     C                   movel     t02uni        dcmr
099400040413      * salvo i dati che mi possono servire x la chiusura ORM
099500040413     c                   Eval      wtenta = d§cmrnca
099600040413     c                   Move      d§cmrcca      sav_cmrcau
099700010302     C                   if        d§cmrnot <> *blanks
099800010302     C                   do        *hival
099900010302     C                   clear                   fior06ds
100000010302     C                   movel     d§cmrfar      i06far
100100080410     c                   movel     'V'           i06flm
100200080410     c                   if        *in01 and wokpda = *on and wokrch = *on
100300080410     c                   movel     'P'           i06flm
100400080410     c                   endif
100500010301     C                   exsr      Sr_Note
100600010301     C                   if        o06f03 = 'S' or o06f12 = 'S'
100700010301     C                   iter
100800010301     C                   endif
100900010301     C                   if        o06dai <> ormdfo
101000010301     C                   iter
101100010301     C                   endif
101200010301     C                   if        o06ori < ormofo
101300010301     C                   iter
101400010301     C                   endif
101500010301     C                   if        o06prg = 0
101600010301     C                   iter
101700010301     C                   endif
101800010301     C                   leave
101900010301     C                   enddo
102000010302     C                   endif
102100010301     C                   endif
102200040406
102300040406      * se ho quadrato con causale controllo quanti tentativi posso fare prima di chiudere
102400040406      * l'ORM in automatico
102500040413     c                   If        vs1cau <> *Blanks and d§cmrnca <> *Zeros
102600040406     c                             and okbolla = *Blanks
102700040406     c                   ExSr      Sr_ContrFase
102800040406     c                   If        wchiudi = *On
102900040406     c                   ExSr      Sr_Chiudi
103000040406     c                   EndIf
103100040406     c                   EndIf
103200010301
103300060309      * se arrivo qua vuol dire che l'orm è stato avanzato quindi aggiorno il flag
103400060309     c                   eval      vs1okorm = '1'
103500010206     C                   endif
103600060309
103700010206     C                   update    fior26s
103800010206
103900010206     C                   enddo
104000010206     C                   endif
104100010206
104200010208     C                   if        flgkf = *blanks
104300010206     C                   goto      emettosf
104400010529     C                   else
104500011015     C                   if        *in44 = *on or
104600020524     C                             *in48 = *on or *in46
104700010606     C                   iter
104800010606     C                   else
104900020529      * se non ho scritto una bolla riemetto il subfile
105000020529     c                   if        wnobolla = *on
105100020529     c                   eval      wnobolla = *off
105200080520     c                   iter
105300020529     C                   else
105400010529     C                   leave
105500020529     c                   endif
105600010606     C                   endif
105700010208     C                   endif
105800020529
105900010206
106000010208     C                   enddo
106100010206
106200010206     C                   endsr
106300070207
106400070205      *--------------------------------------------------------------------*
106500070206      * GESTIONE SUBFILE DI TUTTE LE DISTINTE PASSATE A PDA
106600070206      *--------------------------------------------------------------------*
106700070206     c     sr_allndc     begsr
106800070205
106900070206     c                   do        *hival
107000070206     c                   z-add     1             recsf
107100070206     c                   setoff                                       47
107200070206     c                   eval      *in45 = *off
107300081106     c                   clear                   savormndc
107400070206      * pulisco subfile
107500070206     c                   z-add     *zeros        nrr               4 0
107600070206     c                   eval      *in21 = *off
107700070206     c                   write     fior26c
107800070206     c                   eval      *in20 = *on
107900070206     c                   eval      *in21 = *on
108000070205
108100070205     c                   do        30            xx
108200070205     c                   if        l6(xx) = *zeros
108300070205     c                   leave
108400070205     c                   endif
108500070205
108600070206      * carico il subfile
108700070205     c                   move      l6(xx)        keypor
108800070206     c     keypor        setll     fnorm06l
108900070206     c                   do        *hival
109000070206     c     keypor        reade     fnorm06l
109100070205
109200070206     c                   if        %eof
109300070206     c                   leave
109400070206     c                   endif
109500070205
109600070205      * solo 8000 record xchè il file video è definito così
109700070206     c                   if        nrr > 8000
109800070206     c                   leave
109900070206     c                   endif
110000070206
110100070206      * solo gli orm in fase 400 - 410 - 420
110200070207     c                   if        ormfao <> 400 and ormfao <> 410 and
110300070206     c                             ormfao <> 420
110400070206     c                   iter
110500070206     c                   endif
110600070206
110700080415      * controllo se la distinta ha la fase 'RCH'
110800080415     c                   eval      wndc = ormndc
110900090402     c                   eval      wfase = 'RCH'
111000080415     c                   exsr      sr_ctrfase
111100080415      * se non trova la fase salto la distinta
111200080415     c                   if        or90ndcok = *blanks
111300070206     c                   iter
111400070206     c                   endif
111500080404
111600080404      * controllo se la distinta è una distinta abilitata allo scarico su PDA
111700080404      * ma non in test
111800120613     C                   move      '4'           kdstnpg
111900120613     c                   eval      kdstnfv = ormndc
112000120613     c                   eval      kdstfgs = keypor
112100080404     c                   clear                   ddstflr
112200080404     c     kfidst        chain     fidst01l
112300080404     c                   if        %found(fidst01l)
112400120613     c                   eval      ddstflr = dstflr
112500080404      * non attiva con PDA
112600120613     c                   if        dstpda <> 'O' and dstpda <> 'E'
112700080404     c                   iter
112800080404     c                   endif
112900080404      * in test
113000080404     c                   if        §dsttstpda = 'O' or §dsttstpda = 'E'
113100080404     c                   iter
113200080404     c                   endif
113300080404     c                   endif
113400070212
113500070212      * per ogni distinta devo prima emettere una riga con i dati della distinta
113600070212      * poi i rcd relativi alla distinta
113700070212     c                   if        ormndc <> savormndc
113800070212     c                   exsr      sr_carsfld
113900070212     c                   eval      savormndc = ormndc
114000080404     c                   eval      *in50 = *off
114100080404     C                   exsr      Sr_Carsfl
114200070212     c                   else
114300070212     c                   eval      *in50 = *off
114400070205     C                   exsr      Sr_Carsfl
114500070212     c                   endif
114600070205     C                   enddo
114700070205
114800070205     c                   enddo
114900100409
115000100409     C                   IF        $GIRO > 0  AND NRR = 0
115100100409     C                   LEAVESR
115200100409     C                   ENDIF
115300070205
115400070206     C     emettosfall   tag
115500100409
115600100409     C                   EVAL      $GIRO = 1
115700070205
115800070205      * Se non ho caricato emetto una videata di segnalazione.
115900070205     C                   if        nrr = *zeros
116000070205     C                   exfmt     fior263
116100070205     C                   eval      *in90 = *on
116200070205     C                   endif
116300070205
116400070205      * Se ho caricato emetto il subfile
116500070205     C                   if        nrr > *zeros
116600070205     C                   write     fior262
116700070205     C                   exfmt     fior26c
116800070205     C                   endif
116900070205
117000070205      * F6=Conferma Totale
117100070205     C                   if        *inkf = *on
117200070205     C                   move      '1'           flgkf
117300070205     C                   else
117400070205     C                   clear                   flgkf
117500070205     C                   endif
117600070205      * F12=Ritorno
117700070205     C   kl              leave
117800070205
117900070205      *    Leggo Scelte Effettuate dall'utente
118000070205
118100070205     C                   if        nrr >*zeros
118200070205
118300070205     C                   eval      nrr = *zeros
118400070205     C                   do        *hival
118500070205     C                   eval      nrr = nrr +1
118600070205     C     nrr           chain     fior26s                            30
118700070205     C                   if        *in30 = *on
118800070205     C                   leave
118900070205     C                   endif
119000080218
119100080218      * se riga di descrizione distinta leggo record sucessivo
119200080218     c                   if        vs1in50 = *on
119300080218     c                   iter
119400080218     c                   endif
119500070205
119600070205     C                   setoff                                       28  47
119700070205     C                   clear                   dcmr
119800070205
119900070205      * Se immessa una causale questa deve esistere in tabella CMR
120000070205     C                   if        vs1cau <> *blanks
120100070205     C                   exsr      Sr_Contrcau
120200070205     C                   z-add     nrr           recsf
120300070205     C                   if        *in43 = *on
120400070205     C                   seton                                        28  47
120500080117     c                   eval      *in49 = vs1in49
120600080117     c                   eval      *in50 = vs1in50
120700070205     C                   update    fior26s
120800070206     C                   goto      emettosfall
120900070205     C                   endif
121000070205      * Se orm ha già una bolla non posso dichiararlo non fatto
121100070205      * se non è un prepagato
121200070205     C                   if        vs1tor <> 'P'
121300070205     C                   exsr      Sr_Bolla
121400070205     C                   if        *in45 = *on
121500070205     C                   seton                                        28  47
121600080117     c                   eval      *in49 = vs1in49
121700080117     c                   eval      *in50 = vs1in50
121800070205     C                   update    fior26s
121900070206     C                   goto      emettosfall
122000070205     C                   endif
122100070205     C                   endif
122200070205     C                   endif
122300070205      * Note
122400070205     C                   if        vs1not <> *blanks
122500070205     C                   z-add     nrr           recsf
122600070205     C                   clear                   fior06ds
122700070205     C                   movel     vs1fao        i06far
122800080410     c                   movel     'V'           i06flm
122900070205     C                   exsr      Sr_Note
123000070205     C                   move      *blanks       vs1not
123100070205     c                   If        flgkf <> '1'
123200080117     c                   eval      *in49 = vs1in49
123300080117     c                   eval      *in50 = vs1in50
123400070205     C                   update    fior26s
123500070206     C                   goto      emettosfall
123600070205     c                   EndIf
123700070205     C                   endif
123800070205
123900070205      * F6 conferma totale del subfile
124000070205      * confermo orm se non è ancora stato confermato
124100070205     C                   if        flgkf = '1' and vs1okorm = *blanks
124200070205     C                   z-add     nrr           recsf
124300070205     C                   exsr      Sr_Conferma
124400070205     C                   if        *in44 = *on or
124500070205     C                             *in48 = *on or *in46
124600070205     C                   seton                                        28  47
124700080117     c                   eval      *in49 = vs1in49
124800080117     c                   eval      *in50 = vs1in50
124900070205     C                   update    fior26s
125000070205     C                   leave
125100070205     C                   endif
125200070205      * Se la causale prevede le note queste sono obbligatorie
125300070205     C                   if        vs1cau <> *blanks
125400070205     c                             and okbolla = *blanks
125500070205     C                   clear                   dcmr
125600070205     C                   clear                   TIBS02DS
125700070205     C                   movel     'C'           T02mod
125800070205     C                   movel     knsif         t02sif
125900070205     C                   movel     'CMR'         t02cod
126000070205     C                   movel(p)  vs1cau        T02ke1
126100070205     C                   call      'TIBS02R'
126200070205     C                   parm                    KPJBA
126300070205     C                   parm                    TIBS02DS
126400070205     C                   movel     t02ke1        vs1cau
126500070205     C                   movel     t02uni        dcmr
126600070205      * salvo i dati che mi possono servire x la chiusura ORM
126700070205     c                   Eval      wtenta = d§cmrnca
126800070205     c                   Move      d§cmrcca      sav_cmrcau
126900070205     C                   if        d§cmrnot <> *blanks
127000070205     C                   do        *hival
127100070205     C                   clear                   fior06ds
127200070205     C                   movel     d§cmrfar      i06far
127300080410     c                   movel     'P'           i06flm
127400070205     C                   exsr      Sr_Note
127500070205     C                   if        o06f03 = 'S' or o06f12 = 'S'
127600070205     C                   iter
127700070205     C                   endif
127800070205     C                   if        o06dai <> ormdfo
127900070205     C                   iter
128000070205     C                   endif
128100070205     C                   if        o06ori < ormofo
128200070205     C                   iter
128300070205     C                   endif
128400070205     C                   if        o06prg = 0
128500070205     C                   iter
128600070205     C                   endif
128700070205     C                   leave
128800070205     C                   enddo
128900070205     C                   endif
129000070205     C                   endif
129100070205
129200070205      * se ho quadrato con causale controllo quanti tentativi posso fare prima di chiudere
129300070205      * l'ORM in automatico
129400070205     c                   If        vs1cau <> *Blanks and d§cmrnca <> *Zeros
129500070205     c                             and okbolla = *Blanks
129600070205     c                   ExSr      Sr_ContrFase
129700070205     c                   If        wchiudi = *On
129800070205     c                   ExSr      Sr_Chiudi
129900070205     c                   EndIf
130000070205     c                   EndIf
130100070205
130200070205      * se arrivo qua vuol dire che l'orm è stato avanzato quindi aggiorno il flag
130300070205     c                   eval      vs1okorm = '1'
130400070205     C                   endif
130500070205
130600080117     c                   eval      *in49 = vs1in49
130700080117     c                   eval      *in50 = vs1in50
130800070205     C                   update    fior26s
130900070205
131000070205     C                   enddo
131100070205     C                   endif
131200070205
131300070205     C                   if        flgkf = *blanks
131400070206     C                   goto      emettosfall
131500070205     C                   else
131600070205     C                   if        *in44 = *on or
131700070205     C                             *in48 = *on or *in46
131800070205     C                   iter
131900070205     C                   else
132000070205      * se non ho scritto una bolla riemetto il subfile
132100070205     c                   if        wnobolla = *on
132200070205     c                   eval      wnobolla = *off
132300080520     c                   iter
132400070205     C                   else
132500070205     C                   leave
132600070205     c                   endif
132700070205     C                   endif
132800070205     C                   endif
132900070205
133000070205     C                   enddo
133100070205
133200070205     C                   endsr
133300070207
133400010206      **********************************************************************
133500010206      * CARICO SUBFILE
133600010206      **********************************************************************
133700010206     C     Sr_Carsfl     begsr
133800010206
133900070207     c                   eval      *in49 = *off
134000060309     c                   clear                   vs1okorm
134100010206     C                   clear                   vs1cau
134200010206     C                   clear                   vs1not
134300010206      * Inversione data ORM
134400010206     C                   z-add     ORMdao        G02inv
134500010206     C                   movel     '3'           G02err
134600010206     C                   call      'XSRDA8'
134700010206     C                   parm                    wlbdat
134800010206     C                   movel     G02dat        vs1dao
134900070212     C                   eval      vs1tor = ORMtor
135000070212     C                   eval      vs1poe = ORMpoe
135100070212     C                   eval      vs1nsr = ORMnsr
135200070212     C                   eval      vs1nor = ORMnor
135300070212     C                   eval      vs1nrv = ORMnrv
135400070212
135500070212      * imposto la riga da emettere a video
135600070212     c                   eval      vs1riga =
135700070212     c                             %editc(%dec(ormpoe):'X') + ' ' +
135800070212     c                             %editc(%dec(ormnsr):'Z') + ' ' +
135900070212     c                             %editc(%dec(ormnor):'Z') + ' ' +
136000070212     c                             %editc(%dec(ormnrv):'Z') + ' ' +
136100130417     c                             %subst(ormrsr:1:25) + ' ' +
136200070212     c                             %subst(%editc(%dec(ormdar):'X'):7:2) +
136300070212     c                             '/' +
136400070212     c                             %subst(%editc(%dec(ormdar):'X'):5:2) +
136500070212     c                             '/' +
136600070212     c                             %subst(%editc(%dec(ormdar):'X'):1:4) +
136700070212     c                             ' ' +
136800070212     c                             %subst(%editc(%dec(ormorr):'Z'):1:2) +
136900070212     c                             ':' +
137000070212     c                             %subst(%editc(%dec(ormorr):'X'):3:2)
137100070212     C                   eval      vs1fao = ORMfao
137200070212     C                   eval      vs1por = ORMpor
137300070206
137400070206      * se distinta scaricata su PDA recupero i dati dal file FIPLG
137500070302     c                   if        (*in01 and wokpda = *on and wokrch = *on)
137600070302     c                             or *in02
137700070206     c                   eval      ktiporcd = 'RES'
137800070529     c                   eval      ktipappl = 'R'
137900070206     c                   eval      keyndc = ormndc
138000070206     c     kfiplg01      setll     fiplg01l
138100070206     c                   do        *hival
138200070206     c     kfiplg01      reade     fiplg01l
138300070206     c                   if        %eof(fiplg01l)
138400070206     c                   leave
138500070206     c                   endif
138600070206     c                   eval      fiprcresds = plgdati
138700070206     c                   eval      vs1cau = §rescmr
138800070206     c                   enddo
138900070206      * se fase ancora 400 la metto in RI così l'utente nota che non ci sono
139000070206      * stati esiti per questo ORM
139100070207     c                   if        ormfao = 400
139200070207     c                   eval      *in49 = *on
139300070206      * inoltre forzo la causale = a 'XX' (non esiste in tabella così si ferma)
139400070206     c                   eval      vs1cau = 'XX'
139500070207     c                   endif
139600070206     c                   endif
139700070207      * mi salvo la distinta in schiera per poi stampare il prospetto ORM
139800070207     c                   eval      yy = 1
139900070207     c     ormndc        lookup    skndc(yy)                              30
140000070207     c                   if        not *in30
140100070207     c     *zeros        lookup    skndc(yy)                              30
140200070207     c                   if        *in30
140300070207     c                   eval      skndc(yy) = ormndc
140400071214     c                   eval      skddc(yy) = ormddc
140500080424     c                   eval      skpdr(yy) = dstpdr
140600070207     c                   endif
140700070207     c                   endif
140800080117
140900080117      * mi salvo gli indicatori di visualizzazione riga
141000080117     c                   eval      vs1in49 = *in49
141100080117     c                   eval      vs1in50 = *in50
141200010206
141300010206     C                   add       1             nrr
141400010206     C                   write     fior26s
141500010206
141600010206     C                   endsr
141700070212
141800070212      *-------------------------------------------------------------------------------*
141900070212      * CARICO SUBFILE - riga distinta per separare nel caso di allndc = 'S'
142000070212      *-------------------------------------------------------------------------------*
142100070212     c     sr_carsfld    begsr
142200070212
142300070212      * per prima cosa accendo l'indicatore 50 così non sono disponibili i campi della
142400070212      * riga di dettaglio
142500070212     c                   eval      *in50 = *on
142600070212
142700070212      * poi pulisco i campi di dettaglio
142800070212     c                   clear                   vs1okorm
142900070212     c                   clear                   vs1cau
143000070212     c                   clear                   vs1not
143100070212     c                   clear                   vs1nsr
143200070212     c                   clear                   vs1nor
143300070212     c                   clear                   vs1nrv
143400070212     c                   clear                   vs1fao
143500070212     c                   clear                   vs1por
143600070212     c                   clear                   vs1tor
143700070212     c                   clear                   vs1dao
143800070212      * spengo indicatore di visualizzazione
143900070212     c                   eval      *in49 = *off
144000070212
144100070212      * ora cerco i dati relativi alla distinta e compongo la riga del subfile
144200070212     c                   clear                   vs1riga
144300070212     c                   eval      kpdpdr = dstpdr
144400070212     c                   eval      kpdtip = 'A'
144500070212     c     kfapd         chain     fiapd01L
144600070212     c                   if        %found(fiapd01l)
144700070212     c                   eval      vs1riga = 'Distinta ' +
144800070212     c                             %editc(%dec(dstnfv):'X') + ' ' +
144900070212     c                             'del ' +
145000120613     c                             %subst(%editc(%dec(dstdfv):'X'):7:2) +
145100070212     c                             '/' +
145200120613     c                             %subst(%editc(%dec(dstdfv):'X'):5:2) +
145300070212     c                             '/' +
145400120613     c                             %subst(%editc(%dec(dstdfv):'X'):1:4) +
145500070212     c                             ' ' + 'Aut. ' +
145600070212     c                             %editc(%dec(dstpdr):'X') + ' ' +
145700070212     c                             apdrsc
145800070212     c                   endif
145900080117
146000080117      * mi salvo gli indicatori di visualizzazione riga
146100080117     c                   eval      vs1in49 = *in49
146200080117     c                   eval      vs1in50 = *in50
146300070212
146400070212     c                   add       1             nrr
146500070212     c                   write     fior26s
146600070212
146700070212     c                   endsr
146800010206      **********************************************************************
146900010206      * CONTROLLO  * Causale mancato ritiro *
147000010206      **********************************************************************
147100010206     C     Sr_Contrcau   BEGSR
147200010206
147300010206     C                   eval      *in43 = *off
147400010206
147500010206     C     '?'           scan      vs1cau                                 30
147600010206     C     *in30         ifeq      *on
147700010301     C                   clear                   dcmr
147800010206     C                   clear                   TIBS02DS
147900010206     C                   movel     'R'           t02mod
148000010206     C                   movel     knsif         t02sif
148100010206     C                   movel     'CMR'         t02cod
148200010206     C                   call      'TIBS02R'
148300010206     C                   parm                    KPJBA
148400010206     C                   parm                    TIBS02DS
148500010206     C                   movel     T02ke1        vs1cau
148600010206     C                   endif
148700010206
148800010206      * Controllo Causale
148900010206     C                   if        vs1cau <> *blanks
149000010301     C                   clear                   dcmr
149100010206     C                   clear                   TIBS02DS
149200010206     C                   movel     'C'           T02mod
149300010206     C                   movel     knsif         t02sif
149400010206     C                   movel     'CMR'         t02cod
149500010206     C                   movel(p)  vs1cau        T02ke1
149600010206     C                   call      'TIBS02R'
149700010206     C                   parm                    KPJBA
149800010206     C                   parm                    TIBS02DS
149900010206     C                   if        t02err <> *blanks
150000010206     C                   movel     msg(5)        vs1msg
150100010206     C                   eval      *in43 = *on
150200010206     C                   goto      endsrcau
150300010206     C                   endif
150400010206     C                   movel     t02ke1        vs1cau
150500010301     C                   movel     t02uni        dcmr
150600010302     C                   if        d§cmrnoq <> *blanks
150700010305     C                   movel     msg(8)        vs1msg
150800010302     C                   eval      *in43 = *on
150900010302     C                   goto      endsrcau
151000010302     C                   endif
151100010206     C                   endif
151200010206
151300010206     C     endsrcau      endsr
151400010208      **********************************************************************
151500010208      * CONTROLLO  * Orm / bolla *
151600010208      **********************************************************************
151700010208     C     Sr_Bolla      BEGSR
151800081106
151900081106     c                   eval      *in45 = *off
152000010208
152100010208     C                   clear                   worm             14
152200010208     C                   move      vs1poe        wpoe              3
152300010208     C                   move      vs1nsr        wnsr              2
152400010208     C                   move      vs1nor        wnor              7
152500010208     C                   move      vs1nrv        wnrv              2
152600010208     C                   eval      worm = wpoe + wnsr
152700010208     C                             + wnor + wnrv
152800060214     C                   movel     worm          kar4n14
152900060214     C     kfiar4        chain     fiar404l
153000060214     C                   if        %found(fiar404l)
153100010208     C                   movel     msg(7)        vs1msg
153200010518     C                   eval      *in45 = *on
153300010208     C                   goto      endsrbolla
153400010208     C                   endif
153500010208
153600010208     C     endsrbolla    endsr
153700010206      **********************************************************************
153800010206      * RICHIAMA IL PROGRAMMA DELLE NOTE
153900010206      **********************************************************************
154000010206     C     Sr_Note       BEGSR
154100010206
154200010206     C                   clear                   parm01
154300010206     C                   movel     v1cpor        pfgs
154400010206     C                   movel     vs1dao        i06dta
154500010206     C                   z-add     vs1poe        i06poe
154600010206     C                   z-add     vs1nor        i06nor
154700010206     C                   z-add     vs1nsr        i06nsr
154800010206     C                   z-add     vs1nrv        i06nrv
154900010206     C                   eval      i06dai = dateu
155000010206     C                   TIME                    W0140
155100010206     C                   movel     w0140         I06ori
155200080319      * devo impostare la causale nelle note così viene visualizzata dal
155300080319      * pgm di gestione note
155400080319     c                   clear                   fior06vds
155500080319     c                   if        vs1cau <> *blanks
155600080319     c                   eval      v06car = vs1cau
155700080319     c                   eval      v06dcar = d§cmrdes
155800080319     c                   endif
155900010206
156000010206     C                   movel     kpjbu         §kpjbu
156100010206     C                   movel     parm01        kpjbu
156200010206     C                   CALL      'FIOR06R'
156300010206     C                   PARM                    kpjba
156400010206     C                   parm                    fior06ds
156500080319     C                   parm                    fior06vds
156600010206     C                   movel     kpjbu         parm01
156700010206     C                   movel     §kpjbu        kpjbu
156800010206
156900010206     C                   endsr
157000010206      **********************************************************************
157100010206      * CONFERMA
157200010206      **********************************************************************
157300010206     C     Sr_Conferma   BEGSR
157400010206
157500010206     C                   eval      *in44 = *off
157600010530     C                   eval      *in46 = *off
157700011015     C                   eval      *in48 = *off
157800010530
157900010530      * Controllo che orm non sia allocato
158000010530     C     kfnorm1       chain(e)  fnorm01l
158100010530     C                   if        %error
158200010530     C                   movel     msg(6)        vs1msg
158300010530     C                   eval      *in44 = *on
158400010530     C                   goto      endsrconf
158500010530     C                   endif
158600010530
158700010530      * Se orm è in fase di bollettato (900) non devo aggiornare
158800020524     C                   if        ormfao = 900
158900020524     C                   unlock    fnorm01l
159000020524     C                   movel     msg(11)       vs1msg
159100020524     C                   eval      *in46 = *on
159200020524     C                   goto      endsrconf
159300020524     c                   endif
159400011015
159500011015      * Se orm è chiuso e sto cercando di chiuderlo di nuovo devo dare errore
159600030901     C                   if        (ormfao = 999 or OrmFao = 910) and
159700030901     C                             (d§cmrfar = '999' or d§cmrfar = '910')
159800011015     C                   movel     msg(10)       vs1msg
159900011015     C                   eval      *in48 = *on
160000011015     C                   goto      endsrconf
160100011015     C                   endif
160200040406
160300040406     c                   Eval      kOrfPoe = OrmPoe
160400040406     c                   Eval      kOrfNsr = OrmNsr
160500040406     c                   Eval      kOrfNor = OrmNor
160600040406     c                   Eval      kOrfNrv = OrmNrv
160700040406     c                   Eval      kOrfDae = OrmDfo
160800040406     c                   Eval      kOrfOre = OrmOfo
160900040406     c                   Eval      kOrfFar = OrmFao
161000020524
161100020524     c                   eval      ds_ormcra = ormcra
161200020524     c                   eval      ds_ormcor = ormcor
161300020524     c                   eval      ds_ormcrc = ormcrc
161400010206
161500010206      * RITIRATI
161600010206     C                   if        vs1cau = *blanks
161700010206      * Aggiorno ORM e scrivo fase
161800010529     C                   time                    w0140
161900010529     C                   move      w0140         wdtgio
162000010529     C                   z-add     wdtgio        G02DAT
162100010529     C                   clear                   G02ERR
162200010529     C                   call      'XSRDA8'
162300010529     C                   parm                    WLBDAT
162400010529     C                   z-add     g02inv        ormdfo
162500010206     C                   movel     w0140         ormofo
162600010206     C                   z-add     600           ormfao
162700040909     C                   eval      ormdtt = g02inv
162800070711      * imposto ormeti
162900070711     c                   clear                   dfar
163000070711     c                   clear                   tibs02ds
163100070711     c                   eval      t02mod = 'C'
163200070711     c                   eval      t02sif = knsif
163300070711     c                   eval      t02cod = 'FAR'
163400070711     c                   movel(p)  ormfao        t02ke1
163500070711     c                   call      'TIBS02R'
163600070711     c                   parm                    kpjba
163700070711     c                   parm                    tibs02ds
163800070711     c                   eval      dfar = t02uni
163900070711     c                   eval      ormeti = d§farass
164000010206     C                   update    fnorm000
164100010206
164200040909     c                   Clear                   Fnorf000
164300010206     C                   eval      orfpoe = ormpoe
164400010206     C                   eval      orfnsr = ormnsr
164500010206     C                   eval      orfnor = ormnor
164600010206     C                   eval      orfnrv = ormnrv
164700010406     C                   eval      orfpog = ormpor
164800010206     C                   eval      orfdae = ormdfo
164900010206     C                   eval      orfore = ormofo
165000010206     C                   eval      orffar = ormfao
165100010206     C                   eval      orfcar = *blanks
165200010206     C                   eval      orfpue = KNMUS
165300060516     c                   move      v1cpor        orffgs
165400040909     c                   Eval      OrfNdc = OrmNdc
165500040909     c                   Eval      OrfDdc = OrmDdc
165600010206     C                   write     fnorf000
165700010206      * MANCATO RITIRO
165800010206     C                   else
165900010529
166000020529     c                   clear                   okbolla
166100020523      * la causale richiesta genera addebito e l'orm è in fase 400
166200020523     c                   if        d§cmradd <> *blanks
166300020523     c                   exsr      sr_contradd
166400020523      * se f12 o f03 da immissione bolla riemetto la videata
166500020523     c                   if        okbolla = '1'
166600020529     c                   eval      wnobolla = *on
166700020523     C                   goto      endsrconf
166800020523     c                   endif
166900020523     c                   endif
167000020523
167100040909      * salvo i campi della distinta da riportare sulla fase
167200040909     c                   eval      savndc = ormndc
167300040909     c                   eval      savddc = ormddc
167400040909
167500010529     C                   time                    w0140
167600010529     C                   move      w0140         wdtgio
167700010529     C                   z-add     wdtgio        G02DAT
167800010529     C                   clear                   G02ERR
167900010529     C                   call      'XSRDA8'
168000010529     C                   parm                    WLBDAT
168100010529     C                   z-add     g02inv        ormdfo
168200010206     C                   movel     w0140         ormofo
168300010301     C                   movel     d§cmrfar      ormfao
168400010206     C                   clear                   ormtap
168500010206     C                   clear                   ormnpg
168600010206     C                   clear                   ormndc
168700010206     C                   clear                   ormddc
168800040909     C                   eval      ormdtt = g02inv
168900070711      * imposto ormeti
169000070711     c                   clear                   dfar
169100070711     c                   clear                   tibs02ds
169200070711     c                   eval      t02mod = 'C'
169300070711     c                   eval      t02sif = knsif
169400070711     c                   eval      t02cod = 'FAR'
169500070711     c                   movel(p)  ormfao        t02ke1
169600070711     c                   call      'TIBS02R'
169700070711     c                   parm                    kpjba
169800070711     c                   parm                    tibs02ds
169900070711     c                   eval      dfar = t02uni
170000070711     c                   eval      ormeti = d§farass
170100070926      * se ORM ancora assegnabile pulisco la data di stampa
170200070926     c                   if        ormeti <> *blanks and *in07
170300070926     c                   clear                   ormdst
170400071011      * devo pulire anche i dati su fnorg
170500071011     c     kfnorm1       chain     fnorg01l
170600071011    3c                   if        %found(fnorg01l)
170700080314     c                   eval      dorg01 = orgflo
170800071011     c                   eval      orgndc = 0
170900071011     c                   eval      orgpdc = 0
171000071011     c                   eval      orgfgs = 0
171100071011     c                   eval      orgddc = 0
171200071011     c                   eval      orgnftl= 0
171300071011     c                   eval      orgslo = 0
171400071011     c                   eval      orgdtvdis= 0
171500071011     c                   eval      orghvdis= 0
171600071026     c                   eval      orgfao = 0
171700071026     c                   eval      orgdfo = 0
171800071026     c                   eval      orgofo = 0
171900080314      * pulisco il flag di ORM dato a PDA nel caso di mancato ritiro anche se si chiude l'ORM
172000080314     c                   clear                   §orgpda
172100080314     c                   eval      orgflo = dorg01
172200071011     c                   update    fnorg000
172300071011    3c                   endif
172400070926     c                   endif
172500010206     C                   update    fnorm000
172600010206
172700081125     c                   clear                   dorf01
172800040909     c                   Clear                   Fnorf000
172900010206     C                   eval      orfpoe = ormpoe
173000010206     C                   eval      orfnsr = ormnsr
173100010206     C                   eval      orfnor = ormnor
173200010206     C                   eval      orfnrv = ormnrv
173300010406     C                   eval      orfpog = ormpor
173400010206     C                   eval      orfdae = ormdfo
173500010206     C                   eval      orfore = ormofo
173600010206     C                   eval      orffar = ormfao
173700010206     C                   eval      orfcar = vs1cau
173800010206     C                   eval      orfpue = KNMUS
173900060516     c                   move      v1cpor        orffgs
174000040909     c                   Eval      OrfNdc = savndc
174100040909     c                   Eval      OrfDdc = savddc
174200081125      * imposto se non ho fatto l'ulteriore addebito
174300081125     c                   if        $noadd = *on
174400081125     c                   eval      §orfadd = 'S'
174500081125     c                   endif
174600081125     c                   eval      orfflo = dorf01
174700010206     C                   write     fnorf000
174800010206
174900010206     C                   endif
175000010206
175100010206     C     endsrconf     endsr
175200020523      **********************************************************************
175300020523      * Controllo se devo generare la bolla di addebito
175400020523      **********************************************************************
175500020523     c     sr_contradd   begsr
175600081125
175700081125     c                   eval      $noadd = *off
175800081125
175900081125      * controllo che l'ORM non abbia già avuto una causale che genera addebito
176000081125      * in questo caso faccio solo un addebito
176100081125     c     kfnorm1       setll     fnorf01l
176200081125     c                   do        *hival
176300081125     c     kfnorm1       reade     fnorf01l
176400081125     c                   if        %eof (fnorf01l)
176500081125     c                   leave
176600081125     c                   endif
176700081125      * senza causale rileggo
176800081125     c                   if        orfcar = *blanks
176900081125     c                   iter
177000081125     c                   endif
177100081125      * fase da PDA rileggo
177200081125     c                   if        orffar = 410
177300081125     c                   iter
177400081125     c                   endif
177500081125     c                   clear                   a_dcmr
177600081125     c                   clear                   tibs02ds
177700081125     c                   eval      t02mod = 'C'
177800081125     c                   eval      t02sif = knsif
177900081125     c                   eval      t02cod = 'CMR'
178000081125     c                   eval      t02ke1 = orfcar
178100081125     c                   call      'TIBS02R'
178200081125     c                   parm                    kpjba
178300081125     c                   parm                    tibs02ds
178400081125     c                   eval      a_dcmr = t02uni
178500081125      * se causale che genera bolla vado avanti senza generare l'addebito per la seconda volta
178600081125     c                   if        a_d§cmradd = 'S'
178700081125     c                   eval      $noadd = *on
178800081125     c                   leavesr
178900081125     c                   endif
179000081125     c                   enddo
179100020523
179200020523     c                   clear                   sav_lpksc
179300020523
179400020523      * se cod.ordinante codificato addebito
179500020523 b1  c                   if        ormcor <> *zeros and ormcor2 <> 8888
179600020523     c                             and ormcor2 <> 9999
179700020523     c                   clear                   dblp
179800090505     c                   movel     ormcor        §lpksca
179900020702      * se c'è imposto il codice bollettazione
180000020702     c     ormcor        chain     fnacr01l
180100020702     c                   if        %found(fnacr01l) and acratb = *blanks
180200020702     c                             and acrksc <> *zeros
180300090505     c                   move      acrksc        §lpksca
180400090505     c                   if        acrccc<>999
180500090505     c                   move      acrccc        §lpctra
180600020702     c                   endif
180700090505     c                   endif
180800020523     c                   eval      §lprsd = ormrso
180900020523     c                   eval      §lpind = ormino
181000020523     c                   eval      §lpcad = ormcao
181100020523     c                   eval      §lplod = ormloo
181200020523     c                   eval      §lpprd = ormpro
181300020523     c                   eval      §lpnzd = ormnao
181400020523     c                   exsr      sr_wrtbolla
181500020523 x1  c                   else
181600020523      * se non inserito l'ordinante
181700020523 b2  c                   if        ormrso = *blanks
181800020523      * orm non commissionato
181900020523 b3  c                   if        ormpoe = ormpor
182000020523      * se cod.mittente codificato addebito
182100020523 b4  c                   if        ormcra <> *zeros and ormcra2 <> 8888
182200020523     c                             and ormcra2 <> 9999
182300020523     c                   clear                   dblp
182400090505     c                   movel     ormcra        §lpksca
182500020702      * se c'è imposto il codice bollettazione
182600020702     c     ormcra        chain     fnacr01l
182700020702     c                   if        %found(fnacr01l) and acratb = *blanks
182800020702     c                             and acrksc <> *zeros
182900090505     c                   move      acrksc        §lpksca
183000090505     c                   if        acrccc<>999
183100090505     c                   move      acrccc        §lpctra
183200020702     c                   endif
183300090505     c                   endif
183400020523     c                   eval      §lprsd = ormrsr
183500020523     c                   eval      §lpind = orminr
183600020523     c                   eval      §lpcad = ormcar
183700020523     c                   eval      §lplod = ormlor
183800020523     c                   eval      §lpprd = ormprr
183900020523     c                   eval      §lpnzd = ormnar
184000020523     c                   exsr      sr_wrtbolla
184100020523      * se cod.mittente non codificato
184200020523 x4  c                   else
184300020523      * se cod.destinatario codificato addebito
184400020523 b5  c                   if        ormcrc <> *zeros and ormcrc2 <> 8888
184500020523     c                             and ormcrc2 <> 9999
184600020523     c                   clear                   dblp
184700090505     c                   movel     ormcrc        §lpksca
184800020702      * se c'è imposto il codice bollettazione
184900020702     c     ormcrc        chain     fnacr01l
185000020702     c                   if        %found(fnacr01l) and acratb = *blanks
185100020702     c                             and acrksc <> *zeros
185200090505     c                   move      acrksc        §lpksca
185300090505     c                   if        acrccc<>999
185400090505     c                   move      acrccc        §lpctra
185500020702     c                   endif
185600090505     c                   endif
185700020523     c                   eval      §lprsd = ormrsc
185800020523     c                   eval      §lpind = orminc
185900020523     c                   eval      §lpcad = ormcac
186000020523     c                   eval      §lplod = ormloc
186100020523     c                   eval      §lpprd = ormprc
186200020523     c                   eval      §lpnzd = ormnac
186300020523     c                   exsr      sr_wrtbolla
186400020523 e5  c                   endif
186500020523 e4  c                   endif
186600020523      * orm commissionato
186700020523 x3  c                   else
186800020523      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
186900020523 b4  c                   if        ormcrc <> *zeros and ormcrc2 <> 8888
187000020523     c                             and ormcrc2 <> 9999  and ormcrc1 = ormpoe
187100020523     c                   clear                   dblp
187200090505     c                   movel     ormcrc        §lpksca
187300020702      * se c'è imposto il codice bollettazione
187400020702     c     ormcrc        chain     fnacr01l
187500020702     c                   if        %found(fnacr01l) and acratb = *blanks
187600020702     c                             and acrksc <> *zeros
187700090505     c                   move      acrksc        §lpksca
187800090505     c                   if        acrccc<>999
187900090505     c                   move      acrccc        §lpctra
188000020702     c                   endif
188100090505     c                   endif
188200020523     c                   eval      §lprsd = ormrsc
188300020523     c                   eval      §lpind = orminc
188400020523     c                   eval      §lpcad = ormcac
188500020523     c                   eval      §lplod = ormloc
188600020523     c                   eval      §lpprd = ormprc
188700020523     c                   eval      §lpnzd = ormnac
188800020523     c                   exsr      sr_wrtbolla
188900020523      * se non codificato o p.o. differenti
189000020523 x4  c                   else
189100020523      * se cod.mittente codificato addebito
189200020523 b5  c                   if        ormcra <> *zeros and ormcra2 <> 8888
189300020523     c                             and ormcra2 <> 9999
189400020523     c                   clear                   dblp
189500090505     c                   movel     ormcra        §lpksca
189600020702      * se c'è imposto il codice bollettazione
189700020702     c     ormcra        chain     fnacr01l
189800020702     c                   if        %found(fnacr01l) and acratb = *blanks
189900020702     c                             and acrksc <> *zeros
190000090505     c                   move      acrksc        §lpksca
190100090505     c                   if        acrccc<>999
190200090505     c                   move      acrccc        §lpctra
190300020702     c                   endif
190400090505     c                   endif
190500020523     c                   eval      §lprsd = ormrsr
190600020523     c                   eval      §lpind = orminr
190700020523     c                   eval      §lpcad = ormcar
190800020523     c                   eval      §lplod = ormlor
190900020523     c                   eval      §lpprd = ormprr
191000020523     c                   eval      §lpnzd = ormnar
191100020523     c                   exsr      sr_wrtbolla
191200020523 e5  c                   endif
191300020523 e4  c                   endif
191400020523 e3  c                   endif
191500020523 e2  c                   endif
191600020523 e1  c                   endif
191700020523
191800020523     c                   endsr
191900020523      **********************************************************************
192000020523      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
192100020523      **********************************************************************
192200020523     c     sr_wrtbolla   begsr
192300020523
192400020523      * richiamo il programma per la scelta delle stampanti
192500020523     c                   exsr      sr_scestp
192600020524     c   92              eval      okbolla = '1'
192700020523     c   92              goto      endwrtbolla
192800020523
192900020523     c                   clear                   kpjbu
193000020523     c                   clear                   dtasv
193100020523
193200020523     c                   eval      §lpfpr = 'T'
193300020523     c                   movel     ormpor        §lpccn
193400020523     c                   move      9999          §lpccn
193500020523     c                   movel     dateu         §lpaas
193600020523     c                   move      dateu         §lpmgs
193700020523     c                   eval      §lplnp = ormpor
193800020523     c                   eval      §lpcbo = 'FY'
193900020523     c                   eval      §lpsop = '+'
194000020530     c                   eval      §lplna = ormpoe
194100020523     c                   eval      §lprsm = ormrsr
194200020523     c                   eval      §lpinm = orminr
194300020523     c                   eval      §lpcam = ormcar
194400020523     c                   eval      §lplom = ormlor
194500020523     c                   eval      §lpprm = ormprr
194600020523     c                   eval      §lpnzm = ormnar
194700020523
194800020523 b1  c                   if        ormncl <> 0
194900020523     c                   eval      §lpncl = ormncl
195000020523 x1  c                   else
195100020523     c                   eval      §lpncl = 1
195200020523 e1  c                   endif
195300020523 b1  c                   if        ormpkg <> 0
195400020523     c                   eval      §lppkb = ormpkg
195500020523 x1  c                   else
195600020523 b2  c                   if        ormvlm <> 0
195700020523     c     ormvlm        mult      200           §lppkb
195800020523 x2  c                   else
195900020523 b3  c                   if        ormbnc <> 0
196000020523     c     ormbnc        mult      200           §lppkb
196100020523 e3  c                   endif
196200020523 e2  c                   endif
196300020523 e1  c                   endif
196400020523
196500020523     c                   clear                   ds_keyorm
196600020523     c                   eval      ds_poe = ORMpoe
196700020523     c                   eval      ds_nsr = ORMnsr
196800020523     c                   eval      ds_nor = ORMnor
196900020523     c                   eval      ds_nrv = ORMnrv
197000041201     c                   move      ds_keyorm     §lprmn
197100020523
197200020523     c                   eval      §lprma = ormrfa
197300020523     c                   eval      §lprmo = vs1cau + '-' + d§cmrdes1
197400020523     c                   eval      §lpctm = '4F'
197500020523     c                   movel     dateu         §lpdrt
197600020523     c                   eval      §lppdr = DSTpdr
197700120613     c                   eval      §lpfpp = dstfpp
197800020523
197900020523
198000020523     C                   call      'FNLS01R'
198100020523     C                   parm                    kpjba
198200020523     C                   parm                    dblp
198300020523     C                   parm                    dtasv
198400030623     c                   Parm                    Trul90ds
198500020523
198600020523     C                   if        §lpfpr = '4' or §lpfpr = '3'
198700020523     C                             or §lpfpr = '5'
198800020523     C                   eval      okbolla = '1'
198900020523     C                   endif
199000020523
199100020523     c     endwrtbolla   endsr
199200020523      **********************************************************************
199300020523      * scelta stampante per immissione bolla
199400020523      **********************************************************************
199500020523     c     sr_scestp     begsr
199600020523
199700020523     C                   clear                   kpjbu
199800020523     c                   setoff                                       92
199900020523
200000020523      * Richiesta stampanti
200100020523     C                   if        conta = *zeros
200200030623     C                   clear                   trul90ds
200300030623     C                   movel     'S'           D90rse
200400030623     C                   movel     'S'           D90rsb
200500030623     C                   call      'TRUL90R'
200600020523     C                   parm                    kpjba
200700030623     C                   Parm                    trul90ds
200800020523      * F3=Fine
200900030623     C                   if        D90f3 = '1'
201000020523     C                   eval      *in92 = *on
201100020523     C                   goto      endstp
201200020523     C                   endif
201300030307
201400030307     C                   z-add     103           lung
201500020523
201600020523      * Ovrprtf Segnacolli
201700030623     C                   movea     D90pre        cm3(28)
201800030623     C                   movea     D90mde        cm3(50)
201900020523     C                   clear                   comman
202000020523     C                   movea     cm3           comman
202100020523     C                   CALL      'QCMDEXC'                            92
202200020523     C                   PARM                    comman
202300020523     C                   PARM                    lung
202400020523     C   92              goto      endstp
202500030623
202600030623      * Bolle - Nuovo modulo
202700030623
202800030623     c                   Z-Add     110           lung
202900030623
203000030623      * Ovrprtf Bolle A4
203100030623     c                   Movea     D90prb4       cma4(30)
203200030623     c                   Movea     D90mdb4       cma4(52)
203300030623     c                   Clear                   Comman
203400030623     c                   Movea     cmA4          Comman
203500030623     c                   Call      'QCMDEXC'                            92
203600030623     c                   Parm                    Comman
203700030623     c                   Parm                    Lung
203800030623     c   92              goto      endstp
203900030623
204000030623      * Ovrprtf Bolle A5
204100030623     c                   Movea     D90prb5       cmA5(30)
204200030623     c                   Movea     D90mdb5       cmA5(52)
204300030623     c                   Clear                   Comman
204400030623     c                   Movea     cmA5          Comman
204500030623     c                   Call      'QCMDEXC'                            92
204600030623     c                   Parm                    Comman
204700030623     c                   Parm                    Lung
204800030623     c   92              goto      endstp
204900020523
205000020523     C                   eval      conta = conta +1
205100020523     C                   endif
205200020523
205300020523     c     endstp        endsr
205400040406      *-------------------------------------------------------------------------------*
205500040406      * Controllo se devo chiudere l'ORM in automatico in base i tentativi fatti
205600040406      *-------------------------------------------------------------------------------*
205700040406     c     Sr_ContrFase  BegSr
205800040406
205900040406     c                   Eval      wchiudi = *Off
206000040406     c                   Clear                   wconta
206100081007     c                   clear                   Sav_OrfFar
206200040406      * Mi posiziono sull'ultima fase dell'ORM
206300040406     c     kOrf          Setll     Fnorf01l
206400040406     c                   Do        *Hival
206500040406      * Torno indietro a leggere
206600040406     c     kFnorm1       Readpe    Fnorf01l
206700040406      * Fine file
206800040406     c                   If        %Eof(Fnorf01l)
206900040406     c                   Leave
207000040406     c                   EndIf
207100040406      * se stessa fase torno a leggere
207200040406     c                   If        OrfFar = Sav_OrfFar
207300040406     c                   Iter
207400040406     c                   EndIf
207500040406     c                   Eval      sav_OrfFar = OrfFar
207600040406      * fase senza causale
207700040406     c                   If        OrfCar = *Blanks
207800040406     c                   Iter
207900040406     c                   EndIf
208000040406      * controllo la causale della fase
208100040406     c                   Clear                   dCmr
208200040406     c                   Clear                   Tibs02ds
208300040406     c                   Eval      T02Mod = 'C'
208400040406     c                   Eval      T02Sif = knsif
208500040406     c                   Eval      T02Cod = 'CMR'
208600040406     c                   Eval      T02Ke1 = OrfCar
208700040406     c                   Call      'TIBS02R'
208800040406     c                   Parm                    kpjba
208900040406     c                   Parm                    Tibs02ds
209000040406     c                   Eval      dCmr = T02Uni
209100040413      * se non è da  conteggiare torno a leggere
209200040413     c                   If        d§cmrccc = *Blanks
209300040406     c                   Iter
209400040406     c                   EndIf
209500040406
209600040406      * Conto
209700040406     c                   add       1             wconta
209800040406      * se raggiunto il max dei tentativi chiudo l'ORM
209900040406     c                   If        wconta >= wtenta
210000040406     c                   Eval      wchiudi = *On
210100040406     c                   Leave
210200040406     c                   EndIf
210300040406     c                   EndDo
210400040406
210500040406     c                   EndSr
210600040406      *-------------------------------------------------------------------------------*
210700040406      * Chiamo la manutenzione ORM x proporre la chiusura
210800040406      *-------------------------------------------------------------------------------*
210900040406     c     Sr_Chiudi     BegSr
211000040406
211100040406     c                   Clear                   Fior05ds
211200040406     c                   Eval      §rmtla = 'C'
211300040406     c                   Eval      §rmfpr = 'P'
211400040406     c                   Eval      §rmpoe = OrmPoe
211500040406     c                   Eval      §rmnsr = OrmNsr
211600040406     c                   Eval      §rmnor = OrmNor
211700040406     c                   Eval      §rmnrv = OrmNrv
211800040406     c                   Eval      §rmtor = OrmTor
211900040406     c                   Eval      §rmcor = OrmCor
212000040406     c                   Eval      §rmrso = OrmRso
212100040406     c                   Eval      §rmino = OrmIno
212200040406     c                   Eval      §rmcao = OrmCao
212300040406     c                   Eval      §rmloo = OrmLoo
212400040406     c                   Eval      §rmpro = OrmPro
212500040406     c                   Eval      §rmnao = OrmNao
212600040406     c                   Eval      §rmcra = OrmCra
212700040406     c                   Eval      §rmrsr = OrmRsr
212800040406     c                   Eval      §rminr = OrmInr
212900040406     c                   Eval      §rmcar = OrmCar
213000040406     c                   Eval      §rmlor = OrmLor
213100040406     c                   Eval      §rmprr = OrmPrr
213200040406     c                   Eval      §rmnar = OrmNar
213300040406     c                   Eval      §rmrer = OrmRer
213400040406     c                   Eval      §rmter = OrmTer
213500040406     c                   Eval      §rmcrc = OrmCrc
213600040406     c                   Eval      §rmrsc = OrmRsc
213700040406     c                   Eval      §rminc = OrmInc
213800040406     c                   Eval      §rmcac = OrmCac
213900040406     c                   Eval      §rmloc = OrmLoc
214000040406     c                   Eval      §rmprc = OrmPrc
214100040406     c                   Eval      §rmnac = OrmNac
214200040406     c                   Eval      §rmffd = OrmFfd
214300040406     c                   Eval      §rmdar = OrmDar
214400040406     c                   Eval      §rmorr = OrmOrr
214500040406     c                   Eval      §rmnam = OrmNam
214600040406     c                   Eval      §rmncl = OrmNcl
214700040406     c                   Eval      §rmpkg = OrmPkg
214800040406     c                   Eval      §rmvlm = OrmVlm
214900040406     c                   Eval      §rmbnc = OrmBnc
215000040406     c                   Eval      §rmblc = OrmBlc
215100040406     c                   Eval      §rmatt = OrmAtt
215200040406     c                   Eval      §rmmtc = OrmMtc
215300040406     c                   Eval      §rmpag = OrmPag
215400040406     c                   Eval      §rmksc = OrmKsc
215500040406     c                   Eval      §rmctr = OrmCtr
215600040406     c                   Eval      §rmpor = OrmPor
215700040406     c                   Eval      §rmzor = OrmZor
215800040406     c                   Eval      §rmpoc = OrmPoc
215900040406     c                   Eval      §rmno1 = OrmNo1
216000040406     c                   Eval      §rmno2 = OrmNo2
216100040406     c                   Eval      §rmcst = OrmCst
216200040406     c                   Eval      §rmvcs = OrmVcs
216300040406     c                   Eval      §rmfcs = OrmFcs
216400040406     c                   Eval      §rmddt = OrmDdt
216500040406     c                   Eval      §rmsto = OrmSto
216600040406     c                   Eval      §rmrfa = OrmRfa
216700040406     c                   Eval      §rmspi = OrmSpi
216800040406     c                   Eval      §rmcau = Sav_CmrCau
216900040406     c                   Eval      §rmflo = OrmFlo
217000040406     c                   Eval      §rmflv = 'B'
217100040406      * Passo alcuni parametri anche nella parm01
217200040406     c                   Clear                   parm01
217300040406     c                   eval      pmio = 'S'
217400041014     c                   Eval      pndc = savndc
217500041014     c                   Eval      pddc = savddc
217600120613     c                   eval      pdstfgs = v1cpor
217700040406     c                   Eval      kpjbu = parm01
217800040406
217900040406     c                   Call      'FIOR05R'
218000040406     c                   Parm                    kpjba
218100040406     c                   Parm                    Fior05ds
218200040406
218300040406     c                   EndSr
218400041012      *-------------------------------------------------------------------------------*
218500041012      * Richiamo Fnlrf8r x gestione rientri
218600041012      *-------------------------------------------------------------------------------*
218700041012     c     Sr_Rientri    BegSr
218800041012      * Richiamo Fnlrf8r x gestione rientri
218900041012      * se presente la distinta nel file dei rientri
219000120613     c                   eval      kdstfgs = v1cpor
219100070207     c                   move      skndc(yy)     kqdtnfv
219200080424     c                   move      skddc(yy)     kqdtddc
219300080424     c                   move      skpdr(yy)     kqdtpdr
219400041012     c     kQdt01        setll     Fiqdt01l
219500041012     c                   If        %Equal(Fiqdt01l)
219600120613     c                   Z-Add     kdstfgs       karbifp
219700070207     c                   move      skndc(yy)     karbndc
219800050225     c     karb78        setll     fnarb78l
219900050225     c                   If        Not %Equal(Fnarb78l)
220000041012     c                   clear                   fnlrf8ds
220100041012     c                   z-add     kqdtnfv       rf8dis
220200120613     c                   z-add     kdstfgs       rf8fgs
220300041012     c                   movel(p)  fnlrf8ds      kpjbu
220400041012     c                   call      'FNLRF8R'
220500041012     c                   parm                    kpjba
220600050225     c                   endif
220700041012     c                   endif
220800041012
220900041012     c                   EndSr
221000080314
221100080314      *--------------------------------------------------------------------*
221200080319      * Annullo i dati su FIPDO
221300080314      *--------------------------------------------------------------------*
221400080314     c     sr_aggpdo     begsr
221500080314
221600080314     c                   eval      or56fgs = v1cpor
221700080404     c                   eval      or56ddc = skddc(yy)
221800080404     c                   eval      or56ndcd = skndc(yy)
221900080404     c                   eval      or56ndca = skndc(yy)
222000080314     c                   call      'FIOR56R'
222100080314     c                   parm                    kpjbu
222200080314     c                   parm                    fior56ds
222300080314      * il pgm torna degli errori, ma dato che ho già fatto qua tutti i controlli
222400080314      * del caso, non me ne preoccupo
222500080314
222600080314     c                   endsr
222700080415
222800080415      *-------------------------------------------------------------------------------*
222900080415      * Controllo se esiste la fase RCH sulla distinta
223000080415      *-------------------------------------------------------------------------------*
223100080415     c     sr_ctrfase    begsr
223200080415
223300080415     c                   clear                   fior90ds
223400080415     c                   eval      or90tla = 'C'
223500090402     c                   eval      or90fas = wfase
223600080415     c                   eval      or90fgs = v1cpor
223700080415     c                   eval      or90ndc = wndc
223800080415     c                   call      'FIOR90R'
223900080415     c                   parm                    kpjba
224000080415     c                   parm                    fior90ds
224100080415
224200080415     c                   endsr
224300080314
224400080327      *-------------------------------------------------------------------------------*
224500080415      * Scrivo la fase RQE sulla distinta
224600080327      *-------------------------------------------------------------------------------*
224700080327     c     sr_fase       begsr
224800080327
224900080415     c                   clear                   fior90ds
225000080415     c                   eval      or90tla = 'W'
225100080415     c                   eval      or90fas = 'RQE'
225200080327     c                   eval      or90fgs = v1cpor
225300080327     c                   eval      or90ndc = skndc(yy)
225400080327     c                   eval      or90flgins = 'A'
225500080327     c                   call      'FIOR90R'
225600080327     c                   parm                    kpjba
225700080327     c                   parm                    fior90ds
225800080327      * non controllo l'errore di ritorno tanto sono alla fine del PGM
225900080327      * al massimo non ho scritto la fase
226000080327
226100080327     c                   endsr
226200070212
226300070207      *-------------------------------------------------------------------------------*
226400070207      * Stampo prospetto ORM
226500070207      *-------------------------------------------------------------------------------*
226600070207     c     sr_prospetto  begsr
226700070207
226800070427     c                   eval      i24fgs = v1cpor
226900071214     c                   eval      i24data = skddc(yy)
227000071214     c                   eval      i24ndcda = skndc(yy)
227100071214     c                   eval      i24ndcal = skndc(yy)
227200070212     c                   eval      i24forzaq = wdelpda
227300070207     c                   eval      kpjbu = fior24ds
227400070207     c                   movel     'OR24'        kcoaz
227500070302     c                   call      'BCH10'
227600070207     c                   parm                    kpjba
227700070207
227800070207     c                   endsr
227900080526
228000080526      *------------------------------------------------------------------------*
228100080526      * ROUTINE PER INVIARE MAIL SE ERRORE CODICE SPC
228200080526      *------------------------------------------------------------------------*
228300080526     c     sr_mail       begsr
228400080526
228500080526      * Reperimento tab. "MRA" se non già recuperata
228600080526     c                   if        not $okmra
228700080526     c                   clear                   dmradan
228800080526     c                   clear                   tibs02ds
228900080526     c                   eval      t02mod = 'C'
229000080526     c                   eval      t02sif = knsif
229100080526     c                   eval      t02cod = 'MRA'
229200080526     c                   eval      t02ke1 = 'FIOR90R'
229300080526     c                   call      'TIBS02R'
229400080526     c                   parm                    kpjba
229500080526     c                   parm                    tibs02ds
229600080526     c                   if        t02err = *blanks
229700080526     c                   eval      dmradan = t02uni
229800080526     c                   eval      $okmra = *on
229900080526     c                   endif
230000080526     c                   endif
230100080526
230200080526      * Override al file di stampa ed apertura dello stesso
230300080526     c                   if        not %open(prtemail)
230400080526     c                   exsr      sr_openprtf
230500080526     c                   endif
230600080526
230700080526     c                   eval      o_testo = 'Fase RQE non scritta per la +
230800080526     c                                        distinta ' +
230900080526     c                                        %editc(v1cpor:'X') + '-' +
231000080526     c                                        %editc(skndc(yy):'X') +
231100080526     c                                        ' Fior26r'
231200080526     c                   except    prtdet
231300080526
231400080526     c                   endsr
231500080526
231600080526      *------------------------------------------------------------------------*
231700080526      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
231800080526      *  + STAMPA INIZIO MAIL
231900080526      *------------------------------------------------------------------------*
232000080526     c     sr_openprtf   begsr
232100080526
232200080526      * Override al file di stampa
232300080526     c                   exsr      sr_override
232400080526
232500080526     c                   open      prtemail
232600080526
232700080526      * Stampa una testata se NON è richiesta la e-mail
232800080526     c                   if        §mradreg = *blanks
232900080526     c                   eval      o_testo = knmus + ' - ' + 'FIOR90R'
233000080526     c                             + ' - ' + %editc( *date : 'Y' )
233100080526     c                             + ' - *REM* ' + %subst(§cm1var : 7 : 70)
233200080526     c                   except    prtdet
233300080526     c                   clear                   o_testo
233400080526     c                   except    prtdet
233500080526     c                   except    prtdet
233600080526     c                   endif
233700080526
233800080526      * Stampa testo iniziale
233900080526     c                   eval      o_testo = 'Fase RQE non scritta'
234000080526     c                   except    prtdet
234100080526
234200080526      * Stampa una riga vuota
234300080526     c                   clear                   o_testo
234400080526     c                   except    prtdet
234500080526
234600080526     c                   endsr
234700080526
234800080526      *------------------------------------------------------------------------*
234900080526      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
235000080526      *  + STAMPA INIZIO MAIL
235100080526      *------------------------------------------------------------------------*
235200080526     c     sr_override   begsr
235300080526
235400080526     c                   reset                   $invio
235500080526     c                   reset                   trtcm1ds
235600080526
235700080526     c                   if        §mradreg <> *blanks
235800080526     c                   eval      §cm1mitt = %trim(§mradmitt)
235900080526     c                   eval      §cm1dst  = %trim(§mraddest)
236000080526     c                   eval      §cm1tips = §mradreg
236100080526     c                   eval      §cm1po   = c_sede
236200080526     c                   eval      §cm1var  = '*OBJM*' + §mraddes
236300080526     c                   eval      §cm1idp  = §mradidpro
236400080526     c                   eval      qcmd = c_cmdovrprtf
236500080526     c                             + ' outq(' + %trim(§mradoutqi) + ')'
236600080526     c                             + ' usrdfndta(''' + trtcm1ds + ''')'
236700080526     c                   else
236800080526     c                   eval      qcmd = c_cmdovrprtf
236900080526     c                   endif
237000080526
237100080526     c                   eval      lenght = %size(qcmd)
237200080526     c                   call(e)   'QCMDEXC'
237300080526     c                   parm                    qcmd
237400080526     c                   parm                    lenght
237500080526
237600080526     c                   if        §mradreg <> *blanks and not %error
237700080526     c                   eval      $invio = *on
237800080526     c                   endif
237900080526
238000080526     c                   endsr
238100080526
238200080526      *------------------------------------------------------------------------*
238300080526      * ROUTINE FINALE
238400080526      *------------------------------------------------------------------------*
238500080526     c     sr_end        begsr
238600080526
238700080526     c                   if        %open(prtemail)
238800080526
238900080526      * Chiusura dello spool?
239000080526     c                   clear                   o_testo
239100080526     c                   except    prtdet
239200080526     c                   eval      o_testo = '***   Fine Lista   ***'
239300080526     c                   except    prtdet
239400080526
239500080526     c                   close     prtemail
239600080526
239700080526      * Eliminazione overflow?
239800080526     c                   eval      lenght = %size(qcmd)
239900080526     c                   eval      qcmd = c_cmddltovr
240000080526     c                   call(e)   'QCMDEXC'
240100080526     c                   parm                    qcmd
240200080526     c                   parm                    lenght
240300080526
240400080526     c                   endif
240500080526
240600080526     c                   endsr
240700080526
240800070207
240900010206      *****************************************************************
241000010206      * ROUTINE INIZIALE
241100010206      *****************************************************************
241200010206     C     *INZSR        BEGSR
241300010206
241400010206     C     *ENTRY        PLIST
241500010206     C                   PARM                    KPJBA
241600010206
241700010206     C                   eval      vtctit = '*  Quadratura Ordini '
241800010206     C                             + 'Ritiri Merce  *'
241900010206
242000080723     c     *dtaara       define    §azute        azuteds
242100080723     c     *dtaara       define    §datiute      ddatiute
242200080723     c                   in(e)     *dtaara
242300080723     c                   if        %error or rsut = *blanks
242400080723     c                   clear                   tibs34ds
242500080723     c                   call      'TIBS34R'
242600080723     c                   parm                    tibs34ds
242700080723     c                   in        *dtaara
242800080723     c                   endif
242900130620
243000130620      * controllo abilitazione utente
243100130620     c                   clear                   wabi
243200130620     c                   clear                   dLAT
243300130620     c                   eval      dUTE01 = UTEfaf
243400130620
243500130620      * Verifica errori e autorità profilo
243600130620     c                   SELECT
243700130620      * se ho errori nei dati utente esco dal pgm
243800130620     c                   WHEN      DUTerr = 'E'
243900130620     c                   eval      wErrGrave = *on
244000130620     c                   leavesr
244100130620      * carica abilitazioni del profilo
244200130620     c                   WHEN      §UTEorm <> *blanks
244300130620     c                   eval      wabi = §UTEorm
244400130620     c                   WHEN      UTEaut <> *blanks
244500130620     c                   eval      wabi = UTEaut
244600130620      * se non c'è l'abilitazione
244700130620      * --> se 1° livello, abilitazioni al terminal
244800130620      *     se 2° livello, abilitazioni al punto operativo
244900130620      *     se sede no abilitazioni
245000130620     c                   OTHER
245100130620     c                   IF        DUTlpo = '1'
245200130620     c                   eval      wabi   = 'TP'
245300130620     c                   ENDIF
245400130620     c                   IF        DUTlpo = '2'
245500130620     c                   eval      wabi   = 'PO'
245600130620     c                   ENDIF
245700130620     c                   ENDSL
245800130620
245900130620      * controllo se ok l'abilitazione dell'utente
246000130620     c                   clear                   TIBS02DS
246100130620     c                   eval      T02mod = 'C'
246200130620     c                   eval      T02sif = knsif
246300130620     c                   eval      T02cod = 'LAT'
246400130620     c                   eval      T02ke1 = wabi
246500130620     c                   Call      'TIBS02R'
246600130620     c                   Parm                    KPJBA
246700130620     c                   Parm                    TIBS02DS
246800130620     c                   IF        T02err <> *blanks or §LATabi = 'S'
246900130620     c                   eval      wErrGrave = *on
247000130620     c                   leavesr
247100130620     c                   ENDIF
247200130620     c                   eval      dLAT = T02uni
247300130620
247400130620      * Reperimento delle filiali gestibili dall'utente
247500130620     c                   clear                   TRUL31DS
247600130620     c                   eval      I31abi = wabi
247700130620     c                   eval      I31cdi = DUTdis
247800130620     c                   eval      I31car = DUTare
247900130620     c                   eval      I31cpo = DUTpou
248000130620     c                   call      'TRUL31R'
248100130620     c                   parm                    KPJBA
248200130620     c                   parm                    TRUL31DS
248300130620     c                   IF        O31pog > *zeros
248400130620     c                   movea     O31pog        SKpog
248500130620     c                   ELSE
248600130620     c                   eval      wErrGrave = *on
248700130620     c                   leavesr
248800130620     c                   ENDIF
248900010206
249000010206      * IMPOSTO IL P.O. DI GESTIONE
249100130620     C*****              if        dutlpo = '2' or dutlpo = *blanks
249200080723     C                   movel     dutpou        V1Cpor
249300130620     C*****              eval      *in06 = *on
249400130620     C*****              else
249500130620     C*****              movel     SIMFEL        V1Cpor
249600130620     C*****              endif
249700010206      * Carico schiera P.O. gestiti
249800130620     C*****              CLEAR                   TRUL06ds
249900130620     C*****              MOVE      '£1'          D06COD
250000130620     C*****              MOVEL     SIMFEL        D06KEY
250100130620     C*****              MOVEL     TRUL06DS      KPJBU
250200130620     C*****              CALL      'TRUL06R'
250300130620     C*****              PARM                    KPJBA
250400130620     C*****              MOVEL     KPJBU         TRUL06DS
250500130620     C*****              MOVEA     LIN           L1
250600010206      * Carico £6
250700010206     C                   CLEAR                   trul06ds
250800010206     C                   MOVE      '£6'          D06COD
250900010206     C                   MOVEL     v1cpor        D06KEY
251000010206     C                   MOVEL     trul06ds      KPJBU
251100010206     C                   CALL      'TRUL06R'
251200010206     C                   PARM                    KPJBA
251300010206     C                   MOVEL     KPJBU         trul06ds
251400010206     C                   MOVEA     LIN           L6
251500010206
251600010206      * Decodifico P.o. gestione
251700010206     C     v1cpor        chain     azorg01L
251800010206     C                   if        %found(azorg01l)
251900010206     C                   movel     orgDES        v1dpor
252000070125     c                   eval      og148 = orgde8
252100070926     c                   eval      og147 = orgde7
252200010206     C                   else
252300010206     C                   clear                   v1dpor
252400070125     c                   clear                   og148
252500070926     c                   clear                   og147
252600010206     C                   endif
252700010206
252800010206      * reperisco data e ora
252900010206     C                   TIME                    W0140
253000010206      * UDATE IN GGMMAAAA
253100010206     C                   MOVE      W0140         WDTGIO
253200010206      * UDATE IN AAAAMMGG
253300010206     C                   Z-ADD     WDTGIO        G02DAT
253400010206     C                   MOVEL     *BLANK        G02ERR
253500010206     C                   CALL      'XSRDA8'
253600010206     C                   PARM                    WLBDAT
253700010206     C                   MOVEL     G02INV        DATEU
253800071026
253900071026      * p.o. gestione con abilitazione scarico dati per PDA
254000071026     c                   eval      *in01 = (§ogpdaorm = 'S')
254100071026      * data partenza procedura in organigramma
254200071026     c                   eval      *in07 = *off
254300071026     c                   if        §ogddao > *zeros and §ogcgio = 'S'
254400071026     c                   move      §ogddao       data_org
254500071026      * controllo se attiva
254600071026     c                   if        dateu >= data_org
254700071026     c                   eval      *in07 = *on
254800071026     c                   endif
254900071026     c                   endif
255000010206
255100120613     C     kfidst        klist
255200120613     C                   kfld                    kdstnpg
255300120613     C                   kfld                    kdstnfv
255400120613     C                   kfld                    kdstfgs
255500010206
255600010206     C     kfnorm        klist
255700060516     C                   kfld                    keypor
255800010206     C                   kfld                    keyndc
255900010206
256000010206     C     kfnorm1       klist
256100010206     C                   kfld                    vs1poe
256200010206     C                   kfld                    vs1nsr
256300010206     C                   kfld                    vs1nor
256400010206     C                   kfld                    vs1nrv
256500010208
256600060214     C     kfiar4        klist
256700060214     C                   kfld                    Kar4trc
256800060214     C                   kfld                    kar4n14
256900060214     C                   movel     'M'           kar4trc
257000021202
257100021202     C     Kfapd         klist
257200021202     C                   kfld                    kpdtip
257300021202     C                   kfld                    kpdpdr
257400040406
257500040406     c     kOrf          klist
257600040406     c                   kfld                    kOrfPoe
257700040406     c                   kfld                    kOrfNsr
257800040406     c                   kfld                    kOrfNor
257900040406     c                   kfld                    kOrfNrv
258000040406     c                   kfld                    kOrfDae
258100040406     c                   kfld                    kOrfOre
258200040406     c                   kfld                    kOrfFar
258300041012
258400041012     c     kQdt01        klist
258500120613     c                   kfld                    kdstfgs
258600041012     c                   kfld                    kqdtnfv
258700080424     c                   kfld                    kqdtddc
258800080424     c                   kfld                    kqdtpdr
258900050225
259000050225     c     Karb78        Klist
259100050225     c                   Kfld                    Karbifp
259200050225     c                   Kfld                    Karbndc
259300070206
259400070206     c     kfiplg01      klist
259500070206     c                   Kfld                    ktiporcd
259600070206     c                   kfld                    keypor
259700070206     c                   kfld                    keyndc
259800070215     c                   Kfld                    ktipappl
259900070206     c                   kfld                    kiddoc
260000010206
260100010206     C                   endsr
260200080526
260300080526     oprtemail  e            prtdet      1
260400080526     o                       o_testo
260500010206      *---------------------------------------------------------------------------------------------
260600001113** MSG
260700070418Filiale gestione errata                                                              1
260800010206Numero Distinta Errato                                                               2
260900010206Distinta non aperta                                                                  3
261000010206Immettere Numero Distinta                                                            4
261100010206Causale Errata                                                                       5
261200010206ORM allocato da altro utente                                                         6
261300010628L'ORM è stato bollettato: non si può dichiarare NON FATTO                            7
261400010305Causale non gestibile                                                                8
261500010529ORM in fase errata per dichiararlo NON FATTO                                         9
261600011015ORM già chiuso non è possibile richiuderlo                                          10
261700020524ATTENZIONE!! L'ORM è in fase 900. Non si può quadrare                               11
261800070125Effettuare un'unica scelta: o tutte le distinte o una distinta                      12
261900070206La distinta deve essere prima chiusa sul PDA, poi può essere quadrata               13
262000070207Password errata                                                                     14
262100070207Immettere la password                                                               15
262200070212Manca tabella di controllo password. TEL CED SEDE !!!!!                             16
262300070212Manca tabella di controllo scadenza password. TEL CED SEDE !!!!!                    17
262400070212Password scaduta                                                                    18
262500081030ORM Commissionato. Non si può usare più di 1 causale con resp. al cliente           19
262600090402Distinta già quadrata                                                               20
262700020523** CM3
262800030307OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
262900030307 SHARE(*YES)
263000030623** cmA4
263100030623OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
263200030623 SHARE(*YES)          USRDTA('BolAdd_A4')
263300030623**  cmA5
263400030623OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
263500030623 SHARE(*YES)          USRDTA('BolAdd_A5')
