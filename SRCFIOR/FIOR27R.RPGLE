000100001113     H DECEDIT('0,') DATEDIT(*DMY.)
000200010206      ****************************************************************
000300010206      *                                                              *
000400010530      *        MODIFICA FASE  O R M                                  *
000500010206      *                                                              *
000600010206      ****************************************************************
000700010206      *  RIEPILOGO INDICATORI
000800010206      ****************************************************************
000900010206      * 05 - SPROTEGGO P.O. RITIRO
001000010206      * 06 - NON ABILITO TASTO FUNZIONALE F18
001100070926      * 07 - P.o. gestione abilitato alla procedura automazione ORM
001200010605      * 10 - FASE 500
001300010605      * 11 - FASE 600
001400030901      * 12 - FASE 999 o 910
001500010629      * 13 - REVERSE IMAGE X FASE
001600010629      * 15 - REVERSE IMAGE X CAUSALE
001700041015      * 16 - FASE 999 o 910  con cau non gestibile da quadratura ORM
001800010206      * 30 - Comodo
001900010605      * 31 - Comodo
002000010206      * 40 - ERRORE P.o. ritiro
002100010625      * 44 - ORM allocato
002200020524      * 45 - msg info per annullamento bolla addebito
002300020524      * 46 - ORM in fase 900 al momento della modifica
002400010706      * 47 - POSIZIONAMENTO CURSORE SU V2CLAV
002500010625      * 48 - POSIZIONAMENTO CURSORE SU V2CNDC2
002600010625      * 90 - riemissione videata
002700010206      *--------------------------------------------------------------*
002800001113     FAZORG01L  IF   E           K DISK
002900041015     FFNORF01l  iF A E           K DISK
003000010206     FFNORM01L  UF   E           K DISK
003100071011     ffnorg01l  uf   e           k disk
003200021202     FFIAPD01L  IF   E           K DISK
003300060214     FFIAR404L  IF   E           K DISK
003400020702     ffnacr01l  if   e           k disk
003500120613     ffidst01l  if   e           k disk
003600080423     ffidpo01l  uf   e           k disk
003700010530     FFIOR27D   CF   E             WORKSTN
003800010206      *------------------------------------------------------------------------*
003900010206     D §kpjbu          S                   LIKE(kpjbu)
004000010706     D WCAUSA          S                   LIKE(V2CLAV)
004100010606     D WDESCAU         S                   LIKE(V2DCAU2)
004200010605     D WDAO            S                   LIKE(ORMDAO)
004300010605
004400120613     d kdstnpg         s                   like(dstnpg)
004500120613     d kdstnfv         s                   like(dstnfv)
004600120613     d kdstfgs         s                   like(dstfgs)
004700010605
004800060214     D kar4trc         S                   LIKE(ar4trc)
004900060214     D kar4n14         S                   LIKE(ar4n14)
005000021202
005100021202     D kpdpdr          S                   LIKE(APDpdr)
005200021202     D kpdtip          S                   LIKE(APDtip)
005300020524
005400020524     d sav_lpksc       s                   like(ormksc)
005500020524     d sav_cmradd      s                   like(d§cmradd)
005600020524     d new_cmradd      s                   like(d§cmradd)
005700040406     d sav_OrfFar      s                   like(OrfFar)
005800040406     d sav_CmrCau      s                   like(OrfCar)
005900040406     d sav_OrMFao      s                   like(OrMFao)
006000040909     d savndc          s                   like(OrmNdc)
006100040909     d savddc          s                   like(OrmDdc)
006200060516     d savfgs          s                   like(orffgs)
006300020524
006400020524     d contas          s              2  0
006500020524     d okbolla         s              1
006600020524     d nobolla         s              1    inz('0')
006700081125     d wnobolla        s              1    inz('0')
006800020524     d no400           s              1    inz('0')
006900040406     d wtenta          s              3  0
007000040406     d wconta          s              3  0
007100040406     d wchiudi         s              1    inz('0')
007200020524
007300020524     D lung            S             15  5
007400020524     D comman          S            110
007500010605
007600010206     D flgkf           S              1
007700010206     D W0140           S             14  0
007800010206     D Wdtgio          S              8  0
007900010206     D dateu           S              8  0
008000070926     d data_org        s              8  0 inz
008100080423     d $comdpd         s               n
008200130620
008300130620     d wabi            s                   like(UTEaut)
008400130620     d wEsci           s               n   inz(*off)
008500130620     d wErrGrave       s               n   inz(*off)
008600130620     d w003a           s              3
008700010206
008800010206      *   S C H I E R E
008900081030     D MSG             S             78    DIM(25) CTDATA PERRCD(1)             MSG VIDEO
009000010706     D LAV             S             26    DIM(03) CTDATA PERRCD(1)
009100130620     D**** L1              S              3  0 DIM(30)                              P.O. Gestiti £1
009200001113     D L6              S              3  0 DIM(30)                              P.O. Gestiti £6
009300130620     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
009400030307     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
009500030623      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
009600030623     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
009700030623     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
009800010119
009900010206      *   D S   I N T E R N E / E S T E R N E
010000010206     D WLBDAT          DS                  INZ
010100010206     D  G02DAT                 1      8  0
010200010206     D  G02INV                 9     16  0
010300010206     D  G02ERR                17     17
010400010206     D  G02TGI                18     22  0
010500020524
010600020524     D                 DS
010700020524     d  ormcra1                1      3  0
010800020524     d  ormcra2                4      7  0
010900020524     d  ormcra3                8     10  0
011000020524     d  ds_ormcra              1     10  0
011100020524
011200020524     D                 DS
011300020524     d  ormcor1                1      3  0
011400020524     d  ormcor2                4      7  0
011500020524     d  ormcor3                8     10  0
011600020524     d  ds_ormcor              1     10  0
011700020524
011800020524     D                 DS
011900020524     d  ormcrc1                1      3  0
012000020524     d  ormcrc2                4      7  0
012100020524     d  ormcrc3                8     10  0
012200020524     d  ds_ormcrc              1     10  0
012300020524
012400020524     D                 DS
012500020524     D  ds_poe                 1      3  0
012600020524     D  ds_nsr                 4      5  0
012700020524     D  ds_nor                 6     12  0
012800020524     D  ds_nrv                13     14  0
012900020524     D  ds_keyorm              1     14  0
013000010206
013100001116     D Parm01          DS
013200001116     D  ppoe                          3  0
013300001116     D  pnor                          7  0
013400001116     D  pnsr                          2  0
013500001116     D  pnrv                          2  0
013600001116     D  psce                          1
013700001116     D  pfgs                          3  0
013800001116     D  ppor                          3  0
013900001116     D  pdtr                          8  0
014000001116     D  pmdb                         10
014100001116     D  pprb                         10
014200001116     D  pdts                          8  0
014300001116     D  prmp                          1
014400010130     D  pbrc                          1
014500010130     D  pref                          2
014600040406     D  pmio                          1
014700041015     d  pndc                          6  0
014800041015     d  pddc                          8  0
014900120613     d  pdstfgs                       3  0
015000001113
015100020524     D DTASV         E DS
015200020524     D DBLP          E DS
015300030623     D TRUL90DS      E DS
015400010605     D DFAR          E DS
015500010301     D DCMR          E DS
015600081030     d a_dcmr        e ds                  extname(dcmr) prefix(a_)
015700010530     D FIOR06DS      E DS
015800080319     d fior06vds     e ds
015900010206     D TIBS02DS      E DS
016000010206     D DSLR01        E DS                  inz
016100010206     D TRUL06DS      E DS
016200001113     D  LIN                    1     90  0
016300001113     D                                     DIM(30)
016400040406     d Fior05ds      e ds
016500060516     d fnlv55ds      e ds
016600080423     d og143         e ds
016700070926     d og147         e ds
016800080314     d dorg01        e ds
016900081030     d dorm01        e ds
017000081125     d dorf01        e ds
017100130620
017200130620     d azuteds       e ds                  extname(azute00f)
017300130620     d ddatiute      e ds
017400130620     d tibs34ds      e ds
017500130620     d TRUL31DS      e ds
017600130620     d dLAT          e ds
017700130620     d dUTE01        e ds
017800010206
017900010206     D KPJBA         E DS
018000010206
018100010206     D                SDS
018200010206     D  VTCPGM                 1     10
018300001113
018400010605      * Scomposizione Descrizione Distinta
018500010605     D                 DS
018600010605     D  v2DNDC                 1     40
018700010605     D  wdel                   1      4
018800010605     D  wggg                   5      6
018900010605     D  wbarra1                7      7
019000010605     D  wmmm                   8      9
019100010605     D  wbarra2               10     10
019200010605     D  waaa                  11     15
019300010605     D  wpdr                  16     23
019400010605     D  wdescr                24     40
019500010625      * Scomposizione Descrizione Distinta
019600010625     D                 DS
019700010625     D  v2DNDC2                1     40
019800010625     D  wdel1                  1      4
019900010625     D  wggg1                  5      6
020000010625     D  wbarra3                7      7
020100010625     D  wmmm1                  8      9
020200010625     D  wbarra4               10     10
020300010625     D  waaa1                 11     15
020400010625     D  wpdr1                 16     23
020500010625     D  wdescr1               24     40
020600010206      *------------------------------------------------------------------------*
020700010530      * Emissione Videata
020800010206
020900010206     C                   do        *hival
021000010530      * Pulizia Videata
021100010604     C                   if        *in28 = *off
021200010530     C                   exsr      Sr_Pulvid
021300010206     C                   endif
021400130620
021500130620      /free
021600130620       //?Se utente non autorizzato alla funzione esco
021700130620         IF  wErrGrave;
021800130620           *in28 = *on;
021900130620           wEsci = *on;
022000130620           V1Cmsg = 'Utente non autorizzato alla funzione';
022100130620         ENDIF;
022200130620      /end-free
022300010206
022400010604     C                   exfmt     OR27D01
022500130620      /free
022600130620         IF  wEsci;
022700130620           leave;
022800130620         ENDIF;
022900130620      /end-free
023000130620
023100010206      * F3=Fine
023200010206     C   kc              leave
023300010206
023400010604     C                   setoff                                       28
023500010206      * F18=Cambio P.O. in gestione
023600010206     C   ks              eval      *in05 = *on
023700010206     C   ks              iter
023800010206
023900010206      * 05 on --> controllo P.O. in gestione
024000010206     C                   if        *in05 = *on
024100010604     C                   exsr      Sr_Contrfgs
024200130620     C  n28              eval      *in05 = *off
024300080423     C   28              iter
024400010206     C                   endif
024500010530      * Controlli videata
024600010625     C                   setoff                                       2841
024700010625     C                   exsr      Sr_Controrm
024800010206
024900010206     C   28              iter
025000010604      *--------------
025100010605     C                   SETOFF                                       101112
025200041015     C                   SETOFF                                       16
025300020529     c                   eval      *in45 = *off
025400010604     C                   EXSR      SR_CAR2VID
025500010605     C     EMETTO        TAG
025600010605     C                   EXFMT     OR27D02
025700010629     C*
025800020529     C                   SETOFF                                       28
025900010605      * F3=Fine
026000010605     C   kc              leave
026100010605
026200010605      * F12=Ritorno
026300010605     C   kl              iter
026400010605
026500010629      * F5=Visualizza ORM
026600010629     C                   if        *inke = *on and v1cnor <> 0
026700010629     C                   exsr      Sr_Orm
026800010709     C                   GOTO      EMETTO
026900010629     C                   endif
027000010629
027100010605     C                   EXSR      SR_CONTR2
027200010625     C                   IF        *IN28 = *ON  OR *IN90 = *ON
027300010605     C                   GOTO      EMETTO
027400010605     C                   ENDIF
027500010604      *--------------
027600020524      * F6=Conferma
027700010605     C                   IF        *INKF = *ON
027800010605     C                   exsr      Sr_Conferma
027900020524      * ORM allocato o ORM già in fase 900 al momento della modifica
028000020524     C                   if        *in44  or  *in46
028100020524     c                   seton                                        28  47
028200010605     C                   goto      EMETTO
028300010605     C                   endif
028400020529      * se non faccio la bolla riemetto la videata
028500020529     c                   if        okbolla = '1'
028600020529     c                   goto      emetto
028700020529     c                   endif
028800010605      * Se la causale prevede le note queste sono obbligatorie
028900010605     C                   EXSR      SR_CONTRNOTE
029000040406      * se ho quadrato con causale controllo quanti tentativi posso fare prima di chiudere
029100040406      * l'ORM in automatico
029200040413     c                   If        wcausa <> *Blanks and d§cmrnca <> *Zeros
029300040406     c                   ExSr      Sr_ContrFase
029400040406     c                   If        wchiudi = *On
029500040406     c                   ExSr      Sr_Chiudi
029600040406     c                   EndIf
029700040406     c                   EndIf
029800040406
029900010605     C                   ELSE
030000010605     C                   GOTO      EMETTO
030100010605     C                   ENDIF
030200010605
030300010605     C                   enddo
030400010605
030500010206     C                   eval      *inlr = *on
030600010206      *****************************************************************
030700010530      * PULIZIA VIDEATA
030800010206      *****************************************************************
030900010530     C     Sr_Pulvid     BEGSR
031000010206
031100010604     C                   clear                   v1cpoe
031200010604     C                   clear                   v1cnsr
031300010604     C                   clear                   v1cnor
031400010604     C                   clear                   v1cnrv
031500010206
031600010206     C                   endsr
031700010206      **********************************************************************
031800010206      * CONTROLLO IL P.O. DI RITIRO
031900010206      **********************************************************************
032000010604     C     Sr_Contrfgs   begsr
032100010206
032200010604     C                   setoff                                       40
032300010604     C                   clear                   v1dfgs
032400070926     c                   clear                   og147
032500010206
032600130620     C**** v1cfgs        lookup    L1                                     30
032700130620     c                   move      V1Cfgs        w003a
032800130620     c     w003a         lookup    SKpog                                  30
032900010206     C     *in30         ifeq      *off
033000010206     C                   seton                                        40  28
033100010206     C                   movel     msg(1)        v1cmsg
033200010206     C                   else
033300010206      * Decodifica
033400010604     C     v1cfgs        chain     azorg01L
033500010206     C                   if        %found(azorg01l) and ORGfva = *blanks
033600010604     C                   movel     orgDES        v1dfgs
033700070926     c                   eval      og147 = orgde7
033800010206     C                   else
033900010206     C                   seton                                        40  28
034000010206     C                   movel     msg(1)        v1cmsg
034100010206     C                   endif
034200010207      * Devo ricaricare la £6
034300010207     C                   CLEAR                   trul06ds
034400010207     C                   MOVE      '£6'          D06COD
034500010604     C                   MOVEL     v1cfgs        D06KEY
034600010207     C                   MOVEL     trul06ds      KPJBU
034700010207     C                   CALL      'TRUL06R'
034800010207     C                   PARM                    KPJBA
034900010207     C                   MOVEL     KPJBU         trul06ds
035000010207     C                   MOVEA     LIN           L6
035100010206     C                   endif
035200070926      * data partenza procedura in organigramma
035300071026     c                   eval      *in07 = *off
035400071026     c                   if        §ogddao > *zeros and §ogcgio = 'S'
035500070926     c                   move      §ogddao       data_org
035600071026      * controllo se attiva
035700071026     c                   if        dateu >= data_org
035800071026     c                   eval      *in07 = *on
035900071026     c                   endif
036000071026     c                   endif
036100010206
036200010206     C                   endsr
036300010206      **********************************************************************
036400010530      * CONTROLLO   * Numero ORM *
036500010206      **********************************************************************
036600010530     C     Sr_Controrm   begsr
036700010206
036800070516     C     KFNORM        CHAIN(e)  FNORM01L                           31
036900070516     c                   if        %error
037000070516     c                   eval      *in28 = *on
037100070516     c                   eval      *in41 = *on
037200070516     c                   eval      v1cmsg = msg(22)
037300070516     c                   leavesr
037400070516     c                   endif
037500010604     C                   IF        *IN31 = *ON
037600010604     C                   SETON                                        2841
037700010604     C                   MOVE      MSG(2)        V1CMSG
037800010604     C                   GOTO      ENDSRCORM
037900010604     C                   ENDIF
038000010206
038100010604     C                   IF        ORMATB <> *BLANKS
038200010604     C                   SETON                                        2841
038300010604     C                   MOVE      MSG(3)        V1CMSG
038400010604     C                   GOTO      ENDSRCORM
038500010604     C                   ENDIF
038600010604
038700010604     C                   IF        ORMPOR <> V1CFGS
038800060516     c     ormpor        lookup    l6                                     30
038900060516     c                   if        not *in30
039000010604     C                   SETON                                        2841
039100010604     C                   MOVE      MSG(4)        V1CMSG
039200010604     C                   GOTO      ENDSRCORM
039300060516     c                   endif
039400010604     C                   ENDIF
039500010604
039600010604     C                   IF        ORMFAO < 500
039700010604     C                   SETON                                        2841
039800010604     C                   MOVE      MSG(5)        V1CMSG
039900010604     C                   GOTO      ENDSRCORM
040000010604     C                   ENDIF
040100010706
040200010706     C                   IF        ORMFAO <> 500 AND ORMFAO <> 600 AND
040300030901     C                             ORMFAO <> 999 and OrmFao <> 910
040400010706     C                   SETON                                        2841
040500010706     C                   MOVE      MSG(5)        V1CMSG
040600010706     C                   GOTO      ENDSRCORM
040700010706     C                   ENDIF
040800081030
040900081030     c                   eval      dorm01 = ormflo
041000010604
041100010530     C     endsrcorm     endsr
041200010604      *****************************************************************
041300010604      * CARICO I DATI DELLA SECONDA VIDEATA
041400010604      *****************************************************************
041500010604     C     SR_CAR2VID    BEGSR
041600010604
041700041015     c                   Clear                   savndc
041800041015     c                   Clear                   savddc
041900060516     c                   Clear                   savfgs
042000041015
042100010706     C                   CLEAR                   V2DNDC
042200010606     C                   CLEAR                   V2CCAU1
042300010606     C                   CLEAR                   V2DCAU1
042400010706     C                   CLEAR                   V2CLAV
042500010606     C                   CLEAR                   V2DCAU2
042600010702     C                   CLEAR                   V2CNDC2
042700010702     C                   CLEAR                   V2DNDC2
042800010604     C                   EVAL      V2CFGS = V1CFGS
042900010604     C                   EVAL      V2DFGS = V1DFGS
043000010604     C                   EVAL      V2CPOE = V1CPOE
043100010604     C                   EVAL      V2CNSR = V1CNSR
043200010604     C                   EVAL      V2CNOR = V1CNOR
043300010604     C                   EVAL      V2CNRV = V1CNRV
043400010604     C                   EVAL      V2RSR = ORMRSR
043500010604     C                   EVAL      V2INR = ORMINR
043600010604     C                   EVAL      V2CAR = ORMCAR
043700010604     C                   EVAL      V2LOR = ORMLOR
043800010604     C                   EVAL      V2PRR = ORMPRR
043900010604     C                   EVAL      V2NAR = ORMNAR
044000010604     C                   EVAL      V2FAO = ORMFAO
044100010604     C                   EVAL      V2ORR = ORMORR
044200010605     C                   EVAL      V2CPOR = ORMPOR
044300010604     C                   EVAL      V2CNDC = ORMNDC
044400060516     c                   eval      v2hfgs = ormpor
044500010709     C                   EVAL      WDAO = ORMDAO
044600010629     C                   EVAL      V2CTOR = ORMTOR
044700010709     C*
044800010709      * MITTENTE
044900010709     C                   MOVEL     ORMCRA        V2CRA1
045000010709     C                   MOVE      ORMCRA        WCRA              7 0
045100010709     C                   MOVEL     WCRA          V2CRA2
045200010709     C                   MOVE      WCRA          V2CRA3
045300010706     C*
045400010706      *DATA RITIRO
045500010706     C                   EVAL      G02INV = ORMDAR
045600010706     C                   MOVEL     '3'           G02ERR
045700010706     C                   CALL      'XSRDA8'
045800010706     C                   PARM                    WLBDAT
045900010706     C                   EVAL      V2DAR = G02DAT
046000010706     C*
046100010706      *DATA IMMISSIONE
046200010706     C                   EVAL      G02INV = ORMDAO
046300010706     C                   MOVEL     '3'           G02ERR
046400010706     C                   CALL      'XSRDA8'
046500010706     C                   PARM                    WLBDAT
046600010706     C                   EVAL      V2DAO = G02DAT
046700010629     C*
046800010604      * CAUSALE
046900010605     C                   IF        ORMFAO = 500 OR ORMFAO = 999
047000030901     c                             or OrmFao = 910
047100041015     C     KFNORF        CHAIN     FNORF01L                           31
047200010605     C                   IF        *IN31 = *OFF
047300010605     C                   MOVEL     ORFCAR        V2CCAU
047400041015     c                   Eval      v2cndc = OrfNdc
047500060516     c                   Eval      v2hfgs = orffgs
047600010709     C                   ELSE
047700010709     C                   CLEAR                   V2CCAU
047800010709     C                   CLEAR                   V2DCAU
047900041015     c                   Clear                   v2cndc
048000060516     c                   clear                   v2hfgs
048100010605     C                   ENDIF
048200010709     C                   ELSE
048300010709     C                   CLEAR                   V2CCAU
048400010709     C                   CLEAR                   V2DCAU
048500010605     C                   ENDIF
048600010629     C*
048700010629     C                   IF        V2FAO <> *ZEROS
048800010629     C                   EVAL      *IN13 = *ON
048900010709     C                   ELSE
049000010709     C                   EVAL      *IN13 = *OFF
049100010629     C                   ENDIF
049200010629     C*
049300010629     C                   IF        V2CCAU <> *BLANKS
049400010629     C                   EVAL      *IN15 = *ON
049500010709     C                   ELSE
049600010709     C                   EVAL      *IN15 = *OFF
049700010629     C                   ENDIF
049800010604      * DECSRIZIONI
049900010605     C                   EXSR      SR_FASE
050000020524     c                   clear                   sav_cmradd
050100010605     C                   IF        V2CCAU <> *BLANKS
050200010605     C                   MOVEL     V2CCAU        WCAUSA
050300010605     C                   EXSR      SR_CAUSALE
050400010605     C                   movel     t02uni        dcmr
050500010605     C                   movel     D§CMRDES      V2DCAU
050600020524     c                   eval      sav_cmradd = d§cmradd
050700010605     C                   EVAL      *IN28 = *OFF
050800010605     C                   ENDIF
050900010605     C                   EXSR      SR_PORITIRO
051000010605     C                   EXSR      SR_DISTINTA
051100080423      * controllo se ORM commissionato da DPD
051200080423     c                   exsr      sr_comdpd
051300010605      * GESTIONE FASI
051400010629     C                   MOVE      *BLANKS       V2DLAV
051500010629     C                   MOVEL     *BLANKS       V2DCAU2
051600010629     C*
051700010605     C                   IF        V2FAO = 500
051800010605     C                   SETON                                        10
051900010629     C                   MOVEL     LAV(1)        V2DLAV
052000010629     C                   MOVEL     '(SI)'        V2DCAU2
052100010605     C                   ENDIF
052200010605
052300010605     C                   IF        V2FAO = 600
052400041015     C                   SETON                                        1116
052500010629     C                   MOVEL     LAV(2)        V2DLAV
052600010706     C                   MOVE      '?'           V2CLAV
052700010605     C                   ENDIF
052800010605
052900030901     C                   IF        V2FAO = 999 or V2Fao = 910
053000010605     C                   SETON                                        12
053100010706     C                   MOVEL     LAV(3)        V2DLAV
053200010706     C                   MOVEL     '(SI)'        V2DCAU2
053300041015     c                   if        d§cmrnoq <> *blanks
053400041015     C                   SETON                                        16
053500041015     c                   endif
053600010605     C                   ENDIF
053700010605
053800010605     C                   endsr
053900010605      *****************************************************************
054000010605      *    DESCRIZIONE FASE                                           *
054100010605      *****************************************************************
054200010605     C     SR_FASE       begsr
054300010605
054400010605     C                   CLEAR                   dfar
054500010605     C                   clear                   TIBS02DS
054600010605     C                   movel     'C'           T02mod
054700010605     C                   movel     knsif         t02sif
054800010605     C                   movel     'FAR'         t02cod
054900010605     C                   movel(p)  ormfao        T02ke1
055000010605     C                   call      'TIBS02R'
055100010605     C                   parm                    KPJBA
055200010605     C                   parm                    TIBS02DS
055300010605     C                   movel     T02uni        dfar
055400010605     C                   MOVEL     D§FARDES      V2DFAO
055500010605
055600010605     C                   endsr
055700010605      *****************************************************************
055800010605      *    DESCRIZIONE CAUSALE                                        *
055900010605      *****************************************************************
056000010605     C     Sr_CAUSALE    begsr
056100010605
056200010605     C                   clear                   dcmr
056300010605     C                   clear                   TIBS02DS
056400010605     C                   movel     'C'           T02mod
056500010605     C                   movel     knsif         t02sif
056600010605     C                   movel     'CMR'         t02cod
056700010605     C                   movel(p)  WCAUSA        T02ke1
056800010605     C                   call      'TIBS02R'
056900010605     C                   parm                    KPJBA
057000010605     C                   parm                    TIBS02DS
057100010605     C                   if        t02err <> *blanks
057200010605     C                   movel     msg(7)        v2CMSG
057300010605     C                   eval      *in28 = *on
057400010605     C                   endif
057500010605
057600010605     C                   ENDSR
057700010605      *****************************************************************
057800010605      *    DESCRIZIONE P.O. RITIRO                                    *
057900010605      *****************************************************************
058000010605     C     Sr_PORITIRO   begsr
058100010605
058200070926     c                   clear                   og147
058300010605     C     V1CFGS        CHAIN     AZORG01L                           31
058400010605     C                   IF        *IN31 = *OFF
058500010605     C                   MOVEL     ORGDES        V2DPOR
058600070926     c                   eval      og147 = orgde7
058700010605     C                   ENDIF
058800070926      * data partenza procedura in organigramma
058900071026     c                   eval      *in07 = *off
059000071026     c                   if        §ogddao > *zeros and §ogcgio = 'S'
059100070926     c                   move      §ogddao       data_org
059200071026      * controllo se attiva
059300071026     c                   if        dateu >= data_org
059400071026     c                   eval      *in07 = *on
059500071026     c                   endif
059600071026     c                   endif
059700010605
059800010605     C                   endsr
059900010605      *****************************************************************
060000010605      *    DESCRIZIONE DISTINTA                                       *
060100010605      *****************************************************************
060200010605     C     Sr_DISTINTA   begsr
060300010605
060400010605     C     V2CNDC        ifne      *ZEROS
060500120613     C                   z-add     4             kdstnpg
060600120613     C                   z-add     v2CNDC        kdstnfv
060700120613     C                   z-add     v2CFGS        kdstfgs
060800120613     C     kfidst        chain     fidst01l
060900120613     c                   if        not %found(fidst01l)
061000060516     c                   clear                   fnlv55ds
061100060516     c                   eval      d55tpt = '6'
061200060516     c                   move      v2cfgs        d55lin
061300060516     c                   eval      d55drf = dateu
061400060516     c                   call      'FNLV55R'
061500060516     c                   parm                    fnlv55ds
061600060516     c                   if        d55err = *blanks and d55tfa <> d55lin
061700120613     c                   move      d55tfa        kdstfgs
061800120613     c     kfidst        chain     fidst01l
061900060516     c                   endif
062000060516     c                   endif
062100120613     c                   if        %found(fidst01l)
062200010605     C                   EXSR      SR_DESCRIZ
062300120613     c                   eval      v2hfgs = kdstfgs
062400010605     C                   endif
062500010605     C                   endif
062600010605
062700010605     C                   endsr
062800010605      *****************************************************************
062900010605      *    Recupero Descrizione Distinta                              *
063000010605      *****************************************************************
063100010605
063200010605     C     SR_DESCRIZ    begsr
063300010605
063400021202     C                   movel     DSTpdr        kpdpdr
063500021202     C                   move      'A'           kpdtip
063600021202     C     kfapd         chain     fiapd01L                           31
063700010605     C                   if        *IN31 = *ON
063800010605     C                   clear                   apdrsc
063900010605     C                   ENDIF
064000010605
064100010605     C                   movel     'DEL '        wdel
064200120613     C                   move      dstdfv        wggg
064300010605     C                   move      '/'           wbarra1
064400010605     C                   move      '/'           wbarra2
064500120613     C                   move      dstdfv        wa4               4
064600010605     C                   movel     wa4           wmmm
064700120613     C                   movel     dstdfv        wa4               4
064800010605     C                   movel(p)  wa4           waaa
064900010605     C     dstpdr        ifne      0
065000010605     C                   movel(p)  dstpdr        wpdr
065100010605     C                   else
065200010605     C                   clear                   wpdr
065300010605     C                   endif
065400010625     C                   movel     apdrsc        wdescr
065500010605
065600010605     C                   endsr
065700010625      *****************************************************************
065800010625      *    Recupero Descrizione Distinta                              *
065900010625      *****************************************************************
066000010625     C     SR_DESCRIZ1   begsr
066100010625
066200021202     C                   movel     DSTpdr        kpdpdr
066300021202     C                   move      'A'           kpdtip
066400021202     C     kfapd         chain     fiapd01L                           31
066500010625     C                   if        *IN31 = *ON
066600010625     C                   clear                   apdrsc
066700010625     C                   ENDIF
066800010625
066900010625     C                   movel     'DEL '        wdel1
067000120613     C                   move      dstdfv        wggg1
067100010625     C                   move      '/'           wbarra3
067200010625     C                   move      '/'           wbarra4
067300120613     C                   move      dstdfv        wa41              4
067400010625     C                   movel     wa41          wmmm1
067500120613     C                   movel     dstdfv        wa41              4
067600010625     C                   movel(p)  wa41          waaa1
067700010625     C     dstpdr        ifne      0
067800010625     C                   movel(p)  dstpdr        wpdr1
067900010625     C                   else
068000010625     C                   clear                   wpdr1
068100010625     C                   endif
068200010625     C                   movel     apdrsc        wdescr1
068300010625
068400010625     C                   endsr
068500080423
068600080423      *--------------------------------------------------------------------*
068700080423      * Controllo se orm commissionato da DPD
068800080423      *--------------------------------------------------------------------*
068900080423     c     sr_comdpd     begsr
069000080423
069100080423     c                   clear                   og143
069200080423     c                   eval      $comdpd = *off
069300080423     c     ormpoe        chain     azorg01l
069400080423     c                   if        not %found(azorg01l)
069500080423     c                   leavesr
069600080423     c                   endif
069700080423     c                   eval      og143 = orgde3
069800080423     c                   if        §ogntw = 'DPD'
069900080423     c                   eval      $comdpd = *on
070000080423     c                   endif
070100080423
070200080423     c                   endsr
070300080423
070400010206      **********************************************************************
070500010206      * CONTROLLO  * Causale mancato ritiro *
070600010206      **********************************************************************
070700010605     C     Sr_Contrcau   BEGSR
070800010206
070900010605     C                   eval      *in28 = *off
071000010206
071100010605     C     '?'           scan      WCAUSA                                 30
071200010206     C     *in30         ifeq      *on
071300010301     C                   clear                   dcmr
071400010206     C                   clear                   TIBS02DS
071500010206     C                   movel     'R'           t02mod
071600010206     C                   movel     knsif         t02sif
071700010206     C                   movel     'CMR'         t02cod
071800010206     C                   call      'TIBS02R'
071900010206     C                   parm                    KPJBA
072000010206     C                   parm                    TIBS02DS
072100010605     C                   movel     T02ke1        WCAUSA
072200010206     C                   endif
072300010206      * Controllo Causale
072400010629     C                   IF        WCAUSA = *BLANKS
072500010629     C                   MOVEL     MSG(16)       V2CMSG
072600010629     C                   EVAL      *IN28 = *ON
072700010706     C                   CLEAR                   V2CCAU1
072800010629     C                   GOTO      ENDCAU
072900010629     C                   ENDIF
073000010629     C*
073100010706     C                   if        WCAUSA <> *blanks
073200010706     C                   EXSR      SR_CAUSALE
073300060215     c   28              goto      endcau
073400010706     C                   movel     t02ke1        WCAUSA
073500010706     C                   movel     t02uni        dcmr
073600010706     C                   MOVEL     D§CMRDES      WDESCAU
073700020524     c                   eval      new_cmradd = d§cmradd
073800040413     c                   Eval      wtenta = d§cmrnca
073900040413     c                   Move      d§cmrcca      sav_cmrcau
074000010706     C*
074100010606     C                   IF        *IN11 = *ON
074200010706     C                   EVAL      V2CLAV = WCAUSA
074300010606     C                   EVAL      V2DCAU2 = WDESCAU
074400010606     C                   ENDIF
074500010606
074600010606     C                   IF        *IN12 = *ON OR *IN10 = *ON
074700010606     C                   EVAL      V2CCAU1 = WCAUSA
074800010606     C                   EVAL      V2DCAU1 = WDESCAU
074900010606     C                   ENDIF
075000010206
075100031006     c                   If        d§CmrNmf <> *Blanks
075200010706     C                   movel     msg(8)        v2cmsg
075300010706     C                   eval      *in28 = *on
075400010706     C                   GOTO      ENDCAU
075500010706     C                   endif
075600010709     C                   if        V2CCAU = WCAUSA
075700010706     C                   movel     msg(18)       v2cmsg
075800010706     C                   eval      *in28 = *on
075900010706     C                   GOTO      ENDCAU
076000010706     C                   endif
076100081030
076200081125      * se causale inserita genera bolla di addebito (colpa cliente)
076300081030      * controllo che l'ORM non abbia già avuto una causale che genera addebito
076400081125      * in questo caso faccio solo un addebito
076500081125     c                   if        d§cmradd = 'S'
076600081030     c     kfnorm        setll     fnorf01l
076700081030     c                   do        *hival
076800081030     c     kfnorm        reade(n)  fnorf01l
076900081030     c                   if        %eof (fnorf01l)
077000081030     c                   leave
077100081030     c                   endif
077200081030      * senza causale rileggo
077300081030     c                   if        orfcar = *blanks
077400081030     c                   iter
077500081030     c                   endif
077600081030      * fase da PDA rileggo
077700081030     c                   if        orffar = 410
077800081030     c                   iter
077900081030     c                   endif
078000081030     c                   clear                   a_dcmr
078100081030     c                   clear                   tibs02ds
078200081030     c                   eval      t02mod = 'C'
078300081030     c                   eval      t02sif = knsif
078400081030     c                   eval      t02cod = 'CMR'
078500081030     c                   eval      t02ke1 = orfcar
078600081030     c                   call      'TIBS02R'
078700081030     c                   parm                    kpjba
078800081030     c                   parm                    tibs02ds
078900081030     c                   eval      a_dcmr = t02uni
079000081125      * se causale che genera bolla esco e non genero la seconda bolla
079100081030     c                   if        a_d§cmradd = 'S'
079200081125     c                   eval      wnobolla = *on
079300081030     c                   leavesr
079400081030     c                   endif
079500081030     c                   enddo
079600081030     c                   endif
079700081030
079800010706     C                   endif
079900010706
080000010629     C     ENDCAU        endsr
080100010530      **********************************************************************
080200010530      * RICHIAMA IL PROGRAMMA DELLE NOTE
080300010530      **********************************************************************
080400010530     C     Sr_Note       BEGSR
080500010530
080600010530     C                   clear                   parm01
080700010605     C                   movel     V2CFGS        pfgs
080800010605     C                   movel     WDAO          i06dta
080900010605     C                   z-add     V2CPOE        i06poe
081000010605     C                   z-add     V2CNOR        i06nor
081100010605     C                   z-add     V2CNSR        i06nsr
081200010605     C                   z-add     V2CNRV        i06nrv
081300010530     C                   eval      i06dai = dateu
081400010530     C                   TIME                    W0140
081500010530     C                   movel     w0140         I06ori
081600010530     C                   movel     'V'           i06flm
081700080319      * devo impostare la causale nelle note così viene visualizzata dal
081800080319      * pgm di gestione note
081900080319     c                   clear                   fior06vds
082000080319     c                   if        wcausa <> *blanks
082100080319     c                   eval      v06car = wcausa
082200080319     c                   eval      v06dcar = wdescau
082300080319     c                   endif
082400010530
082500010530     C                   movel     kpjbu         §kpjbu
082600010530     C                   movel     parm01        kpjbu
082700010530     C                   CALL      'FIOR06R'
082800010530     C                   PARM                    kpjba
082900010530     C                   parm                    fior06ds
083000080319     C                   parm                    fior06vds
083100010530     C                   movel     kpjbu         parm01
083200010530     C                   movel     §kpjbu        kpjbu
083300010530
083400010530     C                   endsr
083500010605      *****************************************************************
083600010605      * CONTROLLO NOTE SE OBBLIGATORIE
083700010605      *****************************************************************
083800010605     C     SR_CONTRNOTE  BEGSR
083900010605
084000010605     C                   IF        *IN11 = *ON
084100010706     C                   EVAL      WCAUSA = V2CLAV
084200010605     C                   ENDIF
084300010605     C                   IF        *IN10 = *ON OR *IN12 = *ON
084400010605     C                   EVAL      WCAUSA = V2CCAU1
084500010605     C                   ENDIF
084600010605
084700010605     C                   if        WCAUSA  <> *blanks
084800010605     C                   clear                   dcmr
084900010605     C                   clear                   TIBS02DS
085000010605     C                   movel     'C'           T02mod
085100010605     C                   movel     knsif         t02sif
085200010605     C                   movel     'CMR'         t02cod
085300010605     C                   movel(p)  WCAUSA        T02ke1
085400010605     C                   call      'TIBS02R'
085500010605     C                   parm                    KPJBA
085600010605     C                   parm                    TIBS02DS
085700010605     C                   movel     t02ke1        WCAUSA
085800010605     C                   movel     t02uni        dcmr
085900010706     C   11              EVAL      V2CLAV = WCAUSA
086000010605     C   10
086100010605     COR 12              EVAL      V2CCAU1 = WCAUSA
086200010605
086300010605     C                   if        d§cmrnot <> *blanks
086400010605     C                   do        *hival
086500010605     C                   clear                   fior06ds
086600010605     C                   movel     d§cmrfar      i06far
086700010605     C                   exsr      Sr_Note
086800010605     C                   if        o06f03 = 'S' or o06f12 = 'S'
086900010605     C                   iter
087000010605     C                   endif
087100010605     C                   if        o06dai <> ormdfo
087200010605     C                   iter
087300010605     C                   endif
087400010605     C                   if        o06ori < ormofo
087500010605     C                   iter
087600010605     C                   endif
087700010605     C                   if        o06prg = 0
087800010605     C                   iter
087900010605     C                   endif
088000010605     C                   leave
088100010605     C                   enddo
088200010605     C                   endif
088300010605     C                   endif
088400010605
088500010605     C                   endsr
088600010206      **********************************************************************
088700010206      * CONFERMA
088800010206      **********************************************************************
088900010206     C     Sr_Conferma   BEGSR
089000010206
089100010206     C                   eval      *in44 = *off
089200020524     c                   eval      *in46 = *off
089300020524
089400010530      * Controllo che orm non sia allocato
089500010605     C     kfnorm        chain(e)  fnorm01l
089600010530     C                   if        %error
089700010605     C                   movel     msg(10)       V2CMSG
089800010530     C                   eval      *in44 = *on
089900010530     C                   goto      endsrconf
090000010530     C                   endif
090100010530
090200010605     C                   IF        V2FAO <> ORMFAO
090300010605     C                   MOVEL     MSG(11)       v2cmsg
090400010605     C                   EVAL      *IN28 = *ON
090500010605     C                   goto      endsrconf
090600010605     C                   ENDIF
090700020524
090800010530      * Se orm è in fase di bollettato (900) non devo aggiornare
090900020524     C                   if        ormfao = 900
091000020524     C                   unlock    fnorm01l
091100020524     C                   movel     msg(19)       V2CMSG
091200020524     C                   eval      *in46 = *on
091300020524     C                   goto      endsrconf
091400020524     c                   endif
091500020524
091600020524     c                   eval      ds_ormcra = ormcra
091700020524     c                   eval      ds_ormcor = ormcor
091800020524     c                   eval      ds_ormcrc = ormcrc
091900020529     c                   clear                   okbolla
092000040406     c                   Eval      sav_OrmFao = OrmFao
092100020524
092200020524      * prima di confermare controllo se devo creare la bolla di addebito
092300020529      * se non già fatto in precedenza
092400020529     c                   if        not *in45
092500020524     c                   exsr      sr_contradd
092600020524      * se f12 o f03 da immissione bolla riemetto la videata
092700020524     c                   if        okbolla = '1'
092800020524     C                   goto      endsrconf
092900020524     c                   endif
093000020529      * se msg info di annullo bolla riemetto la videata
093100020529     c                   if        *in45 = *on
093200020529     c                   eval      *in28 = *on
093300020529     C                   goto      emetto
093400020529     c                   endif
093500020529     c                   endif
093600020524
093700010206      * RITIRATI
093800010605     C   12
093900010605     COR 10              EVAL      WCAUSA = V2CCAU1
094000010706     C   11              EVAL      WCAUSA = V2CLAV
094100010605
094200010605     C                   if        WCAUSA = *blanks
094300010206      * Aggiorno ORM e scrivo fase
094400010605      *QUADRATURA
094500010605     C                   IF        *IN10 = *ON
094600010529     C                   time                    w0140
094700010529     C                   move      w0140         wdtgio
094800010529     C                   z-add     wdtgio        G02DAT
094900010529     C                   clear                   G02ERR
095000010529     C                   call      'XSRDA8'
095100010529     C                   parm                    WLBDAT
095200010529     C                   z-add     g02inv        ormdfo
095300010206     C                   movel     w0140         ormofo
095400010206     C                   z-add     600           ormfao
095500010706     C                   Z-ADD     4             ORMNPG
095600010706     C                   MOVE      'M'           ORMTAP
095700010706     C                   MOVE      V2CNDC2       ORMNDC
095800010706     C     V2CNDC2       ifne      *blanks
095900120613     C                   z-add     4             kdstnpg
096000120613     C                   move      v2CNDC2       kdstnfv
096100120613     C                   z-add     v2CFGS        kdstfgs
096200120613     C     kfidst        chain     fidst01l
096300120613     c                   if        not %found(fidst01l)
096400060516     c                   clear                   fnlv55ds
096500060516     c                   eval      d55tpt = '6'
096600060516     c                   move      v2cfgs        d55lin
096700060516     c                   eval      d55drf = dateu
096800060516     c                   call      'FNLV55R'
096900060516     c                   parm                    fnlv55ds
097000060516     c                   if        d55err = *blanks and d55tfa <> d55lin
097100120613     c                   move      d55tfa        kdstfgs
097200120613     c     kfidst        chain     fidst01l
097300060516     c                   endif
097400060516     c                   endif
097500120613     c                   if        %found(fidst01l)
097600120613     C                   z-add     dstdfv        ormddc
097700010706     C                   endif
097800010706     C                   endif
097900040909     C                   eval      ormdtt = g02inv
098000070711      * imposto ormeti
098100070711     c                   exsr      sr_fase
098200070711     c                   eval      ormeti = d§farass
098300010206     C                   update    fnorm000
098400010206
098500040909     c                   Clear                   Fnorf000
098600010206     C                   eval      orfpoe = ormpoe
098700010206     C                   eval      orfnsr = ormnsr
098800010206     C                   eval      orfnor = ormnor
098900010206     C                   eval      orfnrv = ormnrv
099000010406     C                   eval      orfpog = ormpor
099100010206     C                   eval      orfdae = ormdfo
099200010206     C                   eval      orfore = ormofo
099300010206     C                   eval      orffar = ormfao
099400010206     C                   eval      orfcar = *blanks
099500010206     C                   eval      orfpue = KNMUS
099600041013     c                   If        OrmNdc > *Zeros
099700120613     c                   eval      orffgs = kdstfgs
099800040909     c                   Eval      OrfNdc = OrmNdc
099900040909     c                   Eval      OrfDdc = OrmDdc
100000041013     c                   EndIf
100100010206     C                   write     fnorf000
100200010605     C                   ENDIF
100300010605      * RIAPERTURA
100400010605     C                   IF        *IN12 = *ON
100500010605     C                   time                    w0140
100600010605     C                   move      w0140         wdtgio
100700010605     C                   z-add     wdtgio        G02DAT
100800010605     C                   clear                   G02ERR
100900010605     C                   call      'XSRDA8'
101000010605     C                   parm                    WLBDAT
101100010605     C                   z-add     g02inv        ormdfo
101200010605     C                   movel     w0140         ormofo
101300010605     C                   movel     100           ormfao
101400010605     C                   clear                   ormtap
101500010605     C                   clear                   ormnpg
101600010605     C                   clear                   ormndc
101700010605     C                   clear                   ormddc
101800010605     C                   CLEAR                   ORMDST
101900040909     C                   eval      ormdtt = g02inv
102000070711      * imposto ormeti
102100070711     c                   exsr      sr_fase
102200070711     c                   eval      ormeti = d§farass
102300080222      * se ORM ancora assegnabile pulisco la data di stampa
102400080222     c                   if        ormeti <> *blanks and *in07
102500080222     c                   clear                   ormdst
102600080222      * devo pulire anche i dati su fnorg
102700080222     c     kfnorm        chain     fnorg01l
102800080222    3c                   if        %found(fnorg01l)
102900080314     c                   eval      dorg01 = orgflo
103000080222     c                   eval      orgndc = 0
103100080222     c                   eval      orgpdc = 0
103200080222     c                   eval      orgfgs = 0
103300080222     c                   eval      orgddc = 0
103400080222     c                   eval      orgnftl= 0
103500080222     c                   eval      orgslo = 0
103600080222     c                   eval      orgdtvdis= 0
103700080222     c                   eval      orghvdis= 0
103800080222     c                   eval      orgfao = 0
103900080222     c                   eval      orgdfo = 0
104000080222     c                   eval      orgofo = 0
104100080314     c                   clear                   §orgpda
104200080314     c                   eval      orgflo = dorg01
104300080222     c                   update    fnorg000
104400080222    3c                   endif
104500080222     c                   endif
104600010605     C                   update    fnorm000
104700010605
104800040909     c                   Clear                   Fnorf000
104900010605     C                   eval      orfpoe = ormpoe
105000010605     C                   eval      orfnsr = ormnsr
105100010605     C                   eval      orfnor = ormnor
105200010605     C                   eval      orfnrv = ormnrv
105300010605     C                   eval      orfpog = ormpor
105400010605     C                   eval      orfdae = ormdfo
105500010605     C                   eval      orfore = ormofo
105600010605     C                   eval      orffar = ormfao
105700010605     C                   eval      orfcar = *BLANKS
105800010605     C                   eval      orfpue = KNMUS
105900010605     C                   write     fnorf000
106000010605     C                   endif
106100010206      * MANCATO RITIRO
106200010206     C                   else
106300010529
106400040909      * salvo i campi della distinta da riportare sulla fase
106500041015if  1c                   If        v2cndc > *Zeros
106600041015     c                   move      v2cndc        savndc
106700041015     c                   Move      wggg          wa4
106800041015     c                   Movel     wmmm          wa4
106900041015     c                   Move      wa4           savddc
107000041020     c                   Movel     waaa          wa4
107100041020     c                   Movel     wa4           savddc
107200060516     c                   eval      savfgs = v2hfgs
107300041015   x1c                   Else
107400041015      * cerco la fase precedente con la distinta
107500041015     c     kFnorf        Setll     Fnorf01l
107600041015     c                   Do        *Hival
107700041015     c     kFnorf1       Readpe    Fnorf01l
107800041015      * Fine file
107900041015     c                   If        %Eof(Fnorf01l)
108000041015     c                   Leave
108100041015     c                   EndIf
108200041015      * fase con distinta salvo i dati ed esco
108300041015     c                   If        OrfNdc > *Zeros
108400041015     c                   Eval      savndc = OrfNdc
108500041015     c                   Eval      savddc = OrfDdc
108600060516     c                   eval      savfgs = orffgs
108700041015     c                   Leave
108800041015     c                   EndIf
108900041015     c                   EndDo
109000041015e   1c                   EndIf
109100040909
109200010529     C                   time                    w0140
109300010529     C                   move      w0140         wdtgio
109400010529     C                   z-add     wdtgio        G02DAT
109500010529     C                   clear                   G02ERR
109600010529     C                   call      'XSRDA8'
109700010529     C                   parm                    WLBDAT
109800010529     C                   z-add     g02inv        ormdfo
109900010206     C                   movel     w0140         ormofo
110000010301     C                   movel     d§cmrfar      ormfao
110100010206     C                   clear                   ormtap
110200010206     C                   clear                   ormnpg
110300010206     C                   clear                   ormndc
110400010206     C                   clear                   ormddc
110500040909     C                   eval      ormdtt = g02inv
110600070711      * imposto ormeti
110700070711     c                   exsr      sr_fase
110800070711     c                   eval      ormeti = d§farass
110900070926      * se ORM ancora assegnabile pulisco la data di stampa
111000070926     c                   if        ormeti <> *blanks and *in07
111100070926     c                   clear                   ormdst
111200071011      * devo pulire anche i dati su fnorg
111300071011     c     kfnorm        chain     fnorg01l
111400071011    3c                   if        %found(fnorg01l)
111500080314     c                   eval      dorg01 = orgflo
111600071011     c                   eval      orgndc = 0
111700071011     c                   eval      orgpdc = 0
111800071011     c                   eval      orgfgs = 0
111900071011     c                   eval      orgddc = 0
112000071011     c                   eval      orgnftl= 0
112100071011     c                   eval      orgslo = 0
112200071011     c                   eval      orgdtvdis= 0
112300071011     c                   eval      orghvdis= 0
112400071026     c                   eval      orgfao = 0
112500071026     c                   eval      orgdfo = 0
112600071026     c                   eval      orgofo = 0
112700080314     c                   clear                   §orgpda
112800080314     c                   eval      orgflo = dorg01
112900071011     c                   update    fnorg000
113000071011    3c                   endif
113100070926     c                   endif
113200010206     C                   update    fnorm000
113300010206
113400081125     c                   clear                   dorf01
113500040909     c                   Clear                   Fnorf000
113600010206     C                   eval      orfpoe = ormpoe
113700010206     C                   eval      orfnsr = ormnsr
113800010206     C                   eval      orfnor = ormnor
113900010206     C                   eval      orfnrv = ormnrv
114000010406     C                   eval      orfpog = ormpor
114100010206     C                   eval      orfdae = ormdfo
114200010206     C                   eval      orfore = ormofo
114300010206     C                   eval      orffar = ormfao
114400010605     C                   eval      orfcar = WCAUSA
114500010206     C                   eval      orfpue = KNMUS
114600041013     c                   If        savndc > *Zeros
114700060516     c                   eval      orffgs = savfgs
114800040909     c                   Eval      OrfNdc = savndc
114900040909     c                   Eval      OrfDdc = savddc
115000041013     c                   EndIf
115100081125      * imposto se non ho fatto l'ulteriore addebito
115200081125     c                   if        wnobolla = *on
115300081125     c                   eval      §orfadd = 'S'
115400081125     c                   endif
115500081125     c                   eval      orfflo = dorf01
115600010206     C                   write     fnorf000
115700010206
115800010206     C                   endif
115900080423
116000080423      * se ORM commissionato da DPD devo rimettere in trasmissione il file FIDPO
116100080423     c                   if        $comdpd
116200080423     c     kfnorm        chain     fidpo01l
116300080423     c                   if        %found(fidpo01l)
116400080423     c                   clear                   dpodtr
116500080423     c                   update    fidpo000
116600080423     c                   endif
116700080423     c                   endif
116800010206
116900010206     C     endsrconf     endsr
117000020524      **********************************************************************
117100020524      * Controllo se devo generare la bolla di addebito
117200020524      **********************************************************************
117300020524     c     sr_contradd   begsr
117400020524
117500020524     c                   clear                   sav_lpksc
117600020524     c                   eval      nobolla = *off
117700020524     c                   eval      no400 = *off
117800081125
117900081125      * se non devo generare la bolla di addebito vado a fine e imposto il flag
118000081125      * come se avessi fatto la bolla
118100081125     c                   if        wnobolla = *on
118200081125     c                   leavesr
118300081125     c                   endif
118400020524
118500020524      * se cod.ordinante codificato addebito
118600020524 b1  c                   if        ormcor <> *zeros and ormcor2 <> 8888
118700020524     c                             and ormcor2 <> 9999
118800020524      * prima controllo la causale
118900020524     c                   exsr      sr_contrcmr
119000020524     c                   if        nobolla = *on
119100020524     c                   goto      endcontradd
119200020524     c                   endif
119300020524     c                   clear                   dblp
119400090505     c                   movel     ormcor        §lpksca
119500020702      * se c'è imposto il codice bollettazione
119600020702     c     ormcor        chain     fnacr01l
119700020702     c                   if        %found(fnacr01l) and acratb = *blanks
119800020702     c                             and acrksc <> *zeros
119900090505     c                   move      acrksc        §lpksca
120000090505     c                   if        acrccc<>999
120100090505     c                   move      acrccc        §lpctra
120200090505     c                   endif
120300020702     c                   endif
120400020524     c                   eval      §lprsd = ormrso
120500020524     c                   eval      §lpind = ormino
120600020524     c                   eval      §lpcad = ormcao
120700020524     c                   eval      §lplod = ormloo
120800020524     c                   eval      §lpprd = ormpro
120900020524     c                   eval      §lpnzd = ormnao
121000020524     c                   exsr      sr_wrtbolla
121100020524 x1  c                   else
121200020524      * se non inserito l'ordinante
121300020524 b2  c                   if        ormrso = *blanks
121400020524      * orm non commissionato
121500020524 b3  c                   if        ormpoe = ormpor
121600020524      * se cod.mittente codificato addebito
121700020524 b4  c                   if        ormcra <> *zeros and ormcra2 <> 8888
121800020524     c                             and ormcra2 <> 9999
121900020524      * prima controllo la causale
122000020524     c                   exsr      sr_contrcmr
122100020524     c                   if        nobolla = *on
122200020524     c                   goto      endcontradd
122300020524     c                   endif
122400020524     c                   clear                   dblp
122500090505     c                   movel     ormcra        §lpksca
122600020702      * se c'è imposto il codice bollettazione
122700020702     c     ormcra        chain     fnacr01l
122800020702     c                   if        %found(fnacr01l) and acratb = *blanks
122900020702     c                             and acrksc <> *zeros
123000090505     c                   move      acrksc        §lpksca
123100090505     c                   if        acrccc<>999
123200090505     c                   move      acrccc        §lpctra
123300090505     c                   endif
123400020702     c                   endif
123500020524     c                   eval      §lprsd = ormrsr
123600020524     c                   eval      §lpind = orminr
123700020524     c                   eval      §lpcad = ormcar
123800020524     c                   eval      §lplod = ormlor
123900020524     c                   eval      §lpprd = ormprr
124000020524     c                   eval      §lpnzd = ormnar
124100020524     c                   exsr      sr_wrtbolla
124200020524      * se cod.mittente non codificato
124300020524 x4  c                   else
124400020524      * se cod.destinatario codificato addebito
124500020524 b5  c                   if        ormcrc <> *zeros and ormcrc2 <> 8888
124600020524     c                             and ormcrc2 <> 9999
124700020524      * prima controllo la causale
124800020524     c                   exsr      sr_contrcmr
124900020524     c                   if        nobolla = *on
125000020524     c                   goto      endcontradd
125100020524     c                   endif
125200020524     c                   clear                   dblp
125300090505     c                   movel     ormcrc        §lpksca
125400020702      * se c'è imposto il codice bollettazione
125500020702     c     ormcrc        chain     fnacr01l
125600020702     c                   if        %found(fnacr01l) and acratb = *blanks
125700020702     c                             and acrksc <> *zeros
125800090505     c                   move      acrksc        §lpksca
125900090505     c                   if        acrccc<>999
126000090505     c                   move      acrccc        §lpctra
126100090505     c                   endif
126200020702     c                   endif
126300020524     c                   eval      §lprsd = ormrsc
126400020524     c                   eval      §lpind = orminc
126500020524     c                   eval      §lpcad = ormcac
126600020524     c                   eval      §lplod = ormloc
126700020524     c                   eval      §lpprd = ormprc
126800020524     c                   eval      §lpnzd = ormnac
126900020524     c                   exsr      sr_wrtbolla
127000020524 e5  c                   endif
127100020524 e4  c                   endif
127200020524      * orm commissionato
127300020524 x3  c                   else
127400020524      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
127500020524 b4  c                   if        ormcrc <> *zeros and ormcrc2 <> 8888
127600020524     c                             and ormcrc2 <> 9999  and ormcrc1 = ormpoe
127700020524      * prima controllo la causale
127800020524     c                   exsr      sr_contrcmr
127900020524     c                   if        nobolla = *on
128000020524     c                   goto      endcontradd
128100020524     c                   endif
128200020524     c                   clear                   dblp
128300090505     c                   movel     ormcrc        §lpksca
128400020702      * se c'è imposto il codice bollettazione
128500020702     c     ormcrc        chain     fnacr01l
128600020702     c                   if        %found(fnacr01l) and acratb = *blanks
128700020702     c                             and acrksc <> *zeros
128800090505     c                   move      acrksc        §lpksca
128900090505     c                   if        acrccc<>999
129000090505     c                   move      acrccc        §lpctra
129100090505     c                   endif
129200020702     c                   endif
129300020524     c                   eval      §lprsd = ormrsc
129400020524     c                   eval      §lpind = orminc
129500020524     c                   eval      §lpcad = ormcac
129600020524     c                   eval      §lplod = ormloc
129700020524     c                   eval      §lpprd = ormprc
129800020524     c                   eval      §lpnzd = ormnac
129900020524     c                   exsr      sr_wrtbolla
130000020524      * se non codificato o p.o. differenti
130100020524 x4  c                   else
130200020524      * se cod.mittente codificato addebito
130300020524 b5  c                   if        ormcra <> *zeros and ormcra2 <> 8888
130400020524     c                             and ormcra2 <> 9999
130500020524      * prima controllo la causale
130600020524     c                   exsr      sr_contrcmr
130700020524     c                   if        nobolla = *on
130800020524     c                   goto      endcontradd
130900020524     c                   endif
131000020524     c                   clear                   dblp
131100090505     c                   movel     ormcra        §lpksca
131200020702      * se c'è imposto il codice bollettazione
131300020702     c     ormcra        chain     fnacr01l
131400020702     c                   if        %found(fnacr01l) and acratb = *blanks
131500020702     c                             and acrksc <> *zeros
131600090505     c                   move      acrksc        §lpksca
131700090505     c                   if        acrccc<>999
131800090505     c                   move      acrccc        §lpctra
131900090505     c                   endif
132000020702     c                   endif
132100020524     c                   eval      §lprsd = ormrsr
132200020524     c                   eval      §lpind = orminr
132300020524     c                   eval      §lpcad = ormcar
132400020524     c                   eval      §lplod = ormlor
132500020524     c                   eval      §lpprd = ormprr
132600020524     c                   eval      §lpnzd = ormnar
132700020524     c                   exsr      sr_wrtbolla
132800020524 e5  c                   endif
132900020524 e4  c                   endif
133000020524 e3  c                   endif
133100020524 e2  c                   endif
133200020524 e1  c                   endif
133300020524
133400020524     c     endcontradd   endsr
133500020524      **********************************************************************
133600020524      * controllo la causale
133700020524      **********************************************************************
133800020524     c     sr_contrcmr   begsr
133900020524
134000020524      * da causale con addebito a causale senza addebito o no causale
134100020524      * non devo creare la bolla
134200020524      * ma devo emettere msg info x annullamento bolla addebito già creata in precedenza
134300020524     c                   if        (*in10 or *in12) and sav_cmradd = 'S' and
134400020524     c                             new_cmradd = ' '
134500020524     c                   movel     msg(20)       v2cmsg
134600020524     c                   eval      *in45 = *on
134700020524     c                   eval      nobolla = *on
134800020524     c                   endif
134900020524      * da causale con addebito a causale con addebito non devo creare la bolla
135000020524     c                   if        (*in10 or *in12) and sav_cmradd = 'S' and
135100020524     c                             new_cmradd = 'S'
135200020524     c                   eval      nobolla = *on
135300020524     c                   endif
135400020524      * da causale senza addebito o no causale a causale senza addebito non devo creare la bolla
135500020524     c                   if        ((*in10 and sav_cmradd = ' ') or *in11)
135600020524     c                             and new_cmradd = ' '
135700020524     c                   eval      nobolla = *on
135800020524     c                   endif
135900020524      * da causale senza addebito a causale con addebito
136000030901      * e l'orm è in fase 999 o 910 e prima non è stata fatta una fase 400
136100020524      * non devo fare la bolla
136200020524     c                   if        *in12 and sav_cmradd = ' ' and
136300020524     c                             new_cmradd = 'S'
136400020524     c                   exsr      sr_cerca400
136500020524     c                   if        no400 = *on
136600020524     c                   eval      nobolla = *on
136700020524     c                   endif
136800020524     c                   endif
136900080226      * se riapertura ORM non devo creare la bolla di addebito
137000080226     c                   if        *in12
137100080226     c                   eval      nobolla = *on
137200080226     c                   endif
137300020524
137400020524     c                   endsr
137500020524      **********************************************************************
137600020524      * cerco se è stata fatta una fase 400 in precedenza
137700020524      **********************************************************************
137800020524     c     sr_cerca400   begsr
137900020524
138000020524     c     kfnorf        setll     fnorf01l
138100020524     c     kfnorf1       readpe    fnorf01l
138200020524     c                   dow       not %eof(fnorf01l)
138300080226      * la fase precedente è maggiore di 420 non devo fare la bolla
138400080226     c                   if        orffar > 420
138500020524     c                   eval      no400 = *on
138600020524     c                   leave
138700020524     c                   endif
138800080226      * la fase precedente è la 400 o 410 o 420 devo fare la bolla
138900080226     c                   if        orffar = 400 or orffar = 410 or
139000080226     c                             orffar = 420
139100020524     c                   leave
139200020524     c                   endif
139300020524      * la fase precedente è minore 400 e non è la 300 non devo fare la bolla
139400020524     c                   if        orffar < 400 and orffar <> 300
139500020524     c                   eval      no400 = *on
139600020524     c                   leave
139700020524     c                   endif
139800020524     c     kfnorf1       readpe    fnorf01l
139900020524     c                   enddo
140000020524
140100020524     c                   endsr
140200020524      **********************************************************************
140300020524      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
140400020524      **********************************************************************
140500020524     c     sr_wrtbolla   begsr
140600020524
140700020524      * richiamo il programma per la scelta delle stampanti
140800020524     c                   exsr      sr_scestp
140900020524     c   92              eval      okbolla = '1'
141000020524     c   92              goto      endwrtbolla
141100020524
141200020524     c                   clear                   kpjbu
141300020524     c                   clear                   dtasv
141400020524
141500020524     c                   eval      §lpfpr = 'T'
141600020524     c                   movel     ormpor        §lpccn
141700020524     c                   move      9999          §lpccn
141800020524     c                   movel     dateu         §lpaas
141900020524     c                   move      dateu         §lpmgs
142000020524     c                   eval      §lplnp = ormpor
142100020524     c                   eval      §lpcbo = 'FY'
142200020524     c                   eval      §lpsop = '+'
142300020530     c                   eval      §lplna = ormpoe
142400020524     c                   eval      §lprsm = ormrsr
142500020524     c                   eval      §lpinm = orminr
142600020524     c                   eval      §lpcam = ormcar
142700020524     c                   eval      §lplom = ormlor
142800020524     c                   eval      §lpprm = ormprr
142900020524     c                   eval      §lpnzm = ormnar
143000020524
143100020524 b1  c                   if        ormncl <> 0
143200020524     c                   eval      §lpncl = ormncl
143300020524 x1  c                   else
143400020524     c                   eval      §lpncl = 1
143500020524 e1  c                   endif
143600020524 b1  c                   if        ormpkg <> 0
143700020524     c                   eval      §lppkb = ormpkg
143800020524 x1  c                   else
143900020524 b2  c                   if        ormvlm <> 0
144000020524     c     ormvlm        mult      200           §lppkb
144100020524 x2  c                   else
144200020524 b3  c                   if        ormbnc <> 0
144300020524     c     ormbnc        mult      200           §lppkb
144400020524 e3  c                   endif
144500020524 e2  c                   endif
144600020524 e1  c                   endif
144700020524
144800020524     c                   clear                   ds_keyorm
144900020524     c                   eval      ds_poe = ORMpoe
145000020524     c                   eval      ds_nsr = ORMnsr
145100020524     c                   eval      ds_nor = ORMnor
145200020524     c                   eval      ds_nrv = ORMnrv
145300041201     c                   move      ds_keyorm     §lprmn
145400020524
145500020524     c                   eval      §lprma = ormrfa
145600020524     c                   eval      §lprmo = wcausa + '-' + d§cmrdes1
145700020524     c                   eval      §lpctm = '4F'
145800020524     c                   movel     dateu         §lpdrt
145900020524
146000020524      * Cerca codice autotrasportatore e flag mattino/pomeriggio
146100020524 b1  c                   if        ormndc <> 0
146200120613     c                   z-add     ormnpg        kdstnpg
146300120613     c                   z-add     ormndc        kdstnfv
146400120613     c                   z-add     ormpor        kdstfgs
146500120613     c     kfidst        chain     fidst01l
146600120613 b2  c                   if        not %found(fidst01l)
146700060516     c                   clear                   fnlv55ds
146800060516     c                   eval      d55tpt = '6'
146900060516     c                   move      ormpor        d55lin
147000060516     c                   eval      d55drf = dateu
147100060516     c                   call      'FNLV55R'
147200060516     c                   parm                    fnlv55ds
147300060516     c                   if        d55err = *blanks and d55tfa <> d55lin
147400120613     c                   move      d55tfa        kdstfgs
147500120613     c     kfidst        chain     fidst01l
147600060516     c                   endif
147700060516     c                   endif
147800120613 b2  c                   if        %found(fidst01l)
147900020524     c                   eval      §lppdr = DSTpdr
148000120613     c                   eval      §lpfpp = dstfpp
148100020524 x2  c                   else
148200020524     c                   movel     §lplnp        §lppdr
148300020524     c                   move      0999          §lppdr
148400020524     c                   eval      §lpfpp = 'M'
148500020524 e2  c                   endif
148600020524 x1  c                   else
148700020524     c                   movel     §lplnp        §lppdr
148800020524     c                   move      0999          §lppdr
148900020524     c                   eval      §lpfpp = 'M'
149000020524 e1  c                   endif
149100020524
149200020524     C                   call      'FNLS01R'
149300020524     C                   parm                    kpjba
149400020524     C                   parm                    dblp
149500020524     C                   parm                    dtasv
149600030623     c                   Parm                    Trul90ds
149700020524
149800020524     C                   if        §lpfpr = '4' or §lpfpr = '3'
149900020524     C                             or §lpfpr = '5'
150000020524     C                   eval      okbolla = '1'
150100020524     C                   endif
150200020524
150300020524     c     endwrtbolla   endsr
150400020524      **********************************************************************
150500020524      * scelta stampante per immissione bolla
150600020524      **********************************************************************
150700020524     c     sr_scestp     begsr
150800020524
150900020524     C                   clear                   kpjbu
151000020524     c                   setoff                                       92
151100020524
151200020524      * Richiesta stampanti
151300020524     C                   if        contas = *zeros
151400030623     C                   clear                   trul90ds
151500030623     C                   movel     'S'           D90rse
151600030623     C                   movel     'S'           D90rsb
151700030623     C                   call      'TRUL90R'
151800020524     C                   parm                    kpjba
151900030623     C                   parm                    trul90ds
152000020524      * F3=Fine
152100030623     C                   if        D90f3 = '1'
152200020524     C                   eval      *in92 = *on
152300020524     C                   goto      endstp
152400020524     C                   endif
152500030307
152600030307     C                   z-add     103           lung
152700030304
152800020524      * Ovrprtf Segnacolli
152900030623     C                   movea     D90pre        cm3(28)
153000030623     C                   movea     D90mde        cm3(50)
153100020524     C                   clear                   comman
153200020524     C                   movea     cm3           comman
153300020524     C                   CALL      'QCMDEXC'                            92
153400020524     C                   PARM                    comman
153500020524     C                   PARM                    lung
153600020524     C   92              goto      endstp
153700030623
153800030623      * Bolle - Nuovo modulo
153900030623
154000030623     c                   Z-Add     110           lung
154100030623
154200030623      * Ovrprtf Bolle A4
154300030623     c                   Movea     D90prb4       cma4(30)
154400030623     c                   Movea     D90mdb4       cma4(52)
154500030623     c                   Clear                   Comman
154600030623     c                   Movea     cmA4          Comman
154700030623     c                   Call      'QCMDEXC'                            92
154800030623     c                   Parm                    Comman
154900030623     c                   Parm                    Lung
155000030623     c   92              goto      endstp
155100030623
155200030623      * Ovrprtf Bolle A5
155300030623     c                   Movea     D90prb5       cmA5(30)
155400030623     c                   Movea     D90mdb5       cmA5(52)
155500030623     c                   Clear                   Comman
155600030623     c                   Movea     cmA5          Comman
155700030623     c                   Call      'QCMDEXC'                            92
155800030623     c                   Parm                    Comman
155900030623     c                   Parm                    Lung
156000030623     c   92              goto      endstp
156100020524
156200020524     C                   eval      contas = contas +1
156300020524     C                   endif
156400020524
156500020524     c     endstp        endsr
156600010605      *****************************************************************
156700010605      * CONTROLLI SECONDA VIDEATA
156800010605      *****************************************************************
156900010605     C     SR_CONTR2     BEGSR
157000010605
157100010625     C                   setoff                                       474828
157200020524     c                   clear                   new_cmradd
157300081125     c                   eval      wnobolla = *off
157400020524
157500010605     C                   IF        *IN10 = *ON
157600010629     C                   MOVE      *BLANKS       V2DCAU1
157700010709     C                   MOVE      *BLANKS       V2DNDC2
157800010709     C                   IF        V2CLAV = *ZEROS
157900010709     C                   CLEAR                   V2CLAV
158000010709     C                   ENDIF
158100010709     C                   IF        V2CNDC2 = *ZEROS
158200010709     C                   CLEAR                   V2CNDC2
158300010709     C                   ENDIF
158400010706     C                   IF        V2CCAU1 <> *BLANKS AND V2CLAV <> *BLANKS
158500010625     C                   MOVEL     MSG(6)        V2CMSG                         solo 1 scelta
158600010605     C                   SETON                                        28
158700010706     C                   GOTO      ENDCONTR2
158800010605     C                   ENDIF
158900010706     C                   IF        V2CCAU1 <> *BLANKS AND V2cndc2 <> *BLANKS and
159000010706     C                             v2cndc2 <> *zeros
159100010706     C                   MOVEL     MSG(6)        V2CMSG                         solo 1 scelta
159200010706     C                   SETON                                        28
159300010706     C                   GOTO      ENDCONTR2
159400010706     C                   ENDIF
159500010706     C                   IF        V2CCAU1 = *BLANKS AND V2CLAV = *BLANKS
159600010629     C                   MOVEL     MSG(15)       V2CMSG
159700010629     C                   SETON                                        28
159800010629     C                   GOTO      ENDCONTR2
159900010629     C                   ENDIF
160000010706     C                   IF        V2CLAV = 'SI'
160100010625     C                   EXSR      SR_NUMDIS
160200010625     C                   IF        *IN28 = *ON OR *IN90 = *ON
160300010625     C                   SETON                                        48
160400010625     C                   GOTO      ENDCONTR2
160500010625     C                   ENDIF
160600010706     C                   ELSE
160700010706     C                   if        v2clav <> *blanks
160800010706     C                   MOVEL     MSG(17)       V2CMSG                         solo 1 scelta
160900010706     C                   SETON                                        2847
161000010706     C                   GOTO      ENDCONTR2
161100010706     C                   ELSE
161200010706     C                   CLEAR                   V2DNDC2
161300010706     C                   endif
161400010605     C                   ENDIF
161500010625     C                   ENDIF
161600010605
161700010605     C                   IF        *IN12 = *ON
161800010709     C                   IF        V2CLAV = *ZEROS
161900010709     C                   CLEAR                   V2CLAV
162000010709     C                   ENDIF
162100010706     C                   IF        V2CCAU1 <> *BLANKS AND V2Clav <> *BLANKS
162200010625     C                   MOVEL     MSG(6)        V2CMSG                         solo 1 scelta
162300010605     C                   SETON                                        28
162400010605     C                   GOTO      ENDCONTR2
162500010605     C                   ENDIF
162600010706     C                   IF        V2CCAU1 = *BLANKS AND V2Clav = *BLANKS
162700010629     C                   MOVEL     MSG(15)       V2CMSG
162800010629     C                   SETON                                        28
162900010629     C                   GOTO      ENDCONTR2
163000010629     C                   ENDIF
163100010706     C                   if        v2clav <> *blanks and v2clav <> 'SI'
163200010706     C                   seton                                        2847
163300010706     C                   movel     msg(17)       v2cmsg
163400010706     C                   goto      endcontr2
163500010706     C                   endif
163600010605     C                   ENDIF
163700010605      * QUADRATURA CON CAUSALE
163800010605      * FASE 600
163900010605     C                   IF        *IN11 = *ON
164000010629     C                   MOVE      *BLANKS       V2DCAU2
164100010709     C                   IF        V2CLAV = *BLANKS
164200010629     C                   MOVEL     MSG(16)       V2CMSG
164300010629     C                   SETON                                        28
164400010629     C                   GOTO      ENDCONTR2
164500010629     C                   ENDIF
164600010605      * Se immessa una causale questa deve esistere in tabella CMR
164700010706     C                   IF        V2CLAV <> *BLANKS
164800010706     C                   EVAL      WCAUSA = V2CLAV
164900010605     C                   EXSR      SR_CONTRCAU
165000010605     C                   IF        *IN28 = *ON
165100010605     C                   GOTO      ENDCONTR2
165200010605     C                   ENDIF
165300010605      * Se orm ha già una bolla non posso dichiararlo non fatto
165400010605      * se non è un prepagato
165500010605     C                   if        V2CTOR <> 'P'
165600010605     C                   exsr      Sr_Bolla
165700010605     C                   if        *in28 = *on
165800010605     C                   EVAL      *IN47 = *ON
165900010605     C                   goto      ENDCONTR2
166000010605     C                   endif
166100010605     C                   endif
166200010605     C                   endif
166300010605     C                   endif
166400030901      * FASE 500 O 999 o 910
166500010605     C                   IF        *IN10 = *ON OR *IN12 = *ON
166600010605      * CAMBIO CAUSALE
166700010605     C                   IF        V2CCAU1 <> *BLANKS
166800010605      * Se immessa una causale questa deve esistere in tabella CMR
166900010605     C                   EVAL      WCAUSA = V2CCAU1
167000010605     C                   EXSR      SR_CONTRCAU
167100010605     C                   IF        *IN28 = *ON
167200010605     C                   GOTO      ENDCONTR2
167300010605     C                   ENDIF
167400010605      * Se orm ha già una bolla non posso dichiararlo non fatto
167500010605      * se non è un prepagato
167600010605     C                   if        V2CTOR <> 'P'
167700010605     C                   exsr      Sr_Bolla
167800010605     C                   if        *in28 = *on
167900010605     C                   EVAL      *IN47 = *ON
168000010605     C                   goto      ENDCONTR2
168100010605     C                   endif
168200010605     C                   endif
168300010605     C                   endif
168400010605     C                   endif
168500010605
168600010605     C     ENDCONTR2     endsr
168700010625      **********************************************************************
168800010625      * NUMERO DISTINTA
168900010625      **********************************************************************
169000010625     C     SR_NUMDIS     begsr
169100010625      *    Ricerco Distinta
169200010625     c     '?'           scan      V2CNDC2                                39
169300010625     c   39              do
169400010625     C                   clear                   V2CNDC2
169500010625     C                   clear                   V2DNDC2
169600010625     c                   clear                   dslr01
169700010625     c                   movel     'S'           dlrric
169800010625     c                   movel     '4'           dlrnpg
169900010625     c                   movel     '2'           dlrtfv
170000010625     C                   movel     'FIOR27R'     dlrpgm
170100010625     c                   movel     V2CFGS        dlrfgs
170200010625     c                   movel     dslr01        kpjbu
170300010625     c                   call      'FNLR02R'
170400010625     c                   parm                    kpjba
170500010625     c                   movel     kpjbu         dslr01
170600010625     C     dlrnfv        ifeq      *zeros
170700010625     C                   clear                   V2CNDC2
170800010706     C                   seton                                        2848
170900010706     C                   movel     msg(14)       v2cmsg
171000010706     c                   goto      endnumdis
171100010625     C                   else
171200120613     c                   move      dlrnfv        V2CNDC2
171300010625     C                   endif
171400010625     C     V2CNDC2       ifne      *blanks
171500120613     C                   z-add     4             kdstnpg
171600120613     C                   move      V2CNDC2       kdstnfv
171700120613     C                   movel     V2CFGS        kdstfgs
171800120613     C     kfidst        chain     fidst01l
171900120613     C                   if        not %found(fidst01l)
172000060516     c                   clear                   fnlv55ds
172100060516     c                   eval      d55tpt = '6'
172200060516     c                   move      v2cfgs        d55lin
172300060516     c                   eval      d55drf = dateu
172400060516     c                   call      'FNLV55R'
172500060516     c                   parm                    fnlv55ds
172600060516     c                   if        d55err = *blanks and d55tfa <> d55lin
172700120613     c                   move      d55tfa        kdstfgs
172800120613     c     kfidst        chain     fidst01l
172900060516     c                   endif
173000060516     c                   endif
173100120613     C                   if        %found(fidst01l)
173200010625     C                   EXSR      SR_DESCRIZ1
173300010706     C                   ELSE
173400010706     C                   eval      *in28 = *on
173500010706     C                   eval      *in48 = *on
173600010706     C                   movel     msg(12)       V2CMSG
173700010706     C                   goto      endnumdis
173800010625     C                   endif
173900010625     C                   endif
174000010625     c                   seton                                        9048
174100010625     C                   add       1             conta             1 0
174200010625     c                   enddo
174300010625      *    Controllo numero Distinta
174400010706     C     V2CNDC2       ifeq      *all'0'
174500010625     C                   clear                   V2CNDC2
174600010706     C                   clear                   V2DNDC2
174700010625     C                   endif
174800010706     C     V2CNDC2       ifne      *blanks
174900041015      * se c'è la distinta a video quella immessa dall'utente deve essere uguale
175000041015     c                   If        v2cndc > *Zeros
175100120613     c                   move      v2cndc2       w0060             6 0
175200120613     c                   If        w0060 <> v2cndc
175300041015     C                   eval      *in28 = *on
175400041015     C                   eval      *in19 = *on
175500041015     C                   movel     msg(21)       V2CMSG
175600041015     C                   goto      endnumdis
175700041015     c                   EndIf
175800041015     c                   EndIf
175900120613     C                   move      '4'           kdstnpg
176000120613     C                   move      V2CNDC2       kdstnfv
176100120613     C                   move      V2CFGS        kdstfgs
176200120613     C     kfidst        chain     fidst01l
176300120613     C                   if        not %found(fidst01l)
176400060516     c                   clear                   fnlv55ds
176500060516     c                   eval      d55tpt = '6'
176600060516     c                   move      v2cfgs        d55lin
176700060516     c                   eval      d55drf = dateu
176800060516     c                   call      'FNLV55R'
176900060516     c                   parm                    fnlv55ds
177000060516     c                   if        d55err = *blanks and d55tfa <> d55lin
177100120613     c                   move      d55tfa        kdstfgs
177200120613     c     kfidst        chain     fidst01l
177300060516     c                   endif
177400060516     c                   endif
177500120613     C                   if        %found(fidst01l)
177600010706     C                   EXSR      SR_DESCRIZ1
177700120613     C     dstatb        ifne      *blanks
177800010625     C                   eval      *in28 = *on
177900010625     C                   eval      *in19 = *on
178000010625     C                   movel     msg(12)       V2CMSG
178100010625     C                   goto      endnumdis
178200010625     C                   else
178300010625      * Chiusa
178400120613     C     dstfcf        ifne      ' '
178500010625     C                   eval      *in28 = *on
178600010625     C                   eval      *in48 = *on
178700010625     C                   movel     msg(13)       v2cmsg
178800010625     C                   goto      endnumdis
178900010625     C                   endif
179000010706     C                   endif
179100010706     C                   else
179200010706     C                   eval      *in28 = *on
179300010706     C                   eval      *in48 = *on
179400010706     C                   movel     msg(12)       V2CMSG
179500010706     C                   goto      endnumdis
179600010706     C                   endif
179700010706     C                   else
179800010706     C                   if        v2cndc2 = *blanks
179900010706     C                   eval      *in28 = *on
180000010706     C                   eval      *in48 = *on
180100010706     C                   movel     msg(14)       v2cmsg
180200010706     C                   goto      endnumdis
180300010706     C                   endif
180400010706     C                   endif
180500010625
180600010625     C                   if        conta > 0
180700010625     C                   eval      *in90 = *off
180800010625     C                   endif
180900010625     C                   if        V2CNDC2 > *blanks and conta = *zeros
181000010625     C                   add       1             conta
181100010625     C                   eval      *in90 = *on
181200010625     C                   endif
181300010625
181400010625     C     endnumdis     endsr
181500010605      **********************************************************************
181600010605      * CONTROLLO  * Orm / bolla *
181700010605      **********************************************************************
181800010605     C     Sr_Bolla      BEGSR
181900010605
182000010605     C                   clear                   WORM             14
182100010605     C                   move      V2CPOE        WPOE              3
182200010605     C                   move      V2CNSR        WNSR              2
182300010605     C                   move      V2CNOR        WNOR              7
182400010605     C                   move      V2CNRV        WNRV              2
182500010605     C                   eval      worm = wpoe + wnsr
182600010605     C                             + wnor + wnrv
182700060214     C                   movel     worm          kar4n14
182800060214     C     kfiar4        chain     fiar404l
182900060214     C                   if        %found(fiar404l)
183000010605     C                   movel     msg(9)        V2CMSG
183100010605     C                   eval      *in28 = *on
183200010605     C                   goto      endsrbolla
183300010605     C                   endif
183400010605
183500010605     C     endsrbolla    endsr
183600010604      **********************************************************************
183700010604      * VISUALIZZAZIONE ORM
183800010604      **********************************************************************
183900010604     C     Sr_ORM        BEGSR
184000010604
184100010629     C                   movel     v2cpoe        ppoe
184200010629     C                   movel     v2CNOR        pnor
184300010629     C                   movel     v2CNSR        pnsr
184400010629     C                   movel     v2CNRV        pnrv
184500010629     C                   movel     '5'           psce
184600010629     C                   movel     v2cfgs        pfgs
184700010629
184800010629     C                   movel     kpjbu         §kpjbu
184900010629     C                   movel     parm01        kpjbu
185000010629     C                   call      'FIOR07R'
185100010629     C                   parm                    kpjba
185200010629     C                   movel     kpjbu         parm01
185300010629     C                   movel     §kpjbu        kpjbu
185400010629
185500010629     C                   ENDSR
185600040406      *-------------------------------------------------------------------------------*
185700040406      * Controllo se devo chiudere l'ORM in automatico in base i tentativi fatti
185800040406      *-------------------------------------------------------------------------------*
185900040406     c     Sr_ContrFase  BegSr
186000040406
186100040406     c                   Eval      wchiudi = *Off
186200040406     c                   Clear                   wconta
186300040406      * Mi posiziono sull'ultima fase dell'ORM
186400040406     c     kFnorf        Setll     Fnorf01l
186500040406     c                   Do        *Hival
186600040406      * Torno indietro a leggere
186700040406     c     kFnorf1       Readpe    Fnorf01l
186800040406      * Fine file
186900040406     c                   If        %Eof(Fnorf01l)
187000040406     c                   Leave
187100040406     c                   EndIf
187200040406      * se stessa fase torno a leggere
187300040406     c                   If        OrfFar = Sav_OrfFar or OrfFar = sav_OrmFao
187400040406     c                   Iter
187500040406     c                   EndIf
187600040406     c                   Eval      sav_OrfFar = OrfFar
187700040406      * fase senza causale
187800040406     c                   If        OrfCar = *Blanks
187900040406     c                   Iter
188000040406     c                   EndIf
188100040406      * controllo la causale della fase
188200040406     c                   Clear                   dCmr
188300040406     c                   Clear                   Tibs02ds
188400040406     c                   Eval      T02Mod = 'C'
188500040406     c                   Eval      T02Sif = knsif
188600040406     c                   Eval      T02Cod = 'CMR'
188700040406     c                   Eval      T02Ke1 = OrfCar
188800040406     c                   Call      'TIBS02R'
188900040406     c                   Parm                    kpjba
189000040406     c                   Parm                    Tibs02ds
189100040406     c                   Eval      dCmr = T02Uni
189200040413      * se non è da conteggiare torno a leggere
189300040413     c                   If        d§cmrccc = *Blanks
189400040406     c                   Iter
189500040406     c                   EndIf
189600040406
189700040406      * Conto
189800040406     c                   add       1             wconta
189900040406      * se raggiunto il max dei tentativi chiudo l'ORM
190000040406     c                   If        wconta >= wtenta
190100040406     c                   Eval      wchiudi = *On
190200040406     c                   Leave
190300040406     c                   EndIf
190400040406     c                   EndDo
190500040406
190600040406     c                   EndSr
190700040406      *-------------------------------------------------------------------------------*
190800040406      * Chiamo la manutenzione ORM x proporre la chiusura
190900040406      *-------------------------------------------------------------------------------*
191000040406     c     Sr_Chiudi     BegSr
191100040406
191200040406     c                   Clear                   Fior05ds
191300040406     c                   Eval      §rmtla = 'C'
191400040406     c                   Eval      §rmfpr = 'P'
191500040406     c                   Eval      §rmpoe = OrmPoe
191600040406     c                   Eval      §rmnsr = OrmNsr
191700040406     c                   Eval      §rmnor = OrmNor
191800040406     c                   Eval      §rmnrv = OrmNrv
191900040406     c                   Eval      §rmtor = OrmTor
192000040406     c                   Eval      §rmcor = OrmCor
192100040406     c                   Eval      §rmrso = OrmRso
192200040406     c                   Eval      §rmino = OrmIno
192300040406     c                   Eval      §rmcao = OrmCao
192400040406     c                   Eval      §rmloo = OrmLoo
192500040406     c                   Eval      §rmpro = OrmPro
192600040406     c                   Eval      §rmnao = OrmNao
192700040406     c                   Eval      §rmcra = OrmCra
192800040406     c                   Eval      §rmrsr = OrmRsr
192900040406     c                   Eval      §rminr = OrmInr
193000040406     c                   Eval      §rmcar = OrmCar
193100040406     c                   Eval      §rmlor = OrmLor
193200040406     c                   Eval      §rmprr = OrmPrr
193300040406     c                   Eval      §rmnar = OrmNar
193400040406     c                   Eval      §rmrer = OrmRer
193500040406     c                   Eval      §rmter = OrmTer
193600040406     c                   Eval      §rmcrc = OrmCrc
193700040406     c                   Eval      §rmrsc = OrmRsc
193800040406     c                   Eval      §rminc = OrmInc
193900040406     c                   Eval      §rmcac = OrmCac
194000040406     c                   Eval      §rmloc = OrmLoc
194100040406     c                   Eval      §rmprc = OrmPrc
194200040406     c                   Eval      §rmnac = OrmNac
194300040406     c                   Eval      §rmffd = OrmFfd
194400040406     c                   Eval      §rmdar = OrmDar
194500040406     c                   Eval      §rmorr = OrmOrr
194600040406     c                   Eval      §rmnam = OrmNam
194700040406     c                   Eval      §rmncl = OrmNcl
194800040406     c                   Eval      §rmpkg = OrmPkg
194900040406     c                   Eval      §rmvlm = OrmVlm
195000040406     c                   Eval      §rmbnc = OrmBnc
195100040406     c                   Eval      §rmblc = OrmBlc
195200040406     c                   Eval      §rmatt = OrmAtt
195300040406     c                   Eval      §rmmtc = OrmMtc
195400040406     c                   Eval      §rmpag = OrmPag
195500040406     c                   Eval      §rmksc = OrmKsc
195600040406     c                   Eval      §rmctr = OrmCtr
195700040406     c                   Eval      §rmpor = OrmPor
195800040406     c                   Eval      §rmzor = OrmZor
195900040406     c                   Eval      §rmpoc = OrmPoc
196000040406     c                   Eval      §rmno1 = OrmNo1
196100040406     c                   Eval      §rmno2 = OrmNo2
196200040406     c                   Eval      §rmcst = OrmCst
196300040406     c                   Eval      §rmvcs = OrmVcs
196400040406     c                   Eval      §rmfcs = OrmFcs
196500040406     c                   Eval      §rmddt = OrmDdt
196600040406     c                   Eval      §rmsto = OrmSto
196700040406     c                   Eval      §rmrfa = OrmRfa
196800040406     c                   Eval      §rmspi = OrmSpi
196900040406     c                   Eval      §rmcau = Sav_CmrCau
197000040406     c                   Eval      §rmflo = OrmFlo
197100040406     c                   Eval      §rmflv = 'B'
197200040406      * Passo alcuni parametri anche nella parm01
197300040406     c                   Clear                   parm01
197400040406     c                   eval      pmio = 'S'
197500041015     c                   Eval      pndc = savndc
197600041015     c                   Eval      pddc = savddc
197700120613     c                   eval      pdstfgs = savfgs
197800040406     c                   Eval      kpjbu = parm01
197900040406
198000040406     c                   Call      'FIOR05R'
198100040406     c                   Parm                    kpjba
198200040406     c                   Parm                    Fior05ds
198300040406
198400040406     c                   EndSr
198500010206      *****************************************************************
198600010206      * ROUTINE INIZIALE
198700010206      *****************************************************************
198800010206     C     *INZSR        BEGSR
198900010206
199000010206     C     *ENTRY        PLIST
199100010206     C                   PARM                    KPJBA
199200130620
199300130620     c     *dtaara       define    §azute        azuteds
199400130620     c     *dtaara       define    §datiute      ddatiute
199500130620     c                   in(e)     *dtaara
199600130620     c                   if        %error or rsut = *blanks
199700130620     c                   clear                   tibs34ds
199800130620     c                   call      'TIBS34R'
199900130620     c                   parm                    tibs34ds
200000130620     c                   in        *dtaara
200100130620     c                   endif
200200130620
200300130620      * controllo abilitazione utente
200400130620     c                   clear                   wabi
200500130620     c                   clear                   dLAT
200600130620     c                   eval      dUTE01 = UTEfaf
200700130620
200800130620      * Verifica errori e autorità profilo
200900130620     c                   SELECT
201000130620      * se ho errori nei dati utente esco dal pgm
201100130620     c                   WHEN      DUTerr = 'E'
201200130620     c                   eval      wErrGrave = *on
201300130620     c                   leavesr
201400130620      * carica abilitazioni del profilo
201500130620     c                   WHEN      §UTEorm <> *blanks
201600130620     c                   eval      wabi = §UTEorm
201700130620     c                   WHEN      UTEaut <> *blanks
201800130620     c                   eval      wabi = UTEaut
201900130620      * se non c'è l'abilitazione
202000130620      * --> se 1° livello, abilitazioni al terminal
202100130620      *     se 2° livello, abilitazioni al punto operativo
202200130620      *     se sede no abilitazioni
202300130620     c                   OTHER
202400130620     c                   IF        DUTlpo = '1'
202500130620     c                   eval      wabi   = 'TP'
202600130620     c                   ENDIF
202700130620     c                   IF        DUTlpo = '2'
202800130620     c                   eval      wabi   = 'PO'
202900130620     c                   ENDIF
203000130620     c                   ENDSL
203100130620
203200130620      * controllo se ok l'abilitazione dell'utente
203300130620     c                   clear                   TIBS02DS
203400130620     c                   eval      T02mod = 'C'
203500130620     c                   eval      T02sif = knsif
203600130620     c                   eval      T02cod = 'LAT'
203700130620     c                   eval      T02ke1 = wabi
203800130620     c                   Call      'TIBS02R'
203900130620     c                   Parm                    KPJBA
204000130620     c                   Parm                    TIBS02DS
204100130620     c                   IF        T02err <> *blanks or §LATabi = 'S'
204200130620     c                   eval      wErrGrave = *on
204300130620     c                   leavesr
204400130620     c                   ENDIF
204500130620     c                   eval      dLAT = T02uni
204600130620
204700130620      * Reperimento delle filiali gestibili dall'utente
204800130620     c                   clear                   TRUL31DS
204900130620     c                   eval      I31abi = wabi
205000130620     c                   eval      I31cdi = DUTdis
205100130620     c                   eval      I31car = DUTare
205200130620     c                   eval      I31cpo = DUTpou
205300130620     c                   call      'TRUL31R'
205400130620     c                   parm                    KPJBA
205500130620     c                   parm                    TRUL31DS
205600130620     c                   IF        O31pog > *zeros
205700130620     c                   movea     O31pog        SKpog
205800130620     c                   ELSE
205900130620     c                   eval      wErrGrave = *on
206000130620     c                   leavesr
206100130620     c                   ENDIF
206200010206
206300010206      * IMPOSTO IL P.O. DI GESTIONE
206400130620     C*****              if        simtpp = '2' or simtpp = *blanks
206500130620     C                   movel     DUTpou        V1Cfgs
206600130620     C*****              eval      *in06 = *on
206700130620     C*****              else
206800130620     C*****              movel     SIMFEL        V1Cfgs
206900130620     C*****              endif
207000010206      * Carico schiera P.O. gestiti
207100130620     C*****              CLEAR                   TRUL06ds
207200130620     C*****              MOVE      '£1'          D06COD
207300130620     C*****              MOVEL     SIMFEL        D06KEY
207400130620     C*****              MOVEL     TRUL06DS      KPJBU
207500130620     C*****              CALL      'TRUL06R'
207600130620     C*****              PARM                    KPJBA
207700130620     C*****              MOVEL     KPJBU         TRUL06DS
207800130620     C*****              MOVEA     LIN           L1
207900010206      * Carico £6
208000010206     C                   CLEAR                   trul06ds
208100010206     C                   MOVE      '£6'          D06COD
208200060516     C                   MOVEL     V1CFGS        D06KEY
208300010206     C                   MOVEL     trul06ds      KPJBU
208400010206     C                   CALL      'TRUL06R'
208500010206     C                   PARM                    KPJBA
208600010206     C                   MOVEL     KPJBU         trul06ds
208700010206     C                   MOVEA     LIN           L6
208800010206      * Decodifico P.o. gestione
208900010604     C     v1cfgs        chain     azorg01L
209000010206     C                   if        %found(azorg01l)
209100010530     C                   movel     orgDES        v1dfgs
209200010206     C                   else
209300010530     C                   clear                   v1dfgs
209400010206     C                   endif
209500010206      * reperisco data e ora
209600010206     C                   TIME                    W0140
209700010206      * UDATE IN GGMMAAAA
209800010206     C                   MOVE      W0140         WDTGIO
209900010206      * UDATE IN AAAAMMGG
210000010206     C                   Z-ADD     WDTGIO        G02DAT
210100010206     C                   MOVEL     *BLANK        G02ERR
210200010206     C                   CALL      'XSRDA8'
210300010206     C                   PARM                    WLBDAT
210400010206     C                   MOVEL     G02INV        DATEU
210500010206
210600010530     C     kfnorm        klist
210700010604     C                   kfld                    v1cpoe
210800010604     C                   kfld                    v1cnsr
210900010604     C                   kfld                    v1cnor
211000010604     C                   kfld                    v1cnrv
211100010206
211200010605     C     kfnorF        klist
211300010605     C                   kfld                    ORMpoe
211400010605     C                   kfld                    ORMnsr
211500010605     C                   kfld                    ORMnor
211600010605     C                   kfld                    ORMnrv
211700010605     C                   kfld                    ORMDFO
211800010605     C                   kfld                    ORMOFO
211900010605     C                   kfld                    ORMFAO
212000020524     C     kfnorF1       klist
212100020524     C                   kfld                    ORMpoe
212200020524     C                   kfld                    ORMnsr
212300020524     C                   kfld                    ORMnor
212400020524     C                   kfld                    ORMnrv
212500010605
212600120613     C     Kfidst        KLIST
212700120613     C                   kfld                    kdstnpg
212800120613     C                   kfld                    kdstnfv
212900120613     C                   kfld                    kdstfgs
213000010605
213100060214     C     kfiar4        klist
213200060214     C                   kfld                    Kar4trc
213300060214     C                   kfld                    kar4n14
213400060214     C                   movel     'M'           kar4trc
213500021202
213600021202     C     Kfapd         klist
213700021202     C                   kfld                    kpdtip
213800021202     C                   kfld                    kpdpdr
213900010605
214000010206     C                   endsr
214100010206      *---------------------------------------------------------------------------------------------
214200001113** MSG
214300080423Filiale gestione errata                                                              1
214400010604Numero O.R.M. errato                                                                 2
214500010604Numero O.R.M. annullato                                                              3
214600010628O.R.M. non in gestione al punto operativo                                            4
214700010705O.R.M. in fase non modificabile                                                      5
214800010605Immettere una sola scelta                                                            6
214900010605Causale Errata                                                                       7
215000010605Causale non gestibile                                                                8
215100010628L'ORM è stato bollettato: non si può dichiarare NON FATTO                            9
215200010605ORM allocato da altro utente                                                        10
215300010605ORM già stato modificato                                                            11
215400010625Numero distinta errato                                                              12
215500010625Distinta non aperta                                                                 13
215600010625Immettere Numero Distinta                                                           14
215700010629Effettuare almeno una scelta                                                        15
215800010629Immettere codice causale                                                            16
215900010706Il valore immesso per il campo non è valido. Immettere 'SI'                         17
216000010706ORM già chiuso/quadrato con la stessa causale                                       18
216100020524ATTENZIONE!! L'ORM è in fase 900. Non si può modificare                             19
216200020524ATTENZIONE!! Annullare la bolla di addebito creata in precedenza                    20
216300041015La distinta deve essere uguale a quella già presente sulla fase ORM                 21
216400070516ORM in uso da altro utente, riprovare più tardi!                                    22
216500081030ORM Commissionato. Non si può usare più di 1 causale con resp. al cliente           23
216600010629** LAV
216700010629Quadratura...............:                                                           1
216800010629Quadratura con causale(?):                                                           2
216900010706Riapertura...............:                                                           3
217000020524** CM3
217100030307OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
217200030307 SHARE(*YES)
217300030623** cmA4
217400030623OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
217500030623 SHARE(*YES)          USRDTA('BolAdd_A4')
217600030623**  cmA5
217700030623OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
217800030623 SHARE(*YES)          USRDTA('BolAdd_A5')
