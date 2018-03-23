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
008200010206
008300010206      *   S C H I E R E
008400081030     D MSG             S             78    DIM(25) CTDATA PERRCD(1)             MSG VIDEO
008500010706     D LAV             S             26    DIM(03) CTDATA PERRCD(1)
008600001113     D L1              S              3  0 DIM(30)                              P.O. Gestiti £1
008700001113     D L6              S              3  0 DIM(30)                              P.O. Gestiti £6
008800030307     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
008900030623      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
009000030623     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
009100030623     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
009200010119
009300010206      *   D S   I N T E R N E / E S T E R N E
009400010206     D WLBDAT          DS                  INZ
009500010206     D  G02DAT                 1      8  0
009600010206     D  G02INV                 9     16  0
009700010206     D  G02ERR                17     17
009800010206     D  G02TGI                18     22  0
009900020524
010000020524     D                 DS
010100020524     d  ormcra1                1      3  0
010200020524     d  ormcra2                4      7  0
010300020524     d  ormcra3                8     10  0
010400020524     d  ds_ormcra              1     10  0
010500020524
010600020524     D                 DS
010700020524     d  ormcor1                1      3  0
010800020524     d  ormcor2                4      7  0
010900020524     d  ormcor3                8     10  0
011000020524     d  ds_ormcor              1     10  0
011100020524
011200020524     D                 DS
011300020524     d  ormcrc1                1      3  0
011400020524     d  ormcrc2                4      7  0
011500020524     d  ormcrc3                8     10  0
011600020524     d  ds_ormcrc              1     10  0
011700020524
011800020524     D                 DS
011900020524     D  ds_poe                 1      3  0
012000020524     D  ds_nsr                 4      5  0
012100020524     D  ds_nor                 6     12  0
012200020524     D  ds_nrv                13     14  0
012300020524     D  ds_keyorm              1     14  0
012400010206
012500001116     D Parm01          DS
012600001116     D  ppoe                          3  0
012700001116     D  pnor                          7  0
012800001116     D  pnsr                          2  0
012900001116     D  pnrv                          2  0
013000001116     D  psce                          1
013100001116     D  pfgs                          3  0
013200001116     D  ppor                          3  0
013300001116     D  pdtr                          8  0
013400001116     D  pmdb                         10
013500001116     D  pprb                         10
013600001116     D  pdts                          8  0
013700001116     D  prmp                          1
013800010130     D  pbrc                          1
013900010130     D  pref                          2
014000040406     D  pmio                          1
014100041015     d  pndc                          6  0
014200041015     d  pddc                          8  0
014300120613     d  pdstfgs                       3  0
014400001113
014500020524     D DTASV         E DS
014600020524     D DBLP          E DS
014700030623     D TRUL90DS      E DS
014800010605     D DFAR          E DS
014900010301     D DCMR          E DS
015000081030     d a_dcmr        e ds                  extname(dcmr) prefix(a_)
015100010530     D FIOR06DS      E DS
015200080319     d fior06vds     e ds
015300010206     D TIBS02DS      E DS
015400010206     D DSLR01        E DS                  inz
015500010206     D UT§DSE0F      E DS
015600001113     D CNCR80        E DS
015700010206     D TRUL06DS      E DS
015800001113     D  LIN                    1     90  0
015900001113     D                                     DIM(30)
016000040406     d Fior05ds      e ds
016100060516     d fnlv55ds      e ds
016200080423     d og143         e ds
016300070926     d og147         e ds
016400080314     d dorg01        e ds
016500081030     d dorm01        e ds
016600081125     d dorf01        e ds
016700010206
016800010206     D KPJBA         E DS
016900010206
017000010206     D                SDS
017100010206     D  VTCPGM                 1     10
017200001113
017300010605      * Scomposizione Descrizione Distinta
017400010605     D                 DS
017500010605     D  v2DNDC                 1     40
017600010605     D  wdel                   1      4
017700010605     D  wggg                   5      6
017800010605     D  wbarra1                7      7
017900010605     D  wmmm                   8      9
018000010605     D  wbarra2               10     10
018100010605     D  waaa                  11     15
018200010605     D  wpdr                  16     23
018300010605     D  wdescr                24     40
018400010625      * Scomposizione Descrizione Distinta
018500010625     D                 DS
018600010625     D  v2DNDC2                1     40
018700010625     D  wdel1                  1      4
018800010625     D  wggg1                  5      6
018900010625     D  wbarra3                7      7
019000010625     D  wmmm1                  8      9
019100010625     D  wbarra4               10     10
019200010625     D  waaa1                 11     15
019300010625     D  wpdr1                 16     23
019400010625     D  wdescr1               24     40
019500010206      *------------------------------------------------------------------------*
019600010530      * Emissione Videata
019700010206
019800010206     C                   do        *hival
019900010530      * Pulizia Videata
020000010604     C                   if        *in28 = *off
020100010530     C                   exsr      Sr_Pulvid
020200010206     C                   endif
020300010206
020400010604     C                   exfmt     OR27D01
020500010206      * F3=Fine
020600010206     C   kc              leave
020700010206
020800010604     C                   setoff                                       28
020900010206      * F18=Cambio P.O. in gestione
021000010206     C   ks              eval      *in05 = *on
021100010206     C   ks              iter
021200010206
021300010206      * 05 on --> controllo P.O. in gestione
021400010206     C                   if        *in05 = *on
021500010604     C                   exsr      Sr_Contrfgs
021600010206     C                   eval      *in05 = *off
021700080423     C   28              iter
021800010206     C                   endif
021900010530      * Controlli videata
022000010625     C                   setoff                                       2841
022100010625     C                   exsr      Sr_Controrm
022200010206
022300010206     C   28              iter
022400010604      *--------------
022500010605     C                   SETOFF                                       101112
022600041015     C                   SETOFF                                       16
022700020529     c                   eval      *in45 = *off
022800010604     C                   EXSR      SR_CAR2VID
022900010605     C     EMETTO        TAG
023000010605     C                   EXFMT     OR27D02
023100010629     C*
023200020529     C                   SETOFF                                       28
023300010605      * F3=Fine
023400010605     C   kc              leave
023500010605
023600010605      * F12=Ritorno
023700010605     C   kl              iter
023800010605
023900010629      * F5=Visualizza ORM
024000010629     C                   if        *inke = *on and v1cnor <> 0
024100010629     C                   exsr      Sr_Orm
024200010709     C                   GOTO      EMETTO
024300010629     C                   endif
024400010629
024500010605     C                   EXSR      SR_CONTR2
024600010625     C                   IF        *IN28 = *ON  OR *IN90 = *ON
024700010605     C                   GOTO      EMETTO
024800010605     C                   ENDIF
024900010604      *--------------
025000020524      * F6=Conferma
025100010605     C                   IF        *INKF = *ON
025200010605     C                   exsr      Sr_Conferma
025300020524      * ORM allocato o ORM già in fase 900 al momento della modifica
025400020524     C                   if        *in44  or  *in46
025500020524     c                   seton                                        28  47
025600010605     C                   goto      EMETTO
025700010605     C                   endif
025800020529      * se non faccio la bolla riemetto la videata
025900020529     c                   if        okbolla = '1'
026000020529     c                   goto      emetto
026100020529     c                   endif
026200010605      * Se la causale prevede le note queste sono obbligatorie
026300010605     C                   EXSR      SR_CONTRNOTE
026400040406      * se ho quadrato con causale controllo quanti tentativi posso fare prima di chiudere
026500040406      * l'ORM in automatico
026600040413     c                   If        wcausa <> *Blanks and d§cmrnca <> *Zeros
026700040406     c                   ExSr      Sr_ContrFase
026800040406     c                   If        wchiudi = *On
026900040406     c                   ExSr      Sr_Chiudi
027000040406     c                   EndIf
027100040406     c                   EndIf
027200040406
027300010605     C                   ELSE
027400010605     C                   GOTO      EMETTO
027500010605     C                   ENDIF
027600010605
027700010605     C                   enddo
027800010605
027900010206     C                   eval      *inlr = *on
028000010206      *****************************************************************
028100010530      * PULIZIA VIDEATA
028200010206      *****************************************************************
028300010530     C     Sr_Pulvid     BEGSR
028400010206
028500010604     C                   clear                   v1cpoe
028600010604     C                   clear                   v1cnsr
028700010604     C                   clear                   v1cnor
028800010604     C                   clear                   v1cnrv
028900010206
029000010206     C                   endsr
029100010206      **********************************************************************
029200010206      * CONTROLLO IL P.O. DI RITIRO
029300010206      **********************************************************************
029400010604     C     Sr_Contrfgs   begsr
029500010206
029600010604     C                   setoff                                       40
029700010604     C                   clear                   v1dfgs
029800070926     c                   clear                   og147
029900010206
030000010604     C     v1cfgs        lookup    L1                                     30
030100010206     C     *in30         ifeq      *off
030200010206     C                   seton                                        40  28
030300010206     C                   movel     msg(1)        v1cmsg
030400010206     C                   else
030500010206      * Decodifica
030600010604     C     v1cfgs        chain     azorg01L
030700010206     C                   if        %found(azorg01l) and ORGfva = *blanks
030800010604     C                   movel     orgDES        v1dfgs
030900070926     c                   eval      og147 = orgde7
031000010206     C                   else
031100010206     C                   seton                                        40  28
031200010206     C                   movel     msg(1)        v1cmsg
031300010206     C                   endif
031400010207      * Devo ricaricare la £6
031500010207     C                   CLEAR                   trul06ds
031600010207     C                   MOVE      '£6'          D06COD
031700010604     C                   MOVEL     v1cfgs        D06KEY
031800010207     C                   MOVEL     trul06ds      KPJBU
031900010207     C                   CALL      'TRUL06R'
032000010207     C                   PARM                    KPJBA
032100010207     C                   MOVEL     KPJBU         trul06ds
032200010207     C                   MOVEA     LIN           L6
032300010206     C                   endif
032400070926      * data partenza procedura in organigramma
032500071026     c                   eval      *in07 = *off
032600071026     c                   if        §ogddao > *zeros and §ogcgio = 'S'
032700070926     c                   move      §ogddao       data_org
032800071026      * controllo se attiva
032900071026     c                   if        dateu >= data_org
033000071026     c                   eval      *in07 = *on
033100071026     c                   endif
033200071026     c                   endif
033300010206
033400010206     C                   endsr
033500010206      **********************************************************************
033600010530      * CONTROLLO   * Numero ORM *
033700010206      **********************************************************************
033800010530     C     Sr_Controrm   begsr
033900010206
034000070516     C     KFNORM        CHAIN(e)  FNORM01L                           31
034100070516     c                   if        %error
034200070516     c                   eval      *in28 = *on
034300070516     c                   eval      *in41 = *on
034400070516     c                   eval      v1cmsg = msg(22)
034500070516     c                   leavesr
034600070516     c                   endif
034700010604     C                   IF        *IN31 = *ON
034800010604     C                   SETON                                        2841
034900010604     C                   MOVE      MSG(2)        V1CMSG
035000010604     C                   GOTO      ENDSRCORM
035100010604     C                   ENDIF
035200010206
035300010604     C                   IF        ORMATB <> *BLANKS
035400010604     C                   SETON                                        2841
035500010604     C                   MOVE      MSG(3)        V1CMSG
035600010604     C                   GOTO      ENDSRCORM
035700010604     C                   ENDIF
035800010604
035900010604     C                   IF        ORMPOR <> V1CFGS
036000060516     c     ormpor        lookup    l6                                     30
036100060516     c                   if        not *in30
036200010604     C                   SETON                                        2841
036300010604     C                   MOVE      MSG(4)        V1CMSG
036400010604     C                   GOTO      ENDSRCORM
036500060516     c                   endif
036600010604     C                   ENDIF
036700010604
036800010604     C                   IF        ORMFAO < 500
036900010604     C                   SETON                                        2841
037000010604     C                   MOVE      MSG(5)        V1CMSG
037100010604     C                   GOTO      ENDSRCORM
037200010604     C                   ENDIF
037300010706
037400010706     C                   IF        ORMFAO <> 500 AND ORMFAO <> 600 AND
037500030901     C                             ORMFAO <> 999 and OrmFao <> 910
037600010706     C                   SETON                                        2841
037700010706     C                   MOVE      MSG(5)        V1CMSG
037800010706     C                   GOTO      ENDSRCORM
037900010706     C                   ENDIF
038000081030
038100081030     c                   eval      dorm01 = ormflo
038200010604
038300010530     C     endsrcorm     endsr
038400010604      *****************************************************************
038500010604      * CARICO I DATI DELLA SECONDA VIDEATA
038600010604      *****************************************************************
038700010604     C     SR_CAR2VID    BEGSR
038800010604
038900041015     c                   Clear                   savndc
039000041015     c                   Clear                   savddc
039100060516     c                   Clear                   savfgs
039200041015
039300010706     C                   CLEAR                   V2DNDC
039400010606     C                   CLEAR                   V2CCAU1
039500010606     C                   CLEAR                   V2DCAU1
039600010706     C                   CLEAR                   V2CLAV
039700010606     C                   CLEAR                   V2DCAU2
039800010702     C                   CLEAR                   V2CNDC2
039900010702     C                   CLEAR                   V2DNDC2
040000010604     C                   EVAL      V2CFGS = V1CFGS
040100010604     C                   EVAL      V2DFGS = V1DFGS
040200010604     C                   EVAL      V2CPOE = V1CPOE
040300010604     C                   EVAL      V2CNSR = V1CNSR
040400010604     C                   EVAL      V2CNOR = V1CNOR
040500010604     C                   EVAL      V2CNRV = V1CNRV
040600010604     C                   EVAL      V2RSR = ORMRSR
040700010604     C                   EVAL      V2INR = ORMINR
040800010604     C                   EVAL      V2CAR = ORMCAR
040900010604     C                   EVAL      V2LOR = ORMLOR
041000010604     C                   EVAL      V2PRR = ORMPRR
041100010604     C                   EVAL      V2NAR = ORMNAR
041200010604     C                   EVAL      V2FAO = ORMFAO
041300010604     C                   EVAL      V2ORR = ORMORR
041400010605     C                   EVAL      V2CPOR = ORMPOR
041500010604     C                   EVAL      V2CNDC = ORMNDC
041600060516     c                   eval      v2hfgs = ormpor
041700010709     C                   EVAL      WDAO = ORMDAO
041800010629     C                   EVAL      V2CTOR = ORMTOR
041900010709     C*
042000010709      * MITTENTE
042100010709     C                   MOVEL     ORMCRA        V2CRA1
042200010709     C                   MOVE      ORMCRA        WCRA              7 0
042300010709     C                   MOVEL     WCRA          V2CRA2
042400010709     C                   MOVE      WCRA          V2CRA3
042500010706     C*
042600010706      *DATA RITIRO
042700010706     C                   EVAL      G02INV = ORMDAR
042800010706     C                   MOVEL     '3'           G02ERR
042900010706     C                   CALL      'XSRDA8'
043000010706     C                   PARM                    WLBDAT
043100010706     C                   EVAL      V2DAR = G02DAT
043200010706     C*
043300010706      *DATA IMMISSIONE
043400010706     C                   EVAL      G02INV = ORMDAO
043500010706     C                   MOVEL     '3'           G02ERR
043600010706     C                   CALL      'XSRDA8'
043700010706     C                   PARM                    WLBDAT
043800010706     C                   EVAL      V2DAO = G02DAT
043900010629     C*
044000010604      * CAUSALE
044100010605     C                   IF        ORMFAO = 500 OR ORMFAO = 999
044200030901     c                             or OrmFao = 910
044300041015     C     KFNORF        CHAIN     FNORF01L                           31
044400010605     C                   IF        *IN31 = *OFF
044500010605     C                   MOVEL     ORFCAR        V2CCAU
044600041015     c                   Eval      v2cndc = OrfNdc
044700060516     c                   Eval      v2hfgs = orffgs
044800010709     C                   ELSE
044900010709     C                   CLEAR                   V2CCAU
045000010709     C                   CLEAR                   V2DCAU
045100041015     c                   Clear                   v2cndc
045200060516     c                   clear                   v2hfgs
045300010605     C                   ENDIF
045400010709     C                   ELSE
045500010709     C                   CLEAR                   V2CCAU
045600010709     C                   CLEAR                   V2DCAU
045700010605     C                   ENDIF
045800010629     C*
045900010629     C                   IF        V2FAO <> *ZEROS
046000010629     C                   EVAL      *IN13 = *ON
046100010709     C                   ELSE
046200010709     C                   EVAL      *IN13 = *OFF
046300010629     C                   ENDIF
046400010629     C*
046500010629     C                   IF        V2CCAU <> *BLANKS
046600010629     C                   EVAL      *IN15 = *ON
046700010709     C                   ELSE
046800010709     C                   EVAL      *IN15 = *OFF
046900010629     C                   ENDIF
047000010604      * DECSRIZIONI
047100010605     C                   EXSR      SR_FASE
047200020524     c                   clear                   sav_cmradd
047300010605     C                   IF        V2CCAU <> *BLANKS
047400010605     C                   MOVEL     V2CCAU        WCAUSA
047500010605     C                   EXSR      SR_CAUSALE
047600010605     C                   movel     t02uni        dcmr
047700010605     C                   movel     D§CMRDES      V2DCAU
047800020524     c                   eval      sav_cmradd = d§cmradd
047900010605     C                   EVAL      *IN28 = *OFF
048000010605     C                   ENDIF
048100010605     C                   EXSR      SR_PORITIRO
048200010605     C                   EXSR      SR_DISTINTA
048300080423      * controllo se ORM commissionato da DPD
048400080423     c                   exsr      sr_comdpd
048500010605      * GESTIONE FASI
048600010629     C                   MOVE      *BLANKS       V2DLAV
048700010629     C                   MOVEL     *BLANKS       V2DCAU2
048800010629     C*
048900010605     C                   IF        V2FAO = 500
049000010605     C                   SETON                                        10
049100010629     C                   MOVEL     LAV(1)        V2DLAV
049200010629     C                   MOVEL     '(SI)'        V2DCAU2
049300010605     C                   ENDIF
049400010605
049500010605     C                   IF        V2FAO = 600
049600041015     C                   SETON                                        1116
049700010629     C                   MOVEL     LAV(2)        V2DLAV
049800010706     C                   MOVE      '?'           V2CLAV
049900010605     C                   ENDIF
050000010605
050100030901     C                   IF        V2FAO = 999 or V2Fao = 910
050200010605     C                   SETON                                        12
050300010706     C                   MOVEL     LAV(3)        V2DLAV
050400010706     C                   MOVEL     '(SI)'        V2DCAU2
050500041015     c                   if        d§cmrnoq <> *blanks
050600041015     C                   SETON                                        16
050700041015     c                   endif
050800010605     C                   ENDIF
050900010605
051000010605     C                   endsr
051100010605      *****************************************************************
051200010605      *    DESCRIZIONE FASE                                           *
051300010605      *****************************************************************
051400010605     C     SR_FASE       begsr
051500010605
051600010605     C                   CLEAR                   dfar
051700010605     C                   clear                   TIBS02DS
051800010605     C                   movel     'C'           T02mod
051900010605     C                   movel     knsif         t02sif
052000010605     C                   movel     'FAR'         t02cod
052100010605     C                   movel(p)  ormfao        T02ke1
052200010605     C                   call      'TIBS02R'
052300010605     C                   parm                    KPJBA
052400010605     C                   parm                    TIBS02DS
052500010605     C                   movel     T02uni        dfar
052600010605     C                   MOVEL     D§FARDES      V2DFAO
052700010605
052800010605     C                   endsr
052900010605      *****************************************************************
053000010605      *    DESCRIZIONE CAUSALE                                        *
053100010605      *****************************************************************
053200010605     C     Sr_CAUSALE    begsr
053300010605
053400010605     C                   clear                   dcmr
053500010605     C                   clear                   TIBS02DS
053600010605     C                   movel     'C'           T02mod
053700010605     C                   movel     knsif         t02sif
053800010605     C                   movel     'CMR'         t02cod
053900010605     C                   movel(p)  WCAUSA        T02ke1
054000010605     C                   call      'TIBS02R'
054100010605     C                   parm                    KPJBA
054200010605     C                   parm                    TIBS02DS
054300010605     C                   if        t02err <> *blanks
054400010605     C                   movel     msg(7)        v2CMSG
054500010605     C                   eval      *in28 = *on
054600010605     C                   endif
054700010605
054800010605     C                   ENDSR
054900010605      *****************************************************************
055000010605      *    DESCRIZIONE P.O. RITIRO                                    *
055100010605      *****************************************************************
055200010605     C     Sr_PORITIRO   begsr
055300010605
055400070926     c                   clear                   og147
055500010605     C     V1CFGS        CHAIN     AZORG01L                           31
055600010605     C                   IF        *IN31 = *OFF
055700010605     C                   MOVEL     ORGDES        V2DPOR
055800070926     c                   eval      og147 = orgde7
055900010605     C                   ENDIF
056000070926      * data partenza procedura in organigramma
056100071026     c                   eval      *in07 = *off
056200071026     c                   if        §ogddao > *zeros and §ogcgio = 'S'
056300070926     c                   move      §ogddao       data_org
056400071026      * controllo se attiva
056500071026     c                   if        dateu >= data_org
056600071026     c                   eval      *in07 = *on
056700071026     c                   endif
056800071026     c                   endif
056900010605
057000010605     C                   endsr
057100010605      *****************************************************************
057200010605      *    DESCRIZIONE DISTINTA                                       *
057300010605      *****************************************************************
057400010605     C     Sr_DISTINTA   begsr
057500010605
057600010605     C     V2CNDC        ifne      *ZEROS
057700120613     C                   z-add     4             kdstnpg
057800120613     C                   z-add     v2CNDC        kdstnfv
057900120613     C                   z-add     v2CFGS        kdstfgs
058000120613     C     kfidst        chain     fidst01l
058100120613     c                   if        not %found(fidst01l)
058200060516     c                   clear                   fnlv55ds
058300060516     c                   eval      d55tpt = '6'
058400060516     c                   move      v2cfgs        d55lin
058500060516     c                   eval      d55drf = dateu
058600060516     c                   call      'FNLV55R'
058700060516     c                   parm                    fnlv55ds
058800060516     c                   if        d55err = *blanks and d55tfa <> d55lin
058900120613     c                   move      d55tfa        kdstfgs
059000120613     c     kfidst        chain     fidst01l
059100060516     c                   endif
059200060516     c                   endif
059300120613     c                   if        %found(fidst01l)
059400010605     C                   EXSR      SR_DESCRIZ
059500120613     c                   eval      v2hfgs = kdstfgs
059600010605     C                   endif
059700010605     C                   endif
059800010605
059900010605     C                   endsr
060000010605      *****************************************************************
060100010605      *    Recupero Descrizione Distinta                              *
060200010605      *****************************************************************
060300010605
060400010605     C     SR_DESCRIZ    begsr
060500010605
060600021202     C                   movel     DSTpdr        kpdpdr
060700021202     C                   move      'A'           kpdtip
060800021202     C     kfapd         chain     fiapd01L                           31
060900010605     C                   if        *IN31 = *ON
061000010605     C                   clear                   apdrsc
061100010605     C                   ENDIF
061200010605
061300010605     C                   movel     'DEL '        wdel
061400120613     C                   move      dstdfv        wggg
061500010605     C                   move      '/'           wbarra1
061600010605     C                   move      '/'           wbarra2
061700120613     C                   move      dstdfv        wa4               4
061800010605     C                   movel     wa4           wmmm
061900120613     C                   movel     dstdfv        wa4               4
062000010605     C                   movel(p)  wa4           waaa
062100010605     C     dstpdr        ifne      0
062200010605     C                   movel(p)  dstpdr        wpdr
062300010605     C                   else
062400010605     C                   clear                   wpdr
062500010605     C                   endif
062600010625     C                   movel     apdrsc        wdescr
062700010605
062800010605     C                   endsr
062900010625      *****************************************************************
063000010625      *    Recupero Descrizione Distinta                              *
063100010625      *****************************************************************
063200010625     C     SR_DESCRIZ1   begsr
063300010625
063400021202     C                   movel     DSTpdr        kpdpdr
063500021202     C                   move      'A'           kpdtip
063600021202     C     kfapd         chain     fiapd01L                           31
063700010625     C                   if        *IN31 = *ON
063800010625     C                   clear                   apdrsc
063900010625     C                   ENDIF
064000010625
064100010625     C                   movel     'DEL '        wdel1
064200120613     C                   move      dstdfv        wggg1
064300010625     C                   move      '/'           wbarra3
064400010625     C                   move      '/'           wbarra4
064500120613     C                   move      dstdfv        wa41              4
064600010625     C                   movel     wa41          wmmm1
064700120613     C                   movel     dstdfv        wa41              4
064800010625     C                   movel(p)  wa41          waaa1
064900010625     C     dstpdr        ifne      0
065000010625     C                   movel(p)  dstpdr        wpdr1
065100010625     C                   else
065200010625     C                   clear                   wpdr1
065300010625     C                   endif
065400010625     C                   movel     apdrsc        wdescr1
065500010625
065600010625     C                   endsr
065700080423
065800080423      *--------------------------------------------------------------------*
065900080423      * Controllo se orm commissionato da DPD
066000080423      *--------------------------------------------------------------------*
066100080423     c     sr_comdpd     begsr
066200080423
066300080423     c                   clear                   og143
066400080423     c                   eval      $comdpd = *off
066500080423     c     ormpoe        chain     azorg01l
066600080423     c                   if        not %found(azorg01l)
066700080423     c                   leavesr
066800080423     c                   endif
066900080423     c                   eval      og143 = orgde3
067000080423     c                   if        §ogntw = 'DPD'
067100080423     c                   eval      $comdpd = *on
067200080423     c                   endif
067300080423
067400080423     c                   endsr
067500080423
067600010206      **********************************************************************
067700010206      * CONTROLLO  * Causale mancato ritiro *
067800010206      **********************************************************************
067900010605     C     Sr_Contrcau   BEGSR
068000010206
068100010605     C                   eval      *in28 = *off
068200010206
068300010605     C     '?'           scan      WCAUSA                                 30
068400010206     C     *in30         ifeq      *on
068500010301     C                   clear                   dcmr
068600010206     C                   clear                   TIBS02DS
068700010206     C                   movel     'R'           t02mod
068800010206     C                   movel     knsif         t02sif
068900010206     C                   movel     'CMR'         t02cod
069000010206     C                   call      'TIBS02R'
069100010206     C                   parm                    KPJBA
069200010206     C                   parm                    TIBS02DS
069300010605     C                   movel     T02ke1        WCAUSA
069400010206     C                   endif
069500010206      * Controllo Causale
069600010629     C                   IF        WCAUSA = *BLANKS
069700010629     C                   MOVEL     MSG(16)       V2CMSG
069800010629     C                   EVAL      *IN28 = *ON
069900010706     C                   CLEAR                   V2CCAU1
070000010629     C                   GOTO      ENDCAU
070100010629     C                   ENDIF
070200010629     C*
070300010706     C                   if        WCAUSA <> *blanks
070400010706     C                   EXSR      SR_CAUSALE
070500060215     c   28              goto      endcau
070600010706     C                   movel     t02ke1        WCAUSA
070700010706     C                   movel     t02uni        dcmr
070800010706     C                   MOVEL     D§CMRDES      WDESCAU
070900020524     c                   eval      new_cmradd = d§cmradd
071000040413     c                   Eval      wtenta = d§cmrnca
071100040413     c                   Move      d§cmrcca      sav_cmrcau
071200010706     C*
071300010606     C                   IF        *IN11 = *ON
071400010706     C                   EVAL      V2CLAV = WCAUSA
071500010606     C                   EVAL      V2DCAU2 = WDESCAU
071600010606     C                   ENDIF
071700010606
071800010606     C                   IF        *IN12 = *ON OR *IN10 = *ON
071900010606     C                   EVAL      V2CCAU1 = WCAUSA
072000010606     C                   EVAL      V2DCAU1 = WDESCAU
072100010606     C                   ENDIF
072200010206
072300031006     c                   If        d§CmrNmf <> *Blanks
072400010706     C                   movel     msg(8)        v2cmsg
072500010706     C                   eval      *in28 = *on
072600010706     C                   GOTO      ENDCAU
072700010706     C                   endif
072800010709     C                   if        V2CCAU = WCAUSA
072900010706     C                   movel     msg(18)       v2cmsg
073000010706     C                   eval      *in28 = *on
073100010706     C                   GOTO      ENDCAU
073200010706     C                   endif
073300081030
073400081125      * se causale inserita genera bolla di addebito (colpa cliente)
073500081030      * controllo che l'ORM non abbia già avuto una causale che genera addebito
073600081125      * in questo caso faccio solo un addebito
073700081125     c                   if        d§cmradd = 'S'
073800081030     c     kfnorm        setll     fnorf01l
073900081030     c                   do        *hival
074000081030     c     kfnorm        reade(n)  fnorf01l
074100081030     c                   if        %eof (fnorf01l)
074200081030     c                   leave
074300081030     c                   endif
074400081030      * senza causale rileggo
074500081030     c                   if        orfcar = *blanks
074600081030     c                   iter
074700081030     c                   endif
074800081030      * fase da PDA rileggo
074900081030     c                   if        orffar = 410
075000081030     c                   iter
075100081030     c                   endif
075200081030     c                   clear                   a_dcmr
075300081030     c                   clear                   tibs02ds
075400081030     c                   eval      t02mod = 'C'
075500081030     c                   eval      t02sif = knsif
075600081030     c                   eval      t02cod = 'CMR'
075700081030     c                   eval      t02ke1 = orfcar
075800081030     c                   call      'TIBS02R'
075900081030     c                   parm                    kpjba
076000081030     c                   parm                    tibs02ds
076100081030     c                   eval      a_dcmr = t02uni
076200081125      * se causale che genera bolla esco e non genero la seconda bolla
076300081030     c                   if        a_d§cmradd = 'S'
076400081125     c                   eval      wnobolla = *on
076500081030     c                   leavesr
076600081030     c                   endif
076700081030     c                   enddo
076800081030     c                   endif
076900081030
077000010706     C                   endif
077100010706
077200010629     C     ENDCAU        endsr
077300010530      **********************************************************************
077400010530      * RICHIAMA IL PROGRAMMA DELLE NOTE
077500010530      **********************************************************************
077600010530     C     Sr_Note       BEGSR
077700010530
077800010530     C                   clear                   parm01
077900010605     C                   movel     V2CFGS        pfgs
078000010605     C                   movel     WDAO          i06dta
078100010605     C                   z-add     V2CPOE        i06poe
078200010605     C                   z-add     V2CNOR        i06nor
078300010605     C                   z-add     V2CNSR        i06nsr
078400010605     C                   z-add     V2CNRV        i06nrv
078500010530     C                   eval      i06dai = dateu
078600010530     C                   TIME                    W0140
078700010530     C                   movel     w0140         I06ori
078800010530     C                   movel     'V'           i06flm
078900080319      * devo impostare la causale nelle note così viene visualizzata dal
079000080319      * pgm di gestione note
079100080319     c                   clear                   fior06vds
079200080319     c                   if        wcausa <> *blanks
079300080319     c                   eval      v06car = wcausa
079400080319     c                   eval      v06dcar = wdescau
079500080319     c                   endif
079600010530
079700010530     C                   movel     kpjbu         §kpjbu
079800010530     C                   movel     parm01        kpjbu
079900010530     C                   CALL      'FIOR06R'
080000010530     C                   PARM                    kpjba
080100010530     C                   parm                    fior06ds
080200080319     C                   parm                    fior06vds
080300010530     C                   movel     kpjbu         parm01
080400010530     C                   movel     §kpjbu        kpjbu
080500010530
080600010530     C                   endsr
080700010605      *****************************************************************
080800010605      * CONTROLLO NOTE SE OBBLIGATORIE
080900010605      *****************************************************************
081000010605     C     SR_CONTRNOTE  BEGSR
081100010605
081200010605     C                   IF        *IN11 = *ON
081300010706     C                   EVAL      WCAUSA = V2CLAV
081400010605     C                   ENDIF
081500010605     C                   IF        *IN10 = *ON OR *IN12 = *ON
081600010605     C                   EVAL      WCAUSA = V2CCAU1
081700010605     C                   ENDIF
081800010605
081900010605     C                   if        WCAUSA  <> *blanks
082000010605     C                   clear                   dcmr
082100010605     C                   clear                   TIBS02DS
082200010605     C                   movel     'C'           T02mod
082300010605     C                   movel     knsif         t02sif
082400010605     C                   movel     'CMR'         t02cod
082500010605     C                   movel(p)  WCAUSA        T02ke1
082600010605     C                   call      'TIBS02R'
082700010605     C                   parm                    KPJBA
082800010605     C                   parm                    TIBS02DS
082900010605     C                   movel     t02ke1        WCAUSA
083000010605     C                   movel     t02uni        dcmr
083100010706     C   11              EVAL      V2CLAV = WCAUSA
083200010605     C   10
083300010605     COR 12              EVAL      V2CCAU1 = WCAUSA
083400010605
083500010605     C                   if        d§cmrnot <> *blanks
083600010605     C                   do        *hival
083700010605     C                   clear                   fior06ds
083800010605     C                   movel     d§cmrfar      i06far
083900010605     C                   exsr      Sr_Note
084000010605     C                   if        o06f03 = 'S' or o06f12 = 'S'
084100010605     C                   iter
084200010605     C                   endif
084300010605     C                   if        o06dai <> ormdfo
084400010605     C                   iter
084500010605     C                   endif
084600010605     C                   if        o06ori < ormofo
084700010605     C                   iter
084800010605     C                   endif
084900010605     C                   if        o06prg = 0
085000010605     C                   iter
085100010605     C                   endif
085200010605     C                   leave
085300010605     C                   enddo
085400010605     C                   endif
085500010605     C                   endif
085600010605
085700010605     C                   endsr
085800010206      **********************************************************************
085900010206      * CONFERMA
086000010206      **********************************************************************
086100010206     C     Sr_Conferma   BEGSR
086200010206
086300010206     C                   eval      *in44 = *off
086400020524     c                   eval      *in46 = *off
086500020524
086600010530      * Controllo che orm non sia allocato
086700010605     C     kfnorm        chain(e)  fnorm01l
086800010530     C                   if        %error
086900010605     C                   movel     msg(10)       V2CMSG
087000010530     C                   eval      *in44 = *on
087100010530     C                   goto      endsrconf
087200010530     C                   endif
087300010530
087400010605     C                   IF        V2FAO <> ORMFAO
087500010605     C                   MOVEL     MSG(11)       v2cmsg
087600010605     C                   EVAL      *IN28 = *ON
087700010605     C                   goto      endsrconf
087800010605     C                   ENDIF
087900020524
088000010530      * Se orm è in fase di bollettato (900) non devo aggiornare
088100020524     C                   if        ormfao = 900
088200020524     C                   unlock    fnorm01l
088300020524     C                   movel     msg(19)       V2CMSG
088400020524     C                   eval      *in46 = *on
088500020524     C                   goto      endsrconf
088600020524     c                   endif
088700020524
088800020524     c                   eval      ds_ormcra = ormcra
088900020524     c                   eval      ds_ormcor = ormcor
089000020524     c                   eval      ds_ormcrc = ormcrc
089100020529     c                   clear                   okbolla
089200040406     c                   Eval      sav_OrmFao = OrmFao
089300020524
089400020524      * prima di confermare controllo se devo creare la bolla di addebito
089500020529      * se non già fatto in precedenza
089600020529     c                   if        not *in45
089700020524     c                   exsr      sr_contradd
089800020524      * se f12 o f03 da immissione bolla riemetto la videata
089900020524     c                   if        okbolla = '1'
090000020524     C                   goto      endsrconf
090100020524     c                   endif
090200020529      * se msg info di annullo bolla riemetto la videata
090300020529     c                   if        *in45 = *on
090400020529     c                   eval      *in28 = *on
090500020529     C                   goto      emetto
090600020529     c                   endif
090700020529     c                   endif
090800020524
090900010206      * RITIRATI
091000010605     C   12
091100010605     COR 10              EVAL      WCAUSA = V2CCAU1
091200010706     C   11              EVAL      WCAUSA = V2CLAV
091300010605
091400010605     C                   if        WCAUSA = *blanks
091500010206      * Aggiorno ORM e scrivo fase
091600010605      *QUADRATURA
091700010605     C                   IF        *IN10 = *ON
091800010529     C                   time                    w0140
091900010529     C                   move      w0140         wdtgio
092000010529     C                   z-add     wdtgio        G02DAT
092100010529     C                   clear                   G02ERR
092200010529     C                   call      'XSRDA8'
092300010529     C                   parm                    WLBDAT
092400010529     C                   z-add     g02inv        ormdfo
092500010206     C                   movel     w0140         ormofo
092600010206     C                   z-add     600           ormfao
092700010706     C                   Z-ADD     4             ORMNPG
092800010706     C                   MOVE      'M'           ORMTAP
092900010706     C                   MOVE      V2CNDC2       ORMNDC
093000010706     C     V2CNDC2       ifne      *blanks
093100120613     C                   z-add     4             kdstnpg
093200120613     C                   move      v2CNDC2       kdstnfv
093300120613     C                   z-add     v2CFGS        kdstfgs
093400120613     C     kfidst        chain     fidst01l
093500120613     c                   if        not %found(fidst01l)
093600060516     c                   clear                   fnlv55ds
093700060516     c                   eval      d55tpt = '6'
093800060516     c                   move      v2cfgs        d55lin
093900060516     c                   eval      d55drf = dateu
094000060516     c                   call      'FNLV55R'
094100060516     c                   parm                    fnlv55ds
094200060516     c                   if        d55err = *blanks and d55tfa <> d55lin
094300120613     c                   move      d55tfa        kdstfgs
094400120613     c     kfidst        chain     fidst01l
094500060516     c                   endif
094600060516     c                   endif
094700120613     c                   if        %found(fidst01l)
094800120613     C                   z-add     dstdfv        ormddc
094900010706     C                   endif
095000010706     C                   endif
095100040909     C                   eval      ormdtt = g02inv
095200070711      * imposto ormeti
095300070711     c                   exsr      sr_fase
095400070711     c                   eval      ormeti = d§farass
095500010206     C                   update    fnorm000
095600010206
095700040909     c                   Clear                   Fnorf000
095800010206     C                   eval      orfpoe = ormpoe
095900010206     C                   eval      orfnsr = ormnsr
096000010206     C                   eval      orfnor = ormnor
096100010206     C                   eval      orfnrv = ormnrv
096200010406     C                   eval      orfpog = ormpor
096300010206     C                   eval      orfdae = ormdfo
096400010206     C                   eval      orfore = ormofo
096500010206     C                   eval      orffar = ormfao
096600010206     C                   eval      orfcar = *blanks
096700010206     C                   eval      orfpue = KNMUS
096800041013     c                   If        OrmNdc > *Zeros
096900120613     c                   eval      orffgs = kdstfgs
097000040909     c                   Eval      OrfNdc = OrmNdc
097100040909     c                   Eval      OrfDdc = OrmDdc
097200041013     c                   EndIf
097300010206     C                   write     fnorf000
097400010605     C                   ENDIF
097500010605      * RIAPERTURA
097600010605     C                   IF        *IN12 = *ON
097700010605     C                   time                    w0140
097800010605     C                   move      w0140         wdtgio
097900010605     C                   z-add     wdtgio        G02DAT
098000010605     C                   clear                   G02ERR
098100010605     C                   call      'XSRDA8'
098200010605     C                   parm                    WLBDAT
098300010605     C                   z-add     g02inv        ormdfo
098400010605     C                   movel     w0140         ormofo
098500010605     C                   movel     100           ormfao
098600010605     C                   clear                   ormtap
098700010605     C                   clear                   ormnpg
098800010605     C                   clear                   ormndc
098900010605     C                   clear                   ormddc
099000010605     C                   CLEAR                   ORMDST
099100040909     C                   eval      ormdtt = g02inv
099200070711      * imposto ormeti
099300070711     c                   exsr      sr_fase
099400070711     c                   eval      ormeti = d§farass
099500080222      * se ORM ancora assegnabile pulisco la data di stampa
099600080222     c                   if        ormeti <> *blanks and *in07
099700080222     c                   clear                   ormdst
099800080222      * devo pulire anche i dati su fnorg
099900080222     c     kfnorm        chain     fnorg01l
100000080222    3c                   if        %found(fnorg01l)
100100080314     c                   eval      dorg01 = orgflo
100200080222     c                   eval      orgndc = 0
100300080222     c                   eval      orgpdc = 0
100400080222     c                   eval      orgfgs = 0
100500080222     c                   eval      orgddc = 0
100600080222     c                   eval      orgnftl= 0
100700080222     c                   eval      orgslo = 0
100800080222     c                   eval      orgdtvdis= 0
100900080222     c                   eval      orghvdis= 0
101000080222     c                   eval      orgfao = 0
101100080222     c                   eval      orgdfo = 0
101200080222     c                   eval      orgofo = 0
101300080314     c                   clear                   §orgpda
101400080314     c                   eval      orgflo = dorg01
101500080222     c                   update    fnorg000
101600080222    3c                   endif
101700080222     c                   endif
101800010605     C                   update    fnorm000
101900010605
102000040909     c                   Clear                   Fnorf000
102100010605     C                   eval      orfpoe = ormpoe
102200010605     C                   eval      orfnsr = ormnsr
102300010605     C                   eval      orfnor = ormnor
102400010605     C                   eval      orfnrv = ormnrv
102500010605     C                   eval      orfpog = ormpor
102600010605     C                   eval      orfdae = ormdfo
102700010605     C                   eval      orfore = ormofo
102800010605     C                   eval      orffar = ormfao
102900010605     C                   eval      orfcar = *BLANKS
103000010605     C                   eval      orfpue = KNMUS
103100010605     C                   write     fnorf000
103200010605     C                   endif
103300010206      * MANCATO RITIRO
103400010206     C                   else
103500010529
103600040909      * salvo i campi della distinta da riportare sulla fase
103700041015if  1c                   If        v2cndc > *Zeros
103800041015     c                   move      v2cndc        savndc
103900041015     c                   Move      wggg          wa4
104000041015     c                   Movel     wmmm          wa4
104100041015     c                   Move      wa4           savddc
104200041020     c                   Movel     waaa          wa4
104300041020     c                   Movel     wa4           savddc
104400060516     c                   eval      savfgs = v2hfgs
104500041015   x1c                   Else
104600041015      * cerco la fase precedente con la distinta
104700041015     c     kFnorf        Setll     Fnorf01l
104800041015     c                   Do        *Hival
104900041015     c     kFnorf1       Readpe    Fnorf01l
105000041015      * Fine file
105100041015     c                   If        %Eof(Fnorf01l)
105200041015     c                   Leave
105300041015     c                   EndIf
105400041015      * fase con distinta salvo i dati ed esco
105500041015     c                   If        OrfNdc > *Zeros
105600041015     c                   Eval      savndc = OrfNdc
105700041015     c                   Eval      savddc = OrfDdc
105800060516     c                   eval      savfgs = orffgs
105900041015     c                   Leave
106000041015     c                   EndIf
106100041015     c                   EndDo
106200041015e   1c                   EndIf
106300040909
106400010529     C                   time                    w0140
106500010529     C                   move      w0140         wdtgio
106600010529     C                   z-add     wdtgio        G02DAT
106700010529     C                   clear                   G02ERR
106800010529     C                   call      'XSRDA8'
106900010529     C                   parm                    WLBDAT
107000010529     C                   z-add     g02inv        ormdfo
107100010206     C                   movel     w0140         ormofo
107200010301     C                   movel     d§cmrfar      ormfao
107300010206     C                   clear                   ormtap
107400010206     C                   clear                   ormnpg
107500010206     C                   clear                   ormndc
107600010206     C                   clear                   ormddc
107700040909     C                   eval      ormdtt = g02inv
107800070711      * imposto ormeti
107900070711     c                   exsr      sr_fase
108000070711     c                   eval      ormeti = d§farass
108100070926      * se ORM ancora assegnabile pulisco la data di stampa
108200070926     c                   if        ormeti <> *blanks and *in07
108300070926     c                   clear                   ormdst
108400071011      * devo pulire anche i dati su fnorg
108500071011     c     kfnorm        chain     fnorg01l
108600071011    3c                   if        %found(fnorg01l)
108700080314     c                   eval      dorg01 = orgflo
108800071011     c                   eval      orgndc = 0
108900071011     c                   eval      orgpdc = 0
109000071011     c                   eval      orgfgs = 0
109100071011     c                   eval      orgddc = 0
109200071011     c                   eval      orgnftl= 0
109300071011     c                   eval      orgslo = 0
109400071011     c                   eval      orgdtvdis= 0
109500071011     c                   eval      orghvdis= 0
109600071026     c                   eval      orgfao = 0
109700071026     c                   eval      orgdfo = 0
109800071026     c                   eval      orgofo = 0
109900080314     c                   clear                   §orgpda
110000080314     c                   eval      orgflo = dorg01
110100071011     c                   update    fnorg000
110200071011    3c                   endif
110300070926     c                   endif
110400010206     C                   update    fnorm000
110500010206
110600081125     c                   clear                   dorf01
110700040909     c                   Clear                   Fnorf000
110800010206     C                   eval      orfpoe = ormpoe
110900010206     C                   eval      orfnsr = ormnsr
111000010206     C                   eval      orfnor = ormnor
111100010206     C                   eval      orfnrv = ormnrv
111200010406     C                   eval      orfpog = ormpor
111300010206     C                   eval      orfdae = ormdfo
111400010206     C                   eval      orfore = ormofo
111500010206     C                   eval      orffar = ormfao
111600010605     C                   eval      orfcar = WCAUSA
111700010206     C                   eval      orfpue = KNMUS
111800041013     c                   If        savndc > *Zeros
111900060516     c                   eval      orffgs = savfgs
112000040909     c                   Eval      OrfNdc = savndc
112100040909     c                   Eval      OrfDdc = savddc
112200041013     c                   EndIf
112300081125      * imposto se non ho fatto l'ulteriore addebito
112400081125     c                   if        wnobolla = *on
112500081125     c                   eval      §orfadd = 'S'
112600081125     c                   endif
112700081125     c                   eval      orfflo = dorf01
112800010206     C                   write     fnorf000
112900010206
113000010206     C                   endif
113100080423
113200080423      * se ORM commissionato da DPD devo rimettere in trasmissione il file FIDPO
113300080423     c                   if        $comdpd
113400080423     c     kfnorm        chain     fidpo01l
113500080423     c                   if        %found(fidpo01l)
113600080423     c                   clear                   dpodtr
113700080423     c                   update    fidpo000
113800080423     c                   endif
113900080423     c                   endif
114000010206
114100010206     C     endsrconf     endsr
114200020524      **********************************************************************
114300020524      * Controllo se devo generare la bolla di addebito
114400020524      **********************************************************************
114500020524     c     sr_contradd   begsr
114600020524
114700020524     c                   clear                   sav_lpksc
114800020524     c                   eval      nobolla = *off
114900020524     c                   eval      no400 = *off
115000081125
115100081125      * se non devo generare la bolla di addebito vado a fine e imposto il flag
115200081125      * come se avessi fatto la bolla
115300081125     c                   if        wnobolla = *on
115400081125     c                   leavesr
115500081125     c                   endif
115600020524
115700020524      * se cod.ordinante codificato addebito
115800020524 b1  c                   if        ormcor <> *zeros and ormcor2 <> 8888
115900020524     c                             and ormcor2 <> 9999
116000020524      * prima controllo la causale
116100020524     c                   exsr      sr_contrcmr
116200020524     c                   if        nobolla = *on
116300020524     c                   goto      endcontradd
116400020524     c                   endif
116500020524     c                   clear                   dblp
116600090505     c                   movel     ormcor        §lpksca
116700020702      * se c'è imposto il codice bollettazione
116800020702     c     ormcor        chain     fnacr01l
116900020702     c                   if        %found(fnacr01l) and acratb = *blanks
117000020702     c                             and acrksc <> *zeros
117100090505     c                   move      acrksc        §lpksca
117200090505     c                   if        acrccc<>999
117300090505     c                   move      acrccc        §lpctra
117400090505     c                   endif
117500020702     c                   endif
117600020524     c                   eval      §lprsd = ormrso
117700020524     c                   eval      §lpind = ormino
117800020524     c                   eval      §lpcad = ormcao
117900020524     c                   eval      §lplod = ormloo
118000020524     c                   eval      §lpprd = ormpro
118100020524     c                   eval      §lpnzd = ormnao
118200020524     c                   exsr      sr_wrtbolla
118300020524 x1  c                   else
118400020524      * se non inserito l'ordinante
118500020524 b2  c                   if        ormrso = *blanks
118600020524      * orm non commissionato
118700020524 b3  c                   if        ormpoe = ormpor
118800020524      * se cod.mittente codificato addebito
118900020524 b4  c                   if        ormcra <> *zeros and ormcra2 <> 8888
119000020524     c                             and ormcra2 <> 9999
119100020524      * prima controllo la causale
119200020524     c                   exsr      sr_contrcmr
119300020524     c                   if        nobolla = *on
119400020524     c                   goto      endcontradd
119500020524     c                   endif
119600020524     c                   clear                   dblp
119700090505     c                   movel     ormcra        §lpksca
119800020702      * se c'è imposto il codice bollettazione
119900020702     c     ormcra        chain     fnacr01l
120000020702     c                   if        %found(fnacr01l) and acratb = *blanks
120100020702     c                             and acrksc <> *zeros
120200090505     c                   move      acrksc        §lpksca
120300090505     c                   if        acrccc<>999
120400090505     c                   move      acrccc        §lpctra
120500090505     c                   endif
120600020702     c                   endif
120700020524     c                   eval      §lprsd = ormrsr
120800020524     c                   eval      §lpind = orminr
120900020524     c                   eval      §lpcad = ormcar
121000020524     c                   eval      §lplod = ormlor
121100020524     c                   eval      §lpprd = ormprr
121200020524     c                   eval      §lpnzd = ormnar
121300020524     c                   exsr      sr_wrtbolla
121400020524      * se cod.mittente non codificato
121500020524 x4  c                   else
121600020524      * se cod.destinatario codificato addebito
121700020524 b5  c                   if        ormcrc <> *zeros and ormcrc2 <> 8888
121800020524     c                             and ormcrc2 <> 9999
121900020524      * prima controllo la causale
122000020524     c                   exsr      sr_contrcmr
122100020524     c                   if        nobolla = *on
122200020524     c                   goto      endcontradd
122300020524     c                   endif
122400020524     c                   clear                   dblp
122500090505     c                   movel     ormcrc        §lpksca
122600020702      * se c'è imposto il codice bollettazione
122700020702     c     ormcrc        chain     fnacr01l
122800020702     c                   if        %found(fnacr01l) and acratb = *blanks
122900020702     c                             and acrksc <> *zeros
123000090505     c                   move      acrksc        §lpksca
123100090505     c                   if        acrccc<>999
123200090505     c                   move      acrccc        §lpctra
123300090505     c                   endif
123400020702     c                   endif
123500020524     c                   eval      §lprsd = ormrsc
123600020524     c                   eval      §lpind = orminc
123700020524     c                   eval      §lpcad = ormcac
123800020524     c                   eval      §lplod = ormloc
123900020524     c                   eval      §lpprd = ormprc
124000020524     c                   eval      §lpnzd = ormnac
124100020524     c                   exsr      sr_wrtbolla
124200020524 e5  c                   endif
124300020524 e4  c                   endif
124400020524      * orm commissionato
124500020524 x3  c                   else
124600020524      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
124700020524 b4  c                   if        ormcrc <> *zeros and ormcrc2 <> 8888
124800020524     c                             and ormcrc2 <> 9999  and ormcrc1 = ormpoe
124900020524      * prima controllo la causale
125000020524     c                   exsr      sr_contrcmr
125100020524     c                   if        nobolla = *on
125200020524     c                   goto      endcontradd
125300020524     c                   endif
125400020524     c                   clear                   dblp
125500090505     c                   movel     ormcrc        §lpksca
125600020702      * se c'è imposto il codice bollettazione
125700020702     c     ormcrc        chain     fnacr01l
125800020702     c                   if        %found(fnacr01l) and acratb = *blanks
125900020702     c                             and acrksc <> *zeros
126000090505     c                   move      acrksc        §lpksca
126100090505     c                   if        acrccc<>999
126200090505     c                   move      acrccc        §lpctra
126300090505     c                   endif
126400020702     c                   endif
126500020524     c                   eval      §lprsd = ormrsc
126600020524     c                   eval      §lpind = orminc
126700020524     c                   eval      §lpcad = ormcac
126800020524     c                   eval      §lplod = ormloc
126900020524     c                   eval      §lpprd = ormprc
127000020524     c                   eval      §lpnzd = ormnac
127100020524     c                   exsr      sr_wrtbolla
127200020524      * se non codificato o p.o. differenti
127300020524 x4  c                   else
127400020524      * se cod.mittente codificato addebito
127500020524 b5  c                   if        ormcra <> *zeros and ormcra2 <> 8888
127600020524     c                             and ormcra2 <> 9999
127700020524      * prima controllo la causale
127800020524     c                   exsr      sr_contrcmr
127900020524     c                   if        nobolla = *on
128000020524     c                   goto      endcontradd
128100020524     c                   endif
128200020524     c                   clear                   dblp
128300090505     c                   movel     ormcra        §lpksca
128400020702      * se c'è imposto il codice bollettazione
128500020702     c     ormcra        chain     fnacr01l
128600020702     c                   if        %found(fnacr01l) and acratb = *blanks
128700020702     c                             and acrksc <> *zeros
128800090505     c                   move      acrksc        §lpksca
128900090505     c                   if        acrccc<>999
129000090505     c                   move      acrccc        §lpctra
129100090505     c                   endif
129200020702     c                   endif
129300020524     c                   eval      §lprsd = ormrsr
129400020524     c                   eval      §lpind = orminr
129500020524     c                   eval      §lpcad = ormcar
129600020524     c                   eval      §lplod = ormlor
129700020524     c                   eval      §lpprd = ormprr
129800020524     c                   eval      §lpnzd = ormnar
129900020524     c                   exsr      sr_wrtbolla
130000020524 e5  c                   endif
130100020524 e4  c                   endif
130200020524 e3  c                   endif
130300020524 e2  c                   endif
130400020524 e1  c                   endif
130500020524
130600020524     c     endcontradd   endsr
130700020524      **********************************************************************
130800020524      * controllo la causale
130900020524      **********************************************************************
131000020524     c     sr_contrcmr   begsr
131100020524
131200020524      * da causale con addebito a causale senza addebito o no causale
131300020524      * non devo creare la bolla
131400020524      * ma devo emettere msg info x annullamento bolla addebito già creata in precedenza
131500020524     c                   if        (*in10 or *in12) and sav_cmradd = 'S' and
131600020524     c                             new_cmradd = ' '
131700020524     c                   movel     msg(20)       v2cmsg
131800020524     c                   eval      *in45 = *on
131900020524     c                   eval      nobolla = *on
132000020524     c                   endif
132100020524      * da causale con addebito a causale con addebito non devo creare la bolla
132200020524     c                   if        (*in10 or *in12) and sav_cmradd = 'S' and
132300020524     c                             new_cmradd = 'S'
132400020524     c                   eval      nobolla = *on
132500020524     c                   endif
132600020524      * da causale senza addebito o no causale a causale senza addebito non devo creare la bolla
132700020524     c                   if        ((*in10 and sav_cmradd = ' ') or *in11)
132800020524     c                             and new_cmradd = ' '
132900020524     c                   eval      nobolla = *on
133000020524     c                   endif
133100020524      * da causale senza addebito a causale con addebito
133200030901      * e l'orm è in fase 999 o 910 e prima non è stata fatta una fase 400
133300020524      * non devo fare la bolla
133400020524     c                   if        *in12 and sav_cmradd = ' ' and
133500020524     c                             new_cmradd = 'S'
133600020524     c                   exsr      sr_cerca400
133700020524     c                   if        no400 = *on
133800020524     c                   eval      nobolla = *on
133900020524     c                   endif
134000020524     c                   endif
134100080226      * se riapertura ORM non devo creare la bolla di addebito
134200080226     c                   if        *in12
134300080226     c                   eval      nobolla = *on
134400080226     c                   endif
134500020524
134600020524     c                   endsr
134700020524      **********************************************************************
134800020524      * cerco se è stata fatta una fase 400 in precedenza
134900020524      **********************************************************************
135000020524     c     sr_cerca400   begsr
135100020524
135200020524     c     kfnorf        setll     fnorf01l
135300020524     c     kfnorf1       readpe    fnorf01l
135400020524     c                   dow       not %eof(fnorf01l)
135500080226      * la fase precedente è maggiore di 420 non devo fare la bolla
135600080226     c                   if        orffar > 420
135700020524     c                   eval      no400 = *on
135800020524     c                   leave
135900020524     c                   endif
136000080226      * la fase precedente è la 400 o 410 o 420 devo fare la bolla
136100080226     c                   if        orffar = 400 or orffar = 410 or
136200080226     c                             orffar = 420
136300020524     c                   leave
136400020524     c                   endif
136500020524      * la fase precedente è minore 400 e non è la 300 non devo fare la bolla
136600020524     c                   if        orffar < 400 and orffar <> 300
136700020524     c                   eval      no400 = *on
136800020524     c                   leave
136900020524     c                   endif
137000020524     c     kfnorf1       readpe    fnorf01l
137100020524     c                   enddo
137200020524
137300020524     c                   endsr
137400020524      **********************************************************************
137500020524      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
137600020524      **********************************************************************
137700020524     c     sr_wrtbolla   begsr
137800020524
137900020524      * richiamo il programma per la scelta delle stampanti
138000020524     c                   exsr      sr_scestp
138100020524     c   92              eval      okbolla = '1'
138200020524     c   92              goto      endwrtbolla
138300020524
138400020524     c                   clear                   kpjbu
138500020524     c                   clear                   dtasv
138600020524
138700020524     c                   eval      §lpfpr = 'T'
138800020524     c                   movel     ormpor        §lpccn
138900020524     c                   move      9999          §lpccn
139000020524     c                   movel     dateu         §lpaas
139100020524     c                   move      dateu         §lpmgs
139200020524     c                   eval      §lplnp = ormpor
139300020524     c                   eval      §lpcbo = 'FY'
139400020524     c                   eval      §lpsop = '+'
139500020530     c                   eval      §lplna = ormpoe
139600020524     c                   eval      §lprsm = ormrsr
139700020524     c                   eval      §lpinm = orminr
139800020524     c                   eval      §lpcam = ormcar
139900020524     c                   eval      §lplom = ormlor
140000020524     c                   eval      §lpprm = ormprr
140100020524     c                   eval      §lpnzm = ormnar
140200020524
140300020524 b1  c                   if        ormncl <> 0
140400020524     c                   eval      §lpncl = ormncl
140500020524 x1  c                   else
140600020524     c                   eval      §lpncl = 1
140700020524 e1  c                   endif
140800020524 b1  c                   if        ormpkg <> 0
140900020524     c                   eval      §lppkb = ormpkg
141000020524 x1  c                   else
141100020524 b2  c                   if        ormvlm <> 0
141200020524     c     ormvlm        mult      200           §lppkb
141300020524 x2  c                   else
141400020524 b3  c                   if        ormbnc <> 0
141500020524     c     ormbnc        mult      200           §lppkb
141600020524 e3  c                   endif
141700020524 e2  c                   endif
141800020524 e1  c                   endif
141900020524
142000020524     c                   clear                   ds_keyorm
142100020524     c                   eval      ds_poe = ORMpoe
142200020524     c                   eval      ds_nsr = ORMnsr
142300020524     c                   eval      ds_nor = ORMnor
142400020524     c                   eval      ds_nrv = ORMnrv
142500041201     c                   move      ds_keyorm     §lprmn
142600020524
142700020524     c                   eval      §lprma = ormrfa
142800020524     c                   eval      §lprmo = wcausa + '-' + d§cmrdes1
142900020524     c                   eval      §lpctm = '4F'
143000020524     c                   movel     dateu         §lpdrt
143100020524
143200020524      * Cerca codice autotrasportatore e flag mattino/pomeriggio
143300020524 b1  c                   if        ormndc <> 0
143400120613     c                   z-add     ormnpg        kdstnpg
143500120613     c                   z-add     ormndc        kdstnfv
143600120613     c                   z-add     ormpor        kdstfgs
143700120613     c     kfidst        chain     fidst01l
143800120613 b2  c                   if        not %found(fidst01l)
143900060516     c                   clear                   fnlv55ds
144000060516     c                   eval      d55tpt = '6'
144100060516     c                   move      ormpor        d55lin
144200060516     c                   eval      d55drf = dateu
144300060516     c                   call      'FNLV55R'
144400060516     c                   parm                    fnlv55ds
144500060516     c                   if        d55err = *blanks and d55tfa <> d55lin
144600120613     c                   move      d55tfa        kdstfgs
144700120613     c     kfidst        chain     fidst01l
144800060516     c                   endif
144900060516     c                   endif
145000120613 b2  c                   if        %found(fidst01l)
145100020524     c                   eval      §lppdr = DSTpdr
145200120613     c                   eval      §lpfpp = dstfpp
145300020524 x2  c                   else
145400020524     c                   movel     §lplnp        §lppdr
145500020524     c                   move      0999          §lppdr
145600020524     c                   eval      §lpfpp = 'M'
145700020524 e2  c                   endif
145800020524 x1  c                   else
145900020524     c                   movel     §lplnp        §lppdr
146000020524     c                   move      0999          §lppdr
146100020524     c                   eval      §lpfpp = 'M'
146200020524 e1  c                   endif
146300020524
146400020524     C                   call      'FNLS01R'
146500020524     C                   parm                    kpjba
146600020524     C                   parm                    dblp
146700020524     C                   parm                    dtasv
146800030623     c                   Parm                    Trul90ds
146900020524
147000020524     C                   if        §lpfpr = '4' or §lpfpr = '3'
147100020524     C                             or §lpfpr = '5'
147200020524     C                   eval      okbolla = '1'
147300020524     C                   endif
147400020524
147500020524     c     endwrtbolla   endsr
147600020524      **********************************************************************
147700020524      * scelta stampante per immissione bolla
147800020524      **********************************************************************
147900020524     c     sr_scestp     begsr
148000020524
148100020524     C                   clear                   kpjbu
148200020524     c                   setoff                                       92
148300020524
148400020524      * Richiesta stampanti
148500020524     C                   if        contas = *zeros
148600030623     C                   clear                   trul90ds
148700030623     C                   movel     'S'           D90rse
148800030623     C                   movel     'S'           D90rsb
148900030623     C                   call      'TRUL90R'
149000020524     C                   parm                    kpjba
149100030623     C                   parm                    trul90ds
149200020524      * F3=Fine
149300030623     C                   if        D90f3 = '1'
149400020524     C                   eval      *in92 = *on
149500020524     C                   goto      endstp
149600020524     C                   endif
149700030307
149800030307     C                   z-add     103           lung
149900030304
150000020524      * Ovrprtf Segnacolli
150100030623     C                   movea     D90pre        cm3(28)
150200030623     C                   movea     D90mde        cm3(50)
150300020524     C                   clear                   comman
150400020524     C                   movea     cm3           comman
150500020524     C                   CALL      'QCMDEXC'                            92
150600020524     C                   PARM                    comman
150700020524     C                   PARM                    lung
150800020524     C   92              goto      endstp
150900030623
151000030623      * Bolle - Nuovo modulo
151100030623
151200030623     c                   Z-Add     110           lung
151300030623
151400030623      * Ovrprtf Bolle A4
151500030623     c                   Movea     D90prb4       cma4(30)
151600030623     c                   Movea     D90mdb4       cma4(52)
151700030623     c                   Clear                   Comman
151800030623     c                   Movea     cmA4          Comman
151900030623     c                   Call      'QCMDEXC'                            92
152000030623     c                   Parm                    Comman
152100030623     c                   Parm                    Lung
152200030623     c   92              goto      endstp
152300030623
152400030623      * Ovrprtf Bolle A5
152500030623     c                   Movea     D90prb5       cmA5(30)
152600030623     c                   Movea     D90mdb5       cmA5(52)
152700030623     c                   Clear                   Comman
152800030623     c                   Movea     cmA5          Comman
152900030623     c                   Call      'QCMDEXC'                            92
153000030623     c                   Parm                    Comman
153100030623     c                   Parm                    Lung
153200030623     c   92              goto      endstp
153300020524
153400020524     C                   eval      contas = contas +1
153500020524     C                   endif
153600020524
153700020524     c     endstp        endsr
153800010605      *****************************************************************
153900010605      * CONTROLLI SECONDA VIDEATA
154000010605      *****************************************************************
154100010605     C     SR_CONTR2     BEGSR
154200010605
154300010625     C                   setoff                                       474828
154400020524     c                   clear                   new_cmradd
154500081125     c                   eval      wnobolla = *off
154600020524
154700010605     C                   IF        *IN10 = *ON
154800010629     C                   MOVE      *BLANKS       V2DCAU1
154900010709     C                   MOVE      *BLANKS       V2DNDC2
155000010709     C                   IF        V2CLAV = *ZEROS
155100010709     C                   CLEAR                   V2CLAV
155200010709     C                   ENDIF
155300010709     C                   IF        V2CNDC2 = *ZEROS
155400010709     C                   CLEAR                   V2CNDC2
155500010709     C                   ENDIF
155600010706     C                   IF        V2CCAU1 <> *BLANKS AND V2CLAV <> *BLANKS
155700010625     C                   MOVEL     MSG(6)        V2CMSG                         solo 1 scelta
155800010605     C                   SETON                                        28
155900010706     C                   GOTO      ENDCONTR2
156000010605     C                   ENDIF
156100010706     C                   IF        V2CCAU1 <> *BLANKS AND V2cndc2 <> *BLANKS and
156200010706     C                             v2cndc2 <> *zeros
156300010706     C                   MOVEL     MSG(6)        V2CMSG                         solo 1 scelta
156400010706     C                   SETON                                        28
156500010706     C                   GOTO      ENDCONTR2
156600010706     C                   ENDIF
156700010706     C                   IF        V2CCAU1 = *BLANKS AND V2CLAV = *BLANKS
156800010629     C                   MOVEL     MSG(15)       V2CMSG
156900010629     C                   SETON                                        28
157000010629     C                   GOTO      ENDCONTR2
157100010629     C                   ENDIF
157200010706     C                   IF        V2CLAV = 'SI'
157300010625     C                   EXSR      SR_NUMDIS
157400010625     C                   IF        *IN28 = *ON OR *IN90 = *ON
157500010625     C                   SETON                                        48
157600010625     C                   GOTO      ENDCONTR2
157700010625     C                   ENDIF
157800010706     C                   ELSE
157900010706     C                   if        v2clav <> *blanks
158000010706     C                   MOVEL     MSG(17)       V2CMSG                         solo 1 scelta
158100010706     C                   SETON                                        2847
158200010706     C                   GOTO      ENDCONTR2
158300010706     C                   ELSE
158400010706     C                   CLEAR                   V2DNDC2
158500010706     C                   endif
158600010605     C                   ENDIF
158700010625     C                   ENDIF
158800010605
158900010605     C                   IF        *IN12 = *ON
159000010709     C                   IF        V2CLAV = *ZEROS
159100010709     C                   CLEAR                   V2CLAV
159200010709     C                   ENDIF
159300010706     C                   IF        V2CCAU1 <> *BLANKS AND V2Clav <> *BLANKS
159400010625     C                   MOVEL     MSG(6)        V2CMSG                         solo 1 scelta
159500010605     C                   SETON                                        28
159600010605     C                   GOTO      ENDCONTR2
159700010605     C                   ENDIF
159800010706     C                   IF        V2CCAU1 = *BLANKS AND V2Clav = *BLANKS
159900010629     C                   MOVEL     MSG(15)       V2CMSG
160000010629     C                   SETON                                        28
160100010629     C                   GOTO      ENDCONTR2
160200010629     C                   ENDIF
160300010706     C                   if        v2clav <> *blanks and v2clav <> 'SI'
160400010706     C                   seton                                        2847
160500010706     C                   movel     msg(17)       v2cmsg
160600010706     C                   goto      endcontr2
160700010706     C                   endif
160800010605     C                   ENDIF
160900010605      * QUADRATURA CON CAUSALE
161000010605      * FASE 600
161100010605     C                   IF        *IN11 = *ON
161200010629     C                   MOVE      *BLANKS       V2DCAU2
161300010709     C                   IF        V2CLAV = *BLANKS
161400010629     C                   MOVEL     MSG(16)       V2CMSG
161500010629     C                   SETON                                        28
161600010629     C                   GOTO      ENDCONTR2
161700010629     C                   ENDIF
161800010605      * Se immessa una causale questa deve esistere in tabella CMR
161900010706     C                   IF        V2CLAV <> *BLANKS
162000010706     C                   EVAL      WCAUSA = V2CLAV
162100010605     C                   EXSR      SR_CONTRCAU
162200010605     C                   IF        *IN28 = *ON
162300010605     C                   GOTO      ENDCONTR2
162400010605     C                   ENDIF
162500010605      * Se orm ha già una bolla non posso dichiararlo non fatto
162600010605      * se non è un prepagato
162700010605     C                   if        V2CTOR <> 'P'
162800010605     C                   exsr      Sr_Bolla
162900010605     C                   if        *in28 = *on
163000010605     C                   EVAL      *IN47 = *ON
163100010605     C                   goto      ENDCONTR2
163200010605     C                   endif
163300010605     C                   endif
163400010605     C                   endif
163500010605     C                   endif
163600030901      * FASE 500 O 999 o 910
163700010605     C                   IF        *IN10 = *ON OR *IN12 = *ON
163800010605      * CAMBIO CAUSALE
163900010605     C                   IF        V2CCAU1 <> *BLANKS
164000010605      * Se immessa una causale questa deve esistere in tabella CMR
164100010605     C                   EVAL      WCAUSA = V2CCAU1
164200010605     C                   EXSR      SR_CONTRCAU
164300010605     C                   IF        *IN28 = *ON
164400010605     C                   GOTO      ENDCONTR2
164500010605     C                   ENDIF
164600010605      * Se orm ha già una bolla non posso dichiararlo non fatto
164700010605      * se non è un prepagato
164800010605     C                   if        V2CTOR <> 'P'
164900010605     C                   exsr      Sr_Bolla
165000010605     C                   if        *in28 = *on
165100010605     C                   EVAL      *IN47 = *ON
165200010605     C                   goto      ENDCONTR2
165300010605     C                   endif
165400010605     C                   endif
165500010605     C                   endif
165600010605     C                   endif
165700010605
165800010605     C     ENDCONTR2     endsr
165900010625      **********************************************************************
166000010625      * NUMERO DISTINTA
166100010625      **********************************************************************
166200010625     C     SR_NUMDIS     begsr
166300010625      *    Ricerco Distinta
166400010625     c     '?'           scan      V2CNDC2                                39
166500010625     c   39              do
166600010625     C                   clear                   V2CNDC2
166700010625     C                   clear                   V2DNDC2
166800010625     c                   clear                   dslr01
166900010625     c                   movel     'S'           dlrric
167000010625     c                   movel     '4'           dlrnpg
167100010625     c                   movel     '2'           dlrtfv
167200010625     C                   movel     'FIOR27R'     dlrpgm
167300010625     c                   movel     V2CFGS        dlrfgs
167400010625     c                   movel     dslr01        kpjbu
167500010625     c                   call      'FNLR02R'
167600010625     c                   parm                    kpjba
167700010625     c                   movel     kpjbu         dslr01
167800010625     C     dlrnfv        ifeq      *zeros
167900010625     C                   clear                   V2CNDC2
168000010706     C                   seton                                        2848
168100010706     C                   movel     msg(14)       v2cmsg
168200010706     c                   goto      endnumdis
168300010625     C                   else
168400120613     c                   move      dlrnfv        V2CNDC2
168500010625     C                   endif
168600010625     C     V2CNDC2       ifne      *blanks
168700120613     C                   z-add     4             kdstnpg
168800120613     C                   move      V2CNDC2       kdstnfv
168900120613     C                   movel     V2CFGS        kdstfgs
169000120613     C     kfidst        chain     fidst01l
169100120613     C                   if        not %found(fidst01l)
169200060516     c                   clear                   fnlv55ds
169300060516     c                   eval      d55tpt = '6'
169400060516     c                   move      v2cfgs        d55lin
169500060516     c                   eval      d55drf = dateu
169600060516     c                   call      'FNLV55R'
169700060516     c                   parm                    fnlv55ds
169800060516     c                   if        d55err = *blanks and d55tfa <> d55lin
169900120613     c                   move      d55tfa        kdstfgs
170000120613     c     kfidst        chain     fidst01l
170100060516     c                   endif
170200060516     c                   endif
170300120613     C                   if        %found(fidst01l)
170400010625     C                   EXSR      SR_DESCRIZ1
170500010706     C                   ELSE
170600010706     C                   eval      *in28 = *on
170700010706     C                   eval      *in48 = *on
170800010706     C                   movel     msg(12)       V2CMSG
170900010706     C                   goto      endnumdis
171000010625     C                   endif
171100010625     C                   endif
171200010625     c                   seton                                        9048
171300010625     C                   add       1             conta             1 0
171400010625     c                   enddo
171500010625      *    Controllo numero Distinta
171600010706     C     V2CNDC2       ifeq      *all'0'
171700010625     C                   clear                   V2CNDC2
171800010706     C                   clear                   V2DNDC2
171900010625     C                   endif
172000010706     C     V2CNDC2       ifne      *blanks
172100041015      * se c'è la distinta a video quella immessa dall'utente deve essere uguale
172200041015     c                   If        v2cndc > *Zeros
172300120613     c                   move      v2cndc2       w0060             6 0
172400120613     c                   If        w0060 <> v2cndc
172500041015     C                   eval      *in28 = *on
172600041015     C                   eval      *in19 = *on
172700041015     C                   movel     msg(21)       V2CMSG
172800041015     C                   goto      endnumdis
172900041015     c                   EndIf
173000041015     c                   EndIf
173100120613     C                   move      '4'           kdstnpg
173200120613     C                   move      V2CNDC2       kdstnfv
173300120613     C                   move      V2CFGS        kdstfgs
173400120613     C     kfidst        chain     fidst01l
173500120613     C                   if        not %found(fidst01l)
173600060516     c                   clear                   fnlv55ds
173700060516     c                   eval      d55tpt = '6'
173800060516     c                   move      v2cfgs        d55lin
173900060516     c                   eval      d55drf = dateu
174000060516     c                   call      'FNLV55R'
174100060516     c                   parm                    fnlv55ds
174200060516     c                   if        d55err = *blanks and d55tfa <> d55lin
174300120613     c                   move      d55tfa        kdstfgs
174400120613     c     kfidst        chain     fidst01l
174500060516     c                   endif
174600060516     c                   endif
174700120613     C                   if        %found(fidst01l)
174800010706     C                   EXSR      SR_DESCRIZ1
174900120613     C     dstatb        ifne      *blanks
175000010625     C                   eval      *in28 = *on
175100010625     C                   eval      *in19 = *on
175200010625     C                   movel     msg(12)       V2CMSG
175300010625     C                   goto      endnumdis
175400010625     C                   else
175500010625      * Chiusa
175600120613     C     dstfcf        ifne      ' '
175700010625     C                   eval      *in28 = *on
175800010625     C                   eval      *in48 = *on
175900010625     C                   movel     msg(13)       v2cmsg
176000010625     C                   goto      endnumdis
176100010625     C                   endif
176200010706     C                   endif
176300010706     C                   else
176400010706     C                   eval      *in28 = *on
176500010706     C                   eval      *in48 = *on
176600010706     C                   movel     msg(12)       V2CMSG
176700010706     C                   goto      endnumdis
176800010706     C                   endif
176900010706     C                   else
177000010706     C                   if        v2cndc2 = *blanks
177100010706     C                   eval      *in28 = *on
177200010706     C                   eval      *in48 = *on
177300010706     C                   movel     msg(14)       v2cmsg
177400010706     C                   goto      endnumdis
177500010706     C                   endif
177600010706     C                   endif
177700010625
177800010625     C                   if        conta > 0
177900010625     C                   eval      *in90 = *off
178000010625     C                   endif
178100010625     C                   if        V2CNDC2 > *blanks and conta = *zeros
178200010625     C                   add       1             conta
178300010625     C                   eval      *in90 = *on
178400010625     C                   endif
178500010625
178600010625     C     endnumdis     endsr
178700010605      **********************************************************************
178800010605      * CONTROLLO  * Orm / bolla *
178900010605      **********************************************************************
179000010605     C     Sr_Bolla      BEGSR
179100010605
179200010605     C                   clear                   WORM             14
179300010605     C                   move      V2CPOE        WPOE              3
179400010605     C                   move      V2CNSR        WNSR              2
179500010605     C                   move      V2CNOR        WNOR              7
179600010605     C                   move      V2CNRV        WNRV              2
179700010605     C                   eval      worm = wpoe + wnsr
179800010605     C                             + wnor + wnrv
179900060214     C                   movel     worm          kar4n14
180000060214     C     kfiar4        chain     fiar404l
180100060214     C                   if        %found(fiar404l)
180200010605     C                   movel     msg(9)        V2CMSG
180300010605     C                   eval      *in28 = *on
180400010605     C                   goto      endsrbolla
180500010605     C                   endif
180600010605
180700010605     C     endsrbolla    endsr
180800010604      **********************************************************************
180900010604      * VISUALIZZAZIONE ORM
181000010604      **********************************************************************
181100010604     C     Sr_ORM        BEGSR
181200010604
181300010629     C                   movel     v2cpoe        ppoe
181400010629     C                   movel     v2CNOR        pnor
181500010629     C                   movel     v2CNSR        pnsr
181600010629     C                   movel     v2CNRV        pnrv
181700010629     C                   movel     '5'           psce
181800010629     C                   movel     v2cfgs        pfgs
181900010629
182000010629     C                   movel     kpjbu         §kpjbu
182100010629     C                   movel     parm01        kpjbu
182200010629     C                   call      'FIOR07R'
182300010629     C                   parm                    kpjba
182400010629     C                   movel     kpjbu         parm01
182500010629     C                   movel     §kpjbu        kpjbu
182600010629
182700010629     C                   ENDSR
182800040406      *-------------------------------------------------------------------------------*
182900040406      * Controllo se devo chiudere l'ORM in automatico in base i tentativi fatti
183000040406      *-------------------------------------------------------------------------------*
183100040406     c     Sr_ContrFase  BegSr
183200040406
183300040406     c                   Eval      wchiudi = *Off
183400040406     c                   Clear                   wconta
183500040406      * Mi posiziono sull'ultima fase dell'ORM
183600040406     c     kFnorf        Setll     Fnorf01l
183700040406     c                   Do        *Hival
183800040406      * Torno indietro a leggere
183900040406     c     kFnorf1       Readpe    Fnorf01l
184000040406      * Fine file
184100040406     c                   If        %Eof(Fnorf01l)
184200040406     c                   Leave
184300040406     c                   EndIf
184400040406      * se stessa fase torno a leggere
184500040406     c                   If        OrfFar = Sav_OrfFar or OrfFar = sav_OrmFao
184600040406     c                   Iter
184700040406     c                   EndIf
184800040406     c                   Eval      sav_OrfFar = OrfFar
184900040406      * fase senza causale
185000040406     c                   If        OrfCar = *Blanks
185100040406     c                   Iter
185200040406     c                   EndIf
185300040406      * controllo la causale della fase
185400040406     c                   Clear                   dCmr
185500040406     c                   Clear                   Tibs02ds
185600040406     c                   Eval      T02Mod = 'C'
185700040406     c                   Eval      T02Sif = knsif
185800040406     c                   Eval      T02Cod = 'CMR'
185900040406     c                   Eval      T02Ke1 = OrfCar
186000040406     c                   Call      'TIBS02R'
186100040406     c                   Parm                    kpjba
186200040406     c                   Parm                    Tibs02ds
186300040406     c                   Eval      dCmr = T02Uni
186400040413      * se non è da conteggiare torno a leggere
186500040413     c                   If        d§cmrccc = *Blanks
186600040406     c                   Iter
186700040406     c                   EndIf
186800040406
186900040406      * Conto
187000040406     c                   add       1             wconta
187100040406      * se raggiunto il max dei tentativi chiudo l'ORM
187200040406     c                   If        wconta >= wtenta
187300040406     c                   Eval      wchiudi = *On
187400040406     c                   Leave
187500040406     c                   EndIf
187600040406     c                   EndDo
187700040406
187800040406     c                   EndSr
187900040406      *-------------------------------------------------------------------------------*
188000040406      * Chiamo la manutenzione ORM x proporre la chiusura
188100040406      *-------------------------------------------------------------------------------*
188200040406     c     Sr_Chiudi     BegSr
188300040406
188400040406     c                   Clear                   Fior05ds
188500040406     c                   Eval      §rmtla = 'C'
188600040406     c                   Eval      §rmfpr = 'P'
188700040406     c                   Eval      §rmpoe = OrmPoe
188800040406     c                   Eval      §rmnsr = OrmNsr
188900040406     c                   Eval      §rmnor = OrmNor
189000040406     c                   Eval      §rmnrv = OrmNrv
189100040406     c                   Eval      §rmtor = OrmTor
189200040406     c                   Eval      §rmcor = OrmCor
189300040406     c                   Eval      §rmrso = OrmRso
189400040406     c                   Eval      §rmino = OrmIno
189500040406     c                   Eval      §rmcao = OrmCao
189600040406     c                   Eval      §rmloo = OrmLoo
189700040406     c                   Eval      §rmpro = OrmPro
189800040406     c                   Eval      §rmnao = OrmNao
189900040406     c                   Eval      §rmcra = OrmCra
190000040406     c                   Eval      §rmrsr = OrmRsr
190100040406     c                   Eval      §rminr = OrmInr
190200040406     c                   Eval      §rmcar = OrmCar
190300040406     c                   Eval      §rmlor = OrmLor
190400040406     c                   Eval      §rmprr = OrmPrr
190500040406     c                   Eval      §rmnar = OrmNar
190600040406     c                   Eval      §rmrer = OrmRer
190700040406     c                   Eval      §rmter = OrmTer
190800040406     c                   Eval      §rmcrc = OrmCrc
190900040406     c                   Eval      §rmrsc = OrmRsc
191000040406     c                   Eval      §rminc = OrmInc
191100040406     c                   Eval      §rmcac = OrmCac
191200040406     c                   Eval      §rmloc = OrmLoc
191300040406     c                   Eval      §rmprc = OrmPrc
191400040406     c                   Eval      §rmnac = OrmNac
191500040406     c                   Eval      §rmffd = OrmFfd
191600040406     c                   Eval      §rmdar = OrmDar
191700040406     c                   Eval      §rmorr = OrmOrr
191800040406     c                   Eval      §rmnam = OrmNam
191900040406     c                   Eval      §rmncl = OrmNcl
192000040406     c                   Eval      §rmpkg = OrmPkg
192100040406     c                   Eval      §rmvlm = OrmVlm
192200040406     c                   Eval      §rmbnc = OrmBnc
192300040406     c                   Eval      §rmblc = OrmBlc
192400040406     c                   Eval      §rmatt = OrmAtt
192500040406     c                   Eval      §rmmtc = OrmMtc
192600040406     c                   Eval      §rmpag = OrmPag
192700040406     c                   Eval      §rmksc = OrmKsc
192800040406     c                   Eval      §rmctr = OrmCtr
192900040406     c                   Eval      §rmpor = OrmPor
193000040406     c                   Eval      §rmzor = OrmZor
193100040406     c                   Eval      §rmpoc = OrmPoc
193200040406     c                   Eval      §rmno1 = OrmNo1
193300040406     c                   Eval      §rmno2 = OrmNo2
193400040406     c                   Eval      §rmcst = OrmCst
193500040406     c                   Eval      §rmvcs = OrmVcs
193600040406     c                   Eval      §rmfcs = OrmFcs
193700040406     c                   Eval      §rmddt = OrmDdt
193800040406     c                   Eval      §rmsto = OrmSto
193900040406     c                   Eval      §rmrfa = OrmRfa
194000040406     c                   Eval      §rmspi = OrmSpi
194100040406     c                   Eval      §rmcau = Sav_CmrCau
194200040406     c                   Eval      §rmflo = OrmFlo
194300040406     c                   Eval      §rmflv = 'B'
194400040406      * Passo alcuni parametri anche nella parm01
194500040406     c                   Clear                   parm01
194600040406     c                   eval      pmio = 'S'
194700041015     c                   Eval      pndc = savndc
194800041015     c                   Eval      pddc = savddc
194900120613     c                   eval      pdstfgs = savfgs
195000040406     c                   Eval      kpjbu = parm01
195100040406
195200040406     c                   Call      'FIOR05R'
195300040406     c                   Parm                    kpjba
195400040406     c                   Parm                    Fior05ds
195500040406
195600040406     c                   EndSr
195700010206      *****************************************************************
195800010206      * ROUTINE INIZIALE
195900010206      *****************************************************************
196000010206     C     *INZSR        BEGSR
196100010206
196200010206     C     *ENTRY        PLIST
196300010206     C                   PARM                    KPJBA
196400010206
196500010206     C                   Z-ADD     1             CODUT
196600010206     C                   CALL      'X§PARUT'
196700010206     C                   PARM                    UT§DSE0F
196800010206     C                   MOVEL     RAGUT         RSUT
196900010206     C                   MOVEL     REC80         CNCR80
197000010206
197100010206      * IMPOSTO IL P.O. DI GESTIONE
197200020502     C                   if        simtpp = '2' or simtpp = *blanks
197300020502     C                   movel     simpou        V1Cfgs
197400010206     C                   eval      *in06 = *on
197500010206     C                   else
197600010530     C                   movel     SIMFEL        V1Cfgs
197700010206     C                   endif
197800010206      * Carico schiera P.O. gestiti
197900010206     C                   CLEAR                   TRUL06ds
198000010206     C                   MOVE      '£1'          D06COD
198100010206     C                   MOVEL     SIMFEL        D06KEY
198200010206     C                   MOVEL     TRUL06DS      KPJBU
198300010206     C                   CALL      'TRUL06R'
198400010206     C                   PARM                    KPJBA
198500010206     C                   MOVEL     KPJBU         TRUL06DS
198600010206     C                   MOVEA     LIN           L1
198700010206      * Carico £6
198800010206     C                   CLEAR                   trul06ds
198900010206     C                   MOVE      '£6'          D06COD
199000060516     C                   MOVEL     V1CFGS        D06KEY
199100010206     C                   MOVEL     trul06ds      KPJBU
199200010206     C                   CALL      'TRUL06R'
199300010206     C                   PARM                    KPJBA
199400010206     C                   MOVEL     KPJBU         trul06ds
199500010206     C                   MOVEA     LIN           L6
199600010206      * Decodifico P.o. gestione
199700010604     C     v1cfgs        chain     azorg01L
199800010206     C                   if        %found(azorg01l)
199900010530     C                   movel     orgDES        v1dfgs
200000010206     C                   else
200100010530     C                   clear                   v1dfgs
200200010206     C                   endif
200300010206      * reperisco data e ora
200400010206     C                   TIME                    W0140
200500010206      * UDATE IN GGMMAAAA
200600010206     C                   MOVE      W0140         WDTGIO
200700010206      * UDATE IN AAAAMMGG
200800010206     C                   Z-ADD     WDTGIO        G02DAT
200900010206     C                   MOVEL     *BLANK        G02ERR
201000010206     C                   CALL      'XSRDA8'
201100010206     C                   PARM                    WLBDAT
201200010206     C                   MOVEL     G02INV        DATEU
201300010206
201400010530     C     kfnorm        klist
201500010604     C                   kfld                    v1cpoe
201600010604     C                   kfld                    v1cnsr
201700010604     C                   kfld                    v1cnor
201800010604     C                   kfld                    v1cnrv
201900010206
202000010605     C     kfnorF        klist
202100010605     C                   kfld                    ORMpoe
202200010605     C                   kfld                    ORMnsr
202300010605     C                   kfld                    ORMnor
202400010605     C                   kfld                    ORMnrv
202500010605     C                   kfld                    ORMDFO
202600010605     C                   kfld                    ORMOFO
202700010605     C                   kfld                    ORMFAO
202800020524     C     kfnorF1       klist
202900020524     C                   kfld                    ORMpoe
203000020524     C                   kfld                    ORMnsr
203100020524     C                   kfld                    ORMnor
203200020524     C                   kfld                    ORMnrv
203300010605
203400120613     C     Kfidst        KLIST
203500120613     C                   kfld                    kdstnpg
203600120613     C                   kfld                    kdstnfv
203700120613     C                   kfld                    kdstfgs
203800010605
203900060214     C     kfiar4        klist
204000060214     C                   kfld                    Kar4trc
204100060214     C                   kfld                    kar4n14
204200060214     C                   movel     'M'           kar4trc
204300021202
204400021202     C     Kfapd         klist
204500021202     C                   kfld                    kpdtip
204600021202     C                   kfld                    kpdpdr
204700010605
204800010206     C                   endsr
204900010206      *---------------------------------------------------------------------------------------------
205000001113** MSG
205100080423Filiale gestione errata                                                              1
205200010604Numero O.R.M. errato                                                                 2
205300010604Numero O.R.M. annullato                                                              3
205400010628O.R.M. non in gestione al punto operativo                                            4
205500010705O.R.M. in fase non modificabile                                                      5
205600010605Immettere una sola scelta                                                            6
205700010605Causale Errata                                                                       7
205800010605Causale non gestibile                                                                8
205900010628L'ORM è stato bollettato: non si può dichiarare NON FATTO                            9
206000010605ORM allocato da altro utente                                                        10
206100010605ORM già stato modificato                                                            11
206200010625Numero distinta errato                                                              12
206300010625Distinta non aperta                                                                 13
206400010625Immettere Numero Distinta                                                           14
206500010629Effettuare almeno una scelta                                                        15
206600010629Immettere codice causale                                                            16
206700010706Il valore immesso per il campo non è valido. Immettere 'SI'                         17
206800010706ORM già chiuso/quadrato con la stessa causale                                       18
206900020524ATTENZIONE!! L'ORM è in fase 900. Non si può modificare                             19
207000020524ATTENZIONE!! Annullare la bolla di addebito creata in precedenza                    20
207100041015La distinta deve essere uguale a quella già presente sulla fase ORM                 21
207200070516ORM in uso da altro utente, riprovare più tardi!                                    22
207300081030ORM Commissionato. Non si può usare più di 1 causale con resp. al cliente           23
207400010629** LAV
207500010629Quadratura...............:                                                           1
207600010629Quadratura con causale(?):                                                           2
207700010706Riapertura...............:                                                           3
207800020524** CM3
207900030307OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
208000030307 SHARE(*YES)
208100030623** cmA4
208200030623OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
208300030623 SHARE(*YES)          USRDTA('BolAdd_A4')
208400030623**  cmA5
208500030623OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
208600030623 SHARE(*YES)          USRDTA('BolAdd_A5')
