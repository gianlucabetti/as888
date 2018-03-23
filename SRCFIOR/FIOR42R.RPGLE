000100031009     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200031009
000300031009      *---------------------------------------------------------------*
000400031009      *                                                               *
000500031009      *       STATISTICA ORM X CLIENTE                                *
000600031009      *                                                               *
000700031009      *---------------------------------------------------------------*
000800031009
000900031009      *---------------------------------------------------------------*
001000031009      *  RIEPILOGO INDICATORI
001100031009      *---------------------------------------------------------------*
001200031009      * 06 - Secondo livello
001300031009      * 12 - Sono in sede
001400140606      * 13 - Utente EDP
001500031009      * 28 - ERRORE GENERICO DSPF
001600031009      * 30 - P.O. emissione in L1
001700031009      * 31 - P.O. ritiro in L1
001800031010      * 32 - Comodo
001900031009      * 40 - ERRORE p.o. emissione
002000031009      * 41 - ERRORE p.o. ritiro
002100031009      * 42 - ERRORE data immissione dal
002200031009      * 43 - ERRORE data immissione  al
002300031010      * 44 - ERRORE tipo comunicazione
002400031010      * 45 - ERRORE cliente 1
002500031010      * 46 - ERRORE cliente 2
002600031010      * 47 - ERRORE cliente 3
002700031010      * 48 - ERRORE cliente 4
002800031010      * 49 - ERRORE cliente 5
002900140610      * 50 - Errore Flag mittente 1
003000140610      * 51 - Errore Flag mittente 2
003100140610      * 52 - Errore Flag mittente 3
003200140610      * 53 - Errore Flag mittente 4
003300140610      * 54 - Errore Flag mittente 5
003400140610      * 55 - Errore Commissionato/Alert
003500031010      * 90 - Riemissione videata
003600031009      *---------------------------------------------------------------*
003700031009
003800031009     fAzorg01l  if   e           k Disk
003900031010     fFnacr01l  if   e           k Disk
004000031009     fTabel00f  if   e           k Disk
004100031009     fFior42d   cf   e             Workstn
004200031009
004300031009      *---------------------------------------------------------------*
004400031009
004500031010     d kAcr            s                   Like(AcrCro)
004600031009     d kCod            s                   Like(TblCod)
004700031009     d kKey            s                   Like(TblKey)
004800031010     d sav_g02tgi      s                   Like(G02Tgi)
004900031009
005000031010     d codut           s              1  0 Inz(1)
005100031009     d dataoggi        s              8  0
005200031010     d datamsg         s             10
005300031009     d dtpuliaammgg    s              8  0
005400031009     d dtpuliggmmaa    s              8  0
005500031010     d wcdtal          s              8  0
005600031010     d wcdtda          s              8  0
005700031010     d wdtgio          s              8  0
005800031009     d wgiolav         s              5  0
005900031010     d wgiorno         s              2  0
006000031010     d w0030           s              3  0
006100031010     d w0140           s             14  0
006200031010     d xx              s              2  0
006300031010     d yy              s              2  0
006400031009
006500031009     d wdatada         s               d   datfmt(*iso)
006600031009     d wdataal         s               d   datfmt(*iso)
006700031009
006800031009      *  titolo videata (lunghezza massima 35)
006900031009     d Titolo          c                   const('** Statistica O.R.M. per Clie-
007000031009     d                                     nte **')
007100031009
007200031009      *   S C H I E R E
007300031010     d All             s              1    dim(5)
007400031010     d Cli             s             10    dim(5)
007500031010     d Des             s              1    dim(5)
007600031010     d Mit             s              1    dim(5)
007700031015     d Msg             s             78    dim(15) Ctdata Perrcd(1)             msg video
007800031010     d Ord             s              1    dim(5)
007900031010     d Rag             s             25    dim(5)
008000031009
008100031009      *   D S   I N T E R N E / E S T E R N E
008200031009
008300031010     d datapul         ds                  inz
008400031010     d  giorno                 1      2
008500031010     d  mese                   3      4
008600031010     d  anno                   5      8
008700031010
008800031010     d ds_all          ds
008900031010     d  V1cAll1
009000031010     d  V1cAll2
009100031010     d  V1cAll3
009200031010     d  V1cAll4
009300031010     d  V1cAll5
009400031010
009500031010     d ds_cli1         ds
009600031010     d  V1cCli11
009700031010     d  V1cCli12
009800031010     d  V1cCli13
009900031010
010000031010     d ds_cli2         ds
010100031010     d  V1cCli21
010200031010     d  V1cCli22
010300031010     d  V1cCli23
010400031010
010500031010     d ds_cli3         ds
010600031010     d  V1cCli31
010700031010     d  V1cCli32
010800031010     d  V1cCli33
010900031010
011000031010     d ds_cli4         ds
011100031010     d  V1cCli41
011200031010     d  V1cCli42
011300031010     d  V1cCli43
011400031010
011500031010     d ds_cli5         ds
011600031010     d  V1cCli51
011700031010     d  V1cCli52
011800031010     d  V1cCli53
011900031010
012000031010     d ds_des          ds
012100031010     d  V1cDes1
012200031010     d  V1cDes2
012300031010     d  V1cDes3
012400031010     d  V1cDes4
012500031010     d  V1cDes5
012600031010
012700031010     d ds_mit          ds
012800031010     d  V1cMit1
012900031010     d  V1cMit2
013000031010     d  V1cMit3
013100031010     d  V1cMit4
013200031010     d  V1cMit5
013300031010
013400031010     d ds_ord          ds
013500031010     d  V1cOrd1
013600031010     d  V1cOrd2
013700031010     d  V1cOrd3
013800031010     d  V1cOrd4
013900031010     d  V1cOrd5
014000031010
014100031010     d ds_rag          ds
014200031010     d  V1dCli1
014300031010     d  V1dCli2
014400031010     d  V1dCli3
014500031010     d  V1dCli4
014600031010     d  V1dCli5
014700031010
014800031010     d wgidat          ds                  inz
014900031010     d  giodat                 1      8  0
015000031010     d  gioinv                 9     16  0
015100031010     d  giotgi                17     21  0
015200031010
015300031009     d wlbdat          ds                  inz
015400031009     d  g02dat                 1      8  0
015500031009     d  g02inv                 9     16  0
015600031009     d  g02err                17     17
015700031009     d  g02tgi                18     22  0
015800031009
015900031010     d ds5a2         e ds
016000131216     d FIOR81ds      e ds                  inz
016100031009     d fior42ds      e ds
016200031010     d Tibs02ds      e ds
016300070810      *
016400031009
016500031009     d Trul06ds      e ds
016600031009     d  L1                     1     90  0 dim(30)
016700031009
016800031009     d Tibs34ds      e ds                  Inz
016900031009     d dDatiute      e ds
017000031009     d Azuteds       e ds                  Extname(Azute00f)
017100031009     d Kpjba         e ds
017200031009
017300031009     d                sds
017400031010     d  VtcPgm                 1     10
017500031009
017600031009      *---------------------------------------------------------------*
017700031009
017800031009      * pulizia Videata
017900031009     c                   ExSr      Sr_Pulvid
018000031009
018100031009      * emissione Videata
018200031009     c                   Do        *Hival
018300031009
018400031009     c                   Exfmt     Or42d01
018500031009
018600031009      * F3=Fine
018700031009     c   kc              Leave
018800031010     c                   Eval      *In28 = *Off
018900031010     c                   Eval      *In90 = *Off
019000031009
019100031009      * controlli
019200031009     c                   ExSr      Sr_Contr
019300031010     c   28
019400031010     cor 90              Iter
019500031009
019600031009      * F6=Elabora
019700031010     c                   If        *Inkf
019800031010     c                   ExSr      Sr_Elabora
019900031010     c                   Leave
020000031009     c                   EndIf
020100031009
020200031009     c                   EndDo
020300031009
020400031009     c                   Eval      *InLr = *On
020500031009
020600031009      *---------------------------------------------------------------*
020700031009      * PULIZIA VIDEATA
020800031009      *---------------------------------------------------------------*
020900031009     c     Sr_Pulvid     BegSr
021000031009
021100031009     c                   Clear                   V1cDtda
021200031009     c                   Clear                   V1cDtal
021300031009     c                   Clear                   V1cTco
021400031009     c                   Eval      V1cFile = 'N'
021500031010     c                   Clear                   ds_Cli1
021600031010     c                   Clear                   ds_Cli2
021700031010     c                   Clear                   ds_Cli3
021800031010     c                   Clear                   ds_Cli4
021900031010     c                   Clear                   ds_Cli5
022000031010     c                   Clear                   ds_Rag
022100031010     c                   Clear                   ds_All
022200031010     c                   Clear                   ds_Mit
022300031010     c                   Clear                   ds_Ord
022400031010     c                   Clear                   ds_Des
022500031009
022600031009     c                   EndSr
022700031009
022800031009      *---------------------------------------------------------------*
022900031009      * CONTROLLO I DATI DELLA VIDEATA
023000031009      *---------------------------------------------------------------*
023100031009     c     Sr_Contr      BegSr
023200140610
023300140610      /free
023400140610        *in55 = *off;
023500140610      /end-free
023600031009
023700031009      * p.o. emissione
023800031009     c                   ExSr      Sr_ContrPoe
023900031009     c                   If        *In28
024000031009     c                   GoTo      EndContr
024100031009     c                   EndIf
024200031009
024300031009      * p.o. ritiro
024400031009     c                   ExSr      Sr_ContrPor
024500031009     c                   If        *In28
024600031009     c                   GoTo      EndContr
024700031009     c                   EndIf
024800031009
024900031009      * Controllo le combinazioni tra p.o. emissione e p.o. ritiro
025000031009     c                   ExSr      Sr_ContrPo
025100031009     c                   If        *In28
025200031009     c                   GoTo      EndContr
025300031009     c                   EndIf
025400031009
025500031009      * Controllo le date
025600031010     c                   ExSr      Sr_ContrDate
025700031009     c                   If        *In28
025800031009     c                   GoTo      EndContr
025900031009     c                   EndIf
026000031010
026100031010      * Controllo il tipo comunicazione
026200031010     c                   ExSr      Sr_ContrTco
026300031010     c                   If        *In28
026400031010     c                   GoTo      EndContr
026500031010     c                   EndIf
026600140610      /free
026700140610       //?Controllo selezioni commissionato/alert
026800140610        IF  V1Ccom = 'E' and V1Calert = 'I';
026900140610          *in28 = *on;
027000140610          *in55 = *on;
027100140610          V1Cmsg = msg(12);
027200140610          leavesr;
027300140610        ENDIF;
027400140610      /end-free
027500031009
027600031010      * Controllo i clienti
027700031010     c                   ExSr      Sr_ContrCli
027800031010     c                   If        *In28
027900031010     c                   GoTo      EndContr
028000031010     c                   EndIf
028100031010
028200031016      * Controllo il tipo cliente
028300031016     c                   ExSr      Sr_ContrtCli
028400031016     c                   If        *In28
028500031016     c                   GoTo      EndContr
028600031016     c                   EndIf
028700031016
028800031009     c     EndContr      EndSr
028900031009
029000031009      *---------------------------------------------------------------*
029100031009      * CONTROLLO IL P.O. EMISSIONE
029200031009      *---------------------------------------------------------------*
029300031009     c     Sr_ContrPoe   BegSr
029400031009
029500031009     c                   Eval      *In40 = *Off
029600031009     c                   Clear                   V1dPoe
029700031009
029800031009     c                   If        V1cPoe = 999
029900031009     c                   Clear                   V1dPoe
030000031010     c                   Eval      V1dPoe = 'Tutti i p.o.'
030100031009     c                   GoTo      EndContrPoe
030200031009     c                   EndIf
030300031009
030400031009      * non deve essere a zero
030500031009     c                   If        V1cPoe = *Zeros
030600031009     c                   Eval      *In40 = *On
030700031009     c                   Eval      *In28 = *On
030800031009     c                   Eval      V1cMsg = Msg(1)
030900031009     c                   GoTo      EndContrPoe
031000031009     c                   EndIf
031100031009
031200031009      * Controllo se p.o. valido
031300031009     c     V1cPoe        Chain     Azorg01L
031400031010     c                   If         Not %Found(Azorg01l) or
031500031009     c                              OrgFva <> *Blanks or
031600031009     c                             (OrgFag <> 'A' and OrgFag <> 'F')
031700031009     c                   Eval      *In40 = *On
031800031009     c                   Eval      *In28 = *On
031900031009     c                   Eval      V1cMsg = Msg(1)
032000031009     c                   GoTo      EndContrPoe
032100031009     c                   EndIf
032200031009
032300031009     c                   Movel     OrgDes        V1dPoe
032400031009
032500031009     c     EndContrPoe   EndSr
032600031009
032700031009      *---------------------------------------------------------------*
032800031009      * CONTROLLO IL P.O. RITIRO
032900031009      *---------------------------------------------------------------*
033000031009     c     Sr_ContrPor   BegSr
033100031009
033200031009     c                   Eval      *In41 = *Off
033300031009     c                   Clear                   V1dPor
033400031009
033500031009     c                   If        V1cPor = 999
033600031009     c                   Clear                   V1dPor
033700031010     c                   Eval      V1dPor = 'Tutti i p.o.'
033800031009     c                   GoTo      EndContrPor
033900031009     c                   EndIf
034000031009
034100031009      * non deve essere a zero
034200031009     c                   If        V1cPor = *Zeros
034300031009     c                   Eval      *In41 = *On
034400031009     c                   Eval      *In28 = *On
034500031009     c                   Eval      V1cMsg = Msg(1)
034600031009     c                   GoTo      EndContrPor
034700031009     c                   EndIf
034800031009
034900031009      * Controllo se p.o. valido
035000031009     c     V1cPor        Chain     Azorg01L
035100031010     c                   If         Not %Found(Azorg01l) or
035200031009     c                              OrgFva <> *Blanks or
035300031009     c                             (OrgFag <> 'A' and OrgFag <> 'F')
035400031009     c                   Eval      *In41 = *On
035500031009     c                   Eval      *In28 = *On
035600031009     c                   Eval      V1cMsg = Msg(1)
035700031009     c                   GoTo      EndContrPor
035800031009     c                   EndIf
035900031009
036000031009     c                   Movel     OrgDes        V1dPor
036100031009
036200031009     c     EndContrPor   EndSr
036300031009
036400031009      *---------------------------------------------------------------*
036500031009      * CONTROLLO LE COMBINAZIONI TRA P.O. EMISSIONE E P.O. RITIRO
036600031009      *---------------------------------------------------------------*
036700031009     c     Sr_ContrPo    BegSr
036800031009
036900031009     c                   Eval      *In41 = *Off
037000031009
037100031009     c                   Select
037200031009
037300031009      * sono in filiale e sono primo livello
037400031009     c                   When      Not *In12 and Not *In06
037500031010      * non posso richiedere tutti i p.o. come emissione e come ritiro
037600031010if  1c                   If        V1cPoe = 999 and V1cpor = 999
037700031010     c                   Eval      *In41 = *On
037800031010     c                   Eval      *In28 = *On
037900031010     c                   Eval      V1cMsg = Msg(2)
038000031010     c                   GoTo      EndContrPo
038100031010    1c                   EndIf
038200031010     c     V1cPoe        Lookup    L1                                     30
038300031010     c     V1cPor        Lookup    L1                                     31
038400031010      * non posso richiedere p.o. emissione e p.o. ritiro non del terminal
038500031010if  1c                   If        Not *In30 and Not *In31
038600031010     c                   Eval      *In41 = *On
038700031010     c                   Eval      *In28 = *On
038800031010     c                   Eval      V1cMsg = Msg(2)
038900031010     c                   GoTo      EndContrPo
039000031010    1c                   EndIf
039100031009
039200031009      * sono in filiale e sono secondo livello
039300031009     c                   When      Not *In12 and *In06
039400031009      * P.o. emissione non sono io
039500031010if  1c                   If        V1cPoe <> Dutpou and
039600031009      * come p.o. ritiro posso richiedere solo me stesso
039700031010     c                             V1cPor <> Dutpou
039800031009     c                   Eval      *In41 = *On
039900031009     c                   Eval      *In28 = *On
040000031009     c                   Eval      V1cMsg = Msg(3)
040100031009     c                   GoTo      EndContrPo
040200031009    1c                   EndIf
040300031009
040400031009     c                   EndSl
040500031009
040600031009     c     EndContrPo    EndSr
040700031009
040800031009      *---------------------------------------------------------------*
040900031009      * CONTROLLO LE DATE
041000031009      *---------------------------------------------------------------*
041100031009     c     Sr_ContrDate  BegSr
041200031009
041300031009     c                   Eval      *In42 = *Off
041400031009     c                   Eval      *In43 = *Off
041500031009
041600031009      * data 'da' deve essere impostata
041700031009     c                   If        V1cDtda = *Zeros
041800031009     c                   Eval      *In42 = *On
041900031009     c                   Eval      *In28 = *On
042000031009     c                   Eval      V1cMsg = Msg(4)
042100031009     c                   GoTo      EndContrDate
042200031009     c                   EndIf
042300031009
042400031009      * controllo la data inserita 'da'
042500031009     c                   Clear                   wcdtda
042600031009     c                   Clear                   wlbdat
042700031009     c                   Z-Add     V1cDtda       G02Dat
042800031009     c                   Call      'XSRDA8'
042900031009     c                   Parm                    wlbdat
043000031009     c                   If        G02Err = '1'
043100031009     c                   Eval      *In42 = *On
043200031009     c                   Eval      *In28 = *On
043300031009     c                   Eval      V1cMsg = Msg(5)
043400031009     c                   GoTo      EndContrDate
043500031009     c                   Else
043600031009     c                   Z-Add     G02Dat        V1cDtda
043700031009     c                   Z-Add     G02Inv        wcdtda
043800031009     c                   EndIf
043900031009
044000031009      * non può essere > ad oggi
044100031009     c                   If        wcdtda > dataoggi
044200031009     c                   Eval      *In42 = *On
044300031009     c                   Eval      *In28 = *On
044400031009     c                   Eval      V1cMsg = Msg(6)
044500031009     c                   GoTo      EndContrDate
044600031009     c                   EndIf
044700031009
044800031009      * deve essere > della data di pulizia ORM
044900031009     c                   If        wcdtda <= dtpuliaammgg
045000031009     c                   Eval      *In42 = *On
045100031009     c                   Eval      *In28 = *On
045200031009     c                   Eval      V1cMsg = Msg(7)
045300031009     c                   Movel     dtpuliggmmaa  datapul
045400031009     c                   Eval      datamsg = %trim (giorno + '/' +
045500031009     c                             mese + '/' + anno)
045600031009     c                   Eval      %subst(V1cMsg: 62: 10) = datamsg
045700031009     c                   GoTo      EndContrDate
045800031009     c                   EndIf
045900031009
046000031009      * controllo la data inserita 'al'
046100031010if  1c                   If        V1cDtal <> *Zeros
046200031009     c                   Clear                   wcdtal
046300031009     c                   Clear                   wlbdat
046400031009     c                   Z-Add     V1cDtal       G02Dat
046500031009     c                   Call      'XSRDA8'
046600031009     c                   Parm                    wlbdat
046700031010if  2c                   If        G02Err = '1'
046800031009     c                   Eval      *In43 = *On
046900031009     c                   Eval      *In28 = *On
047000031009     c                   Eval      V1cMsg = Msg(5)
047100031009     c                   GoTo      EndContrDate
047200031010   x2c                   Else
047300031009     c                   Z-Add     G02Dat        V1cDtal
047400031009     c                   Z-Add     G02Inv        wcdtal
047500031010    2c                   EndIf
047600031009
047700031009      * non può essere > ad oggi
047800031010if  2c                   if        wcdtal > dataoggi
047900031009     c                   Eval      *In43 = *On
048000031009     c                   Eval      *In28 = *On
048100031009     c                   Eval      V1cMsg = Msg(6)
048200031009     c                   GoTo      EndContrDate
048300031010    2c                   EndIf
048400031009
048500031009      * deve essere > della data di pulizia ORM
048600031010if  2c                   If        wcdtal <= dtpuliaammgg
048700031009     c                   Eval      *In43 = *On
048800031009     c                   Eval      *In28 = *On
048900031009     c                   Eval      V1cMsg = Msg(7)
049000031009     c                   Movel     dtpuliggmmaa  datapul
049100031009     c                   Eval      datamsg = %trim (giorno + '/' +
049200031009     c                             mese + '/' + anno)
049300031009     c                   Eval      %subst(V1cMsg: 62: 10) = datamsg
049400031009     c                   GoTo      EndContrDate
049500031010    2c                   EndIf
049600031009
049700031009      * range
049800031010if  2c                   If        wcdtda > wcdtal
049900031009     c                   Eval      *In42 = *On
050000031009     c                   Eval      *In28 = *On
050100031009     c                   Eval      V1cMsg = Msg(5)
050200031009     c                   GoTo      EndContrDate
050300031010    2c                   EndIf
050400031010    1c                   EndIf
050500031009
050600031009     c     EndContrDate  EndSr
050700031010
050800031010      *---------------------------------------------------------------*
050900031010      * CONTROLLO IL TIPO COMUNICAZIONE
051000031010      *---------------------------------------------------------------*
051100031010     c     Sr_ContrTco   BegSr
051200031010
051300031010     c                   Eval      *In44 = *Off
051400031010
051500031010      * Ricerca tipo comunicazione con "?"
051600031010     c     '?'           Scan      V1cTco                                 32
051700031010     c                   If        *In32
051800031010     c                   Eval      *In44 = *On
051900031010     c                   Eval      *In90 = *On
052000031010     c                   Clear                   Tibs02ds
052100031010     c                   Eval      T02Mod = 'R'
052200031010     c                   Eval      T02Sif = Knsif
052300031010     c                   Eval      T02Cod = 'TCO'
052400031010     c                   Call      'TIBS02R'
052500031010     c                   Parm                    Kpjba
052600031010     c                   Parm                    Tibs02ds
052700031010     c                   Movel     T02Ke1        V1cTco
052800031010     c                   Movel     T02Uni        V1dTco
052900031010     c                   GoTo      EndContrTco
053000031010     c                   EndIf
053100031010
053200031010      * Controllo
053300031010if  1c                   If        V1cTco <> *Blanks
053400031010     c                   Clear                   Tibs02ds
053500031010     c                   Eval      T02Mod = 'C'
053600031010     c                   Eval      T02Sif = Knsif
053700031010     c                   Eval      T02Cod = 'TCO'
053800031010     c                   Movel(p)  V1cTco        T02Ke1
053900031010     c                   Call      'TIBS02R'
054000031010     c                   Parm                    Kpjba
054100031010     c                   Parm                    Tibs02ds
054200031010if  2c                   If        T02Err <> *Blanks
054300031010     c                   Eval      V1cMsg = Msg(8)
054400031010     c                   Eval      *In44 = *On
054500031010     c                   Eval      *In28 = *On
054600031010     c                   GoTo      EndContrTco
054700031010    2c                   EndIf
054800031010     c                   Movel     T02Uni        V1dTco
054900031010    1c                   EndIf
055000031010
055100031010     c     EndContrTco   EndSr
055200031009
055300031010      *---------------------------------------------------------------*
055400031010      * CONTROLLO I CLIENTI
055500031010      *---------------------------------------------------------------*
055600031010     c     Sr_ContrCli   BegSr
055700031016
055800031016     c                   Clear                   ds_rag
055900031010
056000031010     c                   Do        5             xx
056100031010     c                   Eval      yy = (44 + xx)
056200031010     c                   Eval      *In(yy) = *Off
056300031010     c                   EndDo
056400031010
056500031010     c                   Move      ds_cli1       cli(1)
056600031010     c                   Move      ds_cli2       cli(2)
056700031010     c                   Move      ds_cli3       cli(3)
056800031010     c                   Move      ds_cli4       cli(4)
056900031010     c                   Move      ds_cli5       cli(5)
057000031010
057100031010     c                   Movea     ds_rag        rag
057200031010     c                   Movea     ds_all        all
057300031010
057400031015      * Almeno un cliente deve essere inserito
057500031015     c                   If        cli(1) = *Blanks and cli(2) = *Blanks and
057600031015     c                             cli(3) = *Blanks and cli(4) = *Blanks and
057700031015     c                             cli(5) = *Blanks
057800031015     c                   Eval      *In45 = *On
057900031016     c                   Eval      V1cMsg = Msg(10)
058000031015     c                   Eval      *In28 = *On
058100031015     c                   GoTo      EndContrCli
058200031015     c                   EndIf
058300031010
058400031010do  1c                   Do        5             xx
058500031010     c                   Eval      yy = (44 + xx)
058600031016if  2c                   If        cli(xx) <> *Blanks
058700031010      * Ricerca codice cliente
058800031010     c     '?'           Scan      cli(xx)                                32
058900031016if  3c                   If        *In32
059000031010     c                   Eval      *In(yy) = *On
059100031010     c                   Eval      *In90 = *On
059200131216     c                   clear                   FIOR81ds
059300131216     c  N12              movel     DUTpou        I81fil
059400131216     c                   movel(p)  FIOR81ds      KPJBU
059500131216     c                   call      'FIOR81R'
059600070809     c                   parm                    KPJBA
059700131216     c                   parm                    FIOR81ds
059800070809sel 4c                   select
059900131216w   4c                   when      O81err <> *blanks
060000131216     c                   eval      V1Cmsg  = O81msg
060100070809     c                   eval      *in(yy) = *on
060200070809     c                   eval      *in28   = *on
060300131216w   4c                   when      O81cro <> *zeros
060400131216     c                   movel     O81cro        Cli(xx)
060500070809     c                   move      Cli(xx)       kACR
060600070809     c                   exsr      sr_DecoCli
060700070809e   4c                   endsl
060800031010
060900031010      * Decodifico e controllo
061000031016   x3c                   Else
061100031010     c                   Move      cli(xx)       kAcr
061200031010     c                   ExSr      Sr_DecoCli
061300031010     c   28              Leave
061400031016    3c                   EndIf
061500031016    2c                   EndIf
061600031010    1c                   EndDo
061700031010
061800031010     c                   Move      cli(1)        ds_cli1
061900031010     c                   Move      cli(2)        ds_cli2
062000031010     c                   Move      cli(3)        ds_cli3
062100031010     c                   Move      cli(4)        ds_cli4
062200031010     c                   Move      cli(5)        ds_cli5
062300031010
062400031010     c                   Movea     rag           ds_rag
062500031010     c                   Movea     all           ds_all
062600031010
062700031015     c     EndContrCli   EndSr
062800031016
062900031016      *---------------------------------------------------------------*
063000031016      * CONTROLLO IL TIPO CLIENTE
063100031016      *---------------------------------------------------------------*
063200031016     c     Sr_ContrtCli  BegSr
063300031016
063400031016     c                   Do        5             xx
063500031016     c                   Eval      yy = (49 + xx)
063600031016     c                   Eval      *In(yy) = *Off
063700031016     c                   EndDo
063800031016
063900031016     c                   Movea     ds_mit        mit
064000031016     c                   Movea     ds_ord        ord
064100031016     c                   Movea     ds_des        des
064200031016
064300031016do  1c                   Do        5             xx
064400031016     c                   Eval      yy = (49 + xx)
064500031016if  2c                   If        cli(xx) <> *Blanks
064600031016      * devo richiedere il tipo cliente se il codice è inserito
064700031016if  3c                   If        mit(xx) = *Blanks and ord(xx) = *Blanks and
064800031016     c                             des(xx) = *Blanks
064900031016     c                   Eval      *In(yy) = *On
065000031016     c                   Eval      *In28 = *On
065100031016     c                   Eval      V1cMsg = Msg(11)
065200031016     c                   Leave
065300031016    3c                   EndIf
065400031016    2c                   EndIf
065500031016    1c                   EndDo
065600031016
065700031016     c                   Movea     mit           ds_mit
065800031016     c                   Movea     ord           ds_ord
065900031016     c                   Movea     des           ds_des
066000031016
066100031016     c                   EndSr
066200031010
066300031010      *---------------------------------------------------------------*
066400031010      * DECODIFICA CLIENTI
066500031010      *---------------------------------------------------------------*
066600031010     c     Sr_DecoCli    BegSr
066700031010
066800031010      * se richiesto tutti i luogi cerco se almeno 1 esiste
066900031016if  1c                   If        all(xx) = 'S'
067000031010     c                   Move      000           kAcr
067100031010     c     kAcr          Setll     Fnacr01l
067200031016do  2c                   Do        *Hival
067300031010     c                   Read      Fnacr01l
067400031010     c                   If        %Eof(Fnacr01l)
067500031010     c                   Leave
067600031010     c                   EndIf
067700031016     c                   Movel     AcrRsr        rag(xx)
067800031010     c                   Leave
067900031016    2c                   EndDo
068000031016     c                   If        rag(xx) = *Blanks
068100031010     c                   Eval      V1cMsg = Msg(9)
068200031016     c                   Eval      *In(yy) = *On
068300031010     c                   Eval      *In28 = *On
068400031010     c                   GoTo      EndDecoCli
068500031010     c                   EndIf
068600031010      * se no cerco il cliente specifico
068700031016   x1c                   Else
068800031010     c     kAcr          Chain     Fnacr01l
068900031010     c                   If        Not %Found(Fnacr01l) or
069000031010     c                             AcrAtb <> *blanks
069100031010     c                   Eval      V1cMsg = Msg(9)
069200031016     c                   Eval      *In(yy) = *On
069300031010     c                   Eval      *In28 = *On
069400031010     c                   GoTo      EndDecoCli
069500031010     c                   EndIf
069600031016     c                   Movel     AcrRsr        rag(xx)
069700031010    1c                   EndIf
069800031010
069900031010     c     EndDecoCli    EndSr
070000031010
070100031009      *---------------------------------------------------------------*
070200031009      * ELABORA
070300031009      *---------------------------------------------------------------*
070400031009     c     Sr_Elabora    BegSr
070500010606
070600031009     c                   Eval      i42Poe = V1cPoe
070700031009     c                   Eval      i42Por = V1cPor
070800031009     c                   Eval      i42Dtda = wcdtda
070900031010      * data 'al' se non impostata è = a data 'da'
071000031010     c                   If        wcdtal = *zeros
071100031010     c                   Eval      wcdtal = wcdtda
071200031010     c                   EndIf
071300031009     c                   Eval      i42Dtal = wcdtal
071400031010     c                   Eval      i42Tco = V1cTco
071500140610     c                   eval      I42comm = V1Ccom
071600140606     c                   eval      I42alert = V1Calert
071700031009     c                   Eval      i42File = V1cFile
071800031010     c                   Move      ds_cli1       i42cli1
071900031010     c                   Move      ds_cli2       i42cli2
072000031010     c                   Move      ds_cli3       i42cli3
072100031010     c                   Move      ds_cli4       i42cli4
072200031010     c                   Move      ds_cli5       i42cli5
072300031010     c                   Eval      i42all1 = V1cAll1
072400031010     c                   Eval      i42all2 = V1cAll2
072500031010     c                   Eval      i42all3 = V1cAll3
072600031010     c                   Eval      i42all4 = V1cAll4
072700031010     c                   Eval      i42all5 = V1cAll5
072800031010     c                   Eval      i42mit1 = V1cmit1
072900031010     c                   Eval      i42mit2 = V1cmit2
073000031010     c                   Eval      i42mit3 = V1cmit3
073100031010     c                   Eval      i42mit4 = V1cmit4
073200031010     c                   Eval      i42mit5 = V1cmit5
073300031010     c                   Eval      i42ord1 = V1cord1
073400031010     c                   Eval      i42ord2 = V1cord2
073500031010     c                   Eval      i42ord3 = V1cord3
073600031010     c                   Eval      i42ord4 = V1cord4
073700031010     c                   Eval      i42ord5 = V1cord5
073800031010     c                   Eval      i42des1 = V1cdes1
073900031010     c                   Eval      i42des2 = V1cdes2
074000031010     c                   Eval      i42des3 = V1cdes3
074100031010     c                   Eval      i42des4 = V1cdes4
074200031010     c                   Eval      i42des5 = V1cdes5
074300010606
074400031014     c                   clear                   kpjbu
074500031014     c                   movel     fior42ds      kpjbu
074600031014
074700031014     c                   Move      'OR43'        Kcoaz
074800031014     c                   If        *In12
074900031014     c                   Call      'BCH09'
075000031014     c                   Parm                    Kpjba
075100031014     c                   EndIf
075200031014     c                   Call      'BCH10'
075300031014     c                   Parm                    Kpjba
075400010606
075500010618     c                   endsr
075600031009      *---------------------------------------------------------------*
075700031009      * SUBROUTINE INIZIALE                                           *
075800031009      *---------------------------------------------------------------*
075900031009     c     *Inzsr        BegSr
076000001009
076100031009     c     *Entry        Plist
076200031009     c                   Parm                    Kpjba
076300010618
076400031009     c     *dtaara       define    §azute        azuteds
076500031009     c     *dtaara       define    §datiute      ddatiute
076600031009     c                   in(E)     *dtaara
076700031009     c                   IF        %error  or RSUT = *blanks
076800031009     c                   CLEAR                   tibs34ds
076900031009     c                   CALL      'TIBS34R'
077000031009     c                   PARM                    tibs34ds
077100031009     c                   in        *dtaara
077200031009     c                   endif
077300031009
077400031009     c                   Eval      VtcTit = Titolo
077500031009     c                   Clear                   fior42ds
077600031009     c                   Clear                   V1cPoe
077700031009     c                   Clear                   V1dPoe
077800031009     c                   Clear                   V1cPor
077900031009     c                   Clear                   V1dPor
078000031009
078100031009      * Se elaborazione da sede imposto p.o. ritiro e p.o. emissione a 999
078200031009if  1c                   If        Simfel = *Zeros
078300031009     c                   Eval      *In12 = *On
078400031009     c                   Eval      V1cPoe = 999
078500031010     c                   Eval      V1dPoe = 'Tutti i p.o.'
078600031009     c                   Eval      V1cPor = 999
078700031010     c                   Eval      V1dPor = 'Tutti i p.o.'
078800031009   x1c                   Else
078900031009      * da filiale imposto il p.o. da elaborare
079000031010if  2c                   If        Dutlpo = '2' or Dutlpo = *Blanks
079100031009      * secondo livello
079200031010     c                   Eval      V1cPoe = Dutpou
079300031009     c                   Eval      V1cPor = 999
079400031010     c                   Eval      V1dPor = 'Tutti i p.o.'
079500031009     c                   Eval      *In06 = *On
079600031009   x2c                   Else
079700031009      * primo livello
079800031009     c                   Eval      V1cPoe = Simfel
079900031009     c                   Eval      V1cPor = 999
080000031010     c                   Eval      V1dPor = 'Tutti i p.o.'
080100031009    2c                   EndIf
080200031009
080300031009      * Decodifico il p.o. emissione impostato
080400031009     c     V1cPoe        Chain     Azorg01L
080500031010if  2c                   If        %Found(Azorg01l)
080600031009     c                   Movel     OrgDes        V1dPoe
080700031009    2c                   EndIf
080800031009      * Carico i p.o. del terminal di partenza
080900031009     c                   Clear                   Trul06Ds
081000031009     c                   Eval      D06Cod = '£1'
081100031009     c                   Movel     Simfel        D06Key
081200031009     c                   Movel(p)  Trul06ds      Kpjbu
081300031009     c                   Call      'TRUL06R'
081400031009     c                   Parm                    Kpjba
081500031009     c                   Movel     Kpjbu         Trul06ds
081600031009    1c                   EndIf
081700140606
081800140606      /free
081900140606       //?Controllo se utente EDP
082000140606if  1   *in13 = (%subst(knmus:1:3) = 'EDP');
082100140606      /end-free
082200001009
082300031009      * reperisce data e ora
082400031009     c                   Time                    w0140
082500031009      * Udate in GGMMAAAA
082600031009     c                   Move      w0140         wdtgio
082700031009      * Udate in AAAAMMGG
082800031009     c                   Z-Add     wdtgio        G02Dat
082900031009     c                   Movel     *Blanks       G02Err
083000031009     c                   Call      'XSRDA8'
083100031009     c                   Parm                    wlbdat
083200031009     c                   Z-Add     G02Inv        dataoggi
083300031009     c                   z-add     g02tgi        sav_g02tgi
083400031009
083500031009      * cerco data limite lancio elaborazione
083600031009     c                   Clear                   ds5a2
083700031009     c                   Movel     '5A'          kCod
083800031009     c                   Movel     '2       '    kKey
083900031009     c     kTab          Chain     Tabel00f
084000031009     c                   if        %Found(Tabel00f) and TblFlg = *Blanks
084100031009     c                   Movel     TblUni        ds5a2
084200031009     c                   Z-Add     §5aorf        w0030
084300031009     c                   EndIf
084400010618
084500010618      * Calcolo data
084600031009     c     sav_g02tgi    Sub       w0030         giotgi
084700031009     c                   Call      'XSRGI8'
084800031009     c                   Parm                    wgidat
084900031009     c                   Z-Add     giodat        dtpuliggmmaa
085000010618
085100010618      * Giro la data
085200031009     c                   If        dtpuliggmmaa > *Zeros
085300031009     c                   Move      dtpuliggmmaa  G02Dat
085400031009     c                   Movel     *Blanks       G02Err
085500031009     c                   Call      'XSRDA8'
085600031009     c                   Parm                    wlbdat
085700031009     c                   If        G02Err = '1'
085800031009     c                   Clear                   dtpuliggmmaa
085900031009     c                   Else
086000031009     c                   Z-Add     G02Inv        dtpuliaammgg
086100031009     c                   EndIf
086200031009     c                   EndIf
086300031009
086400031009     c     kTab          Klist
086500031009     c                   Kfld                    Codut
086600031009     c                   Kfld                    kCod
086700031009     c                   Kfld                    kKey
086800010618
086900031009     c                   EndSr
087000001010      *---------------------------------------------------------------------------------------------
087100001031** MSG  Lungh. 78                                                            *
087200070418Filiale errata                                                                      1
087300070418Immettere una filiale del terminal                                                  2
087400070418Immettere la filiale utente                                                         3
087500031009Inserire la data immissione                                                         4
087600031009Data immissione errata                                                              5
087700031009Data immissione non può essere superiore alla data odierna                          6
087800031009Data immissione deve essere maggiore alla data pulizia ORM                          7
087900031010Tipo comunicazione errato                                                           8
088000031010Cliente errato                                                                      9
088100031016Immettere il codice cliente                                                         10
088200031016Se immesso il codice cliente immettere anche il tipo cliente                        11
088300140610Se ESCLUDI orm COMMISSIONATI non è possibile scegliere INCLUDI orm con ALERT        12
