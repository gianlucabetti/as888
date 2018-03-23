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
001400031009      * 28 - ERRORE GENERICO DSPF
001500031009      * 30 - P.O. emissione in L1
001600031009      * 31 - P.O. ritiro in L1
001700031010      * 32 - Comodo
001800031009      * 40 - ERRORE p.o. emissione
001900031009      * 41 - ERRORE p.o. ritiro
002000031009      * 42 - ERRORE data immissione dal
002100031009      * 43 - ERRORE data immissione  al
002200031010      * 44 - ERRORE tipo comunicazione
002300031010      * 45 - ERRORE cliente 1
002400031010      * 46 - ERRORE cliente 2
002500031010      * 47 - ERRORE cliente 3
002600031010      * 48 - ERRORE cliente 4
002700031010      * 49 - ERRORE cliente 5
002800031010      * 90 - Riemissione videata
002900031009      *---------------------------------------------------------------*
003000031009
003100031009     fAzorg01l  if   e           k Disk
003200031010     fFnacr01l  if   e           k Disk
003300031009     fTabel00f  if   e           k Disk
003400031009     fFior42d   cf   e             Workstn
003500031009
003600031009      *---------------------------------------------------------------*
003700031009
003800031010     d kAcr            s                   Like(AcrCro)
003900031009     d kCod            s                   Like(TblCod)
004000031009     d kKey            s                   Like(TblKey)
004100031010     d sav_g02tgi      s                   Like(G02Tgi)
004200031009
004300031010     d codut           s              1  0 Inz(1)
004400031009     d dataoggi        s              8  0
004500031010     d datamsg         s             10
004600031009     d dtpuliaammgg    s              8  0
004700031009     d dtpuliggmmaa    s              8  0
004800031010     d wcdtal          s              8  0
004900031010     d wcdtda          s              8  0
005000031010     d wdtgio          s              8  0
005100031009     d wgiolav         s              5  0
005200031010     d wgiorno         s              2  0
005300031010     d w0030           s              3  0
005400031010     d w0140           s             14  0
005500031010     d xx              s              2  0
005600031010     d yy              s              2  0
005700031009
005800031009     d wdatada         s               d   datfmt(*iso)
005900031009     d wdataal         s               d   datfmt(*iso)
006000031009
006100031009      *  titolo videata (lunghezza massima 35)
006200031009     d Titolo          c                   const('** Statistica O.R.M. per Clie-
006300031009     d                                     nte **')
006400031009
006500031009      *   S C H I E R E
006600031010     d All             s              1    dim(5)
006700031010     d Cli             s             10    dim(5)
006800031010     d Des             s              1    dim(5)
006900031010     d Mit             s              1    dim(5)
007000031015     d Msg             s             78    dim(15) Ctdata Perrcd(1)             msg video
007100031010     d Ord             s              1    dim(5)
007200031010     d Rag             s             25    dim(5)
007300031009
007400031009      *   D S   I N T E R N E / E S T E R N E
007500031009
007600031010     d datapul         ds                  inz
007700031010     d  giorno                 1      2
007800031010     d  mese                   3      4
007900031010     d  anno                   5      8
008000031010
008100031010     d ds_all          ds
008200031010     d  V1cAll1
008300031010     d  V1cAll2
008400031010     d  V1cAll3
008500031010     d  V1cAll4
008600031010     d  V1cAll5
008700031010
008800031010     d ds_cli1         ds
008900031010     d  V1cCli11
009000031010     d  V1cCli12
009100031010     d  V1cCli13
009200031010
009300031010     d ds_cli2         ds
009400031010     d  V1cCli21
009500031010     d  V1cCli22
009600031010     d  V1cCli23
009700031010
009800031010     d ds_cli3         ds
009900031010     d  V1cCli31
010000031010     d  V1cCli32
010100031010     d  V1cCli33
010200031010
010300031010     d ds_cli4         ds
010400031010     d  V1cCli41
010500031010     d  V1cCli42
010600031010     d  V1cCli43
010700031010
010800031010     d ds_cli5         ds
010900031010     d  V1cCli51
011000031010     d  V1cCli52
011100031010     d  V1cCli53
011200031010
011300031010     d ds_des          ds
011400031010     d  V1cDes1
011500031010     d  V1cDes2
011600031010     d  V1cDes3
011700031010     d  V1cDes4
011800031010     d  V1cDes5
011900031010
012000031010     d ds_mit          ds
012100031010     d  V1cMit1
012200031010     d  V1cMit2
012300031010     d  V1cMit3
012400031010     d  V1cMit4
012500031010     d  V1cMit5
012600031010
012700031010     d ds_ord          ds
012800031010     d  V1cOrd1
012900031010     d  V1cOrd2
013000031010     d  V1cOrd3
013100031010     d  V1cOrd4
013200031010     d  V1cOrd5
013300031010
013400031010     d ds_rag          ds
013500031010     d  V1dCli1
013600031010     d  V1dCli2
013700031010     d  V1dCli3
013800031010     d  V1dCli4
013900031010     d  V1dCli5
014000031010
014100031010     d wgidat          ds                  inz
014200031010     d  giodat                 1      8  0
014300031010     d  gioinv                 9     16  0
014400031010     d  giotgi                17     21  0
014500031010
014600031009     d wlbdat          ds                  inz
014700031009     d  g02dat                 1      8  0
014800031009     d  g02inv                 9     16  0
014900031009     d  g02err                17     17
015000031009     d  g02tgi                18     22  0
015100031009
015200031010     d ds5a2         e ds
015300131216     d FIOR81ds      e ds                  inz
015400031009     d fior42ds      e ds
015500031010     d Tibs02ds      e ds
015600070810      *
015700031009
015800031009     d Trul06ds      e ds
015900031009     d  L1                     1     90  0 dim(30)
016000031009
016100031009     d Tibs34ds      e ds                  Inz
016200031009     d dDatiute      e ds
016300031009     d Azuteds       e ds                  Extname(Azute00f)
016400031009     d Kpjba         e ds
016500031009
016600031009     d                sds
016700031010     d  VtcPgm                 1     10
016800031009
016900031009      *---------------------------------------------------------------*
017000031009
017100031009      * pulizia Videata
017200031009     c                   ExSr      Sr_Pulvid
017300031009
017400031009      * emissione Videata
017500031009     c                   Do        *Hival
017600031009
017700031009     c                   Exfmt     Or42d01
017800031009
017900031009      * F3=Fine
018000031009     c   kc              Leave
018100031010     c                   Eval      *In28 = *Off
018200031010     c                   Eval      *In90 = *Off
018300031009
018400031009      * controlli
018500031009     c                   ExSr      Sr_Contr
018600031010     c   28
018700031010     cor 90              Iter
018800031009
018900031009      * F6=Elabora
019000031010     c                   If        *Inkf
019100031010     c                   ExSr      Sr_Elabora
019200031010     c                   Leave
019300031009     c                   EndIf
019400031009
019500031009     c                   EndDo
019600031009
019700031009     c                   Eval      *InLr = *On
019800031009
019900031009      *---------------------------------------------------------------*
020000031009      * PULIZIA VIDEATA
020100031009      *---------------------------------------------------------------*
020200031009     c     Sr_Pulvid     BegSr
020300031009
020400031009     c                   Clear                   V1cDtda
020500031009     c                   Clear                   V1cDtal
020600031009     c                   Clear                   V1cTco
020700031118     c***                Eval      V1cTot  = 'NO'
020800031009     c                   Eval      V1cFile = 'N'
020900031010     c                   Clear                   ds_Cli1
021000031010     c                   Clear                   ds_Cli2
021100031010     c                   Clear                   ds_Cli3
021200031010     c                   Clear                   ds_Cli4
021300031010     c                   Clear                   ds_Cli5
021400031010     c                   Clear                   ds_Rag
021500031010     c                   Clear                   ds_All
021600031010     c                   Clear                   ds_Mit
021700031010     c                   Clear                   ds_Ord
021800031010     c                   Clear                   ds_Des
021900031009
022000031009     c                   EndSr
022100031009
022200031009      *---------------------------------------------------------------*
022300031009      * CONTROLLO I DATI DELLA VIDEATA
022400031009      *---------------------------------------------------------------*
022500031009     c     Sr_Contr      BegSr
022600031009
022700031009      * p.o. emissione
022800031009     c                   ExSr      Sr_ContrPoe
022900031009     c                   If        *In28
023000031009     c                   GoTo      EndContr
023100031009     c                   EndIf
023200031009
023300031009      * p.o. ritiro
023400031009     c                   ExSr      Sr_ContrPor
023500031009     c                   If        *In28
023600031009     c                   GoTo      EndContr
023700031009     c                   EndIf
023800031009
023900031009      * Controllo le combinazioni tra p.o. emissione e p.o. ritiro
024000031009     c                   ExSr      Sr_ContrPo
024100031009     c                   If        *In28
024200031009     c                   GoTo      EndContr
024300031009     c                   EndIf
024400031009
024500031009      * Controllo le date
024600031010     c                   ExSr      Sr_ContrDate
024700031009     c                   If        *In28
024800031009     c                   GoTo      EndContr
024900031009     c                   EndIf
025000031010
025100031010      * Controllo il tipo comunicazione
025200031010     c                   ExSr      Sr_ContrTco
025300031010     c                   If        *In28
025400031010     c                   GoTo      EndContr
025500031010     c                   EndIf
025600031009
025700031010      * Controllo i clienti
025800031010     c                   ExSr      Sr_ContrCli
025900031010     c                   If        *In28
026000031010     c                   GoTo      EndContr
026100031010     c                   EndIf
026200031010
026300031016      * Controllo il tipo cliente
026400031016     c                   ExSr      Sr_ContrtCli
026500031016     c                   If        *In28
026600031016     c                   GoTo      EndContr
026700031016     c                   EndIf
026800031016
026900031009     c     EndContr      EndSr
027000031009
027100031009      *---------------------------------------------------------------*
027200031009      * CONTROLLO IL P.O. EMISSIONE
027300031009      *---------------------------------------------------------------*
027400031009     c     Sr_ContrPoe   BegSr
027500031009
027600031009     c                   Eval      *In40 = *Off
027700031009     c                   Clear                   V1dPoe
027800031009
027900031009     c                   If        V1cPoe = 999
028000031009     c                   Clear                   V1dPoe
028100031010     c                   Eval      V1dPoe = 'Tutti i p.o.'
028200031009     c                   GoTo      EndContrPoe
028300031009     c                   EndIf
028400031009
028500031009      * non deve essere a zero
028600031009     c                   If        V1cPoe = *Zeros
028700031009     c                   Eval      *In40 = *On
028800031009     c                   Eval      *In28 = *On
028900031009     c                   Eval      V1cMsg = Msg(1)
029000031009     c                   GoTo      EndContrPoe
029100031009     c                   EndIf
029200031009
029300031009      * Controllo se p.o. valido
029400031009     c     V1cPoe        Chain     Azorg01L
029500031010     c                   If         Not %Found(Azorg01l) or
029600031009     c                              OrgFva <> *Blanks or
029700031009     c                             (OrgFag <> 'A' and OrgFag <> 'F')
029800031009     c                   Eval      *In40 = *On
029900031009     c                   Eval      *In28 = *On
030000031009     c                   Eval      V1cMsg = Msg(1)
030100031009     c                   GoTo      EndContrPoe
030200031009     c                   EndIf
030300031009
030400031009     c                   Movel     OrgDes        V1dPoe
030500031009
030600031009     c     EndContrPoe   EndSr
030700031009
030800031009      *---------------------------------------------------------------*
030900031009      * CONTROLLO IL P.O. RITIRO
031000031009      *---------------------------------------------------------------*
031100031009     c     Sr_ContrPor   BegSr
031200031009
031300031009     c                   Eval      *In41 = *Off
031400031009     c                   Clear                   V1dPor
031500031009
031600031009     c                   If        V1cPor = 999
031700031009     c                   Clear                   V1dPor
031800031010     c                   Eval      V1dPor = 'Tutti i p.o.'
031900031009     c                   GoTo      EndContrPor
032000031009     c                   EndIf
032100031009
032200031009      * non deve essere a zero
032300031009     c                   If        V1cPor = *Zeros
032400031009     c                   Eval      *In41 = *On
032500031009     c                   Eval      *In28 = *On
032600031009     c                   Eval      V1cMsg = Msg(1)
032700031009     c                   GoTo      EndContrPor
032800031009     c                   EndIf
032900031009
033000031009      * Controllo se p.o. valido
033100031009     c     V1cPor        Chain     Azorg01L
033200031010     c                   If         Not %Found(Azorg01l) or
033300031009     c                              OrgFva <> *Blanks or
033400031009     c                             (OrgFag <> 'A' and OrgFag <> 'F')
033500031009     c                   Eval      *In41 = *On
033600031009     c                   Eval      *In28 = *On
033700031009     c                   Eval      V1cMsg = Msg(1)
033800031009     c                   GoTo      EndContrPor
033900031009     c                   EndIf
034000031009
034100031009     c                   Movel     OrgDes        V1dPor
034200031009
034300031009     c     EndContrPor   EndSr
034400031009
034500031009      *---------------------------------------------------------------*
034600031009      * CONTROLLO LE COMBINAZIONI TRA P.O. EMISSIONE E P.O. RITIRO
034700031009      *---------------------------------------------------------------*
034800031009     c     Sr_ContrPo    BegSr
034900031009
035000031009     c                   Eval      *In41 = *Off
035100031009
035200031009     c                   Select
035300031009
035400031009      * sono in filiale e sono primo livello
035500031009     c                   When      Not *In12 and Not *In06
035600031010      * non posso richiedere tutti i p.o. come emissione e come ritiro
035700031010if  1c                   If        V1cPoe = 999 and V1cpor = 999
035800031010     c                   Eval      *In41 = *On
035900031010     c                   Eval      *In28 = *On
036000031010     c                   Eval      V1cMsg = Msg(2)
036100031010     c                   GoTo      EndContrPo
036200031010    1c                   EndIf
036300031010     c     V1cPoe        Lookup    L1                                     30
036400031010     c     V1cPor        Lookup    L1                                     31
036500031010      * non posso richiedere p.o. emissione e p.o. ritiro non del terminal
036600031010if  1c                   If        Not *In30 and Not *In31
036700031010     c                   Eval      *In41 = *On
036800031010     c                   Eval      *In28 = *On
036900031010     c                   Eval      V1cMsg = Msg(2)
037000031010     c                   GoTo      EndContrPo
037100031010    1c                   EndIf
037200031009
037300031009      * sono in filiale e sono secondo livello
037400031009     c                   When      Not *In12 and *In06
037500031009      * P.o. emissione non sono io
037600031010if  1c                   If        V1cPoe <> Dutpou and
037700031009      * come p.o. ritiro posso richiedere solo me stesso
037800031010     c                             V1cPor <> Dutpou
037900031009     c                   Eval      *In41 = *On
038000031009     c                   Eval      *In28 = *On
038100031009     c                   Eval      V1cMsg = Msg(3)
038200031009     c                   GoTo      EndContrPo
038300031009    1c                   EndIf
038400031009
038500031009     c                   EndSl
038600031009
038700031009     c     EndContrPo    EndSr
038800031009
038900031009      *---------------------------------------------------------------*
039000031009      * CONTROLLO LE DATE
039100031009      *---------------------------------------------------------------*
039200031009     c     Sr_ContrDate  BegSr
039300031009
039400031009     c                   Eval      *In42 = *Off
039500031009     c                   Eval      *In43 = *Off
039600031009
039700031009      * data 'da' deve essere impostata
039800031009     c                   If        V1cDtda = *Zeros
039900031009     c                   Eval      *In42 = *On
040000031009     c                   Eval      *In28 = *On
040100031009     c                   Eval      V1cMsg = Msg(4)
040200031009     c                   GoTo      EndContrDate
040300031009     c                   EndIf
040400031009
040500031009      * controllo la data inserita 'da'
040600031009     c                   Clear                   wcdtda
040700031009     c                   Clear                   wlbdat
040800031009     c                   Z-Add     V1cDtda       G02Dat
040900031009     c                   Call      'XSRDA8'
041000031009     c                   Parm                    wlbdat
041100031009     c                   If        G02Err = '1'
041200031009     c                   Eval      *In42 = *On
041300031009     c                   Eval      *In28 = *On
041400031009     c                   Eval      V1cMsg = Msg(5)
041500031009     c                   GoTo      EndContrDate
041600031009     c                   Else
041700031009     c                   Z-Add     G02Dat        V1cDtda
041800031009     c                   Z-Add     G02Inv        wcdtda
041900031009     c                   EndIf
042000031009
042100031009      * non può essere > ad oggi
042200031009     c                   If        wcdtda > dataoggi
042300031009     c                   Eval      *In42 = *On
042400031009     c                   Eval      *In28 = *On
042500031009     c                   Eval      V1cMsg = Msg(6)
042600031009     c                   GoTo      EndContrDate
042700031009     c                   EndIf
042800031009
042900031009      * deve essere > della data di pulizia ORM
043000031009     c                   If        wcdtda <= dtpuliaammgg
043100031009     c                   Eval      *In42 = *On
043200031009     c                   Eval      *In28 = *On
043300031009     c                   Eval      V1cMsg = Msg(7)
043400031009     c                   Movel     dtpuliggmmaa  datapul
043500031009     c                   Eval      datamsg = %trim (giorno + '/' +
043600031009     c                             mese + '/' + anno)
043700031009     c                   Eval      %subst(V1cMsg: 62: 10) = datamsg
043800031009     c                   GoTo      EndContrDate
043900031009     c                   EndIf
044000031009
044100031009      * controllo la data inserita 'al'
044200031010if  1c                   If        V1cDtal <> *Zeros
044300031009     c                   Clear                   wcdtal
044400031009     c                   Clear                   wlbdat
044500031009     c                   Z-Add     V1cDtal       G02Dat
044600031009     c                   Call      'XSRDA8'
044700031009     c                   Parm                    wlbdat
044800031010if  2c                   If        G02Err = '1'
044900031009     c                   Eval      *In43 = *On
045000031009     c                   Eval      *In28 = *On
045100031009     c                   Eval      V1cMsg = Msg(5)
045200031009     c                   GoTo      EndContrDate
045300031010   x2c                   Else
045400031009     c                   Z-Add     G02Dat        V1cDtal
045500031009     c                   Z-Add     G02Inv        wcdtal
045600031010    2c                   EndIf
045700031009
045800031009      * non può essere > ad oggi
045900031010if  2c                   if        wcdtal > dataoggi
046000031009     c                   Eval      *In43 = *On
046100031009     c                   Eval      *In28 = *On
046200031009     c                   Eval      V1cMsg = Msg(6)
046300031009     c                   GoTo      EndContrDate
046400031010    2c                   EndIf
046500031009
046600031009      * deve essere > della data di pulizia ORM
046700031010if  2c                   If        wcdtal <= dtpuliaammgg
046800031009     c                   Eval      *In43 = *On
046900031009     c                   Eval      *In28 = *On
047000031009     c                   Eval      V1cMsg = Msg(7)
047100031009     c                   Movel     dtpuliggmmaa  datapul
047200031009     c                   Eval      datamsg = %trim (giorno + '/' +
047300031009     c                             mese + '/' + anno)
047400031009     c                   Eval      %subst(V1cMsg: 62: 10) = datamsg
047500031009     c                   GoTo      EndContrDate
047600031010    2c                   EndIf
047700031009
047800031009      * range
047900031010if  2c                   If        wcdtda > wcdtal
048000031009     c                   Eval      *In42 = *On
048100031009     c                   Eval      *In28 = *On
048200031009     c                   Eval      V1cMsg = Msg(5)
048300031009     c                   GoTo      EndContrDate
048400031010    2c                   EndIf
048500031010    1c                   EndIf
048600031009
048700031009     c     EndContrDate  EndSr
048800031010
048900031010      *---------------------------------------------------------------*
049000031010      * CONTROLLO IL TIPO COMUNICAZIONE
049100031010      *---------------------------------------------------------------*
049200031010     c     Sr_ContrTco   BegSr
049300031010
049400031010     c                   Eval      *In44 = *Off
049500031010
049600031010      * Ricerca tipo comunicazione con "?"
049700031010     c     '?'           Scan      V1cTco                                 32
049800031010     c                   If        *In32
049900031010     c                   Eval      *In44 = *On
050000031010     c                   Eval      *In90 = *On
050100031010     c                   Clear                   Tibs02ds
050200031010     c                   Eval      T02Mod = 'R'
050300031010     c                   Eval      T02Sif = Knsif
050400031010     c                   Eval      T02Cod = 'TCO'
050500031010     c                   Call      'TIBS02R'
050600031010     c                   Parm                    Kpjba
050700031010     c                   Parm                    Tibs02ds
050800031010     c                   Movel     T02Ke1        V1cTco
050900031010     c                   Movel     T02Uni        V1dTco
051000031010     c                   GoTo      EndContrTco
051100031010     c                   EndIf
051200031010
051300031010      * Controllo
051400031010if  1c                   If        V1cTco <> *Blanks
051500031010     c                   Clear                   Tibs02ds
051600031010     c                   Eval      T02Mod = 'C'
051700031010     c                   Eval      T02Sif = Knsif
051800031010     c                   Eval      T02Cod = 'TCO'
051900031010     c                   Movel(p)  V1cTco        T02Ke1
052000031010     c                   Call      'TIBS02R'
052100031010     c                   Parm                    Kpjba
052200031010     c                   Parm                    Tibs02ds
052300031010if  2c                   If        T02Err <> *Blanks
052400031010     c                   Eval      V1cMsg = Msg(8)
052500031010     c                   Eval      *In44 = *On
052600031010     c                   Eval      *In28 = *On
052700031010     c                   GoTo      EndContrTco
052800031010    2c                   EndIf
052900031010     c                   Movel     T02Uni        V1dTco
053000031010    1c                   EndIf
053100031010
053200031010     c     EndContrTco   EndSr
053300031009
053400031010      *---------------------------------------------------------------*
053500031010      * CONTROLLO I CLIENTI
053600031010      *---------------------------------------------------------------*
053700031010     c     Sr_ContrCli   BegSr
053800031016
053900031016     c                   Clear                   ds_rag
054000031010
054100031010     c                   Do        5             xx
054200031010     c                   Eval      yy = (44 + xx)
054300031010     c                   Eval      *In(yy) = *Off
054400031010     c                   EndDo
054500031010
054600031010     c                   Move      ds_cli1       cli(1)
054700031010     c                   Move      ds_cli2       cli(2)
054800031010     c                   Move      ds_cli3       cli(3)
054900031010     c                   Move      ds_cli4       cli(4)
055000031010     c                   Move      ds_cli5       cli(5)
055100031010
055200031010     c                   Movea     ds_rag        rag
055300031010     c                   Movea     ds_all        all
055400031010
055500031015      * Almeno un cliente deve essere inserito
055600031015     c                   If        cli(1) = *Blanks and cli(2) = *Blanks and
055700031015     c                             cli(3) = *Blanks and cli(4) = *Blanks and
055800031015     c                             cli(5) = *Blanks
055900031015     c                   Eval      *In45 = *On
056000031016     c                   Eval      V1cMsg = Msg(10)
056100031015     c                   Eval      *In28 = *On
056200031015     c                   GoTo      EndContrCli
056300031015     c                   EndIf
056400031010
056500031010do  1c                   Do        5             xx
056600031010     c                   Eval      yy = (44 + xx)
056700031016if  2c                   If        cli(xx) <> *Blanks
056800031010      * Ricerca codice cliente
056900031010     c     '?'           Scan      cli(xx)                                32
057000031016if  3c                   If        *In32
057100031010     c                   Eval      *In(yy) = *On
057200031010     c                   Eval      *In90 = *On
057300131216     c                   clear                   FIOR81ds
057400131216     c  N12              movel     DUTpou        I81fil
057500131216     c                   movel(p)  FIOR81ds      KPJBU
057600131216     c                   call      'FIOR81R'
057700070809     c                   parm                    KPJBA
057800131216     c                   parm                    FIOR81ds
057900070809sel 4c                   select
058000131216w   4c                   when      O81err <> *blanks
058100131216     c                   eval      V1Cmsg  = O81msg
058200070809     c                   eval      *in(yy) = *on
058300070809     c                   eval      *in28   = *on
058400131216w   4c                   when      O81cro <> *zeros
058500131216     c                   movel     O81cro        Cli(xx)
058600070809     c                   move      Cli(xx)       kACR
058700070809     c                   exsr      sr_DecoCli
058800070809e   4c                   endsl
058900031010
059000031010      * Decodifico e controllo
059100031016   x3c                   Else
059200031010     c                   Move      cli(xx)       kAcr
059300031010     c                   ExSr      Sr_DecoCli
059400031010     c   28              Leave
059500031016    3c                   EndIf
059600031016    2c                   EndIf
059700031010    1c                   EndDo
059800031010
059900031010     c                   Move      cli(1)        ds_cli1
060000031010     c                   Move      cli(2)        ds_cli2
060100031010     c                   Move      cli(3)        ds_cli3
060200031010     c                   Move      cli(4)        ds_cli4
060300031010     c                   Move      cli(5)        ds_cli5
060400031010
060500031010     c                   Movea     rag           ds_rag
060600031010     c                   Movea     all           ds_all
060700031010
060800031015     c     EndContrCli   EndSr
060900031016
061000031016      *---------------------------------------------------------------*
061100031016      * CONTROLLO IL TIPO CLIENTE
061200031016      *---------------------------------------------------------------*
061300031016     c     Sr_ContrtCli  BegSr
061400031016
061500031016     c                   Do        5             xx
061600031016     c                   Eval      yy = (49 + xx)
061700031016     c                   Eval      *In(yy) = *Off
061800031016     c                   EndDo
061900031016
062000031016     c                   Movea     ds_mit        mit
062100031016     c                   Movea     ds_ord        ord
062200031016     c                   Movea     ds_des        des
062300031016
062400031016do  1c                   Do        5             xx
062500031016     c                   Eval      yy = (49 + xx)
062600031016if  2c                   If        cli(xx) <> *Blanks
062700031016      * devo richiedere il tipo cliente se il codice è inserito
062800031016if  3c                   If        mit(xx) = *Blanks and ord(xx) = *Blanks and
062900031016     c                             des(xx) = *Blanks
063000031016     c                   Eval      *In(yy) = *On
063100031016     c                   Eval      *In28 = *On
063200031016     c                   Eval      V1cMsg = Msg(11)
063300031016     c                   Leave
063400031016    3c                   EndIf
063500031016    2c                   EndIf
063600031016    1c                   EndDo
063700031016
063800031016     c                   Movea     mit           ds_mit
063900031016     c                   Movea     ord           ds_ord
064000031016     c                   Movea     des           ds_des
064100031016
064200031016     c                   EndSr
064300031010
064400031010      *---------------------------------------------------------------*
064500031010      * DECODIFICA CLIENTI
064600031010      *---------------------------------------------------------------*
064700031010     c     Sr_DecoCli    BegSr
064800031010
064900031010      * se richiesto tutti i luogi cerco se almeno 1 esiste
065000031016if  1c                   If        all(xx) = 'S'
065100031010     c                   Move      000           kAcr
065200031010     c     kAcr          Setll     Fnacr01l
065300031016do  2c                   Do        *Hival
065400031010     c                   Read      Fnacr01l
065500031010     c                   If        %Eof(Fnacr01l)
065600031010     c                   Leave
065700031010     c                   EndIf
065800031016     c                   Movel     AcrRsr        rag(xx)
065900031010     c                   Leave
066000031016    2c                   EndDo
066100031016     c                   If        rag(xx) = *Blanks
066200031010     c                   Eval      V1cMsg = Msg(9)
066300031016     c                   Eval      *In(yy) = *On
066400031010     c                   Eval      *In28 = *On
066500031010     c                   GoTo      EndDecoCli
066600031010     c                   EndIf
066700031010      * se no cerco il cliente specifico
066800031016   x1c                   Else
066900031010     c     kAcr          Chain     Fnacr01l
067000031010     c                   If        Not %Found(Fnacr01l) or
067100031010     c                             AcrAtb <> *blanks
067200031010     c                   Eval      V1cMsg = Msg(9)
067300031016     c                   Eval      *In(yy) = *On
067400031010     c                   Eval      *In28 = *On
067500031010     c                   GoTo      EndDecoCli
067600031010     c                   EndIf
067700031016     c                   Movel     AcrRsr        rag(xx)
067800031010    1c                   EndIf
067900031010
068000031010     c     EndDecoCli    EndSr
068100031010
068200031009      *---------------------------------------------------------------*
068300031009      * ELABORA
068400031009      *---------------------------------------------------------------*
068500031009     c     Sr_Elabora    BegSr
068600010606
068700031009     c                   Eval      i42Poe = V1cPoe
068800031009     c                   Eval      i42Por = V1cPor
068900031009     c                   Eval      i42Dtda = wcdtda
069000031010      * data 'al' se non impostata è = a data 'da'
069100031010     c                   If        wcdtal = *zeros
069200031010     c                   Eval      wcdtal = wcdtda
069300031010     c                   EndIf
069400031009     c                   Eval      i42Dtal = wcdtal
069500031010     c                   Eval      i42Tco = V1cTco
069600031118     c***                Eval      i42Tot = V1cTot
069700031009     c                   Eval      i42File = V1cFile
069800031010     c                   Move      ds_cli1       i42cli1
069900031010     c                   Move      ds_cli2       i42cli2
070000031010     c                   Move      ds_cli3       i42cli3
070100031010     c                   Move      ds_cli4       i42cli4
070200031010     c                   Move      ds_cli5       i42cli5
070300031010     c                   Eval      i42all1 = V1cAll1
070400031010     c                   Eval      i42all2 = V1cAll2
070500031010     c                   Eval      i42all3 = V1cAll3
070600031010     c                   Eval      i42all4 = V1cAll4
070700031010     c                   Eval      i42all5 = V1cAll5
070800031010     c                   Eval      i42mit1 = V1cmit1
070900031010     c                   Eval      i42mit2 = V1cmit2
071000031010     c                   Eval      i42mit3 = V1cmit3
071100031010     c                   Eval      i42mit4 = V1cmit4
071200031010     c                   Eval      i42mit5 = V1cmit5
071300031010     c                   Eval      i42ord1 = V1cord1
071400031010     c                   Eval      i42ord2 = V1cord2
071500031010     c                   Eval      i42ord3 = V1cord3
071600031010     c                   Eval      i42ord4 = V1cord4
071700031010     c                   Eval      i42ord5 = V1cord5
071800031010     c                   Eval      i42des1 = V1cdes1
071900031010     c                   Eval      i42des2 = V1cdes2
072000031010     c                   Eval      i42des3 = V1cdes3
072100031010     c                   Eval      i42des4 = V1cdes4
072200031010     c                   Eval      i42des5 = V1cdes5
072300010606
072400031014     c                   clear                   kpjbu
072500031014     c                   movel     fior42ds      kpjbu
072600031014
072700031014     c                   Move      'OR43'        Kcoaz
072800031014     c                   If        *In12
072900031014     c                   Call      'BCH09'
073000031014     c                   Parm                    Kpjba
073100031014     c                   EndIf
073200031014     c                   Call      'BCH10'
073300031014     c                   Parm                    Kpjba
073400010606
073500010618     c                   endsr
073600031009      *---------------------------------------------------------------*
073700031009      * SUBROUTINE INIZIALE                                           *
073800031009      *---------------------------------------------------------------*
073900031009     c     *Inzsr        BegSr
074000001009
074100031009     c     *Entry        Plist
074200031009     c                   Parm                    Kpjba
074300010618
074400031009     c     *dtaara       define    §azute        azuteds
074500031009     c     *dtaara       define    §datiute      ddatiute
074600031009     c                   in(E)     *dtaara
074700031009     c                   IF        %error  or RSUT = *blanks
074800031009     c                   CLEAR                   tibs34ds
074900031009     c                   CALL      'TIBS34R'
075000031009     c                   PARM                    tibs34ds
075100031009     c                   in        *dtaara
075200031009     c                   endif
075300031009
075400031009     c                   Eval      VtcTit = Titolo
075500031009     c                   Clear                   fior42ds
075600031009     c                   Clear                   V1cPoe
075700031009     c                   Clear                   V1dPoe
075800031009     c                   Clear                   V1cPor
075900031009     c                   Clear                   V1dPor
076000031009
076100031009      * Se elaborazione da sede imposto p.o. ritiro e p.o. emissione a 999
076200031009if  1c                   If        Simfel = *Zeros
076300031009     c                   Eval      *In12 = *On
076400031009     c                   Eval      V1cPoe = 999
076500031010     c                   Eval      V1dPoe = 'Tutti i p.o.'
076600031009     c                   Eval      V1cPor = 999
076700031010     c                   Eval      V1dPor = 'Tutti i p.o.'
076800031009   x1c                   Else
076900031009      * da filiale imposto il p.o. da elaborare
077000031010if  2c                   If        Dutlpo = '2' or Dutlpo = *Blanks
077100031009      * secondo livello
077200031010     c                   Eval      V1cPoe = Dutpou
077300031009     c                   Eval      V1cPor = 999
077400031010     c                   Eval      V1dPor = 'Tutti i p.o.'
077500031009     c                   Eval      *In06 = *On
077600031009   x2c                   Else
077700031009      * primo livello
077800031009     c                   Eval      V1cPoe = Simfel
077900031009     c                   Eval      V1cPor = 999
078000031010     c                   Eval      V1dPor = 'Tutti i p.o.'
078100031009    2c                   EndIf
078200031009
078300031009      * Decodifico il p.o. emissione impostato
078400031009     c     V1cPoe        Chain     Azorg01L
078500031010if  2c                   If        %Found(Azorg01l)
078600031009     c                   Movel     OrgDes        V1dPoe
078700031009    2c                   EndIf
078800031009      * Carico i p.o. del terminal di partenza
078900031009     c                   Clear                   Trul06Ds
079000031009     c                   Eval      D06Cod = '£1'
079100031009     c                   Movel     Simfel        D06Key
079200031009     c                   Movel(p)  Trul06ds      Kpjbu
079300031009     c                   Call      'TRUL06R'
079400031009     c                   Parm                    Kpjba
079500031009     c                   Movel     Kpjbu         Trul06ds
079600031009    1c                   EndIf
079700001009
079800031009      * reperisce data e ora
079900031009     c                   Time                    w0140
080000031009      * Udate in GGMMAAAA
080100031009     c                   Move      w0140         wdtgio
080200031009      * Udate in AAAAMMGG
080300031009     c                   Z-Add     wdtgio        G02Dat
080400031009     c                   Movel     *Blanks       G02Err
080500031009     c                   Call      'XSRDA8'
080600031009     c                   Parm                    wlbdat
080700031009     c                   Z-Add     G02Inv        dataoggi
080800031009     c                   z-add     g02tgi        sav_g02tgi
080900031009
081000031009      * cerco data limite lancio elaborazione
081100031009     c                   Clear                   ds5a2
081200031009     c                   Movel     '5A'          kCod
081300031009     c                   Movel     '2       '    kKey
081400031009     c     kTab          Chain     Tabel00f
081500031009     c                   if        %Found(Tabel00f) and TblFlg = *Blanks
081600031009     c                   Movel     TblUni        ds5a2
081700031009     c                   Z-Add     §5aorf        w0030
081800031009     c                   EndIf
081900010618
082000010618      * Calcolo data
082100031009     c     sav_g02tgi    Sub       w0030         giotgi
082200031009     c                   Call      'XSRGI8'
082300031009     c                   Parm                    wgidat
082400031009     c                   Z-Add     giodat        dtpuliggmmaa
082500010618
082600010618      * Giro la data
082700031009     c                   If        dtpuliggmmaa > *Zeros
082800031009     c                   Move      dtpuliggmmaa  G02Dat
082900031009     c                   Movel     *Blanks       G02Err
083000031009     c                   Call      'XSRDA8'
083100031009     c                   Parm                    wlbdat
083200031009     c                   If        G02Err = '1'
083300031009     c                   Clear                   dtpuliggmmaa
083400031009     c                   Else
083500031009     c                   Z-Add     G02Inv        dtpuliaammgg
083600031009     c                   EndIf
083700031009     c                   EndIf
083800031009
083900031009     c     kTab          Klist
084000031009     c                   Kfld                    Codut
084100031009     c                   Kfld                    kCod
084200031009     c                   Kfld                    kKey
084300010618
084400031009     c                   EndSr
084500001010      *---------------------------------------------------------------------------------------------
084600001031** MSG  Lungh. 78                                                            *
084700070418Filiale errata                                                                      1
084800070418Immettere una filiale del terminal                                                  2
084900070418Immettere la filiale utente                                                         3
085000031009Inserire la data immissione                                                         4
085100031009Data immissione errata                                                              5
085200031009Data immissione non può essere superiore alla data odierna                          6
085300031009Data immissione deve essere maggiore alla data pulizia ORM                          7
085400031010Tipo comunicazione errato                                                           8
085500031010Cliente errato                                                                      9
085600031016Immettere il codice cliente                                                         10
085700031016Se immesso il codice cliente immettere anche il tipo cliente                        11
