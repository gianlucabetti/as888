000100031216     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200031216
000300031216      *------------------------------------------------------------------------*
000400031216      *            CONTROLLO/AGGIORNAMENTO ORM LEGATO A BOLLA RC
000500031216      *------------------------------------------------------------------------*
000600031216
000700020212      * INPUT
000800020212      * -----
000900031217      *  I24TLA   Tipo di lancio
001000031217      *  I24OPE   Operazione richiesta
001100031217      *  I24AAS   Anno spedizione
001200031217      *  I24SPE   Spedizione
001300031217      *  I24LNA   Linea arrivo
001400031217      *  I24ZNC   Zona consegna
001500040227      *  I24NAAS  Anno spedizione NEW
001600040227      *  I24LNP   Linea partenza  NEW
001700040227      *  I24NRS   Serie           NEW
001800040227      *  I24NSP   n. Spedizione   NEW
001900031216
002000031216      * OUTPUT
002100031216      * ------
002200031217      *  O24ERR   Codice di errore:
002300031216      *           00 = Nessun errore
002400031216      *           01 = Non trovato ORM
002500031216      *           02 = Record del file FNORM00F allocato
002600031217      *           03 = ORM non aggiornato non in fase 100
002700031217      *           04 = ORM non aggiornato manca la linea di arrivo
002800031217      *  O24MSG   Messaggio di errore
002900031217      *  O24POE   P.o. emissione ORM
003000031217      *  O24NSR   Serie
003100031217      *  O24NOR   Numero ORM
003200031217      *  O24NRV   Viaggio
003300031216
003400031216      *------------------------------------------------------------------------*
003500020212
003600031216     fFnorm09l  if   e           k Disk
003700031216     fFnorm01l  uf   e           k Disk    Usropn
003800031216     f                                     Rename(Fnorm000:Fnorm01)
003900031216
004000031217      *------------------------------------------------------------------------*
004100031217      *   C A M P I   D I   L A V O R O
004200031217      *------------------------------------------------------------------------*
004300031217
004400031217     d kOrmCor         s                   Like(OrmCor)
004500031217     d kOrmRfa         s                   Like(OrmRfa)
004600040227
004700040227     d wagg            s              1    Inz(*Off)
004800031217
004900031216      *------------------------------------------------------------------------*
005000031216      *   S C H I E R E
005100031216      *------------------------------------------------------------------------*
005200031216
005300031216      * Messaggi di errore
005400031217     d msg             s             78    Dim(05) Ctdata Perrcd(1)
005500031216
005600031216      *------------------------------------------------------------------------*
005700031216      *   D S   I N T E R N E / E S T E R N E
005800031216      *------------------------------------------------------------------------*
005900031216
006000040227     d  OrmRfa         ds
006100040227     d  wrmbolla               1      1
006200040227     d  wrmaas                 2      3
006300040227     d  wrmlnp                 4      6
006400040227     d  wrmnrs                 7      8
006500040227     d  wrmnsp                 9     15
006600040227
006700020212      * Passaggio Parametri
006800031217     d Fnls24ds      e ds
006900031216
007000031216     d Kpjba         e ds
007100031216
007200031216      *------------------------------------------------------------------------*
007300031216      * RIEPILOGO INDICATORI
007400031216      *------------------------------------------------------------------------*
007500031216
007600031216
007700031216      *------------------------------------------------------------------------*
007800031216
007900031216     c     *Entry        Plist
008000031217     c                   Parm                    Fnls24ds
008100031216
008200031217      * Controllo il tipo lancio
008300031217if  1c                   If        I24Tla <> *Blanks and I24Tla <> 'C' and
008400031217     c                             I24Tla <> 'L'
008500031217     c                   Clear                   I24Tla
008600031217    1c                   EndIf
008700031217
008800031217sel 1c                   Select
008900031217
009000031217      * Elaboro
009100031217w   1c                   When      I24tla <> 'C'
009200031217      * Controllo esistenza dell'ORM
009300031217if  2c                   If        I24Ope = 'C'
009400031217     c                   ExSr      Sr_CtrOrm
009500031217    2c                   EndIf
009600031217      * Aggiorno ORM
009700031217if  2c                   If        I24Ope = 'A'
009800031217     c                   ExSr      Sr_AggOrm
009900031217    2c                   EndIf
010000051216      * Controllo esistenza dell'ORM x accorpamento bolle
010100051216if  2c                   If        I24Ope = 'L'
010200051216     c                   ExSr      Sr_CtrOrm1
010300051216    2c                   EndIf
010400051216      * Aggiorno ORM per accorpamento bolle
010500051216if  2c                   If        I24Ope = 'U'
010600051216     c                   ExSr      Sr_AggOrm1
010700051216    2c                   EndIf
010800031217
010900031217      * Chiusura archivi
011000031217w   1c                   When      I24Tla = 'C'
011100031217     c                   ExSr      Sr_CloOrm
011200031217    1c                   EndSl
011300031217
011400031217      * Fine
011500031217     c                   ExSr      Sr_EndPgm
011600031217
011700031217      *------------------------------------------------------------------------*
011800031217      * Controllo esistenza ORM
011900031217      *------------------------------------------------------------------------*
012000031217     c     Sr_CtrOrm     BegSr
012100031217
012200031217      * pulisco i campi di output
012300031217     c                   Clear                   O24Err
012400031217     c                   Clear                   O24Msg
012500031217     c                   Clear                   O24Poe
012600031217     c                   Clear                   O24Nsr
012700031217     c                   Clear                   O24Nor
012800031217     c                   Clear                   O24Nrv
012900031217
013000031217      * preparo la chiave
013100031217     c                   Clear                   kOrmCor
013200031217     c                   Clear                   kOrmRfa
013300031217     c                   Movel     I24Ksc        kOrmCor
013400031217     c                   Move      '333'         kOrmCor
013500031217
013600031217     c                   Eval      %subst(kOrmRfa:1:1)  = 'B'
013700031217     c                   Eval      %subst(kOrmRfa:2:2)  = I24Aas
013800031217     c                   Eval      %subst(kOrmRfa:4:12) = I24Spe
013900031217
014000031217      * cerco l'ORM
014100031217     c     kFnorm9       Chain     Fnorm09l
014200031217      * trovo l'orm imposto i campi di output
014300031217if  1c                   If        %Found(Fnorm09l)
014400031217     c                   Eval      O24Poe = OrmPoe
014500031217     c                   Eval      O24Nsr = OrmNsr
014600031217     c                   Eval      O24Nor = OrmNor
014700031217     c                   Eval      O24Nrv = OrmNrv
014800031217
014900031217      * se p.o. ritiro non impostato faccio altri controlli
015000031217if  2c                   If        Ormpor = *Zeros
015100031217     c                   ExSr      Sr_CtrPor
015200031217    2c                   EndIf
015300031217      * non trovo l'orm errore
015400031217   x1c                   Else
015500031217     c                   Eval      O24Err = 01
015600031217    1c                   EndIf
015700031217
015800031217     c                   EndSr
015900031217
016000031217      *------------------------------------------------------------------------*
016100031217      * Aggiorno l'ORM
016200031217      *------------------------------------------------------------------------*
016300031217     c     Sr_AggOrm     BegSr
016400031217
016500040227     c                   Eval      wagg = *Off
016600040227
016700031217      * Apro i file se non l'ho già fatto
016800031217if  1c                   If        Not %Open(Fnorm01l)
016900031217     c                   Open      Fnorm01l
017000031217    1c                   EndIf
017100031217
017200031217      * Aggancio l'ORM
017300040227     c     kFnorm1       Chain     Fnorm01l
017400040227if  1c                   If        %Found(Fnorm01l)
017500040227      * cambio il p.o. ritiro
017600040227if  2c                   If        OrmPor = *Zeros
017700031217     c                   Eval      OrmPor = I24Lna
017800031217     c                   Move      I24Znc        OrmZor
017900040227     c                   Eval      wagg = *On
018000040227    2c                   EndIf
018100040227      * cambio il riferimento
018200040227if  2c                   If        I24NSP <> *Zeros
018300051216     c                   Move      I24nAas       wrmaas
018400040227     c                   Move      I24Lnp        wrmlnp
018500040227     c                   Move      I24Nrs        wrmnrs
018600040227     c                   Move      I24Nsp        wrmnsp
018700040227     c                   Eval      wagg = *On
018800040227    2c                   EndIf
018900040227      * aggiorno ORM
019000040227if  2c                   If        Wagg = *On
019100040909     c                   Z-add     *Date         OrmDtt
019200031217     c                   Update    Fnorm01
019300040227      * non devo aggiornare disalloco ORM
019400040227     c                   Else
019500040227     c                   Unlock    Fnorm01l
019600040227    2c                   EndIf
019700031217
019800031217    1c                   EndIf
019900031217
020000031217     c                   EndSr
020100051216
020200051216      *------------------------------------------------------------------------*
020300051216      * Controllo esistenza ORM per accorpamento bolle
020400051216      *------------------------------------------------------------------------*
020500051216     c     Sr_CtrOrm1    BegSr
020600051216
020700051216      * pulisco i campi di output
020800051216     c                   Clear                   O24Err
020900051216     c                   Clear                   O24Msg
021000051216     c                   Clear                   O24Poe
021100051216     c                   Clear                   O24Nsr
021200051216     c                   Clear                   O24Nor
021300051216     c                   Clear                   O24Nrv
021400051216
021500051216      * preparo la chiave
021600051216     c                   Clear                   kOrmCor
021700051216     c                   Clear                   kOrmRfa
021800051216     c                   Movel     I24Ksc        kOrmCor
021900051216     c                   Move      '333'         kOrmCor
022000051216
022100051216     c                   Eval      %subst(kOrmRfa:1:1)  = 'B'
022200051216     c                   Eval      %subst(kOrmRfa:2:2)  = I24Aas
022300051216     c                   Eval      %subst(kOrmRfa:4:12) = I24Spe
022400051216
022500051216      * cerco l'ORM
022600051216     c     kFnorm9       Chain     Fnorm09l
022700051216      * trovo l'orm imposto i campi di output
022800051216if  1c                   If        %Found(Fnorm09l)
022900051216     c                   Eval      O24Poe = OrmPoe
023000051216     c                   Eval      O24Nsr = OrmNsr
023100051216     c                   Eval      O24Nor = OrmNor
023200051216     c                   Eval      O24Nrv = OrmNrv
023300051216      * controllo l'orm
023400051216
023500051216      * Apro il file se non l'ho già fatto
023600051216if  1c                   If        Not %Open(Fnorm01l)
023700051216     c                   Open      Fnorm01l
023800051216    1c                   EndIf
023900051216
024000051216      * Aggancio l'ORM
024100051216     c     kFnorm1       Chain(e)  Fnorm01l
024200051216
024300051216sel 1c                   Select
024400051216      * Orm allocato
024500051216w   1c                   When      %Error
024600051216     c                   Eval      O24Err = 02
024700051216      * deve essere in fase 100
024800051216w   1c                   When      OrmFao <> 100
024900051216     c                   Eval      O24Err = 03
025000051216    1c                   EndSl
025100051216
025200051216     c                   Unlock    Fnorm01l
025300051216
025400051216      * non trovo l'orm errore
025500051216   x1c                   Else
025600051216     c                   Eval      O24Err = 01
025700051216    1c                   EndIf
025800051216
025900051216     c                   EndSr
026000051216
026100051216      *------------------------------------------------------------------------*
026200051216      * Aggiorno l'ORM per accorpamento bolle
026300051216      *------------------------------------------------------------------------*
026400051216     c     Sr_AggOrm1    BegSr
026500051216
026600051216     c                   Eval      wagg = *Off
026700051216
026800051216      * Apro i file se non l'ho già fatto
026900051216if  1c                   If        Not %Open(Fnorm01l)
027000051216     c                   Open      Fnorm01l
027100051216    1c                   EndIf
027200051216
027300051216      * Aggancio l'ORM
027400051216     c     kFnorm1       Chain     Fnorm01l
027500051216if  1c                   If        %Found(Fnorm01l)
027600051216      * cambio il riferimento
027700051216if  2c                   If        I24NSP <> *Zeros
027800051219     c                   Move      I24nAas       wrmaas
027900051216     c                   Move      I24Lnp        wrmlnp
028000051216     c                   Move      I24Nrs        wrmnrs
028100051216     c                   Move      I24Nsp        wrmnsp
028200051216     c                   Eval      wagg = *On
028300051216    2c                   EndIf
028400051216      * aggiorno ORM
028500051216if  2c                   If        Wagg = *On
028600051216     c                   Z-add     *Date         OrmDtt
028700051216     c                   Update    Fnorm01
028800051216      * non devo aggiornare disalloco ORM
028900051216     c                   Else
029000051216     c                   Unlock    Fnorm01l
029100051216    2c                   EndIf
029200051216
029300051216    1c                   EndIf
029400051216
029500051216     c                   EndSr
029600020212
029700031217      *------------------------------------------------------------------------*
029800031217      * Chiusura dei file aperti
029900031217      *------------------------------------------------------------------------*
030000031217     c     Sr_CloOrm     BegSr
030100031217
030200031217if  1c                   If        %Open(Fnorm01l)
030300031217     c                   Close     Fnorm01l
030400031217    1c                   EndIf
030500031217
030600031217     c                   EndSr
030700031217
030800031217      *------------------------------------------------------------------------*
030900031217      * Fine programma
031000031217      *------------------------------------------------------------------------*
031100031217     c     Sr_EndPgm     BegSr
031200031217
031300031217      * restituzione dei dati
031400031217sel 1c                   Select
031500031217
031600031217w   1c                   When      O24Err = *Zeros and I24Tla <> 'C'
031700031217     c                   Eval      O24Poe = OrmPoe
031800031217     c                   Eval      O24Nsr = OrmNsr
031900031217     c                   Eval      O24Nor = OrmNor
032000031217     c                   Eval      O24Nrv = OrmNrv
032100031217
032200031217w   1c                   When      O24Err > *Zeros
032300031217     c                   Eval      O24Msg = msg(O24Err)
032400031217
032500031217    1c                   EndSl
032600031217
032700031217if  1c                   If        I24Tla = *Blanks
032800031217     c                   Eval      *InRt = *On
032900031217   x1c                   Else
033000031217     c                   Eval      *InLr = *On
033100031217    1c                   EndIf
033200031217
033300031217     c                   Return
033400031217
033500031217     c                   EndSr
033600031216
033700031217      *------------------------------------------------------------------------*
033800031217      * Altri controlli per p.o. ritiro
033900031217      *------------------------------------------------------------------------*
034000031217     c     Sr_CtrPor     BegSr
034100031217
034200031217      * Apro il file se non l'ho già fatto
034300031217if  1c                   If        Not %Open(Fnorm01l)
034400031217     c                   Open      Fnorm01l
034500031217    1c                   EndIf
034600031217
034700031217      * Aggancio l'ORM
034800031217     c     kFnorm1       Chain(e)  Fnorm01l
034900031217
035000031217sel 1c                   Select
035100031217      * Orm allocato
035200031217w   1c                   When      %Error
035300031217     c                   Eval      O24Err = 02
035400031217      * deve essere in fase 100
035500031217w   1c                   When      OrmFao <> 100
035600031217     c                   Eval      O24Err = 03
035700031217      * deve esserci la linea di arrivo
035800031217w   1c                   When      I24Lna = *Zeros
035900031217     c                   Eval      O24Err = 04
036000031217    1c                   EndSl
036100031217
036200031217     c                   Unlock    Fnorm01l
036300031217
036400031217     c                   EndSr
036500031217
036600031216      *------------------------------------------------------------------------*
036700031216      * Operazioni iniziali
036800031216      *------------------------------------------------------------------------*
036900031216     c     *Inzsr        BegSr
037000031216
037100031217      * Klist
037200031217
037300031217     c     kFnOrm9       Klist
037400031217     c                   Kfld                    kOrmCor
037500031217     c                   Kfld                    kOrmRfa
037600031217
037700031217     c     kFnOrm1       Klist
037800031217     c                   Kfld                    O24Poe
037900031217     c                   Kfld                    O24Nsr
038000031217     c                   Kfld                    O24Nor
038100031217     c                   Kfld                    O24Nrv
038200031216
038300031216     c                   EndSr
038400031216
038500031217** Msg                                                                       *
038600031217FNLS24R - ORM non trovato                                                     01
038700031217FNLS24R - Record del file FNORM00F allocato                                   02
038800031217FNLS24R - ORM non in fase 100                                                 03
038900031217FNLS24R - Manca la linea di arrivo                                            04
