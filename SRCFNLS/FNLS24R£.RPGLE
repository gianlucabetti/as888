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
004800060405     d worm            s              1    inz(*off)
004900031217
005000031216      *------------------------------------------------------------------------*
005100031216      *   S C H I E R E
005200031216      *------------------------------------------------------------------------*
005300031216
005400031216      * Messaggi di errore
005500031217     d msg             s             78    Dim(05) Ctdata Perrcd(1)
005600031216
005700031216      *------------------------------------------------------------------------*
005800031216      *   D S   I N T E R N E / E S T E R N E
005900031216      *------------------------------------------------------------------------*
006000031216
006100040227     d  OrmRfa         ds
006200040227     d  wrmbolla               1      1
006300040227     d  wrmaas                 2      3
006400040227     d  wrmlnp                 4      6
006500040227     d  wrmnrs                 7      8
006600040227     d  wrmnsp                 9     15
006700040227
006800020212      * Passaggio Parametri
006900031217     d Fnls24ds      e ds
007000031216
007100031216     d Kpjba         e ds
007200031216
007300031216      *------------------------------------------------------------------------*
007400031216      * RIEPILOGO INDICATORI
007500031216      *------------------------------------------------------------------------*
007600031216
007700031216
007800031216      *------------------------------------------------------------------------*
007900031216
008000031216     c     *Entry        Plist
008100031217     c                   Parm                    Fnls24ds
008200031216
008300031217      * Controllo il tipo lancio
008400031217if  1c                   If        I24Tla <> *Blanks and I24Tla <> 'C' and
008500031217     c                             I24Tla <> 'L'
008600031217     c                   Clear                   I24Tla
008700031217    1c                   EndIf
008800031217
008900031217sel 1c                   Select
009000031217
009100031217      * Elaboro
009200031217w   1c                   When      I24tla <> 'C'
009300031217      * Controllo esistenza dell'ORM
009400031217if  2c                   If        I24Ope = 'C'
009500031217     c                   ExSr      Sr_CtrOrm
009600031217    2c                   EndIf
009700031217      * Aggiorno ORM
009800031217if  2c                   If        I24Ope = 'A'
009900031217     c                   ExSr      Sr_AggOrm
010000031217    2c                   EndIf
010100051216      * Controllo esistenza dell'ORM x accorpamento bolle
010200051216if  2c                   If        I24Ope = 'L'
010300051216     c                   ExSr      Sr_CtrOrm1
010400051216    2c                   EndIf
010500051216      * Aggiorno ORM per accorpamento bolle
010600051216if  2c                   If        I24Ope = 'U'
010700051216     c                   ExSr      Sr_AggOrm1
010800051216    2c                   EndIf
010900031217
011000031217      * Chiusura archivi
011100031217w   1c                   When      I24Tla = 'C'
011200031217     c                   ExSr      Sr_CloOrm
011300031217    1c                   EndSl
011400031217
011500031217      * Fine
011600031217     c                   ExSr      Sr_EndPgm
011700031217
011800031217      *------------------------------------------------------------------------*
011900031217      * Controllo esistenza ORM
012000031217      *------------------------------------------------------------------------*
012100031217     c     Sr_CtrOrm     BegSr
012200031217
012300031217      * pulisco i campi di output
012400031217     c                   Clear                   O24Err
012500031217     c                   Clear                   O24Msg
012600031217     c                   Clear                   O24Poe
012700031217     c                   Clear                   O24Nsr
012800031217     c                   Clear                   O24Nor
012900031217     c                   Clear                   O24Nrv
013000031217
013100031217      * preparo la chiave
013200031217     c                   Clear                   kOrmCor
013300031217     c                   Clear                   kOrmRfa
013400031217     c                   Movel     I24Ksc        kOrmCor
013500031217     c                   Move      '333'         kOrmCor
013600031217
013700031217     c                   Eval      %subst(kOrmRfa:1:1)  = 'B'
013800031217     c                   Eval      %subst(kOrmRfa:2:2)  = I24Aas
013900031217     c                   Eval      %subst(kOrmRfa:4:12) = I24Spe
014000031217
014100031217      * cerco l'ORM
014200060405      * non cerco più con chain diretta xchè è capitato di trovare orm già
014300060405      * chiusi con lo stesso riferimento della VAB appena arrivato
014400060405     c                   eval      worm = *off
014500060405     c**!!!kFnorm9       Chain     Fnorm09l
014600060405     c     kfnorm9       setll     fnorm09l
014700060405      * non trovo nessun orm errore
014800060405    1c                   if        not %equal(fnorm09l)
014900060405     c                   Eval      O24Err = 01
015000060405     c                   leavesr
015100060405    1c                   EndIf
015200060405      * trovo almeno un orm
015300060405    1c                   do        *hival
015400060405     c     kfnorm9       reade     fnorm09l
015500060405     c                   if        %eof(fnorm09l)
015600060405     c                   leave
015700060405     c                   endif
015800060405      * controllo --> se la fase non è 100 continuo la ricerca
015900060405     c                   if        ormfao <> 100
016000060405     c                   iter
016100060405     c                   endif
016200031217      * trovo l'orm imposto i campi di output
016300031217     c                   Eval      O24Poe = OrmPoe
016400031217     c                   Eval      O24Nsr = OrmNsr
016500031217     c                   Eval      O24Nor = OrmNor
016600031217     c                   Eval      O24Nrv = OrmNrv
016700031217      * se p.o. ritiro non impostato faccio altri controlli
016800031217if  2c                   If        Ormpor = *Zeros
016900031217     c                   ExSr      Sr_CtrPor
017000060405     c                   if        o24err <> *zeros
017100060405     c                   leave
017200060405     c                   endif
017300031217    2c                   EndIf
017400060405     c                   eval      worm = *on
017500060405     c                   leave
017600060405    1c                   enddo
017700060405      * se non ho trovato nessun orm valido errore
017800060405     c                   if        worm = *off
017900060405     c                   eval      o24err = 01
018000060405     c                   endif
018100031217
018200031217     c                   EndSr
018300031217
018400031217      *------------------------------------------------------------------------*
018500031217      * Aggiorno l'ORM
018600031217      *------------------------------------------------------------------------*
018700031217     c     Sr_AggOrm     BegSr
018800031217
018900040227     c                   Eval      wagg = *Off
019000040227
019100031217      * Apro i file se non l'ho già fatto
019200031217if  1c                   If        Not %Open(Fnorm01l)
019300031217     c                   Open      Fnorm01l
019400031217    1c                   EndIf
019500031217
019600031217      * Aggancio l'ORM
019700040227     c     kFnorm1       Chain     Fnorm01l
019800040227if  1c                   If        %Found(Fnorm01l)
019900040227      * cambio il p.o. ritiro
020000040227if  2c                   If        OrmPor = *Zeros
020100031217     c                   Eval      OrmPor = I24Lna
020200031217     c                   Move      I24Znc        OrmZor
020300040227     c                   Eval      wagg = *On
020400040227    2c                   EndIf
020500040227      * cambio il riferimento
020600040227if  2c                   If        I24NSP <> *Zeros
020700051216     c                   Move      I24nAas       wrmaas
020800040227     c                   Move      I24Lnp        wrmlnp
020900040227     c                   Move      I24Nrs        wrmnrs
021000040227     c                   Move      I24Nsp        wrmnsp
021100040227     c                   Eval      wagg = *On
021200040227    2c                   EndIf
021300040227      * aggiorno ORM
021400040227if  2c                   If        Wagg = *On
021500040909     c                   Z-add     *Date         OrmDtt
021600031217     c                   Update    Fnorm01
021700040227      * non devo aggiornare disalloco ORM
021800040227     c                   Else
021900040227     c                   Unlock    Fnorm01l
022000040227    2c                   EndIf
022100031217
022200031217    1c                   EndIf
022300031217
022400031217     c                   EndSr
022500051216
022600051216      *------------------------------------------------------------------------*
022700051216      * Controllo esistenza ORM per accorpamento bolle
022800051216      *------------------------------------------------------------------------*
022900051216     c     Sr_CtrOrm1    BegSr
023000051216
023100051216      * pulisco i campi di output
023200051216     c                   Clear                   O24Err
023300051216     c                   Clear                   O24Msg
023400051216     c                   Clear                   O24Poe
023500051216     c                   Clear                   O24Nsr
023600051216     c                   Clear                   O24Nor
023700051216     c                   Clear                   O24Nrv
023800051216
023900051216      * preparo la chiave
024000051216     c                   Clear                   kOrmCor
024100051216     c                   Clear                   kOrmRfa
024200051216     c                   Movel     I24Ksc        kOrmCor
024300051216     c                   Move      '333'         kOrmCor
024400051216
024500051216     c                   Eval      %subst(kOrmRfa:1:1)  = 'B'
024600051216     c                   Eval      %subst(kOrmRfa:2:2)  = I24Aas
024700051216     c                   Eval      %subst(kOrmRfa:4:12) = I24Spe
024800051216
024900051216      * cerco l'ORM
025000060405      * non cerco più con chain diretta xchè è capitato di trovare orm già
025100060405      * chiusi con lo stesso riferimento della VAB appena arrivato
025200060405     c                   eval      worm = *off
025300060405     c**!!!kFnorm9       Chain     Fnorm09l
025400060405     c     kfnorm9       setll     fnorm09l
025500060405      * non trovo nessun orm errore
025600060405    1c                   if        not %equal(fnorm09l)
025700060405     c                   Eval      O24Err = 01
025800060405     c                   leavesr
025900060405    1c                   EndIf
026000060405      * trovo almeno un orm
026100060405    1c                   do        *hival
026200060405     c     kfnorm9       reade     fnorm09l
026300060405     c                   if        %eof(fnorm09l)
026400060405     c                   leave
026500060405     c                   endif
026600060405      * controllo --> se la fase non è 100 continuo la ricerca
026700060405     c                   if        ormfao <> 100
026800060405     c                   iter
026900060405     c                   endif
027000051216      * trovo l'orm imposto i campi di output
027100051216     c                   Eval      O24Poe = OrmPoe
027200051216     c                   Eval      O24Nsr = OrmNsr
027300051216     c                   Eval      O24Nor = OrmNor
027400051216     c                   Eval      O24Nrv = OrmNrv
027500060405
027600051216      * controllo l'orm
027700051216
027800051216      * Apro il file se non l'ho già fatto
027900051216if  1c                   If        Not %Open(Fnorm01l)
028000051216     c                   Open      Fnorm01l
028100051216    1c                   EndIf
028200051216      * Aggancio l'ORM
028300051216     c     kFnorm1       Chain(e)  Fnorm01l
028400060405      * Orm allocato errore
028500060405     c                   if        %error
028600060405     c                   Eval      O24Err = 02
028700060405     c                   leave
028800060405     c                   endif
028900060405sel 1c**!!!              Select
029000060405w   1c**!!!              When      %Error
029100051216      * deve essere in fase 100
029200060405      * l'ho già controllato prima
029300060405w   1c**!!!              When      OrmFao <> 100
029400060405     c**!!!              Eval      O24Err = 03
029500060405    1c**!!!              EndSl
029600060405     c                   eval      worm = *on
029700051216     c                   Unlock    Fnorm01l
029800060405     c                   leave
029900060405    1c                   enddo
030000060405      * se non ho trovato nessun orm valido errore
030100060405     c                   if        worm = *off
030200060405     c                   eval      o24err = 01
030300060405     c                   endif
030400051216
030500051216     c                   EndSr
030600051216
030700051216      *------------------------------------------------------------------------*
030800051216      * Aggiorno l'ORM per accorpamento bolle
030900051216      *------------------------------------------------------------------------*
031000051216     c     Sr_AggOrm1    BegSr
031100051216
031200051216     c                   Eval      wagg = *Off
031300051216
031400051216      * Apro i file se non l'ho già fatto
031500051216if  1c                   If        Not %Open(Fnorm01l)
031600051216     c                   Open      Fnorm01l
031700051216    1c                   EndIf
031800051216
031900051216      * Aggancio l'ORM
032000051216     c     kFnorm1       Chain     Fnorm01l
032100051216if  1c                   If        %Found(Fnorm01l)
032200051216      * cambio il riferimento
032300051216if  2c                   If        I24NSP <> *Zeros
032400051219     c                   Move      I24nAas       wrmaas
032500051216     c                   Move      I24Lnp        wrmlnp
032600051216     c                   Move      I24Nrs        wrmnrs
032700051216     c                   Move      I24Nsp        wrmnsp
032800051216     c                   Eval      wagg = *On
032900051216    2c                   EndIf
033000051216      * aggiorno ORM
033100051216if  2c                   If        Wagg = *On
033200051216     c                   Z-add     *Date         OrmDtt
033300051216     c                   Update    Fnorm01
033400051216      * non devo aggiornare disalloco ORM
033500051216     c                   Else
033600051216     c                   Unlock    Fnorm01l
033700051216    2c                   EndIf
033800051216
033900051216    1c                   EndIf
034000051216
034100051216     c                   EndSr
034200020212
034300031217      *------------------------------------------------------------------------*
034400031217      * Chiusura dei file aperti
034500031217      *------------------------------------------------------------------------*
034600031217     c     Sr_CloOrm     BegSr
034700031217
034800031217if  1c                   If        %Open(Fnorm01l)
034900031217     c                   Close     Fnorm01l
035000031217    1c                   EndIf
035100031217
035200031217     c                   EndSr
035300031217
035400031217      *------------------------------------------------------------------------*
035500031217      * Fine programma
035600031217      *------------------------------------------------------------------------*
035700031217     c     Sr_EndPgm     BegSr
035800031217
035900031217      * restituzione dei dati
036000031217sel 1c                   Select
036100031217
036200031217w   1c                   When      O24Err = *Zeros and I24Tla <> 'C'
036300031217     c                   Eval      O24Poe = OrmPoe
036400031217     c                   Eval      O24Nsr = OrmNsr
036500031217     c                   Eval      O24Nor = OrmNor
036600031217     c                   Eval      O24Nrv = OrmNrv
036700031217
036800031217w   1c                   When      O24Err > *Zeros
036900031217     c                   Eval      O24Msg = msg(O24Err)
037000031217
037100031217    1c                   EndSl
037200031217
037300031217if  1c                   If        I24Tla = *Blanks
037400031217     c                   Eval      *InRt = *On
037500031217   x1c                   Else
037600031217     c                   Eval      *InLr = *On
037700031217    1c                   EndIf
037800031217
037900031217     c                   Return
038000031217
038100031217     c                   EndSr
038200031216
038300031217      *------------------------------------------------------------------------*
038400031217      * Altri controlli per p.o. ritiro
038500031217      *------------------------------------------------------------------------*
038600031217     c     Sr_CtrPor     BegSr
038700031217
038800031217      * Apro il file se non l'ho già fatto
038900031217if  1c                   If        Not %Open(Fnorm01l)
039000031217     c                   Open      Fnorm01l
039100031217    1c                   EndIf
039200031217
039300031217      * Aggancio l'ORM
039400031217     c     kFnorm1       Chain(e)  Fnorm01l
039500031217
039600031217sel 1c                   Select
039700031217      * Orm allocato
039800031217w   1c                   When      %Error
039900031217     c                   Eval      O24Err = 02
040000031217      * deve essere in fase 100
040100060405      * già fatto prima questo controllo
040200060405w   1c**!!!              When      OrmFao <> 100
040300060405     c**!!!              Eval      O24Err = 03
040400031217      * deve esserci la linea di arrivo
040500031217w   1c                   When      I24Lna = *Zeros
040600031217     c                   Eval      O24Err = 04
040700031217    1c                   EndSl
040800031217
040900031217     c                   Unlock    Fnorm01l
041000031217
041100031217     c                   EndSr
041200031217
041300031216      *------------------------------------------------------------------------*
041400031216      * Operazioni iniziali
041500031216      *------------------------------------------------------------------------*
041600031216     c     *Inzsr        BegSr
041700031216
041800031217      * Klist
041900031217
042000031217     c     kFnOrm9       Klist
042100031217     c                   Kfld                    kOrmCor
042200031217     c                   Kfld                    kOrmRfa
042300031217
042400031217     c     kFnOrm1       Klist
042500031217     c                   Kfld                    O24Poe
042600031217     c                   Kfld                    O24Nsr
042700031217     c                   Kfld                    O24Nor
042800031217     c                   Kfld                    O24Nrv
042900031216
043000031216     c                   EndSr
043100031216
043200031217** Msg                                                                       *
043300031217FNLS24R - ORM non trovato                                                     01
043400031217FNLS24R - Record del file FNORM00F allocato                                   02
043500031217FNLS24R - ORM non in fase 100                                                 03
043600031217FNLS24R - Manca la linea di arrivo                                            04
