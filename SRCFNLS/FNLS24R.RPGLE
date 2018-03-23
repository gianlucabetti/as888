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
003900070709     ffnorg01l  uf   e           k Disk    usropn
004000031216
004100031217      *------------------------------------------------------------------------*
004200031217      *   C A M P I   D I   L A V O R O
004300031217      *------------------------------------------------------------------------*
004400031217
004500031217     d kOrmCor         s                   Like(OrmCor)
004600031217     d kOrmRfa         s                   Like(OrmRfa)
004700040227
004800040227     d wagg            s              1    Inz(*Off)
004900060405     d worm            s              1    inz(*off)
005000031217
005100031216      *------------------------------------------------------------------------*
005200031216      *   S C H I E R E
005300031216      *------------------------------------------------------------------------*
005400031216
005500031216      * Messaggi di errore
005600031217     d msg             s             78    Dim(05) Ctdata Perrcd(1)
005700031216
005800031216      *------------------------------------------------------------------------*
005900031216      *   D S   I N T E R N E / E S T E R N E
006000031216      *------------------------------------------------------------------------*
006100031216
006200040227     d  OrmRfa         ds
006300040227     d  wrmbolla               1      1
006400040227     d  wrmaas                 2      3
006500040227     d  wrmlnp                 4      6
006600040227     d  wrmnrs                 7      8
006700040227     d  wrmnsp                 9     15
006800040227
006900020212      * Passaggio Parametri
007000031217     d Fnls24ds      e ds
007100031216
007200031216     d Kpjba         e ds
007300031216
007400031216      *------------------------------------------------------------------------*
007500031216      * RIEPILOGO INDICATORI
007600031216      *------------------------------------------------------------------------*
007700031216
007800031216
007900031216      *------------------------------------------------------------------------*
008000031216
008100031216     c     *Entry        Plist
008200031217     c                   Parm                    Fnls24ds
008300031216
008400031217      * Controllo il tipo lancio
008500031217if  1c                   If        I24Tla <> *Blanks and I24Tla <> 'C' and
008600031217     c                             I24Tla <> 'L'
008700031217     c                   Clear                   I24Tla
008800031217    1c                   EndIf
008900031217
009000031217sel 1c                   Select
009100031217
009200031217      * Elaboro
009300031217w   1c                   When      I24tla <> 'C'
009400031217      * Controllo esistenza dell'ORM
009500031217if  2c                   If        I24Ope = 'C'
009600031217     c                   ExSr      Sr_CtrOrm
009700031217    2c                   EndIf
009800031217      * Aggiorno ORM
009900031217if  2c                   If        I24Ope = 'A'
010000031217     c                   ExSr      Sr_AggOrm
010100031217    2c                   EndIf
010200051216      * Controllo esistenza dell'ORM x accorpamento bolle
010300051216if  2c                   If        I24Ope = 'L'
010400051216     c                   ExSr      Sr_CtrOrm1
010500051216    2c                   EndIf
010600051216      * Aggiorno ORM per accorpamento bolle
010700051216if  2c                   If        I24Ope = 'U'
010800051216     c                   ExSr      Sr_AggOrm1
010900051216    2c                   EndIf
011000031217
011100031217      * Chiusura archivi
011200031217w   1c                   When      I24Tla = 'C'
011300031217     c                   ExSr      Sr_CloOrm
011400031217    1c                   EndSl
011500031217
011600031217      * Fine
011700031217     c                   ExSr      Sr_EndPgm
011800031217
011900031217      *------------------------------------------------------------------------*
012000031217      * Controllo esistenza ORM
012100031217      *------------------------------------------------------------------------*
012200031217     c     Sr_CtrOrm     BegSr
012300031217
012400031217      * pulisco i campi di output
012500031217     c                   Clear                   O24Err
012600031217     c                   Clear                   O24Msg
012700031217     c                   Clear                   O24Poe
012800031217     c                   Clear                   O24Nsr
012900031217     c                   Clear                   O24Nor
013000031217     c                   Clear                   O24Nrv
013100031217
013200031217      * preparo la chiave
013300031217     c                   Clear                   kOrmCor
013400031217     c                   Clear                   kOrmRfa
013500031217     c                   Movel     I24Ksc        kOrmCor
013600031217     c                   Move      '333'         kOrmCor
013700031217
013800031217     c                   Eval      %subst(kOrmRfa:1:1)  = 'B'
013900031217     c                   Eval      %subst(kOrmRfa:2:2)  = I24Aas
014000031217     c                   Eval      %subst(kOrmRfa:4:12) = I24Spe
014100031217
014200031217      * cerco l'ORM
014300060405      * non cerco più con chain diretta xchè è capitato di trovare orm già
014400060405      * chiusi con lo stesso riferimento della VAB appena arrivato
014500060405     c                   eval      worm = *off
014600060405     c**!!!kFnorm9       Chain     Fnorm09l
014700060405     c     kfnorm9       setll     fnorm09l
014800060405      * non trovo nessun orm errore
014900060405    1c                   if        not %equal(fnorm09l)
015000060405     c                   Eval      O24Err = 01
015100060405     c                   leavesr
015200060405    1c                   EndIf
015300060405      * trovo almeno un orm
015400060405    1c                   do        *hival
015500060405     c     kfnorm9       reade     fnorm09l
015600060405     c                   if        %eof(fnorm09l)
015700060405     c                   leave
015800060405     c                   endif
015900060405      * controllo --> se la fase non è 100 continuo la ricerca
016000060405     c                   if        ormfao <> 100
016100060405     c                   iter
016200060405     c                   endif
016300031217      * trovo l'orm imposto i campi di output
016400031217     c                   Eval      O24Poe = OrmPoe
016500031217     c                   Eval      O24Nsr = OrmNsr
016600031217     c                   Eval      O24Nor = OrmNor
016700031217     c                   Eval      O24Nrv = OrmNrv
016800031217      * se p.o. ritiro non impostato faccio altri controlli
016900031217if  2c                   If        Ormpor = *Zeros
017000031217     c                   ExSr      Sr_CtrPor
017100060405     c                   if        o24err <> *zeros
017200060405     c                   leave
017300060405     c                   endif
017400031217    2c                   EndIf
017500060405     c                   eval      worm = *on
017600060405     c                   leave
017700060405    1c                   enddo
017800060405      * se non ho trovato nessun orm valido errore
017900060405     c                   if        worm = *off
018000060405     c                   eval      o24err = 01
018100060405     c                   endif
018200031217
018300031217     c                   EndSr
018400031217
018500031217      *------------------------------------------------------------------------*
018600031217      * Aggiorno l'ORM
018700031217      *------------------------------------------------------------------------*
018800031217     c     Sr_AggOrm     BegSr
018900031217
019000040227     c                   Eval      wagg = *Off
019100040227
019200031217      * Apro i file se non l'ho già fatto
019300031217if  1c                   If        Not %Open(Fnorm01l)
019400031217     c                   Open      Fnorm01l
019500031217    1c                   EndIf
019600070709if  1c                   If        Not %Open(Fnorg01l)
019700070709     c                   Open      Fnorg01l
019800070709    1c                   EndIf
019900031217
020000031217      * Aggancio l'ORM
020100040227     c     kFnorm1       Chain     Fnorm01l
020200040227if  1c                   If        %Found(Fnorm01l)
020300040227      * cambio il p.o. ritiro
020400040227if  2c                   If        OrmPor = *Zeros
020500031217     c                   Eval      OrmPor = I24Lna
020600031217     c                   Move      I24Znc        OrmZor
020700040227     c                   Eval      wagg = *On
020800040227    2c                   EndIf
020900040227      * cambio il riferimento
021000040227if  2c                   If        I24NSP <> *Zeros
021100051216     c                   Move      I24nAas       wrmaas
021200040227     c                   Move      I24Lnp        wrmlnp
021300040227     c                   Move      I24Nrs        wrmnrs
021400040227     c                   Move      I24Nsp        wrmnsp
021500040227     c                   Eval      wagg = *On
021600040227    2c                   EndIf
021700040227      * aggiorno ORM
021800040227if  2c                   If        Wagg = *On
021900040909     c                   Z-add     *Date         OrmDtt
022000031217     c                   Update    Fnorm01
022100070709      * se ho aggiornato l'ORM devo aggiornare anche FNORG
022200070709     c     kfnorm1       chain     fnorg01l
022300070709if  1c                   if        %found(fnorg01l)
022400070709      * cambio il p.o. ritiro
022500070709     c                   eval      orgpor = ormpor
022600070709     c                   update    fnorg000
022700070709    2c                   endif
022800040227      * non devo aggiornare disalloco ORM
022900040227     c                   Else
023000040227     c                   Unlock    Fnorm01l
023100040227    2c                   EndIf
023200031217
023300031217    1c                   EndIf
023400031217
023500031217     c                   EndSr
023600051216
023700051216      *------------------------------------------------------------------------*
023800051216      * Controllo esistenza ORM per accorpamento bolle
023900051216      *------------------------------------------------------------------------*
024000051216     c     Sr_CtrOrm1    BegSr
024100051216
024200051216      * pulisco i campi di output
024300051216     c                   Clear                   O24Err
024400051216     c                   Clear                   O24Msg
024500051216     c                   Clear                   O24Poe
024600051216     c                   Clear                   O24Nsr
024700051216     c                   Clear                   O24Nor
024800051216     c                   Clear                   O24Nrv
024900051216
025000051216      * preparo la chiave
025100051216     c                   Clear                   kOrmCor
025200051216     c                   Clear                   kOrmRfa
025300051216     c                   Movel     I24Ksc        kOrmCor
025400051216     c                   Move      '333'         kOrmCor
025500051216
025600051216     c                   Eval      %subst(kOrmRfa:1:1)  = 'B'
025700051216     c                   Eval      %subst(kOrmRfa:2:2)  = I24Aas
025800051216     c                   Eval      %subst(kOrmRfa:4:12) = I24Spe
025900051216
026000051216      * cerco l'ORM
026100060405      * non cerco più con chain diretta xchè è capitato di trovare orm già
026200060405      * chiusi con lo stesso riferimento della VAB appena arrivato
026300060405     c                   eval      worm = *off
026400060405     c**!!!kFnorm9       Chain     Fnorm09l
026500060405     c     kfnorm9       setll     fnorm09l
026600060405      * non trovo nessun orm errore
026700060405    1c                   if        not %equal(fnorm09l)
026800060405     c                   Eval      O24Err = 01
026900060405     c                   leavesr
027000060405    1c                   EndIf
027100060405      * trovo almeno un orm
027200060405    1c                   do        *hival
027300060405     c     kfnorm9       reade     fnorm09l
027400060405     c                   if        %eof(fnorm09l)
027500060405     c                   leave
027600060405     c                   endif
027700060405      * controllo --> se la fase non è 100 continuo la ricerca
027800060405     c                   if        ormfao <> 100
027900060405     c                   iter
028000060405     c                   endif
028100051216      * trovo l'orm imposto i campi di output
028200051216     c                   Eval      O24Poe = OrmPoe
028300051216     c                   Eval      O24Nsr = OrmNsr
028400051216     c                   Eval      O24Nor = OrmNor
028500051216     c                   Eval      O24Nrv = OrmNrv
028600060405
028700051216      * controllo l'orm
028800051216
028900051216      * Apro il file se non l'ho già fatto
029000051216if  1c                   If        Not %Open(Fnorm01l)
029100051216     c                   Open      Fnorm01l
029200051216    1c                   EndIf
029300051216      * Aggancio l'ORM
029400051216     c     kFnorm1       Chain(e)  Fnorm01l
029500060405      * Orm allocato errore
029600060405     c                   if        %error
029700060405     c                   Eval      O24Err = 02
029800060405     c                   leave
029900060405     c                   endif
030000060405sel 1c**!!!              Select
030100060405w   1c**!!!              When      %Error
030200051216      * deve essere in fase 100
030300060405      * l'ho già controllato prima
030400060405w   1c**!!!              When      OrmFao <> 100
030500060405     c**!!!              Eval      O24Err = 03
030600060405    1c**!!!              EndSl
030700060405     c                   eval      worm = *on
030800051216     c                   Unlock    Fnorm01l
030900060405     c                   leave
031000060405    1c                   enddo
031100060405      * se non ho trovato nessun orm valido errore
031200060405     c                   if        worm = *off
031300060405     c                   eval      o24err = 01
031400060405     c                   endif
031500051216
031600051216     c                   EndSr
031700051216
031800051216      *------------------------------------------------------------------------*
031900051216      * Aggiorno l'ORM per accorpamento bolle
032000051216      *------------------------------------------------------------------------*
032100051216     c     Sr_AggOrm1    BegSr
032200051216
032300051216     c                   Eval      wagg = *Off
032400051216
032500051216      * Apro i file se non l'ho già fatto
032600051216if  1c                   If        Not %Open(Fnorm01l)
032700051216     c                   Open      Fnorm01l
032800051216    1c                   EndIf
032900051216
033000051216      * Aggancio l'ORM
033100051216     c     kFnorm1       Chain     Fnorm01l
033200051216if  1c                   If        %Found(Fnorm01l)
033300051216      * cambio il riferimento
033400051216if  2c                   If        I24NSP <> *Zeros
033500051219     c                   Move      I24nAas       wrmaas
033600051216     c                   Move      I24Lnp        wrmlnp
033700051216     c                   Move      I24Nrs        wrmnrs
033800051216     c                   Move      I24Nsp        wrmnsp
033900051216     c                   Eval      wagg = *On
034000051216    2c                   EndIf
034100051216      * aggiorno ORM
034200051216if  2c                   If        Wagg = *On
034300051216     c                   Z-add     *Date         OrmDtt
034400051216     c                   Update    Fnorm01
034500051216      * non devo aggiornare disalloco ORM
034600051216     c                   Else
034700051216     c                   Unlock    Fnorm01l
034800051216    2c                   EndIf
034900051216
035000051216    1c                   EndIf
035100051216
035200051216     c                   EndSr
035300020212
035400031217      *------------------------------------------------------------------------*
035500031217      * Chiusura dei file aperti
035600031217      *------------------------------------------------------------------------*
035700031217     c     Sr_CloOrm     BegSr
035800031217
035900031217if  1c                   If        %Open(Fnorm01l)
036000031217     c                   Close     Fnorm01l
036100031217    1c                   EndIf
036200031217
036300031217     c                   EndSr
036400031217
036500031217      *------------------------------------------------------------------------*
036600031217      * Fine programma
036700031217      *------------------------------------------------------------------------*
036800031217     c     Sr_EndPgm     BegSr
036900031217
037000031217      * restituzione dei dati
037100031217sel 1c                   Select
037200031217
037300031217w   1c                   When      O24Err = *Zeros and I24Tla <> 'C'
037400031217     c                   Eval      O24Poe = OrmPoe
037500031217     c                   Eval      O24Nsr = OrmNsr
037600031217     c                   Eval      O24Nor = OrmNor
037700031217     c                   Eval      O24Nrv = OrmNrv
037800031217
037900031217w   1c                   When      O24Err > *Zeros
038000031217     c                   Eval      O24Msg = msg(O24Err)
038100031217
038200031217    1c                   EndSl
038300031217
038400031217if  1c                   If        I24Tla = *Blanks
038500031217     c                   Eval      *InRt = *On
038600031217   x1c                   Else
038700031217     c                   Eval      *InLr = *On
038800031217    1c                   EndIf
038900031217
039000031217     c                   Return
039100031217
039200031217     c                   EndSr
039300031216
039400031217      *------------------------------------------------------------------------*
039500031217      * Altri controlli per p.o. ritiro
039600031217      *------------------------------------------------------------------------*
039700031217     c     Sr_CtrPor     BegSr
039800031217
039900031217      * Apro il file se non l'ho già fatto
040000031217if  1c                   If        Not %Open(Fnorm01l)
040100031217     c                   Open      Fnorm01l
040200031217    1c                   EndIf
040300031217
040400031217      * Aggancio l'ORM
040500031217     c     kFnorm1       Chain(e)  Fnorm01l
040600031217
040700031217sel 1c                   Select
040800031217      * Orm allocato
040900031217w   1c                   When      %Error
041000031217     c                   Eval      O24Err = 02
041100031217      * deve essere in fase 100
041200060405      * già fatto prima questo controllo
041300060405w   1c**!!!              When      OrmFao <> 100
041400060405     c**!!!              Eval      O24Err = 03
041500031217      * deve esserci la linea di arrivo
041600031217w   1c                   When      I24Lna = *Zeros
041700031217     c                   Eval      O24Err = 04
041800031217    1c                   EndSl
041900031217
042000031217     c                   Unlock    Fnorm01l
042100031217
042200031217     c                   EndSr
042300031217
042400031216      *------------------------------------------------------------------------*
042500031216      * Operazioni iniziali
042600031216      *------------------------------------------------------------------------*
042700031216     c     *Inzsr        BegSr
042800031216
042900031217      * Klist
043000031217
043100031217     c     kFnOrm9       Klist
043200031217     c                   Kfld                    kOrmCor
043300031217     c                   Kfld                    kOrmRfa
043400031217
043500031217     c     kFnOrm1       Klist
043600031217     c                   Kfld                    O24Poe
043700031217     c                   Kfld                    O24Nsr
043800031217     c                   Kfld                    O24Nor
043900031217     c                   Kfld                    O24Nrv
044000031216
044100031216     c                   EndSr
044200031216
044300031217** Msg                                                                       *
044400031217FNLS24R - ORM non trovato                                                     01
044500031217FNLS24R - Record del file FNORM00F allocato                                   02
044600031217FNLS24R - ORM non in fase 100                                                 03
044700031217FNLS24R - Manca la linea di arrivo                                            04
