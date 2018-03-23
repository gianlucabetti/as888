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
001500031216
001600031216      * OUTPUT
001700031216      * ------
001800031217      *  O24ERR   Codice di errore:
001900031216      *           00 = Nessun errore
002000031216      *           01 = Non trovato ORM
002100031216      *           02 = Record del file FNORM00F allocato
002200031217      *           03 = ORM non aggiornato non in fase 100
002300031217      *           04 = ORM non aggiornato manca la linea di arrivo
002400031217      *  O24MSG   Messaggio di errore
002500031217      *  O24POE   P.o. emissione ORM
002600031217      *  O24NSR   Serie
002700031217      *  O24NOR   Numero ORM
002800031217      *  O24NRV   Viaggio
002900031216
003000031216      *------------------------------------------------------------------------*
003100020212
003200031216     fFnorm09l  if   e           k Disk
003300031216     fFnorm01l  uf   e           k Disk    Usropn
003400031216     f                                     Rename(Fnorm000:Fnorm01)
003500031216     fFnorf01l  uf   e           k Disk    Usropn
003600031216
003700031217      *------------------------------------------------------------------------*
003800031217      *   C A M P I   D I   L A V O R O
003900031217      *------------------------------------------------------------------------*
004000031217
004100031217     d kOrmCor         s                   Like(OrmCor)
004200031217     d kOrmRfa         s                   Like(OrmRfa)
004300031217
004400031216      *------------------------------------------------------------------------*
004500031216      *   S C H I E R E
004600031216      *------------------------------------------------------------------------*
004700031216
004800031216      * Messaggi di errore
004900031217     d msg             s             78    Dim(05) Ctdata Perrcd(1)
005000031216
005100031216      *------------------------------------------------------------------------*
005200031216      *   D S   I N T E R N E / E S T E R N E
005300031216      *------------------------------------------------------------------------*
005400031216
005500020212      * Passaggio Parametri
005600031217     d Fnls24ds      e ds
005700031216
005800031216     d Kpjba         e ds
005900031216
006000031216      *------------------------------------------------------------------------*
006100031216      * RIEPILOGO INDICATORI
006200031216      *------------------------------------------------------------------------*
006300031216
006400031216
006500031216      *------------------------------------------------------------------------*
006600031216
006700031216     c     *Entry        Plist
006800031217     c                   Parm                    Fnls24ds
006900031216
007000031217      * Controllo il tipo lancio
007100031217if  1c                   If        I24Tla <> *Blanks and I24Tla <> 'C' and
007200031217     c                             I24Tla <> 'L'
007300031217     c                   Clear                   I24Tla
007400031217    1c                   EndIf
007500031217
007600031217sel 1c                   Select
007700031217
007800031217      * Elaboro
007900031217w   1c                   When      I24tla <> 'C'
008000031217      * Controllo esistenza dell'ORM
008100031217if  2c                   If        I24Ope = 'C'
008200031217     c                   ExSr      Sr_CtrOrm
008300031217    2c                   EndIf
008400031217      * Aggiorno ORM
008500031217if  2c                   If        I24Ope = 'A'
008600031217     c                   ExSr      Sr_AggOrm
008700031217    2c                   EndIf
008800031217
008900031217      * Chiusura archivi
009000031217w   1c                   When      I24Tla = 'C'
009100031217     c                   ExSr      Sr_CloOrm
009200031217    1c                   EndSl
009300031217
009400031217      * Fine
009500031217     c                   ExSr      Sr_EndPgm
009600031217
009700031217      *------------------------------------------------------------------------*
009800031217      * Controllo esistenza ORM
009900031217      *------------------------------------------------------------------------*
010000031217     c     Sr_CtrOrm     BegSr
010100031217
010200031217      * pulisco i campi di output
010300031217     c                   Clear                   O24Err
010400031217     c                   Clear                   O24Msg
010500031217     c                   Clear                   O24Poe
010600031217     c                   Clear                   O24Nsr
010700031217     c                   Clear                   O24Nor
010800031217     c                   Clear                   O24Nrv
010900031217
011000031217      * preparo la chiave
011100031217     c                   Clear                   kOrmCor
011200031217     c                   Clear                   kOrmRfa
011300031217     c                   Movel     I24Ksc        kOrmCor
011400031217     c                   Move      '333'         kOrmCor
011500031217
011600031217     c                   Eval      %subst(kOrmRfa:1:1)  = 'B'
011700031217     c                   Eval      %subst(kOrmRfa:2:2)  = I24Aas
011800031217     c                   Eval      %subst(kOrmRfa:4:12) = I24Spe
011900031217
012000031217      * cerco l'ORM
012100031217     c     kFnorm9       Chain     Fnorm09l
012200031217      * trovo l'orm imposto i campi di output
012300031217if  1c                   If        %Found(Fnorm09l)
012400031217     c                   Eval      O24Poe = OrmPoe
012500031217     c                   Eval      O24Nsr = OrmNsr
012600031217     c                   Eval      O24Nor = OrmNor
012700031217     c                   Eval      O24Nrv = OrmNrv
012800031217
012900031217      * se p.o. ritiro non impostato faccio altri controlli
013000031217if  2c                   If        Ormpor = *Zeros
013100031217     c                   ExSr      Sr_CtrPor
013200031217    2c                   EndIf
013300031217      * non trovo l'orm errore
013400031217   x1c                   Else
013500031217     c                   Eval      O24Err = 01
013600031217    1c                   EndIf
013700031217
013800031217     c                   EndSr
013900031217
014000031217      *------------------------------------------------------------------------*
014100031217      * Aggiorno l'ORM
014200031217      *------------------------------------------------------------------------*
014300031217     c     Sr_AggOrm     BegSr
014400031217
014500031217      * Apro i file se non l'ho già fatto
014600031217if  1c                   If        Not %Open(Fnorm01l)
014700031217     c                   Open      Fnorm01l
014800031217    1c                   EndIf
014900031217if  1c                   If        Not %Open(Fnorf01l)
015000031217     c                   Open      Fnorf01l
015100031217    1c                   EndIf
015200031217
015300031217      * Aggancio l'ORM
015400031217     c     kFnorm1       Chain     Fnorm01l
015500031217if  1c                   If        %Found(Fnorm01l) and OrmPor = *Zeros
015600031217     c                   Eval      OrmPor = I24Lna
015700031217     c                   Move      I24Znc        OrmZor
015800031217     c                   Eval      OrmPot = OrmPor
015900031217     c                   Clear                   OrmFlt
016000031217     c                   Clear                   OrmDtt
016100031217     c                   Update    Fnorm01
016200031217      * aggiorno anche la fase
016300031217     c     kFnorf1       Chain     Fnorf01l
016400031217if  2c                   If        %Found(Fnorf01l)
016500031217     c                   Clear                   OrfFtr
016600031217     c                   Clear                   OrfDtr
016700031217     c                   Update    Fnorf000
016800031217    2c                   EndIf
016900031217
017000031217    1c                   EndIf
017100031217
017200031217     c                   EndSr
017300020212
017400031217      *------------------------------------------------------------------------*
017500031217      * Chiusura dei file aperti
017600031217      *------------------------------------------------------------------------*
017700031217     c     Sr_CloOrm     BegSr
017800031217
017900031217if  1c                   If        %Open(Fnorm01l)
018000031217     c                   Close     Fnorm01l
018100031217    1c                   EndIf
018200031217if  1c                   If        %Open(Fnorf01l)
018300031217     c                   Close     Fnorf01l
018400031217    1c                   EndIf
018500031217
018600031217     c                   EndSr
018700031217
018800031217      *------------------------------------------------------------------------*
018900031217      * Fine programma
019000031217      *------------------------------------------------------------------------*
019100031217     c     Sr_EndPgm     BegSr
019200031217
019300031217      * restituzione dei dati
019400031217sel 1c                   Select
019500031217
019600031217w   1c                   When      O24Err = *Zeros and I24Tla <> 'C'
019700031217     c                   Eval      O24Poe = OrmPoe
019800031217     c                   Eval      O24Nsr = OrmNsr
019900031217     c                   Eval      O24Nor = OrmNor
020000031217     c                   Eval      O24Nrv = OrmNrv
020100031217
020200031217w   1c                   When      O24Err > *Zeros
020300031217     c                   Eval      O24Msg = msg(O24Err)
020400031217
020500031217    1c                   EndSl
020600031217
020700031217if  1c                   If        I24Tla = *Blanks
020800031217     c                   Eval      *InRt = *On
020900031217   x1c                   Else
021000031217     c                   Eval      *InLr = *On
021100031217    1c                   EndIf
021200031217
021300031217     c                   Return
021400031217
021500031217     c                   EndSr
021600031216
021700031217      *------------------------------------------------------------------------*
021800031217      * Altri controlli per p.o. ritiro
021900031217      *------------------------------------------------------------------------*
022000031217     c     Sr_CtrPor     BegSr
022100031217
022200031217      * Apro il file se non l'ho già fatto
022300031217if  1c                   If        Not %Open(Fnorm01l)
022400031217     c                   Open      Fnorm01l
022500031217    1c                   EndIf
022600031217
022700031217      * Aggancio l'ORM
022800031217     c     kFnorm1       Chain(e)  Fnorm01l
022900031217
023000031217sel 1c                   Select
023100031217      * Orm allocato
023200031217w   1c                   When      %Error
023300031217     c                   Eval      O24Err = 02
023400031217      * deve essere in fase 100
023500031217w   1c                   When      OrmFao <> 100
023600031217     c                   Eval      O24Err = 03
023700031217      * deve esserci la linea di arrivo
023800031217w   1c                   When      I24Lna = *Zeros
023900031217     c                   Eval      O24Err = 04
024000031217    1c                   EndSl
024100031217
024200031217     c                   Unlock    Fnorm01l
024300031217
024400031217     c                   EndSr
024500031217
024600031216      *------------------------------------------------------------------------*
024700031216      * Operazioni iniziali
024800031216      *------------------------------------------------------------------------*
024900031216     c     *Inzsr        BegSr
025000031216
025100031217      * Klist
025200031217
025300031217     c     kFnOrm9       Klist
025400031217     c                   Kfld                    kOrmCor
025500031217     c                   Kfld                    kOrmRfa
025600031217
025700031217     c     kFnOrm1       Klist
025800031217     c                   Kfld                    O24Poe
025900031217     c                   Kfld                    O24Nsr
026000031217     c                   Kfld                    O24Nor
026100031217     c                   Kfld                    O24Nrv
026200031217
026300031217     c     kFnOrf1       Klist
026400031217     c                   Kfld                    OrmPoe
026500031217     c                   Kfld                    OrmNsr
026600031217     c                   Kfld                    OrmNor
026700031217     c                   Kfld                    OrmNrv
026800031217     c                   Kfld                    OrmDfo
026900031217     c                   Kfld                    OrmOfo
027000031217     c                   Kfld                    OrmFao
027100031216
027200031216     c                   EndSr
027300031216
027400031217** Msg                                                                       *
027500031217FNLS24R - ORM non trovato                                                     01
027600031217FNLS24R - Record del file FNORM00F allocato                                   02
027700031217FNLS24R - ORM non in fase 100                                                 03
027800031217FNLS24R - Manca la linea di arrivo                                            04
