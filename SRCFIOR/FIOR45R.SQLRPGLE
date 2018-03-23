000100040422      *PARMS CLOSQLCSR(*ENDMOD)
000200040422      *PARMS DYNUSRPRF(*OWNER)
000300040422     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000400040422      *------------------------------------------------------------------------*
000500040426      *
000600040422      *
    Statistica ORM fissi non eseguiti   ?
000700040426      *
000800040422      *------------------------------------------------------------------------*
000900040422
001000040426     fFnorm00f  if   e             Disk
001100040426     fAzorg01l  if   e           k Disk
001200040426     fFnorf01l  if   e           k Disk
001300040427     fFnors01l  if   e           k Disk
001400040427     fFnacr01l  if   e           k Disk
001500040426     fFior45p   o    e             Printer OfLind(*In90)
001600040426
001700040426      *------------------------------------------------------------------------*
001800040426      *   S C H I E R E
001900040426      *------------------------------------------------------------------------*
002000040428     d gio             s             10    dim(7) ctdata perrcd(1)              giorni settimana
002100040427     d skpoel          s              3  0 dim(250)                             po da elab.
002200040428     d pgiose          s              3    dim(5)
002300040426
002400040426      *------------------------------------------------------------------------*
002500040426      *   V A R I A B I L I
002600040426      *------------------------------------------------------------------------*
002700040426     d savpoe          s                   like(ormpoe)
002800040426     d StringaSql      s           1000    Varying
002900040426     d xa              s              5i 0 inz(*zeros)
003000040427     d xb              s              5i 0 inz(*zeros)
003100040426     d wdate           s              8  0
003200040428     d wggmmaa         s              6
003300040428     d wgioset         s              1
003400040426     d wora            s              6  0
003500040426     d w0140           s             14  0
003600040426
003700040426      *------------------------------------------------------------------------*
003800040426      *   D S   I N T E R N E / E S T E R N E
003900040426      *------------------------------------------------------------------------*
004000040428     d                 ds
004100040428     d dtadar                  1      8  0
004200040428     d dtaggmm                 1      4  0
004300040428     d dtaaa                   7      8  0
004400040428
004500040427     d ds_numors       ds                  inz
004600040426     d  fOrmpos                1      3  0
004700040426     d  fOrmors                4     10  0
004800040428
004900040428     d ds_savors       ds                  inz
005000040428     d  kOrmpos                1      3  0
005100040428     d  kOrmors                4     10  0
005200040426
005300040426     d wlbdat          ds                  inz
005400040426     d  g02dat                 1      8  0
005500040426     d  g02inv                 9     16  0
005600040426     d  g02err                17     17
005700040426     d  g02tgi                18     22  0
005800040426
005900040426     d Azuteds       e ds                  Extname(Azute00f)
006000040426     d dCmr          e ds
006100040426     d dDatiute      e ds
006200040426     d dFar          e ds
006300040426     d Fior44ds      e ds
006400040426     d FnormxxDs     e ds                  ExtName(Fnorm00f)
006500040426     d Kpjba         e ds
006600040426     d Tibs02ds      e ds
006700040426     d Tibs34ds      e ds                  Inz
006800040426     d Trul26Ds      e ds
006900040426     d  skpo                  11    760  0 dim(250)
007000040426     d  skpot                761   1510  0 dim(250)
007100040426
007200040422      *------------------------------------------------------------------------*
007300040426      *  RIEPILOGO INDICATORI
007400040426      *------------------------------------------------------------------------*
007500040426      * 90 - overflow file stampa statistica
007600040426
007700040426      *------------------------------------------------------------------------*
007800040427
007900040426      * sql per selezionare gli ORM
008000040426      *                            =========
008100040426     c                   ExSr      SqlOrmPos
008200040426      *                            =========
008300040426
008400040426      * leggo il file degli ORM
008500040426      *                            =====
008600040426     c                   ExSr      Leggi
008700040426      *                            =====
008800040426
008900040426      * Totale x orm fisso
009000040427if  2c                   If        kOrmOrs > *Zeros and TotOrm > *Zeros
009100040427     c                   ExSr      Sr_TotFisso
009200040427e   2c                   EndIf
009300040427      * Totale x p.o. emissione
009400040427if  2c                   If        savpoe > *Zeros and TotPoe > *Zeros
009500040427     c                   Write     or45tt2
009600040427e   2c                   EndIf
009700040426      * Totale generale
009800040426     c                   Write     or45g1
009900040426      *                            ======
010000040426      * fine stampa
010100040426     c                   Write     or45e1
010200040426      *                            ======
010300040426
010400040426      * fine programma
010500040426     c                   Eval      *InLr = *On
010600040426
010700040426      *------------------------------------------------------------------------*
010800040426      * - SQL Preparazione sql FNORM00F
010900040426      *------------------------------------------------------------------------*
011000040426     c     SqlOrmPos     BegSr
011100040426
011200040426      * Preparo la stringa sql
011300040426     c                   Eval      StringaSql =
011400040426     c                             'select * from fnorm00f '                   +
011500040426     c                             'where ormatb = '' '' and '                 +
011600040426     c                             'ormtco = ''S'' and '                       +
011700040426     c                             'ormpoe in ('
011800040426      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
011900040426     c                   Do        250           Xa
012000040427     c                   If        Skpoel(Xa) = *Zeros
012100040426     c                   Leave
012200040426     c                   EndIf
012300040426     c                   Eval      StringaSql =
012400040426     c                             %trim(StringaSql) +  ' '                    +
012500040427     c                             %editc(Skpoel(Xa):'1') + ','
012600040426     c                   EndDo
012700040426     c                   Eval      StringaSql =
012800040426     c                             %trim(StringaSql) + ')'
012900040426      * solo il range delle date immissione richiesto
013000040426     c                   Eval      StringaSql =
013100040426     c                             %trim(StringaSql) + ' '                     +
013200040426     c                             'and ormdao >= '
013300040426     c                   Eval      StringaSql =
013400040426     c                             %trim(StringaSql) + ' '                     +
013500040426     c                             %editc(I44Dtda:'4')
013600040426     c                   Eval      StringaSql =
013700040426     c                             %trim(StringaSql) + ' '                     +
013800040426     c                             'and ormdao <= '
013900040426     c                   Eval      StringaSql =
014000040426     c                             %trim(StringaSql) + ' '                     +
014100040426     c                             %editc(I44Dtal:'4')
014200040426      * ordinato per orm fisso, data ritiro, p.o. ritiro
014300040426     c                   Eval      StringaSql =
014400040426     c                             %trim(StringaSql) + ' '                     +
014500040427     c                             'order by ormpoe, ormpos, ormors, ormdar, ' +
014600040427     c                             'ormpor for fetch only'
014700040426
014800040426     C/EXEC SQL
014900040426     C+ prepare S1 from :StringaSql
015000040426     C/END-EXEC
015100040426
015200040426     C/EXEC SQL
015300040426     C+ declare C1 cursor for S1
015400040426     C/END-EXEC
015500040426
015600040426     c                   EndSr
015700040426
015800040426      *------------------------------------------------------------------------*
015900040426      * - LEGGO IL FILE DEGLI ORM
016000040426      *------------------------------------------------------------------------*
016100040426     c     Leggi         BegSr
016200040426
016300040426     C/EXEC SQL
016400040426     C+ Open C1
016500040426     C/END-EXEC
016600040426
016700040426     C/EXEC SQL
016800040426     C+ Fetch C1 into :fnormxxds
016900040426     C/END-EXEC
017000040426
017100040426do  1c                   Dow       Sqlcod = 0                                   *record letto
017200040426
017300040426     c                   Exsr      Sr_Elabora
017400040426
017500040426     C/EXEC SQL
017600040426     C+ Fetch C1 into :fnormxxds
017700040426     C/END-EXEC
017800040426
017900040426    1c                   EndDo
018000040426
018100040426     C/EXEC SQL
018200040426     C+ close C1
018300040426     C/END-EXEC
018400040426
018500040426     c                   EndSr
018600040426
018700040426      *------------------------------------------------------------------------*
018800040426      * - ELABORO I DATI LETTI
018900040426      *------------------------------------------------------------------------*
019000040426     c     Sr_Elabora    BegSr
019100040426
019200040426     c                   Eval      fOrmPos = OrmPos
019300040426     c                   Eval      fOrmOrs = OrmOrs
019400040422
019500040427      * ROTTURA X P.O. EMISSIONE
019600040427if  1c                   If        OrmPoe <> savpoe
019700040427      * --> stampo il totale x orm fisso
019800040428if  2c                   If        kOrmOrs > *Zeros
019900040428if  3c                   If        TotOrm > *Zeros
020000040427     c                   ExSr      Sr_TotFisso
020100040428e   3c                   EndIf
020200040427     c                   Eval      ds_savors = ds_numors
020300040428     c                   Clear                   TotEse
020400040427e   2c                   EndIf
020500040427      * --> stampo il totale x p.o. emissione
020600040427if  2c                   If        savpoe > *Zeros and TotPoe > *Zeros
020700040427     c                   Write     or45tt2
020800040427e   2c                   EndIf
020900040427      * --> pulisco i campi di stampa
021000040427     c                   Clear                   TotPoe
021100040427      * --> salto Pagina a nuovo p.o. emissione
021200040426     c                   Write     or45t1
021300040426     c                   Eval      *In90 = *Off
021400040427      * --> decodifico il p.o. emissione
021500040426     c     OrmPoe        Chain     Azorg01l
021600040427if  2c                   If        %Found(Azorg01l)
021700040422     c                   Movel     OrgDes        despoe
021800040426     c                   Write     or45t2
021900040427e   2c                   EndIf
022000040427     c                   Write     or45t3
022100040427     c                   Eval      savpoe = OrmPoe
022200040427e   1c                   EndIf
022300040426
022400040427      * SALTO PAGINA
022500040427if  1c                   If        *In90 = *On
022600040426     c                   Write     or45t1
022700040426     c                   Write     or45t2
022800040427     c                   Write     or45t3
022900040426     c                   Eval      *In90 = *Off
023000040427e   1c                   EndIf
023100040426
023200040427      * ROTTURA X ORM FISSO
023300040427if  1c                   If        ds_numors <> ds_savors
023400040427      * --> stampo il totale x orm fisso
023500040427if  2c                   If        kOrmOrs > *Zeros and TotOrm > *Zeros
023600040427     c                   ExSr      Sr_TotFisso
023700040427e   2c                   EndIf
023800040427     c                   Eval      ds_savors = ds_numors
023900040428     c                   Clear                   TotEse
024000040427e   1c                   EndIf
024100040426
024200040427      * DETTAGLIO
024300040426     c                   Clear                   desfao
024400040426     c                   Clear                   descar
024500040427
024600040428      * Sommo i fatti
024700040428if  1c                   If        OrmFao = 600 or OrmFao = 900 or Ormfao = 910
024800040428     c                   add       1             TotEse
024900040428e   1c                   EndIf
025000040428
025100040428      * Elaboro i non fatti
025200040428if  1c                   If        OrmFao = 500 or OrmFao = 999
025300040427
025400040427      * --> cerco la causale
025500040427     c     kOrf          Chain     Fnorf01l
025600040427     c                   If        %Found(Fnorf01l) and OrfCar <> *Blanks
025700040427     c                   Clear                   Tibs02ds
025800040427     c                   Clear                   dCmr
025900040427     c                   Eval      T02Mod = 'C'
026000040427     c                   Eval      T02Sif = knsif
026100040427     c                   Eval      T02Cod = 'CMR'
026200040427     c                   Eval      T02Ke1 = OrfCar
026300040427     c                   Call      'TIBS02R'
026400040427     c                   Parm                    kpjba
026500040427     c                   Parm                    Tibs02ds
026600040427     c                   Eval      dCmr = T02Uni
026700040427     c                   EndIf
026800040427
026900040427      * --> sommo i non fatti x colpa del cliente
027000040428if  2c                   If        d§cmradd <> *Blanks
027100040426     c                   Add       1             TotOrm
027200040427     c                   Add       1             TotPoe
027300040426     c                   Add       1             TotGen
027400040427
027500040427      * --> decodifico la causale
027600040427     c                   Eval      descar = d§cmrdes1
027700040426
027800040427      * --> decodifico fase ORM
027900040426     c                   Clear                   Tibs02ds
028000040426     c                   Clear                   dFar
028100040426     c                   Eval      T02Mod = 'C'
028200040426     c                   Eval      T02Sif = knsif
028300040426     c                   Eval      T02Cod = 'FAR'
028400040426     c                   Movel(p)  OrmFao        T02Ke1
028500040426     c                   Call      'TIBS02R'
028600040426     c                   Parm                    kpjba
028700040426     c                   Parm                    Tibs02ds
028800040426     c                   Eval      dFar = T02Uni
028900040426     c                   Eval      desfao = d§fardes
029000040427
029100040427      * --> giro la data ritiro
029200040427     c                   Clear                   wlbdat
029300040427     c                   Move      OrmDar        g02inv
029400040427     c                   Movel     '3'           G02err
029500040427     c                   Call      'XSRDA8'
029600040427     c                   Parm                    wlbdat
029700040427     c                   Movel     g02dat        dtadar
029800040428
029900040428      * --> recupero il giorno della settimana
030000040428     c                   Clear                   xa
030100040428     c                   Clear                   wgioset
030200040428     c                   Movel     dtaggmm       wggmmaa
030300040428     c                   Move      dtaaa         wggmmaa
030400040428     c                   Call      'XGIOSE1'
030500040428     c                   Parm                    wggmmaa
030600040428     c                   Parm                    wgioset
030700040428     c                   Move      wgioset       xa
030800040428     c                   Eval      pgioset = gio(xa)
030900040426
031000040427      * --> stampo il dettaglio
031100040426     c                   Write     or45d1
031200040428e   2c                   EndIf
031300040428e   1c                   EndIf
031400040426
031500040426     c                   EndSr
031600040426
031700040427      *------------------------------------------------------------------------*
031800040427      * - TOTALE PER ORM FISSO
031900040427      *------------------------------------------------------------------------*
032000040427     c     Sr_TotFisso   BegSr
032100040427
032200040427      * Pulisco i campi
032300040427     c                   Clear                   OrsCra
032400040428     c                   Clear                   pacrrsr
032500040428     c                   Clear                   pacrinr
032600040428     c                   Clear                   pacrlor
032700040427     c                   Clear                   AcrPrr
032800040427     c                   Clear                   AcrNar
032900040428     c                   Clear                   pgiose
033000040428     c                   Clear                   pgiose1
033100040428     c                   Clear                   pgiose2
033200040428     c                   Clear                   pgiose3
033300040428     c                   Clear                   pgiose4
033400040428     c                   Clear                   pgiose5
033500040427
033600040427      * Aggancio l'ORM Fisso
033700040427     c     kOrs          Chain     Fnors01l
033800040427if  1c                   If        %Found(Fnors01l)
033900040428     c                   Exsr      Sr_Gioset
034000040427     c     OrsCra        Chain     Fnacr01l
034100040427if  2c                   If        %Found(Fnacr01l)
034200040428     c                   Eval      pacrrsr = AcrRsr
034300040428     c                   Eval      pacrinr = AcrInr
034400040428     c                   Eval      pacrlor = AcrLor
034500040427e   2c                   EndIf
034600040427   x1c                   Else
034700040428     c                   Eval      pacrrsr = *all'*'
034800040427e   1c                   EndIf
034900040427      * Stampo totale x orm fisso
035000040427     c                   Write     or45tt1
035100040428     c                   Clear                   TotOrm
035200040427
035300040427     c                   EndSr
035400040428
035500040428      *------------------------------------------------------------------------*
035600040428      * - IMPOSTO I GIORNI DELLA SETTIMANA
035700040428      *------------------------------------------------------------------------*
035800040428     c     Sr_Gioset     BegSr
035900040428
036000040428if  1c                   If        OrsRlu = 'S'
036100040428     c                   Eval      xa = 1
036200040428     c     *Blanks       Lookup    pgiose(xa)                             30
036300040428     c                   If        *In30
036400040428     c                   Eval      pgiose(xa) = %subst(gio(1):1:3)
036500040428     c                   EndIf
036600040428e   1c                   EndIf
036700040428if  1c                   If        OrsRma = 'S'
036800040428     c                   Eval      xa = 1
036900040428     c     *Blanks       Lookup    pgiose(xa)                             30
037000040428     c                   If        *In30
037100040428     c                   Eval      pgiose(xa) = %subst(gio(2):1:3)
037200040428     c                   EndIf
037300040428e   1c                   EndIf
037400040428if  1c                   If        OrsRme = 'S'
037500040428     c                   Eval      xa = 1
037600040428     c     *Blanks       Lookup    pgiose(xa)                             30
037700040428     c                   If        *In30
037800040428     c                   Eval      pgiose(xa) = %subst(gio(3):1:3)
037900040428     c                   EndIf
038000040428e   1c                   EndIf
038100040428if  1c                   If        OrsRgi = 'S'
038200040428     c                   Eval      xa = 1
038300040428     c     *Blanks       Lookup    pgiose(xa)                             30
038400040428     c                   If        *In30
038500040428     c                   Eval      pgiose(xa) = %subst(gio(4):1:3)
038600040428     c                   EndIf
038700040428e   1c                   EndIf
038800040428if  1c                   If        OrsRve = 'S'
038900040428     c                   Eval      xa = 1
039000040428     c     *Blanks       Lookup    pgiose(xa)                             30
039100040428     c                   If        *In30
039200040428     c                   Eval      pgiose(xa) = %subst(gio(5):1:3)
039300040428     c                   EndIf
039400040428e   1c                   EndIf
039500040428
039600040428     c                   Eval      pgiose1 = pgiose(1)
039700040428     c                   Eval      pgiose2 = pgiose(2)
039800040428     c                   Eval      pgiose3 = pgiose(3)
039900040428     c                   Eval      pgiose4 = pgiose(4)
040000040428     c                   Eval      pgiose5 = pgiose(5)
040100040428
040200040428     c                   EndSr
040300040427
040400040426      *------------------------------------------------------------------------*
040500040426      * - ROUTINE INIZIALE
040600040426      *------------------------------------------------------------------------*
040700040426     c     *Inzsr        BegSr
040800040422
040900040426     c     *Entry        Plist
041000040426     c                   Parm                    Kpjba
041100040426
041200040422     c                   Movel     Kpjbu         Fior44ds
041300040426
041400040426     c     *dtaara       define    §azute        azuteds
041500040426     c     *dtaara       define    §datiute      ddatiute
041600040426     c                   in(E)     *dtaara
041700040426     c                   If        %error  or RSUT = *blanks
041800040426     c                   CLEAR                   tibs34ds
041900040426     c                   CALL      'TIBS34R'
042000040426     c                   PARM                    tibs34ds
042100040426     c                   in        *dtaara
042200040426     c                   EndIf
042300040426
042400040426      * reperisco data e ora
042500040426     c                   Time                    w0140
042600040426      * udate in GGMMAAAA
042700040426     c                   Move      w0140         wdate
042800040426     c                   Movel     w0140         wora
042900040427
043000040427     c                   Clear                   stringasql
043100040427     c                   Clear                   skpoel
043200040427     c                   Eval      xa = 1
043300040426
043400040427      * Carico sk con i p.o. emissione da elaborare
043500040426if  1c                   If        I44Poel = 999
043600040427     c                   Clear                   Trul26ds
043700040426     c                   Eval      I26tpt = 'P'
043800040426     c                   Eval      I26tla = 'L'
043900040426     c                   Call      'TRUL26R'
044000040427     c                   Parm                    Trul26ds
044100040427      * carico i p.o. da elaborare dell'AS in un sk
044200040427do  2c                   Do        250           xb
044300040427     c                   If        Skpot(xb) = *Zeros
044400040427     c                   Leave
044500040427     c                   EndIf
044600040427      * devo cercare quelli che hanno il terminal di partenza = simfel
044700040427     c                   If        Skpot(xb) = Simfel
044800040427     c                   Eval      Skpoel(xa) = Skpo(xb)
044900040427     c                   Add       1             xa
045000040427     c                   Endif
045100040427e   2c                   EndDo
045200040426      * lanciato un solo p.o. carico questo nella sk
045300040426   x1c                   Else
045400040427     c                   Eval      Skpoel(1) = I44Poel
045500040426    1c                   EndIf
045600040427
045700040427      * giro le date elaborazione
045800040427     c                   Clear                   wlbdat
045900040427     c                   Move      I44Dtda       g02inv
046000040427     c                   Movel     '3'           G02err
046100040427     c                   Call      'XSRDA8'
046200040427     c                   Parm                    wlbdat
046300040427     c                   Movel     g02dat        datadal
046400040427
046500040427     c                   Clear                   wlbdat
046600040427     c                   Move      I44Dtal       g02inv
046700040427     c                   Movel     '3'           G02err
046800040427     c                   Call      'XSRDA8'
046900040427     c                   Parm                    wlbdat
047000040427     c                   Movel     g02dat        dataal
047100040426
047200040426     c     kOrf          klist
047300040426     c                   kfld                    OrmPoe
047400040426     c                   kfld                    OrmNsr
047500040426     c                   kfld                    OrmNor
047600040426     c                   kfld                    OrmNrv
047700040426     c                   kfld                    OrmDfo
047800040426     c                   kfld                    OrmOfo
047900040426     c                   kfld                    OrmFao
048000040427
048100040427     c     kOrs          klist
048200040427     c                   kfld                    kOrmPos
048300040427     c                   kfld                    kOrmOrs
048400040426
048500040426     c                   EndSr
048600040427** gio
048700040427Lunedì                                                                              1
048800040427Martedì                                                                             2
048900040427Mercoledì                                                                           3
049000040427Giovedì                                                                             4
049100040427Venerdì                                                                             5
049200040427Sabato                                                                              6
049300040427Domenica                                                                            7
