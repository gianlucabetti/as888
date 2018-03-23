000100070320      *---------------------------------------------------------------*
000200070320      * SCRITTURA RICHIESTE ASSISTENZA CLIENTI pgm RICHIAMATO         *
000300070320      *---------------------------------------------------------------*
000400070320
000500070320     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600070320
000700070320      *---------------------------------------------------------------*
000800070320
000900070320     fFITGD00F  o  a e             disk
000901140328     fFITGR00F  o  a e             disk
001000070320
001100070320      *---------------------------------------------------------------*
001200070320
001300070320      *
001400070320      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
001500070320      *
001600070320
001700070320     D NOB             S             35    DIM(12)                              Note
001800070320     D NORA            S             35    DIM(12)                              Note da scrivere
001900070320
002000070320      *
002100070320      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
002200070320      *
002300070320     d KPJBA         e ds
002400070320      * - Parametri per scrittura nuovi reclami
002500140401     d FIdna6DS      e ds
002501140925      * - DS  per Ritorno del numero R.A.
002502140925     d FIdna6BDS     e ds
002600070320      * - Parametri note
002700070320     d FIDNB0DS      e ds
002800070320      * - Parametri x Numeratore Reclami
002900070320     d TRUL33ds      e ds
002901140401      * - Parametri x Tabella TNTBE
002903140401     d TIBS02ds      e ds                  inz
002904140401      * - Tabella MAR
002905140401     d DMAR          e ds
002906140401      * - Tabella CHR
002907140401     d DCHR          e ds
003000070320      *
003100070320     d Status         sds
003200070320     d  SDSpgm           *proc
003300070320     d  PARMS            *parms
003400090918      * - Flag operativi FITGD TGDFLO
003500090918     d DTGD01        e ds
003600070320      *
003700070320     d WLBdat          ds                  inz
003800070320     d  G02dat                 1      8  0 inz
003900070320     d  G02inv                 9     16  0 inz
004000070320     d  G02err                17     17    inz('3')
004100070320     d  G02tgi                18     22  0 inz
004200070320
004300070320      *
004400070320      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
004500070320      *
004600070320      * - Campi di comodo
004700070320     d w0140           s             14  0
004800070320     d wData           s              8  0
004900070320     d wOra            s              6  0
005000070320     d wanno           s              4  0
005100070320     d Dateu           s              8  0
005200070320
005300070320     d x               s              2  0
005400070320     d y               s              2  0
005500070320      *
005501140924     d WRit_nra        s              1
005600070320      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
005700070320      *
005800070320
005900070320      *===============================================================*
006000070320      *  RIEPILOGO INDICATORI                                         *
006100070320      *===============================================================*
006200070320
006300070320     c                   clear                   fitgd000
006400070320
006500140401     c                   eval      tgdpot = Ida06fil                            p.o. inserimento
006600070320     c                   eval      tgdant = wanno                               anno R.A.
006700070320      *
006800070320      * Stacco il numero dal numeratore
006900070320      *
007000070320
007100070320     c                   clear                   Trul33ds
007200070320     c                   eval      i33tla = 'L'
007300070320     c                   z-add     0             I33ope
007400070320     c                   z-add     040           I33cnu
007500070320     c                   z-add     1             I33num
007600070320     c                   z-add     wanno         I33aaa
007700070320     c                   movel(p)  trul33ds      kpjbu
007800070320     c                   CALL      'TRUL33R'
007900070320     c                   PARM                    KPJBA
008000070320     c                   eval      trul33ds = kpjbu
008100070320
008200070320     c                   if        o33err = *zeros
008300070320     c                   z-add     o33nri        tgdnut                         numero reclamo
008400070320     c                   endif
008500070320
008600070320     c                   z-add     1             tgdpdt                         progressivo
008700070320
008701140401     c                   if        ida06dac = 0
008800140328     c                   eval      tgddac = Wdata                               data inserimento
008801140401     c                   else
008802140401     c                   eval      tgddac = ida06dac
008803140401     c                   Endif
008804140401     c                   if        ida06orc = 0
008900070320     c                   eval      tgdorc = wora                                ora inserimento
008901140401     c                   else
008902140401     c                   eval      tgdorc = ida06orc
008903140401     c                   Endif
008904140401     c                   if        ida06dar = 0
009000140401     c                   eval      tgddar = tgddac                              data ricezione
009001140401     c                   else
009002140401     c                   eval      tgddar = ida06dar                            data ricezione
009003140401     c                   Endif
009004140401     c                   if        ida06orr = 0
009100140401     c                   eval      tgdorr = tgdorc                              ora ricezione
009101140401     c                   else
009102140401     c                   eval      tgdorr = ida06orr                            ora ricezione
009103140401     c                   Endif
009200140401     c                   eval      tgdpru = Ida06ute                            utente inserimento
009300140401     c                   IF        Ida06tco <> *blanks
009400140401     c                   eval      TGDtco = Ida06tco
009500140217     c                   ELSE
009600070320     c                   eval      tgdtco = 'T'                                 tipo comunicazione
009700140217     c                   ENDIF
009800140401     c                   IF        Ida06tch <> *blanks
009900140401     c                   eval      TGDtch = Ida06tch
010000140217     c                   ELSE
010100070320     c                   eval      tgdtch = 'F'                                 tipo chiamante
010200140217     c                   ENDIF
010300140401     c                   IF        Ida06clv <> *blanks
010400140401     c                   eval      TGDclv = Ida06clv
010500140217     c                   ELSE
010600070320     c                   eval      tgdclv = ' '                                 priorità chiamante
010700140217     c                   ENDIF
010701140401     c                   IF        Ida06rsc <> *blanks
010702140401     c                   eval      tgdrsc = Ida06rsc                            rag.sociale chiamant
010703140401     c                   ELSE
010800140401     c                   eval      tgdrsc = Ida06ute                            rag.sociale chiamant
010801140401     c                   ENDIF
010900070320      *
011000070320      * oggetto
011100140401     c                   eval      tgdtor = Ida06tor                            tipo oggetto reclamo
011200140401     c                   eval      tgdogg = Ida06ogg                            oggetto
011300040218      * motivo apertura
011400140401     c                   eval      tgdmad = Ida06mad                            motivo d'apertura
011500070320
011600140401     c                   IF        Ida06prt <> *blanks
011700140401     c                   eval      TGDprt = Ida06prt
011800140217     c                   ELSE
011900070320     c                   eval      tgdprt = '9'                                 priorità
012000140217     c                   ENDIF
012001140401     c                   if        Ida06pog <> *zeros
012002140401     c                   eval      tgdpog = Ida06pog                            P.o. gestione
012003140401     c                   else
012100140401     c                   eval      tgdpog = Ida06fil                            P.o. gestione
012101140401     c                   endif
012102140401     c                   if        Ida06rpg <> *blanks
012200140401     c                   eval      tgdrpg = Ida06rpg                            Utente gestione
012201140401     c                   else
012202140401     c                   eval      tgdrpg = Ida06ute                            Utente gestione
012203140401     c                   Endif
012300040218      * chiusura
012301140401      * Se passato il codice si usa quello passato , se viene passato "999" la R.A. non si chiude
012302140401      * se passato blank si recupera la causale di chiusura dalla tabella "MAR"
012303140401      *
012304140401      * codice chiusra passato
012400140401     c                   IF        Ida06cdt <> *blanks and Ida06cdt <> '999'
012500140401     c                   eval      TGDcdt = Ida06cdt                            Codice chiusura
012600140401     c                   Endif
012601140401      *
012602140401      * codice chiusra blank cerco il codice nella tabella MAR e se non esiste imposto "  I"
012603140401      * chiusura di tipo INFO
012604140401    1c                   IF        Ida06cdt = *blanks
012605140401     c                   clear                   tibs02ds
012606140401     C                   MOVEL     'C'           t02mod
012607140401     C                   MOVEL     knsif         t02sif
012608140401     C                   MOVEL     'MAR'         t02cod
012609140401     C                   MOVEL     tgdmad        t02ke1
012610140401      *
012611140401     C                   CALL      'TIBS02R'
012612140401     C                   PARM                    KPJBA
012613140401     C                   PARM                    TIBS02DS
012614140401      *
012615140401     C                   IF        t02err = *BLANKS
012616140401     C                   movel     T02uni        DMAR
012617140401     c                   If        §marmotch <> *blank
012618140401     C                   movel     §MARmotch     tgdcdt                         Codice chiusura
012619140401     c                   else
012700070320     c                   eval      tgdcdt = '  I'                               Codice chiusura
012701140401     c                   Endif
012800140217     c                   ENDIF
012801140401    1c                   ENDIF
012802140401
012803140401      * se impostato il codice chiusura imposto anche gli altri dati
012804140401    1c                   If        tgdcdt <> *blank
012805140401     c                   If        IDA06dcd = 0
012900140401     c                   eval      tgddcd = tgddac                              data chiusura
012901140401     c                   else
012902140401     c                   eval      tgddcd = ida06dcd                            data chiusura
012903140401     c                   Endif
012904140401     c                   If        IDA06ocd = 0
013000140401     c                   eval      tgdocd = tgdorc                              ora chiusura
013001140401     c                   else
013002140401     c                   eval      tgdocd = ida06ocd                            ora chiusura
013004140401     c                   Endif
013005140401     c                   if        ida06pct = 0
013100140401     c                   eval      tgdpct = Ida06fil                            utente p.o. chiusura
013101140401     c                   else
013102140401     c                   eval      tgdpct = Ida06pct                            utente p.o. chiusura
013103140401     c                   endif
013104140401     c                   if        ida06utc = *blank
013200140401     c                   eval      tgdutc = Ida06ute                            utente chiusura
013201140401     c                   else
013202140401     c                   eval      tgdutc = Ida06utc                            utente chiusura
013204140401     c                   endif
013300140401     c                   IF        Ida06rsp <> *blanks
013400140401     c                   eval      TGDrsp = Ida06rsp                            Flg responsabilità
013401140401
013500140217     c                   ELSE
013501140401
013502140401      * se non impostatao lo recupera dalla tabella dei motivi di chiusura
013503140401     c                   clear                   tibs02ds
013504140401     C                   MOVEL     'C'           t02mod
013505140401     C                   MOVEL     knsif         t02sif
013506140401     C                   MOVEL     'CHR'         t02cod
013507140401     C                   MOVEL     TGDcdt        t02ke1
013508140401      *
013509140401     C                   CALL      'TIBS02R'
013510140401     C                   PARM                    KPJBA
013511140401     C                   PARM                    TIBS02DS
013512140401      *
013513140401     C                   IF        t02err = *BLANKS
013514140401     C                   movel     T02uni        DCHR
013515140401     c                   If        §chrresp  <> *blank
013516140401     C                   movel     §CHRRESP      tgdrsp                         flg responsabilità
013700140217     c                   ENDIF
013701140401     c                   ENDIF
013702140401     c                   ENDIF
013703140401      * filiale responsabile se codice di chiusura Fondato e non è stata passata la filiale
013704140401      * responsabile metto la filiale di apertuta
013705140401     c                   if        Ida06por <> 0
013706140401     c                   eval      tgdpor = ida06por
013707140401     c                   else
013708140401     c                   if        tgdcdt = '  F'
013709140401     c                   eval      tgdpor = ida06fil
013710140401     c                   endif
013711140401     c                   endif
013712140401      *
013713140401    1c                   ENDIF
013800090918      * abilitazione rapida
013900140401     c                   eval      dtgd01= Ida06flo
014000140401     c                   If        §tgdarap = 'S'
014100090918     c                   eval      §tgdarap = 'R'
014200090922     c                   endif
014600090918     c                   eval      tgdflo = dtgd01
014601140924
014602140924     c                   eval      tgdksc = Ida06ksc                            codice chiamante
014700070320
014800040218     c                   write     fitgd000
014900040217
015000140217     c                   clear                   kpjba
015100040217      * confermo le note
015200040217
015300070320      * verifico se ho delle note da scrivere
015400140401     c                   movea     Ida06no1      nob(1)
015500140401     c                   movea     Ida06no2      nob(9)
015600070320      * tolgo le note a blank
015700070320     c                   do        12            x
015800070320     c                   if        nob(x) <> *blanks
015900070320     c                   add       1             y
016000070320     c                   move      nob(x)        nora(Y)
016100070320     c                   endif
016200070320     c                   enddo
016300070320
016400070320      * provo a scrivere le prime 4 note
016500070320    1c                   if        y > *zeros
016600070320     c                   clear                   FIDNB0DS
016700070320     c                   eval      db0no1 = nora(1)
016800070320     c                   eval      db0no2 = nora(2)
016900070320     c                   eval      db0no3 = nora(3)
017000070320     c                   eval      db0no4 = nora(4)
017100070320
017200070320     c                   z-add     tgdant        iB0ant
017300070320     c                   z-add     tgdnut        iB0nut
017400070320     c                   z-add     tgdpdt        iB0pdt
017500040217     c                   movel     'GEN'         iB0tIN
017600070320     c                   eval      ib0pot =tgdpot
017700140401     c                   eval      db0pon =Ida06fil
017800140401     c                   eval      db0utn =Ida06ute
017900070320      * imposto 'W'
018000070320     c                   movel     'W'           iB0flm
018100070320      *
018200070320     c                   call      'FIDNB0R'
018300070320     c                   parm                    kpjba
018400070320     c                   parm                    fidnb0ds
018500070320      * verifico se ho altre note da scrivere
018600070320    2c                   if        nora(5) <> *blanks
018700070320     c                   clear                   fidnb0ds
018800070320     c                   eval      db0no1 = nora(5)
018900070320     c                   eval      db0no2 = nora(6)
019000070320     c                   eval      db0no3 = nora(7)
019100070320     c                   eval      db0no4 = nora(8)
019200070320
019300070320      * in caso di modalità immissione
019400070320     c                   z-add     tgdant        iB0ant
019500070320     c                   z-add     tgdnut        iB0nut
019600070320     c                   z-add     tgdpdt        iB0pdt
019700070320     c                   z-add     999           ib0pgn
019800070320     c                   movel     'GEN'         iB0tIN
019900070320     c                   eval      ib0pot =tgdpot
020000140401     c                   eval      db0pon =Ida06fil
020100140401     c                   eval      db0utn =Ida06ute
020200070320      * imposto 'W'
020300070320     c                   movel     'W'           iB0flm
020400070320      *
020500070320     c                   call      'FIDNB0R'
020600070320     c                   parm                    kpjba
020700070320     c                   parm                    fidnb0ds
020800070320
020900070320    2c                   endif
021000070320      * verifico se ho altre note da scrivere
021100070320    2c                   if        nora(9) <> *blanks
021200070320     c                   clear                   fidnb0ds
021300070320     c                   eval      db0no1 = nora(9)
021400070320     c                   eval      db0no2 = nora(10)
021500070320     c                   eval      db0no3 = nora(11)
021600070320     c                   eval      db0no4 = nora(12)
021700070320
021800070320      * in caso di modalità immissione
021900070320     c                   z-add     tgdant        iB0ant
022000070320     c                   z-add     tgdnut        iB0nut
022100070320     c                   z-add     tgdpdt        iB0pdt
022200070320     c                   z-add     999           ib0pgn
022300070320     c                   movel     'GEN'         iB0tIN
022400070320     c                   eval      ib0pot =tgdpot
022500140401     c                   eval      db0pon =Ida06fil
022600140401     c                   eval      db0utn =Ida06ute
022700070320      * imposto 'W'
022800070320     c                   movel     'W'           iB0flm
022900070320      *
023000070320     c                   call      'FIDNB0R'
023100070320     c                   parm                    kpjba
023200070320     c                   parm                    fidnb0ds
023300070320    2c                   endif
023301140401      * se viene passato  referente / mail / telefono fax scrivo i relativi record
023302140401     c                   if        ida06ref <> *blank
023303140401     c                   clear                   fitgr000
023304140401     c                   eval      tgrpot = tgdpot                              p.o. immette recla
023305140401     c                   eval      tgrant = tgdant                              Anno   reclamo
023306140401     c                   eval      tgrnut = tgdnut                              Numero reclamo
023307140401     c                   eval      tgrtri = 'R'                                 tipo  riferimento
023308140401     c                   movel     ida06ref      tgrrif
023309140401     c                   write     fitgr000
023311140401     c                   endif
023312140401      * se esiste indirizzo e-mail
023313140401     c                   if        ida06mai <> *blank
023314140401     c                   clear                   fitgr000
023318140401     c                   eval      tgrpot = tgdpot                              p.o. immette recla
023319140401     c                   eval      tgrant = tgdant                              Anno   reclamo
023320140401     c                   eval      tgrnut = tgdnut                              Numero reclamo
023321140401     c                   eval      tgrtri = 'M'                                 tipo  riferimento
023322140401     c                   movel     ida06mai      tgrrif
023323140401     c                   write     fitgr000
023325140401     c                   endif
023326140401      * se esiste numero telefono e/o numero fax
023327140401     c                   if        ida06tel <> *blank
023328140401     c                   clear                   fitgr000
023329140401     c                   eval      tgrpot = tgdpot                              p.o. immette recla
023330140401     c                   eval      tgrant = tgdant                              Anno   reclamo
023331140401     c                   eval      tgrnut = tgdnut                              Numero reclamo
023335140401     c                   eval      tgrtri = 'N'                                 tipo  riferimento
023336140401     c                   movel     ida06tel      tgrrif
023338140401     c                   write     fitgr000
023340140401     c                   endif
023400070320      * richiamo a chiusura di pgm
023500070320     c                   clear                   FIDNB0DS
023600070320     c                   movel     'C'           ib0tla
023800070320     c                   call      'FIDNB0R'
023900070320     c                   parm                    kpjba
024000070320     c                   parm                    FIDNB0DS
024100070320      *
024200070320    1c                   endif
024300070320      *
024301140925      * se mi è stato passato anche la 2° ds FIDNA6BDS la valorizzo con mil numero R.A.
024302140925     c                   if        Writ_NRA = 'S'
024303140925     c                   eval      ODA6BANT = tgdant
024304140925     c                   eval      ODA6BNUT = tgdnut
024305140925     c                   eval      ODA6BPDT = tgdpdt
024306140925     c                   endif
024307140925      *
024400070320     c                   seton                                        lr
024500040213      *
024600040126      *---------------------------------------------------------------*
024700040126      * Routine Iniziale                                              *
024800040126      *---------------------------------------------------------------*
024900040126     c     *InzSR        BegSR
025000040126      *
025100040129     c     *Entry        plist
025200140401     c                   parm                    FIdna6DS
025201140924     c                   parm                    FIdna6bDS
025202140924      * verifico se passati + parametri
025203140924     c                   If        %parms> 1
025204140924     c                   eval      Writ_NRA = 'S'
025205140924     c                   endif
025206140924      *
026400140217
026500040126      * Reperisco data corrente
026700140217     c                   time                    w0140
026800140401     c                   move      w0140         dateu
026900070320     c                   move      w0140         wanno
027400140217
027500040126     c                   clear                   WLBdat
027600140401     c                   eval      G02dat = dateu
027700040126     c                   call      'XSRDA8'
027800040126     c                   parm                    WLBdat
027900140328     c                   z-add     G02inv        Wdata
028000041005
028002140328     c                   movel     w0140         wOra
028004140328
028005140401
028006140401      /free
028007140401       //?Imposto anno nel caso mi hanno passato la data
028008140401        IF Ida06dac > 0;
028009140401          wAnno = %dec(%subst(%editc(Ida06dac:'X'):1:4):4:0);
028010140401        ENDIF;
028011140401      /end-free
028100040126     c                   EndSr
028200040126
