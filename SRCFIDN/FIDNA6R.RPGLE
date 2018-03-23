000100070320      *---------------------------------------------------------------*
000200070320      * SCRITTURA RICHIESTE ASSISTENZA CLIENTI pgm RICHIAMATO         *
000300070320      *---------------------------------------------------------------*
000400070320
000500070320     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600070320
000700070320      *---------------------------------------------------------------*
000800070320
000900070320     fFITGD00F  o  a e             disk
001000140328     fFITGR00F  o  a e             disk
001100070320
001200070320      *---------------------------------------------------------------*
001300070320
001400070320      *
001500070320      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
001600070320      *
001700070320
001800070320     D NOB             S             35    DIM(12)                              Note
001900070320     D NORA            S             35    DIM(12)                              Note da scrivere
002000070320
002100070320      *
002200070320      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
002300070320      *
002400070320     d KPJBA         e ds
002500070320      * - Parametri per scrittura nuovi reclami
002600140401     d FIdna6DS      e ds
002700140925      * - DS  per Ritorno del numero R.A.
002800140925     d FIdna6BDS     e ds
002900070320      * - Parametri note
003000070320     d FIDNB0DS      e ds
003100070320      * - Parametri x Numeratore Reclami
003200070320     d TRUL33ds      e ds
003300140401      * - Parametri x Tabella TNTBE
003400140401     d TIBS02ds      e ds                  inz
003500140401      * - Tabella MAR
003600140401     d DMAR          e ds
003700140401      * - Tabella CHR
003800140401     d DCHR          e ds
003900070320      *
004000070320     d Status         sds
004100070320     d  SDSpgm           *proc
004200070320     d  PARMS            *parms
004300090918      * - Flag operativi FITGD TGDFLO
004400090918     d DTGD01        e ds
004500070320      *
004600070320     d WLBdat          ds                  inz
004700070320     d  G02dat                 1      8  0 inz
004800070320     d  G02inv                 9     16  0 inz
004900070320     d  G02err                17     17    inz('3')
005000070320     d  G02tgi                18     22  0 inz
005100070320
005200070320      *
005300070320      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
005400070320      *
005500070320      * - Campi di comodo
005600070320     d w0140           s             14  0
005700070320     d wData           s              8  0
005800070320     d wOra            s              6  0
005900070320     d wanno           s              4  0
006000070320     d Dateu           s              8  0
006100150326      *
006200070320     d x               s              2  0
006300070320     d y               s              2  0
006400070320      *
006500140924     d WRit_nra        s              1
006600150326
006700150326      *
006800070320      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
006900070320      *
007000070320
007100070320      *===============================================================*
007200070320      *  RIEPILOGO INDICATORI                                         *
007300070320      *===============================================================*
007400070320
007500070320     c                   clear                   fitgd000
007600070320
007700140401     c                   eval      tgdpot = Ida06fil                            p.o. inserimento
007800070320     c                   eval      tgdant = wanno                               anno R.A.
007900070320      *
008000070320      * Stacco il numero dal numeratore
008100070320      *
008200070320     c                   clear                   Trul33ds
008300070320     c                   eval      i33tla = 'L'
008400070320     c                   z-add     0             I33ope
008500070320     c                   z-add     040           I33cnu
008600070320     c                   z-add     1             I33num
008700070320     c                   z-add     wanno         I33aaa
008800070320     c                   movel(p)  trul33ds      kpjbu
008900070320     c                   CALL      'TRUL33R'
009000070320     c                   PARM                    KPJBA
009100070320     c                   eval      trul33ds = kpjbu
009200070320
009300070320     c                   if        o33err = *zeros
009400070320     c                   z-add     o33nri        tgdnut                         numero reclamo
009500070320     c                   endif
009600070320
009700070320     c                   z-add     1             tgdpdt                         progressivo
009800070320
009900140401     c                   if        ida06dac = 0
010000140328     c                   eval      tgddac = Wdata                               data inserimento
010100140401     c                   else
010200140401     c                   eval      tgddac = ida06dac
010300140401     c                   Endif
010400140401     c                   if        ida06orc = 0
010500070320     c                   eval      tgdorc = wora                                ora inserimento
010600140401     c                   else
010700140401     c                   eval      tgdorc = ida06orc
010800140401     c                   Endif
010900140401     c                   if        ida06dar = 0
011000140401     c                   eval      tgddar = tgddac                              data ricezione
011100140401     c                   else
011200140401     c                   eval      tgddar = ida06dar                            data ricezione
011300140401     c                   Endif
011400140401     c                   if        ida06orr = 0
011500140401     c                   eval      tgdorr = tgdorc                              ora ricezione
011600140401     c                   else
011700140401     c                   eval      tgdorr = ida06orr                            ora ricezione
011800140401     c                   Endif
011900140401     c                   eval      tgdpru = Ida06ute                            utente inserimento
012000140401     c                   IF        Ida06tco <> *blanks
012100140401     c                   eval      TGDtco = Ida06tco
012200140217     c                   ELSE
012300070320     c                   eval      tgdtco = 'T'                                 tipo comunicazione
012400140217     c                   ENDIF
012500140401     c                   IF        Ida06tch <> *blanks
012600140401     c                   eval      TGDtch = Ida06tch
012700140217     c                   ELSE
012800070320     c                   eval      tgdtch = 'F'                                 tipo chiamante
012900140217     c                   ENDIF
013000140401     c                   IF        Ida06clv <> *blanks
013100140401     c                   eval      TGDclv = Ida06clv
013200140217     c                   ELSE
013300070320     c                   eval      tgdclv = ' '                                 priorità chiamante
013400140217     c                   ENDIF
013500140401     c                   IF        Ida06rsc <> *blanks
013600140401     c                   eval      tgdrsc = Ida06rsc                            rag.sociale chiamant
013700140401     c                   ELSE
013800140401     c                   eval      tgdrsc = Ida06ute                            rag.sociale chiamant
013900140401     c                   ENDIF
014000070320      *
014100070320      * oggetto
014200140401     c                   eval      tgdtor = Ida06tor                            tipo oggetto reclamo
014300140401     c                   eval      tgdogg = Ida06ogg                            oggetto
014400040218      * motivo apertura
014500140401     c                   eval      tgdmad = Ida06mad                            motivo d'apertura
014600150507      * filiale gestione
014700140401     c                   if        Ida06pog <> *zeros
014800140401     c                   eval      tgdpog = Ida06pog                            P.o. gestione
014900140401     c                   else
015000140401     c                   eval      tgdpog = Ida06fil                            P.o. gestione
015100140401     c                   endif
015200150507      * utente gestione
015300140401     c                   if        Ida06rpg <> *blanks
015400140401     c                   eval      tgdrpg = Ida06rpg                            Utente gestione
015500140401     c                   else
015600140401     c                   eval      tgdrpg = Ida06ute                            Utente gestione
015700140401     c                   Endif
015800040218      * chiusura
015900140401      * Se passato il codice si usa quello passato , se viene passato "999" la R.A. non si chiude
016000140401      * se passato blank si recupera la causale di chiusura dalla tabella "MAR"
016100140401      *
016200140401      * codice chiusra passato
016300140401     c                   IF        Ida06cdt <> *blanks and Ida06cdt <> '999'
016400140401     c                   eval      TGDcdt = Ida06cdt                            Codice chiusura
016500140401     c                   Endif
016600140401      *
016700140401      * codice chiusra blank cerco il codice nella tabella MAR e se non esiste imposto "  I"
016800140401      * chiusura di tipo INFO
016900140401    1c                   IF        Ida06cdt = *blanks
017000140401     c                   clear                   tibs02ds
017100140401     C                   MOVEL     'C'           t02mod
017200140401     C                   MOVEL     knsif         t02sif
017300140401     C                   MOVEL     'MAR'         t02cod
017400140401     C                   MOVEL     tgdmad        t02ke1
017500140401      *
017600140401     C                   CALL      'TIBS02R'
017700140401     C                   PARM                    KPJBA
017800140401     C                   PARM                    TIBS02DS
017900140401      *
018000140401     C                   IF        t02err = *BLANKS
018100140401     C                   movel     T02uni        DMAR
018200140401     c                   If        §marmotch <> *blank
018300140401     C                   movel     §MARmotch     tgdcdt                         Codice chiusura
018400140401     c                   else
018500070320     c                   eval      tgdcdt = '  I'                               Codice chiusura
018600140401     c                   Endif
018700140217     c                   ENDIF
018800140401    1c                   ENDIF
018900140401
019000140401      * se impostato il codice chiusura imposto anche gli altri dati
019100140401    1c                   If        tgdcdt <> *blank
019200140401     c                   If        IDA06dcd = 0
019300140401     c                   eval      tgddcd = tgddac                              data chiusura
019400140401     c                   else
019500140401     c                   eval      tgddcd = ida06dcd                            data chiusura
019600140401     c                   Endif
019700140401     c                   If        IDA06ocd = 0
019800140401     c                   eval      tgdocd = tgdorc                              ora chiusura
019900140401     c                   else
020000140401     c                   eval      tgdocd = ida06ocd                            ora chiusura
020100140401     c                   Endif
020200140401     c                   if        ida06pct = 0
020300140401     c                   eval      tgdpct = Ida06fil                            utente p.o. chiusura
020400140401     c                   else
020500140401     c                   eval      tgdpct = Ida06pct                            utente p.o. chiusura
020600140401     c                   endif
020700140401     c                   if        ida06utc = *blank
020800140401     c                   eval      tgdutc = Ida06ute                            utente chiusura
020900140401     c                   else
021000140401     c                   eval      tgdutc = Ida06utc                            utente chiusura
021100140401     c                   endif
021200140401     c                   IF        Ida06rsp <> *blanks
021300140401     c                   eval      TGDrsp = Ida06rsp                            Flg responsabilità
021400140401
021500140217     c                   ELSE
021600140401
021700140401      * se non impostatao lo recupera dalla tabella dei motivi di chiusura
021800140401     c                   clear                   tibs02ds
021900140401     C                   MOVEL     'C'           t02mod
022000140401     C                   MOVEL     knsif         t02sif
022100140401     C                   MOVEL     'CHR'         t02cod
022200140401     C                   MOVEL     TGDcdt        t02ke1
022300140401      *
022400140401     C                   CALL      'TIBS02R'
022500140401     C                   PARM                    KPJBA
022600140401     C                   PARM                    TIBS02DS
022700140401      *
022800140401     C                   IF        t02err = *BLANKS
022900140401     C                   movel     T02uni        DCHR
023000140401     c                   If        §chrresp  <> *blank
023100140401     C                   movel     §CHRRESP      tgdrsp                         flg responsabilità
023200140217     c                   ENDIF
023300140401     c                   ENDIF
023400140401     c                   ENDIF
023500140401      * filiale responsabile se codice di chiusura Fondato e non è stata passata la filiale
023600140401      * responsabile metto la filiale di apertuta
023700140401     c                   if        Ida06por <> 0
023800140401     c                   eval      tgdpor = ida06por
023900140401     c                   else
024000140401     c                   if        tgdcdt = '  F'
024100140401     c                   eval      tgdpor = ida06fil
024200140401     c                   endif
024300140401     c                   endif
024400140401      *
024500140401    1c                   ENDIF
024600150326      *
024700150326      * stato (ex priorità)
024800150326     c                   Select
024900150326     c                   When      Ida06prt <> *blanks
025000150326     c                   eval      TGDprt = Ida06prt
025100150326     c                   When      TGDcdt <> *blanks
025200150326     c                   eval      tgdprt = '7'                                 stato (ex priorità)
025300150326     c                   Other
025400150326     c                   eval      TGDprt = '0'
025500150326     c                   EndSl
025600090918      * abilitazione rapida
025700140401     c                   eval      dtgd01= Ida06flo
025800140401     c                   If        §tgdarap = 'S'
025900090918     c                   eval      §tgdarap = 'R'
026000090922     c                   endif
026100090918     c                   eval      tgdflo = dtgd01
026200150507      * codice chiamante
026300140924     c                   eval      tgdksc = Ida06ksc                            codice chiamante
026400150507      *?reparto gestione?
026500150507     c                   eval      TGDrpr = IDA06rpr
026600070320
026700040218     c                   write     fitgd000
026800040217
026900140217     c                   clear                   kpjba
027000040217      * confermo le note
027100040217
027200070320      * verifico se ho delle note da scrivere
027300140401     c                   movea     Ida06no1      nob(1)
027400140401     c                   movea     Ida06no2      nob(9)
027500070320      * tolgo le note a blank
027600070320     c                   do        12            x
027700070320     c                   if        nob(x) <> *blanks
027800070320     c                   add       1             y
027900070320     c                   move      nob(x)        nora(Y)
028000070320     c                   endif
028100070320     c                   enddo
028200070320
028300070320      * provo a scrivere le prime 4 note
028400070320    1c                   if        y > *zeros
028500070320     c                   clear                   FIDNB0DS
028600070320     c                   eval      db0no1 = nora(1)
028700070320     c                   eval      db0no2 = nora(2)
028800070320     c                   eval      db0no3 = nora(3)
028900070320     c                   eval      db0no4 = nora(4)
029000070320
029100070320     c                   z-add     tgdant        iB0ant
029200070320     c                   z-add     tgdnut        iB0nut
029300070320     c                   z-add     tgdpdt        iB0pdt
029400040217     c                   movel     'GEN'         iB0tIN
029500070320     c                   eval      ib0pot =tgdpot
029600140401     c                   eval      db0pon =Ida06fil
029700140401     c                   eval      db0utn =Ida06ute
029800070320      * imposto 'W'
029900070320     c                   movel     'W'           iB0flm
030000070320      *
030100070320     c                   call      'FIDNB0R'
030200070320     c                   parm                    kpjba
030300070320     c                   parm                    fidnb0ds
030400070320      * verifico se ho altre note da scrivere
030500070320    2c                   if        nora(5) <> *blanks
030600070320     c                   clear                   fidnb0ds
030700070320     c                   eval      db0no1 = nora(5)
030800070320     c                   eval      db0no2 = nora(6)
030900070320     c                   eval      db0no3 = nora(7)
031000070320     c                   eval      db0no4 = nora(8)
031100070320
031200070320      * in caso di modalità immissione
031300070320     c                   z-add     tgdant        iB0ant
031400070320     c                   z-add     tgdnut        iB0nut
031500070320     c                   z-add     tgdpdt        iB0pdt
031600070320     c                   z-add     999           ib0pgn
031700070320     c                   movel     'GEN'         iB0tIN
031800070320     c                   eval      ib0pot =tgdpot
031900140401     c                   eval      db0pon =Ida06fil
032000140401     c                   eval      db0utn =Ida06ute
032100070320      * imposto 'W'
032200070320     c                   movel     'W'           iB0flm
032300070320      *
032400070320     c                   call      'FIDNB0R'
032500070320     c                   parm                    kpjba
032600070320     c                   parm                    fidnb0ds
032700070320
032800070320    2c                   endif
032900070320      * verifico se ho altre note da scrivere
033000070320    2c                   if        nora(9) <> *blanks
033100070320     c                   clear                   fidnb0ds
033200070320     c                   eval      db0no1 = nora(9)
033300070320     c                   eval      db0no2 = nora(10)
033400070320     c                   eval      db0no3 = nora(11)
033500070320     c                   eval      db0no4 = nora(12)
033600070320
033700070320      * in caso di modalità immissione
033800070320     c                   z-add     tgdant        iB0ant
033900070320     c                   z-add     tgdnut        iB0nut
034000070320     c                   z-add     tgdpdt        iB0pdt
034100070320     c                   z-add     999           ib0pgn
034200070320     c                   movel     'GEN'         iB0tIN
034300070320     c                   eval      ib0pot =tgdpot
034400140401     c                   eval      db0pon =Ida06fil
034500140401     c                   eval      db0utn =Ida06ute
034600070320      * imposto 'W'
034700070320     c                   movel     'W'           iB0flm
034800070320      *
034900070320     c                   call      'FIDNB0R'
035000070320     c                   parm                    kpjba
035100070320     c                   parm                    fidnb0ds
035200070320    2c                   endif
035300140401      * se viene passato  referente / mail / telefono fax scrivo i relativi record
035400140401     c                   if        ida06ref <> *blank
035500140401     c                   clear                   fitgr000
035600140401     c                   eval      tgrpot = tgdpot                              p.o. immette recla
035700140401     c                   eval      tgrant = tgdant                              Anno   reclamo
035800140401     c                   eval      tgrnut = tgdnut                              Numero reclamo
035900140401     c                   eval      tgrtri = 'R'                                 tipo  riferimento
036000140401     c                   movel     ida06ref      tgrrif
036100140401     c                   write     fitgr000
036200140401     c                   endif
036300140401      * se esiste indirizzo e-mail
036400140401     c                   if        ida06mai <> *blank
036500140401     c                   clear                   fitgr000
036600140401     c                   eval      tgrpot = tgdpot                              p.o. immette recla
036700140401     c                   eval      tgrant = tgdant                              Anno   reclamo
036800140401     c                   eval      tgrnut = tgdnut                              Numero reclamo
036900140401     c                   eval      tgrtri = 'M'                                 tipo  riferimento
037000140401     c                   movel     ida06mai      tgrrif
037100140401     c                   write     fitgr000
037200140401     c                   endif
037300140401      * se esiste numero telefono e/o numero fax
037400140401     c                   if        ida06tel <> *blank
037500140401     c                   clear                   fitgr000
037600140401     c                   eval      tgrpot = tgdpot                              p.o. immette recla
037700140401     c                   eval      tgrant = tgdant                              Anno   reclamo
037800140401     c                   eval      tgrnut = tgdnut                              Numero reclamo
037900140401     c                   eval      tgrtri = 'N'                                 tipo  riferimento
038000140401     c                   movel     ida06tel      tgrrif
038100140401     c                   write     fitgr000
038200140401     c                   endif
038300070320      * richiamo a chiusura di pgm
038400070320     c                   clear                   FIDNB0DS
038500070320     c                   movel     'C'           ib0tla
038600070320     c                   call      'FIDNB0R'
038700070320     c                   parm                    kpjba
038800070320     c                   parm                    FIDNB0DS
038900070320      *
039000070320    1c                   endif
039100070320      *
039200140925      * se mi è stato passato anche la 2° ds FIDNA6BDS la valorizzo con mil numero R.A.
039300140925     c                   if        Writ_NRA = 'S'
039400140925     c                   eval      ODA6BANT = tgdant
039500140925     c                   eval      ODA6BNUT = tgdnut
039600140925     c                   eval      ODA6BPDT = tgdpdt
039700140925     c                   endif
039800140925      *
039900070320     c                   seton                                        lr
040000040213      *
040100040126      *---------------------------------------------------------------*
040200040126      * Routine Iniziale                                              *
040300040126      *---------------------------------------------------------------*
040400040126     c     *InzSR        BegSR
040500040126      *
040600040129     c     *Entry        plist
040700140401     c                   parm                    FIdna6DS
040800140924     c                   parm                    FIdna6bDS
040900140924      * verifico se passati + parametri
041000140924     c                   If        %parms> 1
041100140924     c                   eval      Writ_NRA = 'S'
041200140924     c                   endif
041300140924      *
041400140217
041500040126      * Reperisco data corrente
041600140217     c                   time                    w0140
041700140401     c                   move      w0140         dateu
041800070320     c                   move      w0140         wanno
041900140217
042000040126     c                   clear                   WLBdat
042100140401     c                   eval      G02dat = dateu
042200040126     c                   call      'XSRDA8'
042300040126     c                   parm                    WLBdat
042400140328     c                   z-add     G02inv        Wdata
042500041005
042600140328     c                   movel     w0140         wOra
042700140328
042800140401
042900140401      /free
043000140401       //?Imposto anno nel caso mi hanno passato la data
043100140401        IF Ida06dac > 0;
043200140401          wAnno = %dec(%subst(%editc(Ida06dac:'X'):1:4):4:0);
043300140401        ENDIF;
043400140401      /end-free
043500040126     c                   EndSr
043600040126
