000100180308      //--------------------------------------------------------------
000200180308      //?FIDNE1R - ANALISI RA PER OBIETTIVI - Crea File Work
000300180308      //--------------------------------------------------------------
000400180308     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500180308     h dftactgrp(*no) actgrp(*caller)
000600180308
000700180308      //---------------------------------------------------------------
000800180308      //?Dichiarazione file.
000900180308      //---------------------------------------------------------------
001000180312      // - Organigramma
001100180312     fAZORG01L  if   e           k disk
001200180312
001300180309      // - Richieste Assistenza
001400180309     fFITGD11L  if   e           k disk    prefix(w_)
001500180309
001600180308      // - Tabelle
001700180308     fTABEL00F  if   e           k disk
001800180308     fTNTBE01L  if   e           k disk
001900180308
002000180309      // - Bolle di sede
002100180309     fTITAS30C  if   e           k disk
002200180309
002300180309      // - File di WORK dettaglio
002400180312     fWFARA10F  o    e             disk
002500180312
002600180312      // - File di WORK totale
002700180312     fWFARA01L  uf a e           k disk
002800180308      //---------------------------------------------------------------
002900180308      //?Definizione costanti.
003000180308      //---------------------------------------------------------------
003100180308
003200180308      //---------------------------------------------------------------
003300180308      //?Definizione schiere.
003400180308      //---------------------------------------------------------------
003500180308      // - Sk Motivi apertura utilizzabili
003600180308     d skMAR           s              3    dim(999)
003700180308
003800180308      //---------------------------------------------------------------
003900180308      //?Definizione aree dati.
004000180308      //---------------------------------------------------------------
004100180308      // - Dati utente
004200180308     d §AzUte        e ds                  extname(AZUTE00F)
004300180308     d                                     dtaara
004400180308     d §DatiUte      e ds                  extname(dDatiUte)
004500180308     d                                     dtaara
004600180308
004700180308      //---------------------------------------------------------------
004800180308      //?Definizione strutture dati.
004900180308      //---------------------------------------------------------------
005000180308      // - Parametri ricevuti
005100180308     d KPJBA         e ds
005200180308
005300180308      // - Passaggio dati a pgm batch
005400180308     d FIDNE1DS      e ds                  inz
005500180308
005600180308      // - Reperimento dati utente
005700180308     d TIBS34DS      e ds
005800180308
005900180308      // - Reperimento filiali gestite dall'utente
006000180308     d TRUL31DS      e ds
006100180308     d  POG                   10    759    dim(250)
006200180308
006300180308      // - Tabella 05 - Aree
006400180308     d ds05          e ds
006500180312
006600180312      // - Tabella 17 - Distretti
006700180312     d ds17          e ds
006800180308
006900180308      // - Tabella MAR - Motivi Apertura
007000180308     d dMAR          e ds
007100180308
007200180308      // - Campo TASflo di TITAS
007300180308     d dTASflo       e ds
007400180308
007500180308      // - Ds File FITGD00F
007600180308     d FITGD00F      e ds                  extname(FITGD00F)
007700180312
007800180312      // - Ds File WFARA10F
007900180312     d WFARADS       e ds                  extname(WFARA10F)
008000180308
008100180308      //---------------------------------------------------------------
008200180308      //?Definizione variabili globali.
008300180308      //---------------------------------------------------------------
008400180308      // - Flags booleani
008500180308     d Alert           s               n   inz(*off)
008600180309     d ErrGrave        s               n   inz(*off)
008700180308     d Fine            s               n   inz(*off)
008800180308     d wEnd            s               n   inz(*off)
008900180308
009000180308      // - Indici di schiera
009100180308     d xx              s              4s 0 inz
009200180309
009300180309      // - Campi di comodo
009400180313     d DopoAff         s              4s 0 inz
009500180312     d DopoCons        s              4s 0 inz
009600180312     d DopoLavv        s              4s 0 inz
009700180312     d DopoManCons     s              4s 0 inz
009800180312     d DopoVarDcm      s              4s 0 inz
009900180313     d DtaOraAff       s             14s 0 inz
010000180313     d DtaOraCons      s             14s 0 inz
010100180313     d DtaOraLavv      s             14s 0 inz
010200180313     d DtaOraManCons   s             14s 0 inz
010300180313     d DtaOraTel       s             14s 0 inz
010400180313     d DtaOraVarDcm    s             14s 0 inz
010500180312     d Oggi            s              8s 0 inz
010600180314     d sav_ARA1mad     s                   like(ARA1mad) inz
010700180314     d sav_ARA1pot     s                   like(ARA1pot) inz
010800180309     d wabi            s              2    inz
010900180309     d warea           s              3s 0 inz
011000180308
011100180308      //---------------------------------------------------------------
011200180308      //?Definizione procedure usate.
011300180308      //---------------------------------------------------------------
011400180308      // - Caricamento Filiali in gestione
011500180308     d TRUL31R         pr                  extpgm('TRUL31R')
011600180308     d  kpjba                              likeds(kpjba)
011700180308     d  trul31ds                           likeds(trul31ds)
011800180308
011900180308      //---------------------------------------------------------------
012000180308      //?Definizione Prototipi.
012100180308      //---------------------------------------------------------------
012200180308      /copy gaitrasrc/srcprotopr,TIBS34R
012300180308
012400180308      //---------------------------------------------------------------
012500180308      //?Definizione key-list.
012600180308      //---------------------------------------------------------------
012700180308      // - File TABEL00F
012800180308     d k03tabel      e ds                  extname(TABEL00F:*key)
012900180308     d                                     prefix(k_)
013000180312
013100180312      // - File WFARA01L
013200180312     d kwfara        e ds                  extname(WFARA01L:*key)
013300180312     d                                     prefix(k_)
013400180312
013500180308      //---------------------------------------------------------------
013600180308      //?M A I N - L I N E
013700180308      //---------------------------------------------------------------
013800180308     c     *Entry        plist
013900180308     c                   parm                    kpjba
014000180308
014100180308      //?Operazioni iniziali
014200180308       exsr RoutInz;
014300180308
014400180308      //?Elaborazione
014500180308       DOW Fine = *off;
014600180308        //Pulisco file di WORK con stessa elaborazione
014700180308         exsr PulisciWorkFile;
014800180312        //Elaboro le RA a dettaglio
014900180312         exsr ElaboraDettaglio;
015000180312        //Elabora il file di WORK a dettaglio
015100180312         exsr ElaboraFileWork;
015200180312         Fine = wEnd;
015300180308       ENDDO;
015400180308
015500180308      //?Operazioni finali
015600180308       exsr RoutEnd;
015700180308
015800180308      //--------------------------------------------------------------
015900180308      //?Operazioni iniziali.
016000180308      //--------------------------------------------------------------
016100180308       BEGSR RoutInz;
016200180308
016300180308      //?Reperimento dati job
016400180308        exsr DatiJob;
016500180308
016600180308      //?Reperimento dati passati dal chiamante
016700180308        FIDNE1DS = kpjbu;
016800180308
016900180308      //?Impostazione campi "fissi"
017000180308        k_TBLkut = 1;
017100180308
017200180308      //?Imposto oggi
017300180308        Oggi = %dec(%date());
017400180308
017500180308      //Carico in sk i motivi di apertura utilizzabili
017600180308        clear xx;
017700180308        clear dMAR;
017800180308        TBEcod = 'MAR';
017900180308        setll (TBEcod) TNTBE01L;
018000180308        reade (TBEcod) TNTBE01L;
018100180308        DOW not %eof(TNTBE01L);
018200180308          IF TBEatb = *blanks;
018300180308            dMAR = TBEuni;
018400180308            IF §MARutap = *blanks;
018500180308              xx += 1;
018600180308              skMAR(xx) = %subst(TBEke1:1:3);
018700180308            ENDIF;
018800180308          ENDIF;
018900180308          reade (TBEcod) TNTBE01L;
019000180308        ENDDO;
019100180308
019200180308        clear wabi;
019300180309        clear warea;
019400180308      //Carico le filiali in base alla richiesta fatta nel lancio
019500180308        SELECT;
019600180308      //Se non ho nè area nè filiale carico tutte le fiali azienda
019700180308        WHEN DNE1area = *zeros and DNE1fil = *zeros;
019800180308          wabi = 'AZ';
019900180308      //Se ho area ma non ho la filiale carico tutte le fiali dell'area
020000180308        WHEN DNE1area > *zeros and DNE1fil = *zeros;
020100180308          wabi = 'RA';
020200180309          warea = DNE1area;
020300180308      //Se non ho l'area ma ho la filiale carico solo la filiale richiesta
020400180308        WHEN DNE1area = *zeros and DNE1fil > *zeros;
020500180308          pog(01) = %editc(DNE1fil:'X');
020600180308        ENDSL;
020700180308
020800180308      //?Carico le filiali abilitate all'utente
020900180308        IF wabi <> *blanks;
021000180308          clear TRUL31DS;
021100180308          I31abi = wabi;
021200180309          I31car = warea;
021300180308          TRUL31R (kpjba:trul31ds);
021400180308          IF O31pog <= *zeros;
021500180308            ErrGrave = *on;
021600180308            leavesr;
021700180308          ENDIF;
021800180308        ENDIF;
021900180308
022000180308        IF Errgrave;
022100180308          Fine = *on;
022200180308        ENDIF;
022300180308
022400180308       ENDSR;
022500180308
022600180308      //--------------------------------------------------------------
022700180308      //?Reperimento Dati del job (Utente/Operativi).
022800180308      //--------------------------------------------------------------
022900180308       BEGSR DatiJob;
023000180308
023100180308        in(E) §AzUte;
023200180308        IF NOT %error;
023300180308          in(E) §DatiUte;
023400180308        ENDIF;
023500180308        IF %error or RSut = *blanks;
023600180308          clear TIBS34ds;
023700180308          tibs34r(tibs34ds);
023800180308          in §AzUte;
023900180308          in §DatiUte;
024000180308        ENDIF;
024100180308
024200180308       ENDSR;
024300180308
024400180308      //--------------------------------------------------------------
024500180308      //?Pulisco file di WORK con stesso lancio.
024600180308      //--------------------------------------------------------------
024700180308       BEGSR PulisciWorkFile;
024800180308
024900180308        exec sql delete from WFARA00F
025000180308                 where ARAdtini = :DNE1dtini and
025100180308                       ARAdtfin = :DNE1dtfin and
025200180312                       ARAarear = :DNE1area  and
025300180312                       ARAfilr  = :DNE1fil   and
025400180308                       ARAuser  = :knmus;
025500180308
025600180308        exec sql delete from WFARA10F
025700180308                 where ARA1dtini = :DNE1dtini and
025800180308                       ARA1dtfin = :DNE1dtfin and
025900180308                       ARA1area  = :DNE1area  and
026000180308                       ARA1fil   = :DNE1fil   and
026100180308                       ARA1user  = :knmus;
026200180308
026300180308       ENDSR;
026400180308
026500180308      //--------------------------------------------------------------
026600180308      //Elaboro le RA.
026700180308      //--------------------------------------------------------------
026800180312       BEGSR ElaboraDettaglio;
026900180308
027000180308        wEnd = *off;
027100180308
027200180308      //Leggo file FITGD
027300180308        exec sql DECLARE TEL cursor for
027400180308                 SELECT * from FITGD00F
027500180309                 WHERE TGDdac between :DNE1dtini and :DNE1dtfin and
027600180308                       TGDtor = 'S' and TGDtch = 'D' and TGDtco = 'T';
027700180308
027800180308      //?Apertura del cursore
027900180308        exec sql OPEN TEL;
028000180308
028100180308        IF sqlcode < 0;
028200180308          wEnd = *on;
028300180308          exec sql CLOSE TEL;
028400180308          leavesr;
028500180308        ENDIF;
028600180308
028700180308        DOW not wEnd;
028800180308          exec sql FETCH NEXT from TEL into :FITGD00F;
028900180308          IF sqlcod = 100 or sqlcod < 0;
029000180308            wEnd = *on;
029100180308            leave;
029200180308          ENDIF;
029300180308
029400180308        //Prendo solo motivi di apertura validi
029500180308          IF %lookup(TGDmad:skMAR) = 0;
029600180308            iter;
029700180308          ENDIF;
029800180308
029900180308        //Prendo solo filiali richieste
030000180308          IF  %lookup(%editc(TGDpot:'X'):pog) = 0;
030100180308            iter;
030200180308          ENDIF;
030300180308
030400180308        //Solo spedizioni con Alert
030500180308          Alert = *off;
030600180308          exsr VerificaAlert;
030700180308          IF not Alert;
030800180308            iter;
030900180308          ENDIF;
031000180308
031100180308        //Scrivo file di WORK a dettaglio
031200180312          exsr WriteWFDettaglio;
031300180308
031400180308        ENDDO;
031500180312
031600180312        feod WFARA10F;
031700180312
031800180312      //?Chiudo il cursore
031900180312        exec sql CLOSE TEL;
032000180312
032100180312       ENDSR;
032200180312
032300180312      //--------------------------------------------------------------
032400180312      //Elaboro le RA.
032500180312      //--------------------------------------------------------------
032600180312       BEGSR ElaboraFileWork;
032700180312
032800180312        wEnd = *off;
032900180309
033000180312      //Leggo il file di WORK a dettaglio e conto
033100180312        exec sql DECLARE WRK cursor for
033200180312                 SELECT * from WFARA10F
033300180312                 WHERE ARA1dtini = :DNE1dtini and
033400180312                       ARA1dtfin = :DNE1dtfin and
033500180312                       ARA1area  = :DNE1area  and
033600180312                       ARA1fil   = :DNE1fil   and
033700180312                       ARA1user  = :knmus     and
033800180312                       ARA1d82 > 0
033900180313                 ORDER BY ARA1pot, ARA1mad, ARA1aas,
034000180313                          ARA1lnp, ARA1nrs, ARA1nsp;
034100180312
034200180312      //?Apertura del cursore
034300180312        exec sql OPEN WRK;
034400180312
034500180312        IF sqlcode < 0;
034600180312          wEnd = *on;
034700180312          exec sql CLOSE WRK;
034800180312          leavesr;
034900180312        ENDIF;
035000180312
035100180312        DOW not wEnd;
035200180312          exec sql FETCH NEXT from WRK into :WFARADS;
035300180312          IF sqlcod = 100 or sqlcod < 0;
035400180312            wEnd = *on;
035500180312            leave;
035600180312          ENDIF;
035700180314
035800180314        //A rottura di filiale
035900180314          IF ARA1pot <> sav_ARA1pot;
036000180314            IF sav_ARA1pot > 0;
036100180314              exsr WriteWFTotale;
036200180314            ENDIF;
036300180314            sav_ARA1pot = ARA1pot;
036400180314            sav_ARA1mad = ARA1mad;
036500180314          ENDIF;
036600180314        //A rottura di Causale Apertura
036700180314          IF ARA1mad <> sav_ARA1mad;
036800180314            IF sav_ARA1mad > *blanks;
036900180314              exsr WriteWFTotale;
037000180314            ENDIF;
037100180314            sav_ARA1pot = ARA1pot;
037200180314            sav_ARA1mad = ARA1mad;
037300180314          ENDIF;
037400180312
037500180312        //Data ora telefonata
037600180313          DtaOraTel = (ARA1dac * 1000000) + ARA1orc;
037700180313        //Data ora consegna
037800180313          DtaOraCons = (ARA1dcm * 1000000) + ARA1hmc;
037900180313        //Data ora Mancata Consegna
038000180313          DtaOraManCons = (ARA1d87 * 1000000) + ARA1h87;
038100180313        //Data ora Variazione Data Consegna
038200180313          DtaOraVarDcm = (ARA1d84 * 1000000) + ARA1h84;
038300180313        //Data ora Lasciato Avviso
038400180313          DtaOraLavv = (ARA1d81 * 1000000) + ARA1h81;
038500180313        //Data ora Affidamento Merce
038600180313          DtaOraAff = (ARA1d82 * 1000000) + ARA1h82;
038700180312
038800180313          SELECT;
038900180313        //Data e ora telefonata maggiore/uguale data e ora consegna
039000180313          WHEN DtaOraCons > 0 and DtaOraTel >= DtaOraCons;
039100180314            DopoCons += 1;
039200180313        //Data e ora telefonata è maggiore data e ora alert mancata consegna
039300180313          WHEN DtaOraManCons > 0 and DtaOraTel > DtaOraManCons;
039400180314            DopoManCons += 1;
039500180313        //Data e ora telefonata è maggiore data e ora alert variazione data consegna
039600180313          WHEN DtaOraVarDcm > 0 and DtaOraTel > DtaOraVarDcm;
039700180314            DopoVarDcm += 1;
039800180313        //Data e ora telefonata è maggiore data e ora alert lasciato avviso
039900180313          WHEN DtaOraLavv > 0 and DtaOraTel > DtaOraLavv;
040000180314            DopoLavv += 1;
040100180313        //Data e ora telefonata è maggiore data e ora alert affidamento merce
040200180313          WHEN DtaOraAff > 0 and DtaOraTel > DtaOraAff;
040300180314            DopoAff += 1;
040400180313          ENDSL;
040500180312
040600180312        ENDDO;
040700180312
040800180312      //?Chiudo il cursore
040900180312        exec sql CLOSE WRK;
041000180309
041100180308       ENDSR;
041200180308
041300180308      //--------------------------------------------------------------
041400180308      //Verifico se bolla con Alert.
041500180308      //--------------------------------------------------------------
041600180308       BEGSR VerificaAlert;
041700180308
041800180308        clear dTASflo;
041900180308        clear TASdcm;
042000180309        clear TAShmc;
042100180308
042200180308        IF TGDogg <> *blanks;
042300180308          TASaas = %dec(%subst(TGDogg:13:4):4:0);
042400180308          TASlnp = %dec(%subst(TGDogg:1:3):3:0);
042500180308          TASnrs = %dec(%subst(TGDogg:4:2):2:0);
042600180308          TASnsp = %dec(%subst(TGDogg:6:7):7:0);
042700180308
042800180309          chain (TASaas:TASlnp:TASnrs:TASnsp) TITAS30C;
042900180308          IF %found(TITAS30C);
043000180308            dTASflo = TASflo;
043100180308            IF §FLOemd <> *blanks;
043200180308              Alert = *on;
043300180308            ENDIF;
043400180308          ENDIF;
043500180308         ENDIF;
043600180308
043700180308       ENDSR;
043800180308
043900180308      //--------------------------------------------------------------
044000180308      //?Scrivo file di WORK a dettaglio.
044100180308      //--------------------------------------------------------------
044200180312       BEGSR WriteWFDettaglio;
044300180308
044400180309        clear WFARA100;
044500180309
044600180308        ARA1user = knmus;
044700180309        ARA1dtini = DNE1dtini;
044800180309        ARA1dtfin = DNE1dtfin;
044900180309        ARA1area = DNE1area;
045000180309        ARA1fil  = DNE1fil;
045100180309
045200180312        ARA1aas = TASaas;
045300180312        ARA1lnp = TASlnp;
045400180312        ARA1nrs = TASnrs;
045500180312        ARA1nsp = TASnsp;
045600180312
045700180312        ARA1pot = TGDpot;
045800180309        ARA1mad = TGDmad;
045900180309        ARA1dac = TGDdac;
046000180309        ARA1orc = TGDorc;
046100180309
046200180309        ARA1dsp = (TASaas * 10000)  + TASmgs;
046300180309        ARA1dcm = TASdcm;
046400180312        ARA1hmc = TAShmc * 100;
046500180309
046600180309      //Cerco data e ora immissione alert '82' Affidamento merce
046700180309        w_TGDtor = 'S';
046800180309        w_TGdogg = %editc(TASlnp:'X') + %editc(TASnrs:'X') +
046900180309                   %editc(TASnsp:'X') + %editc(TASaas:'X');
047000180309        w_TGDmad = ' 82';
047100180309        chain (w_TGDtor:w_TGDogg:w_TGDmad) FITGD11L;
047200180309        IF %found(FITGD11L);
047300180309          ARA1d82 = w_TGDdac;
047400180309          ARA1h82 = w_TGDorc;
047500180309        ENDIF;
047600180309
047700180309      //Cerco data e ora immissione alert '81' Tentata Consegna
047800180309        w_TGDmad = ' 81';
047900180309        chain (w_TGDtor:w_TGDogg:w_TGDmad) FITGD11L;
048000180309        IF %found(FITGD11L);
048100180309          ARA1d81 = w_TGDdac;
048200180309          ARA1h81 = w_TGDorc;
048300180309        ENDIF;
048400180309
048500180309      //Cerco data e ora immissione alert '84' Variazione data Consegna
048600180309        w_TGDmad = ' 84';
048700180309        chain (w_TGDtor:w_TGDogg:w_TGDmad) FITGD11L;
048800180309        IF %found(FITGD11L);
048900180309          ARA1d84 = w_TGDdac;
049000180309          ARA1h84 = w_TGDorc;
049100180309        ENDIF;
049200180309
049300180309      //Cerco data e ora immissione alert '87' Mancata Consegna
049400180309        w_TGDmad = ' 87';
049500180309        chain (w_TGDtor:w_TGDogg:w_TGDmad) FITGD11L;
049600180309        IF %found(FITGD11L);
049700180309          ARA1d87 = w_TGDdac;
049800180309          ARA1h87 = w_TGDorc;
049900180309        ENDIF;
050000180309
050100180309        write WFARA100;
050200180308
050300180308       ENDSR;
050400180314
050500180314      //--------------------------------------------------------------
050600180314      //?Scrivo file di WORK a totale.
050700180314      //--------------------------------------------------------------
050800180314       BEGSR WriteWFTotale;
050900180314
051000180314        clear WFARA000;
051100180314
051200180312      //Dati da file Work Dettaglio
051300180312        ARAuser  = ARA1user;
051400180312        ARAdtini = ARA1dtini;
051500180312        ARAdtfin = ARA1dtfin;
051600180312        ARAarear = ARA1area;
051700180312        ARAfilr  = ARA1fil;
051800180314        ARAfil   = sav_ARA1pot;
051900180314        ARAmad   = sav_ARA1mad;
052000180312
052100180312      //Dati da Organigramma
052200180312        chain (ARAfil) AZORG01L;
052300180312        IF %found(AZORG01L);
052400180312          ARAfild = ORGdes;
052500180312          ARAdist = ORGfl1;
052600180312          ARAarea = ORGcar;
052700180312        ENDIF;
052800180312
052900180312      //Cerco le descrizioni
053000180312        clear ds17;
053100180312        k_TBLcod = '17';
053200180312        k_TBLkey = ARAdist;
053300180312        chain %kds(k03tabel) TABEL00F;
053400180312        IF %found(TABEL00F) and TBLflg = *blanks;
053500180312          ds17 = TBLuni;
053600180312        ENDIF;
053700180312        ARAdistd = §17des;
053800180312
053900180312        clear ds05;
054000180312        k_TBLcod = '05';
054100180312        k_TBLkey = %editc(ARAarea:'X');
054200180312        chain %kds(k03tabel) TABEL00F;
054300180312        IF %found(TABEL00F) and TBLflg = *blanks;
054400180312          ds05 = TBLuni;
054500180312        ENDIF;
054600180312        ARAaread = §05des;
054700180312
054800180312        clear dMAR;
054900180312        TBEcod = 'MAR';
055000180312        TBEke1 = ARAmad;
055100180312        clear TBEke2;
055200180312        clear TBElin;
055300180312        chain (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
055400180312        IF %found(TNTBE01L) and TBEatb = *blanks;
055500180312          dMAR = TBEuni;
055600180312        ENDIF;
055700180312        ARAmadd = §MARdesc;
055800180314
055900180314        ARAra82 = DopoAff;
056000180314        ARAra81 = DopoLavv;
056100180314        ARAra84 = DopoVarDcm;
056200180314        ARAra87 = DopoManCons;
056300180314        ARAradcm = DopoCons;
056400180314        write WFARA000;
056500180314
056600180314        clear DopoAff;
056700180314        clear DopoLavv;
056800180314        clear DopoVarDcm;
056900180314        clear DopoManCons;
057000180314        clear DopoCons;
057100180314
057200180312
057300180312       ENDSR;
057400180308
057500180308      //--------------------------------------------------------------
057600180308      //?Operazioni finali.
057700180308      //--------------------------------------------------------------
057800180308       BEGSR RoutEnd;
057900180308
058000180308        *inLR = *on;
058100180308        return;
058200180308
058300180308       ENDSR;
058400180308
