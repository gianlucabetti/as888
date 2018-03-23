000100180308      //--------------------------------------------------------------
000200180308      //?FIDNE1R - ANALISI RA PER OBIETTIVI - Crea File Work
000300180308      //--------------------------------------------------------------
000400180308     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500180308     h dftactgrp(*no) actgrp(*caller)
000600180308
000700180308      //---------------------------------------------------------------
000800180308      //?Dichiarazione file.
000900180308      //---------------------------------------------------------------
001000180308      // - Organigramma
001100180308     fAZORG01L  if   e           k disk
001200180308
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
002400180309     fWFARA10F  o    e             disk
002500180308      //---------------------------------------------------------------
002600180308      //?Definizione costanti.
002700180308      //---------------------------------------------------------------
002800180308
002900180308      //---------------------------------------------------------------
003000180308      //?Definizione schiere.
003100180308      //---------------------------------------------------------------
003200180308      // - Sk Motivi apertura utilizzabili
003300180308     d skMAR           s              3    dim(999)
003400180308
003500180308      //---------------------------------------------------------------
003600180308      //?Definizione aree dati.
003700180308      //---------------------------------------------------------------
003800180308      // - Dati utente
003900180308     d §AzUte        e ds                  extname(AZUTE00F)
004000180308     d                                     dtaara
004100180308     d §DatiUte      e ds                  extname(dDatiUte)
004200180308     d                                     dtaara
004300180308
004400180308      //---------------------------------------------------------------
004500180308      //?Definizione strutture dati.
004600180308      //---------------------------------------------------------------
004700180308      // - Parametri ricevuti
004800180308     d KPJBA         e ds
004900180308
005000180308      // - Passaggio dati a pgm batch
005100180308     d FIDNE1DS      e ds                  inz
005200180308
005300180308      // - Reperimento dati utente
005400180308     d TIBS34DS      e ds
005500180308
005600180308      // - Reperimento filiali gestite dall'utente
005700180308     d TRUL31DS      e ds
005800180308     d  POG                   10    759    dim(250)
005900180308
006000180308      // - Tabella 05 - Aree
006100180308     d ds05          e ds
006200180308
006300180308      // - Tabella MAR - Motivi Apertura
006400180308     d dMAR          e ds
006500180308
006600180308      // - Campo TASflo di TITAS
006700180308     d dTASflo       e ds
006800180308
006900180308      // - Ds File FITGD00F
007000180308     d FITGD00F      e ds                  extname(FITGD00F)
007100180308
007200180308      //---------------------------------------------------------------
007300180308      //?Definizione variabili globali.
007400180308      //---------------------------------------------------------------
007500180308      // - Flags booleani
007600180308     d Alert           s               n   inz(*off)
007700180308     d Fine            s               n   inz(*off)
007800180308     d wEnd            s               n   inz(*off)
007900180308
008000180308      // - Indici di schiera
008100180308     d xx              s              4s 0 inz
008200180309
008300180309      // - Campi di comodo
008400180309     d oggi            s              8s 0 inz
008500180309     d wabi            s              2    inz
008600180308
008700180308      //---------------------------------------------------------------
008800180308      //?Definizione procedure usate.
008900180308      //---------------------------------------------------------------
009000180308      // - Caricamento Filiali in gestione
009100180308     d TRUL31R         pr                  extpgm('TRUL31R')
009200180308     d  kpjba                              likeds(kpjba)
009300180308     d  trul31ds                           likeds(trul31ds)
009400180308
009500180308      //---------------------------------------------------------------
009600180308      //?Definizione Prototipi.
009700180308      //---------------------------------------------------------------
009800180308      /copy gaitrasrc/srcprotopr,TIBS34R
009900180308
010000180308      //---------------------------------------------------------------
010100180308      //?Definizione key-list.
010200180308      //---------------------------------------------------------------
010300180308      // - File TABEL00F
010400180308     d k03tabel      e ds                  extname(TABEL00F:*key)
010500180308     d                                     prefix(k_)
010600180308      //---------------------------------------------------------------
010700180308      //?M A I N - L I N E
010800180308      //---------------------------------------------------------------
010900180308     c     *Entry        plist
011000180308     c                   parm                    kpjba
011100180308
011200180308      //?Operazioni iniziali
011300180308       exsr RoutInz;
011400180308
011500180308      //?Elaborazione
011600180308       DOW Fine = *off;
011700180308        //Pulisco file di WORK con stessa elaborazione
011800180308         exsr PulisciWorkFile;
011900180308        //Elaboro le RA
012000180308         exsr Elabora;
012100180308       ENDDO;
012200180308
012300180308      //?Operazioni finali
012400180308       exsr RoutEnd;
012500180308
012600180308      //--------------------------------------------------------------
012700180308      //?Operazioni iniziali.
012800180308      //--------------------------------------------------------------
012900180308       BEGSR RoutInz;
013000180308
013100180308      //?Reperimento dati job
013200180308        exsr DatiJob;
013300180308
013400180308      //?Reperimento dati passati dal chiamante
013500180308        FIDNE1DS = kpjbu;
013600180308
013700180308      //?Impostazione campi "fissi"
013800180308        k_TBLkut = 1;
013900180308
014000180308      //?Imposto oggi
014100180308        Oggi = %dec(%date());
014200180308
014300180308      //Carico in sk i motivi di apertura utilizzabili
014400180308        clear xx;
014500180308        clear dMAR;
014600180308        TBEcod = 'MAR';
014700180308        setll (TBEcod) TNTBE01L;
014800180308        reade (TBEcod) TNTBE01L;
014900180308        DOW not %eof(TNTBE01L);
015000180308          IF TBEatb = *blanks;
015100180308            dMAR = TBEuni;
015200180308            IF §MARutap = *blanks;
015300180308              xx += 1;
015400180308              skMAR(xx) = %subst(TBEke1:1:3);
015500180308            ENDIF;
015600180308          ENDIF;
015700180308          reade (TBEcod) TNTBE01L;
015800180308        ENDDO;
015900180308
016000180308        clear wabi;
016100180308      //Carico le filiali in base alla richiesta fatta nel lancio
016200180308        SELECT;
016300180308      //Se non ho nè area nè filiale carico tutte le fiali azienda
016400180308        WHEN DNE1area = *zeros and DNE1fil = *zeros;
016500180308          wabi = 'AZ';
016600180308      //Se ho area ma non ho la filiale carico tutte le fiali dell'area
016700180308        WHEN DNE1area > *zeros and DNE1fil = *zeros;
016800180308          wabi = 'RA';
016900180308      //Se non ho l'area ma ho la filiale carico solo la filiale richiesta
017000180308        WHEN DNE1area = *zeros and DNE1fil > *zeros;
017100180308          pog(01) = %editc(DNE1fil:'X');
017200180308        ENDSL;
017300180308
017400180308      //?Carico le filiali abilitate all'utente
017500180308        IF wabi <> *blanks;
017600180308          clear TRUL31DS;
017700180308          I31abi = wabi;
017800180308          I31cdi = DUTdis;
017900180308          I31car = DUTare;
018000180308          I31cpo = DUTpou;
018100180308          TRUL31R (kpjba:trul31ds);
018200180308          IF O31pog <= *zeros;
018300180308            ErrGrave = *on;
018400180308            leavesr;
018500180308          ENDIF;
018600180308        ENDIF;
018700180308
018800180308        IF Errgrave;
018900180308          Fine = *on;
019000180308        ENDIF;
019100180308
019200180308       ENDSR;
019300180308
019400180308      //--------------------------------------------------------------
019500180308      //?Reperimento Dati del job (Utente/Operativi).
019600180308      //--------------------------------------------------------------
019700180308       BEGSR DatiJob;
019800180308
019900180308        in(E) §AzUte;
020000180308        IF NOT %error;
020100180308          in(E) §DatiUte;
020200180308        ENDIF;
020300180308        IF %error or RSut = *blanks;
020400180308          clear TIBS34ds;
020500180308          tibs34r(tibs34ds);
020600180308          in §AzUte;
020700180308          in §DatiUte;
020800180308        ENDIF;
020900180308
021000180308       ENDSR;
021100180308
021200180308      //--------------------------------------------------------------
021300180308      //?Pulisco file di WORK con stesso lancio.
021400180308      //--------------------------------------------------------------
021500180308       BEGSR PulisciWorkFile;
021600180308
021700180308        exec sql delete from WFARA00F
021800180308                 where ARAdtini = :DNE1dtini and
021900180308                       ARAdtfin = :DNE1dtfin and
022000180308                       ARAarea  = :DNE1area  and
022100180308                       ARAfil   = :DNE1fil   and
022200180308                       ARAuser  = :knmus;
022300180308
022400180308        exec sql delete from WFARA10F
022500180308                 where ARA1dtini = :DNE1dtini and
022600180308                       ARA1dtfin = :DNE1dtfin and
022700180308                       ARA1area  = :DNE1area  and
022800180308                       ARA1fil   = :DNE1fil   and
022900180308                       ARA1user  = :knmus;
023000180308
023100180308       ENDSR;
023200180308
023300180308      //--------------------------------------------------------------
023400180308      //Elaboro le RA.
023500180308      //--------------------------------------------------------------
023600180309       BEGSR Elabora;
023700180308
023800180308        wEnd = *off;
023900180308
024000180308      //Leggo file FITGD
024100180308        exec sql DECLARE TEL cursor for
024200180308                 SELECT * from FITGD00F
024300180309                 WHERE TGDdac between :DNE1dtini and :DNE1dtfin and
024400180308                       TGDtor = 'S' and TGDtch = 'D' and TGDtco = 'T';
024500180308
024600180308      //?Apertura del cursore
024700180308        exec sql OPEN TEL;
024800180308
024900180308        IF sqlcode < 0;
025000180308          wEnd = *on;
025100180308          exec sql CLOSE TEL;
025200180308          leavesr;
025300180308        ENDIF;
025400180308
025500180308        DOW not wEnd;
025600180308          exec sql FETCH NEXT from TEL into :FITGD00F;
025700180308          IF sqlcod = 100 or sqlcod < 0;
025800180308            wEnd = *on;
025900180308            leave;
026000180308          ENDIF;
026100180308
026200180308        //Prendo solo motivi di apertura validi
026300180308          IF %lookup(TGDmad:skMAR) = 0;
026400180308            iter;
026500180308          ENDIF;
026600180308
026700180308        //Prendo solo filiali richieste
026800180308          IF  %lookup(%editc(TGDpot:'X'):pog) = 0;
026900180308            iter;
027000180308          ENDIF;
027100180308
027200180308        //Solo spedizioni con Alert
027300180308          Alert = *off;
027400180308          exsr VerificaAlert;
027500180308          IF not Alert;
027600180308            iter;
027700180308          ENDIF;
027800180308
027900180308        //Scrivo file di WORK a dettaglio
028000180308          exsr writeWFdettaglio;
028100180308
028200180308        ENDDO;
028300180308
028400180308       ENDSR;
028500180308
028600180308      //--------------------------------------------------------------
028700180308      //Verifico se bolla con Alert.
028800180308      //--------------------------------------------------------------
028900180308       BEGSR VerificaAlert;
029000180308
029100180308        clear dTASflo;
029200180308        clear TASdcm;
029300180309        clear TAShmc;
029400180308
029500180308        IF TGDogg <> *blanks;
029600180308          TASaas = %dec(%subst(TGDogg:13:4):4:0);
029700180308          TASlnp = %dec(%subst(TGDogg:1:3):3:0);
029800180308          TASnrs = %dec(%subst(TGDogg:4:2):2:0);
029900180308          TASnsp = %dec(%subst(TGDogg:6:7):7:0);
030000180308
030100180309          chain (TASaas:TASlnp:TASnrs:TASnsp) TITAS30C;
030200180308          IF %found(TITAS30C);
030300180308            dTASflo = TASflo;
030400180308            IF §FLOemd <> *blanks;
030500180308              Alert = *on;
030600180308            ENDIF;
030700180308          ENDIF;
030800180308         ENDIF;
030900180308
031000180308       ENDSR;
031100180308
031200180308      //--------------------------------------------------------------
031300180308      //?Scrivo file di WORK a dettaglio.
031400180308      //--------------------------------------------------------------
031500180308       BEGSR writeWFdettaglio;
031600180308
031700180309        clear WFARA10F;
031800180309
031900180308        ARA1user = knmus;
032000180309        ARA1dtini = DNE1dtini;
032100180309        ARA1dtfin = DTE1dtfin;
032200180309        ARA1dist = ORGfl1;
032300180309        ARA1area = ORGcar;
032400180309
032500180309        ARA1fil = TGDpot;
032600180309        ARA1mad = TGDmad;
032700180309        ARA1dac = TGDdac;
032800180309        ARA1orc = TGDorc;
032900180309
033000180309        ARA1dsp = (TASaas * 10000)  + TASmgs;
033100180309        ARA1dcm = TASdcm;
033200180309        ARA1hmc = TAShmc;
033300180309
033400180309      //Cerco data e ora immissione alert '82' Affidamento merce
033500180309        w_TGDtor = 'S';
033600180309        w_TGdogg = %editc(TASlnp:'X') + %editc(TASnrs:'X') +
033700180309                   %editc(TASnsp:'X') + %editc(TASaas:'X');
033800180309        w_TGDmad = ' 82';
033900180309        chain (w_TGDtor:w_TGDogg:w_TGDmad) FITGD11L;
034000180309        IF %found(FITGD11L);
034100180309          ARA1d82 = w_TGDdac;
034200180309          ARA1h82 = w_TGDorc;
034300180309        ENDIF;
034400180309
034500180309      //Cerco data e ora immissione alert '81' Tentata Consegna
034600180309        w_TGDmad = ' 81';
034700180309        chain (w_TGDtor:w_TGDogg:w_TGDmad) FITGD11L;
034800180309        IF %found(FITGD11L);
034900180309          ARA1d81 = w_TGDdac;
035000180309          ARA1h81 = w_TGDorc;
035100180309        ENDIF;
035200180309
035300180309      //Cerco data e ora immissione alert '84' Variazione data Consegna
035400180309        w_TGDmad = ' 84';
035500180309        chain (w_TGDtor:w_TGDogg:w_TGDmad) FITGD11L;
035600180309        IF %found(FITGD11L);
035700180309          ARA1d84 = w_TGDdac;
035800180309          ARA1h84 = w_TGDorc;
035900180309        ENDIF;
036000180309
036100180309      //Cerco data e ora immissione alert '87' Mancata Consegna
036200180309        w_TGDmad = ' 87';
036300180309        chain (w_TGDtor:w_TGDogg:w_TGDmad) FITGD11L;
036400180309        IF %found(FITGD11L);
036500180309          ARA1d87 = w_TGDdac;
036600180309          ARA1h87 = w_TGDorc;
036700180309        ENDIF;
036800180309
036900180309        write WFARA100;
037000180308
037100180308       ENDSR;
037200180308
037300180308      //--------------------------------------------------------------
037400180308      //?Operazioni finali.
037500180308      //--------------------------------------------------------------
037600180308       BEGSR RoutEnd;
037700180308
037800180308        *inLR = *on;
037900180308        return;
038000180308
038100180308       ENDSR;
038200180308
