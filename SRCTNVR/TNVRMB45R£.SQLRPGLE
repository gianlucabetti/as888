000100151123      //---------------------------------------------------------------
000200151123      //
000300160203      //?      Controllo addebito ORM Telefonico
000400151123      //
000500151123      //---------------------------------------------------------------
000600151123
000700151123     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000800151123
000900151123      //---------------------------------------------------------------
001000151123      //?Dichiarazione file.
001100151123      //---------------------------------------------------------------
001200160204      // -?Organigramma
001300160204     fAZORG01L  if   e           k disk
001400160204
001500160204      // -?Tabelle
001600160204     fTABEL00F  if   e           k disk
001700160204
001800160203      // -?File bolle
001900160203     fTITAS30C  if   e           k disk
002000151123
002100160203      // -?File estensione bolle x legame ORM
002200160203     fTITA432C  if   e           k disk
002300151123
002400160204      // -?File TOTALI ORM
002500160204     fWFORM     o    e             disk
002600160204
002700160204      // -?File Dettaglio bolle
002800160204     fWFTAS     o    e             disk    prefix(w_)
002900151123
003000151123      //---------------------------------------------------------------
003100151123      //?Definizione costanti.
003200151123      //---------------------------------------------------------------
003300151123
003400151123      //---------------------------------------------------------------
003500151123      //?Definizione schiere.
003600151123      //---------------------------------------------------------------
003700160204      // -?Bolle Assegnato
003800160204     d sk_TBa          s              2    dim(50)
003900160204
004000160204      // -?Bolle di Recupero
004100160204     d sk_TBr          s              2    dim(50)
004200151123
004300151123      //---------------------------------------------------------------
004400151123      //?Definizione aree dati.
004500151123      //---------------------------------------------------------------
004600151123
004700151123      //---------------------------------------------------------------
004800151123      //?Definizione strutture dati.
004900151123      //---------------------------------------------------------------
005000151123      // -?Parametri ricevuti?
005100151123     d kpjba         e ds
005200160204
005300160204      // -?Campo ORGde3 di AZORG00F
005400160204     d OG143         e ds                  inz
005500160204
005600160204      // -?Campo TASFLO di TITAS00F
005700160204     d dTASFLO       e ds                  inz
005800160204
005900160204      // -?Tabella TB
006000160204     d dsTB          e ds                  inz
006100151123
006200151123      // -?File FNORM00F
006300151123     d FNORM00F      e ds                  extname(FNORM00F)
006400151123
006500151123     d keyorm          ds
006600151123     d  poe                           3  0
006700151123     d  nsr                           2  0
006800151123     d  nor                           7  0
006900151123     d  nrv                           2  0
007000151123
007100151123      //---------------------------------------------------------------
007200151123      //?Definizione variabili globali.
007300151123      //---------------------------------------------------------------
007400151123
007500151123      // -?Flag booleani
007600151123     d wEoF            s               n   inz(*off)
007700160204     d wEoFTA4         s               n   inz(*off)
007800160204     d wEoFTAS         s               n   inz(*off)
007900160204     d wAddebito       s               n   inz(*off)
008000160204     d wSpedNoData     s               n   inz(*off)
008100160204
008200160204      // -?Contatori
008300160204     d xx              s              3s 0 inz
008400160204     d yy              s              3s 0 inz
008500151123
008600151123      // -?Campi di comodo
008700160204     d wContaAltro     s              9s 0 inz
008800160204     d wContaAss       s              9s 0 inz
008900160204     d wContaLnpExp    s              9s 0 inz
009000160204     d wContaNoData    s              9s 0 inz
009100160204     d wContaPrep      s              9s 0 inz
009200160204
009300160204     d wdataini        s              8s 0 inz
009400160204     d wdatafin        s              8s 0 inz
009500160204     d wdatasped       s              8s 0 inz
009600151123
009700151123      //---------------------------------------------------------------
009800151123      //?Definizione procedure esterne.
009900151123      //---------------------------------------------------------------
010000151123
010100151123      //---------------------------------------------------------------
010200151123      //?Definizione prototipi.
010300151123      //---------------------------------------------------------------
010400151123
010500151123      //---------------------------------------------------------------
010600151123      //?Definizione key-list.
010700151123      //---------------------------------------------------------------
010800151123
010900151123      //---------------------------------------------------------------
011000151123      //?Riepilogo indicatori.
011100151123      //---------------------------------------------------------------
011200151123
011300151123      //---------------------------------------------------------------
011400151123      //?M A I N - L I N E
011500151123      //---------------------------------------------------------------
011600151123
011700151123     c     *Entry        plist
011800151123     c                   parm                    kpjba
011900151123
012000151123      /free
012100151123
012200151123       //?Operazioni iniziali
012300151123       exsr RoutInz;
012400151123
012500151123       //?Elabora file
012600151123       exsr Elabora;
012700151123
012800151123       //?Operazioni finali
012900151123       exsr RoutEnd;
013000151123
013100151123       //--------------------------------------------------------------
013200151123       //?Operazioni iniziali.
013300151123       //--------------------------------------------------------------
013400151123       BEGSR RoutInz;
013500151123
013600151123       //?Impostazione opzioni per SQL?
013700151123         exec sql set option DynUsrPrf = *Owner,
013800151123                             CloSqlCsr = *EndMod;
013900151123
014000151123       //?Imposto le date
014100160203         wdataini = 20160101;
014200160203         wdatafin = 20160131;
014300160204
014400160204       //?Carico in sk le bolle di Recupero
014500160204         clear xx;
014600160204         clear yy;
014700160204         TBLkut = 1;
014800160204         TBLcod = 'TB';
014900160204         setll (TBLkut:TBLcod) TABEL00F;
015000160204         reade (TBLkut:TBLcod) TABEL00F;
015100160204         DOW not %eof(TABEL00F);
015200160204           dsTB = TBLuni;
015300160204           IF  §TBrbl = 'R';
015400160204             xx += 1;
015500160204             sk_TBr(xx) = TBLkey;
015600160204           ENDIF;
015700160204           IF  §TBtpo = 'A';
015800160204             yy += 1;
015900160204             sk_TBa(yy) = TBLkey;
016000160204           ENDIF;
016100160204           reade (TBLkut:TBLcod) TABEL00F;
016200160204         ENDDO;
016300151123
016400151123       ENDSR;
016500151123
016600151123       //--------------------------------------------------------------
016700151123       //?Elabora
016800151123       //--------------------------------------------------------------
016900151123       BEGSR Elabora;
017000151123
017100151123         wEoF = *off;
017200160204
017300160204         clear wform0;
017400160204         datada = wdataini;
017500160204         dataa  = wdatafin;
017600151123
017700151123       //?Leggo gli ORM
017800151123         exec sql
017900151123         DECLARE ORM cursor FOR
018000151123         SELECT *
018100151123         from FNORM00F
018200160203         WHERE ORMdar between :wdataini and :wdatafin
018300160203               and ORMtco in('M', 'E')
018400151123         ORDER BY ORMpoe, ORMnsr, ORMnor, ORMnrv;
018500151123
018600151123         exec sql
018700151123         open ORM;
018800151123         IF sqlcode < 0;
018900151123           wEoF = *on;
019000151123         ENDIF;
019100151123
019200151123         DOW not wEoF;
019300151123           exec sql
019400151123           FETCH NEXT from ORM into :FNORM00F;
019500151123           IF sqlcod = 100 or sqlcod < 0;
019600151123             wEoF = *on;
019700151123             leave;
019800151123           ENDIF;
019900151123
020000160204           clear wContaAltro;
020100160204           clear wContaAss;
020200160204           clear wContaLnpExp;
020300160204           clear wContaNoData;
020400160204           clear wContaPrep;
020500160204
020600160204           wAddebito = *off;
020700160204           wSpedNoData = *off;
020800160204
020900160204           totorm += 1;
021000160203
021100160203         //?Cerco se l'ORM ha almeno 1 bolla legata
021200160203           TA4trc = 'M';
021300160203           poe = ORMpoe;
021400160203           nsr = ORMnsr;
021500160203           nor = ORMnor;
021600160203           nrv = ORMnrv;
021700160204           setll (keyorm) TITA432C;
021800160204           IF  not %equal(TITA432C);
021900160204             nobolle += 1;
022000160203             iter;
022100160203           ENDIF;
022200160204
022300160204         //?Come prima cosa verifico se almeno 1 bolla legata all'ORM ha il flag
022400160204           exsr FlagOK;
022500160204         //?Se ok bolla con flag leggo altro ORM
022600160204           IF  wAddebito;
022700160204             bolleok += 1;
022800160204             iter;
022900160204           ENDIF;
023000151123
023100160204         //?Poi cerco i moti per cui le bolle non hanno il flag
023200160204           exsr FlagNO;
023300160205
023400160205
023500160204         //?Se solo 1 dei contatori è > 0 lo incasello nella colonna relativo
023600160204         //?Se no va nella casella di somma più flag
023700160204           SELECT;
023800160204           WHEN  wContaAltro  > 0 and wContaAss  > 0 and wContaLnpExp > 0 and
023900160204                 wContaNoData > 0 and wContaPrep > 0;
024000160204             piunoflg += 1;
024100160204           WHEN  wContaAltro  > 0 and wContaAss  > 0 and wContaLnpExp > 0 and
024200160204                 wContaNoData > 0;
024300160204             piunoflg += 1;
024400160204           WHEN  wContaAltro  > 0 and wContaAss  > 0 and wContaLnpExp > 0;
024500160204             piunoflg += 1;
024600160204           WHEN  wContaAltro  > 0 and wContaAss  > 0;
024700160204             piunoflg += 1;
024800160204           WHEN  wContaAltro > 0;
024900160204             altro += 1;
025000160204           WHEN  wContaAss > 0;
025100160204             assegnati += 1;
025200160205             IF  ORMpag = 'O';
025300160205               pagaordass += 1;
025400160205             ENDIF;
025500160208             IF  ORMpag = 'M';
025600160208               pagamitass += 1;
025700160208             ENDIF;
025800160208             IF  ORMpag = 'D';
025900160208               pagadesass += 1;
026000160208             ENDIF;
026100160208           //?Scrivo la bolla nel file di dettaglio
026200160208             clear WFTAS0;
026300160208             w_TASaas = TASaas;
026400160208             w_TASlnp = TASlnp;
026500160208             w_TASnrs = TASnrs;
026600160208             w_TASnsp = TASnsp;
026700160208             w_TAStbl = TAStbl;
026800160208             w_TAScbo = TAScbo;
026900160208             w_TASmgs = TASmgs;
027000160208             w_ORMpoe = ORMpoe;
027100160208             w_ORMnsr = ORMnsr;
027200160208             w_ORMnor = ORMnor;
027300160208             w_ORMnrv = ORMnrv;
027400160208             w_flag = 'C';
027500160208             write WFTAS0;
027600160204           WHEN  wContaLnpExp > 0;
027700160204             lnpexport += 1;
027800160204           WHEN  wContaNoData > 0;
027900160204             bollendata += 1;
028000160204           WHEN  wContaPrep > 0;
028100160204             prepagati += 1;
028200160204           ENDSL;
028300151123
028400151123         ENDDO;
028500160204
028600160204       //?Scrivo File a totale
028700160204         write WFORM0;
028800151123
028900151123       //?Chiudo il cursore
029000151123         exec sql
029100151123           close ORM;
029200151123
029300151123       ENDSR;
029400160204
029500160204       //--------------------------------------------------------------
029600160204       //?Controllo se almeno 1 bolla legata ha il flag.
029700160204       //--------------------------------------------------------------
029800160204       BEGSR FlagOK;
029900160204
030000160204       //?Leggo i rcd TA4 per ORM e controllo le bolle
030100160204         wEoFTA4 = *off;
030200160204         DOW  not wEoFTA4;
030300160204           reade (keyorm) TITA432C;
030400160204           IF  %eof(TITA432C);
030500160204             leave;
030600160204           ENDIF;
030700160204
030800160204         //?Controllo su TITAS se c'è il flag
030900160204           wEoFTAS = *off;
031000160204           setll (TA4aas:TA4lnp:TA4nrs:TA4nsp) TITAS30C;
031100160204           DOW  not wEoFTAS;
031200160204             reade (TA4aas:TA4lnp:TA4nrs:TA4nsp) TITAS30C;
031300160204             IF  %eof(TITAS30C);
031400160204               leave;
031500160204             ENDIF;
031600160204           //?NON leggo le bolle di recupero se ORM non annullato
031700160204             IF  ORMfao <> 999 and %lookup(TAStbl:sk_TBr) > 0;
031800160204               iter;
031900160204             ENDIF;
032000160204             wDataSped = TASaas * 10000 + TASmgs;
032100160204           //?Bolla non del periodo
032200160204             IF  wDataSped >= wdataini and wDataSped <= wdatafin;
032300160204               dTASflo = TASflo;
032400160204               IF  §FLOado = 'S';
032500160204                 wAddebito = *on;
032600160204                 leave;
032700160204               ENDIF;
032800160204             ENDIF;
032900160204           ENDDO;
033000160204         ENDDO;
033100160204
033200160204       ENDSR;
033300160204
033400160204       //--------------------------------------------------------------
033500160204       //?Controllo perchè le bolle non hanno il flag.
033600160204       //--------------------------------------------------------------
033700160204       BEGSR FlagNO;
033800160204
033900160204       //?Leggo i rcd TA4 per ORM e controllo le bolle
034000160204         wEoFTA4 = *off;
034100160204         setll (keyorm) TITA432C;
034200160204         DOW  not wEoFTA4;
034300160204           reade (keyorm) TITA432C;
034400160204           IF  %eof(TITA432C);
034500160204             leave;
034600160204           ENDIF;
034700160204
034800160204         //?Controllo su TITAS se c'è il flag
034900160204           wEoFTAS = *off;
035000160204           setll (TA4aas:TA4lnp:TA4nrs:TA4nsp) TITAS30C;
035100160204           DOW  not wEoFTAS;
035200160204             reade (TA4aas:TA4lnp:TA4nrs:TA4nsp) TITAS30C;
035300160204             IF  %eof(TITAS30C);
035400160204               leave;
035500160204             ENDIF;
035600160204           //?NON leggo le bolle di recupero se ORM non annullato
035700160204             IF  ORMfao <> 999 and %lookup(TAStbl:sk_TBr) > 0;
035800160204               iter;
035900160204             ENDIF;
036000160204             wDataSped = TASaas * 10000 + TASmgs;
036100160204           //?Bolla non del periodo
036200160204             IF  wDataSped > wdatafin or wDataSped < wdataini;
036300160204               wContaNoData +=1;
036400160204               wSpedNoData = *On;
036500160204               leave;
036600160204             ENDIF;
036700160204           ENDDO;
036800160204
036900160204         //?Se bolla non del periodo richiesto leggo altra bolla
037000160204           IF  wSpedNoData;
037100160204             iter;
037200160204           ENDIF;
037300160204
037400160204         //?Cerco il NTW della LNP
037500160204           clear OG143;
037600160204           chain TA4lnp AZORG01L;
037700160204           OG143 = ORGde3;
037800160204
037900160204         //?Bolla con LNP estera
038000160204           IF  §OGntw = 'EEX' or §OGntw = 'DPD' or
038100160204               §OGntw = 'FED';
038200160204             wContaLnpExp += 1;
038300160204             iter;
038400160204           ENDIF;
038500160204
038600160204         //?Bolla in assegnato
038700160204           IF  %lookup(TAStbl:sk_TBa) > 0;
038800160204             wContaAss += 1;
038900160204             iter;
039000160204           ENDIF;
039100160204
039200160204         //?Prepagato
039300160204           IF  %lookup (TAStbl:sk_Tba) = 0 and TASnft > 0 and
039400160204               TASfiv <> 998 and TASfiv <> 996;
039500160204             wContaPrep += 1;
039600160204             iter;
039700160204           ENDIF;
039800160204
039900160204         //?Nessuno dei casi precedenti
040000160204           wContaAltro += 1;
040100160204         //?Scrivo la bolla nel file di dettaglio
040200160204           clear WFTAS0;
040300160204           w_TASaas = TASaas;
040400160204           w_TASlnp = TASlnp;
040500160204           w_TASnrs = TASnrs;
040600160204           w_TASnsp = TASnsp;
040700160204           w_TAStbl = TAStbl;
040800160204           w_TAScbo = TAScbo;
040900160204           w_TASmgs = TASmgs;
041000160204           w_ORMpoe = ORMpoe;
041100160204           w_ORMnsr = ORMnsr;
041200160204           w_ORMnor = ORMnor;
041300160204           w_ORMnrv = ORMnrv;
041400160208           w_flag = 'A';
041500160204           write WFTAS0;
041600160204
041700160204         ENDDO;
041800160204
041900160204       ENDSR;
042000151123
042100151123       //--------------------------------------------------------------
042200151123       //?Operazioni finali.
042300151123       //--------------------------------------------------------------
042400151123       BEGSR RoutEnd;
042500151123
042600151123         *inLR = *on;
042700151123         return;
042800151123
042900151123       ENDSR;
043000151123
043100151123      /end-free
