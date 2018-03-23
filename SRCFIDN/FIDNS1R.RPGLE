000100090812      //---------------------------------------------------------------
000200090812      //?FIDNS1R - Statistica danni per cliente
000300090812      //---------------------------------------------------------------
000400090812     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500090812
000600090812      //---------------------------------------------------------------
000700090812      //?Fasi C.A. fisse a programma.
000800090812      //---------------------------------------------------------------
000900090813      // 100 = Conferma C.A.
001000090812
001100090812      //---------------------------------------------------------------
001200090812      //?Dichiarazione file.
001300090812      //---------------------------------------------------------------
001400090812     ftitas38c  if   e           k disk
001500090812     ffndct02l  if   e           k disk
001600090812     ffndcr01l  if   e           k disk
001700090812     ffndcl01l  if   e           k disk
001800090812     ftntbe01l  if   e           k disk
001900090812     ftabel00f  if   e           k disk
002000090812     fazorg01l  if   e           k disk
002100090812     fwfdan11l  uf a e           k disk    rename(wfdan100:wfdan11)
002200090813     fwfdan12l  uf   e           k disk    rename(wfdan100:wfdan12)
002300090812     fwfdan10f  o    e           k disk    prefix(w_)
002400090812
002500090812      //---------------------------------------------------------------
002600090812      //?Definizione costanti.
002700090812      //---------------------------------------------------------------
002800090813     d wFCA            c                   const(100)
002900090812
003000090812      //---------------------------------------------------------------
003100090812      //?Definizione schiere.
003200090812      //---------------------------------------------------------------
003300090813     d $CCH            s                   like(DCTcch)  dim(99)  inz
003400090813     d $CCHD           s                   like(dCCH)    dim(99)  inz
003500090813     d $TAD            s                   like(DCTtad)  dim(99)  inz
003600090813     d $TADD           s                   like(dTAD)    dim(99)  inz
003700090813     d $TB             s                   like(TAStbl)  dim(99)  inz
003800090812
003900090812      //---------------------------------------------------------------
004000090812      //?Definizione aree dati.
004100090812      //---------------------------------------------------------------
004200020311
004300090812      // - Parametri ricevuti
004400090813     d kpjba         e ds
004500090813     d FIDNS1ds      e ds                  inz
004600090812
004700090812      // - Parametri per pgm. TIBS69R (decodifica cliente)
004800090813     d TIBS69ds      e ds                  inz
004900090813     d ds_CNACO      e ds                  extname(CNACO00F) inz
005000090813     d ds_CNIND      e ds                  extname(CNIND00F) inz
005100090813     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
005200090813     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
005300090813     d p_CNACOds     e ds                  extname(CNACO00F) inz
005400090813     d                                     prefix(p_)
005500090812
005600090812      // - Parametri per pgm. TIBS10R (reperimento codice padre cliente)
005700090813     d TIBS10ds      e ds                  inz
005800090813     d  skSC11                21   5520  0 dim(500)
005900090812
006000090812      // - Parametri per pgm. TISE70R (Calcolo DELTA CLIENTE, spedizioni, colli e fatturato)
006100090813     d TISE70ds      e ds                  inz
006200090812
006300090812      // - Flag Operativi dal file FNDCT00F
006400090813     d dDCT01        e ds                  inz
006500090812
006600090812      // - Tabella CCH = Causali Chiusura C.A.
006700090813     d dCCH          e ds                  inz
006800090812
006900090812      // - Tabella TAD = Tipi Anomalie Danni
007000090813     d dTAD          e ds                  inz
007100090812
007200090812      // - Tabella TB  = Tipi Bolla
007300090813     d dsTB          e ds                  inz
007400090812
007500090812      // - Tabella 05  = Codici Area
007600090813     d ds05          e ds                  inz
007700090812
007800090812      // - Tabella 17  = Codici Ditretto
007900090813     d ds17          e ds                  inz
008000090812
008100090812      // - Ridefinizione dei trk x comodo
008200090813     d FNDCLds       e ds                  extname(FNDCL00F) inz
008300090812
008400090812      // - Parametri per controllo ed inversione data - XSRDA8 -
008500090813     d wlbdat          ds
008600090813     d  G02dat                 1      8  0
008700090813     d  G02inv                 9     16  0
008800090813     d  G02err                17     17
008900090813     d  G02tgi                18     22  0
009000090812
009100090812      // - Ridefinizione dei campi per avere la possibilità di fare una
009200090812      //   unica MOVEL comulativa (non campo per campo)
009300090813     d AZORGds         ds                  inz
009400090813     d  ORGdit
009500090813     d  ORGfva
009600090813     d  ORGfag
009700090813     d  ORGcts
009800090813     d  ORGcaa
009900090813     d  ORGfl1
010000090813     d  ORGfl2
010100090813     d  ORGfl3
010200090813     d  ORGdes
010300090813     d  ORGfel
010400090813     d  ORGcar
010500090813     d  ORGcre
010600090812
010700090812      // - Ridefinizione data in formato AAAA/MM/GG da file TITAS00F
010800090813     d AAAAMMGG_f      ds                  inz
010900090813     d  TASaas                             inz
011000090813     d  TASmgs                             inz
011100090812
011200090812      // - Ridefinizione data in formato AAAA/MM/GG da parametri
011300090813     d AAAAMMGG_p      ds                  inz
011400090813     d  p_AAAAMM                           like(IS1dfp) inz
011500090813     d  p_GG                          2  0 inz(*hival)
011600090812
011700090812      //---------------------------------------------------------------
011800090812      //?Definizione variabili globali.
011900090812      //---------------------------------------------------------------
012000090812
012100090812      // - Flags booleani
012200090813     d $OK             s               n   inz(*off)
012300090813     d $Resp           s               n   inz(*off)
012400090812
012500090812      // - Campi di comodo
012600090813     d kDANdfp         S                   like(DANdfp) inz
012700090813     d kDANdip         S                   like(DANdip) inz
012800090813     d xx              S              3  0 inz
012900090813     d yy              S              3  0 inz
013000090813     d wCCH            S                   like(DCTcch) inz
013100090813     d wDANare         s                   like(DANare)
013200090813     d wDANdar         s                   like(DANdar)
013300090813     d wDANddv         s                   like(DANddv)
013400090813     d wDANdiv         s                   like(DANdiv)
013500090813     d wDANdpr         s                   like(DANdpr)
013600090813     d wDANkun         S                   like(DANkun) inz
013700090813     d wDANpor         s                   like(DANpor)
013800090813     d wDANura         s                   like(DANura)
013900090813     d wDATE           S              8  0 inz
014000090813     d wDATA_f         S              8  0 inz
014100090813     D wDATA_p         S              8  0 inz
014200090813     d wELEr           S              3  0 inz
014300090813     d wELEt           S              3  0 inz
014400090812     D wIMP            S                   like(DANfat) inz
014500090812     D wIMPMR          S                   like(DANfat) inz
014600090813     d wKSC            s                   like(DANksc)
014700090813     d wNCLMR          s              9  2 inz
014800090813     d wORA            s              6  0 inz
014900090813     d wPor            s                   like(ORGfil) inz
015000020312     D wTAD            S                   like(DCTtad) inz
015100020312     D wTIR            S                   like(DANtir) inz
015200020312     D wTIT            S                   like(DANtit) inz
015300090813     D w0110           S             11  0 inz
015400090812
015500090812      //---------------------------------------------------------------
015600090812      //?Definizione procedure esterne.
015700090812      //---------------------------------------------------------------
015800090813
015900090813      // - Richiamo pgm per calcolo fatturato
016000090813     d tise70r         pr                  extpgm('TISE70R')
016100090813     d  tise70ds                           likeds(TISE70ds)
016200090813
016300090813      /copy gaitrasrc/srcprotopr,tibs69r
016400090813      /copy gaitrasrc/srcprotopr,tibs10r
016500090812
016600090812      //---------------------------------------------------------------
016700090812      //?Riepilogo indicatori.
016800090812      //---------------------------------------------------------------
016900090812
017000090812      //---------------------------------------------------------------
017100090812      //?M A I N - L I N E
017200090812      //---------------------------------------------------------------
017300090812
017400090812     c     *Entry        plist
017500090812     c                   parm                    KPJBA
017600090812
017700090812      /free
017800090812
017900090813        FIDNS1ds = kpjbu;
018000090812
018100090812       //?Operazioni iniziali
018200090813        exsr sr_RoutInz;
018300090812
018400090813       //?Posizionamento iniziale su TITAS38C
018500090813        TASaas = IS1dip / 100;
018600090813        TASmgs = %dec(%subst(%editc(IS1dip:'X'):5:2):4:0) * 100;
018700090813        TASksc = *loval;
018800090813
018900090813        setll (TASaas:TASmgs:TASksc) TITAS38C;
019000090813        read  TITAS38C;
019100090813
019200090813        DOW  not %Eof(TITAS38C);
019300090813          wDATA_f = %dec(AAAAMMGG_f:8:0);
019400090813          IF  wDATA_f > wDATA_p;
019500090813            leave;
019600090813          ENDIF;
019700090813
019800090813       //?Selezione rcd validi
019900090813          exsr srSELEZ;
020000090813
020100090813       //?Elaborazione
020200090813          IF  $OK;
020300090813            exsr srWFDAN;
020400090813          ENDIF;
020500090813
020600090813        read  TITAS38C;
020700090813
020800090813        ENDDO;
020900090813
021000090813       //?Imposto il fatturato per cliente
021100090813       //?anche dei clienti legati all'unificante che non hanno C.A.
021200090813        exsr srRIEPILOGO;
021300090813
021400090813       //?Operazioni finali
021500090813        exsr sr_RoutEnd;
021600090812
021700090812       //--------------------------------------------------------------
021800090812       //?Operazioni iniziali.
021900090812       //--------------------------------------------------------------
022000090812       BEGSR sr_RoutInz;
022100090812
022200090812         //?imposto la data del giorno
022300090813          wDATE = %dec(%date());
022400090812
022500090812         //?imposto l'ora del batch
022600090813          wORA = %dec(%time());
022700090812
022800090812         //?imposto data finale da elaborare work
022900090813          reset AAAAMMGG_p;
023000090813          p_AAAAMM = IS1dfp;
023100090813          wDATA_p = %dec(AAAAMMGG_p:8:0);
023200090812
023300090812         //?memorizzo tab CCH in sk
023400090812          clear xx;
023500090813          clear $CCH;
023600090813          clear $CCHD;
023700090813          TBEcod = 'CCH';
023800090813
023900090813          setll TBEcod TNTBE01L;
024000090813          reade TBEcod TNTBE01L;
024100090813
024200090813          DOW  not %Eof (TNTBE01L);
024300090813            IF  TBEatb = *blanks;
024400090813              dCCH = TBEuni;
024500090813              IF  §CCHchte = 'S';
024600090813                xx += 1;
024700090813                $CCH(xx) = TBEke1;
024800090814                $CCHD(xx) = TBEuni;
024900090813              ENDIF;
025000090813            ENDIF;
025100090813
025200090813            reade TBEcod TNTBE01L;
025300090813
025400090813          ENDDO;
025500090812
025600090812         //?memorizzo tab TAD in sk
025700090812          clear xx;
025800090813          clear $TAD;
025900090813          clear $TADD;
026000090813          TBEcod = 'TAD';
026100090813
026200090813          setll TBEcod TNTBE01L;
026300090813          reade TBEcod TNTBE01L;
026400090813
026500090813          DOW  not %Eof (TNTBE01L);
026600090813            IF  TBEatb = *blanks;
026700090813              dTAD = TBEuni;
026800090813              IF  §TADragr = 'M' or
026900090813                  §TADragr = 'A' or
027000090813                  §TADragr = 'V';
027100090813                xx += 1;
027200090813                $TAD(xx) = TBEke1;
027300090814                $TADD(xx) = TBEuni;
027400090813              ENDIF;
027500090813            ENDIF;
027600090813
027700090813            reade TBEcod TNTBE01L;
027800090813
027900090813          ENDDO;
028000090812
028100090812         //?memorizzo tab TB in sk
028200090812          clear xx;
028300090813          clear $TB;
028400090813          TBLkut = 1;
028500090813          TBLcod = 'TB';
028600090813
028700090813          setll (TBLkut:TBLcod) TABEL00F;
028800090813          reade (TBLkut:TBLcod) TABEL00F;
028900090813
029000090813          DOW  not %Eof (TABEL00F);
029100090813            IF  TBLflg = *blanks;
029200090813              dsTB = TBLuni;
029300090813              IF  §TBrbl <> 'R';
029400090813                xx += 1;
029500090813                $TB(xx) = TBLkey;
029600090813              ENDIF;
029700090813            ENDIF;
029800090813
029900090813            reade (TBLkut:TBLcod) TABEL00F;
030000090813
030100090813          ENDDO;
030200090812
030300090812       ENDSR;
030400090813
030500090813       //--------------------------------------------------------------
030600090813       //?Selezione record validi.
030700090813       //--------------------------------------------------------------
030800090813       BEGSR srSELEZ;
030900090813
031000090813         $OK = *off;
031100090813
031200090813         //?solo bolle con pratica di danno
031300090813         IF  TASfda <> 'S';
031400090813           leavesr;
031500090813         ENDIF;
031600090813
031700090813         //?tipo bolla valido
031800090813         IF  %lookup (TAStbl : $TB) = 0;
031900090813           leavesr;
032000090813         ENDIF;
032100090813
032200090813         //?controllo se c'è C.A.
032300090813         DCTaas = TASaas;
032400090813         DCTlnp = TASlnp;
032500090813         DCTnrs = TASnrs;
032600090813         DCTnsp = TASnsp;
032700090813         chain (DCTaas:DCTlnp:DCTnrs:DCTnsp) FNDCT02L;
032800090813
032900090813         //?C.A. non trovata o annullata
033000090813         IF  not %Found(FNDCT02L) or DCTatb = 'A' or
033100090813         //?C.A. in fase non consentita
033200090813           DCTfca < wFCA or
033300090813         //?C.A. aperta e richiesta elaborazione delle sole C.A. chiuse
033400090813           (DCTdch = 0 and IS1tca = 'C');
033500090813           leavesr;
033600090813         ENDIF;
033700090813
033800090813         //?controllo tipo anomalia valida e recupero dati
033900090813         xx = %lookup (DCTtad : $TAD);
034000090813         IF xx = 0;
034100090813           leavesr;
034200090813         ELSE;
034300090813           dTAD = $TADD(xx);
034400090813         ENDIF;
034500090813
034600090813         //?recupero dati causali di chiusura
034700090813         xx = %lookup (DCTcch : $CCH);
034800090813         IF xx = 0;
034900090813           clear dCCH;
035000090813         ELSE;
035100090813           dCCH = $CCHD(xx);
035200090813         ENDIF;
035300090813
035400090813         //?controllo tipo gestione
035500090813         dDCT01 = DCTflo;
035600090813         SELECT;
035700090813           WHEN  DCTfpr = 'P' and §DCTpfra <> 'F';
035800090813           WHEN  DCTfpr = 'P' and §DCTpfra = 'F';
035900090813           WHEN  DCTfpr = 'T';
036000090813           OTHER;
036100090813             leavesr;
036200090813         ENDSL;
036300090813
036400090813         //?controllo se c'è almeno un rcd valido su FNDCR (responsabilità)
036500090813          $Resp = *off;
036600090813
036700090813          setll (DCTaac:DCTfil:DCTnca) FNDCR01L;
036800090813          reade (DCTaac:DCTfil:DCTnca) FNDCR01L;
036900090813
037000090813          DOW not %Eof(FNDCR01L);
037100090813
037200090813            IF  DCRatb <> 'A';
037300090813              $Resp = *on;
037400090813              leave;
037500090813            ENDIF;
037600090813
037700090813            reade (DCTaac:DCTfil:DCTnca) FNDCR01L;
037800090813
037900090813          ENDDO;
038000090813
038100090813          IF not $Resp;
038200090813            leavesr;
038300090813          ENDIF;
038400090813
038500090813         //?controllo se c'è FNDCL (importi)
038600090813          chain (DCTaac:DCTfil:DCTnca) FNDCL01L;
038700090813          IF  not %Found(FNDCL01L) or DCLatb = 'A';
038800090814            clear FNDCLds;
038900090813          ENDIF;
039000090813
039100090813         //?Tutto ok rcd valido
039200090813          $OK = *on;
039300090813
039400090813       ENDSR;
039500090813
039600090813       //--------------------------------------------------------------
039700090813       //?Elaborazione.
039800090813       //--------------------------------------------------------------
039900090813       BEGSR srWFDAN;
040000090813
040100090813         clear WFDAN11;
040200090813
040300090813         DANdata = wDATE;
040400090813         DANora = wORA;
040500090813         DANpru = knmus;
040600090813         DANdip = IS1dip;
040700090813         DANdfp = IS1dfp;
040800090813         DANksc = TASksc;
040900090813
041000090813         chain (DANdip:DANdfp:DANksc) WFDAN11L;
041100090813         IF not %Found(WFDAN11L);
041200090813
041300090813           exsr srDECOD;
041400090813
041500090813           DANrag = ACOrag;
041600090813           DANkun = D10cop;
041700090813           DANura = p_ACOrag;
041800090813           DANpor = wPOR;
041900090813           DANdpr = ORGdes;
042000090813           DANdiv = ORGfl3;
042100090813           DANddv = §17des;
042200090813           DANare = ORGcar;
042300090813           DANdar = §05des;
042400090813
042500090813           exsr srCONTA;
042600090813
042700090813           write WFDAN11;
042800090813
042900090813         ELSE;
043000090813
043100090813           exsr srCONTA;
043200090813
043300090813           update WFDAN11;
043400090813
043500090813         ENDIF;
043600090813
043700090813
043800090813       ENDSR;
043900090813
044000090813       //--------------------------------------------------------------
044100090813       //?Decodfico i dati del cliente.
044200090813       //--------------------------------------------------------------
044300090813       BEGSR srDECOD;
044400090813
044500090813         IF  TASksc <> ACOksc;
044600090813           clear TIBS69ds;
044700090813           clear ds_CNACO;
044800090813           clear ds_CNIND;
044900090813           clear ds_CNCLP;
045000090813           clear ds_FNCLS;
045100090813           I69kac = TASksc;
045200090813           TIBS69R (TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
045300090813         ENDIF;
045400090813
045500090813         //?Cerco e decodifico il codice unificante
045600090813         IF  TASksc <> D10cod;
045700090813           clear TIBS10ds;
045800090813           D10tle = 'ST';
045900090813           D10paf = 'P';
046000090813           D10cod = TASksc;
046100090813           D10drf = wDATE;
046200090813           TIBS10R (TIBS10ds);
046300090813           IF  D10err <> *blanks or D10cop = 0;
046400090813             D10cop = TASksc;
046500090813           ENDIF;
046600090813
046700090813           IF  D10cop <> p_ACOksc;
046800090813             IF D10cop = TASksc;
046900090813               p_CNACOds = ds_CNACO;
047000090813             ELSE;
047100090813               clear TIBS69ds;
047200090813               clear p_CNACOds;
047300090813               clear ds_CNIND;
047400090813               clear ds_CNCLP;
047500090813               clear ds_FNCLS;
047600090813               I69kac = D10cop;
047700090813               TIBS69R (TIBS69ds:p_CNACOds:ds_CNIND:ds_CNCLP:ds_FNCLS);
047800090813             ENDIF;
047900090813           ENDIF;
048000090813         ENDIF;
048100090813
048200090813         //?Decodifica filiale unificante
048300090813         wPOR = %dec(%subst(%editc(p_ACOksc:'X'):1:3):3:0);
048400090813         chain wPOR AZORG01L;
048500090813         IF  not %Found(AZORG01L) or ORGfva <> *blanks or
048600090813             ORGfag = 'A' or ORGfag = 'V';
048700090813           clear AZORGds;
048800090813         ENDIF;
048900090813
049000090813           //?Area filiale unificante
049100090813         clear ds05;
049200090813         TBLcod = '05';
049300090813         TBLkey = %editc(ORGcar:'X');
049400090813         chain (TBLkut:TBLcod:TBLkey) TABEL00F;
049500090813         IF %Found(TABEL00F) and TBLflg = *blanks;
049600090813           ds05 = TBLuni;
049700090813         ENDIF;
049800090813
049900090813           //?Distretto filiale unificante
050000090813         clear ds17;
050100090813         TBLcod = '17';
050200090813         TBLkey = ORGfl3;
050300090813         chain (TBLkut:TBLcod:TBLkey) TABEL00F;
050400090813         IF %Found(TABEL00F) and TBLflg = *blanks;
050500090813           ds17 = TBLuni;
050600090813         ENDIF;
050700090813
050800090813       ENDSR;
050900090813
051000090813       //--------------------------------------------------------------
051100090813       //?Conto le C.A. e i relativi importi per categoria.
051200090813       //--------------------------------------------------------------
051300090813       BEGSR srCONTA;
051400090813
051500090813         SELECT;
051600090813
051700090813         //?Chiusura tecnica
051800090813           WHEN  DCTdch > 0 and §CCHchte = 'S';
051900090813             DANcht += 1;
052000090813             DANchc += DCTncn;
052100090813             exsr srCONTA2;
052200090813             DANchi += wIMP;
052300090813             DANchr += wIMPMR;
052400090813             DANchrc += wNCLMR;
052500090813
052600090813         //?Evento
052700090813           WHEN  DCTnev > 0;
052800090813             SELECT;
052900090813
053000090813             //?Mancanza
053100090813               WHEN  §TADragr = 'M';
053200090813                 DANema += 1;
053300090813                 DANemc += DCTncn;
053400090813                 exsr srCONTA2;
053500090813                 DANemi += wIMP;
053600090813                 DANemr += wIMPMR;
053700090813                 DANemrc += wNCLMR;
053800090813
053900090813             //?Ammanco
054000090813               WHEN  §TADragr = 'A';
054100090813                 DANeam += 1;
054200090813                 DANeac += DCTncn;
054300090813                 exsr srCONTA2;
054400090813                 DANeai += wIMP;
054500090813                 DANear += wIMPMR;
054600090813                 DANearc += wNCLMR;
054700090813
054800090813             //?Avaria
054900090813               WHEN  §TADragr = 'V';
055000090813                 DANeav += 1;
055100090813                 DANevc += DCTncn;
055200090813                 exsr srCONTA2;
055300090813                 DANevi += wIMP;
055400090813                 DANevr += wIMPMR;
055500090813                 DANevrc += wNCLMR;
055600090813             ENDSL;
055700090813
055800090813         //?altrimenti
055900090813           WHEN  DCTnev = 0;
056000090813             SELECT;
056100090813
056200090813             //?Mancanza
056300090813               WHEN  §TADragr = 'M';
056400090813                 DANman += 1;
056500090813                 DANmac += DCTncn;
056600090813                 exsr srCONTA2;
056700090813                 DANmai += wIMP;
056800090813                 DANmar += wIMPMR;
056900090813                 DANmarc += wNCLMR;
057000090813
057100090813             //?Ammanco
057200090813               WHEN  §TADragr = 'A';
057300090813                 DANamm += 1;
057400090813                 DANamc += DCTncn;
057500090813                 exsr srCONTA2;
057600090813                 DANami += wIMP;
057700090813                 DANamr += wIMPMR;
057800090813                 DANamrc += wNCLMR;
057900090813
058000090813             //?Avaria
058100090813               WHEN  §TADragr = 'V';
058200090813                 DANava += 1;
058300090813                 DANavc += DCTncn;
058400090813                 exsr srCONTA2;
058500090813                 DANavi += wIMP;
058600090813                 DANavr += wIMPMR;
058700090813                 DANavrc += wNCLMR;
058800090813             ENDSL;
058900090813
059000090813         ENDSL;
059100090813
059200090813         //?Calcolo importo rimpborsato
059300090813         DANtir += DCLipl;
059400090813
059500090813         //?Calcolo importo trattenuto
059600090813         DANtit += DCLipt;
059700090813
059800090813       ENDSR;
059900090813
060000090813       //--------------------------------------------------------------
060100090813       //?Sommo gli importi.
060200090813       //--------------------------------------------------------------
060300090813       BEGSR srCONTA2;
060400090813
060500090813         clear wIMP;
060600090813         clear wIMPMR;
060700090813         clear wNCLMR;
060800090813
060900090813         SELECT;
061000090813
061100090813         //?Importo liquidato
061200090813           WHEN  DCLipl > 0;
061300090813             wIMP = DCLipl;
061400090813
061500090813         //?Importo richiesto
061600090813           WHEN  DCLipr > 0;
061700090813             wIMP = DCLipr;
061800090813
061900090813         //?Importo trattenuto
062000090813           WHEN  DCLipt > 0;
062100090813             wIMP = DCLipt;
062200090813
062300090813         //?Importo massimo risarcibile
062400090813           WHEN  DCTipv > 0;
062500090813             wIMPMR = DCTipv;
062600090813             wNCLMR += DCTncn;
062700090813
062800090813         ENDSL;
062900090813
063000090813       ENDSR;
063100090813
063200090813       //--------------------------------------------------------------
063300090813       //?Calcolo il fatturato
063400090813       //--------------------------------------------------------------
063500090813       BEGSR srRIEPILOGO;
063600090813
063700090813         //?Posizionamento su WFDAN12 per periodo
063800090813         kDANdip = IS1dip;
063900090813         kDANdfp = IS1dfp;
064000090813
064100090813         setll (kDANdip:kDANdfp) WFDAN12L;
064200090813         reade (kDANdip:kDANdfp) WFDAN12L;
064300090813
064400090813         DOW  not %Eof(WFDAN12L);
064500090813
064600090813           SELECT;
064700090813
064800090813         //?Primo cliente unificante
064900090813             WHEN  wDANkun = 0;
065000090813           //?Recupero i figli del cliente unificante
065100090813               exsr sr_KUN;
065200090813           //?Salvo i dati dell'unificante appena letto
065300090813               exsr sr_SAV;
065400090813
065500090813         //?Rottura per cliente unificante
065600090814             WHEN  DANkun <> wDANkun;
065700090813           //?Scrivo i dati dei clienti senza danni
065800090813               exsr sr_NOD;
065900090813           //?Recupero i figli del cliente unificante
066000090813               exsr sr_KUN;
066100090813           //?Salvo i dati dell'unificante appena letto
066200090813               exsr sr_SAV;
066300090813           ENDSL;
066400090813
066500090813         //?Calcolo ricavo reale e presunto x delta, spedizioni, colli e fatturato
066600090813           wKSC = DANksc;
066700090813           exsr sr_DEL;
066800090813
066900090813           DANspe = O70nsp;
067000090813           DANcol = O70nco;
067100090813           DANfat = O70ric;
067200090813           DANrir = O70rir;
067300090813           DANrip = O70rip;
067400090813
067500090813         //?aggiorno WFDAN
067600090813           update WFDAN12;
067700090813
067800090813         //?elimino dalla sk dei figli quello appena elaborato
067900090813         //?e sottraggo 1 dalla contatore di quelli ancora da elaborare
068000090813           w0110 = DANksc;
068100090813           xx = %lookup(w0110 : skSC11);
068200090813           IF xx > 0;
068300090813             clear skSC11(xx);
068400090813             wELEr -= 1;
068500090813           ENDIF;
068600090813
068700090813         reade (kDANdip:kDANdfp) WFDAN12L;
068800090813
068900090813         ENDDO;
069000090813
069100090813       ENDSR;
069200090813
069300090813       //--------------------------------------------------------------
069400090813       //?Recupero i figli dell'unificante.
069500090813       //--------------------------------------------------------------
069600090813       BEGSR sr_KUN;
069700090813
069800090813         clear wELEr;
069900090813         clear wELEt;
070000090813
070100090813         clear TIBS10ds;
070200090813         D10tle = 'ST';
070300090813         D10paf = 'F';
070400090813         D10cod = DANkun;
070500090813         D10drf = wDATE;
070600090813         TIBS10R (TIBS10ds);
070700090813         IF  D10err <> *blanks;
070800090813           skSC11(1) = DANkun;
070900090813           wELEt = 1;
071000090813         ELSE;
071100090813           clear w0110;
071200090813           xx = %lookup(w0110 : skSC11);
071300090813           IF xx <> 0;
071400090813             wELEt = xx - 1;
071500090813           ENDIF;
071600090813         ENDIF;
071700090813
071800090813         wELEr = wELEt;
071900090813
072000090813       ENDSR;
072100090813
072200090813       //--------------------------------------------------------------
072300090813       //?Salvo i dati dell'unificante appena letto.
072400090813       //--------------------------------------------------------------
072500090813       BEGSR sr_SAV;
072600090813
072700090813         wDANkun = DANkun;
072800090813         wDANura = DANura;
072900090813         wDANpor = DANpor;
073000090813         wDANdpr = DANdpr;
073100090813         wDANdiv = DANdiv;
073200090813         wDANddv = DANddv;
073300090813         wDANare = DANare;
073400090813         wDANdar = DANdar;
073500090813
073600090813       ENDSR;
073700090813
073800090813       //--------------------------------------------------------------
073900090813       //?Calcolo ricavo reale e presunto.
074000090813       //--------------------------------------------------------------
074100090813       BEGSR sr_DEL;
074200090813
074300090813         clear TISE70ds;
074400090813         I70tla = 'S';
074500090813         I70tle = 'ST';
074600090813         I70ksc = wKSC;
074700090813         I70daf = IS1dfp;
074800090813         I70dai = IS1dip;
074900090813         tise70r(TISE70ds);
075000090813
075100090813       ENDSR;
075200090813
075300090813       //--------------------------------------------------------------
075400090813       //?Scrivo rcd clienti senza danni.
075500090813       //--------------------------------------------------------------
075600090813       BEGSR sr_NOD;
075700090814
075800090814         xx = 1;
075900090813
076000090813         //?se restano dei figli ancora da elaborare (no danni)
076100090813         //?creao rcd su WFDAN solo se hanno fatturato e/o delta
076200090813         IF wELEr > 0;
076300090813
076400090813           FOR xx by 1 to wELEt;
076500090813
076600090813             IF  skSC11(xx) > 0;
076700090813         //?Calcolo ricavo reale e presunto x delta, spedizioni, colli e fatturato
076800090813               wKSC = skSC11(xx);
076900090813               exsr sr_DEL;
077000090813         //?se ho trovato dei dati memorizzo
077100090813               IF  O70nsp <> 0 or
077200090813                   O70nco <> 0 or
077300090813                   O70ric <> 0 or
077400090813                   O70rir <> 0 or
077500090813                   O70rip <> 0;
077600090813
077700090813                 clear WFDAN100;
077800090813                 w_DANdata = DANdata;
077900090813                 w_DANora  = DANora;
078000090813                 w_DANpru  = DANpru;
078100090813                 w_DANdip  = IS1dip;
078200090813                 w_DANdfp  = IS1dfp;
078300090813
078400090813                 //?Decodifico il cliente
078500090813                 clear TIBS69ds;
078600090813                 I69kac = skSC11(xx);
078700090813                 exsr srDECODksc;
078800090813                 w_DANksc = skSC11(xx);
078900090813                 w_DANrag = ACOrag;
079000090813
079100090813                 w_DANkun = wDANkun;
079200090813                 w_DANura = wDANura;
079300090813                 w_DANpor = wDANpor;
079400090813                 w_DANdpr = wDANdpr;
079500090813                 w_DANdiv = wDANdiv;
079600090813                 w_DANddv = wDANddv;
079700090813                 w_DANare = wDANare;
079800090813                 w_DANdar = wDANdar;
079900090813
080000090813                 w_DANspe = O70nsp;
080100090813                 w_DANcol = O70nco;
080200090813                 w_DANfat = O70ric;
080300090813                 w_DANrir = O70rir;
080400090813                 w_DANrip = O70rip;
080500090813
080600090813                 write WFDAN100;
080700090813
080800090813               ENDIF;
080900090813
081000090813               wELEr -= 1;
081100090813               IF wELEr = 0;
081200090813                 leave;
081300090813               ENDIF;
081400090813
081500090813             ENDIF;
081600090813           ENDFOR;
081700090813
081800090813         ENDIF;
081900090813
082000090813       ENDSR;
082100090813
082200090813       //--------------------------------------------------------------
082300090813       //?Decodfico i dati del cliente da scrivere sul file.
082400090813       //--------------------------------------------------------------
082500090813       BEGSR srDECODksc;
082600090813
082700090813         clear ds_CNACO;
082800090813         clear ds_CNIND;
082900090813         clear ds_CNCLP;
083000090813         clear ds_FNCLS;
083100090813         TIBS69R (TIBS69ds:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
083200090813
083300090813       ENDSR;
083400090813
083500090813       //--------------------------------------------------------------
083600090813       //?Operazioni finali.
083700090813       //--------------------------------------------------------------
083800090813       BEGSR sr_RoutEnd;
083900090813
084000090813         *inLR = *on;
084100090813         return;
084200090813
084300090813       ENDSR;
