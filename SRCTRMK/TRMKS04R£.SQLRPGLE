000100120925      //--------------------------------------------------------------
000200121105      //?TRMKS04R - Pulizia/Storicizza Trattative + file legati.
000300120925      //--------------------------------------------------------------
000400120925
000500120925     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120925
000700120925      //---------------------------------------------------------------
000800120925      //?Dichiarazione file.
000900120925      //---------------------------------------------------------------
001000121008
001100130918       // -?Anagrafica Commerciali
001200130918     fAZCMM01L  if   e           k disk
001300120928
001400121009       // -?File Attività
001500121008     fTIATC06L  uf   e           k disk
001600120928
001700120928       // -?File Note
001800121025     fTICPN04L  if a e             disk
001900121008
002000121008       // -?File Storico
002100121008     fTICRM00F  o    e             disk
002200121009
002300121009       // -?File INFO trattativa
002400121009     fTIVII01L  if   e           k disk
002500121010
002600121010       // -?File Trattative
002700121025     fTIVIS01L  if   e           k disk
002800121008
002900121008       // -?File Offerte
003000121009     fTIVOF11L  if   e           k disk
003100121009
003200121010       // -?File Tabelle
003300121009     fTNTBE01L  if   e           k disk
003400120925
003500120925      //---------------------------------------------------------------
003600120925      //?Definizione costanti.
003700120925      //---------------------------------------------------------------
003800120925
003900120925      //---------------------------------------------------------------
004000120925      //?Definizione schiere.
004100120925      //---------------------------------------------------------------
004200121009
004300121009     d ITO             s              3    dim(50)
004400121009     d ITOds           s             70    dim(50)
004500121009     d ITOor           s              8    dim(50)
004600121011     d TTR_info        s              1    dim(50)
004700120925
004800120925      //---------------------------------------------------------------
004900120925      //?Definizione aree dati.
005000120925      //---------------------------------------------------------------
005100120925
005200120925      //---------------------------------------------------------------
005300120925      //?Definizione strutture dati.
005400120925      //---------------------------------------------------------------
005500121009
005600121009       // -?Tabella CCO = Causale contatto
005700121009     d dCCO          e ds                  inz
005800121008
005900121009       // -?Tabella ITO = Info Trattativa
006000121009     d dITO          e ds                  inz
006100121010
006200121010       // -?Tabella ITS = Info Trattativa
006300121010     d dITS          e ds                  inz
006400121011
006500121011       // -?Tabella TTR = Tipo Trattativa
006600121011     d dTTR          e ds                  inz
006700121008
006800121008       // -?FLO del file TRATTATIVA
006900121008     d dVIS01        e ds                  inz  qualified
007000121010
007100121010       // -?Kpjba
007200121010     d KPJBA         e ds
007300121107     d  DataParm             247    254s 0
007400121008
007500121008       // -?Ricerca/Controllo tabelle
007600121008     d TIBS02ds      e ds                  inz
007700121002
007800121002       // -?File TIVIS00F
007900121002     d TIVISds       e ds                  extname(TIVIS00F)
008000121002
008100121002       // -?File TNCPO00F
008200121002     d TNCPOds       e ds                  extname(TNCPO00F)
008300121025
008400121025       // -?Pulizia Trattative + file legati
008500121025     d TNTA16ds      e ds                  inz
008600120925
008700120925      //---------------------------------------------------------------
008800120925      //?Definizione variabili globali.
008900120925      //---------------------------------------------------------------
009000120925
009100120925       // -?Flags booleani
009200120925     d wEoF            s               n   inz(*off)
009300121002     d wEoF_1          s               n   inz(*off)
009400120925
009500120925       // -?Campi di comodo data
009600120928     d DataISO         s               d   datfmt(*iso)
009700121107     d DataPul         s              8  0 inz
009800121107     d DataSav         s              8  0 inz
009900121012
010000121012       // -?Campi di comodo ora
010100121012     d sav_CPNhim      s                   like(CPNhim)
010200121012     d wTime           s                   like(CPNhim)
010300121012     d wTimeISO        s               t   timfmt(*iso)
010400120925
010500121008       // -?Campi di comodo contatori
010600121010     d nrCONC          s              5s 0
010700121025     d nrVIS           s              5s 0
010800121009
010900121009       // -?Campi di comodo
011000121108     d wDES            s             50a
011100121011     d wITO            s                   like(VIItpf)
011200121012     d wNOTA           s                   like(CPNnot)
011300121011     d wPFT            s                   like(VIIpft)
011400121011     d wRMS            s             13s 2
011500121011     d wSNA            s                   like(VIIsna)
011600121012     d wstringa        s                   like(CPNnot)
011700121011     d wTVA            s                   like(VIItva)
011800121011     d wVAL            s             15a
011900121009
012000121009       // -?Indici di schiera
012100121011     d aa              s              2s 0
012200121011     d bb              s              2s 0
012300121009     d xx              s              2s 0
012400121009     d yy              s              2s 0
012500121008
012600120925      //---------------------------------------------------------------
012700120925      //?Definizione procedure esterne.
012800120925      //---------------------------------------------------------------
012900121025
013000121025       // -?Pulizia Trattative + file legati
013100121109     d TNTA16R         pr                  extpgm('TNTA16R')
013200121025     d  kpjba                              likeds(kpjba)
013300121025     d  tnta16ds                           likeds(tnta16ds)
013400120925
013500120925      //---------------------------------------------------------------
013600120925      //?Definizione prototipi.
013700120925      //---------------------------------------------------------------
013800121008
013900121009       // -?Aggancio tabella TNTBE
014000121008      /copy gaitrasrc/srcprotopr,tibs02r
014100120925
014200120925      //---------------------------------------------------------------
014300120925      //?Definizione key-list.
014400120925      //---------------------------------------------------------------
014500121008
014600120925
014700120925      //---------------------------------------------------------------
014800120925      //?Riepilogo indicatori.
014900120925      //---------------------------------------------------------------
015000120925
015100120925      //---------------------------------------------------------------
015200120925
015300120925      //---------------------------------------------------------------
015400120925      //?M A I N - L I N E
015500120925      //---------------------------------------------------------------
015600121011
015700121011     c     *entry        plist
015800121011     c                   parm                    KPJBA
015900120925
016000120925      /free
016100120925
016200120925       //?Operazioni iniziali
016300120925       exsr RoutInz;
016400120925
016500121002       //?Leggo i potenziali già contattati
016600121002       exsr Leggi_CPO;
016700120925
016800120925       //?Operazioni finali
016900120925       exsr RoutEnd;
017000120925
017100120925       //--------------------------------------------------------------
017200120925       //?Operazioni iniziali.
017300120925       //--------------------------------------------------------------
017400120925       BEGSR RoutInz;
017500120925
017600120925         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017700121107
017800121107       //?Salvo la data passata
017900121107         DataSav = DataParm;
018000120927
018100121025       //?Alla data pulizia aggiungo 1 gg
018200121107         DataISO = %date(DataParm:*ISO);
018300121025         DataISO = DataISO + %days(1);
018400120928         DataPul = %dec(DataISO);
018500121009
018600121009       //?Carico le INFO in sk
018700121009         clear xx;
018800121009         clear ITO;
018900121009         clear ITOds;
019000121009         clear ITOor;
019100121009         setll ('ITO') TNTBE01L;
019200121010         reade ('ITO') TNTBE01L;
019300121009         DOW not %eof(TNTBE01L);
019400121011           IF  TBEatb = *blanks;
019500121011             dITO = TBEuni;
019600121011             xx += 1;
019700121011             ITO(xx)   = TBEke1;
019800121011             ITOds(xx) = dITO;
019900121011             ITOor(xx) = §ITOitg + %editc(§ITOogi:'X') + TBEke1;
020000121011           ENDIF;
020100121009           reade ('ITO') TNTBE01L;
020200121009         ENDDO;
020300121009
020400121009       //?Ordino la sk delle INFO
020500121009         sorta ITOor;
020600121011
020700121011       //?Carico le sk per i tipi Trattativa
020800121011         clear xx;
020900121011         clear yy;
021000121011         setll ('TTR') TNTBE01L;
021100121011         reade ('TTR') TNTBE01L;
021200121011         DOW not %eof(TNTBE01L);
021300121011           IF  TBEatb = *blanks;
021400121011             dTTR = TBEuni;
021500121011             IF  §TTRinfo = 'S';
021600121011               xx += 1;
021700121011               TTR_info(xx)   = TBEke1;
021800121011             ENDIF;
021900121011           ENDIF;
022000121011           reade ('TTR') TNTBE01L;
022100121011         ENDDO;
022200120925
022300120925       ENDSR;
022400120928
022500120928       //--------------------------------------------------------------
022600121002       //?Lettura dei potenziali già contattati.
022700120928       //--------------------------------------------------------------
022800121002       BEGSR Leggi_CPO;
022900121001
023000121001         wEoF = *off;
023100121001
023200121002       //?Leggo i potenziali SOLO quelli contattati
023300121210       //?e NON eliminabili
023400121001         exec sql
023500121002          DECLARE CPO1 cursor for
023600121002          SELECT CPOcpo from TNCPO00F
023700121002          WHERE substr(CPOrst, 13, 8) <> '        ' and
023800121210                substr(CPOrst, 13, 8) <> '00000000' and
023900121210                CPOfls <> 'E'
024000121002          ORDER BY CPOcpo;
024100121001
024200121001         exec sql
024300121002           open CPO1;
024400121001
024500121001         IF sqlcode < 0;
024600121001           wEoF = *on;
024700121001           leavesr;
024800121001         ENDIF;
024900121001
025000121001         DOW  not wEoF;
025100121001
025200121002           exec sql fetch next from CPO1 into :CPOcpo;
025300121001           IF  sqlcod = 100 or sqlcod < 0;
025400121001             wEoF = *on;
025500121001             leave;
025600121001           ENDIF;
025700121025
025800121025           clear nrVIS;
025900121025
026000121029         //?Se il potenziale ha solo 1 trattativa chiusa o nessuna trattativa
026100121029         //?chiusa non faccio niente
026200121025           exec sql
026300121025           SELECT count(*) into :nrVIS  from TIVIS00F
026400121025           WHERE VIScpo = :CPOcpo and VISdch > 0;
026500121025
026600121025           IF  nrVIS <= 1;
026700121025             iter;
026800121025           ENDIF;
026900121025
027000121025           clear nrVIS;
027100121025
027200121002         //?Controllo se per il potenziale esistono trattative chiuse
027300121025         //?con data chiusura < data pulizia
027400121002           exec sql
027500121025           SELECT count(*) into :nrVIS from TIVIS00F
027600121008           WHERE VIScpo = :CPOcpo and VISdch > 0 and
027700121025                 VISdch < :DataPul;
027800121025
027900121025           //?Se ho trovato delle trattative chiuse
028000121008           //?che hanno data chiusura < data pulizia
028100121025           //?le pulisco
028200121025           IF nrVIS > 0;
028300121025             exsr  Pul_VIS;
028400121025           ENDIF;
028500121001
028600121001         ENDDO;
028700121001
028800121002         exec sql close CPO1;
028900121001         wEoF = *off;
029000121001
029100121001       ENDSR;
029200121002
029300121002       //--------------------------------------------------------------
029400121008       //?Pulisco le trattative.
029500121002       //--------------------------------------------------------------
029600121008       BEGSR Pul_VIS;
029700121008
029800121008         wEoF_1 = *off;
029900121008
030000121008       //?Leggo tutte le trattative con data chiusura < data pulizia
030100121008         setll (CPOcpo) TIVIS01L;
030200121008
030300121008         DOW not wEoF_1;
030400121008           reade (CPOcpo) TIVIS01L;
030500121008
030600121008           IF  %eof(TIVIS01L);
030700121008             leavesr;
030800121008           ENDIF;
030900121008
031000121008         //?Data chiusura < data pulizia
031100121008           IF  VISdch = 0;
031200121008             iter;
031300121008           ENDIF;
031400121008           IF  VISdch >= DataPul;
031500121008             leavesr;
031600121008           ENDIF;
031700121008
031800121008           dVIS01 = VISflo;
031900121008
032000121008         //?Storicizzo la trattativa
032100121008           exsr Sto_VIS;
032200121012
032300121012         //?Storicizzo le INFO trattative
032400121012           IF  %lookup(VIStpv : TTR_info) > 0;
032500121012             exsr Sto_INFO;
032600121012           ENDIF;
032700121009
032800121025         //?Pulisco le trattative + file legati
032900121025           clear TNTA16DS;
033000121025           ITA16nrv = VISnrv;
033100121105           ITA16sto = 'S';
033200121109           tnta16r (KPJBA:TNTA16DS);
033300121009
033400121009         ENDDO;
033500121002
033600121002       ENDSR;
033700121008
033800121008       //--------------------------------------------------------------
033900121008       //?Storicizzo la trattativa.
034000121008       //--------------------------------------------------------------
034100121008       BEGSR Sto_VIS;
034200121008
034300121008         clear TICRM000;
034400121008
034500121008         CRMtrc = 'T';
034600121008         CRMcpo = CPOcpo;
034700121008         CRMksc = VISksc;
034800121008         CRMtpv = VIStpv;
034900121008         CRMnrv = VISnrv;
035000121008         CRMdai = VISdat;
035100121008         CRMdaf = VISdch;
035200121008
035300121008       //?Commerciale trattativa
035400130918         chain  (VIScmm)  AZCMM000;
035500130918         IF  %found(AZCMM01L);
035600130918           CRMmmd = CMMdes;
035700121008         ENDIF;
035800121008
035900121008       //?Leggo le offerte
036000121008         setll (VISnrv) TIVOF11L;
036100121008         reade (VISnrv) TIVOF11L;
036200121008
036300121008         DOW not %eof(TIVOF11L);
036400121008
036500121008         //?imposto se offerta italia o estero
036600121008         //?e se convalidate
036700121008           IF  VOFtpt = 'I';
036800121008             CRMpoi = 'S';
036900121008             IF  VOFeso = 'C';
037000121008               CRMcoi = 'S';
037100121008             ENDIF;
037200121008           ELSE;
037300121008             CRMpoe = 'S';
037400121008             IF  VOFeso = 'C';
037500121008               CRMcoe = 'S';
037600121008             ENDIF;
037700121008           ENDIF;
037800121008
037900121008           reade (VISnrv) TIVOF11L;
038000121008         ENDDO;
038100121109
038200121109       //?Se Causale esito trattativa = 'P' imposto 'ESITO POSITIVO'
038300121109         IF  VISesi = 'P';
038400121109           CRMdca = 'ESITO POSITIVO';
038500121109         ENDIF;
038600121109
038700121109       //?Se Causale esito trattativa non è 'P'
038800121109       //?cerco la Causale esito trattativa
038900121109         IF  VISesi <> 'P';
039000121109           clear dCCO;
039100121109           clear TIBS02ds;
039200121109           T02mod = 'C';
039300121109           T02cod = 'CCO';
039400121109           T02ke1 = VISesi;
039500121109           T02sif = KNSIF;
039600121109           TNTBE_RicercaControllo (kpjba : tibs02ds);
039700121109           dCCO = T02uni;
039800121109           CRMdca = §CCOdes;
039900121109         ENDIF;
040000121008
040100121008         CRMesi = dVIS01.§VISesito;
040200121009
040300121009       //?Conto quanti appuntamenti
040400121009         setll (VISnrv) TIATC06L;
040500121009         reade (VISnrv) TIATC06L;
040600121009         DOW not %eof(TIATC06L);
040700121009           IF  ATCcad = '91' and ATCest = 'S';
040800121009             CRMapp += 1;
040900121009           ENDIF;
041000121009           reade (VISnrv) TIATC06L;
041100121009         ENDDO;
041200121008
041300121008         write  TICRM000;
041400121008
041500121008       ENDSR;
041600121009
041700121009       //--------------------------------------------------------------
041800121009       //?Storicizzo le INFO trattativa.
041900121009       //--------------------------------------------------------------
042000121009       BEGSR Sto_INFO;
042100121010
042200121010         clear nrCONC;
042300121011         clear wPFT;
042400121011         clear wRMS;
042500121011         clear wSNA;
042600121012         clear sav_CPNhim;
042700121009
042800121009       //?Parto a leggere dalla SK delle INFO ordinata
042900121009         xx = 1;
043000121009         FOR xx by 1 to %elem(ITOor);
043100121009
043200121108           IF  ITOor(xx) = *blanks;
043300121009             iter;
043400121009           ENDIF;
043500121009
043600121009           wITO = %subst(ITOor(xx):6:3);
043700121009
043800121009         //?Recupero la ds della INFO
043900121009           yy = %lookup(wITO:ITO);
044000121009           IF yy > 0;
044100121009             dITO = ITOds(yy);
044200121009           ENDIF;
044300121009         //?in caso di trattativa di 'Aumento/Sconto'
044400121009         //?devo modificare la descrizione
044500121009           IF  VIStpv = 'A' and wITO = 'TRT';
044600121009             §ITOdes = 'Valore Aumento/Sconto';
044700121009           ENDIF;
044800121012
044900121012           clear yy;
045000121009
045100121009         //?Aggancio le INFO trattativa
045200121009           chain (VISnrv : wITO) TIVII01L;
045300121108         //?La RMS non la troverò  mai sul file, è una INFO calcolata
045400121108         //?automaticamente dal pgm e non memorizzata sul file
045500121108           IF  not %found(TIVII01L) and wITO <> 'RMS';
045600121010             iter;
045700121010           ENDIF;
045800121012
045900121012           clear wDES;
046000121012           clear wTVA;
046100121012           clear wVAL;
046200121009
046300121010         //?INFO 'TRT'
046400121010           IF  wITO = 'TRT' and VIIpft <> 0;
046500121011             wDES = §ITOdes;
046600121011             IF  VIStpv <> 'A';
046700121011               wDES = %trim(wDES) + ' Annuo';
046800121011             ENDIF;
046900121012             wVAL = %trim(%editc(VIIpft:'J'));
047000121011             wTVA = VIIvft;
047100121011             exsr Sistema_INFO;
047200121010             IF  VIIsna > 0;
047300121012               wDES = 'Spedizioni Annue';
047400121012               wVAL = %trim(%editc(VIIsna:'J'));
047500121012               clear wTVA;
047600121011               exsr Sistema_INFO;
047700121010             ENDIF;
047800121011             wPFT = VIIpft;
047900121011             wSNA = VIIsna;
048000121010           ENDIF;
048100121009
048200121010         //?INFO 'TRE' - NO per trattativa di tipo 'A'
048300121010           IF  wITO = 'TRE' and VIStpv <> 'A' and VIIpft <> 0;
048400121011             wDES = §ITOdes;
048500121012             wVAL = %trim(%editc(VIIpft:'J'));
048600121011             wTVA = VIIvft;
048700121011             exsr Sistema_INFO;
048800121010             IF  VIIsna > 0;
048900121012               wDES = 'Spedizioni Annue';
049000121012               wVAL = %trim(%editc(VIIsna:'J'));
049100121012               clear wTVA;
049200121011               exsr Sistema_INFO;
049300121010             ENDIF;
049400121010           ENDIF;
049500121009
049600121010         //?INFO 'A/S' - SOLO per trattativa di tipo 'A'
049700121010           IF  wITO = 'A/S' and VIStpv = 'A' and VIIvald <> 0;
049800121011             wDES = §ITOdes;
049900121012             wVAL = %trim(%editc(VIIvald:'J'));
050000121011             wTVA = VIIvft;
050100121011             exsr Sistema_INFO;
050200121010           ENDIF;
050300121009
050400121010         //?INFO 'DEL'
050500121010           IF  wITO = 'DEL';
050600121011             wDES = §ITOdes;
050700121012             wVAL = %trim(%editc(VIIval:'J'));
050800121011             wTVA = VIItva;
050900121011             exsr Sistema_INFO;
051000121010           ENDIF;
051100121108
051200121108         //?INFO 'RMS' - NO per trattativa di tipo 'A'
051300121108           IF  wITO = 'RMS' and VIStpv <> 'A' and wPFT <> 0;
051400121108             clear wDES;
051500121108             clear wTVA;
051600121108             clear wVAL;
051700121108             wDES = §ITOdes;
051800121108             wRMS = wPFT / wSNA;
051900121108             wVAL = %trim(%editc(wRMS:'J'));
052000121108             wTVA = §ITOtva;
052100121108             exsr Sistema_INFO;
052200121108           ENDIF;
052300121010
052400121010         //?INFO 'KMS' - NO per trattativa di tipo 'A'
052500121010           IF  wITO = 'KMS' and VIStpv <> 'A';
052600121011             wDES = §ITOdes;
052700121012             wVAL = %trim(%editc(VIIval:'J'));
052800121011             wTVA = VIItva;
052900121011             exsr Sistema_INFO;
053000121010           ENDIF;
053100121009
053200121010         //?INFO '10'
053300121010           IF  wITO = '10';
053400121010             clear dITS;
053500121010             clear TIBS02ds;
053600121010             T02mod = 'C';
053700121010             T02cod = 'ITS';
053800121010             T02ke1 = wITO;
053900121010             T02ke2 = VIIspf;
054000121010             T02sif = KNSIF;
054100121010             TNTBE_RicercaControllo (kpjba : tibs02ds);
054200121010             dITS = T02uni;
054300121112             wDES = 'Concorrenti:' + ' ' + %trim(§ITSdes);
054400121112             exsr Sistema_INFO;
054500121112             exsr Info_CONC;
054600121011           ENDIF;
054700121009
054800121009         ENDFOR;
054900121012
055000121012       //?Se a questo punto ho le note piene le devo scrivere
055100121012         IF  wNOTA <> *blanks;
055200121012           exsr Scrivi_Note;
055300121012           clear wNOTA;
055400121012         ENDIF;
055500121009
055600121009       ENDSR;
055700121112
055800121112       //--------------------------------------------------------------
055900121112       //?Continuo a leggere le INFO dei concorrenti.
056000121112       //--------------------------------------------------------------
056100121112       BEGSR Info_CONC;
056200121112
056300121112         reade (VISnrv : wITO) TIVII01L;
056400121112         DOW  not %eof(TIVII01L);
056500121112           clear dITS;
056600121112           clear TIBS02ds;
056700121112           T02mod = 'C';
056800121112           T02cod = 'ITS';
056900121112           T02ke1 = wITO;
057000121112           T02ke2 = VIIspf;
057100121112           T02sif = KNSIF;
057200121112           TNTBE_RicercaControllo (kpjba : tibs02ds);
057300121112           dITS = T02uni;
057400121112           wDES = §ITSdes;
057500121112           exsr Sistema_INFO;
057600121112           reade (VISnrv : wITO) TIVII01L;
057700121112         ENDDO;
057800121112
057900121112       ENDSR;
058000121011
058100121011       //--------------------------------------------------------------
058200121011       //?Aggiusta campi note x INFO.
058300121011       //--------------------------------------------------------------
058400121011       BEGSR Sistema_INFO;
058500121011
058600121011         clear wstringa;
058700121011         clear aa;
058800121011         clear bb;
058900121011
059000121011         wstringa = %trim(wDES) + ' ' +
059100121011                    %trim(wVAL) + ' ' +
059200121011                    %trim(wTVA);
059300121012         aa = %scan('  ':wstringa);
059400121012         bb = %scan('  ':wNOTA);
059500121012
059600121012         IF  (aa + 3 + bb) > %len(wNOTA);
059700121012           exsr Scrivi_Note;
059800121012           clear wNOTA;
059900121012         ENDIF;
060000121011
060100121012         IF  wNOTA = *blanks;
060200121012           wNOTA = %trim(wstringa);
060300121012           leavesr;
060400121108         ENDIF;
060500121012
060600121012         wNOTA = %trim(wNOTA) + ' - ' + %trim(wstringa);
060700121011
060800121011       ENDSR;
060900121009
061000121009       //--------------------------------------------------------------
061100121009       //?Scrivo le note.
061200121009       //--------------------------------------------------------------
061300121009       BEGSR Scrivi_Note;
061400121009
061500121025         clear TICPN000;
061600121012         CPNcpo = VIScpo;
061700121009         IF  VISksc > 0;
061800121012           CPNksc = VISksc;
061900121009         ENDIF;
062000121012         CPNnrv = VISnrv;
062100130201         CPNdim = VISdch;
062200121012         wTime  = %dec(%time());
062300121108         IF  sav_CPNhim = *zeros;
062400121108           CPNhim = wTime;
062500121108         ELSE;
062600121108           wTimeISO = %time(sav_CPNhim:*ISO);
062700121012           wTimeISO = wTimeISO + %seconds(1);
062800121012           CPNhim =  %dec(wTimeISO);
062900121012         ENDIF;
063000121012         sav_CPNhim = CPNhim;
063100121012         CPNpno = 1;
063200121012         CPNnot = wNOTA;
063300121012         CPNsns = 'S';
063400121012         CPNpru = 'BATCH';
063500121012         CPNpos = 46;
063600121025         write TICPN000;
063700121009
063800121009       ENDSR;
063900120925
064000120925       //--------------------------------------------------------------
064100120925       //?Operazioni finali.
064200120925       //--------------------------------------------------------------
064300120925       BEGSR RoutEnd;
064400121107
064500121107       //?Reimposto la data passata
064600121107         DataParm = DataSav;
064700120925
064800120925         *inLR = *on;
064900120925         return;
065000120925
065100120925       ENDSR;
065200120925
065300120925      /end-free
