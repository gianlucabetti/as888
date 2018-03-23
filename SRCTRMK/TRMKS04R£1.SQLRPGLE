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
001100121008       // -?File Tabelle
001200121008     fTABEL00F  if   e           k disk
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
006400121008
006500121008       // -?Tabella 01 = Codici Commerciali
006600121008     d ds01          e ds                  inz
006700121011
006800121011       // -?Tabella TTR = Tipo Trattativa
006900121011     d dTTR          e ds                  inz
007000121008
007100121008       // -?FLO del file TRATTATIVA
007200121008     d dVIS01        e ds                  inz  qualified
007300121010
007400121010       // -?Kpjba
007500121010     d KPJBA         e ds
007600121107     d  DataParm             247    254s 0
007700121008
007800121008       // -?Ricerca/Controllo tabelle
007900121008     d TIBS02ds      e ds                  inz
008000121002
008100121002       // -?File TIVIS00F
008200121002     d TIVISds       e ds                  extname(TIVIS00F)
008300121002
008400121002       // -?File TNCPO00F
008500121002     d TNCPOds       e ds                  extname(TNCPO00F)
008600121025
008700121025       // -?Pulizia Trattative + file legati
008800121025     d TNTA16ds      e ds                  inz
008900120925
009000120925      //---------------------------------------------------------------
009100120925      //?Definizione variabili globali.
009200120925      //---------------------------------------------------------------
009300120925
009400120925       // -?Flags booleani
009500120925     d wEoF            s               n   inz(*off)
009600121002     d wEoF_1          s               n   inz(*off)
009700120925
009800120925       // -?Campi di comodo data
009900120928     d DataISO         s               d   datfmt(*iso)
010000121107     d DataPul         s              8  0 inz
010100121107     d DataSav         s              8  0 inz
010200121012
010300121012       // -?Campi di comodo ora
010400121012     d sav_CPNhim      s                   like(CPNhim)
010500121012     d wTime           s                   like(CPNhim)
010600121012     d wTimeISO        s               t   timfmt(*iso)
010700120925
010800121008       // -?Campi di comodo contatori
010900121010     d nrCONC          s              5s 0
011000121025     d nrVIS           s              5s 0
011100121009
011200121009       // -?Campi di comodo
011300121108     d wDES            s             50a
011400121011     d wITO            s                   like(VIItpf)
011500121012     d wNOTA           s                   like(CPNnot)
011600121011     d wPFT            s                   like(VIIpft)
011700121011     d wRMS            s             13s 2
011800121011     d wSNA            s                   like(VIIsna)
011900121012     d wstringa        s                   like(CPNnot)
012000121011     d wTVA            s                   like(VIItva)
012100121011     d wVAL            s             15a
012200121009
012300121009       // -?Indici di schiera
012400121011     d aa              s              2s 0
012500121011     d bb              s              2s 0
012600121009     d xx              s              2s 0
012700121009     d yy              s              2s 0
012800121008
012900120925      //---------------------------------------------------------------
013000120925      //?Definizione procedure esterne.
013100120925      //---------------------------------------------------------------
013200121025
013300121025       // -?Pulizia Trattative + file legati
013400121109     d TNTA16R         pr                  extpgm('TNTA16R')
013500121025     d  kpjba                              likeds(kpjba)
013600121025     d  tnta16ds                           likeds(tnta16ds)
013700120925
013800120925      //---------------------------------------------------------------
013900120925      //?Definizione prototipi.
014000120925      //---------------------------------------------------------------
014100121008
014200121009       // -?Aggancio tabella TNTBE
014300121008      /copy gaitrasrc/srcprotopr,tibs02r
014400120925
014500120925      //---------------------------------------------------------------
014600120925      //?Definizione key-list.
014700120925      //---------------------------------------------------------------
014800121008
014900121008       // -?File Tabelle
015000121008     d k03tabel      e ds                  extname(TABEL00F:*key)
015100121008     d                                     prefix(k_)
015200120925
015300120925      //---------------------------------------------------------------
015400120925      //?Riepilogo indicatori.
015500120925      //---------------------------------------------------------------
015600120925
015700120925      //---------------------------------------------------------------
015800120925
015900120925      //---------------------------------------------------------------
016000120925      //?M A I N - L I N E
016100120925      //---------------------------------------------------------------
016200121011
016300121011     c     *entry        plist
016400121011     c                   parm                    KPJBA
016500120925
016600120925      /free
016700120925
016800120925       //?Operazioni iniziali
016900120925       exsr RoutInz;
017000120925
017100121002       //?Leggo i potenziali già contattati
017200121002       exsr Leggi_CPO;
017300120925
017400120925       //?Operazioni finali
017500120925       exsr RoutEnd;
017600120925
017700120925       //--------------------------------------------------------------
017800120925       //?Operazioni iniziali.
017900120925       //--------------------------------------------------------------
018000120925       BEGSR RoutInz;
018100120925
018200120925         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018300121107
018400121107       //?Salvo la data passata
018500121107         DataSav = DataParm;
018600120927
018700121025       //?Alla data pulizia aggiungo 1 gg
018800121107         DataISO = %date(DataParm:*ISO);
018900121025         DataISO = DataISO + %days(1);
019000120928         DataPul = %dec(DataISO);
019100121009
019200121009       //?Carico le INFO in sk
019300121009         clear xx;
019400121009         clear ITO;
019500121009         clear ITOds;
019600121009         clear ITOor;
019700121009         setll ('ITO') TNTBE01L;
019800121010         reade ('ITO') TNTBE01L;
019900121009         DOW not %eof(TNTBE01L);
020000121011           IF  TBEatb = *blanks;
020100121011             dITO = TBEuni;
020200121011             xx += 1;
020300121011             ITO(xx)   = TBEke1;
020400121011             ITOds(xx) = dITO;
020500121011             ITOor(xx) = §ITOitg + %editc(§ITOogi:'X') + TBEke1;
020600121011           ENDIF;
020700121009           reade ('ITO') TNTBE01L;
020800121009         ENDDO;
020900121009
021000121009       //?Ordino la sk delle INFO
021100121009         sorta ITOor;
021200121011
021300121011       //?Carico le sk per i tipi Trattativa
021400121011         clear xx;
021500121011         clear yy;
021600121011         setll ('TTR') TNTBE01L;
021700121011         reade ('TTR') TNTBE01L;
021800121011         DOW not %eof(TNTBE01L);
021900121011           IF  TBEatb = *blanks;
022000121011             dTTR = TBEuni;
022100121011             IF  §TTRinfo = 'S';
022200121011               xx += 1;
022300121011               TTR_info(xx)   = TBEke1;
022400121011             ENDIF;
022500121011           ENDIF;
022600121011           reade ('TTR') TNTBE01L;
022700121011         ENDDO;
022800121011
022900121011       //?Imposto campi fissi
023000121011         k_TBLkut = 1;
023100120925
023200120925       ENDSR;
023300120928
023400120928       //--------------------------------------------------------------
023500121002       //?Lettura dei potenziali già contattati.
023600120928       //--------------------------------------------------------------
023700121002       BEGSR Leggi_CPO;
023800121001
023900121001         wEoF = *off;
024000121001
024100121002       //?Leggo i potenziali SOLO quelli contattati
024200121210       //?e NON eliminabili
024300121001         exec sql
024400121002          DECLARE CPO1 cursor for
024500121002          SELECT CPOcpo from TNCPO00F
024600121002          WHERE substr(CPOrst, 13, 8) <> '        ' and
024700121210                substr(CPOrst, 13, 8) <> '00000000' and
024800121210                CPOfls <> 'E'
024900121002          ORDER BY CPOcpo;
025000121001
025100121001         exec sql
025200121002           open CPO1;
025300121001
025400121001         IF sqlcode < 0;
025500121001           wEoF = *on;
025600121001           leavesr;
025700121001         ENDIF;
025800121001
025900121001         DOW  not wEoF;
026000121001
026100121002           exec sql fetch next from CPO1 into :CPOcpo;
026200121001           IF  sqlcod = 100 or sqlcod < 0;
026300121001             wEoF = *on;
026400121001             leave;
026500121001           ENDIF;
026600121025
026700121025           clear nrVIS;
026800121025
026900121029         //?Se il potenziale ha solo 1 trattativa chiusa o nessuna trattativa
027000121029         //?chiusa non faccio niente
027100121025           exec sql
027200121025           SELECT count(*) into :nrVIS  from TIVIS00F
027300121025           WHERE VIScpo = :CPOcpo and VISdch > 0;
027400121025
027500121025           IF  nrVIS <= 1;
027600121025             iter;
027700121025           ENDIF;
027800121025
027900121025           clear nrVIS;
028000121025
028100121002         //?Controllo se per il potenziale esistono trattative chiuse
028200121025         //?con data chiusura < data pulizia
028300121002           exec sql
028400121025           SELECT count(*) into :nrVIS from TIVIS00F
028500121008           WHERE VIScpo = :CPOcpo and VISdch > 0 and
028600121025                 VISdch < :DataPul;
028700121025
028800121025           //?Se ho trovato delle trattative chiuse
028900121008           //?che hanno data chiusura < data pulizia
029000121025           //?le pulisco
029100121025           IF nrVIS > 0;
029200121025             exsr  Pul_VIS;
029300121025           ENDIF;
029400121001
029500121001         ENDDO;
029600121001
029700121002         exec sql close CPO1;
029800121001         wEoF = *off;
029900121001
030000121001       ENDSR;
030100121002
030200121002       //--------------------------------------------------------------
030300121008       //?Pulisco le trattative.
030400121002       //--------------------------------------------------------------
030500121008       BEGSR Pul_VIS;
030600121008
030700121008         wEoF_1 = *off;
030800121008
030900121008       //?Leggo tutte le trattative con data chiusura < data pulizia
031000121008         setll (CPOcpo) TIVIS01L;
031100121008
031200121008         DOW not wEoF_1;
031300121008           reade (CPOcpo) TIVIS01L;
031400121008
031500121008           IF  %eof(TIVIS01L);
031600121008             leavesr;
031700121008           ENDIF;
031800121008
031900121008         //?Data chiusura < data pulizia
032000121008           IF  VISdch = 0;
032100121008             iter;
032200121008           ENDIF;
032300121008           IF  VISdch >= DataPul;
032400121008             leavesr;
032500121008           ENDIF;
032600121008
032700121008           dVIS01 = VISflo;
032800121008
032900121008         //?Storicizzo la trattativa
033000121008           exsr Sto_VIS;
033100121012
033200121012         //?Storicizzo le INFO trattative
033300121012           IF  %lookup(VIStpv : TTR_info) > 0;
033400121012             exsr Sto_INFO;
033500121012           ENDIF;
033600121009
033700121025         //?Pulisco le trattative + file legati
033800121025           clear TNTA16DS;
033900121025           ITA16nrv = VISnrv;
034000121105           ITA16sto = 'S';
034100121109           tnta16r (KPJBA:TNTA16DS);
034200121009
034300121009         ENDDO;
034400121002
034500121002       ENDSR;
034600121008
034700121008       //--------------------------------------------------------------
034800121008       //?Storicizzo la trattativa.
034900121008       //--------------------------------------------------------------
035000121008       BEGSR Sto_VIS;
035100121008
035200121008         clear TICRM000;
035300121008
035400121008         CRMtrc = 'T';
035500121008         CRMcpo = CPOcpo;
035600121008         CRMksc = VISksc;
035700121008         CRMtpv = VIStpv;
035800121008         CRMnrv = VISnrv;
035900121008         CRMdai = VISdat;
036000121008         CRMdaf = VISdch;
036100121008
036200121008       //?Commerciale trattativa
036300121008         clear ds01;
036400121008         k_TBLcod = '01';
036500121008         k_TBLkey = %editc(VIScmm:'X');
036600121008         chain  %kds(K03tabel) TABEL00F;
036700121008         IF  %found(TABEL00F);
036800121008           ds01 = TBLuni;
036900121008         ENDIF;
037000121008         CRMmmd = §01age;
037100121008
037200121008       //?Leggo le offerte
037300121008         setll (VISnrv) TIVOF11L;
037400121008         reade (VISnrv) TIVOF11L;
037500121008
037600121008         DOW not %eof(TIVOF11L);
037700121008
037800121008         //?imposto se offerta italia o estero
037900121008         //?e se convalidate
038000121008           IF  VOFtpt = 'I';
038100121008             CRMpoi = 'S';
038200121008             IF  VOFeso = 'C';
038300121008               CRMcoi = 'S';
038400121008             ENDIF;
038500121008           ELSE;
038600121008             CRMpoe = 'S';
038700121008             IF  VOFeso = 'C';
038800121008               CRMcoe = 'S';
038900121008             ENDIF;
039000121008           ENDIF;
039100121008
039200121008           reade (VISnrv) TIVOF11L;
039300121008         ENDDO;
039400121109
039500121109       //?Se Causale esito trattativa = 'P' imposto 'ESITO POSITIVO'
039600121109         IF  VISesi = 'P';
039700121109           CRMdca = 'ESITO POSITIVO';
039800121109         ENDIF;
039900121109
040000121109       //?Se Causale esito trattativa non è 'P'
040100121109       //?cerco la Causale esito trattativa
040200121109         IF  VISesi <> 'P';
040300121109           clear dCCO;
040400121109           clear TIBS02ds;
040500121109           T02mod = 'C';
040600121109           T02cod = 'CCO';
040700121109           T02ke1 = VISesi;
040800121109           T02sif = KNSIF;
040900121109           TNTBE_RicercaControllo (kpjba : tibs02ds);
041000121109           dCCO = T02uni;
041100121109           CRMdca = §CCOdes;
041200121109         ENDIF;
041300121008
041400121008         CRMesi = dVIS01.§VISesito;
041500121009
041600121009       //?Conto quanti appuntamenti
041700121009         setll (VISnrv) TIATC06L;
041800121009         reade (VISnrv) TIATC06L;
041900121009         DOW not %eof(TIATC06L);
042000121009           IF  ATCcad = '91' and ATCest = 'S';
042100121009             CRMapp += 1;
042200121009           ENDIF;
042300121009           reade (VISnrv) TIATC06L;
042400121009         ENDDO;
042500121008
042600121008         write  TICRM000;
042700121008
042800121008       ENDSR;
042900121009
043000121009       //--------------------------------------------------------------
043100121009       //?Storicizzo le INFO trattativa.
043200121009       //--------------------------------------------------------------
043300121009       BEGSR Sto_INFO;
043400121010
043500121010         clear nrCONC;
043600121011         clear wPFT;
043700121011         clear wRMS;
043800121011         clear wSNA;
043900121012         clear sav_CPNhim;
044000121009
044100121009       //?Parto a leggere dalla SK delle INFO ordinata
044200121009         xx = 1;
044300121009         FOR xx by 1 to %elem(ITOor);
044400121009
044500121108           IF  ITOor(xx) = *blanks;
044600121009             iter;
044700121009           ENDIF;
044800121009
044900121009           wITO = %subst(ITOor(xx):6:3);
045000121009
045100121009         //?Recupero la ds della INFO
045200121009           yy = %lookup(wITO:ITO);
045300121009           IF yy > 0;
045400121009             dITO = ITOds(yy);
045500121009           ENDIF;
045600121009         //?in caso di trattativa di 'Aumento/Sconto'
045700121009         //?devo modificare la descrizione
045800121009           IF  VIStpv = 'A' and wITO = 'TRT';
045900121009             §ITOdes = 'Valore Aumento/Sconto';
046000121009           ENDIF;
046100121012
046200121012           clear yy;
046300121009
046400121009         //?Aggancio le INFO trattativa
046500121009           chain (VISnrv : wITO) TIVII01L;
046600121108         //?La RMS non la troverò  mai sul file, è una INFO calcolata
046700121108         //?automaticamente dal pgm e non memorizzata sul file
046800121108           IF  not %found(TIVII01L) and wITO <> 'RMS';
046900121010             iter;
047000121010           ENDIF;
047100121012
047200121012           clear wDES;
047300121012           clear wTVA;
047400121012           clear wVAL;
047500121009
047600121010         //?INFO 'TRT'
047700121010           IF  wITO = 'TRT' and VIIpft <> 0;
047800121011             wDES = §ITOdes;
047900121011             IF  VIStpv <> 'A';
048000121011               wDES = %trim(wDES) + ' Annuo';
048100121011             ENDIF;
048200121012             wVAL = %trim(%editc(VIIpft:'J'));
048300121011             wTVA = VIIvft;
048400121011             exsr Sistema_INFO;
048500121010             IF  VIIsna > 0;
048600121012               wDES = 'Spedizioni Annue';
048700121012               wVAL = %trim(%editc(VIIsna:'J'));
048800121012               clear wTVA;
048900121011               exsr Sistema_INFO;
049000121010             ENDIF;
049100121011             wPFT = VIIpft;
049200121011             wSNA = VIIsna;
049300121010           ENDIF;
049400121009
049500121010         //?INFO 'TRE' - NO per trattativa di tipo 'A'
049600121010           IF  wITO = 'TRE' and VIStpv <> 'A' and VIIpft <> 0;
049700121011             wDES = §ITOdes;
049800121012             wVAL = %trim(%editc(VIIpft:'J'));
049900121011             wTVA = VIIvft;
050000121011             exsr Sistema_INFO;
050100121010             IF  VIIsna > 0;
050200121012               wDES = 'Spedizioni Annue';
050300121012               wVAL = %trim(%editc(VIIsna:'J'));
050400121012               clear wTVA;
050500121011               exsr Sistema_INFO;
050600121010             ENDIF;
050700121010           ENDIF;
050800121009
050900121010         //?INFO 'A/S' - SOLO per trattativa di tipo 'A'
051000121010           IF  wITO = 'A/S' and VIStpv = 'A' and VIIvald <> 0;
051100121011             wDES = §ITOdes;
051200121012             wVAL = %trim(%editc(VIIvald:'J'));
051300121011             wTVA = VIIvft;
051400121011             exsr Sistema_INFO;
051500121010           ENDIF;
051600121009
051700121010         //?INFO 'DEL'
051800121010           IF  wITO = 'DEL';
051900121011             wDES = §ITOdes;
052000121012             wVAL = %trim(%editc(VIIval:'J'));
052100121011             wTVA = VIItva;
052200121011             exsr Sistema_INFO;
052300121010           ENDIF;
052400121108
052500121108         //?INFO 'RMS' - NO per trattativa di tipo 'A'
052600121108           IF  wITO = 'RMS' and VIStpv <> 'A' and wPFT <> 0;
052700121108             clear wDES;
052800121108             clear wTVA;
052900121108             clear wVAL;
053000121108             wDES = §ITOdes;
053100121108             wRMS = wPFT / wSNA;
053200121108             wVAL = %trim(%editc(wRMS:'J'));
053300121108             wTVA = §ITOtva;
053400121108             exsr Sistema_INFO;
053500121108           ENDIF;
053600121010
053700121010         //?INFO 'KMS' - NO per trattativa di tipo 'A'
053800121010           IF  wITO = 'KMS' and VIStpv <> 'A';
053900121011             wDES = §ITOdes;
054000121012             wVAL = %trim(%editc(VIIval:'J'));
054100121011             wTVA = VIItva;
054200121011             exsr Sistema_INFO;
054300121010           ENDIF;
054400121009
054500121010         //?INFO '10'
054600121010           IF  wITO = '10';
054700121010             clear dITS;
054800121010             clear TIBS02ds;
054900121010             T02mod = 'C';
055000121010             T02cod = 'ITS';
055100121010             T02ke1 = wITO;
055200121010             T02ke2 = VIIspf;
055300121010             T02sif = KNSIF;
055400121010             TNTBE_RicercaControllo (kpjba : tibs02ds);
055500121010             dITS = T02uni;
055600121112             wDES = 'Concorrenti:' + ' ' + %trim(§ITSdes);
055700121112             exsr Sistema_INFO;
055800121112             exsr Info_CONC;
055900121011           ENDIF;
056000121009
056100121009         ENDFOR;
056200121012
056300121012       //?Se a questo punto ho le note piene le devo scrivere
056400121012         IF  wNOTA <> *blanks;
056500121012           exsr Scrivi_Note;
056600121012           clear wNOTA;
056700121012         ENDIF;
056800121009
056900121009       ENDSR;
057000121112
057100121112       //--------------------------------------------------------------
057200121112       //?Continuo a leggere le INFO dei concorrenti.
057300121112       //--------------------------------------------------------------
057400121112       BEGSR Info_CONC;
057500121112
057600121112         reade (VISnrv : wITO) TIVII01L;
057700121112         DOW  not %eof(TIVII01L);
057800121112           clear dITS;
057900121112           clear TIBS02ds;
058000121112           T02mod = 'C';
058100121112           T02cod = 'ITS';
058200121112           T02ke1 = wITO;
058300121112           T02ke2 = VIIspf;
058400121112           T02sif = KNSIF;
058500121112           TNTBE_RicercaControllo (kpjba : tibs02ds);
058600121112           dITS = T02uni;
058700121112           wDES = §ITSdes;
058800121112           exsr Sistema_INFO;
058900121112           reade (VISnrv : wITO) TIVII01L;
059000121112         ENDDO;
059100121112
059200121112       ENDSR;
059300121011
059400121011       //--------------------------------------------------------------
059500121011       //?Aggiusta campi note x INFO.
059600121011       //--------------------------------------------------------------
059700121011       BEGSR Sistema_INFO;
059800121011
059900121011         clear wstringa;
060000121011         clear aa;
060100121011         clear bb;
060200121011
060300121011         wstringa = %trim(wDES) + ' ' +
060400121011                    %trim(wVAL) + ' ' +
060500121011                    %trim(wTVA);
060600121012         aa = %scan('  ':wstringa);
060700121012         bb = %scan('  ':wNOTA);
060800121012
060900121012         IF  (aa + 3 + bb) > %len(wNOTA);
061000121012           exsr Scrivi_Note;
061100121012           clear wNOTA;
061200121012         ENDIF;
061300121011
061400121012         IF  wNOTA = *blanks;
061500121012           wNOTA = %trim(wstringa);
061600121012           leavesr;
061700121108         ENDIF;
061800121012
061900121012         wNOTA = %trim(wNOTA) + ' - ' + %trim(wstringa);
062000121011
062100121011       ENDSR;
062200121009
062300121009       //--------------------------------------------------------------
062400121009       //?Scrivo le note.
062500121009       //--------------------------------------------------------------
062600121009       BEGSR Scrivi_Note;
062700121009
062800121025         clear TICPN000;
062900121012         CPNcpo = VIScpo;
063000121009         IF  VISksc > 0;
063100121012           CPNksc = VISksc;
063200121009         ENDIF;
063300121012         CPNnrv = VISnrv;
063400121025         CPNdim = %dec(%date());
063500121012         wTime  = %dec(%time());
063600121108         IF  sav_CPNhim = *zeros;
063700121108           CPNhim = wTime;
063800121108         ELSE;
063900121108           wTimeISO = %time(sav_CPNhim:*ISO);
064000121012           wTimeISO = wTimeISO + %seconds(1);
064100121012           CPNhim =  %dec(wTimeISO);
064200121012         ENDIF;
064300121012         sav_CPNhim = CPNhim;
064400121012         CPNpno = 1;
064500121012         CPNnot = wNOTA;
064600121012         CPNsns = 'S';
064700121012         CPNpru = 'BATCH';
064800121012         CPNpos = 46;
064900121025         write TICPN000;
065000121009
065100121009       ENDSR;
065200120925
065300120925       //--------------------------------------------------------------
065400120925       //?Operazioni finali.
065500120925       //--------------------------------------------------------------
065600120925       BEGSR RoutEnd;
065700121107
065800121107       //?Reimposto la data passata
065900121107         DataParm = DataSav;
066000120925
066100120925         *inLR = *on;
066200120925         return;
066300120925
066400120925       ENDSR;
066500120925
066600120925      /end-free
