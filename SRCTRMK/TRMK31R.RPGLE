000100160705      //--------------------------------------------------------------
000200160705      //?TRMK31R - INTERROGAZIONE VARIAZIONI POTENZIALI
000300160705      //--------------------------------------------------------------
000400160705     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160705
000600160705      //---------------------------------------------------------------
000700160705      //?Dichiarazione file.
000800160705      //---------------------------------------------------------------
000900160705     fTNCPVT1L  if   e           k disk
001000160705     fTNCPVD1L  if   e           k disk
001100160705     fTNCPO01L  if   e           k disk
001200160705     fAZCMM01L  if   e           k disk
001300160706     fTNTBE01L  if   e           k disk
001400160705     fTRMK31D   cf   e             workstn sfile(MK31S01:NRR1)
001500160705     f                                     sfile(MK31S03:NRR3)
001600160705
001700160705      //---------------------------------------------------------------
001800160705      //?Definizione schiere.
001900160705      //---------------------------------------------------------------
002000160901     d Desprima        s             50    dim(06) ctdata perrcd(1)
002100160901     d Desdopo         s             50    dim(06) ctdata perrcd(1)
002200160705     d Msg             s             78    dim(1)  ctdata perrcd(1)
002300160901     d DatiVariati     s              1    dim(7)
002400160901     d DatiVarEsteso   s              1    dim(7)
002500160705     d TVP             s              1    dim(25)
002600160705     d TVPdes          s              7    dim(25)
002700160705
002800160705      //---------------------------------------------------------------
002900160705      //?Definizione aree dati.
003000160705      //---------------------------------------------------------------
003100160705      // - Dati utente
003200160705     d §AzUte        e ds                  extname(AZUTE00F)
003300160705     d                                     dtaara
003400160705     d §DatiUte      e ds                  extname(dDatiUte)
003500160705     d                                     dtaara
003600160705
003700160705      //---------------------------------------------------------------
003800160705      //?Definizione strutture dati.
003900160705      //---------------------------------------------------------------
004000160705      // - Parametri ricevuti
004100160705     d TRMK31DS      e ds
004200160705
004300160705     d KPJBA         e ds
004400160705
004500160705      // - Reperimento Tabella
004600160705     d TIBS02DS      e ds
004700160705
004800160705      // - Reperimento dati utente
004900160705     d TIBS34DS      e ds
005000160705
005100160705      // - Ds Letture Variazioni
005200160705     d dcpv_a        e ds
005300160705     d dcpv_b        e ds
005400160705     d dcpv_c        e ds
005500160705     d dcpv_d        e ds
005600160706     d dcpv_e        e ds
005700160901     d dcpv_f        e ds                  prefix(f_)
005800160901     d dcpv_g        e ds                  prefix(g_)
005900160705
006000160705      // - Ds Tabella TVP
006100160705     d dTVP          e ds
006200160706
006300160706       // - Parametri per pgm. controllo/inversione data?
006400160706     d wlbdat          ds                  inz
006500160706     d  G08dat                 1      8  0
006600160706     d  G08ggmm                1      4  0
006700160706     d  G08aa                  7      8  0
006800160706     d  G08inv                 9     16  0
006900160706     d  G08err                17     17
007000160706     d  G08tgi                18     22  0
007100160705
007200160705      //---------------------------------------------------------------
007300160705      //?Definizione variabili globali.
007400160705      //---------------------------------------------------------------
007500160705      // - Flag Booleani
007600160706     d OkRecord        s               n   inz(*off)
007700160705     d Sintetico       s               n   inz(*off)
007800160706
007900160706      // - Indici di schiera
008000160706     d xx              s              2s 0 inz
008100160706     d yy              s              2s 0 inz
008200160705
008300160705      // - Campi di comodo data
008400160705     d wData           s              6s 0
008500160705     d DataISO         s               d   datfmt(*iso)
008600160705     d DataDMY         s               d   datfmt(*dmy)
008700160705
008800160705      // - Campi di comodo video
008900160705     d nrr1            s              4s 0 inz
009000160705     d nrr3            s              4s 0 inz
009100160705     d Video           s              2a   inz
009200160705
009300160705      // - Campi di comodo
009400160706     d desbreve        s                   like(§TVPdes1)
009500160706     d priSelezione    s                   like(V1Csel)
009600160706     d savSelezione    s                   like(V1Csel)
009700160705
009800160705      //---------------------------------------------------------------
009900160705      //?Definizione Prototipi.
010000160705      //---------------------------------------------------------------
010100160705      /copy gaitrasrc/srcprotopr,TIBS02R
010200160705      /copy gaitrasrc/srcprotopr,TIBS34R
010300160706      /copy gaitrasrc/srcprotopr,XSRDA8
010400160705
010500160705      //---------------------------------------------------------------
010600160705      //?Definizione Indicatori.
010700160705      //---------------------------------------------------------------
010800160706      *  15 ON   visualizza opzione 5 in testata X SINTETICO
010900160706      *  16 ON   REVERS IM. per identificare linea di analitico richiesta
011000160706      *  20 ON   SFLDSP
011100160706      *  21 ON   SFLDSPCTL
011200160706      *  21 OFF  SFLCLR
011300160706      *  23 ON   SFLEND
011400160706      *  28 ON   emetto campo messaggio
011500160706      *  30 Lettura comodo
011600160706      *  31 Lettura comodo
011700160706      *  50 non visualizza nell'analitico data e ora variazione se + righe
011800160706      *  51 sottilineatura divisione variazioni analitiche
011900160706      *  52 Proteggo campo selezione se non c'e' dettaglio
012000160705
012100160705      //---------------------------------------------------------------
012200160705      //?M A I N - L I N E
012300160705      //---------------------------------------------------------------
012400160705     c     *Entry        plist
012500160705     c                   parm                    kpjba
012600160705
012700160705      /free
012800160705
012900160705       //?Operazioni iniziali
013000160705       exsr RoutInz;
013100160705
013200160705       //?Carico i dati della testata
013300160705       exsr Testata;
013400160705
013500160706       savSelezione = '9';
013600160706       Sintetico = *on;
013700160706
013800160707       DOW  not *inKL and V1Csel <> savSelezione;
013900160906         exsr RieS01;
014000160705
014100160906         //?Emissione Subfile Variazioni
014200160906         IF  nrr1 > 0;
014300160906           *in20 = *on;
014400160906           DOW  not *inKL and V1Csel = savSelezione;
014500160706
014600160906           //?Sintetico
014700160906             IF  Sintetico;
014800160906               exsr EmettiSinte;
014900160906             ENDIF;
015000160706
015100160906           //?Analitico
015200160906             IF  not Sintetico;
015300160906               exsr EmettiAnali;
015400160906             ENDIF;
015500160705
015600160906           ENDDO;
015700160906         ENDIF;
015800160706
015900160906         //?No dati
016000160906         IF  nrr1 = 0;
016100160906           exsr EmettiNoDati;
016200160906         ENDIF;
016300160707       ENDDO;
016400160705
016500160705       //?Operazioni finali
016600160705       exsr RoutEnd;
016700160705
016800160705       //--------------------------------------------------------------
016900160705       //?Operazioni iniziali.
017000160705       //--------------------------------------------------------------
017100160705       BEGSR RoutInz;
017200160705
017300160705         TRMK31DS = kpjbu;
017400160705
017500160705       //?Reperimento dati job
017600160705         exsr DatiJob;
017700160706
017800160706       //?Carico tabella Tipi Variazioni
017900160706         clear xx;
018000160706         TBEcod = 'TVP';
018100160706         setll (TBEcod) TNTBE01L;
018200160706         reade (TBEcod) TNTBE01L;
018300160706         DOW  not %eof(TNTBE01L);
018400160706           IF  TBEatb = *blanks;
018500160706              xx += 1;
018600160706             dTVP = TBEuni;
018700160706             TVP(xx) = TBEke1;
018800160706             TVPdes(xx) = §TVPdes1;
018900160706           ENDIF;
019000160706           reade (TBEcod) TNTBE01L;
019100160706         ENDDO;
019200160705
019300160705       ENDSR;
019400160705
019500160705       //--------------------------------------------------------------
019600160705       //?Reperimento Dati del job (Utente/Operativi).
019700160705       //--------------------------------------------------------------
019800160705       BEGSR DatiJob;
019900160705
020000160705         in(E) §AzUte;
020100160705         IF  NOT %error;
020200160705           in(E) §DatiUte;
020300160705         ENDIF;
020400160705         IF  %error or RSut = *blanks;
020500160705           clear TIBS34ds;
020600160705           tibs34r(tibs34ds);
020700160705           in §AzUte;
020800160705           in §DatiUte;
020900160705         ENDIF;
021000160705
021100160705       ENDSR;
021200160705
021300160705       //--------------------------------------------------------------
021400160705       //?Dati di Testata.
021500160705       //--------------------------------------------------------------
021600160705       BEGSR Testata;
021700160705
021800160705       //?Imposto i dati di testata
021900160705         clear V1Crag;
022000160705         V1Ccpo = IMK31cpo;
022100160705         IF  IMK31rag = *blanks;
022200160705           chain (V1Ccpo) TNCPO01L;
022300160705           IF  %found(TNCPO01L);
022400160705             V1Crag = CPOrag;
022500160705           ENDIF;
022600160705         ELSE;
022700160705           V1Crag = IMK31rag;
022800160705         ENDIF;
022900160705
023000160705       ENDSR;
023100160705
023200160705       //--------------------------------------------------------------
023300160705       //?Imposto dati Subfile S01.
023400160705       //--------------------------------------------------------------
023500160705       BEGSR RieS01;
023600160705
023700160705         *in20 = *off;
023800160705         *in21 = *off;
023900160705         *in23 = *off;
024000160705
024100160705       //?Emissione Testata e Piede con tasti funzionali abilitati
024200160705         write  MK31C01;
024300160705         write  MK31C03;
024400160705
024500160705         *in21 = *on;
024600160705         *in23 = *on;
024700160705
024800160705         clear nrr1;
024900160705         clear nrr3;
025000160705
025100160705         setll (IMK31cpo) TNCPVT1L;
025200160705         reade (IMK31cpo) TNCPVT1L;
025300160705         DOW  not %eof(TNCPVT1L);
025400160705
025500160705           IF  V1Csel = *blanks;
025600160706             OkRecord = *on;
025700160705           ELSE;
025800160706             OkRecord = *off;
025900160705           ENDIF;
026000160706
026100160706         //?Data variazione
026200160706           clear wlbdat;
026300160706           G08inv = CPVdav;
026400160706           G08err = '3';
026500160706           XSRDA8(WLBdat);
026600160706           V1Sdav = G08ggmm * 100;
026700160706           V1Sdav += G08aa;
026800160711           V3Sdav = V1Sdav;
026900160706
027000160706         //?Decodifico pgm da tabella PVP
027100160706           clear TIBS02DS;
027200160706           T02mod = 'C';
027300160706           T02sif = knsif;
027400160706           T02cod = 'PVP';
027500160706           T02ke1 = CPVpgm;
027600160706           TNTBE_RicercaControllo (kpjba:tibs02ds);
027700160706           IF  T02err = *blanks;
027800160706             V1Spgm = T02uni;
027900160706           ELSE;
028000160706             V1Spgm = *all'?';
028100160706           ENDIF;
028200160706
028300160706         //?Scrivo la riga Inserimento
028400160706           clear V1Sins;
028500160706           IF  CPVcta <> 'M';
028600160706             V1Sins = Desprima(05);
028700160706           ENDIF;
028800160706
028900160706           V1Sorv = CPVorv;
029000160706           V3Sorv = CPVorv;
029100160706           V1Spru = CPVpru;
029200160706           V3Spru = CPVpru;
029300160706           V1Snoj = CPVnoj;
029400160706           V3Snoj = CPVnoJ;
029500160706
029600160706           clear V1Sana;
029700160706           clear V1Spiv;
029800160706           clear V1Smer;
029900160706           clear V1Sfat;
030000160706           clear V1Scom;
030100160901           clear V1Sfu;
030200160901           clear V1Sun;
030300160706
030400160706           clear DatiVarEsteso;
030500160902           clear DatiVariati;
030600160706           xx = 1;
030700160706           FOR  xx by 1 to %elem(DatiVariati);
030800160706             IF  %subst(CPVflo:(xx):1) <> *blanks;
030900160706               DatiVariati(xx) = %subst(CPVflo:(xx):1);
031000160706             ENDIF;
031100160706           ENDFOR;
031200160706
031300160706           xx = 1;
031400160706           FOR  xx by 1 to %elem(DatiVariati);
031500160706             IF  DatiVariati(xx) <> *blanks;
031600160706               IF  DatiVariati(xx) = 'A';
031700160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
031800160706                   OkRecord = *on;
031900160706                 ENDIF;
032000160706                 V1Sana = 'SI';
032100160706               ENDIF;
032200160706               IF  DatiVariati(xx) = 'B';
032300160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
032400160706                   OkRecord = *on;
032500160706                 ENDIF;
032600160706                 V1Spiv = 'SI';
032700160706               ENDIF;
032800160706               IF  DatiVariati(xx) = 'C';
032900160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
033000160706                   OkRecord = *on;
033100160706                 ENDIF;
033200160706                 V1Smer = 'SI';
033300160706               ENDIF;
033400160706               IF  DatiVariati(xx) = 'D';
033500160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
033600160706                   OkRecord = *on;
033700160706                 ENDIF;
033800160706                 V1Sfat = 'SI';
033900160706               ENDIF;
034000160706               IF  DatiVariati(xx) = 'E';
034100160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
034200160706                   OkRecord = *on;
034300160706                 ENDIF;
034400160706                 V1Scom = 'SI';
034500160706               ENDIF;
034600160901               IF  DatiVariati(xx) = 'F';
034700160901                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
034800160901                   OkRecord = *on;
034900160901                 ENDIF;
035000160901                 V1Sfu  = 'SI';
035100160901               ENDIF;
035200160901               IF  DatiVariati(xx) = 'G';
035300160901                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
035400160901                   OkRecord = *on;
035500160901                 ENDIF;
035600160901                 V1Sun  = 'SI';
035700160901               ENDIF;
035800160706             ENDIF;
035900160706           ENDFOR;
036000160706
036100160706         //?Scrivo dati per visualizzazione Analitica
036200160706           IF  OkRecord;
036300160706             exsr ScriviAnalitico;
036400160706           //?Scrivo un rcd per dati variati ma non registrati
036500160706             IF  CPVcta = 'M';
036600160706               exsr DatiNoReg;
036700160706             ENDIF;
036800160706           //?Almeno una riga è stata scritta, aggancio l'ultima per sottolinearla
036900160706             chain nrr3 MK31S03;
037000160706             IF  %found;
037100160706               *inKL = *off;
037200160706               *in50 = in50;
037300160706               *in51 = in51;
037400160706               IF  not *in50;
037500160706                 clear V3Spru;
037600160706                 clear V3Snoj;
037700160706                 clear V3Sorv;
037800160706                 clear V3Sdav;
037900160706               ENDIF;
038000160706               *in50 = *on;
038100160706               *in51 = *on;
038200160706               in50 = *in50;
038300160706               in51 = *in51;
038400160706               update MK31S03;
038500160706             ENDIF;
038600160706
038700160706           //?Se non è stata scritta nessuna riga proteggo il campo di scelta
038800160706             IF  V1Sreca = 0;
038900160706               *in52 = *on;
039000160706             ENDIF;
039100160706             in52 = *in52;
039200160706             nrr1 += 1;
039300160706             write MK31S01;
039400160706             *in52 = *off;
039500160706           ENDIF;
039600160706
039700160706           reade (IMK31cpo) TNCPVT1L;
039800160706
039900160705         ENDDO;
040000160706
040100160706         savSelezione = V1Csel;
040200160705
040300160705       ENDSR;
040400160706
040500160706       //--------------------------------------------------------------
040600160706       //?Scrivo subfile Analitico.
040700160706       //--------------------------------------------------------------
040800160706       BEGSR ScriviAnalitico;
040900160706
041000160706         *in16 = *off;
041100160706         *in50 = *on;
041200160706         *in51 = *off;
041300160706         clear V1Sreca;
041400160706
041500160706       //?Se non è Manutenzione non carico la DS
041600160706         IF  CPVcta <> 'M';
041700160706           exsr ScriviRigaIns;
041800160706           leavesr;
041900160706         ENDIF;
042000160706
042100160706         IF  V1Csel = *blanks;
042200160706           setll (IMK31cpo:CPVdav:CPVorv:CPVnoj) TNCPVD1L;
042300160706           reade (IMK31cpo:CPVdav:CPVorv:CPVnoj) TNCPVD1L;
042400160706         ELSE;
042500160706           setll (IMK31cpo:CPVdav:CPVorv:CPVnoj:V1Csel) TNCPVD1L;
042600160706           reade (IMK31cpo:CPVdav:CPVorv:CPVnoj:V1Csel) TNCPVD1L;
042700160706         ENDIF;
042800160706
042900160706         DOW  not %eof(TNCPVD1L);
043000160706
043100160706         //?Se nessun dato variato rcd generico
043200160706           IF  CPVprima = CPVdopo;
043300160706             xx = %lookup(CPVtvp:DatiVariati);
043400160706             IF  xx > 0;
043500160706               DatiVarEsteso(xx) = 'N';
043600160706             ENDIF;
043700160706           ELSE;
043800160706
043900160706           //?Decodifico i dati variati
044000160706             xx = %lookup(CPVtvp:TVP);
044100160706             IF  xx > 0;
044200160706               V1Sdt1 = TVPdes(xx);
044300160706             ELSE;
044400160706               V1Sdt1 = CPVtvp;
044500160706             ENDIF;
044600160706
044700160706             SELECT;
044800160706             //WHEN  CPVtvp = 'A';
044900160706               //exsr Carica_a;
045000160706             WHEN  CPVtvp = 'B';
045100160706               exsr Carica_b;
045200160706             WHEN  CPVtvp = 'C';
045300160706               exsr Carica_c;
045400160706             WHEN  CPVtvp = 'D';
045500160706               exsr Carica_d;
045600160706             WHEN  CPVtvp = 'E';
045700160706               exsr Carica_e;
045800160901             WHEN  CPVtvp = 'F';
045900160901               exsr Carica_f;
046000160901             WHEN  CPVtvp = 'G';
046100160901               exsr Carica_g;
046200160706             ENDSL;
046300160706           ENDIF;
046400160706
046500160706           IF  V1Csel = *blank;
046600160706             reade (IMK31cpo:CPVdav:CPVorv:CPVnoj) TNCPVD1L;
046700160706           ELSE;
046800160706             reade (IMK31cpo:CPVdav:CPVorv:CPVnoj:V1Csel) TNCPVD1L;
046900160706           ENDIF;
047000160706         ENDDO;
047100160706
047200160706       ENDSR;
047300160706
047400160706       //--------------------------------------------------------------
047500160706       //?Emissione Subfile Sintetico.
047600160706       //--------------------------------------------------------------
047700160706       BEGSR EmettiSinte;
047800160706
047900160706         *inKI = *off;
048000160706         Sintetico = *on;
048100160706
048200160706       //?F9 - Cambia da Sintetito ad Analitico
048300160706         DOW  not *inKI and not *inKL and V1csel = savSelezione;
048400160706           nrr1 = 1;
048500160706           rec1 = 1;
048600160906           write MK31T01;
048700160906           write MK31Z01;
048800160906      /end-free
048900160906     c     for01         tag
049000160906      /free
049100160706           exfmt MK31C01;
049200160706           *in28 = *off;
049300160706           IF  not *inKL;
049400160706             exsr ControllaSel;
049500160906         //?emetto msg errore
049600160906             IF  *in28;
049700160906      /end-free
049800160906     c                   goto      for01
049900160906      /free
050000160906             ENDIF;
050100160906         //?verifico se richiesta opzione 5
050200160906             exsr LeggiSfl;
050300160706           ENDIF;
050400160706           IF  *inKI;
050500160706             Sintetico = *off;
050600160706           ENDIF;
050700160706         ENDDO;
050800160706
050900160706       ENDSR;
051000160706
051100160706       //--------------------------------------------------------------
051200160706       //?Emissione Subfile Analitico.
051300160706       //--------------------------------------------------------------
051400160706       BEGSR EmettiAnali;
051500160706
051600160706         *inKI = *off;
051700160706         Sintetico = *off;
051800160706
051900160706       //?F9 - Cambia da Analitco a Sintetico
052000160706         DOW  not *inKI and not *inKL and V1csel = savSelezione;
052100160706           IF  not *in16;
052200160706             nrr3 = 1;
052300160706             rec3 = 1;
052400160706           ENDIF;
052500160906           write MK31T01;
052600160906           write MK31Z03;
052700160906      /end-free
052800160906     c     for03         tag
052900160906      /free
053000160706           exfmt MK31C03;
053100160706           *in28 = *off;
053200160706           IF  not *inKL;
053300160706             exsr ControllaSel;
053400160906             IF  *in28;
053500160906      /end-free
053600160906     c                   goto      for03
053700160906      /free
053800160906             ENDIF;
053900160706           ENDIF;
054000160706           IF  *inKI;
054100160706             Sintetico = *on;
054200160706           ENDIF;
054300160706         ENDDO;
054400160706
054500160706       ENDSR;
054600160906
054700160906       //--------------------------------------------------------------
054800160906       //?Emissione Subfile Vuoto No Dati.
054900160906       //--------------------------------------------------------------
055000160906       BEGSR EmettiNoDati;
055100160906
055200160906         DOW  not *inKL and V1Csel = savSelezione;
055300160906           write MK31T01;
055400160906           write MK31Z01;
055500160906           write MK31D01;
055600160906      /end-free
055700160906     c     fornodati     tag
055800160906      /free
055900160906           exfmt MK31C01;
056000160906           *in28 = *off;
056100160906
056200160906           IF  not *inKL;
056300160906             exsr ControllaSel;
056400160906             IF  *in28;
056500160906      /end-free
056600160906     c                   goto      fornodati
056700160906      /free
056800160906             ENDIF;
056900160906           ENDIF;
057000160906         ENDDO;
057100160906
057200160906       ENDSR;
057300160706
057400160706       //--------------------------------------------------------------
057500160706       //?Scrivo Dati non Registrati.
057600160706       //--------------------------------------------------------------
057700160706       BEGSR DatiNoReg;
057800160706
057900160706         clear V1Sdt1;
058000160706         clear V1Sva2;
058100160706         V1Sva1 = DesPrima(06);
058200160706
058300160706         xx = 1;
058400160706         FOR  xx by 1 to %elem(DatiVariati);
058500160706           IF  DatiVariati(xx) <> *blanks and DatiVarEsteso(xx) <> 'S';
058600160706
058700160706         //?Memorizzo se in questo rcd c'è il tipo dati selezionato
058800160706             IF  (V1Csel <> *blanks and DatiVariati(XX) = V1Csel) or
058900160706                  V1Csel = *blanks;
059000160706               yy =  %lookup(DatiVariati(xx):TVP);
059100160706               IF  yy > 0;
059200160706                 desbreve = TVPdes(yy);
059300160706               ELSE;
059400160706                 desbreve = *all'?';
059500160706               ENDIF;
059600160707               V1Sva2 = %trim(V1Sva2) + ' ' + desbreve;
059700160706             ENDIF;
059800160706
059900160706           ENDIF;
060000160706         ENDFOR;
060100160706
060200160706         IF  V1Sva2 <> *blanks;
060300160706           V1Sva2 = %trim(V1Sva2);
060400160706           exsr RieS03;
060500160706         ENDIF;
060600160706
060700160706       ENDSR;
060800160706
060900160706       //--------------------------------------------------------------
061000160706       //?Scrivo la riga di Inserimento.
061100160706       //--------------------------------------------------------------
061200160706       BEGSR ScriviRigaIns;
061300160706
061400160706         clear V1Sdt1;
061500160706         clear V1Sva1;
061600160706         V1Sva2 = DesPrima(05);
061700160706         *in51 = *on;
061800160706         exsr RieS03;
061900160706         *in51 = *off;
062000160706
062100160706       ENDSR;
062200160706
062300160706       //--------------------------------------------------------------
062400160706       //?Scrivo la riga Analitica.
062500160706       //--------------------------------------------------------------
062600160706       BEGSR RieS03;
062700160706
062800160706         nrr3 += 1;
062900160706         in50 = *in50;
063000160706         in51 = *in51;
063100160706         write MK31S03;
063200160706
063300160706       //?Imposto se è il primo rcd analitico
063400160706         IF  V1Sreca = 0;
063500160706           V1Sreca = nrr3;
063600160706         ENDIF;
063700160707         *in50 = *off;
063800160706
063900160706       //?Memorizzo il rcd dati variati scritto in analitico
064000160706         xx = %lookup(CPVtvp:DatiVariati);
064100160706         IF  xx > 0;
064200160706           DatiVarEsteso(xx) = 'S';
064300160706         ENDIF;
064400160706
064500160706       ENDSR;
064600160706
064700160706       //--------------------------------------------------------------
064800160706       //?Carico dati RCD B.
064900160706       //--------------------------------------------------------------
065000160706       BEGSR Carica_b;
065100160706
065200160706       //?Riga prima
065300160706         dCPV_b = CPVprima;
065400160706         V1Sva1 = DesPrima(01);
065500160706         %subst(V1Sva1:8:16) = CPOpiv;
065600160706         %subst(V1Sva1:32:16) = CPOcdf;
065700160706
065800160706       //?Riga dopo
065900160706         dCPV_b = CPVdopo;
066000160706         V1Sva2 = DesDopo(01);
066100160706         %subst(V1Sva2:8:16) = CPOpiv;
066200160706         %subst(V1Sva2:32:16) = CPOcdf;
066300160706
066400160706         exsr RieS03;
066500160706
066600160706       ENDSR;
066700160706
066800160706       //--------------------------------------------------------------
066900160706       //?Carico dati RCD C.
067000160706       //--------------------------------------------------------------
067100160706       BEGSR Carica_c;
067200160706
067300160706       //?Riga prima
067400160706         dCPV_c = CPVprima;
067500160706         V1Sva1 = DesPrima(02);
067600160706         %subst(V1Sva1:12:5) = %editc(CPOsct:'X');
067700160706         %subst(V1Sva1:33:1) = CPOfsf;
067800160706
067900160706       //?Riga dopo
068000160706         dCPV_c = CPVdopo;
068100160706         V1Sva2 = DesDopo(02);
068200160706         %subst(V1Sva2:12:5) = %editc(CPOsct:'X');
068300160706         %subst(V1Sva2:33:1) = CPOfsf;
068400160706
068500160706         exsr RieS03;
068600160706
068700160706       ENDSR;
068800160706
068900160706       //--------------------------------------------------------------
069000160706       //?Carico dati RCD D.
069100160706       //--------------------------------------------------------------
069200160706       BEGSR Carica_d;
069300160706
069400160706       //?Riga prima
069500160706         dCPV_d = CPVprima;
069600160706         V1Sva1 = DesPrima(03);
069700160706         %subst(V1Sva1:11:14) = %editc(CPIpft:'2');
069800160831         IF  CPIpft = 0 and §CPOsptp > 0;
069900160831           %subst(V1Sva1:11:14) = %editc(§CPOsptp:'2');
070000160831           %subst(V1Sva1:27:8) = 'PRESUNTO';
070100160831         ENDIF;
070200160831         %subst(V1Sva1:49:1) = CPOftr;
070300160706
070400160706       //?Riga dopo
070500160706         dCPV_d = CPVdopo;
070600160706         V1Sva2 = DesDopo(03);
070700160706         %subst(V1Sva2:11:14) = %editc(CPIpft:'2');
070800160831         IF  CPIpft = 0 and §CPOsptp > 0;
070900160831           %subst(V1Sva2:11:14) = %editc(§CPOsptp:'2');
071000160831           %subst(V1Sva2:27:8) = 'PRESUNTO';
071100160831         ENDIF;
071200160831         %subst(V1Sva2:49:1) = CPOftr;
071300160706
071400160706         exsr RieS03;
071500160706
071600160706       ENDSR;
071700160706
071800160706       //--------------------------------------------------------------
071900160706       //?Carico dati RCD E.
072000160706       //--------------------------------------------------------------
072100160706       BEGSR Carica_e;
072200160706
072300160706       //?Riga prima
072400160706         dCPV_e = CPVprima;
072500160706         V1Sva1 = DesPrima(04);
072600160706         %subst(V1Sva1:14:7) = %editc(CPOcmm:'X');
072700160711         %subst(V1Sva1:37:3) = %editc(CPOflt:'X');
072800160706
072900160706       //?Riga dopo
073000160706         dCPV_e = CPVdopo;
073100160706         V1Sva2 = DesDopo(04);
073200160706         %subst(V1Sva2:14:7) = %editc(CPOcmm:'X');
073300160711         %subst(V1Sva2:37:3) = %editc(CPOflt:'X');
073400160706
073500160706         exsr RieS03;
073600160706
073700160706       ENDSR;
073800160901
073900160901       //--------------------------------------------------------------
074000160901       //?Carico dati RCD F.
074100160901       //--------------------------------------------------------------
074200160901       BEGSR Carica_f;
074300160901
074400160901       //?Riga prima
074500160901         dCPV_f = CPVprima;
074600160907         clear V1Sva1;
074700160901
074800160901       //?Riga dopo
074900160901         dCPV_f = CPVdopo;
075000160901         V1Sva2 = DesDopo(05);
075100160901         %subst(V1Sva2:29:11) = %editc(f_CPOcpo:'X');
075200160901
075300160901         exsr RieS03;
075400160901
075500160901       ENDSR;
075600160901
075700160901       //--------------------------------------------------------------
075800160901       //?Carico dati RCD G.
075900160901       //--------------------------------------------------------------
076000160901       BEGSR Carica_g;
076100160901
076200160901       //?Riga prima
076300160901         dCPV_g = CPVprima;
076400160907         clear V1Sva1;
076500160901
076600160901       //?Riga dopo
076700160901         dCPV_g = CPVdopo;
076800160901         V1Sva2 = DesDopo(06);
076900160901         %subst(V1Sva2:30:11) = %editc(g_CPOcpo:'X');
077000160901
077100160901         exsr RieS03;
077200160901
077300160901       ENDSR;
077400160706
077500160706       //--------------------------------------------------------------
077600160706       //?Controllo Selezioni.
077700160706       //--------------------------------------------------------------
077800160706       BEGSR ControllaSel;
077900160706
078000160706         read MK31T01;
078100160706         clear V1Dsel;
078200160706
078300160706         IF  V1Csel = '?';
078400160706           clear TIBS02DS;
078500160706           T02mod = 'R';
078600160706           T02cod = 'TVP';
078700160706           T02sif = knsif;
078800160706           TNTBE_RicercaControllo (kpjba:tibs02ds);
078900160706           V1Csel = T02ke1;
079000160706         ENDIF;
079100160706
079200160706         IF  V1Csel <> *blanks;
079300160706           clear TIBS02DS;
079400160706           T02mod = 'C';
079500160706           T02cod = 'TVP';
079600160706           T02ke1 = V1Csel;
079700160706           T02sif = knsif;
079800160706           TNTBE_RicercaControllo (kpjba:tibs02ds);
079900160706           IF  T02err <> *blanks;
080000160706             V1Cmsg = Msg(01);
080100160706             *in28 = *on;
080200160706             leavesr;
080300160706           ENDIF;
080400160706           dTVP = T02uni;
080500160706           V1Dsel = §TVPdes1;
080600160706         ENDIF;
080700160706
080800160706       ENDSR;
080900160706
081000160706       //--------------------------------------------------------------
081100160706       //?Opzioni immesse sul subfile.
081200160706       //--------------------------------------------------------------
081300160706       BEGSR LeggiSfl;
081400160706
081500160706         priSelezione = savSelezione;
081600160706         savSelezione = V1Csel;
081700160706
081800160706         readc MK31S01;
081900160706         DOW  not %eof;
082000160706           IF  V1Ssce = '5';
082100160706             rec3 = V1Sreca;
082200160706             nrr3 = V1Sreca;
082300160706             chain nrr3 MK31S03;
082400160906             *in16 = *on;
082500160906             *in50 = in50;
082600160906             *in51 = in51;
082700160906             update MK31S03;
082800160706             *inKL = *off;
082900160706             exsr EmettiAnali;
083000160706             chain nrr3 MK31S03;
083100160906             *in16 = *off;
083200160906             *in50 = in50;
083300160906             *in51 = in51;
083400160906             update MK31S03;
083500160706             clear V1Ssce;
083600160706           ENDIF;
083700160706           *in52 = in52;
083800160707           update MK31S01;
083900160706
084000160706           readc MK31S01;
084100160706         ENDDO;
084200160706
084300160706         *inKL = *off;
084400160706         savSelezione = priSelezione;
084500160706         Sintetico = *on;
084600160706
084700160706       ENDSR;
084800160705
084900160705       //--------------------------------------------------------------
085000160705       //?Operazioni finali.
085100160705       //--------------------------------------------------------------
085200160705       BEGSR RoutEnd;
085300160705
085400160705         *inLR = *on;
085500160705
085600160705         return;
085700160705
085800160705       ENDSR;
085900160705
086000160705      /end-free
086100160706
086200160706       //--------------------------------------------------------------
086300160706       //?Schiere a tempo di compilazione.
086400160706       //--------------------------------------------------------------
086500160706
086600160706** -- DESPRIMA-----------------------------------*
086700160706P.IVA "1234567890123456"  C.F."1234567890123456"  01
086800160706Cat.Merc. "12345"  Sede/Filile "1"                02
086900160831Sp.Trasp."12345678901234"          Imp.Cliente "1"03
087000160706Commerciale "1234567" Fil.Gestione "123"          04
087100160706I   N   S   E   R   I   M   E   N   T   O         05
087200160706Dati variati ma non registrati :                  06
087300160706** -- DESDOPO------------------------------------*
087400160706      "1234567890123456"      "1234567890123456"  01
087500160706          "12345"              "1"                02
087600160831         "12345678901234"                      "1"03
087700160706            "1234567"              "123"          04
087800160901Codice Potenziale Fuso con:"12345678901"          05
087900160901Codice Potenziale Unito con:"12345678901"         06
088000060523**
088100060530Selezione  Dati Variati ERRATA
