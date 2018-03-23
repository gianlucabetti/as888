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
002000160706     d Desprima        s             50    dim(06) ctdata perrcd(1)
002100160706     d Desdopo         s             50    dim(04) ctdata perrcd(1)
002200160705     d Msg             s             78    dim(1)  ctdata perrcd(1)
002300160706     d DatiVariati     s              1    dim(5)
002400160706     d DatiVarEsteso   s              1    dim(5)
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
005700160705
005800160705      // - Ds Tabella TVP
005900160705     d dTVP          e ds
006000160706
006100160706       // - Parametri per pgm. controllo/inversione data?
006200160706     d wlbdat          ds                  inz
006300160706     d  G08dat                 1      8  0
006400160706     d  G08ggmm                1      4  0
006500160706     d  G08aa                  7      8  0
006600160706     d  G08inv                 9     16  0
006700160706     d  G08err                17     17
006800160706     d  G08tgi                18     22  0
006900160705
007000160705      //---------------------------------------------------------------
007100160705      //?Definizione variabili globali.
007200160705      //---------------------------------------------------------------
007300160705      // - Flag Booleani
007400160706     d OkRecord        s               n   inz(*off)
007500160705     d Sintetico       s               n   inz(*off)
007600160706
007700160706      // - Indici di schiera
007800160706     d xx              s              2s 0 inz
007900160706     d yy              s              2s 0 inz
008000160705
008100160705      // - Campi di comodo data
008200160705     d wData           s              6s 0
008300160705     d DataISO         s               d   datfmt(*iso)
008400160705     d DataDMY         s               d   datfmt(*dmy)
008500160705
008600160705      // - Campi di comodo video
008700160705     d nrr1            s              4s 0 inz
008800160705     d nrr3            s              4s 0 inz
008900160705     d Video           s              2a   inz
009000160705
009100160705      // - Campi di comodo
009200160706     d desbreve        s                   like(§TVPdes1)
009300160706     d priSelezione    s                   like(V1Csel)
009400160706     d savSelezione    s                   like(V1Csel)
009500160705
009600160705      //---------------------------------------------------------------
009700160705      //?Definizione Prototipi.
009800160705      //---------------------------------------------------------------
009900160705      /copy gaitrasrc/srcprotopr,TIBS02R
010000160705      /copy gaitrasrc/srcprotopr,TIBS34R
010100160706      /copy gaitrasrc/srcprotopr,XSRDA8
010200160705
010300160705      //---------------------------------------------------------------
010400160705      //?Definizione Indicatori.
010500160705      //---------------------------------------------------------------
010600160706      *  15 ON   visualizza opzione 5 in testata X SINTETICO
010700160706      *  16 ON   REVERS IM. per identificare linea di analitico richiesta
010800160706      *  20 ON   SFLDSP
010900160706      *  21 ON   SFLDSPCTL
011000160706      *  21 OFF  SFLCLR
011100160706      *  23 ON   SFLEND
011200160706      *  28 ON   emetto campo messaggio
011300160706      *  30 Lettura comodo
011400160706      *  31 Lettura comodo
011500160706      *  50 non visualizza nell'analitico data e ora variazione se + righe
011600160706      *  51 sottilineatura divisione variazioni analitiche
011700160706      *  52 Proteggo campo selezione se non c'e' dettaglio
011800160705
011900160705      //---------------------------------------------------------------
012000160705      //?M A I N - L I N E
012100160705      //---------------------------------------------------------------
012200160705     c     *Entry        plist
012300160705     c                   parm                    kpjba
012400160705
012500160705      /free
012600160705
012700160705       //?Operazioni iniziali
012800160705       exsr RoutInz;
012900160705
013000160705       //?Carico i dati della testata
013100160705       exsr Testata;
013200160705
013300160706       savSelezione = '9';
013400160706       Sintetico = *on;
013500160706
013600160707       DOW  not *inKL and V1Csel <> savSelezione;
013700160706       exsr RieS01;
013800160705
013900160705       //?Emissione Subfile Variazioni
014000160706       IF  nrr1 > 0;
014100160707         *in20 = *on;
014200160707         DOW  not *inKL and V1Csel = savSelezione;
014300160706
014400160706         //?Sintetico
014500160706           IF  Sintetico;
014600160706             exsr EmettiSinte;
014700160706           ENDIF;
014800160706
014900160706         //?Analitico
015000160706           IF  not Sintetico;
015100160706             exsr EmettiAnali;
015200160706           ENDIF;
015300160705
015400160706         ENDDO;
015500160706       ENDIF;
015600160706
015700160706       //?No dati
015800160706       IF  nrr1 = 0;
015900160706         write MK31T01;
016000160706         write MK31Z01;
016100160706         write MK31D01;
016200160706         DOW  not *inKL and V1Csel = savSelezione;
016300160706           exfmt MK31C01;
016400160706           *IN28 = *off;
016500160706           IF  not *inKL;
016600160706             exsr ControllaSel;
016700160706           ENDIF;
016800160706         ENDDO;
016900160706       ENDIF;
017000160707       ENDDO;
017100160705
017200160705       //?Operazioni finali
017300160705       exsr RoutEnd;
017400160705
017500160705       //--------------------------------------------------------------
017600160705       //?Operazioni iniziali.
017700160705       //--------------------------------------------------------------
017800160705       BEGSR RoutInz;
017900160705
018000160705         TRMK31DS = kpjbu;
018100160705
018200160705       //?Reperimento dati job
018300160705         exsr DatiJob;
018400160706
018500160706       //?Carico tabella Tipi Variazioni
018600160706         clear xx;
018700160706         TBEcod = 'TVP';
018800160706         setll (TBEcod) TNTBE01L;
018900160706         reade (TBEcod) TNTBE01L;
019000160706         DOW  not %eof(TNTBE01L);
019100160706           IF  TBEatb = *blanks;
019200160706              xx += 1;
019300160706             dTVP = TBEuni;
019400160706             TVP(xx) = TBEke1;
019500160706             TVPdes(xx) = §TVPdes1;
019600160706           ENDIF;
019700160706           reade (TBEcod) TNTBE01L;
019800160706         ENDDO;
019900160705
020000160705       ENDSR;
020100160705
020200160705       //--------------------------------------------------------------
020300160705       //?Reperimento Dati del job (Utente/Operativi).
020400160705       //--------------------------------------------------------------
020500160705       BEGSR DatiJob;
020600160705
020700160705         in(E) §AzUte;
020800160705         IF  NOT %error;
020900160705           in(E) §DatiUte;
021000160705         ENDIF;
021100160705         IF  %error or RSut = *blanks;
021200160705           clear TIBS34ds;
021300160705           tibs34r(tibs34ds);
021400160705           in §AzUte;
021500160705           in §DatiUte;
021600160705         ENDIF;
021700160705
021800160705       ENDSR;
021900160705
022000160705       //--------------------------------------------------------------
022100160705       //?Dati di Testata.
022200160705       //--------------------------------------------------------------
022300160705       BEGSR Testata;
022400160705
022500160705       //?Imposto i dati di testata
022600160705         clear V1Crag;
022700160705         V1Ccpo = IMK31cpo;
022800160705         IF  IMK31rag = *blanks;
022900160705           chain (V1Ccpo) TNCPO01L;
023000160705           IF  %found(TNCPO01L);
023100160705             V1Crag = CPOrag;
023200160705           ENDIF;
023300160705         ELSE;
023400160705           V1Crag = IMK31rag;
023500160705         ENDIF;
023600160705
023700160705       ENDSR;
023800160705
023900160705       //--------------------------------------------------------------
024000160705       //?Imposto dati Subfile S01.
024100160705       //--------------------------------------------------------------
024200160705       BEGSR RieS01;
024300160705
024400160705         *in20 = *off;
024500160705         *in21 = *off;
024600160705         *in23 = *off;
024700160705
024800160705       //?Emissione Testata e Piede con tasti funzionali abilitati
024900160705         write  MK31C01;
025000160705         write  MK31C03;
025100160705
025200160705         *in21 = *on;
025300160705         *in23 = *on;
025400160705
025500160705         clear nrr1;
025600160705         clear nrr3;
025700160705
025800160705         setll (IMK31cpo) TNCPVT1L;
025900160705         reade (IMK31cpo) TNCPVT1L;
026000160705         DOW  not %eof(TNCPVT1L);
026100160705
026200160705           IF  V1Csel = *blanks;
026300160706             OkRecord = *on;
026400160705           ELSE;
026500160706             OkRecord = *off;
026600160705           ENDIF;
026700160706
026800160706         //?Data variazione
026900160706           clear wlbdat;
027000160706           G08inv = CPVdav;
027100160706           G08err = '3';
027200160706           XSRDA8(WLBdat);
027300160706           V1Sdav = G08ggmm * 100;
027400160706           V1Sdav += G08aa;
027500160711           V3Sdav = V1Sdav;
027600160706
027700160706         //?Decodifico pgm da tabella PVP
027800160706           clear TIBS02DS;
027900160706           T02mod = 'C';
028000160706           T02sif = knsif;
028100160706           T02cod = 'PVP';
028200160706           T02ke1 = CPVpgm;
028300160706           TNTBE_RicercaControllo (kpjba:tibs02ds);
028400160706           IF  T02err = *blanks;
028500160706             V1Spgm = T02uni;
028600160706           ELSE;
028700160706             V1Spgm = *all'?';
028800160706           ENDIF;
028900160706
029000160706         //?Scrivo la riga Inserimento
029100160706           clear V1Sins;
029200160706           IF  CPVcta <> 'M';
029300160706             V1Sins = Desprima(05);
029400160706           ENDIF;
029500160706
029600160706           V1Sorv = CPVorv;
029700160706           V3Sorv = CPVorv;
029800160706           V1Spru = CPVpru;
029900160706           V3Spru = CPVpru;
030000160706           V1Snoj = CPVnoj;
030100160706           V3Snoj = CPVnoJ;
030200160706
030300160706           clear V1Sana;
030400160706           clear V1Spiv;
030500160706           clear V1Smer;
030600160706           clear V1Sfat;
030700160706           clear V1Scom;
030800160706
030900160706           clear DatiVarEsteso;
031000160706           xx = 1;
031100160706           FOR  xx by 1 to %elem(DatiVariati);
031200160706             IF  %subst(CPVflo:(xx):1) <> *blanks;
031300160706               DatiVariati(xx) = %subst(CPVflo:(xx):1);
031400160706             ENDIF;
031500160706           ENDFOR;
031600160706
031700160706           xx = 1;
031800160706           FOR  xx by 1 to %elem(DatiVariati);
031900160706             IF  DatiVariati(xx) <> *blanks;
032000160706               IF  DatiVariati(xx) = 'A';
032100160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
032200160706                   OkRecord = *on;
032300160706                 ENDIF;
032400160706                 V1Sana = 'SI';
032500160706               ENDIF;
032600160706               IF  DatiVariati(xx) = 'B';
032700160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
032800160706                   OkRecord = *on;
032900160706                 ENDIF;
033000160706                 V1Spiv = 'SI';
033100160706               ENDIF;
033200160706               IF  DatiVariati(xx) = 'C';
033300160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
033400160706                   OkRecord = *on;
033500160706                 ENDIF;
033600160706                 V1Smer = 'SI';
033700160706               ENDIF;
033800160706               IF  DatiVariati(xx) = 'D';
033900160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
034000160706                   OkRecord = *on;
034100160706                 ENDIF;
034200160706                 V1Sfat = 'SI';
034300160706               ENDIF;
034400160706               IF  DatiVariati(xx) = 'E';
034500160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
034600160706                   OkRecord = *on;
034700160706                 ENDIF;
034800160706                 V1Scom = 'SI';
034900160706               ENDIF;
035000160706             ENDIF;
035100160706           ENDFOR;
035200160706
035300160706         //?Scrivo dati per visualizzazione Analitica
035400160706           IF  OkRecord;
035500160706             exsr ScriviAnalitico;
035600160706           //?Scrivo un rcd per dati variati ma non registrati
035700160706             IF  CPVcta = 'M';
035800160706               exsr DatiNoReg;
035900160706             ENDIF;
036000160706           //?Almeno una riga è stata scritta, aggancio l'ultima per sottolinearla
036100160706             chain nrr3 MK31S03;
036200160706             IF  %found;
036300160706               *inKL = *off;
036400160706               *in50 = in50;
036500160706               *in51 = in51;
036600160706               IF  not *in50;
036700160706                 clear V3Spru;
036800160706                 clear V3Snoj;
036900160706                 clear V3Sorv;
037000160706                 clear V3Sdav;
037100160706               ENDIF;
037200160706               *in50 = *on;
037300160706               *in51 = *on;
037400160706               in50 = *in50;
037500160706               in51 = *in51;
037600160706               update MK31S03;
037700160706             ENDIF;
037800160706
037900160706           //?Se non è stata scritta nessuna riga proteggo il campo di scelta
038000160706             IF  V1Sreca = 0;
038100160706               *in52 = *on;
038200160706             ENDIF;
038300160706             in52 = *in52;
038400160706             nrr1 += 1;
038500160706             write MK31S01;
038600160706             *in52 = *off;
038700160706           ENDIF;
038800160706
038900160706           reade (IMK31cpo) TNCPVT1L;
039000160706
039100160705         ENDDO;
039200160706
039300160706         savSelezione = V1Csel;
039400160705
039500160705       ENDSR;
039600160706
039700160706       //--------------------------------------------------------------
039800160706       //?Scrivo subfile Analitico.
039900160706       //--------------------------------------------------------------
040000160706       BEGSR ScriviAnalitico;
040100160706
040200160706         *in16 = *off;
040300160706         *in50 = *on;
040400160706         *in51 = *off;
040500160706         clear V1Sreca;
040600160706
040700160706       //?Se non è Manutenzione non carico la DS
040800160706         IF  CPVcta <> 'M';
040900160706           exsr ScriviRigaIns;
041000160706           leavesr;
041100160706         ENDIF;
041200160706
041300160706         IF  V1Csel = *blanks;
041400160706           setll (IMK31cpo:CPVdav:CPVorv:CPVnoj) TNCPVD1L;
041500160706           reade (IMK31cpo:CPVdav:CPVorv:CPVnoj) TNCPVD1L;
041600160706         ELSE;
041700160706           setll (IMK31cpo:CPVdav:CPVorv:CPVnoj:V1Csel) TNCPVD1L;
041800160706           reade (IMK31cpo:CPVdav:CPVorv:CPVnoj:V1Csel) TNCPVD1L;
041900160706         ENDIF;
042000160706
042100160706         DOW  not %eof(TNCPVD1L);
042200160706
042300160706         //?Se nessun dato variato rcd generico
042400160706           IF  CPVprima = CPVdopo;
042500160706             xx = %lookup(CPVtvp:DatiVariati);
042600160706             IF  xx > 0;
042700160706               DatiVarEsteso(xx) = 'N';
042800160706             ENDIF;
042900160706           ELSE;
043000160706
043100160706           //?Decodifico i dati variati
043200160706             xx = %lookup(CPVtvp:TVP);
043300160706             IF  xx > 0;
043400160706               V1Sdt1 = TVPdes(xx);
043500160706             ELSE;
043600160706               V1Sdt1 = CPVtvp;
043700160706             ENDIF;
043800160706
043900160706             SELECT;
044000160706             //WHEN  CPVtvp = 'A';
044100160706               //exsr Carica_a;
044200160706             WHEN  CPVtvp = 'B';
044300160706               exsr Carica_b;
044400160706             WHEN  CPVtvp = 'C';
044500160706               exsr Carica_c;
044600160706             WHEN  CPVtvp = 'D';
044700160706               exsr Carica_d;
044800160706             WHEN  CPVtvp = 'E';
044900160706               exsr Carica_e;
045000160706             ENDSL;
045100160706           ENDIF;
045200160706
045300160706           IF  V1Csel = *blank;
045400160706             reade (IMK31cpo:CPVdav:CPVorv:CPVnoj) TNCPVD1L;
045500160706           ELSE;
045600160706             reade (IMK31cpo:CPVdav:CPVorv:CPVnoj:V1Csel) TNCPVD1L;
045700160706           ENDIF;
045800160706         ENDDO;
045900160706
046000160706       ENDSR;
046100160706
046200160706       //--------------------------------------------------------------
046300160706       //?Emissione Subfile Sintetico.
046400160706       //--------------------------------------------------------------
046500160706       BEGSR EmettiSinte;
046600160706
046700160706         *inKI = *off;
046800160706         Sintetico = *on;
046900160706
047000160706       //?F9 - Cambia da Sintetito ad Analitico
047100160706         write MK31T01;
047200160706         write MK31Z01;
047300160706         DOW  not *inKI and not *inKL and V1csel = savSelezione;
047400160706           nrr1 = 1;
047500160706           rec1 = 1;
047600160706           exfmt MK31C01;
047700160706           *in28 = *off;
047800160706           IF  not *inKL;
047900160706             exsr ControllaSel;
048000160706             IF  not *in28;
048100160706               exsr LeggiSfl;
048200160706             ENDIF;
048300160706           ENDIF;
048400160706           IF  *inKI;
048500160706             Sintetico = *off;
048600160706           ENDIF;
048700160706         ENDDO;
048800160706
048900160706       ENDSR;
049000160706
049100160706       //--------------------------------------------------------------
049200160706       //?Emissione Subfile Analitico.
049300160706       //--------------------------------------------------------------
049400160706       BEGSR EmettiAnali;
049500160706
049600160706         *inKI = *off;
049700160706         Sintetico = *off;
049800160706
049900160706       //?F9 - Cambia da Analitco a Sintetico
050000160706         write MK31T01;
050100160706         write MK31Z03;
050200160706         DOW  not *inKI and not *inKL and V1csel = savSelezione;
050300160706           IF  not *in16;
050400160706             nrr3 = 1;
050500160706             rec3 = 1;
050600160706           ENDIF;
050700160706           exfmt MK31C03;
050800160706           *in28 = *off;
050900160706           IF  not *inKL;
051000160706             exsr ControllaSel;
051100160706             IF  not *in28;
051200160706               exsr LeggiSfl;
051300160706             ENDIF;
051400160706           ENDIF;
051500160706           IF  *inKI;
051600160706             Sintetico = *on;
051700160706           ENDIF;
051800160706         ENDDO;
051900160706
052000160706       ENDSR;
052100160706
052200160706       //--------------------------------------------------------------
052300160706       //?Scrivo Dati non Registrati.
052400160706       //--------------------------------------------------------------
052500160706       BEGSR DatiNoReg;
052600160706
052700160706         clear V1Sdt1;
052800160706         clear V1Sva2;
052900160706         V1Sva1 = DesPrima(06);
053000160706
053100160706         xx = 1;
053200160706         FOR  xx by 1 to %elem(DatiVariati);
053300160706           IF  DatiVariati(xx) <> *blanks and DatiVarEsteso(xx) <> 'S';
053400160706
053500160706         //?Memorizzo se in questo rcd c'è il tipo dati selezionato
053600160706             IF  (V1Csel <> *blanks and DatiVariati(XX) = V1Csel) or
053700160706                  V1Csel = *blanks;
053800160706               yy =  %lookup(DatiVariati(xx):TVP);
053900160706               IF  yy > 0;
054000160706                 desbreve = TVPdes(yy);
054100160706               ELSE;
054200160706                 desbreve = *all'?';
054300160706               ENDIF;
054400160707               V1Sva2 = %trim(V1Sva2) + ' ' + desbreve;
054500160706             ENDIF;
054600160706
054700160706           ENDIF;
054800160706         ENDFOR;
054900160706
055000160706         IF  V1Sva2 <> *blanks;
055100160706           V1Sva2 = %trim(V1Sva2);
055200160706           exsr RieS03;
055300160706         ENDIF;
055400160706
055500160706       ENDSR;
055600160706
055700160706       //--------------------------------------------------------------
055800160706       //?Scrivo la riga di Inserimento.
055900160706       //--------------------------------------------------------------
056000160706       BEGSR ScriviRigaIns;
056100160706
056200160706         clear V1Sdt1;
056300160706         clear V1Sva1;
056400160706         V1Sva2 = DesPrima(05);
056500160706         *in51 = *on;
056600160706         exsr RieS03;
056700160706         *in51 = *off;
056800160706
056900160706       ENDSR;
057000160706
057100160706       //--------------------------------------------------------------
057200160706       //?Scrivo la riga Analitica.
057300160706       //--------------------------------------------------------------
057400160706       BEGSR RieS03;
057500160706
057600160706         nrr3 += 1;
057700160706         in50 = *in50;
057800160706         in51 = *in51;
057900160706         write MK31S03;
058000160706
058100160706       //?Imposto se è il primo rcd analitico
058200160706         IF  V1Sreca = 0;
058300160706           V1Sreca = nrr3;
058400160706         ENDIF;
058500160707         *in50 = *off;
058600160706
058700160706       //?Memorizzo il rcd dati variati scritto in analitico
058800160706         xx = %lookup(CPVtvp:DatiVariati);
058900160706         IF  xx > 0;
059000160706           DatiVarEsteso(xx) = 'S';
059100160706         ENDIF;
059200160706
059300160706       ENDSR;
059400160706
059500160706       //--------------------------------------------------------------
059600160706       //?Carico dati RCD B.
059700160706       //--------------------------------------------------------------
059800160706       BEGSR Carica_b;
059900160706
060000160706       //?Riga prima
060100160706         dCPV_b = CPVprima;
060200160706         V1Sva1 = DesPrima(01);
060300160706         %subst(V1Sva1:8:16) = CPOpiv;
060400160706         %subst(V1Sva1:32:16) = CPOcdf;
060500160706
060600160706       //?Riga dopo
060700160706         dCPV_b = CPVdopo;
060800160706         V1Sva2 = DesDopo(01);
060900160706         %subst(V1Sva2:8:16) = CPOpiv;
061000160706         %subst(V1Sva2:32:16) = CPOcdf;
061100160706
061200160706         exsr RieS03;
061300160706
061400160706       ENDSR;
061500160706
061600160706       //--------------------------------------------------------------
061700160706       //?Carico dati RCD C.
061800160706       //--------------------------------------------------------------
061900160706       BEGSR Carica_c;
062000160706
062100160706       //?Riga prima
062200160706         dCPV_c = CPVprima;
062300160706         V1Sva1 = DesPrima(02);
062400160706         %subst(V1Sva1:12:5) = %editc(CPOsct:'X');
062500160706         %subst(V1Sva1:33:1) = CPOfsf;
062600160706
062700160706       //?Riga dopo
062800160706         dCPV_c = CPVdopo;
062900160706         V1Sva2 = DesDopo(02);
063000160706         %subst(V1Sva2:12:5) = %editc(CPOsct:'X');
063100160706         %subst(V1Sva2:33:1) = CPOfsf;
063200160706
063300160706         exsr RieS03;
063400160706
063500160706       ENDSR;
063600160706
063700160706       //--------------------------------------------------------------
063800160706       //?Carico dati RCD D.
063900160706       //--------------------------------------------------------------
064000160706       BEGSR Carica_d;
064100160706
064200160706       //?Riga prima
064300160706         dCPV_d = CPVprima;
064400160706         V1Sva1 = DesPrima(03);
064500160706         %subst(V1Sva1:11:14) = %editc(CPIpft:'2');
064600160831         IF  CPIpft = 0 and §CPOsptp > 0;
064700160831           %subst(V1Sva1:11:14) = %editc(§CPOsptp:'2');
064800160831           %subst(V1Sva1:27:8) = 'PRESUNTO';
064900160831         ENDIF;
065000160831         %subst(V1Sva1:49:1) = CPOftr;
065100160706
065200160706       //?Riga dopo
065300160706         dCPV_d = CPVdopo;
065400160706         V1Sva2 = DesDopo(03);
065500160706         %subst(V1Sva2:11:14) = %editc(CPIpft:'2');
065600160831         IF  CPIpft = 0 and §CPOsptp > 0;
065700160831           %subst(V1Sva2:11:14) = %editc(§CPOsptp:'2');
065800160831           %subst(V1Sva2:27:8) = 'PRESUNTO';
065900160831         ENDIF;
066000160831         %subst(V1Sva2:49:1) = CPOftr;
066100160706
066200160706         exsr RieS03;
066300160706
066400160706       ENDSR;
066500160706
066600160706       //--------------------------------------------------------------
066700160706       //?Carico dati RCD E.
066800160706       //--------------------------------------------------------------
066900160706       BEGSR Carica_e;
067000160706
067100160706       //?Riga prima
067200160706         dCPV_e = CPVprima;
067300160706         V1Sva1 = DesPrima(04);
067400160706         %subst(V1Sva1:14:7) = %editc(CPOcmm:'X');
067500160711         %subst(V1Sva1:37:3) = %editc(CPOflt:'X');
067600160706
067700160706       //?Riga dopo
067800160706         dCPV_e = CPVdopo;
067900160706         V1Sva2 = DesDopo(04);
068000160706         %subst(V1Sva2:14:7) = %editc(CPOcmm:'X');
068100160711         %subst(V1Sva2:37:3) = %editc(CPOflt:'X');
068200160706
068300160706         exsr RieS03;
068400160706
068500160706       ENDSR;
068600160706
068700160706       //--------------------------------------------------------------
068800160706       //?Controllo Selezioni.
068900160706       //--------------------------------------------------------------
069000160706       BEGSR ControllaSel;
069100160706
069200160706         read MK31T01;
069300160706         clear V1Dsel;
069400160706
069500160706         IF  V1Csel = '?';
069600160706           clear TIBS02DS;
069700160706           T02mod = 'R';
069800160706           T02cod = 'TVP';
069900160706           T02sif = knsif;
070000160706           TNTBE_RicercaControllo (kpjba:tibs02ds);
070100160706           V1Csel = T02ke1;
070200160706         ENDIF;
070300160706
070400160706         IF  V1Csel <> *blanks;
070500160706           clear TIBS02DS;
070600160706           T02mod = 'C';
070700160706           T02cod = 'TVP';
070800160706           T02ke1 = V1Csel;
070900160706           T02sif = knsif;
071000160706           TNTBE_RicercaControllo (kpjba:tibs02ds);
071100160706           IF  T02err <> *blanks;
071200160706             V1Cmsg = Msg(01);
071300160706             *in28 = *on;
071400160706             leavesr;
071500160706           ENDIF;
071600160706           dTVP = T02uni;
071700160706           V1Dsel = §TVPdes1;
071800160706         ENDIF;
071900160706
072000160706       ENDSR;
072100160706
072200160706       //--------------------------------------------------------------
072300160706       //?Opzioni immesse sul subfile.
072400160706       //--------------------------------------------------------------
072500160706       BEGSR LeggiSfl;
072600160706
072700160706         priSelezione = savSelezione;
072800160706         savSelezione = V1Csel;
072900160706
073000160706         readc MK31S01;
073100160706         DOW  not %eof;
073200160706           IF  V1Ssce = '5';
073300160706             rec3 = V1Sreca;
073400160706             nrr3 = V1Sreca;
073500160706             chain nrr3 MK31S03;
073600160706             IF  %found;
073700160706               *in16 = *on;
073800160706               *in50 = in50;
073900160706               *in51 = in51;
074000160706               update MK31S03;
074100160706             ENDIF;
074200160706             *inKL = *off;
074300160706             exsr EmettiAnali;
074400160706             chain nrr3 MK31S03;
074500160706             IF  %found;
074600160706               *in16 = *off;
074700160706               *in50 = in50;
074800160706               *in51 = in51;
074900160706               update MK31S03;
075000160706             ENDIF;
075100160706             clear V1Ssce;
075200160706           ENDIF;
075300160706           *in52 = in52;
075400160707           update MK31S01;
075500160706
075600160706           readc MK31S01;
075700160706
075800160706         ENDDO;
075900160706
076000160706         *inKL = *off;
076100160706         savSelezione = priSelezione;
076200160706         Sintetico = *on;
076300160706
076400160706       ENDSR;
076500160705
076600160705       //--------------------------------------------------------------
076700160705       //?Operazioni finali.
076800160705       //--------------------------------------------------------------
076900160705       BEGSR RoutEnd;
077000160705
077100160705         *inLR = *on;
077200160705
077300160705         return;
077400160705
077500160705       ENDSR;
077600160705
077700160705      /end-free
077800160706
077900160706       //--------------------------------------------------------------
078000160706       //?Schiere a tempo di compilazione.
078100160706       //--------------------------------------------------------------
078200160706
078300160706** -- DESPRIMA-----------------------------------*
078400160706P.IVA "1234567890123456"  C.F."1234567890123456"  01
078500160706Cat.Merc. "12345"  Sede/Filile "1"                02
078600160831Sp.Trasp."12345678901234"          Imp.Cliente "1"03
078700160706Commerciale "1234567" Fil.Gestione "123"          04
078800160706I   N   S   E   R   I   M   E   N   T   O         05
078900160706Dati variati ma non registrati :                  06
079000160706** -- DESDOPO------------------------------------*
079100160706      "1234567890123456"      "1234567890123456"  01
079200160706          "12345"              "1"                02
079300160831         "12345678901234"                      "1"03
079400160706            "1234567"              "123"          04
079500060523**
079600060530Selezione  Dati Variati ERRATA
