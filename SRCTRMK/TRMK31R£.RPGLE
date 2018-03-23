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
013900160706       exsr RieS01;
014000160705
014100160705       //?Emissione Subfile Variazioni
014200160706       IF  nrr1 > 0;
014300160707         *in20 = *on;
014400160707         DOW  not *inKL and V1Csel = savSelezione;
014500160706
014600160706         //?Sintetico
014700160706           IF  Sintetico;
014800160706             exsr EmettiSinte;
014900160706           ENDIF;
015000160706
015100160706         //?Analitico
015200160706           IF  not Sintetico;
015300160706             exsr EmettiAnali;
015400160706           ENDIF;
015500160705
015600160706         ENDDO;
015700160706       ENDIF;
015800160706
015900160706       //?No dati
016000160706       IF  nrr1 = 0;
016100160706         write MK31T01;
016200160706         write MK31Z01;
016300160706         write MK31D01;
016400160706         DOW  not *inKL and V1Csel = savSelezione;
016500160706           exfmt MK31C01;
016600160706           *IN28 = *off;
016700160706           IF  not *inKL;
016800160706             exsr ControllaSel;
016900160706           ENDIF;
017000160706         ENDDO;
017100160706       ENDIF;
017200160707       ENDDO;
017300160705
017400160705       //?Operazioni finali
017500160705       exsr RoutEnd;
017600160705
017700160705       //--------------------------------------------------------------
017800160705       //?Operazioni iniziali.
017900160705       //--------------------------------------------------------------
018000160705       BEGSR RoutInz;
018100160705
018200160705         TRMK31DS = kpjbu;
018300160705
018400160705       //?Reperimento dati job
018500160705         exsr DatiJob;
018600160706
018700160706       //?Carico tabella Tipi Variazioni
018800160706         clear xx;
018900160706         TBEcod = 'TVP';
019000160706         setll (TBEcod) TNTBE01L;
019100160706         reade (TBEcod) TNTBE01L;
019200160706         DOW  not %eof(TNTBE01L);
019300160706           IF  TBEatb = *blanks;
019400160706              xx += 1;
019500160706             dTVP = TBEuni;
019600160706             TVP(xx) = TBEke1;
019700160706             TVPdes(xx) = §TVPdes1;
019800160706           ENDIF;
019900160706           reade (TBEcod) TNTBE01L;
020000160706         ENDDO;
020100160705
020200160705       ENDSR;
020300160705
020400160705       //--------------------------------------------------------------
020500160705       //?Reperimento Dati del job (Utente/Operativi).
020600160705       //--------------------------------------------------------------
020700160705       BEGSR DatiJob;
020800160705
020900160705         in(E) §AzUte;
021000160705         IF  NOT %error;
021100160705           in(E) §DatiUte;
021200160705         ENDIF;
021300160705         IF  %error or RSut = *blanks;
021400160705           clear TIBS34ds;
021500160705           tibs34r(tibs34ds);
021600160705           in §AzUte;
021700160705           in §DatiUte;
021800160705         ENDIF;
021900160705
022000160705       ENDSR;
022100160705
022200160705       //--------------------------------------------------------------
022300160705       //?Dati di Testata.
022400160705       //--------------------------------------------------------------
022500160705       BEGSR Testata;
022600160705
022700160705       //?Imposto i dati di testata
022800160705         clear V1Crag;
022900160705         V1Ccpo = IMK31cpo;
023000160705         IF  IMK31rag = *blanks;
023100160705           chain (V1Ccpo) TNCPO01L;
023200160705           IF  %found(TNCPO01L);
023300160705             V1Crag = CPOrag;
023400160705           ENDIF;
023500160705         ELSE;
023600160705           V1Crag = IMK31rag;
023700160705         ENDIF;
023800160705
023900160705       ENDSR;
024000160705
024100160705       //--------------------------------------------------------------
024200160705       //?Imposto dati Subfile S01.
024300160705       //--------------------------------------------------------------
024400160705       BEGSR RieS01;
024500160705
024600160705         *in20 = *off;
024700160705         *in21 = *off;
024800160705         *in23 = *off;
024900160705
025000160705       //?Emissione Testata e Piede con tasti funzionali abilitati
025100160705         write  MK31C01;
025200160705         write  MK31C03;
025300160705
025400160705         *in21 = *on;
025500160705         *in23 = *on;
025600160705
025700160705         clear nrr1;
025800160705         clear nrr3;
025900160705
026000160705         setll (IMK31cpo) TNCPVT1L;
026100160705         reade (IMK31cpo) TNCPVT1L;
026200160705         DOW  not %eof(TNCPVT1L);
026300160705
026400160705           IF  V1Csel = *blanks;
026500160706             OkRecord = *on;
026600160705           ELSE;
026700160706             OkRecord = *off;
026800160705           ENDIF;
026900160706
027000160706         //?Data variazione
027100160706           clear wlbdat;
027200160706           G08inv = CPVdav;
027300160706           G08err = '3';
027400160706           XSRDA8(WLBdat);
027500160706           V1Sdav = G08ggmm * 100;
027600160706           V1Sdav += G08aa;
027700160711           V3Sdav = V1Sdav;
027800160706
027900160706         //?Decodifico pgm da tabella PVP
028000160706           clear TIBS02DS;
028100160706           T02mod = 'C';
028200160706           T02sif = knsif;
028300160706           T02cod = 'PVP';
028400160706           T02ke1 = CPVpgm;
028500160706           TNTBE_RicercaControllo (kpjba:tibs02ds);
028600160706           IF  T02err = *blanks;
028700160706             V1Spgm = T02uni;
028800160706           ELSE;
028900160706             V1Spgm = *all'?';
029000160706           ENDIF;
029100160706
029200160706         //?Scrivo la riga Inserimento
029300160706           clear V1Sins;
029400160706           IF  CPVcta <> 'M';
029500160706             V1Sins = Desprima(05);
029600160706           ENDIF;
029700160706
029800160706           V1Sorv = CPVorv;
029900160706           V3Sorv = CPVorv;
030000160706           V1Spru = CPVpru;
030100160706           V3Spru = CPVpru;
030200160706           V1Snoj = CPVnoj;
030300160706           V3Snoj = CPVnoJ;
030400160706
030500160706           clear V1Sana;
030600160706           clear V1Spiv;
030700160706           clear V1Smer;
030800160706           clear V1Sfat;
030900160706           clear V1Scom;
031000160901           clear V1Sfu;
031100160901           clear V1Sun;
031200160706
031300160706           clear DatiVarEsteso;
031400160902           clear DatiVariati;
031500160706           xx = 1;
031600160706           FOR  xx by 1 to %elem(DatiVariati);
031700160706             IF  %subst(CPVflo:(xx):1) <> *blanks;
031800160706               DatiVariati(xx) = %subst(CPVflo:(xx):1);
031900160706             ENDIF;
032000160706           ENDFOR;
032100160706
032200160706           xx = 1;
032300160706           FOR  xx by 1 to %elem(DatiVariati);
032400160706             IF  DatiVariati(xx) <> *blanks;
032500160706               IF  DatiVariati(xx) = 'A';
032600160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
032700160706                   OkRecord = *on;
032800160706                 ENDIF;
032900160706                 V1Sana = 'SI';
033000160706               ENDIF;
033100160706               IF  DatiVariati(xx) = 'B';
033200160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
033300160706                   OkRecord = *on;
033400160706                 ENDIF;
033500160706                 V1Spiv = 'SI';
033600160706               ENDIF;
033700160706               IF  DatiVariati(xx) = 'C';
033800160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
033900160706                   OkRecord = *on;
034000160706                 ENDIF;
034100160706                 V1Smer = 'SI';
034200160706               ENDIF;
034300160706               IF  DatiVariati(xx) = 'D';
034400160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
034500160706                   OkRecord = *on;
034600160706                 ENDIF;
034700160706                 V1Sfat = 'SI';
034800160706               ENDIF;
034900160706               IF  DatiVariati(xx) = 'E';
035000160706                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
035100160706                   OkRecord = *on;
035200160706                 ENDIF;
035300160706                 V1Scom = 'SI';
035400160706               ENDIF;
035500160901               IF  DatiVariati(xx) = 'F';
035600160901                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
035700160901                   OkRecord = *on;
035800160901                 ENDIF;
035900160901                 V1Sfu  = 'SI';
036000160901               ENDIF;
036100160901               IF  DatiVariati(xx) = 'G';
036200160901                 IF  V1Csel <> *blanks and V1Csel = DatiVariati(xx);
036300160901                   OkRecord = *on;
036400160901                 ENDIF;
036500160901                 V1Sun  = 'SI';
036600160901               ENDIF;
036700160706             ENDIF;
036800160706           ENDFOR;
036900160706
037000160706         //?Scrivo dati per visualizzazione Analitica
037100160706           IF  OkRecord;
037200160706             exsr ScriviAnalitico;
037300160706           //?Scrivo un rcd per dati variati ma non registrati
037400160706             IF  CPVcta = 'M';
037500160706               exsr DatiNoReg;
037600160706             ENDIF;
037700160706           //?Almeno una riga è stata scritta, aggancio l'ultima per sottolinearla
037800160706             chain nrr3 MK31S03;
037900160706             IF  %found;
038000160706               *inKL = *off;
038100160706               *in50 = in50;
038200160706               *in51 = in51;
038300160706               IF  not *in50;
038400160706                 clear V3Spru;
038500160706                 clear V3Snoj;
038600160706                 clear V3Sorv;
038700160706                 clear V3Sdav;
038800160706               ENDIF;
038900160706               *in50 = *on;
039000160706               *in51 = *on;
039100160706               in50 = *in50;
039200160706               in51 = *in51;
039300160706               update MK31S03;
039400160706             ENDIF;
039500160706
039600160706           //?Se non è stata scritta nessuna riga proteggo il campo di scelta
039700160706             IF  V1Sreca = 0;
039800160706               *in52 = *on;
039900160706             ENDIF;
040000160706             in52 = *in52;
040100160706             nrr1 += 1;
040200160706             write MK31S01;
040300160706             *in52 = *off;
040400160706           ENDIF;
040500160706
040600160706           reade (IMK31cpo) TNCPVT1L;
040700160706
040800160705         ENDDO;
040900160706
041000160706         savSelezione = V1Csel;
041100160705
041200160705       ENDSR;
041300160706
041400160706       //--------------------------------------------------------------
041500160706       //?Scrivo subfile Analitico.
041600160706       //--------------------------------------------------------------
041700160706       BEGSR ScriviAnalitico;
041800160706
041900160706         *in16 = *off;
042000160706         *in50 = *on;
042100160706         *in51 = *off;
042200160706         clear V1Sreca;
042300160706
042400160706       //?Se non è Manutenzione non carico la DS
042500160706         IF  CPVcta <> 'M';
042600160706           exsr ScriviRigaIns;
042700160706           leavesr;
042800160706         ENDIF;
042900160706
043000160706         IF  V1Csel = *blanks;
043100160706           setll (IMK31cpo:CPVdav:CPVorv:CPVnoj) TNCPVD1L;
043200160706           reade (IMK31cpo:CPVdav:CPVorv:CPVnoj) TNCPVD1L;
043300160706         ELSE;
043400160706           setll (IMK31cpo:CPVdav:CPVorv:CPVnoj:V1Csel) TNCPVD1L;
043500160706           reade (IMK31cpo:CPVdav:CPVorv:CPVnoj:V1Csel) TNCPVD1L;
043600160706         ENDIF;
043700160706
043800160706         DOW  not %eof(TNCPVD1L);
043900160706
044000160706         //?Se nessun dato variato rcd generico
044100160706           IF  CPVprima = CPVdopo;
044200160706             xx = %lookup(CPVtvp:DatiVariati);
044300160706             IF  xx > 0;
044400160706               DatiVarEsteso(xx) = 'N';
044500160706             ENDIF;
044600160706           ELSE;
044700160706
044800160706           //?Decodifico i dati variati
044900160706             xx = %lookup(CPVtvp:TVP);
045000160706             IF  xx > 0;
045100160706               V1Sdt1 = TVPdes(xx);
045200160706             ELSE;
045300160706               V1Sdt1 = CPVtvp;
045400160706             ENDIF;
045500160706
045600160706             SELECT;
045700160706             //WHEN  CPVtvp = 'A';
045800160706               //exsr Carica_a;
045900160706             WHEN  CPVtvp = 'B';
046000160706               exsr Carica_b;
046100160706             WHEN  CPVtvp = 'C';
046200160706               exsr Carica_c;
046300160706             WHEN  CPVtvp = 'D';
046400160706               exsr Carica_d;
046500160706             WHEN  CPVtvp = 'E';
046600160706               exsr Carica_e;
046700160901             WHEN  CPVtvp = 'F';
046800160901               exsr Carica_f;
046900160901             WHEN  CPVtvp = 'G';
047000160901               exsr Carica_g;
047100160706             ENDSL;
047200160706           ENDIF;
047300160706
047400160706           IF  V1Csel = *blank;
047500160706             reade (IMK31cpo:CPVdav:CPVorv:CPVnoj) TNCPVD1L;
047600160706           ELSE;
047700160706             reade (IMK31cpo:CPVdav:CPVorv:CPVnoj:V1Csel) TNCPVD1L;
047800160706           ENDIF;
047900160706         ENDDO;
048000160706
048100160706       ENDSR;
048200160706
048300160706       //--------------------------------------------------------------
048400160706       //?Emissione Subfile Sintetico.
048500160706       //--------------------------------------------------------------
048600160706       BEGSR EmettiSinte;
048700160706
048800160706         *inKI = *off;
048900160706         Sintetico = *on;
049000160706
049100160706       //?F9 - Cambia da Sintetito ad Analitico
049200160706         write MK31T01;
049300160706         write MK31Z01;
049400160706         DOW  not *inKI and not *inKL and V1csel = savSelezione;
049500160706           nrr1 = 1;
049600160706           rec1 = 1;
049700160706           exfmt MK31C01;
049800160706           *in28 = *off;
049900160706           IF  not *inKL;
050000160706             exsr ControllaSel;
050100160706             IF  not *in28;
050200160706               exsr LeggiSfl;
050300160706             ENDIF;
050400160706           ENDIF;
050500160706           IF  *inKI;
050600160706             Sintetico = *off;
050700160706           ENDIF;
050800160706         ENDDO;
050900160706
051000160706       ENDSR;
051100160706
051200160706       //--------------------------------------------------------------
051300160706       //?Emissione Subfile Analitico.
051400160706       //--------------------------------------------------------------
051500160706       BEGSR EmettiAnali;
051600160706
051700160706         *inKI = *off;
051800160706         Sintetico = *off;
051900160706
052000160706       //?F9 - Cambia da Analitco a Sintetico
052100160706         write MK31T01;
052200160706         write MK31Z03;
052300160706         DOW  not *inKI and not *inKL and V1csel = savSelezione;
052400160706           IF  not *in16;
052500160706             nrr3 = 1;
052600160706             rec3 = 1;
052700160706           ENDIF;
052800160706           exfmt MK31C03;
052900160706           *in28 = *off;
053000160706           IF  not *inKL;
053100160706             exsr ControllaSel;
053200160706             IF  not *in28;
053300160706               exsr LeggiSfl;
053400160706             ENDIF;
053500160706           ENDIF;
053600160706           IF  *inKI;
053700160706             Sintetico = *on;
053800160706           ENDIF;
053900160706         ENDDO;
054000160706
054100160706       ENDSR;
054200160706
054300160706       //--------------------------------------------------------------
054400160706       //?Scrivo Dati non Registrati.
054500160706       //--------------------------------------------------------------
054600160706       BEGSR DatiNoReg;
054700160706
054800160706         clear V1Sdt1;
054900160706         clear V1Sva2;
055000160706         V1Sva1 = DesPrima(06);
055100160706
055200160706         xx = 1;
055300160706         FOR  xx by 1 to %elem(DatiVariati);
055400160706           IF  DatiVariati(xx) <> *blanks and DatiVarEsteso(xx) <> 'S';
055500160706
055600160706         //?Memorizzo se in questo rcd c'è il tipo dati selezionato
055700160706             IF  (V1Csel <> *blanks and DatiVariati(XX) = V1Csel) or
055800160706                  V1Csel = *blanks;
055900160706               yy =  %lookup(DatiVariati(xx):TVP);
056000160706               IF  yy > 0;
056100160706                 desbreve = TVPdes(yy);
056200160706               ELSE;
056300160706                 desbreve = *all'?';
056400160706               ENDIF;
056500160707               V1Sva2 = %trim(V1Sva2) + ' ' + desbreve;
056600160706             ENDIF;
056700160706
056800160706           ENDIF;
056900160706         ENDFOR;
057000160706
057100160706         IF  V1Sva2 <> *blanks;
057200160706           V1Sva2 = %trim(V1Sva2);
057300160706           exsr RieS03;
057400160706         ENDIF;
057500160706
057600160706       ENDSR;
057700160706
057800160706       //--------------------------------------------------------------
057900160706       //?Scrivo la riga di Inserimento.
058000160706       //--------------------------------------------------------------
058100160706       BEGSR ScriviRigaIns;
058200160706
058300160706         clear V1Sdt1;
058400160706         clear V1Sva1;
058500160706         V1Sva2 = DesPrima(05);
058600160706         *in51 = *on;
058700160706         exsr RieS03;
058800160706         *in51 = *off;
058900160706
059000160706       ENDSR;
059100160706
059200160706       //--------------------------------------------------------------
059300160706       //?Scrivo la riga Analitica.
059400160706       //--------------------------------------------------------------
059500160706       BEGSR RieS03;
059600160706
059700160706         nrr3 += 1;
059800160706         in50 = *in50;
059900160706         in51 = *in51;
060000160706         write MK31S03;
060100160706
060200160706       //?Imposto se è il primo rcd analitico
060300160706         IF  V1Sreca = 0;
060400160706           V1Sreca = nrr3;
060500160706         ENDIF;
060600160707         *in50 = *off;
060700160706
060800160706       //?Memorizzo il rcd dati variati scritto in analitico
060900160706         xx = %lookup(CPVtvp:DatiVariati);
061000160706         IF  xx > 0;
061100160706           DatiVarEsteso(xx) = 'S';
061200160706         ENDIF;
061300160706
061400160706       ENDSR;
061500160706
061600160706       //--------------------------------------------------------------
061700160706       //?Carico dati RCD B.
061800160706       //--------------------------------------------------------------
061900160706       BEGSR Carica_b;
062000160706
062100160706       //?Riga prima
062200160706         dCPV_b = CPVprima;
062300160706         V1Sva1 = DesPrima(01);
062400160706         %subst(V1Sva1:8:16) = CPOpiv;
062500160706         %subst(V1Sva1:32:16) = CPOcdf;
062600160706
062700160706       //?Riga dopo
062800160706         dCPV_b = CPVdopo;
062900160706         V1Sva2 = DesDopo(01);
063000160706         %subst(V1Sva2:8:16) = CPOpiv;
063100160706         %subst(V1Sva2:32:16) = CPOcdf;
063200160706
063300160706         exsr RieS03;
063400160706
063500160706       ENDSR;
063600160706
063700160706       //--------------------------------------------------------------
063800160706       //?Carico dati RCD C.
063900160706       //--------------------------------------------------------------
064000160706       BEGSR Carica_c;
064100160706
064200160706       //?Riga prima
064300160706         dCPV_c = CPVprima;
064400160706         V1Sva1 = DesPrima(02);
064500160706         %subst(V1Sva1:12:5) = %editc(CPOsct:'X');
064600160706         %subst(V1Sva1:33:1) = CPOfsf;
064700160706
064800160706       //?Riga dopo
064900160706         dCPV_c = CPVdopo;
065000160706         V1Sva2 = DesDopo(02);
065100160706         %subst(V1Sva2:12:5) = %editc(CPOsct:'X');
065200160706         %subst(V1Sva2:33:1) = CPOfsf;
065300160706
065400160706         exsr RieS03;
065500160706
065600160706       ENDSR;
065700160706
065800160706       //--------------------------------------------------------------
065900160706       //?Carico dati RCD D.
066000160706       //--------------------------------------------------------------
066100160706       BEGSR Carica_d;
066200160706
066300160706       //?Riga prima
066400160706         dCPV_d = CPVprima;
066500160706         V1Sva1 = DesPrima(03);
066600160706         %subst(V1Sva1:11:14) = %editc(CPIpft:'2');
066700160831         IF  CPIpft = 0 and §CPOsptp > 0;
066800160831           %subst(V1Sva1:11:14) = %editc(§CPOsptp:'2');
066900160831           %subst(V1Sva1:27:8) = 'PRESUNTO';
067000160831         ENDIF;
067100160831         %subst(V1Sva1:49:1) = CPOftr;
067200160706
067300160706       //?Riga dopo
067400160706         dCPV_d = CPVdopo;
067500160706         V1Sva2 = DesDopo(03);
067600160706         %subst(V1Sva2:11:14) = %editc(CPIpft:'2');
067700160831         IF  CPIpft = 0 and §CPOsptp > 0;
067800160831           %subst(V1Sva2:11:14) = %editc(§CPOsptp:'2');
067900160831           %subst(V1Sva2:27:8) = 'PRESUNTO';
068000160831         ENDIF;
068100160831         %subst(V1Sva2:49:1) = CPOftr;
068200160706
068300160706         exsr RieS03;
068400160706
068500160706       ENDSR;
068600160706
068700160706       //--------------------------------------------------------------
068800160706       //?Carico dati RCD E.
068900160706       //--------------------------------------------------------------
069000160706       BEGSR Carica_e;
069100160706
069200160706       //?Riga prima
069300160706         dCPV_e = CPVprima;
069400160706         V1Sva1 = DesPrima(04);
069500160706         %subst(V1Sva1:14:7) = %editc(CPOcmm:'X');
069600160711         %subst(V1Sva1:37:3) = %editc(CPOflt:'X');
069700160706
069800160706       //?Riga dopo
069900160706         dCPV_e = CPVdopo;
070000160706         V1Sva2 = DesDopo(04);
070100160706         %subst(V1Sva2:14:7) = %editc(CPOcmm:'X');
070200160711         %subst(V1Sva2:37:3) = %editc(CPOflt:'X');
070300160706
070400160706         exsr RieS03;
070500160706
070600160706       ENDSR;
070700160901
070800160901       //--------------------------------------------------------------
070900160901       //?Carico dati RCD F.
071000160901       //--------------------------------------------------------------
071100160901       BEGSR Carica_f;
071200160901
071300160901       //?Riga prima
071400160901         dCPV_f = CPVprima;
071500160901
071600160901       //?Riga dopo
071700160901         dCPV_f = CPVdopo;
071800160901         V1Sva2 = DesDopo(05);
071900160901         %subst(V1Sva2:29:11) = %editc(f_CPOcpo:'X');
072000160901
072100160901         exsr RieS03;
072200160901
072300160901       ENDSR;
072400160901
072500160901       //--------------------------------------------------------------
072600160901       //?Carico dati RCD G.
072700160901       //--------------------------------------------------------------
072800160901       BEGSR Carica_g;
072900160901
073000160901       //?Riga prima
073100160901         dCPV_g = CPVprima;
073200160901
073300160901       //?Riga dopo
073400160901         dCPV_g = CPVdopo;
073500160901         V1Sva2 = DesDopo(06);
073600160901         %subst(V1Sva2:30:11) = %editc(g_CPOcpo:'X');
073700160901
073800160901         exsr RieS03;
073900160901
074000160901       ENDSR;
074100160706
074200160706       //--------------------------------------------------------------
074300160706       //?Controllo Selezioni.
074400160706       //--------------------------------------------------------------
074500160706       BEGSR ControllaSel;
074600160706
074700160706         read MK31T01;
074800160706         clear V1Dsel;
074900160706
075000160706         IF  V1Csel = '?';
075100160706           clear TIBS02DS;
075200160706           T02mod = 'R';
075300160706           T02cod = 'TVP';
075400160706           T02sif = knsif;
075500160706           TNTBE_RicercaControllo (kpjba:tibs02ds);
075600160706           V1Csel = T02ke1;
075700160706         ENDIF;
075800160706
075900160706         IF  V1Csel <> *blanks;
076000160706           clear TIBS02DS;
076100160706           T02mod = 'C';
076200160706           T02cod = 'TVP';
076300160706           T02ke1 = V1Csel;
076400160706           T02sif = knsif;
076500160706           TNTBE_RicercaControllo (kpjba:tibs02ds);
076600160706           IF  T02err <> *blanks;
076700160706             V1Cmsg = Msg(01);
076800160706             *in28 = *on;
076900160706             leavesr;
077000160706           ENDIF;
077100160706           dTVP = T02uni;
077200160706           V1Dsel = §TVPdes1;
077300160706         ENDIF;
077400160706
077500160706       ENDSR;
077600160706
077700160706       //--------------------------------------------------------------
077800160706       //?Opzioni immesse sul subfile.
077900160706       //--------------------------------------------------------------
078000160706       BEGSR LeggiSfl;
078100160706
078200160706         priSelezione = savSelezione;
078300160706         savSelezione = V1Csel;
078400160706
078500160706         readc MK31S01;
078600160706         DOW  not %eof;
078700160706           IF  V1Ssce = '5';
078800160706             rec3 = V1Sreca;
078900160706             nrr3 = V1Sreca;
079000160706             chain nrr3 MK31S03;
079100160706             IF  %found;
079200160706               *in16 = *on;
079300160706               *in50 = in50;
079400160706               *in51 = in51;
079500160706               update MK31S03;
079600160706             ENDIF;
079700160706             *inKL = *off;
079800160706             exsr EmettiAnali;
079900160706             chain nrr3 MK31S03;
080000160706             IF  %found;
080100160706               *in16 = *off;
080200160706               *in50 = in50;
080300160706               *in51 = in51;
080400160706               update MK31S03;
080500160706             ENDIF;
080600160706             clear V1Ssce;
080700160706           ENDIF;
080800160706           *in52 = in52;
080900160707           update MK31S01;
081000160706
081100160706           readc MK31S01;
081200160706
081300160706         ENDDO;
081400160706
081500160706         *inKL = *off;
081600160706         savSelezione = priSelezione;
081700160706         Sintetico = *on;
081800160706
081900160706       ENDSR;
082000160705
082100160705       //--------------------------------------------------------------
082200160705       //?Operazioni finali.
082300160705       //--------------------------------------------------------------
082400160705       BEGSR RoutEnd;
082500160705
082600160705         *inLR = *on;
082700160705
082800160705         return;
082900160705
083000160705       ENDSR;
083100160705
083200160705      /end-free
083300160706
083400160706       //--------------------------------------------------------------
083500160706       //?Schiere a tempo di compilazione.
083600160706       //--------------------------------------------------------------
083700160706
083800160706** -- DESPRIMA-----------------------------------*
083900160706P.IVA "1234567890123456"  C.F."1234567890123456"  01
084000160706Cat.Merc. "12345"  Sede/Filile "1"                02
084100160831Sp.Trasp."12345678901234"          Imp.Cliente "1"03
084200160706Commerciale "1234567" Fil.Gestione "123"          04
084300160706I   N   S   E   R   I   M   E   N   T   O         05
084400160706Dati variati ma non registrati :                  06
084500160706** -- DESDOPO------------------------------------*
084600160706      "1234567890123456"      "1234567890123456"  01
084700160706          "12345"              "1"                02
084800160831         "12345678901234"                      "1"03
084900160706            "1234567"              "123"          04
085000160901Codice Potenziale Fuso con:"12345678901"          05
085100160901Codice Potenziale Unito con:"12345678901"         06
085200060523**
085300060530Selezione  Dati Variati ERRATA
