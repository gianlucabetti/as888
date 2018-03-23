000100160704      //--------------------------------------------------------------
000200160704      //?TRMK30R - VARIAZIONI ANAGRAFICHE POTENZIALI
000300160704      //--------------------------------------------------------------
000400160704     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160704     h dftactgrp(*no) actgrp(*caller)
000600160704
000700160704      //---------------------------------------------------------------
000800160704      //?Dichiarazione file.
000900160704      //---------------------------------------------------------------
001000160704      // - File Variazioni
001100160704     fTNCPVD0F  o    e             disk
001200160704     fTNCPVT1L  uf a e           k disk
001300160704
001400160704      //---------------------------------------------------------------
001500160704      //?Definizione schiere.
001600160704      //---------------------------------------------------------------
001700160704      // - Messggi errore
001800160704     d Msg             s             78    dim(2) ctdata perrcd(1)
001900160704
002000160704     d TipoVar         s              1    dim(15)
002100160704
002200160704      //---------------------------------------------------------------
002300160704      //?Definizione aree dati.
002400160704      //---------------------------------------------------------------
002500160704      // - Dati utente
002600160704     d §AzUte        e ds                  extname(AZUTE00F)
002700160704     d                                     dtaara
002800160704     d §DatiUte      e ds                  extname(dDatiUte)
002900160704     d                                     dtaara
003000160704
003100160704      //---------------------------------------------------------------
003200160704      //?Definizione strutture dati.
003300160704      //---------------------------------------------------------------
003400160704      // - Parametri ricevuti
003500160704     d TRMK30DS      e ds
003600160704     d TNCPODS       e ds                  extname(TNCPO00F)
003700160705     d TNCPO1DS      e ds                  extname(TNCPO10F)
003800160705     d TICPIDS       e ds                  extname(TICPI00F)
003900160704
004000160704     d KPJBA         e ds
004100160705
004200160705      // - Reperimento dati utente
004300160705     d TIBS34DS      e ds
004400160831
004500160831      // - Ds campo CPORST
004600160831     d dCPO01        e ds
004700160704
004800160704      // - Ds Letture Variazioni
004900160705     d dcpv_a        e ds                  prefix(d)
005000160705     d wdcpv_a       e ds                  extname(dcpv_a)
005100160704     d                                     prefix(w)
005200160706     d dcpv_b        e ds                  prefix(d)
005300160706     d wdcpv_b       e ds                  extname(dcpv_b)
005400160705     d                                     prefix(w)
005500160706     d dcpv_c        e ds                  prefix(d)
005600160706     d wdcpv_c       e ds                  extname(dcpv_c)
005700160705     d                                     prefix(w)
005800160706     d dcpv_d        e ds                  prefix(d)
005900160706     d wdcpv_d       e ds                  extname(dcpv_d)
006000160705     d                                     prefix(w)
006100160706     d dcpv_e        e ds                  prefix(d)
006200160706     d wdcpv_e       e ds                  extname(dcpv_e)
006300160705     d                                     prefix(w)
006400160705
006500160705      // - Ds File Testata
006600160705     d wTNCPVT       e ds                  extname(TNCPVT0F)
006700160705     d                                     prefix(w)
006800160705     d TNCPVT        e ds                  extname(TNCPVT0F)
006900160704
007000160704      //---------------------------------------------------------------
007100160704      //?Definizione variabili globali.
007200160704      //---------------------------------------------------------------
007300160704      // - Indici di Schiera
007400160704     d xx              s              2s 0 inz
007500160704     d yy              s              2s 0 inz
007600160704     d zz              s              2s 0 inz
007700160704
007800160704      // - Campi di comodo
007900160704     d Conta           s              4s 0 inz
008000160705     d NewCPVflo       s                   like(CPVflo)
008100160705     d OldCPVflo       s                   like(CPVflo)
008200160704     d Potenziale      s                   like(CPVcpo)
008300160705     d PotenzialeOld   s                   like(CPVcpo)
008400160704
008500160704      //---------------------------------------------------------------
008600160704      //?Definizione Prototipi.
008700160704      //---------------------------------------------------------------
008800160704      /copy gaitrasrc/srcprotopr,TIBS34R
008900160704
009000160704      //---------------------------------------------------------------
009100160704      //?M A I N - L I N E
009200160704      //---------------------------------------------------------------
009300160704
009400160704     c     *Entry        plist
009500160704     c                   parm                    TRMK30DS
009600160704     c                   parm                    TNCPODS
009700160705     c                   parm                    TNCPO1DS
009800160705     c                   parm                    TICPIDS
009900160704
010000160704      /free
010100160704
010200160704       //?Operazioni iniziali
010300160704       exsr RoutInz;
010400160704
010500160704       //?Elabora
010600160704       IF  IMK30tla <> 'C';
010700160704         exsr Elabora;
010800160704       ENDIF;
010900160704
011000160704       //?Operazioni finali
011100160704       exsr RoutEnd;
011200160704
011300160704       //--------------------------------------------------------------
011400160704       //?Operazioni iniziali.
011500160704       //--------------------------------------------------------------
011600160704       BEGSR RoutInz;
011700160704
011800160704       //?Reperimento dati job
011900160704         exsr DatiJob;
012000160704
012100160704       ENDSR;
012200160704
012300160704       //--------------------------------------------------------------
012400160704       //?Reperimento Dati del job (Utente/Operativi).
012500160704       //--------------------------------------------------------------
012600160704       BEGSR DatiJob;
012700160704
012800160704         in(E) §AzUte;
012900160704         IF  NOT %error;
013000160704           in(E) §DatiUte;
013100160704         ENDIF;
013200160704         IF  %error or RSut = *blanks;
013300160704           clear TIBS34ds;
013400160704           tibs34r(tibs34ds);
013500160704           in §AzUte;
013600160704           in §DatiUte;
013700160704         ENDIF;
013800160704
013900160704       ENDSR;
014000160704
014100160704       //--------------------------------------------------------------
014200160704       //?Elabora.
014300160704       //--------------------------------------------------------------
014400160704       BEGSR Elabora;
014500160704
014600160704       //?Memorizzo l'immagine PRIMA
014700160704         IF  IMK30immag = 'P';
014800160704           clear conta;
014900160704           exsr ImpostaDS;
015000160704           exsr ImmaginePrima;
015100160704         ENDIF;
015200160704
015300160704       //?Memorizzo l'immagine DOPO
015400160704         IF  IMK30immag = 'D';
015500160704           exsr SalvaPotenziale;
015600160704
015700160704           SELECT;
015800160704         //?Inserimento: solo immagine DOPO
015900160704           WHEN  IMK30cta <> 'M';
016000160704             exsr ImpostaDS;
016100160704             exsr ImmagineDopo;
016200160704         //?Errore: se non c'è l'immagine PRIMA
016300160704           WHEN  conta <> 1;
016400160704             OMK30err = '1';
016500160704             OMK30msg = Msg(01);
016600160707         //?Errore: se potenziale diverso tra PRIMA e DOPO
016700160704           WHEN  Potenziale <> PotenzialeOld;
016800160704             OMK30err = '1';
016900160704             OMK30msg = Msg(02);
017000160704         //?Manutenzione
017100160704           OTHER;
017200160704             exsr ImpostaDS;
017300160704             exsr ImmagineDopo;
017400160704           ENDSL;
017500160704         ENDIF;
017600160704
017700160704       ENDSR;
017800160704
017900160704       //--------------------------------------------------------------
018000160704       //?Imposta i dati nelle DS.
018100160704       //--------------------------------------------------------------
018200160704       BEGSR ImpostaDS;
018300160704
018400160704         IF  IMK30tvp = 'A' or IMK30tvp = *blanks;
018500160705           dCPOrag = CPOrag;
018600160705           dCPOvia = CPOvia;
018700160705           dCPOcit = CPOcit;
018800160705           dCPOcap = CPOcap;
018900160705           dCPOprv = CPOprv;
019000160705           dCPOnaz = CPOnaz;
019100160705           dCPO1tel = CPO1tel;
019200160705           dCPO1fax = CPO1fax;
019300160704         ENDIF;
019400160706
019500160706         IF  IMK30tvp = 'B' or IMK30tvp = *blanks;
019600160706           dCPOpiv = CPOpiv;
019700160706           dCPOcdf = CPOcdf;
019800160706         ENDIF;
019900160704
020000160706         IF  IMK30tvp = 'C' or IMK30tvp = *blanks;
020100160705           dCPOsct = CPOsct;
020200160704           dCPOfsf = CPOfsf;
020300160704         ENDIF;
020400160704
020500160706         IF  IMK30tvp = 'D' or IMK30tvp = *blanks;
020600160831           dCPO01 = CPOrst;
020700160704           dCPIpft = CPIpft;
020800160704           dCPOftr = CPOftr;
020900160831           d§CPOsptp = §CPOsptp;
021000160704         ENDIF;
021100160704
021200160706         IF  IMK30tvp = 'E' or IMK30tvp = *blanks;
021300160704           dCPOcmm = CPOcmm;
021400160704           dCPOflt = CPOflt;
021500160704         ENDIF;
021600160704
021700160704       ENDSR;
021800160704
021900160704       //--------------------------------------------------------------
022000160704       //?Immagine PRIMA.
022100160704       //--------------------------------------------------------------
022200160704       BEGSR ImmaginePrima;
022300160704
022400160705         Conta = 1;
022500160704         exsr SalvaPotenziale;
022600160704         PotenzialeOld = Potenziale;
022700160704
022800160704         //?Imposto le DS per verificare se i dati sono variati
022900160704         IF  IMK30tvp = 'A' or IMK30tvp = *blanks;
023000160704           wdCPV_a = dCPV_a;
023100160704         ENDIF;
023200160704         IF  IMK30tvp = 'B' or IMK30tvp = *blanks;
023300160704           wdCPV_b = dCPV_b;
023400160704         ENDIF;
023500160704         IF  IMK30tvp = 'C' or IMK30tvp = *blanks;
023600160704           wdCPV_c = dCPV_c;
023700160704         ENDIF;
023800160704         IF  IMK30tvp = 'D' or IMK30tvp = *blanks;
023900160704           wdCPV_d = dCPV_d;
024000160704         ENDIF;
024100160706         IF  IMK30tvp = 'E' or IMK30tvp = *blanks;
024200160706           wdCPV_e = dCPV_e;
024300160706         ENDIF;
024400160704
024500160704       ENDSR;
024600160704
024700160704       //--------------------------------------------------------------
024800160704       //?Immagine DOPO.
024900160704       //--------------------------------------------------------------
025000160704       BEGSR ImmagineDopo;
025100160704
025200160704         clear TipoVar;
025300160704         clear xx;
025400160704         clear TNCPVT00;
025500160704         clear TNCPVD00;
025600160704
025700160704         exsr SalvaPotenziale;
025800160704
025900160704         CPVdav = %dec(%date());
026000160704         CPVorv = %dec(%time());
026100160704         CPVpru = IMK30pru;
026200160704         CPVnoj = IMK30noj;
026300160704         CPVpgm = IMK30pgm;
026400160704
026500160704         //?Inserimento
026600160704         IF  IMK30cta <> 'M';
026700160704           CPVcta = IMK30cta;
026800160712           CPVflo = 'ABCDE';
026900160704           write TNCPVT00;
027000160705           leavesr;
027100160704         ENDIF;
027200160704
027300160704         //?Manutenzione
027400160706         //?RCD A
027500160706         //?verifico se dati variati ma non scrivo dettaglio
027600160706         IF  IMK30tvp = 'A' or IMK30tvp = *blanks;
027700160706           IF  wdCPV_a <> dCPV_a;
027800160706             xx += 1;
027900160706             TipoVar(xx) = 'A';
028000160706           ENDIF;
028100160705         ENDIF;
028200160705         //?RCD B
028300160705         //?verifico se dati variati
028400160705         IF  IMK30tvp = 'B' or IMK30tvp = *blanks;
028500160705           IF  wdCPV_b <> dCPV_b;
028600160705             xx += 1;
028700160705             TipoVar(xx) = 'B';
028800160705             CPVtvp = 'B';
028900160705             CPVprima = wdCPV_b;
029000160705             CPVdopo  = dCPV_b;
029100160705             write TNCPVD00;
029200160705           ENDIF;
029300160705         ENDIF;
029400160705         //?RCD C
029500160705         //?verifico se dati variati
029600160705         IF  IMK30tvp = 'C' or IMK30tvp = *blanks;
029700160705           IF  wdCPV_c <> dCPV_c;
029800160705             xx += 1;
029900160705             TipoVar(xx) = 'C';
030000160705             CPVtvp = 'C';
030100160705             CPVprima = wdCPV_c;
030200160705             CPVdopo  = dCPV_c;
030300160705             write TNCPVD00;
030400160705           ENDIF;
030500160705         ENDIF;
030600160705         //?RCD D
030700160705         //?verifico se dati variati
030800160705         IF  IMK30tvp = 'D' or IMK30tvp = *blanks;
030900160705           IF  wdCPV_d <> dCPV_d;
031000160705             xx += 1;
031100160705             TipoVar(xx) = 'D';
031200160705             CPVtvp = 'D';
031300160705             CPVprima = wdCPV_d;
031400160705             CPVdopo  = dCPV_d;
031500160705             write TNCPVD00;
031600160705           ENDIF;
031700160705         ENDIF;
031800160706         //?RCD E
031900160706         //?verifico se dati variati
032000160706         IF  IMK30tvp = 'E' or IMK30tvp = *blanks;
032100160706           IF  wdCPV_e <> dCPV_e;
032200160706             xx += 1;
032300160706             TipoVar(xx) = 'E';
032400160706             CPVtvp = 'E';
032500160706             CPVprima = wdCPV_e;
032600160706             CPVdopo  = dCPV_e;
032700160706             write TNCPVD00;
032800160706           ENDIF;
032900160706         ENDIF;
033000160705
033100160705         //?Scrittura file di testata
033200160705         IF  TipoVar(1) = *blanks;
033300160705           leavesr;
033400160705         ENDIF;
033500160705
033600160705         CPVcta = 'M';
033700160705         yy = 1;
033800160705         FOR yy by 1 to %elem(TipoVar);
033900160705           %subst(CPVflo:(yy):1) = TipoVar(yy);
034000160705         ENDFOR;
034100160705         //?Salvo il rcd di testata e verifico se sono uguali a parte il campo FLO
034200160705         wTNCPVT = TNCPVT;
034300160705         NewCPVflo = CPVflo;
034400160705         clear wCPVflo;
034500160705         chain (CPVcpo:CPVdav:CPVorv) TNCPVT1L;
034600160705         IF  not %found(TNCPVT1L);
034700160705           write TNCPVT00;
034800160705           leavesr;
034900160705         ENDIF;
035000160705         //?Salvo FLO
035100160705         OldCPVflo = CPVflo;
035200160705         clear CPVflo;
035300160705         //?Se sono diversi scrivo il rcd
035400160705         IF  TNCPVT <> wTNCPVT;
035500160705           TNCPVT = wTNCPVT;
035600160705           CPVflo = NewCPVflo;
035700160705           write TNCPVT00;
035800160705           leavesr;
035900160705         ENDIF;
036000160705         //?Se sono uguali unisco FLO e aggiorno
036100160705         xx = %lookup(' ':TipoVar);
036200160705         IF  xx > 0;
036300160708           zz = 1;
036400160705           FOR zz by 1 to %elem(TipoVar);
036500160705             IF  %subst(OldCPVflo:(zz):1) <> *blanks;
036600160705               IF  %lookup(%subst(OldCPVflo:(zz):1):TipoVar) = 0;
036700160705                 TipoVar(xx) = %subst(OldCPVflo:(zz):1);
036800160705                 xx += 1;
036900160705               ENDIF;
037000160705             ENDIF;
037100160705           ENDFOR;
037200160705         ENDIF;
037300160705         sorta TipoVar;
037400160705         yy = 1;
037500160705         FOR yy by 1 to %elem(TipoVar);
037600160705           %subst(CPVflo:(yy):1) = TipoVar(yy);
037700160705         ENDFOR;
037800160705         CPVflo = %trim(CPVflo);
037900160705         update TNCPVT00;
038000160704
038100160704       ENDSR;
038200160704
038300160704       //--------------------------------------------------------------
038400160704       //?Salvo il codice del potenziale.
038500160704       //--------------------------------------------------------------
038600160704       BEGSR SalvaPotenziale;
038700160704
038800160704         SELECT;
038900160704         WHEN  CPOcpo > 0;
039000160704           CPVcpo = CPOcpo;
039100160705         WHEN  CPO1cpo > 0;
039200160705           CPVcpo = CPO1cpo;
039300160705         OTHER;
039400160705           CPVcpo = CPIcpo;
039500160704         ENDSL;
039600160704
039700160704         Potenziale = CPVcpo;
039800160704
039900160704       ENDSR;
040000160704
040100160704       //--------------------------------------------------------------
040200160704       //?Operazioni finali.
040300160704       //--------------------------------------------------------------
040400160704       BEGSR RoutEnd;
040500160704
040600160704         IF  IMK30tla = *blanks;
040700160704           *inRT = *on;
040800160704         ELSE;
040900160704           *inLR = *on;
041000160704         ENDIF;
041100160704
041200160704         return;
041300160704
041400160704       ENDSR;
041500160704
041600160704      /end-free
041700160704
041800160704       //--------------------------------------------------------------
041900160704       //?Schiere a tempo di compilazione.
042000160704       //--------------------------------------------------------------
042100160704
042200160704** -- MSG -------------------------------------------------------------------*
042300160704Errore: non passata l'immagine PRECEDENTE. Variazione non registrabile
042400160704Errore: Cod.Potenziale diverso tra immagine PRECEDENTE immagine SEGUENTE
