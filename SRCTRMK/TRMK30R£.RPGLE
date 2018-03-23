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
004400160704
004500160704      // - Ds Letture Variazioni
004600160705     d dcpv_a        e ds                  prefix(d)
004700160705     d wdcpv_a       e ds                  extname(dcpv_a)
004800160704     d                                     prefix(w)
004900160706     d dcpv_b        e ds                  prefix(d)
005000160706     d wdcpv_b       e ds                  extname(dcpv_b)
005100160705     d                                     prefix(w)
005200160706     d dcpv_c        e ds                  prefix(d)
005300160706     d wdcpv_c       e ds                  extname(dcpv_c)
005400160705     d                                     prefix(w)
005500160706     d dcpv_d        e ds                  prefix(d)
005600160706     d wdcpv_d       e ds                  extname(dcpv_d)
005700160705     d                                     prefix(w)
005800160706     d dcpv_e        e ds                  prefix(d)
005900160706     d wdcpv_e       e ds                  extname(dcpv_e)
006000160705     d                                     prefix(w)
006100160705
006200160705      // - Ds File Testata
006300160705     d wTNCPVT       e ds                  extname(TNCPVT0F)
006400160705     d                                     prefix(w)
006500160705     d TNCPVT        e ds                  extname(TNCPVT0F)
006600160704
006700160704      //---------------------------------------------------------------
006800160704      //?Definizione variabili globali.
006900160704      //---------------------------------------------------------------
007000160704      // - Indici di Schiera
007100160704     d xx              s              2s 0 inz
007200160704     d yy              s              2s 0 inz
007300160704     d zz              s              2s 0 inz
007400160704
007500160704      // - Campi di comodo
007600160704     d Conta           s              4s 0 inz
007700160705     d NewCPVflo       s                   like(CPVflo)
007800160705     d OldCPVflo       s                   like(CPVflo)
007900160704     d Potenziale      s                   like(CPVcpo)
008000160705     d PotenzialeOld   s                   like(CPVcpo)
008100160704
008200160704      //---------------------------------------------------------------
008300160704      //?Definizione Prototipi.
008400160704      //---------------------------------------------------------------
008500160704      /copy gaitrasrc/srcprotopr,TIBS34R
008600160704
008700160704      //---------------------------------------------------------------
008800160704      //?M A I N - L I N E
008900160704      //---------------------------------------------------------------
009000160704
009100160704     c     *Entry        plist
009200160704     c                   parm                    TRMK30DS
009300160704     c                   parm                    TNCPODS
009400160705     c                   parm                    TNCPO1DS
009500160705     c                   parm                    TICPIDS
009600160704
009700160704      /free
009800160704
009900160704       //?Operazioni iniziali
010000160704       exsr RoutInz;
010100160704
010200160704       //?Elabora
010300160704       IF  IMK30tla <> 'C';
010400160704         exsr Elabora;
010500160704       ENDIF;
010600160704
010700160704       //?Operazioni finali
010800160704       exsr RoutEnd;
010900160704
011000160704       //--------------------------------------------------------------
011100160704       //?Operazioni iniziali.
011200160704       //--------------------------------------------------------------
011300160704       BEGSR RoutInz;
011400160704
011500160704       //?Reperimento dati job
011600160704         exsr DatiJob;
011700160704
011800160704       ENDSR;
011900160704
012000160704       //--------------------------------------------------------------
012100160704       //?Reperimento Dati del job (Utente/Operativi).
012200160704       //--------------------------------------------------------------
012300160704       BEGSR DatiJob;
012400160704
012500160704         in(E) §AzUte;
012600160704         IF  NOT %error;
012700160704           in(E) §DatiUte;
012800160704         ENDIF;
012900160704         IF  %error or RSut = *blanks;
013000160704           clear TIBS34ds;
013100160704           tibs34r(tibs34ds);
013200160704           in §AzUte;
013300160704           in §DatiUte;
013400160704         ENDIF;
013500160704
013600160704       ENDSR;
013700160704
013800160704       //--------------------------------------------------------------
013900160704       //?Elabora.
014000160704       //--------------------------------------------------------------
014100160704       BEGSR Elabora;
014200160704
014300160704       //?Memorizzo l'immagine PRIMA
014400160704         IF  IMK30immag = 'P';
014500160704           clear conta;
014600160704           exsr ImpostaDS;
014700160704           exsr ImmaginePrima;
014800160704         ENDIF;
014900160704
015000160704       //?Memorizzo l'immagine DOPO
015100160704         IF  IMK30immag = 'D';
015200160704           exsr SalvaPotenziale;
015300160704
015400160704           SELECT;
015500160704         //?Inserimento: solo immagine DOPO
015600160704           WHEN  IMK30cta <> 'M';
015700160704             exsr ImpostaDS;
015800160704             exsr ImmagineDopo;
015900160704         //?Errore: se non c'è l'immagine PRIMA
016000160704           WHEN  conta <> 1;
016100160704             OMK30err = '1';
016200160704             OMK30msg = Msg(01);
016300160707         //?Errore: se potenziale diverso tra PRIMA e DOPO
016400160704           WHEN  Potenziale <> PotenzialeOld;
016500160704             OMK30err = '1';
016600160704             OMK30msg = Msg(02);
016700160704         //?Manutenzione
016800160704           OTHER;
016900160704             exsr ImpostaDS;
017000160704             exsr ImmagineDopo;
017100160704           ENDSL;
017200160704         ENDIF;
017300160704
017400160704       ENDSR;
017500160704
017600160704       //--------------------------------------------------------------
017700160704       //?Imposta i dati nelle DS.
017800160704       //--------------------------------------------------------------
017900160704       BEGSR ImpostaDS;
018000160704
018100160704         IF  IMK30tvp = 'A' or IMK30tvp = *blanks;
018200160705           dCPOrag = CPOrag;
018300160705           dCPOvia = CPOvia;
018400160705           dCPOcit = CPOcit;
018500160705           dCPOcap = CPOcap;
018600160705           dCPOprv = CPOprv;
018700160705           dCPOnaz = CPOnaz;
018800160705           dCPO1tel = CPO1tel;
018900160705           dCPO1fax = CPO1fax;
019000160704         ENDIF;
019100160706
019200160706         IF  IMK30tvp = 'B' or IMK30tvp = *blanks;
019300160706           dCPOpiv = CPOpiv;
019400160706           dCPOcdf = CPOcdf;
019500160706         ENDIF;
019600160704
019700160706         IF  IMK30tvp = 'C' or IMK30tvp = *blanks;
019800160705           dCPOsct = CPOsct;
019900160704           dCPOfsf = CPOfsf;
020000160704         ENDIF;
020100160704
020200160706         IF  IMK30tvp = 'D' or IMK30tvp = *blanks;
020300160704           dCPIpft = CPIpft;
020400160704           dCPOftr = CPOftr;
020500160704         ENDIF;
020600160704
020700160706         IF  IMK30tvp = 'E' or IMK30tvp = *blanks;
020800160704           dCPOcmm = CPOcmm;
020900160704           dCPOflt = CPOflt;
021000160704         ENDIF;
021100160704
021200160704       ENDSR;
021300160704
021400160704       //--------------------------------------------------------------
021500160704       //?Immagine PRIMA.
021600160704       //--------------------------------------------------------------
021700160704       BEGSR ImmaginePrima;
021800160704
021900160705         Conta = 1;
022000160704         exsr SalvaPotenziale;
022100160704         PotenzialeOld = Potenziale;
022200160704
022300160704         //?Imposto le DS per verificare se i dati sono variati
022400160704         IF  IMK30tvp = 'A' or IMK30tvp = *blanks;
022500160704           wdCPV_a = dCPV_a;
022600160704         ENDIF;
022700160704         IF  IMK30tvp = 'B' or IMK30tvp = *blanks;
022800160704           wdCPV_b = dCPV_b;
022900160704         ENDIF;
023000160704         IF  IMK30tvp = 'C' or IMK30tvp = *blanks;
023100160704           wdCPV_c = dCPV_c;
023200160704         ENDIF;
023300160704         IF  IMK30tvp = 'D' or IMK30tvp = *blanks;
023400160704           wdCPV_d = dCPV_d;
023500160704         ENDIF;
023600160706         IF  IMK30tvp = 'E' or IMK30tvp = *blanks;
023700160706           wdCPV_e = dCPV_e;
023800160706         ENDIF;
023900160704
024000160704       ENDSR;
024100160704
024200160704       //--------------------------------------------------------------
024300160704       //?Immagine DOPO.
024400160704       //--------------------------------------------------------------
024500160704       BEGSR ImmagineDopo;
024600160704
024700160704         clear TipoVar;
024800160704         clear xx;
024900160704         clear TNCPVT00;
025000160704         clear TNCPVD00;
025100160704
025200160704         exsr SalvaPotenziale;
025300160704
025400160704         CPVdav = %dec(%date());
025500160704         CPVorv = %dec(%time());
025600160704         CPVpru = IMK30pru;
025700160704         CPVnoj = IMK30noj;
025800160704         CPVpgm = IMK30pgm;
025900160704
026000160704         //?Inserimento
026100160704         IF  IMK30cta <> 'M';
026200160704           CPVcta = IMK30cta;
026300160712           CPVflo = 'ABCDE';
026400160704           write TNCPVT00;
026500160705           leavesr;
026600160704         ENDIF;
026700160704
026800160704         //?Manutenzione
026900160706         //?RCD A
027000160706         //?verifico se dati variati ma non scrivo dettaglio
027100160706         IF  IMK30tvp = 'A' or IMK30tvp = *blanks;
027200160706           IF  wdCPV_a <> dCPV_a;
027300160706             xx += 1;
027400160706             TipoVar(xx) = 'A';
027500160706           ENDIF;
027600160705         ENDIF;
027700160705         //?RCD B
027800160705         //?verifico se dati variati
027900160705         IF  IMK30tvp = 'B' or IMK30tvp = *blanks;
028000160705           IF  wdCPV_b <> dCPV_b;
028100160705             xx += 1;
028200160705             TipoVar(xx) = 'B';
028300160705             CPVtvp = 'B';
028400160705             CPVprima = wdCPV_b;
028500160705             CPVdopo  = dCPV_b;
028600160705             write TNCPVD00;
028700160705           ENDIF;
028800160705         ENDIF;
028900160705         //?RCD C
029000160705         //?verifico se dati variati
029100160705         IF  IMK30tvp = 'C' or IMK30tvp = *blanks;
029200160705           IF  wdCPV_c <> dCPV_c;
029300160705             xx += 1;
029400160705             TipoVar(xx) = 'C';
029500160705             CPVtvp = 'C';
029600160705             CPVprima = wdCPV_c;
029700160705             CPVdopo  = dCPV_c;
029800160705             write TNCPVD00;
029900160705           ENDIF;
030000160705         ENDIF;
030100160705         //?RCD D
030200160705         //?verifico se dati variati
030300160705         IF  IMK30tvp = 'D' or IMK30tvp = *blanks;
030400160705           IF  wdCPV_d <> dCPV_d;
030500160705             xx += 1;
030600160705             TipoVar(xx) = 'D';
030700160705             CPVtvp = 'D';
030800160705             CPVprima = wdCPV_d;
030900160705             CPVdopo  = dCPV_d;
031000160705             write TNCPVD00;
031100160705           ENDIF;
031200160705         ENDIF;
031300160706         //?RCD E
031400160706         //?verifico se dati variati
031500160706         IF  IMK30tvp = 'E' or IMK30tvp = *blanks;
031600160706           IF  wdCPV_e <> dCPV_e;
031700160706             xx += 1;
031800160706             TipoVar(xx) = 'E';
031900160706             CPVtvp = 'E';
032000160706             CPVprima = wdCPV_e;
032100160706             CPVdopo  = dCPV_e;
032200160706             write TNCPVD00;
032300160706           ENDIF;
032400160706         ENDIF;
032500160705
032600160705         //?Scrittura file di testata
032700160705         IF  TipoVar(1) = *blanks;
032800160705           leavesr;
032900160705         ENDIF;
033000160705
033100160705         CPVcta = 'M';
033200160705         yy = 1;
033300160705         FOR yy by 1 to %elem(TipoVar);
033400160705           %subst(CPVflo:(yy):1) = TipoVar(yy);
033500160705         ENDFOR;
033600160705         //?Salvo il rcd di testata e verifico se sono uguali a parte il campo FLO
033700160705         wTNCPVT = TNCPVT;
033800160705         NewCPVflo = CPVflo;
033900160705         clear wCPVflo;
034000160705         chain (CPVcpo:CPVdav:CPVorv) TNCPVT1L;
034100160705         IF  not %found(TNCPVT1L);
034200160705           write TNCPVT00;
034300160705           leavesr;
034400160705         ENDIF;
034500160705         //?Salvo FLO
034600160705         OldCPVflo = CPVflo;
034700160705         clear CPVflo;
034800160705         //?Se sono diversi scrivo il rcd
034900160705         IF  TNCPVT <> wTNCPVT;
035000160705           TNCPVT = wTNCPVT;
035100160705           CPVflo = NewCPVflo;
035200160705           write TNCPVT00;
035300160705           leavesr;
035400160705         ENDIF;
035500160705         //?Se sono uguali unisco FLO e aggiorno
035600160705         xx = %lookup(' ':TipoVar);
035700160705         IF  xx > 0;
035800160708           zz = 1;
035900160705           FOR zz by 1 to %elem(TipoVar);
036000160705             IF  %subst(OldCPVflo:(zz):1) <> *blanks;
036100160705               IF  %lookup(%subst(OldCPVflo:(zz):1):TipoVar) = 0;
036200160705                 TipoVar(xx) = %subst(OldCPVflo:(zz):1);
036300160705                 xx += 1;
036400160705               ENDIF;
036500160705             ENDIF;
036600160705           ENDFOR;
036700160705         ENDIF;
036800160705         sorta TipoVar;
036900160705         yy = 1;
037000160705         FOR yy by 1 to %elem(TipoVar);
037100160705           %subst(CPVflo:(yy):1) = TipoVar(yy);
037200160705         ENDFOR;
037300160705         CPVflo = %trim(CPVflo);
037400160705         update TNCPVT00;
037500160704
037600160704       ENDSR;
037700160704
037800160704       //--------------------------------------------------------------
037900160704       //?Salvo il codice del potenziale.
038000160704       //--------------------------------------------------------------
038100160704       BEGSR SalvaPotenziale;
038200160704
038300160704         SELECT;
038400160704         WHEN  CPOcpo > 0;
038500160704           CPVcpo = CPOcpo;
038600160705         WHEN  CPO1cpo > 0;
038700160705           CPVcpo = CPO1cpo;
038800160705         OTHER;
038900160705           CPVcpo = CPIcpo;
039000160704         ENDSL;
039100160704
039200160704         Potenziale = CPVcpo;
039300160704
039400160704       ENDSR;
039500160704
039600160704       //--------------------------------------------------------------
039700160704       //?Operazioni finali.
039800160704       //--------------------------------------------------------------
039900160704       BEGSR RoutEnd;
040000160704
040100160704         IF  IMK30tla = *blanks;
040200160704           *inRT = *on;
040300160704         ELSE;
040400160704           *inLR = *on;
040500160704         ENDIF;
040600160704
040700160704         return;
040800160704
040900160704       ENDSR;
041000160704
041100160704      /end-free
041200160704
041300160704       //--------------------------------------------------------------
041400160704       //?Schiere a tempo di compilazione.
041500160704       //--------------------------------------------------------------
041600160704
041700160704** -- MSG -------------------------------------------------------------------*
041800160704Errore: non passata l'immagine PRECEDENTE. Variazione non registrabile
041900160704Errore: Cod.Potenziale diverso tra immagine PRECEDENTE immagine SEGUENTE
