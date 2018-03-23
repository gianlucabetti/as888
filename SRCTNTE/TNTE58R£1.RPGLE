000100121025       //==============================================================
000200130222       //?TNTE58R * Lancio Simulazione Delta Tariffa/Offerta
000300121025       //==============================================================
000400121025
000500121025       //--------------------------------------------------------------
000600121025       //?Specifiche di controllo.                                     ?
000700121025       //--------------------------------------------------------------
000800121025
000900121025     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000121025
001100121025       //--------------------------------------------------------------
001200121025       //?Dichiarazione file.                                          ?
001300121025       //--------------------------------------------------------------
001400121025
001500121025       // -?File Tabella
001600121025     fTNTBE01L  uf   e           k disk
001700121025
001800121025       // -?File video
001900130222     fTNTE58D   cf   e             workstn
002000121107     f                                     indds(IndDspF)
002100121107     f                                     infds(InfDspF)
002200121025
002300121025       //--------------------------------------------------------------
002400121025       //?Definizione costanti.                                        ?
002500121025       //--------------------------------------------------------------
002600121025
002700121024      // - Tasti funzionali a video
002800121024     d c_F01           c                   const(x'31')
002900121024     d c_F02           c                   const(x'32')
003000121024     d c_F03           c                   const(x'33')
003100121024     d c_F04           c                   const(x'34')
003200121024     d c_F05           c                   const(x'35')
003300121024     d c_F06           c                   const(x'36')
003400121024     d c_F07           c                   const(x'37')
003500121024     d c_F08           c                   const(x'38')
003600121024     d c_F09           c                   const(x'39')
003700121024     d c_F10           c                   const(x'3A')
003800121024     d c_F11           c                   const(x'3B')
003900121024     d c_F12           c                   const(x'3C')
004000121024     d c_F13           c                   const(x'B1')
004100121024     d c_F14           c                   const(x'B2')
004200121024     d c_F15           c                   const(x'B3')
004300121024     d c_F16           c                   const(x'B4')
004400121024     d c_F17           c                   const(x'B5')
004500121024     d c_F18           c                   const(x'B6')
004600121024     d c_F19           c                   const(x'B7')
004700121024     d c_F20           c                   const(x'B8')
004800121024     d c_F21           c                   const(x'B9')
004900121024     d c_F22           c                   const(x'BA')
005000121024     d c_F23           c                   const(x'BB')
005100121024     d c_F24           c                   const(x'BC')
005200121024     d c_Enter         c                   const(x'F1')
005300121024     d c_RollDown      c                   const(x'F4')
005400121024     d c_RollUp        c                   const(x'F5')
005500121024
005600121024     d Digits          c                   const('0123456789')
005700121025
005800121025       //--------------------------------------------------------------
005900121025       //?Definizione schiere.                                         ?
006000121025       //--------------------------------------------------------------
006100121025
006200121024      // -?Messaggi di errore
006300121024     d skMsg           s             78    dim(31) ctdata perrcd(1)
006400121025
006500121025       //--------------------------------------------------------------
006600121025       //?Definizione aree dati.                                       ?
006700121025       //--------------------------------------------------------------
006800121025
006900121025       // -?Dati utente?
007000121025     d §AzUte        e ds                  extname(AZUTE00F)
007100121025     d                                     dtaara
007200121025     d §DatiUte      e ds                  extname(dDatiUte)
007300121025     d                                     dtaara
007400121025
007500121025       //--------------------------------------------------------------
007600121025       //?Definizione strutture dati.                                  ?
007700121025       //--------------------------------------------------------------
007800121025
007900121025       // -?Status ds?
008000121025     d Status         sds
008100121025     d  SDSpgm           *proc
008200121025     d  SDSusr               254    263
008300121024
008400121024      // -?InfDS
008500121024     d InfDspF         ds
008600121024     d  dsp_aid              369    369a
008700121024
008800121024      // -?Indicatori su DspF
008900121024     d IndDspF         ds
009000121024      // -?Indicatori di Errore
009100121024     d  ErrMessage                    1n   overlay(IndDspF : 28)
009200130222     d  PosCurKsc01                   1n   overlay(IndDspF : 50)
009300130222     d  PosCurKsc02                   1n   overlay(IndDspF : 51)
009400130222     d  PosCurKsc03                   1n   overlay(IndDspF : 52)
009500130222     d  PosCurKsc04                   1n   overlay(IndDspF : 53)
009600130222     d  PosCurKsc05                   1n   overlay(IndDspF : 54)
009700130222     d  PosCurTar01                   1n   overlay(IndDspF : 55)
009800130222     d  PosCurTar02                   1n   overlay(IndDspF : 56)
009900130222     d  PosCurTar03                   1n   overlay(IndDspF : 57)
010000130222     d  PosCurTar04                   1n   overlay(IndDspF : 58)
010100130222     d  PosCurTar05                   1n   overlay(IndDspF : 59)
010200130222     d  PosCurLnp01                   1n   overlay(IndDspF : 60)
010300130222     d  PosCurLnp02                   1n   overlay(IndDspF : 61)
010400130222     d  PosCurLnp03                   1n   overlay(IndDspF : 62)
010500130222     d  PosCurLnp04                   1n   overlay(IndDspF : 63)
010600130222     d  PosCurLnp05                   1n   overlay(IndDspF : 64)
010700130222     d  PosCurDspDal                  1n   overlay(IndDspF : 65)
010800130222     d  PosCurDspAl                   1n   overlay(IndDspF : 66)
010900130222     d  PosCurCts01                   1n   overlay(IndDspF : 67)
011000130222     d  PosCurCts02                   1n   overlay(IndDspF : 68)
011100130222     d  PosCurCts03                   1n   overlay(IndDspF : 69)
011200130222     d  PosCurCts04                   1n   overlay(IndDspF : 70)
011300130222     d  PosCurCts05                   1n   overlay(IndDspF : 71)
011400130222     d  PosCurCts06                   1n   overlay(IndDspF : 72)
011500130222     d  PosCurCts07                   1n   overlay(IndDspF : 73)
011600130222     d  PosCurCts08                   1n   overlay(IndDspF : 74)
011700130222     d  PosCurCts09                   1n   overlay(IndDspF : 75)
011800130222     d  PosCurCts10                   1n   overlay(IndDspF : 76)
011900130222     d  PosCurPkgDa                   1n   overlay(IndDspF : 77)
012000130222     d  PosCurPkgA                    1n   overlay(IndDspF : 78)
012100130222     d  PosCurNclDa                   1n   overlay(IndDspF : 79)
012200130222     d  PosCurNclA                    1n   overlay(IndDspF : 80)
012300130222     d  PosCurKsc                     1n   overlay(IndDspF : 81)
012400130222     d  PosCurCtr                     1n   overlay(IndDspF : 82)
012500130222     d  PosCurPrg                     1n   overlay(IndDspF : 83)
012600130222     d  PosCurLnp                     1n   overlay(IndDspF : 84)
012700121024      // -?Indicatori di errore generico
012800121024     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012900121024
013000121024      // -?DS indicatori Dspf
013100121024     d WindDspF        ds                  inz
013200121024     d   WindDspFa             1     49    inz(*zero)
013300121024     d   WindDspFb            50     99    inz(*zero)
013400121024
013500121024      // -?DS controllo data
013600121024     d wlbdat          ds                  inz
013700121024     d  g02dat                 1      8  0
013800121024     d  g02inv                 9     16  0
013900121024     d  g02err                17     17
014000121024     d  g02tgi                18     22  0
014100121025
014200121025       // -?Parametri ricevuti
014300121025     d KPJBA         e ds
014400100507
014500121025       // -?Parametri per Reperimento dati utente?
014600121025     d TIBS34ds      e ds
014700130222
014800130222       // -?Abilitazioni utente
014900130222     d TNTAA1DS      e ds                  inz
015000121025
015100121025       //--------------------------------------------------------------
015200121025       //?Definizione variabili globali.                               ?
015300121025       //--------------------------------------------------------------
015400121025
015500121025       // -?Flags booleani
015600121024     d wEnd            s               n   inz(*off)
015700121024     d wErrGrave       s               n   inz(*off)
015800121024     d wFine           s               n   inz(*off)
015900130222     d wInzD01         s               n   inz(*on)
016000121024
016100121024       // -?Campi Data
016200130222     d wData_ISO       s               d   datfmt(*iso)
016300130222     d wData_EUR       s               d   datfmt(*eur)
016400121024     d wOggi           s              8s 0 inz
016500121025
016600121025       //--------------------------------------------------------------
016700121025       //?Definizione procedure esterne.                     ?
016800121025       //--------------------------------------------------------------
016900121025
017000121025       // -?Sottomissione lavoro batch
017100121025     d bch10           pr                  extpgm('BCH10')
017200121025     d  kpjba                              likeds(KPJBA)
017300121025
017400121025
017500121024       //--------------------------------------------------------------
017600121024       //?Definizione prototipi.
017700121024       //--------------------------------------------------------------
017800121024
017900121025      /copy gaitrasrc/srcprotopr,tibs34r
018000130222      /copy gaitrasrc/srcprotopr,tntaa1c
018100121024      /copy gaitrasrc/srcprotopr,xsrda8
018200121025
018300121025       //--------------------------------------------------------------
018400121025       //?Definizione key-list.                                        ?
018500121025       //--------------------------------------------------------------
018600121025
018700121025       //--------------------------------------------------------------
018800121025       //?M A I N - L I N E                                            ?
018900121025       //--------------------------------------------------------------
019000121025
019100121025     c     *Entry        plist
019200121025     c                   parm                    KPJBA
019300121025
019400121025      /free
019500121025
019600121024       //?Operazioni iniziali?
019700121025       exsr RoutInz;
019800121025
019900121024       //?Gestione vidata?
020000121024       DOW wFine = *off;
020100130222         exsr GesD01;
020200121025       ENDDO;
020300121025
020400121024       //?Operazioni finali?
020500121025       exsr RoutEnd;
020600121025
020700121025       //--------------------------------------------------------------
020800121025       //?Operazioni iniziali.                                         ?
020900121025       //--------------------------------------------------------------
021000121024       BEGSR  RoutInz;
021100121025
021200121024       //?Reperimento dati job?
021300121024         exsr  DatiJob;
021400130222
021500130222       //?Reperimento Autorizzazione utente Tariffe
021600130222         reset TNTAA1DS;
021700130222         ITAA1caut = '§utegtc';
021800130222         kpjbu     = TNTAA1DS;
021900130222         tntaa1c (kpjba);
022000130222         TNTAA1DS = kpjbu;
022100130222
022200130222         IF  OTAA1fabi = 'N';
022300130222           wErrGrave   = *on;
022400130222           ErrMessage  = *on;
022500130222           ErrGenerico = *on;
022600130222           V01msg = skMsg(01);
022700130222           leavesr;
022800130222         ENDIF;
022900121025
023000121025       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
023100121024         wOggi = %dec(%date());
023200121025
023300121025       ENDSR;
023400121025
023500121025       //--------------------------------------------------------------
023600121025       //?Reperimento Dati del job (Utente/Operativi).                 ?
023700121025       //--------------------------------------------------------------
023800121024       BEGSR  DatiJob;
023900121025
024000121025         in(E) §AzUte;
024100121025         IF  NOT %error;
024200121025           in(E) §DatiUte;
024300121025         ENDIF;
024400121025         IF  %error or RSut = *blanks;
024500121025           clear TIBS34DS;
024600121025           tibs34r ( TIBS34DS );
024700121025           in §AzUte;
024800121025           in §DatiUte;
024900121025         ENDIF;
025000121025
025100121025       ENDSR;
025200121024
025300121024       //--------------------------------------------------------------
025400121024       //?Gesdione videata.
025500121024       //--------------------------------------------------------------
025600121024       BEGSR  GesW01;
025700121024
025800121024       //?Inizializzazione videata
025900130222         IF  wInzD01;
026000130222           exsr Inzd01;
026100130222           wInzD01 = *off;
026200121024         ENDIF;
026300121024
026400121024       //?Emissione videata
026500130222         write TE58T01;
026600130222         exfmt TE58D01;
026700121024         ErrMessage   = *off;
026800121024         ErrGenerico  = *off;
026900130222         clear V01msg;
027000121024
027100121024         SELECT;
027200121024
027300121024       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
027400121025           WHEN  wErrGrave;
027500121025             wFine = *on;
027600121024
027700121024       //?F3=Fine
027800121024           WHEN  dsp_aid = c_F03;
027900130222             exsr F03D01;
028000121024
028100121024       //?F6=Conferma
028200121024           WHEN  dsp_aid = c_F06;
028300130222             exsr CtrD01;
028400121024             IF  ErrGenerico;
028500121024               leavesr;
028600121024             ENDIF;
028700130222             exsr F06D01;
028800121024
028900121024       //?Enter
029000121024           OTHER;
029100130222             exsr CtrD01;
029200121024             IF  ErrGenerico;
029300121024               leavesr;
029400121024             ENDIF;
029500121024
029600121024         ENDSL;
029700121024       ENDSR;
029800121024
029900121024       //--------------------------------------------------------------
030000121024       //?Inizializzazione videata.
030100121024       //--------------------------------------------------------------
030200130222       BEGSR InzD01;
030300121024
030400130222         clear TE58D01;
030500130222
030600130222       //?Imposto T=Tariffa
030700130222         V01TaOf = 'T';
030800121205
030900130222       //?Imposto S=Spedizione in Data Calcolo
031000130222         V01DtaCal = 'S';
031100130222
031200130222       //?Imposto N=Stampa dettaglio
031300130222         V01StpDet = 'N';
031400121025
031500121025         clear wlbdat;
031600121205
031700130222       //?Data Spedizione DAL oggi - 1 anno
031800130222         wData_ISO = %date(wOggi:*iso);
031900130222         wData_EUR = wData_ISO - %years(1);
032000130222         V01SpeDal = %dec(wData_EUR);
032100130222
032200130222       //?Data Spedizione AL oggi - 1 giorno
032300130222         wData_ISO = %date(wOggi:*iso);
032400130222         wData_EUR = wData_ISO - %days(1);
032500130222         V01SpeAl = %dec(wData_EUR);
032600121024
032700121024       ENDSR;
032800121024
032900121024       //--------------------------------------------------------------
033000121024       //?Gestione tasto funzionale F3.
033100121024       //?F3=Fine
033200121024       //--------------------------------------------------------------
033300121024       BEGSR F03W01;
033400121024
033500121024       //?Chiusura del programma
033600121024         wFine = *on;
033700121024
033800121024       ENDSR;
033900121024
034000121024       //--------------------------------------------------------------
034100121024       //?Gestione tasto funzionale F6 da videata D01
034200121024       //?F6=Conferma
034300121024       //--------------------------------------------------------------
034400121025       BEGSR F06W01;
034500121024
034600121205       //?Lancia la pulizia attività/trattative
034700121205         IF  W01crm = 'SI';
034800121205           kpjbu = %editc(wDataPULc:'X');
034900121205           kcoaz = 'MKS1';
035000121205           kbuff = *blanks;
035100121025
035200121205         //?Richiamo interattivo
035300121205           IF  knmus = *all'1';
035400121205             TRMKS01C(kpjba);
035500121205         //?Richiamo batch
035600121205           ELSE;
035700121205             BCH10(kpjba);
035800121205           ENDIF;
035900121205         ENDIF;
036000121205
036100121205       //?Lancia la pulizia potenziali eliminabili
036200121205         IF  W01pot = 'SI';
036300121205           kpjbu = %editc(wDataPULp:'X');
036400121205           kcoaz = 'MKS6';
036500121205           kbuff = *blanks;
036600121205
036700121205         //?Richiamo interattivo
036800121205           IF  knmus = *all'1';
036900121205             TRMKS06C(kpjba);
037000121205         //?Richiamo batch
037100121205           ELSE;
037200121205             BCH10(kpjba);
037300121205           ENDIF;
037400121205         ENDIF;
037500121024
037600121024       //?Aggiorno la data ultima pulzia sulla tabella CRM
037700121205         IF  W01crm = 'SI';
037800121205           §CRMdatac = %editc(wDataPULc:'X');
037900121205         ENDIF;
038000121205         IF  W01pot = 'SI';
038100121205           §CRMdatap = %editc(wDataPULp:'X');
038200121205         ENDIF;
038300121205         IF  W01crm ='SI' or W01pot = 'SI';
038400121205           TBEcod = 'PUL';
038500121205           TBEke1 = 'CRM';
038600121205           chain (TBEcod:TBEke1) TNTBE01L;
038700121205           IF  %found(TNTBE01L);
038800121211             TBEuni = dPULCRM;
038900121205             UPDATE  TNTBE000;
039000121205           ENDIF;
039100121205         ENDIF;
039200121025
039300121025       //?Chiusura del programma
039400121025         wFine = *on;
039500121024
039600121024       ENDSR;
039700121024
039800121024       //--------------------------------------------------------------
039900121024       //?Controllo videata.
040000121024       //--------------------------------------------------------------
040100121024       BEGSR CtrW01;
040200121024
040300121024         WindDspF = IndDspF;
040400121024         reset WindDspFb;
040500121024         IndDspF  = WindDspF;
040600121024
040700121205       //?Se pulizia attività/trattative
040800121205         IF  W01crm = 'SI';
040900121205
041000121205         //?Data pulizia attività/trattative
041100121205           IF  W01datac  = 0;
041200121205             ErrMessage  = *on;
041300121205             ErrGenerico = *on;
041400121205             PosCurDatac = *on;
041500121205             W01msg      = skMsg(02);
041600121205             leavesr;
041700121205           ENDIF;
041800121025
041900121205           clear wlbdat;
042000121205           G02dat = W01datac;
042100121205           xsrda8(wlbdat);
042200121205           IF  G02err = '1';
042300121205             ErrMessage  = *on;
042400121205             ErrGenerico = *on;
042500121205             PosCurDatac = *on;
042600121205             W01msg      = skMsg(02);
042700121205             leavesr;
042800121205           ENDIF;
042900121025
043000121205           IF  G02inv = wDataCRM;
043100121205             ErrMessage  = *on;
043200121205             ErrGenerico = *on;
043300121205             PosCurDatac = *on;
043400121205             W01msg      = skMsg(02);
043500121205             W01msg      = %trim(W01msg) + ' Data già elaborata.';
043600121205             leavesr;
043700121205           ENDIF;
043800121025
043900121205           wDataPULc = G02inv;
044000121205         ENDIF;
044100121205
044200121205       //?Se pulizia potenziali eliminabili
044300121205         IF  W01pot = 'SI';
044400121205
044500121205         //?Data pulizia potenziali eliminabili
044600121205           IF  W01datap  = 0;
044700121205             ErrMessage  = *on;
044800121205             ErrGenerico = *on;
044900121205             PosCurDatap = *on;
045000121205             W01msg      = skMsg(02);
045100121205             leavesr;
045200121205           ENDIF;
045300121205
045400121205           clear wlbdat;
045500121205           G02dat = W01datap;
045600121205           xsrda8(wlbdat);
045700121205           IF  G02err = '1';
045800121205             ErrMessage  = *on;
045900121205             ErrGenerico = *on;
046000121205             PosCurDatap = *on;
046100121205             W01msg      = skMsg(02);
046200121205             leavesr;
046300121205           ENDIF;
046400121205
046500121205           IF  G02inv = wDataPOT;
046600121205             ErrMessage  = *on;
046700121205             ErrGenerico = *on;
046800121205             PosCurDatap = *on;
046900121205             W01msg      = skMsg(02);
047000121205             W01msg      = %trim(W01msg) + ' Data già elaborata.';
047100121205             leavesr;
047200121205           ENDIF;
047300121205
047400121211           wDataPULp = G02inv;
047500121205         ENDIF;
047600121024
047700121024       ENDSR;
047800121025
047900121025       //--------------------------------------------------------------
048000121025       //?Operazioni finali.                                           ?
048100121025       //--------------------------------------------------------------
048200121024       BEGSR  RoutEnd;
048300121025
048400121025         *inLR = *on;
048500100507
048600121025       //?Uscita dal pgm
048700121025         return;
048800121025
048900121025       ENDSR;
049000121025
049100121025      /end-free
049200121024
049300121024       //--------------------------------------------------------------
049400121024       //?Schiere a tempo di compilazione.
049500121024       //--------------------------------------------------------------
049600121024
049700130222** - skMSG ------------------------------------------------------------------*
049800130222Utente non abilitato alla funzione                                             01
