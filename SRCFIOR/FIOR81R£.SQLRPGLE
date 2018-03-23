000100130715       //==============================================================
000200130715       //?FIOR81R - Interrogazione Anagrafica clienti Ritiro
000300130715       //==============================================================
000400130715
000500130715       //--------------------------------------------------------------
000600130715       //?Specifiche di controllo.                                     ?
000700130715       //--------------------------------------------------------------
000800130715
000900130715     h decedit('0,') datedit(*ymd.) option(*nodebugio)
001000130731     h dftactgrp(*no) actgrp(*caller)
001100130715
001200130715       //--------------------------------------------------------------
001300130715       //?Dichiarazione file.                                          ?
001400130715       //--------------------------------------------------------------
001500130715
001600070730      *---------------------------------------------------------------*
001700130715
001800130716       // -?File Organigramma
001900130716     fAZORG01L  if   e           k disk
002000130715
002100130731       // -?File video  24x80
002200130716     fFIOR81D   cf   e             workstn
002300130716     f                                     sfile(OR81S01:S01nrr)
002400130715     f                                     indds(IndDspF)
002500130715     f                                     infds(InfDspF)
002600130731     f                                     usropn
002700130731
002800130731       // -?File video  27x132
002900130731     fFIOR81D1  cf   e             workstn
003000130731     f                                     sfile(OR81S11:S11nrr)
003100130731     f                                     indds(IndDspF1)
003200130731     f                                     infds(InfDspF1)
003300130731     f                                     usropn
003400130715
003500130715       //--------------------------------------------------------------
003600130715       //?Definizione costanti.                                        ?
003700130715       //--------------------------------------------------------------
003800130729
003900130729
004000130729       // -?Numero massimo di record gestibili
004100130729     d c_MaxRec        c                   const(9999)
004200130715
004300130715       // -?Tasti funzionali a video
004400130715     d c_F01           c                   const(x'31')
004500130715     d c_F02           c                   const(x'32')
004600130715     d c_F03           c                   const(x'33')
004700130715     d c_F04           c                   const(x'34')
004800130715     d c_F05           c                   const(x'35')
004900130715     d c_F06           c                   const(x'36')
005000130715     d c_F07           c                   const(x'37')
005100130715     d c_F08           c                   const(x'38')
005200130715     d c_F09           c                   const(x'39')
005300130715     d c_F10           c                   const(x'3A')
005400130715     d c_F11           c                   const(x'3B')
005500130715     d c_F12           c                   const(x'3C')
005600130715     d c_F13           c                   const(x'B1')
005700130715     d c_F14           c                   const(x'B2')
005800130715     d c_F15           c                   const(x'B3')
005900130715     d c_F16           c                   const(x'B4')
006000130715     d c_F17           c                   const(x'B5')
006100130715     d c_F18           c                   const(x'B6')
006200130715     d c_F19           c                   const(x'B7')
006300130715     d c_F20           c                   const(x'B8')
006400130715     d c_F21           c                   const(x'B9')
006500130715     d c_F22           c                   const(x'BA')
006600130715     d c_F23           c                   const(x'BB')
006700130715     d c_F24           c                   const(x'BC')
006800130715     d c_Enter         c                   const(x'F1')
006900130715     d c_RollDown      c                   const(x'F4')
007000130715     d c_RollUp        c                   const(x'F5')
007100130715
007200130715     d Digits          c                   const('0123456789')
007300130715
007400130715       //--------------------------------------------------------------
007500130715       //?Definizione schiere.                                         ?
007600130715       //--------------------------------------------------------------
007700130715
007800130715      // -?Messaggi di errore
007900130715     d skMSG           s             78    dim(50) ctdata perrcd(1)
008000130715
008100130715       //--------------------------------------------------------------
008200130715       //?Definizione aree dati.                                       ?
008300130715       //--------------------------------------------------------------
008400130715
008500130715       // -?Dati utente?
008600130715     d §AzUte        e ds                  extname(AZUTE00F)
008700130715     d                                     dtaara
008800130715     d §DatiUte      e ds                  extname(dDatiUte)
008900130715     d                                     dtaara
009000130715
009100130715       //--------------------------------------------------------------
009200130715       //?Definizione strutture dati.                                  ?
009300130715       //--------------------------------------------------------------
009400130715
009500130715       // -?Status ds?
009600130715     d Status         sds
009700130715     d  SDSpgm           *proc
009800130715     d  SDSusr               254    263
009900130715
010000130715       // -?InfDS
010100130801     d InfDspF         ds                  QUALIFIED
010200130715     d  dsp_aid              369    369a
010300130715     d  sfl_rrn              376    377i 0
010400130715     d  min_nrr              378    379i 0
010500130715     d  num_rcds             380    381i 0
010600130731
010700130731       // -?InfDS
010800130801     d InfDspF1        ds                  likeds(InfDspF)
010900130715
011000130801       // -?Indicatori su DspF
011100130801     d IndDspF         ds                  QUALIFIED
011200130715       // -?Indicatori di visualizzazione Errore
011300130715     d  ErrMessage                    1n   overlay(IndDspF : 28)
011400130715       // -?Indicatori di gestione del subfile
011500130715     d  SflDsp                        1n   overlay(IndDspF : 30)
011600130715     d  SflDspCtl                     1n   overlay(IndDspF : 31)
011700130715     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011800130715     d  SflEnd                        1n   overlay(IndDspF : 33)
011900130930       // -?Indicatori di Posizionamento Cursore
012000130716     d  PosCurOPZ                     1n   overlay(IndDspF : 50)
012100130716     d  PosCurFIL                     1n   overlay(IndDspF : 51)
012200130716     d  PosCurRAG                     1n   overlay(IndDspF : 52)
012300131030     d  PosCurPRV                     1n   overlay(IndDspF : 53)
012400130715       // -?Indicatori di errore generico
012500130715     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012600130731
012700130731       // -?Indicatori su DspF
012800130731     d IndDspF1        ds                  likeds(IndDspF)
012900130715
013000130715       // -?DS indicatori Dspf
013100130715     d WindDspF        ds                  inz
013200130715     d   WindDspFa             1     49    inz(*zero)
013300130715     d   WindDspFb            50     99    inz(*zero)
013400130715
013500130715       // -?Parametri ricevuti
013600130715     d KPJBA         e ds
013700130716     d FIOR81DS      e ds
013800130731
013900130731       // -?Interrogazione Anagrafica clienti Ritiro
014000130731     d FIOR37DS      e ds
014100130715
014200130715       // -?Parametri per Reperimento dati utente?
014300130715     d TIBS34DS      e ds
014400130716
014500130716       // -?File anagrafico clienti ritiro
014600130716     d FNACRds       e ds                  extname(FNACR00F)
014700130715
014800130715       //--------------------------------------------------------------
014900130715       //?Definizione variabili globali.                               ?
015000130715       //--------------------------------------------------------------
015100130715
015200130715       // -?Flags booleani
015300130715     d wEnd            s               n   inz(*off)
015400130715     d wErrGrave       s               n   inz(*off)
015500130715     d wFine           s               n   inz(*off)
015600130716     d wInzS01         s               n   inz(*on)
015700130715
015800130715       // -?Indici di schiera
015900130715     d xx              s              4  0 inz
016000131210     d yy              s              2  0 inz
016100130716
016200130716       // -?Stringa SQL da eseguire
016300130716     d wSQL            s           2048    Varying        inz
016400130715
016500130715       // -?Campi di Comodo
016600130731     d c_SflPag        s              2s 0
016700130731     d dsp_mod         s             10I 0
016800130729     d sav_S01csr      s                   like(VC1rcd)
016900130716     d sav_VC1fil      s                   like(VC1fil)
017000131003     d sav_VC1prv      s                   like(VC1prv)
017100130716     d sav_VC1rag      s                   like(VC1rag)
017200130731     d sav_VC1ric      s                   like(VC1ric)
017300130716     d S01nrr          s              4  0 inz
017400130731     d S11nrr          s              4  0 inz
017500130731     d wNrr            s                   like(S01nrr)
017600130729     d wPag            s              4  0 inz
017700130729     d wRig            s              3  0 inz
017800130729     d wSflPag         s              3  0 inz
017900130716     d wVideo          s              2    inz('S1')
018000131210     d wRag60          s             60    inz
018100130715
018200130715       //--------------------------------------------------------------
018300130715       //?Definizione procedure esterne.                     ?
018400130715       //--------------------------------------------------------------
018500130731
018600130731       // -?Interrogazione anagrafica clienti ritiro
018700130731     d FIOR37R1        pr                  extpgm('FIOR37R1')
018800130731     d  kpjba                              likeds(kpjba)
018900130715
019000130715       //--------------------------------------------------------------
019100130715       //?Definizione prototipi.
019200130715       //--------------------------------------------------------------
019300130715
019400130715       // -?Reperimento dati utente?
019500130715      /copy gaitrasrc/srcprotopr,tibs34r
019600130731
019700130731       // -?Verifica se possibile video 27x132
019800130731      /copy gaitrasrc/srcprotopr,QsnQryModS
019900130715
020000130715       //--------------------------------------------------------------
020100130715       //?Definizione key-list.                                        ?
020200130715       //--------------------------------------------------------------
020300130715
020400130715       //--------------------------------------------------------------
020500130715       //?Indicatori.
020600130715       //--------------------------------------------------------------
020700130715
020800130715       //--------------------------------------------------------------
020900130715       //?M A I N - L I N E                                            ?
021000130715       //--------------------------------------------------------------
021100130715
021200130715     c     *Entry        plist
021300130715     c                   parm                    KPJBA
021400130716     c                   parm                    FIOR81DS
021500130715
021600130715      /free
021700130715
021800130715       //?Operazioni iniziali?
021900130715       exsr RoutInz;
022000130715
022100130715       //?Gestione vidate?
022200130715       DOW not wFine;
022300130715         SELECT;
022400130716           WHEN  wVideo = 'S1';
022500130716             exsr GesS01;
022600130715           OTHER;
022700130715             wFine = *on;
022800130715         ENDSL;
022900130715       ENDDO;
023000130715
023100130715       //?Operazioni finali?
023200130715       exsr RoutEnd;
023300130715
023400130715       //--------------------------------------------------------------
023500130715       //?Operazioni iniziali.                                         ?
023600130715       //--------------------------------------------------------------
023700130715       BEGSR  RoutInz;
023800130715
023900130715       //?Reperimento dati job?
024000130715         exsr  DatiJob;
024100130715
024200130715         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
024300130731
024400130731       //?Apro file viedeo in base a 24x80 o 27x132
024500130731         dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
024600130731         IF  dsp_mod = 0;
024700130731           c_SflPag = 16;
024800130731           open FIOR81D;
024900130731         ELSE;
025000130731           c_SflPag = 19;
025100130731           open FIOR81D1;
025200130731         ENDIF;
025300130715
025400130715       //?Imposto nome pgm a video
025500130716         VT1pgm = SDSpgm;
025600130716
025700130716       //?Imposto i campi che sono arrivati dal chiamante
025800130716         VC1fil     = I81fil;
025900130716         sav_VC1fil = I81fil;
026000130716         VC1rag     = I81rag;
026100130716         sav_VC1rag = I81rag;
026200150520         VC1prv     = I81prv;
026300150520         sav_VC1prv = I81prv;
026400130715
026500130715       ENDSR;
026600130715
026700130715       //--------------------------------------------------------------
026800130715       //?Reperimento Dati del job (Utente/Operativi).                 ?
026900130715       //--------------------------------------------------------------
027000130715       BEGSR  DatiJob;
027100130715
027200130715         in(E) §AzUte;
027300130715         IF  NOT %error;
027400130715           in(E) §DatiUte;
027500130715         ENDIF;
027600130715         IF  %error or RSut = *blanks;
027700130715           clear TIBS34DS;
027800130715           tibs34r ( TIBS34DS );
027900130715           in §AzUte;
028000130715           in §DatiUte;
028100130715         ENDIF;
028200130715
028300130715       ENDSR;
028400130715
028500130715       //--------------------------------------------------------------
028600130716       //?Gestione videata S01.
028700130715       //--------------------------------------------------------------
028800130716       BEGSR  GesS01;
028900130715
029000130715       //?Inizializzazione videata
029100130716         IF  wInzS01;
029200130716           exsr InzS01;
029300130716           wInzS01 = *off;
029400130715         ENDIF;
029500130729
029600130729       //?Emissione Testata e Piede con tasti funzionali abilitati?
029700130731         IF  dsp_mod = 0;
029800130731           write OR81T01;
029900130731           write OR81Z01;
030000130731         ELSE;
030100130731           write OR81T11;
030200130731           write OR81Z11;
030300130731         ENDIF;
030400130729
030500130729       //?Posizionamento cursore?
030600130729         IF  VC1csr > *zero;
030700130729           VC1rcd = VC1csr;
030800130729         ELSE;
030900130729           VC1rcd = 1;
031000130731           IF  dsp_mod = 0;
031100130731             write  OR81S00;
031200130731           ELSE;
031300130731             write  OR81S10;
031400130731           ENDIF;
031500130729         ENDIF;
031600130715
031700130715       //?Emissione videata
031800130731         IF  dsp_mod = 0;
031900130731           exfmt OR81C01;
032000130801           IndDspF.ErrMessage  = *off;
032100130801           IndDspF.ErrGenerico = *off;
032200130731         ELSE;
032300130731           exfmt OR81C11;
032400130801           IndDspF1.ErrMessage  = *off;
032500130801           IndDspF1.ErrGenerico = *off;
032600130731         ENDIF;
032700130729
032800130716         clear VC1msg;
032900130715
033000130715         SELECT;
033100131030
033200131030       //?Provincia
033300131030           WHEN  VC1prv <> sav_VC1prv;
033400131030             sav_VC1fil = VC1fil;
033500131030             sav_VC1rag = VC1rag;
033600131030             sav_VC1ric = VC1ric;
033700131030             sav_VC1prv = VC1prv;
033800131030             wInzS01 = *on;
033900131030             exsr CloseCursor;
034000131030             leavesr;
034100130716
034200130716       //?Controllo la filiale
034300130716           WHEN  VC1fil <> sav_VC1fil;
034400130716             exsr CTRfil;
034500130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
034600130731               leavesr;
034700130731             ENDIF;
034800130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
034900130801               leavesr;
035000130801             ENDIF;
035100130716             sav_VC1fil = VC1fil;
035200130716             sav_VC1rag = VC1rag;
035300130731             sav_VC1ric = VC1ric;
035400131003             sav_VC1prv = VC1prv;
035500130716             wInzS01 = *on;
035600130730             exsr CloseCursor;
035700130716             leavesr;
035800130716
035900130716       //?Ragione sociale
036000130716           WHEN  VC1rag <> sav_VC1rag;
036100130716             sav_VC1rag = VC1rag;
036200130716             sav_VC1fil = VC1fil;
036300130731             sav_VC1ric = VC1ric;
036400131003             sav_VC1prv = VC1prv;
036500130716             wInzS01 = *on;
036600130730             exsr CloseCursor;
036700130716             leavesr;
036800130731
036900130731       //?Ricerca per contenuto
037000130731           WHEN  VC1ric <> sav_VC1ric;
037100130731             exsr CTRric;
037200130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
037300130731               leavesr;
037400130731             ENDIF;
037500130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
037600130801               leavesr;
037700130801             ENDIF;
037800130731             sav_VC1rag = VC1rag;
037900130731             sav_VC1fil = VC1fil;
038000130731             sav_VC1ric = VC1ric;
038100131003             sav_VC1prv = VC1prv;
038200130731             wInzS01 = *on;
038300130731             exsr CloseCursor;
038400130731             leavesr;
038500130715
038600130716       //?F12=Ritorno
038700130801           WHEN  dsp_mod = 0 and InfDspF.dsp_aid = c_F12;
038800130716             exsr F12S01;
038900130801           WHEN  dsp_mod = 1 and InfDspF1.dsp_aid = c_F12;
039000130801             exsr F12S01;
039100130729
039200130729       //?Roll-Up?
039300130801           WHEN  dsp_mod = 0 and InfDspF.dsp_aid = c_RollUp;
039400130729             exsr RollUpS01;
039500130801           WHEN  dsp_mod = 1 and InfDspF1.dsp_aid = c_RollUp;
039600130801             exsr RollUpS01;
039700130729
039800130729       //?SubFile vuoto?
039900130731           WHEN  S01nrr = *zeros and dsp_mod = 0;
040000130731
040100130731       //?SubFile vuoto?
040200130731           WHEN  S11nrr = *zeros and dsp_mod <> 0;
040300130716
040400130715       //?Enter
040500130715           OTHER;
040600130716
040700130715         //?Eseguo i controlli
040800130716             exsr CtrS01;
040900130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
041000130715               leavesr;
041100130715             ENDIF;
041200130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
041300130801               leavesr;
041400130801             ENDIF;
041500130715
041600130715         ENDSL;
041700130715
041800130715       ENDSR;
041900130715
042000130715       //--------------------------------------------------------------
042100130716       //?Inizializzazione videata S01.
042200130715       //--------------------------------------------------------------
042300130716       BEGSR InzS01;
042400130729
042500130729       //?Spegnimento degli indicatori di errore?
042600130801         IF  dsp_mod = 0;
042700130801           %subst(IndDspF : 50) = *off;
042800130801         ELSE;
042900130801           %subst(IndDspF1 : 50) = *off;
043000130801         ENDIF;
043100130715
043200130715       //?Pulizia del Subfile
043300130716         exsr PulS01;
043400130715
043500130729       //?Preparo la stringa SQL
043600130716         exsr PrepSQL;
043700130716
043800130729       //?carico la prima pagina delle anagrafiche
043900130729         exsr OpenCursor;
044000130729         exsr ReadCursor;
044100130729         exsr RollUpS01;
044200130731
044300130731         IF  dsp_mod = 0;
044400130731           wNrr = S01nrr;
044500130731         ELSE;
044600130731           wNrr = S11nrr;
044700130731         ENDIF;
044800130731
044900130729       //?Impaginazione subfile?
045000130729       //?-> forza l'impaginazione sul 1° rec. del subfile
045100130731         IF  wNrr > *zeros;
045200130731           VC1rcd  = 1;
045300130731           VC1csr  = 1;
045400130731         ELSE;
045500130731           clear VC1rcd;
045600130731         ENDIF;
045700130930
045800130930       //?Se non ho ricevuto nessun dato di ricerca
045900130930       //?mi posiziono sulla ragione sociale
046000130930         IF  I81fil = 0 and I81rag = *blanks;
046100130930           IndDspF1.PosCurRAG = *on;
046200131030           IndDspF1.PosCurPRV = *off;
046300130930         ELSE;
046400131030       //?altrimenti mi posiziono sulla provincia
046500130930           IndDspF1.PosCurRAG = *off;
046600131030           IndDspF1.PosCurPRV = *on;
046700130930         ENDIF;
046800130715
046900130715       ENDSR;
047000130715
047100130715       //--------------------------------------------------------------
047200130716       //?Pulizia Subfile S01.
047300130715       //--------------------------------------------------------------
047400130716       BEGSR PulS01;
047500130715
047600130731         IF  dsp_mod = 0;
047700130801           IndDspF.SflDsp    = *on;
047800130801           IndDspF.SflDspCtl = *on;
047900130731           write  OR81C01;
048000130801           IndDspF.SflDspCtl = *off;
048100130801           IndDspF.SflEnd    = *off;
048200130731         ELSE;
048300130801           IndDspF1.SflDsp    = *on;
048400130801           IndDspF1.SflDspCtl = *on;
048500130731           write  OR81C11;
048600130801           IndDspF1.SflDspCtl = *off;
048700130801           IndDspF1.SflEnd    = *off;
048800130801         ENDIF;
048900130715
049000130729         clear wSflPag;
049100130716         clear VC1rcd;
049200130716         clear VC1csr;
049300130716         clear VC1msg;
049400130801
049500130801         IF  dsp_mod = 0;
049600130801           clear S01nrr;
049700130801           IndDspF.ErrMessage  = *off;
049800130801           IndDspF.ErrGenerico = *off;
049900130801         ELSE;
050000130801           clear S11nrr;
050100130801           IndDspF1.ErrMessage  = *off;
050200130801           IndDspF1.ErrGenerico = *off;
050300130801         ENDIF;
050400130715
050500130715         WindDspF = IndDspF;
050600130715         reset WindDspFb;
050700130715         IndDspF  = WindDspF;
050800130715
050900130715       ENDSR;
051000130716
051100130716       //--------------------------------------------------------------
051200130716       //?Preparo la Stringa SQL.
051300130716       //--------------------------------------------------------------
051400130716       BEGSR PrepSQL;
051500131210
051600131210         IF  sav_VC1rag <> *blanks;
051700131210           wRag60 = sav_VC1rag;
051800131210           yy = 1;
051900131210           DOW  yy <= 60;
052000131210             IF  %subst(wRag60:yy:1) = '''';
052100131210               wRag60 = %replace('''''':wRag60:yy:1);
052200131210               yy += 1;
052300131210             ENDIF;
052400131210             yy += 1;
052500131210           ENDDO;
052600131210         ENDIF;
052700130716
052800130730         wSQL = 'SELECT * from FNACR00F ';
052900130716
053000130731         IF  sav_VC1rag <> *blanks and sav_VC1fil <> *zeros and
053100130731             sav_VC1ric = *blanks;
053200130730           wSQL += 'WHERE ACRrsr like(''';
053300131210          // wSQL += sav_VC1rag;
053400131210           wSQL += wRag60;
053500130730           wSQL  = %trim(wSQL) + '%'') and +
053600130730                  substr(digits(ACRcro), 1, 3) = ''' +
053700130730                  %editc(sav_VC1fil:'X') + '''';
053800130730         ENDIF;
053900130731
054000130731         IF  sav_VC1rag <> *blanks and sav_VC1fil <> *zeros and
054100130731             sav_VC1ric = 'S';
054200130731           wSQL += 'WHERE ACRrsr like(''%';
054300131210           // wSQL += %trim(sav_VC1rag);
054400131210           wSQL += %trim(wRag60);
054500130731           wSQL  = %trim(wSQL) + '%'') and +
054600130731                  substr(digits(ACRcro), 1, 3) = ''' +
054700130731                  %editc(sav_VC1fil:'X') + '''';
054800130731         ENDIF;
054900130716
055000130731         IF  sav_VC1rag <> *blanks and sav_VC1fil = *zeros and
055100130731             sav_VC1ric = *blanks;
055200130730           wSQL += 'WHERE ACRrsr like(''';
055300131210           // wSQL += sav_VC1rag;
055400131210           wSQL += wRag60;
055500130730           wSQL  = %trim(wSQL) + '%'')';
055600130730         ENDIF;
055700130731
055800130731         IF  sav_VC1rag <> *blanks and sav_VC1fil = *zeros and
055900130731             sav_VC1ric = 'S';
056000130731           wSQL += 'WHERE ACRrsr like(''%';
056100131210           // wSQL += %trim(sav_VC1rag);
056200131210           wSQL += %trim(wRag60);
056300130731           wSQL  = %trim(wSQL) + '%'')';
056400130731         ENDIF;
056500130730
056600130730         IF  sav_VC1fil <> *zeros and sav_VC1rag = *blanks;
056700130730           wSQL += 'WHERE substr(digits(ACRcro), 1, 3) = ''' +
056800130730                    %editc(sav_VC1fil:'X') + '''';
056900130730         ENDIF;
057000131003
057100131003         IF  sav_VC1prv <> *blanks;
057200131003           IF  %scan('WHERE':wSQL) > 0;
057300131003             wSQL += ' and ACRprr = ''' + VC1prv + '''';
057400131003           ELSE;
057500131003             wSQL += 'WHERE ACRprr = ''' + VC1prv + '''';
057600131003           ENDIF;
057700131003         ENDIF;
057800130716
057900131003         IF  I81mai <> 'S' and sav_VC1rag = *blanks and sav_VC1fil = *zeros
058000131003             and sav_VC1prv = *blanks;
058100130730           wSQL += ' WHERE ACRtcr <> ''M''';
058200130716         ENDIF;
058300130730
058400131003         IF  I81mai <> 'S' and (sav_VC1rag <> *blanks or sav_VC1fil <> *zeros
058500131003             or sav_VC1prv <> *blanks);
058600130730           wSQL += ' and ACRtcr <> ''M''';
058700130730         ENDIF;
058800130716
058900130716         wSQL += ' ORDER BY ACRrsr, ACRcro';
059000130716
059100130716       ENDSR;
059200130729
059300130729       //--------------------------------------------------------------
059400130729       //?Apertura cursore.                                            ?
059500130729       //--------------------------------------------------------------
059600130729       BEGSR  OpenCursor;
059700130729
059800130729         wEnd = *off;
059900130729
060000130729       //?Dichiarazione cursore?
060100130729         exec sql
060200130729         PREPARE S1 from :wSQL;
060300130729         exec sql
060400130729         DECLARE ACR  cursor for S1;
060500130729
060600130729       //?Apertura del cursore
060700130729         exec sql
060800130729         OPEN ACR;
060900130729
061000130729         IF sqlcode < 0;
061100130729           wEnd = *on;
061200130729         ENDIF;
061300130729
061400130729       ENDSR;
061500130729
061600130729       //--------------------------------------------------------------
061700130729       //?Lettura cursore.                                             ?
061800130729       //--------------------------------------------------------------
061900130729       BEGSR  ReadCursor;
062000130729
062100130729         clear  FNACRds;
062200130729
062300130729         exec sql
062400130729         FETCH next from ACR into :FNACRds;
062500130729
062600130729         IF sqlcod = 100 or sqlcod < 0;
062700130729           wEnd = *on;
062800130729         ENDIF;
062900130729
063000130729       ENDSR;
063100130729
063200130729       //--------------------------------------------------------------
063300130729       //?Caricamento singola pagina nel subfile S01                   ?
063400130729       //--------------------------------------------------------------
063500130729       BEGSR  RollUpS01;
063600130729
063700130731         IF  dsp_mod = 0;
063800130731           S01nrr    = (wSflPag  * c_SflPag);
063900130801           IndDspF.SflNxtChg = *off;
064000130801           wNrr = S01nrr;
064100130731         ELSE;
064200130731           S11nrr    = (wSflPag  * c_SflPag);
064300130801           wNrr = S11nrr;
064400130801           IndDspF1.SflNxtChg = *off;
064500130731         ENDIF;
064600130801
064700130729         wSflPag  += 1;
064800130729
064900130729       //?Ciclo di caricamento dati da FNACR00F?
065000130731         DOW  wNrr    < (wSflPag  * c_SflPag)   and
065100130731              wNrr    < c_MaxRec                and
065200130729              not wEnD;
065300130729
065400130731           IF  dsp_mod = 0;
065500130731             clear  OR81S01;
065600130731           ELSE;
065700130731             clear  OR81S11;
065800130731           ENDIF;
065900130729
066000130729       //?Caricamento del singolo record nel subfile S01?
066100130729           clear VS1opz;
066200130729           IF  ACRtcr = 'F' or ACRtcr = 'M';
066300130729             VS1tcr = ACRtcr;
066400130729           ELSE;
066500130729             clear VS1tcr;
066600130729           ENDIF;
066700130729           VS1cd1 = %dec(%subst(%editc(ACRcro:'X'):1:3):3:0);
066800130729           VS1cd2 = %dec(%subst(%editc(ACRcro:'X'):4:4):4:0);
066900130729           VS1cd3 = %dec(%subst(%editc(ACRcro:'X'):8:3):3:0);
067000130729           VS1cro = ACRcro;
067100130729           VS1rag = ACRrsr;
067200130729           VS1ind = ACRinr;
067300130729           VS1loc = ACRlor;
067400130729           VS1prv = ACRprr;
067500130729           VS1naz = ACRnar;
067600130729           VS1atb = ACRatb;
067700130729
067800130729       //?Scrittura record nel subfile
067900130731           IF  dsp_mod = 0;
068000130731             S01nrr += 1;
068100130801             wNrr = S01nrr;
068200130731             write  OR81S01;
068300130731           ELSE;
068400130731             S11nrr += 1;
068500130801             wNrr = S11nrr;
068600130731             write  OR81S11;
068700130731           ENDIF;
068800130729
068900130729       //?Leggo record successivo
069000130729           exsr  ReadCursor;
069100130729
069200130729         ENDDO;
069300130731
069400130731       //?Numero Record subfile
069500130801       //?Visualizzazione del SFL (se ci sono dati)?
069600130801       //?Attivazione del SFLEND?
069700130731         IF  dsp_mod = 0;
069800130731           wNrr = S01nrr;
069900130801           IndDspF.SflDsp = (wNrr <= *zero);
070000130801           IndDspF.SflEnd = wNrr >= c_MaxRec  or  wEnD;
070100130731         ELSE;
070200130731           wNrr = S11nrr;
070300130801           IndDspF1.SflDsp = (wNrr <= *zero);
070400130801           IndDspF1.SflEnd = wNrr >= c_MaxRec  or  wEnD;
070500130731         ENDIF;
070600130729
070700130729       //?Posizionamento cursore al 1° rcd della pagina?
070800130731         IF  wNrr > *zero;
070900130731           wPag     = wNrr / c_SflPag;
071000130731           wRig     = wNrr - (c_SflPag * wPag);
071100130729           VC1rcd = wPag * c_SflPag;
071200130729           IF  wRig > *zero;
071300130729             VC1rcd += 1;
071400130729           ELSE;
071500130729             VC1rcd = VC1rcd - c_SflPag + 1;
071600130729           ENDIF;
071700130729         ELSE;
071800130729           clear  VC1rcd;
071900130729         ENDIF;
072000130729
072100130729         VC1csr = VC1rcd;
072200130729
072300130729       ENDSR;
072400130716
072500130716       //--------------------------------------------------------------
072600130716       //?Controllo la filiale inserita nel control.
072700130716       //--------------------------------------------------------------
072800130716       BEGSR ctrFIL;
072900130716
073000130731         IF  VC1fil > *zeros;
073100130731           chain VC1fil AZORG01L;
073200130731           IF  not %found(AZORG01L) or ORGfva <> *blanks or
073300130731              (ORGfag <> 'F' and ORGfag <> 'A');
073400130801             IF  dsp_mod = 0;
073500130801               IndDspf.ErrMessage  = *on;
073600130801               IndDspF.ErrGenerico = *on;
073700130801               IndDspF.PosCurFIL   = *on;
073800130801             ELSE;
073900130801               IndDspF1.ErrMessage  = *on;
074000130801               IndDspF1.ErrGenerico = *on;
074100130801               IndDspF1.PosCurFIL   = *on;
074200130801             ENDIF;
074300130731             VC1msg = skMSG(01);
074400130731           ENDIF;
074500130716         ENDIF;
074600130716
074700130716       ENDSR;
074800130731
074900130731       //--------------------------------------------------------------
075000130731       //?Controllo la ricerca per contenuto.
075100130731       //--------------------------------------------------------------
075200130731       BEGSR ctrRIC;
075300130731
075400130731         IF  VC1ric <> *blanks and VC1rag = *blanks;
075500130801           IF  dsp_mod = 0;
075600130801             IndDspF.ErrMessage  = *on;
075700130801             IndDspF.ErrGenerico = *on;
075800130801             IndDspF.PosCurRAG   = *on;
075900130801           ELSE;
076000130801             IndDspF1.ErrMessage  = *on;
076100130801             IndDspF1.ErrGenerico = *on;
076200130801             IndDspF1.PosCurRAG   = *on;
076300130801           ENDIF;
076400130731           VC1msg = skMSG(03);
076500130731         ENDIF;
076600130731
076700130731       ENDSR;
076800130716
076900130716       //--------------------------------------------------------------
077000130716       //?Gestione tasto funzionale F12.
077100130716       //?F12=Ritorno.
077200130716       //--------------------------------------------------------------
077300130716       BEGSR F12S01;
077400130716
077500130716         O81ret = '1';
077600130730             exsr CloseCursor;
077700130716
077800130716       //?Chiusura del programma
077900130716         wFine = *on;
078000130716
078100130716       ENDSR;
078200130715
078300130715       //--------------------------------------------------------------
078400130716       //?Controllo videata S01.
078500130715       //--------------------------------------------------------------
078600130716       BEGSR CtrS01;
078700130729
078800130729         clear  sav_S01csr;
078900130715
079000130715         WindDspF = IndDspF;
079100130715         reset WindDspFb;
079200130715         IndDspF  = WindDspF;
079300130715
079400130715       //?Controllo cosa è stato inserito nel subfile
079500130731         IF  dsp_mod = 0;
079600130731           readc OR81S01;
079700130731         ELSE;
079800130731           readc OR81S11;
079900130731         ENDIF;
080000130715
080100130731         DOW  not %eof;
080200130716           %subst(IndDspF : 50) = *off;
080300130731           IF  dsp_mod = 0;
080400130801             IndDspF.SflNxtChg = *off;
080500130731             VC1rcd = S01nrr;
080600130731           ELSE;
080700130801             IndDspF1.SflNxtChg = *off;
080800130731             VC1rcd = S11nrr;
080900130731           ENDIF;
081000130715
081100130715       //?Controllo se opzione valida
081200130715           SELECT;
081300130715             //?Nessuna opzione
081400130716             WHEN  VS1opz  = *blank;
081500130715             //?Opzione '1' di scelta
081600130716             WHEN  VS1opz = '1';
081700130716               O81cro = VS1cro;
081800130716              //?Chiusura del programma
081900130716               wFine = *on;
082000130731             //?Opzione '5' di visualizzazione
082100130731             WHEN  VS1opz = '5';
082200130731               exsr Visualizza;
082300130715             //?Altra opzione errore
082400130715           OTHER;
082500130801             IF  dsp_mod = 0;
082600130801               IndDspF.ErrMessage  = *on;
082700130801               IndDspF.ErrGenerico = *on;
082800130801               IndDspF.PosCurOPZ   = *on;
082900130801             ELSE;
083000130801               IndDspF1.ErrMessage  = *on;
083100130801               IndDspF1.ErrGenerico = *on;
083200130801               IndDspF1.PosCurOPZ   = *on;
083300130801             ENDIF;
083400130716               VC1msg = skMSG(02);
083500130715           ENDSL;
083600130729
083700130729       //?Memorizzazione nrr del sfl con la 1ª opzione per?
083800130729       //?riposizionarvici il cursore dopo il tasto "Invio"?
083900130729           IF  VS1opz  <> *blank   and
084000130729              (sav_S01csr = *zeros  or  sav_S01csr < VC1rcd);
084100130729             sav_S01csr = VC1rcd;
084200130729           ENDIF;
084300130715
084400130715       //?Aggiorno il subfile
084500130715           SELECT;
084600130801             WHEN  dsp_mod = 0 and IndDspF.ErrMessage;
084700130801               IndDspF.SflNxtChg = *on;
084800130801               VC1csr    = VC1rcd;
084900130801
085000130801             WHEN  dsp_mod = 1 and IndDspF1.ErrMessage;
085100130801               IndDspF1.SflNxtChg = *on;
085200130716               VC1csr    = VC1rcd;
085300130801
085400130801             WHEN  dsp_mod = 0 and IndDspF.ErrGenerico;
085500130716               VC1csr = VC1rcd;
085600130716               clear  VS1opz;
085700130801
085800130801             WHEN  dsp_mod = 1 and IndDspF1.ErrGenerico;
085900130801               VC1csr = VC1rcd;
086000130801               clear  VS1opz;
086100130715             OTHER;
086200130729               clear  VS1opz;
086300130715           ENDSL;
086400130715
086500130731           IF  dsp_mod = 0;
086600130731             UPDATE OR81S01;
086700130731           ELSE;
086800130731             UPDATE OR81S11;
086900130731           ENDIF;
087000130715
087100130715       //?Al primo errore esco dalla lettura del subfile
087200130801           IF  dsp_mod = 0 and
087300130801              (IndDspF.ErrMessage or IndDspF.ErrGenerico);
087400130715             leave;
087500130801           ENDIF;
087600130801           IF  dsp_mod = 1 and
087700130801              (IndDspF1.ErrMessage or IndDspF1.ErrGenerico);
087800130801             leave;
087900130801           ENDIF;
088000130715
088100130731           IF  dsp_mod = 0;
088200130731             readc OR81S01;
088300130731           ELSE;
088400130731             readc OR81S11;
088500130731           ENDIF;
088600130715
088700130715         ENDDO;
088800130729
088900130729       //?Riposiziono il cursore sul record contenente la prima opz.?
089000130729       //?(se non sono stati rilevati errori)?
089100130801         IF  dsp_mod = 0;
089200130801           IF  not IndDspF.ErrMessage and not IndDspF.ErrGenerico   and
089300130801               sav_S01csr > *zero;
089400130801             VC1csr = sav_S01csr;
089500130801           ENDIF;
089600130801         ELSE;
089700130801           IF  not IndDspF1.ErrMessage and not IndDspF1.ErrGenerico   and
089800130801               sav_S01csr > *zero;
089900130801             VC1csr = sav_S01csr;
090000130801           ENDIF;
090100130729         ENDIF;
090200130715
090300130715       ENDSR;
090400130731
090500130731       //--------------------------------------------------------------
090600130731       //?Visualizza anagrafica.
090700130731       //--------------------------------------------------------------
090800130731       BEGSR Visualizza;
090900130731
091000130731       //?Richiamo PGM
091100130731         clear FIOR37DS;
091200130731         I37opz = '5';
091300130731         I37cro = VS1cro;
091400130731         kpjbu = FIOR37DS;
091500130731         fior37r1 (kpjba);
091600130731
091700130731       ENDSR;
091800130730
091900130730       //--------------------------------------------------------------
092000130730       //?Chiusura cursore.
092100130730       //--------------------------------------------------------------
092200130730       BEGSR CloseCursor;
092300130730
092400130730       //?Chiusura cursore SQL?
092500130730         exec sql
092600130730           close ACR;
092700130730
092800130730       ENDSR;
092900130715
093000130715       //--------------------------------------------------------------
093100130715       //?Operazioni finali.
093200130715       //--------------------------------------------------------------
093300130715       BEGSR RoutEnd;
093400130715
093500130729       //?Chiusura pgm?
093600130715         *inLR = *on;
093700130715         return;
093800130715
093900130715       ENDSR;
094000130715
094100130715      /end-free
094200130715
094300130715       //--------------------------------------------------------------
094400130715       //?Schiere a tempo di compilazione.
094500130715       //--------------------------------------------------------------
094600130715
094700130715** - skMSG ------------------------------------------------------------------*
094800130715Filiale errata                                                                 01
094900130715Opzione errata                                                                 02
095000130731Ricerca possibile solo se inserita Ragione Sociale                             03
