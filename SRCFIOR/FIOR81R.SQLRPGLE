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
026400150907
026500150907         IF  I81rag <> *blanks;
026600150907           VC1ric = 'S';
026700150907           sav_VC1ric = VC1ric;
026800150907         ENDIF;
026900130715
027000130715       ENDSR;
027100130715
027200130715       //--------------------------------------------------------------
027300130715       //?Reperimento Dati del job (Utente/Operativi).                 ?
027400130715       //--------------------------------------------------------------
027500130715       BEGSR  DatiJob;
027600130715
027700130715         in(E) §AzUte;
027800130715         IF  NOT %error;
027900130715           in(E) §DatiUte;
028000130715         ENDIF;
028100130715         IF  %error or RSut = *blanks;
028200130715           clear TIBS34DS;
028300130715           tibs34r ( TIBS34DS );
028400130715           in §AzUte;
028500130715           in §DatiUte;
028600130715         ENDIF;
028700130715
028800130715       ENDSR;
028900130715
029000130715       //--------------------------------------------------------------
029100130716       //?Gestione videata S01.
029200130715       //--------------------------------------------------------------
029300130716       BEGSR  GesS01;
029400130715
029500130715       //?Inizializzazione videata
029600130716         IF  wInzS01;
029700130716           exsr InzS01;
029800130716           wInzS01 = *off;
029900130715         ENDIF;
030000130729
030100130729       //?Emissione Testata e Piede con tasti funzionali abilitati?
030200130731         IF  dsp_mod = 0;
030300130731           write OR81T01;
030400130731           write OR81Z01;
030500130731         ELSE;
030600130731           write OR81T11;
030700130731           write OR81Z11;
030800130731         ENDIF;
030900130729
031000130729       //?Posizionamento cursore?
031100130729         IF  VC1csr > *zero;
031200130729           VC1rcd = VC1csr;
031300130729         ELSE;
031400130729           VC1rcd = 1;
031500130731           IF  dsp_mod = 0;
031600130731             write  OR81S00;
031700130731           ELSE;
031800130731             write  OR81S10;
031900130731           ENDIF;
032000130729         ENDIF;
032100130715
032200130715       //?Emissione videata
032300130731         IF  dsp_mod = 0;
032400130731           exfmt OR81C01;
032500130801           IndDspF.ErrMessage  = *off;
032600130801           IndDspF.ErrGenerico = *off;
032700130731         ELSE;
032800130731           exfmt OR81C11;
032900130801           IndDspF1.ErrMessage  = *off;
033000130801           IndDspF1.ErrGenerico = *off;
033100130731         ENDIF;
033200130729
033300130716         clear VC1msg;
033400130715
033500130715         SELECT;
033600131030
033700131030       //?Provincia
033800131030           WHEN  VC1prv <> sav_VC1prv;
033900131030             sav_VC1fil = VC1fil;
034000131030             sav_VC1rag = VC1rag;
034100131030             sav_VC1ric = VC1ric;
034200131030             sav_VC1prv = VC1prv;
034300131030             wInzS01 = *on;
034400131030             exsr CloseCursor;
034500131030             leavesr;
034600130716
034700130716       //?Controllo la filiale
034800130716           WHEN  VC1fil <> sav_VC1fil;
034900130716             exsr CTRfil;
035000130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
035100130731               leavesr;
035200130731             ENDIF;
035300130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
035400130801               leavesr;
035500130801             ENDIF;
035600130716             sav_VC1fil = VC1fil;
035700130716             sav_VC1rag = VC1rag;
035800130731             sav_VC1ric = VC1ric;
035900131003             sav_VC1prv = VC1prv;
036000130716             wInzS01 = *on;
036100130730             exsr CloseCursor;
036200130716             leavesr;
036300130716
036400130716       //?Ragione sociale
036500130716           WHEN  VC1rag <> sav_VC1rag;
036600130716             sav_VC1rag = VC1rag;
036700130716             sav_VC1fil = VC1fil;
036800130731             sav_VC1ric = VC1ric;
036900131003             sav_VC1prv = VC1prv;
037000130716             wInzS01 = *on;
037100130730             exsr CloseCursor;
037200130716             leavesr;
037300130731
037400130731       //?Ricerca per contenuto
037500130731           WHEN  VC1ric <> sav_VC1ric;
037600130731             exsr CTRric;
037700130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
037800130731               leavesr;
037900130731             ENDIF;
038000130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
038100130801               leavesr;
038200130801             ENDIF;
038300130731             sav_VC1rag = VC1rag;
038400130731             sav_VC1fil = VC1fil;
038500130731             sav_VC1ric = VC1ric;
038600131003             sav_VC1prv = VC1prv;
038700130731             wInzS01 = *on;
038800130731             exsr CloseCursor;
038900130731             leavesr;
039000130715
039100130716       //?F12=Ritorno
039200130801           WHEN  dsp_mod = 0 and InfDspF.dsp_aid = c_F12;
039300130716             exsr F12S01;
039400130801           WHEN  dsp_mod = 1 and InfDspF1.dsp_aid = c_F12;
039500130801             exsr F12S01;
039600130729
039700130729       //?Roll-Up?
039800130801           WHEN  dsp_mod = 0 and InfDspF.dsp_aid = c_RollUp;
039900130729             exsr RollUpS01;
040000130801           WHEN  dsp_mod = 1 and InfDspF1.dsp_aid = c_RollUp;
040100130801             exsr RollUpS01;
040200130729
040300130729       //?SubFile vuoto?
040400130731           WHEN  S01nrr = *zeros and dsp_mod = 0;
040500130731
040600130731       //?SubFile vuoto?
040700130731           WHEN  S11nrr = *zeros and dsp_mod <> 0;
040800130716
040900130715       //?Enter
041000130715           OTHER;
041100130716
041200130715         //?Eseguo i controlli
041300130716             exsr CtrS01;
041400130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
041500130715               leavesr;
041600130715             ENDIF;
041700130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
041800130801               leavesr;
041900130801             ENDIF;
042000130715
042100130715         ENDSL;
042200130715
042300130715       ENDSR;
042400130715
042500130715       //--------------------------------------------------------------
042600130716       //?Inizializzazione videata S01.
042700130715       //--------------------------------------------------------------
042800130716       BEGSR InzS01;
042900130729
043000130729       //?Spegnimento degli indicatori di errore?
043100130801         IF  dsp_mod = 0;
043200130801           %subst(IndDspF : 50) = *off;
043300130801         ELSE;
043400130801           %subst(IndDspF1 : 50) = *off;
043500130801         ENDIF;
043600130715
043700130715       //?Pulizia del Subfile
043800130716         exsr PulS01;
043900130715
044000130729       //?Preparo la stringa SQL
044100130716         exsr PrepSQL;
044200130716
044300130729       //?carico la prima pagina delle anagrafiche
044400130729         exsr OpenCursor;
044500130729         exsr ReadCursor;
044600130729         exsr RollUpS01;
044700130731
044800130731         IF  dsp_mod = 0;
044900130731           wNrr = S01nrr;
045000130731         ELSE;
045100130731           wNrr = S11nrr;
045200130731         ENDIF;
045300130731
045400130729       //?Impaginazione subfile?
045500130729       //?-> forza l'impaginazione sul 1° rec. del subfile
045600130731         IF  wNrr > *zeros;
045700130731           VC1rcd  = 1;
045800130731           VC1csr  = 1;
045900130731         ELSE;
046000130731           clear VC1rcd;
046100130731         ENDIF;
046200130930
046300130930       //?Se non ho ricevuto nessun dato di ricerca
046400130930       //?mi posiziono sulla ragione sociale
046500130930         IF  I81fil = 0 and I81rag = *blanks;
046600130930           IndDspF1.PosCurRAG = *on;
046700131030           IndDspF1.PosCurPRV = *off;
046800130930         ELSE;
046900131030       //?altrimenti mi posiziono sulla provincia
047000130930           IndDspF1.PosCurRAG = *off;
047100131030           IndDspF1.PosCurPRV = *on;
047200130930         ENDIF;
047300130715
047400130715       ENDSR;
047500130715
047600130715       //--------------------------------------------------------------
047700130716       //?Pulizia Subfile S01.
047800130715       //--------------------------------------------------------------
047900130716       BEGSR PulS01;
048000130715
048100130731         IF  dsp_mod = 0;
048200130801           IndDspF.SflDsp    = *on;
048300130801           IndDspF.SflDspCtl = *on;
048400130731           write  OR81C01;
048500130801           IndDspF.SflDspCtl = *off;
048600130801           IndDspF.SflEnd    = *off;
048700130731         ELSE;
048800130801           IndDspF1.SflDsp    = *on;
048900130801           IndDspF1.SflDspCtl = *on;
049000130731           write  OR81C11;
049100130801           IndDspF1.SflDspCtl = *off;
049200130801           IndDspF1.SflEnd    = *off;
049300130801         ENDIF;
049400130715
049500130729         clear wSflPag;
049600130716         clear VC1rcd;
049700130716         clear VC1csr;
049800130716         clear VC1msg;
049900130801
050000130801         IF  dsp_mod = 0;
050100130801           clear S01nrr;
050200130801           IndDspF.ErrMessage  = *off;
050300130801           IndDspF.ErrGenerico = *off;
050400130801         ELSE;
050500130801           clear S11nrr;
050600130801           IndDspF1.ErrMessage  = *off;
050700130801           IndDspF1.ErrGenerico = *off;
050800130801         ENDIF;
050900130715
051000130715         WindDspF = IndDspF;
051100130715         reset WindDspFb;
051200130715         IndDspF  = WindDspF;
051300130715
051400130715       ENDSR;
051500130716
051600130716       //--------------------------------------------------------------
051700130716       //?Preparo la Stringa SQL.
051800130716       //--------------------------------------------------------------
051900130716       BEGSR PrepSQL;
052000131210
052100131210         IF  sav_VC1rag <> *blanks;
052200131210           wRag60 = sav_VC1rag;
052300131210           yy = 1;
052400131210           DOW  yy <= 60;
052500131210             IF  %subst(wRag60:yy:1) = '''';
052600131210               wRag60 = %replace('''''':wRag60:yy:1);
052700131210               yy += 1;
052800131210             ENDIF;
052900131210             yy += 1;
053000131210           ENDDO;
053100131210         ENDIF;
053200130716
053300130730         wSQL = 'SELECT * from FNACR00F ';
053400130716
053500130731         IF  sav_VC1rag <> *blanks and sav_VC1fil <> *zeros and
053600130731             sav_VC1ric = *blanks;
053700130730           wSQL += 'WHERE ACRrsr like(''';
053800131210          // wSQL += sav_VC1rag;
053900131210           wSQL += wRag60;
054000130730           wSQL  = %trim(wSQL) + '%'') and +
054100130730                  substr(digits(ACRcro), 1, 3) = ''' +
054200130730                  %editc(sav_VC1fil:'X') + '''';
054300130730         ENDIF;
054400130731
054500130731         IF  sav_VC1rag <> *blanks and sav_VC1fil <> *zeros and
054600130731             sav_VC1ric = 'S';
054700130731           wSQL += 'WHERE ACRrsr like(''%';
054800131210           // wSQL += %trim(sav_VC1rag);
054900131210           wSQL += %trim(wRag60);
055000130731           wSQL  = %trim(wSQL) + '%'') and +
055100130731                  substr(digits(ACRcro), 1, 3) = ''' +
055200130731                  %editc(sav_VC1fil:'X') + '''';
055300130731         ENDIF;
055400130716
055500130731         IF  sav_VC1rag <> *blanks and sav_VC1fil = *zeros and
055600130731             sav_VC1ric = *blanks;
055700130730           wSQL += 'WHERE ACRrsr like(''';
055800131210           // wSQL += sav_VC1rag;
055900131210           wSQL += wRag60;
056000130730           wSQL  = %trim(wSQL) + '%'')';
056100130730         ENDIF;
056200130731
056300130731         IF  sav_VC1rag <> *blanks and sav_VC1fil = *zeros and
056400130731             sav_VC1ric = 'S';
056500130731           wSQL += 'WHERE ACRrsr like(''%';
056600131210           // wSQL += %trim(sav_VC1rag);
056700131210           wSQL += %trim(wRag60);
056800130731           wSQL  = %trim(wSQL) + '%'')';
056900130731         ENDIF;
057000130730
057100130730         IF  sav_VC1fil <> *zeros and sav_VC1rag = *blanks;
057200130730           wSQL += 'WHERE substr(digits(ACRcro), 1, 3) = ''' +
057300130730                    %editc(sav_VC1fil:'X') + '''';
057400130730         ENDIF;
057500131003
057600131003         IF  sav_VC1prv <> *blanks;
057700131003           IF  %scan('WHERE':wSQL) > 0;
057800131003             wSQL += ' and ACRprr = ''' + VC1prv + '''';
057900131003           ELSE;
058000131003             wSQL += 'WHERE ACRprr = ''' + VC1prv + '''';
058100131003           ENDIF;
058200131003         ENDIF;
058300130716
058400131003         IF  I81mai <> 'S' and sav_VC1rag = *blanks and sav_VC1fil = *zeros
058500131003             and sav_VC1prv = *blanks;
058600130730           wSQL += ' WHERE ACRtcr <> ''M''';
058700130716         ENDIF;
058800130730
058900131003         IF  I81mai <> 'S' and (sav_VC1rag <> *blanks or sav_VC1fil <> *zeros
059000131003             or sav_VC1prv <> *blanks);
059100130730           wSQL += ' and ACRtcr <> ''M''';
059200130730         ENDIF;
059300130716
059400130716         wSQL += ' ORDER BY ACRrsr, ACRcro';
059500130716
059600130716       ENDSR;
059700130729
059800130729       //--------------------------------------------------------------
059900130729       //?Apertura cursore.                                            ?
060000130729       //--------------------------------------------------------------
060100130729       BEGSR  OpenCursor;
060200130729
060300130729         wEnd = *off;
060400130729
060500130729       //?Dichiarazione cursore?
060600130729         exec sql
060700130729         PREPARE S1 from :wSQL;
060800130729         exec sql
060900130729         DECLARE ACR  cursor for S1;
061000130729
061100130729       //?Apertura del cursore
061200130729         exec sql
061300130729         OPEN ACR;
061400130729
061500130729         IF sqlcode < 0;
061600130729           wEnd = *on;
061700130729         ENDIF;
061800130729
061900130729       ENDSR;
062000130729
062100130729       //--------------------------------------------------------------
062200130729       //?Lettura cursore.                                             ?
062300130729       //--------------------------------------------------------------
062400130729       BEGSR  ReadCursor;
062500130729
062600130729         clear  FNACRds;
062700130729
062800130729         exec sql
062900130729         FETCH next from ACR into :FNACRds;
063000130729
063100130729         IF sqlcod = 100 or sqlcod < 0;
063200130729           wEnd = *on;
063300130729         ENDIF;
063400130729
063500130729       ENDSR;
063600130729
063700130729       //--------------------------------------------------------------
063800130729       //?Caricamento singola pagina nel subfile S01                   ?
063900130729       //--------------------------------------------------------------
064000130729       BEGSR  RollUpS01;
064100130729
064200130731         IF  dsp_mod = 0;
064300130731           S01nrr    = (wSflPag  * c_SflPag);
064400130801           IndDspF.SflNxtChg = *off;
064500130801           wNrr = S01nrr;
064600130731         ELSE;
064700130731           S11nrr    = (wSflPag  * c_SflPag);
064800130801           wNrr = S11nrr;
064900130801           IndDspF1.SflNxtChg = *off;
065000130731         ENDIF;
065100130801
065200130729         wSflPag  += 1;
065300130729
065400130729       //?Ciclo di caricamento dati da FNACR00F?
065500130731         DOW  wNrr    < (wSflPag  * c_SflPag)   and
065600130731              wNrr    < c_MaxRec                and
065700130729              not wEnD;
065800130729
065900130731           IF  dsp_mod = 0;
066000130731             clear  OR81S01;
066100130731           ELSE;
066200130731             clear  OR81S11;
066300130731           ENDIF;
066400130729
066500130729       //?Caricamento del singolo record nel subfile S01?
066600130729           clear VS1opz;
066700130729           IF  ACRtcr = 'F' or ACRtcr = 'M';
066800130729             VS1tcr = ACRtcr;
066900130729           ELSE;
067000130729             clear VS1tcr;
067100130729           ENDIF;
067200130729           VS1cd1 = %dec(%subst(%editc(ACRcro:'X'):1:3):3:0);
067300130729           VS1cd2 = %dec(%subst(%editc(ACRcro:'X'):4:4):4:0);
067400130729           VS1cd3 = %dec(%subst(%editc(ACRcro:'X'):8:3):3:0);
067500130729           VS1cro = ACRcro;
067600130729           VS1rag = ACRrsr;
067700130729           VS1ind = ACRinr;
067800130729           VS1loc = ACRlor;
067900130729           VS1prv = ACRprr;
068000130729           VS1naz = ACRnar;
068100130729           VS1atb = ACRatb;
068200130729
068300130729       //?Scrittura record nel subfile
068400130731           IF  dsp_mod = 0;
068500130731             S01nrr += 1;
068600130801             wNrr = S01nrr;
068700130731             write  OR81S01;
068800130731           ELSE;
068900130731             S11nrr += 1;
069000130801             wNrr = S11nrr;
069100130731             write  OR81S11;
069200130731           ENDIF;
069300130729
069400130729       //?Leggo record successivo
069500130729           exsr  ReadCursor;
069600130729
069700130729         ENDDO;
069800130731
069900130731       //?Numero Record subfile
070000130801       //?Visualizzazione del SFL (se ci sono dati)?
070100130801       //?Attivazione del SFLEND?
070200130731         IF  dsp_mod = 0;
070300130731           wNrr = S01nrr;
070400130801           IndDspF.SflDsp = (wNrr <= *zero);
070500130801           IndDspF.SflEnd = wNrr >= c_MaxRec  or  wEnD;
070600130731         ELSE;
070700130731           wNrr = S11nrr;
070800130801           IndDspF1.SflDsp = (wNrr <= *zero);
070900130801           IndDspF1.SflEnd = wNrr >= c_MaxRec  or  wEnD;
071000130731         ENDIF;
071100130729
071200130729       //?Posizionamento cursore al 1° rcd della pagina?
071300130731         IF  wNrr > *zero;
071400130731           wPag     = wNrr / c_SflPag;
071500130731           wRig     = wNrr - (c_SflPag * wPag);
071600130729           VC1rcd = wPag * c_SflPag;
071700130729           IF  wRig > *zero;
071800130729             VC1rcd += 1;
071900130729           ELSE;
072000130729             VC1rcd = VC1rcd - c_SflPag + 1;
072100130729           ENDIF;
072200130729         ELSE;
072300130729           clear  VC1rcd;
072400130729         ENDIF;
072500130729
072600130729         VC1csr = VC1rcd;
072700130729
072800130729       ENDSR;
072900130716
073000130716       //--------------------------------------------------------------
073100130716       //?Controllo la filiale inserita nel control.
073200130716       //--------------------------------------------------------------
073300130716       BEGSR ctrFIL;
073400130716
073500130731         IF  VC1fil > *zeros;
073600130731           chain VC1fil AZORG01L;
073700130731           IF  not %found(AZORG01L) or ORGfva <> *blanks or
073800130731              (ORGfag <> 'F' and ORGfag <> 'A');
073900130801             IF  dsp_mod = 0;
074000130801               IndDspf.ErrMessage  = *on;
074100130801               IndDspF.ErrGenerico = *on;
074200130801               IndDspF.PosCurFIL   = *on;
074300130801             ELSE;
074400130801               IndDspF1.ErrMessage  = *on;
074500130801               IndDspF1.ErrGenerico = *on;
074600130801               IndDspF1.PosCurFIL   = *on;
074700130801             ENDIF;
074800130731             VC1msg = skMSG(01);
074900130731           ENDIF;
075000130716         ENDIF;
075100130716
075200130716       ENDSR;
075300130731
075400130731       //--------------------------------------------------------------
075500130731       //?Controllo la ricerca per contenuto.
075600130731       //--------------------------------------------------------------
075700130731       BEGSR ctrRIC;
075800130731
075900130731         IF  VC1ric <> *blanks and VC1rag = *blanks;
076000130801           IF  dsp_mod = 0;
076100130801             IndDspF.ErrMessage  = *on;
076200130801             IndDspF.ErrGenerico = *on;
076300130801             IndDspF.PosCurRAG   = *on;
076400130801           ELSE;
076500130801             IndDspF1.ErrMessage  = *on;
076600130801             IndDspF1.ErrGenerico = *on;
076700130801             IndDspF1.PosCurRAG   = *on;
076800130801           ENDIF;
076900130731           VC1msg = skMSG(03);
077000130731         ENDIF;
077100130731
077200130731       ENDSR;
077300130716
077400130716       //--------------------------------------------------------------
077500130716       //?Gestione tasto funzionale F12.
077600130716       //?F12=Ritorno.
077700130716       //--------------------------------------------------------------
077800130716       BEGSR F12S01;
077900130716
078000130716         O81ret = '1';
078100130730             exsr CloseCursor;
078200130716
078300130716       //?Chiusura del programma
078400130716         wFine = *on;
078500130716
078600130716       ENDSR;
078700130715
078800130715       //--------------------------------------------------------------
078900130716       //?Controllo videata S01.
079000130715       //--------------------------------------------------------------
079100130716       BEGSR CtrS01;
079200130729
079300130729         clear  sav_S01csr;
079400130715
079500130715         WindDspF = IndDspF;
079600130715         reset WindDspFb;
079700130715         IndDspF  = WindDspF;
079800130715
079900130715       //?Controllo cosa è stato inserito nel subfile
080000130731         IF  dsp_mod = 0;
080100130731           readc OR81S01;
080200130731         ELSE;
080300130731           readc OR81S11;
080400130731         ENDIF;
080500130715
080600130731         DOW  not %eof;
080700130716           %subst(IndDspF : 50) = *off;
080800130731           IF  dsp_mod = 0;
080900130801             IndDspF.SflNxtChg = *off;
081000130731             VC1rcd = S01nrr;
081100130731           ELSE;
081200130801             IndDspF1.SflNxtChg = *off;
081300130731             VC1rcd = S11nrr;
081400130731           ENDIF;
081500130715
081600130715       //?Controllo se opzione valida
081700130715           SELECT;
081800130715             //?Nessuna opzione
081900130716             WHEN  VS1opz  = *blank;
082000130715             //?Opzione '1' di scelta
082100130716             WHEN  VS1opz = '1';
082200130716               O81cro = VS1cro;
082300130716              //?Chiusura del programma
082400130716               wFine = *on;
082500130731             //?Opzione '5' di visualizzazione
082600130731             WHEN  VS1opz = '5';
082700130731               exsr Visualizza;
082800130715             //?Altra opzione errore
082900130715           OTHER;
083000130801             IF  dsp_mod = 0;
083100130801               IndDspF.ErrMessage  = *on;
083200130801               IndDspF.ErrGenerico = *on;
083300130801               IndDspF.PosCurOPZ   = *on;
083400130801             ELSE;
083500130801               IndDspF1.ErrMessage  = *on;
083600130801               IndDspF1.ErrGenerico = *on;
083700130801               IndDspF1.PosCurOPZ   = *on;
083800130801             ENDIF;
083900130716               VC1msg = skMSG(02);
084000130715           ENDSL;
084100130729
084200130729       //?Memorizzazione nrr del sfl con la 1ª opzione per?
084300130729       //?riposizionarvici il cursore dopo il tasto "Invio"?
084400130729           IF  VS1opz  <> *blank   and
084500130729              (sav_S01csr = *zeros  or  sav_S01csr < VC1rcd);
084600130729             sav_S01csr = VC1rcd;
084700130729           ENDIF;
084800130715
084900130715       //?Aggiorno il subfile
085000130715           SELECT;
085100130801             WHEN  dsp_mod = 0 and IndDspF.ErrMessage;
085200130801               IndDspF.SflNxtChg = *on;
085300130801               VC1csr    = VC1rcd;
085400130801
085500130801             WHEN  dsp_mod = 1 and IndDspF1.ErrMessage;
085600130801               IndDspF1.SflNxtChg = *on;
085700130716               VC1csr    = VC1rcd;
085800130801
085900130801             WHEN  dsp_mod = 0 and IndDspF.ErrGenerico;
086000130716               VC1csr = VC1rcd;
086100130716               clear  VS1opz;
086200130801
086300130801             WHEN  dsp_mod = 1 and IndDspF1.ErrGenerico;
086400130801               VC1csr = VC1rcd;
086500130801               clear  VS1opz;
086600130715             OTHER;
086700130729               clear  VS1opz;
086800130715           ENDSL;
086900130715
087000130731           IF  dsp_mod = 0;
087100130731             UPDATE OR81S01;
087200130731           ELSE;
087300130731             UPDATE OR81S11;
087400130731           ENDIF;
087500130715
087600130715       //?Al primo errore esco dalla lettura del subfile
087700130801           IF  dsp_mod = 0 and
087800130801              (IndDspF.ErrMessage or IndDspF.ErrGenerico);
087900130715             leave;
088000130801           ENDIF;
088100130801           IF  dsp_mod = 1 and
088200130801              (IndDspF1.ErrMessage or IndDspF1.ErrGenerico);
088300130801             leave;
088400130801           ENDIF;
088500130715
088600130731           IF  dsp_mod = 0;
088700130731             readc OR81S01;
088800130731           ELSE;
088900130731             readc OR81S11;
089000130731           ENDIF;
089100130715
089200130715         ENDDO;
089300130729
089400130729       //?Riposiziono il cursore sul record contenente la prima opz.?
089500130729       //?(se non sono stati rilevati errori)?
089600130801         IF  dsp_mod = 0;
089700130801           IF  not IndDspF.ErrMessage and not IndDspF.ErrGenerico   and
089800130801               sav_S01csr > *zero;
089900130801             VC1csr = sav_S01csr;
090000130801           ENDIF;
090100130801         ELSE;
090200130801           IF  not IndDspF1.ErrMessage and not IndDspF1.ErrGenerico   and
090300130801               sav_S01csr > *zero;
090400130801             VC1csr = sav_S01csr;
090500130801           ENDIF;
090600130729         ENDIF;
090700130715
090800130715       ENDSR;
090900130731
091000130731       //--------------------------------------------------------------
091100130731       //?Visualizza anagrafica.
091200130731       //--------------------------------------------------------------
091300130731       BEGSR Visualizza;
091400130731
091500130731       //?Richiamo PGM
091600130731         clear FIOR37DS;
091700130731         I37opz = '5';
091800130731         I37cro = VS1cro;
091900130731         kpjbu = FIOR37DS;
092000130731         fior37r1 (kpjba);
092100130731
092200130731       ENDSR;
092300130730
092400130730       //--------------------------------------------------------------
092500130730       //?Chiusura cursore.
092600130730       //--------------------------------------------------------------
092700130730       BEGSR CloseCursor;
092800130730
092900130730       //?Chiusura cursore SQL?
093000130730         exec sql
093100130730           close ACR;
093200130730
093300130730       ENDSR;
093400130715
093500130715       //--------------------------------------------------------------
093600130715       //?Operazioni finali.
093700130715       //--------------------------------------------------------------
093800130715       BEGSR RoutEnd;
093900130715
094000130729       //?Chiusura pgm?
094100130715         *inLR = *on;
094200130715         return;
094300130715
094400130715       ENDSR;
094500130715
094600130715      /end-free
094700130715
094800130715       //--------------------------------------------------------------
094900130715       //?Schiere a tempo di compilazione.
095000130715       //--------------------------------------------------------------
095100130715
095200130715** - skMSG ------------------------------------------------------------------*
095300130715Filiale errata                                                                 01
095400130715Opzione errata                                                                 02
095500130731Ricerca possibile solo se inserita Ragione Sociale                             03
