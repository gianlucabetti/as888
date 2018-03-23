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
026200130715
026300130715       ENDSR;
026400130715
026500130715       //--------------------------------------------------------------
026600130715       //?Reperimento Dati del job (Utente/Operativi).                 ?
026700130715       //--------------------------------------------------------------
026800130715       BEGSR  DatiJob;
026900130715
027000130715         in(E) §AzUte;
027100130715         IF  NOT %error;
027200130715           in(E) §DatiUte;
027300130715         ENDIF;
027400130715         IF  %error or RSut = *blanks;
027500130715           clear TIBS34DS;
027600130715           tibs34r ( TIBS34DS );
027700130715           in §AzUte;
027800130715           in §DatiUte;
027900130715         ENDIF;
028000130715
028100130715       ENDSR;
028200130715
028300130715       //--------------------------------------------------------------
028400130716       //?Gestione videata S01.
028500130715       //--------------------------------------------------------------
028600130716       BEGSR  GesS01;
028700130715
028800130715       //?Inizializzazione videata
028900130716         IF  wInzS01;
029000130716           exsr InzS01;
029100130716           wInzS01 = *off;
029200130715         ENDIF;
029300130729
029400130729       //?Emissione Testata e Piede con tasti funzionali abilitati?
029500130731         IF  dsp_mod = 0;
029600130731           write OR81T01;
029700130731           write OR81Z01;
029800130731         ELSE;
029900130731           write OR81T11;
030000130731           write OR81Z11;
030100130731         ENDIF;
030200130729
030300130729       //?Posizionamento cursore?
030400130729         IF  VC1csr > *zero;
030500130729           VC1rcd = VC1csr;
030600130729         ELSE;
030700130729           VC1rcd = 1;
030800130731           IF  dsp_mod = 0;
030900130731             write  OR81S00;
031000130731           ELSE;
031100130731             write  OR81S10;
031200130731           ENDIF;
031300130729         ENDIF;
031400130715
031500130715       //?Emissione videata
031600130731         IF  dsp_mod = 0;
031700130731           exfmt OR81C01;
031800130801           IndDspF.ErrMessage  = *off;
031900130801           IndDspF.ErrGenerico = *off;
032000130731         ELSE;
032100130731           exfmt OR81C11;
032200130801           IndDspF1.ErrMessage  = *off;
032300130801           IndDspF1.ErrGenerico = *off;
032400130731         ENDIF;
032500130729
032600130716         clear VC1msg;
032700130715
032800130715         SELECT;
032900131030
033000131030       //?Provincia
033100131030           WHEN  VC1prv <> sav_VC1prv;
033200131030             sav_VC1fil = VC1fil;
033300131030             sav_VC1rag = VC1rag;
033400131030             sav_VC1ric = VC1ric;
033500131030             sav_VC1prv = VC1prv;
033600131030             wInzS01 = *on;
033700131030             exsr CloseCursor;
033800131030             leavesr;
033900130716
034000130716       //?Controllo la filiale
034100130716           WHEN  VC1fil <> sav_VC1fil;
034200130716             exsr CTRfil;
034300130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
034400130731               leavesr;
034500130731             ENDIF;
034600130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
034700130801               leavesr;
034800130801             ENDIF;
034900130716             sav_VC1fil = VC1fil;
035000130716             sav_VC1rag = VC1rag;
035100130731             sav_VC1ric = VC1ric;
035200131003             sav_VC1prv = VC1prv;
035300130716             wInzS01 = *on;
035400130730             exsr CloseCursor;
035500130716             leavesr;
035600130716
035700130716       //?Ragione sociale
035800130716           WHEN  VC1rag <> sav_VC1rag;
035900130716             sav_VC1rag = VC1rag;
036000130716             sav_VC1fil = VC1fil;
036100130731             sav_VC1ric = VC1ric;
036200131003             sav_VC1prv = VC1prv;
036300130716             wInzS01 = *on;
036400130730             exsr CloseCursor;
036500130716             leavesr;
036600130731
036700130731       //?Ricerca per contenuto
036800130731           WHEN  VC1ric <> sav_VC1ric;
036900130731             exsr CTRric;
037000130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
037100130731               leavesr;
037200130731             ENDIF;
037300130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
037400130801               leavesr;
037500130801             ENDIF;
037600130731             sav_VC1rag = VC1rag;
037700130731             sav_VC1fil = VC1fil;
037800130731             sav_VC1ric = VC1ric;
037900131003             sav_VC1prv = VC1prv;
038000130731             wInzS01 = *on;
038100130731             exsr CloseCursor;
038200130731             leavesr;
038300130715
038400130716       //?F12=Ritorno
038500130801           WHEN  dsp_mod = 0 and InfDspF.dsp_aid = c_F12;
038600130716             exsr F12S01;
038700130801           WHEN  dsp_mod = 1 and InfDspF1.dsp_aid = c_F12;
038800130801             exsr F12S01;
038900130729
039000130729       //?Roll-Up?
039100130801           WHEN  dsp_mod = 0 and InfDspF.dsp_aid = c_RollUp;
039200130729             exsr RollUpS01;
039300130801           WHEN  dsp_mod = 1 and InfDspF1.dsp_aid = c_RollUp;
039400130801             exsr RollUpS01;
039500130729
039600130729       //?SubFile vuoto?
039700130731           WHEN  S01nrr = *zeros and dsp_mod = 0;
039800130731
039900130731       //?SubFile vuoto?
040000130731           WHEN  S11nrr = *zeros and dsp_mod <> 0;
040100130716
040200130715       //?Enter
040300130715           OTHER;
040400130716
040500130715         //?Eseguo i controlli
040600130716             exsr CtrS01;
040700130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
040800130715               leavesr;
040900130715             ENDIF;
041000130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
041100130801               leavesr;
041200130801             ENDIF;
041300130715
041400130715         ENDSL;
041500130715
041600130715       ENDSR;
041700130715
041800130715       //--------------------------------------------------------------
041900130716       //?Inizializzazione videata S01.
042000130715       //--------------------------------------------------------------
042100130716       BEGSR InzS01;
042200130729
042300130729       //?Spegnimento degli indicatori di errore?
042400130801         IF  dsp_mod = 0;
042500130801           %subst(IndDspF : 50) = *off;
042600130801         ELSE;
042700130801           %subst(IndDspF1 : 50) = *off;
042800130801         ENDIF;
042900130715
043000130715       //?Pulizia del Subfile
043100130716         exsr PulS01;
043200130715
043300130729       //?Preparo la stringa SQL
043400130716         exsr PrepSQL;
043500130716
043600130729       //?carico la prima pagina delle anagrafiche
043700130729         exsr OpenCursor;
043800130729         exsr ReadCursor;
043900130729         exsr RollUpS01;
044000130731
044100130731         IF  dsp_mod = 0;
044200130731           wNrr = S01nrr;
044300130731         ELSE;
044400130731           wNrr = S11nrr;
044500130731         ENDIF;
044600130731
044700130729       //?Impaginazione subfile?
044800130729       //?-> forza l'impaginazione sul 1° rec. del subfile
044900130731         IF  wNrr > *zeros;
045000130731           VC1rcd  = 1;
045100130731           VC1csr  = 1;
045200130731         ELSE;
045300130731           clear VC1rcd;
045400130731         ENDIF;
045500130930
045600130930       //?Se non ho ricevuto nessun dato di ricerca
045700130930       //?mi posiziono sulla ragione sociale
045800130930         IF  I81fil = 0 and I81rag = *blanks;
045900130930           IndDspF1.PosCurRAG = *on;
046000131030           IndDspF1.PosCurPRV = *off;
046100130930         ELSE;
046200131030       //?altrimenti mi posiziono sulla provincia
046300130930           IndDspF1.PosCurRAG = *off;
046400131030           IndDspF1.PosCurPRV = *on;
046500130930         ENDIF;
046600130715
046700130715       ENDSR;
046800130715
046900130715       //--------------------------------------------------------------
047000130716       //?Pulizia Subfile S01.
047100130715       //--------------------------------------------------------------
047200130716       BEGSR PulS01;
047300130715
047400130731         IF  dsp_mod = 0;
047500130801           IndDspF.SflDsp    = *on;
047600130801           IndDspF.SflDspCtl = *on;
047700130731           write  OR81C01;
047800130801           IndDspF.SflDspCtl = *off;
047900130801           IndDspF.SflEnd    = *off;
048000130731         ELSE;
048100130801           IndDspF1.SflDsp    = *on;
048200130801           IndDspF1.SflDspCtl = *on;
048300130731           write  OR81C11;
048400130801           IndDspF1.SflDspCtl = *off;
048500130801           IndDspF1.SflEnd    = *off;
048600130801         ENDIF;
048700130715
048800130729         clear wSflPag;
048900130716         clear VC1rcd;
049000130716         clear VC1csr;
049100130716         clear VC1msg;
049200130801
049300130801         IF  dsp_mod = 0;
049400130801           clear S01nrr;
049500130801           IndDspF.ErrMessage  = *off;
049600130801           IndDspF.ErrGenerico = *off;
049700130801         ELSE;
049800130801           clear S11nrr;
049900130801           IndDspF1.ErrMessage  = *off;
050000130801           IndDspF1.ErrGenerico = *off;
050100130801         ENDIF;
050200130715
050300130715         WindDspF = IndDspF;
050400130715         reset WindDspFb;
050500130715         IndDspF  = WindDspF;
050600130715
050700130715       ENDSR;
050800130716
050900130716       //--------------------------------------------------------------
051000130716       //?Preparo la Stringa SQL.
051100130716       //--------------------------------------------------------------
051200130716       BEGSR PrepSQL;
051300131210
051400131210         IF  sav_VC1rag <> *blanks;
051500131210           wRag60 = sav_VC1rag;
051600131210           yy = 1;
051700131210           DOW  yy <= 60;
051800131210             IF  %subst(wRag60:yy:1) = '''';
051900131210               wRag60 = %replace('''''':wRag60:yy:1);
052000131210               yy += 1;
052100131210             ENDIF;
052200131210             yy += 1;
052300131210           ENDDO;
052400131210         ENDIF;
052500130716
052600130730         wSQL = 'SELECT * from FNACR00F ';
052700130716
052800130731         IF  sav_VC1rag <> *blanks and sav_VC1fil <> *zeros and
052900130731             sav_VC1ric = *blanks;
053000130730           wSQL += 'WHERE ACRrsr like(''';
053100131210          // wSQL += sav_VC1rag;
053200131210           wSQL += wRag60;
053300130730           wSQL  = %trim(wSQL) + '%'') and +
053400130730                  substr(digits(ACRcro), 1, 3) = ''' +
053500130730                  %editc(sav_VC1fil:'X') + '''';
053600130730         ENDIF;
053700130731
053800130731         IF  sav_VC1rag <> *blanks and sav_VC1fil <> *zeros and
053900130731             sav_VC1ric = 'S';
054000130731           wSQL += 'WHERE ACRrsr like(''%';
054100131210           // wSQL += %trim(sav_VC1rag);
054200131210           wSQL += %trim(wRag60);
054300130731           wSQL  = %trim(wSQL) + '%'') and +
054400130731                  substr(digits(ACRcro), 1, 3) = ''' +
054500130731                  %editc(sav_VC1fil:'X') + '''';
054600130731         ENDIF;
054700130716
054800130731         IF  sav_VC1rag <> *blanks and sav_VC1fil = *zeros and
054900130731             sav_VC1ric = *blanks;
055000130730           wSQL += 'WHERE ACRrsr like(''';
055100131210           // wSQL += sav_VC1rag;
055200131210           wSQL += wRag60;
055300130730           wSQL  = %trim(wSQL) + '%'')';
055400130730         ENDIF;
055500130731
055600130731         IF  sav_VC1rag <> *blanks and sav_VC1fil = *zeros and
055700130731             sav_VC1ric = 'S';
055800130731           wSQL += 'WHERE ACRrsr like(''%';
055900131210           // wSQL += %trim(sav_VC1rag);
056000131210           wSQL += %trim(wRag60);
056100130731           wSQL  = %trim(wSQL) + '%'')';
056200130731         ENDIF;
056300130730
056400130730         IF  sav_VC1fil <> *zeros and sav_VC1rag = *blanks;
056500130730           wSQL += 'WHERE substr(digits(ACRcro), 1, 3) = ''' +
056600130730                    %editc(sav_VC1fil:'X') + '''';
056700130730         ENDIF;
056800131003
056900131003         IF  sav_VC1prv <> *blanks;
057000131003           IF  %scan('WHERE':wSQL) > 0;
057100131003             wSQL += ' and ACRprr = ''' + VC1prv + '''';
057200131003           ELSE;
057300131003             wSQL += 'WHERE ACRprr = ''' + VC1prv + '''';
057400131003           ENDIF;
057500131003         ENDIF;
057600130716
057700131003         IF  I81mai <> 'S' and sav_VC1rag = *blanks and sav_VC1fil = *zeros
057800131003             and sav_VC1prv = *blanks;
057900130730           wSQL += ' WHERE ACRtcr <> ''M''';
058000130716         ENDIF;
058100130730
058200131003         IF  I81mai <> 'S' and (sav_VC1rag <> *blanks or sav_VC1fil <> *zeros
058300131003             or sav_VC1prv <> *blanks);
058400130730           wSQL += ' and ACRtcr <> ''M''';
058500130730         ENDIF;
058600130716
058700130716         wSQL += ' ORDER BY ACRrsr, ACRcro';
058800130716
058900130716       ENDSR;
059000130729
059100130729       //--------------------------------------------------------------
059200130729       //?Apertura cursore.                                            ?
059300130729       //--------------------------------------------------------------
059400130729       BEGSR  OpenCursor;
059500130729
059600130729         wEnd = *off;
059700130729
059800130729       //?Dichiarazione cursore?
059900130729         exec sql
060000130729         PREPARE S1 from :wSQL;
060100130729         exec sql
060200130729         DECLARE ACR  cursor for S1;
060300130729
060400130729       //?Apertura del cursore
060500130729         exec sql
060600130729         OPEN ACR;
060700130729
060800130729         IF sqlcode < 0;
060900130729           wEnd = *on;
061000130729         ENDIF;
061100130729
061200130729       ENDSR;
061300130729
061400130729       //--------------------------------------------------------------
061500130729       //?Lettura cursore.                                             ?
061600130729       //--------------------------------------------------------------
061700130729       BEGSR  ReadCursor;
061800130729
061900130729         clear  FNACRds;
062000130729
062100130729         exec sql
062200130729         FETCH next from ACR into :FNACRds;
062300130729
062400130729         IF sqlcod = 100 or sqlcod < 0;
062500130729           wEnd = *on;
062600130729         ENDIF;
062700130729
062800130729       ENDSR;
062900130729
063000130729       //--------------------------------------------------------------
063100130729       //?Caricamento singola pagina nel subfile S01                   ?
063200130729       //--------------------------------------------------------------
063300130729       BEGSR  RollUpS01;
063400130729
063500130731         IF  dsp_mod = 0;
063600130731           S01nrr    = (wSflPag  * c_SflPag);
063700130801           IndDspF.SflNxtChg = *off;
063800130801           wNrr = S01nrr;
063900130731         ELSE;
064000130731           S11nrr    = (wSflPag  * c_SflPag);
064100130801           wNrr = S11nrr;
064200130801           IndDspF1.SflNxtChg = *off;
064300130731         ENDIF;
064400130801
064500130729         wSflPag  += 1;
064600130729
064700130729       //?Ciclo di caricamento dati da FNACR00F?
064800130731         DOW  wNrr    < (wSflPag  * c_SflPag)   and
064900130731              wNrr    < c_MaxRec                and
065000130729              not wEnD;
065100130729
065200130731           IF  dsp_mod = 0;
065300130731             clear  OR81S01;
065400130731           ELSE;
065500130731             clear  OR81S11;
065600130731           ENDIF;
065700130729
065800130729       //?Caricamento del singolo record nel subfile S01?
065900130729           clear VS1opz;
066000130729           IF  ACRtcr = 'F' or ACRtcr = 'M';
066100130729             VS1tcr = ACRtcr;
066200130729           ELSE;
066300130729             clear VS1tcr;
066400130729           ENDIF;
066500130729           VS1cd1 = %dec(%subst(%editc(ACRcro:'X'):1:3):3:0);
066600130729           VS1cd2 = %dec(%subst(%editc(ACRcro:'X'):4:4):4:0);
066700130729           VS1cd3 = %dec(%subst(%editc(ACRcro:'X'):8:3):3:0);
066800130729           VS1cro = ACRcro;
066900130729           VS1rag = ACRrsr;
067000130729           VS1ind = ACRinr;
067100130729           VS1loc = ACRlor;
067200130729           VS1prv = ACRprr;
067300130729           VS1naz = ACRnar;
067400130729           VS1atb = ACRatb;
067500130729
067600130729       //?Scrittura record nel subfile
067700130731           IF  dsp_mod = 0;
067800130731             S01nrr += 1;
067900130801             wNrr = S01nrr;
068000130731             write  OR81S01;
068100130731           ELSE;
068200130731             S11nrr += 1;
068300130801             wNrr = S11nrr;
068400130731             write  OR81S11;
068500130731           ENDIF;
068600130729
068700130729       //?Leggo record successivo
068800130729           exsr  ReadCursor;
068900130729
069000130729         ENDDO;
069100130731
069200130731       //?Numero Record subfile
069300130801       //?Visualizzazione del SFL (se ci sono dati)?
069400130801       //?Attivazione del SFLEND?
069500130731         IF  dsp_mod = 0;
069600130731           wNrr = S01nrr;
069700130801           IndDspF.SflDsp = (wNrr <= *zero);
069800130801           IndDspF.SflEnd = wNrr >= c_MaxRec  or  wEnD;
069900130731         ELSE;
070000130731           wNrr = S11nrr;
070100130801           IndDspF1.SflDsp = (wNrr <= *zero);
070200130801           IndDspF1.SflEnd = wNrr >= c_MaxRec  or  wEnD;
070300130731         ENDIF;
070400130729
070500130729       //?Posizionamento cursore al 1° rcd della pagina?
070600130731         IF  wNrr > *zero;
070700130731           wPag     = wNrr / c_SflPag;
070800130731           wRig     = wNrr - (c_SflPag * wPag);
070900130729           VC1rcd = wPag * c_SflPag;
071000130729           IF  wRig > *zero;
071100130729             VC1rcd += 1;
071200130729           ELSE;
071300130729             VC1rcd = VC1rcd - c_SflPag + 1;
071400130729           ENDIF;
071500130729         ELSE;
071600130729           clear  VC1rcd;
071700130729         ENDIF;
071800130729
071900130729         VC1csr = VC1rcd;
072000130729
072100130729       ENDSR;
072200130716
072300130716       //--------------------------------------------------------------
072400130716       //?Controllo la filiale inserita nel control.
072500130716       //--------------------------------------------------------------
072600130716       BEGSR ctrFIL;
072700130716
072800130731         IF  VC1fil > *zeros;
072900130731           chain VC1fil AZORG01L;
073000130731           IF  not %found(AZORG01L) or ORGfva <> *blanks or
073100130731              (ORGfag <> 'F' and ORGfag <> 'A');
073200130801             IF  dsp_mod = 0;
073300130801               IndDspf.ErrMessage  = *on;
073400130801               IndDspF.ErrGenerico = *on;
073500130801               IndDspF.PosCurFIL   = *on;
073600130801             ELSE;
073700130801               IndDspF1.ErrMessage  = *on;
073800130801               IndDspF1.ErrGenerico = *on;
073900130801               IndDspF1.PosCurFIL   = *on;
074000130801             ENDIF;
074100130731             VC1msg = skMSG(01);
074200130731           ENDIF;
074300130716         ENDIF;
074400130716
074500130716       ENDSR;
074600130731
074700130731       //--------------------------------------------------------------
074800130731       //?Controllo la ricerca per contenuto.
074900130731       //--------------------------------------------------------------
075000130731       BEGSR ctrRIC;
075100130731
075200130731         IF  VC1ric <> *blanks and VC1rag = *blanks;
075300130801           IF  dsp_mod = 0;
075400130801             IndDspF.ErrMessage  = *on;
075500130801             IndDspF.ErrGenerico = *on;
075600130801             IndDspF.PosCurRAG   = *on;
075700130801           ELSE;
075800130801             IndDspF1.ErrMessage  = *on;
075900130801             IndDspF1.ErrGenerico = *on;
076000130801             IndDspF1.PosCurRAG   = *on;
076100130801           ENDIF;
076200130731           VC1msg = skMSG(03);
076300130731         ENDIF;
076400130731
076500130731       ENDSR;
076600130716
076700130716       //--------------------------------------------------------------
076800130716       //?Gestione tasto funzionale F12.
076900130716       //?F12=Ritorno.
077000130716       //--------------------------------------------------------------
077100130716       BEGSR F12S01;
077200130716
077300130716         O81ret = '1';
077400130730             exsr CloseCursor;
077500130716
077600130716       //?Chiusura del programma
077700130716         wFine = *on;
077800130716
077900130716       ENDSR;
078000130715
078100130715       //--------------------------------------------------------------
078200130716       //?Controllo videata S01.
078300130715       //--------------------------------------------------------------
078400130716       BEGSR CtrS01;
078500130729
078600130729         clear  sav_S01csr;
078700130715
078800130715         WindDspF = IndDspF;
078900130715         reset WindDspFb;
079000130715         IndDspF  = WindDspF;
079100130715
079200130715       //?Controllo cosa è stato inserito nel subfile
079300130731         IF  dsp_mod = 0;
079400130731           readc OR81S01;
079500130731         ELSE;
079600130731           readc OR81S11;
079700130731         ENDIF;
079800130715
079900130731         DOW  not %eof;
080000130716           %subst(IndDspF : 50) = *off;
080100130731           IF  dsp_mod = 0;
080200130801             IndDspF.SflNxtChg = *off;
080300130731             VC1rcd = S01nrr;
080400130731           ELSE;
080500130801             IndDspF1.SflNxtChg = *off;
080600130731             VC1rcd = S11nrr;
080700130731           ENDIF;
080800130715
080900130715       //?Controllo se opzione valida
081000130715           SELECT;
081100130715             //?Nessuna opzione
081200130716             WHEN  VS1opz  = *blank;
081300130715             //?Opzione '1' di scelta
081400130716             WHEN  VS1opz = '1';
081500130716               O81cro = VS1cro;
081600130716              //?Chiusura del programma
081700130716               wFine = *on;
081800130731             //?Opzione '5' di visualizzazione
081900130731             WHEN  VS1opz = '5';
082000130731               exsr Visualizza;
082100130715             //?Altra opzione errore
082200130715           OTHER;
082300130801             IF  dsp_mod = 0;
082400130801               IndDspF.ErrMessage  = *on;
082500130801               IndDspF.ErrGenerico = *on;
082600130801               IndDspF.PosCurOPZ   = *on;
082700130801             ELSE;
082800130801               IndDspF1.ErrMessage  = *on;
082900130801               IndDspF1.ErrGenerico = *on;
083000130801               IndDspF1.PosCurOPZ   = *on;
083100130801             ENDIF;
083200130716               VC1msg = skMSG(02);
083300130715           ENDSL;
083400130729
083500130729       //?Memorizzazione nrr del sfl con la 1ª opzione per?
083600130729       //?riposizionarvici il cursore dopo il tasto "Invio"?
083700130729           IF  VS1opz  <> *blank   and
083800130729              (sav_S01csr = *zeros  or  sav_S01csr < VC1rcd);
083900130729             sav_S01csr = VC1rcd;
084000130729           ENDIF;
084100130715
084200130715       //?Aggiorno il subfile
084300130715           SELECT;
084400130801             WHEN  dsp_mod = 0 and IndDspF.ErrMessage;
084500130801               IndDspF.SflNxtChg = *on;
084600130801               VC1csr    = VC1rcd;
084700130801
084800130801             WHEN  dsp_mod = 1 and IndDspF1.ErrMessage;
084900130801               IndDspF1.SflNxtChg = *on;
085000130716               VC1csr    = VC1rcd;
085100130801
085200130801             WHEN  dsp_mod = 0 and IndDspF.ErrGenerico;
085300130716               VC1csr = VC1rcd;
085400130716               clear  VS1opz;
085500130801
085600130801             WHEN  dsp_mod = 1 and IndDspF1.ErrGenerico;
085700130801               VC1csr = VC1rcd;
085800130801               clear  VS1opz;
085900130715             OTHER;
086000130729               clear  VS1opz;
086100130715           ENDSL;
086200130715
086300130731           IF  dsp_mod = 0;
086400130731             UPDATE OR81S01;
086500130731           ELSE;
086600130731             UPDATE OR81S11;
086700130731           ENDIF;
086800130715
086900130715       //?Al primo errore esco dalla lettura del subfile
087000130801           IF  dsp_mod = 0 and
087100130801              (IndDspF.ErrMessage or IndDspF.ErrGenerico);
087200130715             leave;
087300130801           ENDIF;
087400130801           IF  dsp_mod = 1 and
087500130801              (IndDspF1.ErrMessage or IndDspF1.ErrGenerico);
087600130801             leave;
087700130801           ENDIF;
087800130715
087900130731           IF  dsp_mod = 0;
088000130731             readc OR81S01;
088100130731           ELSE;
088200130731             readc OR81S11;
088300130731           ENDIF;
088400130715
088500130715         ENDDO;
088600130729
088700130729       //?Riposiziono il cursore sul record contenente la prima opz.?
088800130729       //?(se non sono stati rilevati errori)?
088900130801         IF  dsp_mod = 0;
089000130801           IF  not IndDspF.ErrMessage and not IndDspF.ErrGenerico   and
089100130801               sav_S01csr > *zero;
089200130801             VC1csr = sav_S01csr;
089300130801           ENDIF;
089400130801         ELSE;
089500130801           IF  not IndDspF1.ErrMessage and not IndDspF1.ErrGenerico   and
089600130801               sav_S01csr > *zero;
089700130801             VC1csr = sav_S01csr;
089800130801           ENDIF;
089900130729         ENDIF;
090000130715
090100130715       ENDSR;
090200130731
090300130731       //--------------------------------------------------------------
090400130731       //?Visualizza anagrafica.
090500130731       //--------------------------------------------------------------
090600130731       BEGSR Visualizza;
090700130731
090800130731       //?Richiamo PGM
090900130731         clear FIOR37DS;
091000130731         I37opz = '5';
091100130731         I37cro = VS1cro;
091200130731         kpjbu = FIOR37DS;
091300130731         fior37r1 (kpjba);
091400130731
091500130731       ENDSR;
091600130730
091700130730       //--------------------------------------------------------------
091800130730       //?Chiusura cursore.
091900130730       //--------------------------------------------------------------
092000130730       BEGSR CloseCursor;
092100130730
092200130730       //?Chiusura cursore SQL?
092300130730         exec sql
092400130730           close ACR;
092500130730
092600130730       ENDSR;
092700130715
092800130715       //--------------------------------------------------------------
092900130715       //?Operazioni finali.
093000130715       //--------------------------------------------------------------
093100130715       BEGSR RoutEnd;
093200130715
093300130729       //?Chiusura pgm?
093400130715         *inLR = *on;
093500130715         return;
093600130715
093700130715       ENDSR;
093800130715
093900130715      /end-free
094000130715
094100130715       //--------------------------------------------------------------
094200130715       //?Schiere a tempo di compilazione.
094300130715       //--------------------------------------------------------------
094400130715
094500130715** - skMSG ------------------------------------------------------------------*
094600130715Filiale errata                                                                 01
094700130715Opzione errata                                                                 02
094800130731Ricerca possibile solo se inserita Ragione Sociale                             03
