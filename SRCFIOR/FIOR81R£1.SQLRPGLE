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
016000130716
016100130716       // -?Stringa SQL da eseguire
016200130716     d wSQL            s           2048    Varying        inz
016300130715
016400130715       // -?Campi di Comodo
016500130731     d c_SflPag        s              2s 0
016600130731     d dsp_mod         s             10I 0
016700130729     d sav_S01csr      s                   like(VC1rcd)
016800130716     d sav_VC1fil      s                   like(VC1fil)
016900131003     d sav_VC1prv      s                   like(VC1prv)
017000130716     d sav_VC1rag      s                   like(VC1rag)
017100130731     d sav_VC1ric      s                   like(VC1ric)
017200130716     d S01nrr          s              4  0 inz
017300130731     d S11nrr          s              4  0 inz
017400130731     d wNrr            s                   like(S01nrr)
017500130729     d wPag            s              4  0 inz
017600130729     d wRig            s              3  0 inz
017700130729     d wSflPag         s              3  0 inz
017800130716     d wVideo          s              2    inz('S1')
017900130715
018000130715       //--------------------------------------------------------------
018100130715       //?Definizione procedure esterne.                     ?
018200130715       //--------------------------------------------------------------
018300130731
018400130731       // -?Interrogazione anagrafica clienti ritiro
018500130731     d FIOR37R1        pr                  extpgm('FIOR37R1')
018600130731     d  kpjba                              likeds(kpjba)
018700130715
018800130715       //--------------------------------------------------------------
018900130715       //?Definizione prototipi.
019000130715       //--------------------------------------------------------------
019100130715
019200130715       // -?Reperimento dati utente?
019300130715      /copy gaitrasrc/srcprotopr,tibs34r
019400130731
019500130731       // -?Verifica se possibile video 27x132
019600130731      /copy gaitrasrc/srcprotopr,QsnQryModS
019700130715
019800130715       //--------------------------------------------------------------
019900130715       //?Definizione key-list.                                        ?
020000130715       //--------------------------------------------------------------
020100130715
020200130715       //--------------------------------------------------------------
020300130715       //?Indicatori.
020400130715       //--------------------------------------------------------------
020500130715
020600130715       //--------------------------------------------------------------
020700130715       //?M A I N - L I N E                                            ?
020800130715       //--------------------------------------------------------------
020900130715
021000130715     c     *Entry        plist
021100130715     c                   parm                    KPJBA
021200130716     c                   parm                    FIOR81DS
021300130715
021400130715      /free
021500130715
021600130715       //?Operazioni iniziali?
021700130715       exsr RoutInz;
021800130715
021900130715       //?Gestione vidate?
022000130715       DOW not wFine;
022100130715         SELECT;
022200130716           WHEN  wVideo = 'S1';
022300130716             exsr GesS01;
022400130715           OTHER;
022500130715             wFine = *on;
022600130715         ENDSL;
022700130715       ENDDO;
022800130715
022900130715       //?Operazioni finali?
023000130715       exsr RoutEnd;
023100130715
023200130715       //--------------------------------------------------------------
023300130715       //?Operazioni iniziali.                                         ?
023400130715       //--------------------------------------------------------------
023500130715       BEGSR  RoutInz;
023600130715
023700130715       //?Reperimento dati job?
023800130715         exsr  DatiJob;
023900130715
024000130715         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
024100130731
024200130731       //?Apro file viedeo in base a 24x80 o 27x132
024300130731         dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
024400130731         IF  dsp_mod = 0;
024500130731           c_SflPag = 16;
024600130731           open FIOR81D;
024700130731         ELSE;
024800130731           c_SflPag = 19;
024900130731           open FIOR81D1;
025000130731         ENDIF;
025100130715
025200130715       //?Imposto nome pgm a video
025300130716         VT1pgm = SDSpgm;
025400130716
025500130716       //?Imposto i campi che sono arrivati dal chiamante
025600130716         VC1fil     = I81fil;
025700130716         sav_VC1fil = I81fil;
025800130716         VC1rag     = I81rag;
025900130716         sav_VC1rag = I81rag;
026000130715
026100130715       ENDSR;
026200130715
026300130715       //--------------------------------------------------------------
026400130715       //?Reperimento Dati del job (Utente/Operativi).                 ?
026500130715       //--------------------------------------------------------------
026600130715       BEGSR  DatiJob;
026700130715
026800130715         in(E) §AzUte;
026900130715         IF  NOT %error;
027000130715           in(E) §DatiUte;
027100130715         ENDIF;
027200130715         IF  %error or RSut = *blanks;
027300130715           clear TIBS34DS;
027400130715           tibs34r ( TIBS34DS );
027500130715           in §AzUte;
027600130715           in §DatiUte;
027700130715         ENDIF;
027800130715
027900130715       ENDSR;
028000130715
028100130715       //--------------------------------------------------------------
028200130716       //?Gestione videata S01.
028300130715       //--------------------------------------------------------------
028400130716       BEGSR  GesS01;
028500130715
028600130715       //?Inizializzazione videata
028700130716         IF  wInzS01;
028800130716           exsr InzS01;
028900130716           wInzS01 = *off;
029000130715         ENDIF;
029100130729
029200130729       //?Emissione Testata e Piede con tasti funzionali abilitati?
029300130731         IF  dsp_mod = 0;
029400130731           write OR81T01;
029500130731           write OR81Z01;
029600130731         ELSE;
029700130731           write OR81T11;
029800130731           write OR81Z11;
029900130731         ENDIF;
030000130729
030100130729       //?Posizionamento cursore?
030200130729         IF  VC1csr > *zero;
030300130729           VC1rcd = VC1csr;
030400130729         ELSE;
030500130729           VC1rcd = 1;
030600130731           IF  dsp_mod = 0;
030700130731             write  OR81S00;
030800130731           ELSE;
030900130731             write  OR81S10;
031000130731           ENDIF;
031100130729         ENDIF;
031200130715
031300130715       //?Emissione videata
031400130731         IF  dsp_mod = 0;
031500130731           exfmt OR81C01;
031600130801           IndDspF.ErrMessage  = *off;
031700130801           IndDspF.ErrGenerico = *off;
031800130731         ELSE;
031900130731           exfmt OR81C11;
032000130801           IndDspF1.ErrMessage  = *off;
032100130801           IndDspF1.ErrGenerico = *off;
032200130731         ENDIF;
032300130729
032400130716         clear VC1msg;
032500130715
032600130715         SELECT;
032700131030
032800131030       //?Provincia
032900131030           WHEN  VC1prv <> sav_VC1prv;
033000131030             sav_VC1fil = VC1fil;
033100131030             sav_VC1rag = VC1rag;
033200131030             sav_VC1ric = VC1ric;
033300131030             sav_VC1prv = VC1prv;
033400131030             wInzS01 = *on;
033500131030             exsr CloseCursor;
033600131030             leavesr;
033700130716
033800130716       //?Controllo la filiale
033900130716           WHEN  VC1fil <> sav_VC1fil;
034000130716             exsr CTRfil;
034100130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
034200130731               leavesr;
034300130731             ENDIF;
034400130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
034500130801               leavesr;
034600130801             ENDIF;
034700130716             sav_VC1fil = VC1fil;
034800130716             sav_VC1rag = VC1rag;
034900130731             sav_VC1ric = VC1ric;
035000131003             sav_VC1prv = VC1prv;
035100130716             wInzS01 = *on;
035200130730             exsr CloseCursor;
035300130716             leavesr;
035400130716
035500130716       //?Ragione sociale
035600130716           WHEN  VC1rag <> sav_VC1rag;
035700130716             sav_VC1rag = VC1rag;
035800130716             sav_VC1fil = VC1fil;
035900130731             sav_VC1ric = VC1ric;
036000131003             sav_VC1prv = VC1prv;
036100130716             wInzS01 = *on;
036200130730             exsr CloseCursor;
036300130716             leavesr;
036400130731
036500130731       //?Ricerca per contenuto
036600130731           WHEN  VC1ric <> sav_VC1ric;
036700130731             exsr CTRric;
036800130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
036900130731               leavesr;
037000130731             ENDIF;
037100130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
037200130801               leavesr;
037300130801             ENDIF;
037400130731             sav_VC1rag = VC1rag;
037500130731             sav_VC1fil = VC1fil;
037600130731             sav_VC1ric = VC1ric;
037700131003             sav_VC1prv = VC1prv;
037800130731             wInzS01 = *on;
037900130731             exsr CloseCursor;
038000130731             leavesr;
038100130715
038200130716       //?F12=Ritorno
038300130801           WHEN  dsp_mod = 0 and InfDspF.dsp_aid = c_F12;
038400130716             exsr F12S01;
038500130801           WHEN  dsp_mod = 1 and InfDspF1.dsp_aid = c_F12;
038600130801             exsr F12S01;
038700130729
038800130729       //?Roll-Up?
038900130801           WHEN  dsp_mod = 0 and InfDspF.dsp_aid = c_RollUp;
039000130729             exsr RollUpS01;
039100130801           WHEN  dsp_mod = 1 and InfDspF1.dsp_aid = c_RollUp;
039200130801             exsr RollUpS01;
039300130729
039400130729       //?SubFile vuoto?
039500130731           WHEN  S01nrr = *zeros and dsp_mod = 0;
039600130731
039700130731       //?SubFile vuoto?
039800130731           WHEN  S11nrr = *zeros and dsp_mod <> 0;
039900130716
040000130715       //?Enter
040100130715           OTHER;
040200130716
040300130715         //?Eseguo i controlli
040400130716             exsr CtrS01;
040500130801             IF  dsp_mod = 0 and IndDspF.ErrGenerico;
040600130715               leavesr;
040700130715             ENDIF;
040800130801             IF  dsp_mod = 1 and IndDspF1.ErrGenerico;
040900130801               leavesr;
041000130801             ENDIF;
041100130715
041200130715         ENDSL;
041300130715
041400130715       ENDSR;
041500130715
041600130715       //--------------------------------------------------------------
041700130716       //?Inizializzazione videata S01.
041800130715       //--------------------------------------------------------------
041900130716       BEGSR InzS01;
042000130729
042100130729       //?Spegnimento degli indicatori di errore?
042200130801         IF  dsp_mod = 0;
042300130801           %subst(IndDspF : 50) = *off;
042400130801         ELSE;
042500130801           %subst(IndDspF1 : 50) = *off;
042600130801         ENDIF;
042700130715
042800130715       //?Pulizia del Subfile
042900130716         exsr PulS01;
043000130715
043100130729       //?Preparo la stringa SQL
043200130716         exsr PrepSQL;
043300130716
043400130729       //?carico la prima pagina delle anagrafiche
043500130729         exsr OpenCursor;
043600130729         exsr ReadCursor;
043700130729         exsr RollUpS01;
043800130731
043900130731         IF  dsp_mod = 0;
044000130731           wNrr = S01nrr;
044100130731         ELSE;
044200130731           wNrr = S11nrr;
044300130731         ENDIF;
044400130731
044500130729       //?Impaginazione subfile?
044600130729       //?-> forza l'impaginazione sul 1° rec. del subfile
044700130731         IF  wNrr > *zeros;
044800130731           VC1rcd  = 1;
044900130731           VC1csr  = 1;
045000130731         ELSE;
045100130731           clear VC1rcd;
045200130731         ENDIF;
045300130930
045400130930       //?Se non ho ricevuto nessun dato di ricerca
045500130930       //?mi posiziono sulla ragione sociale
045600130930         IF  I81fil = 0 and I81rag = *blanks;
045700130930           IndDspF1.PosCurRAG = *on;
045800131030           IndDspF1.PosCurPRV = *off;
045900130930         ELSE;
046000131030       //?altrimenti mi posiziono sulla provincia
046100130930           IndDspF1.PosCurRAG = *off;
046200131030           IndDspF1.PosCurPRV = *on;
046300130930         ENDIF;
046400130715
046500130715       ENDSR;
046600130715
046700130715       //--------------------------------------------------------------
046800130716       //?Pulizia Subfile S01.
046900130715       //--------------------------------------------------------------
047000130716       BEGSR PulS01;
047100130715
047200130731         IF  dsp_mod = 0;
047300130801           IndDspF.SflDsp    = *on;
047400130801           IndDspF.SflDspCtl = *on;
047500130731           write  OR81C01;
047600130801           IndDspF.SflDspCtl = *off;
047700130801           IndDspF.SflEnd    = *off;
047800130731         ELSE;
047900130801           IndDspF1.SflDsp    = *on;
048000130801           IndDspF1.SflDspCtl = *on;
048100130731           write  OR81C11;
048200130801           IndDspF1.SflDspCtl = *off;
048300130801           IndDspF1.SflEnd    = *off;
048400130801         ENDIF;
048500130715
048600130729         clear wSflPag;
048700130716         clear VC1rcd;
048800130716         clear VC1csr;
048900130716         clear VC1msg;
049000130801
049100130801         IF  dsp_mod = 0;
049200130801           clear S01nrr;
049300130801           IndDspF.ErrMessage  = *off;
049400130801           IndDspF.ErrGenerico = *off;
049500130801         ELSE;
049600130801           clear S11nrr;
049700130801           IndDspF1.ErrMessage  = *off;
049800130801           IndDspF1.ErrGenerico = *off;
049900130801         ENDIF;
050000130715
050100130715         WindDspF = IndDspF;
050200130715         reset WindDspFb;
050300130715         IndDspF  = WindDspF;
050400130715
050500130715       ENDSR;
050600130716
050700130716       //--------------------------------------------------------------
050800130716       //?Preparo la Stringa SQL.
050900130716       //--------------------------------------------------------------
051000130716       BEGSR PrepSQL;
051100130716
051200130730         wSQL = 'SELECT * from FNACR00F ';
051300130716
051400130731         IF  sav_VC1rag <> *blanks and sav_VC1fil <> *zeros and
051500130731             sav_VC1ric = *blanks;
051600130730           wSQL += 'WHERE ACRrsr like(''';
051700131015           wSQL += sav_VC1rag;
051800130730           wSQL  = %trim(wSQL) + '%'') and +
051900130730                  substr(digits(ACRcro), 1, 3) = ''' +
052000130730                  %editc(sav_VC1fil:'X') + '''';
052100130730         ENDIF;
052200130731
052300130731         IF  sav_VC1rag <> *blanks and sav_VC1fil <> *zeros and
052400130731             sav_VC1ric = 'S';
052500130731           wSQL += 'WHERE ACRrsr like(''%';
052600130731           wSQL += %trim(sav_VC1rag);
052700130731           wSQL  = %trim(wSQL) + '%'') and +
052800130731                  substr(digits(ACRcro), 1, 3) = ''' +
052900130731                  %editc(sav_VC1fil:'X') + '''';
053000130731         ENDIF;
053100130716
053200130731         IF  sav_VC1rag <> *blanks and sav_VC1fil = *zeros and
053300130731             sav_VC1ric = *blanks;
053400130730           wSQL += 'WHERE ACRrsr like(''';
053500131015           wSQL += sav_VC1rag;
053600130730           wSQL  = %trim(wSQL) + '%'')';
053700130730         ENDIF;
053800130731
053900130731         IF  sav_VC1rag <> *blanks and sav_VC1fil = *zeros and
054000130731             sav_VC1ric = 'S';
054100130731           wSQL += 'WHERE ACRrsr like(''%';
054200130731           wSQL += %trim(sav_VC1rag);
054300130731           wSQL  = %trim(wSQL) + '%'')';
054400130731         ENDIF;
054500130730
054600130730         IF  sav_VC1fil <> *zeros and sav_VC1rag = *blanks;
054700130730           wSQL += 'WHERE substr(digits(ACRcro), 1, 3) = ''' +
054800130730                    %editc(sav_VC1fil:'X') + '''';
054900130730         ENDIF;
055000131003
055100131003         IF  sav_VC1prv <> *blanks;
055200131003           IF  %scan('WHERE':wSQL) > 0;
055300131003             wSQL += ' and ACRprr = ''' + VC1prv + '''';
055400131003           ELSE;
055500131003             wSQL += 'WHERE ACRprr = ''' + VC1prv + '''';
055600131003           ENDIF;
055700131003         ENDIF;
055800130716
055900131003         IF  I81mai <> 'S' and sav_VC1rag = *blanks and sav_VC1fil = *zeros
056000131003             and sav_VC1prv = *blanks;
056100130730           wSQL += ' WHERE ACRtcr <> ''M''';
056200130716         ENDIF;
056300130730
056400131003         IF  I81mai <> 'S' and (sav_VC1rag <> *blanks or sav_VC1fil <> *zeros
056500131003             or sav_VC1prv <> *blanks);
056600130730           wSQL += ' and ACRtcr <> ''M''';
056700130730         ENDIF;
056800130716
056900130716         wSQL += ' ORDER BY ACRrsr, ACRcro';
057000130716
057100130716       ENDSR;
057200130729
057300130729       //--------------------------------------------------------------
057400130729       //?Apertura cursore.                                            ?
057500130729       //--------------------------------------------------------------
057600130729       BEGSR  OpenCursor;
057700130729
057800130729         wEnd = *off;
057900130729
058000130729       //?Dichiarazione cursore?
058100130729         exec sql
058200130729         PREPARE S1 from :wSQL;
058300130729         exec sql
058400130729         DECLARE ACR  cursor for S1;
058500130729
058600130729       //?Apertura del cursore
058700130729         exec sql
058800130729         OPEN ACR;
058900130729
059000130729         IF sqlcode < 0;
059100130729           wEnd = *on;
059200130729         ENDIF;
059300130729
059400130729       ENDSR;
059500130729
059600130729       //--------------------------------------------------------------
059700130729       //?Lettura cursore.                                             ?
059800130729       //--------------------------------------------------------------
059900130729       BEGSR  ReadCursor;
060000130729
060100130729         clear  FNACRds;
060200130729
060300130729         exec sql
060400130729         FETCH next from ACR into :FNACRds;
060500130729
060600130729         IF sqlcod = 100 or sqlcod < 0;
060700130729           wEnd = *on;
060800130729         ENDIF;
060900130729
061000130729       ENDSR;
061100130729
061200130729       //--------------------------------------------------------------
061300130729       //?Caricamento singola pagina nel subfile S01                   ?
061400130729       //--------------------------------------------------------------
061500130729       BEGSR  RollUpS01;
061600130729
061700130731         IF  dsp_mod = 0;
061800130731           S01nrr    = (wSflPag  * c_SflPag);
061900130801           IndDspF.SflNxtChg = *off;
062000130801           wNrr = S01nrr;
062100130731         ELSE;
062200130731           S11nrr    = (wSflPag  * c_SflPag);
062300130801           wNrr = S11nrr;
062400130801           IndDspF1.SflNxtChg = *off;
062500130731         ENDIF;
062600130801
062700130729         wSflPag  += 1;
062800130729
062900130729       //?Ciclo di caricamento dati da FNACR00F?
063000130731         DOW  wNrr    < (wSflPag  * c_SflPag)   and
063100130731              wNrr    < c_MaxRec                and
063200130729              not wEnD;
063300130729
063400130731           IF  dsp_mod = 0;
063500130731             clear  OR81S01;
063600130731           ELSE;
063700130731             clear  OR81S11;
063800130731           ENDIF;
063900130729
064000130729       //?Caricamento del singolo record nel subfile S01?
064100130729           clear VS1opz;
064200130729           IF  ACRtcr = 'F' or ACRtcr = 'M';
064300130729             VS1tcr = ACRtcr;
064400130729           ELSE;
064500130729             clear VS1tcr;
064600130729           ENDIF;
064700130729           VS1cd1 = %dec(%subst(%editc(ACRcro:'X'):1:3):3:0);
064800130729           VS1cd2 = %dec(%subst(%editc(ACRcro:'X'):4:4):4:0);
064900130729           VS1cd3 = %dec(%subst(%editc(ACRcro:'X'):8:3):3:0);
065000130729           VS1cro = ACRcro;
065100130729           VS1rag = ACRrsr;
065200130729           VS1ind = ACRinr;
065300130729           VS1loc = ACRlor;
065400130729           VS1prv = ACRprr;
065500130729           VS1naz = ACRnar;
065600130729           VS1atb = ACRatb;
065700130729
065800130729       //?Scrittura record nel subfile
065900130731           IF  dsp_mod = 0;
066000130731             S01nrr += 1;
066100130801             wNrr = S01nrr;
066200130731             write  OR81S01;
066300130731           ELSE;
066400130731             S11nrr += 1;
066500130801             wNrr = S11nrr;
066600130731             write  OR81S11;
066700130731           ENDIF;
066800130729
066900130729       //?Leggo record successivo
067000130729           exsr  ReadCursor;
067100130729
067200130729         ENDDO;
067300130731
067400130731       //?Numero Record subfile
067500130801       //?Visualizzazione del SFL (se ci sono dati)?
067600130801       //?Attivazione del SFLEND?
067700130731         IF  dsp_mod = 0;
067800130731           wNrr = S01nrr;
067900130801           IndDspF.SflDsp = (wNrr <= *zero);
068000130801           IndDspF.SflEnd = wNrr >= c_MaxRec  or  wEnD;
068100130731         ELSE;
068200130731           wNrr = S11nrr;
068300130801           IndDspF1.SflDsp = (wNrr <= *zero);
068400130801           IndDspF1.SflEnd = wNrr >= c_MaxRec  or  wEnD;
068500130731         ENDIF;
068600130729
068700130729       //?Posizionamento cursore al 1° rcd della pagina?
068800130731         IF  wNrr > *zero;
068900130731           wPag     = wNrr / c_SflPag;
069000130731           wRig     = wNrr - (c_SflPag * wPag);
069100130729           VC1rcd = wPag * c_SflPag;
069200130729           IF  wRig > *zero;
069300130729             VC1rcd += 1;
069400130729           ELSE;
069500130729             VC1rcd = VC1rcd - c_SflPag + 1;
069600130729           ENDIF;
069700130729         ELSE;
069800130729           clear  VC1rcd;
069900130729         ENDIF;
070000130729
070100130729         VC1csr = VC1rcd;
070200130729
070300130729       ENDSR;
070400130716
070500130716       //--------------------------------------------------------------
070600130716       //?Controllo la filiale inserita nel control.
070700130716       //--------------------------------------------------------------
070800130716       BEGSR ctrFIL;
070900130716
071000130731         IF  VC1fil > *zeros;
071100130731           chain VC1fil AZORG01L;
071200130731           IF  not %found(AZORG01L) or ORGfva <> *blanks or
071300130731              (ORGfag <> 'F' and ORGfag <> 'A');
071400130801             IF  dsp_mod = 0;
071500130801               IndDspf.ErrMessage  = *on;
071600130801               IndDspF.ErrGenerico = *on;
071700130801               IndDspF.PosCurFIL   = *on;
071800130801             ELSE;
071900130801               IndDspF1.ErrMessage  = *on;
072000130801               IndDspF1.ErrGenerico = *on;
072100130801               IndDspF1.PosCurFIL   = *on;
072200130801             ENDIF;
072300130731             VC1msg = skMSG(01);
072400130731           ENDIF;
072500130716         ENDIF;
072600130716
072700130716       ENDSR;
072800130731
072900130731       //--------------------------------------------------------------
073000130731       //?Controllo la ricerca per contenuto.
073100130731       //--------------------------------------------------------------
073200130731       BEGSR ctrRIC;
073300130731
073400130731         IF  VC1ric <> *blanks and VC1rag = *blanks;
073500130801           IF  dsp_mod = 0;
073600130801             IndDspF.ErrMessage  = *on;
073700130801             IndDspF.ErrGenerico = *on;
073800130801             IndDspF.PosCurRAG   = *on;
073900130801           ELSE;
074000130801             IndDspF1.ErrMessage  = *on;
074100130801             IndDspF1.ErrGenerico = *on;
074200130801             IndDspF1.PosCurRAG   = *on;
074300130801           ENDIF;
074400130731           VC1msg = skMSG(03);
074500130731         ENDIF;
074600130731
074700130731       ENDSR;
074800130716
074900130716       //--------------------------------------------------------------
075000130716       //?Gestione tasto funzionale F12.
075100130716       //?F12=Ritorno.
075200130716       //--------------------------------------------------------------
075300130716       BEGSR F12S01;
075400130716
075500130716         O81ret = '1';
075600130730             exsr CloseCursor;
075700130716
075800130716       //?Chiusura del programma
075900130716         wFine = *on;
076000130716
076100130716       ENDSR;
076200130715
076300130715       //--------------------------------------------------------------
076400130716       //?Controllo videata S01.
076500130715       //--------------------------------------------------------------
076600130716       BEGSR CtrS01;
076700130729
076800130729         clear  sav_S01csr;
076900130715
077000130715         WindDspF = IndDspF;
077100130715         reset WindDspFb;
077200130715         IndDspF  = WindDspF;
077300130715
077400130715       //?Controllo cosa è stato inserito nel subfile
077500130731         IF  dsp_mod = 0;
077600130731           readc OR81S01;
077700130731         ELSE;
077800130731           readc OR81S11;
077900130731         ENDIF;
078000130715
078100130731         DOW  not %eof;
078200130716           %subst(IndDspF : 50) = *off;
078300130731           IF  dsp_mod = 0;
078400130801             IndDspF.SflNxtChg = *off;
078500130731             VC1rcd = S01nrr;
078600130731           ELSE;
078700130801             IndDspF1.SflNxtChg = *off;
078800130731             VC1rcd = S11nrr;
078900130731           ENDIF;
079000130715
079100130715       //?Controllo se opzione valida
079200130715           SELECT;
079300130715             //?Nessuna opzione
079400130716             WHEN  VS1opz  = *blank;
079500130715             //?Opzione '1' di scelta
079600130716             WHEN  VS1opz = '1';
079700130716               O81cro = VS1cro;
079800130716              //?Chiusura del programma
079900130716               wFine = *on;
080000130731             //?Opzione '5' di visualizzazione
080100130731             WHEN  VS1opz = '5';
080200130731               exsr Visualizza;
080300130715             //?Altra opzione errore
080400130715           OTHER;
080500130801             IF  dsp_mod = 0;
080600130801               IndDspF.ErrMessage  = *on;
080700130801               IndDspF.ErrGenerico = *on;
080800130801               IndDspF.PosCurOPZ   = *on;
080900130801             ELSE;
081000130801               IndDspF1.ErrMessage  = *on;
081100130801               IndDspF1.ErrGenerico = *on;
081200130801               IndDspF1.PosCurOPZ   = *on;
081300130801             ENDIF;
081400130716               VC1msg = skMSG(02);
081500130715           ENDSL;
081600130729
081700130729       //?Memorizzazione nrr del sfl con la 1ª opzione per?
081800130729       //?riposizionarvici il cursore dopo il tasto "Invio"?
081900130729           IF  VS1opz  <> *blank   and
082000130729              (sav_S01csr = *zeros  or  sav_S01csr < VC1rcd);
082100130729             sav_S01csr = VC1rcd;
082200130729           ENDIF;
082300130715
082400130715       //?Aggiorno il subfile
082500130715           SELECT;
082600130801             WHEN  dsp_mod = 0 and IndDspF.ErrMessage;
082700130801               IndDspF.SflNxtChg = *on;
082800130801               VC1csr    = VC1rcd;
082900130801
083000130801             WHEN  dsp_mod = 1 and IndDspF1.ErrMessage;
083100130801               IndDspF1.SflNxtChg = *on;
083200130716               VC1csr    = VC1rcd;
083300130801
083400130801             WHEN  dsp_mod = 0 and IndDspF.ErrGenerico;
083500130716               VC1csr = VC1rcd;
083600130716               clear  VS1opz;
083700130801
083800130801             WHEN  dsp_mod = 1 and IndDspF1.ErrGenerico;
083900130801               VC1csr = VC1rcd;
084000130801               clear  VS1opz;
084100130715             OTHER;
084200130729               clear  VS1opz;
084300130715           ENDSL;
084400130715
084500130731           IF  dsp_mod = 0;
084600130731             UPDATE OR81S01;
084700130731           ELSE;
084800130731             UPDATE OR81S11;
084900130731           ENDIF;
085000130715
085100130715       //?Al primo errore esco dalla lettura del subfile
085200130801           IF  dsp_mod = 0 and
085300130801              (IndDspF.ErrMessage or IndDspF.ErrGenerico);
085400130715             leave;
085500130801           ENDIF;
085600130801           IF  dsp_mod = 1 and
085700130801              (IndDspF1.ErrMessage or IndDspF1.ErrGenerico);
085800130801             leave;
085900130801           ENDIF;
086000130715
086100130731           IF  dsp_mod = 0;
086200130731             readc OR81S01;
086300130731           ELSE;
086400130731             readc OR81S11;
086500130731           ENDIF;
086600130715
086700130715         ENDDO;
086800130729
086900130729       //?Riposiziono il cursore sul record contenente la prima opz.?
087000130729       //?(se non sono stati rilevati errori)?
087100130801         IF  dsp_mod = 0;
087200130801           IF  not IndDspF.ErrMessage and not IndDspF.ErrGenerico   and
087300130801               sav_S01csr > *zero;
087400130801             VC1csr = sav_S01csr;
087500130801           ENDIF;
087600130801         ELSE;
087700130801           IF  not IndDspF1.ErrMessage and not IndDspF1.ErrGenerico   and
087800130801               sav_S01csr > *zero;
087900130801             VC1csr = sav_S01csr;
088000130801           ENDIF;
088100130729         ENDIF;
088200130715
088300130715       ENDSR;
088400130731
088500130731       //--------------------------------------------------------------
088600130731       //?Visualizza anagrafica.
088700130731       //--------------------------------------------------------------
088800130731       BEGSR Visualizza;
088900130731
089000130731       //?Richiamo PGM
089100130731         clear FIOR37DS;
089200130731         I37opz = '5';
089300130731         I37cro = VS1cro;
089400130731         kpjbu = FIOR37DS;
089500130731         fior37r1 (kpjba);
089600130731
089700130731       ENDSR;
089800130730
089900130730       //--------------------------------------------------------------
090000130730       //?Chiusura cursore.
090100130730       //--------------------------------------------------------------
090200130730       BEGSR CloseCursor;
090300130730
090400130730       //?Chiusura cursore SQL?
090500130730         exec sql
090600130730           close ACR;
090700130730
090800130730       ENDSR;
090900130715
091000130715       //--------------------------------------------------------------
091100130715       //?Operazioni finali.
091200130715       //--------------------------------------------------------------
091300130715       BEGSR RoutEnd;
091400130715
091500130729       //?Chiusura pgm?
091600130715         *inLR = *on;
091700130715         return;
091800130715
091900130715       ENDSR;
092000130715
092100130715      /end-free
092200130715
092300130715       //--------------------------------------------------------------
092400130715       //?Schiere a tempo di compilazione.
092500130715       //--------------------------------------------------------------
092600130715
092700130715** - skMSG ------------------------------------------------------------------*
092800130715Filiale errata                                                                 01
092900130715Opzione errata                                                                 02
093000130731Ricerca possibile solo se inserita Ragione Sociale                             03
