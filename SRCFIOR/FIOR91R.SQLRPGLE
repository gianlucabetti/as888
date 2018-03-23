000100160226      //==============================================================
000200160226      //?FIOR91R * Crea WrkF ORM x DLO
000300160226      //==============================================================
000400160226     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500160226     h dftactgrp(*no)
000600160226
000700160226      //--------------------------------------------------------------
000800160226      //?Dichiarazione file.                                          ?
000900160226      //--------------------------------------------------------------
001000160226      //?Organigramma?
001100160226     fAZORG01L  if   e           k disk
001200160229
001300160229      //?Fasi ORM
001400160229     fFNORF01L  if   e           k disk
001500160226
001600160226      //?WrkFile ORM
001700160226     fWFORMT0F  o    e             disk    extfile(wFLibT)
001800160226     f                                     usropn
001900160226     f                                     rename(FNORM000:WFORMT)
002000160226     f                                     prefix(w)
002100160226     fWFORMF0F  o    e             disk    extfile(wFLibF)
002200160226     f                                     usropn
002300160226     f                                     rename(FNORF000:WFORMF)
002400160226     f                                     prefix(w)
002500160226
002600160226      //?File video
002700160226     fFIOR91D   cf   e             workstn indds(IndDspF)
002800160226     f                                     infds(InfDspF)
002900160226
003000160226      //--------------------------------------------------------------
003100160226      //?Definizione costanti.                                        ?
003200160226      //--------------------------------------------------------------
003300160226      //?Tasti funzionali a video
003400160226     d c_F01           c                   const(x'31')
003500160226     d c_F02           c                   const(x'32')
003600160226     d c_F03           c                   const(x'33')
003700160226     d c_F04           c                   const(x'34')
003800160226     d c_F05           c                   const(x'35')
003900160226     d c_F06           c                   const(x'36')
004000160226     d c_F07           c                   const(x'37')
004100160226     d c_F08           c                   const(x'38')
004200160226     d c_F09           c                   const(x'39')
004300160226     d c_F10           c                   const(x'3A')
004400160226     d c_F11           c                   const(x'3B')
004500160226     d c_F12           c                   const(x'3C')
004600160226     d c_F13           c                   const(x'B1')
004700160226     d c_F14           c                   const(x'B2')
004800160226     d c_F15           c                   const(x'B3')
004900160226     d c_F16           c                   const(x'B4')
005000160226     d c_F17           c                   const(x'B5')
005100160226     d c_F18           c                   const(x'B6')
005200160226     d c_F19           c                   const(x'B7')
005300160226     d c_F20           c                   const(x'B8')
005400160226     d c_F21           c                   const(x'B9')
005500160226     d c_F22           c                   const(x'BA')
005600160226     d c_F23           c                   const(x'BB')
005700160226     d c_F24           c                   const(x'BC')
005800160226     d c_Enter         c                   const(x'F1')
005900160226     d c_RollDown      c                   const(x'F4')
006000160226     d c_RollUp        c                   const(x'F5')
006100160226
006200160226     d Digits          c                   const('0123456789')
006300160226
006400160226      //--------------------------------------------------------------
006500160226      //?Definizione schiere.                                         ?
006600160226      //--------------------------------------------------------------
006700160226      //?Messaggi di errore
006800160226     d Msg             s             50    dim(10) ctdata perrcd(1)
006900160226
007000160226      //--------------------------------------------------------------
007100160226      //?Definizione aree dati.                                       ?
007200160226      //--------------------------------------------------------------
007300160226      //?Dati utente?
007400160226     d §AzUte        e ds                  extname(AZUTE00F)
007500160226     d                                     dtaara
007600160226     d §DatiUte      e ds                  extname(dDatiUte)
007700160226     d                                     dtaara
007800160226
007900160226      //--------------------------------------------------------------
008000160226      //?Definizione strutture dati.                                  ?
008100160226      //--------------------------------------------------------------
008200160226      //?InfDS
008300160226     d InfDspF         ds
008400160226     d  dsp_aid              369    369a
008500160226     d  sfl_rrn              376    377i 0
008600160226     d  min_nrr              378    379i 0
008700160226     d  num_rcds             380    381i 0
008800160226
008900160226      //?Indicatori su DspF
009000160226     d IndDspF         ds
009100160226      //?Indicatori di errore in videata
009200160226     d  ErrMessage                    1n   overlay(IndDspF : 28)
009300160226      //?Indicatori di errore
009400160226     d  PosCurPOR                     1n   overlay(IndDspF : 50)
009500160226     d  PosCurDAR                     1n   overlay(IndDspF : 51)
009600160226      //?Indicatori di errore generico
009700160226     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009800160226
009900160226     d WindDspF        ds                  inz
010000160226     d  WindDspFa              1     49    inz(*zero)
010100160226     d  WindDspFb             50     99    inz(*zero)
010200160226
010300160226      //?Parametri per pgm. controllo/inversione data?
010400160226     d wlbdat          ds                  inz
010500160226     d  G08dat                 1      8  0
010600160226     d  G08inv                 9     16  0
010700160226     d  G08err                17     17
010800160226     d  G08tgi                18     22  0
010900160226
011000160226      //?Parametri ricevuti
011100160226     d KPJBA         e ds
011200160226
011300160226      //?Parametri per Reperimento dati utente?
011400160226     d TIBS34ds      e ds
011500160226
011600160226      //?File ORM
011700160226     d FNORM00F      e ds                  extname(FNORM00F)
011800160226
011900160226      //--------------------------------------------------------------
012000160226      //?Definizione variabili globali.                               ?
012100160226      //--------------------------------------------------------------
012200160226      //?Flags booleani
012300160226     d ErrGrave        s               n   inz(*off)
012400160226     d Fine            s               n   inz(*off)
012500160226     d wEnd            s               n   inz(*off)
012600160226     d wInzW01         s               n   inz(*off)
012700160226
012800160226      //?Nome libreria dei work-file
012900160226     d wFLibT          s             21    inz
013000160226     d wFLibF          s             21    inz
013100160226
013200160226      //?Stringa SQL da eseguire
013300160226     d wSQL            s           2048    Varying        inz
013400160226
013500160226      //?Indici di schiera
013600160226     d xx              s              4s 0 inz
013700160226
013800160226      //?Campo di comodo data
013900160226     d Data_EUR        s               d   datfmt(*eur)
014000160226     d Data_ISO        s               d   datfmt(*iso)
014100160226
014200160226      //?Campo di comodo
014300160226     d cmd             s            512a   varying
014400160226     d Oggi            s              8s 0 inz
014500160226     d Video           s              2a   inz
014600160226     d wdar            s              8s 0 inz
014700160226     d wpor            s              3s 0 inz
014800160226
014900160226      //?Parametri per ricerca Filiale
015000160226     d pinFIL          s              3
015100160226     d pinFAG          s              1
015200160226     d pinDES          s             25
015300160226     d pinDIT          s              3
015400160226
015500160226      //--------------------------------------------------------------
015600160226      //?Definizione Procedure usate.
015700160226      //--------------------------------------------------------------
015800160226
015900160226      //---------------------------------------------------------------
016000160226      //?Definizione Prototipi.
016100160226      //---------------------------------------------------------------
016200160226      /copy gaitrasrc/srcProtoPR,TIBS34R
016300160226      /copy gaitrasrc/srcprotopr,TNSD24R
016400160226      /copy gaitrasrc/srcprotopr,SYSTEM
016500160226      /copy gaitrasrc/srcprotopr,XSRDA8
016600160226
016700160226      //--------------------------------------------------------------
016800160226      //?Definizione key-list.                                        ?
016900160226      //--------------------------------------------------------------
017000160226
017100160226      //--------------------------------------------------------------
017200160226      //?M A I N - L I N E                                            ?
017300160226      //--------------------------------------------------------------
017400160226
017500160226     c     *Entry        plist
017600160226     c                   parm                    KPJBA
017700160226
017800160226      /free
017900160226
018000160226       //?Operazioni iniziali
018100160226       exsr RoutInz;
018200160226
018300160226       //?Gestione video
018400160226       DOW  not Fine;
018500160226         SELECT;
018600160226         WHEN  Video = 'W1';
018700160226           exsr GesW01;
018800160226         OTHER;
018900160226           Fine = *on;
019000160226         ENDSL;
019100160226       ENDDO;
019200160226
019300160226       //?Operazioni finali
019400160226       exsr RoutEnd;
019500160226
019600160226       //--------------------------------------------------------------
019700160226       //?Operazioni iniziali.                                         ?
019800160226       //--------------------------------------------------------------
019900160226       BEGSR RoutInz;
020000160226
020100160226         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
020200160226
020300160226       //?Reperimento dati job?
020400160226         exsr  DatiJob;
020500160226
020600160226       //?Impostazione campi "fissi"
020700160226         Video = 'W1';
020800160226         wInzW01 = *on;
020900160226
021000160226       //?Imposto la data di oggi
021100160226         Oggi = %dec(%date());
021200160226         Data_ISO = %date(Oggi);
021300160226         Data_EUR = Data_ISO;
021400160226
021500160226       ENDSR;
021600160226
021700160226       //--------------------------------------------------------------
021800160226       //?Reperimento Dati del job (Utente/Operativi).                 ?
021900160226       //--------------------------------------------------------------
022000160226       BEGSR DatiJob;
022100160226
022200160226         in(E) §AzUte;
022300160226         IF  NOT %error;
022400160226           in(E) §DatiUte;
022500160226         ENDIF;
022600160226         IF  %error or RSut = *blanks;
022700160226           clear TIBS34ds;
022800160226           tibs34r(tibs34ds);
022900160226           in §AzUte;
023000160226           in §DatiUte;
023100160226         ENDIF;
023200160226
023300160226       ENDSR;
023400160226
023500160226       //--------------------------------------------------------------
023600160226       //?Gestione videata W01.
023700160226       //--------------------------------------------------------------
023800160226       BEGSR GesW01;
023900160226
024000160226       //?Inizializzazione videata
024100160226         IF  wInzW01;
024200160226           exsr InzW01;
024300160226           wInzW01 = *off;
024400160226         ENDIF;
024500160226
024600160226       //?Emissione videata
024700160226         exfmt OR91W01;
024800160226         reset ErrMessage;
024900160226         reset ErrGenerico;
025000160226         clear W01msg;
025100160226
025200160226         SELECT;
025300160226
025400160226       //?F03=Fine
025500160226         WHEN  dsp_aid = c_F03;
025600160226           exsr F03W01;
025700160226
025800160226       //?F06=Conferma
025900160226         WHEN  dsp_aid = c_F06;
026000160226           exsr ContrW01;
026100160226           IF  ErrGenerico;
026200160226             leavesr;
026300160226           ENDIF;
026400160226           exsr F06W01;
026500160226           IF  ErrGenerico;
026600160226             leavesr;
026700160226           ENDIF;
026800160226
026900160226       //?Invio
027000160226         OTHER;
027100160226           exsr ContrW01;
027200160226           IF  ErrGenerico;
027300160226             leavesr;
027400160226           ENDIF;
027500160226
027600160226         ENDSL;
027700160226
027800160226       ENDSR;
027900160226
028000160226       //--------------------------------------------------------------
028100160226       //?Inizializzazione Videata W01.
028200160226       //--------------------------------------------------------------
028300160226       BEGSR InzW01;
028400160226
028500160226       //?Pulizia videata
028600160226         clear OR91W01;
028700160226
028800160226         W01dar = %dec(Data_EUR);
028900160226
029000160226       ENDSR;
029100160226
029200160226       //--------------------------------------------------------------
029300160226       //?Gestione tasto funzionale F03 da videata W01
029400160226       //?F03=Fine
029500160226       //--------------------------------------------------------------
029600160226       BEGSR F03W01;
029700160226
029800160226       //?Chiusura del programma
029900160226         Fine = *on;
030000160226
030100160226       ENDSR;
030200160226
030300160226       //--------------------------------------------------------------
030400160226       //?Gestione tasto funzionale F06 da videata W01
030500160226       //?F06=Conferma
030600160226       //--------------------------------------------------------------
030700160226       BEGSR F06W01;
030800160226
030900160226       //?Pulisco e apro i file di Work
031000160226         exsr Pulisci;
031100160226         IF  ErrGenerico;
031200160226           leavesr;
031300160226         ENDIF;
031400160226
031500160226       //?Leggo Gli ORM e scrivo i file di Work
031600160226         exsr Elabora;
031700160226
031800160226       //?Esco dal Programma
031900160226         exsr RoutEnd;
032000160226
032100160226       ENDSR;
032200160226
032300160226       //--------------------------------------------------------------
032400160226       //?Controlla Videata W01.
032500160226       //--------------------------------------------------------------
032600160226       BEGSR ContrW01;
032700160226
032800160226         WindDspF = IndDspF;
032900160226         reset WindDspFb;
033000160226         IndDspF  = WindDspF;
033100160226
033200160226       //?Filale obbligatoria
033300160226         IF  W01por = *blanks;
033400160226           ErrMessage  = *on;
033500160226           ErrGenerico = *on;
033600160226           PosCurPOR = *on;
033700160226           W01msg = Msg(01);
033800160226           leavesr;
033900160226         ENDIF;
034000160226
034100160226       //?Ricerca Filale
034200160226         IF  %scan('?' : W01por) > 0;
034300160226           clear pinFIL;
034400160226           clear pinFAG;
034500160226           clear pinDES;
034600160226           clear pinDIT;
034700160226           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
034800160226           IF pinFIL > *zeros;
034900160226             W01por  = pinFIL;
035000160226             W01pord = pinDES;
035100160226           ENDIF;
035200160226           ErrGenerico = *on;
035300160226           PosCurPOR = *on;
035400160226           leavesr;
035500160226         ENDIF;
035600160226       //?Accetto solo dati numerici
035700160226         xx = 1;
035800160226         FOR xx by 1 to %len(W01por);
035900160226           IF  %subst(W01por:xx:1) <> *blanks and
036000160226               %check(Digits:%subst(W01por:xx:1)) > *zeros;
036100160226             ErrMessage  = *on;
036200160226             ErrGenerico = *on;
036300160226             PosCurPOR = *on;
036400160226             W01msg = Msg(01);
036500160226             leavesr;
036600160226           ENDIF;
036700160226           IF  %subst(W01por:xx:1) = *blanks;
036800160226             %subst(W01por:xx:1) = '0';
036900160226           ENDIF;
037000160226         ENDFOR;
037100160226
037200160226       //?Deve esistere la Filiale
037300160226         wpor = %dec(W01por:3:0);
037400160226         chain wpor AZORG01L;
037500160226         IF  not %found(AZORG01L) or ORGfva <> *blanks;
037600160226           ErrMessage  = *on;
037700160226           ErrGenerico = *on;
037800160226           PosCurPOR = *on;
037900160226           W01msg = Msg(01);
038000160226           leavesr;
038100160226         ENDIF;
038200160226       //?Decodifico filiale
038300160226         W01pord = ORGdes;
038400160226
038500160226       //?Data obbligatoria
038600160226         IF  W01dar = *zeros;
038700160226           ErrMessage  = *on;
038800160226           ErrGenerico = *on;
038900160226           PosCurDAR = *on;
039000160226           W01msg = Msg(02);
039100160226           leavesr;
039200160226         ENDIF;
039300160226
039400160226       //?Controllo Data
039500160226         clear wlbdat;
039600160226         G08dat = W01dar;
039700160226         xsrda8(wlbdat);
039800160226         IF  G08err = '1';
039900160226           ErrMessage  = *on;
040000160226           ErrGenerico = *on;
040100160226           PosCurDAR   = *on;
040200160226           W01msg = Msg(02);
040300160226           leavesr;
040400160226         ENDIF;
040500160226         W01dar = G08dat;
040600160226         wdar   = G08inv;
040700160226
040800160226       ENDSR;
040900160226
041000160226       //--------------------------------------------------------------
041100160226       //?Pulisco e Apro i file di Work.
041200160226       //--------------------------------------------------------------
041300160226       BEGSR Pulisci;
041400160226
041500160226       //?Imposto libreria/file in base al sistema informativo
041600160226         IF  %subst(knsif : 7 : 1) = 'P';
041700160226           wFLibT = 'GAITRAAZP/WFORMT0F';
041800160226           wFLibF = 'GAITRAAZP/WFORMF0F';
041900160226         ELSE;
042000160226           wFLibT = 'GAITRAAZM/WFORMT0F';
042100160226           wFLibF = 'GAITRAAZM/WFORMF0F';
042200160226         ENDIF;
042300160226
042400160226       //?Pulizia ed apertura WrkF?
042500160226         cmd = 'CLRPFM FILE(';
042600160226         cmd = %trim(cmd) + %trim(wFlibT) + ')';
042700160226         IF  ExecuteCommand (cmd) = 0;
042800160226           open WFORMT0F;
042900160226         ELSE;
043000160226           ErrMessage  = *on;
043100160226           ErrGenerico = *on;
043200160226           W01msg = 'Errore nell''esecuzione della funzione avvisare ICT';
043300160226           leavesr;
043400160226         ENDIF;
043500160226         cmd = 'CLRPFM FILE(';
043600160226         cmd = %trim(cmd) + %trim(wFlibF) + ')';
043700160226         IF  ExecuteCommand (cmd) = 0;
043800160226           open WFORMF0F;
043900160226         ELSE;
044000160226           ErrMessage  = *on;
044100160226           ErrGenerico = *on;
044200160226           W01msg = 'Errore nell''esecuzione della funzione avvisare ICT';
044300160226           leavesr;
044400160226         ENDIF;
044500160226
044600160226       ENDSR;
044700160226
044800160226       //--------------------------------------------------------------
044900160226       //?Elaborazione.
045000160226       //--------------------------------------------------------------
045100160226       BEGSR Elabora;
045200160229
045300160229       //?Leggo gli ORM --> Testata
045400160229         exec sql
045500160229         DECLARE ORM cursor for
045600160229         SELECT * from FNORM00F
045700160229         WHERE ORMpor = :wpor and ORMdar = :wdar;
045800160229
045900160229       //?Apro il cursore
046000160229         exec sql
046100160229         OPEN ORM;
046200160229         IF  sqlcode < 0;
046300160229           wEnD = *on;
046400160229         ENDIF;
046500160229
046600160229         DOW not wEnD;
046700160229           exec sql
046800160229           fetch next from ORM into :FNORM00F;
046900160229           IF  sqlcod = 100 or sqlcod < 0;
047000160229             wEnD = *on;
047100160229             leave;
047200160229           ENDIF;
047300160229
047400160229         //?Scrivo WrkF
047500160229           exsr WrtFileT;
047600160229
047700160229         //?Leggo e scrivo le fasi ORM
047800160229           setll (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORF01L;
047900160229           reade (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORF01L;
048000160229           DOW  not %eof(FNORF01L);
048100160229             exsr WrtFileF;
048200160229             reade (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORF01L;
048300160229           ENDDO;
048400160229
048500160229         ENDDO;
048600160229
048700160229         exec sql close ORM;
048800160226
048900160226       ENDSR;
049000160226
049100160226       //--------------------------------------------------------------
049200160229       //?Scrivo WrkF Testate
049300160226       //--------------------------------------------------------------
049400160229       BEGSR WrtFileT;
049500160226
049600160226         clear WFORMT;
049700160226         wORMpoe = ORMpoe;
049800160226         wORMnsr = ORMnsr;
049900160226         wORMnor = ORMnor;
050000160226         wORMnrv = ORMnrv;
050100160226         wORMtor = ORMtor;
050200160226         wORMtco = ORMtco;
050300160226         wORMdao = ORMdao;
050400160226         wORMoao = ORMoao;
050500160226         wORMcor = ORMcor;
050600160226         wORMrso = ORMrso;
050700160226         wORMino = ORMino;
050800160226         wORMcao = ORMcao;
050900160226         wORMloo = ORMloo;
051000160226         wORMpro = ORMpro;
051100160226         wORMnao = ORMnao;
051200160226         wORMcra = ORMcra;
051300160226         wORMrsr = ORMrsr;
051400160226         wORMinr = ORMinr;
051500160226         wORMcar = ORMcar;
051600160226         wORMlor = ORMlor;
051700160226         wORMprr = ORMprr;
051800160226         wORMnar = ORMnar;
051900160226         wORMrer = ORMrer;
052000160226         wORMter = ORMter;
052100160226         wORMcrc = ORMcrc;
052200160226         wORMrsc = ORMrsc;
052300160226         wORMinc = ORMinc;
052400160226         wORMcac = ORMcac;
052500160226         wORMloc = ORMloc;
052600160226         wORMprc = ORMprc;
052700160226         wORMnac = ORMnac;
052800160226         wORMffd = ORMffd;
052900160226         wORMdar = ORMdar;
053000160226         wORMorr = ORMorr;
053100160226         wORMrmp = ORMrmp;
053200160226         wORMnam = ORMnam;
053300160226         wORMncl = ORMncl;
053400160226         wORMpkg = ORMpkg;
053500160226         wORMvlm = ORMvlm;
053600160226         wORMbnc = ORMbnc;
053700160226         wORMblc = ORMblc;
053800160226         wORMatt = ORMatt;
053900160226         wORMmtc = ORMmtc;
054000160226         wORMpag = ORMpag;
054100160226         wORMksc = ORMksc;
054200160226         wORMctr = ORMctr;
054300160226         wORMpor = ORMpor;
054400160226         wORMzor = ORMzor;
054500160226         wORMpoc = ORMpoc;
054600160226         wORMno1 = ORMno1;
054700160226         wORMno2 = ORMno2;
054800160226         wORMddt = ORMddt;
054900160226         wORMcst = ORMcst;
055000160226         wORMvcs = ORMvcs;
055100160226         wORMfcs = ORMfcs;
055200160226         wORMdfo = ORMdfo;
055300160226         wORMofo = ORMofo;
055400160226         wORMfao = ORMfao;
055500160226         wORMsto = ORMsto;
055600160226         wORMdst = ORMdst;
055700160226         wORMnpg = ORMnpg;
055800160226         wORMndc = ORMndc;
055900160226         wORMddc = ORMddc;
056000160226         wORMstp = ORMstp;
056100160226         wORMpos = ORMpos;
056200160226         wORMors = ORMors;
056300160226         wORMrfa = ORMrfa;
056400160226         wORMtap = ORMtap;
056500160226         wORMeti = ORMeti;
056600160226         wORMspi = ORMspi;
056700160226         wORMflo = ORMflo;
056800160226         wORMflt = ORMflt;
056900160226         wORMpot = ORMpot;
057000160226         wORMdtt = ORMdtt;
057100160226         write WFORMT;
057200160226
057300160229       ENDSR;
057400160229
057500160229       //--------------------------------------------------------------
057600160229       //?Scrivo WrkF Fasi.
057700160229       //--------------------------------------------------------------
057800160229       BEGSR WrtFileF;
057900160229
058000160226         clear WFORMF;
058100160226         wORFpoe = ORFpoe;
058200160226         wORFnsr = ORFnsr;
058300160226         wORFnor = ORFnor;
058400160226         wORFnrv = ORFnrv;
058500160226         wORFpog = ORFpog;
058600160226         wORFdae = ORFdae;
058700160226         wORFore = ORFore;
058800160226         wORFfar = ORFfar;
058900160226         wORFcar = ORFcar;
059000160226         wORFpue = ORFpue;
059100160226         wORFfgs = ORFfgs;
059200160226         wORFndc = ORFndc;
059300160226         wORFddc = ORFddc;
059400160226         wORFdfc = ORFdfc;
059500160226         wORFflo = ORFflo;
059600160226         write WFORMF;
059700160226
059800160226       ENDSR;
059900160226
060000160226       //--------------------------------------------------------------
060100160226       //?Operazioni finali.                                           ?
060200160226       //--------------------------------------------------------------
060300160226       BEGSR RoutEnd;
060400160226
060500160226         *inLR = *on;
060600160226
060700160226       //?Esco dal programma
060800160226         return;
060900160226
061000160226       ENDSR;
061100160226
061200160226      /end-free
061300160226
061400160226** -- MSG ---------------------------------------*
061500160226Filiale Ritiro errata                             1
061600160226Data Ritiro errata                                2
