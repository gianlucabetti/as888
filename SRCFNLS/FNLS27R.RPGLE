000100090122      *PARMS dftactgrp(*no) actgrp(*caller) dbgview(*source)
000200090122       //--------------------------------------------------------------
000300090122       //?Stampa Documento di Ritiro (mod. unico A4)             ?
000400090122       //--------------------------------------------------------------
000500090122
000600100414       //--------------------------------------------------------------
000700100414       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800100414       //--------------------------------------------------------------
000900100414
001000100414     /*PRM dbgview(*source)
001100100414     /*END
001200100414
001300100414       //--------------------------------------------------------------
001400100414       //?Specifiche di controllo.                                     ?
001500100414       //--------------------------------------------------------------
001600100414
001700090122     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800090122     h dftactgrp(*no) actgrp(*caller)
001900090122
002000090122       //--------------------------------------------------------------
002100090122       //?Dichiarazione file.                                          ?
002200090122       //--------------------------------------------------------------
002300090122
002400100414       // -?Organigramma?
002500090122     fAZORG01L  if   e           k disk
002600090122
002700100414       // -?Tabelle?
002800090122     fTABEL00F  if   e           k disk
002900090122
003000100414       // -?Numeratori?
003100090122     fFLNUF01L  if   e           k disk
003200090122
003300100414       // -?Video?
003400090122     fFNLS27D   cf   e             workstn
003500090122     f                                     indds(IndDspF)
003600090122     f                                     infds(InfDspF)
003700090122
003800090122       //--------------------------------------------------------------
003900090122       //?Definizione costanti.                                        ?
004000090122       //--------------------------------------------------------------
004100090122
004200100414       // -?Codice numeratore?
004300090122     d c_CodNum        c                   const(020)
004400100414
004500090122     d c_PDC           c                   const('su Piano dei Conti')
004600090123
004700100414       // -?Comandi da eseguire?
004800090123     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004900100723     d                                     file(FNLSB9P) +
005000090123     d                                     usrdta(''Doc.Ritiro'') +
005100090123     d                                     share(*yes)')
005200090123     d c_CmdDltOvr     c                   const('DLTOVR +
005300100723     d                                     file(FNLSB9P)')
005400090122
005500100414       // -?Tasti funzionali a video?
005600090122     d c_F01           c                   const(x'31')
005700090122     d c_F02           c                   const(x'32')
005800090122     d c_F03           c                   const(x'33')
005900090122     d c_F05           c                   const(x'35')
006000090122     d c_F06           c                   const(x'36')
006100090122     d c_F07           c                   const(x'37')
006200090122     d c_F08           c                   const(x'38')
006300090122     d c_F09           c                   const(x'39')
006400090122     d c_F10           c                   const(x'3A')
006500090122     d c_F12           c                   const(x'3C')
006600090122     d c_F13           c                   const(x'B1')
006700090122     d c_F14           c                   const(x'B2')
006800090122     d c_F15           c                   const(x'B3')
006900090122     d c_F16           c                   const(x'B4')
007000090122     d c_F17           c                   const(x'B5')
007100090122     d c_F18           c                   const(x'B6')
007200090122     d c_F19           c                   const(x'B7')
007300090122     d c_F20           c                   const(x'B8')
007400090122     d c_F21           c                   const(x'B9')
007500090122     d c_F22           c                   const(x'BA')
007600090122     d c_F23           c                   const(x'BB')
007700090122     d c_F24           c                   const(x'BC')
007800090122     d c_Enter         c                   const(x'F1')
007900090122     d c_RollDown      c                   const(x'F4')
008000090122     d c_RollUp        c                   const(x'F5')
008100090122
008200090122       //--------------------------------------------------------------
008300090122       //?Definizione schiere.                                         ?
008400090122       //--------------------------------------------------------------
008500090122
008600100414       // -?Giorni validi?
008700090122     d $GG             s              1    dim(08)  ctdata  perrcd(08)
008800090122
008900100414       // -?Messaggi di errore?
009000090122     d $Msg            s             78    dim(45)  ctdata  perrcd(01)
009100090122
009200090122       //--------------------------------------------------------------
009300090122       //?Definizione aree dati.                                       ?
009400090122       //--------------------------------------------------------------
009500090122
009600100414       // -?Dati utente?
009700090122     d §AzUte        e ds                  extname(AZUTE00F)
009800090122     d                                     dtaara
009900090122     d §DatiUte      e ds                  extname(dDatiUte)
010000090122     d                                     dtaara
010100090122
010200090122       //--------------------------------------------------------------
010300090122       //?Definizione strutture dati.                                  ?
010400090122       //--------------------------------------------------------------
010500090122
010600100414       // -?Status ds?
010700090122     d Status         sds
010800090122     d  SDSpgm           *proc
010900090122
011000100414       // -?InfDS?
011100090122     d InfDspF         ds
011200090122     d   dsp_aid             369    369a
011300090122
011400100414       // -?Indicatori su DspF?
011500090122     d IndDspF         ds                  inz
011600100414         // -?Attributi di visualizzazione?
011700100414         //   ·?01  Abilita la Rag.Soc. per la ricerca alfabetica?
011800090122     d   NoProtMittC                   n   overlay(IndDspF : 01)
011900100414         //   ·?02  Abilita tutto il Codice Mittente per l'inserimento?
012000090122     d   NoProtMittD                   n   overlay(IndDspF : 02)
012100100414         //   ·?11  Protegge la provicia del destinatario?
012200090122     d   ProtectPRD                    n   overlay(IndDspF : 11)
012300100414         // -?Emissione messaggio di errore?
012400090122     d   ErrMessage                    n   overlay(IndDspF : 28)
012500100414         // -?Posizionamento cursore & segnalazione errore?
012600090122     d   PosCurDSP                     n   overlay(IndDspF : 40)
012700090122     d   PosCurCBO                     n   overlay(IndDspF : 41)
012800090122     d   PosCurTSP                     n   overlay(IndDspF : 42)
012900090122     d   PosCurFLC                     n   overlay(IndDspF : 62)
013000090122     d   PosCurCCM                     n   overlay(IndDspF : 61)
013100090122     d   PosCurRSM                     n   overlay(IndDspF : 63)
013200090122     d   PosCurCAM                     n   overlay(IndDspF : 64)
013300090122     d   PosCurLOM                     n   overlay(IndDspF : 65)
013400090122     d   PosCurPRM                     n   overlay(IndDspF : 66)
013500090122     d   PosCurNZM                     n   overlay(IndDspF : 67)
013600090122     d   PosCurRMO                     n   overlay(IndDspF : 58)
013700090122     d   PosCurCMO                     n   overlay(IndDspF : 59)
013800090122     d   PosCurNMO                     n   overlay(IndDspF : 60)
013900090122     d   PosCurDCR                     n   overlay(IndDspF : 44)
014000090122     d   PosCurOCR                     n   overlay(IndDspF : 45)
014100090122     d   PosCurRSD                     n   overlay(IndDspF : 69)
014200090122     d   PosCurCAD                     n   overlay(IndDspF : 70)
014300090122     d   PosCurLOD                     n   overlay(IndDspF : 71)
014400090122     d   PosCurPRD                     n   overlay(IndDspF : 72)
014500090122     d   PosCurNZD                     n   overlay(IndDspF : 73)
014600090122     d   PosCurGC1                     n   overlay(IndDspF : 56)
014700090122     d   PosCurGC2                     n   overlay(IndDspF : 57)
014800090122     d   PosCurTIC                     n   overlay(IndDspF : 51)
014900090122     d   PosCurCAS                     n   overlay(IndDspF : 52)
015000090122     d   PosCurVCA                     n   overlay(IndDspF : 53)
015100090122     d   PosCurIAS                     n   overlay(IndDspF : 75)
015200090122     d   PosCurVAS                     n   overlay(IndDspF : 76)
015300100414         // -?Riemissione videata?
015400090122     d   ErrGenerico                   n   overlay(IndDspF : 99)
015500090122
015600100414       // -?Parametri ricevuti?
015700090122     d KPJBA         e ds
015800090122
015900100414       // -?Parametri per Reperimento dati utente?
016000090122     d TIBS34ds      e ds
016100090122
016200100414       // -?Parametri x richiesta stampante per BOLLE ed ETICHETTE?
016300090122     d TRUL90ds      e ds                  inz
016400090122     d   D90rsb      e                     inz('L')
016500090122
016600100414       // -?Parametri per stampa Documento di Ritiro?
016700090122     d FNLSB5ds      e ds                  inz
016800090122
016900100414       // -?Descrizione 143 dell'ORGANIGRAMMA?
017000090122     d og143         e ds                  inz
017100090122
017200100414       // -?Parametri per TIBS02R - Gestione TNTBE00F?
017300090122     d TIBS02ds      e ds
017400100414       // -?Tab. "GED" = Gestionale: divise?
017500090122     d dGED          e ds                  inz
017600090122
017700100414       // -?Tab. "CV" = Divise e cambi?
017800090122     d dsCV          e ds
017900100414       // -?Tab. "1A" = Tipi incasso C/Assegno?
018000090122     d ds1A          e ds
018100100414       // -?Tab. "3A" = Codici bolla?
018200090122     d ds3A          e ds
018300100414       // -?Tab. "5E" = Tipi servizio?
018400090122     d ds5E          e ds
018500090122
018600100414       // -?Parametri per pgm. TIBS69R (decodifica cliente)?
018700090122     d TIBS69ds      e ds                  inz
018800090122     d ds_CNACO      e ds                  extname(CNACO00F) inz
018900090122     d ds_CNIND      e ds                  extname(CNIND00F) inz
019000090122     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
019100090122     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
019200090122
019300100414       // -?Parametri per controllo di un CAP?
019400090122     d FNLV13ds      e ds                  inz
019500090122     d TISI95ds      e ds                  inz
019600100414       // -?Parametri per controllo di un indirizzo completo senza CAP?
019700090122     d FNLV14ds      e ds                  inz
019800090122
019900100414       // -?Controllo tipo incasso c/assegno?
020000090122     d TRULTICds     e ds                  inz
020100100414       // -?Controllo C/Assegno?
020200090122     d TRUL21ds      e ds
020300100414       // -?Controllo Valore da Assicurare?
020400090122     d TRUL22ds      e ds
020500090122
020600100414       // -?Parametri per FNLV06R (creazione numeratori per fil.)?
020700090122     d FNLV06ds        ds                  inz
020800100414       //   ·?Anno?
020900090122     d  D06aas                 1      2  0 inz
021000100414       //   ·?Filiale?
021100090122     d  D06fil                 3      5  0 inz
021200100414       //   ·?Codice fil. elaboratore di PARFIL?
021300090122     d  D06fel                 6      8  0 inz
021400100414       //   ·?Codice Numeratore?
021500090122     d  D06cnu                 9     11    inz
021600100414       //   ·?Tipo fil.?
021700090122     d  D06ftf                12     12    inz
021800090122
021900100414       // -?Codice Cliente?
022000090122     d                 ds                  inz
022100090122     d   wFLC                         3s 0 inz
022200090122     d   wCCM                         4s 0 inz
022300090122     d ds_KSC                  1      7s 0 inz
022400090122
022500100414       // -?Parametri per controllo data e trasform. in giorni -XSRDA8-?
022600090122     d WLBdat          ds                  inz
022700090122     d  G02dat                 1      8  0 inz
022800090122     d  G02inv                 9     16  0 inz
022900090122     d  G02err                17     17    inz
023000090122     d  G02tgi                18     22  0 inz
023100090122
023200100414       // -?Ora consegna richiesta?
023300090122     d W1Cocr          ds             4    inz
023400090122     d  W1Chcr                 1      2  0 inz
023500090122     d  W1Cmcr                 3      4  0 inz
023600090122
023700090122       //--------------------------------------------------------------
023800090122       //?Definizione variabili globali.                               ?
023900090122       //--------------------------------------------------------------
024000090122
024100100414       // -?Flags booleani?
024200090122     d $Fine           s               n   inz
024300090122     d $InzD01         s               n   inz(*on)
024400090122     d $InzD02         s               n   inz(*on)
024500090122     d WinKG           s               n   inz
024600090122     d Win90           s               n   inz
024700090122     d $ErrTabel       s               n   inz
024800090122     d $ForzaCAS       s               n   inz
024900090122     d $ForzaIAS       s               n   inz
025000090122     d $OvrPrtF        s               n   inz
025100090122
025200100414       // -?Variabili per la gestione del video?
025300090122     d $Video          s              2    inz('D1')
025400090122
025500100414       // -?Campi per controllo "cambiamenti"?
025600090122     d sav_V1Ccas      s                   like(V1Ccas)  inz
025700090122     d sav_V1Cias      s                   like(V1Cias)  inz
025800090122     d SAVcmo          s                   like(V1Ccad)  inz
025900090122     d SAVfzv1         s                   like(E13fzv)  inz
026000090122     d SAVfzvM         s                   like(E13fzv)  inz
026100090122     d SAVfz1M         s                   like(E13fz1)  inz
026200090122     d SAVfz2M         s                   like(E13fz2)  inz
026300090122     d SAVfzvD         s                   like(E13fzv)  inz
026400090122     d SAVfz1D         s                   like(E13fz1)  inz
026500090122     d SAVfz2D         s                   like(E13fz2)  inz
026600090122     d SAVccm          s                   like(V1Cccm)  inz
026700090122     d SAVcad          s                   like(V1Ccad)  inz
026800090122     d SAVcam          s                   like(V1Ccad)  inz
026900090122     d SAVlod          s                   like(V1Clod)  inz
027000090122     d SAVlom          s                   like(V1Clom)  inz
027100090122
027200100414       // -?Campi di comodo?
027300090122     d wDate           s              8  0 inz
027400090122     d w0133           s             13  3 inz
027500090122     d w030a           s             30    inz
027600090122     d W1Cdsp          s                   like(V1Cdsp)  inz
027700090122     d W2Cdsp          s                   like(V1Cdsp)  inz
027800090122     d W1Cdcr          s                   like(DB0dcr)  inz
027900090122     d wVLT            s                   like(V1Cvca)  inz
028000090122     d wGC1            s                   like(V1Cgc1)  inz
028100090122     d wGC2            s                   like(V1Cgc2)  inz
028200090122     d wRMO            s                   like(V1Crmo)  inz
028300090122     d wCMO            s                   like(V1Ccmo)  inz
028400090122     d wNAR            s                   like(V1Cnmo)  inz
028500090122     d wCLI            s              1    inz
028600090122     d wGCx            s              1    inz
028700090122     d wERR            s              1    inz
028800090122
028900090122       //--------------------------------------------------------------
029000100414       //?Definizione prototipi procedure e relativi parametri.        ?
029100090122       //--------------------------------------------------------------
029200090122
029300100414       // -?Reperimento dati utente?
029400100722      /copy gaitrasrc/srcprotopr,TIBS34R
029500090122
029600100414       // -?Ricerca e controllo tabelle?
029700090122      /copy gaitrasrc/srcprotopr,TIBS02R
029800090122      /copy gaitrasrc/srcprotopr,X§TABER
029900090122
030000100414       // -?Controllo ed inversione date?
030100090122      /copy gaitrasrc/srcprotopr,XSRDA8
030200090122
030300100414       // -?Controllo/Decodifica cliente?
030400090122      /copy gaitrasrc/srcprotopr,TIBS69R
030500090122
030600100414       // -?Ricerca clienti per ragione sociale?
030700100414      /copy gaitrasrc/srcprotopr,XALFA3BRds
030800090122      /copy gaitrasrc/srcprotopr,XALFA3BR
030900100414
031000100414       // -?Stampa Documento di Ritiro?
031100100722     d fnlsB9r         pr                  extpgm('FNLSB9R')
031200100414     d  fnlsB5ds                           likeds(FNLSB5ds)
031300090122
031400100414       // -?Creazione numeratore?
031500090122     d fnlv06r         pr                  extpgm('FNLV06R')
031600090122     d  kpjba                              likeds(kpjba)
031700100414
031800100414       // -?Controllo tipo incasso c/assegno?
031900100414     d fnlv13r         pr                  extpgm('FNLV13R')
032000100414     d  kpjba                              likeds(kpjba)
032100100414     d  fnlv13ds                           likeds(FNLV13ds)
032200100414     d  tisi95ds                           likeds(TISI95ds)
032300100414
032400100414       // -?Controllo tipo incasso c/assegno?
032500100414     d fnlv14r         pr                  extpgm('FNLV14R')
032600100414     d  kpjba                              likeds(kpjba)
032700100414     d  fnlv14ds                           likeds(FNLV14ds)
032800090122
032900100414       // -?Controllo tipo incasso c/assegno?
033000090122     d trulTICr        pr                  extpgm('TRULTICR')
033100090122     d  trulTICds                          likeds(TRULTICds)
033200090122     d  ds1A                               likeds(ds1A)
033300090122
033400100414       // -?Controllo c/assegno?
033500090122     d trul21r         pr                  extpgm('TRUL21R')
033600090122     d  trul21ds                           likeds(TRUL21ds)
033700090122
033800100414       // -?Calcolo limiti importo da assicurare?
033900090122     d trul22r         pr                  extpgm('TRUL22R')
034000090122     d  trul22ds                           likeds(TRUL22ds)
034100090123
034200100414       // -?Richiesta stampanti?
034300090123     d trul90r         pr                  extpgm('TRUL90R')
034400090123     d  kpjba                              likeds(kpjba)
034500090123     d  trul90ds                           likeds(TRUL90ds)
034600090123
034700100414       // -?Esecuzione comando di sistema?
034800100722     d Qcmd            s           1024    varying    inz
034900100722      /copy gaitrasrc/srcprotopr,QCMDEXC
035000090122
035100090122       //--------------------------------------------------------------
035200090122       //?Definizione key-list.                                        ?
035300090122       //--------------------------------------------------------------
035400090122
035500100414       // -?File TABEL00F?
035600090122     d k03tabel00    e ds                  extname(TABEL00F : *key)
035700090122     d                                     inz
035800090122
035900100414       // -?File FLNUF01L?
036000090122     d k04flnuf01    e ds                  extname(FLNUF01L : *key)
036100090122     d                                     inz
036200090122
036300090122       //--------------------------------------------------------------
036400090122       //?M A I N - L I N E                                            ?
036500090122       //--------------------------------------------------------------
036600090122
036700090122     c     *Entry        plist
036800090122     c                   parm                    KPJBA
036900090122
037000090122      /free
037100090122
037200100414       // -?Operazioni iniziali?
037300100414       exsr sr_RoutInz;
037400090122
037500100414       // -?Gestione video?
037600090122       DOW  $Fine = *off;
037700090122         select;
037800100414           // -?Richiesta dei dati in stampa?
037900090122           when $Video = 'D1';
038000090122             exsr sr_GesD01;
038100100414           // -?Richiesta del numero copie?
038200090122           when $Video = 'D2';
038300090122             exsr sr_GesD02;
038400100414           // -?? ? ? ? ??
038500090122           other;
038600090122             $Fine = *on;
038700090122         endsl;
038800090122       ENDDO;
038900090122
039000100414       // -?Operazioni finali?
039100090122       exsr sr_RoutEnd;
039200090122
039300090122       //--------------------------------------------------------------
039400090122       //?Operazioni iniziali.                                         ?
039500090122       //--------------------------------------------------------------
039600090122       BEGSR  sr_RoutInz;
039700090122
039800090122         *inLR = *on;
039900090122
040000100414         // -?Richiesta stampante?
040100100414         reset  TRUL90ds;
040200090122         trul90r ( kpjba : TRUL90ds );
040300090122
040400090122         if  D90F3 = *on;
040500090122           $Fine = *on;
040600090122           leavesr;
040700090122         endif;
040800090122
040900100414         // -?Impostazione campi "fissi"?
041000090122         V1Cpgm = SDSpgm;
041100090122         TBLkut = 1;
041200090122         NUFaaa = *zero;
041300090122         NUFcnu = c_CodNum;
041400090122         NUFfl2 = *zero;
041500090122
041600100414         // -?Reperimento dati job?
041700090122         exsr  sr_DatiJob;
041800090122
041900100414         // -?Reperimento della data corrente (in formato aaaa/mm/gg)?
042000090122         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
042100090122
042200100414         // -?Aggancio tab."GED" per divisa della moneta di conto?
042300090122         clear  TIBS02ds;
042400090122         T02mod = 'C';
042500090122         T02sif = knsif;
042600090122         T02cod = 'GED';
042700090122         T02ke1 = '1';
042800090122         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
042900090122         if  T02err = *blank;
043000090122           dGED = T02uni;
043100090122         endif;
043200090122
043300100414         // -?Tabella CV della moneta di conto?
043400090122         TBLcod = 'CV';
043500090122         TBLkey = §GEDcn;
043600090122         exsr  sr_Tabel;
043700090122         if  NOT  $ErrTabel;
043800090122           dsCV = TBLuni;
043900090122         endif;
044000090122
044100100414         // -?Reperimento/Creazione numeratore?
044200090122         setll  %kds(k04flnuf01)  FLNUF;
044300090122         if  NOT %equal(FLNUF01L);
044400090122           clear  FNLV06ds;
044500090122           D06fil = DUTpou;
044600090122           D06fel = DUTtfp;
044700090122           D06cnu = %editc( c_CodNum : 'X' );
044800090122           D06ftf = 'P';
044900090122           kpjbu  = FNLV06ds;
045000090122           fnlv06r ( kpjba );
045100090122         endif;
045200090122
045300090122       ENDSR;
045400090122
045500090122       //--------------------------------------------------------------
045600090122       //?Reperimento Dati del job (Utente/Operativi).                 ?
045700090122       //--------------------------------------------------------------
045800090122       BEGSR  sr_DatiJob;
045900090122
046000090122         in(E) §AzUte;
046100090122         if NOT %error;
046200090122           in(E) §DatiUte;
046300090122         endif;
046400090122         if %error or RSut = *blanks;
046500090122           clear TIBS34ds;
046600090122           tibs34r ( tibs34ds );
046700090122           in §AzUte;
046800090122           in §DatiUte;
046900090122         endif;
047000090122
047100090122       ENDSR;
047200090122
047300090122       //--------------------------------------------------------------
047400090122       //?Gestione videata D01                                         ?
047500090122       //--------------------------------------------------------------
047600090122       BEGSR  sr_GesD01;
047700090122
047800100414         // -?Inizializzazione videata?
047900100414         if  $InzD01 = *on;
048000090122           exsr sr_InzD01;
048100090122           $InzD01 = *off;
048200090122         endif;
048300090122
048400100414         // -?Emissione videata?
048500090122         write LS27T01;
048600090122         exfmt LS27D01;
048700090122         reset ErrMessage;
048800090122         reset ErrGenerico;
048900090122         clear V1Dmsg;
049000090122
049100090122         SELECT;
049200090122
049300100414           // -?F3=Fine?
049400090122           WHEN  dsp_aid = c_F03;
049500090122             $Fine = *on;
049600090122
049700100414           // -?F7=Ricerca mittente?
049800090122           WHEN  dsp_aid = c_F07;
049900090122             NoProtMittC = *on;
050000090122             PosCurRSM   = *on;
050100090122             WinKG       = *on;
050200090122
050300100414           // -?Invio / F6=Conferma?
050400090122           OTHER;
050500090122             exsr sr_CtrD01;
050600090122             if  ErrGenerico = *off   and   dsp_aid = c_F06;
050700090122               $Video    = 'D2';
050800090122               $InzD02   = *on;
050900090122             endif;
051000090122
051100090122         ENDSL;
051200090122
051300090122       ENDSR;
051400090122
051500090122       //--------------------------------------------------------------
051600090122       //?Inizializzazione videata D01                                 ?
051700090122       //--------------------------------------------------------------
051800090122       BEGSR  sr_InzD01;
051900090122
052000090122         clear  LS27D01;
052100090122
052200100414         // -?Impostazione dei dati di default:?
052300100414         //   -?Fil. cliente?
052400090122         V1Cflc = DUTtfp;
052500100414         //   -?Divisa di conto gestionale nei campi divisa?
052600090122         V1Cvca = §GEDcn;
052700090122         V1Cvas = §GEDcn;
052800090122
052900090122       ENDSR;
053000090122
053100090122       //--------------------------------------------------------------
053200090122       //?Controllo dati immessi nella videata D01                     ?
053300090122       //--------------------------------------------------------------
053400090122       BEGSR  sr_CtrD01;
053500090122
053600090122         %subst(IndDspF : 28) = *off;
053700090122
053800090122         clear  W1Cdsp;
053900090122         clear  W2Cdsp;
054000090122         clear  W1Cdcr;
054100090122
054200100414         // -?DATA DOCUMENTO (Data Spedizione)?
054300100414         if  V1Cdsp >  *zero;
054400090122           clear  WLBdat;
054500090122           G02dat = V1Cdsp;
054600090122           xsrda8 ( WLBdat );
054700090122           if  G02err =  *on;
054800090122             ErrMessage  = *on;
054900090122             ErrGenerico = *on;
055000090122             PosCurDSP   = *on;
055100090122             V1Dmsg      = $Msg(01);
055200090122             leavesr;
055300090122           endif;
055400090122           V1Cdsp = G02dat;
055500090122           W1Cdsp = G02inv;
055600090122           W2Cdsp = G02inv;
055700090122         else;
055800090122           //W1Cdsp =  wDate;
055900090122           W2Cdsp = wDate;
056000090122         endif;
056100090122
056200100414         // -?CODICE BOLLA?
056300090122         clear  ds3A;
056400090122         if  V1Ccbo <> *blank;
056500090122           TBLcod =  '3A';
056600100414           // -?Ricerca?
056700090122           if  %scan ( '?' : V1Ccbo ) > *zero;
056800090122             exsr  sr_X§TAB;
056900090122             V1Ccbo = TBLkey;
057000090122             ErrGenerico = *on;
057100090122             PosCurCBO   = *on;
057200090122             leavesr;
057300090122           endif;
057400100414           // -?Controllo?
057500090122           TBLkey = V1Ccbo;
057600090122           exsr  sr_TABEL;
057700090122           if  $ErrTabel;
057800090122             ErrMessage  = *on;
057900090122             ErrGenerico = *on;
058000090122             PosCurCBO   = *on;
058100090122             V1Dmsg      = $Msg(02);
058200090122             leavesr;
058300090122           endif;
058400090122           ds3A = TBLuni;
058500100414           // -?Verifica se causale utilizzabile?
058600090122           if  §3Aucb =  'N';
058700090122             ErrMessage  = *on;
058800090122             ErrGenerico = *on;
058900090122             PosCurCBO   = *on;
059000090122             V1Dmsg      = $Msg(02);
059100090122             leavesr;
059200090122           endif;
059300090122         endif;
059400090122
059500100414         // -?TIPO SERVIZIO?
059600100806         clear  V1Dtsp;
059700090122         clear  ds5E;
059800090122         if  V1Ctsp <> *blank;
059900090122           TBLcod =  '5E';
060000090122           // - Ricerca
060100090122           if  %scan ( '?' : V1Ctsp ) > *zero;
060200090122             exsr  sr_X§TAB;
060300090122             V1Ctsp = TBLkey;
060400100414             //V1Dtsp = w030a;         //?(verrà decodificata dopo)?
060500090122             ErrGenerico = *on;
060600090122             PosCurTSP   = *on;
060700100414             //leavesr;                //?(meglio prosegua - x decod.)?
060800090122           endif;
060900100414           // ·?Controllo?
061000090122           TBLkey = V1Ctsp;
061100090122           exsr  sr_TABEL;
061200090122           if  $ErrTabel;
061300100806             V1Dtsp      = *all'? ';
061400090122             ErrMessage  = *on;
061500090122             ErrGenerico = *on;
061600090122             PosCurTSP   = *on;
061700090122             V1Dmsg      = $Msg(03);
061800090122             leavesr;
061900090122           endif;
062000090122           ds5E   = TBLuni;
062100090609           V1Dtsp = §5Ed08;
062200100414           // ·?Verifica se tipo servizio utilizzabile?
062300100414           //  ?in bollettazione?
062400090122           if  §5Efub <> 'S';
062500090122             ErrMessage  = *on;
062600090122             ErrGenerico = *on;
062700090122             PosCurTSP   = *on;
062800090122             V1Dmsg      = $Msg(03);
062900090122             leavesr;
063000090122           endif;
063100090122         endif;
063200100414
063300100414         // -?MITTENTE?
063400100414         // ·?Ricerca alfabetica se:?
063500100414         //  ?· è stato premuto F7         e?
063600100414         //  ?· il codice cliente è vuoto  e?
063700100414         //  ?· la ragione sociale è piena?
063800100414         IF  WinKG  =  *on     and
063900090122             V1Cccm =  *zero   and
064000090122             V1Crsm <> *blank;
064100090122           xpardut = RSut;
064200090122           xparkut = 1;
064300090122           xparrag = V1Crsm;
064400090122           xparkcc = DUTkci;
064500100414           xparsta = 9;          //?esclusione annullati?
064600090122           clear  xpardit;
064700090122           if  V1Cflc <> *zero;
064800090122             xparflr = %editc( V1Cflc : 'X' );
064900090122             chain  V1Cflc  AZORG;
065000090122             if  %found(AZORG01L);
065100090122               xpardit = ORGdit;
065200090122             endif;
065300090122           endif;
065400090122           xparnum = 1;
065500090122           clear xparkcm;
065600090122           clear xparksm;
065700090122           clear xparkdm;
065800090122           clear xparesci;
065900090122           clear xparerr;
066000090122           clear xpariva;
066100090122           clear xparcdf;
066200090122           callp XALFA3BR (xpardut : xparkut : xparrag :
066300090122                           xparkcc : xparsta : xparflr :
066400090122                           xpardit : xparnum : xparkcm :
066500090122                           xparksm : xparkdm : xparesci :
066600090122                           xparerr : xpariva : xparcdf);
066700090122           if  xparsta <> -1;
066800090122             ACOksc = %int( %subst( xparksm : 1 : 7 ));
066900090122             V1Cflc = %int( %subst( xparksm : 1 : 3 ));
067000090122             V1Cccm = %int( %subst( xparksm : 4 : 4 ));
067100090122             //V1Crsm = xparrag   ;
067200090122           endif;
067300090122           ErrGenerico = *on;
067400090122           PosCurCCM   = *on;
067500090122         ENDIF;
067600100414         // ·?Decodifica Mittente?
067700090122         if  not NoProtMittC   and   not NoProtMittD;
067800090122           clear  V1Crsm;
067900090122           clear  V1Cinm;
068000090122           clear  V1Ccam;
068100090122           clear  V1Clom;
068200090122           clear  V1Cprm;
068300090122           clear  V1Cnzm;
068400090122         endif;
068500090122         clear  ds_KSC;
068600090122         if  V1Cccm <> *zero;
068700090122           wFLC = V1Cflc;
068800090122           wCCM = V1Cccm;
068900090122           exsr  sr_CtrCCM;
069000090122           if  ErrGenerico;
069100090122             leavesr;
069200090122           endif;
069300090122         else;
069400090122           NoProtMittD =  *off;
069500090122         endif;
069600100414         // -?*in02 ON => controllo il mittente per esteso?
069700100414         if  NoProtMittD;
069800090122           clear  FNLV14ds;
069900090122           I14rsc = V1Crsm;
070000090122           I14ind = V1Cinm;
070100090122           E14loc = V1Clom;
070200090122           E14prv = V1Cprm;
070300090122           E14cap = V1Ccam;
070400090122           E14nar = V1Cnzm;
070500090122           Wcli   = 'M';
070600090122           exsr  sr_CtrIND;
070700090122           V1Clom = E14loc;
070800090122           V1Cprm = E14prv;
070900090122           V1Ccam = E14cap;
071000090122           V1Cnzm = E14nar;
071100090122           if  ErrMessage   or   ErrGenerico;
071200090122             leavesr;
071300090122           endif;
071400090122         endif;
071500090122
071600100414         // -?MITTENTE ORIGINALE?
071700090122         wRMO   = V1Crmo;
071800090122         wCMO   = V1Ccmo;
071900090122         wNAR   = V1Cnmo;
072000090122         exsr  sr_CtrRMO;
072100090122         V1Cnmo = wNAR;
072200090122         if  ErrGenerico;
072300090122           leavesr;
072400090122         endif;
072500090122
072600100414         // -?DATA CONSEGNA RICHIESTA?
072700090122         IF  V1Cdcr <> *zero;
072800090122           clear  WLBdat;
072900090122           G02dat = V1Cdcr;
073000090122           xsrda8 ( WLBdat );
073100090122           if  G02err =  *on;
073200090122             ErrMessage  = *on;
073300090122             ErrGenerico = *on;
073400090122             PosCurDCR   = *on;
073500090122             V1Dmsg      = $Msg(04);
073600090122             leavesr;
073700090122           endif;
073800090122           V1Cdcr = G02dat;
073900090122           W1Cdcr = G02inv;
074000090122         else;
074100090122           //W1Cdsp = wDate;
074200090122           W2Cdsp = wDate;
074300090122         endif;
074400090122
074500100414         // -?ORA CONSEGNA RICHIESTA?
074600090122         W1Cocr = %editc( V1Cocr : 'X' );
074700090122         if  W1Chcr < *zero   or   W1Chcr > 24;
074800090122           ErrMessage  = *on;
074900090122           ErrGenerico = *on;
075000090122           PosCurOCR   = *on;
075100090122           V1Dmsg      = $Msg(05);
075200090122           leavesr;
075300090122         endif;
075400090122         if  W1Cmcr < *zero   or   W1Cmcr > 59;
075500090122           ErrMessage  = *on;
075600090122           ErrGenerico = *on;
075700090122           PosCurOCR   = *on;
075800090122           V1Dmsg      = $Msg(06);
075900090122           leavesr;
076000090122         endif;
076100100414         // ·?Se immesso TIPO richieste anche DATA e/o ORA?
076200100414         if  V1Ctcr <> *blank   and
076300090122             V1Cdcr =  *zero    and   V1Cocr =  *zero;
076400090122           ErrMessage  = *on;
076500090122           ErrGenerico = *on;
076600090122           PosCurDCR   = *on;
076700090122           V1Dmsg      = $Msg(07);
076800090122           leavesr;
076900090122         endif;
077000090122
077100100414         // -?DESTINATARIO (se immesso)?
077200090122         IF  V1Crsd <> *blank   or
077300090122             V1Cind <> *blank   or
077400090122             V1Ccad <> *blank   or
077500090122             V1Clod <> *blank   or
077600090122             V1Cprd <> *blank   or
077700090122             V1Cnzd <> *blank;
077800090122           clear  FNLV14ds;
077900090122           I14rsc = V1Crsd;
078000090122           I14rs2 = %subst( V1Crsd : %size(I14rsc)+1 );
078100090122           I14ind = V1Cind;
078200090122           E14loc = V1Clod;
078300090122           E14prv = V1Cprd;
078400090122           E14cap = V1Ccad;
078500090122           E14nar = V1Cnzd;
078600090122           Wcli   = 'D';
078700090122           exsr  sr_CtrIND;
078800090122           V1Clod = E14loc;
078900090122           V1Cprd = E14prv;
079000090122           V1Ccad = E14cap;
079100090122           V1Cnzd = E14nar;
079200090122           if  ErrMessage   or   ErrGenerico;
079300090122             leavesr;
079400090122           endif;
079500090122         ENDIF;
079600100414         // ·?Immettere il cod.mittente e/o la rag.soc. destinatario?
079700100414         if  V1Crsm = *blank   and   V1Crsd = *blank;
079800090122           ErrMessage  = *on;
079900090122           ErrGenerico = *on;
080000090122           PosCurCCM   = *on;
080100090122           V1Dmsg      = $Msg(25);
080200090122           leavesr;
080300090122         endif;
080400090122
080500100414         // -?GIORNI DI CHIUSURA?
080600090122         wGC1   = V1Cgc1;
080700090122         wGC2   = V1Cgc2;
080800090122         exsr  sr_CtrGCH;
080900090122         if  ErrGenerico;
081000090122           leavesr;
081100090122         endif;
081200090122
081300100414         // -?CONTRASSEGNO?
081400100414         // ·?Controllo se previsto dal codice bolla?
081500090122         IF  V1Ccbo <> *blank   and   §3Afca = *zero;
081600090122           if  V1Ctic <> *blank;
081700090122             ErrMessage  = *on;
081800090122             ErrGenerico = *on;
081900090122             PosCurTIC   = *on;
082000090122             V1Dmsg      = $Msg(08);
082100090122             leavesr;
082200090122           endif;
082300090122           if  V1Ccas <> *zero;
082400090122             ErrMessage  = *on;
082500090122             ErrGenerico = *on;
082600090122             PosCurCAS   = *on;
082700090122             V1Dmsg      = $Msg(09);
082800090122             leavesr;
082900090122           endif;
083000090122         ENDIF;
083100090122
083200100414         // -?TIPO INCASSO C/ASSEGNO e relativa divisa?
083300090122         clear  trulTICds;
083400090122         clear  ds1A;
083500090122         ITICtbo = 'P';
083600090122         ITICnzd = V1Cnzd;
083700090122         ITICdsp = W2Cdsp;
083800090122         ITICccm = ds_ksc;
083900090122         ITICtic = V1Ctic;
084000090122         ITICdiv = V1Cvca;
084100090122         ITICcas = V1Ccas;
084200090122         ITICpgm = SDSpgm;
084300090122         trulTICr ( TRULTICds : ds1A );
084400090122         select;
084500090122           when  OTICric = 'T';
084600090122             V1Ctic      = OTICtic;
084700090122             ErrGenerico = *on;
084800090122             PosCurTIC   = *on;
084900090122             leavesr;
085000090122           when  OTICric = 'D';
085100090122             V1Cvca      = OTICdiv;
085200090122             ErrGenerico = *on;
085300090122             PosCurVCA   = *on;
085400090122             leavesr;
085500100414           // ·?Errore tipo incasso?
085600090122           when  OTICerr = 'T';
085700090122             ErrMessage  = *on;
085800090122             ErrGenerico = *on;
085900090122             PosCurTIC   = *on;
086000090122             V1Dmsg      = OTICmsg;
086100090122             leavesr;
086200100414           // ·?Errore divisa?
086300090122           when  OTICerr = 'T';
086400090122             ErrMessage  = *on;
086500090122             ErrGenerico = *on;
086600090122             PosCurVCA   = *on;
086700090122             V1Dmsg      = OTICmsg;
086800090122             leavesr;
086900090122         endsl;
087000090122
087100100414         // -?Controllo importo?
087200100414         //   ·?pulisco il flag x la forzatura c/assegno forzabile?
087300100414         if  V1Ccas <> sav_V1Ccas;
087400090122           reset  $ForzaCAS;
087500090122         endif;
087600100414         //   ·?controllo il c/assegno tramite il TRUL21R?
087700090122         IF  V1Ccas <> *zero;
087800090122           sav_V1Ccas = V1Ccas;
087900090122           clear  TRUL21ds;
088000090122           I21tla = 'L';
088100090122           I21cbo = V1Ccbo;
088200090122           I21tsp = V1Ctsp;
088300090122           I21lnp = DUTpou;
088400090122           I21nzm = V1Cnzm;
088500090122           I21nzd = V1Cnzd;
088600090122           I21ksc = (V1Cflc * 10000) + V1Cccm;
088700090122           //I21ctr = %editc( V1Cncp : 'X' );
088800090122           I21tic = V1Ctic;
088900090122           I21imp = V1Ccas;
089000090122           I21div = V1Cvca;
089100090122           I21ute = knmus;
089200090122           I21pgm = SDSpgm;
089300090122           trul21r ( trul21ds );
089400090122           select;
089500100414             //?· controllo che non sia minore del limite previsto?
089600090122             when  O21fmn <> *blank;
089700090122               ErrMessage  = *on;
089800090122               ErrGenerico = *on;
089900090122               PosCurCAS   = *on;
090000090122               V1Dmsg      = $Msg(29);
090100090122               %subst(V1Dmsg : 31 : 14) = (%editc(O21lmn : '4'));
090200090122               leavesr;
090300100414             //?· controllo che non sia maggiore del limite previsto?
090400100414             //?  non forzabile?
090500090122             when  O21fx2 <> *blank;
090600090122               ErrMessage  = *on;
090700090122               ErrGenerico = *on;
090800090122               PosCurCAS   = *on;
090900090122               V1Dmsg      = $Msg(28);
091000090122               %subst(V1Dmsg : 47 : 14) = (%editc(O21lx2 : '4'));
091100090122               leavesr;
091200100414             //?· controllo che non sia maggiore del limite per bancari?
091300100414             //  ?Bartolini non abilitati da sede?
091400100414             //  ?non forzabile?
091500090122             when  O21fb  <> *blank;
091600090122               ErrMessage  = *on;
091700090122               ErrGenerico = *on;
091800090122               PosCurCAS   = *on;
091900090122               V1Dmsg      = $Msg(34);
092000090122               %subst(V1Dmsg : 26 : 14) = (%editc(O21lb  : '4'));
092100090122               leavesr;
092200100414             //?· controllo che non sia maggiore del limite previsto?
092300100414             //  ?forzabile?
092400090122             when  O21fx1 <> *blank   and   $ForzaCAS = *off;
092500090122               $ForzaCAS   = *on;
092600090122               ErrMessage  = *on;
092700090122               ErrGenerico = *on;
092800090122               PosCurCAS   = *on;
092900090122               V1Dmsg      = $Msg(30);
093000090122               %subst(V1Dmsg : 24 : 14) = (%editc(O21lx1 : '4'));
093100090122               leavesr;
093200090122           endsl;
093300090122         ENDIF;
093400090122
093500100414         // -?IMPORTO DA ASSICURARE e relativa divisa?
093600100414         // ·?se immesso importo da assicurare è obbligatorio?
093700100414         //  ?immettere anche la divisa?
093800090122         if  V1Cias > *zero   and   V1Cvas = *blank;
093900090122           ErrMessage  = *on;
094000090122           ErrGenerico = *on;
094100090122           PosCurVAS   = *on;
094200090122           V1Dmsg      = $Msg(13);
094300090122           leavesr;
094400090122         endif;
094500100414         // ·?Controllo divisa?
094600090122         wVLT   = V1Cvas;
094700090122         w0133  = V1Cias;
094800090122         exsr  sr_CtrVLT;
094900090122         V1Cvas = wVLT;
095000090122         if ErrGenerico;
095100090122           PosCurVAS   = *on;
095200090122           leavesr;
095300090122         endif;
095400100414         // -?controllo scadenza divisa?
095500090122         if  W2Cdsp > §CVdli   and   V1Cias <> *zero;
095600090122           ErrMessage  = *on;
095700090122           ErrGenerico = *on;
095800090122           PosCurVAS   = *on;
095900090122           V1Dmsg      = $Msg(14);
096000090122           leavesr;
096100090122         endif;
096200100414         // ·?controllo importo?
096300100414         //  ?· pulisce il flag x la forzatura importo da assicurare?
096400100414         //    ?forzabile?
096500090122         if  V1Cias <> sav_V1Cias;
096600090122           reset  $ForzaIAS;
096700090122         endif;
096800100414         //  ?· controllo l'importo da assicurare tramite il TRUL21R?
096900090122         IF  V1Cias <> *zero;
097000090122           sav_V1Cias = V1Cias;
097100090122           clear  TRUL22ds;
097200090122           I22tla = 'L';
097300090122           I22cbo = V1Ccbo;
097400090122           I22tsp = V1Ctsp;
097500090122           I22lnp = DUTpou;
097600090122           I22nzm = V1Cnzm;
097700090122           I22nzd = V1Cnzd;
097800090122           I22ksc = (V1Cflc * 10000) + V1Cccm;
097900090122           I22ctr = %editc( V1Cncp : 'X' );
098000090122           I22imp = V1Cias;
098100090122           I22div = V1Cvas;
098200090122           I22ute = knmus;
098300090122           I22pgm = SDSpgm;
098400090122           trul22r ( TRUL22ds );
098500090122           select;
098600100414             //?· Con tutti i flag impostati non si può immettere?
098700100414             //  ?l'importo da assicurare?
098800090122             when  O22fmn <> *blank   and
098900090122                   O22fx1 <> *blank   and   O22fx2 <> *blank;
099000090122               ErrMessage  = *on;
099100090122               ErrGenerico = *on;
099200090122               PosCurIAS   = *on;
099300090122               V1Dmsg      = $Msg(31);
099400090122               leavesr;
099500100414             //?· controllo che non sia minore del limite previsto?
099600090122             when  O22fmn <> *blank;
099700090122               ErrMessage  = *on;
099800090122               ErrGenerico = *on;
099900090122               PosCurIAS   = *on;
100000090122               V1Dmsg      = $Msg(32);
100100090122               leavesr;
100200100414             //?· controllo che non sia maggiore del limite previsto?
100300100414             //  ?non forzabile?
100400090122             when  O22fx2 <> *blank;
100500090122               ErrMessage  = *on;
100600090122               ErrGenerico = *on;
100700090122               PosCurIAS   = *on;
100800090122               V1Dmsg      = $Msg(26);
100900090122               %subst(V1Dmsg : 38 : 14) = (%editc(o22lx2 : '4'));
101000090122               leavesr;
101100100414             //?· controllo che non sia maggiore del limite previsto?
101200100414             //  ?forzabile?
101300090122             when  O22fx1 <> *blank   and   $ForzaIAS = *blank;
101400090122               $Forzaias = *on;
101500090122               ErrMessage  = *on;
101600090122               ErrGenerico = *on;
101700090122               PosCurIAS   = *on;
101800090122               V1Dmsg      = $Msg(33);
101900090122               %subst(V1Dmsg : 28 : 14) = (%editc(o22lx1 : '4'));
102000090122               leavesr;
102100090122           endsl;
102200090122         ENDIF;
102300090122
102400090122       ENDSR;
102500090122
102600090122       //--------------------------------------------------------------
102700090122       //?Gestione videata D02                                         ?
102800090122       //--------------------------------------------------------------
102900090122       BEGSR  sr_GesD02;
103000090122
103100100414         // -?Inizializzazione videata?
103200090122         if  $InzD02 = *on;
103300090122           exsr sr_InzD02;
103400090122           $InzD02 = *off;
103500090122         endif;
103600090122
103700100414         // -?Emissione videata?
103800090122         exfmt LS27D02;
103900090122         reset ErrMessage;
104000090122         reset ErrGenerico;
104100090122         clear V1Dmsg;
104200090122
104300090122         SELECT;
104400090122
104500100414           // -?F12=Ritorno?
104600090122           WHEN  dsp_aid = c_F12;
104700090122             reset  $Video;
104800090122
104900100414           // -?Invio?
105000090122           OTHER;
105100090122             exsr sr_StampaDdR;
105200100414             // -?Ritorno alla videata precedente?
105300090122             reset  $Video;
105400090122             reset  $InzD01;
105500090122
105600090122         ENDSL;
105700090122
105800090122       ENDSR;
105900090122
106000090122       //--------------------------------------------------------------
106100090122       //?Inizializzazione videata D02                                 ?
106200090122       //--------------------------------------------------------------
106300090122       BEGSR  sr_InzD02;
106400090122
106500090122         clear  LS27D02;
106600090122
106700100414         // -?Impostazione dei dati di default:?
106800100414         //   -?Num. copie?
106900090122         V1Cncp = 1;
107000100414         //   -?Stampa decodifica mittente?
107100090122         V1Cstm = 'S';
107200090122
107300090122       ENDSR;
107400090122
107500090122       //--------------------------------------------------------------
107600090122       //?Richiamo pgm di ricerca tabelle (TABEL00F)                   ?
107700090122       //--------------------------------------------------------------
107800090122       BEGSR  sr_X§TAB;
107900090122
108000090122         clear  TBLkey;
108100090122         clear  w030a;
108200090122         X§TABER ( TBLkut : TBLcod : TBLkey : w030a) ;
108300090122
108400090122       ENDSR;
108500090122
108600090122       //--------------------------------------------------------------
108700090122       //?Controllo tabelle (TABEL00F)                                 ?
108800090122       //--------------------------------------------------------------
108900090122       BEGSR  sr_TABEL;
109000090122
109100090122         reset  $ErrTabel;
109200090122
109300090122         chain  %kds(k03tabel00)  TABEL;
109400090122
109500090122         if  NOT %found(TABEL00F)   or   TBLflg <> *blank;
109600090122           $ErrTabel = *on;
109700090122         endif;
109800090122
109900090122       ENDSR;
110000090122
110100090122       //--------------------------------------------------------------
110200090122       //?Controllo divisa                                             ?
110300090122       //--------------------------------------------------------------
110400090122       BEGSR  sr_CtrVLT;
110500090122
110600090122         TBLcod = 'CV';
110700090122
110800100414         // -?Ricerca?
110900100414         if  %scan ( '?' : wVLT ) > *zero;
111000100414           exsr  sr_X§TAB;
111100100414           wVLT  = TBLkey;
111200100414           ErrGenerico = *on;
111300100414           leavesr;
111400100414         endif;
111500090122
111600100414         // -?Controllo?
111700100414         TBLkey = wVLT;
111800100414         exsr  sr_TABEL;
111900100414         if  $ErrTabel;
112000100414           ErrMessage  = *on;
112100100414           ErrGenerico = *on;
112200100414           V1Dmsg      = $Msg(10);
112300100414           leavesr;
112400100414         endif;
112500090122
112600100414         dsCV = TBLuni;
112700100414
112800100414         // -?Controllo se ammessi importi decimali?
112900100414         if  §CVfdc <> 'S';
113000100414
113100100414           if  (w0133 - %int(w0133)) <> *zero;
113200100414             V1Dmsg = $Msg(11);
113300100414           endif;
113400100414
113500100414         else;
113600100414
113700100414         // -?Verifico se i decimali inseriti sono quelli ammessi?
113800100414           select;
113900100414             when  §CVdec = 3;
114000100414             when  §CVdec = 2;
114100100414               if  %subst( %editc( w0133 : '3') : 13 : 1 ) > '0';
114200100414                 V1Dmsg = $Msg(12);
114300100414               endif;
114400100414             when  §CVdec = 1;
114500100414               if  %subst( %editc( w0133 : '3') : 12 : 2 ) > '00';
114600100414                 V1Dmsg = $Msg(12);
114700100414               endif;
114800100414             when  §CVdec = *zero;
114900100414               if  %subst( %editc( w0133 : '3') : 11 : 3 ) > '000';
115000100414                 V1Dmsg = $Msg(12);
115100100414               endif;
115200100414           endsl;
115300100414
115400100414         endif;
115500090122
115600090122           if  V1Dmsg <> *blank;
115700090122             ErrMessage  = *on;
115800090122             ErrGenerico = *on;
115900090122             leavesr;
116000090122           endif;
116100090122
116200090122         ENDSR;
116300090122
116400090122       //--------------------------------------------------------------
116500090122       //?Controllo giorni di chiusura                                 ?
116600090122       //--------------------------------------------------------------
116700090122       BEGSR  sr_CtrGCH;
116800090122
116900100414         // -?1° GIORNO CHIUSURA?
117000100414         IF  wGC1 <> *blank;
117100090122
117200100414           // -?giorno settimanale?
117300090122           eval  wGCx = wGC1;
117400090122           if  %lookup( wGCx : $GG ) = *zero;
117500090122             ErrMessage  = *on;
117600090122             ErrGenerico = *on;
117700090122             PosCurGC1   = *on;
117800090122             V1Dmsg      = $Msg(18);
117900090122             leavesr;
118000090122           endif;
118100090122
118200100414           // -?mattino/pomeriggio?
118300090122           evalr wGCx = wGC1;
118400090122           if  wGCx <> *blank   and
118500090122               wGCx <> 'M'      and   wGCx <> 'P';
118600090122             ErrMessage  = *on;
118700090122             ErrGenerico = *on;
118800090122             PosCurGC1   = *on;
118900090122             V1Dmsg      = $Msg(18);
119000090122             leavesr;
119100090122           endif;
119200090122
119300090122         ENDIF;
119400090122
119500100414         // -?2° GIORNO CHIUSURA?
119600090122         IF  wGC2 <> *blank;
119700090122
119800100414           // -?giorno settimanale?
119900090122           eval  wGCx = wGC2;
120000090122           if  %lookup( wGCx : $GG ) = *zero;
120100090122             ErrMessage  = *on;
120200090122             ErrGenerico = *on;
120300090122             PosCurGC2   = *on;
120400090122             V1Dmsg      = $Msg(18);
120500090122             leavesr;
120600090122           endif;
120700090122
120800100414           // -?mattino/pomeriggio?
120900090122           evalr wGCx = wGC2;
121000090122           if  wGCx <> *blank   and
121100090122               wGCx <> 'M'      and   wGCx <> 'P';
121200090122             ErrMessage  = *on;
121300090122             ErrGenerico = *on;
121400090122             PosCurGC2   = *on;
121500090122             V1Dmsg      = $Msg(18);
121600090122             leavesr;
121700090122           endif;
121800090122
121900100414           // -?I giorni di chiusura NON possono essere uguali?
122000100414           if  wGC1 = wGC2;
122100090122             ErrMessage  = *on;
122200090122             ErrGenerico = *on;
122300090122             PosCurGC2   = *on;
122400090122             V1Dmsg      = $Msg(18);
122500090122             leavesr;
122600090122           endif;
122700090122
122800090122         ENDIF;
122900090122
123000100414         // -?NON possono essere entrambi ' P' o ' M'?
123100090122         if  (wGC1 = ' M'   or   wGC1 = ' P')   and
123200090122             (wGC2 = ' M'   or   wGC2 = ' P');
123300090122           ErrMessage  = *on;
123400090122           ErrGenerico = *on;
123500090122           PosCurGC2   = *on;
123600090122           V1Dmsg      = $Msg(18);
123700090122           leavesr;
123800090122         endif;
123900090122
124000090122       ENDSR;
124100090122
124200090122       //--------------------------------------------------------------
124300090122       //?Controllo riferimento al mittente originale                  ?
124400090122       //--------------------------------------------------------------
124500090122       BEGSR  sr_CtrRMO;
124600090122
124700100414         // -?Ragione sociale obbligatoria se immesso il comune?
124800100414         if  wCMO <> *blank   and   wRMO = *blank;
124900090122           ErrMessage  = *on;
125000090122           ErrGenerico = *on;
125100090122           PosCurRMO   = *on;
125200090122           V1Dmsg      = $Msg(19);
125300090122           leavesr;
125400090122         endif;
125500090122
125600100414         // -?Ragione sociale obbligatoria e comune obbligatori?
125700100414         //  ?se immessa la nazione?
125800090122         if  wNAR <> *blank   and
125900090122            (wCMO =  *blank   or    wRMO = *blank);
126000090122           ErrMessage  = *on;
126100090122           ErrGenerico = *on;
126200090122           PosCurRMO   = *on;
126300090122           V1Dmsg      = $Msg(19);
126400090122           leavesr;
126500090122         endif;
126600090122
126700100414         // -?Controllo nazione mittente originale?
126800090122         if  wNAR <> *blank;
126900090122           exsr  sr_CtrNAR;
127000090122           if  ErrGenerico;
127100090122             leavesr;
127200090122           endif;
127300090122         endif;
127400090122
127500100414         // -?Controllo CAP mittente originale?
127600100414         IF  wCMO <> *blank;
127700090122
127800100414           //?· CAP con viario => errore forzabile?
127900090122           if  SAVcmo <> wCMO;
128000090122             clear  SAVfzv1;
128100090122             SAVcmo =  wCMO;
128200090122           endif;
128300090122
128400090122           clear  TISI95ds;
128500090122           clear  FNLV13ds;
128600090122           I95nar = wNAR;
128700090122           I95cap = wCMO;
128800090122           I95tcn = '3';
128900100414           //?· NON posso dare errore per affidabilità = 0 perchè non?
129000100414           //  ?ho la provincia?
129100090122           I13cnv = 'S';
129200090122           I13szv = 'S';
129300090122           E13fzv = SAVfzv1;
129400090122           fnlv13r ( kpjba : FNLV13ds : TISI95ds );
129500100414           // ·?Flag forzatura?
129600090122           SAVfzv1 = E13fzv;
129700090122
129800100414           // ·?Se CAP non trovato => errore?
129900090122           if  O13err <> *blank;
130000090122             ErrMessage  = *on;
130100090122             ErrGenerico = *on;
130200090122             PosCurCAS   = *on;
130300090122             V1Dmsg      = O13msg;
130400090122             leavesr;
130500090122           endif;
130600090122
130700090122         ENDIF;
130800090122
130900090122       ENDSR;
131000090122
131100090122       //--------------------------------------------------------------
131200090122       //?Controllo nazione e partita Iva                              ?
131300090122       //--------------------------------------------------------------
131400090122       BEGSR  sr_CtrNAR;
131500090122
131600090122         TBLcod = '15';
131700090122
131800100414         // -?Ricerca?
131900100414         if  %scan ( '?' : wNAR ) > *zero;
132000100414           exsr  sr_X§TAB;
132100100414           wNAR = TBLkey;
132200100414           ErrGenerico = *on;
132300100414           PosCurNMO   = *on;
132400100414           leavesr;
132500100414         endif;
132600100414
132700100414         // -?Controllo?
132800100414         TBLkey = wNAR;
132900100414         exsr  sr_TABEL;
133000100414         if  $ErrTabel;
133100100414           ErrMessage  = *on;
133200100414           ErrGenerico = *on;
133300100414           PosCurNMO   = *on;
133400100414           V1Dmsg      = $Msg(20);
133500100414           leavesr;
133600100414         endif;
133700100414
133800100414       ENDSR;
133900090122
134000090122       //--------------------------------------------------------------
134100090122       //?Controllo codice cliente mittente                            ?
134200090122       //--------------------------------------------------------------
134300090122       BEGSR  sr_CtrCCM;
134400090122
134500100414         // -?Salvataggio di *in90?
134600090122         Win90 = ErrGenerico;
134700090122         if  ErrGenerico;
134800090122           ErrGenerico = *off;
134900090122           PosCurCCM   = *off;
135000090122         endif;
135100090122
135200100414         // -?Decodifica cliente?
135300090122         IF  ds_KSC <> INDksc;
135400090122           clear  TIBS69ds;
135500090122           clear  ds_CNACO;
135600090122           clear  ds_CNIND;
135700090122           clear  ds_CNCLP;
135800090122           clear  ds_FNCLS;
135900090122           I69kac = ds_KSC;
136000090122           if  wCCM <> 8888   and   wCCM <> 9999;
136100090122             I69kin = ds_KSC;
136200090122           endif;
136300090122           tibs69r ( TIBS69ds : ds_CNACO :
136400090122                                ds_CNIND :
136500090122                                ds_CNCLP :
136600090122                                ds_FNCLS );
136700090122           if  O69err = *on;
136800090122             ErrMessage  = *on;
136900090122             ErrGenerico = *on;
137000090122             PosCurCCM   = *on;
137100090122             V1Dmsg      = $Msg(21);
137200090122             leavesr;
137300090122           endif;
137400090122         ENDIF;
137500090122
137600130315         // -?Flag = <> *blank Cliente bloccato (Non Spedisce - No Bolle)?
137700130315         if  ACOabl <> *blanks;
137800090122           ErrMessage  = *on;
137900090122           ErrGenerico = *on;
138000090122           PosCurCCM   = *on;
138100090122           V1Dmsg      = $Msg(22);
138200090122           leavesr;
138300090122         endif;
138400090122
138500100414         // -?Non accetto clienti con ntw LOG o XXX?
138600090122         chain  wFLC  AZORG;
138700090122         if  %found(AZORG01L);
138800090122           og143  = ORGde3;
138900090122         else;
139000090122           §OGntw = 'XXX';
139100090122         endif;
139200090122         if  §OGntw = 'XXX'   or   §OGntw = 'LOG';
139300090122           ErrMessage  = *on;
139400090122           ErrGenerico = *on;
139500090122           PosCurCCM   = *on;
139600090122           V1Dmsg      = $Msg(23);
139700090122           leavesr;
139800090122         endif;
139900090122
140000100414         // -?Se è 8888 o 9999:?
140100090122         IF  wCCM = 8888   or   wCCM = 9999;
140200090122
140300100414           // ·?Abilitazione di tutto il codice cliente?
140400090122           if  not  NoProtMittD;
140500090122             NoProtMittD = *on;
140600090122             ErrGenerico = *on;
140700090122             PosCurRSM   = *on;
140800090122           endif;
140900090122
141000100414           // ·?Se prima era codificato e ora no:?
141100100414           //  ?si puliscono i campi anagrafici del mittente?
141200090122           if  wCCM <> SAVccm;
141300090122             if  (SAVccm <> 8888   and   SAVccm <> 9999);
141400090122               clear  V1Crsm;
141500090122               clear  V1Cinm;
141600090122               clear  V1Clom;
141700090122               clear  V1Ccam;
141800090122               clear  V1Cprm;
141900090122               clear  V1Cnzm;
142000090122             endif;
142100090122             SAVccm = wCCM;
142200090122           endif;
142300090122
142400100414         // -?...Altrimenti: si eseguono i controlli sull'indirizzo?
142500090122         ELSE;
142600090122
142700090122           V1Crsm = ACOrag;
142800090122
142900100414           // ·?Verifica se esiste in anagrafica la provincia?
143000090122           if  INDprv = *blank;
143100090122             ErrMessage  = *on;
143200090122             ErrGenerico = *on;
143300090122             PosCurCCM   = *on;
143400090122             V1Dmsg      = $Msg(24);
143500090122             leavesr;
143600090122           endif;
143700090122
143800100414           // ·?CAP obbligatorio: verifica congruenza con provincia?
143900090122           clear  TISI95ds;
144000090122           clear  FNLV13ds;
144100090122           I95tcn = '3';
144200090122           I95cap = INDcae;
144300090122           I95nar = INDsta;
144400090122           I95prv = INDprv;
144500090122           I95loc = INDcit;
144600090122           I95dat = wDate;
144700100414           // -?Errore affidabilità = 0?
144800090122           I13af0 = 'S';
144900100414           // -?Controllo congruenza CAP/Provincia?
145000090122           I13cng = 'S';
145100100414           // -?Controllo NON forzabile Città con viario?
145200090122           I13cnv = 'S';
145300090122           fnlv13r ( kpjba : FNLV13ds : TISI95ds );
145400100414           // -?Se errore => messaggio aggiungendovi la scritta?
145500100414           //  ?             "SUL PIANO DEI CONTI"?
145600090122           if  O13err <> *blank;
145700090122             ErrMessage  = *on;
145800090122             ErrGenerico = *on;
145900090122             PosCurCCM   = *on;
146000090122             if  %len ( %trim ( O13msg ) ) + 2 < 61;
146100090122               V1Dmsg = %trim(O13msg) + '  ' + C_PDC;
146200090122             else;
146300090122               V1Dmsg = %subst( O95msg : 1 :
146400090122                                %len(V1Dmsg) - %len(c_PDC) )
146500090122                      + c_PDC;
146600090122             endif;
146700090122             leavesr;
146800090122           endif;
146900090122
147000100414           // ·?Visto che è un codificato, prendo la divisa che può?
147100100414           //  ?servire per l'importo da assicurare o per il valore?
147200100414           //  ?merce dichiarato?
147300090122           TBLcod = '15';
147400090122           TBLkey = INDsta;
147500090122           exsr  sr_TABEL;
147600100414           // ·?· Nazione errata?
147700090122           if  $ErrTabel;
147800090122             ErrMessage  = *on;
147900090122             ErrGenerico = *on;
148000090122             PosCurCCM   = *on;
148100090122             V1Dmsg      = $Msg(20);
148200090122             leavesr;
148300090122           else;
148400090122             NoProtMittD = *off;
148500090122           endif;
148600090122
148700100414           // ·?Se tutto OK (e non è 8888 o 9999 o cliente di altra?
148800100414           //  ?filiale => protegge il mittente e memorizza i campi?
148900090122           V1Crsm = ACOrag;
149000090122           V1Cinm = INDvia;
149100090122           V1Clom = INDcit;
149200090122           V1Ccam = INDcae;
149300090122           V1Cprm = INDprv;
149400090122           V1Cnzm = INDsta;
149500090122
149600090122         ENDIF;
149700090122
149800100414         // -?Ripristino di *in90?
149900090122         if  NOT ErrGenerico   and   Win90 = *on;
150000090122           ErrGenerico = *on;
150100090122         endif;
150200090122
150300090122       ENDSR;
150400090122
150500090122       //--------------------------------------------------------------
150600090122       //?Controllo indirizzo completo Mittente / Destinatario         ?
150700090122       //--------------------------------------------------------------
150800090122       BEGSR  sr_CtrIND;
150900090122
151000090122         clear  FNLV13ds;
151100090122         //clear  FNLV14ds;            NO !?!?!?
151200090122         clear  TISI95ds;
151300090122         clear  I14iso;
151400090122         I14dri = wDate;
151500090122         fnlv14r ( kpjba : FNLV14ds );
151600090122         wERR   = O14err;
151700090122         V1Dmsg = O14msg;
151800090122         exsr  sr_CtrERR;
151900090122         if  ErrGenerico;
152000090122           leavesr;
152100090122         endif;
152200090122
152300100414         // -?Controllo CAP?
152400090122         I95tcn = '7';
152500090122         I95cap  = E14cap;
152600090122         I95loc  = E14loc;
152700090122         I95prv  = E14prv;
152800090122         I95nar  = E14nar;
152900090122         I95dat  = wDate;
153000090122         I13af0  = 'S';
153100090122         I13af1  = 'S';
153200090122         I13sz2  = 'S';
153300090122         I13cnv  = 'S';
153400090122         I13szv  = 'S';
153500090122
153600090122         IF  wCLI = 'M';
153700090122
153800100414           // -?MITTENTE?
153900100414           //?· Se cambiato CAP: ctrl se ha viario (*err forzabile)?
154000090122           if  E14cap <> SAVcam;
154100090122             clear  SAVfzvM;
154200090122             clear  SAVfz1M;
154300090122             clear  SAVfz2M;
154400090122             SAVcam = E14cap;
154500090122           endif;
154600100414           //?· Cambiata località?
154700090122           if  E14loc <> SAVlom;
154800090122             clear  SAVfz1M;
154900090122             clear  SAVfz2M;
155000090122             SAVlom = E14loc;
155100090122           endif;
155200090122
155300090122           E13fzv = SAVfzvM;
155400090122           E13fz1 = SAVfz1M;
155500090122           E13fz2 = SAVfz2M;
155600090122
155700090122         ELSE;
155800090122
155900100414           // -?DESTINATARIO?
156000100414           //?· Se cambiato CAP: controllo se ha viario?
156100090122           if  E14cap <> SAVcad;
156200090122             clear  SAVfzvD;
156300090122             clear  SAVfz1D;
156400090122             clear  SAVfz2D;
156500090122             SAVcad = E14cap;
156600090122           endif;
156700100414           //?· Cambiata località?
156800090122           if  E14loc <> SAVlod;
156900090122             clear  SAVfz1D;
157000090122             clear  SAVfz2D;
157100090122             SAVlod = E14loc;
157200090122           endif;
157300090122
157400090122           E13fzv = SAVfzvD;
157500090122           E13fz1 = SAVfz1D;
157600090122           E13fz2 = SAVfz2D;
157700090122
157800090122         ENDIF;
157900090122
158000090122         fnlv13r ( kpjba : FNLV13ds : TISI95ds );
158100090122
158200090122         wERR   = O13err;
158300090122         V1Dmsg = O13msg;
158400090122         exsr  sr_CtrERR;
158500090122
158600100414         // -?Reimpostazione delle forzature?
158700090122         if  wCLI = 'M';
158800090122           SAVfzvM = E13fzv;
158900090122           SAVfz1M = E13fz1;
159000090122           SAVfz2M = E13fz2;
159100090122         else;
159200090122           SAVfzvD = E13fzv;
159300090122           SAVfz1D = E13fz1;
159400090122           SAVfz2D = E13fz2;
159500090122         endif;
159600090122
159700090122         if  ErrGenerico;
159800090122           leavesr;
159900090122         endif;
160000090122
160100090122       ENDSR;
160200090122
160300090122       //--------------------------------------------------------------
160400090122       //?Controllo errore                                             ?
160500090122       //--------------------------------------------------------------
160600090122       BEGSR  sr_CtrERR;
160700090122
160800100414         // -?Impostazione dei campi passati?
160900090122         if  O13ron = 'S';
161000090122           E14nar = O95nar;
161100090122         endif;
161200090122         if  O13roc = 'S';
161300090122           E14cap = O95cap;
161400090122         endif;
161500090122         if  O13rop = 'S';
161600090122           E14prv = O95prv;
161700090122         endif;
161800090122         if  O13rol = 'S';
161900090122           E14loc = O95loc;
162000090122         endif;
162100090122
162200100414         // -?Errore o ricerca campo?
162300090122         IF  wERR <> *blank;
162400090122
162500090122           if  V1Dmsg <> *blank;
162600090122             ErrMessage =  *on;
162700090122           endif;
162800100414           // -?Posizionamento corsore?
162900090122           select;
163000100414             // ·?ragione sociale?
163100090122             when  wERR = '1'   or   wERR = '2';
163200090122               if  wCLI = 'M';
163300090122                 PosCurRSM = *on;
163400090122               else;
163500090122                 PosCurRSD = *on;
163600090122               endif;
163700100414             // ·?località?
163800090122             when  wERR = '3';
163900090122               if  wCLI = 'M';
164000090122                 PosCurLOM = *on;
164100090122               else;
164200090122                 PosCurLOD = *on;
164300090122               endif;
164400100414             // ·?provincia?
164500090122             when  wERR = '4';
164600090122               if  wCLI = 'M';
164700090122                 PosCurPRM = *on;
164800090122               else;
164900090122                 PosCurPRD = *on;
165000090122               endif;
165100100414             // ·?cap?
165200100414             when  wERR = '5';
165300090122               if  wCLI = 'M';
165400090122                 PosCurCAM = *on;
165500090122               else;
165600090122                 PosCurCAD = *on;
165700090122               endif;
165800100414             // ·?nazione?
165900090122             when  wERR = '6';
166000090122               if  wCLI = 'M';
166100090122                 PosCurNZM = *on;
166200090122               else;
166300090122                 PosCurNZD = *on;
166400090122               endif;
166500100414             // ·?partita Iva?
166600090122             //when  wERR = '7';
166700100414             // ·?partita Iva estera?
166800090122             //when  wERR = '8';
166900090122           endsl;
167000090122
167100090122           ErrGenerico = *on;
167200090122
167300090122         ENDIF;
167400090122
167500090122       ENDSR;
167600090122
167700090122       //--------------------------------------------------------------
167800090122       //?Stampa del Documento di Ritiro                               ?
167900090122       //--------------------------------------------------------------
168000090122       BEGSR  sr_StampaDdR;
168100090122
168200100414         // -?Esecuzione override al file di stampa?
168300100414         if  $OvrPrtF = *off;
168400100722           Qcmd = c_CmdOvrPrtF
168500100722                + ' outq(' + %trim(D90prb4) + ')'
168600100722                + ' formtype(''' + D90mdb4 + ''')';
168700100722           callp(e)  ExecuteCommand(Qcmd : %size(Qcmd));
168800090123           //if %error;
168900090123           //endif;
169000090123           $OvrPrtF = *on;
169100090123         endif;
169200090123
169300100414         // -?Impostazione parametri per il pgm di stampa?
169400090122         clear  FNLSB5ds;
169500090122
169600100414         // -?Tipo Bolla     = Partenza?
169700090122         DB0tbo = 'P';
169800090122
169900100414         // -?Flag gestione  = "B" (dati passati)?
170000090122         DB0ges = 'B';
170100090122
170200100414         // -?Flag ristampa  = "M" (Rimesso Mittente)?
170300090122         DB0ris = 'M';
170400090122
170500100414         // -?Fil. partenza  = Fil. richiedente?
170600090122         DB0lnp = DUTpou;
170700090122
170800100414         // -?Codice tariffa = Numero Copie?
170900090122         DB0ctr = V1Cncp;
171000090122
171100100414         // -?Codice bolla?
171200090122         if  V1Ccbo <> *blank;
171300090122           DB0cbo = V1Ccbo;
171400090122           DB0tb1 = §3Atb1;
171500090122           DB0tb2 = §3Atb2;
171600090122           DB0dcb = §3Ades;
171700090122         endif;
171800090122
171900100414         // -?Tipo servizio bolle?
172000090122         if  V1Ctsp <> *blank;
172100090122           DB0tsp = V1Ctsp;
172200090122           DB0dts = §5Edes;
172300090122           DB0fst = §5Efst;
172400090122         endif;
172500090122
172600100414         // -?Data spedizione?
172700090122         DB0aas = %div ( W1Cdsp : 10000 );
172800090122         DB0mgs = %rem ( W1Cdsp : 10000 );
172900090122
173000100414         // -?Importo da assicurare?
173100090122         DB0ias = V1Cias;
173200090122         if  DB0ias <> *zero;
173300090122           DB0vas = V1Cvas;
173400090122         endif;
173500090122
173600100414         // -?Tipo incasso c/assegno?
173700090122         IF  V1Ctic <> *blank;
173800090122           DB0tic = V1Ctic;
173900090122           DB0ucm = §1Aucm;
174000090122           DB0ste = §1Aste;
174100090122           DB0dti = §1Ades;
174200100414           // -?la descrizione del tipo incasso la imposto solo:?
174300100414           //  ?· se s'è l'importo oppure?
174400100414           //  ?· se §1ADEO = *blanks, cioè ce n'è solo una?
174500100414           //  ?vedo se impostare la descrizione oltre limite?
174600090122           if  §1Adeo <> *blank   and   V1Ccas > *zero   and
174700100414             //?(il TRUL di controllo è già stato richiamato nella?
174800100414             //?subr. dei controlli)?
174900090122               O21fca <> *blank;
175000090122             DB0dti = §1Adeo;
175100090122           endif;
175200100414           // -?se c'è la seconda descrizione e non c'è l'importo del?
175300100414           //  ?C/Assegno non si può decodificare, e quindi ai abblenka?
175400100414           //  ?la decodifica?
175500090122           if  §1Adeo <> *blank   and   V1Ccas = *zero;
175600090122             clear  DB0dti;
175700090122           endif;
175800090122         ENDIF;
175900090122
176000100414         // -?Importo c/assegno?
176100090122         DB0cas = V1Ccas;
176200090122         DB0vca = V1Cvca;
176300090122
176400100414         // -?Codice cliente mittente?
176500090122         DB0ccm = ds_KSC;
176600090122
176700100414         // -?Decodifica del mittente (stampata solo se richiesta)?
176800090122         if  V1Cstm = 'S';
176900090122           DB0rsm = V1Crsm;
177000090122           DB0inm = V1Cinm;
177100090122           DB0lom = V1Clom;
177200090122           DB0prm = V1Cprm;
177300090122           if  V1Ccmo <> *blank;
177400090122             DB0cam = V1Ccmo;
177500090122             DB0nzm = V1Cnmo;
177600090122           else;
177700090122             DB0cam = V1Ccam;
177800090122             DB0nzm = V1Cnzm;
177900090122           endif;
178000090122         endif;
178100090122
178200100414         // -?Decodifica del destinatario?
178300090122         DB0rsd = V1Crsd;
178400090122         evalr DB0rd2 = V1Crsd;
178500090122         DB0ind = V1Cind;
178600090122         DB0cad = V1Ccad;
178700090122         DB0lod = V1Clod;
178800090122         DB0prd = V1Cprd;
178900090122         DB0nzd = V1Cnzd;
179000090122
179100100414         // -?Mittente originale?
179200090122         DB0rmo = V1Crmo;
179300090122
179400100414         // -?Giorni chiusura?
179500090122         DB0gc1 = V1Cgc1;
179600090122         DB0gc2 = V1Cgc2;
179700090122
179800100414         // -?Natura merce?
179900090122         DB0nas = V1Cnas;
180000090122
180100100414         // -?Numero colli?
180200090122         DB0ncl = V1Cncl;
180300090122
180400100414         // -?Peso?
180500090122         DB0pkf = V1Cpkg;
180600090122
180700100414         // -?Volume?
180800090122         if  V1Cvol > *zero;
180900090122           DB0vlf = V1Cvol;
181000090122           DB0fvf = 'R';
181100090122         endif;
181200090122
181300100414         // -?Note?
181400090122         DB0nt1 = V1Cnt1;
181500090122
181600100414         // -?Flag fermo deposito?
181700090122         DB0ffd = V1Cffd;
181800090122
181900100414         // -?Data / Ora / Tipo consegna richiesta?
182000090122         DB0dcr = W1Cdcr;
182100090122         DB0hcr = V1Cocr;
182200090122         DB0tcr = V1Ctcr;
182300090122
182400090122
182500100414         // -?Richiamo pgm di stampa?
182600100722         fnlsB9r ( FNLSB5ds );
182700090122
182800090122       ENDSR;
182900090122
183000090122       //--------------------------------------------------------------
183100090122       //?Operazioni finali.                                           ?
183200090122       //--------------------------------------------------------------
183300090122       BEGSR  sr_RoutEnd;
183400090122
183500100414         // -?Chiusura funzioni precedentemente aperte?
183600090122
183700090123         clear  FNLSB5ds;
183800090123         DB0tla = 'C';
183900100722         fnlsB9r ( FNLSB5ds );
184000090123
184100090123         clear  FNLV13ds;
184200090123         I13tla = 'C';
184300090123         fnlv13r ( kpjba : FNLV13ds : TISI95ds );
184400090123
184500090123         clear  FNLV14ds;
184600090123         I14tla = 'C';
184700090123         fnlv14r ( kpjba : FNLV14ds );
184800090123
184900090123         clear  TIBS69ds;
185000090123         I69tla = 'C';
185100090123         tibs69r (TIBS69ds : ds_CNACO :
185200090123                             ds_CNIND :
185300090123                             ds_CNCLP :
185400090123                             ds_FNCLS);
185500090122
185600090122         clear  trulTICds;
185700090122         ITICtla = 'C';
185800090122         trulticr ( TRULTICds : ds1A );
185900090122
186000090123
186100100414         // -?Cancellazione override al file di stampa?
186200090123         if  $OvrPrtF = *on;
186300100722           callp(e)  ExecuteCommand(c_CmdDltOvr : %size(c_CmdDltOvr));
186400090123         endif;
186500090123
186600090122
186700100414         // -?"Chiusura" pgm?
186800090122         return;
186900090122
187000090122       ENDSR;
187100090122
187200090122      /end-free
187300090122
187400090122       //--------------------------------------------------------------
187500090122       //?Definizione schiere a tempo di compilazione                  ?
187600090122       //--------------------------------------------------------------
187700090122
187800100414** >?$GG?
187900090122 1234567
188000100414** >?$Msg?-------------------------------------------------------------------*
188100090122Data Documento Errata                                                          1
188200090122Codice Bolla errato o non utilizzabile                                         2
188300100806Tipo servizio non utilizzabile (ammessi: _=nessuno, E=Priority, H=10:30)       3
188400090122Data consegna richiesta errata                                                 4
188500090122Ora consegna richiesta errata                                                  5
188600090122Minuti consegna richiesta errata                                               6
188700090122Se immesso tipo immettere anche la data o l'ora Consegna Richiesta             7
188800090122Tipo Incasso C/Assegno inesistente o non e' da indicare                        8
188900090122Importo C/Assegno Errato                                                       9
189000090122Divisa errata                                                                 10
189100090122Non ammessi importi con decimali per la divisa indicata                       11
189200090122Numero di decimali errato per la divisa immessa                               12
189300090122Divisa obbligatoria                                                           13
189400090122Divisa scaduta                                                                14
189500090122Tipo incasso C/Assegno inesistente o non e' da indicare                       15
189600090122Tipo incasso non utilizzabile se C/Assegno in divisa estera                   16
189700090122Tipo Incasso non ammesso per il cliente indicato                              17
189800090122Giorno Chiusura Errato                                                        18
189900090122Se immesso CAP o Nazione Mit.Originale immettere tutti 3 i campi:CAP NAZ e MIT19
190000090122Nazione Errata                                                                20
190100090122Cliente Errato o Annullato                                                    21
190200090122Codice inutilizzabile: cliente che non spedisce                               22
190300090122Codice cliente o linea cliente non utilizzabile                               23
190400090122Manca provincia del Mittente in anagrafica: INSERIRLA!!!!                     24
190500090122Immettere il Codice Mittente e/o la Ragione Sociale Destinatario              25
190600090122Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR. TEL.IN SEDE X AUTOR. 26
190700090122Tipo incasso non utilizzabile per la divisa del contrassegno                  27
190800090122Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              28
190900090122Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              29
191000090122Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        30
191100090122Importo da Assicurare non ammesso                                             31
191200090122Importo da Assicurare INFERIORE a quanto previsto dalla legge                 32
191300090122Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  33
191400110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       34
