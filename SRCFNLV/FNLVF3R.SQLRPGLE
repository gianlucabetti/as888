000100161214      //---------------------------------------------------------------
000200170116      //?FNLVF3R - Anomalie Distinte/Assenze
000300161214      //---------------------------------------------------------------
000400161214     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500161220     h dftactgrp(*no) actgrp(*caller)
000600161214
000700161214      //---------------------------------------------------------------
000800161214      //?Dichiarazione file.
000900161214      //---------------------------------------------------------------
001000161214      // - Autisti
001100161220     fFIAPD01L  if   e           k disk
001200161215
001300161215      // - File di Work
001400161220     fWFADS01L  uf a e           k disk    usropn
001500161220     f                                     extfile('QTEMP/WFADS01L')
001600161214
001700161214      // - File Video
001800161214     fFNLVF3D   cf   e             workstn usropn
001900161214     f                                     sfile(LVF3S01 : S01nrr)
002000161214     f                                     indds(IndDspF)
002100161214     f                                     infds(InfDspF)
002200161214
002300161214      //---------------------------------------------------------------
002400161214      //?Definizione costanti.
002500161214      //---------------------------------------------------------------
002600161220      // - Tasti funzionali a video
002700161220     d c_F01           c                   const(x'31')
002800161220     d c_F02           c                   const(x'32')
002900161220     d c_F03           c                   const(x'33')
003000161220     d c_F04           c                   const(x'34')
003100161220     d c_F05           c                   const(x'35')
003200161220     d c_F06           c                   const(x'36')
003300161220     d c_F07           c                   const(x'37')
003400161220     d c_F08           c                   const(x'38')
003500161220     d c_F09           c                   const(x'39')
003600161220     d c_F10           c                   const(x'3A')
003700161220     d c_F11           c                   const(x'3B')
003800161220     d c_F12           c                   const(x'3C')
003900161220     d c_F13           c                   const(x'B1')
004000161220     d c_F14           c                   const(x'B2')
004100161220     d c_F15           c                   const(x'B3')
004200161220     d c_F16           c                   const(x'B4')
004300161220     d c_F17           c                   const(x'B5')
004400161220     d c_F18           c                   const(x'B6')
004500161220     d c_F19           c                   const(x'B7')
004600161220     d c_F20           c                   const(x'B8')
004700161220     d c_F21           c                   const(x'B9')
004800161220     d c_F22           c                   const(x'BA')
004900161220     d c_F23           c                   const(x'BB')
005000161220     d c_F24           c                   const(x'BC')
005100161220     d c_Enter         c                   const(x'F1')
005200161220     d c_RollDown      c                   const(x'F4')
005300161220     d c_RollUp        c                   const(x'F5')
005400161220
005500161220     d Digits          c                   const('0123456789')
005600161214
005700161214      //---------------------------------------------------------------
005800161214      //?Definizione schiere.
005900161214      //---------------------------------------------------------------
006000161215      // - Sk Date da elaborare
006100161215     d skData          s              8  0 dim(999)
006200161215
006300161214      // - Messaggi di errore
006400161214     d Msg             s             78    dim(10) ctdata perrcd(1)
006500161214
006600161214      //---------------------------------------------------------------
006700161214      //?Definizione aree dati.
006800161214      //---------------------------------------------------------------
006900161214      // - Dati utente
007000161214     d §AzUte        e ds                  extname(AZUTE00F)
007100161214     d                                     dtaara
007200161214     d §DatiUte      e ds                  extname(dDatiUte)
007300161214     d                                     dtaara
007400170116
007500170116       // -?Numero di record in ciascuna videata di subfile?
007600170116     d wSflPag         c                   const(20)
007700170116       // -?Numero massimo di record gestibili?
007800170116     d wMaxRec         c                   const(9999)
007900161214
008000161214      //---------------------------------------------------------------
008100161214      //?Definizione strutture dati.
008200161214      //---------------------------------------------------------------
008300161214      // - Status
008400161214     d Psds           sds
008500161214     d   SDSpgm          *proc
008600161214
008700161214      // - InfDS
008800161214     d InfDspF         ds
008900161214     d  dsp_aid              369    369a
009000161214
009100161214      // - Indicatori su DspF
009200161214     d IndDspF         ds
009300161214        // - Indicatori di visualizzazione errore
009400161214     d  ErrMessage                    1n   overlay(IndDspF : 28)
009500161214        // - Indicatori di gestione del subfile
009600161214     d  SflDsp                        1n   overlay(IndDspF : 30)
009700161214     d  SflDspCtl                     1n   overlay(IndDspF : 31)
009800161219     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009900161219     d  SflEnd                        1n   overlay(IndDspF : 33)
010000161214        // - Indicatori di posizionamento cursore
010100161219     d  PoscurOPZ                     1n   overlay(IndDspF : 50)
010200161214        // - Indicatori di errore generico
010300161214     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010400161214
010500161214      // - Campi di comodo per gestione indicatori a video
010600161214     d WindDspF        ds                  inz
010700161220     d  WindDspFa              1     49    inz(*zero)
010800161220     d  WindDspFb             50     99    inz(*zero)
010900161220
011000161220      // - Controllo data
011100161220     d wlbdat          ds                  inz
011200161220     d  g08dat                 1      8  0
011300161220     d  g08inv                 9     16  0
011400161220     d  g08err                17     17
011500161220     d  g08tgi                18     22  0
011600161214
011700161214      // - Parametri ricevuti
011800161214     d KPJBA         e ds
011900161214     d FNLVF3DS      e ds                  qualified
012000161220
012100161220      // - DS per richiamo a FNLVF1R
012200161220     d FNLVF1DS      e ds                  qualified
012300161214
012400161214      // - DS per richiamo a FNLVF9R
012500161214     d FNLVF9DS      e ds                  qualified
012600161219
012700161219      // - Ricerca/Controllo tabelle
012800170126     d TIBS02DS      e ds                  inz
012900161219
013000161219      // - Reperimento dati utente
013100161219     d TIBS34DS      e ds
013200161228
013300161228      // - Ricerca tabella Causali Assenza
013400170126     d TNTBACADS     e ds
013500161214
013600161214      // - Aggiunge/Toglie gg/mm dalla data
013700161214     d XGIOLAVDS     e ds                  inz
013800161219
013900161219      // - Tabella ACA = Causale assenza
014000161219     d dACA          e ds                  inz
014100170116
014200170116      // - Campo APDflr
014300170116     d dAPDFLR       e ds
014400161214
014500161214      // - Tabella 5A rcd AUT
014600161214     d ds5AAUT       e ds
014700161220
014800161220      // - File Agenda Autisti
014900161220     d FIAGA00F      e ds                  extname(FIAGA00F)
015000161214
015100161214      // - File Distinte
015200161214     d FIDST00F      e ds                  extname(FIDST00F)
015300161214
015400161214      // - File Tabelle
015500161214     d TABEL00F      e ds                  extname(TABEL00F)
015600161219
015700161219      // - File Work
015800161219     d WFADS00F      e ds                  extname(WFADS00F)
015900161214
016000161214      //---------------------------------------------------------------
016100161214      //?Definizione variabili globali.
016200161214      //---------------------------------------------------------------
016300161214      // - Flags booleani
016400161214     d EndSQL          s               n   inz(*off)
016500161219     d EndS01          s               n   inz(*off)
016600161214     d ErrGrave        s               n   inz(*off)
016700161214     d Fine            s               n   inz(*off)
016800170116     d ForzaOpz        s               n   inz
016900161215     d OkAssenza       s               n   inz(*off)
017000161215     d OkDistinta      s               n   inz(*off)
017100161228     d OkOpzione       s               n   inz(*off)
017200161219     d RecOK           s               n   inz(*off)
017300161220     d wEoF            s               n   inz(*off)
017400161228     d wEoFS01         s               n   inz(*off)
017500161214     d wInzS01         s               n   inz(*on)
017600161214
017700161214      // - Campi associati al video
017800161214     d Video           s              2    inz('S1')
017900161214     d S01Nrr          s              4  0 inz
018000161228     d savS01Nrr       s              4  0 inz
018100170116     d savV01csr       s                   like(V01csr) inz
018200170116     d savV01cs1       s                   like(V01csr) inz
018300170116     d savV01cs2       s                   like(V01csr) inz
018400161228     d savV01Opz       s                   like(V01opz) inz
018500170116     d savV01Op1       s                   like(V01opz) inz
018600161215
018700161215      // - Indice di schiera
018800161215     d xx              s              4  0 inz
018900161214
019000161214      // - Campi di comodo
019100161219     d cmd             s            512a   varying
019200161219     d Data_EUR        s               d   datfmt(*eur)
019300161219     d Data_ISO        s               d   datfmt(*iso)
019400170126     d Oggi            s              8s 0 inz
019500161220     d wautini         s                   like(APDpdr) inz
019600161220     d wautfin         s                   like(APDpdr) inz
019700161221     d wATN            s                   like(AGAatn) inz
019800161221     d wCAU            s                   like(AGAcau) inz
019900161221     d wDAT            s                   like(AGAdat) inz
020000170126     d wDatada         s              8s 0 inz
020100161221     d wFGS            s                   like(DSTfgs) inz
020200161220     d wFLib           s             21a   inz
020300161220     d wLib            s             10a   inz
020400161221     d wNFV            s                   like(DSTnfv) inz
020500170116     d wPag            s              4  0 inz
020600161221     d wPDR            s                   like(APDpdr) inz
020700170116     d wRig            s              3  0 inz
020800161214
020900161214      //---------------------------------------------------------------
021000161214      //?Definizione procedure esterne.
021100161214      //---------------------------------------------------------------
021200161220      // - Inserimento Agenda
021300161220     d fnlvf1r         pr                  extpgm('FNLVF1R')
021400161220     d  kpjba                              likeds(KPJBA)
021500161220     d  fnlvf1ds                           likeds(FNLVF1DS)
021600161220
021700161214      // - Controllo Autista Accreditato
021800161214     d fnlvf9r         pr                  extpgm('FNLVF9R')
021900161214     d  fnlvf9ds                           likeds(FNLVF9DS)
022000161228
022100161228      // - Selezione singola causale assenza
022200161228     d tntbacar1       pr                  extpgm('TNTBACAR1')
022300161228     d   kpjba                             likeds(KPJBA)
022400161228     d   tntbacads                         likeds(TNTBACADS)
022500161214
022600161214      // - Aggiunge/Toglie gg/mm dalla data
022700161219     d xgiolav         pr                  extpgm('XGIOLAV')
022800161214     d  xgiolavds                          likeds(xgiolavds)
022900161214
023000161214      //---------------------------------------------------------------
023100161214      //?Definizione prototipi utilizzati.
023200161214      //---------------------------------------------------------------
023300161220      /copy gaitrasrc/srcprotopr,fnlv24r
023400161219      /copy gaitrasrc/srcprotopr,tibs02r
023500161219      /copy gaitrasrc/srcprotopr,tibs34r
023600161219      /copy gaitrasrc/srcprotopr,SYSTEM
023700161220      /copy gaitrasrc/srcprotopr,XSRDA8
023800161214
023900161214      //---------------------------------------------------------------
024000161214      //?Definizione key-list.
024100161214      //---------------------------------------------------------------
024200161220      // - File FIAPD01L?
024300161220     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
024400161220     d                                     prefix(k_) inz
024500161214
024600161214      //---------------------------------------------------------------
024700161214      //?Riepilogo indicatori.
024800161214      //---------------------------------------------------------------
024900161214
025000161214      //---------------------------------------------------------------
025100161214      //?M A I N - L I N E
025200161214      //---------------------------------------------------------------
025300161214     c     *ENTRY        plist
025400161214     c                   parm                    kpjba
025500161214     c                   parm                    fnlvf3ds
025600161214
025700161214      /free
025800161214
025900161214       //?Operazioni iniziali
026000161214       exsr RoutInz;
026100161215
026200161215       //?Leggo gli Autisti accreditati ad oggi
026300161215       //?e scrivo il file di WORK
026400161215       exsr LeggiAutisti;
026500161215
026600161215       //?Se richiamato per Gestione
026700161215       //?devo emettere la videata
026800161215       DOW  not Fine and fnlvf3ds.ILVF3trc = 'G';
026900161219         SELECT;
027000161219         WHEN  Video = 'S1';
027100161219           exsr GesS01;
027200161219         OTHER;
027300161219           Fine = *on;
027400161219         ENDSL;
027500161215       ENDDO;
027600161214
027700161214       //?Operazioni finali
027800161214       exsr RoutEnd;
027900161214
028000161214       //--------------------------------------------------------------
028100161214       //?Operazioni iniziali.
028200161214       //--------------------------------------------------------------
028300161214       BEGSR RoutInz;
028400161219
028500161219       //?Reperimento dati job
028600161219         exsr DatiJob;
028700170109
028800170109       //?Impostazione campi "fissi"
028900170109         T01pgm = SDSpgm;
029000161214
029100161214         clear fnlvf3ds.OLVF3disco;
029200161214         clear fnlvf3ds.OLVF3err;
029300161214         clear fnlvf3ds.OLVF3msg;
029400161214
029500161214       //?Se non c'è il tipo richiamo assumo 'C' di controllo
029600161214         IF  fnlvf3ds.ILVF3trc = *blanks;
029700161214           fnlvf3ds.ILVF3trc = 'C';
029800161214         ENDIF;
029900161214
030000161214       //?Se non c'è la filiale torno errore
030100161214         IF  fnlvf3ds.ILVF3fgs = 0;
030200161214           fnlvf3ds.OLVF3err = '1';
030300161214           fnlvf3ds.OLVF3msg = Msg(01);
030400161214           exsr RoutEnd;
030500161214         ENDIF;
030600161221         wFGS = fnlvf3ds.ILVF3fgs;
030700161214
030800161214       //?Carico gg lavorativi da tabella 5A - AUT
030900161214         clear ds5AAUT;
031000161214         exec sql
031100161214         SELECT tabel00f.* into :TABEL00F
031200161214         FROM   tabel00f
031300161214         WHERE  TBLkut = 1 and TBLcod = '5A' and
031400161214                substr(TBLkey, 1, 3) = 'AUT' and
031500161214                TBLflg = '';
031600161214         IF  sqlcod = 0;
031700161214           ds5AAUT = TBLuni;
031800161214         ENDIF;
031900161214
032000161214       //?Imposto Oggi
032100161214         Oggi = %dec(%date());
032200161214
032300161214       //?Calcolo i gg lavorativi indietro da oggi
032400161214         wDatada = Oggi;
032500161214         exsr CalcolaData;
032600161219
032700161214       //?Apro il file video se richiamato per Gestione
032800161214         IF  fnlvf3ds.ILVF3trc = 'G';
032900161214           open FNLVF3D;
033000161219           Video = 'S1';
033100161219           wInzS01 = *on;
033200161214         ENDIF;
033300161219
033400161219       //?Determino la libreria
033500161219         IF  %subst(knsif : 7 : 1) = 'P';
033600170126           wLib = 'UNITRAGRP';
033700161219         ELSE;
033800170126           wLib = 'UNITRAGRU';
033900161219         ENDIF;
034000161215
034100161215       //?Genero il file di work in qtemp
034200161220         cmd = 'CRTDUPOBJ OBJ(WFADS*) FROMLIB(' +
034300161220                %trim(wLib) + ') ' +
034400161220               'OBJTYPE(*FILE) TOLIB(QTEMP)';
034500161229         callp(e) ExecuteCommand (cmd);
034600161229         IF  %error;
034700161229           cmd = 'CLRPFM QTEMP/WFADS00F';
034800161229           ExecuteCommand (cmd);
034900161229         ENDIF;
035000161219       //?Apro il file
035100161219         open WFADS01L;
035200161220
035300161220       //?Dati Fissi
035400161220         k_APDtip = 'A';
035500161214
035600161214       ENDSR;
035700161215
035800161215       //--------------------------------------------------------------
035900161215       //?Leggo gli Autisti accreditati ad oggi
036000161215       //?e scrivo file di WORK.
036100161215       //--------------------------------------------------------------
036200161215       BEGSR LeggiAutisti;
036300161220
036400161220         wEoF = *off;
036500161220
036600161220       //?Imposto range AUT da caricare in base alla FGS
036700161221         wautini = wFGS * 10000;
036800161220         wautfin = wautini + 9999;
036900161220         k_APDpdr = wautini;
037000161215
037100161221       //?Leggo gli autisti accreditati ad oggi
037200161220         setll %kds(kFIAPD01) FIAPD01L;
037300161220         DOW  not wEoF;
037400161220           reade (k_APDtip) FIAPD01L;
037500161220         //?Fine file
037600161220           IF  %eof(FIAPD01L);
037700161220             wEoF = *on;
037800161220             leave;
037900161220           ENDIF;
038000161220         //?Leggo solo gli autisti della filiale gestione ricevuta
038100161220           IF  APDpdr > wautfin;
038200161220             wEof = *on;
038300161220             leave;
038400161220           ENDIF;
038500170116           dAPDflr = APDflr;
038600161215         //?Escludo gli annullati
038700161220           IF  APDatb <> *blanks;
038800161220             iter;
038900161220           ENDIF;
039000170323         //?Escludo gli autisti che non sono in forza lavoro
039100161220           clear FNLVF9DS;
039200161220           fnlvf9ds.ILVF9pdr = APDpdr;
039300161220           fnlvf9ds.ILVF9data = Oggi;
039400161220           fnlvf9r (fnlvf9ds);
039500170323           IF  fnlvf9ds.OLVF9flg = 'N' or
039600170323               fnlvf9ds.OLVF9err <> *blanks;
039700161220             iter;
039800161220           ENDIF;
039900161220
040000161221         //?Il controllo va fatto su tutti i gg previsti in tabella
040100161221           xx = 1;
040200161221           FOR xx by 1 to §5Aggass;
040300161221
040400161221             wDAT = skData(xx);
040500161221             wPDR = APDpdr;
040600170126
040700161221           //?Controllo se ha distinta/Assenza
040800170126             exsr Controlla;
040900161221
041000161221             SELECT;
041100161221           //?OK se ha la distinta e non l'assenza
041200161221             WHEN  OkDistinta and not OkAssenza;
041300161221               iter;
041400161221           //?OK se non ha la distinta ed ha l'assenza
041500161221             WHEN  not OkDistinta and OkAssenza;
041600161221               iter;
041700161221             ENDSL;
041800161221
041900161221           //?Se richiamato per solo controllo
042000161221           //?e vedo già che c'è una discordanza esco dal pgm
042100161221             IF  fnlvf3ds.ILVF3trc = 'C';
042200170130               fnlvf3ds.OLVF3disco = 'S';
042300161221               exsr RoutEnd;
042400161221             ENDIF;
042500161220
042600161221           //?Scrivo il file di work
042700161221             exsr ScriviWrkf;
042800161221           ENDFOR;
042900161220
043000161215         ENDDO;
043100161215
043200161219         feod WFADS01L;
043300161215
043400161215       ENDSR;
043500161215
043600161215       //--------------------------------------------------------------
043700161219       //?Gestione videata S01.
043800161215       //--------------------------------------------------------------
043900161215       BEGSR GesS01;
044000161215
044100161215       //?Inizializzazione videata
044200161215         IF  wInzS01;
044300161215           exsr InzS01;
044400161215           wInzS01 = *off;
044500161215         ENDIF;
044600161215
044700161215       //?Visualizzazione del SFL (se ci sono dati)
044800161215         SflDsp = (S01nrr <= *zeros);
044900161215
045000161215       //?Posizionamento cursore
045100161219         V01rcd = V01csr;
045200161215
045300161215       //?Emissione Testata e Piede con tasti funzionali abilitati
045400161215         write LVF3T01;
045500161215         write LVF3P01;
045600161215
045700161215       //?Emissione videata
045800161215         exfmt LVF3C01;
045900161215         ErrMessage  = *off;
046000161215         ErrGenerico = *off;
046100161215         clear V01msg;
046200170116
046300170116         IF  dsp_aid <> c_F12 and
046400170116             dsp_aid <> c_F13;
046500170116           clear savV01csr;
046600170116           clear savV01cs1;
046700170116           clear savV01cs2;
046800170116           clear savV01opz;
046900170116           clear savV01op1;
047000170116         ENDIF;
047100161215
047200161215         SELECT;
047300161219
047400161228       //?F05=Refresh
047500161219         WHEN  dsp_aid = c_F05;
047600161219           exsr F05S01;
047700161228
047800161228       //?F06=Conferma
047900161228         WHEN  dsp_aid = c_F06;
048000170118           exsr OpzS01;
048100170118           IF  ErrGenerico;
048200170118             leavesr;
048300170118           ENDIF;
048400170117           exsr F06S01;
048500170116
048600170116       //?F12=Ritorno
048700170116         WHEN  dsp_aid = c_F12;
048800170116           exsr F12S01;
048900161228
049000161228       //?F13=Ripeti Opzione
049100161228         WHEN  dsp_aid = c_F13;
049200161228           exsr F13S01;
049300161215
049400161215       //?Enter
049500161215         OTHER;
049600161219         //?Controllo dati
049700161219           IF  V01csr > *zero;
049800161219             exsr OpzS01;
049900161219             IF  ErrGenerico;
050000161219               leavesr;
050100161219             ENDIF;
050200161219           ENDIF;
050300170111           IF  V01csr = *zero;
050400170111             V01csr = savS01nrr;
050500170111           ENDIF;
050600161215
050700161215         ENDSL;
050800161215
050900161215       ENDSR;
051000161215
051100161215       //--------------------------------------------------------------
051200161215       //?Operazioni finali.
051300161215       //--------------------------------------------------------------
051400161215       BEGSR RoutEnd;
051500161215
051600161215       //?Cancello file di work dalla qtemp
051700161220         IF  %open(WFADS01L);
051800161220           close WFADS01L;
051900161220           cmd = 'DLTF FILE(QTEMP/WFADS01L)';
052000161220           ExecuteCommand (cmd);
052100161220           cmd = 'DLTF FILE(QTEMP/WFADS00F)';
052200161220           ExecuteCommand (cmd);
052300161220         ENDIF;
052400161215
052500161215         *inLR = *on;
052600161215         return;
052700161215
052800161215       ENDSR;
052900161219
053000161219       //--------------------------------------------------------------
053100161219       //?Reperimento Dati del job (Utente/Operativi).
053200161219       //--------------------------------------------------------------
053300161219       BEGSR DatiJob;
053400161219
053500161219         in(E) §AzUte;
053600161219         IF  NOT %error;
053700161219           in(E) §DatiUte;
053800161219         ENDIF;
053900161219         IF  %error or RSut = *blanks;
054000170126           clear TIBS34DS;
054100161219           tibs34r(tibs34ds);
054200161219           in §AzUte;
054300161219           in §DatiUte;
054400161219         ENDIF;
054500161219
054600161219       ENDSR;
054700161215
054800161215       //--------------------------------------------------------------
054900161215       //?Calcolo la data da elaborare.
055000161215       //--------------------------------------------------------------
055100161215       BEGSR CalcolaData;
055200161215
055300161215         xx = 1;
055400161215         FOR xx by 1 to §5Aggass;
055500161215           clear XGIOLAVDS;
055600161220           IXGLdata = wDatada;
055700161220           IXGLsub  = 'S';
055800161220           IXGLgg   = xx;
055900161215           IXGLlav  = 'S';
056000161215           IXGLpa   = 'P';
056100161221           IXGLfil  = wFGS;
056200161215           XGIOLAV (xgiolavds);
056300161215           IF  OXGLerr = *blanks;
056400161215             skData(xx) = OXGLdata;
056500161215           ELSE;
056600161219             Data_ISO = %date(wDatada:*iso);
056700161220             Data_ISO = Data_ISO - %days(xx);
056800161219             skData(xx) = %dec(Data_ISO);
056900161215           ENDIF;
057000161215         ENDFOR;
057100161215
057200161215       ENDSR;
057300161221
057400161221       //--------------------------------------------------------------
057500161221       //?Controllo se ci sono discordanze
057600161221       //--------------------------------------------------------------
057700161221       BEGSR Controlla;
057800161221
057900161221         OkAssenza = *off;
058000161221         OkDistinta = *off;
058100161221         clear wNFV;
058200161221         clear wATN;
058300161221         clear wCAU;
058400161221
058500161221       //?Leggo le Distinte del giorno
058600161221       //?e aggiorno file di work
058700161221         exsr LeggiDistinte;
058800161221
058900161221       //?Leggo le Assenze del giorno
059000161221       //?e aggiorno file di work
059100161221         exsr LeggiAssenze;
059200161221
059300161221       ENDSR;
059400161215
059500161215       //--------------------------------------------------------------
059600161215       //?Scrivo il file di Work.
059700161215       //--------------------------------------------------------------
059800161215       BEGSR ScriviWrkf;
059900161215
060000161221       //?Scrivo
060100161221         clear WFADS000;
060200161221         WFAfgs = wFGS;
060300161221         WFApdr = APDpdr;
060400161221         WFAdat = skData(xx);
060500161221         WFAnfv = wNFV;
060600161221         WFAcau = wCAU;
060700161221         WFAatn = wATN;
060800161221         write WFADS000;
060900161215
061000161215       ENDSR;
061100161214
061200161214       //--------------------------------------------------------------
061300161214       //?Leggo le distinte del giorno.
061400161214       //--------------------------------------------------------------
061500161214       BEGSR LeggiDistinte;
061600161221
061700161221         EndSQL = *off;
061800161215
061900161215         exec sql
062000161215         DECLARE DST cursor for
062100161214         SELECT DSTnfv from FIDST00F
062200161221         WHERE DSTfgs = :wFGS and DSTnpg = 4 and
062300161221               DSTdfv = :wDAT and DSTatb = ' ' and
062400161221               DSTpdr = :wPDR;
062500161214
062600161214       //?Apertura del cursore
062700161214         exec sql open DST;
062800161214         IF  sqlcode < 0;
062900161214           EndSQL = *on;
063000161214         ENDIF;
063100161214
063200161214         DOW  not EndSQL;
063300161214           exec sql
063400161214           fetch next from DST into :DSTnfv;
063500161214           IF  sqlcod = 100 or sqlcod < 0;
063600161214             EndSQL = *on;
063700161214             leave;
063800161214           ENDIF;
063900161221
064000161221           wNFV = DSTnfv;
064100161215           OkDistinta = *on;
064200161215           leave;
064300161214
064400161214         ENDDO;
064500161214
064600161215         exec sql close DST;
064700161214
064800161214       ENDSR;
064900161214
065000161214       //--------------------------------------------------------------
065100161214       //?Leggo le assenze del giorno.
065200161214       //--------------------------------------------------------------
065300161214       BEGSR LeggiAssenze;
065400161215
065500161215         EndSQL = *off;
065600161215
065700161215         exec sql
065800161215         DECLARE AGA cursor for
065900161219         SELECT AGAcau, AGAatn from FIAGA00F
066000161221         WHERE AGApdr = :wPDR and AGAdat = :wDAT and
066100161215               AGAatb = ' ';
066200161215
066300161215       //?Apertura del cursore
066400161215         exec sql open AGA;
066500161215         IF  sqlcode < 0;
066600161215           EndSQL = *on;
066700161215         ENDIF;
066800161215
066900161215         DOW  not EndSQL;
067000161215           exec sql
067100161219           fetch next from AGA into :AGAcau, :AGAatn;
067200161215           IF  sqlcod = 100 or sqlcod < 0;
067300161215             EndSQL = *on;
067400161215             leave;
067500161215           ENDIF;
067600161221
067700161221           wCAU = AGAcau;
067800161221           wATN = AGAatn;
067900161215           OkAssenza = *on;
068000161215           leave;
068100161215
068200161215         ENDDO;
068300161215
068400161215         exec sql close AGA;
068500161214
068600161214       ENDSR;
068700161215
068800161215       //--------------------------------------------------------------
068900161215       //?Inizializzazione videata S01.
069000161215       //--------------------------------------------------------------
069100161215       BEGSR InzS01;
069200161219
069300161219         EndS01 = *off;
069400161215
069500161215       //?Pulizia subfile
069600161215         exsr PulSfl;
069700161215
069800161215       //?Leggo file di work per caricare il subfile
069900161215         exsr LeggiWork;
070000161219
070100161219         SflEnd = *on;
070200161228         savS01nrr = S01nrr;
070300161219
070400161219       //?Imposto il posizionamento al primo rcd
070500161219         IF  S01nrr > 0;
070600161219           V01rcd = 1;
070700161219         ELSE;
070800161219           clear V01rcd;
070900161219         ENDIF;
071000161219
071100161219         V01csr = V01rcd;
071200161215
071300161215       ENDSR;
071400161214
071500161214       //--------------------------------------------------------------
071600161219       //?Pulisco il Subfile.
071700161214       //--------------------------------------------------------------
071800161214       BEGSR PulSfl;
071900161214
072000161214         SflDsp    = *on;
072100161214         SflDspCtl = *on;
072200161214         write LVF3C01;
072300161214         SflDspCtl = *off;
072400161214         SflEnd    = *off;
072500161214
072600161228         clear savS01Nrr;
072700161214         clear S01Nrr;
072800161214         clear V01rcd;
072900161219         clear V01csr;
073000161214         clear V01msg;
073100161214         ErrMessage  = *off;
073200161214         ErrGenerico = *off;
073300161219
073400161219         WindDspF = IndDspF;
073500161219         reset WindDspFb;
073600161219         IndDspF  = WindDspF;
073700161214
073800161214       ENDSR;
073900161215
074000161215       //--------------------------------------------------------------
074100161215       //?Leggo il file di Work.
074200161215       //--------------------------------------------------------------
074300161215       BEGSR LeggiWork;
074400161219
074500161219         EndS01 = *off;
074600161219
074700161219       //?Dichiarazione cursore
074800161219         exec sql
074900161219         DECLARE WRK cursor for
075000161219         SELECT * from WFADS01L
075100161219         ORDER BY WFAfgs, WFApdr, WFAdat;
075200161219
075300161219         //?Apertura del cursore
075400161219         exec sql
075500161219         open WRK;
075600161219
075700161219         IF  sqlcode < 0;
075800161219           EndS01 = *on;
075900161219           exec sql close WRK;
076000161219           leavesr;
076100161219         ENDIF;
076200161219
076300161219         DOW  not EndS01;
076400161219           exec sql
076500161219           fetch next from WRK into :WFADS00F;
076600161219           IF  sqlcod = 100 or sqlcod < 0;
076700161219             EndS01 = *on;
076800161219             leave;
076900161219           ENDIF;
077000161221
077100161221         //?Verifico se c'è ancora la discordanza
077200161221           wDAT = WFAdat;
077300161221           wPDR = WFApdr;
077400161221           exsr Controlla;
077500161221
077600161221         //?Se non c'è più discordanza cancello il rcd
077700161221           IF  (OkDistinta and not OkAssenza) or
077800161221               (not OkDistinta and OkAssenza);
077900170116             chain (WFAfgs:WFApdr:WFAdat) WFADS01L;
078000161221             IF  %found(WFADS01L);
078100161221               delete WFADS000;
078200161221               iter;
078300161221             ENDIF;
078400161221           ENDIF;
078500161219
078600161219         //?Carico i dati nel subfile
078700161220           exsr CaricaS01;
078800161219
078900161219         ENDDO;
079000161219
079100161219         exec sql
079200161219         close WRK;
079300161215
079400161215       ENDSR;
079500161219
079600161219       //--------------------------------------------------------------
079700161219       //?Carico i dati nel Subfile.
079800161219       //--------------------------------------------------------------
079900161220       BEGSR CaricaS01;
080000161219
080100170116         SflNxtChg = *off;
080200161228         clear LVF3S01;
080300161219         PosCurOPZ = *off;
080400161219
080500161219       //?Imposto i campi da WFADS
080600161219         clear V01dpdr;
080700161228         V01pdr = WFApdr;
080800161219         chain (k_APDtip:WFApdr) FIAPD01L;
080900161219         IF  %found(FIAPD01L);
081000161220           V01dpdr = APDrsc;
081100161219         ENDIF;
081200161219
081300161220         Data_ISO = %date(WFAdat:*iso);
081400161219         Data_EUR = Data_ISO;
081500161219         V01data = %dec(Data_EUR);
081600161219
081700161221         IF  WFAnfv > *zeros;
081800161219           V01dst = 'SI';
081900161219         ELSE;
082000161219           clear V01dst;
082100161219         ENDIF;
082200161219
082300161219         clear V01dcau;
082400161228         V01cau = WFAcau;
082500161219         clear dACA;
082600170126         clear TIBS02DS;
082700161219         T02mod = 'C';
082800161219         T02cod = 'ACA';
082900161219         T02ke1 = V01cau;
083000161219         T02sif = KNSIF;
083100161219         TNTBE_RicercaControllo (kpjba : tibs02ds);
083200161219         IF  T02err = *blanks;
083300161219           dACA = T02uni;
083400161219         ENDIF;
083500161219         V01dcau = §ACAdes;
083600161219
083700161219         V01atn = WFAatn;
083800161219
083900161219         S01nrr += 1;
084000161219         write LVF3S01;
084100161219
084200161219       ENDSR;
084300161219
084400161219       //--------------------------------------------------------------
084500161219       //?Gestione tasto funzionale F05 da Subfile S01.
084600161221       //?F05=Refresh
084700161219       //--------------------------------------------------------------
084800161219       BEGSR F05S01;
084900161221
085000161221       //?Come prima cosa devo pulire il file di work
085100161221         exec sql
085200161221         DELETE from QTEMP/WFADS00F;
085300161219
085400161221       //?Riparto dalla lettura degli autisti
085500161221         exsr LeggiAutisti;
085600161221
085700161221       //?Quindi ricarico il subfile con il file di work aggiornato
085800161221         wInzS01 = *on;
085900161219
086000161219       ENDSR;
086100161228
086200161228       //--------------------------------------------------------------
086300161228       //?Gestione tasto funzionale F06 da Subfile S01.
086400161228       //?F06=Conferma
086500161228       //--------------------------------------------------------------
086600161228       BEGSR F06S01;
086700161228
086800161228         readc LVF3S01;
086900161228
087000161228         DOW  not %eof(FNLVF3D);
087100161228           SflNxtChg = *off;
087200161228           V01rcd    = S01nrr;
087300161228           PosCurOPZ = *off;
087400161228
087500170116         //?Le opzioni 1 - 2 - 3 - 4
087600161228         //?Posso farle solo se non ho già un'assenza
087700170117           IF  V01opz <> 'A' and V01opz <> *blanks and
087800170117               V01cau <> *blanks;
087900161228             ErrMessage  = *on;
088000161228             ErrGenerico = *on;
088100161230             PosCurOPZ = *on;
088200161228             V01opz = 'E';
088300161228           ENDIF;
088400161228
088500170116         //?Opzione A
088600161228         //?Posso farla solo se ho un'assenza
088700161228           IF  V01opz = 'A' and V01cau = *blanks;
088800161228             ErrMessage  = *on;
088900161228             ErrGenerico = *on;
089000161230             PosCurOPZ = *on;
089100161228             V01opz = 'E';
089200161228           ENDIF;
089300161228
089400161228         //?Se non ho errore eseguo l'opzione immessa
089500161228           IF  not ErrMessage and V01opz <> *blanks;
089600161228             clear FNLVF1DS;
089700161228             fnlvf1ds.ILVF1pdr = V01pdr;
089800161228             Data_EUR = %date(V01data:*eur);
089900161228             Data_ISO = Data_EUR;
090000161228             fnlvf1ds.ILVF1dat = %dec(Data_ISO);
090100170116             fnlvf1ds.ILVF1ric = V01opz;
090200161228             IF  V01opz = 'A';
090300161228               fnlvf1ds.ILVF1atn = V01atn;
090400170116             ENDIF;
090500161228             fnlvf1r (kpjba:FNLVF1DS);
090600161228             IF  fnlvf1ds.OLVF1err <> *blanks;
090700161228               ErrMessage  = *on;
090800161228               ErrGenerico = *on;
090900170206               V01Msg = fnlvf1ds.OLVF1msg;
091000161228               V01opz = 'E';
091100161228             ENDIF;
091200161230             PosCurOPZ = *off;
091300161230             clear V01opz;
091400161228           ENDIF;
091500161228
091600161228           //?Aggiornamento sfl
091700161228           V01csr = V01rcd;
091800161228           update LVF3S01;
091900161228           readc LVF3S01;
092000161228         ENDDO;
092100161228
092200161228       //?Se non ci sono stati errori ricarico tutto
092300161228         IF  not ErrMessage;
092400161228           exsr F05S01;
092500161228         ENDIF;
092600161228
092700161228       ENDSR;
092800170116
092900170116       //--------------------------------------------------------------
093000170116       //?Gestione tasto funzionale F12 da Subfile S01
093100170116       //?F12=Ritorno
093200170116       //--------------------------------------------------------------
093300170116       BEGSR F12S01;
093400170116
093500170116       //?Chiusura del programma
093600170116         Fine = *on;
093700170116
093800170116       ENDSR;
093900161228
094000161228       //--------------------------------------------------------------
094100161228       //?Gestione tasto funzionale F13 da Subfile S01.
094200161228       //?F13=Ripeti Opzione
094300161228       //--------------------------------------------------------------
094400161228       BEGSR F13S01;
094500170116
094600170116       //?Reperimento pagina del sfl (e Nrr del suo primo record)?
094700170116       //?dalla posizione del cursore?
094800170116         wPag = V01csr / wSflPag;
094900170116         wRig = %rem(V01csr : wSflPag);
095000170116         IF  wRig > *zero;
095100170116           wRig = (wPag * wSflPag) + 1;
095200170116         ELSE;                           // -?ultimo rec. della pag.?
095300170116           wRig = (wPag * wSflPag) - wSflPag + 1;
095400170116         ENDIF;
095500170116         savV01csr = wRig;
095600170116
095700170116       //?Controllo le opzioni inserite
095800170117         PosCurOPZ = *off;
095900170116
096000170116       //?Reperimento della 1ª opzione inserita?
096100170116       //?nella pagina dov'è il cursore?
096200170116         readc LVF3S01;
096300170116         DOW  not %eof(FNLVF3D) and V01opz <> *blanks and
096400170116              S01nrr < savV01csr;
096500170116           SflNxtChg = *on;
096600170116           update LVF3S01;
096700170116           readc LVF3S01;
096800170116         ENDDO;
096900170116         IF  not %eof(FNLVF3D) and V01opz <> *blanks;
097000170116           SflNxtChg = *on;
097100170116           update LVF3S01;
097200170116         ENDIF;
097300170116
097400170116       //?Nessuna opzione nella pagina
097500170116         IF  %eof(FNLVF3D) or
097600170116             S01nrr < savV01csr or
097700170116             S01nrr > (savV01csr + wSflPag - 1);
097800170116           V01rcd = savV01csr;
097900170116           V01csr = savV01csr;
098000170116           clear ForzaOpz;
098100170116           clear savV01opz;
098200170116           clear savV01op1;
098300170116           clear savV01cs1;
098400170116           clear savV01cs2;
098500170116           ErrGenerico = *on;
098600170116           ErrMessage  = *on;
098700170116           PosCurOPZ   = *on;
098800170116           V01msg = Msg(03);
098900170116           leavesr;
099000170116         ENDIF;
099100170116
099200170116       //?Memorizzazione 1ª opzione nella pagina?
099300170116         savV01csr = S01nrr;
099400170116         savV01opz = V01opz;
099500170116         IF  S01nrr <> savV01cs1 or
099600170116             V01opz <> savV01op1;
099700170116           clear ForzaOpz;
099800170116         ENDIF;
099900170116         savV01cs1 = S01nrr;
100000170116         savV01op1 = V01opz;
100100170116
100200170116       //?Reperimento delle altre eventuali opzioni?
100300170116         readc LVF3S01;
100400170116         DOW  not %eof(FNLVF3D);
100500170116
100600170116         //?Aggiornamento subfile?
100700170116           IF  V01opz <> *blanks;
100800170116             SflNxtChg = *on;
100900170116             update LVF3S01;
101000170116           ENDIF;
101100170116
101200170116         //?Reperita una 2ª opzione?
101300170116           SELECT;
101400170116         //?Vuota!?
101500170116           WHEN  V01opz = *blanks;
101600170116         //?1ª opzione nella pagina del cursore  e?
101700170116         //?2ª opzione successiva diversa:?
101800170116         //?messaggio di avviso di "ricopertura"?
101900170116           WHEN  (savV01csr >= wRig       and
102000170116                  S01nrr    >  savV01csr) and
102100170116                  V01opz    <> savV01opz  and
102200170116                  not ForzaOpz;
102300170116             ForzaOpz = *on;
102400170116             savV01cs2 = S01nrr;
102500170116             V01csr    = savV01csr;
102600170116             V01rcd    = savV01csr;
102700170116             ErrGenerico = *on;
102800170116             ErrMessage  = *on;
102900170116             PosCurOPZ   = *on;
103000170116             V01msg = Msg(04);
103100170116             leavesr;
103200170116
103300170116         //?2ª opzione sul record dove è posizionato il cursore?
103400170116         //?(la nuova da considerare per la duplicazione)?
103500170116           WHEN  V01csr = S01nrr;
103600170116             savV01csr = S01nrr;
103700170116             savV01opz = V01opz;
103800170116
103900170116         //?Più opzioni nella stessa videata e?
104000170116         //?cursore su nessuna delle opzioni?
104100170116         //?(riposizionare il cursore)?
104200170116         //?Se stessa opzione in videate diverse: vince la?
104300170116         //?posizione della prima opzione.?
104400170116           WHEN  V01csr <> savV01csr and
104500170116                 V01csr <> S01nrr    and
104600170116                 S01nrr <  wRig + wSflPag;
104700170116             savV01cs2 = S01nrr;
104800170116             V01rcd    = savV01csr;
104900170116             V01csr    = savV01csr;
105000170116             ErrGenerico = *on;
105100170116             ErrMessage  = *on;
105200170116             PosCurOPZ   = *on;
105300170116             V01msg = Msg(05);
105400170116             leavesr;
105500170116
105600170116           ENDSL;
105700170116
105800170116           readc LVF3S01;
105900170116
106000170116         ENDDO;
106100170116
106200170116       //?Opzione NON abilitata?
106300170116         IF  savV01opz <> '1' and savV01opz <> '2' and
106400170116             savV01opz <> '3' and savV01opz <> '4' and
106500170116             savV01opz <> 'A';
106600170116           V01csr = savV01csr;
106700170116           V01rcd = savV01csr;
106800170116           ErrGenerico = *on;
106900170116           ErrMessage  = *on;
107000170116           PosCurOPZ   = *on;
107100170116           V01msg = Msg(02);
107200170116           leavesr;
107300170116         ENDIF;
107400170117
107500170117         PosCurOPZ = *off;
107600170116
107700170116       //?Ripetizione opzione sul resto del subfile?
107800170116         S01nrr = savV01csr + 1;
107900170116         chain  S01nrr  LVF3S01;
108000170116         DOW  %found(FNLVF3D);
108100170116           SflNxtChg = *on;
108200170116           V01opz = savV01opz;
108300170116           update LVF3S01;
108400170116           S01nrr += 1;
108500170116           chain S01nrr LVF3S01;
108600170116         ENDDO;
108700161228
108800161228       ENDSR;
108900161219
109000161219       //--------------------------------------------------------------
109100161219       //?Gestione opzioni Subfile S01.
109200161219       //--------------------------------------------------------------
109300161219       BEGSR OpzS01;
109400170116
109500170116         clear savV01csr;
109600161219
109700161219         readc LVF3S01;
109800161219
109900161220         DOW  not %eof(FNLVF3D);
110000161219
110100161219           SflNxtChg = *off;
110200161219           WindDspF  = IndDspF;
110300161219           reset WindDspFb;
110400161219           IndDspF   = WindDspF;
110500161219           V01rcd    = S01nrr;
110600161219
110700161219           PosCurOPZ = *off;
110800161219
110900161219           SELECT;
111000161219         //?- Nessuna opzione
111100161219           WHEN  V01opz = *blank;
111200161219
111300170116         //?- Immissione Assenza (1-2-3-4)
111400170116           WHEN  V01opz = '1' or V01opz = '2' or
111500170116                 V01opz = '3' or V01opz = '4';
111600161219           //?Posso farla solo se non ho già un'assenza
111700170116             IF  V01cau <> *blanks;
111800170116               ErrMessage  = *on;
111900170116               ErrGenerico = *on;
112000170116               PosCurOPZ   = *on;
112100170116               V01msg = Msg(02);
112200170116               V01msg = %trim(V01msg) + ' Assenza già presente';
112300170116             ENDIF;
112400161219
112500161219         //?- A = Annulla
112600161219           WHEN  V01opz = 'A';
112700161219           //?Posso farla solo se ho un'assenza
112800170116             IF  V01cau = *blanks;
112900170116               ErrMessage  = *on;
113000170116               ErrGenerico = *on;
113100170116               PosCurOPZ   = *on;
113200170116               V01msg = Msg(02);
113300170116               V01msg = %trim(V01msg) + ' Assenza non presente';
113400170116             ENDIF;
113500161219
113600161219           OTHER;
113700161219             ErrMessage  = *on;
113800161219             ErrGenerico = *on;
113900161219             PosCurOPZ   = *on;
114000170109             V01msg = Msg(02);
114100161219           ENDSL;
114200161219
114300161219           //?Aggiornamento sfl
114400170116           SflNxtChg = *on;
114500170116           V01csr    = V01rcd;
114600161219           update LVF3S01;
114700161219
114800161219           IF  ErrMessage or ErrGenerico;
114900161219             leave;
115000161219           ENDIF;
115100161219
115200161219           readc LVF3S01;
115300161219
115400161219         ENDDO;
115500161219
115600161219       ENDSR;
115700161219
115800161219       //--------------------------------------------------------------
115900170116       //?Opzione "I" immissione assenza Subfile S01.
116000161219       //--------------------------------------------------------------
116100170116       BEGSR OpzioneI;
116200161220
116300161221       //?Se OK assenza errore
116400161220         IF  V01cau <> *blanks;
116500161220           ErrMessage  = *on;
116600161220           ErrGenerico = *on;
116700161220           PosCurOPZ   = *on;
116800161220           V01msg = Msg(02);
116900161220           V01msg = %trim(V01msg) + ' Assenza già presente';
117000161220           leavesr;
117100161220         ENDIF;
117200161219
117300161219       //?Devo inserire l'assenza
117400161220         clear FNLVF1DS;
117500170116         fnlvf1ds.ILVF1ric = V01opz;
117600161220         fnlvf1ds.ILVF1pdr = V01pdr;
117700161220         Data_EUR = %date(V01data:*eur);
117800161220         Data_ISO = Data_EUR;
117900161220         fnlvf1ds.ILVF1dat = %dec(Data_ISO);
118000161220         fnlvf1r (kpjba:FNLVF1DS);
118100161220         IF  fnlvf1ds.OLVF1err <> *blanks;
118200161219           ErrMessage  = *on;
118300161219           ErrGenerico = *on;
118400161219           PosCurOPZ   = *on;
118500161220           V01msg = fnlvf1ds.OLVF1msg;
118600161219           leavesr;
118700161219         ENDIF;
118800161219
118900161219       ENDSR;
119000161220
119100161220       //--------------------------------------------------------------
119200161220       //?Opzione "A" Subfile S01.
119300161220       //--------------------------------------------------------------
119400161220       BEGSR OpzioneA;
119500161220
119600161221       //?Se no OK assenza errore
119700161220         IF  V01cau = *blanks;
119800161220           ErrMessage  = *on;
119900161220           ErrGenerico = *on;
120000161220           PosCurOPZ   = *on;
120100161220           V01msg = Msg(02);
120200161220           V01msg = %trim(V01msg) + ' Assenza non presente';
120300161220           leavesr;
120400161220         ENDIF;
120500161220
120600161220       //?Devo annullare l'assenza
120700161220         clear FNLVF1DS;
120800170116         fnlvf1ds.ILVF1ric = V01opz;
120900161220         fnlvf1ds.ILVF1atn = V01atn;
121000161220         fnlvf1r (kpjba:FNLVF1DS);
121100161220         IF  fnlvf1ds.OLVF1err <> *blanks;
121200161220           ErrMessage  = *on;
121300161220           ErrGenerico = *on;
121400161220           PosCurOPZ   = *on;
121500161220           V01msg = fnlvf1ds.OLVF1msg;
121600161220           leavesr;
121700161220         ENDIF;
121800161220
121900161220       ENDSR;
122000161214
122100161214      /end-free
122200161214       //--------------------------------------------------------------
122300161214       //?Schiere a tempo di compilazione.
122400161214       //--------------------------------------------------------------
122500161214
122600170116** -  MSG --------------------------------------------------------------------*
122700161214Non è stata impostata la filiale gestione                                     01
122800161219Opzione errata                                                                02
122900170116Immettere un'opzione su questa pagina prima di premere F13                    03
123000170116Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa04
123100170116Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F1305
