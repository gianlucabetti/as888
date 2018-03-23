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
020100170126     d wDataAlfa       s              8a   inz
020200170126     d wDataAtt        s              8s 0 inz
020300161221     d wFGS            s                   like(DSTfgs) inz
020400161220     d wFLib           s             21a   inz
020500161220     d wLib            s             10a   inz
020600161221     d wNFV            s                   like(DSTnfv) inz
020700170116     d wPag            s              4  0 inz
020800161221     d wPDR            s                   like(APDpdr) inz
020900170116     d wRig            s              3  0 inz
021000161214
021100161214      //---------------------------------------------------------------
021200161214      //?Definizione procedure esterne.
021300161214      //---------------------------------------------------------------
021400161220      // - Inserimento Agenda
021500161220     d fnlvf1r         pr                  extpgm('FNLVF1R')
021600161220     d  kpjba                              likeds(KPJBA)
021700161220     d  fnlvf1ds                           likeds(FNLVF1DS)
021800161220
021900161214      // - Controllo Autista Accreditato
022000161214     d fnlvf9r         pr                  extpgm('FNLVF9R')
022100161214     d  fnlvf9ds                           likeds(FNLVF9DS)
022200161228
022300161228      // - Selezione singola causale assenza
022400161228     d tntbacar1       pr                  extpgm('TNTBACAR1')
022500161228     d   kpjba                             likeds(KPJBA)
022600161228     d   tntbacads                         likeds(TNTBACADS)
022700161214
022800161214      // - Aggiunge/Toglie gg/mm dalla data
022900161219     d xgiolav         pr                  extpgm('XGIOLAV')
023000161214     d  xgiolavds                          likeds(xgiolavds)
023100161214
023200161214      //---------------------------------------------------------------
023300161214      //?Definizione prototipi utilizzati.
023400161214      //---------------------------------------------------------------
023500161220      /copy gaitrasrc/srcprotopr,fnlv24r
023600161219      /copy gaitrasrc/srcprotopr,tibs02r
023700161219      /copy gaitrasrc/srcprotopr,tibs34r
023800161219      /copy gaitrasrc/srcprotopr,SYSTEM
023900161220      /copy gaitrasrc/srcprotopr,XSRDA8
024000161214
024100161214      //---------------------------------------------------------------
024200161214      //?Definizione key-list.
024300161214      //---------------------------------------------------------------
024400161220      // - File FIAPD01L?
024500161220     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
024600161220     d                                     prefix(k_) inz
024700161214
024800161214      //---------------------------------------------------------------
024900161214      //?Riepilogo indicatori.
025000161214      //---------------------------------------------------------------
025100161214
025200161214      //---------------------------------------------------------------
025300161214      //?M A I N - L I N E
025400161214      //---------------------------------------------------------------
025500161214     c     *ENTRY        plist
025600161214     c                   parm                    kpjba
025700161214     c                   parm                    fnlvf3ds
025800161214
025900161214      /free
026000161214
026100161214       //?Operazioni iniziali
026200161214       exsr RoutInz;
026300161215
026400161215       //?Leggo gli Autisti accreditati ad oggi
026500161215       //?e scrivo il file di WORK
026600161215       exsr LeggiAutisti;
026700161215
026800161215       //?Se richiamato per Gestione
026900161215       //?devo emettere la videata
027000161215       DOW  not Fine and fnlvf3ds.ILVF3trc = 'G';
027100161219         SELECT;
027200161219         WHEN  Video = 'S1';
027300161219           exsr GesS01;
027400161219         OTHER;
027500161219           Fine = *on;
027600161219         ENDSL;
027700161215       ENDDO;
027800161214
027900161214       //?Operazioni finali
028000161214       exsr RoutEnd;
028100161214
028200161214       //--------------------------------------------------------------
028300161214       //?Operazioni iniziali.
028400161214       //--------------------------------------------------------------
028500161214       BEGSR RoutInz;
028600161219
028700161219       //?Reperimento dati job
028800161219         exsr DatiJob;
028900170109
029000170109       //?Impostazione campi "fissi"
029100170109         T01pgm = SDSpgm;
029200161214
029300161214         clear fnlvf3ds.OLVF3disco;
029400161214         clear fnlvf3ds.OLVF3err;
029500161214         clear fnlvf3ds.OLVF3msg;
029600161214
029700161214       //?Se non c'è il tipo richiamo assumo 'C' di controllo
029800161214         IF  fnlvf3ds.ILVF3trc = *blanks;
029900161214           fnlvf3ds.ILVF3trc = 'C';
030000161214         ENDIF;
030100161214
030200161214       //?Se non c'è la filiale torno errore
030300161214         IF  fnlvf3ds.ILVF3fgs = 0;
030400161214           fnlvf3ds.OLVF3err = '1';
030500161214           fnlvf3ds.OLVF3msg = Msg(01);
030600161214           exsr RoutEnd;
030700161214         ENDIF;
030800161221         wFGS = fnlvf3ds.ILVF3fgs;
030900170126
031000170126       //?Cerco la data attivazione della funzione
031100170126         clear TIBS02DS;
031200170126         T02mod = 'C';
031300170126         T02sif = knsif;
031400170126         T02cod = 'VPO';
031500170126         T02ke1 = 'ANOMALIEAUT';
031600170206         T02ke2 = %editc(fnlvf3ds.ILVF3fgs:'X');
031700170126         TNTBE_RicercaControllo (kpjba:tibs02ds);
031800170126         IF  T02err = *blanks;
031900170126           wDataAlfa = %subst(T02uni:1:8);
032000170126         ENDIF;
032100170126         //?se non trovo con la filiale utente provo con 999 generico
032200170126         IF  wDataAlfa = *blanks;
032300170126           clear TIBS02DS;
032400170126           T02mod = 'C';
032500170126           T02sif = knsif;
032600170126           T02cod = 'VPO';
032700170126           T02ke1 = 'ANOMALIEAUT';
032800170126           T02ke2 = '999';
032900170126           TNTBE_RicercaControllo (kpjba:tibs02ds);
033000170126           IF  T02err = *blanks;
033100170126             wDataAlfa = %subst(T02uni:1:8);
033200170126           ENDIF;
033300170126         ENDIF;
033400170126       //?se non ho la data è come se la filiale non fosse stata
033500170126       //?attivata, quindi in caso di richiamo per solo controllo
033600170126       //?esco subito dal pgm riportando che tutto OK
033700170126         IF  fnlvf3ds.ILVF3trc = 'C' and wDataAlfa = *blanks;
033800170126           clear fnlvf3ds.OLVF3disco;
033900170126           exsr RoutEnd;
034000170126         ENDIF;
034100170126         wDataAtt = %dec(wDataAlfa:8:0);
034200161214
034300161214       //?Carico gg lavorativi da tabella 5A - AUT
034400161214         clear ds5AAUT;
034500161214         exec sql
034600161214         SELECT tabel00f.* into :TABEL00F
034700161214         FROM   tabel00f
034800161214         WHERE  TBLkut = 1 and TBLcod = '5A' and
034900161214                substr(TBLkey, 1, 3) = 'AUT' and
035000161214                TBLflg = '';
035100161214         IF  sqlcod = 0;
035200161214           ds5AAUT = TBLuni;
035300161214         ENDIF;
035400161214
035500161214       //?Imposto Oggi
035600161214         Oggi = %dec(%date());
035700161214
035800161214       //?Calcolo i gg lavorativi indietro da oggi
035900161214         wDatada = Oggi;
036000161214         exsr CalcolaData;
036100170126
036200170126       //?Se ho la data di attivazione
036300170126       //?e il pgm è stato richiamto solo per controllo
036400170126       //?e la data più alta di calcolo anomalie è più piccola
036500170126       //?della data attivazione esco subito dal pgm riportando che tutto OK
036600170126         IF  fnlvf3ds.ILVF3trc = 'C' and
036700170126             wDataAtt > skData(1);
036800170126           clear fnlvf3ds.OLVF3disco;
036900170126           exsr RoutEnd;
037000170126         ENDIF;
037100161219
037200161214       //?Apro il file video se richiamato per Gestione
037300161214         IF  fnlvf3ds.ILVF3trc = 'G';
037400161214           open FNLVF3D;
037500161219           Video = 'S1';
037600161219           wInzS01 = *on;
037700161214         ENDIF;
037800161219
037900161219       //?Determino la libreria
038000161219         IF  %subst(knsif : 7 : 1) = 'P';
038100170126           wLib = 'UNITRAGRP';
038200161219         ELSE;
038300170126           wLib = 'UNITRAGRU';
038400161219         ENDIF;
038500161215
038600161215       //?Genero il file di work in qtemp
038700161220         cmd = 'CRTDUPOBJ OBJ(WFADS*) FROMLIB(' +
038800161220                %trim(wLib) + ') ' +
038900161220               'OBJTYPE(*FILE) TOLIB(QTEMP)';
039000161229         callp(e) ExecuteCommand (cmd);
039100161229         IF  %error;
039200161229           cmd = 'CLRPFM QTEMP/WFADS00F';
039300161229           ExecuteCommand (cmd);
039400161229         ENDIF;
039500161219       //?Apro il file
039600161219         open WFADS01L;
039700161220
039800161220       //?Dati Fissi
039900161220         k_APDtip = 'A';
040000161214
040100161214       ENDSR;
040200161215
040300161215       //--------------------------------------------------------------
040400161215       //?Leggo gli Autisti accreditati ad oggi
040500161215       //?e scrivo file di WORK.
040600161215       //--------------------------------------------------------------
040700161215       BEGSR LeggiAutisti;
040800161220
040900161220         wEoF = *off;
041000161220
041100161220       //?Imposto range AUT da caricare in base alla FGS
041200161221         wautini = wFGS * 10000;
041300161220         wautfin = wautini + 9999;
041400161220         k_APDpdr = wautini;
041500161215
041600161221       //?Leggo gli autisti accreditati ad oggi
041700161220         setll %kds(kFIAPD01) FIAPD01L;
041800161220         DOW  not wEoF;
041900161220           reade (k_APDtip) FIAPD01L;
042000161220         //?Fine file
042100161220           IF  %eof(FIAPD01L);
042200161220             wEoF = *on;
042300161220             leave;
042400161220           ENDIF;
042500161220         //?Leggo solo gli autisti della filiale gestione ricevuta
042600161220           IF  APDpdr > wautfin;
042700161220             wEof = *on;
042800161220             leave;
042900161220           ENDIF;
043000170116           dAPDflr = APDflr;
043100161215         //?Escludo gli annullati
043200161220           IF  APDatb <> *blanks;
043300161220             iter;
043400161220           ENDIF;
043500170116         //?Escludo gli autisti con attività speciali
043600170116           IF  §APDspec = 'S';
043700170116             iter;
043800170116           ENDIF;
043900161220         //?Escludo i non accreditati
044000161220           clear FNLVF9DS;
044100161220           fnlvf9ds.ILVF9pdr = APDpdr;
044200161220           fnlvf9ds.ILVF9data = Oggi;
044300161220           fnlvf9r (fnlvf9ds);
044400161220           IF  fnlvf9ds.OLVF9ok = *off;
044500161220             iter;
044600161220           ENDIF;
044700161220
044800161221         //?Il controllo va fatto su tutti i gg previsti in tabella
044900161221           xx = 1;
045000161221           FOR xx by 1 to §5Aggass;
045100161221
045200161221             wDAT = skData(xx);
045300161221             wPDR = APDpdr;
045400161221
045500170126           //?se la data da controllare è < data attivazione
045600170126           //?provo con altro giorno
045700170126             IF  wDAT < wDataAtt;
045800170126               iter;
045900170126             ENDIF;
046000170126
046100161221           //?Controllo se ha distinta/Assenza
046200170126             exsr Controlla;
046300161221
046400161221             SELECT;
046500161221           //?OK se ha la distinta e non l'assenza
046600161221             WHEN  OkDistinta and not OkAssenza;
046700161221               iter;
046800161221           //?OK se non ha la distinta ed ha l'assenza
046900161221             WHEN  not OkDistinta and OkAssenza;
047000161221               iter;
047100161221             ENDSL;
047200161221
047300161221           //?Se richiamato per solo controllo
047400161221           //?e vedo già che c'è una discordanza esco dal pgm
047500161221             IF  fnlvf3ds.ILVF3trc = 'C';
047600170130               fnlvf3ds.OLVF3disco = 'S';
047700161221               exsr RoutEnd;
047800161221             ENDIF;
047900161220
048000161221           //?Scrivo il file di work
048100161221             exsr ScriviWrkf;
048200161221           ENDFOR;
048300161220
048400161215         ENDDO;
048500161215
048600161219         feod WFADS01L;
048700161215
048800161215       ENDSR;
048900161215
049000161215       //--------------------------------------------------------------
049100161219       //?Gestione videata S01.
049200161215       //--------------------------------------------------------------
049300161215       BEGSR GesS01;
049400161215
049500161215       //?Inizializzazione videata
049600161215         IF  wInzS01;
049700161215           exsr InzS01;
049800161215           wInzS01 = *off;
049900161215         ENDIF;
050000161215
050100161215       //?Visualizzazione del SFL (se ci sono dati)
050200161215         SflDsp = (S01nrr <= *zeros);
050300161215
050400161215       //?Posizionamento cursore
050500161219         V01rcd = V01csr;
050600161215
050700161215       //?Emissione Testata e Piede con tasti funzionali abilitati
050800161215         write LVF3T01;
050900161215         write LVF3P01;
051000161215
051100161215       //?Emissione videata
051200161215         exfmt LVF3C01;
051300161215         ErrMessage  = *off;
051400161215         ErrGenerico = *off;
051500161215         clear V01msg;
051600170116
051700170116         IF  dsp_aid <> c_F12 and
051800170116             dsp_aid <> c_F13;
051900170116           clear savV01csr;
052000170116           clear savV01cs1;
052100170116           clear savV01cs2;
052200170116           clear savV01opz;
052300170116           clear savV01op1;
052400170116         ENDIF;
052500161215
052600161215         SELECT;
052700161219
052800161228       //?F05=Refresh
052900161219         WHEN  dsp_aid = c_F05;
053000161219           exsr F05S01;
053100161228
053200161228       //?F06=Conferma
053300161228         WHEN  dsp_aid = c_F06;
053400170118           exsr OpzS01;
053500170118           IF  ErrGenerico;
053600170118             leavesr;
053700170118           ENDIF;
053800170117           exsr F06S01;
053900170116
054000170116       //?F12=Ritorno
054100170116         WHEN  dsp_aid = c_F12;
054200170116           exsr F12S01;
054300161228
054400161228       //?F13=Ripeti Opzione
054500161228         WHEN  dsp_aid = c_F13;
054600161228           exsr F13S01;
054700161215
054800161215       //?Enter
054900161215         OTHER;
055000161219         //?Controllo dati
055100161219           IF  V01csr > *zero;
055200161219             exsr OpzS01;
055300161219             IF  ErrGenerico;
055400161219               leavesr;
055500161219             ENDIF;
055600161219           ENDIF;
055700170111           IF  V01csr = *zero;
055800170111             V01csr = savS01nrr;
055900170111           ENDIF;
056000161215
056100161215         ENDSL;
056200161215
056300161215       ENDSR;
056400161215
056500161215       //--------------------------------------------------------------
056600161215       //?Operazioni finali.
056700161215       //--------------------------------------------------------------
056800161215       BEGSR RoutEnd;
056900161215
057000161215       //?Cancello file di work dalla qtemp
057100161220         IF  %open(WFADS01L);
057200161220           close WFADS01L;
057300161220           cmd = 'DLTF FILE(QTEMP/WFADS01L)';
057400161220           ExecuteCommand (cmd);
057500161220           cmd = 'DLTF FILE(QTEMP/WFADS00F)';
057600161220           ExecuteCommand (cmd);
057700161220         ENDIF;
057800161215
057900161215         *inLR = *on;
058000161215         return;
058100161215
058200161215       ENDSR;
058300161219
058400161219       //--------------------------------------------------------------
058500161219       //?Reperimento Dati del job (Utente/Operativi).
058600161219       //--------------------------------------------------------------
058700161219       BEGSR DatiJob;
058800161219
058900161219         in(E) §AzUte;
059000161219         IF  NOT %error;
059100161219           in(E) §DatiUte;
059200161219         ENDIF;
059300161219         IF  %error or RSut = *blanks;
059400170126           clear TIBS34DS;
059500161219           tibs34r(tibs34ds);
059600161219           in §AzUte;
059700161219           in §DatiUte;
059800161219         ENDIF;
059900161219
060000161219       ENDSR;
060100161215
060200161215       //--------------------------------------------------------------
060300161215       //?Calcolo la data da elaborare.
060400161215       //--------------------------------------------------------------
060500161215       BEGSR CalcolaData;
060600161215
060700161215         xx = 1;
060800161215         FOR xx by 1 to §5Aggass;
060900161215           clear XGIOLAVDS;
061000161220           IXGLdata = wDatada;
061100161220           IXGLsub  = 'S';
061200161220           IXGLgg   = xx;
061300161215           IXGLlav  = 'S';
061400161215           IXGLpa   = 'P';
061500161221           IXGLfil  = wFGS;
061600161215           XGIOLAV (xgiolavds);
061700161215           IF  OXGLerr = *blanks;
061800161215             skData(xx) = OXGLdata;
061900161215           ELSE;
062000161219             Data_ISO = %date(wDatada:*iso);
062100161220             Data_ISO = Data_ISO - %days(xx);
062200161219             skData(xx) = %dec(Data_ISO);
062300161215           ENDIF;
062400161215         ENDFOR;
062500161215
062600161215       ENDSR;
062700161221
062800161221       //--------------------------------------------------------------
062900161221       //?Controllo se ci sono discordanze
063000161221       //--------------------------------------------------------------
063100161221       BEGSR Controlla;
063200161221
063300161221         OkAssenza = *off;
063400161221         OkDistinta = *off;
063500161221         clear wNFV;
063600161221         clear wATN;
063700161221         clear wCAU;
063800161221
063900161221       //?Leggo le Distinte del giorno
064000161221       //?e aggiorno file di work
064100161221         exsr LeggiDistinte;
064200161221
064300161221       //?Leggo le Assenze del giorno
064400161221       //?e aggiorno file di work
064500161221         exsr LeggiAssenze;
064600161221
064700161221       ENDSR;
064800161215
064900161215       //--------------------------------------------------------------
065000161215       //?Scrivo il file di Work.
065100161215       //--------------------------------------------------------------
065200161215       BEGSR ScriviWrkf;
065300161215
065400161221       //?Scrivo
065500161221         clear WFADS000;
065600161221         WFAfgs = wFGS;
065700161221         WFApdr = APDpdr;
065800161221         WFAdat = skData(xx);
065900161221         WFAnfv = wNFV;
066000161221         WFAcau = wCAU;
066100161221         WFAatn = wATN;
066200161221         write WFADS000;
066300161215
066400161215       ENDSR;
066500161214
066600161214       //--------------------------------------------------------------
066700161214       //?Leggo le distinte del giorno.
066800161214       //--------------------------------------------------------------
066900161214       BEGSR LeggiDistinte;
067000161221
067100161221         EndSQL = *off;
067200161215
067300161215         exec sql
067400161215         DECLARE DST cursor for
067500161214         SELECT DSTnfv from FIDST00F
067600161221         WHERE DSTfgs = :wFGS and DSTnpg = 4 and
067700161221               DSTdfv = :wDAT and DSTatb = ' ' and
067800161221               DSTpdr = :wPDR;
067900161214
068000161214       //?Apertura del cursore
068100161214         exec sql open DST;
068200161214         IF  sqlcode < 0;
068300161214           EndSQL = *on;
068400161214         ENDIF;
068500161214
068600161214         DOW  not EndSQL;
068700161214           exec sql
068800161214           fetch next from DST into :DSTnfv;
068900161214           IF  sqlcod = 100 or sqlcod < 0;
069000161214             EndSQL = *on;
069100161214             leave;
069200161214           ENDIF;
069300161221
069400161221           wNFV = DSTnfv;
069500161215           OkDistinta = *on;
069600161215           leave;
069700161214
069800161214         ENDDO;
069900161214
070000161215         exec sql close DST;
070100161214
070200161214       ENDSR;
070300161214
070400161214       //--------------------------------------------------------------
070500161214       //?Leggo le assenze del giorno.
070600161214       //--------------------------------------------------------------
070700161214       BEGSR LeggiAssenze;
070800161215
070900161215         EndSQL = *off;
071000161215
071100161215         exec sql
071200161215         DECLARE AGA cursor for
071300161219         SELECT AGAcau, AGAatn from FIAGA00F
071400161221         WHERE AGApdr = :wPDR and AGAdat = :wDAT and
071500161215               AGAatb = ' ';
071600161215
071700161215       //?Apertura del cursore
071800161215         exec sql open AGA;
071900161215         IF  sqlcode < 0;
072000161215           EndSQL = *on;
072100161215         ENDIF;
072200161215
072300161215         DOW  not EndSQL;
072400161215           exec sql
072500161219           fetch next from AGA into :AGAcau, :AGAatn;
072600161215           IF  sqlcod = 100 or sqlcod < 0;
072700161215             EndSQL = *on;
072800161215             leave;
072900161215           ENDIF;
073000161221
073100161221           wCAU = AGAcau;
073200161221           wATN = AGAatn;
073300161215           OkAssenza = *on;
073400161215           leave;
073500161215
073600161215         ENDDO;
073700161215
073800161215         exec sql close AGA;
073900161214
074000161214       ENDSR;
074100161215
074200161215       //--------------------------------------------------------------
074300161215       //?Inizializzazione videata S01.
074400161215       //--------------------------------------------------------------
074500161215       BEGSR InzS01;
074600161219
074700161219         EndS01 = *off;
074800161215
074900161215       //?Pulizia subfile
075000161215         exsr PulSfl;
075100161215
075200161215       //?Leggo file di work per caricare il subfile
075300161215         exsr LeggiWork;
075400161219
075500161219         SflEnd = *on;
075600161228         savS01nrr = S01nrr;
075700161219
075800161219       //?Imposto il posizionamento al primo rcd
075900161219         IF  S01nrr > 0;
076000161219           V01rcd = 1;
076100161219         ELSE;
076200161219           clear V01rcd;
076300161219         ENDIF;
076400161219
076500161219         V01csr = V01rcd;
076600161215
076700161215       ENDSR;
076800161214
076900161214       //--------------------------------------------------------------
077000161219       //?Pulisco il Subfile.
077100161214       //--------------------------------------------------------------
077200161214       BEGSR PulSfl;
077300161214
077400161214         SflDsp    = *on;
077500161214         SflDspCtl = *on;
077600161214         write LVF3C01;
077700161214         SflDspCtl = *off;
077800161214         SflEnd    = *off;
077900161214
078000161228         clear savS01Nrr;
078100161214         clear S01Nrr;
078200161214         clear V01rcd;
078300161219         clear V01csr;
078400161214         clear V01msg;
078500161214         ErrMessage  = *off;
078600161214         ErrGenerico = *off;
078700161219
078800161219         WindDspF = IndDspF;
078900161219         reset WindDspFb;
079000161219         IndDspF  = WindDspF;
079100161214
079200161214       ENDSR;
079300161215
079400161215       //--------------------------------------------------------------
079500161215       //?Leggo il file di Work.
079600161215       //--------------------------------------------------------------
079700161215       BEGSR LeggiWork;
079800161219
079900161219         EndS01 = *off;
080000161219
080100161219       //?Dichiarazione cursore
080200161219         exec sql
080300161219         DECLARE WRK cursor for
080400161219         SELECT * from WFADS01L
080500161219         ORDER BY WFAfgs, WFApdr, WFAdat;
080600161219
080700161219         //?Apertura del cursore
080800161219         exec sql
080900161219         open WRK;
081000161219
081100161219         IF  sqlcode < 0;
081200161219           EndS01 = *on;
081300161219           exec sql close WRK;
081400161219           leavesr;
081500161219         ENDIF;
081600161219
081700161219         DOW  not EndS01;
081800161219           exec sql
081900161219           fetch next from WRK into :WFADS00F;
082000161219           IF  sqlcod = 100 or sqlcod < 0;
082100161219             EndS01 = *on;
082200161219             leave;
082300161219           ENDIF;
082400161221
082500161221         //?Verifico se c'è ancora la discordanza
082600161221           wDAT = WFAdat;
082700161221           wPDR = WFApdr;
082800161221           exsr Controlla;
082900161221
083000161221         //?Se non c'è più discordanza cancello il rcd
083100161221           IF  (OkDistinta and not OkAssenza) or
083200161221               (not OkDistinta and OkAssenza);
083300170116             chain (WFAfgs:WFApdr:WFAdat) WFADS01L;
083400161221             IF  %found(WFADS01L);
083500161221               delete WFADS000;
083600161221               iter;
083700161221             ENDIF;
083800161221           ENDIF;
083900161219
084000161219         //?Carico i dati nel subfile
084100161220           exsr CaricaS01;
084200161219
084300161219         ENDDO;
084400161219
084500161219         exec sql
084600161219         close WRK;
084700161215
084800161215       ENDSR;
084900161219
085000161219       //--------------------------------------------------------------
085100161219       //?Carico i dati nel Subfile.
085200161219       //--------------------------------------------------------------
085300161220       BEGSR CaricaS01;
085400161219
085500170116         SflNxtChg = *off;
085600161228         clear LVF3S01;
085700161219         PosCurOPZ = *off;
085800161219
085900161219       //?Imposto i campi da WFADS
086000161219         clear V01dpdr;
086100161228         V01pdr = WFApdr;
086200161219         chain (k_APDtip:WFApdr) FIAPD01L;
086300161219         IF  %found(FIAPD01L);
086400161220           V01dpdr = APDrsc;
086500161219         ENDIF;
086600161219
086700161220         Data_ISO = %date(WFAdat:*iso);
086800161219         Data_EUR = Data_ISO;
086900161219         V01data = %dec(Data_EUR);
087000161219
087100161221         IF  WFAnfv > *zeros;
087200161219           V01dst = 'SI';
087300161219         ELSE;
087400161219           clear V01dst;
087500161219         ENDIF;
087600161219
087700161219         clear V01dcau;
087800161228         V01cau = WFAcau;
087900161219         clear dACA;
088000170126         clear TIBS02DS;
088100161219         T02mod = 'C';
088200161219         T02cod = 'ACA';
088300161219         T02ke1 = V01cau;
088400161219         T02sif = KNSIF;
088500161219         TNTBE_RicercaControllo (kpjba : tibs02ds);
088600161219         IF  T02err = *blanks;
088700161219           dACA = T02uni;
088800161219         ENDIF;
088900161219         V01dcau = §ACAdes;
089000161219
089100161219         V01atn = WFAatn;
089200161219
089300161219         S01nrr += 1;
089400161219         write LVF3S01;
089500161219
089600161219       ENDSR;
089700161219
089800161219       //--------------------------------------------------------------
089900161219       //?Gestione tasto funzionale F05 da Subfile S01.
090000161221       //?F05=Refresh
090100161219       //--------------------------------------------------------------
090200161219       BEGSR F05S01;
090300161221
090400161221       //?Come prima cosa devo pulire il file di work
090500161221         exec sql
090600161221         DELETE from QTEMP/WFADS00F;
090700161219
090800161221       //?Riparto dalla lettura degli autisti
090900161221         exsr LeggiAutisti;
091000161221
091100161221       //?Quindi ricarico il subfile con il file di work aggiornato
091200161221         wInzS01 = *on;
091300161219
091400161219       ENDSR;
091500161228
091600161228       //--------------------------------------------------------------
091700161228       //?Gestione tasto funzionale F06 da Subfile S01.
091800161228       //?F06=Conferma
091900161228       //--------------------------------------------------------------
092000161228       BEGSR F06S01;
092100161228
092200161228         readc LVF3S01;
092300161228
092400161228         DOW  not %eof(FNLVF3D);
092500161228           SflNxtChg = *off;
092600161228           V01rcd    = S01nrr;
092700161228           PosCurOPZ = *off;
092800161228
092900170116         //?Le opzioni 1 - 2 - 3 - 4
093000161228         //?Posso farle solo se non ho già un'assenza
093100170117           IF  V01opz <> 'A' and V01opz <> *blanks and
093200170117               V01cau <> *blanks;
093300161228             ErrMessage  = *on;
093400161228             ErrGenerico = *on;
093500161230             PosCurOPZ = *on;
093600161228             V01opz = 'E';
093700161228           ENDIF;
093800161228
093900170116         //?Opzione A
094000161228         //?Posso farla solo se ho un'assenza
094100161228           IF  V01opz = 'A' and V01cau = *blanks;
094200161228             ErrMessage  = *on;
094300161228             ErrGenerico = *on;
094400161230             PosCurOPZ = *on;
094500161228             V01opz = 'E';
094600161228           ENDIF;
094700161228
094800161228         //?Se non ho errore eseguo l'opzione immessa
094900161228           IF  not ErrMessage and V01opz <> *blanks;
095000161228             clear FNLVF1DS;
095100161228             fnlvf1ds.ILVF1pdr = V01pdr;
095200161228             Data_EUR = %date(V01data:*eur);
095300161228             Data_ISO = Data_EUR;
095400161228             fnlvf1ds.ILVF1dat = %dec(Data_ISO);
095500170116             fnlvf1ds.ILVF1ric = V01opz;
095600161228             IF  V01opz = 'A';
095700161228               fnlvf1ds.ILVF1atn = V01atn;
095800170116             ENDIF;
095900161228             fnlvf1r (kpjba:FNLVF1DS);
096000161228             IF  fnlvf1ds.OLVF1err <> *blanks;
096100161228               ErrMessage  = *on;
096200161228               ErrGenerico = *on;
096300170206               V01Msg = fnlvf1ds.OLVF1msg;
096400161228               V01opz = 'E';
096500161228             ENDIF;
096600161230             PosCurOPZ = *off;
096700161230             clear V01opz;
096800161228           ENDIF;
096900161228
097000161228           //?Aggiornamento sfl
097100161228           V01csr = V01rcd;
097200161228           update LVF3S01;
097300161228           readc LVF3S01;
097400161228         ENDDO;
097500161228
097600161228       //?Se non ci sono stati errori ricarico tutto
097700161228         IF  not ErrMessage;
097800161228           exsr F05S01;
097900161228         ENDIF;
098000161228
098100161228       ENDSR;
098200170116
098300170116       //--------------------------------------------------------------
098400170116       //?Gestione tasto funzionale F12 da Subfile S01
098500170116       //?F12=Ritorno
098600170116       //--------------------------------------------------------------
098700170116       BEGSR F12S01;
098800170116
098900170116       //?Chiusura del programma
099000170116         Fine = *on;
099100170116
099200170116       ENDSR;
099300161228
099400161228       //--------------------------------------------------------------
099500161228       //?Gestione tasto funzionale F13 da Subfile S01.
099600161228       //?F13=Ripeti Opzione
099700161228       //--------------------------------------------------------------
099800161228       BEGSR F13S01;
099900170116
100000170116       //?Reperimento pagina del sfl (e Nrr del suo primo record)?
100100170116       //?dalla posizione del cursore?
100200170116         wPag = V01csr / wSflPag;
100300170116         wRig = %rem(V01csr : wSflPag);
100400170116         IF  wRig > *zero;
100500170116           wRig = (wPag * wSflPag) + 1;
100600170116         ELSE;                           // -?ultimo rec. della pag.?
100700170116           wRig = (wPag * wSflPag) - wSflPag + 1;
100800170116         ENDIF;
100900170116         savV01csr = wRig;
101000170116
101100170116       //?Controllo le opzioni inserite
101200170117         PosCurOPZ = *off;
101300170116
101400170116       //?Reperimento della 1ª opzione inserita?
101500170116       //?nella pagina dov'è il cursore?
101600170116         readc LVF3S01;
101700170116         DOW  not %eof(FNLVF3D) and V01opz <> *blanks and
101800170116              S01nrr < savV01csr;
101900170116           SflNxtChg = *on;
102000170116           update LVF3S01;
102100170116           readc LVF3S01;
102200170116         ENDDO;
102300170116         IF  not %eof(FNLVF3D) and V01opz <> *blanks;
102400170116           SflNxtChg = *on;
102500170116           update LVF3S01;
102600170116         ENDIF;
102700170116
102800170116       //?Nessuna opzione nella pagina
102900170116         IF  %eof(FNLVF3D) or
103000170116             S01nrr < savV01csr or
103100170116             S01nrr > (savV01csr + wSflPag - 1);
103200170116           V01rcd = savV01csr;
103300170116           V01csr = savV01csr;
103400170116           clear ForzaOpz;
103500170116           clear savV01opz;
103600170116           clear savV01op1;
103700170116           clear savV01cs1;
103800170116           clear savV01cs2;
103900170116           ErrGenerico = *on;
104000170116           ErrMessage  = *on;
104100170116           PosCurOPZ   = *on;
104200170116           V01msg = Msg(03);
104300170116           leavesr;
104400170116         ENDIF;
104500170116
104600170116       //?Memorizzazione 1ª opzione nella pagina?
104700170116         savV01csr = S01nrr;
104800170116         savV01opz = V01opz;
104900170116         IF  S01nrr <> savV01cs1 or
105000170116             V01opz <> savV01op1;
105100170116           clear ForzaOpz;
105200170116         ENDIF;
105300170116         savV01cs1 = S01nrr;
105400170116         savV01op1 = V01opz;
105500170116
105600170116       //?Reperimento delle altre eventuali opzioni?
105700170116         readc LVF3S01;
105800170116         DOW  not %eof(FNLVF3D);
105900170116
106000170116         //?Aggiornamento subfile?
106100170116           IF  V01opz <> *blanks;
106200170116             SflNxtChg = *on;
106300170116             update LVF3S01;
106400170116           ENDIF;
106500170116
106600170116         //?Reperita una 2ª opzione?
106700170116           SELECT;
106800170116         //?Vuota!?
106900170116           WHEN  V01opz = *blanks;
107000170116         //?1ª opzione nella pagina del cursore  e?
107100170116         //?2ª opzione successiva diversa:?
107200170116         //?messaggio di avviso di "ricopertura"?
107300170116           WHEN  (savV01csr >= wRig       and
107400170116                  S01nrr    >  savV01csr) and
107500170116                  V01opz    <> savV01opz  and
107600170116                  not ForzaOpz;
107700170116             ForzaOpz = *on;
107800170116             savV01cs2 = S01nrr;
107900170116             V01csr    = savV01csr;
108000170116             V01rcd    = savV01csr;
108100170116             ErrGenerico = *on;
108200170116             ErrMessage  = *on;
108300170116             PosCurOPZ   = *on;
108400170116             V01msg = Msg(04);
108500170116             leavesr;
108600170116
108700170116         //?2ª opzione sul record dove è posizionato il cursore?
108800170116         //?(la nuova da considerare per la duplicazione)?
108900170116           WHEN  V01csr = S01nrr;
109000170116             savV01csr = S01nrr;
109100170116             savV01opz = V01opz;
109200170116
109300170116         //?Più opzioni nella stessa videata e?
109400170116         //?cursore su nessuna delle opzioni?
109500170116         //?(riposizionare il cursore)?
109600170116         //?Se stessa opzione in videate diverse: vince la?
109700170116         //?posizione della prima opzione.?
109800170116           WHEN  V01csr <> savV01csr and
109900170116                 V01csr <> S01nrr    and
110000170116                 S01nrr <  wRig + wSflPag;
110100170116             savV01cs2 = S01nrr;
110200170116             V01rcd    = savV01csr;
110300170116             V01csr    = savV01csr;
110400170116             ErrGenerico = *on;
110500170116             ErrMessage  = *on;
110600170116             PosCurOPZ   = *on;
110700170116             V01msg = Msg(05);
110800170116             leavesr;
110900170116
111000170116           ENDSL;
111100170116
111200170116           readc LVF3S01;
111300170116
111400170116         ENDDO;
111500170116
111600170116       //?Opzione NON abilitata?
111700170116         IF  savV01opz <> '1' and savV01opz <> '2' and
111800170116             savV01opz <> '3' and savV01opz <> '4' and
111900170116             savV01opz <> 'A';
112000170116           V01csr = savV01csr;
112100170116           V01rcd = savV01csr;
112200170116           ErrGenerico = *on;
112300170116           ErrMessage  = *on;
112400170116           PosCurOPZ   = *on;
112500170116           V01msg = Msg(02);
112600170116           leavesr;
112700170116         ENDIF;
112800170117
112900170117         PosCurOPZ = *off;
113000170116
113100170116       //?Ripetizione opzione sul resto del subfile?
113200170116         S01nrr = savV01csr + 1;
113300170116         chain  S01nrr  LVF3S01;
113400170116         DOW  %found(FNLVF3D);
113500170116           SflNxtChg = *on;
113600170116           V01opz = savV01opz;
113700170116           update LVF3S01;
113800170116           S01nrr += 1;
113900170116           chain S01nrr LVF3S01;
114000170116         ENDDO;
114100161228
114200161228       ENDSR;
114300161219
114400161219       //--------------------------------------------------------------
114500161219       //?Gestione opzioni Subfile S01.
114600161219       //--------------------------------------------------------------
114700161219       BEGSR OpzS01;
114800170116
114900170116         clear savV01csr;
115000161219
115100161219         readc LVF3S01;
115200161219
115300161220         DOW  not %eof(FNLVF3D);
115400161219
115500161219           SflNxtChg = *off;
115600161219           WindDspF  = IndDspF;
115700161219           reset WindDspFb;
115800161219           IndDspF   = WindDspF;
115900161219           V01rcd    = S01nrr;
116000161219
116100161219           PosCurOPZ = *off;
116200161219
116300161219           SELECT;
116400161219         //?- Nessuna opzione
116500161219           WHEN  V01opz = *blank;
116600161219
116700170116         //?- Immissione Assenza (1-2-3-4)
116800170116           WHEN  V01opz = '1' or V01opz = '2' or
116900170116                 V01opz = '3' or V01opz = '4';
117000161219           //?Posso farla solo se non ho già un'assenza
117100170116             IF  V01cau <> *blanks;
117200170116               ErrMessage  = *on;
117300170116               ErrGenerico = *on;
117400170116               PosCurOPZ   = *on;
117500170116               V01msg = Msg(02);
117600170116               V01msg = %trim(V01msg) + ' Assenza già presente';
117700170116             ENDIF;
117800161219
117900161219         //?- A = Annulla
118000161219           WHEN  V01opz = 'A';
118100161219           //?Posso farla solo se ho un'assenza
118200170116             IF  V01cau = *blanks;
118300170116               ErrMessage  = *on;
118400170116               ErrGenerico = *on;
118500170116               PosCurOPZ   = *on;
118600170116               V01msg = Msg(02);
118700170116               V01msg = %trim(V01msg) + ' Assenza non presente';
118800170116             ENDIF;
118900161219
119000161219           OTHER;
119100161219             ErrMessage  = *on;
119200161219             ErrGenerico = *on;
119300161219             PosCurOPZ   = *on;
119400170109             V01msg = Msg(02);
119500161219           ENDSL;
119600161219
119700161219           //?Aggiornamento sfl
119800170116           SflNxtChg = *on;
119900170116           V01csr    = V01rcd;
120000161219           update LVF3S01;
120100161219
120200161219           IF  ErrMessage or ErrGenerico;
120300161219             leave;
120400161219           ENDIF;
120500161219
120600161219           readc LVF3S01;
120700161219
120800161219         ENDDO;
120900161219
121000161219       ENDSR;
121100161219
121200161219       //--------------------------------------------------------------
121300170116       //?Opzione "I" immissione assenza Subfile S01.
121400161219       //--------------------------------------------------------------
121500170116       BEGSR OpzioneI;
121600161220
121700161221       //?Se OK assenza errore
121800161220         IF  V01cau <> *blanks;
121900161220           ErrMessage  = *on;
122000161220           ErrGenerico = *on;
122100161220           PosCurOPZ   = *on;
122200161220           V01msg = Msg(02);
122300161220           V01msg = %trim(V01msg) + ' Assenza già presente';
122400161220           leavesr;
122500161220         ENDIF;
122600161219
122700161219       //?Devo inserire l'assenza
122800161220         clear FNLVF1DS;
122900170116         fnlvf1ds.ILVF1ric = V01opz;
123000161220         fnlvf1ds.ILVF1pdr = V01pdr;
123100161220         Data_EUR = %date(V01data:*eur);
123200161220         Data_ISO = Data_EUR;
123300161220         fnlvf1ds.ILVF1dat = %dec(Data_ISO);
123400161220         fnlvf1r (kpjba:FNLVF1DS);
123500161220         IF  fnlvf1ds.OLVF1err <> *blanks;
123600161219           ErrMessage  = *on;
123700161219           ErrGenerico = *on;
123800161219           PosCurOPZ   = *on;
123900161220           V01msg = fnlvf1ds.OLVF1msg;
124000161219           leavesr;
124100161219         ENDIF;
124200161219
124300161219       ENDSR;
124400161220
124500161220       //--------------------------------------------------------------
124600161220       //?Opzione "A" Subfile S01.
124700161220       //--------------------------------------------------------------
124800161220       BEGSR OpzioneA;
124900161220
125000161221       //?Se no OK assenza errore
125100161220         IF  V01cau = *blanks;
125200161220           ErrMessage  = *on;
125300161220           ErrGenerico = *on;
125400161220           PosCurOPZ   = *on;
125500161220           V01msg = Msg(02);
125600161220           V01msg = %trim(V01msg) + ' Assenza non presente';
125700161220           leavesr;
125800161220         ENDIF;
125900161220
126000161220       //?Devo annullare l'assenza
126100161220         clear FNLVF1DS;
126200170116         fnlvf1ds.ILVF1ric = V01opz;
126300161220         fnlvf1ds.ILVF1atn = V01atn;
126400161220         fnlvf1r (kpjba:FNLVF1DS);
126500161220         IF  fnlvf1ds.OLVF1err <> *blanks;
126600161220           ErrMessage  = *on;
126700161220           ErrGenerico = *on;
126800161220           PosCurOPZ   = *on;
126900161220           V01msg = fnlvf1ds.OLVF1msg;
127000161220           leavesr;
127100161220         ENDIF;
127200161220
127300161220       ENDSR;
127400161214
127500161214      /end-free
127600161214       //--------------------------------------------------------------
127700161214       //?Schiere a tempo di compilazione.
127800161214       //--------------------------------------------------------------
127900161214
128000170116** -  MSG --------------------------------------------------------------------*
128100161214Non è stata impostata la filiale gestione                                     01
128200161219Opzione errata                                                                02
128300170116Immettere un'opzione su questa pagina prima di premere F13                    03
128400170116Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa04
128500170116Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F1305
