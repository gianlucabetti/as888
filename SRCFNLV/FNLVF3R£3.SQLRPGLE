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
043900170302         //?Escludo se massa complessiva >= 26000 Kg
044000170302           IF  §APDmcp >= 26000;
044100170302             iter;
044200170302           ENDIF;
044300161220         //?Escludo i non accreditati
044400161220           clear FNLVF9DS;
044500161220           fnlvf9ds.ILVF9pdr = APDpdr;
044600161220           fnlvf9ds.ILVF9data = Oggi;
044700161220           fnlvf9r (fnlvf9ds);
044800161220           IF  fnlvf9ds.OLVF9ok = *off;
044900161220             iter;
045000161220           ENDIF;
045100161220
045200161221         //?Il controllo va fatto su tutti i gg previsti in tabella
045300161221           xx = 1;
045400161221           FOR xx by 1 to §5Aggass;
045500161221
045600161221             wDAT = skData(xx);
045700161221             wPDR = APDpdr;
045800161221
045900170126           //?se la data da controllare è < data attivazione
046000170126           //?provo con altro giorno
046100170126             IF  wDAT < wDataAtt;
046200170126               iter;
046300170126             ENDIF;
046400170126
046500161221           //?Controllo se ha distinta/Assenza
046600170126             exsr Controlla;
046700161221
046800161221             SELECT;
046900161221           //?OK se ha la distinta e non l'assenza
047000161221             WHEN  OkDistinta and not OkAssenza;
047100161221               iter;
047200161221           //?OK se non ha la distinta ed ha l'assenza
047300161221             WHEN  not OkDistinta and OkAssenza;
047400161221               iter;
047500161221             ENDSL;
047600161221
047700161221           //?Se richiamato per solo controllo
047800161221           //?e vedo già che c'è una discordanza esco dal pgm
047900161221             IF  fnlvf3ds.ILVF3trc = 'C';
048000170130               fnlvf3ds.OLVF3disco = 'S';
048100161221               exsr RoutEnd;
048200161221             ENDIF;
048300161220
048400161221           //?Scrivo il file di work
048500161221             exsr ScriviWrkf;
048600161221           ENDFOR;
048700161220
048800161215         ENDDO;
048900161215
049000161219         feod WFADS01L;
049100161215
049200161215       ENDSR;
049300161215
049400161215       //--------------------------------------------------------------
049500161219       //?Gestione videata S01.
049600161215       //--------------------------------------------------------------
049700161215       BEGSR GesS01;
049800161215
049900161215       //?Inizializzazione videata
050000161215         IF  wInzS01;
050100161215           exsr InzS01;
050200161215           wInzS01 = *off;
050300161215         ENDIF;
050400161215
050500161215       //?Visualizzazione del SFL (se ci sono dati)
050600161215         SflDsp = (S01nrr <= *zeros);
050700161215
050800161215       //?Posizionamento cursore
050900161219         V01rcd = V01csr;
051000161215
051100161215       //?Emissione Testata e Piede con tasti funzionali abilitati
051200161215         write LVF3T01;
051300161215         write LVF3P01;
051400161215
051500161215       //?Emissione videata
051600161215         exfmt LVF3C01;
051700161215         ErrMessage  = *off;
051800161215         ErrGenerico = *off;
051900161215         clear V01msg;
052000170116
052100170116         IF  dsp_aid <> c_F12 and
052200170116             dsp_aid <> c_F13;
052300170116           clear savV01csr;
052400170116           clear savV01cs1;
052500170116           clear savV01cs2;
052600170116           clear savV01opz;
052700170116           clear savV01op1;
052800170116         ENDIF;
052900161215
053000161215         SELECT;
053100161219
053200161228       //?F05=Refresh
053300161219         WHEN  dsp_aid = c_F05;
053400161219           exsr F05S01;
053500161228
053600161228       //?F06=Conferma
053700161228         WHEN  dsp_aid = c_F06;
053800170118           exsr OpzS01;
053900170118           IF  ErrGenerico;
054000170118             leavesr;
054100170118           ENDIF;
054200170117           exsr F06S01;
054300170116
054400170116       //?F12=Ritorno
054500170116         WHEN  dsp_aid = c_F12;
054600170116           exsr F12S01;
054700161228
054800161228       //?F13=Ripeti Opzione
054900161228         WHEN  dsp_aid = c_F13;
055000161228           exsr F13S01;
055100161215
055200161215       //?Enter
055300161215         OTHER;
055400161219         //?Controllo dati
055500161219           IF  V01csr > *zero;
055600161219             exsr OpzS01;
055700161219             IF  ErrGenerico;
055800161219               leavesr;
055900161219             ENDIF;
056000161219           ENDIF;
056100170111           IF  V01csr = *zero;
056200170111             V01csr = savS01nrr;
056300170111           ENDIF;
056400161215
056500161215         ENDSL;
056600161215
056700161215       ENDSR;
056800161215
056900161215       //--------------------------------------------------------------
057000161215       //?Operazioni finali.
057100161215       //--------------------------------------------------------------
057200161215       BEGSR RoutEnd;
057300161215
057400161215       //?Cancello file di work dalla qtemp
057500161220         IF  %open(WFADS01L);
057600161220           close WFADS01L;
057700161220           cmd = 'DLTF FILE(QTEMP/WFADS01L)';
057800161220           ExecuteCommand (cmd);
057900161220           cmd = 'DLTF FILE(QTEMP/WFADS00F)';
058000161220           ExecuteCommand (cmd);
058100161220         ENDIF;
058200161215
058300161215         *inLR = *on;
058400161215         return;
058500161215
058600161215       ENDSR;
058700161219
058800161219       //--------------------------------------------------------------
058900161219       //?Reperimento Dati del job (Utente/Operativi).
059000161219       //--------------------------------------------------------------
059100161219       BEGSR DatiJob;
059200161219
059300161219         in(E) §AzUte;
059400161219         IF  NOT %error;
059500161219           in(E) §DatiUte;
059600161219         ENDIF;
059700161219         IF  %error or RSut = *blanks;
059800170126           clear TIBS34DS;
059900161219           tibs34r(tibs34ds);
060000161219           in §AzUte;
060100161219           in §DatiUte;
060200161219         ENDIF;
060300161219
060400161219       ENDSR;
060500161215
060600161215       //--------------------------------------------------------------
060700161215       //?Calcolo la data da elaborare.
060800161215       //--------------------------------------------------------------
060900161215       BEGSR CalcolaData;
061000161215
061100161215         xx = 1;
061200161215         FOR xx by 1 to §5Aggass;
061300161215           clear XGIOLAVDS;
061400161220           IXGLdata = wDatada;
061500161220           IXGLsub  = 'S';
061600161220           IXGLgg   = xx;
061700161215           IXGLlav  = 'S';
061800161215           IXGLpa   = 'P';
061900161221           IXGLfil  = wFGS;
062000161215           XGIOLAV (xgiolavds);
062100161215           IF  OXGLerr = *blanks;
062200161215             skData(xx) = OXGLdata;
062300161215           ELSE;
062400161219             Data_ISO = %date(wDatada:*iso);
062500161220             Data_ISO = Data_ISO - %days(xx);
062600161219             skData(xx) = %dec(Data_ISO);
062700161215           ENDIF;
062800161215         ENDFOR;
062900161215
063000161215       ENDSR;
063100161221
063200161221       //--------------------------------------------------------------
063300161221       //?Controllo se ci sono discordanze
063400161221       //--------------------------------------------------------------
063500161221       BEGSR Controlla;
063600161221
063700161221         OkAssenza = *off;
063800161221         OkDistinta = *off;
063900161221         clear wNFV;
064000161221         clear wATN;
064100161221         clear wCAU;
064200161221
064300161221       //?Leggo le Distinte del giorno
064400161221       //?e aggiorno file di work
064500161221         exsr LeggiDistinte;
064600161221
064700161221       //?Leggo le Assenze del giorno
064800161221       //?e aggiorno file di work
064900161221         exsr LeggiAssenze;
065000161221
065100161221       ENDSR;
065200161215
065300161215       //--------------------------------------------------------------
065400161215       //?Scrivo il file di Work.
065500161215       //--------------------------------------------------------------
065600161215       BEGSR ScriviWrkf;
065700161215
065800161221       //?Scrivo
065900161221         clear WFADS000;
066000161221         WFAfgs = wFGS;
066100161221         WFApdr = APDpdr;
066200161221         WFAdat = skData(xx);
066300161221         WFAnfv = wNFV;
066400161221         WFAcau = wCAU;
066500161221         WFAatn = wATN;
066600161221         write WFADS000;
066700161215
066800161215       ENDSR;
066900161214
067000161214       //--------------------------------------------------------------
067100161214       //?Leggo le distinte del giorno.
067200161214       //--------------------------------------------------------------
067300161214       BEGSR LeggiDistinte;
067400161221
067500161221         EndSQL = *off;
067600161215
067700161215         exec sql
067800161215         DECLARE DST cursor for
067900161214         SELECT DSTnfv from FIDST00F
068000161221         WHERE DSTfgs = :wFGS and DSTnpg = 4 and
068100161221               DSTdfv = :wDAT and DSTatb = ' ' and
068200161221               DSTpdr = :wPDR;
068300161214
068400161214       //?Apertura del cursore
068500161214         exec sql open DST;
068600161214         IF  sqlcode < 0;
068700161214           EndSQL = *on;
068800161214         ENDIF;
068900161214
069000161214         DOW  not EndSQL;
069100161214           exec sql
069200161214           fetch next from DST into :DSTnfv;
069300161214           IF  sqlcod = 100 or sqlcod < 0;
069400161214             EndSQL = *on;
069500161214             leave;
069600161214           ENDIF;
069700161221
069800161221           wNFV = DSTnfv;
069900161215           OkDistinta = *on;
070000161215           leave;
070100161214
070200161214         ENDDO;
070300161214
070400161215         exec sql close DST;
070500161214
070600161214       ENDSR;
070700161214
070800161214       //--------------------------------------------------------------
070900161214       //?Leggo le assenze del giorno.
071000161214       //--------------------------------------------------------------
071100161214       BEGSR LeggiAssenze;
071200161215
071300161215         EndSQL = *off;
071400161215
071500161215         exec sql
071600161215         DECLARE AGA cursor for
071700161219         SELECT AGAcau, AGAatn from FIAGA00F
071800161221         WHERE AGApdr = :wPDR and AGAdat = :wDAT and
071900161215               AGAatb = ' ';
072000161215
072100161215       //?Apertura del cursore
072200161215         exec sql open AGA;
072300161215         IF  sqlcode < 0;
072400161215           EndSQL = *on;
072500161215         ENDIF;
072600161215
072700161215         DOW  not EndSQL;
072800161215           exec sql
072900161219           fetch next from AGA into :AGAcau, :AGAatn;
073000161215           IF  sqlcod = 100 or sqlcod < 0;
073100161215             EndSQL = *on;
073200161215             leave;
073300161215           ENDIF;
073400161221
073500161221           wCAU = AGAcau;
073600161221           wATN = AGAatn;
073700161215           OkAssenza = *on;
073800161215           leave;
073900161215
074000161215         ENDDO;
074100161215
074200161215         exec sql close AGA;
074300161214
074400161214       ENDSR;
074500161215
074600161215       //--------------------------------------------------------------
074700161215       //?Inizializzazione videata S01.
074800161215       //--------------------------------------------------------------
074900161215       BEGSR InzS01;
075000161219
075100161219         EndS01 = *off;
075200161215
075300161215       //?Pulizia subfile
075400161215         exsr PulSfl;
075500161215
075600161215       //?Leggo file di work per caricare il subfile
075700161215         exsr LeggiWork;
075800161219
075900161219         SflEnd = *on;
076000161228         savS01nrr = S01nrr;
076100161219
076200161219       //?Imposto il posizionamento al primo rcd
076300161219         IF  S01nrr > 0;
076400161219           V01rcd = 1;
076500161219         ELSE;
076600161219           clear V01rcd;
076700161219         ENDIF;
076800161219
076900161219         V01csr = V01rcd;
077000161215
077100161215       ENDSR;
077200161214
077300161214       //--------------------------------------------------------------
077400161219       //?Pulisco il Subfile.
077500161214       //--------------------------------------------------------------
077600161214       BEGSR PulSfl;
077700161214
077800161214         SflDsp    = *on;
077900161214         SflDspCtl = *on;
078000161214         write LVF3C01;
078100161214         SflDspCtl = *off;
078200161214         SflEnd    = *off;
078300161214
078400161228         clear savS01Nrr;
078500161214         clear S01Nrr;
078600161214         clear V01rcd;
078700161219         clear V01csr;
078800161214         clear V01msg;
078900161214         ErrMessage  = *off;
079000161214         ErrGenerico = *off;
079100161219
079200161219         WindDspF = IndDspF;
079300161219         reset WindDspFb;
079400161219         IndDspF  = WindDspF;
079500161214
079600161214       ENDSR;
079700161215
079800161215       //--------------------------------------------------------------
079900161215       //?Leggo il file di Work.
080000161215       //--------------------------------------------------------------
080100161215       BEGSR LeggiWork;
080200161219
080300161219         EndS01 = *off;
080400161219
080500161219       //?Dichiarazione cursore
080600161219         exec sql
080700161219         DECLARE WRK cursor for
080800161219         SELECT * from WFADS01L
080900161219         ORDER BY WFAfgs, WFApdr, WFAdat;
081000161219
081100161219         //?Apertura del cursore
081200161219         exec sql
081300161219         open WRK;
081400161219
081500161219         IF  sqlcode < 0;
081600161219           EndS01 = *on;
081700161219           exec sql close WRK;
081800161219           leavesr;
081900161219         ENDIF;
082000161219
082100161219         DOW  not EndS01;
082200161219           exec sql
082300161219           fetch next from WRK into :WFADS00F;
082400161219           IF  sqlcod = 100 or sqlcod < 0;
082500161219             EndS01 = *on;
082600161219             leave;
082700161219           ENDIF;
082800161221
082900161221         //?Verifico se c'è ancora la discordanza
083000161221           wDAT = WFAdat;
083100161221           wPDR = WFApdr;
083200161221           exsr Controlla;
083300161221
083400161221         //?Se non c'è più discordanza cancello il rcd
083500161221           IF  (OkDistinta and not OkAssenza) or
083600161221               (not OkDistinta and OkAssenza);
083700170116             chain (WFAfgs:WFApdr:WFAdat) WFADS01L;
083800161221             IF  %found(WFADS01L);
083900161221               delete WFADS000;
084000161221               iter;
084100161221             ENDIF;
084200161221           ENDIF;
084300161219
084400161219         //?Carico i dati nel subfile
084500161220           exsr CaricaS01;
084600161219
084700161219         ENDDO;
084800161219
084900161219         exec sql
085000161219         close WRK;
085100161215
085200161215       ENDSR;
085300161219
085400161219       //--------------------------------------------------------------
085500161219       //?Carico i dati nel Subfile.
085600161219       //--------------------------------------------------------------
085700161220       BEGSR CaricaS01;
085800161219
085900170116         SflNxtChg = *off;
086000161228         clear LVF3S01;
086100161219         PosCurOPZ = *off;
086200161219
086300161219       //?Imposto i campi da WFADS
086400161219         clear V01dpdr;
086500161228         V01pdr = WFApdr;
086600161219         chain (k_APDtip:WFApdr) FIAPD01L;
086700161219         IF  %found(FIAPD01L);
086800161220           V01dpdr = APDrsc;
086900161219         ENDIF;
087000161219
087100161220         Data_ISO = %date(WFAdat:*iso);
087200161219         Data_EUR = Data_ISO;
087300161219         V01data = %dec(Data_EUR);
087400161219
087500161221         IF  WFAnfv > *zeros;
087600161219           V01dst = 'SI';
087700161219         ELSE;
087800161219           clear V01dst;
087900161219         ENDIF;
088000161219
088100161219         clear V01dcau;
088200161228         V01cau = WFAcau;
088300161219         clear dACA;
088400170126         clear TIBS02DS;
088500161219         T02mod = 'C';
088600161219         T02cod = 'ACA';
088700161219         T02ke1 = V01cau;
088800161219         T02sif = KNSIF;
088900161219         TNTBE_RicercaControllo (kpjba : tibs02ds);
089000161219         IF  T02err = *blanks;
089100161219           dACA = T02uni;
089200161219         ENDIF;
089300161219         V01dcau = §ACAdes;
089400161219
089500161219         V01atn = WFAatn;
089600161219
089700161219         S01nrr += 1;
089800161219         write LVF3S01;
089900161219
090000161219       ENDSR;
090100161219
090200161219       //--------------------------------------------------------------
090300161219       //?Gestione tasto funzionale F05 da Subfile S01.
090400161221       //?F05=Refresh
090500161219       //--------------------------------------------------------------
090600161219       BEGSR F05S01;
090700161221
090800161221       //?Come prima cosa devo pulire il file di work
090900161221         exec sql
091000161221         DELETE from QTEMP/WFADS00F;
091100161219
091200161221       //?Riparto dalla lettura degli autisti
091300161221         exsr LeggiAutisti;
091400161221
091500161221       //?Quindi ricarico il subfile con il file di work aggiornato
091600161221         wInzS01 = *on;
091700161219
091800161219       ENDSR;
091900161228
092000161228       //--------------------------------------------------------------
092100161228       //?Gestione tasto funzionale F06 da Subfile S01.
092200161228       //?F06=Conferma
092300161228       //--------------------------------------------------------------
092400161228       BEGSR F06S01;
092500161228
092600161228         readc LVF3S01;
092700161228
092800161228         DOW  not %eof(FNLVF3D);
092900161228           SflNxtChg = *off;
093000161228           V01rcd    = S01nrr;
093100161228           PosCurOPZ = *off;
093200161228
093300170116         //?Le opzioni 1 - 2 - 3 - 4
093400161228         //?Posso farle solo se non ho già un'assenza
093500170117           IF  V01opz <> 'A' and V01opz <> *blanks and
093600170117               V01cau <> *blanks;
093700161228             ErrMessage  = *on;
093800161228             ErrGenerico = *on;
093900161230             PosCurOPZ = *on;
094000161228             V01opz = 'E';
094100161228           ENDIF;
094200161228
094300170116         //?Opzione A
094400161228         //?Posso farla solo se ho un'assenza
094500161228           IF  V01opz = 'A' and V01cau = *blanks;
094600161228             ErrMessage  = *on;
094700161228             ErrGenerico = *on;
094800161230             PosCurOPZ = *on;
094900161228             V01opz = 'E';
095000161228           ENDIF;
095100161228
095200161228         //?Se non ho errore eseguo l'opzione immessa
095300161228           IF  not ErrMessage and V01opz <> *blanks;
095400161228             clear FNLVF1DS;
095500161228             fnlvf1ds.ILVF1pdr = V01pdr;
095600161228             Data_EUR = %date(V01data:*eur);
095700161228             Data_ISO = Data_EUR;
095800161228             fnlvf1ds.ILVF1dat = %dec(Data_ISO);
095900170116             fnlvf1ds.ILVF1ric = V01opz;
096000161228             IF  V01opz = 'A';
096100161228               fnlvf1ds.ILVF1atn = V01atn;
096200170116             ENDIF;
096300161228             fnlvf1r (kpjba:FNLVF1DS);
096400161228             IF  fnlvf1ds.OLVF1err <> *blanks;
096500161228               ErrMessage  = *on;
096600161228               ErrGenerico = *on;
096700170206               V01Msg = fnlvf1ds.OLVF1msg;
096800161228               V01opz = 'E';
096900161228             ENDIF;
097000161230             PosCurOPZ = *off;
097100161230             clear V01opz;
097200161228           ENDIF;
097300161228
097400161228           //?Aggiornamento sfl
097500161228           V01csr = V01rcd;
097600161228           update LVF3S01;
097700161228           readc LVF3S01;
097800161228         ENDDO;
097900161228
098000161228       //?Se non ci sono stati errori ricarico tutto
098100161228         IF  not ErrMessage;
098200161228           exsr F05S01;
098300161228         ENDIF;
098400161228
098500161228       ENDSR;
098600170116
098700170116       //--------------------------------------------------------------
098800170116       //?Gestione tasto funzionale F12 da Subfile S01
098900170116       //?F12=Ritorno
099000170116       //--------------------------------------------------------------
099100170116       BEGSR F12S01;
099200170116
099300170116       //?Chiusura del programma
099400170116         Fine = *on;
099500170116
099600170116       ENDSR;
099700161228
099800161228       //--------------------------------------------------------------
099900161228       //?Gestione tasto funzionale F13 da Subfile S01.
100000161228       //?F13=Ripeti Opzione
100100161228       //--------------------------------------------------------------
100200161228       BEGSR F13S01;
100300170116
100400170116       //?Reperimento pagina del sfl (e Nrr del suo primo record)?
100500170116       //?dalla posizione del cursore?
100600170116         wPag = V01csr / wSflPag;
100700170116         wRig = %rem(V01csr : wSflPag);
100800170116         IF  wRig > *zero;
100900170116           wRig = (wPag * wSflPag) + 1;
101000170116         ELSE;                           // -?ultimo rec. della pag.?
101100170116           wRig = (wPag * wSflPag) - wSflPag + 1;
101200170116         ENDIF;
101300170116         savV01csr = wRig;
101400170116
101500170116       //?Controllo le opzioni inserite
101600170117         PosCurOPZ = *off;
101700170116
101800170116       //?Reperimento della 1ª opzione inserita?
101900170116       //?nella pagina dov'è il cursore?
102000170116         readc LVF3S01;
102100170116         DOW  not %eof(FNLVF3D) and V01opz <> *blanks and
102200170116              S01nrr < savV01csr;
102300170116           SflNxtChg = *on;
102400170116           update LVF3S01;
102500170116           readc LVF3S01;
102600170116         ENDDO;
102700170116         IF  not %eof(FNLVF3D) and V01opz <> *blanks;
102800170116           SflNxtChg = *on;
102900170116           update LVF3S01;
103000170116         ENDIF;
103100170116
103200170116       //?Nessuna opzione nella pagina
103300170116         IF  %eof(FNLVF3D) or
103400170116             S01nrr < savV01csr or
103500170116             S01nrr > (savV01csr + wSflPag - 1);
103600170116           V01rcd = savV01csr;
103700170116           V01csr = savV01csr;
103800170116           clear ForzaOpz;
103900170116           clear savV01opz;
104000170116           clear savV01op1;
104100170116           clear savV01cs1;
104200170116           clear savV01cs2;
104300170116           ErrGenerico = *on;
104400170116           ErrMessage  = *on;
104500170116           PosCurOPZ   = *on;
104600170116           V01msg = Msg(03);
104700170116           leavesr;
104800170116         ENDIF;
104900170116
105000170116       //?Memorizzazione 1ª opzione nella pagina?
105100170116         savV01csr = S01nrr;
105200170116         savV01opz = V01opz;
105300170116         IF  S01nrr <> savV01cs1 or
105400170116             V01opz <> savV01op1;
105500170116           clear ForzaOpz;
105600170116         ENDIF;
105700170116         savV01cs1 = S01nrr;
105800170116         savV01op1 = V01opz;
105900170116
106000170116       //?Reperimento delle altre eventuali opzioni?
106100170116         readc LVF3S01;
106200170116         DOW  not %eof(FNLVF3D);
106300170116
106400170116         //?Aggiornamento subfile?
106500170116           IF  V01opz <> *blanks;
106600170116             SflNxtChg = *on;
106700170116             update LVF3S01;
106800170116           ENDIF;
106900170116
107000170116         //?Reperita una 2ª opzione?
107100170116           SELECT;
107200170116         //?Vuota!?
107300170116           WHEN  V01opz = *blanks;
107400170116         //?1ª opzione nella pagina del cursore  e?
107500170116         //?2ª opzione successiva diversa:?
107600170116         //?messaggio di avviso di "ricopertura"?
107700170116           WHEN  (savV01csr >= wRig       and
107800170116                  S01nrr    >  savV01csr) and
107900170116                  V01opz    <> savV01opz  and
108000170116                  not ForzaOpz;
108100170116             ForzaOpz = *on;
108200170116             savV01cs2 = S01nrr;
108300170116             V01csr    = savV01csr;
108400170116             V01rcd    = savV01csr;
108500170116             ErrGenerico = *on;
108600170116             ErrMessage  = *on;
108700170116             PosCurOPZ   = *on;
108800170116             V01msg = Msg(04);
108900170116             leavesr;
109000170116
109100170116         //?2ª opzione sul record dove è posizionato il cursore?
109200170116         //?(la nuova da considerare per la duplicazione)?
109300170116           WHEN  V01csr = S01nrr;
109400170116             savV01csr = S01nrr;
109500170116             savV01opz = V01opz;
109600170116
109700170116         //?Più opzioni nella stessa videata e?
109800170116         //?cursore su nessuna delle opzioni?
109900170116         //?(riposizionare il cursore)?
110000170116         //?Se stessa opzione in videate diverse: vince la?
110100170116         //?posizione della prima opzione.?
110200170116           WHEN  V01csr <> savV01csr and
110300170116                 V01csr <> S01nrr    and
110400170116                 S01nrr <  wRig + wSflPag;
110500170116             savV01cs2 = S01nrr;
110600170116             V01rcd    = savV01csr;
110700170116             V01csr    = savV01csr;
110800170116             ErrGenerico = *on;
110900170116             ErrMessage  = *on;
111000170116             PosCurOPZ   = *on;
111100170116             V01msg = Msg(05);
111200170116             leavesr;
111300170116
111400170116           ENDSL;
111500170116
111600170116           readc LVF3S01;
111700170116
111800170116         ENDDO;
111900170116
112000170116       //?Opzione NON abilitata?
112100170116         IF  savV01opz <> '1' and savV01opz <> '2' and
112200170116             savV01opz <> '3' and savV01opz <> '4' and
112300170116             savV01opz <> 'A';
112400170116           V01csr = savV01csr;
112500170116           V01rcd = savV01csr;
112600170116           ErrGenerico = *on;
112700170116           ErrMessage  = *on;
112800170116           PosCurOPZ   = *on;
112900170116           V01msg = Msg(02);
113000170116           leavesr;
113100170116         ENDIF;
113200170117
113300170117         PosCurOPZ = *off;
113400170116
113500170116       //?Ripetizione opzione sul resto del subfile?
113600170116         S01nrr = savV01csr + 1;
113700170116         chain  S01nrr  LVF3S01;
113800170116         DOW  %found(FNLVF3D);
113900170116           SflNxtChg = *on;
114000170116           V01opz = savV01opz;
114100170116           update LVF3S01;
114200170116           S01nrr += 1;
114300170116           chain S01nrr LVF3S01;
114400170116         ENDDO;
114500161228
114600161228       ENDSR;
114700161219
114800161219       //--------------------------------------------------------------
114900161219       //?Gestione opzioni Subfile S01.
115000161219       //--------------------------------------------------------------
115100161219       BEGSR OpzS01;
115200170116
115300170116         clear savV01csr;
115400161219
115500161219         readc LVF3S01;
115600161219
115700161220         DOW  not %eof(FNLVF3D);
115800161219
115900161219           SflNxtChg = *off;
116000161219           WindDspF  = IndDspF;
116100161219           reset WindDspFb;
116200161219           IndDspF   = WindDspF;
116300161219           V01rcd    = S01nrr;
116400161219
116500161219           PosCurOPZ = *off;
116600161219
116700161219           SELECT;
116800161219         //?- Nessuna opzione
116900161219           WHEN  V01opz = *blank;
117000161219
117100170116         //?- Immissione Assenza (1-2-3-4)
117200170116           WHEN  V01opz = '1' or V01opz = '2' or
117300170116                 V01opz = '3' or V01opz = '4';
117400161219           //?Posso farla solo se non ho già un'assenza
117500170116             IF  V01cau <> *blanks;
117600170116               ErrMessage  = *on;
117700170116               ErrGenerico = *on;
117800170116               PosCurOPZ   = *on;
117900170116               V01msg = Msg(02);
118000170116               V01msg = %trim(V01msg) + ' Assenza già presente';
118100170116             ENDIF;
118200161219
118300161219         //?- A = Annulla
118400161219           WHEN  V01opz = 'A';
118500161219           //?Posso farla solo se ho un'assenza
118600170116             IF  V01cau = *blanks;
118700170116               ErrMessage  = *on;
118800170116               ErrGenerico = *on;
118900170116               PosCurOPZ   = *on;
119000170116               V01msg = Msg(02);
119100170116               V01msg = %trim(V01msg) + ' Assenza non presente';
119200170116             ENDIF;
119300161219
119400161219           OTHER;
119500161219             ErrMessage  = *on;
119600161219             ErrGenerico = *on;
119700161219             PosCurOPZ   = *on;
119800170109             V01msg = Msg(02);
119900161219           ENDSL;
120000161219
120100161219           //?Aggiornamento sfl
120200170116           SflNxtChg = *on;
120300170116           V01csr    = V01rcd;
120400161219           update LVF3S01;
120500161219
120600161219           IF  ErrMessage or ErrGenerico;
120700161219             leave;
120800161219           ENDIF;
120900161219
121000161219           readc LVF3S01;
121100161219
121200161219         ENDDO;
121300161219
121400161219       ENDSR;
121500161219
121600161219       //--------------------------------------------------------------
121700170116       //?Opzione "I" immissione assenza Subfile S01.
121800161219       //--------------------------------------------------------------
121900170116       BEGSR OpzioneI;
122000161220
122100161221       //?Se OK assenza errore
122200161220         IF  V01cau <> *blanks;
122300161220           ErrMessage  = *on;
122400161220           ErrGenerico = *on;
122500161220           PosCurOPZ   = *on;
122600161220           V01msg = Msg(02);
122700161220           V01msg = %trim(V01msg) + ' Assenza già presente';
122800161220           leavesr;
122900161220         ENDIF;
123000161219
123100161219       //?Devo inserire l'assenza
123200161220         clear FNLVF1DS;
123300170116         fnlvf1ds.ILVF1ric = V01opz;
123400161220         fnlvf1ds.ILVF1pdr = V01pdr;
123500161220         Data_EUR = %date(V01data:*eur);
123600161220         Data_ISO = Data_EUR;
123700161220         fnlvf1ds.ILVF1dat = %dec(Data_ISO);
123800161220         fnlvf1r (kpjba:FNLVF1DS);
123900161220         IF  fnlvf1ds.OLVF1err <> *blanks;
124000161219           ErrMessage  = *on;
124100161219           ErrGenerico = *on;
124200161219           PosCurOPZ   = *on;
124300161220           V01msg = fnlvf1ds.OLVF1msg;
124400161219           leavesr;
124500161219         ENDIF;
124600161219
124700161219       ENDSR;
124800161220
124900161220       //--------------------------------------------------------------
125000161220       //?Opzione "A" Subfile S01.
125100161220       //--------------------------------------------------------------
125200161220       BEGSR OpzioneA;
125300161220
125400161221       //?Se no OK assenza errore
125500161220         IF  V01cau = *blanks;
125600161220           ErrMessage  = *on;
125700161220           ErrGenerico = *on;
125800161220           PosCurOPZ   = *on;
125900161220           V01msg = Msg(02);
126000161220           V01msg = %trim(V01msg) + ' Assenza non presente';
126100161220           leavesr;
126200161220         ENDIF;
126300161220
126400161220       //?Devo annullare l'assenza
126500161220         clear FNLVF1DS;
126600170116         fnlvf1ds.ILVF1ric = V01opz;
126700161220         fnlvf1ds.ILVF1atn = V01atn;
126800161220         fnlvf1r (kpjba:FNLVF1DS);
126900161220         IF  fnlvf1ds.OLVF1err <> *blanks;
127000161220           ErrMessage  = *on;
127100161220           ErrGenerico = *on;
127200161220           PosCurOPZ   = *on;
127300161220           V01msg = fnlvf1ds.OLVF1msg;
127400161220           leavesr;
127500161220         ENDIF;
127600161220
127700161220       ENDSR;
127800161214
127900161214      /end-free
128000161214       //--------------------------------------------------------------
128100161214       //?Schiere a tempo di compilazione.
128200161214       //--------------------------------------------------------------
128300161214
128400170116** -  MSG --------------------------------------------------------------------*
128500161214Non è stata impostata la filiale gestione                                     01
128600161219Opzione errata                                                                02
128700170116Immettere un'opzione su questa pagina prima di premere F13                    03
128800170116Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa04
128900170116Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F1305
