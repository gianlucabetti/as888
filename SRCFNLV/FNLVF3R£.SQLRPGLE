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
043500170323         //?Escludo gli autisti che non sono in forza lavoro
043600161220           clear FNLVF9DS;
043700161220           fnlvf9ds.ILVF9pdr = APDpdr;
043800161220           fnlvf9ds.ILVF9data = Oggi;
043900161220           fnlvf9r (fnlvf9ds);
044000170323           IF  fnlvf9ds.OLVF9flg = 'N' or
044100170323               fnlvf9ds.OLVF9err <> *blanks;
044200161220             iter;
044300161220           ENDIF;
044400161220
044500161221         //?Il controllo va fatto su tutti i gg previsti in tabella
044600161221           xx = 1;
044700161221           FOR xx by 1 to §5Aggass;
044800161221
044900161221             wDAT = skData(xx);
045000161221             wPDR = APDpdr;
045100161221
045200170126           //?se la data da controllare è < data attivazione
045300170126           //?provo con altro giorno
045400170126             IF  wDAT < wDataAtt;
045500170126               iter;
045600170126             ENDIF;
045700170126
045800161221           //?Controllo se ha distinta/Assenza
045900170126             exsr Controlla;
046000161221
046100161221             SELECT;
046200161221           //?OK se ha la distinta e non l'assenza
046300161221             WHEN  OkDistinta and not OkAssenza;
046400161221               iter;
046500161221           //?OK se non ha la distinta ed ha l'assenza
046600161221             WHEN  not OkDistinta and OkAssenza;
046700161221               iter;
046800161221             ENDSL;
046900161221
047000161221           //?Se richiamato per solo controllo
047100161221           //?e vedo già che c'è una discordanza esco dal pgm
047200161221             IF  fnlvf3ds.ILVF3trc = 'C';
047300170130               fnlvf3ds.OLVF3disco = 'S';
047400161221               exsr RoutEnd;
047500161221             ENDIF;
047600161220
047700161221           //?Scrivo il file di work
047800161221             exsr ScriviWrkf;
047900161221           ENDFOR;
048000161220
048100161215         ENDDO;
048200161215
048300161219         feod WFADS01L;
048400161215
048500161215       ENDSR;
048600161215
048700161215       //--------------------------------------------------------------
048800161219       //?Gestione videata S01.
048900161215       //--------------------------------------------------------------
049000161215       BEGSR GesS01;
049100161215
049200161215       //?Inizializzazione videata
049300161215         IF  wInzS01;
049400161215           exsr InzS01;
049500161215           wInzS01 = *off;
049600161215         ENDIF;
049700161215
049800161215       //?Visualizzazione del SFL (se ci sono dati)
049900161215         SflDsp = (S01nrr <= *zeros);
050000161215
050100161215       //?Posizionamento cursore
050200161219         V01rcd = V01csr;
050300161215
050400161215       //?Emissione Testata e Piede con tasti funzionali abilitati
050500161215         write LVF3T01;
050600161215         write LVF3P01;
050700161215
050800161215       //?Emissione videata
050900161215         exfmt LVF3C01;
051000161215         ErrMessage  = *off;
051100161215         ErrGenerico = *off;
051200161215         clear V01msg;
051300170116
051400170116         IF  dsp_aid <> c_F12 and
051500170116             dsp_aid <> c_F13;
051600170116           clear savV01csr;
051700170116           clear savV01cs1;
051800170116           clear savV01cs2;
051900170116           clear savV01opz;
052000170116           clear savV01op1;
052100170116         ENDIF;
052200161215
052300161215         SELECT;
052400161219
052500161228       //?F05=Refresh
052600161219         WHEN  dsp_aid = c_F05;
052700161219           exsr F05S01;
052800161228
052900161228       //?F06=Conferma
053000161228         WHEN  dsp_aid = c_F06;
053100170118           exsr OpzS01;
053200170118           IF  ErrGenerico;
053300170118             leavesr;
053400170118           ENDIF;
053500170117           exsr F06S01;
053600170116
053700170116       //?F12=Ritorno
053800170116         WHEN  dsp_aid = c_F12;
053900170116           exsr F12S01;
054000161228
054100161228       //?F13=Ripeti Opzione
054200161228         WHEN  dsp_aid = c_F13;
054300161228           exsr F13S01;
054400161215
054500161215       //?Enter
054600161215         OTHER;
054700161219         //?Controllo dati
054800161219           IF  V01csr > *zero;
054900161219             exsr OpzS01;
055000161219             IF  ErrGenerico;
055100161219               leavesr;
055200161219             ENDIF;
055300161219           ENDIF;
055400170111           IF  V01csr = *zero;
055500170111             V01csr = savS01nrr;
055600170111           ENDIF;
055700161215
055800161215         ENDSL;
055900161215
056000161215       ENDSR;
056100161215
056200161215       //--------------------------------------------------------------
056300161215       //?Operazioni finali.
056400161215       //--------------------------------------------------------------
056500161215       BEGSR RoutEnd;
056600161215
056700161215       //?Cancello file di work dalla qtemp
056800161220         IF  %open(WFADS01L);
056900161220           close WFADS01L;
057000161220           cmd = 'DLTF FILE(QTEMP/WFADS01L)';
057100161220           ExecuteCommand (cmd);
057200161220           cmd = 'DLTF FILE(QTEMP/WFADS00F)';
057300161220           ExecuteCommand (cmd);
057400161220         ENDIF;
057500161215
057600161215         *inLR = *on;
057700161215         return;
057800161215
057900161215       ENDSR;
058000161219
058100161219       //--------------------------------------------------------------
058200161219       //?Reperimento Dati del job (Utente/Operativi).
058300161219       //--------------------------------------------------------------
058400161219       BEGSR DatiJob;
058500161219
058600161219         in(E) §AzUte;
058700161219         IF  NOT %error;
058800161219           in(E) §DatiUte;
058900161219         ENDIF;
059000161219         IF  %error or RSut = *blanks;
059100170126           clear TIBS34DS;
059200161219           tibs34r(tibs34ds);
059300161219           in §AzUte;
059400161219           in §DatiUte;
059500161219         ENDIF;
059600161219
059700161219       ENDSR;
059800161215
059900161215       //--------------------------------------------------------------
060000161215       //?Calcolo la data da elaborare.
060100161215       //--------------------------------------------------------------
060200161215       BEGSR CalcolaData;
060300161215
060400161215         xx = 1;
060500161215         FOR xx by 1 to §5Aggass;
060600161215           clear XGIOLAVDS;
060700161220           IXGLdata = wDatada;
060800161220           IXGLsub  = 'S';
060900161220           IXGLgg   = xx;
061000161215           IXGLlav  = 'S';
061100161215           IXGLpa   = 'P';
061200161221           IXGLfil  = wFGS;
061300161215           XGIOLAV (xgiolavds);
061400161215           IF  OXGLerr = *blanks;
061500161215             skData(xx) = OXGLdata;
061600161215           ELSE;
061700161219             Data_ISO = %date(wDatada:*iso);
061800161220             Data_ISO = Data_ISO - %days(xx);
061900161219             skData(xx) = %dec(Data_ISO);
062000161215           ENDIF;
062100161215         ENDFOR;
062200161215
062300161215       ENDSR;
062400161221
062500161221       //--------------------------------------------------------------
062600161221       //?Controllo se ci sono discordanze
062700161221       //--------------------------------------------------------------
062800161221       BEGSR Controlla;
062900161221
063000161221         OkAssenza = *off;
063100161221         OkDistinta = *off;
063200161221         clear wNFV;
063300161221         clear wATN;
063400161221         clear wCAU;
063500161221
063600161221       //?Leggo le Distinte del giorno
063700161221       //?e aggiorno file di work
063800161221         exsr LeggiDistinte;
063900161221
064000161221       //?Leggo le Assenze del giorno
064100161221       //?e aggiorno file di work
064200161221         exsr LeggiAssenze;
064300161221
064400161221       ENDSR;
064500161215
064600161215       //--------------------------------------------------------------
064700161215       //?Scrivo il file di Work.
064800161215       //--------------------------------------------------------------
064900161215       BEGSR ScriviWrkf;
065000161215
065100161221       //?Scrivo
065200161221         clear WFADS000;
065300161221         WFAfgs = wFGS;
065400161221         WFApdr = APDpdr;
065500161221         WFAdat = skData(xx);
065600161221         WFAnfv = wNFV;
065700161221         WFAcau = wCAU;
065800161221         WFAatn = wATN;
065900161221         write WFADS000;
066000161215
066100161215       ENDSR;
066200161214
066300161214       //--------------------------------------------------------------
066400161214       //?Leggo le distinte del giorno.
066500161214       //--------------------------------------------------------------
066600161214       BEGSR LeggiDistinte;
066700161221
066800161221         EndSQL = *off;
066900161215
067000161215         exec sql
067100161215         DECLARE DST cursor for
067200161214         SELECT DSTnfv from FIDST00F
067300161221         WHERE DSTfgs = :wFGS and DSTnpg = 4 and
067400161221               DSTdfv = :wDAT and DSTatb = ' ' and
067500161221               DSTpdr = :wPDR;
067600161214
067700161214       //?Apertura del cursore
067800161214         exec sql open DST;
067900161214         IF  sqlcode < 0;
068000161214           EndSQL = *on;
068100161214         ENDIF;
068200161214
068300161214         DOW  not EndSQL;
068400161214           exec sql
068500161214           fetch next from DST into :DSTnfv;
068600161214           IF  sqlcod = 100 or sqlcod < 0;
068700161214             EndSQL = *on;
068800161214             leave;
068900161214           ENDIF;
069000161221
069100161221           wNFV = DSTnfv;
069200161215           OkDistinta = *on;
069300161215           leave;
069400161214
069500161214         ENDDO;
069600161214
069700161215         exec sql close DST;
069800161214
069900161214       ENDSR;
070000161214
070100161214       //--------------------------------------------------------------
070200161214       //?Leggo le assenze del giorno.
070300161214       //--------------------------------------------------------------
070400161214       BEGSR LeggiAssenze;
070500161215
070600161215         EndSQL = *off;
070700161215
070800161215         exec sql
070900161215         DECLARE AGA cursor for
071000161219         SELECT AGAcau, AGAatn from FIAGA00F
071100161221         WHERE AGApdr = :wPDR and AGAdat = :wDAT and
071200161215               AGAatb = ' ';
071300161215
071400161215       //?Apertura del cursore
071500161215         exec sql open AGA;
071600161215         IF  sqlcode < 0;
071700161215           EndSQL = *on;
071800161215         ENDIF;
071900161215
072000161215         DOW  not EndSQL;
072100161215           exec sql
072200161219           fetch next from AGA into :AGAcau, :AGAatn;
072300161215           IF  sqlcod = 100 or sqlcod < 0;
072400161215             EndSQL = *on;
072500161215             leave;
072600161215           ENDIF;
072700161221
072800161221           wCAU = AGAcau;
072900161221           wATN = AGAatn;
073000161215           OkAssenza = *on;
073100161215           leave;
073200161215
073300161215         ENDDO;
073400161215
073500161215         exec sql close AGA;
073600161214
073700161214       ENDSR;
073800161215
073900161215       //--------------------------------------------------------------
074000161215       //?Inizializzazione videata S01.
074100161215       //--------------------------------------------------------------
074200161215       BEGSR InzS01;
074300161219
074400161219         EndS01 = *off;
074500161215
074600161215       //?Pulizia subfile
074700161215         exsr PulSfl;
074800161215
074900161215       //?Leggo file di work per caricare il subfile
075000161215         exsr LeggiWork;
075100161219
075200161219         SflEnd = *on;
075300161228         savS01nrr = S01nrr;
075400161219
075500161219       //?Imposto il posizionamento al primo rcd
075600161219         IF  S01nrr > 0;
075700161219           V01rcd = 1;
075800161219         ELSE;
075900161219           clear V01rcd;
076000161219         ENDIF;
076100161219
076200161219         V01csr = V01rcd;
076300161215
076400161215       ENDSR;
076500161214
076600161214       //--------------------------------------------------------------
076700161219       //?Pulisco il Subfile.
076800161214       //--------------------------------------------------------------
076900161214       BEGSR PulSfl;
077000161214
077100161214         SflDsp    = *on;
077200161214         SflDspCtl = *on;
077300161214         write LVF3C01;
077400161214         SflDspCtl = *off;
077500161214         SflEnd    = *off;
077600161214
077700161228         clear savS01Nrr;
077800161214         clear S01Nrr;
077900161214         clear V01rcd;
078000161219         clear V01csr;
078100161214         clear V01msg;
078200161214         ErrMessage  = *off;
078300161214         ErrGenerico = *off;
078400161219
078500161219         WindDspF = IndDspF;
078600161219         reset WindDspFb;
078700161219         IndDspF  = WindDspF;
078800161214
078900161214       ENDSR;
079000161215
079100161215       //--------------------------------------------------------------
079200161215       //?Leggo il file di Work.
079300161215       //--------------------------------------------------------------
079400161215       BEGSR LeggiWork;
079500161219
079600161219         EndS01 = *off;
079700161219
079800161219       //?Dichiarazione cursore
079900161219         exec sql
080000161219         DECLARE WRK cursor for
080100161219         SELECT * from WFADS01L
080200161219         ORDER BY WFAfgs, WFApdr, WFAdat;
080300161219
080400161219         //?Apertura del cursore
080500161219         exec sql
080600161219         open WRK;
080700161219
080800161219         IF  sqlcode < 0;
080900161219           EndS01 = *on;
081000161219           exec sql close WRK;
081100161219           leavesr;
081200161219         ENDIF;
081300161219
081400161219         DOW  not EndS01;
081500161219           exec sql
081600161219           fetch next from WRK into :WFADS00F;
081700161219           IF  sqlcod = 100 or sqlcod < 0;
081800161219             EndS01 = *on;
081900161219             leave;
082000161219           ENDIF;
082100161221
082200161221         //?Verifico se c'è ancora la discordanza
082300161221           wDAT = WFAdat;
082400161221           wPDR = WFApdr;
082500161221           exsr Controlla;
082600161221
082700161221         //?Se non c'è più discordanza cancello il rcd
082800161221           IF  (OkDistinta and not OkAssenza) or
082900161221               (not OkDistinta and OkAssenza);
083000170116             chain (WFAfgs:WFApdr:WFAdat) WFADS01L;
083100161221             IF  %found(WFADS01L);
083200161221               delete WFADS000;
083300161221               iter;
083400161221             ENDIF;
083500161221           ENDIF;
083600161219
083700161219         //?Carico i dati nel subfile
083800161220           exsr CaricaS01;
083900161219
084000161219         ENDDO;
084100161219
084200161219         exec sql
084300161219         close WRK;
084400161215
084500161215       ENDSR;
084600161219
084700161219       //--------------------------------------------------------------
084800161219       //?Carico i dati nel Subfile.
084900161219       //--------------------------------------------------------------
085000161220       BEGSR CaricaS01;
085100161219
085200170116         SflNxtChg = *off;
085300161228         clear LVF3S01;
085400161219         PosCurOPZ = *off;
085500161219
085600161219       //?Imposto i campi da WFADS
085700161219         clear V01dpdr;
085800161228         V01pdr = WFApdr;
085900161219         chain (k_APDtip:WFApdr) FIAPD01L;
086000161219         IF  %found(FIAPD01L);
086100161220           V01dpdr = APDrsc;
086200161219         ENDIF;
086300161219
086400161220         Data_ISO = %date(WFAdat:*iso);
086500161219         Data_EUR = Data_ISO;
086600161219         V01data = %dec(Data_EUR);
086700161219
086800161221         IF  WFAnfv > *zeros;
086900161219           V01dst = 'SI';
087000161219         ELSE;
087100161219           clear V01dst;
087200161219         ENDIF;
087300161219
087400161219         clear V01dcau;
087500161228         V01cau = WFAcau;
087600161219         clear dACA;
087700170126         clear TIBS02DS;
087800161219         T02mod = 'C';
087900161219         T02cod = 'ACA';
088000161219         T02ke1 = V01cau;
088100161219         T02sif = KNSIF;
088200161219         TNTBE_RicercaControllo (kpjba : tibs02ds);
088300161219         IF  T02err = *blanks;
088400161219           dACA = T02uni;
088500161219         ENDIF;
088600161219         V01dcau = §ACAdes;
088700161219
088800161219         V01atn = WFAatn;
088900161219
089000161219         S01nrr += 1;
089100161219         write LVF3S01;
089200161219
089300161219       ENDSR;
089400161219
089500161219       //--------------------------------------------------------------
089600161219       //?Gestione tasto funzionale F05 da Subfile S01.
089700161221       //?F05=Refresh
089800161219       //--------------------------------------------------------------
089900161219       BEGSR F05S01;
090000161221
090100161221       //?Come prima cosa devo pulire il file di work
090200161221         exec sql
090300161221         DELETE from QTEMP/WFADS00F;
090400161219
090500161221       //?Riparto dalla lettura degli autisti
090600161221         exsr LeggiAutisti;
090700161221
090800161221       //?Quindi ricarico il subfile con il file di work aggiornato
090900161221         wInzS01 = *on;
091000161219
091100161219       ENDSR;
091200161228
091300161228       //--------------------------------------------------------------
091400161228       //?Gestione tasto funzionale F06 da Subfile S01.
091500161228       //?F06=Conferma
091600161228       //--------------------------------------------------------------
091700161228       BEGSR F06S01;
091800161228
091900161228         readc LVF3S01;
092000161228
092100161228         DOW  not %eof(FNLVF3D);
092200161228           SflNxtChg = *off;
092300161228           V01rcd    = S01nrr;
092400161228           PosCurOPZ = *off;
092500161228
092600170116         //?Le opzioni 1 - 2 - 3 - 4
092700161228         //?Posso farle solo se non ho già un'assenza
092800170117           IF  V01opz <> 'A' and V01opz <> *blanks and
092900170117               V01cau <> *blanks;
093000161228             ErrMessage  = *on;
093100161228             ErrGenerico = *on;
093200161230             PosCurOPZ = *on;
093300161228             V01opz = 'E';
093400161228           ENDIF;
093500161228
093600170116         //?Opzione A
093700161228         //?Posso farla solo se ho un'assenza
093800161228           IF  V01opz = 'A' and V01cau = *blanks;
093900161228             ErrMessage  = *on;
094000161228             ErrGenerico = *on;
094100161230             PosCurOPZ = *on;
094200161228             V01opz = 'E';
094300161228           ENDIF;
094400161228
094500161228         //?Se non ho errore eseguo l'opzione immessa
094600161228           IF  not ErrMessage and V01opz <> *blanks;
094700161228             clear FNLVF1DS;
094800161228             fnlvf1ds.ILVF1pdr = V01pdr;
094900161228             Data_EUR = %date(V01data:*eur);
095000161228             Data_ISO = Data_EUR;
095100161228             fnlvf1ds.ILVF1dat = %dec(Data_ISO);
095200170116             fnlvf1ds.ILVF1ric = V01opz;
095300161228             IF  V01opz = 'A';
095400161228               fnlvf1ds.ILVF1atn = V01atn;
095500170116             ENDIF;
095600161228             fnlvf1r (kpjba:FNLVF1DS);
095700161228             IF  fnlvf1ds.OLVF1err <> *blanks;
095800161228               ErrMessage  = *on;
095900161228               ErrGenerico = *on;
096000170206               V01Msg = fnlvf1ds.OLVF1msg;
096100161228               V01opz = 'E';
096200161228             ENDIF;
096300161230             PosCurOPZ = *off;
096400161230             clear V01opz;
096500161228           ENDIF;
096600161228
096700161228           //?Aggiornamento sfl
096800161228           V01csr = V01rcd;
096900161228           update LVF3S01;
097000161228           readc LVF3S01;
097100161228         ENDDO;
097200161228
097300161228       //?Se non ci sono stati errori ricarico tutto
097400161228         IF  not ErrMessage;
097500161228           exsr F05S01;
097600161228         ENDIF;
097700161228
097800161228       ENDSR;
097900170116
098000170116       //--------------------------------------------------------------
098100170116       //?Gestione tasto funzionale F12 da Subfile S01
098200170116       //?F12=Ritorno
098300170116       //--------------------------------------------------------------
098400170116       BEGSR F12S01;
098500170116
098600170116       //?Chiusura del programma
098700170116         Fine = *on;
098800170116
098900170116       ENDSR;
099000161228
099100161228       //--------------------------------------------------------------
099200161228       //?Gestione tasto funzionale F13 da Subfile S01.
099300161228       //?F13=Ripeti Opzione
099400161228       //--------------------------------------------------------------
099500161228       BEGSR F13S01;
099600170116
099700170116       //?Reperimento pagina del sfl (e Nrr del suo primo record)?
099800170116       //?dalla posizione del cursore?
099900170116         wPag = V01csr / wSflPag;
100000170116         wRig = %rem(V01csr : wSflPag);
100100170116         IF  wRig > *zero;
100200170116           wRig = (wPag * wSflPag) + 1;
100300170116         ELSE;                           // -?ultimo rec. della pag.?
100400170116           wRig = (wPag * wSflPag) - wSflPag + 1;
100500170116         ENDIF;
100600170116         savV01csr = wRig;
100700170116
100800170116       //?Controllo le opzioni inserite
100900170117         PosCurOPZ = *off;
101000170116
101100170116       //?Reperimento della 1ª opzione inserita?
101200170116       //?nella pagina dov'è il cursore?
101300170116         readc LVF3S01;
101400170116         DOW  not %eof(FNLVF3D) and V01opz <> *blanks and
101500170116              S01nrr < savV01csr;
101600170116           SflNxtChg = *on;
101700170116           update LVF3S01;
101800170116           readc LVF3S01;
101900170116         ENDDO;
102000170116         IF  not %eof(FNLVF3D) and V01opz <> *blanks;
102100170116           SflNxtChg = *on;
102200170116           update LVF3S01;
102300170116         ENDIF;
102400170116
102500170116       //?Nessuna opzione nella pagina
102600170116         IF  %eof(FNLVF3D) or
102700170116             S01nrr < savV01csr or
102800170116             S01nrr > (savV01csr + wSflPag - 1);
102900170116           V01rcd = savV01csr;
103000170116           V01csr = savV01csr;
103100170116           clear ForzaOpz;
103200170116           clear savV01opz;
103300170116           clear savV01op1;
103400170116           clear savV01cs1;
103500170116           clear savV01cs2;
103600170116           ErrGenerico = *on;
103700170116           ErrMessage  = *on;
103800170116           PosCurOPZ   = *on;
103900170116           V01msg = Msg(03);
104000170116           leavesr;
104100170116         ENDIF;
104200170116
104300170116       //?Memorizzazione 1ª opzione nella pagina?
104400170116         savV01csr = S01nrr;
104500170116         savV01opz = V01opz;
104600170116         IF  S01nrr <> savV01cs1 or
104700170116             V01opz <> savV01op1;
104800170116           clear ForzaOpz;
104900170116         ENDIF;
105000170116         savV01cs1 = S01nrr;
105100170116         savV01op1 = V01opz;
105200170116
105300170116       //?Reperimento delle altre eventuali opzioni?
105400170116         readc LVF3S01;
105500170116         DOW  not %eof(FNLVF3D);
105600170116
105700170116         //?Aggiornamento subfile?
105800170116           IF  V01opz <> *blanks;
105900170116             SflNxtChg = *on;
106000170116             update LVF3S01;
106100170116           ENDIF;
106200170116
106300170116         //?Reperita una 2ª opzione?
106400170116           SELECT;
106500170116         //?Vuota!?
106600170116           WHEN  V01opz = *blanks;
106700170116         //?1ª opzione nella pagina del cursore  e?
106800170116         //?2ª opzione successiva diversa:?
106900170116         //?messaggio di avviso di "ricopertura"?
107000170116           WHEN  (savV01csr >= wRig       and
107100170116                  S01nrr    >  savV01csr) and
107200170116                  V01opz    <> savV01opz  and
107300170116                  not ForzaOpz;
107400170116             ForzaOpz = *on;
107500170116             savV01cs2 = S01nrr;
107600170116             V01csr    = savV01csr;
107700170116             V01rcd    = savV01csr;
107800170116             ErrGenerico = *on;
107900170116             ErrMessage  = *on;
108000170116             PosCurOPZ   = *on;
108100170116             V01msg = Msg(04);
108200170116             leavesr;
108300170116
108400170116         //?2ª opzione sul record dove è posizionato il cursore?
108500170116         //?(la nuova da considerare per la duplicazione)?
108600170116           WHEN  V01csr = S01nrr;
108700170116             savV01csr = S01nrr;
108800170116             savV01opz = V01opz;
108900170116
109000170116         //?Più opzioni nella stessa videata e?
109100170116         //?cursore su nessuna delle opzioni?
109200170116         //?(riposizionare il cursore)?
109300170116         //?Se stessa opzione in videate diverse: vince la?
109400170116         //?posizione della prima opzione.?
109500170116           WHEN  V01csr <> savV01csr and
109600170116                 V01csr <> S01nrr    and
109700170116                 S01nrr <  wRig + wSflPag;
109800170116             savV01cs2 = S01nrr;
109900170116             V01rcd    = savV01csr;
110000170116             V01csr    = savV01csr;
110100170116             ErrGenerico = *on;
110200170116             ErrMessage  = *on;
110300170116             PosCurOPZ   = *on;
110400170116             V01msg = Msg(05);
110500170116             leavesr;
110600170116
110700170116           ENDSL;
110800170116
110900170116           readc LVF3S01;
111000170116
111100170116         ENDDO;
111200170116
111300170116       //?Opzione NON abilitata?
111400170116         IF  savV01opz <> '1' and savV01opz <> '2' and
111500170116             savV01opz <> '3' and savV01opz <> '4' and
111600170116             savV01opz <> 'A';
111700170116           V01csr = savV01csr;
111800170116           V01rcd = savV01csr;
111900170116           ErrGenerico = *on;
112000170116           ErrMessage  = *on;
112100170116           PosCurOPZ   = *on;
112200170116           V01msg = Msg(02);
112300170116           leavesr;
112400170116         ENDIF;
112500170117
112600170117         PosCurOPZ = *off;
112700170116
112800170116       //?Ripetizione opzione sul resto del subfile?
112900170116         S01nrr = savV01csr + 1;
113000170116         chain  S01nrr  LVF3S01;
113100170116         DOW  %found(FNLVF3D);
113200170116           SflNxtChg = *on;
113300170116           V01opz = savV01opz;
113400170116           update LVF3S01;
113500170116           S01nrr += 1;
113600170116           chain S01nrr LVF3S01;
113700170116         ENDDO;
113800161228
113900161228       ENDSR;
114000161219
114100161219       //--------------------------------------------------------------
114200161219       //?Gestione opzioni Subfile S01.
114300161219       //--------------------------------------------------------------
114400161219       BEGSR OpzS01;
114500170116
114600170116         clear savV01csr;
114700161219
114800161219         readc LVF3S01;
114900161219
115000161220         DOW  not %eof(FNLVF3D);
115100161219
115200161219           SflNxtChg = *off;
115300161219           WindDspF  = IndDspF;
115400161219           reset WindDspFb;
115500161219           IndDspF   = WindDspF;
115600161219           V01rcd    = S01nrr;
115700161219
115800161219           PosCurOPZ = *off;
115900161219
116000161219           SELECT;
116100161219         //?- Nessuna opzione
116200161219           WHEN  V01opz = *blank;
116300161219
116400170116         //?- Immissione Assenza (1-2-3-4)
116500170116           WHEN  V01opz = '1' or V01opz = '2' or
116600170116                 V01opz = '3' or V01opz = '4';
116700161219           //?Posso farla solo se non ho già un'assenza
116800170116             IF  V01cau <> *blanks;
116900170116               ErrMessage  = *on;
117000170116               ErrGenerico = *on;
117100170116               PosCurOPZ   = *on;
117200170116               V01msg = Msg(02);
117300170116               V01msg = %trim(V01msg) + ' Assenza già presente';
117400170116             ENDIF;
117500161219
117600161219         //?- A = Annulla
117700161219           WHEN  V01opz = 'A';
117800161219           //?Posso farla solo se ho un'assenza
117900170116             IF  V01cau = *blanks;
118000170116               ErrMessage  = *on;
118100170116               ErrGenerico = *on;
118200170116               PosCurOPZ   = *on;
118300170116               V01msg = Msg(02);
118400170116               V01msg = %trim(V01msg) + ' Assenza non presente';
118500170116             ENDIF;
118600161219
118700161219           OTHER;
118800161219             ErrMessage  = *on;
118900161219             ErrGenerico = *on;
119000161219             PosCurOPZ   = *on;
119100170109             V01msg = Msg(02);
119200161219           ENDSL;
119300161219
119400161219           //?Aggiornamento sfl
119500170116           SflNxtChg = *on;
119600170116           V01csr    = V01rcd;
119700161219           update LVF3S01;
119800161219
119900161219           IF  ErrMessage or ErrGenerico;
120000161219             leave;
120100161219           ENDIF;
120200161219
120300161219           readc LVF3S01;
120400161219
120500161219         ENDDO;
120600161219
120700161219       ENDSR;
120800161219
120900161219       //--------------------------------------------------------------
121000170116       //?Opzione "I" immissione assenza Subfile S01.
121100161219       //--------------------------------------------------------------
121200170116       BEGSR OpzioneI;
121300161220
121400161221       //?Se OK assenza errore
121500161220         IF  V01cau <> *blanks;
121600161220           ErrMessage  = *on;
121700161220           ErrGenerico = *on;
121800161220           PosCurOPZ   = *on;
121900161220           V01msg = Msg(02);
122000161220           V01msg = %trim(V01msg) + ' Assenza già presente';
122100161220           leavesr;
122200161220         ENDIF;
122300161219
122400161219       //?Devo inserire l'assenza
122500161220         clear FNLVF1DS;
122600170116         fnlvf1ds.ILVF1ric = V01opz;
122700161220         fnlvf1ds.ILVF1pdr = V01pdr;
122800161220         Data_EUR = %date(V01data:*eur);
122900161220         Data_ISO = Data_EUR;
123000161220         fnlvf1ds.ILVF1dat = %dec(Data_ISO);
123100161220         fnlvf1r (kpjba:FNLVF1DS);
123200161220         IF  fnlvf1ds.OLVF1err <> *blanks;
123300161219           ErrMessage  = *on;
123400161219           ErrGenerico = *on;
123500161219           PosCurOPZ   = *on;
123600161220           V01msg = fnlvf1ds.OLVF1msg;
123700161219           leavesr;
123800161219         ENDIF;
123900161219
124000161219       ENDSR;
124100161220
124200161220       //--------------------------------------------------------------
124300161220       //?Opzione "A" Subfile S01.
124400161220       //--------------------------------------------------------------
124500161220       BEGSR OpzioneA;
124600161220
124700161221       //?Se no OK assenza errore
124800161220         IF  V01cau = *blanks;
124900161220           ErrMessage  = *on;
125000161220           ErrGenerico = *on;
125100161220           PosCurOPZ   = *on;
125200161220           V01msg = Msg(02);
125300161220           V01msg = %trim(V01msg) + ' Assenza non presente';
125400161220           leavesr;
125500161220         ENDIF;
125600161220
125700161220       //?Devo annullare l'assenza
125800161220         clear FNLVF1DS;
125900170116         fnlvf1ds.ILVF1ric = V01opz;
126000161220         fnlvf1ds.ILVF1atn = V01atn;
126100161220         fnlvf1r (kpjba:FNLVF1DS);
126200161220         IF  fnlvf1ds.OLVF1err <> *blanks;
126300161220           ErrMessage  = *on;
126400161220           ErrGenerico = *on;
126500161220           PosCurOPZ   = *on;
126600161220           V01msg = fnlvf1ds.OLVF1msg;
126700161220           leavesr;
126800161220         ENDIF;
126900161220
127000161220       ENDSR;
127100161214
127200161214      /end-free
127300161214       //--------------------------------------------------------------
127400161214       //?Schiere a tempo di compilazione.
127500161214       //--------------------------------------------------------------
127600161214
127700170116** -  MSG --------------------------------------------------------------------*
127800161214Non è stata impostata la filiale gestione                                     01
127900161219Opzione errata                                                                02
128000170116Immettere un'opzione su questa pagina prima di premere F13                    03
128100170116Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa04
128200170116Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F1305
