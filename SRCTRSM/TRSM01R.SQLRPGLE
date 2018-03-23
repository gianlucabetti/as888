000100151112      //--------------------------------------------------------------
000200160113      //?TRSM01R - Crea File Tariffe Inserite
000300151112      //--------------------------------------------------------------
000400151112     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500151112
000600151112      //---------------------------------------------------------------
000700151112      //?Dichiarazione file.
000800151112      //---------------------------------------------------------------
000900151113      // - Eventi
001000151113     fFNEVB04L  if   e           k disk
001100151113
001200151113      // - Tabelle
001300151209     fTABEL00F  uf   e           k disk
001400151117
001500151117      // - Tariffe
001600151124     fTNTAM00L  if   e           k disk    rename(TNTAM000:TNTAM00)
001700151117     fTNTAM01L  if   e           k disk
001800151113
001900151113      // - Bolle Sede
002000151117     fTITAS41C  if   e           k disk    rename(TITAS000:TITAS410)
002100151117     f                                     rename(TITAS010:TITAS411)
002200151117     f                                     rename(TITASP00:TITAS41P)
002300151117     fTITA730C  if   e           k disk
002400151113     fFIAR531C  if   e           k disk
002500151113     fTITAI01L  if   e           k disk
002600151113
002700151113      // - Contrassegni
002800151113     fTNCSB03L  if   e           k disk
002900151113
003000151113      // - File di WORK per bolle da ritassare
003100160107     fWFRFT10F  if a e             disk
003200151117
003300151117      // - File Controllo Tariffe Nuove
003400151117     fTNRST00F  o    e             disk
003500151117     fTNRSD01L  uf a e           k disk
003600151112
003700151112      //---------------------------------------------------------------
003800151112      //?Definizione costanti.
003900151112      //---------------------------------------------------------------
004000151112
004100151112      //---------------------------------------------------------------
004200151112      //?Definizione schiere.
004300151112      //---------------------------------------------------------------
004400151113      // - Calcolo tassazione VARIE
004500151113     d DTASV         e ds
004600151113     d  sv                     1     20    dim(20)
004700151113     d  va                    21    140p 3 dim(20)
004800151113
004900151116      // - Scrittura varie fatturare nel file di lavoro
005000151113     d                 ds
005100151113     d  RFTsv1f                1      1
005200151113     d  RFTsv2f                2      2
005300151113     d  RFTsv3f                3      3
005400151113     d  RFTsv4f                4      4
005500151113     d  RFTsv5f                5      5
005600151113     d  RFTsv6f                6      6
005700151113     d  RFTsv7f                7      7
005800151113     d  RFTsv8f                8      8
005900151113     d  RFTsv9f                9      9
006000151113     d  RFTs10f               10     10
006100151113     d  RFTs11f               11     11
006200151113     d  RFTs12f               12     12
006300151113     d  RFTs13f               13     13
006400151113     d  RFTs14f               14     14
006500151113     d  RFTs15f               15     15
006600151113     d  RFTs16f               16     16
006700151113     d  RFTs17f               17     17
006800151113     d  RFTs18f               18     18
006900151113     d  RFTs19f               19     19
007000151113     d  RFTs20f               20     20
007100151113     d  RFT_svf                1     20    dim(20)
007200151113
007300151113     d                 ds
007400151113     d  RFTva1f                1     11  3
007500151113     d  RFTva2f               12     22  3
007600151113     d  RFTva3f               23     33  3
007700151113     d  RFTva4f               34     44  3
007800151113     d  RFTva5f               45     55  3
007900151113     d  RFTva6f               56     66  3
008000151113     d  RFTva7f               67     77  3
008100151113     d  RFTva8f               78     88  3
008200151113     d  RFTva9f               89     99  3
008300151113     d  RFTv10f              100    110  3
008400151113     d  RFTv11f              111    121  3
008500151113     d  RFTv12f              122    132  3
008600151113     d  RFTv13f              133    143  3
008700151113     d  RFTv14f              144    154  3
008800151113     d  RFTv15f              155    165  3
008900151113     d  RFTv16f              166    176  3
009000151113     d  RFTv17f              177    187  3
009100151113     d  RFTv18f              188    198  3
009200151113     d  RFTv19f              199    209  3
009300151113     d  RFTv20f              210    220  3
009400151113     d  RFT_vaf                1    220  3 dim(20)
009500151117
009600151124      // - Scrittura varie ritassate nel file di lavoro
009700151116     d                 ds
009800151116     d  RFTsv1r                1      1
009900151116     d  RFTsv2r                2      2
010000151116     d  RFTsv3r                3      3
010100151116     d  RFTsv4r                4      4
010200151116     d  RFTsv5r                5      5
010300151116     d  RFTsv6r                6      6
010400151116     d  RFTsv7r                7      7
010500151116     d  RFTsv8r                8      8
010600151116     d  RFTsv9r                9      9
010700151116     d  RFTs10r               10     10
010800151116     d  RFTs11r               11     11
010900151116     d  RFTs12r               12     12
011000151116     d  RFTs13r               13     13
011100151116     d  RFTs14r               14     14
011200151116     d  RFTs15r               15     15
011300151116     d  RFTs16r               16     16
011400151116     d  RFTs17r               17     17
011500151116     d  RFTs18r               18     18
011600151116     d  RFTs19r               19     19
011700151116     d  RFTs20r               20     20
011800151116     d  RFT_svr                1     20    dim(20)
011900151116
012000151116     d                 ds
012100151116     d  RFTva1r                1     11  3
012200151116     d  RFTva2r               12     22  3
012300151116     d  RFTva3r               23     33  3
012400151116     d  RFTva4r               34     44  3
012500151116     d  RFTva5r               45     55  3
012600151116     d  RFTva6r               56     66  3
012700151116     d  RFTva7r               67     77  3
012800151116     d  RFTva8r               78     88  3
012900151116     d  RFTva9r               89     99  3
013000151116     d  RFTv10r              100    110  3
013100151116     d  RFTv11r              111    121  3
013200151116     d  RFTv12r              122    132  3
013300151116     d  RFTv13r              133    143  3
013400151116     d  RFTv14r              144    154  3
013500151116     d  RFTv15r              155    165  3
013600151116     d  RFTv16r              166    176  3
013700151116     d  RFTv17r              177    187  3
013800151116     d  RFTv18r              188    198  3
013900151116     d  RFTv19r              199    209  3
014000151116     d  RFTv20r              210    220  3
014100151116     d  RFT_var                1    220  3 dim(20)
014200151113
014300151113      // - Codici bolla monovarie
014400151113     d CBO             s              2    dim(50)
014500151113     d CBV             s              1    dim(50)
014600151113
014700151113      // - Part.consegna con abilitazione PIN Code
014800151113     d skGMA           s              2a   dim(10)
014900151112
015000151112      //---------------------------------------------------------------
015100151112      //?Definizione aree dati.
015200151112      //---------------------------------------------------------------
015300151112      // - Dati utente
015400151112     d §AzUte        e ds                  extname(AZUTE00F)
015500151112     d                                     dtaara
015600151112     d §DatiUte      e ds                  extname(dDatiUte)
015700151112     d                                     dtaara
015800151112
015900151112      //---------------------------------------------------------------
016000151112      //?Definizione strutture dati.
016100151112      //---------------------------------------------------------------
016200151112      // - Parametri ricevuti
016300151112     d KPJBA         e ds
016400151124
016500151124      // - Ricerca Unificante Padre?
016600151124      /copy gaitrasrc/srcprotopi,TIBS10R
016700151112
016800151112      // - Reperimento dati utente
016900151112     d TIBS34DS      e ds
017000151124
017100151124      // - Reperimento dati Anagrafica Clienti
017200151124      /copy gaitrasrc/srcprotopi,TIBS69R
017300151209
017400151209      // - Tabella 1G
017500151209     d ds1G          e ds                  inz
017600151113
017700151209      // - Codici bolla
017800151124     d ds3A          e ds                  inz
017900151113
018000151113      // - Tabella 7R
018100151124     d ds7R          e ds                  inz
018200151117
018300151117      // - Tabella TB
018400151124     d dsTB          e ds                  inz
018500160627
018600160627      // - DS Flag Operativi TNTAM (TamFlo)
018700160627     d dsTA01        e ds
018800151124
018900151124      // - Rcd FAT file FIAR5
019000151124     d dAR5fat       e ds                  inz
019100151118
019200151118      // - Rcd GEN file FIAR5
019300151124     d dAR5gen       e ds                  inz
019400151113
019500151113      // - Recupero bancali e colli originali
019600151124     d dAR5ban       e ds                  inz
019700151124     d dAR5bnb       e ds                  inz
019800151113
019900151113      // - DS per TASFLO del TITAS
020000151113     d DTASFLO       e ds
020100151113
020200151113      // - DS Calcolo tassazione VARIE
020300151113     d DTAS          e ds
020400151113     d  dsTASflo     e                     extfld(TASflo)
020500151113
020600151113      // - DS Flag Operativi ds DTAS
020700151113     d dTAS01        e ds
020800151113
020900151113      // - DS per valori tassazione peso desunto
021000151113     d dTASpes       e ds
021100151112
021200151112      // - File Variazioni Tariffe
021300151112     d TITAV00F      e ds                  extname(TITAV00F)
021400151117
021500151117      // - File di Work
021600160107     d WFRFTDS       e ds                  extname(WFRFT10F) prefix(w)
021700151112
021800151112      //---------------------------------------------------------------
021900151112      //?Definizione variabili globali.
022000151112      //---------------------------------------------------------------
022100151112      // - Flags booleani
022200151124     d FattFamiglia    s               n   inz(*off)
022300151124     d Fatturato       s               n   inz(*off)
022400151112     d Fine            s               n   inz(*off)
022500151117     d FineBolle       s               n   inz(*off)
022600151117     d FineWrk         s               n   inz(*off)
022700151124     d PiuProgressivi  s               n   inz(*off)
022800160122     d SaltaBolla      s               n   inz(*off)
022900151124     d SiFatt          s               n   inz(*off)
023000160122     d TassaAttuale    s               n   inz(*off)
023100151113
023200151113      // - Indici di schiera
023300151113     d xx              s              3  0
023400151113     d xy              s              3  0
023500151112
023600151112      // - Campi di comodo data
023700151112     d Data_EUR        s               d   datfmt(*eur)
023800151112     d Data_ISO        s               d   datfmt(*iso)
023900151209     d wData           s              8s 0 inz
024000151209     d wDataf          s              8s 0 inz
024100151209     d wDatai          s              8s 0 inz
024200160122     d wseimesi        s              8s 0 inz
024300160122     d wseimesi6       s              6s 0 inz
024400151117
024500151117      // - Campi di comodo contatori
024600151117     d Analizzate      s              9s 0 inz
024700160122     d BolleVariate    s              9s 0 inz
024800151117     d NoTassate       s              9s 0 inz
024900151117     d Tassate         s              9s 0 inz
025000151117     d TassateImpo     s              9s 0 inz
025100151112
025200151112      // - Campi di comodo
025300151125     d antepo          s                   like(TASimv)
025400151116     d wanno           s              4s 0
025500151118     d waasi           s              4s 0
025600151118     d waasf           s              4s 0
025700151116     d wdspfine        s              8s 0
025800151116     d wdspinizio      s              8s 0
025900151113     d wfesta          s              1a
026000151116     d wgiorno         s              2s 0
026100151118     d wimpf           s                   like(RSDimpf)
026200151118     d wimpr           s                   like(RSDimpr)
026300151124     d wksc            s                   like(TAVksc)
026400151116     d wmese           s              2s 0
026500151118     d wmgsi           s              4s 0
026600151118     d wmgsf           s              4s 0
026700151116     d wmmgg           s              4s 0
026800151124     d wnrsvf          s                   like(RSDnrsvf)
026900151124     d wnrsvr          s                   like(RSDnrsvr)
027000151117     d woggi           s              8s 0
027100151113     d wpom            s              1a
027200151124     d wprg            s                   like(TAVprg)
027300151117     d wtrc            s                   like(RSDtrc)
027400160627     d wTaTap          s                   like(§taTap)
027500151119     d w_TAIsvt        s                   like(TAIsvt)
027600151117     d w_TASdiv        s                   like(TASdiv)
027700160122     d w_TASimv        s                   like(TASimv)
027800160122     d w_TASpor        s                   like(TASpor)
027900151117     d w_dTAS          s                   like(dTAS)
028000151117     d w_dTASv         s                   like(dTASv)
028100160125     d wn_dTAS         s                   like(dTAS)
028200160125     d wn_dTASv        s                   like(dTASv)
028300151112
028400151112      //---------------------------------------------------------------
028500151112      //?Definizione Procedure usate.
028600151112      //---------------------------------------------------------------
028700151124      // - Tassazione spedizione senza peso desunto
028800151124     d TNSF20R         pr                  extpgm('TNSF20R')
028900151124     d  kpjba                              likeds(kpjba)
029000151124     d  dTAS                               likeds(dTAS)
029100151124     d  dTASv                              likeds(dTASv)
029200151124
029300151116      // - Tassazione spedizione
029400151116     d TNSF22R         pr                  extpgm('TNSF22R')
029500151116     d  kpjba                              likeds(kpjba)
029600151116     d  dTAS                               likeds(dTAS)
029700151116     d  dTASv                              likeds(dTASv)
029800151116     d  dTASpes                            likeds(dTASpes)
029900151112
030000151112      //---------------------------------------------------------------
030100151112      //?Definizione Prototipi.
030200151112      //---------------------------------------------------------------
030300151124      /copy gaitrasrc/srcprotopr,TIBS10R
030400151112      /copy gaitrasrc/srcprotopr,TIBS34R
030500151124      /copy gaitrasrc/srcprotopr,TIBS69R
030600151112
030700151112      //---------------------------------------------------------------
030800151112      //?Definizione key-list.
030900151112      //---------------------------------------------------------------
031000151113       // - File TABEL00F
031100151113     d k03tabel      e ds                  extname(TABEL00F:*key)
031200151113     d                                     prefix(k_)
031300151112
031400151112      //---------------------------------------------------------------
031500151112      //?M A I N - L I N E
031600151112      //---------------------------------------------------------------
031700151112
031800151112     c     *Entry        plist
031900151112     c                   parm                    kpjba
032000151112
032100151112      /free
032200151112
032300151112       //?Operazioni iniziali
032400151112       exsr RoutInz;
032500151113
032600151113       //?Calcola data elaborazione
032700151113       exsr DataElaborazione;
032800151113
032900151113       //?Carica bolle monovarie
033000151113       exsr carCBO;
033100151113
033200151113       //?Carica particolarità consegna Pin Code.
033300151113       exsr car7RPinCode;
033400151209
033500151209       //?Pulisco le date eventualmente già presenti sul file
033600151209       exsr PulisciTnrs;
033700151112
033800151117       //?Legge le tariffe nuove e scrive file di work con bolle tassate a ritassate
033900151117       //?per ogni tariffa nuova scrive file TNSRT/D
034000151112       exsr Elabora;
034100151209
034200151209       //?Aggiorna ultima data elaborazione cambia tariffa
034300151209       exsr AggTab1G;
034400151112
034500151112       //?Operazioni finali
034600151112       exsr RoutEnd;
034700151112
034800151112       //--------------------------------------------------------------
034900151112       //?Operazioni iniziali.
035000151112       //--------------------------------------------------------------
035100151112       BEGSR RoutInz;
035200151112
035300151112         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
035400151112
035500151112       //?Reperimento dati job
035600151112         exsr DatiJob;
035700151113
035800151113       //?Imposto oggi
035900151113         woggi = %dec(%date());
036000160122
036100160122       //?Imposto oggi - 6 mesi
036200160122         data_ISO = (%date(woggi));
036300160122         data_ISO = data_ISO - %months(6);
036400160122         wseimesi = %dec(data_ISO);
036500160122         wseimesi6 = %dec(%subst(%editc(wseimesi:'X'):3:6):6:0);
036600151113
036700151113       //?Imposto dati di dft
036800151113         k_TBLkut = 1;
036900151112
037000151112       ENDSR;
037100151112
037200151112       //--------------------------------------------------------------
037300151112       //?Reperimento Dati del job (Utente/Operativi).
037400151112       //--------------------------------------------------------------
037500151112       BEGSR DatiJob;
037600151112
037700151112         in(E) §AzUte;
037800151112         IF  NOT %error;
037900151112           in(E) §DatiUte;
038000151112         ENDIF;
038100151112         IF  %error or RSut = *blanks;
038200151112           clear TIBS34ds;
038300151112           tibs34r(tibs34ds);
038400151112           in §AzUte;
038500151112           in §DatiUte;
038600151112         ENDIF;
038700151112
038800151112       ENDSR;
038900151113
039000151113       //--------------------------------------------------------------
039100151113       //?Calcola la data elaborazione.
039200151113       //--------------------------------------------------------------
039300151113       BEGSR DataElaborazione;
039400151113
039500151113         xx = 1;
039600151209
039700151209       //?Devo elaborare dall'ultima data elaborazione + 1 gg fino a oggi - 1 gg
039800151209       //?Cerco ultima data elaborata
039900151209         clear ds1G;
040000151209         k_TBLcod = '1G';
040100151209         k_TBLkey = '1';
040200151209         chain(n) %kds(K03tabel) TABEL00F;
040300151209         IF  %found(TABEL00F);
040400151209           ds1G = TBLuni;
040500151209         ENDIF;
040600151209         data_ISO = (%date(§1Gctu));
040700151209         data_ISO = data_ISO + %days(1);
040800151209         wDatai = %dec(data_ISO);
040900151113
041000151209       //?Da oggi vado indietro di 1 gg
041100151113         data_ISO = (%date(woggi));
041200151209         data_ISO = data_ISO - %days(1);
041300151209         wDataf = %dec(data_ISO);
041400151113
041500151113       ENDSR;
041600151113
041700151113       //--------------------------------------------------------------
041800151113       //?Carico sk bolle monovarie.
041900151113       //--------------------------------------------------------------
042000151113       BEGSR carCBO;
042100151113
042200151117       //?Codici bolla monovaria
042300151113         clear xx;
042400151113         clear ds3A;
042500151113         k_TBLcod = '3A';
042600151113         setll (k_TBLkut:k_TBLcod) TABEL00F;
042700151209         reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
042800151113         DOW  not %eof(TABEL00F);
042900151113           IF  TBLkey <> *blanks;
043000151113             ds3A = TBLuni;
043100151113         //?Codici bolla monovaria
043200151113             IF  §3Asva <> *blanks;
043300151113               xy += 1;
043400151113               CBO(xy) = TBLkey;
043500151113               CBV(xy) = §3Asva;
043600151113             ENDIF;
043700151113           ENDIF;
043800151209           reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
043900151113         ENDDO;
044000151113
044100151113       ENDSR;
044200151113
044300151113       //--------------------------------------------------------------
044400151113       //?Carico particolarità consegna con Pin Code.
044500151113       //--------------------------------------------------------------
044600151113       BEGSR Car7RPinCode;
044700151113
044800151117         k_TBLcod = '7R';
044900151113         clear xx;
045000151117         setll (k_TBLkut:k_TBLcod) TABEL00F;
045100151209         reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
045200151113         DOW  not %eof(TABEL00F);
045300151113           IF  TBLflg = *blanks;
045400151113             ds7R = TBLuni;
045500151113             IF  §7Rpincode = 'S';
045600151113               xx += 1;
045700151113               skGMA(xx) = TBLkey;
045800151113             ENDIF;
045900151113           ENDIF;
046000151209           reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
046100151113         ENDDO;
046200151113
046300151113       ENDSR;
046400151209
046500151209       //--------------------------------------------------------------
046600151209       //?Pulisco le date eventualmente già presenti sul file,
046700151209       //--------------------------------------------------------------
046800151209       BEGSR PulisciTnrs;
046900151209
047000151209         wData = wDatai;
047100151209         DOW  wData <= wDataf;
047200151209
047300151209         //?Cancello i dati già presenti sui file
047400151209           exec sql DELETE from TNRST00F
047500151209                    WHERE RSTdte = :wData;
047600151209
047700151209         //?Cancello i dati già presenti sui file
047800151209           exec sql DELETE from TNRSD00F
047900151209                    WHERE RSDdte = :wData;
048000151209
048100151209           data_ISO = (%date(wData));
048200151209           data_ISO = data_ISO + %days(1);
048300151209           wData = %dec(data_ISO);
048400151209         ENDDO;
048500151209
048600151209       ENDSR;
048700151112
048800151112       //--------------------------------------------------------------
048900151117       //?Elaborazione tariffe nuove
049000151112       //--------------------------------------------------------------
049100151112       BEGSR Elabora;
049200151112
049300151112       //?Estraggo tutte le Tariffe Nuove
049400151113         exec sql DECLARE TAV cursor for
049500151113                  SELECT * from TITAV00F
049600151124                  WHERE TAVtrd = 'TES' and
049700151124                        TAVcta in('D', 'O', 'I') and
049800151209                        TAVdav between :wDatai and :wDataf and
049900170214                        TAVatb = ' ' and TAVpru <> 'BATCH'
050000151113                  ORDER BY TAVksc, TAVctr, TAVprg;
050100151113
050200151117       //?Apertura del cursore
050300151117         exec sql
050400151117         OPEN TAV;
050500151113
050600151117         IF sqlcode < 0;
050700151117           exec sql
050800151117           CLOSE TAV;
050900151117           leavesr;
051000151117         ENDIF;
051100151113
051200151117         DOW  not Fine;
051300151117           exec sql
051400151117           FETCH NEXT from TAV into :TITAV00F;
051500151117           IF sqlcod = 100 or sqlcod < 0;
051600151117             Fine = *on;
051700151117             leave;
051800151117           ENDIF;
051900151124
052000151124           PiuProgressivi = *off;
052100151124           Fatturato = *off;
052200151124           FattFamiglia = *off;
052300151201           clear wdspinizio;
052400151201           clear wdspfine;
052500151201           clear Analizzate;
052600151201           clear NoTassate;
052700151201           clear Tassate;
052800151201           clear TassateImpo;
052900160122           clear BolleVariate;
053000160627           clear WtaTap;
053100151124
053200151124         //?Come prima cosa verifico se ci sono progressivi precedenti
053300151124           wprg = TAVprg;
053400151124           DOW  wprg > 0;
053500151124             wprg -= 1;
053600151124             chain (TAVksc:TAVctr:wprg) TNTAM00L;
053700151124             IF  %found(TNTAM00L);
053800151124               PiuProgressivi = *on;
053900160627         //?Valorizzo il tipo applicazione VDL del progressivo presedente
054000160627               Dsta01=Tamflo;
054100160627               WtaTap=§taTap;
054200151124               leave;
054300151124             ENDIF;
054400151124           ENDDO;
054500151124
054600151124         //?Poi cerco il cliente unificante
054700151124           clear TIBS10DS;
054800151124           tibs10ds.D10tle = 'ST';
054900151124           tibs10ds.D10paf = 'P';
055000151124           tibs10ds.D10cod = TAVksc;
055100151124           TIBS10R (TIBS10DS);
055200151124         //?Se ritorna errore vuol dire che non è un codice figlio
055300151124           IF  tibs10ds.D10err <> *blanks;
055400151124           //?quindi cerco come padre
055500151124             clear TIBS10DS;
055600151124             tibs10ds.D10tle = 'ST';
055700151124             tibs10ds.D10paf = 'F';
055800151124             tibs10ds.D10cod = TAVksc;
055900151124             TIBS10R (TIBS10DS);
056000151124           ENDIF;
056100151124
056200151124         //?non ci sono progressivi precedenti
056300151124           IF  not PiuProgressivi;
056400151124             exsr NoProgressivo;
056500151124             exsr Scrivi;
056600151124           ENDIF;
056700151124
056800151124         //?ci sono progressivi precedenti
056900151124           IF  PiuProgressivi;
057000151124             exsr SiProgressivo;
057100151124           ENDIF;
057200151113
057300151117         ENDDO;
057400151113
057500151117       //?Chiusura del cursore
057600151117         exec sql
057700151117         CLOSE TAV;
057800151112
057900151112       ENDSR;
058000151124
058100151124       //--------------------------------------------------------------
058200151124       //?Elaborazione per nessun progressivo precedente.
058300151124       //--------------------------------------------------------------
058400151124       BEGSR NoProgressivo;
058500151201
058600151201       //?Aggancio la tariffa
058700151201         chain (TAVksc:TAVctr:TAVprg) TNTAM01L;
058800151124
058900151124       //?Verifico se il cliente ha avuto almeno una bolla fatturata
059000151124         clear TIBS69DS;
059100151124         clear CLPdus;
059200151124         I69kcp = TAVksc;
059300151124         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
059400151124       //?ci sono state bolle fatturate
059500160122       //?non considero date inferiori a 6 mesi da oggi
059600160122         IF  CLPdus > 0 and CLPdus >= wseimesi6;
059700151124           Fatturato = *on;
059800151124           leavesr;
059900151124         ENDIF;
060000151124
060100151124       //?non ci sono state bolle fatturate
060200151124       //?cerco se ce ne sono state nella famiglia
060300151124         xx = 1;
060400151124         FOR xx by 1 to %elem(tibs10ds.D10skcary);
060500151124           IF  tibs10ds.D10skcary(xx) = 0;
060600151124             leave;
060700151124           ENDIF;
060800151124           clear TIBS69DS;
060900151124           clear CLPdus;
061000151124           I69kcp = tibs10ds.D10skcary(xx);
061100151124           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
061200160122           IF  CLPdus > 0 and CLPdus >= wseimesi6;
061300151124             FattFamiglia = *on;
061400151124             leavesr;
061500151124           ENDIF;
061600151124         ENDFOR;
061700151124
061800151124       ENDSR;
061900151124
062000151124       //--------------------------------------------------------------
062100151124       //?Elaborazione per progressivo precedente trovato.
062200151124       //--------------------------------------------------------------
062300151124       BEGSR SiProgressivo;
062400151124
062500151124       //?Aggancio il file delle tariffe
062600151124         chain (TAVksc:TAVctr:TAVprg) TNTAM01L;
062700151124
062800151124       //?Cerco la fattura
062900151124         exsr Fattura;
063000151124
063100151124       //?Se la tariffa non è stata utilizzata
063200151124         IF  not SiFatt;
063300151124         //?Stessa cosa che viene fatta per nessun progressivo precedente
063400151124           exsr NoProgressivo;
063500151124           exsr Scrivi;
063600151124           leavesr;
063700151124         ENDIF;
063800151124
063900151124       //?Calcolo il periodo da elaborare
064000151124         exsr DataSped;
064100151124
064200151124       //?Leggo le bolle e scrivo file di work
064300151124         exsr Bolle;
064400151124
064500151124       //?Scrivo file leggendo il file di work
064600151124         IF  Analizzate > 0;
064700151124           exsr Scrivi;
064800151124         ENDIF;
064900151124
065000151124       ENDSR;
065100151113
065200151113       //--------------------------------------------------------------
065300151113       //?Cerco l'ultima fattura dove ho utilizzato la tariffa.
065400151113       //--------------------------------------------------------------
065500151113       BEGSR Fattura;
065600151113
065700151124         SiFatt = *off;
065800151209
065900160107         data_ISO = (%date(TAMddt));
066000151209         wanno   = %subdt(data_ISO:*years);
066100151209         wmese   = %subdt(data_ISO:*months);
066200151209         wgiorno = %subdt(data_ISO:*days);
066300151209         wmmgg = wmese * 100;
066400151209         wmmgg += wgiorno;
066500151209
066600160112         setll (TAVksc:TAVctr:wanno:wmmgg) TITAS41C;
066700151113         readpe (TAVksc:TAVctr) TITAS41C;
066800151113         DOW  not %eof(TITAS41C);
066900170705           IF  TASfiv >= 900;
067000160122             IF  TASdft < wseimesi;
067100160122               leavesr;
067200160122             ENDIF;
067300151116             wdspfine = TASdft;
067400151124             SiFatt = *on;
067500151113             leave;
067600151113           ENDIF;
067700151113           readpe (TAVksc:TAVctr) TITAS41C;
067800151113         ENDDO;
067900151113
068000151113       ENDSR;
068100151116
068200151116       //--------------------------------------------------------------
068300151116       //?Calcolo il periodo bolle da elaborare.
068400151116       //--------------------------------------------------------------
068500151116       BEGSR DataSped;
068600151116
068700151116         data_ISO = (%date(wdspfine));
068800151116         data_ISO = data_ISO - %months(1);
068900151116         wdspinizio = %dec(data_ISO);
069000151118         waasi = %dec(%subst(%editc(wdspinizio:'X'):1:4):4:0);
069100151118         wmgsi = %dec(%subst(%editc(wdspinizio:'X'):5:4):4:0);
069200151118         waasf = %dec(%subst(%editc(wdspfine:'X'):1:4):4:0);
069300151118         wmgsf = %dec(%subst(%editc(wdspfine:'X'):5:4):4:0);
069400151116
069500151116       ENDSR;
069600151113
069700151113       //--------------------------------------------------------------
069800151116       //?Leggo le bolle del periodo calcolato.
069900151113       //--------------------------------------------------------------
070000151113       BEGSR Bolle;
070100151117
070200151117         FineBolle = *off;
070300151113
070400151118         setll (TAVksc:TAVctr:waasi:wmgsi) TITAS41C;
070500151117
070600151117         DOW  not FineBolle;
070700151117
070800151117           clear dTAS;
070900151117           clear dTASv;
071000151117           clear rft_svf;
071100151117           clear rft_vaf;
071200151117           clear rft_svr;
071300151117           clear rft_var;
071400160122           TassaAttuale = *off;
071500160122           SaltaBolla = *off;
071600151117
071700151117           reade (TAVksc:TAVctr) TITAS41C;
071800151117           IF  %eof(TITAS41C);
071900151117             leave;
072000151117           ENDIF;
072100151116
072200151116         //?Solo il periodo calcolato
072300151118           IF  TASaas > waasf;
072400151116             leave;
072500151116           ENDIF;
072600151118           IF  TASmgs > wmgsf;
072700151116             leave;
072800151116           ENDIF;
072900151116
073000151116         //?Solo bolle fatturate
073100151116           IF  TASnft = 0;
073200151116             iter;
073300151116           ENDIF;
073400151116         //?Solo bolle fatturate da sede
073500170705           IF  TASfiv < 900;
073600151116             iter;
073700151116           ENDIF;
073800151117
073900151117           Analizzate += 1;
074000151113
074100151119         //?Verifico se spedizione tassata fino all'imponibile
074200151119           w_TAisvt = '£';
074300151119           chain (TASaas:TASlnp:TASnrs:TASnsp:TAStbl:w_TAIsvt) TITAI01L;
074400151119           IF  %found(TITAI01L);
074500151117         //?Se tassata fino all'imponibile non la devo memorizzare
074600151117             TassateImpo += 1;
074700151117             iter;
074800151117           ENDIF;
074900160122
075000160122         //?Carico la sk delle varie
075100160122           exsr CarTA7;
075200160122
075300160122         //?Tolgo dalla tassazione la varia D=Spese invio fattura
075400160122           xx = %lookup('D':sv);
075500160122           IF  xx > 0;
075600160122             clear sv(xx);
075700160122             TASimv -= va(xx);
075800160122             clear va(xx);
075900160122           ENDIF;
076000160122
076100160122         //?Devo tassare la bolla con la stessa tariffa di fatturazione
076200160122         //?per vedere se la bolla è stata variata dopo la fatturazione
076300160122         //?se variata la devo impostare come Tassata fino all'imponibile
076400160122           exsr TassaBolla;
076500160122         //?se la bolla è stata variata dopo la fatturazione leggo bolla sucessiva
076600160122           IF  SaltaBolla;
076700160122             BolleVariate += 1;
076800160122             iter;
076900160122           ENDIF;
077000151117
077100151113         //?Scrivo file di work
077200151113           exsr WrtWrkFile;
077300151113
077400151113         ENDDO;
077500160111
077600160111       //?Chiudo il TNSF20R così da recuperare sempre la cartello giusta
077700160111       //?per ogni clienti/tariffa
077800160111         TAStla = 'C';
077900160111         tnsf20r (kpjba:dTAS:dTASv);
078000151113
078100151113       ENDSR;
078200160122
078300160122       //--------------------------------------------------------------
078400160122       //?Ritasso la bolla.
078500160122       //--------------------------------------------------------------
078600160122       BEGSR TassaBolla;
078700160122
078800160122         SaltaBolla = *off;
078900160122
079000160122       //?Come prima cosa mi salvo l'imponibile IVA e il porto
079100160122         w_TASimv = TASimv;
079200160122         w_TASpor = TASpor;
079300160122
079400160122       //?Mi salvo la dTAS e la dTASv
079500160122         w_dTAS = dTAS;
079600160122         w_dTASv = dTASv;
079700160122         w_TASdiv = TASdiv;
079800160122
079900160122       //?Imposto che devo tassare con la tariffa attuale
080000160122         TassaAttuale = *on;
080100160122
080200160122       //?Tasso
080300160122         exsr Tassaz;
080400160122
080500160122       //?Reimposto le dTAS e la dTASv
080600160122         dTAS = w_DTAS;
080700160122         dTASv = w_dTASv;
080800160122         TASdiv = w_TASdiv;
080900160122
081000160122         TassaAttuale = *off;
081100160122
081200160122       ENDSR;
081300151113
081400151113       //--------------------------------------------------------------
081500151113       //?Scrivo file di work.
081600151113       //--------------------------------------------------------------
081700151113       BEGSR WrtWrkFile;
081800151113
081900160107         clear WFRFT100;
082000151113
082100151113         RFTfiv = TASfiv;
082200151113         RFTnft = TASnft;
082300151113         RFTdft = TASdft;
082400151113         RFTcbo = TASksc;
082500151113         RFTtbo = TASctr;
082600151113         RFTaas = TASaas;
082700151113         RFTmgs = TASmgs;
082800151113         RFTlnp = TASlnp;
082900151113         RFTnrs = TASnrs;
083000151113         RFTnsp = TASnsp;
083100151113         RFTtbl = TAStbl;
083200151113
083300151113       //?Dati tassazione
083400151117         RFTdivf = TASdiv;
083500151117         RFTporf = TASpor;
083600151113
083700151113         xx = 1;
083800151113         FOR xx by 1 to %elem(sv);
083900151113           rft_svf(xx) = sv(xx);
084000151113           rft_vaf(xx) = va(xx);
084100151113         ENDFOR;
084200151117         RFTimvf = TASimv;
084300151113
084400151113       //?Imposto i dati della nuova tassazione
084500160107         RFTkscr = TAVksc;
084600160107         RFTctrr = TAVctr;
084700160107         RFTprgr = TAVprg;
084800151117         RFTdspr = TAMddt;
084900151117
085000151117       //?Ritasso
085100151117         exsr Tassaz;
085200151117         IF  TASerr <> *blanks;
085300151117           leavesr;
085400151117         ENDIF;
085500160107
085600160107       //?Imposto il progressivo usato in tassazione della bolla.
085700160107         RFTpbo = §AR5prgta;
085800160107       //?se non c'è, visto che è un campo nuovo e non abbiamo impostato il regresso
085900160107       //?metto il progressivo della tariffa variata
086000160107         IF  RFTpbo = *blanks;
086100160107           RFTpbo = %editc(TAMprg:'X');
086200160107         ENDIF;
086300151113
086400160107         write WFRFT100;
086500160107         feod WFRFT10F;
086600151113
086700151113       ENDSR;
086800151113
086900151113       //--------------------------------------------------------------
087000151113       //?Carico le varie.
087100151113       //--------------------------------------------------------------
087200151113       BEGSR CarTA7;
087300151113
087400151113       //?Leggo TITA7 solos e ho la terza varia di TITAS > 0
087500151113         IF  TASsv3 <> *blanks;
087600151113           setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
087700151113           IF  %equal(TITA730C);
087800151113             xx = 3;
087900151113             reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
088000151113             DOW  not %eof(TITA730C);
088100151113         //?Carico le varie
088200151113               xx += 1;
088300151113               sv(xx) = TA7svn;
088400151113               va(xx) = TA7van;
088500151113               reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
088600151113             ENDDO;
088700151113           ENDIF;
088800151113         ENDIF;
088900151113
089000151113       ENDSR;
089100151113
089200151113       //--------------------------------------------------------------
089300151113       //?Ritasso le spedizioni con la nuova tariffa.
089400151113       //--------------------------------------------------------------
089500151113       BEGSR Tassaz;
089600151113
089700151113         clear dTASv;
089800151113         clear dTASpes;
089900151113         clear TASpor;
090000151113         clear TASimv;
090100160530
090200160530       //?Valorizzo il numero dei colli bollettati
090300160530         tasnclb = tasncl ;
090400151119
090500151119       //?Cerco in TITAI l'immagine della spedizione prima della tassazione
090600151119         exsr ric_TAI;
090700151113
090800151113       //?Pulisco i campi dell'assicurazione se calcolata in fattura
090900151113         IF  TASfcm = 'F';
091000151113           clear TASias;
091100151113           clear TASvas;
091200151113         ENDIF;
091300151113
091400151117       //?Cerco il contrassegno
091500151117         IF  TASfca <> *blanks;
091600151117           exsr carCSB;
091700151117         ENDIF;
091800151116
091900151117       //?Se bolla monovaria passo solo quella varia
092000151117         xx = %lookup(TAScbo:CBO);
092100151117         IF  xx > 0;
092200151117           TASsva = CBV(xx);
092300151117         ENDIF;
092400151113
092500151117         TAStc1 = TASftc;
092600160122         TASdct = TASdft;
092700160122         TASdsp = TASaas * 10000;
092800160122         TASdsp += TASmgs;
092900151113
093000151117       //?Flag SI/NO DDT
093100151117         IF  TASll1 = 'C' or TASll1 = 'S';
093200151117           TASddt = 'S';
093300151117         ELSE;
093400151117           clear TASddt;
093500151117         ENDIF;
093600151113
093700151117         clear dTAS01;
093800151117         §ASfbr = TASfbr;
093900151117         §AScca = TAScca;
094000151113
094100151117       //?Imposto flag mail al destinatario
094200151117         dTASflo = TASflo;
094300151117         §ASemd = §FLOemd;
094400151118
094500151118       //?Imposto il flag del calcolo della varia Prenotazione Ritiro
094600151118         TASprt = §FLOado;
094700151113
094800151117       //?Imposto se part.consegna con PinCode
094900151117         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
095000151117           §ASpin = 'S';
095100151117         ENDIF;
095200151113
095300160122       //?La data di riferimento la passo solo se sto tassando con la nuova tariffa
095400160122         IF  not TassaAttuale;
095500160122           §ASdrt = %editc(RFTdspr:'X');
095600160122         ENDIF;
095700151113
095800151117         dsTASflo = dTAS01;
095900151113
096000151117       //?Bancali
096100151118         clear dAR5ban;
096200151117         IF  %subst(TASgva:1:1) = 'B';
096300151117           chain (TASaas:TASlnp:TASnrs:TASnsp:'BAN') FIAR531C;
096400151117           IF  %found(FIAR531C);
096500151117             dAR5ban = AR5uni;
096600151117           ENDIF;
096700151117           TASban = §AR5nba + §AR5nb2;
096800151117         ENDIF;
096900151113
097000151117       //?Colli Originali
097100151118         clear dAR5bnb;
097200151117         IF  %subst(TASgva:1:1) = 'O';
097300151117           chain (TASaas:TASlnp:TASnrs:TASnsp:'BNB') FIAR531C;
097400151117           IF  %found(FIAR531C);
097500151117             dAR5bnb = AR5uni;
097600151117           ENDIF;
097700151117           TASncl = §AR5bcor;
097800151117         ENDIF;
097900151125
098000160107       //?Se bolla di reso
098100151125       //?con varia 'N' valorizzata a 88888888 calcolo l'anteporto
098200151125         IF  TASfbr = 'S';
098300151125           xx = %lookup('N':sv);
098400151125           IF  xx > 0 and va(xx) = 88888888;
098500151125             exsr VariaN;
098600151125         //?Se torna errore imposto come non tassata
098700151125             IF  TASerr <> *blanks;
098800151125               NoTassate += 1;
098900151125               leavesr;
099000151125             ENDIF;
099100151125           ENDIF;
099200151125         ENDIF;
099300151125
099400151125       //?Se bolla con varia & e importo a 88888888
099500151125       //?passo al pgm di tassazione il flag per calolare la varia &
099600151125         xx = %lookup('&':sv);
099700151125         IF  xx > 0 and va(xx) = 88888888;
099800151125           TASfcaa = 'S';
099900151125           clear sv(xx);
100000151125           clear va(xx);
100100151125         ENDIF;
100200151118
100300151118       //?Imposto il flag del calcolo della varia Stampa Packing List
100400151118         clear dAR5gen;
100500151118         chain (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
100600151118         IF  %found(FIAR531C);
100700151118           dAR5gen = AR5uni;
100800151118         ENDIF;
100900151118         IF  §AR5als = 'S' or §AR5alx = 'S';
101000151118           TASspl = 'S';
101100151118         ENDIF;
101200151116
101300151117       //?Verifico se devo calcolare l'addebito di lasciato avviso
101400151119         chain (TASaas:TASlnp:TASnrs:TASnsp:'RIC') FNEVB04L;
101500151119         IF  %found(FNEVB04L);
101600151119           TASric = 'S';
101700151119         ENDIF;
101800151124
101900151124       //?verifico se c'è il rcd FAT su FIAR5
102000151124         clear dAR5fat;
102100151124         chain (TASaas:TASlnp:TASnrs:TASnsp:'FAT') FIAR531C;
102200151124         IF  %found(FIAR531C);
102300151124           dAR5fat = AR5uni;
102400151124         ENDIF;
102500151124
102600151124       //?Se c'è rcd FAT devo passare al pgm di tassazione TNSF20R
102700151124       //?il peso e il volume utilizzato nella fatturazione
102800151124         IF  §AR5pktas > 0;
102900151124           clear TASpkc;
103000151124           clear TASncp;
103100160530           clear TASncr;
103200160530           clear TASvlc;
103300151124           TASpkf = §AR5pktas;
103400151124           TASvlf = §AR5vltas;
103500151124         //?RITASSO
103600151124           tnsf20r (kpjba:dTAS:dTASv);
103700151124
103800151124         ELSE;
103900160122       //?Se non c'è rcd FAT e sto tassando con la tariffa attuale
104000160122       //?richiamo il TNSF20R
104100160122           IF  TassaAttuale;
104200160122             tnsf20r (kpjba:dTAS:dTASv);
104300160122           ELSE;
104400151116
104500160122         //?Se non c'è rcd FAT ritasso chiamando TNSF22R e
104600151124         //?valorizzo i dati nella ds DTASPES
104700160122             TASppkb = TASpkb;
104800160122             TASpdtt = RFTdspr;
104900160504             TASpfvf = TASfvf;
105000160504             TASpncr = TASncr;
105100160504
105200151124         //?RITASSO
105300160122             tnsf22r (kpjba:dTAS:dTASv:dTASpes);
105400160122
105500160122           ENDIF;
105600151124
105700151124         ENDIF;
105800160122
105900160122       //?Se sto tassando con la tariffa attuale
106000160122         IF  TassaAttuale;
106100160122         //?Sono riuscita a tassare e l'imponibile IVA è variato
106200160122         //?quindi è stata variata la bolla dopo la fatturazione
106300160122         //?oppure non sono riuscita a tassare conto la bolla come
106400160122         //?tassata fino all'imponibile
106500160122           IF  TASerr <> *blanks or
106600160122              (TASerr = *blanks and TASimv <> w_TASimv);
106700160122             TassateImpo += 1;
106800160122             SaltaBolla = *on;
106900160122           ENDIF;
107000160122           leavesr;
107100160122         ENDIF;
107200151117
107300151117         IF  TASerr <> *blanks;
107400151117           NoTassate += 1;
107500151117           leavesr;
107600151117         ENDIF;
107700151116
107800151117       //?tassazione
107900151117         RFTdivr = TASdiv;
108000151117         RFTporr = TASpor;
108100151117       //?carico le varie
108200151117         xx = 1;
108300151117         FOR xx by 1 to %elem(sv);
108400151117           rft_svr(xx) = sv(xx);
108500151117           rft_var(xx) = va(xx);
108600151117         ENDFOR;
108700151117         RFTimvr = TASimv;
108800151117
108900151117         Tassate += 1;
109000151113
109100151113       ENDSR;
109200151119
109300151119       //--------------------------------------------------------------
109400151119       //?Recupero importi di tassazione dal file TITAI.
109500151119       //--------------------------------------------------------------
109600151119       BEGSR ric_TAI;
109700151119
109800151119         clear xx;
109900151119
110000151119         setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAI01L;
110100151119         reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAI01L;
110200151119         DOW  not %eof(TITAI01L);
110300151119
110400151119       //?Imposto il porto
110500151119           IF  TAIsvt = ' ';
110600151119             TASpor = TAIvat;
110700151119           ENDIF;
110800151119
110900151119       //?Varie
111000151119           IF  TAIsvt <> ' ' and TAIsvt <> '£';
111100151119             xx += 1;
111200151119             sv(xx) = TAIsvt;
111300151119             va(xx) = TAIvat;
111400151119           ENDIF;
111500151119
111600151119           reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAI01L;
111700151119         ENDDO;
111800151119
111900151119       ENDSR;
112000151113
112100151116       //--------------------------------------------------------------
112200151116       //?Ricerca il Contrassegno.
112300151116       //--------------------------------------------------------------
112400151116       BEGSR carCSB;
112500151116
112600151116         chain (TASaas:TASlnp:TASnrs:TASnsp) TNCSB03L;
112700151116         IF  %found(TNCSB03L) and TAStbl = CSBtbl;
112800151117           TASsta = %editc(CSBsta:'X');
112900151116           TAScas = CSBcas;
113000151116           IF  CSBfus <> *blanks;
113100151116             TAStic = CSBfus;
113200151116           ELSE;
113300151116             TAStic = CSBtpi;
113400151116           ENDIF;
113500151116           TASvca = CSBvca;
113600151116           TAScmb = CSBcmb;
113700151116         ENDIF;
113800151116
113900151116       ENDSR;
114000151125
114100151125       //--------------------------------------------------------------
114200151125       //?Calcolo anteporto varia N.
114300151125       //--------------------------------------------------------------
114400151125       BEGSR VariaN;
114500151125
114600151125       //?salvo i dati di tassazione della bolla
114700160125         wn_dTAS = dTAS;
114800160125         wn_dTASv = dTASv;
114900151125
115000160113         dTAS01 = dsTASflo;
115100160113       //?tolgo flag invio mail al destinatario
115200151125         clear §asemd;
115300160113       //?tolgo flag PinCode
115400160113         clear §aspin;
115500151125         dsTASflo = dTAS01;
115600160113
115700160113       //?tolgo flag per calcolo Diritto di chiamata prenotazione ritiro
115800160113       //?e il flag per calcolo Packing List
115900160113         clear TASprt;
116000160113         clear TASspl;
116100151125
116200151125       //?Richiamo pgm di tassazione senza passare le varie e il porto
116300151125         clear dTASv;
116400151125         clear TASdiv;
116500151125         clear TASpor;
116600151125         clear antepo;
116700151125
116800151125         tnsf20r (kpjba:dTAS:dTASv);
116900151125         IF  TASerr = *blanks;
117000151125           antepo = TASimv;
117100151125         ENDIF;
117200151125
117300151125       //?ripristino i dati di tassazione della bolla
117400160125         dTAS = wn_dTAS;
117500160125         dTASv = wn_dTASv;
117600151125
117700151125       //?valorizzo la varia con i dati dell'anteporto
117800151125         IF  antepo > 0;
117900151125           va(xx) = antepo;
118000151125         ENDIF;
118100151125
118200151125       ENDSR;
118300151113
118400151113       //--------------------------------------------------------------
118500151113       //?Scrivo i file per controllo Nuove Tariffe.
118600151113       //--------------------------------------------------------------
118700151113       BEGSR Scrivi;
118800151117
118900151117         clear TNRST000;
119000151117
119100151117       //?Imposto i dati nel file di testata
119200151117         RSTksc = TAVksc;
119300151117         RSTctr = TAVctr;
119400151117         RSTprg = TAVprg;
119500151209         RSTdte = TAVdav;
119600151123         RSTddt = TAMddt;
119700151117         RSTdspi = wdspinizio;
119800151117         RSTdspf = wdspfine;
119900160627
120000160627       //?Imposto il tipo applicazione VDL del nuovo progressivo
120100160627         dsta01=tamflo;
120200160627         RSTTapVN= §taTap;
120300160627       //?Imposto il tipo applicazione VDL della tariffa precedente
120400160627         RSTTapVP = WtaTap ;
120500151124
120600151124       //?Se Tariffa senza altri progressivi precedenti
120700151124         IF  not PiuProgressivi;
120800151124         //?Imposto il flag mancato confronto tariffe
120900151124           SELECT;
121000151124         //?Altre tariffe utilizzate
121100151124           WHEN  Fatturato;
121200151124             RSTmct = 'A';
121300151124         //?Fatturato Famiglia
121400151124           WHEN  FattFamiglia;
121500151125             RSTmct = 'F';
121600151124         //?Nuovo
121700151124           OTHER;
121800151124             RSTmct = 'N';
121900151124           ENDSL;
122000151124         //?Scrivo il file e poi esco dalla routine
122100151124           write TNRST000;
122200151124           leavesr;
122300151124         ENDIF;
122400151124
122500151124       //?Se Tariffa con altri progressivi ma non ho trovato fatture
122600151124         IF  PiuProgressivi and not SiFatt;
122700151124         //?Imposto il flag mancato confronto tariffe
122800151124           SELECT;
122900151124         //?Altre tariffe utilizzate
123000151124           WHEN  Fatturato;
123100151124             RSTmct = 'B';
123200151124         //?Fatturato Famiglia
123300151124           WHEN  FattFamiglia;
123400151125             RSTmct = 'F';
123500151124         //?Nuovo
123600151124           OTHER;
123700151124             RSTmct = 'N';
123800151124           ENDSL;
123900151124         //?Scrivo il file e poi esco dalla routine
124000151124           write TNRST000;
124100151124           leavesr;
124200151124         ENDIF;
124300151117
124400151117         RSTnbela = Analizzate;
124500151117         RSTnbtas = Tassate;
124600151117         RSTnbnta = NoTassate;
124700151117         RSTnbtim = TassateImpo;
124800160122         RSTnbvar = BolleVariate;
124900151117
125000151117         FineWrk = *off;
125100151117
125200151117       //?Leggo il file di work
125300151117         exec sql DECLARE WRK cursor for
125400160107                  SELECT * from WFRFT10F
125500160107                  WHERE RFTkscr = :TAVksc and RFTctrr = :TAVctr and
125600160107                        RFTprgr = :TAVprg;
125700151117
125800151117       //?Apertura del cursore
125900151117         exec sql
126000151117         OPEN WRK;
126100151117
126200151118         IF  sqlcode < 0;
126300151117           exec sql
126400151117           CLOSE WRK;
126500151117           leavesr;
126600151117         ENDIF;
126700151117
126800151117         DOW  not FineWrk;
126900151117           exec sql
127000151118           FETCH NEXT from WRK into :WFRFTDS;
127100151118           IF  sqlcod = 100 or sqlcod < 0;
127200151117             FineWrk = *on;
127300151117             leave;
127400151117           ENDIF;
127500151117
127600151117         //?Sommo il fatturato e il ritassato
127700151118           RSTimvf += wRFTimvf;
127800151118           RSTimvr += wRFTimvr;
127900151117
128000151117         //?Sommo il porto
128100151118           IF  wRFTporf > *zero;
128200151117             wtrc = '.';
128300151118             wimpf = wRFTporf;
128400151118             wimpr = wRFTporr;
128500151124             IF  wimpf <> 0;
128600151124               wnrsvf = 1;
128700151124             ENDIF;
128800151124             IF  wimpr <> 0;
128900151124               wnrsvr = 1;
129000151124             ENDIF;
129100151117             exsr Somma_Varie;
129200151117           ENDIF;
129300151118
129400151118         //?Sommo le varie Fatturate
129500151118           IF  wRFTsv1f <> *blanks;
129600151118             wtrc = wRFTsv1f;
129700151118             wimpf = wRFTva1f;
129800151124             wnrsvf = 1;
129900151118             clear wimpr;
130000151125             clear wnrsvr;
130100151118             exsr Somma_Varie;
130200151118           ENDIF;
130300151118           IF  wRFTsv2f <> *blanks;
130400151118             wtrc = wRFTsv2f;
130500151118             wimpf = wRFTva2f;
130600151124             wnrsvf = 1;
130700151118             clear wimpr;
130800151125             clear wnrsvr;
130900151118             exsr Somma_Varie;
131000151118           ENDIF;
131100151118           IF  wRFTsv3f <> *blanks;
131200151118             wtrc = wRFTsv3f;
131300151118             wimpf = wRFTva3f;
131400151124             wnrsvf = 1;
131500151118             clear wimpr;
131600151125             clear wnrsvr;
131700151118             exsr Somma_Varie;
131800151118           ENDIF;
131900151118           IF  wRFTsv4f <> *blanks;
132000151118             wtrc = wRFTsv4f;
132100151118             wimpf = wRFTva4f;
132200151124             wnrsvf = 1;
132300151118             clear wimpr;
132400151125             clear wnrsvr;
132500151118             exsr Somma_Varie;
132600151118           ENDIF;
132700151118           IF  wRFTsv5f <> *blanks;
132800151118             wtrc = wRFTsv5f;
132900151118             wimpf = wRFTva5f;
133000151124             wnrsvf = 1;
133100151118             clear wimpr;
133200151125             clear wnrsvr;
133300151118             exsr Somma_Varie;
133400151118           ENDIF;
133500151118           IF  wRFTsv6f <> *blanks;
133600151118             wtrc = wRFTsv6f;
133700151118             wimpf = wRFTva6f;
133800151124             wnrsvf = 1;
133900151118             clear wimpr;
134000151125             clear wnrsvr;
134100151118             exsr Somma_Varie;
134200151118           ENDIF;
134300151118           IF  wRFTsv7f <> *blanks;
134400151118             wtrc = wRFTsv7f;
134500151118             wimpf = wRFTva7f;
134600151124             wnrsvf = 1;
134700151118             clear wimpr;
134800151125             clear wnrsvr;
134900151118             exsr Somma_Varie;
135000151118           ENDIF;
135100151118           IF  wRFTsv8f <> *blanks;
135200151118             wtrc = wRFTsv8f;
135300151118             wimpf = wRFTva8f;
135400151124             wnrsvf = 1;
135500151118             clear wimpr;
135600151125             clear wnrsvr;
135700151118             exsr Somma_Varie;
135800151118           ENDIF;
135900151118           IF  wRFTsv9f <> *blanks;
136000151118             wtrc = wRFTsv9f;
136100151118             wimpf = wRFTva9f;
136200151124             wnrsvf = 1;
136300151118             clear wimpr;
136400151125             clear wnrsvr;
136500151118             exsr Somma_Varie;
136600151118           ENDIF;
136700151118           IF  wRFTs10f <> *blanks;
136800151118             wtrc = wRFTs10f;
136900151118             wimpf = wRFTv10f;
137000151124             wnrsvf = 1;
137100151118             clear wimpr;
137200151125             clear wnrsvr;
137300151118             exsr Somma_Varie;
137400151118           ENDIF;
137500151118           IF  wRFTs11f <> *blanks;
137600151118             wtrc = wRFTs11f;
137700151118             wimpf = wRFTv11f;
137800151124             wnrsvf = 1;
137900151118             clear wimpr;
138000151125             clear wnrsvr;
138100151118             exsr Somma_Varie;
138200151118           ENDIF;
138300151118           IF  wRFTs12f <> *blanks;
138400151118             wtrc = wRFTs12f;
138500151118             wimpf = wRFTv12f;
138600151124             wnrsvf = 1;
138700151118             clear wimpr;
138800151125             clear wnrsvr;
138900151118             exsr Somma_Varie;
139000151118           ENDIF;
139100151118           IF  wRFTs13f <> *blanks;
139200151118             wtrc = wRFTs13f;
139300151118             wimpf = wRFTv13f;
139400151124             wnrsvf = 1;
139500151118             clear wimpr;
139600151125             clear wnrsvr;
139700151118             exsr Somma_Varie;
139800151118           ENDIF;
139900151118           IF  wRFTs14f <> *blanks;
140000151118             wtrc = wRFTs14f;
140100151118             wimpf = wRFTv14f;
140200151124             wnrsvf = 1;
140300151118             clear wimpr;
140400151125             clear wnrsvr;
140500151118             exsr Somma_Varie;
140600151118           ENDIF;
140700151118           IF  wRFTs15f <> *blanks;
140800151118             wtrc = wRFTs15f;
140900151118             wimpf = wRFTv15f;
141000151124             wnrsvf = 1;
141100151118             clear wimpr;
141200151125             clear wnrsvr;
141300151118             exsr Somma_Varie;
141400151118           ENDIF;
141500151118           IF  wRFTs16f <> *blanks;
141600151118             wtrc = wRFTs16f;
141700151118             wimpf = wRFTv16f;
141800151124             wnrsvf = 1;
141900151118             clear wimpr;
142000151125             clear wnrsvr;
142100151118             exsr Somma_Varie;
142200151118           ENDIF;
142300151118           IF  wRFTs17f <> *blanks;
142400151118             wtrc = wRFTs17f;
142500151118             wimpf = wRFTv17f;
142600151124             wnrsvf = 1;
142700151118             clear wimpr;
142800151125             clear wnrsvr;
142900151118             exsr Somma_Varie;
143000151118           ENDIF;
143100151118           IF  wRFTs18f <> *blanks;
143200151118             wtrc = wRFTs18f;
143300151118             wimpf = wRFTv18f;
143400151124             wnrsvf = 1;
143500151118             clear wimpr;
143600151125             clear wnrsvr;
143700151118             exsr Somma_Varie;
143800151118           ENDIF;
143900151118           IF  wRFTs19f <> *blanks;
144000151118             wtrc = wRFTs19f;
144100151118             wimpf = wRFTv19f;
144200151124             wnrsvf = 1;
144300151118             clear wimpr;
144400151125             clear wnrsvr;
144500151118             exsr Somma_Varie;
144600151118           ENDIF;
144700151118           IF  wRFTs20f <> *blanks;
144800151118             wtrc = wRFTs20f;
144900151118             wimpf = wRFTv20f;
145000151124             wnrsvf = 1;
145100151118             clear wimpr;
145200151125             clear wnrsvr;
145300151118             exsr Somma_Varie;
145400151118           ENDIF;
145500151118
145600151118         //?Sommo le varie Ritassate
145700151118           IF  wRFTsv1r <> *blanks;
145800151118             wtrc = wRFTsv1r;
145900151118             clear wimpf;
146000151125             clear wnrsvf;
146100151118             wimpr = wRFTva1r;
146200151124             wnrsvr = 1;
146300151118             exsr Somma_Varie;
146400151118           ENDIF;
146500151118           IF  wRFTsv2r <> *blanks;
146600151118             wtrc = wRFTsv2r;
146700151118             clear wimpf;
146800151125             clear wnrsvf;
146900151118             wimpr = wRFTva2r;
147000151124             wnrsvr = 1;
147100151118             exsr Somma_Varie;
147200151118           ENDIF;
147300151118           IF  wRFTsv3r <> *blanks;
147400151118             wtrc = wRFTsv3r;
147500151118             clear wimpf;
147600151125             clear wnrsvf;
147700151118             wimpr = wRFTva3r;
147800151124             wnrsvr = 1;
147900151118             exsr Somma_Varie;
148000151118           ENDIF;
148100151118           IF  wRFTsv4r <> *blanks;
148200151118             wtrc = wRFTsv4r;
148300151118             clear wimpf;
148400151125             clear wnrsvf;
148500151118             wimpr = wRFTva4r;
148600151124             wnrsvr = 1;
148700151118             exsr Somma_Varie;
148800151118           ENDIF;
148900151118           IF  wRFTsv5r <> *blanks;
149000151118             wtrc = wRFTsv5r;
149100151118             clear wimpf;
149200151125             clear wnrsvf;
149300151118             wimpr = wRFTva5r;
149400151124             wnrsvr = 1;
149500151118             exsr Somma_Varie;
149600151118           ENDIF;
149700151118           IF  wRFTsv6r <> *blanks;
149800151118             wtrc = wRFTsv6r;
149900151118             clear wimpf;
150000151125             clear wnrsvf;
150100151118             wimpr = wRFTva6r;
150200151124             wnrsvr = 1;
150300151118             exsr Somma_Varie;
150400151118           ENDIF;
150500151118           IF  wRFTsv7r <> *blanks;
150600151118             wtrc = wRFTsv7r;
150700151118             clear wimpf;
150800151125             clear wnrsvf;
150900151118             wimpr = wRFTva7r;
151000151124             wnrsvr = 1;
151100151118             exsr Somma_Varie;
151200151118           ENDIF;
151300151118           IF  wRFTsv8r <> *blanks;
151400151118             wtrc = wRFTsv8r;
151500151118             clear wimpf;
151600151125             clear wnrsvf;
151700151118             wimpr = wRFTva8r;
151800151124             wnrsvr = 1;
151900151118             exsr Somma_Varie;
152000151118           ENDIF;
152100151118           IF  wRFTsv9r <> *blanks;
152200151118             wtrc = wRFTsv9r;
152300151118             clear wimpf;
152400151125             clear wnrsvf;
152500151118             wimpr = wRFTva9r;
152600151124             wnrsvr = 1;
152700151118             exsr Somma_Varie;
152800151118           ENDIF;
152900151118           IF  wRFTs10r <> *blanks;
153000151118             wtrc = wRFTs10r;
153100151118             clear wimpf;
153200151125             clear wnrsvf;
153300151118             wimpr = wRFTv10r;
153400151124             wnrsvr = 1;
153500151118             exsr Somma_Varie;
153600151118           ENDIF;
153700151118           IF  wRFTs11r <> *blanks;
153800151118             wtrc = wRFTs11r;
153900151118             clear wimpf;
154000151125             clear wnrsvf;
154100151118             wimpr = wRFTv11r;
154200151124             wnrsvr = 1;
154300151118             exsr Somma_Varie;
154400151118           ENDIF;
154500151118           IF  wRFTs12r <> *blanks;
154600151118             wtrc = wRFTs12r;
154700151118             clear wimpf;
154800151125             clear wnrsvf;
154900151118             wimpr = wRFTv12r;
155000151124             wnrsvr = 1;
155100151118             exsr Somma_Varie;
155200151118           ENDIF;
155300151118           IF  wRFTs13r <> *blanks;
155400151118             wtrc = wRFTs13r;
155500151118             clear wimpf;
155600151125             clear wnrsvf;
155700151118             wimpr = wRFTv13r;
155800151124             wnrsvr = 1;
155900151118             exsr Somma_Varie;
156000151118           ENDIF;
156100151118           IF  wRFTs14r <> *blanks;
156200151118             wtrc = wRFTs14r;
156300151118             clear wimpf;
156400151125             clear wnrsvf;
156500151118             wimpr = wRFTv14r;
156600151124             wnrsvr = 1;
156700151118             exsr Somma_Varie;
156800151118           ENDIF;
156900151118           IF  wRFTs15r <> *blanks;
157000151118             wtrc = wRFTs15r;
157100151118             clear wimpf;
157200151125             clear wnrsvf;
157300151118             wimpr = wRFTv15r;
157400151124             wnrsvr = 1;
157500151118             exsr Somma_Varie;
157600151118           ENDIF;
157700151118           IF  wRFTs16r <> *blanks;
157800151118             wtrc = wRFTs16r;
157900151118             clear wimpf;
158000151125             clear wnrsvf;
158100151118             wimpr = wRFTv16r;
158200151124             wnrsvr = 1;
158300151118             exsr Somma_Varie;
158400151118           ENDIF;
158500151118           IF  wRFTs17r <> *blanks;
158600151118             wtrc = wRFTs17r;
158700151118             clear wimpf;
158800151125             clear wnrsvf;
158900151118             wimpr = wRFTv17r;
159000151124             wnrsvr = 1;
159100151118             exsr Somma_Varie;
159200151118           ENDIF;
159300151118           IF  wRFTs18r <> *blanks;
159400151118             wtrc = wRFTs18r;
159500151118             clear wimpf;
159600151125             clear wnrsvf;
159700151118             wimpr = wRFTv18r;
159800151124             wnrsvr = 1;
159900151118             exsr Somma_Varie;
160000151118           ENDIF;
160100151118           IF  wRFTs19r <> *blanks;
160200151118             wtrc = wRFTs19r;
160300151118             clear wimpf;
160400151125             clear wnrsvf;
160500151118             wimpr = wRFTv19r;
160600151124             wnrsvr = 1;
160700151118             exsr Somma_Varie;
160800151118           ENDIF;
160900151118           IF  wRFTs20r <> *blanks;
161000151118             wtrc = wRFTs20r;
161100151118             clear wimpf;
161200151125             clear wnrsvf;
161300151118             wimpr = wRFTv20r;
161400151124             wnrsvr = 1;
161500151118             exsr Somma_Varie;
161600151118           ENDIF;
161700151118
161800151117         ENDDO;
161900160112
162000160113       //?Se arrivo qua e non ho il valore del fatturato
162100160113       //?devo impostare il flag di mancato confronto a 'X'
162200160113       //?questo capita quando ci sono le bolle ma non sono state tassate
162300160113       //?dalla fatturazione quindi non c'è confronto.
162400160113         IF  RSTimvf = 0;
162500160113           RSTmct = 'X';
162600160113         ENDIF;
162700151117
162800151117       //?Scrivo file di testata
162900151117         write TNRST000;
163000151117
163100151117       //?Chiusura del cursore
163200151117         exec sql
163300151117         CLOSE WRK;
163400151113
163500151113       ENDSR;
163600151117
163700151117       //--------------------------------------------------------------
163800151117       //?Sommo le varie nel file di dettaglio.
163900151117       //--------------------------------------------------------------
164000151117       BEGSR Somma_Varie;
164100151117
164200151117         chain (RSTksc:RSTctr:RSTprg:RSTdte:wtrc) TNRSD01L;
164300151117         IF  %found(TNRSD01L);
164400151117             RSDimpf += wimpf;
164500151117             RSDimpr += wimpr;
164600151124             IF  wnrsvf > 0;
164700151124               RSDnrsvf += 1;
164800151124             ENDIF;
164900151124             IF  wnrsvr > 0;
165000151124               RSDnrsvr += 1;
165100151124             ENDIF;
165200151117             update TNRSD000;
165300151117           ELSE;
165400151117             clear TNRSD000;
165500151117             RSDksc = RSTksc;
165600151117             RSDctr = RSTctr;
165700151117             RSDprg = RSTprg;
165800151117             RSDdte = RSTdte;
165900151117             RSDtrc = wtrc;
166000151117             RSDimpf += wimpf;
166100151117             RSDimpr += wimpr;
166200151124             IF  wnrsvf > 0;
166300151124               RSDnrsvf = 1;
166400151124             ENDIF;
166500151124             IF  wnrsvr > 0;
166600151124               RSDnrsvr = 1;
166700151124             ENDIF;
166800151118             write TNRSD000;
166900151117           ENDIF;
167000151117
167100151117       ENDSR;
167200151209
167300151209       //--------------------------------------------------------------
167400151209       //?Aggiorna data ultima elaborazione cambia tariffa.
167500151209       //--------------------------------------------------------------
167600151209       BEGSR AggTab1G;
167700151209
167800151209         clear ds1G;
167900151209         k_TBLcod = '1G';
168000151209         k_TBLkey = '1';
168100151209         chain %kds(K03tabel) TABEL00F;
168200151209         IF  %found(TABEL00F);
168300151209           ds1G = TBLuni;
168400151209           §1Gctu = wDataf;
168500151209           TBLuni = ds1G;
168600151209           update TABEL;
168700151209         ENDIF;
168800151209
168900151209       ENDSR;
169000151112
169100151112       //--------------------------------------------------------------
169200151112       //?Operazioni finali.
169300151112       //--------------------------------------------------------------
169400151112       BEGSR RoutEnd;
169500151112
169600151112         *inLR = *on;
169700151112         return;
169800151112
169900151112       ENDSR;
170000151112
170100151112      /end-free
