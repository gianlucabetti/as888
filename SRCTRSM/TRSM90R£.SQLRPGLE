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
018500151124
018600151124      // - Rcd FAT file FIAR5
018700151124     d dAR5fat       e ds                  inz
018800151118
018900151118      // - Rcd GEN file FIAR5
019000151124     d dAR5gen       e ds                  inz
019100151113
019200151113      // - Recupero bancali e colli originali
019300151124     d dAR5ban       e ds                  inz
019400151124     d dAR5bnb       e ds                  inz
019500151113
019600151113      // - DS per TASFLO del TITAS
019700151113     d DTASFLO       e ds
019800151113
019900151113      // - DS Calcolo tassazione VARIE
020000151113     d DTAS          e ds
020100151113     d  dsTASflo     e                     extfld(TASflo)
020200151113
020300151113      // - DS Flag Operativi ds DTAS
020400151113     d dTAS01        e ds
020500151113
020600151113      // - DS per valori tassazione peso desunto
020700151113     d dTASpes       e ds
020800151112
020900151112      // - File Variazioni Tariffe
021000151112     d TITAV00F      e ds                  extname(TITAV00F)
021100151117
021200151117      // - File di Work
021300160107     d WFRFTDS       e ds                  extname(WFRFT10F) prefix(w)
021400151112
021500151112      //---------------------------------------------------------------
021600151112      //?Definizione variabili globali.
021700151112      //---------------------------------------------------------------
021800151112      // - Flags booleani
021900151124     d FattFamiglia    s               n   inz(*off)
022000151124     d Fatturato       s               n   inz(*off)
022100151112     d Fine            s               n   inz(*off)
022200151117     d FineBolle       s               n   inz(*off)
022300151117     d FineWrk         s               n   inz(*off)
022400151124     d PiuProgressivi  s               n   inz(*off)
022500160122     d SaltaBolla      s               n   inz(*off)
022600151124     d SiFatt          s               n   inz(*off)
022700160122     d TassaAttuale    s               n   inz(*off)
022800151113
022900151113      // - Indici di schiera
023000151113     d xx              s              3  0
023100151113     d xy              s              3  0
023200151112
023300151112      // - Campi di comodo data
023400151112     d Data_EUR        s               d   datfmt(*eur)
023500151112     d Data_ISO        s               d   datfmt(*iso)
023600151209     d wData           s              8s 0 inz
023700151209     d wDataf          s              8s 0 inz
023800151209     d wDatai          s              8s 0 inz
023900160122     d wseimesi        s              8s 0 inz
024000160122     d wseimesi6       s              6s 0 inz
024100151117
024200151117      // - Campi di comodo contatori
024300151117     d Analizzate      s              9s 0 inz
024400160122     d BolleVariate    s              9s 0 inz
024500151117     d NoTassate       s              9s 0 inz
024600151117     d Tassate         s              9s 0 inz
024700151117     d TassateImpo     s              9s 0 inz
024800151112
024900151112      // - Campi di comodo
025000151125     d antepo          s                   like(TASimv)
025100151116     d wanno           s              4s 0
025200151118     d waasi           s              4s 0
025300151118     d waasf           s              4s 0
025400151116     d wdspfine        s              8s 0
025500151116     d wdspinizio      s              8s 0
025600151113     d wfesta          s              1a
025700151116     d wgiorno         s              2s 0
025800151118     d wimpf           s                   like(RSDimpf)
025900151118     d wimpr           s                   like(RSDimpr)
026000151124     d wksc            s                   like(TAVksc)
026100151116     d wmese           s              2s 0
026200151118     d wmgsi           s              4s 0
026300151118     d wmgsf           s              4s 0
026400151116     d wmmgg           s              4s 0
026500151124     d wnrsvf          s                   like(RSDnrsvf)
026600151124     d wnrsvr          s                   like(RSDnrsvr)
026700151117     d woggi           s              8s 0
026800151113     d wpom            s              1a
026900151124     d wprg            s                   like(TAVprg)
027000151117     d wtrc            s                   like(RSDtrc)
027100151119     d w_TAIsvt        s                   like(TAIsvt)
027200151117     d w_TASdiv        s                   like(TASdiv)
027300160122     d w_TASimv        s                   like(TASimv)
027400160122     d w_TASpor        s                   like(TASpor)
027500151117     d w_dTAS          s                   like(dTAS)
027600151117     d w_dTASv         s                   like(dTASv)
027700160125     d wn_dTAS         s                   like(dTAS)
027800160125     d wn_dTASv        s                   like(dTASv)
027900151112
028000151112      //---------------------------------------------------------------
028100151112      //?Definizione Procedure usate.
028200151112      //---------------------------------------------------------------
028300151124      // - Tassazione spedizione senza peso desunto
028400151124     d TNSF20R         pr                  extpgm('TNSF20R')
028500151124     d  kpjba                              likeds(kpjba)
028600151124     d  dTAS                               likeds(dTAS)
028700151124     d  dTASv                              likeds(dTASv)
028800151124
028900151116      // - Tassazione spedizione
029000151116     d TNSF22R         pr                  extpgm('TNSF22R')
029100151116     d  kpjba                              likeds(kpjba)
029200151116     d  dTAS                               likeds(dTAS)
029300151116     d  dTASv                              likeds(dTASv)
029400151116     d  dTASpes                            likeds(dTASpes)
029500151112
029600151112      //---------------------------------------------------------------
029700151112      //?Definizione Prototipi.
029800151112      //---------------------------------------------------------------
029900151124      /copy gaitrasrc/srcprotopr,TIBS10R
030000151112      /copy gaitrasrc/srcprotopr,TIBS34R
030100151124      /copy gaitrasrc/srcprotopr,TIBS69R
030200151112
030300151112      //---------------------------------------------------------------
030400151112      //?Definizione key-list.
030500151112      //---------------------------------------------------------------
030600151113       // - File TABEL00F
030700151113     d k03tabel      e ds                  extname(TABEL00F:*key)
030800151113     d                                     prefix(k_)
030900151112
031000151112      //---------------------------------------------------------------
031100151112      //?M A I N - L I N E
031200151112      //---------------------------------------------------------------
031300151112
031400151112     c     *Entry        plist
031500151112     c                   parm                    kpjba
031600151112
031700151112      /free
031800151112
031900151112       //?Operazioni iniziali
032000151112       exsr RoutInz;
032100151113
032200151113       //?Calcola data elaborazione
032300151113       exsr DataElaborazione;
032400151113
032500151113       //?Carica bolle monovarie
032600151113       exsr carCBO;
032700151113
032800151113       //?Carica particolarità consegna Pin Code.
032900151113       exsr car7RPinCode;
033000151209
033100151209       //?Pulisco le date eventualmente già presenti sul file
033200151209       exsr PulisciTnrs;
033300151112
033400151117       //?Legge le tariffe nuove e scrive file di work con bolle tassate a ritassate
033500151117       //?per ogni tariffa nuova scrive file TNSRT/D
033600151112       exsr Elabora;
033700151209
033800151209       //?Aggiorna ultima data elaborazione cambia tariffa
033900151209       exsr AggTab1G;
034000151112
034100151112       //?Operazioni finali
034200151112       exsr RoutEnd;
034300151112
034400151112       //--------------------------------------------------------------
034500151112       //?Operazioni iniziali.
034600151112       //--------------------------------------------------------------
034700151112       BEGSR RoutInz;
034800151112
034900151112         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
035000151112
035100151112       //?Reperimento dati job
035200151112         exsr DatiJob;
035300151113
035400151113       //?Imposto oggi
035500151113         woggi = %dec(%date());
035600160122
035700160122       //?Imposto oggi - 6 mesi
035800160122         data_ISO = (%date(woggi));
035900160122         data_ISO = data_ISO - %months(6);
036000160122         wseimesi = %dec(data_ISO);
036100160122         wseimesi6 = %dec(%subst(%editc(wseimesi:'X'):3:6):6:0);
036200151113
036300151113       //?Imposto dati di dft
036400151113         k_TBLkut = 1;
036500151112
036600151112       ENDSR;
036700151112
036800151112       //--------------------------------------------------------------
036900151112       //?Reperimento Dati del job (Utente/Operativi).
037000151112       //--------------------------------------------------------------
037100151112       BEGSR DatiJob;
037200151112
037300151112         in(E) §AzUte;
037400151112         IF  NOT %error;
037500151112           in(E) §DatiUte;
037600151112         ENDIF;
037700151112         IF  %error or RSut = *blanks;
037800151112           clear TIBS34ds;
037900151112           tibs34r(tibs34ds);
038000151112           in §AzUte;
038100151112           in §DatiUte;
038200151112         ENDIF;
038300151112
038400151112       ENDSR;
038500151113
038600151113       //--------------------------------------------------------------
038700151113       //?Calcola la data elaborazione.
038800151113       //--------------------------------------------------------------
038900151113       BEGSR DataElaborazione;
039000151113
039100151113         xx = 1;
039200151209
039300151209       //?Devo elaborare dall'ultima data elaborazione + 1 gg fino a oggi - 1 gg
039400151209       //?Cerco ultima data elaborata
039500151209         clear ds1G;
039600151209         k_TBLcod = '1G';
039700151209         k_TBLkey = '1';
039800151209         chain(n) %kds(K03tabel) TABEL00F;
039900151209         IF  %found(TABEL00F);
040000151209           ds1G = TBLuni;
040100151209         ENDIF;
040200151209         data_ISO = (%date(§1Gctu));
040300151209         data_ISO = data_ISO + %days(1);
040400151209         wDatai = %dec(data_ISO);
040500151113
040600151209       //?Da oggi vado indietro di 1 gg
040700151113         data_ISO = (%date(woggi));
040800151209         data_ISO = data_ISO - %days(1);
040900151209         wDataf = %dec(data_ISO);
041000151113
041100151113       ENDSR;
041200151113
041300151113       //--------------------------------------------------------------
041400151113       //?Carico sk bolle monovarie.
041500151113       //--------------------------------------------------------------
041600151113       BEGSR carCBO;
041700151113
041800151117       //?Codici bolla monovaria
041900151113         clear xx;
042000151113         clear ds3A;
042100151113         k_TBLcod = '3A';
042200151113         setll (k_TBLkut:k_TBLcod) TABEL00F;
042300151209         reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
042400151113         DOW  not %eof(TABEL00F);
042500151113           IF  TBLkey <> *blanks;
042600151113             ds3A = TBLuni;
042700151113         //?Codici bolla monovaria
042800151113             IF  §3Asva <> *blanks;
042900151113               xy += 1;
043000151113               CBO(xy) = TBLkey;
043100151113               CBV(xy) = §3Asva;
043200151113             ENDIF;
043300151113           ENDIF;
043400151209           reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
043500151113         ENDDO;
043600151113
043700151113       ENDSR;
043800151113
043900151113       //--------------------------------------------------------------
044000151113       //?Carico particolarità consegna con Pin Code.
044100151113       //--------------------------------------------------------------
044200151113       BEGSR Car7RPinCode;
044300151113
044400151117         k_TBLcod = '7R';
044500151113         clear xx;
044600151117         setll (k_TBLkut:k_TBLcod) TABEL00F;
044700151209         reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
044800151113         DOW  not %eof(TABEL00F);
044900151113           IF  TBLflg = *blanks;
045000151113             ds7R = TBLuni;
045100151113             IF  §7Rpincode = 'S';
045200151113               xx += 1;
045300151113               skGMA(xx) = TBLkey;
045400151113             ENDIF;
045500151113           ENDIF;
045600151209           reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
045700151113         ENDDO;
045800151113
045900151113       ENDSR;
046000151209
046100151209       //--------------------------------------------------------------
046200151209       //?Pulisco le date eventualmente già presenti sul file,
046300151209       //--------------------------------------------------------------
046400151209       BEGSR PulisciTnrs;
046500151209
046600151209         wData = wDatai;
046700151209         DOW  wData <= wDataf;
046800151209
046900151209         //?Cancello i dati già presenti sui file
047000151209           exec sql DELETE from TNRST00F
047100151209                    WHERE RSTdte = :wData;
047200151209
047300151209         //?Cancello i dati già presenti sui file
047400151209           exec sql DELETE from TNRSD00F
047500151209                    WHERE RSDdte = :wData;
047600151209
047700151209           data_ISO = (%date(wData));
047800151209           data_ISO = data_ISO + %days(1);
047900151209           wData = %dec(data_ISO);
048000151209         ENDDO;
048100151209
048200151209       ENDSR;
048300151112
048400151112       //--------------------------------------------------------------
048500151117       //?Elaborazione tariffe nuove
048600151112       //--------------------------------------------------------------
048700151112       BEGSR Elabora;
048800151112
048900151112       //?Estraggo tutte le Tariffe Nuove
049000151113         exec sql DECLARE TAV cursor for
049100151113                  SELECT * from TITAV00F
049200151124                  WHERE TAVtrd = 'TES' and
049300151124                        TAVcta in('D', 'O', 'I') and
049400151209                        TAVdav between :wDatai and :wDataf and
049500160107                        TAVatb = ' ' and TAVnoj <> 'AUMENTO'
049600151113                  ORDER BY TAVksc, TAVctr, TAVprg;
049700151113
049800151117       //?Apertura del cursore
049900151117         exec sql
050000151117         OPEN TAV;
050100151113
050200151117         IF sqlcode < 0;
050300151117           exec sql
050400151117           CLOSE TAV;
050500151117           leavesr;
050600151117         ENDIF;
050700151113
050800151117         DOW  not Fine;
050900151117           exec sql
051000151117           FETCH NEXT from TAV into :TITAV00F;
051100151117           IF sqlcod = 100 or sqlcod < 0;
051200151117             Fine = *on;
051300151117             leave;
051400151117           ENDIF;
051500151124
051600151124           PiuProgressivi = *off;
051700151124           Fatturato = *off;
051800151124           FattFamiglia = *off;
051900151201           clear wdspinizio;
052000151201           clear wdspfine;
052100151201           clear Analizzate;
052200151201           clear NoTassate;
052300151201           clear Tassate;
052400151201           clear TassateImpo;
052500160122           clear BolleVariate;
052600151124
052700151124         //?Come prima cosa verifico se ci sono progressivi precedenti
052800151124           wprg = TAVprg;
052900151124           DOW  wprg > 0;
053000151124             wprg -= 1;
053100151124             chain (TAVksc:TAVctr:wprg) TNTAM00L;
053200151124             IF  %found(TNTAM00L);
053300151124               PiuProgressivi = *on;
053400151124               leave;
053500151124             ENDIF;
053600151124           ENDDO;
053700151124
053800151124         //?Poi cerco il cliente unificante
053900151124           clear TIBS10DS;
054000151124           tibs10ds.D10tle = 'ST';
054100151124           tibs10ds.D10paf = 'P';
054200151124           tibs10ds.D10cod = TAVksc;
054300151124           TIBS10R (TIBS10DS);
054400151124         //?Se ritorna errore vuol dire che non è un codice figlio
054500151124           IF  tibs10ds.D10err <> *blanks;
054600151124           //?quindi cerco come padre
054700151124             clear TIBS10DS;
054800151124             tibs10ds.D10tle = 'ST';
054900151124             tibs10ds.D10paf = 'F';
055000151124             tibs10ds.D10cod = TAVksc;
055100151124             TIBS10R (TIBS10DS);
055200151124           ENDIF;
055300151124
055400151124         //?non ci sono progressivi precedenti
055500151124           IF  not PiuProgressivi;
055600151124             exsr NoProgressivo;
055700151124             exsr Scrivi;
055800151124           ENDIF;
055900151124
056000151124         //?ci sono progressivi precedenti
056100151124           IF  PiuProgressivi;
056200151124             exsr SiProgressivo;
056300151124           ENDIF;
056400151113
056500151117         ENDDO;
056600151113
056700151117       //?Chiusura del cursore
056800151117         exec sql
056900151117         CLOSE TAV;
057000151112
057100151112       ENDSR;
057200151124
057300151124       //--------------------------------------------------------------
057400151124       //?Elaborazione per nessun progressivo precedente.
057500151124       //--------------------------------------------------------------
057600151124       BEGSR NoProgressivo;
057700151201
057800151201       //?Aggancio la tariffa
057900151201         chain (TAVksc:TAVctr:TAVprg) TNTAM01L;
058000151124
058100151124       //?Verifico se il cliente ha avuto almeno una bolla fatturata
058200151124         clear TIBS69DS;
058300151124         clear CLPdus;
058400151124         I69kcp = TAVksc;
058500151124         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
058600151124       //?ci sono state bolle fatturate
058700160122       //?non considero date inferiori a 6 mesi da oggi
058800160122         IF  CLPdus > 0 and CLPdus >= wseimesi6;
058900151124           Fatturato = *on;
059000151124           leavesr;
059100151124         ENDIF;
059200151124
059300151124       //?non ci sono state bolle fatturate
059400151124       //?cerco se ce ne sono state nella famiglia
059500151124         xx = 1;
059600151124         FOR xx by 1 to %elem(tibs10ds.D10skcary);
059700151124           IF  tibs10ds.D10skcary(xx) = 0;
059800151124             leave;
059900151124           ENDIF;
060000151124           clear TIBS69DS;
060100151124           clear CLPdus;
060200151124           I69kcp = tibs10ds.D10skcary(xx);
060300151124           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
060400160122           IF  CLPdus > 0 and CLPdus >= wseimesi6;
060500151124             FattFamiglia = *on;
060600151124             leavesr;
060700151124           ENDIF;
060800151124         ENDFOR;
060900151124
061000151124       ENDSR;
061100151124
061200151124       //--------------------------------------------------------------
061300151124       //?Elaborazione per progressivo precedente trovato.
061400151124       //--------------------------------------------------------------
061500151124       BEGSR SiProgressivo;
061600151124
061700151124       //?Aggancio il file delle tariffe
061800151124         chain (TAVksc:TAVctr:TAVprg) TNTAM01L;
061900151124
062000151124       //?Cerco la fattura
062100151124         exsr Fattura;
062200151124
062300151124       //?Se la tariffa non è stata utilizzata
062400151124         IF  not SiFatt;
062500151124         //?Stessa cosa che viene fatta per nessun progressivo precedente
062600151124           exsr NoProgressivo;
062700151124           exsr Scrivi;
062800151124           leavesr;
062900151124         ENDIF;
063000151124
063100151124       //?Calcolo il periodo da elaborare
063200151124         exsr DataSped;
063300151124
063400151124       //?Leggo le bolle e scrivo file di work
063500151124         exsr Bolle;
063600151124
063700151124       //?Scrivo file leggendo il file di work
063800151124         IF  Analizzate > 0;
063900151124           exsr Scrivi;
064000151124         ENDIF;
064100151124
064200151124       ENDSR;
064300151113
064400151113       //--------------------------------------------------------------
064500151113       //?Cerco l'ultima fattura dove ho utilizzato la tariffa.
064600151113       //--------------------------------------------------------------
064700151113       BEGSR Fattura;
064800151113
064900151124         SiFatt = *off;
065000151209
065100160107         data_ISO = (%date(TAMddt));
065200151209         wanno   = %subdt(data_ISO:*years);
065300151209         wmese   = %subdt(data_ISO:*months);
065400151209         wgiorno = %subdt(data_ISO:*days);
065500151209         wmmgg = wmese * 100;
065600151209         wmmgg += wgiorno;
065700151209
065800160112         setll (TAVksc:TAVctr:wanno:wmmgg) TITAS41C;
065900151113         readpe (TAVksc:TAVctr) TITAS41C;
066000151113         DOW  not %eof(TITAS41C);
066100151117           IF  TASfiv = 998 or TASfiv = 996;
066200160122             IF  TASdft < wseimesi;
066300160122               leavesr;
066400160122             ENDIF;
066500151116             wdspfine = TASdft;
066600151124             SiFatt = *on;
066700151113             leave;
066800151113           ENDIF;
066900151113           readpe (TAVksc:TAVctr) TITAS41C;
067000151113         ENDDO;
067100151113
067200151113       ENDSR;
067300151116
067400151116       //--------------------------------------------------------------
067500151116       //?Calcolo il periodo bolle da elaborare.
067600151116       //--------------------------------------------------------------
067700151116       BEGSR DataSped;
067800151116
067900151116         data_ISO = (%date(wdspfine));
068000151116         data_ISO = data_ISO - %months(1);
068100151116         wdspinizio = %dec(data_ISO);
068200151118         waasi = %dec(%subst(%editc(wdspinizio:'X'):1:4):4:0);
068300151118         wmgsi = %dec(%subst(%editc(wdspinizio:'X'):5:4):4:0);
068400151118         waasf = %dec(%subst(%editc(wdspfine:'X'):1:4):4:0);
068500151118         wmgsf = %dec(%subst(%editc(wdspfine:'X'):5:4):4:0);
068600151116
068700151116       ENDSR;
068800151113
068900151113       //--------------------------------------------------------------
069000151116       //?Leggo le bolle del periodo calcolato.
069100151113       //--------------------------------------------------------------
069200151113       BEGSR Bolle;
069300151117
069400151117         FineBolle = *off;
069500151113
069600151118         setll (TAVksc:TAVctr:waasi:wmgsi) TITAS41C;
069700151117
069800151117         DOW  not FineBolle;
069900151117
070000151117           clear dTAS;
070100151117           clear dTASv;
070200151117           clear rft_svf;
070300151117           clear rft_vaf;
070400151117           clear rft_svr;
070500151117           clear rft_var;
070600160122           TassaAttuale = *off;
070700160122           SaltaBolla = *off;
070800151117
070900151117           reade (TAVksc:TAVctr) TITAS41C;
071000151117           IF  %eof(TITAS41C);
071100151117             leave;
071200151117           ENDIF;
071300151116
071400151116         //?Solo il periodo calcolato
071500151118           IF  TASaas > waasf;
071600151116             leave;
071700151116           ENDIF;
071800151118           IF  TASmgs > wmgsf;
071900151116             leave;
072000151116           ENDIF;
072100151116
072200151116         //?Solo bolle fatturate
072300151116           IF  TASnft = 0;
072400151116             iter;
072500151116           ENDIF;
072600151116         //?Solo bolle fatturate da sede
072700151116           IF  TASfiv <> 998 and TASfiv <> 996;
072800151116             iter;
072900151116           ENDIF;
073000151117
073100151117           Analizzate += 1;
073200151113
073300151119         //?Verifico se spedizione tassata fino all'imponibile
073400151119           w_TAisvt = '£';
073500151119           chain (TASaas:TASlnp:TASnrs:TASnsp:TAStbl:w_TAIsvt) TITAI01L;
073600151119           IF  %found(TITAI01L);
073700151117         //?Se tassata fino all'imponibile non la devo memorizzare
073800151117             TassateImpo += 1;
073900151117             iter;
074000151117           ENDIF;
074100160122
074200160122         //?Carico la sk delle varie
074300160122           exsr CarTA7;
074400160122
074500160122         //?Tolgo dalla tassazione la varia D=Spese invio fattura
074600160122           xx = %lookup('D':sv);
074700160122           IF  xx > 0;
074800160122             clear sv(xx);
074900160122             TASimv -= va(xx);
075000160122             clear va(xx);
075100160122           ENDIF;
075200160122
075300160122         //?Devo tassare la bolla con la stessa tariffa di fatturazione
075400160122         //?per vedere se la bolla è stata variata dopo la fatturazione
075500160122         //?se variata la devo impostare come Tassata fino all'imponibile
075600160122           exsr TassaBolla;
075700160122         //?se la bolla è stata variata dopo la fatturazione leggo bolla sucessiva
075800160122           IF  SaltaBolla;
075900160122             BolleVariate += 1;
076000160122             iter;
076100160122           ENDIF;
076200151117
076300151113         //?Scrivo file di work
076400151113           exsr WrtWrkFile;
076500151113
076600151113         ENDDO;
076700160111
076800160111       //?Chiudo il TNSF20R così da recuperare sempre la cartello giusta
076900160111       //?per ogni clienti/tariffa
077000160111         TAStla = 'C';
077100160111         tnsf20r (kpjba:dTAS:dTASv);
077200151113
077300151113       ENDSR;
077400160122
077500160122       //--------------------------------------------------------------
077600160122       //?Ritasso la bolla.
077700160122       //--------------------------------------------------------------
077800160122       BEGSR TassaBolla;
077900160122
078000160122         SaltaBolla = *off;
078100160122
078200160122       //?Come prima cosa mi salvo l'imponibile IVA e il porto
078300160122         w_TASimv = TASimv;
078400160122         w_TASpor = TASpor;
078500160122
078600160122       //?Mi salvo la dTAS e la dTASv
078700160122         w_dTAS = dTAS;
078800160122         w_dTASv = dTASv;
078900160122         w_TASdiv = TASdiv;
079000160122
079100160122       //?Imposto che devo tassare con la tariffa attuale
079200160122         TassaAttuale = *on;
079300160122
079400160122       //?Tasso
079500160122         exsr Tassaz;
079600160122
079700160122       //?Reimposto le dTAS e la dTASv
079800160122         dTAS = w_DTAS;
079900160122         dTASv = w_dTASv;
080000160122         TASdiv = w_TASdiv;
080100160122
080200160122         TassaAttuale = *off;
080300160122
080400160122       ENDSR;
080500151113
080600151113       //--------------------------------------------------------------
080700151113       //?Scrivo file di work.
080800151113       //--------------------------------------------------------------
080900151113       BEGSR WrtWrkFile;
081000151113
081100160107         clear WFRFT100;
081200151113
081300151113         RFTfiv = TASfiv;
081400151113         RFTnft = TASnft;
081500151113         RFTdft = TASdft;
081600151113         RFTcbo = TASksc;
081700151113         RFTtbo = TASctr;
081800151113         RFTaas = TASaas;
081900151113         RFTmgs = TASmgs;
082000151113         RFTlnp = TASlnp;
082100151113         RFTnrs = TASnrs;
082200151113         RFTnsp = TASnsp;
082300151113         RFTtbl = TAStbl;
082400151113
082500151113       //?Dati tassazione
082600151117         RFTdivf = TASdiv;
082700151117         RFTporf = TASpor;
082800151113
082900151113         xx = 1;
083000151113         FOR xx by 1 to %elem(sv);
083100151113           rft_svf(xx) = sv(xx);
083200151113           rft_vaf(xx) = va(xx);
083300151113         ENDFOR;
083400151117         RFTimvf = TASimv;
083500151113
083600151113       //?Imposto i dati della nuova tassazione
083700160107         RFTkscr = TAVksc;
083800160107         RFTctrr = TAVctr;
083900160107         RFTprgr = TAVprg;
084000151117         RFTdspr = TAMddt;
084100151117
084200151117       //?Ritasso
084300151117         exsr Tassaz;
084400151117         IF  TASerr <> *blanks;
084500151117           leavesr;
084600151117         ENDIF;
084700160107
084800160107       //?Imposto il progressivo usato in tassazione della bolla.
084900160107         RFTpbo = §AR5prgta;
085000160107       //?se non c'è, visto che è un campo nuovo e non abbiamo impostato il regresso
085100160107       //?metto il progressivo della tariffa variata
085200160107         IF  RFTpbo = *blanks;
085300160107           RFTpbo = %editc(TAMprg:'X');
085400160107         ENDIF;
085500151113
085600160107         write WFRFT100;
085700160107         feod WFRFT10F;
085800151113
085900151113       ENDSR;
086000151113
086100151113       //--------------------------------------------------------------
086200151113       //?Carico le varie.
086300151113       //--------------------------------------------------------------
086400151113       BEGSR CarTA7;
086500151113
086600151113       //?Leggo TITA7 solos e ho la terza varia di TITAS > 0
086700151113         IF  TASsv3 <> *blanks;
086800151113           setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
086900151113           IF  %equal(TITA730C);
087000151113             xx = 3;
087100151113             reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
087200151113             DOW  not %eof(TITA730C);
087300151113         //?Carico le varie
087400151113               xx += 1;
087500151113               sv(xx) = TA7svn;
087600151113               va(xx) = TA7van;
087700151113               reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
087800151113             ENDDO;
087900151113           ENDIF;
088000151113         ENDIF;
088100151113
088200151113       ENDSR;
088300151113
088400151113       //--------------------------------------------------------------
088500151113       //?Ritasso le spedizioni con la nuova tariffa.
088600151113       //--------------------------------------------------------------
088700151113       BEGSR Tassaz;
088800151113
088900151113         clear dTASv;
089000151113         clear dTASpes;
089100151113         clear TASpor;
089200151113         clear TASimv;
089300151119
089400151119       //?Cerco in TITAI l'immagine della spedizione prima della tassazione
089500151119         exsr ric_TAI;
089600151113
089700151113       //?Pulisco i campi dell'assicurazione se calcolata in fattura
089800151113         IF  TASfcm = 'F';
089900151113           clear TASias;
090000151113           clear TASvas;
090100151113         ENDIF;
090200151113
090300151117       //?Cerco il contrassegno
090400151117         IF  TASfca <> *blanks;
090500151117           exsr carCSB;
090600151117         ENDIF;
090700151116
090800151117       //?Se bolla monovaria passo solo quella varia
090900151117         xx = %lookup(TAScbo:CBO);
091000151117         IF  xx > 0;
091100151117           TASsva = CBV(xx);
091200151117         ENDIF;
091300151113
091400151117         TAStc1 = TASftc;
091500160122         TASdct = TASdft;
091600160122         TASdsp = TASaas * 10000;
091700160122         TASdsp += TASmgs;
091800151113
091900151117       //?Flag SI/NO DDT
092000151117         IF  TASll1 = 'C' or TASll1 = 'S';
092100151117           TASddt = 'S';
092200151117         ELSE;
092300151117           clear TASddt;
092400151117         ENDIF;
092500151113
092600151117         clear dTAS01;
092700151117         §ASfbr = TASfbr;
092800151117         §AScca = TAScca;
092900151113
093000151117       //?Imposto flag mail al destinatario
093100151117         dTASflo = TASflo;
093200151117         §ASemd = §FLOemd;
093300151118
093400151118       //?Imposto il flag del calcolo della varia Prenotazione Ritiro
093500151118         TASprt = §FLOado;
093600151113
093700151117       //?Imposto se part.consegna con PinCode
093800151117         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
093900151117           §ASpin = 'S';
094000151117         ENDIF;
094100151113
094200160122       //?La data di riferimento la passo solo se sto tassando con la nuova tariffa
094300160122         IF  not TassaAttuale;
094400160122           §ASdrt = %editc(RFTdspr:'X');
094500160122         ENDIF;
094600151113
094700151117         dsTASflo = dTAS01;
094800151113
094900151117       //?Bancali
095000151118         clear dAR5ban;
095100151117         IF  %subst(TASgva:1:1) = 'B';
095200151117           chain (TASaas:TASlnp:TASnrs:TASnsp:'BAN') FIAR531C;
095300151117           IF  %found(FIAR531C);
095400151117             dAR5ban = AR5uni;
095500151117           ENDIF;
095600151117           TASban = §AR5nba + §AR5nb2;
095700151117         ENDIF;
095800151113
095900151117       //?Colli Originali
096000151118         clear dAR5bnb;
096100151117         IF  %subst(TASgva:1:1) = 'O';
096200151117           chain (TASaas:TASlnp:TASnrs:TASnsp:'BNB') FIAR531C;
096300151117           IF  %found(FIAR531C);
096400151117             dAR5bnb = AR5uni;
096500151117           ENDIF;
096600151117           TASncl = §AR5bcor;
096700151117         ENDIF;
096800151125
096900160107       //?Se bolla di reso
097000151125       //?con varia 'N' valorizzata a 88888888 calcolo l'anteporto
097100151125         IF  TASfbr = 'S';
097200151125           xx = %lookup('N':sv);
097300151125           IF  xx > 0 and va(xx) = 88888888;
097400151125             exsr VariaN;
097500151125         //?Se torna errore imposto come non tassata
097600151125             IF  TASerr <> *blanks;
097700151125               NoTassate += 1;
097800151125               leavesr;
097900151125             ENDIF;
098000151125           ENDIF;
098100151125         ENDIF;
098200151125
098300151125       //?Se bolla con varia & e importo a 88888888
098400151125       //?passo al pgm di tassazione il flag per calolare la varia &
098500151125         xx = %lookup('&':sv);
098600151125         IF  xx > 0 and va(xx) = 88888888;
098700151125           TASfcaa = 'S';
098800151125           clear sv(xx);
098900151125           clear va(xx);
099000151125         ENDIF;
099100151118
099200151118       //?Imposto il flag del calcolo della varia Stampa Packing List
099300151118         clear dAR5gen;
099400151118         chain (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
099500151118         IF  %found(FIAR531C);
099600151118           dAR5gen = AR5uni;
099700151118         ENDIF;
099800151118         IF  §AR5als = 'S' or §AR5alx = 'S';
099900151118           TASspl = 'S';
100000151118         ENDIF;
100100151116
100200151117       //?Verifico se devo calcolare l'addebito di lasciato avviso
100300151119         chain (TASaas:TASlnp:TASnrs:TASnsp:'RIC') FNEVB04L;
100400151119         IF  %found(FNEVB04L);
100500151119           TASric = 'S';
100600151119         ENDIF;
100700151124
100800151124       //?verifico se c'è il rcd FAT su FIAR5
100900151124         clear dAR5fat;
101000151124         chain (TASaas:TASlnp:TASnrs:TASnsp:'FAT') FIAR531C;
101100151124         IF  %found(FIAR531C);
101200151124           dAR5fat = AR5uni;
101300151124         ENDIF;
101400151124
101500151124       //?Se c'è rcd FAT devo passare al pgm di tassazione TNSF20R
101600151124       //?il peso e il volume utilizzato nella fatturazione
101700151124         IF  §AR5pktas > 0;
101800151124           clear TASpkc;
101900151124           clear TASncp;
102000151124           TASpkf = §AR5pktas;
102100151124           TASvlf = §AR5vltas;
102200151124         //?RITASSO
102300151124           tnsf20r (kpjba:dTAS:dTASv);
102400151124
102500151124         ELSE;
102600160122       //?Se non c'è rcd FAT e sto tassando con la tariffa attuale
102700160122       //?richiamo il TNSF20R
102800160122           IF  TassaAttuale;
102900160122             tnsf20r (kpjba:dTAS:dTASv);
103000160122           ELSE;
103100151116
103200160122         //?Se non c'è rcd FAT ritasso chiamando TNSF22R e
103300151124         //?valorizzo i dati nella ds DTASPES
103400160122             TASppkb = TASpkb;
103500160122             TASpdtt = RFTdspr;
103600151124         //?RITASSO
103700160122             tnsf22r (kpjba:dTAS:dTASv:dTASpes);
103800160122
103900160122           ENDIF;
104000151124
104100151124         ENDIF;
104200160122
104300160122       //?Se sto tassando con la tariffa attuale
104400160122         IF  TassaAttuale;
104500160122         //?Sono riuscita a tassare e l'imponibile IVA è variato
104600160122         //?quindi è stata variata la bolla dopo la fatturazione
104700160122         //?oppure non sono riuscita a tassare conto la bolla come
104800160122         //?tassata fino all'imponibile
104900160122           IF  TASerr <> *blanks or
105000160122              (TASerr = *blanks and TASimv <> w_TASimv);
105100160122             TassateImpo += 1;
105200160122             SaltaBolla = *on;
105300160122           ENDIF;
105400160122           leavesr;
105500160122         ENDIF;
105600151117
105700151117         IF  TASerr <> *blanks;
105800151117           NoTassate += 1;
105900151117           leavesr;
106000151117         ENDIF;
106100151116
106200151117       //?tassazione
106300151117         RFTdivr = TASdiv;
106400151117         RFTporr = TASpor;
106500151117       //?carico le varie
106600151117         xx = 1;
106700151117         FOR xx by 1 to %elem(sv);
106800151117           rft_svr(xx) = sv(xx);
106900151117           rft_var(xx) = va(xx);
107000151117         ENDFOR;
107100151117         RFTimvr = TASimv;
107200151117
107300151117         Tassate += 1;
107400151113
107500151113       ENDSR;
107600151119
107700151119       //--------------------------------------------------------------
107800151119       //?Recupero importi di tassazione dal file TITAI.
107900151119       //--------------------------------------------------------------
108000151119       BEGSR ric_TAI;
108100151119
108200151119         clear xx;
108300151119
108400151119         setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAI01L;
108500151119         reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAI01L;
108600151119         DOW  not %eof(TITAI01L);
108700151119
108800151119       //?Imposto il porto
108900151119           IF  TAIsvt = ' ';
109000151119             TASpor = TAIvat;
109100151119           ENDIF;
109200151119
109300151119       //?Varie
109400151119           IF  TAIsvt <> ' ' and TAIsvt <> '£';
109500151119             xx += 1;
109600151119             sv(xx) = TAIsvt;
109700151119             va(xx) = TAIvat;
109800151119           ENDIF;
109900151119
110000151119           reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAI01L;
110100151119         ENDDO;
110200151119
110300151119       ENDSR;
110400151113
110500151116       //--------------------------------------------------------------
110600151116       //?Ricerca il Contrassegno.
110700151116       //--------------------------------------------------------------
110800151116       BEGSR carCSB;
110900151116
111000151116         chain (TASaas:TASlnp:TASnrs:TASnsp) TNCSB03L;
111100151116         IF  %found(TNCSB03L) and TAStbl = CSBtbl;
111200151117           TASsta = %editc(CSBsta:'X');
111300151116           TAScas = CSBcas;
111400151116           IF  CSBfus <> *blanks;
111500151116             TAStic = CSBfus;
111600151116           ELSE;
111700151116             TAStic = CSBtpi;
111800151116           ENDIF;
111900151116           TASvca = CSBvca;
112000151116           TAScmb = CSBcmb;
112100151116         ENDIF;
112200151116
112300151116       ENDSR;
112400151125
112500151125       //--------------------------------------------------------------
112600151125       //?Calcolo anteporto varia N.
112700151125       //--------------------------------------------------------------
112800151125       BEGSR VariaN;
112900151125
113000151125       //?salvo i dati di tassazione della bolla
113100160125         wn_dTAS = dTAS;
113200160125         wn_dTASv = dTASv;
113300151125
113400160113         dTAS01 = dsTASflo;
113500160113       //?tolgo flag invio mail al destinatario
113600151125         clear §asemd;
113700160113       //?tolgo flag PinCode
113800160113         clear §aspin;
113900151125         dsTASflo = dTAS01;
114000160113
114100160113       //?tolgo flag per calcolo Diritto di chiamata prenotazione ritiro
114200160113       //?e il flag per calcolo Packing List
114300160113         clear TASprt;
114400160113         clear TASspl;
114500151125
114600151125       //?Richiamo pgm di tassazione senza passare le varie e il porto
114700151125         clear dTASv;
114800151125         clear TASdiv;
114900151125         clear TASpor;
115000151125         clear antepo;
115100151125
115200151125         tnsf20r (kpjba:dTAS:dTASv);
115300151125         IF  TASerr = *blanks;
115400151125           antepo = TASimv;
115500151125         ENDIF;
115600151125
115700151125       //?ripristino i dati di tassazione della bolla
115800160125         dTAS = wn_dTAS;
115900160125         dTASv = wn_dTASv;
116000151125
116100151125       //?valorizzo la varia con i dati dell'anteporto
116200151125         IF  antepo > 0;
116300151125           va(xx) = antepo;
116400151125         ENDIF;
116500151125
116600151125       ENDSR;
116700151113
116800151113       //--------------------------------------------------------------
116900151113       //?Scrivo i file per controllo Nuove Tariffe.
117000151113       //--------------------------------------------------------------
117100151113       BEGSR Scrivi;
117200151117
117300151117         clear TNRST000;
117400151117
117500151117       //?Imposto i dati nel file di testata
117600151117         RSTksc = TAVksc;
117700151117         RSTctr = TAVctr;
117800151117         RSTprg = TAVprg;
117900151209         RSTdte = TAVdav;
118000151123         RSTddt = TAMddt;
118100151117         RSTdspi = wdspinizio;
118200151117         RSTdspf = wdspfine;
118300151124
118400151124       //?Se Tariffa senza altri progressivi precedenti
118500151124         IF  not PiuProgressivi;
118600151124         //?Imposto il flag mancato confronto tariffe
118700151124           SELECT;
118800151124         //?Altre tariffe utilizzate
118900151124           WHEN  Fatturato;
119000151124             RSTmct = 'A';
119100151124         //?Fatturato Famiglia
119200151124           WHEN  FattFamiglia;
119300151125             RSTmct = 'F';
119400151124         //?Nuovo
119500151124           OTHER;
119600151124             RSTmct = 'N';
119700151124           ENDSL;
119800151124         //?Scrivo il file e poi esco dalla routine
119900151124           write TNRST000;
120000151124           leavesr;
120100151124         ENDIF;
120200151124
120300151124       //?Se Tariffa con altri progressivi ma non ho trovato fatture
120400151124         IF  PiuProgressivi and not SiFatt;
120500151124         //?Imposto il flag mancato confronto tariffe
120600151124           SELECT;
120700151124         //?Altre tariffe utilizzate
120800151124           WHEN  Fatturato;
120900151124             RSTmct = 'B';
121000151124         //?Fatturato Famiglia
121100151124           WHEN  FattFamiglia;
121200151125             RSTmct = 'F';
121300151124         //?Nuovo
121400151124           OTHER;
121500151124             RSTmct = 'N';
121600151124           ENDSL;
121700151124         //?Scrivo il file e poi esco dalla routine
121800151124           write TNRST000;
121900151124           leavesr;
122000151124         ENDIF;
122100151117
122200151117         RSTnbela = Analizzate;
122300151117         RSTnbtas = Tassate;
122400151117         RSTnbnta = NoTassate;
122500151117         RSTnbtim = TassateImpo;
122600160122         RSTnbvar = BolleVariate;
122700151117
122800151117         FineWrk = *off;
122900151117
123000151117       //?Leggo il file di work
123100151117         exec sql DECLARE WRK cursor for
123200160107                  SELECT * from WFRFT10F
123300160107                  WHERE RFTkscr = :TAVksc and RFTctrr = :TAVctr and
123400160107                        RFTprgr = :TAVprg;
123500151117
123600151117       //?Apertura del cursore
123700151117         exec sql
123800151117         OPEN WRK;
123900151117
124000151118         IF  sqlcode < 0;
124100151117           exec sql
124200151117           CLOSE WRK;
124300151117           leavesr;
124400151117         ENDIF;
124500151117
124600151117         DOW  not FineWrk;
124700151117           exec sql
124800151118           FETCH NEXT from WRK into :WFRFTDS;
124900151118           IF  sqlcod = 100 or sqlcod < 0;
125000151117             FineWrk = *on;
125100151117             leave;
125200151117           ENDIF;
125300151117
125400151117         //?Sommo il fatturato e il ritassato
125500151118           RSTimvf += wRFTimvf;
125600151118           RSTimvr += wRFTimvr;
125700151117
125800151117         //?Sommo il porto
125900151118           IF  wRFTporf > *zero;
126000151117             wtrc = '.';
126100151118             wimpf = wRFTporf;
126200151118             wimpr = wRFTporr;
126300151124             IF  wimpf <> 0;
126400151124               wnrsvf = 1;
126500151124             ENDIF;
126600151124             IF  wimpr <> 0;
126700151124               wnrsvr = 1;
126800151124             ENDIF;
126900151117             exsr Somma_Varie;
127000151117           ENDIF;
127100151118
127200151118         //?Sommo le varie Fatturate
127300151118           IF  wRFTsv1f <> *blanks;
127400151118             wtrc = wRFTsv1f;
127500151118             wimpf = wRFTva1f;
127600151124             wnrsvf = 1;
127700151118             clear wimpr;
127800151125             clear wnrsvr;
127900151118             exsr Somma_Varie;
128000151118           ENDIF;
128100151118           IF  wRFTsv2f <> *blanks;
128200151118             wtrc = wRFTsv2f;
128300151118             wimpf = wRFTva2f;
128400151124             wnrsvf = 1;
128500151118             clear wimpr;
128600151125             clear wnrsvr;
128700151118             exsr Somma_Varie;
128800151118           ENDIF;
128900151118           IF  wRFTsv3f <> *blanks;
129000151118             wtrc = wRFTsv3f;
129100151118             wimpf = wRFTva3f;
129200151124             wnrsvf = 1;
129300151118             clear wimpr;
129400151125             clear wnrsvr;
129500151118             exsr Somma_Varie;
129600151118           ENDIF;
129700151118           IF  wRFTsv4f <> *blanks;
129800151118             wtrc = wRFTsv4f;
129900151118             wimpf = wRFTva4f;
130000151124             wnrsvf = 1;
130100151118             clear wimpr;
130200151125             clear wnrsvr;
130300151118             exsr Somma_Varie;
130400151118           ENDIF;
130500151118           IF  wRFTsv5f <> *blanks;
130600151118             wtrc = wRFTsv5f;
130700151118             wimpf = wRFTva5f;
130800151124             wnrsvf = 1;
130900151118             clear wimpr;
131000151125             clear wnrsvr;
131100151118             exsr Somma_Varie;
131200151118           ENDIF;
131300151118           IF  wRFTsv6f <> *blanks;
131400151118             wtrc = wRFTsv6f;
131500151118             wimpf = wRFTva6f;
131600151124             wnrsvf = 1;
131700151118             clear wimpr;
131800151125             clear wnrsvr;
131900151118             exsr Somma_Varie;
132000151118           ENDIF;
132100151118           IF  wRFTsv7f <> *blanks;
132200151118             wtrc = wRFTsv7f;
132300151118             wimpf = wRFTva7f;
132400151124             wnrsvf = 1;
132500151118             clear wimpr;
132600151125             clear wnrsvr;
132700151118             exsr Somma_Varie;
132800151118           ENDIF;
132900151118           IF  wRFTsv8f <> *blanks;
133000151118             wtrc = wRFTsv8f;
133100151118             wimpf = wRFTva8f;
133200151124             wnrsvf = 1;
133300151118             clear wimpr;
133400151125             clear wnrsvr;
133500151118             exsr Somma_Varie;
133600151118           ENDIF;
133700151118           IF  wRFTsv9f <> *blanks;
133800151118             wtrc = wRFTsv9f;
133900151118             wimpf = wRFTva9f;
134000151124             wnrsvf = 1;
134100151118             clear wimpr;
134200151125             clear wnrsvr;
134300151118             exsr Somma_Varie;
134400151118           ENDIF;
134500151118           IF  wRFTs10f <> *blanks;
134600151118             wtrc = wRFTs10f;
134700151118             wimpf = wRFTv10f;
134800151124             wnrsvf = 1;
134900151118             clear wimpr;
135000151125             clear wnrsvr;
135100151118             exsr Somma_Varie;
135200151118           ENDIF;
135300151118           IF  wRFTs11f <> *blanks;
135400151118             wtrc = wRFTs11f;
135500151118             wimpf = wRFTv11f;
135600151124             wnrsvf = 1;
135700151118             clear wimpr;
135800151125             clear wnrsvr;
135900151118             exsr Somma_Varie;
136000151118           ENDIF;
136100151118           IF  wRFTs12f <> *blanks;
136200151118             wtrc = wRFTs12f;
136300151118             wimpf = wRFTv12f;
136400151124             wnrsvf = 1;
136500151118             clear wimpr;
136600151125             clear wnrsvr;
136700151118             exsr Somma_Varie;
136800151118           ENDIF;
136900151118           IF  wRFTs13f <> *blanks;
137000151118             wtrc = wRFTs13f;
137100151118             wimpf = wRFTv13f;
137200151124             wnrsvf = 1;
137300151118             clear wimpr;
137400151125             clear wnrsvr;
137500151118             exsr Somma_Varie;
137600151118           ENDIF;
137700151118           IF  wRFTs14f <> *blanks;
137800151118             wtrc = wRFTs14f;
137900151118             wimpf = wRFTv14f;
138000151124             wnrsvf = 1;
138100151118             clear wimpr;
138200151125             clear wnrsvr;
138300151118             exsr Somma_Varie;
138400151118           ENDIF;
138500151118           IF  wRFTs15f <> *blanks;
138600151118             wtrc = wRFTs15f;
138700151118             wimpf = wRFTv15f;
138800151124             wnrsvf = 1;
138900151118             clear wimpr;
139000151125             clear wnrsvr;
139100151118             exsr Somma_Varie;
139200151118           ENDIF;
139300151118           IF  wRFTs16f <> *blanks;
139400151118             wtrc = wRFTs16f;
139500151118             wimpf = wRFTv16f;
139600151124             wnrsvf = 1;
139700151118             clear wimpr;
139800151125             clear wnrsvr;
139900151118             exsr Somma_Varie;
140000151118           ENDIF;
140100151118           IF  wRFTs17f <> *blanks;
140200151118             wtrc = wRFTs17f;
140300151118             wimpf = wRFTv17f;
140400151124             wnrsvf = 1;
140500151118             clear wimpr;
140600151125             clear wnrsvr;
140700151118             exsr Somma_Varie;
140800151118           ENDIF;
140900151118           IF  wRFTs18f <> *blanks;
141000151118             wtrc = wRFTs18f;
141100151118             wimpf = wRFTv18f;
141200151124             wnrsvf = 1;
141300151118             clear wimpr;
141400151125             clear wnrsvr;
141500151118             exsr Somma_Varie;
141600151118           ENDIF;
141700151118           IF  wRFTs19f <> *blanks;
141800151118             wtrc = wRFTs19f;
141900151118             wimpf = wRFTv19f;
142000151124             wnrsvf = 1;
142100151118             clear wimpr;
142200151125             clear wnrsvr;
142300151118             exsr Somma_Varie;
142400151118           ENDIF;
142500151118           IF  wRFTs20f <> *blanks;
142600151118             wtrc = wRFTs20f;
142700151118             wimpf = wRFTv20f;
142800151124             wnrsvf = 1;
142900151118             clear wimpr;
143000151125             clear wnrsvr;
143100151118             exsr Somma_Varie;
143200151118           ENDIF;
143300151118
143400151118         //?Sommo le varie Ritassate
143500151118           IF  wRFTsv1r <> *blanks;
143600151118             wtrc = wRFTsv1r;
143700151118             clear wimpf;
143800151125             clear wnrsvf;
143900151118             wimpr = wRFTva1r;
144000151124             wnrsvr = 1;
144100151118             exsr Somma_Varie;
144200151118           ENDIF;
144300151118           IF  wRFTsv2r <> *blanks;
144400151118             wtrc = wRFTsv2r;
144500151118             clear wimpf;
144600151125             clear wnrsvf;
144700151118             wimpr = wRFTva2r;
144800151124             wnrsvr = 1;
144900151118             exsr Somma_Varie;
145000151118           ENDIF;
145100151118           IF  wRFTsv3r <> *blanks;
145200151118             wtrc = wRFTsv3r;
145300151118             clear wimpf;
145400151125             clear wnrsvf;
145500151118             wimpr = wRFTva3r;
145600151124             wnrsvr = 1;
145700151118             exsr Somma_Varie;
145800151118           ENDIF;
145900151118           IF  wRFTsv4r <> *blanks;
146000151118             wtrc = wRFTsv4r;
146100151118             clear wimpf;
146200151125             clear wnrsvf;
146300151118             wimpr = wRFTva4r;
146400151124             wnrsvr = 1;
146500151118             exsr Somma_Varie;
146600151118           ENDIF;
146700151118           IF  wRFTsv5r <> *blanks;
146800151118             wtrc = wRFTsv5r;
146900151118             clear wimpf;
147000151125             clear wnrsvf;
147100151118             wimpr = wRFTva5r;
147200151124             wnrsvr = 1;
147300151118             exsr Somma_Varie;
147400151118           ENDIF;
147500151118           IF  wRFTsv6r <> *blanks;
147600151118             wtrc = wRFTsv6r;
147700151118             clear wimpf;
147800151125             clear wnrsvf;
147900151118             wimpr = wRFTva6r;
148000151124             wnrsvr = 1;
148100151118             exsr Somma_Varie;
148200151118           ENDIF;
148300151118           IF  wRFTsv7r <> *blanks;
148400151118             wtrc = wRFTsv7r;
148500151118             clear wimpf;
148600151125             clear wnrsvf;
148700151118             wimpr = wRFTva7r;
148800151124             wnrsvr = 1;
148900151118             exsr Somma_Varie;
149000151118           ENDIF;
149100151118           IF  wRFTsv8r <> *blanks;
149200151118             wtrc = wRFTsv8r;
149300151118             clear wimpf;
149400151125             clear wnrsvf;
149500151118             wimpr = wRFTva8r;
149600151124             wnrsvr = 1;
149700151118             exsr Somma_Varie;
149800151118           ENDIF;
149900151118           IF  wRFTsv9r <> *blanks;
150000151118             wtrc = wRFTsv9r;
150100151118             clear wimpf;
150200151125             clear wnrsvf;
150300151118             wimpr = wRFTva9r;
150400151124             wnrsvr = 1;
150500151118             exsr Somma_Varie;
150600151118           ENDIF;
150700151118           IF  wRFTs10r <> *blanks;
150800151118             wtrc = wRFTs10r;
150900151118             clear wimpf;
151000151125             clear wnrsvf;
151100151118             wimpr = wRFTv10r;
151200151124             wnrsvr = 1;
151300151118             exsr Somma_Varie;
151400151118           ENDIF;
151500151118           IF  wRFTs11r <> *blanks;
151600151118             wtrc = wRFTs11r;
151700151118             clear wimpf;
151800151125             clear wnrsvf;
151900151118             wimpr = wRFTv11r;
152000151124             wnrsvr = 1;
152100151118             exsr Somma_Varie;
152200151118           ENDIF;
152300151118           IF  wRFTs12r <> *blanks;
152400151118             wtrc = wRFTs12r;
152500151118             clear wimpf;
152600151125             clear wnrsvf;
152700151118             wimpr = wRFTv12r;
152800151124             wnrsvr = 1;
152900151118             exsr Somma_Varie;
153000151118           ENDIF;
153100151118           IF  wRFTs13r <> *blanks;
153200151118             wtrc = wRFTs13r;
153300151118             clear wimpf;
153400151125             clear wnrsvf;
153500151118             wimpr = wRFTv13r;
153600151124             wnrsvr = 1;
153700151118             exsr Somma_Varie;
153800151118           ENDIF;
153900151118           IF  wRFTs14r <> *blanks;
154000151118             wtrc = wRFTs14r;
154100151118             clear wimpf;
154200151125             clear wnrsvf;
154300151118             wimpr = wRFTv14r;
154400151124             wnrsvr = 1;
154500151118             exsr Somma_Varie;
154600151118           ENDIF;
154700151118           IF  wRFTs15r <> *blanks;
154800151118             wtrc = wRFTs15r;
154900151118             clear wimpf;
155000151125             clear wnrsvf;
155100151118             wimpr = wRFTv15r;
155200151124             wnrsvr = 1;
155300151118             exsr Somma_Varie;
155400151118           ENDIF;
155500151118           IF  wRFTs16r <> *blanks;
155600151118             wtrc = wRFTs16r;
155700151118             clear wimpf;
155800151125             clear wnrsvf;
155900151118             wimpr = wRFTv16r;
156000151124             wnrsvr = 1;
156100151118             exsr Somma_Varie;
156200151118           ENDIF;
156300151118           IF  wRFTs17r <> *blanks;
156400151118             wtrc = wRFTs17r;
156500151118             clear wimpf;
156600151125             clear wnrsvf;
156700151118             wimpr = wRFTv17r;
156800151124             wnrsvr = 1;
156900151118             exsr Somma_Varie;
157000151118           ENDIF;
157100151118           IF  wRFTs18r <> *blanks;
157200151118             wtrc = wRFTs18r;
157300151118             clear wimpf;
157400151125             clear wnrsvf;
157500151118             wimpr = wRFTv18r;
157600151124             wnrsvr = 1;
157700151118             exsr Somma_Varie;
157800151118           ENDIF;
157900151118           IF  wRFTs19r <> *blanks;
158000151118             wtrc = wRFTs19r;
158100151118             clear wimpf;
158200151125             clear wnrsvf;
158300151118             wimpr = wRFTv19r;
158400151124             wnrsvr = 1;
158500151118             exsr Somma_Varie;
158600151118           ENDIF;
158700151118           IF  wRFTs20r <> *blanks;
158800151118             wtrc = wRFTs20r;
158900151118             clear wimpf;
159000151125             clear wnrsvf;
159100151118             wimpr = wRFTv20r;
159200151124             wnrsvr = 1;
159300151118             exsr Somma_Varie;
159400151118           ENDIF;
159500151118
159600151117         ENDDO;
159700160112
159800160113       //?Se arrivo qua e non ho il valore del fatturato
159900160113       //?devo impostare il flag di mancato confronto a 'X'
160000160113       //?questo capita quando ci sono le bolle ma non sono state tassate
160100160113       //?dalla fatturazione quindi non c'è confronto.
160200160113         IF  RSTimvf = 0;
160300160113           RSTmct = 'X';
160400160113         ENDIF;
160500151117
160600151117       //?Scrivo file di testata
160700151117         write TNRST000;
160800151117
160900151117       //?Chiusura del cursore
161000151117         exec sql
161100151117         CLOSE WRK;
161200151113
161300151113       ENDSR;
161400151117
161500151117       //--------------------------------------------------------------
161600151117       //?Sommo le varie nel file di dettaglio.
161700151117       //--------------------------------------------------------------
161800151117       BEGSR Somma_Varie;
161900151117
162000151117         chain (RSTksc:RSTctr:RSTprg:RSTdte:wtrc) TNRSD01L;
162100151117         IF  %found(TNRSD01L);
162200151117             RSDimpf += wimpf;
162300151117             RSDimpr += wimpr;
162400151124             IF  wnrsvf > 0;
162500151124               RSDnrsvf += 1;
162600151124             ENDIF;
162700151124             IF  wnrsvr > 0;
162800151124               RSDnrsvr += 1;
162900151124             ENDIF;
163000151117             update TNRSD000;
163100151117           ELSE;
163200151117             clear TNRSD000;
163300151117             RSDksc = RSTksc;
163400151117             RSDctr = RSTctr;
163500151117             RSDprg = RSTprg;
163600151117             RSDdte = RSTdte;
163700151117             RSDtrc = wtrc;
163800151117             RSDimpf += wimpf;
163900151117             RSDimpr += wimpr;
164000151124             IF  wnrsvf > 0;
164100151124               RSDnrsvf = 1;
164200151124             ENDIF;
164300151124             IF  wnrsvr > 0;
164400151124               RSDnrsvr = 1;
164500151124             ENDIF;
164600151118             write TNRSD000;
164700151117           ENDIF;
164800151117
164900151117       ENDSR;
165000151209
165100151209       //--------------------------------------------------------------
165200151209       //?Aggiorna data ultima elaborazione cambia tariffa.
165300151209       //--------------------------------------------------------------
165400151209       BEGSR AggTab1G;
165500151209
165600151209         clear ds1G;
165700151209         k_TBLcod = '1G';
165800151209         k_TBLkey = '1';
165900151209         chain %kds(K03tabel) TABEL00F;
166000151209         IF  %found(TABEL00F);
166100151209           ds1G = TBLuni;
166200151209           §1Gctu = wDataf;
166300151209           TBLuni = ds1G;
166400151209           update TABEL;
166500151209         ENDIF;
166600151209
166700151209       ENDSR;
166800151112
166900151112       //--------------------------------------------------------------
167000151112       //?Operazioni finali.
167100151112       //--------------------------------------------------------------
167200151112       BEGSR RoutEnd;
167300151112
167400151112         *inLR = *on;
167500151112         return;
167600151112
167700151112       ENDSR;
167800151112
167900151112      /end-free
