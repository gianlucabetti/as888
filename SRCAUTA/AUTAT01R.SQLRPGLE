000100170321      //--------------------------------------------------------------
000200170321      //?AUTAT01R - Crea File Controllo Aumento Tariffe
000300170321      //--------------------------------------------------------------
000400170321     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170321
000600170321      //---------------------------------------------------------------
000700170321      //?Dichiarazione file.
000800170321      //---------------------------------------------------------------
000900170321      // - Eventi
001000170321     fFNEVB04L  if   e           k disk
001100170321
001200170321      // - Tabelle
001300170321     fTABEL00F  if   e           k disk
001400170321
001500170321      // - Tariffe
001600170321     fTNTAM00L  if   e           k disk    rename(TNTAM000:TNTAM00)
001700170321     fTNTAM01L  if   e           k disk
001800170321
001900170321      // - Bolle Sede
002000170321     fTITAS41C  if   e           k disk    rename(TITAS000:TITAS410)
002100170321     f                                     rename(TITAS010:TITAS411)
002200170321     f                                     rename(TITASP00:TITAS41P)
002300170321     fTITA730C  if   e           k disk
002400170321     fFIAR531C  if   e           k disk
002500170321     fTITAI01L  if   e           k disk
002600170321
002700170321      // - Contrassegni
002800170321     fTNCSB03L  if   e           k disk
002900170321
003000170321      // - File di WORK per bolle da ritassare
003100170321     fWFRFT10F  if a e             disk
003200170321
003300170321      // - File Controllo Aumento Tariffe
003400170321     fWFRST00F  o    e             disk
003500170321     fWFRSD01L  uf a e           k disk
003600170321
003700170321      //---------------------------------------------------------------
003800170321      //?Definizione costanti.
003900170321      //---------------------------------------------------------------
004000170321
004100170321      //---------------------------------------------------------------
004200170321      //?Definizione schiere.
004300170321      //---------------------------------------------------------------
004400170321      // - Calcolo tassazione VARIE
004500170321     d DTASV         e ds
004600170321     d  sv                     1     20    dim(20)
004700170321     d  va                    21    140p 3 dim(20)
004800170321
004900170321      // - Scrittura varie fatturare nel file di lavoro
005000170321     d                 ds
005100170321     d  RFTsv1f                1      1
005200170321     d  RFTsv2f                2      2
005300170321     d  RFTsv3f                3      3
005400170321     d  RFTsv4f                4      4
005500170321     d  RFTsv5f                5      5
005600170321     d  RFTsv6f                6      6
005700170321     d  RFTsv7f                7      7
005800170321     d  RFTsv8f                8      8
005900170321     d  RFTsv9f                9      9
006000170321     d  RFTs10f               10     10
006100170321     d  RFTs11f               11     11
006200170321     d  RFTs12f               12     12
006300170321     d  RFTs13f               13     13
006400170321     d  RFTs14f               14     14
006500170321     d  RFTs15f               15     15
006600170321     d  RFTs16f               16     16
006700170321     d  RFTs17f               17     17
006800170321     d  RFTs18f               18     18
006900170321     d  RFTs19f               19     19
007000170321     d  RFTs20f               20     20
007100170321     d  RFT_svf                1     20    dim(20)
007200170321
007300170321     d                 ds
007400170321     d  RFTva1f                1     11  3
007500170321     d  RFTva2f               12     22  3
007600170321     d  RFTva3f               23     33  3
007700170321     d  RFTva4f               34     44  3
007800170321     d  RFTva5f               45     55  3
007900170321     d  RFTva6f               56     66  3
008000170321     d  RFTva7f               67     77  3
008100170321     d  RFTva8f               78     88  3
008200170321     d  RFTva9f               89     99  3
008300170321     d  RFTv10f              100    110  3
008400170321     d  RFTv11f              111    121  3
008500170321     d  RFTv12f              122    132  3
008600170321     d  RFTv13f              133    143  3
008700170321     d  RFTv14f              144    154  3
008800170321     d  RFTv15f              155    165  3
008900170321     d  RFTv16f              166    176  3
009000170321     d  RFTv17f              177    187  3
009100170321     d  RFTv18f              188    198  3
009200170321     d  RFTv19f              199    209  3
009300170321     d  RFTv20f              210    220  3
009400170321     d  RFT_vaf                1    220  3 dim(20)
009500170321
009600170321      // - Scrittura varie ritassate nel file di lavoro
009700170321     d                 ds
009800170321     d  RFTsv1r                1      1
009900170321     d  RFTsv2r                2      2
010000170321     d  RFTsv3r                3      3
010100170321     d  RFTsv4r                4      4
010200170321     d  RFTsv5r                5      5
010300170321     d  RFTsv6r                6      6
010400170321     d  RFTsv7r                7      7
010500170321     d  RFTsv8r                8      8
010600170321     d  RFTsv9r                9      9
010700170321     d  RFTs10r               10     10
010800170321     d  RFTs11r               11     11
010900170321     d  RFTs12r               12     12
011000170321     d  RFTs13r               13     13
011100170321     d  RFTs14r               14     14
011200170321     d  RFTs15r               15     15
011300170321     d  RFTs16r               16     16
011400170321     d  RFTs17r               17     17
011500170321     d  RFTs18r               18     18
011600170321     d  RFTs19r               19     19
011700170321     d  RFTs20r               20     20
011800170321     d  RFT_svr                1     20    dim(20)
011900170321
012000170321     d                 ds
012100170321     d  RFTva1r                1     11  3
012200170321     d  RFTva2r               12     22  3
012300170321     d  RFTva3r               23     33  3
012400170321     d  RFTva4r               34     44  3
012500170321     d  RFTva5r               45     55  3
012600170321     d  RFTva6r               56     66  3
012700170321     d  RFTva7r               67     77  3
012800170321     d  RFTva8r               78     88  3
012900170321     d  RFTva9r               89     99  3
013000170321     d  RFTv10r              100    110  3
013100170321     d  RFTv11r              111    121  3
013200170321     d  RFTv12r              122    132  3
013300170321     d  RFTv13r              133    143  3
013400170321     d  RFTv14r              144    154  3
013500170321     d  RFTv15r              155    165  3
013600170321     d  RFTv16r              166    176  3
013700170321     d  RFTv17r              177    187  3
013800170321     d  RFTv18r              188    198  3
013900170321     d  RFTv19r              199    209  3
014000170321     d  RFTv20r              210    220  3
014100170321     d  RFT_var                1    220  3 dim(20)
014200170321
014300170321      // - Codici bolla monovarie
014400170321     d CBO             s              2    dim(50)
014500170321     d CBV             s              1    dim(50)
014600170321
014700170321      // - Part.consegna con abilitazione PIN Code
014800170321     d skGMA           s              2a   dim(10)
014900170321
015000170321      //---------------------------------------------------------------
015100170321      //?Definizione aree dati.
015200170321      //---------------------------------------------------------------
015300170321      // - Dati utente
015400170321     d §AzUte        e ds                  extname(AZUTE00F)
015500170321     d                                     dtaara
015600170321     d §DatiUte      e ds                  extname(dDatiUte)
015700170321     d                                     dtaara
015800170321
015900170321      //---------------------------------------------------------------
016000170321      //?Definizione strutture dati.
016100170321      //---------------------------------------------------------------
016200170321      // - Parametri ricevuti
016300170321     d KPJBA         e ds
016400170321
016500170321      // - Ricerca Unificante Padre?
016600170321      /copy gaitrasrc/srcprotopi,TIBS10R
016700170321
016800170321      // - Reperimento dati utente
016900170321     d TIBS34DS      e ds
017000170321
017100170321      // - Reperimento dati Anagrafica Clienti
017200170321      /copy gaitrasrc/srcprotopi,TIBS69R
017300170321
017400170321      // - Codici bolla
017500170321     d ds3A          e ds                  inz
017600170321
017700170321      // - Tabella 7R
017800170321     d ds7R          e ds                  inz
017900170321
018000170321      // - DS Flag Operativi TNTAM (TamFlo)
018100170321     d dsTA01        e ds
018200170321
018300170321      // - Rcd GEN file FIAR5
018400170321     d dAR5gen       e ds                  inz
018500170321
018600170321      // - Recupero bancali e colli originali
018700170321     d dAR5ban       e ds                  inz
018800170321     d dAR5bnb       e ds                  inz
018900170321
019000170321      // - DS per TASFLO del TITAS
019100170321     d DTASFLO       e ds
019200170321
019300170321      // - DS Calcolo tassazione VARIE
019400170321     d DTAS          e ds
019500170321     d  dsTASflo     e                     extfld(TASflo)
019600170321
019700170321      // - DS Flag Operativi ds DTAS
019800170321     d dTAS01        e ds
019900170321
020000170321      // - DS per valori tassazione peso desunto
020100170321     d dTASpes       e ds
020200170321
020300170321      // - File Variazioni Tariffe
020400170321     d TITAV00F      e ds                  extname(TITAV00F)
020500170321
020600170321      // - File di Work
020700170321     d WFRFTDS       e ds                  extname(WFRFT10F) prefix(w)
020800170321
020900170321      //---------------------------------------------------------------
021000170321      //?Definizione variabili globali.
021100170321      //---------------------------------------------------------------
021200170321      // - Flags booleani
021300170321     d FattFamiglia    s               n   inz(*off)
021400170321     d Fatturato       s               n   inz(*off)
021500170321     d Fine            s               n   inz(*off)
021600170321     d FineBolle       s               n   inz(*off)
021700170321     d FineWrk         s               n   inz(*off)
021800170321     d PiuProgressivi  s               n   inz(*off)
021900170321     d SaltaBolla      s               n   inz(*off)
022000170321     d SiFatt          s               n   inz(*off)
022100170321     d TassaAttuale    s               n   inz(*off)
022200170321
022300170321      // - Indici di schiera
022400170321     d xx              s              3  0
022500170321     d xy              s              3  0
022600170321
022700170321      // - Campi di comodo data
022800170321     d Data_EUR        s               d   datfmt(*eur)
022900170321     d Data_ISO        s               d   datfmt(*iso)
023000170321     d wData           s              8s 0 inz
023100170321     d wDataAGG        s              8s 0 inz
023200170321     d wDataAUM        s              8s 0 inz
023300170321     d wseimesi        s              8s 0 inz
023400170321     d wseimesi6       s              6s 0 inz
023500170321
023600170321      // - Campi di comodo contatori
023700170321     d Analizzate      s              9s 0 inz
023800170321     d BolleVariate    s              9s 0 inz
023900170321     d NoTassate       s              9s 0 inz
024000170321     d Tassate         s              9s 0 inz
024100170321     d TassateImpo     s              9s 0 inz
024200170321
024300170321      // - Campi di comodo
024400170321     d antepo          s                   like(TASimv)
024500170321     d wanno           s              4s 0
024600170321     d waasi           s              4s 0
024700170321     d waasf           s              4s 0
024800170321     d wdspfine        s              8s 0
024900170321     d wdspinizio      s              8s 0
025000170321     d wfesta          s              1a
025100170321     d wgiorno         s              2s 0
025200170321     d wimpf           s                   like(RSDimpf)
025300170321     d wimpr           s                   like(RSDimpr)
025400170321     d wksc            s                   like(TAVksc)
025500170321     d wmese           s              2s 0
025600170321     d wmgsi           s              4s 0
025700170321     d wmgsf           s              4s 0
025800170321     d wmmgg           s              4s 0
025900170321     d wnrsvf          s                   like(RSDnrsvf)
026000170321     d wnrsvr          s                   like(RSDnrsvr)
026100170321     d woggi           s              8s 0
026200170321     d wpom            s              1a
026300170321     d wprg            s                   like(TAVprg)
026400170321     d wtrc            s                   like(RSDtrc)
026500170321     d wTaTap          s                   like(§taTap)
026600170321     d w_TAIsvt        s                   like(TAIsvt)
026700170321     d w_TASdiv        s                   like(TASdiv)
026800170321     d w_TASimv        s                   like(TASimv)
026900170321     d w_TASpor        s                   like(TASpor)
027000170321     d w_dTAS          s                   like(dTAS)
027100170321     d w_dTASv         s                   like(dTASv)
027200170321     d wn_dTAS         s                   like(dTAS)
027300170321     d wn_dTASv        s                   like(dTASv)
027400170321
027500170321      //---------------------------------------------------------------
027600170321      //?Definizione Procedure usate.
027700170321      //---------------------------------------------------------------
027800170321      // - Tassazione spedizione senza peso desunto
027900170321     d TNSF20R         pr                  extpgm('TNSF20R')
028000170321     d  kpjba                              likeds(kpjba)
028100170321     d  dTAS                               likeds(dTAS)
028200170321     d  dTASv                              likeds(dTASv)
028300170321
028400170321      // - Tassazione spedizione
028500170321     d TNSF22R         pr                  extpgm('TNSF22R')
028600170321     d  kpjba                              likeds(kpjba)
028700170321     d  dTAS                               likeds(dTAS)
028800170321     d  dTASv                              likeds(dTASv)
028900170321     d  dTASpes                            likeds(dTASpes)
029000170321
029100170321      //---------------------------------------------------------------
029200170321      //?Definizione Prototipi.
029300170321      //---------------------------------------------------------------
029400170321      /copy gaitrasrc/srcprotopr,TIBS10R
029500170321      /copy gaitrasrc/srcprotopr,TIBS34R
029600170321      /copy gaitrasrc/srcprotopr,TIBS69R
029700170321
029800170321      //---------------------------------------------------------------
029900170321      //?Definizione key-list.
030000170321      //---------------------------------------------------------------
030100170321      // - File TABEL00F
030200170321     d k03tabel      e ds                  extname(TABEL00F:*key)
030300170321     d                                     prefix(k_)
030400170321
030500170321      //---------------------------------------------------------------
030600170321      //?M A I N - L I N E
030700170321      //---------------------------------------------------------------
030800170321
030900170321     c     *Entry        plist
031000170321     c                   parm                    kpjba
031100170321
031200170321      /free
031300170321
031400170321       //?Operazioni iniziali
031500170321       exsr RoutInz;
031600170321
031700170321       //?Carica bolle monovarie
031800170321       exsr carCBO;
031900170321
032000170321       //?Carica particolarità consegna Pin Code.
032100170321       exsr car7RPinCode;
032200170321
032300170321       //?Pulisco le date eventualmente già presenti sul file
032400170321       exsr PulisciWfrs;
032500170321
032600170321       //?Legge le tariffe aumentate e scrive file di work con bolle tassate a ritassate
032700170321       //?per ogni tariffa aumentata scrive file WFSRT/D
032800170321       exsr Elabora;
032900170321
033000170321       //?Operazioni finali
033100170321       exsr RoutEnd;
033200170321
033300170321       //--------------------------------------------------------------
033400170321       //?Operazioni iniziali.
033500170321       //--------------------------------------------------------------
033600170321       BEGSR RoutInz;
033700170321
033800170321         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
033900170321
034000170321       //?Reperimento dati job
034100170321         exsr DatiJob;
034200170321
034300170321       //?Imposto oggi
034400170321         woggi = %dec(%date());
034500170321
034600170321       //?Imposto oggi - 6 mesi
034700170321         data_ISO = (%date(woggi));
034800170321         data_ISO = data_ISO - %months(6);
034900170321         wseimesi = %dec(data_ISO);
035000170321         wseimesi6 = %dec(%subst(%editc(wseimesi:'X'):3:6):6:0);
035100170321
035200170321       //?Imposto dati di dft
035300170321         k_TBLkut = 1;
035400170321
035500170321       //?Imposto la data Aggiornamento Tariffe
035600170321         wDataAGG = %dec(%subst(kpjbu:1:8):8:0);
035700170321       //?Imposto la data Inizio Aumento Tariffe
035800170321         wDataAUM = %dec(%subst(kpjbu:9:8):8:0);
035900170321
036000170321       ENDSR;
036100170321
036200170321       //--------------------------------------------------------------
036300170321       //?Reperimento Dati del job (Utente/Operativi).
036400170321       //--------------------------------------------------------------
036500170321       BEGSR DatiJob;
036600170321
036700170321         in(E) §AzUte;
036800170321         IF  NOT %error;
036900170321           in(E) §DatiUte;
037000170321         ENDIF;
037100170321         IF  %error or RSut = *blanks;
037200170321           clear TIBS34ds;
037300170321           tibs34r(tibs34ds);
037400170321           in §AzUte;
037500170321           in §DatiUte;
037600170321         ENDIF;
037700170321
037800170321       ENDSR;
037900170321
038000170321       //--------------------------------------------------------------
038100170321       //?Carico sk bolle monovarie.
038200170321       //--------------------------------------------------------------
038300170321       BEGSR carCBO;
038400170321
038500170321       //?Codici bolla monovaria
038600170321         clear xx;
038700170321         clear ds3A;
038800170321         k_TBLcod = '3A';
038900170321         setll (k_TBLkut:k_TBLcod) TABEL00F;
039000170321         reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
039100170321         DOW  not %eof(TABEL00F);
039200170321           IF  TBLkey <> *blanks;
039300170321             ds3A = TBLuni;
039400170321         //?Codici bolla monovaria
039500170321             IF  §3Asva <> *blanks;
039600170321               xy += 1;
039700170321               CBO(xy) = TBLkey;
039800170321               CBV(xy) = §3Asva;
039900170321             ENDIF;
040000170321           ENDIF;
040100170321           reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
040200170321         ENDDO;
040300170321
040400170321       ENDSR;
040500170321
040600170321       //--------------------------------------------------------------
040700170321       //?Carico particolarità consegna con Pin Code.
040800170321       //--------------------------------------------------------------
040900170321       BEGSR Car7RPinCode;
041000170321
041100170321         k_TBLcod = '7R';
041200170321         clear xx;
041300170321         setll (k_TBLkut:k_TBLcod) TABEL00F;
041400170321         reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
041500170321         DOW  not %eof(TABEL00F);
041600170321           IF  TBLflg = *blanks;
041700170321             ds7R = TBLuni;
041800170321             IF  §7Rpincode = 'S';
041900170321               xx += 1;
042000170321               skGMA(xx) = TBLkey;
042100170321             ENDIF;
042200170321           ENDIF;
042300170321           reade(n) (k_TBLkut:k_TBLcod) TABEL00F;
042400170321         ENDDO;
042500170321
042600170321       ENDSR;
042700170321
042800170321       //--------------------------------------------------------------
042900170321       //?Pulisco le date eventualmente già presenti sul file,
043000170321       //--------------------------------------------------------------
043100170321       BEGSR PulisciWfrs;
043200170321
043300170321       //?Cancello i dati già presenti sui file
043400170321         exec sql DELETE from WFRST00F
043500170321                  WHERE RSTdte = :wDataAGG;
043600170321
043700170321       //?Cancello i dati già presenti sui file
043800170321         exec sql DELETE from WFRSD00F
043900170321                  WHERE RSDdte = :wDataAGG;
044000170321
044100170321       ENDSR;
044200170321
044300170321       //--------------------------------------------------------------
044400170321       //?Elaborazione tariffe aumentate
044500170321       //--------------------------------------------------------------
044600170321       BEGSR Elabora;
044700170321
044800170321       //?Estraggo tutte le Tariffe Aumentate
044900170321         exec sql DECLARE TAV cursor for
045000170321                  SELECT * from TITAV00F
045100170321                  WHERE TAVtrd = 'TES' and
045200170321                        TAVcta = 'D' and
045300170321                        TAVdav = :wDataAGG and
045400170321                        TAVatb = ' ' and TAVpru = 'BATCH'
045500170321                  ORDER BY TAVksc, TAVctr, TAVprg;
045600170321
045700170321       //?Apertura del cursore
045800170321         exec sql
045900170321         OPEN TAV;
046000170321
046100170321         IF sqlcode < 0;
046200170321           exec sql
046300170321           CLOSE TAV;
046400170321           leavesr;
046500170321         ENDIF;
046600170321
046700170321         DOW  not Fine;
046800170321           exec sql
046900170321           FETCH NEXT from TAV into :TITAV00F;
047000170321           IF sqlcod = 100 or sqlcod < 0;
047100170321             Fine = *on;
047200170321             leave;
047300170321           ENDIF;
047400170321
047500170321           PiuProgressivi = *off;
047600170321           Fatturato = *off;
047700170321           FattFamiglia = *off;
047800170321           clear wdspinizio;
047900170321           clear wdspfine;
048000170321           clear Analizzate;
048100170321           clear NoTassate;
048200170321           clear Tassate;
048300170321           clear TassateImpo;
048400170321           clear BolleVariate;
048500170321           clear WtaTap;
048600170321
048700170321         //?Come prima cosa verifico se ci sono progressivi precedenti
048800170321           wprg = TAVprg;
048900170321           DOW  wprg > 0;
049000170321             wprg -= 1;
049100170321             chain (TAVksc:TAVctr:wprg) TNTAM00L;
049200170321             IF  %found(TNTAM00L);
049300170321               PiuProgressivi = *on;
049400170321         //?Valorizzo il tipo applicazione VDL del progressivo presedente
049500170321               Dsta01=Tamflo;
049600170321               WtaTap=§taTap;
049700170321               leave;
049800170321             ENDIF;
049900170321           ENDDO;
050000170321
050100170321         //?Poi cerco il cliente unificante
050200170321           clear TIBS10DS;
050300170321           tibs10ds.D10tle = 'ST';
050400170321           tibs10ds.D10paf = 'P';
050500170321           tibs10ds.D10cod = TAVksc;
050600170321           TIBS10R (TIBS10DS);
050700170321         //?Se ritorna errore vuol dire che non è un codice figlio
050800170321           IF  tibs10ds.D10err <> *blanks;
050900170321           //?quindi cerco come padre
051000170321             clear TIBS10DS;
051100170321             tibs10ds.D10tle = 'ST';
051200170321             tibs10ds.D10paf = 'F';
051300170321             tibs10ds.D10cod = TAVksc;
051400170321             TIBS10R (TIBS10DS);
051500170321           ENDIF;
051600170321
051700170321         //?non ci sono progressivi precedenti
051800170321           IF  not PiuProgressivi;
051900170321             exsr NoProgressivo;
052000170321             exsr Scrivi;
052100170321           ENDIF;
052200170321
052300170321         //?ci sono progressivi precedenti
052400170321           IF  PiuProgressivi;
052500170321             exsr SiProgressivo;
052600170321           ENDIF;
052700170321
052800170321         ENDDO;
052900170321
053000170321       //?Chiusura del cursore
053100170321         exec sql
053200170321         CLOSE TAV;
053300170321
053400170321       ENDSR;
053500170321
053600170321       //--------------------------------------------------------------
053700170321       //?Elaborazione per nessun progressivo precedente.
053800170321       //--------------------------------------------------------------
053900170321       BEGSR NoProgressivo;
054000170321
054100170321       //?Aggancio la tariffa
054200170321         chain (TAVksc:TAVctr:TAVprg) TNTAM01L;
054300170321
054400170321       //?Verifico se il cliente ha avuto almeno una bolla fatturata
054500170321         clear TIBS69DS;
054600170321         clear CLPdus;
054700170321         I69kcp = TAVksc;
054800170321         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
054900170321       //?ci sono state bolle fatturate
055000170321       //?non considero date inferiori a 6 mesi da oggi
055100170321         IF  CLPdus > 0 and CLPdus >= wseimesi6;
055200170321           Fatturato = *on;
055300170321           leavesr;
055400170321         ENDIF;
055500170321
055600170321       //?non ci sono state bolle fatturate
055700170321       //?cerco se ce ne sono state nella famiglia
055800170321         xx = 1;
055900170321         FOR xx by 1 to %elem(tibs10ds.D10skcary);
056000170321           IF  tibs10ds.D10skcary(xx) = 0;
056100170321             leave;
056200170321           ENDIF;
056300170321           clear TIBS69DS;
056400170321           clear CLPdus;
056500170321           I69kcp = tibs10ds.D10skcary(xx);
056600170321           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
056700170321           IF  CLPdus > 0 and CLPdus >= wseimesi6;
056800170321             FattFamiglia = *on;
056900170321             leavesr;
057000170321           ENDIF;
057100170321         ENDFOR;
057200170321
057300170321       ENDSR;
057400170321
057500170321       //--------------------------------------------------------------
057600170321       //?Elaborazione per progressivo precedente trovato.
057700170321       //--------------------------------------------------------------
057800170321       BEGSR SiProgressivo;
057900170321
058000170321       //?Aggancio il file delle tariffe
058100170321         chain (TAVksc:TAVctr:TAVprg) TNTAM01L;
058200170321
058300170321       //?Cerco la fattura
058400170321         exsr Fattura;
058500170321
058600170321       //?Se la tariffa non è stata utilizzata
058700170321         IF  not SiFatt;
058800170321         //?Stessa cosa che viene fatta per nessun progressivo precedente
058900170321           exsr NoProgressivo;
059000170321           exsr Scrivi;
059100170321           leavesr;
059200170321         ENDIF;
059300170321
059400170321       //?Calcolo il periodo da elaborare
059500170321         exsr DataSped;
059600170321
059700170321       //?Leggo le bolle e scrivo file di work
059800170321         exsr Bolle;
059900170321
060000170321       //?Scrivo file leggendo il file di work
060100170321         IF  Analizzate > 0;
060200170321           exsr Scrivi;
060300170321         ENDIF;
060400170321
060500170321       ENDSR;
060600170321
060700170321       //--------------------------------------------------------------
060800170321       //?Cerco l'ultima fattura dove ho utilizzato la tariffa.
060900170321       //--------------------------------------------------------------
061000170321       BEGSR Fattura;
061100170321
061200170321         SiFatt = *off;
061300170321
061400170321         data_ISO = (%date(TAMddt));
061500170321         wanno   = %subdt(data_ISO:*years);
061600170321         wmese   = %subdt(data_ISO:*months);
061700170321         wgiorno = %subdt(data_ISO:*days);
061800170321         wmmgg = wmese * 100;
061900170321         wmmgg += wgiorno;
062000170321
062100170321         setll (TAVksc:TAVctr:wanno:wmmgg) TITAS41C;
062200170321         readpe (TAVksc:TAVctr) TITAS41C;
062300170321         DOW  not %eof(TITAS41C);
062400170321           IF  TASfiv = 998 or TASfiv = 996;
062500170321             IF  TASdft < wseimesi;
062600170321               leavesr;
062700170321             ENDIF;
062800170321             wdspfine = TASdft;
062900170321             SiFatt = *on;
063000170321             leave;
063100170321           ENDIF;
063200170321           readpe (TAVksc:TAVctr) TITAS41C;
063300170321         ENDDO;
063400170321
063500170321       ENDSR;
063600170321
063700170321       //--------------------------------------------------------------
063800170321       //?Calcolo il periodo bolle da elaborare.
063900170321       //--------------------------------------------------------------
064000170321       BEGSR DataSped;
064100170321
064200170321         data_ISO = (%date(wdspfine));
064300170321         data_ISO = data_ISO - %months(1);
064400170321         wdspinizio = %dec(data_ISO);
064500170321         waasi = %dec(%subst(%editc(wdspinizio:'X'):1:4):4:0);
064600170321         wmgsi = %dec(%subst(%editc(wdspinizio:'X'):5:4):4:0);
064700170321         waasf = %dec(%subst(%editc(wdspfine:'X'):1:4):4:0);
064800170321         wmgsf = %dec(%subst(%editc(wdspfine:'X'):5:4):4:0);
064900170321
065000170321       ENDSR;
065100170321
065200170321       //--------------------------------------------------------------
065300170321       //?Leggo le bolle del periodo calcolato.
065400170321       //--------------------------------------------------------------
065500170321       BEGSR Bolle;
065600170321
065700170321         FineBolle = *off;
065800170321
065900170321         setll (TAVksc:TAVctr:waasi:wmgsi) TITAS41C;
066000170321
066100170321         DOW  not FineBolle;
066200170321
066300170321           clear dTAS;
066400170321           clear dTASv;
066500170321           clear rft_svf;
066600170321           clear rft_vaf;
066700170321           clear rft_svr;
066800170321           clear rft_var;
066900170321           TassaAttuale = *off;
067000170321           SaltaBolla = *off;
067100170321
067200170321           reade (TAVksc:TAVctr) TITAS41C;
067300170321           IF  %eof(TITAS41C);
067400170321             leave;
067500170321           ENDIF;
067600170321
067700170321         //?Solo il periodo calcolato
067800170321           IF  TASaas > waasf;
067900170321             leave;
068000170321           ENDIF;
068100170321           IF  TASmgs > wmgsf;
068200170321             leave;
068300170321           ENDIF;
068400170321
068500170321         //?Solo bolle fatturate
068600170321           IF  TASnft = 0;
068700170321             iter;
068800170321           ENDIF;
068900170321         //?Solo bolle fatturate da sede
069000170321           IF  TASfiv <> 998 and TASfiv <> 996;
069100170321             iter;
069200170321           ENDIF;
069300170321
069400170321           Analizzate += 1;
069500170321
069600170321         //?Verifico se spedizione tassata fino all'imponibile
069700170321           w_TAisvt = '£';
069800170321           chain (TASaas:TASlnp:TASnrs:TASnsp:TAStbl:w_TAIsvt) TITAI01L;
069900170321           IF  %found(TITAI01L);
070000170321         //?Se tassata fino all'imponibile non la devo memorizzare
070100170321             TassateImpo += 1;
070200170321             iter;
070300170321           ENDIF;
070400170321
070500170321         //?Carico la sk delle varie
070600170321           exsr CarTA7;
070700170321
070800170321         //?Tolgo dalla tassazione la varia D=Spese invio fattura
070900170321           xx = %lookup('D':sv);
071000170321           IF  xx > 0;
071100170321             clear sv(xx);
071200170321             TASimv -= va(xx);
071300170321             clear va(xx);
071400170321           ENDIF;
071500170321
071600170321         //?Devo tassare la bolla con la stessa tariffa di fatturazione
071700170321         //?per vedere se la bolla è stata variata dopo la fatturazione
071800170321         //?se variata la devo impostare come Tassata fino all'imponibile
071900170321           exsr TassaBolla;
072000170321         //?se la bolla è stata variata dopo la fatturazione leggo bolla sucessiva
072100170321           IF  SaltaBolla;
072200170321             BolleVariate += 1;
072300170321             iter;
072400170321           ENDIF;
072500170321
072600170321         //?Scrivo file di work
072700170321           exsr WrtWrkFile;
072800170321
072900170321         ENDDO;
073000170321
073100170321       //?Chiudo il TNSF20R così da recuperare sempre la cartello giusta
073200170321       //?per ogni clienti/tariffa
073300170321         TAStla = 'C';
073400170321         tnsf20r (kpjba:dTAS:dTASv);
073500170321
073600170321       ENDSR;
073700170321
073800170321       //--------------------------------------------------------------
073900170321       //?Ritasso la bolla.
074000170321       //--------------------------------------------------------------
074100170321       BEGSR TassaBolla;
074200170321
074300170321         SaltaBolla = *off;
074400170321
074500170321       //?Imposto che devo tassare con la tariffa attuale
074600170321         TassaAttuale = *on;
074700170321
074800170321       //?Tasso
074900170321         exsr Tassaz;
075000170321
075100170321         TassaAttuale = *off;
075200170321
075300170321       ENDSR;
075400170321
075500170321       //--------------------------------------------------------------
075600170321       //?Scrivo file di work.
075700170321       //--------------------------------------------------------------
075800170321       BEGSR WrtWrkFile;
075900170321
076000170321         clear WFRFT100;
076100170321
076200170321         RFTfiv = TASfiv;
076300170321         RFTnft = TASnft;
076400170321         RFTdft = TASdft;
076500170321         RFTcbo = TASksc;
076600170321         RFTtbo = TASctr;
076700170321         RFTaas = TASaas;
076800170321         RFTmgs = TASmgs;
076900170321         RFTlnp = TASlnp;
077000170321         RFTnrs = TASnrs;
077100170321         RFTnsp = TASnsp;
077200170321         RFTtbl = TAStbl;
077300170321
077400170321       //?Dati tassazione
077500170321         RFTdivf = TASdiv;
077600170321         RFTporf = TASpor;
077700170321
077800170321         xx = 1;
077900170321         FOR xx by 1 to %elem(sv);
078000170321           rft_svf(xx) = sv(xx);
078100170321           rft_vaf(xx) = va(xx);
078200170321         ENDFOR;
078300170321         RFTimvf = TASimv;
078400170321
078500170321       //?Imposto i dati della nuova tassazione
078600170321         RFTkscr = TAVksc;
078700170321         RFTctrr = TAVctr;
078800170321         RFTprgr = TAVprg;
078900170321         RFTdspr = TAMddt;
079000170321
079100170321       //?Ritasso
079200170321         exsr Tassaz;
079300170321         IF  TASerr <> *blanks;
079400170321           leavesr;
079500170321         ENDIF;
079600170321
079700170321         write WFRFT100;
079800170321         feod WFRFT10F;
079900170321
080000170321       ENDSR;
080100170321
080200170321       //--------------------------------------------------------------
080300170321       //?Carico le varie.
080400170321       //--------------------------------------------------------------
080500170321       BEGSR CarTA7;
080600170321
080700170321       //?Leggo TITA7 solos e ho la terza varia di TITAS > 0
080800170321         IF  TASsv3 <> *blanks;
080900170321           setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
081000170321           IF  %equal(TITA730C);
081100170321             xx = 3;
081200170321             reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
081300170321             DOW  not %eof(TITA730C);
081400170321         //?Carico le varie
081500170321               xx += 1;
081600170321               sv(xx) = TA7svn;
081700170321               va(xx) = TA7van;
081800170321               reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
081900170321             ENDDO;
082000170321           ENDIF;
082100170321         ENDIF;
082200170321
082300170321       ENDSR;
082400170321
082500170321       //--------------------------------------------------------------
082600170321       //?Ritasso le spedizioni con la tariffa aumentata.
082700170321       //--------------------------------------------------------------
082800170321       BEGSR Tassaz;
082900170321
083000170321         clear dTASv;
083100170321         clear dTASpes;
083200170321         clear TASpor;
083300170321         clear TASimv;
083400170321
083500170321       //?Valorizzo il numero dei colli bollettati
083600170321         tasnclb = tasncl ;
083700170321
083800170321       //?Cerco in TITAI l'immagine della spedizione prima della tassazione
083900170321         exsr ric_TAI;
084000170321
084100170321       //?Pulisco i campi dell'assicurazione se calcolata in fattura
084200170321         IF  TASfcm = 'F';
084300170321           clear TASias;
084400170321           clear TASvas;
084500170321         ENDIF;
084600170321
084700170321       //?Cerco il contrassegno
084800170321         IF  TASfca <> *blanks;
084900170321           exsr carCSB;
085000170321         ENDIF;
085100170321
085200170321       //?Se bolla monovaria passo solo quella varia
085300170321         xx = %lookup(TAScbo:CBO);
085400170321         IF  xx > 0;
085500170321           TASsva = CBV(xx);
085600170321         ENDIF;
085700170321
085800170321         TAStc1 = TASftc;
085900170321         TASdct = TASdft;
086000170321         TASdsp = TASaas * 10000;
086100170321         TASdsp += TASmgs;
086200170321
086300170321       //?Flag SI/NO DDT
086400170321         IF  TASll1 = 'C' or TASll1 = 'S';
086500170321           TASddt = 'S';
086600170321         ELSE;
086700170321           clear TASddt;
086800170321         ENDIF;
086900170321
087000170321         clear dTAS01;
087100170321         §ASfbr = TASfbr;
087200170321         §AScca = TAScca;
087300170321
087400170321       //?Imposto flag mail al destinatario
087500170321         dTASflo = TASflo;
087600170321         §ASemd = §FLOemd;
087700170321
087800170321       //?Imposto il flag del calcolo della varia Prenotazione Ritiro
087900170321         TASprt = §FLOado;
088000170321
088100170321       //?Imposto se part.consegna con PinCode
088200170321         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
088300170321           §ASpin = 'S';
088400170321         ENDIF;
088500170321
088600170321       //?Se sto tassando con la tariffa attuale passo come data di riferimento
088700170321       //?la data di scadenza della tariffa precedente a quella aumentata
088800170321         IF  TassaAttuale;
088900170321           data_ISO = (%date(wDataAUM));
089000170321           data_ISO = data_ISO - %days(1);
089100170321           wData    = %dec(data_ISO);
089200170321           §ASdrt = %editc(wData:'X');
089300170321         ENDIF;
089400170321       //?Se sto tassando con la tariffa aumentata passo come data di riferimento
089500170321       //?la data decorrenza della tariffa aumentata
089600170321         IF  not TassaAttuale;
089700170321           §ASdrt = %editc(RFTdspr:'X');
089800170321         ENDIF;
089900170321
090000170321         dsTASflo = dTAS01;
090100170321
090200170321       //?Bancali
090300170321         clear dAR5ban;
090400170321         IF  %subst(TASgva:1:1) = 'B';
090500170321           chain (TASaas:TASlnp:TASnrs:TASnsp:'BAN') FIAR531C;
090600170321           IF  %found(FIAR531C);
090700170321             dAR5ban = AR5uni;
090800170321           ENDIF;
090900170321           TASban = §AR5nba + §AR5nb2;
091000170321         ENDIF;
091100170321
091200170321       //?Colli Originali
091300170321         clear dAR5bnb;
091400170321         IF  %subst(TASgva:1:1) = 'O';
091500170321           chain (TASaas:TASlnp:TASnrs:TASnsp:'BNB') FIAR531C;
091600170321           IF  %found(FIAR531C);
091700170321             dAR5bnb = AR5uni;
091800170321           ENDIF;
091900170321           TASncl = §AR5bcor;
092000170321         ENDIF;
092100170321
092200170321       //?Se bolla di reso
092300170321       //?con varia 'N' valorizzata a 88888888 calcolo l'anteporto
092400170321         IF  TASfbr = 'S';
092500170321           xx = %lookup('N':sv);
092600170321           IF  xx > 0 and va(xx) = 88888888;
092700170321             exsr VariaN;
092800170321         //?Se torna errore imposto come non tassata
092900170321             IF  TASerr <> *blanks;
093000170321               NoTassate += 1;
093100170321               leavesr;
093200170321             ENDIF;
093300170321           ENDIF;
093400170321         ENDIF;
093500170321
093600170321       //?Se bolla con varia & e importo a 88888888
093700170321       //?passo al pgm di tassazione il flag per calolare la varia &
093800170321         xx = %lookup('&':sv);
093900170321         IF  xx > 0 and va(xx) = 88888888;
094000170321           TASfcaa = 'S';
094100170321           clear sv(xx);
094200170321           clear va(xx);
094300170321         ENDIF;
094400170321
094500170321       //?Imposto il flag del calcolo della varia Stampa Packing List
094600170321         clear dAR5gen;
094700170321         chain (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
094800170321         IF  %found(FIAR531C);
094900170321           dAR5gen = AR5uni;
095000170321         ENDIF;
095100170321         IF  §AR5als = 'S' or §AR5alx = 'S';
095200170321           TASspl = 'S';
095300170321         ENDIF;
095400170321
095500170321       //?Verifico se devo calcolare l'addebito di lasciato avviso
095600170321         chain (TASaas:TASlnp:TASnrs:TASnsp:'RIC') FNEVB04L;
095700170321         IF  %found(FNEVB04L);
095800170321           TASric = 'S';
095900170321         ENDIF;
096000170321
096100170321         //?TASSO
096200170321         tnsf22r (kpjba:dTAS:dTASv:dTASpes);
096300170321
096400170321         IF  TASerr <> *blanks;
096500170321           NoTassate += 1;
096600170321           leavesr;
096700170321         ENDIF;
096800170321
096900170321       //?tassazione
097000170321         RFTdivr = TASdiv;
097100170321         RFTporr = TASpor;
097200170321       //?carico le varie
097300170321         xx = 1;
097400170321         FOR xx by 1 to %elem(sv);
097500170321           rft_svr(xx) = sv(xx);
097600170321           rft_var(xx) = va(xx);
097700170321         ENDFOR;
097800170321         RFTimvr = TASimv;
097900170321
098000170321         Tassate += 1;
098100170321
098200170321       ENDSR;
098300170321
098400170321       //--------------------------------------------------------------
098500170321       //?Recupero importi di tassazione dal file TITAI.
098600170321       //--------------------------------------------------------------
098700170321       BEGSR ric_TAI;
098800170321
098900170321         clear xx;
099000170321
099100170321         setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAI01L;
099200170321         reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAI01L;
099300170321         DOW  not %eof(TITAI01L);
099400170321
099500170321       //?Imposto il porto
099600170321           IF  TAIsvt = ' ';
099700170321             TASpor = TAIvat;
099800170321           ENDIF;
099900170321
100000170321       //?Varie
100100170321           IF  TAIsvt <> ' ' and TAIsvt <> '£';
100200170321             xx += 1;
100300170321             sv(xx) = TAIsvt;
100400170321             va(xx) = TAIvat;
100500170321           ENDIF;
100600170321
100700170321           reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITAI01L;
100800170321         ENDDO;
100900170321
101000170321       ENDSR;
101100170321
101200170321       //--------------------------------------------------------------
101300170321       //?Ricerca il Contrassegno.
101400170321       //--------------------------------------------------------------
101500170321       BEGSR carCSB;
101600170321
101700170321         chain (TASaas:TASlnp:TASnrs:TASnsp) TNCSB03L;
101800170321         IF  %found(TNCSB03L) and TAStbl = CSBtbl;
101900170321           TASsta = %editc(CSBsta:'X');
102000170321           TAScas = CSBcas;
102100170321           IF  CSBfus <> *blanks;
102200170321             TAStic = CSBfus;
102300170321           ELSE;
102400170321             TAStic = CSBtpi;
102500170321           ENDIF;
102600170321           TASvca = CSBvca;
102700170321           TAScmb = CSBcmb;
102800170321         ENDIF;
102900170321
103000170321       ENDSR;
103100170321
103200170321       //--------------------------------------------------------------
103300170321       //?Calcolo anteporto varia N.
103400170321       //--------------------------------------------------------------
103500170321       BEGSR VariaN;
103600170321
103700170321       //?salvo i dati di tassazione della bolla
103800170321         wn_dTAS = dTAS;
103900170321         wn_dTASv = dTASv;
104000170321
104100170321         dTAS01 = dsTASflo;
104200170321       //?tolgo flag invio mail al destinatario
104300170321         clear §asemd;
104400170321       //?tolgo flag PinCode
104500170321         clear §aspin;
104600170321         dsTASflo = dTAS01;
104700170321
104800170321       //?tolgo flag per calcolo Diritto di chiamata prenotazione ritiro
104900170321       //?e il flag per calcolo Packing List
105000170321         clear TASprt;
105100170321         clear TASspl;
105200170321
105300170321       //?Richiamo pgm di tassazione senza passare le varie e il porto
105400170321         clear dTASv;
105500170321         clear TASdiv;
105600170321         clear TASpor;
105700170321         clear antepo;
105800170321
105900170321         tnsf20r (kpjba:dTAS:dTASv);
106000170321         IF  TASerr = *blanks;
106100170321           antepo = TASimv;
106200170321         ENDIF;
106300170321
106400170321       //?ripristino i dati di tassazione della bolla
106500170321         dTAS = wn_dTAS;
106600170321         dTASv = wn_dTASv;
106700170321
106800170321       //?valorizzo la varia con i dati dell'anteporto
106900170321         IF  antepo > 0;
107000170321           va(xx) = antepo;
107100170321         ENDIF;
107200170321
107300170321       ENDSR;
107400170321
107500170321       //--------------------------------------------------------------
107600170321       //?Scrivo i file per controllo Aumento Tariffe.
107700170321       //--------------------------------------------------------------
107800170321       BEGSR Scrivi;
107900170321
108000170321         clear WFRST000;
108100170321
108200170321       //?Imposto i dati nel file di testata
108300170321         RSTksc = TAVksc;
108400170321         RSTctr = TAVctr;
108500170321         RSTprg = TAVprg;
108600170321         RSTdte = TAVdav;
108700170321         RSTddt = TAMddt;
108800170321         RSTdspi = wdspinizio;
108900170321         RSTdspf = wdspfine;
109000170321
109100170321       //?Imposto il tipo applicazione VDL del nuovo progressivo
109200170321         dsta01=tamflo;
109300170321         RSTTapVN= §taTap;
109400170321       //?Imposto il tipo applicazione VDL della tariffa precedente
109500170321         RSTTapVP = WtaTap ;
109600170321
109700170321       //?Se Tariffa senza altri progressivi precedenti
109800170321         IF  not PiuProgressivi;
109900170321         //?Imposto il flag mancato confronto tariffe
110000170321           SELECT;
110100170321         //?Altre tariffe utilizzate
110200170321           WHEN  Fatturato;
110300170321             RSTmct = 'A';
110400170321         //?Fatturato Famiglia
110500170321           WHEN  FattFamiglia;
110600170321             RSTmct = 'F';
110700170321         //?Nuovo
110800170321           OTHER;
110900170321             RSTmct = 'N';
111000170321           ENDSL;
111100170321         //?Scrivo il file e poi esco dalla routine
111200170321           write WFRST000;
111300170321           leavesr;
111400170321         ENDIF;
111500170321
111600170321       //?Se Tariffa con altri progressivi ma non ho trovato fatture
111700170321         IF  PiuProgressivi and not SiFatt;
111800170321         //?Imposto il flag mancato confronto tariffe
111900170321           SELECT;
112000170321         //?Altre tariffe utilizzate
112100170321           WHEN  Fatturato;
112200170321             RSTmct = 'B';
112300170321         //?Fatturato Famiglia
112400170321           WHEN  FattFamiglia;
112500170321             RSTmct = 'F';
112600170321         //?Nuovo
112700170321           OTHER;
112800170321             RSTmct = 'N';
112900170321           ENDSL;
113000170321         //?Scrivo il file e poi esco dalla routine
113100170321           write WFRST000;
113200170321           leavesr;
113300170321         ENDIF;
113400170321
113500170321         RSTnbela = Analizzate;
113600170321         RSTnbtas = Tassate;
113700170321         RSTnbnta = NoTassate;
113800170321         RSTnbtim = TassateImpo;
113900170321         RSTnbvar = BolleVariate;
114000170321
114100170321         FineWrk = *off;
114200170321
114300170321       //?Leggo il file di work
114400170321         exec sql DECLARE WRK cursor for
114500170321                  SELECT * from WFRFT10F
114600170321                  WHERE RFTkscr = :TAVksc and RFTctrr = :TAVctr and
114700170321                        RFTprgr = :TAVprg;
114800170321
114900170321       //?Apertura del cursore
115000170321         exec sql
115100170321         OPEN WRK;
115200170321
115300170321         IF  sqlcode < 0;
115400170321           exec sql
115500170321           CLOSE WRK;
115600170321           leavesr;
115700170321         ENDIF;
115800170321
115900170321         DOW  not FineWrk;
116000170321           exec sql
116100170321           FETCH NEXT from WRK into :WFRFTDS;
116200170321           IF  sqlcod = 100 or sqlcod < 0;
116300170321             FineWrk = *on;
116400170321             leave;
116500170321           ENDIF;
116600170321
116700170321         //?Sommo il fatturato e il ritassato
116800170321           RSTimvf += wRFTimvf;
116900170321           RSTimvr += wRFTimvr;
117000170321
117100170321         //?Sommo il porto
117200170321           IF  wRFTporf > *zero;
117300170321             wtrc = '.';
117400170321             wimpf = wRFTporf;
117500170321             wimpr = wRFTporr;
117600170321             IF  wimpf <> 0;
117700170321               wnrsvf = 1;
117800170321             ENDIF;
117900170321             IF  wimpr <> 0;
118000170321               wnrsvr = 1;
118100170321             ENDIF;
118200170321             exsr Somma_Varie;
118300170321           ENDIF;
118400170321
118500170321         //?Sommo le varie Fatturate
118600170321           IF  wRFTsv1f <> *blanks;
118700170321             wtrc = wRFTsv1f;
118800170321             wimpf = wRFTva1f;
118900170321             wnrsvf = 1;
119000170321             clear wimpr;
119100170321             clear wnrsvr;
119200170321             exsr Somma_Varie;
119300170321           ENDIF;
119400170321           IF  wRFTsv2f <> *blanks;
119500170321             wtrc = wRFTsv2f;
119600170321             wimpf = wRFTva2f;
119700170321             wnrsvf = 1;
119800170321             clear wimpr;
119900170321             clear wnrsvr;
120000170321             exsr Somma_Varie;
120100170321           ENDIF;
120200170321           IF  wRFTsv3f <> *blanks;
120300170321             wtrc = wRFTsv3f;
120400170321             wimpf = wRFTva3f;
120500170321             wnrsvf = 1;
120600170321             clear wimpr;
120700170321             clear wnrsvr;
120800170321             exsr Somma_Varie;
120900170321           ENDIF;
121000170321           IF  wRFTsv4f <> *blanks;
121100170321             wtrc = wRFTsv4f;
121200170321             wimpf = wRFTva4f;
121300170321             wnrsvf = 1;
121400170321             clear wimpr;
121500170321             clear wnrsvr;
121600170321             exsr Somma_Varie;
121700170321           ENDIF;
121800170321           IF  wRFTsv5f <> *blanks;
121900170321             wtrc = wRFTsv5f;
122000170321             wimpf = wRFTva5f;
122100170321             wnrsvf = 1;
122200170321             clear wimpr;
122300170321             clear wnrsvr;
122400170321             exsr Somma_Varie;
122500170321           ENDIF;
122600170321           IF  wRFTsv6f <> *blanks;
122700170321             wtrc = wRFTsv6f;
122800170321             wimpf = wRFTva6f;
122900170321             wnrsvf = 1;
123000170321             clear wimpr;
123100170321             clear wnrsvr;
123200170321             exsr Somma_Varie;
123300170321           ENDIF;
123400170321           IF  wRFTsv7f <> *blanks;
123500170321             wtrc = wRFTsv7f;
123600170321             wimpf = wRFTva7f;
123700170321             wnrsvf = 1;
123800170321             clear wimpr;
123900170321             clear wnrsvr;
124000170321             exsr Somma_Varie;
124100170321           ENDIF;
124200170321           IF  wRFTsv8f <> *blanks;
124300170321             wtrc = wRFTsv8f;
124400170321             wimpf = wRFTva8f;
124500170321             wnrsvf = 1;
124600170321             clear wimpr;
124700170321             clear wnrsvr;
124800170321             exsr Somma_Varie;
124900170321           ENDIF;
125000170321           IF  wRFTsv9f <> *blanks;
125100170321             wtrc = wRFTsv9f;
125200170321             wimpf = wRFTva9f;
125300170321             wnrsvf = 1;
125400170321             clear wimpr;
125500170321             clear wnrsvr;
125600170321             exsr Somma_Varie;
125700170321           ENDIF;
125800170321           IF  wRFTs10f <> *blanks;
125900170321             wtrc = wRFTs10f;
126000170321             wimpf = wRFTv10f;
126100170321             wnrsvf = 1;
126200170321             clear wimpr;
126300170321             clear wnrsvr;
126400170321             exsr Somma_Varie;
126500170321           ENDIF;
126600170321           IF  wRFTs11f <> *blanks;
126700170321             wtrc = wRFTs11f;
126800170321             wimpf = wRFTv11f;
126900170321             wnrsvf = 1;
127000170321             clear wimpr;
127100170321             clear wnrsvr;
127200170321             exsr Somma_Varie;
127300170321           ENDIF;
127400170321           IF  wRFTs12f <> *blanks;
127500170321             wtrc = wRFTs12f;
127600170321             wimpf = wRFTv12f;
127700170321             wnrsvf = 1;
127800170321             clear wimpr;
127900170321             clear wnrsvr;
128000170321             exsr Somma_Varie;
128100170321           ENDIF;
128200170321           IF  wRFTs13f <> *blanks;
128300170321             wtrc = wRFTs13f;
128400170321             wimpf = wRFTv13f;
128500170321             wnrsvf = 1;
128600170321             clear wimpr;
128700170321             clear wnrsvr;
128800170321             exsr Somma_Varie;
128900170321           ENDIF;
129000170321           IF  wRFTs14f <> *blanks;
129100170321             wtrc = wRFTs14f;
129200170321             wimpf = wRFTv14f;
129300170321             wnrsvf = 1;
129400170321             clear wimpr;
129500170321             clear wnrsvr;
129600170321             exsr Somma_Varie;
129700170321           ENDIF;
129800170321           IF  wRFTs15f <> *blanks;
129900170321             wtrc = wRFTs15f;
130000170321             wimpf = wRFTv15f;
130100170321             wnrsvf = 1;
130200170321             clear wimpr;
130300170321             clear wnrsvr;
130400170321             exsr Somma_Varie;
130500170321           ENDIF;
130600170321           IF  wRFTs16f <> *blanks;
130700170321             wtrc = wRFTs16f;
130800170321             wimpf = wRFTv16f;
130900170321             wnrsvf = 1;
131000170321             clear wimpr;
131100170321             clear wnrsvr;
131200170321             exsr Somma_Varie;
131300170321           ENDIF;
131400170321           IF  wRFTs17f <> *blanks;
131500170321             wtrc = wRFTs17f;
131600170321             wimpf = wRFTv17f;
131700170321             wnrsvf = 1;
131800170321             clear wimpr;
131900170321             clear wnrsvr;
132000170321             exsr Somma_Varie;
132100170321           ENDIF;
132200170321           IF  wRFTs18f <> *blanks;
132300170321             wtrc = wRFTs18f;
132400170321             wimpf = wRFTv18f;
132500170321             wnrsvf = 1;
132600170321             clear wimpr;
132700170321             clear wnrsvr;
132800170321             exsr Somma_Varie;
132900170321           ENDIF;
133000170321           IF  wRFTs19f <> *blanks;
133100170321             wtrc = wRFTs19f;
133200170321             wimpf = wRFTv19f;
133300170321             wnrsvf = 1;
133400170321             clear wimpr;
133500170321             clear wnrsvr;
133600170321             exsr Somma_Varie;
133700170321           ENDIF;
133800170321           IF  wRFTs20f <> *blanks;
133900170321             wtrc = wRFTs20f;
134000170321             wimpf = wRFTv20f;
134100170321             wnrsvf = 1;
134200170321             clear wimpr;
134300170321             clear wnrsvr;
134400170321             exsr Somma_Varie;
134500170321           ENDIF;
134600170321
134700170321         //?Sommo le varie Ritassate
134800170321           IF  wRFTsv1r <> *blanks;
134900170321             wtrc = wRFTsv1r;
135000170321             clear wimpf;
135100170321             clear wnrsvf;
135200170321             wimpr = wRFTva1r;
135300170321             wnrsvr = 1;
135400170321             exsr Somma_Varie;
135500170321           ENDIF;
135600170321           IF  wRFTsv2r <> *blanks;
135700170321             wtrc = wRFTsv2r;
135800170321             clear wimpf;
135900170321             clear wnrsvf;
136000170321             wimpr = wRFTva2r;
136100170321             wnrsvr = 1;
136200170321             exsr Somma_Varie;
136300170321           ENDIF;
136400170321           IF  wRFTsv3r <> *blanks;
136500170321             wtrc = wRFTsv3r;
136600170321             clear wimpf;
136700170321             clear wnrsvf;
136800170321             wimpr = wRFTva3r;
136900170321             wnrsvr = 1;
137000170321             exsr Somma_Varie;
137100170321           ENDIF;
137200170321           IF  wRFTsv4r <> *blanks;
137300170321             wtrc = wRFTsv4r;
137400170321             clear wimpf;
137500170321             clear wnrsvf;
137600170321             wimpr = wRFTva4r;
137700170321             wnrsvr = 1;
137800170321             exsr Somma_Varie;
137900170321           ENDIF;
138000170321           IF  wRFTsv5r <> *blanks;
138100170321             wtrc = wRFTsv5r;
138200170321             clear wimpf;
138300170321             clear wnrsvf;
138400170321             wimpr = wRFTva5r;
138500170321             wnrsvr = 1;
138600170321             exsr Somma_Varie;
138700170321           ENDIF;
138800170321           IF  wRFTsv6r <> *blanks;
138900170321             wtrc = wRFTsv6r;
139000170321             clear wimpf;
139100170321             clear wnrsvf;
139200170321             wimpr = wRFTva6r;
139300170321             wnrsvr = 1;
139400170321             exsr Somma_Varie;
139500170321           ENDIF;
139600170321           IF  wRFTsv7r <> *blanks;
139700170321             wtrc = wRFTsv7r;
139800170321             clear wimpf;
139900170321             clear wnrsvf;
140000170321             wimpr = wRFTva7r;
140100170321             wnrsvr = 1;
140200170321             exsr Somma_Varie;
140300170321           ENDIF;
140400170321           IF  wRFTsv8r <> *blanks;
140500170321             wtrc = wRFTsv8r;
140600170321             clear wimpf;
140700170321             clear wnrsvf;
140800170321             wimpr = wRFTva8r;
140900170321             wnrsvr = 1;
141000170321             exsr Somma_Varie;
141100170321           ENDIF;
141200170321           IF  wRFTsv9r <> *blanks;
141300170321             wtrc = wRFTsv9r;
141400170321             clear wimpf;
141500170321             clear wnrsvf;
141600170321             wimpr = wRFTva9r;
141700170321             wnrsvr = 1;
141800170321             exsr Somma_Varie;
141900170321           ENDIF;
142000170321           IF  wRFTs10r <> *blanks;
142100170321             wtrc = wRFTs10r;
142200170321             clear wimpf;
142300170321             clear wnrsvf;
142400170321             wimpr = wRFTv10r;
142500170321             wnrsvr = 1;
142600170321             exsr Somma_Varie;
142700170321           ENDIF;
142800170321           IF  wRFTs11r <> *blanks;
142900170321             wtrc = wRFTs11r;
143000170321             clear wimpf;
143100170321             clear wnrsvf;
143200170321             wimpr = wRFTv11r;
143300170321             wnrsvr = 1;
143400170321             exsr Somma_Varie;
143500170321           ENDIF;
143600170321           IF  wRFTs12r <> *blanks;
143700170321             wtrc = wRFTs12r;
143800170321             clear wimpf;
143900170321             clear wnrsvf;
144000170321             wimpr = wRFTv12r;
144100170321             wnrsvr = 1;
144200170321             exsr Somma_Varie;
144300170321           ENDIF;
144400170321           IF  wRFTs13r <> *blanks;
144500170321             wtrc = wRFTs13r;
144600170321             clear wimpf;
144700170321             clear wnrsvf;
144800170321             wimpr = wRFTv13r;
144900170321             wnrsvr = 1;
145000170321             exsr Somma_Varie;
145100170321           ENDIF;
145200170321           IF  wRFTs14r <> *blanks;
145300170321             wtrc = wRFTs14r;
145400170321             clear wimpf;
145500170321             clear wnrsvf;
145600170321             wimpr = wRFTv14r;
145700170321             wnrsvr = 1;
145800170321             exsr Somma_Varie;
145900170321           ENDIF;
146000170321           IF  wRFTs15r <> *blanks;
146100170321             wtrc = wRFTs15r;
146200170321             clear wimpf;
146300170321             clear wnrsvf;
146400170321             wimpr = wRFTv15r;
146500170321             wnrsvr = 1;
146600170321             exsr Somma_Varie;
146700170321           ENDIF;
146800170321           IF  wRFTs16r <> *blanks;
146900170321             wtrc = wRFTs16r;
147000170321             clear wimpf;
147100170321             clear wnrsvf;
147200170321             wimpr = wRFTv16r;
147300170321             wnrsvr = 1;
147400170321             exsr Somma_Varie;
147500170321           ENDIF;
147600170321           IF  wRFTs17r <> *blanks;
147700170321             wtrc = wRFTs17r;
147800170321             clear wimpf;
147900170321             clear wnrsvf;
148000170321             wimpr = wRFTv17r;
148100170321             wnrsvr = 1;
148200170321             exsr Somma_Varie;
148300170321           ENDIF;
148400170321           IF  wRFTs18r <> *blanks;
148500170321             wtrc = wRFTs18r;
148600170321             clear wimpf;
148700170321             clear wnrsvf;
148800170321             wimpr = wRFTv18r;
148900170321             wnrsvr = 1;
149000170321             exsr Somma_Varie;
149100170321           ENDIF;
149200170321           IF  wRFTs19r <> *blanks;
149300170321             wtrc = wRFTs19r;
149400170321             clear wimpf;
149500170321             clear wnrsvf;
149600170321             wimpr = wRFTv19r;
149700170321             wnrsvr = 1;
149800170321             exsr Somma_Varie;
149900170321           ENDIF;
150000170321           IF  wRFTs20r <> *blanks;
150100170321             wtrc = wRFTs20r;
150200170321             clear wimpf;
150300170321             clear wnrsvf;
150400170321             wimpr = wRFTv20r;
150500170321             wnrsvr = 1;
150600170321             exsr Somma_Varie;
150700170321           ENDIF;
150800170321
150900170321         ENDDO;
151000170321
151100170321       //?Se arrivo qua e non ho il valore del fatturato
151200170321       //?devo impostare il flag di mancato confronto a 'X'
151300170321       //?questo capita quando ci sono le bolle ma non sono state tassate
151400170321       //?dalla fatturazione quindi non c'è confronto.
151500170321         IF  RSTimvf = 0;
151600170321           RSTmct = 'X';
151700170321         ENDIF;
151800170321
151900170321       //?Scrivo file di testata
152000170321         write WFRST000;
152100170321
152200170321       //?Chiusura del cursore
152300170321         exec sql
152400170321         CLOSE WRK;
152500170321
152600170321       ENDSR;
152700170321
152800170321       //--------------------------------------------------------------
152900170321       //?Sommo le varie nel file di dettaglio.
153000170321       //--------------------------------------------------------------
153100170321       BEGSR Somma_Varie;
153200170321
153300170321         chain (RSTksc:RSTctr:RSTprg:RSTdte:wtrc) WFRSD01L;
153400170321         IF  %found(WFRSD01L);
153500170321             RSDimpf += wimpf;
153600170321             RSDimpr += wimpr;
153700170321             IF  wnrsvf > 0;
153800170321               RSDnrsvf += 1;
153900170321             ENDIF;
154000170321             IF  wnrsvr > 0;
154100170321               RSDnrsvr += 1;
154200170321             ENDIF;
154300170321             update WFRSD000;
154400170321           ELSE;
154500170321             clear WFRSD000;
154600170321             RSDksc = RSTksc;
154700170321             RSDctr = RSTctr;
154800170321             RSDprg = RSTprg;
154900170321             RSDdte = RSTdte;
155000170321             RSDtrc = wtrc;
155100170321             RSDimpf += wimpf;
155200170321             RSDimpr += wimpr;
155300170321             IF  wnrsvf > 0;
155400170321               RSDnrsvf = 1;
155500170321             ENDIF;
155600170321             IF  wnrsvr > 0;
155700170321               RSDnrsvr = 1;
155800170321             ENDIF;
155900170321             write WFRSD000;
156000170321           ENDIF;
156100170321
156200170321       ENDSR;
156300170321
156400170321       //--------------------------------------------------------------
156500170321       //?Operazioni finali.
156600170321       //--------------------------------------------------------------
156700170321       BEGSR RoutEnd;
156800170321
156900170321         *inLR = *on;
157000170321         return;
157100170321
157200170321       ENDSR;
157300170321
157400170321      /end-free
