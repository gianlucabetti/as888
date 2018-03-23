000100101217       //==============================================================
000200080606       //?TEST - STAMPA LDV SU STAMPANTE LASER IN MODELLO UNICO A4
000300101217       //==============================================================
000400101217
000500101217       //--------------------------------------------------------------
000600101217       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700101217       //--------------------------------------------------------------
000800101217
000900101217     /*END
001000101217
001100101217       //--------------------------------------------------------------
001200101217       //?Specifiche di controllo.                                     ?
001300101217       //--------------------------------------------------------------
001400080424
001500080424     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600100914     h dftactgrp(*no)
001700080424
001800080424       //--------------------------------------------------------------
001900080424       //?Dichiarazione file.
002000080424       //--------------------------------------------------------------
002100080709
002200101217       // -?Archivio "bolle partenze - testate"?
002300100218     fFNBLP01L  if   e           k disk    usropn
002400080424
002500101217       // -?Archivio "bolle arrivo - testate"?
002600100218     fFNARB01L  if   e           k disk    usropn
002700080428
002800101217       // -?Video?
002900080606     fFNLSB6TSTDcf   e             workstn
003000080428     f                                     indds(IndDspF)
003100080428     f                                     infds(InfDspF)
003200080424
003300080424       //--------------------------------------------------------------
003400080424       //?Definizione costanti.
003500080424       //--------------------------------------------------------------
003600080424
003700101217       // -?Tasti funzionali a video?
003800080428     d c_F01           c                   const(x'31')
003900080428     d c_F02           c                   const(x'32')
004000080428     d c_F03           c                   const(x'33')
004100080429     d c_F04           c                   const(x'34')
004200080428     d c_F05           c                   const(x'35')
004300080428     d c_F06           c                   const(x'36')
004400080428     d c_F07           c                   const(x'37')
004500080428     d c_F08           c                   const(x'38')
004600080428     d c_F09           c                   const(x'39')
004700080428     d c_F10           c                   const(x'3A')
004800080429     d c_F11           c                   const(x'3B')
004900080428     d c_F12           c                   const(x'3C')
005000080428     d c_F13           c                   const(x'B1')
005100080428     d c_F14           c                   const(x'B2')
005200080428     d c_F15           c                   const(x'B3')
005300080428     d c_F16           c                   const(x'B4')
005400080428     d c_F17           c                   const(x'B5')
005500080428     d c_F18           c                   const(x'B6')
005600080428     d c_F19           c                   const(x'B7')
005700080428     d c_F20           c                   const(x'B8')
005800080428     d c_F21           c                   const(x'B9')
005900080428     d c_F22           c                   const(x'BA')
006000080428     d c_F23           c                   const(x'BB')
006100080428     d c_F24           c                   const(x'BC')
006200080428     d c_Enter         c                   const(x'F1')
006300080428     d c_RollDown      c                   const(x'F4')
006400080428     d c_RollUp        c                   const(x'F5')
006500080424
006600080424       //--------------------------------------------------------------
006700080424       //?Definizione schiere.
006800080424       //--------------------------------------------------------------
006900080424
007000101217       // -?Messaggi di errore?
007100101217     d $Msg            s             78    dim( 4) ctdata perrcd(1)
007200080424
007300080424       //--------------------------------------------------------------
007400080424       //?Definizione aree dati.
007500080424       //--------------------------------------------------------------
007600080424
007700101217       // -?Dati utente?
007800080424     d §AzUte        e ds                  extname(AZUTE00F)
007900080424     d                                     dtaara
008000080424     d §DatiUte      e ds                  extname(dDatiUte)
008100080424     d                                     dtaara
008200080424
008300080424       //--------------------------------------------------------------
008400080424       //?Definizione strutture dati.
008500080424       //--------------------------------------------------------------
008600080424
008700101217       // -?Status ds?
008800080424     d Status         sds
008900080424     d  SDSpgm           *proc
009000080424     d  SDSprm           *parms
009100080424     d  SDSusr               254    263
009200080428
009300101217       // -?InfDS?
009400080428     d InfDspF         ds
009500080428     d  dsp_aid              369    369a
009600080428
009700101217       // -?Indicatori su DspF?
009800080428     d IndDspF         ds
009900101217         // -?Emissione messaggio di errore?
010000101217     d   ErrMessage                    n   overlay(IndDspF : 28)
010100101217         // -?Posizionamento cursore & Segnalazione errore?
010200101217     d   PosCurSped                    n   overlay(IndDspF : 50)
010300101217     d   PosCurPGM                     n   overlay(IndDspF : 51)
010400101217         // -?Riemissione videata?
010500101217     d   ErrGenerico                   n   overlay(IndDspF : 99)
010600080428
010700101217       // -?Parametro Numero Spedizione?
010800080428     d ds_Spedizione   ds            16    inz
010900080428     d   V1Caas                       4s 0 inz
011000080428     d   V1Clnp                       3s 0 inz
011100080428     d   V1Cnrs                       2s 0 inz
011200080428     d   V1Cnsp                       7s 0 inz
011300101217
011400101217       // -?Parametro Distinta?
011500101217     d ds_Distinta     ds             9    inz
011600101217     d   V1Clna                       3s 0 inz
011700101217     d   V1Cndc                       6s 0 inz
011800101217
011900101217       // -?Parametro Data/Ora creazione immagine?
012000101217     d ds_DataOraCrea  ds            14    inz
012100101217     d   V1Cdci                       8s 0 inz
012200101217     d   V1Chci                       6s 0 inz
012300080428
012400101217       // -?Parametri per Interrogazione bolle arrivi?
012500170127     d*Prm_FNLR36ds    ds                  inz
012600170127     d* PA1aas                14     17  0 inz
012700170127     d* PA1lnp                18     20  0 inz
012800170127     d* PA1nrs                21     22  0 inz
012900170127     d* PA1nsp                23     29  0 inz
013000170127     d* PA1f03                30     30    inz
013100170127     d* PA1flg                31     31    inz
013200170127     d* PA1pgm                32     41    inz
013300170127     d* PA1dsa                42     61    inz
013400170127     d* PA1cml                62     62    inz
013500170127     d* PA1dsp                63     70  0 inz
013600170127     d* PA1rmn                71     85  0 inz
013700170127     d* PA1lna                86     88  0 inz
013800170127     d* PA1ln1                89    108    inz
013900170127     d* PA1rsd               109    143    inz
014000170127      *  // -?Se richiamo da pgm Giac. = 1?
014100170127     d* PA1gia               144    144    inz
014200170127     d* PA1fel               145    147  0 inz
014300170127     d* PA1tbo               148    148    inz
014400080709
014500101217       // -?Parametri per Interrogazione bolle partenze?
014600170127     d*Prm_FNLS04ds    ds                  inz
014700170127     d* PA0aas                 1      4  0 inz
014800170127     d* PA0lnp                 5      7  0 inz
014900170127     d* PA0nrs                 8      9  0 inz
015000170127     d* PA0nsp                10     16  0 inz
015100170127     d* PA0flg                31     31    inz(*on)
015200170127      *  // -?PA0CLI indica se il pgm è richiamato da un cliente (="S")?
015300170127     d* PA0cli               143    143
015400170127      *  // -?Passaggio dati da filtro per cambio azienda - FNLV02R?
015500170127     d* PA0pgm               201    210
015600170127     d* PA0azi               211    211
015700170127     d* PA0f19               212    212
015800170127     d* PA0f03               213    213
015801170127       // -?Parametri per Interrogazione bolle
015803170127     d Prm_FNLRU6ds    ds                  inz
015805170127      *?CAMPI DI OUTPUT:?
015806170127     d* . Key spedizione
015807170127     d  PA1aas                14     17  0 inz
015808170127     d  PA1lnp                18     20  0 inz
015809170127     d  PA1nrs                21     22  0 inz
015810170127     d  PA1nsp                23     29  0 inz
015811170127     d* . Flag '1'=Premuto F3=Fine
015812170127     d  PA1f03                30     30    inz
015814170127      *?CAMPI DI INPUT:?
015815170127     d* . Modalità di richiamo
015816170127     d  PA1flg                31     31    inz('2')
015823170127         // -?Se richiamo da pgm Giac. = 1?
015824170127     d  PA1gia               144    144    inz
015825170127     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
015826170127     d  pa1rma               149    163
015827170127     d  pa1xco               164    164
015828170127     d* non utilizzato
015829170127     d  pa1flo               165    256
015830170127
015831170127     d fnlru6ds      e ds
015900080428
016000101217       // -?Parametri ricevuti?
016100080424     d KPJBA         e ds
016200080505
016300101217       // -?Dati LDV da stampare?
016400080505     d FNLSB5ds      e ds                  inz
016500130307     d FNLSB6ds1     e ds                  inz
016600080424
016700080424       //--------------------------------------------------------------
016800080424       //?Definizione variabili globali.
016900080424       //--------------------------------------------------------------
017000080428
017100101217       // -?Flags booleani?
017200080428     d $Fine           s               n   inz(*off)
017300080429     d $InzD01         s               n   inz(*on)
017400080429
017500101217       // -?Campi di comodo?
017600080709     d SAVtbo          s                   inz  like(V1Ctbo)
017700080506     d SAVpgm          s                   inz  like(V1Cpgm)
017800080711     d SAVfa4          s                   inz  like(V1Cfa4)
017900080711     d SAVfl6          s                   inz  like(V1Cfl6)
018000130307     d SAVpdf          s                   inz  like(V1Cpdf)
018100130307     d SAVnum          s                   inz  like(V1Cnum)
018200100914     d SAVrisult       s                   inz  like(Risultato)
018300100914
018400101217       // -?Parametri per FISP30R:?
018500101217       //  ?Stored Procedure x Dati per Immagine LdV + Stampa?
018600100914     **Spedizione******s             16    inz     (vedi ds_Spedizione)
018700101217     **Distinta********s              9    inz     (vedi ds_Distinta)
018800101217     **DataOraCrea*****s             14    inz     (vedi ds_DataOraCrea)
018900100914     d Risultato       s              2    inz
019000100914     d RootFolder      s            100    inz
019100100914     d FileName        s             30    inz
019200100914     d TemplateName    s             20    inz
019300110228     d LastRow         s            120    inz
019400100914     d RtnEsito        s             10i 0 inz
019500100914     d RtnOpCode       s             10    inz
019600100914     d RtnStatus       s             10i 0 inz
019700100914     d $Prt            s              1    inz('T')
019800080424
019900080424       //--------------------------------------------------------------
020000080424       //?Definizione procedure usate.
020100080424       //--------------------------------------------------------------
020200080424
020300101217       // -?Reperimento dati utente?
020400101217     d TIBS34ds      e ds
020500101217      /copy gaitrasrc/srcProtoPR,TIBS34R
020600080429
020700170127      *// -?Interrogazione bolle arrivi?
020800170127      */copy gaitrasrc/srcProtoPR,FNLR36R
020801170127       // -?Interrogazione bolle unica ?
020802170127      /copy gaitrasrc/srcProtoPR,FNLRU6R
020900080709
021000101217       // -?Interrogazione bolle partenze?
021100170127     d*fnls04r         pr                  extpgm('FNLS04R')
021200170127     d*  kpjba                             likeds(KPJBA)
021300080606
021400101217       // -?Stampa LdV?
021500110330      /copy gaitrasrc/srcProtoPR,FNLSB6R
021600130307     d fnlsB6r£        pr                  extpgm('FNLSB6R£')
021700130307     d   fnlsB5ds                          likeds(FNLSB5ds)
021800130307     d   fnlsB6ds1                         likeds(FNLSB6ds1)
021900130311     d                                       options( *nopass :
022000130311     d                                                *omit )
022100110330     d fnlsBXr         pr                  extpgm('FNLSBXR')
022200110330     d   fnlsB5ds                          likeds(FNLSB5ds)
022300130307     d   fnlsB6ds1                         likeds(FNLSB6ds1)
022400130311     d                                       options( *nopass :
022500130311     d                                                *omit )
022600081217
022700101217       // -?Stored Procedure per immagine LdV?
022800100914     d fisp30r         pr                  extpgm('FISP30R')
022900100914     d   P_Spedizione...
023000100914     d                                     like(ds_Spedizione)
023100101217     d   P_Distinta...
023200101217     d                                     like(ds_Distinta)
023300101217     d   P_DataOraCrea...
023400101217     d                                     like(ds_DataOraCrea)
023500100914     d   P_Risultato...
023600100914     d                                     like(Risultato)
023700100914     d   P_RootFolder...
023800100914     d                                     like(RootFolder)
023900100914     d   P_FileName...
024000100914     d                                     like(FileName)
024100100914     d   P_TemplateName...
024200100914     d                                     like(TemplateName)
024300110228     d   P_LastRow...
024400110228     d                                     like(LastRow)
024500100914     d   P_RtnEsito...
024600100914     d                                     like(RtnEsito)
024700100914     d   P_RtnOpCode...
024800100914     d                                     like(RtnOpCode)
024900100914     d   P_RtnStatus...
025000100914     d                                     like(RtnStatus)
025100100914     d   P_Prt...
025200100914     d                                     like($Prt)
025300080424
025400080424       //--------------------------------------------------------------
025500080424       //?Definizione key-list.
025600080424       //--------------------------------------------------------------
025700080424
025800101217       // -?File FNARB01L / FNBLP01L?
025900080428     d k04fnarb01    e ds                  extname(FNARB01L:*key)
026000080428     d                                     prefix(k_)
026100080428     d                                     inz
026200080424
026300080424       //--------------------------------------------------------------
026400080424       //?Riepilogo indicatori.
026500080424       //--------------------------------------------------------------
026600080424
026700080424     c     *Entry        plist
026800080424     c                   parm                    KPJBA
026900080424
027000080424      /free
027100080424
027200101217       // -?Start?
027300080424       *inLR = *on;
027400080424
027500101217       // -?Elab?
027600080428       dow  $Fine = *off;
027700101217         exsr  sr_GesD01;
027800080428       enddo;
027900080424
028000101217       // -?End?
028100101217       exsr  sr_EndPgm;
028200080424       return;
028300080424
028400080424       //--------------------------------------------------------------
028500080424       //?Operazioni iniziali
028600080424       //--------------------------------------------------------------
028700100218       BEGSR  *InzSR;
028800080424
028900101217         // -?Reperimento dati job?
029000101217         exsr  sr_DatiJob;
029100080506
029200101217         // -?Impostazione nome programma a video?
029300080506         VTDpgm = SDSpgm;
029400080424
029500080424       ENDSR;
029600080424
029700080424       //--------------------------------------------------------------
029800080424       //?Reperimento Dati del job (Utente/Operativi).
029900080424       //--------------------------------------------------------------
030000100218       BEGSR  sr_DatiJob;
030100080424
030200080424         in(E) §AzUte;
030300080424         if NOT %error;
030400080424           in(E) §DatiUte;
030500080424         endif;
030600080424         if %error or RSut = *blanks;
030700080424           clear TIBS34ds;
030800080424           tibs34r(tibs34ds);
030900080424           in §AzUte;
031000080424           in §DatiUte;
031100080424         endif;
031200080424
031300080424       ENDSR;
031400080428
031500080428       //--------------------------------------------------------------
031600080428       //?Gestione videata D01
031700080428       //--------------------------------------------------------------
031800100218       BEGSR  sr_GesD01;
031900080428
032000101217         // -?Inizializzazione videata?
032100080428         if  $InzD01 = *on;
032200080709           SAVtbo   = V1Ctbo;
032300080506           SAVpgm   = V1Cpgm;
032400080711           SAVfa4   = V1Cfa4;
032500080711           SAVfl6   = V1Cfl6;
032600130307           SAVpdf   = V1Cpdf;
032700130307           SAVnum   = V1Cnum;
032800080606           clear LSB6D01;
032900080428           V1Caas   = *year;
033000080709           if  SAVtbo = *blank;
033100080709             V1Ctbo   = 'A';
033200080709           else;
033300080709             V1Ctbo   = SAVtbo;
033400080709           endif;
033500080506           if  SAVpgm = *blank;
033600080606             V1Cpgm   = 'FNLSB6R   ';
033700080506           else;
033800080506             V1Cpgm   = SAVpgm;
033900080506           endif;
034000080711           if  SAVfa4 = *blank;
034100080711             V1Cfa4   = '1';
034200080711           else;
034300080711             V1Cfa4   = SAVfa4;
034400080711           endif;
034500080711           if  SAVfl6 = *blank;
034600080711             V1Cfl6   = *off;
034700080711           else;
034800080711             V1Cfl6   = SAVfl6;
034900080711           endif;
035000130307           V1Cpdf   = SAVpdf;
035100130307           V1Cnum   = SAVnum;
035200080429           $InzD01  = *off;
035300080428         endif;
035400100218
035500101217         // -?Verifica correttezza del sistema informativo?
035600100218         if  %subst(knsif : 1 : 6) <> 'FILTRA';
035700100218           ErrMessage  = *on;
035800100218           ErrGenerico = *on;
035900100218           V1Dmsg = $Msg(01);
036000100218         endif;
036100080428
036200101217         // -?Emissione videata?
036300100218         write LSB6T01;
036400100218         write LSB6Z01;
036500080514
036600080606         exfmt LSB6D01;
036700080514
036800080428         reset ErrMessage;
036900080428         reset ErrGenerico;
037000080428         clear V1Dmsg;
037100080514
037200080514         clear V1Dtxt;
037300080428
037400080428         SELECT;
037500100218
037600101217           // -?Sistema Informativo errato?
037700100218           WHEN  %subst(knsif : 1 : 6) <> 'FILTRA';
037800100218             $Fine = *on;
037900080428
038000101217           // -?F3=Fine?
038100080428           WHEN  dsp_aid = c_F03;
038200080428             $Fine = *on;
038300080428
038400101217           // -?F4=Interrogazione bolle arrivi?
038500170127           //WHEN  dsp_aid = c_F04;
038600170127           //  exsr  sr_Call_FNLR36R;
038700080709
038800101217           // -?F5=Interrogazione bolle partenze?
038900170127           //WHEN  dsp_aid = c_F05;
039000170127           //  exsr  sr_Call_FNLS04R;
039001170127
039002170127           // -?F4=Interrogazione bolle unica ?
039003170127           WHEN  dsp_aid = c_F07;
039004170127             exsr  sr_Call_FNLRU6R;
039100080924
039200101217           // -?F8=Chiusura spool?
039300080924           WHEN  dsp_aid = c_F08;
039400101217             exsr  sr_EndPgm;
039500080428
039600101217           // -?Invio o F6=Conferma?
039700080428           OTHER;
039800101217             exsr  sr_CtrD01;
039900080429             select;
040000101217               // -?rilevato errore?
040100080429               when  ErrGenerico = *on;
040200101217               // -?non premuto F6?
040300080429               when  dsp_aid <> c_F06;
040400101217               // -?selezionata una singola spedizione?
040500130312               when  (V1Clnp  <> *zero   and
040600130312                      V1Cnsp  <> *zero)   or
040700130312                      V1Cpdf  =  'S';
040800101217                 exsr  sr_Call_FNLSB6R;
040900110401                 if  Not ErrGenerico;
041000110401                   reset $InzD01;
041100110401                 endif;
041200080429             endsl;
041300080428
041400080428         ENDSL;
041500080428
041600080428       ENDSR;
041700080424
041800080424       //--------------------------------------------------------------
041900080424       //?Richiamo pgm. Interrogazione Bolle Arrivi.
042000080424       //--------------------------------------------------------------
042100170127       //BEGSR  sr_Call_FNLR36R;
042200080424
042300170127       //  clear  Prm_FNLR36ds;
042400170127       //  PA1flg = '2';
042500170127       //  PA1pgm = SDSpgm;
042600170127       //  if  Pa1Fel <= *zero;
042700170127       //    PA1dsa = RsUt;
042800170127       //    PA1fel = DUTtfp;
042900170127       //  endif;
043000170127       //  kpjbu  = Prm_FNLR36ds;
043100080424
043200170127       //  fnlr36r (kpjba);
043300080424
043400170127       //  Prm_FNLR36ds = kpjbu;
043500080424
043600170127       //  if  Pa1F03 = *on;
043700170127       //    leavesr;
043800170127       //  endif;
043900080424
044000170127       //  clear ds_Spedizione;
044100170127       //  V1Caas = PA1aas;
044200170127       //  V1Clnp = PA1lnp;
044300170127       //  V1Cnrs = PA1nrs;
044400170127       //  V1Cnsp = PA1nsp;
044500170127       //  if  PA1nsp > *zero;
044600170127       //    V1Ctbo = 'A';
044700170127       //  endif;
044800080424
044900170127       //ENDSR;
045000080709
045100080709       //--------------------------------------------------------------
045200080709       //?Richiamo pgm. Interrogazione Bolle Partenze.
045300080709       //--------------------------------------------------------------
045400170127       //BEGSR  sr_Call_FNLS04R;
045500080709
045600170127       //  reset Prm_FNLS04ds;
045700170127       //  PA0lnp = DUTpou;
045800080709         //PA0flg = *blank;
045900080709         //PA0azi = 'S';
046000170127         //kpjbu  = Prm_FNLS04ds;
046100080709
046200170127         //fnls04r (kpjba);
046300080709
046400170127         //Prm_FNLS04ds = kpjbu;
046500080709
046600170127         //if  Pa0F03 = *on;
046700170127         //  leavesr;
046800170127         //endif;
046900080709
047000170127         //clear ds_Spedizione;
047100170127         //V1Caas = PA0aas;
047200170127         //V1Clnp = PA0lnp;
047300170127         //V1Cnrs = PA0nrs;
047400170127         //V1Cnsp = PA0nsp;
047500170127         //if  PA0nsp > *zero;
047600170127         //  V1Ctbo = 'P';
047700170127         //endif;
047800080709
047900170127       //ENDSR;
047901170127       //--------------------------------------------------------------
047902170127       //?Richiamo pgm. Interrogazione Bolle Unica .
047903170127       //--------------------------------------------------------------
047904170127       BEGSR  sr_Call_FNLRU6R;
047905170127
047906170127         clear  Prm_FNLRU6ds;
047907170127         PA1flg = '2';
047913170127         kpjbu  = Prm_FNLRU6ds;
047914170127
047915170127         fnlru6r (kpjba);
047916170127
047917170127         Prm_FNLRU6ds = kpjbu;
047918170127
047919170127         if  Pa1F03 = *on;
047920170127           leavesr;
047921170127         endif;
047922170127
047923170127         clear ds_Spedizione;
047924170127         V1Caas = PA1aas;
047925170127         V1Clnp = PA1lnp;
047926170127         V1Cnrs = PA1nrs;
047927170127         V1Cnsp = PA1nsp;
047932170127
047933170127       ENDSR;
048000080428
048100080428       //--------------------------------------------------------------
048200080428       //?Richiamo stored procedure per immagini LdV
048300080428       //--------------------------------------------------------------
048400101217       BEGSR  sr_Call_FNLSB6R;
048500081217
048600130307         clear   fnlsB5ds;
048700081217         DB0tbo = V1Ctbo;
048800081217         DB0fa4 = V1Cfa4;
048900081217         DB0fl6 = V1Cfl6;
049000081217         DB0aas = V1Caas;
049100081217         DB0lnp = V1Clnp;
049200081217         DB0nrs = V1Cnrs;
049300081217         DB0nsp = V1Cnsp;
049400130307
049500130307         clear   fnlsB6ds1;
049600130307         DB6pdf = V1Cpdf;
049700130307         DB6num = V1Cnum;
049800080429
049900080505         select;
050000080606
050100080606           when V1Cpgm = 'FNLSB6R   ';
050200130307             fnlsB6r (fnlsB5ds : fnlsB6ds1);
050300130307
050400130307           when V1Cpgm = 'FNLSB6R£  ';
050500130307             fnlsB6r£ (fnlsB5ds : fnlsB6ds1);
050600110330
050700110330           when V1Cpgm = 'FNLSBXR   ';
050800130307             fnlsBXr (fnlsB5ds : fnlsb6ds1);
050900081217
051000100914           when V1Cpgm = 'FISP30R   ';
051100100914             reset  $Prt;
051200101217             fisp30r (ds_Spedizione : ds_Distinta : ds_DataOraCrea :
051300101217                      Risultato :
051400100914                      RootFolder : FileName : TemplateName :
051500110228                      LastRow :
051600100914                      RtnEsito : RtnOpCode : RtnStatus : $Prt);
051700100914             SavRisult = Risultato;
051800101217
051900101217           other;
052000101217             ErrMessage  = *on;
052100101217             ErrGenerico = *on;
052200101217             PosCurPGM   = *on;
052300101217             V1Dmsg = $Msg(04);
052400101217             leavesr;
052500080606
052600080505         endsl;
052700080514
052800080926         select;
052900100914           when  V1Cpgm = 'FISP30R   ';
053000100914             V1Dtxt = 'Risultato: ' + SavRisult;
053100100927           when  DB0tms > *zero  or   DB0tms = 'P'  or  DB0tms = 'Q';
053200080926             V1Dtxt = 'Generata stampa.';
053300130307           when  DB0tms = 'I'  or  DB0tms = 'J';
053400130307             V1Dtxt = 'Generata stampa, ma rimane il PDF.';
053500100914           when  DB0tms = 'D'   and   DB0fa4 = '4';
053600080926             V1Dtxt = 'Stampata LdV a ¼, ma rimane la packing-list.';
053700130307           when  DB0tms = 'i'   and   DB0fa4 = '4';
053800130307             V1Dtxt = 'Stampata LdV a ¼, ma rimane il PDF.';
053900130307           when  DB0tms = 'Y'   and   DB0fa4 = '4';
054000130307             V1Dtxt = 'Stampata LdV a ¼, ma rimangono Pkg-Lst e PDF.';
054100130307           when  DB0tms = 'E';
054200130307             V1Dtxt = '» Rilevato errore «';
054300100914           other;
054400080926             V1Dtxt = 'NON generata stampa!!!';
054500080926         endsl;
054600080704         write  Protect;
054700080704         exfmt  LSB6D02;
054800130307         V1Cnum = DB6num;
054900100914
055000101217         // -?F3=Fine?
055100100914         if  dsp_aid = c_F03;
055200100914           $Fine = *on;
055300100914           leavesr;
055400100914         endif;
055500080428
055600080428       ENDSR;
055700080428
055800080428       //--------------------------------------------------------------
055900080428       //?Controllo dati in videata D01
056000080428       //--------------------------------------------------------------
056100100218       BEGSR  sr_CtrD01;
056200080429
056300101217         %subst(IndDspF : 50) = *off;
056400080429
056500080514         select;
056600080514           when  V1Caas  = *zero;
056700080514             V1Caas  = *year;
056800080514           when  V1Caas  > 100;
056900080514           when  V1Caas  > 50;
057000080514             V1Caas += 1900;
057100080514           when  V1Caas <= 50;
057200080514             V1Caas += 2000;
057300080514         endsl;
057400080428
057500080428         SELECT;
057600080428
057700101217           // -?Nessuna spedizione inserita?
057800130312           WHEN  (V1Clnp = *zero   and
057900130312                  V1Cnrs = *zero   and
058000130312                  V1Cnsp = *zero   and
058100130312                  V1Cnum = *blank)  or
058200130312                 (V1Clnp = *zero   and
058300130312                  V1Cnrs = *zero   and
058400130312                  V1Cnsp = *zero   and
058500130312                 (V1Cpdf = *blank   or
058600130312                  V1Cpdf = 'F'      or
058700130312                  V1Cpdf = 'X'))    or
058800130312                 (V1Cnum = *blank   and
058900130312                  V1Cpdf = 'S');
059000080428             ErrMessage  = *on;
059100080428             ErrGenerico = *on;
059200080428             PosCurSped  = *on;
059300100218             V1Dmsg = $Msg(02);
059400080429             leavesr;
059500080428
059600101217           // -?Selezionata una singola spedizione?
059700080709           WHEN  V1Clnp <> *zero   or
059800080428                 V1Cnrs <> *zero   or
059900080709                 V1Cnsp <> *zero;
060000080709             k_ARBaas = V1Caas;
060100080709             k_ARBlnp = V1Clnp;
060200080709             k_ARBnrs = V1Cnrs;
060300080709             k_ARBnsp = V1Cnsp;
060400080709             if  V1Ctbo =  'A';
060500100218               if  not %open(FNARB01L);
060600100218                 open  FNARB01L;
060700100218               endif;
060800101217               chain  %kds(K04fnarb01) FNARB000;
060900101217               if not %found(FNARB01L);
061000080709                 ErrMessage  = *on;
061100080709                 ErrGenerico = *on;
061200080709                 PosCurSped  = *on;
061300100218                 V1Dmsg = $Msg(03);
061400080709                 leavesr;
061500080709               endif;
061600080709             else;
061700100218               if  not %open(FNBLP01L);
061800100218                 open  FNBLP01L;
061900100218               endif;
062000080709               setll  %kds(K04fnarb01) FNBLP000;
062100080709               if not %equal(FNBLP01L);
062200080709                 ErrMessage  = *on;
062300080709                 ErrGenerico = *on;
062400080709                 PosCurSped  = *on;
062500100218                 V1Dmsg = $Msg(03);
062600080709                 leavesr;
062700080709               endif;
062800080709             endif;
062900080428
063000080428         ENDSL;
063100101217
063200101217         // -?Reperimento n° distinta?
063300101217         //  ?(per ora: campi definiti a *pgm, NON a video)?
063400101217         if  ds_Distinta = *zero   and   ds_DataOraCrea = *zero
063500101217                                   and   V1Ctbo = 'A';
063600101217           V1Clna = ARBlna;
063700110210           V1Cndc = %int( %subst( %editc( ARBndc : 'X' ) : 2 ) );
063800101217         endif;
063900080506
064000101217         // -?Programma da richiamare di default?
064100080506         if  V1Cpgm = *blank;
064200080606           V1Cpgm   = 'FNLSB6R   ';
064300080506         endif;
064400080428
064500080428       ENDSR;
064600080606
064700080606       //--------------------------------------------------------------
064800080606       //?Fine: chiusura dei files di stampa
064900080606       //--------------------------------------------------------------
065000100218       BEGSR  sr_EndPgm;
065100080606
065200100915         if  %subst(knsif : 1 : 6) = 'FILTRA'   and
065300100914             V1Cpgm = 'FNLSB6R   ';
065400110330           clear   fnlsB5ds;
065500100914           DB0tla = 'C';
065600130311           fnlsB6r (fnlsB5ds);
065700100914         endif;
065800130307
065900130307         if  %subst(knsif : 1 : 6) = 'FILTRA'   and
066000130307             V1Cpgm = 'FNLSB6R£  ';
066100130307           clear   fnlsB5ds;
066200130307           DB0tla = 'C';
066300130311           fnlsB6r£ (fnlsB5ds);
066400130307         endif;
066500110330
066600110330         if  %subst(knsif : 1 : 6) = 'FILTRA'   and
066700110330             V1Cpgm = 'FNLSBXR   ';
066800110330           clear   fnlsB5ds;
066900110330           DB0tla = 'C';
067000130311           fnlsBXr (fnlsB5ds);
067100110330         endif;
067200080606
067300080606       ENDSR;
067400080424
067500080424      /end-free
067600080428
067700130312** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
067800100218Programma NON eseguibile nel sistema informativo di SEDE. "Invio" per uscire.
067900080428Nessuna parzializzazione inserita
068000080514Spedizione inesistente
068100101217Programma NON previsto
