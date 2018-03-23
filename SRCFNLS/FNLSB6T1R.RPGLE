000100100920     /*END
000200080424       //--------------------------------------------------------------
000300090305       //?TEST - STAMPA LDV SU STAMPANTE LASER IN MODELLO UNICO A4     ?
000400090305       //?       PER TEST LETTURA BARCODE                              ?
000500080424       //--------------------------------------------------------------
000600080424
000700080424     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000800100920     h dftactgrp(*no)
000900100920     h bnddir('TRUL':'UBBNDDIR')
001000080424
001100080424       //--------------------------------------------------------------
001200090305       //?Dichiarazione file.                                          ?
001300080424       //--------------------------------------------------------------
001400090305
001500090305       // - Archivio "bolle arrivo - testate"
001600090305     fFNARB26L  if   e           k disk
001601170201     fFNARB01L  if   e           k disk    rename(fnarb000:fnarb01)
001602170201     f                                     prefix(A1_)
001700090305
001800090305       // - Video
001900090305     fFNLSB6T1D cf   e             workstn
002000080428     f                                     indds(IndDspF)
002100080428     f                                     infds(InfDspF)
002200080424
002300080424       //--------------------------------------------------------------
002400090305       //?Definizione costanti.                                        ?
002500080424       //--------------------------------------------------------------
002600080424
002700090305       // - Tasti funzionali a video
002800080428     d c_F01           c                   const(x'31')
002900080428     d c_F02           c                   const(x'32')
003000080428     d c_F03           c                   const(x'33')
003100080429     d c_F04           c                   const(x'34')
003200080428     d c_F05           c                   const(x'35')
003300080428     d c_F06           c                   const(x'36')
003400080428     d c_F07           c                   const(x'37')
003500080428     d c_F08           c                   const(x'38')
003600080428     d c_F09           c                   const(x'39')
003700080428     d c_F10           c                   const(x'3A')
003800080429     d c_F11           c                   const(x'3B')
003900080428     d c_F12           c                   const(x'3C')
004000080428     d c_F13           c                   const(x'B1')
004100080428     d c_F14           c                   const(x'B2')
004200080428     d c_F15           c                   const(x'B3')
004300080428     d c_F16           c                   const(x'B4')
004400080428     d c_F17           c                   const(x'B5')
004500080428     d c_F18           c                   const(x'B6')
004600080428     d c_F19           c                   const(x'B7')
004700080428     d c_F20           c                   const(x'B8')
004800080428     d c_F21           c                   const(x'B9')
004900080428     d c_F22           c                   const(x'BA')
005000080428     d c_F23           c                   const(x'BB')
005100080428     d c_F24           c                   const(x'BC')
005200080428     d c_Enter         c                   const(x'F1')
005300080428     d c_RollDown      c                   const(x'F4')
005400080428     d c_RollUp        c                   const(x'F5')
005500080424
005600080424       //--------------------------------------------------------------
005700090305       //?Definizione schiere.                                         ?
005800080424       //--------------------------------------------------------------
005900080424
006000090305       // - Messaggi di errore
006100090305     d $Msg            s             78    dim( 5) ctdata perrcd(1)
006200080424
006300080424       //--------------------------------------------------------------
006400090305       //?Definizione aree dati.                                       ?
006500080424       //--------------------------------------------------------------
006600080424
006700080424       // - Dati utente
006800080424     d §AzUte        e ds                  extname(AZUTE00F)
006900080424     d                                     dtaara
007000080424     d §DatiUte      e ds                  extname(dDatiUte)
007100080424     d                                     dtaara
007200080424
007300080424       //--------------------------------------------------------------
007400090305       //?Definizione strutture dati.                                  ?
007500080424       //--------------------------------------------------------------
007600080424
007700080424       // - Status ds
007800080424     d Status         sds
007900080424     d  SDSpgm           *proc
008000080424     d  SDSprm           *parms
008100080424     d  SDSusr               254    263
008200080428
008300090305       // - InfDS
008400080428     d InfDspF         ds
008500080428     d  dsp_aid              369    369a
008600080428
008700090305       // - Indicatori su DspF
008800080428     d IndDspF         ds
008900090305         // Visualizzazione messaggio di errore
009000090305     d  ErrMessage                    1n   overlay(IndDspF : 28)
009100090305         // Protezione campi
009200090305     d  NoProtect                     1n   overlay(IndDspF : 41)
009300090305         // Indicatori di errore
009400090305     d  PosCurNUM                     1n   overlay(IndDspF : 50)
009500090305     d  PosCurFA4                     1n   overlay(IndDspF : 51)
009600090305     d  PosCurDSI                     1n   overlay(IndDspF : 52)
009700090305     d  PosCurDSF                     1n   overlay(IndDspF : 53)
009800090305     d  PosCurCBO                     1n   overlay(IndDspF : 54)
009900080428     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010000080428
010100090305       // - Parametri per Interrogazione bolle arrivi
010200090305     d FNLR36ds        ds                  inz
010300080428     d  PA1aas                14     17  0 inz
010400080428     d  PA1lnp                18     20  0 inz
010500080428     d  PA1nrs                21     22  0 inz
010600080428     d  PA1nsp                23     29  0 inz
010700080428     d  PA1f03                30     30    inz
010800080709     d  PA1flg                31     31    inz
010900080428     d  PA1pgm                32     41    inz
011000080428     d  PA1dsa                42     61    inz
011100080428     d  PA1cml                62     62    inz
011200080428     d  PA1dsp                63     70  0 inz
011300080428     d  PA1rmn                71     85  0 inz
011400080428     d  PA1lna                86     88  0 inz
011500080428     d  PA1ln1                89    108    inz
011600080428     d  PA1rsd               109    143    inz
011700090305       // - Se richiamo da pgm Giac. = 1
011800080428     d  PA1gia               144    144    inz
011900080428     d  PA1fel               145    147  0 inz
012000080428     d  PA1tbo               148    148    inz
012100080429
012200090305       // - Parametri per pgm. controllo/inversione data
012300080429     d WLBdat          ds                  inz
012400080429     d  G08dat                        8  0 inz
012500080429     d  G08inv                        8  0 inz
012600080429     d  G08err                        1    inz('3')
012700080429     d  G08tgi                        5  0 inz
012800080428
012900090305       // - Parametri ricevuti
013000080424     d KPJBA         e ds
013100080424
013200080424       // - Parametri x Controllo profilo utenti
013300080424     d TIBS34ds      e ds
013400080505
013500080505       // - Dati LDV da stampare
013600080505     d FNLSB5ds      e ds                  inz
013700130311     d FNLSB6ds1     e ds
013800130311     d                                     BASED(nullPtr)
013900080424
014000080424       //--------------------------------------------------------------
014100090305       //?Definizione variabili globali.                               ?
014200080424       //--------------------------------------------------------------
014300080428
014400080428       // - Flags booleani
014500080428     d $Fine           s               n   inz(*off)
014600080429     d $InzD01         s               n   inz(*on)
014700090305
014800090305       // - Campi per la gestione del video
014900090305     d $Video          s              2    inz('D1')
015000080429
015100080429       // - Campi di comodo
015200090305     d W1Cdsi          s                   like(V1Cdsi)  inz
015300090305     d W1Cdsf          s                   like(V1Cdsf)  inz
015400090305     d wCountLdV       s              5s 0 inz
015401170201     d dataeur         s               d   datfmt(*eur)
015500080424
015600080424       //--------------------------------------------------------------
015700090305       //?Definizione procedure usate.                                 ?
015800080424       //--------------------------------------------------------------
015900080424
016000080424       // - Reperimento dati utente
016100090305      /copy gaitrasrc/srcProtoPr,TIBS34R
016200090305
016300090305       // - Controllo ed inversione date
016400090305      /copy gaitrasrc/srcProtoPr,XSRDA8
016500100920
016600100920       // -?Parametri per pgm interrogazione tabelle (TABEL00F)?
016700100920     d ds_TABEL      e ds                  extname(TABEL00F)  inz
016800100920      /copy gaitrasrc/srcProtoPI,X§TABER
016900100920       // -?Interrogazione tabelle?
017000100920      /copy gaitrasrc/srcProtoPR,X§TABER
017100100920       // -?Reperimento dati tabelle?
017200100920      /copy gaitrasrc/srcProtoPr,TRULTAB
017300080429
017400080429       // - Interrogazione bolle arrivi
017500090305      /copy gaitrasrc/srcProtoPr,FNLR36R
017600080606
017700090305       // - Stampa singola LdV
017800090305     d fnlsB6T2r       pr                  extpgm('FNLSB6T2R')
017900090305     d   fnlsB5ds                          likeds(FNLSB5ds)
018000090305     d   Prm_LNP                           like(V1Clnp)
018100090305     d   Prm_NRS                           like(V1Cnrs)
018200130311     d   fnlsB6ds1                         likeds(FNLSB6ds1)
018300130311     d                                       options( *omit :
018400130311     d                                                *nopass )
018500080424
018600080424       //--------------------------------------------------------------
018700090305       //?Definizione key-list.                                        ?
018800080424       //--------------------------------------------------------------
018900080424
019000090305       // - File FNARB26L
019100090305     d k06fnarb26    e ds                  extname(FNARB26L:*key)
019200080428     d                                     prefix(k_)
019300080428     d                                     inz
019400080424
019500080424       //--------------------------------------------------------------
019600090305       //?Riepilogo indicatori.                                        ?
019700080424       //--------------------------------------------------------------
019800080424
019900080424     c     *Entry        plist
020000080424     c                   parm                    KPJBA
020100080424
020200080424      /free
020300080424
020400080424       // - Start
020500080424       *inLR = *on;
020600080424
020700080424       // - Elab
020800080428       dow  $Fine = *off;
020900090305         select;
021000090305           when  $Video = 'D1';
021100090305             exsr  sr_GesD01;
021200090305           when  $Video = 'D2';
021300090305             exsr  sr_GesD02;
021400090305           other;
021500090305             $Fine = *on;
021600090305         endsl;
021700080428       enddo;
021800080424
021900080424       // - End
022000080606       exsr sr_EndPgm;
022100080424       return;
022200080424
022300080424       //--------------------------------------------------------------
022400090305       //?Operazioni iniziali                                          ?
022500080424       //--------------------------------------------------------------
022600090305       BEGSR  *InzSR;
022700080424
022800080424         // Reperimento dati utente
022900080424         exsr sr_DatiJob;
023000080506
023100080506         // Impostazione nome programma in testata a video
023200080506         VTDpgm = SDSpgm;
023300080424
023400080424       ENDSR;
023500080424
023600080424       //--------------------------------------------------------------
023700090305       //?Reperimento Dati del job (Utente/Operativi).                 ?
023800080424       //--------------------------------------------------------------
023900090305       BEGSR  sr_DatiJob;
024000080424
024100080424         in(E) §AzUte;
024200080424         if NOT %error;
024300080424           in(E) §DatiUte;
024400080424         endif;
024500080424         if %error or RSut = *blanks;
024600080424           clear TIBS34ds;
024700080424           tibs34r(tibs34ds);
024800080424           in §AzUte;
024900080424           in §DatiUte;
025000080424         endif;
025100080424
025200080424       ENDSR;
025300080428
025400080428       //--------------------------------------------------------------
025500090305       //?Gestione videata D01                                         ?
025600080428       //--------------------------------------------------------------
025700090305       BEGSR  sr_GesD01;
025800080428
025900080428         // Inizializzazione videata
026000080428         if  $InzD01 = *on;
026100090305           exsr  sr_InzD01;
026200080429           $InzD01  = *off;
026300080428         endif;
026400080428
026500090305         // Emissione videata
026600080429         if  ErrMessage = *off;
026700080606           write LSB6T01;
026800080606           write LSB6Z01;
026900080428         endif;
027000080514
027100080606         exfmt LSB6D01;
027200080514
027300080428         reset ErrMessage;
027400080428         reset ErrGenerico;
027500080428         clear V1Dmsg;
027600090305         NoProtect = *off;
027700080428
027800080428         SELECT;
027900080428
028000090305           // - F3=Fine
028100080428           WHEN  dsp_aid = c_F03;
028200080428             $Fine = *on;
028300080428
028400090305           // - F4=Interrogazione bolle arrivi
028500080428           WHEN  dsp_aid = c_F04;
028600080428             exsr Call_FNLR36R;
028700090305
028800090305           // - F8=Modifica Forzature
028900090305           WHEN  dsp_aid = c_F08;
029000090305             NoProtect = *on;
029100080428
029200090305           // - Invio o F6=Conferma
029300080428           OTHER;
029400080428             exsr sr_CtrD01;
029500080429             select;
029600090305               // - rilevato errore
029700080429               when  ErrGenerico = *on;
029800090305               // - non premuto F6
029900080429               when  dsp_aid <> c_F06;
030000090305               // - Stampa LdV
030100090305               other;
030200090305                 $Video = 'D2';
030300080429             endsl;
030400080428
030500080428         ENDSL;
030600080428
030700080428       ENDSR;
030800090305
030900090305       //--------------------------------------------------------------
031000090305       //?Richiamo pgm. Interrogazione Bolle Arrivi.                   ?
031100090305       //--------------------------------------------------------------
031200090305       BEGSR  sr_InzD01;
031300090305
031400090305         clear  LSB6D01;
031500090305
031600090305         V1Clna = DUTpou;
031700090305         V1Cnum = 250;
031800090305         V1Cfa4 = '4';
031900090305         W1Cdsi = %dec( %date(*date) - %days(5) );
032000090305         reset  WLBdat;
032100090305         G08inv = W1Cdsi;
032200090305         xsrda8 ( WLBdat );
032300090305         if  G08err = *off;
032400090305           V1Cdsi = G08dat;
032500090305         endif;
032600090305         W1Cdsf = %dec( %date(*date) );
032700090305         reset  WLBdat;
032800090305         G08inv = W1Cdsf;
032900090305         xsrda8 ( WLBdat );
033000090305         if  G08err = *off;
033100090305           V1Cdsf = G08dat;
033200090305         endif;
033300090305         V1Ccbo = '1 ';
033400090305
033500090305         V1Clnp = 102;
033600090305         V1Cnrs = 88;
033700090305
033800090305       ENDSR;
033900090305
034000090305       //--------------------------------------------------------------
034100090305       //?Controllo dati in videata D01                                ?
034200090305       //--------------------------------------------------------------
034300090305       BEGSR sr_CtrD01;
034400090305
034500090305         %subst(IndDspF : 50) = *off;
034600090305
034700090305         SELECT;
034800090305
034900090305           // - Numero LdV da stampare obbligatorio
035000090305           WHEN  %scan('?' : V1Cfa4) > *zero;
035100090305             V1Cfa4 = '4';
035200090305             exfmt  LSB6W01;
035300090305             leavesr;
035400090305
035500090305           // - Numero LdV da stampare obbligatorio
035600090305           WHEN  V1Cnum <= *zero;
035700090305             ErrMessage  = *on;
035800090305             ErrGenerico = *on;
035900090305             PosCurNUM   = *on;
036000090305             V1Dmsg = $Msg(01);
036100090305             leavesr;
036200090305
036300090305           // - Tipo   LdV da stampare errato o mancante
036400090305           WHEN  V1Cfa4 <> '1'   and
036500090305                 V1Cfa4 <> '2'   and
036600090305                 V1Cfa4 <> '4'   and
036700090305                 V1Cfa4 <> 'P'   and
036800090305                 V1Cfa4 <> 'S';
036900090305             ErrMessage  = *on;
037000090305             ErrGenerico = *on;
037100090305             PosCurFA4   = *on;
037200090305             V1Dmsg = $Msg(02);
037300090305             leavesr;
037400090305
037500090305         ENDSL;
037600090305
037700090305         // - Controllo data spedizione iniziale
037800090305         IF  V1Cdsi <= *zero;
037900090305           ErrMessage  = *on;
038000090305           ErrGenerico = *on;
038100090305           PosCurDSI   = *on;
038200090305           V1Dmsg = $Msg(03);
038300090305           leavesr;
038400090305         ELSE;
038500090305           clear  WLBdat;
038600090305           G08dat = V1Cdsi;
038700090305           xsrda8 ( WLBdat );
038800090305           if  G08err =  *on;
038900090305             ErrMessage  = *on;
039000090305             ErrGenerico = *on;
039100090305             PosCurDSI   = *on;
039200090305             V1Dmsg = $Msg(04);
039300090305             leavesr;
039400090305           endif;
039500090305           V1Cdsi = G08dat;
039600090305           W1Cdsi = G08inv;
039700090305           W1Cdsf = G08inv;
039800090305         ENDIF;
039900090305
040000090305         // - Controllo data spedizione finale
040100090305         IF  V1Cdsf <= *zero;
040200090305           ErrMessage  = *on;
040300090305           ErrGenerico = *on;
040400090305           PosCurDSF   = *on;
040500090305           V1Dmsg = $Msg(03);
040600090305           leavesr;
040700090305         ELSE;
040800090305           clear  WLBdat;
040900090305           G08dat = V1Cdsf;
041000090305           xsrda8 ( WLBdat );
041100090305           if  G08err =  *on;
041200090305             ErrMessage  = *on;
041300090305             ErrGenerico = *on;
041400090305             PosCurDSF   = *on;
041500090305             V1Dmsg = $Msg(04);
041600090305             leavesr;
041700090305           endif;
041800090305           V1Cdsf = G08dat;
041900090305           W1Cdsf = G08inv;
042000090305         ENDIF;
042100100920
042200100920         // - Controllo codice bolla
042300100920         clear  V1Dcbo;
042400100920         Select;
042500100920           When  V1Ccbo = *blank;
042600100920           When  %scan( '?' : V1Ccbo ) > *zero;
042700100920             clear  V1Ccbo;
042800100920             clear  V1Dcbo;
042900100920             X§Tcod = '3A';
043000100920             clear  X§Tkey;
043100100920             clear  X§Tdes;
043200100920             x§taber ( 1 : X§Tcod : X§Tkey : X§Tdes );
043300100920             V1Ccbo = X§Tkey;
043400100920             V1Dcbo = X§Tdes;
043500100920             ErrGenerico = *on;
043600100920             leavesr;
043700100920           Other;
043800100920             if  getTabella ( '1' : '3A' : V1Ccbo :
043900100920                              *omit : *omit : *omit :
044000100920                              *omit : *omit :
044100100920                              *omit : *omit : *omit : *omit :
044200100920                              *omit : *omit : *omit : *omit :
044300100920                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
044400100920                            < *zero       OR
044500100920                 ds_TNTBE.TBEatb <> *blank;
044600100920               ErrMessage  = *on;
044700100920               ErrGenerico = *on;
044800100920               PosCurCBO   = *on;
044900100920               V1Dmsg = $Msg(05);
045000100920               leavesr;
045100100920             else;
045200100920               V1Dcbo = ds_TNTBE.TBEuni;
045300100920             endif;
045400100920         EndSl;
045500090305
045600090305       ENDSR;
045700080424
045800080424       //--------------------------------------------------------------
045900090305       //?Richiamo pgm. Interrogazione Bolle Arrivi.                   ?
046000080424       //--------------------------------------------------------------
046100090305       BEGSR  Call_FNLR36R;
046200080424
046300090305         clear  FNLR36ds;
046400080424         PA1flg = '2';
046500080424         PA1pgm = SDSpgm;
046600080424         if  Pa1Fel <= *zero;
046700080424           PA1dsa = RsUt;
046800080424           PA1fel = DUTtfp;
046900080424         endif;
047000090305         kpjbu  = FNLR36ds;
047100080424
047200080424         fnlr36r (kpjba);
047300080424
047400090305         FNLR36ds = kpjbu;
047500080424
047600080424         if  Pa1F03 = *on;
047700080424           leavesr;
047800080424         endif;
047900080424
048000170201       //  V1Cdsi = PA1dsp;
048001170201         chain (pa1aas:pa1lnp:pa1nrs:pa1nsp) fnarb01l  ;
048002170201         if %found(fnarb01l);
048003170201            dataeur=%date(A1_arbaas*10000+A1_arbmgs:*iso)  ;
048004170201            v1cdsi=%dec(dataeur) ;
048005170201         endif;
048100080424
048200080424       ENDSR;
048300090305
048400090305       //--------------------------------------------------------------
048500090305       //?Gestione videata D02                                         ?
048600090305       //--------------------------------------------------------------
048700090305       BEGSR  sr_GesD02;
048800090305
048900090305         // - 1ª Inizializzazione videata
049000090305         clear  LSB6D02;
049100090305         clear  wCountLdV;
049200090305
049300090305         // - Ciclo di lettura FNARB26L e stampa LdV
049400090305         k_ARBlna = V1Clna;
049500090305         k_ARBaas = %int( W1Cdsi / 10000);
049600090305         k_ARBmgs = W1Cdsi - (k_ARBaas * 10000);
049700090305         setll  %kds(k06fnarb26 : 3)  FNARB000;
049800090305         reade  %kds(k06fnarb26 : 1)  FNARB000;
049900090305
050000090305         DoW  not %eof(FNARB26L)   and
050100090305             ((ARBaas * 10000) + ARBmgs) <= W1Cdsf;
050200090305           if  ARBatb = *blank   and
050300090305              (V1Ccbo = *blank   or
050400090305               V1Ccbo = ARBcbo);
050500090305             exsr  Call_FNLSB6T2;
050600090305             if  wCountLdV >= V1Cnum;
050700090305               leave;
050800090305             endif;
050900090305           endif;
051000090305         reade  %kds(k06fnarb26 : 1)  FNARB000;
051100090305         EndDo;
051200090305
051300090305         // - Chiusura spool
051400090305         clear   fnlsB5ds;
051500090305         DB0tla = 'C';
051600130311         fnlsB6T2r (fnlsB5ds : V1Clnp : V1Cnrs);
051700090305
051800090305         // - 2ª Inizializzazione videata
051900090305         V1Dtxt = 'Stampate n° ' + %char(wCountLdV) + ' LdV.';
052000090305
052100090305         // - Emissione videata
052200080704         write  Protect;
052300080704         exfmt  LSB6D02;
052400080923
052500080923         // - F3=Fine
052600080923         if  dsp_aid = c_F03;
052700080923           $Fine = *on;
052800080923           leavesr;
052900080923         endif;
053000090305
053100090305         // - Invio
053200090305         $Video  = 'D1';
053300090305         $InzD01 = *on;
053400080428
053500080428       ENDSR;
053600090305
053700090305       //--------------------------------------------------------------
053800090305       //?Richiamo stored procedure per stampa (di PROVA) LdV          ?
053900090305       //--------------------------------------------------------------
054000090305       BEGSR  Call_FNLSB6T2;
054100090305
054200090305         clear  fnlsB5ds;
054300090305         DB0fa4 = V1Cfa4;
054400090305         DB0tbo = 'A';
054500090305         DB0aas = ARBaas;
054600090305         DB0lnp = ARBlnp;
054700090305         DB0nrs = ARBnrs;
054800090305         DB0nsp = ARBnsp;
054900090305
055000130311         fnlsB6T2r (fnlsB5ds : V1Clnp : V1Cnrs);
055100090305
055200090305         select;
055300100927           when  DB0tms > *zero  or   DB0tms = 'P'  or  DB0tms = 'Q';
055400090305             //V1Dtxt = 'Stampata LdV.';
055500090305             wCountLdV += 1;
055600090305           when  DB0tms = 'D'   and   DB0fa4 = '4';
055700090305             //V1Dtxt = 'Stampata LdV a ¼, ma rimane la packing-list.';
055800090305             wCountLdV += 1;
055900090305           other;
056000090305             //V1Dtxt = 'NON generata stampa!!!';
056100090305         endsl;
056200090305
056300090305       ENDSR;
056400080606
056500080606       //--------------------------------------------------------------
056600090305       //?Fine: chiusura dei files di stampa                           ?
056700080606       //--------------------------------------------------------------
056800080606       BEGSR sr_EndPgm;
056900080606
057000090305         clear   fnlsb5ds;
057100090305         DB0tla = 'C';
057200130311         fnlsB6T2r (fnlsB5ds : V1Clnp : V1Cnrs);
057300080606
057400080606       ENDSR;
057500080424
057600080424      /end-free
057700080428
057800090305** - $Msg -------------------------------------------------------------------*
057900090305Numero di LdV da stampare obbligatorio                                         1
058000090305Tipo di LdV da stampare errato                                                 2
058100090305Data spedizione obbligatoria                                                   3
058200090305Data spedizione formalmente errata                                             4
058300100920Codice bolla errato                                                            5
