000100101217       //==============================================================
000200080424       //?TEST |STORE PROCEDURE| DATI X IMMAGINI LDV (TERMINALINI AUT)
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
001600100422     h dftactgrp(*no)
001700080424
001800080424       //--------------------------------------------------------------
001900080424       //?Dichiarazione file.
002000080424       //--------------------------------------------------------------
002100080424
002200101217       // -?Archivio "bolle arrivo - testate"?
002300110621     fFNARB01L  if   e           k disk    usropn
002400101217       // -?Organigramma filiale/aziendale?
002500080429     fAZORG01L  if   e           k disk
002600080428
002700101217       // -?Video?
002800080430     fFISP30TD  cf   e             workstn
002900080428     f                                     indds(IndDspF)
003000080428     f                                     infds(InfDspF)
003100080424
003200080424       //--------------------------------------------------------------
003300080424       //?Definizione costanti.
003400080424       //--------------------------------------------------------------
003500080424
003600101217       // -?Tasti funzionali a video?
003700080428     d c_F01           c                   const(x'31')
003800080428     d c_F02           c                   const(x'32')
003900080428     d c_F03           c                   const(x'33')
004000080429     d c_F04           c                   const(x'34')
004100080428     d c_F05           c                   const(x'35')
004200080428     d c_F06           c                   const(x'36')
004300080428     d c_F07           c                   const(x'37')
004400080428     d c_F08           c                   const(x'38')
004500080428     d c_F09           c                   const(x'39')
004600080428     d c_F10           c                   const(x'3A')
004700080429     d c_F11           c                   const(x'3B')
004800080428     d c_F12           c                   const(x'3C')
004900080428     d c_F13           c                   const(x'B1')
005000080428     d c_F14           c                   const(x'B2')
005100080428     d c_F15           c                   const(x'B3')
005200080428     d c_F16           c                   const(x'B4')
005300080428     d c_F17           c                   const(x'B5')
005400080428     d c_F18           c                   const(x'B6')
005500080428     d c_F19           c                   const(x'B7')
005600080428     d c_F20           c                   const(x'B8')
005700080428     d c_F21           c                   const(x'B9')
005800080428     d c_F22           c                   const(x'BA')
005900080428     d c_F23           c                   const(x'BB')
006000080428     d c_F24           c                   const(x'BC')
006100080428     d c_Enter         c                   const(x'F1')
006200080428     d c_RollDown      c                   const(x'F4')
006300080428     d c_RollUp        c                   const(x'F5')
006400080424
006500080424       //--------------------------------------------------------------
006600080424       //?Definizione schiere.
006700080424       //--------------------------------------------------------------
006800080424
006900101217       // -?Messaggi di errore?
007000101217     d $Msg            s             78    dim( 3)  ctdata  perrcd(1)
007100080424
007200080424       //--------------------------------------------------------------
007300080424       //?Definizione aree dati.
007400080424       //--------------------------------------------------------------
007500080424
007600101217       // -?Dati utente?
007700080424     d §AzUte        e ds                  extname(AZUTE00F)
007800080424     d                                     dtaara
007900080424     d §DatiUte      e ds                  extname(dDatiUte)
008000080424     d                                     dtaara
008100080424
008200080424       //--------------------------------------------------------------
008300080424       //?Definizione strutture dati.
008400080424       //--------------------------------------------------------------
008500080424
008600101217       // -?Status ds?
008700080424     d Status         sds
008800080424     d  SDSpgm           *proc
008900080424     d  SDSprm           *parms
009000080424     d  SDSusr               254    263
009100080428
009200101217       // -?InfDS?
009300080428     d InfDspF         ds
009400080428     d  dsp_aid              369    369a                                        AID byte
009500080428
009600101217       // -?Indicatori su DspF?
009700080428     d IndDspF         ds
009800101217         // -?Emissione messaggio di errore?
009900101217     d   ErrMessage                    n   overlay(IndDspF : 28)
010000101217         // -?Posizionamento cursore & Segnalazione errore?
010100101217     d   PosCurSped                    n   overlay(IndDspF : 50)
010200101217     d   PosCurLNA                     n   overlay(IndDspF : 51)
010300101217     d   PosCurNDC                     n   overlay(IndDspF : 52)
010400101217     d   PosCurDCI                     n   overlay(IndDspF : 53)
010500101217     d   PosCurHCI                     n   overlay(IndDspF : 54)
010600101217     d   PosCurPGM                     n   overlay(IndDspF : 55)
010700101217         // -?Riemissione videata?
010800101217     d   ErrGenerico                   n   overlay(IndDspF : 99)
010900080428
011000101217       // -?Parametro Numero Spedizione?
011100080428     d ds_Spedizione   ds            16    inz
011200101217     d   V1Caas                       4s 0 inz
011300101217     d   V1Clnp                       3s 0 inz
011400101217     d   V1Cnrs                       2s 0 inz
011500101217     d   V1Cnsp                       7s 0 inz
011600101217
011700101217       // -?Parametro Distinta?
011800101217     d ds_Distinta     ds             9    inz
011900101217     d   V1Clna                       3s 0 inz
012000101217     d   V1Cndc                       6s 0 inz
012100101217
012200101217       // -?Parametro Data/Ora creazione immagine?
012300101217     d ds_DataOraCrea  ds            14    inz
012400101217     d   V1Cdci                       8s 0 inz
012500101217     d   V1Chci                       6s 0 inz
012600080428
012700101217       // -?Parametri per Interrogazione bolle arrivi?
012800080428     d Prm_FNLR36ds    ds                  inz
012900080428     d  PA1aas                14     17  0 inz
013000080428     d  PA1lnp                18     20  0 inz
013100080428     d  PA1nrs                21     22  0 inz
013200080428     d  PA1nsp                23     29  0 inz
013300080428     d  PA1f03                30     30    inz
013400080428     d  PA1flg                31     31    inz
013500080428     d  PA1pgm                32     41    inz
013600080428     d  PA1dsa                42     61    inz
013700080428     d  PA1cml                62     62    inz
013800080428     d  PA1dsp                63     70  0 inz
013900080428     d  PA1rmn                71     85  0 inz
014000080428     d  PA1lna                86     88  0 inz
014100080428     d  PA1ln1                89    108    inz
014200080428     d  PA1rsd               109    143    inz
014300101217         // -?Se richiamo da pgm Giac. = 1?
014400080428     d  PA1gia               144    144    inz
014500080428     d  PA1fel               145    147  0 inz
014600080428     d  PA1tbo               148    148    inz
014700080428
014800101217       // -?Parametri ricevuti?
014900080424     d KPJBA         e ds
015000080505
015100101217       // -?Dati LDV da stampare?
015200080505     d FNLSB5ds      e ds                  inz
015300130319     d   DB0tla      e                     inz('L')
015400130311     d FNLSB6ds1     e ds
015500130311     d                                     BASED(nullPtr)
015600080424
015700080424       //--------------------------------------------------------------
015800080424       //?Definizione variabili globali.
015900080424       //--------------------------------------------------------------
016000080424
016100101217       // -?Parametri per FISP30R:?
016200101217       //  ?Stored Procedure x Dati per Immagine LdV + Stampa?
016300080424     **Spedizione******s             16    inz     (vedi ds_Spedizione)
016400101217     **Distinta********s              9    inz     (vedi ds_Distinta)
016500101217     **DataOraCrea*****s             14    inz     (vedi ds_DataOraCrea)
016600110518     **Risultato*******s              2    inz     (vedi *DspF)
016700110518     **RootFolder******s            100    inz     (vedi *DspF)
016800110518     **FileName********s             30    inz     (vedi *DspF)
016900110518     d TemplateName    s             20    inz
017000110518     **LastRow*********s            120            (vedi *DspF)
017100080424     d RtnEsito        s             10i 0 inz
017200110518     **RtnOpCode*******s             10    inz     (vedi *DspF)
017300080424     d RtnStatus       s             10i 0 inz
017400080516     d $Prt            s              1    inz('T')
017500080428
017600101217       // -?Flags booleani?
017700080428     d $Fine           s               n   inz(*off)
017800080429     d $InzD01         s               n   inz(*on)
017900080429
018000101217       // -?Campi di comodo?
018100101217     d wDate           s              8  0 inz
018200101217     d wData_Iso       s               d   inz  datfmt(*iso)
018300080710     d SAVrisult       s                   inz  like(Risultato)
018400080506     d SAVpgm          s                   inz  like(V1Cpgm)
018500080424
018600080424       //--------------------------------------------------------------
018700080424       //?Definizione procedure usate.
018800080424       //--------------------------------------------------------------
018900080424
019000101217       // -?Reperimento dati utente?
019100101217     d TIBS34ds      e ds
019200101217      /copy gaitrasrc/srcProtoPR,TIBS34R
019300080429
019400101217       // -?Interrogazione bolle arrivi?
019500101217      /copy gaitrasrc/srcProtoPR,FNLR36R
019600080424
019700101217       // -?Stored Procedure per immagine LdV?
019800080430     d fisp30r         pr                  extpgm('FISP30R')
019900080424     d   P_Spedizione...
020000080424     d                                     like(ds_Spedizione)
020100101217     d   P_Distinta...
020200101217     d                                     like(ds_Distinta)
020300101217     d   P_DataOraCrea...
020400101217     d                                     like(ds_DataOraCrea)
020500080731     d   P_Risultato...
020600080429     d                                     like(Risultato)
020700080731     d   P_RootFolder...
020800080715     d                                     like(RootFolder)
020900080731     d   P_FileName...
021000080519     d                                     like(FileName)
021100080731     d   P_TemplateName...
021200080731     d                                     like(TemplateName)
021300110201     d   P_LastRow...
021400110201     d                                     like(LastRow)
021500080424     d   P_RtnEsito...
021600080424     d                                     like(RtnEsito)
021700080424     d   P_RtnOpCode...
021800080424     d                                     like(RtnOpCode)
021900080424     d   P_RtnStatus...
022000080424     d                                     like(RtnStatus)
022100080516     d   P_Prt...
022200080516     d                                     like($Prt)
022300101221     d fisp30r£        pr                  extpgm('FISP30R£')
022400080516     d   P_Spedizione...
022500080516     d                                     like(ds_Spedizione)
022600101217     d   P_Distinta...
022700101217     d                                     like(ds_Distinta)
022800101217     d   P_DataOraCrea...
022900101217     d                                     like(ds_DataOraCrea)
023000080731     d   P_Risultato...
023100080516     d                                     like(Risultato)
023200080731     d   P_RootFolder...
023300080715     d                                     like(RootFolder)
023400080731     d   P_FileName...
023500080516     d                                     like(FileName)
023600080731     d   P_TemplateName...
023700080731     d                                     like(TemplateName)
023800110201     d   P_LastRow...
023900110201     d                                     like(LastRow)
024000080516     d   P_RtnEsito...
024100080516     d                                     like(RtnEsito)
024200080516     d   P_RtnOpCode...
024300080516     d                                     like(RtnOpCode)
024400080516     d   P_RtnStatus...
024500080516     d                                     like(RtnStatus)
024600080516     d   P_Prt...
024700080516     d                                     like($Prt)
024800100422     d fisp30r___      pr                  extpgm('FISP30R___')
024900080908     d   P_Spedizione...
025000080908     d                                     like(ds_Spedizione)
025100101217     d   P_Distinta...
025200101217     d                                     like(ds_Distinta)
025300101217     d   P_DataOraCrea...
025400101217     d                                     like(ds_DataOraCrea)
025500080908     d   P_Risultato...
025600080908     d                                     like(Risultato)
025700080908     d   P_RootFolder...
025800080908     d                                     like(RootFolder)
025900080908     d   P_FileName...
026000080908     d                                     like(FileName)
026100080908     d   P_TemplateName...
026200080908     d                                     like(TemplateName)
026300110201     d   P_LastRow...
026400110201     d                                     like(LastRow)
026500080908     d   P_RtnEsito...
026600080908     d                                     like(RtnEsito)
026700080908     d   P_RtnOpCode...
026800080908     d                                     like(RtnOpCode)
026900080908     d   P_RtnStatus...
027000080908     d                                     like(RtnStatus)
027100080908     d   P_Prt...
027200080908     d                                     like($Prt)
027300101217       // -?Stampa LdV?
027400090218     d fnlsb6r         pr                  extpgm('FNLSB6R')
027500090218     d   fnlsb5ds...
027600090218     d                                     likeds(FNLSB5ds)
027700130311     d   fnlsB6ds1...
027800130311     d                                     likeds(FNLSB6ds1)
027900130311     d                                       options( *nopass :
028000130311     d                                                *omit )
028100101221
028200101221       // -?Parametri API QCAPCMD (Process Commands)?
028300101221     d Qcmd            s           2048    inz  varying
028400101221      /copy qSysInc/qRpgleSrc,QCAPCMD
028500101221       // -?API QCAPCMD (Process Commands)?
028600101221      /copy gaitrasrc/srcProtoPR,QCAPCMD
028700101221
028800101221       // -?Parametri gestione errori API.?
028900101221      /copy qsysinc/qrpglesrc,QUSEC
029000080424
029100080424       //--------------------------------------------------------------
029200080424       //?Definizione key-list.
029300080424       //--------------------------------------------------------------
029400080424
029500101217       // -?File FNARB01L?
029600080428     d k04fnarb01    e ds                  extname(FNARB01L:*key)
029700080428     d                                     prefix(k_)
029800080428     d                                     inz
029900080424
030000080424       //--------------------------------------------------------------
030100080424       //?Riepilogo indicatori.
030200080424       //--------------------------------------------------------------
030300080424
030400101221
030500101221       //--------------------------------------------------------------
030600101221       //?M A I N - L I N E                                            ?
030700101221       //--------------------------------------------------------------
030800101221
030900080424     c     *Entry        plist
031000080424     c                   parm                    KPJBA
031100080424
031200080424      /free
031300080424
031400101217       // -?Start?
031500080424       *inLR = *on;
031600080424
031700101217       // -?Elab?
031800080428       dow  $Fine = *off;
031900080428         exsr sr_GesD01;
032000080428       enddo;
032100080424
032200101217       // -?End?
032300080424       return;
032400080424
032500080424       //--------------------------------------------------------------
032600080424       //?Operazioni iniziali
032700080424       //--------------------------------------------------------------
032800101217       BEGSR  *InzSR;
032900080424
033000101217         // -?Reperimento dati job?
033100101217         exsr  sr_DatiJob;
033200080506
033300101217         // -?Impostazione nome programma a video?
033400080506         VTDpgm = SDSpgm;
033500110621
033600110621         // -?Apertura archivio FNARB?
033700110621         if  %subst( knsif : 1 : 3 ) = 'FIL';
033800110621           open  FNARB01L;
033900110621         else;
034000110621           clear  SP30D01;
034100110621           ErrGenerico = *on;
034200110621           ErrMessage  = *on;
034300110621           V1Dmsg  = 'Funzione eseguibile SOLO in filiale. +
034400110621                      Premere Invio per tornare a menù.';
034500110621           $InzD01 = *off;
034600110621           $Fine   = *on;
034700110621           write  SP30T01;
034800110621           write  SP30Z01;
034900110621           exfmt  SP30D01;
035000110621         endif;
035100080424
035200080424       ENDSR;
035300080424
035400080424       //--------------------------------------------------------------
035500080424       //?Reperimento Dati del job (Utente/Operativi).
035600080424       //--------------------------------------------------------------
035700101217       BEGSR  sr_DatiJob;
035800080424
035900080424         in(E) §AzUte;
036000080424         if NOT %error;
036100080424           in(E) §DatiUte;
036200080424         endif;
036300080424         if %error or RSut = *blanks;
036400080424           clear TIBS34ds;
036500080424           tibs34r(tibs34ds);
036600080424           in §AzUte;
036700080424           in §DatiUte;
036800080424         endif;
036900080424
037000080424       ENDSR;
037100080428
037200080428       //--------------------------------------------------------------
037300080428       //?Gestione videata D01
037400080428       //--------------------------------------------------------------
037500101217       BEGSR  sr_GesD01;
037600080428
037700101217         // -?Inizializzazione videata?
037800080428         if  $InzD01 = *on;
037900101217           SAVpgm  = V1Cpgm;
038000110518           clear  SP30D01;
038100110518           clear  SP30D02;
038200101217           V1Caas  = *year;
038300080506           if  SAVpgm = *blank;
038400101217             V1Cpgm = 'FISP30R   ';
038500080506           else;
038600101217             V1Cpgm = SAVpgm;
038700080506           endif;
038800101217           $InzD01 = *off;
038900080428         endif;
039000080428
039100101217         // -?Emissione videata?
039200101217         if  Not  ErrMessage;
039300101217           write  SP30T01;
039400101217           write  SP30Z01;
039500080428         endif;
039600080514
039700101217         exfmt  SP30D01;
039800080514
039900101217         reset  ErrMessage;
040000101217         reset  ErrGenerico;
040100101217         clear  V1Dmsg;
040200080514
040300101217         clear  V1Dtxt;
040400080428
040500080428         SELECT;
040600080428
040700101217           // -?F3=Fine?
040800080428           WHEN  dsp_aid = c_F03;
040900080428             $Fine = *on;
041000080428
041100101217           // -?F4=Interrogazione bolle arrivi?
041200080428           WHEN  dsp_aid = c_F04;
041300101217             exsr  sr_Call_FNLR36R;
041400101221
041500110207           // -?F9=Visualizzazione WrkF FIILV00F?
041600101221           WHEN  dsp_aid = c_F09;
041700101221             exsr  sr_F09D01;
041800130111
041900130911           // -?F18=Visualizzazione WrkF FIIRD10F?
042000130111           WHEN  dsp_aid = c_F18;
042100130111             exsr  sr_F18D01;
042200080428
042300101217           // -?Invio o F6=Conferma?
042400080428           OTHER;
042500101217             exsr  sr_CtrD01;
042600080429             select;
042700101217               // -?rilevato errore?
042800080429               when  ErrGenerico = *on;
042900101217               // -?non premuto F6?
043000080429               when  dsp_aid <> c_F06;
043100101217               // -?selezionata una singola spedizione?
043200080429               when  V1Clnp  <> *zero   and
043300080429                     V1Cnsp  <> *zero;
043400101217                 exsr  sr_Call_FISP30R;
043500101217                 if  Not ErrGenerico;
043600101217                   reset $InzD01;
043700101217                 endif;
043800080429             endsl;
043900080428
044000080428         ENDSL;
044100080428
044200080428       ENDSR;
044300080424
044400080424       //--------------------------------------------------------------
044500080424       //?Richiamo pgm. Interrogazione Bolle Arrivi.
044600080424       //--------------------------------------------------------------
044700101217       BEGSR  sr_Call_FNLR36R;
044800080424
044900101217         clear  Prm_FNLR36ds;
045000080424         PA1flg = '2';
045100080424         PA1pgm = SDSpgm;
045200080424         if  Pa1Fel <= *zero;
045300080424           PA1dsa = RsUt;
045400080424           PA1fel = DUTtfp;
045500080424         endif;
045600080424         kpjbu  = Prm_FNLR36ds;
045700080424
045800080424         fnlr36r (kpjba);
045900080424
046000080424         Prm_FNLR36ds = kpjbu;
046100080424
046200080424         if  Pa1F03 = *on;
046300080424           leavesr;
046400080424         endif;
046500080424
046600101217         clear  ds_Spedizione;
046700080428         V1Caas = PA1aas;
046800080428         V1Clnp = PA1lnp;
046900080428         V1Cnrs = PA1nrs;
047000080428         V1Cnsp = PA1nsp;
047100080424
047200080424       ENDSR;
047300101221
047400101221       //--------------------------------------------------------------
047500101221       //?Gestione tasto funzionale F09 da videata D01.                ?
047600101221       //--------------------------------------------------------------
047700101221       BEGSR  sr_F09D01;
047800101221
047900110207         Qcmd = 'DSPDBF file(*libl/FIILV00F) mbr(*first) output(*)';
048000101221         exsr  sr_ExecCmd;
048100101221
048200101221       ENDSR;
048300130111
048400130111       //--------------------------------------------------------------
048500130111       //?Gestione tasto funzionale F18 da videata D01.                ?
048600130111       //--------------------------------------------------------------
048700130111       BEGSR  sr_F18D01;
048800130111
048900130911         Qcmd = 'DSPDBF file(*libl/FIIRD10F) mbr(*first) output(*)';
049000130111         exsr  sr_ExecCmd;
049100130111
049200130111       ENDSR;
049300080428
049400080428       //--------------------------------------------------------------
049500080428       //?Richiamo stored procedure per immagini LdV
049600080428       //--------------------------------------------------------------
049700101217       BEGSR  sr_Call_FISP30R;
049800080428
049900080505         select;
050000081022
050100101217           when  V1Cpgm = 'FISP30R   ';
050200080516             reset  $Prt;
050300101217             fisp30r (ds_Spedizione : ds_Distinta : ds_DataOraCrea :
050400101217                      Risultato :
050500101217                      RootFolder : FileName : TemplateName :
050600110201                      LastRow :
050700080516                      RtnEsito : RtnOpCode : RtnStatus : $Prt);
050800080710             SavRisult = Risultato;
050900081022
051000101221           when  V1Cpgm = 'FISP30R£  ';
051100080516             reset  $Prt;
051200101221             fisp30r£ (ds_Spedizione : ds_Distinta : ds_DataOraCrea :
051300101221                       Risultato :
051400101221                       RootFolder : FileName : TemplateName :
051500110201                       LastRow :
051600101221                       RtnEsito : RtnOpCode : RtnStatus : $Prt);
051700080710             SavRisult = Risultato;
051800081022
051900101217           when  V1Cpgm = 'FISP30R___';
052000080908             reset  $Prt;
052100101217             fisp30r___ (ds_Spedizione : ds_Distinta : ds_DataOraCrea :
052200101217                         Risultato :
052300080908                         RootFolder : FileName : TemplateName :
052400130111                         LastRow :
052500080908                         RtnEsito : RtnOpCode : RtnStatus : $Prt);
052600080908             SavRisult = Risultato;
052700090218
052800101217           when  V1Cpgm = 'FNLSB6R   ';
052900130319             reset  fnlsb5ds;
053000090218             DB0tbo = 'A';
053100090218             DB0aas = V1Caas;
053200090218             DB0lnp = V1Clnp;
053300090218             DB0nrs = V1Cnrs;
053400090218             DB0nsp = V1Cnsp;
053500090218             fnlsb6r (fnlsb5ds);
053600101217
053700101217           other;
053800101217             ErrMessage  = *on;
053900101217             ErrGenerico = *on;
054000101217             PosCurPGM   = *on;
054100101217             V1Dmsg = $Msg(03);
054200101217             leavesr;
054300081022
054400080505         endsl;
054500080514
054600101210         if  V1Cpgm = 'FNLSB6R ';
054700130430           select;
054800130430             when  DB0tms = 'E';
054900130430               V1Dtxt = 'Rilevato errore in fase di stampa';
055000130430             when  DB0tms = 'D';
055100130430               V1Dtxt = 'LdV NON stampata: risulta A4, mentre era +
055200130430                         richiesto un A5';
055300130430             other;
055400130430               V1Dtxt = 'Generata stampa della spedizione '
055500130430                      + %editc( V1Clnp : 'X' ) + '/'
055600130430                      + %editc( V1Cnrs : 'X' ) + '/'
055700130430                      + %editc( V1Cnsp : 'Z' )
055800130430                      + ' anno ' + %editc( V1Caas : 'Z');
055900130430           endsl;
056000080710         else;
056100081022           V1Dtxt = 'Risultato: ' + SavRisult + '   per la spedizione '
056200080710                  + %editc( V1Clnp : 'X' ) + '/'
056300080710                  + %editc( V1Cnrs : 'X' ) + '/'
056400080710                  + %editc( V1Cnsp : 'Z' )
056500080710                  + ' anno ' + %editc( V1Caas : 'Z');
056600110518           vTemplateN = TemplateName;
056700110518           vRtnEsito  = RtnEsito;
056800110518           vRtnStatus = RtnStatus;
056900080710         endif;
057000110518
057100081022         write  Protect;
057200081022         exfmt  SP30D02;
057300081022
057400101217         // -?F3=Fine?
057500081022         if  dsp_aid = c_F03;
057600081022           $Fine = *on;
057700081022           leavesr;
057800081022         endif;
057900080428
058000080428       ENDSR;
058100080428
058200080428       //--------------------------------------------------------------
058300080428       //?Controllo dati in videata D01
058400080428       //--------------------------------------------------------------
058500101217       BEGSR  sr_CtrD01;
058600080429
058700101217         %subst(IndDspF : 50) = *off;
058800080429
058900080514         select;
059000080514           when  V1Caas  = *zero;
059100080514             V1Caas  = *year;
059200080514           when  V1Caas  > 100;
059300080514           when  V1Caas  > 50;
059400080514             V1Caas += 1900;
059500080514           when  V1Caas <= 50;
059600080514             V1Caas += 2000;
059700080514         endsl;
059800080428
059900080428         SELECT;
060000080428
060100101217           // -?Nessuna spedizione inserita?
060200080514           WHEN  V1Clnp = *zero   and
060300080428                 V1Cnrs = *zero   and
060400080514                 V1Cnsp = *zero;
060500080428             ErrMessage  = *on;
060600080428             ErrGenerico = *on;
060700080428             PosCurSped  = *on;
060800080428             V1Dmsg = $Msg(01);
060900080429             leavesr;
061000080428
061100101217           // -?Selezionata una singola spedizione?
061200080428           WHEN  V1Clnp <> *zero   or
061300080428                 V1Cnrs <> *zero   or
061400080428                 V1Cnsp <> *zero;
061500080428             k_ARBaas = V1Caas;
061600080428             k_ARBlnp = V1Clnp;
061700080428             k_ARBnrs = V1Cnrs;
061800080428             k_ARBnsp = V1Cnsp;
061900101217             chain  %kds(K04fnarb01) FNARB000;
062000101217             if not %found(FNARB01L);
062100080428               ErrMessage  = *on;
062200080428               ErrGenerico = *on;
062300080428               PosCurSped  = *on;
062400080514               V1Dmsg = $Msg(02);
062500080429               leavesr;
062600080428             endif;
062700080428
062800080428         ENDSL;
062900101217
063000101217         // -?Reperimento n° distinta?
063100121015         if  ds_Distinta = *zero;
063200121015           V1Clna = ARBlna;
063300121015           V1Cndc = ARBndc;
063400101217         endif;
063500121015
063600121015         // -?Impostazione Data e Ora creazione immagine?
063700121015         if  ds_DataOraCrea = *zero;
063800121015           V1Cdci = %int( %subst( %char( %dec( %timestamp() ) ) : 1 : 8 ) );
063900121015           V1Chci = %int( %subst( %char( %dec( %timestamp() ) ) : 9 : 6 ) );
064000121015         endif;
064100080506
064200101217         // -?Programma da richiamare di default?
064300080506         if  V1Cpgm = *blank;
064400080506           V1Cpgm   = 'FISP30R   ';
064500080506         endif;
064600080428
064700080428       ENDSR;
064800101221
064900101221       //--------------------------------------------------------------
065000101221       //?Esecuzione del comando (già impostato).                      ?
065100101221       //--------------------------------------------------------------
065200101221       BEGSR  sr_ExecCmd;
065300101221
065400101221         clear Qcap0100;
065500101221         Qcabcsdh = *off;
065600101221         Qcapa    = *off;
065700101221         Qcacmdss = *off;
065800101221         Qcaerved = *allX'00';
065900101221
066000101221         clear Qusec;
066100101221         Qusbprv  = %size(Qusec);
066200101221
066300101221         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
066400101221                           %size(Qcap0100) : 'CPOP0100' : *omit :
066500101221                           0 : 0 : Qusec);
066600101221
066700101221         //if  Qusei <> *blank;
066800101221         //  ...;
066900101221         //endif;
067000101221
067100101221       ENDSR;
067200080424
067300080424      /end-free
067400101221
067500101221       //--------------------------------------------------------------
067600101221       //?Schiere a tempo di compilazione.
067700101221       //--------------------------------------------------------------
067800080428
067900101217** - $Msg:?Messaggi di Errore?-----------------------------------------------*
068000101217Spedizione obbligatoria                                                          1
068100080514Spedizione inesistente                                                           2
068200101217Programma NON previsto                                                           3
