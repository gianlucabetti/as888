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
016100170728       // -?Parametri per FISP30R/FISPG30R:?
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
022300170728     d fispG30r        pr                  extpgm('FISPG30R')
022400170728     d   P_Spedizione...
022500170728     d                                     like(ds_Spedizione)
022600170728     d   P_Distinta...
022700170728     d                                     like(ds_Distinta)
022800170728     d   P_DataOraCrea...
022900170728     d                                     like(ds_DataOraCrea)
023000170728     d   P_Risultato...
023100170728     d                                     like(Risultato)
023200170728     d   P_RootFolder...
023300170728     d                                     like(RootFolder)
023400170728     d   P_FileName...
023500170728     d                                     like(FileName)
023600170728     d   P_TemplateName...
023700170728     d                                     like(TemplateName)
023800170728     d   P_LastRow...
023900170728     d                                     like(LastRow)
024000170728     d   P_RtnEsito...
024100170728     d                                     like(RtnEsito)
024200170728     d   P_RtnOpCode...
024300170728     d                                     like(RtnOpCode)
024400170728     d   P_RtnStatus...
024500170728     d                                     like(RtnStatus)
024600170728     d   P_Prt...
024700170728     d                                     like($Prt)
024800101217       // -?Stampa LdV?
024900090218     d fnlsb6r         pr                  extpgm('FNLSB6R')
025000090218     d   fnlsb5ds...
025100090218     d                                     likeds(FNLSB5ds)
025200130311     d   fnlsB6ds1...
025300130311     d                                     likeds(FNLSB6ds1)
025400130311     d                                       options( *nopass :
025500130311     d                                                *omit )
025600101221
025700101221       // -?Parametri API QCAPCMD (Process Commands)?
025800101221     d Qcmd            s           2048    inz  varying
025900101221      /copy qSysInc/qRpgleSrc,QCAPCMD
026000101221       // -?API QCAPCMD (Process Commands)?
026100101221      /copy gaitrasrc/srcProtoPR,QCAPCMD
026200101221
026300101221       // -?Parametri gestione errori API.?
026400101221      /copy qsysinc/qrpglesrc,QUSEC
026500080424
026600080424       //--------------------------------------------------------------
026700080424       //?Definizione key-list.
026800080424       //--------------------------------------------------------------
026900080424
027000101217       // -?File FNARB01L?
027100080428     d k04fnarb01    e ds                  extname(FNARB01L:*key)
027200080428     d                                     prefix(k_)
027300080428     d                                     inz
027400080424
027500080424       //--------------------------------------------------------------
027600080424       //?Riepilogo indicatori.
027700080424       //--------------------------------------------------------------
027800080424
027900101221
028000101221       //--------------------------------------------------------------
028100101221       //?M A I N - L I N E                                            ?
028200101221       //--------------------------------------------------------------
028300101221
028400080424     c     *Entry        plist
028500080424     c                   parm                    KPJBA
028600080424
028700080424      /free
028800080424
028900101217       // -?Start?
029000080424       *inLR = *on;
029100080424
029200101217       // -?Elab?
029300080428       dow  $Fine = *off;
029400080428         exsr sr_GesD01;
029500080428       enddo;
029600080424
029700101217       // -?End?
029800080424       return;
029900080424
030000080424       //--------------------------------------------------------------
030100080424       //?Operazioni iniziali
030200080424       //--------------------------------------------------------------
030300101217       BEGSR  *InzSR;
030400080424
030500101217         // -?Reperimento dati job?
030600101217         exsr  sr_DatiJob;
030700080506
030800101217         // -?Impostazione nome programma a video?
030900080506         VTDpgm = SDSpgm;
031000110621
031100110621         // -?Apertura archivio FNARB?
031200110621         if  %subst( knsif : 1 : 3 ) = 'FIL';
031300110621           open  FNARB01L;
031400110621         else;
031500110621           clear  SP30D01;
031600110621           ErrGenerico = *on;
031700110621           ErrMessage  = *on;
031800110621           V1Dmsg  = 'Funzione eseguibile SOLO in filiale. +
031900110621                      Premere Invio per tornare a menù.';
032000110621           $InzD01 = *off;
032100110621           $Fine   = *on;
032200110621           write  SP30T01;
032300110621           write  SP30Z01;
032400110621           exfmt  SP30D01;
032500110621         endif;
032600080424
032700080424       ENDSR;
032800080424
032900080424       //--------------------------------------------------------------
033000080424       //?Reperimento Dati del job (Utente/Operativi).
033100080424       //--------------------------------------------------------------
033200101217       BEGSR  sr_DatiJob;
033300080424
033400080424         in(E) §AzUte;
033500080424         if NOT %error;
033600080424           in(E) §DatiUte;
033700080424         endif;
033800080424         if %error or RSut = *blanks;
033900080424           clear TIBS34ds;
034000080424           tibs34r(tibs34ds);
034100080424           in §AzUte;
034200080424           in §DatiUte;
034300080424         endif;
034400080424
034500080424       ENDSR;
034600080428
034700080428       //--------------------------------------------------------------
034800080428       //?Gestione videata D01
034900080428       //--------------------------------------------------------------
035000101217       BEGSR  sr_GesD01;
035100080428
035200101217         // -?Inizializzazione videata?
035300080428         if  $InzD01 = *on;
035400101217           SAVpgm  = V1Cpgm;
035500110518           clear  SP30D01;
035600110518           clear  SP30D02;
035700101217           V1Caas  = *year;
035800080506           if  SAVpgm = *blank;
035900170728             V1Cpgm = 'FISPG30R  ';
036000080506           else;
036100101217             V1Cpgm = SAVpgm;
036200080506           endif;
036300101217           $InzD01 = *off;
036400080428         endif;
036500080428
036600101217         // -?Emissione videata?
036700101217         if  Not  ErrMessage;
036800101217           write  SP30T01;
036900101217           write  SP30Z01;
037000080428         endif;
037100080514
037200101217         exfmt  SP30D01;
037300080514
037400101217         reset  ErrMessage;
037500101217         reset  ErrGenerico;
037600101217         clear  V1Dmsg;
037700080514
037800101217         clear  V1Dtxt;
037900080428
038000080428         SELECT;
038100080428
038200101217           // -?F3=Fine?
038300080428           WHEN  dsp_aid = c_F03;
038400080428             $Fine = *on;
038500080428
038600101217           // -?F4=Interrogazione bolle arrivi?
038700080428           WHEN  dsp_aid = c_F04;
038800101217             exsr  sr_Call_FNLR36R;
038900101221
039000110207           // -?F9=Visualizzazione WrkF FIILV00F?
039100101221           WHEN  dsp_aid = c_F09;
039200101221             exsr  sr_F09D01;
039300130111
039400130911           // -?F18=Visualizzazione WrkF FIIRD10F?
039500130111           WHEN  dsp_aid = c_F18;
039600130111             exsr  sr_F18D01;
039700080428
039800101217           // -?Invio o F6=Conferma?
039900080428           OTHER;
040000101217             exsr  sr_CtrD01;
040100080429             select;
040200101217               // -?rilevato errore?
040300080429               when  ErrGenerico = *on;
040400101217               // -?non premuto F6?
040500080429               when  dsp_aid <> c_F06;
040600101217               // -?selezionata una singola spedizione?
040700080429               when  V1Clnp  <> *zero   and
040800080429                     V1Cnsp  <> *zero;
040900101217                 exsr  sr_Call_FISP30R;
041000101217                 if  Not ErrGenerico;
041100101217                   reset $InzD01;
041200101217                 endif;
041300080429             endsl;
041400080428
041500080428         ENDSL;
041600080428
041700080428       ENDSR;
041800080424
041900080424       //--------------------------------------------------------------
042000080424       //?Richiamo pgm. Interrogazione Bolle Arrivi.
042100080424       //--------------------------------------------------------------
042200101217       BEGSR  sr_Call_FNLR36R;
042300080424
042400101217         clear  Prm_FNLR36ds;
042500080424         PA1flg = '2';
042600080424         PA1pgm = SDSpgm;
042700080424         if  Pa1Fel <= *zero;
042800080424           PA1dsa = RsUt;
042900080424           PA1fel = DUTtfp;
043000080424         endif;
043100080424         kpjbu  = Prm_FNLR36ds;
043200080424
043300080424         fnlr36r (kpjba);
043400080424
043500080424         Prm_FNLR36ds = kpjbu;
043600080424
043700080424         if  Pa1F03 = *on;
043800080424           leavesr;
043900080424         endif;
044000080424
044100101217         clear  ds_Spedizione;
044200080428         V1Caas = PA1aas;
044300080428         V1Clnp = PA1lnp;
044400080428         V1Cnrs = PA1nrs;
044500080428         V1Cnsp = PA1nsp;
044600080424
044700080424       ENDSR;
044800101221
044900101221       //--------------------------------------------------------------
045000101221       //?Gestione tasto funzionale F09 da videata D01.                ?
045100101221       //--------------------------------------------------------------
045200101221       BEGSR  sr_F09D01;
045300101221
045400110207         Qcmd = 'DSPDBF file(*libl/FIILV00F) mbr(*first) output(*)';
045500101221         exsr  sr_ExecCmd;
045600101221
045700101221       ENDSR;
045800130111
045900130111       //--------------------------------------------------------------
046000130111       //?Gestione tasto funzionale F18 da videata D01.                ?
046100130111       //--------------------------------------------------------------
046200130111       BEGSR  sr_F18D01;
046300130111
046400130911         Qcmd = 'DSPDBF file(*libl/FIIRD10F) mbr(*first) output(*)';
046500130111         exsr  sr_ExecCmd;
046600130111
046700130111       ENDSR;
046800080428
046900080428       //--------------------------------------------------------------
047000080428       //?Richiamo stored procedure per immagini LdV
047100080428       //--------------------------------------------------------------
047200101217       BEGSR  sr_Call_FISP30R;
047300080428
047400080505         select;
047500081022
047600101217           when  V1Cpgm = 'FISP30R   ';
047700080516             reset  $Prt;
047800101217             fisp30r (ds_Spedizione : ds_Distinta : ds_DataOraCrea :
047900101217                      Risultato :
048000101217                      RootFolder : FileName : TemplateName :
048100110201                      LastRow :
048200080516                      RtnEsito : RtnOpCode : RtnStatus : $Prt);
048300080710             SavRisult = Risultato;
048400081022
048500170728           when  V1Cpgm = 'FISPG30R  ';
048600080516             reset  $Prt;
048700170728             fispG30r (ds_Spedizione : ds_Distinta : ds_DataOraCrea :
048800101221                       Risultato :
048900101221                       RootFolder : FileName : TemplateName :
049000110201                       LastRow :
049100101221                       RtnEsito : RtnOpCode : RtnStatus : $Prt);
049200080710             SavRisult = Risultato;
049300090218
049400101217           when  V1Cpgm = 'FNLSB6R   ';
049500130319             reset  fnlsb5ds;
049600090218             DB0tbo = 'A';
049700090218             DB0aas = V1Caas;
049800090218             DB0lnp = V1Clnp;
049900090218             DB0nrs = V1Cnrs;
050000090218             DB0nsp = V1Cnsp;
050100090218             fnlsb6r (fnlsb5ds);
050200101217
050300101217           other;
050400101217             ErrMessage  = *on;
050500101217             ErrGenerico = *on;
050600101217             PosCurPGM   = *on;
050700101217             V1Dmsg = $Msg(03);
050800101217             leavesr;
050900081022
051000080505         endsl;
051100080514
051200101210         if  V1Cpgm = 'FNLSB6R ';
051300130430           select;
051400130430             when  DB0tms = 'E';
051500130430               V1Dtxt = 'Rilevato errore in fase di stampa';
051600130430             when  DB0tms = 'D';
051700130430               V1Dtxt = 'LdV NON stampata: risulta A4, mentre era +
051800130430                         richiesto un A5';
051900130430             other;
052000130430               V1Dtxt = 'Generata stampa della spedizione '
052100130430                      + %editc( V1Clnp : 'X' ) + '/'
052200130430                      + %editc( V1Cnrs : 'X' ) + '/'
052300130430                      + %editc( V1Cnsp : 'Z' )
052400130430                      + ' anno ' + %editc( V1Caas : 'Z');
052500130430           endsl;
052600080710         else;
052700081022           V1Dtxt = 'Risultato: ' + SavRisult + '   per la spedizione '
052800080710                  + %editc( V1Clnp : 'X' ) + '/'
052900080710                  + %editc( V1Cnrs : 'X' ) + '/'
053000080710                  + %editc( V1Cnsp : 'Z' )
053100080710                  + ' anno ' + %editc( V1Caas : 'Z');
053200110518           vTemplateN = TemplateName;
053300110518           vRtnEsito  = RtnEsito;
053400110518           vRtnStatus = RtnStatus;
053500080710         endif;
053600110518
053700081022         write  Protect;
053800081022         exfmt  SP30D02;
053900081022
054000101217         // -?F3=Fine?
054100081022         if  dsp_aid = c_F03;
054200081022           $Fine = *on;
054300081022           leavesr;
054400081022         endif;
054500080428
054600080428       ENDSR;
054700080428
054800080428       //--------------------------------------------------------------
054900080428       //?Controllo dati in videata D01
055000080428       //--------------------------------------------------------------
055100101217       BEGSR  sr_CtrD01;
055200080429
055300101217         %subst(IndDspF : 50) = *off;
055400080429
055500080514         select;
055600080514           when  V1Caas  = *zero;
055700080514             V1Caas  = *year;
055800080514           when  V1Caas  > 100;
055900080514           when  V1Caas  > 50;
056000080514             V1Caas += 1900;
056100080514           when  V1Caas <= 50;
056200080514             V1Caas += 2000;
056300080514         endsl;
056400080428
056500080428         SELECT;
056600080428
056700101217           // -?Nessuna spedizione inserita?
056800080514           WHEN  V1Clnp = *zero   and
056900080428                 V1Cnrs = *zero   and
057000080514                 V1Cnsp = *zero;
057100080428             ErrMessage  = *on;
057200080428             ErrGenerico = *on;
057300080428             PosCurSped  = *on;
057400080428             V1Dmsg = $Msg(01);
057500080429             leavesr;
057600080428
057700101217           // -?Selezionata una singola spedizione?
057800080428           WHEN  V1Clnp <> *zero   or
057900080428                 V1Cnrs <> *zero   or
058000080428                 V1Cnsp <> *zero;
058100080428             k_ARBaas = V1Caas;
058200080428             k_ARBlnp = V1Clnp;
058300080428             k_ARBnrs = V1Cnrs;
058400080428             k_ARBnsp = V1Cnsp;
058500101217             chain  %kds(K04fnarb01) FNARB000;
058600101217             if not %found(FNARB01L);
058700080428               ErrMessage  = *on;
058800080428               ErrGenerico = *on;
058900080428               PosCurSped  = *on;
059000080514               V1Dmsg = $Msg(02);
059100080429               leavesr;
059200080428             endif;
059300080428
059400080428         ENDSL;
059500101217
059600101217         // -?Reperimento n° distinta?
059700121015         if  ds_Distinta = *zero;
059800121015           V1Clna = ARBlna;
059900121015           V1Cndc = ARBndc;
060000101217         endif;
060100121015
060200121015         // -?Impostazione Data e Ora creazione immagine?
060300121015         if  ds_DataOraCrea = *zero;
060400121015           V1Cdci = %int( %subst( %char( %dec( %timestamp() ) ) : 1 : 8 ) );
060500121015           V1Chci = %int( %subst( %char( %dec( %timestamp() ) ) : 9 : 6 ) );
060600121015         endif;
060700080506
060800101217         // -?Programma da richiamare di default?
060900080506         if  V1Cpgm = *blank;
061000170728           V1Cpgm   = 'FISPG30R  ';
061100080506         endif;
061200080428
061300080428       ENDSR;
061400101221
061500101221       //--------------------------------------------------------------
061600101221       //?Esecuzione del comando (già impostato).                      ?
061700101221       //--------------------------------------------------------------
061800101221       BEGSR  sr_ExecCmd;
061900101221
062000101221         clear Qcap0100;
062100101221         Qcabcsdh = *off;
062200101221         Qcapa    = *off;
062300101221         Qcacmdss = *off;
062400101221         Qcaerved = *allX'00';
062500101221
062600101221         clear Qusec;
062700101221         Qusbprv  = %size(Qusec);
062800101221
062900101221         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
063000101221                           %size(Qcap0100) : 'CPOP0100' : *omit :
063100101221                           0 : 0 : Qusec);
063200101221
063300101221         //if  Qusei <> *blank;
063400101221         //  ...;
063500101221         //endif;
063600101221
063700101221       ENDSR;
063800080424
063900080424      /end-free
064000101221
064100101221       //--------------------------------------------------------------
064200101221       //?Schiere a tempo di compilazione.
064300101221       //--------------------------------------------------------------
064400080428
064500101217** - $Msg:?Messaggi di Errore?-----------------------------------------------*
064600101217Spedizione obbligatoria                                                          1
064700080514Spedizione inesistente                                                           2
064800101217Programma NON previsto                                                           3
