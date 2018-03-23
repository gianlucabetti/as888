000100080206      //--------------------------------------------------------------
000200110706      //?TRMKQ4R - Modifica Info Trattative + SIC x sistemazione errori
000300080206      //--------------------------------------------------------------
000400080206
000500091124     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600090807     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100110930
001200110930       // -?File INFO trattative
001300110930     fTIVII01L  if   e           k disk
001400110706
001500110706       // -?File trattative
001600110804     fTIVIS05L  if   e           k disk
001700110706
001800110706       // -?File per SIC
001900110706     fTIVIIC0F  o    e             disk
002000110706
002100110706       // -?File video
002200110706     fTRMKQ4D   cf   e             workstn
002300080208     f                                     indds(IndDspF)
002400080206     f                                     infds(InfDspF)
002500080206
002600080206      //---------------------------------------------------------------
002700090406      //?Definizione costanti.
002800080206      //---------------------------------------------------------------
002900080207
003000110706       // -?Tasti funzionali a video
003100080207     d c_F01           c                   const(x'31')
003200080207     d c_F02           c                   const(x'32')
003300080207     d c_F03           c                   const(x'33')
003400090406     d c_F04           c                   const(x'34')
003500080207     d c_F05           c                   const(x'35')
003600080207     d c_F06           c                   const(x'36')
003700080207     d c_F07           c                   const(x'37')
003800080207     d c_F08           c                   const(x'38')
003900080207     d c_F09           c                   const(x'39')
004000080207     d c_F10           c                   const(x'3A')
004100090303     d c_F11           c                   const(x'3B')
004200080207     d c_F12           c                   const(x'3C')
004300080207     d c_F13           c                   const(x'B1')
004400080207     d c_F14           c                   const(x'B2')
004500080207     d c_F15           c                   const(x'B3')
004600080207     d c_F16           c                   const(x'B4')
004700080207     d c_F17           c                   const(x'B5')
004800080207     d c_F18           c                   const(x'B6')
004900080207     d c_F19           c                   const(x'B7')
005000080207     d c_F20           c                   const(x'B8')
005100080207     d c_F21           c                   const(x'B9')
005200080207     d c_F22           c                   const(x'BA')
005300080207     d c_F23           c                   const(x'BB')
005400080207     d c_F24           c                   const(x'BC')
005500080207     d c_Enter         c                   const(x'F1')
005600080207     d c_RollDown      c                   const(x'F4')
005700080207     d c_RollUp        c                   const(x'F5')
005800091124
005900110706       // -?Costante per controllo "caratteri solo numerici"?
006000110706     d c_Digits        c                   const('0123456789')
006100080206
006200080206      //---------------------------------------------------------------
006300080206      //?Definizione schiere.
006400080206      //---------------------------------------------------------------
006500080206
006600110706       // -?Messaggi di errore
006700090609     d $Msg            s             78    dim(10) ctdata perrcd(1)
006800080206
006900080206      //---------------------------------------------------------------
007000080206      //?Definizione aree dati.
007100080206      //---------------------------------------------------------------
007200080206
007300110706       // -?Dati utente
007400080206     d §AzUte        e ds                  extname(AZUTE00F)
007500080206     d                                     dtaara
007600080206     d §DatiUte      e ds                  extname(dDatiUte)
007700080206     d                                     dtaara
007800080206
007900080206      //---------------------------------------------------------------
008000080206      //?Definizione strutture dati.
008100080206      //---------------------------------------------------------------
008200080206
008300110706       // -?Status
008400080206     d Psds           sds
008500080206     d   SDSpgm          *proc
008600080206
008700110706       // -?InfDS
008800080206     d InfDspF         ds
008900080207     d  dsp_aid              369    369a
009000080206
009100110706       // -?Indicatori su DspF
009200080208     d IndDspF         ds
009300110706       // -?Indicatori di errore
009400110706     d  ErrMessage                    1n   overlay(IndDspF : 28)
009500120314     d  UteEdpMB                      1n   overlay(IndDspF : 40)
009600110706     d  PosCurNrv                     1n   overlay(IndDspF : 50)
009700120314     d  PosCurInfo                    1n   overlay(IndDspF : 51)
009800090807     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009900090407
010000090407     d WindDspF        ds                  inz
010100090407     d   WindDspFa             1     49    inz(*zero)
010200090407     d   WindDspFb            50     99    inz(*zero)
010300080206
010400110706       // -?Paremetri ricevuti
010500080206     d KPJBA         e ds
010600080206
010700110706       // -?Reperimento dati utente
010800080206     d TIBS34ds      e ds
010900110706
011000110706       // -?Info Trattativa
011100110706     d TNTA41DS      e ds                  inz
011200120314
011300120314      // -?Gestione Note clienti/potenziali
011400120314     d TRMK20ds      e ds                  inz
011500110706
011600110706       // -?File Info Trattative
011700110706     d TIVIIds       e ds                  extname(TIVII00F)
011800090629
011900080206      //---------------------------------------------------------------
012000080206      //?Definizione variabili globali.
012100080206      //---------------------------------------------------------------
012200080206
012300110706       // -?Flags booleani
012400080208     d $Fine           s               n   inz(*off)
012500110706     d $End            s               n   inz(*off)
012600100205     d $ErrGrave       s               n   inz(*off)
012700110706     d $InzD01         s               n   inz(*on)
012800110706     d $OkInfo         s               n   inz(*off)
012900080206
013000110706       // -?Campi associati al video
013100110706     d $Video          s              2    inz('D1')
013200090601     d dsp_mod         s             10I 0
013300110706
013400110706       // -?Campi di comodo
013500110706     d Tiket           s                   like(VICtck)
013600090806
013700110706       // -?Stringa SQL da eseguire
013800090806     d wSQL            s           2048    Varying        inz
013900110706
014000110706       // -?Nome libreria del work-file WFATT00F?
014100110706     d wFLib           s             21    inz
014200090508
014300090508      //---------------------------------------------------------------
014400090807      //?Definizione procedure esterne.
014500090508      //---------------------------------------------------------------
014600110506
014700110706       // -?Gestione Info Trattativa
014800110706     d TNTA41R         pr                  extpgm('TNTA41R')
014900110706     d  kpjba                              likeds(kpjba)
015000110706     d  tnta41ds                           likeds(tnta41ds)
015100110706
015200110706      //---------------------------------------------------------------
015300110706      //?Definizione prototipi.
015400110706      //---------------------------------------------------------------
015500110706
015600110706       // -?Reperimento dati utente
015700110706      /copy gaitrasrc/srcprotopr,tibs34r
015800120314
015900120314       // -?Scrittura note
016000120314      /copy gaitrasrc/srcprotopr,trmk20r
016100090807
016200080206      //---------------------------------------------------------------
016300080206      //?Definizione key-list.
016400080206      //---------------------------------------------------------------
016500090807
016600080206
016700080206      //---------------------------------------------------------------
016800080206      //?Riepilogo indicatori.
016900080206      //---------------------------------------------------------------
017000090807
017100090807
017200080206      //---------------------------------------------------------------
017300080206
017400080206      //---------------------------------------------------------------
017500080206      //?M A I N - L I N E
017600080206      //---------------------------------------------------------------
017700080206
017800080206     c     *Entry        plist
017900080206     c                   parm                    KPJBA
018000080206
018100080206      /free
018200080206
018300090807       //?Operazioni iniziali
018400080206       exsr RoutInz;
018500080206
018600090807       //?Gestione video
018700080206       DOW $Fine = *off;
018800090807
018900080206         select;
019000110706           when $Video = 'D1';
019100110706             exsr GesD01;
019200090428           other;
019300080206             $Fine = *on;
019400080206         endsl;
019500090807
019600080206       ENDDO;
019700080206
019800090807       //?Operazioni finali
019900080206       exsr RoutEnd;
020000080206
020100080206       //--------------------------------------------------------------
020200080206       //?Operazioni iniziali.
020300080206       //--------------------------------------------------------------
020400080206       BEGSR RoutInz;
020500080206
020600110706         $video = 'D1';
020700110706         $inzD01 = *on;
020800090807
020900110706       //?Reperimento dati job
021000090807         exsr DatiJob;
021100100209
021200090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021300090806
021400110706       //?Impostazione campi "fissi"
021500110706         V1Tpgm = SDSpgm;
021600110930
021700120314       //?Utente EDPMB
021800120314         UteEdpMB = (%subst(Knmus:1:5) = 'EDPMB');
021900090714
022000080206       ENDSR;
022100080206
022200080206       //--------------------------------------------------------------
022300080206       //?Reperimento Dati del job (Utente/Operativi).
022400080206       //--------------------------------------------------------------
022500080206       BEGSR DatiJob;
022600080206
022700080206         in(E) §AzUte;
022800110706         IF NOT %error;
022900080206           in(E) §DatiUte;
023000110706         ENDIF;
023100110706         IF %error or RSut = *blanks;
023200080206           clear TIBS34ds;
023300080206           tibs34r(tibs34ds);
023400080206           in §AzUte;
023500080206           in §DatiUte;
023600110706         ENDIF;
023700080206
023800080206       ENDSR;
023900080206
024000090421       //--------------------------------------------------------------
024100110706       //?Gestione videata D01
024200080206       //--------------------------------------------------------------
024300110706       BEGSR GesD01;
024400080207
024500110706       //?Inizializzazione videata
024600110706         IF  $InzD01 = *on;
024700110706            exsr InzD01;
024800110706            $InzD01  = *off;
024900110706         ENDIF;
025000080207
025100110706       //?Emissione Testata
025200110706         write  MKQ4T01;
025300080207
025400110706       //?Emissione videata
025500110706         exfmt  MKQ4D01;
025600110706         reset ErrMessage;
025700110706         reset ErrGenerico;
025800110706         clear V1Dmsg;
025900080207
026000080207         SELECT;
026100100205
026200110706         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
026300100205           WHEN  $ErrGrave;
026400100205             $Fine  = *on;
026500110706
026600110706         //?- F03=Fine
026700110706           WHEN  dsp_aid = c_F03;
026800110706             exsr F03D01;
026900110706
027000110706         //?- F06=Conferma
027100110706           WHEN  dsp_aid = c_F06;
027200111102             exsr CtrD01;
027300111102             IF  ErrGenerico;
027400111102               leavesr;
027500111102             ENDIF;
027600110706             exsr F06D01;
027700080207
027800090807         //?Invio
027900080207           OTHER;
028000110706         //?Eseguo i controlli
028100110706             exsr CtrD01;
028200110706             IF  ErrGenerico;
028300080207               leavesr;
028400110706             ENDIF;
028500080207
028600080207         ENDSL;
028700080207
028800080207       ENDSR;
028900080207
029000080207       //--------------------------------------------------------------
029100110706       //?Inizializzazione videata D01.
029200080207       //--------------------------------------------------------------
029300110706       BEGSR InzD01;
029400080207
029500110706         clear V1Dnrv;
029600110706         V1Dinfo = 'S';
029700120314         V1Dzero = 'N';
029800110706         V1Dsic  = 'S';
029900090508
030000080207       ENDSR;
030100080207
030200091111       //--------------------------------------------------------------
030300110706       //?Gestione tasto funzionale F03 da videata D01.
030400110706       //?F3=Fine
030500091111       //--------------------------------------------------------------
030600110706       BEGSR F03D01;
030700091111
030800110706       //?Chiusura del programma
030900091111         $Fine = *on;
031000091111
031100091111       ENDSR;
031200110706
031300110706       //--------------------------------------------------------------
031400110706       //?Gestione tasto funzionale F06 da videata D01.
031500110706       //?F6=Conferma
031600110706       //--------------------------------------------------------------
031700110706       BEGSR F06D01;
031800110706
031900110706       //?Modifico info trattativa
032000110706         IF  V1Dinfo = 'S';
032100110706           exsr Mod_Info;
032200111102           IF  O41F03 = 'S' or O41F12 = 'S';
032300111102             leavesr;
032400111102           ENDIF;
032500110706         ENDIF;
032600120314
032700120314       //?Azzero Info trattativa
032800120314         IF  V1Dzero = 'S';
032900120314           exsr Del_Info;
033000120314         ENDIF;
033100110706
033200110706       //?Modifico dati SIC
033300110706         IF  V1Dsic = 'S';
033400110706           exsr Mod_Sic;
033500110706         ENDIF;
033600111102
033700111102       //?Dopo aver modificato esco dal pgm
033800111102         //$Fine = *on;
033900120314
034000120314       //?Dopo aver modificato riemetto la prima videata pulita
034100120314         $InzD01 = *on;
034200110706
034300110706       ENDSR;
034400090807
034500080207       //--------------------------------------------------------------
034600110706       //?Controllo dati videata D01.
034700080207       //--------------------------------------------------------------
034800110706       BEGSR CtrD01;
034900080207
035000110706         WindDspF  = IndDspF;
035100110706         reset WindDspFb;
035200110706         IndDspF   = WindDspF;
035300080207
035400110706       //?Obbligo della trattativa
035500110706         IF  V1Dnrv = 0;
035600110706           ErrMessage  = *on;
035700110706           ErrGenerico = *on;
035800110706           PosCurNrv   = *on;
035900110706           V1Dmsg = $Msg(01);
036000110706           leavesr;
036100110706         ENDIF;
036200110706
036300110706       //?La trattativa deve esistere ed essere chiusa
036400110706         chain V1Dnrv TIVIS05L;
036500110706         IF  not %found(TIVIS05L);
036600110706           ErrMessage  = *on;
036700110706           ErrGenerico = *on;
036800110706           PosCurNrv   = *on;
036900110706           V1Dmsg = $Msg(02);
037000110706           leavesr;
037100110706         ENDIF;
037200110706
037300110706       //?La trattativa deve essere chiusa
037400110706         IF  VISdch = 0;
037500110706           ErrMessage  = *on;
037600110706           ErrGenerico = *on;
037700110706           PosCurNrv   = *on;
037800110706           V1Dmsg = $Msg(03);
037900110706           leavesr;
038000110706         ENDIF;
038100110930
038200110930       //?La trattativa deve avere le INFO inserite
038300110930         chain V1Dnrv TIVII01L;
038400110930         IF  not %found(TIVII01L);
038500110930           ErrMessage  = *on;
038600110930           ErrGenerico = *on;
038700110930           PosCurNrv   = *on;
038800110930           V1Dmsg = $Msg(04);
038900110930           leavesr;
039000110930         ENDIF;
039100120314
039200120314       //?Se richiesta la variazione delle INFO non posso richiedere l'azzeramento
039300120314         IF  V1Dinfo = 'S' and V1Dzero = 'S';
039400120314           ErrMessage  = *on;
039500120314           ErrGenerico = *on;
039600120314           PosCurInfo  = *on;
039700120314           V1Dmsg = $Msg(05);
039800120314           leavesr;
039900120314         ENDIF;
040000120314
040100120314       //?Una variazione sulle INFO va fatta
040200120314         IF  V1Dinfo = 'N' and V1Dzero = 'N';
040300120314           ErrMessage  = *on;
040400120314           ErrGenerico = *on;
040500120314           PosCurInfo  = *on;
040600120314           V1Dmsg = $Msg(05);
040700120314           leavesr;
040800120314         ENDIF;
040900080207
041000080207       ENDSR;
041100110706
041200110706       //--------------------------------------------------------------
041300110706       //?Modifico Info Trattative.
041400110706       //--------------------------------------------------------------
041500110706       BEGSR Mod_Info;
041600110706
041700110706         clear TNTA41DS;
041800110706         I41nrv    = VISnrv;
041900110706         I41rag    = VISrag;
042000110706         I41mod    = 'G';
042100110706         I41ifotot = VISinfot;
042200110706         I41tpv    = VIStpv;
042300110706         tnta41r (kpjba:tnta41ds);
042400110706
042500110706       ENDSR;
042600110706
042700120314       //--------------------------------------------------------------
042800120314       //?Cancello le INFO trattative e scrivo una nota sulla trattativa.
042900120314       //--------------------------------------------------------------
043000120314       BEGSR Del_Info;
043100120314
043200120314       //?Cancello le INFO trattative
043300120314         exec sql
043400120314         DELETE from TIVII00F
043500120314         WHERE VIInrv = :VISnrv;
043600120314
043700120314       //?Scrivo una nota legata alla trattativa
043800120314         clear TRMK20ds;
043900120314         IMK20tla = 'L';
044000120314         IMK20flm = 'C';
044100120314         IMK20fno = 'S';
044200120314         IMK20cpo = VIScpo;
044300120314         IMK20ksc = VISksc;
044400120314         IMK20nrv = VISnrv;
044500120314         EMK20no1 = 'Info trattative Azzerate da ' + knmus;
044600120314         trmk20r ( kpjba : trmk20ds );
044700120314
044800120314       ENDSR;
044900120314
045000110706       //--------------------------------------------------------------
045100110706       //?Modifico Sic.
045200110706       //--------------------------------------------------------------
045300110706       BEGSR Mod_Sic;
045400110706
045500110706         IF  %subst(knsif : 7 : 1) = 'P';
045600110706           exec sql
045700110706           SELECT VICtck into :Tiket
045800110706           FROM GAITRAAZP/TIVIIC0F
045900110706           WHERE VICnrv = :VISnrv;
046000110706
046100110706           exec sql
046200110706           DELETE from GAITRAAZP/TIVIIC0F
046300110706           WHERE VICnrv = :VISnrv;
046400110706         ELSE;
046500110706           exec sql
046600110706           SELECT VICtck into :Tiket
046700110706           FROM GAITRAAZM/TIVIIC0F
046800110706           WHERE VICnrv = :VISnrv;
046900110706
047000110706           exec sql
047100110706           DELETE from GAITRAAZM/TIVIIC0F
047200110706           WHERE VICnrv = :VISnrv;
047300110706         ENDIF;
047400110706
047500110706         clear TIVIIC00;
047600110706         $End    = *off;
047700110706         $OKinfo = *off;
047800110706
047900110926         wSQL = 'select * from TIVII00F ';
048000110706
048100110706         wSQL += 'where VIInrv = ' + %editc(VISnrv:'X') +
048200110706                   ' order by VIItpf';
048300110706
048400110706         exec sql prepare A1 from :wSQL;
048500110706
048600110706         exec sql declare VII cursor for A1;
048700110706
048800110706         exec sql open VII;
048900110706
049000110706         IF  sqlcode < 0;
049100110706           $End = *on;
049200110706         ENDIF;
049300110706
049400110706         DOW  not $End;
049500110706           exec sql fetch next from VII into :TIVIIDS;
049600110706           IF  sqlcod = 100 or sqlcod < 0;
049700110706             $End = *on;
049800110706             leave;
049900110706           ENDIF;
050000110706
050100110706       //?Imposto i dati per scrivere il record
050200110706           $OKinfo = *on;
050300110706           VICnrv = VIInrv;
050400110706
050500110706           SELECT;
050600110706         //?Fatturato/Spedizioni totale
050700110706             WHEN  VIItpf = 'TRT';
050800110706               VICfatt = VIIpft;
050900110706               VICnspt = VIIsna;
051000110706         //?Fatturato/Spedizioni estero
051100110706             WHEN  VIItpf = 'TRE';
051200110706               VICfate = VIIpft;
051300110706               VICnspe = VIIsna;
051400110706         //?Aumento/Sconto
051500110706             WHEN  VIItpf = 'A/S';
051600110706               VICausc = VIIvald;
051700110706         //?Delta
051800110706             WHEN  VIItpf = 'DEL';
051900110706               VICdel = VIIval;
052000110706         //?Peso
052100110706             WHEN  VIItpf = 'KMS';
052200110706               VICpkg = VIIval;
052300110706         //?Concorrenti
052400110706             WHEN  VIItpf = '10' and VIIspf = 'ARCO';
052500110706               VICarco = 'S';
052600110706             WHEN  VIItpf = '10' and VIIspf = 'ARTO';
052700110706               VICarto = 'S';
052800110706             WHEN  VIItpf = '10' and VIIspf = 'AWS';
052900110706               VICaws  = 'S';
053000110706             WHEN  VIItpf = '10' and VIIspf = 'BARS';
053100110706               VICbars = 'S';
053200110706             WHEN  VIItpf = '10' and VIIspf = 'DHL';
053300110706               VICdhl  = 'S';
053400110706             WHEN  VIItpf = '10' and VIIspf = 'EXEC';
053500110706               VICexec = 'S';
053600110706             WHEN  VIItpf = '10' and VIIspf = 'FERC';
053700110706               VICferc = 'S';
053800110706             WHEN  VIItpf = '10' and VIIspf = 'MAIL';
053900110706               VICmail = 'S';
054000110706             WHEN  VIItpf = '10' and VIIspf = 'MEP';
054100110706               VICmep  = 'S';
054200110706             WHEN  VIItpf = '10' and VIIspf = 'MTN';
054300110706               VICmtn  = 'S';
054400110706             WHEN  VIItpf = '10' and VIIspf = 'ONEE';
054500110706               VIConee = 'S';
054600110706             WHEN  VIItpf = '10' and VIIspf = 'PALL';
054700110706               VICpall = 'S';
054800110706             WHEN  VIItpf = '10' and VIIspf = 'PALW';
054900110706               VICpalw = 'S';
055000110706             WHEN  VIItpf = '10' and VIIspf = 'SDA';
055100110706               VICsda  = 'S';
055200110706             WHEN  VIItpf = '10' and VIIspf = 'SUSA';
055300110706               VICsusa = 'S';
055400110706             WHEN  VIItpf = '10' and VIIspf = 'TNT';
055500110706               VICtnt  = 'S';
055600110706             WHEN  VIItpf = '10' and VIIspf = 'UPS';
055700110706               VICups  = 'S';
055800110706             WHEN  VIItpf = '10' and VIIspf = '9910';
055900110706               VICaltr = 'S';
056000110706             WHEN  VIItpf = '10' and VIIspf = '8810';
056100110706               VICness = 'S';
056200110706           ENDSL;
056300110706         ENDDO;
056400110706
056500110706       //?Scrivo il rcd delle info commerciali
056600110706         IF  $OKinfo;
056700110706           VICtck = Tiket;
056800110706           write  TIVIIC00;
056900111102           feod TIVIIC0F;
057000110706         ENDIF;
057100110706
057200110706         exec sql close VII;
057300110706
057400110706       ENDSR;
057500090807
057600080206       //--------------------------------------------------------------
057700080206       //?Operazioni finali.
057800080206       //--------------------------------------------------------------
057900080206       BEGSR RoutEnd;
058000090521
058100080206         *inLR = *on;
058200080206         return;
058300080206
058400080206       ENDSR;
058500080206
058600080206      /end-free
058700090807
058800080206       //--------------------------------------------------------------
058900080206       //?Schiere a tempo di compilazione.
059000080206       //--------------------------------------------------------------
059100080206
059200080206** - $MSG -------------------------------------------------------------------*
059300110706Immettere la trattativa da variare                                             1
059400110706Trattativa inesistente                                                         2
059500110930Trattativa ancora aperta                                                       3
059600110930Trattativa SENZA INFO                                                          4
059700120314Modificare INFO o Azzerare INFO????                                            5
