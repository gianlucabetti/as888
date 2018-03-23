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
001900140912     fTIVIIC1F  o    e             disk
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
013600120706     d wOggi           s              8  0 inz
013700090806
013800110706       // -?Stringa SQL da eseguire
013900090806     d wSQL            s           2048    Varying        inz
014000110706
014100110706       // -?Nome libreria del work-file WFATT00F?
014200110706     d wFLib           s             21    inz
014300090508
014400090508      //---------------------------------------------------------------
014500090807      //?Definizione procedure esterne.
014600090508      //---------------------------------------------------------------
014700110506
014800110706       // -?Gestione Info Trattativa
014900110706     d TNTA41R         pr                  extpgm('TNTA41R')
015000110706     d  kpjba                              likeds(kpjba)
015100110706     d  tnta41ds                           likeds(tnta41ds)
015200110706
015300110706      //---------------------------------------------------------------
015400110706      //?Definizione prototipi.
015500110706      //---------------------------------------------------------------
015600110706
015700110706       // -?Reperimento dati utente
015800110706      /copy gaitrasrc/srcprotopr,tibs34r
015900120314
016000120314       // -?Scrittura note
016100120314      /copy gaitrasrc/srcprotopr,trmk20r
016200090807
016300080206      //---------------------------------------------------------------
016400080206      //?Definizione key-list.
016500080206      //---------------------------------------------------------------
016600090807
016700080206
016800080206      //---------------------------------------------------------------
016900080206      //?Riepilogo indicatori.
017000080206      //---------------------------------------------------------------
017100090807
017200090807
017300080206      //---------------------------------------------------------------
017400080206
017500080206      //---------------------------------------------------------------
017600080206      //?M A I N - L I N E
017700080206      //---------------------------------------------------------------
017800080206
017900080206     c     *Entry        plist
018000080206     c                   parm                    KPJBA
018100080206
018200080206      /free
018300080206
018400090807       //?Operazioni iniziali
018500080206       exsr RoutInz;
018600080206
018700090807       //?Gestione video
018800080206       DOW $Fine = *off;
018900090807
019000080206         select;
019100110706           when $Video = 'D1';
019200110706             exsr GesD01;
019300090428           other;
019400080206             $Fine = *on;
019500080206         endsl;
019600090807
019700080206       ENDDO;
019800080206
019900090807       //?Operazioni finali
020000080206       exsr RoutEnd;
020100080206
020200080206       //--------------------------------------------------------------
020300080206       //?Operazioni iniziali.
020400080206       //--------------------------------------------------------------
020500080206       BEGSR RoutInz;
020600080206
020700110706         $video = 'D1';
020800110706         $inzD01 = *on;
020900120706
021000120706       //?Imposto data elaborazione
021100120706         wOggi = %dec(%date());
021200090807
021300110706       //?Reperimento dati job
021400090807         exsr DatiJob;
021500100209
021600090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021700090806
021800110706       //?Impostazione campi "fissi"
021900110706         V1Tpgm = SDSpgm;
022000110930
022100120314       //?Utente EDPMB
022200120314         UteEdpMB = (%subst(Knmus:1:5) = 'EDPMB');
022300090714
022400080206       ENDSR;
022500080206
022600080206       //--------------------------------------------------------------
022700080206       //?Reperimento Dati del job (Utente/Operativi).
022800080206       //--------------------------------------------------------------
022900080206       BEGSR DatiJob;
023000080206
023100080206         in(E) §AzUte;
023200110706         IF NOT %error;
023300080206           in(E) §DatiUte;
023400110706         ENDIF;
023500110706         IF %error or RSut = *blanks;
023600080206           clear TIBS34ds;
023700080206           tibs34r(tibs34ds);
023800080206           in §AzUte;
023900080206           in §DatiUte;
024000110706         ENDIF;
024100080206
024200080206       ENDSR;
024300080206
024400090421       //--------------------------------------------------------------
024500110706       //?Gestione videata D01
024600080206       //--------------------------------------------------------------
024700110706       BEGSR GesD01;
024800080207
024900110706       //?Inizializzazione videata
025000110706         IF  $InzD01 = *on;
025100110706            exsr InzD01;
025200110706            $InzD01  = *off;
025300110706         ENDIF;
025400080207
025500110706       //?Emissione Testata
025600110706         write  MKQ4T01;
025700080207
025800110706       //?Emissione videata
025900110706         exfmt  MKQ4D01;
026000110706         reset ErrMessage;
026100110706         reset ErrGenerico;
026200110706         clear V1Dmsg;
026300080207
026400080207         SELECT;
026500100205
026600110706         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
026700100205           WHEN  $ErrGrave;
026800100205             $Fine  = *on;
026900110706
027000110706         //?- F03=Fine
027100110706           WHEN  dsp_aid = c_F03;
027200110706             exsr F03D01;
027300110706
027400110706         //?- F06=Conferma
027500110706           WHEN  dsp_aid = c_F06;
027600111102             exsr CtrD01;
027700111102             IF  ErrGenerico;
027800111102               leavesr;
027900111102             ENDIF;
028000110706             exsr F06D01;
028100080207
028200090807         //?Invio
028300080207           OTHER;
028400110706         //?Eseguo i controlli
028500110706             exsr CtrD01;
028600110706             IF  ErrGenerico;
028700080207               leavesr;
028800110706             ENDIF;
028900080207
029000080207         ENDSL;
029100080207
029200080207       ENDSR;
029300080207
029400080207       //--------------------------------------------------------------
029500110706       //?Inizializzazione videata D01.
029600080207       //--------------------------------------------------------------
029700110706       BEGSR InzD01;
029800080207
029900110706         clear V1Dnrv;
030000110706         V1Dinfo = 'S';
030100120314         V1Dzero = 'N';
030200110706         V1Dsic  = 'S';
030300090508
030400080207       ENDSR;
030500080207
030600091111       //--------------------------------------------------------------
030700110706       //?Gestione tasto funzionale F03 da videata D01.
030800110706       //?F3=Fine
030900091111       //--------------------------------------------------------------
031000110706       BEGSR F03D01;
031100091111
031200110706       //?Chiusura del programma
031300091111         $Fine = *on;
031400091111
031500091111       ENDSR;
031600110706
031700110706       //--------------------------------------------------------------
031800110706       //?Gestione tasto funzionale F06 da videata D01.
031900110706       //?F6=Conferma
032000110706       //--------------------------------------------------------------
032100110706       BEGSR F06D01;
032200110706
032300110706       //?Modifico info trattativa
032400110706         IF  V1Dinfo = 'S';
032500110706           exsr Mod_Info;
032600111102           IF  O41F03 = 'S' or O41F12 = 'S';
032700111102             leavesr;
032800111102           ENDIF;
032900110706         ENDIF;
033000120314
033100120314       //?Azzero Info trattativa
033200120314         IF  V1Dzero = 'S';
033300120314           exsr Del_Info;
033400120314         ENDIF;
033500110706
033600110706       //?Modifico dati SIC
033700110706         IF  V1Dsic = 'S';
033800110706           exsr Mod_Sic;
033900110706         ENDIF;
034000111102
034100111102       //?Dopo aver modificato esco dal pgm
034200111102         //$Fine = *on;
034300120314
034400120314       //?Dopo aver modificato riemetto la prima videata pulita
034500120314         $InzD01 = *on;
034600110706
034700110706       ENDSR;
034800090807
034900080207       //--------------------------------------------------------------
035000110706       //?Controllo dati videata D01.
035100080207       //--------------------------------------------------------------
035200110706       BEGSR CtrD01;
035300080207
035400110706         WindDspF  = IndDspF;
035500110706         reset WindDspFb;
035600110706         IndDspF   = WindDspF;
035700080207
035800110706       //?Obbligo della trattativa
035900110706         IF  V1Dnrv = 0;
036000110706           ErrMessage  = *on;
036100110706           ErrGenerico = *on;
036200110706           PosCurNrv   = *on;
036300110706           V1Dmsg = $Msg(01);
036400110706           leavesr;
036500110706         ENDIF;
036600110706
036700110706       //?La trattativa deve esistere ed essere chiusa
036800110706         chain V1Dnrv TIVIS05L;
036900110706         IF  not %found(TIVIS05L);
037000110706           ErrMessage  = *on;
037100110706           ErrGenerico = *on;
037200110706           PosCurNrv   = *on;
037300110706           V1Dmsg = $Msg(02);
037400110706           leavesr;
037500110706         ENDIF;
037600110706
037700110706       //?La trattativa deve essere chiusa
037800110706         IF  VISdch = 0;
037900110706           ErrMessage  = *on;
038000110706           ErrGenerico = *on;
038100110706           PosCurNrv   = *on;
038200110706           V1Dmsg = $Msg(03);
038300110706           leavesr;
038400110706         ENDIF;
038500120706
038600120706       //?La trattativa deve avere data chiusura < data del giorno
038700120706         IF  VISdch >= wOggi;
038800120706           ErrMessage  = *on;
038900120706           ErrGenerico = *on;
039000120706           PosCurNrv   = *on;
039100120706           V1Dmsg = $Msg(06);
039200120706           leavesr;
039300120706         ENDIF;
039400110930
039500110930       //?La trattativa deve avere le INFO inserite
039600110930         chain V1Dnrv TIVII01L;
039700110930         IF  not %found(TIVII01L);
039800110930           ErrMessage  = *on;
039900110930           ErrGenerico = *on;
040000110930           PosCurNrv   = *on;
040100110930           V1Dmsg = $Msg(04);
040200110930           leavesr;
040300110930         ENDIF;
040400120314
040500120314       //?Se richiesta la variazione delle INFO non posso richiedere l'azzeramento
040600120314         IF  V1Dinfo = 'S' and V1Dzero = 'S';
040700120314           ErrMessage  = *on;
040800120314           ErrGenerico = *on;
040900120314           PosCurInfo  = *on;
041000120314           V1Dmsg = $Msg(05);
041100120314           leavesr;
041200120314         ENDIF;
041300120314
041400120314       //?Una variazione sulle INFO va fatta
041500120314         IF  V1Dinfo = 'N' and V1Dzero = 'N';
041600120314           ErrMessage  = *on;
041700120314           ErrGenerico = *on;
041800120314           PosCurInfo  = *on;
041900120314           V1Dmsg = $Msg(05);
042000120314           leavesr;
042100120314         ENDIF;
042200080207
042300080207       ENDSR;
042400110706
042500110706       //--------------------------------------------------------------
042600110706       //?Modifico Info Trattative.
042700110706       //--------------------------------------------------------------
042800110706       BEGSR Mod_Info;
042900110706
043000110706         clear TNTA41DS;
043100110706         I41nrv    = VISnrv;
043200110706         I41rag    = VISrag;
043300110706         I41mod    = 'G';
043400110706         I41ifotot = VISinfot;
043500110706         I41tpv    = VIStpv;
043600110706         tnta41r (kpjba:tnta41ds);
043700110706
043800110706       ENDSR;
043900110706
044000120314       //--------------------------------------------------------------
044100120314       //?Cancello le INFO trattative e scrivo una nota sulla trattativa.
044200120314       //--------------------------------------------------------------
044300120314       BEGSR Del_Info;
044400120314
044500120314       //?Cancello le INFO trattative
044600120314         exec sql
044700120314         DELETE from TIVII00F
044800120314         WHERE VIInrv = :VISnrv;
044900120314
045000120314       //?Scrivo una nota legata alla trattativa
045100120314         clear TRMK20ds;
045200120314         IMK20tla = 'L';
045300120314         IMK20flm = 'C';
045400120314         IMK20fno = 'S';
045500120314         IMK20cpo = VIScpo;
045600120314         IMK20ksc = VISksc;
045700120314         IMK20nrv = VISnrv;
045800120314         EMK20no1 = 'Info trattative Azzerate da ' + knmus;
045900120314         trmk20r ( kpjba : trmk20ds );
046000120314
046100120314       ENDSR;
046200120314
046300110706       //--------------------------------------------------------------
046400110706       //?Modifico Sic.
046500110706       //--------------------------------------------------------------
046600110706       BEGSR Mod_Sic;
046700110706
046800110706         IF  %subst(knsif : 7 : 1) = 'P';
046900110706           exec sql
047000110706           SELECT VICtck into :Tiket
047100140912           FROM GAITRAAZP/TIVIIC1F
047200110706           WHERE VICnrv = :VISnrv;
047300110706
047400110706           exec sql
047500140912           DELETE from GAITRAAZP/TIVIIC1F
047600110706           WHERE VICnrv = :VISnrv;
047700110706         ELSE;
047800110706           exec sql
047900110706           SELECT VICtck into :Tiket
048000140912           FROM GAITRAAZM/TIVIIC1F
048100110706           WHERE VICnrv = :VISnrv;
048200110706
048300110706           exec sql
048400140912           DELETE from GAITRAAZM/TIVIIC1F
048500110706           WHERE VICnrv = :VISnrv;
048600110706         ENDIF;
048700110706
048800140912         clear TIVIIC10;
048900110706         $End    = *off;
049000110706         $OKinfo = *off;
049100110706
049200110926         wSQL = 'select * from TIVII00F ';
049300110706
049400110706         wSQL += 'where VIInrv = ' + %editc(VISnrv:'X') +
049500110706                   ' order by VIItpf';
049600110706
049700110706         exec sql prepare A1 from :wSQL;
049800110706
049900110706         exec sql declare VII cursor for A1;
050000110706
050100110706         exec sql open VII;
050200110706
050300110706         IF  sqlcode < 0;
050400110706           $End = *on;
050500110706         ENDIF;
050600110706
050700110706         DOW  not $End;
050800110706           exec sql fetch next from VII into :TIVIIDS;
050900110706           IF  sqlcod = 100 or sqlcod < 0;
051000110706             $End = *on;
051100110706             leave;
051200110706           ENDIF;
051300110706
051400110706       //?Imposto i dati per scrivere il record
051500110706           $OKinfo = *on;
051600110706           VICnrv = VIInrv;
051700110706
051800110706           SELECT;
051900110706         //?Fatturato/Spedizioni totale
052000110706             WHEN  VIItpf = 'TRT';
052100110706               VICfatt = VIIpft;
052200110706               VICnspt = VIIsna;
052300110706         //?Fatturato/Spedizioni estero
052400110706             WHEN  VIItpf = 'TRE';
052500110706               VICfate = VIIpft;
052600110706               VICnspe = VIIsna;
052700110706         //?Aumento/Sconto
052800110706             WHEN  VIItpf = 'A/S';
052900110706               VICausc = VIIvald;
053000110706         //?Delta
053100110706             WHEN  VIItpf = 'DEL';
053200110706               VICdel = VIIval;
053300110706         //?Peso
053400110706             WHEN  VIItpf = 'KMS';
053500110706               VICpkg = VIIval;
053600110706         //?Concorrenti
053700110706             WHEN  VIItpf = '10' and VIIspf = 'ARCO';
053800110706               VICarco = 'S';
053900110706             WHEN  VIItpf = '10' and VIIspf = 'ARTO';
054000110706               VICarto = 'S';
054100110706             WHEN  VIItpf = '10' and VIIspf = 'AWS';
054200110706               VICaws  = 'S';
054300110706             WHEN  VIItpf = '10' and VIIspf = 'BARS';
054400110706               VICbars = 'S';
054500110706             WHEN  VIItpf = '10' and VIIspf = 'DHL';
054600110706               VICdhl  = 'S';
054700110706             WHEN  VIItpf = '10' and VIIspf = 'EXEC';
054800110706               VICexec = 'S';
054900140912             WHEN  VIItpf = '10' and VIIspf = 'FEDE';
055000140912               VICfede = 'S';
055100110706             WHEN  VIItpf = '10' and VIIspf = 'FERC';
055200110706               VICferc = 'S';
055300110706             WHEN  VIItpf = '10' and VIIspf = 'MAIL';
055400110706               VICmail = 'S';
055500110706             WHEN  VIItpf = '10' and VIIspf = 'MEP';
055600110706               VICmep  = 'S';
055700110706             WHEN  VIItpf = '10' and VIIspf = 'MTN';
055800110706               VICmtn  = 'S';
055900110706             WHEN  VIItpf = '10' and VIIspf = 'ONEE';
056000110706               VIConee = 'S';
056100110706             WHEN  VIItpf = '10' and VIIspf = 'PALL';
056200110706               VICpall = 'S';
056300110706             WHEN  VIItpf = '10' and VIIspf = 'PALW';
056400110706               VICpalw = 'S';
056500140912             WHEN  VIItpf = '10' and VIIspf = 'POST';
056600140912               VICpost = 'S';
056700110706             WHEN  VIItpf = '10' and VIIspf = 'SDA';
056800110706               VICsda  = 'S';
056900110706             WHEN  VIItpf = '10' and VIIspf = 'SUSA';
057000110706               VICsusa = 'S';
057100110706             WHEN  VIItpf = '10' and VIIspf = 'TNT';
057200110706               VICtnt  = 'S';
057300140912             WHEN  VIItpf = '10' and VIIspf = 'TNTP';
057400140912               VICtntp = 'S';
057500110706             WHEN  VIItpf = '10' and VIIspf = 'UPS';
057600110706               VICups  = 'S';
057700110706             WHEN  VIItpf = '10' and VIIspf = '9910';
057800110706               VICaltr = 'S';
057900110706             WHEN  VIItpf = '10' and VIIspf = '8810';
058000110706               VICness = 'S';
058100110706           ENDSL;
058200110706         ENDDO;
058300110706
058400110706       //?Scrivo il rcd delle info commerciali
058500110706         IF  $OKinfo;
058600110706           VICtck = Tiket;
058700140912           write  TIVIIC10;
058800140912           feod TIVIIC1F;
058900110706         ENDIF;
059000110706
059100110706         exec sql close VII;
059200110706
059300110706       ENDSR;
059400090807
059500080206       //--------------------------------------------------------------
059600080206       //?Operazioni finali.
059700080206       //--------------------------------------------------------------
059800080206       BEGSR RoutEnd;
059900090521
060000080206         *inLR = *on;
060100080206         return;
060200080206
060300080206       ENDSR;
060400080206
060500080206      /end-free
060600090807
060700080206       //--------------------------------------------------------------
060800080206       //?Schiere a tempo di compilazione.
060900080206       //--------------------------------------------------------------
061000080206
061100080206** - $MSG -------------------------------------------------------------------*
061200110706Immettere la trattativa da variare                                             1
061300110706Trattativa inesistente                                                         2
061400110930Trattativa ancora aperta                                                       3
061500110930Trattativa SENZA INFO                                                          4
061600120314Modificare INFO o Azzerare INFO????                                            5
061700120706La trattativa è stata chiusa oggi, modificarla domani!!!                       6
