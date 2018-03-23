000100140414       //==============================================================
000200140414       //
000300160408       //?TRULDSCR - Reperisce dati servizio clienti filiale BRT
000400140414       //
000500140414       //==============================================================
000600140414     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000700160411     h DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIBS':'UBRTVNETA')
000800140414
000900140414       //--------------------------------------------------------------
001000140414       //?Dichiarazione file.                                          ?
001100140414       //--------------------------------------------------------------
001200151105       //?Oranigramma di filiale
001300151105     fAZORG01L  if   e           k disk
001400160408
001500160408       //?Tabelle
001600160408     fTNTBE01L  if   e           k disk    usropn extfile(wFLib)
001700140414
001800140414       //--------------------------------------------------------------
001900140414       //?Definizione costanti.                                        ?
002000140414       //--------------------------------------------------------------
002100140414
002200140414       //---------------------------------------------------------------
002300140414       //?Definizione aree dati.
002400140414       //---------------------------------------------------------------
002500140414
002600140414       //--------------------------------------------------------------
002700140414       //?Definizione strutture dati.                                  ?
002800140414       //--------------------------------------------------------------
002900160408       // -?Ricerca terminal
003000160408     d FNLV55DS      e ds                  qualified inz
003100160408
003200160408       // -?Ds Passaggio Dati
003300160408     d TRULDSCDS     e ds                  qualified
003400160408
003500160408       //?DS campo ORGdd0
003600160408     d OG130         e ds                  qualified inz
003700160728
003800160728       //?DS campo ORGde3
003900160728     d OG143         e ds                  qualified inz
004000140414
004100140414       //--------------------------------------------------------------
004200140414       //?Definizione schiere.
004300140414       //--------------------------------------------------------------
004400140414
004500140414       //--------------------------------------------------------------
004600140414       //?Definizione campi.
004700140414       //--------------------------------------------------------------
004800140417       //?Flags boolenai
004900140414     d wFine           s               n   inz(*off)
005000151016
005100151016       //?Campi di comodo
005200160408     d wFlib           s             21a   inz
005300160728     d wNTW            s                   like(OG143.§OGNTW)
005400160728     d wVerona         s              3s 0 inz(043)
005500160728     d wRovereto       s              3s 0 inz(172)
005600160411
005700160411       // -?Nome del sistema?
005800160411     d currSysNeta     s              8a   inz
005900140414
006000140414       //--------------------------------------------------------------
006100140414       //?Definizione procedure.
006200140414       //--------------------------------------------------------------
006300160408       // -?Ricerca terminal
006400160408     d fnlv55r         pr                  extpgm('FNLV55R')
006500160408     d  fnlv55ds                           likeds(FNLV55DS)
006600140414
006700140414       //--------------------------------------------------------------
006800140414       //?Definizione prototipi.
006900140414       //--------------------------------------------------------------
007000160408       // -?Converto la lingua
007100160408      /copy gaitrasrc/srcprotopr,TIBS0800R
007200160411
007300160411       // -?Reperimento NETA sistema AS/400 corrente?
007400160411      /copy gaitrasrc/srcProtoPR,UBRTVNETA
007500140417
007600140417       //---------------------------------------------------------------
007700140417       //?Definizione key-list.
007800140417       //---------------------------------------------------------------
007900160408       //?File TNTBE01L
008000160408     d kTNTBE        e ds                  extname(TNTBE01L:*key)
008100160408     d                                     prefix(k_)
008200140414
008300140414       //--------------------------------------------------------------
008400140414       //?MAIN.
008500140414       //--------------------------------------------------------------
008600140416
008700140416     c     *entry        plist
008800160408     c                   parm                    TRULDSCDS
008900140414
009000140414      /free
009100140414
009200140414       //?Operazioni iniziali
009300140414       exsr RoutInz;
009400140414
009500140416       //?Cerca dati richiesti
009600160408       exsr GetDati;
009700140414
009800140414       //?Operazioni finali
009900140414       exsr RoutEnd;
010000140414
010100140414       //--------------------------------------------------------------
010200140414       //?Operazioni iniziali.                                         ?
010300140414       //--------------------------------------------------------------
010400140414       BEGSR  RoutInz;
010500140414
010600140416       //?Pulisco campi di OUTPUT
010700160408         clear truldscds.OULDSCerr;
010800160408         clear truldscds.OULDSCdesc;
010900160408         clear truldscds.OULDSCtel;
011000160408         clear truldscds.OULDSCmail;
011100160408         clear truldscds.OULDSCamda;
011200160408         clear truldscds.OULDSCama;
011300160408         clear truldscds.OULDSCpmda;
011400160408         clear truldscds.OULDSCpma;
011500160408         clear truldscds.OULDSCurl;
011600160411
011700160411       //?Apro file Tabelle
011800160411         IF  %open(TNTBE01L);
011900160411           leavesr;
012000160411         ENDIF;
012100160411
012200160411         UBRTVNETA_Rtv(currSysNeta);
012300160411         IF  %subst(currSysNetA:1:5 ) = 'AS888';
012400160411           wFLib = 'GAITRAGRPS/TNTBE01L';
012500160411         ELSE;
012600160411           wFLib = 'GAITRAGRU/TNTBE01L';
012700160411         ENDIF;
012800160411         open TNTBE01L;
012900140414
013000140414       ENDSR;
013100140414
013200140414       //--------------------------------------------------------------
013300140416       //?Recupero i dati richiesti.
013400140414       //--------------------------------------------------------------
013500140416       BEGSR  GetDati;
013600160408
013700160408       //?Se non passata la filiale esco con errore
013800160408         IF  truldscds.IULDSCfil = 0;
013900160408           truldscds.OULDSCerr = '1';
014000160408           leavesr;
014100160408         ENDIF;
014200160408
014300160408       //?Se non passata la lingua imposto 'it'
014400160408         IF  truldscds.IULDSClin = *blanks;
014500160408           truldscds.IULDSClin = 'it';
014600160408         ENDIF;
014700151016
014800160408       //?Aggancio Organigramma
014900160408         clear OG130;
015000160728         clear OG143;
015100160408         chain (truldscds.IULDSCfil) AZORG01L;
015200160408         IF  %found(AZORG01L);
015300160408           OG130 = ORGdd0;
015400160728           OG143 = ORGde3;
015500160408         ENDIF;
015600160728         wNTW = OG143.§OGntw;
015700160412
015800160412       //?Imposto la chiave per la ricerca dell'URL della filiale
015900160412         clear k_TBEke2;
016000160412         k_TBEcod = 'UFI';
016100160412         k_TBEke1 = %editc(truldscds.IULDSCfil:'X');
016200160408
016300160408       //?Se non trovo gli orari del servizio clienti
016400160408       //?della filiale passata
016500160408       //?devo cercare i dati del terminal di partenza
016600160412         IF  OG130 = *blanks or OG130 = *zeros;
016700160408           clear FNLV55DS;
016800160408           fnlv55ds.D55lin = truldscds.IULDSCfil;
016900160408           fnlv55ds.D55drf = %dec(%date());
017000160408           fnlv55r (FNLV55DS);
017100160408         //?riaggancio Organigramma
017200160408           chain (fnlv55ds.D55tfp) AZORG01L;
017300160408           IF  %found(AZORG01L);
017400160408             OG130 = ORGdd0;
017500160408           ENDIF;
017600160412         //?Reimposto la filiale anche per la ricerca dell'URL
017700160412           k_TBEke1 = %editc(fnlv55ds.D55tfp:'X');
017800160408         ENDIF;
017900160728
018000160728       //?Se la filiale passata dal pgm chiamante è una filiale estera
018100160728       //?e il suo Termina di Partenza è ROVERETO devo forzare come filiale VERONA
018200160728         IF  (wNTW = 'DPD' or wNTW = 'EEX') and fnlv55ds.D55tfp = wRovereto;
018300160728         //?riaggancio Organigramma con 043
018400160728           clear OG130;
018500160728           chain (wVerona) AZORG01L;
018600160728           IF  %found(AZORG01L);
018700160728             OG130 = ORGdd0;
018800160728           ENDIF;
018900160728         //?Reimposto la filiale anche per la ricerca dell'URL
019000160728           k_TBEke1 = %editc(wVerona:'X');
019100160728
019200160728         ENDIF;
019300160408
019400160408         truldscds.OULDSCdesc = ORGloc;
019500160408         truldscds.OULDSCtel  = ORGtel;
019600160408         truldscds.OULDSCmail = ORGdd2;
019700160408         truldscds.OULDSCamda = og130.§OGoramd;
019800160408         truldscds.OULDSCama  = og130.§OGorama;
019900160408         truldscds.OULDSCpmda = og130.§OGorapd;
020000160408         truldscds.OULDSCpma  = og130.§OGorapa;
020100160408
020200160408       //?Cerco l'URL della filiale
020300160408         k_TBElin = cvtLinguaISO2ToTntbe(truldscds.IULDSClin);
020400160408         chain %kds(kTNTBE:4) TNTBE01L;
020500160408         IF  %found(TNTBE01L);
020600160408           truldscds.OULDSCurl  = TBEuni;
020700160408         ENDIF;
020800160408
020900160408       ENDSR;
021000140414
021100140414       //--------------------------------------------------------------
021200140414       //?Operazioni finali.
021300140414       //--------------------------------------------------------------
021400140414       BEGSR  RoutEnd;
021500160411
021600160411       //?Chiusura
021700160411         IF  truldscds.IULDSCtla = *blanks;
021800160411           *inRT = *on;
021900160411         ELSE;
022000160411           *inLR = *on;
022100160411         ENDIF;
022200140414
022300140414         return;
022400140414
022500140414       ENDSR;
022600140414
022700140414       //--------------------------------------------------------------
022800140415      /end-free
