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
003700140414
003800140414       //--------------------------------------------------------------
003900140414       //?Definizione schiere.
004000140414       //--------------------------------------------------------------
004100140414
004200140414       //--------------------------------------------------------------
004300140414       //?Definizione campi.
004400140414       //--------------------------------------------------------------
004500140417       //?Flags boolenai
004600140414     d wFine           s               n   inz(*off)
004700151016
004800151016       //?Campi di comodo
004900160408     d wFlib           s             21a   inz
005000160411
005100160411       // -?Nome del sistema?
005200160411     d currSysNeta     s              8a   inz
005300140414
005400140414       //--------------------------------------------------------------
005500140414       //?Definizione procedure.
005600140414       //--------------------------------------------------------------
005700160408       // -?Ricerca terminal
005800160408     d fnlv55r         pr                  extpgm('FNLV55R')
005900160408     d  fnlv55ds                           likeds(FNLV55DS)
006000140414
006100140414       //--------------------------------------------------------------
006200140414       //?Definizione prototipi.
006300140414       //--------------------------------------------------------------
006400160408       // -?Converto la lingua
006500160408      /copy gaitrasrc/srcprotopr,TIBS0800R
006600160411
006700160411       // -?Reperimento NETA sistema AS/400 corrente?
006800160411      /copy gaitrasrc/srcProtoPR,UBRTVNETA
006900140417
007000140417       //---------------------------------------------------------------
007100140417       //?Definizione key-list.
007200140417       //---------------------------------------------------------------
007300160408       //?File TNTBE01L
007400160408     d kTNTBE        e ds                  extname(TNTBE01L:*key)
007500160408     d                                     prefix(k_)
007600140414
007700140414       //--------------------------------------------------------------
007800140414       //?MAIN.
007900140414       //--------------------------------------------------------------
008000140416
008100140416     c     *entry        plist
008200160408     c                   parm                    TRULDSCDS
008300140414
008400140414      /free
008500140414
008600140414       //?Operazioni iniziali
008700140414       exsr RoutInz;
008800140414
008900140416       //?Cerca dati richiesti
009000160408       exsr GetDati;
009100140414
009200140414       //?Operazioni finali
009300140414       exsr RoutEnd;
009400140414
009500140414       //--------------------------------------------------------------
009600140414       //?Operazioni iniziali.                                         ?
009700140414       //--------------------------------------------------------------
009800140414       BEGSR  RoutInz;
009900140414
010000140416       //?Pulisco campi di OUTPUT
010100160408         clear truldscds.OULDSCerr;
010200160408         clear truldscds.OULDSCdesc;
010300160408         clear truldscds.OULDSCtel;
010400160408         clear truldscds.OULDSCmail;
010500160408         clear truldscds.OULDSCamda;
010600160408         clear truldscds.OULDSCama;
010700160408         clear truldscds.OULDSCpmda;
010800160408         clear truldscds.OULDSCpma;
010900160408         clear truldscds.OULDSCurl;
011000160411
011100160411       //?Apro file Tabelle
011200160411         IF  %open(TNTBE01L);
011300160411           leavesr;
011400160411         ENDIF;
011500160411
011600160411         UBRTVNETA_Rtv(currSysNeta);
011700160411         IF  %subst(currSysNetA:1:5 ) = 'AS888';
011800160411           wFLib = 'GAITRAGRPS/TNTBE01L';
011900160411         ELSE;
012000160411           wFLib = 'GAITRAGRU/TNTBE01L';
012100160411         ENDIF;
012200160411         open TNTBE01L;
012300140414
012400140414       ENDSR;
012500140414
012600140414       //--------------------------------------------------------------
012700140416       //?Recupero i dati richiesti.
012800140414       //--------------------------------------------------------------
012900140416       BEGSR  GetDati;
013000160408
013100160408       //?Se non passata la filiale esco con errore
013200160408         IF  truldscds.IULDSCfil = 0;
013300160408           truldscds.OULDSCerr = '1';
013400160408           leavesr;
013500160408         ENDIF;
013600160408
013700160408       //?Se non passata la lingua imposto 'it'
013800160408         IF  truldscds.IULDSClin = *blanks;
013900160408           truldscds.IULDSClin = 'it';
014000160408         ENDIF;
014100151016
014200160408       //?Aggancio Organigramma
014300160408         clear OG130;
014400160408         chain (truldscds.IULDSCfil) AZORG01L;
014500160408         IF  %found(AZORG01L);
014600160408           OG130 = ORGdd0;
014700160408         ENDIF;
014800160412
014900160412       //?Imposto la chiave per la ricerca dell'URL della filiale
015000160412         clear k_TBEke2;
015100160412         k_TBEcod = 'UFI';
015200160412         k_TBEke1 = %editc(truldscds.IULDSCfil:'X');
015300160408
015400160408       //?Se non trovo gli orari del servizio clienti
015500160408       //?della filiale passata
015600160408       //?devo cercare i dati del terminal di partenza
015700160412         IF  OG130 = *blanks or OG130 = *zeros;
015800160408           clear FNLV55DS;
015900160408           fnlv55ds.D55lin = truldscds.IULDSCfil;
016000160408           fnlv55ds.D55drf = %dec(%date());
016100160408           fnlv55r (FNLV55DS);
016200160408         //?riaggancio Organigramma
016300160408           chain (fnlv55ds.D55tfp) AZORG01L;
016400160408           IF  %found(AZORG01L);
016500160408             OG130 = ORGdd0;
016600160408           ENDIF;
016700160412         //?Reimposto la filiale anche per la ricerca dell'URL
016800160412           k_TBEke1 = %editc(fnlv55ds.D55tfp:'X');
016900160408         ENDIF;
017000160408
017100160408         truldscds.OULDSCdesc = ORGloc;
017200160408         truldscds.OULDSCtel  = ORGtel;
017300160408         truldscds.OULDSCmail = ORGdd2;
017400160408         truldscds.OULDSCamda = og130.別Goramd;
017500160408         truldscds.OULDSCama  = og130.別Gorama;
017600160408         truldscds.OULDSCpmda = og130.別Gorapd;
017700160408         truldscds.OULDSCpma  = og130.別Gorapa;
017800160408
017900160408       //?Cerco l'URL della filiale
018000160408         k_TBElin = cvtLinguaISO2ToTntbe(truldscds.IULDSClin);
018100160408         chain %kds(kTNTBE:4) TNTBE01L;
018200160408         IF  %found(TNTBE01L);
018300160408           truldscds.OULDSCurl  = TBEuni;
018400160408         ENDIF;
018500160408
018600160408       ENDSR;
018700140414
018800140414       //--------------------------------------------------------------
018900140414       //?Operazioni finali.
019000140414       //--------------------------------------------------------------
019100140414       BEGSR  RoutEnd;
019200160411
019300160411       //?Chiusura
019400160411         IF  truldscds.IULDSCtla = *blanks;
019500160411           *inRT = *on;
019600160411         ELSE;
019700160411           *inLR = *on;
019800160411         ENDIF;
019900140414
020000140414         return;
020100140414
020200140414       ENDSR;
020300140414
020400140414       //--------------------------------------------------------------
020500140415      /end-free
