000100080206      //--------------------------------------------------------------
000200100304      //?TRMK86R - Stampa Agenda commerciale
000300080206      //--------------------------------------------------------------
000400080206
000500100505     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600090807     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100100308     fTABEL00F  if   e           k disk
001200130917     fAZCMM01L  if   e           k disk
001300100308     fTFNTC01L  if   e           k disk
001400100308     fTIATC01L  if   e           k disk    rename(tiatc000:w_tiatc)
001500100308     f                                     prefix(w_)
001600100309     fTIATL01L  if   e           k disk
001700100309     fTIATL02L  if   e           k disk    rename(tiatl000:tiatl02)
001800100308     fTICPI01L  if   e           k disk
001900100520     fTICPN05L  if   e           k disk
002000100505     fTICPN04L  if   e           k disk    rename(ticpn000:ticpn04)
002100110803     fTIVII01L  if   e           k disk
002200110803     fTIVIS05L  if   e           k disk
002300110803     fTIVOF01L  if   e           k disk
002400100308     fTNCPO01L  if   e           k disk
002500100308     fTRMK86P   o    e             printer oflind(*in99)
002600100310     f                                     infds(prtinfo)
002700100310
002800080206      //---------------------------------------------------------------
002900090406      //?Definizione costanti.
003000080206      //---------------------------------------------------------------
003100080206
003200080206      //---------------------------------------------------------------
003300080206      //?Definizione schiere.
003400080206      //---------------------------------------------------------------
003500090806
003600100304      // - Agenti
003700100429     d $Age            s              7  0 dim(3500)
003800100429
003900100429      // - Agenti Unificanti
004000100429     d $AgeU           s              7  0 dim(3500)
004100100305
004200100305      // - Data completa
004300100305     d $Date           s              8  0 dim(07)
004400100513
004500100513     d §MK86TATds      ds            10    inz
004600100513     d   $TAT                  1     10    inz  dim(10)
004700080206
004800080206      //---------------------------------------------------------------
004900080206      //?Definizione aree dati.
005000080206      //---------------------------------------------------------------
005100080206
005200080206      // - Dati utente
005300080206     d §AzUte        e ds                  extname(AZUTE00F)
005400080206     d                                     dtaara
005500080206     d §DatiUte      e ds                  extname(dDatiUte)
005600080206     d                                     dtaara
005700080206
005800080206      //---------------------------------------------------------------
005900080206      //?Definizione strutture dati.
006000080206      //---------------------------------------------------------------
006100100310
006200100310     d PrtINFO         ds
006300100310     d  CurLine              367    368i 0
006400080206
006500080206      // - Status
006600080206     d Psds           sds
006700080206     d   SDSpgm          *proc
006800080206
006900080206      // - Parametri ricevuti
007000080206     d KPJBA         e ds
007100100305     d TRMK86DS      e ds
007200080206
007300100308      // - Ricerca/Controllo tabelle
007400100308     d TIBS02ds      e ds                  inz
007500100308
007600080206      // - Reperimento dati utente
007700100305     d TIBS34DS      e ds
007800100308
007900100308       // - Reperimento dati anagrafici
008000100308     d TIBS69ds      e ds
008100100308     d DS_cnaco      e ds                  inz extname(CNACO00F)
008200100308     d DS_cnind      e ds                  inz extname(CNIND00F)
008300100308     d DS_cnclp      e ds                  inz extname(CNCLP00F)
008400100308     d DS_fncls      e ds                  inz extname(FNCLS00F)
008500100429
008600100429      // - controllo abilitazioni
008700100429     d TNTAA1DS      e ds                  inz
008800100429
008900100429      // - Carico Filiali abilitate all'utente
009000100429     d TRUL31DS      e ds
009100100429     d POG                    10    759    DIM(250)
009200100308
009300100308      // - Tabella CCO = Causale contatto
009400100308     d dCCO          e ds                  inz
009500100706
009600100706      // - Tabella IFS = Sottotipi INFO commerciali
009700100706     d dIFS          e ds                  inz
009800100308
009900100308      // - Tabella TTA = Tipo attività
010000100308     d dTTA          e ds                  inz
010100100308
010200100308      // - Tabella 1L = Categoria Merceologica
010300100308     d ds1L          e ds                  inz
010400090807
010500090807      // File attività
010600100305     d TIATC00F      e ds                  extname(TIATC00F)
010700100305
010800100305      // File trattativa
010900100305     d TIVIS00F      e ds                  extname(TIVIS00F)
011000090629
011100080206      //---------------------------------------------------------------
011200080206      //?Definizione variabili globali.
011300080206      //---------------------------------------------------------------
011400080206
011500080206      // - Flags booleani
011600100305     d $End            s               n   inz(*off)
011700100305     d $EndAge         s               n   inz(*off)
011800100305     d $RcdOk          s               n   inz(*off)
011900080206
012000080206      // - Indici di schiera
012100100429     d xx              s              4  0 inz
012200100429     d yy              s              4  0 inz
012300100429     d zz              s              4  0 inz
012400080206
012500090806       // - Stringa SQL da eseguire
012600090806     d wSQL            s           2048    Varying        inz
012700080206
012800090401      // - Campi di comodo data
012900100305     d wDate_EUR       s               d   datfmt(*eur)
013000100305     d wDate_ISO       s               d   datfmt(*iso)
013100100305
013200100305      // - Campi di comodo
013300100308     d sav_ATCdad      s                   like(ATCdad)
013400100309     d wAff            s              1  0 inz
013500100308     d wCatMer         s              5  0 inz
013600100309     d wCmm            s                   like(ATCcmm)
013700100906     d wConc           s              2  0 inz
013800100429     d wLib            s             21    inz
013900100308     d wNota           s              1  0 inz
014000100305     d wTrat           s            196    inz(*all'-')
014100100429
014200090508      //---------------------------------------------------------------
014300090807      //?Definizione procedure esterne.
014400090508      //---------------------------------------------------------------
014500100120
014600100120      //---------------------------------------------------------------
014700100120      //?prototipi
014800100120      //---------------------------------------------------------------
014900090807
015000080626      /copy gaitrasrc/srcprotopr,tibs02r
015100080626      /copy gaitrasrc/srcprotopr,tibs34r
015200100308      /copy gaitrasrc/srcprotopr,tibs69r
015300100429      /copy gaitrasrc/srcprotopr,tntaa1c
015400100429      /copy gaitrasrc/srcprotopr,trul31r
015500090807
015600080206      //---------------------------------------------------------------
015700080206      //?Definizione key-list.
015800080206      //---------------------------------------------------------------
015900090807
016000090807       // - File TABEL00F
016100090807     d k03tabel      e ds                  extname(TABEL00F:*key)
016200090807     d                                     prefix(k_)
016300090807     d                                     inz
016400080206
016500080206      //---------------------------------------------------------------
016600080206      //?Riepilogo indicatori.
016700080206      //---------------------------------------------------------------
016800090807
016900100504      // 10    : HI tipo attività se da eseguire
017000100514      // 11    : Se acceso non stampa in testata le attività escluse
017100100308      // 99    : Salto pagina
017200090807
017300080206      //---------------------------------------------------------------
017400080206
017500080206      //---------------------------------------------------------------
017600080206      //?M A I N - L I N E
017700080206      //---------------------------------------------------------------
017800080206
017900080206     c     *Entry        plist
018000080206     c                   parm                    KPJBA
018100100304     c                   parm                    TRMK86DS
018200080206
018300080206      /free
018400080206
018500090807       //?Operazioni iniziali
018600080206       exsr RoutInz;
018700100305
018800100429       //?Ciclo per sk unificanti e stampo
018900100429       zz = 1;
019000100429       FOR zz by 1 to %elem($AgeU);
019100100429         IF  $AgeU(zz) <> *zeros;
019200100429
019300100429         //?Stampo salto pagina
019400100505           //exsr sr_StpSalto;
019500100505           clear sav_atcdad;
019600100429
019700100429         //?Carico sk agenti dell'unificante
019800100429           exsr sr_CarAge;
019900100305
020000100429        //?Stampo le attività
020100100429           exsr sr_StpAttivita;
020200100429
020300100429         ENDIF;
020400100429
020500100429       ENDFOR;
020600100305
020700100305       //?Stampo la fine
020800100305       write MK86FINE;
020900080206
021000090807       //?Operazioni finali
021100080206       exsr RoutEnd;
021200080206
021300080206       //--------------------------------------------------------------
021400080206       //?Operazioni iniziali.
021500080206       //--------------------------------------------------------------
021600080206       BEGSR RoutInz;
021700090807
021800090807         //?Reperimento dati job
021900090807         exsr DatiJob;
022000090807
022100090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
022200090806
022300100305         //?Imposto nome programma
022400100305         NomePgm = SDSpgm;
022500100305
022600100305         //?Imposto data e ora elaborazione
022700100505         DataEla = %dec(%date():*eur);
022800100305         OraEla  = %dec(%time());
022900100514
023000100514         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
023100100514         If imk86tat =  *blanks  ;
023200100514            *in11 = *on ;
023300100514         endif ;
023400100513         // imposto la schiera dei tipi attività da stampare
023500100513         §mk86tatds = imk86tat ;
023600100514         // li decodifico per la stampa
023700100514         If imk86iot = 'O' ;
023800100514            PINCOMI = 'ESCLUSE' ;
023900100514         else ;
024000100514            PINCOMI = 'INCLUSE' ;
024100100514         endif ;
024200100514         // decodifico le singole attività incluse o escluse
024300100514         xx = 1;
024400100514         FOR xx by 1 to %elem($tat) ;
024500100514         If $tat(xx) <> ' ' ;
024600100514           clear TIBS02ds;
024700100514           clear dTTA;
024800100514           T02mod = 'C';
024900100514           T02cod = 'TTA';
025000100514           T02ke1 = $tat(xx) ;
025100100514           T02sif = KNSIF;
025200100514           TNTBE_RicercaControllo (kpjba : tibs02ds);
025300100514           IF  T02err = *blanks;
025400100514             dTTA = T02uni;
025500100514           ENDIF;
025600100514           if Ptatio = *blanks ;
025700100514              Ptatio  = §ttadesb ;
025800100514           else ;
025900100514              Ptatio  = %trim(Ptatio) + '/' + §ttadesb ;
026000100514           endif ;
026100100514         endif ;
026200100514         ENDFOR ;
026300100429
026400100429         clear $AgeU;
026500100429
026600100429         //?Carico in sk l'unificante richiesto
026700100429         IF  IMK86cmm > 0;
026800100429           $AgeU(01) = IMK86cmm;
026900100429         ENDIF;
027000100429
027100100429         //?Carico gli unificanti delle filiali gestite dall'utente
027200100429         IF  IMK86cmm = 0;
027300100429           exsr sr_CarAgeUni;
027400100429           sorta $AgeU;
027500100429         ENDIF;
027600100305
027700100305         //?Imposto data inizio e fine stampa
027800100308         wDate_ISO = %date(IMK86datai:*iso);
027900100308         wDate_EUR = wDate_ISO;
028000100308         pDatai    = %dec(wDate_EUR);
028100100308         wDate_ISO = %date(IMK86dataf:*iso);
028200100308         wDate_EUR = wDate_ISO;
028300100308         pDataf    = %dec(wDate_EUR);
028400090714
028500080206       ENDSR;
028600100308
028700100308       //--------------------------------------------------------------
028800100308       //?Stampo il salto pagina.
028900100308       //--------------------------------------------------------------
029000100308       BEGSR sr_StpSalto;
029100100429
029200100429         //?Decodifico agente
029300100429         wCmm = $AgeU(zz);
029400100429         pCodCmm = $AgeU(zz);
029500100429         exsr sr_DecCmm;
029600130917         pDesCmm = CMMdes;
029700100308
029800100308         //?Stampo la testata
029900100308         write MK86PAGINA;
030000100308         wrkriga = '*' + wTrat + '*';
030100100308         write MK86SEPARA;
030200100308         write MK86TESTA;
030300100308         write MK86SEPARA;
030400100309         //?Stampo le intestazioni delle colonne
030500100309         write MK86INTCOL;
030600100305
030700100308       ENDSR;
030800100429
030900100305       //--------------------------------------------------------------
031000100305       //?Stampo le attività.
031100100305       //--------------------------------------------------------------
031200100305       BEGSR sr_StpAttivita;
031300100305
031400100305         $End = *off;
031500100305
031600100305         //?Imposto la stringa per SQL
031700100305         exsr  sr_PrepSQL;
031800100305
031900100305         //?Dichiarazione cursore
032000100305         exec sql
032100100305           prepare S1   from :wSQL;
032200100305         exec sql
032300100305           declare ATT  cursor for S1;
032400100305
032500100305         //?Apertura del cursore
032600100305         exec sql
032700100305           open ATT;
032800100305
032900100305         IF sqlcode < 0;
033000100305           $End = *on;
033100100305         ENDIF;
033200100305
033300100305         DOW not $End;
033400100305           exec sql
033500100305             fetch next from ATT into :atctat, :atccpo, :atcksc,
033600100308                                      :atcatn, :atcatnp,
033700100309                                      :atcdad, :atchda, :atcnrv,
033800100331                                      :atcoii, :atcofi, :atccad,
033900100525                                      :atcco3, :atcdco,
034000100525                                      :atccac, :atcest;
034100100305           IF sqlcod = 100 or sqlcod < 0;
034200100305             $End = *on;
034300100305             leave;
034400100305           ENDIF;
034500100305
034600100305         //?Controllo se ok il rcd che sto leggendo per l'agenda
034700100305           exsr sr_CtrRcdOk;
034800100305           IF  not $RcdOk;
034900100305             iter;
035000100305           ENDIF;
035100100305
035200100309         //?Stampo la separazione per data attività
035300100305           IF atcdad <> sav_atcdad;
035400100308           //?Salto pagina
035500100505             IF  CurLine >= 60 or sav_atcdad = 0;
035600100308               exsr sr_StpSalto;
035700100308               *in99 = *off;
035800100308             ENDIF;
035900100308             clear wrkriga;
036000100308             write MK86SEPARA;
036100100308             wDate_ISO  = %date(atcdad:*iso);
036200100308             wDate_EUR  = wDate_ISO;
036300100308             pData      = %dec(wDate_EUR);
036400100305             write MK86GIORNO;
036500100305             sav_atcdad = atcdad;
036600100305           ENDIF;
036700100309
036800100506           if  *in99;
036900100506             exsr sr_StpSalto;
037000100506             *in99 = *off;
037100100506           endif;
037200100309           clear MK86RIGA;
037300100309           clear MK86RIGAUF;
037400100305
037500100309         //?Imposto il tipo attività
037600100309           clear TIBS02ds;
037700100309           clear dTTA;
037800100309           T02mod = 'C';
037900100309           T02cod = 'TTA';
038000100309           T02ke1 = ATCtat;
038100100309           T02sif = KNSIF;
038200100309           TNTBE_RicercaControllo (kpjba : tibs02ds);
038300100309           IF  T02err = *blanks;
038400100309             dTTA = T02uni;
038500100309           ENDIF;
038600100309           pDesTat  = §TTAdesb;
038700100309           pDesTatu = §TTAdesb;
038800100504         //?se attvità da eseguire imposto che il tipo attività è da stampare in HI
038900100504           IF  ATCdco > 0;
039000100504             *in10 = *off;
039100100504           ELSE;
039200100504             *in10 = *on;
039300100504           ENDIF;
039400100309
039500100309           pOra  = ATChda / 100;
039600100309
039700100309         //?Attività legata a cliente
039800100309           IF  §TTALEG = 'S';
039900100305         //?Stampo le attività a dettaglio
040000100309             exsr sr_StpDettaglio;
040100100706         //?Stampo i concorrenti
040200100706             exsr sr_StpConc;
040300100309         //?Stampo l'eventuale affiancamento
040400100309             exsr sr_StpAffianca;
040500100309           ENDIF;
040600100309
040700100309         //?Attività NON legata a cliente
040800100309           IF  §TTAleg <> 'S';
040900100309         //?Stampo le attività a dettaglio
041000100309             exsr sr_StpDetUFF;
041100100309           ENDIF;
041200100305
041300100305         ENDDO;
041400100305
041500100305         exec sql
041600100305           close ATT;
041700100305
041800100305       ENDSR;
041900100305
042000100305       //--------------------------------------------------------------
042100100305       //?Stampo le attività a dettaglio.
042200100305       //--------------------------------------------------------------
042300100305       BEGSR sr_StpDettaglio;
042400100308
042500100308         clear pRagSoc;
042600100504         clear pCodKsc;
042700100308         clear pTel;
042800100308         clear wCatMer;
042900100308         clear pClv;
043000100308         clear pVia;
043100100308         clear pLoc;
043200100308         clear pCap;
043300100331         clear pDesCo3;
043400100504         clear pCausale;
043500100308
043600100308         SELECT;
043700100308
043800100308         //?Dati cliente
043900100308           WHEN  ATCksc > 0;
044000100504             pCodKsc = ATCksc;
044100100308             clear tibs69ds;
044200100308             I69kac = ATCksc;
044300100308             I69kin = ATCksc;
044400100308             I69kcp = ATCksc;
044500100308             tibs69r (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
044600100308             IF  O69err = *blanks;
044700100308               pRagSoc = ACOrag;
044800100308               wCatMer = ACOitc;
044900100308               pVia    = INDvia;
045000100308               pLoc    = INDcit;
045100100308               IF  INDsta = *blanks;
045200100308                 pLoc  = %trim(pLoc) + ' ' + INDprv;
045300100308               ELSE;
045400100308                 pLoc  = %trim(pLoc) + ' ' + INDsta;
045500100308               ENDIF;
045600100308               pCap    = %subst(INDcae:1:5);
045700100309               pTel    = INDtel;
045800100309               pClv    = CLPclv;
045900100308             ENDIF;
046000100308
046100100308         //?Dati potenziale
046200100308           WHEN  ATCcpo <> 0;
046300100308             chain ATCcpo TNCPO01L;
046400100308             IF  %found(TNCPO01L);
046500100308               pRagSoc = CPOrag;
046600100308               pVia    = CPOvia;
046700100308               pLoc    = CPOcit;
046800100308               IF  CPOnaz = *blanks;
046900100308                 pLoc  = %trim(pLoc) + ' ' + CPOprv;
047000100308               ELSE;
047100100308                 pLoc  = %trim(pLoc) + ' ' + CPOnaz;
047200100308               ENDIF;
047300100308               pCap    = %subst(CPOcap:1:5);
047400100309               pTel    = CPOtel;
047500100308               wCatMer = CPOsct;
047600100309               pClv    = CPOftr;
047700100308             ENDIF;
047800100308         ENDSL;
047900100308
048000100308         //?Decodifico categoria commerciale
048100100308         clear ds1L;
048200100308         k_tblkut = 1;
048300100308         k_tblcod = '1L';
048400100308         k_tblkey = %editc(wCatMer:'X');
048500100308         chain  %kds(K03tabel) TABEL00F;
048600100308         IF  %found( TABEL00F );
048700100308           ds1L = TBLuni;
048800100308         ENDIF;
048900100308         pDesCat = §1Ldes;
049000100331
049100100331         //?Decodifica commerciale (chi ha preso appuntamento)
049200100706         //?se appuntamento o affiancamento (CHIODO 91/92 e A)
049300100706         IF  ATCtat = 'A' and (ATCcad = '91' or ATCcad = '92');
049400100331           wCmm = ATCco3;
049500100331           exsr sr_DecCmm;
049600130917           pDesCo3 = CMMdes;
049700100331         ENDIF;
049800100504
049900100504         //?CHIODO
050000100504         //?se attvità telefonata devo stampare anche la causale
050100100504         clear pCausale;
050200100504         IF  ATCtat = 'T';
050300100504           pCausale = ATCcad;
050400100504         ENDIF;
050500100308
050600100308         //?Responsabile trasporti
050700100308         clear pRespTra;
050800100308         clear NTCnk1;
050900100308         clear NTCnk2;
051000100308         NTCtnt = '05';
051100100507         SELECT;
051200100507           WHEN  ATCksc > 0;
051300100507             NTCapl = 'C';
051400100507             NTCnk1 = %editc(DUTkci:'X') + %editc(ATCksc:'X');
051500100507           WHEN  ATCnrv > 0;
051600100507             NTCapl = 'T';
051700100507             NTCnk1 = %editc(DUTkci:'X') + %editc(ATCnrv:'X');
051800100507           OTHER;
051900100507             NTCapl = 'P' ;
052000100507             NTCnk1 = %editc(CPOcpo:'X');
052100100507         ENDSL;
052200100507         chain (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
052300100308         IF  %found(TFNTC01L);
052400100308           pRespTra = NTCrnt;
052500100308         ENDIF;
052600100308
052700100308         //?Dati Info Comm.
052800100308         //?vado con key fissa sulla riga del fatturato annuo presunto 'SPT'
052900100308         clear pFatCpo;
053000100308         clear pSpedCpo;
053100100308         chain (ATCcpo:'SPT') TICPI01L;
053200100308         IF  %found(TICPI01L);
053300100319           pFatCpo  = 'Sp.Trasp.: ' + ' ' + %trim(%editc(CPIpft:'2'));
053400100308           pSpedCpo = 'Sped.Anno: ' + ' ' + %trim(%editc(CPIsna:'2'));
053500100308         ENDIF;
053600100308
053700100308         //?Dati Trattativa
053800100505         clear pTpv;
053900100505         clear pNrv;
054000100308         clear pOfferta;
054100100309         clear pFatNrv;
054200100309         clear pSpedNrv;
054300110112         clear pAuScNrv;
054400100308         IF  ATCnrv > 0;
054500100308           chain ATCnrv TIVIS05L;
054600100308           IF  %found(TIVIS05L) and VISffz <> 'S';
054700100505             pNrv = VISnrv;
054800100505             pTpv = VIStpv;
054900100308           //?controllo se c'è offerta
055000100506             setll ATCnrv TIVOF01L;
055100100506             reade ATCnrv TIVOF01L;
055200100506             DOW  not %EoF(TIVOF01L);
055300100506               IF  VOFeso <> '*';
055400100506                 pOfferta = 'OFF';
055500100506                 leave;
055600100506               ENDIF;
055700100506               reade ATCnrv TIVOF01L;
055800100506             ENDDO;
055900100308           ENDIF;
056000100308           //?Dati Info trattativa
056100100308           //?vado con key fissa sulla riga del fatturato 'TRT'
056200100308           clear pFatNrv;
056300100308           clear pSpedNrv;
056400100308           chain (ATCnrv:'TRT') TIVII01L;
056500100308           IF  %found(TIVII01L);
056600110112             pFatNrv  = 'Valore    : ' + ' ' + %trim(%editc(VIIpft:'K'));
056700100308             pSpedNrv = 'Spedizioni: ' + ' ' + %trim(%editc(VIIsna:'2'));
056800100308           ENDIF;
056900110112           //?Se trattativa di tipo 'A'
057000110112           //?cerco anche 'A/S'
057100110112           IF  VIStpv = 'A';
057200110112             clear pAuScNrv;
057300110112             chain (ATCnrv:'A/S') TIVII01L;
057400110112             IF  %found(TIVII01L);
057500110428               pAuScNrv = 'Aum./Sco. : ' + ' ' + %trim(%editc(VIIvald:'K'))
057600110118                        + ' %';
057700110112             ENDIF;
057800110112           ENDIF;
057900100308         ENDIF;
058000100308
058100100308         //?Note attività (2 righe)
058200100520         clear pNota01;
058300100520         clear pNota02;
058400100520         clear pNota03;
058500100520         clear pNota04;
058600100520
058700100520         //?Prima provo con note legate al progressivo attuale
058800100308         wNota = 1;
058900100520         CPNcpo  = ATCcpo;
059000100520         CPNatn  = ATCatn;
059100100520         CPNatnp = ATCatnp;
059200100520         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
059300100520         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
059400100520
059500100520         DOW  not %eof(TICPN05L) and wNota <= 2;
059600100520           IF  wNota = 1;
059700100520             pNota01 = %subst(CPNnot:1:45);
059800100520             pNota02 = %subst(CPNnot:46:45);
059900100520           ENDIF;
060000100520           IF  wNota = 2;
060100100520             pNota03 = %subst(CPNnot:1:45);
060200100520             pNota04 = %subst(CPNnot:46:45);
060300100520           ENDIF;
060400100520
060500100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
060600100520           wNota += 1;
060700100520         ENDDO;
060800100520
060900100520         //?Se non ho trovato note, provo con note legate al progressivo -1
061000100520         IF  pNota01 = *blanks and pNota02 = *blanks and
061100100520             pNota03 = *blanks and pNota04 = *blanks;
061200100520           CPNatnp = ATCatnp - 1;
061300100520           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
061400100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
061500100308
061600100520           DOW  not %eof(TICPN05L) and wNota <= 2;
061700100520             IF  wNota = 1;
061800100520               pNota01 = %subst(CPNnot:1:45);
061900100520               pNota02 = %subst(CPNnot:46:45);
062000100520             ENDIF;
062100100520             IF  wNota = 2;
062200100520               pNota03 = %subst(CPNnot:1:45);
062300100520               pNota04 = %subst(CPNnot:46:45);
062400100520             ENDIF;
062500100520
062600100520             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
062700100520             wNota += 1;
062800100520           ENDDO;
062900100520         ENDIF;
063000100505
063100100505         //?Se non ci sono note legate all'attività controllo se ci sono note legate alla trattativa
063200100505         IF  ATCnrv > 0 and pNota01 = *blanks and pNota02 = *blanks and
063300100505                            pNota03 = *blanks and pNota04 = *blanks;
063400100505           CPNksc = ATCksc;
063500100505           CPNnrv = ATCnrv;
063600100505           setll (CPNcpo:CPNksc:CPNnrv) TICPN04L;
063700100505           reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
063800100505
063900100505           DOW  not %eof(TICPN04L) and wNota <= 2;
064000100505             IF  wNota = 1;
064100100505               pNota01 = %subst(CPNnot:1:45);
064200100505               pNota02 = %subst(CPNnot:46:45);
064300100505             ENDIF;
064400100505             IF  wNota = 2;
064500100505               pNota03 = %subst(CPNnot:1:45);
064600100505               pNota04 = %subst(CPNnot:46:45);
064700100505             ENDIF;
064800100505
064900100505             reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
065000100505             wNota += 1;
065100100505           ENDDO;
065200100505         ENDIF;
065300100308
065400100308         //?Salto pagina
065500100506         //IF  CurLine >= 64;
065600100506         IF  *in99;
065700100308           exsr sr_StpSalto;
065800100310           clear wrkriga;
065900100310           write MK86SEPARA;
066000100308           *in99 = *off;
066100100308         ENDIF;
066200100305
066300100305         write MK86RIGA;
066400100309
066500100309       ENDSR;
066600100706
066700100706       //--------------------------------------------------------------
066800100706       //?Stampo i Concorrenti.
066900100706       //--------------------------------------------------------------
067000100706       BEGSR sr_StpConc;
067100100706
067200100706         clear pConc;
067300100706         wConc = 1;
067400100706
067500100706         //?Cerco concorrenti
067600100706         setll (ATCcpo:'10') TICPI01L;
067700100706         reade (ATCcpo:'10') TICPI01L;
067800100706         DOW not %eof(TICPI01L);
067900100706           IF  CPIval > 0 and CPIspf <> '_BAR';
068000100706         //?imposto il campo da stampare
068100100706             IF  pConc = *blanks;
068200100706               pConc = 'Corrieri Utilizzati:';
068300100706             ENDIF;
068400100706             clear TIBS02ds;
068500100706             clear dIFS;
068600100706             T02mod = 'C';
068700100706             T02cod = 'IFS';
068800100706             T02ke1 = CPItpf;
068900100706             T02ke2 = CPIspf;
069000100706             T02sif = KNSIF;
069100100706             TNTBE_RicercaControllo (kpjba : tibs02ds);
069200100706             IF  T02err = *blanks;
069300100706               dIFS = T02uni;
069400100706             ENDIF;
069500100706             IF  wConc > 1;
069600100708             //?Aggiungo concorrente fino al massimo di 186 byte
069700100708               IF  (%len(%trim(pConc)) + %len(%trim(§IFSdes)) + 2) > 185;
069800100706                 Pconc = %trim(Pconc) + ' Altri...';
069900100706                 leave;
070000100706               ENDIF;
070100100706               pConc = %trim(pConc) + ',';
070200100706             ENDIF;
070300100706             pConc = %trim(pConc) + ' ' + §IFSdes;
070400100706             wConc += 1;
070500100706           ENDIF;
070600100706           reade (ATCcpo:'10') TICPI01L;
070700100706         ENDDO;
070800100706
070900100706         //?Salto pagina
071000100706         IF  CurLine >= 65;
071100100706           exsr sr_StpSalto;
071200100706           *in99 = *off;
071300100706         ENDIF;
071400100706
071500100706         IF  pConc <> *blanks;
071600100706           write MK86CONC;
071700100706         ENDIF;
071800100706
071900100706       ENDSR;
072000100309
072100100309       //--------------------------------------------------------------
072200100309       //?Stampo le attività a dettaglio UFFICIO.
072300100309       //--------------------------------------------------------------
072400100309       BEGSR sr_StpDetUFF;
072500100309
072600100309         clear pAttUff;
072700100309
072800100309         //?decodifico attività
072900100309         clear TIBS02ds;
073000100309         clear dCCO;
073100100309         T02mod = 'C';
073200100309         T02cod = 'CCO';
073300100309         T02ke1 = ATCcad;
073400100309         T02sif = KNSIF;
073500100309         TNTBE_RicercaControllo (kpjba : tibs02ds);
073600100309         IF  T02err = *blanks;
073700100309           dCCO = T02uni;
073800100309         ENDIF;
073900100309
074000100309         //?CHIODO --> non stampo la decodifica della causale se FERIE
074100100309         IF  ATCtat <> 'F';
074200100309           pAttUff = §CCOdes;
074300100309           pAttUff = %trim(pAttUff) + ' Dalle ' +
074400100309                     %subst(%editc(ATCoii:'X'):1:2) +
074500100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
074600100309                     ' Alle ' +
074700100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
074800100309                     %subst(%editc(ATCofi:'X'):3:2);
074900100309         ELSE;
075000100309           pAttUff = 'Dalle ' +
075100100309                     %subst(%editc(ATCoii:'X'):1:2) +
075200100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
075300100309                     ' Alle ' +
075400100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
075500100309                     %subst(%editc(ATCofi:'X'):3:2);
075600100309         ENDIF;
075700100706
075800100706         //?Note attività (1 riga)
075900100706         clear pNota01;
076000100706         clear pNota02;
076100100706
076200100706         //?Leggo le note legate al progressivo attuale
076300100706         wNota = 1;
076400100706         clear CPNcpo;
076500100706         CPNatn  = ATCatn;
076600100706         CPNatnp = ATCatnp;
076700100706         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
076800100706         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
076900100706
077000100706         DOW  not %eof(TICPN05L) and wNota <= 1;
077100100706           IF  wNota = 1;
077200100706             pNota01 = %subst(CPNnot:1:45);
077300100706             pNota02 = %subst(CPNnot:46:45);
077400100706           ENDIF;
077500100706
077600100706           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
077700100706           wNota += 1;
077800100706         ENDDO;
077900100706
078000100706         //?Se non ho trovato note, provo con note legate al progressivo -1
078100100706         IF  pNota01 = *blanks and pNota02 = *blanks;
078200100706           CPNatnp = ATCatnp - 1;
078300100706           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
078400100706           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
078500100706
078600100706           DOW  not %eof(TICPN05L) and wNota <= 1;
078700100706             IF  wNota = 1;
078800100706               pNota01 = %subst(CPNnot:1:45);
078900100706               pNota02 = %subst(CPNnot:46:45);
079000100706             ENDIF;
079100100706
079200100706             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
079300100706             wNota += 1;
079400100706           ENDDO;
079500100706         ENDIF;
079600100309
079700100309         //?Salto pagina
079800100309         IF  *in99;
079900100309           exsr sr_StpSalto;
080000100309           *in99 = *off;
080100100309         ENDIF;
080200100309
080300100309         write MK86RIGAUF;
080400100309
080500100309       ENDSR;
080600100309
080700100309       //--------------------------------------------------------------
080800100309       //?Stampo l'eventuale affiancamento.
080900100309       //--------------------------------------------------------------
081000100309       BEGSR sr_StpAffianca;
081100100309
081200100309         clear pAffianca;
081300100309         wAff = 1;
081400100309
081500100309         SELECT;
081600100309         //?se appuntamento (CHIODO 91 e A)
081700100309           WHEN  ATCtat = 'A' and ATCcad = '91';
081800100309         //?controllo se c'è affiancamento
081900100309             ATLatl  = ATCatn;
082000100309             ATLatlp = ATCatnp;
082100100309             setll (ATLatl:ATLatlp) TIATL02L;
082200100309             reade (ATLatl:ATLatlp) TIATL02L;
082300100309             DOW  not %eof(TIATL02L);
082400100309         //?imposto il campo da stampare
082500100309               IF  pAffianca = *blanks;
082600100616                 pAffianca = '  Affiancato da:';
082700100309               ENDIF;
082800100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
082900100309               w_ATCtat  = ATCtat;
083000100309               w_ATCatn  = ATLatn;
083100100309               w_ATCatnp = ATLatnp;
083200100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
083300100309               IF  %found(TIATC01L);
083400100309                 wCmm = w_ATCcmm;
083500100309                 exsr sr_DecCmm;
083600100309                 IF  wAff > 1;
083700100319             //?Aggiungo nome agente fino al massimo di 196 byte
083800130917                   IF  (%len(%trim(pAffianca)) + %len(%trim(CMMdes)) + 2) > 196;
083900100309                     leave;
084000100309                   ENDIF;
084100100309                   pAffianca = %trim(pAffianca) + ',';
084200100309                 ENDIF;
084300130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
084400100309               ENDIF;
084500100309               wAff += 1;
084600100309               reade (ATLatl:ATLatlp) TIATL02L;
084700100309             ENDDO;
084800100309         //?Stampo l'affiancamento
084900100309             IF  pAffianca <> *blanks;
085000100309             //?Salto pagina
085100100309               IF  *in99;
085200100309                 exsr sr_StpSalto;
085300100309                 *in99 = *off;
085400100309               ENDIF;
085500100309               write MK86AFF;
085600100309             ENDIF;
085700100309
085800100309         //?se affiancamento (CHIODO 92 e A)
085900100309           WHEN  ATCtat = 'A' and ATCcad = '92';
086000100309         //?controllo l'affiancamento
086100100309             ATLatn  = ATCatn;
086200100309             ATLatnp = ATCatnp;
086300100309             chain (ATLatn:ATLatnp) TIATL01L;
086400100309             IF  %found(TIATL01L);
086500100309         //?imposto il campo da stampare
086600100309               IF  pAffianca = *blanks;
086700100616                 pAffianca = '  In affiancamento a:';
086800100309               ENDIF;
086900100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
087000100309               w_ATCtat  = ATCtat;
087100100309               w_ATCatn  = ATLatl;
087200100309               w_ATCatnp = ATLatlp;
087300100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
087400100309               IF  %found(TIATC01L);
087500100309                 wCmm = w_ATCcmm;
087600100309                 exsr sr_DecCmm;
087700130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
087800100309               ENDIF;
087900100309             ENDIF;
088000100309         //?Stampo l'affiancamento
088100100309             IF  pAffianca <> *blanks;
088200100309             //?Salto pagina
088300100309               IF  *in99;
088400100309                 exsr sr_StpSalto;
088500100309                 *in99 = *off;
088600100309               ENDIF;
088700100309               write MK86AFF;
088800100309             ENDIF;
088900100309
089000100309         ENDSL;
089100100309
089200100309       ENDSR;
089300080206
089400080206       //--------------------------------------------------------------
089500080206       //?Reperimento Dati del job (Utente/Operativi).
089600080206       //--------------------------------------------------------------
089700080206       BEGSR DatiJob;
089800080206
089900080206         in(E) §AzUte;
090000080206         if NOT %error;
090100080206           in(E) §DatiUte;
090200080206         endif;
090300080206         if %error or RSut = *blanks;
090400080206           clear TIBS34ds;
090500080206           tibs34r(tibs34ds);
090600080206           in §AzUte;
090700080206           in §DatiUte;
090800080206         endif;
090900100429
091000100429         //?Richiamo pgm di controllo autorizzazioni utente così mi torna
091100100429         //?il codice autorizzazione relativo alle tariffe clienti
091200100429         //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
091300100429         reset TNTAA1DS;
091400100429         ITAA1caut = '§utegtc';
091500100429         kpjbu     = TNTAA1DS;
091600100429         tntaa1c (kpjba);
091700100429         TNTAA1DS = kpjbu;
091800100429
091900100429         //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di area
092000100429         IF  OTAA1cabi = 'AZ' and DUTpou <> 046;
092100100429           OTAA1cabi = 'RA';
092200100429         ENDIF;
092300100429
092400100429         //?Carico filiali abilitate all'utente
092500100429         //?Non controllo se caricate o no le filiali
092600100429         //?tanto se non ho caricato nessuna filiale e la richiesta
092700100429         //?è per tutti i commerciali, non stampo niente
092800100429         clear TRUL31DS;
092900100429         I31abi = OTAA1cabi;
093000100429         I31cdi = DUTdis;
093100100429         I31car = DUTare;
093200100429         I31cpo = DUTpou;
093300100429         callp trul31r (kpjba : trul31ds);
093400080206
093500080206       ENDSR;
093600100305
093700100305       //--------------------------------------------------------------
093800100305       //?Carico Sk Agenti.
093900100305       //--------------------------------------------------------------
094000100305       BEGSR sr_CarAGE;
094100100305
094200100305         clear xx;
094300100305         clear $Age;
094400100429         $EndAge = *off;
094500100305
094600100305         exec sql
094700100305          declare AGE cursor for
094800130917          select CMMcod, CMMuni
094900130917            from AZCMM00F
095000130917           order by CMMcod;
095100100305
095200100305         exec sql
095300100305           open AGE;
095400100305           IF sqlcode < 0;
095500100305             $EndAge = *on;
095600100305           ENDIF;
095700100305
095800100305         DOW not $EndAge;
095900100305           exec sql
096000130917             fetch next from AGE into :CMMcod, :CMMuni;
096100100305             IF sqlcod = 100 or sqlcod < 0;
096200100305               $EndAge = *on;
096300100305               leave;
096400100305             ENDIF;
096500130917             IF  CMMuni = $AgeU(zz);
096600100305               xx += 1;
096700130917               $Age(xx) = CMMcod;
096800100305             ENDIF;
096900100305         ENDDO;
097000100305
097100100305         exec sql close AGE;
097200100305
097300100305         //?Se non ho cricato niente memorizzo almeno il codice richiesto
097400100305         IF $Age(01) = 0;
097500100429           $Age(01) = $AgeU(zz);
097600100305         ENDIF;
097700100305
097800100305       ENDSR;
097900100429
098000100429       //--------------------------------------------------------------
098100100429       //?Carico sk agenti unificanti.
098200100429       //--------------------------------------------------------------
098300100429       BEGSR sr_CarAgeUni;
098400100429
098500100429         clear xx;
098600100429         clear $AGEU;
098700100429         $EndAge = *off;
098800100429
098900100429           //?Imposto la stringa per SQL
099000130917           wSQL = 'select CMMcod +
099100130917                     from AZCMM00F +
099200130917                    where substr( digits(CMMuni), 1, 3 ) in (';
099300100429
099400100429           yy = 0;
099500100429           xx = 1;
099600100429           FOR xx by 1 to %elem(pog);
099700100429             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
099800100429               IF yy > 0;
099900100429                 wSQL += ', ';
100000100429               ELSE;
100100100429                 yy = 1;
100200100429               ENDIF;
100300100429               wSQL += '''';
100400100429               wSQL += pog(xx);
100500100429               wSQL += '''';
100600100429             ENDIF;
100700100429           ENDFOR;
100800100505
100900130917         SELECT;
101000130917
101100130917           //?Carico TUTTI gli agenti unificanti
101200130917           WHEN  IMK86ioc = 'O' and imk86iec = ' ' ;
101300130917             wSQL += ') and CMMcod = CMMuni +
101400130917                        and CMMpar = '' ''';
101500100505
101600130917           //?Carico TUTTI gli agenti NON INTERNI unificanti
101700130917           WHEN  (IMK86ioc = 'I' and imk86iec = 'E') or
101800130917                 (IMK86ioc = 'O' and IMK86iec = 'I');
101900130917             wSQL += ') and CMMcod = CMMuni +
102000130917                        and CMMfun <> ''COMIN'' +
102100130917                        and CMMfun <> ''ASC  '' +
102200130917                        and CMMfun <> ''SA   '' +
102300130917                        and CMMpar = '' ''';
102400100514
102500130917           //?Carico TUTTI gli agenti INTERNI unificanti
102600130917           WHEN  (IMK86ioc = 'I' and imk86iec = 'I') or
102700130917                 (IMK86ioc = 'O' and IMK86iec = 'E');
102800130917             wSQL += ') and CMMcod = CMMuni and +
102900130917                           (CMMfun = ''COMIN'' +
103000130917                         or CMMfun = ''ASC  '' +
103100130917                         or CMMfun = ''SA   '') +
103200130917                        and CMMpar = '' ''';
103300130917
103400130917         ENDSL;
103500100429
103600130917         wSQL += ' order by CMMcod +
103700100505                   for fetch only';
103800100429
103900100429         //?Dichiarazione cursore
104000100429         exec sql
104100100429           prepare A1   from :wSQL;
104200100429         exec sql
104300100429           declare AGEU cursor for A1;
104400100429
104500100429         //?Apertura del cursore
104600100429         exec sql
104700100429           open AGEU;
104800100429
104900100429         IF sqlcode < 0;
105000100429           $EndAge = *on;
105100100429         ENDIF;
105200100429
105300100429         yy = 0;
105400100429         DOW not $EndAge;
105500100429           exec sql
105600130917             fetch next from AGEU into :CMMcod;
105700100429           IF sqlcod = 100 or sqlcod < 0;
105800100429             $EndAge = *on;
105900100429             leave;
106000100429           ENDIF;
106100100429
106200130917           yy += 1;
106300130917           $AGEU(yy) = CMMcod;
106400100429
106500100429         ENDDO;
106600100429
106700100429         exec sql
106800100429           close AGEU;
106900100429
107000100429       ENDSR;
107100100305
107200100305       //--------------------------------------------------------------
107300100305       //?Preparazione stringa SQL.
107400100305       //--------------------------------------------------------------
107500100305       BEGSR sr_PrepSQL;
107600100305
107700100308         wSQL = 'select atctat, atccpo, atcksc, +
107800100309                 atcatn, atcatnp, atcdad, atchda, atcnrv, +
107900100628                 atcoii, atcofi, atccad, atcco3, atcdco, +
108000100525                 atccac, atcest +
108100100305                  from TIATC00F +
108200100305                  where atccmm in(';
108300100305
108400100305         yy = 0;
108500100305         xx = 1;
108600100305         FOR xx by 1 to %elem($Age);
108700100305           IF $Age(xx) <> *zeros;
108800100305             IF yy > 0;
108900100305               wSQL += ', ';
109000100305             ELSE;
109100100305               yy = 1;
109200100305             ENDIF;
109300100305             wSQL += %editc($Age(xx):'X');
109400100305           ENDIF;
109500100305         ENDFOR;
109600100305
109700100308         wSQL += ') and atcdad between ' + %editc(IMK86datai:'X') +
109800100308                 ' and ' + %editc(IMK86dataf:'X');
109900100305
110000100305         wSQL += ' order by atcdad, atchda, atctat +
110100100305                   for fetch only';
110200100305
110300100305       ENDSR;
110400100305
110500100305       //--------------------------------------------------------------
110600100305       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
110700100305       //--------------------------------------------------------------
110800100305       BEGSR Sr_CtrRcdOk;
110900100305
111000100305         $RcdOk = *on;
111100100513         // se c'è inclusione o omissione di alcuni tipi attività
111200100513         // controllo
111300100513         SELECT ;
111400100513            When imk86iot = 'I' and %lookup(atctat : $tat) = 0 ;
111500100513                 $RcdOk = *off;
111600100513                 leavesr ;
111700100513            When imk86iot = 'O' and %lookup(atctat : $tat) > 0 ;
111800100513                 $RcdOk = *off;
111900100513                 leavesr ;
112000100513         ENDSL ;
112100100305         //?se attività legata a trattativa
112200100305         //?la trattativa NON deve essere fittizia
112300100305         IF  atcnrv > 0;
112400100305           exec sql
112500100305             select visffz into :visffz from tivis05l
112600100305             where visnrv = : atcnrv;
112700100305         //?non trovo il rcd corrispondente
112800100305           IF  sqlcod <> 0;
112900100305             leavesr;
113000100305           ENDIF;
113100100305         //?se trattativa fittizia non carico a video
113200100305           IF  visffz = 'S';
113300100305             $RcdOk = *off;
113400100305           ENDIF;
113500100305         ENDIF;
113600100525
113700100525         //?CHIODO
113800100706         //?MODIFICA CONTROLLO 06 Luglio 2010 ora controllo solo esito senza causale
113900100706         //?Se appuntamento '91' o affiancamento '92'  e esito 'N'
114000100706         IF  (ATCcad = '91' or ATCcad = '92') and ATCest = 'N';
114100100706           $RcdOk = *off;
114200100706         ENDIF;
114300100525         //?Se appuntamento '91' con causale chiusura attività '24' e esito 'N'
114400100525         //?non devo caricare in agenda
114500100706         // IF  ATCcad = '91' and ATCcac = '24' and ATCest = 'N';
114600100706         //   $RcdOk = *off;
114700100706         // ENDIF;
114800100525         //?Se affiancamento '92' con causale chiusura attività 'NO' e esito 'N'
114900100525         //?non devo caricare in agenda
115000100706         // IF  ATCcad = '92' and ATCcac = 'NO' and ATCest = 'N';
115100100706         //   $RcdOk = *off;
115200100706         // ENDIF;
115300100525         //?Se attvità qualsiasi chiusa con attività 'AA'
115400100525         //?non devo caricare in agenda
115500100525         IF  ATCcac = 'AA';
115600100525           $RcdOk = *off;
115700100525         ENDIF;
115800100305
115900100305       ENDSR;
116000100309
116100100309       //--------------------------------------------------------------
116200100309       //?Decodifico agente.
116300100309       //--------------------------------------------------------------
116400100309       BEGSR sr_DecCmm;
116500100309
116600130917         chain  (wCmm)  AZCMM000;
116700130917         IF  NOT  %found( AZCMM01L );
116800130917           clear  CMMdes;
116900100309         ENDIF;
117000100309
117100100309       ENDSR;
117200090807
117300080206       //--------------------------------------------------------------
117400080206       //?Operazioni finali.
117500080206       //--------------------------------------------------------------
117600080206       BEGSR RoutEnd;
117700090521
117800080206         *inLR = *on;
117900080206         return;
118000080206
118100080206       ENDSR;
118200080206
118300080206      /end-free
