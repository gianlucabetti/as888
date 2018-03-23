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
002500140715     fTNCPO11L  if   e           k disk
002600100308     fTRMK86P   o    e             printer oflind(*in99)
002700100310     f                                     infds(prtinfo)
002800100310
002900080206      //---------------------------------------------------------------
003000090406      //?Definizione costanti.
003100080206      //---------------------------------------------------------------
003200080206
003300080206      //---------------------------------------------------------------
003400080206      //?Definizione schiere.
003500080206      //---------------------------------------------------------------
003600090806
003700100304      // - Agenti
003800100429     d $Age            s              7  0 dim(3500)
003900100429
004000100429      // - Agenti Unificanti
004100100429     d $AgeU           s              7  0 dim(3500)
004200100305
004300100305      // - Data completa
004400100305     d $Date           s              8  0 dim(07)
004500100513
004600100513     d §MK86TATds      ds            10    inz
004700100513     d   $TAT                  1     10    inz  dim(10)
004800080206
004900080206      //---------------------------------------------------------------
005000080206      //?Definizione aree dati.
005100080206      //---------------------------------------------------------------
005200080206
005300080206      // - Dati utente
005400080206     d §AzUte        e ds                  extname(AZUTE00F)
005500080206     d                                     dtaara
005600080206     d §DatiUte      e ds                  extname(dDatiUte)
005700080206     d                                     dtaara
005800080206
005900080206      //---------------------------------------------------------------
006000080206      //?Definizione strutture dati.
006100080206      //---------------------------------------------------------------
006200100310
006300100310     d PrtINFO         ds
006400100310     d  CurLine              367    368i 0
006500080206
006600080206      // - Status
006700080206     d Psds           sds
006800080206     d   SDSpgm          *proc
006900080206
007000080206      // - Parametri ricevuti
007100080206     d KPJBA         e ds
007200100305     d TRMK86DS      e ds
007300080206
007400100308      // - Ricerca/Controllo tabelle
007500100308     d TIBS02ds      e ds                  inz
007600100308
007700080206      // - Reperimento dati utente
007800100305     d TIBS34DS      e ds
007900100308
008000100308       // - Reperimento dati anagrafici
008100100308     d TIBS69ds      e ds
008200100308     d DS_cnaco      e ds                  inz extname(CNACO00F)
008300100308     d DS_cnind      e ds                  inz extname(CNIND00F)
008400100308     d DS_cnclp      e ds                  inz extname(CNCLP00F)
008500100308     d DS_fncls      e ds                  inz extname(FNCLS00F)
008600100429
008700100429      // - controllo abilitazioni
008800100429     d TNTAA1DS      e ds                  inz
008900100429
009000100429      // - Carico Filiali abilitate all'utente
009100100429     d TRUL31DS      e ds
009200100429     d POG                    10    759    DIM(250)
009300100308
009400100308      // - Tabella CCO = Causale contatto
009500100308     d dCCO          e ds                  inz
009600100706
009700100706      // - Tabella IFS = Sottotipi INFO commerciali
009800100706     d dIFS          e ds                  inz
009900100308
010000100308      // - Tabella TTA = Tipo attività
010100100308     d dTTA          e ds                  inz
010200100308
010300100308      // - Tabella 1L = Categoria Merceologica
010400100308     d ds1L          e ds                  inz
010500090807
010600090807      // File attività
010700100305     d TIATC00F      e ds                  extname(TIATC00F)
010800100305
010900100305      // File trattativa
011000100305     d TIVIS00F      e ds                  extname(TIVIS00F)
011100090629
011200080206      //---------------------------------------------------------------
011300080206      //?Definizione variabili globali.
011400080206      //---------------------------------------------------------------
011500080206
011600080206      // - Flags booleani
011700100305     d $End            s               n   inz(*off)
011800100305     d $EndAge         s               n   inz(*off)
011900100305     d $RcdOk          s               n   inz(*off)
012000080206
012100080206      // - Indici di schiera
012200100429     d xx              s              4  0 inz
012300100429     d yy              s              4  0 inz
012400100429     d zz              s              4  0 inz
012500080206
012600090806       // - Stringa SQL da eseguire
012700090806     d wSQL            s           2048    Varying        inz
012800080206
012900090401      // - Campi di comodo data
013000100305     d wDate_EUR       s               d   datfmt(*eur)
013100100305     d wDate_ISO       s               d   datfmt(*iso)
013200100305
013300100305      // - Campi di comodo
013400100308     d sav_ATCdad      s                   like(ATCdad)
013500100309     d wAff            s              1  0 inz
013600100308     d wCatMer         s              5  0 inz
013700100309     d wCmm            s                   like(ATCcmm)
013800100906     d wConc           s              2  0 inz
013900100429     d wLib            s             21    inz
014000100308     d wNota           s              1  0 inz
014100180219     d wpft            s              9s 0 inz
014200100305     d wTrat           s            196    inz(*all'-')
014300100429
014400090508      //---------------------------------------------------------------
014500090807      //?Definizione procedure esterne.
014600090508      //---------------------------------------------------------------
014700100120
014800100120      //---------------------------------------------------------------
014900100120      //?prototipi
015000100120      //---------------------------------------------------------------
015100090807
015200080626      /copy gaitrasrc/srcprotopr,tibs02r
015300080626      /copy gaitrasrc/srcprotopr,tibs34r
015400100308      /copy gaitrasrc/srcprotopr,tibs69r
015500100429      /copy gaitrasrc/srcprotopr,tntaa1c
015600100429      /copy gaitrasrc/srcprotopr,trul31r
015700090807
015800080206      //---------------------------------------------------------------
015900080206      //?Definizione key-list.
016000080206      //---------------------------------------------------------------
016100090807
016200090807       // - File TABEL00F
016300090807     d k03tabel      e ds                  extname(TABEL00F:*key)
016400090807     d                                     prefix(k_)
016500090807     d                                     inz
016600080206
016700080206      //---------------------------------------------------------------
016800080206      //?Riepilogo indicatori.
016900080206      //---------------------------------------------------------------
017000090807
017100100504      // 10    : HI tipo attività se da eseguire
017200100514      // 11    : Se acceso non stampa in testata le attività escluse
017300100308      // 99    : Salto pagina
017400090807
017500080206      //---------------------------------------------------------------
017600080206
017700080206      //---------------------------------------------------------------
017800080206      //?M A I N - L I N E
017900080206      //---------------------------------------------------------------
018000080206
018100080206     c     *Entry        plist
018200080206     c                   parm                    KPJBA
018300100304     c                   parm                    TRMK86DS
018400080206
018500080206      /free
018600080206
018700090807       //?Operazioni iniziali
018800080206       exsr RoutInz;
018900100305
019000100429       //?Ciclo per sk unificanti e stampo
019100100429       zz = 1;
019200100429       FOR zz by 1 to %elem($AgeU);
019300100429         IF  $AgeU(zz) <> *zeros;
019400100429
019500100429         //?Stampo salto pagina
019600100505           //exsr sr_StpSalto;
019700100505           clear sav_atcdad;
019800100429
019900100429         //?Carico sk agenti dell'unificante
020000100429           exsr sr_CarAge;
020100100305
020200100429        //?Stampo le attività
020300100429           exsr sr_StpAttivita;
020400100429
020500100429         ENDIF;
020600100429
020700100429       ENDFOR;
020800100305
020900100305       //?Stampo la fine
021000100305       write MK86FINE;
021100080206
021200090807       //?Operazioni finali
021300080206       exsr RoutEnd;
021400080206
021500080206       //--------------------------------------------------------------
021600080206       //?Operazioni iniziali.
021700080206       //--------------------------------------------------------------
021800080206       BEGSR RoutInz;
021900090807
022000090807         //?Reperimento dati job
022100090807         exsr DatiJob;
022200090807
022300090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
022400090806
022500100305         //?Imposto nome programma
022600100305         NomePgm = SDSpgm;
022700100305
022800100305         //?Imposto data e ora elaborazione
022900100505         DataEla = %dec(%date():*eur);
023000100305         OraEla  = %dec(%time());
023100100514
023200100514         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
023300100514         If imk86tat =  *blanks  ;
023400100514            *in11 = *on ;
023500100514         endif ;
023600100513         // imposto la schiera dei tipi attività da stampare
023700100513         §mk86tatds = imk86tat ;
023800100514         // li decodifico per la stampa
023900100514         If imk86iot = 'O' ;
024000100514            PINCOMI = 'ESCLUSE' ;
024100100514         else ;
024200100514            PINCOMI = 'INCLUSE' ;
024300100514         endif ;
024400100514         // decodifico le singole attività incluse o escluse
024500100514         xx = 1;
024600100514         FOR xx by 1 to %elem($tat) ;
024700100514         If $tat(xx) <> ' ' ;
024800100514           clear TIBS02ds;
024900100514           clear dTTA;
025000100514           T02mod = 'C';
025100100514           T02cod = 'TTA';
025200100514           T02ke1 = $tat(xx) ;
025300100514           T02sif = KNSIF;
025400100514           TNTBE_RicercaControllo (kpjba : tibs02ds);
025500100514           IF  T02err = *blanks;
025600100514             dTTA = T02uni;
025700100514           ENDIF;
025800100514           if Ptatio = *blanks ;
025900100514              Ptatio  = §ttadesb ;
026000100514           else ;
026100100514              Ptatio  = %trim(Ptatio) + '/' + §ttadesb ;
026200100514           endif ;
026300100514         endif ;
026400100514         ENDFOR ;
026500100429
026600100429         clear $AgeU;
026700100429
026800100429         //?Carico in sk l'unificante richiesto
026900100429         IF  IMK86cmm > 0;
027000100429           $AgeU(01) = IMK86cmm;
027100100429         ENDIF;
027200100429
027300100429         //?Carico gli unificanti delle filiali gestite dall'utente
027400100429         IF  IMK86cmm = 0;
027500100429           exsr sr_CarAgeUni;
027600100429           sorta $AgeU;
027700100429         ENDIF;
027800100305
027900100305         //?Imposto data inizio e fine stampa
028000100308         wDate_ISO = %date(IMK86datai:*iso);
028100100308         wDate_EUR = wDate_ISO;
028200100308         pDatai    = %dec(wDate_EUR);
028300100308         wDate_ISO = %date(IMK86dataf:*iso);
028400100308         wDate_EUR = wDate_ISO;
028500100308         pDataf    = %dec(wDate_EUR);
028600090714
028700080206       ENDSR;
028800100308
028900100308       //--------------------------------------------------------------
029000100308       //?Stampo il salto pagina.
029100100308       //--------------------------------------------------------------
029200100308       BEGSR sr_StpSalto;
029300100429
029400100429         //?Decodifico agente
029500100429         wCmm = $AgeU(zz);
029600100429         pCodCmm = $AgeU(zz);
029700100429         exsr sr_DecCmm;
029800130917         pDesCmm = CMMdes;
029900100308
030000100308         //?Stampo la testata
030100100308         write MK86PAGINA;
030200100308         wrkriga = '*' + wTrat + '*';
030300100308         write MK86SEPARA;
030400100308         write MK86TESTA;
030500100308         write MK86SEPARA;
030600100309         //?Stampo le intestazioni delle colonne
030700100309         write MK86INTCOL;
030800100305
030900100308       ENDSR;
031000100429
031100100305       //--------------------------------------------------------------
031200100305       //?Stampo le attività.
031300100305       //--------------------------------------------------------------
031400100305       BEGSR sr_StpAttivita;
031500100305
031600100305         $End = *off;
031700100305
031800100305         //?Imposto la stringa per SQL
031900100305         exsr  sr_PrepSQL;
032000100305
032100100305         //?Dichiarazione cursore
032200100305         exec sql
032300100305           prepare S1   from :wSQL;
032400100305         exec sql
032500100305           declare ATT  cursor for S1;
032600100305
032700100305         //?Apertura del cursore
032800100305         exec sql
032900100305           open ATT;
033000100305
033100100305         IF sqlcode < 0;
033200100305           $End = *on;
033300100305         ENDIF;
033400100305
033500100305         DOW not $End;
033600100305           exec sql
033700100305             fetch next from ATT into :atctat, :atccpo, :atcksc,
033800100308                                      :atcatn, :atcatnp,
033900100309                                      :atcdad, :atchda, :atcnrv,
034000100331                                      :atcoii, :atcofi, :atccad,
034100100525                                      :atcco3, :atcdco,
034200100525                                      :atccac, :atcest;
034300100305           IF sqlcod = 100 or sqlcod < 0;
034400100305             $End = *on;
034500100305             leave;
034600100305           ENDIF;
034700100305
034800100305         //?Controllo se ok il rcd che sto leggendo per l'agenda
034900100305           exsr sr_CtrRcdOk;
035000100305           IF  not $RcdOk;
035100100305             iter;
035200100305           ENDIF;
035300100305
035400100309         //?Stampo la separazione per data attività
035500100305           IF atcdad <> sav_atcdad;
035600100308           //?Salto pagina
035700100505             IF  CurLine >= 60 or sav_atcdad = 0;
035800100308               exsr sr_StpSalto;
035900100308               *in99 = *off;
036000100308             ENDIF;
036100100308             clear wrkriga;
036200100308             write MK86SEPARA;
036300100308             wDate_ISO  = %date(atcdad:*iso);
036400100308             wDate_EUR  = wDate_ISO;
036500100308             pData      = %dec(wDate_EUR);
036600100305             write MK86GIORNO;
036700100305             sav_atcdad = atcdad;
036800100305           ENDIF;
036900100309
037000100506           if  *in99;
037100100506             exsr sr_StpSalto;
037200100506             *in99 = *off;
037300100506           endif;
037400100309           clear MK86RIGA;
037500100309           clear MK86RIGAUF;
037600100305
037700100309         //?Imposto il tipo attività
037800100309           clear TIBS02ds;
037900100309           clear dTTA;
038000100309           T02mod = 'C';
038100100309           T02cod = 'TTA';
038200100309           T02ke1 = ATCtat;
038300100309           T02sif = KNSIF;
038400100309           TNTBE_RicercaControllo (kpjba : tibs02ds);
038500100309           IF  T02err = *blanks;
038600100309             dTTA = T02uni;
038700100309           ENDIF;
038800100309           pDesTat  = §TTAdesb;
038900100309           pDesTatu = §TTAdesb;
039000100504         //?se attvità da eseguire imposto che il tipo attività è da stampare in HI
039100100504           IF  ATCdco > 0;
039200100504             *in10 = *off;
039300100504           ELSE;
039400100504             *in10 = *on;
039500100504           ENDIF;
039600100309
039700100309           pOra  = ATChda / 100;
039800100309
039900100309         //?Attività legata a cliente
040000100309           IF  §TTALEG = 'S';
040100100305         //?Stampo le attività a dettaglio
040200100309             exsr sr_StpDettaglio;
040300100706         //?Stampo i concorrenti
040400100706             exsr sr_StpConc;
040500100309         //?Stampo l'eventuale affiancamento
040600100309             exsr sr_StpAffianca;
040700100309           ENDIF;
040800100309
040900100309         //?Attività NON legata a cliente
041000100309           IF  §TTAleg <> 'S';
041100100309         //?Stampo le attività a dettaglio
041200100309             exsr sr_StpDetUFF;
041300100309           ENDIF;
041400100305
041500100305         ENDDO;
041600100305
041700100305         exec sql
041800100305           close ATT;
041900100305
042000100305       ENDSR;
042100100305
042200100305       //--------------------------------------------------------------
042300100305       //?Stampo le attività a dettaglio.
042400100305       //--------------------------------------------------------------
042500100305       BEGSR sr_StpDettaglio;
042600100308
042700100308         clear pRagSoc;
042800100504         clear pCodKsc;
042900100308         clear pTel;
043000100308         clear wCatMer;
043100100308         clear pClv;
043200100308         clear pVia;
043300100308         clear pLoc;
043400100308         clear pCap;
043500100331         clear pDesCo3;
043600100504         clear pCausale;
043700100308
043800100308         SELECT;
043900100308
044000100308         //?Dati cliente
044100100308           WHEN  ATCksc > 0;
044200100504             pCodKsc = ATCksc;
044300100308             clear tibs69ds;
044400100308             I69kac = ATCksc;
044500100308             I69kin = ATCksc;
044600100308             I69kcp = ATCksc;
044700100308             tibs69r (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
044800100308             IF  O69err = *blanks;
044900100308               pRagSoc = ACOrag;
045000100308               wCatMer = ACOitc;
045100100308               pVia    = INDvia;
045200100308               pLoc    = INDcit;
045300100308               IF  INDsta = *blanks;
045400100308                 pLoc  = %trim(pLoc) + ' ' + INDprv;
045500100308               ELSE;
045600100308                 pLoc  = %trim(pLoc) + ' ' + INDsta;
045700100308               ENDIF;
045800100308               pCap    = %subst(INDcae:1:5);
045900100309               pTel    = INDtel;
046000100309               pClv    = CLPclv;
046100100308             ENDIF;
046200100308
046300100308         //?Dati potenziale
046400100308           WHEN  ATCcpo <> 0;
046500100308             chain ATCcpo TNCPO01L;
046600100308             IF  %found(TNCPO01L);
046700100308               pRagSoc = CPOrag;
046800100308               pVia    = CPOvia;
046900100308               pLoc    = CPOcit;
047000100308               IF  CPOnaz = *blanks;
047100100308                 pLoc  = %trim(pLoc) + ' ' + CPOprv;
047200100308               ELSE;
047300100308                 pLoc  = %trim(pLoc) + ' ' + CPOnaz;
047400100308               ENDIF;
047500100308               pCap    = %subst(CPOcap:1:5);
047600100308               wCatMer = CPOsct;
047700100309               pClv    = CPOftr;
047800100308             ENDIF;
047900140715             chain  ATCcpo  TNCPO100;
048000140715             If  %found(TNCPO11L);
048100140715               pTel    = CPO1tel;
048200140715             EndIf;
048300100308         ENDSL;
048400100308
048500100308         //?Decodifico categoria commerciale
048600100308         clear ds1L;
048700100308         k_tblkut = 1;
048800100308         k_tblcod = '1L';
048900100308         k_tblkey = %editc(wCatMer:'X');
049000100308         chain  %kds(K03tabel) TABEL00F;
049100100308         IF  %found( TABEL00F );
049200100308           ds1L = TBLuni;
049300100308         ENDIF;
049400100308         pDesCat = §1Ldes;
049500100331
049600100331         //?Decodifica commerciale (chi ha preso appuntamento)
049700100706         //?se appuntamento o affiancamento (CHIODO 91/92 e A)
049800100706         IF  ATCtat = 'A' and (ATCcad = '91' or ATCcad = '92');
049900100331           wCmm = ATCco3;
050000100331           exsr sr_DecCmm;
050100130917           pDesCo3 = CMMdes;
050200100331         ENDIF;
050300100504
050400100504         //?CHIODO
050500100504         //?se attvità telefonata devo stampare anche la causale
050600100504         clear pCausale;
050700100504         IF  ATCtat = 'T';
050800100504           pCausale = ATCcad;
050900100504         ENDIF;
051000100308
051100100308         //?Responsabile trasporti
051200100308         clear pRespTra;
051300100308         clear NTCnk1;
051400100308         clear NTCnk2;
051500100308         NTCtnt = '05';
051600100507         SELECT;
051700100507           WHEN  ATCksc > 0;
051800100507             NTCapl = 'C';
051900100507             NTCnk1 = %editc(DUTkci:'X') + %editc(ATCksc:'X');
052000100507           WHEN  ATCnrv > 0;
052100100507             NTCapl = 'T';
052200100507             NTCnk1 = %editc(DUTkci:'X') + %editc(ATCnrv:'X');
052300100507           OTHER;
052400100507             NTCapl = 'P' ;
052500100507             NTCnk1 = %editc(CPOcpo:'X');
052600100507         ENDSL;
052700100507         chain (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
052800100308         IF  %found(TFNTC01L);
052900100308           pRespTra = NTCrnt;
053000100308         ENDIF;
053100100308
053200100308         //?Dati Info Comm.
053300100308         //?vado con key fissa sulla riga del fatturato annuo presunto 'SPT'
053400100308         clear pFatCpo;
053500100308         clear pSpedCpo;
053600100308         chain (ATCcpo:'SPT') TICPI01L;
053700100308         IF  %found(TICPI01L);
053800180219           wpft = CPIpft;
053900180219           pFatCpo  = 'Sp.Trasp.: ' +  %trim(%editc(wpft:'2'));
054000180219           pSpedCpo = 'Sped.Anno: ' +  %trim(%editc(CPIsna:'2'));
054100100308         ENDIF;
054200100308
054300100308         //?Dati Trattativa
054400100505         clear pTpv;
054500100505         clear pNrv;
054600100308         clear pOfferta;
054700100309         clear pFatNrv;
054800100309         clear pSpedNrv;
054900110112         clear pAuScNrv;
055000100308         IF  ATCnrv > 0;
055100100308           chain ATCnrv TIVIS05L;
055200100308           IF  %found(TIVIS05L) and VISffz <> 'S';
055300100505             pNrv = VISnrv;
055400100505             pTpv = VIStpv;
055500100308           //?controllo se c'è offerta
055600100506             setll ATCnrv TIVOF01L;
055700100506             reade ATCnrv TIVOF01L;
055800100506             DOW  not %EoF(TIVOF01L);
055900100506               IF  VOFeso <> '*';
056000100506                 pOfferta = 'OFF';
056100100506                 leave;
056200100506               ENDIF;
056300100506               reade ATCnrv TIVOF01L;
056400100506             ENDDO;
056500100308           ENDIF;
056600100308           //?Dati Info trattativa
056700100308           //?vado con key fissa sulla riga del fatturato 'TRT'
056800100308           clear pFatNrv;
056900100308           clear pSpedNrv;
057000100308           chain (ATCnrv:'TRT') TIVII01L;
057100100308           IF  %found(TIVII01L);
057200180219             wpft = VIIpft;
057300180219             pFatNrv  = 'Valore    : ' + %trim(%editc(wpft:'K'));
057400180219             pSpedNrv = 'Spedizioni: ' + %trim(%editc(VIIsna:'2'));
057500100308           ENDIF;
057600110112           //?Se trattativa di tipo 'A'
057700110112           //?cerco anche 'A/S'
057800110112           IF  VIStpv = 'A';
057900110112             clear pAuScNrv;
058000110112             chain (ATCnrv:'A/S') TIVII01L;
058100110112             IF  %found(TIVII01L);
058200110428               pAuScNrv = 'Aum./Sco. : ' + ' ' + %trim(%editc(VIIvald:'K'))
058300110118                        + ' %';
058400110112             ENDIF;
058500110112           ENDIF;
058600100308         ENDIF;
058700100308
058800100308         //?Note attività (2 righe)
058900100520         clear pNota01;
059000100520         clear pNota02;
059100100520         clear pNota03;
059200100520         clear pNota04;
059300100520
059400100520         //?Prima provo con note legate al progressivo attuale
059500100308         wNota = 1;
059600100520         CPNcpo  = ATCcpo;
059700100520         CPNatn  = ATCatn;
059800100520         CPNatnp = ATCatnp;
059900100520         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
060000100520         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
060100100520
060200100520         DOW  not %eof(TICPN05L) and wNota <= 2;
060300100520           IF  wNota = 1;
060400100520             pNota01 = %subst(CPNnot:1:45);
060500100520             pNota02 = %subst(CPNnot:46:45);
060600100520           ENDIF;
060700100520           IF  wNota = 2;
060800100520             pNota03 = %subst(CPNnot:1:45);
060900100520             pNota04 = %subst(CPNnot:46:45);
061000100520           ENDIF;
061100100520
061200100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
061300100520           wNota += 1;
061400100520         ENDDO;
061500100520
061600100520         //?Se non ho trovato note, provo con note legate al progressivo -1
061700100520         IF  pNota01 = *blanks and pNota02 = *blanks and
061800100520             pNota03 = *blanks and pNota04 = *blanks;
061900100520           CPNatnp = ATCatnp - 1;
062000100520           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
062100100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
062200100308
062300100520           DOW  not %eof(TICPN05L) and wNota <= 2;
062400100520             IF  wNota = 1;
062500100520               pNota01 = %subst(CPNnot:1:45);
062600100520               pNota02 = %subst(CPNnot:46:45);
062700100520             ENDIF;
062800100520             IF  wNota = 2;
062900100520               pNota03 = %subst(CPNnot:1:45);
063000100520               pNota04 = %subst(CPNnot:46:45);
063100100520             ENDIF;
063200100520
063300100520             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
063400100520             wNota += 1;
063500100520           ENDDO;
063600100520         ENDIF;
063700100505
063800100505         //?Se non ci sono note legate all'attività controllo se ci sono note legate alla trattativa
063900100505         IF  ATCnrv > 0 and pNota01 = *blanks and pNota02 = *blanks and
064000100505                            pNota03 = *blanks and pNota04 = *blanks;
064100100505           CPNksc = ATCksc;
064200100505           CPNnrv = ATCnrv;
064300100505           setll (CPNcpo:CPNksc:CPNnrv) TICPN04L;
064400100505           reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
064500100505
064600100505           DOW  not %eof(TICPN04L) and wNota <= 2;
064700100505             IF  wNota = 1;
064800100505               pNota01 = %subst(CPNnot:1:45);
064900100505               pNota02 = %subst(CPNnot:46:45);
065000100505             ENDIF;
065100100505             IF  wNota = 2;
065200100505               pNota03 = %subst(CPNnot:1:45);
065300100505               pNota04 = %subst(CPNnot:46:45);
065400100505             ENDIF;
065500100505
065600100505             reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
065700100505             wNota += 1;
065800100505           ENDDO;
065900100505         ENDIF;
066000100308
066100100308         //?Salto pagina
066200100506         //IF  CurLine >= 64;
066300100506         IF  *in99;
066400100308           exsr sr_StpSalto;
066500100310           clear wrkriga;
066600100310           write MK86SEPARA;
066700100308           *in99 = *off;
066800100308         ENDIF;
066900100305
067000100305         write MK86RIGA;
067100100309
067200100309       ENDSR;
067300100706
067400100706       //--------------------------------------------------------------
067500100706       //?Stampo i Concorrenti.
067600100706       //--------------------------------------------------------------
067700100706       BEGSR sr_StpConc;
067800100706
067900100706         clear pConc;
068000100706         wConc = 1;
068100100706
068200100706         //?Cerco concorrenti
068300100706         setll (ATCcpo:'10') TICPI01L;
068400100706         reade (ATCcpo:'10') TICPI01L;
068500100706         DOW not %eof(TICPI01L);
068600100706           IF  CPIval > 0 and CPIspf <> '_BAR';
068700100706         //?imposto il campo da stampare
068800100706             IF  pConc = *blanks;
068900100706               pConc = 'Corrieri Utilizzati:';
069000100706             ENDIF;
069100100706             clear TIBS02ds;
069200100706             clear dIFS;
069300100706             T02mod = 'C';
069400100706             T02cod = 'IFS';
069500100706             T02ke1 = CPItpf;
069600100706             T02ke2 = CPIspf;
069700100706             T02sif = KNSIF;
069800100706             TNTBE_RicercaControllo (kpjba : tibs02ds);
069900100706             IF  T02err = *blanks;
070000100706               dIFS = T02uni;
070100100706             ENDIF;
070200100706             IF  wConc > 1;
070300100708             //?Aggiungo concorrente fino al massimo di 186 byte
070400100708               IF  (%len(%trim(pConc)) + %len(%trim(§IFSdes)) + 2) > 185;
070500100706                 Pconc = %trim(Pconc) + ' Altri...';
070600100706                 leave;
070700100706               ENDIF;
070800100706               pConc = %trim(pConc) + ',';
070900100706             ENDIF;
071000100706             pConc = %trim(pConc) + ' ' + §IFSdes;
071100100706             wConc += 1;
071200100706           ENDIF;
071300100706           reade (ATCcpo:'10') TICPI01L;
071400100706         ENDDO;
071500100706
071600100706         //?Salto pagina
071700100706         IF  CurLine >= 65;
071800100706           exsr sr_StpSalto;
071900100706           *in99 = *off;
072000100706         ENDIF;
072100100706
072200100706         IF  pConc <> *blanks;
072300100706           write MK86CONC;
072400100706         ENDIF;
072500100706
072600100706       ENDSR;
072700100309
072800100309       //--------------------------------------------------------------
072900100309       //?Stampo le attività a dettaglio UFFICIO.
073000100309       //--------------------------------------------------------------
073100100309       BEGSR sr_StpDetUFF;
073200100309
073300100309         clear pAttUff;
073400100309
073500100309         //?decodifico attività
073600100309         clear TIBS02ds;
073700100309         clear dCCO;
073800100309         T02mod = 'C';
073900100309         T02cod = 'CCO';
074000100309         T02ke1 = ATCcad;
074100100309         T02sif = KNSIF;
074200100309         TNTBE_RicercaControllo (kpjba : tibs02ds);
074300100309         IF  T02err = *blanks;
074400100309           dCCO = T02uni;
074500100309         ENDIF;
074600100309
074700100309         //?CHIODO --> non stampo la decodifica della causale se FERIE
074800100309         IF  ATCtat <> 'F';
074900100309           pAttUff = §CCOdes;
075000100309           pAttUff = %trim(pAttUff) + ' Dalle ' +
075100100309                     %subst(%editc(ATCoii:'X'):1:2) +
075200100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
075300100309                     ' Alle ' +
075400100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
075500100309                     %subst(%editc(ATCofi:'X'):3:2);
075600100309         ELSE;
075700100309           pAttUff = 'Dalle ' +
075800100309                     %subst(%editc(ATCoii:'X'):1:2) +
075900100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
076000100309                     ' Alle ' +
076100100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
076200100309                     %subst(%editc(ATCofi:'X'):3:2);
076300100309         ENDIF;
076400100706
076500100706         //?Note attività (1 riga)
076600100706         clear pNota01;
076700100706         clear pNota02;
076800100706
076900100706         //?Leggo le note legate al progressivo attuale
077000100706         wNota = 1;
077100100706         clear CPNcpo;
077200100706         CPNatn  = ATCatn;
077300100706         CPNatnp = ATCatnp;
077400100706         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
077500100706         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
077600100706
077700100706         DOW  not %eof(TICPN05L) and wNota <= 1;
077800100706           IF  wNota = 1;
077900100706             pNota01 = %subst(CPNnot:1:45);
078000100706             pNota02 = %subst(CPNnot:46:45);
078100100706           ENDIF;
078200100706
078300100706           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
078400100706           wNota += 1;
078500100706         ENDDO;
078600100706
078700100706         //?Se non ho trovato note, provo con note legate al progressivo -1
078800100706         IF  pNota01 = *blanks and pNota02 = *blanks;
078900100706           CPNatnp = ATCatnp - 1;
079000100706           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
079100100706           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
079200100706
079300100706           DOW  not %eof(TICPN05L) and wNota <= 1;
079400100706             IF  wNota = 1;
079500100706               pNota01 = %subst(CPNnot:1:45);
079600100706               pNota02 = %subst(CPNnot:46:45);
079700100706             ENDIF;
079800100706
079900100706             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
080000100706             wNota += 1;
080100100706           ENDDO;
080200100706         ENDIF;
080300100309
080400100309         //?Salto pagina
080500100309         IF  *in99;
080600100309           exsr sr_StpSalto;
080700100309           *in99 = *off;
080800100309         ENDIF;
080900100309
081000100309         write MK86RIGAUF;
081100100309
081200100309       ENDSR;
081300100309
081400100309       //--------------------------------------------------------------
081500100309       //?Stampo l'eventuale affiancamento.
081600100309       //--------------------------------------------------------------
081700100309       BEGSR sr_StpAffianca;
081800100309
081900100309         clear pAffianca;
082000100309         wAff = 1;
082100100309
082200100309         SELECT;
082300100309         //?se appuntamento (CHIODO 91 e A)
082400100309           WHEN  ATCtat = 'A' and ATCcad = '91';
082500100309         //?controllo se c'è affiancamento
082600100309             ATLatl  = ATCatn;
082700100309             ATLatlp = ATCatnp;
082800100309             setll (ATLatl:ATLatlp) TIATL02L;
082900100309             reade (ATLatl:ATLatlp) TIATL02L;
083000100309             DOW  not %eof(TIATL02L);
083100100309         //?imposto il campo da stampare
083200100309               IF  pAffianca = *blanks;
083300100616                 pAffianca = '  Affiancato da:';
083400100309               ENDIF;
083500100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
083600100309               w_ATCtat  = ATCtat;
083700100309               w_ATCatn  = ATLatn;
083800100309               w_ATCatnp = ATLatnp;
083900100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
084000100309               IF  %found(TIATC01L);
084100100309                 wCmm = w_ATCcmm;
084200100309                 exsr sr_DecCmm;
084300100309                 IF  wAff > 1;
084400100319             //?Aggiungo nome agente fino al massimo di 196 byte
084500130917                   IF  (%len(%trim(pAffianca)) + %len(%trim(CMMdes)) + 2) > 196;
084600100309                     leave;
084700100309                   ENDIF;
084800100309                   pAffianca = %trim(pAffianca) + ',';
084900100309                 ENDIF;
085000130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
085100100309               ENDIF;
085200100309               wAff += 1;
085300100309               reade (ATLatl:ATLatlp) TIATL02L;
085400100309             ENDDO;
085500100309         //?Stampo l'affiancamento
085600100309             IF  pAffianca <> *blanks;
085700100309             //?Salto pagina
085800100309               IF  *in99;
085900100309                 exsr sr_StpSalto;
086000100309                 *in99 = *off;
086100100309               ENDIF;
086200100309               write MK86AFF;
086300100309             ENDIF;
086400100309
086500100309         //?se affiancamento (CHIODO 92 e A)
086600100309           WHEN  ATCtat = 'A' and ATCcad = '92';
086700100309         //?controllo l'affiancamento
086800100309             ATLatn  = ATCatn;
086900100309             ATLatnp = ATCatnp;
087000100309             chain (ATLatn:ATLatnp) TIATL01L;
087100100309             IF  %found(TIATL01L);
087200100309         //?imposto il campo da stampare
087300100309               IF  pAffianca = *blanks;
087400100616                 pAffianca = '  In affiancamento a:';
087500100309               ENDIF;
087600100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
087700100309               w_ATCtat  = ATCtat;
087800100309               w_ATCatn  = ATLatl;
087900100309               w_ATCatnp = ATLatlp;
088000100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
088100100309               IF  %found(TIATC01L);
088200100309                 wCmm = w_ATCcmm;
088300100309                 exsr sr_DecCmm;
088400130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
088500100309               ENDIF;
088600100309             ENDIF;
088700100309         //?Stampo l'affiancamento
088800100309             IF  pAffianca <> *blanks;
088900100309             //?Salto pagina
089000100309               IF  *in99;
089100100309                 exsr sr_StpSalto;
089200100309                 *in99 = *off;
089300100309               ENDIF;
089400100309               write MK86AFF;
089500100309             ENDIF;
089600100309
089700100309         ENDSL;
089800100309
089900100309       ENDSR;
090000080206
090100080206       //--------------------------------------------------------------
090200080206       //?Reperimento Dati del job (Utente/Operativi).
090300080206       //--------------------------------------------------------------
090400080206       BEGSR DatiJob;
090500080206
090600080206         in(E) §AzUte;
090700080206         if NOT %error;
090800080206           in(E) §DatiUte;
090900080206         endif;
091000080206         if %error or RSut = *blanks;
091100080206           clear TIBS34ds;
091200080206           tibs34r(tibs34ds);
091300080206           in §AzUte;
091400080206           in §DatiUte;
091500080206         endif;
091600100429
091700100429         //?Richiamo pgm di controllo autorizzazioni utente così mi torna
091800100429         //?il codice autorizzazione relativo alle tariffe clienti
091900100429         //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
092000100429         reset TNTAA1DS;
092100100429         ITAA1caut = '§utegtc';
092200100429         kpjbu     = TNTAA1DS;
092300100429         tntaa1c (kpjba);
092400100429         TNTAA1DS = kpjbu;
092500100429
092600150910         //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di distretto
092700100429         IF  OTAA1cabi = 'AZ' and DUTpou <> 046;
092800150910           OTAA1cabi = 'DI';
092900100429         ENDIF;
093000100429
093100100429         //?Carico filiali abilitate all'utente
093200100429         //?Non controllo se caricate o no le filiali
093300100429         //?tanto se non ho caricato nessuna filiale e la richiesta
093400100429         //?è per tutti i commerciali, non stampo niente
093500100429         clear TRUL31DS;
093600100429         I31abi = OTAA1cabi;
093700100429         I31cdi = DUTdis;
093800100429         I31car = DUTare;
093900100429         I31cpo = DUTpou;
094000100429         callp trul31r (kpjba : trul31ds);
094100080206
094200080206       ENDSR;
094300100305
094400100305       //--------------------------------------------------------------
094500100305       //?Carico Sk Agenti.
094600100305       //--------------------------------------------------------------
094700100305       BEGSR sr_CarAGE;
094800100305
094900100305         clear xx;
095000100305         clear $Age;
095100100429         $EndAge = *off;
095200100305
095300100305         exec sql
095400100305          declare AGE cursor for
095500130917          select CMMcod, CMMuni
095600130917            from AZCMM00F
095700130917           order by CMMcod;
095800100305
095900100305         exec sql
096000100305           open AGE;
096100100305           IF sqlcode < 0;
096200100305             $EndAge = *on;
096300100305           ENDIF;
096400100305
096500100305         DOW not $EndAge;
096600100305           exec sql
096700130917             fetch next from AGE into :CMMcod, :CMMuni;
096800100305             IF sqlcod = 100 or sqlcod < 0;
096900100305               $EndAge = *on;
097000100305               leave;
097100100305             ENDIF;
097200130917             IF  CMMuni = $AgeU(zz);
097300100305               xx += 1;
097400130917               $Age(xx) = CMMcod;
097500100305             ENDIF;
097600100305         ENDDO;
097700100305
097800100305         exec sql close AGE;
097900100305
098000100305         //?Se non ho cricato niente memorizzo almeno il codice richiesto
098100100305         IF $Age(01) = 0;
098200100429           $Age(01) = $AgeU(zz);
098300100305         ENDIF;
098400100305
098500100305       ENDSR;
098600100429
098700100429       //--------------------------------------------------------------
098800100429       //?Carico sk agenti unificanti.
098900100429       //--------------------------------------------------------------
099000100429       BEGSR sr_CarAgeUni;
099100100429
099200100429         clear xx;
099300100429         clear $AGEU;
099400100429         $EndAge = *off;
099500100429
099600100429           //?Imposto la stringa per SQL
099700130917           wSQL = 'select CMMcod +
099800130917                     from AZCMM00F +
099900130917                    where substr( digits(CMMuni), 1, 3 ) in (';
100000100429
100100100429           yy = 0;
100200100429           xx = 1;
100300100429           FOR xx by 1 to %elem(pog);
100400100429             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
100500100429               IF yy > 0;
100600100429                 wSQL += ', ';
100700100429               ELSE;
100800100429                 yy = 1;
100900100429               ENDIF;
101000100429               wSQL += '''';
101100100429               wSQL += pog(xx);
101200100429               wSQL += '''';
101300100429             ENDIF;
101400100429           ENDFOR;
101500100505
101600130917         SELECT;
101700130917
101800130917           //?Carico TUTTI gli agenti unificanti
101900130917           WHEN  IMK86ioc = 'O' and imk86iec = ' ' ;
102000130917             wSQL += ') and CMMcod = CMMuni +
102100130917                        and CMMpar = '' ''';
102200100505
102300130917           //?Carico TUTTI gli agenti NON INTERNI unificanti
102400130917           WHEN  (IMK86ioc = 'I' and imk86iec = 'E') or
102500130917                 (IMK86ioc = 'O' and IMK86iec = 'I');
102600130917             wSQL += ') and CMMcod = CMMuni +
102700130917                        and CMMfun <> ''COMIN'' +
102800130917                        and CMMfun <> ''ASC  '' +
102900130917                        and CMMfun <> ''SA   '' +
103000130917                        and CMMpar = '' ''';
103100100514
103200130917           //?Carico TUTTI gli agenti INTERNI unificanti
103300130917           WHEN  (IMK86ioc = 'I' and imk86iec = 'I') or
103400130917                 (IMK86ioc = 'O' and IMK86iec = 'E');
103500130917             wSQL += ') and CMMcod = CMMuni and +
103600130917                           (CMMfun = ''COMIN'' +
103700130917                         or CMMfun = ''ASC  '' +
103800130917                         or CMMfun = ''SA   '') +
103900130917                        and CMMpar = '' ''';
104000130917
104100130917         ENDSL;
104200100429
104300130917         wSQL += ' order by CMMcod +
104400100505                   for fetch only';
104500100429
104600100429         //?Dichiarazione cursore
104700100429         exec sql
104800100429           prepare A1   from :wSQL;
104900100429         exec sql
105000100429           declare AGEU cursor for A1;
105100100429
105200100429         //?Apertura del cursore
105300100429         exec sql
105400100429           open AGEU;
105500100429
105600100429         IF sqlcode < 0;
105700100429           $EndAge = *on;
105800100429         ENDIF;
105900100429
106000100429         yy = 0;
106100100429         DOW not $EndAge;
106200100429           exec sql
106300130917             fetch next from AGEU into :CMMcod;
106400100429           IF sqlcod = 100 or sqlcod < 0;
106500100429             $EndAge = *on;
106600100429             leave;
106700100429           ENDIF;
106800100429
106900130917           yy += 1;
107000130917           $AGEU(yy) = CMMcod;
107100100429
107200100429         ENDDO;
107300100429
107400100429         exec sql
107500100429           close AGEU;
107600100429
107700100429       ENDSR;
107800100305
107900100305       //--------------------------------------------------------------
108000100305       //?Preparazione stringa SQL.
108100100305       //--------------------------------------------------------------
108200100305       BEGSR sr_PrepSQL;
108300100305
108400100308         wSQL = 'select atctat, atccpo, atcksc, +
108500100309                 atcatn, atcatnp, atcdad, atchda, atcnrv, +
108600100628                 atcoii, atcofi, atccad, atcco3, atcdco, +
108700100525                 atccac, atcest +
108800100305                  from TIATC00F +
108900100305                  where atccmm in(';
109000100305
109100100305         yy = 0;
109200100305         xx = 1;
109300100305         FOR xx by 1 to %elem($Age);
109400100305           IF $Age(xx) <> *zeros;
109500100305             IF yy > 0;
109600100305               wSQL += ', ';
109700100305             ELSE;
109800100305               yy = 1;
109900100305             ENDIF;
110000100305             wSQL += %editc($Age(xx):'X');
110100100305           ENDIF;
110200100305         ENDFOR;
110300100305
110400100308         wSQL += ') and atcdad between ' + %editc(IMK86datai:'X') +
110500100308                 ' and ' + %editc(IMK86dataf:'X');
110600100305
110700100305         wSQL += ' order by atcdad, atchda, atctat +
110800100305                   for fetch only';
110900100305
111000100305       ENDSR;
111100100305
111200100305       //--------------------------------------------------------------
111300100305       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
111400100305       //--------------------------------------------------------------
111500100305       BEGSR Sr_CtrRcdOk;
111600100305
111700100305         $RcdOk = *on;
111800100513         // se c'è inclusione o omissione di alcuni tipi attività
111900100513         // controllo
112000100513         SELECT ;
112100100513            When imk86iot = 'I' and %lookup(atctat : $tat) = 0 ;
112200100513                 $RcdOk = *off;
112300100513                 leavesr ;
112400100513            When imk86iot = 'O' and %lookup(atctat : $tat) > 0 ;
112500100513                 $RcdOk = *off;
112600100513                 leavesr ;
112700100513         ENDSL ;
112800100305         //?se attività legata a trattativa
112900100305         //?la trattativa NON deve essere fittizia
113000100305         IF  atcnrv > 0;
113100100305           exec sql
113200100305             select visffz into :visffz from tivis05l
113300100305             where visnrv = : atcnrv;
113400100305         //?non trovo il rcd corrispondente
113500100305           IF  sqlcod <> 0;
113600100305             leavesr;
113700100305           ENDIF;
113800100305         //?se trattativa fittizia non carico a video
113900100305           IF  visffz = 'S';
114000100305             $RcdOk = *off;
114100100305           ENDIF;
114200100305         ENDIF;
114300100525
114400100525         //?CHIODO
114500100706         //?MODIFICA CONTROLLO 06 Luglio 2010 ora controllo solo esito senza causale
114600100706         //?Se appuntamento '91' o affiancamento '92'  e esito 'N'
114700100706         IF  (ATCcad = '91' or ATCcad = '92') and ATCest = 'N';
114800100706           $RcdOk = *off;
114900100706         ENDIF;
115000100525         //?Se appuntamento '91' con causale chiusura attività '24' e esito 'N'
115100100525         //?non devo caricare in agenda
115200100706         // IF  ATCcad = '91' and ATCcac = '24' and ATCest = 'N';
115300100706         //   $RcdOk = *off;
115400100706         // ENDIF;
115500100525         //?Se affiancamento '92' con causale chiusura attività 'NO' e esito 'N'
115600100525         //?non devo caricare in agenda
115700100706         // IF  ATCcad = '92' and ATCcac = 'NO' and ATCest = 'N';
115800100706         //   $RcdOk = *off;
115900100706         // ENDIF;
116000100525         //?Se attvità qualsiasi chiusa con attività 'AA'
116100100525         //?non devo caricare in agenda
116200100525         IF  ATCcac = 'AA';
116300100525           $RcdOk = *off;
116400100525         ENDIF;
116500100305
116600100305       ENDSR;
116700100309
116800100309       //--------------------------------------------------------------
116900100309       //?Decodifico agente.
117000100309       //--------------------------------------------------------------
117100100309       BEGSR sr_DecCmm;
117200100309
117300130917         chain  (wCmm)  AZCMM000;
117400130917         IF  NOT  %found( AZCMM01L );
117500130917           clear  CMMdes;
117600100309         ENDIF;
117700100309
117800100309       ENDSR;
117900090807
118000080206       //--------------------------------------------------------------
118100080206       //?Operazioni finali.
118200080206       //--------------------------------------------------------------
118300080206       BEGSR RoutEnd;
118400090521
118500080206         *inLR = *on;
118600080206         return;
118700080206
118800080206       ENDSR;
118900080206
119000080206      /end-free
