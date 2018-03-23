000100140715     /*END
000200080206      //--------------------------------------------------------------
000300100304      //?TRMK86R - Stampa Agenda commerciale
000400080206      //--------------------------------------------------------------
000500080206
000600100505     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700090807     h dftactgrp(*no) actgrp(*caller)
000800080206
000900080206      //---------------------------------------------------------------
001000080206      //?Dichiarazione file.
001100080206      //---------------------------------------------------------------
001200100308     fTABEL00F  if   e           k disk
001300130917     fAZCMM01L  if   e           k disk
001400100308     fTFNTC01L  if   e           k disk
001500100308     fTIATC01L  if   e           k disk    rename(tiatc000:w_tiatc)
001600100308     f                                     prefix(w_)
001700100309     fTIATL01L  if   e           k disk
001800100309     fTIATL02L  if   e           k disk    rename(tiatl000:tiatl02)
001900100308     fTICPI01L  if   e           k disk
002000100520     fTICPN05L  if   e           k disk
002100100505     fTICPN04L  if   e           k disk    rename(ticpn000:ticpn04)
002200110803     fTIVII01L  if   e           k disk
002300110803     fTIVIS05L  if   e           k disk
002400110803     fTIVOF01L  if   e           k disk
002500100308     fTNCPO01L  if   e           k disk
002600140715     fTNCPO11L  if   e           k disk
002700100308     fTRMK86P   o    e             printer oflind(*in99)
002800100310     f                                     infds(prtinfo)
002900100310
003000080206      //---------------------------------------------------------------
003100090406      //?Definizione costanti.
003200080206      //---------------------------------------------------------------
003300080206
003400080206      //---------------------------------------------------------------
003500080206      //?Definizione schiere.
003600080206      //---------------------------------------------------------------
003700090806
003800100304      // - Agenti
003900100429     d $Age            s              7  0 dim(3500)
004000100429
004100100429      // - Agenti Unificanti
004200100429     d $AgeU           s              7  0 dim(3500)
004300100305
004400100305      // - Data completa
004500100305     d $Date           s              8  0 dim(07)
004600100513
004700100513     d §MK86TATds      ds            10    inz
004800100513     d   $TAT                  1     10    inz  dim(10)
004900080206
005000080206      //---------------------------------------------------------------
005100080206      //?Definizione aree dati.
005200080206      //---------------------------------------------------------------
005300080206
005400080206      // - Dati utente
005500080206     d §AzUte        e ds                  extname(AZUTE00F)
005600080206     d                                     dtaara
005700080206     d §DatiUte      e ds                  extname(dDatiUte)
005800080206     d                                     dtaara
005900080206
006000080206      //---------------------------------------------------------------
006100080206      //?Definizione strutture dati.
006200080206      //---------------------------------------------------------------
006300100310
006400100310     d PrtINFO         ds
006500100310     d  CurLine              367    368i 0
006600080206
006700080206      // - Status
006800080206     d Psds           sds
006900080206     d   SDSpgm          *proc
007000080206
007100080206      // - Parametri ricevuti
007200080206     d KPJBA         e ds
007300100305     d TRMK86DS      e ds
007400080206
007500100308      // - Ricerca/Controllo tabelle
007600100308     d TIBS02ds      e ds                  inz
007700100308
007800080206      // - Reperimento dati utente
007900100305     d TIBS34DS      e ds
008000100308
008100100308       // - Reperimento dati anagrafici
008200100308     d TIBS69ds      e ds
008300100308     d DS_cnaco      e ds                  inz extname(CNACO00F)
008400100308     d DS_cnind      e ds                  inz extname(CNIND00F)
008500100308     d DS_cnclp      e ds                  inz extname(CNCLP00F)
008600100308     d DS_fncls      e ds                  inz extname(FNCLS00F)
008700100429
008800100429      // - controllo abilitazioni
008900100429     d TNTAA1DS      e ds                  inz
009000100429
009100100429      // - Carico Filiali abilitate all'utente
009200100429     d TRUL31DS      e ds
009300100429     d POG                    10    759    DIM(250)
009400100308
009500100308      // - Tabella CCO = Causale contatto
009600100308     d dCCO          e ds                  inz
009700100706
009800100706      // - Tabella IFS = Sottotipi INFO commerciali
009900100706     d dIFS          e ds                  inz
010000100308
010100100308      // - Tabella TTA = Tipo attività
010200100308     d dTTA          e ds                  inz
010300100308
010400100308      // - Tabella 1L = Categoria Merceologica
010500100308     d ds1L          e ds                  inz
010600090807
010700090807      // File attività
010800100305     d TIATC00F      e ds                  extname(TIATC00F)
010900100305
011000100305      // File trattativa
011100100305     d TIVIS00F      e ds                  extname(TIVIS00F)
011200090629
011300080206      //---------------------------------------------------------------
011400080206      //?Definizione variabili globali.
011500080206      //---------------------------------------------------------------
011600080206
011700080206      // - Flags booleani
011800100305     d $End            s               n   inz(*off)
011900100305     d $EndAge         s               n   inz(*off)
012000100305     d $RcdOk          s               n   inz(*off)
012100080206
012200080206      // - Indici di schiera
012300100429     d xx              s              4  0 inz
012400100429     d yy              s              4  0 inz
012500100429     d zz              s              4  0 inz
012600080206
012700090806       // - Stringa SQL da eseguire
012800090806     d wSQL            s           2048    Varying        inz
012900080206
013000090401      // - Campi di comodo data
013100100305     d wDate_EUR       s               d   datfmt(*eur)
013200100305     d wDate_ISO       s               d   datfmt(*iso)
013300100305
013400100305      // - Campi di comodo
013500100308     d sav_ATCdad      s                   like(ATCdad)
013600100309     d wAff            s              1  0 inz
013700100308     d wCatMer         s              5  0 inz
013800100309     d wCmm            s                   like(ATCcmm)
013900100906     d wConc           s              2  0 inz
014000100429     d wLib            s             21    inz
014100100308     d wNota           s              1  0 inz
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
053800100319           pFatCpo  = 'Sp.Trasp.: ' + ' ' + %trim(%editc(CPIpft:'2'));
053900100308           pSpedCpo = 'Sped.Anno: ' + ' ' + %trim(%editc(CPIsna:'2'));
054000100308         ENDIF;
054100100308
054200100308         //?Dati Trattativa
054300100505         clear pTpv;
054400100505         clear pNrv;
054500100308         clear pOfferta;
054600100309         clear pFatNrv;
054700100309         clear pSpedNrv;
054800110112         clear pAuScNrv;
054900100308         IF  ATCnrv > 0;
055000100308           chain ATCnrv TIVIS05L;
055100100308           IF  %found(TIVIS05L) and VISffz <> 'S';
055200100505             pNrv = VISnrv;
055300100505             pTpv = VIStpv;
055400100308           //?controllo se c'è offerta
055500100506             setll ATCnrv TIVOF01L;
055600100506             reade ATCnrv TIVOF01L;
055700100506             DOW  not %EoF(TIVOF01L);
055800100506               IF  VOFeso <> '*';
055900100506                 pOfferta = 'OFF';
056000100506                 leave;
056100100506               ENDIF;
056200100506               reade ATCnrv TIVOF01L;
056300100506             ENDDO;
056400100308           ENDIF;
056500100308           //?Dati Info trattativa
056600100308           //?vado con key fissa sulla riga del fatturato 'TRT'
056700100308           clear pFatNrv;
056800100308           clear pSpedNrv;
056900100308           chain (ATCnrv:'TRT') TIVII01L;
057000100308           IF  %found(TIVII01L);
057100110112             pFatNrv  = 'Valore    : ' + ' ' + %trim(%editc(VIIpft:'K'));
057200100308             pSpedNrv = 'Spedizioni: ' + ' ' + %trim(%editc(VIIsna:'2'));
057300100308           ENDIF;
057400110112           //?Se trattativa di tipo 'A'
057500110112           //?cerco anche 'A/S'
057600110112           IF  VIStpv = 'A';
057700110112             clear pAuScNrv;
057800110112             chain (ATCnrv:'A/S') TIVII01L;
057900110112             IF  %found(TIVII01L);
058000110428               pAuScNrv = 'Aum./Sco. : ' + ' ' + %trim(%editc(VIIvald:'K'))
058100110118                        + ' %';
058200110112             ENDIF;
058300110112           ENDIF;
058400100308         ENDIF;
058500100308
058600100308         //?Note attività (2 righe)
058700100520         clear pNota01;
058800100520         clear pNota02;
058900100520         clear pNota03;
059000100520         clear pNota04;
059100100520
059200100520         //?Prima provo con note legate al progressivo attuale
059300100308         wNota = 1;
059400100520         CPNcpo  = ATCcpo;
059500100520         CPNatn  = ATCatn;
059600100520         CPNatnp = ATCatnp;
059700100520         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
059800100520         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
059900100520
060000100520         DOW  not %eof(TICPN05L) and wNota <= 2;
060100100520           IF  wNota = 1;
060200100520             pNota01 = %subst(CPNnot:1:45);
060300100520             pNota02 = %subst(CPNnot:46:45);
060400100520           ENDIF;
060500100520           IF  wNota = 2;
060600100520             pNota03 = %subst(CPNnot:1:45);
060700100520             pNota04 = %subst(CPNnot:46:45);
060800100520           ENDIF;
060900100520
061000100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
061100100520           wNota += 1;
061200100520         ENDDO;
061300100520
061400100520         //?Se non ho trovato note, provo con note legate al progressivo -1
061500100520         IF  pNota01 = *blanks and pNota02 = *blanks and
061600100520             pNota03 = *blanks and pNota04 = *blanks;
061700100520           CPNatnp = ATCatnp - 1;
061800100520           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
061900100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
062000100308
062100100520           DOW  not %eof(TICPN05L) and wNota <= 2;
062200100520             IF  wNota = 1;
062300100520               pNota01 = %subst(CPNnot:1:45);
062400100520               pNota02 = %subst(CPNnot:46:45);
062500100520             ENDIF;
062600100520             IF  wNota = 2;
062700100520               pNota03 = %subst(CPNnot:1:45);
062800100520               pNota04 = %subst(CPNnot:46:45);
062900100520             ENDIF;
063000100520
063100100520             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
063200100520             wNota += 1;
063300100520           ENDDO;
063400100520         ENDIF;
063500100505
063600100505         //?Se non ci sono note legate all'attività controllo se ci sono note legate alla trattativa
063700100505         IF  ATCnrv > 0 and pNota01 = *blanks and pNota02 = *blanks and
063800100505                            pNota03 = *blanks and pNota04 = *blanks;
063900100505           CPNksc = ATCksc;
064000100505           CPNnrv = ATCnrv;
064100100505           setll (CPNcpo:CPNksc:CPNnrv) TICPN04L;
064200100505           reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
064300100505
064400100505           DOW  not %eof(TICPN04L) and wNota <= 2;
064500100505             IF  wNota = 1;
064600100505               pNota01 = %subst(CPNnot:1:45);
064700100505               pNota02 = %subst(CPNnot:46:45);
064800100505             ENDIF;
064900100505             IF  wNota = 2;
065000100505               pNota03 = %subst(CPNnot:1:45);
065100100505               pNota04 = %subst(CPNnot:46:45);
065200100505             ENDIF;
065300100505
065400100505             reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
065500100505             wNota += 1;
065600100505           ENDDO;
065700100505         ENDIF;
065800100308
065900100308         //?Salto pagina
066000100506         //IF  CurLine >= 64;
066100100506         IF  *in99;
066200100308           exsr sr_StpSalto;
066300100310           clear wrkriga;
066400100310           write MK86SEPARA;
066500100308           *in99 = *off;
066600100308         ENDIF;
066700100305
066800100305         write MK86RIGA;
066900100309
067000100309       ENDSR;
067100100706
067200100706       //--------------------------------------------------------------
067300100706       //?Stampo i Concorrenti.
067400100706       //--------------------------------------------------------------
067500100706       BEGSR sr_StpConc;
067600100706
067700100706         clear pConc;
067800100706         wConc = 1;
067900100706
068000100706         //?Cerco concorrenti
068100100706         setll (ATCcpo:'10') TICPI01L;
068200100706         reade (ATCcpo:'10') TICPI01L;
068300100706         DOW not %eof(TICPI01L);
068400100706           IF  CPIval > 0 and CPIspf <> '_BAR';
068500100706         //?imposto il campo da stampare
068600100706             IF  pConc = *blanks;
068700100706               pConc = 'Corrieri Utilizzati:';
068800100706             ENDIF;
068900100706             clear TIBS02ds;
069000100706             clear dIFS;
069100100706             T02mod = 'C';
069200100706             T02cod = 'IFS';
069300100706             T02ke1 = CPItpf;
069400100706             T02ke2 = CPIspf;
069500100706             T02sif = KNSIF;
069600100706             TNTBE_RicercaControllo (kpjba : tibs02ds);
069700100706             IF  T02err = *blanks;
069800100706               dIFS = T02uni;
069900100706             ENDIF;
070000100706             IF  wConc > 1;
070100100708             //?Aggiungo concorrente fino al massimo di 186 byte
070200100708               IF  (%len(%trim(pConc)) + %len(%trim(§IFSdes)) + 2) > 185;
070300100706                 Pconc = %trim(Pconc) + ' Altri...';
070400100706                 leave;
070500100706               ENDIF;
070600100706               pConc = %trim(pConc) + ',';
070700100706             ENDIF;
070800100706             pConc = %trim(pConc) + ' ' + §IFSdes;
070900100706             wConc += 1;
071000100706           ENDIF;
071100100706           reade (ATCcpo:'10') TICPI01L;
071200100706         ENDDO;
071300100706
071400100706         //?Salto pagina
071500100706         IF  CurLine >= 65;
071600100706           exsr sr_StpSalto;
071700100706           *in99 = *off;
071800100706         ENDIF;
071900100706
072000100706         IF  pConc <> *blanks;
072100100706           write MK86CONC;
072200100706         ENDIF;
072300100706
072400100706       ENDSR;
072500100309
072600100309       //--------------------------------------------------------------
072700100309       //?Stampo le attività a dettaglio UFFICIO.
072800100309       //--------------------------------------------------------------
072900100309       BEGSR sr_StpDetUFF;
073000100309
073100100309         clear pAttUff;
073200100309
073300100309         //?decodifico attività
073400100309         clear TIBS02ds;
073500100309         clear dCCO;
073600100309         T02mod = 'C';
073700100309         T02cod = 'CCO';
073800100309         T02ke1 = ATCcad;
073900100309         T02sif = KNSIF;
074000100309         TNTBE_RicercaControllo (kpjba : tibs02ds);
074100100309         IF  T02err = *blanks;
074200100309           dCCO = T02uni;
074300100309         ENDIF;
074400100309
074500100309         //?CHIODO --> non stampo la decodifica della causale se FERIE
074600100309         IF  ATCtat <> 'F';
074700100309           pAttUff = §CCOdes;
074800100309           pAttUff = %trim(pAttUff) + ' Dalle ' +
074900100309                     %subst(%editc(ATCoii:'X'):1:2) +
075000100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
075100100309                     ' Alle ' +
075200100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
075300100309                     %subst(%editc(ATCofi:'X'):3:2);
075400100309         ELSE;
075500100309           pAttUff = 'Dalle ' +
075600100309                     %subst(%editc(ATCoii:'X'):1:2) +
075700100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
075800100309                     ' Alle ' +
075900100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
076000100309                     %subst(%editc(ATCofi:'X'):3:2);
076100100309         ENDIF;
076200100706
076300100706         //?Note attività (1 riga)
076400100706         clear pNota01;
076500100706         clear pNota02;
076600100706
076700100706         //?Leggo le note legate al progressivo attuale
076800100706         wNota = 1;
076900100706         clear CPNcpo;
077000100706         CPNatn  = ATCatn;
077100100706         CPNatnp = ATCatnp;
077200100706         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
077300100706         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
077400100706
077500100706         DOW  not %eof(TICPN05L) and wNota <= 1;
077600100706           IF  wNota = 1;
077700100706             pNota01 = %subst(CPNnot:1:45);
077800100706             pNota02 = %subst(CPNnot:46:45);
077900100706           ENDIF;
078000100706
078100100706           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
078200100706           wNota += 1;
078300100706         ENDDO;
078400100706
078500100706         //?Se non ho trovato note, provo con note legate al progressivo -1
078600100706         IF  pNota01 = *blanks and pNota02 = *blanks;
078700100706           CPNatnp = ATCatnp - 1;
078800100706           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
078900100706           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
079000100706
079100100706           DOW  not %eof(TICPN05L) and wNota <= 1;
079200100706             IF  wNota = 1;
079300100706               pNota01 = %subst(CPNnot:1:45);
079400100706               pNota02 = %subst(CPNnot:46:45);
079500100706             ENDIF;
079600100706
079700100706             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
079800100706             wNota += 1;
079900100706           ENDDO;
080000100706         ENDIF;
080100100309
080200100309         //?Salto pagina
080300100309         IF  *in99;
080400100309           exsr sr_StpSalto;
080500100309           *in99 = *off;
080600100309         ENDIF;
080700100309
080800100309         write MK86RIGAUF;
080900100309
081000100309       ENDSR;
081100100309
081200100309       //--------------------------------------------------------------
081300100309       //?Stampo l'eventuale affiancamento.
081400100309       //--------------------------------------------------------------
081500100309       BEGSR sr_StpAffianca;
081600100309
081700100309         clear pAffianca;
081800100309         wAff = 1;
081900100309
082000100309         SELECT;
082100100309         //?se appuntamento (CHIODO 91 e A)
082200100309           WHEN  ATCtat = 'A' and ATCcad = '91';
082300100309         //?controllo se c'è affiancamento
082400100309             ATLatl  = ATCatn;
082500100309             ATLatlp = ATCatnp;
082600100309             setll (ATLatl:ATLatlp) TIATL02L;
082700100309             reade (ATLatl:ATLatlp) TIATL02L;
082800100309             DOW  not %eof(TIATL02L);
082900100309         //?imposto il campo da stampare
083000100309               IF  pAffianca = *blanks;
083100100616                 pAffianca = '  Affiancato da:';
083200100309               ENDIF;
083300100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
083400100309               w_ATCtat  = ATCtat;
083500100309               w_ATCatn  = ATLatn;
083600100309               w_ATCatnp = ATLatnp;
083700100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
083800100309               IF  %found(TIATC01L);
083900100309                 wCmm = w_ATCcmm;
084000100309                 exsr sr_DecCmm;
084100100309                 IF  wAff > 1;
084200100319             //?Aggiungo nome agente fino al massimo di 196 byte
084300130917                   IF  (%len(%trim(pAffianca)) + %len(%trim(CMMdes)) + 2) > 196;
084400100309                     leave;
084500100309                   ENDIF;
084600100309                   pAffianca = %trim(pAffianca) + ',';
084700100309                 ENDIF;
084800130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
084900100309               ENDIF;
085000100309               wAff += 1;
085100100309               reade (ATLatl:ATLatlp) TIATL02L;
085200100309             ENDDO;
085300100309         //?Stampo l'affiancamento
085400100309             IF  pAffianca <> *blanks;
085500100309             //?Salto pagina
085600100309               IF  *in99;
085700100309                 exsr sr_StpSalto;
085800100309                 *in99 = *off;
085900100309               ENDIF;
086000100309               write MK86AFF;
086100100309             ENDIF;
086200100309
086300100309         //?se affiancamento (CHIODO 92 e A)
086400100309           WHEN  ATCtat = 'A' and ATCcad = '92';
086500100309         //?controllo l'affiancamento
086600100309             ATLatn  = ATCatn;
086700100309             ATLatnp = ATCatnp;
086800100309             chain (ATLatn:ATLatnp) TIATL01L;
086900100309             IF  %found(TIATL01L);
087000100309         //?imposto il campo da stampare
087100100309               IF  pAffianca = *blanks;
087200100616                 pAffianca = '  In affiancamento a:';
087300100309               ENDIF;
087400100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
087500100309               w_ATCtat  = ATCtat;
087600100309               w_ATCatn  = ATLatl;
087700100309               w_ATCatnp = ATLatlp;
087800100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
087900100309               IF  %found(TIATC01L);
088000100309                 wCmm = w_ATCcmm;
088100100309                 exsr sr_DecCmm;
088200130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
088300100309               ENDIF;
088400100309             ENDIF;
088500100309         //?Stampo l'affiancamento
088600100309             IF  pAffianca <> *blanks;
088700100309             //?Salto pagina
088800100309               IF  *in99;
088900100309                 exsr sr_StpSalto;
089000100309                 *in99 = *off;
089100100309               ENDIF;
089200100309               write MK86AFF;
089300100309             ENDIF;
089400100309
089500100309         ENDSL;
089600100309
089700100309       ENDSR;
089800080206
089900080206       //--------------------------------------------------------------
090000080206       //?Reperimento Dati del job (Utente/Operativi).
090100080206       //--------------------------------------------------------------
090200080206       BEGSR DatiJob;
090300080206
090400080206         in(E) §AzUte;
090500080206         if NOT %error;
090600080206           in(E) §DatiUte;
090700080206         endif;
090800080206         if %error or RSut = *blanks;
090900080206           clear TIBS34ds;
091000080206           tibs34r(tibs34ds);
091100080206           in §AzUte;
091200080206           in §DatiUte;
091300080206         endif;
091400100429
091500100429         //?Richiamo pgm di controllo autorizzazioni utente così mi torna
091600100429         //?il codice autorizzazione relativo alle tariffe clienti
091700100429         //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
091800100429         reset TNTAA1DS;
091900100429         ITAA1caut = '§utegtc';
092000100429         kpjbu     = TNTAA1DS;
092100100429         tntaa1c (kpjba);
092200100429         TNTAA1DS = kpjbu;
092300100429
092400150910         //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di distretto
092500100429         IF  OTAA1cabi = 'AZ' and DUTpou <> 046;
092600150910           OTAA1cabi = 'DI';
092700100429         ENDIF;
092800100429
092900100429         //?Carico filiali abilitate all'utente
093000100429         //?Non controllo se caricate o no le filiali
093100100429         //?tanto se non ho caricato nessuna filiale e la richiesta
093200100429         //?è per tutti i commerciali, non stampo niente
093300100429         clear TRUL31DS;
093400100429         I31abi = OTAA1cabi;
093500100429         I31cdi = DUTdis;
093600100429         I31car = DUTare;
093700100429         I31cpo = DUTpou;
093800100429         callp trul31r (kpjba : trul31ds);
093900080206
094000080206       ENDSR;
094100100305
094200100305       //--------------------------------------------------------------
094300100305       //?Carico Sk Agenti.
094400100305       //--------------------------------------------------------------
094500100305       BEGSR sr_CarAGE;
094600100305
094700100305         clear xx;
094800100305         clear $Age;
094900100429         $EndAge = *off;
095000100305
095100100305         exec sql
095200100305          declare AGE cursor for
095300130917          select CMMcod, CMMuni
095400130917            from AZCMM00F
095500130917           order by CMMcod;
095600100305
095700100305         exec sql
095800100305           open AGE;
095900100305           IF sqlcode < 0;
096000100305             $EndAge = *on;
096100100305           ENDIF;
096200100305
096300100305         DOW not $EndAge;
096400100305           exec sql
096500130917             fetch next from AGE into :CMMcod, :CMMuni;
096600100305             IF sqlcod = 100 or sqlcod < 0;
096700100305               $EndAge = *on;
096800100305               leave;
096900100305             ENDIF;
097000130917             IF  CMMuni = $AgeU(zz);
097100100305               xx += 1;
097200130917               $Age(xx) = CMMcod;
097300100305             ENDIF;
097400100305         ENDDO;
097500100305
097600100305         exec sql close AGE;
097700100305
097800100305         //?Se non ho cricato niente memorizzo almeno il codice richiesto
097900100305         IF $Age(01) = 0;
098000100429           $Age(01) = $AgeU(zz);
098100100305         ENDIF;
098200100305
098300100305       ENDSR;
098400100429
098500100429       //--------------------------------------------------------------
098600100429       //?Carico sk agenti unificanti.
098700100429       //--------------------------------------------------------------
098800100429       BEGSR sr_CarAgeUni;
098900100429
099000100429         clear xx;
099100100429         clear $AGEU;
099200100429         $EndAge = *off;
099300100429
099400100429           //?Imposto la stringa per SQL
099500130917           wSQL = 'select CMMcod +
099600130917                     from AZCMM00F +
099700130917                    where substr( digits(CMMuni), 1, 3 ) in (';
099800100429
099900100429           yy = 0;
100000100429           xx = 1;
100100100429           FOR xx by 1 to %elem(pog);
100200100429             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
100300100429               IF yy > 0;
100400100429                 wSQL += ', ';
100500100429               ELSE;
100600100429                 yy = 1;
100700100429               ENDIF;
100800100429               wSQL += '''';
100900100429               wSQL += pog(xx);
101000100429               wSQL += '''';
101100100429             ENDIF;
101200100429           ENDFOR;
101300100505
101400130917         SELECT;
101500130917
101600130917           //?Carico TUTTI gli agenti unificanti
101700130917           WHEN  IMK86ioc = 'O' and imk86iec = ' ' ;
101800130917             wSQL += ') and CMMcod = CMMuni +
101900130917                        and CMMpar = '' ''';
102000100505
102100130917           //?Carico TUTTI gli agenti NON INTERNI unificanti
102200130917           WHEN  (IMK86ioc = 'I' and imk86iec = 'E') or
102300130917                 (IMK86ioc = 'O' and IMK86iec = 'I');
102400130917             wSQL += ') and CMMcod = CMMuni +
102500130917                        and CMMfun <> ''COMIN'' +
102600130917                        and CMMfun <> ''ASC  '' +
102700130917                        and CMMfun <> ''SA   '' +
102800130917                        and CMMpar = '' ''';
102900100514
103000130917           //?Carico TUTTI gli agenti INTERNI unificanti
103100130917           WHEN  (IMK86ioc = 'I' and imk86iec = 'I') or
103200130917                 (IMK86ioc = 'O' and IMK86iec = 'E');
103300130917             wSQL += ') and CMMcod = CMMuni and +
103400130917                           (CMMfun = ''COMIN'' +
103500130917                         or CMMfun = ''ASC  '' +
103600130917                         or CMMfun = ''SA   '') +
103700130917                        and CMMpar = '' ''';
103800130917
103900130917         ENDSL;
104000100429
104100130917         wSQL += ' order by CMMcod +
104200100505                   for fetch only';
104300100429
104400100429         //?Dichiarazione cursore
104500100429         exec sql
104600100429           prepare A1   from :wSQL;
104700100429         exec sql
104800100429           declare AGEU cursor for A1;
104900100429
105000100429         //?Apertura del cursore
105100100429         exec sql
105200100429           open AGEU;
105300100429
105400100429         IF sqlcode < 0;
105500100429           $EndAge = *on;
105600100429         ENDIF;
105700100429
105800100429         yy = 0;
105900100429         DOW not $EndAge;
106000100429           exec sql
106100130917             fetch next from AGEU into :CMMcod;
106200100429           IF sqlcod = 100 or sqlcod < 0;
106300100429             $EndAge = *on;
106400100429             leave;
106500100429           ENDIF;
106600100429
106700130917           yy += 1;
106800130917           $AGEU(yy) = CMMcod;
106900100429
107000100429         ENDDO;
107100100429
107200100429         exec sql
107300100429           close AGEU;
107400100429
107500100429       ENDSR;
107600100305
107700100305       //--------------------------------------------------------------
107800100305       //?Preparazione stringa SQL.
107900100305       //--------------------------------------------------------------
108000100305       BEGSR sr_PrepSQL;
108100100305
108200100308         wSQL = 'select atctat, atccpo, atcksc, +
108300100309                 atcatn, atcatnp, atcdad, atchda, atcnrv, +
108400100628                 atcoii, atcofi, atccad, atcco3, atcdco, +
108500100525                 atccac, atcest +
108600100305                  from TIATC00F +
108700100305                  where atccmm in(';
108800100305
108900100305         yy = 0;
109000100305         xx = 1;
109100100305         FOR xx by 1 to %elem($Age);
109200100305           IF $Age(xx) <> *zeros;
109300100305             IF yy > 0;
109400100305               wSQL += ', ';
109500100305             ELSE;
109600100305               yy = 1;
109700100305             ENDIF;
109800100305             wSQL += %editc($Age(xx):'X');
109900100305           ENDIF;
110000100305         ENDFOR;
110100100305
110200100308         wSQL += ') and atcdad between ' + %editc(IMK86datai:'X') +
110300100308                 ' and ' + %editc(IMK86dataf:'X');
110400100305
110500100305         wSQL += ' order by atcdad, atchda, atctat +
110600100305                   for fetch only';
110700100305
110800100305       ENDSR;
110900100305
111000100305       //--------------------------------------------------------------
111100100305       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
111200100305       //--------------------------------------------------------------
111300100305       BEGSR Sr_CtrRcdOk;
111400100305
111500100305         $RcdOk = *on;
111600100513         // se c'è inclusione o omissione di alcuni tipi attività
111700100513         // controllo
111800100513         SELECT ;
111900100513            When imk86iot = 'I' and %lookup(atctat : $tat) = 0 ;
112000100513                 $RcdOk = *off;
112100100513                 leavesr ;
112200100513            When imk86iot = 'O' and %lookup(atctat : $tat) > 0 ;
112300100513                 $RcdOk = *off;
112400100513                 leavesr ;
112500100513         ENDSL ;
112600100305         //?se attività legata a trattativa
112700100305         //?la trattativa NON deve essere fittizia
112800100305         IF  atcnrv > 0;
112900100305           exec sql
113000100305             select visffz into :visffz from tivis05l
113100100305             where visnrv = : atcnrv;
113200100305         //?non trovo il rcd corrispondente
113300100305           IF  sqlcod <> 0;
113400100305             leavesr;
113500100305           ENDIF;
113600100305         //?se trattativa fittizia non carico a video
113700100305           IF  visffz = 'S';
113800100305             $RcdOk = *off;
113900100305           ENDIF;
114000100305         ENDIF;
114100100525
114200100525         //?CHIODO
114300100706         //?MODIFICA CONTROLLO 06 Luglio 2010 ora controllo solo esito senza causale
114400100706         //?Se appuntamento '91' o affiancamento '92'  e esito 'N'
114500100706         IF  (ATCcad = '91' or ATCcad = '92') and ATCest = 'N';
114600100706           $RcdOk = *off;
114700100706         ENDIF;
114800100525         //?Se appuntamento '91' con causale chiusura attività '24' e esito 'N'
114900100525         //?non devo caricare in agenda
115000100706         // IF  ATCcad = '91' and ATCcac = '24' and ATCest = 'N';
115100100706         //   $RcdOk = *off;
115200100706         // ENDIF;
115300100525         //?Se affiancamento '92' con causale chiusura attività 'NO' e esito 'N'
115400100525         //?non devo caricare in agenda
115500100706         // IF  ATCcad = '92' and ATCcac = 'NO' and ATCest = 'N';
115600100706         //   $RcdOk = *off;
115700100706         // ENDIF;
115800100525         //?Se attvità qualsiasi chiusa con attività 'AA'
115900100525         //?non devo caricare in agenda
116000100525         IF  ATCcac = 'AA';
116100100525           $RcdOk = *off;
116200100525         ENDIF;
116300100305
116400100305       ENDSR;
116500100309
116600100309       //--------------------------------------------------------------
116700100309       //?Decodifico agente.
116800100309       //--------------------------------------------------------------
116900100309       BEGSR sr_DecCmm;
117000100309
117100130917         chain  (wCmm)  AZCMM000;
117200130917         IF  NOT  %found( AZCMM01L );
117300130917           clear  CMMdes;
117400100309         ENDIF;
117500100309
117600100309       ENDSR;
117700090807
117800080206       //--------------------------------------------------------------
117900080206       //?Operazioni finali.
118000080206       //--------------------------------------------------------------
118100080206       BEGSR RoutEnd;
118200090521
118300080206         *inLR = *on;
118400080206         return;
118500080206
118600080206       ENDSR;
118700080206
118800080206      /end-free
