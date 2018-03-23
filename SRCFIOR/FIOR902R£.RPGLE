000100140414       //==============================================================
000200140414       //
000300151016       //?FIOR902R - Reperisce dati per mail accettazione ORM
000400140414       //
000500140414       //==============================================================
000600140414     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000700151020     h dftactgrp(*no) actgrp(*caller)
000800151020     h bnddir('UBRTVNETA')
000900140414
001000140414       //--------------------------------------------------------------
001100140414       //?Dichiarazione file.                                          ?
001200140414       //--------------------------------------------------------------
001300151016       //?File Anagrafica clienti ritiro
001400151016     fFNACR01L  if   e           k disk
001500151016
001600151016       //?File ORM
001700151016     fFNORM01L  if   e           k disk
001800160408
001900160408       //?File estensione ORM
002000160408     fFNORE01L  if   e           k disk
002100151016
002200151016       //?File ORM da clienti
002300151020     fTIORI01L  if   e           k disk    usropn  extfile(wFLIBori)
002400151016
002500151016       //?File estensione ORM
002600151020     fTIORE01L  if   e           k disk    usropn  extfile(wFLIBore)
002700140414
002800140414       //--------------------------------------------------------------
002900140414       //?Definizione costanti.                                        ?
003000140414       //--------------------------------------------------------------
003100140414
003200140414       //---------------------------------------------------------------
003300140414       //?Definizione aree dati.
003400140414       //---------------------------------------------------------------
003500140414
003600140414       //--------------------------------------------------------------
003700140414       //?Definizione strutture dati.                                  ?
003800140414       //--------------------------------------------------------------
003900151016       //?DS FIOR902R
004000151016     d FIOR902DS     e ds                  qualified
004100160408
004200160408       // -?Ritorno dati Servizio clienti
004300160408     d TRULDSCDS     e ds                  qualified INZ
004400160408
004500160408       //?DS rcd GEN
004600160408     d dOREGEN       e ds                  qualified inz
004700140417
004800151016       //?DS rcd OR
004900151016     d dOREOR        e ds                  qualified inz
005000160408
005100160408       //?DS rcd O
005200160408     d dOREORARI     e ds                  qualified inz
005300151016
005400151016       //?DS campo ORMflo
005500151016     d dORM01        e ds                  qualified inz
005600140414
005700140414       //--------------------------------------------------------------
005800140414       //?Definizione schiere.
005900140414       //--------------------------------------------------------------
006000140414
006100140414       //--------------------------------------------------------------
006200140414       //?Definizione campi.
006300140414       //--------------------------------------------------------------
006400140417       //?Flags boolenai
006500140414     d wFine           s               n   inz(*off)
006600151016
006700151016       //?Campi di comodo
006800151019     d cap             s                   like(ORMcar)
006900151019     d indirizzo       s                   like(ORMinr)
007000151019     d localita        s                   like(ORMlor)
007100151016     d mail            s                   like(fior902ds.OOR902mail)
007200151019     d nazione         s                   like(ORMnar)
007300151016     d nome            s                   like(ORMrsr)
007400151105     d nomeordinante   s                   like(ORMrso)
007500151019     d progressivo     s                   like(ORIprg)
007600151016     d provincia       s                   like(ORMprr)
007700160408     d sms             s                   like(fior902ds.OOR902sms)
007800151020
007900151020       // -?Nome esteso Libreria/File
008000151020     d wFLIBore        s             21a   inz
008100151020     d wFLIBori        s             21a   inz
008200151020
008300151020       // -?Nome del sistema?
008400151020     d currSysNeta     s              8a   inz
008500140414
008600140414       //--------------------------------------------------------------
008700140414       //?Definizione procedure.
008800140414       //--------------------------------------------------------------
008900160408       // -?Ritorno dati Servizio Clienti
009000160408     d truldscr        pr                  extpgm('TRULDSCR')
009100160408     d  truldscds                          likeds(TRULDSCDS)
009200140414
009300140414       //--------------------------------------------------------------
009400140414       //?Definizione prototipi.
009500140414       //--------------------------------------------------------------
009600151020       // -?Reperimento NETA sistema AS/400 corrente?
009700151020      /copy gaitrasrc/srcProtoPR,UBRTVNETA
009800140417
009900140417       //---------------------------------------------------------------
010000140417       //?Definizione key-list.
010100140417       //---------------------------------------------------------------
010200151016       //?File FNORM01L
010300151016     d k04fnorm      e ds                  extname(FNORM01L:*key)
010400151019     d                                     prefix(k_)
010500160408       //?File FNORE01L
010600160408     d k05fnore      e ds                  extname(FNORE01L:*key)
010700160408     d                                     prefix(k_)
010800140414
010900140414       //--------------------------------------------------------------
011000140414       //?MAIN.
011100140414       //--------------------------------------------------------------
011200140416
011300140416     c     *entry        plist
011400151019     c                   parm                    FIOR902DS
011500140414
011600140414      /free
011700140414
011800140414       //?Operazioni iniziali
011900140414       exsr RoutInz;
012000140414
012100140416       //?Cerca dati richiesti
012200151016       IF  fior902ds.IOR902tla <> 'C';
012300140416         exsr GetDati;
012400140416       ENDIF;
012500140414
012600140414       //?Operazioni finali
012700140414       exsr RoutEnd;
012800140414
012900140414       //--------------------------------------------------------------
013000140414       //?Operazioni iniziali.                                         ?
013100140414       //--------------------------------------------------------------
013200140414       BEGSR  RoutInz;
013300140414
013400140416       //?Pulisco campi di OUTPUT
013500151016         clear fior902ds.OOR902err;
013600151105         clear fior902ds.OOR902mail;
013700160408         clear fior902ds.OOR902sms;
013800160408
013900151105         clear fior902ds.OOR902rso;
014000160408
014100151016         clear fior902ds.OOR902rsr;
014200151016         clear fior902ds.OOR902inr;
014300151016         clear fior902ds.OOR902lor;
014400151016         clear fior902ds.OOR902car;
014500151016         clear fior902ds.OOR902prr;
014600151016         clear fior902ds.OOR902nar;
014700160408
014800160408         clear fior902ds.OOR902dam;
014900160408         clear fior902ds.OOR902aam;
015000160408         clear fior902ds.OOR902dpm;
015100160408         clear fior902ds.OOR902apm;
015200160408
015300160408         clear fior902ds.OOR902rfa;
015400160408
015500160408         clear fior902ds.OOR902ncl;
015600160408         clear fior902ds.OOR902pkg;
015700160408         clear fior902ds.OOR902vlm;
015800160408         clear fior902ds.OOR902bnc;
015900160408         clear fior902ds.OOR902nam;
016000160408
016100160408         clear fior902ds.OOR902ter;
016200160408         clear fior902ds.OOR902ref;
016300160408
016400151105         clear fior902ds.OOR902dpag;
016500160408
016600160408         clear fior902ds.OOR902no1;
016700160408         clear fior902ds.OOR902no2;
016800160408
016900151016         clear fior902ds.OOR902dar;
017000151016         clear fior902ds.OOR902ori;
017100151016         clear fior902ds.OOR902orf;
017200160408
017300160408         clear fior902ds.OOR902npr;
017400160408
017500160408         clear fior902ds.OOR902dpor;
017600160408         clear fior902ds.OOR902mpor;
017700160408         clear fior902ds.OOR902tpor;
017800160408         clear fior902ds.OOR902por1;
017900160408         clear fior902ds.OOR902por2;
018000160408         clear fior902ds.OOR902por3;
018100160408         clear fior902ds.OOR902por4;
018200160502
018300160502         clear mail;
018400160502         clear sms;
018500160502         clear nomeordinante;
018600160502         clear nome;
018700160502         clear indirizzo;
018800160502         clear localita;
018900160502         clear cap;
019000160502         clear provincia;
019100160502         clear nazione;
019200160502         clear progressivo;
019300140417
019400140417       //?Chiusura
019500151016         IF  fior902ds.IOR902tla = *blanks;
019600140417           *inRT = *on;
019700140417         ELSE;
019800140417           *inLR = *on;
019900140417         ENDIF;
020000140417
020100160412       //?Imposto già i dati della chiave x FNORM
020200151019         k_ORMpoe = fior902ds.IOR902poe;
020300151019         k_ORMnsr = fior902ds.IOR902nsr;
020400151019         k_ORMnor = fior902ds.IOR902nor;
020500151019         k_ORMnrv = fior902ds.IOR902nrv;
020600160412
020700160412       //?Imposto già i dati della chiave x FNORE
020800160412         k_OREpoe = fior902ds.IOR902poe;
020900160412         k_OREnsr = fior902ds.IOR902nsr;
021000160412         k_OREnor = fior902ds.IOR902nor;
021100160412         k_OREnrv = fior902ds.IOR902nrv;
021200151020
021300151020       //?Apro file TIORI/TIORE
021400151020         IF  %open(TIORI01L);
021500151020           leavesr;
021600151020         ENDIF;
021700151020
021800151020         UBRTVNETA_Rtv(currSysNeta);
021900151020         IF  %subst(currSysNetA:1:5 ) = 'AS888';
022000151020           wFLIBore = 'GAITRAGRPS/TIORE01L';
022100151020           wFLIBori = 'GAITRAGRPS/TIORI01L';
022200151020         ELSE;
022300151020           wFLIBore = 'GAITRAGRU/TIORE01L';
022400151020           wFLIBori = 'GAITRAGRU/TIORI01L';
022500151020         ENDIF;
022600151020         open TIORI01L;
022700151020         open TIORE01L;
022800140414
022900140414       ENDSR;
023000140414
023100140414       //--------------------------------------------------------------
023200140416       //?Recupero i dati richiesti.
023300140414       //--------------------------------------------------------------
023400140416       BEGSR  GetDati;
023500160408
023600160408         clear nome;
023700160408         clear indirizzo;
023800160408         clear localita;
023900160408         clear cap;
024000160408         clear provincia;
024100160408         clear nazione;
024200151016
024300151016       //?Come prima cosa aggancio l'ORM
024400151016         clear dORM01;
024500151016         chain %kds(K04fnorm) FNORM01L;
024600151016         IF  not %found(FNORM01L);
024700160408           fior902ds.OOR902err = 'N';
024800151016           leavesr;
024900151016         ENDIF;
025000151016
025100151016         dORM01 = ORMflo;
025200160408
025300160408       //?Se orm da VAS recupero i dati da VAS
025400160408         IF  ORMtco = 'I' or ORMtco = 'F';
025500160408           exsr CtrDatiVAS;
025600160408           IF  fior902ds.OOR902err <> *blanks;
025700160408             leavesr;
025800160408           ENDIF;
025900160408           exsr GetDatiVAS;
026000160408         ENDIF;
026100160408
026200160408       //?Se orm NON VAS recupero i dati da ORM
026300160408         IF  ORMtco <> 'I' and ORMtco <> 'F';
026400160408           exsr CtrDatiORM;
026500160408           IF  fior902ds.OOR902err <> *blanks;
026600160408             leavesr;
026700160408           ENDIF;
026800160408           exsr GetDatiORM;
026900160408         ENDIF;
027000160502
027100160502         fior902ds.OOR902mail = mail;
027200160502         fior902ds.OOR902sms  = sms;
027300160408
027400160408       //?Recupero di dati Servizio Clienti della filiale ritiro
027500160408         clear TRULDSCDS;
027600160408         truldscds.IULDSCfil = ORMpor;
027700160408         truldscr (truldscds);
027800160408         IF  truldscds.OULDSCerr <> *blanks;
027900160408           leavesr;
028000160408         ENDIF;
028100160408         fior902ds.OOR902dpor = truldscds.OULDSCdesc;
028200160408         fior902ds.OOR902tpor = truldscds.OULDSCtel;
028300160408         fior902ds.OOR902mpor = truldscds.OULDSCmail;
028400160408         fior902ds.OOR902por1 = truldscds.OULDSCamda;
028500160408         fior902ds.OOR902por2 = truldscds.OULDSCama;
028600160408         fior902ds.OOR902por3 = truldscds.OULDSCpmda;
028700160408         fior902ds.OOR902por4 = truldscds.OULDSCpma;
028800160408
028900160408       ENDSR;
029000160408
029100160408       //--------------------------------------------------------------
029200160408       //?Controllo se ci sono i dati da VAS.
029300160408       //--------------------------------------------------------------
029400160408       BEGSR  CtrDatiVAS;
029500160408
029600160408       //?Se ORM da VAS deve avere il n. progressivo
029700151019         IF  dORM01.§ORMpg = *blanks;
029800160408           fior902ds.OOR902err = 'N';
029900151019           leavesr;
030000151019         ENDIF;
030100151019
030200151019         IF  dORM01.§ORMpg < *zeros;
030300160408           fior902ds.OOR902err = 'N';
030400151019           leavesr;
030500151019         ENDIF;
030600151019
030700151019         progressivo = %dec(dORM01.§ORMpg:7:0);
030800140416
030900160408       //?Deve esistere il TIORI e non essere annullato
031000151016       //?Con il progressivo aggancio il TIORI
031100151019         chain (progressivo) TIORI01L;
031200151016         IF  not %found(TIORI01L);
031300160408           fior902ds.OOR902err = 'N';
031400140417           leavesr;
031500140417         ENDIF;
031600151016
031700151016       //?Non deve essere annullato
031800151016         IF  VAOatb <> *blanks;
031900160408           fior902ds.OOR902err = 'N';
032000151016           leavesr;
032100151016         ENDIF;
032200160408
032300160408       //?Deve avere almeno 1 dei due flag Alert impostato
032400160408         clear dOREGEN;
032500160408         chain (ORIprg:'G ') TIORE01L;
032600160408         IF  not %found(TIORE01L);
032700160408           fior902ds.OOR902err = '1';
032800160408           leavesr;
032900160408         ENDIF;
033000160408         dOREGEN = VAOEdati;
033100160408         IF  dOREGEN.§OREfmco <> 'S' and dOREGEN.§OREfsco <> 'S';
033200160408           fior902ds.OOR902err = '1';
033300160408           leavesr;
033400160408         ENDIF;
033500160408
033600160408       //?Recupero mail
033700161103       //?il dato è meglio prenderlo dall'ORM
033800160408         IF  dOREGEN.§OREfmco = 'S';
033900160408           clear mail;
034000161103           //chain (ORIprg:'MC') TIORE01L;
034100161103           //IF  not %found(TIORE01L);
034200161103           k_OREtrc = 'MC';
034300161103           chain %kds(K05fnore) FNORE01L;
034400161103           IF  not %found(FNORE01L);
034500160408             fior902ds.OOR902err = '1';
034600160408             leavesr;
034700160408           ENDIF;
034800161103           //mail = VAOEdati;
034900161103           mail = OREdati;
035000160408         ENDIF;
035100160408
035200160408       //?Recupero sms
035300160408         IF  dOREGEN.§OREfsco = 'S';
035400160408           clear sms;
035500161103           //chain (ORIprg:'SC') TIORE01L;
035600161103           //IF  not %found(TIORE01L);
035700161103           k_OREtrc = 'SC';
035800161103           chain %kds(K05fnore) FNORE01L;
035900161103           IF  not %found(FNORE01L);
036000160408             fior902ds.OOR902err = '1';
036100160408             leavesr;
036200160408           ENDIF;
036300161103           //sms = VAOEdati;
036400161103           sms = OREdati;
036500160408         ENDIF;
036600160408
036700160408       //?Recupero orari ritiro indicativo
036800160408         clear dOREor;
036900160408         chain (ORIprg:'OR') TIORE01L;
037000160408         IF  not %found(TIORE01L);
037100160408           fior902ds.OOR902err = '2';
037200160408           leavesr;
037300160408         ENDIF;
037400160408         dOREor = VAOEdati;
037500160408
037600160408       //?Cerco il Numero Prenotazione Ritiro
037700160408         chain (ORIprg:'NP') TIORE01L;
037800160408         IF  %found(TIORE01L);
037900160408           fior902ds.OOR902npr = VAOEdati;
038000160408         ENDIF;
038100160408
038200160408       //?Cerco gli orari apertura
038300160408         clear dOREorari;
038400160408         chain (ORIprg:'O ') TIORE01L;
038500160408         IF  %found(TIORE01L);
038600160408           dOREorari = VAOEdati;
038700160408         ENDIF;
038800160408
038900160408       ENDSR;
039000160408
039100160408       //--------------------------------------------------------------
039200160408       //?Recupero i dati da VAS.
039300160408       //--------------------------------------------------------------
039400160408       BEGSR  GetDatiVAS;
039500151019
039600151016       //?Se mittente codificato su TIORI non ho i dati, ho solo il codice
039700151016         IF  VAOcra > 0;
039800151016           chain (VAOcra) FNACR01L;
039900151016           IF  %found(FNACR01L);
040000151016             nome = ACRrsr;
040100151016             indirizzo = ACRinr;
040200151016             localita = ACRlor;
040300151016             cap = ACRcar;
040400151016             provincia = ACRprr;
040500151016           ENDIF;
040600151019         ELSE;
040700151019           nome = VAOrsr;
040800151019           indirizzo = VAOinr;
040900151019           localita = VAOlor;
041000151019           cap = VAOcar;
041100151019           provincia = VAOprr;
041200151016         ENDIF;
041300151105
041400160408       //?Nome di chi ci ha comunicato il ritiro
041500151105         clear nomeordinante;
041600151105         IF  VAOcor > 0;
041700151105           chain (VAOcor) FNACR01L;
041800151105           IF  %found(FNACR01L);
041900151105             nomeordinante = ACRrsr;
042000151105           ENDIF;
042100151105         ELSE;
042200151105       //?Se ordinante non codificato imposto il mittente
042300151109           nomeordinante = nome;
042400151105         ENDIF;
042500140417
042600151105         fior902ds.OOR902rso  = nomeordinante;
042700151019         fior902ds.OOR902rsr  = nome;
042800151019         fior902ds.OOR902inr  = indirizzo;
042900151019         fior902ds.OOR902lor  = localita;
043000151019         fior902ds.OOR902car  = cap;
043100151019         fior902ds.OOR902prr  = provincia;
043200151019         fior902ds.OOR902nar  = nazione;
043300151105         fior902ds.OOR902rfa  = VAOrfa;
043400151105         fior902ds.OOR902ter  = VAOter;
043500151109         fior902ds.OOR902ref  = VAOrer;
043600151105         SELECT;
043700151105         WHEN  VAOpag = 'M';
043800151105           fior902ds.OOR902dpag = 'MITTENTE';
043900151105         WHEN  VAOpag = 'O';
044000151105           fior902ds.OOR902dpag = 'ORDINANTE';
044100151105         WHEN  VAOpag = 'D';
044200151105           fior902ds.OOR902dpag = 'DESTINATARIO';
044300151105         ENDSL;
044400151016         fior902ds.OOR902dar  = VAOdar;
044500151019         fior902ds.OOR902ori  = dOREOR.§OREori;
044600151019         fior902ds.OOR902orf  = dOREOR.§OREorf;
044700151016         fior902ds.OOR902ncl  = VAOncl;
044800151019         fior902ds.OOR902pkg  = VAOpkg;
044900151104         fior902ds.OOR902vlm  = VAOvlm;
045000151104         fior902ds.OOR902bnc  = VAObnc;
045100151016         fior902ds.OOR902nam  = VAOnam;
045200151016         fior902ds.OOR902no1  = VAOno1;
045300151016         fior902ds.OOR902no2  = VAOno2;
045400160408         fior902ds.OOR902dam = dOREorari.§OREoramda;
045500160408         fior902ds.OOR902aam = dOREorari.§OREorama;
045600160408         fior902ds.OOR902dpm = dOREorari.§OREorapda;
045700160408         fior902ds.OOR902apm = dOREorari.§OREorapa;
045800140414
045900140414       ENDSR;
046000160408
046100160408       //--------------------------------------------------------------
046200160408       //?Controllo se ci sono i dati da ORM.
046300160408       //--------------------------------------------------------------
046400160408       BEGSR  CtrDatiORM;
046500160408
046600160408       //?Deve avere almeno 1 dei due flag Alert impostato
046700160408         clear dOREGEN;
046800160408         k_OREtrc = 'G ';
046900160408         chain %kds(K05fnore) FNORE01L;
047000160408         IF  not %found(FNORE01L);
047100160408           fior902ds.OOR902err = '1';
047200160408           leavesr;
047300160408         ENDIF;
047400160408         dOREGEN = OREdati;
047500160408         IF  dOREGEN.§OREfmco <> 'S' and dOREGEN.§OREfsco <> 'S';
047600160408           fior902ds.OOR902err = '1';
047700160408           leavesr;
047800160408         ENDIF;
047900160408
048000160408       //?Recupero mail
048100160408         IF  dOREGEN.§OREfmco = 'S';
048200160408           clear mail;
048300160408           k_OREtrc = 'MC';
048400160408           chain %kds(K05fnore) FNORE01L;
048500160408           IF  not %found(FNORE01L);
048600160408             fior902ds.OOR902err = '1';
048700160408             leavesr;
048800160408           ENDIF;
048900160408           mail = OREdati;
049000160408         ENDIF;
049100160408
049200160408       //?Recupero sms
049300160408         IF  dOREGEN.§OREfsco = 'S';
049400160408           clear sms;
049500160408           k_OREtrc = 'SC';
049600160408           chain %kds(K05fnore) FNORE01L;
049700160408           IF  not %found(FNORE01L);
049800160408             fior902ds.OOR902err = '1';
049900160408             leavesr;
050000160408           ENDIF;
050100160408           sms = OREdati;
050200160408         ENDIF;
050300160408
050400160408       //?Recupero orari ritiro indicativo
050500160408         clear dOREor;
050600160408         k_OREtrc = 'OR';
050700160408         chain %kds(K05fnore) FNORE01L;
050800160408         IF  not %found(FNORE01L);
050900160408           fior902ds.OOR902err = '2';
051000160408           leavesr;
051100160408         ENDIF;
051200160408         dOREor = OREdati;
051300160408
051400160408       //?Cerco il Numero Prenotazione Ritiro
051500160408         k_OREtrc = 'NP';
051600160408         chain %kds(K05fnore) FNORE01L;
051700160408         IF  not %found(FNORE01L);
051800160408           fior902ds.OOR902err = '2';
051900160408           leavesr;
052000160408         ENDIF;
052100160408         fior902ds.OOR902npr = OREdati;
052200160408
052300160408       //?Cerco gli orari apertura
052400160408         clear dOREorari;
052500160408         k_OREtrc = 'O ';
052600160408         chain %kds(K05fnore) FNORE01L;
052700160412         IF  %found(FNORE01L);
052800160412           dOREorari = OREdati;
052900160408         ENDIF;
053000160408
053100160408       ENDSR;
053200160408
053300160408       //--------------------------------------------------------------
053400160408       //?Recupero i dati da ORM.
053500160408       //--------------------------------------------------------------
053600160408       BEGSR  GetDatiORM;
053700160408
053800160408       //?Se ordinante presente lo imposto se no prendo il mittente
053900160408         IF  ORMrso <> *blanks;
054000160408           fior902ds.OOR902rso  = ORMrso;
054100160408         ELSE;
054200160408           fior902ds.OOR902rso  = ORMrsr;
054300160408         ENDIF;
054400160408         fior902ds.OOR902rsr  = ORMrsr;
054500160408         fior902ds.OOR902inr  = ORMinr;
054600160408         fior902ds.OOR902lor  = ORMlor;
054700160408         fior902ds.OOR902car  = ORMcar;
054800160408         fior902ds.OOR902prr  = ORMprr;
054900160408         fior902ds.OOR902nar  = ORMnar;
055000160408         fior902ds.OOR902rfa  = ORMrfa;
055100160408         fior902ds.OOR902ter  = ORMter;
055200160408         fior902ds.OOR902ref  = ORMrer;
055300160408         SELECT;
055400160408         WHEN  ORMpag = 'M';
055500160408           fior902ds.OOR902dpag = 'MITTENTE';
055600160408         WHEN  ORMpag = 'O';
055700160408           fior902ds.OOR902dpag = 'ORDINANTE';
055800160408         WHEN  ORMpag = 'D';
055900160408           fior902ds.OOR902dpag = 'DESTINATARIO';
056000160408         ENDSL;
056100160408         fior902ds.OOR902dar = ORMdar;
056200160408         fior902ds.OOR902ori = dOREOR.§OREori;
056300160408         fior902ds.OOR902orf = dOREOR.§OREorf;
056400160408         fior902ds.OOR902ncl = ORMncl;
056500160408         fior902ds.OOR902pkg = ORMpkg;
056600160408         fior902ds.OOR902vlm = ORMvlm;
056700160408         fior902ds.OOR902bnc = ORMbnc;
056800160408         fior902ds.OOR902nam = ORMnam;
056900160408         fior902ds.OOR902no1 = ORMno1;
057000160408         fior902ds.OOR902no2 = ORMno2;
057100160408         fior902ds.OOR902dam = dOREorari.§OREoramda;
057200160408         fior902ds.OOR902aam = dOREorari.§OREorama;
057300160408         fior902ds.OOR902dpm = dOREorari.§OREorapda;
057400160408         fior902ds.OOR902apm = dOREorari.§OREorapa;
057500160408
057600160408       ENDSR;
057700140414
057800140414       //--------------------------------------------------------------
057900140414       //?Operazioni finali.
058000140414       //--------------------------------------------------------------
058100140414       BEGSR  RoutEnd;
058200140414
058300140414         return;
058400140414
058500140414       ENDSR;
058600140414
058700140414       //--------------------------------------------------------------
058800140415      /end-free
