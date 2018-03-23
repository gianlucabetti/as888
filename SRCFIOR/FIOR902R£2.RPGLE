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
033700160408         IF  dOREGEN.§OREfmco = 'S';
033800160408           clear mail;
033900160408           chain (ORIprg:'MC') TIORE01L;
034000160408           IF  not %found(TIORE01L);
034100160408             fior902ds.OOR902err = '1';
034200160408             leavesr;
034300160408           ENDIF;
034400160408           mail = VAOEdati;
034500160408         ENDIF;
034600160408
034700160408       //?Recupero sms
034800160408         IF  dOREGEN.§OREfsco = 'S';
034900160408           clear sms;
035000160408           chain (ORIprg:'SC') TIORE01L;
035100160408           IF  not %found(TIORE01L);
035200160408             fior902ds.OOR902err = '1';
035300160408             leavesr;
035400160408           ENDIF;
035500160408           sms = VAOEdati;
035600160408         ENDIF;
035700160408
035800160408       //?Recupero orari ritiro indicativo
035900160408         clear dOREor;
036000160408         chain (ORIprg:'OR') TIORE01L;
036100160408         IF  not %found(TIORE01L);
036200160408           fior902ds.OOR902err = '2';
036300160408           leavesr;
036400160408         ENDIF;
036500160408         dOREor = VAOEdati;
036600160408
036700160408       //?Cerco il Numero Prenotazione Ritiro
036800160408         chain (ORIprg:'NP') TIORE01L;
036900160408         IF  %found(TIORE01L);
037000160408           fior902ds.OOR902npr = VAOEdati;
037100160408         ENDIF;
037200160408
037300160408       //?Cerco gli orari apertura
037400160408         clear dOREorari;
037500160408         chain (ORIprg:'O ') TIORE01L;
037600160408         IF  %found(TIORE01L);
037700160408           dOREorari = VAOEdati;
037800160408         ENDIF;
037900160408
038000160408       ENDSR;
038100160408
038200160408       //--------------------------------------------------------------
038300160408       //?Recupero i dati da VAS.
038400160408       //--------------------------------------------------------------
038500160408       BEGSR  GetDatiVAS;
038600151019
038700151016       //?Se mittente codificato su TIORI non ho i dati, ho solo il codice
038800151016         IF  VAOcra > 0;
038900151016           chain (VAOcra) FNACR01L;
039000151016           IF  %found(FNACR01L);
039100151016             nome = ACRrsr;
039200151016             indirizzo = ACRinr;
039300151016             localita = ACRlor;
039400151016             cap = ACRcar;
039500151016             provincia = ACRprr;
039600151016           ENDIF;
039700151019         ELSE;
039800151019           nome = VAOrsr;
039900151019           indirizzo = VAOinr;
040000151019           localita = VAOlor;
040100151019           cap = VAOcar;
040200151019           provincia = VAOprr;
040300151016         ENDIF;
040400151105
040500160408       //?Nome di chi ci ha comunicato il ritiro
040600151105         clear nomeordinante;
040700151105         IF  VAOcor > 0;
040800151105           chain (VAOcor) FNACR01L;
040900151105           IF  %found(FNACR01L);
041000151105             nomeordinante = ACRrsr;
041100151105           ENDIF;
041200151105         ELSE;
041300151105       //?Se ordinante non codificato imposto il mittente
041400151109           nomeordinante = nome;
041500151105         ENDIF;
041600140417
041700151105         fior902ds.OOR902rso  = nomeordinante;
041800151019         fior902ds.OOR902rsr  = nome;
041900151019         fior902ds.OOR902inr  = indirizzo;
042000151019         fior902ds.OOR902lor  = localita;
042100151019         fior902ds.OOR902car  = cap;
042200151019         fior902ds.OOR902prr  = provincia;
042300151019         fior902ds.OOR902nar  = nazione;
042400151105         fior902ds.OOR902rfa  = VAOrfa;
042500151105         fior902ds.OOR902ter  = VAOter;
042600151109         fior902ds.OOR902ref  = VAOrer;
042700151105         SELECT;
042800151105         WHEN  VAOpag = 'M';
042900151105           fior902ds.OOR902dpag = 'MITTENTE';
043000151105         WHEN  VAOpag = 'O';
043100151105           fior902ds.OOR902dpag = 'ORDINANTE';
043200151105         WHEN  VAOpag = 'D';
043300151105           fior902ds.OOR902dpag = 'DESTINATARIO';
043400151105         ENDSL;
043500151016         fior902ds.OOR902dar  = VAOdar;
043600151019         fior902ds.OOR902ori  = dOREOR.§OREori;
043700151019         fior902ds.OOR902orf  = dOREOR.§OREorf;
043800151016         fior902ds.OOR902ncl  = VAOncl;
043900151019         fior902ds.OOR902pkg  = VAOpkg;
044000151104         fior902ds.OOR902vlm  = VAOvlm;
044100151104         fior902ds.OOR902bnc  = VAObnc;
044200151016         fior902ds.OOR902nam  = VAOnam;
044300151016         fior902ds.OOR902no1  = VAOno1;
044400151016         fior902ds.OOR902no2  = VAOno2;
044500160408         fior902ds.OOR902dam = dOREorari.§OREoramda;
044600160408         fior902ds.OOR902aam = dOREorari.§OREorama;
044700160408         fior902ds.OOR902dpm = dOREorari.§OREorapda;
044800160408         fior902ds.OOR902apm = dOREorari.§OREorapa;
044900140414
045000140414       ENDSR;
045100160408
045200160408       //--------------------------------------------------------------
045300160408       //?Controllo se ci sono i dati da ORM.
045400160408       //--------------------------------------------------------------
045500160408       BEGSR  CtrDatiORM;
045600160408
045700160408       //?Deve avere almeno 1 dei due flag Alert impostato
045800160408         clear dOREGEN;
045900160408         k_OREtrc = 'G ';
046000160408         chain %kds(K05fnore) FNORE01L;
046100160408         IF  not %found(FNORE01L);
046200160408           fior902ds.OOR902err = '1';
046300160408           leavesr;
046400160408         ENDIF;
046500160408         dOREGEN = OREdati;
046600160408         IF  dOREGEN.§OREfmco <> 'S' and dOREGEN.§OREfsco <> 'S';
046700160408           fior902ds.OOR902err = '1';
046800160408           leavesr;
046900160408         ENDIF;
047000160408
047100160408       //?Recupero mail
047200160408         IF  dOREGEN.§OREfmco = 'S';
047300160408           clear mail;
047400160408           k_OREtrc = 'MC';
047500160408           chain %kds(K05fnore) FNORE01L;
047600160408           IF  not %found(FNORE01L);
047700160408             fior902ds.OOR902err = '1';
047800160408             leavesr;
047900160408           ENDIF;
048000160408           mail = OREdati;
048100160408         ENDIF;
048200160408
048300160408       //?Recupero sms
048400160408         IF  dOREGEN.§OREfsco = 'S';
048500160408           clear sms;
048600160408           k_OREtrc = 'SC';
048700160408           chain %kds(K05fnore) FNORE01L;
048800160408           IF  not %found(FNORE01L);
048900160408             fior902ds.OOR902err = '1';
049000160408             leavesr;
049100160408           ENDIF;
049200160408           sms = OREdati;
049300160408         ENDIF;
049400160408
049500160408       //?Recupero orari ritiro indicativo
049600160408         clear dOREor;
049700160408         k_OREtrc = 'OR';
049800160408         chain %kds(K05fnore) FNORE01L;
049900160408         IF  not %found(FNORE01L);
050000160408           fior902ds.OOR902err = '2';
050100160408           leavesr;
050200160408         ENDIF;
050300160408         dOREor = OREdati;
050400160408
050500160408       //?Cerco il Numero Prenotazione Ritiro
050600160408         k_OREtrc = 'NP';
050700160408         chain %kds(K05fnore) FNORE01L;
050800160408         IF  not %found(FNORE01L);
050900160408           fior902ds.OOR902err = '2';
051000160408           leavesr;
051100160408         ENDIF;
051200160408         fior902ds.OOR902npr = OREdati;
051300160408
051400160408       //?Cerco gli orari apertura
051500160408         clear dOREorari;
051600160408         k_OREtrc = 'O ';
051700160408         chain %kds(K05fnore) FNORE01L;
051800160412         IF  %found(FNORE01L);
051900160412           dOREorari = OREdati;
052000160408         ENDIF;
052100160408
052200160408       ENDSR;
052300160408
052400160408       //--------------------------------------------------------------
052500160408       //?Recupero i dati da ORM.
052600160408       //--------------------------------------------------------------
052700160408       BEGSR  GetDatiORM;
052800160408
052900160408       //?Se ordinante presente lo imposto se no prendo il mittente
053000160408         IF  ORMrso <> *blanks;
053100160408           fior902ds.OOR902rso  = ORMrso;
053200160408         ELSE;
053300160408           fior902ds.OOR902rso  = ORMrsr;
053400160408         ENDIF;
053500160408         fior902ds.OOR902rsr  = ORMrsr;
053600160408         fior902ds.OOR902inr  = ORMinr;
053700160408         fior902ds.OOR902lor  = ORMlor;
053800160408         fior902ds.OOR902car  = ORMcar;
053900160408         fior902ds.OOR902prr  = ORMprr;
054000160408         fior902ds.OOR902nar  = ORMnar;
054100160408         fior902ds.OOR902rfa  = ORMrfa;
054200160408         fior902ds.OOR902ter  = ORMter;
054300160408         fior902ds.OOR902ref  = ORMrer;
054400160408         SELECT;
054500160408         WHEN  ORMpag = 'M';
054600160408           fior902ds.OOR902dpag = 'MITTENTE';
054700160408         WHEN  ORMpag = 'O';
054800160408           fior902ds.OOR902dpag = 'ORDINANTE';
054900160408         WHEN  ORMpag = 'D';
055000160408           fior902ds.OOR902dpag = 'DESTINATARIO';
055100160408         ENDSL;
055200160408         fior902ds.OOR902dar = ORMdar;
055300160408         fior902ds.OOR902ori = dOREOR.§OREori;
055400160408         fior902ds.OOR902orf = dOREOR.§OREorf;
055500160408         fior902ds.OOR902ncl = ORMncl;
055600160408         fior902ds.OOR902pkg = ORMpkg;
055700160408         fior902ds.OOR902vlm = ORMvlm;
055800160408         fior902ds.OOR902bnc = ORMbnc;
055900160408         fior902ds.OOR902nam = ORMnam;
056000160408         fior902ds.OOR902no1 = ORMno1;
056100160408         fior902ds.OOR902no2 = ORMno2;
056200160408         fior902ds.OOR902dam = dOREorari.§OREoramda;
056300160408         fior902ds.OOR902aam = dOREorari.§OREorama;
056400160408         fior902ds.OOR902dpm = dOREorari.§OREorapda;
056500160408         fior902ds.OOR902apm = dOREorari.§OREorapa;
056600160408
056700160408       ENDSR;
056800140414
056900140414       //--------------------------------------------------------------
057000140414       //?Operazioni finali.
057100140414       //--------------------------------------------------------------
057200140414       BEGSR  RoutEnd;
057300140414
057400140414         return;
057500140414
057600140414       ENDSR;
057700140414
057800140414       //--------------------------------------------------------------
057900140415      /end-free
