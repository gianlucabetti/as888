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
001300170809       //?File Anagrafica clienti
001400170809     fCNACO00F  if   e           k disk
001500170809
001600151016       //?File Anagrafica clienti ritiro
001700151016     fFNACR01L  if   e           k disk
001800151016
001900151016       //?File ORM
002000151016     fFNORM01L  if   e           k disk
002100160408
002200160408       //?File estensione ORM
002300160408     fFNORE01L  if   e           k disk
002400151016
002500151016       //?File ORM da clienti
002600151020     fTIORI01L  if   e           k disk    usropn  extfile(wFLIBori)
002700151016
002800151016       //?File estensione ORM
002900151020     fTIORE01L  if   e           k disk    usropn  extfile(wFLIBore)
003000140414
003100140414       //--------------------------------------------------------------
003200140414       //?Definizione costanti.                                        ?
003300140414       //--------------------------------------------------------------
003400140414
003500140414       //---------------------------------------------------------------
003600140414       //?Definizione aree dati.
003700140414       //---------------------------------------------------------------
003800140414
003900140414       //--------------------------------------------------------------
004000140414       //?Definizione strutture dati.                                  ?
004100140414       //--------------------------------------------------------------
004200151016       //?DS FIOR902R
004300151016     d FIOR902DS     e ds                  qualified
004400160408
004500160408       // -?Ritorno dati Servizio clienti
004600160408     d TRULDSCDS     e ds                  qualified INZ
004700160408
004800160408       //?DS rcd GEN
004900160408     d dOREGEN       e ds                  qualified inz
005000140417
005100151016       //?DS rcd OR
005200151016     d dOREOR        e ds                  qualified inz
005300160408
005400160408       //?DS rcd O
005500160408     d dOREORARI     e ds                  qualified inz
005600151016
005700151016       //?DS campo ORMflo
005800151016     d dORM01        e ds                  qualified inz
005900140414
006000140414       //--------------------------------------------------------------
006100140414       //?Definizione schiere.
006200140414       //--------------------------------------------------------------
006300140414
006400140414       //--------------------------------------------------------------
006500140414       //?Definizione campi.
006600140414       //--------------------------------------------------------------
006700140417       //?Flags boolenai
006800140414     d wFine           s               n   inz(*off)
006900151016
007000151016       //?Campi di comodo
007100151019     d cap             s                   like(ORMcar)
007200170809     d codacr          s                   like(ORMcra)
007300170809     d ksc             s                   like(ORMksc)
007400151019     d indirizzo       s                   like(ORMinr)
007500151019     d localita        s                   like(ORMlor)
007600151016     d mail            s                   like(fior902ds.OOR902mail)
007700151019     d nazione         s                   like(ORMnar)
007800151016     d nome            s                   like(ORMrsr)
007900170809     d nomechipaga     s                   like(fior902ds.OOR902dpag)
008000151105     d nomeordinante   s                   like(ORMrso)
008100151019     d progressivo     s                   like(ORIprg)
008200151016     d provincia       s                   like(ORMprr)
008300160408     d sms             s                   like(fior902ds.OOR902sms)
008400151020
008500151020       // -?Nome esteso Libreria/File
008600151020     d wFLIBore        s             21a   inz
008700151020     d wFLIBori        s             21a   inz
008800151020
008900151020       // -?Nome del sistema?
009000151020     d currSysNeta     s              8a   inz
009100140414
009200140414       //--------------------------------------------------------------
009300140414       //?Definizione procedure.
009400140414       //--------------------------------------------------------------
009500160408       // -?Ritorno dati Servizio Clienti
009600160408     d truldscr        pr                  extpgm('TRULDSCR')
009700160408     d  truldscds                          likeds(TRULDSCDS)
009800140414
009900140414       //--------------------------------------------------------------
010000140414       //?Definizione prototipi.
010100140414       //--------------------------------------------------------------
010200151020       // -?Reperimento NETA sistema AS/400 corrente?
010300151020      /copy gaitrasrc/srcProtoPR,UBRTVNETA
010400140417
010500140417       //---------------------------------------------------------------
010600140417       //?Definizione key-list.
010700140417       //---------------------------------------------------------------
010800151016       //?File FNORM01L
010900151016     d k04fnorm      e ds                  extname(FNORM01L:*key)
011000151019     d                                     prefix(k_)
011100160408       //?File FNORE01L
011200160408     d k05fnore      e ds                  extname(FNORE01L:*key)
011300160408     d                                     prefix(k_)
011400140414
011500140414       //--------------------------------------------------------------
011600140414       //?MAIN.
011700140414       //--------------------------------------------------------------
011800140416
011900140416     c     *entry        plist
012000151019     c                   parm                    FIOR902DS
012100140414
012200140414      /free
012300140414
012400140414       //?Operazioni iniziali
012500140414       exsr RoutInz;
012600140414
012700140416       //?Cerca dati richiesti
012800151016       IF  fior902ds.IOR902tla <> 'C';
012900140416         exsr GetDati;
013000140416       ENDIF;
013100140414
013200140414       //?Operazioni finali
013300140414       exsr RoutEnd;
013400140414
013500140414       //--------------------------------------------------------------
013600140414       //?Operazioni iniziali.                                         ?
013700140414       //--------------------------------------------------------------
013800140414       BEGSR  RoutInz;
013900140414
014000140416       //?Pulisco campi di OUTPUT
014100151016         clear fior902ds.OOR902err;
014200151105         clear fior902ds.OOR902mail;
014300160408         clear fior902ds.OOR902sms;
014400160408
014500151105         clear fior902ds.OOR902rso;
014600160408
014700151016         clear fior902ds.OOR902rsr;
014800151016         clear fior902ds.OOR902inr;
014900151016         clear fior902ds.OOR902lor;
015000151016         clear fior902ds.OOR902car;
015100151016         clear fior902ds.OOR902prr;
015200151016         clear fior902ds.OOR902nar;
015300160408
015400160408         clear fior902ds.OOR902dam;
015500160408         clear fior902ds.OOR902aam;
015600160408         clear fior902ds.OOR902dpm;
015700160408         clear fior902ds.OOR902apm;
015800160408
015900160408         clear fior902ds.OOR902rfa;
016000160408
016100160408         clear fior902ds.OOR902ncl;
016200160408         clear fior902ds.OOR902pkg;
016300160408         clear fior902ds.OOR902vlm;
016400160408         clear fior902ds.OOR902bnc;
016500160408         clear fior902ds.OOR902nam;
016600160408
016700160408         clear fior902ds.OOR902ter;
016800160408         clear fior902ds.OOR902ref;
016900160408
017000151105         clear fior902ds.OOR902dpag;
017100160408
017200160408         clear fior902ds.OOR902no1;
017300160408         clear fior902ds.OOR902no2;
017400160408
017500151016         clear fior902ds.OOR902dar;
017600151016         clear fior902ds.OOR902ori;
017700151016         clear fior902ds.OOR902orf;
017800160408
017900160408         clear fior902ds.OOR902npr;
018000160408
018100160408         clear fior902ds.OOR902dpor;
018200160408         clear fior902ds.OOR902mpor;
018300160408         clear fior902ds.OOR902tpor;
018400160408         clear fior902ds.OOR902por1;
018500160408         clear fior902ds.OOR902por2;
018600160408         clear fior902ds.OOR902por3;
018700160408         clear fior902ds.OOR902por4;
018800160502
018900160502         clear mail;
019000160502         clear sms;
019100160502         clear nomeordinante;
019200160502         clear nome;
019300160502         clear indirizzo;
019400160502         clear localita;
019500160502         clear cap;
019600160502         clear provincia;
019700160502         clear nazione;
019800160502         clear progressivo;
019900170809         clear nomechipaga;
020000170809         clear codacr;
020100170809         clear ksc;
020200140417
020300140417       //?Chiusura
020400151016         IF  fior902ds.IOR902tla = *blanks;
020500140417           *inRT = *on;
020600140417         ELSE;
020700140417           *inLR = *on;
020800140417         ENDIF;
020900140417
021000160412       //?Imposto già i dati della chiave x FNORM
021100151019         k_ORMpoe = fior902ds.IOR902poe;
021200151019         k_ORMnsr = fior902ds.IOR902nsr;
021300151019         k_ORMnor = fior902ds.IOR902nor;
021400151019         k_ORMnrv = fior902ds.IOR902nrv;
021500160412
021600160412       //?Imposto già i dati della chiave x FNORE
021700160412         k_OREpoe = fior902ds.IOR902poe;
021800160412         k_OREnsr = fior902ds.IOR902nsr;
021900160412         k_OREnor = fior902ds.IOR902nor;
022000160412         k_OREnrv = fior902ds.IOR902nrv;
022100151020
022200151020       //?Apro file TIORI/TIORE
022300151020         IF  %open(TIORI01L);
022400151020           leavesr;
022500151020         ENDIF;
022600151020
022700151020         UBRTVNETA_Rtv(currSysNeta);
022800151020         IF  %subst(currSysNetA:1:5 ) = 'AS888';
022900151020           wFLIBore = 'GAITRAGRPS/TIORE01L';
023000151020           wFLIBori = 'GAITRAGRPS/TIORI01L';
023100151020         ELSE;
023200151020           wFLIBore = 'GAITRAGRU/TIORE01L';
023300151020           wFLIBori = 'GAITRAGRU/TIORI01L';
023400151020         ENDIF;
023500151020         open TIORI01L;
023600151020         open TIORE01L;
023700140414
023800140414       ENDSR;
023900140414
024000140414       //--------------------------------------------------------------
024100140416       //?Recupero i dati richiesti.
024200140414       //--------------------------------------------------------------
024300140416       BEGSR  GetDati;
024400160408
024500160408         clear nome;
024600160408         clear indirizzo;
024700160408         clear localita;
024800160408         clear cap;
024900160408         clear provincia;
025000160408         clear nazione;
025100170809         clear nomechipaga;
025200170809         clear codacr;
025300170809         clear ksc;
025400151016
025500151016       //?Come prima cosa aggancio l'ORM
025600151016         clear dORM01;
025700151016         chain %kds(K04fnorm) FNORM01L;
025800151016         IF  not %found(FNORM01L);
025900160408           fior902ds.OOR902err = 'N';
026000151016           leavesr;
026100151016         ENDIF;
026200151016
026300151016         dORM01 = ORMflo;
026400160408
026500160408       //?Se orm da VAS recupero i dati da VAS
026600160408         IF  ORMtco = 'I' or ORMtco = 'F';
026700160408           exsr CtrDatiVAS;
026800160408           IF  fior902ds.OOR902err <> *blanks;
026900160408             leavesr;
027000160408           ENDIF;
027100160408           exsr GetDatiVAS;
027200160408         ENDIF;
027300160408
027400160408       //?Se orm NON VAS recupero i dati da ORM
027500160408         IF  ORMtco <> 'I' and ORMtco <> 'F';
027600160408           exsr CtrDatiORM;
027700160408           IF  fior902ds.OOR902err <> *blanks;
027800160408             leavesr;
027900160408           ENDIF;
028000160408           exsr GetDatiORM;
028100160408         ENDIF;
028200160502
028300160502         fior902ds.OOR902mail = mail;
028400160502         fior902ds.OOR902sms  = sms;
028500160408
028600160408       //?Recupero di dati Servizio Clienti della filiale ritiro
028700160408         clear TRULDSCDS;
028800160408         truldscds.IULDSCfil = ORMpor;
028900160408         truldscr (truldscds);
029000160408         IF  truldscds.OULDSCerr <> *blanks;
029100160408           leavesr;
029200160408         ENDIF;
029300160408         fior902ds.OOR902dpor = truldscds.OULDSCdesc;
029400160408         fior902ds.OOR902tpor = truldscds.OULDSCtel;
029500160408         fior902ds.OOR902mpor = truldscds.OULDSCmail;
029600160408         fior902ds.OOR902por1 = truldscds.OULDSCamda;
029700160408         fior902ds.OOR902por2 = truldscds.OULDSCama;
029800160408         fior902ds.OOR902por3 = truldscds.OULDSCpmda;
029900160408         fior902ds.OOR902por4 = truldscds.OULDSCpma;
030000160408
030100160408       ENDSR;
030200160408
030300160408       //--------------------------------------------------------------
030400160408       //?Controllo se ci sono i dati da VAS.
030500160408       //--------------------------------------------------------------
030600160408       BEGSR  CtrDatiVAS;
030700160408
030800160408       //?Se ORM da VAS deve avere il n. progressivo
030900151019         IF  dORM01.§ORMpg = *blanks;
031000160408           fior902ds.OOR902err = 'N';
031100151019           leavesr;
031200151019         ENDIF;
031300151019
031400151019         IF  dORM01.§ORMpg < *zeros;
031500160408           fior902ds.OOR902err = 'N';
031600151019           leavesr;
031700151019         ENDIF;
031800151019
031900151019         progressivo = %dec(dORM01.§ORMpg:7:0);
032000140416
032100160408       //?Deve esistere il TIORI e non essere annullato
032200151016       //?Con il progressivo aggancio il TIORI
032300151019         chain (progressivo) TIORI01L;
032400151016         IF  not %found(TIORI01L);
032500160408           fior902ds.OOR902err = 'N';
032600140417           leavesr;
032700140417         ENDIF;
032800151016
032900151016       //?Non deve essere annullato
033000151016         IF  VAOatb <> *blanks;
033100160408           fior902ds.OOR902err = 'N';
033200151016           leavesr;
033300151016         ENDIF;
033400160408
033500160408       //?Deve avere almeno 1 dei due flag Alert impostato
033600160408         clear dOREGEN;
033700160408         chain (ORIprg:'G ') TIORE01L;
033800160408         IF  not %found(TIORE01L);
033900160408           fior902ds.OOR902err = '1';
034000160408           leavesr;
034100160408         ENDIF;
034200160408         dOREGEN = VAOEdati;
034300160408         IF  dOREGEN.§OREfmco <> 'S' and dOREGEN.§OREfsco <> 'S';
034400160408           fior902ds.OOR902err = '1';
034500160408           leavesr;
034600160408         ENDIF;
034700160408
034800160408       //?Recupero mail
034900161103       //?il dato è meglio prenderlo dall'ORM
035000160408         IF  dOREGEN.§OREfmco = 'S';
035100160408           clear mail;
035200161103           k_OREtrc = 'MC';
035300161103           chain %kds(K05fnore) FNORE01L;
035400161103           IF  not %found(FNORE01L);
035500160408             fior902ds.OOR902err = '1';
035600160408             leavesr;
035700160408           ENDIF;
035800161103           mail = OREdati;
035900160408         ENDIF;
036000160408
036100160408       //?Recupero sms
036200160408         IF  dOREGEN.§OREfsco = 'S';
036300160408           clear sms;
036400161103           k_OREtrc = 'SC';
036500161103           chain %kds(K05fnore) FNORE01L;
036600161103           IF  not %found(FNORE01L);
036700160408             fior902ds.OOR902err = '1';
036800160408             leavesr;
036900160408           ENDIF;
037000161103           sms = OREdati;
037100160408         ENDIF;
037200160408
037300160408       //?Recupero orari ritiro indicativo
037400160408         clear dOREor;
037500160408         chain (ORIprg:'OR') TIORE01L;
037600160408         IF  not %found(TIORE01L);
037700160408           fior902ds.OOR902err = '2';
037800160408           leavesr;
037900160408         ENDIF;
038000160408         dOREor = VAOEdati;
038100160408
038200160408       //?Cerco il Numero Prenotazione Ritiro
038300160408         chain (ORIprg:'NP') TIORE01L;
038400160408         IF  %found(TIORE01L);
038500160408           fior902ds.OOR902npr = VAOEdati;
038600160408         ENDIF;
038700160408
038800160408       //?Cerco gli orari apertura
038900160408         clear dOREorari;
039000160408         chain (ORIprg:'O ') TIORE01L;
039100160408         IF  %found(TIORE01L);
039200160408           dOREorari = VAOEdati;
039300160408         ENDIF;
039400160408
039500160408       ENDSR;
039600160408
039700160408       //--------------------------------------------------------------
039800160408       //?Recupero i dati da VAS.
039900160408       //--------------------------------------------------------------
040000160408       BEGSR  GetDatiVAS;
040100151019
040200151016       //?Se mittente codificato su TIORI non ho i dati, ho solo il codice
040300151016         IF  VAOcra > 0;
040400151016           chain (VAOcra) FNACR01L;
040500151016           IF  %found(FNACR01L);
040600151016             nome = ACRrsr;
040700151016             indirizzo = ACRinr;
040800151016             localita = ACRlor;
040900151016             cap = ACRcar;
041000151016             provincia = ACRprr;
041100151016           ENDIF;
041200151019         ELSE;
041300151019           nome = VAOrsr;
041400151019           indirizzo = VAOinr;
041500151019           localita = VAOlor;
041600151019           cap = VAOcar;
041700151019           provincia = VAOprr;
041800151016         ENDIF;
041900151105
042000160408       //?Nome di chi ci ha comunicato il ritiro
042100151105         clear nomeordinante;
042200151105         IF  VAOcor > 0;
042300151105           chain (VAOcor) FNACR01L;
042400151105           IF  %found(FNACR01L);
042500151105             nomeordinante = ACRrsr;
042600151105           ENDIF;
042700151105         ELSE;
042800151105       //?Se ordinante non codificato imposto il mittente
042900151109           nomeordinante = nome;
043000151105         ENDIF;
043100140417
043200151105         fior902ds.OOR902rso  = nomeordinante;
043300151019         fior902ds.OOR902rsr  = nome;
043400151019         fior902ds.OOR902inr  = indirizzo;
043500151019         fior902ds.OOR902lor  = localita;
043600151019         fior902ds.OOR902car  = cap;
043700151019         fior902ds.OOR902prr  = provincia;
043800151019         fior902ds.OOR902nar  = nazione;
043900151105         fior902ds.OOR902rfa  = VAOrfa;
044000151105         fior902ds.OOR902ter  = VAOter;
044100151109         fior902ds.OOR902ref  = VAOrer;
044200170809
044300170809       //?Nome di chi paga
044400151105         SELECT;
044500151105         WHEN  VAOpag = 'M';
044600170809           SELECT;
044700170809           WHEN  VAOksc > *zeros;
044800170809             ksc = VAOksc;
044900170809             exsr GetNomeKsc;
045000170809             fior902ds.OOR902dpag = nomechipaga;
045100170809           WHEN  VAOcra > *zeros;
045200170809             codacr = VAOcra;
045300170809             exsr GetNomeCodAcr;
045400170809             fior902ds.OOR902dpag = nomechipaga;
045500170809           OTHER;
045600170809             fior902ds.OOR902dpag = VAOrsr;
045700170809           ENDSL;
045800151105         WHEN  VAOpag = 'O';
045900170809           SELECT;
046000170809           WHEN  VAOksc > *zeros;
046100170809             ksc = VAOksc;
046200170809             exsr GetNomeKsc;
046300170809             fior902ds.OOR902dpag = nomechipaga;
046400170809           WHEN  VAOcor > *zeros;
046500170809             codacr = VAOcor;
046600170809             exsr GetNomeCodAcr;
046700170809             fior902ds.OOR902dpag = nomechipaga;
046800170809           ENDSL;
046900151105         WHEN  VAOpag = 'D';
047000170809           SELECT;
047100170809           WHEN  VAOksc > *zeros;
047200170809             ksc = VAOksc;
047300170809             exsr GetNomeKsc;
047400170809             fior902ds.OOR902dpag = nomechipaga;
047500170809           WHEN  VAOcrc > *zeros;
047600170809             codacr = VAOcrc;
047700170809             exsr GetNomeCodAcr;
047800170809             fior902ds.OOR902dpag = nomechipaga;
047900170809           WHEN  VAOrsc <> *blanks;
048000170809             fior902ds.OOR902dpag = VAOrsc;
048100170809           OTHER;
048200170809             fior902ds.OOR902dpag = 'DESTINATARIO';
048300170809           ENDSL;
048400151105         ENDSL;
048500170809
048600151016         fior902ds.OOR902dar  = VAOdar;
048700151019         fior902ds.OOR902ori  = dOREOR.§OREori;
048800151019         fior902ds.OOR902orf  = dOREOR.§OREorf;
048900151016         fior902ds.OOR902ncl  = VAOncl;
049000151019         fior902ds.OOR902pkg  = VAOpkg;
049100151104         fior902ds.OOR902vlm  = VAOvlm;
049200151104         fior902ds.OOR902bnc  = VAObnc;
049300151016         fior902ds.OOR902nam  = VAOnam;
049400151016         fior902ds.OOR902no1  = VAOno1;
049500151016         fior902ds.OOR902no2  = VAOno2;
049600160408         fior902ds.OOR902dam = dOREorari.§OREoramda;
049700160408         fior902ds.OOR902aam = dOREorari.§OREorama;
049800160408         fior902ds.OOR902dpm = dOREorari.§OREorapda;
049900160408         fior902ds.OOR902apm = dOREorari.§OREorapa;
050000140414
050100140414       ENDSR;
050200160408
050300160408       //--------------------------------------------------------------
050400160408       //?Controllo se ci sono i dati da ORM.
050500160408       //--------------------------------------------------------------
050600160408       BEGSR  CtrDatiORM;
050700160408
050800160408       //?Deve avere almeno 1 dei due flag Alert impostato
050900160408         clear dOREGEN;
051000160408         k_OREtrc = 'G ';
051100160408         chain %kds(K05fnore) FNORE01L;
051200160408         IF  not %found(FNORE01L);
051300160408           fior902ds.OOR902err = '1';
051400160408           leavesr;
051500160408         ENDIF;
051600160408         dOREGEN = OREdati;
051700160408         IF  dOREGEN.§OREfmco <> 'S' and dOREGEN.§OREfsco <> 'S';
051800160408           fior902ds.OOR902err = '1';
051900160408           leavesr;
052000160408         ENDIF;
052100160408
052200160408       //?Recupero mail
052300160408         IF  dOREGEN.§OREfmco = 'S';
052400160408           clear mail;
052500160408           k_OREtrc = 'MC';
052600160408           chain %kds(K05fnore) FNORE01L;
052700160408           IF  not %found(FNORE01L);
052800160408             fior902ds.OOR902err = '1';
052900160408             leavesr;
053000160408           ENDIF;
053100160408           mail = OREdati;
053200160408         ENDIF;
053300160408
053400160408       //?Recupero sms
053500160408         IF  dOREGEN.§OREfsco = 'S';
053600160408           clear sms;
053700160408           k_OREtrc = 'SC';
053800160408           chain %kds(K05fnore) FNORE01L;
053900160408           IF  not %found(FNORE01L);
054000160408             fior902ds.OOR902err = '1';
054100160408             leavesr;
054200160408           ENDIF;
054300160408           sms = OREdati;
054400160408         ENDIF;
054500160408
054600160408       //?Recupero orari ritiro indicativo
054700160408         clear dOREor;
054800160408         k_OREtrc = 'OR';
054900160408         chain %kds(K05fnore) FNORE01L;
055000160408         IF  not %found(FNORE01L);
055100160408           fior902ds.OOR902err = '2';
055200160408           leavesr;
055300160408         ENDIF;
055400160408         dOREor = OREdati;
055500160408
055600160408       //?Cerco il Numero Prenotazione Ritiro
055700160408         k_OREtrc = 'NP';
055800160408         chain %kds(K05fnore) FNORE01L;
055900160408         IF  not %found(FNORE01L);
056000160408           fior902ds.OOR902err = '2';
056100160408           leavesr;
056200160408         ENDIF;
056300160408         fior902ds.OOR902npr = OREdati;
056400160408
056500160408       //?Cerco gli orari apertura
056600160408         clear dOREorari;
056700160408         k_OREtrc = 'O ';
056800160408         chain %kds(K05fnore) FNORE01L;
056900160412         IF  %found(FNORE01L);
057000160412           dOREorari = OREdati;
057100160408         ENDIF;
057200160408
057300160408       ENDSR;
057400160408
057500160408       //--------------------------------------------------------------
057600160408       //?Recupero i dati da ORM.
057700160408       //--------------------------------------------------------------
057800160408       BEGSR  GetDatiORM;
057900160408
058000160408       //?Se ordinante presente lo imposto se no prendo il mittente
058100160408         IF  ORMrso <> *blanks;
058200160408           fior902ds.OOR902rso  = ORMrso;
058300160408         ELSE;
058400160408           fior902ds.OOR902rso  = ORMrsr;
058500160408         ENDIF;
058600160408         fior902ds.OOR902rsr  = ORMrsr;
058700160408         fior902ds.OOR902inr  = ORMinr;
058800160408         fior902ds.OOR902lor  = ORMlor;
058900160408         fior902ds.OOR902car  = ORMcar;
059000160408         fior902ds.OOR902prr  = ORMprr;
059100160408         fior902ds.OOR902nar  = ORMnar;
059200160408         fior902ds.OOR902rfa  = ORMrfa;
059300160408         fior902ds.OOR902ter  = ORMter;
059400160408         fior902ds.OOR902ref  = ORMrer;
059500170809
059600170809       //?Nome di chi paga
059700160408         SELECT;
059800160408         WHEN  ORMpag = 'M';
059900170809           SELECT;
060000170809           WHEN  ORMksc > *zeros;
060100170809             ksc = ORMksc;
060200170809             exsr GetNomeKsc;
060300170809             fior902ds.OOR902dpag = nomechipaga;
060400170809           WHEN  ORMcra > *zeros;
060500170809             codacr = ORMcra;
060600170809             exsr GetNomeCodAcr;
060700170809             fior902ds.OOR902dpag = nomechipaga;
060800170809           OTHER;
060900170809             fior902ds.OOR902dpag = ORMrsr;
061000170809           ENDSL;
061100160408         WHEN  ORMpag = 'O';
061200170809           SELECT;
061300170809           WHEN  ORMksc > *zeros;
061400170809             ksc = ORMksc;
061500170809             exsr GetNomeKsc;
061600170809             fior902ds.OOR902dpag = nomechipaga;
061700170809           WHEN  ORMcor > *zeros;
061800170809             codacr = ORMcor;
061900170809             exsr GetNomeCodAcr;
062000170809             fior902ds.OOR902dpag = nomechipaga;
062100170809           ENDSL;
062200160408         WHEN  ORMpag = 'D';
062300170809           SELECT;
062400170809           WHEN  ORMksc > *zeros;
062500170809             ksc = ORMksc;
062600170809             exsr GetNomeKsc;
062700170809             fior902ds.OOR902dpag = nomechipaga;
062800170809           WHEN  ORMcrc > *zeros;
062900170809             codacr = ORMcrc;
063000170809             exsr GetNomeCodAcr;
063100170809             fior902ds.OOR902dpag = nomechipaga;
063200170809           WHEN  ORMrsc <> *blanks;
063300170809             fior902ds.OOR902dpag = ORMrsc;
063400170809           OTHER;
063500170809             fior902ds.OOR902dpag = 'DESTINATARIO';
063600170809           ENDSL;
063700160408         ENDSL;
063800170809
063900160408         fior902ds.OOR902dar = ORMdar;
064000160408         fior902ds.OOR902ori = dOREOR.§OREori;
064100160408         fior902ds.OOR902orf = dOREOR.§OREorf;
064200160408         fior902ds.OOR902ncl = ORMncl;
064300160408         fior902ds.OOR902pkg = ORMpkg;
064400160408         fior902ds.OOR902vlm = ORMvlm;
064500160408         fior902ds.OOR902bnc = ORMbnc;
064600160408         fior902ds.OOR902nam = ORMnam;
064700160408         fior902ds.OOR902no1 = ORMno1;
064800160408         fior902ds.OOR902no2 = ORMno2;
064900160408         fior902ds.OOR902dam = dOREorari.§OREoramda;
065000160408         fior902ds.OOR902aam = dOREorari.§OREorama;
065100160408         fior902ds.OOR902dpm = dOREorari.§OREorapda;
065200160408         fior902ds.OOR902apm = dOREorari.§OREorapa;
065300160408
065400160408       ENDSR;
065500170809
065600170809       //--------------------------------------------------------------
065700170809       //?Recupero ragione sociale da anagrafica clienti.
065800170809       //--------------------------------------------------------------
065900170809       BEGSR  GetNomeKsc;
066000170809
066100170809         clear nomechipaga;
066200170809         ACOkut = 1;
066300170809         ACOkcc = 151;
066400170809         chain (ACOkut:ACOkcc:ksc) CNACO00F;
066500170809         IF  %found(CNACO00F);
066600170809           nomechipaga = ACOrag;
066700170809         ENDIF;
066800170809
066900170809       ENDSR;
067000170809
067100170809       //--------------------------------------------------------------
067200170809       //?Recupero ragione sociale da anagrafica clienti ritiro.
067300170809       //--------------------------------------------------------------
067400170809       BEGSR  GetNomeCodAcr;
067500170809
067600170809         clear nomechipaga;
067700170809         chain (codacr) FNACR01L;
067800170809         IF  %found(FNACR01L);
067900170809           nomechipaga = ACRrsr;
068000170809         ENDIF;
068100170809
068200170809       ENDSR;
068300140414
068400140414       //--------------------------------------------------------------
068500140414       //?Operazioni finali.
068600140414       //--------------------------------------------------------------
068700140414       BEGSR  RoutEnd;
068800140414
068900140414         return;
069000140414
069100140414       ENDSR;
069200140414
069300140414       //--------------------------------------------------------------
069400140415      /end-free
