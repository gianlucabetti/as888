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
001300151105       //?Oranigramma di filiale
001400151105     fAZORG01L  if   e           k disk
001500151105
001600151016       //?File Anagrafica clienti ritiro
001700151016     fFNACR01L  if   e           k disk
001800151016
001900151016       //?File ORM
002000151016     fFNORM01L  if   e           k disk
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
004100140417
004200151016       //?DS rcd OR
004300151016     d dOREOR        e ds                  qualified inz
004400151016
004500151016       //?DS campo ORMflo
004600151016     d dORM01        e ds                  qualified inz
004700140414
004800140414       //--------------------------------------------------------------
004900140414       //?Definizione schiere.
005000140414       //--------------------------------------------------------------
005100140414
005200140414       //--------------------------------------------------------------
005300140414       //?Definizione campi.
005400140414       //--------------------------------------------------------------
005500140417       //?Flags boolenai
005600140414     d wFine           s               n   inz(*off)
005700151016
005800151016       //?Campi di comodo
005900151019     d cap             s                   like(ORMcar)
006000151019     d indirizzo       s                   like(ORMinr)
006100151019     d localita        s                   like(ORMlor)
006200151016     d mail            s                   like(fior902ds.OOR902mail)
006300151019     d nazione         s                   like(ORMnar)
006400151016     d nome            s                   like(ORMrsr)
006500151105     d nomeordinante   s                   like(ORMrso)
006600151019     d progressivo     s                   like(ORIprg)
006700151016     d provincia       s                   like(ORMprr)
006800151020
006900151020       // -?Nome esteso Libreria/File
007000151020     d wFLIBore        s             21a   inz
007100151020     d wFLIBori        s             21a   inz
007200151020
007300151020       // -?Nome del sistema?
007400151020     d currSysNeta     s              8a   inz
007500140414
007600140414       //--------------------------------------------------------------
007700140414       //?Definizione procedure.
007800140414       //--------------------------------------------------------------
007900140414
008000140414       //--------------------------------------------------------------
008100140414       //?Definizione prototipi.
008200140414       //--------------------------------------------------------------
008300151020       // -?Reperimento NETA sistema AS/400 corrente?
008400151020      /copy gaitrasrc/srcProtoPR,UBRTVNETA
008500140417
008600140417       //---------------------------------------------------------------
008700140417       //?Definizione key-list.
008800140417       //---------------------------------------------------------------
008900151016       //?File FNORM01L
009000151016     d k04fnorm      e ds                  extname(FNORM01L:*key)
009100151019     d                                     prefix(k_)
009200140414
009300140414       //--------------------------------------------------------------
009400140414       //?MAIN.
009500140414       //--------------------------------------------------------------
009600140416
009700140416     c     *entry        plist
009800151019     c                   parm                    FIOR902DS
009900140414
010000140414      /free
010100140414
010200140414       //?Operazioni iniziali
010300140414       exsr RoutInz;
010400140414
010500140416       //?Cerca dati richiesti
010600151016       IF  fior902ds.IOR902tla <> 'C';
010700140416         exsr GetDati;
010800140416       ENDIF;
010900140414
011000140414       //?Operazioni finali
011100140414       exsr RoutEnd;
011200140414
011300140414       //--------------------------------------------------------------
011400140414       //?Operazioni iniziali.                                         ?
011500140414       //--------------------------------------------------------------
011600140414       BEGSR  RoutInz;
011700140414
011800140416       //?Pulisco campi di OUTPUT
011900151016         clear fior902ds.OOR902err;
012000151105         clear fior902ds.OOR902mail;
012100151104         clear fior902ds.OOR902por;
012200151105         clear fior902ds.OOR902dpor;
012300151105         clear fior902ds.OOR902poe;
012400151105         clear fior902ds.OOR902dpoe;
012500151105         clear fior902ds.OOR902rso;
012600151016         clear fior902ds.OOR902rsr;
012700151016         clear fior902ds.OOR902inr;
012800151016         clear fior902ds.OOR902lor;
012900151016         clear fior902ds.OOR902car;
013000151016         clear fior902ds.OOR902prr;
013100151016         clear fior902ds.OOR902nar;
013200151105         clear fior902ds.OOR902rfa;
013300151105         clear fior902ds.OOR902ter;
013400151109         clear fior902ds.OOR902ref;
013500151016         clear fior902ds.OOR902pag;
013600151105         clear fior902ds.OOR902dpag;
013700151016         clear fior902ds.OOR902dar;
013800151016         clear fior902ds.OOR902ori;
013900151016         clear fior902ds.OOR902orf;
014000151016         clear fior902ds.OOR902ncl;
014100151016         clear fior902ds.OOR902pkg;
014200151104         clear fior902ds.OOR902vlm;
014300151104         clear fior902ds.OOR902bnc;
014400151016         clear fior902ds.OOR902nam;
014500151016         clear fior902ds.OOR902no1;
014600151016         clear fior902ds.OOR902no2;
014700140417
014800140417       //?Chiusura
014900151016         IF  fior902ds.IOR902tla = *blanks;
015000140417           *inRT = *on;
015100140417         ELSE;
015200140417           *inLR = *on;
015300140417         ENDIF;
015400140417
015500140417       //?Imposto già i dati della chiave x FNORE
015600151019         k_ORMpoe = fior902ds.IOR902poe;
015700151019         k_ORMnsr = fior902ds.IOR902nsr;
015800151019         k_ORMnor = fior902ds.IOR902nor;
015900151019         k_ORMnrv = fior902ds.IOR902nrv;
016000151020
016100151020       //?Apro file TIORI/TIORE
016200151020         IF  %open(TIORI01L);
016300151020           leavesr;
016400151020         ENDIF;
016500151020
016600151020         UBRTVNETA_Rtv(currSysNeta);
016700151020         IF  %subst(currSysNetA:1:5 ) = 'AS888';
016800151020           wFLIBore = 'GAITRAGRPS/TIORE01L';
016900151020           wFLIBori = 'GAITRAGRPS/TIORI01L';
017000151020         ELSE;
017100151020           wFLIBore = 'GAITRAGRU/TIORE01L';
017200151020           wFLIBori = 'GAITRAGRU/TIORI01L';
017300151020         ENDIF;
017400151020         open TIORI01L;
017500151020         open TIORE01L;
017600140414
017700140414       ENDSR;
017800140414
017900140414       //--------------------------------------------------------------
018000140416       //?Recupero i dati richiesti.
018100140414       //--------------------------------------------------------------
018200140416       BEGSR  GetDati;
018300151016
018400151016       //?Come prima cosa aggancio l'ORM
018500151016         clear dORM01;
018600151016         chain %kds(K04fnorm) FNORM01L;
018700151016         IF  not %found(FNORM01L);
018800151016           fior902ds.OOR902err = '1';
018900151016           leavesr;
019000151016         ENDIF;
019100151016
019200151016         dORM01 = ORMflo;
019300151019
019400151019         IF  dORM01.§ORMpg = *blanks;
019500151019           fior902ds.OOR902err = '1';
019600151019           leavesr;
019700151019         ENDIF;
019800151019
019900151019         IF  dORM01.§ORMpg < *zeros;
020000151019           fior902ds.OOR902err = '1';
020100151019           leavesr;
020200151019         ENDIF;
020300151019
020400151019         progressivo = %dec(dORM01.§ORMpg:7:0);
020500140416
020600151016       //?Con il progressivo aggancio il TIORI
020700151019         chain (progressivo) TIORI01L;
020800151016         IF  not %found(TIORI01L);
020900151016           fior902ds.OOR902err = '1';
021000140417           leavesr;
021100140417         ENDIF;
021200151016
021300151016       //?Non deve essere annullato
021400151016         IF  VAOatb <> *blanks;
021500151016           fior902ds.OOR902err = '1';
021600151016           leavesr;
021700151016         ENDIF;
021800151016
021900151016         clear nome;
022000151016         clear indirizzo;
022100151016         clear localita;
022200151016         clear cap;
022300151016         clear provincia;
022400151016         clear nazione;
022500151019
022600151016       //?Se mittente codificato su TIORI non ho i dati, ho solo il codice
022700151016         IF  VAOcra > 0;
022800151016           chain (VAOcra) FNACR01L;
022900151016           IF  %found(FNACR01L);
023000151016             nome = ACRrsr;
023100151016             indirizzo = ACRinr;
023200151016             localita = ACRlor;
023300151016             cap = ACRcar;
023400151016             provincia = ACRprr;
023500151016           ENDIF;
023600151019         ELSE;
023700151019           nome = VAOrsr;
023800151019           indirizzo = VAOinr;
023900151019           localita = VAOlor;
024000151019           cap = VAOcar;
024100151019           provincia = VAOprr;
024200151016         ENDIF;
024300151105
024400151105         clear nomeordinante;
024500151105
024600151105       //?Se ordinante codificato su TIORI non ho i dati, ho solo il codice
024700151105         IF  VAOcor > 0;
024800151105           chain (VAOcor) FNACR01L;
024900151105           IF  %found(FNACR01L);
025000151105             nomeordinante = ACRrsr;
025100151105           ENDIF;
025200151105         ELSE;
025300151105       //?Se ordinante non codificato imposto il mittente
025400151109           nomeordinante = nome;
025500151105         ENDIF;
025600140417
025700140417       //?Recupero mail
025800151016         clear mail;
025900151016         chain (ORIprg:'MC') TIORE01L;
026000151016         IF  not %found(TIORE01L);
026100151016           fior902ds.OOR902err = '1';
026200151016           leavesr;
026300140417         ENDIF;
026400151016
026500151016         mail = VAOEdati;
026600151016
026700151016       //?Recupero orari ritiro indicativo
026800151016         clear dOREor;
026900151016         chain (ORIprg:'OR') TIORE01L;
027000151016         IF  not %found(TIORE01L);
027100151016           fior902ds.OOR902err = '1';
027200151016           leavesr;
027300151016         ENDIF;
027400151016
027500151016         dOREor = VAOEdati;
027600140417
027700151016       //?A questo punto devo impostare i dati di ritorno
027800151105         fior902ds.OOR902mail = mail;
027900151105         fior902ds.OOR902poe  = VAOpoe;
028000151105         chain VAOpoe AZORG01L;
028100151105         IF  %found(AZORG01L);
028200151105           fior902ds.OOR902dpoe = ORGdes;
028300151105         ENDIF;
028400151104         fior902ds.OOR902por  = VAOpor;
028500151105         chain VAOpor AZORG01L;
028600151105         IF  %found(AZORG01L);
028700151105           fior902ds.OOR902dpor = ORGdes;
028800151105         ENDIF;
028900151105         fior902ds.OOR902rso  = nomeordinante;
029000151019         fior902ds.OOR902rsr  = nome;
029100151019         fior902ds.OOR902inr  = indirizzo;
029200151019         fior902ds.OOR902lor  = localita;
029300151019         fior902ds.OOR902car  = cap;
029400151019         fior902ds.OOR902prr  = provincia;
029500151019         fior902ds.OOR902nar  = nazione;
029600151105         fior902ds.OOR902rfa  = VAOrfa;
029700151105         fior902ds.OOR902ter  = VAOter;
029800151109         fior902ds.OOR902ref  = VAOrer;
029900151016         fior902ds.OOR902pag  = VAOpag;
030000151105         SELECT;
030100151105         WHEN  VAOpag = 'M';
030200151105           fior902ds.OOR902dpag = 'MITTENTE';
030300151105         WHEN  VAOpag = 'O';
030400151105           fior902ds.OOR902dpag = 'ORDINANTE';
030500151105         WHEN  VAOpag = 'D';
030600151105           fior902ds.OOR902dpag = 'DESTINATARIO';
030700151105         ENDSL;
030800151016         fior902ds.OOR902dar  = VAOdar;
030900151019         fior902ds.OOR902ori  = dOREOR.§OREori;
031000151019         fior902ds.OOR902orf  = dOREOR.§OREorf;
031100151016         fior902ds.OOR902ncl  = VAOncl;
031200151019         fior902ds.OOR902pkg  = VAOpkg;
031300151104         fior902ds.OOR902vlm  = VAOvlm;
031400151104         fior902ds.OOR902bnc  = VAObnc;
031500151016         fior902ds.OOR902nam  = VAOnam;
031600151016         fior902ds.OOR902no1  = VAOno1;
031700151016         fior902ds.OOR902no2  = VAOno2;
031800140414
031900140414       ENDSR;
032000140414
032100140414       //--------------------------------------------------------------
032200140414       //?Operazioni finali.
032300140414       //--------------------------------------------------------------
032400140414       BEGSR  RoutEnd;
032500140414
032600140414         return;
032700140414
032800140414       ENDSR;
032900140414
033000140414
033100140414       //--------------------------------------------------------------
033200140415      /end-free
