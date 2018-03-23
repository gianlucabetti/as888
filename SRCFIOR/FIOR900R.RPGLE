000100140414       //==============================================================
000200140414       //
000300140417       //?FIOR900R - Reperisce mail/sms
000400140414       //
000500140414       //==============================================================
000600140414     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000700140414
000800140414       //--------------------------------------------------------------
000900140414       //?Dichiarazione file.                                          ?
001000140414       //--------------------------------------------------------------
001100140417       //?File estensione ORM
001200140416     fFNORE01L  if   e           k disk
001300140414
001400140414       //--------------------------------------------------------------
001500140414       //?Definizione costanti.                                        ?
001600140414       //--------------------------------------------------------------
001700140414
001800140414       //---------------------------------------------------------------
001900140414       //?Definizione aree dati.
002000140414       //---------------------------------------------------------------
002100140414
002200140414       //--------------------------------------------------------------
002300140414       //?Definizione strutture dati.                                  ?
002400140414       //--------------------------------------------------------------
002500140417       //?DS FIOR900R
002600140417     d FIOR900DS     e ds                  qualified
002700140417
002800140417       //?DS rcd GEN FNVAOE
002900140417     d dOREGEN       e ds                  qualified inz
003000140417
003100140417       //?DS rcd S FNVAOE
003200140417     d dORESMS       e ds                  qualified inz
003300140414
003400140414       //--------------------------------------------------------------
003500140414       //?Definizione schiere.
003600140414       //--------------------------------------------------------------
003700140415
003800140414
003900140414       //--------------------------------------------------------------
004000140414       //?Definizione campi.
004100140414       //--------------------------------------------------------------
004200140417       //?Flags boolenai
004300140414     d wFine           s               n   inz(*off)
004400140414
004500140417       //?Campi di comodo
004600140417     d wmail           s                   like(fior900ds.OOR900mail)
004700140414
004800140414       //--------------------------------------------------------------
004900140414       //?Definizione procedure.
005000140414       //--------------------------------------------------------------
005100140414
005200140414       //--------------------------------------------------------------
005300140414       //?Definizione prototipi.
005400140414       //--------------------------------------------------------------
005500140417
005600140417       //---------------------------------------------------------------
005700140417       //?Definizione key-list.
005800140417       //---------------------------------------------------------------
005900140417
006000140417       //?File FNORE01L
006100140417     d k05fnore      e ds                  extname(FNORE01L:*key)
006200140417     d                                     prefix(k_)
006300140414
006400140414       //--------------------------------------------------------------
006500140414       //?MAIN.
006600140414       //--------------------------------------------------------------
006700140416
006800140416     c     *entry        plist
006900140416     c                   parm                    FIOR900DS
007000140414
007100140414      /free
007200140414
007300140414       //?Operazioni iniziali
007400140414       exsr RoutInz;
007500140414
007600140416       //?Cerca dati richiesti
007700140416       IF  fior900ds.IOR900tla <> 'C';
007800140416         exsr GetDati;
007900140416       ENDIF;
008000140414
008100140414       //?Operazioni finali
008200140414       exsr RoutEnd;
008300140414
008400140414       //--------------------------------------------------------------
008500140414       //?Operazioni iniziali.                                         ?
008600140414       //--------------------------------------------------------------
008700140414       BEGSR  RoutInz;
008800140414
008900140416       //?Pulisco campi di OUTPUT
009000140416         clear fior900ds.OOR900err;
009100140416         clear fior900ds.OOR900mail;
009200140416         clear fior900ds.OOR900sms;
009300140417
009400140417       //?Chiusura
009500140417         IF  fior900ds.IOR900tla = *blanks;
009600140417           *inRT = *on;
009700140417         ELSE;
009800140417           *inLR = *on;
009900140417         ENDIF;
010000140414
010100140416       //?Se tipo rcd non passato imposto MA x mail e S x SMS
010200140416         IF  fior900ds.IOR900rcdm = *blanks;
010300140416           fior900ds.IOR900rcdm = 'MA';
010400140416         ENDIF;
010500140416         IF  fior900ds.IOR900rcds = *blanks;
010600140416           fior900ds.IOR900rcds = 'S ';
010700140416         ENDIF;
010800140417
010900140417       //?Imposto già i dati della chiave x FNORE
011000140417         k_OREpoe = fior900ds.IOR900poe;
011100140417         k_OREnsr = fior900ds.IOR900nsr;
011200140417         k_OREnor = fior900ds.IOR900nor;
011300140417         k_OREnrv = fior900ds.IOR900nrv;
011400140414
011500140414       ENDSR;
011600140414
011700140414       //--------------------------------------------------------------
011800140416       //?Recupero i dati richiesti.
011900140414       //--------------------------------------------------------------
012000140416       BEGSR  GetDati;
012100140417
012200140417       //?Se tipo ricerca ha un valore non previsto errore
012300140417         IF  fior900ds.IOR900trc <> 'M' and
012400140417             fior900ds.IOR900trc <> 'S' and
012500140417             fior900ds.IOR900trc <> 'E' and
012600140417             fior900ds.IOR900trc <> 'A';
012700140417           fior900ds.OOR900err = 'N';
012800140417           leavesr;
012900140417         ENDIF;
013000140416
013100140416       //?Come prima cosa devo agganciare il rcd GEN
013200140417         clear dOREGEN;
013300140417         k_OREtrc = 'G';
013400140417         chain %kds(K05fnore) FNORE01L;
013500140417         IF  not %found(FNORE01L);
013600140417           fior900ds.OOR900err = 'N';
013700140417           leavesr;
013800140417         ENDIF;
013900140416
014000140417         dOREGEN = OREdati;
014100140417
014200140417         SELECT;
014300140417       //?Se richiesto mail o sms e non ho i relativi flag --> errore
014400140417           WHEN  (fior900ds.IOR900trc = 'M' and
014500140417                  doregen.§OREfimo = *blanks) or
014600140417                 (fior900ds.IOR900trc = 'S' and
014700140417                  doregen.§OREfiso = *blanks);
014800140417             fior900ds.OOR900err = 'N';
014900140417             leavesr;
015000140417
015100140417       //?Se richiesto mail e sms e non ho i 2 flag --> errore
015200140417           WHEN  fior900ds.IOR900trc = 'E' and
015300140417                 doregen.§OREfimo = *blanks and
015400140417                 doregen.§OREfiso = *blanks;
015500140417             fior900ds.OOR900err = 'N';
015600140417             leavesr;
015700140417
015800140417       //?Se richiesto alert e i 2 flag NON sono a S --> errore
015900140417           WHEN  fior900ds.IOR900trc = 'A' and
016000140417                 doregen.§OREfimo <> 'S' and
016100140417                 doregen.§OREfiso <> 'S';
016200140417             fior900ds.OOR900err = 'N';
016300140417             leavesr;
016400140417         ENDSL;
016500140417
016600140417       //?Se arrivo qua vuol dire che i flag sono congruenti con le richieste
016700140417
016800140417       //?Recupero mail
016900140417         clear wmail;
017000140417         k_OREtrc = fior900ds.IOR900rcdm;
017100140417         chain %kds(k05fnore) FNORE01L;
017200140417         IF  %found(FNORE01L);
017300140417           wmail = OREdati;
017400140417         ENDIF;
017500140417
017600140417       //?Recupero SMS
017700140417         clear dORESMS;
017800140417         k_OREtrc = fior900ds.IOR900rcds;
017900140417         chain %kds(k05fnore) FNORE01L;
018000140417         IF  %found(FNORE01L);
018100140417           dORESMS = OREdati;
018200140417         ENDIF;
018300140417
018400140417       //?A questo punto devo impostare i dati di ritorno in base a quanto richiesto
018500140417         SELECT;
018600140417         //?Richiesta mail e sms
018700140417           WHEN  fior900ds.IOR900trc = 'E';
018800140417             fior900ds.OOR900mail = wmail;
018900140417             fior900ds.OOR900sms = doresms.§OREsms;
019000140417         //?Richiesta la mail
019100140417           WHEN  fior900ds.IOR900trc = 'M';
019200140417             fior900ds.OOR900mail = wmail;
019300140417         //?Richiesto sms
019400140417           WHEN  fior900ds.IOR900trc = 'S';
019500140417             fior900ds.OOR900sms = doresms.§OREsms;
019600140417         //?Richiesto Alert
019700140417           WHEN  fior900ds.IOR900trc = 'A';
019800140417           //?passo tutti e 2 i dati se flag = S
019900140417             IF  doregen.§OREfimo = 'S';
020000140417               fior900ds.OOR900mail = wmail;
020100140417             ENDIF;
020200140417             IF  doregen.§OREfiso = 'S';
020300140417               fior900ds.OOR900sms = doresms.§OREsms;
020400140417             ENDIF;
020500140417
020600140417         ENDSL;
020700140414
020800140414       ENDSR;
020900140414
021000140414       //--------------------------------------------------------------
021100140414       //?Operazioni finali.
021200140414       //--------------------------------------------------------------
021300140414       BEGSR  RoutEnd;
021400140414
021500140414         return;
021600140414
021700140414       ENDSR;
021800140414
021900140414
022000140414       //--------------------------------------------------------------
022100140415      /end-free
