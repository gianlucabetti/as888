000100100413      //---------------------------------------------------------------
000200100519      //?TNVRCRM11 - Aggiorna file di work visite
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000100511     fWFVIS01L  uf   e           k disk    rename(WFVIS000:WFVIS01)
001100100511     f                                     prefix(n_)
001200100414     fTNVRCRMP  o    e             printer oflind(*in99)
001300100413
001400100413      //---------------------------------------------------------------
001500100413      //?Definizione costanti.
001600100413      //---------------------------------------------------------------
001700100413
001800100413      //---------------------------------------------------------------
001900100413      //?Definizione schiere.
002000100413      //---------------------------------------------------------------
002100100513
002200100513     d filiali         s              3a   dim(10)
002300090715
002400100413      //---------------------------------------------------------------
002500100413      //?Definizione aree dati.
002600100413      //---------------------------------------------------------------
002700100413
002800100413      //---------------------------------------------------------------
002900100413      //?Definizione strutture dati.
003000100413      //---------------------------------------------------------------
003100100423
003200100423     d KPJBA         e ds
003300100510
003400100510      // File trattative
003500100510     d TIVISds       e ds                  extname(TIVIS00F)
003600100510
003700100510      // File work
003800100510     d WFVISds       e ds                  extname(WFVIS00F)
003900100413
004000100413      //---------------------------------------------------------------
004100100413      //?Definizione variabili globali.
004200100413      //---------------------------------------------------------------
004300100413
004400100413      // - Flags booleani
004500100413     d $End            s               n   inz(*off)
004600100413     d $Fine           s               n   inz(*off)
004700100511     d $Ok             s               n   inz(*off)
004800100621     d $NoOff          s               n   inz(*off)
004900100413
005000100413      // - Campi di comodo
005100100511     d conta           s              5i 0
005200100510     d old_WIScmm      s                   like(WIScmm)
005300100510     d old_WIScpo      s                   like(WIScpo)
005400100510     d old_WISksc      s                   like(WISksc)
005500100513     d TotA            s              7s 0 inz
005600100513     d TotB            s              7s 0 inz
005700100513     d TotC            s              7s 0 inz
005800100513     d TotD            s              7s 0 inz
005900100513     d TotE            s              7s 0 inz
006000100513     d TotF            s              7s 0 inz
006100100513     d TotG            s              7s 0 inz
006200100513     d TotH            s              7s 0 inz
006300100513     d TotI            s              7s 0 inz
006400100510     d wDataMin        s              8  0
006500100510     d wDataOggi       s              8  0
006600100621     d wDataOggi30     s              8  0
006700100510     d wData_ISO       s               d   datfmt(*iso)
006800100413
006900100413      //---------------------------------------------------------------
007000100413      //?Definizione procedure esterne.
007100100413      //---------------------------------------------------------------
007200100413
007300100413      //---------------------------------------------------------------
007400100413      //?prototipi
007500100413      //---------------------------------------------------------------
007600100413
007700100413      //---------------------------------------------------------------
007800100413      //?Definizione key-list.
007900100413      //---------------------------------------------------------------
008000100413
008100100413      //---------------------------------------------------------------
008200100413      //?Riepilogo indicatori.
008300100413      //---------------------------------------------------------------
008400100414
008500100414      // 99    : Salto pagina
008600100413
008700100413      //---------------------------------------------------------------
008800100413      //?M A I N - L I N E
008900100413      //---------------------------------------------------------------
009000100413
009100100413     c     *Entry        plist
009200100513     c                   parm                    filiali
009300100413
009400100413      /free
009500100413
009600100413       //?Operazioni iniziali
009700100513       exsr RoutInz;
009800100413
009900100510       //?Aggiorno le trattative da trascodificare
010000100510       exsr Elabora;
010100100413
010200100413       //?Operazioni finali
010300100513       exsr RoutEnd;
010400100413
010500100413       //--------------------------------------------------------------
010600100413       //?Operazioni iniziali.
010700100413       //--------------------------------------------------------------
010800100513       BEGSR RoutInz;
010900100510
011000100510         wData_ISO = %date();
011100100510         wDataOggi = %dec(wData_ISO);
011200100621         wDataOggi30 = %dec(wData_ISO - %days(30));
011300100510
011400100511       //?Imposto data minima per tenere le visite
011500100511         wDataMin = 20100301;
011600100513
011700100513         clear TotA;
011800100513         clear TotB;
011900100513         clear TotC;
012000100513         clear TotD;
012100100513         clear TotE;
012200100513         clear TotF;
012300100513         clear TotG;
012400100513         clear TotH;
012500100513         clear TotI;
012600100413
012700100510         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012800100413
012900100413       ENDSR;
013000100413
013100100413       //--------------------------------------------------------------
013200100510       //?Elabora.
013300100413       //--------------------------------------------------------------
013400100510       BEGSR Elabora;
013500100511
013600100511       //?Se data visita < data consentita non devo scrivere la visita nel nuovo file
013700100513         exec sql
013800100513         SELECT  count(*) into :TotA FROM WFVIS00F
013900100513         WHERE   WISdvi < :wDataMin;
014000100511         exec sql
014100100511         UPDATE  WFVIS00F set WISnewf = 'N'
014200100511         WHERE   WISdvi < :wDataMin;
014300100510
014400100511       //?Se visita 'M' o 'C' senza offerte con data visita <= data trascodifica
014500100511       //?non devo riportare nel nuovo file
014600100513         exec sql
014700100513         SELECT  count(*) into :TotB FROM WFVIS00F
014800100513         WHERE   WIStpv in ('M', 'C') and WISoff = ' ' and
014900100513                 WISdvi <= :wDataOggi and WISnewf = ' ';
015000100510         exec sql
015100100510         UPDATE  WFVIS00F set WISnewf = 'N'
015200100510         WHERE   WIStpv in ('M', 'C') and WISoff = ' ' and
015300100511                 WISdvi <= :wDataOggi and WISnewf = ' ';
015400100510
015500100510       //?Se trattativa di prova diventa fittizia
015600100510         exec sql
015700100510         UPDATE  WFVIS00F set WISnffz = 'S'
015800100510         WHERE   WISffz = 'P';
015900100510
016000100510       //?Se trattativa di tipo 'I' diventa 'A'
016100100510         exec sql
016200100510         UPDATE  WFVIS00F set WISntpv = 'A'
016300100510         WHERE   WIStpv = 'I';
016400100510
016500100510       //?Se trattativa 'C' incasso o 'M' mantenimento con offerta
016600100510       //?diventa 'A'
016700100510         exec sql
016800100510         UPDATE  WFVIS00F set WISntpv = 'A'
016900100510         WHERE   WIStpv in ('C', 'M') and WISoff = 'S';
017000100510
017100100510       //?Se trattativa fittizia imposto nuovo flag fittizia
017200100510         exec sql
017300100510         UPDATE  WFVIS00F set WISnffz = 'S'
017400100510         WHERE   WISffz = 'F';
017500100510
017600100510       //?Se trattativa fittizia senza offerta non la devo scrivere nel nuovo file
017700100513         exec sql
017800100513         SELECT  count(*) into :TotC FROM WFVIS00F
017900100513         WHERE   WISnffz = 'S' and WISoff = ' ' and WISnewf = ' ';
018000100510         exec sql
018100100510         UPDATE  WFVIS00F set WISnewf = 'N'
018200100511         WHERE   WISnffz = 'S' and WISoff = ' ' and WISnewf = ' ';
018300100510
018400100511       //?Leggo le visite non fittizie per controllare che non ci siano già trattative aperte
018500100511       //?per ogni commerciale/potenziale/cliente devo avere una sola trattativa NON fittizia
018600100511         $End = *off;
018700100510         exec sql
018800100511         DECLARE VISTRAT cursor for
018900100511         SELECT  WIScmm, WIScpo, WISksc
019000100510         FROM WFVIS00F
019100100511         WHERE WISnewf = ' ' and WISnffz = ' '
019200100511         GROUP by WIScmm, WIScpo, WISksc
019300100511         ORDER by WIScmm, WIScpo, WISksc;
019400100510
019500100511         exec sql OPEN VISTRAT;
019600100510
019700100510         DOW  not $End;
019800100511           exec sql FETCH next from VISTRAT into :WIScmm, :WIScpo, :WISksc;
019900100510           IF  sqlcod = 100 or sqlcod < 0;
020000100510             $End = *on;
020100100510             leave;
020200100510           ENDIF;
020300100510
020400100511         //?controllo se ci sono già trattative aperte
020500100511           clear conta;
020600100511           exec sql
020700100511           SELECT count(*) into :conta from TIVIS00F
020800100511           WHERE VIScmm = :WIScmm and VIScpo = :WIScpo and
020900100511                 VISksc = :WISksc and VISdch = 0;
021000100511         //?se ho trovato almeno una trattativa devo rendere fittizia le nuove che
021100100511         //?andrò a scrivere
021200100511           IF conta > *zeros;
021300100511             setll (WIScmm:WIScpo:WISksc) WFVIS01L;
021400100511             reade (WIScmm:WIScpo:WISksc) WFVIS01L;
021500100511             DOW not %eof(WFVIS01L);
021600100511               n_WISnffz = 'S';
021700100511               update WFVIS01;
021800100511             ENDDO;
021900100511           ENDIF;
022000100510
022100100510         ENDDO;
022200100510
022300100511         exec sql CLOSE VISTRAT;
022400100511
022500100511       //?Leggo le visite non fittizie per controllare di avere solo una trattativa NON fittizia
022600100511       //?per ogni commerciale/potenziale/cliente
022700100511         $End = *off;
022800100511         exec sql
022900100511         DECLARE VIS cursor for
023000100511         SELECT  WFVIS00F.*
023100100511         FROM WFVIS00F
023200100511         WHERE WISnewf = ' ' and WISnffz = ' '
023300100511         ORDER by WIScmm, WISCPO, WISksc, WISdvi desc,
023400100511                  WIShmv desc, WISnrv desc;
023500100511
023600100511         exec sql OPEN VIS;
023700100511
023800100511         DOW  not $End;
023900100511           exec sql FETCH next from VIS into :WFVISds;
024000100511           IF  sqlcod = 100 or sqlcod < 0;
024100100511             $End = *on;
024200100511             leave;
024300100511           ENDIF;
024400100511
024500100511           //?per ogni commerciale/potenziale/cliente devo avere solo una trattativa NON fittizia
024600100511           IF  WIScmm <> old_WIScmm or WIScpo <> old_WIScpo or
024700100511               WISksc <> old_WISksc;
024800100511             $Ok = *off;
024900100621             $NoOff = *off;
025000100621             //?salvo la rottura per comm/pot/cliente
025100100621             old_WIScmm = WIScmm;
025200100621             old_WIScpo = WIScpo;
025300100621             old_WISksc = WISksc;
025400100511             //?se data visita > data trascodifica OK la visita che sto leggendo e le altre fittizie
025500100511             IF  WISdvi > wDataOggi;
025600100511               $Ok = *on;
025700100621               iter;
025800100621             ENDIF;
025900100621             //?se data visita <= data trascodifica
026000100621             //?e visita con offerta OK la visita che sto leggendo e le altre fittizie
026100100621             IF  WISdvi <= wDataOggi and WISoff = 'S';
026200100621               $Ok = *on;
026300100621               iter;
026400100621             ENDIF;
026500100621             //?se data visita <= data trascodifica
026600100621             //?e visita senza offerta OK
026700100621             //?e tutte le visite sono senza offerte e data visita compresa tra oggi e oggi -30 gg.
026800100621             //?OK la visita che sto leggendo e le altre fittizie
026900100621             IF  WISdvi <= wDataOggi and WISoff = *blanks;
027000100621               setll (WIScmm:WIScpo:WISksc) WFVIS01L;
027100100621               reade (WIScmm:WIScpo:WISksc) WFVIS01L;
027200100621               $NoOff = *on;
027300100621               DOW  not %eof(WFVIS01L);
027400100621                 IF  n_WISoff = 'S';
027500100621                   $NoOff = *off;
027600100621                   leave;
027700100621                 ENDIF;
027800100621                 reade (WIScmm:WIScpo:WISksc) WFVIS01L;
027900100621               ENDDO;
028000100621               //?Nessuna con offerta controllo la data
028100100621               IF  $NoOff and WISdvi >= wDataOggi30;
028200100621                 $Ok = *on;
028300100621                 iter;
028400100621               ENDIF;
028500100621             ENDIF;
028600100621             //?se arrivo a questo punto vuol dire che la visita non è da tenere
028700100621             //?quindi la mmetto fittizia
028800100621             chain (WIScmm:WIScpo:WISksc:WISnrv) WFVIS01L;
028900100621             IF  %Found(WFVIS01L);
029000100621               n_WISnffz = 'S';
029100100621               update WFVIS01;
029200100621             ENDIF;
029300100511             iter;
029400100511           ENDIF;
029500100511
029600100511           //?stesso commerciale/potenziale/cliente
029700100511           //?se ho già trovato una visita buona le altre fittizie
029800100511           IF  $Ok;
029900100511             chain (WIScmm:WIScpo:WISksc:WISnrv) WFVIS01L;
030000100511             IF  %Found(WFVIS01L);
030100100511               n_WISnffz = 'S';
030200100511               update WFVIS01;
030300100511             ENDIF;
030400100511           ELSE;
030500100511           //?se non ho trovato ancora una visita OK controllo se quella che sto leggendo
030600100511           //?ha offerte, quindi questa diventa OK e le altre fittizie
030700100511             IF  WISoff = 'S';
030800100511               $Ok = *on;
030900100511             ELSE;
031000100511               chain (WIScmm:WIScpo:WISksc:WISnrv) WFVIS01L;
031100100511               IF  %Found(WFVIS01L);
031200100511                 n_WISnffz = 'S';
031300100511                 update WFVIS01;
031400100511               ENDIF;
031500100511             ENDIF;
031600100511           ENDIF;
031700100511
031800100511         ENDDO;
031900100511
032000100511         exec sql CLOSE VIS;
032100100511
032200100511       //?Se trattativa fittizia senza offerta non la devo scrivere nel nuovo file
032300100511       //?rifaccio il giro dopo aver controllato di avere una sola trattativa per
032400100511       //?commerciale potenziale cliente
032500100513         exec sql
032600100513         SELECT  count(*) into :TotD FROM WFVIS00F
032700100513         WHERE   WISnffz = 'S' and WISoff = ' ' and WISnewf = ' ';
032800100511         exec sql
032900100511         UPDATE  WFVIS00F set WISnewf = 'N'
033000100511         WHERE   WISnffz = 'S' and WISoff = ' ' and WISnewf = ' ';
033100100513
033200100513       //?Conto quante visite ho trasformato in fittizie e che diventeranno trattative
033300100513         exec sql
033400100513         SELECT  count(*) into :TotE FROM WFVIS00F
033500100513         WHERE   WISnffz = 'S' and WISffz = ' ' and WISnewf = ' ';
033600100513
033700100513       //?Conto quante trattative su clienti mai codificati
033800100513         exec sql
033900100513         SELECT  count(*) into :TotF FROM WFVIS00F
034000100513         WHERE   WISnffz = ' ' and WIScod = 'S' and WISnewf = ' ';
034100100513
034200100513       //?Conto quante trattative con offerta
034300100513         exec sql
034400100513         SELECT  count(*) into :TotG FROM WFVIS00F
034500100513         WHERE   WISoff = 'S' and WISnewf = ' ' and WISnffz = ' ';
034600100513
034700100513       //?Conto quante trattative senza offerta con data > oggi
034800100513         exec sql
034900100513         SELECT  count(*) into :TotH FROM WFVIS00F
035000100513         WHERE   WISoff = ' ' and WISnewf = ' ' and WISnffz = ' '
035100100513                 and WISdvi > :wDataOggi;
035200100513
035300100513       //?Conto quante trattative senza offerta con data <= oggi
035400100513         exec sql
035500100513         SELECT  count(*) into :TotI FROM WFVIS00F
035600100513         WHERE   WISoff = ' ' and WISnewf = ' ' and WISnffz = ' '
035700100513                 and WISdvi <= :wDataOggi;
035800100513
035900100513       //?Stampa totali
036000100513         exsr Stampa;
036100100413
036200100413       ENDSR;
036300100414
036400100414       //--------------------------------------------------------------
036500100414       //?Stampa errori.
036600100414       //--------------------------------------------------------------
036700100513       BEGSR Stampa;
036800100414
036900100513         write CRMT01;
037000100519         DESpgm = 'Aggiorna file di Work WFVIS00F';
037100100517         fil01 = %dec(Filiali(01):3:0);
037200100517         fil02 = %dec(Filiali(02):3:0);
037300100517         fil03 = %dec(Filiali(03):3:0);
037400100517         fil04 = %dec(Filiali(04):3:0);
037500100517         fil05 = %dec(Filiali(05):3:0);
037600100517         fil06 = %dec(Filiali(06):3:0);
037700100517         fil07 = %dec(Filiali(07):3:0);
037800100517         fil08 = %dec(Filiali(08):3:0);
037900100517         fil09 = %dec(Filiali(09):3:0);
038000100517         fil10 = %dec(Filiali(10):3:0);
038100100513         write CRMT02;
038200100513         write CRMTOTW;
038300100414
038400100414       ENDSR;
038500100413
038600100413       //--------------------------------------------------------------
038700100413       //?Operazioni finali.
038800100413       //--------------------------------------------------------------
038900100513       BEGSR RoutEnd;
039000100413
039100100413         *inLR = *on;
039200100413         return;
039300100413
039400100413       ENDSR;
039500100413
039600100413      /end-free
