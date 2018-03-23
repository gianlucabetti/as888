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
004800100413
004900100413      // - Campi di comodo
005000100511     d conta           s              5i 0
005100100510     d old_WIScmm      s                   like(WIScmm)
005200100510     d old_WIScpo      s                   like(WIScpo)
005300100510     d old_WISksc      s                   like(WISksc)
005400100513     d TotA            s              7s 0 inz
005500100513     d TotB            s              7s 0 inz
005600100513     d TotC            s              7s 0 inz
005700100513     d TotD            s              7s 0 inz
005800100513     d TotE            s              7s 0 inz
005900100513     d TotF            s              7s 0 inz
006000100513     d TotG            s              7s 0 inz
006100100513     d TotH            s              7s 0 inz
006200100513     d TotI            s              7s 0 inz
006300100510     d wDataMin        s              8  0
006400100510     d wDataOggi       s              8  0
006500100510     d wData_ISO       s               d   datfmt(*iso)
006600100413
006700100413      //---------------------------------------------------------------
006800100413      //?Definizione procedure esterne.
006900100413      //---------------------------------------------------------------
007000100413
007100100413      //---------------------------------------------------------------
007200100413      //?prototipi
007300100413      //---------------------------------------------------------------
007400100413
007500100413      //---------------------------------------------------------------
007600100413      //?Definizione key-list.
007700100413      //---------------------------------------------------------------
007800100413
007900100413      //---------------------------------------------------------------
008000100413      //?Riepilogo indicatori.
008100100413      //---------------------------------------------------------------
008200100414
008300100414      // 99    : Salto pagina
008400100413
008500100413      //---------------------------------------------------------------
008600100413      //?M A I N - L I N E
008700100413      //---------------------------------------------------------------
008800100413
008900100413     c     *Entry        plist
009000100513     c                   parm                    filiali
009100100413
009200100413      /free
009300100413
009400100413       //?Operazioni iniziali
009500100513       exsr RoutInz;
009600100413
009700100510       //?Aggiorno le trattative da trascodificare
009800100510       exsr Elabora;
009900100413
010000100413       //?Operazioni finali
010100100513       exsr RoutEnd;
010200100413
010300100413       //--------------------------------------------------------------
010400100413       //?Operazioni iniziali.
010500100413       //--------------------------------------------------------------
010600100513       BEGSR RoutInz;
010700100510
010800100510         wData_ISO = %date();
010900100510         wDataOggi = %dec(wData_ISO);
011000100510
011100100511       //?Imposto data minima per tenere le visite
011200100511         wDataMin = 20100301;
011300100513
011400100513         clear TotA;
011500100513         clear TotB;
011600100513         clear TotC;
011700100513         clear TotD;
011800100513         clear TotE;
011900100513         clear TotF;
012000100513         clear TotG;
012100100513         clear TotH;
012200100513         clear TotI;
012300100413
012400100510         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012500100413
012600100413       ENDSR;
012700100413
012800100413       //--------------------------------------------------------------
012900100510       //?Elabora.
013000100413       //--------------------------------------------------------------
013100100510       BEGSR Elabora;
013200100511
013300100511       //?Se data visita < data consentita non devo scrivere la visita nel nuovo file
013400100513         exec sql
013500100513         SELECT  count(*) into :TotA FROM WFVIS00F
013600100513         WHERE   WISdvi < :wDataMin;
013700100511         exec sql
013800100511         UPDATE  WFVIS00F set WISnewf = 'N'
013900100511         WHERE   WISdvi < :wDataMin;
014000100510
014100100511       //?Se visita 'M' o 'C' senza offerte con data visita <= data trascodifica
014200100511       //?non devo riportare nel nuovo file
014300100513         exec sql
014400100513         SELECT  count(*) into :TotB FROM WFVIS00F
014500100513         WHERE   WIStpv in ('M', 'C') and WISoff = ' ' and
014600100513                 WISdvi <= :wDataOggi and WISnewf = ' ';
014700100510         exec sql
014800100510         UPDATE  WFVIS00F set WISnewf = 'N'
014900100510         WHERE   WIStpv in ('M', 'C') and WISoff = ' ' and
015000100511                 WISdvi <= :wDataOggi and WISnewf = ' ';
015100100510
015200100510       //?Se trattativa di prova diventa fittizia
015300100510         exec sql
015400100510         UPDATE  WFVIS00F set WISnffz = 'S'
015500100510         WHERE   WISffz = 'P';
015600100510
015700100510       //?Se trattativa di tipo 'I' diventa 'A'
015800100510         exec sql
015900100510         UPDATE  WFVIS00F set WISntpv = 'A'
016000100510         WHERE   WIStpv = 'I';
016100100510
016200100510       //?Se trattativa 'C' incasso o 'M' mantenimento con offerta
016300100510       //?diventa 'A'
016400100510         exec sql
016500100510         UPDATE  WFVIS00F set WISntpv = 'A'
016600100510         WHERE   WIStpv in ('C', 'M') and WISoff = 'S';
016700100510
016800100510       //?Se trattativa fittizia imposto nuovo flag fittizia
016900100510         exec sql
017000100510         UPDATE  WFVIS00F set WISnffz = 'S'
017100100510         WHERE   WISffz = 'F';
017200100510
017300100510       //?Se trattativa fittizia senza offerta non la devo scrivere nel nuovo file
017400100513         exec sql
017500100513         SELECT  count(*) into :TotC FROM WFVIS00F
017600100513         WHERE   WISnffz = 'S' and WISoff = ' ' and WISnewf = ' ';
017700100510         exec sql
017800100510         UPDATE  WFVIS00F set WISnewf = 'N'
017900100511         WHERE   WISnffz = 'S' and WISoff = ' ' and WISnewf = ' ';
018000100510
018100100511       //?Leggo le visite non fittizie per controllare che non ci siano già trattative aperte
018200100511       //?per ogni commerciale/potenziale/cliente devo avere una sola trattativa NON fittizia
018300100511         $End = *off;
018400100510         exec sql
018500100511         DECLARE VISTRAT cursor for
018600100511         SELECT  WIScmm, WIScpo, WISksc
018700100510         FROM WFVIS00F
018800100511         WHERE WISnewf = ' ' and WISnffz = ' '
018900100511         GROUP by WIScmm, WIScpo, WISksc
019000100511         ORDER by WIScmm, WIScpo, WISksc;
019100100510
019200100511         exec sql OPEN VISTRAT;
019300100510
019400100510         DOW  not $End;
019500100511           exec sql FETCH next from VISTRAT into :WIScmm, :WIScpo, :WISksc;
019600100510           IF  sqlcod = 100 or sqlcod < 0;
019700100510             $End = *on;
019800100510             leave;
019900100510           ENDIF;
020000100510
020100100511         //?controllo se ci sono già trattative aperte
020200100511           clear conta;
020300100511           exec sql
020400100511           SELECT count(*) into :conta from TIVIS00F
020500100511           WHERE VIScmm = :WIScmm and VIScpo = :WIScpo and
020600100511                 VISksc = :WISksc and VISdch = 0;
020700100511         //?se ho trovato almeno una trattativa devo rendere fittizia le nuove che
020800100511         //?andrò a scrivere
020900100511           IF conta > *zeros;
021000100511             setll (WIScmm:WIScpo:WISksc) WFVIS01L;
021100100511             reade (WIScmm:WIScpo:WISksc) WFVIS01L;
021200100511             DOW not %eof(WFVIS01L);
021300100511               n_WISnffz = 'S';
021400100511               update WFVIS01;
021500100511             ENDDO;
021600100511           ENDIF;
021700100510
021800100510         ENDDO;
021900100510
022000100511         exec sql CLOSE VISTRAT;
022100100511
022200100511       //?Leggo le visite non fittizie per controllare di avere solo una trattativa NON fittizia
022300100511       //?per ogni commerciale/potenziale/cliente
022400100511         $End = *off;
022500100511         exec sql
022600100511         DECLARE VIS cursor for
022700100511         SELECT  WFVIS00F.*
022800100511         FROM WFVIS00F
022900100511         WHERE WISnewf = ' ' and WISnffz = ' '
023000100511         ORDER by WIScmm, WISCPO, WISksc, WISdvi desc,
023100100511                  WIShmv desc, WISnrv desc;
023200100511
023300100511         exec sql OPEN VIS;
023400100511
023500100511         DOW  not $End;
023600100511           exec sql FETCH next from VIS into :WFVISds;
023700100511           IF  sqlcod = 100 or sqlcod < 0;
023800100511             $End = *on;
023900100511             leave;
024000100511           ENDIF;
024100100511
024200100511           //?per ogni commerciale/potenziale/cliente devo avere solo una trattativa NON fittizia
024300100511           IF  WIScmm <> old_WIScmm or WIScpo <> old_WIScpo or
024400100511               WISksc <> old_WISksc;
024500100511             $Ok = *off;
024600100511             //?se data visita > data trascodifica OK la visita che sto leggendo e le altre fittizie
024700100511             IF  WISdvi > wDataOggi;
024800100511               $Ok = *on;
024900100511             ELSE;
025000100511             //?se visita con offerta OK la visita che sto leggendo e le altre fittizie
025100100511               IF  WISoff = 'S';
025200100511                 $Ok = *on;
025300100511               ELSE;
025400100511                 chain (WIScmm:WIScpo:WISksc:WISnrv) WFVIS01L;
025500100511                 IF  %Found(WFVIS01L);
025600100511                   n_WISnffz = 'S';
025700100511                   update WFVIS01;
025800100511                 ENDIF;
025900100511               ENDIF;
026000100511             ENDIF;
026100100511             old_WIScmm = WIScmm;
026200100511             old_WIScpo = WIScpo;
026300100511             old_WISksc = WISksc;
026400100511             iter;
026500100511           ENDIF;
026600100511
026700100511           //?stesso commerciale/potenziale/cliente
026800100511           //?se ho già trovato una visita buona le altre fittizie
026900100511           IF  $Ok;
027000100511             chain (WIScmm:WIScpo:WISksc:WISnrv) WFVIS01L;
027100100511             IF  %Found(WFVIS01L);
027200100511               n_WISnffz = 'S';
027300100511               update WFVIS01;
027400100511             ENDIF;
027500100511           ELSE;
027600100511           //?se non ho trovato ancora una visita OK controllo se quella che sto leggendo
027700100511           //?ha offerte, quindi questa diventa OK e le altre fittizie
027800100511             IF  WISoff = 'S';
027900100511               $Ok = *on;
028000100511             ELSE;
028100100511               chain (WIScmm:WIScpo:WISksc:WISnrv) WFVIS01L;
028200100511               IF  %Found(WFVIS01L);
028300100511                 n_WISnffz = 'S';
028400100511                 update WFVIS01;
028500100511               ENDIF;
028600100511             ENDIF;
028700100511           ENDIF;
028800100511
028900100511         ENDDO;
029000100511
029100100511         exec sql CLOSE VIS;
029200100511
029300100511       //?Se trattativa fittizia senza offerta non la devo scrivere nel nuovo file
029400100511       //?rifaccio il giro dopo aver controllato di avere una sola trattativa per
029500100511       //?commerciale potenziale cliente
029600100513         exec sql
029700100513         SELECT  count(*) into :TotD FROM WFVIS00F
029800100513         WHERE   WISnffz = 'S' and WISoff = ' ' and WISnewf = ' ';
029900100511         exec sql
030000100511         UPDATE  WFVIS00F set WISnewf = 'N'
030100100511         WHERE   WISnffz = 'S' and WISoff = ' ' and WISnewf = ' ';
030200100513
030300100513       //?Conto quante visite ho trasformato in fittizie e che diventeranno trattative
030400100513         exec sql
030500100513         SELECT  count(*) into :TotE FROM WFVIS00F
030600100513         WHERE   WISnffz = 'S' and WISffz = ' ' and WISnewf = ' ';
030700100513
030800100513       //?Conto quante trattative su clienti mai codificati
030900100513         exec sql
031000100513         SELECT  count(*) into :TotF FROM WFVIS00F
031100100513         WHERE   WISnffz = ' ' and WIScod = 'S' and WISnewf = ' ';
031200100513
031300100513       //?Conto quante trattative con offerta
031400100513         exec sql
031500100513         SELECT  count(*) into :TotG FROM WFVIS00F
031600100513         WHERE   WISoff = 'S' and WISnewf = ' ' and WISnffz = ' ';
031700100513
031800100513       //?Conto quante trattative senza offerta con data > oggi
031900100513         exec sql
032000100513         SELECT  count(*) into :TotH FROM WFVIS00F
032100100513         WHERE   WISoff = ' ' and WISnewf = ' ' and WISnffz = ' '
032200100513                 and WISdvi > :wDataOggi;
032300100513
032400100513       //?Conto quante trattative senza offerta con data <= oggi
032500100513         exec sql
032600100513         SELECT  count(*) into :TotI FROM WFVIS00F
032700100513         WHERE   WISoff = ' ' and WISnewf = ' ' and WISnffz = ' '
032800100513                 and WISdvi <= :wDataOggi;
032900100513
033000100513       //?Stampa totali
033100100513         exsr Stampa;
033200100413
033300100413       ENDSR;
033400100414
033500100414       //--------------------------------------------------------------
033600100414       //?Stampa errori.
033700100414       //--------------------------------------------------------------
033800100513       BEGSR Stampa;
033900100414
034000100513         write CRMT01;
034100100519         DESpgm = 'Aggiorna file di Work WFVIS00F';
034200100517         fil01 = %dec(Filiali(01):3:0);
034300100517         fil02 = %dec(Filiali(02):3:0);
034400100517         fil03 = %dec(Filiali(03):3:0);
034500100517         fil04 = %dec(Filiali(04):3:0);
034600100517         fil05 = %dec(Filiali(05):3:0);
034700100517         fil06 = %dec(Filiali(06):3:0);
034800100517         fil07 = %dec(Filiali(07):3:0);
034900100517         fil08 = %dec(Filiali(08):3:0);
035000100517         fil09 = %dec(Filiali(09):3:0);
035100100517         fil10 = %dec(Filiali(10):3:0);
035200100513         write CRMT02;
035300100513         write CRMTOTW;
035400100414
035500100414       ENDSR;
035600100413
035700100413       //--------------------------------------------------------------
035800100413       //?Operazioni finali.
035900100413       //--------------------------------------------------------------
036000100513       BEGSR RoutEnd;
036100100413
036200100413         *inLR = *on;
036300100413         return;
036400100413
036500100413       ENDSR;
036600100413
036700100413      /end-free
