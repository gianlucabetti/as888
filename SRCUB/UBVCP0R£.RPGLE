000100091228       //==============================================================
000200161124       // Driver VAS Contatori Procedure
000300161124       //
000400161124       // Può essere usato solo come sottoprocedura.
000500091228       //==============================================================
000600091228
000700091228       //--------------------------------------------------------------
000800170427       // NOTE:
000900170427       // - questo srvpgm contiene più procedure, ogni procedura coi suoi parametri producono una
001000170523       //   modifica della firma, che lo rendebbe inusabile ai pgm che già lo usano e che
001100170523       //   non sono stati ricompilati. Per questo c'è un sorgente con l'elenco delle
001200170523       //   procedure e una firma fissa.
001300091228       //--------------------------------------------------------------
001400091228
001500160202     **
001600160202     ** ISTRUZIONI PER LA COMPILAZIONE
001700160202     **
001800161124     ** 1. Creare il modulo UBVCP0R (Opz 15 PDM)
001900170314     ** 2. Creare/Aggiornare il programma di servizio UBVCP0R (CRTSRVPGM / UPDSRVPGM)
002000170314     **    con ACTGRP(*caller)
002100170427     **    CRTSRVPGM SRVPGM(GAITRAFIL/UBVCP0R) EXPORT(*SRCFILE)
002200170427     **    SRCFILE(GAITRASRC/SRCSRVPGM) TEXT('Driver VAS Contatori Procedure')
002300170314     ** 3. Non deve essere anche pgm
002400160202     ** 4. Cancellare il modulo.
002500160202     **
002600091228
002700091228       //--------------------------------------------------------------
002800121106       // Specifiche di controllo.
002900091228       //--------------------------------------------------------------
003000091223
003100091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003200161124     h NoMain
003300091223
003400091223       //--------------------------------------------------------------
003500121106       // Dichiarazione file.
003600091223       //--------------------------------------------------------------
003700100112
003800170426     FTIVCP01I  UF A E           K DISK
003900091223
004000091223       //--------------------------------------------------------------
004100121106       // Definizione costanti.
004200091223       //--------------------------------------------------------------
004300100302
004400091223
004500091223       //--------------------------------------------------------------
004600121106       // Definizione schiere.
004700091223       //--------------------------------------------------------------
004800091223
004900091223
005000091223       //--------------------------------------------------------------
005100121106       // Definizione aree dati.
005200091223       //--------------------------------------------------------------
005300091223
005400091223
005500140724       //--------------------------------------------------------------
005600140724       // Definizione strutture dati.
005700140724       //--------------------------------------------------------------
005800140724
005900121106       // - Status
006000110516     d Psds           sds
006100110516     d   SDSpgm          *proc
006200121106     d   JobName             244    253                                         Job name
006300121106     d   JobUser             254    263                                         User name
006400121106     d   JobNumber           264    269s 0                                      Job number
006500091223
006600091223       //--------------------------------------------------------------
006700161124       // Definizione variabili globali.
006800091223       //--------------------------------------------------------------
006900091223
007000091223
007100091223       //--------------------------------------------------------------
007200121106       // Definizione prototipi procedure.
007300091223       //--------------------------------------------------------------
007400130307
007500161124
007600091223       //--------------------------------------------------------------
007700121106       // Definizione key-list.
007800091223       //--------------------------------------------------------------
007900091223
008000140806
008100161124       //--------------------------------------------------------------
008200170426       // Procedure name: UBVCP0R_PRG                                 ??
008300161129       // Purpose:        Restituisce un progressivo contatore aggiornando il file dei contatori
008400161124       //--------------------------------------------------------------
008500161124
008600161129     p UBVCP0R_PRG...
008700161124     p                 B                   export
008800170412
008900170426      /copy gaitrasrc/srcProtoPR,ubvcp0r_01
009000170412
009100170426      /copy gaitrasrc/srcProtoPI,ubvcp0r_01
009200161124
009300161124       //--------------------------------------------------------------
009400161124       // Definizione variabili locali.                                ?
009500161124       //--------------------------------------------------------------
009600161124
009700161129     d wPrg            s                   like(VCPPrg)
009800170413     d SavePrg         s                   like(VCPPrg)
009900161124     d wAnno           s                   like(VCPAnno)
010000170307       // quanti progressivi stacco?
010100170412     d wNrPrg          s                   like(VCPPrg)
010200170412       // primo progressivo staccato
010300170412     d wNumDal         s                   like(VCPPrg)
010400170412       // valore massimo
010500170412     d wTo             s                   like(VCPTo)
010600161124
010700170307       // inizializzazione
010800170307
010900170307       clear oVCPNum;
011000170413       clear oVCPEsito;
011100170413       select;
011200170413        when %parms >= 7;
011300170412         clear oVCPNumDal;
011400170413         clear oVCPIncr;
011500170413        when %parms = 6;
011600170413         clear oVCPNumDal;
011700170413       endsl;
011800170307       // se il numero progressivi da staccare è 0, lo imposto a 1
011900170413       if %parms >= 5;
012000170307         if iVCPNrPrg = 0;
012100170307           wNrPrg = 1;
012200170307         else;
012300170307           wNrPrg = iVCPNrPrg;
012400170307         endif;
012500170307       else;
012600170307         wNrPrg = 1;
012700170412         wNumDal = 0;
012800170307       endif;
012900170307
013000161124       // monitorizzo l'intera attività
013100161124       monitor;
013200161124
013300170314         // leggo il contatore richiesto allocandolo
013400170412         chain  (iVCPTPID:iVCPID)  TIVCP000;
013500170413
013600170413         // se non trovato esco con errore
013700170413         if not %found();
013800170413           oVCPEsito=-1;
013900170413           return;
014000170413         endif;
014100161124
014200170413         // se annullato non lo considero ed esco con errore
014300161124         if VCPAnn <> *blank;
014400170314           unlock(e) TIVCP01I;
014500170413           oVCPEsito=-1;
014600170307           return;
014700161124         endif;
014800170413
014900170413         // se in anagrafica il numero massimo stacchi non stato indicato, lo considero =1
015000170413         if VCPMaxPrg = 0;
015100170413           VCPMaxPrg = 1;
015200170413         endif;
015300170412
015400170412         // se il chiamante ha richiesto un numero stacchi > del possibile
015500170412         if wNrPrg > VCPMaxPrg;
015600170413           // esco con errore
015700170412           unlock(e) TIVCP01I;
015800170413           oVCPEsito=-2;
015900170412           return;
016000170412         endif;
016100170412
016200170413         // se in anagrafica il valore di partenza non è stato indicato, lo considero =1
016300170413         if VCPFrom = 0;
016400170413           VCPFrom = 1;
016500170412         endif;
016600170413
016700170413         // se in anagrafica l'incremento non è stato indicato, lo considero =1
016800170413         if VCPIncr = 0;
016900170413           VCPIncr = 1;
017000170413         endif;
017100130307
017200161129         clear wAnno;
017300161124         // se il contatore ha l'anno
017400161124         if VCPAnno > 0;
017500161124           wAnno = %subdt(%date():*years);
017600161124           // se l'anno del contatore non è quello in corso
017700161124           if VCPAnno <> wAnno;
017800170412             // azzero il numeratore
017900161129             wPrg = 0;
018000161124           else;
018100161124             // parto dal valore nel rcd
018200161129             wPrg = VCPPrg;
018300161124           endif;
018400161124         else;
018500161124           // parto dal valore nel rcd
018600170314           wPrg = VCPPrg;
018700161124         endif;
018800170413         // memorizzo da che valore sono partito
018900170413         SavePrg = wPrg;
019000091223
019100170412         // definisco il valore massimo del progressivo
019200170412         // se in anagrafica è impostata una lunghezza massima, è questa
019300170412         if VCPTo > 0;
019400170412           wTo = VCPTo;
019500170412         // altrimenti è il valore massimo contenibile nel campo
019600170412         else;
019700170412           //    123456789012345 (15 cifre)
019800170412           //    999999999999999
019900170412           wTo = *hival;
020000170412         endif;
020100170523
020200170523         // controllo congruenza progressivo salvato rispetto ai limiti partenza/arrivo
020300170523         if wPrg < VCPFrom or
020400170523            wPrg > wTo;
020500170523           // faccio finta sia la prima volta
020600170523           wPrg = 0;
020700170523         endif;
020800170412
020900170413         dou wPrg > 0;
021000170523
021100170523           // se sono all'inizio
021200170523           if wPrg = 0;
021300170523             // il primo progressivo da staccare è il valore di partenza incrementato
021400170523             wNumDal = VCPFrom + VCPIncr - 1;
021500170523           // se NON sono all'inizio
021600170523           else;
021700170523             // il primo progressivo da staccare è il progressivo incrementato
021800170523             wNumDal = wPrg + VCPIncr;
021900170523           endif;
022000170413           // l'ultimo progressivo da staccare è
022100170413           // l'incremento per tutti i progressivi da staccare - 1 (che è il primo)
022200170413           wPrg = wNumDal + VCPIncr*(wNrPrg - 1);
022300170413
022400170413           // se supero la lunghezza massima
022500170413           if wPrg > wTo;
022600170413             // se sono partito da 0 e ho superato il valore massimo, non serve ripartire da capo
022700170413             if SavePrg = 0;
022800170413               // esco con errore
022900170413               unlock(e) TIVCP01I;
023000170413               oVCPEsito=-2;
023100170413               return;
023200170413             endif;
023300170413             // altrimenti riparto da capo
023400170413             wPrg = 0;
023500170413             // memorizzo che riparto da capo
023600170413             SavePrg = wPrg;
023700170413           endif;
023800161124
023900170413         enddo;
024000170413
024100161124         // valorizzo i dati che potrebbero variare
024200161129         VCPPrg = wPrg;
024300161124         VCPAnno = wAnno;
024400161124
024500161124         // aggiorno
024600161124         update TIVCP000;
024700170307
024800170412         oVCPNum = wPrg;
024900170412         // valorizzo il campo di uscita DAL (quello di arrivo è valorizzato prima)
025000170413         select;
025100170413          when %parms >= 7;
025200170413           oVCPNumDal = wNumDal;
025300170413           oVCPIncr = VCPIncr;
025400170413          when %parms = 6;
025500170413           oVCPNumDal = wNumDal;
025600170413         endsl;
025700161124
025800161124       on-error;
025900161124         // se c'è un errore restiruisco progressivo 0
026000170307         clear oVCPNum;
026100161124
026200161124       endmon;
026300091223
026400170314       unlock(e) TIVCP01I;
026500170307       return;
026600161124
026700161129     p UBVCP0R_PRG...
026800161124     p                 E
026900170426
027000170426       //--------------------------------------------------------------
027100170426       // Procedure name: UBVCP0R_CRT                                 ??
027200170426       // Purpose:        Crea/iscrive un contatore in modo automatico
027300170426       //--------------------------------------------------------------
027400170426
027500170426     p UBVCP0R_CRT...
027600170426     p                 B                   export
027700170426
027800170426      /copy gaitrasrc/srcProtoPR,ubvcp0r_02
027900170426
028000170426      /copy gaitrasrc/srcProtoPI,ubvcp0r_02
028100170426
028200170426       //--------------------------------------------------------------
028300170426       // Definizione variabili locali.                                ?
028400170426       //--------------------------------------------------------------
028500170426     d wVCPIncr        s                   like(VCPIncr)
028600170427     d wVCPMaxPrg      s                   like(VCPMaxPrg)
028700170426
028800170426
028900170426       // inizializzazione
029000170426       clear oVCPEsito;
029100170426       // se l'incremento passato è 0, lo considero =1
029200170426       wVCPIncr = iVCPIncr;
029300170426       if wVCPIncr = 0;
029400170426         wVCPIncr = 1;
029500170426       endif;
029600170427       // se il nr. max stacchi passato è 0, lo considero =1
029700170427       wVCPMaxPrg = iVCPMaxPrg;
029800170427       if wVCPMaxPrg = 0;
029900170427         wVCPMaxPrg = 1;
030000170427       endif;
030100170426
030200170426       // controllo parametri in ingresso
030300170426       if iVCPId = *blank;
030400170426         oVCPEsito=-2;
030500170426         return;
030600170426       endif;
030700170426
030800170426       // monitorizzo l'intera attività
030900170426       monitor;
031000170426
031100170428         // controllo se esiste già il contatore da iscrivere
031200170428         chain (iVCPTPID:iVCPID)  TIVCP000;
031300170426
031400170428         // se trovato ma mai usato (Prg = 0) o annullato (Ann='A') lo sovrascrivo
031500170428         // in altri casi dò errore
031600170428         if %found(TIVCP01I)
031700170428         and (VCPPrg > 0);
031800170426           oVCPEsito=-2;
031900170426           return;
032000170426         endif;
032100170426
032200170426         // valorizzo i dati con ciò che è stato passato
032300170426         VCPTPID   = iVCPTPID    ;
032400170426         VCPID     = iVCPID      ;
032500170426         VCPAnno   = iVCPAnno    ;
032600170426         VCPFrom   = iVCPFrom    ;
032700170426         VCPTo     = iVCPTo      ;
032800170426         VCPIncr   = wVCPIncr    ;
032900170426         VCPMaxPrg = iVCPMaxPrg  ;
033000170427         // valorizzo i dati restanti
033100170427         VCPFlo = *blank         ;
033200170426         %subst(VCPFlo:1:1)= 'A' ;
033300170427         VCPAnn = *blank;
033400170427         VCPPrg = 0;
033500170426
033600170426         // iscrivo
033700170428         if %found(TIVCP01I);
033800170428           update TIVCP000;
033900170428         else;
034000170428           write TIVCP000;
034100170428         endif;
034200170426
034300170426       on-error;
034400170426         // se c'è un errore restiruisco esito in errore
034500170426         oVCPEsito=-1;
034600170426
034700170426       endmon;
034800170426
034900170426       unlock(e) TIVCP01I;
035000170426       return;
035100170426
035200170426     p UBVCP0R_CRT...
035300170426     p                 E
035400170426
035500170426       //--------------------------------------------------------------
035600170426       // Procedure name: UBVCP0R_ANN                                 ??
035700170426       // Purpose:        Annulla un contatore in modo automatico
035800170426       //--------------------------------------------------------------
035900170426
036000170426     p UBVCP0R_ANN...
036100170426     p                 B                   export
036200170426
036300170426      /copy gaitrasrc/srcProtoPR,ubvcp0r_03
036400170426
036500170426      /copy gaitrasrc/srcProtoPI,ubvcp0r_03
036600170426
036700170426       //--------------------------------------------------------------
036800170426       // Definizione variabili locali.                                ?
036900170426       //--------------------------------------------------------------
037000170426
037100170426
037200170426       // inizializzazione
037300170426       clear oVCPEsito;
037400170426
037500170426       // controllo parametri in ingresso
037600170426       if iVCPId = *blank;
037700170426         oVCPEsito=-2;
037800170426         return;
037900170426       endif;
038000170426
038100170426       // monitorizzo l'intera attività
038200170426       monitor;
038300170426
038400170426         // controllo che esista già il contatore da annullare
038500170426         chain  (iVCPTPID:iVCPID)  TIVCP000;
038600170426
038700170426         // se NON trovato (anche annullato) esco con errore
038800170426         if not %found() or
038900170426           %found() and VCPAnn = 'A';
039000170426           oVCPEsito=-2;
039100170426           return;
039200170426         endif;
039300170426
039400170426         // annullo logicamente
039500170426         VCPAnn = 'A';
039600170426
039700170426         // aggiorno
039800170426         update TIVCP000;
039900170426
040000170426       on-error;
040100170426         // se c'è un errore restiruisco esito in errore
040200170426         oVCPEsito=-1;
040300170426
040400170426       endmon;
040500170426
040600170426       unlock(e) TIVCP01I;
040700170426       return;
040800170426
040900170426     p UBVCP0R_ANN...
041000170426     p                 E
041100170426
041200170426       //--------------------------------------------------------------
041300170426       // Procedure name: UBVCP0R_CTL                                 ??
041400170426       // Purpose:        Controlla esistenza e conformità del contatore
041500170426       //--------------------------------------------------------------
041600170426
041700170426     p UBVCP0R_CTL...
041800170426     p                 B                   export
041900170426
042000170426      /copy gaitrasrc/srcProtoPR,ubvcp0r_04
042100170426
042200170426      /copy gaitrasrc/srcProtoPI,ubvcp0r_04
042300170426
042400170426       //--------------------------------------------------------------
042500170426       // Definizione variabili locali.                                ?
042600170426       //--------------------------------------------------------------
042700170524       // progressivo da controllare
042800170524     d wPrg            s                   like(VCPPrg)
042900170524       // valore massimo
043000170524     d wTo             s                   like(VCPTo)
043100170426
043200170426
043300170426       // inizializzazione
043400170426       clear oVCPEsito;
043500170426
043600170426       // controllo parametri in ingresso
043700170426       if iVCPId = *blank;
043800170426         oVCPEsito=-2;
043900170426         return;
044000170426       endif;
044100170426
044200170426       // monitorizzo l'intera attività
044300170426       monitor;
044400170426
044500170426         // leggo il contatore richiesto NON allocandolo
044600170426         chain(n)  (iVCPTPID:iVCPID)  TIVCP000;
044700170426
044800170426         // se NON trovato esco con errore specifico
044900170426         if not %found();
045000170426           oVCPEsito=-3;
045100170426           return;
045200170426         endif;
045300170426
045400170426         // se trovato ma l'anno non è quello richiesto, esco con errore come se non esistesse
045500170426         if iVCPAnno>0
045600170426         and VCPAnno <> iVCPAnno;
045700170426           oVCPEsito=-3;
045800170426           return;
045900170426         endif;
046000170426
046100170426         // se trovato ma annullato esco con errore specifico
046200170426         if VCPAnn = 'A';
046300170426           oVCPEsito=-4;
046400170426           return;
046500170426         endif;
046600170427
046700170427         if VCPTo > 0;
046800170427           wTo = VCPTo;
046900170427         // altrimenti è il valore massimo contenibile nel campo
047000170427         else;
047100170427           //    123456789012345 (15 cifre)
047200170427           //    999999999999999
047300170427           wTo = *hival;
047400170427         endif;
047500170426
047600170524         // se il progressivo da controllare è stato passato
047700170524         if %parms >= 5;
047800170524           wPrg = iVCPPrg;
047900170524         else;
048000170524           wPRG = VCPPrg;
048100170524         endif;
048200170524
048300170524         // se il progressivo non è corente coi limiti, esco con errore specifico
048400170524         if wPrg > 0
048500170524         and (wPrg < VCPFrom or
048600170524              wPrg > WTO     );
048700170426           oVCPEsito=-5;
048800170426           return;
048900170426         endif;
049000170426
049100170426       on-error;
049200170426         // se c'è un errore restiruisco esito in errore
049300170426         oVCPEsito=-1;
049400170426
049500170426       endmon;
049600170426
049700170426       return;
049800170426
049900170426     p UBVCP0R_CTL...
050000170426     p                 E
050100161124
