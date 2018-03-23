000100110617      *---------------------------------------------------------------*
000200130308      * Trattamento DDT PDF pre-stampa                                *
000300110617      *---------------------------------------------------------------*
000400110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500130308     h dftactgrp(*no) actgrp(*caller)
000600110617
000700110617      *---------------------------------------------------------------*
000800141027     ffiar803l  if   e           k disk    infds(infAR803)
000900141027     f                                     rename(fiar8000:fiar8030)
001000141027     ffiar800f  uf   e             disk
001100130315     ffiar501L  uf   e           k disk
001200130319     ffnblp01L  if   e           k disk
001300110617
001400130309     D FNLSPDFC1       PR
001500130309     D                                     extpgm('FNLSPDFC1')
001600130309     D iPathOrig                    120A
001700130309     D iPathCli                            like(§AR8PPDFP)
001800130426     D iNomePDF                      61A
001900130318     D iFileName                    256A
002000130309     D iPathWork                    120A
002100130309     D iPDFresol                      1A
002200130309     D iPDForient                    10A
002300130309     D iPDFdelete                     4A
002400130309     D iEsi001                        1A
002500130318
002600130318     D FNLSPDFR1       PR
002700130318     D                                     extpgm('FNLSPDFR1')
002800130318     D i2PathOrig                   120A
002900130318     D i2PathCli                           like(§AR8PPDFP)
003000130426     D i2NomePDF                     61A
003100130416     D iCheckOnly                     1A
003200130318     D i2FileName                   256A
003300130318     D i2Esi001                       1A
003400130309
003500130309     D PDF100R         PR
003600130309     D                                     extpgm('PDF100R')
003700130309     D iTBKEY                              like(TBKEY)
003800130309     D iTBELE                              like(TBELE)
003900130416     D iPGM                          10A
004000130309     D iTBDAT                              like(TBDAT)
004100130416     D iEsi001                        1A
004200130309
004300130309
004400130309     D pPathOrig       s            120A
004500130309     D pPathCli        s                   like(§AR8PPDFP)
004600130426     D pNomePDF        s             61A
004700130318     D pFileName       s            256A
004800130416     D pCheckOnly      s              1A
004900130309     D pPathWork       s            120A
005000130309     D pPDFresol       s              1A
005100130309     D pPDForient      s             10A
005200130309     D pPDFdelete      s              4A
005300130309     D pEsito          s              1A
005400130309     D pTBKEY          s                   like(TBKEY)
005500130309     D pTBELE          s                   like(TBELE)
005600130416     D pPGM            s             10A
005700130309     D pTBDAT          s                   like(TBDAT)
005800130309
005900130309
006000130309     D dar8§pdf      e ds
006100130315     D dar5gen       e ds
006200130309     D pdftabel      e ds
006300130321     D wDivid          s                   like(divid)
006400130309     D wEsito          s              1A
006500120619
006600141027     D infAR803        DS
006700141027     D   ar803_rrn           397    400I 0
006800141027
006900110617
007000110627     C     *ENTRY        PLIST
007100110627     C                   PARM                    chiudi            1
007200130321     C                   PARM                    divis             2 0
007300130321     C                   PARM                    divid             2 0
007400130309
007500110617      /free
007600130309
007700130309       // setto esito a OK
007800130309       wEsito = '0';
007900130308
008000130309       // inizializzo variabili di work e passaggi parametri
008100130309       pPathOrig = *blanks;
008200130309       pPathCli = *blanks;
008300130309       pNomePDF = *blanks;
008400130309       pPathWork = *blanks;
008500130309       pPDFresol = 'S';
008600130309       pPDForient = '*AUTO';
008700130309       pPDFdelete = '*YES';
008800130309       pEsito = *blanks;
008900130321
009000130321       // procedo con l'elaborazione
009100130308       exsr sr_elabora;
009200130401
009300130401       // chiusura *pgm
009400110627       if chiudi='S';
009500130309          eval *inlr = *on;
009600110627       else;
009700130309          eval *inrt = *on;
009800110627       endif;
009900130308
010000130308
010100110617       //-------------------------------------------------------------*
010200130308       //Lettura fiar803l record "§PDF" da trattare                   *
010300110617       //-------------------------------------------------------------*
010400130308       Begsr SR_elabora;
010500130401
010600130401         // carico le tabelle di procedura
010700130401         exsr sr_cartab;
010800130401
010900130401         // se in precedenza tutto ok => procedo
011000130401         if wEsito = '0';
011100130401
011200130308           setll (*loval) fiar803l;
011300141027           read(e)  fiar803l;
011400110617
011500131212           // se tutto OK proseguo elaborazione
011600131212           if not %error;
011700131212
011800130309           // leggo TUTTO il fiar803l
011900130308           dow not %eof(fiar803l);
012000130309
012100130308             // controllo se è stata richiesta la chiusura del sottosistema
012200130308             if %shtdn;
012300130309                chiudi = 'S';
012400130308                leavesr;
012500130308             endif;
012600130401
012700130401             // Se richiesta elaborazione parallela gestisco
012800130401             exsr sr_elapar;
012900130329
013000130329             // se OK record da elaborare => procedo
013100130329             if *in40;
013200130401
013300130401                // ridefinisco i dati relativi al PDF
013400130401                dar8§pdf = ar8UNI;
013500130401
013600130401                // effettuo controlli sul record corrente
013700130401                exsr sr_chkrec;
013800130401
013900130401                // se OK record da elaborare => procedo
014000130401                if *in40;
014100130309
014200130315                   // effettuo il trattamento PDF pre-stampa
014300130401                   exsr sr_trattaPDF;
014400130309
014500130315                   // se tutto ok marco il PDF corrente come "pronto per stampa"
014600130315                   select;
014700130315                     when pEsito = '0';            // trattamento PDF OK
014800130315                          §AR8PTRT = 'S';
014900130401                          // aggiorno flag su AR5-GEN che indica PDF ok
015000130315                          chain (ar8AAS:ar8LNP:ar8NRS:ar8NSP:'GEN') fiar501l;
015100130315                          if  %found(fiar501l);
015200130315                              clear dar5gen;
015300130315                              dar5gen = ar5UNI;
015400130315                              §AR5ALS = 'S';
015500130315                              ar5UNI  = dar5gen;
015600130315                              update fiar5000;
015700130315                          endif;
015800130401
015900130315                     when pEsito = '2';            // PDF non trovato => ritento
016000130315                          §AR8PTRT = *blanks;
016100130401
016200130315                     when pEsito = '3';            // trattamento PDF KO
016300130315                          §AR8PTRT = 'E';
016400130315                   endsl;
016500130401                endif;
016600130315                ar8UNI = dar8§pdf;
016700141027                update    fiar8000;
016800130309             endif;
016900130309
017000130309             // quindi prosegui con la lettura
017100141027             read(e)  fiar803l;
017200130329
017300130308           enddo;
017400131212
017500131212           endif;
017600130321
017700130308         endif;
017800130308
017900110617        endsr;
018000130401
018100130401
018200130401       //-------------------------------------------------------------*
018300130401       //gestione elaborazione parallela                              *
018400130401       //-------------------------------------------------------------*
018500130401       Begsr SR_elapar;
018600130401
018700130401           *in40 = *off;                  // record KO
018800130401           if divis > *zeros AND divid > *zeros;
018900130401              wDivid = divid - 1;
019000130401              if %rem(ar8NSP:divis) = wDivid;
019100130401                 *in40 = *on;             // record OK
019200141027                 chain ar803_rrn fiar800f;
019300130401              else;
019400141027                 // unlock(e) fiar803l;      // record KO
019500130401              endif;
019600130401           else;
019700130401              *in40 = *on;                // record OK
019800130401           endif;
019900130401
020000130401       endsr;
020100130401
020200130401
020300130401       //-------------------------------------------------------------*
020400130401       //controlli sul record corrente                                *
020500130401       //-------------------------------------------------------------*
020600130401       Begsr SR_chkrec;
020700130401
020800130401           *in40 = *off;                  // record KO
020900130401
021000130401           // ridondante ma x "sicurezza" verifico se record da elaborare
021100130401           if ar8TMD = '§PDF' and ar8TRT = *blanks and
021200130401              §AR8PPDFP <> *blanks and §AR8PPDFN <> *blanks;
021300130401
021400130401              // verifico se la bolla non è ancora consegnata o cmq se
021500131211              // non più vecchia di 7 gg. rispetto a data borderau
021600130401              chain (ar8AAS:ar8LNP:ar8NRS:ar8NSP) fnblp01l;
021700130401              if  %found(fnblp01l) and blpDCM = *zeros  and
021800131211                  (blpDBR > %dec((%date()-%days(7)) : *ISO) OR
021900130401                   blpDBR = *zeros);
022000130401
022100130401                  *in40 = *on;            // record OK
022200130401              else;
022300130426                  §AR8PTRT = 'M';         // PDF mai affidato
022400130401              endif;
022500130401
022600130401           else;
022700130401              §AR8PTRT = 'E';             // record KO
022800130401           endif;
022900130401
023000130401       endsr;
023100130401
023200130401
023300110617       //-------------------------------------------------------------*
023400130308       //Richiamo driver trattamento PDF pre-stampa                   *
023500110617       //-------------------------------------------------------------*
023600130308        Begsr sr_TrattaPDF;
023700130309
023800130318             pPathCli  = §AR8PPDFP;
023900130318             pNomePDF  = §AR8PPDFN;
024000130401             pFileName = *blanks;
024100130401             pEsito    = '0';
024200130318
024300130401             // Se necessario verifico presenza e univocità del file da trattare
024400130401             if %scan('*':pNomePDF) > *zeros;
024500130401                exsr sr_findPDF;
024600130401             endif;
024700130401
024800130318
024900130318             // Se tutto OK procedo ed elaboro il file
025000130318             if pEsito = '0';
025100130322
025200130322                // Imposto il nome "finale su IFS" da dare al PDF
025300130322                if pFileName = *blanks;
025400130322                   pFileName = pNomePDF;
025500130322                endif;
025600130322
025700130322                // Effettuo il "pre-trattamento" PDF ai fini stampa LDV
025800130318                callP(e) FNLSPDFC1 (pPathOrig  :
025900130318                                    pPathCli   :
026000130318                                    pNomePDF   :
026100130318                                    pFileName  :
026200130318                                    pPathWork  :
026300130318                                    pPDFresol  :
026400130318                                    pPDForient :
026500130318                                    pPDFdelete :
026600130318                                    pEsito);
026700130318                if %error;
026800130318                   pEsito = '3';
026900130318                endif;
027000130318             endif;
027100130308
027200110617        endsr;
027300130401
027400130401
027500130401       //-------------------------------------------------------------*
027600130401       //Richiamo driver "trova PDF"                                  *
027700130401       //-------------------------------------------------------------*
027800130401        Begsr sr_findPDF;
027900130401
028000130416            pFileName  = *blanks;
028100130416            pEsito     = '0';
028200130416            pCheckOnly = *blanks;
028300130401
028400130401            callP(e) FNLSPDFR1 (pPathOrig  :
028500130401                                pPathCli   :
028600130401                                pNomePDF   :
028700130416                                pCheckOnly :
028800130401                                pFileName  :
028900130401                                pEsito);
029000130401
029100130401            // Verifico esito
029200130401            if %error;
029300130401               pEsito = '2';
029400130401            endif;
029500130401
029600130401            if pEsito = '0';
029700130401               // Se chiamata OK => elimino dal nome PDF i caratteri "jolly"
029800130401               dow %scan('*':pNomePDF:1) > *zeros;
029900130401                   pNomePDF = %replace('':pNomePDF:%scan('*':pNomePDF:1):1);
030000130401               enddo;
030100130401               pNomePDF = %trim(pNomePDF);
030200130401            endif;
030300130401
030400130401        endsr;
030500130321
030600130321
030700130321       //-------------------------------------------------------------*
030800130321       //caricamento valori tabellati                                 *
030900130321       //-------------------------------------------------------------*
031000130321       Begsr SR_cartab;
031100130321
031200130321         // Reperisco i parametri di procedura da PDFTABEL
031300130321         pTBKEY = 'BRT';
031400130321         pTBELE = 'PATHORIGINE';
031500130416         pPGM   = 'FNLSPDFR';
031600130321         pTBDAT = *blanks;
031700130321         callP(e) PDF100R (pTBKEY :
031800130321                           pTBELE :
031900130416                           pPGM   :
032000130321                           pTBDAT :
032100130321                           pEsito);
032200130321         if %error or pEsito <> '0';
032300130321            wEsito = '1';
032400130321         else;
032500130321            pPathOrig = %trim(pTBDAT);
032600130321         endif;
032700130321
032800130321         pTBKEY = 'BRT';
032900130321         pTBELE = 'PATHWORK';
033000130416         pPGM   = 'FNLSPDFR';
033100130321         pTBDAT = *blanks;
033200130321         callP(e) PDF100R (pTBKEY :
033300130321                           pTBELE :
033400130416                           pPGM   :
033500130321                           pTBDAT :
033600130321                           pEsito);
033700130321         if %error or pEsito <> '0';
033800130321            wEsito = '1';
033900130321         else;
034000130321            pPathWork = %trim(pTBDAT);
034100130321         endif;
034200130321
034300130321       endsr;
