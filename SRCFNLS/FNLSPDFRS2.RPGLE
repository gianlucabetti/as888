000100110617      *---------------------------------------------------------------*
000200130308      * Trattamento DDT PDF pre-stampa                                *
000300110617      *---------------------------------------------------------------*
000400110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500130308     h dftactgrp(*no) actgrp(*caller)
000600110617
000700110617      *---------------------------------------------------------------*
000800130308     ffiar803l  uf   e           k disk
000900130315     ffiar501L  uf   e           k disk
001000130319     ffnblp01L  if   e           k disk
001100110617
001200130309     D FNLSPDFC1       PR
001300130309     D                                     extpgm('FNLSPDFC1')
001400130309     D iPathOrig                    120A
001500130309     D iPathCli                            like(§AR8PPDFP)
001600130309     D iNomePDF                            like(§AR8PPDFN)
001700130318     D iFileName                    256A
001800130309     D iPathWork                    120A
001900130309     D iPDFresol                      1A
002000130309     D iPDForient                    10A
002100130309     D iPDFdelete                     4A
002200130309     D iEsi001                        1A
002300130318
002400130318     D FNLSPDFR1       PR
002500130318     D                                     extpgm('FNLSPDFR1')
002600130318     D i2PathOrig                   120A
002700130318     D i2PathCli                           like(§AR8PPDFP)
002800130318     D i2NomePDF                           like(§AR8PPDFN)
002900130416     D iCheckOnly                     1A
003000130318     D i2FileName                   256A
003100130318     D i2Esi001                       1A
003200130309
003300130309     D PDF100R         PR
003400130309     D                                     extpgm('PDF100R')
003500130309     D iTBKEY                              like(TBKEY)
003600130309     D iTBELE                              like(TBELE)
003700130416     D iPGM                          10A
003800130309     D iTBDAT                              like(TBDAT)
003900130416     D iEsi001                        1A
004000130309
004100130309
004200130309     D pPathOrig       s            120A
004300130309     D pPathCli        s                   like(§AR8PPDFP)
004400130309     D pNomePDF        s                   like(§AR8PPDFN)
004500130318     D pFileName       s            256A
004600130416     D pCheckOnly      s              1A
004700130309     D pPathWork       s            120A
004800130309     D pPDFresol       s              1A
004900130309     D pPDForient      s             10A
005000130309     D pPDFdelete      s              4A
005100130309     D pEsito          s              1A
005200130309     D pTBKEY          s                   like(TBKEY)
005300130309     D pTBELE          s                   like(TBELE)
005400130416     D pPGM            s             10A
005500130309     D pTBDAT          s                   like(TBDAT)
005600130309
005700130309
005800130309     D dar8§pdf      e ds
005900130315     D dar5gen       e ds
006000130309     D pdftabel      e ds
006100130321     D wDivid          s                   like(divid)
006200130309     D wEsito          s              1A
006300120619
006400110617
006500110617
006600110627     C     *ENTRY        PLIST
006700110627     C                   PARM                    chiudi            1
006800130321     C                   PARM                    divis             2 0
006900130321     C                   PARM                    divid             2 0
007000130309
007100110617      /free
007200130309
007300130309       // setto esito a OK
007400130309       wEsito = '0';
007500130308
007600130309       // inizializzo variabili di work e passaggi parametri
007700130309       pPathOrig = *blanks;
007800130309       pPathCli = *blanks;
007900130309       pNomePDF = *blanks;
008000130309       pPathWork = *blanks;
008100130309       pPDFresol = 'S';
008200130309       pPDForient = '*AUTO';
008300130309       pPDFdelete = '*YES';
008400130309       pEsito = *blanks;
008500130321
008600130321       // procedo con l'elaborazione
008700130308       exsr sr_elabora;
008800130401
008900130401       // chiusura *pgm
009000110627       if chiudi='S';
009100130309          eval *inlr = *on;
009200110627       else;
009300130309          eval *inrt = *on;
009400110627       endif;
009500130308
009600130308
009700110617       //-------------------------------------------------------------*
009800130308       //Lettura fiar803l record "§PDF" da trattare                   *
009900110617       //-------------------------------------------------------------*
010000130308       Begsr SR_elabora;
010100130401
010200130401         // carico le tabelle di procedura
010300130401         exsr sr_cartab;
010400130401
010500130401         // se in precedenza tutto ok => procedo
010600130401         if wEsito = '0';
010700130401
010800130308           setll (*loval) fiar803l;
010900130308           read  fiar803l;
011000110617
011100130309           // leggo TUTTO il fiar803l
011200130308           dow not %eof(fiar803l);
011300130309
011400130308             // controllo se è stata richiesta la chiusura del sottosistema
011500130308             if %shtdn;
011600130309                chiudi = 'S';
011700130308                leavesr;
011800130308             endif;
011900130401
012000130401             // Se richiesta elaborazione parallela gestisco
012100130401             exsr sr_elapar;
012200130329
012300130329             // se OK record da elaborare => procedo
012400130329             if *in40;
012500130401
012600130401                // ridefinisco i dati relativi al PDF
012700130401                dar8§pdf = ar8UNI;
012800130401
012900130401                // effettuo controlli sul record corrente
013000130401                exsr sr_chkrec;
013100130401
013200130401                // se OK record da elaborare => procedo
013300130401                if *in40;
013400130309
013500130315                   // effettuo il trattamento PDF pre-stampa
013600130401                   exsr sr_trattaPDF;
013700130309
013800130315                   // se tutto ok marco il PDF corrente come "pronto per stampa"
013900130315                   select;
014000130315                     when pEsito = '0';            // trattamento PDF OK
014100130315                          §AR8PTRT = 'S';
014200130401                          // aggiorno flag su AR5-GEN che indica PDF ok
014300130315                          chain (ar8AAS:ar8LNP:ar8NRS:ar8NSP:'GEN') fiar501l;
014400130315                          if  %found(fiar501l);
014500130315                              clear dar5gen;
014600130315                              dar5gen = ar5UNI;
014700130315                              §AR5ALS = 'S';
014800130315                              ar5UNI  = dar5gen;
014900130315                              update fiar5000;
015000130315                          endif;
015100130401
015200130315                     when pEsito = '2';            // PDF non trovato => ritento
015300130315                          §AR8PTRT = *blanks;
015400130401
015500130315                     when pEsito = '3';            // trattamento PDF KO
015600130315                          §AR8PTRT = 'E';
015700130315                   endsl;
015800130401                endif;
015900130315                ar8UNI = dar8§pdf;
016000130315                update fiar8000;
016100130309             endif;
016200130309
016300130309             // quindi prosegui con la lettura
016400130309             read  fiar803l;
016500130329
016600130308           enddo;
016700130321
016800130308         endif;
016900130308
017000110617        endsr;
017100130401
017200130401
017300130401       //-------------------------------------------------------------*
017400130401       //gestione elaborazione parallela                              *
017500130401       //-------------------------------------------------------------*
017600130401       Begsr SR_elapar;
017700130401
017800130401           *in40 = *off;                  // record KO
017900130401           if divis > *zeros AND divid > *zeros;
018000130401              wDivid = divid - 1;
018100130401              if %rem(ar8NSP:divis) = wDivid;
018200130401                 *in40 = *on;             // record OK
018300130401              else;
018400130401                 unlock(e) fiar803l;      // record KO
018500130401              endif;
018600130401           else;
018700130401              *in40 = *on;                // record OK
018800130401           endif;
018900130401
019000130401       endsr;
019100130401
019200130401
019300130401       //-------------------------------------------------------------*
019400130401       //controlli sul record corrente                                *
019500130401       //-------------------------------------------------------------*
019600130401       Begsr SR_chkrec;
019700130401
019800130401           *in40 = *off;                  // record KO
019900130401
020000130401           // ridondante ma x "sicurezza" verifico se record da elaborare
020100130401           if ar8TMD = '§PDF' and ar8TRT = *blanks and
020200130401              §AR8PPDFP <> *blanks and §AR8PPDFN <> *blanks;
020300130401
020400130401              // verifico se la bolla non è ancora consegnata o cmq se
020500130402              // non più vecchia di 5 gg. rispetto a data borderau
020600130401              chain (ar8AAS:ar8LNP:ar8NRS:ar8NSP) fnblp01l;
020700130401              if  %found(fnblp01l) and blpDCM = *zeros  and
020800130402                  (blpDBR > %dec((%date()-%days(5)) : *ISO) OR
020900130401                   blpDBR = *zeros);
021000130401
021100130401                  *in40 = *on;            // record OK
021200130401              else;
021300130401                  §AR8PTRT = 'E';         // record KO
021400130401              endif;
021500130401
021600130401           else;
021700130401              §AR8PTRT = 'E';             // record KO
021800130401           endif;
021900130401
022000130401       endsr;
022100130401
022200130401
022300110617       //-------------------------------------------------------------*
022400130308       //Richiamo driver trattamento PDF pre-stampa                   *
022500110617       //-------------------------------------------------------------*
022600130308        Begsr sr_TrattaPDF;
022700130309
022800130318             pPathCli  = §AR8PPDFP;
022900130318             pNomePDF  = §AR8PPDFN;
023000130401             pFileName = *blanks;
023100130401             pEsito    = '0';
023200130318
023300130401             // Se necessario verifico presenza e univocità del file da trattare
023400130401             if %scan('*':pNomePDF) > *zeros;
023500130401                exsr sr_findPDF;
023600130401             endif;
023700130401
023800130318
023900130318             // Se tutto OK procedo ed elaboro il file
024000130318             if pEsito = '0';
024100130322
024200130322                // Imposto il nome "finale su IFS" da dare al PDF
024300130322                if pFileName = *blanks;
024400130322                   pFileName = pNomePDF;
024500130322                endif;
024600130322
024700130322                // Effettuo il "pre-trattamento" PDF ai fini stampa LDV
024800130318                callP(e) FNLSPDFC1 (pPathOrig  :
024900130318                                    pPathCli   :
025000130318                                    pNomePDF   :
025100130318                                    pFileName  :
025200130318                                    pPathWork  :
025300130318                                    pPDFresol  :
025400130318                                    pPDForient :
025500130318                                    pPDFdelete :
025600130318                                    pEsito);
025700130318                if %error;
025800130318                   pEsito = '3';
025900130318                endif;
026000130318             endif;
026100130308
026200110617        endsr;
026300130401
026400130401
026500130401       //-------------------------------------------------------------*
026600130401       //Richiamo driver "trova PDF"                                  *
026700130401       //-------------------------------------------------------------*
026800130401        Begsr sr_findPDF;
026900130401
027000130416            pFileName  = *blanks;
027100130416            pEsito     = '0';
027200130416            pCheckOnly = *blanks;
027300130401
027400130401            callP(e) FNLSPDFR1 (pPathOrig  :
027500130401                                pPathCli   :
027600130401                                pNomePDF   :
027700130416                                pCheckOnly :
027800130401                                pFileName  :
027900130401                                pEsito);
028000130401
028100130401            // Verifico esito
028200130401            if %error;
028300130401               pEsito = '2';
028400130401            endif;
028500130401
028600130401            if pEsito = '0';
028700130401               // Se chiamata OK => elimino dal nome PDF i caratteri "jolly"
028800130401               dow %scan('*':pNomePDF:1) > *zeros;
028900130401                   pNomePDF = %replace('':pNomePDF:%scan('*':pNomePDF:1):1);
029000130401               enddo;
029100130401               pNomePDF = %trim(pNomePDF);
029200130401            endif;
029300130401
029400130401        endsr;
029500130321
029600130321
029700130321       //-------------------------------------------------------------*
029800130321       //caricamento valori tabellati                                 *
029900130321       //-------------------------------------------------------------*
030000130321       Begsr SR_cartab;
030100130321
030200130321         // Reperisco i parametri di procedura da PDFTABEL
030300130321         pTBKEY = 'BRT';
030400130321         pTBELE = 'PATHORIGINE';
030500130416         pPGM   = 'FNLSPDFR';
030600130321         pTBDAT = *blanks;
030700130321         callP(e) PDF100R (pTBKEY :
030800130321                           pTBELE :
030900130416                           pPGM   :
031000130321                           pTBDAT :
031100130321                           pEsito);
031200130321         if %error or pEsito <> '0';
031300130321            wEsito = '1';
031400130321         else;
031500130321            pPathOrig = %trim(pTBDAT);
031600130321         endif;
031700130321
031800130321         pTBKEY = 'BRT';
031900130321         pTBELE = 'PATHWORK';
032000130416         pPGM   = 'FNLSPDFR';
032100130321         pTBDAT = *blanks;
032200130321         callP(e) PDF100R (pTBKEY :
032300130321                           pTBELE :
032400130416                           pPGM   :
032500130321                           pTBDAT :
032600130321                           pEsito);
032700130321         if %error or pEsito <> '0';
032800130321            wEsito = '1';
032900130321         else;
033000130321            pPathWork = %trim(pTBDAT);
033100130321         endif;
033200130321
033300130321       endsr;
