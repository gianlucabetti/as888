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
002900130318     D i2FileName                   256A
003000130318     D i2Esi001                       1A
003100130309
003200130309     D PDF100R         PR
003300130309     D                                     extpgm('PDF100R')
003400130309     D iTBKEY                              like(TBKEY)
003500130309     D iTBELE                              like(TBELE)
003600130309     D iTBDAT                              like(TBDAT)
003700130309     D iEsi002                        1A
003800130309
003900130309
004000130309     D pPathOrig       s            120A
004100130309     D pPathCli        s                   like(§AR8PPDFP)
004200130309     D pNomePDF        s                   like(§AR8PPDFN)
004300130318     D pFileName       s            256A
004400130309     D pPathWork       s            120A
004500130309     D pPDFresol       s              1A
004600130309     D pPDForient      s             10A
004700130309     D pPDFdelete      s              4A
004800130309     D pEsito          s              1A
004900130309     D pTBKEY          s                   like(TBKEY)
005000130309     D pTBELE          s                   like(TBELE)
005100130309     D pTBDAT          s                   like(TBDAT)
005200130309
005300130309
005400130309     D dar8§pdf      e ds
005500130315     D dar5gen       e ds
005600130309     D pdftabel      e ds
005700130321     D wDivid          s                   like(divid)
005800130309     D wEsito          s              1A
005900120619
006000110617
006100110617
006200110627     C     *ENTRY        PLIST
006300110627     C                   PARM                    chiudi            1
006400130321     C                   PARM                    divis             2 0
006500130321     C                   PARM                    divid             2 0
006600130309
006700110617      /free
006800130309
006900130309       // setto esito a OK
007000130309       wEsito = '0';
007100130308
007200130309       // inizializzo variabili di work e passaggi parametri
007300130309       pPathOrig = *blanks;
007400130309       pPathCli = *blanks;
007500130309       pNomePDF = *blanks;
007600130309       pPathWork = *blanks;
007700130309       pPDFresol = 'S';
007800130309       pPDForient = '*AUTO';
007900130309       pPDFdelete = '*YES';
008000130309       pEsito = *blanks;
008100130321
008200130321       // procedo con l'elaborazione
008300130308       exsr sr_elabora;
008400110627       if chiudi='S';
008500130309          eval *inlr = *on;
008600110627       else;
008700130309          eval *inrt = *on;
008800110627       endif;
008900130308
009000130308
009100110617       //-------------------------------------------------------------*
009200130308       //Lettura fiar803l record "§PDF" da trattare                   *
009300110617       //-------------------------------------------------------------*
009400130308       Begsr SR_elabora;
009500130308
009600130309        // se in precedenza tutto ok => procedo
009700130309        if wEsito = '0';
009800130308           setll (*loval) fiar803l;
009900130308           read  fiar803l;
010000130321
010100130321           // Se richiesta elaborazione parallela gestisco
010200130321           *in40 = *off;
010300130321           if divis > *zeros AND divid > *zeros;
010400130321              wDivid = divid - 1;
010500130321              if %rem(ar8NSP:divis) = wDivid;
010600130321                 *in40 = *on;
010700130329              else;
010800130329                 unlock(e) fiar803l;
010900130321              endif;
011000130321           else;
011100130321              *in40 = *on;
011200130321           endif;
011300130321
011400130321
011500130321           // se OK record da elaborare => procedo
011600130321           if *in40;
011700130321
011800130321           // carico le tabelle di procedura
011900130321           exsr sr_cartab;
012000130321
012100130321           // se in precedenza tutto ok => procedo
012200130321           if wEsito = '0';
012300110617
012400130309           // leggo TUTTO il fiar803l
012500130308           dow not %eof(fiar803l);
012600130309
012700130308             // controllo se è stata richiesta la chiusura del sottosistema
012800130308             if %shtdn;
012900130309                chiudi = 'S';
013000130308                leavesr;
013100130308             endif;
013200130309
013300130329
013400130329             // se OK record da elaborare => procedo
013500130329             if *in40;
013600130309
013700130309             // ridondante ma x "sicurezza" verifico se record da elaborare
013800130309             if ar8TMD = '§PDF' and ar8TRT = *blanks;
013900130315
014000130315                // verifico se la bolla non è ancora consegnata o cmq se
014100130315                // non più vecchia di 30 gg. rispetto a data borderau
014200130319                chain (ar8AAS:ar8LNP:ar8NRS:ar8NSP) fnblp01l;
014300130319                if  %found(fnblp01l) and
014400130319                    blpDCM = *zeros  and
014500130319                    (blpDBR > %dec((%date()-%days(30)) : *ISO) OR
014600130319                     blpDBR = *zeros);
014700130309
014800130315                   // effettuo il trattamento PDF pre-stampa
014900130315                   dar8§pdf = ar8UNI;
015000130315                   exsr sr_TrattaPDF;
015100130309
015200130315                   // se tutto ok marco il PDF corrente come "pronto per stampa"
015300130315                   select;
015400130315                     when pEsito = '0';            // trattamento PDF OK
015500130315                          §AR8PTRT = 'S';
015600130315                          // aggiorno flag su AR5-GEN che indica PDF stampa ok
015700130315                          chain (ar8AAS:ar8LNP:ar8NRS:ar8NSP:'GEN') fiar501l;
015800130315                          if  %found(fiar501l);
015900130315                              clear dar5gen;
016000130315                              dar5gen = ar5UNI;
016100130315                              §AR5ALS = 'S';
016200130315                              ar5UNI  = dar5gen;
016300130315                              update fiar5000;
016400130315                          endif;
016500130315                     when pEsito = '2';            // PDF non trovato => ritento
016600130315                          §AR8PTRT = *blanks;
016700130315                     when pEsito = '3';            // trattamento PDF KO
016800130315                          §AR8PTRT = 'E';
016900130315                   endsl;
017000130315                else;
017100130315                   §AR8PTRT = 'E';
017200130315                endif;
017300130315                ar8UNI = dar8§pdf;
017400130315                update fiar8000;
017500130309             endif;
017600130329             endif;
017700130309
017800130309             // quindi prosegui con la lettura
017900130309             read  fiar803l;
018000130329
018100130329             // Se richiesta elaborazione parallela gestisco
018200130329             *in40 = *off;
018300130329             if divis > *zeros AND divid > *zeros;
018400130329                wDivid = divid - 1;
018500130329                if %rem(ar8NSP:divis) = wDivid;
018600130329                   *in40 = *on;
018700130329                else;
018800130329                   unlock(e) fiar803l;
018900130329                endif;
019000130329             else;
019100130329                *in40 = *on;
019200130329             endif;
019300130329
019400130308           enddo;
019500130321           endif;
019600130321           endif;
019700130321
019800130308         endif;
019900130308
020000110617        endsr;
020100130308
020200130308
020300110617       //-------------------------------------------------------------*
020400130308       //Richiamo driver trattamento PDF pre-stampa                   *
020500110617       //-------------------------------------------------------------*
020600130308        Begsr sr_TrattaPDF;
020700130309
020800130318             pPathCli  = §AR8PPDFP;
020900130318             pNomePDF  = §AR8PPDFN;
021000130322             pFileName = *blanks;
021100130318             pEsito    = '0';
021200130318
021300130318             // Se necessario verifico presenza e univocità del file da trattare
021400130321             if %scan('*':pNomePDF) > *zeros;
021500130318                callP(e) FNLSPDFR1 (pPathOrig  :
021600130318                                    pPathCli   :
021700130318                                    pNomePDF   :
021800130318                                    pFileName  :
021900130318                                    pEsito);
022000130322                // Verifico esito
022100130318                if %error;
022200130318                   pEsito = '2';
022300130322                else;
022400130322
022500130322                   // Se chiamata OK => elimino dal nome PDF i caratteri "jolly"
022600130322                   dow %scan('*':pNomePDF:1) > *zeros;
022700130322                       pNomePDF = %replace('':pNomePDF:
022800130322                                  %scan('*':pNomePDF:1):1);
022900130322                   enddo;
023000130322                   pNomePDF = %trim(pNomePDF);
023100130322
023200130318                endif;
023300130318             endif;
023400130318
023500130318
023600130318             // Se tutto OK procedo ed elaboro il file
023700130318             if pEsito = '0';
023800130322
023900130322                // Imposto il nome "finale su IFS" da dare al PDF
024000130322                if pFileName = *blanks;
024100130322                   pFileName = pNomePDF;
024200130322                endif;
024300130322
024400130322                // Effettuo il "pre-trattamento" PDF ai fini stampa LDV
024500130318                callP(e) FNLSPDFC1 (pPathOrig  :
024600130318                                    pPathCli   :
024700130318                                    pNomePDF   :
024800130318                                    pFileName  :
024900130318                                    pPathWork  :
025000130318                                    pPDFresol  :
025100130318                                    pPDForient :
025200130318                                    pPDFdelete :
025300130318                                    pEsito);
025400130318                if %error;
025500130318                   pEsito = '3';
025600130318                endif;
025700130318             endif;
025800130308
025900110617        endsr;
026000130321
026100130321
026200130321       //-------------------------------------------------------------*
026300130321       //caricamento valori tabellati                                 *
026400130321       //-------------------------------------------------------------*
026500130321       Begsr SR_cartab;
026600130321
026700130321         // Reperisco i parametri di procedura da PDFTABEL
026800130321         pTBKEY = 'BRT';
026900130321         pTBELE = 'PATHORIGINE';
027000130321         pTBDAT = *blanks;
027100130321         callP(e) PDF100R (pTBKEY :
027200130321                           pTBELE :
027300130321                           pTBDAT :
027400130321                           pEsito);
027500130321         if %error or pEsito <> '0';
027600130321            wEsito = '1';
027700130321         else;
027800130321            pPathOrig = %trim(pTBDAT);
027900130321         endif;
028000130321
028100130321         pTBKEY = 'BRT';
028200130321         pTBELE = 'PATHWORK';
028300130321         pTBDAT = *blanks;
028400130321         callP(e) PDF100R (pTBKEY :
028500130321                           pTBELE :
028600130321                           pTBDAT :
028700130321                           pEsito);
028800130321         if %error or pEsito <> '0';
028900130321            wEsito = '1';
029000130321         else;
029100130321            pPathWork = %trim(pTBDAT);
029200130321         endif;
029300130321
029400130321       endsr;
