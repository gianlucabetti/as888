000001160112      //************************************************************************
000002160112      //
000003160112      // Questo programma sostituisce l'omonimo programma di Proj che funziona
000004160112      // non perfettamente (p.e. cercando LORENZO DEL BENE trova niente).
000005160112      //
000006160114      // Il programma restituisce massimo 9999 soggetti con la seguente priorità:
000008160114      //  1 La descrizione è uguale alla stringa cercata.
000009160114      //  2 La descrizione inizia con la stringa cercata.
000010160114      //  3 La descrizione contiene la stringa cercata.
000011160114      //
000012160114      // Esempio: stringa cercata "FAVA", il programma restituisce
000013160114      //  1 "FAVA"
000014160114      //  2 "FAVARO" "FAVALLI"
000015160114      //  3 "BOIFAVA" "BUTTAFAVA"
000016160114      //
000017160112      //************************************************************************
000018160112
000100160113       CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER)
000101160113               OPTION(*NOUNREF:*NODEBUGIO:*SRCSTMT);
000102160112
000103160112      //************************************************************************
000104160112      //
000105160112      // Costanti.
000106160112      //
000107160112      //************************************************************************
000108160111
000109160112       DCL-C UNO_SPAZIO ' ';
000110160112       DCL-C DUE_SPAZI '  ';
000111160112       DCL-C UN_APICE '''';
000112160112       DCL-C DUE_APICI '''''';
000113160114       DCL-C UN_PUNTO '.';
000114160112       DCL-C SI 1;
000115160112       DCL-C NO_DATI 1;
000116160112       DCL-C RICERCA_DESCRIZIONE '1';
000117160112
000118160112      //************************************************************************
000119160112      //
000120160112      // Campi.
000121160112      //
000122160112      //************************************************************************
000123160112
000124160112       DCL-S primaVolta PACKED(1:0);
000125160112       DCL-S tpRicerca CHAR(1);
000126160112       DCL-S descr CHAR(50);
000127160112       DCL-S risultato PACKED(1:0);
000128160112       DCL-S lenOut BINDEC(9:0);
000129160112       DCL-S output CHAR(4000);
000130160112       DCL-S lenOut1 BINDEC(9:0);
000131160112       DCL-S output1 CHAR(4000);
000132160112       DCL-S flgSog CHAR(1);
000133160112
000134160112      //************************************************************************
000135160112      //
000136160112      // Strutture.
000137160112      //
000138160112      //************************************************************************
000139160111
000140160112       DCL-DS anind00f EXT QUALIFIED END-DS;
000141160113       DCL-DS ansog00f EXT QUALIFIED END-DS;
000145160112
000155160112       DCL-DS this QUALIFIED INZ;
000158160112         descr VARCHAR(50);
000159160112         like1 VARCHAR(52);
000160160114         like2 VARCHAR(52);
000161160114         like3 VARCHAR(54);
000162160113         p INT(10:0);
000167160113         where1 VARCHAR(2000) INZ('WHERE');
000168160114         where2 VARCHAR(2000) INZ('WHERE');
000169160114         where3 VARCHAR(2000) INZ('WHERE');
000170160112         stmSql VARCHAR(32000);
000174160112       END-DS;
000175160112
000176160112      //************************************************************************
000177160112      //
000178160112      // Parametri.
000179160112      //
000180160112      //************************************************************************
000181160111
000182160111     C     *ENTRY        PLIST
000183160111     C                   PARM                    primaVolta
000184160111     C                   PARM                    tpRicerca
000185160111     C                   PARM                    descr
000186160111     C                   PARM                    risultato
000187160111     C                   PARM                    lenOut
000188160111     C                   PARM                    output
000189160111     C                   PARM                    lenOut1
000190160111     C                   PARM                    output1
000191160111     C                   PARM                    flgSog
000192160112
000193160112      //************************************************************************
000194160112      //
000195160112      // Procedura principale.
000196160112      //
000197160112      //************************************************************************
000198160111
000199160111      /FREE
000200160111
000201160112       CLEAR risultato;
000202160111
000203160112       // Prima chiamata: preparo l'istruzione SQL di ricerca.
000204160112
000205160112       IF primaVolta = SI;
000206160112
000207160112         // Questo programma esegue la ricerca solo sulla descrizione del soggetto.
000208160111
000209160112         IF descr = *BLANK OR tpRicerca <> RICERCA_DESCRIZIONE;
000210160112           risultato = NO_DATI;
000211160113           EXSR Uscita;
000212160111         ENDIF;
000213160111
000214160111         RESET this;
000215160111         EXEC SQL CLOSE soggetti;
000216160113         EXEC SQL DELETE FROM qtemp/ySoggettiAna908;
000218160111
000219160112         // Tolgo gli eventuali spazi iniziali e finali,
000220160113         // cioè " LORENZO  DEL  BENE " diventa "LORENZO  DEL  BENE".
000221160111
000222160111         this.descr = %TRIM(descr);
000223160111
000224160112         // Tolgo gli eventuali spazi multipli,
000225160113         // cioè "LORENZO  DEL  BENE" diventa "LORENZO DEL BENE".
000226160111
000227160111         DOU this.p = *ZERO;
000228160112           this.p = %SCAN(DUE_SPAZI : this.descr);
000229160111           IF this.p > *ZERO;
000230160112             this.descr = %SCANRPL( DUE_SPAZI : UNO_SPAZIO
000231160112                                  : this.descr : this.p);
000232160111           ENDIF;
000233160111         ENDDO;
000234160112
000235160112         // Raddoppio l'apice altrimenti SQL fallisce.
000236160113         // Per esempio, "D'ANTONI" diventa "D''ANTONI".
000237160112
000238160112         this.descr = %SCANRPL(UN_APICE : DUE_APICI : this.descr);
000239160113
000240160113         // Tolgo i punti, cioè "S.P.A." diventa "SPA".
000241160113
000242160114         this.descr = %SCANRPL(UN_PUNTO : '' : this.descr);
000247160111
000248160113         // A questo punto ho una descrizione così "LORENZO DEL BENE".
000249160111         // Costruisco l'istruzione SQL
000250160113         // SELECT * FROM ANSOG00F
000251160111         //  WHERE SOGDES LIKE '%LORENZO%'
000252160111         //    AND SOGDES LIKE '%DEL%'
000253160111         //    AND SOGDES LIKE '%BENE%'
000254160111
000255160111         DOU this.p = *ZERO;
000256160111
000257160112           this.p = %SCAN(UNO_SPAZIO : this.descr);
000258160111
000259160111           IF this.p = *ZERO;
000260160112             this.like1 = this.descr;
000261160114             this.like2 = this.descr;
000262160114             this.like3 = this.descr;
000263160111           ELSE;
000264160112             this.like1 = %SUBST(this.descr : 1 : this.p - 1);
000265160114             this.like2 = %SUBST(this.descr : 1 : this.p - 1);
000266160114             this.like3 = %SUBST(this.descr : 1 : this.p - 1);
000267160111             this.descr = %SUBST(this.descr : this.p + 1);
000268160111           ENDIF;
000269160112
000270160114           // Imposto la stringa di ricerca precisa "è FAVA".
000271160114
000272160114           this.like1 = '% ' + this.like1 + ' %';
000273160112
000274160112           IF this.where1 <> 'WHERE';
000275160112             this.where1 += ' AND';
000276160112           ENDIF;
000277160112
000278160113           this.where1 += ' '' '' CONCAT REPLACE(descrizione_soggetto, ''.'', +
000279160114                          '''') CONCAT '' '' LIKE ''' + this.like1 + '''';
000280160114
000281160114           // Imposto la stringa di ricerca "inizia con FAVA".
000282160114
000283160114           this.like2 = '% ' + this.like2 + '%';
000284160114
000285160114           IF this.where2 <> 'WHERE';
000286160114             this.where2 += ' AND';
000287160114           ENDIF;
000288160114
000289160114           this.where2 += ' '' '' CONCAT REPLACE(descrizione_soggetto, ''.'', +
000290160114                          '''') LIKE ''' + this.like2 + '''';
000291160112
000292160114           // Imposto la stringa di ricerca "contiene FAVA".
000293160114
000294160114           this.like3 = '%' + this.like3 + '%';
000295160112
000296160114           IF this.where3 <> 'WHERE';
000297160114             this.where3 += ' AND';
000298160112           ENDIF;
000299160112
000300160114           this.where3 += ' REPLACE(descrizione_soggetto, ''.'', '''') +
000301160114                           LIKE ''' + this.like3 + '''';
000302160111
000303160111         ENDDO;
000304160111
000305160113         EXEC SQL DELETE FROM qtemp/ySoggettiAna908;
000306160113
000307160113         // Estrazione di tutti i soggetti la cui descrizione contiene le
000308160114         // strignhe di ricerca. Alle righe estratte imposto priorità 3.
000309160113
000310160113         this.stmSql = 'INSERT INTO qtemp/ySoggettiAna908 +
000311160114                        SELECT 3 AS pty, sogsogg, descrizione_soggetto +
000312160114                        FROM ansog99v ' + this.where3
000313160113                     + ' FETCH FIRST 9999 ROWS ONLY'
000314160113         ;
000315160113
000316160113         EXEC SQL EXECUTE IMMEDIATE :this.stmSql;
000317160113
000318160113         SELECT;
000319160113           WHEN sqlCode < *ZERO;
000320160113             DUMP(A);
000321160113             risultato = NO_DATI;
000322160113             EXSR Uscita;
000323160113           WHEN sqlErrD(3) = *ZERO; // Estratti zero soggetti.
000324160113             risultato = NO_DATI;
000325160113             EXSR Uscita;
000326160113         ENDSL;
000341160113
000342160114         // Imposto priorità 2 ai soggetti estratti la cui descrizione inizia
000343160114         // con la stringa di ricerca , così li visualizzerò
000344160114         // per secondi. Per esempio, se ho cercato "FAVA" ed ho estratto due soggetti
000345160114         // "FAVARO" e "COTTAFAVA", a "FAVARO" assegno priorità 2, a "COTTAFAVA" lascio 3.
000346160113
000347160114         this.stmSql = 'UPDATE qtemp/ySoggettiAna908 SET pty = 2 '
000348160114                     + this.where2
000349160113         ;
000350160113
000351160113         EXEC SQL EXECUTE IMMEDIATE :this.stmSql;
000352160113
000353160113         IF sqlCode < *ZERO;
000354160113           DUMP(A);
000355160113           risultato = NO_DATI;
000356160113           EXSR Uscita;
000357160113         ENDIF;
000358160114
000359160114         // Imposto priorità 1 ai soggetti estratti la cui descrizione è uguale
000360160114         // alla stringa di ricerca , così li visualizzerò
000361160114         // per primi. Per esempio, se ho cercato "FAVA" ed ho estratto due soggetti
000362160114         // "FAVA" e "FAVARO", a "FAVA" assegno priorità 1, a "FAVARO" lascio 3.
000363160114
000364160114         this.stmSql = 'UPDATE qtemp/ySoggettiAna908 SET pty = 1 '
000365160114                     + this.where1 + ' AND pty = 2'
000366160114         ;
000367160114
000368160114         EXEC SQL EXECUTE IMMEDIATE :this.stmSql;
000369160114
000370160114         IF sqlCode < *ZERO;
000371160114           DUMP(A);
000372160114           risultato = NO_DATI;
000373160114           EXSR Uscita;
000374160114         ENDIF;
000375160113
000376160114         // E' giunto il momento di leggere il risultato.
000377160114
000378160113         EXEC SQL
000379160113           DECLARE soggetti NO SCROLL CURSOR FOR
000380160113             SELECT ansog00f.*, anind00f.*
000381160113               FROM qtemp/ySoggettiAna908 AS soggetti
000382160113               JOIN ansog00f ON soggetti.sogg = ansog00f.sogSogg
000383160113               JOIN anind00f ON soggetti.sogg = anind00f.indSogg
000384160113                            AND '' = anind00f.indTpInd
000385160113                            AND '' = anind00f.indCdInd
000386160113           ORDER BY pty, descrizione_soggetto
000387160113           FOR FETCH ONLY
000388160113         ;
000389160113
000390160111         EXEC SQL OPEN soggetti;
000391160111
000392160111         IF sqlCode < *ZERO;
000393160111           DUMP(A);
000394160112           risultato = NO_DATI;
000395160113           EXSR Uscita;
000396160111         ENDIF;
000397160111
000398160111       ENDIF;
000399160111
000400160113       EXEC SQL
000401160113         FETCH NEXT FROM soggetti
000402160113          INTO :ansog00f, :anind00f
000403160113       ;
000404160111
000405160111       SELECT;
000406160111         WHEN sqlCode = 100;
000407160112           risultato = NO_DATI;
000408160113           EXSR Uscita;
000409160111         WHEN sqlCode < *ZERO;
000410160111           DUMP(A);
000411160112           risultato = NO_DATI;
000412160113           EXSR Uscita;
000413160111       ENDSL;
000414160113
000415160113       %SUBST(output : 1 : lenOut) = ansog00f;
000416160113       %SUBST(output1 : 1 : lenOut1) = anind00f;
000417160111
000418160113       EXSR Uscita;
000419160112
000420160112      //************************************************************************
000421160112      //
000422160112      // Inizializzazioni.
000423160112      //
000424160112      //************************************************************************
000425160111
000426160112       BEGSR *INZSR;
000427160112
000428160113         EXEC SQL
000429160113           CREATE TABLE qtemp/ySoggettiAna908
000430160113             ( pty SMALLINT NOT NULL WITH DEFAULT
000431160113             , sogg CHAR (8 ) NOT NULL WITH DEFAULT
000432160113             , descrizione_soggetto VARCHAR (88 ) NOT NULL WITH DEFAULT )
000433160113         ;
000434160113
000435160112       ENDSR;
000436160113
000437160113      //************************************************************************
000438160113      //
000439160113      // Operazioni finali.
000440160113      //
000441160113      //************************************************************************
000442160113
000443160113       BEGSR Uscita;
000444160113
000445160113         IF risultato = NO_DATI;
000446160113           *INLR = *ON;
000447160113           EXEC SQL CLOSE soggetti;
000448160113           EXEC SQL DROP TABLE qtemp/ySoggettiAna908;
000449160113         ENDIF;
000450160113
000451160113         RETURN;
000452160113
000453160113       ENDSR;
000454160112
000455160111      /END-FREE
