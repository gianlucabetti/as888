000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300030611     H* TRASCODIFICA TARIFFE/OFFERTE X DIRITTO DI FATTURAZIONE
000400030611     H*-------------------------------------------------------------------------
000500030605     FCNIND00F  UF   E           K DISK
000600030605     FCNACO00F  UF   E           K DISK
000700030605     FTNTAM01L  IF   E           K DISK
000800030605     FTITPD01L  IF   E           K DISK
000900030610     FTITPD02L  UF   E           K DISK    PREFIX(U_) RENAME(titpd000:titpd02)
001000030701     FTITPT01L  UF   E           K DISK
001100030610     FTNTBE01L  IF   E           K DISK
001200030610     FTAVR15P   O    E             PRINTER OFLIND(*IN01)
001300030610     D*--------------------
001400030610     D* DS ESTERNE
001500030610     D*--------------------
001600030610     D DGEI          E DS
001700001218     D*--------------------
001800030610     D* SCAMBIO PARAMETRI
001900001218     D*--------------------
002000030611     D parPO           s              3                                         * P.O. da elaborare
002100030613     D parFILE         s              1                                         * Elab. x tar/off
002200030613     D parTIPELA       s              1                                         * Flag tipo elab.
002300030605     D*--------------------
002400030605     D* SCHIERE DI WRK X MEMORIZZAZIONE
002500030605     D*--------------------
002600030610     D skKSC           s                   dim(100) like(tamKSC)                * Codice Cliente
002700030611     D skRAGSOC        s                   dim(100) like(acoRAG)                * Rag. Soc. Cliente
002800030610     D skCTR           s                   dim(100) like(tamCTR)                * Codice Tariffa
002900030610     D skPRG           s                   dim(100) like(tamPRG)                * Progressivo Tar
003000030610     D skDDT           s                   dim(100) like(tamDDT)                * Data decorr. tar.
003100030610     D skDST           s                   dim(100) like(tamDST)                * Data scad. tar.
003200030610     D skSIN           s                   dim(100) like(indSIN)                * Spese Incasso
003300030610     D skFLGANN        s              1    dim(100)                             * Flag Annulam TP §
003400030610     D skFLGCHK        s              1    dim(100)                             * Flag Tar. da cons.
003500030610     D skFLGATT        s              1    diM(100)                             * Flag Tar. attiva
003600030610     D skIMPDF         s                   dim(100) like(indSIN)                * Importo Dir.Fatt.
003700030610     D skFLGKSCN       s              1    dim(100)                             * Flag DF "negato"
003800030610     D skFLGKSCK       s              1    dim(100)                             * Flag DF da cart.
003900030605     D*------------------
004000030605     D* DS "XSRDA8" - CONTROLLA DATA (8)
004100030605     D*------------------
004200030605     D WLBDA8          DS                  INZ
004300030605     D  G08DAT                 1      8  0
004400030605     D  G08INV                 9     16  0
004500030605     D  G08ERR                17     17
004600030605     D  G08TGI                18     22  0
004700030414     D*--------------------
004800030414     D* VARIABILI DI WRK
004900030414     D*--------------------
005000030611     D parPOn          s              3  0 inz(*zeros)
005100030605     D i               s              3  0 inz(*zeros)
005200030605     D recOK           s              1
005300030605     D wPO             s              3  0 inz(*zeros)
005400030605     D wCli            s              4  0 inz(*zeros)
005500030605     D wLEAVE          s              1
005600030611     D wIMPDF          s                   inz(*zeros) like(tpdITR)
005700030610     D wFLGATT         s              1
005800030612     D wFLGTARANN      s              1
005900030610     D savIMPDF        s                   inz(*zeros) like(indSIN)
006000030611     D depSTPKSC       s                   inz(*zeros) like(STPKSC)
006100030612     D depSTPCTR       s                   inz(*zeros) like(STPCTR)
006200001218
006300001218
006400920812     C*---------------------------------------------------------------*
006500001218     C* MAIN
006600001218     C*---------------------------------------------------------------*
006700030611     C                   EXSR      STATES                                       * testata prospetto
006800030611     C                   EXSR      PROCEDI                                      * elaborazione
006900030611     C                   EXSR      STAFIN                                       * chiusura prospetto
007000001218     C*
007100030611     C                   SETON                                        LR
007200010509     C*---------------------------------------------------------------*
007300030605
007400030605
007500030605
007600030605
007700030605     C*------------------------------------------------------------------------*
007800030605     C* PROCEDI - Routine di elaborazione principale
007900030605     C*------------------------------------------------------------------------*
008000030605     C     PROCEDI       BEGSR
008100030605     C*
008200030610     C                   EXSR      CARTAB
008300030605     C                   EXSR      REPINFOTAR
008400030605     C*
008500030605     C                   ENDSR
008600030605     C*------------------------------------------------------------------------*
008700030605
008800030605
008900030605
009000030605
009100030605     C*------------------------------------------------------------------------*
009200030605     C* REPINFORTAR - Routine di reperimento informazioni tariffe/tariffe particolari
009300030605     C*------------------------------------------------------------------------*
009400030605     C     REPINFOTAR    BEGSR
009500030605     C*
009600030605     C* Scorro il file di estensione anagrafico clienti CNIND00F
009700030923     C                   MOVEL(P)  *zeros        indKSC
009800030605     C     KEYind00C     SETLL     cnind00f
009900030605     C                   IF        %found(cnind00f)
010000030605     C                   MOVEL     '0'           wLEAVE
010100030605     C                   DOW       not %eof(cnind00f) AND
010200030605     C                             wLEAVE = '0'
010300030605     C                   READ      cnind00f
010400030605     C                   EXSR      CHKREC
010500030605     C                   IF        recOK = 'S'
010600030605     C                   EXSR      INZWRK
010700030610     C                   EXSR      REPTAR
010800030610     C                   EXSR      CHKTAR
010900030605     C                   ENDIF
011000030605     C                   ENDDO
011100030605     C                   ENDIF
011200030605     C*
011300030605     C                   ENDSR
011400030605     C*------------------------------------------------------------------------*
011500030605
011600030605
011700030605
011800030605
011900030605     C*------------------------------------------------------------------------*
012000030605     C* CHKREC - Routine di verifica record da trattare
012100030605     C*------------------------------------------------------------------------*
012200030605     C     CHKREC        BEGSR
012300030605     C*
012400030605     C* Inizializzo flag di controllo record x prosecuzoine elaborazione
012500030605     C                   MOVEL     'S'           recOK
012600030605     C*
012700030605     C* Verifico che nn si tratti di "cartello"
012800030605     C                   IF        recOK = 'S'
012900030611     C                   MOVEL     indKSC        wPO
013000030605     C                   IF        wPO = 888
013100030605     C                   MOVEL     'N'           recOK
013200030605     C                   ENDIF
013300030605     C                   ENDIF
013400030605     C*
013500030605     C                   ENDSR
013600030605     C*---------------------------------------------------------------*
013700030610
013800030610
013900030610
014000030610     C*------------------------------------------------------------------------*
014100030610     C* INZWRK - Routine di inizializzazione variabili wrk di memorizzazione
014200030610     C*------------------------------------------------------------------------*
014300030610     C     INZWRK        BEGSR
014400030610     C*
014500030611     C                   RESET                   i
014600030612     C                   CLEAR                   wFLGTARANN
014700030610     C                   CLEAR                   skKSC
014800030611     C                   CLEAR                   skRAGSOC
014900030610     C                   CLEAR                   skCTR
015000030610     C                   CLEAR                   skPRG
015100030610     C                   CLEAR                   skDDT
015200030610     C                   CLEAR                   skDST
015300030610     C                   CLEAR                   skSIN
015400030610     C                   CLEAR                   skFLGANN
015500030611     C                   CLEAR                   skFLGCHK
015600030610     C                   CLEAR                   skFLGATT
015700030610     C                   CLEAR                   skIMPDF
015800030610     C                   CLEAR                   skFLGKSCN
015900030611     C                   CLEAR                   skFLGKSCK
016000030610     C*
016100030610     C                   ENDSR
016200030610     C*------------------------------------------------------------------------*
016300001218
016400001218
016500001218
016600001218     C*------------------------------------------------------------------------*
016700030610     C* REPTAR - Routine considerazioni su tariffe
016800001218     C*------------------------------------------------------------------------*
016900030610     C     REPTAR        BEGSR
017000030611     C*
017100030611     C                   MOVE      indKSC        wCli
017200030611     C*
017300030611     C* Innanzitutto verifico se trattasi d clienti 8888 o 9999
017400030611     C                   IF        wCli = 8888 OR
017500030611     C                             wCli = 9999
017600030611     C                   EVAL      i = i + 1
017700030611     C                   EVAL      skKSC(i) = indKSC                               * Cod.  cliente
017800030611     C*
017900030611     C* Reperisco la Ragione Sociale del cliente corrente
018000030611     C     KEYaco00C     CHAIN     cnaco00f
018100030611     C                   IF        %found(cnaco00f)
018200030611     C                   EVAL      skRAGSOC(i) = acoRAG
018300030611     C                   ELSE
018400030611     C                   EVAL      skRAGSOC(i) = *all'*'
018500030611     C                   ENDIF
018600030611     C*
018700030611     C                   EVAL      skSIN(i) = indSIN                               * Spese Incasso
018800030611     C                   EVAL      skIMPDF(i) = *zeros                             * DF a zero
018900030611     C                   EVAL      skFLGCHK(i)  = 'S'                              * Tar. da cons.
019000030611     C                   EVAL      skFLGATT(i)  = 'S'                              * Tar. attiva
019100030605     C*
019200030611     C* Altrimenti scorro le tariffe del cliente corrente
019300030611     C                   ELSE
019400030605     C     KEYtam01P     SETLL     tntam01l
019500030605     C                   IF        %equal(tntam01l)
019600030605     C     KEYtam01P     READE     tntam01l
019700030605     C                   DOW       not %eof(tntam01l)
019800030611     C*
019900030611     C* Elaboro solo le tariffe nn annullate
020000030611     C                   IF        tamATB = *blanks
020100030612     C*
020200030612     C* Valorizzo subito il flag d wrk che indica che il cliente ha tariffe NON annullate
020300030612     C                   MOVEL     '1'           wFLGTARANN
020400030605     C*
020500030612     C* ...x ogni tariffa => azzreo l'importo del DF max e memorizzo
020600030612     C                   CLEAR                   wIMPDF
020700030605     C                   EVAL      i = i + 1
020800030610     C                   EVAL      skKSC(i) = tamKSC                               * Cod.  cliente
020900030611     C*
021000030611     C* Reperisco la Ragione Sociale del cliente corrente
021100030611     C     KEYaco00C     CHAIN     cnaco00f
021200030611     C                   IF        %found(cnaco00f)
021300030611     C                   EVAL      skRAGSOC(i) = acoRAG
021400030611     C                   ELSE
021500030611     C                   EVAL      skRAGSOC(i) = *all'*'
021600030611     C                   ENDIF
021700030611     C*
021800030610     C                   EVAL      skCTR(i) = tamCTR                               * Cod.  tariffa
021900030610     C                   EVAL      skPRG(i) = tamPRG                               * Prog. tariffa
022000030610     C                   EVAL      skDDT(i) = tamDDT                               * Data dec.  tar.
022100030610     C                   EVAL      skDST(i) = tamDST                               * Data scad. tar.
022200030610     C                   EVAL      skSIN(i) = indSIN                               * Spese Incasso
022300030610     C                   EVAL      skFLGCHK(i) = 'S'                               * Tar. da cons.
022400030610     C*
022500030611     C* ...dal 2° giro in poi verifico rottura di codice x tariffa; se x ugual tariffa esiste
022600030611     C* 1 progressivo maggiore => considero attivo quello e nn gli eventuali precedenti
022700030611     C                   IF        i > 1
022800030611     C                   IF        skCTR(i) = skCTR(i-1)
022900030610     C                   EVAL      skFLGATT(i-1) = 'N'                             * Tar. NN attiva
023000030610     C                   EVAL      skFLGCHK(i-1) = 'N'                             * Tar. da NN cons
023100030610     C                   ELSE
023200030610     C                   EVAL      skFLGCHK(i-1) = 'S'                             * Tar. da cons.
023300030610     C                   ENDIF
023400030611     C                   ENDIF
023500030605     C*
023600030610     C* ...verifico se trattasi di tariffa attiva
023700030610     C                   IF        tamDST >= DATCOR
023800030610     C                   EVAL      skFLGATT(i) = 'S'                               * Tar. attiva
023900030605     C                   ENDIF
024000030605     C*
024100030605     C* ...verifico le tariffe particolari con codice '§' correlate
024200030605     C     KEYtpd01P     SETLL     titpd01l
024300030605     C*
024400030605     C* ...se esiste una tariffa particolare proseguo con le considerazioni relative
024500030605     C                   IF        %equal(titpd01l)
024600030605     C*
024700030605     C* ...quindi scorro tutte le tariffe particolari x reperire il maggior diritto di fatturazione
024800030605     C     KEYtpd01P     READE     titpd01l
024900030605     C                   DOW       not %eof(titpd01l)
025000030611     C*
025100030611     C* Elaboro solo le tariffe particolari nn annullate
025200030611     C                   IF        tpdATB = *blanks
025300030611     C                   EVAL      skFLGANN(i)  = 'A'                              * TP da annullare
025400030605     C                   IF        tpdITR = *zeros
025500030610     C                   EVAL      skFLGKSCN(i) = 'S'                              * DF negato
025600030605     C                   EVAL      wIMPDF = *zeros
025700030605     C                   LEAVE
025800030605     C                   ELSE
025900030612     C                   IF        tpdITR * 1000 > wIMPDF                          * molt. per 1000
026000030612     C                   EVAL      wIMPDF = tpdITR * 1000                          * x simul. 3 dec
026100030612     C                   ENDIF                                                     * in campo INDSIN
026200030605     C                   ENDIF
026300030611     C                   ENDIF
026400030605     C     KEYtpd01P     READE     titpd01l
026500030605     C                   ENDDO
026600030611     C*
026700030611     C* Siccome il campo INDSIN è un 5,0 gestisco eventuali importi superiori => forzo 99999
026800030612     C                   IF        wIMPDF >= 100 * 1000                            * ...come sopra
026900030611     C                   EVAL      skIMPDF(i) = 99999                              * MAX DF tar.par.
027000030611     C                   ELSE
027100030610     C                   EVAL      skIMPDF(i) = wIMPDF                             * MAX DF tar.par.
027200030611     C                   ENDIF
027300030605     C*
027400030610     C* ...se nn esiste una tariffa particolare proseguo con altre considerazioni relative
027500030605     C                   ELSE
027600030611     C                   EVAL      skIMPDF(i) = §GEDRF * 1000                      * DF di cartello
027700030610     C                   EVAL      skFLGKSCK(i) = 'S'                              * DF da cartello
027800030605     C                   ENDIF
027900030611     C                   ENDIF
028000030610     C*
028100030610     C* Proseguo con la lettura delle tariffe
028200030610     C     KEYtam01P     READE     tntam01l
028300030610     C                   ENDDO
028400030612     C*
028500030612     C* Se a questo punto il cliente ha tariffe ma nn è stato memorizzato ancora nulla =>
028600030612     C* il cliente ha solo tariffe annullate x cui gestisco questo caso analogamente a come se nn
028700030612     C* avesse affatto tariffe
028800030612     C                   IF        wFLGTARANN <> '1'
028900030612     C                   EVAL      i = i + 1
029000030612     C                   EVAL      skKSC(i) = indKSC                               * Cod.  cliente
029100030612     C*
029200030612     C* Reperisco la Ragione Sociale del cliente corrente
029300030612     C     KEYaco00C     CHAIN     cnaco00f
029400030612     C                   IF        %found(cnaco00f)
029500030612     C                   EVAL      skRAGSOC(i) = acoRAG
029600030612     C                   ELSE
029700030612     C                   EVAL      skRAGSOC(i) = *all'*'
029800030612     C                   ENDIF
029900030612     C*
030000030612     C                   EVAL      skSIN(i) = indSIN                               * Spese Incasso
030100030612     C                   EVAL      skIMPDF(i) = §GEDRF * 1000                      * DF di cartello
030200030612     C                   EVAL      skFLGCHK(i)  = 'S'                              * Tar. da cons.
030300030612     C                   EVAL      skFLGATT(i)  = 'S'                              * Tar. attiva
030400030612     C                   EVAL      skFLGKSCK(i) = 'S'                              * DF da cartello
030500030612     C                   ENDIF
030600030610     C*
030700030610     C* ...se x il cliente proprio nn esistono tariffe => imposto DF di cartello
030800030610     C                   ELSE
030900030610     C                   EVAL      i = i + 1
031000030610     C                   EVAL      skKSC(i) = indKSC                               * Cod.  cliente
031100030611     C*
031200030611     C* Reperisco la Ragione Sociale del cliente corrente
031300030611     C     KEYaco00C     CHAIN     cnaco00f
031400030611     C                   IF        %found(cnaco00f)
031500030611     C                   EVAL      skRAGSOC(i) = acoRAG
031600030611     C                   ELSE
031700030611     C                   EVAL      skRAGSOC(i) = *all'*'
031800030611     C                   ENDIF
031900030611     C*
032000030610     C                   EVAL      skSIN(i) = indSIN                               * Spese Incasso
032100030611     C                   EVAL      skIMPDF(i) = §GEDRF * 1000                      * DF di cartello
032200030610     C                   EVAL      skFLGCHK(i)  = 'S'                              * Tar. da cons.
032300030610     C                   EVAL      skFLGATT(i)  = 'S'                              * Tar. attiva
032400030610     C                   EVAL      skFLGKSCK(i) = 'S'                              * DF da cartello
032500030610     C                   ENDIF
032600030611     C                   ENDIF
032700030605     C*
032800001218     C                   ENDSR
032900001218     C*------------------------------------------------------------------------*
033000030610
033100030610
033200030610
033300030610
033400030610     C*------------------------------------------------------------------------*
033500030610     C* CHKTAR - Routine di elaborazione tariffe/tariffe particolari
033600030610     C*------------------------------------------------------------------------*
033700030610     C     CHKTAR        BEGSR
033800030611     C*
033900030612     C* Innanzitutto eseguo considerazioni su DF da cartello e su tariffe scadute/valide
034000030611     C                   EXSR      CHKTARCAR
034100030612     C                   EXSR      CHKTARVAL
034200030610     C*
034300030610     C* Inizializzo l'importo del diritto di fatturazione da attribuire al cliente
034400030611     C* e indicatore di spia x aggiornamento SI/NO
034500030611     C* flag DF da cartello su ciascuna tariffa
034600030610     C                   Z-ADD     *zeros        savIMPDF
034700030611     C                   SETOFF                                       54
034800030610     C*
034900030610     C* E i flag di wrk
035000030610     C                   EVAL      wFLGATT = '0'
035100030610     C*
035200030610     C* Scorro la schiera contenete le info delle tariffe/tariffe particolari per determinare
035300030610     C* quale sia l'importo/negazione da considerare per l'attribuzione al cliente
035400030610     C                   Z-ADD     1             i
035500030610     C                   DOW       i <= %elem(skKSC)
035600030611     C*
035700030611     C* Procedo solo x gli elementi pieni
035800030611     C                   IF        skKSC(i) <> *zeros
035900030610     C*
036000030610     C* Elaboro solo tariffe da considerare
036100030611     C                   IF        skFLGCHK(i) = 'S'
036200030611     C*
036300030611     C* Accendo l'indicatore di spia x aggiornamento SI/NO
036400030611     C                   SETON                                        54
036500030610     C*
036600030610     C* Se nn ho ancora trovato 1 tariffa valida => ragiono x importo maggiore/negazione
036700030610     C                   IF        wFLGATT = '0'
036800030610     C*
036900030610     C* Se la tariffa corrente è valida
037000030610     C                   IF        skFLGATT(i) = 'S'
037100030610     C*
037200030610     C* Se la tariffa ha il DF negato => imposto ed esco
037300030610     C                   IF        skFLGKSCN(i) = 'S'
037400030610     C                   EVAL      savIMPDF = *zeros                            * nego il DF
037500030610     C                   LEAVE
037600030610     C*
037700030610     C* Altrimenti tengo il DF corrente
037800030610     C                   ELSE
037900030610     C                   EVAL      savIMPDF = skIMPDF(i)                        * DF tar. da cons.
038000030610     C                   ENDIF
038100030610     C*
038200030610     C* Se la tariffa corrente nn è valida
038300030610     C                   ELSE
038400030610     C*
038500030610     C* Se la tariffa ha il DF negato => imposto
038600030610     C                   IF        skFLGKSCN(i) = 'S'
038700030610     C                   EVAL      savIMPDF = *zeros                            * nego il DF
038800030610     C*
038900030610     C* Altrimenti tengo il DF maggiore
039000030610     C                   ELSE
039100030610     C                   IF        skIMPDF(i) > savIMPDF
039200030610     C                   EVAL      savIMPDF = skIMPDF(i)                        * tengo DF maggiore
039300030610     C                   ENDIF
039400030610     C                   ENDIF
039500030610     C                   ENDIF
039600030610     C*
039700030610     C* Se ho già trovato 1 tariffa valida => ragiono x importo maggiore/negazione solo tar. valide
039800030610     C                   ELSE
039900030610     C                   IF        skFLGATT(i) = 'S'
040000030610     C*
040100030610     C* Se la tariffa ha il DF negato => imposto ed esco
040200030610     C                   IF        skFLGKSCN(i) = 'S'
040300030610     C                   EVAL      savIMPDF = *zeros                            * nego il DF
040400030610     C                   LEAVE
040500030610     C*
040600030610     C* Altrimenti tengo il DF maggiore
040700030610     C                   ELSE
040800030610     C                   IF        skIMPDF(i) > savIMPDF
040900030610     C                   EVAL      savIMPDF = skIMPDF(i)                        * tengo DF maggiore
041000030610     C                   ENDIF
041100030610     C                   ENDIF
041200030610     C                   ENDIF
041300030610     C                   ENDIF
041400030610     C*
041500030610     C* Appena incontro 1 tariffa valida => memorizzo
041600030610     C                   IF        skFLGATT(i) = 'S'
041700030610     C                   EVAL      wFLGATT = '1'
041800030610     C                   ENDIF
041900030610     C                   ENDIF
042000030610     C*
042100030610     C* Continuo con lo scorrimento della schiera tariffe
042200030610     C                   EVAL      i = i + 1
042300030611     C                   ELSE
042400030611     C                   LEAVE
042500030611     C                   ENDIF
042600030610     C                   ENDDO
042700030610     C*
042800030610     C* A questo punto se richiesto nel lancio effettuo operazioni di aggiornamento sul cliente
042900030611     C   54              EXSR      UPDKSC
043000030610     C*
043100030610     C* Ed infine listo e se richiesto annullo/sfleggo le tariffe particolari
043200030611     C   54              EXSR      EXETAR
043300030610     C*
043400030610     C                   ENDSR
043500030610     C*---------------------------------------------------------------*
043600030611
043700030611
043800030611
043900030611
044000030611     C*------------------------------------------------------------------------*
044100030612     C* CHKTARCAR - Routine di considerazioni particolari su falg DF da cartello
044200030611     C*------------------------------------------------------------------------*
044300030611     C     CHKTARCAR     BEGSR
044400030611     C*
044500030611     C* Inizializzo indicatore d wrk
044600030611     C                   SETOFF                                       53
044700030611     C*
044800030611     C* 1° GIRO: Scorro la schiera contenete le info delle tariffe/tariffe particolari
044900030611     C*          ed effettuo considerazioni sul flag DF da cartello
045000030611     C                   Z-ADD     1             i
045100030611     C                   DOW       i <= %elem(skKSC)
045200030611     C*
045300030611     C* Procedo solo x gli elementi pieni
045400030611     C                   IF        skKSC(i) <> *zeros
045500030611     C*
045600030612     C* Se c'è almeno 1 tariffa da considerare senza il flag d DF da cartello con DF <> 0
045700030612     C* => x il cliente NON considero nessun flag d DF da cartello (ed abbandono il ciclo)
045800030612     C                   IF        skFLGKSCK(i) <> 'S' AND
045900030612     C                             skIMPDF(i)   <> *zeros
046000030611     C                   SETON                                        53
046100030611     C                   LEAVE
046200030611     C                   ENDIF
046300030611     C*
046400030611     C* Continuo con lo scorrimento della schiera tariffe
046500030611     C                   EVAL      i = i + 1
046600030611     C                   ELSE
046700030611     C                   LEAVE
046800030611     C                   ENDIF
046900030611     C                   ENDDO
047000030611     C*
047100030611     C* 2° GIRO: Se indicatore d wrk accesso => annullo tutti i flag DF da cartello
047200030611     C*          precedentemente impostati (e azzero l'importo del DF)
047300030611     C                   IF        *IN53 = *ON
047400030611     C                   Z-ADD     1             i
047500030611     C                   DOW       i <= %elem(skKSC)
047600030611     C*
047700030611     C* Procedo solo x gli elementi pieni
047800030611     C                   IF        skKSC(i) <> *zeros
047900030611     C*
048000030611     C* Se valorizzato a 'S' il flag d DF da cartello => imposto a 'N' ed azzero l'importo e
048100030611     C* imposto il flag d NON considerazione tariffa
048200030611     C                   IF        skFLGKSCK(i) = 'S'
048300030611     C                   EVAL      skFLGKSCK(i) = 'N'
048400030611     C                   EVAL      skFLGCHK(i)  = 'N'
048500030611     C                   EVAL      skIMPDF(i)   = *zeros
048600030611     C                   ENDIF
048700030611     C*
048800030611     C* Continuo con lo scorrimento della schiera tariffe
048900030611     C                   EVAL      i = i + 1
049000030611     C                   ELSE
049100030611     C                   LEAVE
049200030611     C                   ENDIF
049300030611     C                   ENDDO
049400030611     C                   ENDIF
049500030611     C*
049600030611     C                   ENDSR
049700030611     C*---------------------------------------------------------------*
049800030612
049900030612
050000030612
050100030612
050200030612     C*------------------------------------------------------------------------*
050300030612     C* CHKTARVAL - Routine di considerazioni particolari su falg tariffa scaduta/valida
050400030612     C*------------------------------------------------------------------------*
050500030612     C     CHKTARVAL     BEGSR
050600030612     C*
050700030612     C* Inizializzo indicatore d wrk
050800030612     C                   SETOFF                                       52
050900030612     C*
051000030612     C* 1° GIRO: Scorro la schiera contenete le info delle tariffe
051100030612     C*          ed effettuo considerazioni sul flag tariffe scadute/attive
051200030612     C                   Z-ADD     1             i
051300030612     C                   DOW       i <= %elem(skKSC)
051400030612     C*
051500030612     C* Procedo solo x gli elementi pieni
051600030612     C                   IF        skKSC(i) <> *zeros
051700030612     C*
051800030612     C* Se c'è almeno 1 tariffa attiva con DF <> 0 => x il cliente NON
051900030612     C* considero nessuna tariffa scaduta (ed abbandono il ciclo)
052000030612     C                   IF        skFLGATT(i) = 'S'      AND
052100030612     C                             skIMPDF(i)   <> *zeros
052200030612     C                   SETON                                        52
052300030612     C                   LEAVE
052400030612     C                   ENDIF
052500030612     C*
052600030612     C* Continuo con lo scorrimento della schiera tariffe
052700030612     C                   EVAL      i = i + 1
052800030612     C                   ELSE
052900030612     C                   LEAVE
053000030612     C                   ENDIF
053100030612     C                   ENDDO
053200030612     C*
053300030612     C* 2° GIRO: Se indicatore d wrk accesso => imposto a 0 gli importi delle tariffe scadute
053400030612     C                   IF        *IN52 = *ON
053500030612     C                   Z-ADD     1             i
053600030612     C                   DOW       i <= %elem(skKSC)
053700030612     C*
053800030612     C* Procedo solo x gli elementi pieni
053900030612     C                   IF        skKSC(i) <> *zeros
054000030612     C*
054100030612     C* Se NON valorizzato a 'S' il flag d tariffa attiva => azzero l'importo e
054200030612     C* imposto il flag d NON considerazione tariffa
054300030612     C                   IF        skFLGATT(i) <> 'S'
054400030612     C                   EVAL      skFLGCHK(i)  = 'N'
054500030612     C                   EVAL      skIMPDF(i)   = *zeros
054600030612     C                   ENDIF
054700030612     C*
054800030612     C* Continuo con lo scorrimento della schiera tariffe
054900030612     C                   EVAL      i = i + 1
055000030612     C                   ELSE
055100030612     C                   LEAVE
055200030612     C                   ENDIF
055300030612     C                   ENDDO
055400030612     C                   ENDIF
055500030612     C*
055600030612     C                   ENDSR
055700030612     C*---------------------------------------------------------------*
055800030610
055900030610
056000030610
056100030610
056200030610     C*------------------------------------------------------------------------*
056300030610     C* UPDKSC - Routine di aggiornamento dati su anagrafica cliente
056400030610     C*------------------------------------------------------------------------*
056500030610     C     UPDKSC        BEGSR
056600030610     C*
056700030610     C* Aggiorno i dati del cliente su cnind00f
056800030611     C                   EVAL      indSIN = savIMPDF
056900030610     C   55              UPDATE    cnind000
057000030612     C*
057100030612     C* Incremento il contatore dei record dell'anagrafica clienti modificati
057200030612     C                   EVAL      STPTCM  = STPTCM  + 1
057300030610     C*
057400030610     C* Aggancio il relativo record d cnaco00f x sfleggare il cliente ai fini della trasmissione
057500030610     C     KEYaco00C     CHAIN     cnaco00f
057600030610     C                   IF        %found(cnaco00f)
057700030610     C                   EVAL      acoFTR = *blanks
057800030610     C   55              UPDATE    cnaco000
057900030610     C                   ENDIF
058000030610     C*
058100030610     C                   ENDSR
058200030610     C*------------------------------------------------------------------------*
058300030610
058400030610
058500030610
058600030610
058700030610     C*------------------------------------------------------------------------*
058800030610     C* EXETAR - Routine di listing e se richiesto annullamento/sfleggamento tariffe particolari
058900030610     C*------------------------------------------------------------------------*
059000030610     C     EXETAR        BEGSR
059100030610     C*
059200030610     C* Scorro la schiera contenete le info delle tariffe/tariffe particolari ed eseguo
059300030610     C* operazioni di listing/aggiornamento sulle tariffe particolari del cliente
059400030610     C                   Z-ADD     1             i
059500030610     C                   DOW       i <= %elem(skKSC)
059600030611     C*
059700030611     C* Procedo solo x gli elementi pieni
059800030611     C                   IF        skKSC(i) <> *zeros
059900030610     C*
060000030610     C* Elaboro tutte le tariffe da annullare
060100030610     C                   IF        skFLGANN(i) = 'A'
060200030610     C*
060300030610     C* Imposto la chiave di lettura tariffa particolare
060400030610     C                   EVAL      u_tpdKSC = skKSC(i)
060500030610     C                   EVAL      u_tpdCTR = skCTR(i)
060600030610     C                   EVAL      u_tpdPRG = skPRG(i)
060700030612     C                   EVAL      u_tpdFTC = '§'
060800030610     C*
060900030610     C* ...quindi scorro tutti i record...
061000030610     C     KEYtpd02P     SETLL     titpd02l
061100030610     C                   IF        %equal(titpd02l)
061200030610     C     KEYtpd02P     READE     titpd02l
061300030610     C                   DOW       not %eof(titpd02l)
061400030610     C                   EVAL      u_tpdATB = 'A'
061500030610     C                   EVAL      u_tpdFTR = *blanks
061600030610     C*
061700030701     C* Se richiesto lancio in aggiornamento => agiorno il file tariffe particolari
061800030701     C                   IF        *IN55 = *ON
061900030701     C                   UPDATE    titpd02
062000030701     C* ...contestualmnte annullo anche la testata tariffe particolari
062100030701     C     KEYtpd02P     CHAIN     titpt01l
062200030701     C                   IF        %found(titpt01l)
062300030701     C                   EVAL      tptATB = 'A'
062400030701     C                   EVAL      tptFTR = *blanks
062500030701     C                   UPDATE    titpt000
062600030701     C                   ENDIF
062700030701     C                   ENDIF
062800030611     C*
062900030612     C* Incremento il contatore delle tariffe particolari annullate
063000030611     C                   EVAL      STPTTPA = STPTTPA + 1
063100030610     C*
063200030610     C* Ed eseguo il listing di quanto aggiornato
063300030610     C                   EXSR      STADET
063400030610     C*
063500030610     C* Proseguo la lettura delle tariffe particolari
063600030610     C     KEYtpd02P     READE     titpd02l
063700030610     C                   ENDDO
063800030610     C                   ENDIF
063900030611     C*
064000030611     C* Effettuo comunque il listing anche se NON annullo tariffe particolari ma aggiorno i file
064100030612     C* e se l'importo che viene ricoperto è <> 0
064200030612     C                   ELSE
064300030612     C                   IF        skSIN(i) <> 0
064400030611     C                   CLEAR                   titpd02
064500030611     C                   EXSR      STADET
064600030612     C                   ENDIF
064700030610     C                   ENDIF
064800030610     C*
064900030610     C* Continuo con lo scorrimento della schiera tariffe
065000030610     C                   EVAL      i = i + 1
065100030611     C                   ELSE
065200030611     C                   LEAVE
065300030611     C                   ENDIF
065400030610     C                   ENDDO
065500030610     C*
065600030610     C                   ENDSR
065700030610     C*---------------------------------------------------------------*
065800030610
065900030610
066000030610
066100030610
066200030610     C*------------------------------------------------------------------------*
066300030610     C* STATES - Routine di stampa testata prospetto
066400030610     C*------------------------------------------------------------------------*
066500030610     C     STATES        BEGSR
066600030610     C*
066700030613     C* Imposto campi variabili a seconda dei file da elaborare (tariffe/offerte)
066800030613     C                   EVAL      STPTT1 = 'TARIFFE'
066900030613     C                   EVAL      STPTT2 = 'COD. CLI.'
067000030616     C                   EVAL      STPTT3 = 'tariffe'
067100030923     C                   EVAL      STPSYF = 'P.O.: '
067200030613     C*
067300030610     C                   WRITE     VR15PT
067400030610     C                   WRITE     VR15PI
067500030610     C*
067600030610     C                   ENDSR
067700030610     C*------------------------------------------------------------------------*
067800030610
067900030610
068000030610
068100030610
068200030610     C*------------------------------------------------------------------------*
068300030610     C* STADET - Routine di stampa riga di dettaglio prospetto
068400030610     C*------------------------------------------------------------------------*
068500030610     C     STADET        BEGSR
068600030610     C*
068700030610     C* Testo se necessario salto pagina
068800030610     C   01              EXSR      STATES
068900030610     C   01              SETOFF                                       01
069000030610     C*
069100030611     C* Imposto il buffer di stampa
069200030611     C                   CLEAR                   VR15P2
069300030611     C                   IF        skKSC(i) <> depSTPKSC
069400030610     C                   EVAL      STPKSC = skKSC(i)
069500030611     C                   EVAL      STPRAG = skRAGSOC(i)
069600030613     C                   MOVE      skSIN(i)      STPSIN
069700030611     C                   EVAL      STPIDF = %editw(savIMPDF:' 0,   ')
069800030611     C* Se impostato valore DF da cartello lo esplicito nel prospetto
069900030611     C                   IF        skFLGKSCK(i)    = 'S'
070000030611     C                   EVAL      STPCAR = 'Tariffa di cartello'
070100030611     C                   ENDIF
070200030611     C                   ENDIF
070300030612     C*
070400030612     C* I dati relativi alla tariffa/tariffa particolare li valorizzo solo se trattasi d tariffa
070500030612     C* da annullare così nel prospetto vengono evidenziate solo le tariffe/tariffe particolari
070600030612     C* modificate (annullate)
070700030612     C                   IF        skFLGANN(i) = 'A'
070800030612     C* Se la data decorrenza tariffa è > 0 => vuol dire che ho la tariffa x cui valorizzo i campi
070900030612     C* in stampa, altrimenti nn faccio nemmeno le assegnazioni (che mi farebbero casino nel layout)
071000030612     C                   IF        skDDT(i) > 0
071100030612     C                   MOVEL     skCTR(i)      STPCTR
071200030610     C                   EVAL      STPPRG = skPRG(i)
071300030610     C                   EVAL      STPDDT = skDDT(i)
071400030610     C                   EVAL      STPDST = skDST(i)
071500030610     C                   EVAL      STPCTS = u_tpdCTS
071600030610     C                   EVAL      STPSGL = u_tpdSGL
071700030610     C                   EVAL      STPITR = u_tpdITR
071800030612     C                   ENDIF
071900030612     C                   ENDIF
072000030612     C*
072100030612     C* Stampo solo se la tariffa particolare è da annullare oppure se
072200030612     C* KSC e CTR nn sono gli stessi precedentemente già stampati
072300030612     C                   IF        skKSC(i)    <> depSTPKSC OR
072400030612     C                             STPCTR      <> depSTPCTR OR
072500030612     C                             skFLGANN(i) = 'A'
072600030610     C                   WRITE     VR15P2
072700030612     C                   EVAL      depSTPKSC = skKSC(i)
072800030612     C                   EVAL      depSTPCTR = STPCTR
072900030612     C                   ENDIF
073000030610     C*
073100030610     C                   ENDSR
073200030610     C*------------------------------------------------------------------------*
073300030610
073400030610
073500030610
073600030610
073700030610     C*------------------------------------------------------------------------*
073800030610     C* STAFIN - Routine di stampa chiusura prospetto
073900030610     C*------------------------------------------------------------------------*
074000030610     C     STAFIN        BEGSR
074100030610     C*
074200030610     C                   WRITE     VR15RI
074300030611     C                   WRITE     VR15TT
074400030610     C                   WRITE     VR15FI
074500030610     C*
074600030610     C                   ENDSR
074700030610     C*------------------------------------------------------------------------*
074800030610
074900030610
075000030610
075100030610
075200030610     C*------------------------------------------------------------------------*
075300030610     C* CARTAB - Routine di reperimento valori tabellati
075400030610     C*------------------------------------------------------------------------*
075500030610     C     CARTAB        BEGSR
075600030610     C*
075700030610     C* Reperisco il valore del diritto di fatturazione di cartello
075800030610     C                   EVAL      tbeCOD = 'GEI'
075900030610     C                   EVAL      tbeKE1 = 'EUR'
076000030610     C     KEYtbe01P     CHAIN     tntbe01l
076100030610     C                   IF        %found(tntbe01l)
076200030610     C                   IF        tbeATB = *blanks
076300030610     C                   EVAL      DGEI = tbeUNI
076400030610     C                   ENDIF
076500030610     C                   ENDIF
076600030610     C*
076700030610     C                   ENDSR
076800030610     C*------------------------------------------------------------------------*
076900010509
077000010509
077100001218
077200001218     C*------------------------------------------------------------------------*
077300001218     C* *INZSR - ROUTINE INIZIALE
077400001218     C*------------------------------------------------------------------------*
077500001218     C     *INZSR        BEGSR
077600030611     C*
077700030610     C                   SETON                                        55
077800030521     C*
077900030521     C* Reperisco l'UDATE del job
078000030521     C                   TIME                    WN14             14 0
078100030521     C                   MOVE      WN14          WN8               8 0
078200030521     C                   Z-ADD     WN8           G08DAT
078300030521     C                   Z-ADD     *zeros        G08INV
078400030521     C                   MOVEL     '0'           G08ERR
078500030521     C                   CALL      'XSRDA8'
078600030521     C                   PARM                    WLBDA8
078700030521     C                   Z-ADD     G08INV        DATCOR            8 0
078800010726     C*
078900030422     C* Inizializzo campi chiave "fissi"
079000030605     C                   EVAL      indKUT = 1
079100030605     C                   EVAL      indKCC = 151
079200030605     C                   EVAL      tpdFTC = '§'
079300030414     C****
079400030414     C* Definizione chiavi
079500030414     C****
079600030605     C* Chiave su CNIND00F - completa
079700030605     C     KEYind00C     KLIST
079800030605     C                   KFLD                    indKUT
079900030605     C                   KFLD                    indKCC
080000030605     C                   KFLD                    indKSC
080100030605     C* Chiave su CNACO00F - completa
080200030605     C     KEYaco00C     KLIST
080300030605     C                   KFLD                    indKUT
080400030605     C                   KFLD                    indKCC
080500030605     C                   KFLD                    indKSC
080600030605     C* Chiave su TNTAM01L - parziale
080700030605     C     KEYtam01P     KLIST
080800030605     C                   KFLD                    indKSC
080900030605     C* Chiave su TITPD01L - parziale
081000030605     C     KEYtpd01P     KLIST
081100030605     C                   KFLD                    tamKSC
081200030605     C                   KFLD                    tamCTR
081300030605     C                   KFLD                    tamPRG
081400030605     C                   KFLD                    tpdFTC
081500030610     C* Chiave su TITPD02L - parziale
081600030610     C     KEYtpd02P     KLIST
081700030610     C                   KFLD                    u_tpdKSC
081800030610     C                   KFLD                    u_tpdCTR
081900030610     C                   KFLD                    u_tpdPRG
082000030610     C                   KFLD                    u_tpdFTC
082100030610     C* Chiave su TNTBE01L - parziale
082200030610     C     KEYtbe01P     KLIST
082300030610     C                   KFLD                    tbeCOD
082400030610     C                   KFLD                    tbeKE1
082500001218     C*
082600001218     C                   ENDSR
