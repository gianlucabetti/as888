000100091222     ***************************************************************************
000200091222     **
000300091222     ** Questo programma sistema il codice di esenzione IVA sui clienti.
000400091222     **
000500091222     ***************************************************************************
000600091223     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('PJXBND':'QC2LE')
000700091222
000800091222     ***************************************************************************
000900091222     **
001000091222     ** File.
001100091222     **
001200091222     ***************************************************************************
001300091222     Fprtf198   O    F  198        PRINTER OFLIND(*INOA)
001400091222
001500091222     ***************************************************************************
001600091222     **
001700091222     ** Costanti.
001800091222     **
001900091222     ***************************************************************************
002000091222     D DATA_FATTURA_FINE_MESE...
002100091222     D                 C                   'F'
002200091222     D DATA_FATTURA_INIZIO_MESE...
002300091222     D                 C                   'I'
002400091222     D ESITO_ERROR...
002500091222     D                 C                   -1
002600091222     D ESITO_OK...
002700091222     D                 C                   0
002800091222     D PROJ_ESITO_OK...
002900091222     D                 C                   '0'
003000091222
003100091222     ***************************************************************************
003200091222     **
003300091222     ** Prototipi.
003400091222     **
003500091222     ***************************************************************************
003600091222      /COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
003700091223      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
003800091222      /COPY GAITRASRC/SRCPROTOPR,YCOCDIVA0
003900091222
004000091222     ***************************************************************************
004100091222     **
004200091222     ** Strutture dati.
004300091222     **
004400091222     ***************************************************************************
004500091222     D andiz00f      E DS                  QUALIFIED
004600091222     D                                     BASED(nullPtr)
004700091222     D dvaCli        E DS                  QUALIFIED
004800091222     D                                     INZ
004900091222     D kpjba         E DS                  QUALIFIED
005000091222     D                                     BASED(nullPtr)
005100091222     D ycoCdIvaI     E DS                  QUALIFIED
005200091222     D                                     INZ
005300091222     D ycoCdIvaO     E DS                  QUALIFIED
005400091222     D                                     INZ
005500091222
005600091222     ***************************************************************************
005700091222     **
005800091222     ** Campi.
005900091222     **
006000091222     ***************************************************************************
006100091222     D aggiornare      S               N
006200091222     D dataFattura     S              1A
006300091222     D esito           S             10I 0
006400091222     D esitoProj       S              1A
006500091222     D kscCliente      S                   LIKE(andiz00f.ksc)
006600091222     D logTxt          S            198A
006700091223     D rowCount        S             10I 0
006800091222     D societa         S                   LIKE(andiz00f.societa)
006900091222     D unita           S                   LIKE(andiz00f.unita)
007000091222
007100091222     ***************************************************************************
007200091222     **
007300091222     ** Parametri.
007400091222     **
007500091222     ***************************************************************************
007600091222     C     *ENTRY        PLIST
007700091222     C                   PARM                    dataFattura
007800091222     C                   PARM                    aggiornare
007900091222
008000091222     ***************************************************************************
008100091222     **
008200091222     ** Main.
008300091222     **
008400091222     ***************************************************************************
008500091222
008600091222      /FREE
008700091222
008800091222       *INLR = *ON;
008900091222
009000091222       logTxt = 'Sistemazione codice esenzione IVA clienti'
009100091222              + ' Data fattura '
009200091222              + dataFattura
009300091222              + ' Aggiornamento '
009400091222              + aggiornare
009500091222              ;
009600091222
009700091222       EXCEPT log;
009800091222
009900091222       //***********************************************************************
010000091222       //
010100091222       // Controllo parametri ricevuti.
010200091222       //
010300091222       //***********************************************************************
010400091222
010500091222       IF dataFattura <> DATA_FATTURA_FINE_MESE
010600091222       AND dataFattura <> DATA_FATTURA_INIZIO_MESE;
010700091222         logTxt = 'Parametri non validi. Elaborazione non eseguita.';
010800091222         EXCEPT log;
010900091222         RETURN;
011000091222       ENDIF;
011100091222
011200091222       //***********************************************************************
011300091222       //
011400091222       // Pulizia del codice esenzione IVA su tutti i clienti di bollettazione.
011500091222       //
011600091222       //***********************************************************************
011700091222
011800091223       IF aggiornare;
011900091222
012000091222         EXEC SQL
012100091222           UPDATE ANCLN00F
012200091222             SET ANCLN00F.CLNCDIVA = ''
012300091222             WHERE ANCLN00F.CLNSOCIETA = '201'
012400091222               AND ANCLN00F.CLNCLIFATT NOT IN (ANCLN00F.CLNKSC , '')
012500091222               AND ANCLN00F.CLNCDIVA <> ''
012600091222         ;
012700091223
012800091222         SELECT;
012900091223           WHEN sqlState = '57033';
013000091223             logTxt = 'ERRORE SQL ' + sqlState
013100091223                    + ' Almeno una riga non è stata aggiornata.'
013200091223                    ;
013300091223             EXCEPT log;
013400091222           WHEN sqlState > '03';
013500091222             DUMP(A);
013600091223             logTxt = 'ERRORE SQL ' + sqlState;
013700091222             EXCEPT log;
013800091223             ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
013900091222             RETURN;
014000091222         ENDSL;
014100091222
014200091223         EXEC SQL
014300091223           GET DIAGNOSTICS :rowCount = ROW_COUNT
014400091223         ;
014500091223
014600091223         logTxt = 'Tolto codice esenzione IVA da ' + %CHAR(rowCount)
014700091223                + ' clienti di bollettazione.'
014800091223                ;
014900091223
015000091223         EXCEPT log;
015100091223
015200091222       ENDIF;
015300091222
015400091222       //***********************************************************************
015500091222       //
015600091222       // Impostazione del codice esenzione IVA sui clienti di fatturazione.
015700091222       //
015800091222       //***********************************************************************
015900091222
016000091222       IF dataFattura = DATA_FATTURA_INIZIO_MESE;
016100091222
016200091222         EXEC SQL
016300091222           DECLARE CLIENTI_INIZIO NO SCROLL CURSOR FOR
016400091222             SELECT YBACL00F.ACLSOCIETA
016500091222                  , YBACL00F.ACLKSC
016600091222             FROM YBACL00F
016700091222             JOIN ANCLN00F
016800091222               ON YBACL00F.ACLSOCIETA = ANCLN00F.CLNSOCIETA
016900091222              AND YBACL00F.ACLKSC = ANCLN00F.CLNKSC
017000091222              AND 0 = ANCLN00F.CLNFILIALE
017100091222              AND '' = ANCLN00F.CLNLINEAV
017200091222             WHERE YBACL00F.ACLSOCIETA = '201'
017300091223               AND YBACL00F.ACLTDF = 'I'
017400091223               AND YBACL00F.ACLTFT = 0
017500091223               AND YBACL00F.ACLFFT IN ('1','4')
017600091222               AND ANCLN00F.CLNCLIFATT IN (ANCLN00F.CLNKSC , '')
017700091223             ORDER BY YBACL00F.ACLSOCIETA
017800091223                    , YBACL00F.ACLKCC
017900091223                    , YBACL00F.ACLKSC
018000091222           FOR FETCH ONLY
018100091222         ;
018200091222
018300091222         EXEC SQL
018400091222           OPEN CLIENTI_INIZIO
018500091222         ;
018600091222
018700091222       ELSE;
018800091222
018900091222         EXEC SQL
019000091222           DECLARE CLIENTI_FINE NO SCROLL CURSOR FOR
019100091222             SELECT YBACL00F.ACLSOCIETA
019200091222                  , YBACL00F.ACLKSC
019300091222             FROM YBACL00F
019400091222             JOIN ANCLN00F
019500091222               ON YBACL00F.ACLSOCIETA = ANCLN00F.CLNSOCIETA
019600091222              AND YBACL00F.ACLKSC = ANCLN00F.CLNKSC
019700091222              AND 0 = ANCLN00F.CLNFILIALE
019800091222              AND '' = ANCLN00F.CLNLINEAV
019900091222             WHERE YBACL00F.ACLSOCIETA = '201'
020000091223               AND ( YBACL00F.ACLTFT = 1
020100091223                     OR
020200091223                     YBACL00F.ACLTDF = 'F' AND YBACL00F.ACLTFT = 0
020300091223                   )
020400091223               AND ANCLN00F.CLNCLIFATT IN (ANCLN00F.CLNKSC , '')
020500091223             ORDER BY YBACL00F.ACLSOCIETA
020600091223                    , YBACL00F.ACLKCC
020700091223                    , YBACL00F.ACLKSC
020800091222           FOR FETCH ONLY
020900091222         ;
021000091222
021100091222         EXEC SQL
021200091222           OPEN CLIENTI_FINE
021300091222         ;
021400091222
021500091222       ENDIF;
021600091222
021700091222       SELECT;
021800091222         WHEN sqlState > '03';
021900091222           DUMP(A);
022000091223           logTxt = 'ERRORE SQL ' + sqlState;
022100091222           EXCEPT log;
022200091223           ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
022300091222           RETURN;
022400091222       ENDSL;
022500091222
022600091222       DOU sqlState > '03';
022700091222
022800091222         IF dataFattura = DATA_FATTURA_INIZIO_MESE;
022900091222           EXEC SQL
023000091222             FETCH NEXT FROM CLIENTI_INIZIO
023100091222               INTO :societa
023200091222                  , :kscCliente
023300091222           ;
023400091222         ELSE;
023500091222           EXEC SQL
023600091222             FETCH NEXT FROM CLIENTI_FINE
023700091222               INTO :societa
023800091222                  , :kscCliente
023900091222           ;
024000091222         ENDIF;
024100091222
024200091222         SELECT;
024300091222           WHEN sqlState = '02000';
024400091222             LEAVE;
024500091222           WHEN sqlState > '03';
024600091222             DUMP(A);
024700091222             logTxt = 'Errore SQL ' + sqlState;
024800091222             EXCEPT log;
024900091223             ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
025000091222             LEAVE;
025100091222         ENDSL;
025200091222
025300091222         Proj_Soggetto( '1'
025400091222                      : societa
025500091222                      : unita
025600091222                      : 'DVACLI'
025700091222                      : %DATE()
025800091222                      : %SIZE(dvaCli)
025900091222                      : 'C'
026000091222                      : kscCliente
026100091222                      : *BLANK
026200091222                      : *ZERO
026300091222                      : *OMIT
026400091222                      : *BLANK
026500091222                      : *BLANK
026600091222                      : esitoProj
026700091222                      : dvaCli
026800091222                      );
026900091222
027000091222         IF esitoProj <> *OFF;
027100091223           logTxt = societa + ' ' + kscCliente
027200091222                  + ' ERRORE Anagrafica Proj non reperita.'
027300091222                  ;
027400091222           EXCEPT log;
027500091222         ENDIF;
027600091222
027700091222         ycoCdIvaI.societa = societa;
027800091222         ycoCdIvaI.kscCliente = kscCliente;
027900091222         ycoCdIvaI.partitaIva = dvaCli.dvcPartIva;
028000091222         ycoCdIvaI.nazione = dvaCli.dvcStato;
028100091222         ycoCdIvaI.cap = dvaCli.dvcCap;
028200091222         ycoCdIvaI.codiceIva = dvaCli.dvcCdIva;
028300100104         ycoCdIvai.provincia = dvaCli.dvcProv;
028400100104         ycoCdIvai.localita = dvaCli.dvcLocalit;
028500100104
028600091222         GetCodiceIvaPerCliente( 'GETCDIVA'
028700091222                               : esito
028800091222                               : ycoCdIvaI
028900091222                               : %SIZE(ycoCdIvaI)
029000091222                               : ycoCdIvaO
029100091222                               : %SIZE(ycoCdIvaO)
029200091222                               );
029300091222
029400091222         IF esito < ESITO_OK;
029500091223
029600091223           logTxt = societa + ' ' + kscCliente
029700091222                  + ' ' + dvaCli.dvcDes
029800091222                  + ' ' + dvaCli.dvcPartIva
029900091222                  + ' ' + dvaCli.dvcStato
030000100104                  + ' ' + dvaCli.dvcProv
030100091222                  + ' ' + dvaCli.dvcCap
030200100104                  + ' ' + dvaCli.dvcLocalit
030300091222                  + ' ERRORE Codice IVA non calcolato.'
030400091222                  ;
030500091223
030600091222           EXCEPT log;
030700091223
030800091223         ELSE;
030900091222
031000091223           IF ycoCdIvaI.codiceIva <> ycoCdIvaO.codiceIva;
031100091222
031200091223             logTxt = societa + ' ' + kscCliente
031300091223                    + ' ' + dvaCli.dvcDes
031400091223                    + ' ' + dvaCli.dvcPartIva
031500091223                    + ' ' + dvaCli.dvcStato
031600100104                    + ' ' + dvaCli.dvcProv
031700091223                    + ' ' + dvaCli.dvcCap
031800100104                    + ' ' + dvaCli.dvcLocalit
031900100104                    + ' Da ' + ycoCdIvaI.codiceIva
032000100104                    + ' a ' + ycoCdIvaO.codiceIva
032100091223                    ;
032200091222
032300091223             EXCEPT log;
032400091222
032500091223             IF aggiornare;
032600091222
032700091223               DOU sqlState <> '57033';
032800091223
032900091223                 EXEC SQL
033000091223                   UPDATE ANCLN00F
033100091223                     SET ANCLN00F.CLNCDIVA = :ycoCdIvaO.codiceIva
033200091223                     WHERE ANCLN00F.CLNSOCIETA = :societa
033300091223                       AND ANCLN00F.CLNKSC = :kscCliente
033400091223                       AND ANCLN00F.CLNFILIALE = 0
033500091223                       AND ANCLN00F.CLNLINEAV = ''
033600091223                 ;
033700091222
033800091223                 SELECT;
033900091223                   WHEN sqlState = '57033';
034000091223                   WHEN sqlState > '03';
034100091223                     DUMP(A);
034200091223                     logTxt = 'ERRORE SQL ' + sqlState;
034300091223                     EXCEPT log;
034400091223                     ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
034500091223                     LEAVE;
034600091223                 ENDSL;
034700091222
034800091223               ENDDO;
034900091223
035000091223             ENDIF;
035100091222
035200091223           ENDIF;
035300091223
035400091223         ENDIF;
035500091222
035600091222       ENDDO;
035700091222
035800091222       IF dataFattura = DATA_FATTURA_INIZIO_MESE;
035900091222         EXEC SQL
036000091222           CLOSE CLIENTI_INIZIO
036100091222         ;
036200091222       ELSE;
036300091222         EXEC SQL
036400091222           CLOSE CLIENTI_FINE
036500091222         ;
036600091222       ENDIF;
036700091222
036800091222      /END-FREE
036900091222
037000091222     ***************************************************************************
037100091222     **
037200091222     ** Stampa log.
037300091222     **
037400091222     ***************************************************************************
037500091222     Oprtf198   E            log         1
037600091222     O                       logTxt
