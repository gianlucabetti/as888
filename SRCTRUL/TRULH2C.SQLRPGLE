000100140318      //************************************************************************
000101140318      //
000102140318      // Questo programma converte i caratteri HTML nel corrispondente valore
000103140318      // EBCDIC (p.e. "&apos;" in "'").
000104140318      //
000105140318      //************************************************************************
000106140318
000107140319     H DFTACTGRP(*NO) ACTGRP('H2C')
000108140318
000109140318      //************************************************************************
000110140318      //
000112140318      // Costanti.
000113140318      //
000114140318      //************************************************************************
000115140318
000116140318     D/COPY GAITRASRC/SRCCONST,TRULH2C
000117140318     D THIS_H2C        S              3A   INZ('H2C')
000118140318     D THIS_HTML2CHAR  S             15A   INZ('*HTML2CHAR')
000119140318     D THIS_CHAR2HTML  S             15A   INZ('*CHAR2HTML')
000120140319     D AMPERSAND       C                   '&'
000121140319     D PUNTOVIRGOLA    C                   ';'
000122140318
000153140318      //************************************************************************
000154140318      //
000155140318      // Prototipi.
000156140318      //
000157140318      //************************************************************************
000158140318
000159140318     D/COPY GAITRASRC/SRCPROTOPR,TRULH2C
000160140318
000161140318      //************************************************************************
000162140318      //
000163140318      // Strutture dati.
000164140318      //
000165140318      //************************************************************************
000166140318
000173140318     D this            DS                  QUALIFIED INZ
000174140319     D  codiceOperativo...
000175140319     D                               10I 0
000176140318     D  stringaInput...
000177140319     D                            32767A   VARYING
000178140318     D  stringaOutput...
000179140319     D                            32767A   VARYING
000180140319     D  htmlLower                    15A   VARYING
000181140319     D  htmlUpper                    15A   VARYING
000182140318     D  char                          1A
000184140319     D  p                            10I 0
000185140318
000186140318      //************************************************************************
000187140318      //
000188140318      // Procedure interface.
000189140318      //
000190140318      //************************************************************************
000191140318
000192140318     D TrulH2C         PI
000193140319     D  prmCodiceOperativo...
000194140318     D                               10I 0 CONST
000195140320     D  prmEsito...
000196140320     D                               10I 0 OPTIONS(*NOPASS)
000197140318     D  prmStringaInput...
000198140320     D                            32767A   OPTIONS(*VARSIZE:*NOPASS)
000199140318     D  prmStringaInputSize...
000200140320     D                               10I 0 CONST OPTIONS(*NOPASS)
000201140318     D  prmStringaOutput...
000202140320     D                            32767A   OPTIONS(*VARSIZE:*NOPASS)
000203140318     D  prmStringaOutputSize...
000204140320     D                               10I 0 CONST OPTIONS(*NOPASS)
000205140318
000206140318      //************************************************************************
000207140318      //
000208140318      // Main.
000209140318      //
000210140318      //************************************************************************
000211140318
000212140318      /FREE
000215140319
000216140319       IF prmCodiceOperativo = H2C_SETONLR;
000217140320         *INLR = *ON;
000218140320         SELECT;
000219140320           WHEN this.codiceOperativo = H2C_HTML2CHAR;
000234140320             EXEC SQL CLOSE cvtTblHtml2Char;
000236140320           WHEN this.codiceOperativo = H2C_CHAR2HTML;
000250140320             EXEC SQL CLOSE cvtTblChar2Html;
000257140320         ENDSL;
000259140319         RETURN;
000260140319       ENDIF;
000261140320
000262140320       CLEAR prmEsito;
000263140320
000264140320       // Il codice operativo specifica il tipo di conversione (HTML <-> EBCDIC)
000265140320       // e deve essere uguale in tutto il ciclo delle chiamate.
000266140320
000267140320       IF prmCodiceOperativo <> this.codiceOperativo;
000268140320         prmEsito = H2C_ESITO_ERRORE;
000269140320         RETURN;
000270140320       ENDIF;
000271140320
000272140320       // Controllo di aver ricevuto delle dimensioni valide.
000273140318
000274140320       IF prmStringaInputSize < 1 OR prmStringaOutputSize < 1
000276140318       OR prmStringaInputSize > %SIZE(prmStringaInput)
000277140318       OR prmStringaOutputSize > %SIZE(prmStringaOutput);
000280140318         prmEsito = H2C_ESITO_ERRORE;
000281140318         RETURN;
000282140318       ENDIF;
000283140320
000284140320       // Resettando la struttura this pulisco e resetto tutte le variabili
000285140320       // usate nel programma.
000286140318
000287140318       RESET this;
000288140318
000289140320       // Prendo solo la porzione di stringa su cui devo lavorare.
000290140320       // Già che ci sono elimino gli spazi superflui.
000291140320
000292140320       this.stringaInput = %TRIMR(%SUBST( prmStringaInput
000293140320                                        : 1 : prmStringaInputSize ));
000294140319
000295140320       // Restituisco sempre qualcosa.
000296140320
000297140319       this.stringaOutput = this.stringaInput;
000300140320
000301140320       IF %LEN(this.stringaOutput) > *ZERO;
000304140318
000305140320         // Eseguo la conversione richiesta.
000306140320
000307140320         SELECT;
000308140318
000309140320           WHEN this.codiceOperativo = H2C_HTML2CHAR;
000310140319
000311140320             // Poiché i caratteri HTML iniziano con "&" e finiscono con ";"
000312140320             // (p.e. &apos;) eseguo le sostituzioni solo se nella stringa
000313140320             // esiste almeno un "&" seguito da almeno un ";".
000314140319
000315140320             this.p = %SCAN(AMPERSAND : this.stringaOutput);
000316140319
000317140320             IF this.p > *ZERO
000318140320             AND %SCAN(PUNTOVIRGOLA : this.stringaOutput : this.p) > this.p;
000319140319
000320140320               EXEC SQL
000321140320                 FETCH BEFORE FROM cvtTblHtml2Char
000322140320               ;
000323140318
000324140320               SELECT;
000325140320                 WHEN sqlCode < *ZERO;
000326140320                   DUMP(A);
000327140320                   prmEsito = sqlCode;
000328140320                   RETURN;
000329140320               ENDSL;
000330140318
000331140320               DOU sqlCode < *ZERO;
000332140318
000333140320                 EXEC SQL
000334140320                   FETCH NEXT FROM cvtTblHtml2Char
000335140320                     INTO :this.htmlLower, :this.htmlUpper, :this.char
000336140320                 ;
000337140318
000338140320                 SELECT;
000339140320                   WHEN sqlCode = 100;
000340140320                     LEAVE;
000341140320                   WHEN sqlCode < *ZERO;
000342140320                     DUMP(A);
000343140320                     prmEsito = sqlCode;
000344140320                     LEAVE;
000345140320                 ENDSL;
000346140318
000347140320                 this.stringaOutput = %SCANRPL( this.htmlLower
000348140320                                              : this.char
000349140320                                              : this.stringaOutput
000350140320                                              : this.p );
000351140319
000352140320                 this.stringaOutput = %SCANRPL( this.htmlUpper
000353140320                                              : this.char
000354140320                                              : this.stringaOutput
000355140320                                              : this.p );
000356140318
000357140320               ENDDO;
000358140319
000359140320             ENDIF;
000360140318
000361140320           WHEN this.codiceOperativo = H2C_CHAR2HTML;
000362140319
000363140320             EXEC SQL
000364140320               FETCH BEFORE FROM cvtTblChar2Html
000365140320             ;
000366140319
000367140320             SELECT;
000368140320               WHEN sqlCode < *ZERO;
000369140320                 DUMP(A);
000370140320                 prmEsito = sqlCode;
000371140320                 RETURN;
000372140320             ENDSL;
000373140319
000374140320             DOU sqlCode < *ZERO;
000375140319
000376140320               EXEC SQL
000377140320                 FETCH NEXT FROM cvtTblChar2Html
000378140320                   INTO :this.char, :this.htmlLower
000379140320               ;
000380140319
000381140320               SELECT;
000382140320                 WHEN sqlCode = 100;
000383140320                   LEAVE;
000384140320                 WHEN sqlCode < *ZERO;
000385140320                   DUMP(A);
000386140320                   prmEsito = sqlCode;
000387140320                   LEAVE;
000388140320               ENDSL;
000389140319
000390140320               this.stringaOutput = %SCANRPL( this.char
000391140320                                            : this.htmlLower
000392140320                                            : this.stringaOutput );
000393140319
000394140320             ENDDO;
000395140318
000396140320         ENDSL;
000397140318
000398140320       ENDIF;
000401140318
000402140318       %SUBST(prmStringaOutput : 1 : prmStringaOutputSize) = this.stringaOutput;
000403140318
000404140318       RETURN;
000405140318
000406140318       //***********************************************************************
000407140318       //
000408140318       // Operazioni iniziali.
000409140318       //
000410140318       //***********************************************************************
000411140318
000412140318       BEGSR *INZSR;
000413140318
000414140318         SELECT;
000415140318
000416140319           WHEN prmCodiceOperativo = H2C_HTML2CHAR;
000417140318
000418140318             EXEC SQL
000419140318               DECLARE cvtTblHtml2Char INSENSITIVE SCROLL CURSOR FOR
000420140319                 SELECT LOWER(RTRIM(tbeKe2))
000421140319                      , UPPER(RTRIM(tbeKe2))
000422140319                      , CHAR(LEFT(tbeUni, 1), 1)
000423140318                   FROM tntbe00f
000424140318                  WHERE tbeCod = :THIS_H2C
000425140318                    AND tbeKe1 = :THIS_HTML2CHAR
000426140318                    AND tbeLin = ''
000427140318                    AND tbeSif = ''
000428140318                    AND tbeAtb = ''
000429140318             ;
000430140318
000431140318             EXEC SQL OPEN cvtTblHtml2Char;
000432140318
000433140319           WHEN prmCodiceOperativo = H2C_CHAR2HTML;
000434140318
000435140318             EXEC SQL
000436140318               DECLARE cvtTblChar2Html INSENSITIVE SCROLL CURSOR FOR
000437140319                 SELECT CHAR(LEFT(tbeKe2, 1), 1)
000438140319                      , RTRIM(tbeUni)
000439140318                   FROM tntbe00f
000440140318                  WHERE tbeCod = :THIS_H2C
000441140318                    AND tbeKe1 = :THIS_CHAR2HTML
000442140318                    AND tbeLin = ''
000443140318                    AND tbeSif = ''
000444140318                    AND tbeAtb = ''
000445140318             ;
000446140318
000447140318             EXEC SQL OPEN cvtTblChar2Html;
000448140319
000449140319           OTHER;
000450140319
000451140319             prmEsito = H2C_ESITO_ERRORE;
000452140319             RETURN;
000453140319
000454140318         ENDSL;
000455140318
000456140318         IF sqlCode < *ZERO;
000457140318           DUMP(A);
000458140318           prmEsito = sqlCode;
000459140318           RETURN;
000460140318         ENDIF;
000461140318
000462140319         this.codiceOperativo = prmCodiceOperativo;
000463140319
000464140318       ENDSR;
000465140318
000466140318      /END-FREE
