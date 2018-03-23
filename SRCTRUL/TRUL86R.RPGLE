000100970526     H*------------------------------------------------------------------------*
000200000920     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600060331     Ftitas30c  IF   E           K DISK
000700030808     Ftita730c  IF   E           K DISK
000800970526     D*------------------------------------------------------------------------*
000900030808     D* SCHIERE
001000030808     D*------------------------------------------------------------------------*
001100030808     D*------------
001200030808     D* SCHIERE SIGLE E DESCRIZIONI VARIE
001300030808     D*------------
001400060421     D skVARIE         S                   LIKE(dsVARIA) DIM(50) INZ
001500060331     D jVARIE          S              2  0 INZ
001600060331     D*------------------
001700060331     D* DS RIDEFINIZIONE OUTPUT
001800060331     D*------------------
001900060331     D dsVARIA         DS
002000060420     D  dVARIA_COD                    1    INZ
002100060420     D  dVARIA_VAL                   13  3 INZ
002200030808     D*------------------
002300060331     D* DS D PROCEDURA
002400030808     D*------------------
002500060331     D TRUL86DS      E DS
002600970526     C*------------------------------------------------------------------------*
002700991214     C* MAIN LINES
002800970526     C*------------------------------------------------------------------------*
002900030808     C*
003000060331     C* Verifico subito il tipo lancio richiesto
003100060331     C                   if        UL86ITLA = 'C'                               * solo chiusura
003200060331     C                   seton                                        lr
003300060331     C                   endif
003400060331     C*
003500060331     C                   if        UL86ITLA = 'E'                               * solo elaborazione
003600060331     C                   exsr      esegui
003700060331     C                   seton                                        rt
003800060331     C                   endif
003900060331     C*
004000060331     C                   if        UL86ITLA = *blanks                           * elabora e chiude
004100060331     C                   exsr      esegui
004200060331     C                   seton                                        lr
004300060331     C                   endif
004400060331     C*
004500030808     C*------------------------------------------------------------------------*
004600060331     C* ESEGUI - Routine di reperimento PORTO e VARIE fatturate
004700030808     C*------------------------------------------------------------------------*
004800060331     C     ESEGUI        BEGSR
004900060420     C*
005000060421     C* Inizializzo indici, schiere e campi d output
005100060420     C                   clear                   jVARIE
005200060421     C                   clear                   skVARIE
005300060421     C                   clear                   UL86ODIV
005400060421     C                   clear                   UL86OPOR
005500060421     C                   clear                   UL86OOUT
005600060421     C                   clear                   UL86OIMV
005700060421     C                   clear                   UL86OTOT
005800060421     C*
005900060421     C* Inizializzo il campo di totalizzazione di tutti gli importi
006000060421     C                   z-add     *zeros        wTotImporti      15 3
006100060331     C*
006200060331     C* Innanzitutto aggancio la bolla
006300060421     C                   if        %trim(UL86ITBL) = '*'
006400060421     C                   seton                                        50
006500060421     C                   else
006600060421     C                   setoff                                       50
006700060421     C                   endif
006800060421     C*
006900060421     C   50KEYtas30_P    setll     titas30c
007000060421     C  N50KEYtas30_C    setll     titas30c
007100060421     C*
007200060331     C                   if        %found(titas30c)
007300060421     C   50KEYtas30_P    reade     titas30c
007400060421     C  N50KEYtas30_C    reade     titas30c
007500060421     C                   dow       not %eof(titas30c)
007600060420     C*
007700060420     C****
007800060420     C* DIVISA
007900060420     C****
008000060420     C*
008100060420     C* Valorizzo il campo DS di procedura preposto alla memorizzazione della DIVISA degli importi bolla corrente
008200060420     C                   eval      UL86ODIV = tasDIV
008300060331     C*
008400060331     C****
008500060331     C* PORTO
008600060331     C****
008700060331     C*
008800060420     C* Valorizzo il campo DS di procedura preposto alla memorizzazione del PORTO
008900060421     C                   eval      UL86OPOR = UL86OPOR + tasPOR
009000060420     C                   eval      wTotImporti = wTotImporti + tasPOR
009100060421     C*
009200060421     C****
009300060421     C* IMPONIBILE
009400060421     C****
009500060421     C*
009600060421     C* Valorizzo il campo DS di procedura preposto alla memorizzazione del totale imponibile
009700060421     C                   eval      UL86OIMV = UL86OIMV + tasIMV
009800060331     C*
009900060331     C****
010000060331     C* VARIA 1
010100060331     C****
010200060420     C*
010300060420     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
010400060420     C                   clear                   dsVARIA
010500060331     C*
010600060331     C* Solo se presente...
010700060331     C                   if        tasSV1 <> *blanks
010800060331     C*
010900060331     C* Incremento il contatore d schiera VARIE
011000060331     C                   add       1             jVARIE
011100060331     C*
011200060331     C* Valorizzo   la DS d ridefinizione elemento d schiera VARIE
011300060331     C                   EVAL      dVARIA_COD = tasSV1
011400060331     C                   EVAL      dVARIA_VAL = tasVA1
011500060420     C                   eval      wTotImporti = wTotImporti + dVARIA_VAL
011600060331     C*
011700060331     C* Valorizzo quindi l'elemento d schiera
011800060331     C                   eval      skVARIE(jVARIE) = dsVARIA
011900060331     C*
012000060331     C                   endif
012100060331     C*
012200060331     C****
012300060331     C* VARIA 2
012400060331     C****
012500060420     C*
012600060420     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
012700060420     C                   clear                   dsVARIA
012800060331     C*
012900060331     C* Solo se presente...
013000060331     C                   if        tasSV2 <> *blanks
013100060331     C*
013200060331     C* Incremento il contatore d schiera VARIE
013300060331     C                   add       1             jVARIE
013400060331     C*
013500060331     C* Valorizzo   la DS d ridefinizione elemento d schiera VARIE
013600060331     C                   EVAL      dVARIA_COD = tasSV2
013700060331     C                   EVAL      dVARIA_VAL = tasVA2
013800060420     C                   eval      wTotImporti = wTotImporti + dVARIA_VAL
013900060331     C*
014000060331     C* Valorizzo quindi l'elemento d schiera
014100060331     C                   eval      skVARIE(jVARIE) = dsVARIA
014200060331     C*
014300060331     C                   endif
014400060331     C*
014500060331     C****
014600060331     C* VARIA 3
014700060331     C****
014800060420     C*
014900060420     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
015000060420     C                   clear                   dsVARIA
015100060331     C*
015200060331     C* Solo se presente...
015300060331     C                   if        tasSV3 <> *blanks
015400060331     C*
015500060331     C* Incremento il contatore d schiera VARIE
015600060331     C                   add       1             jVARIE
015700060331     C*
015800060331     C* Valorizzo   la DS d ridefinizione elemento d schiera VARIE
015900060331     C                   EVAL      dVARIA_COD = tasSV3
016000060331     C                   EVAL      dVARIA_VAL = tasVA3
016100060420     C                   eval      wTotImporti = wTotImporti + dVARIA_VAL
016200060331     C*
016300060331     C* Valorizzo quindi l'elemento d schiera
016400060331     C                   eval      skVARIE(jVARIE) = dsVARIA
016500060331     C*
016600060331     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di estensione
016700060331     C     KEYtas30_C    setll     tita730c
016800060331     C                   if        %equal(tita730c)
016900060331     C     KEYtas30_C    reade     tita730c
017000060331     C                   dow       not %eof(tita730c)
017100060331     C*
017200060331     C* Incremento il contatore d schiera VARIE
017300060331     C                   add       1             jVARIE
017400060331     C*
017500060331     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
017600060331     C                   clear                   dsVARIA
017700060331     C*
017800060331     C* Valorizzo   la DS d ridefinizione elemento d schiera VARIE
017900060331     C                   EVAL      dVARIA_COD = ta7SVN
018000060331     C                   EVAL      dVARIA_VAL = ta7VAN
018100060420     C                   eval      wTotImporti = wTotImporti + dVARIA_VAL
018200060331     C*
018300060331     C* Valorizzo quindi l'elemento d schiera
018400060331     C                   eval      skVARIE(jVARIE) = dsVARIA
018500060331     C*
018600060331     C* Leggo tutte le varie della bolla
018700060331     C     KEYtas30_C    reade     tita730c
018800060331     C                   enddo
018900060331     C                   endif
019000060331     C*
019100060331     C                   endif
019200060421     C*
019300060421     C* Proseguo nella lettura delle bolle richieste
019400060421     C   50KEYtas30_P    reade     titas30c
019500060421     C  N50KEYtas30_C    reade     titas30c
019600060421     C                   enddo
019700060421     C*
019800060421     C                   endif
019900060421     C*
020000060421     C* Incremento il contatore d schiera VARIE
020100060421     C                   add       1             jVARIE
020200060421     C*
020300060421     C* Completo quindi i restanti elementi d schiera inizializzandoli
020400060421     C                   dow       jVARIE <= %elem(skVARIE)
020500060421     C*
020600060421     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
020700060421     C                   clear                   dsVARIA
020800060421     C*
020900060421     C* Valorizzo quindi l'elemento d schiera
021000060421     C                   eval      skVARIE(jVARIE) = dsVARIA
021100060421     C*
021200060421     C* Incremento il contatore d schiera VARIE
021300060421     C                   add       1             jVARIE
021400060421     C                   enddo
021500060331     C*
021600060331     C* Quindi imposto la DS x la restituzione dell'output richiesto
021700060420     C                   movea(p)  skVARIE       UL86OOUT
021800060420     C                   z-add     wTotImporti   UL86OTOT
021900030808     C*
022000030808     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200970526     C* *INZSR - OPERAZIONI INIZIALI
022300970526     C*------------------------------------------------------------------------*
022400970526     C     *INZSR        BEGSR
022500970526     C*---
022600970526     C* RICEVIMENTO PARAMETRI
022700970526     C*---
022800970526     C     *ENTRY        PLIST
022900060331     C                   PARM                    TRUL86DS
023000970526     C*---
023100991214     C* CHIAVI DI LETTURA
023200991214     C*---
023300000810     C*
023400060331     C* LETTURA titas30c - COMPLETA
023500060331     C     KEYtas30_C    KLIST
023600060331     C                   KFLD                    UL86IAAS
023700060331     C                   KFLD                    UL86ILNP
023800060331     C                   KFLD                    UL86INRS
023900060331     C                   KFLD                    UL86INSP
024000060331     C                   KFLD                    UL86ITBL
024100060421     C*
024200060421     C* LETTURA titas30c - PARZIALE
024300060421     C     KEYtas30_P    KLIST
024400060421     C                   KFLD                    UL86IAAS
024500060421     C                   KFLD                    UL86ILNP
024600060421     C                   KFLD                    UL86INRS
024700060421     C                   KFLD                    UL86INSP
024800991214     C*
024900970526     C                   ENDSR
