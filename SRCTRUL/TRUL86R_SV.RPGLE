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
001400060331     D skVARIE         S             14    DIM(50) INZ
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
005000060420     C* Inizializzo l'indice della schiera d wrk
005100060420     C                   clear                   jVARIE
005200060331     C*
005300060331     C* Innanzitutto aggancio la bolla
005400060331     C     KEYtas30_C    chain     titas30c
005500060331     C                   if        %found(titas30c)
005600060420     C*
005700060420     C* Inizializzo il campo di totalizzazione di tutti gli importi
005800060420     C                   z-add     *zeros        wTotImporti      15 3
005900060420     C*
006000060420     C****
006100060420     C* DIVISA
006200060420     C****
006300060420     C*
006400060420     C* Valorizzo il campo DS di procedura preposto alla memorizzazione della DIVISA degli importi bolla corrente
006500060420     C                   eval      UL86ODIV = tasDIV
006600060331     C*
006700060331     C****
006800060331     C* PORTO
006900060331     C****
007000060331     C*
007100060420     C* Valorizzo il campo DS di procedura preposto alla memorizzazione del PORTO
007200060420     C                   eval      UL86OPOR = tasPOR
007300060420     C                   eval      wTotImporti = wTotImporti + tasPOR
007400060331     C*
007500060331     C****
007600060331     C* VARIA 1
007700060331     C****
007800060420     C*
007900060420     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
008000060420     C                   clear                   dsVARIA
008100060331     C*
008200060331     C* Solo se presente...
008300060331     C                   if        tasSV1 <> *blanks
008400060331     C*
008500060331     C* Incremento il contatore d schiera VARIE
008600060331     C                   add       1             jVARIE
008700060331     C*
008800060331     C* Valorizzo   la DS d ridefinizione elemento d schiera VARIE
008900060331     C                   EVAL      dVARIA_COD = tasSV1
009000060331     C                   EVAL      dVARIA_VAL = tasVA1
009100060420     C                   eval      wTotImporti = wTotImporti + dVARIA_VAL
009200060331     C*
009300060331     C* Valorizzo quindi l'elemento d schiera
009400060331     C                   eval      skVARIE(jVARIE) = dsVARIA
009500060331     C*
009600060331     C                   endif
009700060331     C*
009800060331     C****
009900060331     C* VARIA 2
010000060331     C****
010100060420     C*
010200060420     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
010300060420     C                   clear                   dsVARIA
010400060331     C*
010500060331     C* Solo se presente...
010600060331     C                   if        tasSV2 <> *blanks
010700060331     C*
010800060331     C* Incremento il contatore d schiera VARIE
010900060331     C                   add       1             jVARIE
011000060331     C*
011100060331     C* Valorizzo   la DS d ridefinizione elemento d schiera VARIE
011200060331     C                   EVAL      dVARIA_COD = tasSV2
011300060331     C                   EVAL      dVARIA_VAL = tasVA2
011400060420     C                   eval      wTotImporti = wTotImporti + dVARIA_VAL
011500060331     C*
011600060331     C* Valorizzo quindi l'elemento d schiera
011700060331     C                   eval      skVARIE(jVARIE) = dsVARIA
011800060331     C*
011900060331     C                   endif
012000060331     C*
012100060331     C****
012200060331     C* VARIA 3
012300060331     C****
012400060420     C*
012500060420     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
012600060420     C                   clear                   dsVARIA
012700060331     C*
012800060331     C* Solo se presente...
012900060331     C                   if        tasSV3 <> *blanks
013000060331     C*
013100060331     C* Incremento il contatore d schiera VARIE
013200060331     C                   add       1             jVARIE
013300060331     C*
013400060331     C* Valorizzo   la DS d ridefinizione elemento d schiera VARIE
013500060331     C                   EVAL      dVARIA_COD = tasSV3
013600060331     C                   EVAL      dVARIA_VAL = tasVA3
013700060420     C                   eval      wTotImporti = wTotImporti + dVARIA_VAL
013800060331     C*
013900060331     C* Valorizzo quindi l'elemento d schiera
014000060331     C                   eval      skVARIE(jVARIE) = dsVARIA
014100060331     C*
014200060331     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di estensione
014300060331     C     KEYtas30_C    setll     tita730c
014400060331     C                   if        %equal(tita730c)
014500060331     C     KEYtas30_C    reade     tita730c
014600060331     C                   dow       not %eof(tita730c)
014700060331     C*
014800060331     C* Incremento il contatore d schiera VARIE
014900060331     C                   add       1             jVARIE
015000060331     C*
015100060331     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
015200060331     C                   clear                   dsVARIA
015300060331     C*
015400060331     C* Valorizzo   la DS d ridefinizione elemento d schiera VARIE
015500060331     C                   EVAL      dVARIA_COD = ta7SVN
015600060331     C                   EVAL      dVARIA_VAL = ta7VAN
015700060420     C                   eval      wTotImporti = wTotImporti + dVARIA_VAL
015800060331     C*
015900060331     C* Valorizzo quindi l'elemento d schiera
016000060331     C                   eval      skVARIE(jVARIE) = dsVARIA
016100060331     C*
016200060331     C* Leggo tutte le varie della bolla
016300060331     C     KEYtas30_C    reade     tita730c
016400060331     C                   enddo
016500060331     C                   endif
016600060331     C*
016700060331     C                   endif
016800060331     C*
016900060331     C                   endif
017000060420     C*
017100060420     C* Incremento il contatore d schiera VARIE
017200060420     C                   add       1             jVARIE
017300060420     C*
017400060420     C* Completo quindi i restanti elemneti d schiera inizializzandoli
017500060420     C                   dow       jVARIE <= %elem(skVARIE)
017600060420     C*
017700060420     C* Inizializzo la DS d ridefinizione elemento d schiera VARIE
017800060420     C                   clear                   dsVARIA
017900060420     C*
018000060420     C* Valorizzo quindi l'elemento d schiera
018100060420     C                   eval      skVARIE(jVARIE) = dsVARIA
018200060420     C*
018300060420     C* Incremento il contatore d schiera VARIE
018400060420     C                   add       1             jVARIE
018500060420     C                   enddo
018600060331     C*
018700060331     C* Quindi imposto la DS x la restituzione dell'output richiesto
018800060420     C                   movea(p)  skVARIE       UL86OOUT
018900060420     C                   z-add     wTotImporti   UL86OTOT
019000030808     C*
019100030808     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300970526     C* *INZSR - OPERAZIONI INIZIALI
019400970526     C*------------------------------------------------------------------------*
019500970526     C     *INZSR        BEGSR
019600970526     C*---
019700970526     C* RICEVIMENTO PARAMETRI
019800970526     C*---
019900970526     C     *ENTRY        PLIST
020000060331     C                   PARM                    TRUL86DS
020100970526     C*---
020200991214     C* CHIAVI DI LETTURA
020300991214     C*---
020400000810     C*
020500060331     C* LETTURA titas30c - COMPLETA
020600060331     C     KEYtas30_C    KLIST
020700060331     C                   KFLD                    UL86IAAS
020800060331     C                   KFLD                    UL86ILNP
020900060331     C                   KFLD                    UL86INRS
021000060331     C                   KFLD                    UL86INSP
021100060331     C                   KFLD                    UL86ITBL
021200991214     C*
021300970526     C                   ENDSR
