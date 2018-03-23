000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030709     FTITAS30C  IF   E           K DISK
000500030709     FTITAA30C  IF   E           K DISK
000600091201     FTITA430C  IF   E           K DISK
000700090325     Ftivgdtmp  uF   E             DISK
000800091201     FTIVAWWWT  UF A E             DISK    commit
000900030709     D*------------------
001000030709     D* DS REPERIMENTO DATI CLIENTE
001100030709     D*-------------------
001200030709     D BS69DS        E DS                  EXTNAME(TIBS69DS)
001300030709     D ACODS         E DS                  EXTNAME(CNACO00F)
001400030709     D INDDS         E DS                  EXTNAME(CNIND00F)
001500030709     D CLPDS         E DS                  EXTNAME(CNCLP00F)
001600030709     D CLSDS         E DS                  EXTNAME(FNCLS00F)
001700991027
001800991027     D prmppt          s             50
001900991027     D prmesito        s              1
002000991027     D wrkesito        s                   like(prmesito)
002100090325     D tivafds       e ds                  extname(tivaf00t)
002200090325     D dsvafa        e ds                  extname(tivafawt)
002300070316     D sepFld          s              1
002400071205
002500071205     D wPKB            s              8
002600071205     D wVLF            s              6
002700071205     D wQFT            s             14
002800071205     D wPOR            s             12
002900071205     D wVA1            s             12
003000071205     D wVA2            s             12
003100071205     D wVA3            s             12
003200071205     D wVA4            s             12
003300071205     D wVA5            s             12
003400071205     D wVA6            s             12
003500071205     D wVA7            s             12
003600071205     D wVA8            s             12
003700071205     D wVAX            s             12
003800071205     D wIMV            s             14
003900071205     D wIAS            s             14
004000071205     D wPKC            s              8
004100071205     D wVLC            s              6
004200091201     D wNOT            s                   like(TA4NOT)
004300091201     D wNT2            s                   like(TA4NOT)
004400991027
004500030908     C                   EXSR      chkRECFILMBR
004600991027     C                   EXSR      traduci
004700921023     C                   SETON                                        LR
004800030908
004900030908     C     chkRECFILMBR  BEGSR
005000030908     C*
005100091201     C                   READ      TIVAWWWT                               55
005200030908     C*
005300030908     C                   ENDSR
005400991027
005500991027     C     traduci       BEGSR
005600030325     C*
005700030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005800090427     C* (nomi campi)
005900030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
006000091201     C                   EVAL      VAWDTA =
006100090427     C                             '"VAFAAS"'                           +sepFld+
006200090427     C                             '"VAFLNP"'                           +sepFld+
006300090427     C                             '"VAFNRS"'                           +sepFld+
006400090427     C                             '"VAFNSP"'                           +sepFld+
006500090427     C                             '"VAFTBL"'                           +sepFld+
006600090427     C                             '"VAFMGS"'                           +sepFld+
006700090427     C                             '"VAFKSC"'                           +sepFld+
006800090427     C                             '"VAFLNA"'                           +sepFld+
006900090427     C                             '"VAFNCL"'                           +sepFld+
007000090427     C                             '"VAFPKB"'                           +sepFld+
007100090427     C                             '"VAFVLF"'                           +sepFld+
007200090427     C                             '"VAFQFT"'                           +sepFld+
007300090427     C                             '"VAFPOR"'                           +sepFld+
007400090427     C                             '"VAFSV1"'                           +sepFld+
007500090427     C                             '"VAFVA1"'                           +sepFld+
007600090427     C                             '"VAFSV2"'                           +sepFld+
007700090427     C                             '"VAFVA2"'                           +sepFld+
007800090427     C                             '"VAFSV3"'                           +sepFld+
007900090427     C                             '"VAFVA3"'                           +sepFld+
008000090427     C                             '"VAFSV4"'                           +sepFld+
008100090427     C                             '"VAFVA4"'                           +sepFld+
008200090427     C                             '"VAFSV5"'                           +sepFld+
008300090427     C                             '"VAFVA5"'                           +sepFld+
008400090427     C                             '"VAFSV6"'                           +sepFld+
008500090427     C                             '"VAFVA6"'                           +sepFld+
008600090427     C                             '"VAFSV7"'                           +sepFld+
008700090427     C                             '"VAFVA7"'                           +sepFld+
008800090427     C                             '"VAFSV8"'                           +sepFld+
008900090427     C                             '"VAFVA8"'                           +sepFld+
009000090427     C                             '"VAFVAX"'                           +sepFld+
009100090427     C                             '"VAFIMV"'                           +sepFld+
009200090427     C                             '"VAFDFT"'                           +sepFld+
009300090427     C                             '"VAFNFT"'                           +sepFld+
009400090427     C                             '"VAFFIV"'                           +sepFld+
009500090427     C                             '"VAFDIV"'                           +sepFld+
009600090427     C                             '"VAFDRT"'                           +sepFld+
009700090427     C                             '"VAFRMN"'                           +sepFld+
009800090427     C                             '"VAFRMA"'                           +sepFld+
009900090427     C                             '"VAFRMX"'                           +sepFld+
010000090427     C                             '"VAFFTC"'                           +sepFld+
010100090427     C                             '"VAFTC2"'                           +sepFld+
010200090427     C                             '"VAFTSP"'                           +sepFld+
010300090427     C                             '"VAFFAP"'                           +sepFld+
010400090427     C                             '"VAFFIN"'                           +sepFld+
010500090427     C                             '"VAFCTR"'                           +sepFld+
010600090427     C                             '"VAFNAS"'                           +sepFld+
010700090427     C                             '"VAFCTS"'                           +sepFld+
010800090427     C                             '"VAFCAD"'                           +sepFld+
010900090427     C                             '"VAFLOD"'                           +sepFld+
011000090427     C                             '"VAFPRD"'                           +sepFld+
011100090427     C                             '"VAFNZD"'                           +sepFld+
011200090427     C                             '"VAFVAS"'                           +sepFld+
011300090427     C                             '"VAFIAS"'                           +sepFld+
011400090427     C                             '"VAFFPC"'                           +sepFld+
011500090427     C                             '"VAFPKC"'                           +sepFld+
011600090427     C                             '"VAFFVC"'                           +sepFld+
011700090427     C                             '"VAFVLC"'                           +sepFld+
011800090427     C                             '"VAFRSD"'                           +sepFld+
011900090427     C                             '"VAFIND"'                           +sepFld+
012000090427     C                             '"VAFRSM"'                           +sepFld+
012100090427     C                             '"VAFINM"'                           +sepFld+
012200090427     C                             '"VAFCAM"'                           +sepFld+
012300090427     C                             '"VAFLOM"'                           +sepFld+
012400090427     C                             '"VAFPRM"'                           +sepFld+
012500090427     C                             '"VAFNZM"'                           +sepFld+
012600090427     C                             '"VAFRMO"'                           +sepFld+
012700090427     C                             '"VAFCMO"'                           +sepFld+
012800091201     C                             '"VAFNMO"'                           +sepFld+
012900091201     C                             '"VAFNOT"'                           +sepFld+
013000091201     C                             '"VAFNT2"'
013100030325     C*
013200091201     C                   WRITE     TIVAW000
013300030325     C                   ENDIF
013400090427     C*
013500090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013600090427     C* (descrizione campi)
013700090427     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
013800091201     C                   EVAL      VAWDTA =
013900090427     C                             '"ANNO SPEDIZIONE"'                  +sepFld+
014000090427     C                             '"P.O. PARTENZA"'                    +sepFld+
014100090427     C                             '"NUMERO SERIE"'                     +sepFld+
014200090427     C                             '"NUMERO SPEDIZIONE"'                +sepFld+
014300090427     C                             '"TIPO BOLLA"'                       +sepFld+
014400090427     C                             '"MESE/GIORNO SPEDIZIONE"'           +sepFld+
014500090427     C                             '"CODICE CLIENTE"'                   +sepFld+
014600090427     C                             '"P.O. ARRIVO"'                      +sepFld+
014700090427     C                             '"NUMERO COLLI"'                     +sepFld+
014800090427     C                             '"PESO BOLLETTATO IN KG."'           +sepFld+
014900090427     C                             '"VOLUME DA FATTURARE"'              +sepFld+
015000090427     C                             '"QUANTITA DA FATTURARE"'            +sepFld+
015100090427     C                             '"PORTO"'                            +sepFld+
015200090427     C                             '"SIGLA VARIA 1"'                    +sepFld+
015300090427     C                             '"VARIA 1"'                          +sepFld+
015400090427     C                             '"SIGLA VARIA 2"'                    +sepFld+
015500090427     C                             '"VARIA 2"'                          +sepFld+
015600090427     C                             '"SIGLA VARIA 3"'                    +sepFld+
015700090427     C                             '"VARIA 3"'                          +sepFld+
015800090427     C                             '"SIGLA VARIA 4"'                    +sepFld+
015900090427     C                             '"VARIA 4"'                          +sepFld+
016000090427     C                             '"SIGLA VARIA 5"'                    +sepFld+
016100090427     C                             '"VARIA 5"'                          +sepFld+
016200090427     C                             '"SIGLA VARIA 6"'                    +sepFld+
016300090427     C                             '"VARIA 6"'                          +sepFld+
016400090427     C                             '"SIGLA VARIA 7"'                    +sepFld+
016500090427     C                             '"VARIA 7"'                          +sepFld+
016600090427     C                             '"SIGLA VARIA 8"'                    +sepFld+
016700090427     C                             '"VARIA 8"'                          +sepFld+
016800090427     C                             '"CUMULATIVO ALTRE VARIE"'           +sepFld+
016900090427     C                             '"IMPONIBILE IVA"'                   +sepFld+
017000090427     C                             '"DATA FATTURA (AMG)"'               +sepFld+
017100090427     C                             '"NUMERO FATTURA"'                   +sepFld+
017200090427     C                             '"NUMERO REGISTRO FATTURA"'          +sepFld+
017300090427     C                             '"VALUTA FATTURA"'                   +sepFld+
017400090427     C                             '"DATA RITIRO (AMG)"'                +sepFld+
017500090427     C                             '"RIFERIM.MITTENTE NUMERICO"'        +sepFld+
017600090427     C                             '"RIFERIM.MITTENTE ALFABETICO"'      +sepFld+
017700090427     C                             '"ALTRO RIFERIM.MITTENTE"'           +sepFld+
017800090427     C                             '"1 CONSEGNA PARTICOLARE"'           +sepFld+
017900090427     C                             '"2 CONSEGNA PARTICOLARE"'           +sepFld+
018000090427     C                             '"TIPO SERVIZIO SPEDIZIONE"'         +sepFld+
018100090427     C                             '"TIPO ANTEPORTO"'                   +sepFld+
018200090427     C                             '"TIPO INOLTRO"'                     +sepFld+
018300090427     C                             '"CODICE TARIFFA"'                   +sepFld+
018400090427     C                             '"NATURA MERCE"'                     +sepFld+
018500090427     C                             '"COD.TASSAZIONE DESTINATARIO"'      +sepFld+
018600090427     C                             '"CAP DESTINATARIO"'                 +sepFld+
018700090427     C                             '"LOCALITA DESTINATARIO"'            +sepFld+
018800090427     C                             '"PROVINCIA DESTINATARIO"'           +sepFld+
018900090427     C                             '"NAZIONE DESTINATARIO"'             +sepFld+
019000090427     C                             '"VALUTA IMPORTO DA ASSICURARE"'     +sepFld+
019100090427     C                             '"IMPORTO DA ASSICURARE"'            +sepFld+
019200090427     C                             '"TIPO PESO RILEVATO"'               +sepFld+
019300090427     C                             '"PESO RILEVATO IN KG."'             +sepFld+
019400090427     C                             '"TIPO VOLUME RILEVATO"'             +sepFld+
019500090427     C                             '"VOLUME RILEVATO"'                  +sepFld+
019600090427     C                             '"RAGIONE SOCIALE DESTINATARIO"'     +sepFld+
019700090427     C                             '"INDIRIZZO DESTINATARIO"'           +sepFld+
019800090427     C                             '"RAGIONE SOCIALE MITTENTE"'         +sepFld+
019900090427     C                             '"INDIRIZZO MITTENTE"'               +sepFld+
020000090427     C                             '"CAP MITTENTE"'                     +sepFld+
020100090427     C                             '"LOCALITA MITTENTE"'                +sepFld+
020200090427     C                             '"PROVINCIA MITTENTE"'               +sepFld+
020300090427     C                             '"NAZIONE MITTENTE"'                 +sepFld+
020400090427     C                             '"RAG. SOC. MITTENTE ORIGINALE"'     +sepFld+
020500090427     C                             '"CAP MITTENTE ORIGINALE"'           +sepFld+
020600091201     C                             '"NAZIONE MITTENTE ORIGINALE"'       +sepFld+
020700091201     C                             '"NOTE DI CONSGENA"'                 +sepFld+
020800091201     C                             '"NOTE DI CONSGENA AGGIUNTIVE"'
020900090427     C*
021000091201     C                   WRITE     TIVAW000
021100090427     C                   ENDIF
021200090427     C*
021300090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
021400090427     C* (descrizione campi in inglese)
021500090427     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
021600091201     C                   EVAL      VAWDTA =
021700090427     C                             '"YEAR OF SHIPMENT"'                 +sepFld+
021800090427     C                             '"OP. DEPART. POINT"'                +sepFld+
021900090427     C                             '"SERIAL NUMBER"'                    +sepFld+
022000090427     C                             '"SHIPMENT NUMBER"'                  +sepFld+
022100090427     C                             '"DELIV. NOTE TYPE"'                 +sepFld+
022200090427     C                             '"SHIPMENT MONTH/DAY"'               +sepFld+
022300090427     C                             '"CLIENT CODE"'                      +sepFld+
022400090427     C                             '"OP. DEST. POINT"'                  +sepFld+
022500090427     C                             '"NO. OF PACKS"'                     +sepFld+
022600090427     C                             '"DATA ENTRY WEIGHT/KG"'             +sepFld+
022700090427     C                             '"VOLUME TO INVOICE"'                +sepFld+
022800090427     C                             '"QUANTITY TO INVOICE"'              +sepFld+
022900090427     C                             '"FREIGHTS"'                         +sepFld+
023000090427     C                             '"MISC. SYMBOL 1"'                   +sepFld+
023100090427     C                             '"MISC. 1"'                          +sepFld+
023200090427     C                             '"MISC. SYMBOL 2"'                   +sepFld+
023300090427     C                             '"MISC. 2"'                          +sepFld+
023400090427     C                             '"MISC. SYMBOL 3"'                   +sepFld+
023500090427     C                             '"MISC. 3"'                          +sepFld+
023600090427     C                             '"MISC. SYMBOL 4"'                   +sepFld+
023700090427     C                             '"MISC. 4"'                          +sepFld+
023800090427     C                             '"MISC. SYMBOL 5"'                   +sepFld+
023900090427     C                             '"MISC. 5"'                          +sepFld+
024000090427     C                             '"MISC. SYMBOL 6"'                   +sepFld+
024100090427     C                             '"MISC. 6"'                          +sepFld+
024200090427     C                             '"MISC. SYMBOL 7"'                   +sepFld+
024300090427     C                             '"MISC. 7"'                          +sepFld+
024400090427     C                             '"MISC. SYMBOL 8"'                   +sepFld+
024500090427     C                             '"MISC. 8"'                          +sepFld+
024600090427     C                             '"OTHER MISC. SUM"'                  +sepFld+
024700090427     C                             '"VAT"'                              +sepFld+
024800090427     C                             '"INVOICE DATE"'                     +sepFld+
024900090427     C                             '"INVOICE NUMBER"'                   +sepFld+
025000090427     C                             '"INVOICE REG. NUMBER"'              +sepFld+
025100090427     C                             '"INVOICE CURRENCY"'                 +sepFld+
025200090427     C                             '"COLLECTION DATE (YMD)"'            +sepFld+
025300090427     C                             '"NUMERICAL SENDER REF."'            +sepFld+
025400090427     C                             '"ALPHAB. SENDER REF."'              +sepFld+
025500090427     C                             '"OTHER SENDER REF."'                +sepFld+
025600090427     C                             '"1 SPECIAL DELIVERY"'               +sepFld+
025700090427     C                             '"2 SPECIAL DELIVERY"'               +sepFld+
025800090427     C                             '"NOTE SERVICE TYPE"'                +sepFld+
025900090427     C                             '"FLAG COLLECTION IN PROVINC"'       +sepFld+
026000090427     C                             '"FLAG DELIVERY IN PROVINCE"'        +sepFld+
026100090427     C                             '"TARIFF CODE"'                      +sepFld+
026200090427     C                             '"GOODS TYPE"'                       +sepFld+
026300090427     C                             '"DEST. TAXATION CODE"'              +sepFld+
026400090427     C                             '"DEST. P.CODE"'                     +sepFld+
026500090427     C                             '"DEST. AREA"'                       +sepFld+
026600090427     C                             '"DEST. MUNICIP."'                   +sepFld+
026700090427     C                             '"DEST. NATION"'                     +sepFld+
026800090427     C                             '"CURRENCY OF SUM TO INSURE"'        +sepFld+
026900090427     C                             '"SUM TO INSURE"'                    +sepFld+
027000090427     C                             '"TYPE OF WEIGHT READ"'              +sepFld+
027100090427     C                             '"WEIGHT READ IN KG."'               +sepFld+
027200090427     C                             '"TYPE OF VOLUME READ"'              +sepFld+
027300090427     C                             '"VOLUME READ"'                      +sepFld+
027400090427     C                             '"CONSIGNEE COMPANY NAME"'           +sepFld+
027500090427     C                             '"CONSIGNEE ADDRESS"'                +sepFld+
027600090427     C                             '"SENDER COMPANY NAME"'              +sepFld+
027700090427     C                             '"SENDER ADDRESS"'                   +sepFld+
027800090427     C                             '"SENDER ZIP CODE"'                  +sepFld+
027900090427     C                             '"SENDER TOWN"'                      +sepFld+
028000090427     C                             '"SENDER PROVINCE"'                  +sepFld+
028100090427     C                             '"SENDER COUNTRY"'                   +sepFld+
028200090427     C                             '"COMPANY NAME OF ORIGINAL SENDER"'  +sepFld+
028300090427     C                             '"ORIGINAL SENDER ZIP CODE"'         +sepFld+
028400091201     C                             '"ORIGINAL SENDER COUNTRY"'          +sepFld+
028500091201     C                             '"DELIVERY NOTES"'                   +sepFld+
028600091201     C                             '"ADDITIONAL DELIVERY NOTES"'
028700090427     C*
028800091201     C                   WRITE     TIVAW000
028900090427     C                   ENDIF
029000991027     C*
029100090325     C                   READ      tivgdtmp
029200090325     C                   DOW       not %eof
029300090325     c                   movel     vgddta        tivafds
029400041220     C*
029500041220     C* Inizializzo i campi "estensione"
029600041220     C                   EVAL      VHFRSD = *blanks
029700041220     C                   EVAL      VHFIND = *blanks
029800041220     C                   EVAL      VHFRSM = *blanks
029900041220     C                   EVAL      VHFINM = *blanks
030000041220     C                   EVAL      VHFCAM = *blanks
030100041220     C                   EVAL      VHFLOM = *blanks
030200041220     C                   EVAL      VHFPRM = *blanks
030300041220     C                   EVAL      VHFNZM = *blanks
030400041220     C                   EVAL      VHFRMO = *blanks
030500041220     C                   EVAL      VHFCMO = *blanks
030600041220     C                   EVAL      VHFNMO = *blanks
030700091201     C                   EVAL      wNOT   = *blanks
030800091201     C                   EVAL      wNT2   = *blanks
030900030709     C*
031000030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
031100030709     C     KEYtas30C     CHAIN     titas30c
031200030709     C                   IF        %found(titas30c)
031300030709     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
031400030709     C                   EVAL      VHFRSD = TASRSD
031500030709     C                   EVAL      VHFIND = TASIND
031600030709     C* X reperire i dati del mittente verifico il codice del cliente mittente:
031700030709     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
031800030709     C                   MOVE(P)   tasCCM        wCLI              4 0
031900030709     C                   IF        wCLI =    0 OR
032000030709     C                             wCLI = 8888 OR
032100030709     C                             wCLI = 9999
032200030709     C                   EVAL      taaTRC = 'M'
032300030709     C     KEYtaa30C     CHAIN     titaa30c
032400030709     C                   IF        %found(titaa30c)
032500030709     C                   EVAL      VHFRSM = TAARSC
032600030709     C                   EVAL      VHFINM = TAAIND
032700030709     C                   EVAL      VHFCAM = TAACAP
032800030709     C                   EVAL      VHFLOM = TAALOC
032900030709     C                   EVAL      VHFPRM = TAAPRV
033000030709     C                   EVAL      VHFNZM = TAANAZ
033100030709     C                   ENDIF
033200030709     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
033300030709     C                   ELSE
033400030709     C                   CLEAR                   BS69DS
033500030709     C                   CLEAR                   ACODS
033600030709     C                   CLEAR                   INDDS
033700030709     C                   CLEAR                   CLPDS
033800030709     C                   CLEAR                   CLSDS
033900030709     C                   Z-ADD     tasCCM        I69KAC
034000030709     C                   Z-ADD     tasCCM        I69KIN
034100030709     C                   CALL      'TIBS69R'
034200030709     C                   PARM                    BS69DS
034300030709     C                   PARM                    ACODS
034400030709     C                   PARM                    INDDS
034500030709     C                   PARM                    CLPDS
034600030709     C                   PARM                    CLSDS
034700030709     C     O69ERR        IFNE      '1'
034800030709     C                   EVAL      VHFRSM = ACORAG
034900030709     C                   EVAL      VHFINM = INDVIA
035000030709     C                   MOVEL(P)  INDCAP        VHFCAM
035100030709     C                   EVAL      VHFLOM = INDCIT
035200030709     C                   EVAL      VHFPRM = INDPRV
035300030709     C                   EVAL      VHFNZM = INDSTA
035400030709     C                   ENDIF
035500030709     C*
035600030709     C                   ENDIF
035700030709     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
035800030709     C                   EVAL      taaTRC = 'O'
035900030709     C     KEYtaa30C     CHAIN     titaa30c
036000030709     C                   IF        %found(titaa30c)
036100030709     C                   EVAL      VHFRMO = TAARSC
036200030709     C                   EVAL      VHFCMO = TAACAP
036300030709     C                   EVAL      VHFNMO = TAANAZ
036400030709     C                   ENDIF
036500091201     C*
036600091201     C* Reperisco le note partenza 1 e 2
036700091201     C                   EVAL      ta4TRC = '8'
036800091201     C     KEYta430C     CHAIN     tita430c
036900091201     C                   IF        %found(tita430c)
037000091201     C                   EVAL      wNOT = TA4NOT
037100091201     C                   ENDIF
037200091201     C*
037300091201     C                   EVAL      ta4TRC = '9'
037400091201     C     KEYta430C     CHAIN     tita430c
037500091201     C                   IF        %found(tita430c)
037600091201     C                   EVAL      wNT2 = TA4NOT
037700091201     C                   ENDIF
037800091201     C*
037900030709     C                   ENDIF
038000071205     C*
038100071205     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
038200071205     C  N60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 , '))
038300071205     C  N60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 ,   '))
038400071205     C  N60              EVAL      wQFT = %trim(%editw(VAFQFT:'        0 ,   '))
038500071205     C  N60              EVAL      wPOR = %trim(%editw(VAFPOR:'      0 ,   '))
038600071205     C  N60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 ,   '))
038700071205     C  N60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 ,   '))
038800071205     C  N60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 ,   '))
038900071205     C  N60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 ,   '))
039000071205     C  N60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 ,   '))
039100071205     C  N60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 ,   '))
039200071205     C  N60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 ,   '))
039300071205     C  N60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 ,   '))
039400071205     C  N60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 ,   '))
039500071205     C  N60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 ,   '))
039600071205     C  N60              EVAL      wIAS = %trim(%editw(VAFIAS:'        0 ,   '))
039700071205     C  N60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 , '))
039800071205     C  N60              EVAL      wVLC = %trim(%editw(VAFVLC:'0 ,   '))
039900071205     C   60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 . '))
040000071205     C   60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 .   '))
040100071205     C   60              EVAL      wQFT = %trim(%editw(VAFQFT:'        0 .   '))
040200071205     C   60              EVAL      wPOR = %trim(%editw(VAFPOR:'      0 .   '))
040300071205     C   60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 .   '))
040400071205     C   60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 .   '))
040500071205     C   60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 .   '))
040600071205     C   60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 .   '))
040700071205     C   60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 .   '))
040800071205     C   60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 .   '))
040900071205     C   60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 .   '))
041000071205     C   60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 .   '))
041100071205     C   60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 .   '))
041200071205     C   60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 .   '))
041300071205     C   60              EVAL      wIAS = %trim(%editw(VAFIAS:'        0 .   '))
041400071205     C   60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 . '))
041500071205     C   60              EVAL      wVLC = %trim(%editw(VAFVLC:'0 .   '))
041600030325     C*
041700091201     C                   EVAL      VAWDTA =  %trim(%editc(VAFAAS:'Q'))+sepFld+
041800070316     C                                       %trim(%editc(VAFLNP:'Q'))+sepFld+
041900070316     C                                       %trim(%editc(VAFNRS:'Q'))+sepFld+
042000070316     C                                       %trim(%editc(VAFNSP:'Q'))+sepFld+
042100070316     C                                          '"'+%trim(VAFTBL)+'"'+sepFld+
042200070316     C                                       %trim(%editc(VAFMGS:'Q'))+sepFld+
042300070316     C                                       %trim(%editc(VAFKSC:'Q'))+sepFld+
042400070316     C                                       %trim(%editc(VAFLNA:'Q'))+sepFld+
042500070316     C                                       %trim(%editc(VAFNCL:'Q'))+sepFld+
042600071205     C                                                %trim(wPKB)+sepFld+
042700071205     C                                                %trim(wVLF)+sepFld+
042800071205     C                                                %trim(wQFT)+sepFld+
042900071205     C                                                %trim(wPOR)+sepFld+
043000070316     C                                          '"'+%trim(VAFSV1)+'"'+sepFld+
043100071205     C                                                %trim(wVA1)+sepFld+
043200070316     C                                          '"'+%trim(VAFSV2)+'"'+sepFld+
043300071205     C                                                %trim(wVA2)+sepFld+
043400070316     C                                          '"'+%trim(VAFSV3)+'"'+sepFld+
043500071205     C                                                %trim(wVA3)+sepFld+
043600070316     C                                          '"'+%trim(VAFSV4)+'"'+sepFld+
043700071205     C                                                %trim(wVA4)+sepFld+
043800070316     C                                          '"'+%trim(VAFSV5)+'"'+sepFld+
043900071205     C                                                %trim(wVA5)+sepFld+
044000070316     C                                          '"'+%trim(VAFSV6)+'"'+sepFld+
044100071205     C                                                %trim(wVA6)+sepFld+
044200070316     C                                          '"'+%trim(VAFSV7)+'"'+sepFld+
044300071205     C                                                %trim(wVA7)+sepFld+
044400070316     C                                          '"'+%trim(VAFSV8)+'"'+sepFld+
044500071205     C                                                %trim(wVA8)+sepFld+
044600071205     C                                                %trim(wVAX)+sepFld+
044700071205     C                                                %trim(wIMV)+sepFld+
044800070316     C                                       %trim(%editc(VAFDFT:'Q'))+sepFld+
044900070316     C                                       %trim(%editc(VAFNFT:'Q'))+sepFld+
045000070316     C                                       %trim(%editc(VAFFIV:'Q'))+sepFld+
045100070316     C                                          '"'+%trim(VAFDIV)+'"'+sepFld+
045200070316     C                                       %trim(%editc(VAFDRT:'Q'))+sepFld+
045300070316     C                                       %trim(%editc(VAFRMN:'Q'))+sepFld+
045400070316     C                                          '"'+%trim(VAFRMA)+'"'+sepFld+
045500070316     C                                          '"'+%trim(VAFRMX)+'"'+sepFld+
045600070316     C                                          '"'+%trim(VAFFTC)+'"'+sepFld+
045700070316     C                                          '"'+%trim(VAFTC2)+'"'+sepFld+
045800070316     C                                          '"'+%trim(VAFTSP)+'"'+sepFld+
045900070316     C                                          '"'+%trim(VAFFAP)+'"'+sepFld+
046000070316     C                                          '"'+%trim(VAFFIN)+'"'+sepFld+
046100070316     C                                       %trim(%editc(VAFCTR:'Q'))+sepFld+
046200070316     C                                          '"'+%trim(VAFNAS)+'"'+sepFld+
046300070316     C                                          '"'+%trim(VAFCTS)+'"'+sepFld+
046400070316     C                                          '"'+%trim(VAFCAD)+'"'+sepFld+
046500070316     C                                          '"'+%trim(VAFLOD)+'"'+sepFld+
046600070316     C                                          '"'+%trim(VAFPRD)+'"'+sepFld+
046700070316     C                                          '"'+%trim(VAFNZD)+'"'+sepFld+
046800070316     C                                          '"'+%trim(VAFVAS)+'"'+sepFld+
046900071205     C                                                %trim(wIAS)+sepFld+
047000070316     C                                          '"'+%trim(VAFFPC)+'"'+sepFld+
047100071205     C                                                %trim(wPKC)+sepFld+
047200070316     C                                          '"'+%trim(VAFFVC)+'"'+sepFld+
047300071205     C                                                %trim(wVLC)+sepFld+
047400070316     C                                          '"'+%trim(VHFRSD)+'"'+sepFld+
047500070316     C                                          '"'+%trim(VHFIND)+'"'+sepFld+
047600070316     C                                          '"'+%trim(VHFRSM)+'"'+sepFld+
047700070316     C                                          '"'+%trim(VHFINM)+'"'+sepFld+
047800070316     C                                          '"'+%trim(VHFCAM)+'"'+sepFld+
047900070316     C                                          '"'+%trim(VHFLOM)+'"'+sepFld+
048000070316     C                                          '"'+%trim(VHFPRM)+'"'+sepFld+
048100070316     C                                          '"'+%trim(VHFNZM)+'"'+sepFld+
048200070316     C                                          '"'+%trim(VHFRMO)+'"'+sepFld+
048300070316     C                                          '"'+%trim(VHFCMO)+'"'+sepFld+
048400091201     C                                          '"'+%trim(VHFNMO)+'"'+sepFld+
048500091201     C                                          '"'+%trim(wNOT)+'"'  +sepFld+
048600091201     C                                          '"'+%trim(wNT2)+'"'
048700930409     C*
048800091201     C                   WRITE     TIVAW000
048900090325     C                   delete    tivgd000
049000991027     C*
049100090325     C                   READ      tivgdtmp
049200030325     C                   ENDDO
049300991027     C*
049400030325     C                   EVAL      wrkesito = '0'
049500991027     C*
049600910830     C                   ENDSR
049700991027
049800991027      /TITLE Operazioni iniziali.
049900991027     C     *inzsr        BEGSR
050000991027     C*
050100991027     C     *ENTRY        PLIST
050200991027     C                   parm                    prmppt
050300991027     C     wrkesito      parm      wrkesito      prmesito
050400070316     C*
050500070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
050600070316     C                   eval      sepFld = %subst(prmppt:2:1)
050700070316     C                   if        sepFld = *blanks
050800070316     C                   eval      sepFld = ','
050900070316     C                   endif
051000071205     C*
051100071205     C* Verifico se richiesto un particolare carattere d separatore decimale
051200071205     C                   SETON                                        60
051300071205     C                   IF        %subst(prmppt:3:1) = ','
051400071205     C                   SETOFF                                       60
051500071205     C                   ENDIF
051600030709     C*
051700030709     C* Definizione chiavi
051800030709     C*
051900030709     C* Chiave su TITAS30C - Completa
052000030709     C     KEYtas30C     KLIST
052100030709     C                   KFLD                    vafAAS
052200030709     C                   KFLD                    vafLNP
052300030709     C                   KFLD                    vafNRS
052400030709     C                   KFLD                    vafNSP
052500030709     C                   KFLD                    vafTBL
052600030709     C*
052700030709     C* Chiave su TITAA30C - Completa
052800030709     C     KEYtaa30C     KLIST
052900030709     C                   KFLD                    vafAAS
053000030709     C                   KFLD                    vafLNP
053100030709     C                   KFLD                    vafNRS
053200030709     C                   KFLD                    vafNSP
053300030709     C                   KFLD                    taaTRC
053400091201     C*
053500091201     C* Chiave su TITA430C - Completa
053600091201     C     KEYta430C     KLIST
053700091201     C                   KFLD                    vafAAS
053800091201     C                   KFLD                    vafLNP
053900091201     C                   KFLD                    vafNRS
054000091201     C                   KFLD                    vafNSP
054100091201     C                   KFLD                    ta4TRC
054200991027     C*
054300991027     C                   ENDSR
