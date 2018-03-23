000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400060413     FTITAS31C  IF   E           K DISK
000500060418     FTITA430C  IF   E           K DISK
000600060420     FTITAA30C  IF   E           K DISK
000700060418     FTIGCP51L  IF   E           K DISK
000800060413     FTNCSB03L  IF   E           K DISK
000900060420     FFIAR531C  IF   E           K DISK
001000060418     FTABEL00F  IF   E           K DISK
001100060331     FTIVAWWWT  UF A E             DISK
001200060421
001300060421     D*------------
001400060421     D* DECODIFICHE COSTANTI
001500060421     D*------------
001600060421     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001700060331
001800060418     D****
001900060418     D* SCHIERE DI WRK
002000060418     D****
002100060418     D skVAF           s              7  0 inz dim(1000)
002200060418     D jVAF            s              4  0 inz
002300060419
002400060419     D*------------------
002500060419     D* DS "XSRDA8" - CONTROLLA DATA (8)
002600060419     D*------------------
002700060419     D WLBDA8          DS                  INZ
002800060419     D  G08DAT                 1      8  0
002900060419     D  G08INV                 9     16  0
003000060419     D  G08ERR                17     17
003100060419     D  G08TGI                18     22  0
003200060420
003300060420     D*------------------
003400060420     D* DS REPERIMENTO DATI CLIENTE
003500060420     D*-------------------
003600060420     D BS69DS        E DS                  EXTNAME(TIBS69DS)
003700060420     D ACODS         E DS                  EXTNAME(CNACO00F)
003800060420     D INDDS         E DS                  EXTNAME(CNIND00F)
003900060420     D CLPDS         E DS                  EXTNAME(CNCLP00F)
004000060420     D CLSDS         E DS                  EXTNAME(FNCLS00F)
004100060419
004200060419     D*------------
004300060420     D* DS ESTERNE VARIE
004400060419     D*------------
004500060420     D DSVAF         E DS                  EXTNAME(TIVAF00T)
004600060420     D DSVAFA        E DS                  EXTNAME(TIVAFAWT)
004700060420     D TRUL86DS      E DS
004800060420     D DAR5BNB       E DS
004900060420     D DSQT1         E DS
005000060420     D DS4C          E DS
005100060420
005200060420     D*------------
005300060420     D* SCHIERE SIGLE E DESCRIZIONI VARIE
005400060420     D*------------
005500060420     D skVARIE         S                   LIKE(dsVARIA) DIM(50) INZ
005600060420     D jVARIE          S              2  0 INZ
005700060420
005800060420     D*------------------
005900060420     D* DS RIDEFINIZIONE OUTPUT ELEMENTO PROCEDURA TRUL86R
006000060420     D*------------------
006100060420     D dsVARIA         DS
006200060420     D  dVARIA_COD                    1
006300060420     D  dVARIA_VAL                   13  3
006400060420
006500060420     D*------------
006600060420     D* DS RIDEFINIZIONE TABELLA "QC" TIPO RECORD "2"
006700060420     D*------------
006800060420     D DSQC2         E DS
006900060419
007000060419     D*------------------
007100060419     D* DS X PASSAGGIO PARAMETRI
007200060419     D*------------------
007300060419     D                 DS
007400060419     D  wPERIODO               1      6  0
007500060419     D  wMM                    5      6  0
007600060419     D  wAAAA                  1      4  0
007700060418
007800060331     D****
007900060331     D* VARIABILI DI WRK
008000060331     D****
008100991027     D prmppt          s             50
008200991027     D prmesito        s              1
008300991027     D wrkesito        s                   like(prmesito)
008400060418     D wKSUVAFa        s              7
008500060418     D wKSUVAF         s              7  0
008600060420     D wAAAAMMa        s              6
008700060420     D wAAAAMM         s              6  0
008800060420     D wDataINZ        s              8  0
008900060420     D wDataFIN        s              8  0
009000060420     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
009100060420     D wTara           s                   like(§QTTPC)
009200060420     D wNtara          s                   like(TASPKF)
009300991027
009400060419     C*
009500060419     C* Verifico ed effettuo considerazioni sui parametri ricevuti in input
009600060419     C                   EXSR      chkPAR
009700060418     C*
009800060418     C* Carico le tabelle occorrenti
009900060419     C                   EXSR      carTAB
010000060418     C*
010100060418     C* Verifico se già presenti dati nel file d output
010200030908     C                   EXSR      chkRECFILMBR
010300060418     C*
010400060418     C* Effettuo elaborazione
010500991027     C                   EXSR      traduci
010600060420     C*
010700060420     C* Effettuo chiamata x chiusura procedure esterne
010800060420     C                   CLEAR                   TRUL86DS
010900060420     C                   EVAL      UL86ITLA = 'C'
011000060420     C                   CALL      'TRUL86R'
011100060420     C                   PARM                    TRUL86DS
011200060418     C*
011300921023     C                   SETON                                        LR
011400060413
011500060413
011600030908
011700030908     C     chkRECFILMBR  BEGSR
011800030908     C*
011900060413     C                   READ      TIVAWWWT                               55
012000030908     C*
012100030908     C                   ENDSR
012200060413
012300060413
012400991027
012500991027     C     traduci       BEGSR
012600030325     C*
012700030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
012800030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
012900060420     C                   EVAL      VAWDTA  = '"VAFAAS"'+','+
013000060420     C                                       '"VAFLNP"'+','+
013100060420     C                                       '"VAFNRS"'+','+
013200060420     C                                       '"VAFNSP"'+','+
013300060420     C                                       '"VAFTBL"'+','+
013400060420     C                                       '"VAFMGS"'+','+
013500060420     C                                       '"VAFKSC"'+','+
013600060420     C                                       '"VAFLNA"'+','+
013700060420     C                                       '"VAFNCL"'+','+
013800060420     C                                       '"VAFPKB"'+','+
013900060420     C                                       '"VAFVLF"'+','+
014000060420     C                                       '"VAFQFT"'+','+
014100060420     C                                       '"VAFPOR"'+','+
014200060420     C                                       '"VAFSV1"'+','+
014300060420     C                                       '"VAFVA1"'+','+
014400060420     C                                       '"VAFSV2"'+','+
014500060420     C                                       '"VAFVA2"'+','+
014600060420     C                                       '"VAFSV3"'+','+
014700060420     C                                       '"VAFVA3"'+','+
014800060420     C                                       '"VAFSV4"'+','+
014900060420     C                                       '"VAFVA4"'+','+
015000060420     C                                       '"VAFSV5"'+','+
015100060420     C                                       '"VAFVA5"'+','+
015200060420     C                                       '"VAFSV6"'+','+
015300060420     C                                       '"VAFVA6"'+','+
015400060420     C                                       '"VAFSV7"'+','+
015500060420     C                                       '"VAFVA7"'+','+
015600060420     C                                       '"VAFSV8"'+','+
015700060420     C                                       '"VAFVA8"'+','+
015800060420     C                                       '"VAFVAX"'+','+
015900060420     C                                       '"VAFIMV"'+','+
016000060420     C                                       '"VAFDFT"'+','+
016100060420     C                                       '"VAFNFT"'+','+
016200060420     C                                       '"VAFFIV"'+','+
016300060420     C                                       '"VAFDIV"'+','+
016400060420     C                                       '"VAFDRT"'+','+
016500060420     C                                       '"VAFRMN"'+','+
016600060420     C                                       '"VAFRMA"'+','+
016700060420     C                                       '"VAFRMX"'+','+
016800060420     C                                       '"VAFFTC"'+','+
016900060420     C                                       '"VAFTC2"'+','+
017000060420     C                                       '"VAFTSP"'+','+
017100060420     C                                       '"VAFFAP"'+','+
017200060420     C                                       '"VAFFIN"'+','+
017300060420     C                                       '"VAFCTR"'+','+
017400060420     C                                       '"VAFNAS"'+','+
017500060420     C                                       '"VAFCTS"'+','+
017600060420     C                                       '"VAFCAD"'+','+
017700060420     C                                       '"VAFLOD"'+','+
017800060420     C                                       '"VAFPRD"'+','+
017900060420     C                                       '"VAFNZD"'+','+
018000060420     C                                       '"VAFVAS"'+','+
018100060420     C                                       '"VAFIAS"'+','+
018200060420     C                                       '"VAFFPC"'+','+
018300060420     C                                       '"VAFPKC"'+','+
018400060420     C                                       '"VAFFVC"'+','+
018500060420     C                                       '"VAFVLC"'+','+
018600060420     C                                       '"VAFRSD"'+','+
018700060420     C                                       '"VAFIND"'+','+
018800060420     C                                       '"VAFRSM"'+','+
018900060420     C                                       '"VAFINM"'+','+
019000060420     C                                       '"VAFCAM"'+','+
019100060420     C                                       '"VAFLOM"'+','+
019200060420     C                                       '"VAFPRM"'+','+
019300060420     C                                       '"VAFNZM"'+','+
019400060420     C                                       '"VAFRMO"'+','+
019500060420     C                                       '"VAFCMO"'+','+
019600060420     C                                       '"VAFNMO"'+','+
019700060418     C                                      '"GIACENZA"'+','+
019800060418     C                                      '"CONS_AI_PIANI"'+','+
019900060418     C                                      '"CONS_APPUNTAMENTO"'+','+
020000060418     C                                      '"CONS_LOCALITA_DISAGIATA"'+','+
020100060418     C                                      '"CONS_ISOLE_MINORI"'+','+
020200060418     C                                      '"CONS_DISAGIATA"'+','+
020300060418     C                                      '"POD_IMAGE"'+','+
020400060418     C                                      '"DIROTTAMENTO"'+','+
020500060418     C                                      '"IMPORTO_CONTRASSEGNO"'+','+
020600060420     C                                      '"DIVISA_CONTRASSEGNO"'
020700030325     C*
020800060413     C                   WRITE     TIVAW000
020900030325     C                   ENDIF
021000991027     C*
021100060418     C* Ciclo x tutti i codici clienti ricondotti al cliente unificante fatture (tab. 4C)
021200060418     C                   EVAL      jVAF = 1
021300060418     C                   DOW       jVAF <= %elem(skVAF)
021400060418     C                   IF        skVAF(jVAF) = *zeros
021500060418     C                   LEAVE
021600060418     C                   ELSE
021700060418     C                   EVAL      tasKSC = skVAF(jVAF)
021800060420     C                   EVAL      tasDFT = wDataINZ
021900060420     C     KEYtas31      SETLL     TITAS31C
022000060413     C                   IF        %found(TITAS31C)
022100060420     C     skVAF(jVAF)   READE     TITAS31C
022200060413     C                   DOW       not %eof(TITAS31C)
022300060418     C*
022400060419     C* Inizializzo il flag che condiziona l'elaborazione del record bolla corrente
022500060419     C                   MOVEL     'S'           wRecOK            1
022600060420     C                   MOVEL     'N'           wLeave            1
022700060419     C*
022800060419     C* Effettuo verifiche validità record
022900060419     C                   EXSR      chkREC
023000060419     C*
023100060419     C* Se record da considerare procedo con le considerazioni
023200060419     C                   IF        wRecOK = 'S'
023300060419     C*
023400060419     C* Re inizializzo il flag che indica se la bolla corrente è da considerare
023500060419     C                   MOVEL     'N'           wRecOK            1
023600060418     C*
023700060418     C* Inizializzo tutti i flag d wrk relativi agli attributi della bolla corrente
023800060418     C                   MOVEL     *blanks       wGIAC             1
023900060418     C                   MOVEL     *blanks       wPIAN             1
024000060418     C                   MOVEL     *blanks       wAPPU             1
024100060418     C                   MOVEL     *blanks       wLDIS             1
024200060418     C                   MOVEL     *blanks       wISOL             1
024300060418     C                   MOVEL     *blanks       wCDIS             1
024400060418     C                   MOVEL     *blanks       wPODI             1
024500060418     C                   MOVEL     *blanks       wDIRO             1
024600060418     C*
024700060418     C* Aggancio il file giacenze della bolla corrente
024800060418     C     KEYgcp51_P    CHAIN     TIGCP51L
024900060418     C                   IF        %found(TIGCP51L)
025000060418     C                   MOVEL     'S'           wGIAC
025100060418     C                   MOVEL     'S'           wRecOK
025200060418     C                   ENDIF
025300060413     C*
025400060413     C* Aggancio il file contrassegni della bolla corrente
025500060421     C                   Z-ADD     *zeros        csbCAS
025600060421     C                   MOVEL     *blanks       csbVCA
025700060413     C     KEYcsb03      CHAIN     TNCSB03L
025800060413     C                   IF        %found(TNCSB03L)
025900060421     C                   IF        csbSTA <> 9
026000060418     C                   MOVEL     'S'           wRecOK
026100060421     C                   ENDIF
026200060418     C                   ENDIF
026300060418     C*
026400060418     C* Verifica se APPUNTAMENTO
026500060418     C                   IF        tasFTC = 'A' or
026600060418     C                             tasTC2 = 'A'
026700060418     C                   MOVEL     'S'           wRecOK
026800060418     C                   MOVEL     'S'           wAPPU
026900060418     C                   ENDIF
027000060418     C*
027100060418     C* Verifica se AI PIANI
027200060418     C                   IF        tasFTC = 'P' or
027300060418     C                             tasTC2 = 'P'
027400060418     C                   MOVEL     'S'           wRecOK
027500060418     C                   MOVEL     'S'           wPIAN
027600060418     C                   ENDIF
027700060418     C*
027800060418     C* Verifica se CONSEGNA DISAGIATA
027900060418     C                   IF        tasFTC = 'X' or
028000060418     C                             tasTC2 = 'X'
028100060418     C                   MOVEL     'S'           wRecOK
028200060418     C                   MOVEL     'S'           wCDIS
028300060418     C                   ENDIF
028400060418     C*
028500060418     C* Verifica se LOCALITA DISAGIATA
028600060418     C                   IF        tasFIN = 'D'
028700060418     C                   MOVEL     'S'           wRecOK
028800060418     C                   MOVEL     'S'           wLDIS
028900060418     C                   ENDIF
029000060418     C*
029100060418     C* Verifica se ISOLA MINORE
029200060418     C                   IF        tasFIN = 'I'
029300060418     C                   MOVEL     'S'           wRecOK
029400060418     C                   MOVEL     'S'           wISOL
029500060418     C                   ENDIF
029600060418     C*
029700060418     C* Verifica se P.O.D. IMAGE
029800060418     C                   IF        tasCBO = 'FI'
029900060418     C                   MOVEL     'S'           wRecOK
030000060418     C                   MOVEL     'S'           wPODI
030100060418     C                   ENDIF
030200060418     C*
030300060418     C* Verifica se DIROTTAMENTO
030400060418     C                   IF        tasCBO = 'F*'
030500060418     C                   MOVEL     'S'           wRecOK
030600060418     C                   MOVEL     'S'           wDIRO
030700060418     C                   ENDIF
030800060418     C*
030900060418     C* Se presente numero fattura considero sempre la bolla
031000060418     C                   IF        tasNFT > *zeros
031100060418     C                   MOVEL     'S'           wRecOK
031200060418     C                   ENDIF
031300060413     C*
031400060413     C* Se tutto ok procedo con la scrittura del buffer d out
031500060420     C                   IF        wRecOK = 'S'
031600060420     C*
031700060420     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
031800060420     C                   EVAL      ta4TRC = 'A'
031900060420     C     KEYta430      CHAIN     TITA430C
032000060420     C                   IF        %found(TITA430C)
032100060420     C                   ELSE
032200060420     C                   CLEAR                   TITA4000
032300060420     C                   CLEAR                   TITA4P00
032400060420     C                   ENDIF
032500060420     C*
032600060420     C* Inizializzo il buffer del file d output e delle ds d ridefinizione dati
032700060420     C                   CLEAR                   TIVAW000
032800060420     C                   CLEAR                   DSVAF
032900060420     C                   CLEAR                   DSVAFA
033000060420     C*
033100060420     C* Valorizzo i dati del destinatario nn già presenti sul TIVAF
033200060420     C                   EVAL      VHFRSD = TASRSD
033300060420     C                   EVAL      VHFIND = TASIND
033400060420     C*
033500060420     C* X reperire i dati del mittente verifico il codice del cliente mittente:
033600060420     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
033700060420     C                   MOVE(P)   tasCCM        wCLI              4 0
033800060420     C                   IF        wCLI =    0 OR
033900060420     C                             wCLI = 8888 OR
034000060420     C                             wCLI = 9999
034100060420     C                   EVAL      taaTRC = 'M'
034200060420     C     KEYtaa30      CHAIN     titaa30c
034300060420     C                   IF        %found(titaa30c)
034400060420     C                   EVAL      VHFRSM = TAARSC
034500060420     C                   EVAL      VHFINM = TAAIND
034600060420     C                   EVAL      VHFCAM = TAACAP
034700060420     C                   EVAL      VHFLOM = TAALOC
034800060420     C                   EVAL      VHFPRM = TAAPRV
034900060420     C                   EVAL      VHFNZM = TAANAZ
035000060420     C                   ENDIF
035100060420     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
035200060420     C                   ELSE
035300060420     C                   CLEAR                   BS69DS
035400060420     C                   CLEAR                   ACODS
035500060420     C                   CLEAR                   INDDS
035600060420     C                   CLEAR                   CLPDS
035700060420     C                   CLEAR                   CLSDS
035800060420     C                   Z-ADD     tasCCM        I69KAC
035900060420     C                   Z-ADD     tasCCM        I69KIN
036000060420     C                   CALL      'TIBS69R'
036100060420     C                   PARM                    BS69DS
036200060420     C                   PARM                    ACODS
036300060420     C                   PARM                    INDDS
036400060420     C                   PARM                    CLPDS
036500060420     C                   PARM                    CLSDS
036600060420     C     O69ERR        IFNE      '1'
036700060420     C                   EVAL      VHFRSM = ACORAG
036800060420     C                   EVAL      VHFINM = INDVIA
036900060420     C                   MOVEL(P)  INDCAP        VHFCAM
037000060420     C                   EVAL      VHFLOM = INDCIT
037100060420     C                   EVAL      VHFPRM = INDPRV
037200060420     C                   EVAL      VHFNZM = INDSTA
037300060420     C                   ENDIF
037400060420     C                   ENDIF
037500060420     C*
037600060420     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
037700060420     C                   EVAL      taaTRC = 'O'
037800060420     C     KEYtaa30      CHAIN     titaa30c
037900060420     C                   IF        %found(titaa30c)
038000060420     C                   EVAL      VHFRMO = TAARSC
038100060420     C                   EVAL      VHFCMO = TAACAP
038200060420     C                   EVAL      VHFNMO = TAANAZ
038300060420     C                   ENDIF
038400060420     C*
038500060420     C* Se particolità varia = 'O' recupero da FIAR5 il numero colli originali
038600060420     C                   IF        %subst(tasGVA:1:1) = 'O'
038700060420     C                   EVAL      ar5TRD = 'BNB'
038800060420     C                   CLEAR                   DAR5BNB
038900060420     C     KEYar531_P    CHAIN     FIAR531C
039000060420     C                   IF        %found(FIAR531C)
039100060420     C                   MOVEL     AR5UNI        DAR5BNB
039200060420     C                   ENDIF
039300060420     C* Colli originali
039400060420     C                   EVAL      VAFNCL = §AR5BCOR
039500060420     C* Colli TITAS
039600060420     C                   ELSE
039700060420     C                   Z-ADD     TASNCL        VAFNCL
039800060420     C                   ENDIF
039900060420     C*
040000060420     C* Eseguo reperimento varie
040100060420     C                   EXSR      repVARIE
040200060420     C*
040300060420     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
040400060420     C                   EXSR      TAStoVAF
040500060420     C*
040600060420     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'Q'))+','+
040700060420     C                                       %trim(%editc(VAFLNP:'Q'))+','+
040800060420     C                                       %trim(%editc(VAFNRS:'Q'))+','+
040900060420     C                                       %trim(%editc(VAFNSP:'Q'))+','+
041000060420     C                                          '"'+%trim(VAFTBL)+'"'+','+
041100060420     C                                       %trim(%editc(VAFMGS:'Q'))+','+
041200060420     C                                       %trim(%editc(VAFKSC:'Q'))+','+
041300060420     C                                       %trim(%editc(VAFLNA:'Q'))+','+
041400060420     C                                       %trim(%editc(VAFNCL:'Q'))+','+
041500060420     C                                       %trim(%editc(VAFPKB:'Q'))+','+
041600060420     C                                       %trim(%editc(VAFVLF:'Q'))+','+
041700060420     C                                       %trim(%editc(VAFQFT:'Q'))+','+
041800060420     C                                       %trim(%editc(VAFPOR:'Q'))+','+
041900060420     C                                          '"'+%trim(VAFSV1)+'"'+','+
042000060420     C                                       %trim(%editc(VAFVA1:'Q'))+','+
042100060420     C                                          '"'+%trim(VAFSV2)+'"'+','+
042200060420     C                                       %trim(%editc(VAFVA2:'Q'))+','+
042300060420     C                                          '"'+%trim(VAFSV3)+'"'+','+
042400060420     C                                       %trim(%editc(VAFVA3:'Q'))+','+
042500060420     C                                          '"'+%trim(VAFSV4)+'"'+','+
042600060420     C                                       %trim(%editc(VAFVA4:'Q'))+','+
042700060420     C                                          '"'+%trim(VAFSV5)+'"'+','+
042800060420     C                                       %trim(%editc(VAFVA5:'Q'))+','+
042900060420     C                                          '"'+%trim(VAFSV6)+'"'+','+
043000060420     C                                       %trim(%editc(VAFVA6:'Q'))+','+
043100060420     C                                          '"'+%trim(VAFSV7)+'"'+','+
043200060420     C                                       %trim(%editc(VAFVA7:'Q'))+','+
043300060420     C                                          '"'+%trim(VAFSV8)+'"'+','+
043400060420     C                                       %trim(%editc(VAFVA8:'Q'))+','+
043500060420     C                                       %trim(%editc(VAFVAX:'Q'))+','+
043600060420     C                                       %trim(%editc(VAFIMV:'Q'))+','+
043700060420     C                                       %trim(%editc(VAFDFT:'Q'))+','+
043800060420     C                                       %trim(%editc(VAFNFT:'Q'))+','+
043900060420     C                                       %trim(%editc(VAFFIV:'Q'))+','+
044000060420     C                                          '"'+%trim(VAFDIV)+'"'+','+
044100060420     C                                       %trim(%editc(VAFDRT:'Q'))+','+
044200060420     C                                       %trim(%editc(VAFRMN:'Q'))+','+
044300060420     C                                          '"'+%trim(VAFRMA)+'"'+','+
044400060420     C                                          '"'+%trim(VAFRMX)+'"'+','+
044500060420     C                                          '"'+%trim(VAFFTC)+'"'+','+
044600060420     C                                          '"'+%trim(VAFTC2)+'"'+','+
044700060420     C                                          '"'+%trim(VAFTSP)+'"'+','+
044800060420     C                                          '"'+%trim(VAFFAP)+'"'+','+
044900060420     C                                          '"'+%trim(VAFFIN)+'"'+','+
045000060420     C                                       %trim(%editc(VAFCTR:'Q'))+','+
045100060420     C                                          '"'+%trim(VAFNAS)+'"'+','+
045200060420     C                                          '"'+%trim(VAFCTS)+'"'+','+
045300060420     C                                          '"'+%trim(VAFCAD)+'"'+','+
045400060420     C                                          '"'+%trim(VAFLOD)+'"'+','+
045500060420     C                                          '"'+%trim(VAFPRD)+'"'+','+
045600060420     C                                          '"'+%trim(VAFNZD)+'"'+','+
045700060420     C                                          '"'+%trim(VAFVAS)+'"'+','+
045800060420     C                                       %trim(%editc(VAFIAS:'Q'))+','+
045900060420     C                                          '"'+%trim(VAFFPC)+'"'+','+
046000060420     C                                       %trim(%editc(VAFPKC:'Q'))+','+
046100060420     C                                          '"'+%trim(VAFFVC)+'"'+','+
046200060420     C                                       %trim(%editc(VAFVLC:'Q'))+','+
046300060420     C                                          '"'+%trim(VHFRSD)+'"'+','+
046400060420     C                                          '"'+%trim(VHFIND)+'"'+','+
046500060420     C                                          '"'+%trim(VHFRSM)+'"'+','+
046600060420     C                                          '"'+%trim(VHFINM)+'"'+','+
046700060420     C                                          '"'+%trim(VHFCAM)+'"'+','+
046800060420     C                                          '"'+%trim(VHFLOM)+'"'+','+
046900060420     C                                          '"'+%trim(VHFPRM)+'"'+','+
047000060420     C                                          '"'+%trim(VHFNZM)+'"'+','+
047100060420     C                                          '"'+%trim(VHFRMO)+'"'+','+
047200060420     C                                          '"'+%trim(VHFCMO)+'"'+','+
047300060420     C                                          '"'+%trim(VHFNMO)+'"'+','+
047400060413     C                                         '"'+%trim(wGIAC)+'"'+','+
047500060413     C                                         '"'+%trim(wPIAN)+'"'+','+
047600060413     C                                         '"'+%trim(wAPPU)+'"'+','+
047700060413     C                                         '"'+%trim(wLDIS)+'"'+','+
047800060413     C                                         '"'+%trim(wISOL)+'"'+','+
047900060413     C                                         '"'+%trim(wCDIS)+'"'+','+
048000060413     C                                         '"'+%trim(wPODI)+'"'+','+
048100060413     C                                         '"'+%trim(wDIRO)+'"'+','+
048200060413     C                                      %trim(%editc(CSBCAS:'Q'))+','+
048300060413     C                                         '"'+%trim(CSBVCA)
048400930409     C*
048500060413     C                   WRITE     TIVAW000
048600060418     C                   ENDIF
048700991027     C*
048800060420     C* Se già oltre data superiore range => esco dal ciclo
048900060420     C                   IF        wLeave = 'S'
049000060420     C                   LEAVE
049100060420     C                   ENDIF
049200060420     C*
049300060420     C                   ENDIF
049400060420     C*
049500060420     C     skVAF(jVAF)   READE     TITAS31C
049600030325     C                   ENDDO
049700060418     C*
049800060418     C                   ENDIF
049900060420     C                   ENDIF
050000060418     C                   ADD       1             jVAF
050100060418     C                   ENDDO
050200060413     C*
050300030325     C                   EVAL      wrkesito = '0'
050400991027     C*
050500910830     C                   ENDSR
050600060419
050700060419
050800060419
050900060419     C     chkrec        BEGSR
051000060419     C*
051100060419     C* Considero solo le bolle del periodo richiesto
051200060419     C                   IF        wRecOK = 'S'
051300060420     C                   IF        tasDFT > wDataFIN
051400060420     C                   MOVEL     'N'           wRecOK
051500060420     C                   MOVEL     'S'           wLeave
051600060419     C                   ENDIF
051700060419     C                   ENDIF
051800060420     C*
051900060419     C* Verifica le bolle fatturate inizio mese richiesto, occorre considerare solo quelle aventi
052000060419     C* P.O. IVA uguale a zero o uguale ai valori indicati in tabella QT-2.
052100060420     C***                IF        wRecOK = 'S'
052200060420     C***                IF        tasDFT = wDataINZ
052300060420     C***                IF        tasFIV = §QCFIV  OR
052400060420     C***                          tasFIV = §QCFI2  OR
052500060420     C***                          tasFIV = *zeros
052600060420     C***                ELSE
052700060420     C***                MOVEL     'N'           wRecOK
052800060420     C***                ENDIF
052900060420     C***                ENDIF
053000060420     C***                ENDIF
053100060420     C*
053200060419     C* Verifica le bolle fatturate inizio mese successivo, occorre considerare solo quelle aventi
053300060419     C* P.O. IVA diverso da zero e diverso valori indicati in tabella QT-2.
053400060419     C                   IF        wRecOK = 'S'
053500060420     C                   IF        tasDFT = wDataFIN AND
053600060419     C                             tasFIV <> §QCFIV  AND
053700060419     C                             tasFIV <> §QCFI2  AND
053800060420     C                             tasFIV <> *zeros
053900060420     C                   MOVEL     'N'           wRecOK
054000060419     C                   ENDIF
054100060419     C                   ENDIF
054200060419     C*
054300060419     C                   ENDSR
054400060420
054500060420
054600060420
054700060420     C     TAStoVAF      BEGSR
054800060420     C*
054900060420     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
055000060420     C                   EVAL      VAFAAS = tasAAS
055100060420     C                   EVAL      VAFLNP = tasLNP
055200060420     C                   EVAL      VAFNRS = tasNRS
055300060420     C                   EVAL      VAFNSP = tasNSP
055400060420     C                   EVAL      VAFTBL = tasTBL
055500060420     C                   EVAL      VAFMGS = tasMGS
055600060420     C                   EVAL      VAFKSC = tasKSC
055700060420     C                   EVAL      VAFLNA = tasLNA
055800060420     C                   EVAL      VAFPKB = tasPKF
055900060420     C                   EVAL      VAFVLF = tasVLF
056000060420     C                   EVAL      VAFQFT = tasQFT
056100060420     C                   EVAL      VAFPOR = tasPOR
056200060420     C                   EVAL      VAFIMV = tasIMV
056300060420     C                   EVAL      VAFDFT = tasDFT
056400060420     C                   EVAL      VAFNFT = tasNFT
056500060420     C                   EVAL      VAFFIV = tasFIV
056600060420     C                   EVAL      VAFDIV = tasDIV
056700060420     C                   EVAL      VAFDRT = tasDRT
056800060420     C                   EVAL      VAFRMN = tasRMN
056900060420     C****** NON GESTISCO VARIE PARTICOLARITA DEL RIFERIMENTO MITTENTE ALFA *******
057000060420     C                   EVAL      VAFRMA = ta4NOT
057100060420     C*****************************************************************************
057200060420     C                   EVAL      VAFFTC = tasFTC
057300060420     C                   EVAL      VAFTC2 = tasTC2
057400060420     C                   EVAL      VAFTSP = tasTSP
057500060420     C                   EVAL      VAFFAP = tasFAP
057600060420     C                   EVAL      VAFFIN = tasFIN
057700060420     C                   EVAL      VAFCTR = tasCTR
057800060420     C                   EVAL      VAFCTS = tasCTS
057900060420     C                   EVAL      VAFCAD = tasCAD
058000060420     C                   EVAL      VAFLOD = tasLOD
058100060420     C                   EVAL      VAFPRD = tasPRD
058200060420     C                   EVAL      VAFNZD = tasNZD
058300060420     C                   EVAL      VAFVAS = tasVAS
058400060420     C                   EVAL      VAFIAS = tasIAS
058500060420     C*
058600060420     C* Gestisco particolari considerazioni
058700060420     C                   IF        tasFPF = 'V'
058800150716     C*
058900150716     C                   IF        TASDFT <= §QTDST
059000150716     C     §QTTCO        MULT      TASNCP        WTARA
059100150716     C     TASPKC        SUB(h)    WTARA         WNTARA
059200150716     C                   Z-ADD     WNTARA        VAFPKC
059300150716     C                   ELSE
059400150716     C     §QTTPC        MULT      TASNCP        WTARA
059500150716     C     TASPKC        SUB(h)    WTARA         WNTARA
059600150716     C                   Z-ADD     WNTARA        VAFPKC
059700150716     C                   ENDIF
059800150716     C*
059900060420     C                   ELSE
060000060420     C                   Z-ADD     *zeros        VAFPKC
060100060420     C                   ENDIF
060200060420     C*
060300060420     C                   ENDSR
060400060420
060500060420
060600060420
060700060420     C     repVARIE      BEGSR
060800060420     C*
060900060420     C* Inizializzo la DS d procedura esterna
061000060420     C                   CLEAR                   TRUL86DS
061100060420     C                   EVAL      UL86ITLA = 'E'
061200060420     C                   EVAL      UL86IAAS = tasAAS
061300060420     C                   EVAL      UL86ILNP = tasLNP
061400060420     C                   EVAL      UL86INRS = tasNRS
061500060420     C                   EVAL      UL86INSP = tasNSP
061600060420     C                   EVAL      UL86ITBL = tasTBL
061700060420     C                   CALL      'TRUL86R'
061800060420     C                   PARM                    TRUL86DS
061900060420     C*
062000060420     C                   MOVEA(P)  UL86OOUT      skVARIE
062100060420     C*
062200060420     C* Valorizzo VARIA 1
062300060420     C                   EVAL      jVARIE = 1
062400060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
062500060420     C                   EVAL      VAFSV1 = dVARIA_COD
062600060420     C                   EVAL      VAFVA1 = dVARIA_VAL
062700060420     C*
062800060420     C* Valorizzo VARIA 2
062900060420     C                   EVAL      jVARIE = 2
063000060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
063100060420     C                   EVAL      VAFSV2 = dVARIA_COD
063200060420     C                   EVAL      VAFVA2 = dVARIA_VAL
063300060420     C*
063400060420     C* Valorizzo VARIA 3
063500060420     C                   EVAL      jVARIE = 3
063600060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
063700060420     C                   EVAL      VAFSV3 = dVARIA_COD
063800060420     C                   EVAL      VAFVA3 = dVARIA_VAL
063900060420     C*
064000060420     C* Valorizzo VARIA 4
064100060420     C                   EVAL      jVARIE = 4
064200060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
064300060420     C                   EVAL      VAFSV4 = dVARIA_COD
064400060420     C                   EVAL      VAFVA4 = dVARIA_VAL
064500060420     C*
064600060420     C* Valorizzo VARIA 5
064700060420     C                   EVAL      jVARIE = 5
064800060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
064900060420     C                   EVAL      VAFSV5 = dVARIA_COD
065000060420     C                   EVAL      VAFVA5 = dVARIA_VAL
065100060420     C*
065200060420     C* Valorizzo VARIA 6
065300060420     C                   EVAL      jVARIE = 6
065400060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
065500060420     C                   EVAL      VAFSV6 = dVARIA_COD
065600060420     C                   EVAL      VAFVA6 = dVARIA_VAL
065700060420     C*
065800060420     C* Valorizzo VARIA 7
065900060420     C                   EVAL      jVARIE = 7
066000060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
066100060420     C                   EVAL      VAFSV7 = dVARIA_COD
066200060420     C                   EVAL      VAFVA7 = dVARIA_VAL
066300060420     C*
066400060420     C* Valorizzo VARIA 8
066500060420     C                   EVAL      jVARIE = 8
066600060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
066700060420     C                   EVAL      VAFSV8 = dVARIA_COD
066800060420     C                   EVAL      VAFVA8 = dVARIA_VAL
066900060420     C*
067000060420     C* Totalizzo tutte le restanti varie
067100060420     C                   DOW       jVARIE <= %elem(skVARIE)
067200060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
067300060420     C                   EVAL      VAFVAX = VAFVAX + dVARIA_VAL
067400060420     C                   ADD       1             jVARIE
067500060420     C                   ENDDO
067600060420     C*
067700060420     C                   ENDSR
067800060418
067900060418
068000060419
068100060418     C     cartab        BEGSR
068200060418     C*
068300060419     C* Reperisco tutti i codici clienti che x la fatturazione sono unificati sul cliente richiesto
068400060418     C                   Z-ADD     *zeros        jVAF
068500060420     C                   CLEAR                   DS4C
068600060418     C                   EVAL      tblKUT = 1
068700060418     C                   EVAL      tblCOD = '4C'
068800060419     C                   EVAL      tblKEY = *blanks
068900060418     C     KEYtabel_P    SETLL     TABEL00F
069000060418     C                   IF        %found(TABEL00F)
069100060418     C     KEYtabel_P    READE     TABEL00F
069200060418     C                   DOW       not %eof(TABEL00F)
069300060418     C                   IF        tblFLG = *blanks
069400060418     C                   EVAL      DS4C = tblUNI
069500060418     C                   IF        §4CFKS = wKSUVAF
069600060418     C                   ADD       1             jVAF
069700060418     C                   MOVEL(P)  tblKEY        skVAF(jVAF)
069800060418     C                   ENDIF
069900060418     C                   ENDIF
070000060418     C     KEYtabel_P    READE     TABEL00F
070100060418     C                   ENDDO
070200060418     C                   ENDIF
070300060419     C*
070400060419     C* Reperisco i P.O. IVA da considerare x le fatture fine mese clienti codificati
070500060419     C                   CLEAR                   DSQC2
070600060419     C                   Z-ADD     1             tblKUT
070700060419     C                   MOVEL(P)  'QC'          tblCOD
070800060419     C                   MOVEL(P)  '2'           tblKEY
070900060419     C     KEYtabel      CHAIN     TABEL00F
071000060419     C                   IF        %found(TABEL00F)
071100060420     C                   IF        tblFLG = *blanks
071200060419     C                   MOVEL(P)  tblUNI        DSQC2
071300060419     C                   ENDIF
071400060420     C                   ENDIF
071500060420     C*
071600060420     C** Reperisco campi standard tassazione
071700060420     C                   CLEAR                   DSQT1
071800060420     C                   Z-ADD     1             tblKUT
071900060420     C                   MOVEL     'QT'          tblCOD
072000060420     C                   MOVEL     1             tblKEY
072100060420     C     KEYtabel      CHAIN     TABEL00F
072200060420     C                   IF        %found(TABEL00F)
072300060420     C                   IF        tblFLG = *blanks
072400060420     C                   MOVEL     TBLUNI        DSQT1
072500060420     C                   ENDIF
072600060420     C                   ENDIF
072700060418     C*
072800060418     C                   ENDSR
072900991027
073000060419
073100060419
073200060419     C     chkPAR        BEGSR
073300060419     C*
073400060419     C* Estrapolo il codice cliente unificante (unificante secondo tabella 4C) d fatturazione
073500060419     C                   EVAL      wKSUVAFa = %subst(prmppt:2:7)
073600060419     C                   MOVE(P)   wKSUVAFa      wKSUVAF
073700060419     C*
073800060419     C* Se presente nei parametri la forzatura periodo fatturazione (anno + mese) considero quello
073900060419     C* altrimenti considero il mese precedente a quello corrente
074000060419     C                   EVAL      wAAAAMMa = %subst(prmppt:9:6)
074100060419     C                   MOVE(P)   wAAAAMMa      wAAAAMM
074200060419     C                   IF        wAAAAMM = *zeros
074300060419     C                   Z-ADD     wAAAAMMcor    wPERIODO
074400060419     C                   EVAL      wMM = wMM - 1                                * mese precedente
074500060419     C                   ELSE
074600060419     C                   Z-ADD     wAAAAMM       wPERIODO
074700060419     C                   ENDIF
074800060419     C*
074900060420     C* Calcolo il primo giorno del mese successivo
075000060419     C                   IF        wMM = 12
075100060420     C***                EVAL      wDataFIN = 0101*10000+wAAAA+1
075200060420     C                   EVAL      wDataFIN = (wAAAA+1)*10000+0101
075300060419     C                   ELSE
075400060420     C***                EVAL      wDataFIN = 01*1000000+(wMM+1)*10000+wAAAA
075500060420     C                   EVAL      wDataFIN = wAAAA*10000+(wMM+1)*100+01
075600060419     C                   ENDIF
075700060420     C*
075800060420     C* Calcolo l'ultimo giorno del mese precedente
075900060420     C     *ISO          MOVE      wDataFIN      wDataISO
076000060420     C     wDataISO      SUBDUR    1:*D          wDataISO
076100060420     C     *ISO          MOVE      wDataISO      wDataINZ
076200060420     C*
076300060420     C* Calcolo il primo giorno del mese precedente
076400060420     C***                IF        wMM = 01
076500060420     C***                EVAL      wDataINZ = 0112*10000+wAAAA-1
076600060420     C***                ELSE
076700060420     C***                EVAL      wDataINZ = 01*1000000+(wMM-1)*10000+wAAAA
076800060420     C***                ENDIF
076900060419     C*
077000060419     C                   ENDSR
077100060418
077200060419
077300060418
077400991027      /TITLE Operazioni iniziali.
077500991027     C     *inzsr        BEGSR
077600991027     C*
077700991027     C     *ENTRY        PLIST
077800991027     C                   parm                    prmppt
077900991027     C     wrkesito      parm      wrkesito      prmesito
078000060419     C*
078100060419     C* CALCOLA LA DATA CORRENTE
078200060419     C                   time                    wn14             14 0
078300060419     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
078400060419     C                   z-add     wn8           g08dat
078500060419     C                   z-add     *zeros        g08inv
078600060419     C                   movel     '0'           g08err
078700060419     C                   call      'XSRDA8'
078800060419     C                   parm                    wlbda8
078900060419     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
079000060419     C                   movel     datcor        wAAAAMMcor        6 0
079100060413     C*
079200060413     C* Definizione chiavi di lettura
079300060419     C*
079400060419     C* TABEL00F - Completa
079500060419     C     KEYtabel      KLIST
079600060419     C                   KFLD                    tblKUT
079700060419     C                   KFLD                    tblCOD
079800060419     C                   KFLD                    tblKEY
079900060418     C*
080000060418     C* TABEL00F - Parziale
080100060418     C     KEYtabel_P    KLIST
080200060418     C                   KFLD                    tblKUT
080300060418     C                   KFLD                    tblCOD
080400060413     C*
080500060413     C* TITAS31L - Completa
080600060413     C     KEYtas31      KLIST
080700060413     C                   KFLD                    tasKSC
080800060420     C                   KFLD                    tasDFT
080900060413     C*
081000060413     C* TNCSB03L - Completa
081100060413     C     KEYcsb03      KLIST
081200060413     C                   KFLD                    tasAAS
081300060413     C                   KFLD                    tasLNP
081400060413     C                   KFLD                    tasNRS
081500060413     C                   KFLD                    tasNSP
081600060418     C*
081700060418     C* TIGCP51L - Parziale
081800060418     C     KEYgcp51_P    KLIST
081900060418     C                   KFLD                    tasAAS
082000060418     C                   KFLD                    tasLNP
082100060418     C                   KFLD                    tasNRS
082200060418     C                   KFLD                    tasNSP
082300060418     C*
082400060418     C* TITA430C - Completa
082500060418     C     KEYta430      KLIST
082600060418     C                   KFLD                    tasAAS
082700060418     C                   KFLD                    tasLNP
082800060418     C                   KFLD                    tasNRS
082900060418     C                   KFLD                    tasNSP
083000060418     C                   KFLD                    ta4TRC
083100060420     C*
083200060420     C* TITAA30C - Completa
083300060420     C     KEYtaa30      KLIST
083400060420     C                   KFLD                    tasAAS
083500060420     C                   KFLD                    tasLNP
083600060420     C                   KFLD                    tasNRS
083700060420     C                   KFLD                    tasNSP
083800060420     C                   KFLD                    taaTRC
083900060420     C*
084000060420     C* FIAR531C - Parziale
084100060420     C     KEYar531_P    KLIST
084200060420     C                   KFLD                    tasAAS
084300060420     C                   KFLD                    tasLNP
084400060420     C                   KFLD                    tasNRS
084500060420     C                   KFLD                    tasNSP
084600060420     C                   KFLD                    ar5TRD
084700991027     C*
084800991027     C                   ENDSR
084900060421** DEC - COSTANTI
085000060421<?xml version="1.0"?>
085100060421<?mso-application progid="Excel.Sheet"?>
085200060421<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
085300060421xmlns:o="urn:schemas-microsoft-com:office:office"
085400060421xmlns:x="urn:schemas-microsoft-com:office:excel"
085500060421xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
085600060421xmlns:html="http://www.w3.org/TR/REC-html40">
085700060421</Workbook>
085800060421<Worksheet ss:Name="PAG1">
085900060421</Worksheet>
086000060421<Table border="1">
086100060421</Table>
086200060421<Row>
086300060421</Row>
086400060421<Cell><Data ss:Type="String">
086500060421<Cell><Data ss:Type="Number">
086600060421</Data></Cell>
086700060421
086800060421
086900060421
