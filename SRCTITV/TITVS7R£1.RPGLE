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
001100090309     FTIVAWWWT  UF A E             DISK    commit
001200060331
001300060418     D****
001400060418     D* SCHIERE DI WRK
001500060418     D****
001600060418     D skVAF           s              7  0 inz dim(1000)
001700060418     D jVAF            s              4  0 inz
001800060419
001900060419     D*------------------
002000060419     D* DS "XSRDA8" - CONTROLLA DATA (8)
002100060419     D*------------------
002200060419     D WLBDA8          DS                  INZ
002300060419     D  G08DAT                 1      8  0
002400060419     D  G08INV                 9     16  0
002500060419     D  G08ERR                17     17
002600060419     D  G08TGI                18     22  0
002700060420
002800060420     D*------------------
002900060420     D* DS REPERIMENTO DATI CLIENTE
003000060420     D*-------------------
003100060420     D BS69DS        E DS                  EXTNAME(TIBS69DS)
003200060420     D ACODS         E DS                  EXTNAME(CNACO00F)
003300060420     D INDDS         E DS                  EXTNAME(CNIND00F)
003400060420     D CLPDS         E DS                  EXTNAME(CNCLP00F)
003500060420     D CLSDS         E DS                  EXTNAME(FNCLS00F)
003600060419
003700060419     D*------------
003800060420     D* DS ESTERNE VARIE
003900060419     D*------------
004000060420     D DSVAF         E DS                  EXTNAME(TIVAF00T)
004100060420     D DSVAFA        E DS                  EXTNAME(TIVAFAWT)
004200060420     D TRUL86DS      E DS
004300060420     D DAR5BNB       E DS
004400151016     D DAR5FAT       E DS
004500060420     D DSQT1         E DS
004600060420     D DS4C          E DS
004700060420
004800060420     D*------------
004900060420     D* SCHIERE SIGLE E DESCRIZIONI VARIE
005000060420     D*------------
005100060420     D skVARIE         S                   LIKE(dsVARIA) DIM(50) INZ
005200060420     D jVARIE          S              2  0 INZ
005300060420
005400060420     D*------------------
005500060420     D* DS RIDEFINIZIONE OUTPUT ELEMENTO PROCEDURA TRUL86R
005600060420     D*------------------
005700060420     D dsVARIA         DS
005800060420     D  dVARIA_COD                    1
005900060420     D  dVARIA_VAL                   13  3
006000060420
006100060420     D*------------
006200060420     D* DS RIDEFINIZIONE TABELLA "QC" TIPO RECORD "2"
006300060420     D*------------
006400060420     D DSQC2         E DS
006500060419
006600060419     D*------------------
006700060419     D* DS X PASSAGGIO PARAMETRI
006800060419     D*------------------
006900060419     D                 DS
007000060419     D  wPERIODO               1      6  0
007100060419     D  wMM                    5      6  0
007200060419     D  wAAAA                  1      4  0
007300060418
007400060331     D****
007500060331     D* VARIABILI DI WRK
007600060331     D****
007700991027     D prmppt          s             50
007800991027     D prmesito        s              1
007900991027     D wrkesito        s                   like(prmesito)
008000060418     D wKSUVAFa        s              7
008100060418     D wKSUVAF         s              7  0
008200060420     D wAAAAMMa        s              6
008300060420     D wAAAAMM         s              6  0
008400060420     D wDataINZ        s              8  0
008500060420     D wDataFIN        s              8  0
008600060420     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
008700060420     D wTara           s                   like(§QTTPC)
008800060420     D wNtara          s                   like(TASPKF)
008900991027
009000060419     C*
009100060419     C* Verifico ed effettuo considerazioni sui parametri ricevuti in input
009200060419     C                   EXSR      chkPAR
009300060418     C*
009400060418     C* Carico le tabelle occorrenti
009500060419     C                   EXSR      carTAB
009600060418     C*
009700060418     C* Verifico se già presenti dati nel file d output
009800030908     C                   EXSR      chkRECFILMBR
009900060418     C*
010000060418     C* Effettuo elaborazione
010100991027     C                   EXSR      traduci
010200060420     C*
010300060420     C* Effettuo chiamata x chiusura procedure esterne
010400060420     C                   CLEAR                   TRUL86DS
010500060420     C                   EVAL      UL86ITLA = 'C'
010600060420     C                   CALL      'TRUL86R'
010700060420     C                   PARM                    TRUL86DS
010800060418     C*
010900921023     C                   SETON                                        LR
011000060413
011100060413
011200030908
011300030908     C     chkRECFILMBR  BEGSR
011400030908     C*
011500060413     C                   READ      TIVAWWWT                               55
011600030908     C*
011700030908     C                   ENDSR
011800060413
011900060413
012000991027
012100991027     C     traduci       BEGSR
012200030325     C*
012300030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
012400030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
012500060420     C                   EVAL      VAWDTA  = '"VAFAAS"'+','+
012600060420     C                                       '"VAFLNP"'+','+
012700060420     C                                       '"VAFNRS"'+','+
012800060420     C                                       '"VAFNSP"'+','+
012900060420     C                                       '"VAFTBL"'+','+
013000060420     C                                       '"VAFMGS"'+','+
013100060420     C                                       '"VAFKSC"'+','+
013200060420     C                                       '"VAFLNA"'+','+
013300060420     C                                       '"VAFNCL"'+','+
013400060420     C                                       '"VAFPKB"'+','+
013500060420     C                                       '"VAFVLF"'+','+
013600060420     C                                       '"VAFQFT"'+','+
013700060420     C                                       '"VAFPOR"'+','+
013800060420     C                                       '"VAFSV1"'+','+
013900060420     C                                       '"VAFVA1"'+','+
014000060420     C                                       '"VAFSV2"'+','+
014100060420     C                                       '"VAFVA2"'+','+
014200060420     C                                       '"VAFSV3"'+','+
014300060420     C                                       '"VAFVA3"'+','+
014400060420     C                                       '"VAFSV4"'+','+
014500060420     C                                       '"VAFVA4"'+','+
014600060420     C                                       '"VAFSV5"'+','+
014700060420     C                                       '"VAFVA5"'+','+
014800060420     C                                       '"VAFSV6"'+','+
014900060420     C                                       '"VAFVA6"'+','+
015000060420     C                                       '"VAFSV7"'+','+
015100060420     C                                       '"VAFVA7"'+','+
015200060420     C                                       '"VAFSV8"'+','+
015300060420     C                                       '"VAFVA8"'+','+
015400060420     C                                       '"VAFVAX"'+','+
015500060420     C                                       '"VAFIMV"'+','+
015600060420     C                                       '"VAFDFT"'+','+
015700060420     C                                       '"VAFNFT"'+','+
015800060420     C                                       '"VAFFIV"'+','+
015900060420     C                                       '"VAFDIV"'+','+
016000060420     C                                       '"VAFDRT"'+','+
016100060420     C                                       '"VAFRMN"'+','+
016200060420     C                                       '"VAFRMA"'+','+
016300060420     C                                       '"VAFRMX"'+','+
016400060420     C                                       '"VAFFTC"'+','+
016500060420     C                                       '"VAFTC2"'+','+
016600060420     C                                       '"VAFTSP"'+','+
016700060420     C                                       '"VAFFAP"'+','+
016800060420     C                                       '"VAFFIN"'+','+
016900060420     C                                       '"VAFCTR"'+','+
017000060420     C                                       '"VAFNAS"'+','+
017100060420     C                                       '"VAFCTS"'+','+
017200060420     C                                       '"VAFCAD"'+','+
017300060420     C                                       '"VAFLOD"'+','+
017400060420     C                                       '"VAFPRD"'+','+
017500060420     C                                       '"VAFNZD"'+','+
017600060420     C                                       '"VAFVAS"'+','+
017700060420     C                                       '"VAFIAS"'+','+
017800060420     C                                       '"VAFFPC"'+','+
017900060420     C                                       '"VAFPKC"'+','+
018000060420     C                                       '"VAFFVC"'+','+
018100060420     C                                       '"VAFVLC"'+','+
018200060420     C                                       '"VAFRSD"'+','+
018300060420     C                                       '"VAFIND"'+','+
018400060420     C                                       '"VAFRSM"'+','+
018500060420     C                                       '"VAFINM"'+','+
018600060420     C                                       '"VAFCAM"'+','+
018700060420     C                                       '"VAFLOM"'+','+
018800060420     C                                       '"VAFPRM"'+','+
018900060420     C                                       '"VAFNZM"'+','+
019000060420     C                                       '"VAFRMO"'+','+
019100060420     C                                       '"VAFCMO"'+','+
019200060420     C                                       '"VAFNMO"'+','+
019300060418     C                                      '"GIACENZA"'+','+
019400060418     C                                      '"CONS_AI_PIANI"'+','+
019500060418     C                                      '"CONS_APPUNTAMENTO"'+','+
019600060418     C                                      '"CONS_LOCALITA_DISAGIATA"'+','+
019700060418     C                                      '"CONS_ISOLE_MINORI"'+','+
019800060418     C                                      '"CONS_DISAGIATA"'+','+
019900060418     C                                      '"POD_IMAGE"'+','+
020000060418     C                                      '"DIROTTAMENTO"'+','+
020100060418     C                                      '"IMPORTO_CONTRASSEGNO"'+','+
020200060420     C                                      '"DIVISA_CONTRASSEGNO"'
020300030325     C*
020400060413     C                   WRITE     TIVAW000
020500030325     C                   ENDIF
020600991027     C*
020700060418     C* Ciclo x tutti i codici clienti ricondotti al cliente unificante fatture (tab. 4C)
020800060418     C                   EVAL      jVAF = 1
020900060418     C                   DOW       jVAF <= %elem(skVAF)
021000060418     C                   IF        skVAF(jVAF) = *zeros
021100060418     C                   LEAVE
021200060418     C                   ELSE
021300060418     C                   EVAL      tasKSC = skVAF(jVAF)
021400060420     C                   EVAL      tasDFT = wDataINZ
021500060420     C     KEYtas31      SETLL     TITAS31C
021600060413     C                   IF        %found(TITAS31C)
021700060420     C     skVAF(jVAF)   READE     TITAS31C
021800060413     C                   DOW       not %eof(TITAS31C)
021900060418     C*
022000060419     C* Inizializzo il flag che condiziona l'elaborazione del record bolla corrente
022100060419     C                   MOVEL     'S'           wRecOK            1
022200060420     C                   MOVEL     'N'           wLeave            1
022300060419     C*
022400060419     C* Effettuo verifiche validità record
022500060419     C                   EXSR      chkREC
022600060419     C*
022700060419     C* Se record da considerare procedo con le considerazioni
022800060419     C                   IF        wRecOK = 'S'
022900060419     C*
023000060419     C* Re inizializzo il flag che indica se la bolla corrente è da considerare
023100060419     C                   MOVEL     'N'           wRecOK            1
023200060418     C*
023300060418     C* Inizializzo tutti i flag d wrk relativi agli attributi della bolla corrente
023400060418     C                   MOVEL     *blanks       wGIAC             1
023500060418     C                   MOVEL     *blanks       wPIAN             1
023600060418     C                   MOVEL     *blanks       wAPPU             1
023700060418     C                   MOVEL     *blanks       wLDIS             1
023800060418     C                   MOVEL     *blanks       wISOL             1
023900060418     C                   MOVEL     *blanks       wCDIS             1
024000060418     C                   MOVEL     *blanks       wPODI             1
024100060418     C                   MOVEL     *blanks       wDIRO             1
024200060418     C*
024300060418     C* Aggancio il file giacenze della bolla corrente
024400060418     C     KEYgcp51_P    CHAIN     TIGCP51L
024500060418     C                   IF        %found(TIGCP51L)
024600060418     C                   MOVEL     'S'           wGIAC
024700060418     C                   MOVEL     'S'           wRecOK
024800060418     C                   ENDIF
024900060413     C*
025000060413     C* Aggancio il file contrassegni della bolla corrente
025100060421     C                   Z-ADD     *zeros        csbCAS
025200060421     C                   MOVEL     *blanks       csbVCA
025300060413     C     KEYcsb03      CHAIN     TNCSB03L
025400060413     C                   IF        %found(TNCSB03L)
025500060421     C                   IF        csbSTA <> 9
025600060418     C                   MOVEL     'S'           wRecOK
025700060421     C                   ENDIF
025800060418     C                   ENDIF
025900060418     C*
026000060418     C* Verifica se APPUNTAMENTO
026100060418     C                   IF        tasFTC = 'A' or
026200060418     C                             tasTC2 = 'A'
026300060418     C                   MOVEL     'S'           wRecOK
026400060418     C                   MOVEL     'S'           wAPPU
026500060418     C                   ENDIF
026600060418     C*
026700060418     C* Verifica se AI PIANI
026800060418     C                   IF        tasFTC = 'P' or
026900060418     C                             tasTC2 = 'P'
027000060418     C                   MOVEL     'S'           wRecOK
027100060418     C                   MOVEL     'S'           wPIAN
027200060418     C                   ENDIF
027300060418     C*
027400060418     C* Verifica se CONSEGNA DISAGIATA
027500060418     C                   IF        tasFTC = 'X' or
027600060418     C                             tasTC2 = 'X'
027700060418     C                   MOVEL     'S'           wRecOK
027800060418     C                   MOVEL     'S'           wCDIS
027900060418     C                   ENDIF
028000060418     C*
028100060418     C* Verifica se LOCALITA DISAGIATA
028200060418     C                   IF        tasFIN = 'D'
028300060418     C                   MOVEL     'S'           wRecOK
028400060418     C                   MOVEL     'S'           wLDIS
028500060418     C                   ENDIF
028600060418     C*
028700060418     C* Verifica se ISOLA MINORE
028800060418     C                   IF        tasFIN = 'I'
028900060418     C                   MOVEL     'S'           wRecOK
029000060418     C                   MOVEL     'S'           wISOL
029100060418     C                   ENDIF
029200060418     C*
029300060418     C* Verifica se P.O.D. IMAGE
029400060418     C                   IF        tasCBO = 'FI'
029500060418     C                   MOVEL     'S'           wRecOK
029600060418     C                   MOVEL     'S'           wPODI
029700060418     C                   ENDIF
029800060418     C*
029900060418     C* Verifica se DIROTTAMENTO
030000060418     C                   IF        tasCBO = 'F*'
030100060418     C                   MOVEL     'S'           wRecOK
030200060418     C                   MOVEL     'S'           wDIRO
030300060418     C                   ENDIF
030400060418     C*
030500060418     C* Se presente numero fattura considero sempre la bolla
030600060418     C                   IF        tasNFT > *zeros
030700060418     C                   MOVEL     'S'           wRecOK
030800060418     C                   ENDIF
030900060413     C*
031000060413     C* Se tutto ok procedo con la scrittura del buffer d out
031100060420     C                   IF        wRecOK = 'S'
031200060420     C*
031300060420     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
031400060420     C                   EVAL      ta4TRC = 'A'
031500060420     C     KEYta430      CHAIN     TITA430C
031600060420     C                   IF        %found(TITA430C)
031700060420     C                   ELSE
031800060420     C                   CLEAR                   TITA4000
031900060420     C                   CLEAR                   TITA4P00
032000060420     C                   ENDIF
032100060420     C*
032200060420     C* Inizializzo il buffer del file d output e delle ds d ridefinizione dati
032300060420     C                   CLEAR                   TIVAW000
032400060420     C                   CLEAR                   DSVAF
032500060420     C                   CLEAR                   DSVAFA
032600060420     C*
032700060420     C* Valorizzo i dati del destinatario nn già presenti sul TIVAF
032800060420     C                   EVAL      VHFRSD = TASRSD
032900060420     C                   EVAL      VHFIND = TASIND
033000060420     C*
033100060420     C* X reperire i dati del mittente verifico il codice del cliente mittente:
033200060420     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
033300060420     C                   MOVE(P)   tasCCM        wCLI              4 0
033400060420     C                   IF        wCLI =    0 OR
033500060420     C                             wCLI = 8888 OR
033600060420     C                             wCLI = 9999
033700060420     C                   EVAL      taaTRC = 'M'
033800060420     C     KEYtaa30      CHAIN     titaa30c
033900060420     C                   IF        %found(titaa30c)
034000060420     C                   EVAL      VHFRSM = TAARSC
034100060420     C                   EVAL      VHFINM = TAAIND
034200060420     C                   EVAL      VHFCAM = TAACAP
034300060420     C                   EVAL      VHFLOM = TAALOC
034400060420     C                   EVAL      VHFPRM = TAAPRV
034500060420     C                   EVAL      VHFNZM = TAANAZ
034600060420     C                   ENDIF
034700060420     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
034800060420     C                   ELSE
034900060420     C                   CLEAR                   BS69DS
035000060420     C                   CLEAR                   ACODS
035100060420     C                   CLEAR                   INDDS
035200060420     C                   CLEAR                   CLPDS
035300060420     C                   CLEAR                   CLSDS
035400060420     C                   Z-ADD     tasCCM        I69KAC
035500060420     C                   Z-ADD     tasCCM        I69KIN
035600060420     C                   CALL      'TIBS69R'
035700060420     C                   PARM                    BS69DS
035800060420     C                   PARM                    ACODS
035900060420     C                   PARM                    INDDS
036000060420     C                   PARM                    CLPDS
036100060420     C                   PARM                    CLSDS
036200060420     C     O69ERR        IFNE      '1'
036300060420     C                   EVAL      VHFRSM = ACORAG
036400060420     C                   EVAL      VHFINM = INDVIA
036500060420     C                   MOVEL(P)  INDCAP        VHFCAM
036600060420     C                   EVAL      VHFLOM = INDCIT
036700060420     C                   EVAL      VHFPRM = INDPRV
036800060420     C                   EVAL      VHFNZM = INDSTA
036900060420     C                   ENDIF
037000060420     C                   ENDIF
037100060420     C*
037200060420     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
037300060420     C                   EVAL      taaTRC = 'O'
037400060420     C     KEYtaa30      CHAIN     titaa30c
037500060420     C                   IF        %found(titaa30c)
037600060420     C                   EVAL      VHFRMO = TAARSC
037700060420     C                   EVAL      VHFCMO = TAACAP
037800060420     C                   EVAL      VHFNMO = TAANAZ
037900060420     C                   ENDIF
038000060420     C*
038100060420     C* Se particolità varia = 'O' recupero da FIAR5 il numero colli originali
038200060420     C                   IF        %subst(tasGVA:1:1) = 'O'
038300060420     C                   EVAL      ar5TRD = 'BNB'
038400060420     C                   CLEAR                   DAR5BNB
038500060420     C     KEYar531_P    CHAIN     FIAR531C
038600060420     C                   IF        %found(FIAR531C)
038700060420     C                   MOVEL     AR5UNI        DAR5BNB
038800060420     C                   ENDIF
038900060420     C* Colli originali
039000060420     C                   EVAL      VAFNCL = §AR5BCOR
039100060420     C* Colli TITAS
039200060420     C                   ELSE
039300060420     C                   Z-ADD     TASNCL        VAFNCL
039400060420     C                   ENDIF
039500060420     C*
039600060420     C* Eseguo reperimento varie
039700060420     C                   EXSR      repVARIE
039800060420     C*
039900060420     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
040000060420     C                   EXSR      TAStoVAF
040100060420     C*
040200151016     C* Eseguo eventuali "forzature"
040300151016     C                   EXSR      RTVAR5FAT
040400151016     C*
040500060420     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'Q'))+','+
040600060420     C                                       %trim(%editc(VAFLNP:'Q'))+','+
040700060420     C                                       %trim(%editc(VAFNRS:'Q'))+','+
040800060420     C                                       %trim(%editc(VAFNSP:'Q'))+','+
040900060420     C                                          '"'+%trim(VAFTBL)+'"'+','+
041000060420     C                                       %trim(%editc(VAFMGS:'Q'))+','+
041100060420     C                                       %trim(%editc(VAFKSC:'Q'))+','+
041200060420     C                                       %trim(%editc(VAFLNA:'Q'))+','+
041300060420     C                                       %trim(%editc(VAFNCL:'Q'))+','+
041400060420     C                                       %trim(%editc(VAFPKB:'Q'))+','+
041500060420     C                                       %trim(%editc(VAFVLF:'Q'))+','+
041600060420     C                                       %trim(%editc(VAFQFT:'Q'))+','+
041700060420     C                                       %trim(%editc(VAFPOR:'Q'))+','+
041800060420     C                                          '"'+%trim(VAFSV1)+'"'+','+
041900060420     C                                       %trim(%editc(VAFVA1:'Q'))+','+
042000060420     C                                          '"'+%trim(VAFSV2)+'"'+','+
042100060420     C                                       %trim(%editc(VAFVA2:'Q'))+','+
042200060420     C                                          '"'+%trim(VAFSV3)+'"'+','+
042300060420     C                                       %trim(%editc(VAFVA3:'Q'))+','+
042400060420     C                                          '"'+%trim(VAFSV4)+'"'+','+
042500060420     C                                       %trim(%editc(VAFVA4:'Q'))+','+
042600060420     C                                          '"'+%trim(VAFSV5)+'"'+','+
042700060420     C                                       %trim(%editc(VAFVA5:'Q'))+','+
042800060420     C                                          '"'+%trim(VAFSV6)+'"'+','+
042900060420     C                                       %trim(%editc(VAFVA6:'Q'))+','+
043000060420     C                                          '"'+%trim(VAFSV7)+'"'+','+
043100060420     C                                       %trim(%editc(VAFVA7:'Q'))+','+
043200060420     C                                          '"'+%trim(VAFSV8)+'"'+','+
043300060420     C                                       %trim(%editc(VAFVA8:'Q'))+','+
043400060420     C                                       %trim(%editc(VAFVAX:'Q'))+','+
043500060420     C                                       %trim(%editc(VAFIMV:'Q'))+','+
043600060420     C                                       %trim(%editc(VAFDFT:'Q'))+','+
043700060420     C                                       %trim(%editc(VAFNFT:'Q'))+','+
043800060420     C                                       %trim(%editc(VAFFIV:'Q'))+','+
043900060420     C                                          '"'+%trim(VAFDIV)+'"'+','+
044000060420     C                                       %trim(%editc(VAFDRT:'Q'))+','+
044100060420     C                                       %trim(%editc(VAFRMN:'Q'))+','+
044200060420     C                                          '"'+%trim(VAFRMA)+'"'+','+
044300060420     C                                          '"'+%trim(VAFRMX)+'"'+','+
044400060420     C                                          '"'+%trim(VAFFTC)+'"'+','+
044500060420     C                                          '"'+%trim(VAFTC2)+'"'+','+
044600060420     C                                          '"'+%trim(VAFTSP)+'"'+','+
044700060420     C                                          '"'+%trim(VAFFAP)+'"'+','+
044800060420     C                                          '"'+%trim(VAFFIN)+'"'+','+
044900060420     C                                       %trim(%editc(VAFCTR:'Q'))+','+
045000060420     C                                          '"'+%trim(VAFNAS)+'"'+','+
045100060420     C                                          '"'+%trim(VAFCTS)+'"'+','+
045200060420     C                                          '"'+%trim(VAFCAD)+'"'+','+
045300060420     C                                          '"'+%trim(VAFLOD)+'"'+','+
045400060420     C                                          '"'+%trim(VAFPRD)+'"'+','+
045500060420     C                                          '"'+%trim(VAFNZD)+'"'+','+
045600060420     C                                          '"'+%trim(VAFVAS)+'"'+','+
045700060420     C                                       %trim(%editc(VAFIAS:'Q'))+','+
045800060420     C                                          '"'+%trim(VAFFPC)+'"'+','+
045900060420     C                                       %trim(%editc(VAFPKC:'Q'))+','+
046000060420     C                                          '"'+%trim(VAFFVC)+'"'+','+
046100060420     C                                       %trim(%editc(VAFVLC:'Q'))+','+
046200060420     C                                          '"'+%trim(VHFRSD)+'"'+','+
046300060420     C                                          '"'+%trim(VHFIND)+'"'+','+
046400060420     C                                          '"'+%trim(VHFRSM)+'"'+','+
046500060420     C                                          '"'+%trim(VHFINM)+'"'+','+
046600060420     C                                          '"'+%trim(VHFCAM)+'"'+','+
046700060420     C                                          '"'+%trim(VHFLOM)+'"'+','+
046800060420     C                                          '"'+%trim(VHFPRM)+'"'+','+
046900060420     C                                          '"'+%trim(VHFNZM)+'"'+','+
047000060420     C                                          '"'+%trim(VHFRMO)+'"'+','+
047100060420     C                                          '"'+%trim(VHFCMO)+'"'+','+
047200060420     C                                          '"'+%trim(VHFNMO)+'"'+','+
047300060413     C                                         '"'+%trim(wGIAC)+'"'+','+
047400060413     C                                         '"'+%trim(wPIAN)+'"'+','+
047500060413     C                                         '"'+%trim(wAPPU)+'"'+','+
047600060413     C                                         '"'+%trim(wLDIS)+'"'+','+
047700060413     C                                         '"'+%trim(wISOL)+'"'+','+
047800060413     C                                         '"'+%trim(wCDIS)+'"'+','+
047900060413     C                                         '"'+%trim(wPODI)+'"'+','+
048000060413     C                                         '"'+%trim(wDIRO)+'"'+','+
048100060413     C                                      %trim(%editc(CSBCAS:'Q'))+','+
048200060413     C                                         '"'+%trim(CSBVCA)
048300930409     C*
048400060413     C                   WRITE     TIVAW000
048500060418     C                   ENDIF
048600991027     C*
048700060420     C* Se già oltre data superiore range => esco dal ciclo
048800060420     C                   IF        wLeave = 'S'
048900060420     C                   LEAVE
049000060420     C                   ENDIF
049100060420     C*
049200060420     C                   ENDIF
049300060420     C*
049400060420     C     skVAF(jVAF)   READE     TITAS31C
049500030325     C                   ENDDO
049600060418     C*
049700060418     C                   ENDIF
049800060420     C                   ENDIF
049900060418     C                   ADD       1             jVAF
050000060418     C                   ENDDO
050100060413     C*
050200030325     C                   EVAL      wrkesito = '0'
050300991027     C*
050400910830     C                   ENDSR
050500060419
050600060419
050700060419
050800060419     C     chkrec        BEGSR
050900060419     C*
051000060419     C* Considero solo le bolle del periodo richiesto
051100060419     C                   IF        wRecOK = 'S'
051200060420     C                   IF        tasDFT > wDataFIN
051300060420     C                   MOVEL     'N'           wRecOK
051400060420     C                   MOVEL     'S'           wLeave
051500060419     C                   ENDIF
051600060419     C                   ENDIF
051700060420     C*
051800060419     C* Verifica le bolle fatturate inizio mese richiesto, occorre considerare solo quelle aventi
051900060419     C* P.O. IVA uguale a zero o uguale ai valori indicati in tabella QT-2.
052000060420     C***                IF        wRecOK = 'S'
052100060420     C***                IF        tasDFT = wDataINZ
052200060420     C***                IF        tasFIV = §QCFIV  OR
052300060420     C***                          tasFIV = §QCFI2  OR
052400060420     C***                          tasFIV = *zeros
052500060420     C***                ELSE
052600060420     C***                MOVEL     'N'           wRecOK
052700060420     C***                ENDIF
052800060420     C***                ENDIF
052900060420     C***                ENDIF
053000060420     C*
053100060419     C* Verifica le bolle fatturate inizio mese successivo, occorre considerare solo quelle aventi
053200060419     C* P.O. IVA diverso da zero e diverso valori indicati in tabella QT-2.
053300060419     C                   IF        wRecOK = 'S'
053400060420     C                   IF        tasDFT = wDataFIN AND
053500060419     C                             tasFIV <> §QCFIV  AND
053600060419     C                             tasFIV <> §QCFI2  AND
053700060420     C                             tasFIV <> *zeros
053800060420     C                   MOVEL     'N'           wRecOK
053900060419     C                   ENDIF
054000060419     C                   ENDIF
054100060419     C*
054200060419     C                   ENDSR
054300060420
054400060420
054500060420
054600060420     C     TAStoVAF      BEGSR
054700060420     C*
054800060420     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
054900060420     C                   EVAL      VAFAAS = tasAAS
055000060420     C                   EVAL      VAFLNP = tasLNP
055100060420     C                   EVAL      VAFNRS = tasNRS
055200060420     C                   EVAL      VAFNSP = tasNSP
055300060420     C                   EVAL      VAFTBL = tasTBL
055400060420     C                   EVAL      VAFMGS = tasMGS
055500060420     C                   EVAL      VAFKSC = tasKSC
055600060420     C                   EVAL      VAFLNA = tasLNA
055700060420     C                   EVAL      VAFPKB = tasPKF
055800060420     C                   EVAL      VAFVLF = tasVLF
055900060420     C                   EVAL      VAFQFT = tasQFT
056000060420     C                   EVAL      VAFPOR = tasPOR
056100060420     C                   EVAL      VAFIMV = tasIMV
056200060420     C                   EVAL      VAFDFT = tasDFT
056300060420     C                   EVAL      VAFNFT = tasNFT
056400060420     C                   EVAL      VAFFIV = tasFIV
056500060420     C                   EVAL      VAFDIV = tasDIV
056600060420     C                   EVAL      VAFDRT = tasDRT
056700060420     C                   EVAL      VAFRMN = tasRMN
056800060420     C****** NON GESTISCO VARIE PARTICOLARITA DEL RIFERIMENTO MITTENTE ALFA *******
056900060420     C                   EVAL      VAFRMA = ta4NOT
057000060420     C*****************************************************************************
057100060420     C                   EVAL      VAFFTC = tasFTC
057200060420     C                   EVAL      VAFTC2 = tasTC2
057300060420     C                   EVAL      VAFTSP = tasTSP
057400060420     C                   EVAL      VAFFAP = tasFAP
057500060420     C                   EVAL      VAFFIN = tasFIN
057600060420     C                   EVAL      VAFCTR = tasCTR
057700060420     C                   EVAL      VAFNAS = tasNAS
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
060000151020     C                   IF        tasFPF <> 'D'
060100060420     C                   Z-ADD     *zeros        VAFPKC
060200151020     C                   ENDIF
060300060420     C                   ENDIF
060400060420     C*
060500060420     C                   ENDSR
060600060420
060700060420
060800060420
060900060420     C     repVARIE      BEGSR
061000060420     C*
061100060420     C* Inizializzo la DS d procedura esterna
061200060420     C                   CLEAR                   TRUL86DS
061300060420     C                   EVAL      UL86ITLA = 'E'
061400060420     C                   EVAL      UL86IAAS = tasAAS
061500060420     C                   EVAL      UL86ILNP = tasLNP
061600060420     C                   EVAL      UL86INRS = tasNRS
061700060420     C                   EVAL      UL86INSP = tasNSP
061800060420     C                   EVAL      UL86ITBL = tasTBL
061900060420     C                   CALL      'TRUL86R'
062000060420     C                   PARM                    TRUL86DS
062100060420     C*
062200060420     C                   MOVEA(P)  UL86OOUT      skVARIE
062300060420     C*
062400060420     C* Valorizzo VARIA 1
062500060420     C                   EVAL      jVARIE = 1
062600060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
062700060420     C                   EVAL      VAFSV1 = dVARIA_COD
062800060420     C                   EVAL      VAFVA1 = dVARIA_VAL
062900060420     C*
063000060420     C* Valorizzo VARIA 2
063100060420     C                   EVAL      jVARIE = 2
063200060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
063300060420     C                   EVAL      VAFSV2 = dVARIA_COD
063400060420     C                   EVAL      VAFVA2 = dVARIA_VAL
063500060420     C*
063600060420     C* Valorizzo VARIA 3
063700060420     C                   EVAL      jVARIE = 3
063800060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
063900060420     C                   EVAL      VAFSV3 = dVARIA_COD
064000060420     C                   EVAL      VAFVA3 = dVARIA_VAL
064100060420     C*
064200060420     C* Valorizzo VARIA 4
064300060420     C                   EVAL      jVARIE = 4
064400060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
064500060420     C                   EVAL      VAFSV4 = dVARIA_COD
064600060420     C                   EVAL      VAFVA4 = dVARIA_VAL
064700060420     C*
064800060420     C* Valorizzo VARIA 5
064900060420     C                   EVAL      jVARIE = 5
065000060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
065100060420     C                   EVAL      VAFSV5 = dVARIA_COD
065200060420     C                   EVAL      VAFVA5 = dVARIA_VAL
065300060420     C*
065400060420     C* Valorizzo VARIA 6
065500060420     C                   EVAL      jVARIE = 6
065600060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
065700060420     C                   EVAL      VAFSV6 = dVARIA_COD
065800060420     C                   EVAL      VAFVA6 = dVARIA_VAL
065900060420     C*
066000060420     C* Valorizzo VARIA 7
066100060420     C                   EVAL      jVARIE = 7
066200060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
066300060420     C                   EVAL      VAFSV7 = dVARIA_COD
066400060420     C                   EVAL      VAFVA7 = dVARIA_VAL
066500060420     C*
066600060420     C* Valorizzo VARIA 8
066700060420     C                   EVAL      jVARIE = 8
066800060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
066900060420     C                   EVAL      VAFSV8 = dVARIA_COD
067000060420     C                   EVAL      VAFVA8 = dVARIA_VAL
067100060420     C*
067200060420     C* Totalizzo tutte le restanti varie
067300060420     C                   DOW       jVARIE <= %elem(skVARIE)
067400060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
067500060420     C                   EVAL      VAFVAX = VAFVAX + dVARIA_VAL
067600060420     C                   ADD       1             jVARIE
067700060420     C                   ENDDO
067800060420     C*
067900060420     C                   ENDSR
068000060418
068100060418
068200060419
068300060418     C     cartab        BEGSR
068400060418     C*
068500060419     C* Reperisco tutti i codici clienti che x la fatturazione sono unificati sul cliente richiesto
068600060418     C                   Z-ADD     *zeros        jVAF
068700060420     C                   CLEAR                   DS4C
068800060418     C                   EVAL      tblKUT = 1
068900060418     C                   EVAL      tblCOD = '4C'
069000060419     C                   EVAL      tblKEY = *blanks
069100060418     C     KEYtabel_P    SETLL     TABEL00F
069200060418     C                   IF        %found(TABEL00F)
069300060418     C     KEYtabel_P    READE     TABEL00F
069400060418     C                   DOW       not %eof(TABEL00F)
069500060418     C                   IF        tblFLG = *blanks
069600060418     C                   EVAL      DS4C = tblUNI
069700060418     C                   IF        §4CFKS = wKSUVAF
069800060418     C                   ADD       1             jVAF
069900060418     C                   MOVEL(P)  tblKEY        skVAF(jVAF)
070000060418     C                   ENDIF
070100060418     C                   ENDIF
070200060418     C     KEYtabel_P    READE     TABEL00F
070300060418     C                   ENDDO
070400060418     C                   ENDIF
070500060419     C*
070600060419     C* Reperisco i P.O. IVA da considerare x le fatture fine mese clienti codificati
070700060419     C                   CLEAR                   DSQC2
070800060419     C                   Z-ADD     1             tblKUT
070900060419     C                   MOVEL(P)  'QC'          tblCOD
071000060419     C                   MOVEL(P)  '2'           tblKEY
071100060419     C     KEYtabel      CHAIN     TABEL00F
071200060419     C                   IF        %found(TABEL00F)
071300060420     C                   IF        tblFLG = *blanks
071400060419     C                   MOVEL(P)  tblUNI        DSQC2
071500060419     C                   ENDIF
071600060420     C                   ENDIF
071700060420     C*
071800060420     C** Reperisco campi standard tassazione
071900060420     C                   CLEAR                   DSQT1
072000060420     C                   Z-ADD     1             tblKUT
072100060420     C                   MOVEL     'QT'          tblCOD
072200060420     C                   MOVEL     1             tblKEY
072300060420     C     KEYtabel      CHAIN     TABEL00F
072400060420     C                   IF        %found(TABEL00F)
072500060420     C                   IF        tblFLG = *blanks
072600060420     C                   MOVEL     TBLUNI        DSQT1
072700060420     C                   ENDIF
072800060420     C                   ENDIF
072900060418     C*
073000060418     C                   ENDSR
073100991027
073200060419
073300060419
073400060419     C     chkPAR        BEGSR
073500060419     C*
073600060419     C* Estrapolo il codice cliente unificante (unificante secondo tabella 4C) d fatturazione
073700060419     C                   EVAL      wKSUVAFa = %subst(prmppt:2:7)
073800060419     C                   MOVE(P)   wKSUVAFa      wKSUVAF
073900060419     C*
074000060419     C* Se presente nei parametri la forzatura periodo fatturazione (anno + mese) considero quello
074100060419     C* altrimenti considero il mese precedente a quello corrente
074200060419     C                   EVAL      wAAAAMMa = %subst(prmppt:9:6)
074300060419     C                   MOVE(P)   wAAAAMMa      wAAAAMM
074400060419     C                   IF        wAAAAMM = *zeros
074500060419     C                   Z-ADD     wAAAAMMcor    wPERIODO
074600060419     C                   EVAL      wMM = wMM - 1                                * mese precedente
074700060419     C                   ELSE
074800060419     C                   Z-ADD     wAAAAMM       wPERIODO
074900060419     C                   ENDIF
075000060419     C*
075100060420     C* Calcolo il primo giorno del mese successivo
075200060419     C                   IF        wMM = 12
075300060420     C***                EVAL      wDataFIN = 0101*10000+wAAAA+1
075400060420     C                   EVAL      wDataFIN = (wAAAA+1)*10000+0101
075500060419     C                   ELSE
075600060420     C***                EVAL      wDataFIN = 01*1000000+(wMM+1)*10000+wAAAA
075700060420     C                   EVAL      wDataFIN = wAAAA*10000+(wMM+1)*100+01
075800060419     C                   ENDIF
075900060420     C*
076000060420     C* Calcolo l'ultimo giorno del mese precedente
076100060420     C     *ISO          MOVE      wDataFIN      wDataISO
076200060420     C     wDataISO      SUBDUR    1:*D          wDataISO
076300060420     C     *ISO          MOVE      wDataISO      wDataINZ
076400060420     C*
076500060420     C* Calcolo il primo giorno del mese precedente
076600060420     C***                IF        wMM = 01
076700060420     C***                EVAL      wDataINZ = 0112*10000+wAAAA-1
076800060420     C***                ELSE
076900060420     C***                EVAL      wDataINZ = 01*1000000+(wMM-1)*10000+wAAAA
077000060420     C***                ENDIF
077100060419     C*
077200060419     C                   ENDSR
077300151016
077400151016
077500151016
077600151016     C     RTVAR5FAT     BEGSR
077700151016     C*
077800151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
077900151016     C                   SETOFF                                       7071
078000151016     C                   EVAL      ar5TRD = 'FAT'
078100151016     C                   CLEAR                   DAR5FAT
078200151016     C     KEYar531_P    CHAIN     FIAR531C
078300151016     C                   IF        %found(FIAR531C)
078400151016     C                   MOVEL     AR5UNI        DAR5FAT
078500151016     C                   IF        §AR5PKTAS > *zeros
078600151016     C                   SETON                                        70
078700151016     C                   EVAL      VAFPKC = §AR5PKTAS
078800151016     C                   ENDIF
078900151016     C                   IF        §AR5VLTAS > *zeros
079000151016     C                   SETON                                        71
079100151016     C***                EVAL      VAFVLC = §AR5VLTAS
079200151016     C                   ENDIF
079300151016     C                   ENDIF
079400151016     C*
079500151016     C                   ENDSR
079600060418
079700060419
079800060418
079900991027      /TITLE Operazioni iniziali.
080000991027     C     *inzsr        BEGSR
080100991027     C*
080200991027     C     *ENTRY        PLIST
080300991027     C                   parm                    prmppt
080400991027     C     wrkesito      parm      wrkesito      prmesito
080500060419     C*
080600060419     C* CALCOLA LA DATA CORRENTE
080700060419     C                   time                    wn14             14 0
080800060419     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
080900060419     C                   z-add     wn8           g08dat
081000060419     C                   z-add     *zeros        g08inv
081100060419     C                   movel     '0'           g08err
081200060419     C                   call      'XSRDA8'
081300060419     C                   parm                    wlbda8
081400060419     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
081500060419     C                   movel     datcor        wAAAAMMcor        6 0
081600060413     C*
081700060413     C* Definizione chiavi di lettura
081800060419     C*
081900060419     C* TABEL00F - Completa
082000060419     C     KEYtabel      KLIST
082100060419     C                   KFLD                    tblKUT
082200060419     C                   KFLD                    tblCOD
082300060419     C                   KFLD                    tblKEY
082400060418     C*
082500060418     C* TABEL00F - Parziale
082600060418     C     KEYtabel_P    KLIST
082700060418     C                   KFLD                    tblKUT
082800060418     C                   KFLD                    tblCOD
082900060413     C*
083000060413     C* TITAS31L - Completa
083100060413     C     KEYtas31      KLIST
083200060413     C                   KFLD                    tasKSC
083300060420     C                   KFLD                    tasDFT
083400060413     C*
083500060413     C* TNCSB03L - Completa
083600060413     C     KEYcsb03      KLIST
083700060413     C                   KFLD                    tasAAS
083800060413     C                   KFLD                    tasLNP
083900060413     C                   KFLD                    tasNRS
084000060413     C                   KFLD                    tasNSP
084100060418     C*
084200060418     C* TIGCP51L - Parziale
084300060418     C     KEYgcp51_P    KLIST
084400060418     C                   KFLD                    tasAAS
084500060418     C                   KFLD                    tasLNP
084600060418     C                   KFLD                    tasNRS
084700060418     C                   KFLD                    tasNSP
084800060418     C*
084900060418     C* TITA430C - Completa
085000060418     C     KEYta430      KLIST
085100060418     C                   KFLD                    tasAAS
085200060418     C                   KFLD                    tasLNP
085300060418     C                   KFLD                    tasNRS
085400060418     C                   KFLD                    tasNSP
085500060418     C                   KFLD                    ta4TRC
085600060420     C*
085700060420     C* TITAA30C - Completa
085800060420     C     KEYtaa30      KLIST
085900060420     C                   KFLD                    tasAAS
086000060420     C                   KFLD                    tasLNP
086100060420     C                   KFLD                    tasNRS
086200060420     C                   KFLD                    tasNSP
086300060420     C                   KFLD                    taaTRC
086400060420     C*
086500060420     C* FIAR531C - Parziale
086600060420     C     KEYar531_P    KLIST
086700060420     C                   KFLD                    tasAAS
086800060420     C                   KFLD                    tasLNP
086900060420     C                   KFLD                    tasNRS
087000060420     C                   KFLD                    tasNSP
087100060420     C                   KFLD                    ar5TRD
087200991027     C*
087300991027     C                   ENDSR
