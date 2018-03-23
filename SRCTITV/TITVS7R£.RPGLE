000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400060413     FTITAS31C  IF   E           K DISK
000500060420     FTITAA30C  IF   E           K DISK
000600060418     FTIGCP51L  IF   E           K DISK
000700060413     FTNCSB03L  IF   E           K DISK
000800060420     FFIAR531C  IF   E           K DISK
000900060418     FTABEL00F  IF   E           K DISK
001000090309     FTIVAWWWT  UF A E             DISK    commit
001100060331
001200060418     D****
001300060418     D* SCHIERE DI WRK
001400060418     D****
001500060418     D skVAF           s              7  0 inz dim(1000)
001600060418     D jVAF            s              4  0 inz
001700060420
001800060420     D*------------------
001900060420     D* DS REPERIMENTO DATI CLIENTE
002000060420     D*-------------------
002100060420     D BS69DS        E DS                  EXTNAME(TIBS69DS)
002200060420     D ACODS         E DS                  EXTNAME(CNACO00F)
002300060420     D INDDS         E DS                  EXTNAME(CNIND00F)
002400060420     D CLPDS         E DS                  EXTNAME(CNCLP00F)
002500060420     D CLSDS         E DS                  EXTNAME(FNCLS00F)
002600060419
002700060419     D*------------
002800060420     D* DS ESTERNE VARIE
002900060419     D*------------
003000060420     D DSVAF         E DS                  EXTNAME(TIVAF00T)
003100060420     D DSVAFA        E DS                  EXTNAME(TIVAFAWT)
003200060420     D TRUL86DS      E DS
003300060420     D DAR5BNB       E DS
003400151016     D DAR5FAT       E DS
003500060420     D DSQT1         E DS
003600060420     D DS4C          E DS
003700160209     D DTA4A         E DS
003800060420
003900060420     D*------------
004000060420     D* SCHIERE SIGLE E DESCRIZIONI VARIE
004100060420     D*------------
004200060420     D skVARIE         S                   LIKE(dsVARIA) DIM(50) INZ
004300060420     D jVARIE          S              2  0 INZ
004400060420
004500060420     D*------------------
004600060420     D* DS RIDEFINIZIONE OUTPUT ELEMENTO PROCEDURA TRUL86R
004700060420     D*------------------
004800060420     D dsVARIA         DS
004900060420     D  dVARIA_COD                    1
005000060420     D  dVARIA_VAL                   13  3
005100060420
005200060420     D*------------
005300060420     D* DS RIDEFINIZIONE TABELLA "QC" TIPO RECORD "2"
005400060420     D*------------
005500060420     D DSQC2         E DS
005600060419
005700060419     D*------------------
005800060419     D* DS X PASSAGGIO PARAMETRI
005900060419     D*------------------
006000060419     D                 DS
006100060419     D  wPERIODO               1      6  0
006200060419     D  wMM                    5      6  0
006300060419     D  wAAAA                  1      4  0
006400060418
006500060331     D****
006600060331     D* VARIABILI DI WRK
006700060331     D****
006800991027     D prmppt          s             50
006900991027     D prmesito        s              1
007000991027     D wrkesito        s                   like(prmesito)
007100060418     D wKSUVAFa        s              7
007200060418     D wKSUVAF         s              7  0
007300060420     D wAAAAMMa        s              6
007400060420     D wAAAAMM         s              6  0
007500060420     D wDataINZ        s              8  0
007600060420     D wDataFIN        s              8  0
007700060420     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
007800060420     D wTara           s                   like(§QTTPC)
007900060420     D wNtara          s                   like(TASPKF)
008000160209
008100160209     D*--------------------
008200160209     D* DEFINIZIONI A PROCEDURE ESTERNE
008300160209     D*--------------------
008400160209     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
008500160209
008600991027
008700060419     C*
008800060419     C* Verifico ed effettuo considerazioni sui parametri ricevuti in input
008900060419     C                   EXSR      chkPAR
009000060418     C*
009100060418     C* Carico le tabelle occorrenti
009200060419     C                   EXSR      carTAB
009300060418     C*
009400060418     C* Verifico se già presenti dati nel file d output
009500030908     C                   EXSR      chkRECFILMBR
009600060418     C*
009700060418     C* Effettuo elaborazione
009800991027     C                   EXSR      traduci
009900060420     C*
010000060420     C* Effettuo chiamata x chiusura procedure esterne
010100060420     C                   CLEAR                   TRUL86DS
010200060420     C                   EVAL      UL86ITLA = 'C'
010300060420     C                   CALL      'TRUL86R'
010400060420     C                   PARM                    TRUL86DS
010500160209     C*
010600160209     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
010700160209     C                   CALL      'UBTA400R'
010800160209     C                   PARM                    UBTA4IOPZ
010900160209     C                   PARM      'C'           UBTA4ITLA
011000160209     C                   PARM                    UBTA4IAAS
011100160209     C                   PARM                    UBTA4ILNP
011200160209     C                   PARM                    UBTA4INRS
011300160209     C                   PARM                    UBTA4INSP
011400160209     C                   PARM                    UBTA4ITRC
011500160209     C                   PARM                    UBTA4OERR
011600160209     C                   PARM                    UBTA4ODS
011700160209     C                   PARM                    UBTA4OLEN
011800160209     C                   PARM                    UBTA4ODATI
011900060418     C*
012000921023     C                   SETON                                        LR
012100060413
012200060413
012300030908
012400030908     C     chkRECFILMBR  BEGSR
012500030908     C*
012600060413     C                   READ      TIVAWWWT                               55
012700030908     C*
012800030908     C                   ENDSR
012900060413
013000060413
013100991027
013200991027     C     traduci       BEGSR
013300030325     C*
013400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013500030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
013600060420     C                   EVAL      VAWDTA  = '"VAFAAS"'+','+
013700060420     C                                       '"VAFLNP"'+','+
013800060420     C                                       '"VAFNRS"'+','+
013900060420     C                                       '"VAFNSP"'+','+
014000060420     C                                       '"VAFTBL"'+','+
014100060420     C                                       '"VAFMGS"'+','+
014200060420     C                                       '"VAFKSC"'+','+
014300060420     C                                       '"VAFLNA"'+','+
014400060420     C                                       '"VAFNCL"'+','+
014500060420     C                                       '"VAFPKB"'+','+
014600060420     C                                       '"VAFVLF"'+','+
014700060420     C                                       '"VAFQFT"'+','+
014800060420     C                                       '"VAFPOR"'+','+
014900060420     C                                       '"VAFSV1"'+','+
015000060420     C                                       '"VAFVA1"'+','+
015100060420     C                                       '"VAFSV2"'+','+
015200060420     C                                       '"VAFVA2"'+','+
015300060420     C                                       '"VAFSV3"'+','+
015400060420     C                                       '"VAFVA3"'+','+
015500060420     C                                       '"VAFSV4"'+','+
015600060420     C                                       '"VAFVA4"'+','+
015700060420     C                                       '"VAFSV5"'+','+
015800060420     C                                       '"VAFVA5"'+','+
015900060420     C                                       '"VAFSV6"'+','+
016000060420     C                                       '"VAFVA6"'+','+
016100060420     C                                       '"VAFSV7"'+','+
016200060420     C                                       '"VAFVA7"'+','+
016300060420     C                                       '"VAFSV8"'+','+
016400060420     C                                       '"VAFVA8"'+','+
016500060420     C                                       '"VAFVAX"'+','+
016600060420     C                                       '"VAFIMV"'+','+
016700060420     C                                       '"VAFDFT"'+','+
016800060420     C                                       '"VAFNFT"'+','+
016900060420     C                                       '"VAFFIV"'+','+
017000060420     C                                       '"VAFDIV"'+','+
017100060420     C                                       '"VAFDRT"'+','+
017200060420     C                                       '"VAFRMN"'+','+
017300060420     C                                       '"VAFRMA"'+','+
017400060420     C                                       '"VAFRMX"'+','+
017500060420     C                                       '"VAFFTC"'+','+
017600060420     C                                       '"VAFTC2"'+','+
017700060420     C                                       '"VAFTSP"'+','+
017800060420     C                                       '"VAFFAP"'+','+
017900060420     C                                       '"VAFFIN"'+','+
018000060420     C                                       '"VAFCTR"'+','+
018100060420     C                                       '"VAFNAS"'+','+
018200060420     C                                       '"VAFCTS"'+','+
018300060420     C                                       '"VAFCAD"'+','+
018400060420     C                                       '"VAFLOD"'+','+
018500060420     C                                       '"VAFPRD"'+','+
018600060420     C                                       '"VAFNZD"'+','+
018700060420     C                                       '"VAFVAS"'+','+
018800060420     C                                       '"VAFIAS"'+','+
018900060420     C                                       '"VAFFPC"'+','+
019000060420     C                                       '"VAFPKC"'+','+
019100060420     C                                       '"VAFFVC"'+','+
019200060420     C                                       '"VAFVLC"'+','+
019300060420     C                                       '"VAFRSD"'+','+
019400060420     C                                       '"VAFIND"'+','+
019500060420     C                                       '"VAFRSM"'+','+
019600060420     C                                       '"VAFINM"'+','+
019700060420     C                                       '"VAFCAM"'+','+
019800060420     C                                       '"VAFLOM"'+','+
019900060420     C                                       '"VAFPRM"'+','+
020000060420     C                                       '"VAFNZM"'+','+
020100060420     C                                       '"VAFRMO"'+','+
020200060420     C                                       '"VAFCMO"'+','+
020300060420     C                                       '"VAFNMO"'+','+
020400060418     C                                      '"GIACENZA"'+','+
020500060418     C                                      '"CONS_AI_PIANI"'+','+
020600060418     C                                      '"CONS_APPUNTAMENTO"'+','+
020700060418     C                                      '"CONS_LOCALITA_DISAGIATA"'+','+
020800060418     C                                      '"CONS_ISOLE_MINORI"'+','+
020900060418     C                                      '"CONS_DISAGIATA"'+','+
021000060418     C                                      '"POD_IMAGE"'+','+
021100060418     C                                      '"DIROTTAMENTO"'+','+
021200060418     C                                      '"IMPORTO_CONTRASSEGNO"'+','+
021300060420     C                                      '"DIVISA_CONTRASSEGNO"'
021400030325     C*
021500060413     C                   WRITE     TIVAW000
021600030325     C                   ENDIF
021700991027     C*
021800060418     C* Ciclo x tutti i codici clienti ricondotti al cliente unificante fatture (tab. 4C)
021900060418     C                   EVAL      jVAF = 1
022000060418     C                   DOW       jVAF <= %elem(skVAF)
022100060418     C                   IF        skVAF(jVAF) = *zeros
022200060418     C                   LEAVE
022300060418     C                   ELSE
022400060418     C                   EVAL      tasKSC = skVAF(jVAF)
022500060420     C                   EVAL      tasDFT = wDataINZ
022600060420     C     KEYtas31      SETLL     TITAS31C
022700060413     C                   IF        %found(TITAS31C)
022800060420     C     skVAF(jVAF)   READE     TITAS31C
022900060413     C                   DOW       not %eof(TITAS31C)
023000060418     C*
023100060419     C* Inizializzo il flag che condiziona l'elaborazione del record bolla corrente
023200060419     C                   MOVEL     'S'           wRecOK            1
023300060420     C                   MOVEL     'N'           wLeave            1
023400060419     C*
023500060419     C* Effettuo verifiche validità record
023600060419     C                   EXSR      chkREC
023700060419     C*
023800060419     C* Se record da considerare procedo con le considerazioni
023900060419     C                   IF        wRecOK = 'S'
024000060419     C*
024100060419     C* Re inizializzo il flag che indica se la bolla corrente è da considerare
024200060419     C                   MOVEL     'N'           wRecOK            1
024300060418     C*
024400060418     C* Inizializzo tutti i flag d wrk relativi agli attributi della bolla corrente
024500060418     C                   MOVEL     *blanks       wGIAC             1
024600060418     C                   MOVEL     *blanks       wPIAN             1
024700060418     C                   MOVEL     *blanks       wAPPU             1
024800060418     C                   MOVEL     *blanks       wLDIS             1
024900060418     C                   MOVEL     *blanks       wISOL             1
025000060418     C                   MOVEL     *blanks       wCDIS             1
025100060418     C                   MOVEL     *blanks       wPODI             1
025200060418     C                   MOVEL     *blanks       wDIRO             1
025300060418     C*
025400060418     C* Aggancio il file giacenze della bolla corrente
025500060418     C     KEYgcp51_P    CHAIN     TIGCP51L
025600060418     C                   IF        %found(TIGCP51L)
025700060418     C                   MOVEL     'S'           wGIAC
025800060418     C                   MOVEL     'S'           wRecOK
025900060418     C                   ENDIF
026000060413     C*
026100060413     C* Aggancio il file contrassegni della bolla corrente
026200060421     C                   Z-ADD     *zeros        csbCAS
026300060421     C                   MOVEL     *blanks       csbVCA
026400060413     C     KEYcsb03      CHAIN     TNCSB03L
026500060413     C                   IF        %found(TNCSB03L)
026600060421     C                   IF        csbSTA <> 9
026700060418     C                   MOVEL     'S'           wRecOK
026800060421     C                   ENDIF
026900060418     C                   ENDIF
027000060418     C*
027100060418     C* Verifica se APPUNTAMENTO
027200060418     C                   IF        tasFTC = 'A' or
027300060418     C                             tasTC2 = 'A'
027400060418     C                   MOVEL     'S'           wRecOK
027500060418     C                   MOVEL     'S'           wAPPU
027600060418     C                   ENDIF
027700060418     C*
027800060418     C* Verifica se AI PIANI
027900060418     C                   IF        tasFTC = 'P' or
028000060418     C                             tasTC2 = 'P'
028100060418     C                   MOVEL     'S'           wRecOK
028200060418     C                   MOVEL     'S'           wPIAN
028300060418     C                   ENDIF
028400060418     C*
028500060418     C* Verifica se CONSEGNA DISAGIATA
028600060418     C                   IF        tasFTC = 'X' or
028700060418     C                             tasTC2 = 'X'
028800060418     C                   MOVEL     'S'           wRecOK
028900060418     C                   MOVEL     'S'           wCDIS
029000060418     C                   ENDIF
029100060418     C*
029200060418     C* Verifica se LOCALITA DISAGIATA
029300060418     C                   IF        tasFIN = 'D'
029400060418     C                   MOVEL     'S'           wRecOK
029500060418     C                   MOVEL     'S'           wLDIS
029600060418     C                   ENDIF
029700060418     C*
029800060418     C* Verifica se ISOLA MINORE
029900060418     C                   IF        tasFIN = 'I'
030000060418     C                   MOVEL     'S'           wRecOK
030100060418     C                   MOVEL     'S'           wISOL
030200060418     C                   ENDIF
030300060418     C*
030400060418     C* Verifica se P.O.D. IMAGE
030500060418     C                   IF        tasCBO = 'FI'
030600060418     C                   MOVEL     'S'           wRecOK
030700060418     C                   MOVEL     'S'           wPODI
030800060418     C                   ENDIF
030900060418     C*
031000060418     C* Verifica se DIROTTAMENTO
031100060418     C                   IF        tasCBO = 'F*'
031200060418     C                   MOVEL     'S'           wRecOK
031300060418     C                   MOVEL     'S'           wDIRO
031400060418     C                   ENDIF
031500060418     C*
031600060418     C* Se presente numero fattura considero sempre la bolla
031700060418     C                   IF        tasNFT > *zeros
031800060418     C                   MOVEL     'S'           wRecOK
031900060418     C                   ENDIF
032000060413     C*
032100060413     C* Se tutto ok procedo con la scrittura del buffer d out
032200060420     C                   IF        wRecOK = 'S'
032300160209     C*
032400160209     C* Reperisco tipo record 'A' da TITA4
032500160209     C                   EXSR      RTVTA4A
032600060420     C*
032700060420     C* Inizializzo il buffer del file d output e delle ds d ridefinizione dati
032800060420     C                   CLEAR                   TIVAW000
032900060420     C                   CLEAR                   DSVAF
033000060420     C                   CLEAR                   DSVAFA
033100060420     C*
033200060420     C* Valorizzo i dati del destinatario nn già presenti sul TIVAF
033300060420     C                   EVAL      VHFRSD = TASRSD
033400060420     C                   EVAL      VHFIND = TASIND
033500060420     C*
033600060420     C* X reperire i dati del mittente verifico il codice del cliente mittente:
033700060420     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
033800060420     C                   MOVE(P)   tasCCM        wCLI              4 0
033900060420     C                   IF        wCLI =    0 OR
034000060420     C                             wCLI = 8888 OR
034100060420     C                             wCLI = 9999
034200060420     C                   EVAL      taaTRC = 'M'
034300060420     C     KEYtaa30      CHAIN     titaa30c
034400060420     C                   IF        %found(titaa30c)
034500060420     C                   EVAL      VHFRSM = TAARSC
034600060420     C                   EVAL      VHFINM = TAAIND
034700060420     C                   EVAL      VHFCAM = TAACAP
034800060420     C                   EVAL      VHFLOM = TAALOC
034900060420     C                   EVAL      VHFPRM = TAAPRV
035000060420     C                   EVAL      VHFNZM = TAANAZ
035100060420     C                   ENDIF
035200060420     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
035300060420     C                   ELSE
035400060420     C                   CLEAR                   BS69DS
035500060420     C                   CLEAR                   ACODS
035600060420     C                   CLEAR                   INDDS
035700060420     C                   CLEAR                   CLPDS
035800060420     C                   CLEAR                   CLSDS
035900060420     C                   Z-ADD     tasCCM        I69KAC
036000060420     C                   Z-ADD     tasCCM        I69KIN
036100060420     C                   CALL      'TIBS69R'
036200060420     C                   PARM                    BS69DS
036300060420     C                   PARM                    ACODS
036400060420     C                   PARM                    INDDS
036500060420     C                   PARM                    CLPDS
036600060420     C                   PARM                    CLSDS
036700060420     C     O69ERR        IFNE      '1'
036800060420     C                   EVAL      VHFRSM = ACORAG
036900060420     C                   EVAL      VHFINM = INDVIA
037000060420     C                   MOVEL(P)  INDCAP        VHFCAM
037100060420     C                   EVAL      VHFLOM = INDCIT
037200060420     C                   EVAL      VHFPRM = INDPRV
037300060420     C                   EVAL      VHFNZM = INDSTA
037400060420     C                   ENDIF
037500060420     C                   ENDIF
037600060420     C*
037700060420     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
037800060420     C                   EVAL      taaTRC = 'O'
037900060420     C     KEYtaa30      CHAIN     titaa30c
038000060420     C                   IF        %found(titaa30c)
038100060420     C                   EVAL      VHFRMO = TAARSC
038200060420     C                   EVAL      VHFCMO = TAACAP
038300060420     C                   EVAL      VHFNMO = TAANAZ
038400060420     C                   ENDIF
038500060420     C*
038600060420     C* Se particolità varia = 'O' recupero da FIAR5 il numero colli originali
038700060420     C                   IF        %subst(tasGVA:1:1) = 'O'
038800060420     C                   EVAL      ar5TRD = 'BNB'
038900060420     C                   CLEAR                   DAR5BNB
039000060420     C     KEYar531_P    CHAIN     FIAR531C
039100060420     C                   IF        %found(FIAR531C)
039200060420     C                   MOVEL     AR5UNI        DAR5BNB
039300060420     C                   ENDIF
039400060420     C* Colli originali
039500060420     C                   EVAL      VAFNCL = §AR5BCOR
039600060420     C* Colli TITAS
039700060420     C                   ELSE
039800060420     C                   Z-ADD     TASNCL        VAFNCL
039900060420     C                   ENDIF
040000060420     C*
040100060420     C* Eseguo reperimento varie
040200060420     C                   EXSR      repVARIE
040300060420     C*
040400060420     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
040500060420     C                   EXSR      TAStoVAF
040600060420     C*
040700151016     C* Eseguo eventuali "forzature"
040800151016     C                   EXSR      RTVAR5FAT
040900151016     C*
041000060420     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'Q'))+','+
041100060420     C                                       %trim(%editc(VAFLNP:'Q'))+','+
041200060420     C                                       %trim(%editc(VAFNRS:'Q'))+','+
041300060420     C                                       %trim(%editc(VAFNSP:'Q'))+','+
041400060420     C                                          '"'+%trim(VAFTBL)+'"'+','+
041500060420     C                                       %trim(%editc(VAFMGS:'Q'))+','+
041600060420     C                                       %trim(%editc(VAFKSC:'Q'))+','+
041700060420     C                                       %trim(%editc(VAFLNA:'Q'))+','+
041800060420     C                                       %trim(%editc(VAFNCL:'Q'))+','+
041900060420     C                                       %trim(%editc(VAFPKB:'Q'))+','+
042000060420     C                                       %trim(%editc(VAFVLF:'Q'))+','+
042100060420     C                                       %trim(%editc(VAFQFT:'Q'))+','+
042200060420     C                                       %trim(%editc(VAFPOR:'Q'))+','+
042300060420     C                                          '"'+%trim(VAFSV1)+'"'+','+
042400060420     C                                       %trim(%editc(VAFVA1:'Q'))+','+
042500060420     C                                          '"'+%trim(VAFSV2)+'"'+','+
042600060420     C                                       %trim(%editc(VAFVA2:'Q'))+','+
042700060420     C                                          '"'+%trim(VAFSV3)+'"'+','+
042800060420     C                                       %trim(%editc(VAFVA3:'Q'))+','+
042900060420     C                                          '"'+%trim(VAFSV4)+'"'+','+
043000060420     C                                       %trim(%editc(VAFVA4:'Q'))+','+
043100060420     C                                          '"'+%trim(VAFSV5)+'"'+','+
043200060420     C                                       %trim(%editc(VAFVA5:'Q'))+','+
043300060420     C                                          '"'+%trim(VAFSV6)+'"'+','+
043400060420     C                                       %trim(%editc(VAFVA6:'Q'))+','+
043500060420     C                                          '"'+%trim(VAFSV7)+'"'+','+
043600060420     C                                       %trim(%editc(VAFVA7:'Q'))+','+
043700060420     C                                          '"'+%trim(VAFSV8)+'"'+','+
043800060420     C                                       %trim(%editc(VAFVA8:'Q'))+','+
043900060420     C                                       %trim(%editc(VAFVAX:'Q'))+','+
044000060420     C                                       %trim(%editc(VAFIMV:'Q'))+','+
044100060420     C                                       %trim(%editc(VAFDFT:'Q'))+','+
044200060420     C                                       %trim(%editc(VAFNFT:'Q'))+','+
044300060420     C                                       %trim(%editc(VAFFIV:'Q'))+','+
044400060420     C                                          '"'+%trim(VAFDIV)+'"'+','+
044500060420     C                                       %trim(%editc(VAFDRT:'Q'))+','+
044600060420     C                                       %trim(%editc(VAFRMN:'Q'))+','+
044700060420     C                                          '"'+%trim(VAFRMA)+'"'+','+
044800060420     C                                          '"'+%trim(VAFRMX)+'"'+','+
044900060420     C                                          '"'+%trim(VAFFTC)+'"'+','+
045000060420     C                                          '"'+%trim(VAFTC2)+'"'+','+
045100060420     C                                          '"'+%trim(VAFTSP)+'"'+','+
045200060420     C                                          '"'+%trim(VAFFAP)+'"'+','+
045300060420     C                                          '"'+%trim(VAFFIN)+'"'+','+
045400060420     C                                       %trim(%editc(VAFCTR:'Q'))+','+
045500060420     C                                          '"'+%trim(VAFNAS)+'"'+','+
045600060420     C                                          '"'+%trim(VAFCTS)+'"'+','+
045700060420     C                                          '"'+%trim(VAFCAD)+'"'+','+
045800060420     C                                          '"'+%trim(VAFLOD)+'"'+','+
045900060420     C                                          '"'+%trim(VAFPRD)+'"'+','+
046000060420     C                                          '"'+%trim(VAFNZD)+'"'+','+
046100060420     C                                          '"'+%trim(VAFVAS)+'"'+','+
046200060420     C                                       %trim(%editc(VAFIAS:'Q'))+','+
046300060420     C                                          '"'+%trim(VAFFPC)+'"'+','+
046400060420     C                                       %trim(%editc(VAFPKC:'Q'))+','+
046500060420     C                                          '"'+%trim(VAFFVC)+'"'+','+
046600060420     C                                       %trim(%editc(VAFVLC:'Q'))+','+
046700060420     C                                          '"'+%trim(VHFRSD)+'"'+','+
046800060420     C                                          '"'+%trim(VHFIND)+'"'+','+
046900060420     C                                          '"'+%trim(VHFRSM)+'"'+','+
047000060420     C                                          '"'+%trim(VHFINM)+'"'+','+
047100060420     C                                          '"'+%trim(VHFCAM)+'"'+','+
047200060420     C                                          '"'+%trim(VHFLOM)+'"'+','+
047300060420     C                                          '"'+%trim(VHFPRM)+'"'+','+
047400060420     C                                          '"'+%trim(VHFNZM)+'"'+','+
047500060420     C                                          '"'+%trim(VHFRMO)+'"'+','+
047600060420     C                                          '"'+%trim(VHFCMO)+'"'+','+
047700060420     C                                          '"'+%trim(VHFNMO)+'"'+','+
047800060413     C                                         '"'+%trim(wGIAC)+'"'+','+
047900060413     C                                         '"'+%trim(wPIAN)+'"'+','+
048000060413     C                                         '"'+%trim(wAPPU)+'"'+','+
048100060413     C                                         '"'+%trim(wLDIS)+'"'+','+
048200060413     C                                         '"'+%trim(wISOL)+'"'+','+
048300060413     C                                         '"'+%trim(wCDIS)+'"'+','+
048400060413     C                                         '"'+%trim(wPODI)+'"'+','+
048500060413     C                                         '"'+%trim(wDIRO)+'"'+','+
048600060413     C                                      %trim(%editc(CSBCAS:'Q'))+','+
048700060413     C                                         '"'+%trim(CSBVCA)
048800930409     C*
048900060413     C                   WRITE     TIVAW000
049000060418     C                   ENDIF
049100991027     C*
049200060420     C* Se già oltre data superiore range => esco dal ciclo
049300060420     C                   IF        wLeave = 'S'
049400060420     C                   LEAVE
049500060420     C                   ENDIF
049600060420     C*
049700060420     C                   ENDIF
049800060420     C*
049900060420     C     skVAF(jVAF)   READE     TITAS31C
050000030325     C                   ENDDO
050100060418     C*
050200060418     C                   ENDIF
050300060420     C                   ENDIF
050400060418     C                   ADD       1             jVAF
050500060418     C                   ENDDO
050600060413     C*
050700030325     C                   EVAL      wrkesito = '0'
050800991027     C*
050900910830     C                   ENDSR
051000060419
051100060419
051200060419
051300060419     C     chkrec        BEGSR
051400060419     C*
051500060419     C* Considero solo le bolle del periodo richiesto
051600060419     C                   IF        wRecOK = 'S'
051700060420     C                   IF        tasDFT > wDataFIN
051800060420     C                   MOVEL     'N'           wRecOK
051900060420     C                   MOVEL     'S'           wLeave
052000060419     C                   ENDIF
052100060419     C                   ENDIF
052200060420     C*
052300060419     C* Verifica le bolle fatturate inizio mese richiesto, occorre considerare solo quelle aventi
052400060419     C* P.O. IVA uguale a zero o uguale ai valori indicati in tabella QT-2.
052500060420     C***                IF        wRecOK = 'S'
052600060420     C***                IF        tasDFT = wDataINZ
052700060420     C***                IF        tasFIV = §QCFIV  OR
052800060420     C***                          tasFIV = §QCFI2  OR
052900060420     C***                          tasFIV = *zeros
053000060420     C***                ELSE
053100060420     C***                MOVEL     'N'           wRecOK
053200060420     C***                ENDIF
053300060420     C***                ENDIF
053400060420     C***                ENDIF
053500060420     C*
053600060419     C* Verifica le bolle fatturate inizio mese successivo, occorre considerare solo quelle aventi
053700060419     C* P.O. IVA diverso da zero e diverso valori indicati in tabella QT-2.
053800060419     C                   IF        wRecOK = 'S'
053900060420     C                   IF        tasDFT = wDataFIN AND
054000060419     C                             tasFIV <> §QCFIV  AND
054100060419     C                             tasFIV <> §QCFI2  AND
054200060420     C                             tasFIV <> *zeros
054300060420     C                   MOVEL     'N'           wRecOK
054400060419     C                   ENDIF
054500060419     C                   ENDIF
054600060419     C*
054700060419     C                   ENDSR
054800060420
054900060420
055000060420
055100060420     C     TAStoVAF      BEGSR
055200060420     C*
055300060420     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
055400060420     C                   EVAL      VAFAAS = tasAAS
055500060420     C                   EVAL      VAFLNP = tasLNP
055600060420     C                   EVAL      VAFNRS = tasNRS
055700060420     C                   EVAL      VAFNSP = tasNSP
055800060420     C                   EVAL      VAFTBL = tasTBL
055900060420     C                   EVAL      VAFMGS = tasMGS
056000060420     C                   EVAL      VAFKSC = tasKSC
056100060420     C                   EVAL      VAFLNA = tasLNA
056200060420     C                   EVAL      VAFPKB = tasPKF
056300060420     C                   EVAL      VAFVLF = tasVLF
056400060420     C                   EVAL      VAFQFT = tasQFT
056500060420     C                   EVAL      VAFPOR = tasPOR
056600060420     C                   EVAL      VAFIMV = tasIMV
056700060420     C                   EVAL      VAFDFT = tasDFT
056800060420     C                   EVAL      VAFNFT = tasNFT
056900060420     C                   EVAL      VAFFIV = tasFIV
057000060420     C                   EVAL      VAFDIV = tasDIV
057100060420     C                   EVAL      VAFDRT = tasDRT
057200060420     C                   EVAL      VAFRMN = tasRMN
057300060420     C****** NON GESTISCO VARIE PARTICOLARITA DEL RIFERIMENTO MITTENTE ALFA *******
057400160209     C                   EVAL      VAFRMA = §TA4ARMA
057500060420     C*****************************************************************************
057600060420     C                   EVAL      VAFFTC = tasFTC
057700060420     C                   EVAL      VAFTC2 = tasTC2
057800060420     C                   EVAL      VAFTSP = tasTSP
057900060420     C                   EVAL      VAFFAP = tasFAP
058000060420     C                   EVAL      VAFFIN = tasFIN
058100060420     C                   EVAL      VAFCTR = tasCTR
058200160209     C                   EVAL      VAFNAS = §TA4ANAS
058300060420     C                   EVAL      VAFCTS = tasCTS
058400060420     C                   EVAL      VAFCAD = tasCAD
058500060420     C                   EVAL      VAFLOD = tasLOD
058600060420     C                   EVAL      VAFPRD = tasPRD
058700060420     C                   EVAL      VAFNZD = tasNZD
058800060420     C                   EVAL      VAFVAS = tasVAS
058900060420     C                   EVAL      VAFIAS = tasIAS
059000060420     C*
059100060420     C* Gestisco particolari considerazioni
059200060420     C                   IF        tasFPF = 'V'
059300150716     C*
059400150716     C                   IF        TASDFT <= §QTDST
059500150716     C     §QTTCO        MULT      TASNCP        WTARA
059600150716     C     TASPKC        SUB(h)    WTARA         WNTARA
059700150716     C                   Z-ADD     WNTARA        VAFPKC
059800150716     C                   ELSE
059900150716     C     §QTTPC        MULT      TASNCP        WTARA
060000150716     C     TASPKC        SUB(h)    WTARA         WNTARA
060100150716     C                   Z-ADD     WNTARA        VAFPKC
060200150716     C                   ENDIF
060300150716     C*
060400060420     C                   ELSE
060500151020     C                   IF        tasFPF <> 'D'
060600060420     C                   Z-ADD     *zeros        VAFPKC
060700151020     C                   ENDIF
060800060420     C                   ENDIF
060900060420     C*
061000060420     C                   ENDSR
061100060420
061200060420
061300060420
061400060420     C     repVARIE      BEGSR
061500060420     C*
061600060420     C* Inizializzo la DS d procedura esterna
061700060420     C                   CLEAR                   TRUL86DS
061800060420     C                   EVAL      UL86ITLA = 'E'
061900060420     C                   EVAL      UL86IAAS = tasAAS
062000060420     C                   EVAL      UL86ILNP = tasLNP
062100060420     C                   EVAL      UL86INRS = tasNRS
062200060420     C                   EVAL      UL86INSP = tasNSP
062300060420     C                   EVAL      UL86ITBL = tasTBL
062400060420     C                   CALL      'TRUL86R'
062500060420     C                   PARM                    TRUL86DS
062600060420     C*
062700060420     C                   MOVEA(P)  UL86OOUT      skVARIE
062800060420     C*
062900060420     C* Valorizzo VARIA 1
063000060420     C                   EVAL      jVARIE = 1
063100060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
063200060420     C                   EVAL      VAFSV1 = dVARIA_COD
063300060420     C                   EVAL      VAFVA1 = dVARIA_VAL
063400060420     C*
063500060420     C* Valorizzo VARIA 2
063600060420     C                   EVAL      jVARIE = 2
063700060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
063800060420     C                   EVAL      VAFSV2 = dVARIA_COD
063900060420     C                   EVAL      VAFVA2 = dVARIA_VAL
064000060420     C*
064100060420     C* Valorizzo VARIA 3
064200060420     C                   EVAL      jVARIE = 3
064300060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
064400060420     C                   EVAL      VAFSV3 = dVARIA_COD
064500060420     C                   EVAL      VAFVA3 = dVARIA_VAL
064600060420     C*
064700060420     C* Valorizzo VARIA 4
064800060420     C                   EVAL      jVARIE = 4
064900060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
065000060420     C                   EVAL      VAFSV4 = dVARIA_COD
065100060420     C                   EVAL      VAFVA4 = dVARIA_VAL
065200060420     C*
065300060420     C* Valorizzo VARIA 5
065400060420     C                   EVAL      jVARIE = 5
065500060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
065600060420     C                   EVAL      VAFSV5 = dVARIA_COD
065700060420     C                   EVAL      VAFVA5 = dVARIA_VAL
065800060420     C*
065900060420     C* Valorizzo VARIA 6
066000060420     C                   EVAL      jVARIE = 6
066100060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
066200060420     C                   EVAL      VAFSV6 = dVARIA_COD
066300060420     C                   EVAL      VAFVA6 = dVARIA_VAL
066400060420     C*
066500060420     C* Valorizzo VARIA 7
066600060420     C                   EVAL      jVARIE = 7
066700060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
066800060420     C                   EVAL      VAFSV7 = dVARIA_COD
066900060420     C                   EVAL      VAFVA7 = dVARIA_VAL
067000060420     C*
067100060420     C* Valorizzo VARIA 8
067200060420     C                   EVAL      jVARIE = 8
067300060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
067400060420     C                   EVAL      VAFSV8 = dVARIA_COD
067500060420     C                   EVAL      VAFVA8 = dVARIA_VAL
067600060420     C*
067700060420     C* Totalizzo tutte le restanti varie
067800060420     C                   DOW       jVARIE <= %elem(skVARIE)
067900060420     C                   EVAL      dsVARIA = skVARIE(jVARIE)
068000060420     C                   EVAL      VAFVAX = VAFVAX + dVARIA_VAL
068100060420     C                   ADD       1             jVARIE
068200060420     C                   ENDDO
068300060420     C*
068400060420     C                   ENDSR
068500060418
068600060418
068700060419
068800060418     C     cartab        BEGSR
068900060418     C*
069000060419     C* Reperisco tutti i codici clienti che x la fatturazione sono unificati sul cliente richiesto
069100060418     C                   Z-ADD     *zeros        jVAF
069200060420     C                   CLEAR                   DS4C
069300060418     C                   EVAL      tblKUT = 1
069400060418     C                   EVAL      tblCOD = '4C'
069500060419     C                   EVAL      tblKEY = *blanks
069600060418     C     KEYtabel_P    SETLL     TABEL00F
069700060418     C                   IF        %found(TABEL00F)
069800060418     C     KEYtabel_P    READE     TABEL00F
069900060418     C                   DOW       not %eof(TABEL00F)
070000060418     C                   IF        tblFLG = *blanks
070100060418     C                   EVAL      DS4C = tblUNI
070200060418     C                   IF        §4CFKS = wKSUVAF
070300060418     C                   ADD       1             jVAF
070400060418     C                   MOVEL(P)  tblKEY        skVAF(jVAF)
070500060418     C                   ENDIF
070600060418     C                   ENDIF
070700060418     C     KEYtabel_P    READE     TABEL00F
070800060418     C                   ENDDO
070900060418     C                   ENDIF
071000060419     C*
071100060419     C* Reperisco i P.O. IVA da considerare x le fatture fine mese clienti codificati
071200060419     C                   CLEAR                   DSQC2
071300060419     C                   Z-ADD     1             tblKUT
071400060419     C                   MOVEL(P)  'QC'          tblCOD
071500060419     C                   MOVEL(P)  '2'           tblKEY
071600060419     C     KEYtabel      CHAIN     TABEL00F
071700060419     C                   IF        %found(TABEL00F)
071800060420     C                   IF        tblFLG = *blanks
071900060419     C                   MOVEL(P)  tblUNI        DSQC2
072000060419     C                   ENDIF
072100060420     C                   ENDIF
072200060420     C*
072300060420     C** Reperisco campi standard tassazione
072400060420     C                   CLEAR                   DSQT1
072500060420     C                   Z-ADD     1             tblKUT
072600060420     C                   MOVEL     'QT'          tblCOD
072700060420     C                   MOVEL     1             tblKEY
072800060420     C     KEYtabel      CHAIN     TABEL00F
072900060420     C                   IF        %found(TABEL00F)
073000060420     C                   IF        tblFLG = *blanks
073100060420     C                   MOVEL     TBLUNI        DSQT1
073200060420     C                   ENDIF
073300060420     C                   ENDIF
073400060418     C*
073500060418     C                   ENDSR
073600991027
073700060419
073800060419
073900060419     C     chkPAR        BEGSR
074000060419     C*
074100060419     C* Estrapolo il codice cliente unificante (unificante secondo tabella 4C) d fatturazione
074200060419     C                   EVAL      wKSUVAFa = %subst(prmppt:2:7)
074300060419     C                   MOVE(P)   wKSUVAFa      wKSUVAF
074400060419     C*
074500060419     C* Se presente nei parametri la forzatura periodo fatturazione (anno + mese) considero quello
074600060419     C* altrimenti considero il mese precedente a quello corrente
074700060419     C                   EVAL      wAAAAMMa = %subst(prmppt:9:6)
074800060419     C                   MOVE(P)   wAAAAMMa      wAAAAMM
074900060419     C                   IF        wAAAAMM = *zeros
075000060419     C                   Z-ADD     wAAAAMMcor    wPERIODO
075100060419     C                   EVAL      wMM = wMM - 1                                * mese precedente
075200060419     C                   ELSE
075300060419     C                   Z-ADD     wAAAAMM       wPERIODO
075400060419     C                   ENDIF
075500060419     C*
075600060420     C* Calcolo il primo giorno del mese successivo
075700060419     C                   IF        wMM = 12
075800060420     C***                EVAL      wDataFIN = 0101*10000+wAAAA+1
075900060420     C                   EVAL      wDataFIN = (wAAAA+1)*10000+0101
076000060419     C                   ELSE
076100060420     C***                EVAL      wDataFIN = 01*1000000+(wMM+1)*10000+wAAAA
076200060420     C                   EVAL      wDataFIN = wAAAA*10000+(wMM+1)*100+01
076300060419     C                   ENDIF
076400060420     C*
076500060420     C* Calcolo l'ultimo giorno del mese precedente
076600060420     C     *ISO          MOVE      wDataFIN      wDataISO
076700060420     C     wDataISO      SUBDUR    1:*D          wDataISO
076800060420     C     *ISO          MOVE      wDataISO      wDataINZ
076900060420     C*
077000060420     C* Calcolo il primo giorno del mese precedente
077100060420     C***                IF        wMM = 01
077200060420     C***                EVAL      wDataINZ = 0112*10000+wAAAA-1
077300060420     C***                ELSE
077400060420     C***                EVAL      wDataINZ = 01*1000000+(wMM-1)*10000+wAAAA
077500060420     C***                ENDIF
077600060419     C*
077700060419     C                   ENDSR
077800151016
077900151016
078000151016
078100151016     C     RTVAR5FAT     BEGSR
078200151016     C*
078300151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
078400151016     C                   SETOFF                                       7071
078500151016     C                   EVAL      ar5TRD = 'FAT'
078600151016     C                   CLEAR                   DAR5FAT
078700151016     C     KEYar531_P    CHAIN     FIAR531C
078800151016     C                   IF        %found(FIAR531C)
078900151016     C                   MOVEL     AR5UNI        DAR5FAT
079000151016     C                   IF        §AR5PKTAS > *zeros
079100151016     C                   SETON                                        70
079200151016     C                   EVAL      VAFPKC = §AR5PKTAS
079300151016     C                   ENDIF
079400151016     C                   IF        §AR5VLTAS > *zeros
079500151016     C                   SETON                                        71
079600160505     C                   EVAL      VAFVLC = §AR5VLTAS
079700151016     C                   ENDIF
079800151016     C                   ENDIF
079900151016     C*
080000151016     C                   ENDSR
080100160209
080200160209
080300160209
080400160209     C     RTVTA4A       BEGSR
080500160209     C*
080600160209     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
080700160209     C                   CLEAR                   DTA4A
080800160209     C*
080900160209     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
081000160209     C                   CALL      'UBTA400R'
081100160209     C                   PARM      *blanks       UBTA4IOPZ
081200160209     C                   PARM      *blanks       UBTA4ITLA
081300160209     C                   PARM      tasAAS        UBTA4IAAS
081400160209     C                   PARM      tasLNP        UBTA4ILNP
081500160209     C                   PARM      tasNRS        UBTA4INRS
081600160209     C                   PARM      tasNSP        UBTA4INSP
081700160209     C                   PARM      'A'           UBTA4ITRC
081800160209     C                   PARM                    UBTA4OERR
081900160209     C                   PARM                    UBTA4ODS
082000160209     C                   PARM                    UBTA4OLEN
082100160209     C                   PARM                    UBTA4ODATI
082200160209     C*
082300160209     C                   IF        UBTA4OERR = *zeros
082400160209     C                   SELECT
082500160209     C* Gestione output tipo record 'A'
082600160209     C                   WHEN      UBTA4ODS = 'DTA4A'
082700160209     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
082800160209     C                   ENDSL
082900160209     C*
083000160209     C                   ENDIF
083100160209     C*
083200160209     C                   ENDSR
083300060418
083400060419
083500060418
083600991027      /TITLE Operazioni iniziali.
083700991027     C     *inzsr        BEGSR
083800991027     C*
083900991027     C     *ENTRY        PLIST
084000991027     C                   parm                    prmppt
084100991027     C     wrkesito      parm      wrkesito      prmesito
084200060419     C*
084300060419     C* CALCOLA LA DATA CORRENTE
084400160209     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084500160209     C                   eval      datcor = %dec(%date() : *ISO)
084600060419     C                   movel     datcor        wAAAAMMcor        6 0
084700060413     C*
084800060413     C* Definizione chiavi di lettura
084900060419     C*
085000060419     C* TABEL00F - Completa
085100060419     C     KEYtabel      KLIST
085200060419     C                   KFLD                    tblKUT
085300060419     C                   KFLD                    tblCOD
085400060419     C                   KFLD                    tblKEY
085500060418     C*
085600060418     C* TABEL00F - Parziale
085700060418     C     KEYtabel_P    KLIST
085800060418     C                   KFLD                    tblKUT
085900060418     C                   KFLD                    tblCOD
086000060413     C*
086100060413     C* TITAS31L - Completa
086200060413     C     KEYtas31      KLIST
086300060413     C                   KFLD                    tasKSC
086400060420     C                   KFLD                    tasDFT
086500060413     C*
086600060413     C* TNCSB03L - Completa
086700060413     C     KEYcsb03      KLIST
086800060413     C                   KFLD                    tasAAS
086900060413     C                   KFLD                    tasLNP
087000060413     C                   KFLD                    tasNRS
087100060413     C                   KFLD                    tasNSP
087200060418     C*
087300060418     C* TIGCP51L - Parziale
087400060418     C     KEYgcp51_P    KLIST
087500060418     C                   KFLD                    tasAAS
087600060418     C                   KFLD                    tasLNP
087700060418     C                   KFLD                    tasNRS
087800060418     C                   KFLD                    tasNSP
087900060420     C*
088000060420     C* TITAA30C - Completa
088100060420     C     KEYtaa30      KLIST
088200060420     C                   KFLD                    tasAAS
088300060420     C                   KFLD                    tasLNP
088400060420     C                   KFLD                    tasNRS
088500060420     C                   KFLD                    tasNSP
088600060420     C                   KFLD                    taaTRC
088700060420     C*
088800060420     C* FIAR531C - Parziale
088900060420     C     KEYar531_P    KLIST
089000060420     C                   KFLD                    tasAAS
089100060420     C                   KFLD                    tasLNP
089200060420     C                   KFLD                    tasNRS
089300060420     C                   KFLD                    tasNSP
089400060420     C                   KFLD                    ar5TRD
089500991027     C*
089600991027     C                   ENDSR
