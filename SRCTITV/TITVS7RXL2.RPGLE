000100060426     H DECEDIT('0,') DATEDIT(*DMY.)
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
001600060704     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001700060704     D CFG             S              3    DIM(10) CTDATA PERRCD(1)
001800060331
001900060418     D****
002000060418     D* SCHIERE DI WRK
002100060418     D****
002200060418     D skVAF           s              7  0 inz dim(1000)
002300060418     D jVAF            s              4  0 inz
002400060911
002500060911
002600060911     D*-------------------
002700060911     D* SCHIERE FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA E TEMPI DI CONSEGNA
002800060911     D*-------------------
002900060911     D RBL             S              1    DIM(20)                              RAGGRUPPAMENTO BOLLA
003000060911     D*-------------------
003100060911     D* SCHIERE TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003200060911     D*-------------------
003300060911     D TB              S              2    DIM(20)                              TIP0 BOLLA
003400060419
003500060419     D*------------------
003600060419     D* DS "XSRDA8" - CONTROLLA DATA (8)
003700060419     D*------------------
003800060419     D WLBDA8          DS                  INZ
003900060419     D  G08DAT                 1      8  0
004000060419     D  G08INV                 9     16  0
004100060419     D  G08ERR                17     17
004200060419     D  G08TGI                18     22  0
004300060420
004400060420     D*------------------
004500060420     D* DS REPERIMENTO DATI CLIENTE
004600060420     D*-------------------
004700060420     D BS69DS        E DS                  EXTNAME(TIBS69DS)
004800060420     D ACODS         E DS                  EXTNAME(CNACO00F)
004900060420     D INDDS         E DS                  EXTNAME(CNIND00F)
005000060420     D CLPDS         E DS                  EXTNAME(CNCLP00F)
005100060420     D CLSDS         E DS                  EXTNAME(FNCLS00F)
005200060419
005300060419     D*------------
005400060420     D* DS ESTERNE VARIE
005500060419     D*------------
005600060420     D DSVAF         E DS                  EXTNAME(TIVAF00T)
005700060420     D DSVAFA        E DS                  EXTNAME(TIVAFAWT)
005800060420     D TRUL86DS      E DS
005900060420     D DAR5BNB       E DS
006000060420     D DSQT1         E DS
006100060420     D DS4C          E DS
006200060704     D DS2C          E DS                                                       * occhio in fondo !!
006300060911     D DSTB          E DS
006400060420
006500060420     D*------------
006600060420     D* SCHIERE SIGLE E DESCRIZIONI VARIE
006700060420     D*------------
006800060420     D skVARIE         S                   LIKE(dsVARIA) DIM(50) INZ
006900060420     D jVARIE          S              2  0 INZ
007000060420     D*------------------
007100060420     D* DS RIDEFINIZIONE OUTPUT ELEMENTO PROCEDURA TRUL86R
007200060420     D*------------------
007300060420     D dsVARIA         DS
007400060420     D  dVARIA_COD                    1
007500060420     D  dVARIA_VAL                   13  3
007600060911
007700060911     D*------------
007800060911     D* SCHIERE SIGLE E DESCRIZIONI VARIE - "RMN"
007900060911     D*------------
008000060912     D skVARIE_RMN     S                   LIKE(dsVARIA_RMN) DIM(16000)
008100060911     D                                     INZ DESCEND
008200060911     D yVARIE          S              5  0 INZ
008300060911     D*------------------
008400060911     D* DS RIDEFINIZIONE OUTPUT ELEMENTO PROCEDURA TRUL86R - "RMN"
008500060911     D*------------------
008600060911     D dsVARIA_RMN     DS
008700060911     D  dRMN                         15  0
008800060911     D  dNSP                          7  0
008900060911     D  dPOR                         13  3
009000060911     D  dVARIE                             LIKE(dsVARIA) DIM(50) INZ
009100060911     D  dIMV                         15  3
009200060420
009300060420     D*------------
009400060420     D* DS RIDEFINIZIONE TABELLA "QC" TIPO RECORD "2"
009500060420     D*------------
009600060420     D DSQC2         E DS
009700060419
009800060419     D*------------------
009900060419     D* DS X PASSAGGIO PARAMETRI
010000060419     D*------------------
010100060419     D                 DS
010200060419     D  wPERIODO               1      6  0
010300060419     D  wMM                    5      6  0
010400060419     D  wAAAA                  1      4  0
010500060421
010600060421     D*------------
010700060421     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
010800060421     D*------------
010900060421     D iHtml           s             10    inz('<html>')
011000060421     D fHtml           s             10    inz('</html>')
011100060421     D iBody           s             10    inz('<body>')
011200060421     D fBody           s             10    inz('</body>')
011300060421     D iTable          s             20    inz('<table border="1">')
011400060421     D fTable          s             10    inz('</table>')
011500060426     D iThead          s             10    inz('<thead>')
011600060421     D fThead          s             10    inz('</thead>')
011700060421     D iRow            s              5    inz('<tr>')
011800060421     D fRow            s              5    inz('</tr>')
011900060421     D iCol            s              5    inz('<td>')
012000060421     D fCol            s              5    inz('</td>')
012100060426     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
012200060421     D fColBold        s              5    inz('</th>')
012300060418
012400060331     D****
012500060331     D* VARIABILI DI WRK
012600060331     D****
012700991027     D prmppt          s             50
012800991027     D prmesito        s              1
012900991027     D wrkesito        s                   like(prmesito)
013000060418     D wKSUVAFa        s              7
013100060418     D wKSUVAF         s              7  0
013200060420     D wAAAAMMa        s              6
013300060420     D wAAAAMM         s              6  0
013400060420     D wDataINZ        s              8  0
013500060420     D wDataFIN        s              8  0
013600060420     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
013700060420     D wTara           s                   like(§QTTPC)
013800060420     D wNtara          s                   like(TASPKF)
013900060421     D wBufferOut      s          10000    varying inz
014000060911     D i               s              5i 0 inz(*zeros)
014100060911     D j               s              5i 0 inz(*zeros)
014200991027
014300060419     C*
014400060419     C* Verifico ed effettuo considerazioni sui parametri ricevuti in input
014500060419     C                   EXSR      chkPAR
014600060418     C*
014700060418     C* Carico le tabelle occorrenti
014800060419     C                   EXSR      carTAB
014900060418     C*
015000060418     C* Verifico se già presenti dati nel file d output
015100030908     C                   EXSR      chkRECFILMBR
015200060418     C*
015300060418     C* Effettuo elaborazione
015400991027     C                   EXSR      traduci
015500060420     C*
015600060420     C* Effettuo chiamata x chiusura procedure esterne
015700060420     C                   CLEAR                   TRUL86DS
015800060420     C                   EVAL      UL86ITLA = 'C'
015900060420     C                   CALL      'TRUL86R'
016000060420     C                   PARM                    TRUL86DS
016100060418     C*
016200921023     C                   SETON                                        LR
016300060413
016400060413
016500030908
016600030908     C     chkRECFILMBR  BEGSR
016700030908     C*
016800060413     C                   READ      TIVAWWWT                               55
016900030908     C*
017000030908     C                   ENDSR
017100060413
017200060413
017300991027
017400991027     C     traduci       BEGSR
017500060421     C*
017600060421     C* Head iniziali della struttura HTML
017700060421     C                   EVAL      VAWDTA = iHtml + iBody + iTable
017800060421     C                   WRITE     TIVAW000
017900030325     C*
018000030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
018100030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
018200060421     C                   EVAL      wBufferOut = iThead + iRow
018300060421     C                             +iColBold+ 'VAFAAS'+fColBold
018400060421     C                             +iColBold+ 'VAFLNP'+fColBold
018500060421     C                             +iColBold+ 'VAFNRS'+fColBold
018600060421     C                             +iColBold+ 'VAFNSP'+fColBold
018700060421     C                             +iColBold+ 'VAFTBL'+fColBold
018800060421     C                             +iColBold+ 'VAFMGS'+fColBold
018900060421     C                             +iColBold+ 'VAFKSC'+fColBold
019000060421     C                             +iColBold+ 'VAFLNA'+fColBold
019100060421     C                             +iColBold+ 'VAFNCL'+fColBold
019200060421     C                             +iColBold+ 'VAFPKB'+fColBold
019300060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
019400060426     C                   IF        %len(%trim(wBufferOut)) >= 700
019500060426     C                   EVAL      VAWDTA = wBufferOut
019600060426     C                   WRITE     TIVAW000
019700060426     C                   EVAL      wBufferOut = *blanks
019800060426     C                   ENDIF
019900060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
020000060426     C                              iColBold+ 'VAFVLF'+fColBold
020100060421     C                             +iColBold+ 'VAFQFT'+fColBold
020200060421     C                             +iColBold+ 'VAFPOR'+fColBold
020300060421     C                             +iColBold+ 'VAFSV1'+fColBold
020400060421     C                             +iColBold+ 'VAFVA1'+fColBold
020500060421     C                             +iColBold+ 'VAFSV2'+fColBold
020600060421     C                             +iColBold+ 'VAFVA2'+fColBold
020700060421     C                             +iColBold+ 'VAFSV3'+fColBold
020800060421     C                             +iColBold+ 'VAFVA3'+fColBold
020900060421     C                             +iColBold+ 'VAFSV4'+fColBold
021000060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
021100060426     C                   IF        %len(%trim(wBufferOut)) >= 700
021200060426     C                   EVAL      VAWDTA = wBufferOut
021300060426     C                   WRITE     TIVAW000
021400060426     C                   EVAL      wBufferOut = *blanks
021500060426     C                   ENDIF
021600060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
021700060426     C                              iColBold+ 'VAFVA4'+fColBold
021800060421     C                             +iColBold+ 'VAFSV5'+fColBold
021900060421     C                             +iColBold+ 'VAFVA5'+fColBold
022000060421     C                             +iColBold+ 'VAFSV6'+fColBold
022100060421     C                             +iColBold+ 'VAFVA6'+fColBold
022200060421     C                             +iColBold+ 'VAFSV7'+fColBold
022300060421     C                             +iColBold+ 'VAFVA7'+fColBold
022400060421     C                             +iColBold+ 'VAFSV8'+fColBold
022500060421     C                             +iColBold+ 'VAFVA8'+fColBold
022600060421     C                             +iColBold+ 'VAFVAX'+fColBold
022700060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
022800060426     C                   IF        %len(%trim(wBufferOut)) >= 700
022900060426     C                   EVAL      VAWDTA = wBufferOut
023000060426     C                   WRITE     TIVAW000
023100060426     C                   EVAL      wBufferOut = *blanks
023200060426     C                   ENDIF
023300060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
023400060426     C                              iColBold+ 'VAFIMV'+fColBold
023500060421     C                             +iColBold+ 'VAFDFT'+fColBold
023600060421     C                             +iColBold+ 'VAFNFT'+fColBold
023700060421     C                             +iColBold+ 'VAFFIV'+fColBold
023800060421     C                             +iColBold+ 'VAFDIV'+fColBold
023900060421     C                             +iColBold+ 'VAFDRT'+fColBold
024000060421     C                             +iColBold+ 'VAFRMN'+fColBold
024100060421     C                             +iColBold+ 'VAFRMA'+fColBold
024200060421     C                             +iColBold+ 'VAFRMX'+fColBold
024300060421     C                             +iColBold+ 'VAFFTC'+fColBold
024400060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
024500060426     C                   IF        %len(%trim(wBufferOut)) >= 700
024600060426     C                   EVAL      VAWDTA = wBufferOut
024700060426     C                   WRITE     TIVAW000
024800060426     C                   EVAL      wBufferOut = *blanks
024900060426     C                   ENDIF
025000060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
025100060426     C                              iColBold+ 'VAFTC2'+fColBold
025200060421     C                             +iColBold+ 'VAFTSP'+fColBold
025300060421     C                             +iColBold+ 'VAFFAP'+fColBold
025400060421     C                             +iColBold+ 'VAFFIN'+fColBold
025500060421     C                             +iColBold+ 'VAFCTR'+fColBold
025600060421     C                             +iColBold+ 'VAFNAS'+fColBold
025700060421     C                             +iColBold+ 'VAFCTS'+fColBold
025800060421     C                             +iColBold+ 'VAFCAD'+fColBold
025900060421     C                             +iColBold+ 'VAFLOD'+fColBold
026000060421     C                             +iColBold+ 'VAFPRD'+fColBold
026100060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
026200060426     C                   IF        %len(%trim(wBufferOut)) >= 700
026300060426     C                   EVAL      VAWDTA = wBufferOut
026400060426     C                   WRITE     TIVAW000
026500060426     C                   EVAL      wBufferOut = *blanks
026600060426     C                   ENDIF
026700060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
026800060426     C                              iColBold+ 'VAFNZD'+fColBold
026900060421     C                             +iColBold+ 'VAFVAS'+fColBold
027000060421     C                             +iColBold+ 'VAFIAS'+fColBold
027100060421     C                             +iColBold+ 'VAFFPC'+fColBold
027200060421     C                             +iColBold+ 'VAFPKC'+fColBold
027300060421     C                             +iColBold+ 'VAFFVC'+fColBold
027400060421     C                             +iColBold+ 'VAFVLC'+fColBold
027500060421     C                             +iColBold+ 'VAFRSD'+fColBold
027600060421     C                             +iColBold+ 'VAFIND'+fColBold
027700060421     C                             +iColBold+ 'VAFRSM'+fColBold
027800060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
027900060426     C                   IF        %len(%trim(wBufferOut)) >= 700
028000060426     C                   EVAL      VAWDTA = wBufferOut
028100060426     C                   WRITE     TIVAW000
028200060426     C                   EVAL      wBufferOut = *blanks
028300060426     C                   ENDIF
028400060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
028500060426     C                              iColBold+ 'VAFINM'+fColBold
028600060421     C                             +iColBold+ 'VAFCAM'+fColBold
028700060421     C                             +iColBold+ 'VAFLOM'+fColBold
028800060421     C                             +iColBold+ 'VAFPRM'+fColBold
028900060421     C                             +iColBold+ 'VAFNZM'+fColBold
029000060421     C                             +iColBold+ 'VAFRMO'+fColBold
029100060421     C                             +iColBold+ 'VAFCMO'+fColBold
029200060421     C                             +iColBold+ 'VAFNMO'+fColBold
029300060421     C                             +iColBold+ 'GIACENZA'+fColBold
029400060421     C                             +iColBold+ 'CONS_AI_PIANI'+fColBold
029500060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
029600060426     C                   IF        %len(%trim(wBufferOut)) >= 700
029700060426     C                   EVAL      VAWDTA = wBufferOut
029800060426     C                   WRITE     TIVAW000
029900060426     C                   EVAL      wBufferOut = *blanks
030000060426     C                   ENDIF
030100060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
030200060426     C                              iColBold+ 'CONS_APPUNTAMENTO'+fColBold
030300060421     C                             +iColBold+ 'CONS_LOCALITA_DISAGIATA'+fColBold
030400060421     C                             +iColBold+ 'CONS_ISOLE_MINORI'+fColBold
030500060421     C                             +iColBold+ 'CONS_DISAGIATA'+fColBold
030600060421     C                             +iColBold+ 'POD_IMAGE'+fColBold
030700060421     C                             +iColBold+ 'DIROTTAMENTO'+fColBold
030800060421     C                             +iColBold+ 'IMPORTO_CONTRASSEGNO'+fColBold
030900060421     C                             +iColBold+ 'DIVISA_CONTRASSEGNO'+fColBold
031000060704     C                             +iColBold+ 'RICONSEGNA_SU_GIACENZA'+fColBold
031100060704     C                             +iColBold+ 'ZONA_FRANCA'+fColBold
031200060421     C                   EVAL      wBufferOut = wBufferOut + fRow + fThead
031300060426     C                   EVAL      VAWDTA = wBufferOut
031400060426     C                   WRITE     TIVAW000
031500060911     C*
031600030325     C                   ENDIF
031700991027     C*
031800060418     C* Ciclo x tutti i codici clienti ricondotti al cliente unificante fatture (tab. 4C)
031900060418     C                   EVAL      jVAF = 1
032000060418     C                   DOW       jVAF <= %elem(skVAF)
032100060418     C                   IF        skVAF(jVAF) = *zeros
032200060418     C                   LEAVE
032300060418     C                   ELSE
032400060418     C                   EVAL      tasKSC = skVAF(jVAF)
032500060420     C                   EVAL      tasDFT = wDataINZ
032600060420     C     KEYtas31      SETLL     TITAS31C
032700060413     C                   IF        %found(TITAS31C)
032800060420     C     skVAF(jVAF)   READE     TITAS31C
032900060413     C                   DOW       not %eof(TITAS31C)
033000060418     C*
033100060419     C* Inizializzo il flag che condiziona l'elaborazione del record bolla corrente
033200060419     C                   MOVEL     'S'           wRecOK            1
033300060911     C                   MOVEL     'S'           wRecOkSped        1
033400060420     C                   MOVEL     'N'           wLeave            1
033500060419     C*
033600060419     C* Effettuo verifiche validità record
033700060419     C                   EXSR      chkREC
033800060419     C*
033900060419     C* Se record da considerare procedo con le considerazioni
034000060911     C                   IF        wRecOK     = 'S'
034100060419     C*
034200060419     C* Re inizializzo il flag che indica se la bolla corrente è da considerare
034300060419     C                   MOVEL     'N'           wRecOK            1
034400060418     C*
034500060418     C* Inizializzo tutti i flag d wrk relativi agli attributi della bolla corrente
034600060418     C                   MOVEL     *blanks       wGIAC             1
034700060418     C                   MOVEL     *blanks       wPIAN             1
034800060418     C                   MOVEL     *blanks       wAPPU             1
034900060418     C                   MOVEL     *blanks       wLDIS             1
035000060418     C                   MOVEL     *blanks       wISOL             1
035100060418     C                   MOVEL     *blanks       wCDIS             1
035200060418     C                   MOVEL     *blanks       wPODI             1
035300060418     C                   MOVEL     *blanks       wDIRO             1
035400060704     C                   MOVEL     *blanks       wZONF             1
035500060704     C                   MOVEL     *blanks       wRICO             1
035600060418     C*
035700060418     C* Aggancio il file giacenze della bolla corrente
035800060704     C                   EVAL      gcpFRG = *zeros
035900060418     C     KEYgcp51_P    CHAIN     TIGCP51L
036000060418     C                   IF        %found(TIGCP51L)
036100060704     C                   MOVEL     'S'           wRecOK
036200060418     C                   MOVEL     'S'           wGIAC
036300060704     C* Verifico se "riconsegna x giacenza"
036400060704     C                   IF        gcpCFG <> *blanks
036500060704     C     gcpCFG        LOOKUP    CFG                                    55
036600060704     C                   IF        %found
036700060704     C                   MOVEL     'S'           wRICO
036800060704     C                   ENDIF
036900060704     C                   ENDIF
037000060418     C                   ENDIF
037100060413     C*
037200060413     C* Aggancio il file contrassegni della bolla corrente
037300060421     C                   Z-ADD     *zeros        csbCAS
037400060421     C                   MOVEL     *blanks       csbVCA
037500060413     C     KEYcsb03      CHAIN     TNCSB03L
037600060413     C                   IF        %found(TNCSB03L)
037700060421     C                   IF        csbSTA <> 9
037800060418     C                   MOVEL     'S'           wRecOK
037900060421     C                   ENDIF
038000060418     C                   ENDIF
038100060704     C*
038200060704     C* Verifica se CAMPIONE D'ITALIA o LIVIGNO
038300060704     C                   IF        (tasCAD = '22060' and tasFIN = 'I') or
038400060704     C                             (tasCAD = '23030' and tasFIN = 'I')
038500060704     C                   MOVEL     'S'           wRecOK
038600060704     C                   MOVEL     'S'           wZONF
038700060704     C                   ENDIF
038800060418     C*
038900060418     C* Verifica se APPUNTAMENTO
039000060418     C                   IF        tasFTC = 'A' or
039100060418     C                             tasTC2 = 'A'
039200060418     C                   MOVEL     'S'           wRecOK
039300060418     C                   MOVEL     'S'           wAPPU
039400060418     C                   ENDIF
039500060418     C*
039600060418     C* Verifica se AI PIANI
039700060418     C                   IF        tasFTC = 'P' or
039800060418     C                             tasTC2 = 'P'
039900060418     C                   MOVEL     'S'           wRecOK
040000060418     C                   MOVEL     'S'           wPIAN
040100060418     C                   ENDIF
040200060418     C*
040300060418     C* Verifica se CONSEGNA DISAGIATA
040400060418     C                   IF        tasFTC = 'X' or
040500060418     C                             tasTC2 = 'X'
040600060418     C                   MOVEL     'S'           wRecOK
040700060418     C                   MOVEL     'S'           wCDIS
040800060418     C                   ENDIF
040900060418     C*
041000060418     C* Verifica se LOCALITA DISAGIATA
041100060418     C                   IF        tasFIN = 'D'
041200060418     C                   MOVEL     'S'           wRecOK
041300060418     C                   MOVEL     'S'           wLDIS
041400060418     C                   ENDIF
041500060418     C*
041600060418     C* Verifica se ISOLA MINORE
041700060418     C                   IF        tasFIN = 'I'
041800060418     C                   MOVEL     'S'           wRecOK
041900060418     C                   MOVEL     'S'           wISOL
042000060418     C                   ENDIF
042100060418     C*
042200060418     C* Verifica se P.O.D. IMAGE
042300060418     C                   IF        tasCBO = 'FI'
042400060418     C                   MOVEL     'S'           wRecOK
042500060418     C                   MOVEL     'S'           wPODI
042600060418     C                   ENDIF
042700060418     C*
042800060418     C* Verifica se DIROTTAMENTO
042900060418     C                   IF        tasCBO = 'F*'
043000060418     C                   MOVEL     'S'           wRecOK
043100060418     C                   MOVEL     'S'           wDIRO
043200060418     C                   ENDIF
043300060418     C*
043400060418     C* Se presente numero fattura considero sempre la bolla
043500060418     C                   IF        tasNFT > *zeros
043600060418     C                   MOVEL     'S'           wRecOK
043700060418     C                   ENDIF
043800060413     C*
043900060911     C* Se tutto ok procedo con scrittura del buffer d out
044000060911     C                   IF        wRecOK = 'S' AND wRecOkSped = 'S'
044100060420     C*
044200060420     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
044300060420     C                   EVAL      ta4TRC = 'A'
044400060420     C     KEYta430      CHAIN     TITA430C
044500060420     C                   IF        %found(TITA430C)
044600060420     C                   ELSE
044700060420     C                   CLEAR                   TITA4000
044800060420     C                   CLEAR                   TITA4P00
044900060420     C                   ENDIF
045000060420     C*
045100060420     C* Inizializzo il buffer del file d output e delle ds d ridefinizione dati
045200060420     C                   CLEAR                   TIVAW000
045300060420     C                   CLEAR                   DSVAF
045400060420     C                   CLEAR                   DSVAFA
045500060420     C*
045600060420     C* Valorizzo i dati del destinatario nn già presenti sul TIVAF
045700060420     C                   EVAL      VHFRSD = TASRSD
045800060420     C                   EVAL      VHFIND = TASIND
045900060420     C*
046000060420     C* X reperire i dati del mittente verifico il codice del cliente mittente:
046100060420     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
046200060420     C                   MOVE(P)   tasCCM        wCLI              4 0
046300060420     C                   IF        wCLI =    0 OR
046400060420     C                             wCLI = 8888 OR
046500060420     C                             wCLI = 9999
046600060420     C                   EVAL      taaTRC = 'M'
046700060420     C     KEYtaa30      CHAIN     titaa30c
046800060420     C                   IF        %found(titaa30c)
046900060420     C                   EVAL      VHFRSM = TAARSC
047000060420     C                   EVAL      VHFINM = TAAIND
047100060420     C                   EVAL      VHFCAM = TAACAP
047200060420     C                   EVAL      VHFLOM = TAALOC
047300060420     C                   EVAL      VHFPRM = TAAPRV
047400060420     C                   EVAL      VHFNZM = TAANAZ
047500060420     C                   ENDIF
047600060420     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
047700060420     C                   ELSE
047800060420     C                   CLEAR                   BS69DS
047900060420     C                   CLEAR                   ACODS
048000060420     C                   CLEAR                   INDDS
048100060420     C                   CLEAR                   CLPDS
048200060420     C                   CLEAR                   CLSDS
048300060420     C                   Z-ADD     tasCCM        I69KAC
048400060420     C                   Z-ADD     tasCCM        I69KIN
048500060420     C                   CALL      'TIBS69R'
048600060420     C                   PARM                    BS69DS
048700060420     C                   PARM                    ACODS
048800060420     C                   PARM                    INDDS
048900060420     C                   PARM                    CLPDS
049000060420     C                   PARM                    CLSDS
049100060420     C     O69ERR        IFNE      '1'
049200060420     C                   EVAL      VHFRSM = ACORAG
049300060420     C                   EVAL      VHFINM = INDVIA
049400060420     C                   MOVEL(P)  INDCAP        VHFCAM
049500060420     C                   EVAL      VHFLOM = INDCIT
049600060420     C                   EVAL      VHFPRM = INDPRV
049700060420     C                   EVAL      VHFNZM = INDSTA
049800060420     C                   ENDIF
049900060420     C                   ENDIF
050000060420     C*
050100060420     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
050200060420     C                   EVAL      taaTRC = 'O'
050300060420     C     KEYtaa30      CHAIN     titaa30c
050400060420     C                   IF        %found(titaa30c)
050500060420     C                   EVAL      VHFRMO = TAARSC
050600060420     C                   EVAL      VHFCMO = TAACAP
050700060420     C                   EVAL      VHFNMO = TAANAZ
050800060420     C                   ENDIF
050900060420     C*
051000060420     C* Se particolità varia = 'O' recupero da FIAR5 il numero colli originali
051100060420     C                   IF        %subst(tasGVA:1:1) = 'O'
051200060420     C                   EVAL      ar5TRD = 'BNB'
051300060420     C                   CLEAR                   DAR5BNB
051400060420     C     KEYar531_P    CHAIN     FIAR531C
051500060420     C                   IF        %found(FIAR531C)
051600060420     C                   MOVEL     AR5UNI        DAR5BNB
051700060420     C                   ENDIF
051800060420     C* Colli originali
051900060420     C                   EVAL      VAFNCL = §AR5BCOR
052000060420     C* Colli TITAS
052100060420     C                   ELSE
052200060420     C                   Z-ADD     TASNCL        VAFNCL
052300060420     C                   ENDIF
052400060420     C*
052500060420     C* Eseguo reperimento varie
052600060911     C                   EXSR      sumVARIE
052700060911     C*
052800060911     C* Eseguo reperimento varie altre bolle nn valide come spedizione con stesso riferimento num.
052900060911     C                   EXSR      repVARIE_RMN
053000060420     C*
053100060420     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
053200060420     C                   EXSR      TAStoVAF
053300060420     C*
053400060421     C                   EVAL      wBufferOut = iRow +
053500060421     C                                  iCol+%trim(%editc(VAFAAS:'Q'))+fCol+
053600060421     C                                  iCol+%trim(%editc(VAFLNP:'Q'))+fCol+
053700060421     C                                  iCol+%trim(%editc(VAFNRS:'Q'))+fCol+
053800060421     C                                  iCol+%trim(%editc(VAFNSP:'Q'))+fCol+
053900060421     C                                  iCol+   %trim(VAFTBL)         +fCol+
054000060421     C                                  iCol+%trim(%editc(VAFMGS:'Q'))+fCol+
054100060421     C                                  iCol+%trim(%editc(VAFKSC:'Q'))+fCol+
054200060421     C                                  iCol+%trim(%editc(VAFLNA:'Q'))+fCol+
054300060421     C                                  iCol+%trim(%editc(VAFNCL:'Q'))+fCol+
054400060426     C                                  iCol+%trim(%editc(VAFPKB:'Q'))+fCol
054500060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
054600060426     C                   IF        %len(%trim(wBufferOut)) >= 700
054700060426     C                   EVAL      VAWDTA = wBufferOut
054800060426     C                   WRITE     TIVAW000
054900060426     C                   EVAL      wBufferOut = *blanks
055000060426     C                   ENDIF
055100060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
055200060421     C                                  iCol+%trim(%editc(VAFVLF:'Q'))+fCol+
055300060421     C                                  iCol+%trim(%editc(VAFQFT:'Q'))+fCol+
055400060421     C                                  iCol+%trim(%editc(VAFPOR:'Q'))+fCol+
055500060421     C                                  iCol+   %trim(VAFSV1)         +fCol+
055600060421     C                                  iCol+%trim(%editc(VAFVA1:'Q'))+fCol+
055700060421     C                                  iCol+   %trim(VAFSV2)         +fCol+
055800060421     C                                  iCol+%trim(%editc(VAFVA2:'Q'))+fCol+
055900060421     C                                  iCol+   %trim(VAFSV3)         +fCol+
056000060421     C                                  iCol+%trim(%editc(VAFVA3:'Q'))+fCol+
056100060426     C                                  iCol+   %trim(VAFSV4)         +fCol
056200060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
056300060426     C                   IF        %len(%trim(wBufferOut)) >= 700
056400060426     C                   EVAL      VAWDTA = wBufferOut
056500060426     C                   WRITE     TIVAW000
056600060426     C                   EVAL      wBufferOut = *blanks
056700060426     C                   ENDIF
056800060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
056900060421     C                                  iCol+%trim(%editc(VAFVA4:'Q'))+fCol+
057000060421     C                                  iCol+   %trim(VAFSV5)         +fCol+
057100060421     C                                  iCol+%trim(%editc(VAFVA5:'Q'))+fCol+
057200060421     C                                  iCol+   %trim(VAFSV6)         +fCol+
057300060421     C                                  iCol+%trim(%editc(VAFVA6:'Q'))+fCol+
057400060421     C                                  iCol+   %trim(VAFSV7)         +fCol+
057500060421     C                                  iCol+%trim(%editc(VAFVA7:'Q'))+fCol+
057600060421     C                                  iCol+   %trim(VAFSV8)         +fCol+
057700060421     C                                  iCol+%trim(%editc(VAFVA8:'Q'))+fCol+
057800060426     C                                  iCol+%trim(%editc(VAFVAX:'Q'))+fCol
057900060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
058000060426     C                   IF        %len(%trim(wBufferOut)) >= 700
058100060426     C                   EVAL      VAWDTA = wBufferOut
058200060426     C                   WRITE     TIVAW000
058300060426     C                   EVAL      wBufferOut = *blanks
058400060426     C                   ENDIF
058500060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
058600060421     C                                  iCol+%trim(%editc(VAFIMV:'Q'))+fCol+
058700060421     C                                  iCol+%trim(%editc(VAFDFT:'Q'))+fCol+
058800060421     C                                  iCol+%trim(%editc(VAFNFT:'Q'))+fCol+
058900060421     C                                  iCol+%trim(%editc(VAFFIV:'Q'))+fCol+
059000060421     C                                  iCol+   %trim(VAFDIV)         +fCol+
059100060421     C                                  iCol+%trim(%editc(VAFDRT:'Q'))+fCol+
059200060421     C                                  iCol+%trim(%editc(VAFRMN:'Q'))+fCol+
059300060421     C                                  iCol+   %trim(VAFRMA)         +fCol+
059400060421     C                                  iCol+   %trim(VAFRMX)         +fCol+
059500060426     C                                  iCol+   %trim(VAFFTC)         +fCol
059600060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
059700060426     C                   IF        %len(%trim(wBufferOut)) >= 700
059800060426     C                   EVAL      VAWDTA = wBufferOut
059900060426     C                   WRITE     TIVAW000
060000060426     C                   EVAL      wBufferOut = *blanks
060100060426     C                   ENDIF
060200060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
060300060421     C                                  iCol+   %trim(VAFTC2)         +fCol+
060400060421     C                                  iCol+   %trim(VAFTSP)         +fCol+
060500060421     C                                  iCol+   %trim(VAFFAP)         +fCol+
060600060421     C                                  iCol+   %trim(VAFFIN)         +fCol+
060700060421     C                                  iCol+%trim(%editc(VAFCTR:'Q'))+fCol+
060800060421     C                                  iCol+   %trim(VAFNAS)         +fCol+
060900060421     C                                  iCol+   %trim(VAFCTS)         +fCol+
061000060421     C                                  iCol+   %trim(VAFCAD)         +fCol+
061100060421     C                                  iCol+   %trim(VAFLOD)         +fCol+
061200060426     C                                  iCol+   %trim(VAFPRD)         +fCol
061300060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
061400060426     C                   IF        %len(%trim(wBufferOut)) >= 700
061500060426     C                   EVAL      VAWDTA = wBufferOut
061600060426     C                   WRITE     TIVAW000
061700060426     C                   EVAL      wBufferOut = *blanks
061800060426     C                   ENDIF
061900060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
062000060421     C                                  iCol+   %trim(VAFNZD)         +fCol+
062100060421     C                                  iCol+   %trim(VAFVAS)         +fCol+
062200060421     C                                  iCol+%trim(%editc(VAFIAS:'Q'))+fCol+
062300060421     C                                  iCol+   %trim(VAFFPC)         +fCol+
062400060426     C                                  iCol+%trim(%editc(VAFPKC:'Q'))+fCol+
062500060421     C                                  iCol+   %trim(VAFFVC)         +fCol+
062600060421     C                                  iCol+%trim(%editc(VAFVLC:'Q'))+fCol+
062700060421     C                                  iCol+   %trim(VHFRSD)         +fCol+
062800060421     C                                  iCol+   %trim(VHFIND)         +fCol+
062900060426     C                                  iCol+   %trim(VHFRSM)         +fCol
063000060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
063100060426     C                   IF        %len(%trim(wBufferOut)) >= 700
063200060426     C                   EVAL      VAWDTA = wBufferOut
063300060426     C                   WRITE     TIVAW000
063400060426     C                   EVAL      wBufferOut = *blanks
063500060426     C                   ENDIF
063600060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
063700060421     C                                  iCol+   %trim(VHFINM)         +fCol+
063800060421     C                                  iCol+   %trim(VHFCAM)         +fCol+
063900060421     C                                  iCol+   %trim(VHFLOM)         +fCol+
064000060421     C                                  iCol+   %trim(VHFPRM)         +fCol+
064100060421     C                                  iCol+   %trim(VHFNZM)         +fCol+
064200060421     C                                  iCol+   %trim(VHFRMO)         +fCol+
064300060421     C                                  iCol+   %trim(VHFCMO)         +fCol+
064400060421     C                                  iCol+   %trim(VHFNMO)         +fCol+
064500060421     C                                  iCol+   %trim(wGIAC)          +fCol+
064600060426     C                                  iCol+   %trim(wPIAN)          +fCol
064700060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
064800060426     C                   IF        %len(%trim(wBufferOut)) >= 700
064900060426     C                   EVAL      VAWDTA = wBufferOut
065000060426     C                   WRITE     TIVAW000
065100060426     C                   EVAL      wBufferOut = *blanks
065200060426     C                   ENDIF
065300060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
065400060421     C                                  iCol+   %trim(wAPPU)          +fCol+
065500060421     C                                  iCol+   %trim(wLDIS)          +fCol+
065600060421     C                                  iCol+   %trim(wISOL)          +fCol+
065700060421     C                                  iCol+   %trim(wCDIS)          +fCol+
065800060421     C                                  iCol+   %trim(wPODI)          +fCol+
065900060421     C                                  iCol+   %trim(wDIRO)          +fCol+
066000060421     C                                  iCol+%trim(%editc(CSBCAS:'Q'))+fCol+
066100060704     C                                  iCol+   %trim(CSBVCA)         +fCol+
066200060704     C                                  iCol+   %trim(wRICO)          +fCol+
066300060704     C                                  iCol+   %trim(wZONF)          +fCol
066400060426     C                   EVAL      VAWDTA = wBufferOut
066500060426     C                   WRITE     TIVAW000
066600060421     C*
066700060418     C                   ENDIF
066800991027     C*
066900060420     C* Se già oltre data superiore range => esco dal ciclo
067000060420     C                   IF        wLeave = 'S'
067100060420     C                   LEAVE
067200060420     C                   ENDIF
067300060420     C*
067400060420     C                   ENDIF
067500060420     C*
067600060420     C     skVAF(jVAF)   READE     TITAS31C
067700030325     C                   ENDDO
067800060418     C*
067900060418     C                   ENDIF
068000060420     C                   ENDIF
068100060418     C                   ADD       1             jVAF
068200060418     C                   ENDDO
068300060421     C*
068400060421     C* Head finali della struttura HTML
068500060421     C                   EVAL      VAWDTA = fTable + fBody + fHtml
068600060421     C                   WRITE     TIVAW000
068700060421     C*
068800030325     C                   EVAL      wrkesito = '0'
068900991027     C*
069000910830     C                   ENDSR
069100060419
069200060419
069300060419
069400060419     C     chkrec        BEGSR
069500060419     C*
069600060419     C* Considero solo le bolle del periodo richiesto
069700060419     C                   IF        wRecOK = 'S'
069800060420     C                   IF        tasDFT > wDataFIN
069900060420     C                   MOVEL     'N'           wRecOK
070000060420     C                   MOVEL     'S'           wLeave
070100060419     C                   ENDIF
070200060419     C                   ENDIF
070300060719     C*
070400060911     C* Nn considero mai le bolle in conto/servizio (F7 e A7)
070500060911     C                   IF        wRecOK = 'S'
070600060911     C                   IF        tasTBL = 'F7' OR
070700060911     C                             tasTBL = 'A7'
070800060911     C                   MOVEL     'N'           wRecOK
070900060911     C                   ENDIF
071000060911     C                   ENDIF
071100060911     C*
071200060911     C* Esclude le bolle nn valide come "SPEDIZIONE"
071300060911     C*--------------------
071400060911     C                   if        wRecOk = 'S'
071500060911     C                   z-add     1             j
071600060911     C     tasTBL        lookup    TB(j)                                  28
071700060911     C                   if        %found and RBL(j) = 'N'
071800060911     C                   else
071900060911     C                   movel     'N'           wRecOkSped
072000060911     C                   eval      dRMN   = tasRMN
072100060911     C                   eval      dNSP   = tasNSP
072200060911     C                   exsr      repVARIE
072300060911     C                   eval      dPOR   = UL86OPOR
072400060911     C                   eval      dVARIE = skVARIE
072500060911     C                   eval      dIMV   = UL86OIMV
072600060911     C                   add       1             yVARIE
072700060911     C                   eval      skVARIE_RMN(yVARIE) = dsVARIA_RMN
072800060911     C                   endif
072900060911     C                   endif
073000060420     C*
073100060419     C* Verifica le bolle fatturate inizio mese richiesto, occorre considerare solo quelle aventi
073200060419     C* P.O. IVA uguale a zero o uguale ai valori indicati in tabella QT-2.
073300060420     C***                IF        wRecOK = 'S'
073400060420     C***                IF        tasDFT = wDataINZ
073500060420     C***                IF        tasFIV = §QCFIV  OR
073600060420     C***                          tasFIV = §QCFI2  OR
073700060420     C***                          tasFIV = *zeros
073800060420     C***                ELSE
073900060420     C***                MOVEL     'N'           wRecOK
074000060420     C***                ENDIF
074100060420     C***                ENDIF
074200060420     C***                ENDIF
074300060420     C*
074400060419     C* Verifica le bolle fatturate inizio mese successivo, occorre considerare solo quelle aventi
074500060419     C* P.O. IVA diverso da zero e diverso valori indicati in tabella QT-2.
074600060419     C                   IF        wRecOK = 'S'
074700060420     C                   IF        tasDFT = wDataFIN AND
074800060419     C                             tasFIV <> §QCFIV  AND
074900060419     C                             tasFIV <> §QCFI2  AND
075000060420     C                             tasFIV <> *zeros
075100060420     C                   MOVEL     'N'           wRecOK
075200060419     C                   ENDIF
075300060419     C                   ENDIF
075400060419     C*
075500060419     C                   ENDSR
075600060420
075700060420
075800060420
075900060420     C     TAStoVAF      BEGSR
076000060420     C*
076100060420     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
076200060420     C                   EVAL      VAFAAS = tasAAS
076300060420     C                   EVAL      VAFLNP = tasLNP
076400060420     C                   EVAL      VAFNRS = tasNRS
076500060420     C                   EVAL      VAFNSP = tasNSP
076600060420     C                   EVAL      VAFTBL = tasTBL
076700060420     C                   EVAL      VAFMGS = tasMGS
076800060420     C                   EVAL      VAFKSC = tasKSC
076900060420     C                   EVAL      VAFLNA = tasLNA
077000060420     C                   EVAL      VAFPKB = tasPKF
077100060420     C                   EVAL      VAFVLF = tasVLF
077200060420     C                   EVAL      VAFQFT = tasQFT
077300060420     C                   EVAL      VAFPOR = tasPOR
077400060420     C                   EVAL      VAFIMV = tasIMV
077500060420     C                   EVAL      VAFDFT = tasDFT
077600060420     C                   EVAL      VAFNFT = tasNFT
077700060420     C                   EVAL      VAFFIV = tasFIV
077800060420     C                   EVAL      VAFDIV = tasDIV
077900060420     C                   EVAL      VAFDRT = tasDRT
078000060420     C                   EVAL      VAFRMN = tasRMN
078100060420     C****** NON GESTISCO VARIE PARTICOLARITA DEL RIFERIMENTO MITTENTE ALFA *******
078200060420     C                   EVAL      VAFRMA = ta4NOT
078300060420     C*****************************************************************************
078400060420     C                   EVAL      VAFFTC = tasFTC
078500060420     C                   EVAL      VAFTC2 = tasTC2
078600060420     C                   EVAL      VAFTSP = tasTSP
078700060420     C                   EVAL      VAFFAP = tasFAP
078800060420     C                   EVAL      VAFFIN = tasFIN
078900060420     C                   EVAL      VAFCTR = tasCTR
079000060420     C                   EVAL      VAFCTS = tasCTS
079100060420     C                   EVAL      VAFCAD = tasCAD
079200060420     C                   EVAL      VAFLOD = tasLOD
079300060420     C                   EVAL      VAFPRD = tasPRD
079400060420     C                   EVAL      VAFNZD = tasNZD
079500060420     C                   EVAL      VAFVAS = tasVAS
079600060420     C                   EVAL      VAFIAS = tasIAS
079700060420     C*
079800060420     C* Gestisco particolari considerazioni
079900060420     C                   IF        tasFPF = 'V'
080000150716     C*
080100150716     C                   IF        TASDFT <= §QTDST
080200150716     C     §QTTCO        MULT      TASNCP        WTARA
080300150716     C     TASPKC        SUB(h)    WTARA         WNTARA
080400150716     C                   Z-ADD     WNTARA        VAFPKC
080500150716     C                   ELSE
080600150716     C     §QTTPC        MULT      TASNCP        WTARA
080700150716     C     TASPKC        SUB(h)    WTARA         WNTARA
080800150716     C                   Z-ADD     WNTARA        VAFPKC
080900150716     C                   ENDIF
081000150716     C*
081100060420     C                   ELSE
081200060420     C                   Z-ADD     *zeros        VAFPKC
081300060420     C                   ENDIF
081400060420     C*
081500060420     C                   ENDSR
081600060420
081700060420
081800060420
081900060420     C     repVARIE      BEGSR
082000060420     C*
082100060420     C* Inizializzo la DS d procedura esterna
082200060420     C                   CLEAR                   TRUL86DS
082300060420     C                   EVAL      UL86ITLA = 'E'
082400060420     C                   EVAL      UL86IAAS = tasAAS
082500060420     C                   EVAL      UL86ILNP = tasLNP
082600060420     C                   EVAL      UL86INRS = tasNRS
082700060420     C                   EVAL      UL86INSP = tasNSP
082800060420     C                   EVAL      UL86ITBL = tasTBL
082900060420     C                   CALL      'TRUL86R'
083000060420     C                   PARM                    TRUL86DS
083100060420     C*
083200060420     C                   MOVEA(P)  UL86OOUT      skVARIE
083300060420     C*
083400060420     C                   ENDSR
083500060911
083600060911
083700060911
083800060911     C     sumVARIE      BEGSR
083900060911     C*
084000060911     C* Reperisco le varie della bolla
084100060911     C                   EXSR      repVARIE
084200060911     C*
084300060911     C* Valorizzo VARIA 1
084400060911     C                   EVAL      jVARIE = 1
084500060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
084600060911     C                   EVAL      VAFSV1 = dVARIA_COD
084700060911     C                   EVAL      VAFVA1 = dVARIA_VAL
084800060911     C*
084900060911     C* Valorizzo VARIA 2
085000060911     C                   EVAL      jVARIE = 2
085100060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
085200060911     C                   EVAL      VAFSV2 = dVARIA_COD
085300060911     C                   EVAL      VAFVA2 = dVARIA_VAL
085400060911     C*
085500060911     C* Valorizzo VARIA 3
085600060911     C                   EVAL      jVARIE = 3
085700060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
085800060911     C                   EVAL      VAFSV3 = dVARIA_COD
085900060911     C                   EVAL      VAFVA3 = dVARIA_VAL
086000060911     C*
086100060911     C* Valorizzo VARIA 4
086200060911     C                   EVAL      jVARIE = 4
086300060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
086400060911     C                   EVAL      VAFSV4 = dVARIA_COD
086500060911     C                   EVAL      VAFVA4 = dVARIA_VAL
086600060911     C*
086700060911     C* Valorizzo VARIA 5
086800060911     C                   EVAL      jVARIE = 5
086900060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
087000060911     C                   EVAL      VAFSV5 = dVARIA_COD
087100060911     C                   EVAL      VAFVA5 = dVARIA_VAL
087200060911     C*
087300060911     C* Valorizzo VARIA 6
087400060911     C                   EVAL      jVARIE = 6
087500060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
087600060911     C                   EVAL      VAFSV6 = dVARIA_COD
087700060911     C                   EVAL      VAFVA6 = dVARIA_VAL
087800060911     C*
087900060911     C* Valorizzo VARIA 7
088000060911     C                   EVAL      jVARIE = 7
088100060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
088200060911     C                   EVAL      VAFSV7 = dVARIA_COD
088300060911     C                   EVAL      VAFVA7 = dVARIA_VAL
088400060911     C*
088500060911     C* Valorizzo VARIA 8
088600060911     C                   EVAL      jVARIE = 8
088700060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
088800060911     C                   EVAL      VAFSV8 = dVARIA_COD
088900060911     C                   EVAL      VAFVA8 = dVARIA_VAL
089000060911     C*
089100060911     C* Totalizzo tutte le restanti varie
089200060911     C                   DOW       jVARIE <= %elem(skVARIE)
089300060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
089400060911     C                   EVAL      VAFVAX = VAFVAX + dVARIA_VAL
089500060911     C                   ADD       1             jVARIE
089600060911     C                   ENDDO
089700060911     C*
089800060911     C                   ENDSR
089900060418
090000060911
090100060911
090200060911
090300060911     C     repVARIE_RMN  BEGSR
090400060911     C*
090500060911     C* Innanzitutto ordino la schiera
090600060911     C                   SORTA     skVARIE_RMN
090700060911     C*
090800060911     C* Scorro tutta la schiera dei depositi varie x RMN
090900060911     C                   z-add     1             yVARIE
091000060911     C                   dow       yVARIE <= %elem(skVARIE_RMN)
091100060912     C                   if        skVARIE_RMN(yVARIE) = *blanks
091200060912     C                   leave
091300060912     C                   else
091400060911     C                   eval      dsVARIA_RMN = skVARIE_RMN(yVARIE)
091500060912     C                   if        dRMN = *zeros
091600060912     C                   leave
091700060912     C                   else
091800060911     C                   if        dRMN = tasRMN
091900060912     C*
092000060912     C* Aggiungo subito il porto e l'imponibile
092100060912     C                   EVAL      VAFPOR = VAFPOR + dPOR
092200060912     C                   EVAL      VAFIMV = VAFIMV + dIMV
092300060911     C*
092400060911     C* Analizzo le varie della bolla con medesimo RMN
092500060911     C                   eval      skVARIE = dVARIE
092600060911     C*
092700060911     C* Scorro tutta la schiera delle varie reperite x la bolla corrente
092800060911     C                   EVAL      jVARIE = 1
092900060911     C                   DOW       jVARIE <= %elem(skVARIE)
093000060912     C                   MOVEL     'N'           wVariaVAF         1
093100060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
093200060911     C*
093300060911     C                   IF        wVariaVAF = 'N' AND
093400060911     C                             dVARIA_COD <> *blanks AND
093500060911     C                             dVARIA_COD = VAFSV1
093600060911     C                   EVAL      VAFVA1 = VAFVA1 + dVARIA_VAL
093700060911     C                   MOVEL     'S'           wVariaVAF
093800060911     C                   ENDIF
093900060911     C*
094000060911     C                   IF        wVariaVAF = 'N' AND
094100060911     C                             dVARIA_COD <> *blanks AND
094200060911     C                             dVARIA_COD = VAFSV2
094300060911     C                   EVAL      VAFVA2 = VAFVA2 + dVARIA_VAL
094400060911     C                   MOVEL     'S'           wVariaVAF
094500060911     C                   ENDIF
094600060911     C*
094700060911     C                   IF        wVariaVAF = 'N' AND
094800060911     C                             dVARIA_COD <> *blanks AND
094900060911     C                             dVARIA_COD = VAFSV3
095000060911     C                   EVAL      VAFVA3 = VAFVA3 + dVARIA_VAL
095100060911     C                   MOVEL     'S'           wVariaVAF
095200060911     C                   ENDIF
095300060911     C*
095400060911     C                   IF        wVariaVAF = 'N' AND
095500060911     C                             dVARIA_COD <> *blanks AND
095600060911     C                             dVARIA_COD = VAFSV4
095700060911     C                   EVAL      VAFVA4 = VAFVA4 + dVARIA_VAL
095800060911     C                   MOVEL     'S'           wVariaVAF
095900060911     C                   ENDIF
096000060911     C*
096100060911     C                   IF        wVariaVAF = 'N' AND
096200060911     C                             dVARIA_COD <> *blanks AND
096300060911     C                             dVARIA_COD = VAFSV5
096400060911     C                   EVAL      VAFVA5 = VAFVA5 + dVARIA_VAL
096500060911     C                   MOVEL     'S'           wVariaVAF
096600060911     C                   ENDIF
096700060911     C*
096800060911     C                   IF        wVariaVAF = 'N' AND
096900060911     C                             dVARIA_COD <> *blanks AND
097000060911     C                             dVARIA_COD = VAFSV6
097100060911     C                   EVAL      VAFVA6 = VAFVA6 + dVARIA_VAL
097200060911     C                   MOVEL     'S'           wVariaVAF
097300060911     C                   ENDIF
097400060911     C*
097500060911     C                   IF        wVariaVAF = 'N' AND
097600060911     C                             dVARIA_COD <> *blanks AND
097700060911     C                             dVARIA_COD = VAFSV7
097800060911     C                   EVAL      VAFVA7 = VAFVA7 + dVARIA_VAL
097900060911     C                   MOVEL     'S'           wVariaVAF
098000060911     C                   ENDIF
098100060911     C*
098200060911     C                   IF        wVariaVAF = 'N' AND
098300060911     C                             dVARIA_COD <> *blanks AND
098400060911     C                             dVARIA_COD = VAFSV8
098500060911     C                   EVAL      VAFVA8 = VAFVA8 + dVARIA_VAL
098600060911     C                   MOVEL     'S'           wVariaVAF
098700060911     C                   ENDIF
098800060911     C*
098900060911     C* Se varia già attribuita bene, altrimenti cerco 1 "buco varia" nn già utilizzato
099000060911     C                   IF        wVariaVAF <> 'S'
099100060911     C*
099200060911     C                   IF        wVariaVAF = 'N' AND
099300060911     C                             VAFSV1 = *blanks AND dVARIA_COD <> *blanks
099400060911     C                   EVAL      VAFSV1 = dVARIA_COD
099500060911     C                   EVAL      VAFVA1 = dVARIA_VAL
099600060911     C                   MOVEL     'S'           wVariaVAF
099700060911     C                   ENDIF
099800060911     C*
099900060911     C                   IF        wVariaVAF = 'N' AND
100000060911     C                             VAFSV2 = *blanks AND dVARIA_COD <> *blanks
100100060911     C                   EVAL      VAFSV2 = dVARIA_COD
100200060911     C                   EVAL      VAFVA2 = dVARIA_VAL
100300060911     C                   MOVEL     'S'           wVariaVAF
100400060911     C                   ENDIF
100500060911     C*
100600060911     C                   IF        wVariaVAF = 'N' AND
100700060911     C                             VAFSV3 = *blanks AND dVARIA_COD <> *blanks
100800060911     C                   EVAL      VAFSV3 = dVARIA_COD
100900060911     C                   EVAL      VAFVA3 = dVARIA_VAL
101000060911     C                   MOVEL     'S'           wVariaVAF
101100060911     C                   ENDIF
101200060911     C*
101300060911     C                   IF        wVariaVAF = 'N' AND
101400060911     C                             VAFSV4 = *blanks AND dVARIA_COD <> *blanks
101500060911     C                   EVAL      VAFSV4 = dVARIA_COD
101600060911     C                   EVAL      VAFVA4 = dVARIA_VAL
101700060911     C                   MOVEL     'S'           wVariaVAF
101800060911     C                   ENDIF
101900060911     C*
102000060911     C                   IF        wVariaVAF = 'N' AND
102100060911     C                             VAFSV5 = *blanks AND dVARIA_COD <> *blanks
102200060911     C                   EVAL      VAFSV5 = dVARIA_COD
102300060911     C                   EVAL      VAFVA5 = dVARIA_VAL
102400060911     C                   MOVEL     'S'           wVariaVAF
102500060911     C                   ENDIF
102600060911     C*
102700060911     C                   IF        wVariaVAF = 'N' AND
102800060911     C                             VAFSV6 = *blanks AND dVARIA_COD <> *blanks
102900060911     C                   EVAL      VAFSV6 = dVARIA_COD
103000060911     C                   EVAL      VAFVA6 = dVARIA_VAL
103100060911     C                   MOVEL     'S'           wVariaVAF
103200060911     C                   ENDIF
103300060911     C*
103400060911     C                   IF        wVariaVAF = 'N' AND
103500060911     C                             VAFSV7 = *blanks AND dVARIA_COD <> *blanks
103600060911     C                   EVAL      VAFSV7 = dVARIA_COD
103700060911     C                   EVAL      VAFVA7 = dVARIA_VAL
103800060911     C                   MOVEL     'S'           wVariaVAF
103900060911     C                   ENDIF
104000060911     C*
104100060911     C                   IF        wVariaVAF = 'N' AND
104200060911     C                             VAFSV8 = *blanks AND dVARIA_COD <> *blanks
104300060911     C                   EVAL      VAFSV8 = dVARIA_COD
104400060911     C                   EVAL      VAFVA8 = dVARIA_VAL
104500060911     C                   MOVEL     'S'           wVariaVAF
104600060911     C                   ENDIF
104700060911     C*
104800060911     C* Se varia già attribuita bene, altrimenti la totalizzo nelle "varie cumulative generiche"
104900060911     C                   IF        wVariaVAF <> 'S'
105000060911     C                   EVAL      VAFVAX = VAFVAX + dVARIA_VAL
105100060911     C                   ENDIF
105200060911     C*
105300060911     C                   ENDIF
105400060911     C*
105500060911     C                   ADD       1             jVARIE
105600060911     C                   ENDDO
105700060911     C*
105800060912     C                   clear                   dRMN
105900060912     C                   clear                   dNSP
106000060912     C                   eval      skVARIE_RMN(yVARIE) = dsVARIA_RMN
106100060912     C*
106200060912     C                   endif
106300060911     C                   endif
106400060911     C                   endif
106500060911     C                   add       1             yVARIE
106600060911     C                   enddo
106700060911     C*
106800060911     C                   ENDSR
106900060418
107000060419
107100060418     C     cartab        BEGSR
107200060418     C*
107300060419     C* Reperisco tutti i codici clienti che x la fatturazione sono unificati sul cliente richiesto
107400060418     C                   Z-ADD     *zeros        jVAF
107500060420     C                   CLEAR                   DS4C
107600060418     C                   EVAL      tblKUT = 1
107700060418     C                   EVAL      tblCOD = '4C'
107800060419     C                   EVAL      tblKEY = *blanks
107900060418     C     KEYtabel_P    SETLL     TABEL00F
108000060418     C                   IF        %found(TABEL00F)
108100060418     C     KEYtabel_P    READE     TABEL00F
108200060418     C                   DOW       not %eof(TABEL00F)
108300060418     C                   IF        tblFLG = *blanks
108400060418     C                   EVAL      DS4C = tblUNI
108500060418     C                   IF        §4CFKS = wKSUVAF
108600060418     C                   ADD       1             jVAF
108700060418     C                   MOVEL(P)  tblKEY        skVAF(jVAF)
108800060418     C                   ENDIF
108900060418     C                   ENDIF
109000060418     C     KEYtabel_P    READE     TABEL00F
109100060418     C                   ENDDO
109200060418     C                   ENDIF
109300060419     C*
109400060419     C* Reperisco i P.O. IVA da considerare x le fatture fine mese clienti codificati
109500060419     C                   CLEAR                   DSQC2
109600060419     C                   Z-ADD     1             tblKUT
109700060419     C                   MOVEL(P)  'QC'          tblCOD
109800060419     C                   MOVEL(P)  '2'           tblKEY
109900060419     C     KEYtabel      CHAIN     TABEL00F
110000060419     C                   IF        %found(TABEL00F)
110100060420     C                   IF        tblFLG = *blanks
110200060419     C                   MOVEL(P)  tblUNI        DSQC2
110300060419     C                   ENDIF
110400060420     C                   ENDIF
110500060420     C*
110600060420     C** Reperisco campi standard tassazione
110700060420     C                   CLEAR                   DSQT1
110800060420     C                   Z-ADD     1             tblKUT
110900060420     C                   MOVEL     'QT'          tblCOD
111000060420     C                   MOVEL     1             tblKEY
111100060420     C     KEYtabel      CHAIN     TABEL00F
111200060420     C                   IF        %found(TABEL00F)
111300060420     C                   IF        tblFLG = *blanks
111400060420     C                   MOVEL     TBLUNI        DSQT1
111500060420     C                   ENDIF
111600060420     C                   ENDIF
111700060911     C*
111800060911     C* Reperisco i tipi bolle e relativi flags spedizione/ricavo/etc...
111900060911     C                   Z-ADD     0             i
112000060911     C                   Z-ADD     1             tblKUT
112100060911     C                   MOVEL     'TB'          tblCOD
112200060911     C                   MOVEL     *BLANKS       tblKEY
112300060911     C     KEYtabel      SETLL     TABEL00F
112400060911     C                   READ      TABEL00F                               98
112500060911     C     *IN98         DOWEQ     *OFF
112600060911     C     tblCOD        ANDEQ     'TB'
112700060911     C     tblFLG        IFEQ      *BLANKS
112800060911     C                   ADD       1             i
112900060911     C                   MOVEL(P)  tblKEY        TB(I)                          *TIPO BOLLA
113000060911     C                   MOVEL(P)  tblUNI        DSTB
113100060911     C                   MOVEL(P)  §TBRBL        RBL(I)
113200060911     C                   ENDIF
113300060911     C                   READ      TABEL00F                               98
113400060911     C                   ENDDO
113500060418     C*
113600060418     C                   ENDSR
113700991027
113800060419
113900060419
114000060419     C     chkPAR        BEGSR
114100060419     C*
114200060419     C* Estrapolo il codice cliente unificante (unificante secondo tabella 4C) d fatturazione
114300060419     C                   EVAL      wKSUVAFa = %subst(prmppt:2:7)
114400060419     C                   MOVE(P)   wKSUVAFa      wKSUVAF
114500060419     C*
114600060419     C* Se presente nei parametri la forzatura periodo fatturazione (anno + mese) considero quello
114700060419     C* altrimenti considero il mese precedente a quello corrente
114800060419     C                   EVAL      wAAAAMMa = %subst(prmppt:9:6)
114900060419     C                   MOVE(P)   wAAAAMMa      wAAAAMM
115000060419     C                   IF        wAAAAMM = *zeros
115100060419     C                   Z-ADD     wAAAAMMcor    wPERIODO
115200060419     C                   EVAL      wMM = wMM - 1                                * mese precedente
115300060419     C                   ELSE
115400060419     C                   Z-ADD     wAAAAMM       wPERIODO
115500060419     C                   ENDIF
115600060419     C*
115700060420     C* Calcolo il primo giorno del mese successivo
115800060419     C                   IF        wMM = 12
115900060420     C***                EVAL      wDataFIN = 0101*10000+wAAAA+1
116000060420     C                   EVAL      wDataFIN = (wAAAA+1)*10000+0101
116100060419     C                   ELSE
116200060420     C***                EVAL      wDataFIN = 01*1000000+(wMM+1)*10000+wAAAA
116300060420     C                   EVAL      wDataFIN = wAAAA*10000+(wMM+1)*100+01
116400060419     C                   ENDIF
116500060420     C*
116600060420     C* Calcolo l'ultimo giorno del mese precedente
116700060420     C     *ISO          MOVE      wDataFIN      wDataISO
116800060420     C     wDataISO      SUBDUR    1:*D          wDataISO
116900060420     C     *ISO          MOVE      wDataISO      wDataINZ
117000060420     C*
117100060420     C* Calcolo il primo giorno del mese precedente
117200060420     C***                IF        wMM = 01
117300060420     C***                EVAL      wDataINZ = 0112*10000+wAAAA-1
117400060420     C***                ELSE
117500060420     C***                EVAL      wDataINZ = 01*1000000+(wMM-1)*10000+wAAAA
117600060420     C***                ENDIF
117700060419     C*
117800060419     C                   ENDSR
117900060419
118000060418
118100991027      /TITLE Operazioni iniziali.
118200991027     C     *inzsr        BEGSR
118300991027     C*
118400991027     C     *ENTRY        PLIST
118500991027     C                   parm                    prmppt
118600991027     C     wrkesito      parm      wrkesito      prmesito
118700060419     C*
118800060419     C* CALCOLA LA DATA CORRENTE
118900060419     C                   time                    wn14             14 0
119000060419     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
119100060419     C                   z-add     wn8           g08dat
119200060419     C                   z-add     *zeros        g08inv
119300060419     C                   movel     '0'           g08err
119400060419     C                   call      'XSRDA8'
119500060419     C                   parm                    wlbda8
119600060419     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
119700060419     C                   movel     datcor        wAAAAMMcor        6 0
119800060413     C*
119900060413     C* Definizione chiavi di lettura
120000060419     C*
120100060419     C* TABEL00F - Completa
120200060419     C     KEYtabel      KLIST
120300060419     C                   KFLD                    tblKUT
120400060419     C                   KFLD                    tblCOD
120500060419     C                   KFLD                    tblKEY
120600060418     C*
120700060418     C* TABEL00F - Parziale
120800060418     C     KEYtabel_P    KLIST
120900060418     C                   KFLD                    tblKUT
121000060418     C                   KFLD                    tblCOD
121100060413     C*
121200060413     C* TITAS31L - Completa
121300060413     C     KEYtas31      KLIST
121400060413     C                   KFLD                    tasKSC
121500060420     C                   KFLD                    tasDFT
121600060413     C*
121700060413     C* TNCSB03L - Completa
121800060413     C     KEYcsb03      KLIST
121900060413     C                   KFLD                    tasAAS
122000060413     C                   KFLD                    tasLNP
122100060413     C                   KFLD                    tasNRS
122200060413     C                   KFLD                    tasNSP
122300060418     C*
122400060418     C* TIGCP51L - Parziale
122500060418     C     KEYgcp51_P    KLIST
122600060418     C                   KFLD                    tasAAS
122700060418     C                   KFLD                    tasLNP
122800060418     C                   KFLD                    tasNRS
122900060418     C                   KFLD                    tasNSP
123000060911     C                   KFLD                    gcpFRG
123100060418     C*
123200060418     C* TITA430C - Completa
123300060418     C     KEYta430      KLIST
123400060418     C                   KFLD                    tasAAS
123500060418     C                   KFLD                    tasLNP
123600060418     C                   KFLD                    tasNRS
123700060418     C                   KFLD                    tasNSP
123800060418     C                   KFLD                    ta4TRC
123900060420     C*
124000060420     C* TITAA30C - Completa
124100060420     C     KEYtaa30      KLIST
124200060420     C                   KFLD                    tasAAS
124300060420     C                   KFLD                    tasLNP
124400060420     C                   KFLD                    tasNRS
124500060420     C                   KFLD                    tasNSP
124600060420     C                   KFLD                    taaTRC
124700060420     C*
124800060420     C* FIAR531C - Parziale
124900060420     C     KEYar531_P    KLIST
125000060420     C                   KFLD                    tasAAS
125100060420     C                   KFLD                    tasLNP
125200060420     C                   KFLD                    tasNRS
125300060420     C                   KFLD                    tasNSP
125400060420     C                   KFLD                    ar5TRD
125500991027     C*
125600991027     C                   ENDSR
125700060421** DEC - COSTANTI
125800060421<?xml version="1.0"?>
125900060421<?mso-application progid="Excel.Sheet"?>
126000060421<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
126100060421xmlns:o="urn:schemas-microsoft-com:office:office"
126200060421xmlns:x="urn:schemas-microsoft-com:office:excel"
126300060421xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
126400060421xmlns:html="http://www.w3.org/TR/REC-html40">
126500060421</Workbook>
126600060421<Worksheet ss:Name="PAG1">
126700060421</Worksheet>
126800060421<Table border="1">
126900060421</Table>
127000060421<Row>
127100060421</Row>
127200060421<Cell><Data ss:Type="String">
127300060421<Cell><Data ss:Type="Number">
127400060421</Data></Cell>
127500060421
127600060421
127700060421
127800060704** CFG - CODICI FINE GIACENZA
127900060704001
128000060704003
128100060704005
128200060704006
128300060704008
128400060704010
128500060704
128600060704
128700060704
128800060704
