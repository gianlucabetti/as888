000100060426     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400060912     FTITAS31C  IF   E           K DISK
000500060912     FTITAS30C  IF   E           K DISK
000600060912     F                                     rename(titas000:titas0)
000700060912     F                                     rename(titas010:titas1)
000800060912     F                                     rename(titasP00:titasP)
000900060418     FTITA430C  IF   E           K DISK
001000060420     FTITAA30C  IF   E           K DISK
001100060418     FTIGCP51L  IF   E           K DISK
001200060413     FTNCSB03L  IF   E           K DISK
001300060420     FFIAR531C  IF   E           K DISK
001400060418     FTABEL00F  IF   E           K DISK
001500090309     FTIVAWWWT  UF A E             DISK    commit
001600060912
001700060912
001800060421
001900060421     D*------------
002000060421     D* DECODIFICHE COSTANTI
002100060421     D*------------
002200060704     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
002300060704     D CFG             S              3    DIM(10) CTDATA PERRCD(1)
002400060331
002500060418     D****
002600060418     D* SCHIERE DI WRK
002700060418     D****
002800060418     D skVAF           s              7  0 inz dim(1000)
002900060418     D jVAF            s              4  0 inz
003000060911
003100060911
003200060911     D*-------------------
003300060911     D* SCHIERE FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA E TEMPI DI CONSEGNA
003400060911     D*-------------------
003500060911     D RBL             S              1    DIM(20)                              RAGGRUPPAMENTO BOLLA
003600060911     D*-------------------
003700060911     D* SCHIERE TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003800060911     D*-------------------
003900060911     D TB              S              2    DIM(20)                              TIP0 BOLLA
004000060419
004100060419     D*------------------
004200060419     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300060419     D*------------------
004400060419     D WLBDA8          DS                  INZ
004500060419     D  G08DAT                 1      8  0
004600060419     D  G08INV                 9     16  0
004700060419     D  G08ERR                17     17
004800060419     D  G08TGI                18     22  0
004900060420
005000060420     D*------------------
005100060420     D* DS REPERIMENTO DATI CLIENTE
005200060420     D*-------------------
005300060420     D BS69DS        E DS                  EXTNAME(TIBS69DS)
005400060420     D ACODS         E DS                  EXTNAME(CNACO00F)
005500060420     D INDDS         E DS                  EXTNAME(CNIND00F)
005600060420     D CLPDS         E DS                  EXTNAME(CNCLP00F)
005700060420     D CLSDS         E DS                  EXTNAME(FNCLS00F)
005800060419
005900060419     D*------------
006000060420     D* DS ESTERNE VARIE
006100060419     D*------------
006200060420     D DSVAF         E DS                  EXTNAME(TIVAF00T)
006300060420     D DSVAFA        E DS                  EXTNAME(TIVAFAWT)
006400060420     D TRUL86DS      E DS
006500060420     D DAR5BNB       E DS
006600060420     D DSQT1         E DS
006700060420     D DS4C          E DS
006800060704     D DS2C          E DS                                                       * occhio in fondo !!
006900060911     D DSTB          E DS
007000060420
007100060420     D*------------
007200060420     D* SCHIERE SIGLE E DESCRIZIONI VARIE
007300060420     D*------------
007400060420     D skVARIE         S                   LIKE(dsVARIA) DIM(50) INZ
007500060420     D jVARIE          S              2  0 INZ
007600060420     D*------------------
007700060420     D* DS RIDEFINIZIONE OUTPUT ELEMENTO PROCEDURA TRUL86R
007800060420     D*------------------
007900060420     D dsVARIA         DS
008000060420     D  dVARIA_COD                    1
008100060420     D  dVARIA_VAL                   13  3
008200060911
008300060911     D*------------
008400060911     D* SCHIERE SIGLE E DESCRIZIONI VARIE - "RMN"
008500060911     D*------------
008600060912     D skVARIE_RMN     S                   LIKE(dsVARIA_RMN) DIM(22000)
008700060911     D                                     INZ DESCEND
008800060911     D yVARIE          S              5  0 INZ
008900060911     D*------------------
009000060911     D* DS RIDEFINIZIONE OUTPUT ELEMENTO PROCEDURA TRUL86R - "RMN"
009100060911     D*------------------
009200060911     D dsVARIA_RMN     DS
009300060911     D  dRMN                         15  0
009400060912     D  dAAS                          4  0
009500060912     D  dLNP                          3  0
009600060912     D  dNRS                          2  0
009700060912     D  dNSP                          7  0
009800060912     D  dTBL                          2
009900060911     D  dPOR                         13  3
010000060911     D  dVARIE                             LIKE(dsVARIA) DIM(50) INZ
010100060911     D  dIMV                         15  3
010200060420
010300060420     D*------------
010400060420     D* DS RIDEFINIZIONE TABELLA "QC" TIPO RECORD "2"
010500060420     D*------------
010600060420     D DSQC2         E DS
010700060419
010800060419     D*------------------
010900060419     D* DS X PASSAGGIO PARAMETRI
011000060419     D*------------------
011100060419     D                 DS
011200060419     D  wPERIODO               1      6  0
011300060419     D  wMM                    5      6  0
011400060419     D  wAAAA                  1      4  0
011500060421
011600060421     D*------------
011700060421     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
011800060421     D*------------
011900060421     D iHtml           s             10    inz('<html>')
012000060421     D fHtml           s             10    inz('</html>')
012100060421     D iBody           s             10    inz('<body>')
012200060421     D fBody           s             10    inz('</body>')
012300060421     D iTable          s             20    inz('<table border="1">')
012400060421     D fTable          s             10    inz('</table>')
012500060426     D iThead          s             10    inz('<thead>')
012600060421     D fThead          s             10    inz('</thead>')
012700060421     D iRow            s              5    inz('<tr>')
012800060421     D fRow            s              5    inz('</tr>')
012900060421     D iCol            s              5    inz('<td>')
013000060421     D fCol            s              5    inz('</td>')
013100060426     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
013200060421     D fColBold        s              5    inz('</th>')
013300060418
013400060331     D****
013500060331     D* VARIABILI DI WRK
013600060331     D****
013700991027     D prmppt          s             50
013800991027     D prmesito        s              1
013900991027     D wrkesito        s                   like(prmesito)
014000060418     D wKSUVAFa        s              7
014100060418     D wKSUVAF         s              7  0
014200060420     D wAAAAMMa        s              6
014300060420     D wAAAAMM         s              6  0
014400060420     D wDataINZ        s              8  0
014500060420     D wDataFIN        s              8  0
014600060420     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
014700060420     D wTara           s                   like(§QTTPC)
014800060420     D wNtara          s                   like(TASPKF)
014900060421     D wBufferOut      s          10000    varying inz
015000060911     D i               s              5i 0 inz(*zeros)
015100060911     D j               s              5i 0 inz(*zeros)
015200991027
015300060419     C*
015400060419     C* Verifico ed effettuo considerazioni sui parametri ricevuti in input
015500060419     C                   EXSR      chkPAR
015600060418     C*
015700060418     C* Carico le tabelle occorrenti
015800060419     C                   EXSR      carTAB
015900060418     C*
016000060418     C* Verifico se già presenti dati nel file d output
016100030908     C                   EXSR      chkRECFILMBR
016200060418     C*
016300060418     C* Effettuo elaborazione
016400991027     C                   EXSR      traduci
016500060420     C*
016600060420     C* Effettuo chiamata x chiusura procedure esterne
016700060420     C                   CLEAR                   TRUL86DS
016800060420     C                   EVAL      UL86ITLA = 'C'
016900060420     C                   CALL      'TRUL86R'
017000060420     C                   PARM                    TRUL86DS
017100070221     C*
017200070221     C* Effettuo chiamata x chiusura procedure esterne
017300070221     C                   MOVEL     'C'           I69TLA
017400070221     C                   CALL      'TIBS69R'
017500070221     C                   PARM                    BS69DS
017600070221     C                   PARM                    ACODS
017700070221     C                   PARM                    INDDS
017800070221     C                   PARM                    CLPDS
017900070221     C                   PARM                    CLSDS
018000060418     C*
018100921023     C                   SETON                                        LR
018200060413
018300060413
018400030908
018500030908     C     chkRECFILMBR  BEGSR
018600030908     C*
018700060413     C                   READ      TIVAWWWT                               55
018800030908     C*
018900030908     C                   ENDSR
019000060413
019100060413
019200991027
019300991027     C     traduci       BEGSR
019400060421     C*
019500060421     C* Head iniziali della struttura HTML
019600060421     C                   EVAL      VAWDTA = iHtml + iBody + iTable
019700060421     C                   WRITE     TIVAW000
019800030325     C*
019900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
020000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
020100060421     C                   EVAL      wBufferOut = iThead + iRow
020200060421     C                             +iColBold+ 'VAFAAS'+fColBold
020300060421     C                             +iColBold+ 'VAFLNP'+fColBold
020400060421     C                             +iColBold+ 'VAFNRS'+fColBold
020500060421     C                             +iColBold+ 'VAFNSP'+fColBold
020600060421     C                             +iColBold+ 'VAFTBL'+fColBold
020700060421     C                             +iColBold+ 'VAFMGS'+fColBold
020800060421     C                             +iColBold+ 'VAFKSC'+fColBold
020900060421     C                             +iColBold+ 'VAFLNA'+fColBold
021000060421     C                             +iColBold+ 'VAFNCL'+fColBold
021100060421     C                             +iColBold+ 'VAFPKB'+fColBold
021200060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
021300060426     C                   IF        %len(%trim(wBufferOut)) >= 700
021400060426     C                   EVAL      VAWDTA = wBufferOut
021500060426     C                   WRITE     TIVAW000
021600060426     C                   EVAL      wBufferOut = *blanks
021700060426     C                   ENDIF
021800060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
021900060426     C                              iColBold+ 'VAFVLF'+fColBold
022000060421     C                             +iColBold+ 'VAFQFT'+fColBold
022100060421     C                             +iColBold+ 'VAFPOR'+fColBold
022200060421     C                             +iColBold+ 'VAFSV1'+fColBold
022300060421     C                             +iColBold+ 'VAFVA1'+fColBold
022400060421     C                             +iColBold+ 'VAFSV2'+fColBold
022500060421     C                             +iColBold+ 'VAFVA2'+fColBold
022600060421     C                             +iColBold+ 'VAFSV3'+fColBold
022700060421     C                             +iColBold+ 'VAFVA3'+fColBold
022800060421     C                             +iColBold+ 'VAFSV4'+fColBold
022900060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
023000060426     C                   IF        %len(%trim(wBufferOut)) >= 700
023100060426     C                   EVAL      VAWDTA = wBufferOut
023200060426     C                   WRITE     TIVAW000
023300060426     C                   EVAL      wBufferOut = *blanks
023400060426     C                   ENDIF
023500060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
023600060426     C                              iColBold+ 'VAFVA4'+fColBold
023700060421     C                             +iColBold+ 'VAFSV5'+fColBold
023800060421     C                             +iColBold+ 'VAFVA5'+fColBold
023900060421     C                             +iColBold+ 'VAFSV6'+fColBold
024000060421     C                             +iColBold+ 'VAFVA6'+fColBold
024100060421     C                             +iColBold+ 'VAFSV7'+fColBold
024200060421     C                             +iColBold+ 'VAFVA7'+fColBold
024300060421     C                             +iColBold+ 'VAFSV8'+fColBold
024400060421     C                             +iColBold+ 'VAFVA8'+fColBold
024500060421     C                             +iColBold+ 'VAFVAX'+fColBold
024600060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
024700060426     C                   IF        %len(%trim(wBufferOut)) >= 700
024800060426     C                   EVAL      VAWDTA = wBufferOut
024900060426     C                   WRITE     TIVAW000
025000060426     C                   EVAL      wBufferOut = *blanks
025100060426     C                   ENDIF
025200060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
025300060426     C                              iColBold+ 'VAFIMV'+fColBold
025400060421     C                             +iColBold+ 'VAFDFT'+fColBold
025500060421     C                             +iColBold+ 'VAFNFT'+fColBold
025600060421     C                             +iColBold+ 'VAFFIV'+fColBold
025700060421     C                             +iColBold+ 'VAFDIV'+fColBold
025800060421     C                             +iColBold+ 'VAFDRT'+fColBold
025900060421     C                             +iColBold+ 'VAFRMN'+fColBold
026000060421     C                             +iColBold+ 'VAFRMA'+fColBold
026100060421     C                             +iColBold+ 'VAFRMX'+fColBold
026200060421     C                             +iColBold+ 'VAFFTC'+fColBold
026300060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
026400060426     C                   IF        %len(%trim(wBufferOut)) >= 700
026500060426     C                   EVAL      VAWDTA = wBufferOut
026600060426     C                   WRITE     TIVAW000
026700060426     C                   EVAL      wBufferOut = *blanks
026800060426     C                   ENDIF
026900060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
027000060426     C                              iColBold+ 'VAFTC2'+fColBold
027100060421     C                             +iColBold+ 'VAFTSP'+fColBold
027200060421     C                             +iColBold+ 'VAFFAP'+fColBold
027300060421     C                             +iColBold+ 'VAFFIN'+fColBold
027400060421     C                             +iColBold+ 'VAFCTR'+fColBold
027500060421     C                             +iColBold+ 'VAFNAS'+fColBold
027600060421     C                             +iColBold+ 'VAFCTS'+fColBold
027700060421     C                             +iColBold+ 'VAFCAD'+fColBold
027800060421     C                             +iColBold+ 'VAFLOD'+fColBold
027900060421     C                             +iColBold+ 'VAFPRD'+fColBold
028000060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
028100060426     C                   IF        %len(%trim(wBufferOut)) >= 700
028200060426     C                   EVAL      VAWDTA = wBufferOut
028300060426     C                   WRITE     TIVAW000
028400060426     C                   EVAL      wBufferOut = *blanks
028500060426     C                   ENDIF
028600060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
028700060426     C                              iColBold+ 'VAFNZD'+fColBold
028800060421     C                             +iColBold+ 'VAFVAS'+fColBold
028900060421     C                             +iColBold+ 'VAFIAS'+fColBold
029000060421     C                             +iColBold+ 'VAFFPC'+fColBold
029100060421     C                             +iColBold+ 'VAFPKC'+fColBold
029200060421     C                             +iColBold+ 'VAFFVC'+fColBold
029300060421     C                             +iColBold+ 'VAFVLC'+fColBold
029400060421     C                             +iColBold+ 'VAFRSD'+fColBold
029500060421     C                             +iColBold+ 'VAFIND'+fColBold
029600060421     C                             +iColBold+ 'VAFRSM'+fColBold
029700060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
029800060426     C                   IF        %len(%trim(wBufferOut)) >= 700
029900060426     C                   EVAL      VAWDTA = wBufferOut
030000060426     C                   WRITE     TIVAW000
030100060426     C                   EVAL      wBufferOut = *blanks
030200060426     C                   ENDIF
030300060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
030400060426     C                              iColBold+ 'VAFINM'+fColBold
030500060421     C                             +iColBold+ 'VAFCAM'+fColBold
030600060421     C                             +iColBold+ 'VAFLOM'+fColBold
030700060421     C                             +iColBold+ 'VAFPRM'+fColBold
030800060421     C                             +iColBold+ 'VAFNZM'+fColBold
030900060421     C                             +iColBold+ 'VAFRMO'+fColBold
031000060421     C                             +iColBold+ 'VAFCMO'+fColBold
031100060421     C                             +iColBold+ 'VAFNMO'+fColBold
031200060421     C                             +iColBold+ 'GIACENZA'+fColBold
031300060421     C                             +iColBold+ 'CONS_AI_PIANI'+fColBold
031400060426     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
031500060426     C                   IF        %len(%trim(wBufferOut)) >= 700
031600060426     C                   EVAL      VAWDTA = wBufferOut
031700060426     C                   WRITE     TIVAW000
031800060426     C                   EVAL      wBufferOut = *blanks
031900060426     C                   ENDIF
032000060426     C                   EVAL      wBufferOut = %trim(wBufferOut) +
032100060426     C                              iColBold+ 'CONS_APPUNTAMENTO'+fColBold
032200060421     C                             +iColBold+ 'CONS_LOCALITA_DISAGIATA'+fColBold
032300060421     C                             +iColBold+ 'CONS_ISOLE_MINORI'+fColBold
032400060421     C                             +iColBold+ 'CONS_DISAGIATA'+fColBold
032500060421     C                             +iColBold+ 'POD_IMAGE'+fColBold
032600060421     C                             +iColBold+ 'DIROTTAMENTO'+fColBold
032700060421     C                             +iColBold+ 'IMPORTO_CONTRASSEGNO'+fColBold
032800060421     C                             +iColBold+ 'DIVISA_CONTRASSEGNO'+fColBold
032900060704     C                             +iColBold+ 'RICONSEGNA_SU_GIACENZA'+fColBold
033000060704     C                             +iColBold+ 'ZONA_FRANCA'+fColBold
033100060421     C                   EVAL      wBufferOut = wBufferOut + fRow + fThead
033200060426     C                   EVAL      VAWDTA = wBufferOut
033300060426     C                   WRITE     TIVAW000
033400060911     C*
033500030325     C                   ENDIF
033600991027     C*
033700060418     C* Ciclo x tutti i codici clienti ricondotti al cliente unificante fatture (tab. 4C)
033800060418     C                   EVAL      jVAF = 1
033900060418     C                   DOW       jVAF <= %elem(skVAF)
034000060418     C                   IF        skVAF(jVAF) = *zeros
034100060418     C                   LEAVE
034200060418     C                   ELSE
034300060912     C*
034400060912     C* Effettuo 2 giri: 1° x le bolle d recupero e 2° x le bolle originali
034500060912     C                   Z-ADD     1             wGiro             1 0
034600060912     C                   DOW       wGiro <= 2
034700060912     C     *loval        SETLL     TITAS31C
034800060912     C                   EVAL      tasKSC = skVAF(jVAF)
034900060912     C                   EVAL      tasDFT = wDataINZ
035000060420     C     KEYtas31      SETLL     TITAS31C
035100060413     C                   IF        %found(TITAS31C)
035200060420     C     skVAF(jVAF)   READE     TITAS31C
035300060413     C                   DOW       not %eof(TITAS31C)
035400060418     C*
035500060419     C* Inizializzo il flag che condiziona l'elaborazione del record bolla corrente
035600060419     C                   MOVEL     'S'           wRecOK            1
035700060911     C                   MOVEL     'S'           wRecOkSped        1
035800060420     C                   MOVEL     'N'           wLeave            1
035900060419     C*
036000060419     C* Effettuo verifiche validità record
036100060419     C                   EXSR      chkREC
036200060912     C*
036300060912     C* Se 2° giro elaboro le bolle originali e compngo il dato
036400060912     C                   IF        wGiro = 2
036500060419     C*
036600060419     C* Se record da considerare procedo con le considerazioni
036700060911     C                   IF        wRecOK     = 'S'
036800060419     C*
036900060419     C* Re inizializzo il flag che indica se la bolla corrente è da considerare
037000060419     C                   MOVEL     'N'           wRecOK            1
037100061218     C*
037200061218     C* Eseguo routine x considerazioni su flag "particolari"
037300061218     C                   EXSR      repFlg
037400060413     C*
037500060911     C* Se tutto ok procedo con scrittura del buffer d out
037600060912     C                   IF        wRecOK='S' AND wRecOkSped='S'
037700060912     C*
037800060912     C* Inizializzo il buffer del file d output e delle ds d ridefinizione dati
037900060912     C                   CLEAR                   TIVAW000
038000060912     C                   CLEAR                   DSVAF
038100060912     C                   CLEAR                   DSVAFA
038200060912     C*
038300060912     C* Eseguo quindi la routine d formazione del dato e scrittura dello stesso
038400060912     C                   EXSR      wriFile
038500060912     C*
038600060912     C                   ENDIF
038700991027     C*
038800060420     C* Se già oltre data superiore range => esco dal ciclo
038900060420     C                   IF        wLeave = 'S'
039000060420     C                   LEAVE
039100060420     C                   ENDIF
039200060420     C*
039300060420     C                   ENDIF
039400060912     C                   ENDIF
039500060420     C*
039600060420     C     skVAF(jVAF)   READE     TITAS31C
039700030325     C                   ENDDO
039800060418     C*
039900060418     C                   ENDIF
040000060912     C*
040100060912     C                   ADD       1             wGiro
040200060912     C                   ENDDO
040300060912     C*
040400060420     C                   ENDIF
040500060418     C                   ADD       1             jVAF
040600060418     C                   ENDDO
040700060912     C*
040800060912     C* Scarico le eventuali bolle d recupero rimaste "orfane" d madre in 1 periodo precedente
040900060912     C                   EXSR      repBOLLE_RMN
041000060421     C*
041100060912     C* Head finali della struttura HTML
041200060421     C                   EVAL      VAWDTA = fTable + fBody + fHtml
041300060421     C                   WRITE     TIVAW000
041400060421     C*
041500030325     C                   EVAL      wrkesito = '0'
041600991027     C*
041700910830     C                   ENDSR
041800061218
041900061218
042000061218
042100061218     C     repFlg        BEGSR
042200061218     C*
042300061218     C* Inizializzo tutti i flag d wrk relativi agli attributi della bolla corrente
042400061218     C                   MOVEL     *blanks       wGIAC             1
042500061218     C                   MOVEL     *blanks       wPIAN             1
042600061218     C                   MOVEL     *blanks       wAPPU             1
042700061218     C                   MOVEL     *blanks       wLDIS             1
042800061218     C                   MOVEL     *blanks       wISOL             1
042900061218     C                   MOVEL     *blanks       wCDIS             1
043000061218     C                   MOVEL     *blanks       wPODI             1
043100061218     C                   MOVEL     *blanks       wDIRO             1
043200061218     C                   MOVEL     *blanks       wZONF             1
043300061218     C                   MOVEL     *blanks       wRICO             1
043400061218     C*
043500061218     C* Aggancio il file giacenze della bolla corrente
043600061218     C                   EVAL      gcpFRG = *zeros
043700061218     C     KEYgcp51_P    CHAIN     TIGCP51L
043800061218     C                   IF        %found(TIGCP51L)
043900061218     C                   MOVEL     'S'           wRecOK
044000061218     C                   MOVEL     'S'           wGIAC
044100061218     C* Verifico se "riconsegna x giacenza"
044200061218     C                   IF        gcpCFG <> *blanks
044300061218     C     gcpCFG        LOOKUP    CFG                                    55
044400061218     C                   IF        %found
044500061218     C                   MOVEL     'S'           wRICO
044600061218     C                   ENDIF
044700061218     C                   ENDIF
044800061218     C                   ENDIF
044900061218     C*
045000061218     C* Aggancio il file contrassegni della bolla corrente
045100061218     C                   Z-ADD     *zeros        csbCAS
045200061218     C                   MOVEL     *blanks       csbVCA
045300061218     C     KEYcsb03      CHAIN     TNCSB03L
045400061218     C                   IF        %found(TNCSB03L)
045500061218     C                   IF        csbSTA <> 9
045600061218     C                   MOVEL     'S'           wRecOK
045700061218     C                   ENDIF
045800061218     C                   ENDIF
045900061218     C*
046000061218     C* Verifica se CAMPIONE D'ITALIA o LIVIGNO
046100061218     C                   IF        (tasCAD = '22060' and tasFIN = 'I') or
046200061218     C                             (tasCAD = '23030' and tasFIN = 'I')
046300061218     C                   MOVEL     'S'           wRecOK
046400061218     C                   MOVEL     'S'           wZONF
046500061218     C                   ENDIF
046600061218     C*
046700061218     C* Verifica se APPUNTAMENTO
046800061218     C                   IF        tasFTC = 'A' or
046900061218     C                             tasTC2 = 'A'
047000061218     C                   MOVEL     'S'           wRecOK
047100061218     C                   MOVEL     'S'           wAPPU
047200061218     C                   ENDIF
047300061218     C*
047400061218     C* Verifica se AI PIANI
047500061218     C                   IF        tasFTC = 'P' or
047600061218     C                             tasTC2 = 'P'
047700061218     C                   MOVEL     'S'           wRecOK
047800061218     C                   MOVEL     'S'           wPIAN
047900061218     C                   ENDIF
048000061218     C*
048100061218     C* Verifica se CONSEGNA DISAGIATA
048200061218     C                   IF        tasFTC = 'X' or
048300061218     C                             tasTC2 = 'X'
048400061218     C                   MOVEL     'S'           wRecOK
048500061218     C                   MOVEL     'S'           wCDIS
048600061218     C                   ENDIF
048700061218     C*
048800061218     C* Verifica se LOCALITA DISAGIATA
048900061218     C                   IF        tasFIN = 'D'
049000061218     C                   MOVEL     'S'           wRecOK
049100061218     C                   MOVEL     'S'           wLDIS
049200061218     C                   ENDIF
049300061218     C*
049400061218     C* Verifica se ISOLA MINORE
049500061218     C                   IF        tasFIN = 'I'
049600061218     C                   MOVEL     'S'           wRecOK
049700061218     C                   MOVEL     'S'           wISOL
049800061218     C                   ENDIF
049900061218     C*
050000061218     C* Verifica se P.O.D. IMAGE
050100061218     C                   IF        tasCBO = 'FI'
050200061218     C                   MOVEL     'S'           wRecOK
050300061218     C                   MOVEL     'S'           wPODI
050400061218     C                   ENDIF
050500061218     C*
050600061218     C* Verifica se DIROTTAMENTO
050700061218     C                   IF        tasCBO = 'F*'
050800061218     C                   MOVEL     'S'           wRecOK
050900061218     C                   MOVEL     'S'           wDIRO
051000061218     C                   ENDIF
051100061218     C*
051200061218     C* Se presente numero fattura considero sempre la bolla
051300061218     C                   IF        tasNFT > *zeros
051400061218     C                   MOVEL     'S'           wRecOK
051500061218     C                   ENDIF
051600061218     C*
051700061218     C                   ENDSR
051800060912
051900060912
052000060912
052100060912     C     wriFile       BEGSR
052200060912     C*
052300060912     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
052400060912     C                   EVAL      ta4TRC = 'A'
052500060912     C     KEYta430      CHAIN     TITA430C
052600060912     C                   IF        %found(TITA430C)
052700060912     C                   ELSE
052800060912     C                   CLEAR                   TITA4000
052900060912     C                   CLEAR                   TITA4P00
053000060912     C                   ENDIF
053100060912     C*
053200060912     C* Valorizzo i dati del destinatario nn già presenti sul TIVAF
053300060912     C                   EVAL      VHFRSD = TASRSD
053400060912     C                   EVAL      VHFIND = TASIND
053500060912     C*
053600060912     C* X reperire i dati del mittente verifico il codice del cliente mittente:
053700060912     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
053800060912     C                   MOVE(P)   tasCCM        wCLI              4 0
053900060912     C                   IF        wCLI =    0 OR
054000060912     C                             wCLI = 8888 OR
054100060912     C                             wCLI = 9999
054200060912     C                   EVAL      taaTRC = 'M'
054300060912     C     KEYtaa30      CHAIN     titaa30c
054400060912     C                   IF        %found(titaa30c)
054500060912     C                   EVAL      VHFRSM = TAARSC
054600060912     C                   EVAL      VHFINM = TAAIND
054700060912     C                   EVAL      VHFCAM = TAACAP
054800060912     C                   EVAL      VHFLOM = TAALOC
054900060912     C                   EVAL      VHFPRM = TAAPRV
055000060912     C                   EVAL      VHFNZM = TAANAZ
055100060912     C                   ENDIF
055200060912     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
055300060912     C                   ELSE
055400060912     C                   CLEAR                   BS69DS
055500060912     C                   CLEAR                   ACODS
055600060912     C                   CLEAR                   INDDS
055700060912     C                   CLEAR                   CLPDS
055800060912     C                   CLEAR                   CLSDS
055900060912     C                   Z-ADD     tasCCM        I69KAC
056000060912     C                   Z-ADD     tasCCM        I69KIN
056100060912     C                   CALL      'TIBS69R'
056200060912     C                   PARM                    BS69DS
056300060912     C                   PARM                    ACODS
056400060912     C                   PARM                    INDDS
056500060912     C                   PARM                    CLPDS
056600060912     C                   PARM                    CLSDS
056700060912     C     O69ERR        IFNE      '1'
056800060912     C                   EVAL      VHFRSM = ACORAG
056900060912     C                   EVAL      VHFINM = INDVIA
057000060912     C                   MOVEL(P)  INDCAP        VHFCAM
057100060912     C                   EVAL      VHFLOM = INDCIT
057200060912     C                   EVAL      VHFPRM = INDPRV
057300060912     C                   EVAL      VHFNZM = INDSTA
057400060912     C                   ENDIF
057500060912     C                   ENDIF
057600060912     C*
057700060912     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
057800060912     C                   EVAL      taaTRC = 'O'
057900060912     C     KEYtaa30      CHAIN     titaa30c
058000060912     C                   IF        %found(titaa30c)
058100060912     C                   EVAL      VHFRMO = TAARSC
058200060912     C                   EVAL      VHFCMO = TAACAP
058300060912     C                   EVAL      VHFNMO = TAANAZ
058400060912     C                   ENDIF
058500060912     C*
058600060912     C* Se particolità varia = 'O' recupero da FIAR5 il numero colli originali
058700060912     C                   IF        %subst(tasGVA:1:1) = 'O'
058800060912     C                   EVAL      ar5TRD = 'BNB'
058900060912     C                   CLEAR                   DAR5BNB
059000060912     C     KEYar531_P    CHAIN     FIAR531C
059100060912     C                   IF        %found(FIAR531C)
059200060912     C                   MOVEL     AR5UNI        DAR5BNB
059300060912     C                   ENDIF
059400060912     C* Colli originali
059500060912     C                   EVAL      VAFNCL = §AR5BCOR
059600060912     C* Colli TITAS
059700060912     C                   ELSE
059800060912     C                   Z-ADD     TASNCL        VAFNCL
059900060912     C                   ENDIF
060000060912     C*
060100060912     C* Se giro x bolle originali reperisco le varie anche delle "collegate"
060200060912     C                   IF        wGiro = 2
060300060912     C*
060400060912     C* Eseguo reperimento varie
060500060912     C                   EXSR      sumVARIE
060600060912     C*
060700060912     C* Eseguo reperimento varie altre bolle nn valide come spedizione con stesso riferimento num.
060800060912     C                   EXSR      repVARIE_RMN
060900060912     C*
061000060912     C                   ENDIF
061100061218     C*
061200061218     C* Se giro x bolle d recupero effettuo considerazioni x reperimento flag "particolari"
061300061218     C                   IF        wGiro = 3
061400061218     C*
061500061218     C* Eseguo routine x considerazioni su flag "particolari"
061600061218     C                   EXSR      repFlg
061700061218     C*
061800061218     C                   ENDIF
061900060912     C*
062000060912     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
062100060912     C                   EXSR      TAStoVAF
062200060912     C*
062300060912     C                   EVAL      wBufferOut = iRow +
062400060912     C                                  iCol+%trim(%editc(VAFAAS:'Q'))+fCol+
062500060912     C                                  iCol+%trim(%editc(VAFLNP:'Q'))+fCol+
062600060912     C                                  iCol+%trim(%editc(VAFNRS:'Q'))+fCol+
062700060912     C                                  iCol+%trim(%editc(VAFNSP:'Q'))+fCol+
062800060912     C                                  iCol+   %trim(VAFTBL)         +fCol+
062900060912     C                                  iCol+%trim(%editc(VAFMGS:'Q'))+fCol+
063000060912     C                                  iCol+%trim(%editc(VAFKSC:'Q'))+fCol+
063100060912     C                                  iCol+%trim(%editc(VAFLNA:'Q'))+fCol+
063200060912     C                                  iCol+%trim(%editc(VAFNCL:'Q'))+fCol+
063300060912     C                                  iCol+%trim(%editc(VAFPKB:'Q'))+fCol
063400060912     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
063500060912     C                   IF        %len(%trim(wBufferOut)) >= 700
063600060912     C                   EVAL      VAWDTA = wBufferOut
063700060912     C                   WRITE     TIVAW000
063800060912     C                   EVAL      wBufferOut = *blanks
063900060912     C                   ENDIF
064000060912     C                   EVAL      wBufferOut = %trim(wBufferOut) +
064100060912     C                                  iCol+%trim(%editc(VAFVLF:'Q'))+fCol+
064200060912     C                                  iCol+%trim(%editc(VAFQFT:'Q'))+fCol+
064300060912     C                                  iCol+%trim(%editc(VAFPOR:'Q'))+fCol+
064400060912     C                                  iCol+   %trim(VAFSV1)         +fCol+
064500060912     C                                  iCol+%trim(%editc(VAFVA1:'Q'))+fCol+
064600060912     C                                  iCol+   %trim(VAFSV2)         +fCol+
064700060912     C                                  iCol+%trim(%editc(VAFVA2:'Q'))+fCol+
064800060912     C                                  iCol+   %trim(VAFSV3)         +fCol+
064900060912     C                                  iCol+%trim(%editc(VAFVA3:'Q'))+fCol+
065000060912     C                                  iCol+   %trim(VAFSV4)         +fCol
065100060912     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
065200060912     C                   IF        %len(%trim(wBufferOut)) >= 700
065300060912     C                   EVAL      VAWDTA = wBufferOut
065400060912     C                   WRITE     TIVAW000
065500060912     C                   EVAL      wBufferOut = *blanks
065600060912     C                   ENDIF
065700060912     C                   EVAL      wBufferOut = %trim(wBufferOut) +
065800060912     C                                  iCol+%trim(%editc(VAFVA4:'Q'))+fCol+
065900060912     C                                  iCol+   %trim(VAFSV5)         +fCol+
066000060912     C                                  iCol+%trim(%editc(VAFVA5:'Q'))+fCol+
066100060912     C                                  iCol+   %trim(VAFSV6)         +fCol+
066200060912     C                                  iCol+%trim(%editc(VAFVA6:'Q'))+fCol+
066300060912     C                                  iCol+   %trim(VAFSV7)         +fCol+
066400060912     C                                  iCol+%trim(%editc(VAFVA7:'Q'))+fCol+
066500060912     C                                  iCol+   %trim(VAFSV8)         +fCol+
066600060912     C                                  iCol+%trim(%editc(VAFVA8:'Q'))+fCol+
066700060912     C                                  iCol+%trim(%editc(VAFVAX:'Q'))+fCol
066800060912     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
066900060912     C                   IF        %len(%trim(wBufferOut)) >= 700
067000060912     C                   EVAL      VAWDTA = wBufferOut
067100060912     C                   WRITE     TIVAW000
067200060912     C                   EVAL      wBufferOut = *blanks
067300060912     C                   ENDIF
067400060912     C                   EVAL      wBufferOut = %trim(wBufferOut) +
067500060912     C                                  iCol+%trim(%editc(VAFIMV:'Q'))+fCol+
067600060912     C                                  iCol+%trim(%editc(VAFDFT:'Q'))+fCol+
067700060912     C                                  iCol+%trim(%editc(VAFNFT:'Q'))+fCol+
067800060912     C                                  iCol+%trim(%editc(VAFFIV:'Q'))+fCol+
067900060912     C                                  iCol+   %trim(VAFDIV)         +fCol+
068000060912     C                                  iCol+%trim(%editc(VAFDRT:'Q'))+fCol+
068100060912     C                                  iCol+%trim(%editc(VAFRMN:'Q'))+fCol+
068200060912     C                                  iCol+   %trim(VAFRMA)         +fCol+
068300060912     C                                  iCol+   %trim(VAFRMX)         +fCol+
068400060912     C                                  iCol+   %trim(VAFFTC)         +fCol
068500060912     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
068600060912     C                   IF        %len(%trim(wBufferOut)) >= 700
068700060912     C                   EVAL      VAWDTA = wBufferOut
068800060912     C                   WRITE     TIVAW000
068900060912     C                   EVAL      wBufferOut = *blanks
069000060912     C                   ENDIF
069100060912     C                   EVAL      wBufferOut = %trim(wBufferOut) +
069200060912     C                                  iCol+   %trim(VAFTC2)         +fCol+
069300060912     C                                  iCol+   %trim(VAFTSP)         +fCol+
069400060912     C                                  iCol+   %trim(VAFFAP)         +fCol+
069500060912     C                                  iCol+   %trim(VAFFIN)         +fCol+
069600060912     C                                  iCol+%trim(%editc(VAFCTR:'Q'))+fCol+
069700060912     C                                  iCol+   %trim(VAFNAS)         +fCol+
069800060912     C                                  iCol+   %trim(VAFCTS)         +fCol+
069900060912     C                                  iCol+   %trim(VAFCAD)         +fCol+
070000060912     C                                  iCol+   %trim(VAFLOD)         +fCol+
070100060912     C                                  iCol+   %trim(VAFPRD)         +fCol
070200060912     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
070300060912     C                   IF        %len(%trim(wBufferOut)) >= 700
070400060912     C                   EVAL      VAWDTA = wBufferOut
070500060912     C                   WRITE     TIVAW000
070600060912     C                   EVAL      wBufferOut = *blanks
070700060912     C                   ENDIF
070800060912     C                   EVAL      wBufferOut = %trim(wBufferOut) +
070900060912     C                                  iCol+   %trim(VAFNZD)         +fCol+
071000060912     C                                  iCol+   %trim(VAFVAS)         +fCol+
071100060912     C                                  iCol+%trim(%editc(VAFIAS:'Q'))+fCol+
071200060912     C                                  iCol+   %trim(VAFFPC)         +fCol+
071300060912     C                                  iCol+%trim(%editc(VAFPKC:'Q'))+fCol+
071400060912     C                                  iCol+   %trim(VAFFVC)         +fCol+
071500060912     C                                  iCol+%trim(%editc(VAFVLC:'Q'))+fCol+
071600060912     C                                  iCol+   %trim(VHFRSD)         +fCol+
071700060912     C                                  iCol+   %trim(VHFIND)         +fCol+
071800060912     C                                  iCol+   %trim(VHFRSM)         +fCol
071900060912     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
072000060912     C                   IF        %len(%trim(wBufferOut)) >= 700
072100060912     C                   EVAL      VAWDTA = wBufferOut
072200060912     C                   WRITE     TIVAW000
072300060912     C                   EVAL      wBufferOut = *blanks
072400060912     C                   ENDIF
072500060912     C                   EVAL      wBufferOut = %trim(wBufferOut) +
072600060912     C                                  iCol+   %trim(VHFINM)         +fCol+
072700060912     C                                  iCol+   %trim(VHFCAM)         +fCol+
072800060912     C                                  iCol+   %trim(VHFLOM)         +fCol+
072900060912     C                                  iCol+   %trim(VHFPRM)         +fCol+
073000060912     C                                  iCol+   %trim(VHFNZM)         +fCol+
073100060912     C                                  iCol+   %trim(VHFRMO)         +fCol+
073200060912     C                                  iCol+   %trim(VHFCMO)         +fCol+
073300060912     C                                  iCol+   %trim(VHFNMO)         +fCol+
073400060912     C                                  iCol+   %trim(wGIAC)          +fCol+
073500060912     C                                  iCol+   %trim(wPIAN)          +fCol
073600060912     C* Ogni 10 campi verifico l'occupazione corrente del buffer, se >= 700 byte vado a capo
073700060912     C                   IF        %len(%trim(wBufferOut)) >= 700
073800060912     C                   EVAL      VAWDTA = wBufferOut
073900060912     C                   WRITE     TIVAW000
074000060912     C                   EVAL      wBufferOut = *blanks
074100060912     C                   ENDIF
074200060912     C                   EVAL      wBufferOut = %trim(wBufferOut) +
074300060912     C                                  iCol+   %trim(wAPPU)          +fCol+
074400060912     C                                  iCol+   %trim(wLDIS)          +fCol+
074500060912     C                                  iCol+   %trim(wISOL)          +fCol+
074600060912     C                                  iCol+   %trim(wCDIS)          +fCol+
074700060912     C                                  iCol+   %trim(wPODI)          +fCol+
074800060912     C                                  iCol+   %trim(wDIRO)          +fCol+
074900060912     C                                  iCol+%trim(%editc(CSBCAS:'Q'))+fCol+
075000060912     C                                  iCol+   %trim(CSBVCA)         +fCol+
075100060912     C                                  iCol+   %trim(wRICO)          +fCol+
075200060912     C                                  iCol+   %trim(wZONF)          +fCol
075300060912     C                   EVAL      VAWDTA = wBufferOut
075400060912     C                   WRITE     TIVAW000
075500060912     C*
075600060912     C                   ENDSR
075700060419
075800060419
075900060419
076000060419     C     chkrec        BEGSR
076100060419     C*
076200060419     C* Considero solo le bolle del periodo richiesto
076300060419     C                   IF        wRecOK = 'S'
076400060420     C                   IF        tasDFT > wDataFIN
076500060420     C                   MOVEL     'N'           wRecOK
076600060420     C                   MOVEL     'S'           wLeave
076700060419     C                   ENDIF
076800060419     C                   ENDIF
076900060719     C*
077000060911     C* Nn considero mai le bolle in conto/servizio (F7 e A7)
077100060911     C                   IF        wRecOK = 'S'
077200060911     C                   IF        tasTBL = 'F7' OR
077300060911     C                             tasTBL = 'A7'
077400060911     C                   MOVEL     'N'           wRecOK
077500060911     C                   ENDIF
077600060911     C                   ENDIF
077700060912     C*
077800060912     C* Al 2° giro nn considero mai le bolle di recupero (A3 e F3)
077900060912     C                   IF        wGiro = 2
078000060912     C                   IF        wRecOK = 'S'
078100060912     C                   IF        tasTBL = 'F3' OR
078200060912     C                             tasTBL = 'A3'
078300060912     C                   MOVEL     'N'           wRecOK
078400060912     C                   ENDIF
078500060912     C                   ENDIF
078600060912     C                   ENDIF
078700060911     C*
078800060911     C* Esclude le bolle nn valide come "SPEDIZIONE"
078900060911     C                   if        wRecOk = 'S'
079000060911     C                   z-add     1             j
079100060911     C     tasTBL        lookup    TB(j)                                  28
079200060911     C                   if        %found and RBL(j) = 'N'
079300060911     C                   else
079400060911     C                   movel     'N'           wRecOkSped
079500060911     C                   eval      dRMN   = tasRMN
079600060912     C                   eval      dAAS   = tasAAS
079700060912     C                   eval      dLNP   = tasLNP
079800060912     C                   eval      dNRS   = tasNRS
079900060912     C                   eval      dNSP   = tasNSP
080000060912     C                   eval      dTBL   = tasTBL
080100060911     C                   exsr      repVARIE
080200060911     C                   eval      dPOR   = UL86OPOR
080300060911     C                   eval      dVARIE = skVARIE
080400060911     C                   eval      dIMV   = UL86OIMV
080500060911     C                   add       1             yVARIE
080600060911     C                   eval      skVARIE_RMN(yVARIE) = dsVARIA_RMN
080700060911     C                   endif
080800060911     C                   endif
080900060420     C*
081000060419     C* Verifica le bolle fatturate inizio mese richiesto, occorre considerare solo quelle aventi
081100060419     C* P.O. IVA uguale a zero o uguale ai valori indicati in tabella QT-2.
081200060420     C***                IF        wRecOK = 'S'
081300060420     C***                IF        tasDFT = wDataINZ
081400060420     C***                IF        tasFIV = §QCFIV  OR
081500060420     C***                          tasFIV = §QCFI2  OR
081600060420     C***                          tasFIV = *zeros
081700060420     C***                ELSE
081800060420     C***                MOVEL     'N'           wRecOK
081900060420     C***                ENDIF
082000060420     C***                ENDIF
082100060420     C***                ENDIF
082200060420     C*
082300060419     C* Verifica le bolle fatturate inizio mese successivo, occorre considerare solo quelle aventi
082400060419     C* P.O. IVA diverso da zero e diverso valori indicati in tabella QT-2.
082500060419     C                   IF        wRecOK = 'S'
082600060420     C                   IF        tasDFT = wDataFIN AND
082700060419     C                             tasFIV <> §QCFIV  AND
082800060419     C                             tasFIV <> §QCFI2  AND
082900060420     C                             tasFIV <> *zeros
083000060420     C                   MOVEL     'N'           wRecOK
083100060419     C                   ENDIF
083200060419     C                   ENDIF
083300060419     C*
083400060419     C                   ENDSR
083500060420
083600060420
083700060420
083800060420     C     TAStoVAF      BEGSR
083900060420     C*
084000060420     C* Eseguo passaggio dati da formato "TAS" a formato "VAF"
084100060420     C                   EVAL      VAFAAS = tasAAS
084200060420     C                   EVAL      VAFLNP = tasLNP
084300060420     C                   EVAL      VAFNRS = tasNRS
084400060420     C                   EVAL      VAFNSP = tasNSP
084500060420     C                   EVAL      VAFTBL = tasTBL
084600060420     C                   EVAL      VAFMGS = tasMGS
084700060420     C                   EVAL      VAFKSC = tasKSC
084800060420     C                   EVAL      VAFLNA = tasLNA
084900060420     C                   EVAL      VAFPKB = tasPKF
085000060420     C                   EVAL      VAFVLF = tasVLF
085100060420     C                   EVAL      VAFQFT = tasQFT
085200060912     C* Solo se giro nn è il 3°
085300060912     C                   IF        wGiro <> 3
085400060912     C                   EVAL      VAFPOR = VAFPOR + tasPOR
085500060912     C                   EVAL      VAFIMV = VAFIMV + tasIMV
085600060912     C                   ENDIF
085700060912     C*
085800060420     C                   EVAL      VAFDFT = tasDFT
085900060420     C                   EVAL      VAFNFT = tasNFT
086000060420     C                   EVAL      VAFFIV = tasFIV
086100060420     C                   EVAL      VAFDIV = tasDIV
086200060420     C                   EVAL      VAFDRT = tasDRT
086300060420     C                   EVAL      VAFRMN = tasRMN
086400060420     C****** NON GESTISCO VARIE PARTICOLARITA DEL RIFERIMENTO MITTENTE ALFA *******
086500060420     C                   EVAL      VAFRMA = ta4NOT
086600060420     C*****************************************************************************
086700060420     C                   EVAL      VAFFTC = tasFTC
086800060420     C                   EVAL      VAFTC2 = tasTC2
086900060420     C                   EVAL      VAFTSP = tasTSP
087000060420     C                   EVAL      VAFFAP = tasFAP
087100060420     C                   EVAL      VAFFIN = tasFIN
087200060420     C                   EVAL      VAFCTR = tasCTR
087300060420     C                   EVAL      VAFCTS = tasCTS
087400060420     C                   EVAL      VAFCAD = tasCAD
087500060420     C                   EVAL      VAFLOD = tasLOD
087600060420     C                   EVAL      VAFPRD = tasPRD
087700060420     C                   EVAL      VAFNZD = tasNZD
087800060420     C                   EVAL      VAFVAS = tasVAS
087900060420     C                   EVAL      VAFIAS = tasIAS
088000060420     C*
088100060420     C* Gestisco particolari considerazioni
088200060420     C                   IF        tasFPF = 'V'
088300150716     C*
088400150716     C                   IF        TASDFT <= §QTDST
088500150716     C     §QTTCO        MULT      TASNCP        WTARA
088600150716     C     TASPKC        SUB(h)    WTARA         WNTARA
088700150716     C                   Z-ADD     WNTARA        VAFPKC
088800150716     C                   ELSE
088900150716     C     §QTTPC        MULT      TASNCP        WTARA
089000150716     C     TASPKC        SUB(h)    WTARA         WNTARA
089100150716     C                   Z-ADD     WNTARA        VAFPKC
089200150716     C                   ENDIF
089300150716     C*
089400060420     C                   ELSE
089500060420     C                   Z-ADD     *zeros        VAFPKC
089600060420     C                   ENDIF
089700060420     C*
089800060420     C                   ENDSR
089900060420
090000060420
090100060420
090200060420     C     repVARIE      BEGSR
090300060420     C*
090400060420     C* Inizializzo la DS d procedura esterna
090500060420     C                   CLEAR                   TRUL86DS
090600060420     C                   EVAL      UL86ITLA = 'E'
090700060420     C                   EVAL      UL86IAAS = tasAAS
090800060420     C                   EVAL      UL86ILNP = tasLNP
090900060420     C                   EVAL      UL86INRS = tasNRS
091000060420     C                   EVAL      UL86INSP = tasNSP
091100060420     C                   EVAL      UL86ITBL = tasTBL
091200060420     C                   CALL      'TRUL86R'
091300060420     C                   PARM                    TRUL86DS
091400060420     C*
091500060420     C                   MOVEA(P)  UL86OOUT      skVARIE
091600060420     C*
091700060420     C                   ENDSR
091800060911
091900060911
092000060911
092100060911     C     sumVARIE      BEGSR
092200060911     C*
092300060911     C* Reperisco le varie della bolla
092400060911     C                   EXSR      repVARIE
092500060911     C*
092600060911     C* Valorizzo VARIA 1
092700060911     C                   EVAL      jVARIE = 1
092800060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
092900060911     C                   EVAL      VAFSV1 = dVARIA_COD
093000060911     C                   EVAL      VAFVA1 = dVARIA_VAL
093100060911     C*
093200060911     C* Valorizzo VARIA 2
093300060911     C                   EVAL      jVARIE = 2
093400060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
093500060911     C                   EVAL      VAFSV2 = dVARIA_COD
093600060911     C                   EVAL      VAFVA2 = dVARIA_VAL
093700060911     C*
093800060911     C* Valorizzo VARIA 3
093900060911     C                   EVAL      jVARIE = 3
094000060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
094100060911     C                   EVAL      VAFSV3 = dVARIA_COD
094200060911     C                   EVAL      VAFVA3 = dVARIA_VAL
094300060911     C*
094400060911     C* Valorizzo VARIA 4
094500060911     C                   EVAL      jVARIE = 4
094600060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
094700060911     C                   EVAL      VAFSV4 = dVARIA_COD
094800060911     C                   EVAL      VAFVA4 = dVARIA_VAL
094900060911     C*
095000060911     C* Valorizzo VARIA 5
095100060911     C                   EVAL      jVARIE = 5
095200060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
095300060911     C                   EVAL      VAFSV5 = dVARIA_COD
095400060911     C                   EVAL      VAFVA5 = dVARIA_VAL
095500060911     C*
095600060911     C* Valorizzo VARIA 6
095700060911     C                   EVAL      jVARIE = 6
095800060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
095900060911     C                   EVAL      VAFSV6 = dVARIA_COD
096000060911     C                   EVAL      VAFVA6 = dVARIA_VAL
096100060911     C*
096200060911     C* Valorizzo VARIA 7
096300060911     C                   EVAL      jVARIE = 7
096400060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
096500060911     C                   EVAL      VAFSV7 = dVARIA_COD
096600060911     C                   EVAL      VAFVA7 = dVARIA_VAL
096700060911     C*
096800060911     C* Valorizzo VARIA 8
096900060911     C                   EVAL      jVARIE = 8
097000060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
097100060911     C                   EVAL      VAFSV8 = dVARIA_COD
097200060911     C                   EVAL      VAFVA8 = dVARIA_VAL
097300060911     C*
097400060911     C* Totalizzo tutte le restanti varie
097500060911     C                   DOW       jVARIE <= %elem(skVARIE)
097600060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
097700060911     C                   EVAL      VAFVAX = VAFVAX + dVARIA_VAL
097800060911     C                   ADD       1             jVARIE
097900060911     C                   ENDDO
098000060911     C*
098100060911     C                   ENDSR
098200060418
098300060911
098400060911
098500060911
098600060911     C     repVARIE_RMN  BEGSR
098700060911     C*
098800060911     C* Innanzitutto ordino la schiera
098900060911     C                   SORTA     skVARIE_RMN
099000060911     C*
099100060911     C* Scorro tutta la schiera dei depositi varie x RMN
099200060911     C                   z-add     1             yVARIE
099300060911     C                   dow       yVARIE <= %elem(skVARIE_RMN)
099400060912     C                   if        skVARIE_RMN(yVARIE) = *blanks
099500060912     C                   leave
099600060912     C                   else
099700060911     C                   eval      dsVARIA_RMN = skVARIE_RMN(yVARIE)
099800060912     C                   if        dRMN = *zeros
099900060912     C                   leave
100000060912     C                   else
100100060911     C                   if        dRMN = tasRMN
100200060912     C*
100300060912     C* Aggiungo subito il porto e l'imponibile
100400060912     C                   EVAL      VAFPOR = VAFPOR + dPOR
100500060912     C                   EVAL      VAFIMV = VAFIMV + dIMV
100600060911     C*
100700060911     C* Analizzo le varie della bolla con medesimo RMN
100800060911     C                   eval      skVARIE = dVARIE
100900060911     C*
101000060911     C* Scorro tutta la schiera delle varie reperite x la bolla corrente
101100060911     C                   EVAL      jVARIE = 1
101200060911     C                   DOW       jVARIE <= %elem(skVARIE)
101300060912     C                   MOVEL     'N'           wVariaVAF         1
101400060911     C                   EVAL      dsVARIA = skVARIE(jVARIE)
101500060911     C*
101600060911     C                   IF        wVariaVAF = 'N' AND
101700060911     C                             dVARIA_COD <> *blanks AND
101800060911     C                             dVARIA_COD = VAFSV1
101900060911     C                   EVAL      VAFVA1 = VAFVA1 + dVARIA_VAL
102000060911     C                   MOVEL     'S'           wVariaVAF
102100060911     C                   ENDIF
102200060911     C*
102300060911     C                   IF        wVariaVAF = 'N' AND
102400060911     C                             dVARIA_COD <> *blanks AND
102500060911     C                             dVARIA_COD = VAFSV2
102600060911     C                   EVAL      VAFVA2 = VAFVA2 + dVARIA_VAL
102700060911     C                   MOVEL     'S'           wVariaVAF
102800060911     C                   ENDIF
102900060911     C*
103000060911     C                   IF        wVariaVAF = 'N' AND
103100060911     C                             dVARIA_COD <> *blanks AND
103200060911     C                             dVARIA_COD = VAFSV3
103300060911     C                   EVAL      VAFVA3 = VAFVA3 + dVARIA_VAL
103400060911     C                   MOVEL     'S'           wVariaVAF
103500060911     C                   ENDIF
103600060911     C*
103700060911     C                   IF        wVariaVAF = 'N' AND
103800060911     C                             dVARIA_COD <> *blanks AND
103900060911     C                             dVARIA_COD = VAFSV4
104000060911     C                   EVAL      VAFVA4 = VAFVA4 + dVARIA_VAL
104100060911     C                   MOVEL     'S'           wVariaVAF
104200060911     C                   ENDIF
104300060911     C*
104400060911     C                   IF        wVariaVAF = 'N' AND
104500060911     C                             dVARIA_COD <> *blanks AND
104600060911     C                             dVARIA_COD = VAFSV5
104700060911     C                   EVAL      VAFVA5 = VAFVA5 + dVARIA_VAL
104800060911     C                   MOVEL     'S'           wVariaVAF
104900060911     C                   ENDIF
105000060911     C*
105100060911     C                   IF        wVariaVAF = 'N' AND
105200060911     C                             dVARIA_COD <> *blanks AND
105300060911     C                             dVARIA_COD = VAFSV6
105400060911     C                   EVAL      VAFVA6 = VAFVA6 + dVARIA_VAL
105500060911     C                   MOVEL     'S'           wVariaVAF
105600060911     C                   ENDIF
105700060911     C*
105800060911     C                   IF        wVariaVAF = 'N' AND
105900060911     C                             dVARIA_COD <> *blanks AND
106000060911     C                             dVARIA_COD = VAFSV7
106100060911     C                   EVAL      VAFVA7 = VAFVA7 + dVARIA_VAL
106200060911     C                   MOVEL     'S'           wVariaVAF
106300060911     C                   ENDIF
106400060911     C*
106500060911     C                   IF        wVariaVAF = 'N' AND
106600060911     C                             dVARIA_COD <> *blanks AND
106700060911     C                             dVARIA_COD = VAFSV8
106800060911     C                   EVAL      VAFVA8 = VAFVA8 + dVARIA_VAL
106900060911     C                   MOVEL     'S'           wVariaVAF
107000060911     C                   ENDIF
107100060911     C*
107200060911     C* Se varia già attribuita bene, altrimenti cerco 1 "buco varia" nn già utilizzato
107300060911     C                   IF        wVariaVAF <> 'S'
107400060911     C*
107500060911     C                   IF        wVariaVAF = 'N' AND
107600060911     C                             VAFSV1 = *blanks AND dVARIA_COD <> *blanks
107700060911     C                   EVAL      VAFSV1 = dVARIA_COD
107800060911     C                   EVAL      VAFVA1 = dVARIA_VAL
107900060911     C                   MOVEL     'S'           wVariaVAF
108000060911     C                   ENDIF
108100060911     C*
108200060911     C                   IF        wVariaVAF = 'N' AND
108300060911     C                             VAFSV2 = *blanks AND dVARIA_COD <> *blanks
108400060911     C                   EVAL      VAFSV2 = dVARIA_COD
108500060911     C                   EVAL      VAFVA2 = dVARIA_VAL
108600060911     C                   MOVEL     'S'           wVariaVAF
108700060911     C                   ENDIF
108800060911     C*
108900060911     C                   IF        wVariaVAF = 'N' AND
109000060911     C                             VAFSV3 = *blanks AND dVARIA_COD <> *blanks
109100060911     C                   EVAL      VAFSV3 = dVARIA_COD
109200060911     C                   EVAL      VAFVA3 = dVARIA_VAL
109300060911     C                   MOVEL     'S'           wVariaVAF
109400060911     C                   ENDIF
109500060911     C*
109600060911     C                   IF        wVariaVAF = 'N' AND
109700060911     C                             VAFSV4 = *blanks AND dVARIA_COD <> *blanks
109800060911     C                   EVAL      VAFSV4 = dVARIA_COD
109900060911     C                   EVAL      VAFVA4 = dVARIA_VAL
110000060911     C                   MOVEL     'S'           wVariaVAF
110100060911     C                   ENDIF
110200060911     C*
110300060911     C                   IF        wVariaVAF = 'N' AND
110400060911     C                             VAFSV5 = *blanks AND dVARIA_COD <> *blanks
110500060911     C                   EVAL      VAFSV5 = dVARIA_COD
110600060911     C                   EVAL      VAFVA5 = dVARIA_VAL
110700060911     C                   MOVEL     'S'           wVariaVAF
110800060911     C                   ENDIF
110900060911     C*
111000060911     C                   IF        wVariaVAF = 'N' AND
111100060911     C                             VAFSV6 = *blanks AND dVARIA_COD <> *blanks
111200060911     C                   EVAL      VAFSV6 = dVARIA_COD
111300060911     C                   EVAL      VAFVA6 = dVARIA_VAL
111400060911     C                   MOVEL     'S'           wVariaVAF
111500060911     C                   ENDIF
111600060911     C*
111700060911     C                   IF        wVariaVAF = 'N' AND
111800060911     C                             VAFSV7 = *blanks AND dVARIA_COD <> *blanks
111900060911     C                   EVAL      VAFSV7 = dVARIA_COD
112000060911     C                   EVAL      VAFVA7 = dVARIA_VAL
112100060911     C                   MOVEL     'S'           wVariaVAF
112200060911     C                   ENDIF
112300060911     C*
112400060911     C                   IF        wVariaVAF = 'N' AND
112500060911     C                             VAFSV8 = *blanks AND dVARIA_COD <> *blanks
112600060911     C                   EVAL      VAFSV8 = dVARIA_COD
112700060911     C                   EVAL      VAFVA8 = dVARIA_VAL
112800060911     C                   MOVEL     'S'           wVariaVAF
112900060911     C                   ENDIF
113000060911     C*
113100060911     C* Se varia già attribuita bene, altrimenti la totalizzo nelle "varie cumulative generiche"
113200060911     C                   IF        wVariaVAF <> 'S'
113300060911     C                   EVAL      VAFVAX = VAFVAX + dVARIA_VAL
113400060911     C                   ENDIF
113500060911     C*
113600060911     C                   ENDIF
113700060911     C*
113800060911     C                   ADD       1             jVARIE
113900060911     C                   ENDDO
114000060911     C*
114100060912     C                   clear                   dRMN
114200060912     C                   eval      skVARIE_RMN(yVARIE) = dsVARIA_RMN
114300060912     C*
114400060912     C                   endif
114500060911     C                   endif
114600060911     C                   endif
114700060911     C                   add       1             yVARIE
114800060911     C                   enddo
114900060911     C*
115000060911     C                   ENDSR
115100060912
115200060912
115300060912
115400060912
115500060912     C     repBOLLE_RMN  BEGSR
115600060912     C*
115700060912     C* Innanzitutto ordino la schiera
115800060912     C                   SORTA     skVARIE_RMN
115900060912     C*
116000060912     C* Scorro tutta la schiera dei depositi varie x RMN
116100060912     C                   z-add     1             yVARIE
116200060912     C                   dow       yVARIE <= %elem(skVARIE_RMN)
116300060912     C                   if        skVARIE_RMN(yVARIE) = *blanks
116400060912     C                   leave
116500060912     C                   else
116600060912     C                   eval      dsVARIA_RMN = skVARIE_RMN(yVARIE)
116700060912     C                   if        dRMN = *zeros
116800060912     C                   leave
116900060912     C                   else
117000060912     C*
117100060912     C* Inizializzo il buffer del file d output e delle ds d ridefinizione dati
117200060912     C                   CLEAR                   TIVAW000
117300060912     C                   CLEAR                   DSVAF
117400060912     C                   CLEAR                   DSVAFA
117500060912     C*
117600060912     C* Aggancio la bolla relativa x il completamento dei dati
117700060912     C     KEYtas30      chain     titas30c
117800060912     C                   if        %found(titas30c)
117900060912     C*
118000060912     C* Aggiungo subito il porto e l'imponibile
118100060912     C                   EVAL      VAFPOR = VAFPOR + dPOR
118200060912     C                   EVAL      VAFIMV = VAFIMV + dIMV
118300060912     C*
118400060912     C* Analizzo le varie della bolla con medesimo RMN
118500060912     C                   eval      skVARIE = dVARIE
118600060912     C*
118700060912     C* Scorro tutta la schiera delle varie reperite x la bolla corrente
118800060912     C                   EVAL      jVARIE = 1
118900060912     C                   DOW       jVARIE <= %elem(skVARIE)
119000060912     C                   MOVEL     'N'           wVariaVAF         1
119100060912     C                   EVAL      dsVARIA = skVARIE(jVARIE)
119200060912     C*
119300060912     C                   IF        wVariaVAF = 'N' AND
119400060912     C                             dVARIA_COD <> *blanks AND
119500060912     C                             dVARIA_COD = VAFSV1
119600060912     C                   EVAL      VAFVA1 = VAFVA1 + dVARIA_VAL
119700060912     C                   MOVEL     'S'           wVariaVAF
119800060912     C                   ENDIF
119900060912     C*
120000060912     C                   IF        wVariaVAF = 'N' AND
120100060912     C                             dVARIA_COD <> *blanks AND
120200060912     C                             dVARIA_COD = VAFSV2
120300060912     C                   EVAL      VAFVA2 = VAFVA2 + dVARIA_VAL
120400060912     C                   MOVEL     'S'           wVariaVAF
120500060912     C                   ENDIF
120600060912     C*
120700060912     C                   IF        wVariaVAF = 'N' AND
120800060912     C                             dVARIA_COD <> *blanks AND
120900060912     C                             dVARIA_COD = VAFSV3
121000060912     C                   EVAL      VAFVA3 = VAFVA3 + dVARIA_VAL
121100060912     C                   MOVEL     'S'           wVariaVAF
121200060912     C                   ENDIF
121300060912     C*
121400060912     C                   IF        wVariaVAF = 'N' AND
121500060912     C                             dVARIA_COD <> *blanks AND
121600060912     C                             dVARIA_COD = VAFSV4
121700060912     C                   EVAL      VAFVA4 = VAFVA4 + dVARIA_VAL
121800060912     C                   MOVEL     'S'           wVariaVAF
121900060912     C                   ENDIF
122000060912     C*
122100060912     C                   IF        wVariaVAF = 'N' AND
122200060912     C                             dVARIA_COD <> *blanks AND
122300060912     C                             dVARIA_COD = VAFSV5
122400060912     C                   EVAL      VAFVA5 = VAFVA5 + dVARIA_VAL
122500060912     C                   MOVEL     'S'           wVariaVAF
122600060912     C                   ENDIF
122700060912     C*
122800060912     C                   IF        wVariaVAF = 'N' AND
122900060912     C                             dVARIA_COD <> *blanks AND
123000060912     C                             dVARIA_COD = VAFSV6
123100060912     C                   EVAL      VAFVA6 = VAFVA6 + dVARIA_VAL
123200060912     C                   MOVEL     'S'           wVariaVAF
123300060912     C                   ENDIF
123400060912     C*
123500060912     C                   IF        wVariaVAF = 'N' AND
123600060912     C                             dVARIA_COD <> *blanks AND
123700060912     C                             dVARIA_COD = VAFSV7
123800060912     C                   EVAL      VAFVA7 = VAFVA7 + dVARIA_VAL
123900060912     C                   MOVEL     'S'           wVariaVAF
124000060912     C                   ENDIF
124100060912     C*
124200060912     C                   IF        wVariaVAF = 'N' AND
124300060912     C                             dVARIA_COD <> *blanks AND
124400060912     C                             dVARIA_COD = VAFSV8
124500060912     C                   EVAL      VAFVA8 = VAFVA8 + dVARIA_VAL
124600060912     C                   MOVEL     'S'           wVariaVAF
124700060912     C                   ENDIF
124800060912     C*
124900060912     C* Se varia già attribuita bene, altrimenti cerco 1 "buco varia" nn già utilizzato
125000060912     C                   IF        wVariaVAF <> 'S'
125100060912     C*
125200060912     C                   IF        wVariaVAF = 'N' AND
125300060912     C                             VAFSV1 = *blanks AND dVARIA_COD <> *blanks
125400060912     C                   EVAL      VAFSV1 = dVARIA_COD
125500060912     C                   EVAL      VAFVA1 = dVARIA_VAL
125600060912     C                   MOVEL     'S'           wVariaVAF
125700060912     C                   ENDIF
125800060912     C*
125900060912     C                   IF        wVariaVAF = 'N' AND
126000060912     C                             VAFSV2 = *blanks AND dVARIA_COD <> *blanks
126100060912     C                   EVAL      VAFSV2 = dVARIA_COD
126200060912     C                   EVAL      VAFVA2 = dVARIA_VAL
126300060912     C                   MOVEL     'S'           wVariaVAF
126400060912     C                   ENDIF
126500060912     C*
126600060912     C                   IF        wVariaVAF = 'N' AND
126700060912     C                             VAFSV3 = *blanks AND dVARIA_COD <> *blanks
126800060912     C                   EVAL      VAFSV3 = dVARIA_COD
126900060912     C                   EVAL      VAFVA3 = dVARIA_VAL
127000060912     C                   MOVEL     'S'           wVariaVAF
127100060912     C                   ENDIF
127200060912     C*
127300060912     C                   IF        wVariaVAF = 'N' AND
127400060912     C                             VAFSV4 = *blanks AND dVARIA_COD <> *blanks
127500060912     C                   EVAL      VAFSV4 = dVARIA_COD
127600060912     C                   EVAL      VAFVA4 = dVARIA_VAL
127700060912     C                   MOVEL     'S'           wVariaVAF
127800060912     C                   ENDIF
127900060912     C*
128000060912     C                   IF        wVariaVAF = 'N' AND
128100060912     C                             VAFSV5 = *blanks AND dVARIA_COD <> *blanks
128200060912     C                   EVAL      VAFSV5 = dVARIA_COD
128300060912     C                   EVAL      VAFVA5 = dVARIA_VAL
128400060912     C                   MOVEL     'S'           wVariaVAF
128500060912     C                   ENDIF
128600060912     C*
128700060912     C                   IF        wVariaVAF = 'N' AND
128800060912     C                             VAFSV6 = *blanks AND dVARIA_COD <> *blanks
128900060912     C                   EVAL      VAFSV6 = dVARIA_COD
129000060912     C                   EVAL      VAFVA6 = dVARIA_VAL
129100060912     C                   MOVEL     'S'           wVariaVAF
129200060912     C                   ENDIF
129300060912     C*
129400060912     C                   IF        wVariaVAF = 'N' AND
129500060912     C                             VAFSV7 = *blanks AND dVARIA_COD <> *blanks
129600060912     C                   EVAL      VAFSV7 = dVARIA_COD
129700060912     C                   EVAL      VAFVA7 = dVARIA_VAL
129800060912     C                   MOVEL     'S'           wVariaVAF
129900060912     C                   ENDIF
130000060912     C*
130100060912     C                   IF        wVariaVAF = 'N' AND
130200060912     C                             VAFSV8 = *blanks AND dVARIA_COD <> *blanks
130300060912     C                   EVAL      VAFSV8 = dVARIA_COD
130400060912     C                   EVAL      VAFVA8 = dVARIA_VAL
130500060912     C                   MOVEL     'S'           wVariaVAF
130600060912     C                   ENDIF
130700060912     C*
130800060912     C* Se varia già attribuita bene, altrimenti la totalizzo nelle "varie cumulative generiche"
130900060912     C                   IF        wVariaVAF <> 'S'
131000060912     C                   EVAL      VAFVAX = VAFVAX + dVARIA_VAL
131100060912     C                   ENDIF
131200060912     C*
131300060912     C                   ENDIF
131400060912     C*
131500060912     C                   ADD       1             jVARIE
131600060912     C                   ENDDO
131700060912     C*
131800060912     C* Eseguo quindi la routine d formazione del dato e scrittura dello stesso
131900060912     C                   EXSR      wriFile
132000060912     C*
132100060912     C                   clear                   dRMN
132200060912     C                   eval      skVARIE_RMN(yVARIE) = dsVARIA_RMN
132300060912     C*
132400060912     C                   endif
132500060912     C                   endif
132600060912     C                   endif
132700060912     C                   add       1             yVARIE
132800060912     C                   enddo
132900060912     C*
133000060912     C                   ENDSR
133100060418
133200060419
133300060912
133400060912
133500060418     C     cartab        BEGSR
133600060418     C*
133700060419     C* Reperisco tutti i codici clienti che x la fatturazione sono unificati sul cliente richiesto
133800060418     C                   Z-ADD     *zeros        jVAF
133900060420     C                   CLEAR                   DS4C
134000060418     C                   EVAL      tblKUT = 1
134100060418     C                   EVAL      tblCOD = '4C'
134200060419     C                   EVAL      tblKEY = *blanks
134300060418     C     KEYtabel_P    SETLL     TABEL00F
134400060418     C                   IF        %found(TABEL00F)
134500060418     C     KEYtabel_P    READE     TABEL00F
134600060418     C                   DOW       not %eof(TABEL00F)
134700060418     C                   IF        tblFLG = *blanks
134800060418     C                   EVAL      DS4C = tblUNI
134900060418     C                   IF        §4CFKS = wKSUVAF
135000060418     C                   ADD       1             jVAF
135100060418     C                   MOVEL(P)  tblKEY        skVAF(jVAF)
135200060418     C                   ENDIF
135300060418     C                   ENDIF
135400060418     C     KEYtabel_P    READE     TABEL00F
135500060418     C                   ENDDO
135600060418     C                   ENDIF
135700060419     C*
135800060419     C* Reperisco i P.O. IVA da considerare x le fatture fine mese clienti codificati
135900060419     C                   CLEAR                   DSQC2
136000060419     C                   Z-ADD     1             tblKUT
136100060419     C                   MOVEL(P)  'QC'          tblCOD
136200060419     C                   MOVEL(P)  '2'           tblKEY
136300060419     C     KEYtabel      CHAIN     TABEL00F
136400060419     C                   IF        %found(TABEL00F)
136500060420     C                   IF        tblFLG = *blanks
136600060419     C                   MOVEL(P)  tblUNI        DSQC2
136700060419     C                   ENDIF
136800060420     C                   ENDIF
136900060420     C*
137000060420     C** Reperisco campi standard tassazione
137100060420     C                   CLEAR                   DSQT1
137200060420     C                   Z-ADD     1             tblKUT
137300060420     C                   MOVEL     'QT'          tblCOD
137400060420     C                   MOVEL     1             tblKEY
137500060420     C     KEYtabel      CHAIN     TABEL00F
137600060420     C                   IF        %found(TABEL00F)
137700060420     C                   IF        tblFLG = *blanks
137800060420     C                   MOVEL     TBLUNI        DSQT1
137900060420     C                   ENDIF
138000060420     C                   ENDIF
138100060911     C*
138200060911     C* Reperisco i tipi bolle e relativi flags spedizione/ricavo/etc...
138300060911     C                   Z-ADD     0             i
138400060911     C                   Z-ADD     1             tblKUT
138500060911     C                   MOVEL     'TB'          tblCOD
138600060911     C                   MOVEL     *BLANKS       tblKEY
138700060911     C     KEYtabel      SETLL     TABEL00F
138800060911     C                   READ      TABEL00F                               98
138900060911     C     *IN98         DOWEQ     *OFF
139000060911     C     tblCOD        ANDEQ     'TB'
139100060911     C     tblFLG        IFEQ      *BLANKS
139200060911     C                   ADD       1             i
139300060911     C                   MOVEL(P)  tblKEY        TB(I)                          *TIPO BOLLA
139400060911     C                   MOVEL(P)  tblUNI        DSTB
139500060911     C                   MOVEL(P)  §TBRBL        RBL(I)
139600060911     C                   ENDIF
139700060911     C                   READ      TABEL00F                               98
139800060911     C                   ENDDO
139900060418     C*
140000060418     C                   ENDSR
140100991027
140200060419
140300060419
140400060419     C     chkPAR        BEGSR
140500060419     C*
140600060419     C* Estrapolo il codice cliente unificante (unificante secondo tabella 4C) d fatturazione
140700060419     C                   EVAL      wKSUVAFa = %subst(prmppt:2:7)
140800060419     C                   MOVE(P)   wKSUVAFa      wKSUVAF
140900060419     C*
141000060419     C* Se presente nei parametri la forzatura periodo fatturazione (anno + mese) considero quello
141100060419     C* altrimenti considero il mese precedente a quello corrente
141200060419     C                   EVAL      wAAAAMMa = %subst(prmppt:9:6)
141300060419     C                   MOVE(P)   wAAAAMMa      wAAAAMM
141400060419     C                   IF        wAAAAMM = *zeros
141500060419     C                   Z-ADD     wAAAAMMcor    wPERIODO
141600060419     C                   EVAL      wMM = wMM - 1                                * mese precedente
141700060419     C                   ELSE
141800060419     C                   Z-ADD     wAAAAMM       wPERIODO
141900060419     C                   ENDIF
142000060419     C*
142100060420     C* Calcolo il primo giorno del mese successivo
142200060419     C                   IF        wMM = 12
142300060420     C***                EVAL      wDataFIN = 0101*10000+wAAAA+1
142400060420     C                   EVAL      wDataFIN = (wAAAA+1)*10000+0101
142500060419     C                   ELSE
142600060420     C***                EVAL      wDataFIN = 01*1000000+(wMM+1)*10000+wAAAA
142700060420     C                   EVAL      wDataFIN = wAAAA*10000+(wMM+1)*100+01
142800060419     C                   ENDIF
142900060420     C*
143000060420     C* Calcolo l'ultimo giorno del mese precedente
143100060420     C     *ISO          MOVE      wDataFIN      wDataISO
143200060420     C     wDataISO      SUBDUR    1:*D          wDataISO
143300060420     C     *ISO          MOVE      wDataISO      wDataINZ
143400060420     C*
143500060420     C* Calcolo il primo giorno del mese precedente
143600060420     C***                IF        wMM = 01
143700060420     C***                EVAL      wDataINZ = 0112*10000+wAAAA-1
143800060420     C***                ELSE
143900060420     C***                EVAL      wDataINZ = 01*1000000+(wMM-1)*10000+wAAAA
144000060420     C***                ENDIF
144100060419     C*
144200060419     C                   ENDSR
144300060419
144400060418
144500991027      /TITLE Operazioni iniziali.
144600991027     C     *inzsr        BEGSR
144700991027     C*
144800991027     C     *ENTRY        PLIST
144900991027     C                   parm                    prmppt
145000991027     C     wrkesito      parm      wrkesito      prmesito
145100060419     C*
145200060419     C* CALCOLA LA DATA CORRENTE
145300060419     C                   time                    wn14             14 0
145400060419     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
145500060419     C                   z-add     wn8           g08dat
145600060419     C                   z-add     *zeros        g08inv
145700060419     C                   movel     '0'           g08err
145800060419     C                   call      'XSRDA8'
145900060419     C                   parm                    wlbda8
146000060419     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
146100060419     C                   movel     datcor        wAAAAMMcor        6 0
146200060413     C*
146300060413     C* Definizione chiavi di lettura
146400060419     C*
146500060419     C* TABEL00F - Completa
146600060419     C     KEYtabel      KLIST
146700060419     C                   KFLD                    tblKUT
146800060419     C                   KFLD                    tblCOD
146900060419     C                   KFLD                    tblKEY
147000060418     C*
147100060418     C* TABEL00F - Parziale
147200060418     C     KEYtabel_P    KLIST
147300060418     C                   KFLD                    tblKUT
147400060418     C                   KFLD                    tblCOD
147500060413     C*
147600060912     C* TITAS31C - Completa
147700060413     C     KEYtas31      KLIST
147800060413     C                   KFLD                    tasKSC
147900060420     C                   KFLD                    tasDFT
148000060912     C*
148100060912     C* TITAS30C - Completa
148200060912     C     KEYtas30      KLIST
148300060912     C                   KFLD                    dAAS
148400060912     C                   KFLD                    dLNP
148500060912     C                   KFLD                    dNRS
148600060912     C                   KFLD                    dNSP
148700060912     C                   KFLD                    dTBL
148800060413     C*
148900060413     C* TNCSB03L - Completa
149000060413     C     KEYcsb03      KLIST
149100060413     C                   KFLD                    tasAAS
149200060413     C                   KFLD                    tasLNP
149300060413     C                   KFLD                    tasNRS
149400060413     C                   KFLD                    tasNSP
149500060418     C*
149600060418     C* TIGCP51L - Parziale
149700060418     C     KEYgcp51_P    KLIST
149800060418     C                   KFLD                    tasAAS
149900060418     C                   KFLD                    tasLNP
150000060418     C                   KFLD                    tasNRS
150100060418     C                   KFLD                    tasNSP
150200060911     C                   KFLD                    gcpFRG
150300060418     C*
150400060418     C* TITA430C - Completa
150500060418     C     KEYta430      KLIST
150600060418     C                   KFLD                    tasAAS
150700060418     C                   KFLD                    tasLNP
150800060418     C                   KFLD                    tasNRS
150900060418     C                   KFLD                    tasNSP
151000060418     C                   KFLD                    ta4TRC
151100060420     C*
151200060420     C* TITAA30C - Completa
151300060420     C     KEYtaa30      KLIST
151400060420     C                   KFLD                    tasAAS
151500060420     C                   KFLD                    tasLNP
151600060420     C                   KFLD                    tasNRS
151700060420     C                   KFLD                    tasNSP
151800060420     C                   KFLD                    taaTRC
151900060420     C*
152000060420     C* FIAR531C - Parziale
152100060420     C     KEYar531_P    KLIST
152200060420     C                   KFLD                    tasAAS
152300060420     C                   KFLD                    tasLNP
152400060420     C                   KFLD                    tasNRS
152500060420     C                   KFLD                    tasNSP
152600060420     C                   KFLD                    ar5TRD
152700991027     C*
152800991027     C                   ENDSR
152900060421** DEC - COSTANTI
153000060421<?xml version="1.0"?>
153100060421<?mso-application progid="Excel.Sheet"?>
153200060421<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
153300060421xmlns:o="urn:schemas-microsoft-com:office:office"
153400060421xmlns:x="urn:schemas-microsoft-com:office:excel"
153500060421xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
153600060421xmlns:html="http://www.w3.org/TR/REC-html40">
153700060421</Workbook>
153800060421<Worksheet ss:Name="PAG1">
153900060421</Worksheet>
154000060421<Table border="1">
154100060421</Table>
154200060421<Row>
154300060421</Row>
154400060421<Cell><Data ss:Type="String">
154500060421<Cell><Data ss:Type="Number">
154600060421</Data></Cell>
154700060421
154800060421
154900060421
155000060704** CFG - CODICI FINE GIACENZA
155100060704001
155200060704003
155300060704005
155400060704006
155500060704008
155600060704010
155700060704
155800060704
155900060704
156000060704
