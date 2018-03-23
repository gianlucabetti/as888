000100010925      *PARMS DYNUSRPRF(*OWNER)
000200000707     H dftactgrp(*yes)
000300970526     H*------------------------------------------------------------------------*
000400031110     H* Raggruppamento bolle x stesso destinatario da EDIVAB a FIVAB
000500970526     H*------------------------------------------------------------------------*
000600970526     H DECEDIT('0,') DATEDIT(*DMY.)
000700970526     F*------------------------------------------------------------------------*
000800970526     F* DATA BASE
000900970526     F*------------------------------------------------------------------------*
001000070809     FEDIVABQT1LUF   E           K DISK    USROPN
001100040712     FEDIVADQT1LUF   E           K DISK    USROPN
001101171219     FEDIVADQ11LUF   E           K DISK    USROPN
001200040712     FEDIVATQT1LUF   E           K DISK    USROPN
001300070807     FEDIVAXQT1LUF   E           K DISK    USROPN
001400070808     F*EDIVAX2L  UF   E           K DISK    COMMIT
001500070802     FFIVAB00F  O    E             DISK    COMMIT
001600070802     FFIVAD00F  O    E             DISK    COMMIT
001601171219     FFIVAD10F  O    E             DISK    COMMIT
001700070802     FFIVAT00F  O    E             DISK    COMMIT
001800070802     FFIVAX00F  O    E             DISK    COMMIT
001900081013     FFIRAB02L  UF A E           K DISK    COMMIT
002000040227     FTABEL00F  IF   E           K DISK
002100970526     D*------------------------------------------------------------------------*
002200970526     D* SCHIERE
002300970526     D*------------------------------------------------------------------------*
002400991112     D*------------
002500991112     D* ARCHITETTURA
002600991112     D*------------
002700991112     D KPJBA         E DS
002800041116     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900041116     D CNCR80        E DS
003000991110     D*------------
003100031110     D* DS D PROCEDURA
003200991110     D*------------
003300031110     D TRTC86DS      E DS
003400040227     D DS1B          E DS
003500050610     D DS3C          E DS
003600051021     D DSEDIVAB      E DS                  EXTNAME(EDIVAB0F) PREFIX(SQL_)
003700031110     D DSEDIVABW     E DS                  EXTNAME(EDIVAB0F) PREFIX(WRK_)
003800031112     D DSEDIVABOUT   E DS                  EXTNAME(EDIVAB0F)
003900070420     D DSEDIVABINZ   E DS                  EXTNAME(EDIVAB0F) PREFIX(INZ_) INZ
004000070809     D DSEDIVAX      E DS                  EXTNAME(EDIVAX0F) INZ
004100070809     D DSEDIVAXSAV   E DS                  EXTNAME(EDIVAX0F) PREFIX(SAV_) INZ
004200051215     D FNLS24DS      E DS
004300070227     D Fnls25ds      e ds
004400041116     D TRUL06DS      E DS
004500041116     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
004600070228     D*------------------
004700070228     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800070228     D*------------------
004900070228     D WLBDA8          DS                  INZ
005000070228     D  G08DAT                 1      8  0
005100070228     D  G08INV                 9     16  0
005200070228     D  G08ERR                17     17
005300070228     D  G08TGI                18     22  0
005400031110     D*------------
005500031110     D* DS D WRK
005600031110     D*------------
005700051021     D  DSEDIABD       DS
005800031114     D   S_vabCCM                          LIKE(SQL_vabCCM)
005900031110     D   S_vabRSD                          LIKE(SQL_vabRSD)
006000031110     D   S_vabRD2                          LIKE(SQL_vabRD2)
006100031110     D   S_vabIND                          LIKE(SQL_vabIND)
006200031110     D   S_vabCAD                          LIKE(SQL_vabCAD)
006300031110     D   S_vabLOD                          LIKE(SQL_vabLOD)
006400031110     D   S_vabPRD                          LIKE(SQL_vabPRD)
006500031110     D   S_vabNZD                          LIKE(SQL_vabNZD)
006600031110     D   S_vabLNA                          LIKE(SQL_vabLNA)
006700031110     D   S_vabZNC                          LIKE(SQL_vabZNC)
006800031110     D   S_vabCBO                          LIKE(SQL_vabCBO)
006900031110     D   S_vabTSP                          LIKE(SQL_vabTSP)
007000031110     D   S_vabCTR                          LIKE(SQL_vabCTR)
007100031110     D   S_vabGC1                          LIKE(SQL_vabGC1)
007200031110     D   S_vabGC2                          LIKE(SQL_vabGC2)
007300031110     D   S_vabFFD                          LIKE(SQL_vabFFD)
007400031110     D   S_vabDCR                          LIKE(SQL_vabDCR)
007500031110     D   S_vabTCR                          LIKE(SQL_vabTCR)
007600031110     D   S_vabHCR                          LIKE(SQL_vabHCR)
007700031110     D   S_vabGMA                          LIKE(SQL_vabGMA)
007800031110     D   S_vabGGA                          LIKE(SQL_vabGGA)
007900031110     D   S_vabGVA                          LIKE(SQL_vabGVA)
008000031110     D   S_vabTC1                          LIKE(SQL_vabTC1)
008100031110     D   S_vabTC2                          LIKE(SQL_vabTC2)
008200031110     D   S_vabVAS                          LIKE(SQL_vabVAS)
008300051021     D   S_vabVAD                          LIKE(SQL_vabVAD)
008400051021     D*
008500031110     D                 DS
008600031110     D   ANTTIP1                      2  0
008700031110     D   ANTNUM1                      2  0
008800031110     D   ANTTIP2                      2  0
008900031110     D   ANTNUM2                      2  0
009000150518     D   ANTFILL                      1  0
009100150518     D  DSVABANT               1      9  0
009200051215     D*------------------
009300051215     D* DS RIDEFINIZIONE CHIAVE BOLLA ALFA
009400051215     D*------------------
009500051215     D                 DS                  INZ
009600051215     D  wRC_AAS                1      4  0
009700051215     D  wRC_LNP                5      7  0
009800051215     D  wRC_NRS                8      9  0
009900051215     D  wRC_NSP               10     16  0
010000051215     D  wRC_KEY                1     16
010100051215     D*
010200031110     D*------------
010300031110     D* VARIABILI D WRK
010400031110     D*------------
010500051021     D SAVEDIABD       S                   LIKE(DSEDIABD)
010600051021     D SAV_vabCAS      S                   LIKE(SQL_vabCAS)
010700051021     D SAV_vabVCA      S                   LIKE(SQL_vabVCA)
010800051021     D SAV_vabTIC      S                   LIKE(SQL_vabTIC)
010900060530     D wGiro           S              3  0
011000060530     D wGiro2          S              2  0
011100031110     D wANTTIP1        S                   LIKE(ANTTIP1)
011200031110     D wANTNUM1        S                   LIKE(ANTNUM1)
011300031110     D wANTTIP2        S                   LIKE(ANTTIP2)
011400031110     D wANTNUM2        S                   LIKE(ANTNUM2)
011500050610     D wRIF            S             99    VARYING
011600031114     D wRMO            S                   LIKE(VABRMO)
011700050620     D wNOT            S                   LIKE(VABNOT)
011800050620     D wNT2            S                   LIKE(VABNT2)
011900080929     D wNOTNT2         S             70A   INZ
012000051215     D wFlgOrmRC       S              1N   INZ(*off)
012100081016     D ACC_NSP         S                   LIKE(VABNSP)
012200070807     D wVAXmaster      s              9S 0 inz(*zeros)
012300070905     D wVAXmaster01    s                   like(wVAXmaster) inz(*zeros)
012400070905     D wVAXmaster02    s                   like(wVAXmaster) inz(*zeros)
012500081013     D wFlgMaxVB       s              1  0 inz(*zeros)
012600050610     D*------------
012700050620     D* VARIABILE RELATIVE AL RAGGRUPPAMENTO BOLLE
012800050610     D*------------
012900050610     D wTipABD         S                   like(§3CABD)
013000050610     D wLenRIF         S                   like(§3CLRM)
013100050610     D wAlmRIF         S                   like(§3CALM)
013200050620     D wRgrNOT         S                   like(§3CNOT)
013300051021     D wAccCAS         S                   like(§3CABC)
013400051215     D wAccRC          S                   like(§3CARC)
013500070809     D wAccVX          S                   like(§3CAVX)
013600081013     D wMaxVB          S                   like(§3CMVR)
013700000629
013800000707
013900970526     C*------------------------------------------------------------------------*
014000970526     C* MAIN LINES
014100970526     C*------------------------------------------------------------------------*
014200071203     C*
014300071203     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
014400071203     C
014500071203     C/EXEC SQL
014600071203     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014700071203     C/END-EXEC
014800050708     C*
014900050708     C* Reperisce quale riferimenti mittente mantenere durante l'accorpamento
015000050708     C                   exsr      repABD
015100031110     C*
015200031110     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
015300031110     C                   exsr      exeABD
015400051216     C*
015500051216     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
015600051216     C* x gestione discorso particolarità consegna 'RC'
015700051216     C                   exsr      exeABDRC
015800031110     C*
015900031110     C* Eseguo routine d scrittura del FIVAB/FIVAD x bolle NN raggruppabili
016000031110     C                   exsr      exeNOABD
016100991112     C*
016200970526     C* FINE PGM
016300031110     C                   seton                                        LR
016400991112     C*------------------------------------------------------------------------*
016500031110     C* EXEABD - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
016600991112     C*------------------------------------------------------------------------*
016700031110     C     EXEABD        BEGSR
016800081013     C*
016900081013     C                   setoff                                       25
017000081013     C*
017100081013     C* Ciclo finchè nn diversamente specificato da condizionamenti a run-time
017200081013     C                   dou       *in25
017300050620     C*
017400050620     C* Inizializzo indicatori d wrk
017500050620     C                   setoff                                       51
017600051216     C                   setoff                                       52
017700091002     C                   setoff                                       53
017800040709     C*
017900040712     C* Innanzitutto apro i file EDI*QT temporanei
018000040709     C                   open      EDIVABQT1L
018100040712     C                   open      EDIVADQT1L
018101171219     C                   open      EDIVADQ11L
018200040712     C                   open      EDIVATQT1L
018300070807     C                   open      EDIVAXQT1L
018400031110     C*
018500031110     C* Inizializzazioni
018600031110     C                   clear                   DSEDIVAB
018700031110     C                   clear                   DSEDIABD
018800031110     C                   clear                   SAVEDIABD
018900031110     C                   exsr      exeINZ
019000031114     C                   eval      wGiro = *zeros
019100031113     C                   seton                                        55
019200081013     C                   eval      wFlgMaxVB = *zeros
019300000707     C*
019400000707     C/EXEC SQL
019500051216     C+ declare C1 cursor for select * from edivabqt
019600051216     C+ where vabGMA <> 'RC' and vabGMA <> '  ' and vabATB = ' ' and vabxco=' '
019700031114     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
019800031110     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
019900031110     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
020000031110     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
020100051021     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
020200000707     C+ for fetch only
020300000707     C/END-EXEC
020400000707     C
020500000707     C/EXEC SQL
020600051216     C+ open C1
020700000707     C/END-EXEC
020800000707     C
020900000707     C/EXEC SQL
021000051216     C+ Fetch C1 into :DSEDIVAB
021100000707     C/END-EXEC
021200000707     C*
021300081013     C                   if        sqlcod <> *zeros
021400081013     C                   seton                                        25
021500081013     C                   else
021600081013     C                   dow       sqlcod = *zeros AND wFlgMaxVB = *zeros
021700040709     C*
021800040709     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
021900051021     C                   exsr      CHKREC
022000040709     C*
022100040709     C* Se ok proseguo
022200040709     C                   if        wRECOK = 'S'
022300091002     C*
022400091002     C* Chiodo x TTL (cod. 0504453 - 0504450)
022500091002     C                   if        SQL_vabCCM = 0504453 or
022600091002     C                             SQL_vabCCM = 0504450
022700091002     C                   seton                                        53
022800091002     C                   endif
022900031110     C*
023000031110     C* Eseguo routine d valorizzazione DS d livello rottura
023100031110     C                   exsr      valDSROT
023200051216     C*
023300051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
023400051216     C                   z-add     *zeros        wFlgErr           1 0
023500051021     C*
023600051215     C* Verifico avvenuta rottura d codice
023700051021     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
023800051021     C                             (DSEDIABD=SAVEDIABD     AND
023900051021     C                              SQL_vabCAS>*zeros      AND
024000051021     C                             (SQL_vabVCA=SAV_vabVCA  AND
024100051021     C                              SQL_vabTIC=SAV_vabTIC   OR
024200051021     C                              SAV_vabCAS=*zeros)     AND
024300051021     C                              wAccCAS='S')
024400051021     C*
024500051021     C* ****************  ESEGUO ACCORPAMENTO **********************
024600051216     C*
024700051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
024800051216     C                   exsr      exeRC
024900051216     C*
025000051216     C* Se fin qui tutto ok => proseguo
025100051216     C                   if        wFlgErr = *zeros
025200051021     C*
025300051021     C* Eseguo la sommatoria dei campi da raggruppare
025400051216     C                   exsr      exeSOMMA
025500051021     C*
025600051021     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
025700051021     C                   exsr      exeNORM
025800070227     C*
025900070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
026000070227     C   60              exsr      wriRAB
026100051021     C*
026200051021     C* Effettuo scrittura del file FIVAD
026300051021     C                   exsr      wriVAD
026301171219
026302171219     C* Effettuo scrittura del file FIVAD10
026303171219     C                   exsr      wriVAD10
026400051021     C*
026500051021     C* Effettuo scrittura del file FIVAT
026600051021     C                   exsr      wriVAT
026700070807     C*
026800070807     C* Effettuo scrittura del file FIVAX
026900070807     C                   exsr      wriVAX
027000051216     C*
027100051216     C                   endif
027200051021     C*
027300070228     C* ****************  NUOVA ROTTURA ************************
027400051021     C                   else
027500051021     C*
027600051021     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
027700051021     C* dell'ultima bolla accorpata
027800051021     C                   if        wGiro > 0
027900051021     C                   exsr      wriVAB
028000051021     C                   eval      wGiro = *zeros
028100051021     C                   endif
028200051021     C*
028300051021     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
028400051021     C                   eval      SAVEDIABD  = DSEDIABD
028500051021     C*
028600051021     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
028700051021     C                   exsr      exeINZ
028800051021     C*
028900051021     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
029000081016     C   60              exsr      repnum
029100051021     C                   exsr      impVAB
029200070227     C*
029300070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
029400070227     C   60              exsr      wriRAB
029500051021     C*
029600051021     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
029700051021     C                   exsr      wriVAD
029701171219
029702171219     C* Effettuo scrittura del file FIVAD10 x dettaglio bolla "master"
029703171219     C                   exsr      wriVAD10
029800051021     C*
029900051021     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
030000051021     C                   exsr      wriVAT
030100070807     C*
030200070807     C* Effettuo scrittura del file FIVAX x dettaglio bolla "master"
030300070807     C                   exsr      wriVAX
030400031110     C*
030500031110     C                   endif
030600031110     C*
030700031110     C* Incremento il contatore che identifica il 1° giro
030800031114     C                   eval      wGiro = wGiro + 1
030900051102     C*
031000051102     C* Salvo i nuovi attributi relativi al contrassegno
031100051102     C                   eval      SAV_vabCAS = SQL_vabCAS
031200051102     C                   eval      SAV_vabVCA = SQL_vabVCA
031300051102     C                   eval      SAV_vabTIC = SQL_vabTIC
031400051216     C*
031500051216     C* Come ultima cosa deleto il record bolla testata appena considerato
031600051216     C* ...solo se tutto ok => proseguo
031700051216     C                   if        wFlgErr = *zeros
031800040712     C     KEYvab1       delete    EDIVABQT1L
031900051216     C                   endif
032000040709     C*
032100040709     C                   endif
032200031110     C*
032300031110     C/EXEC SQL
032400051216     C+ Fetch C1 into :DSEDIVAB
032500031110     C/END-EXEC
032600081013     C*
032700081013     C* Se richiesto verifico se raggiunto massimo limite accorpamento bolle
032800081013     C                   if        wMaxVB > *zeros
032900081013     C                   if        wGiro  < wMaxVB
033000081013     C                   else
033100081013     C                   eval      wFlgMaxVB = 1
033200081013     C                   endif
033300081013     C                   endif
033400081013     C*
033500031110     C                   enddo
033600031114     C*
033700031114     C* Scarico la bolla "master" in sospeso
033800031114     C* dell'ultima bolla accorpata
033900031114     C                   exsr      wriVAB
034000081013     C*
034100081013     C                   endif
034200081013     C*
034300081013     C* Al termine chiudo i file EDI*QT temporanei
034400081013     C                   close     EDIVABQT1L
034500081013     C                   close     EDIVADQT1L
034501171219     C                   close     EDIVADQ11L
034600081013     C                   close     EDIVATQT1L
034700081013     C                   close     EDIVAXQT1L
034800081013     C*
034900081013     C/EXEC SQL
035000081013     C+ close C1
035100081013     C/END-EXEC
035200081013     C*
035300081013     C                   enddo
035400991112     C*
035500991112     C                   ENDSR
035600051216     C*------------------------------------------------------------------------*
035700051216     C* EXEABDRC - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
035800051216     C*------------------------------------------------------------------------*
035900051216     C     EXEABDRC      BEGSR
036000081013     C*
036100081013     C                   setoff                                       25
036200081013     C*
036300081013     C* Ciclo finchè nn diversamente specificato da condizionamenti a run-time
036400081013     C                   dou       *in25
036500051216     C*
036600051216     C* Inizializzo indicatori d wrk
036700051216     C                   setoff                                       51
036800051216     C                   seton                                        52
036900091002     C                   setoff                                       53
037000051216     C*
037100051216     C* Innanzitutto apro i file EDI*QT temporanei
037200051216     C                   open      EDIVABQT1L
037300051216     C                   open      EDIVADQT1L
037301171220     C                   open      EDIVADQ11L
037400051216     C                   open      EDIVATQT1L
037500070807     C                   open      EDIVAXQT1L
037600051216     C*
037700051216     C* Inizializzazioni
037800051216     C                   clear                   DSEDIVAB
037900051216     C                   clear                   DSEDIABD
038000051216     C                   clear                   SAVEDIABD
038100051216     C                   exsr      exeINZ
038200051216     C                   eval      wGiro = *zeros
038300051216     C                   seton                                        55
038400081013     C                   eval      wFlgMaxVB = *zeros
038500051216     C*
038600051216     C/EXEC SQL
038700051216     C+ declare C2 cursor for select * from edivabqt
038800051216     C+ where (vabGMA = 'RC' or vabGMA = '  ') and vabATB = ' ' and vabxco=' '
038900051216     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
039000051216     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
039100051216     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
039200051216     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
039300051216     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
039400051216     C+ for fetch only
039500051216     C/END-EXEC
039600051216     C
039700051216     C/EXEC SQL
039800051216     C+ open C2
039900051216     C/END-EXEC
040000051216     C
040100051216     C/EXEC SQL
040200051216     C+ Fetch C2 into :DSEDIVAB
040300051216     C/END-EXEC
040400051216     C*
040500081013     C                   if        sqlcod <> *zeros
040600081013     C                   seton                                        25
040700081013     C                   else
040800081013     C                   dow       sqlcod = *zeros AND wFlgMaxVB = *zeros
040900051216     C*
041000051216     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
041100051216     C                   exsr      CHKREC
041200051216     C*
041300051216     C* Se ok proseguo
041400051216     C                   if        wRECOK = 'S'
041500091002     C*
041600091002     C* Chiodo x TTL (cod. 0504453 - 0504450)
041700091002     C                   if        SQL_vabCCM = 0504453 or
041800091002     C                             SQL_vabCCM = 0504450
041900091002     C                   seton                                        53
042000091002     C                   endif
042100051216     C*
042200051216     C* Eseguo routine d valorizzazione DS d livello rottura
042300051216     C                   exsr      valDSROT
042400051216     C*
042500051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
042600051216     C                   z-add     *zeros        wFlgErr           1 0
042700051216     C*
042800051216     C* Verifico avvenuta rottura d codice
042900051216     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
043000051216     C                             (DSEDIABD=SAVEDIABD     AND
043100051216     C                              SQL_vabCAS>*zeros      AND
043200051216     C                             (SQL_vabVCA=SAV_vabVCA  AND
043300051216     C                              SQL_vabTIC=SAV_vabTIC   OR
043400051216     C                              SAV_vabCAS=*zeros)     AND
043500051216     C                              wAccCAS='S')
043600051216     C*
043700051216     C* ****************  ESEGUO ACCORPAMENTO **********************
043800051216     C*
043900051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
044000051216     C                   exsr      exeRC
044100051216     C*
044200051216     C* Se fin qui tutto ok => proseguo
044300051216     C                   if        wFlgErr = *zeros
044400051216     C*
044500051216     C* Eseguo la sommatoria dei campi da raggruppare
044600051216     C                   exsr      exeSOMMA
044700051216     C*
044800051216     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
044900051216     C                   exsr      exeNORM
045000070227     C*
045100070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
045200070227     C   60              exsr      wriRAB
045300051216     C*
045400051216     C* Effettuo scrittura del file FIVAD
045500051216     C                   exsr      wriVAD
045501171219
045502171219     C* Effettuo scrittura del file FIVAD10
045503171219     C                   exsr      wriVAD10
045600051216     C*
045700051216     C* Effettuo scrittura del file FIVAT
045800051216     C                   exsr      wriVAT
045900070807     C*
046000070807     C* Effettuo scrittura del file FIVAX
046100070807     C                   exsr      wriVAX
046200051216     C*
046300051216     C                   endif
046400051216     C*
046500070228     C* ****************  NUOVA ROTTURA ************************
046600051216     C                   else
046700051216     C*
046800051216     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
046900051216     C* dell'ultima bolla accorpata
047000051216     C                   if        wGiro > 0
047100051216     C                   exsr      wriVAB
047200051216     C                   eval      wGiro = *zeros
047300051216     C                   endif
047400051216     C*
047500051216     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
047600051216     C                   eval      SAVEDIABD  = DSEDIABD
047700051216     C*
047800051216     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
047900051216     C                   exsr      exeINZ
048000051216     C*
048100051216     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
048200081016     C   60              exsr      repnum
048300070227     C                   exsr      impVAB
048400070227     C*
048500070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
048600070227     C   60              exsr      wriRAB
048700051216     C*
048800051216     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
048900051216     C                   exsr      wriVAD
048901171219     C*
048902171219     C* Effettuo scrittura del file FIVAD10 x dettaglio bolla "master"
048903171219     C                   exsr      wriVAD10
049000051216     C*
049100051216     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
049200051216     C                   exsr      wriVAT
049300070807     C*
049400070807     C* Effettuo scrittura del file FIVAX x dettaglio bolla "master"
049500070807     C                   exsr      wriVAX
049600051216     C*
049700051216     C                   endif
049800051216     C*
049900051216     C* Incremento il contatore che identifica il 1° giro
050000051216     C                   eval      wGiro = wGiro + 1
050100051216     C*
050200051216     C* Salvo i nuovi attributi relativi al contrassegno
050300051216     C                   eval      SAV_vabCAS = SQL_vabCAS
050400051216     C                   eval      SAV_vabVCA = SQL_vabVCA
050500051216     C                   eval      SAV_vabTIC = SQL_vabTIC
050600051216     C*
050700051216     C* Come ultima cosa deleto il record bolla testata appena considerato
050800051216     C* ...solo se tutto ok => proseguo
050900051216     C                   if        wFlgErr = *zeros
051000051216     C     KEYvab1       delete    EDIVABQT1L
051100051216     C                   endif
051200051216     C*
051300051216     C                   endif
051400051216     C*
051500051216     C/EXEC SQL
051600051216     C+ Fetch C2 into :DSEDIVAB
051700051216     C/END-EXEC
051800081013     C*
051900081013     C* Se richiesto verifico se raggiunto massimo limite accorpamento bolle
052000081013     C                   if        wMaxVB > *zeros
052100081013     C                   if        wGiro  < wMaxVB
052200081013     C                   else
052300081013     C                   eval      wFlgMaxVB = 1
052400081013     C                   endif
052500081013     C                   endif
052600081013     C*
052700051216     C                   enddo
052800051216     C*
052900051216     C* Scarico la bolla "master" in sospeso
053000051216     C* dell'ultima bolla accorpata
053100051216     C                   exsr      wriVAB
053200081013     C*
053300081013     C                   endif
053400081013     C*
053500081013     C* Al termine chiudo i file EDI*QT temporanei
053600081013     C                   close     EDIVABQT1L
053700081013     C                   close     EDIVADQT1L
053701171219     C                   close     EDIVADQ11L
053800081013     C                   close     EDIVATQT1L
053900081013     C                   close     EDIVAXQT1L
054000081013     C*
054100081013     C/EXEC SQL
054200081013     C+ close C2
054300081013     C/END-EXEC
054400081013     C*
054500081013     C                   enddo
054600051216     C*
054700051216     C                   ENDSR
054800031110     C*------------------------------------------------------------------------*
054900040712     C* EXENOABD - SCRITTIURA FILE FIVAB/FIVAD X BOLLE NN RAGGRUPPABILI
055000031110     C*------------------------------------------------------------------------*
055100031110     C     EXENOABD      BEGSR
055200050620     C*
055300050620     C* Inizializzo indicatori d wrk
055400050620     C                   setoff                                       51
055500040712     C*
055600040712     C* Innanzitutto apro i file EDI*QT temporanei
055700040712     C                   open      EDIVABQT1L
055800040712     C                   open      EDIVADQT1L
055801171219     C                   open      EDIVADQ11L
055900040712     C                   open      EDIVATQT1L
056000070807     C                   open      EDIVAXQT1L
056100171219     C*
056200040712     C* Porto tutto ciò che è rimasto nei file EDI*QT nei relativi FIVA*
056300040712     C*
056400040712     C* ...VAB
056500040712     C     *loval        setll     EDIVABQT1L
056600040712     C                   read      EDIVABQT1L
056700040712     C                   dow       not %eof(EDIVABQT1L)
056800040712     C                   write     FIVAB000
056900040712     C                   delete    EDIVABQT1L
057000040712     C                   read      EDIVABQT1L
057100040712     C                   enddo
057200040712     C*
057300040712     C* ...VAD
057400040712     C     *loval        setll     EDIVADQT1L
057500040712     C                   read      EDIVADQT1L
057600040712     C                   dow       not %eof(EDIVADQT1L)
057700040712     C                   write     FIVAD000
057800040712     C                   delete    EDIVADQT1L
057900040712     C                   read      EDIVADQT1L
058000040712     C                   enddo
058001171219     C* ...VAD10
058002171219     C     *loval        setll     EDIVADQ11L
058003171219     C                   read      EDIVADQ11L
058004171219     C                   dow       not %eof(EDIVADQ11L)
058005171219     C                   write     FIVAD100
058006171219     C                   delete    EDIVADQ11L
058007171219     C                   read      EDIVADQ11L
058008171219     C                   enddo
058100040712     C*
058200040712     C* ...VAT
058300040712     C     *loval        setll     EDIVATQT1L
058400040712     C                   read      EDIVATQT1L
058500040712     C                   dow       not %eof(EDIVATQT1L)
058600040712     C                   write     FIVAT000
058700040712     C                   delete    EDIVATQT1L
058800040712     C                   read      EDIVATQT1L
058900040712     C                   enddo
059000070807     C*
059100070807     C* ...VAX
059200070807     C     *loval        setll     EDIVAXQT1L
059300070807     C                   read      EDIVAXQT1L
059400070807     C                   dow       not %eof(EDIVAXQT1L)
059500070807     C                   write     FIVAX000
059600070807     C                   delete    EDIVAXQT1L
059700070807     C                   read      EDIVAXQT1L
059800070807     C                   enddo
059900040714     C*
060000070807     C** ...VAX
060100070807     C*     KEYvax2_C     setll     EDIVAX2L
060200070807     C*     KEYvax2_C     reade     EDIVAX2L
060300070807     C*                   dow       not %eof(EDIVAX2L)
060400070807     c*     vaxfgs        lookup    lin                                    50
060500070807     c*                   if        *in50  and vaxfgs>0
060600070807     C*                   write     FIVAX000
060700070807     C*                   delete    EDIVAX2L
060800070807     c*                   endif
060900070807     c*
061000070807     C*     KEYvax2_C     reade     EDIVAX2L
061100070807     C*                   enddo
061200040712     C*
061300040714     C* Al termine chiudo i file EDI* temporanei
061400040712     C                   close     EDIVABQT1L
061500040712     C                   close     EDIVADQT1L
061501171219     C                   close     EDIVADQ11L
061600040712     C                   close     EDIVATQT1L
061700070807     C                   close     EDIVAXQT1L
061800031110     C*
061900031110     C                   ENDSR
062000031110     C*------------------------------------------------------------------------*
062100031110     C* EXEINZ - ROUTINE D INIZIALIZZAZIONE X OGNI NUOVO LIVELLO D ROTTURA BOLLE
062200031110     C*------------------------------------------------------------------------*
062300031110     C     EXEINZ        BEGSR
062400031110     C*
062500031110     C                   clear                   DSEDIVABW
062600031114     C                   clear                   wRMO
062700050620     C                   clear                   wNOT
062800050620     C                   clear                   wNT2
062900080929     C                   clear                   wNOTNT2
063000051215     C                   eval      wFlgOrmRC = *off
063100031110     C*
063200031110     C                   ENDSR
063300031110     C*------------------------------------------------------------------------*
063400031110     C* VALDSROT - VALORIZZAZIONE DS LIVELLO ROTTURA
063500031110     C*------------------------------------------------------------------------*
063600031112     C     VALDSROT      BEGSR
063700031110     C*
063800031110     C* Campi in rottura
063900031114     C                   EVAL      S_vabCCM = SQL_vabCCM
064000031110     C                   EVAL      S_vabRSD = SQL_vabRSD
064100031110     C                   EVAL      S_vabRD2 = SQL_vabRD2
064200031110     C                   EVAL      S_vabIND = SQL_vabIND
064300031110     C                   EVAL      S_vabCAD = SQL_vabCAD
064400031110     C                   EVAL      S_vabLOD = SQL_vabLOD
064500031110     C                   EVAL      S_vabPRD = SQL_vabPRD
064600031110     C                   EVAL      S_vabNZD = SQL_vabNZD
064700031110     C                   EVAL      S_vabLNA = SQL_vabLNA
064800091002     C  N53              EVAL      S_vabZNC = SQL_vabZNC
064900031110     C                   EVAL      S_vabCBO = SQL_vabCBO
065000031110     C                   EVAL      S_vabTSP = SQL_vabTSP
065100031110     C                   EVAL      S_vabCTR = SQL_vabCTR
065200031110     C                   EVAL      S_vabGC1 = SQL_vabGC1
065300031110     C                   EVAL      S_vabGC2 = SQL_vabGC2
065400031110     C                   EVAL      S_vabFFD = SQL_vabFFD
065500031110     C                   EVAL      S_vabDCR = SQL_vabDCR
065600031110     C                   EVAL      S_vabTCR = SQL_vabTCR
065700031110     C                   EVAL      S_vabHCR = SQL_vabHCR
065800051216     C  N52              EVAL      S_vabGMA = SQL_vabGMA
065900031110     C                   EVAL      S_vabGGA = SQL_vabGGA
066000031110     C                   EVAL      S_vabGVA = SQL_vabGVA
066100031110     C                   EVAL      S_vabTC1 = SQL_vabTC1
066200031110     C                   EVAL      S_vabTC2 = SQL_vabTC2
066300031110     C                   EVAL      S_vabVAS = SQL_vabVAS
066400031110     C                   EVAL      S_vabVAD = SQL_vabVAD
066500031110     C*
066600031110     C                   ENDSR
066700031110     C*------------------------------------------------------------------------*
066800031110     C* IMPVAB - VALORIZZAZIONE CAMPI "FISSI" 1° BOLLA D OGNI LIVELLO D ROTTURA
066900031110     C*------------------------------------------------------------------------*
067000031110     C     IMPVAB        BEGSR
067100031110     C*
067200031110     C* Valorizzo la DS d wrk da quella d lettura SQL
067300031110     C                   EVAL      DSEDIVABW = DSEDIVAB
067400031110     C*
067500031110     C                   ENDSR
067600051216     C*------------------------------------------------------------------------*
067700051216     C* EXERC - GESTIONE EVENTUALE PRESENZA PARTICOLARITA' CONSEGNA 'RC'
067800051216     C*------------------------------------------------------------------------*
067900051216     C     EXERC         BEGSR
068000051216     C*
068100051216     C* Gestisco anche l'accorpamento anche dell'ORM da "RC"
068200051216     C* ...ma solo se trattasi d primo "RC" delle bolle oggetto d accorpamento
068300060126     C**!!!              IF        SQL_vabGMA = 'RC' and
068400060126     C**!!!                        wFlgOrmRC = *off
068500070227 MB 1C                   IF        SQL_vabGMA = 'RC'
068600070227 MB 2c                   If        wflgormrc = *On
068700060127     C*
068800060127     C* Elimino l'ORM RC della bolla corrente (accorpata)
068900060126     c                   Clear                   Fnls25ds
069000060126     c                   Eval      i25ksc  = sql_vabccm
069100060126     c                   Move(p)   sql_vabaas    i25aas
069200060126     c                   Eval      i25spe = %editc(sql_vablnp:'X') +
069300060126     c                                      %editc(sql_vabnrs:'X') +
069400060126     c                                      %editc(sql_vabnsp:'X')
069500060126     c                   Call      'FNLS25R'
069600060126     c                   Parm                    Fnls25ds
069700060126    2C                   ENDIF
069800060126    2C                   If        wFlgOrmRC = *off
069900060127     C*
070000060127     C* Aggiorno l'orm RC legato alla bolla risultante dall'accorpamento solo se
070100060127     C* bolla corrente (accorpata) e bolla accorpante hanno chiave diversa.
070200060127     C                   IF        SQL_vabAAS <> WRK_vabAAS OR
070300060127     C                             SQL_vabLNP <> WRK_vabLNP OR
070400060127     C                             SQL_vabNRS <> WRK_vabNRS OR
070500060127     C                             SQL_vabNSP <> WRK_vabNSP
070600060127     C*
070700060127     C* Prima dell'aggiornamento del riferimento ORM RC con la nuova chiave spedizione
070800060127     C* elimino l'ORM RC della bolla accorpante
070900060127     c                   Clear                   Fnls25ds
071000060127     c                   Eval      i25ksc  = sql_vabccm
071100060127     c                   Move(p)   sql_vabaas    i25aas
071200060127     c                   Eval      i25spe = %editc(WRK_vabLNP:'X') +
071300060127     c                                      %editc(WRK_vabNRS:'X') +
071400060127     c                                      %editc(WRK_vabNSP:'X')
071500060127     c                   Call      'FNLS25R'
071600060127     c                   Parm                    Fnls25ds
071700060127     C*
071800060127     C* Quindi aggiorno l'ORM RC della bolla corrente (accorpata) con la nuova chiave
071900060127     C* spedizione (bolla accorpante)
072000051216     C                   CLEAR                   wRC_KEY
072100051216     C                   EVAL      wRC_AAS = SQL_vabAAS
072200051216     C                   EVAL      wRC_LNP = SQL_vabLNP
072300051216     C                   EVAL      wRC_NRS = SQL_vabNRS
072400051216     C                   EVAL      wRC_NSP = SQL_vabNSP
072500051220     C                   EVAL      I24TLA  = 'L'
072600051216     C                   EVAL      I24OPE  = 'U'
072700051216     C                   EVAL      I24KSC  = SQL_vabCCM
072800051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
072900051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
073000051216     C                   EVAL      I24NAAS = WRK_vabAAS
073100051216     C                   EVAL      I24LNP  = WRK_vabLNP
073200051216     C                   EVAL      I24NRS  = WRK_vabNRS
073300051216     C                   EVAL      I24NSP  = WRK_vabNSP
073400051216     C                   CALL      'FNLS24R'
073500051216     C                   PARM                    FNLS24DS
073600060127     C*
073700051216     C* Se tutto ok => accendo l'indicatore x nn aggiornare ORM generati da bolle "RC"
073800051216     C* accorpate successivamente (ovviamente x medesimo destinatario)
073900060126    3C                   IF        O24ERR = *zeros
074000051216     C                   EVAL      wFlgOrmRC = *on
074100060126   x3C                   ELSE
074200051216     C                   Z-ADD     1             wFlgErr
074300060126    3C                   ENDIF
074400060127     C                   ENDIF
074500060126    2C                   ENDIF
074600060126    1C                   ENDIF
074700051216     C*
074800051216     C                   ENDSR
074900031110     C*------------------------------------------------------------------------*
075000031110     C* EXESOMMA - SOMMATORIA CAMPI X RAGGRUPPAMENTO
075100031110     C*------------------------------------------------------------------------*
075200031110     C     EXESOMMA      BEGSR
075300031110     C*
075400051216     C* Eseguo sommatoria dati bolle
075500031110     C                   EVAL      WRK_vabIAS = WRK_vabIAS + SQL_vabIAS
075600031110     C                   EVAL      WRK_vabNCL = WRK_vabNCL + SQL_vabNCL
075700031110     C                   EVAL      WRK_vabPKB = WRK_vabPKB + SQL_vabPKB
075800031110     C                   EVAL      WRK_vabVLB = WRK_vabVLB + SQL_vabVLB
075900031110     C                   EVAL      WRK_vabQFT = WRK_vabQFT + SQL_vabQFT
076000031110     C                   EVAL      WRK_vabVMD = WRK_vabVMD + SQL_vabVMD
076100051021     C                   EVAL      WRK_vabCAS = WRK_vabCAS + SQL_vabCAS
076200031110     C*
076300031110     C                   ENDSR
076400031110     C*------------------------------------------------------------------------*
076500031110     C* EXENORM - NORMALIZZAZIONE CAMPI X OTTENIMENTO BOLLA "MASTER"
076600031110     C*------------------------------------------------------------------------*
076700031110     C     EXENORM       BEGSR
076800050620     C*
076900050620     C* Valorizzo indicatore d wrk
077000050620     C                   SETON                                        51
077100031110     C*
077200031114     C                   IF        WRK_vabNAS =  *blanks AND
077300031114     C                             SQL_vabNAS <> *blanks
077400031110     C                   EVAL      WRK_vabNAS = SQL_vabNAS
077500031110     C                   ENDIF
077600031110     C*
077700031114     C                   IF        WRK_vabCTM =  *blanks AND
077800031114     C                             SQL_vabCTM <> *blanks
077900031110     C                   EVAL      WRK_vabCTM = SQL_vabCTM
078000031110     C                   ENDIF
078100031110     C*
078200031114     C                   IF        WRK_vabCTS =  *blanks AND
078300031114     C                             SQL_vabCTS <> *blanks
078400031110     C                   EVAL      WRK_vabCTS = SQL_vabCTS
078500031110     C                   ENDIF
078600031110     C*
078700031114     C                   IF        WRK_vabFTM =  *blanks AND
078800031114     C                             SQL_vabFTM <> *blanks
078900031110     C                   EVAL      WRK_vabFTM = SQL_vabFTM
079000031110     C                   ENDIF
079100031110     C*
079200031114     C                   IF        WRK_vabSCL =  *blanks AND
079300031114     C                             SQL_vabSCL <> *blanks
079400031110     C                   EVAL      WRK_vabSCL = SQL_vabSCL
079500031110     C                   ENDIF
079600051021     C*
079700051021     C                   IF        WRK_vabVCA =  *blanks AND
079800051021     C                             SQL_vabVCA <> *blanks
079900051021     C                   EVAL      WRK_vabVCA = SQL_vabVCA
080000051021     C                   ENDIF
080100051021     C*
080200051021     C                   IF        WRK_vabTIC =  *blanks AND
080300051021     C                             SQL_vabTIC <> *blanks
080400051021     C                   EVAL      WRK_vabTIC = SQL_vabTIC
080500051021     C                   ENDIF
080600051216     C*
080700051216     C                   IF        WRK_vabGMA =  *blanks AND
080800051216     C                             SQL_vabGMA <> *blanks
080900051216     C                   EVAL      WRK_vabGMA = SQL_vabGMA
081000051216     C                   ENDIF
081100031110     C*
081200031110     C* Gestisco comportamento particolare del campo VABANT (tipo e numero bancali)
081300031110     C                   IF        WRK_vabANT = *zeros
081400031110     C                   EVAL      WRK_vabANT = SQL_vabANT
081500031110     C                   ELSE
081600031110     C                   IF        SQL_vabANT = *zeros
081700031110     C                   ELSE
081800031110     C                   EVAL      DSVABANT = SQL_vabANT
081900031110     C                   EVAL      wANTTIP1 = ANTTIP1
082000031110     C                   EVAL      wANTNUM1 = ANTNUM1
082100031110     C                   EVAL      wANTTIP2 = ANTTIP2
082200031110     C                   EVAL      wANTNUM2 = ANTNUM2
082300031110     C                   EVAL      DSVABANT = WRK_vabANT
082400031110     C                   IF        ANTTIP1 > *zeros
082500031110     C                   IF        wANTTIP1 = ANTTIP1
082600031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM1
082700031110     C                   ENDIF
082800031110     C                   IF        wANTTIP2 = ANTTIP1
082900031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM2
083000031110     C                   ENDIF
083100031110     C                   ELSE
083200031110     C                   IF        wANTTIP1 > *zeros
083300031110     C                   EVAL      ANTTIP1 = wANTTIP1
083400031110     C                   EVAL      ANTNUM1 = wANTNUM1
083500031110     C                   ELSE
083600031110     C                   IF        wANTTIP2 > *zeros
083700031110     C                   EVAL      ANTTIP1 = wANTTIP2
083800031110     C                   EVAL      ANTNUM1 = wANTNUM2
083900031110     C                   ENDIF
084000031110     C                   ENDIF
084100031110     C                   ENDIF
084200031110     C                   IF        ANTTIP2 > *zeros
084300031110     C                   IF        wANTTIP1 = ANTTIP2
084400031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM1
084500031110     C                   ENDIF
084600031110     C                   IF        wANTTIP2 = ANTTIP2
084700031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM2
084800031110     C                   ENDIF
084900031110     C                   ELSE
085000031110     C                   IF        wANTTIP1 > *zeros
085100031110     C                   EVAL      ANTTIP2 = wANTTIP1
085200031110     C                   EVAL      ANTNUM2 = wANTNUM1
085300031110     C                   ELSE
085400031110     C                   IF        wANTTIP2 > *zeros
085500031110     C                   EVAL      ANTTIP2 = wANTTIP2
085600031110     C                   EVAL      ANTNUM2 = wANTNUM2
085700031110     C                   ENDIF
085800031110     C                   ENDIF
085900031110     C                   ENDIF
086000031110     C                   EVAL      WRK_vabANT = DSVABANT
086100031110     C                   ENDIF
086200031110     C                   ENDIF
086300031114     C*
086400051021     C* Eseguo raggruppamento dei riferimenti delle testate bolle accorpate
086500051021     C                   IF        wLenRIF > *zeros
086600050906     C                   IF        wTIPABD = 'S'
086700050614     C                   EVAL      wRIF = %trim(%editc(SQL_vabRMN:'4'))
086800050610     C                   ENDIF
086900050610     C                   IF        wTIPABD = 'A'
087000050610     C                   EVAL      wRIF = %trim(SQL_vabRMA)
087100050610     C                   ENDIF
087200050610     C*
087300050610     C* Gestisco l'allineamento d default a seconda della tipologia d riferimento da utilizzare
087400050610     C                   IF        wAlmRIF = *blanks
087500050610     C                   IF        wTipABD = 'S'
087600050610     C                   EVAL      wAlmRIF = 'D'
087700050610     C                   ENDIF
087800050610     C                   IF        wTipABD = 'A'
087900050610     C                   EVAL      wAlmRIF = 'S'
088000050610     C                   ENDIF
088100050610     C                   ENDIF
088200051021     C*
088300051021     C* Effettuo le operazioni seguenti SOLO in caso in cui il riferimento voluto
088400051021     C* risulti essere effettivamente valorizzato
088500051021     C                   IF        wRIF <> *blanks
088600050614     C*
088700050614     C* Considero la minore tra la lunghezza reale del riferimento da considerare e il
088800050614     C* numero d byte da considerare indicati in tabella x il cliente corrente
088900050614     C                   Z-ADD     *zeros        wByteRIF          2 0
089000050614     C                   IF        wLenRIF <= %len(%trim(wRIF))
089100050614     C                   EVAL      wByteRIF = wLenRIF
089200050614     C                   ELSE
089300050614     C                   EVAL      wByteRIF = %len(%trim(wRIF))
089400050614     C                   ENDIF
089500050610     C*
089600050610     C* A questo punto verifico se sono richiesti solo 1 numero specifico d byte ed il
089700050610     C* relativo allineamento
089800050610     C                   IF        wLenRIF <> 99
089900050610     C                   IF        wAlmRIF = 'S'
090000050614     C                   EVAL      wRIF = %subst(%trim(wRIF):1:wByteRIF)
090100050610     C                   ENDIF
090200050610     C                   IF        wAlmRIF = 'D'
090300050610     C                   EVAL      wRIF = %subst(%trim(wRIF):
090400050614     C                                    %len(%trim(wRIF))-wByteRIF+1)
090500050610     C                   ENDIF
090600050610     C                   ENDIF
090700051021     C*
090800051021     C                   ELSE
090900051021     C                   MOVEL     'N'           wOltreRIF
091000051021     C                   ENDIF
091100050610     C*
091200031114     C* Se c stanno...
091300050610     C                   IF        %len(%trim(wRMO))+1+%len(%trim(wRIF)) <=
091400040517     C                             %len(wRMO)-4
091500050610     C                   EVAL      wRMO = %trim(wRMO) + %trim(wRIF) + '-'
091600050610     C                   MOVEL     'N'           wOltreRIF         1
091700050610     C                   ELSE
091800050610     C                   MOVEL     'S'           wOltreRIF
091900031114     C                   ENDIF
092000050906     C*
092100050906     C                   ELSE
092200050906     C                   MOVEL     'N'           wOltreRIF
092300050906     C                   ENDIF
092400051021     C*
092500050620     C*
092600050620     C* Se richiesto raggruppo anche le note
092700080929     C                   IF        wRgrNOT <> *blanks
092800060726     C* Memorizzo subito note originali
092900080929     C                   IF        wRgrNOT = 'S'
093000060726     C                   IF        wNOT = *blanks AND WRK_vabNOT <> *blanks AND
093100060726     C                             %scan(%trim(WRK_vabNOT):wNT2) = 0
093200060726     C                   EVAL      wNOT = %trim(WRK_vabNOT)
093300060726     C                   ENDIF
093400060726     C                   IF        wNT2 = *blanks AND WRK_vabNT2 <> *blanks AND
093500060726     C                             %scan(%trim(WRK_vabNT2):wNOT) = 0
093600060726     C                   EVAL      wNT2 = %trim(WRK_vabNT2)
093700060726     C                   ENDIF
093800080929     C                   ENDIF
093900080929     C                   IF        wRgrNOT = 'U'
094000080929     C                   IF        wNOTNT2=*blanks AND
094100080929     C                             (WRK_vabNOT<>*blanks OR WRK_vabNT2<>*blanks)
094200080929     C                             AND
094300080929     C                             %scan(%trim(WRK_vabNOT)+%trim(WRK_vabNT2):
094400080929     C                             wNOTNT2)=0
094500080929     C                   EVAL      wNOTNT2=%trim(WRK_vabNOT)+ ' '+
094600080929     C                                     %trim(WRK_vabNT2)
094700080929     C                   ENDIF
094800080929     C                   ENDIF
094900080929     C*
095000080929     C                   IF        wRgrNOT = 'S'
095100060629     C***  VABNOT
095200060726     C* Se presente nota => gestisco
095300060726     C                   IF        SQL_vabNOT <> *blanks
095400080929     C* Innanzitutto verifico che la nota corrente nn sia già presente
095500060726     C                   SETOFF                                       40
095600060726     C                   IF        wNOT <> *blanks
095700060726     C                   IF        %scan(%trim(SQL_vabNOT):wNOT) <> 0 OR
095800060726     C                             %scan(%trim(SQL_vabNOT):wNT2) <> 0
095900060726     C                   SETON                                        40
096000060726     C                   ENDIF
096100060726     C                   ENDIF
096200060629     C* Verifico se lo spazio su NOT è già esaurito mentre quello su NT2 è ancora dispnibile
096300060726     C                   IF        *IN40 = *OFF
096400060629     C                   IF        (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNOT))
096500060629     C                             >%size(wNOT)) AND
096600060629     C                             (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNOT))
096700060629     C                             <=%size(wNT2))
096800060726     C                   IF        %scan(%trim(SQL_vabNOT):wNT2) = 0 AND
096900060726     C                             %scan(%trim(SQL_vabNOT):wNOT) = 0
097000060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNOT)
097100060726     C                   ENDIF
097200060629     C                   ELSE
097300050708     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNOT)
097400060726     C                   ENDIF
097500060726     C                   ENDIF
097600060629     C***  VABNT2
097700060726     C* Se presente nota => gestisco
097800060726     C                   IF        SQL_vabNT2 <> *blanks
097900060629     C* Innanzitutto verifico che il riferimento corrente nn sia già presente
098000060726     C                   SETOFF                                       40
098100060726     C                   IF        wNT2 <> *blanks
098200060726     C                   IF        %scan(%trim(SQL_vabNT2):wNT2) <> 0 OR
098300060726     C                             %scan(%trim(SQL_vabNT2):wNOT) <> 0
098400060726     C                   SETON                                        40
098500060726     C                   ENDIF
098600060726     C                   ENDIF
098700060629     C* Verifico se lo spazio su NT2 è già esaurito mentre quello su NOT è ancora dispnibile
098800060726     C                   IF        *IN40 = *OFF
098900060629     C                   IF        (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNT2))
099000060629     C                             >%size(wNT2)) AND
099100060629     C                             (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNT2))
099200060629     C                             <=%size(wNOT))
099300060726     C                   IF        %scan(%trim(SQL_vabNT2):wNOT) = 0 AND
099400060726     C                             %scan(%trim(SQL_vabNT2):wNT2) = 0
099500060629     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNT2)
099600060726     C                   ENDIF
099700060629     C                   ELSE
099800060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNT2)
099900060629     C                   ENDIF
100000060629     C                   ENDIF
100100060726     C                   ENDIF
100200080929     C                   ENDIF
100300080929     C                   ENDIF
100400080929     C*
100500080929     C                   IF        wRgrNOT = 'U'
100600080929     C
100700080929     C                   IF        (%len(%trim(wNOTNT2))+1+
100800080929     C                              %len(%trim(SQL_vabNOT))+
100900080929     C                              %len(%trim(SQL_vabNT2)))
101000080929     C                             <=%size(wNOTNT2)
101100080929     C                   EVAL      wNOTNT2 = %trim(wNOTNT2)    + ' ' +
101200080929     C                                       %trim(SQL_vabNOT) + ' ' +
101300080929     C                                       %trim(SQL_vabNT2)
101400080929     C                   ENDIF
101500080929     C                   ENDIF
101600060629     C*
101700050620     C                   ELSE
101800050708     C                   EVAL      wNOT = %trim(SQL_vabNOT)
101900050708     C                   EVAL      wNT2 = %trim(SQL_vabNT2)
102000050620     C                   ENDIF
102100031110     C*
102200031110     C                   ENDSR
102300031112     C*------------------------------------------------------------------------*
102400031112     C* WRIVAD - SCRITTIURA FILE FIVAD
102500031112     C*------------------------------------------------------------------------*
102600031112     C     WRIVAD        BEGSR
102700031112     C*
102800031112     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAD)
102900031112     C                   CLEAR                   FIVAD000
103000031112     C*
103100051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAD
103200040712     C     KEYva_1       SETLL     EDIVADQT1L
103300031113     C                   IF        %equal(EDIVADQT1L)
103400040227     C* Eseguo routine x modifica codice trattamento merce
103500040227     C                   EXSR      CHGCTM
103600040227     C*
103700040712     C     KEYva_1       READE     EDIVADQT1L
103800031113     C                   DOW       not %eof(EDIVADQT1L)
103900031112     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
104000031112     C                   EVAL      vadAAS = WRK_vabAAS
104100031112     C                   EVAL      vadLNP = WRK_vabLNP
104200031112     C                   EVAL      vadNRS = WRK_vabNRS
104300031112     C                   EVAL      vadNSP = WRK_vabNSP
104400070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
104500081016     C   60              EVAL      vadNSP = ACC_NSP
104600031112     C* Inserisco record d dettaglio in FIVAD
104700031112     C                   WRITE     FIVAD000
104800031112     C* Elimino record d dettaglio in EDIVAD
104900031113     C                   DELETE    EDIVADQT1L
105000031112     C* Continuo la lettura dei dettagli della testata bolla corrente
105100040712     C     KEYva_1       READE     EDIVADQT1L
105200031112     C                   ENDDO
105300031112     C*
105400031112     C* Se invece nn esiste già il dettaglio colli in EDIVAD e sulla testata c'è l'indicazioine
105500031112     C* dei segnacolli => provvedo a creare il VAD
105600031112     C                   ELSE
105700031112     C                   IF        SQL_vabNCD > *zeros OR
105800031112     C                             SQL_vabNCA > *zeros
105900040227     C* Eseguo routine x modifica codice trattamento merce
106000040227     C                   EXSR      CHGCTM
106100031112     C* Valorizzo la chiave bolla del dettaglio con quella della relativa testata bolla "master"
106200031112     C                   EVAL      vadAAS = WRK_vabAAS
106300031112     C                   EVAL      vadLNP = WRK_vabLNP
106400031112     C                   EVAL      vadNRS = WRK_vabNRS
106500031112     C                   EVAL      vadNSP = WRK_vabNSP
106600031112     C* Valorizzo i restanti campi con quelli della testata bolla corrente
106700031112     C                   EVAL      vadFGS = SQL_vabFGS
106800031112     C                   EVAL      vadCCM = SQL_vabCCM
106900031112     C                   EVAL      vadNCL = SQL_vabNCL
107000031112     C                   EVAL      vadNCD = SQL_vabNCD
107100031112     C                   EVAL      vadNCA = SQL_vabNCA
107200070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
107300081016     C   60              EVAL      vadNSP = ACC_NSP
107400031112     C* Inserisco record d dettaglio in FIVAD
107500171220     C                   WRITE     FIVAD000
107600031112     C                   ENDIF
107700031112     C                   ENDIF
107800031112     C*
107900031112     C                   ENDSR
107901171219     C*------------------------------------------------------------------------*
107902171219     C* WRIVAD10 - SCRITTIURA FILE FIVAD
107903171219     C*------------------------------------------------------------------------*
107904171219     C     WRIVAD10      BEGSR
107905171219     C*
107906171219     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAD)
107907171219     C                   CLEAR                   FIVAD100
107908171219     C*
107909171219     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAD
107910171219     C     KEYva_1       SETLL     EDIVADQ11L
107911171219     C                   IF        %equal(EDIVADQ11L)
107912180208     C* Non eseguo routine x modifica codice trattamento merce: se ho il vad10 il cliente
107913180208     c* DEVE indicare il ctm. Se non c'è '2D' non andrebbe bene e in ogni caso se ho il vad10
107914180208     c* il traduttore imposta il CTM apposito in automatico
107915180208     C******             EXSR      CHGCTM
107916171219     C*
107917171219     C     KEYva_1       READE     EDIVADQ11L
107918171219     C                   DOW       not %eof(EDIVADQ11L)
107919171219     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
107920171219     C                   EVAL      vadAAS = WRK_vabAAS
107921171219     C                   EVAL      vadLNP = WRK_vabLNP
107922171219     C                   EVAL      vadNRS = WRK_vabNRS
107923171219     C                   EVAL      vadNSP = WRK_vabNSP
107924171219     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
107925171219     C   60              EVAL      vadNSP = ACC_NSP
107926171219     C* Inserisco record d dettaglio in FIVAD
107927171219     C                   WRITE     FIVAD100
107928171219     C* Elimino record d dettaglio in EDIVAD
107929171219     C                   DELETE    EDIVADQ11L
107930171219     C* Continuo la lettura dei dettagli della testata bolla corrente
107931171219     C     KEYva_1       READE     EDIVADQ11L
107932171219     C                   ENDDO
107933171219     C*
107955171219     C                   ENDIF
107956171219     C*
107957171219     C                   ENDSR
108000031114     C*------------------------------------------------------------------------*
108100031114     C* WRIVAT - SCRITTIURA FILE FIVAT
108200031114     C*------------------------------------------------------------------------*
108300031114     C     WRIVAT        BEGSR
108400031114     C*
108500031114     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAT)
108600031114     C                   CLEAR                   FIVAT000
108700031114     C*
108800051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAT
108900040712     C     KEYva_1       SETLL     EDIVATQT1L
109000031114     C                   IF        %equal(EDIVATQT1L)
109100040227     C* Eseguo routine x modifica codice trattamento merce
109200040227     C                   EXSR      CHGCTM
109300040227     C*
109400040712     C     KEYva_1       READE     EDIVATQT1L
109500031114     C                   DOW       not %eof(EDIVATQT1L)
109600031114     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
109700031114     C                   EVAL      vatAAS = WRK_vabAAS
109800031114     C                   EVAL      vatLNP = WRK_vabLNP
109900031114     C                   EVAL      vatNRS = WRK_vabNRS
110000031114     C                   EVAL      vatNSP = WRK_vabNSP
110100070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
110200081016     C   60              EVAL      vatNSP = ACC_NSP
110300031114     C* Inserisco record d dettaglio in FIVAT
110400031114     C                   WRITE     FIVAT000
110500031114     C* Elimino record d dettaglio in EDIVAT
110600031114     C                   DELETE    EDIVATQT1L
110700031114     C* Continuo la lettura dei dettagli della testata bolla corrente
110800040712     C     KEYva_1       READE     EDIVATQT1L
110900031114     C                   ENDDO
111000031114     C                   ENDIF
111100031114     C*
111200031114     C                   ENDSR
111300070807     C*------------------------------------------------------------------------*
111400070807     C* WRIVAX - SCRITTIURA FILE FIVAX
111500070807     C*------------------------------------------------------------------------*
111600070807     C     WRIVAX        BEGSR
111700070809     C*
111800070809     C* Solo x record VAX diversi dalla bolla master (accorpante)
111900070809     C                   IF        SQL_vabFGS  = WRK_vabFGS AND
112000070809     C                             SQL_vabCCM  = WRK_vabCCM AND
112100070809     C                             SQL_vabAAS  = WRK_vabAAS AND
112200070809     C                             SQL_vabLNP  = WRK_vabLNP AND
112300070809     C                             SQL_vabNRS  = WRK_vabNRS AND
112400070809     C                             SQL_vabNSP <> WRK_vabNSP
112500070807     C*
112600070807     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAX)
112700070807     C                   CLEAR                   FIVAX000
112800070807     C*
112900070807     C                   Z-ADD     *zeros        wVAXmaster
113000071203     C                   Z-ADD     *zeros        wVAXmaster01
113100071203     C                   Z-ADD     *zeros        wVAXmaster02
113200070807     C*
113300070807     C* Effettuo conteggio righe attuali packing list della bolla master
113400070807     C*
113500070807     C/EXEC SQL
113600070905     C+ declare C1VX cursor for
113700070906     C+ select max(vaxprr) from edivaxqt
113800070911     C+ where
113900070911     C+ vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
114000070807     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
114100070807     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
114200070807     C+ for fetch only
114300070807     C/END-EXEC
114400070807     C
114500070807     C/EXEC SQL
114600070905     C+ open C1VX
114700070807     C/END-EXEC
114800070807     C
114900070807     C/EXEC SQL
115000070905     C+ Fetch C1VX into :wVAXmaster01
115100070807     C/END-EXEC
115200070905     C*
115300070905     C/EXEC SQL
115400070905     C+ declare C2VX cursor for
115500070905     C+ select max(vaxprr) from fivax00f
115600070911     C+ where
115700070911     C+ vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
115800070905     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
115900070905     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
116000070905     C+ for fetch only
116100070905     C/END-EXEC
116200070905     C
116300070905     C/EXEC SQL
116400070905     C+ open C2VX
116500070905     C/END-EXEC
116600070905     C
116700070905     C/EXEC SQL
116800070905     C+ Fetch C2VX into :wVAXmaster02
116900070905     C/END-EXEC
117000070809     C*
117100070905     C* Tengo il maggiore progressivo riga tra EDIVAX e FIVAX
117200070905     C                   EVAL      wVAXmaster = wVAXmaster01
117300070905     C                   IF        wVAXmaster02 > wVAXmaster01
117400070905     C                   EVAL      wVAXmaster = wVAXmaster02
117500070905     C                   ENDIF
117600070905     C*
117700070809     C* Proseguo elaborazione solo se effettivamente trovati VAX da accorpare
117800070905     C                   IF        wVAXmaster > *zeros
117900070807     C*
118000070807     C* Verifico se x la bolla corrente di EDIVAB esiste il relativo packing list in EDIVAX
118100070807     C     KEYva_1       SETLL     EDIVAXQT1L
118200070807     C                   IF        %equal(EDIVAXQT1L)
118300070809     C*
118400070809     C                   SETOFF                                       88
118500070807     C*
118600070807     C     KEYva_1       READE     EDIVAXQT1L
118700070807     C                   DOW       not %eof(EDIVAXQT1L)
118800070809     C*
118900070809     C* Se nn ancora gestito il salto pagina fittizio forzato
119000070809     C                   IF        *IN88 = *off
119100070809     C                   SETON                                        88
119200070905     C***                EXSR      GESFORMFEED
119300070809     C                   ENDIF
119400070809     C*
119500070807     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
119600070807     C                   EVAL      vaxAAS = WRK_vabAAS
119700070807     C                   EVAL      vaxLNP = WRK_vabLNP
119800070807     C                   EVAL      vaxNRS = WRK_vabNRS
119900070807     C                   EVAL      vaxNSP = WRK_vabNSP
120000070807     C* Auto-incremento numero progressivo riga packing list bolla master + bolla slave corrente
120100070905     C***                EVAL      vaxPRR = vaxPRR + wVAXmaster + wInsRighe
120200070905     C                   EVAL      vaxPRR = vaxPRR + wVAXmaster
120300070807     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
120400081016     C   60              EVAL      vaxNSP = ACC_NSP
120500070807     C* Inserisco record d dettaglio in FIVAX
120600070807     C                   WRITE     FIVAX000
120700070807     C* Elimino record d dettaglio in EDIVAX
120800070807     C                   DELETE    EDIVAXQT1L
120900070807     C* Continuo la lettura dei dettagli della testata bolla corrente
121000070807     C     KEYva_1       READE     EDIVAXQT1L
121100070807     C                   ENDDO
121200070807     C                   ENDIF
121300070807     C*
121400070809     C                   ENDIF
121500070809     C*
121600070807     C/EXEC SQL
121700070905     C+ close C1VX
121800070807     C/END-EXEC
121900070905     C*
122000070905     C/EXEC SQL
122100070905     C+ close C2VX
122200070905     C/END-EXEC
122300070905     C*
122400070809     C                   ENDIF
122500070807     C*
122600070807     C                   ENDSR
122700070809     C*------------------------------------------------------------------------*
122800070809     C* GESFORMFEED - GESTIONE FORZATURA SALTO PAGINA FITTIZIO
122900070809     C*------------------------------------------------------------------------*
123000070809     C     GESFORMFEED   BEGSR
123100070809     C*
123200070809     C* Salvo il formato record corrente
123300070809     C                   EVAL      DSEDIVAXSAV = DSEDIVAX
123400070809     C*
123500070809     C* Effettuo considerazioni x simulazione forzata salto pagina (ma solo se conviene davvero)
123600070809     C                   Z-ADD     *zeros        wRigheVuote       2 0
123700070809     C                   EVAL      wRigheVuote = wVAXmaster -
123800070809     C                             (42 * %div(wVAXmaster:42))
123900070809     C                   IF        wRigheVuote > *zeros
124000070809     C                   EVAL      wRigheVuote =
124100070809     C                             (42 * (%div(wVAXmaster:42)+1)) -
124200070809     C                             wVAXmaster
124300070809     C                   IF        wRigheVuote < 21
124400070809     C                   Z-ADD     wRigheVuote   wInsRighe         2 0
124500070809     C                   ELSE
124600070809     C                   Z-ADD     3             wInsRighe
124700070809     C                   ENDIF
124800070809     C*
124900070809     C                   Z-ADD     1             wRigaVuota        2 0
125000070809     C                   DOW       wRigaVuota <= wInsRighe
125100070809     C*
125200070809     C* Inserisco riga vuota sulla bolla master (accorpante) fino a capienza ultima pagina
125300070809     C                   EVAL      vaxAAS = WRK_vabAAS
125400070809     C                   EVAL      vaxLNP = WRK_vabLNP
125500070809     C                   EVAL      vaxNRS = WRK_vabNRS
125600070809     C                   EVAL      vaxNSP = WRK_vabNSP
125700070809     C                   EVAL      vaxPRR = wVAXmaster + wRigaVuota
125800070809     C                   EVAL      vaxDTT = *blanks
125900070809     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
126000081016     C   60              EVAL      vaxNSP = ACC_NSP
126100070809     C* Inserisco record d dettaglio in FIVAX
126200070809     C                   WRITE     FIVAX000
126300070809     C*
126400070809     C                   ADD       1             wRigaVuota
126500070809     C                   ENDDO
126600070809     C                   ENDIF
126700070809     C*
126800070809     C* Ripristino il formato record corrente
126900070809     C                   EVAL      DSEDIVAX = DSEDIVAXSAV
127000070809     C*
127100070809     C                   ENDSR
127200031112     C*------------------------------------------------------------------------*
127300031112     C* WRIVAB - SCRITTIURA FILE FIVAB X BOLLA "MASTER" ACCORPATA
127400031112     C*------------------------------------------------------------------------*
127500031112     C     WRIVAB        BEGSR
127600031112     C*
127700031112     C* Innanzitutto svuoto il buffer del record d testata d output (FIVAB)
127800031112     C                   CLEAR                   FIVAB000
127900031112     C* Valorizzo il buffer del record d testata d output (FIVAB)
128000031112     C                   EVAL      DSEDIVABOUT = DSEDIVABW
128100070420     C*
128200070420     C* Procedo solo se buffer effettivamente valorizzato
128300070420     C                   IF        DSEDIVABOUT <> DSEDIVABINZ
128400070420     C*
128500031112     C* Resetto alcuni campi che nella bolla "master" occcorrono NN valorizzati
128600031113     C   55              CLEAR                   vabNCD
128700031113     C   55              CLEAR                   vabNCA
128800080226     C   54
128900080226     CAN 55              CLEAR                   vabRMO
129000031114     C   55              CLEAR                   vabCMO
129100031114     C   55              CLEAR                   vabNMO
129200050620     C                   IF        *IN55 = *on
129300050620     C* Campo RIFERIMENTO MITTENTE
129400050701     C                   IF        wRMO <> *blanks
129500060530     C                   Z-ADD     wGiro         wGiro2
129600060530     C                   EVAL      vabRMO = 'N'+%editw(wGiro2:'  ')+':'+
129700040303     C                                      %trim(wRMO)
129800050610     C                   IF        wGiro > 1 AND wOltreRIF = 'S'
129900131007     C                   MOVE      '..'          vabRMO
130000040303     C                   ENDIF
130100050701     C                   ENDIF
130200080929     C* Campi NOTE
130300080929     C*
130400080929     C* ..gestisco tipo raggruppamento note
130500080929     C                   IF        wRgrNOT = 'U'
130600080929     C   51              EVAL      vabNOT = %subst(wNOTNT2:01:35)
130700080929     C   51              EVAL      vabNT2 = %subst(wNOTNT2:36:35)
130800080929     C                   ELSE
130900060726     C   51              EVAL      vabNOT = %trim(wNOT)
131000060726     C   51              EVAL      vabNT2 = %trim(wNT2)
131100080929     C                   ENDIF
131200080929     C*
131300040303     C                   ENDIF
131400070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
131500081016     C   60              EVAL      vabNSP = ACC_NSP
131600031112     C* Inserisco record d testata in FIVAB
131700031112     C                   WRITE     FIVAB000
131800070420     C                   ENDIF
131900050620     C*
132000050620     C* Inizializzo indicatori d wrk
132100050701     C                   SETOFF                                       51
132200031112     C*
132300031112     C                   ENDSR
132400070227     C*------------------------------------------------------------------------*
132500040709     C* CHKREC - CONTROLLO CORRETTEZZA RECORD TESTATA BOLLE AI FINI DEL RAGGRUPPAMENTO
132600040709     C*------------------------------------------------------------------------*
132700040709     C     CHKREC        BEGSR
132800040709     C*
132900040709     C* Inizializzo il flag d correttezza record testata bolle
133000040709     C                   MOVEL     'S'           wRECOK            1
133100051215     C*
133200051215     C* Verifica se bolla corrente presenta particolarità consegna RC e cliente nn abilitato
133300051215     C* specificatamente all'accorpamento anche con part. consegna RC
133400051215     C                   IF        wRECOK = 'S'
133500051216     C                   IF        SQL_vabGMA = 'RC'
133600051216     C                   IF        wAccRC <> 'S'
133700051215     C                   MOVEL     'N'           wRECOK
133800051216     C                   ELSE
133900051216     C* Se abilitazione ok controllo subito anche l'ORM relativo (quello che poi sarà aggiornato)
134000051216     C                   CLEAR                   wRC_KEY
134100051216     C                   EVAL      wRC_AAS = SQL_vabAAS
134200051216     C                   EVAL      wRC_LNP = SQL_vabLNP
134300051216     C                   EVAL      wRC_NRS = SQL_vabNRS
134400051216     C                   EVAL      wRC_NSP = SQL_vabNSP
134500051216     C                   CLEAR                   FNLS24DS
134600051216     C                   EVAL      I24TLA  = *blanks
134700051216     C                   EVAL      I24OPE  = 'L'
134800051216     C                   EVAL      I24KSC  = SQL_vabCCM
134900051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
135000051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
135100051216     C                   CALL      'FNLS24R'
135200051216     C                   PARM                    FNLS24DS
135300051216     C* Se tutto ok procedo altrimenti skippo la bolla corrente con particolarità RC ai fini
135400051216     C* dell'accorpamento
135500051216     C                   IF        O24ERR <> *zeros
135600051216     C                   MOVEL     'N'           wRECOK
135700051216     C                   ENDIF
135800051215     C                   ENDIF
135900051216     C                   ENDIF
136000051215     C                   ENDIF
136100040709     C*
136200040709     C* Verifica codice trattamento
136300051215     C                   IF        wRECOK = 'S'
136400040709     C                   EXSR      CHKCTM
136500051215     C                   ENDIF
136600040709     C*
136700040709     C                   ENDSR
136800040709     C*------------------------------------------------------------------------*
136900040709     C* CHKCTM - CONTROLLO CODICE TRATTAMENTO MERCE
137000040709     C*------------------------------------------------------------------------*
137100040709     C     CHKCTM        BEGSR
137200040709     C*
137300040709     C* Verifico che il codice trattamento merce della bolla corrente nn preveda il VAX
137400040709     C                   EVAL      tblKUT = 1
137500040709     C                   EVAL      tblCOD = '1B'
137600040709     C                   EVAL      tblKEY = SQL_vabCTM
137700040709     C     KEYtab        CHAIN     TABEL00F
137800040709     C                   IF        %found(TABEL00F)
137900040709     C                   EVAL      DS1B = tblUNI
138000070809     C                   IF        §1BF16 = 'S' AND wAccVX <> 'S'
138100040709     C                   MOVEL     'N'           wRECOK
138200040709     C                   ENDIF
138300040709     C                   ENDIF
138400040709     C*
138500040709     C                   ENDSR
138600040227     C*------------------------------------------------------------------------*
138700040227     C* CHGCTM - CAMBIO CODICE TRATTAMENTO MERCE
138800040227     C*------------------------------------------------------------------------*
138900040227     C     CHGCTM        BEGSR
139000040227     C*
139100040227     C* Se sulla testata bolle NN è espressamente indicato il codice tratamento merce
139200040227     C* (e naturalmente o c'è il dettaglio colli o l'indicazione in testata dei segnacolli
139300040227     C* sequenziali) imposto come default il CTM = '2D'
139400040227     C                   IF        WRK_vabCTM = *blanks
139500040227     C                   EVAL      WRK_vabCTM = '2D'
139600040227     C                   ENDIF
139700040227     C*
139800040227     C* Valorizzo la chiave x reperire le informazioni relative al codice
139900040227     C* trattamento merce da impostare in caso d raggruppamento bolle
140000040227     C                   EVAL      tblKUT = 1
140100040227     C                   EVAL      tblCOD = '1B'
140200040227     C                   EVAL      tblKEY = WRK_vabCTM
140300040227     C     KEYtab        CHAIN     TABEL00F
140400040227     C                   IF        %found(TABEL00F)
140500040227     C                   EVAL      DS1B = tblUNI
140600040227     C                   IF        §1BCTR <> *blanks
140700040227     C                   EVAL      WRK_vabCTM = §1BCTR
140800040227     C                   ENDIF
140900040227     C                   ENDIF
141000040227     C*
141100040227     C                   ENDSR
141200050610     C*------------------------------------------------------------------------*
141300051021     C* REPABD - REPERISCE I DATI TABELLATI CHE PILOTANO LE MODALITA DI ACCORPAMENTO
141400050610     C*------------------------------------------------------------------------*
141500050610     C     REPABD        BEGSR
141600050610     C*
141700050610     C                   EVAL      tblKUT = 1
141800050610     C                   EVAL      tblCOD = '3C'
141900050708     C                   MOVEL(P)  D86CCM        tblKEY
142000050610     C     KEYtab        CHAIN     TABEL00F
142100050610     C                   IF        %found(TABEL00F)
142200050610     C                   EVAL      DS3C = tblUNI
142300050610     C                   MOVEL     §3CABD        wTipABD
142400050610     C                   Z-ADD     §3CLRM        wLenRIF
142500050610     C                   MOVEL     §3CALM        wAlmRIF
142600050620     C                   MOVEL     §3CNOT        wRgrNOT
142700051021     C                   MOVEL     §3CABC        wAccCAS
142800051215     C                   MOVEL     §3CARC        wAccRC
142900070809     C                   MOVEL     §3CAVX        wAccVX
143000081013     C                   MOVEL     §3CMVR        wMaxVB
143100050610     C                   ENDIF
143200080226     C*
143300080226     C* Se lunghezza riferimento è zero accendo indicatore preposto x nn sovrascrittura RMO
143400080226     C                   if        wLenRIF <> *zeros
143500080226     C                   seton                                        54
143600080226     C                   endif
143700070228     C*
143800070228     C* Se cliente che necessita ritorno informazioni d accorpamento => accendo indicatore preposto
143900081006     C                   if        wTipABD <> *blanks
144000070228     C                   seton                                        60
144100070228     C                   else
144200070228     C                   setoff                                       60
144300070228     C                   endif
144400050610     C*
144500050610     C                   ENDSR
144600081016     C*------------------------------------------------------------------------*
144700081016     C* REPNUM - STACCA NUMERATORE X MANTENIMENTO NUMERO SPEDIZIONE
144800081016     C*------------------------------------------------------------------------*
144900081016     C     REPNUM        BEGSR
145000081016     C*
145100081016     C                   z-add     SQL_VABNSP    ACC_NSP
145200081016     C*
145300081016     C                   ENDSR
145400070227     C*------------------------------------------------------------------------*
145500070227     C* WRIRAB - SCRITTURA FILE RACCORDO BOLLE CLIENTE "ACCORPANTI"/"ACCORPATE"
145600070227     C*------------------------------------------------------------------------*
145700070227     C     WRIRAB        BEGSR
145800081013     C*
145900081013     C     KEYrab02_C    chain     firab02l
146000081013     C                   if        %found(firab02l)
146100081013     C                   delete    firab000
146200081013     C                   endif
146300081013     C*
146400081013     C                   clear                   FIRAB000
146500070227     C*
146600070227     C* Chiave spedizione
146700081017     C                   eval      rabFGS  = SQL_vabFGS
146800070227     C                   eval      rabAAS  = SQL_vabAAS
146900070227     C                   eval      rabLNP  = SQL_vabLNP
147000070227     C                   eval      rabNRS  = SQL_vabNRS
147100081016     C                   eval      rabNSP  = ACC_NSP
147200070227     C                   eval      rabCCM  = SQL_vabCCM
147300070227     C* Riferimenti cliente "accorpanti"
147400070227     C                   eval      rabRMN  = WRK_vabRMN
147500070227     C                   eval      rabRMA  = WRK_vabRMA
147600070227     C* Riferimenti cliente "accorpati"
147700070227     C                   eval      rabRMNR = SQL_vabRMN
147800070227     C                   eval      rabRMAR = SQL_vabRMA
147900070228     C* Attributi
148000070228     C                   eval      rabDAT  = datcor
148100070227     C*
148200070301     C                   write     FIRAB000
148300070227     C*
148400070227     C                   ENDSR
148500970526     C*------------------------------------------------------------------------*
148600970526     C* *INZSR - OPERAZIONI INIZIALI
148700970526     C*------------------------------------------------------------------------*
148800970526     C     *INZSR        BEGSR
148900040714     C*
149000040714     C*  Reperisco parametri in entrata
149100040714     C     *ENTRY        PLIST
149200070420     C                   PARM                    KPJBA
149300040714     C                   PARM                    TRTC86DS
149400070419     C*
149500080226     C                   SETOFF                                       6054
149600060726     C*
149700031113     C*
149800031110     C*---
149900031110     C* CHIAVI DI LETTURA
150000031110     C*---
150100040709     C* CHIAVE DI LETTURA X EDIVABQT1L - Completa
150200040709     C     KEYvab1       KLIST
150300040709     C                   KFLD                    SQL_vabCCM
150400040709     C                   KFLD                    SQL_vabCMR
150500040709     C                   KFLD                    SQL_vabCNT
150600040709     C                   KFLD                    SQL_vabAAS
150700040709     C                   KFLD                    SQL_vabLNP
150800040709     C                   KFLD                    SQL_vabNRS
150900040709     C                   KFLD                    SQL_vabNSP
151000070807     C* CHIAVE DI LETTURA X EDIVADQT1L/EDIVATQT1L/EDIVAXQT1L
151100040712     C     KEYva_1       KLIST
151200031112     C                   KFLD                    SQL_vabFGS
151300031112     C                   KFLD                    SQL_vabCCM
151400031112     C                   KFLD                    SQL_vabCMR
151500031112     C                   KFLD                    SQL_vabCNT
151600031112     C                   KFLD                    SQL_vabAAS
151700031112     C                   KFLD                    SQL_vabLNP
151800031112     C                   KFLD                    SQL_vabNRS
151900031112     C                   KFLD                    SQL_vabNSP
152000040227     C* CHIAVE DI LETTURA X TABEL00F - Completa
152100040227     C     KEYtab        KLIST
152200040227     C                   KFLD                    tblKUT
152300040227     C                   KFLD                    tblCOD
152400040227     C                   KFLD                    tblKEY
152500081013     C* CHIAVE DI LETTURA X FIRAB02L - Completa
152600081013     C     KEYrab02_C    KLIST
152700081017     C                   KFLD                    SQL_vabFGS
152800081017     C                   KFLD                    SQL_vabCCM
152900081013     C                   KFLD                    SQL_vabAAS
153000081013     C                   KFLD                    SQL_vabLNP
153100081013     C                   KFLD                    SQL_vabNRS
153200081016     C                   KFLD                    ACC_NSP
153300081013     C                   KFLD                    WRK_vabRMN
153400081013     C                   KFLD                    WRK_vabRMA
153500081016     C                   KFLD                    SQL_vabRMN
153600081016     C                   KFLD                    SQL_vabRMA
153700081016     C*
153800041116     C*  Richiamo XPARUT
153900041116     C                   Z-ADD     1             CODUT             1 0
154000041116     C                   CALL      'X§PARUT'
154100041116     C                   PARM                    UT§DSE
154200041116     C                   MOVEL     REC80         CNCR80
154300041116     c
154400041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
154500041116     C                   CLEAR                   TRUL06DS
154600041116     C                   MOVE      '£1'          D06COD
154700041116     C                   MOVEL     SIMFEL        D06KEY
154800041116     C                   MOVEL     'L'           D06TLA
154900041116     C                   MOVEL     TRUL06DS      KPJBU
155000041116      *
155100041116     C                   CALL      'TRUL06R'
155200041116     C                   PARM                    KPJBA
155300041116     C                   MOVEL     KPJBU         TRUL06DS
155400070228     C*
155500070228     C* CALCOLA LA DATA CORRENTE
155600070228     C                   time                    wn14             14 0
155700070228     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
155800070228     C                   z-add     wn8           g08dat
155900070228     C                   z-add     *zeros        g08inv
156000070228     C                   movel     '0'           g08err
156100070228     C                   call      'XSRDA8'
156200070228     C                   parm                    wlbda8
156300070228     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
156400991112     C*
156500991112     C                   ENDSR
