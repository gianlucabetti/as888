000100010925      *PARMS DYNUSRPRF(*OWNER)
000200000707     H dftactgrp(*yes)
000300970526     H*------------------------------------------------------------------------*
000400031110     H* Raggruppamento bolle x stesso destinatario da EDIVAB a FIVAB
000500970526     H*------------------------------------------------------------------------*
000600970526     H DECEDIT('0,') DATEDIT(*DMY.)
000700970526     F*------------------------------------------------------------------------*
000800970526     F* DATA BASE
000900970526     F*------------------------------------------------------------------------*
001000040712     FEDIVABQT1LUF   E           K DISK    USROPN
001100040712     FEDIVADQT1LUF   E           K DISK    USROPN
001200040712     FEDIVATQT1LUF   E           K DISK    USROPN
001300040714     FEDIVAX2L  UF   E           K DISK
001400031110     FFIVAB00F  O    E             DISK
001500031110     FFIVAD00F  O    E             DISK
001600031114     FFIVAT00F  O    E             DISK
001700040714     FFIVAX00F  O    E             DISK
001800040227     FTABEL00F  IF   E           K DISK
001900970526     D*------------------------------------------------------------------------*
002000970526     D* SCHIERE
002100970526     D*------------------------------------------------------------------------*
002200991112     D*------------
002300991112     D* ARCHITETTURA
002400991112     D*------------
002500991112     D KPJBA         E DS
002600041116     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002700041116     D CNCR80        E DS
002800991110     D*------------
002900031110     D* DS D PROCEDURA
003000991110     D*------------
003100031110     D TRTC86DS      E DS
003200040227     D DS1B          E DS
003300050610     D DS3C          E DS
003400051021     D DSEDIVAB      E DS                  EXTNAME(EDIVAB0F) PREFIX(SQL_)
003500031110     D DSEDIVABW     E DS                  EXTNAME(EDIVAB0F) PREFIX(WRK_)
003600031112     D DSEDIVABOUT   E DS                  EXTNAME(EDIVAB0F)
003700051215     D FNLS24DS      E DS
003800060126     d Fnls25ds      e ds
003900041116     D TRUL06DS      E DS
004000041116     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
004100031110     D*------------
004200031110     D* DS D WRK
004300031110     D*------------
004400051021     D  DSEDIABD       DS
004500031114     D   S_vabCCM                          LIKE(SQL_vabCCM)
004600031110     D   S_vabRSD                          LIKE(SQL_vabRSD)
004700031110     D   S_vabRD2                          LIKE(SQL_vabRD2)
004800031110     D   S_vabIND                          LIKE(SQL_vabIND)
004900031110     D   S_vabCAD                          LIKE(SQL_vabCAD)
005000031110     D   S_vabLOD                          LIKE(SQL_vabLOD)
005100031110     D   S_vabPRD                          LIKE(SQL_vabPRD)
005200031110     D   S_vabNZD                          LIKE(SQL_vabNZD)
005300031110     D   S_vabLNA                          LIKE(SQL_vabLNA)
005400031110     D   S_vabZNC                          LIKE(SQL_vabZNC)
005500031110     D   S_vabCBO                          LIKE(SQL_vabCBO)
005600031110     D   S_vabTSP                          LIKE(SQL_vabTSP)
005700031110     D   S_vabCTR                          LIKE(SQL_vabCTR)
005800031110     D   S_vabGC1                          LIKE(SQL_vabGC1)
005900031110     D   S_vabGC2                          LIKE(SQL_vabGC2)
006000031110     D   S_vabFFD                          LIKE(SQL_vabFFD)
006100031110     D   S_vabDCR                          LIKE(SQL_vabDCR)
006200031110     D   S_vabTCR                          LIKE(SQL_vabTCR)
006300031110     D   S_vabHCR                          LIKE(SQL_vabHCR)
006400031110     D   S_vabGMA                          LIKE(SQL_vabGMA)
006500031110     D   S_vabGGA                          LIKE(SQL_vabGGA)
006600031110     D   S_vabGVA                          LIKE(SQL_vabGVA)
006700031110     D   S_vabTC1                          LIKE(SQL_vabTC1)
006800031110     D   S_vabTC2                          LIKE(SQL_vabTC2)
006900031110     D   S_vabVAS                          LIKE(SQL_vabVAS)
007000051021     D   S_vabVAD                          LIKE(SQL_vabVAD)
007100051021     D*
007200031110     D                 DS
007300031110     D   ANTTIP1                      2  0
007400031110     D   ANTNUM1                      2  0
007500031110     D   ANTTIP2                      2  0
007600031110     D   ANTNUM2                      2  0
007700031110     D  DSVABANT               1      8  0
007800051215     D*------------------
007900051215     D* DS RIDEFINIZIONE CHIAVE BOLLA ALFA
008000051215     D*------------------
008100051215     D                 DS                  INZ
008200051215     D  wRC_AAS                1      4  0
008300051215     D  wRC_LNP                5      7  0
008400051215     D  wRC_NRS                8      9  0
008500051215     D  wRC_NSP               10     16  0
008600051215     D  wRC_KEY                1     16
008700051215     D*
008800031110     D*------------
008900031110     D* VARIABILI D WRK
009000031110     D*------------
009100051021     D SAVEDIABD       S                   LIKE(DSEDIABD)
009200051021     D SAV_vabCAS      S                   LIKE(SQL_vabCAS)
009300051021     D SAV_vabVCA      S                   LIKE(SQL_vabVCA)
009400051021     D SAV_vabTIC      S                   LIKE(SQL_vabTIC)
009500060530     D wGiro           S              3  0
009600060530     D wGiro2          S              2  0
009700031110     D wANTTIP1        S                   LIKE(ANTTIP1)
009800031110     D wANTNUM1        S                   LIKE(ANTNUM1)
009900031110     D wANTTIP2        S                   LIKE(ANTTIP2)
010000031110     D wANTNUM2        S                   LIKE(ANTNUM2)
010100050610     D wRIF            S             99    VARYING
010200031114     D wRMO            S                   LIKE(VABRMO)
010300050620     D wNOT            S                   LIKE(VABNOT)
010400050620     D wNT2            S                   LIKE(VABNT2)
010500051215     D wFlgOrmRC       S              1N   INZ(*off)
010600050610     D*------------
010700050620     D* VARIABILE RELATIVE AL RAGGRUPPAMENTO BOLLE
010800050610     D*------------
010900050610     D wTipABD         S                   like(§3CABD)
011000050610     D wLenRIF         S                   like(§3CLRM)
011100050610     D wAlmRIF         S                   like(§3CALM)
011200050620     D wRgrNOT         S                   like(§3CNOT)
011300051021     D wAccCAS         S                   like(§3CABC)
011400051215     D wAccRC          S                   like(§3CARC)
011500000629
011600000707
011700970526     C*------------------------------------------------------------------------*
011800970526     C* MAIN LINES
011900970526     C*------------------------------------------------------------------------*
012000050708     C*
012100050708     C* Reperisce quale riferimenti mittente mantenere durante l'accorpamento
012200050708     C                   exsr      repABD
012300031110     C*
012400031110     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
012500031110     C                   exsr      exeABD
012600051216     C*
012700051216     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
012800051216     C* x gestione discorso particolarità consegna 'RC'
012900051216     C                   exsr      exeABDRC
013000031110     C*
013100031110     C* Eseguo routine d scrittura del FIVAB/FIVAD x bolle NN raggruppabili
013200031110     C                   exsr      exeNOABD
013300991112     C*
013400970526     C* FINE PGM
013500031110     C                   seton                                        LR
013600991112     C*------------------------------------------------------------------------*
013700031110     C* EXEABD - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
013800991112     C*------------------------------------------------------------------------*
013900031110     C     EXEABD        BEGSR
014000050620     C*
014100050620     C* Inizializzo indicatori d wrk
014200050620     C                   setoff                                       51
014300051216     C                   setoff                                       52
014400040709     C*
014500040712     C* Innanzitutto apro i file EDI*QT temporanei
014600040709     C                   open      EDIVABQT1L
014700040712     C                   open      EDIVADQT1L
014800040712     C                   open      EDIVATQT1L
014900031110     C*
015000031110     C* Inizializzazioni
015100031110     C                   clear                   DSEDIVAB
015200031110     C                   clear                   DSEDIABD
015300031110     C                   clear                   SAVEDIABD
015400031110     C                   exsr      exeINZ
015500031114     C                   eval      wGiro = *zeros
015600031113     C                   seton                                        55
015700000707     C*
015800000707     C/EXEC SQL
015900051216     C+ declare C1 cursor for select * from edivabqt
016000051021     C*+ where vabCAS = 0 and vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
016100051215     C*+ where vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
016200051216     C*+ where vabATB = ' ' and vabxco=' '
016300051216     C+ where vabGMA <> 'RC' and vabGMA <> '  ' and vabATB = ' ' and vabxco=' '
016400031114     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
016500031110     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
016600031110     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
016700031110     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
016800051021     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
016900000707     C+ for fetch only
017000000707     C/END-EXEC
017100000707     C
017200000707     C/EXEC SQL
017300051216     C+ open C1
017400000707     C/END-EXEC
017500000707     C
017600000707     C/EXEC SQL
017700051216     C+ Fetch C1 into :DSEDIVAB
017800000707     C/END-EXEC
017900000707     C*
018000031110     C                   dow       sqlcod = *zeros
018100040709     C*
018200040709     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
018300051021     C                   exsr      CHKREC
018400040709     C*
018500040709     C* Se ok proseguo
018600040709     C                   if        wRECOK = 'S'
018700031110     C*
018800031110     C* Eseguo routine d valorizzazione DS d livello rottura
018900031110     C                   exsr      valDSROT
019000051216     C*
019100051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
019200051216     C                   z-add     *zeros        wFlgErr           1 0
019300051021     C*
019400051215     C* Verifico avvenuta rottura d codice
019500051021     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
019600051021     C                             (DSEDIABD=SAVEDIABD     AND
019700051021     C                              SQL_vabCAS>*zeros      AND
019800051021     C                             (SQL_vabVCA=SAV_vabVCA  AND
019900051021     C                              SQL_vabTIC=SAV_vabTIC   OR
020000051021     C                              SAV_vabCAS=*zeros)     AND
020100051021     C                              wAccCAS='S')
020200051021     C*
020300051021     C* ****************  ESEGUO ACCORPAMENTO **********************
020400051216     C*
020500051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
020600051216     C                   exsr      exeRC
020700051216     C*
020800051216     C* Se fin qui tutto ok => proseguo
020900051216     C                   if        wFlgErr = *zeros
021000051021     C*
021100051021     C* Eseguo la sommatoria dei campi da raggruppare
021200051216     C                   exsr      exeSOMMA
021300051021     C*
021400051021     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
021500051021     C                   exsr      exeNORM
021600051021     C*
021700051021     C* Effettuo scrittura del file FIVAD
021800051021     C                   exsr      wriVAD
021900051021     C*
022000051021     C* Effettuo scrittura del file FIVAT
022100051021     C                   exsr      wriVAT
022200051216     C*
022300051216     C                   endif
022400051021     C*
022500051021     C* ****************  NO ACCORPAMENTO **********************
022600051021     C                   else
022700051021     C*
022800051021     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
022900051021     C* dell'ultima bolla accorpata
023000051021     C                   if        wGiro > 0
023100051021     C                   exsr      wriVAB
023200051021     C                   eval      wGiro = *zeros
023300051021     C                   endif
023400051021     C*
023500051021     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
023600051021     C                   eval      SAVEDIABD  = DSEDIABD
023700051021     C*
023800051021     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
023900051021     C                   exsr      exeINZ
024000051021     C*
024100051021     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
024200051021     C                   exsr      impVAB
024300051021     C*
024400051021     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
024500051021     C                   exsr      wriVAD
024600051021     C*
024700051021     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
024800051021     C                   exsr      wriVAT
024900031110     C*
025000031110     C                   endif
025100031110     C*
025200031110     C* Incremento il contatore che identifica il 1° giro
025300031114     C                   eval      wGiro = wGiro + 1
025400051102     C*
025500051102     C* Salvo i nuovi attributi relativi al contrassegno
025600051102     C                   eval      SAV_vabCAS = SQL_vabCAS
025700051102     C                   eval      SAV_vabVCA = SQL_vabVCA
025800051102     C                   eval      SAV_vabTIC = SQL_vabTIC
025900051216     C*
026000051216     C* Come ultima cosa deleto il record bolla testata appena considerato
026100051216     C* ...solo se tutto ok => proseguo
026200051216     C                   if        wFlgErr = *zeros
026300040712     C     KEYvab1       delete    EDIVABQT1L
026400051216     C                   endif
026500040709     C*
026600040709     C                   endif
026700031110     C*
026800031110     C/EXEC SQL
026900051216     C+ Fetch C1 into :DSEDIVAB
027000031110     C/END-EXEC
027100031110     C                   enddo
027200031110     C*
027300000707     C/EXEC SQL
027400051216     C+ close C1
027500000707     C/END-EXEC
027600031114     C*
027700031114     C* Scarico la bolla "master" in sospeso
027800031114     C* dell'ultima bolla accorpata
027900031114     C                   exsr      wriVAB
028000040709     C*
028100040712     C* Al termine chiudo i file EDI*QT temporanei
028200040709     C                   close     EDIVABQT1L
028300040712     C                   close     EDIVADQT1L
028400040712     C                   close     EDIVATQT1L
028500991112     C*
028600991112     C                   ENDSR
028700051216     C*------------------------------------------------------------------------*
028800051216     C* EXEABDRC - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
028900051216     C*------------------------------------------------------------------------*
029000051216     C     EXEABDRC      BEGSR
029100051216     C*
029200051216     C* Inizializzo indicatori d wrk
029300051216     C                   setoff                                       51
029400051216     C                   seton                                        52
029500051216     C*
029600051216     C* Innanzitutto apro i file EDI*QT temporanei
029700051216     C                   open      EDIVABQT1L
029800051216     C                   open      EDIVADQT1L
029900051216     C                   open      EDIVATQT1L
030000051216     C*
030100051216     C* Inizializzazioni
030200051216     C                   clear                   DSEDIVAB
030300051216     C                   clear                   DSEDIABD
030400051216     C                   clear                   SAVEDIABD
030500051216     C                   exsr      exeINZ
030600051216     C                   eval      wGiro = *zeros
030700051216     C                   seton                                        55
030800051216     C*
030900051216     C/EXEC SQL
031000051216     C+ declare C2 cursor for select * from edivabqt
031100051216     C*+ where vabCAS = 0 and vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
031200051216     C*+ where vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
031300051216     C*+ where vabATB = ' ' and vabxco=' '
031400051216     C+ where (vabGMA = 'RC' or vabGMA = '  ') and vabATB = ' ' and vabxco=' '
031500051216     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
031600051216     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
031700051216     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
031800051216     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
031900051216     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
032000051216     C+ for fetch only
032100051216     C/END-EXEC
032200051216     C
032300051216     C/EXEC SQL
032400051216     C+ open C2
032500051216     C/END-EXEC
032600051216     C
032700051216     C/EXEC SQL
032800051216     C+ Fetch C2 into :DSEDIVAB
032900051216     C/END-EXEC
033000051216     C*
033100051216     C                   dow       sqlcod = *zeros
033200051216     C*
033300051216     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
033400051216     C                   exsr      CHKREC
033500051216     C*
033600051216     C* Se ok proseguo
033700051216     C                   if        wRECOK = 'S'
033800051216     C*
033900051216     C* Eseguo routine d valorizzazione DS d livello rottura
034000051216     C                   exsr      valDSROT
034100051216     C*
034200051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
034300051216     C                   z-add     *zeros        wFlgErr           1 0
034400051216     C*
034500051216     C* Verifico avvenuta rottura d codice
034600051216     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
034700051216     C                             (DSEDIABD=SAVEDIABD     AND
034800051216     C                              SQL_vabCAS>*zeros      AND
034900051216     C                             (SQL_vabVCA=SAV_vabVCA  AND
035000051216     C                              SQL_vabTIC=SAV_vabTIC   OR
035100051216     C                              SAV_vabCAS=*zeros)     AND
035200051216     C                              wAccCAS='S')
035300051216     C*
035400051216     C* ****************  ESEGUO ACCORPAMENTO **********************
035500051216     C*
035600051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
035700051216     C                   exsr      exeRC
035800051216     C*
035900051216     C* Se fin qui tutto ok => proseguo
036000051216     C                   if        wFlgErr = *zeros
036100051216     C*
036200051216     C* Eseguo la sommatoria dei campi da raggruppare
036300051216     C                   exsr      exeSOMMA
036400051216     C*
036500051216     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
036600051216     C                   exsr      exeNORM
036700051216     C*
036800051216     C* Effettuo scrittura del file FIVAD
036900051216     C                   exsr      wriVAD
037000051216     C*
037100051216     C* Effettuo scrittura del file FIVAT
037200051216     C                   exsr      wriVAT
037300051216     C*
037400051216     C                   endif
037500051216     C*
037600051216     C* ****************  NO ACCORPAMENTO **********************
037700051216     C                   else
037800051216     C*
037900051216     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
038000051216     C* dell'ultima bolla accorpata
038100051216     C                   if        wGiro > 0
038200051216     C                   exsr      wriVAB
038300051216     C                   eval      wGiro = *zeros
038400051216     C                   endif
038500051216     C*
038600051216     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
038700051216     C                   eval      SAVEDIABD  = DSEDIABD
038800051216     C*
038900051216     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
039000051216     C                   exsr      exeINZ
039100051216     C*
039200051216     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
039300051216     C                   exsr      impVAB
039400051216     C*
039500051216     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
039600051216     C                   exsr      wriVAD
039700051216     C*
039800051216     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
039900051216     C                   exsr      wriVAT
040000051216     C*
040100051216     C                   endif
040200051216     C*
040300051216     C* Incremento il contatore che identifica il 1° giro
040400051216     C                   eval      wGiro = wGiro + 1
040500051216     C*
040600051216     C* Salvo i nuovi attributi relativi al contrassegno
040700051216     C                   eval      SAV_vabCAS = SQL_vabCAS
040800051216     C                   eval      SAV_vabVCA = SQL_vabVCA
040900051216     C                   eval      SAV_vabTIC = SQL_vabTIC
041000051216     C*
041100051216     C* Come ultima cosa deleto il record bolla testata appena considerato
041200051216     C* ...solo se tutto ok => proseguo
041300051216     C                   if        wFlgErr = *zeros
041400051216     C     KEYvab1       delete    EDIVABQT1L
041500051216     C                   endif
041600051216     C*
041700051216     C                   endif
041800051216     C*
041900051216     C/EXEC SQL
042000051216     C+ Fetch C2 into :DSEDIVAB
042100051216     C/END-EXEC
042200051216     C                   enddo
042300051216     C*
042400051216     C/EXEC SQL
042500051216     C+ close C2
042600051216     C/END-EXEC
042700051216     C*
042800051216     C* Scarico la bolla "master" in sospeso
042900051216     C* dell'ultima bolla accorpata
043000051216     C                   exsr      wriVAB
043100051216     C*
043200051216     C* Al termine chiudo i file EDI*QT temporanei
043300051216     C                   close     EDIVABQT1L
043400051216     C                   close     EDIVADQT1L
043500051216     C                   close     EDIVATQT1L
043600051216     C*
043700051216     C                   ENDSR
043800031110     C*------------------------------------------------------------------------*
043900040712     C* EXENOABD - SCRITTIURA FILE FIVAB/FIVAD X BOLLE NN RAGGRUPPABILI
044000031110     C*------------------------------------------------------------------------*
044100031110     C     EXENOABD      BEGSR
044200050620     C*
044300050620     C* Inizializzo indicatori d wrk
044400050620     C                   setoff                                       51
044500040712     C*
044600040712     C* Innanzitutto apro i file EDI*QT temporanei
044700040712     C                   open      EDIVABQT1L
044800040712     C                   open      EDIVADQT1L
044900040712     C                   open      EDIVATQT1L
045000040712     C*
045100040712     C* Porto tutto ciò che è rimasto nei file EDI*QT nei relativi FIVA*
045200040712     C*
045300040712     C* ...VAB
045400040712     C     *loval        setll     EDIVABQT1L
045500040712     C                   read      EDIVABQT1L
045600040712     C                   dow       not %eof(EDIVABQT1L)
045700040712     C                   write     FIVAB000
045800040712     C                   delete    EDIVABQT1L
045900040712     C                   read      EDIVABQT1L
046000040712     C                   enddo
046100040712     C*
046200040712     C* ...VAD
046300040712     C     *loval        setll     EDIVADQT1L
046400040712     C                   read      EDIVADQT1L
046500040712     C                   dow       not %eof(EDIVADQT1L)
046600040712     C                   write     FIVAD000
046700040712     C                   delete    EDIVADQT1L
046800040712     C                   read      EDIVADQT1L
046900040712     C                   enddo
047000040712     C*
047100040712     C* ...VAT
047200040712     C     *loval        setll     EDIVATQT1L
047300040712     C                   read      EDIVATQT1L
047400040712     C                   dow       not %eof(EDIVATQT1L)
047500040712     C                   write     FIVAT000
047600040712     C                   delete    EDIVATQT1L
047700040712     C                   read      EDIVATQT1L
047800040712     C                   enddo
047900040714     C*
048000040714     C* ...VAX
048100050708     C     KEYvax2_C     setll     EDIVAX2L
048200050708     C     KEYvax2_C     reade     EDIVAX2L
048300040714     C                   dow       not %eof(EDIVAX2L)
048400041116     c     vaxfgs        lookup    lin                                    50
048500041116     c                   if        *in50  and vaxfgs>0
048600040714     C                   write     FIVAX000
048700040714     C                   delete    EDIVAX2L
048800041116     c                   endif
048900041116     c
049000050708     C     KEYvax2_C     reade     EDIVAX2L
049100040714     C                   enddo
049200040712     C*
049300040714     C* Al termine chiudo i file EDI* temporanei
049400040712     C                   close     EDIVABQT1L
049500040712     C                   close     EDIVADQT1L
049600040712     C                   close     EDIVATQT1L
049700031110     C*
049800031110     C                   ENDSR
049900031110     C*------------------------------------------------------------------------*
050000031110     C* EXEINZ - ROUTINE D INIZIALIZZAZIONE X OGNI NUOVO LIVELLO D ROTTURA BOLLE
050100031110     C*------------------------------------------------------------------------*
050200031110     C     EXEINZ        BEGSR
050300031110     C*
050400031110     C                   clear                   DSEDIVABW
050500031114     C                   clear                   wRMO
050600050620     C                   clear                   wNOT
050700050620     C                   clear                   wNT2
050800051215     C                   eval      wFlgOrmRC = *off
050900031110     C*
051000031110     C                   ENDSR
051100031110     C*------------------------------------------------------------------------*
051200031110     C* VALDSROT - VALORIZZAZIONE DS LIVELLO ROTTURA
051300031110     C*------------------------------------------------------------------------*
051400031112     C     VALDSROT      BEGSR
051500031110     C*
051600031110     C* Campi in rottura
051700031114     C                   EVAL      S_vabCCM = SQL_vabCCM
051800031110     C                   EVAL      S_vabRSD = SQL_vabRSD
051900031110     C                   EVAL      S_vabRD2 = SQL_vabRD2
052000031110     C                   EVAL      S_vabIND = SQL_vabIND
052100031110     C                   EVAL      S_vabCAD = SQL_vabCAD
052200031110     C                   EVAL      S_vabLOD = SQL_vabLOD
052300031110     C                   EVAL      S_vabPRD = SQL_vabPRD
052400031110     C                   EVAL      S_vabNZD = SQL_vabNZD
052500031110     C                   EVAL      S_vabLNA = SQL_vabLNA
052600031110     C                   EVAL      S_vabZNC = SQL_vabZNC
052700031110     C                   EVAL      S_vabCBO = SQL_vabCBO
052800031110     C                   EVAL      S_vabTSP = SQL_vabTSP
052900031110     C                   EVAL      S_vabCTR = SQL_vabCTR
053000031110     C                   EVAL      S_vabGC1 = SQL_vabGC1
053100031110     C                   EVAL      S_vabGC2 = SQL_vabGC2
053200031110     C                   EVAL      S_vabFFD = SQL_vabFFD
053300031110     C                   EVAL      S_vabDCR = SQL_vabDCR
053400031110     C                   EVAL      S_vabTCR = SQL_vabTCR
053500031110     C                   EVAL      S_vabHCR = SQL_vabHCR
053600051216     C  N52              EVAL      S_vabGMA = SQL_vabGMA
053700031110     C                   EVAL      S_vabGGA = SQL_vabGGA
053800031110     C                   EVAL      S_vabGVA = SQL_vabGVA
053900031110     C                   EVAL      S_vabTC1 = SQL_vabTC1
054000031110     C                   EVAL      S_vabTC2 = SQL_vabTC2
054100031110     C                   EVAL      S_vabVAS = SQL_vabVAS
054200031110     C                   EVAL      S_vabVAD = SQL_vabVAD
054300031110     C*
054400031110     C                   ENDSR
054500031110     C*------------------------------------------------------------------------*
054600031110     C* IMPVAB - VALORIZZAZIONE CAMPI "FISSI" 1° BOLLA D OGNI LIVELLO D ROTTURA
054700031110     C*------------------------------------------------------------------------*
054800031110     C     IMPVAB        BEGSR
054900031110     C*
055000031110     C* Valorizzo la DS d wrk da quella d lettura SQL
055100031110     C                   EVAL      DSEDIVABW = DSEDIVAB
055200031110     C*
055300031110     C                   ENDSR
055400051216     C*------------------------------------------------------------------------*
055500051216     C* EXERC - GESTIONE EVENTUALE PRESENZA PARTICOLARITA' CONSEGNA 'RC'
055600051216     C*------------------------------------------------------------------------*
055700051216     C     EXERC         BEGSR
055800051216     C*
055900051216     C* Gestisco anche l'accorpamento anche dell'ORM da "RC"
056000051216     C* ...ma solo se trattasi d primo "RC" delle bolle oggetto d accorpamento
056100060126     C**!!!              IF        SQL_vabGMA = 'RC' and
056200060126     C**!!!                        wFlgOrmRC = *off
056300060126    1C                   IF        SQL_vabGMA = 'RC'
056400060126    2c                   If        wflgormrc = *On
056500060127     C*
056600060127     C* Elimino l'ORM RC della bolla corrente (accorpata)
056700060126     c                   Clear                   Fnls25ds
056800060126     c                   Eval      i25ksc  = sql_vabccm
056900060126     c                   Move(p)   sql_vabaas    i25aas
057000060126     c                   Eval      i25spe = %editc(sql_vablnp:'X') +
057100060126     c                                      %editc(sql_vabnrs:'X') +
057200060126     c                                      %editc(sql_vabnsp:'X')
057300060126     c                   Call      'FNLS25R'
057400060126     c                   Parm                    Fnls25ds
057500060126    2C                   ENDIF
057600060126    2C                   If        wFlgOrmRC = *off
057700060127     C*
057800060127     C* Aggiorno l'orm RC legato alla bolla risultante dall'accorpamento solo se
057900060127     C* bolla corrente (accorpata) e bolla accorpante hanno chiave diversa.
058000060127     C                   IF        SQL_vabAAS <> WRK_vabAAS OR
058100060127     C                             SQL_vabLNP <> WRK_vabLNP OR
058200060127     C                             SQL_vabNRS <> WRK_vabNRS OR
058300060127     C                             SQL_vabNSP <> WRK_vabNSP
058400060127     C*
058500060127     C* Prima dell'aggiornamento del riferimento ORM RC con la nuova chiave spedizione
058600060127     C* elimino l'ORM RC della bolla accorpante
058700060127     c                   Clear                   Fnls25ds
058800060127     c                   Eval      i25ksc  = sql_vabccm
058900060127     c                   Move(p)   sql_vabaas    i25aas
059000060127     c                   Eval      i25spe = %editc(WRK_vabLNP:'X') +
059100060127     c                                      %editc(WRK_vabNRS:'X') +
059200060127     c                                      %editc(WRK_vabNSP:'X')
059300060127     c                   Call      'FNLS25R'
059400060127     c                   Parm                    Fnls25ds
059500060127     C*
059600060127     C* Quindi aggiorno l'ORM RC della bolla corrente (accorpata) con la nuova chiave
059700060127     C* spedizione (bolla accorpante)
059800051216     C                   CLEAR                   wRC_KEY
059900051216     C                   EVAL      wRC_AAS = SQL_vabAAS
060000051216     C                   EVAL      wRC_LNP = SQL_vabLNP
060100051216     C                   EVAL      wRC_NRS = SQL_vabNRS
060200051216     C                   EVAL      wRC_NSP = SQL_vabNSP
060300051220     C                   EVAL      I24TLA  = 'L'
060400051216     C                   EVAL      I24OPE  = 'U'
060500051216     C                   EVAL      I24KSC  = SQL_vabCCM
060600051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
060700051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
060800051216     C                   EVAL      I24NAAS = WRK_vabAAS
060900051216     C                   EVAL      I24LNP  = WRK_vabLNP
061000051216     C                   EVAL      I24NRS  = WRK_vabNRS
061100051216     C                   EVAL      I24NSP  = WRK_vabNSP
061200051216     C                   CALL      'FNLS24R'
061300051216     C                   PARM                    FNLS24DS
061400060127     C*
061500051216     C* Se tutto ok => accendo l'indicatore x nn aggiornare ORM generati da bolle "RC"
061600051216     C* accorpate successivamente (ovviamente x medesimo destinatario)
061700060126    3C                   IF        O24ERR = *zeros
061800051216     C                   EVAL      wFlgOrmRC = *on
061900060126   x3C                   ELSE
062000051216     C                   Z-ADD     1             wFlgErr
062100060126    3C                   ENDIF
062200060127     C                   ENDIF
062300060126    2C                   ENDIF
062400060126    1C                   ENDIF
062500051216     C*
062600051216     C                   ENDSR
062700031110     C*------------------------------------------------------------------------*
062800031110     C* EXESOMMA - SOMMATORIA CAMPI X RAGGRUPPAMENTO
062900031110     C*------------------------------------------------------------------------*
063000031110     C     EXESOMMA      BEGSR
063100031110     C*
063200051216     C* Eseguo sommatoria dati bolle
063300031110     C                   EVAL      WRK_vabIAS = WRK_vabIAS + SQL_vabIAS
063400031110     C                   EVAL      WRK_vabNCL = WRK_vabNCL + SQL_vabNCL
063500031110     C                   EVAL      WRK_vabPKB = WRK_vabPKB + SQL_vabPKB
063600031110     C                   EVAL      WRK_vabVLB = WRK_vabVLB + SQL_vabVLB
063700031110     C                   EVAL      WRK_vabQFT = WRK_vabQFT + SQL_vabQFT
063800031110     C                   EVAL      WRK_vabVMD = WRK_vabVMD + SQL_vabVMD
063900051021     C                   EVAL      WRK_vabCAS = WRK_vabCAS + SQL_vabCAS
064000031110     C*
064100031110     C                   ENDSR
064200031110     C*------------------------------------------------------------------------*
064300031110     C* EXENORM - NORMALIZZAZIONE CAMPI X OTTENIMENTO BOLLA "MASTER"
064400031110     C*------------------------------------------------------------------------*
064500031110     C     EXENORM       BEGSR
064600050620     C*
064700050620     C* Valorizzo indicatore d wrk
064800050620     C                   SETON                                        51
064900031110     C*
065000031114     C                   IF        WRK_vabNAS =  *blanks AND
065100031114     C                             SQL_vabNAS <> *blanks
065200031110     C                   EVAL      WRK_vabNAS = SQL_vabNAS
065300031110     C                   ENDIF
065400031110     C*
065500031114     C                   IF        WRK_vabCTM =  *blanks AND
065600031114     C                             SQL_vabCTM <> *blanks
065700031110     C                   EVAL      WRK_vabCTM = SQL_vabCTM
065800031110     C                   ENDIF
065900031110     C*
066000031114     C                   IF        WRK_vabCTS =  *blanks AND
066100031114     C                             SQL_vabCTS <> *blanks
066200031110     C                   EVAL      WRK_vabCTS = SQL_vabCTS
066300031110     C                   ENDIF
066400031110     C*
066500031114     C                   IF        WRK_vabFTM =  *blanks AND
066600031114     C                             SQL_vabFTM <> *blanks
066700031110     C                   EVAL      WRK_vabFTM = SQL_vabFTM
066800031110     C                   ENDIF
066900031110     C*
067000031114     C                   IF        WRK_vabSCL =  *blanks AND
067100031114     C                             SQL_vabSCL <> *blanks
067200031110     C                   EVAL      WRK_vabSCL = SQL_vabSCL
067300031110     C                   ENDIF
067400051021     C*
067500051021     C                   IF        WRK_vabVCA =  *blanks AND
067600051021     C                             SQL_vabVCA <> *blanks
067700051021     C                   EVAL      WRK_vabVCA = SQL_vabVCA
067800051021     C                   ENDIF
067900051021     C*
068000051021     C                   IF        WRK_vabTIC =  *blanks AND
068100051021     C                             SQL_vabTIC <> *blanks
068200051021     C                   EVAL      WRK_vabTIC = SQL_vabTIC
068300051021     C                   ENDIF
068400051216     C*
068500051216     C                   IF        WRK_vabGMA =  *blanks AND
068600051216     C                             SQL_vabGMA <> *blanks
068700051216     C                   EVAL      WRK_vabGMA = SQL_vabGMA
068800051216     C                   ENDIF
068900031110     C*
069000031110     C* Gestisco comportamento particolare del campo VABANT (tipo e numero bancali)
069100031110     C                   IF        WRK_vabANT = *zeros
069200031110     C                   EVAL      WRK_vabANT = SQL_vabANT
069300031110     C                   ELSE
069400031110     C                   IF        SQL_vabANT = *zeros
069500031110     C                   ELSE
069600031110     C                   EVAL      DSVABANT = SQL_vabANT
069700031110     C                   EVAL      wANTTIP1 = ANTTIP1
069800031110     C                   EVAL      wANTNUM1 = ANTNUM1
069900031110     C                   EVAL      wANTTIP2 = ANTTIP2
070000031110     C                   EVAL      wANTNUM2 = ANTNUM2
070100031110     C                   EVAL      DSVABANT = WRK_vabANT
070200031110     C                   IF        ANTTIP1 > *zeros
070300031110     C                   IF        wANTTIP1 = ANTTIP1
070400031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM1
070500031110     C                   ENDIF
070600031110     C                   IF        wANTTIP2 = ANTTIP1
070700031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM2
070800031110     C                   ENDIF
070900031110     C                   ELSE
071000031110     C                   IF        wANTTIP1 > *zeros
071100031110     C                   EVAL      ANTTIP1 = wANTTIP1
071200031110     C                   EVAL      ANTNUM1 = wANTNUM1
071300031110     C                   ELSE
071400031110     C                   IF        wANTTIP2 > *zeros
071500031110     C                   EVAL      ANTTIP1 = wANTTIP2
071600031110     C                   EVAL      ANTNUM1 = wANTNUM2
071700031110     C                   ENDIF
071800031110     C                   ENDIF
071900031110     C                   ENDIF
072000031110     C                   IF        ANTTIP2 > *zeros
072100031110     C                   IF        wANTTIP1 = ANTTIP2
072200031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM1
072300031110     C                   ENDIF
072400031110     C                   IF        wANTTIP2 = ANTTIP2
072500031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM2
072600031110     C                   ENDIF
072700031110     C                   ELSE
072800031110     C                   IF        wANTTIP1 > *zeros
072900031110     C                   EVAL      ANTTIP2 = wANTTIP1
073000031110     C                   EVAL      ANTNUM2 = wANTNUM1
073100031110     C                   ELSE
073200031110     C                   IF        wANTTIP2 > *zeros
073300031110     C                   EVAL      ANTTIP2 = wANTTIP2
073400031110     C                   EVAL      ANTNUM2 = wANTNUM2
073500031110     C                   ENDIF
073600031110     C                   ENDIF
073700031110     C                   ENDIF
073800031110     C                   EVAL      WRK_vabANT = DSVABANT
073900031110     C                   ENDIF
074000031110     C                   ENDIF
074100031114     C*
074200051021     C* Eseguo raggruppamento dei riferimenti delle testate bolle accorpate
074300051021     C                   IF        wLenRIF > *zeros
074400050906     C                   IF        wTIPABD = 'S'
074500050614     C                   EVAL      wRIF = %trim(%editc(SQL_vabRMN:'4'))
074600050610     C                   ENDIF
074700050610     C                   IF        wTIPABD = 'A'
074800050610     C                   EVAL      wRIF = %trim(SQL_vabRMA)
074900050610     C                   ENDIF
075000050610     C*
075100050610     C* Gestisco l'allineamento d default a seconda della tipologia d riferimento da utilizzare
075200050610     C                   IF        wAlmRIF = *blanks
075300050610     C                   IF        wTipABD = 'S'
075400050610     C                   EVAL      wAlmRIF = 'D'
075500050610     C                   ENDIF
075600050610     C                   IF        wTipABD = 'A'
075700050610     C                   EVAL      wAlmRIF = 'S'
075800050610     C                   ENDIF
075900050610     C                   ENDIF
076000051021     C*
076100051021     C* Effettuo le operazioni seguenti SOLO in caso in cui il riferimento voluto
076200051021     C* risulti essere effettivamente valorizzato
076300051021     C                   IF        wRIF <> *blanks
076400050614     C*
076500050614     C* Considero la minore tra la lunghezza reale del riferimento da considerare e il
076600050614     C* numero d byte da considerare indicati in tabella x il cliente corrente
076700050614     C                   Z-ADD     *zeros        wByteRIF          2 0
076800050614     C                   IF        wLenRIF <= %len(%trim(wRIF))
076900050614     C                   EVAL      wByteRIF = wLenRIF
077000050614     C                   ELSE
077100050614     C                   EVAL      wByteRIF = %len(%trim(wRIF))
077200050614     C                   ENDIF
077300050610     C*
077400050610     C* A questo punto verifico se sono richiesti solo 1 numero specifico d byte ed il
077500050610     C* relativo allineamento
077600050610     C                   IF        wLenRIF <> 99
077700050610     C                   IF        wAlmRIF = 'S'
077800050614     C                   EVAL      wRIF = %subst(%trim(wRIF):1:wByteRIF)
077900050610     C                   ENDIF
078000050610     C                   IF        wAlmRIF = 'D'
078100050610     C                   EVAL      wRIF = %subst(%trim(wRIF):
078200050614     C                                    %len(%trim(wRIF))-wByteRIF+1)
078300050610     C                   ENDIF
078400050610     C                   ENDIF
078500051021     C*
078600051021     C                   ELSE
078700051021     C                   MOVEL     'N'           wOltreRIF
078800051021     C                   ENDIF
078900050610     C*
079000031114     C* Se c stanno...
079100050610     C                   IF        %len(%trim(wRMO))+1+%len(%trim(wRIF)) <=
079200040517     C                             %len(wRMO)-4
079300050610     C                   EVAL      wRMO = %trim(wRMO) + %trim(wRIF) + '-'
079400050610     C                   MOVEL     'N'           wOltreRIF         1
079500050610     C                   ELSE
079600050610     C                   MOVEL     'S'           wOltreRIF
079700031114     C                   ENDIF
079800050906     C*
079900050906     C                   ELSE
080000050906     C                   MOVEL     'N'           wOltreRIF
080100050906     C                   ENDIF
080200051021     C*
080300050620     C*
080400050620     C* Se richiesto raggruppo anche le note
080500050620     C                   IF        wRgrNOT = 'S'
080600060726     C* Memorizzo subito note originali
080700060726     C                   IF        wNOT = *blanks AND WRK_vabNOT <> *blanks AND
080800060726     C                             %scan(%trim(WRK_vabNOT):wNT2) = 0
080900060726     C                   EVAL      wNOT = %trim(WRK_vabNOT)
081000060726     C                   ENDIF
081100060726     C                   IF        wNT2 = *blanks AND WRK_vabNT2 <> *blanks AND
081200060726     C                             %scan(%trim(WRK_vabNT2):wNOT) = 0
081300060726     C                   EVAL      wNT2 = %trim(WRK_vabNT2)
081400060726     C                   ENDIF
081500060629     C***  VABNOT
081600060726     C* Se presente nota => gestisco
081700060726     C                   IF        SQL_vabNOT <> *blanks
081800060629     C* Innanzitutto verifico che il riferimento corrente nn sia già presente
081900060726     C                   SETOFF                                       40
082000060726     C                   IF        wNOT <> *blanks
082100060726     C                   IF        %scan(%trim(SQL_vabNOT):wNOT) <> 0 OR
082200060726     C                             %scan(%trim(SQL_vabNOT):wNT2) <> 0
082300060726     C                   SETON                                        40
082400060726     C                   ENDIF
082500060726     C                   ENDIF
082600060629     C* Verifico se lo spazio su NOT è già esaurito mentre quello su NT2 è ancora dispnibile
082700060726     C                   IF        *IN40 = *OFF
082800060629     C                   IF        (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNOT))
082900060629     C                             >%size(wNOT)) AND
083000060629     C                             (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNOT))
083100060629     C                             <=%size(wNT2))
083200060726     C                   IF        %scan(%trim(SQL_vabNOT):wNT2) = 0 AND
083300060726     C                             %scan(%trim(SQL_vabNOT):wNOT) = 0
083400060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNOT)
083500060726     C                   ENDIF
083600060629     C                   ELSE
083700050708     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNOT)
083800060726     C                   ENDIF
083900060726     C                   ENDIF
084000060726     C                   ENDIF
084100060629     C***  VABNT2
084200060726     C* Se presente nota => gestisco
084300060726     C                   IF        SQL_vabNT2 <> *blanks
084400060629     C* Innanzitutto verifico che il riferimento corrente nn sia già presente
084500060726     C                   SETOFF                                       40
084600060726     C                   IF        wNT2 <> *blanks
084700060726     C                   IF        %scan(%trim(SQL_vabNT2):wNT2) <> 0 OR
084800060726     C                             %scan(%trim(SQL_vabNT2):wNOT) <> 0
084900060726     C                   SETON                                        40
085000060726     C                   ENDIF
085100060726     C                   ENDIF
085200060629     C* Verifico se lo spazio su NT2 è già esaurito mentre quello su NOT è ancora dispnibile
085300060726     C                   IF        *IN40 = *OFF
085400060629     C                   IF        (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNT2))
085500060629     C                             >%size(wNT2)) AND
085600060629     C                             (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNT2))
085700060629     C                             <=%size(wNOT))
085800060726     C                   IF        %scan(%trim(SQL_vabNT2):wNOT) = 0 AND
085900060726     C                             %scan(%trim(SQL_vabNT2):wNT2) = 0
086000060629     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNT2)
086100060726     C                   ENDIF
086200060629     C                   ELSE
086300060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNT2)
086400060629     C                   ENDIF
086500060629     C                   ENDIF
086600060726     C                   ENDIF
086700060629     C*
086800050620     C                   ELSE
086900050708     C                   EVAL      wNOT = %trim(SQL_vabNOT)
087000050708     C                   EVAL      wNT2 = %trim(SQL_vabNT2)
087100050620     C                   ENDIF
087200031110     C*
087300031110     C                   ENDSR
087400031112     C*------------------------------------------------------------------------*
087500031112     C* WRIVAD - SCRITTIURA FILE FIVAD
087600031112     C*------------------------------------------------------------------------*
087700031112     C     WRIVAD        BEGSR
087800031112     C*
087900031112     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAD)
088000031112     C                   CLEAR                   FIVAD000
088100031112     C*
088200051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAD
088300040712     C     KEYva_1       SETLL     EDIVADQT1L
088400031113     C                   IF        %equal(EDIVADQT1L)
088500040227     C* Eseguo routine x modifica codice trattamento merce
088600040227     C                   EXSR      CHGCTM
088700040227     C*
088800040712     C     KEYva_1       READE     EDIVADQT1L
088900031113     C                   DOW       not %eof(EDIVADQT1L)
089000031112     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
089100031112     C                   EVAL      vadAAS = WRK_vabAAS
089200031112     C                   EVAL      vadLNP = WRK_vabLNP
089300031112     C                   EVAL      vadNRS = WRK_vabNRS
089400031112     C                   EVAL      vadNSP = WRK_vabNSP
089500031112     C* Inserisco record d dettaglio in FIVAD
089600031112     C                   WRITE     FIVAD000
089700031112     C* Elimino record d dettaglio in EDIVAD
089800031113     C                   DELETE    EDIVADQT1L
089900031112     C* Continuo la lettura dei dettagli della testata bolla corrente
090000040712     C     KEYva_1       READE     EDIVADQT1L
090100031112     C                   ENDDO
090200031112     C*
090300031112     C* Se invece nn esiste già il dettaglio colli in EDIVAD e sulla testata c'è l'indicazioine
090400031112     C* dei segnacolli => provvedo a creare il VAD
090500031112     C                   ELSE
090600031112     C                   IF        SQL_vabNCD > *zeros OR
090700031112     C                             SQL_vabNCA > *zeros
090800040227     C* Eseguo routine x modifica codice trattamento merce
090900040227     C                   EXSR      CHGCTM
091000031112     C* Valorizzo la chiave bolla del dettaglio con quella della relativa testata bolla "master"
091100031112     C                   EVAL      vadAAS = WRK_vabAAS
091200031112     C                   EVAL      vadLNP = WRK_vabLNP
091300031112     C                   EVAL      vadNRS = WRK_vabNRS
091400031112     C                   EVAL      vadNSP = WRK_vabNSP
091500031112     C* Valorizzo i restanti campi con quelli della testata bolla corrente
091600031112     C                   EVAL      vadFGS = SQL_vabFGS
091700031112     C                   EVAL      vadCCM = SQL_vabCCM
091800031112     C                   EVAL      vadNCL = SQL_vabNCL
091900031112     C                   EVAL      vadNCD = SQL_vabNCD
092000031112     C                   EVAL      vadNCA = SQL_vabNCA
092100031112     C* Inserisco record d dettaglio in FIVAD
092200031112     C                   WRITE     FIVAD000
092300031112     C                   ENDIF
092400031112     C                   ENDIF
092500031112     C*
092600031112     C                   ENDSR
092700031114     C*------------------------------------------------------------------------*
092800031114     C* WRIVAT - SCRITTIURA FILE FIVAT
092900031114     C*------------------------------------------------------------------------*
093000031114     C     WRIVAT        BEGSR
093100031114     C*
093200031114     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAT)
093300031114     C                   CLEAR                   FIVAT000
093400031114     C*
093500051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAT
093600040712     C     KEYva_1       SETLL     EDIVATQT1L
093700031114     C                   IF        %equal(EDIVATQT1L)
093800040227     C* Eseguo routine x modifica codice trattamento merce
093900040227     C                   EXSR      CHGCTM
094000040227     C*
094100040712     C     KEYva_1       READE     EDIVATQT1L
094200031114     C                   DOW       not %eof(EDIVATQT1L)
094300031114     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
094400031114     C                   EVAL      vatAAS = WRK_vabAAS
094500031114     C                   EVAL      vatLNP = WRK_vabLNP
094600031114     C                   EVAL      vatNRS = WRK_vabNRS
094700031114     C                   EVAL      vatNSP = WRK_vabNSP
094800031114     C* Inserisco record d dettaglio in FIVAT
094900031114     C                   WRITE     FIVAT000
095000031114     C* Elimino record d dettaglio in EDIVAT
095100031114     C                   DELETE    EDIVATQT1L
095200031114     C* Continuo la lettura dei dettagli della testata bolla corrente
095300040712     C     KEYva_1       READE     EDIVATQT1L
095400031114     C                   ENDDO
095500031114     C                   ENDIF
095600031114     C*
095700031114     C                   ENDSR
095800031112     C*------------------------------------------------------------------------*
095900031112     C* WRIVAB - SCRITTIURA FILE FIVAB X BOLLA "MASTER" ACCORPATA
096000031112     C*------------------------------------------------------------------------*
096100031112     C     WRIVAB        BEGSR
096200031112     C*
096300031112     C* Innanzitutto svuoto il buffer del record d testata d output (FIVAB)
096400031112     C                   CLEAR                   FIVAB000
096500031112     C* Valorizzo il buffer del record d testata d output (FIVAB)
096600031112     C                   EVAL      DSEDIVABOUT = DSEDIVABW
096700031112     C* Resetto alcuni campi che nella bolla "master" occcorrono NN valorizzati
096800031113     C   55              CLEAR                   vabNCD
096900031113     C   55              CLEAR                   vabNCA
097000031114     C   55              CLEAR                   vabRMO
097100031114     C   55              CLEAR                   vabCMO
097200031114     C   55              CLEAR                   vabNMO
097300050620     C                   IF        *IN55 = *on
097400050620     C* Campo RIFERIMENTO MITTENTE
097500050701     C                   IF        wRMO <> *blanks
097600060530     C                   Z-ADD     wGiro         wGiro2
097700060530     C                   EVAL      vabRMO = 'N'+%editw(wGiro2:'  ')+':'+
097800040303     C                                      %trim(wRMO)
097900050610     C                   IF        wGiro > 1 AND wOltreRIF = 'S'
098000040514     C                   MOVE      '...'         vabRMO
098100040303     C                   ENDIF
098200050701     C                   ENDIF
098300050620     C* Campi NOTE
098400060726     C   51              EVAL      vabNOT = %trim(wNOT)
098500060726     C   51              EVAL      vabNT2 = %trim(wNT2)
098600040303     C                   ENDIF
098700031112     C* Inserisco record d testata in FIVAB
098800031112     C                   WRITE     FIVAB000
098900050620     C*
099000050620     C* Inizializzo indicatori d wrk
099100050701     C                   SETOFF                                       51
099200031112     C*
099300031112     C                   ENDSR
099400040709     C*------------------------------------------------------------------------*
099500040709     C* CHKREC - CONTROLLO CORRETTEZZA RECORD TESTATA BOLLE AI FINI DEL RAGGRUPPAMENTO
099600040709     C*------------------------------------------------------------------------*
099700040709     C     CHKREC        BEGSR
099800040709     C*
099900040709     C* Inizializzo il flag d correttezza record testata bolle
100000040709     C                   MOVEL     'S'           wRECOK            1
100100051215     C*
100200051215     C* Verifica se bolla corrente presenta particolarità consegna RC e cliente nn abilitato
100300051215     C* specificatamente all'accorpamento anche con part. consegna RC
100400051215     C                   IF        wRECOK = 'S'
100500051216     C                   IF        SQL_vabGMA = 'RC'
100600051216     C                   IF        wAccRC <> 'S'
100700051215     C                   MOVEL     'N'           wRECOK
100800051216     C                   ELSE
100900051216     C* Se abilitazione ok controllo subito anche l'ORM relativo (quello che poi sarà aggiornato)
101000051216     C                   CLEAR                   wRC_KEY
101100051216     C                   EVAL      wRC_AAS = SQL_vabAAS
101200051216     C                   EVAL      wRC_LNP = SQL_vabLNP
101300051216     C                   EVAL      wRC_NRS = SQL_vabNRS
101400051216     C                   EVAL      wRC_NSP = SQL_vabNSP
101500051216     C                   CLEAR                   FNLS24DS
101600051216     C                   EVAL      I24TLA  = *blanks
101700051216     C                   EVAL      I24OPE  = 'L'
101800051216     C                   EVAL      I24KSC  = SQL_vabCCM
101900051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
102000051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
102100051216     C                   CALL      'FNLS24R'
102200051216     C                   PARM                    FNLS24DS
102300051216     C* Se tutto ok procedo altrimenti skippo la bolla corrente con particolarità RC ai fini
102400051216     C* dell'accorpamento
102500051216     C                   IF        O24ERR <> *zeros
102600051216     C                   MOVEL     'N'           wRECOK
102700051216     C                   ENDIF
102800051215     C                   ENDIF
102900051216     C                   ENDIF
103000051215     C                   ENDIF
103100040709     C*
103200040709     C* Verifica codice trattamento
103300051215     C                   IF        wRECOK = 'S'
103400040709     C                   EXSR      CHKCTM
103500051215     C                   ENDIF
103600040709     C*
103700040709     C                   ENDSR
103800040709     C*------------------------------------------------------------------------*
103900040709     C* CHKCTM - CONTROLLO CODICE TRATTAMENTO MERCE
104000040709     C*------------------------------------------------------------------------*
104100040709     C     CHKCTM        BEGSR
104200040709     C*
104300040709     C* Verifico che il codice trattamento merce della bolla corrente nn preveda il VAX
104400040709     C                   EVAL      tblKUT = 1
104500040709     C                   EVAL      tblCOD = '1B'
104600040709     C                   EVAL      tblKEY = SQL_vabCTM
104700040709     C     KEYtab        CHAIN     TABEL00F
104800040709     C                   IF        %found(TABEL00F)
104900040709     C                   EVAL      DS1B = tblUNI
105000040709     C                   IF        §1BF16 = 'S'
105100040709     C                   MOVEL     'N'           wRECOK
105200040709     C                   ENDIF
105300040709     C                   ENDIF
105400040709     C*
105500040709     C                   ENDSR
105600040227     C*------------------------------------------------------------------------*
105700040227     C* CHGCTM - CAMBIO CODICE TRATTAMENTO MERCE
105800040227     C*------------------------------------------------------------------------*
105900040227     C     CHGCTM        BEGSR
106000040227     C*
106100040227     C* Se sulla testata bolle NN è espressamente indicato il codice tratamento merce
106200040227     C* (e naturalmente o c'è il dettaglio colli o l'indicazione in testata dei segnacolli
106300040227     C* sequenziali) imposto come default il CTM = '2D'
106400040227     C                   IF        WRK_vabCTM = *blanks
106500040227     C                   EVAL      WRK_vabCTM = '2D'
106600040227     C                   ENDIF
106700040227     C*
106800040227     C* Valorizzo la chiave x reperire le informazioni relative al codice
106900040227     C* trattamento merce da impostare in caso d raggruppamento bolle
107000040227     C                   EVAL      tblKUT = 1
107100040227     C                   EVAL      tblCOD = '1B'
107200040227     C                   EVAL      tblKEY = WRK_vabCTM
107300040227     C     KEYtab        CHAIN     TABEL00F
107400040227     C                   IF        %found(TABEL00F)
107500040227     C                   EVAL      DS1B = tblUNI
107600040227     C                   IF        §1BCTR <> *blanks
107700040227     C                   EVAL      WRK_vabCTM = §1BCTR
107800040227     C                   ENDIF
107900040227     C                   ENDIF
108000040227     C*
108100040227     C                   ENDSR
108200050610     C*------------------------------------------------------------------------*
108300051021     C* REPABD - REPERISCE I DATI TABELLATI CHE PILOTANO LE MODALITA DI ACCORPAMENTO
108400050610     C*------------------------------------------------------------------------*
108500050610     C     REPABD        BEGSR
108600050610     C*
108700050610     C                   EVAL      tblKUT = 1
108800050610     C                   EVAL      tblCOD = '3C'
108900050708     C                   MOVEL(P)  D86CCM        tblKEY
109000050610     C     KEYtab        CHAIN     TABEL00F
109100050610     C                   IF        %found(TABEL00F)
109200050610     C                   EVAL      DS3C = tblUNI
109300050610     C                   MOVEL     §3CABD        wTipABD
109400050610     C                   Z-ADD     §3CLRM        wLenRIF
109500050610     C                   MOVEL     §3CALM        wAlmRIF
109600050620     C                   MOVEL     §3CNOT        wRgrNOT
109700051021     C                   MOVEL     §3CABC        wAccCAS
109800051215     C                   MOVEL     §3CARC        wAccRC
109900050610     C                   ENDIF
110000050610     C*
110100050610     C                   ENDSR
110200970526     C*------------------------------------------------------------------------*
110300970526     C* *INZSR - OPERAZIONI INIZIALI
110400970526     C*------------------------------------------------------------------------*
110500970526     C     *INZSR        BEGSR
110600040714     C*
110700040714     C*  Reperisco parametri in entrata
110800040714     C     *ENTRY        PLIST
110900040714     C                   PARM                    TRTC86DS
111000060726     C*
111100060726     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
111200060726     C
111300060726     C/EXEC SQL
111400060726     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
111500060726     C/END-EXEC
111600060726     C*
111700031113     C*
111800031110     C*---
111900031110     C* CHIAVI DI LETTURA
112000031110     C*---
112100040709     C* CHIAVE DI LETTURA X EDIVABQT1L - Completa
112200040709     C     KEYvab1       KLIST
112300040709     C                   KFLD                    SQL_vabCCM
112400040709     C                   KFLD                    SQL_vabCMR
112500040709     C                   KFLD                    SQL_vabCNT
112600040709     C                   KFLD                    SQL_vabAAS
112700040709     C                   KFLD                    SQL_vabLNP
112800040709     C                   KFLD                    SQL_vabNRS
112900040709     C                   KFLD                    SQL_vabNSP
113000040712     C* CHIAVE DI LETTURA X EDIVADQT1L/EDIVATQT1L
113100040712     C     KEYva_1       KLIST
113200031112     C                   KFLD                    SQL_vabFGS
113300031112     C                   KFLD                    SQL_vabCCM
113400031112     C                   KFLD                    SQL_vabCMR
113500031112     C                   KFLD                    SQL_vabCNT
113600031112     C                   KFLD                    SQL_vabAAS
113700031112     C                   KFLD                    SQL_vabLNP
113800031112     C                   KFLD                    SQL_vabNRS
113900031112     C                   KFLD                    SQL_vabNSP
114000040227     C* CHIAVE DI LETTURA X TABEL00F - Completa
114100040227     C     KEYtab        KLIST
114200040227     C                   KFLD                    tblKUT
114300040227     C                   KFLD                    tblCOD
114400040227     C                   KFLD                    tblKEY
114500040714     C* CHIAVE DI LETTURA X EDIVAX2L - Completa
114600050708     C     KEYvax2_C     KLIST
114700050708     C                   KFLD                    D86CCM
114800050708     C                   KFLD                    D86CMR
114900050708     C                   KFLD                    D86CNT
115000041116     c
115100041116     C*  Richiamo XPARUT
115200041116     C                   Z-ADD     1             CODUT             1 0
115300041116     C                   CALL      'X§PARUT'
115400041116     C                   PARM                    UT§DSE
115500041116     C                   MOVEL     REC80         CNCR80
115600041116     c
115700041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
115800041116     C                   CLEAR                   TRUL06DS
115900041116     C                   MOVE      '£1'          D06COD
116000041116     C                   MOVEL     SIMFEL        D06KEY
116100041116     C                   MOVEL     'L'           D06TLA
116200041116     C                   MOVEL     TRUL06DS      KPJBU
116300041116      *
116400041116     C                   CALL      'TRUL06R'
116500041116     C                   PARM                    KPJBA
116600041116     C                   MOVEL     KPJBU         TRUL06DS
116700991112     C*
116800991112     C                   ENDSR
