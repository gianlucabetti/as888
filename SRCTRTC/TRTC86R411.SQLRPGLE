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
001200040712     FEDIVATQT1LUF   E           K DISK    USROPN
001300070807     FEDIVAXQT1LUF   E           K DISK    USROPN
001400070808     F*EDIVAX2L  UF   E           K DISK    COMMIT
001500070802     FFIVAB00F  O    E             DISK    COMMIT
001600070802     FFIVAD00F  O    E             DISK    COMMIT
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
009000031110     D  DSVABANT               1      8  0
009100051215     D*------------------
009200051215     D* DS RIDEFINIZIONE CHIAVE BOLLA ALFA
009300051215     D*------------------
009400051215     D                 DS                  INZ
009500051215     D  wRC_AAS                1      4  0
009600051215     D  wRC_LNP                5      7  0
009700051215     D  wRC_NRS                8      9  0
009800051215     D  wRC_NSP               10     16  0
009900051215     D  wRC_KEY                1     16
010000051215     D*
010100031110     D*------------
010200031110     D* VARIABILI D WRK
010300031110     D*------------
010400051021     D SAVEDIABD       S                   LIKE(DSEDIABD)
010500051021     D SAV_vabCAS      S                   LIKE(SQL_vabCAS)
010600051021     D SAV_vabVCA      S                   LIKE(SQL_vabVCA)
010700051021     D SAV_vabTIC      S                   LIKE(SQL_vabTIC)
010800060530     D wGiro           S              3  0
010900060530     D wGiro2          S              2  0
011000031110     D wANTTIP1        S                   LIKE(ANTTIP1)
011100031110     D wANTNUM1        S                   LIKE(ANTNUM1)
011200031110     D wANTTIP2        S                   LIKE(ANTTIP2)
011300031110     D wANTNUM2        S                   LIKE(ANTNUM2)
011400050610     D wRIF            S             99    VARYING
011500031114     D wRMO            S                   LIKE(VABRMO)
011600050620     D wNOT            S                   LIKE(VABNOT)
011700050620     D wNT2            S                   LIKE(VABNT2)
011800080929     D wNOTNT2         S             70A   INZ
011900051215     D wFlgOrmRC       S              1N   INZ(*off)
012000081016     D ACC_NSP         S                   LIKE(VABNSP)
012100070807     D wVAXmaster      s              9S 0 inz(*zeros)
012200070905     D wVAXmaster01    s                   like(wVAXmaster) inz(*zeros)
012300070905     D wVAXmaster02    s                   like(wVAXmaster) inz(*zeros)
012400081013     D wFlgMaxVB       s              1  0 inz(*zeros)
012500050610     D*------------
012600050620     D* VARIABILE RELATIVE AL RAGGRUPPAMENTO BOLLE
012700050610     D*------------
012800050610     D wTipABD         S                   like(§3CABD)
012900050610     D wLenRIF         S                   like(§3CLRM)
013000050610     D wAlmRIF         S                   like(§3CALM)
013100050620     D wRgrNOT         S                   like(§3CNOT)
013200051021     D wAccCAS         S                   like(§3CABC)
013300051215     D wAccRC          S                   like(§3CARC)
013400070809     D wAccVX          S                   like(§3CAVX)
013500081013     D wMaxVB          S                   like(§3CMVR)
013600000629
013700000707
013800970526     C*------------------------------------------------------------------------*
013900970526     C* MAIN LINES
014000970526     C*------------------------------------------------------------------------*
014100071203     C*
014200071203     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
014300071203     C
014400071203     C/EXEC SQL
014500071203     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014600071203     C/END-EXEC
014700050708     C*
014800050708     C* Reperisce quale riferimenti mittente mantenere durante l'accorpamento
014900050708     C                   exsr      repABD
015000031110     C*
015100031110     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
015200031110     C                   exsr      exeABD
015300051216     C*
015400051216     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
015500051216     C* x gestione discorso particolarità consegna 'RC'
015600051216     C                   exsr      exeABDRC
015700031110     C*
015800031110     C* Eseguo routine d scrittura del FIVAB/FIVAD x bolle NN raggruppabili
015900031110     C                   exsr      exeNOABD
016000991112     C*
016100970526     C* FINE PGM
016200031110     C                   seton                                        LR
016300991112     C*------------------------------------------------------------------------*
016400031110     C* EXEABD - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
016500991112     C*------------------------------------------------------------------------*
016600031110     C     EXEABD        BEGSR
016700081013     C*
016800081013     C                   setoff                                       25
016900081013     C*
017000081013     C* Ciclo finchè nn diversamente specificato da condizionamenti a run-time
017100081013     C                   dou       *in25
017200050620     C*
017300050620     C* Inizializzo indicatori d wrk
017400050620     C                   setoff                                       51
017500051216     C                   setoff                                       52
017600091002     C                   setoff                                       53
017700040709     C*
017800040712     C* Innanzitutto apro i file EDI*QT temporanei
017900040709     C                   open      EDIVABQT1L
018000040712     C                   open      EDIVADQT1L
018100040712     C                   open      EDIVATQT1L
018200070807     C                   open      EDIVAXQT1L
018300031110     C*
018400031110     C* Inizializzazioni
018500031110     C                   clear                   DSEDIVAB
018600031110     C                   clear                   DSEDIABD
018700031110     C                   clear                   SAVEDIABD
018800031110     C                   exsr      exeINZ
018900031114     C                   eval      wGiro = *zeros
019000031113     C                   seton                                        55
019100081013     C                   eval      wFlgMaxVB = *zeros
019200000707     C*
019300000707     C/EXEC SQL
019400051216     C+ declare C1 cursor for select * from edivabqt
019500051216     C+ where vabGMA <> 'RC' and vabGMA <> '  ' and vabATB = ' ' and vabxco=' '
019600031114     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
019700031110     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
019800031110     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
019900031110     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
020000051021     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
020100000707     C+ for fetch only
020200000707     C/END-EXEC
020300000707     C
020400000707     C/EXEC SQL
020500051216     C+ open C1
020600000707     C/END-EXEC
020700000707     C
020800000707     C/EXEC SQL
020900051216     C+ Fetch C1 into :DSEDIVAB
021000000707     C/END-EXEC
021100000707     C*
021200081013     C                   if        sqlcod <> *zeros
021300081013     C                   seton                                        25
021400081013     C                   else
021500081013     C                   dow       sqlcod = *zeros AND wFlgMaxVB = *zeros
021600040709     C*
021700040709     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
021800051021     C                   exsr      CHKREC
021900040709     C*
022000040709     C* Se ok proseguo
022100040709     C                   if        wRECOK = 'S'
022200091002     C*
022300091002     C* Chiodo x TTL (cod. 0504453 - 0504450)
022400091002     C                   if        SQL_vabCCM = 0504453 or
022500091002     C                             SQL_vabCCM = 0504450
022600091002     C                   seton                                        53
022700091002     C                   endif
022800031110     C*
022900031110     C* Eseguo routine d valorizzazione DS d livello rottura
023000031110     C                   exsr      valDSROT
023100051216     C*
023200051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
023300051216     C                   z-add     *zeros        wFlgErr           1 0
023400051021     C*
023500051215     C* Verifico avvenuta rottura d codice
023600051021     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
023700051021     C                             (DSEDIABD=SAVEDIABD     AND
023800051021     C                              SQL_vabCAS>*zeros      AND
023900051021     C                             (SQL_vabVCA=SAV_vabVCA  AND
024000051021     C                              SQL_vabTIC=SAV_vabTIC   OR
024100051021     C                              SAV_vabCAS=*zeros)     AND
024200051021     C                              wAccCAS='S')
024300051021     C*
024400051021     C* ****************  ESEGUO ACCORPAMENTO **********************
024500051216     C*
024600051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
024700051216     C                   exsr      exeRC
024800051216     C*
024900051216     C* Se fin qui tutto ok => proseguo
025000051216     C                   if        wFlgErr = *zeros
025100051021     C*
025200051021     C* Eseguo la sommatoria dei campi da raggruppare
025300051216     C                   exsr      exeSOMMA
025400051021     C*
025500051021     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
025600051021     C                   exsr      exeNORM
025700070227     C*
025800070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
025900070227     C   60              exsr      wriRAB
026000051021     C*
026100051021     C* Effettuo scrittura del file FIVAD
026200051021     C                   exsr      wriVAD
026300051021     C*
026400051021     C* Effettuo scrittura del file FIVAT
026500051021     C                   exsr      wriVAT
026600070807     C*
026700070807     C* Effettuo scrittura del file FIVAX
026800070807     C                   exsr      wriVAX
026900051216     C*
027000051216     C                   endif
027100051021     C*
027200070228     C* ****************  NUOVA ROTTURA ************************
027300051021     C                   else
027400051021     C*
027500051021     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
027600051021     C* dell'ultima bolla accorpata
027700051021     C                   if        wGiro > 0
027800051021     C                   exsr      wriVAB
027900051021     C                   eval      wGiro = *zeros
028000051021     C                   endif
028100051021     C*
028200051021     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
028300051021     C                   eval      SAVEDIABD  = DSEDIABD
028400051021     C*
028500051021     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
028600051021     C                   exsr      exeINZ
028700051021     C*
028800051021     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
028900081016     C   60              exsr      repnum
029000051021     C                   exsr      impVAB
029100070227     C*
029200070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
029300070227     C   60              exsr      wriRAB
029400051021     C*
029500051021     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
029600051021     C                   exsr      wriVAD
029700051021     C*
029800051021     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
029900051021     C                   exsr      wriVAT
030000070807     C*
030100070807     C* Effettuo scrittura del file FIVAX x dettaglio bolla "master"
030200070807     C                   exsr      wriVAX
030300031110     C*
030400031110     C                   endif
030500031110     C*
030600031110     C* Incremento il contatore che identifica il 1° giro
030700031114     C                   eval      wGiro = wGiro + 1
030800051102     C*
030900051102     C* Salvo i nuovi attributi relativi al contrassegno
031000051102     C                   eval      SAV_vabCAS = SQL_vabCAS
031100051102     C                   eval      SAV_vabVCA = SQL_vabVCA
031200051102     C                   eval      SAV_vabTIC = SQL_vabTIC
031300051216     C*
031400051216     C* Come ultima cosa deleto il record bolla testata appena considerato
031500051216     C* ...solo se tutto ok => proseguo
031600051216     C                   if        wFlgErr = *zeros
031700040712     C     KEYvab1       delete    EDIVABQT1L
031800051216     C                   endif
031900040709     C*
032000040709     C                   endif
032100031110     C*
032200031110     C/EXEC SQL
032300051216     C+ Fetch C1 into :DSEDIVAB
032400031110     C/END-EXEC
032500081013     C*
032600081013     C* Se richiesto verifico se raggiunto massimo limite accorpamento bolle
032700081013     C                   if        wMaxVB > *zeros
032800081013     C                   if        wGiro  < wMaxVB
032900081013     C                   else
033000081013     C                   eval      wFlgMaxVB = 1
033100081013     C                   endif
033200081013     C                   endif
033300081013     C*
033400031110     C                   enddo
033500031114     C*
033600031114     C* Scarico la bolla "master" in sospeso
033700031114     C* dell'ultima bolla accorpata
033800031114     C                   exsr      wriVAB
033900081013     C*
034000081013     C                   endif
034100081013     C*
034200081013     C* Al termine chiudo i file EDI*QT temporanei
034300081013     C                   close     EDIVABQT1L
034400081013     C                   close     EDIVADQT1L
034500081013     C                   close     EDIVATQT1L
034600081013     C                   close     EDIVAXQT1L
034700081013     C*
034800081013     C/EXEC SQL
034900081013     C+ close C1
035000081013     C/END-EXEC
035100081013     C*
035200081013     C                   enddo
035300991112     C*
035400991112     C                   ENDSR
035500051216     C*------------------------------------------------------------------------*
035600051216     C* EXEABDRC - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
035700051216     C*------------------------------------------------------------------------*
035800051216     C     EXEABDRC      BEGSR
035900081013     C*
036000081013     C                   setoff                                       25
036100081013     C*
036200081013     C* Ciclo finchè nn diversamente specificato da condizionamenti a run-time
036300081013     C                   dou       *in25
036400051216     C*
036500051216     C* Inizializzo indicatori d wrk
036600051216     C                   setoff                                       51
036700051216     C                   seton                                        52
036800091002     C                   setoff                                       53
036900051216     C*
037000051216     C* Innanzitutto apro i file EDI*QT temporanei
037100051216     C                   open      EDIVABQT1L
037200051216     C                   open      EDIVADQT1L
037300051216     C                   open      EDIVATQT1L
037400070807     C                   open      EDIVAXQT1L
037500051216     C*
037600051216     C* Inizializzazioni
037700051216     C                   clear                   DSEDIVAB
037800051216     C                   clear                   DSEDIABD
037900051216     C                   clear                   SAVEDIABD
038000051216     C                   exsr      exeINZ
038100051216     C                   eval      wGiro = *zeros
038200051216     C                   seton                                        55
038300081013     C                   eval      wFlgMaxVB = *zeros
038400051216     C*
038500051216     C/EXEC SQL
038600051216     C+ declare C2 cursor for select * from edivabqt
038700051216     C+ where (vabGMA = 'RC' or vabGMA = '  ') and vabATB = ' ' and vabxco=' '
038800051216     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
038900051216     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
039000051216     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
039100051216     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
039200051216     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
039300051216     C+ for fetch only
039400051216     C/END-EXEC
039500051216     C
039600051216     C/EXEC SQL
039700051216     C+ open C2
039800051216     C/END-EXEC
039900051216     C
040000051216     C/EXEC SQL
040100051216     C+ Fetch C2 into :DSEDIVAB
040200051216     C/END-EXEC
040300051216     C*
040400081013     C                   if        sqlcod <> *zeros
040500081013     C                   seton                                        25
040600081013     C                   else
040700081013     C                   dow       sqlcod = *zeros AND wFlgMaxVB = *zeros
040800051216     C*
040900051216     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
041000051216     C                   exsr      CHKREC
041100051216     C*
041200051216     C* Se ok proseguo
041300051216     C                   if        wRECOK = 'S'
041400091002     C*
041500091002     C* Chiodo x TTL (cod. 0504453 - 0504450)
041600091002     C                   if        SQL_vabCCM = 0504453 or
041700091002     C                             SQL_vabCCM = 0504450
041800091002     C                   seton                                        53
041900091002     C                   endif
042000051216     C*
042100051216     C* Eseguo routine d valorizzazione DS d livello rottura
042200051216     C                   exsr      valDSROT
042300051216     C*
042400051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
042500051216     C                   z-add     *zeros        wFlgErr           1 0
042600051216     C*
042700051216     C* Verifico avvenuta rottura d codice
042800051216     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
042900051216     C                             (DSEDIABD=SAVEDIABD     AND
043000051216     C                              SQL_vabCAS>*zeros      AND
043100051216     C                             (SQL_vabVCA=SAV_vabVCA  AND
043200051216     C                              SQL_vabTIC=SAV_vabTIC   OR
043300051216     C                              SAV_vabCAS=*zeros)     AND
043400051216     C                              wAccCAS='S')
043500051216     C*
043600051216     C* ****************  ESEGUO ACCORPAMENTO **********************
043700051216     C*
043800051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
043900051216     C                   exsr      exeRC
044000051216     C*
044100051216     C* Se fin qui tutto ok => proseguo
044200051216     C                   if        wFlgErr = *zeros
044300051216     C*
044400051216     C* Eseguo la sommatoria dei campi da raggruppare
044500051216     C                   exsr      exeSOMMA
044600051216     C*
044700051216     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
044800051216     C                   exsr      exeNORM
044900070227     C*
045000070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
045100070227     C   60              exsr      wriRAB
045200051216     C*
045300051216     C* Effettuo scrittura del file FIVAD
045400051216     C                   exsr      wriVAD
045500051216     C*
045600051216     C* Effettuo scrittura del file FIVAT
045700051216     C                   exsr      wriVAT
045800070807     C*
045900070807     C* Effettuo scrittura del file FIVAX
046000070807     C                   exsr      wriVAX
046100051216     C*
046200051216     C                   endif
046300051216     C*
046400070228     C* ****************  NUOVA ROTTURA ************************
046500051216     C                   else
046600051216     C*
046700051216     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
046800051216     C* dell'ultima bolla accorpata
046900051216     C                   if        wGiro > 0
047000051216     C                   exsr      wriVAB
047100051216     C                   eval      wGiro = *zeros
047200051216     C                   endif
047300051216     C*
047400051216     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
047500051216     C                   eval      SAVEDIABD  = DSEDIABD
047600051216     C*
047700051216     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
047800051216     C                   exsr      exeINZ
047900051216     C*
048000051216     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
048100081016     C   60              exsr      repnum
048200070227     C                   exsr      impVAB
048300070227     C*
048400070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
048500070227     C   60              exsr      wriRAB
048600051216     C*
048700051216     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
048800051216     C                   exsr      wriVAD
048900051216     C*
049000051216     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
049100051216     C                   exsr      wriVAT
049200070807     C*
049300070807     C* Effettuo scrittura del file FIVAX x dettaglio bolla "master"
049400070807     C                   exsr      wriVAX
049500051216     C*
049600051216     C                   endif
049700051216     C*
049800051216     C* Incremento il contatore che identifica il 1° giro
049900051216     C                   eval      wGiro = wGiro + 1
050000051216     C*
050100051216     C* Salvo i nuovi attributi relativi al contrassegno
050200051216     C                   eval      SAV_vabCAS = SQL_vabCAS
050300051216     C                   eval      SAV_vabVCA = SQL_vabVCA
050400051216     C                   eval      SAV_vabTIC = SQL_vabTIC
050500051216     C*
050600051216     C* Come ultima cosa deleto il record bolla testata appena considerato
050700051216     C* ...solo se tutto ok => proseguo
050800051216     C                   if        wFlgErr = *zeros
050900051216     C     KEYvab1       delete    EDIVABQT1L
051000051216     C                   endif
051100051216     C*
051200051216     C                   endif
051300051216     C*
051400051216     C/EXEC SQL
051500051216     C+ Fetch C2 into :DSEDIVAB
051600051216     C/END-EXEC
051700081013     C*
051800081013     C* Se richiesto verifico se raggiunto massimo limite accorpamento bolle
051900081013     C                   if        wMaxVB > *zeros
052000081013     C                   if        wGiro  < wMaxVB
052100081013     C                   else
052200081013     C                   eval      wFlgMaxVB = 1
052300081013     C                   endif
052400081013     C                   endif
052500081013     C*
052600051216     C                   enddo
052700051216     C*
052800051216     C* Scarico la bolla "master" in sospeso
052900051216     C* dell'ultima bolla accorpata
053000051216     C                   exsr      wriVAB
053100081013     C*
053200081013     C                   endif
053300081013     C*
053400081013     C* Al termine chiudo i file EDI*QT temporanei
053500081013     C                   close     EDIVABQT1L
053600081013     C                   close     EDIVADQT1L
053700081013     C                   close     EDIVATQT1L
053800081013     C                   close     EDIVAXQT1L
053900081013     C*
054000081013     C/EXEC SQL
054100081013     C+ close C2
054200081013     C/END-EXEC
054300081013     C*
054400081013     C                   enddo
054500051216     C*
054600051216     C                   ENDSR
054700031110     C*------------------------------------------------------------------------*
054800040712     C* EXENOABD - SCRITTIURA FILE FIVAB/FIVAD X BOLLE NN RAGGRUPPABILI
054900031110     C*------------------------------------------------------------------------*
055000031110     C     EXENOABD      BEGSR
055100050620     C*
055200050620     C* Inizializzo indicatori d wrk
055300050620     C                   setoff                                       51
055400040712     C*
055500040712     C* Innanzitutto apro i file EDI*QT temporanei
055600040712     C                   open      EDIVABQT1L
055700040712     C                   open      EDIVADQT1L
055800040712     C                   open      EDIVATQT1L
055900070807     C                   open      EDIVAXQT1L
056000040712     C*
056100040712     C* Porto tutto ciò che è rimasto nei file EDI*QT nei relativi FIVA*
056200040712     C*
056300040712     C* ...VAB
056400040712     C     *loval        setll     EDIVABQT1L
056500040712     C                   read      EDIVABQT1L
056600040712     C                   dow       not %eof(EDIVABQT1L)
056700040712     C                   write     FIVAB000
056800040712     C                   delete    EDIVABQT1L
056900040712     C                   read      EDIVABQT1L
057000040712     C                   enddo
057100040712     C*
057200040712     C* ...VAD
057300040712     C     *loval        setll     EDIVADQT1L
057400040712     C                   read      EDIVADQT1L
057500040712     C                   dow       not %eof(EDIVADQT1L)
057600040712     C                   write     FIVAD000
057700040712     C                   delete    EDIVADQT1L
057800040712     C                   read      EDIVADQT1L
057900040712     C                   enddo
058000040712     C*
058100040712     C* ...VAT
058200040712     C     *loval        setll     EDIVATQT1L
058300040712     C                   read      EDIVATQT1L
058400040712     C                   dow       not %eof(EDIVATQT1L)
058500040712     C                   write     FIVAT000
058600040712     C                   delete    EDIVATQT1L
058700040712     C                   read      EDIVATQT1L
058800040712     C                   enddo
058900070807     C*
059000070807     C* ...VAX
059100070807     C     *loval        setll     EDIVAXQT1L
059200070807     C                   read      EDIVAXQT1L
059300070807     C                   dow       not %eof(EDIVAXQT1L)
059400070807     C                   write     FIVAX000
059500070807     C                   delete    EDIVAXQT1L
059600070807     C                   read      EDIVAXQT1L
059700070807     C                   enddo
059800040714     C*
059900070807     C** ...VAX
060000070807     C*     KEYvax2_C     setll     EDIVAX2L
060100070807     C*     KEYvax2_C     reade     EDIVAX2L
060200070807     C*                   dow       not %eof(EDIVAX2L)
060300070807     c*     vaxfgs        lookup    lin                                    50
060400070807     c*                   if        *in50  and vaxfgs>0
060500070807     C*                   write     FIVAX000
060600070807     C*                   delete    EDIVAX2L
060700070807     c*                   endif
060800070807     c*
060900070807     C*     KEYvax2_C     reade     EDIVAX2L
061000070807     C*                   enddo
061100040712     C*
061200040714     C* Al termine chiudo i file EDI* temporanei
061300040712     C                   close     EDIVABQT1L
061400040712     C                   close     EDIVADQT1L
061500040712     C                   close     EDIVATQT1L
061600070807     C                   close     EDIVAXQT1L
061700031110     C*
061800031110     C                   ENDSR
061900031110     C*------------------------------------------------------------------------*
062000031110     C* EXEINZ - ROUTINE D INIZIALIZZAZIONE X OGNI NUOVO LIVELLO D ROTTURA BOLLE
062100031110     C*------------------------------------------------------------------------*
062200031110     C     EXEINZ        BEGSR
062300031110     C*
062400031110     C                   clear                   DSEDIVABW
062500031114     C                   clear                   wRMO
062600050620     C                   clear                   wNOT
062700050620     C                   clear                   wNT2
062800080929     C                   clear                   wNOTNT2
062900051215     C                   eval      wFlgOrmRC = *off
063000031110     C*
063100031110     C                   ENDSR
063200031110     C*------------------------------------------------------------------------*
063300031110     C* VALDSROT - VALORIZZAZIONE DS LIVELLO ROTTURA
063400031110     C*------------------------------------------------------------------------*
063500031112     C     VALDSROT      BEGSR
063600031110     C*
063700031110     C* Campi in rottura
063800031114     C                   EVAL      S_vabCCM = SQL_vabCCM
063900031110     C                   EVAL      S_vabRSD = SQL_vabRSD
064000031110     C                   EVAL      S_vabRD2 = SQL_vabRD2
064100031110     C                   EVAL      S_vabIND = SQL_vabIND
064200031110     C                   EVAL      S_vabCAD = SQL_vabCAD
064300031110     C                   EVAL      S_vabLOD = SQL_vabLOD
064400031110     C                   EVAL      S_vabPRD = SQL_vabPRD
064500031110     C                   EVAL      S_vabNZD = SQL_vabNZD
064600031110     C                   EVAL      S_vabLNA = SQL_vabLNA
064700091002     C  N53              EVAL      S_vabZNC = SQL_vabZNC
064800031110     C                   EVAL      S_vabCBO = SQL_vabCBO
064900031110     C                   EVAL      S_vabTSP = SQL_vabTSP
065000031110     C                   EVAL      S_vabCTR = SQL_vabCTR
065100031110     C                   EVAL      S_vabGC1 = SQL_vabGC1
065200031110     C                   EVAL      S_vabGC2 = SQL_vabGC2
065300031110     C                   EVAL      S_vabFFD = SQL_vabFFD
065400031110     C                   EVAL      S_vabDCR = SQL_vabDCR
065500031110     C                   EVAL      S_vabTCR = SQL_vabTCR
065600031110     C                   EVAL      S_vabHCR = SQL_vabHCR
065700051216     C  N52              EVAL      S_vabGMA = SQL_vabGMA
065800031110     C                   EVAL      S_vabGGA = SQL_vabGGA
065900031110     C                   EVAL      S_vabGVA = SQL_vabGVA
066000031110     C                   EVAL      S_vabTC1 = SQL_vabTC1
066100031110     C                   EVAL      S_vabTC2 = SQL_vabTC2
066200031110     C                   EVAL      S_vabVAS = SQL_vabVAS
066300031110     C                   EVAL      S_vabVAD = SQL_vabVAD
066400031110     C*
066500031110     C                   ENDSR
066600031110     C*------------------------------------------------------------------------*
066700031110     C* IMPVAB - VALORIZZAZIONE CAMPI "FISSI" 1° BOLLA D OGNI LIVELLO D ROTTURA
066800031110     C*------------------------------------------------------------------------*
066900031110     C     IMPVAB        BEGSR
067000031110     C*
067100031110     C* Valorizzo la DS d wrk da quella d lettura SQL
067200031110     C                   EVAL      DSEDIVABW = DSEDIVAB
067300031110     C*
067400031110     C                   ENDSR
067500051216     C*------------------------------------------------------------------------*
067600051216     C* EXERC - GESTIONE EVENTUALE PRESENZA PARTICOLARITA' CONSEGNA 'RC'
067700051216     C*------------------------------------------------------------------------*
067800051216     C     EXERC         BEGSR
067900051216     C*
068000051216     C* Gestisco anche l'accorpamento anche dell'ORM da "RC"
068100051216     C* ...ma solo se trattasi d primo "RC" delle bolle oggetto d accorpamento
068200060126     C**!!!              IF        SQL_vabGMA = 'RC' and
068300060126     C**!!!                        wFlgOrmRC = *off
068400070227 MB 1C                   IF        SQL_vabGMA = 'RC'
068500070227 MB 2c                   If        wflgormrc = *On
068600060127     C*
068700060127     C* Elimino l'ORM RC della bolla corrente (accorpata)
068800060126     c                   Clear                   Fnls25ds
068900060126     c                   Eval      i25ksc  = sql_vabccm
069000060126     c                   Move(p)   sql_vabaas    i25aas
069100060126     c                   Eval      i25spe = %editc(sql_vablnp:'X') +
069200060126     c                                      %editc(sql_vabnrs:'X') +
069300060126     c                                      %editc(sql_vabnsp:'X')
069400060126     c                   Call      'FNLS25R'
069500060126     c                   Parm                    Fnls25ds
069600060126    2C                   ENDIF
069700060126    2C                   If        wFlgOrmRC = *off
069800060127     C*
069900060127     C* Aggiorno l'orm RC legato alla bolla risultante dall'accorpamento solo se
070000060127     C* bolla corrente (accorpata) e bolla accorpante hanno chiave diversa.
070100060127     C                   IF        SQL_vabAAS <> WRK_vabAAS OR
070200060127     C                             SQL_vabLNP <> WRK_vabLNP OR
070300060127     C                             SQL_vabNRS <> WRK_vabNRS OR
070400060127     C                             SQL_vabNSP <> WRK_vabNSP
070500060127     C*
070600060127     C* Prima dell'aggiornamento del riferimento ORM RC con la nuova chiave spedizione
070700060127     C* elimino l'ORM RC della bolla accorpante
070800060127     c                   Clear                   Fnls25ds
070900060127     c                   Eval      i25ksc  = sql_vabccm
071000060127     c                   Move(p)   sql_vabaas    i25aas
071100060127     c                   Eval      i25spe = %editc(WRK_vabLNP:'X') +
071200060127     c                                      %editc(WRK_vabNRS:'X') +
071300060127     c                                      %editc(WRK_vabNSP:'X')
071400060127     c                   Call      'FNLS25R'
071500060127     c                   Parm                    Fnls25ds
071600060127     C*
071700060127     C* Quindi aggiorno l'ORM RC della bolla corrente (accorpata) con la nuova chiave
071800060127     C* spedizione (bolla accorpante)
071900051216     C                   CLEAR                   wRC_KEY
072000051216     C                   EVAL      wRC_AAS = SQL_vabAAS
072100051216     C                   EVAL      wRC_LNP = SQL_vabLNP
072200051216     C                   EVAL      wRC_NRS = SQL_vabNRS
072300051216     C                   EVAL      wRC_NSP = SQL_vabNSP
072400051220     C                   EVAL      I24TLA  = 'L'
072500051216     C                   EVAL      I24OPE  = 'U'
072600051216     C                   EVAL      I24KSC  = SQL_vabCCM
072700051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
072800051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
072900051216     C                   EVAL      I24NAAS = WRK_vabAAS
073000051216     C                   EVAL      I24LNP  = WRK_vabLNP
073100051216     C                   EVAL      I24NRS  = WRK_vabNRS
073200051216     C                   EVAL      I24NSP  = WRK_vabNSP
073300051216     C                   CALL      'FNLS24R'
073400051216     C                   PARM                    FNLS24DS
073500060127     C*
073600051216     C* Se tutto ok => accendo l'indicatore x nn aggiornare ORM generati da bolle "RC"
073700051216     C* accorpate successivamente (ovviamente x medesimo destinatario)
073800060126    3C                   IF        O24ERR = *zeros
073900051216     C                   EVAL      wFlgOrmRC = *on
074000060126   x3C                   ELSE
074100051216     C                   Z-ADD     1             wFlgErr
074200060126    3C                   ENDIF
074300060127     C                   ENDIF
074400060126    2C                   ENDIF
074500060126    1C                   ENDIF
074600051216     C*
074700051216     C                   ENDSR
074800031110     C*------------------------------------------------------------------------*
074900031110     C* EXESOMMA - SOMMATORIA CAMPI X RAGGRUPPAMENTO
075000031110     C*------------------------------------------------------------------------*
075100031110     C     EXESOMMA      BEGSR
075200031110     C*
075300051216     C* Eseguo sommatoria dati bolle
075400031110     C                   EVAL      WRK_vabIAS = WRK_vabIAS + SQL_vabIAS
075500031110     C                   EVAL      WRK_vabNCL = WRK_vabNCL + SQL_vabNCL
075600031110     C                   EVAL      WRK_vabPKB = WRK_vabPKB + SQL_vabPKB
075700031110     C                   EVAL      WRK_vabVLB = WRK_vabVLB + SQL_vabVLB
075800031110     C                   EVAL      WRK_vabQFT = WRK_vabQFT + SQL_vabQFT
075900031110     C                   EVAL      WRK_vabVMD = WRK_vabVMD + SQL_vabVMD
076000051021     C                   EVAL      WRK_vabCAS = WRK_vabCAS + SQL_vabCAS
076100031110     C*
076200031110     C                   ENDSR
076300031110     C*------------------------------------------------------------------------*
076400031110     C* EXENORM - NORMALIZZAZIONE CAMPI X OTTENIMENTO BOLLA "MASTER"
076500031110     C*------------------------------------------------------------------------*
076600031110     C     EXENORM       BEGSR
076700050620     C*
076800050620     C* Valorizzo indicatore d wrk
076900050620     C                   SETON                                        51
077000031110     C*
077100031114     C                   IF        WRK_vabNAS =  *blanks AND
077200031114     C                             SQL_vabNAS <> *blanks
077300031110     C                   EVAL      WRK_vabNAS = SQL_vabNAS
077400031110     C                   ENDIF
077500031110     C*
077600031114     C                   IF        WRK_vabCTM =  *blanks AND
077700031114     C                             SQL_vabCTM <> *blanks
077800031110     C                   EVAL      WRK_vabCTM = SQL_vabCTM
077900031110     C                   ENDIF
078000031110     C*
078100031114     C                   IF        WRK_vabCTS =  *blanks AND
078200031114     C                             SQL_vabCTS <> *blanks
078300031110     C                   EVAL      WRK_vabCTS = SQL_vabCTS
078400031110     C                   ENDIF
078500031110     C*
078600031114     C                   IF        WRK_vabFTM =  *blanks AND
078700031114     C                             SQL_vabFTM <> *blanks
078800031110     C                   EVAL      WRK_vabFTM = SQL_vabFTM
078900031110     C                   ENDIF
079000031110     C*
079100031114     C                   IF        WRK_vabSCL =  *blanks AND
079200031114     C                             SQL_vabSCL <> *blanks
079300031110     C                   EVAL      WRK_vabSCL = SQL_vabSCL
079400031110     C                   ENDIF
079500051021     C*
079600051021     C                   IF        WRK_vabVCA =  *blanks AND
079700051021     C                             SQL_vabVCA <> *blanks
079800051021     C                   EVAL      WRK_vabVCA = SQL_vabVCA
079900051021     C                   ENDIF
080000051021     C*
080100051021     C                   IF        WRK_vabTIC =  *blanks AND
080200051021     C                             SQL_vabTIC <> *blanks
080300051021     C                   EVAL      WRK_vabTIC = SQL_vabTIC
080400051021     C                   ENDIF
080500051216     C*
080600051216     C                   IF        WRK_vabGMA =  *blanks AND
080700051216     C                             SQL_vabGMA <> *blanks
080800051216     C                   EVAL      WRK_vabGMA = SQL_vabGMA
080900051216     C                   ENDIF
081000031110     C*
081100031110     C* Gestisco comportamento particolare del campo VABANT (tipo e numero bancali)
081200031110     C                   IF        WRK_vabANT = *zeros
081300031110     C                   EVAL      WRK_vabANT = SQL_vabANT
081400031110     C                   ELSE
081500031110     C                   IF        SQL_vabANT = *zeros
081600031110     C                   ELSE
081700031110     C                   EVAL      DSVABANT = SQL_vabANT
081800031110     C                   EVAL      wANTTIP1 = ANTTIP1
081900031110     C                   EVAL      wANTNUM1 = ANTNUM1
082000031110     C                   EVAL      wANTTIP2 = ANTTIP2
082100031110     C                   EVAL      wANTNUM2 = ANTNUM2
082200031110     C                   EVAL      DSVABANT = WRK_vabANT
082300031110     C                   IF        ANTTIP1 > *zeros
082400031110     C                   IF        wANTTIP1 = ANTTIP1
082500031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM1
082600031110     C                   ENDIF
082700031110     C                   IF        wANTTIP2 = ANTTIP1
082800031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM2
082900031110     C                   ENDIF
083000031110     C                   ELSE
083100031110     C                   IF        wANTTIP1 > *zeros
083200031110     C                   EVAL      ANTTIP1 = wANTTIP1
083300031110     C                   EVAL      ANTNUM1 = wANTNUM1
083400031110     C                   ELSE
083500031110     C                   IF        wANTTIP2 > *zeros
083600031110     C                   EVAL      ANTTIP1 = wANTTIP2
083700031110     C                   EVAL      ANTNUM1 = wANTNUM2
083800031110     C                   ENDIF
083900031110     C                   ENDIF
084000031110     C                   ENDIF
084100031110     C                   IF        ANTTIP2 > *zeros
084200031110     C                   IF        wANTTIP1 = ANTTIP2
084300031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM1
084400031110     C                   ENDIF
084500031110     C                   IF        wANTTIP2 = ANTTIP2
084600031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM2
084700031110     C                   ENDIF
084800031110     C                   ELSE
084900031110     C                   IF        wANTTIP1 > *zeros
085000031110     C                   EVAL      ANTTIP2 = wANTTIP1
085100031110     C                   EVAL      ANTNUM2 = wANTNUM1
085200031110     C                   ELSE
085300031110     C                   IF        wANTTIP2 > *zeros
085400031110     C                   EVAL      ANTTIP2 = wANTTIP2
085500031110     C                   EVAL      ANTNUM2 = wANTNUM2
085600031110     C                   ENDIF
085700031110     C                   ENDIF
085800031110     C                   ENDIF
085900031110     C                   EVAL      WRK_vabANT = DSVABANT
086000031110     C                   ENDIF
086100031110     C                   ENDIF
086200031114     C*
086300051021     C* Eseguo raggruppamento dei riferimenti delle testate bolle accorpate
086400051021     C                   IF        wLenRIF > *zeros
086500050906     C                   IF        wTIPABD = 'S'
086600050614     C                   EVAL      wRIF = %trim(%editc(SQL_vabRMN:'4'))
086700050610     C                   ENDIF
086800050610     C                   IF        wTIPABD = 'A'
086900050610     C                   EVAL      wRIF = %trim(SQL_vabRMA)
087000050610     C                   ENDIF
087100050610     C*
087200050610     C* Gestisco l'allineamento d default a seconda della tipologia d riferimento da utilizzare
087300050610     C                   IF        wAlmRIF = *blanks
087400050610     C                   IF        wTipABD = 'S'
087500050610     C                   EVAL      wAlmRIF = 'D'
087600050610     C                   ENDIF
087700050610     C                   IF        wTipABD = 'A'
087800050610     C                   EVAL      wAlmRIF = 'S'
087900050610     C                   ENDIF
088000050610     C                   ENDIF
088100051021     C*
088200051021     C* Effettuo le operazioni seguenti SOLO in caso in cui il riferimento voluto
088300051021     C* risulti essere effettivamente valorizzato
088400051021     C                   IF        wRIF <> *blanks
088500050614     C*
088600050614     C* Considero la minore tra la lunghezza reale del riferimento da considerare e il
088700050614     C* numero d byte da considerare indicati in tabella x il cliente corrente
088800050614     C                   Z-ADD     *zeros        wByteRIF          2 0
088900050614     C                   IF        wLenRIF <= %len(%trim(wRIF))
089000050614     C                   EVAL      wByteRIF = wLenRIF
089100050614     C                   ELSE
089200050614     C                   EVAL      wByteRIF = %len(%trim(wRIF))
089300050614     C                   ENDIF
089400050610     C*
089500050610     C* A questo punto verifico se sono richiesti solo 1 numero specifico d byte ed il
089600050610     C* relativo allineamento
089700050610     C                   IF        wLenRIF <> 99
089800050610     C                   IF        wAlmRIF = 'S'
089900050614     C                   EVAL      wRIF = %subst(%trim(wRIF):1:wByteRIF)
090000050610     C                   ENDIF
090100050610     C                   IF        wAlmRIF = 'D'
090200050610     C                   EVAL      wRIF = %subst(%trim(wRIF):
090300050614     C                                    %len(%trim(wRIF))-wByteRIF+1)
090400050610     C                   ENDIF
090500050610     C                   ENDIF
090600051021     C*
090700051021     C                   ELSE
090800051021     C                   MOVEL     'N'           wOltreRIF
090900051021     C                   ENDIF
091000050610     C*
091100031114     C* Se c stanno...
091200050610     C                   IF        %len(%trim(wRMO))+1+%len(%trim(wRIF)) <=
091300040517     C                             %len(wRMO)-4
091400050610     C                   EVAL      wRMO = %trim(wRMO) + %trim(wRIF) + '-'
091500050610     C                   MOVEL     'N'           wOltreRIF         1
091600050610     C                   ELSE
091700050610     C                   MOVEL     'S'           wOltreRIF
091800031114     C                   ENDIF
091900050906     C*
092000050906     C                   ELSE
092100050906     C                   MOVEL     'N'           wOltreRIF
092200050906     C                   ENDIF
092300051021     C*
092400050620     C*
092500050620     C* Se richiesto raggruppo anche le note
092600080929     C                   IF        wRgrNOT <> *blanks
092700060726     C* Memorizzo subito note originali
092800080929     C                   IF        wRgrNOT = 'S'
092900060726     C                   IF        wNOT = *blanks AND WRK_vabNOT <> *blanks AND
093000060726     C                             %scan(%trim(WRK_vabNOT):wNT2) = 0
093100060726     C                   EVAL      wNOT = %trim(WRK_vabNOT)
093200060726     C                   ENDIF
093300060726     C                   IF        wNT2 = *blanks AND WRK_vabNT2 <> *blanks AND
093400060726     C                             %scan(%trim(WRK_vabNT2):wNOT) = 0
093500060726     C                   EVAL      wNT2 = %trim(WRK_vabNT2)
093600060726     C                   ENDIF
093700080929     C                   ENDIF
093800080929     C                   IF        wRgrNOT = 'U'
093900080929     C                   IF        wNOTNT2=*blanks AND
094000080929     C                             (WRK_vabNOT<>*blanks OR WRK_vabNT2<>*blanks)
094100080929     C                             AND
094200080929     C                             %scan(%trim(WRK_vabNOT)+%trim(WRK_vabNT2):
094300080929     C                             wNOTNT2)=0
094400080929     C                   EVAL      wNOTNT2=%trim(WRK_vabNOT)+ ' '+
094500080929     C                                     %trim(WRK_vabNT2)
094600080929     C                   ENDIF
094700080929     C                   ENDIF
094800080929     C*
094900080929     C                   IF        wRgrNOT = 'S'
095000060629     C***  VABNOT
095100060726     C* Se presente nota => gestisco
095200060726     C                   IF        SQL_vabNOT <> *blanks
095300080929     C* Innanzitutto verifico che la nota corrente nn sia già presente
095400060726     C                   SETOFF                                       40
095500060726     C                   IF        wNOT <> *blanks
095600060726     C                   IF        %scan(%trim(SQL_vabNOT):wNOT) <> 0 OR
095700060726     C                             %scan(%trim(SQL_vabNOT):wNT2) <> 0
095800060726     C                   SETON                                        40
095900060726     C                   ENDIF
096000060726     C                   ENDIF
096100060629     C* Verifico se lo spazio su NOT è già esaurito mentre quello su NT2 è ancora dispnibile
096200060726     C                   IF        *IN40 = *OFF
096300060629     C                   IF        (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNOT))
096400060629     C                             >%size(wNOT)) AND
096500060629     C                             (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNOT))
096600060629     C                             <=%size(wNT2))
096700060726     C                   IF        %scan(%trim(SQL_vabNOT):wNT2) = 0 AND
096800060726     C                             %scan(%trim(SQL_vabNOT):wNOT) = 0
096900060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNOT)
097000060726     C                   ENDIF
097100060629     C                   ELSE
097200050708     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNOT)
097300060726     C                   ENDIF
097400060726     C                   ENDIF
097500060629     C***  VABNT2
097600060726     C* Se presente nota => gestisco
097700060726     C                   IF        SQL_vabNT2 <> *blanks
097800060629     C* Innanzitutto verifico che il riferimento corrente nn sia già presente
097900060726     C                   SETOFF                                       40
098000060726     C                   IF        wNT2 <> *blanks
098100060726     C                   IF        %scan(%trim(SQL_vabNT2):wNT2) <> 0 OR
098200060726     C                             %scan(%trim(SQL_vabNT2):wNOT) <> 0
098300060726     C                   SETON                                        40
098400060726     C                   ENDIF
098500060726     C                   ENDIF
098600060629     C* Verifico se lo spazio su NT2 è già esaurito mentre quello su NOT è ancora dispnibile
098700060726     C                   IF        *IN40 = *OFF
098800060629     C                   IF        (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNT2))
098900060629     C                             >%size(wNT2)) AND
099000060629     C                             (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNT2))
099100060629     C                             <=%size(wNOT))
099200060726     C                   IF        %scan(%trim(SQL_vabNT2):wNOT) = 0 AND
099300060726     C                             %scan(%trim(SQL_vabNT2):wNT2) = 0
099400060629     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNT2)
099500060726     C                   ENDIF
099600060629     C                   ELSE
099700060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNT2)
099800060629     C                   ENDIF
099900060629     C                   ENDIF
100000060726     C                   ENDIF
100100080929     C                   ENDIF
100200080929     C                   ENDIF
100300080929     C*
100400080929     C                   IF        wRgrNOT = 'U'
100500080929     C
100600080929     C                   IF        (%len(%trim(wNOTNT2))+1+
100700080929     C                              %len(%trim(SQL_vabNOT))+
100800080929     C                              %len(%trim(SQL_vabNT2)))
100900080929     C                             <=%size(wNOTNT2)
101000080929     C                   EVAL      wNOTNT2 = %trim(wNOTNT2)    + ' ' +
101100080929     C                                       %trim(SQL_vabNOT) + ' ' +
101200080929     C                                       %trim(SQL_vabNT2)
101300080929     C                   ENDIF
101400080929     C                   ENDIF
101500060629     C*
101600050620     C                   ELSE
101700050708     C                   EVAL      wNOT = %trim(SQL_vabNOT)
101800050708     C                   EVAL      wNT2 = %trim(SQL_vabNT2)
101900050620     C                   ENDIF
102000031110     C*
102100031110     C                   ENDSR
102200031112     C*------------------------------------------------------------------------*
102300031112     C* WRIVAD - SCRITTIURA FILE FIVAD
102400031112     C*------------------------------------------------------------------------*
102500031112     C     WRIVAD        BEGSR
102600031112     C*
102700031112     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAD)
102800031112     C                   CLEAR                   FIVAD000
102900031112     C*
103000051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAD
103100040712     C     KEYva_1       SETLL     EDIVADQT1L
103200031113     C                   IF        %equal(EDIVADQT1L)
103300040227     C* Eseguo routine x modifica codice trattamento merce
103400040227     C                   EXSR      CHGCTM
103500040227     C*
103600040712     C     KEYva_1       READE     EDIVADQT1L
103700031113     C                   DOW       not %eof(EDIVADQT1L)
103800031112     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
103900031112     C                   EVAL      vadAAS = WRK_vabAAS
104000031112     C                   EVAL      vadLNP = WRK_vabLNP
104100031112     C                   EVAL      vadNRS = WRK_vabNRS
104200031112     C                   EVAL      vadNSP = WRK_vabNSP
104300070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
104400081016     C   60              EVAL      vadNSP = ACC_NSP
104500031112     C* Inserisco record d dettaglio in FIVAD
104600031112     C                   WRITE     FIVAD000
104700031112     C* Elimino record d dettaglio in EDIVAD
104800031113     C                   DELETE    EDIVADQT1L
104900031112     C* Continuo la lettura dei dettagli della testata bolla corrente
105000040712     C     KEYva_1       READE     EDIVADQT1L
105100031112     C                   ENDDO
105200031112     C*
105300031112     C* Se invece nn esiste già il dettaglio colli in EDIVAD e sulla testata c'è l'indicazioine
105400031112     C* dei segnacolli => provvedo a creare il VAD
105500031112     C                   ELSE
105600031112     C                   IF        SQL_vabNCD > *zeros OR
105700031112     C                             SQL_vabNCA > *zeros
105800040227     C* Eseguo routine x modifica codice trattamento merce
105900040227     C                   EXSR      CHGCTM
106000031112     C* Valorizzo la chiave bolla del dettaglio con quella della relativa testata bolla "master"
106100031112     C                   EVAL      vadAAS = WRK_vabAAS
106200031112     C                   EVAL      vadLNP = WRK_vabLNP
106300031112     C                   EVAL      vadNRS = WRK_vabNRS
106400031112     C                   EVAL      vadNSP = WRK_vabNSP
106500031112     C* Valorizzo i restanti campi con quelli della testata bolla corrente
106600031112     C                   EVAL      vadFGS = SQL_vabFGS
106700031112     C                   EVAL      vadCCM = SQL_vabCCM
106800031112     C                   EVAL      vadNCL = SQL_vabNCL
106900031112     C                   EVAL      vadNCD = SQL_vabNCD
107000031112     C                   EVAL      vadNCA = SQL_vabNCA
107100070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
107200081016     C   60              EVAL      vadNSP = ACC_NSP
107300031112     C* Inserisco record d dettaglio in FIVAD
107400031112     C                   WRITE     FIVAD000
107500031112     C                   ENDIF
107600031112     C                   ENDIF
107700031112     C*
107800031112     C                   ENDSR
107900031114     C*------------------------------------------------------------------------*
108000031114     C* WRIVAT - SCRITTIURA FILE FIVAT
108100031114     C*------------------------------------------------------------------------*
108200031114     C     WRIVAT        BEGSR
108300031114     C*
108400031114     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAT)
108500031114     C                   CLEAR                   FIVAT000
108600031114     C*
108700051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAT
108800040712     C     KEYva_1       SETLL     EDIVATQT1L
108900031114     C                   IF        %equal(EDIVATQT1L)
109000040227     C* Eseguo routine x modifica codice trattamento merce
109100040227     C                   EXSR      CHGCTM
109200040227     C*
109300040712     C     KEYva_1       READE     EDIVATQT1L
109400031114     C                   DOW       not %eof(EDIVATQT1L)
109500031114     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
109600031114     C                   EVAL      vatAAS = WRK_vabAAS
109700031114     C                   EVAL      vatLNP = WRK_vabLNP
109800031114     C                   EVAL      vatNRS = WRK_vabNRS
109900031114     C                   EVAL      vatNSP = WRK_vabNSP
110000070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
110100081016     C   60              EVAL      vatNSP = ACC_NSP
110200031114     C* Inserisco record d dettaglio in FIVAT
110300031114     C                   WRITE     FIVAT000
110400031114     C* Elimino record d dettaglio in EDIVAT
110500031114     C                   DELETE    EDIVATQT1L
110600031114     C* Continuo la lettura dei dettagli della testata bolla corrente
110700040712     C     KEYva_1       READE     EDIVATQT1L
110800031114     C                   ENDDO
110900031114     C                   ENDIF
111000031114     C*
111100031114     C                   ENDSR
111200070807     C*------------------------------------------------------------------------*
111300070807     C* WRIVAX - SCRITTIURA FILE FIVAX
111400070807     C*------------------------------------------------------------------------*
111500070807     C     WRIVAX        BEGSR
111600070809     C*
111700070809     C* Solo x record VAX diversi dalla bolla master (accorpante)
111800070809     C                   IF        SQL_vabFGS  = WRK_vabFGS AND
111900070809     C                             SQL_vabCCM  = WRK_vabCCM AND
112000070809     C                             SQL_vabAAS  = WRK_vabAAS AND
112100070809     C                             SQL_vabLNP  = WRK_vabLNP AND
112200070809     C                             SQL_vabNRS  = WRK_vabNRS AND
112300070809     C                             SQL_vabNSP <> WRK_vabNSP
112400070807     C*
112500070807     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAX)
112600070807     C                   CLEAR                   FIVAX000
112700070807     C*
112800070807     C                   Z-ADD     *zeros        wVAXmaster
112900071203     C                   Z-ADD     *zeros        wVAXmaster01
113000071203     C                   Z-ADD     *zeros        wVAXmaster02
113100070807     C*
113200070807     C* Effettuo conteggio righe attuali packing list della bolla master
113300070807     C*
113400070807     C/EXEC SQL
113500070905     C+ declare C1VX cursor for
113600070906     C+ select max(vaxprr) from edivaxqt
113700070911     C+ where
113800070911     C+ vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
113900070807     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
114000070807     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
114100070807     C+ for fetch only
114200070807     C/END-EXEC
114300070807     C
114400070807     C/EXEC SQL
114500070905     C+ open C1VX
114600070807     C/END-EXEC
114700070807     C
114800070807     C/EXEC SQL
114900070905     C+ Fetch C1VX into :wVAXmaster01
115000070807     C/END-EXEC
115100070905     C*
115200070905     C/EXEC SQL
115300070905     C+ declare C2VX cursor for
115400070905     C+ select max(vaxprr) from fivax00f
115500070911     C+ where
115600070911     C+ vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
115700070905     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
115800070905     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
115900070905     C+ for fetch only
116000070905     C/END-EXEC
116100070905     C
116200070905     C/EXEC SQL
116300070905     C+ open C2VX
116400070905     C/END-EXEC
116500070905     C
116600070905     C/EXEC SQL
116700070905     C+ Fetch C2VX into :wVAXmaster02
116800070905     C/END-EXEC
116900070809     C*
117000070905     C* Tengo il maggiore progressivo riga tra EDIVAX e FIVAX
117100070905     C                   EVAL      wVAXmaster = wVAXmaster01
117200070905     C                   IF        wVAXmaster02 > wVAXmaster01
117300070905     C                   EVAL      wVAXmaster = wVAXmaster02
117400070905     C                   ENDIF
117500070905     C*
117600070809     C* Proseguo elaborazione solo se effettivamente trovati VAX da accorpare
117700070905     C                   IF        wVAXmaster > *zeros
117800070807     C*
117900070807     C* Verifico se x la bolla corrente di EDIVAB esiste il relativo packing list in EDIVAX
118000070807     C     KEYva_1       SETLL     EDIVAXQT1L
118100070807     C                   IF        %equal(EDIVAXQT1L)
118200070809     C*
118300070809     C                   SETOFF                                       88
118400070807     C*
118500070807     C     KEYva_1       READE     EDIVAXQT1L
118600070807     C                   DOW       not %eof(EDIVAXQT1L)
118700070809     C*
118800070809     C* Se nn ancora gestito il salto pagina fittizio forzato
118900070809     C                   IF        *IN88 = *off
119000070809     C                   SETON                                        88
119100070905     C***                EXSR      GESFORMFEED
119200070809     C                   ENDIF
119300070809     C*
119400070807     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
119500070807     C                   EVAL      vaxAAS = WRK_vabAAS
119600070807     C                   EVAL      vaxLNP = WRK_vabLNP
119700070807     C                   EVAL      vaxNRS = WRK_vabNRS
119800070807     C                   EVAL      vaxNSP = WRK_vabNSP
119900070807     C* Auto-incremento numero progressivo riga packing list bolla master + bolla slave corrente
120000070905     C***                EVAL      vaxPRR = vaxPRR + wVAXmaster + wInsRighe
120100070905     C                   EVAL      vaxPRR = vaxPRR + wVAXmaster
120200070807     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
120300081016     C   60              EVAL      vaxNSP = ACC_NSP
120400070807     C* Inserisco record d dettaglio in FIVAX
120500070807     C                   WRITE     FIVAX000
120600070807     C* Elimino record d dettaglio in EDIVAX
120700070807     C                   DELETE    EDIVAXQT1L
120800070807     C* Continuo la lettura dei dettagli della testata bolla corrente
120900070807     C     KEYva_1       READE     EDIVAXQT1L
121000070807     C                   ENDDO
121100070807     C                   ENDIF
121200070807     C*
121300070809     C                   ENDIF
121400070809     C*
121500070807     C/EXEC SQL
121600070905     C+ close C1VX
121700070807     C/END-EXEC
121800070905     C*
121900070905     C/EXEC SQL
122000070905     C+ close C2VX
122100070905     C/END-EXEC
122200070905     C*
122300070809     C                   ENDIF
122400070807     C*
122500070807     C                   ENDSR
122600070809     C*------------------------------------------------------------------------*
122700070809     C* GESFORMFEED - GESTIONE FORZATURA SALTO PAGINA FITTIZIO
122800070809     C*------------------------------------------------------------------------*
122900070809     C     GESFORMFEED   BEGSR
123000070809     C*
123100070809     C* Salvo il formato record corrente
123200070809     C                   EVAL      DSEDIVAXSAV = DSEDIVAX
123300070809     C*
123400070809     C* Effettuo considerazioni x simulazione forzata salto pagina (ma solo se conviene davvero)
123500070809     C                   Z-ADD     *zeros        wRigheVuote       2 0
123600070809     C                   EVAL      wRigheVuote = wVAXmaster -
123700070809     C                             (42 * %div(wVAXmaster:42))
123800070809     C                   IF        wRigheVuote > *zeros
123900070809     C                   EVAL      wRigheVuote =
124000070809     C                             (42 * (%div(wVAXmaster:42)+1)) -
124100070809     C                             wVAXmaster
124200070809     C                   IF        wRigheVuote < 21
124300070809     C                   Z-ADD     wRigheVuote   wInsRighe         2 0
124400070809     C                   ELSE
124500070809     C                   Z-ADD     3             wInsRighe
124600070809     C                   ENDIF
124700070809     C*
124800070809     C                   Z-ADD     1             wRigaVuota        2 0
124900070809     C                   DOW       wRigaVuota <= wInsRighe
125000070809     C*
125100070809     C* Inserisco riga vuota sulla bolla master (accorpante) fino a capienza ultima pagina
125200070809     C                   EVAL      vaxAAS = WRK_vabAAS
125300070809     C                   EVAL      vaxLNP = WRK_vabLNP
125400070809     C                   EVAL      vaxNRS = WRK_vabNRS
125500070809     C                   EVAL      vaxNSP = WRK_vabNSP
125600070809     C                   EVAL      vaxPRR = wVAXmaster + wRigaVuota
125700070809     C                   EVAL      vaxDTT = *blanks
125800070809     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
125900081016     C   60              EVAL      vaxNSP = ACC_NSP
126000070809     C* Inserisco record d dettaglio in FIVAX
126100070809     C                   WRITE     FIVAX000
126200070809     C*
126300070809     C                   ADD       1             wRigaVuota
126400070809     C                   ENDDO
126500070809     C                   ENDIF
126600070809     C*
126700070809     C* Ripristino il formato record corrente
126800070809     C                   EVAL      DSEDIVAX = DSEDIVAXSAV
126900070809     C*
127000070809     C                   ENDSR
127100031112     C*------------------------------------------------------------------------*
127200031112     C* WRIVAB - SCRITTIURA FILE FIVAB X BOLLA "MASTER" ACCORPATA
127300031112     C*------------------------------------------------------------------------*
127400031112     C     WRIVAB        BEGSR
127500031112     C*
127600031112     C* Innanzitutto svuoto il buffer del record d testata d output (FIVAB)
127700031112     C                   CLEAR                   FIVAB000
127800031112     C* Valorizzo il buffer del record d testata d output (FIVAB)
127900031112     C                   EVAL      DSEDIVABOUT = DSEDIVABW
128000070420     C*
128100070420     C* Procedo solo se buffer effettivamente valorizzato
128200070420     C                   IF        DSEDIVABOUT <> DSEDIVABINZ
128300070420     C*
128400031112     C* Resetto alcuni campi che nella bolla "master" occcorrono NN valorizzati
128500031113     C   55              CLEAR                   vabNCD
128600031113     C   55              CLEAR                   vabNCA
128700080226     C   54
128800080226     CAN 55              CLEAR                   vabRMO
128900031114     C   55              CLEAR                   vabCMO
129000031114     C   55              CLEAR                   vabNMO
129100050620     C                   IF        *IN55 = *on
129200050620     C* Campo RIFERIMENTO MITTENTE
129300050701     C                   IF        wRMO <> *blanks
129400060530     C                   Z-ADD     wGiro         wGiro2
129500060530     C                   EVAL      vabRMO = 'N'+%editw(wGiro2:'  ')+':'+
129600040303     C                                      %trim(wRMO)
129700050610     C                   IF        wGiro > 1 AND wOltreRIF = 'S'
129800040514     C                   MOVE      '...'         vabRMO
129900040303     C                   ENDIF
130000050701     C                   ENDIF
130100080929     C* Campi NOTE
130200080929     C*
130300080929     C* ..gestisco tipo raggruppamento note
130400080929     C                   IF        wRgrNOT = 'U'
130500080929     C   51              EVAL      vabNOT = %subst(wNOTNT2:01:35)
130600080929     C   51              EVAL      vabNT2 = %subst(wNOTNT2:36:35)
130700080929     C                   ELSE
130800060726     C   51              EVAL      vabNOT = %trim(wNOT)
130900060726     C   51              EVAL      vabNT2 = %trim(wNT2)
131000080929     C                   ENDIF
131100080929     C*
131200040303     C                   ENDIF
131300070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
131400081016     C   60              EVAL      vabNSP = ACC_NSP
131500031112     C* Inserisco record d testata in FIVAB
131600031112     C                   WRITE     FIVAB000
131700070420     C                   ENDIF
131800050620     C*
131900050620     C* Inizializzo indicatori d wrk
132000050701     C                   SETOFF                                       51
132100031112     C*
132200031112     C                   ENDSR
132300070227     C*------------------------------------------------------------------------*
132400040709     C* CHKREC - CONTROLLO CORRETTEZZA RECORD TESTATA BOLLE AI FINI DEL RAGGRUPPAMENTO
132500040709     C*------------------------------------------------------------------------*
132600040709     C     CHKREC        BEGSR
132700040709     C*
132800040709     C* Inizializzo il flag d correttezza record testata bolle
132900040709     C                   MOVEL     'S'           wRECOK            1
133000051215     C*
133100051215     C* Verifica se bolla corrente presenta particolarità consegna RC e cliente nn abilitato
133200051215     C* specificatamente all'accorpamento anche con part. consegna RC
133300051215     C                   IF        wRECOK = 'S'
133400051216     C                   IF        SQL_vabGMA = 'RC'
133500051216     C                   IF        wAccRC <> 'S'
133600051215     C                   MOVEL     'N'           wRECOK
133700051216     C                   ELSE
133800051216     C* Se abilitazione ok controllo subito anche l'ORM relativo (quello che poi sarà aggiornato)
133900051216     C                   CLEAR                   wRC_KEY
134000051216     C                   EVAL      wRC_AAS = SQL_vabAAS
134100051216     C                   EVAL      wRC_LNP = SQL_vabLNP
134200051216     C                   EVAL      wRC_NRS = SQL_vabNRS
134300051216     C                   EVAL      wRC_NSP = SQL_vabNSP
134400051216     C                   CLEAR                   FNLS24DS
134500051216     C                   EVAL      I24TLA  = *blanks
134600051216     C                   EVAL      I24OPE  = 'L'
134700051216     C                   EVAL      I24KSC  = SQL_vabCCM
134800051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
134900051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
135000051216     C                   CALL      'FNLS24R'
135100051216     C                   PARM                    FNLS24DS
135200051216     C* Se tutto ok procedo altrimenti skippo la bolla corrente con particolarità RC ai fini
135300051216     C* dell'accorpamento
135400051216     C                   IF        O24ERR <> *zeros
135500051216     C                   MOVEL     'N'           wRECOK
135600051216     C                   ENDIF
135700051215     C                   ENDIF
135800051216     C                   ENDIF
135900051215     C                   ENDIF
136000040709     C*
136100040709     C* Verifica codice trattamento
136200051215     C                   IF        wRECOK = 'S'
136300040709     C                   EXSR      CHKCTM
136400051215     C                   ENDIF
136500040709     C*
136600040709     C                   ENDSR
136700040709     C*------------------------------------------------------------------------*
136800040709     C* CHKCTM - CONTROLLO CODICE TRATTAMENTO MERCE
136900040709     C*------------------------------------------------------------------------*
137000040709     C     CHKCTM        BEGSR
137100040709     C*
137200040709     C* Verifico che il codice trattamento merce della bolla corrente nn preveda il VAX
137300040709     C                   EVAL      tblKUT = 1
137400040709     C                   EVAL      tblCOD = '1B'
137500040709     C                   EVAL      tblKEY = SQL_vabCTM
137600040709     C     KEYtab        CHAIN     TABEL00F
137700040709     C                   IF        %found(TABEL00F)
137800040709     C                   EVAL      DS1B = tblUNI
137900070809     C                   IF        §1BF16 = 'S' AND wAccVX <> 'S'
138000040709     C                   MOVEL     'N'           wRECOK
138100040709     C                   ENDIF
138200040709     C                   ENDIF
138300040709     C*
138400040709     C                   ENDSR
138500040227     C*------------------------------------------------------------------------*
138600040227     C* CHGCTM - CAMBIO CODICE TRATTAMENTO MERCE
138700040227     C*------------------------------------------------------------------------*
138800040227     C     CHGCTM        BEGSR
138900040227     C*
139000040227     C* Se sulla testata bolle NN è espressamente indicato il codice tratamento merce
139100040227     C* (e naturalmente o c'è il dettaglio colli o l'indicazione in testata dei segnacolli
139200040227     C* sequenziali) imposto come default il CTM = '2D'
139300040227     C                   IF        WRK_vabCTM = *blanks
139400040227     C                   EVAL      WRK_vabCTM = '2D'
139500040227     C                   ENDIF
139600040227     C*
139700040227     C* Valorizzo la chiave x reperire le informazioni relative al codice
139800040227     C* trattamento merce da impostare in caso d raggruppamento bolle
139900040227     C                   EVAL      tblKUT = 1
140000040227     C                   EVAL      tblCOD = '1B'
140100040227     C                   EVAL      tblKEY = WRK_vabCTM
140200040227     C     KEYtab        CHAIN     TABEL00F
140300040227     C                   IF        %found(TABEL00F)
140400040227     C                   EVAL      DS1B = tblUNI
140500040227     C                   IF        §1BCTR <> *blanks
140600040227     C                   EVAL      WRK_vabCTM = §1BCTR
140700040227     C                   ENDIF
140800040227     C                   ENDIF
140900040227     C*
141000040227     C                   ENDSR
141100050610     C*------------------------------------------------------------------------*
141200051021     C* REPABD - REPERISCE I DATI TABELLATI CHE PILOTANO LE MODALITA DI ACCORPAMENTO
141300050610     C*------------------------------------------------------------------------*
141400050610     C     REPABD        BEGSR
141500050610     C*
141600050610     C                   EVAL      tblKUT = 1
141700050610     C                   EVAL      tblCOD = '3C'
141800050708     C                   MOVEL(P)  D86CCM        tblKEY
141900050610     C     KEYtab        CHAIN     TABEL00F
142000050610     C                   IF        %found(TABEL00F)
142100050610     C                   EVAL      DS3C = tblUNI
142200050610     C                   MOVEL     §3CABD        wTipABD
142300050610     C                   Z-ADD     §3CLRM        wLenRIF
142400050610     C                   MOVEL     §3CALM        wAlmRIF
142500050620     C                   MOVEL     §3CNOT        wRgrNOT
142600051021     C                   MOVEL     §3CABC        wAccCAS
142700051215     C                   MOVEL     §3CARC        wAccRC
142800070809     C                   MOVEL     §3CAVX        wAccVX
142900081013     C                   MOVEL     §3CMVR        wMaxVB
143000050610     C                   ENDIF
143100080226     C*
143200080226     C* Se lunghezza riferimento è zero accendo indicatore preposto x nn sovrascrittura RMO
143300080226     C                   if        wLenRIF <> *zeros
143400080226     C                   seton                                        54
143500080226     C                   endif
143600070228     C*
143700070228     C* Se cliente che necessita ritorno informazioni d accorpamento => accendo indicatore preposto
143800081006     C                   if        wTipABD <> *blanks
143900070228     C                   seton                                        60
144000070228     C                   else
144100070228     C                   setoff                                       60
144200070228     C                   endif
144300050610     C*
144400050610     C                   ENDSR
144500081016     C*------------------------------------------------------------------------*
144600081016     C* REPNUM - STACCA NUMERATORE X MANTENIMENTO NUMERO SPEDIZIONE
144700081016     C*------------------------------------------------------------------------*
144800081016     C     REPNUM        BEGSR
144900081016     C*
145000081016     C                   z-add     SQL_VABNSP    ACC_NSP
145100081016     C*
145200081016     C                   ENDSR
145300070227     C*------------------------------------------------------------------------*
145400070227     C* WRIRAB - SCRITTURA FILE RACCORDO BOLLE CLIENTE "ACCORPANTI"/"ACCORPATE"
145500070227     C*------------------------------------------------------------------------*
145600070227     C     WRIRAB        BEGSR
145700081013     C*
145800081013     C     KEYrab02_C    chain     firab02l
145900081013     C                   if        %found(firab02l)
146000081013     C                   delete    firab000
146100081013     C                   endif
146200081013     C*
146300081013     C                   clear                   FIRAB000
146400070227     C*
146500070227     C* Chiave spedizione
146600081017     C                   eval      rabFGS  = SQL_vabFGS
146700070227     C                   eval      rabAAS  = SQL_vabAAS
146800070227     C                   eval      rabLNP  = SQL_vabLNP
146900070227     C                   eval      rabNRS  = SQL_vabNRS
147000081016     C                   eval      rabNSP  = ACC_NSP
147100070227     C                   eval      rabCCM  = SQL_vabCCM
147200070227     C* Riferimenti cliente "accorpanti"
147300070227     C                   eval      rabRMN  = WRK_vabRMN
147400070227     C                   eval      rabRMA  = WRK_vabRMA
147500070227     C* Riferimenti cliente "accorpati"
147600070227     C                   eval      rabRMNR = SQL_vabRMN
147700070227     C                   eval      rabRMAR = SQL_vabRMA
147800070228     C* Attributi
147900070228     C                   eval      rabDAT  = datcor
148000070227     C*
148100070301     C                   write     FIRAB000
148200070227     C*
148300070227     C                   ENDSR
148400970526     C*------------------------------------------------------------------------*
148500970526     C* *INZSR - OPERAZIONI INIZIALI
148600970526     C*------------------------------------------------------------------------*
148700970526     C     *INZSR        BEGSR
148800040714     C*
148900040714     C*  Reperisco parametri in entrata
149000040714     C     *ENTRY        PLIST
149100070420     C                   PARM                    KPJBA
149200040714     C                   PARM                    TRTC86DS
149300070419     C*
149400080226     C                   SETOFF                                       6054
149500060726     C*
149600031113     C*
149700031110     C*---
149800031110     C* CHIAVI DI LETTURA
149900031110     C*---
150000040709     C* CHIAVE DI LETTURA X EDIVABQT1L - Completa
150100040709     C     KEYvab1       KLIST
150200040709     C                   KFLD                    SQL_vabCCM
150300040709     C                   KFLD                    SQL_vabCMR
150400040709     C                   KFLD                    SQL_vabCNT
150500040709     C                   KFLD                    SQL_vabAAS
150600040709     C                   KFLD                    SQL_vabLNP
150700040709     C                   KFLD                    SQL_vabNRS
150800040709     C                   KFLD                    SQL_vabNSP
150900070807     C* CHIAVE DI LETTURA X EDIVADQT1L/EDIVATQT1L/EDIVAXQT1L
151000040712     C     KEYva_1       KLIST
151100031112     C                   KFLD                    SQL_vabFGS
151200031112     C                   KFLD                    SQL_vabCCM
151300031112     C                   KFLD                    SQL_vabCMR
151400031112     C                   KFLD                    SQL_vabCNT
151500031112     C                   KFLD                    SQL_vabAAS
151600031112     C                   KFLD                    SQL_vabLNP
151700031112     C                   KFLD                    SQL_vabNRS
151800031112     C                   KFLD                    SQL_vabNSP
151900040227     C* CHIAVE DI LETTURA X TABEL00F - Completa
152000040227     C     KEYtab        KLIST
152100040227     C                   KFLD                    tblKUT
152200040227     C                   KFLD                    tblCOD
152300040227     C                   KFLD                    tblKEY
152400081013     C* CHIAVE DI LETTURA X FIRAB02L - Completa
152500081013     C     KEYrab02_C    KLIST
152600081017     C                   KFLD                    SQL_vabFGS
152700081017     C                   KFLD                    SQL_vabCCM
152800081013     C                   KFLD                    SQL_vabAAS
152900081013     C                   KFLD                    SQL_vabLNP
153000081013     C                   KFLD                    SQL_vabNRS
153100081016     C                   KFLD                    ACC_NSP
153200081013     C                   KFLD                    WRK_vabRMN
153300081013     C                   KFLD                    WRK_vabRMA
153400081016     C                   KFLD                    SQL_vabRMN
153500081016     C                   KFLD                    SQL_vabRMA
153600081016     C*
153700041116     C*  Richiamo XPARUT
153800041116     C                   Z-ADD     1             CODUT             1 0
153900041116     C                   CALL      'X§PARUT'
154000041116     C                   PARM                    UT§DSE
154100041116     C                   MOVEL     REC80         CNCR80
154200041116     c
154300041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
154400041116     C                   CLEAR                   TRUL06DS
154500041116     C                   MOVE      '£1'          D06COD
154600041116     C                   MOVEL     SIMFEL        D06KEY
154700041116     C                   MOVEL     'L'           D06TLA
154800041116     C                   MOVEL     TRUL06DS      KPJBU
154900041116      *
155000041116     C                   CALL      'TRUL06R'
155100041116     C                   PARM                    KPJBA
155200041116     C                   MOVEL     KPJBU         TRUL06DS
155300070228     C*
155400070228     C* CALCOLA LA DATA CORRENTE
155500070228     C                   time                    wn14             14 0
155600070228     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
155700070228     C                   z-add     wn8           g08dat
155800070228     C                   z-add     *zeros        g08inv
155900070228     C                   movel     '0'           g08err
156000070228     C                   call      'XSRDA8'
156100070228     C                   parm                    wlbda8
156200070228     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
156300991112     C*
156400991112     C                   ENDSR
