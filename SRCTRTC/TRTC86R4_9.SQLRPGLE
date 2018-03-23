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
001900070802     FFIRAB01L  O    E             DISK    COMMIT
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
004400070419     D trul34ds      e ds                  inz
004500041116     D TRUL06DS      E DS
004600041116     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
004700070228     D*------------------
004800070228     D* DS "XSRDA8" - CONTROLLA DATA (8)
004900070228     D*------------------
005000070228     D WLBDA8          DS                  INZ
005100070228     D  G08DAT                 1      8  0
005200070228     D  G08INV                 9     16  0
005300070228     D  G08ERR                17     17
005400070228     D  G08TGI                18     22  0
005500031110     D*------------
005600031110     D* DS D WRK
005700031110     D*------------
005800051021     D  DSEDIABD       DS
005900031114     D   S_vabCCM                          LIKE(SQL_vabCCM)
006000031110     D   S_vabRSD                          LIKE(SQL_vabRSD)
006100031110     D   S_vabRD2                          LIKE(SQL_vabRD2)
006200031110     D   S_vabIND                          LIKE(SQL_vabIND)
006300031110     D   S_vabCAD                          LIKE(SQL_vabCAD)
006400031110     D   S_vabLOD                          LIKE(SQL_vabLOD)
006500031110     D   S_vabPRD                          LIKE(SQL_vabPRD)
006600031110     D   S_vabNZD                          LIKE(SQL_vabNZD)
006700031110     D   S_vabLNA                          LIKE(SQL_vabLNA)
006800031110     D   S_vabZNC                          LIKE(SQL_vabZNC)
006900031110     D   S_vabCBO                          LIKE(SQL_vabCBO)
007000031110     D   S_vabTSP                          LIKE(SQL_vabTSP)
007100031110     D   S_vabCTR                          LIKE(SQL_vabCTR)
007200031110     D   S_vabGC1                          LIKE(SQL_vabGC1)
007300031110     D   S_vabGC2                          LIKE(SQL_vabGC2)
007400031110     D   S_vabFFD                          LIKE(SQL_vabFFD)
007500031110     D   S_vabDCR                          LIKE(SQL_vabDCR)
007600031110     D   S_vabTCR                          LIKE(SQL_vabTCR)
007700031110     D   S_vabHCR                          LIKE(SQL_vabHCR)
007800031110     D   S_vabGMA                          LIKE(SQL_vabGMA)
007900031110     D   S_vabGGA                          LIKE(SQL_vabGGA)
008000031110     D   S_vabGVA                          LIKE(SQL_vabGVA)
008100031110     D   S_vabTC1                          LIKE(SQL_vabTC1)
008200031110     D   S_vabTC2                          LIKE(SQL_vabTC2)
008300031110     D   S_vabVAS                          LIKE(SQL_vabVAS)
008400051021     D   S_vabVAD                          LIKE(SQL_vabVAD)
008500051021     D*
008600031110     D                 DS
008700031110     D   ANTTIP1                      2  0
008800031110     D   ANTNUM1                      2  0
008900031110     D   ANTTIP2                      2  0
009000031110     D   ANTNUM2                      2  0
009100031110     D  DSVABANT               1      8  0
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
012100070227     D ACC_NSP         S                   LIKE(VABNSP)
012200070807     D wVAXmaster      s              9S 0 inz(*zeros)
012300070905     D wVAXmaster01    s                   like(wVAXmaster) inz(*zeros)
012400070905     D wVAXmaster02    s                   like(wVAXmaster) inz(*zeros)
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
013500000629
013600000707
013700970526     C*------------------------------------------------------------------------*
013800970526     C* MAIN LINES
013900970526     C*------------------------------------------------------------------------*
014000071203     C*
014100071203     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
014200071203     C
014300071203     C/EXEC SQL
014400071203     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014500071203     C/END-EXEC
014600050708     C*
014700050708     C* Reperisce quale riferimenti mittente mantenere durante l'accorpamento
014800050708     C                   exsr      repABD
014900031110     C*
015000031110     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
015100031110     C                   exsr      exeABD
015200051216     C*
015300051216     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
015400051216     C* x gestione discorso particolarità consegna 'RC'
015500051216     C                   exsr      exeABDRC
015600031110     C*
015700031110     C* Eseguo routine d scrittura del FIVAB/FIVAD x bolle NN raggruppabili
015800031110     C                   exsr      exeNOABD
015900991112     C*
016000970526     C* FINE PGM
016100031110     C                   seton                                        LR
016200991112     C*------------------------------------------------------------------------*
016300031110     C* EXEABD - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
016400991112     C*------------------------------------------------------------------------*
016500031110     C     EXEABD        BEGSR
016600050620     C*
016700050620     C* Inizializzo indicatori d wrk
016800050620     C                   setoff                                       51
016900051216     C                   setoff                                       52
017000040709     C*
017100040712     C* Innanzitutto apro i file EDI*QT temporanei
017200040709     C                   open      EDIVABQT1L
017300040712     C                   open      EDIVADQT1L
017400040712     C                   open      EDIVATQT1L
017500070807     C                   open      EDIVAXQT1L
017600031110     C*
017700031110     C* Inizializzazioni
017800031110     C                   clear                   DSEDIVAB
017900031110     C                   clear                   DSEDIABD
018000031110     C                   clear                   SAVEDIABD
018100031110     C                   exsr      exeINZ
018200031114     C                   eval      wGiro = *zeros
018300031113     C                   seton                                        55
018400000707     C*
018500000707     C/EXEC SQL
018600051216     C+ declare C1 cursor for select * from edivabqt
018700051021     C*+ where vabCAS = 0 and vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
018800051215     C*+ where vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
018900051216     C*+ where vabATB = ' ' and vabxco=' '
019000051216     C+ where vabGMA <> 'RC' and vabGMA <> '  ' and vabATB = ' ' and vabxco=' '
019100031114     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
019200031110     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
019300031110     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
019400031110     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
019500051021     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
019600000707     C+ for fetch only
019700000707     C/END-EXEC
019800000707     C
019900000707     C/EXEC SQL
020000051216     C+ open C1
020100000707     C/END-EXEC
020200000707     C
020300000707     C/EXEC SQL
020400051216     C+ Fetch C1 into :DSEDIVAB
020500000707     C/END-EXEC
020600000707     C*
020700031110     C                   dow       sqlcod = *zeros
020800040709     C*
020900040709     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
021000051021     C                   exsr      CHKREC
021100040709     C*
021200040709     C* Se ok proseguo
021300040709     C                   if        wRECOK = 'S'
021400031110     C*
021500031110     C* Eseguo routine d valorizzazione DS d livello rottura
021600031110     C                   exsr      valDSROT
021700051216     C*
021800051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
021900051216     C                   z-add     *zeros        wFlgErr           1 0
022000051021     C*
022100051215     C* Verifico avvenuta rottura d codice
022200051021     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
022300051021     C                             (DSEDIABD=SAVEDIABD     AND
022400051021     C                              SQL_vabCAS>*zeros      AND
022500051021     C                             (SQL_vabVCA=SAV_vabVCA  AND
022600051021     C                              SQL_vabTIC=SAV_vabTIC   OR
022700051021     C                              SAV_vabCAS=*zeros)     AND
022800051021     C                              wAccCAS='S')
022900051021     C*
023000051021     C* ****************  ESEGUO ACCORPAMENTO **********************
023100051216     C*
023200051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
023300051216     C                   exsr      exeRC
023400051216     C*
023500051216     C* Se fin qui tutto ok => proseguo
023600051216     C                   if        wFlgErr = *zeros
023700051021     C*
023800051021     C* Eseguo la sommatoria dei campi da raggruppare
023900051216     C                   exsr      exeSOMMA
024000051021     C*
024100051021     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
024200051021     C                   exsr      exeNORM
024300070227     C*
024400070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
024500070227     C   60              exsr      wriRAB
024600051021     C*
024700051021     C* Effettuo scrittura del file FIVAD
024800051021     C                   exsr      wriVAD
024900051021     C*
025000051021     C* Effettuo scrittura del file FIVAT
025100051021     C                   exsr      wriVAT
025200070807     C*
025300070807     C* Effettuo scrittura del file FIVAX
025400070807     C                   exsr      wriVAX
025500051216     C*
025600051216     C                   endif
025700051021     C*
025800070228     C* ****************  NUOVA ROTTURA ************************
025900051021     C                   else
026000051021     C*
026100051021     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
026200051021     C* dell'ultima bolla accorpata
026300051021     C                   if        wGiro > 0
026400051021     C                   exsr      wriVAB
026500051021     C                   eval      wGiro = *zeros
026600051021     C                   endif
026700051021     C*
026800051021     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
026900051021     C                   eval      SAVEDIABD  = DSEDIABD
027000051021     C*
027100051021     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
027200051021     C                   exsr      exeINZ
027300051021     C*
027400051021     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
027500070227     C   60              exsr      repnum
027600051021     C                   exsr      impVAB
027700070227     C*
027800070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
027900070227     C   60              exsr      wriRAB
028000051021     C*
028100051021     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
028200051021     C                   exsr      wriVAD
028300051021     C*
028400051021     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
028500051021     C                   exsr      wriVAT
028600070807     C*
028700070807     C* Effettuo scrittura del file FIVAX x dettaglio bolla "master"
028800070807     C                   exsr      wriVAX
028900031110     C*
029000031110     C                   endif
029100031110     C*
029200031110     C* Incremento il contatore che identifica il 1° giro
029300031114     C                   eval      wGiro = wGiro + 1
029400051102     C*
029500051102     C* Salvo i nuovi attributi relativi al contrassegno
029600051102     C                   eval      SAV_vabCAS = SQL_vabCAS
029700051102     C                   eval      SAV_vabVCA = SQL_vabVCA
029800051102     C                   eval      SAV_vabTIC = SQL_vabTIC
029900051216     C*
030000051216     C* Come ultima cosa deleto il record bolla testata appena considerato
030100051216     C* ...solo se tutto ok => proseguo
030200051216     C                   if        wFlgErr = *zeros
030300040712     C     KEYvab1       delete    EDIVABQT1L
030400051216     C                   endif
030500040709     C*
030600040709     C                   endif
030700031110     C*
030800031110     C/EXEC SQL
030900051216     C+ Fetch C1 into :DSEDIVAB
031000031110     C/END-EXEC
031100031110     C                   enddo
031200031110     C*
031300000707     C/EXEC SQL
031400051216     C+ close C1
031500000707     C/END-EXEC
031600031114     C*
031700031114     C* Scarico la bolla "master" in sospeso
031800031114     C* dell'ultima bolla accorpata
031900031114     C                   exsr      wriVAB
032000040709     C*
032100040712     C* Al termine chiudo i file EDI*QT temporanei
032200040709     C                   close     EDIVABQT1L
032300040712     C                   close     EDIVADQT1L
032400040712     C                   close     EDIVATQT1L
032500070807     C                   close     EDIVAXQT1L
032600991112     C*
032700991112     C                   ENDSR
032800051216     C*------------------------------------------------------------------------*
032900051216     C* EXEABDRC - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
033000051216     C*------------------------------------------------------------------------*
033100051216     C     EXEABDRC      BEGSR
033200051216     C*
033300051216     C* Inizializzo indicatori d wrk
033400051216     C                   setoff                                       51
033500051216     C                   seton                                        52
033600051216     C*
033700051216     C* Innanzitutto apro i file EDI*QT temporanei
033800051216     C                   open      EDIVABQT1L
033900051216     C                   open      EDIVADQT1L
034000051216     C                   open      EDIVATQT1L
034100070807     C                   open      EDIVAXQT1L
034200051216     C*
034300051216     C* Inizializzazioni
034400051216     C                   clear                   DSEDIVAB
034500051216     C                   clear                   DSEDIABD
034600051216     C                   clear                   SAVEDIABD
034700051216     C                   exsr      exeINZ
034800051216     C                   eval      wGiro = *zeros
034900051216     C                   seton                                        55
035000051216     C*
035100051216     C/EXEC SQL
035200051216     C+ declare C2 cursor for select * from edivabqt
035300051216     C*+ where vabCAS = 0 and vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
035400051216     C*+ where vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
035500051216     C*+ where vabATB = ' ' and vabxco=' '
035600051216     C+ where (vabGMA = 'RC' or vabGMA = '  ') and vabATB = ' ' and vabxco=' '
035700051216     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
035800051216     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
035900051216     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
036000051216     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
036100051216     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
036200051216     C+ for fetch only
036300051216     C/END-EXEC
036400051216     C
036500051216     C/EXEC SQL
036600051216     C+ open C2
036700051216     C/END-EXEC
036800051216     C
036900051216     C/EXEC SQL
037000051216     C+ Fetch C2 into :DSEDIVAB
037100051216     C/END-EXEC
037200051216     C*
037300051216     C                   dow       sqlcod = *zeros
037400051216     C*
037500051216     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
037600051216     C                   exsr      CHKREC
037700051216     C*
037800051216     C* Se ok proseguo
037900051216     C                   if        wRECOK = 'S'
038000051216     C*
038100051216     C* Eseguo routine d valorizzazione DS d livello rottura
038200051216     C                   exsr      valDSROT
038300051216     C*
038400051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
038500051216     C                   z-add     *zeros        wFlgErr           1 0
038600051216     C*
038700051216     C* Verifico avvenuta rottura d codice
038800051216     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
038900051216     C                             (DSEDIABD=SAVEDIABD     AND
039000051216     C                              SQL_vabCAS>*zeros      AND
039100051216     C                             (SQL_vabVCA=SAV_vabVCA  AND
039200051216     C                              SQL_vabTIC=SAV_vabTIC   OR
039300051216     C                              SAV_vabCAS=*zeros)     AND
039400051216     C                              wAccCAS='S')
039500051216     C*
039600051216     C* ****************  ESEGUO ACCORPAMENTO **********************
039700051216     C*
039800051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
039900051216     C                   exsr      exeRC
040000051216     C*
040100051216     C* Se fin qui tutto ok => proseguo
040200051216     C                   if        wFlgErr = *zeros
040300051216     C*
040400051216     C* Eseguo la sommatoria dei campi da raggruppare
040500051216     C                   exsr      exeSOMMA
040600051216     C*
040700051216     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
040800051216     C                   exsr      exeNORM
040900070227     C*
041000070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
041100070227     C   60              exsr      wriRAB
041200051216     C*
041300051216     C* Effettuo scrittura del file FIVAD
041400051216     C                   exsr      wriVAD
041500051216     C*
041600051216     C* Effettuo scrittura del file FIVAT
041700051216     C                   exsr      wriVAT
041800070807     C*
041900070807     C* Effettuo scrittura del file FIVAX
042000070807     C                   exsr      wriVAX
042100051216     C*
042200051216     C                   endif
042300051216     C*
042400070228     C* ****************  NUOVA ROTTURA ************************
042500051216     C                   else
042600051216     C*
042700051216     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
042800051216     C* dell'ultima bolla accorpata
042900051216     C                   if        wGiro > 0
043000051216     C                   exsr      wriVAB
043100051216     C                   eval      wGiro = *zeros
043200051216     C                   endif
043300051216     C*
043400051216     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
043500051216     C                   eval      SAVEDIABD  = DSEDIABD
043600051216     C*
043700051216     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
043800051216     C                   exsr      exeINZ
043900051216     C*
044000051216     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
044100070227     C   60              exsr      repnum
044200070227     C                   exsr      impVAB
044300070227     C*
044400070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
044500070227     C   60              exsr      wriRAB
044600051216     C*
044700051216     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
044800051216     C                   exsr      wriVAD
044900051216     C*
045000051216     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
045100051216     C                   exsr      wriVAT
045200070807     C*
045300070807     C* Effettuo scrittura del file FIVAX x dettaglio bolla "master"
045400070807     C                   exsr      wriVAX
045500051216     C*
045600051216     C                   endif
045700051216     C*
045800051216     C* Incremento il contatore che identifica il 1° giro
045900051216     C                   eval      wGiro = wGiro + 1
046000051216     C*
046100051216     C* Salvo i nuovi attributi relativi al contrassegno
046200051216     C                   eval      SAV_vabCAS = SQL_vabCAS
046300051216     C                   eval      SAV_vabVCA = SQL_vabVCA
046400051216     C                   eval      SAV_vabTIC = SQL_vabTIC
046500051216     C*
046600051216     C* Come ultima cosa deleto il record bolla testata appena considerato
046700051216     C* ...solo se tutto ok => proseguo
046800051216     C                   if        wFlgErr = *zeros
046900051216     C     KEYvab1       delete    EDIVABQT1L
047000051216     C                   endif
047100051216     C*
047200051216     C                   endif
047300051216     C*
047400051216     C/EXEC SQL
047500051216     C+ Fetch C2 into :DSEDIVAB
047600051216     C/END-EXEC
047700051216     C                   enddo
047800051216     C*
047900051216     C/EXEC SQL
048000051216     C+ close C2
048100051216     C/END-EXEC
048200051216     C*
048300051216     C* Scarico la bolla "master" in sospeso
048400051216     C* dell'ultima bolla accorpata
048500051216     C                   exsr      wriVAB
048600051216     C*
048700051216     C* Al termine chiudo i file EDI*QT temporanei
048800051216     C                   close     EDIVABQT1L
048900051216     C                   close     EDIVADQT1L
049000051216     C                   close     EDIVATQT1L
049100070807     C                   close     EDIVAXQT1L
049200051216     C*
049300051216     C                   ENDSR
049400031110     C*------------------------------------------------------------------------*
049500040712     C* EXENOABD - SCRITTIURA FILE FIVAB/FIVAD X BOLLE NN RAGGRUPPABILI
049600031110     C*------------------------------------------------------------------------*
049700031110     C     EXENOABD      BEGSR
049800050620     C*
049900050620     C* Inizializzo indicatori d wrk
050000050620     C                   setoff                                       51
050100040712     C*
050200040712     C* Innanzitutto apro i file EDI*QT temporanei
050300040712     C                   open      EDIVABQT1L
050400040712     C                   open      EDIVADQT1L
050500040712     C                   open      EDIVATQT1L
050600070807     C                   open      EDIVAXQT1L
050700040712     C*
050800040712     C* Porto tutto ciò che è rimasto nei file EDI*QT nei relativi FIVA*
050900040712     C*
051000040712     C* ...VAB
051100040712     C     *loval        setll     EDIVABQT1L
051200040712     C                   read      EDIVABQT1L
051300040712     C                   dow       not %eof(EDIVABQT1L)
051400040712     C                   write     FIVAB000
051500040712     C                   delete    EDIVABQT1L
051600040712     C                   read      EDIVABQT1L
051700040712     C                   enddo
051800040712     C*
051900040712     C* ...VAD
052000040712     C     *loval        setll     EDIVADQT1L
052100040712     C                   read      EDIVADQT1L
052200040712     C                   dow       not %eof(EDIVADQT1L)
052300040712     C                   write     FIVAD000
052400040712     C                   delete    EDIVADQT1L
052500040712     C                   read      EDIVADQT1L
052600040712     C                   enddo
052700040712     C*
052800040712     C* ...VAT
052900040712     C     *loval        setll     EDIVATQT1L
053000040712     C                   read      EDIVATQT1L
053100040712     C                   dow       not %eof(EDIVATQT1L)
053200040712     C                   write     FIVAT000
053300040712     C                   delete    EDIVATQT1L
053400040712     C                   read      EDIVATQT1L
053500040712     C                   enddo
053600070807     C*
053700070807     C* ...VAX
053800070807     C     *loval        setll     EDIVAXQT1L
053900070807     C                   read      EDIVAXQT1L
054000070807     C                   dow       not %eof(EDIVAXQT1L)
054100070807     C                   write     FIVAX000
054200070807     C                   delete    EDIVAXQT1L
054300070807     C                   read      EDIVAXQT1L
054400070807     C                   enddo
054500040714     C*
054600070807     C** ...VAX
054700070807     C*     KEYvax2_C     setll     EDIVAX2L
054800070807     C*     KEYvax2_C     reade     EDIVAX2L
054900070807     C*                   dow       not %eof(EDIVAX2L)
055000070807     c*     vaxfgs        lookup    lin                                    50
055100070807     c*                   if        *in50  and vaxfgs>0
055200070807     C*                   write     FIVAX000
055300070807     C*                   delete    EDIVAX2L
055400070807     c*                   endif
055500070807     c*
055600070807     C*     KEYvax2_C     reade     EDIVAX2L
055700070807     C*                   enddo
055800040712     C*
055900040714     C* Al termine chiudo i file EDI* temporanei
056000040712     C                   close     EDIVABQT1L
056100040712     C                   close     EDIVADQT1L
056200040712     C                   close     EDIVATQT1L
056300070807     C                   close     EDIVAXQT1L
056400031110     C*
056500031110     C                   ENDSR
056600031110     C*------------------------------------------------------------------------*
056700031110     C* EXEINZ - ROUTINE D INIZIALIZZAZIONE X OGNI NUOVO LIVELLO D ROTTURA BOLLE
056800031110     C*------------------------------------------------------------------------*
056900031110     C     EXEINZ        BEGSR
057000031110     C*
057100031110     C                   clear                   DSEDIVABW
057200031114     C                   clear                   wRMO
057300050620     C                   clear                   wNOT
057400050620     C                   clear                   wNT2
057500080929     C                   clear                   wNOTNT2
057600051215     C                   eval      wFlgOrmRC = *off
057700031110     C*
057800031110     C                   ENDSR
057900031110     C*------------------------------------------------------------------------*
058000031110     C* VALDSROT - VALORIZZAZIONE DS LIVELLO ROTTURA
058100031110     C*------------------------------------------------------------------------*
058200031112     C     VALDSROT      BEGSR
058300031110     C*
058400031110     C* Campi in rottura
058500031114     C                   EVAL      S_vabCCM = SQL_vabCCM
058600031110     C                   EVAL      S_vabRSD = SQL_vabRSD
058700031110     C                   EVAL      S_vabRD2 = SQL_vabRD2
058800031110     C                   EVAL      S_vabIND = SQL_vabIND
058900031110     C                   EVAL      S_vabCAD = SQL_vabCAD
059000031110     C                   EVAL      S_vabLOD = SQL_vabLOD
059100031110     C                   EVAL      S_vabPRD = SQL_vabPRD
059200031110     C                   EVAL      S_vabNZD = SQL_vabNZD
059300031110     C                   EVAL      S_vabLNA = SQL_vabLNA
059400031110     C                   EVAL      S_vabZNC = SQL_vabZNC
059500031110     C                   EVAL      S_vabCBO = SQL_vabCBO
059600031110     C                   EVAL      S_vabTSP = SQL_vabTSP
059700031110     C                   EVAL      S_vabCTR = SQL_vabCTR
059800031110     C                   EVAL      S_vabGC1 = SQL_vabGC1
059900031110     C                   EVAL      S_vabGC2 = SQL_vabGC2
060000031110     C                   EVAL      S_vabFFD = SQL_vabFFD
060100031110     C                   EVAL      S_vabDCR = SQL_vabDCR
060200031110     C                   EVAL      S_vabTCR = SQL_vabTCR
060300031110     C                   EVAL      S_vabHCR = SQL_vabHCR
060400051216     C  N52              EVAL      S_vabGMA = SQL_vabGMA
060500031110     C                   EVAL      S_vabGGA = SQL_vabGGA
060600031110     C                   EVAL      S_vabGVA = SQL_vabGVA
060700031110     C                   EVAL      S_vabTC1 = SQL_vabTC1
060800031110     C                   EVAL      S_vabTC2 = SQL_vabTC2
060900031110     C                   EVAL      S_vabVAS = SQL_vabVAS
061000031110     C                   EVAL      S_vabVAD = SQL_vabVAD
061100031110     C*
061200031110     C                   ENDSR
061300031110     C*------------------------------------------------------------------------*
061400031110     C* IMPVAB - VALORIZZAZIONE CAMPI "FISSI" 1° BOLLA D OGNI LIVELLO D ROTTURA
061500031110     C*------------------------------------------------------------------------*
061600031110     C     IMPVAB        BEGSR
061700031110     C*
061800031110     C* Valorizzo la DS d wrk da quella d lettura SQL
061900031110     C                   EVAL      DSEDIVABW = DSEDIVAB
062000031110     C*
062100031110     C                   ENDSR
062200051216     C*------------------------------------------------------------------------*
062300051216     C* EXERC - GESTIONE EVENTUALE PRESENZA PARTICOLARITA' CONSEGNA 'RC'
062400051216     C*------------------------------------------------------------------------*
062500051216     C     EXERC         BEGSR
062600051216     C*
062700051216     C* Gestisco anche l'accorpamento anche dell'ORM da "RC"
062800051216     C* ...ma solo se trattasi d primo "RC" delle bolle oggetto d accorpamento
062900060126     C**!!!              IF        SQL_vabGMA = 'RC' and
063000060126     C**!!!                        wFlgOrmRC = *off
063100070227 MB 1C                   IF        SQL_vabGMA = 'RC'
063200070227 MB 2c                   If        wflgormrc = *On
063300060127     C*
063400060127     C* Elimino l'ORM RC della bolla corrente (accorpata)
063500060126     c                   Clear                   Fnls25ds
063600060126     c                   Eval      i25ksc  = sql_vabccm
063700060126     c                   Move(p)   sql_vabaas    i25aas
063800060126     c                   Eval      i25spe = %editc(sql_vablnp:'X') +
063900060126     c                                      %editc(sql_vabnrs:'X') +
064000060126     c                                      %editc(sql_vabnsp:'X')
064100060126     c                   Call      'FNLS25R'
064200060126     c                   Parm                    Fnls25ds
064300060126    2C                   ENDIF
064400060126    2C                   If        wFlgOrmRC = *off
064500060127     C*
064600060127     C* Aggiorno l'orm RC legato alla bolla risultante dall'accorpamento solo se
064700060127     C* bolla corrente (accorpata) e bolla accorpante hanno chiave diversa.
064800060127     C                   IF        SQL_vabAAS <> WRK_vabAAS OR
064900060127     C                             SQL_vabLNP <> WRK_vabLNP OR
065000060127     C                             SQL_vabNRS <> WRK_vabNRS OR
065100060127     C                             SQL_vabNSP <> WRK_vabNSP
065200060127     C*
065300060127     C* Prima dell'aggiornamento del riferimento ORM RC con la nuova chiave spedizione
065400060127     C* elimino l'ORM RC della bolla accorpante
065500060127     c                   Clear                   Fnls25ds
065600060127     c                   Eval      i25ksc  = sql_vabccm
065700060127     c                   Move(p)   sql_vabaas    i25aas
065800060127     c                   Eval      i25spe = %editc(WRK_vabLNP:'X') +
065900060127     c                                      %editc(WRK_vabNRS:'X') +
066000060127     c                                      %editc(WRK_vabNSP:'X')
066100060127     c                   Call      'FNLS25R'
066200060127     c                   Parm                    Fnls25ds
066300060127     C*
066400060127     C* Quindi aggiorno l'ORM RC della bolla corrente (accorpata) con la nuova chiave
066500060127     C* spedizione (bolla accorpante)
066600051216     C                   CLEAR                   wRC_KEY
066700051216     C                   EVAL      wRC_AAS = SQL_vabAAS
066800051216     C                   EVAL      wRC_LNP = SQL_vabLNP
066900051216     C                   EVAL      wRC_NRS = SQL_vabNRS
067000051216     C                   EVAL      wRC_NSP = SQL_vabNSP
067100051220     C                   EVAL      I24TLA  = 'L'
067200051216     C                   EVAL      I24OPE  = 'U'
067300051216     C                   EVAL      I24KSC  = SQL_vabCCM
067400051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
067500051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
067600051216     C                   EVAL      I24NAAS = WRK_vabAAS
067700051216     C                   EVAL      I24LNP  = WRK_vabLNP
067800051216     C                   EVAL      I24NRS  = WRK_vabNRS
067900051216     C                   EVAL      I24NSP  = WRK_vabNSP
068000051216     C                   CALL      'FNLS24R'
068100051216     C                   PARM                    FNLS24DS
068200060127     C*
068300051216     C* Se tutto ok => accendo l'indicatore x nn aggiornare ORM generati da bolle "RC"
068400051216     C* accorpate successivamente (ovviamente x medesimo destinatario)
068500060126    3C                   IF        O24ERR = *zeros
068600051216     C                   EVAL      wFlgOrmRC = *on
068700060126   x3C                   ELSE
068800051216     C                   Z-ADD     1             wFlgErr
068900060126    3C                   ENDIF
069000060127     C                   ENDIF
069100060126    2C                   ENDIF
069200060126    1C                   ENDIF
069300051216     C*
069400051216     C                   ENDSR
069500031110     C*------------------------------------------------------------------------*
069600031110     C* EXESOMMA - SOMMATORIA CAMPI X RAGGRUPPAMENTO
069700031110     C*------------------------------------------------------------------------*
069800031110     C     EXESOMMA      BEGSR
069900031110     C*
070000051216     C* Eseguo sommatoria dati bolle
070100031110     C                   EVAL      WRK_vabIAS = WRK_vabIAS + SQL_vabIAS
070200031110     C                   EVAL      WRK_vabNCL = WRK_vabNCL + SQL_vabNCL
070300031110     C                   EVAL      WRK_vabPKB = WRK_vabPKB + SQL_vabPKB
070400031110     C                   EVAL      WRK_vabVLB = WRK_vabVLB + SQL_vabVLB
070500031110     C                   EVAL      WRK_vabQFT = WRK_vabQFT + SQL_vabQFT
070600031110     C                   EVAL      WRK_vabVMD = WRK_vabVMD + SQL_vabVMD
070700051021     C                   EVAL      WRK_vabCAS = WRK_vabCAS + SQL_vabCAS
070800031110     C*
070900031110     C                   ENDSR
071000031110     C*------------------------------------------------------------------------*
071100031110     C* EXENORM - NORMALIZZAZIONE CAMPI X OTTENIMENTO BOLLA "MASTER"
071200031110     C*------------------------------------------------------------------------*
071300031110     C     EXENORM       BEGSR
071400050620     C*
071500050620     C* Valorizzo indicatore d wrk
071600050620     C                   SETON                                        51
071700031110     C*
071800031114     C                   IF        WRK_vabNAS =  *blanks AND
071900031114     C                             SQL_vabNAS <> *blanks
072000031110     C                   EVAL      WRK_vabNAS = SQL_vabNAS
072100031110     C                   ENDIF
072200031110     C*
072300031114     C                   IF        WRK_vabCTM =  *blanks AND
072400031114     C                             SQL_vabCTM <> *blanks
072500031110     C                   EVAL      WRK_vabCTM = SQL_vabCTM
072600031110     C                   ENDIF
072700031110     C*
072800031114     C                   IF        WRK_vabCTS =  *blanks AND
072900031114     C                             SQL_vabCTS <> *blanks
073000031110     C                   EVAL      WRK_vabCTS = SQL_vabCTS
073100031110     C                   ENDIF
073200031110     C*
073300031114     C                   IF        WRK_vabFTM =  *blanks AND
073400031114     C                             SQL_vabFTM <> *blanks
073500031110     C                   EVAL      WRK_vabFTM = SQL_vabFTM
073600031110     C                   ENDIF
073700031110     C*
073800031114     C                   IF        WRK_vabSCL =  *blanks AND
073900031114     C                             SQL_vabSCL <> *blanks
074000031110     C                   EVAL      WRK_vabSCL = SQL_vabSCL
074100031110     C                   ENDIF
074200051021     C*
074300051021     C                   IF        WRK_vabVCA =  *blanks AND
074400051021     C                             SQL_vabVCA <> *blanks
074500051021     C                   EVAL      WRK_vabVCA = SQL_vabVCA
074600051021     C                   ENDIF
074700051021     C*
074800051021     C                   IF        WRK_vabTIC =  *blanks AND
074900051021     C                             SQL_vabTIC <> *blanks
075000051021     C                   EVAL      WRK_vabTIC = SQL_vabTIC
075100051021     C                   ENDIF
075200051216     C*
075300051216     C                   IF        WRK_vabGMA =  *blanks AND
075400051216     C                             SQL_vabGMA <> *blanks
075500051216     C                   EVAL      WRK_vabGMA = SQL_vabGMA
075600051216     C                   ENDIF
075700031110     C*
075800031110     C* Gestisco comportamento particolare del campo VABANT (tipo e numero bancali)
075900031110     C                   IF        WRK_vabANT = *zeros
076000031110     C                   EVAL      WRK_vabANT = SQL_vabANT
076100031110     C                   ELSE
076200031110     C                   IF        SQL_vabANT = *zeros
076300031110     C                   ELSE
076400031110     C                   EVAL      DSVABANT = SQL_vabANT
076500031110     C                   EVAL      wANTTIP1 = ANTTIP1
076600031110     C                   EVAL      wANTNUM1 = ANTNUM1
076700031110     C                   EVAL      wANTTIP2 = ANTTIP2
076800031110     C                   EVAL      wANTNUM2 = ANTNUM2
076900031110     C                   EVAL      DSVABANT = WRK_vabANT
077000031110     C                   IF        ANTTIP1 > *zeros
077100031110     C                   IF        wANTTIP1 = ANTTIP1
077200031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM1
077300031110     C                   ENDIF
077400031110     C                   IF        wANTTIP2 = ANTTIP1
077500031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM2
077600031110     C                   ENDIF
077700031110     C                   ELSE
077800031110     C                   IF        wANTTIP1 > *zeros
077900031110     C                   EVAL      ANTTIP1 = wANTTIP1
078000031110     C                   EVAL      ANTNUM1 = wANTNUM1
078100031110     C                   ELSE
078200031110     C                   IF        wANTTIP2 > *zeros
078300031110     C                   EVAL      ANTTIP1 = wANTTIP2
078400031110     C                   EVAL      ANTNUM1 = wANTNUM2
078500031110     C                   ENDIF
078600031110     C                   ENDIF
078700031110     C                   ENDIF
078800031110     C                   IF        ANTTIP2 > *zeros
078900031110     C                   IF        wANTTIP1 = ANTTIP2
079000031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM1
079100031110     C                   ENDIF
079200031110     C                   IF        wANTTIP2 = ANTTIP2
079300031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM2
079400031110     C                   ENDIF
079500031110     C                   ELSE
079600031110     C                   IF        wANTTIP1 > *zeros
079700031110     C                   EVAL      ANTTIP2 = wANTTIP1
079800031110     C                   EVAL      ANTNUM2 = wANTNUM1
079900031110     C                   ELSE
080000031110     C                   IF        wANTTIP2 > *zeros
080100031110     C                   EVAL      ANTTIP2 = wANTTIP2
080200031110     C                   EVAL      ANTNUM2 = wANTNUM2
080300031110     C                   ENDIF
080400031110     C                   ENDIF
080500031110     C                   ENDIF
080600031110     C                   EVAL      WRK_vabANT = DSVABANT
080700031110     C                   ENDIF
080800031110     C                   ENDIF
080900031114     C*
081000051021     C* Eseguo raggruppamento dei riferimenti delle testate bolle accorpate
081100051021     C                   IF        wLenRIF > *zeros
081200050906     C                   IF        wTIPABD = 'S'
081300050614     C                   EVAL      wRIF = %trim(%editc(SQL_vabRMN:'4'))
081400050610     C                   ENDIF
081500050610     C                   IF        wTIPABD = 'A'
081600050610     C                   EVAL      wRIF = %trim(SQL_vabRMA)
081700050610     C                   ENDIF
081800050610     C*
081900050610     C* Gestisco l'allineamento d default a seconda della tipologia d riferimento da utilizzare
082000050610     C                   IF        wAlmRIF = *blanks
082100050610     C                   IF        wTipABD = 'S'
082200050610     C                   EVAL      wAlmRIF = 'D'
082300050610     C                   ENDIF
082400050610     C                   IF        wTipABD = 'A'
082500050610     C                   EVAL      wAlmRIF = 'S'
082600050610     C                   ENDIF
082700050610     C                   ENDIF
082800051021     C*
082900051021     C* Effettuo le operazioni seguenti SOLO in caso in cui il riferimento voluto
083000051021     C* risulti essere effettivamente valorizzato
083100051021     C                   IF        wRIF <> *blanks
083200050614     C*
083300050614     C* Considero la minore tra la lunghezza reale del riferimento da considerare e il
083400050614     C* numero d byte da considerare indicati in tabella x il cliente corrente
083500050614     C                   Z-ADD     *zeros        wByteRIF          2 0
083600050614     C                   IF        wLenRIF <= %len(%trim(wRIF))
083700050614     C                   EVAL      wByteRIF = wLenRIF
083800050614     C                   ELSE
083900050614     C                   EVAL      wByteRIF = %len(%trim(wRIF))
084000050614     C                   ENDIF
084100050610     C*
084200050610     C* A questo punto verifico se sono richiesti solo 1 numero specifico d byte ed il
084300050610     C* relativo allineamento
084400050610     C                   IF        wLenRIF <> 99
084500050610     C                   IF        wAlmRIF = 'S'
084600050614     C                   EVAL      wRIF = %subst(%trim(wRIF):1:wByteRIF)
084700050610     C                   ENDIF
084800050610     C                   IF        wAlmRIF = 'D'
084900050610     C                   EVAL      wRIF = %subst(%trim(wRIF):
085000050614     C                                    %len(%trim(wRIF))-wByteRIF+1)
085100050610     C                   ENDIF
085200050610     C                   ENDIF
085300051021     C*
085400051021     C                   ELSE
085500051021     C                   MOVEL     'N'           wOltreRIF
085600051021     C                   ENDIF
085700050610     C*
085800031114     C* Se c stanno...
085900050610     C                   IF        %len(%trim(wRMO))+1+%len(%trim(wRIF)) <=
086000040517     C                             %len(wRMO)-4
086100050610     C                   EVAL      wRMO = %trim(wRMO) + %trim(wRIF) + '-'
086200050610     C                   MOVEL     'N'           wOltreRIF         1
086300050610     C                   ELSE
086400050610     C                   MOVEL     'S'           wOltreRIF
086500031114     C                   ENDIF
086600050906     C*
086700050906     C                   ELSE
086800050906     C                   MOVEL     'N'           wOltreRIF
086900050906     C                   ENDIF
087000051021     C*
087100050620     C*
087200050620     C* Se richiesto raggruppo anche le note
087300080929     C                   IF        wRgrNOT <> *blanks
087400060726     C* Memorizzo subito note originali
087500080929     C                   IF        wRgrNOT = 'S'
087600060726     C                   IF        wNOT = *blanks AND WRK_vabNOT <> *blanks AND
087700060726     C                             %scan(%trim(WRK_vabNOT):wNT2) = 0
087800060726     C                   EVAL      wNOT = %trim(WRK_vabNOT)
087900060726     C                   ENDIF
088000060726     C                   IF        wNT2 = *blanks AND WRK_vabNT2 <> *blanks AND
088100060726     C                             %scan(%trim(WRK_vabNT2):wNOT) = 0
088200060726     C                   EVAL      wNT2 = %trim(WRK_vabNT2)
088300060726     C                   ENDIF
088400080929     C                   ENDIF
088500080929     C                   IF        wRgrNOT = 'U'
088600080929     C                   IF        wNOTNT2=*blanks AND
088700080929     C                             (WRK_vabNOT<>*blanks OR WRK_vabNT2<>*blanks)
088800080929     C                             AND
088900080929     C                             %scan(%trim(WRK_vabNOT)+%trim(WRK_vabNT2):
089000080929     C                             wNOTNT2)=0
089100080929     C                   EVAL      wNOTNT2=%trim(WRK_vabNOT)+ ' '+
089200080929     C                                     %trim(WRK_vabNT2)
089300080929     C                   ENDIF
089400080929     C                   ENDIF
089500080929     C*
089600080929     C                   IF        wRgrNOT = 'S'
089700060629     C***  VABNOT
089800060726     C* Se presente nota => gestisco
089900060726     C                   IF        SQL_vabNOT <> *blanks
090000080929     C* Innanzitutto verifico che la nota corrente nn sia già presente
090100060726     C                   SETOFF                                       40
090200060726     C                   IF        wNOT <> *blanks
090300060726     C                   IF        %scan(%trim(SQL_vabNOT):wNOT) <> 0 OR
090400060726     C                             %scan(%trim(SQL_vabNOT):wNT2) <> 0
090500060726     C                   SETON                                        40
090600060726     C                   ENDIF
090700060726     C                   ENDIF
090800060629     C* Verifico se lo spazio su NOT è già esaurito mentre quello su NT2 è ancora dispnibile
090900060726     C                   IF        *IN40 = *OFF
091000060629     C                   IF        (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNOT))
091100060629     C                             >%size(wNOT)) AND
091200060629     C                             (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNOT))
091300060629     C                             <=%size(wNT2))
091400060726     C                   IF        %scan(%trim(SQL_vabNOT):wNT2) = 0 AND
091500060726     C                             %scan(%trim(SQL_vabNOT):wNOT) = 0
091600060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNOT)
091700060726     C                   ENDIF
091800060629     C                   ELSE
091900050708     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNOT)
092000060726     C                   ENDIF
092100060726     C                   ENDIF
092200060629     C***  VABNT2
092300060726     C* Se presente nota => gestisco
092400060726     C                   IF        SQL_vabNT2 <> *blanks
092500060629     C* Innanzitutto verifico che il riferimento corrente nn sia già presente
092600060726     C                   SETOFF                                       40
092700060726     C                   IF        wNT2 <> *blanks
092800060726     C                   IF        %scan(%trim(SQL_vabNT2):wNT2) <> 0 OR
092900060726     C                             %scan(%trim(SQL_vabNT2):wNOT) <> 0
093000060726     C                   SETON                                        40
093100060726     C                   ENDIF
093200060726     C                   ENDIF
093300060629     C* Verifico se lo spazio su NT2 è già esaurito mentre quello su NOT è ancora dispnibile
093400060726     C                   IF        *IN40 = *OFF
093500060629     C                   IF        (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNT2))
093600060629     C                             >%size(wNT2)) AND
093700060629     C                             (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNT2))
093800060629     C                             <=%size(wNOT))
093900060726     C                   IF        %scan(%trim(SQL_vabNT2):wNOT) = 0 AND
094000060726     C                             %scan(%trim(SQL_vabNT2):wNT2) = 0
094100060629     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNT2)
094200060726     C                   ENDIF
094300060629     C                   ELSE
094400060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNT2)
094500060629     C                   ENDIF
094600060629     C                   ENDIF
094700060726     C                   ENDIF
094800080929     C                   ENDIF
094900080929     C                   ENDIF
095000080929     C*
095100080929     C                   IF        wRgrNOT = 'U'
095200080929     C
095300080929     C                   IF        (%len(%trim(wNOTNT2))+1+
095400080929     C                              %len(%trim(SQL_vabNOT))+
095500080929     C                              %len(%trim(SQL_vabNT2)))
095600080929     C                             <=%size(wNOTNT2)
095700080929     C                   EVAL      wNOTNT2 = %trim(wNOTNT2)    + ' ' +
095800080929     C                                       %trim(SQL_vabNOT) + ' ' +
095900080929     C                                       %trim(SQL_vabNT2)
096000080929     C                   ENDIF
096100080929     C                   ENDIF
096200060629     C*
096300050620     C                   ELSE
096400050708     C                   EVAL      wNOT = %trim(SQL_vabNOT)
096500050708     C                   EVAL      wNT2 = %trim(SQL_vabNT2)
096600050620     C                   ENDIF
096700031110     C*
096800031110     C                   ENDSR
096900031112     C*------------------------------------------------------------------------*
097000031112     C* WRIVAD - SCRITTIURA FILE FIVAD
097100031112     C*------------------------------------------------------------------------*
097200031112     C     WRIVAD        BEGSR
097300031112     C*
097400031112     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAD)
097500031112     C                   CLEAR                   FIVAD000
097600031112     C*
097700051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAD
097800040712     C     KEYva_1       SETLL     EDIVADQT1L
097900031113     C                   IF        %equal(EDIVADQT1L)
098000040227     C* Eseguo routine x modifica codice trattamento merce
098100040227     C                   EXSR      CHGCTM
098200040227     C*
098300040712     C     KEYva_1       READE     EDIVADQT1L
098400031113     C                   DOW       not %eof(EDIVADQT1L)
098500031112     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
098600031112     C                   EVAL      vadAAS = WRK_vabAAS
098700031112     C                   EVAL      vadLNP = WRK_vabLNP
098800031112     C                   EVAL      vadNRS = WRK_vabNRS
098900031112     C                   EVAL      vadNSP = WRK_vabNSP
099000070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
099100070227     C   60              EVAL      vadNSP = ACC_NSP
099200031112     C* Inserisco record d dettaglio in FIVAD
099300031112     C                   WRITE     FIVAD000
099400031112     C* Elimino record d dettaglio in EDIVAD
099500031113     C                   DELETE    EDIVADQT1L
099600031112     C* Continuo la lettura dei dettagli della testata bolla corrente
099700040712     C     KEYva_1       READE     EDIVADQT1L
099800031112     C                   ENDDO
099900031112     C*
100000031112     C* Se invece nn esiste già il dettaglio colli in EDIVAD e sulla testata c'è l'indicazioine
100100031112     C* dei segnacolli => provvedo a creare il VAD
100200031112     C                   ELSE
100300031112     C                   IF        SQL_vabNCD > *zeros OR
100400031112     C                             SQL_vabNCA > *zeros
100500040227     C* Eseguo routine x modifica codice trattamento merce
100600040227     C                   EXSR      CHGCTM
100700031112     C* Valorizzo la chiave bolla del dettaglio con quella della relativa testata bolla "master"
100800031112     C                   EVAL      vadAAS = WRK_vabAAS
100900031112     C                   EVAL      vadLNP = WRK_vabLNP
101000031112     C                   EVAL      vadNRS = WRK_vabNRS
101100031112     C                   EVAL      vadNSP = WRK_vabNSP
101200031112     C* Valorizzo i restanti campi con quelli della testata bolla corrente
101300031112     C                   EVAL      vadFGS = SQL_vabFGS
101400031112     C                   EVAL      vadCCM = SQL_vabCCM
101500031112     C                   EVAL      vadNCL = SQL_vabNCL
101600031112     C                   EVAL      vadNCD = SQL_vabNCD
101700031112     C                   EVAL      vadNCA = SQL_vabNCA
101800070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
101900070227     C   60              EVAL      vadNSP = ACC_NSP
102000031112     C* Inserisco record d dettaglio in FIVAD
102100031112     C                   WRITE     FIVAD000
102200031112     C                   ENDIF
102300031112     C                   ENDIF
102400031112     C*
102500031112     C                   ENDSR
102600031114     C*------------------------------------------------------------------------*
102700031114     C* WRIVAT - SCRITTIURA FILE FIVAT
102800031114     C*------------------------------------------------------------------------*
102900031114     C     WRIVAT        BEGSR
103000031114     C*
103100031114     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAT)
103200031114     C                   CLEAR                   FIVAT000
103300031114     C*
103400051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAT
103500040712     C     KEYva_1       SETLL     EDIVATQT1L
103600031114     C                   IF        %equal(EDIVATQT1L)
103700040227     C* Eseguo routine x modifica codice trattamento merce
103800040227     C                   EXSR      CHGCTM
103900040227     C*
104000040712     C     KEYva_1       READE     EDIVATQT1L
104100031114     C                   DOW       not %eof(EDIVATQT1L)
104200031114     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
104300031114     C                   EVAL      vatAAS = WRK_vabAAS
104400031114     C                   EVAL      vatLNP = WRK_vabLNP
104500031114     C                   EVAL      vatNRS = WRK_vabNRS
104600031114     C                   EVAL      vatNSP = WRK_vabNSP
104700070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
104800070227     C   60              EVAL      vatNSP = ACC_NSP
104900031114     C* Inserisco record d dettaglio in FIVAT
105000031114     C                   WRITE     FIVAT000
105100031114     C* Elimino record d dettaglio in EDIVAT
105200031114     C                   DELETE    EDIVATQT1L
105300031114     C* Continuo la lettura dei dettagli della testata bolla corrente
105400040712     C     KEYva_1       READE     EDIVATQT1L
105500031114     C                   ENDDO
105600031114     C                   ENDIF
105700031114     C*
105800031114     C                   ENDSR
105900070807     C*------------------------------------------------------------------------*
106000070807     C* WRIVAX - SCRITTIURA FILE FIVAX
106100070807     C*------------------------------------------------------------------------*
106200070807     C     WRIVAX        BEGSR
106300070809     C*
106400070809     C* Solo x record VAX diversi dalla bolla master (accorpante)
106500070809     C                   IF        SQL_vabFGS  = WRK_vabFGS AND
106600070809     C                             SQL_vabCCM  = WRK_vabCCM AND
106700070809     C                             SQL_vabAAS  = WRK_vabAAS AND
106800070809     C                             SQL_vabLNP  = WRK_vabLNP AND
106900070809     C                             SQL_vabNRS  = WRK_vabNRS AND
107000070809     C                             SQL_vabNSP <> WRK_vabNSP
107100070807     C*
107200070807     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAX)
107300070807     C                   CLEAR                   FIVAX000
107400070807     C*
107500070807     C                   Z-ADD     *zeros        wVAXmaster
107600071203     C                   Z-ADD     *zeros        wVAXmaster01
107700071203     C                   Z-ADD     *zeros        wVAXmaster02
107800070807     C*
107900070807     C* Effettuo conteggio righe attuali packing list della bolla master
108000070807     C*
108100070807     C/EXEC SQL
108200070905     C+ declare C1VX cursor for
108300070906     C+ select max(vaxprr) from edivaxqt
108400070911     C+ where
108500070911     C+ vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
108600070807     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
108700070807     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
108800070807     C+ for fetch only
108900070807     C/END-EXEC
109000070807     C
109100070807     C/EXEC SQL
109200070905     C+ open C1VX
109300070807     C/END-EXEC
109400070807     C
109500070807     C/EXEC SQL
109600070905     C+ Fetch C1VX into :wVAXmaster01
109700070807     C/END-EXEC
109800070905     C*
109900070905     C/EXEC SQL
110000070905     C+ declare C2VX cursor for
110100070905     C+ select max(vaxprr) from fivax00f
110200070911     C+ where
110300070911     C+ vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
110400070905     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
110500070905     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
110600070905     C+ for fetch only
110700070905     C/END-EXEC
110800070905     C
110900070905     C/EXEC SQL
111000070905     C+ open C2VX
111100070905     C/END-EXEC
111200070905     C
111300070905     C/EXEC SQL
111400070905     C+ Fetch C2VX into :wVAXmaster02
111500070905     C/END-EXEC
111600070809     C*
111700070905     C* Tengo il maggiore progressivo riga tra EDIVAX e FIVAX
111800070905     C                   EVAL      wVAXmaster = wVAXmaster01
111900070905     C                   IF        wVAXmaster02 > wVAXmaster01
112000070905     C                   EVAL      wVAXmaster = wVAXmaster02
112100070905     C                   ENDIF
112200070905     C*
112300070809     C* Proseguo elaborazione solo se effettivamente trovati VAX da accorpare
112400070905     C                   IF        wVAXmaster > *zeros
112500070807     C*
112600070807     C* Verifico se x la bolla corrente di EDIVAB esiste il relativo packing list in EDIVAX
112700070807     C     KEYva_1       SETLL     EDIVAXQT1L
112800070807     C                   IF        %equal(EDIVAXQT1L)
112900070809     C*
113000070809     C                   SETOFF                                       88
113100070807     C*
113200070807     C     KEYva_1       READE     EDIVAXQT1L
113300070807     C                   DOW       not %eof(EDIVAXQT1L)
113400070809     C*
113500070809     C* Se nn ancora gestito il salto pagina fittizio forzato
113600070809     C                   IF        *IN88 = *off
113700070809     C                   SETON                                        88
113800070905     C***                EXSR      GESFORMFEED
113900070809     C                   ENDIF
114000070809     C*
114100070807     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
114200070807     C                   EVAL      vaxAAS = WRK_vabAAS
114300070807     C                   EVAL      vaxLNP = WRK_vabLNP
114400070807     C                   EVAL      vaxNRS = WRK_vabNRS
114500070807     C                   EVAL      vaxNSP = WRK_vabNSP
114600070807     C* Auto-incremento numero progressivo riga packing list bolla master + bolla slave corrente
114700070905     C***                EVAL      vaxPRR = vaxPRR + wVAXmaster + wInsRighe
114800070905     C                   EVAL      vaxPRR = vaxPRR + wVAXmaster
114900070807     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
115000070807     C   60              EVAL      vaxNSP = ACC_NSP
115100070807     C* Inserisco record d dettaglio in FIVAX
115200070807     C                   WRITE     FIVAX000
115300070807     C* Elimino record d dettaglio in EDIVAX
115400070807     C                   DELETE    EDIVAXQT1L
115500070807     C* Continuo la lettura dei dettagli della testata bolla corrente
115600070807     C     KEYva_1       READE     EDIVAXQT1L
115700070807     C                   ENDDO
115800070807     C                   ENDIF
115900070807     C*
116000070809     C                   ENDIF
116100070809     C*
116200070807     C/EXEC SQL
116300070905     C+ close C1VX
116400070807     C/END-EXEC
116500070905     C*
116600070905     C/EXEC SQL
116700070905     C+ close C2VX
116800070905     C/END-EXEC
116900070905     C*
117000070809     C                   ENDIF
117100070807     C*
117200070807     C                   ENDSR
117300070809     C*------------------------------------------------------------------------*
117400070809     C* GESFORMFEED - GESTIONE FORZATURA SALTO PAGINA FITTIZIO
117500070809     C*------------------------------------------------------------------------*
117600070809     C     GESFORMFEED   BEGSR
117700070809     C*
117800070809     C* Salvo il formato record corrente
117900070809     C                   EVAL      DSEDIVAXSAV = DSEDIVAX
118000070809     C*
118100070809     C* Effettuo considerazioni x simulazione forzata salto pagina (ma solo se conviene davvero)
118200070809     C                   Z-ADD     *zeros        wRigheVuote       2 0
118300070809     C                   EVAL      wRigheVuote = wVAXmaster -
118400070809     C                             (42 * %div(wVAXmaster:42))
118500070809     C                   IF        wRigheVuote > *zeros
118600070809     C                   EVAL      wRigheVuote =
118700070809     C                             (42 * (%div(wVAXmaster:42)+1)) -
118800070809     C                             wVAXmaster
118900070809     C                   IF        wRigheVuote < 21
119000070809     C                   Z-ADD     wRigheVuote   wInsRighe         2 0
119100070809     C                   ELSE
119200070809     C                   Z-ADD     3             wInsRighe
119300070809     C                   ENDIF
119400070809     C*
119500070809     C                   Z-ADD     1             wRigaVuota        2 0
119600070809     C                   DOW       wRigaVuota <= wInsRighe
119700070809     C*
119800070809     C* Inserisco riga vuota sulla bolla master (accorpante) fino a capienza ultima pagina
119900070809     C                   EVAL      vaxAAS = WRK_vabAAS
120000070809     C                   EVAL      vaxLNP = WRK_vabLNP
120100070809     C                   EVAL      vaxNRS = WRK_vabNRS
120200070809     C                   EVAL      vaxNSP = WRK_vabNSP
120300070809     C                   EVAL      vaxPRR = wVAXmaster + wRigaVuota
120400070809     C                   EVAL      vaxDTT = *blanks
120500070809     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
120600070809     C   60              EVAL      vaxNSP = ACC_NSP
120700070809     C* Inserisco record d dettaglio in FIVAX
120800070809     C                   WRITE     FIVAX000
120900070809     C*
121000070809     C                   ADD       1             wRigaVuota
121100070809     C                   ENDDO
121200070809     C                   ENDIF
121300070809     C*
121400070809     C* Ripristino il formato record corrente
121500070809     C                   EVAL      DSEDIVAX = DSEDIVAXSAV
121600070809     C*
121700070809     C                   ENDSR
121800031112     C*------------------------------------------------------------------------*
121900031112     C* WRIVAB - SCRITTIURA FILE FIVAB X BOLLA "MASTER" ACCORPATA
122000031112     C*------------------------------------------------------------------------*
122100031112     C     WRIVAB        BEGSR
122200031112     C*
122300031112     C* Innanzitutto svuoto il buffer del record d testata d output (FIVAB)
122400031112     C                   CLEAR                   FIVAB000
122500031112     C* Valorizzo il buffer del record d testata d output (FIVAB)
122600031112     C                   EVAL      DSEDIVABOUT = DSEDIVABW
122700070420     C*
122800070420     C* Procedo solo se buffer effettivamente valorizzato
122900070420     C                   IF        DSEDIVABOUT <> DSEDIVABINZ
123000070420     C*
123100031112     C* Resetto alcuni campi che nella bolla "master" occcorrono NN valorizzati
123200031113     C   55              CLEAR                   vabNCD
123300031113     C   55              CLEAR                   vabNCA
123400080226     C   54
123500080226     CAN 55              CLEAR                   vabRMO
123600031114     C   55              CLEAR                   vabCMO
123700031114     C   55              CLEAR                   vabNMO
123800050620     C                   IF        *IN55 = *on
123900050620     C* Campo RIFERIMENTO MITTENTE
124000050701     C                   IF        wRMO <> *blanks
124100060530     C                   Z-ADD     wGiro         wGiro2
124200060530     C                   EVAL      vabRMO = 'N'+%editw(wGiro2:'  ')+':'+
124300040303     C                                      %trim(wRMO)
124400050610     C                   IF        wGiro > 1 AND wOltreRIF = 'S'
124500040514     C                   MOVE      '...'         vabRMO
124600040303     C                   ENDIF
124700050701     C                   ENDIF
124800080929     C* Campi NOTE
124900080929     C*
125000080929     C* ..gestisco tipo raggruppamento note
125100080929     C                   IF        wRgrNOT = 'U'
125200080929     C   51              EVAL      vabNOT = %subst(wNOTNT2:01:35)
125300080929     C   51              EVAL      vabNT2 = %subst(wNOTNT2:36:35)
125400080929     C                   ELSE
125500060726     C   51              EVAL      vabNOT = %trim(wNOT)
125600060726     C   51              EVAL      vabNT2 = %trim(wNT2)
125700080929     C                   ENDIF
125800080929     C*
125900040303     C                   ENDIF
126000070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
126100070227     C   60              EVAL      vabNSP = ACC_NSP
126200031112     C* Inserisco record d testata in FIVAB
126300031112     C                   WRITE     FIVAB000
126400070420     C                   ENDIF
126500050620     C*
126600050620     C* Inizializzo indicatori d wrk
126700050701     C                   SETOFF                                       51
126800031112     C*
126900031112     C                   ENDSR
127000070227     C*------------------------------------------------------------------------*
127100040709     C* CHKREC - CONTROLLO CORRETTEZZA RECORD TESTATA BOLLE AI FINI DEL RAGGRUPPAMENTO
127200040709     C*------------------------------------------------------------------------*
127300040709     C     CHKREC        BEGSR
127400040709     C*
127500040709     C* Inizializzo il flag d correttezza record testata bolle
127600040709     C                   MOVEL     'S'           wRECOK            1
127700051215     C*
127800051215     C* Verifica se bolla corrente presenta particolarità consegna RC e cliente nn abilitato
127900051215     C* specificatamente all'accorpamento anche con part. consegna RC
128000051215     C                   IF        wRECOK = 'S'
128100051216     C                   IF        SQL_vabGMA = 'RC'
128200051216     C                   IF        wAccRC <> 'S'
128300051215     C                   MOVEL     'N'           wRECOK
128400051216     C                   ELSE
128500051216     C* Se abilitazione ok controllo subito anche l'ORM relativo (quello che poi sarà aggiornato)
128600051216     C                   CLEAR                   wRC_KEY
128700051216     C                   EVAL      wRC_AAS = SQL_vabAAS
128800051216     C                   EVAL      wRC_LNP = SQL_vabLNP
128900051216     C                   EVAL      wRC_NRS = SQL_vabNRS
129000051216     C                   EVAL      wRC_NSP = SQL_vabNSP
129100051216     C                   CLEAR                   FNLS24DS
129200051216     C                   EVAL      I24TLA  = *blanks
129300051216     C                   EVAL      I24OPE  = 'L'
129400051216     C                   EVAL      I24KSC  = SQL_vabCCM
129500051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
129600051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
129700051216     C                   CALL      'FNLS24R'
129800051216     C                   PARM                    FNLS24DS
129900051216     C* Se tutto ok procedo altrimenti skippo la bolla corrente con particolarità RC ai fini
130000051216     C* dell'accorpamento
130100051216     C                   IF        O24ERR <> *zeros
130200051216     C                   MOVEL     'N'           wRECOK
130300051216     C                   ENDIF
130400051215     C                   ENDIF
130500051216     C                   ENDIF
130600051215     C                   ENDIF
130700040709     C*
130800040709     C* Verifica codice trattamento
130900051215     C                   IF        wRECOK = 'S'
131000040709     C                   EXSR      CHKCTM
131100051215     C                   ENDIF
131200040709     C*
131300040709     C                   ENDSR
131400040709     C*------------------------------------------------------------------------*
131500040709     C* CHKCTM - CONTROLLO CODICE TRATTAMENTO MERCE
131600040709     C*------------------------------------------------------------------------*
131700040709     C     CHKCTM        BEGSR
131800040709     C*
131900040709     C* Verifico che il codice trattamento merce della bolla corrente nn preveda il VAX
132000040709     C                   EVAL      tblKUT = 1
132100040709     C                   EVAL      tblCOD = '1B'
132200040709     C                   EVAL      tblKEY = SQL_vabCTM
132300040709     C     KEYtab        CHAIN     TABEL00F
132400040709     C                   IF        %found(TABEL00F)
132500040709     C                   EVAL      DS1B = tblUNI
132600070809     C                   IF        §1BF16 = 'S' AND wAccVX <> 'S'
132700040709     C                   MOVEL     'N'           wRECOK
132800040709     C                   ENDIF
132900040709     C                   ENDIF
133000040709     C*
133100040709     C                   ENDSR
133200040227     C*------------------------------------------------------------------------*
133300040227     C* CHGCTM - CAMBIO CODICE TRATTAMENTO MERCE
133400040227     C*------------------------------------------------------------------------*
133500040227     C     CHGCTM        BEGSR
133600040227     C*
133700040227     C* Se sulla testata bolle NN è espressamente indicato il codice tratamento merce
133800040227     C* (e naturalmente o c'è il dettaglio colli o l'indicazione in testata dei segnacolli
133900040227     C* sequenziali) imposto come default il CTM = '2D'
134000040227     C                   IF        WRK_vabCTM = *blanks
134100040227     C                   EVAL      WRK_vabCTM = '2D'
134200040227     C                   ENDIF
134300040227     C*
134400040227     C* Valorizzo la chiave x reperire le informazioni relative al codice
134500040227     C* trattamento merce da impostare in caso d raggruppamento bolle
134600040227     C                   EVAL      tblKUT = 1
134700040227     C                   EVAL      tblCOD = '1B'
134800040227     C                   EVAL      tblKEY = WRK_vabCTM
134900040227     C     KEYtab        CHAIN     TABEL00F
135000040227     C                   IF        %found(TABEL00F)
135100040227     C                   EVAL      DS1B = tblUNI
135200040227     C                   IF        §1BCTR <> *blanks
135300040227     C                   EVAL      WRK_vabCTM = §1BCTR
135400040227     C                   ENDIF
135500040227     C                   ENDIF
135600040227     C*
135700040227     C                   ENDSR
135800050610     C*------------------------------------------------------------------------*
135900051021     C* REPABD - REPERISCE I DATI TABELLATI CHE PILOTANO LE MODALITA DI ACCORPAMENTO
136000050610     C*------------------------------------------------------------------------*
136100050610     C     REPABD        BEGSR
136200050610     C*
136300050610     C                   EVAL      tblKUT = 1
136400050610     C                   EVAL      tblCOD = '3C'
136500050708     C                   MOVEL(P)  D86CCM        tblKEY
136600050610     C     KEYtab        CHAIN     TABEL00F
136700050610     C                   IF        %found(TABEL00F)
136800050610     C                   EVAL      DS3C = tblUNI
136900050610     C                   MOVEL     §3CABD        wTipABD
137000050610     C                   Z-ADD     §3CLRM        wLenRIF
137100050610     C                   MOVEL     §3CALM        wAlmRIF
137200050620     C                   MOVEL     §3CNOT        wRgrNOT
137300051021     C                   MOVEL     §3CABC        wAccCAS
137400051215     C                   MOVEL     §3CARC        wAccRC
137500070809     C                   MOVEL     §3CAVX        wAccVX
137600050610     C                   ENDIF
137700080226     C*
137800080226     C* Se lunghezza riferimento è zero accendo indicatore preposto x nn sovrascrittura RMO
137900080226     C                   if        wLenRIF <> *zeros
138000080226     C                   seton                                        54
138100080226     C                   endif
138200070228     C*
138300070228     C* Se cliente che necessita ritorno informazioni d accorpamento => accendo indicatore preposto
138400081006     C                   if        wTipABD <> *blanks
138500070228     C                   seton                                        60
138600070228     C                   else
138700070228     C                   setoff                                       60
138800070228     C                   endif
138900050610     C*
139000050610     C                   ENDSR
139100070227     C*------------------------------------------------------------------------*
139200070419     C* REPNUM - STACCA NUMERATORE X MANTENIMENTO NUMERO SPEDIZIONE
139300070227     C*------------------------------------------------------------------------*
139400070227     C     REPNUM        BEGSR
139500070227     C*
139600070419     C* Numero spedizione (da Flnuf cod. 3)
139700070419     C                   clear                   Trul34ds
139800070419     C                   eval      i34aas = SQL_VABAAS
139900070419     C                   eval      i34lnp = SQL_VABLNP
140000070419     C                   call      'TRUL34R2'
140100070419     C                   parm                    kpjba
140200070419     C                   parm                    Trul34ds
140300070419     C                   if        o34err = *Zeros
140400070419     C                   z-add     o34nsp        ACC_NSP
140500070419     C                   else
140600070420     C                   z-add     SQL_VABNSP    ACC_NSP
140700070419     C                   endif
140800070227     C*
140900070227     C                   ENDSR
141000070227     C*------------------------------------------------------------------------*
141100070227     C* WRIRAB - SCRITTURA FILE RACCORDO BOLLE CLIENTE "ACCORPANTI"/"ACCORPATE"
141200070227     C*------------------------------------------------------------------------*
141300070227     C     WRIRAB        BEGSR
141400070227     C*
141500070227     C* Chiave spedizione
141600070227     C                   eval      rabAAS  = SQL_vabAAS
141700070227     C                   eval      rabLNP  = SQL_vabLNP
141800070227     C                   eval      rabNRS  = SQL_vabNRS
141900070227     C                   eval      rabNSP  = ACC_NSP
142000070227     C                   eval      rabCCM  = SQL_vabCCM
142100070227     C* Riferimenti cliente "accorpanti"
142200070227     C                   eval      rabRMN  = WRK_vabRMN
142300070227     C                   eval      rabRMA  = WRK_vabRMA
142400070227     C* Riferimenti cliente "accorpati"
142500070227     C                   eval      rabRMNR = SQL_vabRMN
142600070227     C                   eval      rabRMAR = SQL_vabRMA
142700070228     C* Attributi
142800070228     C                   eval      rabDAT  = datcor
142900070227     C*
143000070301     C                   write     FIRAB000
143100070227     C*
143200070227     C                   ENDSR
143300970526     C*------------------------------------------------------------------------*
143400970526     C* *INZSR - OPERAZIONI INIZIALI
143500970526     C*------------------------------------------------------------------------*
143600970526     C     *INZSR        BEGSR
143700040714     C*
143800040714     C*  Reperisco parametri in entrata
143900040714     C     *ENTRY        PLIST
144000070420     C                   PARM                    KPJBA
144100040714     C                   PARM                    TRTC86DS
144200070419     C*
144300080226     C                   SETOFF                                       6054
144400060726     C*
144500031113     C*
144600031110     C*---
144700031110     C* CHIAVI DI LETTURA
144800031110     C*---
144900040709     C* CHIAVE DI LETTURA X EDIVABQT1L - Completa
145000040709     C     KEYvab1       KLIST
145100040709     C                   KFLD                    SQL_vabCCM
145200040709     C                   KFLD                    SQL_vabCMR
145300040709     C                   KFLD                    SQL_vabCNT
145400040709     C                   KFLD                    SQL_vabAAS
145500040709     C                   KFLD                    SQL_vabLNP
145600040709     C                   KFLD                    SQL_vabNRS
145700040709     C                   KFLD                    SQL_vabNSP
145800070807     C* CHIAVE DI LETTURA X EDIVADQT1L/EDIVATQT1L/EDIVAXQT1L
145900040712     C     KEYva_1       KLIST
146000031112     C                   KFLD                    SQL_vabFGS
146100031112     C                   KFLD                    SQL_vabCCM
146200031112     C                   KFLD                    SQL_vabCMR
146300031112     C                   KFLD                    SQL_vabCNT
146400031112     C                   KFLD                    SQL_vabAAS
146500031112     C                   KFLD                    SQL_vabLNP
146600031112     C                   KFLD                    SQL_vabNRS
146700031112     C                   KFLD                    SQL_vabNSP
146800040227     C* CHIAVE DI LETTURA X TABEL00F - Completa
146900040227     C     KEYtab        KLIST
147000040227     C                   KFLD                    tblKUT
147100040227     C                   KFLD                    tblCOD
147200040227     C                   KFLD                    tblKEY
147300070807     C** CHIAVE DI LETTURA X EDIVAX2L - Completa
147400070807     C*     KEYvax2_C     KLIST
147500070807     C*                   KFLD                    D86CCM
147600070807     C*                   KFLD                    D86CMR
147700070807     C*                   KFLD                    D86CNT
147800070228     C*
147900041116     C*  Richiamo XPARUT
148000041116     C                   Z-ADD     1             CODUT             1 0
148100041116     C                   CALL      'X§PARUT'
148200041116     C                   PARM                    UT§DSE
148300041116     C                   MOVEL     REC80         CNCR80
148400041116     c
148500041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
148600041116     C                   CLEAR                   TRUL06DS
148700041116     C                   MOVE      '£1'          D06COD
148800041116     C                   MOVEL     SIMFEL        D06KEY
148900041116     C                   MOVEL     'L'           D06TLA
149000041116     C                   MOVEL     TRUL06DS      KPJBU
149100041116      *
149200041116     C                   CALL      'TRUL06R'
149300041116     C                   PARM                    KPJBA
149400041116     C                   MOVEL     KPJBU         TRUL06DS
149500070228     C*
149600070228     C* CALCOLA LA DATA CORRENTE
149700070228     C                   time                    wn14             14 0
149800070228     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
149900070228     C                   z-add     wn8           g08dat
150000070228     C                   z-add     *zeros        g08inv
150100070228     C                   movel     '0'           g08err
150200070228     C                   call      'XSRDA8'
150300070228     C                   parm                    wlbda8
150400070228     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
150500991112     C*
150600991112     C                   ENDSR
