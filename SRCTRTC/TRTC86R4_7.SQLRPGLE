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
011900051215     D wFlgOrmRC       S              1N   INZ(*off)
012000070227     D ACC_NSP         S                   LIKE(VABNSP)
012100070807     D wVAXmaster      s              9S 0 inz(*zeros)
012200070905     D wVAXmaster01    s                   like(wVAXmaster) inz(*zeros)
012300070905     D wVAXmaster02    s                   like(wVAXmaster) inz(*zeros)
012400050610     D*------------
012500050620     D* VARIABILE RELATIVE AL RAGGRUPPAMENTO BOLLE
012600050610     D*------------
012700050610     D wTipABD         S                   like(§3CABD)
012800050610     D wLenRIF         S                   like(§3CLRM)
012900050610     D wAlmRIF         S                   like(§3CALM)
013000050620     D wRgrNOT         S                   like(§3CNOT)
013100051021     D wAccCAS         S                   like(§3CABC)
013200051215     D wAccRC          S                   like(§3CARC)
013300070227     D wSpedCli        S                   like(§3CFSP)
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
057500051215     C                   eval      wFlgOrmRC = *off
057600031110     C*
057700031110     C                   ENDSR
057800031110     C*------------------------------------------------------------------------*
057900031110     C* VALDSROT - VALORIZZAZIONE DS LIVELLO ROTTURA
058000031110     C*------------------------------------------------------------------------*
058100031112     C     VALDSROT      BEGSR
058200031110     C*
058300031110     C* Campi in rottura
058400031114     C                   EVAL      S_vabCCM = SQL_vabCCM
058500031110     C                   EVAL      S_vabRSD = SQL_vabRSD
058600031110     C                   EVAL      S_vabRD2 = SQL_vabRD2
058700031110     C                   EVAL      S_vabIND = SQL_vabIND
058800031110     C                   EVAL      S_vabCAD = SQL_vabCAD
058900031110     C                   EVAL      S_vabLOD = SQL_vabLOD
059000031110     C                   EVAL      S_vabPRD = SQL_vabPRD
059100031110     C                   EVAL      S_vabNZD = SQL_vabNZD
059200031110     C                   EVAL      S_vabLNA = SQL_vabLNA
059300031110     C                   EVAL      S_vabZNC = SQL_vabZNC
059400031110     C                   EVAL      S_vabCBO = SQL_vabCBO
059500031110     C                   EVAL      S_vabTSP = SQL_vabTSP
059600031110     C                   EVAL      S_vabCTR = SQL_vabCTR
059700031110     C                   EVAL      S_vabGC1 = SQL_vabGC1
059800031110     C                   EVAL      S_vabGC2 = SQL_vabGC2
059900031110     C                   EVAL      S_vabFFD = SQL_vabFFD
060000031110     C                   EVAL      S_vabDCR = SQL_vabDCR
060100031110     C                   EVAL      S_vabTCR = SQL_vabTCR
060200031110     C                   EVAL      S_vabHCR = SQL_vabHCR
060300051216     C  N52              EVAL      S_vabGMA = SQL_vabGMA
060400031110     C                   EVAL      S_vabGGA = SQL_vabGGA
060500031110     C                   EVAL      S_vabGVA = SQL_vabGVA
060600031110     C                   EVAL      S_vabTC1 = SQL_vabTC1
060700031110     C                   EVAL      S_vabTC2 = SQL_vabTC2
060800031110     C                   EVAL      S_vabVAS = SQL_vabVAS
060900031110     C                   EVAL      S_vabVAD = SQL_vabVAD
061000031110     C*
061100031110     C                   ENDSR
061200031110     C*------------------------------------------------------------------------*
061300031110     C* IMPVAB - VALORIZZAZIONE CAMPI "FISSI" 1° BOLLA D OGNI LIVELLO D ROTTURA
061400031110     C*------------------------------------------------------------------------*
061500031110     C     IMPVAB        BEGSR
061600031110     C*
061700031110     C* Valorizzo la DS d wrk da quella d lettura SQL
061800031110     C                   EVAL      DSEDIVABW = DSEDIVAB
061900031110     C*
062000031110     C                   ENDSR
062100051216     C*------------------------------------------------------------------------*
062200051216     C* EXERC - GESTIONE EVENTUALE PRESENZA PARTICOLARITA' CONSEGNA 'RC'
062300051216     C*------------------------------------------------------------------------*
062400051216     C     EXERC         BEGSR
062500051216     C*
062600051216     C* Gestisco anche l'accorpamento anche dell'ORM da "RC"
062700051216     C* ...ma solo se trattasi d primo "RC" delle bolle oggetto d accorpamento
062800060126     C**!!!              IF        SQL_vabGMA = 'RC' and
062900060126     C**!!!                        wFlgOrmRC = *off
063000070227 MB 1C                   IF        SQL_vabGMA = 'RC'
063100070227 MB 2c                   If        wflgormrc = *On
063200060127     C*
063300060127     C* Elimino l'ORM RC della bolla corrente (accorpata)
063400060126     c                   Clear                   Fnls25ds
063500060126     c                   Eval      i25ksc  = sql_vabccm
063600060126     c                   Move(p)   sql_vabaas    i25aas
063700060126     c                   Eval      i25spe = %editc(sql_vablnp:'X') +
063800060126     c                                      %editc(sql_vabnrs:'X') +
063900060126     c                                      %editc(sql_vabnsp:'X')
064000060126     c                   Call      'FNLS25R'
064100060126     c                   Parm                    Fnls25ds
064200060126    2C                   ENDIF
064300060126    2C                   If        wFlgOrmRC = *off
064400060127     C*
064500060127     C* Aggiorno l'orm RC legato alla bolla risultante dall'accorpamento solo se
064600060127     C* bolla corrente (accorpata) e bolla accorpante hanno chiave diversa.
064700060127     C                   IF        SQL_vabAAS <> WRK_vabAAS OR
064800060127     C                             SQL_vabLNP <> WRK_vabLNP OR
064900060127     C                             SQL_vabNRS <> WRK_vabNRS OR
065000060127     C                             SQL_vabNSP <> WRK_vabNSP
065100060127     C*
065200060127     C* Prima dell'aggiornamento del riferimento ORM RC con la nuova chiave spedizione
065300060127     C* elimino l'ORM RC della bolla accorpante
065400060127     c                   Clear                   Fnls25ds
065500060127     c                   Eval      i25ksc  = sql_vabccm
065600060127     c                   Move(p)   sql_vabaas    i25aas
065700060127     c                   Eval      i25spe = %editc(WRK_vabLNP:'X') +
065800060127     c                                      %editc(WRK_vabNRS:'X') +
065900060127     c                                      %editc(WRK_vabNSP:'X')
066000060127     c                   Call      'FNLS25R'
066100060127     c                   Parm                    Fnls25ds
066200060127     C*
066300060127     C* Quindi aggiorno l'ORM RC della bolla corrente (accorpata) con la nuova chiave
066400060127     C* spedizione (bolla accorpante)
066500051216     C                   CLEAR                   wRC_KEY
066600051216     C                   EVAL      wRC_AAS = SQL_vabAAS
066700051216     C                   EVAL      wRC_LNP = SQL_vabLNP
066800051216     C                   EVAL      wRC_NRS = SQL_vabNRS
066900051216     C                   EVAL      wRC_NSP = SQL_vabNSP
067000051220     C                   EVAL      I24TLA  = 'L'
067100051216     C                   EVAL      I24OPE  = 'U'
067200051216     C                   EVAL      I24KSC  = SQL_vabCCM
067300051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
067400051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
067500051216     C                   EVAL      I24NAAS = WRK_vabAAS
067600051216     C                   EVAL      I24LNP  = WRK_vabLNP
067700051216     C                   EVAL      I24NRS  = WRK_vabNRS
067800051216     C                   EVAL      I24NSP  = WRK_vabNSP
067900051216     C                   CALL      'FNLS24R'
068000051216     C                   PARM                    FNLS24DS
068100060127     C*
068200051216     C* Se tutto ok => accendo l'indicatore x nn aggiornare ORM generati da bolle "RC"
068300051216     C* accorpate successivamente (ovviamente x medesimo destinatario)
068400060126    3C                   IF        O24ERR = *zeros
068500051216     C                   EVAL      wFlgOrmRC = *on
068600060126   x3C                   ELSE
068700051216     C                   Z-ADD     1             wFlgErr
068800060126    3C                   ENDIF
068900060127     C                   ENDIF
069000060126    2C                   ENDIF
069100060126    1C                   ENDIF
069200051216     C*
069300051216     C                   ENDSR
069400031110     C*------------------------------------------------------------------------*
069500031110     C* EXESOMMA - SOMMATORIA CAMPI X RAGGRUPPAMENTO
069600031110     C*------------------------------------------------------------------------*
069700031110     C     EXESOMMA      BEGSR
069800031110     C*
069900051216     C* Eseguo sommatoria dati bolle
070000031110     C                   EVAL      WRK_vabIAS = WRK_vabIAS + SQL_vabIAS
070100031110     C                   EVAL      WRK_vabNCL = WRK_vabNCL + SQL_vabNCL
070200031110     C                   EVAL      WRK_vabPKB = WRK_vabPKB + SQL_vabPKB
070300031110     C                   EVAL      WRK_vabVLB = WRK_vabVLB + SQL_vabVLB
070400031110     C                   EVAL      WRK_vabQFT = WRK_vabQFT + SQL_vabQFT
070500031110     C                   EVAL      WRK_vabVMD = WRK_vabVMD + SQL_vabVMD
070600051021     C                   EVAL      WRK_vabCAS = WRK_vabCAS + SQL_vabCAS
070700031110     C*
070800031110     C                   ENDSR
070900031110     C*------------------------------------------------------------------------*
071000031110     C* EXENORM - NORMALIZZAZIONE CAMPI X OTTENIMENTO BOLLA "MASTER"
071100031110     C*------------------------------------------------------------------------*
071200031110     C     EXENORM       BEGSR
071300050620     C*
071400050620     C* Valorizzo indicatore d wrk
071500050620     C                   SETON                                        51
071600031110     C*
071700031114     C                   IF        WRK_vabNAS =  *blanks AND
071800031114     C                             SQL_vabNAS <> *blanks
071900031110     C                   EVAL      WRK_vabNAS = SQL_vabNAS
072000031110     C                   ENDIF
072100031110     C*
072200031114     C                   IF        WRK_vabCTM =  *blanks AND
072300031114     C                             SQL_vabCTM <> *blanks
072400031110     C                   EVAL      WRK_vabCTM = SQL_vabCTM
072500031110     C                   ENDIF
072600031110     C*
072700031114     C                   IF        WRK_vabCTS =  *blanks AND
072800031114     C                             SQL_vabCTS <> *blanks
072900031110     C                   EVAL      WRK_vabCTS = SQL_vabCTS
073000031110     C                   ENDIF
073100031110     C*
073200031114     C                   IF        WRK_vabFTM =  *blanks AND
073300031114     C                             SQL_vabFTM <> *blanks
073400031110     C                   EVAL      WRK_vabFTM = SQL_vabFTM
073500031110     C                   ENDIF
073600031110     C*
073700031114     C                   IF        WRK_vabSCL =  *blanks AND
073800031114     C                             SQL_vabSCL <> *blanks
073900031110     C                   EVAL      WRK_vabSCL = SQL_vabSCL
074000031110     C                   ENDIF
074100051021     C*
074200051021     C                   IF        WRK_vabVCA =  *blanks AND
074300051021     C                             SQL_vabVCA <> *blanks
074400051021     C                   EVAL      WRK_vabVCA = SQL_vabVCA
074500051021     C                   ENDIF
074600051021     C*
074700051021     C                   IF        WRK_vabTIC =  *blanks AND
074800051021     C                             SQL_vabTIC <> *blanks
074900051021     C                   EVAL      WRK_vabTIC = SQL_vabTIC
075000051021     C                   ENDIF
075100051216     C*
075200051216     C                   IF        WRK_vabGMA =  *blanks AND
075300051216     C                             SQL_vabGMA <> *blanks
075400051216     C                   EVAL      WRK_vabGMA = SQL_vabGMA
075500051216     C                   ENDIF
075600031110     C*
075700031110     C* Gestisco comportamento particolare del campo VABANT (tipo e numero bancali)
075800031110     C                   IF        WRK_vabANT = *zeros
075900031110     C                   EVAL      WRK_vabANT = SQL_vabANT
076000031110     C                   ELSE
076100031110     C                   IF        SQL_vabANT = *zeros
076200031110     C                   ELSE
076300031110     C                   EVAL      DSVABANT = SQL_vabANT
076400031110     C                   EVAL      wANTTIP1 = ANTTIP1
076500031110     C                   EVAL      wANTNUM1 = ANTNUM1
076600031110     C                   EVAL      wANTTIP2 = ANTTIP2
076700031110     C                   EVAL      wANTNUM2 = ANTNUM2
076800031110     C                   EVAL      DSVABANT = WRK_vabANT
076900031110     C                   IF        ANTTIP1 > *zeros
077000031110     C                   IF        wANTTIP1 = ANTTIP1
077100031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM1
077200031110     C                   ENDIF
077300031110     C                   IF        wANTTIP2 = ANTTIP1
077400031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM2
077500031110     C                   ENDIF
077600031110     C                   ELSE
077700031110     C                   IF        wANTTIP1 > *zeros
077800031110     C                   EVAL      ANTTIP1 = wANTTIP1
077900031110     C                   EVAL      ANTNUM1 = wANTNUM1
078000031110     C                   ELSE
078100031110     C                   IF        wANTTIP2 > *zeros
078200031110     C                   EVAL      ANTTIP1 = wANTTIP2
078300031110     C                   EVAL      ANTNUM1 = wANTNUM2
078400031110     C                   ENDIF
078500031110     C                   ENDIF
078600031110     C                   ENDIF
078700031110     C                   IF        ANTTIP2 > *zeros
078800031110     C                   IF        wANTTIP1 = ANTTIP2
078900031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM1
079000031110     C                   ENDIF
079100031110     C                   IF        wANTTIP2 = ANTTIP2
079200031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM2
079300031110     C                   ENDIF
079400031110     C                   ELSE
079500031110     C                   IF        wANTTIP1 > *zeros
079600031110     C                   EVAL      ANTTIP2 = wANTTIP1
079700031110     C                   EVAL      ANTNUM2 = wANTNUM1
079800031110     C                   ELSE
079900031110     C                   IF        wANTTIP2 > *zeros
080000031110     C                   EVAL      ANTTIP2 = wANTTIP2
080100031110     C                   EVAL      ANTNUM2 = wANTNUM2
080200031110     C                   ENDIF
080300031110     C                   ENDIF
080400031110     C                   ENDIF
080500031110     C                   EVAL      WRK_vabANT = DSVABANT
080600031110     C                   ENDIF
080700031110     C                   ENDIF
080800031114     C*
080900051021     C* Eseguo raggruppamento dei riferimenti delle testate bolle accorpate
081000051021     C                   IF        wLenRIF > *zeros
081100050906     C                   IF        wTIPABD = 'S'
081200050614     C                   EVAL      wRIF = %trim(%editc(SQL_vabRMN:'4'))
081300050610     C                   ENDIF
081400050610     C                   IF        wTIPABD = 'A'
081500050610     C                   EVAL      wRIF = %trim(SQL_vabRMA)
081600050610     C                   ENDIF
081700050610     C*
081800050610     C* Gestisco l'allineamento d default a seconda della tipologia d riferimento da utilizzare
081900050610     C                   IF        wAlmRIF = *blanks
082000050610     C                   IF        wTipABD = 'S'
082100050610     C                   EVAL      wAlmRIF = 'D'
082200050610     C                   ENDIF
082300050610     C                   IF        wTipABD = 'A'
082400050610     C                   EVAL      wAlmRIF = 'S'
082500050610     C                   ENDIF
082600050610     C                   ENDIF
082700051021     C*
082800051021     C* Effettuo le operazioni seguenti SOLO in caso in cui il riferimento voluto
082900051021     C* risulti essere effettivamente valorizzato
083000051021     C                   IF        wRIF <> *blanks
083100050614     C*
083200050614     C* Considero la minore tra la lunghezza reale del riferimento da considerare e il
083300050614     C* numero d byte da considerare indicati in tabella x il cliente corrente
083400050614     C                   Z-ADD     *zeros        wByteRIF          2 0
083500050614     C                   IF        wLenRIF <= %len(%trim(wRIF))
083600050614     C                   EVAL      wByteRIF = wLenRIF
083700050614     C                   ELSE
083800050614     C                   EVAL      wByteRIF = %len(%trim(wRIF))
083900050614     C                   ENDIF
084000050610     C*
084100050610     C* A questo punto verifico se sono richiesti solo 1 numero specifico d byte ed il
084200050610     C* relativo allineamento
084300050610     C                   IF        wLenRIF <> 99
084400050610     C                   IF        wAlmRIF = 'S'
084500050614     C                   EVAL      wRIF = %subst(%trim(wRIF):1:wByteRIF)
084600050610     C                   ENDIF
084700050610     C                   IF        wAlmRIF = 'D'
084800050610     C                   EVAL      wRIF = %subst(%trim(wRIF):
084900050614     C                                    %len(%trim(wRIF))-wByteRIF+1)
085000050610     C                   ENDIF
085100050610     C                   ENDIF
085200051021     C*
085300051021     C                   ELSE
085400051021     C                   MOVEL     'N'           wOltreRIF
085500051021     C                   ENDIF
085600050610     C*
085700031114     C* Se c stanno...
085800050610     C                   IF        %len(%trim(wRMO))+1+%len(%trim(wRIF)) <=
085900040517     C                             %len(wRMO)-4
086000050610     C                   EVAL      wRMO = %trim(wRMO) + %trim(wRIF) + '-'
086100050610     C                   MOVEL     'N'           wOltreRIF         1
086200050610     C                   ELSE
086300050610     C                   MOVEL     'S'           wOltreRIF
086400031114     C                   ENDIF
086500050906     C*
086600050906     C                   ELSE
086700050906     C                   MOVEL     'N'           wOltreRIF
086800050906     C                   ENDIF
086900051021     C*
087000050620     C*
087100050620     C* Se richiesto raggruppo anche le note
087200050620     C                   IF        wRgrNOT = 'S'
087300060726     C* Memorizzo subito note originali
087400060726     C                   IF        wNOT = *blanks AND WRK_vabNOT <> *blanks AND
087500060726     C                             %scan(%trim(WRK_vabNOT):wNT2) = 0
087600060726     C                   EVAL      wNOT = %trim(WRK_vabNOT)
087700060726     C                   ENDIF
087800060726     C                   IF        wNT2 = *blanks AND WRK_vabNT2 <> *blanks AND
087900060726     C                             %scan(%trim(WRK_vabNT2):wNOT) = 0
088000060726     C                   EVAL      wNT2 = %trim(WRK_vabNT2)
088100060726     C                   ENDIF
088200060629     C***  VABNOT
088300060726     C* Se presente nota => gestisco
088400060726     C                   IF        SQL_vabNOT <> *blanks
088500060629     C* Innanzitutto verifico che il riferimento corrente nn sia già presente
088600060726     C                   SETOFF                                       40
088700060726     C                   IF        wNOT <> *blanks
088800060726     C                   IF        %scan(%trim(SQL_vabNOT):wNOT) <> 0 OR
088900060726     C                             %scan(%trim(SQL_vabNOT):wNT2) <> 0
089000060726     C                   SETON                                        40
089100060726     C                   ENDIF
089200060726     C                   ENDIF
089300060629     C* Verifico se lo spazio su NOT è già esaurito mentre quello su NT2 è ancora dispnibile
089400060726     C                   IF        *IN40 = *OFF
089500060629     C                   IF        (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNOT))
089600060629     C                             >%size(wNOT)) AND
089700060629     C                             (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNOT))
089800060629     C                             <=%size(wNT2))
089900060726     C                   IF        %scan(%trim(SQL_vabNOT):wNT2) = 0 AND
090000060726     C                             %scan(%trim(SQL_vabNOT):wNOT) = 0
090100060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNOT)
090200060726     C                   ENDIF
090300060629     C                   ELSE
090400050708     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNOT)
090500060726     C                   ENDIF
090600060726     C                   ENDIF
090700060726     C                   ENDIF
090800060629     C***  VABNT2
090900060726     C* Se presente nota => gestisco
091000060726     C                   IF        SQL_vabNT2 <> *blanks
091100060629     C* Innanzitutto verifico che il riferimento corrente nn sia già presente
091200060726     C                   SETOFF                                       40
091300060726     C                   IF        wNT2 <> *blanks
091400060726     C                   IF        %scan(%trim(SQL_vabNT2):wNT2) <> 0 OR
091500060726     C                             %scan(%trim(SQL_vabNT2):wNOT) <> 0
091600060726     C                   SETON                                        40
091700060726     C                   ENDIF
091800060726     C                   ENDIF
091900060629     C* Verifico se lo spazio su NT2 è già esaurito mentre quello su NOT è ancora dispnibile
092000060726     C                   IF        *IN40 = *OFF
092100060629     C                   IF        (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNT2))
092200060629     C                             >%size(wNT2)) AND
092300060629     C                             (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNT2))
092400060629     C                             <=%size(wNOT))
092500060726     C                   IF        %scan(%trim(SQL_vabNT2):wNOT) = 0 AND
092600060726     C                             %scan(%trim(SQL_vabNT2):wNT2) = 0
092700060629     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNT2)
092800060726     C                   ENDIF
092900060629     C                   ELSE
093000060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNT2)
093100060629     C                   ENDIF
093200060629     C                   ENDIF
093300060726     C                   ENDIF
093400060629     C*
093500050620     C                   ELSE
093600050708     C                   EVAL      wNOT = %trim(SQL_vabNOT)
093700050708     C                   EVAL      wNT2 = %trim(SQL_vabNT2)
093800050620     C                   ENDIF
093900031110     C*
094000031110     C                   ENDSR
094100031112     C*------------------------------------------------------------------------*
094200031112     C* WRIVAD - SCRITTIURA FILE FIVAD
094300031112     C*------------------------------------------------------------------------*
094400031112     C     WRIVAD        BEGSR
094500031112     C*
094600031112     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAD)
094700031112     C                   CLEAR                   FIVAD000
094800031112     C*
094900051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAD
095000040712     C     KEYva_1       SETLL     EDIVADQT1L
095100031113     C                   IF        %equal(EDIVADQT1L)
095200040227     C* Eseguo routine x modifica codice trattamento merce
095300040227     C                   EXSR      CHGCTM
095400040227     C*
095500040712     C     KEYva_1       READE     EDIVADQT1L
095600031113     C                   DOW       not %eof(EDIVADQT1L)
095700031112     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
095800031112     C                   EVAL      vadAAS = WRK_vabAAS
095900031112     C                   EVAL      vadLNP = WRK_vabLNP
096000031112     C                   EVAL      vadNRS = WRK_vabNRS
096100031112     C                   EVAL      vadNSP = WRK_vabNSP
096200070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
096300070227     C   60              EVAL      vadNSP = ACC_NSP
096400031112     C* Inserisco record d dettaglio in FIVAD
096500031112     C                   WRITE     FIVAD000
096600031112     C* Elimino record d dettaglio in EDIVAD
096700031113     C                   DELETE    EDIVADQT1L
096800031112     C* Continuo la lettura dei dettagli della testata bolla corrente
096900040712     C     KEYva_1       READE     EDIVADQT1L
097000031112     C                   ENDDO
097100031112     C*
097200031112     C* Se invece nn esiste già il dettaglio colli in EDIVAD e sulla testata c'è l'indicazioine
097300031112     C* dei segnacolli => provvedo a creare il VAD
097400031112     C                   ELSE
097500031112     C                   IF        SQL_vabNCD > *zeros OR
097600031112     C                             SQL_vabNCA > *zeros
097700040227     C* Eseguo routine x modifica codice trattamento merce
097800040227     C                   EXSR      CHGCTM
097900031112     C* Valorizzo la chiave bolla del dettaglio con quella della relativa testata bolla "master"
098000031112     C                   EVAL      vadAAS = WRK_vabAAS
098100031112     C                   EVAL      vadLNP = WRK_vabLNP
098200031112     C                   EVAL      vadNRS = WRK_vabNRS
098300031112     C                   EVAL      vadNSP = WRK_vabNSP
098400031112     C* Valorizzo i restanti campi con quelli della testata bolla corrente
098500031112     C                   EVAL      vadFGS = SQL_vabFGS
098600031112     C                   EVAL      vadCCM = SQL_vabCCM
098700031112     C                   EVAL      vadNCL = SQL_vabNCL
098800031112     C                   EVAL      vadNCD = SQL_vabNCD
098900031112     C                   EVAL      vadNCA = SQL_vabNCA
099000070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
099100070227     C   60              EVAL      vadNSP = ACC_NSP
099200031112     C* Inserisco record d dettaglio in FIVAD
099300031112     C                   WRITE     FIVAD000
099400031112     C                   ENDIF
099500031112     C                   ENDIF
099600031112     C*
099700031112     C                   ENDSR
099800031114     C*------------------------------------------------------------------------*
099900031114     C* WRIVAT - SCRITTIURA FILE FIVAT
100000031114     C*------------------------------------------------------------------------*
100100031114     C     WRIVAT        BEGSR
100200031114     C*
100300031114     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAT)
100400031114     C                   CLEAR                   FIVAT000
100500031114     C*
100600051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAT
100700040712     C     KEYva_1       SETLL     EDIVATQT1L
100800031114     C                   IF        %equal(EDIVATQT1L)
100900040227     C* Eseguo routine x modifica codice trattamento merce
101000040227     C                   EXSR      CHGCTM
101100040227     C*
101200040712     C     KEYva_1       READE     EDIVATQT1L
101300031114     C                   DOW       not %eof(EDIVATQT1L)
101400031114     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
101500031114     C                   EVAL      vatAAS = WRK_vabAAS
101600031114     C                   EVAL      vatLNP = WRK_vabLNP
101700031114     C                   EVAL      vatNRS = WRK_vabNRS
101800031114     C                   EVAL      vatNSP = WRK_vabNSP
101900070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
102000070227     C   60              EVAL      vatNSP = ACC_NSP
102100031114     C* Inserisco record d dettaglio in FIVAT
102200031114     C                   WRITE     FIVAT000
102300031114     C* Elimino record d dettaglio in EDIVAT
102400031114     C                   DELETE    EDIVATQT1L
102500031114     C* Continuo la lettura dei dettagli della testata bolla corrente
102600040712     C     KEYva_1       READE     EDIVATQT1L
102700031114     C                   ENDDO
102800031114     C                   ENDIF
102900031114     C*
103000031114     C                   ENDSR
103100070807     C*------------------------------------------------------------------------*
103200070807     C* WRIVAX - SCRITTIURA FILE FIVAX
103300070807     C*------------------------------------------------------------------------*
103400070807     C     WRIVAX        BEGSR
103500070809     C*
103600070809     C* Solo x record VAX diversi dalla bolla master (accorpante)
103700070809     C                   IF        SQL_vabFGS  = WRK_vabFGS AND
103800070809     C                             SQL_vabCCM  = WRK_vabCCM AND
103900070809     C                             SQL_vabAAS  = WRK_vabAAS AND
104000070809     C                             SQL_vabLNP  = WRK_vabLNP AND
104100070809     C                             SQL_vabNRS  = WRK_vabNRS AND
104200070809     C                             SQL_vabNSP <> WRK_vabNSP
104300070807     C*
104400070807     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAX)
104500070807     C                   CLEAR                   FIVAX000
104600070807     C*
104700070807     C                   Z-ADD     *zeros        wVAXmaster
104800071203     C                   Z-ADD     *zeros        wVAXmaster01
104900071203     C                   Z-ADD     *zeros        wVAXmaster02
105000070807     C*
105100070807     C* Effettuo conteggio righe attuali packing list della bolla master
105200070807     C*
105300070807     C/EXEC SQL
105400070905     C+ declare C1VX cursor for
105500070906     C+ select max(vaxprr) from edivaxqt
105600070911     C+ where
105700070911     C+ vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
105800070807     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
105900070807     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
106000070807     C+ for fetch only
106100070807     C/END-EXEC
106200070807     C
106300070807     C/EXEC SQL
106400070905     C+ open C1VX
106500070807     C/END-EXEC
106600070807     C
106700070807     C/EXEC SQL
106800070905     C+ Fetch C1VX into :wVAXmaster01
106900070807     C/END-EXEC
107000070905     C*
107100070905     C/EXEC SQL
107200070905     C+ declare C2VX cursor for
107300070905     C+ select max(vaxprr) from fivax00f
107400070911     C+ where
107500070911     C+ vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
107600070905     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
107700070905     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
107800070905     C+ for fetch only
107900070905     C/END-EXEC
108000070905     C
108100070905     C/EXEC SQL
108200070905     C+ open C2VX
108300070905     C/END-EXEC
108400070905     C
108500070905     C/EXEC SQL
108600070905     C+ Fetch C2VX into :wVAXmaster02
108700070905     C/END-EXEC
108800070809     C*
108900070905     C* Tengo il maggiore progressivo riga tra EDIVAX e FIVAX
109000070905     C                   EVAL      wVAXmaster = wVAXmaster01
109100070905     C                   IF        wVAXmaster02 > wVAXmaster01
109200070905     C                   EVAL      wVAXmaster = wVAXmaster02
109300070905     C                   ENDIF
109400070905     C*
109500070809     C* Proseguo elaborazione solo se effettivamente trovati VAX da accorpare
109600070905     C                   IF        wVAXmaster > *zeros
109700070807     C*
109800070807     C* Verifico se x la bolla corrente di EDIVAB esiste il relativo packing list in EDIVAX
109900070807     C     KEYva_1       SETLL     EDIVAXQT1L
110000070807     C                   IF        %equal(EDIVAXQT1L)
110100070809     C*
110200070809     C                   SETOFF                                       88
110300070807     C*
110400070807     C     KEYva_1       READE     EDIVAXQT1L
110500070807     C                   DOW       not %eof(EDIVAXQT1L)
110600070809     C*
110700070809     C* Se nn ancora gestito il salto pagina fittizio forzato
110800070809     C                   IF        *IN88 = *off
110900070809     C                   SETON                                        88
111000070905     C***                EXSR      GESFORMFEED
111100070809     C                   ENDIF
111200070809     C*
111300070807     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
111400070807     C                   EVAL      vaxAAS = WRK_vabAAS
111500070807     C                   EVAL      vaxLNP = WRK_vabLNP
111600070807     C                   EVAL      vaxNRS = WRK_vabNRS
111700070807     C                   EVAL      vaxNSP = WRK_vabNSP
111800070807     C* Auto-incremento numero progressivo riga packing list bolla master + bolla slave corrente
111900070905     C***                EVAL      vaxPRR = vaxPRR + wVAXmaster + wInsRighe
112000070905     C                   EVAL      vaxPRR = vaxPRR + wVAXmaster
112100070807     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
112200070807     C   60              EVAL      vaxNSP = ACC_NSP
112300070807     C* Inserisco record d dettaglio in FIVAX
112400070807     C                   WRITE     FIVAX000
112500070807     C* Elimino record d dettaglio in EDIVAX
112600070807     C                   DELETE    EDIVAXQT1L
112700070807     C* Continuo la lettura dei dettagli della testata bolla corrente
112800070807     C     KEYva_1       READE     EDIVAXQT1L
112900070807     C                   ENDDO
113000070807     C                   ENDIF
113100070807     C*
113200070809     C                   ENDIF
113300070809     C*
113400070807     C/EXEC SQL
113500070905     C+ close C1VX
113600070807     C/END-EXEC
113700070905     C*
113800070905     C/EXEC SQL
113900070905     C+ close C2VX
114000070905     C/END-EXEC
114100070905     C*
114200070809     C                   ENDIF
114300070807     C*
114400070807     C                   ENDSR
114500070809     C*------------------------------------------------------------------------*
114600070809     C* GESFORMFEED - GESTIONE FORZATURA SALTO PAGINA FITTIZIO
114700070809     C*------------------------------------------------------------------------*
114800070809     C     GESFORMFEED   BEGSR
114900070809     C*
115000070809     C* Salvo il formato record corrente
115100070809     C                   EVAL      DSEDIVAXSAV = DSEDIVAX
115200070809     C*
115300070809     C* Effettuo considerazioni x simulazione forzata salto pagina (ma solo se conviene davvero)
115400070809     C                   Z-ADD     *zeros        wRigheVuote       2 0
115500070809     C                   EVAL      wRigheVuote = wVAXmaster -
115600070809     C                             (42 * %div(wVAXmaster:42))
115700070809     C                   IF        wRigheVuote > *zeros
115800070809     C                   EVAL      wRigheVuote =
115900070809     C                             (42 * (%div(wVAXmaster:42)+1)) -
116000070809     C                             wVAXmaster
116100070809     C                   IF        wRigheVuote < 21
116200070809     C                   Z-ADD     wRigheVuote   wInsRighe         2 0
116300070809     C                   ELSE
116400070809     C                   Z-ADD     3             wInsRighe
116500070809     C                   ENDIF
116600070809     C*
116700070809     C                   Z-ADD     1             wRigaVuota        2 0
116800070809     C                   DOW       wRigaVuota <= wInsRighe
116900070809     C*
117000070809     C* Inserisco riga vuota sulla bolla master (accorpante) fino a capienza ultima pagina
117100070809     C                   EVAL      vaxAAS = WRK_vabAAS
117200070809     C                   EVAL      vaxLNP = WRK_vabLNP
117300070809     C                   EVAL      vaxNRS = WRK_vabNRS
117400070809     C                   EVAL      vaxNSP = WRK_vabNSP
117500070809     C                   EVAL      vaxPRR = wVAXmaster + wRigaVuota
117600070809     C                   EVAL      vaxDTT = *blanks
117700070809     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
117800070809     C   60              EVAL      vaxNSP = ACC_NSP
117900070809     C* Inserisco record d dettaglio in FIVAX
118000070809     C                   WRITE     FIVAX000
118100070809     C*
118200070809     C                   ADD       1             wRigaVuota
118300070809     C                   ENDDO
118400070809     C                   ENDIF
118500070809     C*
118600070809     C* Ripristino il formato record corrente
118700070809     C                   EVAL      DSEDIVAX = DSEDIVAXSAV
118800070809     C*
118900070809     C                   ENDSR
119000031112     C*------------------------------------------------------------------------*
119100031112     C* WRIVAB - SCRITTIURA FILE FIVAB X BOLLA "MASTER" ACCORPATA
119200031112     C*------------------------------------------------------------------------*
119300031112     C     WRIVAB        BEGSR
119400031112     C*
119500031112     C* Innanzitutto svuoto il buffer del record d testata d output (FIVAB)
119600031112     C                   CLEAR                   FIVAB000
119700031112     C* Valorizzo il buffer del record d testata d output (FIVAB)
119800031112     C                   EVAL      DSEDIVABOUT = DSEDIVABW
119900070420     C*
120000070420     C* Procedo solo se buffer effettivamente valorizzato
120100070420     C                   IF        DSEDIVABOUT <> DSEDIVABINZ
120200070420     C*
120300031112     C* Resetto alcuni campi che nella bolla "master" occcorrono NN valorizzati
120400031113     C   55              CLEAR                   vabNCD
120500031113     C   55              CLEAR                   vabNCA
120600031114     C   55              CLEAR                   vabRMO
120700031114     C   55              CLEAR                   vabCMO
120800031114     C   55              CLEAR                   vabNMO
120900050620     C                   IF        *IN55 = *on
121000050620     C* Campo RIFERIMENTO MITTENTE
121100050701     C                   IF        wRMO <> *blanks
121200060530     C                   Z-ADD     wGiro         wGiro2
121300060530     C                   EVAL      vabRMO = 'N'+%editw(wGiro2:'  ')+':'+
121400040303     C                                      %trim(wRMO)
121500050610     C                   IF        wGiro > 1 AND wOltreRIF = 'S'
121600040514     C                   MOVE      '...'         vabRMO
121700040303     C                   ENDIF
121800050701     C                   ENDIF
121900050620     C* Campi NOTE
122000060726     C   51              EVAL      vabNOT = %trim(wNOT)
122100060726     C   51              EVAL      vabNT2 = %trim(wNT2)
122200040303     C                   ENDIF
122300070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
122400070227     C   60              EVAL      vabNSP = ACC_NSP
122500031112     C* Inserisco record d testata in FIVAB
122600031112     C                   WRITE     FIVAB000
122700070420     C                   ENDIF
122800050620     C*
122900050620     C* Inizializzo indicatori d wrk
123000050701     C                   SETOFF                                       51
123100031112     C*
123200031112     C                   ENDSR
123300070227     C*------------------------------------------------------------------------*
123400040709     C* CHKREC - CONTROLLO CORRETTEZZA RECORD TESTATA BOLLE AI FINI DEL RAGGRUPPAMENTO
123500040709     C*------------------------------------------------------------------------*
123600040709     C     CHKREC        BEGSR
123700040709     C*
123800040709     C* Inizializzo il flag d correttezza record testata bolle
123900040709     C                   MOVEL     'S'           wRECOK            1
124000051215     C*
124100051215     C* Verifica se bolla corrente presenta particolarità consegna RC e cliente nn abilitato
124200051215     C* specificatamente all'accorpamento anche con part. consegna RC
124300051215     C                   IF        wRECOK = 'S'
124400051216     C                   IF        SQL_vabGMA = 'RC'
124500051216     C                   IF        wAccRC <> 'S'
124600051215     C                   MOVEL     'N'           wRECOK
124700051216     C                   ELSE
124800051216     C* Se abilitazione ok controllo subito anche l'ORM relativo (quello che poi sarà aggiornato)
124900051216     C                   CLEAR                   wRC_KEY
125000051216     C                   EVAL      wRC_AAS = SQL_vabAAS
125100051216     C                   EVAL      wRC_LNP = SQL_vabLNP
125200051216     C                   EVAL      wRC_NRS = SQL_vabNRS
125300051216     C                   EVAL      wRC_NSP = SQL_vabNSP
125400051216     C                   CLEAR                   FNLS24DS
125500051216     C                   EVAL      I24TLA  = *blanks
125600051216     C                   EVAL      I24OPE  = 'L'
125700051216     C                   EVAL      I24KSC  = SQL_vabCCM
125800051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
125900051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
126000051216     C                   CALL      'FNLS24R'
126100051216     C                   PARM                    FNLS24DS
126200051216     C* Se tutto ok procedo altrimenti skippo la bolla corrente con particolarità RC ai fini
126300051216     C* dell'accorpamento
126400051216     C                   IF        O24ERR <> *zeros
126500051216     C                   MOVEL     'N'           wRECOK
126600051216     C                   ENDIF
126700051215     C                   ENDIF
126800051216     C                   ENDIF
126900051215     C                   ENDIF
127000040709     C*
127100040709     C* Verifica codice trattamento
127200051215     C                   IF        wRECOK = 'S'
127300040709     C                   EXSR      CHKCTM
127400051215     C                   ENDIF
127500040709     C*
127600040709     C                   ENDSR
127700040709     C*------------------------------------------------------------------------*
127800040709     C* CHKCTM - CONTROLLO CODICE TRATTAMENTO MERCE
127900040709     C*------------------------------------------------------------------------*
128000040709     C     CHKCTM        BEGSR
128100040709     C*
128200040709     C* Verifico che il codice trattamento merce della bolla corrente nn preveda il VAX
128300040709     C                   EVAL      tblKUT = 1
128400040709     C                   EVAL      tblCOD = '1B'
128500040709     C                   EVAL      tblKEY = SQL_vabCTM
128600040709     C     KEYtab        CHAIN     TABEL00F
128700040709     C                   IF        %found(TABEL00F)
128800040709     C                   EVAL      DS1B = tblUNI
128900070809     C                   IF        §1BF16 = 'S' AND wAccVX <> 'S'
129000040709     C                   MOVEL     'N'           wRECOK
129100040709     C                   ENDIF
129200040709     C                   ENDIF
129300040709     C*
129400040709     C                   ENDSR
129500040227     C*------------------------------------------------------------------------*
129600040227     C* CHGCTM - CAMBIO CODICE TRATTAMENTO MERCE
129700040227     C*------------------------------------------------------------------------*
129800040227     C     CHGCTM        BEGSR
129900040227     C*
130000040227     C* Se sulla testata bolle NN è espressamente indicato il codice tratamento merce
130100040227     C* (e naturalmente o c'è il dettaglio colli o l'indicazione in testata dei segnacolli
130200040227     C* sequenziali) imposto come default il CTM = '2D'
130300040227     C                   IF        WRK_vabCTM = *blanks
130400040227     C                   EVAL      WRK_vabCTM = '2D'
130500040227     C                   ENDIF
130600040227     C*
130700040227     C* Valorizzo la chiave x reperire le informazioni relative al codice
130800040227     C* trattamento merce da impostare in caso d raggruppamento bolle
130900040227     C                   EVAL      tblKUT = 1
131000040227     C                   EVAL      tblCOD = '1B'
131100040227     C                   EVAL      tblKEY = WRK_vabCTM
131200040227     C     KEYtab        CHAIN     TABEL00F
131300040227     C                   IF        %found(TABEL00F)
131400040227     C                   EVAL      DS1B = tblUNI
131500040227     C                   IF        §1BCTR <> *blanks
131600040227     C                   EVAL      WRK_vabCTM = §1BCTR
131700040227     C                   ENDIF
131800040227     C                   ENDIF
131900040227     C*
132000040227     C                   ENDSR
132100050610     C*------------------------------------------------------------------------*
132200051021     C* REPABD - REPERISCE I DATI TABELLATI CHE PILOTANO LE MODALITA DI ACCORPAMENTO
132300050610     C*------------------------------------------------------------------------*
132400050610     C     REPABD        BEGSR
132500050610     C*
132600050610     C                   EVAL      tblKUT = 1
132700050610     C                   EVAL      tblCOD = '3C'
132800050708     C                   MOVEL(P)  D86CCM        tblKEY
132900050610     C     KEYtab        CHAIN     TABEL00F
133000050610     C                   IF        %found(TABEL00F)
133100050610     C                   EVAL      DS3C = tblUNI
133200050610     C                   MOVEL     §3CABD        wTipABD
133300050610     C                   Z-ADD     §3CLRM        wLenRIF
133400050610     C                   MOVEL     §3CALM        wAlmRIF
133500050620     C                   MOVEL     §3CNOT        wRgrNOT
133600051021     C                   MOVEL     §3CABC        wAccCAS
133700051215     C                   MOVEL     §3CARC        wAccRC
133800070227     C                   MOVEL     §3CFSP        wSpedCli
133900070809     C                   MOVEL     §3CAVX        wAccVX
134000050610     C                   ENDIF
134100070228     C*
134200070228     C* Se cliente che necessita ritorno informazioni d accorpamento => accendo indicatore preposto
134300070228     C                   if        wTipABD <> *blanks AND
134400070228     C                             wSpedCli = 'S'
134500070228     C                   seton                                        60
134600070228     C                   else
134700070228     C                   setoff                                       60
134800070228     C                   endif
134900050610     C*
135000050610     C                   ENDSR
135100070227     C*------------------------------------------------------------------------*
135200070419     C* REPNUM - STACCA NUMERATORE X MANTENIMENTO NUMERO SPEDIZIONE
135300070227     C*------------------------------------------------------------------------*
135400070227     C     REPNUM        BEGSR
135500070227     C*
135600070419     C* Numero spedizione (da Flnuf cod. 3)
135700070419     C                   clear                   Trul34ds
135800070419     C                   eval      i34aas = SQL_VABAAS
135900070419     C                   eval      i34lnp = SQL_VABLNP
136000070419     C                   call      'TRUL34R2'
136100070419     C                   parm                    kpjba
136200070419     C                   parm                    Trul34ds
136300070419     C                   if        o34err = *Zeros
136400070419     C                   z-add     o34nsp        ACC_NSP
136500070419     C                   else
136600070420     C                   z-add     SQL_VABNSP    ACC_NSP
136700070419     C                   endif
136800070227     C*
136900070227     C                   ENDSR
137000070227     C*------------------------------------------------------------------------*
137100070227     C* WRIRAB - SCRITTURA FILE RACCORDO BOLLE CLIENTE "ACCORPANTI"/"ACCORPATE"
137200070227     C*------------------------------------------------------------------------*
137300070227     C     WRIRAB        BEGSR
137400070227     C*
137500070227     C* Chiave spedizione
137600070227     C                   eval      rabAAS  = SQL_vabAAS
137700070227     C                   eval      rabLNP  = SQL_vabLNP
137800070227     C                   eval      rabNRS  = SQL_vabNRS
137900070227     C                   eval      rabNSP  = ACC_NSP
138000070227     C                   eval      rabCCM  = SQL_vabCCM
138100070227     C* Riferimenti cliente "accorpanti"
138200070227     C                   eval      rabRMN  = WRK_vabRMN
138300070227     C                   eval      rabRMA  = WRK_vabRMA
138400070227     C* Riferimenti cliente "accorpati"
138500070227     C                   eval      rabRMNR = SQL_vabRMN
138600070227     C                   eval      rabRMAR = SQL_vabRMA
138700070228     C* Attributi
138800070228     C                   eval      rabDAT  = datcor
138900070227     C*
139000070301     C                   write     FIRAB000
139100070227     C*
139200070227     C                   ENDSR
139300970526     C*------------------------------------------------------------------------*
139400970526     C* *INZSR - OPERAZIONI INIZIALI
139500970526     C*------------------------------------------------------------------------*
139600970526     C     *INZSR        BEGSR
139700040714     C*
139800040714     C*  Reperisco parametri in entrata
139900040714     C     *ENTRY        PLIST
140000070420     C                   PARM                    KPJBA
140100040714     C                   PARM                    TRTC86DS
140200070419     C*
140300070419     C                   SETOFF                                       60
140400060726     C*
140500031113     C*
140600031110     C*---
140700031110     C* CHIAVI DI LETTURA
140800031110     C*---
140900040709     C* CHIAVE DI LETTURA X EDIVABQT1L - Completa
141000040709     C     KEYvab1       KLIST
141100040709     C                   KFLD                    SQL_vabCCM
141200040709     C                   KFLD                    SQL_vabCMR
141300040709     C                   KFLD                    SQL_vabCNT
141400040709     C                   KFLD                    SQL_vabAAS
141500040709     C                   KFLD                    SQL_vabLNP
141600040709     C                   KFLD                    SQL_vabNRS
141700040709     C                   KFLD                    SQL_vabNSP
141800070807     C* CHIAVE DI LETTURA X EDIVADQT1L/EDIVATQT1L/EDIVAXQT1L
141900040712     C     KEYva_1       KLIST
142000031112     C                   KFLD                    SQL_vabFGS
142100031112     C                   KFLD                    SQL_vabCCM
142200031112     C                   KFLD                    SQL_vabCMR
142300031112     C                   KFLD                    SQL_vabCNT
142400031112     C                   KFLD                    SQL_vabAAS
142500031112     C                   KFLD                    SQL_vabLNP
142600031112     C                   KFLD                    SQL_vabNRS
142700031112     C                   KFLD                    SQL_vabNSP
142800040227     C* CHIAVE DI LETTURA X TABEL00F - Completa
142900040227     C     KEYtab        KLIST
143000040227     C                   KFLD                    tblKUT
143100040227     C                   KFLD                    tblCOD
143200040227     C                   KFLD                    tblKEY
143300070807     C** CHIAVE DI LETTURA X EDIVAX2L - Completa
143400070807     C*     KEYvax2_C     KLIST
143500070807     C*                   KFLD                    D86CCM
143600070807     C*                   KFLD                    D86CMR
143700070807     C*                   KFLD                    D86CNT
143800070228     C*
143900041116     C*  Richiamo XPARUT
144000041116     C                   Z-ADD     1             CODUT             1 0
144100041116     C                   CALL      'X§PARUT'
144200041116     C                   PARM                    UT§DSE
144300041116     C                   MOVEL     REC80         CNCR80
144400041116     c
144500041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
144600041116     C                   CLEAR                   TRUL06DS
144700041116     C                   MOVE      '£1'          D06COD
144800041116     C                   MOVEL     SIMFEL        D06KEY
144900041116     C                   MOVEL     'L'           D06TLA
145000041116     C                   MOVEL     TRUL06DS      KPJBU
145100041116      *
145200041116     C                   CALL      'TRUL06R'
145300041116     C                   PARM                    KPJBA
145400041116     C                   MOVEL     KPJBU         TRUL06DS
145500070228     C*
145600070228     C* CALCOLA LA DATA CORRENTE
145700070228     C                   time                    wn14             14 0
145800070228     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
145900070228     C                   z-add     wn8           g08dat
146000070228     C                   z-add     *zeros        g08inv
146100070228     C                   movel     '0'           g08err
146200070228     C                   call      'XSRDA8'
146300070228     C                   parm                    wlbda8
146400070228     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
146500991112     C*
146600991112     C                   ENDSR
