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
014000050708     C*
014100050708     C* Reperisce quale riferimenti mittente mantenere durante l'accorpamento
014200050708     C                   exsr      repABD
014300031110     C*
014400031110     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
014500031110     C                   exsr      exeABD
014600051216     C*
014700051216     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
014800051216     C* x gestione discorso particolarità consegna 'RC'
014900051216     C                   exsr      exeABDRC
015000031110     C*
015100031110     C* Eseguo routine d scrittura del FIVAB/FIVAD x bolle NN raggruppabili
015200031110     C                   exsr      exeNOABD
015300070419     C*
015400070419     C* Eseguo chiusure finali
015500070419     C* - TRUL34R2
015600070419     C                   clear                   Trul34ds
015700070419     C                   eval      i34tla = 'C'
015800070419     C                   call      'TRUL34R2'
015900070419     C                   parm                    kpjba
016000070419     C                   parm                    Trul34ds
016100991112     C*
016200970526     C* FINE PGM
016300031110     C                   seton                                        LR
016400991112     C*------------------------------------------------------------------------*
016500031110     C* EXEABD - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
016600991112     C*------------------------------------------------------------------------*
016700031110     C     EXEABD        BEGSR
016800050620     C*
016900050620     C* Inizializzo indicatori d wrk
017000050620     C                   setoff                                       51
017100051216     C                   setoff                                       52
017200040709     C*
017300040712     C* Innanzitutto apro i file EDI*QT temporanei
017400040709     C                   open      EDIVABQT1L
017500040712     C                   open      EDIVADQT1L
017600040712     C                   open      EDIVATQT1L
017700070807     C                   open      EDIVAXQT1L
017800031110     C*
017900031110     C* Inizializzazioni
018000031110     C                   clear                   DSEDIVAB
018100031110     C                   clear                   DSEDIABD
018200031110     C                   clear                   SAVEDIABD
018300031110     C                   exsr      exeINZ
018400031114     C                   eval      wGiro = *zeros
018500031113     C                   seton                                        55
018600000707     C*
018700000707     C/EXEC SQL
018800051216     C+ declare C1 cursor for select * from edivabqt
018900051021     C*+ where vabCAS = 0 and vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
019000051215     C*+ where vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
019100051216     C*+ where vabATB = ' ' and vabxco=' '
019200051216     C+ where vabGMA <> 'RC' and vabGMA <> '  ' and vabATB = ' ' and vabxco=' '
019300031114     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
019400031110     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
019500031110     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
019600031110     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
019700051021     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
019800000707     C+ for fetch only
019900000707     C/END-EXEC
020000000707     C
020100000707     C/EXEC SQL
020200051216     C+ open C1
020300000707     C/END-EXEC
020400000707     C
020500000707     C/EXEC SQL
020600051216     C+ Fetch C1 into :DSEDIVAB
020700000707     C/END-EXEC
020800000707     C*
020900031110     C                   dow       sqlcod = *zeros
021000040709     C*
021100040709     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
021200051021     C                   exsr      CHKREC
021300040709     C*
021400040709     C* Se ok proseguo
021500040709     C                   if        wRECOK = 'S'
021600031110     C*
021700031110     C* Eseguo routine d valorizzazione DS d livello rottura
021800031110     C                   exsr      valDSROT
021900051216     C*
022000051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
022100051216     C                   z-add     *zeros        wFlgErr           1 0
022200051021     C*
022300051215     C* Verifico avvenuta rottura d codice
022400051021     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
022500051021     C                             (DSEDIABD=SAVEDIABD     AND
022600051021     C                              SQL_vabCAS>*zeros      AND
022700051021     C                             (SQL_vabVCA=SAV_vabVCA  AND
022800051021     C                              SQL_vabTIC=SAV_vabTIC   OR
022900051021     C                              SAV_vabCAS=*zeros)     AND
023000051021     C                              wAccCAS='S')
023100051021     C*
023200051021     C* ****************  ESEGUO ACCORPAMENTO **********************
023300051216     C*
023400051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
023500051216     C                   exsr      exeRC
023600051216     C*
023700051216     C* Se fin qui tutto ok => proseguo
023800051216     C                   if        wFlgErr = *zeros
023900051021     C*
024000051021     C* Eseguo la sommatoria dei campi da raggruppare
024100051216     C                   exsr      exeSOMMA
024200051021     C*
024300051021     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
024400051021     C                   exsr      exeNORM
024500070227     C*
024600070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
024700070227     C   60              exsr      wriRAB
024800051021     C*
024900051021     C* Effettuo scrittura del file FIVAD
025000051021     C                   exsr      wriVAD
025100051021     C*
025200051021     C* Effettuo scrittura del file FIVAT
025300051021     C                   exsr      wriVAT
025400070807     C*
025500070807     C* Effettuo scrittura del file FIVAX
025600070807     C                   exsr      wriVAX
025700051216     C*
025800051216     C                   endif
025900051021     C*
026000070228     C* ****************  NUOVA ROTTURA ************************
026100051021     C                   else
026200051021     C*
026300051021     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
026400051021     C* dell'ultima bolla accorpata
026500051021     C                   if        wGiro > 0
026600051021     C                   exsr      wriVAB
026700051021     C                   eval      wGiro = *zeros
026800051021     C                   endif
026900051021     C*
027000051021     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
027100051021     C                   eval      SAVEDIABD  = DSEDIABD
027200051021     C*
027300051021     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
027400051021     C                   exsr      exeINZ
027500051021     C*
027600051021     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
027700070227     C   60              exsr      repnum
027800051021     C                   exsr      impVAB
027900070227     C*
028000070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
028100070227     C   60              exsr      wriRAB
028200051021     C*
028300051021     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
028400051021     C                   exsr      wriVAD
028500051021     C*
028600051021     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
028700051021     C                   exsr      wriVAT
028800070807     C*
028900070807     C* Effettuo scrittura del file FIVAX x dettaglio bolla "master"
029000070807     C                   exsr      wriVAX
029100031110     C*
029200031110     C                   endif
029300031110     C*
029400031110     C* Incremento il contatore che identifica il 1° giro
029500031114     C                   eval      wGiro = wGiro + 1
029600051102     C*
029700051102     C* Salvo i nuovi attributi relativi al contrassegno
029800051102     C                   eval      SAV_vabCAS = SQL_vabCAS
029900051102     C                   eval      SAV_vabVCA = SQL_vabVCA
030000051102     C                   eval      SAV_vabTIC = SQL_vabTIC
030100051216     C*
030200051216     C* Come ultima cosa deleto il record bolla testata appena considerato
030300051216     C* ...solo se tutto ok => proseguo
030400051216     C                   if        wFlgErr = *zeros
030500040712     C     KEYvab1       delete    EDIVABQT1L
030600051216     C                   endif
030700040709     C*
030800040709     C                   endif
030900031110     C*
031000031110     C/EXEC SQL
031100051216     C+ Fetch C1 into :DSEDIVAB
031200031110     C/END-EXEC
031300031110     C                   enddo
031400031110     C*
031500000707     C/EXEC SQL
031600051216     C+ close C1
031700000707     C/END-EXEC
031800031114     C*
031900031114     C* Scarico la bolla "master" in sospeso
032000031114     C* dell'ultima bolla accorpata
032100031114     C                   exsr      wriVAB
032200040709     C*
032300040712     C* Al termine chiudo i file EDI*QT temporanei
032400040709     C                   close     EDIVABQT1L
032500040712     C                   close     EDIVADQT1L
032600040712     C                   close     EDIVATQT1L
032700070807     C                   close     EDIVAXQT1L
032800991112     C*
032900991112     C                   ENDSR
033000051216     C*------------------------------------------------------------------------*
033100051216     C* EXEABDRC - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
033200051216     C*------------------------------------------------------------------------*
033300051216     C     EXEABDRC      BEGSR
033400051216     C*
033500051216     C* Inizializzo indicatori d wrk
033600051216     C                   setoff                                       51
033700051216     C                   seton                                        52
033800051216     C*
033900051216     C* Innanzitutto apro i file EDI*QT temporanei
034000051216     C                   open      EDIVABQT1L
034100051216     C                   open      EDIVADQT1L
034200051216     C                   open      EDIVATQT1L
034300070807     C                   open      EDIVAXQT1L
034400051216     C*
034500051216     C* Inizializzazioni
034600051216     C                   clear                   DSEDIVAB
034700051216     C                   clear                   DSEDIABD
034800051216     C                   clear                   SAVEDIABD
034900051216     C                   exsr      exeINZ
035000051216     C                   eval      wGiro = *zeros
035100051216     C                   seton                                        55
035200051216     C*
035300051216     C/EXEC SQL
035400051216     C+ declare C2 cursor for select * from edivabqt
035500051216     C*+ where vabCAS = 0 and vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
035600051216     C*+ where vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
035700051216     C*+ where vabATB = ' ' and vabxco=' '
035800051216     C+ where (vabGMA = 'RC' or vabGMA = '  ') and vabATB = ' ' and vabxco=' '
035900051216     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
036000051216     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
036100051216     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
036200051216     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
036300051216     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
036400051216     C+ for fetch only
036500051216     C/END-EXEC
036600051216     C
036700051216     C/EXEC SQL
036800051216     C+ open C2
036900051216     C/END-EXEC
037000051216     C
037100051216     C/EXEC SQL
037200051216     C+ Fetch C2 into :DSEDIVAB
037300051216     C/END-EXEC
037400051216     C*
037500051216     C                   dow       sqlcod = *zeros
037600051216     C*
037700051216     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
037800051216     C                   exsr      CHKREC
037900051216     C*
038000051216     C* Se ok proseguo
038100051216     C                   if        wRECOK = 'S'
038200051216     C*
038300051216     C* Eseguo routine d valorizzazione DS d livello rottura
038400051216     C                   exsr      valDSROT
038500051216     C*
038600051216     C* Inizializzo flag d eventuale errore in fase d sommatoria dati bolle
038700051216     C                   z-add     *zeros        wFlgErr           1 0
038800051216     C*
038900051216     C* Verifico avvenuta rottura d codice
039000051216     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
039100051216     C                             (DSEDIABD=SAVEDIABD     AND
039200051216     C                              SQL_vabCAS>*zeros      AND
039300051216     C                             (SQL_vabVCA=SAV_vabVCA  AND
039400051216     C                              SQL_vabTIC=SAV_vabTIC   OR
039500051216     C                              SAV_vabCAS=*zeros)     AND
039600051216     C                              wAccCAS='S')
039700051216     C*
039800051216     C* ****************  ESEGUO ACCORPAMENTO **********************
039900051216     C*
040000051216     C* Eseguo l'aggiornamento dell'ORM in caso d bolla con particolarità 'RC'
040100051216     C                   exsr      exeRC
040200051216     C*
040300051216     C* Se fin qui tutto ok => proseguo
040400051216     C                   if        wFlgErr = *zeros
040500051216     C*
040600051216     C* Eseguo la sommatoria dei campi da raggruppare
040700051216     C                   exsr      exeSOMMA
040800051216     C*
040900051216     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
041000051216     C                   exsr      exeNORM
041100070227     C*
041200070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
041300070227     C   60              exsr      wriRAB
041400051216     C*
041500051216     C* Effettuo scrittura del file FIVAD
041600051216     C                   exsr      wriVAD
041700051216     C*
041800051216     C* Effettuo scrittura del file FIVAT
041900051216     C                   exsr      wriVAT
042000070807     C*
042100070807     C* Effettuo scrittura del file FIVAX
042200070807     C                   exsr      wriVAX
042300051216     C*
042400051216     C                   endif
042500051216     C*
042600070228     C* ****************  NUOVA ROTTURA ************************
042700051216     C                   else
042800051216     C*
042900051216     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
043000051216     C* dell'ultima bolla accorpata
043100051216     C                   if        wGiro > 0
043200051216     C                   exsr      wriVAB
043300051216     C                   eval      wGiro = *zeros
043400051216     C                   endif
043500051216     C*
043600051216     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
043700051216     C                   eval      SAVEDIABD  = DSEDIABD
043800051216     C*
043900051216     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
044000051216     C                   exsr      exeINZ
044100051216     C*
044200051216     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
044300070227     C   60              exsr      repnum
044400070227     C                   exsr      impVAB
044500070227     C*
044600070227     C* Scrivo il record d raccordo tra bolla "accorpante" e bolle "accorpate"
044700070227     C   60              exsr      wriRAB
044800051216     C*
044900051216     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
045000051216     C                   exsr      wriVAD
045100051216     C*
045200051216     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
045300051216     C                   exsr      wriVAT
045400070807     C*
045500070807     C* Effettuo scrittura del file FIVAX x dettaglio bolla "master"
045600070807     C                   exsr      wriVAX
045700051216     C*
045800051216     C                   endif
045900051216     C*
046000051216     C* Incremento il contatore che identifica il 1° giro
046100051216     C                   eval      wGiro = wGiro + 1
046200051216     C*
046300051216     C* Salvo i nuovi attributi relativi al contrassegno
046400051216     C                   eval      SAV_vabCAS = SQL_vabCAS
046500051216     C                   eval      SAV_vabVCA = SQL_vabVCA
046600051216     C                   eval      SAV_vabTIC = SQL_vabTIC
046700051216     C*
046800051216     C* Come ultima cosa deleto il record bolla testata appena considerato
046900051216     C* ...solo se tutto ok => proseguo
047000051216     C                   if        wFlgErr = *zeros
047100051216     C     KEYvab1       delete    EDIVABQT1L
047200051216     C                   endif
047300051216     C*
047400051216     C                   endif
047500051216     C*
047600051216     C/EXEC SQL
047700051216     C+ Fetch C2 into :DSEDIVAB
047800051216     C/END-EXEC
047900051216     C                   enddo
048000051216     C*
048100051216     C/EXEC SQL
048200051216     C+ close C2
048300051216     C/END-EXEC
048400051216     C*
048500051216     C* Scarico la bolla "master" in sospeso
048600051216     C* dell'ultima bolla accorpata
048700051216     C                   exsr      wriVAB
048800051216     C*
048900051216     C* Al termine chiudo i file EDI*QT temporanei
049000051216     C                   close     EDIVABQT1L
049100051216     C                   close     EDIVADQT1L
049200051216     C                   close     EDIVATQT1L
049300070807     C                   close     EDIVAXQT1L
049400051216     C*
049500051216     C                   ENDSR
049600031110     C*------------------------------------------------------------------------*
049700040712     C* EXENOABD - SCRITTIURA FILE FIVAB/FIVAD X BOLLE NN RAGGRUPPABILI
049800031110     C*------------------------------------------------------------------------*
049900031110     C     EXENOABD      BEGSR
050000050620     C*
050100050620     C* Inizializzo indicatori d wrk
050200050620     C                   setoff                                       51
050300040712     C*
050400040712     C* Innanzitutto apro i file EDI*QT temporanei
050500040712     C                   open      EDIVABQT1L
050600040712     C                   open      EDIVADQT1L
050700040712     C                   open      EDIVATQT1L
050800070807     C                   open      EDIVAXQT1L
050900040712     C*
051000040712     C* Porto tutto ciò che è rimasto nei file EDI*QT nei relativi FIVA*
051100040712     C*
051200040712     C* ...VAB
051300040712     C     *loval        setll     EDIVABQT1L
051400040712     C                   read      EDIVABQT1L
051500040712     C                   dow       not %eof(EDIVABQT1L)
051600040712     C                   write     FIVAB000
051700040712     C                   delete    EDIVABQT1L
051800040712     C                   read      EDIVABQT1L
051900040712     C                   enddo
052000040712     C*
052100040712     C* ...VAD
052200040712     C     *loval        setll     EDIVADQT1L
052300040712     C                   read      EDIVADQT1L
052400040712     C                   dow       not %eof(EDIVADQT1L)
052500040712     C                   write     FIVAD000
052600040712     C                   delete    EDIVADQT1L
052700040712     C                   read      EDIVADQT1L
052800040712     C                   enddo
052900040712     C*
053000040712     C* ...VAT
053100040712     C     *loval        setll     EDIVATQT1L
053200040712     C                   read      EDIVATQT1L
053300040712     C                   dow       not %eof(EDIVATQT1L)
053400040712     C                   write     FIVAT000
053500040712     C                   delete    EDIVATQT1L
053600040712     C                   read      EDIVATQT1L
053700040712     C                   enddo
053800070807     C*
053900070807     C* ...VAX
054000070807     C     *loval        setll     EDIVAXQT1L
054100070807     C                   read      EDIVAXQT1L
054200070807     C                   dow       not %eof(EDIVAXQT1L)
054300070807     C                   write     FIVAX000
054400070807     C                   delete    EDIVAXQT1L
054500070807     C                   read      EDIVAXQT1L
054600070807     C                   enddo
054700040714     C*
054800070807     C** ...VAX
054900070807     C*     KEYvax2_C     setll     EDIVAX2L
055000070807     C*     KEYvax2_C     reade     EDIVAX2L
055100070807     C*                   dow       not %eof(EDIVAX2L)
055200070807     c*     vaxfgs        lookup    lin                                    50
055300070807     c*                   if        *in50  and vaxfgs>0
055400070807     C*                   write     FIVAX000
055500070807     C*                   delete    EDIVAX2L
055600070807     c*                   endif
055700070807     c*
055800070807     C*     KEYvax2_C     reade     EDIVAX2L
055900070807     C*                   enddo
056000040712     C*
056100040714     C* Al termine chiudo i file EDI* temporanei
056200040712     C                   close     EDIVABQT1L
056300040712     C                   close     EDIVADQT1L
056400040712     C                   close     EDIVATQT1L
056500070807     C                   close     EDIVAXQT1L
056600031110     C*
056700031110     C                   ENDSR
056800031110     C*------------------------------------------------------------------------*
056900031110     C* EXEINZ - ROUTINE D INIZIALIZZAZIONE X OGNI NUOVO LIVELLO D ROTTURA BOLLE
057000031110     C*------------------------------------------------------------------------*
057100031110     C     EXEINZ        BEGSR
057200031110     C*
057300031110     C                   clear                   DSEDIVABW
057400031114     C                   clear                   wRMO
057500050620     C                   clear                   wNOT
057600050620     C                   clear                   wNT2
057700051215     C                   eval      wFlgOrmRC = *off
057800031110     C*
057900031110     C                   ENDSR
058000031110     C*------------------------------------------------------------------------*
058100031110     C* VALDSROT - VALORIZZAZIONE DS LIVELLO ROTTURA
058200031110     C*------------------------------------------------------------------------*
058300031112     C     VALDSROT      BEGSR
058400031110     C*
058500031110     C* Campi in rottura
058600031114     C                   EVAL      S_vabCCM = SQL_vabCCM
058700031110     C                   EVAL      S_vabRSD = SQL_vabRSD
058800031110     C                   EVAL      S_vabRD2 = SQL_vabRD2
058900031110     C                   EVAL      S_vabIND = SQL_vabIND
059000031110     C                   EVAL      S_vabCAD = SQL_vabCAD
059100031110     C                   EVAL      S_vabLOD = SQL_vabLOD
059200031110     C                   EVAL      S_vabPRD = SQL_vabPRD
059300031110     C                   EVAL      S_vabNZD = SQL_vabNZD
059400031110     C                   EVAL      S_vabLNA = SQL_vabLNA
059500031110     C                   EVAL      S_vabZNC = SQL_vabZNC
059600031110     C                   EVAL      S_vabCBO = SQL_vabCBO
059700031110     C                   EVAL      S_vabTSP = SQL_vabTSP
059800031110     C                   EVAL      S_vabCTR = SQL_vabCTR
059900031110     C                   EVAL      S_vabGC1 = SQL_vabGC1
060000031110     C                   EVAL      S_vabGC2 = SQL_vabGC2
060100031110     C                   EVAL      S_vabFFD = SQL_vabFFD
060200031110     C                   EVAL      S_vabDCR = SQL_vabDCR
060300031110     C                   EVAL      S_vabTCR = SQL_vabTCR
060400031110     C                   EVAL      S_vabHCR = SQL_vabHCR
060500051216     C  N52              EVAL      S_vabGMA = SQL_vabGMA
060600031110     C                   EVAL      S_vabGGA = SQL_vabGGA
060700031110     C                   EVAL      S_vabGVA = SQL_vabGVA
060800031110     C                   EVAL      S_vabTC1 = SQL_vabTC1
060900031110     C                   EVAL      S_vabTC2 = SQL_vabTC2
061000031110     C                   EVAL      S_vabVAS = SQL_vabVAS
061100031110     C                   EVAL      S_vabVAD = SQL_vabVAD
061200031110     C*
061300031110     C                   ENDSR
061400031110     C*------------------------------------------------------------------------*
061500031110     C* IMPVAB - VALORIZZAZIONE CAMPI "FISSI" 1° BOLLA D OGNI LIVELLO D ROTTURA
061600031110     C*------------------------------------------------------------------------*
061700031110     C     IMPVAB        BEGSR
061800031110     C*
061900031110     C* Valorizzo la DS d wrk da quella d lettura SQL
062000031110     C                   EVAL      DSEDIVABW = DSEDIVAB
062100031110     C*
062200031110     C                   ENDSR
062300051216     C*------------------------------------------------------------------------*
062400051216     C* EXERC - GESTIONE EVENTUALE PRESENZA PARTICOLARITA' CONSEGNA 'RC'
062500051216     C*------------------------------------------------------------------------*
062600051216     C     EXERC         BEGSR
062700051216     C*
062800051216     C* Gestisco anche l'accorpamento anche dell'ORM da "RC"
062900051216     C* ...ma solo se trattasi d primo "RC" delle bolle oggetto d accorpamento
063000060126     C**!!!              IF        SQL_vabGMA = 'RC' and
063100060126     C**!!!                        wFlgOrmRC = *off
063200070227 MB 1C                   IF        SQL_vabGMA = 'RC'
063300070227 MB 2c                   If        wflgormrc = *On
063400060127     C*
063500060127     C* Elimino l'ORM RC della bolla corrente (accorpata)
063600060126     c                   Clear                   Fnls25ds
063700060126     c                   Eval      i25ksc  = sql_vabccm
063800060126     c                   Move(p)   sql_vabaas    i25aas
063900060126     c                   Eval      i25spe = %editc(sql_vablnp:'X') +
064000060126     c                                      %editc(sql_vabnrs:'X') +
064100060126     c                                      %editc(sql_vabnsp:'X')
064200060126     c                   Call      'FNLS25R'
064300060126     c                   Parm                    Fnls25ds
064400060126    2C                   ENDIF
064500060126    2C                   If        wFlgOrmRC = *off
064600060127     C*
064700060127     C* Aggiorno l'orm RC legato alla bolla risultante dall'accorpamento solo se
064800060127     C* bolla corrente (accorpata) e bolla accorpante hanno chiave diversa.
064900060127     C                   IF        SQL_vabAAS <> WRK_vabAAS OR
065000060127     C                             SQL_vabLNP <> WRK_vabLNP OR
065100060127     C                             SQL_vabNRS <> WRK_vabNRS OR
065200060127     C                             SQL_vabNSP <> WRK_vabNSP
065300060127     C*
065400060127     C* Prima dell'aggiornamento del riferimento ORM RC con la nuova chiave spedizione
065500060127     C* elimino l'ORM RC della bolla accorpante
065600060127     c                   Clear                   Fnls25ds
065700060127     c                   Eval      i25ksc  = sql_vabccm
065800060127     c                   Move(p)   sql_vabaas    i25aas
065900060127     c                   Eval      i25spe = %editc(WRK_vabLNP:'X') +
066000060127     c                                      %editc(WRK_vabNRS:'X') +
066100060127     c                                      %editc(WRK_vabNSP:'X')
066200060127     c                   Call      'FNLS25R'
066300060127     c                   Parm                    Fnls25ds
066400060127     C*
066500060127     C* Quindi aggiorno l'ORM RC della bolla corrente (accorpata) con la nuova chiave
066600060127     C* spedizione (bolla accorpante)
066700051216     C                   CLEAR                   wRC_KEY
066800051216     C                   EVAL      wRC_AAS = SQL_vabAAS
066900051216     C                   EVAL      wRC_LNP = SQL_vabLNP
067000051216     C                   EVAL      wRC_NRS = SQL_vabNRS
067100051216     C                   EVAL      wRC_NSP = SQL_vabNSP
067200051220     C                   EVAL      I24TLA  = 'L'
067300051216     C                   EVAL      I24OPE  = 'U'
067400051216     C                   EVAL      I24KSC  = SQL_vabCCM
067500051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
067600051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
067700051216     C                   EVAL      I24NAAS = WRK_vabAAS
067800051216     C                   EVAL      I24LNP  = WRK_vabLNP
067900051216     C                   EVAL      I24NRS  = WRK_vabNRS
068000051216     C                   EVAL      I24NSP  = WRK_vabNSP
068100051216     C                   CALL      'FNLS24R'
068200051216     C                   PARM                    FNLS24DS
068300060127     C*
068400051216     C* Se tutto ok => accendo l'indicatore x nn aggiornare ORM generati da bolle "RC"
068500051216     C* accorpate successivamente (ovviamente x medesimo destinatario)
068600060126    3C                   IF        O24ERR = *zeros
068700051216     C                   EVAL      wFlgOrmRC = *on
068800060126   x3C                   ELSE
068900051216     C                   Z-ADD     1             wFlgErr
069000060126    3C                   ENDIF
069100060127     C                   ENDIF
069200060126    2C                   ENDIF
069300060126    1C                   ENDIF
069400051216     C*
069500051216     C                   ENDSR
069600031110     C*------------------------------------------------------------------------*
069700031110     C* EXESOMMA - SOMMATORIA CAMPI X RAGGRUPPAMENTO
069800031110     C*------------------------------------------------------------------------*
069900031110     C     EXESOMMA      BEGSR
070000031110     C*
070100051216     C* Eseguo sommatoria dati bolle
070200031110     C                   EVAL      WRK_vabIAS = WRK_vabIAS + SQL_vabIAS
070300031110     C                   EVAL      WRK_vabNCL = WRK_vabNCL + SQL_vabNCL
070400031110     C                   EVAL      WRK_vabPKB = WRK_vabPKB + SQL_vabPKB
070500031110     C                   EVAL      WRK_vabVLB = WRK_vabVLB + SQL_vabVLB
070600031110     C                   EVAL      WRK_vabQFT = WRK_vabQFT + SQL_vabQFT
070700031110     C                   EVAL      WRK_vabVMD = WRK_vabVMD + SQL_vabVMD
070800051021     C                   EVAL      WRK_vabCAS = WRK_vabCAS + SQL_vabCAS
070900031110     C*
071000031110     C                   ENDSR
071100031110     C*------------------------------------------------------------------------*
071200031110     C* EXENORM - NORMALIZZAZIONE CAMPI X OTTENIMENTO BOLLA "MASTER"
071300031110     C*------------------------------------------------------------------------*
071400031110     C     EXENORM       BEGSR
071500050620     C*
071600050620     C* Valorizzo indicatore d wrk
071700050620     C                   SETON                                        51
071800031110     C*
071900031114     C                   IF        WRK_vabNAS =  *blanks AND
072000031114     C                             SQL_vabNAS <> *blanks
072100031110     C                   EVAL      WRK_vabNAS = SQL_vabNAS
072200031110     C                   ENDIF
072300031110     C*
072400031114     C                   IF        WRK_vabCTM =  *blanks AND
072500031114     C                             SQL_vabCTM <> *blanks
072600031110     C                   EVAL      WRK_vabCTM = SQL_vabCTM
072700031110     C                   ENDIF
072800031110     C*
072900031114     C                   IF        WRK_vabCTS =  *blanks AND
073000031114     C                             SQL_vabCTS <> *blanks
073100031110     C                   EVAL      WRK_vabCTS = SQL_vabCTS
073200031110     C                   ENDIF
073300031110     C*
073400031114     C                   IF        WRK_vabFTM =  *blanks AND
073500031114     C                             SQL_vabFTM <> *blanks
073600031110     C                   EVAL      WRK_vabFTM = SQL_vabFTM
073700031110     C                   ENDIF
073800031110     C*
073900031114     C                   IF        WRK_vabSCL =  *blanks AND
074000031114     C                             SQL_vabSCL <> *blanks
074100031110     C                   EVAL      WRK_vabSCL = SQL_vabSCL
074200031110     C                   ENDIF
074300051021     C*
074400051021     C                   IF        WRK_vabVCA =  *blanks AND
074500051021     C                             SQL_vabVCA <> *blanks
074600051021     C                   EVAL      WRK_vabVCA = SQL_vabVCA
074700051021     C                   ENDIF
074800051021     C*
074900051021     C                   IF        WRK_vabTIC =  *blanks AND
075000051021     C                             SQL_vabTIC <> *blanks
075100051021     C                   EVAL      WRK_vabTIC = SQL_vabTIC
075200051021     C                   ENDIF
075300051216     C*
075400051216     C                   IF        WRK_vabGMA =  *blanks AND
075500051216     C                             SQL_vabGMA <> *blanks
075600051216     C                   EVAL      WRK_vabGMA = SQL_vabGMA
075700051216     C                   ENDIF
075800031110     C*
075900031110     C* Gestisco comportamento particolare del campo VABANT (tipo e numero bancali)
076000031110     C                   IF        WRK_vabANT = *zeros
076100031110     C                   EVAL      WRK_vabANT = SQL_vabANT
076200031110     C                   ELSE
076300031110     C                   IF        SQL_vabANT = *zeros
076400031110     C                   ELSE
076500031110     C                   EVAL      DSVABANT = SQL_vabANT
076600031110     C                   EVAL      wANTTIP1 = ANTTIP1
076700031110     C                   EVAL      wANTNUM1 = ANTNUM1
076800031110     C                   EVAL      wANTTIP2 = ANTTIP2
076900031110     C                   EVAL      wANTNUM2 = ANTNUM2
077000031110     C                   EVAL      DSVABANT = WRK_vabANT
077100031110     C                   IF        ANTTIP1 > *zeros
077200031110     C                   IF        wANTTIP1 = ANTTIP1
077300031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM1
077400031110     C                   ENDIF
077500031110     C                   IF        wANTTIP2 = ANTTIP1
077600031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM2
077700031110     C                   ENDIF
077800031110     C                   ELSE
077900031110     C                   IF        wANTTIP1 > *zeros
078000031110     C                   EVAL      ANTTIP1 = wANTTIP1
078100031110     C                   EVAL      ANTNUM1 = wANTNUM1
078200031110     C                   ELSE
078300031110     C                   IF        wANTTIP2 > *zeros
078400031110     C                   EVAL      ANTTIP1 = wANTTIP2
078500031110     C                   EVAL      ANTNUM1 = wANTNUM2
078600031110     C                   ENDIF
078700031110     C                   ENDIF
078800031110     C                   ENDIF
078900031110     C                   IF        ANTTIP2 > *zeros
079000031110     C                   IF        wANTTIP1 = ANTTIP2
079100031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM1
079200031110     C                   ENDIF
079300031110     C                   IF        wANTTIP2 = ANTTIP2
079400031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM2
079500031110     C                   ENDIF
079600031110     C                   ELSE
079700031110     C                   IF        wANTTIP1 > *zeros
079800031110     C                   EVAL      ANTTIP2 = wANTTIP1
079900031110     C                   EVAL      ANTNUM2 = wANTNUM1
080000031110     C                   ELSE
080100031110     C                   IF        wANTTIP2 > *zeros
080200031110     C                   EVAL      ANTTIP2 = wANTTIP2
080300031110     C                   EVAL      ANTNUM2 = wANTNUM2
080400031110     C                   ENDIF
080500031110     C                   ENDIF
080600031110     C                   ENDIF
080700031110     C                   EVAL      WRK_vabANT = DSVABANT
080800031110     C                   ENDIF
080900031110     C                   ENDIF
081000031114     C*
081100051021     C* Eseguo raggruppamento dei riferimenti delle testate bolle accorpate
081200051021     C                   IF        wLenRIF > *zeros
081300050906     C                   IF        wTIPABD = 'S'
081400050614     C                   EVAL      wRIF = %trim(%editc(SQL_vabRMN:'4'))
081500050610     C                   ENDIF
081600050610     C                   IF        wTIPABD = 'A'
081700050610     C                   EVAL      wRIF = %trim(SQL_vabRMA)
081800050610     C                   ENDIF
081900050610     C*
082000050610     C* Gestisco l'allineamento d default a seconda della tipologia d riferimento da utilizzare
082100050610     C                   IF        wAlmRIF = *blanks
082200050610     C                   IF        wTipABD = 'S'
082300050610     C                   EVAL      wAlmRIF = 'D'
082400050610     C                   ENDIF
082500050610     C                   IF        wTipABD = 'A'
082600050610     C                   EVAL      wAlmRIF = 'S'
082700050610     C                   ENDIF
082800050610     C                   ENDIF
082900051021     C*
083000051021     C* Effettuo le operazioni seguenti SOLO in caso in cui il riferimento voluto
083100051021     C* risulti essere effettivamente valorizzato
083200051021     C                   IF        wRIF <> *blanks
083300050614     C*
083400050614     C* Considero la minore tra la lunghezza reale del riferimento da considerare e il
083500050614     C* numero d byte da considerare indicati in tabella x il cliente corrente
083600050614     C                   Z-ADD     *zeros        wByteRIF          2 0
083700050614     C                   IF        wLenRIF <= %len(%trim(wRIF))
083800050614     C                   EVAL      wByteRIF = wLenRIF
083900050614     C                   ELSE
084000050614     C                   EVAL      wByteRIF = %len(%trim(wRIF))
084100050614     C                   ENDIF
084200050610     C*
084300050610     C* A questo punto verifico se sono richiesti solo 1 numero specifico d byte ed il
084400050610     C* relativo allineamento
084500050610     C                   IF        wLenRIF <> 99
084600050610     C                   IF        wAlmRIF = 'S'
084700050614     C                   EVAL      wRIF = %subst(%trim(wRIF):1:wByteRIF)
084800050610     C                   ENDIF
084900050610     C                   IF        wAlmRIF = 'D'
085000050610     C                   EVAL      wRIF = %subst(%trim(wRIF):
085100050614     C                                    %len(%trim(wRIF))-wByteRIF+1)
085200050610     C                   ENDIF
085300050610     C                   ENDIF
085400051021     C*
085500051021     C                   ELSE
085600051021     C                   MOVEL     'N'           wOltreRIF
085700051021     C                   ENDIF
085800050610     C*
085900031114     C* Se c stanno...
086000050610     C                   IF        %len(%trim(wRMO))+1+%len(%trim(wRIF)) <=
086100040517     C                             %len(wRMO)-4
086200050610     C                   EVAL      wRMO = %trim(wRMO) + %trim(wRIF) + '-'
086300050610     C                   MOVEL     'N'           wOltreRIF         1
086400050610     C                   ELSE
086500050610     C                   MOVEL     'S'           wOltreRIF
086600031114     C                   ENDIF
086700050906     C*
086800050906     C                   ELSE
086900050906     C                   MOVEL     'N'           wOltreRIF
087000050906     C                   ENDIF
087100051021     C*
087200050620     C*
087300050620     C* Se richiesto raggruppo anche le note
087400050620     C                   IF        wRgrNOT = 'S'
087500060726     C* Memorizzo subito note originali
087600060726     C                   IF        wNOT = *blanks AND WRK_vabNOT <> *blanks AND
087700060726     C                             %scan(%trim(WRK_vabNOT):wNT2) = 0
087800060726     C                   EVAL      wNOT = %trim(WRK_vabNOT)
087900060726     C                   ENDIF
088000060726     C                   IF        wNT2 = *blanks AND WRK_vabNT2 <> *blanks AND
088100060726     C                             %scan(%trim(WRK_vabNT2):wNOT) = 0
088200060726     C                   EVAL      wNT2 = %trim(WRK_vabNT2)
088300060726     C                   ENDIF
088400060629     C***  VABNOT
088500060726     C* Se presente nota => gestisco
088600060726     C                   IF        SQL_vabNOT <> *blanks
088700060629     C* Innanzitutto verifico che il riferimento corrente nn sia già presente
088800060726     C                   SETOFF                                       40
088900060726     C                   IF        wNOT <> *blanks
089000060726     C                   IF        %scan(%trim(SQL_vabNOT):wNOT) <> 0 OR
089100060726     C                             %scan(%trim(SQL_vabNOT):wNT2) <> 0
089200060726     C                   SETON                                        40
089300060726     C                   ENDIF
089400060726     C                   ENDIF
089500060629     C* Verifico se lo spazio su NOT è già esaurito mentre quello su NT2 è ancora dispnibile
089600060726     C                   IF        *IN40 = *OFF
089700060629     C                   IF        (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNOT))
089800060629     C                             >%size(wNOT)) AND
089900060629     C                             (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNOT))
090000060629     C                             <=%size(wNT2))
090100060726     C                   IF        %scan(%trim(SQL_vabNOT):wNT2) = 0 AND
090200060726     C                             %scan(%trim(SQL_vabNOT):wNOT) = 0
090300060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNOT)
090400060726     C                   ENDIF
090500060629     C                   ELSE
090600050708     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNOT)
090700060726     C                   ENDIF
090800060726     C                   ENDIF
090900060726     C                   ENDIF
091000060629     C***  VABNT2
091100060726     C* Se presente nota => gestisco
091200060726     C                   IF        SQL_vabNT2 <> *blanks
091300060629     C* Innanzitutto verifico che il riferimento corrente nn sia già presente
091400060726     C                   SETOFF                                       40
091500060726     C                   IF        wNT2 <> *blanks
091600060726     C                   IF        %scan(%trim(SQL_vabNT2):wNT2) <> 0 OR
091700060726     C                             %scan(%trim(SQL_vabNT2):wNOT) <> 0
091800060726     C                   SETON                                        40
091900060726     C                   ENDIF
092000060726     C                   ENDIF
092100060629     C* Verifico se lo spazio su NT2 è già esaurito mentre quello su NOT è ancora dispnibile
092200060726     C                   IF        *IN40 = *OFF
092300060629     C                   IF        (%len(%trim(wNT2))+1+%len(%trim(SQL_vabNT2))
092400060629     C                             >%size(wNT2)) AND
092500060629     C                             (%len(%trim(wNOT))+1+%len(%trim(SQL_vabNT2))
092600060629     C                             <=%size(wNOT))
092700060726     C                   IF        %scan(%trim(SQL_vabNT2):wNOT) = 0 AND
092800060726     C                             %scan(%trim(SQL_vabNT2):wNT2) = 0
092900060629     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNT2)
093000060726     C                   ENDIF
093100060629     C                   ELSE
093200060629     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNT2)
093300060629     C                   ENDIF
093400060629     C                   ENDIF
093500060726     C                   ENDIF
093600060629     C*
093700050620     C                   ELSE
093800050708     C                   EVAL      wNOT = %trim(SQL_vabNOT)
093900050708     C                   EVAL      wNT2 = %trim(SQL_vabNT2)
094000050620     C                   ENDIF
094100031110     C*
094200031110     C                   ENDSR
094300031112     C*------------------------------------------------------------------------*
094400031112     C* WRIVAD - SCRITTIURA FILE FIVAD
094500031112     C*------------------------------------------------------------------------*
094600031112     C     WRIVAD        BEGSR
094700031112     C*
094800031112     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAD)
094900031112     C                   CLEAR                   FIVAD000
095000031112     C*
095100051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAD
095200040712     C     KEYva_1       SETLL     EDIVADQT1L
095300031113     C                   IF        %equal(EDIVADQT1L)
095400040227     C* Eseguo routine x modifica codice trattamento merce
095500040227     C                   EXSR      CHGCTM
095600040227     C*
095700040712     C     KEYva_1       READE     EDIVADQT1L
095800031113     C                   DOW       not %eof(EDIVADQT1L)
095900031112     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
096000031112     C                   EVAL      vadAAS = WRK_vabAAS
096100031112     C                   EVAL      vadLNP = WRK_vabLNP
096200031112     C                   EVAL      vadNRS = WRK_vabNRS
096300031112     C                   EVAL      vadNSP = WRK_vabNSP
096400070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
096500070227     C   60              EVAL      vadNSP = ACC_NSP
096600031112     C* Inserisco record d dettaglio in FIVAD
096700031112     C                   WRITE     FIVAD000
096800031112     C* Elimino record d dettaglio in EDIVAD
096900031113     C                   DELETE    EDIVADQT1L
097000031112     C* Continuo la lettura dei dettagli della testata bolla corrente
097100040712     C     KEYva_1       READE     EDIVADQT1L
097200031112     C                   ENDDO
097300031112     C*
097400031112     C* Se invece nn esiste già il dettaglio colli in EDIVAD e sulla testata c'è l'indicazioine
097500031112     C* dei segnacolli => provvedo a creare il VAD
097600031112     C                   ELSE
097700031112     C                   IF        SQL_vabNCD > *zeros OR
097800031112     C                             SQL_vabNCA > *zeros
097900040227     C* Eseguo routine x modifica codice trattamento merce
098000040227     C                   EXSR      CHGCTM
098100031112     C* Valorizzo la chiave bolla del dettaglio con quella della relativa testata bolla "master"
098200031112     C                   EVAL      vadAAS = WRK_vabAAS
098300031112     C                   EVAL      vadLNP = WRK_vabLNP
098400031112     C                   EVAL      vadNRS = WRK_vabNRS
098500031112     C                   EVAL      vadNSP = WRK_vabNSP
098600031112     C* Valorizzo i restanti campi con quelli della testata bolla corrente
098700031112     C                   EVAL      vadFGS = SQL_vabFGS
098800031112     C                   EVAL      vadCCM = SQL_vabCCM
098900031112     C                   EVAL      vadNCL = SQL_vabNCL
099000031112     C                   EVAL      vadNCD = SQL_vabNCD
099100031112     C                   EVAL      vadNCA = SQL_vabNCA
099200070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
099300070227     C   60              EVAL      vadNSP = ACC_NSP
099400031112     C* Inserisco record d dettaglio in FIVAD
099500031112     C                   WRITE     FIVAD000
099600031112     C                   ENDIF
099700031112     C                   ENDIF
099800031112     C*
099900031112     C                   ENDSR
100000031114     C*------------------------------------------------------------------------*
100100031114     C* WRIVAT - SCRITTIURA FILE FIVAT
100200031114     C*------------------------------------------------------------------------*
100300031114     C     WRIVAT        BEGSR
100400031114     C*
100500031114     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAT)
100600031114     C                   CLEAR                   FIVAT000
100700031114     C*
100800051216     C* Verifico se x la bolla corrente di EDIVAB esiste già il relativo dettaglio in EDIVAT
100900040712     C     KEYva_1       SETLL     EDIVATQT1L
101000031114     C                   IF        %equal(EDIVATQT1L)
101100040227     C* Eseguo routine x modifica codice trattamento merce
101200040227     C                   EXSR      CHGCTM
101300040227     C*
101400040712     C     KEYva_1       READE     EDIVATQT1L
101500031114     C                   DOW       not %eof(EDIVATQT1L)
101600031114     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
101700031114     C                   EVAL      vatAAS = WRK_vabAAS
101800031114     C                   EVAL      vatLNP = WRK_vabLNP
101900031114     C                   EVAL      vatNRS = WRK_vabNRS
102000031114     C                   EVAL      vatNSP = WRK_vabNSP
102100070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
102200070227     C   60              EVAL      vatNSP = ACC_NSP
102300031114     C* Inserisco record d dettaglio in FIVAT
102400031114     C                   WRITE     FIVAT000
102500031114     C* Elimino record d dettaglio in EDIVAT
102600031114     C                   DELETE    EDIVATQT1L
102700031114     C* Continuo la lettura dei dettagli della testata bolla corrente
102800040712     C     KEYva_1       READE     EDIVATQT1L
102900031114     C                   ENDDO
103000031114     C                   ENDIF
103100031114     C*
103200031114     C                   ENDSR
103300070807     C*------------------------------------------------------------------------*
103400070807     C* WRIVAX - SCRITTIURA FILE FIVAX
103500070807     C*------------------------------------------------------------------------*
103600070807     C     WRIVAX        BEGSR
103700070809     C*
103800070809     C* Solo x record VAX diversi dalla bolla master (accorpante)
103900070809     C                   IF        SQL_vabFGS  = WRK_vabFGS AND
104000070809     C                             SQL_vabCCM  = WRK_vabCCM AND
104100070809     C                             SQL_vabCMR  = WRK_vabCMR AND
104200070809     C                             SQL_vabCNT  = WRK_vabCNT AND
104300070809     C                             SQL_vabAAS  = WRK_vabAAS AND
104400070809     C                             SQL_vabLNP  = WRK_vabLNP AND
104500070809     C                             SQL_vabNRS  = WRK_vabNRS AND
104600070809     C                             SQL_vabNSP <> WRK_vabNSP
104700070807     C*
104800070807     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAX)
104900070807     C                   CLEAR                   FIVAX000
105000070807     C*
105100070807     C                   Z-ADD     *zeros        wVAXmaster
105200070807     C*
105300070807     C* Effettuo conteggio righe attuali packing list della bolla master
105400070807     C*
105500070807     C/EXEC SQL
105600070905     C+ declare C1VX cursor for
105700070905     C*+ select count(*) from edivaxqt
105800070906     C+ select max(vaxprr) from edivaxqt
105900070807     C+ where vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
106000070807     C+ vaxCMR = :WRK_vabCMR and vaxCNT = :WRK_vabCNT and
106100070807     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
106200070807     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
106300070807     C+ for fetch only
106400070807     C/END-EXEC
106500070807     C
106600070807     C/EXEC SQL
106700070905     C+ open C1VX
106800070807     C/END-EXEC
106900070807     C
107000070807     C/EXEC SQL
107100070905     C+ Fetch C1VX into :wVAXmaster01
107200070807     C/END-EXEC
107300070905     C*
107400070905     C/EXEC SQL
107500070905     C+ declare C2VX cursor for
107600070905     C+ select max(vaxprr) from fivax00f
107700070905     C+ where vaxFGS = :SQL_vabFGS and vaxCCM = :SQL_vabCCM and
107800070905     C+ vaxAAS = :WRK_vabAAS and vaxLNP = :WRK_vabLNP and
107900070905     C+ vaxNRS = :WRK_vabNRS and vaxNSP = :WRK_vabNSP
108000070905     C+ for fetch only
108100070905     C/END-EXEC
108200070905     C
108300070905     C/EXEC SQL
108400070905     C+ open C2VX
108500070905     C/END-EXEC
108600070905     C
108700070905     C/EXEC SQL
108800070905     C+ Fetch C2VX into :wVAXmaster02
108900070905     C/END-EXEC
109000070809     C*
109100070905     C* Tengo il maggiore progressivo riga tra EDIVAX e FIVAX
109200070905     C                   EVAL      wVAXmaster = wVAXmaster01
109300070905     C                   IF        wVAXmaster02 > wVAXmaster01
109400070905     C                   EVAL      wVAXmaster = wVAXmaster02
109500070905     C                   ENDIF
109600070905     C*
109700070809     C* Proseguo elaborazione solo se effettivamente trovati VAX da accorpare
109800070905     C                   IF        wVAXmaster > *zeros
109900070807     C*
110000070807     C* Verifico se x la bolla corrente di EDIVAB esiste il relativo packing list in EDIVAX
110100070807     C     KEYva_1       SETLL     EDIVAXQT1L
110200070807     C                   IF        %equal(EDIVAXQT1L)
110300070809     C*
110400070809     C                   SETOFF                                       88
110500070807     C*
110600070807     C     KEYva_1       READE     EDIVAXQT1L
110700070807     C                   DOW       not %eof(EDIVAXQT1L)
110800070809     C*
110900070809     C* Se nn ancora gestito il salto pagina fittizio forzato
111000070809     C                   IF        *IN88 = *off
111100070809     C                   SETON                                        88
111200070905     C***                EXSR      GESFORMFEED
111300070809     C                   ENDIF
111400070809     C*
111500070807     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
111600070807     C                   EVAL      vaxAAS = WRK_vabAAS
111700070807     C                   EVAL      vaxLNP = WRK_vabLNP
111800070807     C                   EVAL      vaxNRS = WRK_vabNRS
111900070807     C                   EVAL      vaxNSP = WRK_vabNSP
112000070807     C* Auto-incremento numero progressivo riga packing list bolla master + bolla slave corrente
112100070905     C***                EVAL      vaxPRR = vaxPRR + wVAXmaster + wInsRighe
112200070905     C                   EVAL      vaxPRR = vaxPRR + wVAXmaster
112300070807     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
112400070807     C   60              EVAL      vaxNSP = ACC_NSP
112500070807     C* Inserisco record d dettaglio in FIVAX
112600070807     C                   WRITE     FIVAX000
112700070807     C* Elimino record d dettaglio in EDIVAX
112800070807     C                   DELETE    EDIVAXQT1L
112900070807     C* Continuo la lettura dei dettagli della testata bolla corrente
113000070807     C     KEYva_1       READE     EDIVAXQT1L
113100070807     C                   ENDDO
113200070807     C                   ENDIF
113300070807     C*
113400070809     C                   ENDIF
113500070809     C*
113600070807     C/EXEC SQL
113700070905     C+ close C1VX
113800070807     C/END-EXEC
113900070905     C*
114000070905     C/EXEC SQL
114100070905     C+ close C2VX
114200070905     C/END-EXEC
114300070905     C*
114400070809     C                   ENDIF
114500070807     C*
114600070807     C                   ENDSR
114700070809     C*------------------------------------------------------------------------*
114800070809     C* GESFORMFEED - GESTIONE FORZATURA SALTO PAGINA FITTIZIO
114900070809     C*------------------------------------------------------------------------*
115000070809     C     GESFORMFEED   BEGSR
115100070809     C*
115200070809     C* Salvo il formato record corrente
115300070809     C                   EVAL      DSEDIVAXSAV = DSEDIVAX
115400070809     C*
115500070809     C* Effettuo considerazioni x simulazione forzata salto pagina (ma solo se conviene davvero)
115600070809     C                   Z-ADD     *zeros        wRigheVuote       2 0
115700070809     C                   EVAL      wRigheVuote = wVAXmaster -
115800070809     C                             (42 * %div(wVAXmaster:42))
115900070809     C                   IF        wRigheVuote > *zeros
116000070809     C                   EVAL      wRigheVuote =
116100070809     C                             (42 * (%div(wVAXmaster:42)+1)) -
116200070809     C                             wVAXmaster
116300070809     C                   IF        wRigheVuote < 21
116400070809     C                   Z-ADD     wRigheVuote   wInsRighe         2 0
116500070809     C                   ELSE
116600070809     C                   Z-ADD     3             wInsRighe
116700070809     C                   ENDIF
116800070809     C*
116900070809     C                   Z-ADD     1             wRigaVuota        2 0
117000070809     C                   DOW       wRigaVuota <= wInsRighe
117100070809     C*
117200070809     C* Inserisco riga vuota sulla bolla master (accorpante) fino a capienza ultima pagina
117300070809     C                   EVAL      vaxAAS = WRK_vabAAS
117400070809     C                   EVAL      vaxLNP = WRK_vabLNP
117500070809     C                   EVAL      vaxNRS = WRK_vabNRS
117600070809     C                   EVAL      vaxNSP = WRK_vabNSP
117700070809     C                   EVAL      vaxPRR = wVAXmaster + wRigaVuota
117800070809     C                   EVAL      vaxDTT = *blanks
117900070809     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
118000070809     C   60              EVAL      vaxNSP = ACC_NSP
118100070809     C* Inserisco record d dettaglio in FIVAX
118200070809     C                   WRITE     FIVAX000
118300070809     C*
118400070809     C                   ADD       1             wRigaVuota
118500070809     C                   ENDDO
118600070809     C                   ENDIF
118700070809     C*
118800070809     C* Ripristino il formato record corrente
118900070809     C                   EVAL      DSEDIVAX = DSEDIVAXSAV
119000070809     C*
119100070809     C                   ENDSR
119200031112     C*------------------------------------------------------------------------*
119300031112     C* WRIVAB - SCRITTIURA FILE FIVAB X BOLLA "MASTER" ACCORPATA
119400031112     C*------------------------------------------------------------------------*
119500031112     C     WRIVAB        BEGSR
119600031112     C*
119700031112     C* Innanzitutto svuoto il buffer del record d testata d output (FIVAB)
119800031112     C                   CLEAR                   FIVAB000
119900031112     C* Valorizzo il buffer del record d testata d output (FIVAB)
120000031112     C                   EVAL      DSEDIVABOUT = DSEDIVABW
120100070420     C*
120200070420     C* Procedo solo se buffer effettivamente valorizzato
120300070420     C                   IF        DSEDIVABOUT <> DSEDIVABINZ
120400070420     C*
120500031112     C* Resetto alcuni campi che nella bolla "master" occcorrono NN valorizzati
120600031113     C   55              CLEAR                   vabNCD
120700031113     C   55              CLEAR                   vabNCA
120800031114     C   55              CLEAR                   vabRMO
120900031114     C   55              CLEAR                   vabCMO
121000031114     C   55              CLEAR                   vabNMO
121100050620     C                   IF        *IN55 = *on
121200050620     C* Campo RIFERIMENTO MITTENTE
121300050701     C                   IF        wRMO <> *blanks
121400060530     C                   Z-ADD     wGiro         wGiro2
121500060530     C                   EVAL      vabRMO = 'N'+%editw(wGiro2:'  ')+':'+
121600040303     C                                      %trim(wRMO)
121700050610     C                   IF        wGiro > 1 AND wOltreRIF = 'S'
121800040514     C                   MOVE      '...'         vabRMO
121900040303     C                   ENDIF
122000050701     C                   ENDIF
122100050620     C* Campi NOTE
122200060726     C   51              EVAL      vabNOT = %trim(wNOT)
122300060726     C   51              EVAL      vabNT2 = %trim(wNT2)
122400040303     C                   ENDIF
122500070227     C* Se richiesto ritorno al cliente file raccordo bolle "accorpanti"/"accorpate"
122600070227     C   60              EVAL      vabNSP = ACC_NSP
122700031112     C* Inserisco record d testata in FIVAB
122800031112     C                   WRITE     FIVAB000
122900070420     C                   ENDIF
123000050620     C*
123100050620     C* Inizializzo indicatori d wrk
123200050701     C                   SETOFF                                       51
123300031112     C*
123400031112     C                   ENDSR
123500070227     C*------------------------------------------------------------------------*
123600040709     C* CHKREC - CONTROLLO CORRETTEZZA RECORD TESTATA BOLLE AI FINI DEL RAGGRUPPAMENTO
123700040709     C*------------------------------------------------------------------------*
123800040709     C     CHKREC        BEGSR
123900040709     C*
124000040709     C* Inizializzo il flag d correttezza record testata bolle
124100040709     C                   MOVEL     'S'           wRECOK            1
124200051215     C*
124300051215     C* Verifica se bolla corrente presenta particolarità consegna RC e cliente nn abilitato
124400051215     C* specificatamente all'accorpamento anche con part. consegna RC
124500051215     C                   IF        wRECOK = 'S'
124600051216     C                   IF        SQL_vabGMA = 'RC'
124700051216     C                   IF        wAccRC <> 'S'
124800051215     C                   MOVEL     'N'           wRECOK
124900051216     C                   ELSE
125000051216     C* Se abilitazione ok controllo subito anche l'ORM relativo (quello che poi sarà aggiornato)
125100051216     C                   CLEAR                   wRC_KEY
125200051216     C                   EVAL      wRC_AAS = SQL_vabAAS
125300051216     C                   EVAL      wRC_LNP = SQL_vabLNP
125400051216     C                   EVAL      wRC_NRS = SQL_vabNRS
125500051216     C                   EVAL      wRC_NSP = SQL_vabNSP
125600051216     C                   CLEAR                   FNLS24DS
125700051216     C                   EVAL      I24TLA  = *blanks
125800051216     C                   EVAL      I24OPE  = 'L'
125900051216     C                   EVAL      I24KSC  = SQL_vabCCM
126000051216     C                   MOVE(P)   SQL_vabAAS    I24AAS
126100051216     C                   EVAL      I24SPE  = %subst(wRC_KEY:5:12)
126200051216     C                   CALL      'FNLS24R'
126300051216     C                   PARM                    FNLS24DS
126400051216     C* Se tutto ok procedo altrimenti skippo la bolla corrente con particolarità RC ai fini
126500051216     C* dell'accorpamento
126600051216     C                   IF        O24ERR <> *zeros
126700051216     C                   MOVEL     'N'           wRECOK
126800051216     C                   ENDIF
126900051215     C                   ENDIF
127000051216     C                   ENDIF
127100051215     C                   ENDIF
127200040709     C*
127300040709     C* Verifica codice trattamento
127400051215     C                   IF        wRECOK = 'S'
127500040709     C                   EXSR      CHKCTM
127600051215     C                   ENDIF
127700040709     C*
127800040709     C                   ENDSR
127900040709     C*------------------------------------------------------------------------*
128000040709     C* CHKCTM - CONTROLLO CODICE TRATTAMENTO MERCE
128100040709     C*------------------------------------------------------------------------*
128200040709     C     CHKCTM        BEGSR
128300040709     C*
128400040709     C* Verifico che il codice trattamento merce della bolla corrente nn preveda il VAX
128500040709     C                   EVAL      tblKUT = 1
128600040709     C                   EVAL      tblCOD = '1B'
128700040709     C                   EVAL      tblKEY = SQL_vabCTM
128800040709     C     KEYtab        CHAIN     TABEL00F
128900040709     C                   IF        %found(TABEL00F)
129000040709     C                   EVAL      DS1B = tblUNI
129100070809     C                   IF        §1BF16 = 'S' AND wAccVX <> 'S'
129200040709     C                   MOVEL     'N'           wRECOK
129300040709     C                   ENDIF
129400040709     C                   ENDIF
129500040709     C*
129600040709     C                   ENDSR
129700040227     C*------------------------------------------------------------------------*
129800040227     C* CHGCTM - CAMBIO CODICE TRATTAMENTO MERCE
129900040227     C*------------------------------------------------------------------------*
130000040227     C     CHGCTM        BEGSR
130100040227     C*
130200040227     C* Se sulla testata bolle NN è espressamente indicato il codice tratamento merce
130300040227     C* (e naturalmente o c'è il dettaglio colli o l'indicazione in testata dei segnacolli
130400040227     C* sequenziali) imposto come default il CTM = '2D'
130500040227     C                   IF        WRK_vabCTM = *blanks
130600040227     C                   EVAL      WRK_vabCTM = '2D'
130700040227     C                   ENDIF
130800040227     C*
130900040227     C* Valorizzo la chiave x reperire le informazioni relative al codice
131000040227     C* trattamento merce da impostare in caso d raggruppamento bolle
131100040227     C                   EVAL      tblKUT = 1
131200040227     C                   EVAL      tblCOD = '1B'
131300040227     C                   EVAL      tblKEY = WRK_vabCTM
131400040227     C     KEYtab        CHAIN     TABEL00F
131500040227     C                   IF        %found(TABEL00F)
131600040227     C                   EVAL      DS1B = tblUNI
131700040227     C                   IF        §1BCTR <> *blanks
131800040227     C                   EVAL      WRK_vabCTM = §1BCTR
131900040227     C                   ENDIF
132000040227     C                   ENDIF
132100040227     C*
132200040227     C                   ENDSR
132300050610     C*------------------------------------------------------------------------*
132400051021     C* REPABD - REPERISCE I DATI TABELLATI CHE PILOTANO LE MODALITA DI ACCORPAMENTO
132500050610     C*------------------------------------------------------------------------*
132600050610     C     REPABD        BEGSR
132700050610     C*
132800050610     C                   EVAL      tblKUT = 1
132900050610     C                   EVAL      tblCOD = '3C'
133000050708     C                   MOVEL(P)  D86CCM        tblKEY
133100050610     C     KEYtab        CHAIN     TABEL00F
133200050610     C                   IF        %found(TABEL00F)
133300050610     C                   EVAL      DS3C = tblUNI
133400050610     C                   MOVEL     §3CABD        wTipABD
133500050610     C                   Z-ADD     §3CLRM        wLenRIF
133600050610     C                   MOVEL     §3CALM        wAlmRIF
133700050620     C                   MOVEL     §3CNOT        wRgrNOT
133800051021     C                   MOVEL     §3CABC        wAccCAS
133900051215     C                   MOVEL     §3CARC        wAccRC
134000070227     C                   MOVEL     §3CFSP        wSpedCli
134100070809     C                   MOVEL     §3CAVX        wAccVX
134200050610     C                   ENDIF
134300070228     C*
134400070228     C* Se cliente che necessita ritorno informazioni d accorpamento => accendo indicatore preposto
134500070228     C                   if        wTipABD <> *blanks AND
134600070228     C                             wSpedCli = 'S'
134700070228     C                   seton                                        60
134800070228     C                   else
134900070228     C                   setoff                                       60
135000070228     C                   endif
135100050610     C*
135200050610     C                   ENDSR
135300070227     C*------------------------------------------------------------------------*
135400070419     C* REPNUM - STACCA NUMERATORE X MANTENIMENTO NUMERO SPEDIZIONE
135500070227     C*------------------------------------------------------------------------*
135600070227     C     REPNUM        BEGSR
135700070227     C*
135800070419     C* Numero spedizione (da Flnuf cod. 3)
135900070419     C                   clear                   Trul34ds
136000070419     C                   eval      i34aas = SQL_VABAAS
136100070419     C                   eval      i34lnp = SQL_VABLNP
136200070419     C                   call      'TRUL34R2'
136300070419     C                   parm                    kpjba
136400070419     C                   parm                    Trul34ds
136500070419     C                   if        o34err = *Zeros
136600070419     C                   z-add     o34nsp        ACC_NSP
136700070419     C                   else
136800070420     C                   z-add     SQL_VABNSP    ACC_NSP
136900070419     C                   endif
137000070227     C*
137100070227     C                   ENDSR
137200070227     C*------------------------------------------------------------------------*
137300070227     C* WRIRAB - SCRITTURA FILE RACCORDO BOLLE CLIENTE "ACCORPANTI"/"ACCORPATE"
137400070227     C*------------------------------------------------------------------------*
137500070227     C     WRIRAB        BEGSR
137600070227     C*
137700070227     C* Chiave spedizione
137800070227     C                   eval      rabAAS  = SQL_vabAAS
137900070227     C                   eval      rabLNP  = SQL_vabLNP
138000070227     C                   eval      rabNRS  = SQL_vabNRS
138100070227     C                   eval      rabNSP  = ACC_NSP
138200070227     C                   eval      rabCCM  = SQL_vabCCM
138300070227     C* Riferimenti cliente "accorpanti"
138400070227     C                   eval      rabRMN  = WRK_vabRMN
138500070227     C                   eval      rabRMA  = WRK_vabRMA
138600070227     C* Riferimenti cliente "accorpati"
138700070227     C                   eval      rabRMNR = SQL_vabRMN
138800070227     C                   eval      rabRMAR = SQL_vabRMA
138900070228     C* Attributi
139000070228     C                   eval      rabDAT  = datcor
139100070227     C*
139200070301     C                   write     FIRAB000
139300070227     C*
139400070227     C                   ENDSR
139500970526     C*------------------------------------------------------------------------*
139600970526     C* *INZSR - OPERAZIONI INIZIALI
139700970526     C*------------------------------------------------------------------------*
139800970526     C     *INZSR        BEGSR
139900040714     C*
140000040714     C*  Reperisco parametri in entrata
140100040714     C     *ENTRY        PLIST
140200070420     C                   PARM                    KPJBA
140300040714     C                   PARM                    TRTC86DS
140400070419     C*
140500070419     C                   SETOFF                                       60
140600060726     C*
140700060726     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
140800060726     C
140900060726     C/EXEC SQL
141000060726     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
141100060726     C/END-EXEC
141200060726     C*
141300031113     C*
141400031110     C*---
141500031110     C* CHIAVI DI LETTURA
141600031110     C*---
141700040709     C* CHIAVE DI LETTURA X EDIVABQT1L - Completa
141800040709     C     KEYvab1       KLIST
141900040709     C                   KFLD                    SQL_vabCCM
142000040709     C                   KFLD                    SQL_vabCMR
142100040709     C                   KFLD                    SQL_vabCNT
142200040709     C                   KFLD                    SQL_vabAAS
142300040709     C                   KFLD                    SQL_vabLNP
142400040709     C                   KFLD                    SQL_vabNRS
142500040709     C                   KFLD                    SQL_vabNSP
142600070807     C* CHIAVE DI LETTURA X EDIVADQT1L/EDIVATQT1L/EDIVAXQT1L
142700040712     C     KEYva_1       KLIST
142800031112     C                   KFLD                    SQL_vabFGS
142900031112     C                   KFLD                    SQL_vabCCM
143000031112     C                   KFLD                    SQL_vabCMR
143100031112     C                   KFLD                    SQL_vabCNT
143200031112     C                   KFLD                    SQL_vabAAS
143300031112     C                   KFLD                    SQL_vabLNP
143400031112     C                   KFLD                    SQL_vabNRS
143500031112     C                   KFLD                    SQL_vabNSP
143600040227     C* CHIAVE DI LETTURA X TABEL00F - Completa
143700040227     C     KEYtab        KLIST
143800040227     C                   KFLD                    tblKUT
143900040227     C                   KFLD                    tblCOD
144000040227     C                   KFLD                    tblKEY
144100070807     C** CHIAVE DI LETTURA X EDIVAX2L - Completa
144200070807     C*     KEYvax2_C     KLIST
144300070807     C*                   KFLD                    D86CCM
144400070807     C*                   KFLD                    D86CMR
144500070807     C*                   KFLD                    D86CNT
144600070228     C*
144700041116     C*  Richiamo XPARUT
144800041116     C                   Z-ADD     1             CODUT             1 0
144900041116     C                   CALL      'X§PARUT'
145000041116     C                   PARM                    UT§DSE
145100041116     C                   MOVEL     REC80         CNCR80
145200041116     c
145300041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
145400041116     C                   CLEAR                   TRUL06DS
145500041116     C                   MOVE      '£1'          D06COD
145600041116     C                   MOVEL     SIMFEL        D06KEY
145700041116     C                   MOVEL     'L'           D06TLA
145800041116     C                   MOVEL     TRUL06DS      KPJBU
145900041116      *
146000041116     C                   CALL      'TRUL06R'
146100041116     C                   PARM                    KPJBA
146200041116     C                   MOVEL     KPJBU         TRUL06DS
146300070228     C*
146400070228     C* CALCOLA LA DATA CORRENTE
146500070228     C                   time                    wn14             14 0
146600070228     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
146700070228     C                   z-add     wn8           g08dat
146800070228     C                   z-add     *zeros        g08inv
146900070228     C                   movel     '0'           g08err
147000070228     C                   call      'XSRDA8'
147100070228     C                   parm                    wlbda8
147200070228     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
147300991112     C*
147400991112     C                   ENDSR
