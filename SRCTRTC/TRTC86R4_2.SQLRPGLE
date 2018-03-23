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
003700050620     D
003800041116     D TRUL06DS      E DS
003900041116     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
004000031110     D*------------
004100031110     D* DS D WRK
004200031110     D*------------
004300051021     D  DSEDIABD       DS
004400031114     D   S_vabCCM                          LIKE(SQL_vabCCM)
004500031110     D   S_vabRSD                          LIKE(SQL_vabRSD)
004600031110     D   S_vabRD2                          LIKE(SQL_vabRD2)
004700031110     D   S_vabIND                          LIKE(SQL_vabIND)
004800031110     D   S_vabCAD                          LIKE(SQL_vabCAD)
004900031110     D   S_vabLOD                          LIKE(SQL_vabLOD)
005000031110     D   S_vabPRD                          LIKE(SQL_vabPRD)
005100031110     D   S_vabNZD                          LIKE(SQL_vabNZD)
005200031110     D   S_vabLNA                          LIKE(SQL_vabLNA)
005300031110     D   S_vabZNC                          LIKE(SQL_vabZNC)
005400031110     D   S_vabCBO                          LIKE(SQL_vabCBO)
005500031110     D   S_vabTSP                          LIKE(SQL_vabTSP)
005600031110     D   S_vabCTR                          LIKE(SQL_vabCTR)
005700031110     D   S_vabGC1                          LIKE(SQL_vabGC1)
005800031110     D   S_vabGC2                          LIKE(SQL_vabGC2)
005900031110     D   S_vabFFD                          LIKE(SQL_vabFFD)
006000031110     D   S_vabDCR                          LIKE(SQL_vabDCR)
006100031110     D   S_vabTCR                          LIKE(SQL_vabTCR)
006200031110     D   S_vabHCR                          LIKE(SQL_vabHCR)
006300031110     D   S_vabGMA                          LIKE(SQL_vabGMA)
006400031110     D   S_vabGGA                          LIKE(SQL_vabGGA)
006500031110     D   S_vabGVA                          LIKE(SQL_vabGVA)
006600031110     D   S_vabTC1                          LIKE(SQL_vabTC1)
006700031110     D   S_vabTC2                          LIKE(SQL_vabTC2)
006800031110     D   S_vabVAS                          LIKE(SQL_vabVAS)
006900051021     D   S_vabVAD                          LIKE(SQL_vabVAD)
007000051021     D*
007100031110     D                 DS
007200031110     D   ANTTIP1                      2  0
007300031110     D   ANTNUM1                      2  0
007400031110     D   ANTTIP2                      2  0
007500031110     D   ANTNUM2                      2  0
007600031110     D  DSVABANT               1      8  0
007700031110     D*------------
007800031110     D* VARIABILI D WRK
007900031110     D*------------
008000051021     D SAVEDIABD       S                   LIKE(DSEDIABD)
008100051021     D SAV_vabCAS      S                   LIKE(SQL_vabCAS)
008200051021     D SAV_vabVCA      S                   LIKE(SQL_vabVCA)
008300051021     D SAV_vabTIC      S                   LIKE(SQL_vabTIC)
008400040303     D wGiro           S              2  0
008500031110     D wANTTIP1        S                   LIKE(ANTTIP1)
008600031110     D wANTNUM1        S                   LIKE(ANTNUM1)
008700031110     D wANTTIP2        S                   LIKE(ANTTIP2)
008800031110     D wANTNUM2        S                   LIKE(ANTNUM2)
008900050610     D wRIF            S             99    VARYING
009000031114     D wRMO            S                   LIKE(VABRMO)
009100050620     D wNOT            S                   LIKE(VABNOT)
009200050620     D wNT2            S                   LIKE(VABNT2)
009300050610     D*------------
009400050620     D* VARIABILE RELATIVE AL RAGGRUPPAMENTO BOLLE
009500050610     D*------------
009600050610     D wTipABD         S                   like(§3CABD)
009700050610     D wLenRIF         S                   like(§3CLRM)
009800050610     D wAlmRIF         S                   like(§3CALM)
009900050620     D wRgrNOT         S                   like(§3CNOT)
010000051021     D wAccCAS         S                   like(§3CABC)
010100000629
010200000707
010300970526     C*------------------------------------------------------------------------*
010400970526     C* MAIN LINES
010500970526     C*------------------------------------------------------------------------*
010600050708     C*
010700050708     C* Reperisce quale riferimenti mittente mantenere durante l'accorpamento
010800050708     C                   exsr      repABD
010900031110     C*
011000031110     C* Eseguo routine d accorpamento bolle x medesimo destinatario e scrittura del FIVAB/FIVAD
011100031110     C                   exsr      exeABD
011200031110     C*
011300031110     C* Eseguo routine d scrittura del FIVAB/FIVAD x bolle NN raggruppabili
011400031110     C                   exsr      exeNOABD
011500991112     C*
011600970526     C* FINE PGM
011700031110     C                   seton                                        LR
011800991112     C*------------------------------------------------------------------------*
011900031110     C* EXEABD - ACCORPAMENTO BOLE X MEDESIMO DESTINATARIO
012000991112     C*------------------------------------------------------------------------*
012100031110     C     EXEABD        BEGSR
012200050620     C*
012300050620     C* Inizializzo indicatori d wrk
012400050620     C                   setoff                                       51
012500040709     C*
012600040712     C* Innanzitutto apro i file EDI*QT temporanei
012700040709     C                   open      EDIVABQT1L
012800040712     C                   open      EDIVADQT1L
012900040712     C                   open      EDIVATQT1L
013000031110     C*
013100031110     C* Inizializzazioni
013200031110     C                   clear                   DSEDIVAB
013300031110     C                   clear                   DSEDIABD
013400031110     C                   clear                   SAVEDIABD
013500031110     C                   exsr      exeINZ
013600031114     C                   eval      wGiro = *zeros
013700031113     C                   seton                                        55
013800000707     C*
013900000707     C/EXEC SQL
014000031113     C+ declare C1 cursor for select * from edivabqt
014100051021     C*+ where vabCAS = 0 and vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
014200051021     C+ where vabGMA <> 'RC' and vabATB = ' ' and vabxco=' '
014300031114     C+ order by vabCCM, vabRSD, vabRD2, vabIND, vabCAD, vabLOD,
014400031110     C+ vabPRD, vabNZD, vabLNA, vabZNC, vabCBO, vabTSP,
014500031110     C+ vabCTR, vabGC1, vabGC2, vabFFD, vabDCR, vabTCR,
014600031110     C+ vabHCR, vabGMA, vabGGA, vabGVA, vabTC1, vabTC2,
014700051021     C+ vabVAS, vabVAD, vabCAS, vabVCA, vabTIC, vabRMN, vabRMA
014800000707     C+ for fetch only
014900000707     C/END-EXEC
015000000707     C
015100000707     C/EXEC SQL
015200000707     C+ open C1
015300000707     C/END-EXEC
015400000707     C
015500000707     C/EXEC SQL
015600031110     C+ Fetch C1 into :DSEDIVAB
015700000707     C/END-EXEC
015800000707     C*
015900031110     C                   dow       sqlcod = *zeros
016000040709     C*
016100040709     C* Eseguo routine d controlli correttezza record x elaborazione accorpamento
016200051021     C                   exsr      CHKREC
016300040709     C*
016400040709     C* Se ok proseguo
016500040709     C                   if        wRECOK = 'S'
016600031110     C*
016700031110     C* Eseguo routine d valorizzazione DS d livello rottura
016800031110     C                   exsr      valDSROT
016900051021     C*
017000051021     C* Verifico avvenuta rottura d codice oppure bolla avente particolarità consegna RC
017100051021     C                   if        (DSEDIABD=SAVEDIABD AND SQL_vabCAS=*zeros) OR
017200051021     C                             (DSEDIABD=SAVEDIABD     AND
017300051021     C                              SQL_vabCAS>*zeros      AND
017400051021     C                             (SQL_vabVCA=SAV_vabVCA  AND
017500051021     C                              SQL_vabTIC=SAV_vabTIC   OR
017600051021     C                              SAV_vabCAS=*zeros)     AND
017700051021     C                              wAccCAS='S')
017800051021     C*
017900051021     C* ****************  ESEGUO ACCORPAMENTO **********************
018000051021     C*
018100051021     C* Eseguo la sommatoria dei campi da raggruppare
018200051021     C                   exsr      exeSOMMA
018300051021     C*
018400051021     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
018500051021     C                   exsr      exeNORM
018600051021     C*
018700051021     C* Effettuo scrittura del file FIVAD
018800051021     C                   exsr      wriVAD
018900051021     C*
019000051021     C* Effettuo scrittura del file FIVAT
019100051021     C                   exsr      wriVAT
019200051021     C*
019300051021     C* ****************  NO ACCORPAMENTO **********************
019400051021     C                   else
019500051021     C*
019600051021     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
019700051021     C* dell'ultima bolla accorpata
019800051021     C                   if        wGiro > 0
019900051021     C                   exsr      wriVAB
020000051021     C                   eval      wGiro = *zeros
020100051021     C                   endif
020200051021     C*
020300051021     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
020400051021     C                   eval      SAVEDIABD  = DSEDIABD
020500051102     C***                eval      SAV_vabCAS = *loval
020600051102     C***                eval      SAV_vabVCA = *loval
020700051102     C***                eval      SAV_vabTIC = *loval
020800051021     C*
020900051021     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
021000051021     C                   exsr      exeINZ
021100051021     C*
021200051021     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
021300051021     C                   exsr      impVAB
021400051021     C*
021500051021     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
021600051021     C                   exsr      wriVAD
021700051021     C*
021800051021     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
021900051021     C                   exsr      wriVAT
022000031110     C*
022100031110     C                   endif
022200031110     C*
022300031110     C* Incremento il contatore che identifica il 1° giro
022400031114     C                   eval      wGiro = wGiro + 1
022500051102     C*
022600051102     C* Salvo i nuovi attributi relativi al contrassegno
022700051102     C                   eval      SAV_vabCAS = SQL_vabCAS
022800051102     C                   eval      SAV_vabVCA = SQL_vabVCA
022900051102     C                   eval      SAV_vabTIC = SQL_vabTIC
023000040709     C*
023100040709     C* Come ultima cosa deleto il record bolla testata appena considerato
023200040712     C     KEYvab1       delete    EDIVABQT1L
023300040709     C*
023400040709     C                   endif
023500031110     C*
023600031110     C/EXEC SQL
023700031110     C+ Fetch C1 into :DSEDIVAB
023800031110     C/END-EXEC
023900031110     C                   enddo
024000031110     C*
024100000707     C/EXEC SQL
024200000707     C+ close C1
024300000707     C/END-EXEC
024400031114     C*
024500031114     C* Scarico la bolla "master" in sospeso
024600031114     C* dell'ultima bolla accorpata
024700031114     C                   exsr      wriVAB
024800040709     C*
024900040712     C* Al termine chiudo i file EDI*QT temporanei
025000040709     C                   close     EDIVABQT1L
025100040712     C                   close     EDIVADQT1L
025200040712     C                   close     EDIVATQT1L
025300991112     C*
025400991112     C                   ENDSR
025500031110     C*------------------------------------------------------------------------*
025600040712     C* EXENOABD - SCRITTIURA FILE FIVAB/FIVAD X BOLLE NN RAGGRUPPABILI
025700031110     C*------------------------------------------------------------------------*
025800031110     C     EXENOABD      BEGSR
025900050620     C*
026000050620     C* Inizializzo indicatori d wrk
026100050620     C                   setoff                                       51
026200040712     C*
026300040712     C* Innanzitutto apro i file EDI*QT temporanei
026400040712     C                   open      EDIVABQT1L
026500040712     C                   open      EDIVADQT1L
026600040712     C                   open      EDIVATQT1L
026700040712     C*
026800040712     C* Porto tutto ciò che è rimasto nei file EDI*QT nei relativi FIVA*
026900040712     C*
027000040712     C* ...VAB
027100040712     C     *loval        setll     EDIVABQT1L
027200040712     C                   read      EDIVABQT1L
027300040712     C                   dow       not %eof(EDIVABQT1L)
027400040712     C                   write     FIVAB000
027500040712     C                   delete    EDIVABQT1L
027600040712     C                   read      EDIVABQT1L
027700040712     C                   enddo
027800040712     C*
027900040712     C* ...VAD
028000040712     C     *loval        setll     EDIVADQT1L
028100040712     C                   read      EDIVADQT1L
028200040712     C                   dow       not %eof(EDIVADQT1L)
028300040712     C                   write     FIVAD000
028400040712     C                   delete    EDIVADQT1L
028500040712     C                   read      EDIVADQT1L
028600040712     C                   enddo
028700040712     C*
028800040712     C* ...VAT
028900040712     C     *loval        setll     EDIVATQT1L
029000040712     C                   read      EDIVATQT1L
029100040712     C                   dow       not %eof(EDIVATQT1L)
029200040712     C                   write     FIVAT000
029300040712     C                   delete    EDIVATQT1L
029400040712     C                   read      EDIVATQT1L
029500040712     C                   enddo
029600040714     C*
029700040714     C* ...VAX
029800050708     C     KEYvax2_C     setll     EDIVAX2L
029900050708     C     KEYvax2_C     reade     EDIVAX2L
030000040714     C                   dow       not %eof(EDIVAX2L)
030100041116     c     vaxfgs        lookup    lin                                    50
030200041116     c                   if        *in50  and vaxfgs>0
030300040714     C                   write     FIVAX000
030400040714     C                   delete    EDIVAX2L
030500041116     c                   endif
030600041116     c
030700050708     C     KEYvax2_C     reade     EDIVAX2L
030800040714     C                   enddo
030900040712     C*
031000040714     C* Al termine chiudo i file EDI* temporanei
031100040712     C                   close     EDIVABQT1L
031200040712     C                   close     EDIVADQT1L
031300040712     C                   close     EDIVATQT1L
031400031110     C*
031500031110     C                   ENDSR
031600031110     C*------------------------------------------------------------------------*
031700031110     C* EXEINZ - ROUTINE D INIZIALIZZAZIONE X OGNI NUOVO LIVELLO D ROTTURA BOLLE
031800031110     C*------------------------------------------------------------------------*
031900031110     C     EXEINZ        BEGSR
032000031110     C*
032100031110     C                   clear                   DSEDIVABW
032200031114     C                   clear                   wRMO
032300050620     C                   clear                   wNOT
032400050620     C                   clear                   wNT2
032500031110     C*
032600031110     C                   ENDSR
032700031110     C*------------------------------------------------------------------------*
032800031110     C* VALDSROT - VALORIZZAZIONE DS LIVELLO ROTTURA
032900031110     C*------------------------------------------------------------------------*
033000031112     C     VALDSROT      BEGSR
033100031110     C*
033200031110     C* Campi in rottura
033300031114     C                   EVAL      S_vabCCM = SQL_vabCCM
033400031110     C                   EVAL      S_vabRSD = SQL_vabRSD
033500031110     C                   EVAL      S_vabRD2 = SQL_vabRD2
033600031110     C                   EVAL      S_vabIND = SQL_vabIND
033700031110     C                   EVAL      S_vabCAD = SQL_vabCAD
033800031110     C                   EVAL      S_vabLOD = SQL_vabLOD
033900031110     C                   EVAL      S_vabPRD = SQL_vabPRD
034000031110     C                   EVAL      S_vabNZD = SQL_vabNZD
034100031110     C                   EVAL      S_vabLNA = SQL_vabLNA
034200031110     C                   EVAL      S_vabZNC = SQL_vabZNC
034300031110     C                   EVAL      S_vabCBO = SQL_vabCBO
034400031110     C                   EVAL      S_vabTSP = SQL_vabTSP
034500031110     C                   EVAL      S_vabCTR = SQL_vabCTR
034600031110     C                   EVAL      S_vabGC1 = SQL_vabGC1
034700031110     C                   EVAL      S_vabGC2 = SQL_vabGC2
034800031110     C                   EVAL      S_vabFFD = SQL_vabFFD
034900031110     C                   EVAL      S_vabDCR = SQL_vabDCR
035000031110     C                   EVAL      S_vabTCR = SQL_vabTCR
035100031110     C                   EVAL      S_vabHCR = SQL_vabHCR
035200031110     C                   EVAL      S_vabGMA = SQL_vabGMA
035300031110     C                   EVAL      S_vabGGA = SQL_vabGGA
035400031110     C                   EVAL      S_vabGVA = SQL_vabGVA
035500031110     C                   EVAL      S_vabTC1 = SQL_vabTC1
035600031110     C                   EVAL      S_vabTC2 = SQL_vabTC2
035700031110     C                   EVAL      S_vabVAS = SQL_vabVAS
035800031110     C                   EVAL      S_vabVAD = SQL_vabVAD
035900031110     C*
036000031110     C                   ENDSR
036100031110     C*------------------------------------------------------------------------*
036200031110     C* IMPVAB - VALORIZZAZIONE CAMPI "FISSI" 1° BOLLA D OGNI LIVELLO D ROTTURA
036300031110     C*------------------------------------------------------------------------*
036400031110     C     IMPVAB        BEGSR
036500031110     C*
036600031110     C* Valorizzo la DS d wrk da quella d lettura SQL
036700031110     C                   EVAL      DSEDIVABW = DSEDIVAB
036800031110     C*
036900031110     C                   ENDSR
037000031110     C*------------------------------------------------------------------------*
037100031110     C* EXESOMMA - SOMMATORIA CAMPI X RAGGRUPPAMENTO
037200031110     C*------------------------------------------------------------------------*
037300031110     C     EXESOMMA      BEGSR
037400031110     C*
037500031110     C                   EVAL      WRK_vabIAS = WRK_vabIAS + SQL_vabIAS
037600031110     C                   EVAL      WRK_vabNCL = WRK_vabNCL + SQL_vabNCL
037700031110     C                   EVAL      WRK_vabPKB = WRK_vabPKB + SQL_vabPKB
037800031110     C                   EVAL      WRK_vabVLB = WRK_vabVLB + SQL_vabVLB
037900031110     C                   EVAL      WRK_vabQFT = WRK_vabQFT + SQL_vabQFT
038000031110     C                   EVAL      WRK_vabVMD = WRK_vabVMD + SQL_vabVMD
038100051021     C                   EVAL      WRK_vabCAS = WRK_vabCAS + SQL_vabCAS
038200031110     C*
038300031110     C                   ENDSR
038400031110     C*------------------------------------------------------------------------*
038500031110     C* EXENORM - NORMALIZZAZIONE CAMPI X OTTENIMENTO BOLLA "MASTER"
038600031110     C*------------------------------------------------------------------------*
038700031110     C     EXENORM       BEGSR
038800050620     C*
038900050620     C* Valorizzo indicatore d wrk
039000050620     C                   SETON                                        51
039100031110     C*
039200031114     C                   IF        WRK_vabNAS =  *blanks AND
039300031114     C                             SQL_vabNAS <> *blanks
039400031110     C                   EVAL      WRK_vabNAS = SQL_vabNAS
039500031110     C                   ENDIF
039600031110     C*
039700031114     C                   IF        WRK_vabCTM =  *blanks AND
039800031114     C                             SQL_vabCTM <> *blanks
039900031110     C                   EVAL      WRK_vabCTM = SQL_vabCTM
040000031110     C                   ENDIF
040100031110     C*
040200031114     C                   IF        WRK_vabCTS =  *blanks AND
040300031114     C                             SQL_vabCTS <> *blanks
040400031110     C                   EVAL      WRK_vabCTS = SQL_vabCTS
040500031110     C                   ENDIF
040600031110     C*
040700031114     C                   IF        WRK_vabFTM =  *blanks AND
040800031114     C                             SQL_vabFTM <> *blanks
040900031110     C                   EVAL      WRK_vabFTM = SQL_vabFTM
041000031110     C                   ENDIF
041100031110     C*
041200031114     C                   IF        WRK_vabSCL =  *blanks AND
041300031114     C                             SQL_vabSCL <> *blanks
041400031110     C                   EVAL      WRK_vabSCL = SQL_vabSCL
041500031110     C                   ENDIF
041600051021     C*
041700051021     C                   IF        WRK_vabVCA =  *blanks AND
041800051021     C                             SQL_vabVCA <> *blanks
041900051021     C                   EVAL      WRK_vabVCA = SQL_vabVCA
042000051021     C                   ENDIF
042100051021     C*
042200051021     C                   IF        WRK_vabTIC =  *blanks AND
042300051021     C                             SQL_vabTIC <> *blanks
042400051021     C                   EVAL      WRK_vabTIC = SQL_vabTIC
042500051021     C                   ENDIF
042600031110     C*
042700031110     C* Gestisco comportamento particolare del campo VABANT (tipo e numero bancali)
042800031110     C                   IF        WRK_vabANT = *zeros
042900031110     C                   EVAL      WRK_vabANT = SQL_vabANT
043000031110     C                   ELSE
043100031110     C                   IF        SQL_vabANT = *zeros
043200031110     C                   ELSE
043300031110     C                   EVAL      DSVABANT = SQL_vabANT
043400031110     C                   EVAL      wANTTIP1 = ANTTIP1
043500031110     C                   EVAL      wANTNUM1 = ANTNUM1
043600031110     C                   EVAL      wANTTIP2 = ANTTIP2
043700031110     C                   EVAL      wANTNUM2 = ANTNUM2
043800031110     C                   EVAL      DSVABANT = WRK_vabANT
043900031110     C                   IF        ANTTIP1 > *zeros
044000031110     C                   IF        wANTTIP1 = ANTTIP1
044100031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM1
044200031110     C                   ENDIF
044300031110     C                   IF        wANTTIP2 = ANTTIP1
044400031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM2
044500031110     C                   ENDIF
044600031110     C                   ELSE
044700031110     C                   IF        wANTTIP1 > *zeros
044800031110     C                   EVAL      ANTTIP1 = wANTTIP1
044900031110     C                   EVAL      ANTNUM1 = wANTNUM1
045000031110     C                   ELSE
045100031110     C                   IF        wANTTIP2 > *zeros
045200031110     C                   EVAL      ANTTIP1 = wANTTIP2
045300031110     C                   EVAL      ANTNUM1 = wANTNUM2
045400031110     C                   ENDIF
045500031110     C                   ENDIF
045600031110     C                   ENDIF
045700031110     C                   IF        ANTTIP2 > *zeros
045800031110     C                   IF        wANTTIP1 = ANTTIP2
045900031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM1
046000031110     C                   ENDIF
046100031110     C                   IF        wANTTIP2 = ANTTIP2
046200031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM2
046300031110     C                   ENDIF
046400031110     C                   ELSE
046500031110     C                   IF        wANTTIP1 > *zeros
046600031110     C                   EVAL      ANTTIP2 = wANTTIP1
046700031110     C                   EVAL      ANTNUM2 = wANTNUM1
046800031110     C                   ELSE
046900031110     C                   IF        wANTTIP2 > *zeros
047000031110     C                   EVAL      ANTTIP2 = wANTTIP2
047100031110     C                   EVAL      ANTNUM2 = wANTNUM2
047200031110     C                   ENDIF
047300031110     C                   ENDIF
047400031110     C                   ENDIF
047500031110     C                   EVAL      WRK_vabANT = DSVABANT
047600031110     C                   ENDIF
047700031110     C                   ENDIF
047800031114     C*
047900051021     C* Eseguo raggruppamento dei riferimenti delle testate bolle accorpate
048000051021     C                   IF        wLenRIF > *zeros
048100050906     C                   IF        wTIPABD = 'S'
048200050614     C***                MOVE(P)   SQL_vabRMN    wRMN             15
048300050614     C***                EVAL      wRIF = %trim(wRMN)
048400050614     C                   EVAL      wRIF = %trim(%editc(SQL_vabRMN:'4'))
048500050610     C                   ENDIF
048600050610     C                   IF        wTIPABD = 'A'
048700050610     C                   EVAL      wRIF = %trim(SQL_vabRMA)
048800050610     C                   ENDIF
048900050610     C*
049000050610     C* Gestisco l'allineamento d default a seconda della tipologia d riferimento da utilizzare
049100050610     C                   IF        wAlmRIF = *blanks
049200050610     C                   IF        wTipABD = 'S'
049300050610     C                   EVAL      wAlmRIF = 'D'
049400050610     C                   ENDIF
049500050610     C                   IF        wTipABD = 'A'
049600050610     C                   EVAL      wAlmRIF = 'S'
049700050610     C                   ENDIF
049800050610     C                   ENDIF
049900051021     C*
050000051021     C* Effettuo le operazioni seguenti SOLO in caso in cui il riferimento voluto
050100051021     C* risulti essere effettivamente valorizzato
050200051021     C                   IF        wRIF <> *blanks
050300050614     C*
050400050614     C* Considero la minore tra la lunghezza reale del riferimento da considerare e il
050500050614     C* numero d byte da considerare indicati in tabella x il cliente corrente
050600050614     C                   Z-ADD     *zeros        wByteRIF          2 0
050700050614     C                   IF        wLenRIF <= %len(%trim(wRIF))
050800050614     C                   EVAL      wByteRIF = wLenRIF
050900050614     C                   ELSE
051000050614     C                   EVAL      wByteRIF = %len(%trim(wRIF))
051100050614     C                   ENDIF
051200050610     C*
051300050610     C* A questo punto verifico se sono richiesti solo 1 numero specifico d byte ed il
051400050610     C* relativo allineamento
051500050610     C                   IF        wLenRIF <> 99
051600050610     C                   IF        wAlmRIF = 'S'
051700050614     C                   EVAL      wRIF = %subst(%trim(wRIF):1:wByteRIF)
051800050610     C                   ENDIF
051900050610     C                   IF        wAlmRIF = 'D'
052000050610     C                   EVAL      wRIF = %subst(%trim(wRIF):
052100050614     C                                    %len(%trim(wRIF))-wByteRIF+1)
052200050610     C                   ENDIF
052300050610     C                   ENDIF
052400051021     C*
052500051021     C                   ELSE
052600051021     C                   MOVEL     'N'           wOltreRIF
052700051021     C                   ENDIF
052800050610     C*
052900031114     C* Se c stanno...
053000050610     C                   IF        %len(%trim(wRMO))+1+%len(%trim(wRIF)) <=
053100040517     C                             %len(wRMO)-4
053200050610     C                   EVAL      wRMO = %trim(wRMO) + %trim(wRIF) + '-'
053300050610     C                   MOVEL     'N'           wOltreRIF         1
053400050610     C                   ELSE
053500050610     C                   MOVEL     'S'           wOltreRIF
053600031114     C                   ENDIF
053700050906     C*
053800050906     C                   ELSE
053900050906     C                   MOVEL     'N'           wOltreRIF
054000050906     C                   ENDIF
054100051021     C*
054200050620     C*
054300050620     C* Se richiesto raggruppo anche le note
054400050620     C                   IF        wRgrNOT = 'S'
054500050708     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNOT)
054600050708     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNT2)
054700050620     C                   ELSE
054800050708     C                   EVAL      wNOT = %trim(SQL_vabNOT)
054900050708     C                   EVAL      wNT2 = %trim(SQL_vabNT2)
055000050620     C                   ENDIF
055100031110     C*
055200031110     C                   ENDSR
055300031112     C*------------------------------------------------------------------------*
055400031112     C* WRIVAD - SCRITTIURA FILE FIVAD
055500031112     C*------------------------------------------------------------------------*
055600031112     C     WRIVAD        BEGSR
055700031112     C*
055800031112     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAD)
055900031112     C                   CLEAR                   FIVAD000
056000031112     C*
056100031112     C* Verifico se x la bolla corrente di EDIVAB esiste giò il relativo dettaglio in EDIVAD
056200040712     C     KEYva_1       SETLL     EDIVADQT1L
056300031113     C                   IF        %equal(EDIVADQT1L)
056400040227     C* Eseguo routine x modifica codice trattamento merce
056500040227     C                   EXSR      CHGCTM
056600040227     C*
056700040712     C     KEYva_1       READE     EDIVADQT1L
056800031113     C                   DOW       not %eof(EDIVADQT1L)
056900031112     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
057000031112     C                   EVAL      vadAAS = WRK_vabAAS
057100031112     C                   EVAL      vadLNP = WRK_vabLNP
057200031112     C                   EVAL      vadNRS = WRK_vabNRS
057300031112     C                   EVAL      vadNSP = WRK_vabNSP
057400031112     C* Inserisco record d dettaglio in FIVAD
057500031112     C                   WRITE     FIVAD000
057600031112     C* Elimino record d dettaglio in EDIVAD
057700031113     C                   DELETE    EDIVADQT1L
057800031112     C* Continuo la lettura dei dettagli della testata bolla corrente
057900040712     C     KEYva_1       READE     EDIVADQT1L
058000031112     C                   ENDDO
058100031112     C*
058200031112     C* Se invece nn esiste già il dettaglio colli in EDIVAD e sulla testata c'è l'indicazioine
058300031112     C* dei segnacolli => provvedo a creare il VAD
058400031112     C                   ELSE
058500031112     C                   IF        SQL_vabNCD > *zeros OR
058600031112     C                             SQL_vabNCA > *zeros
058700040227     C* Eseguo routine x modifica codice trattamento merce
058800040227     C                   EXSR      CHGCTM
058900031112     C* Valorizzo la chiave bolla del dettaglio con quella della relativa testata bolla "master"
059000031112     C                   EVAL      vadAAS = WRK_vabAAS
059100031112     C                   EVAL      vadLNP = WRK_vabLNP
059200031112     C                   EVAL      vadNRS = WRK_vabNRS
059300031112     C                   EVAL      vadNSP = WRK_vabNSP
059400031112     C* Valorizzo i restanti campi con quelli della testata bolla corrente
059500031112     C                   EVAL      vadFGS = SQL_vabFGS
059600031112     C                   EVAL      vadCCM = SQL_vabCCM
059700031112     C                   EVAL      vadNCL = SQL_vabNCL
059800031112     C                   EVAL      vadNCD = SQL_vabNCD
059900031112     C                   EVAL      vadNCA = SQL_vabNCA
060000031112     C* Inserisco record d dettaglio in FIVAD
060100031112     C                   WRITE     FIVAD000
060200031112     C                   ENDIF
060300031112     C                   ENDIF
060400031112     C*
060500031112     C                   ENDSR
060600031114     C*------------------------------------------------------------------------*
060700031114     C* WRIVAT - SCRITTIURA FILE FIVAT
060800031114     C*------------------------------------------------------------------------*
060900031114     C     WRIVAT        BEGSR
061000031114     C*
061100031114     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAT)
061200031114     C                   CLEAR                   FIVAT000
061300031114     C*
061400031124     C* Verifico se x la bolla corrente di EDIVAB esiste giò il relativo dettaglio in EDIVAT
061500040712     C     KEYva_1       SETLL     EDIVATQT1L
061600031114     C                   IF        %equal(EDIVATQT1L)
061700040227     C* Eseguo routine x modifica codice trattamento merce
061800040227     C                   EXSR      CHGCTM
061900040227     C*
062000040712     C     KEYva_1       READE     EDIVATQT1L
062100031114     C                   DOW       not %eof(EDIVATQT1L)
062200031114     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
062300031114     C                   EVAL      vatAAS = WRK_vabAAS
062400031114     C                   EVAL      vatLNP = WRK_vabLNP
062500031114     C                   EVAL      vatNRS = WRK_vabNRS
062600031114     C                   EVAL      vatNSP = WRK_vabNSP
062700031114     C* Inserisco record d dettaglio in FIVAT
062800031114     C                   WRITE     FIVAT000
062900031114     C* Elimino record d dettaglio in EDIVAT
063000031114     C                   DELETE    EDIVATQT1L
063100031114     C* Continuo la lettura dei dettagli della testata bolla corrente
063200040712     C     KEYva_1       READE     EDIVATQT1L
063300031114     C                   ENDDO
063400031114     C                   ENDIF
063500031114     C*
063600031114     C                   ENDSR
063700031112     C*------------------------------------------------------------------------*
063800031112     C* WRIVAB - SCRITTIURA FILE FIVAB X BOLLA "MASTER" ACCORPATA
063900031112     C*------------------------------------------------------------------------*
064000031112     C     WRIVAB        BEGSR
064100031112     C*
064200031112     C* Innanzitutto svuoto il buffer del record d testata d output (FIVAB)
064300031112     C                   CLEAR                   FIVAB000
064400031112     C* Valorizzo il buffer del record d testata d output (FIVAB)
064500031112     C                   EVAL      DSEDIVABOUT = DSEDIVABW
064600031112     C* Resetto alcuni campi che nella bolla "master" occcorrono NN valorizzati
064700031113     C   55              CLEAR                   vabNCD
064800031113     C   55              CLEAR                   vabNCA
064900031114     C   55              CLEAR                   vabRMO
065000031114     C   55              CLEAR                   vabCMO
065100031114     C   55              CLEAR                   vabNMO
065200050620     C                   IF        *IN55 = *on
065300050620     C* Campo RIFERIMENTO MITTENTE
065400050701     C                   IF        wRMO <> *blanks
065500040517     C                   EVAL      vabRMO = 'N'+%editw(wGiro:'  ')+':'+
065600040303     C                                      %trim(wRMO)
065700050610     C                   IF        wGiro > 1 AND wOltreRIF = 'S'
065800040514     C                   MOVE      '...'         vabRMO
065900040303     C                   ENDIF
066000050701     C                   ENDIF
066100050620     C* Campi NOTE
066200050620     C   51              EVAL      vabNOT = %trim(wNOT)
066300050620     C   51              EVAL      vabNT2 = %trim(wNT2)
066400040303     C                   ENDIF
066500031112     C* Inserisco record d testata in FIVAB
066600031112     C                   WRITE     FIVAB000
066700050620     C*
066800050620     C* Inizializzo indicatori d wrk
066900050701     C                   SETOFF                                       51
067000031112     C*
067100031112     C                   ENDSR
067200040709     C*------------------------------------------------------------------------*
067300040709     C* CHKREC - CONTROLLO CORRETTEZZA RECORD TESTATA BOLLE AI FINI DEL RAGGRUPPAMENTO
067400040709     C*------------------------------------------------------------------------*
067500040709     C     CHKREC        BEGSR
067600040709     C*
067700040709     C* Inizializzo il flag d correttezza record testata bolle
067800040709     C                   MOVEL     'S'           wRECOK            1
067900040709     C*
068000040709     C* Verifica codice trattamento
068100040709     C                   EXSR      CHKCTM
068200040709     C*
068300040709     C                   ENDSR
068400040709     C*------------------------------------------------------------------------*
068500040709     C* CHKCTM - CONTROLLO CODICE TRATTAMENTO MERCE
068600040709     C*------------------------------------------------------------------------*
068700040709     C     CHKCTM        BEGSR
068800040709     C*
068900040709     C* Verifico che il codice trattamento merce della bolla corrente nn preveda il VAX
069000040709     C                   EVAL      tblKUT = 1
069100040709     C                   EVAL      tblCOD = '1B'
069200040709     C                   EVAL      tblKEY = SQL_vabCTM
069300040709     C     KEYtab        CHAIN     TABEL00F
069400040709     C                   IF        %found(TABEL00F)
069500040709     C                   EVAL      DS1B = tblUNI
069600040709     C                   IF        §1BF16 = 'S'
069700040709     C                   MOVEL     'N'           wRECOK
069800040709     C                   ENDIF
069900040709     C                   ENDIF
070000040709     C*
070100040709     C                   ENDSR
070200040227     C*------------------------------------------------------------------------*
070300040227     C* CHGCTM - CAMBIO CODICE TRATTAMENTO MERCE
070400040227     C*------------------------------------------------------------------------*
070500040227     C     CHGCTM        BEGSR
070600040227     C*
070700040227     C* Se sulla testata bolle NN è espressamente indicato il codice tratamento merce
070800040227     C* (e naturalmente o c'è il dettaglio colli o l'indicazione in testata dei segnacolli
070900040227     C* sequenziali) imposto come default il CTM = '2D'
071000040227     C                   IF        WRK_vabCTM = *blanks
071100040227     C                   EVAL      WRK_vabCTM = '2D'
071200040227     C                   ENDIF
071300040227     C*
071400040227     C* Valorizzo la chiave x reperire le informazioni relative al codice
071500040227     C* trattamento merce da impostare in caso d raggruppamento bolle
071600040227     C                   EVAL      tblKUT = 1
071700040227     C                   EVAL      tblCOD = '1B'
071800040227     C                   EVAL      tblKEY = WRK_vabCTM
071900040227     C     KEYtab        CHAIN     TABEL00F
072000040227     C                   IF        %found(TABEL00F)
072100040227     C                   EVAL      DS1B = tblUNI
072200040227     C                   IF        §1BCTR <> *blanks
072300040227     C                   EVAL      WRK_vabCTM = §1BCTR
072400040227     C                   ENDIF
072500040227     C                   ENDIF
072600040227     C*
072700040227     C                   ENDSR
072800050610     C*------------------------------------------------------------------------*
072900051021     C* REPABD - REPERISCE I DATI TABELLATI CHE PILOTANO LE MODALITA DI ACCORPAMENTO
073000050610     C*------------------------------------------------------------------------*
073100050610     C     REPABD        BEGSR
073200050610     C*
073300050610     C                   EVAL      tblKUT = 1
073400050610     C                   EVAL      tblCOD = '3C'
073500050708     C                   MOVEL(P)  D86CCM        tblKEY
073600050610     C     KEYtab        CHAIN     TABEL00F
073700050610     C                   IF        %found(TABEL00F)
073800050610     C                   EVAL      DS3C = tblUNI
073900050610     C                   MOVEL     §3CABD        wTipABD
074000050610     C                   Z-ADD     §3CLRM        wLenRIF
074100050610     C                   MOVEL     §3CALM        wAlmRIF
074200050620     C                   MOVEL     §3CNOT        wRgrNOT
074300051021     C                   MOVEL     §3CABC        wAccCAS
074400050610     C                   ENDIF
074500050610     C*
074600050610     C                   ENDSR
074700970526     C*------------------------------------------------------------------------*
074800970526     C* *INZSR - OPERAZIONI INIZIALI
074900970526     C*------------------------------------------------------------------------*
075000970526     C     *INZSR        BEGSR
075100040714     C*
075200040714     C*  Reperisco parametri in entrata
075300040714     C     *ENTRY        PLIST
075400040714     C                   PARM                    TRTC86DS
075500031113     C*
075600031110     C*---
075700031110     C* CHIAVI DI LETTURA
075800031110     C*---
075900040709     C* CHIAVE DI LETTURA X EDIVABQT1L - Completa
076000040709     C     KEYvab1       KLIST
076100040709     C                   KFLD                    SQL_vabCCM
076200040709     C                   KFLD                    SQL_vabCMR
076300040709     C                   KFLD                    SQL_vabCNT
076400040709     C                   KFLD                    SQL_vabAAS
076500040709     C                   KFLD                    SQL_vabLNP
076600040709     C                   KFLD                    SQL_vabNRS
076700040709     C                   KFLD                    SQL_vabNSP
076800040712     C* CHIAVE DI LETTURA X EDIVADQT1L/EDIVATQT1L
076900040712     C     KEYva_1       KLIST
077000031112     C                   KFLD                    SQL_vabFGS
077100031112     C                   KFLD                    SQL_vabCCM
077200031112     C                   KFLD                    SQL_vabCMR
077300031112     C                   KFLD                    SQL_vabCNT
077400031112     C                   KFLD                    SQL_vabAAS
077500031112     C                   KFLD                    SQL_vabLNP
077600031112     C                   KFLD                    SQL_vabNRS
077700031112     C                   KFLD                    SQL_vabNSP
077800040227     C* CHIAVE DI LETTURA X TABEL00F - Completa
077900040227     C     KEYtab        KLIST
078000040227     C                   KFLD                    tblKUT
078100040227     C                   KFLD                    tblCOD
078200040227     C                   KFLD                    tblKEY
078300040714     C* CHIAVE DI LETTURA X EDIVAX2L - Completa
078400050708     C     KEYvax2_C     KLIST
078500050708     C                   KFLD                    D86CCM
078600050708     C                   KFLD                    D86CMR
078700050708     C                   KFLD                    D86CNT
078800041116     c
078900041116     C*  Richiamo XPARUT
079000041116     C                   Z-ADD     1             CODUT             1 0
079100041116     C                   CALL      'X§PARUT'
079200041116     C                   PARM                    UT§DSE
079300041116     C                   MOVEL     REC80         CNCR80
079400041116     c
079500041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
079600041116     C                   CLEAR                   TRUL06DS
079700041116     C                   MOVE      '£1'          D06COD
079800041116     C                   MOVEL     SIMFEL        D06KEY
079900041116     C                   MOVEL     'L'           D06TLA
080000041116     C                   MOVEL     TRUL06DS      KPJBU
080100041116      *
080200041116     C                   CALL      'TRUL06R'
080300041116     C                   PARM                    KPJBA
080400041116     C                   MOVEL     KPJBU         TRUL06DS
080500991112     C*
080600991112     C                   ENDSR
