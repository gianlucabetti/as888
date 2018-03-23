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
007000031110     D*
007100051021     D   S_vabVCA      S                   LIKE(SQL_vabVCA)
007200051021     D   S_vabTIC      S                   LIKE(SQL_vabTIC)
007300051021     D*
007400031110     D                 DS
007500031110     D   ANTTIP1                      2  0
007600031110     D   ANTNUM1                      2  0
007700031110     D   ANTTIP2                      2  0
007800031110     D   ANTNUM2                      2  0
007900031110     D  DSVABANT               1      8  0
008000031110     D*------------
008100031110     D* VARIABILI D WRK
008200031110     D*------------
008300051021     D SAVEDIABD       S                   LIKE(DSEDIABD)
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
014700051021     C+ vabVAS, vabVAD, vabVCA, vabTIC, vabRMN, vabRMA
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
016900031110     C*
017000040303     C* Verifico avvenuta rottura d codice oppure bolla avente particolarità consegna RC
017100051021     C                   if        DSEDIABD  <> SAVEDIABD OR
017200051021     C                             (DSEDIABD  = SAVEDIABD AND
017300051021     C                               wAccCAS  = 'S' AND SQL_vabCAS <> *zeros AND
017400051021     C                             (WRK_vabCAS <> *zeros   AND
017500051021     C                             (WRK_vabVCA <> S_vabVCA OR
017600051021     C                              WRK_vabTIC <> S_vabTIC))) OR
017700051021     C                             (DSEDIABD  = SAVEDIABD  AND
017800051021     C                               wAccCAS <> 'S'        AND
017900051021     C                              SQL_vabCAS > *zeros)
018000031110     C*
018100031112     C* Se nn è il 1° giro => scarico la bolla "master" in sospeso
018200031112     C* dell'ultima bolla accorpata
018300031114     C                   if        wGiro > 0
018400031112     C                   exsr      wriVAB
018500031114     C                   eval      wGiro = *zeros
018600031110     C                   endif
018700031110     C*
018800051021     C* Salvo il nuovo livello d rottura e i campi reletivi ai test sul contrassegno
018900051021     C                   eval      SAVEDIABD  = DSEDIABD
019000031110     C*
019100031110     C* Eseguo le inizializzazioni x ogni nuovo livello d rottura
019200031110     C                   exsr      exeINZ
019300031110     C*
019400031110     C* Eseguo le valorizzazioni dei campi relativi alla 1° bolla del livello d rottura corrente
019500031110     C                   exsr      impVAB
019600031112     C*
019700031112     C* Effettuo scrittura del file FIVAD x dettaglio bolla "master"
019800031114     C                   exsr      wriVAD
019900031114     C*
020000031114     C* Effettuo scrittura del file FIVAT x dettaglio bolla "master"
020100031114     C                   exsr      wriVAT
020200031110     C*
020300051021     C* ****************  ESEGUO ACCORPAMENTO **********************
020400031110     C                   else
020500031110     C*
020600031110     C* Eseguo la sommatoria dei campi da raggruppare
020700031110     C                   exsr      exeSOMMA
020800031110     C*
020900031112     C* Eseguo la normalizzazione campi x ottenere bolla "master" finale
021000031110     C                   exsr      exeNORM
021100031110     C*
021200031110     C* Effettuo scrittura del file FIVAD
021300031114     C                   exsr      wriVAD
021400031114     C*
021500031114     C* Effettuo scrittura del file FIVAT
021600031114     C                   exsr      wriVAT
021700031110     C*
021800031110     C                   endif
021900031110     C*
022000031110     C* Incremento il contatore che identifica il 1° giro
022100031114     C                   eval      wGiro = wGiro + 1
022200040709     C*
022300040709     C* Come ultima cosa deleto il record bolla testata appena considerato
022400040712     C     KEYvab1       delete    EDIVABQT1L
022500040709     C*
022600040709     C                   endif
022700031110     C*
022800031110     C/EXEC SQL
022900031110     C+ Fetch C1 into :DSEDIVAB
023000031110     C/END-EXEC
023100031110     C                   enddo
023200031110     C*
023300000707     C/EXEC SQL
023400000707     C+ close C1
023500000707     C/END-EXEC
023600031114     C*
023700031114     C* Scarico la bolla "master" in sospeso
023800031114     C* dell'ultima bolla accorpata
023900031114     C                   exsr      wriVAB
024000040709     C*
024100040712     C* Al termine chiudo i file EDI*QT temporanei
024200040709     C                   close     EDIVABQT1L
024300040712     C                   close     EDIVADQT1L
024400040712     C                   close     EDIVATQT1L
024500991112     C*
024600991112     C                   ENDSR
024700031110     C*------------------------------------------------------------------------*
024800040712     C* EXENOABD - SCRITTIURA FILE FIVAB/FIVAD X BOLLE NN RAGGRUPPABILI
024900031110     C*------------------------------------------------------------------------*
025000031110     C     EXENOABD      BEGSR
025100050620     C*
025200050620     C* Inizializzo indicatori d wrk
025300050620     C                   setoff                                       51
025400040712     C*
025500040712     C* Innanzitutto apro i file EDI*QT temporanei
025600040712     C                   open      EDIVABQT1L
025700040712     C                   open      EDIVADQT1L
025800040712     C                   open      EDIVATQT1L
025900040712     C*
026000040712     C* Porto tutto ciò che è rimasto nei file EDI*QT nei relativi FIVA*
026100040712     C*
026200040712     C* ...VAB
026300040712     C     *loval        setll     EDIVABQT1L
026400040712     C                   read      EDIVABQT1L
026500040712     C                   dow       not %eof(EDIVABQT1L)
026600040712     C                   write     FIVAB000
026700040712     C                   delete    EDIVABQT1L
026800040712     C                   read      EDIVABQT1L
026900040712     C                   enddo
027000040712     C*
027100040712     C* ...VAD
027200040712     C     *loval        setll     EDIVADQT1L
027300040712     C                   read      EDIVADQT1L
027400040712     C                   dow       not %eof(EDIVADQT1L)
027500040712     C                   write     FIVAD000
027600040712     C                   delete    EDIVADQT1L
027700040712     C                   read      EDIVADQT1L
027800040712     C                   enddo
027900040712     C*
028000040712     C* ...VAT
028100040712     C     *loval        setll     EDIVATQT1L
028200040712     C                   read      EDIVATQT1L
028300040712     C                   dow       not %eof(EDIVATQT1L)
028400040712     C                   write     FIVAT000
028500040712     C                   delete    EDIVATQT1L
028600040712     C                   read      EDIVATQT1L
028700040712     C                   enddo
028800040714     C*
028900040714     C* ...VAX
029000050708     C     KEYvax2_C     setll     EDIVAX2L
029100050708     C     KEYvax2_C     reade     EDIVAX2L
029200040714     C                   dow       not %eof(EDIVAX2L)
029300041116     c     vaxfgs        lookup    lin                                    50
029400041116     c                   if        *in50  and vaxfgs>0
029500040714     C                   write     FIVAX000
029600040714     C                   delete    EDIVAX2L
029700041116     c                   endif
029800041116     c
029900050708     C     KEYvax2_C     reade     EDIVAX2L
030000040714     C                   enddo
030100040712     C*
030200040714     C* Al termine chiudo i file EDI* temporanei
030300040712     C                   close     EDIVABQT1L
030400040712     C                   close     EDIVADQT1L
030500040712     C                   close     EDIVATQT1L
030600031110     C*
030700031110     C                   ENDSR
030800031110     C*------------------------------------------------------------------------*
030900031110     C* EXEINZ - ROUTINE D INIZIALIZZAZIONE X OGNI NUOVO LIVELLO D ROTTURA BOLLE
031000031110     C*------------------------------------------------------------------------*
031100031110     C     EXEINZ        BEGSR
031200031110     C*
031300031110     C                   clear                   DSEDIVABW
031400031114     C                   clear                   wRMO
031500050620     C                   clear                   wNOT
031600050620     C                   clear                   wNT2
031700031110     C*
031800031110     C                   ENDSR
031900031110     C*------------------------------------------------------------------------*
032000031110     C* VALDSROT - VALORIZZAZIONE DS LIVELLO ROTTURA
032100031110     C*------------------------------------------------------------------------*
032200031112     C     VALDSROT      BEGSR
032300031110     C*
032400031110     C* Campi in rottura
032500031114     C                   EVAL      S_vabCCM = SQL_vabCCM
032600031110     C                   EVAL      S_vabRSD = SQL_vabRSD
032700031110     C                   EVAL      S_vabRD2 = SQL_vabRD2
032800031110     C                   EVAL      S_vabIND = SQL_vabIND
032900031110     C                   EVAL      S_vabCAD = SQL_vabCAD
033000031110     C                   EVAL      S_vabLOD = SQL_vabLOD
033100031110     C                   EVAL      S_vabPRD = SQL_vabPRD
033200031110     C                   EVAL      S_vabNZD = SQL_vabNZD
033300031110     C                   EVAL      S_vabLNA = SQL_vabLNA
033400031110     C                   EVAL      S_vabZNC = SQL_vabZNC
033500031110     C                   EVAL      S_vabCBO = SQL_vabCBO
033600031110     C                   EVAL      S_vabTSP = SQL_vabTSP
033700031110     C                   EVAL      S_vabCTR = SQL_vabCTR
033800031110     C                   EVAL      S_vabGC1 = SQL_vabGC1
033900031110     C                   EVAL      S_vabGC2 = SQL_vabGC2
034000031110     C                   EVAL      S_vabFFD = SQL_vabFFD
034100031110     C                   EVAL      S_vabDCR = SQL_vabDCR
034200031110     C                   EVAL      S_vabTCR = SQL_vabTCR
034300031110     C                   EVAL      S_vabHCR = SQL_vabHCR
034400031110     C                   EVAL      S_vabGMA = SQL_vabGMA
034500031110     C                   EVAL      S_vabGGA = SQL_vabGGA
034600031110     C                   EVAL      S_vabGVA = SQL_vabGVA
034700031110     C                   EVAL      S_vabTC1 = SQL_vabTC1
034800031110     C                   EVAL      S_vabTC2 = SQL_vabTC2
034900031110     C                   EVAL      S_vabVAS = SQL_vabVAS
035000031110     C                   EVAL      S_vabVAD = SQL_vabVAD
035100051021     C                   EVAL      S_vabVCA = SQL_vabVCA
035200051021     C                   EVAL      S_vabTIC = SQL_vabTIC
035300031110     C*
035400031110     C                   ENDSR
035500031110     C*------------------------------------------------------------------------*
035600031110     C* IMPVAB - VALORIZZAZIONE CAMPI "FISSI" 1° BOLLA D OGNI LIVELLO D ROTTURA
035700031110     C*------------------------------------------------------------------------*
035800031110     C     IMPVAB        BEGSR
035900031110     C*
036000031110     C* Valorizzo la DS d wrk da quella d lettura SQL
036100031110     C                   EVAL      DSEDIVABW = DSEDIVAB
036200031110     C*
036300031110     C                   ENDSR
036400031110     C*------------------------------------------------------------------------*
036500031110     C* EXESOMMA - SOMMATORIA CAMPI X RAGGRUPPAMENTO
036600031110     C*------------------------------------------------------------------------*
036700031110     C     EXESOMMA      BEGSR
036800031110     C*
036900031110     C                   EVAL      WRK_vabIAS = WRK_vabIAS + SQL_vabIAS
037000031110     C                   EVAL      WRK_vabNCL = WRK_vabNCL + SQL_vabNCL
037100031110     C                   EVAL      WRK_vabPKB = WRK_vabPKB + SQL_vabPKB
037200031110     C                   EVAL      WRK_vabVLB = WRK_vabVLB + SQL_vabVLB
037300031110     C                   EVAL      WRK_vabQFT = WRK_vabQFT + SQL_vabQFT
037400031110     C                   EVAL      WRK_vabVMD = WRK_vabVMD + SQL_vabVMD
037500051021     C                   EVAL      WRK_vabCAS = WRK_vabCAS + SQL_vabCAS
037600051021     C                   EVAL      WRK_vabCAS = WRK_vabCAS + SQL_vabCAS
037700031110     C*
037800031110     C                   ENDSR
037900031110     C*------------------------------------------------------------------------*
038000031110     C* EXENORM - NORMALIZZAZIONE CAMPI X OTTENIMENTO BOLLA "MASTER"
038100031110     C*------------------------------------------------------------------------*
038200031110     C     EXENORM       BEGSR
038300050620     C*
038400050620     C* Valorizzo indicatore d wrk
038500050620     C                   SETON                                        51
038600031110     C*
038700031114     C                   IF        WRK_vabNAS =  *blanks AND
038800031114     C                             SQL_vabNAS <> *blanks
038900031110     C                   EVAL      WRK_vabNAS = SQL_vabNAS
039000031110     C                   ENDIF
039100031110     C*
039200031114     C                   IF        WRK_vabCTM =  *blanks AND
039300031114     C                             SQL_vabCTM <> *blanks
039400031110     C                   EVAL      WRK_vabCTM = SQL_vabCTM
039500031110     C                   ENDIF
039600031110     C*
039700031114     C                   IF        WRK_vabCTS =  *blanks AND
039800031114     C                             SQL_vabCTS <> *blanks
039900031110     C                   EVAL      WRK_vabCTS = SQL_vabCTS
040000031110     C                   ENDIF
040100031110     C*
040200031114     C                   IF        WRK_vabFTM =  *blanks AND
040300031114     C                             SQL_vabFTM <> *blanks
040400031110     C                   EVAL      WRK_vabFTM = SQL_vabFTM
040500031110     C                   ENDIF
040600031110     C*
040700031114     C                   IF        WRK_vabSCL =  *blanks AND
040800031114     C                             SQL_vabSCL <> *blanks
040900031110     C                   EVAL      WRK_vabSCL = SQL_vabSCL
041000031110     C                   ENDIF
041100031110     C*
041200031110     C* Gestisco comportamento particolare del campo VABANT (tipo e numero bancali)
041300031110     C                   IF        WRK_vabANT = *zeros
041400031110     C                   EVAL      WRK_vabANT = SQL_vabANT
041500031110     C                   ELSE
041600031110     C                   IF        SQL_vabANT = *zeros
041700031110     C                   ELSE
041800031110     C                   EVAL      DSVABANT = SQL_vabANT
041900031110     C                   EVAL      wANTTIP1 = ANTTIP1
042000031110     C                   EVAL      wANTNUM1 = ANTNUM1
042100031110     C                   EVAL      wANTTIP2 = ANTTIP2
042200031110     C                   EVAL      wANTNUM2 = ANTNUM2
042300031110     C                   EVAL      DSVABANT = WRK_vabANT
042400031110     C                   IF        ANTTIP1 > *zeros
042500031110     C                   IF        wANTTIP1 = ANTTIP1
042600031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM1
042700031110     C                   ENDIF
042800031110     C                   IF        wANTTIP2 = ANTTIP1
042900031110     C                   EVAL      ANTNUM1 = ANTNUM1 + wANTNUM2
043000031110     C                   ENDIF
043100031110     C                   ELSE
043200031110     C                   IF        wANTTIP1 > *zeros
043300031110     C                   EVAL      ANTTIP1 = wANTTIP1
043400031110     C                   EVAL      ANTNUM1 = wANTNUM1
043500031110     C                   ELSE
043600031110     C                   IF        wANTTIP2 > *zeros
043700031110     C                   EVAL      ANTTIP1 = wANTTIP2
043800031110     C                   EVAL      ANTNUM1 = wANTNUM2
043900031110     C                   ENDIF
044000031110     C                   ENDIF
044100031110     C                   ENDIF
044200031110     C                   IF        ANTTIP2 > *zeros
044300031110     C                   IF        wANTTIP1 = ANTTIP2
044400031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM1
044500031110     C                   ENDIF
044600031110     C                   IF        wANTTIP2 = ANTTIP2
044700031110     C                   EVAL      ANTNUM2 = ANTNUM2 + wANTNUM2
044800031110     C                   ENDIF
044900031110     C                   ELSE
045000031110     C                   IF        wANTTIP1 > *zeros
045100031110     C                   EVAL      ANTTIP2 = wANTTIP1
045200031110     C                   EVAL      ANTNUM2 = wANTNUM1
045300031110     C                   ELSE
045400031110     C                   IF        wANTTIP2 > *zeros
045500031110     C                   EVAL      ANTTIP2 = wANTTIP2
045600031110     C                   EVAL      ANTNUM2 = wANTNUM2
045700031110     C                   ENDIF
045800031110     C                   ENDIF
045900031110     C                   ENDIF
046000031110     C                   EVAL      WRK_vabANT = DSVABANT
046100031110     C                   ENDIF
046200031110     C                   ENDIF
046300031114     C*
046400031114     C* Eseguo raggruppamento dei RMN delle testate bolle accorpate
046500050906     C                   IF        wTIPABD = 'S'
046600050906     C                   IF        wLenRIF > *zeros
046700050614     C***                MOVE(P)   SQL_vabRMN    wRMN             15
046800050614     C***                EVAL      wRIF = %trim(wRMN)
046900050614     C                   EVAL      wRIF = %trim(%editc(SQL_vabRMN:'4'))
047000050610     C                   ENDIF
047100050610     C                   IF        wTIPABD = 'A'
047200050610     C                   EVAL      wRIF = %trim(SQL_vabRMA)
047300050610     C                   ENDIF
047400050610     C*
047500050610     C* Gestisco l'allineamento d default a seconda della tipologia d riferimento da utilizzare
047600050610     C                   IF        wAlmRIF = *blanks
047700050610     C                   IF        wTipABD = 'S'
047800050610     C                   EVAL      wAlmRIF = 'D'
047900050610     C                   ENDIF
048000050610     C                   IF        wTipABD = 'A'
048100050610     C                   EVAL      wAlmRIF = 'S'
048200050610     C                   ENDIF
048300050610     C                   ENDIF
048400050614     C*
048500050614     C* Considero la minore tra la lunghezza reale del riferimento da considerare e il
048600050614     C* numero d byte da considerare indicati in tabella x il cliente corrente
048700050614     C                   Z-ADD     *zeros        wByteRIF          2 0
048800050614     C                   IF        wLenRIF <= %len(%trim(wRIF))
048900050614     C                   EVAL      wByteRIF = wLenRIF
049000050614     C                   ELSE
049100050614     C                   EVAL      wByteRIF = %len(%trim(wRIF))
049200050614     C                   ENDIF
049300050610     C*
049400050610     C* A questo punto verifico se sono richiesti solo 1 numero specifico d byte ed il
049500050610     C* relativo allineamento
049600050610     C                   IF        wLenRIF <> 99
049700050610     C                   IF        wAlmRIF = 'S'
049800050614     C                   EVAL      wRIF = %subst(%trim(wRIF):1:wByteRIF)
049900050610     C                   ENDIF
050000050610     C                   IF        wAlmRIF = 'D'
050100050610     C                   EVAL      wRIF = %subst(%trim(wRIF):
050200050614     C                                    %len(%trim(wRIF))-wByteRIF+1)
050300050610     C                   ENDIF
050400050610     C                   ENDIF
050500050610     C*
050600031114     C* Se c stanno...
050700050610     C                   IF        %len(%trim(wRMO))+1+%len(%trim(wRIF)) <=
050800040517     C                             %len(wRMO)-4
050900050610     C                   EVAL      wRMO = %trim(wRMO) + %trim(wRIF) + '-'
051000050610     C                   MOVEL     'N'           wOltreRIF         1
051100050610     C                   ELSE
051200050610     C                   MOVEL     'S'           wOltreRIF
051300031114     C                   ENDIF
051400050906     C*
051500050906     C                   ELSE
051600050906     C                   MOVEL     'N'           wOltreRIF
051700050906     C                   ENDIF
051800050620     C*
051900050620     C* Se richiesto raggruppo anche le note
052000050620     C                   IF        wRgrNOT = 'S'
052100050708     C                   EVAL      wNOT = %trim(wNOT) + ' ' + %trim(SQL_vabNOT)
052200050708     C                   EVAL      wNT2 = %trim(wNT2) + ' ' + %trim(SQL_vabNT2)
052300050620     C                   ELSE
052400050708     C                   EVAL      wNOT = %trim(SQL_vabNOT)
052500050708     C                   EVAL      wNT2 = %trim(SQL_vabNT2)
052600050620     C                   ENDIF
052700031110     C*
052800031110     C                   ENDSR
052900031112     C*------------------------------------------------------------------------*
053000031112     C* WRIVAD - SCRITTIURA FILE FIVAD
053100031112     C*------------------------------------------------------------------------*
053200031112     C     WRIVAD        BEGSR
053300031112     C*
053400031112     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAD)
053500031112     C                   CLEAR                   FIVAD000
053600031112     C*
053700031112     C* Verifico se x la bolla corrente di EDIVAB esiste giò il relativo dettaglio in EDIVAD
053800040712     C     KEYva_1       SETLL     EDIVADQT1L
053900031113     C                   IF        %equal(EDIVADQT1L)
054000040227     C* Eseguo routine x modifica codice trattamento merce
054100040227     C                   EXSR      CHGCTM
054200040227     C*
054300040712     C     KEYva_1       READE     EDIVADQT1L
054400031113     C                   DOW       not %eof(EDIVADQT1L)
054500031112     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
054600031112     C                   EVAL      vadAAS = WRK_vabAAS
054700031112     C                   EVAL      vadLNP = WRK_vabLNP
054800031112     C                   EVAL      vadNRS = WRK_vabNRS
054900031112     C                   EVAL      vadNSP = WRK_vabNSP
055000031112     C* Inserisco record d dettaglio in FIVAD
055100031112     C                   WRITE     FIVAD000
055200031112     C* Elimino record d dettaglio in EDIVAD
055300031113     C                   DELETE    EDIVADQT1L
055400031112     C* Continuo la lettura dei dettagli della testata bolla corrente
055500040712     C     KEYva_1       READE     EDIVADQT1L
055600031112     C                   ENDDO
055700031112     C*
055800031112     C* Se invece nn esiste già il dettaglio colli in EDIVAD e sulla testata c'è l'indicazioine
055900031112     C* dei segnacolli => provvedo a creare il VAD
056000031112     C                   ELSE
056100031112     C                   IF        SQL_vabNCD > *zeros OR
056200031112     C                             SQL_vabNCA > *zeros
056300040227     C* Eseguo routine x modifica codice trattamento merce
056400040227     C                   EXSR      CHGCTM
056500031112     C* Valorizzo la chiave bolla del dettaglio con quella della relativa testata bolla "master"
056600031112     C                   EVAL      vadAAS = WRK_vabAAS
056700031112     C                   EVAL      vadLNP = WRK_vabLNP
056800031112     C                   EVAL      vadNRS = WRK_vabNRS
056900031112     C                   EVAL      vadNSP = WRK_vabNSP
057000031112     C* Valorizzo i restanti campi con quelli della testata bolla corrente
057100031112     C                   EVAL      vadFGS = SQL_vabFGS
057200031112     C                   EVAL      vadCCM = SQL_vabCCM
057300031112     C                   EVAL      vadNCL = SQL_vabNCL
057400031112     C                   EVAL      vadNCD = SQL_vabNCD
057500031112     C                   EVAL      vadNCA = SQL_vabNCA
057600031112     C* Inserisco record d dettaglio in FIVAD
057700031112     C                   WRITE     FIVAD000
057800031112     C                   ENDIF
057900031112     C                   ENDIF
058000031112     C*
058100031112     C                   ENDSR
058200031114     C*------------------------------------------------------------------------*
058300031114     C* WRIVAT - SCRITTIURA FILE FIVAT
058400031114     C*------------------------------------------------------------------------*
058500031114     C     WRIVAT        BEGSR
058600031114     C*
058700031114     C* Innanzitutto svuoto il buffer del record d dettaglio d output (FIVAT)
058800031114     C                   CLEAR                   FIVAT000
058900031114     C*
059000031124     C* Verifico se x la bolla corrente di EDIVAB esiste giò il relativo dettaglio in EDIVAT
059100040712     C     KEYva_1       SETLL     EDIVATQT1L
059200031114     C                   IF        %equal(EDIVATQT1L)
059300040227     C* Eseguo routine x modifica codice trattamento merce
059400040227     C                   EXSR      CHGCTM
059500040227     C*
059600040712     C     KEYva_1       READE     EDIVATQT1L
059700031114     C                   DOW       not %eof(EDIVATQT1L)
059800031114     C* Modifico la chiave bolla del dettaglio con quella della relativa testata bolla "master"
059900031114     C                   EVAL      vatAAS = WRK_vabAAS
060000031114     C                   EVAL      vatLNP = WRK_vabLNP
060100031114     C                   EVAL      vatNRS = WRK_vabNRS
060200031114     C                   EVAL      vatNSP = WRK_vabNSP
060300031114     C* Inserisco record d dettaglio in FIVAT
060400031114     C                   WRITE     FIVAT000
060500031114     C* Elimino record d dettaglio in EDIVAT
060600031114     C                   DELETE    EDIVATQT1L
060700031114     C* Continuo la lettura dei dettagli della testata bolla corrente
060800040712     C     KEYva_1       READE     EDIVATQT1L
060900031114     C                   ENDDO
061000031114     C                   ENDIF
061100031114     C*
061200031114     C                   ENDSR
061300031112     C*------------------------------------------------------------------------*
061400031112     C* WRIVAB - SCRITTIURA FILE FIVAB X BOLLA "MASTER" ACCORPATA
061500031112     C*------------------------------------------------------------------------*
061600031112     C     WRIVAB        BEGSR
061700031112     C*
061800031112     C* Innanzitutto svuoto il buffer del record d testata d output (FIVAB)
061900031112     C                   CLEAR                   FIVAB000
062000031112     C* Valorizzo il buffer del record d testata d output (FIVAB)
062100031112     C                   EVAL      DSEDIVABOUT = DSEDIVABW
062200031112     C* Resetto alcuni campi che nella bolla "master" occcorrono NN valorizzati
062300031113     C   55              CLEAR                   vabNCD
062400031113     C   55              CLEAR                   vabNCA
062500031114     C   55              CLEAR                   vabRMO
062600031114     C   55              CLEAR                   vabCMO
062700031114     C   55              CLEAR                   vabNMO
062800050620     C                   IF        *IN55 = *on
062900050620     C* Campo RIFERIMENTO MITTENTE
063000050701     C                   IF        wRMO <> *blanks
063100040517     C                   EVAL      vabRMO = 'N'+%editw(wGiro:'  ')+':'+
063200040303     C                                      %trim(wRMO)
063300050610     C                   IF        wGiro > 1 AND wOltreRIF = 'S'
063400040514     C                   MOVE      '...'         vabRMO
063500040303     C                   ENDIF
063600050701     C                   ENDIF
063700050620     C* Campi NOTE
063800050620     C   51              EVAL      vabNOT = %trim(wNOT)
063900050620     C   51              EVAL      vabNT2 = %trim(wNT2)
064000040303     C                   ENDIF
064100031112     C* Inserisco record d testata in FIVAB
064200031112     C                   WRITE     FIVAB000
064300050620     C*
064400050620     C* Inizializzo indicatori d wrk
064500050701     C                   SETOFF                                       51
064600031112     C*
064700031112     C                   ENDSR
064800040709     C*------------------------------------------------------------------------*
064900040709     C* CHKREC - CONTROLLO CORRETTEZZA RECORD TESTATA BOLLE AI FINI DEL RAGGRUPPAMENTO
065000040709     C*------------------------------------------------------------------------*
065100040709     C     CHKREC        BEGSR
065200040709     C*
065300040709     C* Inizializzo il flag d correttezza record testata bolle
065400040709     C                   MOVEL     'S'           wRECOK            1
065500040709     C*
065600040709     C* Verifica codice trattamento
065700040709     C                   EXSR      CHKCTM
065800040709     C*
065900040709     C                   ENDSR
066000040709     C*------------------------------------------------------------------------*
066100040709     C* CHKCTM - CONTROLLO CODICE TRATTAMENTO MERCE
066200040709     C*------------------------------------------------------------------------*
066300040709     C     CHKCTM        BEGSR
066400040709     C*
066500040709     C* Verifico che il codice trattamento merce della bolla corrente nn preveda il VAX
066600040709     C                   EVAL      tblKUT = 1
066700040709     C                   EVAL      tblCOD = '1B'
066800040709     C                   EVAL      tblKEY = SQL_vabCTM
066900040709     C     KEYtab        CHAIN     TABEL00F
067000040709     C                   IF        %found(TABEL00F)
067100040709     C                   EVAL      DS1B = tblUNI
067200040709     C                   IF        §1BF16 = 'S'
067300040709     C                   MOVEL     'N'           wRECOK
067400040709     C                   ENDIF
067500040709     C                   ENDIF
067600040709     C*
067700040709     C                   ENDSR
067800040227     C*------------------------------------------------------------------------*
067900040227     C* CHGCTM - CAMBIO CODICE TRATTAMENTO MERCE
068000040227     C*------------------------------------------------------------------------*
068100040227     C     CHGCTM        BEGSR
068200040227     C*
068300040227     C* Se sulla testata bolle NN è espressamente indicato il codice tratamento merce
068400040227     C* (e naturalmente o c'è il dettaglio colli o l'indicazione in testata dei segnacolli
068500040227     C* sequenziali) imposto come default il CTM = '2D'
068600040227     C                   IF        WRK_vabCTM = *blanks
068700040227     C                   EVAL      WRK_vabCTM = '2D'
068800040227     C                   ENDIF
068900040227     C*
069000040227     C* Valorizzo la chiave x reperire le informazioni relative al codice
069100040227     C* trattamento merce da impostare in caso d raggruppamento bolle
069200040227     C                   EVAL      tblKUT = 1
069300040227     C                   EVAL      tblCOD = '1B'
069400040227     C                   EVAL      tblKEY = WRK_vabCTM
069500040227     C     KEYtab        CHAIN     TABEL00F
069600040227     C                   IF        %found(TABEL00F)
069700040227     C                   EVAL      DS1B = tblUNI
069800040227     C                   IF        §1BCTR <> *blanks
069900040227     C                   EVAL      WRK_vabCTM = §1BCTR
070000040227     C                   ENDIF
070100040227     C                   ENDIF
070200040227     C*
070300040227     C                   ENDSR
070400050610     C*------------------------------------------------------------------------*
070500051021     C* REPABD - REPERISCE I DATI TABELLATI CHE PILOTANO LE MODALITA DI ACCORPAMENTO
070600050610     C*------------------------------------------------------------------------*
070700050610     C     REPABD        BEGSR
070800050610     C*
070900050610     C                   EVAL      tblKUT = 1
071000050610     C                   EVAL      tblCOD = '3C'
071100050708     C                   MOVEL(P)  D86CCM        tblKEY
071200050610     C     KEYtab        CHAIN     TABEL00F
071300050610     C                   IF        %found(TABEL00F)
071400050610     C                   EVAL      DS3C = tblUNI
071500050610     C                   MOVEL     §3CABD        wTipABD
071600050610     C                   Z-ADD     §3CLRM        wLenRIF
071700050610     C                   MOVEL     §3CALM        wAlmRIF
071800050620     C                   MOVEL     §3CNOT        wRgrNOT
071900051021     C                   MOVEL     §3CABC        wAccCAS
072000050610     C                   ENDIF
072100050610     C*
072200050610     C                   ENDSR
072300970526     C*------------------------------------------------------------------------*
072400970526     C* *INZSR - OPERAZIONI INIZIALI
072500970526     C*------------------------------------------------------------------------*
072600970526     C     *INZSR        BEGSR
072700040714     C*
072800040714     C*  Reperisco parametri in entrata
072900040714     C     *ENTRY        PLIST
073000040714     C                   PARM                    TRTC86DS
073100031113     C*
073200031110     C*---
073300031110     C* CHIAVI DI LETTURA
073400031110     C*---
073500040709     C* CHIAVE DI LETTURA X EDIVABQT1L - Completa
073600040709     C     KEYvab1       KLIST
073700040709     C                   KFLD                    SQL_vabCCM
073800040709     C                   KFLD                    SQL_vabCMR
073900040709     C                   KFLD                    SQL_vabCNT
074000040709     C                   KFLD                    SQL_vabAAS
074100040709     C                   KFLD                    SQL_vabLNP
074200040709     C                   KFLD                    SQL_vabNRS
074300040709     C                   KFLD                    SQL_vabNSP
074400040712     C* CHIAVE DI LETTURA X EDIVADQT1L/EDIVATQT1L
074500040712     C     KEYva_1       KLIST
074600031112     C                   KFLD                    SQL_vabFGS
074700031112     C                   KFLD                    SQL_vabCCM
074800031112     C                   KFLD                    SQL_vabCMR
074900031112     C                   KFLD                    SQL_vabCNT
075000031112     C                   KFLD                    SQL_vabAAS
075100031112     C                   KFLD                    SQL_vabLNP
075200031112     C                   KFLD                    SQL_vabNRS
075300031112     C                   KFLD                    SQL_vabNSP
075400040227     C* CHIAVE DI LETTURA X TABEL00F - Completa
075500040227     C     KEYtab        KLIST
075600040227     C                   KFLD                    tblKUT
075700040227     C                   KFLD                    tblCOD
075800040227     C                   KFLD                    tblKEY
075900040714     C* CHIAVE DI LETTURA X EDIVAX2L - Completa
076000050708     C     KEYvax2_C     KLIST
076100050708     C                   KFLD                    D86CCM
076200050708     C                   KFLD                    D86CMR
076300050708     C                   KFLD                    D86CNT
076400041116     c
076500041116     C*  Richiamo XPARUT
076600041116     C                   Z-ADD     1             CODUT             1 0
076700041116     C                   CALL      'X§PARUT'
076800041116     C                   PARM                    UT§DSE
076900041116     C                   MOVEL     REC80         CNCR80
077000041116     c
077100041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
077200041116     C                   CLEAR                   TRUL06DS
077300041116     C                   MOVE      '£1'          D06COD
077400041116     C                   MOVEL     SIMFEL        D06KEY
077500041116     C                   MOVEL     'L'           D06TLA
077600041116     C                   MOVEL     TRUL06DS      KPJBU
077700041116      *
077800041116     C                   CALL      'TRUL06R'
077900041116     C                   PARM                    KPJBA
078000041116     C                   MOVEL     KPJBU         TRUL06DS
078100991112     C*
078200991112     C                   ENDSR
