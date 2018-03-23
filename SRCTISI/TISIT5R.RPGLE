000100970526     H*------------------------------------------------------------------------*
000200051227     H* Reperimento DESTINATARI PARTICOLARI
000300970526     H*------------------------------------------------------------------------*
000400160302     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900051227     FAZDST99J  IF   E           K DISK
001000070823     FAZDSE01L  IF   E           K DISK
001100970526     D*------------------------------------------------------------------------*
001200970526     D* INPUT
001300970526     D*------------------------------------------------------------------------*
001400020415     D*------------------
001500020415     D* DS "XSRDA8" - INVERSIONE DATA (8)
001600020415     D*------------------
001700020415     D WLBDA8          DS                  INZ
001800020415     D  G08DAT                 1      8  0
001900020415     D  G08INV                 9     16  0
002000020415     D  G08ERR                17     17
002100020415     D  G08TGI                18     22  0
002200020412     D*------------------
002300020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
002400020412     D*------------------
002500020412     D TISIT0DS      E DS
002600070823     D TISIT0X1DS    E DS
002700070905     D WTISIT0X1DS   E DS                  extname(TISIT0X1DS) prefix(w_)
002800070904     D*------------------
002900070904     D* DS RIDEFINIZIONE TIPI RECORD ESTENSIONI DEST. PART.
003000070904     D*------------------
003100070904     D DDSEXT1       E DS
003200020415     D*------------------
003300020415     D* DS X CONTROLLO DATI DI "INSTRADAMENTO"
003400020415     D*------------------
003500020415     D TISI95DS      E DS                  INZ
003600020415     D*------------------
003700020415     D* VARIABILI DI WRK
003800020415     D*------------------
003900020415     D wrkRICERCA      S             35    VARYING
004000040318     D wScanDST        S              1    DIM(35)
004100040318     D wScanPROP       S              1    DIM(35)
004200040318     D wValore         S              2  0
004300040318     D wValoreSav      S              2  0
004400040318     D wIndice         S              2  0
004500040318     D wIT0RAG         S                   LIKE(IT0RAG)
004600040318     D wDSTRAG         S                   LIKE(DSTRAG)
004700080508     D*------------------
004800080508     D* Costanti
004900080508     D*------------------
005000080508     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005100080508     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005200970526     C*------------------------------------------------------------------------*
005300970526     C* MAIN LINES
005400970526     C*------------------------------------------------------------------------*
005500071008     C*
005600071008     C* Ricevimento parametri
005700071008     C     *ENTRY        PLIST
005800071008     C                   PARM                    TISIT0DS
005900071008     C                   PARM                    WTISIT0X1DS
006000080508     C*
006100080508     C* Porto eventualmente da minuscolo in maiuscolo lo scambio parametri
006200080508     C     minu:maiu     XLATE     TISIT0DS      TISIT0DS
006300071008     C*
006400071008     C* Gestisco il ricevimento parametri
006500071008     C                   CLEAR                   TISIT0X1DS
006600071008     C                   CLEAR                   wNumParms         2 0
006700071008     C                   IF        %parms = 2
006800071008     C                   Z-ADD     2             wNumParms
006900071009     C                   CLEAR                   WTISIT0X1DS
007000071008     C                   ENDIF
007100060509     C*
007200060509     C                   EXSR      INZDS
007300970526     C*
007400020415     C                   IF        IT0TLA <> 'C'
007500020415     C                   EXSR      PROCEDI
007600020415     C                   ENDIF
007700020415     C*
007800970526     C* FINE PGM
007900040319     C                   IF        IT0TLA = 'E' OR
008000040319     C                             IT0TLA = 'P'
008100020415     C                   SETON                                        RT
008200020415     C                   ELSE
008300020415     C                   SETON                                        LR
008400020415     C                   ENDIF
008500970526     C*------------------------------------------------------------------------*
008600051227     C* PROCEDI - ELABORA E RESTITUISCE IL FLAG TIPO DESTINATARIO PARTICOLARE
008700970526     C*------------------------------------------------------------------------*
008800020415     C     PROCEDI       BEGSR
008900060307     C*
009000060307     C* EFFETTUO 1° TENTATIVO SENZA NORMALIZZAZIONE
009100060307     C                   EXSR      EXESEARCH
009200060307     C*
009300060307     C* SE NN TROVATO ALLORA "NORMALIZZO" L'INPUT TRAMITE TISI95 E EFFETTUO 2° TENTATIVO
009400060307     C                   IF        OT0ERR = '1'
009500020415     C                   EXSR      EXENORM
009600970526     C*
009700060307     C* QUINDI PROSEGUO CERCANDO ANCORA NELL'ANAGRAFICO DESTINATARI PARTICOLARI
009800061120     C   55              EXSR      EXESEARCH
009900060307     C                   ENDIF
010000020415     C*
010100070906     C* VALORIZZO IL BUFFER DI OUT DEI DATI DI ESTENSIONE "XT1"
010200070907     C                   IF        wNumParms = 2
010300070906     C                   EVAL      WTISIT0X1DS = TISIT0X1DS
010400070907     C                   ENDIF
010500070906     C*
010600970526     C                   ENDSR
010700020415     C*------------------------------------------------------------------------*
010800020415     C* EXENORM - CONTROLLA E "NORMALIZZA" TUTTI I DATI DI "INSTRADAMENTO"
010900020415     C*------------------------------------------------------------------------*
011000020415     C     EXENORM       BEGSR
011100061120     C*
011200061120     C* INIZIALIZZO INDICATORE D "EFFETTUATA NORMALIZZAZIONE"
011300061120     C                   SETOFF                                       55
011400020415     C*
011500020415     C                   CLEAR                   TISI95DS                       *azzera i parametri
011600020415     C                   MOVEL     *blanks       I95TLA                         *tipo lancio
011700020415     C                   MOVEL     '7'           I95TCN                         *tipo controllo
011800020415     C                   MOVEL     IT0NAZ        I95NAR                         *nazione
011900020415     C                   MOVEL     IT0CAP        I95CAP                         *cap
012000020415     C                   MOVEL     IT0LOC        I95LOC                         *località
012100020415     C                   MOVEL     IT0PRV        I95PRV                         *provincia
012200020415     C                   Z-ADD     DATCOR        I95DAT                         *data di riferimento
012300020415     C*
012400020415     C                   CALL      'TISI95R'
012500020415     C                   PARM                    TISI95DS
012600020415     C*
012700061120     C* PROSEGUO SOLO SE EFFETTIVAMENTE QUALCOSA E' STATO NORMALIZZATO
012800020415     C                   IF        IT0NAZ <> O95NAR OR
012900020415     C                             IT0CAP <> O95CAP OR
013000020415     C                             IT0PRV <> O95PRV OR
013100020415     C                             IT0LOC <> O95LOC
013200061120     C*
013300061120     C* SE LIVELLO DATI 3 O 4 "NORMALIZZO" I DATI DI "INSTRADAMENTO" INDICATI IN INPUT
013400061120     C                   IF        O95LID = '3' OR
013500061120     C                             O95LID = '4'
013600020415     C                   EVAL      IT0NAZ = O95NAR
013700020415     C                   EVAL      IT0CAP = O95CAP
013800020415     C                   EVAL      IT0PRV = O95PRV
013900020415     C                   EVAL      IT0LOC = O95LOC
014000061120     C                   SETON                                        55
014100020415     C                   ENDIF
014200061120     C*
014300061120     C* SE LIVELLO DATI 3 O 4 "NORMALIZZO" I DATI DI "INSTRADAMENTO" INDICATI IN INPUT
014400061120     C                   IF        O95LID = '2'
014500061120     C                   EVAL      IT0PRV = O95PRV
014600061120     C                   SETON                                        55
014700061120     C                   ENDIF
014800020415     C*
014900020415     C                   ENDIF
015000020415     C*
015100020415     C                   ENDSR
015200020415     C*------------------------------------------------------------------------*
015300020415     C* EXESEARCH - CERCA NEL JOIN AZDST+AZDSS
015400020415     C*------------------------------------------------------------------------*
015500020415     C     EXESEARCH     BEGSR
015600040318     C*
015700040318     C* INIZIALIZZO IL VALORE DI WRK X IL CONFRONTO RAGIONI SOCIAE PROPOSTE/DESTINATARI IN ANAG.
015800040318     C                   EVAL      wValoreSav = *zeros
015900020415     C*
016000020415     C* X SICUREZZA INIZIALIZZO OGNI VOLTA I CAMPI DI OUTPUT DELLA DS
016100020415     C                   EVAL      OT0LIV = *blanks
016200020415     C                   EVAL      OT0DOS = *blanks
016300020415     C                   EVAL      OT0ERR = *blanks
016400020415     C                   EVAL      OT0MSG = *blanks
016500020415     C*
016600020415     C* MI POSIZIONO X PORZIONE CHIAVE UGUALE AD INPUT NORMALIZZATO
016700060307     C***                EVAL      KDSNAZ = O95NAR
016800060307     C***                EVAL      KDSPRV = O95PRV
016900060307 xxx C******             EVAL      KDSCAP = O95CAP
017000060307     C***                EVAL      KDSLOC = O95LOC
017100060307     C                   EVAL      KDSNAZ = IT0NAZ
017200060307     C                   EVAL      KDSPRV = IT0PRV
017300060307     C                   EVAL      KDSLOC = IT0LOC
017400051227     C     KEYDST99JP    SETLL     AZDST99J
017500051227     C     KEYDST99JP    READE     AZDST99J
017600051227     C                   DOW       not %eof(AZDST99J)
017700020415     C*
017800020415     C* 1° LIVELLO DATI
017900020415     C                   IF        IT0RAG = DSTRAG AND
018000021223     C                             DSTRAG <> '*'   AND
018100021223     C                             DSTATB = *blanks
018200020415     C                   EVAL      %len(wrkRICERCA) = %len(%trim(DSTKWI))
018300020415     C                   EVAL      wrkRICERCA = %trim(DSTKWI)
018400020415     C     wrkRICERCA    SCAN      IT0IND
018500020415     C                   IF        %found
018600020415     C                   IF        OT0LIV = *blanks OR
018700020415     C                             OT0LIV > '1'
018800020415     C                   EVAL      DEPLIV = '1'
018900020415     C                   EXSR      OUTDS
019000040319     C* SE GIA' HO TROVATO UN LIVELLO 1 ESCO DAL CICLO IN QUANTO TRATTASI GIA'
019100040319     C* DEL LIVELLO + AFFIDABILE'
019200040319     C                   LEAVE
019300020415     C                   ENDIF
019400020415     C                   ENDIF
019500020415     C                   ENDIF
019600020415     C*
019700040319     C* 2° LIVELLO DATI
019800040416     C                   IF        IT0RAG = DSSSIN AND
019900040416     C                             DSSATB <> 'A'
020000020415     C                   EVAL      %len(wrkRICERCA) = %len(%trim(DSTKWI))
020100020415     C                   EVAL      wrkRICERCA = %trim(DSTKWI)
020200020415     C     wrkRICERCA    SCAN      IT0IND
020300020415     C                   IF        %found
020400020415     C                   IF        OT0LIV = *blanks OR
020500020415     C                             OT0LIV > '2'
020600020415     C                   EVAL      DEPLIV = '2'
020700020415     C                   EXSR      OUTDS
020800020415     C                   ENDIF
020900020415     C                   ENDIF
021000020415     C                   ENDIF
021100040319     C*
021200040319     C* 3° LIVELLO DATI
021300040416     C                   IF        DSSATB <> 'A'
021400040319     C     '*'           SCAN      DSSSIN        wPosAstRagSSin    2 0
021500040319     C                   IF        %found
021600040319     C                   IF        %subst(IT0RAG:1:wPosAstRagSSin-1) =
021700040319     C                             %subst(DSSSIN:1:wPosAstRagSSin-1)
021800040319     C                   EVAL      %len(wrkRICERCA) = %len(%trim(DSTKWI))
021900040319     C                   EVAL      wrkRICERCA = %trim(DSTKWI)
022000040319     C     wrkRICERCA    SCAN      IT0IND
022100040319     C                   IF        %found
022200040319     C                   IF        OT0LIV = *blanks OR
022300040319     C                             OT0LIV > '3'
022400040319     C                   EVAL      DEPLIV = '3'
022500040319     C                   EXSR      OUTDS
022600040319     C                   ENDIF
022700040319     C                   ENDIF
022800040319     C                   ENDIF
022900040319     C                   ENDIF
023000040416     C                   ENDIF
023100040319     C******
023200040319     C* I LIVELLI DATI CHE PREVEDONO '*' NELLA KEYWORD INDIRIZZO D FATTO NN VERRANNO PIU'
023300040319     C* GESTITI IN QUANTO IN INSERIMENTO ANAGRAFICA NN E' + CONSENTITO INDICARE '*' NELLA KWI
023400040319     C******
023500020415     C*
023600040319     C* 4° LIVELLO DATI
023700020415     C                   IF        IT0RAG = DSTRAG AND
023800021223     C                             DSTKWI = '*'    AND
023900021223     C                             DSTATB = *blanks
024000020415     C                   IF        OT0LIV = *blanks OR
024100040319     C                             OT0LIV > '4'
024200040319     C                   EVAL      DEPLIV = '4'
024300020415     C                   EXSR      OUTDS
024400020415     C                   ENDIF
024500020415     C                   ENDIF
024600020415     C*
024700040319     C* 5° LIVELLO DATI
024800040416     C                   IF        IT0RAG =  DSSSIN AND
024900040416     C                             DSSATB <> 'A'    AND
025000030129     C                             DSTKWI = '*'
025100020415     C                   IF        OT0LIV = *blanks OR
025200040319     C                             OT0LIV > '5'
025300040319     C                   EVAL      DEPLIV = '5'
025400020415     C                   EXSR      OUTDS
025500020415     C                   ENDIF
025600020415     C                   ENDIF
025700040319     C******
025800020415     C*
025900040319     C* 6° LIVELLO DATI
026000021223     C                   IF        DSTRAG = '*' AND
026100021223     C                             DSTATB = *blanks
026200020415     C                   EVAL      %len(wrkRICERCA) = %len(%trim(DSTKWI))
026300020415     C                   EVAL      wrkRICERCA = %trim(DSTKWI)
026400020415     C     wrkRICERCA    SCAN      IT0IND
026500020415     C                   IF        %found
026600020415     C                   IF        OT0LIV = *blanks OR
026700040319     C                             OT0LIV > '6'
026800040319     C                   EVAL      DEPLIV = '6'
026900020415     C                   EXSR      OUTDS
027000020415     C                   ENDIF
027100020415     C                   ENDIF
027200020415     C                   ENDIF
027300020416     C*
027400020423     C* 9° LIVELLO DATI
027500021223     C                   IF        DSTATB = *blanks
027600040319     C* Solo x lancio d tipo 'P' = Proposte...
027700040319     C* effettuo verifiche x stabilire maggior somiglianza ragioni sociali proposta/dest. in anag.
027800040319     C                   IF        IT0TLA = 'P'
027900040318     C                   EVAL      wIT0RAG = %trim(IT0RAG)
028000040318     C                   EVAL      wDSTRAG = %trim(DSTRAG)
028100040318     C                   MOVEA     wIT0RAG       wScanPROP
028200040318     C                   MOVEA     wDSTRAG       wScanDST
028300040318     C                   EVAL      wValore = *zeros
028400040318     C                   EVAL      wIndice = 1
028500040318     C                   DOW       wIndice <= %elem(wScanPROP)
028600040318     C                   IF        wScanPROP(wIndice) <> *blanks AND
028700040318     C                             wScanDST(wIndice)  <> *blanks
028800040318     C                   IF        wScanPROP(wIndice) = wScanDST(wIndice)
028900040318     C                   EVAL      wValore = wValore + 1
029000040318     C                   ELSE
029100040318     C                   LEAVE
029200040318     C                   ENDIF
029300040318     C                   ENDIF
029400040318     C                   EVAL      wIndice = wIndice + 1
029500040318     C                   ENDDO
029600040319     C                   ENDIF
029700040318     C* Effettuo verifiche x congrità indirizzo proposta con keyword indirizzo destinatario in anag.
029800020416     C                   EVAL      %len(wrkRICERCA) = %len(%trim(DSTKWI))
029900020416     C                   EVAL      wrkRICERCA = %trim(DSTKWI)
030000020416     C     wrkRICERCA    SCAN      IT0IND
030100040319     C                   IF        %found and wValore >= wValoreSav
030200040318     C                   EVAL      wValoreSav = wValore
030300040318     C                   IF        OT0LIV  = *blanks OR
030400040318     C                             OT0LIV >= '9'
030500020423     C                   EVAL      DEPLIV = '9'
030600020416     C                   EXSR      OUTDS
030700020416     C                   ENDIF
030800020416     C                   ENDIF
030900021223     C                   ENDIF
031000020415     C*
031100051227     C     KEYDST99JP    READE     AZDST99J
031200020415     C                   ENDDO
031300020415     C*
031400020415     C* SE QUANDO FINISCO DI SCORRERE LA CHIAVE PARZIALE NN HO TROVATO NULLA => ERRORE
031500020603     C                   IF        OT0LIV = *blanks OR
031600020603     C                             OT0LIV = '9'
031700020415     C                   EVAL      OT0ERR = '1'
031800020415     C                   EVAL      OT0MSG = 'Il destinatario non è ' +
031900051227     C                                      'riconosciuto come particolare.'
032000020415     C                   ENDIF
032100020415     C*
032200020415     C                   ENDSR
032300020415     C*------------------------------------------------------------------------*
032400020415     C* OUTDS - IMPOSTO I CAMPI DI OUTPUT DELLA DS DI PROCEDURA
032500020415     C*------------------------------------------------------------------------*
032600020415     C     OUTDS         BEGSR
032700020415     C*
032800041011     C* Innanzitutto x evitare che rimangano valorizzazioni precedenti inizializzo i campi d OUT
032900041011     C                   EVAL      OT0LIV = *blanks
033000041011     C                   EVAL      OT0DOS = *blanks
033100041011     C                   EVAL      OT0COD = *zeros
033200041011     C                   EVAL      OT0NAZ = *blanks
033300041011     C                   EVAL      OT0PRV = *blanks
033400041011     C                   EVAL      OT0CAP = *blanks
033500041011     C                   EVAL      OT0LOC = *blanks
033600041011     C                   EVAL      OT0IND = *blanks
033700041011     C                   EVAL      OT0KWI = *blanks
033800041011     C                   EVAL      OT0RAG = *blanks
033900041011     C                   EVAL      OT0TEL = *blanks
034000041011     C                   EVAL      OT0REF = *blanks
034100041011     C                   EVAL      OT0SIN = *blanks
034200051227     C                   EVAL      OT0FLO = *blanks
034300041011     C                   EVAL      OT0MSG = *blanks
034400041011     C*
034500020502     C                   EVAL      OT0COD = DSTCOD
034600020415     C                   EVAL      OT0DOS = DSTDOS
034700020415     C                   EVAL      OT0LIV = DEPLIV
034800020415     C                   EVAL      OT0MSG = DSTNOT
034900020415     C                   EVAL      OT0NAZ = DSTNAZ
035000020415     C                   EVAL      OT0PRV = DSTPRV
035100020415     C                   EVAL      OT0CAP = DSTCAP
035200020415     C                   EVAL      OT0LOC = DSTLOC
035300020415     C                   EVAL      OT0IND = DSTIND
035400020416     C                   EVAL      OT0KWI = DSTKWI
035500040609     C                   IF        DSTTEL <> *blanks
035600040609     C                   EVAL      OT0TEL = '#' + DSTTEL
035700040609     C                   ENDIF
035800040609     C                   IF        DSTREF <> *blanks
035900040609     C                   EVAL      OT0REF = '#' + DSTREF
036000040609     C                   ENDIF
036100020415     C                   EVAL      OT0RAG = DSTRAG
036200020415     C                   EVAL      OT0SIN = DSSSIN
036300051227     C                   EVAL      OT0FLO = DSTFLO
036400070906     C*
036500070906     C* Inizializzazione ds dati estensioni X1
036600070906     C                   CLEAR                   TISIT0X1DS
036700070904     C*
036800070904     C* In fase di valorizzazione DS OUT efffettuo reperimento dati estensioni
036900070904     C                   EVAL      DSETRK = 'XT1'
037000070904     C     KEYDSE01LC    CHAIN     AZDSE01L
037100070904     C                   IF        %found(AZDSE01L)
037200070905     C                   EVAL      DDSEXT1     = DSEUNI
037300070905     C                   EVAL      OT0X1POG    = §DSEXT1POG
037400070905     C                   EVAL      OT0X1CGI    = §DSEXT1CGI
037500070905     C                   EVAL      OT0X1GC1    = §DSEXT1GC1
037600070905     C                   EVAL      OT0X1GC2    = §DSEXT1GC2
037700090707     C                   EVAL      OT0X1TICF   = §DSEXT1TIC
037800070904     C                   ENDIF
037900020415     C*
038000020415     C                   ENDSR
038100060509     C*------------------------------------------------------------------------*
038200060509     C* INZDS - INIZIALIZZA I CAMPI DI OUTPUT DELLA DS DI PROCEDURA
038300060509     C*------------------------------------------------------------------------*
038400060509     C     INZDS         BEGSR
038500060509     C*
038600060509     C                   EVAL      OT0LIV = *blanks
038700060509     C                   EVAL      OT0DOS = *blanks
038800060509     C                   EVAL      OT0COD = *zeros
038900060509     C                   EVAL      OT0NAZ = *blanks
039000060509     C                   EVAL      OT0PRV = *blanks
039100060509     C                   EVAL      OT0CAP = *blanks
039200060509     C                   EVAL      OT0LOC = *blanks
039300060509     C                   EVAL      OT0IND = *blanks
039400060509     C                   EVAL      OT0KWI = *blanks
039500060509     C                   EVAL      OT0RAG = *blanks
039600060509     C                   EVAL      OT0TEL = *blanks
039700060509     C                   EVAL      OT0REF = *blanks
039800060509     C                   EVAL      OT0SIN = *blanks
039900060509     C                   EVAL      OT0FLO = *blanks
040000060509     C                   EVAL      OT0MSG = *blanks
040100070904     C*
040200070904     C* Inizializzo DS ritorno dati da estensione X1
040300070904     C                   CLEAR                   DDSEXT1
040400060509     C*
040500060509     C                   ENDSR
040600020415     C*------------------------------------------------------------------------*
040700020415     C* FINSR - OPERAZIONI FINALI
040800020415     C*------------------------------------------------------------------------*
040900020415     C     FINSR         BEGSR
041000020415     C*
041100020415     C                   CLEAR                   TISI95DS                       *azzera i parametri
041200020415     C                   MOVEL     'C'           I95TLA                         *tipo lancio
041300020415     C*
041400020415     C                   CALL      'TISI95R'
041500020415     C                   PARM                    TISI95DS
041600020415     C*
041700020415     C                   ENDSR
041800970526     C*------------------------------------------------------------------------*
041900970526     C* *INZSR - OPERAZIONI INIZIALI
042000970526     C*------------------------------------------------------------------------*
042100970526     C     *INZSR        BEGSR
042200020415     C*---
042300020415     C* VARIABILI RIFERITE AL DATA BASE
042400020415     C*---
042500020415     C     *LIKE         DEFINE    DSTTIP        KDSTIP
042600020415     C     *LIKE         DEFINE    DSTNAZ        KDSNAZ
042700020415     C     *LIKE         DEFINE    DSTPRV        KDSPRV
042800040114 xxx C***  *LIKE         DEFINE    DSTCAP        KDSCAP
042900020415     C     *LIKE         DEFINE    DSTLOC        KDSLOC
043000020415     C     *LIKE         DEFINE    DSTRAG        KDSRAG
043100020415     C     *LIKE         DEFINE    DSTKWI        KDSKWI
043200020415     C     *LIKE         DEFINE    DSTDOS        DEPDOS
043300020415     C     *LIKE         DEFINE    OT0LIV        DEPLIV
043400020415     C*---
043500020415     C* INIZIALIZZAZIONI
043600020415     C*---
043700020415     C                   EVAL      KDSTIP = 'C'
043800000407     C*---
043900000407     C* CHIAVI DI LETTURA
044000000407     C*---
044100051227     C* LETTURA AZDST99J - PARZIALE
044200051227     C     KEYDST99JP    KLIST
044300020415     C                   KFLD                    KDSTIP                         *TIPO
044400020415     C                   KFLD                    KDSNAZ                         *NAZIONE
044500020415     C                   KFLD                    KDSPRV                         *PROVINCIA
044600020415     C                   KFLD                    KDSLOC                         *LOCALITA
044700040114 xxx C***                KFLD                    KDSCAP                         *CAP
044800070904     C*
044900070904     C* LETTURA AZDSE01L - COMPLETA
045000070904     C     KEYDSE01LC    KLIST
045100070904     C                   KFLD                    DSTCOD                         *CODICE DEST. PART.
045200070904     C                   KFLD                    DSETRK                         *TIPO RECORD
045300020415     C*
045400020415     C* CALCOLA LA DATA CORRENTE
045500020415     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
045600020415     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
045700020415     C                   Z-ADD     WN8           G08DAT
045800020415     C                   Z-ADD     *ZEROS        G08INV
045900020415     C                   MOVEL     '0'           G08ERR
046000020415     C                   CALL      'XSRDA8'
046100020415     C                   PARM                    WLBDA8
046200020415     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
046300970526     C*
046400970526     C                   ENDSR
