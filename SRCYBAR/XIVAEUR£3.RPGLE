000100000000     H
000200000000     F*-------------------------------------------------------------*
000300940824     F*- Controllo P.Iva Europea                            *
000400940824     F*-------------------------------------------------------------*
000500050726      *
000600050726      * ATTENZIONE al programma ora vengono passati 2 parametri
000700050726      *            il secondo viene passato solo da alcuni programmi
000800050907      *            questo secondo parametro serve per il controllo
000900050726      *            della partita iva di un cliente di San Marino
001000050726      *            il secondo parametro passato contiene la provincia
001100040115      *
001200941003     F* ERRORI:
001300061106     F* IVAFLG = -9 ERRORE ANCHE FORZABILE-->COD ISO P.IVA INCONGR NAZ
001400941003     F* IVAFLG = -5 ERRORE NON   FORZABILE-->NAZIONE ERRATA
001500941003     F* IVAFLG = -3 ERRORE NON   FORZABILE-->MANCA RECORD P.IVA IN DS4I
001600941003     F* IVAFLG = -2 ERRORE NON   FORZABILE-->ERRATA LA LUNGHEZZA
001700061106     F* IVAFLG = -1 ERRORE NON   FORZABILE--> ERRATA LA FORMALITA'
001800941003     F*
001900000000     F*-------------------------------------------------------------*
002000940228     FTABEL00F  IF   E           K DISK
002100940228     D*-------------------------------------------------------------*
002200940228     D*- Schiera per ricerca lunghezza della stringa               -*
002300940228     D*-------------------------------------------------------------*
002400940823     D EIVA            S              1    DIM(14)
002500950616     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
002600950717     D K78             S              1    DIM(78)
002700940228     D*-------------------------------------------------------------*
002800940301     D XIVAEU        E DS
002900940301     D*
003000940930     D DS15          E DS
003100940930     D DS4I          E DS
003200050726
003300050726     d wpos            s              2  0
003400940301     C*-------------------------------------------------------------*
003500940301     C*  Ciclo Principale
003600940301     C*-------------------------------------------------------------*
003700940301     C     *ENTRY        PLIST
003800940301     C                   PARM                    XIVAEU
003900050726     c                   Parm                    xivaprv           2
004000941003     C*
004100941003     C                   CLEAR                   IVAFLG
004200941003     C                   CLEAR                   IVAMSG
004300941003     C*
004400941003     C* TIPO LANCIO = "C"           CHIUSO CON LR IL FILE
004500941003     C*             = "L" ELABORO E CHIUDO CON LR
004600941003     C*             = " " ELABORO E CHIUDO CON RETRN
004700940930     C*
004800941003    1C     IVATLA        IFEQ      'C'
004900941003     C                   SETON                                        LR
005000941003   X1C                   ELSE
005100941003     C*
005200940930     C* REPERISCO DALLA TABELLA NAZIONE SE E' NAZIONE ITALIA
005300940930     C                   MOVE      '15'          TBWCOD
005400941003     C                   MOVEL(P)  IVANAR        TBWKEY
005500940930     C*
005600940930     C     KEYTAB        CHAIN     TABEL00F                           80
005700941003     C* ERRORE --> NAZIONE ERRATA
005800941003    2C     *IN80         IFEQ      *ON
005900941003     C                   MOVEL     MSG(5)        IVAMSG
006000941003     C                   Z-SUB     5             IVAFLG
006100941003     C                   GOTO      ENDCTR
006200941003     C                   ELSE
006300941003     C                   MOVEL     TBLUNI        DS15
006400941003    2C                   ENDIF
006500950116     C*
006600950116     C* SE IVASTA ='$$'   ----> NESSUN CONTROLLO
006700950616     C     IVASTA        IFEQ      '$$'
006800061123     C**!!!�15ISO        OREQ      '$$'
006900950616     C* SE PRIVATO LA P.IVA DEVE ESSERE VUOTA
007000950626     c* o uguale alla scritta privato
007100950616     C     IVAPAR        IFNE      *BLANKS
007200950626     C     'PRIVATO '    SCAN      IVAPAR                                 99
007300950626     C     *IN99         IFEQ      *OFF
007400950616     C                   MOVEL     MSG(6)        IVAMSG
007500950616     C                   Z-SUB     6             IVAFLG
007600950616    2C                   ENDIF
007700950626    2C                   ENDIF
007800950616     C                   GOTO      ENDCTR
007900950616    2C                   ENDIF
008000050726
008100050726      * se passato anche il secondo parametro (provincia) controllo se p.i. di San Marino
008200050726      * in questo caso non devo fare nessun controllo
008300050726if  2c                   If        %Parms > 1
008400050726      * nazione italia
008500050726     c                             and ivanar = *Blanks
008600050726      * provincia FO o RN
008700061025     c                             and (xivaprv = 'FO' or xivaprv = 'RN'
008800061025     c                                  or xivaprv = 'FC')
008900050726      * controllo se nella partita iva c'� la sigla SM x San Marino
009000050726     c                   Clear                   wpos
009100050726     c                   Eval      wpos = %scan('SM':ivapar)
009200050907      * trovato SM controllo se la lunghezza � giusta
009300070426if  3c                   If        wpos > *Zeros or ivasta = 'SM'
009400050907     c                   ExSr      Sr_Ctrsm
009500050907      * vado sempre a fine controlli
009600050907     c                   Goto      endctr
009700070426    3c                   EndIf
009800050726    2c                   EndIf
009900950116     C*
010000950116     C* SE NON IMMESSO IL CODICE ISO LO PRENDO
010100950116     C     IVASTA        IFEQ      *BLANKS
010200950116    2C     �15ITA        ANDNE     'I'
010300950116     C                   MOVEL     �15ISO        IVASTA
010400950116    2C                   ENDIF
010500941121     C*
010600941121     C*  Reperisco sulla tabella la lunghezza della stringa
010700941121     C                   CLEAR                   DS4I
010800941121     C                   MOVE      '4I'          TBWCOD
010900941121     C                   MOVEL(P)  IVASTA        TBWKEY
011000941121     C     KEYTAB        CHAIN     TABEL00F                           80
011100941121     C*
011200941121     C* ERRORE --> NON ESISTE LA TABELLA DELLA P.IVA
011300941121    2C     *IN80         IFEQ      *ON
011400941121     C     TBLFLG        ORNE      *BLANKS
011500941121     C                   MOVEL     MSG(3)        IVAMSG
011600941121     C                   Z-SUB     3             IVAFLG
011700941121     C                   GOTO      ENDCTR
011800941121   x2C                   ELSE
011900941121     C                   MOVEL     TBLUNI        DS4I
012000941121    2C                   ENDIF
012100941122     C*
012200940301     C                   MOVEA     IVAPAR        EIVA
012300940824     C*
012400940301     C*  Determino la lunghezza effettiva della partita IVA
012500940301     C                   Z-ADD     1             X                 2 0
012600940301     C     ' '           LOOKUP    EIVA(X)                                40
012700941003    3C     *IN40         IFEQ      '0'
012800940823     C                   Z-ADD     14            LUNGH             2 0
012900940301     C                   ELSE
013000940301     C     X             SUB       1             LUNGH
013100941003    3C                   END
013200940823     C*
013300940301     C*  Se la lunghezza effettiva della stringa � diversa da
013400940301     C*  quella prevista gestisco errore
013500941003    3C     LUNGH         IFNE      �4ICHR
013600040630    3C     LUNGH         ANDNE     �4ICH2
013700040630    3C     LUNGH         ANDNE     �4ICH3
013800071102     c* non accetto lunghezza =0
013900071102     c     LUNGH         oreq      0
014000071102     c
014100950717     C                   MOVEA     MSG(2)        K78
014200950717     C                   MOVEL     �4ICHR        W002A             2
014300950717     C                   MOVEA     W002A         K78(47)
014400950717     C                   MOVEA     K78           IVAMSG
014500040630     C                   EVAL      IVAMsg = 'Il codice IVA della nazione ' +
014600040630     C                             IVASta + ' pu� essere lungo ' +
014700040630     C                             %TRIML(%EDITC(�4IChr:'Z'))
014800040630     C                   IF        �4ICh2 > 0
014900040630     C                   EVAL      IVAMsg = %TRIMR(IVAMsg) + ', ' +
015000040630     C                             %TRIML(%EDITC(�4ICh2:'Z'))
015100040630     C                   ENDIF
015200040630     C                   IF        �4ICh3 > 0
015300040630     C                   EVAL      IVAMsg = %TRIMR(IVAMsg) + ', ' +
015400040630     C                             %TRIML(%EDITC(�4ICh3:'Z'))
015500040630     C                   ENDIF
015600040630     C                   EVAL      IVAMsg = %TRIMR(IVAMsg) + ' caratteri.'
015700941003     C                   Z-SUB     2             IVAFLG
015800941003     C                   GOTO      ENDCTR
015900941003   X3C                   ELSE
016000940823     C*
016100940823     C* SE P.IVA ITALIANA ESEGUO ANCHE IL CONTROLLO FORMALE
016200061113    4C**!!!�15ITA        IFEQ      'I'
016300061113      * controllo il codice iso e non la nazione italia perch� posso inserire una
016400061113      * partita iva straniera su cliente italia
016500061113    4c     �4iITA        IFEQ      'I'
016600940823     C                   MOVEL     IVAPAR        XPIVA
016700940824     C                   CLEAR                   XSTATO
016800940823     C*
016900981124     C                   CALL      'XPARIVAO'
017000940823     C                   PARM                    XPIVA            16
017100940824     C                   PARM                    XSTATO            1 0
017200940824     C*
017300940824     C                   Z-ADD     XSTATO        IVAFLG
017400941003     C* ERRORE --> P.IVA ITALIANA FORMALMENTE ERRATA
017500941003    5C     IVAFLG        IFEQ      -1
017600941003     C                   MOVEL     MSG(1)        IVAMSG
017700941003     C                   GOTO      ENDCTR
017800941003    5C                   ENDIF
017900941003    4C                   ENDIF
018000941003     C*
018100941003    3C                   ENDIF
018200040115     C*
018300040115     C* SE CODICE ISO DELLA P.IVA INCONGRUENTE CON CODICE ISO DELLA NAZ
018400040115     C*  --> ERRORE FORZABILE
018500040115    3C     �15ISO        IFNE      IVASTA
018600040115     C     �4IITA        ANDNE     �15ITA
018700061123     c     �15iso        oreq      '$$'
018800040115     C                   MOVEL     MSG(4)        IVAMSG
018900061123     c                   eval      %subst(ivamsg:60:2) = �15iso
019000061106     C                   Z-SUB     9             IVAFLG
019100040115     C                   GOTO      ENDCTR
019200040115    3C                   ENDIF
019300941003     C*
019400941003     C     ENDCTR        TAG
019500940823     C*
019600941003     C* SE IVATLA = "L" CHIUDO CON LR
019700941003    2C     IVATLA        IFEQ      'L'
019800941003     C                   SETON                                        LR
019900941003   X2C                   ELSE
020000941003     C                   RETURN
020100941003    2C                   ENDIF
020200941003    1C                   ENDIF
020300050907
020400050907      *-------------------------------------------------------------*
020500050907      * Controllo la lunghezza della partita iva di S.Marino
020600050907      *-------------------------------------------------------------*
020700050907     c     Sr_Ctrsm      BegSr
020800050907
020900050907      * Aggancio la tabella 4I per recuperare la lunghezza
021000050907     c                   Clear                   ds4i
021100050907     c                   Eval      tbwcod = '4I'
021200050907     c                   Eval      tbwkey = 'SM'
021300050907     c     keytab        Chain     Tabel00f
021400050907     c                   If        %Found(Tabel00f) and tblflg = *Blanks
021500050907     c                   Eval      ds4i = tbluni
021600050907      * Se non trovo la tabella o se � annullata chiodo la lunghezza a 5
021700050907     c                   Else
021800050907     c                   Eval      �4ichr = 5
021900050907     c                   EndIf
022000050907
022100050907     c                   Movea     ivapar        eiva
022200050907
022300050907      * Determino la lunghezza effettiva della partita iva
022400050907     c                   Eval      x = 1
022500050907     c     ' '           Lookup    eiva(x)                                40
022600050907      * non c'� nessun campo vuoto imposto la lunghezza massima del campo
022700050907      * meno 2 x SM
022800050907     c                   If        Not *In40
022900050907     c                   Eval      Lungh = 12
023000050907      * ci sono dei campi vuoti tolgo 3 campi xch� 2 sono x SM
023100050907     c                   Else
023200070426      * se nel cod. ISO c'� SM non devo togliere 3 (SM � da un'altra parte)
023300070426      * tolgo 1
023400070426     c                   if        ivasta = 'SM'
023500070426     c     x             sub       1             lungh
023600070426     c                   else
023700050907     c     x             sub       3             lungh
023800070426     c                   endif
023900050907     c                   EndIf
024000050907
024100050907      * Se la lunghezza effettiva della stringa � diversa da quella prevista
024200050907      * gestisco l'errore
024300050907     c                   If        lungh <> �4ichr and lungh <> �4ich2 and
024400050907     c                             lungh <> �4ich3
024500050907     c                   Movea     msg(2)        k78
024600050907     c                   Movel     �4ichr        w002a
024700050907     c                   Movea     w002a         k78(47)
024800050907     c                   Movea     k78           ivamsg
024900050908     c                   Eval      ivamsg = 'La partita IVA di San Marino deve-
025000050908     c                              essere: SM pi� ' +
025100050907     c                             %triml(%editc(�4ichr:'Z'))
025200050907     c                   If        �4iCh2 > 0
025300050907     c                   Eval      ivamsg = %trimr(ivamsg) + ', ' +
025400050907     c                             %triml(%editc(�4ich2:'Z'))
025500050907     c                   EndIf
025600050907     c                   If        �4ich3 > 0
025700050907     c                   Eval      ivamsg = %trimr(ivamsg) + ', ' +
025800050907     c                             %triml(%editc(�4ich3:'Z'))
025900050907     c                   EndIf
026000050907     c                   Eval      ivamsg = %trimr(ivamsg) + ' caratteri.'
026100050907     c                   Z-sub     2             ivaflg
026200050907     c                   EndIf
026300050907
026400050907     c                   EndSr
026500050907
026600940301     C*-------------------------------------------------------------*
026700940301     C     *INZSR        BEGSR
026800940301     C*-------------------------------------------------------------*
026900940301     C*  Definizione della chiave di accesso alla tabella 4I
027000940301     C     KEYTAB        KLIST
027100940301     C                   KFLD                    TBWKUT
027200940301     C                   KFLD                    TBWCOD
027300940301     C                   KFLD                    TBWKEY
027400940301     C*  Definizione Varibili
027500940301     C     *LIKE         DEFINE    TBLKUT        TBWKUT
027600940301     C     *LIKE         DEFINE    TBLCOD        TBWCOD
027700940301     C     *LIKE         DEFINE    TBLKEY        TBWKEY
027800940301     C*  Inizializzazione
027900940301     C                   Z-ADD     1             TBWKUT
028000940301     C                   ENDSR
028100950626**
028200941003Partita IVA italiana formalmente errata
028300950717Lunghezza Partita IVA errata: e' composta di  xx caratteri
028400941003Codice ISO inesistente o non prevista tabella della Partita IVA
028500061106Cod.ISO della P.IVA incongruente con cod.ISO della nazione
028600941003Nazione Errata
028700950626Se Privato -codice ISO $$ -lasciare Part.IVA in bianco o uguale a "PRIVATO"
