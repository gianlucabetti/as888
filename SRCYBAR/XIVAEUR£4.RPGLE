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
002500080311     D MSG             S             78    DIM(7) CTDATA PERRCD(1)
002600950717     D K78             S              1    DIM(78)
002700940228     D*-------------------------------------------------------------*
002800940301     D XIVAEU        E DS
002900940301     D*
003000940930     D DS15          E DS
003100940930     D DS4I          E DS
003200050726
003300050726     d wpos            s              2  0
003400080311     d wforzaITA       s              1
003500940301     C*-------------------------------------------------------------*
003600940301     C*  Ciclo Principale
003700940301     C*-------------------------------------------------------------*
003800940301     C     *ENTRY        PLIST
003900940301     C                   PARM                    XIVAEU
004000050726     c                   Parm                    xivaprv           2
004100080311     c                   Parm                    xforzaITA         1
004200080311     c
004300080311     c                   if        %parms=3
004400080311     c                   eval      wforzaita=xforzaita
004500080311     c                   else
004600080311     c                   eval      Wforzaita=' '
004700080311     c                   endif
004800941003     C*
004900941003     C                   CLEAR                   IVAFLG
005000941003     C                   CLEAR                   IVAMSG
005100941003     C*
005200941003     C* TIPO LANCIO = "C"           CHIUSO CON LR IL FILE
005300941003     C*             = "L" ELABORO E CHIUDO CON LR
005400941003     C*             = " " ELABORO E CHIUDO CON RETRN
005500940930     C*
005600941003    1C     IVATLA        IFEQ      'C'
005700941003     C                   SETON                                        LR
005800941003   X1C                   ELSE
005900941003     C*
006000940930     C* REPERISCO DALLA TABELLA NAZIONE SE E' NAZIONE ITALIA
006100940930     C                   MOVE      '15'          TBWCOD
006200941003     C                   MOVEL(P)  IVANAR        TBWKEY
006300940930     C*
006400940930     C     KEYTAB        CHAIN     TABEL00F                           80
006500941003     C* ERRORE --> NAZIONE ERRATA
006600941003    2C     *IN80         IFEQ      *ON
006700941003     C                   MOVEL     MSG(5)        IVAMSG
006800941003     C                   Z-SUB     5             IVAFLG
006900941003     C                   GOTO      ENDCTR
007000941003     C                   ELSE
007100941003     C                   MOVEL     TBLUNI        DS15
007200941003    2C                   ENDIF
007300950116     C*
007400950116     C* SE IVASTA ='$$'   ----> NESSUN CONTROLLO
007500950616     C     IVASTA        IFEQ      '$$'
007600061123     C**!!!§15ISO        OREQ      '$$'
007700950616     C* SE PRIVATO LA P.IVA DEVE ESSERE VUOTA
007800950626     c* o uguale alla scritta privato
007900950616     C     IVAPAR        IFNE      *BLANKS
008000950626     C     'PRIVATO '    SCAN      IVAPAR                                 99
008100950626     C     *IN99         IFEQ      *OFF
008200950616     C                   MOVEL     MSG(6)        IVAMSG
008300950616     C                   Z-SUB     6             IVAFLG
008400950616    2C                   ENDIF
008500950626    2C                   ENDIF
008600950616     C                   GOTO      ENDCTR
008700950616    2C                   ENDIF
008800050726
008900050726      * se passato anche il secondo parametro (provincia) controllo se p.i. di San Marino
009000050726      * in questo caso non devo fare nessun controllo
009100050726if  2c                   If        %Parms > 1
009200050726      * nazione italia
009300050726     c                             and ivanar = *Blanks
009400050726      * provincia FO o RN
009500061025     c                             and (xivaprv = 'FO' or xivaprv = 'RN'
009600061025     c                                  or xivaprv = 'FC')
009700050726      * controllo se nella partita iva c'è la sigla SM x San Marino
009800050726     c                   Clear                   wpos
009900050726     c                   Eval      wpos = %scan('SM':ivapar)
010000050907      * trovato SM controllo se la lunghezza è giusta
010100070426if  3c                   If        wpos > *Zeros or ivasta = 'SM'
010200050907     c                   ExSr      Sr_Ctrsm
010300050907      * vado sempre a fine controlli
010400050907     c                   Goto      endctr
010500070426    3c                   EndIf
010600050726    2c                   EndIf
010700950116     C*
010800950116     C* SE NON IMMESSO IL CODICE ISO LO PRENDO
010900950116     C     IVASTA        IFEQ      *BLANKS
011000950116    2C     §15ITA        ANDNE     'I'
011100950116     C                   MOVEL     §15ISO        IVASTA
011200950116    2C                   ENDIF
011300941121     C*
011400941121     C*  Reperisco sulla tabella la lunghezza della stringa
011500941121     C                   CLEAR                   DS4I
011600941121     C                   MOVE      '4I'          TBWCOD
011700941121     C                   MOVEL(P)  IVASTA        TBWKEY
011800941121     C     KEYTAB        CHAIN     TABEL00F                           80
011900941121     C*
012000941121     C* ERRORE --> NON ESISTE LA TABELLA DELLA P.IVA
012100941121    2C     *IN80         IFEQ      *ON
012200941121     C     TBLFLG        ORNE      *BLANKS
012300941121     C                   MOVEL     MSG(3)        IVAMSG
012400941121     C                   Z-SUB     3             IVAFLG
012500941121     C                   GOTO      ENDCTR
012600941121   x2C                   ELSE
012700941121     C                   MOVEL     TBLUNI        DS4I
012800941121    2C                   ENDIF
012900941122     C*
013000940301     C                   MOVEA     IVAPAR        EIVA
013100940824     C*
013200940301     C*  Determino la lunghezza effettiva della partita IVA
013300940301     C                   Z-ADD     1             X                 2 0
013400940301     C     ' '           LOOKUP    EIVA(X)                                40
013500941003    3C     *IN40         IFEQ      '0'
013600940823     C                   Z-ADD     14            LUNGH             2 0
013700940301     C                   ELSE
013800940301     C     X             SUB       1             LUNGH
013900941003    3C                   END
014000940823     C*
014100940301     C*  Se la lunghezza effettiva della stringa è diversa da
014200940301     C*  quella prevista gestisco errore
014300941003    3C     LUNGH         IFNE      §4ICHR
014400040630    3C     LUNGH         ANDNE     §4ICH2
014500040630    3C     LUNGH         ANDNE     §4ICH3
014600071102     c* non accetto lunghezza =0
014700071102     c     LUNGH         oreq      0
014800071102     c
014900950717     C                   MOVEA     MSG(2)        K78
015000950717     C                   MOVEL     §4ICHR        W002A             2
015100950717     C                   MOVEA     W002A         K78(47)
015200950717     C                   MOVEA     K78           IVAMSG
015300040630     C                   EVAL      IVAMsg = 'Il codice IVA della nazione ' +
015400040630     C                             IVASta + ' può essere lungo ' +
015500040630     C                             %TRIML(%EDITC(§4IChr:'Z'))
015600040630     C                   IF        §4ICh2 > 0
015700040630     C                   EVAL      IVAMsg = %TRIMR(IVAMsg) + ', ' +
015800040630     C                             %TRIML(%EDITC(§4ICh2:'Z'))
015900040630     C                   ENDIF
016000040630     C                   IF        §4ICh3 > 0
016100040630     C                   EVAL      IVAMsg = %TRIMR(IVAMsg) + ', ' +
016200040630     C                             %TRIML(%EDITC(§4ICh3:'Z'))
016300040630     C                   ENDIF
016400040630     C                   EVAL      IVAMsg = %TRIMR(IVAMsg) + ' caratteri.'
016500941003     C                   Z-SUB     2             IVAFLG
016600941003     C                   GOTO      ENDCTR
016700941003   X3C                   ELSE
016800940823     C*
016900940823     C* SE P.IVA ITALIANA ESEGUO ANCHE IL CONTROLLO FORMALE
017000061113    4C**!!!§15ITA        IFEQ      'I'
017100061113      * controllo il codice iso e non la nazione italia perchè posso inserire una
017200061113      * partita iva straniera su cliente italia
017300061113    4c     §4iITA        IFEQ      'I'
017400940823     C                   MOVEL     IVAPAR        XPIVA
017500940824     C                   CLEAR                   XSTATO
017600940823     C*
017700981124     C                   CALL      'XPARIVAO'
017800940823     C                   PARM                    XPIVA            16
017900940824     C                   PARM                    XSTATO            1 0
018000940824     C*
018100940824     C                   Z-ADD     XSTATO        IVAFLG
018200941003     C* ERRORE --> P.IVA ITALIANA FORMALMENTE ERRATA
018300941003    5C     IVAFLG        IFEQ      -1
018400941003     C                   MOVEL     MSG(1)        IVAMSG
018500941003     C                   GOTO      ENDCTR
018600941003    5C                   ENDIF
018700941003    4C                   ENDIF
018800941003     C*
018900941003    3C                   ENDIF
019000040115     C*
019100040115     C* SE CODICE ISO DELLA P.IVA INCONGRUENTE CON CODICE ISO DELLA NAZ
019200040115     C*  --> ERRORE FORZABILE
019300080312    3C***  §15ISO        IFNE      IVASTA
019400080312     C***  §4IITA        ANDNE     §15ITA
019500080312     c***  §15iso        oreq      '$$'
019600080312     c                   if        (§15iso<>ivasta and (§4iita<>'I' or
019700080312     c                             §15ita<>'I')) or §15iso='$$'
019800080311     c
019900080311     c* In filiale errore non forzabile se nazione italia
020000080311     c                   if        §15ita='I' and wforzaita=' '
020100080311     C                   MOVEL     MSG(7)        IVAMSG
020200080311     C                   Z-SUB     1             IVAFLG
020300080311     C                   GOTO      ENDCTR
020400080311     c                   else
020500040115     C                   MOVEL     MSG(4)        IVAMSG
020600061123     c                   eval      %subst(ivamsg:60:2) = §15iso
020700061106     C                   Z-SUB     9             IVAFLG
020800040115     C                   GOTO      ENDCTR
020900040115    3C                   ENDIF
021000080311    3C                   ENDIF
021100941003     C*
021200941003     C     ENDCTR        TAG
021300940823     C*
021400941003     C* SE IVATLA = "L" CHIUDO CON LR
021500941003    2C     IVATLA        IFEQ      'L'
021600941003     C                   SETON                                        LR
021700941003   X2C                   ELSE
021800941003     C                   RETURN
021900941003    2C                   ENDIF
022000941003    1C                   ENDIF
022100050907
022200050907      *-------------------------------------------------------------*
022300050907      * Controllo la lunghezza della partita iva di S.Marino
022400050907      *-------------------------------------------------------------*
022500050907     c     Sr_Ctrsm      BegSr
022600050907
022700050907      * Aggancio la tabella 4I per recuperare la lunghezza
022800050907     c                   Clear                   ds4i
022900050907     c                   Eval      tbwcod = '4I'
023000050907     c                   Eval      tbwkey = 'SM'
023100050907     c     keytab        Chain     Tabel00f
023200050907     c                   If        %Found(Tabel00f) and tblflg = *Blanks
023300050907     c                   Eval      ds4i = tbluni
023400050907      * Se non trovo la tabella o se è annullata chiodo la lunghezza a 5
023500050907     c                   Else
023600050907     c                   Eval      §4ichr = 5
023700050907     c                   EndIf
023800050907
023900050907     c                   Movea     ivapar        eiva
024000050907
024100050907      * Determino la lunghezza effettiva della partita iva
024200050907     c                   Eval      x = 1
024300050907     c     ' '           Lookup    eiva(x)                                40
024400050907      * non c'è nessun campo vuoto imposto la lunghezza massima del campo
024500050907      * meno 2 x SM
024600050907     c                   If        Not *In40
024700050907     c                   Eval      Lungh = 12
024800050907      * ci sono dei campi vuoti tolgo 3 campi xchè 2 sono x SM
024900050907     c                   Else
025000070426      * se nel cod. ISO c'è SM non devo togliere 3 (SM è da un'altra parte)
025100070426      * tolgo 1
025200070426     c                   if        ivasta = 'SM'
025300070426     c     x             sub       1             lungh
025400070426     c                   else
025500050907     c     x             sub       3             lungh
025600070426     c                   endif
025700050907     c                   EndIf
025800050907
025900050907      * Se la lunghezza effettiva della stringa è diversa da quella prevista
026000050907      * gestisco l'errore
026100050907     c                   If        lungh <> §4ichr and lungh <> §4ich2 and
026200050907     c                             lungh <> §4ich3
026300050907     c                   Movea     msg(2)        k78
026400050907     c                   Movel     §4ichr        w002a
026500050907     c                   Movea     w002a         k78(47)
026600050907     c                   Movea     k78           ivamsg
026700050908     c                   Eval      ivamsg = 'La partita IVA di San Marino deve-
026800050908     c                              essere: SM più ' +
026900050907     c                             %triml(%editc(§4ichr:'Z'))
027000050907     c                   If        §4iCh2 > 0
027100050907     c                   Eval      ivamsg = %trimr(ivamsg) + ', ' +
027200050907     c                             %triml(%editc(§4ich2:'Z'))
027300050907     c                   EndIf
027400050907     c                   If        §4ich3 > 0
027500050907     c                   Eval      ivamsg = %trimr(ivamsg) + ', ' +
027600050907     c                             %triml(%editc(§4ich3:'Z'))
027700050907     c                   EndIf
027800050907     c                   Eval      ivamsg = %trimr(ivamsg) + ' caratteri.'
027900050907     c                   Z-sub     2             ivaflg
028000050907     c                   EndIf
028100050907
028200050907     c                   EndSr
028300050907
028400940301     C*-------------------------------------------------------------*
028500940301     C     *INZSR        BEGSR
028600940301     C*-------------------------------------------------------------*
028700940301     C*  Definizione della chiave di accesso alla tabella 4I
028800940301     C     KEYTAB        KLIST
028900940301     C                   KFLD                    TBWKUT
029000940301     C                   KFLD                    TBWCOD
029100940301     C                   KFLD                    TBWKEY
029200940301     C*  Definizione Varibili
029300940301     C     *LIKE         DEFINE    TBLKUT        TBWKUT
029400940301     C     *LIKE         DEFINE    TBLCOD        TBWCOD
029500940301     C     *LIKE         DEFINE    TBLKEY        TBWKEY
029600940301     C*  Inizializzazione
029700940301     C                   Z-ADD     1             TBWKUT
029800940301     C                   ENDSR
029900950626**
030000941003Partita IVA italiana formalmente errata
030100950717Lunghezza Partita IVA errata: e' composta di  xx caratteri
030200941003Codice ISO inesistente o non prevista tabella della Partita IVA
030300061106Cod.ISO della P.IVA incongruente con cod.ISO della nazione
030400941003Nazione Errata
030500950626Se Privato -codice ISO $$ -lasciare Part.IVA in bianco o uguale a "PRIVATO"
030600080311Per nazione ITALIA non ammessa una partita IVA estera
