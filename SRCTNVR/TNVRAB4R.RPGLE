000100000619      /TITLE Esempio gestione SFL con vista alternativa e menù a discesa.
000200000615
000300000621     FTNVRAB4R  CF   E             WORKSTN
000400000614
000500000616     D File            C                   1
000600000616     D Ordinamento     C                   2
000700000616     D Vista           C                   3
000800000619     D Selezioni       C                   4
000900000619     D F3              C                   3
001000000619     D F5              C                   5
001100000620     D F10             C                   10
001200000619     D F11             C                   11
001300000619     D F12             C                   12
001400000616     D PerCodice       C                   1
001500000616     D PerDescrizione  C                   2
001600000616     D PerRegione      C                   3
001700000619     D Vista1          C                   1
001800000619     D Vista2          C                   2
001900000619     D Vista3          C                   3
002000000619     D Omettere        C                   0
002100000619     D Selezionare     C                   1
002200000619     D Filiale         C                   'F'
002300000619     D Agenzia         C                   'A'
002400000619     D Varie           C                   'V'
002500000616      *
002600000615     D IndAra          DS
002700000615     D  SflDsp                 1      1N
002800000615     D  SflClr                 2      2N
002900000615      *
003000000614     D RrnS01          S              5U 0
003100000616     D WrkFineC01      S              1
003200000619     D WrkCarS01       S              1
003300000619     D WrkValido       S              1
003400000615     D SavRcdNbr       S
003500000615     D                                     LIKE(C01RcdNbr)
003600000614
003700000614     C                   EXSR      ExfC01
003800000614     C                   EVAL      *INLR = *ON
003900000614
004000000615      * Emissione del subfile control.
004100000614     C     ExfC01        BEGSR
004200000616      *
004300000619     C                   EVAL      WrkFineC01 = *OFF
004400000616     C                   DOU       WrkFineC01 = *ON
004500000619      * Carico i subfile.
004600000616     C                   EXSR      CarS01
004700000615      * Visualizzo i subfile se ho scritto almeno una riga.
004800000614     C                   IF        RrnS01 > *ZERO
004900000615     C                   EVAL      SflDsp = *ON
005000000615     C                   ENDIF
005100000619      * In C01CSRRRN c'è il numero di riga del subfile
005200000619      * su cui era il cursore quando l'utente ha premuto un tasto.
005300000619      * Impostando C01RCDNBR,
005400000619      * visualizzo la stessa pagina che vedeva l'utente
005500000615      * quando ha premuto l'ultimo tasto.
005600000615     C                   IF        C01CsrRrn > *ZERO
005700000615     C                   EVAL      C01RcdNbr = C01CsrRrn
005800000615     C                   ELSE
005900000615     C                   EVAL      C01RcdNbr = SavRcdNbr
006000000615     C                   ENDIF
006100000615      * Se non so quale pagina visualizzare, forzo pagina 1.
006200000615     C                   IF        C01RcdNbr < 1
006300000615     C                             OR
006400000615     C                             C01RcdNbr > RrnS01
006500000615     C                   EVAL      C01RcdNbr = 1
006600000615     C                   ENDIF
006700000619      * Salvo il record number del subfile.
006800000619     C                   EVAL      SavRcdNbr = C01RcdNbr
006900000619      * Testata.
007000000619     C                   WRITE     T01
007100000619      * Emetto il subfile control corrispondente alla vista richiesta.
007200000614     C                   SELECT
007300000619     C                   WHEN      P03Slt = Vista1
007400000614     C                   EXFMT     C01a
007500000619     C                   WHEN      P03Slt = Vista2
007600000614     C                   EXFMT     C01b
007700000619     C                   WHEN      P03Slt = Vista3
007800000614     C                   EXFMT     C01c
007900000614     C                   ENDSL
008000000619      * L'utente ha cliccato sulla barra di menù.
008100000619     C                   IF        C01ChcMnu <> *ZERO
008200000619     C                   EXSR      WrkM01
008300000619     C                   ENDIF
008400000615      * Gestione dei tasti funzionali.
008500000614     C                   SELECT
008600000616      * F3 Uscita.
008700000616     C                   WHEN      *INKC
008800000619     C                             OR
008900000619     C                             (C01ChcMnu = File
009000000619     C                              AND
009100000619     C                              P01Slt = F3)
009200000616     C                   EVAL      WrkFineC01 = *ON
009300000619      * F5  Ripristina.
009400000619     C                   WHEN      *INKE
009500000619     C                             OR
009600000619     C                             (C01ChcMnu = File
009700000619     C                              AND
009800000619     C                              P01Slt = F5)
009900000619     C                   EXSR      InzPgm
010000000615      * F11 Imposto vista alternativa.
010100000614     C                   WHEN      *INKK
010200000619     C                             OR
010300000619     C                             (C01ChcMnu = File
010400000619     C                              AND
010500000619     C                              P01Slt = F11)
010600000614     C                   SELECT
010700000619     C                   WHEN      P03Slt = Vista1
010800000619     C                   EVAL      P03Slt = Vista2
010900000619     C                   WHEN      P03Slt = Vista2
011000000619     C                   EVAL      P03Slt = Vista3
011100000619     C                   WHEN      P03Slt = Vista3
011200000619     C                   EVAL      P03Slt = Vista1
011300000614     C                   ENDSL
011400000620      * Memorizzo la vista nel menù a discesa.
011500000619     C                   WRITE     P03
011600000619      *
011700000614     C                   OTHER
011800000614     C                   ENDSL
011900000614      *
012000000614     C                   ENDDO
012100000614      *
012200000614     C                   ENDSR
012300000616
012400000616      * Caricamento subfile.
012500000616     C     CarS01        BEGSR
012600000619      *
012700000619     C                   IF        WrkCarS01 = *ON
012800000616      * Pulisco i subfile e le loro variabili di gestione.
012900000616     C                   CLEAR                   RrnS01
013000000616     C                   CLEAR                   C01CsrRrn
013100000616     C                   CLEAR                   C01RcdNbr
013200000616     C                   CLEAR                   SavRcdNbr
013300000616     C                   EVAL      SflDsp = *OFF
013400000616     C                   EVAL      SflClr = *ON
013500000616     C                   WRITE     C01a
013600000616     C                   WRITE     C01b
013700000616     C                   WRITE     C01c
013800000616     C                   EVAL      SflClr = *OFF
013900000616      *
014000000616     C                   CLOSE(E)  AzOrg01L
014100000616     C                   CLOSE(E)  AzOrg03L
014200000616     C                   CLOSE(E)  AzOrg04L
014300000616      *
014400000616     C                   SELECT
014500000619     C                   WHEN      P02Slt = PerCodice
014600000616     C                   OPEN      AzOrg01L
014700000616     C     *START        SETLL     AzOrg01L
014800000619     C                   WHEN      P02Slt = PerDescrizione
014900000616     C                   OPEN      AzOrg03L
015000000616     C     *START        SETLL     AzOrg03L
015100000619     C                   WHEN      P02Slt = PerRegione
015200000616     C                   OPEN      AzOrg04L
015300000616     C     *START        SETLL     AzOrg04L
015400000616     C                   ENDSL
015500000616      *
015600000616     C                   DOU       (%EOF(AzOrg01L)
015700000619     C                              AND
015800000619     C                              P02Slt = PerCodice)
015900000616     C                             OR
016000000619     C                             (%EOF(AzOrg03L)
016100000619     C                              AND
016200000619     C                              P02Slt = PerDescrizione)
016300000616     C                             OR
016400000616     C                             (%EOF(AzOrg04L)
016500000619     C                              AND
016600000619     C                              P02Slt = PerRegione)
016700000616      *
016800000616     C                   SELECT
016900000619     C                   WHEN      P02Slt = PerCodice
017000000616     C                   READ      AzOrg01L
017100000619     C                   WHEN      P02Slt = PerDescrizione
017200000616     C                   READ      AzOrg03L
017300000619     C                   WHEN      P02Slt = PerRegione
017400000616     C                   READ      AzOrg04L
017500000616     C                   ENDSL
017600000616      *
017700000616     C                   IF        NOT %EOF
017800000619      *
017900000619     C                   EVAL      WrkValido = *ON
018000000619      * Selezione tipo filiale.
018100000619     C                   SELECT
018200000619     C                   WHEN      (OrgFag = Filiale
018300000619     C                              AND
018400000619     C                              P04SltFil = Omettere)
018500000619     C                             OR
018600000619     C                             (OrgFag = Agenzia
018700000619     C                              AND
018800000619     C                              P04SltAge = Omettere)
018900000619     C                             OR
019000000619     C                             (OrgFag = Varie
019100000619     C                              AND
019200000619     C                              P04SltVar = Omettere)
019300000619     C                   EVAL      WrkValido = *OFF
019400000619     C                   ENDSL
019500000616      * Scrivo i subfile in parallelo.
019600000619     C                   IF        WrkValido = *ON
019700000616     C                   ADD       1             RrnS01
019800000616     C                   WRITE(E)  S01a
019900000616     C                   WRITE(E)  S01b
020000000616     C                   WRITE(E)  S01c
020100000619     C                   ENDIF
020200000616      * Il subfile è pieno.
020300000616     C                   IF        %ERROR
020400000616     C                             AND
020500000616     C                             %STATUS = 01255
020600000616     C                   LEAVE
020700000616     C                   ENDIF
020800000616      *
020900000616     C                   ENDIF
021000000616      *
021100000616     C                   ENDDO
021200000619      *
021300000619     C                   EVAL      WrkCarS01 = *OFF
021400000619      *
021500000619     C                   ENDIF
021600000616      *
021700000616     C                   ENDSR
021800000616
021900000620      * Gestione barra dei menù.
022000000619     C     WrkM01        BEGSR
022100000619      * Determino quale menù a discesa ha scelto l'utente.
022200000616     C                   SELECT
022300000616     C                   WHEN      C01ChcMnu = File
022400000619     C                   EXSR      WrkP01
022500000616     C                   WHEN      C01ChcMnu = Ordinamento
022600000619     C                   EXSR      WrkP02
022700000616     C                   WHEN      C01ChcMnu = Vista
022800000619     C                   EXSR      WrkP03
022900000619     C                   WHEN      C01ChcMnu = Selezioni
023000000619     C                   EXSR      WrkP04
023100000616     C                   ENDSL
023200000616      *
023300000616     C                   ENDSR
023400000616
023500000619      * Gestione menù a discesa "File".
023600000619     C     WrkP01        BEGSR
023700000620      * Leggo la scelta fatta.
023800000619     C                   READ      P01
023900000616      *
024000000616     C                   ENDSR
024100000616
024200000619      * Gestione menù a discesa "Ordinamento".
024300000619     C     WrkP02        BEGSR
024400000620      * Leggo la scelta fatta.
024500000619     C                   READ      P02
024600000620      * Memorizzo la scelta fatta, così la prossima volta che l'utente
024700000620      * apre il menù a discesa, la scelta è in alta intensità.
024800000619     C                   WRITE     P02
024900000620      * L'utente ha cambiato l'ordinamento, quindi devo ricaricare i SFL.
025000000619     C                   EVAL      WrkCarS01 = *ON
025100000616      *
025200000616     C                   ENDSR
025300000616
025400000619      * Gestione menù a discesa "Vista".
025500000619     C     WrkP03        BEGSR
025600000620      * Leggo la scelta fatta.
025700000619     C                   READ      P03
025800000620      * Memorizzo la scelta fatta, così la prossima volta che l'utente
025900000620      * apre il menù a discesa, la scelta è in alta intensità.
026000000619     C                   WRITE     P03
026100000616      *
026200000616     C                   ENDSR
026300000619
026400000619      * Gestione menù a discesa "Selezioni".
026500000619     C     WrkP04        BEGSR
026600000620      * Leggo la scelta fatta.
026700000619     C                   READ      P04
026800000620      * Memorizzo la scelta fatta, così la prossima volta che l'utente
026900000620      * apre il menù a discesa, la scelta è in alta intensità.
027000000619     C                   WRITE     P04
027100000620      * L'utente ha cambiato le selezioni, quindi devo ricaricare i SFL.
027200000619     C                   EVAL      WrkCarS01 = *ON
027300000619      *
027400000619     C                   ENDSR
027500000621      *--------------------------------------------------------------------------
027600000619      * Inizializzo variabili generali programma.
027700000619     C     InzPgm        BEGSR
027800000619      * Imposto i valori iniziali delle scelte dei menù.
027900000619     C                   EVAL      P02Slt = PerCodice
028000000619     C                   WRITE     P02
028100000619     C                   EVAL      P03Slt = Vista1
028200000619     C                   WRITE     P03
028300000619     C                   EVAL      P04SltFil = Selezionare
028400000619     C                   EVAL      P04SltAge = Selezionare
028500000619     C                   EVAL      P04SltVar = Selezionare
028600000619     C                   WRITE     P04
028700000619      * Carico i subfile.
028800000619     C                   EVAL      WrkCarS01 = *ON
028900000619      *
029000000619     C                   ENDSR
029100000621      *--------------------------------------------------------------------------
029200000619      * Operazioni iniziali.
029300000616     C     *INZSR        BEGSR
029400000619      *
029500000619     C                   EXSR      InzPgm
029600000616      *
029700000616     C                   ENDSR
