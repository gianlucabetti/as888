000100000619      /TITLE Esempio gestione SFL con vista alternativa e menù a discesa.
000200000615
000300000621     FTNVRAB5R  CF   E             WORKSTN
000400000614     F                                     SFILE(S01a:RrnS01)
000500000614     F                                     SFILE(S01b:RrnS01)
000600000614     F                                     SFILE(S01c:RrnS01)
000700000616     F                                     INDDS(IndAra)
000800000614     FAzOrg01L  IF   E           K DISK
000900000616     F                                     RENAME(AzOrg:AzOrg01)
001000000616     F                                     USROPN
001100000616     FAzOrg03L  IF   E           K DISK
001200000616     F                                     RENAME(AzOrg:AzOrg03)
001300000616     F                                     USROPN
001400000616     FAzOrg04L  IF   E           K DISK
001500000616     F                                     RENAME(AzOrg:AzOrg04)
001600000616     F                                     USROPN
001700000614
001800000616     D File            C                   1
001900000616     D Ordinamento     C                   2
002000000616     D Vista           C                   3
002100000619     D Selezioni       C                   4
002200000619     D F3              C                   3
002300000619     D F5              C                   5
002400000620     D F10             C                   10
002500000619     D F11             C                   11
002600000619     D F12             C                   12
002700000616     D PerCodice       C                   1
002800000616     D PerDescrizione  C                   2
002900000616     D PerRegione      C                   3
003000000619     D Vista1          C                   1
003100000619     D Vista2          C                   2
003200000619     D Vista3          C                   3
003300000619     D Omettere        C                   0
003400000619     D Selezionare     C                   1
003500000619     D Filiale         C                   'F'
003600000619     D Agenzia         C                   'A'
003700000619     D Varie           C                   'V'
003800000616      *
003900000615     D IndAra          DS
004000000615     D  SflDsp                 1      1N
004100000615     D  SflClr                 2      2N
004200000615      *
004300000614     D RrnS01          S              5U 0
004400000616     D WrkFineC01      S              1
004500000619     D WrkCarS01       S              1
004600000619     D WrkValido       S              1
004700000615     D SavRcdNbr       S
004800000615     D                                     LIKE(C01RcdNbr)
004900000614
005000000614     C                   EXSR      ExfC01
005100000614     C                   EVAL      *INLR = *ON
005200000614
005300000615      * Emissione del subfile control.
005400000614     C     ExfC01        BEGSR
005500000616      *
005600000619     C                   EVAL      WrkFineC01 = *OFF
005700000616     C                   DOU       WrkFineC01 = *ON
005800000619      * Carico i subfile.
005900000616     C                   EXSR      CarS01
006000000615      * Visualizzo i subfile se ho scritto almeno una riga.
006100000614     C                   IF        RrnS01 > *ZERO
006200000615     C                   EVAL      SflDsp = *ON
006300000615     C                   ENDIF
006400000619      * In C01CSRRRN c'è il numero di riga del subfile
006500000619      * su cui era il cursore quando l'utente ha premuto un tasto.
006600000619      * Impostando C01RCDNBR,
006700000619      * visualizzo la stessa pagina che vedeva l'utente
006800000615      * quando ha premuto l'ultimo tasto.
006900000615     C                   IF        C01CsrRrn > *ZERO
007000000615     C                   EVAL      C01RcdNbr = C01CsrRrn
007100000615     C                   ELSE
007200000615     C                   EVAL      C01RcdNbr = SavRcdNbr
007300000615     C                   ENDIF
007400000615      * Se non so quale pagina visualizzare, forzo pagina 1.
007500000615     C                   IF        C01RcdNbr < 1
007600000615     C                             OR
007700000615     C                             C01RcdNbr > RrnS01
007800000615     C                   EVAL      C01RcdNbr = 1
007900000615     C                   ENDIF
008000000619      * Salvo il record number del subfile.
008100000619     C                   EVAL      SavRcdNbr = C01RcdNbr
008200000619      * Testata.
008300000619     C                   WRITE     T01
008400000619      * Emetto il subfile control corrispondente alla vista richiesta.
008500000614     C                   SELECT
008600000619     C                   WHEN      P03Slt = Vista1
008700000614     C                   EXFMT     C01a
008800000619     C                   WHEN      P03Slt = Vista2
008900000614     C                   EXFMT     C01b
009000000619     C                   WHEN      P03Slt = Vista3
009100000614     C                   EXFMT     C01c
009200000614     C                   ENDSL
009300000619      * L'utente ha cliccato sulla barra di menù.
009400000619     C                   IF        C01ChcMnu <> *ZERO
009500000619     C                   EXSR      WrkM01
009600000619     C                   ENDIF
009700000615      * Gestione dei tasti funzionali.
009800000614     C                   SELECT
009900000616      * F3 Uscita.
010000000616     C                   WHEN      *INKC
010100000619     C                             OR
010200000619     C                             (C01ChcMnu = File
010300000619     C                              AND
010400000619     C                              P01Slt = F3)
010500000616     C                   EVAL      WrkFineC01 = *ON
010600000619      * F5  Ripristina.
010700000619     C                   WHEN      *INKE
010800000619     C                             OR
010900000619     C                             (C01ChcMnu = File
011000000619     C                              AND
011100000619     C                              P01Slt = F5)
011200000619     C                   EXSR      InzPgm
011300000615      * F11 Imposto vista alternativa.
011400000614     C                   WHEN      *INKK
011500000619     C                             OR
011600000619     C                             (C01ChcMnu = File
011700000619     C                              AND
011800000619     C                              P01Slt = F11)
011900000614     C                   SELECT
012000000619     C                   WHEN      P03Slt = Vista1
012100000619     C                   EVAL      P03Slt = Vista2
012200000619     C                   WHEN      P03Slt = Vista2
012300000619     C                   EVAL      P03Slt = Vista3
012400000619     C                   WHEN      P03Slt = Vista3
012500000619     C                   EVAL      P03Slt = Vista1
012600000614     C                   ENDSL
012700000620      * Memorizzo la vista nel menù a discesa.
012800000619     C                   WRITE     P03
012900000619      *
013000000614     C                   OTHER
013100000614     C                   ENDSL
013200000614      *
013300000614     C                   ENDDO
013400000614      *
013500000614     C                   ENDSR
013600000616
013700000616      * Caricamento subfile.
013800000616     C     CarS01        BEGSR
013900000619      *
014000000619     C                   IF        WrkCarS01 = *ON
014100000616      * Pulisco i subfile e le loro variabili di gestione.
014200000616     C                   CLEAR                   RrnS01
014300000616     C                   CLEAR                   C01CsrRrn
014400000616     C                   CLEAR                   C01RcdNbr
014500000616     C                   CLEAR                   SavRcdNbr
014600000616     C                   EVAL      SflDsp = *OFF
014700000616     C                   EVAL      SflClr = *ON
014800000616     C                   WRITE     C01a
014900000616     C                   WRITE     C01b
015000000616     C                   WRITE     C01c
015100000616     C                   EVAL      SflClr = *OFF
015200000616      *
015300000616     C                   CLOSE(E)  AzOrg01L
015400000616     C                   CLOSE(E)  AzOrg03L
015500000616     C                   CLOSE(E)  AzOrg04L
015600000616      *
015700000616     C                   SELECT
015800000619     C                   WHEN      P02Slt = PerCodice
015900000616     C                   OPEN      AzOrg01L
016000000616     C     *START        SETLL     AzOrg01L
016100000619     C                   WHEN      P02Slt = PerDescrizione
016200000616     C                   OPEN      AzOrg03L
016300000616     C     *START        SETLL     AzOrg03L
016400000619     C                   WHEN      P02Slt = PerRegione
016500000616     C                   OPEN      AzOrg04L
016600000616     C     *START        SETLL     AzOrg04L
016700000616     C                   ENDSL
016800000616      *
016900000616     C                   DOU       (%EOF(AzOrg01L)
017000000619     C                              AND
017100000619     C                              P02Slt = PerCodice)
017200000616     C                             OR
017300000619     C                             (%EOF(AzOrg03L)
017400000619     C                              AND
017500000619     C                              P02Slt = PerDescrizione)
017600000616     C                             OR
017700000616     C                             (%EOF(AzOrg04L)
017800000619     C                              AND
017900000619     C                              P02Slt = PerRegione)
018000000616      *
018100000616     C                   SELECT
018200000619     C                   WHEN      P02Slt = PerCodice
018300000616     C                   READ      AzOrg01L
018400000619     C                   WHEN      P02Slt = PerDescrizione
018500000616     C                   READ      AzOrg03L
018600000619     C                   WHEN      P02Slt = PerRegione
018700000616     C                   READ      AzOrg04L
018800000616     C                   ENDSL
018900000616      *
019000000616     C                   IF        NOT %EOF
019100000619      *
019200000619     C                   EVAL      WrkValido = *ON
019300000619      * Selezione tipo filiale.
019400000619     C                   SELECT
019500000619     C                   WHEN      (OrgFag = Filiale
019600000619     C                              AND
019700000619     C                              P04SltFil = Omettere)
019800000619     C                             OR
019900000619     C                             (OrgFag = Agenzia
020000000619     C                              AND
020100000619     C                              P04SltAge = Omettere)
020200000619     C                             OR
020300000619     C                             (OrgFag = Varie
020400000619     C                              AND
020500000619     C                              P04SltVar = Omettere)
020600000619     C                   EVAL      WrkValido = *OFF
020700000619     C                   ENDSL
020800000616      * Scrivo i subfile in parallelo.
020900000619     C                   IF        WrkValido = *ON
021000000616     C                   ADD       1             RrnS01
021100000616     C                   WRITE(E)  S01a
021200000616     C                   WRITE(E)  S01b
021300000616     C                   WRITE(E)  S01c
021400000619     C                   ENDIF
021500000616      * Il subfile è pieno.
021600000616     C                   IF        %ERROR
021700000616     C                             AND
021800000616     C                             %STATUS = 01255
021900000616     C                   LEAVE
022000000616     C                   ENDIF
022100000616      *
022200000616     C                   ENDIF
022300000616      *
022400000616     C                   ENDDO
022500000619      *
022600000619     C                   EVAL      WrkCarS01 = *OFF
022700000619      *
022800000619     C                   ENDIF
022900000616      *
023000000616     C                   ENDSR
023100000616
023200000620      * Gestione barra dei menù.
023300000619     C     WrkM01        BEGSR
023400000619      * Determino quale menù a discesa ha scelto l'utente.
023500000616     C                   SELECT
023600000616     C                   WHEN      C01ChcMnu = File
023700000619     C                   EXSR      WrkP01
023800000616     C                   WHEN      C01ChcMnu = Ordinamento
023900000619     C                   EXSR      WrkP02
024000000616     C                   WHEN      C01ChcMnu = Vista
024100000619     C                   EXSR      WrkP03
024200000619     C                   WHEN      C01ChcMnu = Selezioni
024300000619     C                   EXSR      WrkP04
024400000616     C                   ENDSL
024500000616      *
024600000616     C                   ENDSR
024700000616
024800000619      * Gestione menù a discesa "File".
024900000619     C     WrkP01        BEGSR
025000000620      * Leggo la scelta fatta.
025100000619     C                   READ      P01
025200000616      *
025300000616     C                   ENDSR
025400000616
025500000619      * Gestione menù a discesa "Ordinamento".
025600000619     C     WrkP02        BEGSR
025700000620      * Leggo la scelta fatta.
025800000619     C                   READ      P02
025900000620      * Memorizzo la scelta fatta, così la prossima volta che l'utente
026000000620      * apre il menù a discesa, la scelta è in alta intensità.
026100000619     C                   WRITE     P02
026200000620      * L'utente ha cambiato l'ordinamento, quindi devo ricaricare i SFL.
026300000619     C                   EVAL      WrkCarS01 = *ON
026400000616      *
026500000616     C                   ENDSR
026600000616
026700000619      * Gestione menù a discesa "Vista".
026800000619     C     WrkP03        BEGSR
026900000620      * Leggo la scelta fatta.
027000000619     C                   READ      P03
027100000620      * Memorizzo la scelta fatta, così la prossima volta che l'utente
027200000620      * apre il menù a discesa, la scelta è in alta intensità.
027300000619     C                   WRITE     P03
027400000616      *
027500000616     C                   ENDSR
027600000619
027700000619      * Gestione menù a discesa "Selezioni".
027800000619     C     WrkP04        BEGSR
027900000620      * Leggo la scelta fatta.
028000000619     C                   READ      P04
028100000620      * Memorizzo la scelta fatta, così la prossima volta che l'utente
028200000620      * apre il menù a discesa, la scelta è in alta intensità.
028300000619     C                   WRITE     P04
028400000620      * L'utente ha cambiato le selezioni, quindi devo ricaricare i SFL.
028500000619     C                   EVAL      WrkCarS01 = *ON
028600000619      *
028700000619     C                   ENDSR
028800000619
028900000619      * Inizializzo variabili generali programma.
029000000619     C     InzPgm        BEGSR
029100000619      * Imposto i valori iniziali delle scelte dei menù.
029200000619     C                   EVAL      P02Slt = PerCodice
029300000619     C                   WRITE     P02
029400000619     C                   EVAL      P03Slt = Vista1
029500000619     C                   WRITE     P03
029600000619     C                   EVAL      P04SltFil = Selezionare
029700000619     C                   EVAL      P04SltAge = Selezionare
029800000619     C                   EVAL      P04SltVar = Selezionare
029900000619     C                   WRITE     P04
030000000619      * Carico i subfile.
030100000619     C                   EVAL      WrkCarS01 = *ON
030200000619      *
030300000619     C                   ENDSR
030400000616
030500000619      * Operazioni iniziali.
030600000616     C     *INZSR        BEGSR
030700000619      *
030800000619     C                   EXSR      InzPgm
030900000616      *
031000000616     C                   ENDSR
