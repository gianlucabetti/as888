000100010308      /TITLE Restituisce schiere £3, L3 e §L3.
000200010309      **********************************************************************
000300010309      *
000400010309      * Parametri di input:
000500010309      *
000600010309      * PrmDrf         Data riferimento *ISO.
000700010309      *                Obbligatorio.
000800010309      *                Se è *LOVAL, si assume la data corrente.
000900010309      *
001000010309      * PrmTfp         Filiale I livello da selezionare.
001100010309      *                Obbligatorio.
001200010309      *                Se vuoto, sono selezionate tutte le filiali.
001300010309      *                Se pieno, sono selezionate solo  le filiali
001400010309      *                che appartengono a quel gruppo.
001500010309      *
001600010309      * PrmAddReplace  Aggiunta o sostituzione schiere.
001700010309      *                Obbligatorio.
001800010309      *                'A' = il programma non pulisce le schiere.
001900010309      *                'R' = il programma     pulisce le schiere.
002000010309      *
002100010309      *--------------------------------------------------------------------*
002200010309      *
002300010309      * Parametri di output:
002400010309      *
002500010309      * PrmEsito       Esito elaborazione.
002600010309      *                Obbligatorio.
002700010309      *                '0' = eseguito.
002800010309      *                '1' = errori.
002900010309      *
003000010309      * £3             Filiali di I e II livello.
003100010309      *                Obbligatorio.
003200010309      *                E' una schiera di 999 elementi 3,0 packed,
003300010309      *                per un totale di 1998 bytes.
003400010309      *                La posizione dell'elemento corrisponde al
003500010309      *                codice filiale di II livello.
003600010309      *                L'elemento contiene il codice filiale I livello.
003700010309      *                Es.: £3(70) vale 1, cioè Bologna è la filiale
003800010309      *                di I livello di Ferrara.
003900010309      *
004000010309      * §L3            Filiali II livello.
004100010309      *                Facoltativo.
004200010309      *                E' una schiera di 999 elementi 3 A,
004300010309      *                per un totale di 2997 bytes.
004400010309      *                Gli elementi sono riempiti in sequenza.
004500010309      *                L'elemento contiene il codice filiale II livello.
004600010309      *
004700010309      * L3             Filiali I livello.
004800010309      *                Facoltativo.
004900010309      *                E' una schiera di 999 elementi 3 A,
005000010309      *                per un totale di 2997 bytes,
005100010309      *                correlata a §L3.
005200010309      *                Gli elementi sono riempiti in sequenza.
005300010309      *                L'elemento contiene il codice filiale I livello.
005400010309      *
005500010309      *--------------------------------------------------------------------*
005600010309      *
005700010309      * Esempi di chiamata:
005800010309      *
005900010309      * 1) Tutte le filiali.
006000010309      *
006100010309     C*                  CLEAR                   PrmDrf
006200010309     C*                  CLEAR                   PrmTfp
006300010309     C*                  EVAL      PrmAddReplace = 'R'
006400010309     C*                  CLEAR                   £3
006500010309     C*                  CLEAR                   §L3
006600010309     C*                  CLEAR                   L3
006700010309     C*                  CALL      'TRUL18R'
006800010309     C*                  PARM                    PrmDrf
006900010309     C*                  PARM                    PrmTfp
007000010309     C*                  PARM                    PrmAddReplace
007100010309     C*                  PARM      *BLANK        PrmEsito
007200010309     C*                  PARM                    £3
007300010309     C*                  PARM                    §L3
007400010309     C*                  PARM                    L3
007500010309      *
007600010309      * 2) Solo le filiali di Bologna (001) e Genova (002).
007700010309      *
007800010309     C*                  CLEAR                   PrmDrf
007900010309     C*                  EVAL      PrmTfp = 1
008000010309     C*                  EVAL      PrmAddReplace = 'R'
008100010309     C*                  CLEAR                   £3
008200010309     C*                  CLEAR                   §L3
008300010309     C*                  CLEAR                   L3
008400010309     C*                  CALL      'TRUL18R'
008500010309     C*                  PARM                    PrmDrf
008600010309     C*                  PARM                    PrmTfp
008700010309     C*                  PARM                    PrmAddReplace
008800010309     C*                  PARM      *BLANK        PrmEsito
008900010309     C*                  PARM                    £3
009000010309     C*                  PARM                    §L3
009100010309     C*                  PARM                    L3
009200010309      *
009300010309     C*                  EVAL      PrmTfp = 2
009400010309     C*                  EVAL      PrmAddReplace = 'A'
009500010309     C*                  CALL      'TRUL18R'
009600010309     C*                  PARM                    PrmDrf
009700010309     C*                  PARM                    PrmTfp
009800010309     C*                  PARM                    PrmAddReplace
009900010309     C*                  PARM      *BLANK        PrmEsito
010000010309     C*                  PARM                    £3
010100010309     C*                  PARM                    §L3
010200010309     C*                  PARM                    L3
010300010309      *
010400010309      **********************************************************************
010500010308
010600010308     H DFTACTGRP(*NO) ACTGRP(*CALLER)
010700010308
010800010308     FAzOrg01L  IF   E           K DISK
010900010308     F                                     USROPN
011000010308
011100010308     D Eseguito        C                   '0'
011200010308     D Errore          C                   '1'
011300010308     D Add             C                   'A'
011400010308     D Replace         C                   'R'
011500010308     D Partenza        C                   'P'
011600010308     D PrmDrf          S               D
011700010308     D PrmTfp          S
011800010308     D                                     LIKE(D55Tfp)
011900010308     D PrmAddReplace   S              1
012000010308     D PrmEsito        S              1
012100010308     D £3              S              3  0 DIM(999)
012200010308     D L3              S              3    DIM(999)
012300010308     D §L3             S              3    DIM(999)
012400010308     D L               S              5I 0
012500010308     D FNLV55DS      E DS
012600010308     D                                     INZ
012700010308
012800010308     C     *ENTRY        PLIST
012900010308     C                   PARM                    PrmDrf
013000010308     C                   PARM                    PrmTfp
013100010308     C                   PARM                    PrmAddReplace
013200010308     C                   PARM                    PrmEsito
013300010308     C                   PARM                    £3
013400010308     C                   PARM                    §L3
013500010308     C                   PARM                    L3
013600010308      *
013700010308     C                   EXSR      ChkParm
013800010308     C                   EXSR      SetSch
013900010308      *
014000010308     C                   EXSR      Uscita
014100010308
014200010308      * Impostazione schiere.
014300010308     C     SetSch        BEGSR
014400010308      * Se richiesto pulisco le schiere.
014500010308     C                   IF        PrmAddReplace = Replace
014600010308     C                   CLEAR                   £3
014700010308     C                   IF        %PARMS > 5
014800010308     C                   CLEAR                   L
014900010308     C                   CLEAR                   L3
015000010308     C                   CLEAR                   §L3
015100010308     C                   ENDIF
015200010308     C                   ENDIF
015300010308      *
015400010309     C                   CLEAR                   FNLV55DS
015500010308     C                   EVAL      D55Tpt = Partenza
015600010308     C                   MOVE      PrmDrf        D55Drf
015700010308      *
015800010308     C                   OPEN      AZORG01L
015900010308      *
016000010308     C                   READ      AZORG01L
016100010308     C                   DOW       NOT %EOF
016200010308      *  Reperisco terminal di partenza collegato
016300010308     C                   EVAL      D55Lnp = ORGFil
016400010308     C                   EVAL      D55Lin = ORGFil
016500010308     C                   CALL      'FNLV55R'
016600010308     C                   PARM                    FNLV55DS
016700010308      * Selezione della filiale di I livello.
016800010308     C                   IF        PrmTfp = *ZERO
016900010308     C                             OR
017000010308     C                             PrmTfp = D55Tfp
017100010308      *
017200010308     C                   EVAL      £3(OrgFil) = D55Tfp
017300010308      *
017400010308     C                   IF        %PARMS > 5
017500010308     C                   ADD       1             L
017600010308     C                   MOVE      OrgFil        §L3(L)
017700010308     C                   MOVE      D55Tfp        L3(L)
017800010308     C                   ENDIF
017900010308      *
018000010308     C                   ENDIF
018100010308      *
018200010308     C                   READ      AZORG01L
018300010308     C                   ENDDO
018400010308      *
018500010308     C                   CLOSE     AZORG01L
018600010308      *
018700010308     C                   ENDSR
018800010308
018900010308      * Controllo parametri ricevuti.
019000010308     C     ChkParm       BEGSR
019100010308      *
019200010308     C                   IF        PrmDrf = *LOVAL
019300010308     C                   TIME                    PrmDrf
019400010308     C                   ENDIF
019500010308      *
019600010308     C                   CLEAR                   PrmEsito
019700010308      *
019800010308     C                   ENDSR
019900010308
020000010308      * Uscita dal programma.
020100010308     C     Uscita        BEGSR
020200010308      *
020300010308     C                   IF        PrmEsito = *BLANK
020400010308     C                   EVAL      PrmEsito = Eseguito
020500010308     C                   ENDIF
020600010308      *
020700010308     C                   RETURN
020800010308      *
020900010308     C                   ENDSR
