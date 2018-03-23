000100071128     ***********************************************************************************************
000200071128     **
000300071128     ** Questo modulo raddoppia gli apici in una stringa,
000400071128     ** in modo che possa essere usata p.e. in una istruzione SQL senza problemi.
000500071128     ** Per esempio "Ilaria D'Amico e' una gnocca abarth"
000600071128     **     diventa "Ilaria D''Amico e'' una gnocca abarth".
000700071128     **
000800071128     ** Può essere usato come sottoprocedura o come programma.
000900071128     ** La sottoprocedura restituisce la stringa manipolata.
001000071128     **
001100071128     ** PARAMETRI DELLA SOTTOPROCEDURA
001200071128     **
001300071128     ** Posizione Uso Tipo      Descrizione
001400071128     ** --------- --- --------- ------------------------------
001500071128     ** 1         IN  CHARACTER La stringa da manipolare.
001600071128     ** 2         IN  INTEGER   La lunghezza del parametro 1.
001700071128     ** 3         OUT CHARACTER La stringa manipolata.
001800071128     ** 4         IN  INTEGER   La lunghezza del parametro 3.
001900071128     ** --------- --- --------- ------------------------------
002000071128     **
002100071128     ** PARAMETRI DEL PROGRAMMA
002200071128     **
002300071128     ** Posizione Uso Tipo      Descrizione
002400071128     ** --------- --- --------- ------------------------------
002500071128     ** 1         IN  CHARACTER La stringa da manipolare.
002600071128     ** 2         IN  DECIMAL   La lunghezza del parametro 1.
002700071128     ** 3         OUT CHARACTER La stringa manipolata.
002800071128     ** 4         IN  DECIMAL   La lunghezza del parametro 3.
002900071128     ** --------- --- --------- ------------------------------
003000071128     **
003100071128     ** ESEMPI DI CHIAMATA
003200071128     **
003300071128     ** output = RaddoppiareApici(input : %SIZE(input));
003400071128     ** RaddoppiareApici(input : %SIZE(input) : output : %SIZE(output));
003500071128     **
003600071128     ** ISTRUZIONI PER LA COMPILAZIONE
003700071128     **
003800071128     ** 1. Creare il modulo TRULAPICI.
003900071128     ** 2. Aggiornare il programma di servizio TRULAPICI.
004000071128     ** 3. Creare il programma TRULAPICI dal modulo.
004100071128     ** 4. Cancellare il modulo.
004200071128     **
004300071128     ***********************************************************************************************
004400071128
004500071128      //********************************************************************************************
004600071128      //
004700071128      // Definizione prototipi procedure.
004800071128      //
004900071128      //********************************************************************************************
005000071127     D/COPY GAITRASRC/SRCPROTOPR,TRULAPICI
005100071128
005200071128      //********************************************************************************************
005300071128      //
005400071128      // Definizione variabili globali.
005500071128      //
005600071128      //********************************************************************************************
005700071128     D prmInput        S          65535A
005800071128     D prmInputLen     S             15P 5
005900071128     D prmOutput       S          65535A
006000071128     D prmOutputLen    S             15P 5
006100071127
006200071128      //********************************************************************************************
006300071128      //
006400071128      // Dichiarazione parametri.
006500071128      //
006600071128      //********************************************************************************************
006700071128     C     *ENTRY        PLIST
006800071128     C                   PARM                    prmInput
006900071128     C                   PARM                    prmInputLen
007000071128     C                   PARM                    prmOutput
007100071128     C                   PARM                    prmOutputLen
007200071128
007300071128      /FREE
007400071128       RaddoppiareApici(prmInput : prmInputLen : prmOutput : prmOutputLen);
007500071128       RETURN;
007600071128      /END-FREE
007700071128
007800071127     P*--------------------------------------------------
007900071127     P* Procedure name: RaddoppiareApici
008000071127     P* Purpose:        Raddoppia gli apici in una stringa.
008100071127     P* Returns:        Stringa di output.
008200071127     P* Parameter:      piInput => Stringa di input.
008300071127     P* Parameter:      piInputLength => Lunghezza input
008400071127     P* Parameter:      piOutput => Stringa di output
008500071127     P* Parameter:      piOutputLength => Lunghezza output
008600071127     P*--------------------------------------------------
008700071127     P RaddoppiareApici...
008800071127     P                 B                   EXPORT
008900071127     D RaddoppiareApici...
009000071127     D                 PI         65535A   VARYING
009100071127     D  piInput                   65535A   OPTIONS(*VARSIZE)
009200071127     D  piInputLength                10I 0 CONST
009300071127     D  piOutput                  65535A   OPTIONS(*NOPASS:*VARSIZE)
009400071127     D  piOutputLength...
009500071127     D                               10I 0 OPTIONS(*NOPASS) CONST
009600071127
009700071127     D UN_APICE        C                   ''''
009800071127     D output          S          65535A   VARYING
009900071127     D p               S             10I 0
010000071127
010100071127      /FREE
010200071127
010300071127       // Ho ricevuto i parametri di output.
010400071127       IF %PARMS() > 2;
010500071127         %SUBST(piOutput : 1 : piOutputLength) = *BLANK;
010600071127       ENDIF;
010700071127
010800071127       // La stringa di input è vuota.
010900071127       IF %SUBST(piInput : 1 : piInputLength) = *BLANK;
011000071127         RETURN output;
011100071127       ENDIF;
011200071127
011300071127       output = %TRIMR(%SUBST(piInput : 1 : piInputLength));
011400071127
011500071128       // La stringa di output può risultare lunga al massimo il doppio di quella di input
011600071128       // nella del tutto teorica ipotesi che la striga sia piena di apici.
011700071127       IF (%LEN(output) * 2) < %SIZE(output);
011800071127         %LEN(output) = (%LEN(output) * 2);
011900071127       ELSE;
012000071127         %LEN(output) = %SIZE(output);
012100071127       ENDIF;
012200071127
012300071128       // Raddoppio gli apici.
012400071127       DOU p = 0 OR p >= %LEN(output);
012500071127         p = %SCAN(UN_APICE : output : p + 1);
012600071127         IF p > 0;
012700071127           output = %REPLACE(UN_APICE : output : p : 0);
012800071127           p += 1;
012900071127         ENDIF;
013000071127       ENDDO;
013100071127
013200071127       IF %PARMS() > 2;
013300071127         %SUBST(piOutput : 1 : piOutputLength) = output;
013400071127       ENDIF;
013500071127
013600071127       RETURN %TRIMR(output);
013700071127
013800071127      /END-FREE
013900071127     P RaddoppiareApici...
014000071127     P                 E
014100071127
