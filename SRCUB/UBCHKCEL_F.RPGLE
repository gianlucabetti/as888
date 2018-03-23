000100080611     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400131017     ** Questo modulo verifica / normalizza un numero di cellulare
000500081003     **
000600081003     ** Può essere usato solo come sottoprocedura.
000700081003     ** Ogni metodo restituisce un valore 10I:
000800081003     **             - = 0 => tutto ok
000900081003     **             - < 0 => errore
001000071128     **
001100131017     ** PARAMETRI DELLA SOTTOPROCEDURA: UBCHKCEL_Check...
001200071129     **
001300081003     ** Posizione Uso Tipo      Lunghezza Descrizione
001400081003     ** --------- --- --------- --------- ------------------------------
001500131017     ** 1         IN  CHARACTER 20        Numero cellulare da verificare
001600131017     ** 2         OUT CHARACTER 20        Numero cellulare normalizzato
001700131017     ** 3         OUT CHARACTER  1        Esito verifica
001800141106     ** 4         IN  CHARACTER  1        Opzione di verifica
001900081003     ** --------- --- --------- --------- ------------------------------
002000071128     **
002100071128     **
002200071128     ** ESEMPI DI CHIAMATA
002300071128     **
002400131017     ** if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) = 0
002500071128     **
002600071128     ** ISTRUZIONI PER LA COMPILAZIONE
002700071128     **
002800140521     ** 1. Creare il modulo UBCHKCEL (Opz 15 PDM)
002900141106     ** 2. Creare/Aggiornare il programma di servizio UBCHKCEL (CRTSRVPGM / UPDSRVPGM)
003000080610     ** 4. Cancellare il modulo.
003100071128     **
003200071128     ***********************************************************************************************
003300071128
003400071128      //********************************************************************************************
003500071128      //
003600071128      // Definizione prototipi procedure.
003700071128      //
003800071128      //********************************************************************************************
003900131017     D/COPY GAITRASRC/SRCPROTOPR,UBCHKCEL
004000071128
004100071128      //********************************************************************************************
004200071128      //
004300071128      // Definizione variabili globali.
004400071128      //
004500071128      //********************************************************************************************
004600080612     D
004700071128
004800080611     P*--------------------------------------------------
004900131017     P* Procedure name: UBCHKCEL_Check...
005000131017     P* Purpose:        Controllo / normalizzaz. numero cellulare
005100131017     P* Returns:        Integer (Status_Code)
005200131017     P* Parameter:      pInCell        => Numero cellulare da verificare
005300131017     P* Parameter:      pOutCell       => Numero cellulare normalizzato
005400131017     P* Parameter:      pOutErr        => Esito verifica
005500141106     P* Parameter:      pInCellOpz     => Opzione di verifica
005600080611     P*--------------------------------------------------
005700080611     P*
005800131017     P UBCHKCEL_Check...
005900071129     P                 B                   EXPORT
006000071129     D*
006100131017     D UBCHKCEL_Check...
006200080612     D                 PI            10I 0
006300131017     D   pInCell                     20A
006400131017     D   pOutCell                    20A
006500131017     D   pOutErr                      1A
006600141106     D   pInCellOpz                   1A   options(*nopass)
006700080612
006800081020     D*
006900081020     D* varibili d wrk
007000141119     D wPos            S              2s 0 inz
007100141119     D wLPref          S              1s 0 inz
007200141119     D wLTel           S              2s 0 inz
007300131017
007400131017     D*
007500131017     D* Costanti
007600141222     D caratteri       c                   const('1234567890+')
007700131017     D numeri          c                   const('1234567890')
007800080612
007900080612      //********************************************************************************************
008000080612      //
008100080612      // Definizione variabili work.
008200080612      //
008300080612      //********************************************************************************************
008400081003
008500081003
008600081003     C*
008700081003     C* Inizializzazioni
008800131017      /free
008900131017
009000131017           // Inizializzazioni
009100131017           clear pOutCell;
009200131017           pOutErr = 'E';
009300131017
009400131017           // normalizzazione numero di cellulare
009500141222           // 1° giro => tolgo tutto ciò che non è tra i caratteri consentiti
009600131017           wPos = 1;
009700131017           pOutCell = %trim(pInCell);
009800131017           dow wPos <= %len(pOutCell);
009900141222               if %scan(%subst(pOutCell:wPos:1):caratteri:1) > *zeros;
010000131017               else;
010100131017                  %subst(pOutCell:wPos:1) = *blanks;
010200131017               endif;
010300141222
010400141222               // tolgo anche gli zeri non significativi (a sx)
010500141222               //if %subst(pOutCell:wPos:1) = *zero AND not *in35;
010600141222               //   %subst(pOutCell:wPos:1) = *blanks;
010700141222               //else;
010800141222               //   if %subst(pOutCell:wPos:1) <> *blanks;
010900141222               //      *in35 = *on;
011000141222               //   endif;
011100141222               //endif;
011200141222
011300131017               wPos = wPos + 1;
011400131017           enddo;
011500131017
011600141222
011700131017           // 2° giro => compatto tutti i *blanks (da precedente normalizz.)
011800131017           wPos = 1;
011900131017           pOutCell = %trim(pOutCell);
012000131017           dow wPos <= %len(%trim(pOutCell));
012100131017               if %subst(pOutCell:wPos:1) = *blanks;
012200131017                  pOutCell = %subst(pOutCell:1:wPos-1) +
012300131017                           %subst(pOutCell:wPos+1);
012400131017                  wPos = wPos - 1;
012500131017               endif;
012600131017               wPos = wPos + 1;
012700131017           enddo;
012800131017
012900141222
013000141222           // 3° giro => effettuo considerazioni relative a prefisso internazionale (DFT Italia)
013100131017           pOutCell = %trim(pOutCell);
013200141119           wLTel = %len(%trim(pOutCell));
013300141120           wLPref = 0;
013400141119           if wLTel < 11;
013500141106             // valorizzo il prefisso a seconda dell'opzione
013600141106             select;
013700141106               // nessuna opzione = prefisso 39
013800141222               when %parms <= 3 or pInCellOpz = *blanks;
013900141222                 select;
014000141222                    when %subst(pOutCell:1:3) = '+39';
014100141222                      eval pOutCell = %scanrpl('+39':'39':pOutCell:1:3);
014200141222                      wLPref = -1;
014300141222                    when %subst(pOutCell:1:4) = '0039';
014400141222                      eval pOutCell = %scanrpl('0039':'39':pOutCell:1:4);
014500141222                      wLPref = -2;
014600141222                    other;
014700141222                      pOutCell = '39' + %trim(pOutCell);
014800141222                      wLPref = 2;
014900141222                 endsl;
015000141222
015100141106               // opzione '0' = non passare prefisso
015200141106               when %parms > 3 and pInCellOpz = '0';
015300141119                 wLPref = 0;
015400141106                 // non faccio nulla
015500141222
015600141106               // opzione '1' = passare prefisso 0039
015700141106               when %parms > 3 and pInCellOpz = '1';
015800141222                 select;
015900141222                    when %subst(pOutCell:1:3) = '+39';
016000141222                      eval pOutCell = %scanrpl('+39':'0039':pOutCell:1:3);
016100141222                      wLPref = 1;
016200141222                    when %subst(pOutCell:1:2) = '39';
016300141222                      eval pOutCell = %scanrpl('39':'0039':pOutCell:1:2);
016400141222                      wLPref = 2;
016500141222                    other;
016600141222                      pOutCell = '0039' + %trim(pOutCell);
016700141222                      wLPref = 4;
016800141222                 endsl;
016900141222
017000141222               // opzione '2' = passare prefisso +39
017100141106               when %parms > 3 and pInCellOpz = '2';
017200141222                 select;
017300141222                    when %subst(pOutCell:1:2) = '39';
017400141222                      eval pOutCell = %scanrpl('39':'+39':pOutCell:1:2);
017500141222                      wLPref = 1;
017600141222                    when %subst(pOutCell:1:4) = '0039';
017700141222                      eval pOutCell = %scanrpl('0039':'+39':pOutCell:1:4);
017800141222                      wLPref = -1;
017900141222                    other;
018000141222                      pOutCell = '+39' + %trim(pOutCell);
018100141222                      wLPref = 3;
018200141222                 endsl;
018300141222
018400141106             endsl;
018500131017           endif;
018600141222
018700141222
018800141222           // Considerazioni finali
018900141120           if %len(%trim(pOutCell)) - wLPref < 9
019000141120           or %len(%trim(pOutCell)) - wLPref > 14;
019100131017              pOutErr = 'E';
019200131017           else;
019300131017              pOutErr = 'N';
019400131017           endif;
019500131017
019600141222
019700131017           // Valorizzo l'output chiamata
019800131017           if %error;
019900131017              return -1;
020000131017           else;
020100131017              return 0;
020200131017           endif;
020300131017
020400131017      /end-free
020500071129
020600131017     P UBCHKCEL_Check...
020700071129     P                 E
