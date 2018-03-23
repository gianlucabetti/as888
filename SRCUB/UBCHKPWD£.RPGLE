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
007600131017     D numeri          c                   const('1234567890')
007700080612
007800080612      //********************************************************************************************
007900080612      //
008000080612      // Definizione variabili work.
008100080612      //
008200080612      //********************************************************************************************
008300081003
008400081003
008500081003     C*
008600081003     C* Inizializzazioni
008700131017      /free
008800131017
008900131017           // Inizializzazioni
009000131017           clear pOutCell;
009100131017           pOutErr = 'E';
009200131017
009300131017           // normalizzazione numero di cellulare
009400131017           // 1° giro => tolgo tutto ciò che non è numero e gli zeri
009500131017           //            non significativi ai fini del numero di cell
009600131017           *in35 = *off;
009700131017           wPos = 1;
009800131017           pOutCell = %trim(pInCell);
009900131017           dow wPos <= %len(pOutCell);
010000131017               if %scan(%subst(pOutCell:wPos:1):numeri:1) > *zeros;
010100131017               else;
010200131017                  %subst(pOutCell:wPos:1) = *blanks;
010300131017               endif;
010400131017               if %subst(pOutCell:wPos:1) = *zero AND not *in35;
010500131017                  %subst(pOutCell:wPos:1) = *blanks;
010600131017               else;
010700131017                  if %subst(pOutCell:wPos:1) <> *blanks;
010800131017                     *in35 = *on;
010900131017                  endif;
011000131017               endif;
011100131017               wPos = wPos + 1;
011200131017           enddo;
011300131017
011400131017           // 2° giro => compatto tutti i *blanks (da precedente normalizz.)
011500131017           wPos = 1;
011600131017           pOutCell = %trim(pOutCell);
011700131017           dow wPos <= %len(%trim(pOutCell));
011800131017               if %subst(pOutCell:wPos:1) = *blanks;
011900131017                  pOutCell = %subst(pOutCell:1:wPos-1) +
012000131017                           %subst(pOutCell:wPos+1);
012100131017                  wPos = wPos - 1;
012200131017               endif;
012300131017               wPos = wPos + 1;
012400131017           enddo;
012500131017
012600131017           // verifica presenza presunta prefisso internazionale (DFT Italia)
012700131017           pOutCell = %trim(pOutCell);
012800141119           wLTel = %len(%trim(pOutCell));
012900141120           wLPref = 0;
013000141119           if wLTel < 11;
013100141106             // valorizzo il prefisso a seconda dell'opzione
013200141106             select;
013300141106               // nessuna opzione = prefisso 39
013400141106               when %parms <= 3;
013500141119                 wLPref = 2;
013600141106                 pOutCell = '39' + %trim(pOutCell);
013700141106               // opzione '0' = non passare prefisso
013800141106               when %parms > 3 and pInCellOpz = '0';
013900141119                 wLPref = 0;
014000141106                 // non faccio nulla
014100141106               // opzione '1' = passare prefisso 0039
014200141106               when %parms > 3 and pInCellOpz = '1';
014300141119                 wLPref = 4;
014400141106                 pOutCell = '0039' + %trim(pOutCell);
014500141106               // opzione '2' = passare prefisso 0039
014600141106               when %parms > 3 and pInCellOpz = '2';
014700141119                 wLPref = 3;
014800141106                 pOutCell = '+39' + %trim(pOutCell);
014900141106             endsl;
015000131017           endif;
015100141120           if %len(%trim(pOutCell)) - wLPref < 9
015200141120           or %len(%trim(pOutCell)) - wLPref > 14;
015300131017              pOutErr = 'E';
015400131017           else;
015500131017              pOutErr = 'N';
015600131017           endif;
015700131017
015800131017           // Valorizzo l'output chiamata
015900131017           if %error;
016000131017              return -1;
016100131017           else;
016200131017              return 0;
016300131017           endif;
016400131017
016500131017      /end-free
016600071129
016700131017     P UBCHKCEL_Check...
016800071129     P                 E
