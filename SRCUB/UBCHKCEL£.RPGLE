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
006600161103      //  =aggiungo 39 / 0=non aggiungo prefisso / 1=aggiungo 0039 / 2=aggiungo +39
006700141106     D   pInCellOpz                   1A   options(*nopass)
006800080612
006900081020     D*
007000081020     D* varibili d wrk
007100141119     D wPos            S              2s 0 inz
007200141119     D wLPref          S              1s 0 inz
007300141119     D wLTel           S              2s 0 inz
007400150119     D fine0NonSign    S               n   inz
007500131017
007600131017     D*
007700131017     D* Costanti
007800131017     D numeri          c                   const('1234567890')
007900150119     D numNon0         c                   const('123456789')
008000080612
008100080612      //********************************************************************************************
008200080612      //
008300080612      // Definizione variabili work.
008400080612      //
008500080612      //********************************************************************************************
008600081003
008700081003
008800081003     C*
008900081003     C* Inizializzazioni
009000131017      /free
009100131017
009200131017           // Inizializzazioni
009300131017           clear pOutCell;
009400131017           pOutErr = 'E';
009500131017
009600131017           // normalizzazione numero di cellulare
009700131017           // 1° giro => tolgo tutto ciò che non è numero e gli zeri
009800131017           //            non significativi ai fini del numero di cell
009900131017           *in35 = *off;
010000150119           *in36 = *off;
010100150119           fine0NonSign = *off;
010200131017           wPos = 1;
010300131017           pOutCell = %trim(pInCell);
010400131017           dow wPos <= %len(pOutCell);
010500150119             //sostituisco i char non numerici con blank
010600150119             if %scan(%subst(pOutCell:wPos:1):numeri:1) > *zeros;
010700150119             else;
010800150119               %subst(pOutCell:wPos:1) = *blanks;
010900150119             endif;
011000150119             //se l'opzione NON è 0
011100150119             if %parms <= 3 OR
011200150119               (%parms >3 and
011300150119                pInCellOpz <> '0');
011400150119               //compatto tutti gli zeri iniziali in modo da lasciarne solo 2
011500150119               //siccome cerco gli zeri a blocchi di 3 devo fermare questo test prima della fine
011600150119               //della variabile
011700150119               if wPos < %len(pOutCell) - 2 AND
011800150119                  %subst(pOutCell:wPos:3) = '000' AND not *in35 AND not *in36;
011900131017                  %subst(pOutCell:wPos:1) = *blanks;
012000150119                  pOutCell = %scanrpl('000' : '00' : pOutCell : wPos : 3);
012100131017               else;
012200131017                  if %subst(pOutCell:wPos:1) <> *blanks;
012300131017                     *in35 = *on;
012400131017                  endif;
012500150119                  //appena trovo una cifra diversa da 0 il compattamento zeri INIZIALI deve finire
012600150119                  if *in36 = *off and
012700150119                   %scan(%subst(pOutCell:wPos:1):numNon0:1) > *zeros;
012800150119                     *in36 = *on;
012900150119                  endif;
013000131017               endif;
013100150119             //se l'opzione è 0
013200150119             else;
013300150119               //appena trovo una cifra diversa da '0' non eseguo più la normalizzazione degli
013400150119               //zeri non significativi
013500150119               if %scan(%subst(pOutCell:wPos:1):numNon0:1) > *zeros and
013600150119                %subst(pOutCell:wPos:1) <> ' ' and fine0NonSign = *off;
013700150119                 fine0NonSign = *on;
013800150119               endif;
013900150119               //la uso come normmalizzazione che toglie gli 0 non significativi
014000150119               if %subst(pOutCell:wPos:1) = '0' and fine0NonSign = *off;
014100150119                 %subst(pOutCell:wPos:1) = *blank;
014200150119               endif;
014300150119             endif;
014400150119             wPos = wPos + 1;
014500131017           enddo;
014600131017
014700131017           // 2° giro => compatto tutti i *blanks (da precedente normalizz.)
014800131017           wPos = 1;
014900131017           pOutCell = %trim(pOutCell);
015000131017           dow wPos <= %len(%trim(pOutCell));
015100131017               if %subst(pOutCell:wPos:1) = *blanks;
015200131017                  pOutCell = %subst(pOutCell:1:wPos-1) +
015300131017                           %subst(pOutCell:wPos+1);
015400131017                  wPos = wPos - 1;
015500131017               endif;
015600131017               wPos = wPos + 1;
015700131017           enddo;
015800131017
015900131017           // verifica presenza presunta prefisso internazionale (DFT Italia)
016000131017           pOutCell = %trim(pOutCell);
016100150119           // nessuna opzione = prefisso 39
016200150119           if %parms <= 3 OR pInCellOpz = *blank;
016300150119             select;
016400150119              when %subst(pOutCell:1:3) = '+39';
016500150119               eval pOutCell = %scanrpl('+39':'39':pOutCell:1:3);
016600150119              when %subst(pOutCell:1:4) = '0039';
016700150119               eval pOutCell = %scanrpl('0039':'39':pOutCell:1:4);
016800150119             endsl;
016900150119           endif;
017000150119           // normalizzo controllando cosa contengono i primi char
017100141119           wLTel = %len(%trim(pOutCell));
017200141120           wLPref = 0;
017300150119           //numero cellulare non valido come lunghezza
017400141119           if wLTel < 11;
017500141106             // valorizzo il prefisso a seconda dell'opzione
017600141106             select;
017700141106               // nessuna opzione = prefisso 39
017800150119               when %parms <= 3 OR pInCellOpz = *blank;
017900150119                 pOutCell = '39' + %trim(pOutCell);
018000150119                 wLPref = 2;
018100141106               // opzione '0' = non passare prefisso
018200141106               when %parms > 3 and pInCellOpz = '0';
018300141119                 wLPref = 0;
018400141106                 // non faccio nulla
018500150119               // opzione '1' = passare prefisso 0039 (solo se già non c'è)
018600150119               when %parms > 3 and pInCellOpz = '1' and
018700150119                %subst(pInCell : 1 : 4) <> '0039';
018800141119                 wLPref = 4;
018900141106                 pOutCell = '0039' + %trim(pOutCell);
019000150119               // opzione '2' = passare prefisso +39 (solo se già non c'è)
019100150119               when %parms > 3 and pInCellOpz = '2' and
019200150119                %subst(pInCell : 1 : 1) <> '+';
019300141119                 wLPref = 3;
019400141106                 pOutCell = '+39' + %trim(pOutCell);
019500141106             endsl;
019600131017           endif;
019700161103
019800161103           // se l'opzione d'ingresso è 2
019900161103           // e il numero cellulare normalizzato ha una lunghezza tale per cui è presumibile
020000161103           // pensare che comprenda già il prefisso internazionale
020100161104           if %parms > 3 and pInCellOpz = '2' and
020200161103              %len(%trim(pOutCell)) >=11 and
020300161104              %len(%trim(pOutCell)) <=14;
020400161104             // se in testa ci sono due zeri (00)
020500161104             if %subst(%trim(pOutCell) : 1 : 2) = '00';
020600161104             // li elimino sostituendoli col +
020700161104               pOutCell = %scanrpl('00':'+':%trim(pOutCell):1:2);
020800161104             endif;
020900161104             // se non c'è il + in testa
021000161104             if %subst(%trim(pOutCell) : 1 : 1) <> '+';
021100161104             // lo aggiungo
021200161103               pOutCell = '+' + %trim(pOutCell);
021300161103             endif;
021400161103           endif;
021500161103
021600141120           if %len(%trim(pOutCell)) - wLPref < 9
021700141120           or %len(%trim(pOutCell)) - wLPref > 14;
021800131017              pOutErr = 'E';
021900131017           else;
022000131017              pOutErr = 'N';
022100131017           endif;
022200131017
022300131017           // Valorizzo l'output chiamata
022400131017           if %error;
022500131017              return -1;
022600131017           else;
022700131017              return 0;
022800131017           endif;
022900131017
023000131017      /end-free
023100071129
023200131017     P UBCHKCEL_Check...
023300071129     P                 E
