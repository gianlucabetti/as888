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
002900170630     ** 2. Creare/Aggiornare il programma di servizio UBCHKCEL (CRTSRVPGM / UPDSRVPGM)
003000170630     **    mediante sorgente BND con firma esplicita
003100170630     **    CRTSRVPGM SRVPGM(GAITRAFIL/UBCHKCEL) EXPORT(*SRCFILE)
003200170630     **    SRCFILE(GAITRASRC/SRCSRVPGM)
003300080610     ** 4. Cancellare il modulo.
003400071128     **
003500071128     ***********************************************************************************************
003600071128
003700071128      //********************************************************************************************
003800071128      //
003900071128      // Definizione prototipi procedure.
004000071128      //
004100071128      //********************************************************************************************
004200131017     D/COPY GAITRASRC/SRCPROTOPR,UBCHKCEL
004300071128
004400071128      //********************************************************************************************
004500071128      //
004600071128      // Definizione variabili globali.
004700071128      //
004800071128      //********************************************************************************************
004900080612     D
005000071128
005100080611     P*--------------------------------------------------
005200131017     P* Procedure name: UBCHKCEL_Check...
005300131017     P* Purpose:        Controllo / normalizzaz. numero cellulare
005400131017     P* Returns:        Integer (Status_Code)
005500131017     P* Parameter:      pInCell        => Numero cellulare da verificare
005600131017     P* Parameter:      pOutCell       => Numero cellulare normalizzato
005700131017     P* Parameter:      pOutErr        => Esito verifica
005800141106     P* Parameter:      pInCellOpz     => Opzione di verifica
005900080611     P*--------------------------------------------------
006000080611     P*
006100131017     P UBCHKCEL_Check...
006200071129     P                 B                   EXPORT
006300071129     D*
006400131017     D UBCHKCEL_Check...
006500080612     D                 PI            10I 0
006600131017     D   pInCell                     20A
006700131017     D   pOutCell                    20A
006800131017     D   pOutErr                      1A
006900161103      //  =aggiungo 39 / 0=non aggiungo prefisso / 1=aggiungo 0039 / 2=aggiungo +39
007000141106     D   pInCellOpz                   1A   options(*nopass)
007100080612
007200081020     D*
007300081020     D* varibili d wrk
007400141119     D wPos            S              2s 0 inz
007500141119     D wLPref          S              1s 0 inz
007600141119     D wLTel           S              2s 0 inz
007700150119     D fine0NonSign    S               n   inz
007800131017
007900131017     D*
008000131017     D* Costanti
008100131017     D numeri          c                   const('1234567890')
008200150119     D numNon0         c                   const('123456789')
008300080612
008400080612      //********************************************************************************************
008500080612      //
008600080612      // Definizione variabili work.
008700080612      //
008800080612      //********************************************************************************************
008900081003
009000081003
009100081003     C*
009200081003     C* Inizializzazioni
009300131017      /free
009400131017
009500131017           // Inizializzazioni
009600131017           clear pOutCell;
009700131017           pOutErr = 'E';
009800131017
009900131017           // normalizzazione numero di cellulare
010000131017           // 1° giro => tolgo tutto ciò che non è numero e gli zeri
010100131017           //            non significativi ai fini del numero di cell
010200131017           *in35 = *off;
010300150119           *in36 = *off;
010400150119           fine0NonSign = *off;
010500131017           wPos = 1;
010600131017           pOutCell = %trim(pInCell);
010700131017           dow wPos <= %len(pOutCell);
010800150119             //sostituisco i char non numerici con blank
010900150119             if %scan(%subst(pOutCell:wPos:1):numeri:1) > *zeros;
011000150119             else;
011100150119               %subst(pOutCell:wPos:1) = *blanks;
011200150119             endif;
011300150119             //se l'opzione NON è 0
011400150119             if %parms <= 3 OR
011500150119               (%parms >3 and
011600150119                pInCellOpz <> '0');
011700150119               //compatto tutti gli zeri iniziali in modo da lasciarne solo 2
011800150119               //siccome cerco gli zeri a blocchi di 3 devo fermare questo test prima della fine
011900150119               //della variabile
012000150119               if wPos < %len(pOutCell) - 2 AND
012100150119                  %subst(pOutCell:wPos:3) = '000' AND not *in35 AND not *in36;
012200131017                  %subst(pOutCell:wPos:1) = *blanks;
012300150119                  pOutCell = %scanrpl('000' : '00' : pOutCell : wPos : 3);
012400131017               else;
012500131017                  if %subst(pOutCell:wPos:1) <> *blanks;
012600131017                     *in35 = *on;
012700131017                  endif;
012800150119                  //appena trovo una cifra diversa da 0 il compattamento zeri INIZIALI deve finire
012900150119                  if *in36 = *off and
013000150119                   %scan(%subst(pOutCell:wPos:1):numNon0:1) > *zeros;
013100150119                     *in36 = *on;
013200150119                  endif;
013300131017               endif;
013400150119             //se l'opzione è 0
013500150119             else;
013600150119               //appena trovo una cifra diversa da '0' non eseguo più la normalizzazione degli
013700150119               //zeri non significativi
013800150119               if %scan(%subst(pOutCell:wPos:1):numNon0:1) > *zeros and
013900150119                %subst(pOutCell:wPos:1) <> ' ' and fine0NonSign = *off;
014000150119                 fine0NonSign = *on;
014100150119               endif;
014200150119               //la uso come normmalizzazione che toglie gli 0 non significativi
014300150119               if %subst(pOutCell:wPos:1) = '0' and fine0NonSign = *off;
014400150119                 %subst(pOutCell:wPos:1) = *blank;
014500150119               endif;
014600150119             endif;
014700150119             wPos = wPos + 1;
014800131017           enddo;
014900131017
015000131017           // 2° giro => compatto tutti i *blanks (da precedente normalizz.)
015100131017           wPos = 1;
015200131017           pOutCell = %trim(pOutCell);
015300131017           dow wPos <= %len(%trim(pOutCell));
015400131017               if %subst(pOutCell:wPos:1) = *blanks;
015500131017                  pOutCell = %subst(pOutCell:1:wPos-1) +
015600131017                           %subst(pOutCell:wPos+1);
015700131017                  wPos = wPos - 1;
015800131017               endif;
015900131017               wPos = wPos + 1;
016000131017           enddo;
016100131017
016200131017           // verifica presenza presunta prefisso internazionale (DFT Italia)
016300131017           pOutCell = %trim(pOutCell);
016400150119           // nessuna opzione = prefisso 39
016500150119           if %parms <= 3 OR pInCellOpz = *blank;
016600150119             select;
016700150119              when %subst(pOutCell:1:3) = '+39';
016800150119               eval pOutCell = %scanrpl('+39':'39':pOutCell:1:3);
016900150119              when %subst(pOutCell:1:4) = '0039';
017000150119               eval pOutCell = %scanrpl('0039':'39':pOutCell:1:4);
017100150119             endsl;
017200150119           endif;
017300150119           // normalizzo controllando cosa contengono i primi char
017400141119           wLTel = %len(%trim(pOutCell));
017500141120           wLPref = 0;
017600150119           //numero cellulare non valido come lunghezza
017700170630           //if wLTel < 11;
017800170630           if wLTel <= 11;
017900141106             // valorizzo il prefisso a seconda dell'opzione
018000141106             select;
018100141106               // nessuna opzione = prefisso 39
018200150119               when %parms <= 3 OR pInCellOpz = *blank;
018300150119                 pOutCell = '39' + %trim(pOutCell);
018400150119                 wLPref = 2;
018500141106               // opzione '0' = non passare prefisso
018600141106               when %parms > 3 and pInCellOpz = '0';
018700141119                 wLPref = 0;
018800141106                 // non faccio nulla
018900150119               // opzione '1' = passare prefisso 0039 (solo se già non c'è)
019000150119               when %parms > 3 and pInCellOpz = '1' and
019100150119                %subst(pInCell : 1 : 4) <> '0039';
019200141119                 wLPref = 4;
019300141106                 pOutCell = '0039' + %trim(pOutCell);
019400150119               // opzione '2' = passare prefisso +39 (solo se già non c'è)
019500150119               when %parms > 3 and pInCellOpz = '2' and
019600150119                %subst(pInCell : 1 : 1) <> '+';
019700141119                 wLPref = 3;
019800141106                 pOutCell = '+39' + %trim(pOutCell);
019900141106             endsl;
020000131017           endif;
020100161103
020200161103           // se l'opzione d'ingresso è 2
020300161103           // e il numero cellulare normalizzato ha una lunghezza tale per cui è presumibile
020400161103           // pensare che comprenda già il prefisso internazionale
020500161104           if %parms > 3 and pInCellOpz = '2' and
020600170630              //%len(%trim(pOutCell)) >=11 and
020700170630              %len(%trim(pOutCell)) > 11 and
020800161104              %len(%trim(pOutCell)) <=14;
020900161104             // se in testa ci sono due zeri (00)
021000161104             if %subst(%trim(pOutCell) : 1 : 2) = '00';
021100161104             // li elimino sostituendoli col +
021200161104               pOutCell = %scanrpl('00':'+':%trim(pOutCell):1:2);
021300161104             endif;
021400161104             // se non c'è il + in testa
021500161104             if %subst(%trim(pOutCell) : 1 : 1) <> '+';
021600161104             // lo aggiungo
021700161103               pOutCell = '+' + %trim(pOutCell);
021800161103             endif;
021900161103           endif;
022000161103
022100141120           if %len(%trim(pOutCell)) - wLPref < 9
022200141120           or %len(%trim(pOutCell)) - wLPref > 14;
022300131017              pOutErr = 'E';
022400131017           else;
022500131017              pOutErr = 'N';
022600131017           endif;
022700131017
022800131017           // Valorizzo l'output chiamata
022900131017           if %error;
023000131017              return -1;
023100131017           else;
023200131017              return 0;
023300131017           endif;
023400131017
023500131017      /end-free
023600071129
023700131017     P UBCHKCEL_Check...
023800071129     P                 E
