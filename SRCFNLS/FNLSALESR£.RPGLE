000100110617      *---------------------------------------------------------------*
000200140207      * Invio ALERTS - SMS                                            *
000300110617      *---------------------------------------------------------------*
000400110617
000500110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600131017     h dftactgrp(*no) actgrp(*caller) bnddir('UBBNDDIR')
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001000130626     ftivgd00f  uf a e             disk
001100130701
001200110617
001300130626      * - DS procedure esterne
001400130626     d trul47ds      e ds
001500130719     d tis781ds      e ds
001600130719     d tis781dflo    e ds
001700130719     d dvgdflo       e ds
001800130626
001900130626
002000110617      * - Campi di comodo
002100130626     d wProcedi        s              1    inz
002200131004     d currDate        s              8  0 inz
002300130704     d wSep            s              1    inz('|')
002400130719     d wEsito          s              1    inz
002500130704
002600130703
002700130703     d* DS di output
002800140207     d  InDS_SMS       ds
002900140207     d   i_MittSMS                   30
003000140207     d   i_IdSMS                     36
003100140207     d   i_cell                      20
003200140207     d   i_message                 1600
003300130626
003400130626
003500130701     d trul47r         PR
003600130701     d                                     EXTPGM('TRUL47R')
003700130701     d  trul47ds                           likeds(trul47ds)
003800130719
003900130719
004000130719     d tis781c1        PR
004100130719     d                                     extpgm('TIS781C1')
004200130719     d  esito                         1
004300130719     d  tis781ds                           likeds(tis781ds)
004400130719     d  vgdPRG                             like(vgdPRG)
004500130626
004600130719
004700110617
004800110617      // ? PROTOTIPI ?
004900131017      /copy gaitrasrc/srcprotopr,ubchkcel
005000131017      /copy gaitrasrc/srcprotopi,ubchkcel
005100110617
005200130406
005300110627     C     *ENTRY        PLIST
005400140207     C                   PARM                    InChiudi          1
005500140207     C                   PARM                    InOpz             1
005600140207     C                   PARM                    InKSU             8
005700140207     C                   PARM                    InTIP             2
005800140207     C                   PARM                    InTSC             2
005900140207     C                   PARM                    InPGM            10
006000140731     C                   PARM                    InFLOELA          1
006100140207     C                   PARM                    InDS_SMS
006200140207     C                   PARM                    OutPRG           10
006300140207     C                   PARM                    OutEsito          1
006400140207
006500140207
006600130406
006700110617      /free
006800140207
006900140207
007000140207       // Gestisco chiusura controllata
007100140207       if InChiudi = 'S' or %shtdn;
007200140207          *inlr = *on;
007300140207       else;
007400140207
007500140207          // inizializzazioni
007600140207          OutPRG   = *blanks;
007700140207          OutEsito = *blanks;
007800140731          wProcedi = 'S';
007900140207
008000140207          // Inizializzo variabili di procedura
008100140207          currDate = %dec(%date() : *ISO);
008200140207
008300140207          // eseguo operazioni relative al opzione richiesta iin input
008400140207          select;
008500140207          when InOpz = 'S';
008600140207               exsr sr_Start;
008700140207          when InOpz = 'W';
008800140207               exsr sr_Write;
008900140207          when InOpz = 'E';
009000140207               exsr sr_End;
009100140207          when InOpz = 'F';
009200140207               exsr sr_Forza;
009300140207          other;
009400140207               OutEsito = 'E';
009500140207          endsl;
009600140207
009700140207          *inrt = *on;
009800140207
009900140207       endif;
010000130626
010100130626
010200130626
010300130626       //-------------------------------------------------------------*
010400130626       //Avvio il blocco elaborazione TIVGD x tipo file in questione  *
010500130626       //-------------------------------------------------------------*
010600140207        Begsr sr_Start;
010700130626
010800130626          clear trul47ds;
010900130626          d47opz  = 'I';
011000140207          d47tip  = InTIP;
011100130626          d47lck  = 'N';
011200130626          d47chkj = 'S';
011300140207          d47pgm  = InPGM;
011400130626          callP(e) TRUL47R (trul47ds);
011500130626
011600140731          if d47sts <> 'A';
011700140731          else;
011800140731             wProcedi = 'N';
011900130626          endif;
012000130626
012100130626        endsr;
012200130626
012300130626
012400130626
012500130626       //-------------------------------------------------------------*
012600130626       //Elimino il blocco elaborazione TIVGD x tipo file in questione*
012700130626       //-------------------------------------------------------------*
012800140207        Begsr sr_End;
012900130626
013000130626          if wProcedi = 'S';
013100130626             clear trul47ds;
013200130626             d47opz  = 'F';
013300140207             d47tip  = InTIP;
013400130626             callP(e) TRUL47R (trul47ds);
013500130626          endif;
013600130626
013700130626        endsr;
013800130719
013900130719
014000130719
014100130719       //-------------------------------------------------------------*
014200130719       //Forzo l'esecuzione del download                              *
014300130719       //-------------------------------------------------------------*
014400140207        Begsr sr_Forza;
014500130719
014600130719          // Forzo lo scarico del buffer d output del TIVGD
014700130719          feod tivgd00f;
014800130719
014900130719          clear tis781ds;
015000140207          §781tip = InTIP;
015100140207          §781ksu = InKSU;
015200140207          §781tsc = InTSC;
015300131004          §781dat = currDate;
015400140207          §781pgm = InPGM;
015500130719
015600131004          clear tis781dflo;
015700130719          §781flocsi = 'S';
015800140731          §781floela = InFLOELA;
015900130719          §781flo    = tis781dflo;
016000130719
016100130719          wEsito = *blanks;
016200140207          callP(e) TIS781C1  (wEsito : tis781ds : OutPRG);
016300140207          if wEsito <> *blanks;
016400140207             OutEsito = 'E';
016500140207          endif;
016600130719
016700130719        endsr;
016800130626
016900130406
017000130406
017100110617       //-------------------------------------------------------------*
017200140207       //Esecuzione scrittura dati                                    *
017300110617       //-------------------------------------------------------------*
017400140207       Begsr sr_Write;
017500140207
017600140207         // Se consentita elaborazione => procedo
017700140731         if wProcedi = 'S';
017800130719
017900140207            // inizializzazioni
018000140207            OutPRG   = *blanks;
018100140207            OutEsito = *blanks;
018200130703
018300140207            // normalizzo numero di cellulare
018400140207            exsr sr_normCELL;
018500140207            if OutEsito <> 'E';
018600140207               exsr sr_inviaSMS;
018700140207            endif;
018800140207
018900140207         endif;
019000130406
019100130626       endsr;
019200130719
019300130719
019400130719
019500130719       //-------------------------------------------------------------*
019600130719       //Normalizza numero ci cellulare                               *
019700130719       //-------------------------------------------------------------*
019800130719        Begsr sr_normCELL;
019900130719
020000131017           // Chiamo il driver preposto
020100140207           pInCell = i_cell;
020200131017           pOutErr = *blanks;
020300131017           if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
020400140207              OutEsito = 'E';
020500131017           else;
020600140207              i_cell = pOutCell;
020700131017           endif;
020800130719
020900130719        endsr;
021000130406
021100130626
021200130406
021300110617       //-------------------------------------------------------------*
021400130626       //Richiamo driver per invio SMS e aggiorno dos su fiar5        *
021500110617       //-------------------------------------------------------------*
021600130626        Begsr sr_inviaSMS;
021700130626
021800140207           clear tivgd000;
021900160706
022000160706           i_message = %scanrpl(wSep:'':i_message);
022100160706
022200140207           vgddta = %trim(i_MittSMS) + wSep +
022300140207                    %trim(i_IdSMS)   + wSep +
022400140207                    %trim(i_cell)    + wSep +
022500140207                    %trim(i_message);
022600140207
022700140207           vgdtip = InTIP;
022800140207           vgdksu = InKSU;
022900140207           vgdtsc = InTSC;
023000140207           vgddat = currDate;
023100140207           vgdpgm = InPGM;
023200140207
023300140207           clear dvgdflo;
023400140731           §vgdFELA = InFLOELA;
023500140207           vgdFLO   = dvgdflo;
023600140207
023700140207           // Scarico il buffer di output finale
023800140207           write tivgd000;
023900130406
024000110617        endsr;
024100140207
024200140207
