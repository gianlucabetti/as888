000100110617      *---------------------------------------------------------------*
000200140207      * Invio ALERTS - SMS                                            *
000300110617      *---------------------------------------------------------------*
000400110617
000500110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600131017     h dftactgrp(*no) actgrp(*caller) bnddir('UBBNDDIR')
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001000170323     ftivgd00f  o    e             disk
001100130701
001200110617
001300130626      * - DS procedure esterne
001400170323     d psds           sds
001500170323     d  procname         *PROC
001600170323     d tis7vasds     e ds                  inz
001700130626
001800130626
001900110617      * - Campi di comodo
002000130626     d wProcedi        s              1    inz
002100131004     d currDate        s              8  0 inz
002200130704     d wSep            s              1    inz('|')
002300130719     d wEsito          s              1    inz
002400130704
002500130703
002600130703     d* DS di output
002700140207     d  InDS_SMS       ds
002800140207     d   i_MittSMS                   30
002900140207     d   i_IdSMS                     36
003000140207     d   i_cell                      20
003100140207     d   i_message                 1600
003200130719
003300170323
003400170323     D tis7vasr1       PR
003500170323     D                                     extpgm('TIS7VASR1')
003600170323     D  tis7vasds                          like(tis7vasds)
003700130626
003800130719
003900110617
004000110617      // ? PROTOTIPI ?
004100131017      /copy gaitrasrc/srcprotopr,ubchkcel
004200131017      /copy gaitrasrc/srcprotopi,ubchkcel
004300110617
004400130406
004500110627     C     *ENTRY        PLIST
004600140207     C                   PARM                    InChiudi          1
004700140207     C                   PARM                    InOpz             1
004800140207     C                   PARM                    InKSU             8
004900140207     C                   PARM                    InTIP             2
005000140207     C                   PARM                    InTSC             2
005100140207     C                   PARM                    InPGM            10
005200170323     C                   PARM                    InPRG            10
005300140207     C                   PARM                    InDS_SMS
005400140207     C                   PARM                    OutPRG           10
005500170323     C                   PARM                    OutEsito          1
005600140207
005700140207
005800130406
005900110617      /free
006000140207
006100140207
006200140207       // Gestisco chiusura controllata
006300140207       if InChiudi = 'S' or %shtdn;
006400140207          *inlr = *on;
006500140207       else;
006600140207
006700140207          // inizializzazioni
006800140207          OutPRG   = *blanks;
006900170323          OutEsito = 'E';
007000170323          wProcedi = 'S';
007100140207
007200140207          // Inizializzo variabili di procedura
007300140207          currDate = %dec(%date() : *ISO);
007400140207
007500140207          // eseguo operazioni relative al opzione richiesta iin input
007600140207          select;
007700140207          when InOpz = 'S';
007800170323               exsr strVGD;
007900140207          when InOpz = 'W';
008000170323               exsr sr_Write;
008100140207          when InOpz = 'E';
008200170323               exsr endVGD;
008300140207          other;
008400140207               OutEsito = 'E';
008500140207          endsl;
008600140207
008700140207          *inrt = *on;
008800140207
008900140207       endif;
009000130719
009100130719
009200130406
009300110617       //-------------------------------------------------------------*
009400140207       //Esecuzione scrittura dati                                    *
009500110617       //-------------------------------------------------------------*
009600140207       Begsr sr_Write;
009700140207
009800140207         // Se consentita elaborazione => procedo
009900170323         if wProcedi = 'S';
010000130703
010100140207            // normalizzo numero di cellulare
010200140207            exsr sr_normCELL;
010300140207            if OutEsito <> 'E';
010400140207               exsr sr_inviaSMS;
010500140207            endif;
010600140207
010700140207         endif;
010800130406
010900130626       endsr;
011000130719
011100130719
011200130719
011300130719       //-------------------------------------------------------------*
011400130719       //Normalizza numero ci cellulare                               *
011500130719       //-------------------------------------------------------------*
011600130719        Begsr sr_normCELL;
011700130719
011800131017           // Chiamo il driver preposto
011900140207           pInCell = i_cell;
012000131017           pOutErr = *blanks;
012100131017           if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
012200131017           else;
012300170323              OutEsito = *blanks;
012400140207              i_cell = pOutCell;
012500131017           endif;
012600130719
012700130719        endsr;
012800130406
012900130626
013000130406
013100110617       //-------------------------------------------------------------*
013200130626       //Richiamo driver per invio SMS e aggiorno dos su fiar5        *
013300110617       //-------------------------------------------------------------*
013400130626        Begsr sr_inviaSMS;
013500130626
013600140207           clear tivgd000;
013700170323
013800170323           if InPGM = *blanks;
013900170323              InPGM = procname;
014000170323           endif;
014100160706
014200160706           i_message = %scanrpl(wSep:'':i_message);
014300160706
014400140207           vgddta = %trim(i_MittSMS) + wSep +
014500140207                    %trim(i_IdSMS)   + wSep +
014600140207                    %trim(i_cell)    + wSep +
014700140207                    %trim(i_message);
014800140207
014900140207           vgdtip = InTIP;
015000140207           vgdksu = InKSU;
015100140207           vgdtsc = InTSC;
015200140207           vgddat = currDate;
015300170323           vgdpgm = InPGM;
015400170323           vgdprg = InPRG;
015500170323           vgdsto = '?';
015600140207
015700140207           // Scarico il buffer di output finale
015800140207           write tivgd000;
015900130406
016000110617        endsr;
016100170323
016200170323
016300170323
016400170323       //-------------------------------------------------------------*
016500170323       // Avvio Inizio transazione TIVGD x file SMS
016600170323       //-------------------------------------------------------------*
016700170323        Begsr strVGD;
016800170323
016900170323          clear tis7vasds;
017000170323          i§7VASOPZ  = 'PRG';
017100170323          callP(e) tis7vasr1 (tis7vasds);
017200170323
017300170323          if %error OR o§7VASOK = *off;
017400170323             wProcedi = 'N';
017500170323          else;
017600170323             OutEsito = *blanks;
017700170323             OutPRG = o§7VASPRG;
017800170323          endif;
017900170323
018000170323        endsr;
018100170323
018200170323
018300170323
018400170323       //-------------------------------------------------------------*
018500170323       // Fine transazione TIVGD x file SMS
018600170323       //-------------------------------------------------------------*
018700170323        Begsr endVGD;
018800170323
018900170323          clear tis7vasds;
019000170323          I§7VASOPZ  = 'RLS';
019100170323          I§7VASTIP  = InTIP;
019200170323          I§7VASKSU  = InKSU;
019300170323          I§7VASTSC  = InTSC;
019400170323          I§7VASPRG  = InPRG;
019500170323          callP(e) tis7vasr1 (tis7vasds);
019600170323
019700170323          if %error OR o§7VASOK = *off;
019800170323             dump(A);
019900170323             rolbk(e);
020000170323             *inlr = *on;
020100170323             return;
020200170323          else;
020300170323             OutEsito = *blanks;
020400170323          endif;
020500170323
020600170323        endsr;
020700140207
020800140207
