000100091228       //==============================================================
000200130314       // Sostituzione maschera - PDF to SPOOL
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100091228
001200091228       //--------------------------------------------------------------
001300121106       // Specifiche di controllo.
001400091228       //--------------------------------------------------------------
001500091223
001600091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700100325     h dftactgrp(*no)
001800091223     h alwnull(*inputonly)
001900100325     h bnddir('UBBNDDIR')
002000091223
002100091223       //--------------------------------------------------------------
002200121106       // Dichiarazione file.
002300091223       //--------------------------------------------------------------
002400100112
002500091223
002600091223       //--------------------------------------------------------------
002700121106       // Definizione costanti.
002800091223       //--------------------------------------------------------------
002900100302
003000091223
003100091223       //--------------------------------------------------------------
003200121106       // Definizione schiere.
003300091223       //--------------------------------------------------------------
003400091223
003500091223
003600091223       //--------------------------------------------------------------
003700121106       // Definizione aree dati.
003800091223       //--------------------------------------------------------------
003900091223
004000091223
004100091223       //--------------------------------------------------------------
004200121106       // Definizione strutture dati.
004300091223       //--------------------------------------------------------------
004400110516
004500130314     D TIVLR00F      e ds
004600130314     D FIVAB00F      e ds
004700121106       // - Status
004800110516     d Psds           sds
004900110516     d   SDSpgm          *proc
005000121106     d   JobName             244    253                                         Job name
005100121106     d   JobUser             254    263                                         User name
005200121106     d   JobNumber           264    269s 0                                      Job number
005300091223
005400091223       //--------------------------------------------------------------
005500121106       // Definizione variabili.
005600091223       //--------------------------------------------------------------
005700091223
005800121106       // - Parametri ricevuti:
005900130307     d pIn_MaskPDF     s             50a
006000130314     d pOut_MaskPDF    s             50a
006100130307     d pOut_Esito      s              1a
006200091223
006300121106       // Stringa SQL da eseguire
006400130108     d wSQL            s           5000    inz  varying
006500121106
006600121106       // Campi di comodo
006700130307     d wMaskPDF        s             50a
006800121106     d wDate           s              8  0 inz
006900130307     d wEsito          s              1a
007000130307
007100130307       // parametri XCHKCHAR
007200130307     D TxtInOut        S           2048
007300130307     D ElencoChar      S            256
007400130307     D TipoElenco      S              1
007500130307     D CharSost        S              1
007600130307     D UpperCase       S              1
007700130307     D ChkNull         S              1
007800130307     D CharNull        S              1
007900130307     D EsitoBon        S              1
008000091223
008100091223       //--------------------------------------------------------------
008200121106       // Definizione prototipi procedure.
008300091223       //--------------------------------------------------------------
008400130307
008500130314
008600091223       //--------------------------------------------------------------
008700121106       // Definizione key-list.
008800091223       //--------------------------------------------------------------
008900091223
009000091223
009100091223       //--------------------------------------------------------------
009200121107       // Definizione parametri procedura.
009300091223       //--------------------------------------------------------------
009400091223
009500091223     c     *Entry        plist
009600130314     c                   parm                    FIVAB00F
009700130307     c                   parm                    pIn_MaskPDF
009800130314     c                   parm                    pOut_MaskPDF
009900130307     c                   parm                    pOut_Esito
010000091223
010100130307      /free
010200091223
010300091223       //--------------------------------------------------------------
010400121106       // M A I N - L I N E
010500091223       //--------------------------------------------------------------
010600130307
010700130307       // Operazioni iniziali?
010800130307       exsr sr_RoutInz;
010900130307
011000130314       // sostituzione maschera
011100130314       exsr sostituzMask;
011200091223
011300121106       // Operazioni finali?
011400091223       exsr sr_RoutEnd;
011500091223
011600091223       //--------------------------------------------------------------
011700121107       // Operazioni iniziali.
011800091223       //--------------------------------------------------------------
011900091223       BEGSR  sr_RoutInz;
012000091223
012100091223         exec sql  set option  dynusrprf = *owner,
012200121107                               closqlcsr = *endmod;
012300130307
012400130307         pOut_Esito = '1';
012500130314         pOut_MaskPDF = *blank;
012600121108
012700121108         // Reperimento data odierna (fmt aaaa/mm/gg)
012800121108         wDate = %dec( %date() );
012900091223
013000091223       ENDSR;
013100130307
013200130307       //--------------------------------------------------------------
013300130314       // Sostituzione maschera
013400130307       //--------------------------------------------------------------
013500130314       BEGSR  sostituzMask;
013600130307
013700130307
013800130307         //sostituzione
013900130307
014000130307         wMaskPDF = pIn_MaskPDF;
014100130307
014200130307         //eseguo per tutte le tipologie di costanti figurative da sostituire con valori ricevuti
014300130307         //&RMN*
014400130307         if %scan('&RMN*':wMaskPDF) > 0;
014500130307           wMaskPDF = %replace(%editc(VABRMN:'X') : wMaskPDF :
014600130307                        %scan('&RMN*':wMaskPDF) : 5);
014700130307         endif;
014800130307         //&0RMN*
014900130307         if %scan('&0RMN*':wMaskPDF) > 0;
015000130307           wMaskPDF = %replace(%trim(%char(VABRMN)) : wMaskPDF :
015100130307                         %scan('&0RMN*':wMaskPDF) : 6);
015200130307         endif;
015300130307         //&RMA*
015400130307         if %scan('&RMA*':wMaskPDF) > 0;
015500130307           wMaskPDF = %replace(%trim(VABRMA) : wMaskPDF :
015600130307                         %scan('&RMA*':wMaskPDF) : 5);
015700130307         endif;
015800130307         //&NAS*
015900130314         if %scan('&NAS*':wMaskPDF) > 0;
016000130307           wMaskPDF = %replace(%trim(VABNAS) : wMaskPDF :
016100130314                         %scan('&NAS*':wMaskPDF) : 5);
016200130307         endif;
016300130307         //&CCM*
016400130307         if %scan('&CCM*':wMaskPDF) > 0;
016500130307           wMaskPDF = %replace(%editc(VABCCM:'X') : wMaskPDF :
016600130307                        %scan('&CCM*':wMaskPDF) : 5);
016700130307         endif;
016800140113         //&AAS4*
016900140113         if %scan('&AAS4*':wMaskPDF) > 0;
017000140113           wMaskPDF = %replace(%editc(VABAAS:'X') : wMaskPDF :
017100140114                        %scan('&AAS4*':wMaskPDF) : 6);
017200140113         endif;
017300140113         //&AAS2*
017400140113         if %scan('&AAS2*':wMaskPDF) > 0;
017500140114           wMaskPDF = %replace(%subst(%editc(VABAAS:'X') : 3 : 2) :
017600140114                        wMaskPDF :
017700140114                        %scan('&AAS2*':wMaskPDF) : 6);
017800140113         endif;
017900140113         //&AAC4*
018000140114         if %scan('&AAC4*':wMaskPDF) > 0;
018100140114           wMaskPDF = %replace(%subst(%editc(wDate:'X') : 1 : 4) :
018200140114                        wMaskPDF :
018300140114                        %scan('&AAC4*':wMaskPDF) : 6);
018400140113         endif;
018500140113         //&AAC2*
018600140114         if %scan('&AAC2*':wMaskPDF) > 0;
018700140114           wMaskPDF = %replace(%subst(%editc(wDate:'X') : 3 : 2) :
018800140114                        wMaskPDF :
018900140114                        %scan('&AAC2*':wMaskPDF) : 6);
019000140113         endif;
019100130307
019200130307         //il risultato non deve superare i 33 char
019300130307         if %len(%trim(wMaskPDF)) > 33;
019400130307           pOut_Esito = '2';
019500130307           exsr sr_RoutEnd;
019600130307         endif;
019700130307
019800130307         //controllo che il nome contenga solo lettere, cifre, il meno (-), l'underscore, il blank,
019900130307         //e il punto(.)
020000130307         TxtInOut   = wMaskPDF;
020100130307         exsr exeBon;
020200130307         //se trovato carattere non voluto, esco
020300130307         if EsitoBon <> '0';
020400130307           pOut_Esito = '2';
020500130307           exsr sr_RoutEnd;
020600130307         endif;
020700130314
020800130314         pOut_MaskPDF = wMaskPDF;
020900130307
021000130307       ENDSR;
021100121106
021200091223       //--------------------------------------------------------------
021300121107       // Operazioni finali.
021400091223       //--------------------------------------------------------------
021500091223       BEGSR  sr_RoutEnd;
021600130109
021700130109         // Chiusura pgm
021800130314         *inrt = *on;
021900130109         return;
022000091223
022100091223       ENDSR;
022200091223
022300091223      /end-free
022400130307      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
022500130307     C     exeBon        BEGSR
022600130307     C*
022700130307     C* Imposto i parametri x il *pgm d "bonifica"
022800130307     C                   EVAL      ElencoChar =
022900130307     C                              'qwertyuiopasdfghjklzxcvbnm' +
023000130307     C                              'QWERTYUIOPASDFGHJKLZXCVBNM' +
023100130406     C                              '1234567890 -_.*'
023200130307     C                   EVAL      TipoElenco = *blanks
023300130307     C                   EVAL      CharSost   = *blank
023400130307     C                   EVAL      UpperCase  = *blank
023500130307     C                   EVAL      ChkNull    = *blank
023600130307     C                   EVAL      CharNull   = *blanks
023700130307     C*
023800130307     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
023900130307     C                   EVAL      EsitoBon   = *blanks
024000130307     C*
024100130307     C                   CALL      'XCHKCHAR'
024200130307     C                   PARM                    TxtInOut
024300130307     C                   PARM                    ElencoChar
024400130307     C                   PARM                    TipoElenco
024500130307     C                   PARM                    CharSost
024600130307     C                   PARM                    UpperCase
024700130307     C                   PARM                    ChkNull
024800130307     C                   PARM                    CharNull
024900130307     C                   PARM                    EsitoBon
025000130307     C*
025100130307     C                   ENDSR
