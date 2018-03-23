000100170412     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200171228
000300180214       // al momento gestisce solo come cliente:
000400180214       // 0898888 - CHE ANDRA' CAMBIATO CON GLI STANDARD FREE
000500180214       // 1664106
000600180214       // al momento gestisce solo come chiamante la routine UPLTO_DL_EB per:
000700180214       // 0898888 - CHE ANDRA' CAMBIATO CON GLI STANDARD FREE
000800180214       // 1664106
000900171228
001000170412
001100170412
001200170412      //********************************************************************************************
001300170412      //
001400170412      // Definizione prototipi procedure.
001500170412      //
001600170412      //********************************************************************************************
001700171228
001800171228       /copy gaitrasrc/srcProtoPR,QUSRSPLA
001900171228     d TRUL28R1        pr                  extpgm('TRUL28R1')
002000171228     D  pTRUL28DS                          like(TRUL28DS)
002100170412
002200170412      //********************************************************************************************
002300170412      //
002400170412      // Definizione interfacce procedure.
002500170412      //
002600170412      //********************************************************************************************
002700990907
002800010524
002900170419     D*
003000170824     D  EDIVABDS     e ds                  inz extname(edivabwr)
003100171228     D*
003200171228       // reperimento check digits
003300171228     D TRUL28DS      e ds                  inz
003400170822     D*
003500171228     D wCCM            s                   like(vabCCM)
003600171228     D wCaller         s             15
003700171228     D wBufferIO       s           2048
003800171228     D wLenBuff        s              4s 0
003900171228       // Campi di comodo
004000171228     d wBarcode        s             18a
004100170901     D*
004200170901     D psds           sds
004300170901     D  procname         *PROC
004400010525
004500010525
004600171228       // MAIN
004700171228
004800171228       // elenco clienti con buffer da personalizzare
004900171228       select;
005000171228        when wCCM = 0898888;
005100171228         exsr CCM_0898888;
005200180214        when wCCM = 1664106;
005300180214         exsr CCM_1664106;
005400171228       endsl;
005500171228
005600171228       return;
005700171228
005800171228       begsr CCM_0898888;
005900171228
006000171228       // elenco chiamate personalizzate per il cliente
006100171228         select;
006200171228          when wCaller = 'UPLTO_DL_EB';
006300171228           exsr UPLTO_DL_EB_CCM_0898888;
006400171228         endsl;
006500171228
006600171228       endsr;
006700171228
006800171228       begsr UPLTO_DL_EB_CCM_0898888;
006900171228
007000171228         // aggiungere il barcode alla fine del buffer
007100171228         EDIVABDS = wBufferIO;
007200171228
007300171228         // calcolo barcode 18 char (l'ultimo è il check digit)
007400171228         // compongo barcode e calcolo chek digit
007500171228         wBarcode = %editc(VABLNP:'X') +
007600171228                    %editc(VABLNA:'X') +
007700171228                    %editc(VABNRS:'X') +
007800171228                    %editc(VABNCD:'X') +
007900171228                    %editc(VABZNC:'X');
008000171228         // calcolo il check digit
008100171228         clear TRUL28DS;
008200171228         I28Mod  = 'BAR';
008300171228         I28COD = %trim(wBarcode);
008400171228         TRUL28R1(TRUL28DS);
008500171228         // Se tutto ok aggiungo il check digit
008600171228         if O28err = *blanks;
008700171228           wBarcode = O28Cod;
008800171228           wBufferIO = %subst(wBufferIO:1:wLenBuff) + O28Cod;
008900171228         endif;
009000171228
009100171228       endsr;
009200180214
009300180214       begsr CCM_1664106;
009400180214
009500180214       // elenco chiamate personalizzate per il cliente
009600180214         select;
009700180214          when wCaller = 'UPLTO_DL_EB';
009800180214           exsr UPLTO_DL_EB_CCM_1664106;
009900180214         endsl;
010000180214
010100180214       endsr;
010200180214
010300180214       begsr UPLTO_DL_EB_CCM_1664106;
010400180214
010500180214         // aggiungere il barcode alla fine del buffer
010600180214         EDIVABDS = wBufferIO;
010700180214
010800180214         // calcolo barcode 18 char (l'ultimo è il check digit)
010900180214         // compongo barcode e calcolo chek digit
011000180214         wBarcode = %editc(VABLNP:'X') +
011100180214                    %editc(VABLNA:'X') +
011200180214                    %editc(VABNRS:'X') +
011300180214                    %editc(VABNCD:'X') +
011400180214                    %editc(VABZNC:'X');
011500180214         // calcolo il check digit
011600180214         clear TRUL28DS;
011700180214         I28Mod  = 'BAR';
011800180214         I28COD = %trim(wBarcode);
011900180214         TRUL28R1(TRUL28DS);
012000180214         // Se tutto ok aggiungo il check digit
012100180214         if O28err = *blanks;
012200180214           wBarcode = O28Cod;
012300180214           wBufferIO = %subst(wBufferIO:1:wLenBuff) + O28Cod;
012400180214         endif;
012500180214
012600180214       endsr;
012700050920
012800000714      /TITLE Routine di *init PGM
012900000714     C*
013000000714     C     *inzsr        begsr
013100060103     C*
013200060103     C     *entry        plist
013300171228     C                   parm                    wCCM
013400171228     C                   parm                    wCaller
013500171228     C                   parm                    wBufferIO
013600171228     C                   parm                    wLenBuff
013700000714     C*
013800000714     C                   endsr
013900000714
