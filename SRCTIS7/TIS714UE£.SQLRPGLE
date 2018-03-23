000100170412     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200171228
000300171228       // al momento gestisce solo come cliente 0898888
000400171228       // CHE ANDRA' CAMBIATO CON GLI STANDARD FREE
000500171228       // al momento gestisce solo come chiamante la routine UPLTO_DL_EB per 0898888
000600171228
000700170412
000800170412
000900170412      //********************************************************************************************
001000170412      //
001100170412      // Definizione prototipi procedure.
001200170412      //
001300170412      //********************************************************************************************
001400171228
001500171228       /copy gaitrasrc/srcProtoPR,QUSRSPLA
001600171228     d TRUL28R1        pr                  extpgm('TRUL28R1')
001700171228     D  pTRUL28DS                          like(TRUL28DS)
001800170412
001900170412      //********************************************************************************************
002000170412      //
002100170412      // Definizione interfacce procedure.
002200170412      //
002300170412      //********************************************************************************************
002400990907
002500010524
002600170419     D*
002700170824     D  EDIVABDS     e ds                  inz extname(edivabwr)
002800171228     D*
002900171228       // reperimento check digits
003000171228     D TRUL28DS      e ds                  inz
003100170822     D*
003200171228     D wCCM            s                   like(vabCCM)
003300171228     D wCaller         s             15
003400171228     D wBufferIO       s           2048
003500171228     D wLenBuff        s              4s 0
003600171228       // Campi di comodo
003700171228     d wBarcode        s             18a
003800170901     D*
003900170901     D psds           sds
004000170901     D  procname         *PROC
004100010525
004200010525
004300171228       // MAIN
004400171228
004500171228       // elenco clienti con buffer da personalizzare
004600171228       select;
004700171228        when wCCM = 0898888;
004800171228         exsr CCM_0898888;
004900171228       endsl;
005000171228
005100171228       return;
005200171228
005300171228       begsr CCM_0898888;
005400171228
005500171228       // elenco chiamate personalizzate per il cliente
005600171228         select;
005700171228          when wCaller = 'UPLTO_DL_EB';
005800171228           exsr UPLTO_DL_EB_CCM_0898888;
005900171228         endsl;
006000171228
006100171228       endsr;
006200171228
006300171228       begsr UPLTO_DL_EB_CCM_0898888;
006400171228
006500171228         // aggiungere il barcode alla fine del buffer
006600171228         EDIVABDS = wBufferIO;
006700171228
006800171228         // calcolo barcode 18 char (l'ultimo è il check digit)
006900171228         // compongo barcode e calcolo chek digit
007000171228         wBarcode = %editc(VABLNP:'X') +
007100171228                    %editc(VABLNA:'X') +
007200171228                    %editc(VABNRS:'X') +
007300171228                    %editc(VABNCD:'X') +
007400171228                    %editc(VABZNC:'X');
007500171228         // calcolo il check digit
007600171228         clear TRUL28DS;
007700171228         I28Mod  = 'BAR';
007800171228         I28COD = %trim(wBarcode);
007900171228         TRUL28R1(TRUL28DS);
008000171228         // Se tutto ok aggiungo il check digit
008100171228         if O28err = *blanks;
008200171228           wBarcode = O28Cod;
008300171228           wBufferIO = %subst(wBufferIO:1:wLenBuff) + O28Cod;
008400171228         endif;
008500171228
008600171228       endsr;
008700050920
008800000714      /TITLE Routine di *init PGM
008900000714     C*
009000000714     C     *inzsr        begsr
009100060103     C*
009200060103     C     *entry        plist
009300171228     C                   parm                    wCCM
009400171228     C                   parm                    wCaller
009500171228     C                   parm                    wBufferIO
009600171228     C                   parm                    wLenBuff
009700000714     C*
009800000714     C                   endsr
009900000714
