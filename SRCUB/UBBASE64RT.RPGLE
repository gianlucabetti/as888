000100000000     H Option( *SrcStmt )
000200090610     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('UBTEST')
000300000000     **-- Parameters:  -------------------------------------------------------**
000400090610
000500161019     D/COPY GAITRASRC/SRCPROTOPI,UBBASE64R
000600161019     D/COPY GAITRASRC/SRCPROTOPR,UBBASE64R
000700090521
000800170314     D pInTesto        S             30A
000900170314     D pOutBase64      S             30A
001000170314     D pOutErrore      S              1S 0
001100170314
001200090520
001300090520     C*
001400161026     C     *entry        plist
001500170314     C                   parm                    pInTesto
001600170314     C                   parm                    pOutBase64
001700170314     C                   parm                    pOutErrore
001800090610
001900090610      /FREE
002000170314
002100161024
002200170314        // Inizializzo parametri di output
002300170314        clear pOutBase64;
002400170314
002500161026
002600170314        // Inizializzo l'esito chiamata ad errore
002700170314        pOutErrore = -1;
002800170314
002900170314
003000170314        // Encoda il testo di input in Base64
003100170314        iBASE64src_dta = %trim(pIntesto);
003200170314        iBASE64src_len = %len(%trim(pIntesto));
003300170314        if UBBASE64R_Encode_Binary(oBASE64enc_dta
003400170314                                  :oBASE64enc_len
003500170314                                  :iBASE64src_dta
003600170314                                  :iBASE64src_len) = *zeros ;
003700170314           pOutBase64 = %subst(oBASE64enc_dta:1:oBASE64enc_len);
003800170314           pOutErrore = *zeros;
003900170314        endif;
004000170314
004100170314
004200170314        // Visualizzo il testo encodato in Base64
004300170314     C     pOutBase64    dsply
004400170314
004500170314
004600170314        // Uscita
004700170314        *inlr = *on;
004800090710
004900090610
005000090610      /END-FREE
005100161025
