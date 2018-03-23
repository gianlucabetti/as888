000100110617      *---------------------------------------------------------------*
000200130405      * Stampa DDT PDF - pre elaborazione                             *
000300110617      *---------------------------------------------------------------*
000400110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500130308     h dftactgrp(*no) actgrp(*caller)
000600110617
000700110617      *---------------------------------------------------------------*
000800130405     fPDFLIST01Iuf   e           k disk
000900110617
001000130309
001100110627     C     *ENTRY        PLIST
001200130405     C                   PARM                    pKLIST           15
001300130309
001400110617      /free
001500130309
001600130309
001700130405       // scorro tutti i PDF del ID lancio ricevuto in input
001800130405       setll (pKLIST) PDFLIST01I;
001900130405       reade (pKLIST) PDFLIST01I;
002000130405
002100130405       dow not %eof(PDFLIST01I);
002200130405
002300130405           // Se presente elimino dal nome PDF i caratteri "jolly"
002400130405           dow %scan('*':PDFKEY:1) > *zeros;
002500130405               PDFKEY = %replace('':PDFKEY:%scan('*':PDFKEY:1):1);
002600130405           enddo;
002700130405           PDFKEY = %trim(PDFKEY);
002800130405
002900130405           update RPDF;
003000130405           reade (pKLIST) PDFLIST01I;
003100130405
003200130405       enddo;
003300130405
003400130308
003500130313       eval *inlr = *on;
003600130308
