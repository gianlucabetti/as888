000100110617      *---------------------------------------------------------------*
000200130312      * Stampa DDT PDF - guida                                        *
000300110617      *---------------------------------------------------------------*
000400110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500130308     h dftactgrp(*no) actgrp(*caller)
000600110617
000700110617      *---------------------------------------------------------------*
000800130312     D pKLIST          s             15
000900130312     D pELENCO         s             30
001000130312     D pPRINTER        s             10
001100130312     D pFORMTYPE       s             10
001200130312     D pUSRDTA         s             10
001300130312     D pDRAWER         s             10
001400110617
001500130309
001600110627     C     *ENTRY        PLIST
001700130312     C                   PARM                    pKLIST           15
001800130312     C                   PARM                    pPRINTER         10
001900130312     C                   PARM                    pFORMTYPE        10
002000130312     C                   PARM                    pUSRDTA          10
002100130405     C*
002200130405     C* Pre elaborazione stampa => eliminazione caratteri jolly
002300130405     C                   call      'PDF00STAR1'
002400130405     C                   parm                    pKLIST
002500130312     C*
002600130312     C* Inizializzazioni
002700130312     C                   eval      pELENCO = 'PDFLIST   *LIBL     *FIRST'
002800130312     C                   eval      pDRAWER = '1'
002900130312     C*
003000130312     C* Effettuo stampa del intero "lotto" spedizioni aventi ID RAGGR. STAMPA = pKLIST ricevuto
003100130312     C                   call      'PDF00STAC'
003200130312     C                   parm                    pKLIST
003300130312     C                   parm                    pELENCO
003400130312     C                   parm                    pPRINTER
003500130312     C                   parm                    pFORMTYPE
003600130312     C                   parm                    pUSRDTA
003700130312     C                   parm                    pDRAWER
003800130312     C*
003900130312     C* Chiusura sempre il *LR
004000130312     C                   seton                                        LR
004100130312
