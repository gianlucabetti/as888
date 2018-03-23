000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020116      * TNVR23R *----------------------------------------------------*
000300020312      * STAMPA RINUM. CODICE VISITE
000400020114      *--------------------------------------------------------------*
000500020115      * File con vecchi e nuovi numeri visita
000600020308     FTnWNr00f  IF A E           K DISK
000700020118      *
000800020123     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900921027     C*****************************************************************
001000020131      *
001100020131     C                   z-add     0             $WPou             3 0
001200020115      *
001300020131      * Stampo la testata a rottura di P.O e a fine pagina.
001400020128     C                   read      TnWNr00f                               21
001500020128     C     *in21         doweq     *off
001600020131      *
001700020131     C     $WPou         ifne      WNrPou
001800020131     C                   z-add     WNrPou        $WPou
001900020131     C                   setoff                                       77
002000020131     C                   else
002100020131     C                   seton                                        77
002200020131     C                   endif
002300020115      * stampo
002400020128     C   OF
002500020128     CORN77              except    TesVis
002600020128     C  N77              seton                                        77
002700020128     C
002800020128     C                   except    DetVis
002900020128     C                   read      TnWNr00f                               21
003000020128     C                   enddo
003100020115      *
003200020312     C                   SETON                                        LR
003300020312      *********************************************************************
003400020118     OQSYSPRT   E            TESVIS         2 02
003500020131     O                                         +  2 'RINUMERAZIONE'
003600020118     O                                         +  1 'VISITE/OFFERTE'
003700020305     O                                         +  2 'DEL P.O. .......:'
003800020131     O                       WNrPou            +  1
003900020118     O                                         +  5 'TNVR23R'
004000020129     O                       UDATE             +  2 '  /  /  '
004100020118     O                                         +  5 'PAG.'
004200020118     O                       PAGE1         Z
004300020118     O          E            TESVIS         2
004400020129     O                                         +  5 'PUNTO OPERATIVO'
004500020129     O                                         +  3 'NUMERO VECCHIO'
004600020118     O                                         +  3 'NUMERO NUOVO'
004700020123     O          E            DETVIS
004800020129     O                       WNrPou            + 12
004900020129     O                       WNrOld            +  9 '     /  '
005000020129     O                       WNrNew            +  9 '      / '
