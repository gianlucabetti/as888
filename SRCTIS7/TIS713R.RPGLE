000100170406     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200990907
000300170406     Ftntbe01l  if   e           k disk
000400170406
000500990907
000600170412     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
000700170406     D  DVPC         e ds
000800010525
000900010525
001000010525     C*-----------------------------------------------------------------------------
001100170329     C*
001200170406     C* Inizializzo parametri di ouput
001300170406     C                   clear                   OS713TBEUNI
001400170406     C                   movel     '2'           OS713ESITO
001500170406     C*
001600170406     C* Monitoro l'intero flusso del *pgm
001700170406     C                   monitor
001800170406     C*
001900170406     C* Reperisco tabella per filtro richiesto in input
002000170407     C     KeyTBE01_C    chain     tntbe01l
002100170406     C                   if        %found(tntbe01l)
002200170406     C*
002300170406     C* Se trovata => esito OK
002400170406     C                   eval       OS713TBEUNI = tbeUNI
002500170406     C                   eval       OS713ESITO  = '1'
002600170406     C*
002700170406     C* Se NON trovata => esito KO
002800170406     C                   else
002900170406     C                   eval       OS713ESITO  = '0'
003000170406     C                   endif
003100170406     C*
003200170406     C* Gestisco eventuale errore
003300170406     C                   on-error
003400170406     C                   dump(A)
003500170420     C                   seton                                        lr
003600170406     C*
003700170406     C* Interrompo monitoraggio errori
003800170406     C                   endmon
003900170406     C*
004000170406     C                   return
004100000714
004200050920
004300050920
004400000714      /TITLE Routine di *init PGM
004500000714     C*
004600000714     C     *inzsr        begsr
004700060103     C*
004800060103     C     *entry        plist
004900170406     C                   parm                    IS713TBECOD
005000170406     C                   parm                    IS713TBEKE1
005100170406     C                   parm                    IS713TBEKE2
005200170407     C                   parm                    IS713TBELIN
005300170407     C                   parm                    IS713TBESIF
005400170406     C                   parm                    OS713TBEUNI
005500170406     C                   parm                    OS713ESITO
005600170406     C*
005700170406     C* Definizione chiavi
005800170407     C     KeyTBE01_C    klist
005900170406     C                   kfld                    IS713TBECOD
006000170406     C                   kfld                    IS713TBEKE1
006100170406     C                   kfld                    IS713TBEKE2
006200170407     C                   kfld                    IS713TBELIN
006300170407     C                   kfld                    IS713TBESIF
006400000714     C*
006500000714     C                   endsr
006600000714
