000100070315      * --------------------------------------­¬
000200070315     C*? Aggiorna FIARG nei campi:             |
000300070315      *                                        |
000400070315      * ? ARGFGS , ARGNDC , ARGDDC             |
000500070412      *  elabora tutte le spedizione di ARG    |
000501070412      *  del p.o. che parte e se consegnate    |
000502070412      *  su ARB imposto i campi di ARG :       |
000900070315      *                                        |
001000070412      *?Imposta tutti 999 nel P.O. (FGS)      ?|
001100070315      *?Imposta tutti 999 nella Distinta      ?|
001200070412      *?Imposta 19401231  nella Data Distinta ?|
001300070315      *                                        |
001400070329     c     *entry        plist
001500070329     c                   parm                    fgs               3
001600070329      *
001700070307     C/EXEC SQL
001800070329     C+ UPDATE FIARG00F SET ARGFGS = 999, ARGNDC = 9999999,
001900070412     C+ ARGDDC = 19401231 WHERE argpoc = :fgs and
002000070315     C+ digits(argaas)concat(digits(arglnp))concat(
002100070329     C+ digits(argnrs))concat(digits(argnsp)) in (SELECT
002200070329     C+ digits(arbaas)concat(digits(arblnp))concat(
002300070329     C+ digits(arbnrs))concat(digits(arbnsp)) FROM fnarb00f WHERE arbndc
002400070412     C+ > 0 and arbdcm > 0 and arbicc<>'R' and arbica<>'R')
002500070307     C/END-EXEC
002600070307
002601070412     c* puliamo i riferimenti della distinta su ARG del p.o. che parte
002602070412     c* se spedizione non consegnata o ritorno all'incasso
002700070326
002701070412     C/EXEC SQL
002702070412     C+ UPDATE FIARG00F SET ARGFGS = 0, ARGNDC = 0, ARGDDC = 0,
002703070412     C+ ARGPDC = 0, ARGSTP = 0, ARGNFTL = 0, ARGSLB = 0, ARGDTVDIS = 0,
002704070412     C+ ARGHVDIS = 0 WHERE argpoc = :fgs and
002705070412     C+ digits(argaas)concat(digits(arglnp))concat(
002706070412     C+ digits(argnrs))concat(digits(argnsp)) in (SELECT
002707070412     C+ digits(arbaas)concat(digits(arblnp))concat(
002708070412     C+ digits(arbnrs))concat(digits(arbnsp)) FROM fnarb00f WHERE arbdcm
002709070412     C+ = 0 or arbicc='R' or arbica='R')
002710070412     C/END-EXEC
002800070326      **
002900070315     c                   seton                                        LR
