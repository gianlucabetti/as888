000100020419      *--------------------------------------------------------------*
000200020419      * Compilare con la VECCHIA DS ridenominata in DDCT01OLD        *
000300020419      *            &  la NUOVA   DS già compilata                    *
000400020419      *--------------------------------------------------------------*
000500020419     h DECEDIT('0,') DATEDIT(*DMY.)
000600020419     fFNDCT00F  UP   e           k disk
000700020419     d DDCT01new     e ds                  extname(DDCT01) inz
000800020419     d DDCT01old     e ds                  prefix(§DCX:4)  inz
000900020419     c                   movel     DCTflo        DDCT01old
001000020419     c                   exsr      fromOLDtoNEW
001100020419     c                   movel(p)  DDCT01new     DCTflo
001200020419     c                   update    FNDCT000
001300020419     c     fromOLDtoNEW  begsr
001400020419      * E' stato eliminato il campo §DCTsdpd (11° byte).
001500020419      * Al suo posto è stato spostato il campo §DCTspgd (ex 13° byte);
001600020419      * il 12° byte rimane al suo posto;
001700020419      * dal 13° i byte saranno liberi.
001800020419     c                   movel     DCTflo        DDCT01new
001900020419     c                   eval      §DCTspgd = §DCXspgd
002000020419      * Sono inoltre stati variati i campi §dctTISP e §DCTEURO
002100020419     c                   select
002200020419      * - I/E EEX o EUP (rimangono così)
002300020419     c                   when           §DCXtisp <> *blanks
002400020419     c                             and  §DCXeuro <> *blanks
002500020419      * - I/E DPD
002600020419     c                   when           §DCXtisp =  *blanks
002700020419     c                             and  §DCXsdpd <> *blanks
002800020419     c                   eval      §DCTtisp = §DCXsdpd
002900020419     c                   eval      §DCTeuro = 'D'
003000020419      * - Nessun network estero (per ora)
003100020419     c                   other
003200020419     c                   clear                   §DCTeuro
003300020419     c                   endsl
003400020419     c                   endsr
