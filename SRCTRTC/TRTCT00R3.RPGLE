000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200110520      **?__________________________________________________________________ */
000300110520      *
000400110520      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
000500110520      *   dichiarati nei segmenti    (+:'?)
000600110520      *   [più, due punti, apostrofo, punto interrogativo]
000700110520      *  Sono i caratteri che servono anche alla struttura EDI e quindi
000800110520      *   considerati caratteri Funzionali che, qualora debbano essere
000900110520      *   utilizzati come semplici caratteri di testo, dovranno essere
001000110520      *   preceduti da un (?).
001100110520      *
001200110520      **?__________________________________________________________________ */
001300110520      * Riceve:
001400110520      *  la stringa alfanumeria          in  campo_test
001500110520      *  e restituisce                   in  campo_ritorno
001600110520      **?__________________________________________________________________ */
001700110621     d  esito          s              1
001800110520     D   Char          s              1    DIM(4)
001900110520     D   Lettere       s              1    DIM(512)
002000110520     D   Byte_Prima    s              1
002100110520      **?__________________________________________________________________ */
002200110520      **
002300110520     D Char_Jolly      C                   CONST('?')
002400110520      **
002500110520     D Punto_interrogativo...
002600110520     D                 C                   CONST('?')
002700110520     D Apostrofo       C                   CONST('''')
002800110520     D Piu             C                   CONST('+')
002900110520     D Due_Punti       C                   CONST(':')
003000110520     D Chr_Speciali...
003100110520     D                 C                   CONST('?''+:')
003200110520      **?__________________________________________________________________ */
003300110520      **
003400050629     D Digits          C                   CONST('0123456789')
003500110520      **?__________________________________________________________________ */
003600110520     C*
003700050628     c     *entry        plist
003800110520     C*
003900110520      * ?     Input
004000110520     c                   parm                    campo_test      512
004100110520      * ?     Output
004200110520     c                   parm                    campo_ritorno   512
004300110621      *
004400110621     c                   parm                    esito
004500110520      *
004600110520     C*
004700110520      *  carica la schiera dei carratteri speciali da cercare nel campo
004800110520     c                   movea     Chr_speciali  char
004900110520     c                   movea     campo_test    Lettere
005000110520      *
005100110520      **?__________________________________________________________________ */
005200110520      *  Prova con i caratteri speciali
005300110520      **?__________________________________________________________________ */
005400110520      *  (4) CARATTERI PARTICOLARI
005500110520     c                   do        4             speciale          3 0
005600110520      *
005700110520      * a partire dal primo BYTE
005800110520     c                   z-add     1             dove              3 0
005900110520      *
006000110520     c     cerca_ancora  tag
006100110520      *
006200110520      *            *-  cerca il carattere  -*
006300110520      *  *--------------------------------------------*
006400110520     c     Char(speciale)lookup    Lettere(dove)                          01
006500110520      *  *--------------------------------------------*
006600110520      *
006700110520      *  se Trovato
006800110520     c                   if        *in01
006900110520      *
007000110520     c                   if         Dove = 1
007100110520      *  sposta tutti i caratteri
007200110520     c                   exsr      scala_Caratt
007300110520      *
007400110520     c                   elseIf    Dove > 1
007500110520     c                   eval      Byte_Prima = Lettere(Dove-1)
007600110520      *
007700110520      *  è presente il (?) davanti al carattere Speciale per trasformarlo in
007800110520      *    carattere semplice ?
007900110520     c                   if        Byte_Prima <> Punto_Interrogativo
008000110520      *  sposta tutti i caratteri
008100110520     c                   exsr      scala_Caratt
008200110520     c                   end
008300110520      *
008400110520     c                   end
008500110520      *  se Trovato:
008600110520      *   si sposta nella posizione successiva per provare da lì in poi
008700110520      *     se è presente nuovamente il carattere cercato
008800110520     c                   eval      dove = dove + 1
008900110520     c                   if        dove < ULTIMO_Byte
009000110520     c                   goto      cerca_ancora
009100110520     c                   end
009200110520     c                   endIf
009300110520      *
009400110520     c                   enddo
009500110520      *
009600110520     c                   movea     Lettere       campo_ritorno
009700110520     c                   seton                                        LR
009800050628      * ?------------------------------------------------------------------ */
009900110520      *   sposta lettera per lettera tutte quelle presenti nel campo
010000110520      * ?------------------------------------------------------------------ */
010100110520     c     scala_Caratt  begsr
010200110520      *
010300110520     c                   z-add     dove          FinoA             3 0
010400110520     c                   clear                   ULTIMO_Byte       3 0
010500110523     c                   eval      ULTIMO_Byte = %elem(Lettere)
010600110520      *
010700110523     c                   do        ULTIMO_Byte   nr                3 0
010800110523     c                   clear                   pos               3 0
010900110523     c                   eval      pos = %elem(Lettere) - nr
011000110520      *
011100110520      *  sposta nella successiva la lettera
011200110523     c                   eval      Lettere(pos+1) = Lettere(pos)
011300110523      *
011400110523     c                   if        POS = FinoA
011500110523     c                   leave
011600110523     c                   end
011700110520      *
011800110523     c                   enddo
011900110520      *
012000110520      *  A questo punto imposta il punto interrogativo davanti al carattere
012100110520      *   speciale
012200110523     c                   eval      Lettere(pos) = Punto_Interrogativo
012300110520      *
012400110520      *  e reimposta il nuovo punto di riferimento dove adesso si trova il
012500110520      *   carattere speciale da cui partire per una nuova ricerca.
012600110520     c                   eval      dove = dove + 1
012700110520      *
012800110520     c                   endsr
012900110520      * ?------------------------------------------------------------------ */
013000110621      * ?------------------------------------------------------------------ */
013100110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
013200110621      * ?------------------------------------------------------------------ */
013300110621     C     *pssr         BEGSR
013400110621     C
013500110621     C                   eval      esito = 'E'
013600110621     C                   ENDSR     '*CANCL'
013700110621     C
013800110621      *  ------------------------------------------------------------------ */
