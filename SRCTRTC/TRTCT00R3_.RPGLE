000100110520      **?__________________________________________________________________ */
000200110520      *
000300110520      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
000400110520      *   dichiarati nei segmenti    (+:'?)
000500110520      *   [più, due punti, apostrofo, punto interrogativo]
000600110520      *  Sono i caratteri che servono anche alla struttura EDI e quindi
000700110520      *   considerati caratteri Funzionali che, qualora debbano essere
000800110520      *   utilizzati come semplici caratteri di testo, dovranno essere
000900110520      *   preceduti da un (?).
001000110520      *
001100110520      **?__________________________________________________________________ */
001200110520      * Riceve:
001300110520      *  la stringa alfanumeria          in  campo_test
001400110520      *  e restituisce                   in  campo_ritorno
001500110520      **?__________________________________________________________________ */
001600110621     d  esito          s              1
001700110520     D   Char          s              1    DIM(4)
001800110520     D   Lettere       s              1    DIM(512)
001900110520     D   Byte_Prima    s              1
002000110520      **?__________________________________________________________________ */
002100110520      **
002200110520     D Char_Jolly      C                   CONST('?')
002300110520      **
002400110520     D Punto_interrogativo...
002500110520     D                 C                   CONST('?')
002600110520     D Apostrofo       C                   CONST('''')
002700110520     D Piu             C                   CONST('+')
002800110520     D Due_Punti       C                   CONST(':')
002900110520     D Chr_Speciali...
003000110520     D                 C                   CONST('?''+:')
003100110520      **?__________________________________________________________________ */
003200110520      **
003300050629     D Digits          C                   CONST('0123456789')
003400110520      **?__________________________________________________________________ */
003500110520     C*
003600050628     c     *entry        plist
003700110520     C*
003800110520      * ?     Input
003900110520     c                   parm                    campo_test      512
004000110520      * ?     Output
004100110520     c                   parm                    campo_ritorno   512
004200110621      *
004300110621     c                   parm                    esito
004400110520      *
004500110520     C*
004600110520      *  carica la schiera dei carratteri speciali da cercare nel campo
004700110520     c                   movea     Chr_speciali  char
004800110520     c                   movea     campo_test    Lettere
004900110520      *
005000110520      **?__________________________________________________________________ */
005100110520      *  Prova con i caratteri speciali
005200110520      **?__________________________________________________________________ */
005300110520      *  (4) CARATTERI PARTICOLARI
005400110520     c                   do        4             speciale          3 0
005500110520      *
005600110520      * a partire dal primo BYTE
005700110520     c                   z-add     1             dove              3 0
005800110520      *
005900110520     c     cerca_ancora  tag
006000110520      *
006100110520      *            *-  cerca il carattere  -*
006200110520      *  *--------------------------------------------*
006300110520     c     Char(speciale)lookup    Lettere(dove)                          01
006400110520      *  *--------------------------------------------*
006500110520      *
006600110520      *  se Trovato
006700110520     c                   if        *in01
006800110520      *
006900110520     c                   if         Dove = 1
007000110520      *  sposta tutti i caratteri
007100110520     c                   exsr      scala_Caratt
007200110520      *
007300110520     c                   elseIf    Dove > 1
007400110520     c                   eval      Byte_Prima = Lettere(Dove-1)
007500110520      *
007600110520      *  è presente il (?) davanti al carattere Speciale per trasformarlo in
007700110520      *    carattere semplice ?
007800110520     c                   if        Byte_Prima <> Punto_Interrogativo
007900110520      *  sposta tutti i caratteri
008000110520     c                   exsr      scala_Caratt
008100110520     c                   end
008200110520      *
008300110520     c                   end
008400110520      *  se Trovato:
008500110520      *   si sposta nella posizione successiva per provare da lì in poi
008600110520      *     se è presente nuovamente il carattere cercato
008700110520     c                   eval      dove = dove + 1
008800110520     c                   if        dove < ULTIMO_Byte
008900110520     c                   goto      cerca_ancora
009000110520     c                   end
009100110520     c                   endIf
009200110520      *
009300110520     c                   enddo
009400110520      *
009500110520     c                   movea     Lettere       campo_ritorno
009600110520     c                   seton                                        LR
009700050628      * ?------------------------------------------------------------------ */
009800110520      *   sposta lettera per lettera tutte quelle presenti nel campo
009900110520      * ?------------------------------------------------------------------ */
010000110520     c     scala_Caratt  begsr
010100110520      *
010200110520     c                   z-add     dove          FinoA             3 0
010300110520     c                   clear                   ULTIMO_Byte       3 0
010400110523     c                   eval      ULTIMO_Byte = %elem(Lettere)
010500110520      *
010600110523     c                   do        ULTIMO_Byte   nr                3 0
010700110523     c                   clear                   pos               3 0
010800110523     c                   eval      pos = %elem(Lettere) - nr
010900110520      *
011000110520      *  sposta nella successiva la lettera
011100110523     c                   eval      Lettere(pos+1) = Lettere(pos)
011200110523      *
011300110523     c                   if        POS = FinoA
011400110523     c                   leave
011500110523     c                   end
011600110520      *
011700110523     c                   enddo
011800110520      *
011900110520      *  A questo punto imposta il punto interrogativo davanti al carattere
012000110520      *   speciale
012100110523     c                   eval      Lettere(pos) = Punto_Interrogativo
012200110520      *
012300110520      *  e reimposta il nuovo punto di riferimento dove adesso si trova il
012400110520      *   carattere speciale da cui partire per una nuova ricerca.
012500110520     c                   eval      dove = dove + 1
012600110520      *
012700110520     c                   endsr
012800110520      * ?------------------------------------------------------------------ */
012900110621      * ?------------------------------------------------------------------ */
013000110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
013100110621      * ?------------------------------------------------------------------ */
013200110621     C     *pssr         BEGSR
013300110621     C
013400110621     C                   eval      esito = 'E'
013500110621     C                   ENDSR     '*CANCL'
013600110621     C
013700110621      *  ------------------------------------------------------------------ */
