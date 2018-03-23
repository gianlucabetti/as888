000100051229     H DECEDIT('0,') DATEDIT(*YMD.)
000200011127     H*--------------------------------------------------------------*
000300060329     H*             Attribuzione serie parziale libera per p.o.      *
000400000000     H*--------------------------------------------------------------*
000500051229     D wrkGetLista     S           4096
000600051229     D                                     VARYING
000700051229     d tntbeds       e ds                  extname(tntbe00f)
000800051207     D kpjba         E DS
000900060329     D tntb54ds      E DS
001000060405     D d3cp          E DS
001100060331     D w_dal           S              8s 0
001200060331     D w_al            S              8s 0
001300060329     D                 DS
001400060329     Dnumdala                  1      7a
001500060329     Dnumdal                   1      7s 0
001600060329     D                 DS
001700060329     Dnumnrsa                  1      2a
001800060329     Dnumnrs                   1      2s 0
001900060329     D nrspre          S                   LIKE(numnrs)
002000051228     C**************************************************************************
002100051228     C* compongo istruzione sql
002200051228     C**************************************************************************
002300080123     c/exec sql
002400080123     c+ set option dynusrprf = *owner, closqlcsr = *endmod
002500080123     c/end-exec
002600080123
002700060331     c* pulizia campi di output
002800060331     c                   clear                   o54nrs
002900060331     c                   clear                   o54dal
003000060331     c                   clear                   o54al
003100060331     c                   clear                   o54err
003200060509     c                   clear                   o54prz
003300070205     c                   clear                   o54ksc
003400060331     c*
003500060331     c*
003600051228     **?Imposto la parte fissa dell'istruzione.
003700051228     C                   EVAL      wrkGetLista =
003800060329     C                             'SELECT * from tntbe01l where  -
003900060406     C                             tbecod = ''3CP''  -
004000060329     C                             and substr(tbeke1, 1, 3) = '
004100051229     C                             +
004200060329     C                             '''' + i54fil  + ''''
004300060516    1c                   if        i54nrs > *zeros
004400060329     C                   EVAL      wrkGetLista = wrkGetLista +
004500060329     c                             ' and substr(tbeke2, 1, 2) = '
004600060329     C                             +
004700060329     C                             '''' + i54nrs  + ''''
004800060516    1c                   endif
004900051228     **?Ordinamento.
005000051228     C                   EVAL      wrkGetLista = wrkGetLista +
005100060329     C                             ' ORDER BY tbeke2'
005200051228     **?Solo lettura.
005300051228     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
005400051228     C
005500051228     C/EXEC SQL
005600051228     C+ PREPARE s1 FROM :wrkGetLista
005700051228     C/END-EXEC
005800051228     C
005900051228     C/EXEC SQL
006000051228     C+ DECLARE A1 CURSOR FOR S1
006100051228     C/END-EXEC
006200051228     C
006300051228     C/EXEC SQL
006400051228     C+ OPEN a1
006500051228     C/END-EXEC
006600051228
006700060516    1C                   DO        *hival
006800051228     C/EXEC SQL
006900060329     C+ FETCH NEXT FROM a1 INTO :tntbeDs
007000051228     C/END-EXEC
007100060516    2C                   SELECT
007200051228     c*
007300051228     C                   WHEN      SqlCod = 100
007400051228     c                   leave
007500051228     C                   WHEN      SqlCod < 0
007600080123     C****               seton                                        H1
007700080123     c                   eval      o54err='S'
007800051228     c                   exsr      srfine
007900051228     c                   other
008000060913     C*
008100060913    3c                   if        i54tel='C' and tbeatb='A'
008200060913     c                   iter
008300060913     c                   endif
008400060331     c*
008500060516    3c                   if        i54tel='C' or i54nrs > '00'
008600060509     c                   eval      o54prz='P'
008700060516    3c                   endif
008800060912     c*
008900060912     c                   if        i54tel='C' and i54ksc=tbeke1
009000060912     c                   iter
009100060912     c                   endif
009200060509     c*
009300060329     c                   eval      numdala = %subst(tbeke2: 3: 7)
009400060329     c                   eval      numnrsa = %subst(tbeke2: 1: 2)
009500060329     c* rottura numero serie
009600060516    3c                   if        numnrs <> nrspre
009700060516    4c                   if        nrspre > *zeros
009800060517    1c                   if        i54tel = 'R' and
009900060517     c                             w_dal > *zeros and w_al = *zeros
010000060517     C                             and W_DAL < 9999999
010100060517     c                   eval      w_al  = (w_dal + (i54tot-1))
010200060517    2c                   if        w_al <= 9999999
010300060517     c                   z-add     w_Al          o54al
010400060517     c                   eval      o54nrs  = nrspre
010500060517     c                   eval      o54dal  = w_dal
010600060517     c                   leave
010700060517    2c                   endif
010800060517    1c                   endif
010900060329     c                   clear                   w_dal
011000060329     c                   clear                   w_al
011100060516    4c                   endif
011200060329     c                   eval      nrspre = numnrs
011300060516    3c                   endif
011400060329     c*
011500060405     c                   movel     tbeuni        d3cp
011600060516     c* OPERAZIONI PER TIPO ELABORAZIONE "RICERCA"
011700060516    3c                   if        i54tel = 'R'
011800060516    4c                   select
011900060329     c* cerco il dal
012000060329     c                   when      w_dal = *zeros and
012100060331     c                             numdal > 1 and i54tot < numdal
012200060329     c                   eval      o54dal = 1
012300060329     c                   eval      o54al  = i54tot
012400060329     c                   eval      o54nrs  = numnrs
012500060329     c                   leave
012600060405     c                   when      w_dal = *zeros and §3cpal < 9999999
012700060405     c                   eval      w_dal = (§3cpal + 1)
012800060329     c* cerco l'al
012900060329     c                   when      w_al = *zeros
013000060329     c                   eval      w_al = (numdal-1)
013100060516    5c                   if        (W_Al - w_dal) >= i54tot
013200060329     c                   eval      o54dal = w_dal
013300060329     c                   eval      o54al  = (W_dal + (i54tot-1))
013400060329     c                   eval      o54nrs  = numnrs
013500060329     c                   leave
013600060516   x5c                   else
013700060329     c* range insufficiente assumo come nuovo numero dal il numero al + 1
013800060329     c* e vado avanti nella lettura
013900060405     c                   eval      w_dal = (§3cpal + 1)
014000060331     c                   clear                   w_al
014100060516    5c                   endif
014200060516    4c                   endsl
014300060516   x3c                   else
014400060516     c* OPERAZIONI PER TIPO ELABORAZIONE "CONTROLLO"
014500060516    4c                   select
014600060516     c                   when      i54dal < numdal and i54al < numdal
014700060516     c                   leave
014800060516     c                   when      i54dal > §3cpal and §3cpal < 9999999
014900060516     c                   iter
015000060516     c                   other
015100060516     c                   eval      o54err = '1'
015200070205     c                   movel     tbeke1        o54ksc
015300070206     c                   z-add     numdal        o54dal
015400070206     c                   z-add     §3cpal        o54al
015500070206     c                   z-add     numnrs        o54nrs
015600060516     c                   leave
015700060516    4c                   endsl
015800060516     c
015900060516    3c                   endif
016000060329     c*
016100060516    2C                   ENDSL
016200051228      *
016300060516    1C                   ENDDO
016400051228     C/EXEC SQL
016500051228     C+ CLOSE a1
016600051228     C/END-EXEC
016700060516    1c                   if        i54tel = 'R' and o54dal = 0  and
016800060331     c                             w_dal > *zeros and w_al = *zeros
016900060331     C                             and W_DAL < 9999999
017000060331     c                   eval      w_al  = (w_dal + (i54tot-1))
017100060516    2c                   if        w_al <= 9999999
017200060331     c                   z-add     w_Al          o54al
017300060331     c                   eval      o54nrs  = numnrs
017400060331     c                   eval      o54dal  = w_dal
017500060516    2c                   endif
017600060516    1c                   endif
017700060516     c* Se richiesta ricerca per una particolare serie e il pgm non
017800060516     c* ha trovato nessun range assegnabile (tutto già impegnato)
017900060516     c* restituisco errore al chiamante
018000060516    1c                   if        i54tel = 'R' and o54dal = 0 and
018100060516     c                             o54prz = 'P'
018200060516     c                   eval      o54err = '1'
018300060516    1c                   endif
018400060418     c
018500060331     c*
018600060331     c     fine          tag
018700060331     c                   movel     tntb54ds      kpjbu
018800051229     c                   seton                                        lr
018900011127     C******************************************************
019000011127     C     *INZSR        BEGSR
019100011127     C******************************************************
019200011127     C*
019300051229     c     *entry        plist
019400051229     c                   parm                    kpjba
019500060329     c                   movel     kpjbu         tntb54ds
019600011127     C                   ENDSR
019700051229     C**************************************************************************
019800051229     C* esco con codice d'errore
019900051229     C**************************************************************************
020000051229     C     srfine        BEGSR
020100060331     c                   movel     tntb54ds      kpjbu
020200051229     c                   seton                                        lr
020300051229     c                   return
020400051229     c*
020500051229     c                   endsr
