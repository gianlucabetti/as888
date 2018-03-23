000100141127     H*
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400120406      * utility dato un codice autista e data conteggio restituisce il se trova un codice
000500120406      * tariffa che potrebbe essere utilizzato per valorizzare
000600120406      * con questa priorità di ricerca per tipo tariffa
000700120406      * ?__TIG TARIFFA INTERA GIORNATA
000800120406      * ?__TMG TARIFFA MEZZA GIORNATA
000900120406      * ?__MGA MINIMO GARANTITO
001000120406      * ?__BON BONUS
001100120406      * ?__ETI ETICHETTATURA
001200120406      * ?__PCK PICKING
001300020624      *
001400120406      *---------------------------------------------------------------
001500120406     d fgtds         e ds                  extname(fifgt00f)
001600120406
001700020624     D param           DS
001800120406     D  parauti                       7s 0
001900120406     D  pardati                       8s 0
002000120412     D  partsri                       1
002100120406     d  partaro                       3
002200120216     d  esito                         1
002300030109     d  error                         1
002400030109
002500020624     D*
002600011026
002700000000     C*---------------------------------------------------------------*
002800020624     c     *entry        plist
002900120412     c                   parm                    param
003000120412      *
003100120406     c                   clear                   partaro
003200120216     c                   clear                   esito
003300120216     c                   clear                   error
003400120418     c                   clear                   fgtds
003500030109
003600120406     C/EXEC SQL
003700120406     C+ Select * into :fgtds  FROM fifgt00F WHERE fgttsr =
003800120412     C+ :partsri and fgtpdr = :parauti  and fgtddt <=
003900120406     C+ :pardati and fgtdst >= :pardati and fgtatb = ' '
004000130918     C+  and fgtsml = ' '
004100120412     C+ fetch first row only
004200120406     C/END-EXEC
004300120406      *
004400120406     c                   if        sqlcod < 0
004500120406     c                   clear                   fgtds
004600120406     c                   move      '1'           error
004700120406     c                   endif
004800120406     c
004900120406      *
005000120406     c                   if        error <> '1'
005100120406     c                   clear                   tipctd            3
005200120406     c     altra         tag
005300120406     c                   select
005400120406     c                   when      tipctd = *blank
005500120406     c                   move      'TIG'         tipctd
005600120406     c                   when      tipctd = 'TIG'
005700120406     c                   move      'TMG'         tipctd
005800120406     c                   when      tipctd = 'TMG'
005900120406     c                   move      'MGA'         tipctd
006000120406     c                   when      tipctd = 'MGA'
006100120406     c                   move      'BON'         tipctd
006200120406     c                   when      tipctd = 'BON'
006300120406     c                   move      'ETI'         tipctd
006400120406     c                   when      tipctd = 'ETI'
006500120406     c                   move      'PCK'         tipctd
006600120406     c                   when      tipctd = 'PCK'
006700120406     c                   goto      fine
006800120406     c                   endsl
006900020313     C/EXEC SQL
007000120406     C+ SELECT '1' into :esito FROM fifpt00F WHERE fptpdr=
007100120406     C+ :fgtpdr and fptctr = :fgtctr and fptprg = :fgtprg
007200130918     C+ and fptctd = :tipctd and fptsml=' '
007300130918     C+  fetch first row only
007400120406     C/END-EXEC
007500120406      *
007600120406     c                   select
007700120406     c                   when      sqlcod < 0
007800030109     c                   move      '1'           error
007900120406     c                   when      sqlcode = 100
008000120406     c                   goto      altra
008100120406     c                   other
008200120406     c                   move      tipctd        partaro
008300120406     c                   goto      fine
008400120406     c                   endsl
008500120406     c                   endif
008600030109     c
008700020624      *
008800120406     c     fine          tag
008900030109     c                   seton                                        lr
009000011129     C*--------------------------------------------------------------------
