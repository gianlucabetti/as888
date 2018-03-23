000200171212     H DEBUG DECEDIT('0,') DATEDIT(*ymd)
000300000000     H*--------------------------------------------------------------*
000400171212     ffiapd40F  if   e           k disk
000401171212     faitra03l  if   e           k disk
000800120216      * utility data una partita IVA verifica se esiste nell'anagrafica certificazione traini di Pedini
000900020624      *
000901171212     D aitrads         DS
001000020624     D param           DS
001100171212     D  Icodaut                       7s 0
001101171212     D  Ocodaut                       7s 0
001102171212     D  Onumcel                      16
001200120216     d  esito                         1
001201171212      *
001405120502     C*Legenda errori possibili di ritorno
001406171212     C* esito <> '1' non trovato numero di cellulare
001410120502      * ?_______________________________________________________________________
001500020624     c     *entry        plist
001600030109     c                   parm                    param
001601171212     c                   move      *date         wdate
002300120306     c*
002900171212     c     icodaut       setll     aitra03l
002901171212     c                   do        *hival
002902171212     c     icodaut       reade     aitra03l
003100171212     c                   if        %eof(aitra03l)
003200171212     c                   leavesr
003300171212     c                   endif
003400171212     c                   if        tradfi = 0 or tradfi > Wdate
003500171212     c                   movel     tracfi        soctraini
003501171212     c                   leave
003600171212     c                   endif
003601171212     c                   enddo
003700120306     c*
004400020313     C/EXEC SQL
004500120306     C+ SELECT '1', aiats00f.* into :esito, :DSats
004600120306     C+    FROM aiats00F WHERE atspiva = :pariva  and
004700120502     C+         atsSOC = :soctraini
004800120502     C+         and atsdec > 0  fetch first row only
004900020313     C/END-EXEC
005000030109      *
005100030109     c                   if        sqlcod < 0
005200030109     c                   move      '1'           error
005201120502     c                   eval      errmsg = 'Non trovata anagrafica -
005202120502     c                             come trazionista '
005300030109     c                   endif
005400030109     c
005401120502      * se lo trovo fra i trazionisti e ho inserito le date verifico anche periodo di validità
005402120502     c                   if        error = *blank and esito = '1'
005403120502     c                             and pardtin > 0
005404120502     c                   select
005405120502     c                   when      pardtin < atsdec
005406120502     c                   eval      error = '2'
005407120502     c                   eval      errmsg = 'Data decorrenza allegato -
005408120502     c                             minore di decorrenza contratto'
005409120508     c                   when      pardtfi > 0 and atsdfc > 0 and
005410120508     c                             pardtfi > atsdfc
005411120502     c                   eval      error = '3'
005412120502     c                   eval      errmsg = 'Data scadenza allegato -
005413120502     c                             maggiore di scadenza contratto'
005414120502     c                   endsl
005415120502     c                   endif
005416120502     c
005500020624      *
005600030109     c                   seton                                        lr
005700011129     C*--------------------------------------------------------------------
