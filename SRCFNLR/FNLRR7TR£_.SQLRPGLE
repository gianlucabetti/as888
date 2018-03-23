000100120406     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(*caller)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000301120406      * utility dato un codice autista e data conteggio restituisce il se trova un codice
000302120406      * tariffa che potrebbe essere utilizzato per valorizzare
000303120406      * con questa priorità di ricerca per tipo tariffa
000304120406      * ?__TIG TARIFFA INTERA GIORNATA
000305120406      * ?__TMG TARIFFA MEZZA GIORNATA
000306120406      * ?__MGA MINIMO GARANTITO
000307120406      * ?__BON BONUS
000308120406      * ?__ETI ETICHETTATURA
000309120406      * ?__PCK PICKING
000400020624      *
000401120406      *---------------------------------------------------------------
000402120406     d fgtds         e ds                  extname(fifgt00f)
000404120406
000500020624     D param           DS
000600120406     D  parauti                       7s 0
000601120406     D  pardati                       8s 0
000602120412     D  partsri                       1
000603120406     d  partaro                       3
000604120216     d  esito                         1
001000030109     d  error                         1
001300030109
001600020624     D*
001700011026
001800000000     C*---------------------------------------------------------------*
001900020624     c     *entry        plist
002000120412     c                   parm                    param
002001120412      *
002006120406     c                   clear                   partaro
002007120216     c                   clear                   esito
002008120216     c                   clear                   error
002009120418     c                   clear                   fgtds
002400030109
002401120406     C/EXEC SQL
002402120406     C+ Select * into :fgtds  FROM fifgt00F WHERE fgttsr =
002403120412     C+ :partsri and fgtpdr = :parauti  and fgtddt <=
002404120406     C+ :pardati and fgtdst >= :pardati and fgtatb = ' '
002405120412     C+ fetch first row only
002406120406     C/END-EXEC
002407120406      *
002414120406     c                   if        sqlcod < 0
002415120406     c                   clear                   fgtds
002416120406     c                   move      '1'           error
002419120406     c                   endif
002423120406     c
002424120406      *
002501120406     c                   if        error <> '1'
002502120406     c                   clear                   tipctd            3
002503120406     c     altra         tag
002504120406     c                   select
002505120406     c                   when      tipctd = *blank
002506120406     c                   move      'TIG'         tipctd
002507120406     c                   when      tipctd = 'TIG'
002508120406     c                   move      'TMG'         tipctd
002509120406     c                   when      tipctd = 'TMG'
002510120406     c                   move      'MGA'         tipctd
002511120406     c                   when      tipctd = 'MGA'
002512120406     c                   move      'BON'         tipctd
002513120406     c                   when      tipctd = 'BON'
002514120406     c                   move      'ETI'         tipctd
002515120406     c                   when      tipctd = 'ETI'
002516120406     c                   move      'PCK'         tipctd
002517120406     c                   when      tipctd = 'PCK'
002518120406     c                   goto      fine
002519120406     c                   endsl
002600020313     C/EXEC SQL
002700120406     C+ SELECT '1' into :esito FROM fifpt00F WHERE fptpdr=
002701120406     C+ :fgtpdr and fptctr = :fgtctr and fptprg = :fgtprg
002800120406     C+ and fptctd = :tipctd fetch first row only
003000120406     C/END-EXEC
003100120406      *
003101120406     c                   select
003900120406     c                   when      sqlcod < 0
004100030109     c                   move      '1'           error
004401120406     c                   when      sqlcode = 100
004402120406     c                   goto      altra
004405120406     c                   other
004406120406     c                   move      tipctd        partaro
004407120406     c                   goto      fine
004408120406     c                   endsl
004409120406     c                   endif
004800030109     c
004900020624      *
004901120406     c     fine          tag
005300030109     c                   seton                                        lr
005400011129     C*--------------------------------------------------------------------
