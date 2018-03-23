000100090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300090109     Ffifgt03l  uf   e           k disk    commit
000400090109     Ffitgt00f  uf   e             disk    commit
000500090330     Ffiapd01l  if   e           k disk
000600090330     Ffitgt01l  if   e           k disk    rename(fitgt000:fitgt001)
000700090109     F                                     INFDS(TAR)
000800090408     dficn05ds       e ds
000900090408     dficn08ds       e ds
001000090408     ddgpi           e ds
001100090109     D TAR             DS
001200090109     D  tgtNRR               397    400B 0
001300090324     d ficn02ds      e ds
001400090402     d dapdflr       e ds
001500020305     d dataiso         s               d   datfmt(*iso)
001600020305     d dataeur         s               d   datfmt(*eur)
001700000000     C*---------------------------------------------------------------*
001800020304      *
001900020403     c                   exsr      elabora
002000020308     c                   seton                                        lr
002100020308      *--------------------------------------------------
002200020308     c     elabora       begsr
002300020308      *--------------------------------------------------
002400090112     c                   clear                   ok                1
002500090109     c* cerco il progressivo
002600090324     c     ktgt          chain     fitgt01l
002700090324     c                   if        %found(fitgt01l) and
002800090417     c*                            tgtatb = ' ' and tgtkm <> 0
002900090417     c                             tgtatb = ' '
003000020304      * per giornata
003100020304     c                   move      'G'           ktsr
003200011026     c                   exsr      fifgt
003300020304      * per consegna
003400020304     c                   move      'C'           ktsr
003500020304     c                   exsr      fifgt
003600020304      * per ritiro
003700020304     c                   move      'R'           ktsr
003800020304     c                   exsr      fifgt
003900090330     c     kapd          chain     fiapd01l
004000090401     c                   if        not %found(fiapd01l) or
004100090401     c                             (%found(fiapd01l) and apdatb <>' ')
004200090330     c                   clear                   ok
004300090402     c                   else
004400090402     c                   movel     apdflr        dapdflr
004500090330     c                   end
004600090112     c* aggiorno la supertestata se trovato dettaglio
004700090112     c                   if        ok = '1'
004800090112     c     tgtnrr        chain     fitgt00f
004900090325     c                   if        %found(fitgt00f) and tgtdts = 0
005000090408     c                   do
005100090401     c* se non presente il peso imposto quello dell'anagrafico
005200090401     c                   if        tgtpkl = 0
005300090402     c                   eval      tgtpkl = §APDMCP
005400090401     c                   end
005500090408     c* reperisco i tempi di carico e scarico
005600090408     c                   clear                   ficn08ds
005700090408     c                   eval      i08data= tgtddt
005800090408     c                   eval      i08tip= 'A'
005900090408     c                   eval      i08mcp = tgtpkl
006000090408     c                   call      'FICN08R'
006100090408     c                   parm                    ficn08ds
006200090408     c                   if        o08err <> ' '
006300090408     c                   clear                   ok
006400090408     c                   leave
006500090408     c                   end
006600090408     c                   eval      TGTTMPC = o08tc
006700090408     c                   eval      TGTTMPS = o08ts
006800090408     c* prezzo carburante
006900090408     c                   clear                   ficn05ds
007000090408     c                   call      'FICN05R'
007100090408     c                   parm                    ficn05ds
007200090408     c                   if        o05err <> ' '
007300090408     c                   clear                   ok
007400090408     c                   leave
007500090408     c                   end
007600090408     c                   eval      dgpi = o05gpi
007700090408     c                   z-add     D§GPIGPI      tgtpcar
007800090408     c*
007900090330     c                   movel     apdcsf        tgtsoc
008000090330     c                   movel     *all'0'       tgtcdf
008100090330     c                   move      apdksc        tgtcdf
008200090427     c                   move      20090501      tgtdts
008300090325     c                   move      dataoggi      tgtduv
008400090112     c                   update    fitgt000
008500090408     c                   enddo
008600090408     c                   end
008700090112     c                   end
008800090324     c                   end
008900090324
009000090324     c                   if        ok = ' '
009100090408     c                   rolbk
009200090324     c                   eval      cn02err = '1'
009300090325     c                   eval      cn02msg = 'Tariffa non convalidabile -
009400090325     c                             mancano dei dati'
009500090408     c                   else
009600090408     c                   commit
009700090324     c                   end
009800020308
009900020308     c                   endsr
010000020304
010100990113     c*-------------------------------------------------
010200011026     c     fifgt         begsr
010300990113     c*-------------------------------------------------
010400990113     c                   setoff                                           99
010500020305     c     kfgt          setll     fifgt03l
010600981029     c                   do        *hival
010700020305     c     kfgt          reade     fifgt03l                               99
010800981029     c   99              leave
010900090109     c* solo il progressivo della supertestata se l'ho trovata
011000090324     c                   if        fgtprg <> tgtprg
011100090109     c                   iter
011200090109     c                   end
011300090429      * non considero le tariffe che risultano non stampate
011400090429     c                   if        fgtdts = 0
011500020114     c                   iter
011600020114     c                   end
011700090427     c                   move      20090501      fgtdts
011800090325     c                   move      dataoggi      fgtduv
011900020308     c                   update    fifgt000
012000090112     c                   movel     '1'           ok
012100020304     c                   enddo
012200020304
012300020304     c                   endsr
012400981029      *
012500011026     c*-------------------------------------------------
012600011026     c     *inzsr        begsr
012700011026     c*-------------------------------------------------
012800020304     c     *entry        plist
012900090427     c                   parm                    ficn02ds
013000090325     c                   clear                   cn02err
013100090325     c                   clear                   cn02msg
013200090324     c                   if        cn02sml <> ' '
013300090324     c                   eval      cn02err = '1'
013400090324     c                   eval      cn02msg = 'Non valida la convalida -
013500090324     c                             per tariffe in simulazione'
013600090324     c                   seton                                        lr
013700090324     c                   return
013800090324     c                   end
013900090324     c*
014000020304     c     kfgt          klist
014100090324     c                   kfld                    CN02PDR
014200090324     c                   kfld                    CN02sml
014300020403     c                   kfld                    ktsr              1
014400090109     c     ktgt          klist
014500090324     c                   kfld                    CN02PDR
014600090324     c                   kfld                    CN02sml
014700090324     c                   kfld                    CN02PRG
014800090330     c     kapd          klist
014900090330     c                   kfld                    apdtip
015000090330     c                   kfld                    CN02PDR
015100090330     c                   eval      apdtip = 'A'
015200011026
015300020218     C                   time                    w0120            14 0
015400020218     C                   move      w0120         wdat              8 0
015500090324      * data da impostare su data stampa
015600020308     c                   move      wdat          dataeur
015700020308     c                   move      dataeur       dataiso
015800020308     c                   move      dataiso       dataoggi          8 0
015900020313
016000011026     c                   endsr
