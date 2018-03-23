000200090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000000     H*--------------------------------------------------------------*
000500090109     Ffifgt03l  uf   e           k disk    commit
000501090109     Ffitgt00f  uf   e             disk    commit
000502090330     Ffiapd01l  if   e           k disk
000503090330     Ffitgt01l  if   e           k disk    rename(fitgt000:fitgt001)
000504090109     F                                     INFDS(TAR)
000600020304     dkpjba          e ds
000601090408     dficn05ds       e ds
000602090408     dficn08ds       e ds
000603090408     ddgpi           e ds
000604090109     D TAR             DS
000605090109     D  tgtNRR               397    400B 0
000606090324     d ficn02ds      e ds
000607090402     d dapdflr       e ds
001100020305     d dataiso         s               d   datfmt(*iso)
001200020305     d dataeur         s               d   datfmt(*eur)
001300000000     C*---------------------------------------------------------------*
001400020304      *
001500020403     c                   exsr      elabora
001501090325     c                   movel     ficn02ds      kpjbu
001600020308     c                   seton                                        lr
001700020308      *--------------------------------------------------
001800020308     c     elabora       begsr
001900020308      *--------------------------------------------------
001902090112     c                   clear                   ok                1
001904090109     c* cerco il progressivo
001905090324     c     ktgt          chain     fitgt01l
001908090324     c                   if        %found(fitgt01l) and
001912090417     c*                            tgtatb = ' ' and tgtkm <> 0
001913090417     c                             tgtatb = ' '
002000020304      * per giornata
002100020304     c                   move      'G'           ktsr
002200011026     c                   exsr      fifgt
002300020304      * per consegna
002400020304     c                   move      'C'           ktsr
002500020304     c                   exsr      fifgt
002600020304      * per ritiro
002700020304     c                   move      'R'           ktsr
002800020304     c                   exsr      fifgt
002801090330     c     kapd          chain     fiapd01l
002802090401     c                   if        not %found(fiapd01l) or
002803090401     c                             (%found(fiapd01l) and apdatb <>' ')
002805090330     c                   clear                   ok
002806090402     c                   else
002807090402     c                   movel     apdflr        dapdflr
002808090330     c                   end
002809090112     c* aggiorno la supertestata se trovato dettaglio
002810090112     c                   if        ok = '1'
002811090112     c     tgtnrr        chain     fitgt00f
002812090325     c                   if        %found(fitgt00f) and tgtdts = 0
002813090408     c                   do
002814090401     c* se non presente il peso imposto quello dell'anagrafico
002815090401     c                   if        tgtpkl = 0
002816090402     c                   eval      tgtpkl = §APDMCP
002817090401     c                   end
002818090408     c* reperisco i tempi di carico e scarico
002819090408     c                   clear                   ficn08ds
002820090408     c                   eval      i08data= tgtddt
002821090408     c                   eval      i08tip= 'A'
002822090408     c                   eval      i08mcp = tgtpkl
002823090408     c                   call      'FICN08R'
002824090408     c                   parm                    ficn08ds
002825090408     c                   if        o08err <> ' '
002826090408     c                   clear                   ok
002827090408     c                   leave
002828090408     c                   end
002829090408     c                   eval      TGTTMPC = o08tc
002830090408     c                   eval      TGTTMPS = o08ts
002831090408     c* prezzo carburante
002832090408     c                   clear                   ficn05ds
002833090408     c                   call      'FICN05R'
002834090408     c                   parm                    ficn05ds
002835090408     c                   if        o05err <> ' '
002836090408     c                   clear                   ok
002837090408     c                   leave
002838090408     c                   end
002839090408     c                   eval      dgpi = o05gpi
002840090408     c                   z-add     D§GPIGPI      tgtpcar
002841090408     c*
002842090330     c                   movel     apdcsf        tgtsoc
002843090330     c                   movel     *all'0'       tgtcdf
002844090330     c                   move      apdksc        tgtcdf
002845090330     c                   move      dataoggi      tgtdts
002846090325     c                   move      dataoggi      tgtduv
002847090325     c                   movel(p)  knmus         tgtpru
002848090112     c                   update    fitgt000
002849090408     c                   enddo
002850090408     c                   end
002851090112     c                   end
002855090324     c                   end
002856090324
002857090324     c                   if        ok = ' '
002858090408     c                   rolbk
002859090324     c                   eval      cn02err = '1'
002860090325     c                   eval      cn02msg = 'Tariffa non convalidabile -
002861090325     c                             mancano dei dati'
002862090408     c                   else
002863090408     c                   commit
002864090324     c                   end
002900020308
003000020308     c                   endsr
003100020304
003200990113     c*-------------------------------------------------
003300011026     c     fifgt         begsr
003400990113     c*-------------------------------------------------
003500990113     c                   setoff                                           99
003600020305     c     kfgt          setll     fifgt03l
003700981029     c                   do        *hival
003800020305     c     kfgt          reade     fifgt03l                               99
003900981029     c   99              leave
003901090109     c* solo il progressivo della supertestata se l'ho trovata
003902090324     c                   if        fgtprg <> tgtprg
003903090109     c                   iter
003904090109     c                   end
004000090324      * non considero le tariffe che risultano già stampate
004200090324     c                   if        fgtdts <>0
004300020114     c                   iter
004400020114     c                   end
004600090324     c                   move      dataoggi      fgtdts
004601090325     c                   move      dataoggi      fgtduv
004800020308     c                   update    fifgt000
004801090112     c                   movel     '1'           ok
004900020304     c                   enddo
005000020304
005100020304     c                   endsr
005200981029      *
005300011026     c*-------------------------------------------------
005400011026     c     *inzsr        begsr
005500011026     c*-------------------------------------------------
005600020304     c     *entry        plist
005700020304     c                   parm                    kpjba
005800090324     c                   movel     kpjbu         ficn02ds
005801090325     c                   clear                   cn02err
005802090325     c                   clear                   cn02msg
005803090324     c                   if        cn02sml <> ' '
005804090324     c                   eval      cn02err = '1'
005805090324     c                   eval      cn02msg = 'Non valida la convalida -
005806090324     c                             per tariffe in simulazione'
005807090324     c                   seton                                        lr
005808090324     c                   return
005809090324     c                   end
006100090324     c*
006200020304     c     kfgt          klist
006300090324     c                   kfld                    CN02PDR
006400090324     c                   kfld                    CN02sml
006500020403     c                   kfld                    ktsr              1
006601090109     c     ktgt          klist
006602090324     c                   kfld                    CN02PDR
006603090324     c                   kfld                    CN02sml
006604090324     c                   kfld                    CN02PRG
006605090330     c     kapd          klist
006606090330     c                   kfld                    apdtip
006607090330     c                   kfld                    CN02PDR
006608090330     c                   eval      apdtip = 'A'
006700011026
006800020218     C                   time                    w0120            14 0
006900020218     C                   move      w0120         wdat              8 0
007000090324      * data da impostare su data stampa
007100020308     c                   move      wdat          dataeur
007200020308     c                   move      dataeur       dataiso
007300020308     c                   move      dataiso       dataoggi          8 0
007800020313
007900011026     c                   endsr
