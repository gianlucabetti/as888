000200090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000000     H*--------------------------------------------------------------*
000500090109     Ffifgt03l  uf   e           k disk    commit
000501090109     Ffitgt00f  uf   e             disk    commit
000502090504     Ffifpt03l  if   e           k disk
000504090504     Ffiapd01l  if   e           k disk
000505090330     Ffitgt01l  if   e           k disk    rename(fitgt000:fitgt001)
000506090109     F                                     INFDS(TAR)
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
001201090527     dparam            ds
001202090527     D lancio                         1
001203090527     d codpadr                        7s 0
001204090527     D codpadr2                       7s 0
001205090527     d alladat                        8s 0
001206090527     d pardtcerta                     8s 0
001207090527     d parmNRC                        7s 0
001208090527     d parmFIL                        3s 0
001209090527     D OUTQOVR                       10
001210090527     D errore                         1
001211090527     D msgError                      35
001300000000     C*---------------------------------------------------------------*
001400020304      *
001500020403     c                   exsr      elabora
001501090325     c                   movel     ficn02ds      kpjbu
001600020308     c                   seton                                        lr
001700020308      *--------------------------------------------------
001800020308     c     elabora       begsr
001900020308      *--------------------------------------------------
001902090112     c                   clear                   ok                1
001903090511     c                   clear                   okmga             1
001904090109     c* cerco il progressivo
001905090324     c     ktgt          chain     fitgt01l
001908090324     c                   if        %found(fitgt01l) and
001912090417     c*                            tgtatb = ' ' and tgtkm <> 0
001913090417     c                             tgtatb = ' '
001914090527      **  Chiama programma di controllo per verificare se tutto OK
001915090527      **   per stampare altrimenti deve uscire DANDO l'ERRORE
001916090527     c                   clear                   param
001917090527     c                   z-add     tgtpdr        codpadr
001918090527     c                   z-add     tgtpdr        codpadr2
001919090527     c                   z-add     tgtddt        alladat
001925090527     c                   movel     param         kpjbu
001926090527     c                   CALL      'FICN14R0'
001927090527     c                   parm                    kpjba
001928090527     c                   eval      param = kpjbu
001929090527     c                   if        errore <> *blank
001931090527     c                   eval      cn02err = '1'
001932090527     c                   eval      cn02msg = 'Tariffa non convalidabile -
001933090527     c                             mancano dei dati'
001934090527     c                   movel(p)  ficn02ds      kpjbu
001935090527     c                   seton                                        lr
001936090527     c                   return
001937090527     c                   end
002000020304      * per giornata
002100020304     c                   move      'G'           ktsr
002200011026     c                   exsr      fifgt
002201090505     c* controllo che esista il dettaglio
002202090505     c                   if        okfpt = ' ' and ok = '1'
002203090505     c                   clear                   ok
002204090505     c                   else
002300020304      * per consegna
002400020304     c                   move      'C'           ktsr
002500020304     c                   exsr      fifgt
002501090505     c* controllo che esista il dettaglio
002502090505     c                   if        okfpt = ' ' and ok = '1'
002503090505     c                   clear                   ok
002504090505     c                   else
002600020304      * per ritiro
002700020304     c                   move      'R'           ktsr
002800020304     c                   exsr      fifgt
002801090505     c* controllo che esista il dettaglio
002802090505     c                   if        okfpt = ' ' and ok = '1'
002803090505     c                   clear                   ok
002804090505     c                   else
002805090330     c     kapd          chain     fiapd01l
002806090401     c                   if        not %found(fiapd01l) or
002807090401     c                             (%found(fiapd01l) and apdatb <>' ')
002808090330     c                   clear                   ok
002809090402     c                   else
002810090402     c                   movel     apdflr        dapdflr
002811090330     c                   end
002812090505     c                   end
002813090505     c                   end
002814090505     c                   end
002815090112     c* aggiorno la supertestata se trovato dettaglio
002816090112     c                   if        ok = '1'
002817090112     c     tgtnrr        chain     fitgt00f
002818090325     c                   if        %found(fitgt00f) and tgtdts = 0
002819090408     c                   do
002820090401     c* se non presente il peso imposto quello dell'anagrafico
002821090401     c                   if        tgtpkl = 0
002822090402     c                   eval      tgtpkl = §APDMCP
002823090401     c                   end
002824090408     c* reperisco i tempi di carico e scarico
002825090408     c                   clear                   ficn08ds
002826090408     c                   eval      i08data= tgtddt
002827090408     c                   eval      i08tip= 'A'
002828090408     c                   eval      i08mcp = tgtpkl
002829090408     c                   call      'FICN08R'
002830090408     c                   parm                    ficn08ds
002831090408     c                   if        o08err <> ' '
002832090408     c                   clear                   ok
002833090408     c                   leave
002834090408     c                   end
002835090408     c                   eval      TGTTMPC = o08tc
002836090408     c                   eval      TGTTMPS = o08ts
002837090408     c* prezzo carburante
002838090408     c                   clear                   ficn05ds
002839090408     c                   call      'FICN05R'
002840090408     c                   parm                    ficn05ds
002841090408     c                   if        o05err <> ' '
002842090408     c                   clear                   ok
002843090408     c                   leave
002844090408     c                   end
002845090408     c                   eval      dgpi = o05gpi
002846090408     c                   z-add     D§GPIGPI      tgtpcar
002847090408     c*
002848090330     c                   movel     apdcsf        tgtsoc
002849090330     c                   movel     *all'0'       tgtcdf
002850090330     c                   move      apdksc        tgtcdf
002851090330     c                   move      dataoggi      tgtdts
002852090325     c                   move      dataoggi      tgtduv
002853090325     c                   movel(p)  knmus         tgtpru
002854090112     c                   update    fitgt000
002855090408     c                   enddo
002856090408     c                   end
002857090112     c                   end
002858090324     c                   end
002859090324
002860090511     c                   if        ok = ' ' or okmga = '1'
002861090408     c                   rolbk
002862090324     c                   eval      cn02err = '1'
002863090325     c                   eval      cn02msg = 'Tariffa non convalidabile -
002864090325     c                             mancano dei dati'
002865090408     c                   else
002866090408     c                   commit
002867090324     c                   end
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
004401090504     c* devo avere un dettaglio tariffario
004402090505     c                   movel     '1'           ok
004403090504     c                   exsr      fifpt
004404090504     c                   if        okfpt = ' '
004406090504     c                   iter
004407090504     c                   end
004408090511     c* verifico se ho il minimo garantito devo avere anche CONS/RIT
004409090511     c                   if        okmga = '1' and fgtctr <> 999
004410090511     c                             and (fgttsr = 'C' or
004411090511     c                             fgttsr = 'R')
004412090511     c                   clear                   okmga
004413090511     c                   end
004600090324     c                   move      dataoggi      fgtdts
004601090325     c                   move      dataoggi      fgtduv
004800020308     c                   update    fifgt000
004900020304     c                   enddo
005000020304
005100020304     c                   endsr
005200981029      *
005201090504     c*-------------------------------------------------
005202090504     c     fifpt         begsr
005203090504     c*-------------------------------------------------
005204090504     c
005205090504     c                   clear                   okfpt             1
005206090504     c     kfpt          setll     fifpt03l
005207090504     c                   do        *hival
005208090504     c     kfpt          reade     fifpt03l
005209090504     c                   if        %eof(fifpt03l)
005210090504     c                   leave
005211090504     c                   end
005212090504     c                   if        fptatb <> *blank
005213090504     c                   iter
005214090504     c                   endif
005215090511     c* verifico se ho il minimo garantito devo avere anche CONS/RIT
005216090511     c                   if        fptctd = 'MGA'
005217090511     c                   eval      okmga = '1'
005218090511     c                   end
005219090504     c                   eval      okfpt = '1'
005221090504     c                   enddo
005222090504     c                   endsr
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
006605090504     c     kfpt          klist
006606090504     c                   kfld                    fgtpdr
006607090504     c                   kfld                    fgtsml
006608090504     c                   kfld                    fgttsr
006609090504     c                   kfld                    fgtctr
006610090504     c                   kfld                    fgtprg
006611090330     c     kapd          klist
006612090330     c                   kfld                    apdtip
006613090330     c                   kfld                    CN02PDR
006614090330     c                   eval      apdtip = 'A'
006700011026
006800020218     C                   time                    w0120            14 0
006900020218     C                   move      w0120         wdat              8 0
007000090324      * data da impostare su data stampa
007100020308     c                   move      wdat          dataeur
007200020308     c                   move      dataeur       dataiso
007300020308     c                   move      dataiso       dataoggi          8 0
007800020313
007900011026     c                   endsr
