000100090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300090109     Ffifgt03l  uf   e           k disk    commit
000400090109     Ffitgt00f  uf   e             disk    commit
000500090504     Ffifpt03l  if   e           k disk
000600090504     Ffiapd01l  if   e           k disk
000700090330     Ffitgt01l  if   e           k disk    rename(fitgt000:fitgt001)
000800090109     F                                     INFDS(TAR)
000900020304     dkpjba          e ds
001000090408     dficn05ds       e ds
001100090408     dficn08ds       e ds
001200090408     ddgpi           e ds
001300090109     D TAR             DS
001400090109     D  tgtNRR               397    400B 0
001500090324     d ficn02ds      e ds
001600090402     d dapdflr       e ds
001700020305     d dataiso         s               d   datfmt(*iso)
001800020305     d dataeur         s               d   datfmt(*eur)
001900090527     dparam            ds
002000090527     D lancio                         1
002100090527     d codpadr                        7s 0
002200090527     D codpadr2                       7s 0
002300090527     d alladat                        8s 0
002400090527     d pardtcerta                     8s 0
002500090527     d parmNRC                        7s 0
002600090527     d parmFIL                        3s 0
002700090527     D OUTQOVR                       10
002800090527     D errore                         1
002900090527     D msgError                      35
003000000000     C*---------------------------------------------------------------*
003100020304      *
003200020403     c                   exsr      elabora
003300090325     c                   movel     ficn02ds      kpjbu
003400020308     c                   seton                                        lr
003500020308      *--------------------------------------------------
003600020308     c     elabora       begsr
003700020308      *--------------------------------------------------
003800090112     c                   clear                   ok                1
003900090511     c                   clear                   okmga             1
004000090109     c* cerco il progressivo
004100090324     c     ktgt          chain     fitgt01l
004200090324     c                   if        %found(fitgt01l) and
004300090417     c*                            tgtatb = ' ' and tgtkm <> 0
004400090417     c                             tgtatb = ' '
004500090527      **  Chiama programma di controllo per verificare se tutto OK
004600090527      **   per stampare altrimenti deve uscire DANDO l'ERRORE
004700090527     c                   clear                   param
004800090527     c                   z-add     tgtpdr        codpadr
004900090527     c                   z-add     tgtpdr        codpadr2
005000090527     c                   z-add     tgtddt        alladat
005100090527     c                   movel     param         kpjbu
005200090527     c                   CALL      'FICN14R0'
005300090527     c                   parm                    kpjba
005400090527     c                   eval      param = kpjbu
005500090527     c                   if        errore <> *blank
005600090527     c                   eval      cn02err = '1'
005700090918     c                   eval      cn02msg = msgError
005800090918     c*****              eval      cn02msg = 'Tariffa non convalidabile -
005900090918     c*****                        mancano dei dati'
006000090527     c                   movel(p)  ficn02ds      kpjbu
006100090527     c                   seton                                        lr
006200090527     c                   return
006300090527     c                   end
006400020304      * per giornata
006500020304     c                   move      'G'           ktsr
006600011026     c                   exsr      fifgt
006700090505     c* controllo che esista il dettaglio
006800090505     c                   if        okfpt = ' ' and ok = '1'
006900090505     c                   clear                   ok
007000090505     c                   else
007100020304      * per consegna
007200020304     c                   move      'C'           ktsr
007300020304     c                   exsr      fifgt
007400090505     c* controllo che esista il dettaglio
007500090505     c                   if        okfpt = ' ' and ok = '1'
007600090505     c                   clear                   ok
007700090505     c                   else
007800020304      * per ritiro
007900020304     c                   move      'R'           ktsr
008000020304     c                   exsr      fifgt
008100090505     c* controllo che esista il dettaglio
008200090505     c                   if        okfpt = ' ' and ok = '1'
008300090505     c                   clear                   ok
008400090505     c                   else
008500090330     c     kapd          chain     fiapd01l
008600090401     c                   if        not %found(fiapd01l) or
008700090401     c                             (%found(fiapd01l) and apdatb <>' ')
008800090330     c                   clear                   ok
008900090402     c                   else
009000090402     c                   movel     apdflr        dapdflr
009100090330     c                   end
009200090505     c                   end
009300090505     c                   end
009400090505     c                   end
009500090112     c* aggiorno la supertestata se trovato dettaglio
009600090112     c                   if        ok = '1'
009700090112     c     tgtnrr        chain     fitgt00f
009800090325     c                   if        %found(fitgt00f) and tgtdts = 0
009900090408     c                   do
010000090401     c* se non presente il peso imposto quello dell'anagrafico
010100090401     c                   if        tgtpkl = 0
010200090402     c                   eval      tgtpkl = §APDMCP
010300090401     c                   end
010400090408     c* reperisco i tempi di carico e scarico
010500090408     c                   clear                   ficn08ds
010600090408     c                   eval      i08data= tgtddt
010700090408     c                   eval      i08tip= 'A'
010800090408     c                   eval      i08mcp = tgtpkl
010900090408     c                   call      'FICN08R'
011000090408     c                   parm                    ficn08ds
011100090408     c                   if        o08err <> ' '
011200090408     c                   clear                   ok
011300090408     c                   leave
011400090408     c                   end
011500090408     c                   eval      TGTTMPC = o08tc
011600090408     c                   eval      TGTTMPS = o08ts
011700090408     c* prezzo carburante
011800090408     c                   clear                   ficn05ds
011900090408     c                   call      'FICN05R'
012000090408     c                   parm                    ficn05ds
012100090408     c                   if        o05err <> ' '
012200090408     c                   clear                   ok
012300090408     c                   leave
012400090408     c                   end
012500090408     c                   eval      dgpi = o05gpi
012600090408     c                   z-add     D§GPIGPI      tgtpcar
012700090408     c*
012800090330     c                   movel     apdcsf        tgtsoc
012900090330     c                   movel     *all'0'       tgtcdf
013000090330     c                   move      apdksc        tgtcdf
013100090330     c                   move      dataoggi      tgtdts
013200090325     c                   move      dataoggi      tgtduv
013300090325     c                   movel(p)  knmus         tgtpru
013400090112     c                   update    fitgt000
013500090408     c                   enddo
013600090408     c                   end
013700090112     c                   end
013800090324     c                   end
013900090324
014000090511     c                   if        ok = ' ' or okmga = '1'
014100090408     c                   rolbk
014200090324     c                   eval      cn02err = '1'
014300090325     c                   eval      cn02msg = 'Tariffa non convalidabile -
014400090325     c                             mancano dei dati'
014500090408     c                   else
014600090408     c                   commit
014700090324     c                   end
014800020308
014900020308     c                   endsr
015000020304
015100990113     c*-------------------------------------------------
015200011026     c     fifgt         begsr
015300990113     c*-------------------------------------------------
015400990113     c                   setoff                                           99
015500020305     c     kfgt          setll     fifgt03l
015600981029     c                   do        *hival
015700020305     c     kfgt          reade     fifgt03l                               99
015800981029     c   99              leave
015900090109     c* solo il progressivo della supertestata se l'ho trovata
016000090324     c                   if        fgtprg <> tgtprg
016100090109     c                   iter
016200090109     c                   end
016300090324      * non considero le tariffe che risultano già stampate
016400090324     c                   if        fgtdts <>0
016500020114     c                   iter
016600020114     c                   end
016700090504     c* devo avere un dettaglio tariffario
016800090505     c                   movel     '1'           ok
016900090504     c                   exsr      fifpt
017000090504     c                   if        okfpt = ' '
017100090504     c                   iter
017200090504     c                   end
017300090511     c* verifico se ho il minimo garantito devo avere anche CONS/RIT
017400090511     c                   if        okmga = '1' and fgtctr <> 999
017500090511     c                             and (fgttsr = 'C' or
017600090511     c                             fgttsr = 'R')
017700090511     c                   clear                   okmga
017800090511     c                   end
017900090324     c                   move      dataoggi      fgtdts
018000090325     c                   move      dataoggi      fgtduv
018100020308     c                   update    fifgt000
018200020304     c                   enddo
018300020304
018400020304     c                   endsr
018500981029      *
018600090504     c*-------------------------------------------------
018700090504     c     fifpt         begsr
018800090504     c*-------------------------------------------------
018900090504     c
019000090504     c                   clear                   okfpt             1
019100090504     c     kfpt          setll     fifpt03l
019200090504     c                   do        *hival
019300090504     c     kfpt          reade     fifpt03l
019400090504     c                   if        %eof(fifpt03l)
019500090504     c                   leave
019600090504     c                   end
019700090504     c                   if        fptatb <> *blank
019800090504     c                   iter
019900090504     c                   endif
020000090511     c* verifico se ho il minimo garantito devo avere anche CONS/RIT
020100090511     c                   if        fptctd = 'MGA'
020200090511     c                   eval      okmga = '1'
020300090511     c                   end
020400090504     c                   eval      okfpt = '1'
020500090504     c                   enddo
020600090504     c                   endsr
020700011026     c*-------------------------------------------------
020800011026     c     *inzsr        begsr
020900011026     c*-------------------------------------------------
021000020304     c     *entry        plist
021100020304     c                   parm                    kpjba
021200090324     c                   movel     kpjbu         ficn02ds
021300090325     c                   clear                   cn02err
021400090325     c                   clear                   cn02msg
021500090324     c                   if        cn02sml <> ' '
021600090324     c                   eval      cn02err = '1'
021700090324     c                   eval      cn02msg = 'Non valida la convalida -
021800090324     c                             per tariffe in simulazione'
021900090324     c                   seton                                        lr
022000090324     c                   return
022100090324     c                   end
022200090324     c*
022300020304     c     kfgt          klist
022400090324     c                   kfld                    CN02PDR
022500090324     c                   kfld                    CN02sml
022600020403     c                   kfld                    ktsr              1
022700090109     c     ktgt          klist
022800090324     c                   kfld                    CN02PDR
022900090324     c                   kfld                    CN02sml
023000090324     c                   kfld                    CN02PRG
023100090504     c     kfpt          klist
023200090504     c                   kfld                    fgtpdr
023300090504     c                   kfld                    fgtsml
023400090504     c                   kfld                    fgttsr
023500090504     c                   kfld                    fgtctr
023600090504     c                   kfld                    fgtprg
023700090330     c     kapd          klist
023800090330     c                   kfld                    apdtip
023900090330     c                   kfld                    CN02PDR
024000090330     c                   eval      apdtip = 'A'
024100011026
024200020218     C                   time                    w0120            14 0
024300020218     C                   move      w0120         wdat              8 0
024400090324      * data da impostare su data stampa
024500020308     c                   move      wdat          dataeur
024600020308     c                   move      dataeur       dataiso
024700020308     c                   move      dataiso       dataoggi          8 0
024800020313
024900011026     c                   endsr
