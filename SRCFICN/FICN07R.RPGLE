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
004000131126     c                   clear                   okgio             1
004100090109     c* cerco il progressivo
004200090324     c     ktgt          chain     fitgt01l
004300090324     c                   if        %found(fitgt01l) and
004400090417     c*                            tgtatb = ' ' and tgtkm <> 0
004500090417     c                             tgtatb = ' '
004600090527      **  Chiama programma di controllo per verificare se tutto OK
004700090527      **   per stampare altrimenti deve uscire DANDO l'ERRORE
004800090527     c                   clear                   param
004900090527     c                   z-add     tgtpdr        codpadr
005000090527     c                   z-add     tgtpdr        codpadr2
005100090527     c                   z-add     tgtddt        alladat
005200090527     c                   movel     param         kpjbu
005300090527     c                   CALL      'FICN14R0'
005400090527     c                   parm                    kpjba
005500090527     c                   eval      param = kpjbu
005600090527     c                   if        errore <> *blank
005700090527     c                   eval      cn02err = '1'
005800090918     c                   eval      cn02msg = msgError
005900090918     c*****              eval      cn02msg = 'Tariffa non convalidabile -
006000090918     c*****                        mancano dei dati'
006100090527     c                   movel(p)  ficn02ds      kpjbu
006200090527     c                   seton                                        lr
006300090527     c                   return
006400090527     c                   end
006500020304      * per giornata
006600020304     c                   move      'G'           ktsr
006700011026     c                   exsr      fifgt
006800090505     c* controllo che esista il dettaglio
006900090505     c                   if        okfpt = ' ' and ok = '1'
007000090505     c                   clear                   ok
007100090505     c                   else
007200020304      * per consegna
007300020304     c                   move      'C'           ktsr
007400020304     c                   exsr      fifgt
007500090505     c* controllo che esista il dettaglio
007600090505     c                   if        okfpt = ' ' and ok = '1'
007700090505     c                   clear                   ok
007800090505     c                   else
007900020304      * per ritiro
008000020304     c                   move      'R'           ktsr
008100020304     c                   exsr      fifgt
008200090505     c* controllo che esista il dettaglio
008300090505     c                   if        okfpt = ' ' and ok = '1'
008400090505     c                   clear                   ok
008500090505     c                   else
008600090330     c     kapd          chain     fiapd01l
008700090401     c                   if        not %found(fiapd01l) or
008800090401     c                             (%found(fiapd01l) and apdatb <>' ')
008900090330     c                   clear                   ok
009000090402     c                   else
009100090402     c                   movel     apdflr        dapdflr
009200090330     c                   end
009300090505     c                   end
009400090505     c                   end
009500090505     c                   end
009600090112     c* aggiorno la supertestata se trovato dettaglio
009700090112     c                   if        ok = '1'
009800090112     c     tgtnrr        chain     fitgt00f
009900090325     c                   if        %found(fitgt00f) and tgtdts = 0
010000090408     c                   do
010100090401     c* se non presente il peso imposto quello dell'anagrafico
010200090401     c                   if        tgtpkl = 0
010300090402     c                   eval      tgtpkl = §APDMCP
010400090401     c                   end
010500090408     c* reperisco i tempi di carico e scarico
010600090408     c                   clear                   ficn08ds
010700090408     c                   eval      i08data= tgtddt
010800090408     c                   eval      i08tip= 'A'
010900090408     c                   eval      i08mcp = tgtpkl
011000090408     c                   call      'FICN08R'
011100090408     c                   parm                    ficn08ds
011200090408     c                   if        o08err <> ' '
011300090408     c                   clear                   ok
011400090408     c                   leave
011500090408     c                   end
011600090408     c                   eval      TGTTMPC = o08tc
011700090408     c                   eval      TGTTMPS = o08ts
011800090408     c* prezzo carburante
011900090408     c                   clear                   ficn05ds
012000090408     c                   call      'FICN05R'
012100090408     c                   parm                    ficn05ds
012200090408     c                   if        o05err <> ' '
012300090408     c                   clear                   ok
012400090408     c                   leave
012500090408     c                   end
012600090408     c                   eval      dgpi = o05gpi
012700090408     c                   z-add     D§GPIGPI      tgtpcar
012800090408     c*
012900090330     c                   movel     apdcsf        tgtsoc
013000090330     c                   movel     *all'0'       tgtcdf
013100090330     c                   move      apdksc        tgtcdf
013200090330     c                   move      dataoggi      tgtdts
013300090325     c                   move      dataoggi      tgtduv
013400090325     c                   movel(p)  knmus         tgtpru
013500090112     c                   update    fitgt000
013600090408     c                   enddo
013700090408     c                   end
013800090112     c                   end
013900090324     c                   end
014000090324
014100090511     c                   if        ok = ' ' or okmga = '1'
014200131126     c                               or
014300131126     c                             (okmga ='1' and okgio ='1')
014400090408     c                   rolbk
014500090324     c                   eval      cn02err = '1'
014600131126     c                   if         okmga ='1' and okgio ='1'
014700131126     c                   eval      cn02msg = 'Errore: Presente Tar.MGA e Tar.a-
014800131126     c                              Giornata'
014900131126     c                   else
015000131126     c                   eval      cn02msg = 'Errore: Presente Tar.MGA senza T-
015100131126     c                             ar.di dettaglio'
015200131126     c                   end
015300090408     c                   else
015400090408     c                   commit
015500090324     c                   end
015600020308
015700020308     c                   endsr
015800020304
015900990113     c*-------------------------------------------------
016000011026     c     fifgt         begsr
016100990113     c*-------------------------------------------------
016200990113     c                   setoff                                           99
016300020305     c     kfgt          setll     fifgt03l
016400981029     c                   do        *hival
016500020305     c     kfgt          reade     fifgt03l                               99
016600981029     c   99              leave
016700090109     c* solo il progressivo della supertestata se l'ho trovata
016800090324     c                   if        fgtprg <> tgtprg
016900090109     c                   iter
017000090109     c                   end
017100090324      * non considero le tariffe che risultano già stampate
017200090324     c                   if        fgtdts <>0
017300020114     c                   iter
017400020114     c                   end
017500090504     c* devo avere un dettaglio tariffario
017600090505     c                   movel     '1'           ok
017700090504     c                   exsr      fifpt
017800090504     c                   if        okfpt = ' '
017900090504     c                   iter
018000090504     c                   end
018100090511     c* verifico se ho il minimo garantito devo avere anche CONS/RIT
018200090511     c                   if        okmga = '1' and fgtctr <> 999
018300090511     c                             and (fgttsr = 'C' or
018400090511     c                             fgttsr = 'R')
018500090511     c                   clear                   okmga
018600090511     c                   end
018700090324     c                   move      dataoggi      fgtdts
018800090325     c                   move      dataoggi      fgtduv
018900020308     c                   update    fifgt000
019000020304     c                   enddo
019100020304
019200020304     c                   endsr
019300981029      *
019400090504     c*-------------------------------------------------
019500090504     c     fifpt         begsr
019600090504     c*-------------------------------------------------
019700090504     c
019800090504     c                   clear                   okfpt             1
019900090504     c     kfpt          setll     fifpt03l
020000090504     c                   do        *hival
020100090504     c     kfpt          reade     fifpt03l
020200090504     c                   if        %eof(fifpt03l)
020300090504     c                   leave
020400090504     c                   end
020500090504     c                   if        fptatb <> *blank
020600090504     c                   iter
020700090504     c                   endif
020800090511     c* verifico se ho il minimo garantito devo avere anche CONS/RIT
020900090511     c                   if        fptctd = 'MGA'
021000090511     c                   eval      okmga = '1'
021100131126     c                   end
021200131126     c                   if        fptctd = 'TIG' or
021300131126     c                             fptctd = 'TMG'
021400131126     c                   eval      okgio = '1'
021500131126     c                   end
021600090504     c                   eval      okfpt = '1'
021700090504     c                   enddo
021800090504     c                   endsr
021900011026     c*-------------------------------------------------
022000011026     c     *inzsr        begsr
022100011026     c*-------------------------------------------------
022200020304     c     *entry        plist
022300020304     c                   parm                    kpjba
022400090324     c                   movel     kpjbu         ficn02ds
022500090325     c                   clear                   cn02err
022600090325     c                   clear                   cn02msg
022700090324     c                   if        cn02sml <> ' '
022800090324     c                   eval      cn02err = '1'
022900090324     c                   eval      cn02msg = 'Non valida la convalida -
023000090324     c                             per tariffe in simulazione'
023100090324     c                   seton                                        lr
023200090324     c                   return
023300090324     c                   end
023400090324     c*
023500020304     c     kfgt          klist
023600090324     c                   kfld                    CN02PDR
023700090324     c                   kfld                    CN02sml
023800020403     c                   kfld                    ktsr              1
023900090109     c     ktgt          klist
024000090324     c                   kfld                    CN02PDR
024100090324     c                   kfld                    CN02sml
024200090324     c                   kfld                    CN02PRG
024300090504     c     kfpt          klist
024400090504     c                   kfld                    fgtpdr
024500090504     c                   kfld                    fgtsml
024600090504     c                   kfld                    fgttsr
024700090504     c                   kfld                    fgtctr
024800090504     c                   kfld                    fgtprg
024900090330     c     kapd          klist
025000090330     c                   kfld                    apdtip
025100090330     c                   kfld                    CN02PDR
025200090330     c                   eval      apdtip = 'A'
025300011026
025400020218     C                   time                    w0120            14 0
025500020218     C                   move      w0120         wdat              8 0
025600090324      * data da impostare su data stampa
025700020308     c                   move      wdat          dataeur
025800020308     c                   move      dataeur       dataiso
025900020308     c                   move      dataiso       dataoggi          8 0
026000020313
026100011026     c                   endsr
