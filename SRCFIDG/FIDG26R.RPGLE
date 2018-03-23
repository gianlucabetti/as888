000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300070213     Ffidst08l  if   e           k disk
000400021203     Ffiapd01l  if   e           k disk
000500030723     Fazorg01l  if   e           k disk
000600070214     Ffidst31l  if   e           k disk
000700070215     Ffidst32l  if   e           k disk    rename(fidst300:fidst32)
000800070214     Ffigrg03l  if   e           k disk
000900070403     Ffiarg52j  if   e           k disk
001000070214     Ffnarb01l  if   e           k disk
001100100208     Ftabel00f  if   e           k disk
001200070214     Fprtf198   O    F  198        PRINTER prtctl(prtds)
001300020304     dkpjba          e ds
001400030505     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500030505     D CNCR80        E DS
001600070213     D fidg26ds      e DS
001700070302     D fidg27ds      e DS
001800070214     D Fidg16ds      E DS
001900100208     D ds7r          E DS
002000070403     D og146         E DS
002001160421     D og150         E DS
002100070214     D fnarbds       e ds                  extname(fnarb00f)
002200070214     D fiargds       e ds                  extname(fiarg00f)
002300160421     d ds03          e ds
002301160421     d ddstflr       e ds
002302160421     d dgrgflo       e ds
002400130423     d tnsd99ds      e ds
002500020404     d prtds           ds
002600020404     d   spab                         3  0
002700020404     d   spaa                         3  0
002800020404     d   skab                         3  0
002900020404     d   skaa                         3  0
003000020404     d   line                         3  0
003100030724     d primo           s              1
003200020305     d dataiso         s               d   datfmt(*iso)
003300020305     d dataeur         s               d   datfmt(*eur)
003400070214     d diffpkl         s                   like(dstpkl)
003500070214     d diffvlu         s                   like(dstvlu)
003600070214     d diffnftl        s                   like(dstnftl)
003700070221     d diffnbl         s                   like(dstnbl)
003800070214     d desgir          s                   like(grgdes)
003900070214     d exargcgi        s                   like(argcgi)
004000070510     d totpkl          s                   like(dstpkl) inz
004100070510     d totvlu          s              7  3 inz
004200070510     d totspe          s              5  0 inz
004300070510     d totcol          s              5  0 inz
004400070510     d ttotpkl         s                   like(dstpkl) inz
004500070510     d ttotvlu         s              7  3 inz
004600070510     d ttotspe         s              5  0 inz
004700070510     d ttotcol         s              5  0 inz
004800070510     d wpes            s                   like(dstpkl) inz
004900070510     d wvol            s                   like(dstvlu) inz
005000070215     d dis             s              6  0 dim(5)
005100070215     d pta             s              2  0 dim(5)
005200070215     d cn              s              1  0
005300070510     d tdstpkla        s                   like(dstpkla) inz
005400070629     d tdstvlua        s              7  3 inz
005500070510     d tdststpa        s                   like(dststpa) inz
005600070510     d tdstnbla        s                   like(dstnbla) inz
005700100208      *
005800000000     C*---------------------------------------------------------------*
005900020304      *
006000070329     c                   if        d26spea = 'S'
006100070213     c                   exsr      fidst
006200070329     c                   endif
006300070214     c                   z-add     99            line
006400070329     c                   if        d26noas = 'S'
006500070214     c                   exsr      fiarg
006600070329     c                   endif
006700070215     c                   seton                                        lr
006800990113     c*-------------------------------------------------
006900070213     c     fidst         begsr
007000990113     c*-------------------------------------------------
007100070213     c     kdst          setll     fidst08l
007200981029     c                   do        *hival
007300070213     c     kdst          reade     fidst08l
007400070213     c                   if        %eof(fidst08l)
007500070213     c                   leave
007600070213     c                   endif
007601160421     c                   eval      ddstflr = dstflr
007700070213      * richiesta una sola distinta
007800070329      * oppure distinta annullata
007900070213     c                   if        d26dis <> 0 and dstnfv <> d26dis
008000070329     c                             or dstatb = 'A'
008100031114     c                   iter
008200031114     c                   endif
008300070618      * richieste solo distinte PARCEL/ oppure solo messaggerie
008400070618     c                   if        (d26pame = 'P' and dsttpm <>' ') or
008500070618     c                             (d26pame = 'M' and dsttpm <>'M')
008600070618     c                   iter
008700070618     c                   endif
008701160422     c* turno solo per parcel
008702160421     c                   if        (d26tur1 = ' ' or d26tur2 = ' ')
008703160421     c                             and d26tur1 <> 'U' and d26tur2 <> 'U'
008704160422     c                             and dsttpm = ' '
008705160421     c                   select
008706160421     c                   when      (d26tur1 = '1' or
008707160421     c                              d26tur2 = '1') and
008708160421     c                             §dstturno <> ' '
008709160421     c                   iter
008710160421     c                   when      (d26tur1 = '2' or
008711160421     c                              d26tur2 = '2') and
008712160421     c                             §dstturno <> '2'
008713160421     c                   iter
008714160421     c                   endsl
008715160421     c                   end
008800070830      * richieste solo distinte mattina e pomeriggio
008900070830     c                   if        (d26FPPI = 'P' and dstfpp <>'P') or
009000070830     c                             (d26FPPI = 'M' and dstfpp = 'P')
009100070830     c                   iter
009200070830     c                   endif
009300070618      *
009400070213     c     kapd          chain     fiapd01l                           97
009500070213      *se l'anagrafica non corrisponde non stampo la decodifica
009600070213     c   97              clear                   despdr           25
009700070213     c  n97              movel     apdrsc        despdr
009800070214      *reperisco descrizione primo giro distinta
009900070214     c                   clear                   desgir
010000070215     c     kdst3         chain     fidst31l                           97
010100070214     c                   if        %found(fidst31l)
010200070214     c     kgrg          setll     figrg03l
010300070214     c                   do        *hival
010400070214     c     kgrg          reade     figrg03l
010500070214     c                   if        %eof(figrg03l)
010600070214     c                   leave
010700070214     c                   endif
010800070706     c                   eval      dgrgflo = grgflo
010900070214     c                   if        dstdfv >= grgdde and dstdfv <= grgdscad
011000070706      * verifico tipo utilizzo giro
011100070706     c                             and (§grgtug = 'C' or §grgtug = 'E')
011200070214     c                   movel     grgdes        desgir
011300070214     c                   endif
011400070214     c                   enddo
011500070214     c                   endif
011600070214
011700030724     c                   if        line > 60
011800030724     c                   except    testata
011900030724     c                   endif
012000070214      * calcolo le differenze
012100070214     c                   clear                   diffpkl
012200070214     c                   clear                   diffvlu
012300070214     c                   clear                   diffnftl
012400070221     c                   clear                   diffnbl
012500070214     c     dstpkla       sub       dstpkl        diffpkl
012600070214     c     dstvlua       sub       dstvlu        diffvlu
012700070214     c     dststpa       sub       dstnftl       diffnftl
012800070330
012900070330     c     diffpkl       comp      0                                  30
013000070330     c     diffvlu       comp      0                                  31
013100070330     c     diffnftl      comp      0                                  32
013200070510      * totalizzo
013300070510     c                   add       dstpkla       tdstpkla
013400070510     c                   add       dstvlua       tdstvlua
013500070510     c                   add       dststpa       tdststpa
013600070510     c                   add       dstnbla       tdstnbla
013700070214     c
013800070213     c                   except    detta
013900030724     c
014000020304     c                   enddo
014100070510     c                   except    totalia
014200020304
014300020304     c                   endsr
014400981029      *
014500070214     c*-------------------------------------------------
014600070214     c     fiarg         begsr
014700070214     c*-------------------------------------------------
014800070403     c     filk          setll     fiarg52j
014900070214     c                   do        *hival
015000070403     c     filk          reade     fiarg52j
015100070403     c                   if        %eof(fiarg52j)
015200070214     c                   leave
015300070214     c                   endif
015400070403     c*    karb          chain     fnarb01l
015500070403     c*                  if        %found(fnarb01l)
015600070403      * se parallelo testa la distinta su FIARG
015700070424     c                   if        §ogdda = '20391231' and argndc > 0 or
015800070424     c                             wdymd < ogddaN and argndc > 0
015900070403     c                   iter
016000070403     c                   endif
016100070403      * se produzione testa la distinta su FNARB
016200070403     c                   if        wdymd >= ogddaN and arbndc > 0
016300070403     c                   iter
016400070403     c                   endif
016500070330     c
016600070214      * richiamo pgm per esito consegnabilità
016700070214     c                   exsr      buona
016800070214     c                   if        d16conso = 'N'
016900070214     c                   iter
017000070214     c                   endif
017002160502     c* se richiesto di vedere o meno le bolle con zona del turno scelto    sa
017003160502     c                   if        *in72 and
017004160502     c                             (d26tur1 = ' ' or d26tur2 = ' ')
017005160502     c                             and (d26tur1 <>'U' and d26tur2 <>'U')
017013160421     c* nel turno 1 devo escludere le zone del turno 2
017016160421     c                   clear                   ds03
017017160421     C                   eval      tblcod = '03'
017018160421     C                   movel     d26fil        com05             5
017019160421     C                   move      arbznc        com05             5
017020160421     C                   movel(p)  com05         tblkey
017021160421     C     ktab          chain     Tabel00f
017022160421     c                   if        %found(tabel00f)
017023160421     C                   MOVEL     TBLUNI        DS03
017024160502     c                   else
017025160502     c                   clear                   ds03
017027160421     c                   end
017028160502     c                   if        §03turno = ' ' and §03tpm = ' '
017029160502     c                   eval      §03turno = '1'
017030160502     c                   end
017031160502     c* se scelto solo parcel ecldudo anche messaggerie
017032160502     c                   if        d26sptur= 'S' and §03turno = ' '
017033160502     c                             and d26pame ='P'
017034160502     C                   iter
017035160502     c                   end
017036160502     c* se scelto di vedere solo le spedizioni del turno scelto
017037160502     c                   if        d26sptur= 'S' and §03turno <> ' ' and (
017038160502     c                             (d26tur1<>' ' and §03turno <> d26tur1) or
017039160502     c                             (d26tur2<>' ' and §03turno <> d26tur2))
017040160502     c                   iter
017041160502     C                   END
017042160502     C                   END
017100100208      * verifica richiesta esclusione spedizioni con particolarità attesa
017200100208     c                   if        d26atte = 'N' and arbgma <> *blank and
017300100208     c                             arbtcr  = 'P' and arbdcr > datk
017400100208     C                   MOVEL     '7R'          tblcod
017500100208     C                   movel     arbgma        tblkey
017600100208     C     Ktab          chain     tabel00F
017700100208     c                   if        %found(tabel00f)
017800100208     c                   movel     tbluni        ds7r
017900100208     c                   if        §7ratt = 'S'
018000130429     c                   clear                   tnsd99ds
018100130423     C                   Z-ADD     d16dfvi       D98DFC
018200130423     C                   Z-ADD     2359          D98OFC
018300130423     C                   MOVEL     ' '           D98TLA
018400130423     C                   MOVEL     'A'           D98TBO
018500130423     C                   Z-ADD     ARBAAS        D98AAS
018600130423     C                   Z-ADD     ARBLNP        D98LNP
018700130423     C                   Z-ADD     ARBNRS        D98NRS
018800130423     C                   Z-ADD     ARBNSP        D98NSP
018900130423     C                   CALL      'TNSD99R'
019000130423     C                   PARM                    TNSD99DS
019100130423     c                   if        d98nci <=-24
019200100208     c                   iter
019300100208     c                   endif
019400100208     c                   endif
019500100208     c                   endif
019600100208     c                   endif
019700070214      * primo rekord
019800070214     c                   if        primo = *blank
019900070214     c                   move      argcgi        exargcgi
020000070214     c                   move      'X'           primo
020100070214     c                   endif
020200070214     c                   if        exargcgi = argcgi
020300070302      * reperisce peso volume colli consegnabili
020400070302     c                   exsr      calpesvol
020500070302     c                   add       D27PKLDCO     totpkl
020600070302     c                   add       D27VLUDCO     totvlu
020700070302     c                   add       D27NCLDCO     totcol
020800070302     c                   add       1             totspe
020900070214     c                   else
021000070214      *reperisco descrizione primo giro distinta
021100070214     c                   clear                   desgir
021200070214     c     kgrg2         setll     figrg03l
021300070214     c                   do        *hival
021400070214     c     kgrg2         reade     figrg03l
021500070214     c                   if        %eof(figrg03l)
021600070214     c                   leave
021700070214     c                   endif
021800070706     c                   eval      dgrgflo = grgflo
021900070214     c                   if        datk >= grgdde and datk <= grgdscad
022000070706      * verifico tipo utilizzo giro
022100070706     c                             and (§grgtug = 'C' or §grgtug = 'E')
022200070214     c                   movel     grgdes        desgir
022300070214     c                   endif
022400070214     c                   enddo
022500070214
022600070215      *reperisco distinte con giro nella giornata in esame
022700070215     c                   clear                   dis
022800070215     c                   clear                   pta
022900070215     c                   z-add     0             cn
023000070215     c     kdst32        setll     fidst32l
023100070215     c                   do        *hival
023200070215     c     kdst32        reade     fidst32l
023300070215     c                   if        %eof(fidst32l)
023400070215     c                   leave
023500070215     c                   endif
023600070329      * scarta distinte annullate
023700070329     c                   if        dst3atb = 'A'
023800070329     c                   iter
023900070329     c                   endif
024000070215     c                   add       1             cn
024100070215     c                   if        cn > 5
024200070215     c                   leave
024300070215     c                   endif
024400070215     c                   z-add     dst3nfv       dis(cn)
024500070215     c                   z-add     dst3prgg      pta(cn)
024600070215     c                   enddo
024700070215
024800070214     c                   if        line > 60
024900070214     c                   except    testatab
025000070214     c                   endif
025100070214     c                   except    dettab
025200070214     c                   add       totpkl        ttotpkl
025300070214     c                   add       totvlu        ttotvlu
025400070214     c                   add       totspe        ttotspe
025500070214     c                   add       totcol        ttotcol
025600070214      * memorizza valori prima bolla del nuovo giro
025700070214     c                   move      argcgi        exargcgi
025800070302      * reperisce peso volume colli consegnabili
025900070302     c                   exsr      calpesvol
026000070302     c                   z-add     D27PKLDCO     totpkl
026100070302     c                   z-add     D27VLUDCO     totvlu
026200070302     c                   z-add     D27NCLDCO     totcol
026300070302     c                   z-add     1             totspe
026400070214     c                   endif
026500070214      *arb
026600070403     c*                  endif
026700070214      *arg
026800070214     c                   enddo
026900070214      * ultima riga
027000070403      *reperisco descrizione ultimo giro distinta
027100070403     c                   clear                   desgir
027200070403     c     kgrg2         setll     figrg03l
027300070403     c                   do        *hival
027400070403     c     kgrg2         reade     figrg03l
027500070403     c                   if        %eof(figrg03l)
027600070403     c                   leave
027700070403     c                   endif
027800070403     c                   if        datk >= grgdde and datk <= grgdscad
027900070403     c                   movel     grgdes        desgir
028000070403     c                   endif
028100070403     c                   enddo
028200070419
028300070419      *reperisco distinte con giro nella giornata in esame
028400070419     c                   clear                   dis
028500070419     c                   clear                   pta
028600070419     c                   z-add     0             cn
028700070419     c     kdst32        setll     fidst32l
028800070419     c                   do        *hival
028900070419     c     kdst32        reade     fidst32l
029000070419     c                   if        %eof(fidst32l)
029100070419     c                   leave
029200070419     c                   endif
029300070419      * scarta distinte annullate
029400070419     c                   if        dst3atb = 'A'
029500070419     c                   iter
029600070419     c                   endif
029700070419     c                   add       1             cn
029800070419     c                   if        cn > 5
029900070419     c                   leave
030000070419     c                   endif
030100070419     c                   z-add     dst3nfv       dis(cn)
030200070419     c                   z-add     dst3prgg      pta(cn)
030300070419     c                   enddo
030400070403
030500070214     c                   if        line > 60
030600070214     c                   except    testatab
030700070214     c                   endif
030800070214     c                   except    dettab
030900070214     c                   add       totpkl        ttotpkl
031000070214     c                   add       totvlu        ttotvlu
031100070214     c                   add       totspe        ttotspe
031200070214     c                   add       totcol        ttotcol
031300070214     c                   except    totali
031400070214
031500070214     c                   endsr
031600070214      *
031700070214      *-----------------------------------------------
031800070214     C     buona         begsr
031900070214      *-----------------------------------------------
032000070214     c                   clear                   fidg16ds
032100070214     c                   eval      d16aasi = arbaas
032200070214     c                   eval      d16lnpi = arblnp
032300070214     c                   eval      d16nrsi = arbnrs
032400070214     c                   eval      d16nspi = arbnsp
032500070214     c                   eval      d16dfvi = datk
032600070830     c                   eval      d16dtai = d26dtai
032700070830     c                   eval      d16orai = d26orai
032701150430     c                   eval      d16p88i = d26p88i
032800071210     c                   eval      D16LDCRI= D26dtdcr
032900070416      *se parallelo passa flag per testare solo il numero distinta da FIARG
033000070424     c                   if        §ogdda = '20391231' or
033100070424     c                             wdymd < ogddaN
033200070416     c                   eval      d16ndci = 'G'
033300070416     c                   endif
033400130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
033500130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
033600130221     c******             if        arbxco <> ' '
033700130221     c                   if        arbxco > '0'
033800110728     c                   eval      d16prfoI= 'S'
033900110728     c                   end
034000150112     c* se sto assegnando una spedizione presente nel giro DIR
034100150112     c* inibisco alcuni controlli di
034200150112     c* assegnabilità in quanto inutili visto che sto dirottando
034300150112     c* es. la sospensione alla consegna, la data consegna richiesta ecc.)
034400150112     c* questo lo faccio abilitando i singoli flag e non
034500150112     c* quello totale
034600150112     c                   if        argcgi = 'DIR'
034700150112     c                   eval      d16dcmi = 'S'
034800150112     c                   eval      d16fbci = 'S'
034900150112     c                   eval      d16cmci = 'S'
035000150112     c                   eval      d16lblmi= 'S'
035100150112     c                   eval      d16colci= 'S'
035200150112     c                   eval      d16parasgi='S'
035300150112     c                   else
035400150112     c                   eval      d16consi= 'S'
035500150112     c                   end
035600070416
035700070214     c                   eval      kpjbu = fidg16ds
035800070214     c                   call      'FIDG16R'
035900070214     c                   parm                    kpjba
036000070214     c                   parm                    fnarbds
036100070214     c                   parm                    fiargds
036200070214     c                   eval      fidg16ds = kpjbu
036300070214     C                   ENDSR
036400070214      *---------------------------------------------------------*
036500070214      *  Determinazione del peso da utilizzare
036600070214      *---------------------------------------------------------*
036700070302     C     calPESvol     BEGSR
036800070302     c                   clear                   fidg27ds
036900070723     c                   move      d26dat        d27dfvI
037000070723     c                   z-add     arbAAS        d27aasI
037100070723     c                   z-add     arbLNP        d27lnpI
037200070723     c                   z-add     arbNRS        d27nrsI
037300070723     c                   z-add     arbNSP        d27nspI
037400070830     c                   eval      d27dtai = d26dtai
037500070830     c                   eval      d27orai = d26orai
037600070302     c                   movel(p)  fidg27ds      kpjbu
037700070302     c                   call      'FIDG27R'
037800070302     c                   parm                    kpjba
037900070302     c                   parm                    fnarbds
038000070302     c
038100070302     c                   movel(p)  kpjbu         fidg27ds
038200070214      *
038300070214     C                   ENDSR
038400011026     c*-------------------------------------------------
038500011026     c     *inzsr        begsr
038600011026     c*-------------------------------------------------
038700020304     c     *entry        plist
038800020304     c                   parm                    kpjba
038900070213     c                   movel     kpjbu         fidg26ds
039000070329      * inizializzo parametri di stampa elenchi a 'S' se non impostati
039100070329     c                   if        d26spea = *blank
039200070329     c                   move      'S'           d26spea
039300070329     c                   endif
039400070329     c                   if        d26noas = *blank
039500070329     c                   move      'S'           d26noas
039600070329     c                   endif
039700070830     c                   if        d26dtai > 0
039800070830     c                   move      d26dtai       dataiso
039900070830     c                   move      dataiso       dataeur
040000070830     c                   move      dataeur       d26dtaig          8 0
040100070830     c                   endif
040101150514     c*
040102150515     c                   clear                   decp88i          21
040103150514     c                   if        d26p88i= 'N'
040107150515     c                   eval      decp88i='Spediz. pistola 88 NO'
040108150515     c                   else
040109150515     c                   clear                   decp88i
040110150514     c                   end
040200070329      *
040300071210     c                   if        d26dtdcr > 0
040400071210     c                   move      d26dtdcr      dataiso
040500071210     c                   move      dataiso       dataeur
040600071210     c                   move      dataeur       d26dtdcrg         8 0
040700071210     c                   endif
040800071210      *
040900020218     C                   time                    w0120            14 0
041000020218     C                   move      w0120         wdat              8 0
041100070222     C                   movel     w0120         wora              6 0
041200070403     c                   move      wdat          dataeur
041300070403     c                   move      dataeur       dataiso
041400070403     c                   move      dataiso       wdymd             8 0
041500070403
041600020404     c                   z-add     99            line
041700070213     c     kdst          klist
041800070213     c                   kfld                    filk              3 0
041900070213     c                   kfld                    catk              1 0
042000070213     c                   kfld                    datk              8 0
042100070213     c                   z-add     d26fil        filk
042200070213     c                   z-add     4             catk
042300070213     c                   z-add     d26dat        datk
042400070213
042500070214     c     kdst3         klist
042600070214     c                   kfld                    dstfgs
042700070214     c                   kfld                    dstnfv
042800070214     c                   kfld                    dstdfv
042900100208
043000100208     c     ktab          klist
043100100208     c                   kfld                    tblkut
043200100208     c                   kfld                    tblcod
043300100208     c                   kfld                    tblkey
043400100208     c                   z-add     1             tblkut
043500070214
043600070215     c     kdst32        klist
043700070215     c                   kfld                    filk
043800070215     c                   kfld                    datk
043900070223     c                   kfld                    filk
044000070215     c                   kfld                    exargcgi
044100070215
044200070214     c     kgrg          klist
044300070214     c                   kfld                    dst3fgs
044400070214     c                   kfld                    dst3cgi
044500070214
044600070214     c     kgrg2         klist
044700070214     c                   kfld                    filk
044800070214     c                   kfld                    exargcgi
044900070214
045000070213     c     kapd          klist
045100070213     c                   kfld                    tipk              1
045200070213     c                   kfld                    dstpdr
045300070213     c                   movel     'A'           tipk
045400030725
045500070214     c     karb          klist
045600070214     c                   kfld                    argaas
045700070214     c                   kfld                    arglnp
045800070214     c                   kfld                    argnrs
045900070214     c                   kfld                    argnsp
046000070214
046100030505     C                   Z-ADD     1             CODUT
046200030505     C                   CALL      'X§PARUT'
046300030505     C                   PARM                    UT§DSE
046400030505     C                   MOVEL     RAGUT         RSUT             20
046500030505     C                   MOVEL     REC80         CNCR80
046600070213     c                   move      datk          dataiso
046700070213     c                   move      dataiso       dataeur
046800070222     c                   move      *all'_'       separa          192
046900070213      * decodifica p.o.
047000070403     c                   clear                   desfil
047100070403     c                   clear                   ogddaN            8 0
047200070403     c     filk          chain     azorg01l
047300070403     c                   if        %found(azorg01l)
047400070403     c                   movel     orgde6        og146
047401160421     c                   movel     orgdf0        og150
047402160421     c     §OGPKM        comp      'S'                                    70
047404160421     c     §OGturno      comp      'S'                                    72
047405160421     c                   movel     d26pame       d26pamed          5
047406160421     c                   if        *in70 and d26pame = ' '
047407160421     c                   eval      d26pamed = 'Tutte'
047408160421     c                   end
047500070403     c                   if        §ogdda <> ' '
047600070403     c                   move      §ogdda        ogddaN
047700070403     c                   endif
047800070403     c                   movel     orgdes        desfil           20
047900070403     c                   endif
048000070403
048100011026     c                   endsr
048200020404     C*--------------------------------------------------------------------
048300070214     Oprtf198   E            testata        1 02
048400070222     o                       rsut                21
048500070222     o                       knmus               33
048600070327     o                                          100 'Situazione Assegnazione'
048700070215     o                                        +   2 'Distinte del'
048800070215     o                       dataeur          +   2
048900070510     o                                          150 'FIDG26R'
049000070510     o                       wdat          y    170
049100070510     o                       wora               181 '  :  :  '
049200070222     o                                          190 'Pag.'
049300070222     O                       page          z    196
049400070223     O          E            testata     1
049500070223     o                                           13 'P.O Gestione'
049600160421     o                       filk             +   1
049700160421     o                       desfil           +   1
049800160421     o                                        +   2 'Data/ora limite'
049900070830     o                                        +   1 'abbinamento'
050000160421     o                       d26dtaig      y  +   1
050100160421     o                       d26orai          +   1 '0 :  '
050200160421     o                                        +   2 'Limite Data cons.rich.'
050300071213     o                                        +   0 'Prima del'
050400160421     o                       d26dtdcrg     y  +   1
050403160421     o                       DecP88I          +   1
050404160421     o               70                       +   2 'Parcel/Messagerie'
050405160421     o               70      d26pamed         +   1
050406160421     o               72                       +   2 'Turno'
050407160421     o               72      d26tur1          +   1
050408160421     o               72      d26tur2          +   1
050500070223
050600070222     O          E            testata     1
050700070222     o                       separa             192
050800070223     O          E            testata     1
050900070223     o                                           73 '|'
051000070330     o                                           84 'TOLLERANZA'
051100070330     o                                          100 'SPEDIZIONI'
051200070223     o                                          102 '|'
051300070223     o                                          120 'FERMATE'
051400070223     o                                          132 '|'
051500070223     o                                          150 'VOLUME'
051600070223     o                                          162 '|'
051700070223     o                                          179 'PESO '
051800070223     o                                          192 '|'
051900070223     O          E            testata     1
052000070213     o                                           10 'Distinta'
052100070213     o                                        +   2 'Autotrasportatore'
052200070213     o                                           41 'M/P'
052300070228     o                                           60 'Descrizione 1°giro'
052400070222     o                                           73 '|'
052500070330     o*                                          80 'Nr. Max'
052600070330     o                                           80 ' %  Mag'
052700070330     o                                          100 'Assegnate'
052800070330     o*                                         101 'Differ.'
052900070222     o                                          102 '|'
053000070223     o                                          110 'Nr. Max '
053100070223     o                                          121 'Assegnate'
053200070223     o                                          131 'Differ.'
053300070222     o                                          132 '|'
053400070223     o                                          140 'Mc. Max '
053500070223     o                                          151 'Assegnati'
053600070223     o                                          161 'Differ.'
053700070222     o                                          162 '|'
053800070223     o                                          171 'Kg. Max '
053900070223     o                                          181 'Assegnati'
054000070223     o                                          191 'Differ.'
054100070222     o                                          192 '|'
054200070222     O          E            testata     1
054300070222     o                       separa             192
054400070222     O          E            detta       1
054500070222     o                                           73 '|'
054600070222     o                                          102 '|'
054700070222     o                                          132 '|'
054800070222     o                                          162 '|'
054900070222     o                                          192 '|'
055000070222     O          E            detta       1
055100070213     o                       dstnfv        z      9
055200070213     o                       despdr              37
055300070213     o                       dstfpp              40
055400070214     o                       desgir              72
055500070222     o                                           73 '|'
055600070330     o*                      dstnbl        2     81
055700070330     o                       dsttol        2     81
055800070330     o                       dstnbla       2    100
055900070330     o*                      diffnbl       k    101
056000070222     o                                          102 '|'
056100070221     o                       dstnftl       z    111
056200070221     o                       dststpa       z    121
056300070221     o                       diffnftl      k    131
056400070222     o                                          132 '|'
056500070221     o                       dstvlu        2    141
056600070221     o                       dstvlua       2    151
056700070221     o                       diffvlu       k    161
056800070222     o                                          162 '|'
056900070221     o                       dstpkl        2    171
057000070221     o                       dstpkla       2    181
057100070221     o                       diffpkl       k    191
057200070222     o                                          192 '|'
057300070215     O          E            detta       0
057400070330     o*                      diffnbl       k    101
057500070330     o               32      diffnftl      k    131
057600070330     o               31      diffvlu       k    161
057700070330     o               30      diffpkl       k    191
057800070215     O          E            detta       0
057900070330     o*                      diffnbl       k    101
058000070330     o               32      diffnftl      k    131
058100070330     o               31      diffvlu       k    161
058200070330     o               30      diffpkl       k    191
058300070510     O          E            totalia     1
058400070510     o                                           73 '|'
058500070510     o                                          102 '|'
058600070510     o                                          132 '|'
058700070510     o                                          162 '|'
058800070510     o                                          192 '|'
058900070510     O          E            totalia     1
059000070510     o                                           30 'Totale spedizioni -
059100070510     o                                              assegnate '
059200070510     o                                           73 '|'
059300070510     o                       tdstnbla      2    100
059400070510     o                                          102 '|'
059500070510     o                       tdststpa      z    121
059600070510     o                                          132 '|'
059700070510     o                       tdstvlua      2    151
059800070510     o                                          162 '|'
059900070510     o                       tdstpkla      2    181
060000070510     o                                          192 '|'
060100070510     O          E            totalia     0
060200070510     o                                           30 'Totale spedizioni -
060300070510     o                                              assegnate '
060400070510     o                                           73 '|'
060500070510     o                       tdstnbla      2    100
060600070510     o                                          102 '|'
060700070510     o                       tdststpa      z    121
060800070510     o                                          132 '|'
060900070510     o                       tdstvlua      2    151
061000070510     o                                          162 '|'
061100070510     o                       tdstpkla      2    181
061200070510     o                                          192 '|'
061300070510     O          E            totalia     0
061400070510     o                                           30 'Totale spedizioni -
061500070510     o                                              assegnate '
061600070510     o                                           73 '|'
061700070510     o                       tdstnbla      2    100
061800070510     o                                          102 '|'
061900070510     o                       tdststpa      z    121
062000070510     o                                          132 '|'
062100070510     o                       tdstvlua      2    151
062200070510     o                                          162 '|'
062300070510     o                       tdstpkla      2    181
062400070510     o                                          192 '|'
062500070214      *__totali Giri non asssegnati______________________________
062600070214     O          E            testatab       1 02
062700070223     o                       rsut                21
062800070215     o                       knmus            +   2
062900070223     o                                          110 'Spedizioni da consegnare n-
063000070223     o                                              on assegnate alle '
063100070215     o                                        +   2 'Distinte del'
063200070215     o                       dataeur          +   2
063300070510     o                                          150 'FIDG26R'
063400070510     o                       wdat          y    170
063500070510     o                       wora               181 '  :  :  '
063600070222     o                                          190 'Pag.'
063700070222     O                       page          z    196
063800070223     O          E            testatab    1
063900070223     o                                           13 'P.O Gestione'
064000160421     o                       filk             +   1
064100160421     o                       desfil           +   1
064200160421     o                                        +   2 'Data/ora limite'
064300070830     o                                        +   1 'abbinamento'
064400160421     o                       d26dtaig      y  +   1
064500160421     o                       d26orai          +   1 '0 :  '
064600160421     o                                        +   2 'Limite Data cons.rich.'
064700071213     o                                        +   0 'Prima del'
064800160421     o                       d26dtdcrg     y  +   1
064802160421     o                       DecP88I          +   1
064803160421     o               70                       +   2 'Parcel/Messagerie'
064804160422     o               70      d26pamed         +   1
064805160421     o               72                       +   2 'Turno'
064806160421     o               72      d26tur1          +   1
064807160421     o               72      d26tur2          +   1
064900070223
065000070222     O          E            testatab    1
065100070222     o                       separa             192
065200070214     O          E            testatab    2
065300070214     o                                            5 'Giro'
065400070214     o                                           23 'Descrizione'
065500070222     o                                           53 'Tot Sped'
065600070222     o                                           63 'Tot Colli'
065700070222     o                                           73 'Tot Vol.'
065800070222     o                                           83 'Tot Peso'
065900070222     o                                        +  30 'Distinte in cui il giro -
066000070215     o                                              e'' stato pianificato'
066100070215     O          E            testatab    1
066200070222     o                                           85 '|'
066300070215     o                                           93 'Numero'
066400070215     o                                        +   1 'Ptà'
066500070222     o                                        +   1 '|'
066600070215     o                                        +   3 'Numero'
066700070215     o                                        +   1 'Ptà'
066800070222     o                                        +   1 '|'
066900070215     o                                        +   3 'Numero'
067000070215     o                                        +   1 'Ptà'
067100070222     o                                        +   1 '|'
067200070215     o                                        +   3 'Numero'
067300070215     o                                        +   1 'Ptà'
067400070222     o                                        +   1 '|'
067500070215     o                                        +   3 'Numero'
067600070215     o                                        +   1 'Ptà'
067700070222     o                                        +   1 '|'
067800070222     O          E            testatab    1
067900070222     o                       separa             192
068000070222     O          E            dettab      1
068100070222     o                                           85 '|'
068200070222     o                                        +  13 '|'
068300070222     o                                        +  14 '|'
068400070222     o                                        +  14 '|'
068500070222     o                                        +  14 '|'
068600070222     o                                        +  14 '|'
068700070222     O          E            dettab      1
068800070214     o                       exargcgi            11
068900070214     o                       desgir              42
069000070222     o                       totspe        2     53
069100070222     o                       totcol        2     63
069200070222     o                       totvlu        2     73
069300070222     o                       totpkl        2     83
069400070222     o                                           85 '|'
069500070222     o                       dis(1)        z  +   2
069600070215     o                       pta(1)        z  +   2
069700070222     o                                        +   1 '|'
069800070215     o                       dis(2)        z  +   3
069900070215     o                       pta(2)        z  +   2
070000070222     o                                        +   1 '|'
070100070215     o                       dis(3)        z  +   3
070200070215     o                       pta(3)        z  +   2
070300070222     o                                        +   1 '|'
070400070215     o                       dis(4)        z  +   3
070500070215     o                       pta(4)        z  +   2
070600070222     o                                        +   1 '|'
070700070215     o                       dis(5)        z  +   3
070800070215     o                       pta(5)        z  +   2
070900070222     o                                        +   1 '|'
071000070214     O          E            totali      2
071100070214     o                                           33 'Totale spedizioni non -
071200070214     o                                              assegnate '
071300070222     o                       ttotspe       2     53
071400070222     o                       ttotcol       2     63
071500070222     o                       ttotvlu       2     73
071600070222     o                       ttotpkl       2     83
