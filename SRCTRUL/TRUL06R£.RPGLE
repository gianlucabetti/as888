000100020412     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020412
000300020412      * TRUL06R  *---------------------------------------------------*
000400020412      * - CARICAMENTO P.O. IN SCHIERA NUMERICA
000500020412      *--------------------------------------------------------------*
000600020412
000700020412     fazcae05l  if   e           k disk
000800020412     fazcae01l  if   e           k disk    rename(azcae000:azcae01)
000900020412
001000020412     d kcaeepa         s                   like(caeepa)
001100020412     d kcaetfe         s                   like(caetfe)
001200020412     d kcaetfp         s                   like(caetfp)
001300020412     d wdata           s                   like(d06drf)
001400020412
001500020412     d wok             s              1
001600020412     d xxx             s              2  0
001700020509     d yyy             s              2  0
001800020509
001900020509     d skpog           s              3  0 dim(30)
002000051130     d sk£6            s              3  0 dim(30)
002100020412
002200020412     d trul06ds      e ds
002300020412     d  lin                    1     90  0 dim(30)
002400020412     d  d06fdt               100    100
002500020412     d kpjba         e ds
002600090114
002700090114     d datasys         s               d   inz(*sys) datfmt(*iso)
002800090114
002900020412      *---------------------------------------------------------------
003000020412     c     *entry        plist
003100020412     c                   parm                    kpjba
003200020412     c                   movel     kpjbu         trul06ds
003300020412
003400020412     c     kcaet         klist
003500020412     c                   kfld                    kcaeepa
003600020412     c                   kfld                    kcaetfe
003700020412
003800020412     c     kcaep         klist
003900020412     c                   kfld                    kcaeepa
004000020412     c                   kfld                    kcaetfp
004100020412
004200020412      * d06tla = ' '  Elaboro e chiudo con RETURN
004300020412      * d06tla = 'L'  Elaboro e chiudo con LR
004400020412      * d06tla = 'C'            chiudo con LR  solo per chiusura file
004500020412
004600020412    1c                   if        d06tla = 'C'
004700020412     c                   eval      *inlr = *on
004800020412
004900020412   X1c                   else
005000020509      * se richiamato con £1 o PP e d06esc = 'G' errore subito
005100020509     c                   if        (d06cod = '£1' or d06cod = 'PP') and
005200020509     c                              d06esc = 'G'
005300020509     c                   eval      d06err = '1'
005400020509     c                   clear                   lin
005500020509     c                   goto      noelab
005600020509     c                   endif
005700020412
005800020412      * se non passata data dal chiamante prendo udate per i controlli della data
005900020412     c                   if        d06fdt = 'S'
006000020412     c                   eval      wdata = d06drf
006100020412     c                   else
006200090114     c* da TIME prendo la data
006300090114     c*******            eval      wdata = *date
006400090114     c                   eval      wdata=%dec(datasys)
006500020412     c                   endif
006600020412
006700020412     c                   select
006800020412
006900020412      * Richiamato con cod = '£1' - tutti i p.o. del terminal di partenza
007000020412     c                   when      d06cod = '£1'
007100020412     c                   exsr      sr_car£1
007200020412      * Richiamato con cod = '£6' - tutti i p.o. in gestione
007300020412     c                   when      d06cod = '£6'
007400020412     c                   exsr      sr_car£6
007500020412      * Richiamato con cod = 'PP' - tutti i p.o. che fanno parte dello stesso terminal di partenza
007600020412     c                   when      d06cod = 'PP'
007700020412     c                   exsr      sr_carpp
007800051130      * Richiamato con cod = '£X' - tutti i p.o. del terminal  partenzastesso terminal di partenza
007900051130      *                            e p.o. gestiti non del term.partenza
008000051130     c                   when      d06cod = '£X'
008100051130     c                   exsr      sr_car£X
008200020412
008300020412     c                   endsl
008400020509
008500020509     c     noelab        tag
008600020412
008700020412     c                   movel     trul06ds      kpjbu
008800020412
008900020412    2c                   if        d06tla = 'L'
009000020412     c                   eval      *inlr = *on
009100020412   X2c                   else
009200020412     c                   return
009300020412    2c                   endif
009400020412
009500020412     c                   endif
009600020412
009700020412      *---------------------------------------------------------------*
009800020412      * Sr_car£1  - carico tutti i p.o. del terminal di partenza
009900020412      *---------------------------------------------------------------*
010000020412     c     sr_car£1      begsr
010100020412
010200020412     c                   clear                   lin
010300020412     c                   clear                   xxx
010400020412     c                   clear                   wok
010500020809     c                   clear                   d06err
010600020412
010700020412     c                   eval      kcaeepa = 'P'
010800020412     c                   movel     d06key        kcaetfe
010900020412     c     kcaet         setll     azcae05l
011000020412     c                   do        *hival
011100020412     c     kcaet         reade     azcae05l
011200020412      * fine file
011300020412     c                   if        %eof(azcae05l)
011400020412     c                   leave
011500020412     c                   endif
011600020412      * record annullato
011700020412     c                   if        caeatb <> *blanks
011800020412     c                   iter
011900020412     c                   endif
012000020412      * controllo la data validità
012100020412     c                   if        caedde > wdata or caedsc < wdata
012200020412     c                   iter
012300020412     c                   endif
012400020530     c                   eval      wok = 'S'
012500020412      * se non escluso al primo posto metto il terminal di partenza
012600020509     c                   if        d06esc <> 'S' and xxx = 0
012700020412     c                   eval      xxx = 1
012800020412     c                   movel     d06key        lin(xxx)
012900020412     c                   endif
013000020412      * carico la schiera
013100020412     c                   if        caetfp <> caetfe
013200020412     c                   add       1             xxx
013300020412     c                   movel     caetfp        lin(xxx)
013400020412     c                   endif
013500020412
013600020412     c                   enddo
013700020412
013800020530      * se non ho trovato nessun terminal di partenza Errore
013900020412     c                   if        wok <> 'S'
014000020509     c                   if        d06esc <> 'S'
014100020419     c                   eval      xxx = 1
014200020419     c                   movel     d06key        lin(xxx)
014300020419     c                   endif
014400020412     c                   eval      d06err = '1'
014500020412     c                   endif
014600020412
014700020412     c                   endsr
014800020412      *---------------------------------------------------------------*
014900020412      * Sr_car£6  - carico tutti i p.o. in gestione
015000020412      *---------------------------------------------------------------*
015100020412     c     sr_car£6      begsr
015200020412
015300020412     c                   clear                   lin
015400020412     c                   clear                   xxx
015500020412     c                   clear                   wok
015600020509     c                   clear                   d06err
015700020412
015800020509      * carico i p.o. gestiti dal p.o. passato in d06key
015900020509     c                   if        d06esc <> 'G'
016000020412     c                   eval      kcaeepa = '6'
016100020412     c                   movel     d06key        kcaetfe
016200020412     c     kcaet         setll     azcae05l
016300020412     c                   do        *hival
016400020412     c     kcaet         reade     azcae05l
016500020412      * fine file
016600020412     c                   if        %eof(azcae05l)
016700020412     c                   leave
016800020412     c                   endif
016900020412      * record annullato
017000020412     c                   if        caeatb <> *blanks
017100020412     c                   iter
017200020412     c                   endif
017300020412      * controllo la data validità
017400020412     c                   if        caedde > wdata or caedsc < wdata
017500020412     c                   iter
017600020412     c                   endif
017700020530     c                   eval      wok = 'S'
017800020412      * se non escluso al primo posto metto il p.o. in gestione
017900020509     c                   if        d06esc = *blanks and xxx = 0
018000020412     c                   eval      xxx = 1
018100020412     c                   movel     d06key        lin(xxx)
018200020412     c                   endif
018300020412      * carico la schiera
018400020418     c                   if        caetfa <> caetfe
018500020412     c                   add       1             xxx
018600020418     c                   movel     caetfa        lin(xxx)
018700020412     c                   endif
018800020412
018900020412     c                   enddo
019000020509
019100020509      * carico tutti i p.o. gestiti nell'area del p.o. passato in d06key
019200020509     c                   else
019300020509      * per prima cosa devo caricare tutti i p.o. dell'area (£1)
019400020509     c                   exsr      sr_car£1
019500020509      * ok trovati i p.o. dell'area
019600020509     c                   if        d06err = *blanks
019700020509     c                   movea     lin           skpog
019800020509     c                   clear                   lin
019900020509     c                   clear                   xxx
020000020509     c                   clear                   yyy
020100020509     c                   clear                   wok
020200020509     c                   eval      kcaeepa = '6'
020300020509      * per ogni p.o. dell'area carico i p.o. gestiti
020400020509     c                   do        30            xxx
020500020509     c                   if        skpog(xxx) = *zeros
020600020509     c                   leave
020700020509     c                   endif
020800020509     c                   movel     skpog(xxx)    kcaetfe
020900020509     c     kcaet         setll     azcae05l
021000020509     c                   do        *hival
021100020509     c     kcaet         reade     azcae05l
021200020509      * fine file
021300020509     c                   if        %eof(azcae05l)
021400020509     c                   leave
021500020509     c                   endif
021600020509      * record annullato
021700020509     c                   if        caeatb <> *blanks
021800020509     c                   iter
021900020509     c                   endif
022000020509      * controllo la data validità
022100020509     c                   if        caedde > wdata or caedsc < wdata
022200020509     c                   iter
022300020509     c                   endif
022400020530     c                   eval      wok = 'S'
022500020509      * se non escluso al primo posto metto il p.o. in gestione
022600020509     c                   if        d06esc = *blanks and yyy = 0
022700020509     c                   eval      yyy = 1
022800020509     c                   movel     d06key        lin(yyy)
022900020509     c                   endif
023000020509      * carico la schiera
023100020509     c                   if        caetfa <> caetfe
023200020509     c                   add       1             yyy
023300020509     c                   movel     caetfa        lin(yyy)
023400020509     c                   endif
023500020509     c                   enddo
023600020509
023700020509 e2  c                   enddo
023800020509      * errore nel caricare la £1
023900020509     c                   else
024000020509     c                   clear                   lin
024100020509     c                   endif
024200020509     c                   endif
024300020412
024400020530      * non è stato trovato nessun p.o. in gestione
024500020509     c                   if        wok <> 'S' and d06esc = *blanks
024600020412     c                   eval      xxx = 1
024700020412     c                   movel     d06key        lin(xxx)
024800020412     c                   endif
024900020412
025000020412
025100020412     c                   endsr
025200020412      *---------------------------------------------------------------*
025300020412      * Sr_carpp  - carico tutti i p.o. che fanno parte dello stesso terminal
025400020412      *---------------------------------------------------------------*
025500020412     c     sr_carpp      begsr
025600020412
025700020412     c                   clear                   lin
025800020412     c                   clear                   xxx
025900020809     c                   clear                   d06err
026000020412
026100020412     c                   eval      kcaeepa = 'P'
026200020412     c                   movel     d06key        kcaetfp
026300020412
026400020412      * cerco il terminal di partenza del p.o. passato
026500020412     c     kcaep         setll     azcae01l
026600020412     c                   do        *hival
026700020412     c     kcaep         reade     azcae01l
026800020412      * fine file
026900020412     c                   if        %eof(azcae01l)
027000020412     c                   leave
027100020412     c                   endif
027200020412      * record annullato
027300020412     c                   if        caeatb <> *blanks
027400020412     c                   iter
027500020412     c                   endif
027600020412      * controllo la data validità
027700020412     c                   if        caedde > wdata or caedsc < wdata
027800020412     c                   iter
027900020412     c                   endif
028000020412      * terminal di partenza
028100020412     c                   movel     caetfe        kcaetfe
028200020412     c                   enddo
028300020412
028400020412      * se non è stato trovato il terminal del p.o. passato Errore
028500020412     c                   if        kcaetfe = *zeros
028600020412     c                   eval      d06err = 'P'
028700020412
028800020412      * trovato il terminal di partenza
028900020412     c                   else
029000020412
029100020412      * cerco tutti i p.o. di quel terminal di partenza
029200020412     c     kcaet         setll     azcae05l
029300020412     c                   do        *hival
029400020412     c     kcaet         reade     azcae05l
029500020412      * fine file
029600020412     c                   if        %eof(azcae05l)
029700020412     c                   leave
029800020412     c                   endif
029900020412      * record annullato
030000020412     c                   if        caeatb <> *blanks
030100020412     c                   iter
030200020412     c                   endif
030300020412      * controllo la data validità
030400020412     c                   if        caedde > wdata or caedsc < wdata
030500020412     c                   iter
030600020412     c                   endif
030700020507      * al primo posto metto il terminal di partenza
030800020507     c                   if        xxx = 0
030900020507      * se non è escluso
031000020509     c                   if        d06esc <> 'S'
031100020507     c                   eval      xxx = 1
031200020507     c                   movel     caetfe        lin(xxx)
031300020507     c                   else
031400020507      * se è escluso controllo se il p.o. passato in d06key è il terminal di partenza
031500020507     c                   if        caetfe <> kcaetfp
031600020412     c                   eval      xxx = 1
031700020412     c                   movel     caetfe        lin(xxx)
031800020507     c                   endif
031900020507     c                   endif
032000020412     c                   endif
032100020412      * se escluso non carico il p.o. passato in d06key
032200020412     c                   if        d06esc = 'S' and caetfp = kcaetfp
032300020412     c                   iter
032400020412     c                   endif
032500020412      * carico la schiera
032600020412     c                   if        caetfp <> caetfe
032700020412     c                   add       1             xxx
032800020412     c                   movel     caetfp        lin(xxx)
032900020412     c                   endif
033000020412
033100020412     c                   enddo
033200020412
033300020412     c                   endif
033400020412
033500020412     c                   endsr
033600051130      *---------------------------------------------------------------*
033700051130      * Sr_car£X  - carico tutti i p.o. delle £1 + i p.o. gestiti
033800051130      *             in £6 non in £1
033900051130      *---------------------------------------------------------------*
034000051130     c     sr_car£X      begsr
034100051130     c                   eval      d06esc='G'
034200051130     c                   exsr      sr_car£6
034300051130     c                   clear                   d06esc
034400051130     c                   movea     lin           sk£6
034500051130     c                   clear                   lin
034600051130     c* Leggo schiera lin e aggiungo a skpog
034700051130     c                   z-add     1             xxx
034800051130     c                   dow       sk£6(xxx)>0
034900051130     c     sk£6(xxx)     lookup    skpog                                  30
035000051130     c                   if        not *in30
035100051130     c                   z-add     1             yyy
035200051130     c     000           lookup    skpog(YYY)                             31
035300051130     c                   if        *in31
035400051130     c                   movel     sk£6(xxx)     skpog(yyy)
035500051130     c                   endif
035600051130     c                   endif
035700051130     c                   add       1             xxx
035800051130     c                   enddo
035900051130     c
036000051130     c                   movea     skpog         lin
036100051130     c                   endsr
