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
007800140117      * Richiamato con cod = '£X' - tutti i p.o. del terminal (£1) + le fil in £6 non presenti in £1
007900140117      *                             di tutti i p.o. della £1
008000051130     c                   when      d06cod = '£X'
008100051130     c                   exsr      sr_car£X
008200140117      * Richiamato con cod = '£J' - tutti i p.o. in gestione contabile
008300140117     c                   when      d06cod = '£J'
008400140117     c                   exsr      sr_car£J
008500020412
008600020412     c                   endsl
008700020509
008800020509     c     noelab        tag
008900020412
009000020412     c                   movel     trul06ds      kpjbu
009100020412
009200020412    2c                   if        d06tla = 'L'
009300020412     c                   eval      *inlr = *on
009400020412   X2c                   else
009500020412     c                   return
009600020412    2c                   endif
009700020412
009800020412     c                   endif
009900020412
010000020412      *---------------------------------------------------------------*
010100020412      * Sr_car£1  - carico tutti i p.o. del terminal di partenza
010200020412      *---------------------------------------------------------------*
010300020412     c     sr_car£1      begsr
010400020412
010500020412     c                   clear                   lin
010600020412     c                   clear                   xxx
010700020412     c                   clear                   wok
010800020809     c                   clear                   d06err
010900020412
011000020412     c                   eval      kcaeepa = 'P'
011100020412     c                   movel     d06key        kcaetfe
011200020412     c     kcaet         setll     azcae05l
011300020412     c                   do        *hival
011400020412     c     kcaet         reade     azcae05l
011500020412      * fine file
011600020412     c                   if        %eof(azcae05l)
011700020412     c                   leave
011800020412     c                   endif
011900020412      * record annullato
012000020412     c                   if        caeatb <> *blanks
012100020412     c                   iter
012200020412     c                   endif
012300020412      * controllo la data validità
012400020412     c                   if        caedde > wdata or caedsc < wdata
012500020412     c                   iter
012600020412     c                   endif
012700020530     c                   eval      wok = 'S'
012800020412      * se non escluso al primo posto metto il terminal di partenza
012900020509     c                   if        d06esc <> 'S' and xxx = 0
013000020412     c                   eval      xxx = 1
013100020412     c                   movel     d06key        lin(xxx)
013200020412     c                   endif
013300020412      * carico la schiera
013400020412     c                   if        caetfp <> caetfe
013500020412     c                   add       1             xxx
013600020412     c                   movel     caetfp        lin(xxx)
013700020412     c                   endif
013800020412
013900020412     c                   enddo
014000020412
014100020530      * se non ho trovato nessun terminal di partenza Errore
014200020412     c                   if        wok <> 'S'
014300020509     c                   if        d06esc <> 'S'
014400020419     c                   eval      xxx = 1
014500020419     c                   movel     d06key        lin(xxx)
014600020419     c                   endif
014700020412     c                   eval      d06err = '1'
014800020412     c                   endif
014900020412
015000020412     c                   endsr
015100020412      *---------------------------------------------------------------*
015200020412      * Sr_car£6  - carico tutti i p.o. in gestione
015300020412      *---------------------------------------------------------------*
015400020412     c     sr_car£6      begsr
015500020412
015600020412     c                   clear                   lin
015700020412     c                   clear                   xxx
015800020412     c                   clear                   wok
015900020509     c                   clear                   d06err
016000020412
016100020509      * carico i p.o. gestiti dal p.o. passato in d06key
016200020509     c                   if        d06esc <> 'G'
016300020412     c                   eval      kcaeepa = '6'
016400020412     c                   movel     d06key        kcaetfe
016500020412     c     kcaet         setll     azcae05l
016600020412     c                   do        *hival
016700020412     c     kcaet         reade     azcae05l
016800020412      * fine file
016900020412     c                   if        %eof(azcae05l)
017000020412     c                   leave
017100020412     c                   endif
017200020412      * record annullato
017300020412     c                   if        caeatb <> *blanks
017400020412     c                   iter
017500020412     c                   endif
017600020412      * controllo la data validità
017700020412     c                   if        caedde > wdata or caedsc < wdata
017800020412     c                   iter
017900020412     c                   endif
018000020530     c                   eval      wok = 'S'
018100020412      * se non escluso al primo posto metto il p.o. in gestione
018200020509     c                   if        d06esc = *blanks and xxx = 0
018300020412     c                   eval      xxx = 1
018400020412     c                   movel     d06key        lin(xxx)
018500020412     c                   endif
018600020412      * carico la schiera
018700020418     c                   if        caetfa <> caetfe
018800020412     c                   add       1             xxx
018900020418     c                   movel     caetfa        lin(xxx)
019000020412     c                   endif
019100020412
019200020412     c                   enddo
019300020509
019400020509      * carico tutti i p.o. gestiti nell'area del p.o. passato in d06key
019500020509     c                   else
019600020509      * per prima cosa devo caricare tutti i p.o. dell'area (£1)
019700020509     c                   exsr      sr_car£1
019800020509      * ok trovati i p.o. dell'area
019900020509     c                   if        d06err = *blanks
020000020509     c                   movea     lin           skpog
020100020509     c                   clear                   lin
020200020509     c                   clear                   xxx
020300020509     c                   clear                   yyy
020400020509     c                   clear                   wok
020500020509     c                   eval      kcaeepa = '6'
020600020509      * per ogni p.o. dell'area carico i p.o. gestiti
020700020509     c                   do        30            xxx
020800020509     c                   if        skpog(xxx) = *zeros
020900020509     c                   leave
021000020509     c                   endif
021100020509     c                   movel     skpog(xxx)    kcaetfe
021200020509     c     kcaet         setll     azcae05l
021300020509     c                   do        *hival
021400020509     c     kcaet         reade     azcae05l
021500020509      * fine file
021600020509     c                   if        %eof(azcae05l)
021700020509     c                   leave
021800020509     c                   endif
021900020509      * record annullato
022000020509     c                   if        caeatb <> *blanks
022100020509     c                   iter
022200020509     c                   endif
022300020509      * controllo la data validità
022400020509     c                   if        caedde > wdata or caedsc < wdata
022500020509     c                   iter
022600020509     c                   endif
022700020530     c                   eval      wok = 'S'
022800020509      * se non escluso al primo posto metto il p.o. in gestione
022900020509     c                   if        d06esc = *blanks and yyy = 0
023000020509     c                   eval      yyy = 1
023100020509     c                   movel     d06key        lin(yyy)
023200020509     c                   endif
023300020509      * carico la schiera
023400020509     c                   if        caetfa <> caetfe
023500020509     c                   add       1             yyy
023600020509     c                   movel     caetfa        lin(yyy)
023700020509     c                   endif
023800020509     c                   enddo
023900020509
024000020509 e2  c                   enddo
024100020509      * errore nel caricare la £1
024200020509     c                   else
024300020509     c                   clear                   lin
024400020509     c                   endif
024500020509     c                   endif
024600020412
024700020530      * non è stato trovato nessun p.o. in gestione
024800020509     c                   if        wok <> 'S' and d06esc = *blanks
024900020412     c                   eval      xxx = 1
025000020412     c                   movel     d06key        lin(xxx)
025100020412     c                   endif
025200020412
025300020412
025400020412     c                   endsr
025500020412      *---------------------------------------------------------------*
025600020412      * Sr_carpp  - carico tutti i p.o. che fanno parte dello stesso terminal
025700020412      *---------------------------------------------------------------*
025800020412     c     sr_carpp      begsr
025900020412
026000020412     c                   clear                   lin
026100020412     c                   clear                   xxx
026200020809     c                   clear                   d06err
026300020412
026400020412     c                   eval      kcaeepa = 'P'
026500020412     c                   movel     d06key        kcaetfp
026600020412
026700020412      * cerco il terminal di partenza del p.o. passato
026800020412     c     kcaep         setll     azcae01l
026900020412     c                   do        *hival
027000020412     c     kcaep         reade     azcae01l
027100020412      * fine file
027200020412     c                   if        %eof(azcae01l)
027300020412     c                   leave
027400020412     c                   endif
027500020412      * record annullato
027600020412     c                   if        caeatb <> *blanks
027700020412     c                   iter
027800020412     c                   endif
027900020412      * controllo la data validità
028000020412     c                   if        caedde > wdata or caedsc < wdata
028100020412     c                   iter
028200020412     c                   endif
028300020412      * terminal di partenza
028400020412     c                   movel     caetfe        kcaetfe
028500020412     c                   enddo
028600020412
028700020412      * se non è stato trovato il terminal del p.o. passato Errore
028800020412     c                   if        kcaetfe = *zeros
028900020412     c                   eval      d06err = 'P'
029000020412
029100020412      * trovato il terminal di partenza
029200020412     c                   else
029300020412
029400020412      * cerco tutti i p.o. di quel terminal di partenza
029500020412     c     kcaet         setll     azcae05l
029600020412     c                   do        *hival
029700020412     c     kcaet         reade     azcae05l
029800020412      * fine file
029900020412     c                   if        %eof(azcae05l)
030000020412     c                   leave
030100020412     c                   endif
030200020412      * record annullato
030300020412     c                   if        caeatb <> *blanks
030400020412     c                   iter
030500020412     c                   endif
030600020412      * controllo la data validità
030700020412     c                   if        caedde > wdata or caedsc < wdata
030800020412     c                   iter
030900020412     c                   endif
031000020507      * al primo posto metto il terminal di partenza
031100020507     c                   if        xxx = 0
031200020507      * se non è escluso
031300020509     c                   if        d06esc <> 'S'
031400020507     c                   eval      xxx = 1
031500020507     c                   movel     caetfe        lin(xxx)
031600020507     c                   else
031700020507      * se è escluso controllo se il p.o. passato in d06key è il terminal di partenza
031800020507     c                   if        caetfe <> kcaetfp
031900020412     c                   eval      xxx = 1
032000020412     c                   movel     caetfe        lin(xxx)
032100020507     c                   endif
032200020507     c                   endif
032300020412     c                   endif
032400020412      * se escluso non carico il p.o. passato in d06key
032500020412     c                   if        d06esc = 'S' and caetfp = kcaetfp
032600020412     c                   iter
032700020412     c                   endif
032800020412      * carico la schiera
032900020412     c                   if        caetfp <> caetfe
033000020412     c                   add       1             xxx
033100020412     c                   movel     caetfp        lin(xxx)
033200020412     c                   endif
033300020412
033400020412     c                   enddo
033500020412
033600020412     c                   endif
033700020412
033800020412     c                   endsr
033900051130      *---------------------------------------------------------------*
034000051130      * Sr_car£X  - carico tutti i p.o. delle £1 + i p.o. gestiti
034100140117      *             in £6 da tutte le filiali in £1 non in £1
034200051130      *---------------------------------------------------------------*
034300051130     c     sr_car£X      begsr
034400051130     c                   eval      d06esc='G'
034500051130     c                   exsr      sr_car£6
034600051130     c                   clear                   d06esc
034700051130     c                   movea     lin           sk£6
034800051130     c                   clear                   lin
034900051130     c* Leggo schiera lin e aggiungo a skpog
035000051130     c                   z-add     1             xxx
035100051130     c                   dow       sk£6(xxx)>0
035200051130     c     sk£6(xxx)     lookup    skpog                                  30
035300051130     c                   if        not *in30
035400051130     c                   z-add     1             yyy
035500051130     c     000           lookup    skpog(YYY)                             31
035600051130     c                   if        *in31
035700051130     c                   movel     sk£6(xxx)     skpog(yyy)
035800051130     c                   endif
035900051130     c                   endif
036000051130     c                   add       1             xxx
036100051130     c                   enddo
036200051130     c
036300051130     c                   movea     skpog         lin
036400051130     c                   endsr
036500140117      *---------------------------------------------------------------*
036600140117      * Sr_car£J  - carico tutti i p.o. in gestione  contabile
036700140117      *---------------------------------------------------------------*
036800140117     c     sr_car£J      begsr
036900140117
037000140117     c                   clear                   lin
037100140117     c                   clear                   xxx
037200140117     c                   clear                   wok
037300140117     c                   clear                   d06err
037400140117
037500140117      * Carico i p.o. gestiti dal p.o. passato in d06key
037600140117     c                   eval      kcaeepa = 'J'
037700140117     c                   movel     d06key        kcaetfe
037800140117     c     kcaet         setll     azcae05l
037900140117    2c                   do        *hival
038000140117     c     kcaet         reade     azcae05l
038100140117      * fine file
038200140117    3c                   if        %eof(azcae05l)
038300140117     c                   leave
038400140117    3c                   endif
038500140117      * record annullato
038600140117    3c                   if        caeatb <> *blanks
038700140117     c                   iter
038800140117    3c                   endif
038900140117      * controllo la data validità
039000140117    3c                   if        caedde > wdata or caedsc < wdata
039100140117     c                   iter
039200140117    3c                   endif
039300140117     c                   eval      wok = 'S'
039400140117      * se non escluso al primo posto metto il p.o. in gestione
039500140117    3c                   if        d06esc = *blanks and xxx = 0
039600140117     c                   eval      xxx = 1
039700140117     c                   movel     d06key        lin(xxx)
039800140117    3c                   endif
039900140117      * carico la schiera
040000140117    3c                   if        caetfa <> caetfe
040100140117     c                   add       1             xxx
040200140117     c                   movel     caetfa        lin(xxx)
040300140117    3c                   endif
040400140117
040500140117    2c                   enddo
040600140117
040700140117      * non è stato trovato nessun p.o. in gestione
040800140117     c                   if        wok <> 'S' and d06esc = *blanks
040900140117     c                   eval      xxx = 1
041000140117     c                   movel     d06key        lin(xxx)
041100140117     c                   endif
041200140117
041300140117     c                   endsr
