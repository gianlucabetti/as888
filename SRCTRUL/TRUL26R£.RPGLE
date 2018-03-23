000100020412     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020529      *
000300020529      *  Caricamento di tutti i terminal
000400020529      *
000500020412
000600020529     fazcae01l  if   e           k disk
000700020802     fazorg01l  if   e           k disk
000800020412
000900020802     D og143         E DS
001000020412     d kcaeepa         s                   like(caeepa)
001100020529     d wdata           s                   like(i26dta)
001200020412
001300020529     d aa              s              2  0
001400020529     d bb              s              3  0
001500020412
001600020529     d trul26ds      e ds
001700020529     d  skpo                  11    760  0 dim(250)
001800020603     d  skpot                761   1510  0 dim(250)
001900020529
002000020529     d trul06ds      e ds
002100020412     d  lin                    1     90  0 dim(30)
002200020529
002300020529     d kpjba         e ds
002400020802
002500020802     d  sort           s              3  0 dim(250)
002600020802     d  skig           s              3  0 dim(250)
002700020802     d  skip           s              3  0 dim(250)
002800020412
002900020412      *---------------------------------------------------------------
003000020412     c     *entry        plist
003100020529     c                   parm                    trul26ds
003200020529
003300020603     c                   eval      skpo  = *all'0'
003400020603     c                   eval      skpot = *all'0'
003500020802     c                   eval      sORT = *all'0'
003600020802     c                   eval      SKIP = *all'0'
003700020802     c                   eval      SKIG = *all'0'
003800020412
003900020529      * i26tla = ' '  Elaboro e chiudo con RETURN
004000020529      * i26tla = 'L'  Elaboro e chiudo con LR
004100020529      * i26tla = 'C'            chiudo con LR  solo per chiusura file
004200020412
004300020529    1c                   if        i26tla = 'C'
004400020412     c                   eval      *inlr = *on
004500020412
004600020412   X1c                   else
004700020808
004800020808     c                   Clear                   aa
004900020808     c                   Clear                   bb
005000020412
005100020412      * se non passata data dal chiamante prendo udate per i controlli della data
005200020529     c                   if        i26dta <> *zeros
005300020529     c                   eval      wdata = i26dta
005400020412     c                   else
005500020412     c                   eval      wdata = *date
005600020412     c                   endif
005700020412
005800020412     c                   select
005900020412
006000020529      * Richiamato con tipo terminal 'P' - tutti i terminal di partenza
006100020529     c                   when      i26tpt = 'P'
006200020529     c                   exsr      sr_carp
006300020529      * Richiamato con tipo terminal '6' - tutti i terminal di £6
006400020529     c                   when      i26tpt = '6'
006500020529     c                   exsr      sr_car6
006600020802      * Richiamato con tipo terminal 'g' - orgfel o §ogas di gruppo GRU
006700020802     c                   when      i26tpt = 'G'
006800020802     c                   exsr      sr_carg
006900020412
007000020412     c                   endsl
007100020412
007200020529    2c                   if        i26tla = 'L'
007300020603      * chiudo i file del trul06
007400020412     c                   eval      *inlr = *on
007500020603     c                   eval      d06tla = 'C'
007600020603     c                   movel     trul06ds      kpjbu
007700020603     c                   call      'TRUL06R'
007800020603     c                   parm                    kpjba
007900020603
008000020412   X2c                   else
008100020603
008200020412     c                   return
008300020603
008400020412    2c                   endif
008500020412
008600020412     c                   endif
008700020412
008800020412      *---------------------------------------------------------------*
008900020529      * Sr_carp  - carico tutti i terminal di partenza
009000020412      *---------------------------------------------------------------*
009100020529     c     sr_carp       begsr
009200020802
009300020808     C**!!!              Z-ADD     1             bb
009400020808     C**!!!              Z-ADD     1             aa
009500020412     c                   eval      kcaeepa = 'P'
009600020529     c     kcaeepa       setll     azcae01l
009700020412     c                   do        *hival
009800020529     c     kcaeepa       reade     azcae01l
009900020412      * fine file
010000020529     c                   if        %eof(azcae01l)
010100020412     c                   leave
010200020412     c                   endif
010300020412      * record annullato
010400020412     c                   if        caeatb <> *blanks
010500020412     c                   iter
010600020412     c                   endif
010700020412      * controllo la data validità
010800020412     c                   if        caedde > wdata or caedsc < wdata
010900020412     c                   iter
011000020412     c                   endif
011100020529      * caetfp = caetfe
011200020529     c                   if        caetfp <> caetfe
011300020529     c                   iter
011400020529     c                   endif
011500020529      * richiamo il trul06r con '£1'
011600020529     c                   clear                   trul06ds
011700020529     c                   eval      d06cod = '£1'
011800020529     c                   movel     caetfe        d06key
011900020529     c                   if        i26dta <> *zeros
012000020529     c                   move      'S'           d06key
012100020529     c                   eval      d06drf = i26dta
012200020529     c                   endif
012300020529     c                   movel     trul06ds      kpjbu
012400020529     c                   call      'TRUL06R'
012500020529     c                   parm                    kpjba
012600020529     c                   movel     kpjbu         trul06ds
012700020529      * se non ok trul06r leggo un altro terminal
012800020529     c                   if        d06err <> *blanks
012900020529     c                   iter
013000020529     c                   endif
013100020529      * carico la schiera dei terminal
013200020529     c                   do        30            aa
013300020529     c                   if        lin(aa) = *zeros
013400020529     c                   leave
013500020529     c                   endif
013600020529     c     lin(aa)       lookup    skpo                                   90
013700020529     c                   if        not %found
013800020529     c                   add       1             bb
013900020603     c                   eval      skpo(bb)  = lin(aa)
014000020603     c                   eval      skpot(bb) = caetfe
014100020529     c                   endif
014200020529     c                   enddo
014300020412
014400020412     c                   enddo
014500020412
014600020412     c                   endsr
014700020412      *---------------------------------------------------------------*
014800020529      * Sr_car6  - carico tutti i terminal di £6
014900020412      *---------------------------------------------------------------*
015000020529     c     sr_car6       begsr
015100020808     C**!!!              Z-ADD     1             bb
015200020808     C**!!!              Z-ADD     1             aa
015300020530     c                   eval      kcaeepa = '6'
015400020530     c     kcaeepa       setll     azcae01l
015500020530     c                   do        *hival
015600020530     c     kcaeepa       reade     azcae01l
015700020530      * fine file
015800020530     c                   if        %eof(azcae01l)
015900020530     c                   leave
016000020530     c                   endif
016100020530      * record annullato
016200020530     c                   if        caeatb <> *blanks
016300020530     c                   iter
016400020530     c                   endif
016500020530      * controllo la data validità
016600020530     c                   if        caedde > wdata or caedsc < wdata
016700020530     c                   iter
016800020530     c                   endif
016900020530      * caetfp = caetfe
017000020530     c                   if        caetfa <> caetfe
017100020530     c                   iter
017200020530     c                   endif
017300020530      * richiamo il trul06r con '£6'
017400020530     c                   clear                   trul06ds
017500020530     c                   eval      d06cod = '£6'
017600020530     c                   movel     caetfe        d06key
017700020530     c                   if        i26dta <> *zeros
017800020530     c                   move      'S'           d06key
017900020530     c                   eval      d06drf = i26dta
018000020530     c                   endif
018100020530     c                   movel     trul06ds      kpjbu
018200020530     c                   call      'TRUL06R'
018300020530     c                   parm                    kpjba
018400020530     c                   movel     kpjbu         trul06ds
018500020530      * se non ok trul06r leggo un altro terminal
018600020530     c                   if        d06err <> *blanks
018700020530     c                   iter
018800020530     c                   endif
018900020530      * carico la schiera dei terminal
019000020530     c                   do        30            aa
019100020530     c                   if        lin(aa) = *zeros
019200020530     c                   leave
019300020530     c                   endif
019400020530     c     lin(aa)       lookup    skpo                                   90
019500020530     c                   if        not %found
019600020530     c                   add       1             bb
019700020603     c                   eval      skpo(bb)  = lin(aa)
019800020603     c                   eval      skpot(bb) = caetfe
019900020530     c                   endif
020000020530     c                   enddo
020100020530
020200020530     c                   enddo
020300020412
020400020412     c                   endsr
020500020802      *---------------------------------------------------------------*
020600020802      * Sr_carg  - carico orgfel o §ogas di gruppo GRU
020700020802      *---------------------------------------------------------------*
020800020802     c     sr_carg       begsr
020900020802     C                   Z-ADD     1             C                 4 0
021000020802     C     *LOVAL        SETLL     AZORG
021100020802     C                   READ      AZORG                                  31
021200020802    1C     *IN31         DOWEQ     '0'
021300020802     C*
021400020802    2c                   if        orgfag='F' or orgfag='A'
021500020802     C* NON ANNULLATO
021600020802    3C     ORGFVA        IFEQ      ' '
021700020802     c                   movel     orgde3        og143
021800020802    5c                   if        §ogas=946
021900020802     C                   Z-ADD     §ogas         sort(C)
022000020802     C                   Z-ADD     §ogas         skig(C)
022100020802     C                   Z-ADD     orgfil        skip(C)
022200020802     C                   ADD       1             C
022300020802     c                   else
022400020802     C                   Z-ADD     orgfel        sort(C)
022500020802     C                   Z-ADD     orgfel        SKIG(C)
022600020802     C                   Z-ADD     orgfil        skip(C)
022700020802     C                   ADD       1             C
022800020802     c                   endif
022900020802     c                   endif
023000020802     c                   endif
023100020802     C                   READ      AZORG                                  31
023200020802    1C                   ENDdo
023300020802     c
023400020802     c* ordino la schiera
023500020802     c                   sorta     sort
023600020802     c
023700020802     C                   Z-ADD     1             C                 4 0
023800020802     C                   Z-ADD     1             E                 4 0
023900020802     c                   clear                   savpo             3 0
024000020802    1c                   do        250           C
024100020802    2C                   IF        SORT(C)<>SAVPO  AND SORT(C)>0
024200020802     C                   EVAL      SAVPO=SORT(C)
024300020802     C
024400020802     C                   Z-ADD     1             d                 4 0
024500020802     C     SAVPO         LOOKUP    SKIG(D)                                33
024600020802    3C                   DOW       *IN33
024700020802     C                   EVAL      SKPO(E)=SKIP(D)
024800020802     C                   EVAL      SKPOT(E)=SKIG(D)
024900020802     C                   ADD       1             E
025000020802     C                   ADD       1             D
025100020802     C     SAVPO         LOOKUP    SKIG(D)                                33
025200020802    3C                   ENDDO
025300020802    2C                   ENDIF
025400020802    1C                   ENDDO
025500020802     C
025600020802     c
025700020802     c                   endsr
