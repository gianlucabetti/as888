000100040505     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040505      *------------------------------------------------------------------------*
000300040505      *
000400040505      *                       RICERCA C.A.
000500040505      *
000600040505      *------------------------------------------------------------------------*
000700040505
000800040505     fFndct02l  if   e           k Disk
000900040505     fFndct01l  if   e           k Disk    rename(Fndct000:Fndct001)
001000040505     fFndck02l  if   e           k Disk
001100040505
001200040505      *------------------------------------------------------------------------*
001300040505      *   S C H I E R E
001400040505      *------------------------------------------------------------------------*
001500040505
001600040505      *------------------------------------------------------------------------*
001700040505      *   V A R I A B I L I
001800040505      *------------------------------------------------------------------------*
001900040505     d FineCa          s              1    inz('0')
002000040505     d kaac            s                   like(DctAac)
002100040505     d kfil            s                   like(DctFil)
002200040505     d knca            s                   like(DctNca)
002300040505     d nca             s                   like(I12Nca)
002400040505     d NoOkCa          s              1    inz('0')
002500040505
002600040505      *------------------------------------------------------------------------*
002700040505      *   D S   I N T E R N E / E S T E R N E
002800040505      *------------------------------------------------------------------------*
002900040505     d dsDca           ds
003000040505     d  dsdac                         4  0
003100040505     d  dsmgc                         4  0
003200040505
003300040505     d dsKey           ds
003400040505     d  dsaac                         4  0
003500040505     d  dsfil                         3  0
003600040505     d  dsnca                         7  0
003700040505
003800040505     d dDct01        e ds
003900040505
004000040505     d Fidn12ds      e ds
004100040505     d  skKey                 26    305    dim(20)
004200040505     d  skAnn                306    325    dim(20)
004300040505     d  skDca                326    485    dim(20)
004400040505     d  skFca                486    545  0 dim(20)
004500040505     d  skDch                546    705  0 dim(20)
004600040505     d  skCch                706    745    dim(20)
004700050217
004800050217     d Azuteds       e ds                  extname(Azute00f)
004900050217     d dDatiute      e ds
005000050217     d dute01        e ds
005100050217     d Tibs34ds      e ds
005200040505
005300040505      *------------------------------------------------------------------------*
005400040505      *   C O S A T A N T I
005500040505      *------------------------------------------------------------------------*
005600040505
005700040505      *------------------------------------------------------------------------*
005800040505      *  RIEPILOGO INDICATORI
005900040505      *------------------------------------------------------------------------*
006000040505
006100040505      *------------------------------------------------------------------------*
006200040505
006300040505     c     *Entry        Plist
006400040505     c                   Parm                    Fidn12ds
006500050217
006600050217     c     *dtaara       define    §azute        azuteds
006700050217     c     *dtaara       define    §datiute      ddatiute
006800050217     c                   in(E)     *dtaara
006900050217     c                   If        %error  or RSUT = *blanks
007000050217     c                   Clear                   Tibs34ds
007100050217     c                   Call      'TIBS34R'
007200050217     c                   Parm                    Tibs34ds
007300050217     c                   In        *dtaara
007400050217     c                   EndIf
007500040505
007600040505     c     kSped         klist
007700040505     c                   kfld                    I12Aas
007800040505     c                   kfld                    I12Lnp
007900040505     c                   kfld                    I12Nrs
008000040505     c                   kfld                    I12Nsp
008100040505
008200040505     c     kCa           klist
008300040505     c                   kfld                    kaac
008400040505     c                   kfld                    kfil
008500040505     c                   kfld                    knca
008600040505
008700040505      * I12Tla = ' '  Elaboro e chiudo con RETURN
008800040505      * I12Tla = 'L'  Elaboro e chiudo con LR
008900040505      * I12Tla = 'C'            chiudo con LR  solo per chiusura file
009000040505
009100040505if  1c                   If        I12Tla = 'C'
009200040505     c                   Eval      *InLr = *On
009300040505
009400040505   x1c                   Else
009500040505
009600040505     c                   Eval      skKey = *all'0'
009700040505     c                   Eval      skDca = *all'0'
009800040505     c                   Eval      skFca = *all'0'
009900040505     c                   Eval      skDch = *all'0'
010000040505     c                   Clear                   O12Err
010100040505     c                   Clear                   O12Msg
010200040505
010300040505if  2c                   If        I12Tla = 'L'
010400040505     c                   Eval      *InLr = *On
010500040505
010600040809e   2c                   EndIf
010700040505
010800040505      * Controllo se passata la spedizione
010900040505if  3c                   If        I12Aas = *Zeros or
011000040505     c                             I12Lnp = *Zeros or
011100040505     c                             I12Nsp = *Zeros
011200040505     c                   Eval      O12Err = '1'
011300040505     c                   Eval      O12Msg = 'Non è stata passata la spedizione'
011400040505     c                   GoTo      Noelab
011500040505e   3c                   EndIf
011600040505      * Controllo il flag di elaborazione
011700040505      * deve contenere 'M' - 'F' - 'E'
011800040505if  3c                   If        I12Fel <> 'M' and I12Fel <> 'F' and
011900040505     c                             I12Fel <> 'E'
012000040505     c                   Eval      O12Err = '1'
012100040505     c                   Eval      O12Msg = 'Flag elaborazione errato'
012200040505     c                   GoTo      Noelab
012300040505e   3c                   EndIf
012400040714      * Controllo i flag di escludi
012500040714      * devono contenere ' ' - 'E'
012600040714if  3c                   If        I12Fan <> ' ' and I12Fan <> 'E'
012700040505     c                   Eval      O12Err = '1'
012800040505     c                   Eval      O12Msg = 'Flag annullate errato'
012900040505     c                   GoTo      Noelab
013000040505e   3c                   EndIf
013100040714if  3c                   If        I12Fch <> ' ' and I12Fch <> 'E'
013200040505     c                   Eval      O12Err = '1'
013300040505     c                   Eval      O12Msg = 'Flag chiuse errato'
013400040505     c                   GoTo      Noelab
013500040505e   3c                   EndIf
013600040714if  3c                   If        I12Fpt <> ' ' and I12Fpt <> 'E'
013700040505     c                   Eval      O12Err = '1'
013800040505     c                   Eval      O12Msg = 'Flag trasmesse errato'
013900040505     c                   GoTo      Noelab
014000040505e   3c                   EndIf
014100040505
014200040505     c                   Eval      FineCa = *Off
014300040505
014400040505      * Elaborazioni
014500040505s   1c                   Select
014600040505      * mamma
014700040505w   1c                   When      I12Fel = 'M'
014800040505     c                   ExSr      Sr_CercaM
014900040505      * figlia
015000040505w   1c                   When      I12Fel = 'F'
015100040505     c                   ExSr      Sr_CercaF
015200040505      * mamma/figlia
015300040505o   1c                   Other
015400040505     c                   ExSr      Sr_CercaE
015500040505e   1c                   EndSl
015600040505
015700040505     c     Noelab        Tag
015800040505
015900040505     c                   Return
016000040505
016100040505e   1c                   EndIf
016200040505
016300040505      *------------------------------------------------------------------------*
016400040505      * - Cerco le C.A. per spedizione mamma
016500040505      *------------------------------------------------------------------------*
016600040505     c     Sr_CercaM     BegSr
016700040505
016800040505     c     kSped         Setll     Fndct02l
016900040505do  1c                   Do        *Hival
017000040505     c     kSped         Reade     Fndct02l
017100040505     c                   If        %Eof(Fndct02l)
017200040505     c                   Leave
017300040505     c                   EndIf
017400040505
017500040505     c                   Eval      NoOkCa = *Off
017600040505
017700040505     c                   ExSr      Sr_Dati
017800040505
017900040505      * C.A. da escludere torno a leggere
018000040505     c                   If        NoOkca = *On
018100040505     c                   Iter
018200040505     c                   EndIf
018300040505
018400040505      * Esco quando ho già trovato le c.a. che hanno richiesto
018500040505     c                   If        FineCa = *On
018600040505     c                   Leave
018700040505     c                   EndIf
018800040505
018900040505e   1c                   EndDo
019000040505
019100040505     c                   EndSr
019200040505
019300040505      *------------------------------------------------------------------------*
019400040505      * - Cerco le C.A. per spedizione figlia
019500040505      *------------------------------------------------------------------------*
019600040505     c     Sr_CercaF     BegSr
019700040505
019800040505     c     kSped         Setll     Fndck02l
019900040505do  1c                   Do        *Hival
020000040505     c     kSped         Reade     Fndck02l
020100040505     c                   If        %Eof(Fndck02l)
020200040505     c                   Leave
020300040505     c                   EndIf
020400040505
020500040505     c                   Eval      NoOkCa = *Off
020600040505
020700040505      * Aggancio la testata delle c.a.
020800040505     c                   Eval      kaac = DckAac
020900040505     c                   Eval      kfil = DckFil
021000040505     c                   Eval      knca = DckNca
021100040505     c     kCa           Chain     Fndct01l
021200040505     c                   If        Not %Found(Fndct01l)
021300040505     c                   Iter
021400040505     c                   EndIf
021500040505
021600040505     c                   ExSr      Sr_Dati
021700040505
021800040505      * C.A. da escludere torno a leggere
021900040505     c                   If        NoOkca = *On
022000040505     c                   Iter
022100040505     c                   EndIf
022200040505
022300040505      * Esco quando ho già trovato le c.a. che hanno richiesto
022400040505     c                   If        FineCa = *On
022500040505     c                   Leave
022600040505     c                   EndIf
022700040505
022800040505e   1c                   EndDo
022900040505
023000040505     c                   EndSr
023100040505
023200040505      *------------------------------------------------------------------------*
023300040505      * - Cerco le C.A. per spedizione mamma/figlia
023400040505      *------------------------------------------------------------------------*
023500040505     c     Sr_CercaE     BegSr
023600040505
023700040505      * mamma
023800040505     c     kSped         Setll     Fndct02l
023900040505do  1c                   Do        *Hival
024000040505     c     kSped         Reade     Fndct02l
024100040505     c                   If        %Eof(Fndct02l)
024200040505     c                   Leave
024300040505     c                   EndIf
024400040505
024500040505     c                   Eval      NoOkCa = *Off
024600040505
024700040505     c                   ExSr      Sr_Dati
024800040505
024900040505      * C.A. da escludere torno a leggere
025000040505     c                   If        NoOkca = *On
025100040505     c                   Iter
025200040505     c                   EndIf
025300040505
025400040505      * Esco quando ho già trovato le c.a. che hanno richiesto
025500040505     c                   If        FineCa = *On
025600040505     c                   Leave
025700040505     c                   EndIf
025800040505
025900040505e   1c                   EndDo
026000040505
026100040505      * leggo le figlie solo se non ho già raggiunto il n. di c.a. richiesto
026200040505if  1c                   If        FineCa = *Off
026300040505      * figlia
026400040505     c     kSped         Setll     Fndck02l
026500040505do  2c                   Do        *Hival
026600040505     c     kSped         Reade     Fndck02l
026700040505     c                   If        %Eof(Fndck02l)
026800040505     c                   Leave
026900040505     c                   EndIf
027000040505
027100040505     c                   Eval      NoOkCa = *Off
027200040505
027300040505      * Aggancio la testata delle c.a.
027400040505     c                   Eval      kaac = DckAac
027500040505     c                   Eval      kfil = DckFil
027600040505     c                   Eval      knca = DckNca
027700040505     c     kCa           Chain     Fndct01l
027800040505     c                   If        Not %Found(Fndct01l)
027900040505     c                   Iter
028000040505     c                   EndIf
028100040505
028200040505     c                   ExSr      Sr_Dati
028300040505
028400040505      * C.A. da escludere torno a leggere
028500040505     c                   If        NoOkca = *On
028600040505     c                   Iter
028700040505     c                   EndIf
028800040505
028900040505      * Esco quando ho già trovato le c.a. che hanno richiesto
029000040505     c                   If        FineCa = *On
029100040505     c                   Leave
029200040505     c                   EndIf
029300040505
029400040505e   2c                   EndDo
029500040505
029600040505e   1c                   EndIf
029700040505
029800040505     c                   EndSr
029900040505
030000040505      *------------------------------------------------------------------------*
030100040505      * - Controllo se ok la c.a. e carico i dati
030200040505      *------------------------------------------------------------------------*
030300040505     c     Sr_Dati       BegSr
030400050217      * Se sono in filiale scarto le c.a. con dctdt2 > 0 e torno a leggere
030500050217     c                   If        dutpou <> 46 and dctdt2 > *Zeros
030600050217     c                   Eval      NoOkCa = *On
030700050217     c                   GoTo      EndSrDati
030800050217     c                   EndIf
030900040505
031000040505      * Se escludo le annullate torno a leggere
031100040505     c                   If        I12Fan = 'E' and DctAtb <> *Blanks
031200040505     c                   Eval      NoOkCa = *On
031300040505     c                   GoTo      EndSrDati
031400040505     c                   EndIf
031500040505      * Se escludo le chiuse torno a leggere
031600040505     c                   If        I12Fch = 'E' and DctCch <> *Blanks and
031700040505     c                             DctDch > *Zeros
031800040505     c                   Eval      NoOkCa = *On
031900040505     c                   GoTo      EndSrDati
032000040505     c                   EndIf
032100040505      * Se escludo le trasmesse torno a leggere
032200040505     c                   Eval      dDct01 = DctFlo
032300040505     c                   If        I12Fpt = 'E' and §Dcttrpt = 'S'
032400040505     c                   Eval      NoOkCa = *On
032500040505     c                   GoTo      EndSrDati
032600040505     c                   EndIf
032700040505
032800040505      * conto le c.a. che ho trovato
032900040505     c                   add       1             O12Nca
033000040505     c                   Eval      dsaac = DctAac
033100040505     c                   Eval      dsfil = DctFil
033200040505     c                   Eval      dsnca = DctNca
033300040505
033400040505     c                   Eval      dsdac = DctAac
033500040505     c                   Eval      dsmgc = DctMgc
033600040505
033700040505     c                   Eval      skKey(O12Nca) = dsKey
033800040505     c                   Eval      skAnn(O12Nca) = DctAtb
033900040505     c                   Eval      skDca(O12Nca) = dsDca
034000040505     c                   Eval      skFca(O12Nca) = DctFca
034100040505     c                   Eval      skDch(O12Nca) = DctDch
034200040505     c                   Eval      skCch(O12Nca) = DctCch
034300040505
034400040505      * quando arrivo al numero di c.a. richiesto smetto di cercare
034500040505     c                   If        (O12Nca = I12Nca and I12Nca > *Zeros) or
034600040505     c                             (O12Nca = 20 and I12Nca = *Zeros)
034700040505     c                   Eval      FineCa = *On
034800040505     c                   EndIf
034900040505
035000040505     c     EndSrDati     EndSr
