000100020927     h Decedit('0,') Datedit(*dmy.) option(*nodebugio)
000200040806      * FITS13R *----------------------------------------------------*
000300040806      *  estrapola FOGLI E RELATIVE SPUNTE                      *
000400020927      *--------------------------------------------------------------*
000500040806     fFnfvv07l  if   e           k Disk
000600040806     fFnfgv02l  if   e           k Disk
000700040806     fFnfgw01l  if   e           k Disk
000800040806     fFnfv401l  if   e           k Disk
000900040806     fFnfv201l  if   e           k Disk
001000040806     fFnfvd01l  if   e           k Disk
001100040806     fFnFVA05L  if   e           k Disk
001200040806     fFnFwa01l  if   e           k Disk
001300040806     fFnfvvNEW  if a e           k Disk    Rename(fnfvv000:fnfvvn)
001400040806     fFnfgvNEW  if a e           k Disk    Rename(fnfgv000:fnfgvn)
001500040806     fFnfgwNEW  if a e           k Disk    Rename(fnfgw000:fnfgwn)
001600040806     fFnfv4NEW  if a e           k Disk    Rename(fnfv4000:fnfv4n)
001700040806     fFnfv2NEW  if a e           k Disk    Rename(fnfv2000:fnfv2n)
001800040806     fFnfvdNEW  if a e           k Disk    Rename(fnfvd000:fnfvdn)
001900040806     fFndstNEW  o  a e           k Disk    Rename(fndst000:fndstn)
002000070430     fFidstNEW  o  a e           k Disk    Rename(fidst000:fidstn) prefix(ids)
002100070430     fFidst3NW  o  a e           k Disk    Rename(fidst300:fidst3n)
002200070430     fFidst4NW  o  a e           k Disk    Rename(fidst400:fidst4n)
002300070725     fFidst5NW  o  a e           k Disk    Rename(fidst500:fidst5n)
002400070725     fFidst6NW  o  a e           k Disk    Rename(fidst600:fidst6n)
002500090209     fFidsfNEW  o  a e           k Disk    Rename(fidsf000:fidsfn)
002600070123     fFnbrvNEW  o  a e           k Disk    Rename(fnbrv000:fnbrvn)
002700070123     fFnbrvENW  o  a e           k Disk    Rename(fnbrv000:fnbrvEn)
002800040806     fFNFVANEW  if a e           k Disk    Rename(fnfva000:fnfvan)
002900040806     fFNFwaNEW  if a e           k Disk    Rename(fnfwa000:fnfwan)
003000040806     fFndst01l  if   e           k Disk
003100070430     fFidst09l  if   e           k Disk    prefix(ids)
003200070430     fFidst31l  if   e           k Disk
003300070430     fFidst41l  if   e           k Disk
003400070725     fFidst51l  if   e           k Disk
003500070725     fFidst61l  if   e           k Disk
003600090209     fFidsf02l  if   e           k Disk
003700070123     fFnbrv01l  if   e           k Disk
003800070123     fFnbrv02L  if   e           k Disk    Rename(fnbrv000:fnbrv002)
003900070123     fFnbrve1l  if   e           k Disk    Rename(fnbrv000:fnbrve)
004000040806     d*
004100040806     D* Schiera p.o. da trasferire
004200040806     D  SKPO           s              3    dim(500)
004300040806     d*
004400040806     d KLIN            s                   like(BRVLNP)
004500040806     d KLINSPU         s                   like(BRVLNP)
004600040806     d KNpg            s                   like(BRVnpg)
004700040806     d KNfv            s                   like(BRVnfv)
004800040806     d KNfv5           s                   like(fgvnfv)
004900040806     d Kfgs            s                   like(BRVfgs)
005000040806     d Klnp            s                   like(BRVLNP)
005100040806     d kEpa            s                   Like(Fv4Epa)
005200040806     d klai            s                   Like(Fvalai)
005300040806     D KPJBA         E DS
005400040806     c*------------------------------------------------------------------
005500040806     C     *ENTRY        PLIST
005600040806     C                   PARM                    KPJBA
005700040806     C                   parm                    PARAM          1500
005800040806     c
005900040806     c     ' ':'0'       xlate     param         param
006000040806     c
006100040806     c                   movea     param         skpo
006200020927
006300020927      * Fndst
006400020927     c     Kdst          Klist
006500040806     c                   Kfld                    FvvNfv
006600040806     c                   Kfld                    FvvFgs
006700070430      * Fidst
006800070430     c     Kidst         Klist
006900070430     c                   Kfld                    FvvFgs
007000070430     c                   Kfld                    FvvNpg
007100070430     c                   Kfld                    FvvNfv6           6 0
007200070430      * Fidst3
007300070430     c     Kidst3        Klist
007400070430     c                   Kfld                    FvvFgs
007500070430     c                   Kfld                    FvvNfv6           6 0
007600090209      * Fidsf
007700090209     c     Kidsf         Klist
007800090209     c                   Kfld                    FvvNpg
007900090209     c                   Kfld                    FvvNfv6
008000090209     c                   Kfld                    FvvFgs
008100020927      * Fnfvv
008200040806     c     Kfvv          Klist
008300040806     c                   Kfld                    kNpg
008400040806     c                   Kfld                    kNfv
008500040806     c                   Kfld                    klin
008600040806     c     KfvvO         Klist
008700040806     c                   Kfld                    fvvnpg
008800040806     c                   Kfld                    fvvnfv
008900040806     c                   Kfld                    fvvfgs
009000040806     c     Kfvv1         Klist
009100040806     c                   Kfld                    kNpg
009200040806     c                   Kfld                    kNfv
009300040806     c                   Kfld                    klin
009400040806     c                   Kfld                    KLINSPU
009500040806     c     Kfgv          Klist
009600040806     c                   Kfld                    kNfv5
009700040806     c                   Kfld                    klin
009800040806     c     Kfgv1         Klist
009900040806     c                   Kfld                    kLin
010000040806     c                   Kfld                    kNfv5
010100040806     c     Kfv4          Klist
010200040806     c                   Kfld                    Fv4Lnp
010300040806     c                   Kfld                    Fv4Nfv
010400040806     c                   Kfld                    Fv4Epa
010500040806     c                   Kfld                    Fv4Lai
010600040806     c                   Kfld                    Fv4Trc
010700040806     c                   Kfld                    Fv4Not
010800040806     c     Kfv2          Klist
010900040806     c                   Kfld                    Fv2Lnp
011000040806     c                   Kfld                    Fv2Nfv
011100040806     c                   Kfld                    Fv2Epa
011200040806     c                   Kfld                    Fv2Lai
011300040806     c                   Kfld                    Fv2Tdh
011400040806     c     Kfvd          Klist
011500040806     c                   Kfld                    FvdLnp
011600040806     c                   Kfld                    FvdNfv
011700040806     c                   Kfld                    FvdLna
011800040806     c                   Kfld                    FvdTrc
011900040806     c     Kfwa          Klist
012000040806     c                   Kfld                    FvaLnp
012100040806     c                   Kfld                    FvaNfv
012200040806     c                   Kfld                    FvaLai
012300040806     c     Kfv4O         Klist
012400040806     c                   Kfld                    kLnp
012500040806     c                   Kfld                    kNfv5
012600040806     c                   Kfld                    KEpa
012700040806     c                   Kfld                    klai
012800040806     c     Kfva          Klist
012900040806     c                   Kfld                    FvaNpg
013000040806     c                   Kfld                    FvaNfa
013100040806     c                   Kfld                    KLai
013200040806     c                   Kfld                    KLnp
013300040806     c                   Kfld                    KNfv5
013400040806     c*------------------------------------------------------------------
013500040806     c                   z-add     1             x                 4 0
013600040806    1c                   dow       skpo(x)>*zeros
013700040806     c                   movel     skpo(X)       klin
013800040806     C     klin          SETLL     FNfvv07l
013900040806     C     klin          READe     FNfvv07l                               30
014000040806     C*
014100040806    2C     *IN30         DOWEQ     *OFF
014200040806     c
014300040806     c* Elaboro e prendo le spunte
014400040806     c                   z-add     fvvnpg        knpg
014500040806     c                   z-add     fvvnfv        knfv
014600040806     c                   EXSR      SPUNTE
014700040806     c
014800040806     c* Elaboro tutti i fogli vari del p.o. con relative spunte
014900040806     c                   write     fnfvvn                               99
015000040806     c* Se cat. 4 carico anche DST
015100040806     c                   if        fvvnpg=4
015200040806     c     kdst          chain     fndst01l                           32
015300040806     c  n32              write     fndstn                               99
015400070430      * nuove distinte
015500070430     c                   z-add     fvvnfv        fvvNFV6
015600070430     c     kidst         chain     fidst09l                           32
015700070430     c  n32              do
015800070430     c                   write     fidstn                               99
015900070430      * nuove distinte estensioni
016000070430     c     kidst3        setll     fidst31l
016100070430     c     kidst3        reade     fidst31l
016200070430     c                   dow       not %Eof(fidst31l)
016300070430     c                   write     fidst3n                              99
016400070430     c     kidst3        reade     fidst31l
016500070430     c                   enddo
016600070430      * nuove distinte estensioni
016700070430     c     kidst         setll     fidst41l
016800070430     c     kidst         reade     fidst41l
016900070430     c                   dow       not %Eof(fidst41l)
017000070430     c                   write     fidst4n                              99
017100070430     c     kidst         reade     fidst41l
017200070430     c                   enddo
017300070725      * nuove distinte estensioni
017400070725     c     kidst3        setll     fidst51l
017500070725     c     kidst3        reade     fidst51l
017600070725     c                   dow       not %Eof(fidst51l)
017700070725     c                   write     fidst5n                              99
017800070725     c     kidst3        reade     fidst51l
017900070725     c                   enddo
018000070725      * nuove distinte estensioni
018100070725     c     kidst         setll     fidst61l
018200070725     c     kidst         reade     fidst61l
018300070725     c                   dow       not %Eof(fidst61l)
018400070725     c                   write     fidst6n                              99
018500070725     c     kidst         reade     fidst61l
018600070725     c                   enddo
018700090209      * nuove distinte estensioni fasi
018800090209     c     kidsf         setll     fidsf02l
018900090209     c     kidsf         reade     fidsf02l
019000090209     c                   dow       not %Eof(fidsf02l)
019100090209     c                   write     fidsfn                               99
019200090209     c     kidsf         reade     fidsf02l
019300090209     c                   enddo
019400070430     c                   end
019500040806     c                   endif
019600040806     c
019700040806     C     klin          READe     FNfvv07l                               30
019800040806     c                   enddo
019900040806     c*
020000040806     c* fogli viaggio partenza
020100040806     C     klin          SETLL     FNfgv02l
020200040806     C     klin          READe     FNfgv02l                               30
020300040806     C*
020400040806    2C     *IN30         DOWEQ     *OFF
020500040806     c
020600040806     c* Elaboro e prendo le spunte
020700040806     c                   z-add     1             knpg
020800040806     c                   z-add     fgvnfv        knfv
020900040806     c                   EXSR      SPUNTE
021000040806     c* Scrivo il foglio
021100040806     c                   EXSR      WRITEFGV
021200040806     c*
021300040806     C     klin          READe     FNfgv02l                               30
021400040806     c                   enddo
021500040806     c*
021600040806     c* fogli viaggio partenza in arrivo
021700040806     C     klin          SETLL     FNfva05l
021800040806     C     klin          READe     FNfva05l                               30
021900040806     C*
022000040806    2C     *IN30         DOWEQ     *OFF
022100040806     c                   z-add     fvanfv        knfv
022200040806     c*
022300040806     C* SE FVALNP È TRA I P.O. DA TRASFERIFE, le spunte le
022400040806     c*  elaboro con p.o. = a fvalnp
022500040806     c                   movel     fvalnp        w003a             3
022600040806     c     w003a         lookup    skpo                                   32
022700040806     c                   if        not *in32
022800040806     c
022900040806     c* Elaboro e prendo le spunte
023000040806     c                   z-add     1             knpg
023100040806     c                   EXSR      SPUNTE
023200040806     c                   endif
023300040806     c*
023400040806     c                   EXSR      WRITEFVA
023500040806     c*
023600040806     C     klin          READe     FNfva05l                               30
023700040806     c                   enddo
023800040806     C
023900040806     C                   ADD       1             X
024000040806     c                   enddo
024100040806     c***
024200040806     c* Leggo tutti i fogli non dei p.o. trasferiti ma con una spunta
024300040806     c*  riguardante i p.o. trasferiti
024400040806     c***
024500040806     c     *loval        setll     fnfvv07l
024600040806     c                   read      fnfvv07l                               30
024700040806     c
024800040806     c                   dow       not *in30
024900040806     c                   eval      klin=fvvfgs
025000040806     c                   movel     fvvfgs        w003a
025100040806     c
025200040806     c     w003a         lookup    skpo                                   32
025300040806     c
025400040806     c* Se P.o. gestione non elaborato: lo leggo
025500040806     c                   if        not *in32
025600040806     C
025700040806     c     klin          setll     fnfvv07l
025800040806     c     klin          reade     fnfvv07l                               30
025900040806     c                   dow       not *in30
026000040806     c
026100040806     c* Elaboro solo le spunte con lnp o lna = al p.o. trasferito
026200040806     c                   z-add     fvvnpg        knpg
026300040806     c                   z-add     fvvnfv        knfv
026400040806     c                   exsr      SPUNTEMIE
026500040806     c
026600040806     c* Se estrapolata almeno una spunta, scrivo il foglio
026700040806     c                   if        welaspu='S'
026800040806     c     kfvvo         setll     fnfvvnew                               32
026900040806     c*
027000040806     c                   if        not *in32
027100040806     c                   write     fnfvvn                               99
027200040806     c* Se cat. 4 carico anche DST
027300040806     c                   if        fvvnpg=4
027400040806     c     kdst          chain     fndst01l                           32
027500040806     c  n32              write     fndstn                               99
027600070430     c                   z-add     fvvnfv        fvvNFV6
027700070430     c     kidst         chain     fidst09l                           32
027800070430     c  n32              do
027900070430     c                   write     fidstn                               99
028000070430      * nuove distinte estensioni
028100070430     c     kidst3        setll     fidst31l
028200070430     c     kidst3        reade     fidst31l
028300070430     c                   dow       not %Eof(fidst31l)
028400070430     c                   write     fidst3n                              99
028500070430     c     kidst3        reade     fidst31l
028600070430     c                   enddo
028700070430      * nuove distinte estensioni
028800070430     c     kidst         setll     fidst41l
028900070430     c     kidst         reade     fidst41l
029000070430     c                   dow       not %Eof(fidst41l)
029100070430     c                   write     fidst4n                              99
029200070430     c     kidst         reade     fidst41l
029300070430     c                   enddo
029400070725      * nuove distinte estensioni
029500070725     c     kidst3        setll     fidst51l
029600070725     c     kidst3        reade     fidst51l
029700070725     c                   dow       not %Eof(fidst51l)
029800070725     c                   write     fidst5n                              99
029900070725     c     kidst3        reade     fidst51l
030000070725     c                   enddo
030100070725      * nuove distinte estensioni
030200070725     c     kidst         setll     fidst61l
030300070725     c     kidst         reade     fidst61l
030400070725     c                   dow       not %Eof(fidst61l)
030500070725     c                   write     fidst6n                              99
030600070725     c     kidst         reade     fidst61l
030700070725     c                   enddo
030800090209      * nuove distinte estensioni fasi
030900090209     c     kidsf         setll     fidsf02l
031000090209     c     kidsf         reade     fidsf02l
031100090209     c                   dow       not %Eof(fidsf02l)
031200090209     c                   write     fidsfn                               99
031300090209     c     kidsf         reade     fidsf02l
031400090209     c                   enddo
031500070430     c                   end
031600040806     c                   endif
031700040806     c                   endif
031800040806     c                   endif
031900040806     C
032000040806     c     klin          reade     fnfvv07l                               30
032100040806     c                   enddo
032200040806     c                   endif
032300040806     c**
032400040806     c* elaboro nuovo p.o. gestione
032500040806     c     klin          setgt     fnfvv07l
032600040806     c                   read      fnfvv07l                               30
032700040806     c                   enddo
032800040806     C**
032900040806     C**
033000040806     c     *loval        setll     fnfGV02l
033100040806     c                   read      fnfGv02l                               30
033200040806     c
033300040806     c                   dow       not *in30
033400040806     c                   eval      klin=fGvlnp
033500040806     c                   movel     fgvlnp        w003a
033600040806     c
033700040806     c     w003a         lookup    skpo                                   32
033800040806     c
033900040806     c* Se P.o. gestione non elaborato: lo leggo
034000040806     c                   if        not *in32
034100040806     C
034200040806     c     klin          setll     fnfGv02l
034300040806     c     klin          reade     fnfGv02l                               30
034400040806     c                   dow       not *in30
034500040806     c
034600040806     c* Elaboro solo le spunte con lnp o lna = al p.o. trasferito
034700040806     c                   z-add     1             knpg
034800040806     c                   z-add     fGvnfv        knfv
034900040806     c                   exsr      SPUNTEMIE
035000040806     c
035100040806     c* Se estrapolata almeno una spunta, scrivo il foglio
035200040806     c                   if        welaspu='S'
035300040806     c                   EXSR      WRITEFGV
035400040806     c                   endif
035500040806     C
035600040806     c     klin          reade     fnfGV02l                               30
035700040806     c                   enddo
035800040806     c                   endif
035900040806     c**
036000040806     c* elaboro nuovo p.o. gestione
036100040806     c     klin          setgt     fnfGv02l
036200040806     c                   read      fnfGv02l                               30
036300040806     c                   enddo
036400040806     c
036500021001     c                   Eval      *InLr = *On
036600040806     C*-------------------------------------------------------------------
036700040806     C* SCRIVO LE SPUNTE DEI FOGLI LETTI DA ESTRAPOLARE
036800040806     C*-------------------------------------------------------------------
036900040806     c     SPUNTE        begsr
037000070123     C     kfvv          SETLL     Fnbrv01l
037100070123     C     kfvv          READe     Fnbrv01l                               31
037200040806     c                   dow       not *in31
037300070123     c                   write     fnbrvn                               99
037400040806     c*
037500070123     C     kfvv          READe     Fnbrv01l                               31
037600040806     c                   enddo
037700070123     C     kfvv          SETLL     Fnbrve1l
037800070123     C     kfvv          READe     Fnbrve1l                               31
037900040806     c                   dow       not *in31
038000070123     c                   write     fnbrven                              99
038100040806     c*
038200070123     C     kfvv          READe     Fnbrve1l                               31
038300040806     c                   enddo
038400040806     c                   ENDSR
038500040806     C*-------------------------------------------------------------------
038600040806     C* ESTRAPOLO LE SPUNTE DEI FOGLI LETTI DELLE LINEE DA ELABORARE
038700040806     C*-------------------------------------------------------------------
038800040806     c     SPUNTEMIE     begsr
038900040806     c                   clear                   welaspu           1
039000040806     c                   z-add     1             Y                 4 0
039100040806    1c                   dow       skpo(y)>*zeros
039200040806     c                   movel     skpo(y)       klinSPU
039300040806     C* PRIMA LA LEGGO COME LNP
039400070123     C     kfvv1         SETLL     Fnbrv01l
039500070123     C     kfvv1         READe     Fnbrv01l                               31
039600040806     c                   dow       not *in31
039700070123     c                   write     fnbrvn                               99
039800040806     C                   EVAL      WELASPU='S'
039900040806     c*
040000070123     C     kfvv1         READe     Fnbrv01l                               31
040100040806     c                   enddo
040200040806     C* POI   LA LEGGO COME LNA
040300070123     C     kfvv1         SETLL     Fnbrv02l
040400070123     C     kfvv1         READe     Fnbrv02l                               31
040500040806     c                   dow       not *in31
040600070123     c                   write     fnbrvn                               99
040700040806     C                   EVAL      WELASPU='S'
040800040806     c*
040900070123     C     kfvv1         READe     Fnbrv02l                               31
041000040806     c                   enddo
041100040806     c
041200040806     c                   add       1             y
041300040806     c                   enddo
041400040806     C
041500070123     C     kfvv          SETLL     Fnbrve1l
041600070123     C     kfvv          READe     Fnbrve1l                               31
041700040806     c                   dow       not *in31
041800040806     C                   IF        BRVLNP=KLINSPU  OR BRVLNA=KLINSPU
041900070123     c                   write     fnbrven                              99
042000040806     C                   EVAL      WELASPU='S'
042100040806     C                   ENDIF
042200040806     c*
042300070123     C     kfvv          READe     Fnbrve1l                               31
042400040806     c                   enddo
042500040806     c                   ENDSR
042600040806     C*-------------------------------------------------------------------
042700040806     C* scrivo tutti i file relativi a fnfgv
042800040806     C*-------------------------------------------------------------------
042900040806     c     WRITEFGV      BEGSR
043000041111     c                   z-add     fgvnfv        knfv5
043100040806     c*
043200040806      *-- Leggo FNFGW
043300040806     c     Kfgv          Chain     Fnfgw01l
043400040806If  2c                   If        %Found(Fnfgw01l)
043500040806      * Controllo se già scritto FNFGW
043600040806     c     Kfgv          Setll     Fnfgwnew                               32
043700040806     c  n32              Write     Fnfgwn
043800040806    3c                   EndIf
043900040806      *-- Leggo FNFV4
044000040806     c     Kfgv1         Setll     Fnfv401l
044100040806Do  2c                   Do        *Hival
044200040806     c     Kfgv1         Reade     Fnfv401l
044300040806      * Fine file esco
044400040806     c                   If        %Eof(Fnfv401l)
044500040806     c                   Leave
044600040806     c                   EndIf
044700040806      * Controllo se già scritto FNFV4
044800040806     c     Kfv4          Setll     Fnfv4NEW                               32
044900040806     c  n32              Write     Fnfv4n
045000040806    2c                   EndDo
045100040806      *-- Leggo FNFV2
045200040806     c     Kfgv1         Setll     Fnfv201l
045300040806Do  2c                   Do        *Hival
045400040806     c     Kfgv1         Reade     Fnfv201l
045500040806      * Fine file esco
045600040806     c                   If        %Eof(Fnfv201l)
045700040806     c                   Leave
045800040806     c                   EndIf
045900040806      * Controllo se già scritto FNFV2
046000040806     c     Kfv2          Setll     Fnfv2new                               32
046100040806     c  n32              Write     Fnfv2n
046200040806    2c                   EndDo
046300040806      *-- Leggo FNFVD
046400040806     c     Kfgv1         Setll     Fnfvd01l
046500040806Do  2c                   Do        *Hival
046600040806     c     Kfgv1         Reade     Fnfvd01l
046700040806      * Fine file esco
046800040806     c                   If        %Eof(Fnfvd01l)
046900040806     c                   Leave
047000040806     c                   EndIf
047100040806      * Controllo se già scritto FNFVD
047200040806     c     Kfvd          Setll     Fnfvdnew                               32
047300040806     c  n32              Write     Fnfvdn
047400040806    2c                   EndDo
047500040806      * Controllo se già scritto FNFGV
047600040806     c     Kfgv          Setll     Fnfgvnew                               32
047700040806     c  n32              Write     Fnfgvn
047800040806
047900040806     c                   EndSr
048000040806     C*-------------------------------------------------------------------
048100040806     C* scrivo tutti i file relativi a fnfva
048200040806     C*-------------------------------------------------------------------
048300040806     c     WRITEFva      BEGSR
048400040806     c                   eval      klnp=fvalnp
048500040806     c                   eval      klai=fvalai
048600040806     c                   eval      knfv5=fvanfv
048700040806      *-- Leggo FNFWA
048800040806     c     Kfwa          Chain     Fnfwa01l
048900040806If  2c                   If        %Found(Fnfwa01l)
049000040806      * Controllo se già scritto FNFWA
049100040806     c     Kfwa          Setll     Fnfwanew                               33    2
049200040806     c  n32              Write     Fnfwan
049300040806    2c                   EndIf
049400040806      *-- Leggo FNFV4 Epa = 'A'
049500040806     c                   Eval      KEpa = 'A'
049600040806     c     Kfv4O         Setll     Fnfv401l
049700040806Do  2c                   Do        *Hival
049800040806     c     Kfv4O         Reade     Fnfv401l
049900040806      * Fine file esco
050000040806     c                   If        %Eof(Fnfv401l)
050100040806     c                   Leave
050200040806     c                   EndIf
050300040806      * Controllo se già scritto FNFV4
050400040806     c     Kfv4          Setll     Fnfv4new                               32
050500040806     c  n32              Write     Fnfv4n
050600040806    2c                   EndDo
050700040806      *-- Leggo FNFV4 Epa = 'P'
050800040806     c                   Eval      KEpa = 'P'
050900040806     c                   Clear                   kLai
051000040806     c     Kfv4O         Setll     Fnfv401l
051100040806Do  2c                   Do        *Hival
051200040806     c     Kfv4O         Reade     Fnfv401l
051300040806      * Fine file esco
051400040806     c                   If        %Eof(Fnfv401l)
051500040806     c                   Leave
051600040806     c                   EndIf
051700040806      * Controllo se già scritto FNFV4
051800040806     c     Kfv4          Setll     Fnfv4new                               32
051900040806     c  n32              Write     Fnfv4n
052000040806    2c                   EndDo
052100040806     c
052200040806      * Controllo se già scritto FNFVA
052300040806     c                   Eval      kLai = FvaLai
052400040806     c     Kfva          Setll     Fnfvanew                               32
052500040806     c  n32              Write     Fnfvan
052600040806     c
052700040806     c                   endsr
