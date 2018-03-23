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
001900070430     fFidstNEW  o  a e           k Disk    Rename(fidst000:fidstn) prefix(ids)
002000070430     fFidst3NW  o  a e           k Disk    Rename(fidst300:fidst3n)
002100070430     fFidst4NW  o  a e           k Disk    Rename(fidst400:fidst4n)
002200070725     fFidst5NW  o  a e           k Disk    Rename(fidst500:fidst5n)
002300070725     fFidst6NW  o  a e           k Disk    Rename(fidst600:fidst6n)
002400090209     fFidsfNEW  o  a e           k Disk    Rename(fidsf000:fidsfn)
002500070123     fFnbrvNEW  o  a e           k Disk    Rename(fnbrv000:fnbrvn)
002600070123     fFnbrvENW  o  a e           k Disk    Rename(fnbrv000:fnbrvEn)
002700040806     fFNFVANEW  if a e           k Disk    Rename(fnfva000:fnfvan)
002800040806     fFNFwaNEW  if a e           k Disk    Rename(fnfwa000:fnfwan)
002900070430     fFidst09l  if   e           k Disk    prefix(ids)
003000070430     fFidst31l  if   e           k Disk
003100070430     fFidst41l  if   e           k Disk
003200070725     fFidst51l  if   e           k Disk
003300070725     fFidst61l  if   e           k Disk
003400090209     fFidsf02l  if   e           k Disk
003500070123     fFnbrv01l  if   e           k Disk
003600070123     fFnbrv02L  if   e           k Disk    Rename(fnbrv000:fnbrv002)
003700070123     fFnbrve1l  if   e           k Disk    Rename(fnbrv000:fnbrve)
003800040806     d*
003900040806     D* Schiera p.o. da trasferire
004000040806     D  SKPO           s              3    dim(500)
004100040806     d*
004200040806     d KLIN            s                   like(BRVLNP)
004300040806     d KLINSPU         s                   like(BRVLNP)
004400040806     d KNpg            s                   like(BRVnpg)
004500040806     d KNfv            s                   like(BRVnfv)
004600040806     d KNfv5           s                   like(fgvnfv)
004700040806     d Kfgs            s                   like(BRVfgs)
004800040806     d Klnp            s                   like(BRVLNP)
004900040806     d kEpa            s                   Like(Fv4Epa)
005000040806     d klai            s                   Like(Fvalai)
005100040806     D KPJBA         E DS
005200040806     c*------------------------------------------------------------------
005300040806     C     *ENTRY        PLIST
005400040806     C                   PARM                    KPJBA
005500040806     C                   parm                    PARAM          1500
005600040806     c
005700040806     c     ' ':'0'       xlate     param         param
005800040806     c
005900040806     c                   movea     param         skpo
006000020927
006100070430      * Fidst
006200070430     c     Kidst         Klist
006300070430     c                   Kfld                    FvvFgs
006400070430     c                   Kfld                    FvvNpg
006500070430     c                   Kfld                    FvvNfv6           6 0
006600070430      * Fidst3
006700070430     c     Kidst3        Klist
006800070430     c                   Kfld                    FvvFgs
006900070430     c                   Kfld                    FvvNfv6           6 0
007000090209      * Fidsf
007100090209     c     Kidsf         Klist
007200090209     c                   Kfld                    FvvNpg
007300090209     c                   Kfld                    FvvNfv6
007400090209     c                   Kfld                    FvvFgs
007500020927      * Fnfvv
007600040806     c     Kfvv          Klist
007700040806     c                   Kfld                    kNpg
007800040806     c                   Kfld                    kNfv
007900040806     c                   Kfld                    klin
008000040806     c     KfvvO         Klist
008100040806     c                   Kfld                    fvvnpg
008200040806     c                   Kfld                    fvvnfv
008300040806     c                   Kfld                    fvvfgs
008400040806     c     Kfvv1         Klist
008500040806     c                   Kfld                    kNpg
008600040806     c                   Kfld                    kNfv
008700040806     c                   Kfld                    klin
008800040806     c                   Kfld                    KLINSPU
008900040806     c     Kfgv          Klist
009000040806     c                   Kfld                    kNfv5
009100040806     c                   Kfld                    klin
009200040806     c     Kfgv1         Klist
009300040806     c                   Kfld                    kLin
009400040806     c                   Kfld                    kNfv5
009500040806     c     Kfv4          Klist
009600040806     c                   Kfld                    Fv4Lnp
009700040806     c                   Kfld                    Fv4Nfv
009800040806     c                   Kfld                    Fv4Epa
009900040806     c                   Kfld                    Fv4Lai
010000040806     c                   Kfld                    Fv4Trc
010100040806     c                   Kfld                    Fv4Not
010200040806     c     Kfv2          Klist
010300040806     c                   Kfld                    Fv2Lnp
010400040806     c                   Kfld                    Fv2Nfv
010500040806     c                   Kfld                    Fv2Epa
010600040806     c                   Kfld                    Fv2Lai
010700040806     c                   Kfld                    Fv2Tdh
010800040806     c     Kfvd          Klist
010900040806     c                   Kfld                    FvdLnp
011000040806     c                   Kfld                    FvdNfv
011100040806     c                   Kfld                    FvdLna
011200040806     c                   Kfld                    FvdTrc
011300040806     c     Kfwa          Klist
011400040806     c                   Kfld                    FvaLnp
011500040806     c                   Kfld                    FvaNfv
011600040806     c                   Kfld                    FvaLai
011700040806     c     Kfv4O         Klist
011800040806     c                   Kfld                    kLnp
011900040806     c                   Kfld                    kNfv5
012000040806     c                   Kfld                    KEpa
012100040806     c                   Kfld                    klai
012200040806     c     Kfva          Klist
012300040806     c                   Kfld                    FvaNpg
012400040806     c                   Kfld                    FvaNfa
012500040806     c                   Kfld                    KLai
012600040806     c                   Kfld                    KLnp
012700040806     c                   Kfld                    KNfv5
012800040806     c*------------------------------------------------------------------
012900040806     c                   z-add     1             x                 4 0
013000040806    1c                   dow       skpo(x)>*zeros
013100040806     c                   movel     skpo(X)       klin
013200040806     C     klin          SETLL     FNfvv07l
013300040806     C     klin          READe     FNfvv07l                               30
013400040806     C*
013500040806    2C     *IN30         DOWEQ     *OFF
013600040806     c
013700040806     c* Elaboro e prendo le spunte
013800040806     c                   z-add     fvvnpg        knpg
013900040806     c                   z-add     fvvnfv        knfv
014000040806     c                   EXSR      SPUNTE
014100040806     c
014200040806     c* Elaboro tutti i fogli vari del p.o. con relative spunte
014300040806     c                   write     fnfvvn                               99
014400040806     c* Se cat. 4 carico anche DST
014500040806     c                   if        fvvnpg=4
014600070430      * nuove distinte
014700070430     c                   z-add     fvvnfv        fvvNFV6
014800070430     c     kidst         chain     fidst09l                           32
014900070430     c  n32              do
015000070430     c                   write     fidstn                               99
015100070430      * nuove distinte estensioni
015200070430     c     kidst3        setll     fidst31l
015300070430     c     kidst3        reade     fidst31l
015400070430     c                   dow       not %Eof(fidst31l)
015500070430     c                   write     fidst3n                              99
015600070430     c     kidst3        reade     fidst31l
015700070430     c                   enddo
015800070430      * nuove distinte estensioni
015900070430     c     kidst         setll     fidst41l
016000070430     c     kidst         reade     fidst41l
016100070430     c                   dow       not %Eof(fidst41l)
016200070430     c                   write     fidst4n                              99
016300070430     c     kidst         reade     fidst41l
016400070430     c                   enddo
016500070725      * nuove distinte estensioni
016600070725     c     kidst3        setll     fidst51l
016700070725     c     kidst3        reade     fidst51l
016800070725     c                   dow       not %Eof(fidst51l)
016900070725     c                   write     fidst5n                              99
017000070725     c     kidst3        reade     fidst51l
017100070725     c                   enddo
017200070725      * nuove distinte estensioni
017300070725     c     kidst         setll     fidst61l
017400070725     c     kidst         reade     fidst61l
017500070725     c                   dow       not %Eof(fidst61l)
017600070725     c                   write     fidst6n                              99
017700070725     c     kidst         reade     fidst61l
017800070725     c                   enddo
017900090209      * nuove distinte estensioni fasi
018000090209     c     kidsf         setll     fidsf02l
018100090209     c     kidsf         reade     fidsf02l
018200090209     c                   dow       not %Eof(fidsf02l)
018300090209     c                   write     fidsfn                               99
018400090209     c     kidsf         reade     fidsf02l
018500090209     c                   enddo
018600070430     c                   end
018700040806     c                   endif
018800040806     c
018900040806     C     klin          READe     FNfvv07l                               30
019000040806     c                   enddo
019100040806     c*
019200040806     c* fogli viaggio partenza
019300040806     C     klin          SETLL     FNfgv02l
019400040806     C     klin          READe     FNfgv02l                               30
019500040806     C*
019600040806    2C     *IN30         DOWEQ     *OFF
019700040806     c
019800040806     c* Elaboro e prendo le spunte
019900040806     c                   z-add     1             knpg
020000040806     c                   z-add     fgvnfv        knfv
020100040806     c                   EXSR      SPUNTE
020200040806     c* Scrivo il foglio
020300040806     c                   EXSR      WRITEFGV
020400040806     c*
020500040806     C     klin          READe     FNfgv02l                               30
020600040806     c                   enddo
020700040806     c*
020800040806     c* fogli viaggio partenza in arrivo
020900040806     C     klin          SETLL     FNfva05l
021000040806     C     klin          READe     FNfva05l                               30
021100040806     C*
021200040806    2C     *IN30         DOWEQ     *OFF
021300040806     c                   z-add     fvanfv        knfv
021400040806     c*
021500040806     C* SE FVALNP È TRA I P.O. DA TRASFERIFE, le spunte le
021600040806     c*  elaboro con p.o. = a fvalnp
021700040806     c                   movel     fvalnp        w003a             3
021800040806     c     w003a         lookup    skpo                                   32
021900040806     c                   if        not *in32
022000040806     c
022100040806     c* Elaboro e prendo le spunte
022200040806     c                   z-add     1             knpg
022300040806     c                   EXSR      SPUNTE
022400040806     c                   endif
022500040806     c*
022600040806     c                   EXSR      WRITEFVA
022700040806     c*
022800040806     C     klin          READe     FNfva05l                               30
022900040806     c                   enddo
023000040806     C
023100040806     C                   ADD       1             X
023200040806     c                   enddo
023300040806     c***
023400040806     c* Leggo tutti i fogli non dei p.o. trasferiti ma con una spunta
023500040806     c*  riguardante i p.o. trasferiti
023600040806     c***
023700040806     c     *loval        setll     fnfvv07l
023800040806     c                   read      fnfvv07l                               30
023900040806     c
024000040806     c                   dow       not *in30
024100040806     c                   eval      klin=fvvfgs
024200040806     c                   movel     fvvfgs        w003a
024300040806     c
024400040806     c     w003a         lookup    skpo                                   32
024500040806     c
024600040806     c* Se P.o. gestione non elaborato: lo leggo
024700040806     c                   if        not *in32
024800040806     C
024900040806     c     klin          setll     fnfvv07l
025000040806     c     klin          reade     fnfvv07l                               30
025100040806     c                   dow       not *in30
025200040806     c
025300040806     c* Elaboro solo le spunte con lnp o lna = al p.o. trasferito
025400040806     c                   z-add     fvvnpg        knpg
025500040806     c                   z-add     fvvnfv        knfv
025600040806     c                   exsr      SPUNTEMIE
025700040806     c
025800040806     c* Se estrapolata almeno una spunta, scrivo il foglio
025900040806     c                   if        welaspu='S'
026000040806     c     kfvvo         setll     fnfvvnew                               32
026100040806     c*
026200040806     c                   if        not *in32
026300040806     c                   write     fnfvvn                               99
026400040806     c* Se cat. 4 carico anche DST
026500040806     c                   if        fvvnpg=4
026600070430     c                   z-add     fvvnfv        fvvNFV6
026700070430     c     kidst         chain     fidst09l                           32
026800070430     c  n32              do
026900070430     c                   write     fidstn                               99
027000070430      * nuove distinte estensioni
027100070430     c     kidst3        setll     fidst31l
027200070430     c     kidst3        reade     fidst31l
027300070430     c                   dow       not %Eof(fidst31l)
027400070430     c                   write     fidst3n                              99
027500070430     c     kidst3        reade     fidst31l
027600070430     c                   enddo
027700070430      * nuove distinte estensioni
027800070430     c     kidst         setll     fidst41l
027900070430     c     kidst         reade     fidst41l
028000070430     c                   dow       not %Eof(fidst41l)
028100070430     c                   write     fidst4n                              99
028200070430     c     kidst         reade     fidst41l
028300070430     c                   enddo
028400070725      * nuove distinte estensioni
028500070725     c     kidst3        setll     fidst51l
028600070725     c     kidst3        reade     fidst51l
028700070725     c                   dow       not %Eof(fidst51l)
028800070725     c                   write     fidst5n                              99
028900070725     c     kidst3        reade     fidst51l
029000070725     c                   enddo
029100070725      * nuove distinte estensioni
029200070725     c     kidst         setll     fidst61l
029300070725     c     kidst         reade     fidst61l
029400070725     c                   dow       not %Eof(fidst61l)
029500070725     c                   write     fidst6n                              99
029600070725     c     kidst         reade     fidst61l
029700070725     c                   enddo
029800090209      * nuove distinte estensioni fasi
029900090209     c     kidsf         setll     fidsf02l
030000090209     c     kidsf         reade     fidsf02l
030100090209     c                   dow       not %Eof(fidsf02l)
030200090209     c                   write     fidsfn                               99
030300090209     c     kidsf         reade     fidsf02l
030400090209     c                   enddo
030500070430     c                   end
030600040806     c                   endif
030700040806     c                   endif
030800040806     c                   endif
030900040806     C
031000040806     c     klin          reade     fnfvv07l                               30
031100040806     c                   enddo
031200040806     c                   endif
031300040806     c**
031400040806     c* elaboro nuovo p.o. gestione
031500040806     c     klin          setgt     fnfvv07l
031600040806     c                   read      fnfvv07l                               30
031700040806     c                   enddo
031800040806     C**
031900040806     C**
032000040806     c     *loval        setll     fnfGV02l
032100040806     c                   read      fnfGv02l                               30
032200040806     c
032300040806     c                   dow       not *in30
032400040806     c                   eval      klin=fGvlnp
032500040806     c                   movel     fgvlnp        w003a
032600040806     c
032700040806     c     w003a         lookup    skpo                                   32
032800040806     c
032900040806     c* Se P.o. gestione non elaborato: lo leggo
033000040806     c                   if        not *in32
033100040806     C
033200040806     c     klin          setll     fnfGv02l
033300040806     c     klin          reade     fnfGv02l                               30
033400040806     c                   dow       not *in30
033500040806     c
033600040806     c* Elaboro solo le spunte con lnp o lna = al p.o. trasferito
033700040806     c                   z-add     1             knpg
033800040806     c                   z-add     fGvnfv        knfv
033900040806     c                   exsr      SPUNTEMIE
034000040806     c
034100040806     c* Se estrapolata almeno una spunta, scrivo il foglio
034200040806     c                   if        welaspu='S'
034300040806     c                   EXSR      WRITEFGV
034400040806     c                   endif
034500040806     C
034600040806     c     klin          reade     fnfGV02l                               30
034700040806     c                   enddo
034800040806     c                   endif
034900040806     c**
035000040806     c* elaboro nuovo p.o. gestione
035100040806     c     klin          setgt     fnfGv02l
035200040806     c                   read      fnfGv02l                               30
035300040806     c                   enddo
035400040806     c
035500021001     c                   Eval      *InLr = *On
035600040806     C*-------------------------------------------------------------------
035700040806     C* SCRIVO LE SPUNTE DEI FOGLI LETTI DA ESTRAPOLARE
035800040806     C*-------------------------------------------------------------------
035900040806     c     SPUNTE        begsr
036000070123     C     kfvv          SETLL     Fnbrv01l
036100070123     C     kfvv          READe     Fnbrv01l                               31
036200040806     c                   dow       not *in31
036300070123     c                   write     fnbrvn                               99
036400040806     c*
036500070123     C     kfvv          READe     Fnbrv01l                               31
036600040806     c                   enddo
036700070123     C     kfvv          SETLL     Fnbrve1l
036800070123     C     kfvv          READe     Fnbrve1l                               31
036900040806     c                   dow       not *in31
037000070123     c                   write     fnbrven                              99
037100040806     c*
037200070123     C     kfvv          READe     Fnbrve1l                               31
037300040806     c                   enddo
037400040806     c                   ENDSR
037500040806     C*-------------------------------------------------------------------
037600040806     C* ESTRAPOLO LE SPUNTE DEI FOGLI LETTI DELLE LINEE DA ELABORARE
037700040806     C*-------------------------------------------------------------------
037800040806     c     SPUNTEMIE     begsr
037900040806     c                   clear                   welaspu           1
038000040806     c                   z-add     1             Y                 4 0
038100040806    1c                   dow       skpo(y)>*zeros
038200040806     c                   movel     skpo(y)       klinSPU
038300040806     C* PRIMA LA LEGGO COME LNP
038400070123     C     kfvv1         SETLL     Fnbrv01l
038500070123     C     kfvv1         READe     Fnbrv01l                               31
038600040806     c                   dow       not *in31
038700070123     c                   write     fnbrvn                               99
038800040806     C                   EVAL      WELASPU='S'
038900040806     c*
039000070123     C     kfvv1         READe     Fnbrv01l                               31
039100040806     c                   enddo
039200040806     C* POI   LA LEGGO COME LNA
039300070123     C     kfvv1         SETLL     Fnbrv02l
039400070123     C     kfvv1         READe     Fnbrv02l                               31
039500040806     c                   dow       not *in31
039600070123     c                   write     fnbrvn                               99
039700040806     C                   EVAL      WELASPU='S'
039800040806     c*
039900070123     C     kfvv1         READe     Fnbrv02l                               31
040000040806     c                   enddo
040100040806     c
040200040806     c                   add       1             y
040300040806     c                   enddo
040400040806     C
040500070123     C     kfvv          SETLL     Fnbrve1l
040600070123     C     kfvv          READe     Fnbrve1l                               31
040700040806     c                   dow       not *in31
040800040806     C                   IF        BRVLNP=KLINSPU  OR BRVLNA=KLINSPU
040900070123     c                   write     fnbrven                              99
041000040806     C                   EVAL      WELASPU='S'
041100040806     C                   ENDIF
041200040806     c*
041300070123     C     kfvv          READe     Fnbrve1l                               31
041400040806     c                   enddo
041500040806     c                   ENDSR
041600040806     C*-------------------------------------------------------------------
041700040806     C* scrivo tutti i file relativi a fnfgv
041800040806     C*-------------------------------------------------------------------
041900040806     c     WRITEFGV      BEGSR
042000041111     c                   z-add     fgvnfv        knfv5
042100040806     c*
042200040806      *-- Leggo FNFGW
042300040806     c     Kfgv          Chain     Fnfgw01l
042400040806If  2c                   If        %Found(Fnfgw01l)
042500040806      * Controllo se già scritto FNFGW
042600040806     c     Kfgv          Setll     Fnfgwnew                               32
042700040806     c  n32              Write     Fnfgwn
042800040806    3c                   EndIf
042900040806      *-- Leggo FNFV4
043000040806     c     Kfgv1         Setll     Fnfv401l
043100040806Do  2c                   Do        *Hival
043200040806     c     Kfgv1         Reade     Fnfv401l
043300040806      * Fine file esco
043400040806     c                   If        %Eof(Fnfv401l)
043500040806     c                   Leave
043600040806     c                   EndIf
043700040806      * Controllo se già scritto FNFV4
043800040806     c     Kfv4          Setll     Fnfv4NEW                               32
043900040806     c  n32              Write     Fnfv4n
044000040806    2c                   EndDo
044100040806      *-- Leggo FNFV2
044200040806     c     Kfgv1         Setll     Fnfv201l
044300040806Do  2c                   Do        *Hival
044400040806     c     Kfgv1         Reade     Fnfv201l
044500040806      * Fine file esco
044600040806     c                   If        %Eof(Fnfv201l)
044700040806     c                   Leave
044800040806     c                   EndIf
044900040806      * Controllo se già scritto FNFV2
045000040806     c     Kfv2          Setll     Fnfv2new                               32
045100040806     c  n32              Write     Fnfv2n
045200040806    2c                   EndDo
045300040806      *-- Leggo FNFVD
045400040806     c     Kfgv1         Setll     Fnfvd01l
045500040806Do  2c                   Do        *Hival
045600040806     c     Kfgv1         Reade     Fnfvd01l
045700040806      * Fine file esco
045800040806     c                   If        %Eof(Fnfvd01l)
045900040806     c                   Leave
046000040806     c                   EndIf
046100040806      * Controllo se già scritto FNFVD
046200040806     c     Kfvd          Setll     Fnfvdnew                               32
046300040806     c  n32              Write     Fnfvdn
046400040806    2c                   EndDo
046500040806      * Controllo se già scritto FNFGV
046600040806     c     Kfgv          Setll     Fnfgvnew                               32
046700040806     c  n32              Write     Fnfgvn
046800040806
046900040806     c                   EndSr
047000040806     C*-------------------------------------------------------------------
047100040806     C* scrivo tutti i file relativi a fnfva
047200040806     C*-------------------------------------------------------------------
047300040806     c     WRITEFva      BEGSR
047400040806     c                   eval      klnp=fvalnp
047500040806     c                   eval      klai=fvalai
047600040806     c                   eval      knfv5=fvanfv
047700040806      *-- Leggo FNFWA
047800040806     c     Kfwa          Chain     Fnfwa01l
047900040806If  2c                   If        %Found(Fnfwa01l)
048000040806      * Controllo se già scritto FNFWA
048100040806     c     Kfwa          Setll     Fnfwanew                               33    2
048200040806     c  n32              Write     Fnfwan
048300040806    2c                   EndIf
048400040806      *-- Leggo FNFV4 Epa = 'A'
048500040806     c                   Eval      KEpa = 'A'
048600040806     c     Kfv4O         Setll     Fnfv401l
048700040806Do  2c                   Do        *Hival
048800040806     c     Kfv4O         Reade     Fnfv401l
048900040806      * Fine file esco
049000040806     c                   If        %Eof(Fnfv401l)
049100040806     c                   Leave
049200040806     c                   EndIf
049300040806      * Controllo se già scritto FNFV4
049400040806     c     Kfv4          Setll     Fnfv4new                               32
049500040806     c  n32              Write     Fnfv4n
049600040806    2c                   EndDo
049700040806      *-- Leggo FNFV4 Epa = 'P'
049800040806     c                   Eval      KEpa = 'P'
049900040806     c                   Clear                   kLai
050000040806     c     Kfv4O         Setll     Fnfv401l
050100040806Do  2c                   Do        *Hival
050200040806     c     Kfv4O         Reade     Fnfv401l
050300040806      * Fine file esco
050400040806     c                   If        %Eof(Fnfv401l)
050500040806     c                   Leave
050600040806     c                   EndIf
050700040806      * Controllo se già scritto FNFV4
050800040806     c     Kfv4          Setll     Fnfv4new                               32
050900040806     c  n32              Write     Fnfv4n
051000040806    2c                   EndDo
051100040806     c
051200040806      * Controllo se già scritto FNFVA
051300040806     c                   Eval      kLai = FvaLai
051400040806     c     Kfva          Setll     Fnfvanew                               32
051500040806     c  n32              Write     Fnfvan
051600040806     c
051700040806     c                   endsr
