000100041230     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200130415      *----------------------------------------------------------------*
000300150917     ftabel00f  if   e           k disk
000400150917     fazorg01l  if   e           k disk
000500161124     fFITGD32l  if   e           k disk
000600161123     ffnevb03l  if   e           k disk
000700161123     ffnarb01l  if   e           k disk
000800161122
000900150917     ffiar501l  if   e           k disk
001000150917     f
001100161124     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
001200161124     Fqsysprt   O    F  132        PRINTER OFLIND(*INOe)
001300150917
001400161124     d cons            s              7  0 dim(4)
001500161124     d AScon           s              6  0 dim(4)
001600161124     d ASnora          s              6  0 dim(4)
001700161124     d asprra          s              6  0 dim(4)
001800161124     d ASdora          s              6  0 dim(4)
001900161124     d ANcon           s              6  0 dim(4)
002000161124     d ANnora          s              6  0 dim(4)
002100161124     d ANprra          s              6  0 dim(4)
002200161124     d ANdora          s              6  0 dim(4)
002300161124     d tcons           s              7  0 dim(4)
002400161124     d tAScon          s              6  0 dim(4)
002500161124     d tASnora         s              6  0 dim(4)
002600161124     d tASprra         s              6  0 dim(4)
002700161124     d tASdora         s              6  0 dim(4)
002800161124     d tANcon          s              6  0 dim(4)
002900161124     d tANnora         s              6  0 dim(4)
003000161124     d tANprra         s              6  0 dim(4)
003100161124     d tANdora         s              6  0 dim(4)
003200041230      *
003300041231      * - Parametri x Controllo profilo utenti
003400041231     d TIBS34DS      e ds                  inz
003500041231      * - Ds di riferimento al file esterno AZUTE00F
003600041231     d AZUTEDS       e ds                  extname(AZUTE00F)
003700041231      * - Ds per dati organigramma
003800041231     d DDatiUte      e ds
003900161123     d
004000161123     D WDAT8           DS
004100161123     D  DATADA                 1      8  0
004200161123     D  DATAA                  9     16  0
004300161123     D  GGL                   17     21  0
004400161123     D tfptfa          DS
004500161123     D  p_tfp                  1      3  0
004600161123     D  p_tfa                  4      6  0
004700041230      *
004800041231     d KPJBA         e ds
004900150917     d dar5emd       e ds
005000150917     d ds2a          e ds
005100150917     d
005200150917     D param           DS
005300161124     D  pdatada                1      8  0
005400161124     D  pdataa                 9     16  0
005500150917     D  fil                   17     19  0
005600150917     D                 DS
005700161124     D  arblnp                 1      3  0
005800161124     D  arbnrs                 4      5  0
005900161124     D  arbnsp                 6     12  0
006000161124     D  arbaas                13     16  0
006100150917     D  wbolla                 1     16
006200150917     d
006300150918     d wdata           s              8  0
006400150918     d tal             s              1
006500150917     d primo           s              1
006600150930     d variaz          s              1
006700150930     d nummin          s              7  0
006800161124     d tot             s              9  0
006900041230      *
007000150917     c     *entry        plist
007100150917     c                   parm                    kpjba
007200150917     c                   movel     kpjbu         param
007300161122     c                   setoff                                       01
007400150917     c
007500161122    1c                   if        fil>0
007600150917     c                   eval      orgfil=fil
007700161122     c     orgfil        chain     azorg01l
007800161123     c                   exsr      leggiEVB
007900161123     c
008000161124     c                   movel     orgdes        de10             10
008100161124     c                   eval      tot=cons(1)+cons(2)+cons(3)+
008200161124     c                                 cons(4)
008300161124     c                   clear                   perc              5 2
008400161124     c                   clear                   consp             7 0
008500161124     c                   if        cons(1) >0 or cons(2)>0
008600161124     c                   eval      consp=cons(1)+cons(2)
008700161124     c                   eval(h)   perc=(consp*100)/tot
008800161124     c                   endif
008900161124     c
009000161122     c                   except    totf
009100161122
009200161122   x1c                   else
009300161122
009400161122     c     *loval        setll     azorg01l
009500150917     c                   read      azorg01l
009600161122    2c                   dow       not %eof(azorg01l)
009700161122    3c                   if        orgfag='F' or orgfag='A'
009800161122    4c                   if        orgfva=' '
009900161123     c                   exsr      leggiEVB
010000161122
010100161124     c                   movel     orgdes        de10             10
010200161124     c                   eval      tot=cons(1)+cons(2)+cons(3)+
010300161124     c                                 cons(4)
010400161124     c                   clear                   perc              5 2
010500161124     c                   clear                   consp             7 0
010600161124     c                   if        cons(1) >0 or cons(2)>0
010700161124     c                   eval      consp=cons(1)+cons(2)
010800161124     c                   eval(h)   perc=(consp*100)/tot
010900161124     c                   endif
011000161122     c                   except    totf
011100161122     c                   seton                                        01
011200161124     c                   z-add     1             xx                3 0
011300161124     c                   do        4             xx
011400161124     c                   add       cons(xx)      tcons(xx)
011500161124     c                   add       ascon(xx)     tascon(xx)
011600161124     c                   add       asnora(xx)    tasnora(xx)
011700161124     c                   add       asprra(xx)    tasprra(xx)
011800161124     c                   add       asnora(xx)    tasnora(xx)
011900161124     c                   add       ancon(xx)     tancon(xx)
012000161124     c                   add       annora(xx)    tannora(xx)
012100161124     c                   add       anprra(xx)    tanprra(xx)
012200161124     c                   add       annora(xx)    tannora(xx)
012300161124     c                   enddo
012400161124     c                   clear                   cons
012500161124     c                   clear                   ascon
012600161124     c                   clear                   asnora
012700161124     c                   clear                   asprra
012800161124     c                   clear                   asdora
012900161124     c                   clear                   ancon
013000161124     c                   clear                   annora
013100161124     c                   clear                   anprra
013200161124     c                   clear                   andora
013300161122    4c                   endif
013400161122    3c                   endif
013500161122     c
013600150917     c                   read      azorg01l
013700161122    2c                   enddo
013800161124     c
013900161124     c* eseguo totali finali
014000161124     c                   clear                   orgfil
014100161124     c                   eVal      de10='TotAzienda'
014200161124     c                   z-add     1             xx
014300161124     c                   do        4             xx
014400161124     c                   add       tcons(xx)     cons(xx)
014500161124     c                   add       tascon(xx)    ascon(xx)
014600161124     c                   add       tasnora(xx)   asnora(xx)
014700161124     c                   add       tasprra(xx)   asprra(xx)
014800161124     c                   add       tasnora(xx)   asnora(xx)
014900161124     c                   add       tancon(xx)    ancon(xx)
015000161124     c                   add       tannora(xx)   annora(xx)
015100161124     c                   add       tanprra(xx)   anprra(xx)
015200161124     c                   add       tannora(xx)   annora(xx)
015300161124     c                   enddo
015400161124     c
015500161124     c                   eval      tot=cons(1)+cons(2)+cons(3)+
015600161124     c                                 cons(4)
015700161124     c                   clear                   perc              5 2
015800161124     c                   clear                   consp             7 0
015900161124     c                   if        cons(1) >0 or cons(2)>0
016000161124     c                   eval      consp=cons(1)+cons(2)
016100161124     c                   eval(h)   perc=(consp*100)/tot
016200161124     c                   endif
016300161124     c                   except    totf
016400161124     c
016500161122    1c                   endif
016600150917     c
016700130415      *
016800041231     c                   seton                                        lr
016900041230      *
017000041231      *---------------------------------------------------------------*
017100041231      * Routine Iniziale                                              *
017200041231      *---------------------------------------------------------------*
017300041231     c     *InzSR        BegSR
017400041231      *
017500041231      * Reperisco dati job
017600041231     c                   exsr      DatiJob
017700150917     c
017800150917     c     kbolla        klist
017900161123     c                   kfld                    evbaas
018000161123     c                   kfld                    evblnp
018100161123     c                   kfld                    evbnrs
018200161123     c                   kfld                    evbnsp
018300161122     c     kar5          klist
018400161123     c                   kfld                    arbaas
018500161122     c                   kfld                    arblnp
018600161122     c                   kfld                    arbnrs
018700161122     c                   kfld                    arbnsp
018800161122     c                   kfld                    ktrc              3
018900150917     c                   eval      ktrc='EMD'
019000150917     c     ktab          klist
019100150917     c                   kfld                    codut             1 0
019200150917     c                   kfld                    kcod              2
019300150917     c                   z-add     1             codut
019400150917     c
019500041231     C                   ENDSR
019600041231      *
019700041231      *---------------------------------------------------------------*
019800041231      * Reperimento Dati del job (Utente/Operativi)                   *
019900041231      *---------------------------------------------------------------*
020000041231     c     DatiJob       BegSr
020100041231      *
020200041231     c     *dtaara       define    §azute        azuteds
020300041231     c     *dtaara       define    §datiute      ddatiute
020400041231      *
020500041231     c                   in(E)     *dtaara
020600041231     c                   IF        %ERROR or RSUT = *blanks
020700041231     c                   clear                   Tibs34Ds
020800041231     c                   call      'TIBS34R'
020900041231     c                   parm                    Tibs34Ds
021000041231     c                   in        *dtaara
021100041231     c                   ENDIF
021200041231     C                   ENDSR
021300150917      *---------------------------------------------------------------*
021400161122      * lettura
021500150917      *---------------------------------------------------------------*
021600161123     c     LeggiEVB      BegSr
021700161123     c     kevb          klist
021800161123     c                   kfld                    orgfil
021900161123     c                   kfld                    kcev              3
022000161124     c     kevb1         klist
022100161124     c                   kfld                    orgfil
022200161124     c                   kfld                    kcev              3
022300161124     c                   kfld                    pdatada
022400161124     c                   eval      kcev='RIC'
022500161123     c
022600161124     c     kevb1         setll     fnevb03l
022700161123     c     kevb          reade     fnevb03l
022800161124    1c                   dow       not %eof(fnevb03l)
022900150917     c
023000161123     c* elaboro solo tra le date
023100161124     c                   if        evbdev>=pdatada and evbdev<=pdataa
023200150917     c                   exsr      elabolla
023300161123     c                   endif
023400150917     c
023500161123     c     kevb          reade     fnevb03l
023600150917     c                   enddo
023700150917     c
023800150917     c                   endsr
023900150917      *---------------------------------------------------------------*
024000150917      * lettura bolle
024100150917      *---------------------------------------------------------------*
024200150917     c     elabolla      BegSr
024300161123     c
024400161123     c     kbolla        chain     fnarb01l
024500161124    1c                   if        %found(fnarb01l)
024600161122
024700150917     c* possibilità di alert
024800150917     c     kar5          chain     fiar501l
024900150917    2c                   if        not %found(fiar501l)
025000150917     c                   eval      tal='N'
025100150917   x2c                   else
025200150917     c                   movel     ar5uni        dar5emd
025300150921    3c                   if        §AR5smp='S'         or §ar5mmp='S'
025400150917     c                   eval      tal='S'
025500150917     c                   else
025600150917     c                   eval      tal='N'
025700150917    3c                   endif
025800150917    2c                   endif
025900161123     c*
026000161123     c* conto i giorni tra la data di consegna e la data del RIC
026100161123     c* se non consegnata imposto giorni max
026200161124    1c                   if        arbdcm=0 or arbcca<>' '
026300161123     c                   eval      XX=4
026400161124   x1c                   else
026500161123     c                   clear                   wdat8
026600161123     c                   clear                   tfptfa
026700161123     c                   eval      datada=arbdcm
026800161123     c                   eval      dataa=evbdev
026900161123     c                   eval      p_tfp=arbtfp
027000161123     c                   eval      p_tfa=arbtfa
027100161123     c                   call      'XSRLAV8'
027200161123     c                   parm                    wdat8
027300161123     c                   parm                    tfptfa
027400161123     c                   eval      xx=ggl
027500161124    2c                   if        ggl=0
027600161123     c                   eval      xx=1
027700161124    2c                   endif
027800161124    2c                   if        ggl>3
027900161123     c                   eval      xx=4
028000161124    2c                   endif
028100161124    1c                   endif
028200161123     c
028300161123     c* cerco se ha una r.a. telefonata prima del ric o >=al ric
028400161123     c     ktgd          klist
028500161123     c                   kfld                    ktor              1
028600161123     c                   kfld                    kogg             20
028700161123     c
028800161123     c                   clear                   doporic           1
028900161123     c                   movel(P)  wbolla        kogg
029000161123     c                   eval      ktor='S'
029100161124     c     ktgd          setll     fitgd32l
029200161124     c     ktgd          reade     fitgd32l
029300161124    2c                   dow       not %eof(fitgd32l)
029400161123     c
029500161123     c                   if        tgdtco='T' and %subst(tgdflo:1:1)<>'R' and
029600161123     c                             %subst(tgdflo:1:1)<>'C'
029700161124     c
029800161124     c                   if        tgdtch='D' or tgdtch='M'
029900161124     c                   eval      doporic=' '
030000161124     c***                eval      doporic='N'
030100161123     c* Verifico l'orario
030200161124     c                   movel     tgdorr        wora              4 0
030300161124    5c***                if        tgddac>evbdev
030400161124    5c                   if        tgddac=evbdev
030500161124     c                   eval      doporic='N'
030600161124  x5 c**                 else
030700161124    6c**                 if        tgddar=evbdev and wora >=evbhev
030800161124     c**                 eval      doporic='S'
030900161124    6c**                 endif
031000161123    5c                   endif
031100161123     c                   leave
031200161123     c                   endif
031300161124     c                   endif
031400161123     c
031500161124     c     ktgd          reade     fitgd32l
031600161124    2c                   enddo
031700161123     c
031800161124     c                   except    detf
031900150929     c
032000161123     c* totali
032100161123     c*1)  consegnate
032200161123     c                   add       1             cons(xx)
032300161124    2c                   if        tal='S'
032400161123     c                   add       1             AScon(xx)
032500161123     c* senza r.a.
032600161123     c                   if        doporic=' '
032700161123     c                   add       1             ASnora(xx)
032800161123     c                   endif
032900161123     c* conr.a. prima del ric
033000161123     c                   if        doporic='N'
033100161123     c                   add       1             ASPRra(xx)
033200161123     c                   endif
033300161123     c* conr.a. dopo  il  ric
033400161123     c                   if        doporic='S'
033500161123     c                   add       1             ASDOra(xx)
033600161123     c                   endif
033700161123     c
033800161124   x2c                   else
033900161124     c                   add       1             ANcon(xx)
034000161123     c* senza r.a.
034100161123     c                   if        doporic=' '
034200161123     c                   add       1             ANnora(xx)
034300161123     c                   endif
034400161123     c* conr.a. prima del ric
034500161123     c                   if        doporic='N'
034600161123     c                   add       1             ANPRra(xx)
034700161123     c                   endif
034800161123     c* conr.a. dopo  il  ric
034900161123     c                   if        doporic='S'
035000161123     c                   add       1             ANDOra(xx)
035100161123     c                   endif
035200161124    2c                   endif
035300161124     c*
035400161123     c
035500161124    1c                   endif
035600161122     c
035700161122     c                   ENDSR
035800150917     c
035900161124     Oqsysprt   E            detf        1
036000161124     O                       arbaas        z  +   1
036100161124     O                       arblnp        z  +   1
036200161124     O                       arbnrs        z  +   1
036300161124     O                       arbnsp        z  +   1
036400161124     O                       arbdcm        z  +   1
036500161124     O                       tal              +   1
036600161124     O                       doporic          +   1
036700161124     o
036800161124     Oprtf198   E            totf        1
036900161122     O              n01                             'Dal'
037000161124     O              n01      pdatada          +   1 '    /  /  '
037100161122     O              n01                       +   1 'al '
037200161124     O              n01      pdataa           +   1 '    /  /  '
037300161124     O              n01                       +   1 'fil.Arr'
037400161122     o                       orgfil           +   1
037500161124     o                       de10             +   1
037600161124     O                                        +   1 'bolle con RIC'
037700161124     o                       tot           2  +   1
037800161124     O                                        +   1 '% consegnate max 1 gg'
037900161124     o                       perc          2  +   1
038000161124     o                                        +   0 '%'
038100161124     O          E            totf        1
038200161124     O                                        +  10 'CONS stesG'
038300161124     o                       cons(1)       z  +   1
038400161124     O                                        +   1 'SI ALERT'
038500161124     o                       ASCON(1)      z  +   1
038600161124     O**                                      +   1 'dCui no RA'
038700161124     o**                     ASnora(1)     z  +   1
038800161124     O                                        +   1 'dCui RA = RIC'
038900161124     o                       ASprra(1)     z  +   1
039000161124     O**                                      +   1 'dCui RA dopoRIC'
039100161124     o**                     ASdora(1)     z  +   1
039200161124     O                                        +   1 'NO ALERT'
039300161124     o                       AnCON(1)      z  +   1
039400161124     O**                                      +   1 'dCui no RA'
039500161124     o**                     Annora(1)     z  +   1
039600161124     O                                        +   1 'dCui RA = RIC'
039700161124     o                       Anprra(1)     z  +   1
039800161124     O**                                      +   1 'dCui RA dopoRIC'
039900161124     o**                     Andora(1)     z  +   1
040000161124     O          E            totf        1
040100161124     O                                        +  10 'CONS 1gg D'
040200161124     o                       cons(2)       z  +   1
040300161124     O                                        +   1 'SI ALERT'
040400161124     o                       ASCON(2)      z  +   1
040500161124     O**                                      +   1 'dCui no RA'
040600161124     o**                     ASnora(2)     z  +   1
040700161124     O                                        +   1 'dCui RA = RIC'
040800161124     o                       ASprra(2)     z  +   1
040900161124     O**                                      +   1 'dCui RA dopoRIC'
041000161124     o***                    ASdora(2)     z  +   1
041100161124     O                                        +   1 'NO ALERT'
041200161124     o                       AnCON(2)      z  +   1
041300161124     O**                                      +   1 'dCui no RA'
041400161124     o**                     Annora(2)     z  +   1
041500161124     O                                        +   1 'dCui RA = RIC'
041600161124     o                       Anprra(2)     z  +   1
041700161124     O**                                      +   1 'dCui RA dopoRIC'
041800161124     o**                     Andora(2)     z  +   1
041900161124     O          E            totf        1
042000161124     O                                        +  10 'CONS 2gg D'
042100161124     o                       cons(3)       z  +   1
042200161124     O                                        +   1 'SI ALERT'
042300161124     o                       ASCON(3)      z  +   1
042400161124     O**                                      +   1 'dCui no RA'
042500161124     o**                     ASnora(3)     z  +   1
042600161124     O                                        +   1 'dCui RA = RIC'
042700161124     o                       ASprra(3)     z  +   1
042800161124     O**                                      +   1 'dCui RA dopoRIC'
042900161124     o**                     ASdora(3)     z  +   1
043000161124     O                                        +   1 'NO ALERT'
043100161124     o                       AnCON(3)      z  +   1
043200161124     O**                                      +   1 'dCui no RA'
043300161124     o**                     Annora(3)     z  +   1
043400161124     O                                        +   1 'dCui RA = RIC'
043500161124     o                       Anprra(3)     z  +   1
043600161124     O**                                      +   1 'dCui RA dopoRIC'
043700161124     o**                     Andora(3)     z  +   1
043800161124     O          E            totf        1
043900161124     O                                        +  10 'Oltre/non '
044000161124     o                       cons(4)       z  +   1
044100161124     O                                        +   1 'SI ALERT'
044200161124     o                       ASCON(4)      z  +   1
044300161124     O**                                      +   1 'dCui no RA'
044400161124     o**                     ASnora(4)     z  +   1
044500161124     O                                        +   1 'dCui RA = RIC'
044600161124     o                       ASprra(4)     z  +   1
044700161124     O**                                      +   1 'dCui RA dopoRIC'
044800161124     o**                     ASdora(4)     z  +   1
044900161124     O                                        +   1 'NO ALERT'
045000161124     o                       AnCON(4)      z  +   1
045100161124     O**                                      +   1 'dCui no RA'
045200161124     o**                     Annora(4)     z  +   1
045300161124     O                                        +   1 'dCui RA = RIC'
045400161124     o                       Anprra(4)     z  +   1
045500161124     O**                                      +   1 'dCui RA dopoRIC'
045600161124     o**                     Andora(4)     z  +   1
