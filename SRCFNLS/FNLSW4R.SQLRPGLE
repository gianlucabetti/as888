000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300130918     h*   Conteggio colli di bolle abbinate
000400130305     H*-------------------------------------------------------------------------
000500130304     FFNBLP31L  IF   E           K DISK
000600130304     FFNBLt01L  IF   E           K DISK
000700030422     FAZORG01L  IF   E           K DISK
000800030414     FTABEL00F  IF   E           K DISK
000900130304     FTNTBE01L  IF   E           K DISK
001000130305     Ffnbrv09l  IF   E           K DISK
001100130925     Ffnbrv07l  IF   E           K DISK    rename(fnbrv000:fnbrv007)
001200130305     Fwfpcl01l  uF A E           K DISK
001300130305     FFNLSW4P   O    E             PRINTER OFLIND(*IN01)
001400130919     FFNLSW4P1  O    E             PRINTER OFLIND(*IN02)
001500001218     D*--------------------
001600001218     D* DS ESTERNE
001700001218     D*--------------------
001800900517     D KPJBA         E DS
001900130306     D fnlv53ds      E DS
002000130923     D tnsd99ds      E DS                  inz
002100030422     D*--------------------
002200030422     D* SCHIERE DI MEMORIZZAZIONE
002300030422     D*--------------------
002400130305     d* tfp abilitati a far partire colli di altro tfp
002500130305     D xcotfp          S              3    DIM(85)
002600130305     D ncl             S              9  0 DIM(85)
002700130305     D puc             S             13  3 DIM(85)
002800130305     D pucB            S             13  3 DIM(85)
002900130305     D vuc             S             13  6 DIM(85)
003000130305     D vucB            S             13  6 DIM(85)
003100130919
003200130919     d* Memorizzo colli e quando partiti rispetto alla bolla
003300130919     D Moltre          s              7  0 dim(85)
003400130919     D M48             s              7  0 dim(85)
003500130919     D M24             s              7  0 dim(85)
003600130919     D confo           s              7  0 dim(85)
003700130919     D Poltre          s              7  0 dim(85)
003800130919     D P48             s              7  0 dim(85)
003900130919     D P24             s              7  0 dim(85)
004000130919     d* Memorizzo colli bolle non conformi
004100130919     D N_Moltre        s              7  0 dim(85)
004200130919     D n_M48           s              7  0 dim(85)
004300130919     D n_M24           s              7  0 dim(85)
004400130919     D n_confo         s              7  0 dim(85)
004500130919     D n_Poltre        s              7  0 dim(85)
004600130919     D n_P48           s              7  0 dim(85)
004700130919     D n_P24           s              7  0 dim(85)
004800130924     D n_boco          s              5  0 dim(85)
004900130924     D n_bop24         s              5  0 dim(85)
005000130924     D n_bop48         s              5  0 dim(85)
005100130924     D n_boOLT         s              5  0 dim(85)
005200130919     d* Memorizzo colli conformi
005300130919     D C_confo         s              7  0 dim(85)
005400130919     d
005500030414     D*--------------------
005600030414     D* VARIABILI DI WRK
005700030414     D*--------------------
005800130305     D conta           s              2  0
005900130305     D YY              s              4  0
006000130305     D I               s              4  0
006100130304     D kke1            s                   like(tbeke1)
006200130304     D kke2            s                   like(tbeke2)
006300130304     D kcod1           s                   like(tbecod)
006400130305     D wpes            s                   like(blppkf)
006500130305     D wvol            s                   like(blpvlf)
006600130305     D savfnrs         s                   like(wflsnrs)
006700130304     D Wflsnrs         s              5  0
006800130305     D Wblpdsp         s              8  0
006900130305     D unipkf          s              9  3
007000130305     D univol          s              9  6
007100130305     D Indx            s              3  0
007200130919     d
007300130925     D n_cltfp         s              7  0
007400130925     D c_cltfp         s              7  0
007500130919     D c_colli         s              7  0
007600130919     D n_colli         s              7  0
007700130920     D n_nospu         s              7  0
007800130925     D n_spumai        s              7  0
007900130919     D c_bolle         s              7  0
008000130919     D n_bolle         s              7  0
008100130919     D c_bolleCO       s              7  0
008200130919     D c_bolleNO       s              7  0
008300130919     D n_bolleCO       s              7  0
008400130919     D n_bolleNO       s              7  0
008500130919     d
008600130305     d Dataiso         s               d   datfmt(*iso)
008700130305     d Dataeur         s               d   datfmt(*eur)
008800130923     d Datasys         s               d   datfmt(*iso) inz(*sys)
008900030414     D*---
009000030414     D* DS REPERIMENTO DATI UTENTE
009100030414     D*---
009200030414     D TIBS34DS      E DS
009300030414     D DDATIUTE      E DS
009400030414     D AZUTEDS       E DS                  extname(AZUTE00F)
009500030423     D*---
009600030423     D* DS REPERIMENTO £ire
009700030423     D*---
009800030423     D TRUL06DS      E DS
009900030423     D  L1                     1     90  0 DIM(30)
010000030521     D*------------------
010100030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
010200030521     D*------------------
010300030521     D WLBDA8          DS                  INZ
010400030521     D  G08DAT                 1      8  0
010500030521     D  G08INV                 9     16  0
010600030521     D  G08ERR                17     17
010700030521     D  G08TGI                18     22  0
010800130919
010900130919     D WDAT8           DS
011000130919     D  DATADA                 1      8  0
011100130919     D  DATAA                  9     16  0
011200130919     D  GGL                   17     21  0
011300130919     D tfptfa          DS
011400130919     D  p_tfp                  1      3  0
011500130919     D  p_tfa                  4      6  0
011600001218
011700130304     D*------------------
011800130304     D FNLSW3DS        DS
011900130304     D  ilsw3dsd               1      8  0
012000130304     D  ilsw3dsa               9     16  0
012100130304     D  ilsW3TFP              18     20  0
012200130923     D  ilsw3Cont             21     21
012300130923     D  ilsw3sprg             22     22
012400130304
012500920812     C*---------------------------------------------------------------*
012600001218     C* MAIN
012700001218     C*---------------------------------------------------------------*
012800130304     c*
012900130306     c                   clear                   contabo           5 0
013000030422     C                   z-add     1             i
013100130304    1C                   dow       i <= %elem(L1)
013200130304     c
013300130304    2C                   if        L1(i) > *zeros
013400130304     C                   eval      blpLNP = L1(i)
013500130305     c                   movel     ilsw3dsd      kaas
013600130305     c                   move      ilsw3dsd      kmgs
013700130304     c
013800130304     C     kblp          setll     fnblp31l
013900130304     C     blplnp        reade     fnblp31l
014000130304     c
014100130304    3C                   dow       not %eof(fnblp31l)
014200130304     c
014300130304     c* se data sped > data AL --> esco dal ciclo
014400130304     c                   eval      wblpdsp=(blpaas*10000)+blpmgs
014500130304     c
014600130304     c                   if        wblpdsp>ilsw3dsa
014700130304     c                   leave
014800130304     c                   endif
014900130304     c
015000130304     c* Leggo le bolle con BLPXCO='W'
015100130304    4c                   if        blpxco='W'
015200130307     c                   eval      stptbol=stptbol+1
015300130304     c* cerco il record in XCO
015400130304     c                   eval      Wflsnrs=(blpfls*100)+blpnrs
015500130304
015600130304    5c                   if        wflsnrs<>savfnrs
015700130305
015800130305     c* memorizzo i dati per fls/nrs nel file di work
015900130305     c                   if        savfnrs>0
016000130305     c                   exsr      memdat
016100130305     c                   endif
016200130305     c
016300130304     c                   clear                   xcotfp
016400130304     C                   MOVEL     'XCO'         kcod1
016500130304     C                   eval      kke1='ALTRITFP'
016600130304     C                   eval      kke2=%editc(wflsnrs:'X')
016700130304     c     ktbe2         chain     tntbe01l
016800130304    6c                   if        %found(tntbe01l) AND tbeatb=' '
016900130304     C                   MOVEA     TBeUNI        xcotfp
017000130304    2c                   endif
017100131024     c
017200131024     c* Nella schiera aggiungo sempre me stesso
017300131024     c                   movel     ilsw3tfp      alfafgs           3
017400131024     c                   if        %lookup(alfafgs:xcotfp)=0
017500131024     c                   eval      Indx=%lookup('   ':xcotfp)
017600131024     c                   eval      xcotfp(indx)=%editc(ilsw3tfp:'X')
017700131024     c                   endif
017800130304     c
017900130304     c                   eval      savfnrs=wflsnrs
018000130304    5c                   endif
018100130304     c* Leggo collo per collo e controllo chi lo ha spuntato
018200130304     c
018300130919     c                   clear                   Wnoconfo          1
018400130919     c                   clear                   moltre
018500130919     c                   clear                   m48
018600130919     c                   clear                   m24
018700130919     c                   clear                   p48
018800130919     c                   clear                   p24
018900130919     c                   clear                   poltre
019000130919     c                   clear                   confo
019100130924     c                   clear                   savfgs            3 0
019200130924     c                   clear                   savdt_b           8 0
019300130304     c     kblt          setll     fnblt01l
019400130304     c     kblt          reade     fnblt01l
019500130304     c
019600130304    5c                   dow       not %eof(fnblt01l)
019700130305     c
019800130304     c                   exsr      leggispu
019900130304     c
020000130304     c     kblt          reade     fnblt01l
020100130304     c                   enddo
020200130919     c
020300130923     c* Se non ha i tempi di resa li calcolo
020400130923     c                   if        blpdce=0
020500130923     C*
020600130923     C                   MOVEL     ' '           D98TLA
020700130923     C                   MOVEL     'P'           D98TBO
020800130923     C                   Z-ADD     blpAAS        D98AAS
020900130923     C                   Z-ADD     blpLNP        D98LNP
021000130923     C                   Z-ADD     blpNRS        D98NRS
021100130923     C                   Z-ADD     blpnSP        D98NSP
021200130923     c* A udate e utime se non consseganta
021300130923     c                   if        blpdcm=0
021400130923     C                   movel     datasys       D98DFC
021500130923     C                   TIME                    WTIME            14 0
021600130923     c                   endif
021700130923     c
021800130923     C                   MOVEL     UTIME         D98OFC
021900130923     C                   CALL      'TNSD99R'
022000130923     C                   PARM                    tnsd99ds
022100130923     C*
022200130923     C                   Z-ADD     D98Nrc        blpnrc
022300130923     c                   endif
022400130925     c
022500130925     c* Verifico se ci sono colli dal terminal di partenza bolle
022600130925     c                   clear                   nocltfp           1
022700130925     c                   movel     ilsw3tfp      alfafgs           3
022800130925     c                   eval      Indx=%lookup(alfafgs:xcotfp)
022900130925     c                   if        moltre(indx)=0 and m48(indx)=0 and
023000130925     c                             m24(indx)=0 and confo(indx)=0 and
023100130925     c                             poltre(indx)=0 and p48(indx)=0 and
023200130925     c                             p24(indx)=0
023300130925     c                   eval      nocltfp='S'
023400130925     c                   else
023500130925     c                   eval      nocltfp='N'
023600130925     c                   endif
023700130925     c
023800130923     c
023900130919     c* Memorizzo nelle spareggiate o conformi
024000130924    1c                   if        wnoconfo='S'
024100130919     c                   add       moltre        n_moltre
024200130919     c                   add       m24           n_m24
024300130920     c                   add       m48           n_m48
024400130919     c                   add       confo         n_confo
024500130919     c                   add       poltre        n_poltre
024600130919     c                   add       p24           n_p24
024700130920     c                   add       p48           n_p48
024800130919     c                   add       blpncl        n_colli
024900130919     c                   add       1             n_bolle
025000130925     c                   if        nocltfp='S'
025100130925     c                   add       1             n_cltfp
025200130925     c                   endif
025300130925     c
025400130924    2c                   if        blpnrc<=0
025500130923     c                   add       1             n_bolleCO
025600130924    2c                   else
025700130923     c                   add       1             n_bolleNO
025800130924    2c                   endif
025900130924     c* La bolla va conteggiata alla filiale con data + alta tra bolla
026000130924     c*  e spunte
026100130924     c* Controllo anche data spedizione rispetto alla data spunta
026200130924    2c                   if        wblpdsp>savdt_b
026300130924     c                   eval      savdt_B=wblpdsp
026400130924     c                   eval      savfgs=blplnp
026500130924    2c                   endif
026600130924     c
026700130924     c                   clear                   wdat8
026800130924     c                   clear                   tfptfa
026900130924     c                   eval      datada=savdt_b
027000130924     c                   eval      dataa =wblpdsp
027100130924     c                   eval      p_tfp=blptfp
027200130924     c                   eval      p_tfa=blptfa
027300130924     c                   call      'XSRLAV8'
027400130924     c                   parm                    wdat8
027500130924     c                   parm                    tfptfa
027600130924     c                   movel     savfgs        alfafgs           3
027700130924     c                   eval      Indx=%lookup(alfafgs:xcotfp)
027800130924
027900130924    2c                   if        ggl<=1
028000130924     c                   add       1             n_boco(indx)
028100130924   x2c                   else
028200130924     c                   select
028300130924    3c                   when      ggl=2
028400130924     c                   add       1             n_bop24(indx)
028500130924     c                   when      ggl=3
028600130924     c                   add       1             n_bop48(indx)
028700130924     c                   other
028800130924     c                   add       1             n_boolt(indx)
028900130924    3c                   endsl
029000130924    2c                   endif
029100130923     c
029200130924   x1c                   else
029300130919     c                   add       confo         c_confo
029400130919     c                   add       blpncl        c_colli
029500130919     c                   add       1             c_bolle
029600130925     c                   if        nocltfp='S'
029700130925     c                   add       1             c_cltfp
029800130925     c                   endif
029900130924    2c                   if        blpnrc<=0
030000130923     c                   add       1             c_bolleCO
030100130923     c                   else
030200130923     c                   add       1             c_bolleNO
030300130924    2c                   endif
030400130924    1c                   endif
030500130304     c
030600130304    4c                   endif
030700130304     c
030800130304     C     blplnp        reade     fnblp31l
030900130304    3c                   enddo
031000130304    2c                   endif
031100030512     C*
031200030422     C                   eval      i = i +1
031300130304    1C                   enddo
031400020613     C*
031500130305     c* memorizzo i dati per fls/nrs nel file di work
031600130305     c                   if        savfnrs>0
031700130305     c                   exsr      memdat
031800130305     c                   endif
031900130305     c
032000130923     c                   if        ilsw3cont='S'
032100130305     c                   EXSR      Stampa
032200130923     c                   endif
032300130306     c
032400130306     C                   CLEAR                   fnlv53ds
032500130306     C                   MOVEL     'C'           D53TLA
032600130306     C                   CALL      'FNLV53R'
032700130306     C                   PARM                    fnlv53ds
032800130923     C                   CLEAR                   tnsd99ds
032900130923     C                   MOVEL     'C'           D98TLA
033000130923     C                   CALL      'TNSD99R'
033100130923     C                   PARM                    tnsd99ds
033200130305     C                   seton                                        LR
033300001218     C*------------------------------------------------------------------------*
033400130304     c     LeggiSpu      BEGSR
033500130304     c* Leggo spunta entrata o partenza se c'e' di un'altra filiale
033600130304     c*  prevista in XCOTFP
033700130304     c                   z-add     5             knpg              1 0
033800130305     c                   clear                   spufgs            3 0
033900130305     c                   eval      conta=1
034000130305
034100130305    1c                   dow       spufgs=0  and conta<=2
034200130304     c
034300130304     c     kbrv          setll     fnbrv09l
034400130304     c     kbrv          reade     fnbrv09l
034500130305    2c                   dow       not %eof(fnbrv09l)
034600130304     c*
034700130306   2ac                   if        brvdcs>0
034800130306     c                   clear                   fnlv53ds
034900130306     c* se si tratta di spunta partenza devo escludere i fogli defluenza
035000130306   2bc                   if        brvnpg=1
035100130306     C                   MOVEL     'G'           D53TFO
035200130306     C                   Z-ADD     brvNFV        D53NFV
035300130306     C                   MOVEL     brvnpg        D53NPG
035400130306     C                   MOVEL     brvFGS        D53FGS
035500130306     C                   CALL      'FNLV53R'
035600130306     C                   PARM                    fnlv53ds
035700130306   2bc                   endif
035800130306     c
035900130306   2bC                   if        brvnpg=5 or d53def=' '
036000130305     c                   movel     brvfgs        alfafgs           3
036100130305     c                   eval      Indx=%lookup(alfafgs:xcotfp)
036200130304     c
036300130305    3c                   if        indx>0
036400130305     c                   eval      spufgs=brvfgs
036500130304     c                   leave
036600130305    3c                   endif
036700130306   2bc                   endif
036800130306   2ac                   endif
036900030512
037000130304     c     kbrv          reade     fnbrv09l
037100130305    2c                   enddo
037200130305    c
037300130304     c                   z-add     1             knpg
037400130305     c                   eval      conta=conta+1
037500130305    1c                   enddo
037600130304     c
037700130920     c* Trovata spunta di altra filiale autorizzata --> conteggio
037800130305    1c                   if        spufgs>0
037900130306     c***                clear                   wfpcl000
038000130306     c***                eval      pclfil=dutpou
038100130306     c***                eval      pcltfp=ilsw3tfp
038200130306     c***                eval      pclfls=brvlnp
038300130306     c***                eval      pclnrs=brvnrs
038400130306     c****               eval      pcldsd=bltnsp
038500130306     c***                eval      pcldsa=bltnsc
038600130306     c***                eval      pclfgs=spufgs
038700130306     c***                write     wfpcl000
038800130304     c* Colli
038900130304     c                   add       1             ncl(indx)
039000130304     c
039100130304     c* peso
039200130305     C*        : - SE TOTALE  , uso   BLPpkc
039300130305     C*          - SE PARZIALE, uso   BLPPKC (SE MAGGIORE)
039400130305     C*                               ALTRIMENTI uso  BLPPKF
039500130305     C     BLPNCp        ifeq      BLPNCL
039600130305     C                   Z-ADD     BLPpkC        Wpes
039700130305     C*
039800130305   X4C                   ELSE
039900130305    5C     BLPpkC        IFGE      BLPpkF
040000130305     C                   Z-ADD     BLPpkC        Wpes
040100130305   X5C                   ELSE
040200130305     C                   Z-ADD     BLPpkF        Wpes
040300130305    5C                   ENDIF
040400130305    4C                   ENDIF
040500130305     c
040600130304     c                   select
040700130304     c                   when      brvpuc>0
040800130305     c                   add       brvpuc        puc(indx)
040900130305
041000130304     c                   when      bltpuc>0
041100130305     c                   add       bltpuc        puc(indx)
041200130304     c                   other
041300130304     c
041400130305     c                   eval(H)   unipkf    =wpes  /blpncl
041500130305     c                   add       unipkf        puc(indx)
041600130304     c                   endsl
041700130305     c* sempre media spedizione
041800130305     c                   eval(H)   unipkf    =wpes  /blpncl
041900130305     c                   add       unipkf        pucb(indx)
042000130304     c* volume
042100130305     C*        : - SE TOTALE  , uso   BLPpkc
042200130305     C*          - SE PARZIALE, uso   BLPPKC (SE MAGGIORE)
042300130305     C*                               ALTRIMENTI uso  BLPPKF
042400130305     C     BLPNCR        IFEQ      BLPNCL
042500130305     C                   Z-ADD     BLPVLC        WVOL
042600130305     C*
042700130305   X4C                   ELSE
042800130305    5C     BLPVLC        IFGE      BLPVLF
042900130305     C                   Z-ADD     BLPVLC        WVOL
043000130305   X5C                   ELSE
043100130305     C                   Z-ADD     BLPVLF        WVOL
043200130305    5C                   ENDIF
043300130305    4C                   ENDIF
043400130305     c
043500130920    2c                   select
043600130920    2c                   when      brvvuc>0
043700130305     c                   add       brvvuc        vuc(indx)
043800130305
043900130920    2c                   when      bltvuc>0
044000130305     c                   add       bltvuc        vuc(indx)
044100130920   x2c                   other
044200130305     c
044300130305     c                   eval(H)   univol    =wvol  /blpncl
044400130305     c                   add       univol        vuc(indx)
044500130920    2c                   endsl
044600130305
044700130305     c* sempre media spedizione
044800130305     c                   eval(H)   univol    =wvol  /blpncl
044900130305     c                   add       univol        vucb(indx)
045000130919     c
045100130919     c* Oltre al conteggio verifico se sono partite in modo spareggiato
045200130919     c*  se data spunta = data spedizione --> conforme altrimenti conto i
045300130919     c*  giorni lavorativi tra le date
045400130920    2c                   if        brvdcs=wblpdsp
045500130923     c                   add       1             confo(indx)
045600130924     c                   if        brvdcs>savdt_b
045700130924     c                   eval      savdt_B=brvdcs
045800130924     c                   eval      savfgs=spufgs
045900130924     c                   endif
046000130924
046100130920   x2c                   else
046200130919     c
046300130920     c                   clear                   wdat8
046400130919     c                   clear                   tfptfa
046500130919     c                   eval      datada=brvdcs
046600130919     c                   eval      dataa =wblpdsp
046700130919     c                   eval      p_tfp=blptfp
046800130919     c                   eval      p_tfa=blptfa
046900130919     c                   call      'XSRLAV8'
047000130920     c                   parm                    wdat8
047100130919     c                   parm                    tfptfa
047200130924
047300130924     c                   if        brvdcs>savdt_b
047400130924     c                   eval      savdt_B=brvdcs
047500130924     c                   eval      savfgs=spufgs
047600130924     c                   endif
047700130919
047800130924    3c                   if        ggl<=1
047900130923     c                   add       1             confo(indx)
048000130924     c
048100130919   x3c                   else
048200130919
048300130919     c                   eval      Wnoconfo='S'
048400130919
048500130920    4c                   if        wblpdsp>brvdcs
048600130919
048700130920     c                   select
048800130923    5c                   when      ggl=2
048900130919     c                   add       1             m24(indx)
049000130923     c                   when      ggl=3
049100130919     c                   add       1             m48(indx)
049200130919     c                   other
049300130919     c                   add       1             moltre(indx)
049400130919    5c                   endsl
049500130919   x4c                   else
049600130919   c
049700130920     c                   select
049800130923    5c                   when      ggl=2
049900130919     c                   add       1             p24(indx)
050000130923     c                   when      ggl=3
050100130919     c                   add       1             p48(indx)
050200130919     c                   other
050300130919     c                   add       1             poltre(indx)
050400130919    5c                   endsl
050500130919     c
050600130919    4c                   endif
050700130919    3c                   endif
050800130919    2c                   endif
050900130919     c
051000130920   x1c                   else
051100130920     c* se collo non ancora spuntato, conteggio a parte
051200130920     c                   eval      Wnoconfo='S'
051300130920     c* e metto la bolla tra le spareggiate
051400130920     c                   add       1             n_nospu
051500130925     c* Verifico se mai spuntato senon ha la data di consegna
051600130925    2c                   if        bltdcm=0
051700130925     c     kbrv7         setll     fnbrv07l
051800130925     c     kbrv7         reade     fnbrv07l
051900130925    3c                   dow       not %eof(fnbrv07l) and brvdcs=0
052000130925     c     kbrv7         reade     fnbrv07l
052100130925    3c                   enddo
052200130925     c
052300130925    3c                   if        %eof(fnbrv07l)
052400130925     c                   add       1             n_spumai
052500130925    3c                   endif
052600130925
052700130925    2c                   endif
052800130920     c
052900130919    1c                   endif
053000130304     c
053100130304     c                   ENDSR
053200130305     C*------------------------------------------------------------------------*
053300130305     c     Memdat        BEGSR
053400130305     c* Memorizzo i dati
053500130305     c* se  già scritto --> vado in aggiunta
053600130305     c
053700130305     c                   z-add     1             yy
053800130305    1c                   do        85            yy
053900130305
054000130305    2c                   if        ncl(yy)>0
054100130305     c                   movel     xcotfp(YY)    kfgs
054200130305     c                   movel     wflsnrs       kfls
054300130305     c                   move      wflsnrs       knrs
054400130305     c
054500130305     c     kpcl          chain     wfpcl01l
054600130305    3c                   if        not %found(wfpcl01l)
054700130305     c                   clear                   wfpcl000
054800130305     c                   eval      pclfil=dutpou
054900130305     c                   eval      pcltfp=ilsw3tfp
055000130305     c                   eval      pclfls=kfls
055100130305     c                   eval      pclnrs=knrs
055200130305     c                   eval      pcldsd=ilsw3dsd
055300130305     c                   eval      pcldsa=ilsw3dsa
055400130305     c                   eval      pclfgs=kfgs
055500130305    3c                   endif
055600130305     c                   add       ncl(yy)       pcltncl
055700130305     c                   add       puc(yy)       pcltpkg
055800130305     c                   add       pucb(yy)      pcltpkgb
055900130305     c                   add       vuc(yy)       pcltvlm
056000130305     c                   add       vucb(yy)      pcltvlmb
056100130307     c                   add       stptbol       pcltbol
056200130305     c
056300130305    3c                   if        not %found(wfpcl01l)
056400130305     c                   write     wfpcl000
056500130305     c                   else
056600130305     c                   update    wfpcl000
056700130305    3c                   endif
056800130305    2c                   endif
056900130305    1c                   enddo
057000130305     c
057100130305     c                   clear                   ncl
057200130305     c                   clear                   puc
057300130305     c                   clear                   pucb
057400130305     c                   clear                   vuc
057500130305     c                   clear                   vucb
057600130307     c                   clear                   stptbol
057700130305     c
057800130919     c* Stampa statistica spareggiate
057900130920     c     savfnrs       div       100           stpfls
058000130919     c                   mvr                     stpnrs
058100130919     c
058200130919
058300130923     c* Solo se richiesta la stampa delle spareggiate
058400130923     c                   if        ilsw3sprg='S'
058500130923     c
058600130919     c* Stampo prima le bolle conformi
058700130919     c                   z-add     1             XX                3 0
058800130923     c* totale bolle e colli
058900130923     c                   eval      stpbolle='  BOLLE PARTITE OK :'
059000130923     C
059100130923     C                   EVAL      STPTOTC =C_COLLI
059200130923     C                   EVAL      STPTOTB =C_BOLLE
059300130923     C                   EVAL      STPTOTB_c =C_BOLLECO
059400130923     C                   EVAL      STPTOTB_n =C_BOLLENO
059500130925     C                   EVAL      STPNOSIM  =C_cltfp
059600130924     c                   clear                   stpnospu
059700130924     c                   clear                   stpb_co
059800130924     c                   clear                   stpb_p24
059900130924     c                   clear                   stpb_p48
060000130924     c                   clear                   stpb_olt
060100130923
060200130923     C                   write     LSw4PT1
060300130923     C                   write     LSw4PI1
060400130923     c                   write     lsw4d31
060500130923     C                   write     LSw4PI2
060600130923
060700130919     c                   dow       xcotfp(xx)>*zeros
060800130919
060900130919     c                   movel     xcotfp(xx)    stpfgs
061000130919     c     stpfgs        CHAIN     AZORG01L
061100130919     c                   if        %found(azorg01l)
061200130919     c                   eval      stpdfgs=orgdes
061300130919     c                   else
061400130919     c                   eval      stpdfgs=*all'?'
061500130919     c                   endif
061600130920     c                   z-add     c_confo(xx)   stpconfo
061700130919     c
061800130919     c                   write     lsw4d21
061900130919
062000130919     c                   add       1             xx
062100130919     c                   enddo
062200130923
062300130923     c                   write     lsw4RI1
062400130919     c
062500130919     c* Stampo poi   le bolle NON conformi
062600130919     c                   z-add     1             XX                3 0
062700130923     c* totale bolle e colli
062800130923     c                   eval      stpbolle='  BOLLE SPAREGGIATE:'
062900130923     C
063000130923     C                   EVAL      STPnospu=N_NOSPU
063100130923     C                   EVAL      STPTOTC =N_COLLI
063200130923     C                   EVAL      STPTOTB =N_BOLLE
063300130923     C                   EVAL      STPTOTB_C =N_BOLLECO
063400130923     C                   EVAL      STPTOTB_n =N_BOLLENO
063500130925     C                   EVAL      STPNOSIM  =N_cltfp
063600130925     C                   EVAL      STPmai    =N_spumai
063700130923
063800130923     c                   write     lsw4d31
063900130923     C                   write     LSw4PI2
064000130919     c                   dow       xcotfp(xx)>*zeros
064100130919
064200130919     c                   movel     xcotfp(xx)    stpfgs
064300130919     c     stpfgs        CHAIN     AZORG01L
064400130919     c                   if        %found(azorg01l)
064500130919     c                   eval      stpdfgs=orgdes
064600130919     c                   else
064700130919     c                   eval      stpdfgs=*all'?'
064800130919     c                   endif
064900130919     c                   z-add     n_confo(xx)   stpconfo
065000130924     c                   z-add     n_moltre(xx)  stpMOlt
065100130919     c                   z-add     n_m48(xx)     stpM48
065200130919     c                   z-add     n_m24(xx)     stpM24
065300130919     c                   z-add     n_p24(xx)     stpp24
065400130919     c                   z-add     n_p48(xx)     stpp48
065500130924     c                   z-add     n_poltre(xx)  stppolt
065600130924     C                   EVAL      STPb_co   =N_BOco(xx)
065700130924     C                   EVAL      STPb_p24  =N_BOp24(xx)
065800130924     C                   EVAL      STPb_p48  =N_BOp48(xx)
065900130924     C                   EVAL      STPb_olt  =N_BOolt(xx)
066000130919     c
066100130919     c                   write     lsw4d21
066200130919
066300130919     c                   add       1             xx
066400130919     c                   enddo
066500130923
066600130923     c                   write     lsw4RI1
066700130923     c                   endif
066800130919     C
066900130925     C                   CLEAR                   n_spumai
067000130925     C                   CLEAR                   n_nospu
067100130925     C                   CLEAR                   n_cltfp
067200130925     C                   CLEAR                   c_cltfp
067300130925     C                   CLEAR                   n_colli
067400130919     C                   CLEAR                   n_bolle
067500130919     C                   CLEAR                   n_bolleco
067600130919     C                   CLEAR                   n_bolleno
067700130919     C                   CLEAR                   c_colli
067800130919     C                   CLEAR                   c_bolle
067900130919     C                   CLEAR                   c_bolleco
068000130919     C                   CLEAR                   c_bolleno
068100130919     c                   clear                   c_confo
068200130919     c                   clear                   n_confo
068300130919     c                   clear                   n_moltre
068400130919     c                   clear                   n_m48
068500130919     c                   clear                   n_m24
068600130919     c                   clear                   n_p24
068700130919     c                   clear                   n_p48
068800130919     c                   clear                   n_poltre
068900130924     c                   clear                   n_BOCO
069000130924     c                   clear                   n_BOP24
069100130924     c                   clear                   n_BOP48
069200130924     c                   clear                   n_BOOLT
069300130919     c
069400130305     c                   ENDSR
069500130305     C*------------------------------------------------------------------------*
069600130305     c     Stampa        BEGSR
069700130305     c* rileggo i dati
069800130305     c                   clear                   savfnrs
069900130305     c
070000130305     c     kpcl_sta      setll     wfpcl01l
070100130305     c     kpcl_sta      reade     wfpcl01l
070200130305     c
070300130312    1c                   dow       not %eof(wfpcl01l)
070400130305     c* a cambio fls/nrs ristampo la testata
070500130305     c                   eval      wflsnrs=(pclfls+100)+pclnrs
070600130305     c
070700130305     c                   if        wflsnrs<>savfnrs
070800150910     c                   if        savfnrs>0
070900150910     C                   write     LSw4RI
071000150910     c                   endif
071100150910     c
071200130919     c                   seton                                        01
071300130305     c                   eval      savfnrs=wflsnrs
071400130305     c                   endif
071500130305     c
071600130305     c* Stampa  testata
071700130305     c
071800130305     c                   eval      stpfls=pclfls
071900130305     c                   eval      stpnrs=pclnrs
072000130307     c                   eval      stptbol=pcltbol
072100130305
072200130305     c                   if        *in01
072300130305     C                   write     LSw4PT
072400130305     C                   write     LSw4PI
072500130305     c                   setoff                                       01
072600130305     c                   endif
072700130305     c
072800130305     c* stampa detaglio
072900130305     c                   eval      stpfgs=pclfgs
073000130305     c     pclfgs        CHAIN     AZORG01L
073100130305     c                   if        %found(azorg01l)
073200130305     c                   eval      stpdfgs=orgdes
073300130305     c                   else
073400130305     c                   eval      stpdfgs=*all'?'
073500130305     c                   endif
073600130305     c
073700130305     c                   eval      stptncl=pcltncl
073800130305     c                   eval(H)   stptpkg=pcltpkg
073900130305     c                   eval(H)   stptvlm=pcltvlm
074000130305     c                   write     lsw4d2
074100130305     c
074200130305     c     kpcl_sta      reade     wfpcl01l
074300130312    1c                   enddo
074400130305     c
074500130312     c                   if        *in01
074600130312     C                   write     LSw4PT
074700130312     C                   write     LSw4PI
074800130312     c                   endif
074900130312     c
075000130305     C                   write     LSw4RI
075100130305     c
075200130305     c                   ENDSR
075300030414     C*---------------------------------------------------------------*
075400030414     C* CARTAB - Caricamento tabelle di wrk
075500030414     C*---------------------------------------------------------------*
075600030414     C     CARTAB        BEGSR
075700030423     C*
075800030423     C* Carico i P.O. del T.P. dell'utente reperendoli dalla £1
075900030423     C                   MOVEL     '£1'          D06COD
076000130305     C                   MOVEL     ilsw3tfp      D06KEY
076100030423     C                   MOVEL     'L'           D06TLA
076200030423     C                   MOVEL     *BLANKS       D06ESC
076300030423     C                   MOVEL(P)  TRUL06DS      KPJBU
076400030423     C                   CALL      'TRUL06R'
076500030423     C                   PARM                    KPJBA
076600030423     C                   MOVEL     KPJBU         TRUL06DS
076700030414     C*
076800130923     C                   MOVE      WTIME         WDATE             8 0
076900130923     C                   MOVEL     WTIME         UTIME             6 0
077000130923     C*
077100030414     C                   ENDSR
077200030414     C*------------------------------------------------------------------------*
077300030414     C* REPERISCE I DATI UTENTE
077400030414     C*------------------------------------------------------------------------*
077500030414     C     REPDATIUTE    BEGSR
077600030414     C*
077700030414     C* INIZIALIZZA VARIABILI DI WRK
077800030414     C                   CLEAR                   TIBS34DS
077900030414     C                   CLEAR                   AZUTEDS
078000030414     C                   CLEAR                   DDATIUTE
078100030414     C*
078200030414     C     *DTAARA       DEFINE    §azute        azuteds
078300030414     C     *DTAARA       DEFINE    §datiute      ddatiute
078400030414     C                   IN(E)     *DTAARA
078500030422     C                   IF        %Error
078600030422     C                   EVAL      I34Tla = 'L'
078700030414     C                   CALL      'TIBS34R'
078800030414     C                   PARM                    Tibs34Ds
078900030414     C                   IN        *DTAARA
079000030422     C                   ENDIF
079100030414     C*
079200030414     C                   ENDSR
079300001218     C*------------------------------------------------------------------------*
079400001218     C* *INZSR - ROUTINE INIZIALE
079500001218     C*------------------------------------------------------------------------*
079600001218     C     *INZSR        BEGSR
079700001218     C*
079800030422     C* Ricevo in la DS di architettura
079900010726     C     *ENTRY        PLIST
080000020613     C                   PARM                    KPJBA
080100130304     c                   movel     kpjbu         fnlsw3ds
080200130305     c
080300130305     c                   if        ilsw3dsa=0
080400130305     c                   eval      ilsw3dsa=ilsw3dsd
080500130305     c                   endif
080600030521     C*
080700030422     C* Inizializzo campi chiave "fissi"
080800030414     C                   Z-ADD     1             tblKUT
080900030414     C****
081000030414     C* Definizione chiavi
081100030414     C****
081200030512     C* Chiave su TABEL00F - parziale
081300030512     C     KEYtabP       KLIST
081400030414     C                   KFLD                    tblKUT
081500030414     C                   KFLD                    tblCOD
081600130304     C     KTBE2         KLIST
081700130304     C                   KFLD                    KCOD1
081800130304     C                   KFLD                    KKE1
081900130304     C                   KFLD                    KKE2
082000130304     C*
082100130304     C     kblp          KLIST
082200030422     C                   KFLD                    blpLNP
082300130304     C                   KFLD                    kaas              4 0
082400130304     C                   KFLD                    kmgs              4 0
082500130304     C     kblt          KLIST
082600130304     C                   KFLD                    blpaas
082700130304     C                   KFLD                    blplnp
082800130304     C                   KFLD                    blpnrs
082900130304     C                   KFLD                    blpnsp
083000130304     C     kbrv          KLIST
083100130304     C                   KFLD                    knpg              1 0
083200130304     C                   KFLD                    bltfls
083300130304     C                   KFLD                    bltlna
083400130304     C                   KFLD                    bltnrs
083500130304     C                   KFLD                    bltnsc
083600130925     C     kbrv7         KLIST
083700130925     C                   KFLD                    bltfls
083800130925     C                   KFLD                    bltlna
083900130925     C                   KFLD                    bltnrs
084000130925     C                   KFLD                    bltnsc
084100130305     C     kpcl          KLIST
084200130305     C                   KFLD                    dutpou
084300130305     C                   KFLD                    ilsw3tfp
084400130305     C                   KFLD                    ilsw3dsd
084500130305     C                   KFLD                    ilsw3dsa
084600130305     C                   KFLD                    kfls              3 0
084700130305     C                   KFLD                    knrs              2 0
084800130305     C                   KFLD                    kfgs              3 0
084900130305     c
085000130305     C     kpcl_sta      KLIST
085100130305     C                   KFLD                    dutpou
085200130305     C                   KFLD                    ilsw3tfp
085300130305     C                   KFLD                    ilsw3dsd
085400130305     C                   KFLD                    ilsw3dsa
085500030414     C*
085600030414     C* REPERIMEMTO DATI UTENTE
085700030414     C                   EXSR      REPDATIUTE
085800030422     C*
085900030422     C* CARICAMENTO TABELLE DI WRK
086000030422     C                   EXSR      CARTAB
086100130919     c
086200130919     c* Giro la richiesta date
086300130919     c                   eval      dataiso=%date(ilsw3dsd)
086400130919     c                   eval      dataeur=dataiso
086500130919     c                   eval      stpdsd=%dec(dataeur)
086600130923     c                   seton                                        0102
086700130919
086800130919     c                   if        ilsw3dsa<>ilsw3dsd and ilsw3dsa>0
086900130919     c                   eval      dataiso=%date(ilsw3dsa)
087000130919     c                   eval      dataeur=dataiso
087100130919     c                   eval      stpdsa=%dec(dataeur)
087200130919     c                   endif
087300130919
087400130919     c     ilsw3tfp      CHAIN     AZORG01L
087500130919     c                   if        %found(azorg01l)
087600130919     c                   eval      stpdtfp=orgdes
087700130919     c                   endif
087800130925     c
087900130925     c                   eval      stptfp=ilsw3tfp
088000130305      /free
088100130305       setll (dutpou)  wfpcl01l   ;
088200130305       reade (dutpou)  wfpcl01l   ;
088300130305
088400130305       dow  not %eof(wfpcl01l)   ;
088500130305       delete wfpcl000  ;
088600130305
088700130305       reade (dutpou)  wfpcl01l   ;
088800130305       enddo  ;
088900130305
089000130305      /end-free
089100130305
089200001218     C                   ENDSR
