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
017100130304     c
017200130304     c                   eval      savfnrs=wflsnrs
017300130304    5c                   endif
017400130304     c* Leggo collo per collo e controllo chi lo ha spuntato
017500130304     c
017600130919     c                   clear                   Wnoconfo          1
017700130919     c                   clear                   moltre
017800130919     c                   clear                   m48
017900130919     c                   clear                   m24
018000130919     c                   clear                   p48
018100130919     c                   clear                   p24
018200130919     c                   clear                   poltre
018300130919     c                   clear                   confo
018400130924     c                   clear                   savfgs            3 0
018500130924     c                   clear                   savdt_b           8 0
018600130304     c     kblt          setll     fnblt01l
018700130304     c     kblt          reade     fnblt01l
018800130304     c
018900130304    5c                   dow       not %eof(fnblt01l)
019000130305     c
019100130304     c                   exsr      leggispu
019200130304     c
019300130304     c     kblt          reade     fnblt01l
019400130304     c                   enddo
019500130919     c
019600130923     c* Se non ha i tempi di resa li calcolo
019700130923     c                   if        blpdce=0
019800130923     C*
019900130923     C                   MOVEL     ' '           D98TLA
020000130923     C                   MOVEL     'P'           D98TBO
020100130923     C                   Z-ADD     blpAAS        D98AAS
020200130923     C                   Z-ADD     blpLNP        D98LNP
020300130923     C                   Z-ADD     blpNRS        D98NRS
020400130923     C                   Z-ADD     blpnSP        D98NSP
020500130923     c* A udate e utime se non consseganta
020600130923     c                   if        blpdcm=0
020700130923     C                   movel     datasys       D98DFC
020800130923     C                   TIME                    WTIME            14 0
020900130923     c                   endif
021000130923     c
021100130923     C                   MOVEL     UTIME         D98OFC
021200130923     C                   CALL      'TNSD99R'
021300130923     C                   PARM                    tnsd99ds
021400130923     C*
021500130923     C                   Z-ADD     D98Nrc        blpnrc
021600130923     c                   endif
021700130925     c
021800130925     c* Verifico se ci sono colli dal terminal di partenza bolle
021900130925     c                   clear                   nocltfp           1
022000130925     c                   movel     ilsw3tfp      alfafgs           3
022100130925     c                   eval      Indx=%lookup(alfafgs:xcotfp)
022200130925     c                   if        moltre(indx)=0 and m48(indx)=0 and
022300130925     c                             m24(indx)=0 and confo(indx)=0 and
022400130925     c                             poltre(indx)=0 and p48(indx)=0 and
022500130925     c                             p24(indx)=0
022600130925     c                   eval      nocltfp='S'
022700130925     c                   else
022800130925     c                   eval      nocltfp='N'
022900130925     c                   endif
023000130925     c
023100130923     c
023200130919     c* Memorizzo nelle spareggiate o conformi
023300130924    1c                   if        wnoconfo='S'
023400130919     c                   add       moltre        n_moltre
023500130919     c                   add       m24           n_m24
023600130920     c                   add       m48           n_m48
023700130919     c                   add       confo         n_confo
023800130919     c                   add       poltre        n_poltre
023900130919     c                   add       p24           n_p24
024000130920     c                   add       p48           n_p48
024100130919     c                   add       blpncl        n_colli
024200130919     c                   add       1             n_bolle
024300130925     c                   if        nocltfp='S'
024400130925     c                   add       1             n_cltfp
024500130925     c                   endif
024600130925     c
024700130924    2c                   if        blpnrc<=0
024800130923     c                   add       1             n_bolleCO
024900130924    2c                   else
025000130923     c                   add       1             n_bolleNO
025100130924    2c                   endif
025200130924     c* La bolla va conteggiata alla filiale con data + alta tra bolla
025300130924     c*  e spunte
025400130924     c* Controllo anche data spedizione rispetto alla data spunta
025500130924    2c                   if        wblpdsp>savdt_b
025600130924     c                   eval      savdt_B=wblpdsp
025700130924     c                   eval      savfgs=blplnp
025800130924    2c                   endif
025900130924     c
026000130924     c                   clear                   wdat8
026100130924     c                   clear                   tfptfa
026200130924     c                   eval      datada=savdt_b
026300130924     c                   eval      dataa =wblpdsp
026400130924     c                   eval      p_tfp=blptfp
026500130924     c                   eval      p_tfa=blptfa
026600130924     c                   call      'XSRLAV8'
026700130924     c                   parm                    wdat8
026800130924     c                   parm                    tfptfa
026900130924     c                   movel     savfgs        alfafgs           3
027000130924     c                   eval      Indx=%lookup(alfafgs:xcotfp)
027100130924
027200130924    2c                   if        ggl<=1
027300130924     c                   add       1             n_boco(indx)
027400130924   x2c                   else
027500130924     c                   select
027600130924    3c                   when      ggl=2
027700130924     c                   add       1             n_bop24(indx)
027800130924     c                   when      ggl=3
027900130924     c                   add       1             n_bop48(indx)
028000130924     c                   other
028100130924     c                   add       1             n_boolt(indx)
028200130924    3c                   endsl
028300130924    2c                   endif
028400130923     c
028500130924   x1c                   else
028600130919     c                   add       confo         c_confo
028700130919     c                   add       blpncl        c_colli
028800130919     c                   add       1             c_bolle
028900130925     c                   if        nocltfp='S'
029000130925     c                   add       1             c_cltfp
029100130925     c                   endif
029200130924    2c                   if        blpnrc<=0
029300130923     c                   add       1             c_bolleCO
029400130923     c                   else
029500130923     c                   add       1             c_bolleNO
029600130924    2c                   endif
029700130924    1c                   endif
029800130304     c
029900130304    4c                   endif
030000130304     c
030100130304     C     blplnp        reade     fnblp31l
030200130304    3c                   enddo
030300130304    2c                   endif
030400030512     C*
030500030422     C                   eval      i = i +1
030600130304    1C                   enddo
030700020613     C*
030800130305     c* memorizzo i dati per fls/nrs nel file di work
030900130305     c                   if        savfnrs>0
031000130305     c                   exsr      memdat
031100130305     c                   endif
031200130305     c
031300130923     c                   if        ilsw3cont='S'
031400130305     c                   EXSR      Stampa
031500130923     c                   endif
031600130306     c
031700130306     C                   CLEAR                   fnlv53ds
031800130306     C                   MOVEL     'C'           D53TLA
031900130306     C                   CALL      'FNLV53R'
032000130306     C                   PARM                    fnlv53ds
032100130923     C                   CLEAR                   tnsd99ds
032200130923     C                   MOVEL     'C'           D98TLA
032300130923     C                   CALL      'TNSD99R'
032400130923     C                   PARM                    tnsd99ds
032500130305     C                   seton                                        LR
032600001218     C*------------------------------------------------------------------------*
032700130304     c     LeggiSpu      BEGSR
032800130304     c* Leggo spunta entrata o partenza se c'e' di un'altra filiale
032900130304     c*  prevista in XCOTFP
033000130304     c                   z-add     5             knpg              1 0
033100130305     c                   clear                   spufgs            3 0
033200130305     c                   eval      conta=1
033300130305
033400130305    1c                   dow       spufgs=0  and conta<=2
033500130304     c
033600130304     c     kbrv          setll     fnbrv09l
033700130304     c     kbrv          reade     fnbrv09l
033800130305    2c                   dow       not %eof(fnbrv09l)
033900130304     c*
034000130306   2ac                   if        brvdcs>0
034100130306     c                   clear                   fnlv53ds
034200130306     c* se si tratta di spunta partenza devo escludere i fogli defluenza
034300130306   2bc                   if        brvnpg=1
034400130306     C                   MOVEL     'G'           D53TFO
034500130306     C                   Z-ADD     brvNFV        D53NFV
034600130306     C                   MOVEL     brvnpg        D53NPG
034700130306     C                   MOVEL     brvFGS        D53FGS
034800130306     C                   CALL      'FNLV53R'
034900130306     C                   PARM                    fnlv53ds
035000130306   2bc                   endif
035100130306     c
035200130306   2bC                   if        brvnpg=5 or d53def=' '
035300130305     c                   movel     brvfgs        alfafgs           3
035400130305     c                   eval      Indx=%lookup(alfafgs:xcotfp)
035500130304     c
035600130305    3c                   if        indx>0
035700130305     c                   eval      spufgs=brvfgs
035800130304     c                   leave
035900130305    3c                   endif
036000130306   2bc                   endif
036100130306   2ac                   endif
036200030512
036300130304     c     kbrv          reade     fnbrv09l
036400130305    2c                   enddo
036500130305    c
036600130304     c                   z-add     1             knpg
036700130305     c                   eval      conta=conta+1
036800130305    1c                   enddo
036900130304     c
037000130920     c* Trovata spunta di altra filiale autorizzata --> conteggio
037100130305    1c                   if        spufgs>0
037200130306     c***                clear                   wfpcl000
037300130306     c***                eval      pclfil=dutpou
037400130306     c***                eval      pcltfp=ilsw3tfp
037500130306     c***                eval      pclfls=brvlnp
037600130306     c***                eval      pclnrs=brvnrs
037700130306     c****               eval      pcldsd=bltnsp
037800130306     c***                eval      pcldsa=bltnsc
037900130306     c***                eval      pclfgs=spufgs
038000130306     c***                write     wfpcl000
038100130304     c* Colli
038200130304     c                   add       1             ncl(indx)
038300130304     c
038400130304     c* peso
038500130305     C*        : - SE TOTALE  , uso   BLPpkc
038600130305     C*          - SE PARZIALE, uso   BLPPKC (SE MAGGIORE)
038700130305     C*                               ALTRIMENTI uso  BLPPKF
038800130305     C     BLPNCp        ifeq      BLPNCL
038900130305     C                   Z-ADD     BLPpkC        Wpes
039000130305     C*
039100130305   X4C                   ELSE
039200130305    5C     BLPpkC        IFGE      BLPpkF
039300130305     C                   Z-ADD     BLPpkC        Wpes
039400130305   X5C                   ELSE
039500130305     C                   Z-ADD     BLPpkF        Wpes
039600130305    5C                   ENDIF
039700130305    4C                   ENDIF
039800130305     c
039900130304     c                   select
040000130304     c                   when      brvpuc>0
040100130305     c                   add       brvpuc        puc(indx)
040200130305
040300130304     c                   when      bltpuc>0
040400130305     c                   add       bltpuc        puc(indx)
040500130304     c                   other
040600130304     c
040700130305     c                   eval(H)   unipkf    =wpes  /blpncl
040800130305     c                   add       unipkf        puc(indx)
040900130304     c                   endsl
041000130305     c* sempre media spedizione
041100130305     c                   eval(H)   unipkf    =wpes  /blpncl
041200130305     c                   add       unipkf        pucb(indx)
041300130304     c* volume
041400130305     C*        : - SE TOTALE  , uso   BLPpkc
041500130305     C*          - SE PARZIALE, uso   BLPPKC (SE MAGGIORE)
041600130305     C*                               ALTRIMENTI uso  BLPPKF
041700130305     C     BLPNCR        IFEQ      BLPNCL
041800130305     C                   Z-ADD     BLPVLC        WVOL
041900130305     C*
042000130305   X4C                   ELSE
042100130305    5C     BLPVLC        IFGE      BLPVLF
042200130305     C                   Z-ADD     BLPVLC        WVOL
042300130305   X5C                   ELSE
042400130305     C                   Z-ADD     BLPVLF        WVOL
042500130305    5C                   ENDIF
042600130305    4C                   ENDIF
042700130305     c
042800130920    2c                   select
042900130920    2c                   when      brvvuc>0
043000130305     c                   add       brvvuc        vuc(indx)
043100130305
043200130920    2c                   when      bltvuc>0
043300130305     c                   add       bltvuc        vuc(indx)
043400130920   x2c                   other
043500130305     c
043600130305     c                   eval(H)   univol    =wvol  /blpncl
043700130305     c                   add       univol        vuc(indx)
043800130920    2c                   endsl
043900130305
044000130305     c* sempre media spedizione
044100130305     c                   eval(H)   univol    =wvol  /blpncl
044200130305     c                   add       univol        vucb(indx)
044300130919     c
044400130919     c* Oltre al conteggio verifico se sono partite in modo spareggiato
044500130919     c*  se data spunta = data spedizione --> conforme altrimenti conto i
044600130919     c*  giorni lavorativi tra le date
044700130920    2c                   if        brvdcs=wblpdsp
044800130923     c                   add       1             confo(indx)
044900130924     c                   if        brvdcs>savdt_b
045000130924     c                   eval      savdt_B=brvdcs
045100130924     c                   eval      savfgs=spufgs
045200130924     c                   endif
045300130924
045400130920   x2c                   else
045500130919     c
045600130920     c                   clear                   wdat8
045700130919     c                   clear                   tfptfa
045800130919     c                   eval      datada=brvdcs
045900130919     c                   eval      dataa =wblpdsp
046000130919     c                   eval      p_tfp=blptfp
046100130919     c                   eval      p_tfa=blptfa
046200130919     c                   call      'XSRLAV8'
046300130920     c                   parm                    wdat8
046400130919     c                   parm                    tfptfa
046500130924
046600130924     c                   if        brvdcs>savdt_b
046700130924     c                   eval      savdt_B=brvdcs
046800130924     c                   eval      savfgs=spufgs
046900130924     c                   endif
047000130919
047100130924    3c                   if        ggl<=1
047200130923     c                   add       1             confo(indx)
047300130924     c
047400130919   x3c                   else
047500130919
047600130919     c                   eval      Wnoconfo='S'
047700130919
047800130920    4c                   if        wblpdsp>brvdcs
047900130919
048000130920     c                   select
048100130923    5c                   when      ggl=2
048200130919     c                   add       1             m24(indx)
048300130923     c                   when      ggl=3
048400130919     c                   add       1             m48(indx)
048500130919     c                   other
048600130919     c                   add       1             moltre(indx)
048700130919    5c                   endsl
048800130919   x4c                   else
048900130919   c
049000130920     c                   select
049100130923    5c                   when      ggl=2
049200130919     c                   add       1             p24(indx)
049300130923     c                   when      ggl=3
049400130919     c                   add       1             p48(indx)
049500130919     c                   other
049600130919     c                   add       1             poltre(indx)
049700130919    5c                   endsl
049800130919     c
049900130919    4c                   endif
050000130919    3c                   endif
050100130919    2c                   endif
050200130919     c
050300130920   x1c                   else
050400130920     c* se collo non ancora spuntato, conteggio a parte
050500130920     c                   eval      Wnoconfo='S'
050600130920     c* e metto la bolla tra le spareggiate
050700130920     c                   add       1             n_nospu
050800130925     c* Verifico se mai spuntato senon ha la data di consegna
050900130925    2c                   if        bltdcm=0
051000130925     c     kbrv7         setll     fnbrv07l
051100130925     c     kbrv7         reade     fnbrv07l
051200130925    3c                   dow       not %eof(fnbrv07l) and brvdcs=0
051300130925     c     kbrv7         reade     fnbrv07l
051400130925    3c                   enddo
051500130925     c
051600130925    3c                   if        %eof(fnbrv07l)
051700130925     c                   add       1             n_spumai
051800130925    3c                   endif
051900130925
052000130925    2c                   endif
052100130920     c
052200130919    1c                   endif
052300130304     c
052400130304     c                   ENDSR
052500130305     C*------------------------------------------------------------------------*
052600130305     c     Memdat        BEGSR
052700130305     c* Memorizzo i dati
052800130305     c* se  già scritto --> vado in aggiunta
052900130305     c
053000130305     c                   z-add     1             yy
053100130305    1c                   do        85            yy
053200130305
053300130305    2c                   if        ncl(yy)>0
053400130305     c                   movel     xcotfp(YY)    kfgs
053500130305     c                   movel     wflsnrs       kfls
053600130305     c                   move      wflsnrs       knrs
053700130305     c
053800130305     c     kpcl          chain     wfpcl01l
053900130305    3c                   if        not %found(wfpcl01l)
054000130305     c                   clear                   wfpcl000
054100130305     c                   eval      pclfil=dutpou
054200130305     c                   eval      pcltfp=ilsw3tfp
054300130305     c                   eval      pclfls=kfls
054400130305     c                   eval      pclnrs=knrs
054500130305     c                   eval      pcldsd=ilsw3dsd
054600130305     c                   eval      pcldsa=ilsw3dsa
054700130305     c                   eval      pclfgs=kfgs
054800130305    3c                   endif
054900130305     c                   add       ncl(yy)       pcltncl
055000130305     c                   add       puc(yy)       pcltpkg
055100130305     c                   add       pucb(yy)      pcltpkgb
055200130305     c                   add       vuc(yy)       pcltvlm
055300130305     c                   add       vucb(yy)      pcltvlmb
055400130307     c                   add       stptbol       pcltbol
055500130305     c
055600130305    3c                   if        not %found(wfpcl01l)
055700130305     c                   write     wfpcl000
055800130305     c                   else
055900130305     c                   update    wfpcl000
056000130305    3c                   endif
056100130305    2c                   endif
056200130305    1c                   enddo
056300130305     c
056400130305     c                   clear                   ncl
056500130305     c                   clear                   puc
056600130305     c                   clear                   pucb
056700130305     c                   clear                   vuc
056800130305     c                   clear                   vucb
056900130307     c                   clear                   stptbol
057000130305     c
057100130919     c* Stampa statistica spareggiate
057200130920     c     savfnrs       div       100           stpfls
057300130919     c                   mvr                     stpnrs
057400130919     c
057500130919
057600130923     c* Solo se richiesta la stampa delle spareggiate
057700130923     c                   if        ilsw3sprg='S'
057800130923     c
057900130919     c* Stampo prima le bolle conformi
058000130919     c                   z-add     1             XX                3 0
058100130923     c* totale bolle e colli
058200130923     c                   eval      stpbolle='  BOLLE PARTITE OK :'
058300130923     C
058400130923     C                   EVAL      STPTOTC =C_COLLI
058500130923     C                   EVAL      STPTOTB =C_BOLLE
058600130923     C                   EVAL      STPTOTB_c =C_BOLLECO
058700130923     C                   EVAL      STPTOTB_n =C_BOLLENO
058800130925     C                   EVAL      STPNOSIM  =C_cltfp
058900130924     c                   clear                   stpnospu
059000130924     c                   clear                   stpb_co
059100130924     c                   clear                   stpb_p24
059200130924     c                   clear                   stpb_p48
059300130924     c                   clear                   stpb_olt
059400130923
059500130923     C                   write     LSw4PT1
059600130923     C                   write     LSw4PI1
059700130923     c                   write     lsw4d31
059800130923     C                   write     LSw4PI2
059900130923
060000130919     c                   dow       xcotfp(xx)>*zeros
060100130919
060200130919     c                   movel     xcotfp(xx)    stpfgs
060300130919     c     stpfgs        CHAIN     AZORG01L
060400130919     c                   if        %found(azorg01l)
060500130919     c                   eval      stpdfgs=orgdes
060600130919     c                   else
060700130919     c                   eval      stpdfgs=*all'?'
060800130919     c                   endif
060900130920     c                   z-add     c_confo(xx)   stpconfo
061000130919     c
061100130919     c                   write     lsw4d21
061200130919
061300130919     c                   add       1             xx
061400130919     c                   enddo
061500130923
061600130923     c                   write     lsw4RI1
061700130919     c
061800130919     c* Stampo poi   le bolle NON conformi
061900130919     c                   z-add     1             XX                3 0
062000130923     c* totale bolle e colli
062100130923     c                   eval      stpbolle='  BOLLE SPAREGGIATE:'
062200130923     C
062300130923     C                   EVAL      STPnospu=N_NOSPU
062400130923     C                   EVAL      STPTOTC =N_COLLI
062500130923     C                   EVAL      STPTOTB =N_BOLLE
062600130923     C                   EVAL      STPTOTB_C =N_BOLLECO
062700130923     C                   EVAL      STPTOTB_n =N_BOLLENO
062800130925     C                   EVAL      STPNOSIM  =N_cltfp
062900130925     C                   EVAL      STPmai    =N_spumai
063000130923
063100130923     c                   write     lsw4d31
063200130923     C                   write     LSw4PI2
063300130919     c                   dow       xcotfp(xx)>*zeros
063400130919
063500130919     c                   movel     xcotfp(xx)    stpfgs
063600130919     c     stpfgs        CHAIN     AZORG01L
063700130919     c                   if        %found(azorg01l)
063800130919     c                   eval      stpdfgs=orgdes
063900130919     c                   else
064000130919     c                   eval      stpdfgs=*all'?'
064100130919     c                   endif
064200130919     c                   z-add     n_confo(xx)   stpconfo
064300130924     c                   z-add     n_moltre(xx)  stpMOlt
064400130919     c                   z-add     n_m48(xx)     stpM48
064500130919     c                   z-add     n_m24(xx)     stpM24
064600130919     c                   z-add     n_p24(xx)     stpp24
064700130919     c                   z-add     n_p48(xx)     stpp48
064800130924     c                   z-add     n_poltre(xx)  stppolt
064900130924     C                   EVAL      STPb_co   =N_BOco(xx)
065000130924     C                   EVAL      STPb_p24  =N_BOp24(xx)
065100130924     C                   EVAL      STPb_p48  =N_BOp48(xx)
065200130924     C                   EVAL      STPb_olt  =N_BOolt(xx)
065300130919     c
065400130919     c                   write     lsw4d21
065500130919
065600130919     c                   add       1             xx
065700130919     c                   enddo
065800130923
065900130923     c                   write     lsw4RI1
066000130923     c                   endif
066100130919     C
066200130925     C                   CLEAR                   n_spumai
066300130925     C                   CLEAR                   n_nospu
066400130925     C                   CLEAR                   n_cltfp
066500130925     C                   CLEAR                   c_cltfp
066600130925     C                   CLEAR                   n_colli
066700130919     C                   CLEAR                   n_bolle
066800130919     C                   CLEAR                   n_bolleco
066900130919     C                   CLEAR                   n_bolleno
067000130919     C                   CLEAR                   c_colli
067100130919     C                   CLEAR                   c_bolle
067200130919     C                   CLEAR                   c_bolleco
067300130919     C                   CLEAR                   c_bolleno
067400130919     c                   clear                   c_confo
067500130919     c                   clear                   n_confo
067600130919     c                   clear                   n_moltre
067700130919     c                   clear                   n_m48
067800130919     c                   clear                   n_m24
067900130919     c                   clear                   n_p24
068000130919     c                   clear                   n_p48
068100130919     c                   clear                   n_poltre
068200130924     c                   clear                   n_BOCO
068300130924     c                   clear                   n_BOP24
068400130924     c                   clear                   n_BOP48
068500130924     c                   clear                   n_BOOLT
068600130919     c
068700130305     c                   ENDSR
068800130305     C*------------------------------------------------------------------------*
068900130305     c     Stampa        BEGSR
069000130305     c* rileggo i dati
069100130305     c                   clear                   savfnrs
069200130305     c
069300130305     c     kpcl_sta      setll     wfpcl01l
069400130305     c     kpcl_sta      reade     wfpcl01l
069500130305     c
069600130312    1c                   dow       not %eof(wfpcl01l)
069700130305     c* a cambio fls/nrs ristampo la testata
069800130305     c                   eval      wflsnrs=(pclfls+100)+pclnrs
069900130305     c
070000130305     c                   if        wflsnrs<>savfnrs
070100130919     c                   seton                                        01
070200130305     c                   eval      savfnrs=wflsnrs
070300130305     c                   endif
070400130305     c
070500130305     c* Stampa  testata
070600130305     c
070700130305     c                   eval      stpfls=pclfls
070800130305     c                   eval      stpnrs=pclnrs
070900130307     c                   eval      stptbol=pcltbol
071000130305
071100130305     c                   if        *in01
071200130305     C                   write     LSw4PT
071300130305     C                   write     LSw4PI
071400130305     c                   setoff                                       01
071500130305     c                   endif
071600130305     c
071700130305     c* stampa detaglio
071800130305     c                   eval      stpfgs=pclfgs
071900130305     c     pclfgs        CHAIN     AZORG01L
072000130305     c                   if        %found(azorg01l)
072100130305     c                   eval      stpdfgs=orgdes
072200130305     c                   else
072300130305     c                   eval      stpdfgs=*all'?'
072400130305     c                   endif
072500130305     c
072600130305     c                   eval      stptncl=pcltncl
072700130305     c                   eval(H)   stptpkg=pcltpkg
072800130305     c                   eval(H)   stptvlm=pcltvlm
072900130305     c                   write     lsw4d2
073000130305     c
073100130305     c     kpcl_sta      reade     wfpcl01l
073200130312    1c                   enddo
073300130305     c
073400130312     c                   if        *in01
073500130312     C                   write     LSw4PT
073600130312     C                   write     LSw4PI
073700130312     c                   endif
073800130312     c
073900130305     C                   write     LSw4RI
074000130305     c
074100130305     c                   ENDSR
074200030414     C*---------------------------------------------------------------*
074300030414     C* CARTAB - Caricamento tabelle di wrk
074400030414     C*---------------------------------------------------------------*
074500030414     C     CARTAB        BEGSR
074600030423     C*
074700030423     C* Carico i P.O. del T.P. dell'utente reperendoli dalla £1
074800030423     C                   MOVEL     '£1'          D06COD
074900130305     C                   MOVEL     ilsw3tfp      D06KEY
075000030423     C                   MOVEL     'L'           D06TLA
075100030423     C                   MOVEL     *BLANKS       D06ESC
075200030423     C                   MOVEL(P)  TRUL06DS      KPJBU
075300030423     C                   CALL      'TRUL06R'
075400030423     C                   PARM                    KPJBA
075500030423     C                   MOVEL     KPJBU         TRUL06DS
075600030414     C*
075700130923     C                   MOVE      WTIME         WDATE             8 0
075800130923     C                   MOVEL     WTIME         UTIME             6 0
075900130923     C*
076000030414     C                   ENDSR
076100030414     C*------------------------------------------------------------------------*
076200030414     C* REPERISCE I DATI UTENTE
076300030414     C*------------------------------------------------------------------------*
076400030414     C     REPDATIUTE    BEGSR
076500030414     C*
076600030414     C* INIZIALIZZA VARIABILI DI WRK
076700030414     C                   CLEAR                   TIBS34DS
076800030414     C                   CLEAR                   AZUTEDS
076900030414     C                   CLEAR                   DDATIUTE
077000030414     C*
077100030414     C     *DTAARA       DEFINE    §azute        azuteds
077200030414     C     *DTAARA       DEFINE    §datiute      ddatiute
077300030414     C                   IN(E)     *DTAARA
077400030422     C                   IF        %Error
077500030422     C                   EVAL      I34Tla = 'L'
077600030414     C                   CALL      'TIBS34R'
077700030414     C                   PARM                    Tibs34Ds
077800030414     C                   IN        *DTAARA
077900030422     C                   ENDIF
078000030414     C*
078100030414     C                   ENDSR
078200001218     C*------------------------------------------------------------------------*
078300001218     C* *INZSR - ROUTINE INIZIALE
078400001218     C*------------------------------------------------------------------------*
078500001218     C     *INZSR        BEGSR
078600001218     C*
078700030422     C* Ricevo in la DS di architettura
078800010726     C     *ENTRY        PLIST
078900020613     C                   PARM                    KPJBA
079000130304     c                   movel     kpjbu         fnlsw3ds
079100130305     c
079200130305     c                   if        ilsw3dsa=0
079300130305     c                   eval      ilsw3dsa=ilsw3dsd
079400130305     c                   endif
079500030521     C*
079600030422     C* Inizializzo campi chiave "fissi"
079700030414     C                   Z-ADD     1             tblKUT
079800030414     C****
079900030414     C* Definizione chiavi
080000030414     C****
080100030512     C* Chiave su TABEL00F - parziale
080200030512     C     KEYtabP       KLIST
080300030414     C                   KFLD                    tblKUT
080400030414     C                   KFLD                    tblCOD
080500130304     C     KTBE2         KLIST
080600130304     C                   KFLD                    KCOD1
080700130304     C                   KFLD                    KKE1
080800130304     C                   KFLD                    KKE2
080900130304     C*
081000130304     C     kblp          KLIST
081100030422     C                   KFLD                    blpLNP
081200130304     C                   KFLD                    kaas              4 0
081300130304     C                   KFLD                    kmgs              4 0
081400130304     C     kblt          KLIST
081500130304     C                   KFLD                    blpaas
081600130304     C                   KFLD                    blplnp
081700130304     C                   KFLD                    blpnrs
081800130304     C                   KFLD                    blpnsp
081900130304     C     kbrv          KLIST
082000130304     C                   KFLD                    knpg              1 0
082100130304     C                   KFLD                    bltfls
082200130304     C                   KFLD                    bltlna
082300130304     C                   KFLD                    bltnrs
082400130304     C                   KFLD                    bltnsc
082500130925     C     kbrv7         KLIST
082600130925     C                   KFLD                    bltfls
082700130925     C                   KFLD                    bltlna
082800130925     C                   KFLD                    bltnrs
082900130925     C                   KFLD                    bltnsc
083000130305     C     kpcl          KLIST
083100130305     C                   KFLD                    dutpou
083200130305     C                   KFLD                    ilsw3tfp
083300130305     C                   KFLD                    ilsw3dsd
083400130305     C                   KFLD                    ilsw3dsa
083500130305     C                   KFLD                    kfls              3 0
083600130305     C                   KFLD                    knrs              2 0
083700130305     C                   KFLD                    kfgs              3 0
083800130305     c
083900130305     C     kpcl_sta      KLIST
084000130305     C                   KFLD                    dutpou
084100130305     C                   KFLD                    ilsw3tfp
084200130305     C                   KFLD                    ilsw3dsd
084300130305     C                   KFLD                    ilsw3dsa
084400030414     C*
084500030414     C* REPERIMEMTO DATI UTENTE
084600030414     C                   EXSR      REPDATIUTE
084700030422     C*
084800030422     C* CARICAMENTO TABELLE DI WRK
084900030422     C                   EXSR      CARTAB
085000130919     c
085100130919     c* Giro la richiesta date
085200130919     c                   eval      dataiso=%date(ilsw3dsd)
085300130919     c                   eval      dataeur=dataiso
085400130919     c                   eval      stpdsd=%dec(dataeur)
085500130923     c                   seton                                        0102
085600130919
085700130919     c                   if        ilsw3dsa<>ilsw3dsd and ilsw3dsa>0
085800130919     c                   eval      dataiso=%date(ilsw3dsa)
085900130919     c                   eval      dataeur=dataiso
086000130919     c                   eval      stpdsa=%dec(dataeur)
086100130919     c                   endif
086200130919
086300130919     c     ilsw3tfp      CHAIN     AZORG01L
086400130919     c                   if        %found(azorg01l)
086500130919     c                   eval      stpdtfp=orgdes
086600130919     c                   endif
086700130925     c
086800130925     c                   eval      stptfp=ilsw3tfp
086900130305      /free
087000130305       setll (dutpou)  wfpcl01l   ;
087100130305       reade (dutpou)  wfpcl01l   ;
087200130305
087300130305       dow  not %eof(wfpcl01l)   ;
087400130305       delete wfpcl000  ;
087500130305
087600130305       reade (dutpou)  wfpcl01l   ;
087700130305       enddo  ;
087800130305
087900130305      /end-free
088000130305
088100001218     C                   ENDSR
