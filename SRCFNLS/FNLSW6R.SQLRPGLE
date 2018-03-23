000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300150910     h*   Conteggio colli in partenza da più filiali
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
002100150910     D dsblp         E DS                  extname(fnblp00f)
002200030422     D*--------------------
002300030422     D* SCHIERE DI MEMORIZZAZIONE
002400030422     D*--------------------
002500130305     d* tfp abilitati a far partire colli di altro tfp
002600130305     D xcotfp          S              3    DIM(85)
002700130305     D ncl             S              9  0 DIM(85)
002800130305     D puc             S             13  3 DIM(85)
002900130305     D pucB            S             13  3 DIM(85)
003000130305     D vuc             S             13  6 DIM(85)
003100130305     D vucB            S             13  6 DIM(85)
003200130919
003300130919     d* Memorizzo colli e quando partiti rispetto alla bolla
003400130919     D Moltre          s              7  0 dim(85)
003500130919     D M48             s              7  0 dim(85)
003600130919     D M24             s              7  0 dim(85)
003700130919     D confo           s              7  0 dim(85)
003800130919     D Poltre          s              7  0 dim(85)
003900130919     D P48             s              7  0 dim(85)
004000130919     D P24             s              7  0 dim(85)
004100130919     d* Memorizzo colli bolle non conformi
004200130919     D N_Moltre        s              7  0 dim(85)
004300130919     D n_M48           s              7  0 dim(85)
004400130919     D n_M24           s              7  0 dim(85)
004500130919     D n_confo         s              7  0 dim(85)
004600130919     D n_Poltre        s              7  0 dim(85)
004700130919     D n_P48           s              7  0 dim(85)
004800130919     D n_P24           s              7  0 dim(85)
004900130924     D n_boco          s              5  0 dim(85)
005000130924     D n_bop24         s              5  0 dim(85)
005100130924     D n_bop48         s              5  0 dim(85)
005200130924     D n_boOLT         s              5  0 dim(85)
005300130919     d* Memorizzo colli conformi
005400130919     D C_confo         s              7  0 dim(85)
005500130919     d
005600030414     D*--------------------
005700030414     D* VARIABILI DI WRK
005800030414     D*--------------------
005900130305     D conta           s              2  0
006000130305     D YY              s              4  0
006100130305     D I               s              4  0
006200130304     D kke1            s                   like(tbeke1)
006300130304     D kke2            s                   like(tbeke2)
006400130304     D kcod1           s                   like(tbecod)
006500130305     D wpes            s                   like(blppkf)
006600130305     D wvol            s                   like(blpvlf)
006700130305     D savfnrs         s                   like(wflsnrs)
006800130304     D Wflsnrs         s              5  0
006900130305     D Wblpdsp         s              8  0
007000130305     D unipkf          s              9  3
007100130305     D univol          s              9  6
007200130305     D Indx            s              3  0
007300130919     d
007400130925     D n_cltfp         s              7  0
007500130925     D c_cltfp         s              7  0
007600130919     D c_colli         s              7  0
007700130919     D n_colli         s              7  0
007800130920     D n_nospu         s              7  0
007900130925     D n_spumai        s              7  0
008000130919     D c_bolle         s              7  0
008100130919     D n_bolle         s              7  0
008200130919     D c_bolleCO       s              7  0
008300130919     D c_bolleNO       s              7  0
008400130919     D n_bolleCO       s              7  0
008500130919     D n_bolleNO       s              7  0
008600150909     d wrkgetlista     s           4096    varying
008700130919     d
008800130305     d Dataiso         s               d   datfmt(*iso)
008900130305     d Dataeur         s               d   datfmt(*eur)
009000130923     d Datasys         s               d   datfmt(*iso) inz(*sys)
009100030414     D*---
009200030414     D* DS REPERIMENTO DATI UTENTE
009300030414     D*---
009400030414     D TIBS34DS      E DS
009500030414     D DDATIUTE      E DS
009600030414     D AZUTEDS       E DS                  extname(AZUTE00F)
009700030423     D*---
009800030423     D* DS REPERIMENTO £ire
009900030423     D*---
010000030423     D TRUL06DS      E DS
010100030423     D  L1                     1     90  0 DIM(30)
010200030521     D*------------------
010300030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
010400030521     D*------------------
010500030521     D WLBDA8          DS                  INZ
010600030521     D  G08DAT                 1      8  0
010700030521     D  G08INV                 9     16  0
010800030521     D  G08ERR                17     17
010900030521     D  G08TGI                18     22  0
011000130919
011100130919     D WDAT8           DS
011200130919     D  DATADA                 1      8  0
011300130919     D  DATAA                  9     16  0
011400130919     D  GGL                   17     21  0
011500130919     D tfptfa          DS
011600130919     D  p_tfp                  1      3  0
011700130919     D  p_tfa                  4      6  0
011800001218
011900130304     D*------------------
012000130304     D FNLSW3DS        DS
012100130304     D  ilsw3dsd               1      8  0
012200130304     D  ilsw3dsa               9     16  0
012300130304     D  ilsW3TFP              18     20  0
012400130923     D  ilsw3Cont             21     21
012500130923     D  ilsw3sprg             22     22
012600130304
012700920812     C*---------------------------------------------------------------*
012800001218     C* MAIN
012900001218     C*---------------------------------------------------------------*
013000130304     c*
013100150910     c* la stampa delle spareggiate qui non la faccio mai!!!
013200150910     c                   eval      ilsw3sprg=' '
013300150909     c
013400150909     c                   clear                   xcotfp
013500150909     C                   MOVEL     'XCO'         kcod1
013600150909     C                   eval      kke1='RANDOM'
013700150909     c     ktbe3         setll     tntbe01l
013800150909     c     ktbe3         reade     tntbe01l
013900150910    0c                   dow       not %eof(tntbe01l)
014000150910   0ac                   if        tbeatb=' '
014100150909     C                   MOVEA     TBeUNI        xcotfp
014200150909     c                   eval      Wflsnrs=%int(%subst(tbeke2:1:5))
014300150909     c                   movel     wflsnrs       wfls              3 0
014400150909     c                   move      wflsnrs       wnrs              2 0
014500150909     c
014600150909     c* Nella schiera aggiungo sempre me stesso
014700150909     c                   movel     ilsw3tfp      alfafgs           3
014800150910    1c                   if        %lookup(alfafgs:xcotfp)=0
014900150909     c                   eval      Indx=%lookup('   ':xcotfp)
015000150909     c                   eval      xcotfp(indx)=%editc(ilsw3tfp:'X')
015100150909     c                   endif
015200150910     c                   eval      savfnrs=wflsnrs
015300150909     c*
015400130306     c                   clear                   contabo           5 0
015500150909     c* Preparo sql
015600150909     c                   exsr      prepasql
015700150910     c* eseguo
015800150909     c                   exsr      sqlesegui
015900030512     C*
016000150910     c* memorizzo i dati per fls/nrs nel file di work
016100150910     c                   if        savfnrs>0
016200150910     c                   exsr      memdat
016300150910     c                   endif
016400150910   0ac                   endif
016500150910     c
016600150910     c
016700150910     c     ktbe3         reade     tntbe01l
016800150910    0C                   enddo
016900020613     C*
017000130305     c* memorizzo i dati per fls/nrs nel file di work
017100130305     c                   if        savfnrs>0
017200130305     c                   exsr      memdat
017300130305     c                   endif
017400130305     c
017500130923     c                   if        ilsw3cont='S'
017600130305     c                   EXSR      Stampa
017700130923     c                   endif
017800130306     c
017900130306     C                   CLEAR                   fnlv53ds
018000130306     C                   MOVEL     'C'           D53TLA
018100130306     C                   CALL      'FNLV53R'
018200130306     C                   PARM                    fnlv53ds
018300130923     C                   CLEAR                   tnsd99ds
018400130923     C                   MOVEL     'C'           D98TLA
018500130923     C                   CALL      'TNSD99R'
018600130923     C                   PARM                    tnsd99ds
018700130305     C                   seton                                        LR
018800150910     C*------------------------------------------------------------------------*
018900150910     c* Leggo collo per collo e controllo chi lo ha spuntato
019000150910     c     Elabora       BEGSR
019100150910     c
019200150910     c                   clear                   Wnoconfo          1
019300150910     c                   clear                   moltre
019400150910     c                   clear                   m48
019500150910     c                   clear                   m24
019600150910     c                   clear                   p48
019700150910     c                   clear                   p24
019800150910     c                   clear                   poltre
019900150910     c                   clear                   confo
020000150910     c                   clear                   savfgs            3 0
020100150910     c                   clear                   savdt_b           8 0
020200150910     c
020300150910     c                   eval      wblpdsp=(blpaas*10000)+blpmgs
020400150910     c
020500150910     c     kblt          setll     fnblt01l
020600150910     c     kblt          reade     fnblt01l
020700150910     c
020800150910    5c                   dow       not %eof(fnblt01l)
020900150910     c
021000150910     c                   exsr      leggispu
021100150910     c
021200150910     c     kblt          reade     fnblt01l
021300150910     c                   enddo
021400150910     c
021500150910     c* Se non ha i tempi di resa li calcolo
021600150910     c                   if        blpdce=0
021700150910     C*
021800150910     C                   MOVEL     ' '           D98TLA
021900150910     C                   MOVEL     'P'           D98TBO
022000150910     C                   Z-ADD     blpAAS        D98AAS
022100150910     C                   Z-ADD     blpLNP        D98LNP
022200150910     C                   Z-ADD     blpNRS        D98NRS
022300150910     C                   Z-ADD     blpnSP        D98NSP
022400150910     c* A udate e utime se non consseganta
022500150910     c                   if        blpdcm=0
022600150910     C                   movel     datasys       D98DFC
022700150910     C                   TIME                    WTIME            14 0
022800150910     c                   endif
022900150910     c
023000150910     C                   MOVEL     UTIME         D98OFC
023100150910     C                   CALL      'TNSD99R'
023200150910     C                   PARM                    tnsd99ds
023300150910     C*
023400150910     C                   Z-ADD     D98Nrc        blpnrc
023500150910     c                   endif
023600150910     c
023700150910     c* Verifico se ci sono colli dal terminal di partenza bolle
023800150910     c                   clear                   nocltfp           1
023900150910     c                   movel     ilsw3tfp      alfafgs           3
024000150910     c                   eval      Indx=%lookup(alfafgs:xcotfp)
024100150910     c                   if        moltre(indx)=0 and m48(indx)=0 and
024200150910     c                             m24(indx)=0 and confo(indx)=0 and
024300150910     c                             poltre(indx)=0 and p48(indx)=0 and
024400150910     c                             p24(indx)=0
024500150910     c                   eval      nocltfp='S'
024600150910     c                   else
024700150910     c                   eval      nocltfp='N'
024800150910     c                   endif
024900150910     c
025000150910     c
025100150910     c* Memorizzo nelle spareggiate o conformi
025200150910    1c                   if        wnoconfo='S'
025300150910     c                   add       moltre        n_moltre
025400150910     c                   add       m24           n_m24
025500150910     c                   add       m48           n_m48
025600150910     c                   add       confo         n_confo
025700150910     c                   add       poltre        n_poltre
025800150910     c                   add       p24           n_p24
025900150910     c                   add       p48           n_p48
026000150910     c                   add       blpncl        n_colli
026100150910     c                   add       1             n_bolle
026200150910     c                   if        nocltfp='S'
026300150910     c                   add       1             n_cltfp
026400150910     c                   endif
026500150910     c
026600150910    2c                   if        blpnrc<=0
026700150910     c                   add       1             n_bolleCO
026800150910    2c                   else
026900150910     c                   add       1             n_bolleNO
027000150910    2c                   endif
027100150910     c* La bolla va conteggiata alla filiale con data + alta tra bolla
027200150910     c*  e spunte
027300150910     c* Controllo anche data spedizione rispetto alla data spunta
027400150910    2c                   if        wblpdsp>savdt_b
027500150910     c                   eval      savdt_B=wblpdsp
027600150910     c                   eval      savfgs=blplnp
027700150910    2c                   endif
027800150910     c
027900150910     c                   clear                   wdat8
028000150910     c                   clear                   tfptfa
028100150910     c                   eval      datada=savdt_b
028200150910     c                   eval      dataa =wblpdsp
028300150910     c                   eval      p_tfp=blptfp
028400150910     c                   eval      p_tfa=blptfa
028500150910     c                   call      'XSRLAV8'
028600150910     c                   parm                    wdat8
028700150910     c                   parm                    tfptfa
028800150910     c                   movel     savfgs        alfafgs           3
028900150910     c                   eval      Indx=%lookup(alfafgs:xcotfp)
029000150910
029100150910    2c                   if        ggl<=1
029200150910     c                   add       1             n_boco(indx)
029300150910   x2c                   else
029400150910     c                   select
029500150910    3c                   when      ggl=2
029600150910     c                   add       1             n_bop24(indx)
029700150910     c                   when      ggl=3
029800150910     c                   add       1             n_bop48(indx)
029900150910     c                   other
030000150910     c                   add       1             n_boolt(indx)
030100150910    3c                   endsl
030200150910    2c                   endif
030300150910     c
030400150910   x1c                   else
030500150910     c                   add       confo         c_confo
030600150910     c                   add       blpncl        c_colli
030700150910     c                   add       1             c_bolle
030800150910     c                   if        nocltfp='S'
030900150910     c                   add       1             c_cltfp
031000150910     c                   endif
031100150910    2c                   if        blpnrc<=0
031200150910     c                   add       1             c_bolleCO
031300150910     c                   else
031400150910     c                   add       1             c_bolleNO
031500150910    2c                   endif
031600150910    1c                   endif
031700150910     c                   ENDSR
031800001218     C*------------------------------------------------------------------------*
031900130304     c     LeggiSpu      BEGSR
032000130304     c* Leggo spunta entrata o partenza se c'e' di un'altra filiale
032100130304     c*  prevista in XCOTFP
032200130304     c                   z-add     5             knpg              1 0
032300130305     c                   clear                   spufgs            3 0
032400130305     c                   eval      conta=1
032500130305
032600130305    1c                   dow       spufgs=0  and conta<=2
032700130304     c
032800130304     c     kbrv          setll     fnbrv09l
032900130304     c     kbrv          reade     fnbrv09l
033000130305    2c                   dow       not %eof(fnbrv09l)
033100130304     c*
033200130306   2ac                   if        brvdcs>0
033300130306     c                   clear                   fnlv53ds
033400130306     c* se si tratta di spunta partenza devo escludere i fogli defluenza
033500130306   2bc                   if        brvnpg=1
033600130306     C                   MOVEL     'G'           D53TFO
033700130306     C                   Z-ADD     brvNFV        D53NFV
033800130306     C                   MOVEL     brvnpg        D53NPG
033900130306     C                   MOVEL     brvFGS        D53FGS
034000130306     C                   CALL      'FNLV53R'
034100130306     C                   PARM                    fnlv53ds
034200130306   2bc                   endif
034300130306     c
034400130306   2bC                   if        brvnpg=5 or d53def=' '
034500130305     c                   movel     brvfgs        alfafgs           3
034600130305     c                   eval      Indx=%lookup(alfafgs:xcotfp)
034700130304     c
034800130305    3c                   if        indx>0
034900130305     c                   eval      spufgs=brvfgs
035000130304     c                   leave
035100130305    3c                   endif
035200130306   2bc                   endif
035300130306   2ac                   endif
035400030512
035500130304     c     kbrv          reade     fnbrv09l
035600130305    2c                   enddo
035700130305    c
035800130304     c                   z-add     1             knpg
035900130305     c                   eval      conta=conta+1
036000130305    1c                   enddo
036100130304     c
036200130920     c* Trovata spunta di altra filiale autorizzata --> conteggio
036300130305    1c                   if        spufgs>0
036400130304     c* Colli
036500130304     c                   add       1             ncl(indx)
036600130304     c
036700130304     c* peso
036800130305     C*        : - SE TOTALE  , uso   BLPpkc
036900130305     C*          - SE PARZIALE, uso   BLPPKC (SE MAGGIORE)
037000130305     C*                               ALTRIMENTI uso  BLPPKF
037100130305     C     BLPNCp        ifeq      BLPNCL
037200130305     C                   Z-ADD     BLPpkC        Wpes
037300130305     C*
037400130305   X4C                   ELSE
037500130305    5C     BLPpkC        IFGE      BLPpkF
037600130305     C                   Z-ADD     BLPpkC        Wpes
037700130305   X5C                   ELSE
037800130305     C                   Z-ADD     BLPpkF        Wpes
037900130305    5C                   ENDIF
038000130305    4C                   ENDIF
038100130305     c
038200130304     c                   select
038300130304     c                   when      brvpuc>0
038400130305     c                   add       brvpuc        puc(indx)
038500130305
038600130304     c                   when      bltpuc>0
038700130305     c                   add       bltpuc        puc(indx)
038800130304     c                   other
038900130304     c
039000130305     c                   eval(H)   unipkf    =wpes  /blpncl
039100130305     c                   add       unipkf        puc(indx)
039200130304     c                   endsl
039300130305     c* sempre media spedizione
039400130305     c                   eval(H)   unipkf    =wpes  /blpncl
039500130305     c                   add       unipkf        pucb(indx)
039600130304     c* volume
039700130305     C*        : - SE TOTALE  , uso   BLPpkc
039800130305     C*          - SE PARZIALE, uso   BLPPKC (SE MAGGIORE)
039900130305     C*                               ALTRIMENTI uso  BLPPKF
040000130305     C     BLPNCR        IFEQ      BLPNCL
040100130305     C                   Z-ADD     BLPVLC        WVOL
040200130305     C*
040300130305   X4C                   ELSE
040400130305    5C     BLPVLC        IFGE      BLPVLF
040500130305     C                   Z-ADD     BLPVLC        WVOL
040600130305   X5C                   ELSE
040700130305     C                   Z-ADD     BLPVLF        WVOL
040800130305    5C                   ENDIF
040900130305    4C                   ENDIF
041000130305     c
041100130920    2c                   select
041200130920    2c                   when      brvvuc>0
041300130305     c                   add       brvvuc        vuc(indx)
041400130305
041500130920    2c                   when      bltvuc>0
041600130305     c                   add       bltvuc        vuc(indx)
041700130920   x2c                   other
041800130305     c
041900130305     c                   eval(H)   univol    =wvol  /blpncl
042000130305     c                   add       univol        vuc(indx)
042100130920    2c                   endsl
042200130305
042300130305     c* sempre media spedizione
042400130305     c                   eval(H)   univol    =wvol  /blpncl
042500130305     c                   add       univol        vucb(indx)
042600130919     c
042700130919     c* Oltre al conteggio verifico se sono partite in modo spareggiato
042800130919     c*  se data spunta = data spedizione --> conforme altrimenti conto i
042900130919     c*  giorni lavorativi tra le date
043000130920    2c                   if        brvdcs=wblpdsp
043100130923     c                   add       1             confo(indx)
043200130924     c                   if        brvdcs>savdt_b
043300130924     c                   eval      savdt_B=brvdcs
043400130924     c                   eval      savfgs=spufgs
043500130924     c                   endif
043600130924
043700130920   x2c                   else
043800130919     c
043900130920     c                   clear                   wdat8
044000130919     c                   clear                   tfptfa
044100130919     c                   eval      datada=brvdcs
044200130919     c                   eval      dataa =wblpdsp
044300130919     c                   eval      p_tfp=blptfp
044400130919     c                   eval      p_tfa=blptfa
044500130919     c                   call      'XSRLAV8'
044600130920     c                   parm                    wdat8
044700130919     c                   parm                    tfptfa
044800130924
044900130924     c                   if        brvdcs>savdt_b
045000130924     c                   eval      savdt_B=brvdcs
045100130924     c                   eval      savfgs=spufgs
045200130924     c                   endif
045300130919
045400130924    3c                   if        ggl<=1
045500130923     c                   add       1             confo(indx)
045600130924     c
045700130919   x3c                   else
045800130919
045900130919     c                   eval      Wnoconfo='S'
046000130919
046100130920    4c                   if        wblpdsp>brvdcs
046200130919
046300130920     c                   select
046400130923    5c                   when      ggl=2
046500130919     c                   add       1             m24(indx)
046600130923     c                   when      ggl=3
046700130919     c                   add       1             m48(indx)
046800130919     c                   other
046900130919     c                   add       1             moltre(indx)
047000130919    5c                   endsl
047100130919   x4c                   else
047200130919   c
047300130920     c                   select
047400130923    5c                   when      ggl=2
047500130919     c                   add       1             p24(indx)
047600130923     c                   when      ggl=3
047700130919     c                   add       1             p48(indx)
047800130919     c                   other
047900130919     c                   add       1             poltre(indx)
048000130919    5c                   endsl
048100130919     c
048200130919    4c                   endif
048300130919    3c                   endif
048400130919    2c                   endif
048500130919     c
048600130920   x1c                   else
048700130920     c* se collo non ancora spuntato, conteggio a parte
048800130920     c                   eval      Wnoconfo='S'
048900130920     c* e metto la bolla tra le spareggiate
049000130920     c                   add       1             n_nospu
049100130925     c* Verifico se mai spuntato senon ha la data di consegna
049200130925    2c                   if        bltdcm=0
049300130925     c     kbrv7         setll     fnbrv07l
049400130925     c     kbrv7         reade     fnbrv07l
049500130925    3c                   dow       not %eof(fnbrv07l) and brvdcs=0
049600130925     c     kbrv7         reade     fnbrv07l
049700130925    3c                   enddo
049800130925     c
049900130925    3c                   if        %eof(fnbrv07l)
050000130925     c                   add       1             n_spumai
050100130925    3c                   endif
050200130925
050300130925    2c                   endif
050400130920     c
050500130919    1c                   endif
050600130304     c
050700130304     c                   ENDSR
050800130305     C*------------------------------------------------------------------------*
050900130305     c     Memdat        BEGSR
051000130305     c* Memorizzo i dati
051100130305     c* se  già scritto --> vado in aggiunta
051200130305     c
051300130305     c                   z-add     1             yy
051400130305    1c                   do        85            yy
051500130305
051600130305    2c                   if        ncl(yy)>0
051700130305     c                   movel     xcotfp(YY)    kfgs
051800130305     c                   movel     wflsnrs       kfls
051900130305     c                   move      wflsnrs       knrs
052000130305     c
052100130305     c     kpcl          chain     wfpcl01l
052200130305    3c                   if        not %found(wfpcl01l)
052300130305     c                   clear                   wfpcl000
052400130305     c                   eval      pclfil=dutpou
052500130305     c                   eval      pcltfp=ilsw3tfp
052600130305     c                   eval      pclfls=kfls
052700130305     c                   eval      pclnrs=knrs
052800130305     c                   eval      pcldsd=ilsw3dsd
052900130305     c                   eval      pcldsa=ilsw3dsa
053000130305     c                   eval      pclfgs=kfgs
053100130305    3c                   endif
053200130305     c                   add       ncl(yy)       pcltncl
053300130305     c                   add       puc(yy)       pcltpkg
053400130305     c                   add       pucb(yy)      pcltpkgb
053500130305     c                   add       vuc(yy)       pcltvlm
053600130305     c                   add       vucb(yy)      pcltvlmb
053700130307     c                   add       stptbol       pcltbol
053800130305     c
053900130305    3c                   if        not %found(wfpcl01l)
054000130305     c                   write     wfpcl000
054100130305     c                   else
054200130305     c                   update    wfpcl000
054300130305    3c                   endif
054400130305    2c                   endif
054500130305    1c                   enddo
054600130305     c
054700130305     c                   clear                   ncl
054800130305     c                   clear                   puc
054900130305     c                   clear                   pucb
055000130305     c                   clear                   vuc
055100130305     c                   clear                   vucb
055200130307     c                   clear                   stptbol
055300130305     c
055400130919     c* Stampa statistica spareggiate
055500130920     c     savfnrs       div       100           stpfls
055600130919     c                   mvr                     stpnrs
055700130919     c
055800130919
055900130923     c* Solo se richiesta la stampa delle spareggiate
056000130923     c                   if        ilsw3sprg='S'
056100130923     c
056200130919     c* Stampo prima le bolle conformi
056300130919     c                   z-add     1             XX                3 0
056400130923     c* totale bolle e colli
056500130923     c                   eval      stpbolle='  BOLLE PARTITE OK :'
056600130923     C
056700130923     C                   EVAL      STPTOTC =C_COLLI
056800130923     C                   EVAL      STPTOTB =C_BOLLE
056900130923     C                   EVAL      STPTOTB_c =C_BOLLECO
057000130923     C                   EVAL      STPTOTB_n =C_BOLLENO
057100130925     C                   EVAL      STPNOSIM  =C_cltfp
057200130924     c                   clear                   stpnospu
057300130924     c                   clear                   stpb_co
057400130924     c                   clear                   stpb_p24
057500130924     c                   clear                   stpb_p48
057600130924     c                   clear                   stpb_olt
057700130923
057800130923     C                   write     LSw4PT1
057900130923     C                   write     LSw4PI1
058000130923     c                   write     lsw4d31
058100130923     C                   write     LSw4PI2
058200130923
058300130919     c                   dow       xcotfp(xx)>*zeros
058400130919
058500130919     c                   movel     xcotfp(xx)    stpfgs
058600130919     c     stpfgs        CHAIN     AZORG01L
058700130919     c                   if        %found(azorg01l)
058800130919     c                   eval      stpdfgs=orgdes
058900130919     c                   else
059000130919     c                   eval      stpdfgs=*all'?'
059100130919     c                   endif
059200130920     c                   z-add     c_confo(xx)   stpconfo
059300130919     c
059400130919     c                   write     lsw4d21
059500130919
059600130919     c                   add       1             xx
059700130919     c                   enddo
059800130923
059900130923     c                   write     lsw4RI1
060000130919     c
060100130919     c* Stampo poi   le bolle NON conformi
060200130919     c                   z-add     1             XX                3 0
060300130923     c* totale bolle e colli
060400130923     c                   eval      stpbolle='  BOLLE SPAREGGIATE:'
060500130923     C
060600130923     C                   EVAL      STPnospu=N_NOSPU
060700130923     C                   EVAL      STPTOTC =N_COLLI
060800130923     C                   EVAL      STPTOTB =N_BOLLE
060900130923     C                   EVAL      STPTOTB_C =N_BOLLECO
061000130923     C                   EVAL      STPTOTB_n =N_BOLLENO
061100130925     C                   EVAL      STPNOSIM  =N_cltfp
061200130925     C                   EVAL      STPmai    =N_spumai
061300130923
061400130923     c                   write     lsw4d31
061500130923     C                   write     LSw4PI2
061600130919     c                   dow       xcotfp(xx)>*zeros
061700130919
061800130919     c                   movel     xcotfp(xx)    stpfgs
061900130919     c     stpfgs        CHAIN     AZORG01L
062000130919     c                   if        %found(azorg01l)
062100130919     c                   eval      stpdfgs=orgdes
062200130919     c                   else
062300130919     c                   eval      stpdfgs=*all'?'
062400130919     c                   endif
062500130919     c                   z-add     n_confo(xx)   stpconfo
062600130924     c                   z-add     n_moltre(xx)  stpMOlt
062700130919     c                   z-add     n_m48(xx)     stpM48
062800130919     c                   z-add     n_m24(xx)     stpM24
062900130919     c                   z-add     n_p24(xx)     stpp24
063000130919     c                   z-add     n_p48(xx)     stpp48
063100130924     c                   z-add     n_poltre(xx)  stppolt
063200130924     C                   EVAL      STPb_co   =N_BOco(xx)
063300130924     C                   EVAL      STPb_p24  =N_BOp24(xx)
063400130924     C                   EVAL      STPb_p48  =N_BOp48(xx)
063500130924     C                   EVAL      STPb_olt  =N_BOolt(xx)
063600130919     c
063700130919     c                   write     lsw4d21
063800130919
063900130919     c                   add       1             xx
064000130919     c                   enddo
064100130923
064200130923     c                   write     lsw4RI1
064300130923     c                   endif
064400130919     C
064500130925     C                   CLEAR                   n_spumai
064600130925     C                   CLEAR                   n_nospu
064700130925     C                   CLEAR                   n_cltfp
064800130925     C                   CLEAR                   c_cltfp
064900130925     C                   CLEAR                   n_colli
065000130919     C                   CLEAR                   n_bolle
065100130919     C                   CLEAR                   n_bolleco
065200130919     C                   CLEAR                   n_bolleno
065300130919     C                   CLEAR                   c_colli
065400130919     C                   CLEAR                   c_bolle
065500130919     C                   CLEAR                   c_bolleco
065600130919     C                   CLEAR                   c_bolleno
065700130919     c                   clear                   c_confo
065800130919     c                   clear                   n_confo
065900130919     c                   clear                   n_moltre
066000130919     c                   clear                   n_m48
066100130919     c                   clear                   n_m24
066200130919     c                   clear                   n_p24
066300130919     c                   clear                   n_p48
066400130919     c                   clear                   n_poltre
066500130924     c                   clear                   n_BOCO
066600130924     c                   clear                   n_BOP24
066700130924     c                   clear                   n_BOP48
066800130924     c                   clear                   n_BOOLT
066900130919     c
067000130305     c                   ENDSR
067100130305     C*------------------------------------------------------------------------*
067200130305     c     Stampa        BEGSR
067300130305     c* rileggo i dati
067400130305     c                   clear                   savfnrs
067500130305     c
067600130305     c     kpcl_sta      setll     wfpcl01l
067700130305     c     kpcl_sta      reade     wfpcl01l
067800130305     c
067900130312    1c                   dow       not %eof(wfpcl01l)
068000130305     c* a cambio fls/nrs ristampo la testata
068100130305     c                   eval      wflsnrs=(pclfls+100)+pclnrs
068200130305     c
068300130305     c                   if        wflsnrs<>savfnrs
068400150910     c                   if        savfnrs>0
068500150910     C                   write     LSw4RI
068600150910     c                   endif
068700150910     c
068800130919     c                   seton                                        01
068900130305     c                   eval      savfnrs=wflsnrs
069000130305     c                   endif
069100130305     c
069200130305     c* Stampa  testata
069300130305     c
069400130305     c                   eval      stpfls=pclfls
069500130305     c                   eval      stpnrs=pclnrs
069600130307     c                   eval      stptbol=pcltbol
069700130305
069800130305     c                   if        *in01
069900130305     C                   write     LSw4PT
070000130305     C                   write     LSw4PI
070100130305     c                   setoff                                       01
070200130305     c                   endif
070300130305     c
070400130305     c* stampa detaglio
070500130305     c                   eval      stpfgs=pclfgs
070600130305     c     pclfgs        CHAIN     AZORG01L
070700130305     c                   if        %found(azorg01l)
070800130305     c                   eval      stpdfgs=orgdes
070900130305     c                   else
071000130305     c                   eval      stpdfgs=*all'?'
071100130305     c                   endif
071200130305     c
071300130305     c                   eval      stptncl=pcltncl
071400130305     c                   eval(H)   stptpkg=pcltpkg
071500130305     c                   eval(H)   stptvlm=pcltvlm
071600130305     c                   write     lsw4d2
071700130305     c
071800130305     c     kpcl_sta      reade     wfpcl01l
071900130312    1c                   enddo
072000130305     c
072100150910     c                   if        savfnrs>0
072200130312     c                   if        *in01
072300130312     C                   write     LSw4PT
072400130312     C                   write     LSw4PI
072500130312     c                   endif
072600130312     c
072700130305     C                   write     LSw4RI
072800150910     c                   endif
072900130305     c
073000130305     c                   ENDSR
073100030414     C*---------------------------------------------------------------*
073200030414     C* CARTAB - Caricamento tabelle di wrk
073300030414     C*---------------------------------------------------------------*
073400030414     C     CARTAB        BEGSR
073500030423     C*
073600030423     C* Carico i P.O. del T.P. dell'utente reperendoli dalla £1
073700030423     C                   MOVEL     '£1'          D06COD
073800130305     C                   MOVEL     ilsw3tfp      D06KEY
073900030423     C                   MOVEL     'L'           D06TLA
074000030423     C                   MOVEL     *BLANKS       D06ESC
074100030423     C                   MOVEL(P)  TRUL06DS      KPJBU
074200030423     C                   CALL      'TRUL06R'
074300030423     C                   PARM                    KPJBA
074400030423     C                   MOVEL     KPJBU         TRUL06DS
074500030414     C*
074600130923     C                   MOVE      WTIME         WDATE             8 0
074700130923     C                   MOVEL     WTIME         UTIME             6 0
074800130923     C*
074900030414     C                   ENDSR
075000030414     C*------------------------------------------------------------------------*
075100030414     C* REPERISCE I DATI UTENTE
075200030414     C*------------------------------------------------------------------------*
075300030414     C     REPDATIUTE    BEGSR
075400030414     C*
075500030414     C* INIZIALIZZA VARIABILI DI WRK
075600030414     C                   CLEAR                   TIBS34DS
075700030414     C                   CLEAR                   AZUTEDS
075800030414     C                   CLEAR                   DDATIUTE
075900030414     C*
076000030414     C     *DTAARA       DEFINE    §azute        azuteds
076100030414     C     *DTAARA       DEFINE    §datiute      ddatiute
076200030414     C                   IN(E)     *DTAARA
076300030422     C                   IF        %Error
076400030422     C                   EVAL      I34Tla = 'L'
076500030414     C                   CALL      'TIBS34R'
076600030414     C                   PARM                    Tibs34Ds
076700030414     C                   IN        *DTAARA
076800030422     C                   ENDIF
076900030414     C*
077000030414     C                   ENDSR
077100001218     C*------------------------------------------------------------------------*
077200001218     C* *INZSR - ROUTINE INIZIALE
077300001218     C*------------------------------------------------------------------------*
077400001218     C     *INZSR        BEGSR
077500001218     C*
077600030422     C* Ricevo in la DS di architettura
077700010726     C     *ENTRY        PLIST
077800020613     C                   PARM                    KPJBA
077900130304     c                   movel     kpjbu         fnlsw3ds
078000130305     c
078100130305     c                   if        ilsw3dsa=0
078200130305     c                   eval      ilsw3dsa=ilsw3dsd
078300130305     c                   endif
078400030521     C*
078500030422     C* Inizializzo campi chiave "fissi"
078600030414     C                   Z-ADD     1             tblKUT
078700030414     C****
078800030414     C* Definizione chiavi
078900030414     C****
079000030512     C* Chiave su TABEL00F - parziale
079100030512     C     KEYtabP       KLIST
079200030414     C                   KFLD                    tblKUT
079300030414     C                   KFLD                    tblCOD
079400130304     C     KTBE2         KLIST
079500130304     C                   KFLD                    KCOD1
079600130304     C                   KFLD                    KKE1
079700130304     C                   KFLD                    KKE2
079800150909     C     KTBE3         KLIST
079900150909     C                   KFLD                    KCOD1
080000150909     C                   KFLD                    KKE1
080100130304     C*
080200130304     C     kblp          KLIST
080300030422     C                   KFLD                    blpLNP
080400130304     C                   KFLD                    kaas              4 0
080500130304     C                   KFLD                    kmgs              4 0
080600130304     C     kblt          KLIST
080700130304     C                   KFLD                    blpaas
080800130304     C                   KFLD                    blplnp
080900130304     C                   KFLD                    blpnrs
081000130304     C                   KFLD                    blpnsp
081100130304     C     kbrv          KLIST
081200130304     C                   KFLD                    knpg              1 0
081300130304     C                   KFLD                    bltfls
081400130304     C                   KFLD                    bltlna
081500130304     C                   KFLD                    bltnrs
081600130304     C                   KFLD                    bltnsc
081700130925     C     kbrv7         KLIST
081800130925     C                   KFLD                    bltfls
081900130925     C                   KFLD                    bltlna
082000130925     C                   KFLD                    bltnrs
082100130925     C                   KFLD                    bltnsc
082200130305     C     kpcl          KLIST
082300130305     C                   KFLD                    dutpou
082400130305     C                   KFLD                    ilsw3tfp
082500130305     C                   KFLD                    ilsw3dsd
082600130305     C                   KFLD                    ilsw3dsa
082700130305     C                   KFLD                    kfls              3 0
082800130305     C                   KFLD                    knrs              2 0
082900130305     C                   KFLD                    kfgs              3 0
083000130305     c
083100130305     C     kpcl_sta      KLIST
083200130305     C                   KFLD                    dutpou
083300130305     C                   KFLD                    ilsw3tfp
083400130305     C                   KFLD                    ilsw3dsd
083500130305     C                   KFLD                    ilsw3dsa
083600030414     C*
083700030414     C* REPERIMEMTO DATI UTENTE
083800030414     C                   EXSR      REPDATIUTE
083900030422     C*
084000030422     C* CARICAMENTO TABELLE DI WRK
084100030422     C                   EXSR      CARTAB
084200130919     c
084300130919     c* Giro la richiesta date
084400130919     c                   eval      dataiso=%date(ilsw3dsd)
084500130919     c                   eval      dataeur=dataiso
084600130919     c                   eval      stpdsd=%dec(dataeur)
084700130923     c                   seton                                        0102
084800130919
084900130919     c                   if        ilsw3dsa<>ilsw3dsd and ilsw3dsa>0
085000130919     c                   eval      dataiso=%date(ilsw3dsa)
085100130919     c                   eval      dataeur=dataiso
085200130919     c                   eval      stpdsa=%dec(dataeur)
085300130919     c                   endif
085400130919
085500130919     c     ilsw3tfp      CHAIN     AZORG01L
085600130919     c                   if        %found(azorg01l)
085700130919     c                   eval      stpdtfp=orgdes
085800130919     c                   endif
085900130925     c
086000130925     c                   eval      stptfp=ilsw3tfp
086100130305      /free
086200130305       setll (dutpou)  wfpcl01l   ;
086300130305       reade (dutpou)  wfpcl01l   ;
086400130305
086500130305       dow  not %eof(wfpcl01l)   ;
086600130305       delete wfpcl000  ;
086700130305
086800130305       reade (dutpou)  wfpcl01l   ;
086900130305       enddo  ;
087000150909
087100150909       ENDSR  ;
087200130305
087300150910       //-------------------------------------------------------------------*
087400150909       BEGSR Prepasql   ;
087500150909        wrkgetlista ='select * from FNBLP00F where (blpaas*10000)+blpmgs>='
087600150910                     +%editc(ilsw3dsd:'X') +' and (blpaas*10000)+blpmgs<='
087700150910                     +%editc(ilsw3dsa:'X')
087800150910                     + ' and blpfls=' + %editc(wfls:'X')
087900150910                     + ' and blpnrs=' + %editc(wnrs:'X') +
088000150910                     ' and blptfp=' + %editc(ilsw3tfp:'X')  ;
088100150910       ENDSR  ;
088200150909       //-------------------------------------------------------------------
088300150909       BEGSR  SQLesegui              ;
088400150909       EXEC sql  prepare s1 from : wrkgetlista ;
088500150909       EXEC sql  declare a1 cursor for s1      ;
088600150909       EXEC sql  open    a1                    ;
088700150909
088800150909       EXEC sql fetch next from a1 into :dsblp ;
088900150909
089000150909       dow sqlcod=0    ;
089100150909
089200150909       exsr elabora    ;
089300150909
089400150909       EXEC sql fetch next from a1 into :dsblp ;
089500150909       enddo           ;
089600150909
089700150909       EXEC sql  CLOSE   a1                    ;
089800150909
089900150909       ENDSR     ;
090000130305      /end-free
090100130305
