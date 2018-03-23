000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300130305     h*   conteggio colli di bolle abbinate
000400130305     H*-------------------------------------------------------------------------
000500130304     FFNBLP31L  IF   E           K DISK
000600130304     FFNBLt01L  IF   E           K DISK
000700030422     FAZORG01L  IF   E           K DISK
000800030414     FTABEL00F  IF   E           K DISK
000900130304     FTNTBE01L  IF   E           K DISK
001000130305     Ffnbrv09l  IF   E           K DISK
001100130305     Fwfpcl01l  uF A E           K DISK
001200130305     FFNLSW4P   O    E             PRINTER OFLIND(*IN01)
001300001218     D*--------------------
001400001218     D* DS ESTERNE
001500001218     D*--------------------
001600900517     D KPJBA         E DS
001700130306     D fnlv53ds      E DS
001800030422     D*--------------------
001900030422     D* SCHIERE DI MEMORIZZAZIONE
002000030422     D*--------------------
002100130305     d* tfp abilitati a far partire colli di altro tfp
002200130305     D xcotfp          S              3    DIM(85)
002300130305     D ncl             S              9  0 DIM(85)
002400130305     D puc             S             13  3 DIM(85)
002500130305     D pucB            S             13  3 DIM(85)
002600130305     D vuc             S             13  6 DIM(85)
002700130305     D vucB            S             13  6 DIM(85)
002800030414     D*--------------------
002900030414     D* VARIABILI DI WRK
003000030414     D*--------------------
003100130305     D conta           s              2  0
003200130305     D YY              s              4  0
003300130305     D I               s              4  0
003400130304     D kke1            s                   like(tbeke1)
003500130304     D kke2            s                   like(tbeke2)
003600130304     D kcod1           s                   like(tbecod)
003700130305     D wpes            s                   like(blppkf)
003800130305     D wvol            s                   like(blpvlf)
003900130305     D savfnrs         s                   like(wflsnrs)
004000130304     D Wflsnrs         s              5  0
004100130305     D Wblpdsp         s              8  0
004200130305     D unipkf          s              9  3
004300130305     D univol          s              9  6
004400130305     D Indx            s              3  0
004500130305     d Dataiso         s               d   datfmt(*iso)
004600130305     d Dataeur         s               d   datfmt(*eur)
004700030414     D*---
004800030414     D* DS REPERIMENTO DATI UTENTE
004900030414     D*---
005000030414     D TIBS34DS      E DS
005100030414     D DDATIUTE      E DS
005200030414     D AZUTEDS       E DS                  extname(AZUTE00F)
005300030423     D*---
005400030423     D* DS REPERIMENTO £ire
005500030423     D*---
005600030423     D TRUL06DS      E DS
005700030423     D  L1                     1     90  0 DIM(30)
005800030521     D*------------------
005900030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
006000030521     D*------------------
006100030521     D WLBDA8          DS                  INZ
006200030521     D  G08DAT                 1      8  0
006300030521     D  G08INV                 9     16  0
006400030521     D  G08ERR                17     17
006500030521     D  G08TGI                18     22  0
006600001218
006700130304     D*------------------
006800130304     D FNLSW3DS        DS
006900130304     D  ilsw3dsd               1      8  0
007000130304     D  ilsw3dsa               9     16  0
007100130304     D  ilsW3TFP              18     20  0
007200130304
007300920812     C*---------------------------------------------------------------*
007400001218     C* MAIN
007500001218     C*---------------------------------------------------------------*
007600130304     c*
007700130306     c                   clear                   contabo           5 0
007800030422     C                   z-add     1             i
007900130304    1C                   dow       i <= %elem(L1)
008000130304     c
008100130304    2C                   if        L1(i) > *zeros
008200130304     C                   eval      blpLNP = L1(i)
008300130305     c                   movel     ilsw3dsd      kaas
008400130305     c                   move      ilsw3dsd      kmgs
008500130304     c
008600130304     C     kblp          setll     fnblp31l
008700130304     C     blplnp        reade     fnblp31l
008800130304     c
008900130304    3C                   dow       not %eof(fnblp31l)
009000130304     c
009100130304     c* se data sped > data AL --> esco dal ciclo
009200130304     c                   eval      wblpdsp=(blpaas*10000)+blpmgs
009300130304     c
009400130304     c                   if        wblpdsp>ilsw3dsa
009500130304     c                   leave
009600130304     c                   endif
009700130304     c
009800130304     c* Leggo le bolle con BLPXCO='W'
009900130304    4c                   if        blpxco='W'
010000130307     c                   eval      stptbol=stptbol+1
010100130304     c* cerco il record in XCO
010200130304     c                   eval      Wflsnrs=(blpfls*100)+blpnrs
010300130304
010400130304    5c                   if        wflsnrs<>savfnrs
010500130305
010600130305     c* memorizzo i dati per fls/nrs nel file di work
010700130305     c                   if        savfnrs>0
010800130305     c                   exsr      memdat
010900130305     c                   endif
011000130305     c
011100130304     c                   clear                   xcotfp
011200130304     C                   MOVEL     'XCO'         kcod1
011300130304     C                   eval      kke1='ALTRITFP'
011400130304     C                   eval      kke2=%editc(wflsnrs:'X')
011500130304     c     ktbe2         chain     tntbe01l
011600130304    6c                   if        %found(tntbe01l) AND tbeatb=' '
011700130304     C                   MOVEA     TBeUNI        xcotfp
011800130304    2c                   endif
011900130304     c
012000130304     c                   eval      savfnrs=wflsnrs
012100130304    5c                   endif
012200130304     c* Leggo collo per collo e controllo chi lo ha spuntato
012300130304     c
012400130304     c     kblt          setll     fnblt01l
012500130304     c     kblt          reade     fnblt01l
012600130304     c
012700130304    5c                   dow       not %eof(fnblt01l)
012800130305     c
012900130304     c                   exsr      leggispu
013000130304     c
013100130304     c     kblt          reade     fnblt01l
013200130304     c                   enddo
013300130304     c
013400130304    4c                   endif
013500130304     c
013600130304     C     blplnp        reade     fnblp31l
013700130304    3c                   enddo
013800130304    2c                   endif
013900030512     C*
014000030422     C                   eval      i = i +1
014100130304    1C                   enddo
014200020613     C*
014300130305     c* memorizzo i dati per fls/nrs nel file di work
014400130305     c                   if        savfnrs>0
014500130305     c                   exsr      memdat
014600130305     c                   endif
014700130305     c
014800130305     c                   EXSR      Stampa
014900130306     c
015000130306     C                   CLEAR                   fnlv53ds
015100130306     C                   MOVEL     'C'           D53TLA
015200130306     C                   CALL      'FNLV53R'
015300130306     C                   PARM                    fnlv53ds
015400130305     C                   seton                                        LR
015500001218     C*------------------------------------------------------------------------*
015600130304     c     LeggiSpu      BEGSR
015700130304     c* Leggo spunta entrata o partenza se c'e' di un'altra filiale
015800130304     c*  prevista in XCOTFP
015900130304     c                   z-add     5             knpg              1 0
016000130305     c                   clear                   spufgs            3 0
016100130305     c                   eval      conta=1
016200130305
016300130305    1c                   dow       spufgs=0  and conta<=2
016400130304     c
016500130304     c     kbrv          setll     fnbrv09l
016600130304     c     kbrv          reade     fnbrv09l
016700130305    2c                   dow       not %eof(fnbrv09l)
016800130304     c*
016900130306   2ac                   if        brvdcs>0
017000130306     c                   clear                   fnlv53ds
017100130306     c* se si tratta di spunta partenza devo escludere i fogli defluenza
017200130306   2bc                   if        brvnpg=1
017300130306     C                   MOVEL     'G'           D53TFO
017400130306     C                   Z-ADD     brvNFV        D53NFV
017500130306     C                   MOVEL     brvnpg        D53NPG
017600130306     C                   MOVEL     brvFGS        D53FGS
017700130306     C                   CALL      'FNLV53R'
017800130306     C                   PARM                    fnlv53ds
017900130306   2bc                   endif
018000130306     c
018100130306   2bC                   if        brvnpg=5 or d53def=' '
018200130305     c                   movel     brvfgs        alfafgs           3
018300130305     c                   eval      Indx=%lookup(alfafgs:xcotfp)
018400130304     c
018500130305    3c                   if        indx>0
018600130305     c                   eval      spufgs=brvfgs
018700130304     c                   leave
018800130305    3c                   endif
018900130306   2bc                   endif
019000130306   2ac                   endif
019100030512
019200130304     c     kbrv          reade     fnbrv09l
019300130305    2c                   enddo
019400130305    c
019500130304     c                   z-add     1             knpg
019600130305     c                   eval      conta=conta+1
019700130305    1c                   enddo
019800130304     c
019900130305     c* Trovata spuna di altra filiale autorizzata --> conteggio
020000130305    1c                   if        spufgs>0
020100130306     c***                clear                   wfpcl000
020200130306     c***                eval      pclfil=dutpou
020300130306     c***                eval      pcltfp=ilsw3tfp
020400130306     c***                eval      pclfls=brvlnp
020500130306     c***                eval      pclnrs=brvnrs
020600130306     c****               eval      pcldsd=bltnsp
020700130306     c***                eval      pcldsa=bltnsc
020800130306     c***                eval      pclfgs=spufgs
020900130306     c***                write     wfpcl000
021000130304     c* Colli
021100130304     c                   add       1             ncl(indx)
021200130304     c
021300130304     c* peso
021400130305     C*        : - SE TOTALE  , uso   BLPpkc
021500130305     C*          - SE PARZIALE, uso   BLPPKC (SE MAGGIORE)
021600130305     C*                               ALTRIMENTI uso  BLPPKF
021700130305     C     BLPNCp        ifeq      BLPNCL
021800130305     C                   Z-ADD     BLPpkC        Wpes
021900130305     C*
022000130305   X4C                   ELSE
022100130305    5C     BLPpkC        IFGE      BLPpkF
022200130305     C                   Z-ADD     BLPpkC        Wpes
022300130305   X5C                   ELSE
022400130305     C                   Z-ADD     BLPpkF        Wpes
022500130305    5C                   ENDIF
022600130305    4C                   ENDIF
022700130305     c
022800130304     c                   select
022900130304     c                   when      brvpuc>0
023000130305     c                   add       brvpuc        puc(indx)
023100130305
023200130304     c                   when      bltpuc>0
023300130305     c                   add       bltpuc        puc(indx)
023400130304     c                   other
023500130304     c
023600130305     c                   eval(H)   unipkf    =wpes  /blpncl
023700130305     c                   add       unipkf        puc(indx)
023800130304     c                   endsl
023900130305     c* sempre media spedizione
024000130305     c                   eval(H)   unipkf    =wpes  /blpncl
024100130305     c                   add       unipkf        pucb(indx)
024200130304     c* volume
024300130305     C*        : - SE TOTALE  , uso   BLPpkc
024400130305     C*          - SE PARZIALE, uso   BLPPKC (SE MAGGIORE)
024500130305     C*                               ALTRIMENTI uso  BLPPKF
024600130305     C     BLPNCR        IFEQ      BLPNCL
024700130305     C                   Z-ADD     BLPVLC        WVOL
024800130305     C*
024900130305   X4C                   ELSE
025000130305    5C     BLPVLC        IFGE      BLPVLF
025100130305     C                   Z-ADD     BLPVLC        WVOL
025200130305   X5C                   ELSE
025300130305     C                   Z-ADD     BLPVLF        WVOL
025400130305    5C                   ENDIF
025500130305    4C                   ENDIF
025600130305     c
025700130305     c                   select
025800130305     c                   when      brvvuc>0
025900130305     c                   add       brvvuc        vuc(indx)
026000130305
026100130305     c                   when      bltvuc>0
026200130305     c                   add       bltvuc        vuc(indx)
026300130305     c                   other
026400130305     c
026500130305     c                   eval(H)   univol    =wvol  /blpncl
026600130305     c                   add       univol        vuc(indx)
026700130305     c                   endsl
026800130305
026900130305     c* sempre media spedizione
027000130305     c                   eval(H)   univol    =wvol  /blpncl
027100130305     c                   add       univol        vucb(indx)
027200130305     c                   endif
027300130304     c
027400130304     c                   ENDSR
027500130305     C*------------------------------------------------------------------------*
027600130305     c     Memdat        BEGSR
027700130305     c* Memorizzo i dati
027800130305     c* se  già scritto --> vado in aggiunta
027900130305     c
028000130305     c                   z-add     1             yy
028100130305    1c                   do        85            yy
028200130305
028300130305    2c                   if        ncl(yy)>0
028400130305     c                   movel     xcotfp(YY)    kfgs
028500130305     c                   movel     wflsnrs       kfls
028600130305     c                   move      wflsnrs       knrs
028700130305     c
028800130305     c     kpcl          chain     wfpcl01l
028900130305    3c                   if        not %found(wfpcl01l)
029000130305     c                   clear                   wfpcl000
029100130305     c                   eval      pclfil=dutpou
029200130305     c                   eval      pcltfp=ilsw3tfp
029300130305     c                   eval      pclfls=kfls
029400130305     c                   eval      pclnrs=knrs
029500130305     c                   eval      pcldsd=ilsw3dsd
029600130305     c                   eval      pcldsa=ilsw3dsa
029700130305     c                   eval      pclfgs=kfgs
029800130305    3c                   endif
029900130305     c                   add       ncl(yy)       pcltncl
030000130305     c                   add       puc(yy)       pcltpkg
030100130305     c                   add       pucb(yy)      pcltpkgb
030200130305     c                   add       vuc(yy)       pcltvlm
030300130305     c                   add       vucb(yy)      pcltvlmb
030400130307     c                   add       stptbol       pcltbol
030500130305     c
030600130305    3c                   if        not %found(wfpcl01l)
030700130305     c                   write     wfpcl000
030800130305     c                   else
030900130305     c                   update    wfpcl000
031000130305    3c                   endif
031100130305    2c                   endif
031200130305    1c                   enddo
031300130305     c
031400130305     c                   clear                   ncl
031500130305     c                   clear                   puc
031600130305     c                   clear                   pucb
031700130305     c                   clear                   vuc
031800130305     c                   clear                   vucb
031900130307     c                   clear                   stptbol
032000130305     c
032100130305     c                   ENDSR
032200130305     C*------------------------------------------------------------------------*
032300130305     c     Stampa        BEGSR
032400130305     c* rileggo i dati
032500130305     c                   clear                   savfnrs
032600130305     c* Giro la richiesta date
032700130305     c                   eval      dataiso=%date(ilsw3dsd)
032800130305     c                   eval      dataeur=dataiso
032900130305     c                   eval      stpdsd=%dec(dataeur)
033000130312     c                   seton                                        01
033100130305
033200130305     c                   if        ilsw3dsa<>ilsw3dsd and ilsw3dsa>0
033300130305     c                   eval      dataiso=%date(ilsw3dsa)
033400130305     c                   eval      dataeur=dataiso
033500130305     c                   eval      stpdsa=%dec(dataeur)
033600130305     c                   endif
033700130305
033800130305     c     ilsw3tfp      CHAIN     AZORG01L
033900130305     c                   if        %found(azorg01l)
034000130305     c                   eval      stpdtfp=orgdes
034100130305     c                   endif
034200130305     c
034300130305     c     kpcl_sta      setll     wfpcl01l
034400130305     c     kpcl_sta      reade     wfpcl01l
034500130305     c
034600130312    1c                   dow       not %eof(wfpcl01l)
034700130305     c* a cambio fls/nrs ristampo la testata
034800130305     c                   eval      wflsnrs=(pclfls+100)+pclnrs
034900130305     c
035000130305     c                   if        wflsnrs<>savfnrs
035100130305     c                   seton                                        01
035200130305     c                   eval      savfnrs=wflsnrs
035300130305     c                   endif
035400130305     c
035500130305     c* Stampa  testata
035600130305     c
035700130305     c                   eval      stpfls=pclfls
035800130305     c                   eval      stpnrs=pclnrs
035900130307     c                   eval      stptbol=pcltbol
036000130305
036100130305     c                   if        *in01
036200130305     C                   write     LSw4PT
036300130305     C                   write     LSw4PI
036400130305     c                   setoff                                       01
036500130305     c                   endif
036600130305     c
036700130305     c* stampa detaglio
036800130305     c                   eval      stpfgs=pclfgs
036900130305     c     pclfgs        CHAIN     AZORG01L
037000130305     c                   if        %found(azorg01l)
037100130305     c                   eval      stpdfgs=orgdes
037200130305     c                   else
037300130305     c                   eval      stpdfgs=*all'?'
037400130305     c                   endif
037500130305     c
037600130305     c                   eval      stptncl=pcltncl
037700130305     c                   eval(H)   stptpkg=pcltpkg
037800130305     c                   eval(H)   stptvlm=pcltvlm
037900130305     c                   write     lsw4d2
038000130305     c
038100130305     c     kpcl_sta      reade     wfpcl01l
038200130312    1c                   enddo
038300130305     c
038400130312     c                   if        *in01
038500130312     C                   write     LSw4PT
038600130312     C                   write     LSw4PI
038700130312     c                   endif
038800130312     c
038900130305     C                   write     LSw4RI
039000130305     c
039100130305     c                   ENDSR
039200030414     C*---------------------------------------------------------------*
039300030414     C* CARTAB - Caricamento tabelle di wrk
039400030414     C*---------------------------------------------------------------*
039500030414     C     CARTAB        BEGSR
039600030423     C*
039700030423     C* Carico i P.O. del T.P. dell'utente reperendoli dalla £1
039800030423     C                   MOVEL     '£1'          D06COD
039900130305     C                   MOVEL     ilsw3tfp      D06KEY
040000030423     C                   MOVEL     'L'           D06TLA
040100030423     C                   MOVEL     *BLANKS       D06ESC
040200030423     C                   MOVEL(P)  TRUL06DS      KPJBU
040300030423     C                   CALL      'TRUL06R'
040400030423     C                   PARM                    KPJBA
040500030423     C                   MOVEL     KPJBU         TRUL06DS
040600030414     C*
040700030414     C                   ENDSR
040800030414     C*------------------------------------------------------------------------*
040900030414     C* REPERISCE I DATI UTENTE
041000030414     C*------------------------------------------------------------------------*
041100030414     C     REPDATIUTE    BEGSR
041200030414     C*
041300030414     C* INIZIALIZZA VARIABILI DI WRK
041400030414     C                   CLEAR                   TIBS34DS
041500030414     C                   CLEAR                   AZUTEDS
041600030414     C                   CLEAR                   DDATIUTE
041700030414     C*
041800030414     C     *DTAARA       DEFINE    §azute        azuteds
041900030414     C     *DTAARA       DEFINE    §datiute      ddatiute
042000030414     C                   IN(E)     *DTAARA
042100030422     C                   IF        %Error
042200030422     C                   EVAL      I34Tla = 'L'
042300030414     C                   CALL      'TIBS34R'
042400030414     C                   PARM                    Tibs34Ds
042500030414     C                   IN        *DTAARA
042600030422     C                   ENDIF
042700030414     C*
042800030414     C                   ENDSR
042900001218     C*------------------------------------------------------------------------*
043000001218     C* *INZSR - ROUTINE INIZIALE
043100001218     C*------------------------------------------------------------------------*
043200001218     C     *INZSR        BEGSR
043300001218     C*
043400030422     C* Ricevo in la DS di architettura
043500010726     C     *ENTRY        PLIST
043600020613     C                   PARM                    KPJBA
043700130304     c                   movel     kpjbu         fnlsw3ds
043800130305     c
043900130305     c                   if        ilsw3dsa=0
044000130305     c                   eval      ilsw3dsa=ilsw3dsd
044100130305     c                   endif
044200030521     C*
044300030422     C* Inizializzo campi chiave "fissi"
044400030414     C                   Z-ADD     1             tblKUT
044500030414     C****
044600030414     C* Definizione chiavi
044700030414     C****
044800030512     C* Chiave su TABEL00F - parziale
044900030512     C     KEYtabP       KLIST
045000030414     C                   KFLD                    tblKUT
045100030414     C                   KFLD                    tblCOD
045200130304     C     KTBE2         KLIST
045300130304     C                   KFLD                    KCOD1
045400130304     C                   KFLD                    KKE1
045500130304     C                   KFLD                    KKE2
045600130304     C*
045700130304     C     kblp          KLIST
045800030422     C                   KFLD                    blpLNP
045900130304     C                   KFLD                    kaas              4 0
046000130304     C                   KFLD                    kmgs              4 0
046100130304     C     kblt          KLIST
046200130304     C                   KFLD                    blpaas
046300130304     C                   KFLD                    blplnp
046400130304     C                   KFLD                    blpnrs
046500130304     C                   KFLD                    blpnsp
046600130304     C     kbrv          KLIST
046700130304     C                   KFLD                    knpg              1 0
046800130304     C                   KFLD                    bltfls
046900130304     C                   KFLD                    bltlna
047000130304     C                   KFLD                    bltnrs
047100130304     C                   KFLD                    bltnsc
047200130305     C     kpcl          KLIST
047300130305     C                   KFLD                    dutpou
047400130305     C                   KFLD                    ilsw3tfp
047500130305     C                   KFLD                    ilsw3dsd
047600130305     C                   KFLD                    ilsw3dsa
047700130305     C                   KFLD                    kfls              3 0
047800130305     C                   KFLD                    knrs              2 0
047900130305     C                   KFLD                    kfgs              3 0
048000130305     c
048100130305     C     kpcl_sta      KLIST
048200130305     C                   KFLD                    dutpou
048300130305     C                   KFLD                    ilsw3tfp
048400130305     C                   KFLD                    ilsw3dsd
048500130305     C                   KFLD                    ilsw3dsa
048600030414     C*
048700030414     C* REPERIMEMTO DATI UTENTE
048800030414     C                   EXSR      REPDATIUTE
048900030422     C*
049000030422     C* CARICAMENTO TABELLE DI WRK
049100030422     C                   EXSR      CARTAB
049200130305      /free
049300130305       setll (dutpou)  wfpcl01l   ;
049400130305       reade (dutpou)  wfpcl01l   ;
049500130305
049600130305       dow  not %eof(wfpcl01l)   ;
049700130305       delete wfpcl000  ;
049800130305
049900130305       reade (dutpou)  wfpcl01l   ;
050000130305       enddo  ;
050100130305
050200130305      /end-free
050300130305
050400001218     C                   ENDSR
