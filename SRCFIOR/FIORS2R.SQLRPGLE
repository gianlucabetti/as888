000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300130215     Ftita432c  IF   E           K DISK
000400130215     Ftitas30c  IF   E           K DISK
000500130215     Ffnorm14l  IF   E           K DISK
000600030414     FTABEL00F  IF   E           K DISK
000700130215     FAZorg01l  IF   E           K DISK
000800130215     fWFROM00F  o    e             disk
000900001218     D*--------------------
001000001218     D* DS ESTERNE
001100001218     D*--------------------
001200900517     D KPJBA         E DS
001300030422     D*--------------------
001400030422     D* SCHIERE DI MEMORIZZAZIONE
001500030414     D*--------------------
001600030414     D* VARIABILI DI WRK
001700030414     D*--------------------
001800040430     D wdsd            s                   like(v1cdsd)
001900040430     D wdsa            s                   like(v1cdsa)
002000130215     D sav_tas         s                   like(ds_tas)
002100130206     d Dataiso         s               d   datfmt(*iso)
002200130206     d Dataeur         s               d   datfmt(*eur)
002300030414     D*---
002400030414     D* DS REPERIMENTO DATI UTENTE
002500030414     D*---
002600030414     D TIBS34DS      E DS
002700030414     D DDATIUTE      E DS
002800030414     D AZUTEDS       E DS                  extname(AZUTE00F)
002900030521     D*------------------
003000030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100030521     D*------------------
003200030521     D WLBDA8          DS                  INZ
003300030521     D  G08DAT                 1      8  0
003400030521     D  G08INV                 9     16  0
003500030521     D  G08ERR                17     17
003600030521     D  G08TGI                18     22  0
003700130205     D*------------------
003800130215     D fiors1DS        DS
003900130215     D  iors1dsd               1      8  0
004000130215     D  iors1dsa               9     16  0
004100130215
004200130215     D ds_orm          DS                  inz
004300130215     D  ormpoe                 1      3  0
004400130215     D  omrnsr                 4      5  0
004500130215     D  ormnor                 6     12  0
004600130215     D  ormnrv                13     14  0
004700130215     d
004800130215     D ds_TAS          DS                  inz
004900130215     D  tasaas                 1      4  0
005000130215     D  taslnp                 5      7  0
005100130215     D  tasnrs                 8      9  0
005200130215     D  tasnsp                10     16  0
005300130215     D  tastbl                17     18
005400920812     C*---------------------------------------------------------------*
005500001218     C* MAIN
005600001218     C*---------------------------------------------------------------*
005700130215      /FREE
005800130215       exec sql delete from WFROM00F    ;
005900130215      /END-FREE
006000130215     c                   clear                   totman           11 0
006100130215     c                   clear                   totcra           11 0
006200130215     c                   clear                   contaass         11 0
006300130215     c                   clear                   contaFksc        11 0
006400130215     c                   clear                   contaF888        11 0
006500130215     c                   clear                   contaNoB         11 0
006600130215     c
006700130215     c     *loval        setll     azorg01l
006800130215     c                   read      azorg01l
006900130215    0c                   dow       not %eof(azorg01l)
007000130215   0ac                   if        orgfva=' ' and (orgfag='F' or orgfag='A')
007100130215     c
007200130215     c                   eval      kpoe=orgfil
007300130215     c     korm          klist
007400130215     c                   kfld                    kpoe              3 0
007500130215     c                   kfld                    iors1dsd
007600130215     c
007700130215     c     korm          setll     fnorm14l
007800130215     c     kpoe          reade     fnorm14l
007900130215     c
008000130215    1c                   dow       not %eof(fnorm14l)
008100130215     c
008200130215    2c                   if        ormdar>iors1dsa
008300130215     c                   leave
008400130215    2c                   endif
008500130215     c
008600130215    2c                   if        ormfao=900  and ormtco='M'
008700141212     c                             and ORMtco = 'E'
008800130215     c* totale
008900130215     c                   eval      totman=totman+1
009000130215     c*
009100130215     c* senza cod mittente ortm
009200130215    3c                   if        ormcra=0
009300130215     c* totale
009400130215     c                   eval      totcra=totcra+1
009500130215     c* chaino bolla
009600130215     C     ds_orm        setll     TITA432C
009700130215     C     ds_orm        reade     TITA432C
009800130215
009900130215     C     ktas          KLIST
010000130215     C                   KFLD                    ta4AAS
010100130215     C                   KFLD                    ta4LNP
010200130215     C                   KFLD                    ta4NRS
010300130215     C                   KFLD                    ta4NSP
010400130215     C     ktas2         KLIST
010500130215     C                   KFLD                    tasAAS
010600130215     C                   KFLD                    tasLNP
010700130215     C                   KFLD                    tasNRS
010800130215     C                   KFLD                    tasNSP
010900130215     C                   KFLD                    tasTBL
011000130215      *
011100130215     c
011200130215     c                   clear                   ds_tas
011300130215     c                   clear                   sav_tas
011400130215    4c                   dow       not %eof(tita432c)
011500130215     C     ktas          chain     TITAS30C                           24
011600130215     c
011700130215    5c                   dow       not *in24
011800130215     c* Escludo le bolle di recupero
011900130215    6c                   if        tastbl <>'F3' and tastbl<>'A3' and
012000130215     c                             tastbl<>'AR'
012100130215
012200130215    7c                   if        tastbl= 'F1'
012300130215     c                   eval      sav_tas= ds_tas
012400130215     c                   leave
012500130215   x7c                   else
012600130215
012700130215     c                   eval      sav_tas=ds_tas
012800130215    7c                   endif
012900130215
013000130215    6c                   endif
013100130215     c
013200130215     C     ktas          reade     TITAS30C                               24
013300130215   5ac                   enddo
013400130215     c
013500130215     c* se ho trovato un franco -> esco altrimenti ci riprovo
013600130215     c                   if        %subst(sav_tas:17:2)='F1'
013700130215     c                   leave
013800130215     c                   else
013900130215     C     ds_orm        reade     TITA432C
014000130215     c                   endif
014100130215     c
014200130215    4c                   enddo
014300130215     c
014400130215     c* se ho trovato la bolla --> conteggio
014500130215    4c                   if        %subst(sav_tas:1:4)>*zeros
014600130215     c                   eval      ds_tas=sav_tas
014700130215     c
014800130215     c     ktas2         chain     titas30c
014900130215    5c                   if        %found(titas30c)
015000130215     c                   z-add     tasccm        w0040             4 0
015100130215     c
015200130215    6c                   select
015300130215     c                   when      %subst(tastbl:1:1)='A'
015400130215     c                   eval      contaass=contaass+1
015500130215     c
015600130215     c                   when      w0040<>8888
015700130215     c                   eval      contaFksc=contaFksc+1
015800130215     c                   other
015900130215     c                   eval      contaF888=contaF888+1
016000130215    6c                   endsl
016100130215   x5c                   else
016200130215     c
016300130215     c                   eval      contanoB =contaNoB +1
016400130215    5c                   endif
016500130215   x4c                   else
016600130215     c
016700130215     c                   eval      contanoB =contaNoB +1
016800130215    4c                   endif
016900130215    3c                   endif
017000130215    2c                   endif
017100130215    c
017200130215     c     kpoe          reade     fnorm14l
017300130215     c                   enddo
017400001218     C*
017500130215     c* Scrito i totali per filiale
017600130215     c                   exsr      Totfil
017700130215     c
017800130215     c                   clear                   totman           11 0
017900130215     c                   clear                   totcra           11 0
018000130215     c                   clear                   contaass         11 0
018100130215     c                   clear                   contafksc        11 0
018200130215     c                   clear                   contaf888        11 0
018300130215     c                   clear                   contaNoB         11 0
018400130215     c                   endif
018500130215     c
018600130215     c                   read      azorg01l
018700130215     c                   enddo
018800001218     C                   seton                                        LR
018900001218
019000030414     C*------------------------------------------------------------------------*
019100030414     C* REPERISCE I DATI UTENTE
019200030414     C*------------------------------------------------------------------------*
019300030414     C     REPDATIUTE    BEGSR
019400030414     C*
019500030414     C* INIZIALIZZA VARIABILI DI WRK
019600030414     C                   CLEAR                   TIBS34DS
019700030414     C                   CLEAR                   AZUTEDS
019800030414     C                   CLEAR                   DDATIUTE
019900030414     C*
020000030414     C     *DTAARA       DEFINE    §azute        azuteds
020100030414     C     *DTAARA       DEFINE    §datiute      ddatiute
020200030414     C                   IN(E)     *DTAARA
020300030422     C                   IF        %Error
020400030422     C                   EVAL      I34Tla = 'L'
020500030414     C                   CALL      'TIBS34R'
020600030414     C                   PARM                    Tibs34Ds
020700030414     C                   IN        *DTAARA
020800030422     C                   ENDIF
020900030414     C*
021000030414     C                   ENDSR
021100030414     C*------------------------------------------------------------------------*
021200010509
021300010509
021400001218
021500001218     C*------------------------------------------------------------------------*
021600001218     C* *INZSR - ROUTINE INIZIALE
021700001218     C*------------------------------------------------------------------------*
021800001218     C     *INZSR        BEGSR
021900001218     C*
022000030422     C* Ricevo in la DS di architettura
022100010726     C     *ENTRY        PLIST
022200020613     C                   PARM                    KPJBA
022300130215     c                   movel     kpjbu         fiors1ds
022400030521     C*
022500030521     C* Reperisco l'UDATE del job
022600030521     C                   TIME                    WN14             14 0
022700030521     C                   MOVE      WN14          WN8               8 0
022800030521     C                   Z-ADD     WN8           G08DAT
022900030521     C                   Z-ADD     *zeros        G08INV
023000030521     C                   MOVEL     '0'           G08ERR
023100030521     C                   CALL      'XSRDA8'
023200030521     C                   PARM                    WLBDA8
023300030521     C                   Z-ADD     G08INV        DATCOR            8 0
023400010726     C*
023500030422     C* Inizializzo campi chiave "fissi"
023600130206     c* Giro le date per la stampa
023700130206     c                   clear                   v1cdsd
023800130215     c                   if        iors1dsd>0
023900130215     c     *iso          movel     iors1dsd      dataiso
024000130206     c                   movel     dataiso       dataeur
024100130215     c                   movel     dataeur       v1cdsd            8 0
024200130206     c                   endif
024300130206     c
024400130206     c                   clear                   v1cdsa
024500130215     c                   if        iors1dsa>0
024600130215     c     *iso          movel     iors1dsa      dataiso
024700130206     c                   movel     dataiso       dataeur
024800130215     c                   movel     dataeur       v1cdsa            8 0
024900130206     c                   endif
025000130205     c
025100030414     C*
025200030414     C* REPERIMEMTO DATI UTENTE
025300030414     C                   EXSR      REPDATIUTE
025400001218     C*
025500001218     C                   ENDSR
025600130215     C*------------------------------------------------------------------------*
025700130215     C* totali - scrittura
025800130215     C*------------------------------------------------------------------------*
025900130215     C     Totfil        BEGSR
026000130215     c                   z-add     1             codut             1 0
026100130215     c     ktab          klist
026200130215     c                   kfld                    codut
026300130215     c                   kfld                    tblcod
026400130215     c                   kfld                    tblkey
026500130215     c
026600130215     c                   clear                   wfrom000
026700130215     c                   eval      rompoe =kpoe
026800130215     c                   eval      rompoed=orgdes
026900130215     c                   eval      romare =orgcar
027000130215     c                   movel     '05'          tblcod
027100130215     c                   movel(P)  romare        tblkey
027200130215     c     ktab          chain     tabel00f
027300130215 b4  c                   if        %found(tabel00f)
027400130215     c                   movel     tbluni        romared
027500130215 e4  c                   endif
027600130215 c
027700130215     c                   eval      romdis =orgfl3
027800130215     c                   movel     '17'          tblcod
027900130215     c                   movel(P)  romdis        tblkey
028000130215     c     ktab          chain     tabel00f
028100130215 b4  c                   if        %found(tabel00f)
028200130215     c                   movel     tbluni        romdisd
028300130215 e4  c                   endif
028400130215     c
028500130215     c                   eval      romdaed=iors1dsd
028600130215     c                   eval      romdaea=iors1dsa
028700130215     c                   eval      romtman=totman
028800130215     c                   eval      romtcra=totcra
028900130215     c                   eval      romrass=contaass
029000130215     c                   eval      romrFksc=contaFksc
029100130215     c                   eval      romrF888=contaF888
029200130215     c                   eval      romrNob =contaNoB
029300130215     c                   write     wfrom000
029400130215     c
029500130215     c                   ENDSR
