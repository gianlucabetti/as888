000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200130319     Ftitas38c  iF   E           K DISK
000201130315     Ftitas30x  iF   E           K DISK    extfile('TITAS30C') usropn
000202130314     F                                     RENAME(titas000:titas0x)
000203130314     F                                     rENAME(titas010:titas1x)
000204130314     F                                     RENAME(titasp00:titaspx)
000205130315     f                                     prefix(X_)
000206130314     Ffnlbl02l  iF   E           K DISK
000207130314     Ftabel00f  iF   E           K DISK
000208130315     Ffiar531C  UF   E           K DISK
000400130314     Ffiar501l  UF   E           K DISK    usropn extfile(war5F)
000401130315     F                                     RENAME(Fiar5000:fiar5f)
000406130314     fwferrgen  o  a E             disk
000408111012
000800130314     d dAr5gen       e ds                  Inz
000801130314     d ds1b          e ds
000802130315     d dstb          e ds
000803130314     d wtrd            s                   like (ar5trd) Inz('GEN')
000804130315     d kaas            s                   like (tasaas)
000805130315     d klnp            s                   like (taslnp)
000806130315     d knrs            s                   like (tasnrs)
000807130315     d knsp            s                   like (tasnsp)
000808130318     d k_aas           s                   like (tasaas)
000811130319     d k_mgs           s                   like (tasmgs)
000901130314     d War5f           s             21    inz('FILTRA201 /FIAR501L')
000902130315     d codut           s              1  0 inz(1)
000903130314     d x               s              2  0
000904130314     d cod             s                   like(tblcod)
000905130314     D CTM             S              2    DIM(70)
000906130315     D tbl             S              2    DIM(70)
001500100317
001501130314     iFIAR5000      50
001502130314     iFIAR5P00      51
001600011003     C*------------------------------------------------------------------------*
001700011003     C* MAIN LINE
001800011003     C*------------------------------------------------------------------------*
002600100317     c
002601130318     c     ktas          klist
002602130318     c                   kfld                    k_aas
002605130319     c                   kfld                    k_mgs
002700130314     c     kar5          klist
002800130315     c                   kfld                    kaas
002900130315     c                   kfld                    klnp
003000130315     c                   kfld                    knrs
003001130315     c                   kfld                    knsp
003002130314     c                   kfld                    wtrd
003003130314     c     keylbln       klist
003008130314     c                   Kfld                    LBLAAN
003009130314     c                   Kfld                    LBLLPN
003010130314     c                   Kfld                    LBLNRN
003011130314     c                   Kfld                    LBLNSN
003012130314     c     ktas30x       klist
003013130314     c                   kfld                    lblAAN
003014130314     c                   kfld                    lblLPN
003015130314     c                   kfld                    lblNRN
003016130314     c                   kfld                    lblnsp
003017130314     c     ktab          klist
003018130314     c                   kfld                    codut
003019130314     c                   kfld                    cod
003100100317     c
004401111012     c*
004409130314     c                   Open(e)   fiar501l
004410130314     c                   if        not %open(fiar501l)
004411130314     c                   eval      %subst(War5f:7:3)='PRD'
004412130314     c                   Open      fiar501l
004413111012     c                   endif
004414130314     c                   open      titas30x
004415130314     c*
004416130315     c                   exsr      cartab
004500011003     C*
004501130319     c                   z-add     2013          k_aas
004502130319     c                   z-add     0318          k_mgs
004600130319     C     ktas          SETLL     titas38c
004700130319     C     ktas          reade     titas38c
004800090715     c
004900130319    1c                   dow       not %eof(titas38c)
004901130314     c* bolla con ctm con packing list
004902130314    2c                   if        %lookup(tasctm:ctm)>0
004903130315     c                   eval      kaas=tasaas
004904130315     c                   eval      klnp=taslnp
004905130315     c                   eval      knrs=tasnrs
004906130315     c                   eval      knsp=tasnsp
004907130314     c* aggiornamento fiar5 di sede
004908130314     c                   exsr      SR_ar5sede
004914130314     c* aggiornamento fiar5 di filiale
004915130314     c                   exsr      SR_ar5fil
004916130314     c* verifica presenza di figlie da aggiornare
004917130314     c                   exsr      sr_lbl
004918130314    2c                   endif
004956111012     c*
004957130319     C     ktas          reade     titas38c
004958111012    1c                   enddo
004959111012     c*
004960130314     c                   close     fiar501l
005000090715     c
013800011003     C
013900011003     C                   SETON                                        LR
013901111012     c******************************************************************************
013902130315     c*Carica schiera dei ctm con packing list (§1bf16='S') e schiera TB di recupero
013903111012     c******************************************************************************
013904130315     c     cartab        begsr
013905130314     C                   MOVEL     '1B'          COD
013907130314     C                   Z-ADD     1             X
013908130314     c                   clear                   ctm
013909130314     c     ktab          setll     tabel
013910130314     c     ktab          reade     tabel
013911130315     c                   dow       not %eof(tabel00F)
013912130314     C                   MOVEL     TBLUNI        DS1B
013913130314     C     §1BF16        IFEQ      'S'
013914130314     C                   MOVEL     TBLKEY        CTM(X)
013915130314     C                   ADD       1             X
013916130314     c                   endif
013917130314     C     KTAB          READE     TABEL
013918130314     c                   enddo
013919130315     c*
013920130315     C                   MOVEL     'TB'          COD
013921130315     C                   Z-ADD     1             X
013922130315     c                   clear                   tbl
013923130315     c     ktab          setll     tabel
013924130315     c     ktab          reade     tabel
013925130315     c                   dow       not %eof(tabel00f)
013926130315     C                   MOVEL     TBLUNI        DSTB
013927130315     C     §TBRBL        IFEQ      'R'
013928130315     C                   MOVEL     TBLKEY        tbl(X)
013929130315     C                   ADD       1             X
013930130315     c                   endif
013931130315     C     KTAB          READE     TABEL
013932130315     c                   enddo
013933130314     c                   endsr
013934130314     c******************************************************************************
013935130314     c*aggiornamento fiar5 di sede
013936130314     c******************************************************************************
013937130314     c     sr_ar5sede    begsr
013938130318     c                   setoff                                       5051
013939130315     c     kar5          chain(e)  fiar531c
013940130314     c* se errore di allocazione memorizzo in wf
013941130314    1c                   if        %error
013942130315     c                   eval      Waas=kaas
013943130315     c                   eval      Wlnp=klnp
013944130315     c                   eval      Wnrs=knrs
013945130315     c                   eval      Wnsp=knsp
013946130314     c                   eval      Wtip='S'
013947130315     c                   eval      Wtipe='A'
013948130314     c                   write     wferrrec
013949130314   x1c                   else
013950130314     c* se no errore aggiorno
013951130315    2c                   if        %found(fiar531c)
013952130314     c                   movel     ar5uni        dar5gen
013953130314     c                   eval      §ar5alx='S'
013954130314     c                   movel     dar5gen       ar5uni
013955130314     c   50              update    fiar5000
013956130314     c   51              update    fiar5P00
013957130314     c                   else
013958130314     c* se non trovato scrivo
013959130315     c                   eval      Waas=kaas
013960130315     c                   eval      Wlnp=klnp
013961130315     c                   eval      Wnrs=knrs
013962130315     c                   eval      Wnsp=knsp
013963130315     c                   eval      Wtip='S'
013964130315     c                   eval      Wtipe='M'
013965130315     c                   write     wferrrec
013966130314    2c                   endif
013967130314    1c                   endif
013968130314     c                   endsr
014000130314     c******************************************************************************
014100130314     c*aggiornamento fiar5 di filiale
014200130314     c******************************************************************************
014300130314     c     sr_ar5fil     begsr
014400130314     c     kar5          chain(e)  fiar501l
014500130314     c* se errore di allocazione memorizzo in wf
014600130314    1c                   if        %error
014700130315     c                   eval      Waas=kaas
014800130315     c                   eval      Wlnp=klnp
014900130315     c                   eval      Wnrs=knrs
015000130315     c                   eval      Wnsp=knsp
015100130314     c                   eval      Wtip='F'
015101130315     c                   eval      Wtipe='A'
015200130314     c                   write     wferrrec
015300130314   x1c                   else
015400130314     c* se no errore aggiorno
015500130314    2c                   if        %found(fiar501l)
015600130314     c                   movel     ar5uni        dar5gen
015700130314     c                   eval      §ar5alx='S'
015800130314     c                   movel     dar5gen       ar5uni
015900130314     c                   update    fiar5f
016100130314     c                   else
016200130314     c* se non trovato ????
016201130315     c                   eval      Waas=kaas
016202130315     c                   eval      Wlnp=klnp
016203130315     c                   eval      Wnrs=knrs
016204130315     c                   eval      Wnsp=knsp
016205130315     c                   eval      Wtip='F'
016206130315     c                   eval      Wtipe='M'
016207130318     c*****              write     wferrrec
016300130314    2c                   endif
016400130314    1c                   endif
016500130314     c                   endsr
016600130314     c******************************************************************************
016700130314     c*verifica bolle figlie
016800130314     c******************************************************************************
016900130314     c     sr_lbl        begsr
016903130314     c                   eval      LBLAAN=tasAAS
016904130314     c                   eval      LBLLPN=tasLNP
016905130314     c                   eval      LBLNRN=tasNRS
016906130314     c                   eval      LBLNSN=tasNSP
016907130314     c     keylblN       chain     fnlbl02l
016908130314     c                   dow       %found(fnlbl02l)
016909130315     c     keylbln       chain     titas30x
016910130315     c                   if        %found(titas30x) and
016911130318     c***                          %lookup(x_tastbl:tbl)=0
016912130319     c                             x_tasfbr<>'S'
016913130315     c                   eval      kaas=x_tasaas
016914130315     c                   eval      klnp=x_taslnp
016915130315     c                   eval      knrs=x_tasnrs
016916130315     c                   eval      knsp=x_tasnsp
016917130314     c* aggiornamento fiar5 di sede
016918130314     c                   exsr      SR_ar5sede
016919130314     c* aggiornamento fiar5 di filiale
016920130314     c                   exsr      SR_ar5fil
016921130315     c                   endif
016922130314     c
016923130314     c     keylblN       chain     fnlbl02l
016924130314     c                   enddo
017000130314     c                   endsr
