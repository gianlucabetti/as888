000100040804     H DECEDIT('0,') DATEDIT(*ymd.)
000200040910      * FNTS16R *----------------------------------------------------*
000300040910      *  VISITE OFFERTE
000400030211      *--------------------------------------------------------------*
000500040910     FTNVIS02L  iF   E           K DISK
000600040910     Ftfaco00f  iF   E           K DISK
000700040910     Ftfind00f  iF   E           K DISK
000800040910     Ftfclp00f  iF   E           K DISK
000900060206     Ftfcls01l  iF   E           K DISK
001000040910     FTNofm01L  iF   E           K DISK
001100040910     FTiofd01L  iF   E           K DISK
001200040910     FTiogc01L  iF   E           K DISK
001300040910     FTiopt01L  iF   E           K DISK
001400040910     FTiopd01L  iF   E           K DISK
001500040910     FTEETC01L  iF   E           K DISK
001600040910     FTEETT01L  iF   E           K DISK
001700040910     FTEETS01L  iF   E           K DISK
001800061009     FTEETS11L  iF   E           K DISK    RENAME(TEETs000:teets001)
001900040910     FTFNTC01L  iF   E           K DISK
002000100803     fTfcba01l  iF   e           k disk
002100040910      *
002200040910     FTNVISNEW  iF a E           K DISK
002300040910     F                                     RENAME(tnvis000:visnew)
002400040910     FTFACONEW  iF a E           K DISK
002500040910     F                                     RENAME(cnaco000:aconew)
002600040910     FTFINDNEW  iF a E           K DISK
002700040910     F                                     RENAME(cnind000:indnew)
002800040910     FTFCLPNEW  iF a E           K DISK
002900040910     F                                     RENAME(cnclp000:clpnew)
003000060206     FTFCLsNEW  iF a E           K DISK
003100060206     F                                     RENAME(fncls000:clsnew)
003200040910     FTnofmNEW  iF a E           K DISK
003300040910     F                                     RENAME(tntam000:tamnew)
003400040910     FTiofdNEW  iF a E           K DISK
003500040910     F                                     RENAME(titad000:tadnew)
003600040910     FTiogcNEW  iF a E           K DISK
003700040910     F                                     RENAME(titgc000:tgcnew)
003800040910     fTioptNEW  iF a E           K DISK
003900040910     F                                     RENAME(titpt000:tptnew)
004000040910     FTiopdNEW  iF a E           K DISK
004100040910     F                                     RENAME(titpd000:tpdnew)
004200040910     FTEETCNEW  iF a E           K DISK
004300040910     F                                     RENAME(TEETC000:ETCnew)
004400040910     FTEETTNEW  iF a E           K DISK
004500040910     F                                     RENAME(TEETt000:ETtnew)
004600040910     FTEETSNEW  iF a E           K DISK
004700040910     F                                     RENAME(TEETs000:ETsnew)
004800061009     FTEETSNEW1 iF a E           K DISK
004900061009     F                                     RENAME(TEETs000:ETsnew1)
005000040910     FTFNTCNEW  iF a E           K DISK
005100040910     F                                     RENAME(TFNTC:NTCnew)
005200100803     fTfcbanew  if a e           k disk
005300100803     f                                     rename(FNCBA000:CBAnew)
005400040910     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
005500040804     D* Schiera p.o. da trasferire
005600040804     D  SKPO           s              3    dim(500)
005700030211      *
005800040804
005900040910     d Kkci            s                   like(acokcc)
006000040910     d KCMM            s                   like(VISCMM)
006100040910     d wcmmfine        s                   like(VISCMM)
006200061009     D kDSF            S                   LIKE(ETCDSF) inz('S')
006300040910     D kCTO            S                   LIKE(ETCCTO) inz('O')
006400040910     D kAPL            S                   LIKE(NTCAPL) inz('V')
006500040910     D kNK1            S                   LIKE(NTCNK1) inz
006600040804     d okarbg          s              1
006700030211      *
006800040910     D tibs34ds      E DS                  inz
006900040910     D ddatiute      e ds
007000040910     D azuteds       e ds                  extname(AZUTE00F)
007100900511     D KPJBA         E DS
007200900523     C*****************************************************************
007300000000     C     *ENTRY        PLIST
007400000000     C                   PARM                    KPJBA
007500040804     C                   parm                    PARAM          1500
007600040910      *
007700040910     c     *dtaara       define    §azute        azuteds
007800040910     c     *dtaara       define    §datiute      ddatiute
007900040910     C                   in(E)     *dtaara
008000040910     C                   IF        %error  or RSUT = *blanks
008100040910     C                   CLEAR                   tibs34ds
008200040910     C                   CALL      'TIBS34R'
008300040910     C                   PARM                    tibs34ds
008400040910     C                   in        *dtaara
008500040910     c                   endif
008600040910     C                   z-add     DutKci        kkci
008700040910     C                   MOVEL     dutkci        kNK1
008800040910     c                   z-add     1             codut             1 0
008900040910      *
009000040804     c
009100040804     c     ' ':'0'       xlate     param         param
009200040804     c
009300040804     c                   movea     param         skpo
009400900511     C*---------------------------------------------------------------*
009500040910     C     KACO          KLIST
009600040910     C                   KFLD                    CODUT
009700040910     C                   KFLD                    kkci
009800040910     C                   KFLD                    visnrv
009900040910     C     KCAT          KLIST
010000040910     C                   KFLD                    kDSF
010100040910     C                   KFLD                    kCTO
010200040910     C                   KFLD                    visnrv
010300040910      *
010400040910     C     KNTC          KLIST
010500040910     C                   KFLD                    kAPL
010600040910     C                   KFLD                    kNK1
010700950621     C*---------------------------------------------------------------*
010800040804     C* LEGGO le linee passate
010900040804     C*
011000040804     c                   z-add     1             x                 4 0
011100040804    1c                   dow       skpo(x)>*zeros
011200040910     C                   CLEAR                   KCMM
011300040910     C                   eval      wcmmfine=9999999
011400040910     c                   movel     skpo(X)       kCMM
011500040910     c                   movel     skpo(X)       wcmmfine
011600040910     C     kCMM          SETLL     tnvis02l
011700040910     C                   READ      tnvis02l                               30
011800040910     c*
011900040910     c                   dow       not %eof(tnvis02l)  and viscmm<wcmmfine
012000040804     C*
012100040910     C** ELABORAZIONE RECORDS relativi
012200040910     C                   EXSR      elabora
012300040804     C*
012400040910     C                   READ      tnvis02l                               30
012500040804    2C                   ENDDO
012600040804     C                   ADD       1             X
012700040804     C                   ENDDO
012800900523     C*
012900040910     c                   EXCEPT    RINUM1
013000900523     C                   SETON                                        LR
013100040804     C**-------------------------------------------------------------**
013200040910     C**  elabora record relativi                                    **
013300040804     C**-------------------------------------------------------------**
013400040910     C     ELABORA       BEGSR
013500040804     C*
013600040910     C
013700040910     C     visnrv        SETLL     tnvisnew                               31
013800040804     C* se non trovata
013900040910    0C                   IF        not *in31
014000040910      * ARCHIVI ANAGRAFICI
014100040910     C     KACO          CHAIN     CNACO000                           31
014200040910     c                   if        %found(tfaco00f)
014300040910     c                   write     aconew                               99
014400040910     c  n99              add       1             totaco            7 0
014500040910     c                   endif
014600040910     C     KACO          CHAIN     CNind000                           31
014700040910     c                   if        %found(tfind00f)
014800040910     c                   write     indnew                               99
014900040910     c  n99              add       1             totind            7 0
015000040910     c                   endif
015100040910     C     KACO          CHAIN     cnclp000                           31
015200040910     c                   if        %found(tfclp00f)
015300040910     c                   write     clpnew                               99
015400040910     c  n99              add       1             totclp            7 0
015500040910     c                   endif
015600060206     C     visnrv        CHAIN     fncls000                           31
015700060206     c                   if        %found(tfcls01l)
015800060206     c                   write     clsnew                               99
015900060206     c  n99              add       1             totcls            7 0
016000060206     c                   endif
016100100803      * File coordinate bancarie
016200100803
016300100803     c     visnrv        setll     tfcba01l
016400100803     c                   do        *hival
016500100803     c     visnrv        reade     tfcba01l
016600100803     c                   if        %eof(tfcba01l)
016700100803     c                   leave
016800100803     c                   endif
016900100803     c                   write     cbanew                               99
017000100803     c  n99              add       1             totcba            7 0
017100100803     c                   enddo
017200040910     c*
017300040910     C* FILES  offerta
017400040910     C     visnrv        setll     tnofm01l
017500040910     C     visnrv        reade     tnofm01l
017600040910     c                   dow       not %eof(tnofm01l)
017700040910     c                   write     tamnew                               99
017800040910     c  n99              add       1             tottam            7 0
017900040910     c
018000040910     C     visnrv        reade     tnofm01l
018100040910     c                   enddo
018200040910     c
018300040910     C     visnrv        setll     tiofd01l
018400040910     C     visnrv        reade     tiofd01l
018500040910     c                   dow       not %eof(tiofd01l)
018600040910     c                   write     tadnew                               99
018700040910     c  n99              add       1             tottad            7 0
018800040910     c
018900040910     C     visnrv        reade     tiofd01l
019000040910     c                   enddo
019100040910     c
019200040910     C     visnrv        setll     tiogc01l
019300040910     C     visnrv        reade     tiogc01l
019400040910     c                   dow       not %eof(tiogc01l)
019500040910     c                   write     tgcnew                               99
019600040910     c  n99              add       1             tottgc            7 0
019700040910     c
019800040910     C     visnrv        reade     tiogc01l
019900040910     c                   enddo
020000040910     c
020100040910     C     visnrv        setll     tiopt01l
020200040910     C     visnrv        reade     tiopt01l
020300040910     c                   dow       not %eof(tiopt01l)
020400040910     c                   write     tptnew                               99
020500040910     c  n99              add       1             tottpt            7 0
020600040910     c
020700040910     C     visnrv        reade     tiopt01l
020800040910     c                   enddo
020900040910     c
021000040910     C     visnrv        setll     tiopd01l
021100040910     C     visnrv        reade     tiopd01l
021200040910     c                   dow       not %eof(tiopd01l)
021300040910     c                   write     tpdnew                               99
021400040910     c  n99              add       1             tottpd            7 0
021500040910     c
021600040910     C     visnrv        reade     tiopd01l
021700040910     c                   enddo
021800040910     c*
021900040910     C* FILES  CAT
022000040910     C     kcat          setll     teetc01l
022100040910     C     kcat          reade     teetc01l
022200040910     c                   dow       not %eof(teetc01l)
022300040910     c                   write     etcnew                               99
022400040910     c  n99              add       1             totetc            7 0
022500040910     c
022600040910     C     kcat          reade     teetc01l
022700040910     c                   enddo
022800040910     c
022900040910     C     kcat          setll     teett01l
023000040910     C     kcat          reade     teett01l
023100040910     c                   dow       not %eof(teett01l)
023200040910     c                   write     ettnew                               99
023300040910     c  n99              add       1             totett            7 0
023400040910     c
023500040910     C     kcat          reade     teett01l
023600040910     c                   enddo
023700061009     c* scaglioni x ricavi
023800040910     C     kcat          setll     teets01l
023900040910     C     kcat          reade     teets01l
024000040910     c                   dow       not %eof(teets01l)
024100040910     c                   write     etsnew                               99
024200040910     c  n99              add       1             totets            7 0
024300040910     c
024400040910     C     kcat          reade     teets01l
024500040910     c                   enddo
024600061009     c* scaglioni x competenze
024700061009     C     kcat          setll     teets11l
024800061009     C     kcat          reade     teets11l
024900061009     c                   dow       not %eof(teets11l)
025000061009     c                   write     etsnew1                              99
025100061009     c  n99              add       1             totets1           7 0
025200061009     c
025300061009     C     kcat          reade     teets11l
025400061009     c                   enddo
025500040910     c
025600040910     c* Note
025700040910     C                   MOVE      visnrv        kNK1
025800040910     C     kntc          setll     tfntc01l
025900040910     C     kntc          reade     tfntc01l
026000040910     c                   dow       not %eof(tfntc01l)
026100040910     c                   write     ntcnew                               99
026200040910     c  n99              add       1             totntc            7 0
026300040910     c
026400040910     C     kntc          reade     tfntc01l
026500040910     c                   enddo
026600040804     C*
026700040804     C*****
026800040910     C***  VISITA
026900040804     C*****
027000040910     C                   WRITE     VISNEW
027100040910     c  n99              add       1             totvis            7 0
027200040804     c                   endif
027300040804     C*
027400040804     C                   ENDSR
027500040910      *---------------------------------------------------------------*
027600040910     oQSYSPRT   E            RINUM1         2 02
027700040910     o                                         +  5 'VISITE OFFERTE'
027800040910     o                                         +  5 'Fits16R'
027900040910     o                       UDATE             +  2 '  /  /  '
028000040910     o          E            RINUM1      1
028100040910     o                                         +  3 'VISITE:'
028200040910     o                       TOTVIS        2   +  2
028300040910     o                                         +  3 'TFACO00F:'
028400040910     o                       TOTACO        2   +  2
028500040910     o                                         +  3 'TFIND00F:'
028600040910     o                       TOTIND        2   +  2
028700040910     o                                         +  3 'TFCLP00F:'
028800040910     o                       TOTCLP        2   +  2
028900060206     o                                         +  3 'TFCLS00F:'
029000060206     o                       TOTCLS        2   +  2
029100040910     o          E            RINUM1      2
029200040910     o                                         +  3 'TNOFM00F:'
029300040910     o                       TOTTAM        2   +  2
029400040910     o                                         +  3 'TIOFD00F:'
029500040910     o                       TOTTAD        2   +  2
029600040910     o                                         +  3 'TIOPT00F:'
029700040910     o                       TOTTPT        2   +  2
029800040910     o                                         +  3 'TIOPD00F:'
029900040910     o                       TOTTPD        2   +  2
030000040910     o                                         +  3 'TIOGC00F:'
030100040910     o                       TOTTGC        2   +  2
030200040910     o          E            RINUM1      2
030300040910     o                                         +  3 'TEETC00F:'
030400040910     o                       TOTETC        2   +  2
030500040910     o                                         +  3 'TEETT00F:'
030600040910     o                       TOTETT        2   +  2
030700040910     o                                         +  3 'TEETS00F:'
030800040910     o                       TOTETS        2   +  2
030900061009     o                                         +  3 'TEETS10F:'
031000061009     o                       TOTETS1       2   +  2
031100040910     o                                         +  3 'TFNTC00F:'
031200040910     o                       TOTNTC        2   +  2
031300100803     o          E            RINUM1      2
031400100803     o                                         +  3 'TFCBA00F:'
031500100803     o                       TOTCBA        2   +  2
