000100990430     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200990430     H*PARMS BNDDIR(PJXBND PJCBND)
000300990430     H*PARMS CVTOPT(*DATETIME)
000400990430     H DECEDIT('0,') DATEDIT(*DMY.)
000500990430     F*---------------------------------------------------------------*H
000600000411     F* NDCR84R   *      SCRITTURA ARCHIVIO per dischetto             *
000700100512     F*                                                               *
000800100512R024 F* Personalizzazione.                                            *
000900100512 "   F* R024 : modifiche normativa 2010                               *
001000100512R024 F*                                                               *
001100110413R055 F* Personalizzazione.                                            *
001200110413 "   F* R055 : modifiche normativa 2011                               *
001300110413R055 F*                                                               *
001400990430     F*---------------------------------------------------------------*
001500990430     F*   D A T A    B A S E                                          *
001600990430     F*---------------------------------------------------------------*
001700990430     FND77010F  IF   E           K DISK
001800990430     F*
001900990430     FND77099F  O  A E             DISK
002000990430     D*
002100990430     D*----------------------------------------------------*
002200990430     D KPJBA         E DS
002300990430     D*-------------
002400990430     D NdCr80ds      E DS
002500990430     D*-------------
002600080118C2073X*AADS          E DS                  EXTNAME(ND77000ADS) INZ
002700100512R024 D*** ex C2073 AADS          E DS                  EXTNAME(ND77008ADS) INZ
002800040622C1770D*-------------
002900050704C1846X**** ex C1770 D BBDS          E DS                  EXTNAME(ND77000BDS) INZ
003000060619C1943X**** ex C1846   BBDS          E DS                  EXTNAME(ND77005BDS) INZ
003100070423C2014X**** ex c1943   BBDS          E Ds                  EXTNAME(ND77006BDS) INZ
003200080118C2073X**** ex C2014  E DS                  EXTNAME(ND77007BDS) INZ
003300090514D2500X**** ex C2073 D BBDS          E DS                  EXTNAME(ND77008BDS) INZ
003400100512R024 D*** ex D2500 BBDS          E DS                  EXTNAME(ND77009BDS) INZ
003500990430     D*-------------
003600080118c2073X*ZZDS          E DS                  EXTNAME(ND77000ZDS) INZ
003700090514D2500X**** ex C2073 ZZDS          E DS                  EXTNAME(ND77008ZDS) INZ
003800100512R024 D*** ex D2500 ZZDS          E DS                  EXTNAME(ND77009ZDS) INZ
003900100512R024 D*-------------  anno 2010
004000100512R024 D AADS          E DS                  EXTNAME(ND77010ADS) INZ
004100100512R024 D BBDS          E DS                  EXTNAME(ND77010BDS) INZ
004200110413R055 D*** ex R024 ZZDS          E DS                  EXTNAME(ND77010ZDS) INZ
004300120502R102 D*** ex R055 ZZDS          E DS                  EXTNAME(ND77011ZDS) INZ
004400120502R102 D ZZDS          E DS                  EXTNAME(ND77012ZDS) INZ
004500990430     D*-------------
004600990430     D* Dati di ambiente ottenuti da XSOC
004700990430     D SOC001        E DS                  EXTNAME(XSOC001DS)
004800990430     D*-------------
004900990430     D* DS Interna per dati di output di XSOC
005000990430     D XSOCDS          DS          1000
005100990430     D*-------------
005200990430     D AMG             DS
005300990430     D  AMGAnno                1      4
005400990430     D  T1                     5      5
005500990430     D  AMGMese                6      7
005600990430     D  T2                     8      8
005700990430     D  AMGGiorno              9     10
005800990430     D*-------------
005900990430     D                 DS
006000990430     D  GG1                    1      2  0
006100990430     D  MM1                    3      4  0
006200990430     D  AA1                    5      8  0
006300990430     D  AMG19                  1      8  0
006400990430     D*-------------
006500990430     D  Rec770         DS
006600990430     D  M710TR
006700990430     D  M710CF
006800990430     D  M710PM
006900020418     D  M710F1
007000020418     D  M710CP
007100990430     D  M71rec
007200990430     D*-------------
007300990430     D WrkData         S               D
007400160607R218 X***D ContE           S              5  0 inz
007500160607R218 X***D ContH           S              5  0 inz
007501160607R218 D ContD           S              5  0 inz
007502160620R218 D ContI           S              5  0 inz
007600990430     D*-------------
007700990430     I*----------------------------------------------------*
007800990430     C*  C I C L O       P R I N C I P A L E               *
007900990430     C*----------------------------------------------------*
008000990430     C*                                                    *
008100990430     C                   Exsr      Rec770A
008200000530     C                   Exsr      Rec770B
008300990430     C*                                                    *
008400990430     C     *LOVAL        SETLL     ND77010F                                     *
008500990430     C                   READ      ND77010F                               21    *
008600990430     C     *IN21         DOWEQ     '0'
008700990430     C*
008800990430     C                   Select
008900160607R218 X***  M710TR        WHENEQ    'E'
008901160607R218 X***                Add       1             ContE
008902160607R218 C     M710TR        WHENEQ    'D'
009000160607R218 C                   Add       1             ContD
009100160607R218 X**   M710TR        WHENEQ    'H'
009200160607R218 X**                 Add       1             ContH
009201160616R218 C     M710TR        WHENEQ    'I'
009202160616R218 C                   Add       1             ContI
009300990430     C                   EndSl
009400990430     C*
009500990430     C                   MOVEL     Rec770        MFREC
009600000530     C                   MOVE      'A  '         MFREC
009700990430     C                   WRITE     M77099
009800990430     C                   READ      ND77010F                               21    *
009900990430     C                   END                                                    *
010000990430     C*                                                    *
010100990430     C                   Exsr      Rec770Z
010200990430     C*                                                    *
010300990430     C     FINE          TAG
010400990430     C                   SETON                                        LR        *
010500990430     C*----------------------------------------------------*
010600990430     C* Scrittura tipo record "A"
010700990430     C*----------------------------------------------------*
010800990430     C     Rec770A       BEGSR                                                  *
010900990430     C*
011000990430     C                   MOVEL     'A'           TMA0TR
011100030618C1667X****               MOVEL     '77S02'       TMA0F1
011200040622C1770X**** ex C1667      MOVEL     '77S03'       TMA0F1
011300050630C1846X**** ex C1770      MOVEL     '77S04'       TMA0F1
011400060619C1943X**** ex C1846      MOVEL     '77S05'       TMA0F1
011500070420C2014X***  ex C1943      MOVEL     '77S06'       TMA0F1
011600080118C2073X***  ex C2014      MOVEL     '77S07'       TMA0F1
011700090513D2500X**** ex C2073      MOVEL     '77S08'       TMA0F1
011800100512R024 C**** ex D2500      MOVEL     '77S09'       TMA0F1
011900100512R024 C*---
012000110413R055 C**** ex R024       MOVEL     '77S10'       TMA0F1
012100100512R024 C*---
012200120502R102 C**** ex R055       MOVEL     '77S11'       TMA0F1
012300110413R055 C*---
012400130219R110 C**** ex R102       MOVEL     '77S12'       TMA0F1
012500120502R102 C*---
012600140509R148 C**** ex R110       MOVEL     '77S13'       TMA0F1
012700130219R110 C*---
012800150519R186 C**** ex R148       MOVEL     '77S14'       TMA0F1
012900140509R148 C*---
012901160607R186 x*** ex r186        MOVEL     '77S15'       TMA0F1
012902160607R218 C                   MOVEL     '77S16'       TMA0F1
012903150519R186 C*---
013000050715C1846C                   MOVE      '01'          TMA0F2
013100990430     C                   MOVEL     XSCCDF        TMA0F3
013200990430     C                   MOVEL     AADS          MFREC
013300000530     C                   MOVE      'A  '         MFREC
013400990430     C                   WRITE     M77099
013500990430     C*
013600990430     C                   ENDSR
013700000530     C*----------------------------------------------------*
013800000530     C* Scrittura tipo record "B"
013900000530     C*----------------------------------------------------*
014000000530     C     Rec770B       BEGSR                                                  *
014100000530     C*
014200000530     C                   Clear                   MFREC
014300040622C1770X****               MOVEL     'B'           MFREC
014400040624C1770C                   MOVEL     'B'           TMB0TR
014500050718C1846C*
014600050718  "  C* codice fiscale del soggetto dichiarante
014700050718  "  C                   MOVEL     XSCCDF        TMBCFD1
014800050718  "  C*
014900050718  "  C* numero progressivo modulo - fisso 1
015000050718  "  C                   Z-ADD     1             TMBPRGMO
015100050718C1846C*
015200050718C1770C*
015300160607R218 X*** ex C1170* conto quante sono le certificazioni di lavoro autonomo
015400160607R218 X*** ex c1170*  presenti e se presente almeno un dato nel quadro ST
015401160607R218 C* Indico se è valorizzato il quadro ST
015500160607C1770C                   EXSR      ContNrCert
015600160607R218 X***                Z-ADD     ContH         TMBNAU1
015700160607R218 X**                 if        ContE       > 0
015701160607R218 C                   if        ContD       > 0
015800160607C1770C                   MOVEL     '1'           TMBQST1
015900160607C1770C                   endif
015901160620R218 C                   if        ContI       > 0
015902160616  "  C                   MOVEL     '1'           TMBQSY1
015903160620R218 C                   endif
016000160616R218 X***                Z-ADD     0             ContH
016100160607R218 X***                Z-ADD     0             ContE
016101160607R218 C                   Z-ADD     0             ContD
016102160620R218 C                   Z-ADD     0             ContI
016200160607C1770C*
016300040622C1770C                   MOVEL     BBDS          MFREC
016400000530     C                   MOVE      'A  '         MFREC
016500000530     C                   WRITE     M77099
016600000530     C*
016700000530     C                   ENDSR
016800040624C1770C*----------------------------------------------------*
016900040624  "  C* Conto certificazioni
017000040624  "  C*----------------------------------------------------*
017100040624C1770C     ContNrCert    BEGSR                                                  *
017200040624     C*
017300160607R218 X***                Z-ADD     0             ContH
017400160607R218 X***                Z-ADD     0             ContE
017401160607R218 C                   Z-ADD     0             ContD
017402160616R218 C                   Z-ADD     0             ContI
017500040624     C*
017600040624     C     *LOVAL        SETLL     ND77010F                                     *
017700040624     C                   READ      ND77010F                               21    *
017800040624     C     *IN21         DOWEQ     '0'
017900040624     C*
018000040624     C                   Select
018100160607R218 X***  M710TR        WHENEQ    'E'
018101160607R218 X***                Add       1             ContE
018102160607R218 C     M710TR        WHENEQ    'D'
018201160607R218 C                   Add       1             ContD
018300160607R218 X**   M710TR        WHENEQ    'H'
018400160607R218 X**                 Add       1             ContH
018401160620R218 C     M710TR        WHENEQ    'I'
018402160620R218 C                   Add       1             ContI
018500040624     C                   EndSl
018600040624     C*
018700040624     C                   READ      ND77010F                               21    *
018800040624     C                   ENDDO                                                  *
018900040624     C*
019000040624C1770C                   ENDSR
019100040624C1770C/EJECT
019200990430     C*----------------------------------------------------*
019300990430     C* Scrittura tipo record "Z"
019400990430     C*----------------------------------------------------*
019500990430     C     Rec770Z       BEGSR                                                  *
019600990430     C*
019700990430     C                   MOVEL     'Z'           TMZ0TR
019800040622C1770X****               Z-ADD     0             TMZ0NB
019900040622C1770C                   Z-ADD     1             TMZ0NB
020000160607R218 X*** EX C1770       Z-ADD     ContE         TMZ0NE
020001160607R218 C                   Z-ADD     ContD         TMZ0ND
020100160607R218 X***                Z-ADD     0             TMZ0NF
020200160607R218 X***                Z-ADD     0             TMZ0NG
020300160607R218 X***                Z-ADD     ContH         TMZ0NH
020400160616R102 X***                Z-ADD     0             TMZ0NI
020401160620R218 C                   Z-ADD     1             TMZ0NI
020500160607R218 X*** ex R102        Z-ADD     0             TMZ0NJ
020600990430     C                   MOVEL     ZZDS          MFREC
020700000530     C                   MOVE      'A  '         MFREC
020800990430     C                   WRITE     M77099
020900990430     C*
021000990430     C                   ENDSR
021100990430     C*----------------------------------------------------*
021200990430     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
021300990430     C*----------------------------------------------------*
021400990430     C     *INZSR        BEGSR                                                  *
021500990430     C* --                                                 *
021600990430     C     *ENTRY        PLIST                                                  *
021700990430     C                   PARM                    KPJBA                          *
021800990430     C                   MOVEL     KPJBU         ndcr80ds                       *
021900990430     C* Chiamata a xsoc
022000990430     C                   move      socR80        SOCXSC
022100990430     C                   MOVEL     'SOC001'      TIPXSC
022200990430     C                   CALLb     'XSOC'
022300990430     C                   PARM                    TIPXSC            6
022400990430     C                   PARM                    SOCXSC            3
022500990430     C                   PARM                    CDSXSC            9 0
022600990430     C                   PARM                    MODXSC            3
022700990430     C                   PARM      *BLANKS       RTNXSC            1
022800990430     C                   PARM                    XSOCDS
022900990430     C                   PARM                    KPJBA
023000990430     C     RTNXSC        IFNE      '1'
023100990430     C                   MOVEL     XSOCDS        SOC001
023200990430     C                   ELSE
023300990430     C                   GOTO      FINE
023400990430     C                   ENDIF
023500990430     C*
023600990430     C                   ENDSR
023700990430     C*----------------------------------------------------*
