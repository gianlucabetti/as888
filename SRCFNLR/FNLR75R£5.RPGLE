000100941214     H* FNLR75R *-----------------------------------------------------*
000200941214     H*              - VISUALIZZA VARIAZIONI BOLLE                    *
000300941214     H*---------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500940714      *
000600940714     FFNLR75D   CF   E             WORKSTN
000700940714     F                                     SFILE(LR75S01:NRR1)
000800911024     FTABEL00F  IF   E           K DISK
000900060306     FTntbe01l  IF   E           K DISK
001000941222     FFNARB01L  IF   E           K DISK    USROPN
001100160930     FFIAR601L  IF   E           K DISK    usropn
001200160930     FFiAR901L  IF   E           K DISK    usropn
001300160930     FFiAR501L  IF   E           K DISK    usropn
001400941222     F*
001500941222     FFNBLP01L  IF   E           K DISK    USROPN
001501160929     Ftitas30c  IF   E           K DISK    USROPN
001502160929     Ftncsb03l  IF   E           K DISK    USROPN
001503161006     FFiAR531c  IF   E           K DISK    usropn
001504161006     f                                     rename(fiar5000:fiar5t00)
001600130927     FFIARBF2C  IF   E           K DISK
001700991110     FFIARBU1L  IF   E           K DISK
001800940714      *------------------------------------------------------------------------*
001900941212     D BD              S              1    DIM(240) CTDATA PERRCD(80)
002000941212     D BP              S              1    DIM(80) CTDATA PERRCD(80)
002100941212     D VP              S              1    DIM(80) CTDATA PERRCD(80)
002200941214     D BK              S              1    DIM(240) CTDATA PERRCD(80)
002300941212     D BC              S              1    DIM(160) CTDATA PERRCD(80)
002400941212     D BT              S              1    DIM(240) CTDATA PERRCD(80)
002500941212     D T2              S              1    DIM(240) CTDATA PERRCD(80)
002600941212     D CR              S              1    DIM(240) CTDATA PERRCD(80)
002700941231     D BM              S              1    DIM(240) CTDATA PERRCD(80)
002800060307     D IS              S             79    DIM(2) CTDATA PERRCD(1)
002900060316     D RM              S              1    DIM(80) CTDATA PERRCD(80)
003000130927     D FI              S              1    DIM(160) CTDATA PERRCD(80)
003100130927     D REFN            S              1    DIM(160) CTDATA PERRCD(80)
003200130927     D EMLN            S              1    DIM(80) CTDATA PERRCD(80)
003300130927     D SMSN            S              1    DIM(80) CTDATA PERRCD(80)
003400130930     D NOTN            S              1    DIM(80) CTDATA PERRCD(80)
003401160930     D BKt             S              1    DIM(80) CTDATA PERRCD(80)
003402161024     D BTI             S              1    DIM(80) CTDATA PERRCD(80)
003500911024     D*
003600941214     D SKI             S              1    DIM(15)
003700940715     D WWW             S              1    DIM(30)
003800991111     D
003900991111     D Wtstdec         S                   LIKE(§CVfdc)
004000991111     D W014A           s             14
004100991111     D W013A           s             13
004200060316     D W017A           s             17
004300991111     D IMPeditato      s             13
004400991111     D w0140           s             14  0
004500991111     D w0130           s             13  0
004600991111     D w0133           s             13  3
004700991111     D w0122           s             12  2
004800991112     D wvaria          s              1  0
004900991112     D wvaria5         s             13  3
005000991112     D wsvn            s                   like(arbsv1)
005100040202     d wesiva          s              5
005101161006     d wprg            s                   like(ar5prg) inz(0)
005102161026     d wias_ed         s             14
005200940714      *------------------------------------------------------------------------*
005300940714      * DS PER I PARAMETRI DI INGRESSO/USCITA
005400920402     D PARAM           DS
005500940714     D  VCCAAS                14     17  0
005600940714     D  VCCLNP                18     20  0
005700940714     D  VCCNRS                21     22  0
005800940714     D  VCCNSP                23     29  0
005900940714     D  PARF03                30     30
006000940714     D  PARFLG                31     31
006100940714     D  PARPGM                32     41
006200940714     D  VCCDSA                42     61
006300950126     D  PARGIA               144    144
006400950126     D  PARFEL               145    147  0
006500950629     D  PARTBO               148    148
006501160930     d  partbl               149    150
006600941222      * DS PER LA ROUTINE DI CONTROLLO DELLA DATA (8 CIFRE)
006700940714     D WLBDAT          DS                  INZ
006800940714     D  G02DAT                 1      8  0
006900940714     D  G02INV                 9     16  0
007000940714     D  G02ERR                17     17
007100940714     D  G02TGI                18     22  0
007200940714      * DS PER COMPORRE LA DATA SPEDIZIONE DI FNARB00F
007300940714     D                 DS
007400940714     D  ARBDSP                 1      8  0
007500940714     D  ARBAAS                 1      4  0
007600940714     D  ARBMGS                 5      8  0
007700941222     D                 DS
007800941222     D  BLPDSP                 1      8  0
007900941222     D  BLPAAS                 1      4  0
008000941222     D  BLPMGS                 5      8  0
008100940714     D                 DS
008200940714     D  AA                     1      2  0
008300940714     D  GG                     5      6  0
008400940714     D  DATA                   1      6  0
008500940715     D DS1A          E DS
008600940715     D DS1P          E DS
008700940715     D DS3A          E DS
008800940715     D DSEI          E DS
008900940715     D DSTR          E DS
009000991111     D DSCV          E DS
009100991112     D DS$2          E DS
009200060306     D Dmtv          E DS
009300060306     D Ds7l          E DS
009400911024     D CNCR80        E DS
009500940715     D KPJBA         E DS
009600051128     D FNLV80DS      E DS
009700060306     D tibs02ds      E DS
009800060306     D dar5mtv       E DS
009900130927     d
010000130927     d darbn_REF     E DS
010100130927     d darbn_eml     E DS
010200130927     d darbn_sms     E DS
010300130930     d darbn_NOT     E DS
010400130927     d
010500120510     d*dVpodeco      e ds
010600061102     d
010700061102     D Cdest           C                   CONST('D E S T I N A T A R I O')
010800061102     D CMITT           C                   CONST(' M I T T E N T E ......')
010900940714      *------------------------------------------------------------------------*
011000940714     IFNARBG00      97
011100940714     IFNARBD00      96
011200940714     IFNARBK00      95
011300991110     IFIARBT00      94
011400941231     IFNARBV00      92
011500941231     IFNARBM00      91
011600130927     IFNARBN00      88
011700940714     C**************************************************************************
011800940714     C*          M A I N       L I N E
011900940714     C**************************************************************************
012000940714     C*
012100940714     C     *ENTRY        PLIST
012200940714     C                   PARM                    KPJBA
012300940714     C                   MOVEL     KPJBU         PARAM
012400950107     C                   CLEAR                   PARF03
012500940714     C*
012600940714     C* PARFLG = 'C' CHIUDO CON LR SENZA VISUALIZZARE
012700940714     C     PARFLG        IFEQ      'C'
012800930524     C                   SETON                                        LR
012900930524     C                   ELSE
013000940714     C*
013100940714     C                   EXSR      RIEC01
013200941222     C   99              GOTO      FINE
013300930524     C*
013400920402     C* PULIZIA SUBFILE
013500920402     C                   SETON                                        89
013600940714     C                   WRITE     LR75C01
013700920402     C                   SETOFF                                       89
013800060306     c                   clear                   wesmtv            1
013900920429     C*
014000940715     C* DECODIFICHE
014100920429     C*
014200940714     C                   Z-ADD     1             NRR1              5 0
014300991110     C                   SETOFF                                       9594
014400941231     C                   SETOFF                                       969792
014500130927     C                   SETOFF                                       9188
014600130927     C     KARB1         SETLL     FIARBF2C
014700130927     C     KARB1         READE     FIARBF2C                               31
014800920402     C*
014900930524    1C     *IN31         DOWEQ     '0'
015000991111     C                   MOVEL     ARBPRU        VSCPRU
015100941231     C   92              MOVEL     *BLANKS       VSCPRU
015101161228     c                   clear                   vscamb
015102161228     c                   if        not *in92
015103161228     c                   select
015104161228     c                   when      %subst(arbpru:10:1)='S'
015105161228     c                   eval      vscamb='SEDE'
015106161228     c                   when      %subst(arbpru:10:1)='P'
015107161228     c                   eval      vscamb='PAR '
015108161228     c                   when      %subst(arbpru:10:1)='A'
015109170102     c                   eval      vscamb='ARR '
015110161228     c                   when      %subst(arbpru:10:1)='a'
015111161228     c                   eval      vscamb='ARRp'
015112161228     c                   endsl
015113161228     c                   endif
015200940714     C                   MOVEL     *BLANKS       VSCCAM
015300940715     C*
015400940715     C*?--------- F N A R B D 0 0    ( 96 ) -----------------------------------?
015500920403     C*** VARIAZ DESTINATARIO
015600061102     c   96              select
015700061102    2C     ARBCVB        wheneq    'I0'
015800140407    2C     ARBCVB        oreq      'I7'
015900140407    2C     ARBCVB        oreq      'I8'
016000140407    2C     ARBCVB        oreq      'I9'
016100941212     C                   MOVEA     ARBRSD        BD(15)
016200941212     C                   MOVEA     ARBRD2        BD(51)
016300941212     C                   MOVEA     ARBIND        BD(87)
016400941212     C                   MOVEA     ARBCAD        BD(123)
016500941212     C                   MOVEA     ARBLOD        BD(133)
016600941212     C                   MOVEA     ARBPRD        BD(170)
016700941212     C                   MOVEA     ARBNZD        BD(174)
016800941212     C                   MOVEA     ARBFIN        BD(189)
016900051104     c                   select
017000051104     c                   when      arbffd='S'
017100051104     C                   MOVEA     'SI '         BD(206)
017200051104     c                   when      arbffd='M'
017300051104     C                   MOVEA     'SAB'         BD(206)
017400051104     C                   other
017500051104     C                   MOVEA     '   '         BD(206)
017600051104    3C                   ENDsl
017700941212     C                   MOVEA     ARBCPI        BD(225)
017800940714     C                   MOVEA     BD            VSCCAM
017900130927     C*------------------------------
018000060316     c* Rif. mitt. alfa e Natura merce
018100061102    2c     arbcvb        wheneq    'RM'
018200060316     C                   MOVEA     arbrsd        RM(22)
018300060316     C                   MOVEA     arbrd2        RM(54)
018400060316     c                   eval      w017A =  '   Natura Merce: '
018500060316     c                   movea     w017a         RM(37)
018600060316     C                   MOVEL     *BLANKS       VSCCAM
018700060316     C                   MOVEA     RM            VSCCAM
018800061102     C*------------------------------
018900071026     c* Codice fiscale / Partiva iva
019000061102    2c     arbcvb        wheneq    'FI'
019100061102     c                   if        arbrd2='D'
019200071026     c                   movea     cdest         FI(1)
019300061102     c                   else
019400071026     c                   movea     cmitt         FI(1)
019500061102     c                   endif
019600071026     c                   movea     arbcpi        FI(42)
019700071026     c* Se trascorsa la data attivazione, visualizzo anche partita iva
019800120510     c*****              if        arbdtV>§vpopiv
019900071026     c                   movel     arbrsd        w016a            16
020000071026     c                   movea     w016a         FI(122)
020100071026     C                   MOVEA     FI            VSCCAM
020200120510     c****               else
020300120510     c***                movea     fi            vsccam
020400120510     c***                eval      %subst(vsccam:106:50)=*blanks
020500120510     c****               endif
020600071026     c
020700061102     C*------------------------------
020800061102   x2c                   other
020900941216     C                   MOVEL     ARBCVB        UNOCVB            1
021000940715     C*
021100941216    3C     UNOCVB        IFEQ      'I'
021200940715     C*------------------------------
021300920402     C* PESO
021400941212     C                   MOVEL     ARBPKF        WCOM7
021500941214     C                   MOVEA     WCOM7         SKI(9)
021600940715     C                   Z-ADD     9             I                 2 0
021700941214     C                   EXSR      ABBL
021800920403     C*
021900941214    4C     SKI(14)       IFEQ      ' '
022000940715     C                   MOVEL     '0'           SKI(14)
022100940715    4C                   ENDIF
022200920403     C*
022300941212     C                   MOVEA     SKI(9)        BP(12)
022400941212     C                   MOVEA     ','           BP(18)
022500941212     C                   MOVEA     SKI(15)       BP(19)
022600940715     C*------------------------------
022700920402     C* VOLUME
022800941212     C                   MOVEL     ARBVLF        WCOM5
022900940715     C                   MOVEA     WCOM5         SKI(11)
023000940715    4C     SKI(11)       IFEQ      '0'
023100940715     C                   MOVEL     ' '           SKI(11)
023200940715    4C                   ENDIF
023300920403     C*
023400941212     C                   MOVEA     ARBFVF        BP(62)
023500941212     C                   MOVEA     SKI(11)       BP(65)
023600941212     C                   MOVEA     ','           BP(67)
023700941212     C                   MOVEA     SKI(13)       BP(68)
023800940715     C                   MOVEL     *BLANKS       VSCCAM
023900940714     C                   MOVEA     BP            VSCCAM
024000940715     C*------------------------------
024100930524   X3C                   ELSE
024200940715     C*------------------------------
024300940715     C* VARIAZIONE "VA", "VP" E "VT" DEL VOLUME
024400941212     C                   MOVEL     ARBVLF        WCOM5
024500940715     C                   MOVEA     WCOM5         SKI(11)
024600940715    4C     SKI(11)       IFEQ      '0'
024700940715     C                   MOVEL     ' '           SKI(11)
024800940715    4C                   ENDIF
024900930524     C*
025000941212     C                   MOVEA     ARBFVF        VP(23)
025100941212     C                   MOVEA     SKI(11)       VP(26)
025200941212     C                   MOVEA     ','           VP(28)
025300941212     C                   MOVEA     SKI(13)       VP(29)
025400940715     C                   MOVEL     *BLANKS       VSCCAM
025500941212     C                   MOVEA     VP            VSCCAM
025600940715    3C                   ENDIF
025700061102    2C                   ENDSL
025800130927     c
025900130927     C*?--------- F N A R B N 0 0    ( 88 ) -----------------------------------?
026000130927     C*** VARIAZ referente consegna + telefono
026100130927     C*------------------------------
026200130927     c   88              select
026300130927     c
026400130927    2c     arbtrd        wheneq    'REF'
026500130927     c                   movel     arbuni70      darbn_REf
026600130927     c                   movea     refN          vsccam
026700130927     c                   eval      %subst(vsccam:25:35)=§arbnref
026800130927     c                   eval      %subst(vsccam:105:16)=§arbnteld
026900130927    2c     arbtrd        wheneq    'SMS'
027000130927     c                   movel     arbuni70      darbn_sms
027100130927     c                   movea     smsN          vsccam
027200130927     c                   eval      %subst(vsccam:25:70)=§arbncel
027300130927    2c     arbtrd        wheneq    'EML'
027400130927     c                   movel     arbuni70      darbn_eml
027500130927     c                   movea     emlN          vsccam
027600130927     c                   eval      %subst(vsccam:25:70)=§arbneml
027700130930    2c     arbtrd        wheneq    'NOT'
027800130930     c                   movel     arbuni70      darbn_not
027900130930     c                   movea     notN          vsccam
028000130930     c                   eval      %subst(vsccam:13:70)=§arbnnot1+§arbnnot2
028100130927     c
028200130927     c                   endsl
028300130927     c
028400940715     C*
028500941231     C*?--------- F N A R B V 0 0    ( 92 ) -----------------------------------?
028600941231     C*** VARIAZ VOLUME CML
028700941231    2C   92              DO
028800941231     C* VARIAZIONE "VA", "VP" E "VT" DEL VOLUME
028900941231     C                   MOVEL     ARBVLC        WCOM5
029000941231     C                   MOVEA     WCOM5         SKI(11)
029100941231    3C     SKI(11)       IFEQ      '0'
029200941231     C                   MOVEL     ' '           SKI(11)
029300941231    3C                   ENDIF
029400941231     C*
029500941231     C                   MOVEA     ARBFVC        VP(23)
029600941231     C                   MOVEA     SKI(11)       VP(26)
029700941231     C                   MOVEA     ','           VP(28)
029800941231     C                   MOVEA     SKI(13)       VP(29)
029900941231     C                   MOVEL     *BLANKS       VSCCAM
030000941231     C                   MOVEA     VP            VSCCAM
030100941231    2C                   ENDDO
030200940715     C*?--------- F N A R B K 0 0    ( 95 ) -----------------------------------?
030300920403     C*** VARIAZ COD BOLLA
030400940715     C   95              DO
030401160930     c* KT --> variazione Tipo bolla da man.bolle di sede
030402160930     c                   if        arbcvb='KT'
030403160930     C                   MOVEA     ARBcbp        BKt(24)
030404160930     C                   MOVEA     ARBcbn        BKt(48)
030406160930     C                   MOVEL     *BLANKS       VSCCAM
030407160930     C                   MOVEA     BKt           VSCCAM
030408160930     c                   else
030500920403     C* C/ASSEGNO
030600941214     C                   MOVE      ARBCAS        WCOM13
030700941214     C                   MOVEA     WCOM13        SKI(3)
030800941214     C                   Z-ADD     3             I
030900941214     C                   EXSR      ABBL
031000991111     c     arbcas        ifgt      *zeros
031100991111     c     arbvca        andeq     *blanks
031200991111     c                   movel     'ITL'         arbvca
031300991111     c                   endif
031400920403     C*
031500941219     C                   MOVE      ARBCVB        W001A             1
031600940715     C*
031700940715     C* VERIFICO SE VARIATO IL CODICE BOLLA
031800941214     C     W001A         IFGT      *ZEROS
031900941214     C* DECODIFICO    CODICE BOLLA OLD
032000941214     C                   MOVEL     '3A'          KCOD
032100941214     C                   MOVEL     *BLANKS       KKEY
032200941214     C                   MOVEL     ARBCBP        KKEY
032300941214     C     KTAB1         CHAIN     TABEL00F                           99
032400941214     C  N99              MOVEL     TBLUNI        DS3A
032500941214     C   99              CLEAR                   DS3A
032600941214     C                   MOVEA     ARBCBP        BK(24)
032700941214     C                   MOVEA     §3ADES        BK(27)
032800920403     C*
032900940715     C* DECODIFICO IL CODICE BOLLA NUOVO
033000940714     C                   MOVEL     '3A'          KCOD
033100940715     C                   MOVEL     *BLANKS       KKEY
033200940715     C                   MOVEL     ARBCBN        KKEY
033300940715     C     KTAB1         CHAIN     TABEL00F                           99
033400940715     C  N99              MOVEL     TBLUNI        DS3A
033500940715     C   99              CLEAR                   DS3A
033600941231     C                   MOVEA     ARBCBN        BK(81)
033700941231     C                   MOVEA     §3ADES        BK(84)
033800941214     C                   MOVEA     ARBTIC        BK(138)
033900941214     C                   MOVEA     SKI(3)        BK(142)
034000941214     C                   MOVEA     ','           BK(152)
034100941214     C                   MOVEA     SKI(13)       BK(153)
034200941214     C                   MOVEA     ARBVCA        BK(158)
034300941214     C                   MOVEA     ARBGCA        BK(187)
034400920403     C*
034500940715     C                   MOVEL     *BLANKS       VSCCAM
034600940714     C                   MOVEA     BK            VSCCAM
034700920403     C                   ELSE
034800941221     C* DECODIFICO CODICE BOLLA
034900941221     C                   MOVEL     '3A'          KCOD
035000941221     C                   MOVEL     *BLANKS       KKEY
035100941221     C                   MOVEL     ARBCBP        KKEY
035200941221     C     KTAB1         CHAIN     TABEL00F                           99
035300941221     C  N99              MOVEL     TBLUNI        DS3A
035400941221     C   99              CLEAR                   DS3A
035500941221     C                   MOVEA     ARBCBP        BC(24)
035600941221     C                   MOVEA     §3ADES        BC(27)
035700920403     C* VARIATO SOLO C/ASSEGNO
035800941214     C                   MOVEA     ARBTIC        BC(106)
035900941214     C                   MOVEA     SKI(3)        BC(110)
036000941214     C                   MOVEA     ','           BC(120)
036100941214     C                   MOVEA     SKI(13)       BC(121)
036200941214     C                   MOVEA     ARBVCA        BC(126)
036300941221     C                   MOVEA     ARBGCA        BC(158)
036400920403     C*
036500940715     C                   MOVEL     *BLANKS       VSCCAM
036600991110     C                   MOVEA     BC            VSCCAM
036700940715     C                   ENDIF
036701160930     C                   ENDIF
036800940715     C                   ENDDO
036900920403     C***
037000991111     C*?--------- F I A R B T 0 0    ( 94 ) -----------------------------------?
037100991112     c     *in94         ifeq      *on
037200991112     C                   MOVEL     'CV'          KCOD
037300991112     C                   MOVEL(P)  arbdiv        KKEY
037400991112     C     KTAB1         CHAIN     TABEL                              31
037500991112     C  N31              MOVEL     TBLUNI        DSCV
037600991112     C   31              clear                   DSCV
037700991112     c                   endif
037800991111     C*** VARIAZ TASSAZIONE 1BOLLA
037900160928    2C** 94ARBTRC        IFEQ      '1'
037901160930     c                   if        *in94
037902160930     c                   if        arbtrc='1' or
037903160930     c                             %subst(arbpru:10:1)='S'
037904161024     c                   if        arbcvb='TI'
037905161024     c                   if        arbias>0
037906161026     c                   eval      wias_ed=%editc(arbias:'4')
037907161026     C                   MOVEA     wias_ed       BTI(24)
037912161024     c                   else
037913161024     C                   MOVEA     *blanks       BTI(24)
037914161024     c                   endif
037915161024     C                   MOVEA     ARBvas        BTI(39)
037916161024     C                   MOVEL     *BLANKS       VSCCAM
037917161024     C                   MOVEA     BTI           VSCCAM
037918161024     c                   else
038000930524     C*
038100940715     C                   MOVEL     ARBKSC        WCOM7
038200941214     C                   MOVEA     WCOM7         BT(5)
038300940715     C                   MOVEL     ARBCTR        WCOM3
038400941214     C                   MOVEA     WCOM3         BT(17)
038500941215     C*
038600941215     C     ARBVMD        IFGT      0
038700940715     C                   MOVE      ARBVMD        WCOM13
038800941214     C                   MOVEA     WCOM13        SKI(3)
038900941214     C                   Z-ADD     3             I
039000941214     C                   EXSR      ABBL
039100941215     C                   MOVEA     SKI(3)        BT(27)
039200941215     C                   MOVEA     ','           BT(37)
039300941215     C     BT(36)        IFEQ      ' '
039400941215     C                   MOVEL     '0'           BT(36)
039500941215     C                   ENDIF
039600941215     C                   MOVEA     SKI(13)       BT(38)
039700991111     c*
039800991111     c     arbvad        ifeq      *blanks
039900991111     c                   movel     'ITL'         arbvad
040000991111     c                   endif
040100941215     C                   ELSE
040200941215     C* NON C'E' VALORE MERCE
040300941215     C                   CLEAR                   SKI
040400941215     C                   MOVEA     SKI(2)        BT(27)
040500941215     C                   ENDIF
040600941215     C                   MOVEA     ARBVAD        BT(42)
040700940715     C*
040800941215     C     ARBIAS        IFGT      0
040900940715     C                   MOVE      ARBIAS        WCOM13
041000941214     C                   MOVEA     WCOM13        SKI(3)
041100941214     C                   Z-ADD     3             I
041200941214     C                   EXSR      ABBL
041300941215     C                   MOVEA     SKI(3)        BT(49)
041400941215     C                   MOVEA     ','           BT(59)
041500941215     C     BT(58)        IFEQ      ' '
041600941215     C                   MOVEL     '0'           BT(58)
041700941215     C                   ENDIF
041800941215     C                   MOVEA     SKI(13)       BT(60)
041900991111     c*
042000991111     c     arbvas        ifeq      *blanks
042100991111     c                   movel     'ITL'         arbvas
042200991111     c                   endif
042300941215     C                   ELSE
042400941215     C* NON C'E' IMPORTO
042500941215     C                   CLEAR                   SKI
042600941215     C                   MOVEA     SKI(2)        BT(49)
042700941215     C                   ENDIF
042800941215     C                   MOVEA     ARBVAS        BT(64)
042900941214     C*
043000941215     C     ARBQFT        IFGT      0
043100941214     C                   MOVE      ARBQFT        WCOM13
043200941214     C                   MOVEA     WCOM13        SKI(3)
043300941214     C                   Z-ADD     3             I
043400941214     C                   EXSR      ABBL
043500941215     C                   MOVEA     SKI(3)        BT(75)
043600941215     C                   MOVEA     ','           BT(85)
043700941215     C     BT(84)        IFEQ      ' '
043800941215     C                   MOVEL     '0'           BT(84)
043900941215     C                   ENDIF
044000941215     C                   MOVEA     SKI(13)       BT(86)
044100941215     C                   ELSE
044200941215     C* NON C'E' IMPORTO
044300941215     C                   CLEAR                   SKI
044400941215     C                   MOVEA     SKI(2)        BT(75)
044500941215     C                   ENDIF
044600991111     c*
044700991111     c*
044800991111     C                   MOVEA     ARBDIV        BT(93)
044900991111     c*
045000991111     c                   z-add     arbpor        w0133
045100991111     c                   movel     §cvfdc        wtstdec
045200991111     c                   exsr      edtimporti
045300991111     c                   move      impeditato    wcom10
045400991111     C                   MOVEA     wcom10        BT(102)
045500920403     C*
045600991112     c                   clear                   wvaria
045700991112     c                   clear                   wvaria5
045800941214     C     ARBSV1        IFNE      ' '
045900991112     c* non carico se bollo o iva
046000991112     c     arbsv1        ifne      §$2bol
046100991112     c     arbsv1        andne     §$2iva
046200991111     C                   z-add     ARBVA1        W0133
046300991111     c                   movel     §cvfdc        wtstdec
046400991111     c                   exsr      edtimporti
046500991111     c                   move      impeditato    wcom10
046600991112     c                   movel     arbsv1        wsvn
046700991112     c                   add       1             wvaria
046800991112     c                   exsr      rievar
046900991112     c                   else
047000991112     C                   CLEAR                   SKI
047100991112     C                   MOVEA     SKI(5)        BT(119)
047200991112     c                   endif
047300941215     C                   ELSE
047400941215     C* NON C'E' VARIA
047500941215     C                   CLEAR                   SKI
047600991111     C                   MOVEA     SKI(5)        BT(119)
047700941215     C                   ENDIF
047800920403     C*
047900941215     C     ARBSV2        IFNE      ' '
048000991112     c* non carico se bollo o iva
048100991112     c     arbsv2        ifne      §$2bol
048200991112     c     arbsv2        andne     §$2iva
048300991111     C                   z-add     ARBVA2        w0133
048400991111     C                   movel     §cvfdc        wtstdec
048500991111     C                   exsr      edtimporti
048600991111     C                   move      impeditato    wcom10
048700991112     c                   movel     arbsv2        wsvn
048800991112     c                   add       1             wvaria
048900991112     c                   exsr      rievar
049000991112     c                   else
049100991112     C                   CLEAR                   SKI
049200991112     C                   MOVEA     SKI(5)        BT(131)
049300991112     c                   endif
049400941215     C                   ELSE
049500941215     C* NON C'E' VARIA
049600941215     C                   CLEAR                   SKI
049700991111     C                   MOVEA     SKI(5)        BT(131)
049800941215     C                   ENDIF
049900940715     C*
050000941214     C     ARBSV3        IFNE      ' '
050100991112     c* non carico se bollo o iva
050200991112     c     arbsv3        ifne      §$2bol
050300991112     c     arbsv3        andne     §$2iva
050400991111     C                   z-add     ARBVA3        w0133
050500991111     C                   movel     §cvfdc        wtstdec
050600991111     C                   exsr      edtimporti
050700991111     C                   move      impeditato    wcom10
050800991112     c                   movel     arbsv3        wsvn
050900991112     c                   add       1             wvaria
051000991112     c                   exsr      rievar
051100991112     c                   else
051200991112     C                   CLEAR                   SKI
051300991112     C                   MOVEA     SKI(5)        BT(143)
051400991112     c                   endif
051500941215     C                   ELSE
051600941215     C* NON C'E' VARIA
051700941215     C                   CLEAR                   SKI
051800991111     C                   MOVEA     SKI(5)        BT(143)
051900941215     C                   ENDIF
052000941215     C*
052100991112     C* VEDO SE ESISTono altre  varie
052200941215     C                   CLEAR                   SKI
052300991111     C                   MOVEA     SKI(5)        BT(155)
052400991111     C                   MOVEA     SKI(5)        BT(167)
052500941215     C*
052600941214     C     ARBSV3        IFNE      *BLANKS
052700991111     c                   movel     arbtrc        wtrc
052800991112     C     KARBU         CHAIN     FiARBU00                           30
052900991112     c     *in30         doweq     *off
053000991112     c* non carico se bollo o iva
053100991112     c     arbsvn        ifne      §$2bol
053200991112     c     arbsvn        andne     §$2iva
053300991111     C                   z-add     ARBVAN        W0133
053400991111     C                   movel     §cvfdc        wtstdec
053500991111     C                   exsr      edtimporti
053600991111     C                   move      impeditato    wcom10
053700991112     c                   add       1             wvaria
053800991112     c                   movel     arbsvn        wsvn
053900991112     c     wvaria        ifle      5
054000991112     c     wvaria        ifeq      5
054100991112     c                   z-add     arbvan        wvaria5
054200991112     c                   endif
054300991112     c                   exsr      rievar
054400991112     c                   else
054500991112     c* se le varie da caricare sono più di 5 le sommo tutte insieme e le
054600991112     c* visualizzo nella quinta
054700991112     c                   add       arbvan        wvaria5
054800991112     c                   endif
054900991112     c                   endif
055000991112     C     KARBU         reade     FiARBU00                               30
055100991111     c                   enddo
055200941214     C                   ENDIF
055300991112     c* se ho più di cinque varie imposto la quinta = alla sommma delle
055400991112     c* varie dalla quinta in poi
055500991112     c     wvaria        ifgt      5
055600991112     C                   z-add     wvaria5       w0133
055700991112     c                   movel     §cvfdc        wtstdec
055800991112     c                   exsr      edtimporti
055900991112     c                   move      impeditato    wcom11
056000991112     C                   movea     wcom11        BT(167)
056100991112     c                   endif
056200941214     C*
056300941215     C     ARBIMV        IFGT      0
056400991111     C                   z-add     ARBIMV        W0122
056500991111     C                   movel     §cvfdc        wtstdec
056600991111     C                   exsr      edtimport122
056700991111     C                   move      impeditato    wcom12
056800991111     c                   movea     wcom12        bt(182)
056900941215     C                   ELSE
057000941215     C                   CLEAR                   SKI
057100991111     C                   MOVEA     SKI(4)        BT(182)
057200941215     C                   ENDIF
057300941214     C*
057400991111     C                   MOVEA     ARBCEI        BT(201)
057500941214     C*
057600941215     C     ARBIFT        IFGT      0
057700991111     C                   z-add     ARBIFT        W0122
057800991111     C                   movel     §cvfdc        wtstdec
057900991111     C                   exsr      edtimport122
058000991111     C                   move      impeditato    wcom12
058100991111     c                   movea     wcom12        bt(212)
058200941215     C                   ELSE
058300941215     C                   CLEAR                   SKI
058400991111     C                   MOVEA     SKI(4)        BT(212)
058500941215     C                   ENDIF
058600920403     C*
058700940715     C                   MOVEL     ARBNFT        WCOM6
058800940715     C                   MOVEA     WCOM6         SKI(10)
058900940715     C                   Z-ADD     10            I
059000920403     C                   EXSR      ABBL
059100941214     C                   MOVEA     SKI(10)       BT(229)
059200920403     C*
059300940715     C     ARBDFT        IFNE      0
059400940715     C                   MOVE      ARBDFT        WCOM8
059500941214     C                   MOVEA     WCOM8         SKI(8)
059600941214     C                   MOVEA     SKI(14)       WGG               2
059700941214     C                   MOVEA     SKI(12)       WMM               2
059800941214     C                   MOVEA     SKI(8)        WAA               4
059900941214     C                   MOVEA     WGG           BT(236)
060000941214     C                   MOVEA     '/'           BT(238)
060100941214     C                   MOVEA     WMM           BT(239)
060200920403     C                   ELSE
060300941215     C                   MOVEA     *BLANKS       BT(229)
060400920403     C                   END
060500920403     C*
060600940715     C                   MOVEL     *BLANKS       VSCCAM
060700940714     C                   MOVEA     BT            VSCCAM
060800991111     c
060801161024     c                   endif
060802161024
060900991111    2C                   else
061000940715     C*
061100991111     C*?--------- F I A R B T 0 0    ( 94 ) -----------------------------------?
061200940715     C*   VARIAZ TASSAZIONE 2 BOLLA
061300991112     c* non carico se bollo o iva
061400040202      * carico tutte e 4 le varie
061500040202     c                   Clear                   Ski
061600041130     c                   Clear                   wvaria
061700041130     c                   Clear                   wvaria5
061800041130
061900041130     c                   Movea     ski(5)        T2(88)
062000041130     c                   Movea     ski(5)        T2(100)
062100041130     c                   Movea     ski(5)        T2(112)
062200041130     c                   Movea     ski(5)        T2(124)
062300041130     c                   Movea     ski(5)        T2(136)
062400041130     c                   Movea     ski(5)        T2(148)
062500040202
062600040202if  3c                   if        ArbSv1 <> §$2bol and ArbSv1 <> §$2iva
062700040202     c                   Z-add     ArbVa1        w0133
062800040202     c                   Eval      Wtstdec = §cvfdc
062900040202     c                   ExSr      EDTimporti
063000041130     c                   Move      IMPeditato    wcom10
063100041130     c                   Eval      wsvn = ArbSv1
063200041130     c                   add       1             wvaria
063300041130     c                   ExSr      rievar1
063400040202    3c                   EndIf
063500040202
063600040202if  3c                   if        ArbSv2 <> §$2bol and ArbSv2 <> §$2iva
063700040202     c                   Z-add     ArbVa2        w0133
063800040202     c                   Eval      Wtstdec = §cvfdc
063900040202     c                   ExSr      EDTimporti
064000041130     c                   Move      IMPeditato    wcom10
064100041130     c                   Eval      wsvn = ArbSv2
064200041130     c                   add       1             wvaria
064300041130     c                   ExSr      rievar1
064400040202    3c                   EndIf
064500040202
064600040202if  3c                   if        ArbSv3 <> §$2bol and ArbSv3 <> §$2iva
064700040202     c                   Z-add     ArbVa3        w0133
064800040202     c                   Eval      Wtstdec = §cvfdc
064900040202     c                   ExSr      EDTimporti
065000041130     c                   Move      IMPeditato    wcom10
065100041130     c                   Eval      wsvn = ArbSv3
065200041130     c                   add       1             wvaria
065300041130     c                   ExSr      rievar1
065400040202    3c                   EndIf
065500040202
065600040202     c                   Movel     ArbTrc        wtrc
065700040202     c     kArbu         Setll     Fiarbu1l
065800040202do  3c                   Do        *Hival
065900040202     c     kArbu         Reade     Fiarbu1l
066000040202     c                   If        %Eof(Fiarbu1l)
066100040202     c                   Leave
066200040202     c                   EndIf
066300040202if  4c                   if        ArbSvn <> §$2bol and ArbSvn <> §$2iva
066400040202     c                   Z-add     ArbVan        w0133
066500040202     c                   Eval      Wtstdec = §cvfdc
066600040202     c                   ExSr      EDTimporti
066700041130     c                   Move      IMPeditato    wcom10
066800041130     c                   Eval      wsvn = ArbSvn
066900041130     c                   add       1             wvaria
067000041130if  5c                   If        wvaria <= 6
067100041130if  6c                   If        wvaria = 6
067200041130     c                   Z-add     ArbVan        wvaria5
067300041130    6c                   EndIf
067400041130     c                   ExSr      rievar1
067500041130   x5c                   Else
067600041130      * se le varie da caricare sono più di 6 le sommo tutte insieme e le visualizzo nella sesta
067700041130     c                   add       ArbVan        wvaria5
067800041130    5c                   EndIf
067900040202    4c                   EndIf
068000040202    3c                   EndDo
068100041130      * imposto la sesta varia a video = alla somma delle varie dalla sesta in poi
068200041130     c                   If        wvaria > 6
068300041130     c                   Z-add     wvaria5       w0133
068400041130     c                   Eval      Wtstdec = §cvfdc
068500041130     c                   ExSr      EDTimporti
068600041130     c                   Move      IMPeditato    wcom11
068700041130     c                   Movea     wcom11        T2(149)
068800041130     c                   EndIf
068900040202
069000040202     c**!!!arbsv1        ifne      §$2bol
069100040202     c**!!!arbsv1        andne     §$2iva
069200040202     C**!!!              MOVEA     ARBSV1        T2(70)
069300040202     C**!!!              z-add     ARBVA1        w0133
069400040202     c**!!!              else
069500040202     C**!!!              MOVEA     ARBSV2        T2(70)
069600040202     C**!!!              z-add     ARBVA2        w0133
069700040202     c**!!!              endif
069800040202     c**!!!              movel     §cvfdc        wtstdec
069900040202     c**!!!              exsr      edtimporti
070000040202     c**!!!              move      impeditato    wcom10
070100040202     C**!!!              MOVEA     WCOM10        t2(71)
070200930210     C* IMPORTO FATTURA
070300991111     C                   z-add     ARbIFT        w0122
070400991111     C                   movel     §cvfdc        wtstdec
070500991111     C                   exsr      edtimport122
070600991111     C                   move      impeditato    wcom12
070700040202     C**!!!              MOVEA     wcom12        T2(97)
070800040202     c                   MOVEA     wcom12        T2(175)
070900920403     C* ES IVA
071000040202     C**!!!              MOVEA     ARbCEI        T2(125)
071100040202     c                   MOVEA     ARbCEI        T2(197)
071200920403     C*
071300991111     C     ARbCEI        IFNE      ' '
071400040202     C**!!!              MOVEA     '-'           T2(126)
071500040202     c                   MOVEA     '-'           T2(198)
071600940714     C                   MOVEL     'EI'          KCOD
071700991111     C                   MOVEL(P)  ARBCEI        KKEY
071800940715     C     KTAB1         CHAIN     TABEL00F                           99
071900940715     C  N99              MOVEL     TBLUNI        DSEI
072000950203     C   99              CLEAR                   DSEI
072100940715     C                   ELSE
072200940715     C                   CLEAR                   DSEI
072300040202     C**!!!              MOVEL     ' '           T2(126)
072400040202     c                   MOVEL     ' '           T2(198)
072500940715     C                   ENDIF
072600040202     C**!!!              MOVEA     §EIDES        T2(127)
072700040202     c                   movel     §eides        wesiva
072800040202     c                   MOVEA     wesiva        T2(199)
072900920403     C*
073000920403     C* FIL IVA /NUMERO /DATA FATTURA
073100991111     C     ARbfIV        IFGT      0
073200991111     C                   MOVEL     ARbFIV        WCOM3
073300950203     C                   ELSE
073400950203     C                   CLEAR                   WCOM3
073500950203     C                   ENDIF
073600040202     C**!!!              MOVEA     WCOM3         T2(170)
073700040202     c                   MOVEA     WCOM3         T2(214)
073800920403     C*
073900991111     C     ARBNFT        IFGT      0
074000991111     C                   MOVEL     ARBNFT        WCOM6
074100940715     C                   MOVEA     WCOM6         SKI(10)
074200940715     C                   Z-ADD     10            I
074300920403     C                   EXSR      ABBL
074400040202     C**!!!              MOVEA     SKI(10)       T2(175)
074500040202     c                   MOVEA     SKI(10)       T2(219)
074600950203     C                   ELSE
074700950203     C                   CLEAR                   WCOM6
074800040202     C**!!!              MOVEA     WCOM6         T2(175)
074900040202     c                   MOVEA     WCOM6         T2(219)
075000950203     C                   ENDIF
075100920403     C*
075200991111     C     ARBDFT        IFNE      0
075300991111     C                   MOVEL     ARBDFT        WCOM8
075400941215     C                   MOVEA     WCOM8         SKI(8)
075500941215     C                   MOVEA     SKI(14)       WGG               2
075600941215     C                   MOVEA     SKI(12)       WMM               2
075700941215     C                   MOVEA     SKI(8)        WAA               4
075800040202     c                   Move      waa           waa2              2
075900941215     C*
076000040202     C**!!!              MOVEA     WGG           T2(186)
076100040202     c                   MOVEA     WGG           T2(230)
076200040202     C**!!!              MOVEA     '/'           T2(188)
076300040202     c                   MOVEA     '/'           T2(232)
076400040202     C**!!!              MOVEA     WMM           T2(189)
076500040202     c                   MOVEA     WMM           T2(233)
076600040202     C**!!!              MOVEA     '/'           T2(191)
076700040202     c                   MOVEA     '/'           T2(235)
076800040202     C**!!!              MOVEA     WAA           T2(192)
076900040202     c                   MOVEA     WAA2          T2(236)
077000920403     C                   ELSE
077100920403     C                   MOVEA     *BLANKS       SKI
077200040202     C**!!!              MOVEA     SKI(5)        T2(186)
077300040202     c                   MOVEA     SKI(5)        T2(230)
077400940715     C                   ENDIF
077500940715     C*
077600991111     C                   MOVEL     ARBKSC        WCOM7
077700040202     C**!!!              MOVEA     WCOM7         T2(12)
077800040202     c                   MOVEA     WCOM7         T2(14)
077900991111     C                   MOVEL     ARBCTR        WCOM3
078000040202     C**!!!              MOVEA     WCOM3         T2(31)
078100040202     c                   MOVEA     WCOM3         T2(36)
078200040202     C**!!!              MOVEA     '-'           T2(34)
078300040202     c                   MOVEA     '-'           T2(39)
078400940715     C                   MOVEL     'TR'          KCOD
078500991112     C                   MOVEL     ARBCTR        unoctr            1
078600991112     c                   movel(P)  unoctr        kkey
078700940715     C     KTAB1         CHAIN     TABEL00F                           99
078800940715     C  N99              MOVEL     TBLUNI        DSTR
078900991112     C   99              clear                   DSTR
079000040202     C**!!!              MOVEA     §TRDES        T2(35)
079100040202     c                   MOVEA     §TRDES        T2(40)
079200991112     c*
079300040202     C**!!!              MOVEA     arbdiv        T2(60)
079400040202     c                   MOVEA     arbdiv        T2(67)
079500920403     C*
079600940715     C                   MOVEL     *BLANKS       VSCCAM
079700940714     C                   MOVEA     T2            VSCCAM
079800920403     C*
079900991111     C                   ENDif
079901160930     C                   ENDif
080000940715     C*
080100940715     C*?--------- F N A R B G 0 0    ( 97 ) -----------------------------------?
080200940715     C*   CONSEGNA RICHIESTA GIORNI CHIUSURA
080300940715     C   97              DO
080400950111     C                   MOVEA     '   '         CR(30)
080500051128     C                   MOVEA     *BLANKS       SKI(7)
080600051128     C                   MOVEA     SKI(5)        CR(21)
080700051128     c* Verifico tramite fnlv80r se era una tassativa
080800051128     c                   if        arbdcr>0 or arbhcr>0
080900051128     c                   clear                   fnlv80ds
081000051128     c                   eval      ilv80aas=arbaas
081100051128     c                   eval      ilv80lnp=arblnp
081200051128     c                   eval      ilv80nrs=arbnrs
081300051128     c                   eval      ilv80nsp=arbnsp
081400051202     c                   if        partbo='A'
081500051128     c                   eval      ilv80mgs=arbmgs
081600051202     c                   else
081700051202     c                   eval      ilv80mgs=blpmgs
081800051202     c                   endif
081900051128     c                   eval      ilv80tc1=arbtc1
082000051128     c                   eval      ilv80tc2=arbtc2
082100051128     c                   eval      ilv80dcr=arbdcr
082200051128     c                   eval      ilv80hcr=arbhcr
082300051128     c                   eval      ilv80tcr=arbtcr
082400051128     c                   call      'FNLV80R'
082500051128     c                   parm                    kpjba
082600051128     c                   parm                    fnlv80ds
082700051128     c
082800051128     c                   movea     olv80tcrd     cr(21)
082900051128     C                   ENDIF
083000051128     C* DATA
083100051128     c                   if        arbdcr>0
083200051128     C                   MOVEL     ARBDCR        WCOM8
083300051128     C                   MOVEA     WCOM8         SKI(8)
083400051128     C                   MOVEA     SKI(14)       WGG               2
083500051128     C                   MOVEA     SKI(12)       WMM               2
083600051128     C                   MOVEA     SKI(8)        WAA               4
083700051128     C*
083800051128     C                   MOVEA     WGG           CR(33)
083900051128     C                   MOVEA     '/'           CR(35)
084000051128     C                   MOVEA     WMM           CR(36)
084100051128     C                   MOVEA     '/'           CR(38)
084200051128     C                   MOVEA     WAA           CR(39)
084300051128     c                   else
084400051128     C                   MOVEA     *BLANKS       SKI(6)
084500051128     C                   MOVEA     SKI(6)        CR(33)
084600051128     c                   endif
084700051128     c* ORA
084800051128     c                   if        arbhcr>0
084900051128     C                   MOVEL     ARBHCR        WCOM4
085000051128     C                   MOVEA     WCOM4         SKI(12)
085100051128     C                   MOVEA     SKI(12)       CR(45)
085200051128     C                   MOVEA     ':'           CR(47)
085300051128     C                   MOVEA     SKI(14)       CR(48)
085400051128     C                   ELSE
085500051128     C                   MOVEA     *BLANKS       SKI(11)
085600051128     C                   MOVEA     SKI(11)       CR(45)
085700051128     c                   endif
085800051128     C
085900940715     C* GIORNI CHIUSURA
086000941213     C                   MOVEA     ARBGC1        CR(74)
086100941213     C                   MOVEA     ARBGC2        CR(78)
086200941212     C*
086300941212     C*------------------------------
086400941212     C* TIPO CONSEGNA PARTICOLARE (1)
086500941212     C                   MOVEL     *BLANKS       §1PDES
086600941213     C                   MOVEL     ' '           CR(113)
086700941231    3C     ARBTC1        IFNE      *BLANKS
086800941212     C                   MOVEL     '1P'          KCOD
086900941212     C                   MOVEL     *BLANKS       KKEY
087000941212     C                   MOVEL     ARBTC1        KKEY
087100941212     C     KTAB1         CHAIN     TABEL00F                           99
087200941212     C  N99              MOVEL     TBLUNI        DS1P
087300941213     C                   MOVEL     '-'           CR(113)
087400941231    3C                   ENDIF
087500941213     C                   MOVEA     ARBTC1        CR(111)
087600941213     C                   MOVEA     §1PDES        CR(115)
087700941212     C*------------------------------
087800941212     C* TIPO CONSEGNA PARTICOLARE (2)
087900941212     C                   MOVEL     *BLANKS       §1PDES
088000941213     C                   MOVEL     ' '           CR(193)
088100941231    3C     ARBTC2        IFNE      *BLANKS
088200941212     C                   MOVEL     '1P'          KCOD
088300941212     C                   MOVEL     *BLANKS       KKEY
088400941212     C                   MOVEL     ARBTC2        KKEY
088500941212     C     KTAB1         CHAIN     TABEL00F                           99
088600941212     C  N99              MOVEL     TBLUNI        DS1P
088700941213     C                   MOVEL     '-'           CR(193)
088800941231    3C                   ENDIF
088900941213     C                   MOVEA     ARBTC2        CR(191)
089000941213     C                   MOVEA     §1PDES        CR(195)
089100940715     C*
089200940715     C                   MOVEL     *BLANKS       VSCCAM
089300941231     C                   MOVEA     CR            VSCCAM
089400941231    2C                   END
089500941231     C*
089600941231     C*?--------- F N A R B M 0 0    ( 91 ) -----------------------------------?
089700941231     C*** VARIAZ DESTINATARIO
089800941231    2C   91              DO
089900941231     C     ARBCCM        IFGT      0
090000941231     C                   MOVEL     ARBCCM        WCOM7
090100941231     C                   ELSE
090200941231     C                   CLEAR                   WCOM7
090300941231     C                   ENDIF
090400941231     C                   MOVEA     WCOM7         BM(11)
090500941231     C                   MOVEA     ARBRSM        BM(20)
090600941231     C                   MOVEA     ARBINM        BM(81)
090700941231     C                   MOVEA     ARBCAM        BM(118)
090800941231     C                   MOVEA     ARBLOM        BM(129)
090900941231     C                   MOVEA     ARBPRM        BM(167)
091000941231     C                   MOVEA     ARBNZM        BM(171)
091100941231     C                   MOVEA     ARBFAP        BM(188)
091200941231     C                   MOVEA     ARBCPI        BM(207)
091300941231     C                   MOVEA     BM            VSCCAM
091400941231     C                   ENDDO
091500940715     C* -----------------------------------------------------------------------
091600920403     C*
091700920403     C* GIRO DATA VARIAZIONE
091800940714     C                   Z-ADD     ARBDTV        G02INV
091900940714     C                   Z-ADD     *ZEROS        G02DAT
092000940714     C                   MOVEL     '3'           G02ERR
092100940714     C                   CALL      'XSRDA8'
092200940714     C                   PARM                    WLBDAT
092300940714     C                   Z-ADD     G02DAT        VSCDTV
092400060307     C                   Z-ADD     ARBDTV        vshdtv
092500940714     C                   MOVEL     ARBORV        VSCORV
092600060307     C                   MOVEL     ARBORV        VShORV
092700940714     C                   MOVEL     ARBCVB        VSCCVB
092800920402     C* CAUSALE VARIAZIONE
092900060306     c                   clear                   ds7l
093000940714     C                   MOVEL     '7L'          KCOD
093100940714     C                   MOVEL     *BLANKS       KKEY
093200940714     C                   MOVEL     VSCCVB        KKEY
093300940715     C     KTAB1         CHAIN     TABEL00F                           99
093400940714     C* RIEMPO CAMPO VSDCVB
093500060306     c  n99              movel     tbluni        ds7l
093600060306     c                   clear                   vsdcvb
093700060306     c                   clear                   vsdcvbw
093800060306     c                   eval      vsdcvb = arbcvb + '-' + §7ldes
093900060306     c                   eval      vsdcvbw = arbcvb + '-' + §7ldes
094000060306     c
094100060306     C**                 MOVEL     *BLANKS       WWW
094200060306     C**N99              MOVEA     TBLUNI        WWW(4)
094300060306     C** 99              MOVEA     *BLANKS       WWW(4)
094400060306     C**                 MOVEA     VSCCVB        WWW(1)
094500060306     C**                 MOVEA     '-'           WWW(3)
094600060306     C**                 MOVEA     WWW           VSDCVB
094700060306     C**                 MOVEA     WWW           VSDCVBw
094800920402     C*
094900060306     c* verifico se per la variazione esiste motivo
095000060306     c                   exsr      ges_mtv
095100060306     c*
095200940714     C                   WRITE     LR75S01
095300940714     C                   ADD       1             NRR1
095400920402     C*
095500991111     C                   SETOFF                                       9594
095600941231     C                   SETOFF                                       969792
095700130927     C                   SETOFF                                       9188
095800130927     C     KARB1         READE     FiARBF2C                               31
095900920402     C                   END
096000940715     C*
096100940714     C     NRR1          IFNE      1
096200940714     C                   Z-ADD     1             NRR1
096300940714     C                   Z-ADD     1             REC1
096400060306     c                   if        wesmtv = '1'
096500060307     c                   eval      intesta = is(2)
096600060306     c                   endif
096700920402     C     FORCT3        TAG
096800940714     C                   WRITE     LR75Z01
096900940714     C                   EXFMT     LR75C01
097000060306     c* se in almeno una variazione è presente il motivo devo leggere il
097100060306     c* subfile per visualizzare il motivo se richiesto
097200060306     c                   if        wesmtv = '1'
097300060306     c                   if        *inkc = '0' and *inkl = '0'
097400060306     c                   exsr      vis_mtv
097500060306     c                   goto      forct3
097600060306     c                   endif
097700060306     c                   endif
097800940802     C*
097900940715     C                   ENDIF
098000930524     C*
098100940802     C   KC              MOVEL     '1'           PARF03
098200940802     C   KC              MOVEL     PARAM         KPJBU
098300940802     C*
098400941222     C     FINE          TAG
098500051128     C  nlr              RETURN
098600051128     c                   if        *inlr
098700051128     c                   clear                   fnlv80ds
098800051128     c                   eval      ilv80tla = 'C'
098900051128     c                   call      'FNLV80R'
099000051128     c                   parm                    kpjba
099100051128     c                   parm                    FNLV80DS
099200060307     c*
099300060307     C                   CLEAR                   tibs02ds
099400060307     C                   MOVEL     'C'           T02TLA
099500060307     C                   CALL      'TIBS02R'
099600060307     C                   PARM                    KPJBA
099700060307     C                   PARM                    tibs02ds
099800051128     c                   endif
099900940714     C                   END
100000991112     C**************************************************************************
100100991112     C*  riempie varie a video
100200991112     C**************************************************************************
100300991112     C     rievar        BEGSR
100400991112     C*
100500991112     c                   select
100600991112     c     wvaria        wheneq    1
100700991112     C                   MOVEA     wcom10        BT(120)
100800991112     C                   MOVEA     wsvn          BT(119)
100900991112     c     wvaria        wheneq    2
101000991112     C                   MOVEA     wcom10        BT(132)
101100991112     C                   MOVEA     wsvn          BT(131)
101200991112     c     wvaria        wheneq    3
101300991112     C                   MOVEA     wcom10        BT(144)
101400991112     C                   MOVEA     wsvn          BT(143)
101500991112     c     wvaria        wheneq    4
101600991112     c                   MOVEA     wcom10        BT(156)
101700991112     C                   MOVEA     wsvn          BT(155)
101800991112     c     wvaria        wheneq    5
101900991112     C                   MOVEA     wcom10        BT(168)
102000991112     C                   MOVEA     wsvn          BT(167)
102100991112     c                   endsl
102200991112     c                   endsr
102300041130     C**************************************************************************
102400041130     C*  riempie varie a video seconda bolla
102500041130     C**************************************************************************
102600041130     C     rievar1       BEGSR
102700041130     C*
102800041130     c                   select
102900041130     c     wvaria        wheneq    1
103000041130     C                   MOVEA     wcom10        T2(89)
103100041130     C                   MOVEA     wsvn          T2(88)
103200041130     c     wvaria        wheneq    2
103300041130     C                   MOVEA     wcom10        T2(101)
103400041130     C                   MOVEA     wsvn          T2(100)
103500041130     c     wvaria        wheneq    3
103600041130     C                   MOVEA     wcom10        T2(113)
103700041130     C                   MOVEA     wsvn          T2(112)
103800041130     c     wvaria        wheneq    4
103900041130     c                   MOVEA     wcom10        T2(125)
104000041130     C                   MOVEA     wsvn          T2(124)
104100041130     c     wvaria        wheneq    5
104200041130     C                   MOVEA     wcom10        T2(137)
104300041130     C                   MOVEA     wsvn          T2(136)
104400041130     c     wvaria        wheneq    6
104500041130     C                   MOVEA     wcom10        T2(149)
104600041130     C                   MOVEA     wsvn          T2(148)
104700041130     c                   endsl
104800041130     c                   endsr
104900940714     C**************************************************************************
105000940714     C*  ABBLANCA ZERO NON SIGNIFICATIVI
105100940714     C**************************************************************************
105200920403     C     ABBL          BEGSR
105300940714     C*
105400940715     C                   SETOFF                                       99
105500920403     C     SKI(I)        DOWEQ     '0'
105600920403     C                   MOVEL     ' '           SKI(I)
105700920403     C                   ADD       1             I
105800941214     C     I             COMP      15                                     99
105900940715     C  N99              END
106000940714     C*
106100920403     C                   ENDSR
106200940714     C**************************************************************************
106300940714     C*  RIEMPIMENTO CAMPI DEL FORMATO LR75C01 (SUBFILE CONTROL)
106400940714     C**************************************************************************
106500940714     C     RIEC01        BEGSR
106501160929     c                   select
106600950629     C* BOLLA IN PARTENZA
106700160929    1C                   when      PARTBO='P'
106800941222     C                   SETOFF                                       10
106900950629    2C     WAPRP         IFEQ      ' '
107000941222     C                   MOVEL     '1'           WAPRP             1
107100941222     C                   OPEN      FNBLP01L
107101160930     C                   OPEN      Fiar601L
107102160930     C                   OPEN      Fiar901L
107103160930     C                   OPEN      Fiar501L
107200950629    2C                   ENDIF
107201160929     C     KARB1         CHAIN     FNBLP01L                           99
107202160929     C*
107203160929     C   99              GOTO      ENDRIE
107204160929     C*
107205160929     C                   Z-ADD     BLPDSP        G02INV
107206160929     C                   Z-ADD     *ZEROS        G02DAT
107207160929     C                   MOVEL     '3'           G02ERR
107208160929     C                   CALL      'XSRDA8'
107209160929     C                   PARM                    WLBDAT
107210160929     C                   Z-ADD     G02DAT        VCCDSP
107211160929     C*
107212160929     C                   MOVEL     '3A'          KCOD
107213160929     C                   MOVEL     *BLANKS       KKEY
107214160929     C                   MOVEL     BLPCBO        KKEY
107215160929     C     KTAB1         CHAIN     TABEL00F                           99
107216160929     C   99              CLEAR                   DS3A
107217160929     C  N99              MOVEL     TBLUNI        DS3A
107218160929     C                   MOVEL     *BLANKS       WWW
107219160929     C                   MOVEA     §3ADES        WWW(4)
107220160929     C                   MOVEA     BLPCBO        WWW(1)
107221160929     C                   MOVEA     '-'           WWW(3)
107222160929     C                   MOVEA     WWW           VCDCBO
107223160929     C*
107224160929     C                   MOVEL     BLPRSD        VCCRSD
107225160929     C                   MOVEL     BLPIND        VCCIND
107226160929     C                   MOVEL     BLPLOD        VCCLOD
107227160929     C                   MOVEL     BLPPRD        VCCPRD
107228160929     C                   Z-ADD     BLPKSC        VCCKSC
107229160929     C                   Z-ADD     BLPPKF        VCCPKF
107230160929     C                   Z-ADD     BLPVLF        VCCVLF
107231160929     C                   MOVEL     BLPFVF        VCCFVF
107232160929     C*
107233160929     C                   MOVEL     *BLANKS       VCDTC1
107234160929     C     BLPTC1        IFNE      *BLANKS
107235160929     C                   MOVEL     '1P'          KCOD
107236160929     C                   MOVEL     *BLANKS       KKEY
107237160929     C                   MOVEL     BLPTC1        KKEY
107238160929     C     KTAB1         CHAIN     TABEL00F                           99
107239160929     C                   MOVEL     *BLANKS       WWW
107240160929     C  N99              MOVEA     TBLUNI        WWW(3)
107241160929     C                   MOVEL     BLPTC1        WWW(1)
107242160929     C                   MOVEL     '-'           WWW(2)
107243160929     C                   MOVEA     WWW           VCDTC1
107244160929     C                   ENDIF
107245160929     C                   MOVEL     *BLANKS       VCDTC2
107246160929     C     BLPTC2        IFNE      *BLANKS
107247160929     C                   MOVEL     '1P'          KCOD
107248160929     C                   MOVEL     *BLANKS       KKEY
107249160929     C                   MOVEL     BLPTC2        KKEY
107250160929     C     KTAB1         CHAIN     TABEL00F                           99
107251160929     C                   MOVEL     *BLANKS       WWW
107252160929     C  N99              MOVEA     TBLUNI        WWW(3)
107253160929     C                   MOVEL     BLPTC2        WWW(1)
107254160929     C                   MOVEL     '-'           WWW(2)
107255160929     C                   MOVEA     WWW           VCDTC2
107256160929     C                   END
107257160929     C* C/ASSEGNO
107258160929     C     KARB1         CHAIN     Fiar901L                           99
107259160929     C     *IN99         IFEQ      *OFF
107260160929     C                   MOVEL     ar9TIC        VCCTIC
107261160929     C                   Z-ADD     ar9CAS        VCCCAS
107262160929     C                   MOVEL     ar9VCA        VCCVCA
107263160929     c     vcccas        ifgt      *zeros
107264160929     c     vccvca        andeq     *blanks
107265160929     c                   movel     'ITL'         vccvca
107266160929     c                   endif
107267160929     C                   ELSE
107268160929     C                   CLEAR                   VCCTIC
107269160929     C                   CLEAR                   VCCCAS
107270160929     C                   CLEAR                   VCCVCA
107271160929     C                   ENDIF
107272160929     C*
107273160929     C***                Z-ADD     BLPVMD        VCCVMD
107274160929     C***                MOVEL     BLPVAD        VCCVAD
107275160929     c***  vccvmd        ifgt      *zeros
107276160929     c***  vccvad        andeq     *blanks
107277160929     c***                movel     'ITL'         vccvad
107278160929     c***                endif
107279160929     C                   Z-ADD     BLPIAS        VCCIAS
107280160929     C                   MOVEL     BLPVAS        VCCVAS
107281160929     c     vccias        ifgt      *zeros
107282160929     c     vccvas        andeq     *blanks
107283160929     c                   movel     'ITL'         vccvas
107284160929     c                   end
107285160929     C                   MOVEL     'TR'          KCOD
107286160929     C                   MOVEL     *BLANKS       KKEY
107287160929     C                   MOVEL     BLPCTR        UNOCTR            1
107288160929     C                   MOVEL     UNOCTR        KKEY
107289160929     C     KTAB1         CHAIN     TABEL00F                           99
107290160929     C                   MOVEL     TBLUNI        DSTR
107291160929     C                   MOVEL     *BLANKS       WWW
107292160929     C                   MOVEA     §TRDES        WWW(5)
107293160929     C                   MOVEL     BLPCTR        WCOM3
107294160929     C                   MOVEA     WCOM3         WWW(1)
107295160929     C                   MOVEA     '-'           WWW(4)
107296160929     C                   MOVEA     WWW           VCDCTR
107297160929     c                   select
107298160929     c                   when      blpffd='S'
107299160929     C                   MOVEL     'SI '         VCCFFD
107300160929     c                   when      blpffd='M'
107301160929     C                   MOVEL     'SAB'         VCCFFD
107302160929     C                   other
107303160929     C                   MOVEL     '   '         VCCFFD
107304160929     C                   ENDsl
107305160929     c* consegna richiesta
107306160929     c                   clear                   VCDCRICH
107307160929     c                   if        blpdcr>0 or blphcr>0
107308160929     c                   clear                   fnlv80ds
107309160929     c                   eval      ilv80aas=blpaas
107310160929     c                   eval      ilv80lnp=blplnp
107311160929     c                   eval      ilv80nrs=blpnrs
107312160929     c                   eval      ilv80nsp=blpnsp
107313160929     c                   eval      ilv80mgs=blpmgs
107314160929     c                   eval      ilv80tc1=blptc1
107315160929     c                   eval      ilv80tc2=blptc2
107316160929     c                   eval      ilv80dcr=blpdcr
107317160929     c                   eval      ilv80hcr=blphcr
107318160929     c                   eval      ilv80tcr=blptcr
107319160929     c                   call      'FNLV80R'
107320160929     c                   parm                    kpjba
107321160929     c                   parm                    fnlv80ds
107322160929     c                   eval      VCDCRICH=%trim(olv80tcrd)
107323160929     c                   if        blpdcr>0
107324160929     c                   eval      VCDCRICH=%trim(VCDCRICH) + ' ' +
107325160929     c                             %editw(olv80dcr6:'  /  /  ')
107326160929     c                   endif
107327160929     c                   if        blphcr>0
107328160929     c                   eval      VCDCRICH=%trim(VCDCRICH) + ' ' +
107329160929     c                             %editw(blphcr:'  :  ')
107330160929     c                   endif
107331160929     c                   endif
107332160929     C*
107333160929     c                   exsr      rietas
107335941222     C*
107400160929     C                   when      PARTBO='A'
107500950629     C* BOLLA IN ARRIVO
107600941222     C                   SETON                                        10
107700950629    2C     WAPRA         IFEQ      ' '
107800941222     C                   MOVEL     '1'           WAPRA             1
107900941222     C                   OPEN      FNARB01L
107901160930     C                   OPEN      Fiar601L
107902160930     C                   OPEN      Fiar901L
107903160930     C                   OPEN      Fiar501L
108000950629    2C                   ENDIF
108001160929     C     KARB1         CHAIN     FNARB01L                           99
108002160929     C*
108003160929     C   99              GOTO      ENDRIE
108004160929     C*
108005160929     C                   Z-ADD     ARBDSP        G02INV
108006160929     C                   Z-ADD     *ZEROS        G02DAT
108007160929     C                   MOVEL     '3'           G02ERR
108008160929     C                   CALL      'XSRDA8'
108009160929     C                   PARM                    WLBDAT
108010160929     C                   Z-ADD     G02DAT        VCCDSP
108011160929     C*
108012160929     C                   MOVEL     '3A'          KCOD
108013160929     C                   MOVEL     *BLANKS       KKEY
108014160929     C                   MOVEL     ARBCBO        KKEY
108015160929     C     KTAB1         CHAIN     TABEL00F                           99
108016160929     C   99              CLEAR                   DS3A
108017160929     C  N99              MOVEL     TBLUNI        DS3A
108018160929     C                   MOVEL     *BLANKS       WWW
108019160929     C                   MOVEA     §3ADES        WWW(4)
108020160929     C                   MOVEA     ARBCBO        WWW(1)
108021160929     C                   MOVEA     '-'           WWW(3)
108022160929     C                   MOVEA     WWW           VCDCBO
108023160929     C*
108024160929     C                   MOVEL     ARBRSD        VCCRSD
108025160929     C                   MOVEL     ARBIND        VCCIND
108026160929     C                   MOVEL     ARBLOD        VCCLOD
108027160929     C                   MOVEL     ARBPRD        VCCPRD
108028160929     C                   Z-ADD     ARBKSC        VCCKSC
108029160929     C                   Z-ADD     ARBPKF        VCCPKF
108030160929     C                   Z-ADD     ARBVLF        VCCVLF
108031160929     C                   MOVEL     ARBFVF        VCCFVF
108032160929     C*
108033160929     C                   MOVEL     *BLANKS       VCDTC1
108034160929     C     ARBTC1        IFNE      *BLANKS
108035160929     C                   MOVEL     '1P'          KCOD
108036160929     C                   MOVEL     *BLANKS       KKEY
108037160929     C                   MOVEL     ARBTC1        KKEY
108038160929     C     KTAB1         CHAIN     TABEL00F                           99
108039160929     C                   MOVEL     *BLANKS       WWW
108040160929     C  N99              MOVEA     TBLUNI        WWW(3)
108041160929     C                   MOVEL     ARBTC1        WWW(1)
108042160929     C                   MOVEL     '-'           WWW(2)
108043160929     C                   MOVEA     WWW           VCDTC1
108044160929     C                   ENDIF
108045160929     C                   MOVEL     *BLANKS       VCDTC2
108046160929     C     ARBTC2        IFNE      *BLANKS
108047160929     C                   MOVEL     '1P'          KCOD
108048160929     C                   MOVEL     *BLANKS       KKEY
108049160929     C                   MOVEL     ARBTC2        KKEY
108050160929     C     KTAB1         CHAIN     TABEL00F                           99
108051160929     C                   MOVEL     *BLANKS       WWW
108052160929     C  N99              MOVEA     TBLUNI        WWW(3)
108053160929     C                   MOVEL     ARBTC2        WWW(1)
108054160929     C                   MOVEL     '-'           WWW(2)
108055160929     C                   MOVEA     WWW           VCDTC2
108056160929     C                   END
108057160929     C* C/ASSEGNO
108058160929     C     KARB1         CHAIN     FiAR901L                           99
108059160929     C     *IN99         IFEQ      *OFF
108060160929     C                   MOVEL     AR9TIC        VCCTIC
108061160929     C                   Z-ADD     AR9CAS        VCCCAS
108062160929     C                   MOVEL     AR9VCA        VCCVCA
108063160929     C     VCCCAS        IFGT      *ZEROS
108064160929     C     VCCVCA        ANDEQ     *BLANKS
108065160929     C                   MOVEL     'ITL'         VCCVCA
108066160929     C                   ENDIF
108067160929     C                   ELSE
108068160929     C                   CLEAR                   VCCTIC
108069160929     C                   CLEAR                   VCCCAS
108070160929     C                   CLEAR                   VCCVCA
108071160929     C                   ENDIF
108072160929     C*
108073160929     C***                Z-ADD     ARBVMD        VCCVMD
108074160929     C***                MOVEL     ARBVAD        VCCVAD
108075160929     C***  VCCVMD        IFGT      *ZEROS
108076160929     C***  VCCVAD        ANDEQ     *BLANKS
108077160929     C***                MOVEL     'ITL'         VCCVAD
108078160929     C***                ENDIF
108079160929     C                   Z-ADD     ARBIAS        VCCIAS
108080160929     C                   MOVEL     ARBVAS        VCCVAS
108081160929     C     VCCIAS        IFGT      *ZEROS
108082160929     C     VCCVAS        ANDEQ     *BLANKS
108083160929     C                   MOVEL     'ITL'         VCCVAS
108084160929     C                   ENDIF
108085160929     C                   MOVEL     'TR'          KCOD
108086160929     C                   MOVEL     *BLANKS       KKEY
108087160929     C                   MOVEL     ARBCTR        UNOCTR            1
108088160929     C                   MOVEL     UNOCTR        KKEY
108089160929     C     KTAB1         CHAIN     TABEL00F                           99
108090160929     C                   MOVEL     TBLUNI        DSTR
108091160929     C                   MOVEL     *BLANKS       WWW
108092160929     C                   MOVEA     §TRDES        WWW(5)
108093160929     C                   MOVEL     ARBCTR        WCOM3
108094160929     C                   MOVEA     WCOM3         WWW(1)
108095160929     C                   MOVEA     '-'           WWW(4)
108096160929     C                   MOVEA     WWW           VCDCTR
108097160929     c                   select
108098160929     c                   when      arbffd='S'
108099160929     C                   MOVEL     'SI '         VCCFFD
108100160929     c                   when      arbffd='M'
108101160929     C                   MOVEL     'SAB'         VCCFFD
108102160929     C                   other
108103160929     C                   MOVEL     '   '         VCCFFD
108104160929     C                   ENDsl
108105160929     c* consegna richiesta
108106160929     c                   clear                   VCDCRICH
108107160929     c                   if        arbdcr>0 or arbhcr>0
108108160929     c                   clear                   fnlv80ds
108109160929     c                   eval      ilv80aas=arbaas
108110160929     c                   eval      ilv80lnp=arblnp
108111160929     c                   eval      ilv80nrs=arbnrs
108112160929     c                   eval      ilv80nsp=arbnsp
108113160929     c                   eval      ilv80mgs=arbmgs
108114160929     c                   eval      ilv80tc1=arbtc1
108115160929     c                   eval      ilv80tc2=arbtc2
108116160929     c                   eval      ilv80dcr=arbdcr
108117160929     c                   eval      ilv80hcr=arbhcr
108118160929     c                   eval      ilv80tcr=arbtcr
108119160929     c                   call      'FNLV80R'
108120160929     c                   parm                    kpjba
108121160929     c                   parm                    fnlv80ds
108122160929     c                   eval      VCDCRICH=%trim(olv80tcrd)
108123160929     c                   if        arbdcr>0
108124160929     c                   eval      VCDCRICH=%trim(VCDCRICH) + ' ' +
108125160929     c                             %editw(olv80dcr6:'  /  /  ')
108126160929     c                   endif
108127160929     c                   if        arbhcr>0
108128160929     c                   eval      VCDCRICH=%trim(VCDCRICH) + ' ' +
108129160929     c                             %editw(arbhcr:'  :  ')
108130160929     c                   endif
108131160929     c                   endif
108132160929     C*
108133160929     C* AGGANCIO LA TASSAZIONE PER GLI ALTRI DATI DI TESTATA
108134160929     c                   exsr      rietas
108135160929     C                   when      PARTBO='S'
108136160929     C* BOLLA IN SEDE
108137160929     c                   if        not %open(titas30c)
108138160929     C                   OPEN      TITAS30C
108139160930     C                   OPEN      tncsb03l
108140161006     C                   OPEN      fiar531c
108141160929     c                   endif
108142160929     C     Ktas          CHAIN     titas30c
108143160929     C*
108144160929     C                   if        not %found(titas30c)
108145160929     c                   goto      endrie
108146160929     c                   endif
108147160929     C*
108148160929     C                   movel     tasaas        G02INV
108149160929     C                   move      tasmgs        G02INV
108150160929     C                   Z-ADD     *ZEROS        G02DAT
108151160929     C                   MOVEL     '3'           G02ERR
108152160929     C                   CALL      'XSRDA8'
108153160929     C                   PARM                    WLBDAT
108154160929     C                   Z-ADD     G02DAT        VCCDSP
108155160929     C*
108156160929     C                   MOVEL     '3A'          KCOD
108157160929     C                   MOVEL     *BLANKS       KKEY
108158160929     C                   MOVEL     tasCBO        KKEY
108159160929     C     KTAB1         CHAIN     TABEL00F                           99
108160160929     C   99              CLEAR                   DS3A
108161160929     C  N99              MOVEL     TBLUNI        DS3A
108162160929     C                   MOVEL     *BLANKS       WWW
108163160929     C                   MOVEA     §3ADES        WWW(4)
108164160929     C                   MOVEA     tasCBO        WWW(1)
108165160929     C                   MOVEA     '-'           WWW(3)
108166160929     C                   MOVEA     WWW           VCDCBO
108167160929     C*
108168160929     C                   MOVEL     tasRSD        VCCRSD
108169160929     C                   MOVEL     tasIND        VCCIND
108170160929     C                   MOVEL     tasLOD        VCCLOD
108171160929     C                   MOVEL     tasPRD        VCCPRD
108172160929     C                   Z-ADD     tasKSC        VCCKSC
108173160929     C                   Z-ADD     tasPKF        VCCPKF
108174160929     C                   Z-ADD     tasVLF        VCCVLF
108175160929     C                   MOVEL     tasFVF        VCCFVF
108176160929     C*
108177160929     C                   MOVEL     *BLANKS       VCDTC1
108178160929     C     tasftc        IFNE      *BLANKS
108179160929     C                   MOVEL     '1P'          KCOD
108180160929     C                   MOVEL     *BLANKS       KKEY
108181160929     C                   MOVEL     tasftc        KKEY
108182160929     C     KTAB1         CHAIN     TABEL00F                           99
108183160929     C                   MOVEL     *BLANKS       WWW
108184160929     C  N99              MOVEA     TBLUNI        WWW(3)
108185160929     C                   MOVEL     tasftc        WWW(1)
108186160929     C                   MOVEL     '-'           WWW(2)
108187160929     C                   MOVEA     WWW           VCDTC1
108188160929     C                   ENDIF
108189160929     C                   MOVEL     *BLANKS       VCDTC2
108190160929     C     tasTC2        IFNE      *BLANKS
108191160929     C                   MOVEL     '1P'          KCOD
108192160929     C                   MOVEL     *BLANKS       KKEY
108193160929     C                   MOVEL     tasTC2        KKEY
108194160929     C     KTAB1         CHAIN     TABEL00F                           99
108195160929     C                   MOVEL     *BLANKS       WWW
108196160929     C  N99              MOVEA     TBLUNI        WWW(3)
108197160929     C                   MOVEL     tasTC2        WWW(1)
108198160929     C                   MOVEL     '-'           WWW(2)
108199160929     C                   MOVEA     WWW           VCDTC2
108200160929     C                   END
108201160929     C* C/ASSEGNO
108202160930     C     karb1         CHAIN     tncsb03l                           99
108203160929     C     *IN99         IFEQ      *OFF
108204160929     C                   Z-ADD     csbCAS        VCCCAS
108205160929     C                   MOVEL     csbVCA        VCCVCA
108206160929     C     VCCCAS        IFGT      *ZEROS
108207160929     C     VCCVCA        ANDEQ     *BLANKS
108208160929     C                   MOVEL     'ITL'         VCCVCA
108209160929     C                   ENDIF
108210160929     C                   ELSE
108211160929     C                   CLEAR                   VCCTIC
108212160929     C                   CLEAR                   VCCCAS
108213160929     C                   CLEAR                   VCCVCA
108214160929     C                   ENDIF
108215160929     C*
108221160929     C                   Z-ADD     tasIAS        VCCIAS
108222160929     C                   MOVEL     tasVAS        VCCVAS
108223160929     C     VCCIAS        IFGT      *ZEROS
108224160929     C     VCCVAS        ANDEQ     *BLANKS
108225160929     C                   MOVEL     'ITL'         VCCVAS
108226160929     C                   ENDIF
108227160929     C                   MOVEL     'TR'          KCOD
108228160929     C                   MOVEL     *BLANKS       KKEY
108229160929     C                   MOVEL     tasCTR        UNOCTR            1
108230160929     C                   MOVEL     UNOCTR        KKEY
108231160929     C     KTAB1         CHAIN     TABEL00F                           99
108232160929     C                   MOVEL     TBLUNI        DSTR
108233160929     C                   MOVEL     *BLANKS       WWW
108234160929     C                   MOVEA     §TRDES        WWW(5)
108235160929     C                   MOVEL     tasCTR        WCOM3
108236160929     C                   MOVEA     WCOM3         WWW(1)
108237160929     C                   MOVEA     '-'           WWW(4)
108238160929     C                   MOVEA     WWW           VCDCTR
108239160929     c                   select
108240160929     c                   when      tasffd='S'
108241160929     C                   MOVEL     'SI '         VCCFFD
108242160929     c                   when      tasffd='M'
108243160929     C                   MOVEL     'SAB'         VCCFFD
108244160929     C                   other
108245160929     C                   MOVEL     '   '         VCCFFD
108246160929     C                   ENDsl
108247160929     c* consegna richiesta
108248160929     c                   clear                   VCDCRICH
108249160929     c                   if        tasdcr>0 or tashcr>0
108250160929     c                   clear                   fnlv80ds
108251160929     c                   eval      ilv80aas=tasaas
108252160929     c                   eval      ilv80lnp=taslnp
108253160929     c                   eval      ilv80nrs=tasnrs
108254160929     c                   eval      ilv80nsp=tasnsp
108255160929     c                   eval      ilv80mgs=tasmgs
108256160930     c                   eval      ilv80tc1=tasftc
108257160929     c                   eval      ilv80tc2=tastc2
108258160929     c                   eval      ilv80dcr=tasdcr
108259160929     c                   eval      ilv80hcr=tashcr
108260160929     c                   eval      ilv80tcr=tastcr
108261160929     c                   call      'FNLV80R'
108262160929     c                   parm                    kpjba
108263160929     c                   parm                    fnlv80ds
108264160929     c                   eval      VCDCRICH=%trim(olv80tcrd)
108265160929     c                   if        tasdcr>0
108266160929     c                   eval      VCDCRICH=%trim(VCDCRICH) + ' ' +
108267160929     c                             %editw(olv80dcr6:'  /  /  ')
108268160929     c                   endif
108269160929     c                   if        tashcr>0
108270160929     c                   eval      VCDCRICH=%trim(VCDCRICH) + ' ' +
108271160929     c                             %editw(tashcr:'  :  ')
108272160929     c                   endif
108273160929     c                   endif
108274160930     c* Tassazione
108275160930     C                   CLEAR                   VCCPOR
108276160930     C                   CLEAR                   VCCIMV
108277160930     C                   CLEAR                   VCCNFT
108278160930     C                   CLEAR                   VCCIFT
108279160930     C                   CLEAR                   VCCDFT
108280160930     C                   CLEAR                   VCCSV1
108281160930     C                   CLEAR                   VCCVA1
108282160930     C                   CLEAR                   VCCSV2
108283160930     C                   CLEAR                   VCCVA2
108284160930     C                   CLEAR                   VCCSV3
108285160930     C                   CLEAR                   VCCVA3
108286160930
108287160930     C                   MOVEL     'CV'          KCOD
108288160930     C                   MOVEL(P)  tasdiv        KKEY
108289160930     C     KTAB1         CHAIN     TABEL                              31
108290160930     C  N31              MOVEL     TBLUNI        DSCV
108291160930     C   31              clear                   DSCV
108292160930     c* porto
108293160930     c                   z-add     taspor        w0133
108294160930     c                   movel     §cvfdc        wtstdec
108295160930     c                   exsr      edtimporti
108296160930     c                   move      impeditato    vccpor
108297160930     c* totale imponibile
108298160930     C                   z-add     tasIMV        W0122
108299160930     C                   movel     §cvfdc        wtstdec
108300160930     C                   exsr      edtimport122
108301160930     C                   move      impeditato    vccimv
108302160930     c*
108303160930     C                   Z-ADD     tasNFT        VCCNFT
108305160930     C                   Z-ADD     tasDFT        G02INV
108306160930     C                   Z-ADD     *ZEROS        G02DAT
108307160930     C                   MOVEL     '3'           G02ERR
108308160930     C                   CALL      'XSRDA8'
108309160930     C                   PARM                    WLBDAT
108310160930     C                   Z-ADD     G02DAT        VCCDFT
108311160930     c* varia 1
108312160930     C                   MOVEL     tasSV1        VCCSV1
108313160930     c                   z-add     tasva1        w0133
108314160930     c                   movel     §cvfdc        wtstdec
108315160930     c                   exsr      edtimporti
108316160930     c                   move      impeditato    vccva1
108317160930     c* varia 2
108318160930     C                   MOVEL     tasSV2        VCCSV2
108319160930     c                   z-add     tasva2        w0133
108320160930     c                   movel     §cvfdc        wtstdec
108321160930     c                   exsr      edtimporti
108322160930     c                   move      impeditato    vccva2
108323160930     c* varia 3
108324160930     C                   MOVEL     tasSV3        VCCSV3
108325160930     c                   z-add     tasva3        w0133
108326160930     c                   movel     §cvfdc        wtstdec
108327160930     c                   exsr      edtimporti
108328160930     c                   move      impeditato    vccva3
108329160929     C*
108330160929     C*
108331160929    1C                   ENDSL
135500941222     C*
135600941222     C                   SETOFF                                       99
135700060306     c                   eval      intesta = is(1)
135800940714     C*
135900941222     C     ENDRIE        ENDSR
136000991111      **********************************************************************
136100991111      * riempio campi del video relativi la tassazione
136200991111      **********************************************************************
136300991111     C     RIETAS        BEGSR
136400991111     C                   MOVEL     '1'           WTRC
136500991111     C     KARB2         CHAIN     FIAR601L                           99
136600991111     C*
136700991111     C     *IN99         IFEQ      *OFF
136800991111     c* divisa
136900160122     C****               MOVEL     AR6DIV        VCCDIV
137000991111     C                   MOVEL     'CV'          KCOD
137100991111     C                   MOVEL(P)  AR6div        KKEY
137200991111     C     KTAB1         CHAIN     TABEL                              31
137300991111     C  N31              MOVEL     TBLUNI        DSCV
137400991111     C   31              clear                   DSCV
137500991111     c* porto
137600991111     c                   z-add     ar6por        w0133
137700991111     c                   movel     §cvfdc        wtstdec
137800991111     c                   exsr      edtimporti
137900991111     c                   move      impeditato    vccpor
138000160122     c* totale imponibile
138100160125     C                   z-add     ar6IMV        W0122
138200160122     C                   movel     §cvfdc        wtstdec
138300160125     C                   exsr      edtimport122
138400160122     C                   move      impeditato    vccimv
138500991111     c*
138600991111     C                   Z-ADD     AR6NFT        VCCNFT
138700991111     C                   Z-ADD     AR6IFT        VCCIFT
138800991111     C                   Z-ADD     AR6DFT        G02INV
138900991111     C                   Z-ADD     *ZEROS        G02DAT
139000991111     C                   MOVEL     '3'           G02ERR
139100991111     C                   CALL      'XSRDA8'
139200991111     C                   PARM                    WLBDAT
139300991111     C                   Z-ADD     G02DAT        VCCDFT
139400991111     c* varia 1
139500991111     C                   MOVEL     AR6SV1        VCCSV1
139600991111     c                   z-add     ar6va1        w0133
139700991111     c                   movel     §cvfdc        wtstdec
139800991111     c                   exsr      edtimporti
139900991111     c                   move      impeditato    vccva1
140000991111     c* varia 2
140100991111     C                   MOVEL     AR6SV2        VCCSV2
140200991111     c                   z-add     ar6va2        w0133
140300991111     c                   movel     §cvfdc        wtstdec
140400991111     c                   exsr      edtimporti
140500991111     c                   move      impeditato    vccva2
140600991111     c* varia 3
140700991111     C                   MOVEL     AR6SV3        VCCSV3
140800991111     c                   z-add     ar6va3        w0133
140900991111     c                   movel     §cvfdc        wtstdec
141000991111     c                   exsr      edtimporti
141100991111     c                   move      impeditato    vccva3
141200991111     C                   ELSE
141300160122     C****               CLEAR                   VCCDIV
141400991111     C                   CLEAR                   VCCPOR
141500160122     C                   CLEAR                   VCCIMV
141600991111     C                   CLEAR                   VCCNFT
141700991111     C                   CLEAR                   VCCIFT
141800991111     C                   CLEAR                   VCCDFT
141900991111     C                   CLEAR                   VCCSV1
142000991111     C                   CLEAR                   VCCVA1
142100991111     C                   CLEAR                   VCCSV2
142200991111     C                   CLEAR                   VCCVA2
142300991111     C                   CLEAR                   VCCSV3
142400991111     C                   CLEAR                   VCCVA3
142500991111     C                   ENDIF
142600991111     C*
142700991111     C                   ENDSR
142800991111      **********************************************************************
142900991111      * EDITO IMPORTI PER CAMPI in stampa
143000991111      **********************************************************************
143100991111     C     EDTIMPORTI    BEGSR
143200991111
143300991111      * ------------------------------------------------------------------------
143400991111      * W0133      contiene il numero da editare
143500991111      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il num
143600991111      *            ero ha decimali
143700991111      * IMPEDITATO ritorna il valore editato per la stampa lungo13 (troncate
143800991111      *            cifre oltre 10 mld)
143900991111      * ------------------------------------------------------------------------
144000991111
144100991111     c                   clear                   IMPeditato
144200991111     c                   clear                   W014A
144300991111     c                   if        Wtstdec = 'S'
144400991111     C                   eval      W014a = %EDITC(W0133:'4')
144500991111     c                   else
144600991111     c                   z-add     W0133         W0140
144700991111     C                   eval      W014a = %EDITC(W0140:'4')
144800991111     c                   endif
144900991111     c
145000991111     c                   move      W014a         IMPeditato
145100991111     c
145200991111     c                   ENDSR
145300991111     C*
145400991111      **********************************************************************
145500991111      * EDITO IMPORTI PER CAMPI a video
145600991111      **********************************************************************
145700991111     C     EDTimport122  BEGSR
145800991111
145900991111     c                   clear                   IMPeditato
146000991111     c                   clear                   W013A
146100991111     c                   if        Wtstdec = 'S'
146200991111     C                   eval      W013a = %EDITC(W0122:'4')
146300991111     c                   else
146400991111     c                   z-add     W0122         W0130
146500991111     C                   eval      W013a = %EDITC(W0130:'4')
146600991111     c                   endif
146700991111     c
146800991111     c                   move      W013a         IMPeditato
146900991111     c
147000991111     c                   ENDSR
147100060306     C**************************************************************************
147200060306     C* Impostazione campo per visualizzazione motivo variazione
147300060306     C**************************************************************************
147400060306     C     ges_mtv       BEGSR
147500060306     c                   seton                                        01
147600060306     c                   clear                   vscvmv
147700060307     c                   clear                   vshvmv
147800060307     c                   clear                   vsdvmv
147900060306     c                   eval      wtrd = '§' + arbcvb
147901161006     c                   if        partbo<>'S'
148000060307     c     kar5          chain     fiar501l
148001161006     c                   else
148002161006     c     kar5s         chain     fiar531c
148003161006     c                   endif
148100161006     c                   if        %found
148200060306     c                   setoff                                       01
148300060307     c                   movel     ar5uni        dar5mtv
148400060307     c                   clear                   dmtv
148500060307     C                   CLEAR                   tibs02ds
148600060307     C                   MOVEL     'C'           T02MOD
148700060307     C                   MOVEL     KNSIF         T02SIF
148800060307     C                   MOVEL     'MTV'         T02COD
148900060307     c                   movel(P)  §ar5mtv       t02ke1
149000060307     c                   call      'TIBS02R'
149100060307     C                   PARM                    KPJBA
149200060307     C                   PARM                    tibs02ds
149300060307    2C                   IF        T02ERR = *BLANKS
149400060307     C                   MOVEL     T02uni        dmtv
149500060307    2C                   ENDIF
149600060307     c                   movel     §mtvdes       vsdvmv
149700060307     c                   movel     §mtvdes       vshvmv
149800060306     c                   eval      wesmtv = '1'
149900060306     c                   endif
150000991111     C*
150100060306     c                   endsr
150200060306     C**************************************************************************
150300060306     C* Visualizzazione motivo variazione
150400060306     C**************************************************************************
150500060306     C     vis_mtv       BEGSR
150600060310     c                   setoff                                       01
150700060306     c                   readc     lr75s01
150800060306     c                   dow       %eof = *off
150900060306     c                   if        vscvmv = 'V'
151000060306     c*
151100060306     c                   clear                   vsctitw
151200060306     c                   eval      vsctitw = 'Variazione del ' +
151300060306     c                             %editc(vscdtv:'8') +  ' ore ' +
151400060306     c                             %editw(vscorv : '  :  ') + ' utente ' +
151500060307     c                             vscpru
151600060306     c*
151700060307     c                   eval      wtrd = '§' + %subst(vsdcvb:1:2)
151701161006     c                   if        partbo<>'S'
151800060307     c     kar5          chain     fiar501l
151801161006     c                   else
151802161006     c     kar5s         chain     fiar531c
151803161006     c                   endif
151900161006     c                   if        %found
152000060306     c                   movel     ar5uni        dar5mtv
152100060306     c                   movel     §ar5mtv       vscmtv
152200060306     c* decodifico motivo variazione
152300060307     c                   eval      vsdmtv = vshvmv
152400060306     c                   movel     §ar5txt       vscnt1
152500060306     c                   move      §ar5txt       vscnt2
152600060306     c                   exfmt     lr75wd1
152700060307     c                   endif
152800060306     c                   clear                   vscvmv
152900060306     C                   Z-ADD     NRR1          REC1
153000060306     C                   UPDATE    Lr75S01
153100060306     c                   endif
153200060306     c                   readc     lr75s01
153300060306     c                   enddo
153400060306     C*
153500060306     c                   endsr
153600940714     C**************************************************************************
153700940714     C*  ROUTINE INIZIALE
153800940714     C**************************************************************************
153900940714     C     *INZSR        BEGSR
154000940714     C*
154100940714     C     KARB1         KLIST
154200940714     C                   KFLD                    VCCAAS
154300940714     C                   KFLD                    VCCLNP
154400940714     C                   KFLD                    VCCNRS
154500940714     C                   KFLD                    VCCNSP
154600991110     C     KARB2         KLIST
154700991110     C                   KFLD                    VCCAAS
154800991110     C                   KFLD                    VCCLNP
154900991110     C                   KFLD                    VCCNRS
155000991110     C                   KFLD                    VCCNSP
155100991110     C                   KFLD                    WTRC
155200991112     C     KARBU         KLIST
155300991112     C                   KFLD                    arbAAS
155400991112     C                   KFLD                    arbLNP
155500991112     C                   KFLD                    arbNRS
155600991112     C                   KFLD                    arbNSP
155700991112     C                   KFLD                    WTRC
155800991112     c                   kfld                    arbdtv
155900991112     c                   kfld                    arborv
156000060307     C     KAR5          KLIST
156100060307     C                   KFLD                    arbAAS
156200060307     C                   KFLD                    arbLNP
156300060307     C                   KFLD                    arbNRS
156400060307     C                   KFLD                    arbNSP
156500060307     C                   KFLD                    WTRd
156600060307     c                   kfld                    vshdtv
156700060307     c                   kfld                    vshorv
156701161006     C     KAR5s         KLIST
156702161006     C                   KFLD                    arbAAS
156703161006     C                   KFLD                    arbLNP
156704161006     C                   KFLD                    arbNRS
156705161006     C                   KFLD                    arbNSP
156706161006     C                   KFLD                    WTRd
156707161006     C                   KFLD                    Wprg
156708161006     c                   kfld                    vshdtv
156709161006     c                   kfld                    vshorv
156800940714     C     KTAB1         KLIST
156900940714     C                   KFLD                    KKUT
157000940714     C                   KFLD                    KCOD
157100940714     C                   KFLD                    KKEY
157101160929     C     Ktas          KLIST
157102160929     C                   KFLD                    VCCAAS
157103160929     C                   KFLD                    VCCLNP
157104160929     C                   KFLD                    VCCNRS
157105160929     C                   KFLD                    VCCNSP
157106160929     C                   KFLD                    partbl
157200991112     c*
157300940714     C*
157400991111     C     *LIKE         DEFINE    ARbCVB        VSCCVB
157500940714     C     *LIKE         DEFINE    TBLKUT        KKUT
157600940714     C     *LIKE         DEFINE    TBLCOD        KCOD
157700940714     C     *LIKE         DEFINE    TBLKEY        KKEY
157800991110     C     *LIKE         DEFINE    AR6TRC        WTRC
157900060306     C     *LIKE         DEFINE    AR5TRd        WTRd
158000940715     C                   MOVEL     *BLANKS       WCOM3             3
158100940715     C                   MOVEL     *BLANKS       WCOM4             4
158200940715     C                   MOVEL     *BLANKS       WCOM5             5
158300940715     C                   MOVEL     *BLANKS       WCOM6             6
158400940715     C                   MOVEL     *BLANKS       WCOM7             7
158500940715     C                   MOVEL     *BLANKS       WCOM8             8
158600940715     C                   MOVEL     *BLANKS       WCOM9             9
158700991111     C                   MOVEL     *BLANKS       WCOM10           10
158800991112     C                   MOVEL     *BLANKS       WCOM11           11
158900991111     C                   MOVEL     *BLANKS       WCOM12           12
159000940715     C                   MOVEL     *BLANKS       WCOM13           13
159100940715     C                   MOVEL     *BLANKS       WCOM16           16
159200940715     C*
159300940714     C                   Z-ADD     1             KKUT
159400991112     C* CARICO DA SIGLE VARIE
159500991112     C                   MOVEL     '$2'          KCOD
159600991112     C                   MOVEL(p)  '1'           KKEY
159700991112     C     KTAB1         CHAIN     TABEL                              31
159800991112     C  N31              MOVEL     TBLUNI        DS$2
159900991112     C   31              MOVEL     *BLANKS       DS$2
160000071026     c
160100071026      * Aggancio tabella "VPODECO" per reperimento data attivazione
160200071026      * gestione prepagati
160300120510     C****               CLEAR                   tibs02ds
160400120510     c****               clear                   dvpodeco
160500120510     C****               MOVEL     'C'           T02MOD
160600120510     C****               MOVEL     KNSIF         T02SIF
160700120510     C****               MOVEL     'VPO'         T02COD
160800120510     c***                movel(P)  'DECO'        t02ke1
160900120510     C****               CALL      'TIBS02R'
161000120510     C****               PARM                    KPJBA
161100120510     C****               PARM                    tibs02ds
161200120510    2C****               IF        T02ERR = *BLANKS
161300120510     C****               MOVEL     T02uni        dvpodeco
161400120510    2C***                ENDIF
161500071026     C*
161600940714     C*
161700940714     C                   ENDSR
161800940714      **************************************************************************
161900940715      *  1         2         3         4         5         6         7         8
162000940715123456*8901234567890123456789012345678901234567890123456789012345678901234567890
162100941231** ?----  BD  -----------------------------------------------------------------?
162200941212Destinatario: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
162300941212Xxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxx
162400941212Xxxxxxx  xx  xxx   Inoltro: x    F.Deposito: xx    Partita IVA: xxxxxxxxxxxxxxxx
162500940715** ?----  BP  -----------------------------------------------------------------?
162600941212Peso  Kg.: xxxxxx,x                    Volume da Fatturare:  x  xx,xxx
162700941212** ?----  VP  -----------------------------------------------------------------?
162800941212Volume da Fatturare:  x  xx,xxx
162900940715** ?----  BK  -----------------------------------------------------------------?
163000941231Cod.bolla precedente:  xx-xxxxxxxxxxxxxxxxxxxxxxxxx           Cod.bolla nuovo:
163100941231XX-xxxxxxxxxxxxxxxxxxxxxxxxx   Contrassegno precedente:  xx  xxxxxxxxxx,xxx  xxx
163200941214Particolarita' C/Assegno: xx
163300940715** ?----  BC  -----------------------------------------------------------------?
163400941214Codice   Bolla      :  xx-xxxxxxxxxxxxxxxxxxxxxxxxx
163500941221Contrassegno precedente: xx  XXXXXXXXXX,XXX  XXX   Particolarita' C/Assegno: xx
163600940715** ?----  BT  -----------------------------------------------------------------?
163700941215Cli xxxxxxx Tar xxx VMercexxxxxxxxxx,xxx xxx IAsxxxxxxxxxx,xxx xxx Qta'Fatxxxxxx
163800991110xxxx,xxx Divxxx Traspxxxxxxxxxx Varie:xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxx
163900991110xxxxx xxxxxxxxxxx Imp123456789012 Esenz.X Fatt:tot.123456789012 num.xxxxxx xx/xx
164000940715** ?----  T2  -----------------------------------------------------------------?
164100040202Cod.Cliente: xxxxxxx  Cod.Tariffa: xxx-xxxxxxxxxxxxxxxxxxxx  Div: xxx
164200041130Varie: xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
164300040203Importo fat.: xxxxxxxxxxxx  Es.IVA: x-xxxxx  Fat.n.: XXX  xxxxxx del xx/xx/xx
164400940715** ?----  CR  -----------------------------------------------------------------?
164500051128Consegna richiesta: xxxxxxxxxxx xx/xx/xxxx  xx:xx    Giorni di Chiusura: xx  xx
164600941213      Consegna Particolare 1: x x xxxxxxxxxxxxxxxxxxxxxxxxx
164700941213      Consegna Particolare 2: x x xxxxxxxxxxxxxxxxxxxxxxxxx
164800941231** ?----  BM  -----------------------------------------------------------------?
164900161024Mittente: xxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
165000941231xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
165100941231xxx   xx  xxx   Anteporto: x     Partita IVA: xxxxxxxxxxxxxxxx
165200060306** ?----  IS  -----------------------------------------------------------------?
165300161228 Data        Ora    Utente         Causale
165400161228 Data        Ora    Utente         Causale                    Motivo          V
165500060316** ?----  RM  -----------------------------------------------------------------?
165600060316Rif.Mitt.Alfabetico: xxxxxxxxxxxxxxx   Natura Merce: xxxxxxxxxxxxxxx
165700061102** ?----  FI  -----------------------------------------------------------------?
165800071026XXXXXXXXXXXXXXXXXXXXXXX: Codice Fiscale  xxxxxxxxxxxxxxxx
165900071026                            Partita IVA  xxxxxxxxxxxxxxxx
166000130927** ?----  REF - fnarbn0f ------------------------------------------------------?
166100130927Referente Consegna....: xxxxxxxxxxxxxxxxxxxxxxxxx
166200130927Telefono destinatario : xxxxxxxxxxxxxxxx
166300130927** ?----  EML - fnarbn0f ------------------------------------------------------?
166400130927E-mail destinatario...: xxxxxxxxxxxxxxxxxxxxxxxxx
166500130927** ?----  SMS - fnarbn0f ------------------------------------------------------?
166600130927Cellulare destinatario: xxxxxxxxxxxxxxxxxxxxxxxxx
166700130930** ?----  NOT - fnarbn0f ------------------------------------------------------?
166800130930Note bolla:
166801160930** ?----  BKT - fnarbk0f ------------------------------------------------------?
166900160930Tipo bolla precedente: xx    Tipo Bolla nuovo: xx
167000161024** ?----  BTI - ---------------------------------------------------------------?
167100161024Importo da Assicurare: xxxxxxxxxx,xxx xxx
