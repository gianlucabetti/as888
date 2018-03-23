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
001100991110     FFIAR601L  IF   E           K DISK
001200051201     FFiAR901L  IF   E           K DISK
001300060306     FFiAR501L  IF   E           K DISK
001400941222     F*
001500941222     FFNBLP01L  IF   E           K DISK    USROPN
001600991110     FFIARBF1C  IF   E           K DISK
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
003000071026     D FI              S              1    DIM(160) CTDATA PERRCD(80)
003100911024     D*
003200941214     D SKI             S              1    DIM(15)
003300940715     D WWW             S              1    DIM(30)
003400991111     D
003500991111     D Wtstdec         S                   LIKE(§CVfdc)
003600991111     D W014A           s             14
003700991111     D W013A           s             13
003800060316     D W017A           s             17
003900991111     D IMPeditato      s             13
004000991111     D w0140           s             14  0
004100991111     D w0130           s             13  0
004200991111     D w0133           s             13  3
004300991111     D w0122           s             12  2
004400991112     D wvaria          s              1  0
004500991112     D wvaria5         s             13  3
004600991112     D wsvn            s                   like(arbsv1)
004700040202     d wesiva          s              5
004800940714      *------------------------------------------------------------------------*
004900940714      * DS PER I PARAMETRI DI INGRESSO/USCITA
005000920402     D PARAM           DS
005100940714     D  VCCAAS                14     17  0
005200940714     D  VCCLNP                18     20  0
005300940714     D  VCCNRS                21     22  0
005400940714     D  VCCNSP                23     29  0
005500940714     D  PARF03                30     30
005600940714     D  PARFLG                31     31
005700940714     D  PARPGM                32     41
005800940714     D  VCCDSA                42     61
005900950126     D  PARGIA               144    144
006000950126     D  PARFEL               145    147  0
006100950629     D  PARTBO               148    148
006200941222      * DS PER LA ROUTINE DI CONTROLLO DELLA DATA (8 CIFRE)
006300940714     D WLBDAT          DS                  INZ
006400940714     D  G02DAT                 1      8  0
006500940714     D  G02INV                 9     16  0
006600940714     D  G02ERR                17     17
006700940714     D  G02TGI                18     22  0
006800940714      * DS PER COMPORRE LA DATA SPEDIZIONE DI FNARB00F
006900940714     D                 DS
007000940714     D  ARBDSP                 1      8  0
007100940714     D  ARBAAS                 1      4  0
007200940714     D  ARBMGS                 5      8  0
007300941222     D                 DS
007400941222     D  BLPDSP                 1      8  0
007500941222     D  BLPAAS                 1      4  0
007600941222     D  BLPMGS                 5      8  0
007700940714     D                 DS
007800940714     D  AA                     1      2  0
007900940714     D  GG                     5      6  0
008000940714     D  DATA                   1      6  0
008100940715     D DS1A          E DS
008200940715     D DS1P          E DS
008300940715     D DS3A          E DS
008400940715     D DSEI          E DS
008500940715     D DSTR          E DS
008600991111     D DSCV          E DS
008700991112     D DS$2          E DS
008800060306     D Dmtv          E DS
008900060306     D Ds7l          E DS
009000911024     D CNCR80        E DS
009100940715     D KPJBA         E DS
009200051128     D FNLV80DS      E DS
009300060306     D tibs02ds      E DS
009400060306     D dar5mtv       E DS
009500120510     d*dVpodeco      e ds
009600061102     d
009700061102     D Cdest           C                   CONST('D E S T I N A T A R I O')
009800061102     D CMITT           C                   CONST(' M I T T E N T E ......')
009900940714      *------------------------------------------------------------------------*
010000940714     IFNARBG00      97
010100940714     IFNARBD00      96
010200940714     IFNARBK00      95
010300991110     IFIARBT00      94
010400941231     IFNARBV00      92
010500941231     IFNARBM00      91
010600940714     C**************************************************************************
010700940714     C*          M A I N       L I N E
010800940714     C**************************************************************************
010900940714     C*
011000940714     C     *ENTRY        PLIST
011100940714     C                   PARM                    KPJBA
011200940714     C                   MOVEL     KPJBU         PARAM
011300950107     C                   CLEAR                   PARF03
011400940714     C*
011500940714     C* PARFLG = 'C' CHIUDO CON LR SENZA VISUALIZZARE
011600940714     C     PARFLG        IFEQ      'C'
011700930524     C                   SETON                                        LR
011800930524     C                   ELSE
011900940714     C*
012000940714     C                   EXSR      RIEC01
012100941222     C   99              GOTO      FINE
012200930524     C*
012300920402     C* PULIZIA SUBFILE
012400920402     C                   SETON                                        89
012500940714     C                   WRITE     LR75C01
012600920402     C                   SETOFF                                       89
012700060306     c                   clear                   wesmtv            1
012800920429     C*
012900940715     C* DECODIFICHE
013000920429     C*
013100940714     C                   Z-ADD     1             NRR1              5 0
013200991110     C                   SETOFF                                       9594
013300941231     C                   SETOFF                                       969792
013400941231     C                   SETOFF                                       91
013500991111     C     KARB1         SETLL     FIARBF1C
013600991111     C     KARB1         READE     FIARBF1C                               31
013700920402     C*
013800930524    1C     *IN31         DOWEQ     '0'
013900991111     C                   MOVEL     ARBPRU        VSCPRU
014000941231     C   92              MOVEL     *BLANKS       VSCPRU
014100940714     C                   MOVEL     *BLANKS       VSCCAM
014200940715     C*
014300940715     C*?--------- F N A R B D 0 0    ( 96 ) -----------------------------------?
014400920403     C*** VARIAZ DESTINATARIO
014500061102     c   96              select
014600061102    2C     ARBCVB        wheneq    'I0'
014700941212     C                   MOVEA     ARBRSD        BD(15)
014800941212     C                   MOVEA     ARBRD2        BD(51)
014900941212     C                   MOVEA     ARBIND        BD(87)
015000941212     C                   MOVEA     ARBCAD        BD(123)
015100941212     C                   MOVEA     ARBLOD        BD(133)
015200941212     C                   MOVEA     ARBPRD        BD(170)
015300941212     C                   MOVEA     ARBNZD        BD(174)
015400941212     C                   MOVEA     ARBFIN        BD(189)
015500051104     c                   select
015600051104     c                   when      arbffd='S'
015700051104     C                   MOVEA     'SI '         BD(206)
015800051104     c                   when      arbffd='M'
015900051104     C                   MOVEA     'SAB'         BD(206)
016000051104     C                   other
016100051104     C                   MOVEA     '   '         BD(206)
016200051104    3C                   ENDsl
016300941212     C                   MOVEA     ARBCPI        BD(225)
016400940714     C                   MOVEA     BD            VSCCAM
016500940715     C*------------------------------
016600060316     c* Rif. mitt. alfa e Natura merce
016700061102    2c     arbcvb        wheneq    'RM'
016800060316     C                   MOVEA     arbrsd        RM(22)
016900060316     C                   MOVEA     arbrd2        RM(54)
017000060316     c                   eval      w017A =  '   Natura Merce: '
017100060316     c                   movea     w017a         RM(37)
017200060316     C                   MOVEL     *BLANKS       VSCCAM
017300060316     C                   MOVEA     RM            VSCCAM
017400061102     C*------------------------------
017500071026     c* Codice fiscale / Partiva iva
017600061102    2c     arbcvb        wheneq    'FI'
017700061102     c                   if        arbrd2='D'
017800071026     c                   movea     cdest         FI(1)
017900061102     c                   else
018000071026     c                   movea     cmitt         FI(1)
018100061102     c                   endif
018200071026     c                   movea     arbcpi        FI(42)
018300071026     c* Se trascorsa la data attivazione, visualizzo anche partita iva
018400120510     c*****              if        arbdtV>§vpopiv
018500071026     c                   movel     arbrsd        w016a            16
018600071026     c                   movea     w016a         FI(122)
018700071026     C                   MOVEA     FI            VSCCAM
018800120510     c****               else
018900120510     c***                movea     fi            vsccam
019000120510     c***                eval      %subst(vsccam:106:50)=*blanks
019100120510     c****               endif
019200071026     c
019300061102     C*------------------------------
019400061102   x2c                   other
019500941216     C                   MOVEL     ARBCVB        UNOCVB            1
019600940715     C*
019700941216    3C     UNOCVB        IFEQ      'I'
019800940715     C*------------------------------
019900920402     C* PESO
020000941212     C                   MOVEL     ARBPKF        WCOM7
020100941214     C                   MOVEA     WCOM7         SKI(9)
020200940715     C                   Z-ADD     9             I                 2 0
020300941214     C                   EXSR      ABBL
020400920403     C*
020500941214    4C     SKI(14)       IFEQ      ' '
020600940715     C                   MOVEL     '0'           SKI(14)
020700940715    4C                   ENDIF
020800920403     C*
020900941212     C                   MOVEA     SKI(9)        BP(12)
021000941212     C                   MOVEA     ','           BP(18)
021100941212     C                   MOVEA     SKI(15)       BP(19)
021200940715     C*------------------------------
021300920402     C* VOLUME
021400941212     C                   MOVEL     ARBVLF        WCOM5
021500940715     C                   MOVEA     WCOM5         SKI(11)
021600940715    4C     SKI(11)       IFEQ      '0'
021700940715     C                   MOVEL     ' '           SKI(11)
021800940715    4C                   ENDIF
021900920403     C*
022000941212     C                   MOVEA     ARBFVF        BP(62)
022100941212     C                   MOVEA     SKI(11)       BP(65)
022200941212     C                   MOVEA     ','           BP(67)
022300941212     C                   MOVEA     SKI(13)       BP(68)
022400940715     C                   MOVEL     *BLANKS       VSCCAM
022500940714     C                   MOVEA     BP            VSCCAM
022600940715     C*------------------------------
022700930524   X3C                   ELSE
022800940715     C*------------------------------
022900940715     C* VARIAZIONE "VA", "VP" E "VT" DEL VOLUME
023000941212     C                   MOVEL     ARBVLF        WCOM5
023100940715     C                   MOVEA     WCOM5         SKI(11)
023200940715    4C     SKI(11)       IFEQ      '0'
023300940715     C                   MOVEL     ' '           SKI(11)
023400940715    4C                   ENDIF
023500930524     C*
023600941212     C                   MOVEA     ARBFVF        VP(23)
023700941212     C                   MOVEA     SKI(11)       VP(26)
023800941212     C                   MOVEA     ','           VP(28)
023900941212     C                   MOVEA     SKI(13)       VP(29)
024000940715     C                   MOVEL     *BLANKS       VSCCAM
024100941212     C                   MOVEA     VP            VSCCAM
024200940715    3C                   ENDIF
024300061102    2C                   ENDSL
024400940715     C*
024500941231     C*?--------- F N A R B V 0 0    ( 92 ) -----------------------------------?
024600941231     C*** VARIAZ VOLUME CML
024700941231    2C   92              DO
024800941231     C* VARIAZIONE "VA", "VP" E "VT" DEL VOLUME
024900941231     C                   MOVEL     ARBVLC        WCOM5
025000941231     C                   MOVEA     WCOM5         SKI(11)
025100941231    3C     SKI(11)       IFEQ      '0'
025200941231     C                   MOVEL     ' '           SKI(11)
025300941231    3C                   ENDIF
025400941231     C*
025500941231     C                   MOVEA     ARBFVC        VP(23)
025600941231     C                   MOVEA     SKI(11)       VP(26)
025700941231     C                   MOVEA     ','           VP(28)
025800941231     C                   MOVEA     SKI(13)       VP(29)
025900941231     C                   MOVEL     *BLANKS       VSCCAM
026000941231     C                   MOVEA     VP            VSCCAM
026100941231    2C                   ENDDO
026200940715     C*?--------- F N A R B K 0 0    ( 95 ) -----------------------------------?
026300920403     C*** VARIAZ COD BOLLA
026400940715     C   95              DO
026500920403     C* C/ASSEGNO
026600941214     C                   MOVE      ARBCAS        WCOM13
026700941214     C                   MOVEA     WCOM13        SKI(3)
026800941214     C                   Z-ADD     3             I
026900941214     C                   EXSR      ABBL
027000991111     c     arbcas        ifgt      *zeros
027100991111     c     arbvca        andeq     *blanks
027200991111     c                   movel     'ITL'         arbvca
027300991111     c                   endif
027400920403     C*
027500941219     C                   MOVE      ARBCVB        W001A             1
027600940715     C*
027700940715     C* VERIFICO SE VARIATO IL CODICE BOLLA
027800941214     C     W001A         IFGT      *ZEROS
027900941214     C* DECODIFICO    CODICE BOLLA OLD
028000941214     C                   MOVEL     '3A'          KCOD
028100941214     C                   MOVEL     *BLANKS       KKEY
028200941214     C                   MOVEL     ARBCBP        KKEY
028300941214     C     KTAB1         CHAIN     TABEL00F                           99
028400941214     C  N99              MOVEL     TBLUNI        DS3A
028500941214     C   99              CLEAR                   DS3A
028600941214     C                   MOVEA     ARBCBP        BK(24)
028700941214     C                   MOVEA     §3ADES        BK(27)
028800920403     C*
028900940715     C* DECODIFICO IL CODICE BOLLA NUOVO
029000940714     C                   MOVEL     '3A'          KCOD
029100940715     C                   MOVEL     *BLANKS       KKEY
029200940715     C                   MOVEL     ARBCBN        KKEY
029300940715     C     KTAB1         CHAIN     TABEL00F                           99
029400940715     C  N99              MOVEL     TBLUNI        DS3A
029500940715     C   99              CLEAR                   DS3A
029600941231     C                   MOVEA     ARBCBN        BK(81)
029700941231     C                   MOVEA     §3ADES        BK(84)
029800941214     C                   MOVEA     ARBTIC        BK(138)
029900941214     C                   MOVEA     SKI(3)        BK(142)
030000941214     C                   MOVEA     ','           BK(152)
030100941214     C                   MOVEA     SKI(13)       BK(153)
030200941214     C                   MOVEA     ARBVCA        BK(158)
030300941214     C                   MOVEA     ARBGCA        BK(187)
030400920403     C*
030500940715     C                   MOVEL     *BLANKS       VSCCAM
030600940714     C                   MOVEA     BK            VSCCAM
030700920403     C                   ELSE
030800941221     C* DECODIFICO CODICE BOLLA
030900941221     C                   MOVEL     '3A'          KCOD
031000941221     C                   MOVEL     *BLANKS       KKEY
031100941221     C                   MOVEL     ARBCBP        KKEY
031200941221     C     KTAB1         CHAIN     TABEL00F                           99
031300941221     C  N99              MOVEL     TBLUNI        DS3A
031400941221     C   99              CLEAR                   DS3A
031500941221     C                   MOVEA     ARBCBP        BC(24)
031600941221     C                   MOVEA     §3ADES        BC(27)
031700920403     C* VARIATO SOLO C/ASSEGNO
031800941214     C                   MOVEA     ARBTIC        BC(106)
031900941214     C                   MOVEA     SKI(3)        BC(110)
032000941214     C                   MOVEA     ','           BC(120)
032100941214     C                   MOVEA     SKI(13)       BC(121)
032200941214     C                   MOVEA     ARBVCA        BC(126)
032300941221     C                   MOVEA     ARBGCA        BC(158)
032400920403     C*
032500940715     C                   MOVEL     *BLANKS       VSCCAM
032600991110     C                   MOVEA     BC            VSCCAM
032700940715     C                   ENDIF
032800940715     C                   ENDDO
032900920403     C***
033000991111     C*?--------- F I A R B T 0 0    ( 94 ) -----------------------------------?
033100991112     c     *in94         ifeq      *on
033200991112     C                   MOVEL     'CV'          KCOD
033300991112     C                   MOVEL(P)  arbdiv        KKEY
033400991112     C     KTAB1         CHAIN     TABEL                              31
033500991112     C  N31              MOVEL     TBLUNI        DSCV
033600991112     C   31              clear                   DSCV
033700991112     c                   endif
033800991111     C*** VARIAZ TASSAZIONE 1BOLLA
033900991111    2C   94ARBTRC        IFEQ      '1'
034000930524     C*
034100940715     C                   MOVEL     ARBKSC        WCOM7
034200941214     C                   MOVEA     WCOM7         BT(5)
034300940715     C                   MOVEL     ARBCTR        WCOM3
034400941214     C                   MOVEA     WCOM3         BT(17)
034500941215     C*
034600941215     C     ARBVMD        IFGT      0
034700940715     C                   MOVE      ARBVMD        WCOM13
034800941214     C                   MOVEA     WCOM13        SKI(3)
034900941214     C                   Z-ADD     3             I
035000941214     C                   EXSR      ABBL
035100941215     C                   MOVEA     SKI(3)        BT(27)
035200941215     C                   MOVEA     ','           BT(37)
035300941215     C     BT(36)        IFEQ      ' '
035400941215     C                   MOVEL     '0'           BT(36)
035500941215     C                   ENDIF
035600941215     C                   MOVEA     SKI(13)       BT(38)
035700991111     c*
035800991111     c     arbvad        ifeq      *blanks
035900991111     c                   movel     'ITL'         arbvad
036000991111     c                   endif
036100941215     C                   ELSE
036200941215     C* NON C'E' VALORE MERCE
036300941215     C                   CLEAR                   SKI
036400941215     C                   MOVEA     SKI(2)        BT(27)
036500941215     C                   ENDIF
036600941215     C                   MOVEA     ARBVAD        BT(42)
036700940715     C*
036800941215     C     ARBIAS        IFGT      0
036900940715     C                   MOVE      ARBIAS        WCOM13
037000941214     C                   MOVEA     WCOM13        SKI(3)
037100941214     C                   Z-ADD     3             I
037200941214     C                   EXSR      ABBL
037300941215     C                   MOVEA     SKI(3)        BT(49)
037400941215     C                   MOVEA     ','           BT(59)
037500941215     C     BT(58)        IFEQ      ' '
037600941215     C                   MOVEL     '0'           BT(58)
037700941215     C                   ENDIF
037800941215     C                   MOVEA     SKI(13)       BT(60)
037900991111     c*
038000991111     c     arbvas        ifeq      *blanks
038100991111     c                   movel     'ITL'         arbvas
038200991111     c                   endif
038300941215     C                   ELSE
038400941215     C* NON C'E' IMPORTO
038500941215     C                   CLEAR                   SKI
038600941215     C                   MOVEA     SKI(2)        BT(49)
038700941215     C                   ENDIF
038800941215     C                   MOVEA     ARBVAS        BT(64)
038900941214     C*
039000941215     C     ARBQFT        IFGT      0
039100941214     C                   MOVE      ARBQFT        WCOM13
039200941214     C                   MOVEA     WCOM13        SKI(3)
039300941214     C                   Z-ADD     3             I
039400941214     C                   EXSR      ABBL
039500941215     C                   MOVEA     SKI(3)        BT(75)
039600941215     C                   MOVEA     ','           BT(85)
039700941215     C     BT(84)        IFEQ      ' '
039800941215     C                   MOVEL     '0'           BT(84)
039900941215     C                   ENDIF
040000941215     C                   MOVEA     SKI(13)       BT(86)
040100941215     C                   ELSE
040200941215     C* NON C'E' IMPORTO
040300941215     C                   CLEAR                   SKI
040400941215     C                   MOVEA     SKI(2)        BT(75)
040500941215     C                   ENDIF
040600991111     c*
040700991111     c*
040800991111     C                   MOVEA     ARBDIV        BT(93)
040900991111     c*
041000991111     c                   z-add     arbpor        w0133
041100991111     c                   movel     §cvfdc        wtstdec
041200991111     c                   exsr      edtimporti
041300991111     c                   move      impeditato    wcom10
041400991111     C                   MOVEA     wcom10        BT(102)
041500920403     C*
041600991112     c                   clear                   wvaria
041700991112     c                   clear                   wvaria5
041800941214     C     ARBSV1        IFNE      ' '
041900991112     c* non carico se bollo o iva
042000991112     c     arbsv1        ifne      §$2bol
042100991112     c     arbsv1        andne     §$2iva
042200991111     C                   z-add     ARBVA1        W0133
042300991111     c                   movel     §cvfdc        wtstdec
042400991111     c                   exsr      edtimporti
042500991111     c                   move      impeditato    wcom10
042600991112     c                   movel     arbsv1        wsvn
042700991112     c                   add       1             wvaria
042800991112     c                   exsr      rievar
042900991112     c                   else
043000991112     C                   CLEAR                   SKI
043100991112     C                   MOVEA     SKI(5)        BT(119)
043200991112     c                   endif
043300941215     C                   ELSE
043400941215     C* NON C'E' VARIA
043500941215     C                   CLEAR                   SKI
043600991111     C                   MOVEA     SKI(5)        BT(119)
043700941215     C                   ENDIF
043800920403     C*
043900941215     C     ARBSV2        IFNE      ' '
044000991112     c* non carico se bollo o iva
044100991112     c     arbsv2        ifne      §$2bol
044200991112     c     arbsv2        andne     §$2iva
044300991111     C                   z-add     ARBVA2        w0133
044400991111     C                   movel     §cvfdc        wtstdec
044500991111     C                   exsr      edtimporti
044600991111     C                   move      impeditato    wcom10
044700991112     c                   movel     arbsv2        wsvn
044800991112     c                   add       1             wvaria
044900991112     c                   exsr      rievar
045000991112     c                   else
045100991112     C                   CLEAR                   SKI
045200991112     C                   MOVEA     SKI(5)        BT(131)
045300991112     c                   endif
045400941215     C                   ELSE
045500941215     C* NON C'E' VARIA
045600941215     C                   CLEAR                   SKI
045700991111     C                   MOVEA     SKI(5)        BT(131)
045800941215     C                   ENDIF
045900940715     C*
046000941214     C     ARBSV3        IFNE      ' '
046100991112     c* non carico se bollo o iva
046200991112     c     arbsv3        ifne      §$2bol
046300991112     c     arbsv3        andne     §$2iva
046400991111     C                   z-add     ARBVA3        w0133
046500991111     C                   movel     §cvfdc        wtstdec
046600991111     C                   exsr      edtimporti
046700991111     C                   move      impeditato    wcom10
046800991112     c                   movel     arbsv3        wsvn
046900991112     c                   add       1             wvaria
047000991112     c                   exsr      rievar
047100991112     c                   else
047200991112     C                   CLEAR                   SKI
047300991112     C                   MOVEA     SKI(5)        BT(143)
047400991112     c                   endif
047500941215     C                   ELSE
047600941215     C* NON C'E' VARIA
047700941215     C                   CLEAR                   SKI
047800991111     C                   MOVEA     SKI(5)        BT(143)
047900941215     C                   ENDIF
048000941215     C*
048100991112     C* VEDO SE ESISTono altre  varie
048200941215     C                   CLEAR                   SKI
048300991111     C                   MOVEA     SKI(5)        BT(155)
048400991111     C                   MOVEA     SKI(5)        BT(167)
048500941215     C*
048600941214     C     ARBSV3        IFNE      *BLANKS
048700991111     c                   movel     arbtrc        wtrc
048800991112     C     KARBU         CHAIN     FiARBU00                           30
048900991112     c     *in30         doweq     *off
049000991112     c* non carico se bollo o iva
049100991112     c     arbsvn        ifne      §$2bol
049200991112     c     arbsvn        andne     §$2iva
049300991111     C                   z-add     ARBVAN        W0133
049400991111     C                   movel     §cvfdc        wtstdec
049500991111     C                   exsr      edtimporti
049600991111     C                   move      impeditato    wcom10
049700991112     c                   add       1             wvaria
049800991112     c                   movel     arbsvn        wsvn
049900991112     c     wvaria        ifle      5
050000991112     c     wvaria        ifeq      5
050100991112     c                   z-add     arbvan        wvaria5
050200991112     c                   endif
050300991112     c                   exsr      rievar
050400991112     c                   else
050500991112     c* se le varie da caricare sono più di 5 le sommo tutte insieme e le
050600991112     c* visualizzo nella quinta
050700991112     c                   add       arbvan        wvaria5
050800991112     c                   endif
050900991112     c                   endif
051000991112     C     KARBU         reade     FiARBU00                               30
051100991111     c                   enddo
051200941214     C                   ENDIF
051300991112     c* se ho più di cinque varie imposto la quinta = alla sommma delle
051400991112     c* varie dalla quinta in poi
051500991112     c     wvaria        ifgt      5
051600991112     C                   z-add     wvaria5       w0133
051700991112     c                   movel     §cvfdc        wtstdec
051800991112     c                   exsr      edtimporti
051900991112     c                   move      impeditato    wcom11
052000991112     C                   movea     wcom11        BT(167)
052100991112     c                   endif
052200941214     C*
052300941215     C     ARBIMV        IFGT      0
052400991111     C                   z-add     ARBIMV        W0122
052500991111     C                   movel     §cvfdc        wtstdec
052600991111     C                   exsr      edtimport122
052700991111     C                   move      impeditato    wcom12
052800991111     c                   movea     wcom12        bt(182)
052900941215     C                   ELSE
053000941215     C                   CLEAR                   SKI
053100991111     C                   MOVEA     SKI(4)        BT(182)
053200941215     C                   ENDIF
053300941214     C*
053400991111     C                   MOVEA     ARBCEI        BT(201)
053500941214     C*
053600941215     C     ARBIFT        IFGT      0
053700991111     C                   z-add     ARBIFT        W0122
053800991111     C                   movel     §cvfdc        wtstdec
053900991111     C                   exsr      edtimport122
054000991111     C                   move      impeditato    wcom12
054100991111     c                   movea     wcom12        bt(212)
054200941215     C                   ELSE
054300941215     C                   CLEAR                   SKI
054400991111     C                   MOVEA     SKI(4)        BT(212)
054500941215     C                   ENDIF
054600920403     C*
054700940715     C                   MOVEL     ARBNFT        WCOM6
054800940715     C                   MOVEA     WCOM6         SKI(10)
054900940715     C                   Z-ADD     10            I
055000920403     C                   EXSR      ABBL
055100941214     C                   MOVEA     SKI(10)       BT(229)
055200920403     C*
055300940715     C     ARBDFT        IFNE      0
055400940715     C                   MOVE      ARBDFT        WCOM8
055500941214     C                   MOVEA     WCOM8         SKI(8)
055600941214     C                   MOVEA     SKI(14)       WGG               2
055700941214     C                   MOVEA     SKI(12)       WMM               2
055800941214     C                   MOVEA     SKI(8)        WAA               4
055900941214     C                   MOVEA     WGG           BT(236)
056000941214     C                   MOVEA     '/'           BT(238)
056100941214     C                   MOVEA     WMM           BT(239)
056200920403     C                   ELSE
056300941215     C                   MOVEA     *BLANKS       BT(229)
056400920403     C                   END
056500920403     C*
056600940715     C                   MOVEL     *BLANKS       VSCCAM
056700940714     C                   MOVEA     BT            VSCCAM
056800991111     c
056900991111    2C                   else
057000940715     C*
057100991111     C*?--------- F I A R B T 0 0    ( 94 ) -----------------------------------?
057200940715     C*   VARIAZ TASSAZIONE 2 BOLLA
057300991112     c* non carico se bollo o iva
057400040202      * carico tutte e 4 le varie
057500040202     c                   Clear                   Ski
057600041130     c                   Clear                   wvaria
057700041130     c                   Clear                   wvaria5
057800041130
057900041130     c                   Movea     ski(5)        T2(88)
058000041130     c                   Movea     ski(5)        T2(100)
058100041130     c                   Movea     ski(5)        T2(112)
058200041130     c                   Movea     ski(5)        T2(124)
058300041130     c                   Movea     ski(5)        T2(136)
058400041130     c                   Movea     ski(5)        T2(148)
058500040202
058600040202if  3c                   if        ArbSv1 <> §$2bol and ArbSv1 <> §$2iva
058700040202     c                   Z-add     ArbVa1        w0133
058800040202     c                   Eval      Wtstdec = §cvfdc
058900040202     c                   ExSr      EDTimporti
059000041130     c                   Move      IMPeditato    wcom10
059100041130     c                   Eval      wsvn = ArbSv1
059200041130     c                   add       1             wvaria
059300041130     c                   ExSr      rievar1
059400040202    3c                   EndIf
059500040202
059600040202if  3c                   if        ArbSv2 <> §$2bol and ArbSv2 <> §$2iva
059700040202     c                   Z-add     ArbVa2        w0133
059800040202     c                   Eval      Wtstdec = §cvfdc
059900040202     c                   ExSr      EDTimporti
060000041130     c                   Move      IMPeditato    wcom10
060100041130     c                   Eval      wsvn = ArbSv2
060200041130     c                   add       1             wvaria
060300041130     c                   ExSr      rievar1
060400040202    3c                   EndIf
060500040202
060600040202if  3c                   if        ArbSv3 <> §$2bol and ArbSv3 <> §$2iva
060700040202     c                   Z-add     ArbVa3        w0133
060800040202     c                   Eval      Wtstdec = §cvfdc
060900040202     c                   ExSr      EDTimporti
061000041130     c                   Move      IMPeditato    wcom10
061100041130     c                   Eval      wsvn = ArbSv3
061200041130     c                   add       1             wvaria
061300041130     c                   ExSr      rievar1
061400040202    3c                   EndIf
061500040202
061600040202     c                   Movel     ArbTrc        wtrc
061700040202     c     kArbu         Setll     Fiarbu1l
061800040202do  3c                   Do        *Hival
061900040202     c     kArbu         Reade     Fiarbu1l
062000040202     c                   If        %Eof(Fiarbu1l)
062100040202     c                   Leave
062200040202     c                   EndIf
062300040202if  4c                   if        ArbSvn <> §$2bol and ArbSvn <> §$2iva
062400040202     c                   Z-add     ArbVan        w0133
062500040202     c                   Eval      Wtstdec = §cvfdc
062600040202     c                   ExSr      EDTimporti
062700041130     c                   Move      IMPeditato    wcom10
062800041130     c                   Eval      wsvn = ArbSvn
062900041130     c                   add       1             wvaria
063000041130if  5c                   If        wvaria <= 6
063100041130if  6c                   If        wvaria = 6
063200041130     c                   Z-add     ArbVan        wvaria5
063300041130    6c                   EndIf
063400041130     c                   ExSr      rievar1
063500041130   x5c                   Else
063600041130      * se le varie da caricare sono più di 6 le sommo tutte insieme e le visualizzo nella sesta
063700041130     c                   add       ArbVan        wvaria5
063800041130    5c                   EndIf
063900040202    4c                   EndIf
064000040202    3c                   EndDo
064100041130      * imposto la sesta varia a video = alla somma delle varie dalla sesta in poi
064200041130     c                   If        wvaria > 6
064300041130     c                   Z-add     wvaria5       w0133
064400041130     c                   Eval      Wtstdec = §cvfdc
064500041130     c                   ExSr      EDTimporti
064600041130     c                   Move      IMPeditato    wcom11
064700041130     c                   Movea     wcom11        T2(149)
064800041130     c                   EndIf
064900040202
065000040202     c**!!!arbsv1        ifne      §$2bol
065100040202     c**!!!arbsv1        andne     §$2iva
065200040202     C**!!!              MOVEA     ARBSV1        T2(70)
065300040202     C**!!!              z-add     ARBVA1        w0133
065400040202     c**!!!              else
065500040202     C**!!!              MOVEA     ARBSV2        T2(70)
065600040202     C**!!!              z-add     ARBVA2        w0133
065700040202     c**!!!              endif
065800040202     c**!!!              movel     §cvfdc        wtstdec
065900040202     c**!!!              exsr      edtimporti
066000040202     c**!!!              move      impeditato    wcom10
066100040202     C**!!!              MOVEA     WCOM10        t2(71)
066200930210     C* IMPORTO FATTURA
066300991111     C                   z-add     ARbIFT        w0122
066400991111     C                   movel     §cvfdc        wtstdec
066500991111     C                   exsr      edtimport122
066600991111     C                   move      impeditato    wcom12
066700040202     C**!!!              MOVEA     wcom12        T2(97)
066800040202     c                   MOVEA     wcom12        T2(175)
066900920403     C* ES IVA
067000040202     C**!!!              MOVEA     ARbCEI        T2(125)
067100040202     c                   MOVEA     ARbCEI        T2(197)
067200920403     C*
067300991111     C     ARbCEI        IFNE      ' '
067400040202     C**!!!              MOVEA     '-'           T2(126)
067500040202     c                   MOVEA     '-'           T2(198)
067600940714     C                   MOVEL     'EI'          KCOD
067700991111     C                   MOVEL(P)  ARBCEI        KKEY
067800940715     C     KTAB1         CHAIN     TABEL00F                           99
067900940715     C  N99              MOVEL     TBLUNI        DSEI
068000950203     C   99              CLEAR                   DSEI
068100940715     C                   ELSE
068200940715     C                   CLEAR                   DSEI
068300040202     C**!!!              MOVEL     ' '           T2(126)
068400040202     c                   MOVEL     ' '           T2(198)
068500940715     C                   ENDIF
068600040202     C**!!!              MOVEA     §EIDES        T2(127)
068700040202     c                   movel     §eides        wesiva
068800040202     c                   MOVEA     wesiva        T2(199)
068900920403     C*
069000920403     C* FIL IVA /NUMERO /DATA FATTURA
069100991111     C     ARbfIV        IFGT      0
069200991111     C                   MOVEL     ARbFIV        WCOM3
069300950203     C                   ELSE
069400950203     C                   CLEAR                   WCOM3
069500950203     C                   ENDIF
069600040202     C**!!!              MOVEA     WCOM3         T2(170)
069700040202     c                   MOVEA     WCOM3         T2(214)
069800920403     C*
069900991111     C     ARBNFT        IFGT      0
070000991111     C                   MOVEL     ARBNFT        WCOM6
070100940715     C                   MOVEA     WCOM6         SKI(10)
070200940715     C                   Z-ADD     10            I
070300920403     C                   EXSR      ABBL
070400040202     C**!!!              MOVEA     SKI(10)       T2(175)
070500040202     c                   MOVEA     SKI(10)       T2(219)
070600950203     C                   ELSE
070700950203     C                   CLEAR                   WCOM6
070800040202     C**!!!              MOVEA     WCOM6         T2(175)
070900040202     c                   MOVEA     WCOM6         T2(219)
071000950203     C                   ENDIF
071100920403     C*
071200991111     C     ARBDFT        IFNE      0
071300991111     C                   MOVEL     ARBDFT        WCOM8
071400941215     C                   MOVEA     WCOM8         SKI(8)
071500941215     C                   MOVEA     SKI(14)       WGG               2
071600941215     C                   MOVEA     SKI(12)       WMM               2
071700941215     C                   MOVEA     SKI(8)        WAA               4
071800040202     c                   Move      waa           waa2              2
071900941215     C*
072000040202     C**!!!              MOVEA     WGG           T2(186)
072100040202     c                   MOVEA     WGG           T2(230)
072200040202     C**!!!              MOVEA     '/'           T2(188)
072300040202     c                   MOVEA     '/'           T2(232)
072400040202     C**!!!              MOVEA     WMM           T2(189)
072500040202     c                   MOVEA     WMM           T2(233)
072600040202     C**!!!              MOVEA     '/'           T2(191)
072700040202     c                   MOVEA     '/'           T2(235)
072800040202     C**!!!              MOVEA     WAA           T2(192)
072900040202     c                   MOVEA     WAA2          T2(236)
073000920403     C                   ELSE
073100920403     C                   MOVEA     *BLANKS       SKI
073200040202     C**!!!              MOVEA     SKI(5)        T2(186)
073300040202     c                   MOVEA     SKI(5)        T2(230)
073400940715     C                   ENDIF
073500940715     C*
073600991111     C                   MOVEL     ARBKSC        WCOM7
073700040202     C**!!!              MOVEA     WCOM7         T2(12)
073800040202     c                   MOVEA     WCOM7         T2(14)
073900991111     C                   MOVEL     ARBCTR        WCOM3
074000040202     C**!!!              MOVEA     WCOM3         T2(31)
074100040202     c                   MOVEA     WCOM3         T2(36)
074200040202     C**!!!              MOVEA     '-'           T2(34)
074300040202     c                   MOVEA     '-'           T2(39)
074400940715     C                   MOVEL     'TR'          KCOD
074500991112     C                   MOVEL     ARBCTR        unoctr            1
074600991112     c                   movel(P)  unoctr        kkey
074700940715     C     KTAB1         CHAIN     TABEL00F                           99
074800940715     C  N99              MOVEL     TBLUNI        DSTR
074900991112     C   99              clear                   DSTR
075000040202     C**!!!              MOVEA     §TRDES        T2(35)
075100040202     c                   MOVEA     §TRDES        T2(40)
075200991112     c*
075300040202     C**!!!              MOVEA     arbdiv        T2(60)
075400040202     c                   MOVEA     arbdiv        T2(67)
075500920403     C*
075600940715     C                   MOVEL     *BLANKS       VSCCAM
075700940714     C                   MOVEA     T2            VSCCAM
075800920403     C*
075900991111     C                   ENDif
076000940715     C*
076100940715     C*?--------- F N A R B G 0 0    ( 97 ) -----------------------------------?
076200940715     C*   CONSEGNA RICHIESTA GIORNI CHIUSURA
076300940715     C   97              DO
076400950111     C                   MOVEA     '   '         CR(30)
076500051128     C                   MOVEA     *BLANKS       SKI(7)
076600051128     C                   MOVEA     SKI(5)        CR(21)
076700051128     c* Verifico tramite fnlv80r se era una tassativa
076800051128     c                   if        arbdcr>0 or arbhcr>0
076900051128     c                   clear                   fnlv80ds
077000051128     c                   eval      ilv80aas=arbaas
077100051128     c                   eval      ilv80lnp=arblnp
077200051128     c                   eval      ilv80nrs=arbnrs
077300051128     c                   eval      ilv80nsp=arbnsp
077400051202     c                   if        partbo='A'
077500051128     c                   eval      ilv80mgs=arbmgs
077600051202     c                   else
077700051202     c                   eval      ilv80mgs=blpmgs
077800051202     c                   endif
077900051128     c                   eval      ilv80tc1=arbtc1
078000051128     c                   eval      ilv80tc2=arbtc2
078100051128     c                   eval      ilv80dcr=arbdcr
078200051128     c                   eval      ilv80hcr=arbhcr
078300051128     c                   eval      ilv80tcr=arbtcr
078400051128     c                   call      'FNLV80R'
078500051128     c                   parm                    kpjba
078600051128     c                   parm                    fnlv80ds
078700051128     c
078800051128     c                   movea     olv80tcrd     cr(21)
078900051128     C                   ENDIF
079000051128     C* DATA
079100051128     c                   if        arbdcr>0
079200051128     C                   MOVEL     ARBDCR        WCOM8
079300051128     C                   MOVEA     WCOM8         SKI(8)
079400051128     C                   MOVEA     SKI(14)       WGG               2
079500051128     C                   MOVEA     SKI(12)       WMM               2
079600051128     C                   MOVEA     SKI(8)        WAA               4
079700051128     C*
079800051128     C                   MOVEA     WGG           CR(33)
079900051128     C                   MOVEA     '/'           CR(35)
080000051128     C                   MOVEA     WMM           CR(36)
080100051128     C                   MOVEA     '/'           CR(38)
080200051128     C                   MOVEA     WAA           CR(39)
080300051128     c                   else
080400051128     C                   MOVEA     *BLANKS       SKI(6)
080500051128     C                   MOVEA     SKI(6)        CR(33)
080600051128     c                   endif
080700051128     c* ORA
080800051128     c                   if        arbhcr>0
080900051128     C                   MOVEL     ARBHCR        WCOM4
081000051128     C                   MOVEA     WCOM4         SKI(12)
081100051128     C                   MOVEA     SKI(12)       CR(45)
081200051128     C                   MOVEA     ':'           CR(47)
081300051128     C                   MOVEA     SKI(14)       CR(48)
081400051128     C                   ELSE
081500051128     C                   MOVEA     *BLANKS       SKI(11)
081600051128     C                   MOVEA     SKI(11)       CR(45)
081700051128     c                   endif
081800051128     C
081900940715     C* GIORNI CHIUSURA
082000941213     C                   MOVEA     ARBGC1        CR(74)
082100941213     C                   MOVEA     ARBGC2        CR(78)
082200941212     C*
082300941212     C*------------------------------
082400941212     C* TIPO CONSEGNA PARTICOLARE (1)
082500941212     C                   MOVEL     *BLANKS       §1PDES
082600941213     C                   MOVEL     ' '           CR(113)
082700941231    3C     ARBTC1        IFNE      *BLANKS
082800941212     C                   MOVEL     '1P'          KCOD
082900941212     C                   MOVEL     *BLANKS       KKEY
083000941212     C                   MOVEL     ARBTC1        KKEY
083100941212     C     KTAB1         CHAIN     TABEL00F                           99
083200941212     C  N99              MOVEL     TBLUNI        DS1P
083300941213     C                   MOVEL     '-'           CR(113)
083400941231    3C                   ENDIF
083500941213     C                   MOVEA     ARBTC1        CR(111)
083600941213     C                   MOVEA     §1PDES        CR(115)
083700941212     C*------------------------------
083800941212     C* TIPO CONSEGNA PARTICOLARE (2)
083900941212     C                   MOVEL     *BLANKS       §1PDES
084000941213     C                   MOVEL     ' '           CR(193)
084100941231    3C     ARBTC2        IFNE      *BLANKS
084200941212     C                   MOVEL     '1P'          KCOD
084300941212     C                   MOVEL     *BLANKS       KKEY
084400941212     C                   MOVEL     ARBTC2        KKEY
084500941212     C     KTAB1         CHAIN     TABEL00F                           99
084600941212     C  N99              MOVEL     TBLUNI        DS1P
084700941213     C                   MOVEL     '-'           CR(193)
084800941231    3C                   ENDIF
084900941213     C                   MOVEA     ARBTC2        CR(191)
085000941213     C                   MOVEA     §1PDES        CR(195)
085100940715     C*
085200940715     C                   MOVEL     *BLANKS       VSCCAM
085300941231     C                   MOVEA     CR            VSCCAM
085400941231    2C                   END
085500941231     C*
085600941231     C*?--------- F N A R B M 0 0    ( 91 ) -----------------------------------?
085700941231     C*** VARIAZ DESTINATARIO
085800941231    2C   91              DO
085900941231     C     ARBCCM        IFGT      0
086000941231     C                   MOVEL     ARBCCM        WCOM7
086100941231     C                   ELSE
086200941231     C                   CLEAR                   WCOM7
086300941231     C                   ENDIF
086400941231     C                   MOVEA     WCOM7         BM(11)
086500941231     C                   MOVEA     ARBRSM        BM(20)
086600941231     C                   MOVEA     ARBINM        BM(81)
086700941231     C                   MOVEA     ARBCAM        BM(118)
086800941231     C                   MOVEA     ARBLOM        BM(129)
086900941231     C                   MOVEA     ARBPRM        BM(167)
087000941231     C                   MOVEA     ARBNZM        BM(171)
087100941231     C                   MOVEA     ARBFAP        BM(188)
087200941231     C                   MOVEA     ARBCPI        BM(207)
087300941231     C                   MOVEA     BM            VSCCAM
087400941231     C                   ENDDO
087500940715     C* -----------------------------------------------------------------------
087600920403     C*
087700920403     C* GIRO DATA VARIAZIONE
087800940714     C                   Z-ADD     ARBDTV        G02INV
087900940714     C                   Z-ADD     *ZEROS        G02DAT
088000940714     C                   MOVEL     '3'           G02ERR
088100940714     C                   CALL      'XSRDA8'
088200940714     C                   PARM                    WLBDAT
088300940714     C                   Z-ADD     G02DAT        VSCDTV
088400060307     C                   Z-ADD     ARBDTV        vshdtv
088500940714     C                   MOVEL     ARBORV        VSCORV
088600060307     C                   MOVEL     ARBORV        VShORV
088700940714     C                   MOVEL     ARBCVB        VSCCVB
088800920402     C* CAUSALE VARIAZIONE
088900060306     c                   clear                   ds7l
089000940714     C                   MOVEL     '7L'          KCOD
089100940714     C                   MOVEL     *BLANKS       KKEY
089200940714     C                   MOVEL     VSCCVB        KKEY
089300940715     C     KTAB1         CHAIN     TABEL00F                           99
089400940714     C* RIEMPO CAMPO VSDCVB
089500060306     c  n99              movel     tbluni        ds7l
089600060306     c                   clear                   vsdcvb
089700060306     c                   clear                   vsdcvbw
089800060306     c                   eval      vsdcvb = arbcvb + '-' + §7ldes
089900060306     c                   eval      vsdcvbw = arbcvb + '-' + §7ldes
090000060306     c
090100060306     C**                 MOVEL     *BLANKS       WWW
090200060306     C**N99              MOVEA     TBLUNI        WWW(4)
090300060306     C** 99              MOVEA     *BLANKS       WWW(4)
090400060306     C**                 MOVEA     VSCCVB        WWW(1)
090500060306     C**                 MOVEA     '-'           WWW(3)
090600060306     C**                 MOVEA     WWW           VSDCVB
090700060306     C**                 MOVEA     WWW           VSDCVBw
090800920402     C*
090900060306     c* verifico se per la variazione esiste motivo
091000060306     c                   exsr      ges_mtv
091100060306     c*
091200940714     C                   WRITE     LR75S01
091300940714     C                   ADD       1             NRR1
091400920402     C*
091500991111     C                   SETOFF                                       9594
091600941231     C                   SETOFF                                       969792
091700941231     C                   SETOFF                                       91
091800991111     C     KARB1         READE     FiARBF1C                               31
091900920402     C                   END
092000940715     C*
092100940714     C     NRR1          IFNE      1
092200940714     C                   Z-ADD     1             NRR1
092300940714     C                   Z-ADD     1             REC1
092400060306     c                   if        wesmtv = '1'
092500060307     c                   eval      intesta = is(2)
092600060306     c                   endif
092700920402     C     FORCT3        TAG
092800940714     C                   WRITE     LR75Z01
092900940714     C                   EXFMT     LR75C01
093000060306     c* se in almeno una variazione è presente il motivo devo leggere il
093100060306     c* subfile per visualizzare il motivo se richiesto
093200060306     c                   if        wesmtv = '1'
093300060306     c                   if        *inkc = '0' and *inkl = '0'
093400060306     c                   exsr      vis_mtv
093500060306     c                   goto      forct3
093600060306     c                   endif
093700060306     c                   endif
093800940802     C*
093900940715     C                   ENDIF
094000930524     C*
094100940802     C   KC              MOVEL     '1'           PARF03
094200940802     C   KC              MOVEL     PARAM         KPJBU
094300940802     C*
094400941222     C     FINE          TAG
094500051128     C  nlr              RETURN
094600051128     c                   if        *inlr
094700051128     c                   clear                   fnlv80ds
094800051128     c                   eval      ilv80tla = 'C'
094900051128     c                   call      'FNLV80R'
095000051128     c                   parm                    kpjba
095100051128     c                   parm                    FNLV80DS
095200060307     c*
095300060307     C                   CLEAR                   tibs02ds
095400060307     C                   MOVEL     'C'           T02TLA
095500060307     C                   CALL      'TIBS02R'
095600060307     C                   PARM                    KPJBA
095700060307     C                   PARM                    tibs02ds
095800051128     c                   endif
095900940714     C                   END
096000991112     C**************************************************************************
096100991112     C*  riempie varie a video
096200991112     C**************************************************************************
096300991112     C     rievar        BEGSR
096400991112     C*
096500991112     c                   select
096600991112     c     wvaria        wheneq    1
096700991112     C                   MOVEA     wcom10        BT(120)
096800991112     C                   MOVEA     wsvn          BT(119)
096900991112     c     wvaria        wheneq    2
097000991112     C                   MOVEA     wcom10        BT(132)
097100991112     C                   MOVEA     wsvn          BT(131)
097200991112     c     wvaria        wheneq    3
097300991112     C                   MOVEA     wcom10        BT(144)
097400991112     C                   MOVEA     wsvn          BT(143)
097500991112     c     wvaria        wheneq    4
097600991112     c                   MOVEA     wcom10        BT(156)
097700991112     C                   MOVEA     wsvn          BT(155)
097800991112     c     wvaria        wheneq    5
097900991112     C                   MOVEA     wcom10        BT(168)
098000991112     C                   MOVEA     wsvn          BT(167)
098100991112     c                   endsl
098200991112     c                   endsr
098300041130     C**************************************************************************
098400041130     C*  riempie varie a video seconda bolla
098500041130     C**************************************************************************
098600041130     C     rievar1       BEGSR
098700041130     C*
098800041130     c                   select
098900041130     c     wvaria        wheneq    1
099000041130     C                   MOVEA     wcom10        T2(89)
099100041130     C                   MOVEA     wsvn          T2(88)
099200041130     c     wvaria        wheneq    2
099300041130     C                   MOVEA     wcom10        T2(101)
099400041130     C                   MOVEA     wsvn          T2(100)
099500041130     c     wvaria        wheneq    3
099600041130     C                   MOVEA     wcom10        T2(113)
099700041130     C                   MOVEA     wsvn          T2(112)
099800041130     c     wvaria        wheneq    4
099900041130     c                   MOVEA     wcom10        T2(125)
100000041130     C                   MOVEA     wsvn          T2(124)
100100041130     c     wvaria        wheneq    5
100200041130     C                   MOVEA     wcom10        T2(137)
100300041130     C                   MOVEA     wsvn          T2(136)
100400041130     c     wvaria        wheneq    6
100500041130     C                   MOVEA     wcom10        T2(149)
100600041130     C                   MOVEA     wsvn          T2(148)
100700041130     c                   endsl
100800041130     c                   endsr
100900940714     C**************************************************************************
101000940714     C*  ABBLANCA ZERO NON SIGNIFICATIVI
101100940714     C**************************************************************************
101200920403     C     ABBL          BEGSR
101300940714     C*
101400940715     C                   SETOFF                                       99
101500920403     C     SKI(I)        DOWEQ     '0'
101600920403     C                   MOVEL     ' '           SKI(I)
101700920403     C                   ADD       1             I
101800941214     C     I             COMP      15                                     99
101900940715     C  N99              END
102000940714     C*
102100920403     C                   ENDSR
102200940714     C**************************************************************************
102300940714     C*  RIEMPIMENTO CAMPI DEL FORMATO LR75C01 (SUBFILE CONTROL)
102400940714     C**************************************************************************
102500940714     C     RIEC01        BEGSR
102600950629     C* BOLLA IN PARTENZA
102700950629    1C     PARTBO        IFEQ      'P'
102800941222     C                   SETOFF                                       10
102900950629    2C     WAPRP         IFEQ      ' '
103000941222     C                   MOVEL     '1'           WAPRP             1
103100941222     C                   OPEN      FNBLP01L
103200950629    2C                   ENDIF
103300941222     C*
103400950629   X1C                   ELSE
103500950629     C* BOLLA IN ARRIVO
103600941222     C                   SETON                                        10
103700950629    2C     WAPRA         IFEQ      ' '
103800941222     C                   MOVEL     '1'           WAPRA             1
103900941222     C                   OPEN      FNARB01L
104000950629    2C                   ENDIF
104100950629    1C                   ENDIF
104200941222     C*
104300940714     C* AGGANCIO LA BOLLA PER I DATI DI TESTATA
104400941222     C     *IN10         IFEQ      *ON
104500941222     C     KARB1         CHAIN     FNARB01L                           99
104600940714     C*
104700941222     C   99              GOTO      ENDRIE
104800941222     C*
104900940714     C                   Z-ADD     ARBDSP        G02INV
105000940714     C                   Z-ADD     *ZEROS        G02DAT
105100940714     C                   MOVEL     '3'           G02ERR
105200940714     C                   CALL      'XSRDA8'
105300940714     C                   PARM                    WLBDAT
105400940714     C                   Z-ADD     G02DAT        VCCDSP
105500941221     C*
105600940714     C                   MOVEL     '3A'          KCOD
105700940714     C                   MOVEL     *BLANKS       KKEY
105800940718     C                   MOVEL     ARBCBO        KKEY
105900940714     C     KTAB1         CHAIN     TABEL00F                           99
106000940714     C   99              CLEAR                   DS3A
106100940714     C  N99              MOVEL     TBLUNI        DS3A
106200940715     C                   MOVEL     *BLANKS       WWW
106300940715     C                   MOVEA     §3ADES        WWW(4)
106400940718     C                   MOVEA     ARBCBO        WWW(1)
106500940715     C                   MOVEA     '-'           WWW(3)
106600940715     C                   MOVEA     WWW           VCDCBO
106700941221     C*
106800940714     C                   MOVEL     ARBRSD        VCCRSD
106900940714     C                   MOVEL     ARBIND        VCCIND
107000940714     C                   MOVEL     ARBLOD        VCCLOD
107100940714     C                   MOVEL     ARBPRD        VCCPRD
107200940714     C                   Z-ADD     ARBKSC        VCCKSC
107300941215     C                   Z-ADD     ARBPKF        VCCPKF
107400941215     C                   Z-ADD     ARBVLF        VCCVLF
107500941215     C                   MOVEL     ARBFVF        VCCFVF
107600941221     C*
107700940714     C                   MOVEL     *BLANKS       VCDTC1
107800940714     C     ARBTC1        IFNE      *BLANKS
107900940714     C                   MOVEL     '1P'          KCOD
108000940714     C                   MOVEL     *BLANKS       KKEY
108100940714     C                   MOVEL     ARBTC1        KKEY
108200940714     C     KTAB1         CHAIN     TABEL00F                           99
108300940715     C                   MOVEL     *BLANKS       WWW
108400940715     C  N99              MOVEA     TBLUNI        WWW(3)
108500940715     C                   MOVEL     ARBTC1        WWW(1)
108600940715     C                   MOVEL     '-'           WWW(2)
108700940715     C                   MOVEA     WWW           VCDTC1
108800940714     C                   ENDIF
108900940714     C                   MOVEL     *BLANKS       VCDTC2
109000940714     C     ARBTC2        IFNE      *BLANKS
109100940714     C                   MOVEL     '1P'          KCOD
109200940714     C                   MOVEL     *BLANKS       KKEY
109300940714     C                   MOVEL     ARBTC2        KKEY
109400940714     C     KTAB1         CHAIN     TABEL00F                           99
109500940715     C                   MOVEL     *BLANKS       WWW
109600940715     C  N99              MOVEA     TBLUNI        WWW(3)
109700940715     C                   MOVEL     ARBTC2        WWW(1)
109800940715     C                   MOVEL     '-'           WWW(2)
109900940715     C                   MOVEA     WWW           VCDTC2
110000940714     C                   END
110100941221     C* C/ASSEGNO
110200051114     C     KARB1         CHAIN     FiAR901L                           99
110300941221     C     *IN99         IFEQ      *OFF
110400940802     C                   MOVEL     AR9TIC        VCCTIC
110500940802     C                   Z-ADD     AR9CAS        VCCCAS
110600940802     C                   MOVEL     AR9VCA        VCCVCA
110700991110     C     VCCCAS        IFGT      *ZEROS
110800991110     C     VCCVCA        ANDEQ     *BLANKS
110900991110     C                   MOVEL     'ITL'         VCCVCA
111000991110     C                   ENDIF
111100941221     C                   ELSE
111200941221     C                   CLEAR                   VCCTIC
111300941221     C                   CLEAR                   VCCCAS
111400941221     C                   CLEAR                   VCCVCA
111500941221     C                   ENDIF
111600941221     C*
111700940714     C                   Z-ADD     ARBVMD        VCCVMD
111800940714     C                   MOVEL     ARBVAD        VCCVAD
111900991110     C     VCCVMD        IFGT      *ZEROS
112000991110     C     VCCVAD        ANDEQ     *BLANKS
112100991110     C                   MOVEL     'ITL'         VCCVAD
112200991110     C                   ENDIF
112300940714     C                   Z-ADD     ARBIAS        VCCIAS
112400940714     C                   MOVEL     ARBVAS        VCCVAS
112500991110     C     VCCIAS        IFGT      *ZEROS
112600991110     C     VCCVAS        ANDEQ     *BLANKS
112700991110     C                   MOVEL     'ITL'         VCCVAS
112800991110     C                   ENDIF
112900940714     C                   MOVEL     'TR'          KCOD
113000940714     C                   MOVEL     *BLANKS       KKEY
113100940714     C                   MOVEL     ARBCTR        UNOCTR            1
113200940714     C                   MOVEL     UNOCTR        KKEY
113300940714     C     KTAB1         CHAIN     TABEL00F                           99
113400940715     C                   MOVEL     TBLUNI        DSTR
113500940715     C                   MOVEL     *BLANKS       WWW
113600940715     C                   MOVEA     §TRDES        WWW(5)
113700940715     C                   MOVEL     ARBCTR        WCOM3
113800940715     C                   MOVEA     WCOM3         WWW(1)
113900940715     C                   MOVEA     '-'           WWW(4)
114000940715     C                   MOVEA     WWW           VCDCTR
114100051104     c                   select
114200051104     c                   when      arbffd='S'
114300051104     C                   MOVEL     'SI '         VCCFFD
114400051104     c                   when      arbffd='M'
114500051104     C                   MOVEL     'SAB'         VCCFFD
114600051104     C                   other
114700051104     C                   MOVEL     '   '         VCCFFD
114800051104     C                   ENDsl
114900940714     C*
115000940714     C* AGGANCIO LA TASSAZIONE PER GLI ALTRI DATI DI TESTATA
115100991111     c                   exsr      rietas
115200941222     C*
115300941222     C                   ELSE
115400941222     C     KARB1         CHAIN     FNBLP01L                           99
115500941222     C*
115600941222     C   99              GOTO      ENDRIE
115700941222     C*
115800941222     C                   Z-ADD     BLPDSP        G02INV
115900941222     C                   Z-ADD     *ZEROS        G02DAT
116000941222     C                   MOVEL     '3'           G02ERR
116100941222     C                   CALL      'XSRDA8'
116200941222     C                   PARM                    WLBDAT
116300941222     C                   Z-ADD     G02DAT        VCCDSP
116400941222     C*
116500941222     C                   MOVEL     '3A'          KCOD
116600941222     C                   MOVEL     *BLANKS       KKEY
116700941222     C                   MOVEL     BLPCBO        KKEY
116800941222     C     KTAB1         CHAIN     TABEL00F                           99
116900941222     C   99              CLEAR                   DS3A
117000941222     C  N99              MOVEL     TBLUNI        DS3A
117100941222     C                   MOVEL     *BLANKS       WWW
117200941222     C                   MOVEA     §3ADES        WWW(4)
117300941222     C                   MOVEA     BLPCBO        WWW(1)
117400941222     C                   MOVEA     '-'           WWW(3)
117500941222     C                   MOVEA     WWW           VCDCBO
117600941222     C*
117700941222     C                   MOVEL     BLPRSD        VCCRSD
117800941222     C                   MOVEL     BLPIND        VCCIND
117900941222     C                   MOVEL     BLPLOD        VCCLOD
118000941222     C                   MOVEL     BLPPRD        VCCPRD
118100941222     C                   Z-ADD     BLPKSC        VCCKSC
118200941222     C                   Z-ADD     BLPPKF        VCCPKF
118300941222     C                   Z-ADD     BLPVLF        VCCVLF
118400941222     C                   MOVEL     BLPFVF        VCCFVF
118500941222     C*
118600941222     C                   MOVEL     *BLANKS       VCDTC1
118700941222     C     BLPTC1        IFNE      *BLANKS
118800941222     C                   MOVEL     '1P'          KCOD
118900941222     C                   MOVEL     *BLANKS       KKEY
119000941222     C                   MOVEL     BLPTC1        KKEY
119100941222     C     KTAB1         CHAIN     TABEL00F                           99
119200941222     C                   MOVEL     *BLANKS       WWW
119300941222     C  N99              MOVEA     TBLUNI        WWW(3)
119400941222     C                   MOVEL     BLPTC1        WWW(1)
119500941222     C                   MOVEL     '-'           WWW(2)
119600941222     C                   MOVEA     WWW           VCDTC1
119700941222     C                   ENDIF
119800941222     C                   MOVEL     *BLANKS       VCDTC2
119900941222     C     BLPTC2        IFNE      *BLANKS
120000941222     C                   MOVEL     '1P'          KCOD
120100941222     C                   MOVEL     *BLANKS       KKEY
120200941222     C                   MOVEL     BLPTC2        KKEY
120300941222     C     KTAB1         CHAIN     TABEL00F                           99
120400941222     C                   MOVEL     *BLANKS       WWW
120500941222     C  N99              MOVEA     TBLUNI        WWW(3)
120600941222     C                   MOVEL     BLPTC2        WWW(1)
120700941222     C                   MOVEL     '-'           WWW(2)
120800941222     C                   MOVEA     WWW           VCDTC2
120900941222     C                   END
121000941222     C* C/ASSEGNO
121100051118     C     KARB1         CHAIN     Fiar901L                           99
121200941222     C     *IN99         IFEQ      *OFF
121300051118     C                   MOVEL     ar9TIC        VCCTIC
121400051118     C                   Z-ADD     ar9CAS        VCCCAS
121500051118     C                   MOVEL     ar9VCA        VCCVCA
121600991111     c     vcccas        ifgt      *zeros
121700991111     c     vccvca        andeq     *blanks
121800991111     c                   movel     'ITL'         vccvca
121900991111     c                   endif
122000941222     C                   ELSE
122100941222     C                   CLEAR                   VCCTIC
122200941222     C                   CLEAR                   VCCCAS
122300941222     C                   CLEAR                   VCCVCA
122400941222     C                   ENDIF
122500941222     C*
122600941222     C                   Z-ADD     BLPVMD        VCCVMD
122700941222     C                   MOVEL     BLPVAD        VCCVAD
122800991111     c     vccvmd        ifgt      *zeros
122900991111     c     vccvad        andeq     *blanks
123000991111     c                   movel     'ITL'         vccvad
123100991111     c                   endif
123200941222     C                   Z-ADD     BLPIAS        VCCIAS
123300941222     C                   MOVEL     BLPVAS        VCCVAS
123400991111     c     vccias        ifgt      *zeros
123500991111     c     vccvas        andeq     *blanks
123600991111     c                   movel     'ITL'         vccvas
123700991111     c                   end
123800941222     C                   MOVEL     'TR'          KCOD
123900941222     C                   MOVEL     *BLANKS       KKEY
124000941222     C                   MOVEL     BLPCTR        UNOCTR            1
124100941222     C                   MOVEL     UNOCTR        KKEY
124200941222     C     KTAB1         CHAIN     TABEL00F                           99
124300941222     C                   MOVEL     TBLUNI        DSTR
124400941222     C                   MOVEL     *BLANKS       WWW
124500941222     C                   MOVEA     §TRDES        WWW(5)
124600941222     C                   MOVEL     BLPCTR        WCOM3
124700941222     C                   MOVEA     WCOM3         WWW(1)
124800941222     C                   MOVEA     '-'           WWW(4)
124900941222     C                   MOVEA     WWW           VCDCTR
125000051104     c                   select
125100051104     c                   when      blpffd='S'
125200051104     C                   MOVEL     'SI '         VCCFFD
125300051104     c                   when      blpffd='M'
125400051104     C                   MOVEL     'SAB'         VCCFFD
125500051104     C                   other
125600051104     C                   MOVEL     '   '         VCCFFD
125700051104     C                   ENDsl
125800941222     C*
125900991111     c                   exsr      rietas
126000941222     C                   ENDIF
126100941222     C*
126200941222     C                   SETOFF                                       99
126300060306     c                   eval      intesta = is(1)
126400940714     C*
126500941222     C     ENDRIE        ENDSR
126600991111      **********************************************************************
126700991111      * riempio campi del video relativi la tassazione
126800991111      **********************************************************************
126900991111     C     RIETAS        BEGSR
127000991111     C                   MOVEL     '1'           WTRC
127100991111     C     KARB2         CHAIN     FIAR601L                           99
127200991111     C*
127300991111     C     *IN99         IFEQ      *OFF
127400991111     c* divisa
127500991111     C                   MOVEL     AR6DIV        VCCDIV
127600991111     C                   MOVEL     'CV'          KCOD
127700991111     C                   MOVEL(P)  AR6div        KKEY
127800991111     C     KTAB1         CHAIN     TABEL                              31
127900991111     C  N31              MOVEL     TBLUNI        DSCV
128000991111     C   31              clear                   DSCV
128100991111     c* porto
128200991111     c                   z-add     ar6por        w0133
128300991111     c                   movel     §cvfdc        wtstdec
128400991111     c                   exsr      edtimporti
128500991111     c                   move      impeditato    vccpor
128600991111     c*
128700991111     C                   Z-ADD     AR6NFT        VCCNFT
128800991111     C                   Z-ADD     AR6IFT        VCCIFT
128900991111     C                   Z-ADD     AR6DFT        G02INV
129000991111     C                   Z-ADD     *ZEROS        G02DAT
129100991111     C                   MOVEL     '3'           G02ERR
129200991111     C                   CALL      'XSRDA8'
129300991111     C                   PARM                    WLBDAT
129400991111     C                   Z-ADD     G02DAT        VCCDFT
129500991111     c* varia 1
129600991111     C                   MOVEL     AR6SV1        VCCSV1
129700991111     c                   z-add     ar6va1        w0133
129800991111     c                   movel     §cvfdc        wtstdec
129900991111     c                   exsr      edtimporti
130000991111     c                   move      impeditato    vccva1
130100991111     c* varia 2
130200991111     C                   MOVEL     AR6SV2        VCCSV2
130300991111     c                   z-add     ar6va2        w0133
130400991111     c                   movel     §cvfdc        wtstdec
130500991111     c                   exsr      edtimporti
130600991111     c                   move      impeditato    vccva2
130700991111     c* varia 3
130800991111     C                   MOVEL     AR6SV3        VCCSV3
130900991111     c                   z-add     ar6va3        w0133
131000991111     c                   movel     §cvfdc        wtstdec
131100991111     c                   exsr      edtimporti
131200991111     c                   move      impeditato    vccva3
131300991111     C                   ELSE
131400991111     C                   CLEAR                   VCCDIV
131500991111     C                   CLEAR                   VCCPOR
131600991111     C                   CLEAR                   VCCNFT
131700991111     C                   CLEAR                   VCCIFT
131800991111     C                   CLEAR                   VCCDFT
131900991111     C                   CLEAR                   VCCSV1
132000991111     C                   CLEAR                   VCCVA1
132100991111     C                   CLEAR                   VCCSV2
132200991111     C                   CLEAR                   VCCVA2
132300991111     C                   CLEAR                   VCCSV3
132400991111     C                   CLEAR                   VCCVA3
132500991111     C                   ENDIF
132600991111     C*
132700991111     C                   ENDSR
132800991111      **********************************************************************
132900991111      * EDITO IMPORTI PER CAMPI in stampa
133000991111      **********************************************************************
133100991111     C     EDTIMPORTI    BEGSR
133200991111
133300991111      * ------------------------------------------------------------------------
133400991111      * W0133      contiene il numero da editare
133500991111      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il num
133600991111      *            ero ha decimali
133700991111      * IMPEDITATO ritorna il valore editato per la stampa lungo13 (troncate
133800991111      *            cifre oltre 10 mld)
133900991111      * ------------------------------------------------------------------------
134000991111
134100991111     c                   clear                   IMPeditato
134200991111     c                   clear                   W014A
134300991111     c                   if        Wtstdec = 'S'
134400991111     C                   eval      W014a = %EDITC(W0133:'4')
134500991111     c                   else
134600991111     c                   z-add     W0133         W0140
134700991111     C                   eval      W014a = %EDITC(W0140:'4')
134800991111     c                   endif
134900991111     c
135000991111     c                   move      W014a         IMPeditato
135100991111     c
135200991111     c                   ENDSR
135300991111     C*
135400991111      **********************************************************************
135500991111      * EDITO IMPORTI PER CAMPI a video
135600991111      **********************************************************************
135700991111     C     EDTimport122  BEGSR
135800991111
135900991111     c                   clear                   IMPeditato
136000991111     c                   clear                   W013A
136100991111     c                   if        Wtstdec = 'S'
136200991111     C                   eval      W013a = %EDITC(W0122:'4')
136300991111     c                   else
136400991111     c                   z-add     W0122         W0130
136500991111     C                   eval      W013a = %EDITC(W0130:'4')
136600991111     c                   endif
136700991111     c
136800991111     c                   move      W013a         IMPeditato
136900991111     c
137000991111     c                   ENDSR
137100060306     C**************************************************************************
137200060306     C* Impostazione campo per visualizzazione motivo variazione
137300060306     C**************************************************************************
137400060306     C     ges_mtv       BEGSR
137500060306     c                   seton                                        01
137600060306     c                   clear                   vscvmv
137700060307     c                   clear                   vshvmv
137800060307     c                   clear                   vsdvmv
137900060306     c                   eval      wtrd = '§' + arbcvb
138000060307     c     kar5          chain     fiar501l
138100060307     c                   if        %found(fiar501l)
138200060306     c                   setoff                                       01
138300060307     c                   movel     ar5uni        dar5mtv
138400060307     c                   clear                   dmtv
138500060307     C                   CLEAR                   tibs02ds
138600060307     C                   MOVEL     'C'           T02MOD
138700060307     C                   MOVEL     KNSIF         T02SIF
138800060307     C                   MOVEL     'MTV'         T02COD
138900060307     c                   movel(P)  §ar5mtv       t02ke1
139000060307     c                   call      'TIBS02R'
139100060307     C                   PARM                    KPJBA
139200060307     C                   PARM                    tibs02ds
139300060307    2C                   IF        T02ERR = *BLANKS
139400060307     C                   MOVEL     T02uni        dmtv
139500060307    2C                   ENDIF
139600060307     c                   movel     §mtvdes       vsdvmv
139700060307     c                   movel     §mtvdes       vshvmv
139800060306     c                   eval      wesmtv = '1'
139900060306     c                   endif
140000991111     C*
140100060306     c                   endsr
140200060306     C**************************************************************************
140300060306     C* Visualizzazione motivo variazione
140400060306     C**************************************************************************
140500060306     C     vis_mtv       BEGSR
140600060310     c                   setoff                                       01
140700060306     c                   readc     lr75s01
140800060306     c                   dow       %eof = *off
140900060306     c                   if        vscvmv = 'V'
141000060306     c*
141100060306     c                   clear                   vsctitw
141200060306     c                   eval      vsctitw = 'Variazione del ' +
141300060306     c                             %editc(vscdtv:'8') +  ' ore ' +
141400060306     c                             %editw(vscorv : '  :  ') + ' utente ' +
141500060307     c                             vscpru
141600060306     c*
141700060307     c                   eval      wtrd = '§' + %subst(vsdcvb:1:2)
141800060307     c     kar5          chain     fiar501l
141900060307     c                   if        %found(fiar501l)
142000060306     c                   movel     ar5uni        dar5mtv
142100060306     c                   movel     §ar5mtv       vscmtv
142200060306     c* decodifico motivo variazione
142300060307     c                   eval      vsdmtv = vshvmv
142400060306     c                   movel     §ar5txt       vscnt1
142500060306     c                   move      §ar5txt       vscnt2
142600060306     c                   exfmt     lr75wd1
142700060307     c                   endif
142800060306     c                   clear                   vscvmv
142900060306     C                   Z-ADD     NRR1          REC1
143000060306     C                   UPDATE    Lr75S01
143100060306     c                   endif
143200060306     c                   readc     lr75s01
143300060306     c                   enddo
143400060306     C*
143500060306     c                   endsr
143600940714     C**************************************************************************
143700940714     C*  ROUTINE INIZIALE
143800940714     C**************************************************************************
143900940714     C     *INZSR        BEGSR
144000940714     C*
144100940714     C     KARB1         KLIST
144200940714     C                   KFLD                    VCCAAS
144300940714     C                   KFLD                    VCCLNP
144400940714     C                   KFLD                    VCCNRS
144500940714     C                   KFLD                    VCCNSP
144600991110     C     KARB2         KLIST
144700991110     C                   KFLD                    VCCAAS
144800991110     C                   KFLD                    VCCLNP
144900991110     C                   KFLD                    VCCNRS
145000991110     C                   KFLD                    VCCNSP
145100991110     C                   KFLD                    WTRC
145200991112     C     KARBU         KLIST
145300991112     C                   KFLD                    arbAAS
145400991112     C                   KFLD                    arbLNP
145500991112     C                   KFLD                    arbNRS
145600991112     C                   KFLD                    arbNSP
145700991112     C                   KFLD                    WTRC
145800991112     c                   kfld                    arbdtv
145900991112     c                   kfld                    arborv
146000060307     C     KAR5          KLIST
146100060307     C                   KFLD                    arbAAS
146200060307     C                   KFLD                    arbLNP
146300060307     C                   KFLD                    arbNRS
146400060307     C                   KFLD                    arbNSP
146500060307     C                   KFLD                    WTRd
146600060307     c                   kfld                    vshdtv
146700060307     c                   kfld                    vshorv
146800940714     C     KTAB1         KLIST
146900940714     C                   KFLD                    KKUT
147000940714     C                   KFLD                    KCOD
147100940714     C                   KFLD                    KKEY
147200991112     c*
147300940714     C*
147400991111     C     *LIKE         DEFINE    ARbCVB        VSCCVB
147500940714     C     *LIKE         DEFINE    TBLKUT        KKUT
147600940714     C     *LIKE         DEFINE    TBLCOD        KCOD
147700940714     C     *LIKE         DEFINE    TBLKEY        KKEY
147800991110     C     *LIKE         DEFINE    AR6TRC        WTRC
147900060306     C     *LIKE         DEFINE    AR5TRd        WTRd
148000940715     C                   MOVEL     *BLANKS       WCOM3             3
148100940715     C                   MOVEL     *BLANKS       WCOM4             4
148200940715     C                   MOVEL     *BLANKS       WCOM5             5
148300940715     C                   MOVEL     *BLANKS       WCOM6             6
148400940715     C                   MOVEL     *BLANKS       WCOM7             7
148500940715     C                   MOVEL     *BLANKS       WCOM8             8
148600940715     C                   MOVEL     *BLANKS       WCOM9             9
148700991111     C                   MOVEL     *BLANKS       WCOM10           10
148800991112     C                   MOVEL     *BLANKS       WCOM11           11
148900991111     C                   MOVEL     *BLANKS       WCOM12           12
149000940715     C                   MOVEL     *BLANKS       WCOM13           13
149100940715     C                   MOVEL     *BLANKS       WCOM16           16
149200940715     C*
149300940714     C                   Z-ADD     1             KKUT
149400991112     C* CARICO DA SIGLE VARIE
149500991112     C                   MOVEL     '$2'          KCOD
149600991112     C                   MOVEL(p)  '1'           KKEY
149700991112     C     KTAB1         CHAIN     TABEL                              31
149800991112     C  N31              MOVEL     TBLUNI        DS$2
149900991112     C   31              MOVEL     *BLANKS       DS$2
150000071026     c
150100071026      * Aggancio tabella "VPODECO" per reperimento data attivazione
150200071026      * gestione prepagati
150300120510     C****               CLEAR                   tibs02ds
150400120510     c****               clear                   dvpodeco
150500120510     C****               MOVEL     'C'           T02MOD
150600120510     C****               MOVEL     KNSIF         T02SIF
150700120510     C****               MOVEL     'VPO'         T02COD
150800120510     c***                movel(P)  'DECO'        t02ke1
150900120510     C****               CALL      'TIBS02R'
151000120510     C****               PARM                    KPJBA
151100120510     C****               PARM                    tibs02ds
151200120510    2C****               IF        T02ERR = *BLANKS
151300120510     C****               MOVEL     T02uni        dvpodeco
151400120510    2C***                ENDIF
151500071026     C*
151600940714     C*
151700940714     C                   ENDSR
151800940714      **************************************************************************
151900940715      *  1         2         3         4         5         6         7         8
152000940715123456*8901234567890123456789012345678901234567890123456789012345678901234567890
152100941231** ?----  BD  -----------------------------------------------------------------?
152200941212Destinatario: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
152300941212Xxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxx
152400941212Xxxxxxx  xx  xxx   Inoltro: x    F.Deposito: xx    Partita IVA: xxxxxxxxxxxxxxxx
152500940715** ?----  BP  -----------------------------------------------------------------?
152600941212Peso  Kg.: xxxxxx,x                    Volume da Fatturare:  x  xx,xxx
152700941212** ?----  VP  -----------------------------------------------------------------?
152800941212Volume da Fatturare:  x  xx,xxx
152900940715** ?----  BK  -----------------------------------------------------------------?
153000941231Cod.bolla precedente:  xx-xxxxxxxxxxxxxxxxxxxxxxxxx           Cod.bolla nuovo:
153100941231XX-xxxxxxxxxxxxxxxxxxxxxxxxx   Contrassegno precedente:  xx  xxxxxxxxxx,xxx  xxx
153200941214Particolarita' C/Assegno: xx
153300940715** ?----  BC  -----------------------------------------------------------------?
153400941214Codice   Bolla      :  xx-xxxxxxxxxxxxxxxxxxxxxxxxx
153500941221Contrassegno precedente: xx  XXXXXXXXXX,XXX  XXX   Particolarita' C/Assegno: xx
153600940715** ?----  BT  -----------------------------------------------------------------?
153700941215Cli xxxxxxx Tar xxx VMercexxxxxxxxxx,xxx xxx IAsxxxxxxxxxx,xxx xxx Qta'Fatxxxxxx
153800991110xxxx,xxx Divxxx Traspxxxxxxxxxx Varie:xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxx
153900991110xxxxx xxxxxxxxxxx Imp123456789012 Esenz.X Fatt:tot.123456789012 num.xxxxxx xx/xx
154000940715** ?----  T2  -----------------------------------------------------------------?
154100040202Cod.Cliente: xxxxxxx  Cod.Tariffa: xxx-xxxxxxxxxxxxxxxxxxxx  Div: xxx
154200041130Varie: xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
154300040203Importo fat.: xxxxxxxxxxxx  Es.IVA: x-xxxxx  Fat.n.: XXX  xxxxxx del xx/xx/xx
154400940715** ?----  CR  -----------------------------------------------------------------?
154500051128Consegna richiesta: xxxxxxxxxxx xx/xx/xxxx  xx:xx    Giorni di Chiusura: xx  xx
154600941213      Consegna Particolare 1: x x xxxxxxxxxxxxxxxxxxxxxxxxx
154700941213      Consegna Particolare 2: x x xxxxxxxxxxxxxxxxxxxxxxxxx
154800941231** ?----  BM  -----------------------------------------------------------------?
154900941231Mittente: xxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
155000941231xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
155100941231xxx   xx  xxx   Anteporto: x     Partita IVA: xxxxxxxxxxxxxxxx
155200060306** ?----  IS  -----------------------------------------------------------------?
155300060306 Data         Ora     Utente       Causale
155400060307 Data         Ora     Utente       Causale                    Motivo          V
155500060316** ?----  RM  -----------------------------------------------------------------?
155600060316Rif.Mitt.Alfabetico: xxxxxxxxxxxxxxx   Natura Merce: xxxxxxxxxxxxxxx
155700061102** ?----  FI  -----------------------------------------------------------------?
155800071026XXXXXXXXXXXXXXXXXXXXXXX: Codice Fiscale  xxxxxxxxxxxxxxxx
155900071026                            Partita IVA  xxxxxxxxxxxxxxxx
