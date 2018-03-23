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
015200940714     C                   MOVEL     *BLANKS       VSCCAM
015300940715     C*
015400940715     C*?--------- F N A R B D 0 0    ( 96 ) -----------------------------------?
015500920403     C*** VARIAZ DESTINATARIO
015600061102     c   96              select
015700061102    2C     ARBCVB        wheneq    'I0'
015800941212     C                   MOVEA     ARBRSD        BD(15)
015900941212     C                   MOVEA     ARBRD2        BD(51)
016000941212     C                   MOVEA     ARBIND        BD(87)
016100941212     C                   MOVEA     ARBCAD        BD(123)
016200941212     C                   MOVEA     ARBLOD        BD(133)
016300941212     C                   MOVEA     ARBPRD        BD(170)
016400941212     C                   MOVEA     ARBNZD        BD(174)
016500941212     C                   MOVEA     ARBFIN        BD(189)
016600051104     c                   select
016700051104     c                   when      arbffd='S'
016800051104     C                   MOVEA     'SI '         BD(206)
016900051104     c                   when      arbffd='M'
017000051104     C                   MOVEA     'SAB'         BD(206)
017100051104     C                   other
017200051104     C                   MOVEA     '   '         BD(206)
017300051104    3C                   ENDsl
017400941212     C                   MOVEA     ARBCPI        BD(225)
017500940714     C                   MOVEA     BD            VSCCAM
017600130927     C*------------------------------
017700060316     c* Rif. mitt. alfa e Natura merce
017800061102    2c     arbcvb        wheneq    'RM'
017900060316     C                   MOVEA     arbrsd        RM(22)
018000060316     C                   MOVEA     arbrd2        RM(54)
018100060316     c                   eval      w017A =  '   Natura Merce: '
018200060316     c                   movea     w017a         RM(37)
018300060316     C                   MOVEL     *BLANKS       VSCCAM
018400060316     C                   MOVEA     RM            VSCCAM
018500061102     C*------------------------------
018600071026     c* Codice fiscale / Partiva iva
018700061102    2c     arbcvb        wheneq    'FI'
018800061102     c                   if        arbrd2='D'
018900071026     c                   movea     cdest         FI(1)
019000061102     c                   else
019100071026     c                   movea     cmitt         FI(1)
019200061102     c                   endif
019300071026     c                   movea     arbcpi        FI(42)
019400071026     c* Se trascorsa la data attivazione, visualizzo anche partita iva
019500120510     c*****              if        arbdtV>§vpopiv
019600071026     c                   movel     arbrsd        w016a            16
019700071026     c                   movea     w016a         FI(122)
019800071026     C                   MOVEA     FI            VSCCAM
019900120510     c****               else
020000120510     c***                movea     fi            vsccam
020100120510     c***                eval      %subst(vsccam:106:50)=*blanks
020200120510     c****               endif
020300071026     c
020400061102     C*------------------------------
020500061102   x2c                   other
020600941216     C                   MOVEL     ARBCVB        UNOCVB            1
020700940715     C*
020800941216    3C     UNOCVB        IFEQ      'I'
020900940715     C*------------------------------
021000920402     C* PESO
021100941212     C                   MOVEL     ARBPKF        WCOM7
021200941214     C                   MOVEA     WCOM7         SKI(9)
021300940715     C                   Z-ADD     9             I                 2 0
021400941214     C                   EXSR      ABBL
021500920403     C*
021600941214    4C     SKI(14)       IFEQ      ' '
021700940715     C                   MOVEL     '0'           SKI(14)
021800940715    4C                   ENDIF
021900920403     C*
022000941212     C                   MOVEA     SKI(9)        BP(12)
022100941212     C                   MOVEA     ','           BP(18)
022200941212     C                   MOVEA     SKI(15)       BP(19)
022300940715     C*------------------------------
022400920402     C* VOLUME
022500941212     C                   MOVEL     ARBVLF        WCOM5
022600940715     C                   MOVEA     WCOM5         SKI(11)
022700940715    4C     SKI(11)       IFEQ      '0'
022800940715     C                   MOVEL     ' '           SKI(11)
022900940715    4C                   ENDIF
023000920403     C*
023100941212     C                   MOVEA     ARBFVF        BP(62)
023200941212     C                   MOVEA     SKI(11)       BP(65)
023300941212     C                   MOVEA     ','           BP(67)
023400941212     C                   MOVEA     SKI(13)       BP(68)
023500940715     C                   MOVEL     *BLANKS       VSCCAM
023600940714     C                   MOVEA     BP            VSCCAM
023700940715     C*------------------------------
023800930524   X3C                   ELSE
023900940715     C*------------------------------
024000940715     C* VARIAZIONE "VA", "VP" E "VT" DEL VOLUME
024100941212     C                   MOVEL     ARBVLF        WCOM5
024200940715     C                   MOVEA     WCOM5         SKI(11)
024300940715    4C     SKI(11)       IFEQ      '0'
024400940715     C                   MOVEL     ' '           SKI(11)
024500940715    4C                   ENDIF
024600930524     C*
024700941212     C                   MOVEA     ARBFVF        VP(23)
024800941212     C                   MOVEA     SKI(11)       VP(26)
024900941212     C                   MOVEA     ','           VP(28)
025000941212     C                   MOVEA     SKI(13)       VP(29)
025100940715     C                   MOVEL     *BLANKS       VSCCAM
025200941212     C                   MOVEA     VP            VSCCAM
025300940715    3C                   ENDIF
025400061102    2C                   ENDSL
025500130927     c
025600130927     C*?--------- F N A R B N 0 0    ( 88 ) -----------------------------------?
025700130927     C*** VARIAZ referente consegna + telefono
025800130927     C*------------------------------
025900130927     c   88              select
026000130927     c
026100130927    2c     arbtrd        wheneq    'REF'
026200130927     c                   movel     arbuni70      darbn_REf
026300130927     c                   movea     refN          vsccam
026400130927     c                   eval      %subst(vsccam:25:35)=§arbnref
026500130927     c                   eval      %subst(vsccam:105:16)=§arbnteld
026600130927    2c     arbtrd        wheneq    'SMS'
026700130927     c                   movel     arbuni70      darbn_sms
026800130927     c                   movea     smsN          vsccam
026900130927     c                   eval      %subst(vsccam:25:70)=§arbncel
027000130927    2c     arbtrd        wheneq    'EML'
027100130927     c                   movel     arbuni70      darbn_eml
027200130927     c                   movea     emlN          vsccam
027300130927     c                   eval      %subst(vsccam:25:70)=§arbneml
027400130930    2c     arbtrd        wheneq    'NOT'
027500130930     c                   movel     arbuni70      darbn_not
027600130930     c                   movea     notN          vsccam
027700130930     c                   eval      %subst(vsccam:13:70)=§arbnnot1+§arbnnot2
027800130927     c
027900130927     c                   endsl
028000130927     c
028100940715     C*
028200941231     C*?--------- F N A R B V 0 0    ( 92 ) -----------------------------------?
028300941231     C*** VARIAZ VOLUME CML
028400941231    2C   92              DO
028500941231     C* VARIAZIONE "VA", "VP" E "VT" DEL VOLUME
028600941231     C                   MOVEL     ARBVLC        WCOM5
028700941231     C                   MOVEA     WCOM5         SKI(11)
028800941231    3C     SKI(11)       IFEQ      '0'
028900941231     C                   MOVEL     ' '           SKI(11)
029000941231    3C                   ENDIF
029100941231     C*
029200941231     C                   MOVEA     ARBFVC        VP(23)
029300941231     C                   MOVEA     SKI(11)       VP(26)
029400941231     C                   MOVEA     ','           VP(28)
029500941231     C                   MOVEA     SKI(13)       VP(29)
029600941231     C                   MOVEL     *BLANKS       VSCCAM
029700941231     C                   MOVEA     VP            VSCCAM
029800941231    2C                   ENDDO
029900940715     C*?--------- F N A R B K 0 0    ( 95 ) -----------------------------------?
030000920403     C*** VARIAZ COD BOLLA
030100940715     C   95              DO
030200920403     C* C/ASSEGNO
030300941214     C                   MOVE      ARBCAS        WCOM13
030400941214     C                   MOVEA     WCOM13        SKI(3)
030500941214     C                   Z-ADD     3             I
030600941214     C                   EXSR      ABBL
030700991111     c     arbcas        ifgt      *zeros
030800991111     c     arbvca        andeq     *blanks
030900991111     c                   movel     'ITL'         arbvca
031000991111     c                   endif
031100920403     C*
031200941219     C                   MOVE      ARBCVB        W001A             1
031300940715     C*
031400940715     C* VERIFICO SE VARIATO IL CODICE BOLLA
031500941214     C     W001A         IFGT      *ZEROS
031600941214     C* DECODIFICO    CODICE BOLLA OLD
031700941214     C                   MOVEL     '3A'          KCOD
031800941214     C                   MOVEL     *BLANKS       KKEY
031900941214     C                   MOVEL     ARBCBP        KKEY
032000941214     C     KTAB1         CHAIN     TABEL00F                           99
032100941214     C  N99              MOVEL     TBLUNI        DS3A
032200941214     C   99              CLEAR                   DS3A
032300941214     C                   MOVEA     ARBCBP        BK(24)
032400941214     C                   MOVEA     §3ADES        BK(27)
032500920403     C*
032600940715     C* DECODIFICO IL CODICE BOLLA NUOVO
032700940714     C                   MOVEL     '3A'          KCOD
032800940715     C                   MOVEL     *BLANKS       KKEY
032900940715     C                   MOVEL     ARBCBN        KKEY
033000940715     C     KTAB1         CHAIN     TABEL00F                           99
033100940715     C  N99              MOVEL     TBLUNI        DS3A
033200940715     C   99              CLEAR                   DS3A
033300941231     C                   MOVEA     ARBCBN        BK(81)
033400941231     C                   MOVEA     §3ADES        BK(84)
033500941214     C                   MOVEA     ARBTIC        BK(138)
033600941214     C                   MOVEA     SKI(3)        BK(142)
033700941214     C                   MOVEA     ','           BK(152)
033800941214     C                   MOVEA     SKI(13)       BK(153)
033900941214     C                   MOVEA     ARBVCA        BK(158)
034000941214     C                   MOVEA     ARBGCA        BK(187)
034100920403     C*
034200940715     C                   MOVEL     *BLANKS       VSCCAM
034300940714     C                   MOVEA     BK            VSCCAM
034400920403     C                   ELSE
034500941221     C* DECODIFICO CODICE BOLLA
034600941221     C                   MOVEL     '3A'          KCOD
034700941221     C                   MOVEL     *BLANKS       KKEY
034800941221     C                   MOVEL     ARBCBP        KKEY
034900941221     C     KTAB1         CHAIN     TABEL00F                           99
035000941221     C  N99              MOVEL     TBLUNI        DS3A
035100941221     C   99              CLEAR                   DS3A
035200941221     C                   MOVEA     ARBCBP        BC(24)
035300941221     C                   MOVEA     §3ADES        BC(27)
035400920403     C* VARIATO SOLO C/ASSEGNO
035500941214     C                   MOVEA     ARBTIC        BC(106)
035600941214     C                   MOVEA     SKI(3)        BC(110)
035700941214     C                   MOVEA     ','           BC(120)
035800941214     C                   MOVEA     SKI(13)       BC(121)
035900941214     C                   MOVEA     ARBVCA        BC(126)
036000941221     C                   MOVEA     ARBGCA        BC(158)
036100920403     C*
036200940715     C                   MOVEL     *BLANKS       VSCCAM
036300991110     C                   MOVEA     BC            VSCCAM
036400940715     C                   ENDIF
036500940715     C                   ENDDO
036600920403     C***
036700991111     C*?--------- F I A R B T 0 0    ( 94 ) -----------------------------------?
036800991112     c     *in94         ifeq      *on
036900991112     C                   MOVEL     'CV'          KCOD
037000991112     C                   MOVEL(P)  arbdiv        KKEY
037100991112     C     KTAB1         CHAIN     TABEL                              31
037200991112     C  N31              MOVEL     TBLUNI        DSCV
037300991112     C   31              clear                   DSCV
037400991112     c                   endif
037500991111     C*** VARIAZ TASSAZIONE 1BOLLA
037600991111    2C   94ARBTRC        IFEQ      '1'
037700930524     C*
037800940715     C                   MOVEL     ARBKSC        WCOM7
037900941214     C                   MOVEA     WCOM7         BT(5)
038000940715     C                   MOVEL     ARBCTR        WCOM3
038100941214     C                   MOVEA     WCOM3         BT(17)
038200941215     C*
038300941215     C     ARBVMD        IFGT      0
038400940715     C                   MOVE      ARBVMD        WCOM13
038500941214     C                   MOVEA     WCOM13        SKI(3)
038600941214     C                   Z-ADD     3             I
038700941214     C                   EXSR      ABBL
038800941215     C                   MOVEA     SKI(3)        BT(27)
038900941215     C                   MOVEA     ','           BT(37)
039000941215     C     BT(36)        IFEQ      ' '
039100941215     C                   MOVEL     '0'           BT(36)
039200941215     C                   ENDIF
039300941215     C                   MOVEA     SKI(13)       BT(38)
039400991111     c*
039500991111     c     arbvad        ifeq      *blanks
039600991111     c                   movel     'ITL'         arbvad
039700991111     c                   endif
039800941215     C                   ELSE
039900941215     C* NON C'E' VALORE MERCE
040000941215     C                   CLEAR                   SKI
040100941215     C                   MOVEA     SKI(2)        BT(27)
040200941215     C                   ENDIF
040300941215     C                   MOVEA     ARBVAD        BT(42)
040400940715     C*
040500941215     C     ARBIAS        IFGT      0
040600940715     C                   MOVE      ARBIAS        WCOM13
040700941214     C                   MOVEA     WCOM13        SKI(3)
040800941214     C                   Z-ADD     3             I
040900941214     C                   EXSR      ABBL
041000941215     C                   MOVEA     SKI(3)        BT(49)
041100941215     C                   MOVEA     ','           BT(59)
041200941215     C     BT(58)        IFEQ      ' '
041300941215     C                   MOVEL     '0'           BT(58)
041400941215     C                   ENDIF
041500941215     C                   MOVEA     SKI(13)       BT(60)
041600991111     c*
041700991111     c     arbvas        ifeq      *blanks
041800991111     c                   movel     'ITL'         arbvas
041900991111     c                   endif
042000941215     C                   ELSE
042100941215     C* NON C'E' IMPORTO
042200941215     C                   CLEAR                   SKI
042300941215     C                   MOVEA     SKI(2)        BT(49)
042400941215     C                   ENDIF
042500941215     C                   MOVEA     ARBVAS        BT(64)
042600941214     C*
042700941215     C     ARBQFT        IFGT      0
042800941214     C                   MOVE      ARBQFT        WCOM13
042900941214     C                   MOVEA     WCOM13        SKI(3)
043000941214     C                   Z-ADD     3             I
043100941214     C                   EXSR      ABBL
043200941215     C                   MOVEA     SKI(3)        BT(75)
043300941215     C                   MOVEA     ','           BT(85)
043400941215     C     BT(84)        IFEQ      ' '
043500941215     C                   MOVEL     '0'           BT(84)
043600941215     C                   ENDIF
043700941215     C                   MOVEA     SKI(13)       BT(86)
043800941215     C                   ELSE
043900941215     C* NON C'E' IMPORTO
044000941215     C                   CLEAR                   SKI
044100941215     C                   MOVEA     SKI(2)        BT(75)
044200941215     C                   ENDIF
044300991111     c*
044400991111     c*
044500991111     C                   MOVEA     ARBDIV        BT(93)
044600991111     c*
044700991111     c                   z-add     arbpor        w0133
044800991111     c                   movel     §cvfdc        wtstdec
044900991111     c                   exsr      edtimporti
045000991111     c                   move      impeditato    wcom10
045100991111     C                   MOVEA     wcom10        BT(102)
045200920403     C*
045300991112     c                   clear                   wvaria
045400991112     c                   clear                   wvaria5
045500941214     C     ARBSV1        IFNE      ' '
045600991112     c* non carico se bollo o iva
045700991112     c     arbsv1        ifne      §$2bol
045800991112     c     arbsv1        andne     §$2iva
045900991111     C                   z-add     ARBVA1        W0133
046000991111     c                   movel     §cvfdc        wtstdec
046100991111     c                   exsr      edtimporti
046200991111     c                   move      impeditato    wcom10
046300991112     c                   movel     arbsv1        wsvn
046400991112     c                   add       1             wvaria
046500991112     c                   exsr      rievar
046600991112     c                   else
046700991112     C                   CLEAR                   SKI
046800991112     C                   MOVEA     SKI(5)        BT(119)
046900991112     c                   endif
047000941215     C                   ELSE
047100941215     C* NON C'E' VARIA
047200941215     C                   CLEAR                   SKI
047300991111     C                   MOVEA     SKI(5)        BT(119)
047400941215     C                   ENDIF
047500920403     C*
047600941215     C     ARBSV2        IFNE      ' '
047700991112     c* non carico se bollo o iva
047800991112     c     arbsv2        ifne      §$2bol
047900991112     c     arbsv2        andne     §$2iva
048000991111     C                   z-add     ARBVA2        w0133
048100991111     C                   movel     §cvfdc        wtstdec
048200991111     C                   exsr      edtimporti
048300991111     C                   move      impeditato    wcom10
048400991112     c                   movel     arbsv2        wsvn
048500991112     c                   add       1             wvaria
048600991112     c                   exsr      rievar
048700991112     c                   else
048800991112     C                   CLEAR                   SKI
048900991112     C                   MOVEA     SKI(5)        BT(131)
049000991112     c                   endif
049100941215     C                   ELSE
049200941215     C* NON C'E' VARIA
049300941215     C                   CLEAR                   SKI
049400991111     C                   MOVEA     SKI(5)        BT(131)
049500941215     C                   ENDIF
049600940715     C*
049700941214     C     ARBSV3        IFNE      ' '
049800991112     c* non carico se bollo o iva
049900991112     c     arbsv3        ifne      §$2bol
050000991112     c     arbsv3        andne     §$2iva
050100991111     C                   z-add     ARBVA3        w0133
050200991111     C                   movel     §cvfdc        wtstdec
050300991111     C                   exsr      edtimporti
050400991111     C                   move      impeditato    wcom10
050500991112     c                   movel     arbsv3        wsvn
050600991112     c                   add       1             wvaria
050700991112     c                   exsr      rievar
050800991112     c                   else
050900991112     C                   CLEAR                   SKI
051000991112     C                   MOVEA     SKI(5)        BT(143)
051100991112     c                   endif
051200941215     C                   ELSE
051300941215     C* NON C'E' VARIA
051400941215     C                   CLEAR                   SKI
051500991111     C                   MOVEA     SKI(5)        BT(143)
051600941215     C                   ENDIF
051700941215     C*
051800991112     C* VEDO SE ESISTono altre  varie
051900941215     C                   CLEAR                   SKI
052000991111     C                   MOVEA     SKI(5)        BT(155)
052100991111     C                   MOVEA     SKI(5)        BT(167)
052200941215     C*
052300941214     C     ARBSV3        IFNE      *BLANKS
052400991111     c                   movel     arbtrc        wtrc
052500991112     C     KARBU         CHAIN     FiARBU00                           30
052600991112     c     *in30         doweq     *off
052700991112     c* non carico se bollo o iva
052800991112     c     arbsvn        ifne      §$2bol
052900991112     c     arbsvn        andne     §$2iva
053000991111     C                   z-add     ARBVAN        W0133
053100991111     C                   movel     §cvfdc        wtstdec
053200991111     C                   exsr      edtimporti
053300991111     C                   move      impeditato    wcom10
053400991112     c                   add       1             wvaria
053500991112     c                   movel     arbsvn        wsvn
053600991112     c     wvaria        ifle      5
053700991112     c     wvaria        ifeq      5
053800991112     c                   z-add     arbvan        wvaria5
053900991112     c                   endif
054000991112     c                   exsr      rievar
054100991112     c                   else
054200991112     c* se le varie da caricare sono più di 5 le sommo tutte insieme e le
054300991112     c* visualizzo nella quinta
054400991112     c                   add       arbvan        wvaria5
054500991112     c                   endif
054600991112     c                   endif
054700991112     C     KARBU         reade     FiARBU00                               30
054800991111     c                   enddo
054900941214     C                   ENDIF
055000991112     c* se ho più di cinque varie imposto la quinta = alla sommma delle
055100991112     c* varie dalla quinta in poi
055200991112     c     wvaria        ifgt      5
055300991112     C                   z-add     wvaria5       w0133
055400991112     c                   movel     §cvfdc        wtstdec
055500991112     c                   exsr      edtimporti
055600991112     c                   move      impeditato    wcom11
055700991112     C                   movea     wcom11        BT(167)
055800991112     c                   endif
055900941214     C*
056000941215     C     ARBIMV        IFGT      0
056100991111     C                   z-add     ARBIMV        W0122
056200991111     C                   movel     §cvfdc        wtstdec
056300991111     C                   exsr      edtimport122
056400991111     C                   move      impeditato    wcom12
056500991111     c                   movea     wcom12        bt(182)
056600941215     C                   ELSE
056700941215     C                   CLEAR                   SKI
056800991111     C                   MOVEA     SKI(4)        BT(182)
056900941215     C                   ENDIF
057000941214     C*
057100991111     C                   MOVEA     ARBCEI        BT(201)
057200941214     C*
057300941215     C     ARBIFT        IFGT      0
057400991111     C                   z-add     ARBIFT        W0122
057500991111     C                   movel     §cvfdc        wtstdec
057600991111     C                   exsr      edtimport122
057700991111     C                   move      impeditato    wcom12
057800991111     c                   movea     wcom12        bt(212)
057900941215     C                   ELSE
058000941215     C                   CLEAR                   SKI
058100991111     C                   MOVEA     SKI(4)        BT(212)
058200941215     C                   ENDIF
058300920403     C*
058400940715     C                   MOVEL     ARBNFT        WCOM6
058500940715     C                   MOVEA     WCOM6         SKI(10)
058600940715     C                   Z-ADD     10            I
058700920403     C                   EXSR      ABBL
058800941214     C                   MOVEA     SKI(10)       BT(229)
058900920403     C*
059000940715     C     ARBDFT        IFNE      0
059100940715     C                   MOVE      ARBDFT        WCOM8
059200941214     C                   MOVEA     WCOM8         SKI(8)
059300941214     C                   MOVEA     SKI(14)       WGG               2
059400941214     C                   MOVEA     SKI(12)       WMM               2
059500941214     C                   MOVEA     SKI(8)        WAA               4
059600941214     C                   MOVEA     WGG           BT(236)
059700941214     C                   MOVEA     '/'           BT(238)
059800941214     C                   MOVEA     WMM           BT(239)
059900920403     C                   ELSE
060000941215     C                   MOVEA     *BLANKS       BT(229)
060100920403     C                   END
060200920403     C*
060300940715     C                   MOVEL     *BLANKS       VSCCAM
060400940714     C                   MOVEA     BT            VSCCAM
060500991111     c
060600991111    2C                   else
060700940715     C*
060800991111     C*?--------- F I A R B T 0 0    ( 94 ) -----------------------------------?
060900940715     C*   VARIAZ TASSAZIONE 2 BOLLA
061000991112     c* non carico se bollo o iva
061100040202      * carico tutte e 4 le varie
061200040202     c                   Clear                   Ski
061300041130     c                   Clear                   wvaria
061400041130     c                   Clear                   wvaria5
061500041130
061600041130     c                   Movea     ski(5)        T2(88)
061700041130     c                   Movea     ski(5)        T2(100)
061800041130     c                   Movea     ski(5)        T2(112)
061900041130     c                   Movea     ski(5)        T2(124)
062000041130     c                   Movea     ski(5)        T2(136)
062100041130     c                   Movea     ski(5)        T2(148)
062200040202
062300040202if  3c                   if        ArbSv1 <> §$2bol and ArbSv1 <> §$2iva
062400040202     c                   Z-add     ArbVa1        w0133
062500040202     c                   Eval      Wtstdec = §cvfdc
062600040202     c                   ExSr      EDTimporti
062700041130     c                   Move      IMPeditato    wcom10
062800041130     c                   Eval      wsvn = ArbSv1
062900041130     c                   add       1             wvaria
063000041130     c                   ExSr      rievar1
063100040202    3c                   EndIf
063200040202
063300040202if  3c                   if        ArbSv2 <> §$2bol and ArbSv2 <> §$2iva
063400040202     c                   Z-add     ArbVa2        w0133
063500040202     c                   Eval      Wtstdec = §cvfdc
063600040202     c                   ExSr      EDTimporti
063700041130     c                   Move      IMPeditato    wcom10
063800041130     c                   Eval      wsvn = ArbSv2
063900041130     c                   add       1             wvaria
064000041130     c                   ExSr      rievar1
064100040202    3c                   EndIf
064200040202
064300040202if  3c                   if        ArbSv3 <> §$2bol and ArbSv3 <> §$2iva
064400040202     c                   Z-add     ArbVa3        w0133
064500040202     c                   Eval      Wtstdec = §cvfdc
064600040202     c                   ExSr      EDTimporti
064700041130     c                   Move      IMPeditato    wcom10
064800041130     c                   Eval      wsvn = ArbSv3
064900041130     c                   add       1             wvaria
065000041130     c                   ExSr      rievar1
065100040202    3c                   EndIf
065200040202
065300040202     c                   Movel     ArbTrc        wtrc
065400040202     c     kArbu         Setll     Fiarbu1l
065500040202do  3c                   Do        *Hival
065600040202     c     kArbu         Reade     Fiarbu1l
065700040202     c                   If        %Eof(Fiarbu1l)
065800040202     c                   Leave
065900040202     c                   EndIf
066000040202if  4c                   if        ArbSvn <> §$2bol and ArbSvn <> §$2iva
066100040202     c                   Z-add     ArbVan        w0133
066200040202     c                   Eval      Wtstdec = §cvfdc
066300040202     c                   ExSr      EDTimporti
066400041130     c                   Move      IMPeditato    wcom10
066500041130     c                   Eval      wsvn = ArbSvn
066600041130     c                   add       1             wvaria
066700041130if  5c                   If        wvaria <= 6
066800041130if  6c                   If        wvaria = 6
066900041130     c                   Z-add     ArbVan        wvaria5
067000041130    6c                   EndIf
067100041130     c                   ExSr      rievar1
067200041130   x5c                   Else
067300041130      * se le varie da caricare sono più di 6 le sommo tutte insieme e le visualizzo nella sesta
067400041130     c                   add       ArbVan        wvaria5
067500041130    5c                   EndIf
067600040202    4c                   EndIf
067700040202    3c                   EndDo
067800041130      * imposto la sesta varia a video = alla somma delle varie dalla sesta in poi
067900041130     c                   If        wvaria > 6
068000041130     c                   Z-add     wvaria5       w0133
068100041130     c                   Eval      Wtstdec = §cvfdc
068200041130     c                   ExSr      EDTimporti
068300041130     c                   Move      IMPeditato    wcom11
068400041130     c                   Movea     wcom11        T2(149)
068500041130     c                   EndIf
068600040202
068700040202     c**!!!arbsv1        ifne      §$2bol
068800040202     c**!!!arbsv1        andne     §$2iva
068900040202     C**!!!              MOVEA     ARBSV1        T2(70)
069000040202     C**!!!              z-add     ARBVA1        w0133
069100040202     c**!!!              else
069200040202     C**!!!              MOVEA     ARBSV2        T2(70)
069300040202     C**!!!              z-add     ARBVA2        w0133
069400040202     c**!!!              endif
069500040202     c**!!!              movel     §cvfdc        wtstdec
069600040202     c**!!!              exsr      edtimporti
069700040202     c**!!!              move      impeditato    wcom10
069800040202     C**!!!              MOVEA     WCOM10        t2(71)
069900930210     C* IMPORTO FATTURA
070000991111     C                   z-add     ARbIFT        w0122
070100991111     C                   movel     §cvfdc        wtstdec
070200991111     C                   exsr      edtimport122
070300991111     C                   move      impeditato    wcom12
070400040202     C**!!!              MOVEA     wcom12        T2(97)
070500040202     c                   MOVEA     wcom12        T2(175)
070600920403     C* ES IVA
070700040202     C**!!!              MOVEA     ARbCEI        T2(125)
070800040202     c                   MOVEA     ARbCEI        T2(197)
070900920403     C*
071000991111     C     ARbCEI        IFNE      ' '
071100040202     C**!!!              MOVEA     '-'           T2(126)
071200040202     c                   MOVEA     '-'           T2(198)
071300940714     C                   MOVEL     'EI'          KCOD
071400991111     C                   MOVEL(P)  ARBCEI        KKEY
071500940715     C     KTAB1         CHAIN     TABEL00F                           99
071600940715     C  N99              MOVEL     TBLUNI        DSEI
071700950203     C   99              CLEAR                   DSEI
071800940715     C                   ELSE
071900940715     C                   CLEAR                   DSEI
072000040202     C**!!!              MOVEL     ' '           T2(126)
072100040202     c                   MOVEL     ' '           T2(198)
072200940715     C                   ENDIF
072300040202     C**!!!              MOVEA     §EIDES        T2(127)
072400040202     c                   movel     §eides        wesiva
072500040202     c                   MOVEA     wesiva        T2(199)
072600920403     C*
072700920403     C* FIL IVA /NUMERO /DATA FATTURA
072800991111     C     ARbfIV        IFGT      0
072900991111     C                   MOVEL     ARbFIV        WCOM3
073000950203     C                   ELSE
073100950203     C                   CLEAR                   WCOM3
073200950203     C                   ENDIF
073300040202     C**!!!              MOVEA     WCOM3         T2(170)
073400040202     c                   MOVEA     WCOM3         T2(214)
073500920403     C*
073600991111     C     ARBNFT        IFGT      0
073700991111     C                   MOVEL     ARBNFT        WCOM6
073800940715     C                   MOVEA     WCOM6         SKI(10)
073900940715     C                   Z-ADD     10            I
074000920403     C                   EXSR      ABBL
074100040202     C**!!!              MOVEA     SKI(10)       T2(175)
074200040202     c                   MOVEA     SKI(10)       T2(219)
074300950203     C                   ELSE
074400950203     C                   CLEAR                   WCOM6
074500040202     C**!!!              MOVEA     WCOM6         T2(175)
074600040202     c                   MOVEA     WCOM6         T2(219)
074700950203     C                   ENDIF
074800920403     C*
074900991111     C     ARBDFT        IFNE      0
075000991111     C                   MOVEL     ARBDFT        WCOM8
075100941215     C                   MOVEA     WCOM8         SKI(8)
075200941215     C                   MOVEA     SKI(14)       WGG               2
075300941215     C                   MOVEA     SKI(12)       WMM               2
075400941215     C                   MOVEA     SKI(8)        WAA               4
075500040202     c                   Move      waa           waa2              2
075600941215     C*
075700040202     C**!!!              MOVEA     WGG           T2(186)
075800040202     c                   MOVEA     WGG           T2(230)
075900040202     C**!!!              MOVEA     '/'           T2(188)
076000040202     c                   MOVEA     '/'           T2(232)
076100040202     C**!!!              MOVEA     WMM           T2(189)
076200040202     c                   MOVEA     WMM           T2(233)
076300040202     C**!!!              MOVEA     '/'           T2(191)
076400040202     c                   MOVEA     '/'           T2(235)
076500040202     C**!!!              MOVEA     WAA           T2(192)
076600040202     c                   MOVEA     WAA2          T2(236)
076700920403     C                   ELSE
076800920403     C                   MOVEA     *BLANKS       SKI
076900040202     C**!!!              MOVEA     SKI(5)        T2(186)
077000040202     c                   MOVEA     SKI(5)        T2(230)
077100940715     C                   ENDIF
077200940715     C*
077300991111     C                   MOVEL     ARBKSC        WCOM7
077400040202     C**!!!              MOVEA     WCOM7         T2(12)
077500040202     c                   MOVEA     WCOM7         T2(14)
077600991111     C                   MOVEL     ARBCTR        WCOM3
077700040202     C**!!!              MOVEA     WCOM3         T2(31)
077800040202     c                   MOVEA     WCOM3         T2(36)
077900040202     C**!!!              MOVEA     '-'           T2(34)
078000040202     c                   MOVEA     '-'           T2(39)
078100940715     C                   MOVEL     'TR'          KCOD
078200991112     C                   MOVEL     ARBCTR        unoctr            1
078300991112     c                   movel(P)  unoctr        kkey
078400940715     C     KTAB1         CHAIN     TABEL00F                           99
078500940715     C  N99              MOVEL     TBLUNI        DSTR
078600991112     C   99              clear                   DSTR
078700040202     C**!!!              MOVEA     §TRDES        T2(35)
078800040202     c                   MOVEA     §TRDES        T2(40)
078900991112     c*
079000040202     C**!!!              MOVEA     arbdiv        T2(60)
079100040202     c                   MOVEA     arbdiv        T2(67)
079200920403     C*
079300940715     C                   MOVEL     *BLANKS       VSCCAM
079400940714     C                   MOVEA     T2            VSCCAM
079500920403     C*
079600991111     C                   ENDif
079700940715     C*
079800940715     C*?--------- F N A R B G 0 0    ( 97 ) -----------------------------------?
079900940715     C*   CONSEGNA RICHIESTA GIORNI CHIUSURA
080000940715     C   97              DO
080100950111     C                   MOVEA     '   '         CR(30)
080200051128     C                   MOVEA     *BLANKS       SKI(7)
080300051128     C                   MOVEA     SKI(5)        CR(21)
080400051128     c* Verifico tramite fnlv80r se era una tassativa
080500051128     c                   if        arbdcr>0 or arbhcr>0
080600051128     c                   clear                   fnlv80ds
080700051128     c                   eval      ilv80aas=arbaas
080800051128     c                   eval      ilv80lnp=arblnp
080900051128     c                   eval      ilv80nrs=arbnrs
081000051128     c                   eval      ilv80nsp=arbnsp
081100051202     c                   if        partbo='A'
081200051128     c                   eval      ilv80mgs=arbmgs
081300051202     c                   else
081400051202     c                   eval      ilv80mgs=blpmgs
081500051202     c                   endif
081600051128     c                   eval      ilv80tc1=arbtc1
081700051128     c                   eval      ilv80tc2=arbtc2
081800051128     c                   eval      ilv80dcr=arbdcr
081900051128     c                   eval      ilv80hcr=arbhcr
082000051128     c                   eval      ilv80tcr=arbtcr
082100051128     c                   call      'FNLV80R'
082200051128     c                   parm                    kpjba
082300051128     c                   parm                    fnlv80ds
082400051128     c
082500051128     c                   movea     olv80tcrd     cr(21)
082600051128     C                   ENDIF
082700051128     C* DATA
082800051128     c                   if        arbdcr>0
082900051128     C                   MOVEL     ARBDCR        WCOM8
083000051128     C                   MOVEA     WCOM8         SKI(8)
083100051128     C                   MOVEA     SKI(14)       WGG               2
083200051128     C                   MOVEA     SKI(12)       WMM               2
083300051128     C                   MOVEA     SKI(8)        WAA               4
083400051128     C*
083500051128     C                   MOVEA     WGG           CR(33)
083600051128     C                   MOVEA     '/'           CR(35)
083700051128     C                   MOVEA     WMM           CR(36)
083800051128     C                   MOVEA     '/'           CR(38)
083900051128     C                   MOVEA     WAA           CR(39)
084000051128     c                   else
084100051128     C                   MOVEA     *BLANKS       SKI(6)
084200051128     C                   MOVEA     SKI(6)        CR(33)
084300051128     c                   endif
084400051128     c* ORA
084500051128     c                   if        arbhcr>0
084600051128     C                   MOVEL     ARBHCR        WCOM4
084700051128     C                   MOVEA     WCOM4         SKI(12)
084800051128     C                   MOVEA     SKI(12)       CR(45)
084900051128     C                   MOVEA     ':'           CR(47)
085000051128     C                   MOVEA     SKI(14)       CR(48)
085100051128     C                   ELSE
085200051128     C                   MOVEA     *BLANKS       SKI(11)
085300051128     C                   MOVEA     SKI(11)       CR(45)
085400051128     c                   endif
085500051128     C
085600940715     C* GIORNI CHIUSURA
085700941213     C                   MOVEA     ARBGC1        CR(74)
085800941213     C                   MOVEA     ARBGC2        CR(78)
085900941212     C*
086000941212     C*------------------------------
086100941212     C* TIPO CONSEGNA PARTICOLARE (1)
086200941212     C                   MOVEL     *BLANKS       §1PDES
086300941213     C                   MOVEL     ' '           CR(113)
086400941231    3C     ARBTC1        IFNE      *BLANKS
086500941212     C                   MOVEL     '1P'          KCOD
086600941212     C                   MOVEL     *BLANKS       KKEY
086700941212     C                   MOVEL     ARBTC1        KKEY
086800941212     C     KTAB1         CHAIN     TABEL00F                           99
086900941212     C  N99              MOVEL     TBLUNI        DS1P
087000941213     C                   MOVEL     '-'           CR(113)
087100941231    3C                   ENDIF
087200941213     C                   MOVEA     ARBTC1        CR(111)
087300941213     C                   MOVEA     §1PDES        CR(115)
087400941212     C*------------------------------
087500941212     C* TIPO CONSEGNA PARTICOLARE (2)
087600941212     C                   MOVEL     *BLANKS       §1PDES
087700941213     C                   MOVEL     ' '           CR(193)
087800941231    3C     ARBTC2        IFNE      *BLANKS
087900941212     C                   MOVEL     '1P'          KCOD
088000941212     C                   MOVEL     *BLANKS       KKEY
088100941212     C                   MOVEL     ARBTC2        KKEY
088200941212     C     KTAB1         CHAIN     TABEL00F                           99
088300941212     C  N99              MOVEL     TBLUNI        DS1P
088400941213     C                   MOVEL     '-'           CR(193)
088500941231    3C                   ENDIF
088600941213     C                   MOVEA     ARBTC2        CR(191)
088700941213     C                   MOVEA     §1PDES        CR(195)
088800940715     C*
088900940715     C                   MOVEL     *BLANKS       VSCCAM
089000941231     C                   MOVEA     CR            VSCCAM
089100941231    2C                   END
089200941231     C*
089300941231     C*?--------- F N A R B M 0 0    ( 91 ) -----------------------------------?
089400941231     C*** VARIAZ DESTINATARIO
089500941231    2C   91              DO
089600941231     C     ARBCCM        IFGT      0
089700941231     C                   MOVEL     ARBCCM        WCOM7
089800941231     C                   ELSE
089900941231     C                   CLEAR                   WCOM7
090000941231     C                   ENDIF
090100941231     C                   MOVEA     WCOM7         BM(11)
090200941231     C                   MOVEA     ARBRSM        BM(20)
090300941231     C                   MOVEA     ARBINM        BM(81)
090400941231     C                   MOVEA     ARBCAM        BM(118)
090500941231     C                   MOVEA     ARBLOM        BM(129)
090600941231     C                   MOVEA     ARBPRM        BM(167)
090700941231     C                   MOVEA     ARBNZM        BM(171)
090800941231     C                   MOVEA     ARBFAP        BM(188)
090900941231     C                   MOVEA     ARBCPI        BM(207)
091000941231     C                   MOVEA     BM            VSCCAM
091100941231     C                   ENDDO
091200940715     C* -----------------------------------------------------------------------
091300920403     C*
091400920403     C* GIRO DATA VARIAZIONE
091500940714     C                   Z-ADD     ARBDTV        G02INV
091600940714     C                   Z-ADD     *ZEROS        G02DAT
091700940714     C                   MOVEL     '3'           G02ERR
091800940714     C                   CALL      'XSRDA8'
091900940714     C                   PARM                    WLBDAT
092000940714     C                   Z-ADD     G02DAT        VSCDTV
092100060307     C                   Z-ADD     ARBDTV        vshdtv
092200940714     C                   MOVEL     ARBORV        VSCORV
092300060307     C                   MOVEL     ARBORV        VShORV
092400940714     C                   MOVEL     ARBCVB        VSCCVB
092500920402     C* CAUSALE VARIAZIONE
092600060306     c                   clear                   ds7l
092700940714     C                   MOVEL     '7L'          KCOD
092800940714     C                   MOVEL     *BLANKS       KKEY
092900940714     C                   MOVEL     VSCCVB        KKEY
093000940715     C     KTAB1         CHAIN     TABEL00F                           99
093100940714     C* RIEMPO CAMPO VSDCVB
093200060306     c  n99              movel     tbluni        ds7l
093300060306     c                   clear                   vsdcvb
093400060306     c                   clear                   vsdcvbw
093500060306     c                   eval      vsdcvb = arbcvb + '-' + §7ldes
093600060306     c                   eval      vsdcvbw = arbcvb + '-' + §7ldes
093700060306     c
093800060306     C**                 MOVEL     *BLANKS       WWW
093900060306     C**N99              MOVEA     TBLUNI        WWW(4)
094000060306     C** 99              MOVEA     *BLANKS       WWW(4)
094100060306     C**                 MOVEA     VSCCVB        WWW(1)
094200060306     C**                 MOVEA     '-'           WWW(3)
094300060306     C**                 MOVEA     WWW           VSDCVB
094400060306     C**                 MOVEA     WWW           VSDCVBw
094500920402     C*
094600060306     c* verifico se per la variazione esiste motivo
094700060306     c                   exsr      ges_mtv
094800060306     c*
094900940714     C                   WRITE     LR75S01
095000940714     C                   ADD       1             NRR1
095100920402     C*
095200991111     C                   SETOFF                                       9594
095300941231     C                   SETOFF                                       969792
095400130927     C                   SETOFF                                       9188
095500130927     C     KARB1         READE     FiARBF2C                               31
095600920402     C                   END
095700940715     C*
095800940714     C     NRR1          IFNE      1
095900940714     C                   Z-ADD     1             NRR1
096000940714     C                   Z-ADD     1             REC1
096100060306     c                   if        wesmtv = '1'
096200060307     c                   eval      intesta = is(2)
096300060306     c                   endif
096400920402     C     FORCT3        TAG
096500940714     C                   WRITE     LR75Z01
096600940714     C                   EXFMT     LR75C01
096700060306     c* se in almeno una variazione è presente il motivo devo leggere il
096800060306     c* subfile per visualizzare il motivo se richiesto
096900060306     c                   if        wesmtv = '1'
097000060306     c                   if        *inkc = '0' and *inkl = '0'
097100060306     c                   exsr      vis_mtv
097200060306     c                   goto      forct3
097300060306     c                   endif
097400060306     c                   endif
097500940802     C*
097600940715     C                   ENDIF
097700930524     C*
097800940802     C   KC              MOVEL     '1'           PARF03
097900940802     C   KC              MOVEL     PARAM         KPJBU
098000940802     C*
098100941222     C     FINE          TAG
098200051128     C  nlr              RETURN
098300051128     c                   if        *inlr
098400051128     c                   clear                   fnlv80ds
098500051128     c                   eval      ilv80tla = 'C'
098600051128     c                   call      'FNLV80R'
098700051128     c                   parm                    kpjba
098800051128     c                   parm                    FNLV80DS
098900060307     c*
099000060307     C                   CLEAR                   tibs02ds
099100060307     C                   MOVEL     'C'           T02TLA
099200060307     C                   CALL      'TIBS02R'
099300060307     C                   PARM                    KPJBA
099400060307     C                   PARM                    tibs02ds
099500051128     c                   endif
099600940714     C                   END
099700991112     C**************************************************************************
099800991112     C*  riempie varie a video
099900991112     C**************************************************************************
100000991112     C     rievar        BEGSR
100100991112     C*
100200991112     c                   select
100300991112     c     wvaria        wheneq    1
100400991112     C                   MOVEA     wcom10        BT(120)
100500991112     C                   MOVEA     wsvn          BT(119)
100600991112     c     wvaria        wheneq    2
100700991112     C                   MOVEA     wcom10        BT(132)
100800991112     C                   MOVEA     wsvn          BT(131)
100900991112     c     wvaria        wheneq    3
101000991112     C                   MOVEA     wcom10        BT(144)
101100991112     C                   MOVEA     wsvn          BT(143)
101200991112     c     wvaria        wheneq    4
101300991112     c                   MOVEA     wcom10        BT(156)
101400991112     C                   MOVEA     wsvn          BT(155)
101500991112     c     wvaria        wheneq    5
101600991112     C                   MOVEA     wcom10        BT(168)
101700991112     C                   MOVEA     wsvn          BT(167)
101800991112     c                   endsl
101900991112     c                   endsr
102000041130     C**************************************************************************
102100041130     C*  riempie varie a video seconda bolla
102200041130     C**************************************************************************
102300041130     C     rievar1       BEGSR
102400041130     C*
102500041130     c                   select
102600041130     c     wvaria        wheneq    1
102700041130     C                   MOVEA     wcom10        T2(89)
102800041130     C                   MOVEA     wsvn          T2(88)
102900041130     c     wvaria        wheneq    2
103000041130     C                   MOVEA     wcom10        T2(101)
103100041130     C                   MOVEA     wsvn          T2(100)
103200041130     c     wvaria        wheneq    3
103300041130     C                   MOVEA     wcom10        T2(113)
103400041130     C                   MOVEA     wsvn          T2(112)
103500041130     c     wvaria        wheneq    4
103600041130     c                   MOVEA     wcom10        T2(125)
103700041130     C                   MOVEA     wsvn          T2(124)
103800041130     c     wvaria        wheneq    5
103900041130     C                   MOVEA     wcom10        T2(137)
104000041130     C                   MOVEA     wsvn          T2(136)
104100041130     c     wvaria        wheneq    6
104200041130     C                   MOVEA     wcom10        T2(149)
104300041130     C                   MOVEA     wsvn          T2(148)
104400041130     c                   endsl
104500041130     c                   endsr
104600940714     C**************************************************************************
104700940714     C*  ABBLANCA ZERO NON SIGNIFICATIVI
104800940714     C**************************************************************************
104900920403     C     ABBL          BEGSR
105000940714     C*
105100940715     C                   SETOFF                                       99
105200920403     C     SKI(I)        DOWEQ     '0'
105300920403     C                   MOVEL     ' '           SKI(I)
105400920403     C                   ADD       1             I
105500941214     C     I             COMP      15                                     99
105600940715     C  N99              END
105700940714     C*
105800920403     C                   ENDSR
105900940714     C**************************************************************************
106000940714     C*  RIEMPIMENTO CAMPI DEL FORMATO LR75C01 (SUBFILE CONTROL)
106100940714     C**************************************************************************
106200940714     C     RIEC01        BEGSR
106300950629     C* BOLLA IN PARTENZA
106400950629    1C     PARTBO        IFEQ      'P'
106500941222     C                   SETOFF                                       10
106600950629    2C     WAPRP         IFEQ      ' '
106700941222     C                   MOVEL     '1'           WAPRP             1
106800941222     C                   OPEN      FNBLP01L
106900950629    2C                   ENDIF
107000941222     C*
107100950629   X1C                   ELSE
107200950629     C* BOLLA IN ARRIVO
107300941222     C                   SETON                                        10
107400950629    2C     WAPRA         IFEQ      ' '
107500941222     C                   MOVEL     '1'           WAPRA             1
107600941222     C                   OPEN      FNARB01L
107700950629    2C                   ENDIF
107800950629    1C                   ENDIF
107900941222     C*
108000940714     C* AGGANCIO LA BOLLA PER I DATI DI TESTATA
108100941222     C     *IN10         IFEQ      *ON
108200941222     C     KARB1         CHAIN     FNARB01L                           99
108300940714     C*
108400941222     C   99              GOTO      ENDRIE
108500941222     C*
108600940714     C                   Z-ADD     ARBDSP        G02INV
108700940714     C                   Z-ADD     *ZEROS        G02DAT
108800940714     C                   MOVEL     '3'           G02ERR
108900940714     C                   CALL      'XSRDA8'
109000940714     C                   PARM                    WLBDAT
109100940714     C                   Z-ADD     G02DAT        VCCDSP
109200941221     C*
109300940714     C                   MOVEL     '3A'          KCOD
109400940714     C                   MOVEL     *BLANKS       KKEY
109500940718     C                   MOVEL     ARBCBO        KKEY
109600940714     C     KTAB1         CHAIN     TABEL00F                           99
109700940714     C   99              CLEAR                   DS3A
109800940714     C  N99              MOVEL     TBLUNI        DS3A
109900940715     C                   MOVEL     *BLANKS       WWW
110000940715     C                   MOVEA     §3ADES        WWW(4)
110100940718     C                   MOVEA     ARBCBO        WWW(1)
110200940715     C                   MOVEA     '-'           WWW(3)
110300940715     C                   MOVEA     WWW           VCDCBO
110400941221     C*
110500940714     C                   MOVEL     ARBRSD        VCCRSD
110600940714     C                   MOVEL     ARBIND        VCCIND
110700940714     C                   MOVEL     ARBLOD        VCCLOD
110800940714     C                   MOVEL     ARBPRD        VCCPRD
110900940714     C                   Z-ADD     ARBKSC        VCCKSC
111000941215     C                   Z-ADD     ARBPKF        VCCPKF
111100941215     C                   Z-ADD     ARBVLF        VCCVLF
111200941215     C                   MOVEL     ARBFVF        VCCFVF
111300941221     C*
111400940714     C                   MOVEL     *BLANKS       VCDTC1
111500940714     C     ARBTC1        IFNE      *BLANKS
111600940714     C                   MOVEL     '1P'          KCOD
111700940714     C                   MOVEL     *BLANKS       KKEY
111800940714     C                   MOVEL     ARBTC1        KKEY
111900940714     C     KTAB1         CHAIN     TABEL00F                           99
112000940715     C                   MOVEL     *BLANKS       WWW
112100940715     C  N99              MOVEA     TBLUNI        WWW(3)
112200940715     C                   MOVEL     ARBTC1        WWW(1)
112300940715     C                   MOVEL     '-'           WWW(2)
112400940715     C                   MOVEA     WWW           VCDTC1
112500940714     C                   ENDIF
112600940714     C                   MOVEL     *BLANKS       VCDTC2
112700940714     C     ARBTC2        IFNE      *BLANKS
112800940714     C                   MOVEL     '1P'          KCOD
112900940714     C                   MOVEL     *BLANKS       KKEY
113000940714     C                   MOVEL     ARBTC2        KKEY
113100940714     C     KTAB1         CHAIN     TABEL00F                           99
113200940715     C                   MOVEL     *BLANKS       WWW
113300940715     C  N99              MOVEA     TBLUNI        WWW(3)
113400940715     C                   MOVEL     ARBTC2        WWW(1)
113500940715     C                   MOVEL     '-'           WWW(2)
113600940715     C                   MOVEA     WWW           VCDTC2
113700940714     C                   END
113800941221     C* C/ASSEGNO
113900051114     C     KARB1         CHAIN     FiAR901L                           99
114000941221     C     *IN99         IFEQ      *OFF
114100940802     C                   MOVEL     AR9TIC        VCCTIC
114200940802     C                   Z-ADD     AR9CAS        VCCCAS
114300940802     C                   MOVEL     AR9VCA        VCCVCA
114400991110     C     VCCCAS        IFGT      *ZEROS
114500991110     C     VCCVCA        ANDEQ     *BLANKS
114600991110     C                   MOVEL     'ITL'         VCCVCA
114700991110     C                   ENDIF
114800941221     C                   ELSE
114900941221     C                   CLEAR                   VCCTIC
115000941221     C                   CLEAR                   VCCCAS
115100941221     C                   CLEAR                   VCCVCA
115200941221     C                   ENDIF
115300941221     C*
115400940714     C                   Z-ADD     ARBVMD        VCCVMD
115500940714     C                   MOVEL     ARBVAD        VCCVAD
115600991110     C     VCCVMD        IFGT      *ZEROS
115700991110     C     VCCVAD        ANDEQ     *BLANKS
115800991110     C                   MOVEL     'ITL'         VCCVAD
115900991110     C                   ENDIF
116000940714     C                   Z-ADD     ARBIAS        VCCIAS
116100940714     C                   MOVEL     ARBVAS        VCCVAS
116200991110     C     VCCIAS        IFGT      *ZEROS
116300991110     C     VCCVAS        ANDEQ     *BLANKS
116400991110     C                   MOVEL     'ITL'         VCCVAS
116500991110     C                   ENDIF
116600940714     C                   MOVEL     'TR'          KCOD
116700940714     C                   MOVEL     *BLANKS       KKEY
116800940714     C                   MOVEL     ARBCTR        UNOCTR            1
116900940714     C                   MOVEL     UNOCTR        KKEY
117000940714     C     KTAB1         CHAIN     TABEL00F                           99
117100940715     C                   MOVEL     TBLUNI        DSTR
117200940715     C                   MOVEL     *BLANKS       WWW
117300940715     C                   MOVEA     §TRDES        WWW(5)
117400940715     C                   MOVEL     ARBCTR        WCOM3
117500940715     C                   MOVEA     WCOM3         WWW(1)
117600940715     C                   MOVEA     '-'           WWW(4)
117700940715     C                   MOVEA     WWW           VCDCTR
117800051104     c                   select
117900051104     c                   when      arbffd='S'
118000051104     C                   MOVEL     'SI '         VCCFFD
118100051104     c                   when      arbffd='M'
118200051104     C                   MOVEL     'SAB'         VCCFFD
118300051104     C                   other
118400051104     C                   MOVEL     '   '         VCCFFD
118500051104     C                   ENDsl
118600940714     C*
118700940714     C* AGGANCIO LA TASSAZIONE PER GLI ALTRI DATI DI TESTATA
118800991111     c                   exsr      rietas
118900941222     C*
119000941222     C                   ELSE
119100941222     C     KARB1         CHAIN     FNBLP01L                           99
119200941222     C*
119300941222     C   99              GOTO      ENDRIE
119400941222     C*
119500941222     C                   Z-ADD     BLPDSP        G02INV
119600941222     C                   Z-ADD     *ZEROS        G02DAT
119700941222     C                   MOVEL     '3'           G02ERR
119800941222     C                   CALL      'XSRDA8'
119900941222     C                   PARM                    WLBDAT
120000941222     C                   Z-ADD     G02DAT        VCCDSP
120100941222     C*
120200941222     C                   MOVEL     '3A'          KCOD
120300941222     C                   MOVEL     *BLANKS       KKEY
120400941222     C                   MOVEL     BLPCBO        KKEY
120500941222     C     KTAB1         CHAIN     TABEL00F                           99
120600941222     C   99              CLEAR                   DS3A
120700941222     C  N99              MOVEL     TBLUNI        DS3A
120800941222     C                   MOVEL     *BLANKS       WWW
120900941222     C                   MOVEA     §3ADES        WWW(4)
121000941222     C                   MOVEA     BLPCBO        WWW(1)
121100941222     C                   MOVEA     '-'           WWW(3)
121200941222     C                   MOVEA     WWW           VCDCBO
121300941222     C*
121400941222     C                   MOVEL     BLPRSD        VCCRSD
121500941222     C                   MOVEL     BLPIND        VCCIND
121600941222     C                   MOVEL     BLPLOD        VCCLOD
121700941222     C                   MOVEL     BLPPRD        VCCPRD
121800941222     C                   Z-ADD     BLPKSC        VCCKSC
121900941222     C                   Z-ADD     BLPPKF        VCCPKF
122000941222     C                   Z-ADD     BLPVLF        VCCVLF
122100941222     C                   MOVEL     BLPFVF        VCCFVF
122200941222     C*
122300941222     C                   MOVEL     *BLANKS       VCDTC1
122400941222     C     BLPTC1        IFNE      *BLANKS
122500941222     C                   MOVEL     '1P'          KCOD
122600941222     C                   MOVEL     *BLANKS       KKEY
122700941222     C                   MOVEL     BLPTC1        KKEY
122800941222     C     KTAB1         CHAIN     TABEL00F                           99
122900941222     C                   MOVEL     *BLANKS       WWW
123000941222     C  N99              MOVEA     TBLUNI        WWW(3)
123100941222     C                   MOVEL     BLPTC1        WWW(1)
123200941222     C                   MOVEL     '-'           WWW(2)
123300941222     C                   MOVEA     WWW           VCDTC1
123400941222     C                   ENDIF
123500941222     C                   MOVEL     *BLANKS       VCDTC2
123600941222     C     BLPTC2        IFNE      *BLANKS
123700941222     C                   MOVEL     '1P'          KCOD
123800941222     C                   MOVEL     *BLANKS       KKEY
123900941222     C                   MOVEL     BLPTC2        KKEY
124000941222     C     KTAB1         CHAIN     TABEL00F                           99
124100941222     C                   MOVEL     *BLANKS       WWW
124200941222     C  N99              MOVEA     TBLUNI        WWW(3)
124300941222     C                   MOVEL     BLPTC2        WWW(1)
124400941222     C                   MOVEL     '-'           WWW(2)
124500941222     C                   MOVEA     WWW           VCDTC2
124600941222     C                   END
124700941222     C* C/ASSEGNO
124800051118     C     KARB1         CHAIN     Fiar901L                           99
124900941222     C     *IN99         IFEQ      *OFF
125000051118     C                   MOVEL     ar9TIC        VCCTIC
125100051118     C                   Z-ADD     ar9CAS        VCCCAS
125200051118     C                   MOVEL     ar9VCA        VCCVCA
125300991111     c     vcccas        ifgt      *zeros
125400991111     c     vccvca        andeq     *blanks
125500991111     c                   movel     'ITL'         vccvca
125600991111     c                   endif
125700941222     C                   ELSE
125800941222     C                   CLEAR                   VCCTIC
125900941222     C                   CLEAR                   VCCCAS
126000941222     C                   CLEAR                   VCCVCA
126100941222     C                   ENDIF
126200941222     C*
126300941222     C                   Z-ADD     BLPVMD        VCCVMD
126400941222     C                   MOVEL     BLPVAD        VCCVAD
126500991111     c     vccvmd        ifgt      *zeros
126600991111     c     vccvad        andeq     *blanks
126700991111     c                   movel     'ITL'         vccvad
126800991111     c                   endif
126900941222     C                   Z-ADD     BLPIAS        VCCIAS
127000941222     C                   MOVEL     BLPVAS        VCCVAS
127100991111     c     vccias        ifgt      *zeros
127200991111     c     vccvas        andeq     *blanks
127300991111     c                   movel     'ITL'         vccvas
127400991111     c                   end
127500941222     C                   MOVEL     'TR'          KCOD
127600941222     C                   MOVEL     *BLANKS       KKEY
127700941222     C                   MOVEL     BLPCTR        UNOCTR            1
127800941222     C                   MOVEL     UNOCTR        KKEY
127900941222     C     KTAB1         CHAIN     TABEL00F                           99
128000941222     C                   MOVEL     TBLUNI        DSTR
128100941222     C                   MOVEL     *BLANKS       WWW
128200941222     C                   MOVEA     §TRDES        WWW(5)
128300941222     C                   MOVEL     BLPCTR        WCOM3
128400941222     C                   MOVEA     WCOM3         WWW(1)
128500941222     C                   MOVEA     '-'           WWW(4)
128600941222     C                   MOVEA     WWW           VCDCTR
128700051104     c                   select
128800051104     c                   when      blpffd='S'
128900051104     C                   MOVEL     'SI '         VCCFFD
129000051104     c                   when      blpffd='M'
129100051104     C                   MOVEL     'SAB'         VCCFFD
129200051104     C                   other
129300051104     C                   MOVEL     '   '         VCCFFD
129400051104     C                   ENDsl
129500941222     C*
129600991111     c                   exsr      rietas
129700941222     C                   ENDIF
129800941222     C*
129900941222     C                   SETOFF                                       99
130000060306     c                   eval      intesta = is(1)
130100940714     C*
130200941222     C     ENDRIE        ENDSR
130300991111      **********************************************************************
130400991111      * riempio campi del video relativi la tassazione
130500991111      **********************************************************************
130600991111     C     RIETAS        BEGSR
130700991111     C                   MOVEL     '1'           WTRC
130800991111     C     KARB2         CHAIN     FIAR601L                           99
130900991111     C*
131000991111     C     *IN99         IFEQ      *OFF
131100991111     c* divisa
131200991111     C                   MOVEL     AR6DIV        VCCDIV
131300991111     C                   MOVEL     'CV'          KCOD
131400991111     C                   MOVEL(P)  AR6div        KKEY
131500991111     C     KTAB1         CHAIN     TABEL                              31
131600991111     C  N31              MOVEL     TBLUNI        DSCV
131700991111     C   31              clear                   DSCV
131800991111     c* porto
131900991111     c                   z-add     ar6por        w0133
132000991111     c                   movel     §cvfdc        wtstdec
132100991111     c                   exsr      edtimporti
132200991111     c                   move      impeditato    vccpor
132300991111     c*
132400991111     C                   Z-ADD     AR6NFT        VCCNFT
132500991111     C                   Z-ADD     AR6IFT        VCCIFT
132600991111     C                   Z-ADD     AR6DFT        G02INV
132700991111     C                   Z-ADD     *ZEROS        G02DAT
132800991111     C                   MOVEL     '3'           G02ERR
132900991111     C                   CALL      'XSRDA8'
133000991111     C                   PARM                    WLBDAT
133100991111     C                   Z-ADD     G02DAT        VCCDFT
133200991111     c* varia 1
133300991111     C                   MOVEL     AR6SV1        VCCSV1
133400991111     c                   z-add     ar6va1        w0133
133500991111     c                   movel     §cvfdc        wtstdec
133600991111     c                   exsr      edtimporti
133700991111     c                   move      impeditato    vccva1
133800991111     c* varia 2
133900991111     C                   MOVEL     AR6SV2        VCCSV2
134000991111     c                   z-add     ar6va2        w0133
134100991111     c                   movel     §cvfdc        wtstdec
134200991111     c                   exsr      edtimporti
134300991111     c                   move      impeditato    vccva2
134400991111     c* varia 3
134500991111     C                   MOVEL     AR6SV3        VCCSV3
134600991111     c                   z-add     ar6va3        w0133
134700991111     c                   movel     §cvfdc        wtstdec
134800991111     c                   exsr      edtimporti
134900991111     c                   move      impeditato    vccva3
135000991111     C                   ELSE
135100991111     C                   CLEAR                   VCCDIV
135200991111     C                   CLEAR                   VCCPOR
135300991111     C                   CLEAR                   VCCNFT
135400991111     C                   CLEAR                   VCCIFT
135500991111     C                   CLEAR                   VCCDFT
135600991111     C                   CLEAR                   VCCSV1
135700991111     C                   CLEAR                   VCCVA1
135800991111     C                   CLEAR                   VCCSV2
135900991111     C                   CLEAR                   VCCVA2
136000991111     C                   CLEAR                   VCCSV3
136100991111     C                   CLEAR                   VCCVA3
136200991111     C                   ENDIF
136300991111     C*
136400991111     C                   ENDSR
136500991111      **********************************************************************
136600991111      * EDITO IMPORTI PER CAMPI in stampa
136700991111      **********************************************************************
136800991111     C     EDTIMPORTI    BEGSR
136900991111
137000991111      * ------------------------------------------------------------------------
137100991111      * W0133      contiene il numero da editare
137200991111      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il num
137300991111      *            ero ha decimali
137400991111      * IMPEDITATO ritorna il valore editato per la stampa lungo13 (troncate
137500991111      *            cifre oltre 10 mld)
137600991111      * ------------------------------------------------------------------------
137700991111
137800991111     c                   clear                   IMPeditato
137900991111     c                   clear                   W014A
138000991111     c                   if        Wtstdec = 'S'
138100991111     C                   eval      W014a = %EDITC(W0133:'4')
138200991111     c                   else
138300991111     c                   z-add     W0133         W0140
138400991111     C                   eval      W014a = %EDITC(W0140:'4')
138500991111     c                   endif
138600991111     c
138700991111     c                   move      W014a         IMPeditato
138800991111     c
138900991111     c                   ENDSR
139000991111     C*
139100991111      **********************************************************************
139200991111      * EDITO IMPORTI PER CAMPI a video
139300991111      **********************************************************************
139400991111     C     EDTimport122  BEGSR
139500991111
139600991111     c                   clear                   IMPeditato
139700991111     c                   clear                   W013A
139800991111     c                   if        Wtstdec = 'S'
139900991111     C                   eval      W013a = %EDITC(W0122:'4')
140000991111     c                   else
140100991111     c                   z-add     W0122         W0130
140200991111     C                   eval      W013a = %EDITC(W0130:'4')
140300991111     c                   endif
140400991111     c
140500991111     c                   move      W013a         IMPeditato
140600991111     c
140700991111     c                   ENDSR
140800060306     C**************************************************************************
140900060306     C* Impostazione campo per visualizzazione motivo variazione
141000060306     C**************************************************************************
141100060306     C     ges_mtv       BEGSR
141200060306     c                   seton                                        01
141300060306     c                   clear                   vscvmv
141400060307     c                   clear                   vshvmv
141500060307     c                   clear                   vsdvmv
141600060306     c                   eval      wtrd = '§' + arbcvb
141700060307     c     kar5          chain     fiar501l
141800060307     c                   if        %found(fiar501l)
141900060306     c                   setoff                                       01
142000060307     c                   movel     ar5uni        dar5mtv
142100060307     c                   clear                   dmtv
142200060307     C                   CLEAR                   tibs02ds
142300060307     C                   MOVEL     'C'           T02MOD
142400060307     C                   MOVEL     KNSIF         T02SIF
142500060307     C                   MOVEL     'MTV'         T02COD
142600060307     c                   movel(P)  §ar5mtv       t02ke1
142700060307     c                   call      'TIBS02R'
142800060307     C                   PARM                    KPJBA
142900060307     C                   PARM                    tibs02ds
143000060307    2C                   IF        T02ERR = *BLANKS
143100060307     C                   MOVEL     T02uni        dmtv
143200060307    2C                   ENDIF
143300060307     c                   movel     §mtvdes       vsdvmv
143400060307     c                   movel     §mtvdes       vshvmv
143500060306     c                   eval      wesmtv = '1'
143600060306     c                   endif
143700991111     C*
143800060306     c                   endsr
143900060306     C**************************************************************************
144000060306     C* Visualizzazione motivo variazione
144100060306     C**************************************************************************
144200060306     C     vis_mtv       BEGSR
144300060310     c                   setoff                                       01
144400060306     c                   readc     lr75s01
144500060306     c                   dow       %eof = *off
144600060306     c                   if        vscvmv = 'V'
144700060306     c*
144800060306     c                   clear                   vsctitw
144900060306     c                   eval      vsctitw = 'Variazione del ' +
145000060306     c                             %editc(vscdtv:'8') +  ' ore ' +
145100060306     c                             %editw(vscorv : '  :  ') + ' utente ' +
145200060307     c                             vscpru
145300060306     c*
145400060307     c                   eval      wtrd = '§' + %subst(vsdcvb:1:2)
145500060307     c     kar5          chain     fiar501l
145600060307     c                   if        %found(fiar501l)
145700060306     c                   movel     ar5uni        dar5mtv
145800060306     c                   movel     §ar5mtv       vscmtv
145900060306     c* decodifico motivo variazione
146000060307     c                   eval      vsdmtv = vshvmv
146100060306     c                   movel     §ar5txt       vscnt1
146200060306     c                   move      §ar5txt       vscnt2
146300060306     c                   exfmt     lr75wd1
146400060307     c                   endif
146500060306     c                   clear                   vscvmv
146600060306     C                   Z-ADD     NRR1          REC1
146700060306     C                   UPDATE    Lr75S01
146800060306     c                   endif
146900060306     c                   readc     lr75s01
147000060306     c                   enddo
147100060306     C*
147200060306     c                   endsr
147300940714     C**************************************************************************
147400940714     C*  ROUTINE INIZIALE
147500940714     C**************************************************************************
147600940714     C     *INZSR        BEGSR
147700940714     C*
147800940714     C     KARB1         KLIST
147900940714     C                   KFLD                    VCCAAS
148000940714     C                   KFLD                    VCCLNP
148100940714     C                   KFLD                    VCCNRS
148200940714     C                   KFLD                    VCCNSP
148300991110     C     KARB2         KLIST
148400991110     C                   KFLD                    VCCAAS
148500991110     C                   KFLD                    VCCLNP
148600991110     C                   KFLD                    VCCNRS
148700991110     C                   KFLD                    VCCNSP
148800991110     C                   KFLD                    WTRC
148900991112     C     KARBU         KLIST
149000991112     C                   KFLD                    arbAAS
149100991112     C                   KFLD                    arbLNP
149200991112     C                   KFLD                    arbNRS
149300991112     C                   KFLD                    arbNSP
149400991112     C                   KFLD                    WTRC
149500991112     c                   kfld                    arbdtv
149600991112     c                   kfld                    arborv
149700060307     C     KAR5          KLIST
149800060307     C                   KFLD                    arbAAS
149900060307     C                   KFLD                    arbLNP
150000060307     C                   KFLD                    arbNRS
150100060307     C                   KFLD                    arbNSP
150200060307     C                   KFLD                    WTRd
150300060307     c                   kfld                    vshdtv
150400060307     c                   kfld                    vshorv
150500940714     C     KTAB1         KLIST
150600940714     C                   KFLD                    KKUT
150700940714     C                   KFLD                    KCOD
150800940714     C                   KFLD                    KKEY
150900991112     c*
151000940714     C*
151100991111     C     *LIKE         DEFINE    ARbCVB        VSCCVB
151200940714     C     *LIKE         DEFINE    TBLKUT        KKUT
151300940714     C     *LIKE         DEFINE    TBLCOD        KCOD
151400940714     C     *LIKE         DEFINE    TBLKEY        KKEY
151500991110     C     *LIKE         DEFINE    AR6TRC        WTRC
151600060306     C     *LIKE         DEFINE    AR5TRd        WTRd
151700940715     C                   MOVEL     *BLANKS       WCOM3             3
151800940715     C                   MOVEL     *BLANKS       WCOM4             4
151900940715     C                   MOVEL     *BLANKS       WCOM5             5
152000940715     C                   MOVEL     *BLANKS       WCOM6             6
152100940715     C                   MOVEL     *BLANKS       WCOM7             7
152200940715     C                   MOVEL     *BLANKS       WCOM8             8
152300940715     C                   MOVEL     *BLANKS       WCOM9             9
152400991111     C                   MOVEL     *BLANKS       WCOM10           10
152500991112     C                   MOVEL     *BLANKS       WCOM11           11
152600991111     C                   MOVEL     *BLANKS       WCOM12           12
152700940715     C                   MOVEL     *BLANKS       WCOM13           13
152800940715     C                   MOVEL     *BLANKS       WCOM16           16
152900940715     C*
153000940714     C                   Z-ADD     1             KKUT
153100991112     C* CARICO DA SIGLE VARIE
153200991112     C                   MOVEL     '$2'          KCOD
153300991112     C                   MOVEL(p)  '1'           KKEY
153400991112     C     KTAB1         CHAIN     TABEL                              31
153500991112     C  N31              MOVEL     TBLUNI        DS$2
153600991112     C   31              MOVEL     *BLANKS       DS$2
153700071026     c
153800071026      * Aggancio tabella "VPODECO" per reperimento data attivazione
153900071026      * gestione prepagati
154000120510     C****               CLEAR                   tibs02ds
154100120510     c****               clear                   dvpodeco
154200120510     C****               MOVEL     'C'           T02MOD
154300120510     C****               MOVEL     KNSIF         T02SIF
154400120510     C****               MOVEL     'VPO'         T02COD
154500120510     c***                movel(P)  'DECO'        t02ke1
154600120510     C****               CALL      'TIBS02R'
154700120510     C****               PARM                    KPJBA
154800120510     C****               PARM                    tibs02ds
154900120510    2C****               IF        T02ERR = *BLANKS
155000120510     C****               MOVEL     T02uni        dvpodeco
155100120510    2C***                ENDIF
155200071026     C*
155300940714     C*
155400940714     C                   ENDSR
155500940714      **************************************************************************
155600940715      *  1         2         3         4         5         6         7         8
155700940715123456*8901234567890123456789012345678901234567890123456789012345678901234567890
155800941231** ?----  BD  -----------------------------------------------------------------?
155900941212Destinatario: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
156000941212Xxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxx
156100941212Xxxxxxx  xx  xxx   Inoltro: x    F.Deposito: xx    Partita IVA: xxxxxxxxxxxxxxxx
156200940715** ?----  BP  -----------------------------------------------------------------?
156300941212Peso  Kg.: xxxxxx,x                    Volume da Fatturare:  x  xx,xxx
156400941212** ?----  VP  -----------------------------------------------------------------?
156500941212Volume da Fatturare:  x  xx,xxx
156600940715** ?----  BK  -----------------------------------------------------------------?
156700941231Cod.bolla precedente:  xx-xxxxxxxxxxxxxxxxxxxxxxxxx           Cod.bolla nuovo:
156800941231XX-xxxxxxxxxxxxxxxxxxxxxxxxx   Contrassegno precedente:  xx  xxxxxxxxxx,xxx  xxx
156900941214Particolarita' C/Assegno: xx
157000940715** ?----  BC  -----------------------------------------------------------------?
157100941214Codice   Bolla      :  xx-xxxxxxxxxxxxxxxxxxxxxxxxx
157200941221Contrassegno precedente: xx  XXXXXXXXXX,XXX  XXX   Particolarita' C/Assegno: xx
157300940715** ?----  BT  -----------------------------------------------------------------?
157400941215Cli xxxxxxx Tar xxx VMercexxxxxxxxxx,xxx xxx IAsxxxxxxxxxx,xxx xxx Qta'Fatxxxxxx
157500991110xxxx,xxx Divxxx Traspxxxxxxxxxx Varie:xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxx
157600991110xxxxx xxxxxxxxxxx Imp123456789012 Esenz.X Fatt:tot.123456789012 num.xxxxxx xx/xx
157700940715** ?----  T2  -----------------------------------------------------------------?
157800040202Cod.Cliente: xxxxxxx  Cod.Tariffa: xxx-xxxxxxxxxxxxxxxxxxxx  Div: xxx
157900041130Varie: xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
158000040203Importo fat.: xxxxxxxxxxxx  Es.IVA: x-xxxxx  Fat.n.: XXX  xxxxxx del xx/xx/xx
158100940715** ?----  CR  -----------------------------------------------------------------?
158200051128Consegna richiesta: xxxxxxxxxxx xx/xx/xxxx  xx:xx    Giorni di Chiusura: xx  xx
158300941213      Consegna Particolare 1: x x xxxxxxxxxxxxxxxxxxxxxxxxx
158400941213      Consegna Particolare 2: x x xxxxxxxxxxxxxxxxxxxxxxxxx
158500941231** ?----  BM  -----------------------------------------------------------------?
158600941231Mittente: xxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
158700941231xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
158800941231xxx   xx  xxx   Anteporto: x     Partita IVA: xxxxxxxxxxxxxxxx
158900060306** ?----  IS  -----------------------------------------------------------------?
159000060306 Data         Ora     Utente       Causale
159100060307 Data         Ora     Utente       Causale                    Motivo          V
159200060316** ?----  RM  -----------------------------------------------------------------?
159300060316Rif.Mitt.Alfabetico: xxxxxxxxxxxxxxx   Natura Merce: xxxxxxxxxxxxxxx
159400061102** ?----  FI  -----------------------------------------------------------------?
159500071026XXXXXXXXXXXXXXXXXXXXXXX: Codice Fiscale  xxxxxxxxxxxxxxxx
159600071026                            Partita IVA  xxxxxxxxxxxxxxxx
159700130927** ?----  REF - fnarbn0f ------------------------------------------------------?
159800130927Referente Consegna....: xxxxxxxxxxxxxxxxxxxxxxxxx
159900130927Telefono destinatario : xxxxxxxxxxxxxxxx
160000130927** ?----  EML - fnarbn0f ------------------------------------------------------?
160100130927E-mail destinatario...: xxxxxxxxxxxxxxxxxxxxxxxxx
160200130927** ?----  SMS - fnarbn0f ------------------------------------------------------?
160300130927Cellulare destinatario: xxxxxxxxxxxxxxxxxxxxxxxxx
160400130930** ?----  NOT - fnarbn0f ------------------------------------------------------?
160500130930Note bolla:
