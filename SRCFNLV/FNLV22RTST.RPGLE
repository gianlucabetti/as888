000100980402     H DECEDIT('0,') DATEDIT(*DMY.)
000200980402     F* LANCIO TEST ETICHETTE
000300980318     D DSBLD         E DS
000400980318     D DSBLDE        E DS
000500990323     C                   DO        1
000600980318     C                   CLEAR                   DSBLD
000700980318     C                   CLEAR                   DSBLDE
000800980402     C                   EXSR      FISSI
000900980321     C                   CALL      'FNLV22RQ'
001000980318     C                   PARM                    DSBLD
001100980318     C                   PARM                    DSBLDE
001200980327     C                   CLEAR                   DSBLD
001300980327     C                   CLEAR                   DSBLDE
001400980402     C                   EXSR      FISSI
001500980327     C                   CALL      'FNLV22R'
001600980327     C                   PARM                    DSBLD
001700980327     C                   PARM                    DSBLDE
001800980327     C                   CLEAR                   DSBLD
001900980327     C                   CLEAR                   DSBLDE
002000980402     C                   EXSR      FISSI
002100980327     C                   CALL      'FNLVI2R'
002200980327     C                   PARM                    DSBLD
002300980327     C                   PARM                    DSBLDE
002400991231     C                   CLEAR                   DSBLD
002500991231     C                   CLEAR                   DSBLDE
002600991231     C                   EXSR      FISSI
002700991231     C                   CALL      'FNLV22RZ'
002800991231     C                   PARM                    DSBLD
002900991231     C                   PARM                    DSBLDE
003000990316     C                   ENDDO
003100980210     C                   SETON                                        LR
003200980402     C*
003300980402     C     FISSI         BEGSR
003400980402     C                   MOVE      *BLANK        §LDCDP
003500980402     C                   MOVE      *BLANK        §LDDLA
003600980402     C                   MOVE      *BLANK        §LDRSM
003700980402     C                   MOVE      *BLANK        §LDRSD
003800980402     C                   MOVE      *BLANK        §LDIND
003900980402     C                   MOVE      *BLANK        §LDLOD
004000980402     C*
004100980402     C                   Z-ADD     001           §LDLNP
004200980402     C                   MOVE      *BLANK        §LDDLP
004300980402     C                   MOVEL     'BOLOGNA '    §LDDLP
004400980402     C                   MOVEL     'A1B2C3..'    §LDCDP
004500040511     C***                MOVE      '.§.AAAAA'    §LDCDP
004600040511     C                   MOVE      '...AAAAA'    §LDCDP
004700980402     C                   Z-ADD     32            §LDNRS
004800040429     C                   Z-ADD     6234          §LDNCD
004900040429     C                   Z-ADD     195           §LDLNA
005000040429     C                   MOVEL     ' DECLNA '    §LDDLA
005100000118     C                   Z-ADD     043           §LDTNA
005200040429     C                   MOVE      'E'           §LDTSP
005300980402     C                   MOVE      'D'           §LDTSP
005400980402     C                   Z-ADD     04            §LDZSC
005500980402     C                   Z-ADD     1             §LDNC1
005600980402     C                   Z-ADD     1             §LDNC2
005700980402     C                   MOVEL     'MITTENTE'    §LDRSM
005800980402     C                   MOVE      '....FINE'    §LDRSM
005900980402     C                   MOVEL     'TO'          §LDPRM
006000040429     C                   MOVEL     'DESTINAT'    §LDRSD
006100040429     C***                MOVE      '....FINE'    §LDRSD
006200980402     C                   MOVEL     '70032  '     §LDCAD
006300980402     C     'RUE DE L'    CAT       'A GARE  '    §LDIND
006400980402     C                   MOVEL     'PARIS   '    §LDLOD
006500040429     C***                MOVE      '..A MARE'    §LDLOD
006600040429     C***                MOVEL     'CA'          §LDPRD
006700980402     C                   Z-ADD     0             §LDCPD
006800980402     C                   Z-ADD     0             §LDNCP
006900980402     C                   Z-ADD     0             §LDNCU
007000980402     C                   MOVEL     'R'           §LDRST
007100980402     C                   Z-ADD     12,123        §LDVOL
007200980402     C                   Z-ADD     123456,1      §LDPKG
007300980402     C                   MOVEL     'P'           §LDFVR
007400000118     C                   MOVEL     '2000'        §LDAAS
007500000118     C                   MOVEL     '0118'        §LDMGS
007600980402     C*
007700011116     C                   MOVEL     'EUR'         §DETSE
007800040429     C                   MOVEL     'EEX'         §DETSE
007900000118     C                   MOVEL     'X'           §DESSE
008000000118     C                   MOVEL     'V1/00'       §DEXX1
008100000124     C                   MOVE      '74001'       §DEXX1
008200000124     C                   MOVE      *ALL'9'       §DEXX2
008300980402     C                   MOVEL     'MIL'         §DESIP
008400000118     C                   MOVEL     'D  '         §DESIA
008500981117     C* ATTIVANDO IL CAMPO §DEAWB STAMPA EUROPOLITAN
008600000118     C                   MOVEL     '4930519'     §DEAWB
008700000118     C                   MOVE      '021'         §DEAWB
008800990323     C                   MOVEL     'FR'          §DEISA
008900980402     C                   MOVEL     'X'           §DEFCA
009000980402     C*
009100980402     C     'JUTIT001'    CAT       '32000002'    §AA16            16
009200980402     C     '9 55L700'    CAT       '32FR    '    §AB16            16
009300980402     C                   MOVE      '        '    §AA19            19
009400980402     C                   MOVEL     §AB16         §AA19
009500980402     C                   MOVEL     §AA16         §DEEUR
009600980402     C                   MOVE      §AA19         §DEEUR
009700011116     C*                    MOVE *BLANK    §DEEUR
009800011116     C*                    MOVE '        '§AA12  12
009900011116     C*                    MOVEL'83781000'§AA12
010000011116     C*                    MOVE '0018'    §AA12
010100011116     C*                    MOVEL§AA12     §DEEUR
010200980402     C*
010300981117     C                   MOVEL     'ALL'         §DETST
010400980402     C                   ENDSR
