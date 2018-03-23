000100980402     H DECEDIT('0,') DATEDIT(*DMY.)
000200980402     F* LANCIO TEST ETICHETTE
000300980318     D DSBLD         E DS
000400980318     D DSBLDE        E DS
000500010802     C     *ENTRY        PLIST
000600010802     C                   PARM                    nompgm           10
000700990323     C                   DO        1
000800980318     C                   CLEAR                   DSBLDE
000900991231     C                   CLEAR                   DSBLD
001000991231     C                   EXSR      FISSI
001100010802     C                   CALL      NOMPGM
001200991231     C                   PARM                    DSBLD
001300991231     C                   PARM                    DSBLDE
001400990316     C                   ENDDO
001500980210     C                   SETON                                        LR
001600980402     C*
001700980402     C     FISSI         BEGSR
001800980402     C                   MOVE      *BLANK        §LDCDP
001900980402     C                   MOVE      *BLANK        §LDDLA
002000980402     C                   MOVE      *BLANK        §LDRSM
002100980402     C                   MOVE      *BLANK        §LDRSD
002200980402     C                   MOVE      *BLANK        §LDIND
002300980402     C                   MOVE      *BLANK        §LDLOD
002400980402     C*
002500010516     C                   Z-ADD     053           §LDLNP
002600980402     C                   MOVE      *BLANK        §LDDLP
002700980402     C                   MOVEL     'BOLOGNA '    §LDDLP
002800980402     C                   MOVEL     'A1B2C3..'    §LDCDP
002900980402     C                   MOVE      '...AAAAA'    §LDCDP
003000011113     C                   Z-ADD     06            §LDNRS
003100011113     C                   Z-ADD     6234          §LDNCD
003200010516     C                   Z-ADD     100           §LDLNA
003300010719     C                   MOVEL     'IMOLA  '     §LDDLA
003400000118     C                   Z-ADD     043           §LDTNA
003500010516     C                   MOVE      'E'           §LDTSP
003600010516     C                   Z-ADD     02            §LDZSC
003700980402     C                   Z-ADD     1             §LDNC1
003800980402     C                   Z-ADD     1             §LDNC2
003900020508     C                   MOVEL     'MMMMMMMMMMMM'§LDRSM
004000020508     C                   MOVE      'MMMMMMMMMMMM'§LDRSM
004100980402     C                   MOVEL     'TO'          §LDPRM
004200010516     C                   MOVEL     'DESTINAT'    §LDRSD
004300010706     C                   MOVE      '....FINE'    §LDRSD
004400010516     C                   MOVEL     '99999  '     §LDCAD
004500010516     C     'VIA ITAL'    CAT       'IA, 16  '    §LDIND
004600010706     C                   MOVE      'A GARE  '    §LDIND
004700020110     C                   MOVEL     'IMOLA'       §LDLOD
004800010706     C                   MOVE      '..A MARE'    §LDLOD
004900020110     C                   EVAL      §LDLOD = 'CASALE MONFERRATO'
005000010706     C                   MOVEL     'CA'          §LDPRD
005100980402     C                   Z-ADD     0             §LDCPD
005200980402     C                   Z-ADD     0             §LDNCP
005300980402     C                   Z-ADD     0             §LDNCU
005400980402     C                   MOVEL     'R'           §LDRST
005500980402     C                   Z-ADD     12,123        §LDVOL
005600980402     C                   Z-ADD     123456,1      §LDPKG
005700980402     C                   MOVEL     'P'           §LDFVR
005800010516     C                   MOVEL     '2001'        §LDAAS
005900010516     C                   MOVEL     '0516'        §LDMGS
006000980402     C*
006100040422     C                   MOVEL     'EUR'         §DETSE
006200040422     C                   MOVEL     'EEX'         §DETSE
006300010629     C                   MOVEL     'X'           §DESSE
006400010629     C                   MOVEL     'V1/00'       §DEXX1
006500010629     C                   MOVE      '74001'       §DEXX1
006600010629     C                   MOVE      *ALL'9'       §DEXX2
006700010629     C                   MOVEL     'MIL'         §DESIP
006800010629     C                   MOVEL     'D  '         §DESIA
006900981117     C* ATTIVANDO IL CAMPO §DEAWB STAMPA EUROPOLITAN
007000010629     C                   MOVEL     '4930519'     §DEAWB
007100010629     C                   MOVE      '021'         §DEAWB
007200010629     C                   MOVEL     'FR'          §DEISA
007300010629     C                   MOVEL     'X'           §DEFCA
007400980402     C*
007500010629     C     'JUTIT001'    CAT       '32000002'    §AA16            16
007600010629     C     '9 55L700'    CAT       '32FR    '    §AB16            16
007700010629     C                   MOVE      '        '    §AA19            19
007800010629     C                   MOVEL     §AB16         §AA19
007900010629     C                   MOVEL     §AA16         §DEEUR
008000010629     C                   MOVE      §AA19         §DEEUR
008100010706     C*                  MOVE      *BLANK        §DEEUR
008200010706     C*                  MOVE      '        '    §AA12            12
008300010706     C*                  MOVEL     '83781000'    §AA12
008400010706     C*                  MOVE      '0018'        §AA12
008500010706     C*                  MOVEL     §AA12         §DEEUR
008600980402     C*
008700010629     C                   MOVEL     'ALL'         §DETST
008800980402     C                   ENDSR
