000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500031022      *       TRASCODIFICA BL4 RCD "E" IN VAT RCD "C"                 *
000600031017      *                                                               *
000700031017      *---------------------------------------------------------------*
000800031017
000900161103     fFivab46F  if   e             Disk
001000161103     fazorg01l  if   e           k Disk
001100161103     fQsysprt   o    f  132        Printer Oflind(*InOf)
001200161103     FFNLV22P   O    F  132        PRINTER
001300161103     D DSLV55        E DS                  EXTNAME(FNLV55DS)
001400161103     D DSBS69        E DS                  EXTNAME(TIBS69DS)
001500161103     D DSBLD         E DS
001600161103     D DSBLDE        E DS                  INZ
001700161103     D KPJBA         E DS
001800161103     D DSACO         E DS                  EXTNAME(CNACO00F)
001900161103     D DSIND         E DS                  EXTNAME(CNIND00F)
002000161103     D DSCLP         E DS                  EXTNAME(CNCLP00F)
002100161103     D DSCLS         E DS                  EXTNAME(FNCLS00F)
002200161103     C     *ENTRY        PLIST
002300161103     C                   PARM                    KPJBA
002400031017
002500031021do  1c                   Do        *Hival
002600161103     c                   Read      Fivab46f
002700161103     c                   If        %Eof(Fivab46f)
002800031017     c                   Leave
002900031017     c                   EndIf
003000161103     c                   if        vabnsp>=61552  and vabnsp<=61590
003100161103     c                   exsr      gessnc
003200161103     c                   endif
003300161103     c
003400031017
003500031021    1c                   EndDo
003600161103
003700161103     c                   except    rigaf
003800031017
003900161103     C                   MOVEL     'C'           D55TLA
004000161103     C                   CALL      'FNLV55R'
004100161103     C                   PARM                    DSLV55
004200161103     C*
004300031017     c                   Eval      *InLr = *On
004400161103     c* --------------------
004500161103     C     GESSNC        BEGSR
004600161103     C*
004700161103     C                   Z-ADD     VABNCD        CO2SCD            7 0
004800161103     C     VABNCA        IFEQ      0
004900161103     C                   Z-ADD     VABNCD        VABNCA
005000161103     C                   END
005100161103     C*
005200161103     C* VALORIZZO LA DS PARAMETRO PASSANTE PER PGM STAMPA SEGNACOLLI
005300161103     C*
005400161103     C                   Z-ADD     0             §LDNC1
005500161103     C                   MOVEL     VABLNP        §LDLNP
005600161103     C     VABLNP        CHAIN     AZORG                              56
005700161103    1C     *IN56         IFEQ      *OFF
005800161103    2C     ORGDE5        IFNE      *BLANKS
005900161103     C                   MOVEL     ORGDE5        §LDDLP
006000161103   X2C                   ELSE
006100161103     C                   MOVEL     ORGDES        §LDDLP
006200161103    2C                   END
006300161103   X1C                   ELSE
006400161103     C                   MOVEL     *BLANKS       §LDDLP
006500161103    1C                   END
006600161103     C                   MOVEL     *BLANK        §LDCDP
006700161103     C                   MOVEL     VABNRS        §LDNRS
006800161103     C                   MOVEL     VABLNA        §LDLNA
006900161103     C     VABLNA        CHAIN     AZORG                              56
007000161103     C     *IN56         IFEQ      *OFF
007100161103    2C     ORGDE5        IFNE      *BLANKS
007200161103     C                   MOVEL     ORGDE5        §LDDLA
007300161103     C                   ELSE
007400161103     C                   MOVEL     ORGDES        §LDDLA
007500161103     C                   END
007600161103     C                   ELSE
007700161103     C                   MOVEL     *BLANKS       §LDDLA
007800161103     C                   END
007900161103     C                   CLEAR                   DSLV55
008000161103     C                   MOVEL     'A'           D55TPT
008100161103     C                   MOVE      VABLNA        D55LIN
008200161103     C                   MOVE      VABMGS        D55DRF
008300161103     C                   MOVEL     VABAAS        D55DRF
008400161103     C                   CALL      'FNLV55R'
008500161103     C                   PARM                    DSLV55
008600161103     C                   MOVEL     D55TFA        §LDTNA
008700161103     C                   MOVEL     VABTSP        §LDTSP
008800161103     C                   MOVEL     VABZNC        §LDZSC
008900161103     C                   MOVE      VABNCL        §LDNC2
009000161103     C* REPERISCO DATI ANAGRAIFCI CLIENTE MITTENTE
009100161103     C                   CLEAR                   DSBS69
009200161103     C                   Z-ADD     VABCCM        I69KAC
009300161103     C                   Z-ADD     VABCCM        I69KIN
009400161103     C                   MOVEL     KNSIF         I69SIF
009500161103     C                   CALL      'TIBS69R'
009600161103     C                   PARM                    DSBS69
009700161103     C                   PARM                    DSACO
009800161103     C                   PARM                    DSIND
009900161103     C                   PARM                    DSCLP
010000161103     C                   PARM                    DSCLS
010100161103     C     O69ERR        IFNE      '1'
010200161103     C     ACOFLG        IFEQ      '*'
010300161103     C                   MOVEL     *BLANKS       §LDRSM
010400161103     C                   ELSE
010500161103     C                   MOVEL     ACORAG        §LDRSM
010600161103     C                   END
010700161103     C                   MOVEL     INDPRV        §LDPRM
010800161103     C                   END
010900161103     C                   MOVEL     VABRSD        §LDRSD
011000161103     C                   MOVEL     VABCAD        §LDCAD
011100161103     C                   MOVEL     VABLOD        §LDLOD
011200161103     C                   MOVEL     VABIND        §LDIND
011300161103     C                   MOVEL     VABPRD        §LDPRD
011400161103     C                   MOVEL     999           §LDCPD
011500161103     C                   MOVEL     *ZEROS        §LDNCP
011600161103     C                   MOVEL     *ZEROS        §LDNCU
011700161103     C     VABVLB        IFGT      0
011800161103     C                   MOVEL     'R'           §LDFVR
011900161103     C                   ELSE
012000161103     C                   CLEAR                   §LDFVR
012100161103     C                   ENDIF
012200161103     C                   Z-ADD     VABVLB        §LDVOL
012300161103     C                   Z-ADD     VABPKB        §LDPKG
012400161103     C                   Z-ADD     VABAAS        §LDAAS
012500161103     C                   Z-ADD     VABMGS        §LDMGS
012600161103     C                   MOVEL     'ITA'         §DETST
012700161103     C     CO2SCD        DOWLE     VABNCA
012800161103     C                   ADD       1             §LDNC1
012900161103     C                   Z-ADD     CO2SCD        §LDNCD
013000161103     C*
013100161103     c                   if        co2scd>=1558007
013200161103     C                   CALL      'FNLV2RZS'
013300161103     C                   PARM                    DSBLD
013400161103     C                   PARM                    DSBLDE
013500161103     c                   add       1             staeti            9 0
013600161103     c                   endif
013700161103     C*
013800161103     C                   ADD       1             CO2SCD
013900161103     C*
014000161103     C                   END
014100161103     c                   except    riga
014200161103     C*
014300161103     C                   ENDSR
014400031017
014500161103     oqsysprt   e            Riga           1
014600031017     o                                           +1 'Spedizione:'
014700161103     o                       vabAas              +1
014800161103     o                       vabLnp              +1
014900161103     o                       vabNrs        z     +1
015000161103     o                       vabNsp        z     +1
015100161103     o                       vabncd        z     +1
015200161103     o                       vabnca        z     +1
015300161103     oqsysprt   e            Rigaf          1
015400161103     o                                           +1 'totale:'
015500161103     o                       staeti        z     +1
015600161103     OFNLV22P   T    LRNLR
