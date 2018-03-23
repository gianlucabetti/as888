000100940222     H DECEDIT('0,') DATEDIT(*DMY.)
000200941215     H* FNLV42R  *----------------------------------------------------*
000300930406     H*------------------*                                            *
000400940222     H*        GESTIONE PARAMETRI STAMPA PER TESTI LETTERE            **
000500921214     H*---------------------------------------------------------------*
000600021024     FFNTXT02L  UF A E           K DISK
000700941215     FFNLV42D   CF   E             WORKSTN
000800921214     D*
000900940210     D PARAM           DS
001000940307     D* CODICE TESTO
001100940210     D  S08TTX                 1      2
001200940218     D*
001300021022     d FNLV41DS      E DS
001400021024     d DSTX          E DS                  EXTNAME(DFNTXTH)
001500930406     D DS1H          E DS
001600921214     D KPJBA         E DS
001700940307     D CNCR80        E DS
001800940307     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001900921214     I*
002000940209     C*****************************************************************
002100940209     C* RIEPILOGO INDICATORI
002200940209     C*****************************************************************
002300940307     C* 15 - PROTEGGO I CAMPI A VIDEO
002400940222     C* 30 - LETTURA TABEL00F
002500940209     C*****************************************************************
002600940209     C     *ENTRY        PLIST
002700940209     C                   PARM                    KPJBA
002800940209     C                   MOVEL     KPJBU         PARAM
002900940210     C                   Z-ADD     1             CODUT             1 0
003000940307     C                   CALL      'X§PARUT'
003100940307     C                   PARM                    UT§DSE
003200940307     C                   MOVEL     REC80         CNCR80
003300940210     C*
003400940209     C*---------------------------------------------------------------*
003500940209     C*
003600021024     C* ACCESSO TnTxt00F
003700021024     C     KTXT          KLIST
003800021024     C                   KFLD                    KLET
003900021024     C                   KFLD                    KPOL
004000021024     C                   KFLD                    KLIN
004100021024     C                   KFLD                    KPOS
004200940210     C*
004300921214     C*---------------------------------------------------------------*
004400021022      *
004500021024      * Disalloco il file testi
004600021024     C                   unlock    FnTxt02l
004700021024      *
004800021022     C                   CLEAR                   Ds1H
004900021022     C                   CLEAR                   Fnlv41ds
005000021024     C                   MOVEL     '2'           I41MOD
005100021022     C                   MOVEL     S08TTX        I41LET
005200021022     C                   CALL      'FNLV41R'
005300021022     C                   PARM                    FNLV41DS
005400021022     C     O41ERR        IFNE      'E'
005500021022     C                   MOVEL     O41DTA        DS1H
005600021022     C                   ELSE
005700021022     C                   GOTO      FINE
005800021022     C                   ENDIF
005900021022     C*
006000921104     C*
006100940222     C* MUOVO CAMPI A VIDEO
006200940222     C                   MOVEL     §1HFV1        VIDSIF
006300940222     C                   MOVEL     §1HFV2        VIDSIC
006400940222     C                   MOVEL     §1HFV3        VIDSCC
006500940222     C                   MOVEL     §1HFV4        VIDSCA
006600940222     C                   MOVEL     §1HFV5        VIDSTC
006700940222     C                   MOVEL     §1HFV6        VIDSDC
006800940222     C                   MOVEL     §1HFV7        VIDSFF
006900940222     C                   MOVEL     §1HFV8        VIDSNP
007000940224     C                   MOVEL     §1HFV9        VIDSRI
007100940330     C                   MOVEL     §1HPVA        VIDPVA
007200940222     C*
007300940307     C* CONTROLLO SE SI TRATTA DI TESTO NON GESTIBILE
007400940307     C     SIMFEL        IFNE      0
007500940307     C* TESTO GESTIBILE SOLO IN SEDE:    CAMPI PROTETTI + CMD6 INIBITO
007600940307     C     §1HFGE        IFEQ      'S'
007700940307     C                   SETON                                        15
007800940307     C                   ENDIF
007900940307     C*
008000940307     C                   ELSE
008100940307     C* TESTO GESTIBILE SOLO IN FILIALE: CAMPI PROTETTI + CMD6 INIBITO
008200940307     C     §1HFGE        IFEQ      'F'
008300940307     C                   SETON                                        15
008400940307     C                   ENDIF
008500940307     C                   ENDIF
008600940307     C*
008700940222     C* EMISSIONE FORMATO
008800941215     C                   WRITE     LV42D02
008900940222     C     FMT01         TAG
009000941215     C                   EXFMT     LV42D01
009100940222     C*
009200940222     C** CMD12 - FINE LAVORO
009300940222     C   KL              GOTO      FINE
009400940222     C*
009500940222     C** ENTER- RIMANE NELLA VIDEATA
009600940222     C  NKF              GOTO      FMT01
009700940222     C*
009800021022     C** CMD6 - AGGIORNAMENTO TESTO (FnTxt00f record tipo H)
009900940222     C                   EXSR      AGGIOR
010000921214     C*
010100921214     C     FINE          TAG
010200921214     C                   SETON                                        LR
010300940222     C*
010400940222     C*--- AGGIORNAMENTO ---------------------------------------------*
010500940222     C     AGGIOR        BEGSR
010600940222     C*
010700021024     c                   move      S08Ttx        Klet              2
010800021024     c                   move      Simfel        KPol              3 0
010900021024     c                   move      'I'           KLin              1
011000021024     c                   move      'H'           KPos              1
011100021024     c                   CLEAR                   DsTx
011200031106      *
011300031106     c     ktxt          chain     FnTxt02l                           21
011400031106     c                   movel     txtrig        dstx
011500031106      *
011600021024      * imposto i campi
011700021024     C                   MOVEL     VIDSIF        §TXSIP
011800021022     C                   MOVEL     VIDSIC        §TXSIC
011900021022     C                   MOVEL     VIDSCC        §TXSCC
012000021022     C                   MOVEL     VIDSCA        §TXSCA
012100021022     C                   MOVEL     VIDSTC        §TXSTC
012200021022     C                   MOVEL     VIDSDC        §TXSDC
012300021024     C                   MOVEL     VIDSFF        §TXSPF
012400021022     C                   MOVEL     VIDSNP        §TXSNP
012500021022     C                   MOVEL     VIDSRI        §TXSRI
012600021022     C                   MOVEL     VIDPVA        §TXPVA
012700021024     c   21              do
012800021024     c                   movel     S08Ttx        TxtLet
012900021024     c                   movel     §1HDes        §TxDes
013000031106     c                   movel     §1Hfts        §TxFts
013100021024     c                   move      Simfel        TxtPol
013200021024     c                   move      'H'           TxtPos
013300021024     c                   move      *zeros        TxtNrr
013400021024     c                   move      *blanks       TxtRib
013500021024     c                   enddo
013600031106      *
013700031106     C                   MOVEL(P)  DSTX          TXTRIG
013800031106      *
013900021024     C   21              write     FnTxt000
014000021024     C  n21              update    FnTxt000
014100940222     C*
014200870610     C                   ENDSR
