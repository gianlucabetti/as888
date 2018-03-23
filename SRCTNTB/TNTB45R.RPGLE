000100010628     H DECEDIT('0,') DATEDIT(*YMD.)
000200010628      ****************************************************************
000300010628      *  RIEPILOGO INDICATORI
000400010628      ****************************************************************
000500010628      * 01 - RECORD ANNULLATO
000600010628      * 02 - *ON VARIAZIONE - *OFF IMMISSIONE
000700010628      * 28 - ERRORE GENERICO DSPF
000800010628      * 31 - GENERICO
000900010628      * 90 - RIEMISSIONE VIDEATA
001000010628      ****************************************************************
001100010628     FTNTBE01L  UF A E           K DISK
001200040625     Fazorg01L  iF   E           K DISK
001300040625     FAZCAE05L  IF   E           K DISK
001400040625     FTNTB45D   CF   E             WORKSTN
001500010628      *------------------------------------------------------------------------*
001600040625     D KCOD            S                   LIKE(TBECOD) inz('SOT')
001700010628     D KKE1            S                   LIKE(TBEKE1)
001800010628      *   S C H I E R E
001900040625     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MSG VIDEO
002000010628     D*
002100010628     D TIBS02DS      E DS
002200010628     D UT§DSE0F      E DS
002300010628     D*
002400010628     D KPJBA         E DS
002500010628     D*
002600040625     d kepa            s                   Like(caeepa)
002700040625     d wtfe            s                   Like(caetfe)
002800010628     D*------------------------------------------------------------------------*
002900010628     C                   DO        *HIVAL
003000010628     C* Pulizia 1° Videata
003100010628     C                   IF        (*IN28 = *OFF AND *IN90 = *OFF)
003200040625     C                   MOVE      *BLANKS       V1Ctfa
003300040625     C                   MOVE      '?'           V1Ctfa
003400010628     C                   ENDIF
003500010628     C*
003600010628      * Emissione 1° Videata
003700040625     C                   EXFMT     TNTB451
003800010628      * F3=Fine
003900010628     C   KC              LEAVE
004000010628     C*
004100010628      * Controlli 1° videata
004200010628     C                   SETOFF                                       28
004300010628     C                   EXSR      SR_CONTR01
004400010628     C*
004500010628     C   28              ITER
004600010628     C   90              ITER
004700010628     C*
004800010628      * Caricamento e pulizia seconda videata
004900010628     C                   EXSR      SR_CARVID
005000010628     C*
005100010628      * Emissione 2° Videata
005200010628     C     RIEMETTO      TAG
005300040625     C                   EXFMT     TNTB452
005400010628     C*
005500010628     C                   EVAL      *IN28 = *OFF
005600010705     C                   EVAL      *IN15 = *OFF
005700010628     C*
005800010628      * F3=Fine
005900010628     C   KC              LEAVE
006000010628     C*
006100010628      * F12=Ritorno
006200010628     C   KL              ITER
006300010628     C*
006400010628     C                   EXSR      SR_CONTR02
006500010628     C   28              GOTO      RIEMETTO
006600010628     C*
006700010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
006800010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
006900010629     C                   EXSR      SR_REGISTRA
007000010628     C                   ITER
007100010628     C                   ENDIF
007200010628     C*
007300010628     C                   GOTO      RIEMETTO
007400010628     C*
007500010628     C                   ENDDO
007600040625     c*
007700040625     c                   clear                   tibs02ds
007800040625     c                   movel     'C'           t02tla
007900040625     c                   call      'TIBS02R'
008000040625     c                   parm                    kpjba
008100040625     c                   parm                    tibs02ds
008200010628     C*
008300010628     C                   EVAL      *INLR = *ON
008400010628      **********************************************************************
008500010628      * CONTROLLI VIDEO 1
008600010628      **********************************************************************
008700010628     C     SR_CONTR01    BEGSR
008800010628      * Reset indicatori
008900010628     C                   SETOFF                                       90
009000040625     C     '?'           SCAN      V1Ctfa                                 31
009100010628     C     *IN31         IFEQ      *ON
009200010628     C*
009300010628     C                   EVAL      *IN90 = *ON
009400010628     C                   CLEAR                   TIBS02DS
009500010628     C                   MOVEL     'R'           T02MOD
009600010628     C                   MOVEL     KNSIF         T02SIF
009700040625     C                   MOVEL     'SOT'         T02COD
009800010628     C                   CALL      'TIBS02R'
009900010628     C                   PARM                    KPJBA
010000010628     C                   PARM                    TIBS02DS
010100010628     C                   IF        T02ERR = *BLANKS
010200040625     C                   MOVEL     T02KE1        V1Ctfa
010300040625     c                   else
010400040625     c                   clear                   v1ctfa
010500010628     C                   SETON                                        90
010600010628     C                   ENDIF
010700010628     C                   ENDIF
010800010628
010900010628      * controllo se campo valorizzato
011000040625     C                   IF        V1Ctfa = *ZEROS OR V1Ctfa = *BLANKS
011100010628     C                   SETON                                        28
011200010628     C                   MOVEL     MSG(1)        V1CMSG
011300010628     C                   GOTO      ENDSRCONTR
011400010628     C                   ENDIF
011500010628     C*
011600040625     C* il ter-arr deve essere effettivamente un ter-arr
011700040625     c     v1ctfa        ifne      'GEN'
011800040625     c                   move      v1ctfa        wtfe
011900040625     c                   exsr      sr_terarr
012000040625     c     wtarr         ifeq      *blanks
012100040625     C                   SETON                                        28
012200040625     C                   MOVEL     MSG(2)        V1CMSG
012300040625     C                   GOTO      ENDSRCONTR
012400040625     c                   endif
012500040625     c                   endif
012600040504     c
012700010628     C*
012800010628     C     ENDSRCONTR    endsr
012900010628      **********************************************************************
013000010628      * CARICAMENTO SECONDA VIDEATA
013100010628      **********************************************************************
013200010628     C     SR_CARVID     BEGSR
013300010628     C*
013400040504     C                   SETOFF                                       01
013500040625     C                   MOVE      V1Ctfa        V2Ctfa
013600040625     C                   clear                   V2dtfa
013700040625     c*
013800040625     c     v1ctfa        ifne      'GEN'
013900040625     c                   move      v1ctfa        w0030             3 0
014000040625     c     w0030         chain     azorg01l
014100040625     c                   if        %found(azorg01l)
014200040625     c                   eval      v2dtfa=orgdes
014300040625     c                   endif
014400040625     c                   else
014500040625     c                   movel     'Generico'    v2dtfa
014600040625     c                   endif
014700040625     c*
014800040625     C                   MOVEL(p)  V1Ctfa        KKE1
014900040625     C     KTNTBE        CHAIN     TNTBE01L
015000040625     c                   if        %found(tntbe01l)
015100040625     C                   MOVEL     TBEUNI        v1chso
015200040625     c                   seton                                        02
015300010629     C                   ELSE
015400040625     c                   z-add     0800          v1chso
015500040504     C                   SETOFF                                       02        IMMISSIONE
015600010628     C                   ENDIF
015700010628     C*
015800010628     C                   MOVEL     *BLANKS       V2VAIM                         VARIAZ./IMMISSIONE
015900010628     C*
016000010628     C                   IF        *IN02 = *ON                                  VARIAZIONE
016100010628     C                   IF        TBEATB = *BLANKS
016200010628     C                   MOVEL     'VARIAZIONE'  V2VAIM
016300010628     C                   ELSE
016400010628     C                   MOVEL     'ANNULLATO '  V2VAIM
016500010628     C                   SETON                                        01        ANNULLATO
016600010628     C                   ENDIF
016700010628     C                   ELSE                                                   IMMISSIONE
016800010628     C                   MOVEL     'IMMISSIONE'  V2VAIM
016900010628     C                   ENDIF
017000010628     C*
017100010628     C                   ENDSR
017200010628      **********************************************************************
017300010628      * CONTROLLI VIDEO 2
017400010628      **********************************************************************
017500010628     C     Sr_Contr02    begsr
017600010628      * Reset indicatori
017700040625     C                   SETOFF                                       28
017800040625      * ora soglia
017900040625     C                   IF        V1Chso = *ZEROS
018000040503     C                   SETON                                        2840
018100010628     C                   MOVEL     MSG(4)        V2CMSG
018200010628     C                   ENDIF
018300010628     C*
018400040625     C                   ENDSR
018500010628      **********************************************************************
018600010628      * REGISTRAZIONE DATI IMMESSI
018700010628      **********************************************************************
018800010628     C     SR_REGISTRA   BEGSR
018900010628
019000040504
019100040625     C                   MOVEL(P)  v1chso        TBEUNI
019200040504     C                   MOVE      'S'           TBEFTT
019300040504     C                   MOVE      *BLANKS       TBEFTR
019400040504      * immissione
019500040625     C                   IF        *IN02 = *Off
019600010628     C                   MOVE      *BLANKS       TBEATB
019700040625     C                   MOVEL     'SOT'         TBECOD
019800040625     C                   MOVEL     V2CTFA        TBEKE1
019900010628     C                   WRITE     TNTBE000
020000010628     C                   ELSE
020100040504      * variazione
020200040625     c   ke              clear                   tbeatb
020300040625     c   kq              movel     'A'           tbeatb
020400010628     C                   UPDATE    TNTBE000
020500010628     C                   ENDIF
020600040504
020700040625     C                   ENDSR
020800040625      *------------------------------------------------------------------------*
020900040625      *    Verifica se p.o. è ter-arr
021000040625      *------------------------------------------------------------------------*
021100040625     c     Sr_TerArr     BegSr
021200040625
021300040625     C                   MOVEL     'A'           KEPA
021400040625     C                   MOVE      *BLANKS       WTARR             1
021500040625     C     KCAE5         SETLL     AZCAE05L
021600040625     C     KCAE5         READE     AZCAE05L                               30
021700040625     C     *IN30         DOWEQ     *OFF
021800040625     C     WTARR         ANDEQ     *BLANKS
021900040625     C     CAEATB        IFEQ      *BLANKS
022000040625     C     *date         ANDGE     CAEDDE
022100040625     C     *date         ANDLE     CAEDSC
022200040625     C                   MOVEL     '1'           WTARR
022300040625     C                   END
022400040625     C     KCAE5         READE     AZCAE05L                               30
022500040625     C                   ENDDO
022600040625
022700040625     c                   endsr
022800010628      *****************************************************************
022900010628      * ROUTINE INIZIALE
023000010628      *****************************************************************
023100010628     C     *INZSR        BEGSR
023200010628      *
023300010628     C     *ENTRY        PLIST
023400010628     C                   PARM                    KPJBA
023500010628     C*
023600010628     C                   Z-ADD     1             CODUT
023700010628     C                   CALL      'X§PARUT'
023800010628     C                   PARM                    UT§DSE0F
023900010628     C                   MOVEL     RAGUT         RSUT
024000010628     C*
024100010628     C     KTNTBE        KLIST
024200010628     C                   KFLD                    KCOD
024300010628     C                   KFLD                    KKE1
024400040625     C     KCAE5         KLIST
024500040625     C                   KFLD                    KEPA
024600040625     C                   KFLD                    wtfe
024700010628     C*
024800010628     C                   endsr
024900010628      *---------------------------------------------------------------------------------------------
025000010628** MSG  Lungh. 78                                                            *
025100040625Inserire il terminal di arrivo                                                1
025200040625Terminal di arrivo errato                                                     2
025300040625Ora di soglia obbligatoria                                                    4
