000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200950731     H*       - CONTROLLO SE ESISTONO BOLLE DA TRASMETTERE      -    *
000300950731     H*         CON MAND.INTROITO NON DEL GIORNO                     *
000400950802     H*         E RISOTTOMETTE LA TRASMISS.                          *
000500911014     H*--------------------------------------------------------------*
000600100709     FFNARB72L  UF   E           K DISK
000700100709     FFNBLP01L  IF   E           K DISK
000800911014     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
000900950731     D CNCR80        E DS
001000941025     D WLBDA8          DS
001100941025     D  G08DAT                 1      8  0
001200941025     D  G08INV                 9     16  0
001300941025     D  G08ERR                17     17
001400941025     D  G08TGI                18     22  0
001500911112      *
001600950801     D KPJBA         E DS
001700911014     C*---------------------------------------------------------------*
001800100715     C* prima di trasmettere verifico se esistono spedizioni che in BLP
001900100715     C* sono già aggiornate con un immagine + recente, questo perchè ci
002000100715     C* possono essere sped."mamma" chiuse ma trasmesse solo dopo aver
002100100715     C* consegnato la "figlia". Ciò sporcherebbe i dati di consegna.
002200100715     C                   EXSR      ARBCSR1
002300100715      *
002400100715     C                   EXSR      ARBCSR
002500930415      *
002600911015     C                   SETON                                        LR
002700930421     C***--------------------------------------------***
002800950731     C* LETTURA X NMI NON TRASMESSO
002900930421     C***--------------------------------------------***
003000100715     C     ARBCSR1       BEGSR
003100930421     C                   SETOFF                                       10
003200020925     C     *LOVAL        SETGT     FNARB72L
003300930421     C*
003400930421     C     *IN10         DOUEQ     '1'
003500020925     C                   READ      FNARB72L                             1510
003600950801     C  N15
003700950801     CANN10ARBNMI        IFGT      0
003800950801     C     ARBDMI        ANDLT     AMGUDT
003900100709     C                   IF        ARBDCM <> 0 AND ARBCCA <> ' '
004000100709     C     KBLP          CHAIN     FNBLP01L
004100100709     c                   if        %found(fnblp01l) and
004200100709     c                             ((arbddc = blpddc and arbndc = 9999999
004300100709     c                             and blpndc <> 0 )
004400100709     c                             or arbddc < blpddc)
004500100709     C                   MOVE      'T'           ARBFT1
004600100709     C                   Z-ADD     AMGUDT        ARBDT1
004700100709     c                   update    fnarb000
004800100709     c                   iter
004900100709     c                   end
005000100709     c                   end
005100100715     C                   ENDIF
005200950731     C                   ENDDO
005300950801      *
005400930421     C                   ENDSR
005500100715     C***--------------------------------------------***
005600100715     C* LETTURA X NMI NON TRASMESSO
005700100715     C***--------------------------------------------***
005800100715     C     ARBCSR        BEGSR
005900100715     C                   SETOFF                                       10
006000100715     C     *LOVAL        SETGT     FNARB72L
006100100715     C*
006200100715     C     *IN10         DOUEQ     '1'
006300100715     C                   READ      FNARB72L                             1510
006400100715     C  N15
006500100715     CANN10ARBNMI        IFGT      0
006600100715     C     ARBDMI        ANDLT     AMGUDT
006700100715     C                   MOVEL     ARBNMI        NMI7A            10
006800100715     C                   MOVE      ARBIPP        COM03             3 0
006900100715     C                   MOVE      COM03         NMI7A
007000100715     C                   MOVEL     NMI7A         KPJBU
007100100715     C                   MOVEL     'LR90'        KCOAZ
007200100715     C                   CALL      'BCH10'
007300100715     C                   PARM                    KPJBA
007400100715      *
007500100715     C                   ENDIF
007600100715      *
007700100715     C     KARB          SETGT     FNARB72L
007800100715     C                   ENDDO
007900100715      *
008000100715     C                   ENDSR
008100941025     C*****************************************************************
008200941025     C*     I N Z S R
008300941025     C*****************************************************************
008400941025     C     *INZSR        BEGSR
008500950802     C     *ENTRY        PLIST
008600950802     C                   PARM                    KPJBA
008700941025     C                   Z-ADD     1             CODUT
008800941025     C                   CALL      'X§PARUT'
008900941025     C                   PARM                    UTEDSE
009000941025     C*
009100950801     C                   MOVEL     REC80         CNCR80
009200941025     C*§§
009300950801     C                   TIME                    WHHUDT           14 0
009400950801     C                   MOVEL     WHHUDT        ORA               6 0
009500941025     C                   MOVE      WHHUDT        G08DAT
009600941025     C                   MOVE      *ZEROS        G08INV
009700941025     C                   MOVEL     *BLANK        G08ERR
009800941025     C                   CALL      'XSRDA8'
009900941025     C                   PARM                    WLBDA8
010000941025     C                   Z-ADD     G08INV        AMGUDT            8 0
010100941025     C                   Z-ADD     G08DAT        GMAUDT            8 0
010200020925     C*
010300020925     C     KARB          KLIST
010400020925     C                   KFLD                    ARBIPP
010500020925     C                   KFLD                    ARBNMI
010600100709     C     Kblp          KLIST
010700100709     C                   KFLD                    ARBaas
010800100709     C                   KFLD                    ARBlnp
010900100709     C                   KFLD                    ARBnrs
011000100709     C                   KFLD                    ARBnsp
011100941025     C*
011200941025     C                   ENDSR
