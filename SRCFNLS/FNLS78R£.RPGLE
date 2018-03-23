000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941221     H* FNLS78R *----------------------------------------------------*
000300910219     H*      GESTIONE DETTAGLIO STAMPA SEGNACOLLI
000400000000     H*--------------------------------------------------------------*
000500941221     FFNLS78D   CF   E             WORKSTN
000600941221     F                                     SFILE(LS78DSF:NRR)
000700910219     FAZORG01L  IF   E           K DISK
000800910219     FCNACO00F  IF   E           K DISK
000900020917     FFIVAD01L  UF A E           K DISK
001000020917     FFIVAD02L  UF   E           K DISK
001100020917     F                                     RENAME(FIVAD000:FIVAD002)
001200900523     D PARAM           DS
001300910219     D  VIDKSC               101    107  0
001400941221     D  VI2AAS               108    111  0
001500941221     D  VI2LNP               112    114  0
001600941221     D  VI2NRS               115    116  0
001700941221     D  VI2NSP               117    123  0
001800941221     D  VI2LNA               124    126  0
001900941221     D  VI2FLG               127    127
002000941221     D  KCI                  128    131  0
002100020917     D  VI2FGS               132    134  0
002200900517     D KPJBA         E DS
002300000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002400000000     C*---------------------------------------------------------------*
002500000000     C     *ENTRY        PLIST
002600000000     C                   PARM                    KPJBA
002700000000     C                   Z-ADD     1             CODUT
002800000000     C                   CALL      'XPARUT'
002900000000     C                   PARM                    UTEDSE
003000000000     C                   MOVEL     RAGUT         RSUT             20
003100910219     C*----------------------------------------------------*
003200910205     C* ACCESSO   CNACO                                    *
003300910205     C     KACO          KLIST
003400910205     C                   KFLD                    CODUT
003500910219     C                   KFLD                    KCI
003600910219     C                   KFLD                    VIDKSC            7 0
003700020917     C* ACCESSO   FIVAD                                    *
003800941221     C     KVAD          KLIST
003900020917     C                   KFLD                    VI2FGS
004000020917     C                   KFLD                    VIDKSC
004100910219     C                   KFLD                    VI2AAS
004200910219     C                   KFLD                    VI2LNP
004300910219     C                   KFLD                    VI2NRS
004400910219     C                   KFLD                    VI2NSP
004500910205     C*                                                    *
004600941221     C     KVAD1         KLIST
004700020917     C                   KFLD                    VI2FGS
004800910219     C                   KFLD                    VIDKSC
004900910219     C                   KFLD                    VI2AAS
005000910219     C                   KFLD                    VI2LNP
005100910219     C                   KFLD                    VI2NRS
005200910219     C                   KFLD                    VI2NSP
005300910219     C                   KFLD                    VIDCDP
005400950718     C     KVAD2         KLIST
005500020917     C                   KFLD                    VI2FGS
005600950718     C                   KFLD                    VIDKSC
005700950718     C                   KFLD                    VI2AAS
005800950718     C                   KFLD                    VI2LNP
005900950718     C                   KFLD                    VI2NRS
006000950718     C                   KFLD                    VI2NSP
006100950718     C                   KFLD                    VIDNSC
006200950718     C                   KFLD                    VIDCDP
006300910205     C*---------------------------------------------------------------*
006400900517     C*
006500910219     C                   MOVEL     KPJBU         PARAM
006600910219     C* IMPOSTO CAMPI PER SCRITTURE
006700910219     C                   EXSR      IMPO
006800910219     C*
006900910219     C* DECODIFICA CODICE CLIENTE
007000910219     C     KACO          CHAIN     CNACO000                           30
007100910219     C  N30              MOVEL     ACORAG        DESKSC
007200910219     C   30              MOVEL     *BLANKS       DESKSC
007300910219     C*
007400910220     C* DECODIFICA LINEA DI ARRIVO
007500910219     C     VI2LNA        CHAIN     AZORG                              30
007600910219     C  N30              MOVEL     ORGDES        DESLNA
007700910219     C   30              MOVEL     *BLANKS       DESLNA
007800910219     C*
007900910219     C* CARICO RIGHE DI DETTAGLIO STAMPA BOLLA
008000910219     C                   EXSR      CARBLD
008100910219     C*
008200941221     C                   WRITE     LS78D02
008300900509     C     FOR01         TAG
008400941221     C                   EXFMT     LS78DCT
008500920921     C** CMD12 - RITORNO
008600920921     C   KL              GOTO      FINE
008700910219     C** CMD09-AGGIUNTA RIGHE DETTAGLIO
008800910805     C   KI
008900910805     CANN02              EXSR      AGGRIG
009000910805     C   KI
009100910805     CANN02              GOTO      FOR01
009200910219     C** CONTROLLO
009300910219     C                   EXSR      CONTR
009400910220     C   36              GOTO      FOR01
009500910219     C** CMD16-ANNULLAMENTO
009600910219     C   KQ              EXSR      ANNULL
009700910219     C   KQ              EXSR      CARBLD
009800910219     C** CMD06-AGGIORNAMENTO
009900910219     C   KF              EXSR      AGGIOR
010000930204     C  NKF
010100930204     COR KF
010200930204     CAN 36              GOTO      FOR01
010300910219     C   KF              GOTO      FINE
010400910219     C*
010500000000     C     FINE          TAG
010600000000     C                   SETON                                        LR
010700910205     C*
010800910219     C* CARICO RECORD DELLA BOLLA ------------------------------------*
010900910219     C     CARBLD        BEGSR
011000910205     C                   EXSR      PULSF
011100910205     C                   Z-ADD     0             NRR               4 0
011200020917     C     KVAD          SETLL     FIVAD000                               30
011300910220     C*** I M M I S S I O N E
011400910220     C  N30              Z-ADD     0             SAVNRR
011500910220     C  N30              EXSR      AGGRIG
011600910220     C*
011700910220     C*** M A N U T E N Z I O N E
011800910220     C   30              SETON                                        10
011900910220     C   30              DO        *HIVAL
012000020917     C     KVAD          READE     FIVAD000                               30
012100910219     C*
012200910219     C  N30              DO
012300910219     C                   MOVEL     *BLANKS       VIDSCE
012400941221     C                   MOVEL     VADNCL        VIDNCL
012500941221     C                   MOVEL     VADCDP        VIDCDP
012600941221     C                   MOVEL     VADNCD        VIDNSC
012700941221     C     VADNCD        IFGT      *ZEROS
012800920921     C                   SETON                                        02
012900920921     C                   END
013000941221     C                   Z-ADD     VADNCA        VIDNCA
013100930204     C                   Z-ADD     1             VIDNRR
013200910205     C* SCRIVO SUBFILE
013300910205     C                   ADD       1             NRR
013400941221     C                   WRITE     LS78DSF
013500910205     C                   END
013600910205     C*
013700910205     C  N30              END
013800910219     C                   Z-ADD     NRR           SAVNRR            4 0
013900930204     C                   SETOFF                                       10
014000910205     C*
014100910205     C                   ENDSR
014200910205     C*
014300910205     C* PULIZIA SUBFILE ---------------------------------------------
014400910205     C     PULSF         BEGSR
014500910205     C                   SETON                                        35
014600941221     C                   WRITE     LS78DCT
014700910205     C                   SETOFF                                       35
014800910205     C                   ENDSR
014900910205     C*
015000910219     C* AGGIUNTA RIGHE DI DETTAGLIO COLLI ---------------------------
015100910219     C     AGGRIG        BEGSR
015200930204     C                   SETOFF                                       10
015300910219     C                   Z-ADD     SAVNRR        NRR
015400910220     C                   DO        14
015500910219     C                   MOVEL     *BLANKS       VIDSCE
015600910219     C                   MOVEL     *BLANKS       VIDCDP
015700910219     C                   MOVEL     *ZEROS        VIDNCL
015800910219     C                   MOVEL     *ZEROS        VIDNRR
015900950718     C                   CLEAR                   VIDNSC
016000910219     C* SCRIVO SUBFILE
016100910219     C                   ADD       1             NRR
016200941221     C                   WRITE     LS78DSF
016300910219     C                   END
016400910219     C*
016500910219     C                   Z-ADD     NRR           SAVNRR
016600910205     C                   ENDSR
016700910219     C*
016800910219     C* CONTROLLI ---------------------------------------------------
016900910219     C     CONTR         BEGSR
017000910219     C* NUMERO COLLI OBBLIGATORIO
017100910219     C                   SETOFF                                       3136
017200910219     C                   DO        *HIVAL
017300941221     C                   READC     LS78DSF                                31
017400910219     C*
017500910219     C  N31VIDNRR        IFNE      0
017600930204     C*
017700910219     C* NUMERO COLLI OBBLIGATORIO
017800910219     C     VIDNCL        IFEQ      0
017900930204     C                   SETON                                        4036
018000910219     C                   END
018100910219     C*
018200910219     C                   ELSE
018300910219     C* SE IMMESSO CODICE PRODOTTO E NON NUMERO COLLI ERRORE
018400910219     C     VIDNCL        IFEQ      0
018500930204     C     VIDCDP        ANDNE     *BLANKS
018600910219     C                   SETON                                        4036
018700910219     C                   END
018800910219     C* SE IMMESSO NUMERO COLLI E NON CODICE PRODOTTO ERRORE
018900910219     C     VIDNCL        IFNE      0
019000930204     C     VIDCDP        ANDEQ     *BLANKS
019100910219     C                   SETON                                        4136
019200910219     C                   END
019300910219     C* SE IMMESSI ENTRAMBI CONTROLLO SE GIA' ESISTENTE
019400910219     C     VIDNCL        IFNE      0
019500910219     C     VIDCDP        ANDNE     *BLANKS
019600020917     C     KVAD1         SETLL     FIVAD000                               42
019700910219     C   42              SETON                                        36
019800910219     C                   END
019900910219     C*
020000910219     C                   END
020100910219     C*
020200930204     C  N31VIDNRR        IFNE      0
020300930204     C                   SETON                                        10
020400930204     C                   END
020500930204     C*
020600941221     C  N31              UPDATE    LS78DSF
020700930204     C                   SETOFF                                       10
020800930204     C  N36
020900930204     CANN31              END
021000910219     C*
021100910219     C                   ENDSR
021200910219     C*
021300910219     C* ANNULLAMENTO RIGHE ------------------------------------------
021400910219     C     ANNULL        BEGSR
021500910219     C                   Z-ADD     1             NRR
021600910219     C*
021700910219     C     NRR           DOWLE     SAVNRR
021800941221     C     NRR           CHAIN     LS78DSF                            31
021900910219     C*
022000910219     C  N31VIDSCE        IFEQ      'A'
022100950718     C     VIDNRR        ANDNE     0
022200020917     C     KVAD2         CHAIN     FIVAD002                           32
022300020917     C  N32              DELETE    FIVAD002
022400950718     C                   ENDIF
022500950718     C*
022600910219     C                   ADD       1             NRR
022700950718     C                   ENDDO
022800910219     C*
022900910219     C                   ENDSR
023000910219     C*
023100910219     C* AGGIORNAMENTO RIGHE DETTAGLIO -------------------------------
023200910219     C     AGGIOR        BEGSR
023300910219     C                   EXSR      IMPO
023400910219     C                   Z-ADD     1             NRR
023500910219     C*
023600910219     C     NRR           DOWLE     SAVNRR
023700941221     C     NRR           CHAIN     LS78DSF                            31
023800910219     C*
023900910219     C  N31VIDNCL        IFNE      0
024000930204     C*
024100930204     C* NON E' DA ANNULLARE
024200930204     C     VIDSCE        IFEQ      ' '
024300020917     C     KVAD1         CHAIN     FIVAD000                           32
024400941221     C                   MOVEL     VIDCDP        VADCDP
024500941221     C                   MOVEL     VIDNCL        VADNCL
024600020917     C  N32              UPDATE    FIVAD000
024700020917     C   32              WRITE     FIVAD000
024800930204     C                   Z-ADD     1             VIDNRR
024900930204     C* RECORD DA ANNULLARE
025000930204     C                   ELSE
025100930204     C*
025200930204     C                   SETON                                        4336
025300930204     C                   END
025400930204     C*
025500930204     C     VIDNRR        IFNE      0
025600930204     C                   SETON                                        10
025700930204     C                   ELSE
025800930204     C                   SETOFF                                       10
025900930204     C                   END
026000930204     C*
026100941221     C                   UPDATE    LS78DSF
026200930204     C                   END
026300930204     C*
026400910219     C                   ADD       1             NRR
026500910219     C                   END
026600910219     C*
026700910219     C                   ENDSR
026800910219     C*
026900910219     C* IMPOSTAZIONE CAMPI ------------------------------------------
027000910219     C     IMPO          BEGSR
027100941221     C                   MOVEL     *BLANKS       VADATB
027200941221     C                   MOVEL     VIDKSC        VADCCM
027300941221     C                   MOVEL     VI2AAS        VADAAS
027400941221     C                   MOVEL     VI2LNP        VADLNP
027500941221     C                   MOVEL     VI2NRS        VADNRS
027600941221     C                   MOVEL     VI2NSP        VADNSP
027700941221     C                   MOVEL     *ZEROS        VADNCD
027800941221     C                   MOVEL     *ZEROS        VADNCA
027900910219     C                   ENDSR
