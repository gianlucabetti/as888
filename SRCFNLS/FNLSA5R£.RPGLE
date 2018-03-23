000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200970925     H* FNLSA5R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400970930     H*        - RICEZIONE STOP PADRONCINI                           *
000500970930     H*                                                              *
000600970930     H* N.B.: QUESTO PGM CONSIDERA LA POSSIBILITA' DI RICEVERE       *
000700970930     H*       ANCHE STOP DI RITIRO. DI FATTO CIO' NON ACCADRA' MAI   *
000800970930     H*       IN QUANTO GLI STOP DI RITIRO NON VENGONO TRASMESSI MA  *
000900970930     H*       VANNO AD AGGIORNARE DIRETTAMENTE FNBL4 (V.PGM FNLRC1R) *
001000000000     H*--------------------------------------------------------------*
001100970925     FFNBL401L  UF A E           K DISK
001200971002     FFNBLP01L  IF   E           K DISK
001300970925     FFNSTP00R  UF   E           K DISK    USROPN
001400911001     FCMRIC01L  UF   E           K DISK
001500941122     D*
001600020916     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)
001700020916     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)
001800020916     D CMD5            S             48    DIM(1) CTDATA PERRCD(1)
001900020916     D CMDF            S             48    DIM(2) CTDATA PERRCD(1)
002000941122     D*
002100000000     D KPJBA         E DS
002200911001     D  LEGFIL               483    492
002300911001     D  MBRFIL               493    502
002400911001     D CNCR80        E DS
002500911001     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002600970929     D DSBL4A        E DS
002700000000     C*---------------------------------------------------------------*
002800000000     C     *ENTRY        PLIST
002900000000     C                   PARM                    KPJBA
003000941122     C*
003100000000     C                   Z-ADD     1             CODUT
003200911001     C                   CALL      'X§PARUT'
003300911001     C                   PARM                    UT§DSE
003400911001     C                   MOVEL     REC80         CNCR80
003500900618     C*---------------------------------------------------------------*
003600970925     C     *LIKE         DEFINE    BL4TRC        WTRC
003700970925     C*
003800970925     C     KBL4          KLIST
003900970925     C                   KFLD                    STPAAS
004000970925     C                   KFLD                    STPLNP
004100970925     C                   KFLD                    STPNRS
004200970925     C                   KFLD                    STPNSP
004300970925     C                   KFLD                    WTRC
004400971002     C*
004500971002     C     KBLP          KLIST
004600971002     C                   KFLD                    STPAAS
004700971002     C                   KFLD                    STPLNP
004800971002     C                   KFLD                    STPNRS
004900971002     C                   KFLD                    STPNSP
005000970925     C*
005100970925     C                   MOVEL     'A'           WTRC
005200941007     C*---------------------------------------------------------------*
005300941007     C*
005400911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
005500911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
005600971002    1C     MBRFIL        IFEQ      *BLANK
005700911001     C                   MOVEL     '*FIRST'      MBRFIL
005800971002    1C                   END
005900911001     C* MEMBRO LUNGO 3
006000911001     C                   MOVEL     MBRFIL        COMODO            4
006100911001     C                   MOVE      COMODO        MBR               3 0
006200020916     c                   Movel     MbrFil        Comodo7           7
006300020916     c                   Move      Comodo7       Mbr6              6 0
006400911001     C* SE SONO ME STESSA NON SCRIVO CMRIC
006500971002    1C     MBR           IFEQ      SIMFEL
006600911001     C                   SETON                                        03
006700971002    1C                   ENDIF
006800911001     C*
006900911001     C* ALLOCO MEMBRI
007000911001     C                   EXSR      CAEX
007100911001     C   20              GOTO      FINE
007200911001     C*
007300970925     C                   READ      FNSTP00R                               31
007400941007     C*
007500941007    1C     *IN31         DOWEQ     *OFF
007600911001     C*
007700970929     C                   CLEAR                   DSBL4A
007800971002     C                   CLEAR                   NOWRT
007900970925     C     KBL4          CHAIN     FNBL401L                           21
008000971002     C* SCRIVO NUOVO RECORD IN BL4 SOLO SE ESISTE IL RELATIVO IN BLP
008100971002    2C     *IN21         IFEQ      *ON
008200971002     C     KBLP          SETLL     FNBLP01L                               30
008300971002     C  N30              MOVE      'N'           NOWRT             1
008400971002    2C                   END
008500971002     C*
008600971002    2C     NOWRT         IFNE      'N'
008700970929     C  N21              MOVEL     BL4NOT        DSBL4A
008800971002    3C     STPTSR        IFEQ      'R'
008900970929     C                   Z-ADD     STPSET        §B4ASR
009000970925     C                   ELSE
009100970929     C                   Z-ADD     STPSET        §B4ASC
009200971002    3C                   END
009300970929     C                   MOVEL     DSBL4A        BL4NOT
009400971002    3C     *IN21         IFEQ      *ON
009500970925     C                   Z-ADD     STPAAS        BL4AAS
009600970925     C                   Z-ADD     STPLNP        BL4LNP
009700970925     C                   Z-ADD     STPNRS        BL4NRS
009800970925     C                   Z-ADD     STPNSP        BL4NSP
009900970925     C                   MOVEL     WTRC          BL4TRC
010000970925     C                   WRITE     FNBL4000
010100971002   X3C                   ELSE
010200970925     C                   UPDATE    FNBL4000
010300971002    3C                   END
010400971002    2C                   END
010500941122     C*
010600970925     C* DELETO RECORD RICEVUTO
010700970925     C                   DELETE    FNSTP000
010800911001     C                   ADD       1             NUMREC            7 0
010900941122     C*
011000970925     C                   READ      FNSTP00R                               31
011100941007    1C                   ENDDO
011200911001     C*
011300930112     C* CHIUSURA MEMBRO
011400970925     C                   CLOSE     FNSTP00R
011500911001     C*
011600911001     C* AGGIORNA ARCHIVIO DI LOG SE NON SONO ME STESSO
011700941007    1C  N03              DO
011800911001     C     LEGFIL        CHAIN     CMRIC01L                           20
011900911001     C  N20              Z-ADD     NUMREC        RICNR1
012000911001     C  N20              MOVE      '1'           RICFL2
012100911001     C  N20              UPDATE    CMRIC000
012200941007    1C                   END
012300900911     C*
012400941123     C* PRIMA DI USCIRE DAL PGM: DLCOBJ FILE E DLTOVR
012500900409     C     FINE          TAG
012600941123     C*
012700971002     C                   MOVE      VAR12         CMDF(1)
012800971002     C                   MOVEL     *BLANKS       COMMAN
012900971002     C                   MOVEA     CMDF(1)       COMMAN
013000941123     C                   CALL      'QCMDEXC'                            20
013100941123     C                   PARM                    COMMAN
013200941123     C                   PARM                    LUNG
013300941123     C*
013400941123     C                   MOVEL     *BLANKS       COMMAN
013500941123     C                   MOVEL     CMDF(2)       COMMAN
013600941123     C                   CALL      'QCMDEXC'                            20
013700941123     C                   PARM                    COMMAN
013800941123     C                   PARM                    LUNG
013900941123     C*
014000000000     C                   SETON                                        LR
014100950104     C**************************************************************************
014200950104     C*  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI
014300950104     C**************************************************************************
014400911001     C     CAEX          BEGSR
014500950104     C*
014600020916     C**!!!              MOVE      *BLANK        VAR11             4
014700020916     C**!!!              MOVE      *BLANK        VAR12             5
014800020916     c                   Move      *blanks       Var11             7
014900020916     c                   Move      *blanks       Var12             8
015000911001     C                   MOVE      ')'           VAR11
015100020916     C**!!!              MOVEL     MBR           VAR11
015200020916     c                   Movel     Mbr6          Var11
015300911001     C                   MOVE      '))'          VAR12
015400020916     C**!!!              MOVEL     MBR           VAR12
015500020916     c                   Movel     Mbr6          Var12
015600020916     C                   Z-ADD     48            LUNG             15 5
015700911001     C*
015800911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
015900911001     C                   SETOFF                                       20
016000970925     C                   MOVE      VAR11         CMD1
016100911001     C                   MOVEL     *BLANKS       COMMAN
016200911001     C                   MOVEA     CMD1(1)       COMMAN           80
016300911001     C                   CALL      'QCMDEXC'                            20
016400911001     C                   PARM                    COMMAN
016500911001     C                   PARM                    LUNG
016600911001     C* ALLOCO MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
016700941007    1C  N20              DO
016800911001     C                   SETOFF                                       20
016900911001     C                   MOVE      VAR12         CMD2
017000911001     C                   MOVEL     *BLANKS       COMMAN
017100911001     C                   MOVEA     CMD2(1)       COMMAN
017200911001     C                   CALL      'QCMDEXC'                            20
017300911001     C                   PARM                    COMMAN
017400911001     C                   PARM                    LUNG
017500911001     C* OVRDBF
017600941007    2C  N20              DO
017700911001     C                   SETOFF                                       20
017800911002     C                   MOVE      VAR11         CMD5
017900911001     C                   MOVEL     *BLANKS       COMMAN
018000911002     C                   MOVEA     CMD5(1)       COMMAN
018100911001     C                   CALL      'QCMDEXC'                            20
018200911001     C                   PARM                    COMMAN
018300911001     C                   PARM                    LUNG
018400911001     C*
018500941007    3C     *IN20         IFEQ      *OFF
018600970925     C                   OPEN      FNSTP00R
018700911001     C*
018800941007    3C                   ENDIF
018900941007     C*
019000941007    2C                   END
019100941007    1C                   END
019200950104     C*
019300911001     C                   ENDSR
019400950315     O**************************************************************************
019500941123**         CMD1
019600020916CHKOBJ OBJ(FNSTP00R) OBJTYPE(*FILE) MBR(M000000)
019700911001**         CMD2
019800020916ALCOBJ OBJ((FNSTP00R *FILE *EXCL       M000000))
019900911002**         CMD5
020000020916OVRDBF FILE(FNSTP00R)               MBR(M567890)
020100941123**         CMDF    COMANDI DI FINE PROGRAMMA
020200020916DLCOBJ OBJ((FNSTP00R *FILE *EXCL       M000000))
020300941123DLTOVR FILE(*ALL)
