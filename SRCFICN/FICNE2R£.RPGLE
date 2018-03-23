000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011114     H* FICNE2R *----------------------------------------------------*
000300931209     H*           SVALORIZZAZIONE DISTINTA PADRONCINI                *
000400000000     H*--------------------------------------------------------------*
000500011114     Ffiftt01L  UF   E           K DISK
000600011114     Ffiftd03L  UF   E           K DISK
000700020403     Ffifce01L  UF   E           K DISK
000800930707     F*
000900931129     D PARAM           DS
001000931202     D* CODICE PADRONCINO
001100931202     D  PARPDR                 1      7  0
001200931209     D* TIPO PRESTAZIONE
001300931209     D  PARTSR                 8      8
001400950104     D* NUMERO DISTINTA CONSEGNA/RITIRI
001500931209     D  PARNDC                 9     15  0
001600931209     D* PULIZIA CODICE TARIFFA DETTAGLIO (S/N)
001700931209     D  PARFCT                16     16
001800931209     D* PULIZIA VALORI MANUALI DETTAGLIO (S/N)
001900931209     D  PARFVD                17     17
002000931209     D* PULIZIA VALORI MANUALI TESTATA   (S/N)
002100931209     D  PARFVT                18     18
002200950113     D* DATA   DISTINTA CONSEGNA/RITIRI
002300950113     D  PARDDC                19     26  0
002400020418     D* SIMULATO
002500020418     D  PARSML               256    256
002600931129     D*
002700931202     D KPJBA         E DS
002800900521     C*****************************************************************
002900000000     C     *ENTRY        PLIST
003000000000     C                   PARM                    KPJBA
003100931130     C                   MOVEL     KPJBU         PARAM
003200931209     C*---------------------------------------------------------------*
003300020403     C     *like         define    fttfgs        kfgs
003400931209     C* ACCESSO FLFTT01L
003500931209     C     KFTT          KLIST
003600931202     C                   KFLD                    PARPDR
003700931202     C                   KFLD                    PARTSR
003800950113     C                   KFLD                    PARNDC
003900950113     C                   KFLD                    PARDDC
004000020403      *
004100020403     C     KFce          KLIST
004200020403     C                   KFLD                    kfgs
004300020403     C                   KFLD                    fttPDR
004400020403     C                   KFLD                    fttTSR
004500020403     C                   KFLD                    fttNDC
004600020403     C                   KFLD                    fttDDC
004700931116     C*---------------------------------------------------------------*
004800931209     C***
004900931209     C**  S V A L O R I Z Z O   F L F T T 0 0 F
005000931209     C***
005100011114     C     KFTT          CHAIN     fiftt000                           30
005200931209     C*
005300931209    1C     *IN30         IFEQ      *OFF
005400931209     C                   EXSR      SVLFTT
005500931209     C*
005600011114     C                   UPDATE    fiftt000
005700931209    1C                   ENDIF
005800931209     C*
005900931209     C***
006000931209     C**  S V A L O R I Z Z O   F L F T D 0 0 F
006100931209     C***
006200011114     C     KFTT          SETLL     fiftd000
006300011114     C     KFTT          READE     fiftd000                               30
006400931202     C*
006500931209    1C     *IN30         DOWEQ     *OFF
006600931202     C                   EXSR      SVLFTD
006700931209     C*
006800011114     C                   UPDATE    fiftd000
006900931130     C*
007000011114     C     KFTT          READE     fiftd000                               30
007100931209    1C                   ENDDO
007200931202     C*
007300020403     C***
007400020403     C**  S V A L O R I Z Z O   F I F C E 0 1 L
007500020403     C***
007600020418     C     PARSML        IFNE      'S'
007700020403     C                   move      fttfgs        kfgs
007800020403     C*
007900020403     c                   if        kfgs = 0
008000020403     C                   movel     fttPDR        kfgs
008100020403     c                   end
008200020403     C* deve cancellare tutti i records per documento
008300020403     C     KFce          SETLL     fifce000
008400020403     C     KFce          READE     fifce000                               30
008500020403     C*
008600020403    1C     *IN30         DOWEQ     *OFF
008700020403     C                   delete    fifce000
008800020403     C     KFce          READE     fifce000                               30
008900020403    1C                   ENDDO
009000020403     C*
009100020418    1C                   END
009200020418     C*
009300931027     C                   SETON                                        LR
009400931111     C*
009500931209     C*--- SVALORIZZO CAMPI DI FLFTT00F ------------------------------*
009600931202     C     SVLFTT        BEGSR
009700020403     C*
009800931209     C                   CLEAR                   FTTFVL
009900931209     C                   CLEAR                   FTTFLA
010000931209     C                   CLEAR                   FTTSNA
010100931209     C                   CLEAR                   FTTCLA
010200950117     C                   CLEAR                   FTTCPE
010300950117     C                   CLEAR                   FTTSNE
010400931209     C                   CLEAR                   FTTSNP
010500931209     C                   CLEAR                   FTTKFA
010600931209     C                   CLEAR                   FTTTVL
010700020422     C                   CLEAR                   FTTftr
010800020422     C                   CLEAR                   FTTdtr
010900931209     C*
011000931209     C* PARFVT = 'N'  ===>  PULIRE I VALORI INSERITI MANUALMENTE SOLO
011100931209     C*                     SE  FTTFIT  E  FTTFIA  SONO VUOTI
011200931209    1C     PARFVT        IFNE      'S'
011300931209     C*
011400931209     C     FTTFIT        IFEQ      ' '
011500931209     C                   CLEAR                   FTTITT
011600931209     C                   ENDIF
011700931209     C     FTTFIA        IFEQ      ' '
011800931209     C                   CLEAR                   FTTITA
011900931209     C                   ENDIF
012000931209     C*
012100931209   X1C                   ELSE
012200931209     C* PARFVT = 'S'  ===>  PULIRE SEMPRE I VALORI INSERITI MANUALMENTE
012300931209     C*
012400931209     C                   CLEAR                   FTTITT
012500931209     C                   CLEAR                   FTTFIT
012600931209     C                   CLEAR                   FTTITA
012700931209     C                   CLEAR                   FTTFIA
012800931209    1C                   ENDIF
012900931209     C*
013000020403     C* attenzione se il record è stato trasmesso a sede
013100020403     c     fttftr        ifeq      'T'
013200020403     C                   CLEAR                   fttftr
013300051221     C*******            CLEAR                   fttdtr
013400020403     C* Deve scrivere un record nel file di trasmissione sul membro
013500020403     C*  da mandare in sede per segnalare l'avvenuta svalorizzazione
013600020403     c                   end
013700020403     C*
013800931209     C                   ENDSR
013900931209     C*
014000931209     C*--- SVALORIZZO CAMPI DI FLFTD00F ------------------------------*
014100931202     C     SVLFTD        BEGSR
014200931209     C*
014300931209     C* PARFCT = 'S'  ===>  PULIRE IL CODICE TARIFFA PADRONCINO
014400931209     C     PARFCT        IFEQ      'S'
014500931209     C                   CLEAR                   FTDCTP
014600931209     C                   ENDIF
014700931209     C*
014800931209     C* PARFVD = 'N'  ===>  PULIRE I VALORI INSERITI MANUALMENTE SOLO
014900931209     C*                     SE  FTDFIT  E  FTDFIA  SONO VUOTI
015000931209    1C     PARFVD        IFNE      'S'
015100931209     C*
015200931209     C     FTDFIT        IFEQ      ' '
015300931209     C                   CLEAR                   FTDITT
015400931209     C                   ENDIF
015500931209     C     FTDFIA        IFEQ      ' '
015600931209     C                   CLEAR                   FTDITA
015700931209     C                   ENDIF
015800931209     C*
015900931209   X1C                   ELSE
016000931209     C* PARFVD = 'S'  ===>  PULIRE SEMPRE I VALORI INSERITI MANUALMENTE
016100931209     C*
016200931209     C                   CLEAR                   FTDITT
016300931209     C                   CLEAR                   FTDFIT
016400931209     C                   CLEAR                   FTDITA
016500931209     C                   CLEAR                   FTDFIA
016600931209    1C                   ENDIF
016700931202     C                   ENDSR
