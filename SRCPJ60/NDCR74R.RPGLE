000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300951124     H*PARMS CVTOPT(*DATETIME)
000400951124     H DECEDIT('0,') DATEDIT(*DMY.)
000500951124     F*---------------------------------------------------------------*
000600951124     F*             ESTRAZIONE RIEPILOGO VERSAMENTI                   *
000700951124     F*                       MODELLO 770                             *
000800000000     F*---------------------------------------------------------------*
000900951124     F*     INDICATORI                                                *
001000000000     F*                                                               *
001100951124     F*  90 FINE FILE                                                 *
001200000000     F*  50 AZZERO ANNO E MESE DI CORRESPONSIONE QUANDO PER LA STESSA *
001300000000     F*     QUIETANZA NE INCONTRO DIVERSI                             *
001400000000     F* N50 MEMORIZZO ANNO E MESE DI CORRESPONSIONE                   *
001500951124     F*---------------------------------------------------------------*
001600951124     F*  --- FLUSSO PRINCIPALE ---                                    *
001700000000     F*                                                               *
001800000000     F*  - CANCELLAZIONE EVENTUALI RECORD ESISTENTI PER L'ANNO DI     *
001900951124     F*    COMPETENZA SELEZIONATO SU FILE ND77R01L                    *
002000000000     F*                                                               *
002100951124     F*  - LETTURA MOVIMENTI DA ND77W01L IN ORDINE DI ANNO DI COMP.   *
002200951124     F*    FISCALE/QUADRO/DATA E NUMERO QUIETANZA/NUM. DISTINTA       *
002300000000     F*                                                               *
002400890419     F*  - TOTALIZZAZIONE IMPORTI NELL'AMBITO DELLA STESSA DISTINTA   *
002500000000     F*                                                               *
002600951124     F*  - LETTURA NUOVO RECORD ND77W01L                              *
002700000000     F*                                                               *
002800890419     F*  - A ROTTURA DISTINTA ARROTONDO IMPORTO DISTINTA E MEMORIZZO  *
002900890419     F*    NEL TOTALE QUIETANZA                                       *
003000000000     F*                                                               *
003100951124     F*  - A ROTTURA QUIETANZA MEMORIZZO RECORD SU ND77R01L           *
003200890419     F*                                                               *
003300951124     F*  - FINE FILE ND77W01L VAI A FINE                              *
003400000000     F*---------------------------------------------------------------*
003500120516R102 F***ND77W01L  IF   E           K DISK
003600120521R102 FND77W92L  IF A E           K DISK
003700951124     F*------------
003800951124     FND77R01L  UF A E           K DISK
003900951124     D*---------------------------------------------------------------*
004000000000     D KPJBA         E DS
004100951124     D*------------
004200951124     D ndcr70ds      E DS
004300951124     D*------------
004400951124     D W77             DS
004500951127     D  W77SOCIETA             1      3
004600951124     D  W77ACF                 4      6P 0
004700951124     D  W77QUA                 7      8
004800951124     D  W77DQV                 9     18
004900951124     D  W77NQV                19     34
005000951124     D  W77NDT                35     39P 0
005100120516R102 D  W77TRI                        4
005200150626R186 X*** EX R102  W77PTR                        2
005300951124     D*-------------
005400951124     D SAVW77          DS
005500951124     D  SAVSOC                 1      3
005600951124     D  SAVACF                 4      6P 0
005700951124     D  SAVQUA                 7      8
005800951124     D  SAVDQV                 9     18
005900951124     D  SAVNQV                19     34
006000951124     D  SAVNDT                35     39P 0
006100120607R102 D  SAVTRI                        4
006200150626R186 X*** EX R102  SAVPTR                        2
006300951124     D*-------------
006400951124     D MAC             S              4  0 DIM(50)
006500010410C1478D*-------------
006600010410  "  D SOC001        E DS                  EXTNAME(XSOC001DS)
006700010410  "  D*-------------
006800010410  "  D* divisa
006900010410  "  D $CdInter        S                   Like(DVSCdInter)
007000010410  "  D $Divisa         S                   Like(XSCDIV) INZ
007100010410  "  D*-------------
007200010410  "  D* campi di lavoro
007300010410  "  D DvSEsito        S              1  0
007400010410  "  D $Data           S             10
007500010410  "  D W3015           S             30S15
007600010410  "  D NrDec           S              1  0
007700010410  "  D*-------------
007800010410  "  D* Prototipo per la procedura Euro
007900010410  "  D/COPY *LIBL/SRCCPY,PJX002PR
008000040305  "  D*-------------
008100010410  "  D* data odierna
008200010410  "  D UDateISO        S               D
008300010410C1478D*-------------
008400951124     C*---------------------------------------------------------------*
008500951124     C                   EXSR      INZVAR                                       *
008600951124     C*
008700951124     C                   EXSR      DEL77R                                       *
008800951124     C*
008900120516R102 C**** *LOVAL        SETLL     ND77W01L                                     *
009000120516  "  C****               READ      ND77W01L                               90    *
009100120521  "  C     *LOVAL        SETLL     ND77W92L                                     *
009200120521R102 C                   READ      ND77W92L                               90    *
009300000000     C   90              GOTO      FINE                                         *
009400951124     C*
009500000000     C                   DO        *HIVAL                                       *
009600951124     C*
009700000000     C                   MOVE      W77TPQ        SAVTPQ            1            *
009800120516R102 C****               MOVE      W77TRI        SAVTRI            4            *
009900120516R102 C****               MOVE      W77PTR        SAVPTR            2            *
010000000000     C                   MOVE      W77           SAVW77                         *
010100000000     C                   Z-ADD     0             MAC                            *
010200000000     C                   Z-ADD     0             I                 2 0          *
010300010525     C                   Z-ADD     0             IMS              15 2          *
010400010525     C                   Z-ADD     0             RAC              15 2          *
010500010525     C                   Z-ADD     0             IMV1             15 2          *
010600150514R183 C                   Z-ADD     0             IRO              15 2
010700000000     C                   SETOFF                                           50    *
010800951124     C*
010900951124     C     W77           DOWEQ     SAVW77                                       *
011000951124     C*
011100010525     C                   Z-ADD     0             IMV              15 2          *
011200000000     C                   ADD       W77IMS        IMS                            *
011300000000     C                   ADD       W77RAC        RAC                            *
011400000000     C                   ADD       W77RAC        IMV                            *
011500000000     C                   ADD       1             I                              *
011600000000     C                   Z-ADD     W77MAC        MAC(I)                         *
011700150514R183 C                   ADD       W77IRO        IRO
011800951124     C*
011900010410     C* Calcolo arrotondato alle 1000 lire se Lire
012000010410C1478C*                     o all'unità di euro se euro
012100010410C1478C                   Select
012200010410C1478C                   When      $CdInter       = 'ITL'
012300010525     C                   Z-ADD     0             TOTARR           15 2          *
012400000000     C     IMV           ADD       499           TOTARR                         *
012500000000     C     TOTARR        DIV       1000          TOTARR                         *
012600000000     C     TOTARR        MULT      1000          TOTARR                         *
012700010410     C                   ADD       IMV           TOTARR                         *
012800010410C1478C                   When      $CdInter       = 'EUR'
012900010410  "  C                   Z-add     IMV           W3015
013000030903C1478C                   Z-add     0             NrDec
013100040305C1667x***ex C1478        CallP     X0201Arrot(W3015:'H':NrDec)
013200030903C1478C                   Z-add     W3015         TotArr
013300010410  "  C                   Other
013400010410  "  C                   Z-Add     IMV           TotArr
013500010410  "  C                   EndSl
013600010410C1478C*
013700000000     C                   ADD       TOTARR        IMV1                           *
013800951124     C*
013900120516R102 C****               READ      ND77W01L                               90    *
014000120521R102 C                   READ      ND77W92L                               90    *
014100951124     C*
014200951124     C  N90              ENDDO                                                  *
014300951124     C*
014400000000     C                   EXSR      SCRIVI                                       *
014500951124     C*
014600951124     C                   WRITE     ND77R000                                     *
014700951124     C*
014800951124     C  N90              ENDDO                                                  *
014900951124     C*
015000000000     C     FINE          TAG                                                    *
015100000000     C                   SETON                                        LR        *
015200000000     C*----------------------------------------------------*
015300000000     C* SUBROUTINE CANCELLAZIONE RECORD X ANNO COMPETENZA  *
015400000000     C*----------------------------------------------------*
015500951124     C     DEL77R        BEGSR                                                  *
015600951124     C*
015700951124     C                   z-add     annoR70       R77ACF
015800951124     C     KEY77R        SETLL     ND77R01L                                     *
015900951124     C*
016000000000     C                   DO        *HIVAL                                       *
016100951124     C     KEY77R        READE     ND77R01L                               90    *
016200000000     C   90              GOTO      ENDS                                         *
016300951124     C                   DELETE    ND77R000                                     *
016400951124     C                   ENDDO                                                  *
016500951124     C*
016600000000     C     ENDS          ENDSR                                                  *
016700000000     C*----------------------------------------------------*
016800000000     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
016900000000     C*----------------------------------------------------*
017000951124     C     INZVAR        BEGSR                                                  *
017100951124     C*
017200000000     C     *ENTRY        PLIST                                                  *
017300000000     C                   PARM                    KPJBA                          *
017400951124     C*
017500951124     C                   movel     kpjbu         ndcr70ds
017600951124     C*
017700000000     C     KEY77R        KLIST                                                  *
017800951127     C                   KFLD                    socR70                         *
017900951124     C                   KFLD                    R77ACF                         *
018000951127     C*
018100010410C1478C* Reperimento dati società
018200010410  "  C                   MOVE      SOCR70        SOCXSC
018300010410  "  C                   CALL      'XSOC'
018400010410  "  C                   PARM      'SOC001'      TIPXSC            6
018500010410  "  C                   PARM                    SOCXSC            3
018600010410  "  C                   PARM                    CDSXSC            9 0
018700010410  "  C                   PARM      *BLANK        MODXSC            3
018800010410  "  C                   PARM      *BLANKS       RTNXSC            1
018900010410  "  C                   PARM                    SOC001
019000010410  "  C                   PARM                    KPJBA
019100010410  "  C     RTNXSC        IFEQ      *ON
019200010410  "  C                   SETON                                        LR        *
019300010410  "  C                   endif
019400010410  "  C                   Eval      $Divisa    =  XScDiv
019500010410  "  C* reperisco il codice internazionale
019600010410  "  C                   Move      UDateIso      $Data
019700010410  "  C                   Eval      DvSEsito   = X0201RpDVS(
019800010410  "  C                                          $Divisa: $Data:
019900010410  "  C                                          AnDvS00F:%Size(AnDvS00F):
020000010410  "  C                                          AnDvX00F:%Size(AnDvX00F))
020100010410  "  C*
020200010410  "  C                   if        DvSEsito    = 0
020300010410  "  C                   Eval      $CdInter    = DVSCdInter
020400010410  "  C                   endif
020500010410C1478C*
020504160603     C*
020600000000     C                   ENDSR                                                  *
020700000000     C*----------------------------------------------------*
020800000000     C* SUBROUTINE SCRITTURA RECORD                        *
020900000000     C*----------------------------------------------------*
021000000000     C     SCRIVI        BEGSR                                                  *
021100951124     C*
021200951124     C* Ricerca mese/anno di corresponsione uguali
021300000000     C                   Z-ADD     1             I                              *
021400000000     C                   DO        50            I                              *
021500000000     C     MAC(I)        IFNE      0                                            *
021600000000     C     MAC(1)        IFNE      MAC(I)                                       *
021700000000     C                   SETON                                        50        *
021800951124     C                   ENDIF                                                  *
021900951124     C                   ENDIF                                                  *
022000951124     C                   ENDDO                                                  *
022100951124     C*
022200951124     C* Impostazione record
022300951124     C                   MOVE      SAVSOC        R77SOCIETA                     *
022400000000     C                   Z-ADD     SAVACF        R77ACF                         *
022500000000     C                   MOVE      SAVQUA        R77QUA                         *
022600951124     C                   MOVE      SAVDQV        R77DQV                         *
022700000000     C                   MOVE      SAVNQV        R77NQV                         *
022800000000     C                   MOVE      SAVTPQ        R77TPQ                         *
022900920722     C                   MOVE      SAVTRI        R77TRI                         *
023000150626R186 X***                MOVE      SAVPTR        R77PTR                         *
023001150626R186 C                   MOVE      '  '          R77PTR                         *
023100000000     C                   Z-ADD     IMS           R77IMS                         *
023200000000     C                   Z-ADD     RAC           R77RAC                         *
023300150514R183 C                   Z-ADD     IRO           R77IRO                         *
023400150514  "  C                   If        IRO > 0                                      *
023500150508  "  C                   Movel     *on           R77Ravvedi                     *
023600150508  "  C                   Else                                                   *
023700150508  "  C                   Clear                   R77Ravvedi
023800150508R183 C                   Endif
023900010410     C* Calcolo arrotondato alle 1000 lire se lire
024000010410C1478C*                     o all'unità di euro se euro
024100010410C1478C                   Select
024200010410C1478C                   When      $CdInter       = 'ITL'
024300010525     C                   Z-ADD     0             TOTARR           15 2
024400010410     C     RAC           ADD       499           TOTARR
024500010410     C     TOTARR        DIV       1000          TOTARR
024600010410     C     TOTARR        MULT      1000          TOTARR
024700010410     C                   Z-ADD     TOTARR        R77IMV
024800010410C1478C                   When      $CdInter       = 'EUR'
024900010410  "  C                   Z-add     RAC           W3015
025000030903C1478C                   Z-add     0             NrDec
025100040305C1667x***ex C1478        CallP     X0201Arrot(W3015:'H':NrDec)
025200030903C1478C                   Z-add     W3015         R77IMV
025300010410  "  C                   Other
025400010410  "  C                   Z-Add     RAC           R77IMV
025500010410  "  C                   EndSl
025600010410C1478C*
025700000000     C   50              Z-ADD     0             R77MAC                         *
025800000000     C  N50              Z-ADD     MAC(1)        R77MAC                         *
025801160603R218 C* Pulisco i campi gestiti manualmente
025802160603  "  C                   Clear                   R77Cdfs
025803160603  "  C                   Clear                   R77Ius
025804160603  "  C                   Clear                   R77Note
025805160603R218 C                   Clear                   R77Teso
025900951124     C*
026000000000     C                   ENDSR                                                  *
