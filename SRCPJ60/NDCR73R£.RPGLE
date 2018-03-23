000100981211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300951124     H*PARMS CVTOPT(*DATETIME)
000400951124     H DECEDIT('0,') DATEDIT(*DMY.)
000500951124     F*---------------------------------------------------------------*
000600951124     F*          ESTRAZIONE WORK FILE CON DETTAGLIO PER               *
000700951124     F*          PREPARAZIONE DEL RIEPILOGO DELLE SOMME               *
000800951124     F*                  ASSOGETTATE A RITENUTA                       *
000900000000     F*---------------------------------------------------------------*
001000951124     F*  --- FLUSSO PRINCIPALE ---                                    *
001100000000     F*                                                               *
001200951124     F*  - LETTURA MOVIMENTI RITENUTE IN ORDINE DI ANNO DI COMPET.    *
001300000000     F*    FISCALE/DATA E NUMERO QUIETANZA/TRIBUTO                    *
001400000000     F*                                                               *
001500000000     F*  - SELEZIONE DEI MOVIMENTI CHE HANNO QUADRO D OPPURE D1       *
001600000000     F*                                                               *
001700000000     F*  - SELEZIONE MOVIMENTI CHE HANNO STATO = 3                    *
001800000000     F*                                                               *
001900000000     F*  - TOTALIZZAZIONE IMPORTI PER MOVIMENTI CON STESSO TRIBUTO    *
002000000000     F*                                                               *
002100951124     F*  - A ROTTURA DI TRIBUTO SCRITTURA DI UN RECORD SU ND77W00F    *
002200000000     F*                                                               *
002300000000     F*  - LETTURA RITENUTE                                           *
002400000000     F*                                                               *
002500000000     F*  - FINE FILE RITENUTE VAI A FINE                              *
002600000000     F*---------------------------------------------------------------*
002700951124     FANTRI01L  IF   E           K DISK
002800951124     F*-------------
002900951124     FNDMRA07L  IF   E           K DISK
003000150508R183 F*-------------
003100150508R183 FNDMOV01L  IF   E           K DISK
003200951124     F*-------------
003300951124     FND77W01L  O    E           K DISK
003400951124     D*---------------------------------------------------------------*
003500951124     D KPJBA         E DS
003600951124     D*------------
003700951124     D ndcr70ds      E DS
003800150508R183 D*------------
003900150508  "  D* Parametro RABASE
004000150508  "  D ANP012DS      E DS                  INZ
004100150508R183 D XPAOUT          DS          2000
004200951124     D*------------
004300951127     D MRA             DS
004400951127     D  MRASOCIETA             1      3
004500951127     D  MRAACF                 4      7  0
004600951127     D  MRADQV                 8     17
004700951127     D  MRANQV                18     33
004800951127     D  MRANDT                34     42  0
004900951127     D  MRATRI                43     46
005000951127     D  MRAPTR                47     48
005100951127     D*------------
005200951127     D SAVMRA          DS
005300951127     D  SAVSOC                 1      3
005400951127     D  SAVACF                 4      7  0
005500951127     D  SAVDQV                 8     17
005600951127     D  SAVNQV                18     33
005700951127     D  SAVNDT                34     42  0
005800951127     D  SAVTRI                43     46
005900951127     D  SAVPTR                47     48
006000951124     D*------------
006100951124     D AMG             DS
006200951124     D  AMGSecolo              1      2
006300951124     D  AMGAnno                3      4
006400951124     D  T1                     5      5
006500951124     D  AMGMese                6      7
006600951124     D  T2                     8      8
006700951128     D  AMGGiorno              9     10
006800150508R183 D*------------
006900150508  "  D  DSNrASS        DS
007000150508  "  D  WrkSYS                             Like(MraSysQ)
007100150508  "  D  WrkNrAss                           Like(MraNasRegQ)
007200150508  "  D  SKNRASS        S                   Like(DSNrAss) dim(10000)
007300150508R183 D  WrkInd         S              5  0
007400951124     C*---------------------------------------------------------------*
007500160621      * FELLETTI - BRT 2016
007600160621     D  CauInRO012     S              4    INZ('XXXX')
007700160621     D  KccInRO012     S              6    INZ('XXXXXX')
007800160621     D  KscInRO012     S              8    INZ('XXXXXXXX')
007900160621      *
008000951124     C                   EXSR      INZVAR                                       *
008100951124     C*
008200951124     C                   z-add     annoR70       MRAACF
008300951124     C     KEYMRA        SETLL     NDMRA07L                                     *
008400951124     C     KEYMRA        READE     NDMRA07L                               90    *
008500000000     C   90              GOTO      FINE                                         *
008600951124     C*
008700000000     C                   DO        *HIVAL                                       *
008800951124     C*
008900951124     C                   MOVE      MRATRI        TRITRIBUTO                     *
009000951124     C                   MOVE      MRAPTR        TRITRIPROG                     *
009100951124     C     KEYTRI        CHAIN     ANTRI01L                           91        *
009200951124     C   91              MOVE      *BLANK        TRIQUA770                      *
009300951124     C*
009400020415     C     TRIQUA770     IFEQ      'AU'                                         *
009500981027     C     MRARAC        ANDNE     0                                            *
009600000000     C                   EXSR      ELAB                                         *
009700000000     C                   ELSE                                                   *
009800951124     C     KEYMRA        READE     NDMRA07L                               90    *
009900951124     C                   ENDIF                                                  *
010000951124     C*
010100951124     C  N90              ENDDO                                                  *
010200951124     C*
010300000000     C     FINE          TAG                                                    *
010400000000     C                   SETON                                            LR    *
010500000000     C*----------------------------------------------------*
010600000000     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
010700000000     C*----------------------------------------------------*
010800951124     C     INZVAR        BEGSR                                                  *
010900951124     C*
011000000000     C     *ENTRY        PLIST                                                  *
011100000000     C                   PARM                    KPJBA                          *
011200951124     C*
011300951124     C                   MOVEL     KPJBU         ndcr70ds                       *
011400951124     C*
011500000000     C     KEYMRA        KLIST                                                  *
011600951127     C                   KFLD                    socR70                         *
011700951124     C                   KFLD                    MRAACF                         *
011800951124     C*
011900951124     C     KEYTRI        KLIST
012000951127     C                   KFLD                    socR70
012100951124     C                   KFLD                    TRITRIBUTO
012200951124     C                   KFLD                    TRITRIPROG
012300150508R183 C*
012400150508  "  C     K02MOV01      KLIST
012500150508  "  C                   KFLD                    MOVSYS
012600150508  "  C                   KFLD                    MOVNRASREG
012700150508  "  C*
012800150508  "  C* Reperimento parametro RABASE
012900150508  "  C                   CallB     'XPAR'
013000150508  "  C                   Parm                    SocR70
013100150508  "  C                   Parm      'RABASE  '    XPaPar            8
013200150508  "  C                   Parm                    XPaOut
013300150508  "  C                   Parm      *Off          XPaErr            1
013400150508  "  C                   Parm      ' '           XPaRic            1
013500150508  "  C     XPaErr        IfEq      *On
013600150508  "  C                   Goto      Fine
013700150508  "  C                   Else
013800150508  "  C                   MoveL     XPaOut        ANP012DS
013900150508  "  C                   EndIf
014000150508  "  C*
014100150508  "  C                   Clear                   SKNrAss
014200150508R183 C                   z-add     0             WrkInd
014300951124     C*
014400000000     C                   ENDSR                                                  *
014500000000     C*----------------------------------------------------*
014600000000     C* SUBROUTINE ELABORAZIONE DETTAGLIO                  *
014700000000     C*----------------------------------------------------*
014800000000     C     ELAB          BEGSR                                                  *
014900951124     C*
015000010525     C                   Z-ADD     0             IMS              15 2          *
015100010525     C                   Z-ADD     0             RAC              15 2          *
015200150514R183 C                   Z-ADD     0             IRO              15 2
015300951124     C*
015400951124     C                   MOVE      TRIQUA770     SAVQUA            2            *
015500951124     C                   MOVE      MRADPF        AMG                            *
015600000000     C     MRATPQ        IFEQ      'E'                                          *
015700000000     C                   MOVE      'D'           SAVTPQ            2            *
015800951124     C                   ENDIF                                                  *
015900000000     C     MRATPQ        IFEQ      'P'                                          *
016000000000     C                   MOVE      'C'           SAVTPQ                         *
016100951124     C                   ENDIF                                                  *
016200000000     C     MRATPQ        IFNE      'E'                                          *
016300000000     C     MRATPQ        ANDNE     'P'                                          *
016400000000     C                   MOVE      *BLANK        SAVTPQ                         *
016500951124     C                   ENDIF                                                  *
016600951124     C*
016700951127     C                   MOVE      MRA           SAVMRA
016800951124     C*
016900951127     C     MRA           DOWEQ     SAVMRA
017000951124     C*
017100000000     C     MRASTA        IFEQ      '3'                                          *
017200000000     C                   ADD       MRAIMS        IMS                            *
017300000000     C                   ADD       MRARAC        RAC                            *
017400150514R183 C                   EXSR      REPINTROP
017500951124     C                   ENDIF                                                  *
017600951124     C*
017700951124     C     KEYMRA        READE     NDMRA07L                               90    *
017800951127     C*
017900951127     C  N90              ENDDO
018000951124     C*
018100951124     C                   MOVEL     SAVSOC        W77SOCIETA
018200951124     C                   Z-ADD     SAVACF        W77ACF
018300951124     C                   MOVE      SAVQUA        W77QUA
018400951124     C                   MOVE      SAVDQV        W77DQV
018500951124     C                   MOVE      SAVNQV        W77NQV
018600951124     C                   MOVE      SAVTPQ        W77TPQ
018700951124     C                   MOVE      SAVTRI        W77TRI
018800951124     C                   MOVE      SAVPTR        W77PTR
018900951124     C                   Z-ADD     IMS           W77IMS
019000951124     C                   Z-ADD     RAC           W77RAC
019100951124     C                   MOVEL     AMGMese       W77MAC
019200951124     C                   MOVE      AMGAnno       W77MAC
019300951124&&   C                   Z-ADD     SAVNDT        W77NDT
019400150514R183 C                   Z-ADD     IRO           W77IRO
019500951124     C*
019600951124     C                   WRITE     ND77W000                                     *
019700951124     C*
019800000000     C                   ENDSR                                                  *
019900150508R183 C*----------------------------------------------------*
020000150514  "  C* RAVVEDIMENTO OPEROSO : Reperimento interessi       *
020100150508  "  C*----------------------------------------------------*
020200150514R183 C     REPINTROP     BEGSR                                                  *
020300150508     C*
020400150514     C                   If        CauINRO012 <> *blanks and
020500150514     C                             KccINRO012 <> *blanks and
020600150514     C                             KscINRO012 <> *blanks
020700150508     C* Verifico se esistono interessi da ravvedimento.
020800150508     C                   Eval      MovSys     = MRASYSQ
020900150508     C                   Eval      MovNrAsReg = MRANASREGQ
021000150508     C*
021100150508     C* Verifico di non avere già inserito il numero assoluto
021200150508     C* se ci fossero pagamenti che riguardano più ritenute.
021300150508     C* devo indicare un unico interesse da ravvedimento.
021400150508     C                   z-add     MraSysQ       WRKSys
021500150508     C                   z-add     MraNasRegQ    WRKNrAss
021600150508     C     DSNrAss       lookup    SkNrAss                                10
021700150508     C                   If        not *in10
021800150508     C                   add       1             wrkInd
021900150508     C                   Eval      SkNrAss(Wrkind) = DsNrAss
022000150508     C*
022100150508     C     K02MOV01      Setll     NdMov01l
022200150508     C                   do        *Hival
022300150508     C     K02MOV01      reade     NDMOV01l
022400150508     C                   If        %eof(NDMOV01L)
022500150508     C                   Leave
022600150508     C                   Endif
022700150508     C* Se il conto e la causale sono i medesimi del parametro modulo
022800150508     C* è un interesse da ravvedimento.
022900150514     C                   If        MovCausRig = CauInRO012 and
023000150514     C                             MovKcc     = KccInRO012 and
023100150514     C                             MovKsc     = KscInRO012
023200150514     C                   add       MovImporto    IRO
023300150508     C                   Endif
023400150508     C*
023500150508     C                   Enddo
023600150508     C*
023700150508     C                   Endif
023800150508     C*
023900150508     C                   Endif
024000150508     C*
024100150508R183 C                   ENDSR
