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
002901150508R183 F*-------------
002902150508R183 FNDMOV01L  IF   E           K DISK
003000951124     F*-------------
003100951124     FND77W01L  O    E           K DISK
003200951124     D*---------------------------------------------------------------*
003300951124     D KPJBA         E DS
003400951124     D*------------
003500170413R254 X*** ndcr70ds      E DS
003501170413R254 D SdgSk01ds     E DS
003502150508R183 D*------------
003503150508  "  D* Parametro RABASE
003504150508  "  D ANP012DS      E DS                  INZ
003505150508R183 D XPAOUT          DS          2000
003600951124     D*------------
003700951127     D MRA             DS
003800951127     D  MRASOCIETA             1      3
003900951127     D  MRAACF                 4      7  0
004000951127     D  MRADQV                 8     17
004100951127     D  MRANQV                18     33
004200951127     D  MRANDT                34     42  0
004300951127     D  MRATRI                43     46
004400951127     D  MRAPTR                47     48
004500951127     D*------------
004600951127     D SAVMRA          DS
004700951127     D  SAVSOC                 1      3
004800951127     D  SAVACF                 4      7  0
004900951127     D  SAVDQV                 8     17
005000951127     D  SAVNQV                18     33
005100951127     D  SAVNDT                34     42  0
005200951127     D  SAVTRI                43     46
005300951127     D  SAVPTR                47     48
005400951124     D*------------
005500951124     D AMG             DS
005600951124     D  AMGSecolo              1      2
005700951124     D  AMGAnno                3      4
005800951124     D  T1                     5      5
005900951124     D  AMGMese                6      7
006000951124     D  T2                     8      8
006100951128     D  AMGGiorno              9     10
006101150508R183 D*------------
006102150508  "  D  DSNrASS        DS
006103150508  "  D  WrkSYS                             Like(MraSysQ)
006104150508  "  D  WrkNrAss                           Like(MraNasRegQ)
006105150508  "  D  SKNRASS        S                   Like(DSNrAss) dim(10000)
006106150508R183 D  WrkInd         S              5  0
006200951124     C*---------------------------------------------------------------*
006201170620      * FELLETTI - BRT 2016
006202170620     D  CauInRO012     S              4    INZ('XXXX')
006203170620     D  KccInRO012     S              6    INZ('XXXXXX')
006204170620     D  KscInRO012     S              8    INZ('XXXXXXXX')
006205170620      *
006300951124     C                   EXSR      INZVAR                                       *
006400951124     C*
006500170413R254 X***                z-add     annoR70       MRAACF
006501170413R254 C                   z-add     annoK01       MRAACF
006600951124     C     KEYMRA        SETLL     NDMRA07L                                     *
006700951124     C     KEYMRA        READE     NDMRA07L                               90    *
006800000000     C   90              GOTO      FINE                                         *
006900951124     C*
007000000000     C                   DO        *HIVAL                                       *
007100951124     C*
007200951124     C                   MOVE      MRATRI        TRITRIBUTO                     *
007300951124     C                   MOVE      MRAPTR        TRITRIPROG                     *
007400951124     C     KEYTRI        CHAIN     ANTRI01L                           91        *
007500951124     C   91              MOVE      *BLANK        TRIQUA770                      *
007600951124     C*
007901020415     C     TRIQUA770     IFEQ      'AU'                                         *
008000981027     C     MRARAC        ANDNE     0                                            *
008100000000     C                   EXSR      ELAB                                         *
008200000000     C                   ELSE                                                   *
008300951124     C     KEYMRA        READE     NDMRA07L                               90    *
008400951124     C                   ENDIF                                                  *
008500951124     C*
008600951124     C  N90              ENDDO                                                  *
008700951124     C*
008800000000     C     FINE          TAG                                                    *
008900000000     C                   SETON                                            LR    *
009000000000     C*----------------------------------------------------*
009100000000     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
009200000000     C*----------------------------------------------------*
009300951124     C     INZVAR        BEGSR                                                  *
009400951124     C*
009500000000     C     *ENTRY        PLIST                                                  *
009600000000     C                   PARM                    KPJBA                          *
009700951124     C*
009800170413R254 X***                MOVEL     KPJBU         ndcr70ds                       *
009801170413R254 C                   MOVEL     KPJBU         SdgSk01ds                      *
009900951124     C*
010000000000     C     KEYMRA        KLIST                                                  *
010100170413R254 X***                KFLD                    socR70                         *
010101170413R254 C                   KFLD                    SocietaK01                     *
010200951124     C                   KFLD                    MRAACF                         *
010300951124     C*
010400951124     C     KEYTRI        KLIST
010500170413R254 C***                KFLD                    socR70                         *
010501170413R254 C                   KFLD                    SocietaK01                     *
010600951124     C                   KFLD                    TRITRIBUTO
010700951124     C                   KFLD                    TRITRIPROG
010701150508R183 C*
010702150508  "  C     K02MOV01      KLIST
010703150508  "  C                   KFLD                    MOVSYS
010704150508  "  C                   KFLD                    MOVNRASREG
010705150508  "  C*
010706150508  "  C* Reperimento parametro RABASE
010707170413R183 C                   CallB     'XPAR'
010708170413R254 X***                Parm                    SocR70
010709170413R254 C                   Parm                    SocietaK01
010710170413R183 C                   Parm      'RABASE  '    XPaPar            8
010711150508  "  C                   Parm                    XPaOut
010712150508  "  C                   Parm      *Off          XPaErr            1
010713150508  "  C                   Parm      ' '           XPaRic            1
010714150508  "  C     XPaErr        IfEq      *On
010715150508  "  C                   Goto      Fine
010716150508  "  C                   Else
010717150508  "  C                   MoveL     XPaOut        ANP012DS
010718150508  "  C                   EndIf
010719150508  "  C*
010720150508  "  C                   Clear                   SKNrAss
010721150508R183 C                   z-add     0             WrkInd
010800951124     C*
010900000000     C                   ENDSR                                                  *
011000000000     C*----------------------------------------------------*
011100000000     C* SUBROUTINE ELABORAZIONE DETTAGLIO                  *
011200000000     C*----------------------------------------------------*
011300000000     C     ELAB          BEGSR                                                  *
011400951124     C*
011500010525     C                   Z-ADD     0             IMS              15 2          *
011600010525     C                   Z-ADD     0             RAC              15 2          *
011601150514R183 C                   Z-ADD     0             IRO              15 2
011700951124     C*
011800951124     C                   MOVE      TRIQUA770     SAVQUA            2            *
011900951124     C                   MOVE      MRADPF        AMG                            *
012000000000     C     MRATPQ        IFEQ      'E'                                          *
012100000000     C                   MOVE      'D'           SAVTPQ            2            *
012200951124     C                   ENDIF                                                  *
012300000000     C     MRATPQ        IFEQ      'P'                                          *
012400000000     C                   MOVE      'C'           SAVTPQ                         *
012500951124     C                   ENDIF                                                  *
012600000000     C     MRATPQ        IFNE      'E'                                          *
012700000000     C     MRATPQ        ANDNE     'P'                                          *
012800000000     C                   MOVE      *BLANK        SAVTPQ                         *
012900951124     C                   ENDIF                                                  *
013000951124     C*
013100951127     C                   MOVE      MRA           SAVMRA
013200951124     C*
013300951127     C     MRA           DOWEQ     SAVMRA
013400951124     C*
013500000000     C     MRASTA        IFEQ      '3'                                          *
013600000000     C                   ADD       MRAIMS        IMS                            *
013700000000     C                   ADD       MRARAC        RAC                            *
013701150514R183 C                   EXSR      REPINTROP
013800951124     C                   ENDIF                                                  *
013900951124     C*
014000951124     C     KEYMRA        READE     NDMRA07L                               90    *
014100951127     C*
014200951127     C  N90              ENDDO
014300951124     C*
014400951124     C                   MOVEL     SAVSOC        W77SOCIETA
014500951124     C                   Z-ADD     SAVACF        W77ACF
014600951124     C                   MOVE      SAVQUA        W77QUA
014700951124     C                   MOVE      SAVDQV        W77DQV
014800951124     C                   MOVE      SAVNQV        W77NQV
014900951124     C                   MOVE      SAVTPQ        W77TPQ
015000951124     C                   MOVE      SAVTRI        W77TRI
015100951124     C                   MOVE      SAVPTR        W77PTR
015200951124     C                   Z-ADD     IMS           W77IMS
015300951124     C                   Z-ADD     RAC           W77RAC
015400951124     C                   MOVEL     AMGMese       W77MAC
015500951124     C                   MOVE      AMGAnno       W77MAC
015600951124&&   C                   Z-ADD     SAVNDT        W77NDT
015601150514R183 C                   Z-ADD     IRO           W77IRO
015700951124     C*
015800951124     C                   WRITE     ND77W000                                     *
015900951124     C*
016000000000     C                   ENDSR                                                  *
016100150508R183 C*----------------------------------------------------*
016200150514  "  C* RAVVEDIMENTO OPEROSO : Reperimento interessi       *
016300150508  "  C*----------------------------------------------------*
016400150514R183 C     REPINTROP     BEGSR                                                  *
016500150508     C*
016600150514     C                   If        CauINRO012 <> *blanks and
016700150514     C                             KccINRO012 <> *blanks and
016800150514     C                             KscINRO012 <> *blanks
016900150508     C* Verifico se esistono interessi da ravvedimento.
017000150508     C                   Eval      MovSys     = MRASYSQ
017100150508     C                   Eval      MovNrAsReg = MRANASREGQ
017200150508     C*
017300150508     C* Verifico di non avere già inserito il numero assoluto
017400150508     C* se ci fossero pagamenti che riguardano più ritenute.
017500150508     C* devo indicare un unico interesse da ravvedimento.
017600150508     C                   z-add     MraSysQ       WRKSys
017700150508     C                   z-add     MraNasRegQ    WRKNrAss
017800150508     C     DSNrAss       lookup    SkNrAss                                10
017801150508     C                   If        not *in10
017802150508     C                   add       1             wrkInd
017803150508     C                   Eval      SkNrAss(Wrkind) = DsNrAss
017900150508     C*
018000150508     C     K02MOV01      Setll     NdMov01l
018100150508     C                   do        *Hival
018200150508     C     K02MOV01      reade     NDMOV01l
018300150508     C                   If        %eof(NDMOV01L)
018400150508     C                   Leave
018500150508     C                   Endif
018600150508     C* Se il conto e la causale sono i medesimi del parametro modulo
018700150508     C* è un interesse da ravvedimento.
018800150514     C                   If        MovCausRig = CauInRO012 and
018900150514     C                             MovKcc     = KccInRO012 and
019000150514     C                             MovKsc     = KscInRO012
019100150514     C                   add       MovImporto    IRO
019200150508     C                   Endif
019300150508     C*
019400150508     C                   Enddo
019500150508     C*
019600150508     C                   Endif
019601150508     C*
019700150508     C                   Endif
020000150508     C*
020100150508R183 C                   ENDSR
