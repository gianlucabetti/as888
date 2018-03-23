000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(PROBAS) BNDDIR(*NONE)
000200000000     H*PARMS CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     F*-------------
000500000000     F*
000600000000     F*  Esecuzione job schedulato
000700000000     F*
000800000000     F*-------------
000900000000     FKPSCH01L  UF   E           K DISK
001000000000X0030X****KFSIF01L  IF   E           K DISK
001100000000     FKPPRF01L  IF   E           K DISK
001200000000     FKFAZN11L  IF   E           K DISK    USROPN
001300000000     D*-------------
001400000000     D KPJBA         E DS
001500000000X0179X*****  KCANCJBA     E                     EXTFLD(KCANC)
001600000000  "  X*****  KGRAJBA      E                     EXTFLD(KGRAZ)
001700000000  "  D KCANC_JBA     E                     EXTFLD(KCANC)
001800000000X0179D KGRAZ_JBA     E                     EXTFLD(KGRAZ)
001900000000     D*-------------
002000000000     D* Copia della KPJBA per BAK025C
002100000000     D KPJBA1          DS           503
002200000000     D  KPJXXX               503    503    INZ('X')
002300000000     D*-------------
002400000000     D* Immagine di KFAZN10F per BAK025C
002500000000     D KFAZN         E DS                  EXTNAME(KFAZN10F)
002600000000     D*-------------
002700000000     D* Param. input
002800000000     D IdJobSch        S                   Like(SCHIdS)
002900000000     D*-------------
003000000000     D Lingua          S              1A
003100000000     D*-------------
003200000000     C*----------------------------------------------------*
003300000000     C*                MAIN LINE PROGRAM
003400000000     C*----------------------------------------------------*
003500000000     C*
003600000000     C* Inizializzazione variabili
003700000000     C                   EXSR      INZVAR
003800000000     C*
003900000000     C* Esecuzione job schedulato
004000000000     C                   EXSR      EXEJOB
004100000000     C*
004200000000     C* Fine programma
004300000000     C                   EXSR      ENDPGM
004400000000     C*
004500000000     C/EJECT
004600000000     C************************************************************
004700000000     C* Esecuzione job schedulato
004800000000     C************************************************************
004900000000     C     EXEJOB        BEGSR
005000000000     C*
005100000000     C* Job schedulato
005200000000     C     IdJobSch      Chain(N)  KPSCH01L                           21
005300000000     C                   If        *In21 = *Off
005400000000     C*
005500000000     C* Profilo esecuzione
005600000000     C     SCHPrf        Chain     KPPRF01L                           21
005700000000     C                   If        *In21 = *Off
005800000000X0030X*
005900000000  "  X* S.I esecuzione
006000000000  "  X**** SCHSIf        Chain     KFSIF01L                           21
006100000000X0030X****               If        *In21 = *Off
006200000000     C*
006300000000     C* Imposto lista di librerie S.I.
006400000000X0030X****               Move      PRFLin        KFLRC
006500000000     C                   EXSR      SETLIB
006600000000     C*
006700000000     C* Eseguo job
006800000000X0030C                   If        RtnCde002='0'
006900000000     C                   Select
007000000000     C* - azione
007100000000     C                   When      SCHTpS = 'A'
007200000000     C                   EXSR      EXEAZN
007300000000     C                   EndSl
007400000000     C*
007500000000     C* Se la frequenza ? *ONCE (flag SCHScd='O') cancello il record
007600000000     C                   If        SCHScd = 'O'
007700000000     C     IdJobSch      Delete    KPSCH01L                           2122
007800000000     C                   EndIf
007900000000     C*
008000000000     C                   EndIf
008100000000     C                   EndIf
008200000000     C                   EndIf
008300000000     C*
008400000000     C                   ENDSR
008500000000     C/EJECT
008600000000     C************************************************************
008700000000     C* Esecuzione azione
008800000000     C************************************************************
008900000000     C     EXEAZN        BEGSR
009000000000     C*
009100000000     C                   Open      KFAZN11L                             21
009200000000     C                   If        *In21 = *Off
009300000000     C*
009400000000     C                   MoveL     SCHAzn        KCAZN
009500000000     C     KCAZN         Chain     KFAZN11L                           21
009600000000     C                   If        *In21 = *Off
009700000000     C* Imposto KPJBA
009800000000     C                   Clear                   KPJBA
009900000000     C                   Move      SCHSIf        KNSIF
010000000000X0114X*****              Move      PRFLin        KFLRC
010100000000X0114C                   Move      Lingua        KFLRC
010200000000     C                   Move      KCAZN         KCDAZ
010300000000X0091C* le azioni schedulate sono a tutti gli effetti azioni batch !!!
010400000000X0091C                   Move      KCAZN         KCOAZ
010500000000     C                   CALL      'BAK021R'
010600000000     C                   PARM                    KPJBA
010700000000     C                   Move      SCHPrf        KNMUS
010800000000     C                   Move      SCHJbQ        KQIEB
010900000000X0212C                   Move      SCHIDS        KNMEB
011000000000     C* Imposto KPJBU dell'azione schedulata
011100000000     C* (SCHKPJ ? l'intera KPJBA, D1Kpjbu ? solo la KPJBU)
011200000000     C                   If        SCHKpj <> *Blank
011300000000     C                   Move      SCHKpj        KPJBU
011400000000     C                   EndIf
011500000000X0091C* Richiamo pgm x La gestione del File di Log KPLOG00F
011600000000  "  C* per poter simulare effettivamente un'azione batch !!!
011700000000  "  C* (... che ha bisogno di un "giro" in pi? sul file di log)
011800000000  "  C                   CALL      'BAK035R'
011900000000  "  C                   PARM                    KPJBA
012000000000X0091C                   PARM                    KFAZN
012100000000     C* Esecuzione azione
012200000000X0091X*****              Move      KCAZN         KCOAZ
012300000000     C                   MoveL     KPJBA         KPJBA1
012400000000     C                   CALL      'BAK025C'
012500000000     C                   PARM                    KPJBA1
012600000000     C                   PARM                    KFAZN
012700000000     C                   PARM                    ERR025            1
012800000000     C*
012900000000     C                   EndIf
013000000000     C*
013100000000     C                   Close     KFAZN11L
013200000000     C                   EndIf
013300000000     C*
013400000000     C                   ENDSR
013500000000     C/EJECT
013600000000     C************************************************************
013700000000     C* Setta la lista delle librerie come da s.i.
013800000000     C************************************************************
013900000000     C     SETLIB        BEGSR
014000000000     C*
014100000000X0030X****               CALL      'BAK012R'
014200000000  "  X****               PARM                    KLBSI                          LIB.  1-14
014300000000  "  X****               PARM                    KLBSI2                         LIB. 15-20
014400000000  "  X****               PARM                    KNMSI                          SYS.INFO.
014500000000X0030X****               PARM                    KFLRC                          LINGUA
014600000000X0114C                   Move      PRFLin        Lingua
014700000000X0030 *
014800000000  "  C                   call      'BAK002R'
014900000000X0030C                   parm                    SchSIf
015000000000X0114X*****              parm                    PrfLin
015100000000X0114C                   parm                    Lingua
015200000000X0030C                   parm                    RtnCde002         1
015300000000     C*
015400000000     C                   ENDSR
015500000000     C/EJECT
015600000000     C************************************************************
015700000000     C* FINE PROGRAMMA
015800000000     C************************************************************
015900000000     C     ENDPGM        BEGSR
016000000000     C*
016100000000     C                   SETON                                        LR
016200000000     C                   RETURN
016300000000     C*
016400000000     C                   ENDSR
016500000000     C/EJECT
016600000000     C************************************************************
016700000000     C* Definizioni e inizializzazioni
016800000000     C************************************************************
016900000000     C     *INZSR        BEGSR
017000000000     C*
017100000000     C     *ENTRY        PLIST
017200000000     C                   PARM                    IdJobSch
017300000000     C*
017400000000     C                   ENDSR
017500000000     C/EJECT
017600000000     C************************************************************
017700000000     C* INIZIALIZZAZIONE VARIABILI
017800000000     C************************************************************
017900000000     C     INZVAR        BEGSR
018000000000     C*
018200000000     C                   ENDSR
