000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200040127     H*--------------------------------------------------------------*
000400940616     H*               - DUPLICAZIONE TARIFFA -     BATCH             *
000500000000     H*--------------------------------------------------------------*
000600040127     FTNTAM01L  iF   E           K DISK
000700990714     FTITAD04L  UF A E           K DISK
000800040127     FTITPT01L  UF A E           K DISK
000900990714     FTITPD01L  UF A E           K DISK
002000000000     D PARAM           DS
002100040127     D*                                     CODICE CLIENTE VECCHIO
002200000000     D  CLIOLD                 1      7  0
002201040127     D*                                     CODICE TARIFFA VECCHIO
002202040127     D  CTROLD                 8     10  0
002300040127     D*                                     PROGRESSIVO TARIFFA VECCHIO
002400040127     D  PRGOLD                11     13  0
002500040127     D*                                     CODICE CLIENTE NUOVO
002600040127     D  CLINEW                14     20  0
002900040127     D*                                     CODICE TARIFFA NUOVO
003000040127     D  CTRNEW                21     23  0
003001040127     D*                                     CODICE progressivo nuovo
003002040127     D  PRGNEW                24     26  0
003003040127      ****
003004040127     D                 DS
003005040127     D  GG                     1      2  0
003006040127     D  MM                     3      4  0
003007040127     D  AA                     5      8  0
003008040127     D  DATA                   1      8  0
009400940616     C*****************************************************************
009500940616     C* RIEPILOGO INDICATORI
009600940616     C*****************************************************************
009700940616     C* 30/33 - LETTURE
009800940616     C* 31/34 - DI COMODO
009900940616     C*****************************************************************
010000000000     C     *ENTRY        PLIST
010100040127     C                   PARM                    PARAM
010400000000     C*---------------------------------------------------------------*
010500990714     C* ACCESSO TNTAM/TITAD/TITPT
010600921030     C     KTAM          KLIST
010700921030     C                   KFLD                    CLIOLD
010800921030     C                   KFLD                    CTROLD
010900921030     C                   KFLD                    PRGOLD
011000940616     C* ACCESSO TNTAM
011100921030     C     KTAMW         KLIST
011200921030     C                   KFLD                    CLINEW
011300921030     C                   KFLD                    CTRNEW
011400921030     C                   KFLD                    PRGNEW
016300990714     C* ACCESSO TITPT
016400921102     C     KTPTW         KLIST
016500921102     C                   KFLD                    CLINEW
016600921102     C                   KFLD                    CTRNEW
016700921102     C                   KFLD                    PRGNEW
016800921102     C                   KFLD                    TPTFTC
016900930430     C     KTPTP         KLIST
017000930430     C                   KFLD                    CLIOLD
017100930430     C                   KFLD                    CTROLD
017200930430     C                   KFLD                    PRGOLD
017300930430     C                   KFLD                    TPTFTC
017301040127     C*---------------------------------------------------------------*
017302040127     C** LA DATA DEL GIORNO LA PRENDO DA TIME
017303040127     C                   TIME                    W0140            14 0
017304040127     C                   MOVE      W0140         UDATE8            8 0
017305040127     C                   Z-ADD     UDATE8        DATA
017306040127     C                   Z-ADD     GG            MMGG              4 0
017307040127     C                   MOVEL     MM            MMGG
017308040127     C                   Z-ADD     MMGG          DATEU8            8 0
017309040127     C                   MOVEL     AA            DATEU8
026500991027     C*
026600040127     C*****    verifica di tntam                    *****
027000040127     C     KTAmw         CHAin     TNTAM01l
027100040127     C*
027101040127    1c                   if        %found(tntam01l)
027102040127      * verifico il vecchio
027103040127     c     Ktam          chain     tntam01l
027104040127      * se esisto verifico se esiste il dettaglio nuovo
027105040127    2c                   if        %found(tntam01l)
027106040127     c     ktamw         chain     titad04l
027107040127      * se non è stato trovato
027108040127    3c                   if        not %found(titad04l)
027109040127      * COPIO dal vecchio dettaglio
027110040127     c     ktam          SETLL     titad04l
027111040127    4c                   do        *hival
027200040127     C
042100040127     C     KTAM          READE     TITAD04l
042300940616     C*
042301040127    5c                   if        %eof(titad04l)
042302040127     c                   leave
042303040127    5c                   endif
048100020207      *
049400020207      *
049500900503     C*
049700000000     C                   Z-ADD     CLINEW        TADKSC
049800000000     C                   Z-ADD     PRGNEW        TADPRG
049900900423     C                   Z-ADD     CTRNEW        TADCTR
050000940616     C                   Z-ADD     0             TADDTR
050100940616     C                   MOVEL     *BLANKS       TADFTR
057400990714     C                   WRITE     TITAD000
057600020207      *
058500040127    4C                   ENDDO
058501040127      *
058502040127    3c                   endif
058505040127      *
058506040127     c     ktam          setll     titpt01l
058507040127      *
058508040127    3c                   do        *hival
058509040127     c     ktam          reade     titpt01l
058510040127      *
058511040127    4c                   if        %eof(titpt01l)
058512040127     c                   leave
058513040127    4c                   endif
058514040127      * verifico se esiste già nel nuovo
058515040127     c     ktptw         chain     titpt01l
058516040127    4c                   if        not %found(titpt01l)
058517040127     c     ktptp         chain     titpt01l
058518040127    5c                   if        %found(titpt01l)
058519040127     C                   Z-ADD     CLINEW        TPTKSC
058520040127     C                   Z-ADD     CTRNEW        TPTCTR
058521040127     C                   Z-ADD     PRGNEW        TPTPRG
058522040127     C                   Z-ADD     0             TPTDTR
058523040127     C                   MOVEL     *BLANKS       TPTFTR
058524040127     C                   Z-ADD     DATEU8        TPTDUV
058525040127     C*
058526040127     C                   WRITE     TITPT000
058528040127    5c                   endif
058529040127      *
058533040127    4c                   endif
058534040127      * aggancio il precedente per la prossima lettura
058535040127     c     ktptp         chain     titpt01l
058536040127      * cerco il dettaglio nel nuovpo progressivo se c'è vado al codice successivo
058537040127     c     ktptw         chain     titpd01l
058538040127    4c                   if        not %found(titpd01l)
058539040127      *
058540040127     c     ktptp         setll     titpd01l
058541040127    5c                   do        *hival
058542040127     c     ktptp         reade     titpd01l
058543040127    6c                   if        %eof(titpd01l)
058544040127     c                   leave
058545040127    6c                   endif
058546040127      * scrivo nel nuovo progressivo
058547040127     C*
058548040127     C                   Z-ADD     CLINEW        TPDKSC
058549040127     C                   Z-ADD     CTRNEW        TPDCTR
058550040127     C                   Z-ADD     PRGNEW        TPDPRG
058551040127     C                   Z-ADD     0             TPDDTR
058552040127     C                   MOVEL     *BLANKS       TPDFTR
058553040127     C*
058554040127     C                   WRITE     TITPD000
058600040127      *
058700040127    5c                   enddo
058800040127      *
058900040127    4c                   endif
059000040127      *
059100040127    3c                   enddo
059200040127      *
059201040127      *
059202040127    2c                   endif
059300040127    1c                   endif
059400040127      *
059500040127     c                   seton                                        lr
