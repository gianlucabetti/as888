000100020415     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200940726     H* TNTA51R *----------------------------------------------------*
000300940726     H*         PRENOTAZIONE NUOVO CODICE CLIENTE/FORNITORE          *
000400000000     H*--------------------------------------------------------------*
000500940726     FTNTA51D   CF   E             WORKSTN
000600900420     FAZORG01L  IF   E           K DISK
000700900420     FTABEL00F  IF   E           K DISK
000800900420     FCNACO00F  IF   E           K DISK
000900930121     FFLNUF01L  UF A E           K DISK
001000060202
001100060202     d codut           s                   like(TblKut) inz(1)
001200060202
001300090707     d msg             s             78    dim(02) ctdata perrcd(1)
001400060202
001500060202     d Azuteds       e ds                  Extname(Azute00f)
001600060202     d dDatiute      e ds
001700000000     D KPJBA         E DS
001800060202     d Tibs02ds      e ds
001900060202     d Tibs34ds      e ds
002000090707     d tntaa1ds      e ds                  INZ
002100060202
002200000000     I/SPACE 3
002300000000     C*---------------------------------------------------------------*
002400000000     C     *ENTRY        PLIST
002500000000     C                   PARM                    KPJBA
002600060202
002700060202     c     *dtaara       define    §azute        azuteds
002800060202     c     *dtaara       define    §datiute      ddatiute
002900060202     c                   in(E)     *dtaara
003000060202     c                   If        %error  or rsut = *blanks
003100060202     c                   Clear                   Tibs34ds
003200060202     c                   Call      'TIBS34R'
003300060202     c                   Parm                    Tibs34ds
003400060202     c                   In        *dtaara
003500060202     c                   EndIf
003600060202
003700090707     c* Richiamo solo per avere codice di abilitazione
003800090707     c                   clear                   tntaa1ds
003900090707     c                   eval      itaa1caut='§utecli'
004000090707     c                   clear                   kpjbu
004100090707     c                   movel     tntaa1ds      kpjbu
004200090707     c                   call      'TNTAA1C'
004300090707     c                   parm                    kpjba
004400090707     c                   movel     kpjbu         tntaa1ds
004500090707     C* UTENTE NON abilitato
004600090707     c                   if        otaa1fabi='N'
004700060202
004800060202     c                   Eval      *In08 = *On
004900060202     c                   Eval      *In28 = *On
005000060202     c                   Eval      v1cmsg = msg(01)
005100090707     c                   endif
005200090707     c
005300900420     C*---------------------------------------------------------------*
005400940726     C* ACCESSO TABEL00F
005500900420     C     KTAB          KLIST
005600900420     C                   KFLD                    CODUT
005700940726     C                   KFLD                    COD
005800940726     C* ACCESSO CNACO00F
005900900420     C     KCLI          KLIST
006000900420     C                   KFLD                    CODUT
006100900420     C                   KFLD                    VIDCCC
006200940726     C                   KFLD                    KSC
006300940726     C* ACCESSO FLNUF01L
006400900420     C     KNUF          KLIST
006500940726     C                   KFLD                    AAA
006600940726     C                   KFLD                    NUM
006700900420     C                   KFLD                    VIDFIL
006800940726     C*
006900940726     C* DEFINIZIONE CAMPI
007000940726     C     *LIKE         DEFINE    TBLCOD        COD
007100940726     C     *LIKE         DEFINE    NUFAAA        AAA
007200940726     C     *LIKE         DEFINE    NUFCNU        NUM
007300940726     C     *LIKE         DEFINE    ACOKSC        KSC
007400940726     C*
007500940726     C*---------------------------------------------------------------*
007600900420     C     INIZIO        TAG
007700900420     C                   SETOFF                                       42
007800900420     C                   Z-ADD     0             VIDFIL
007900060202     C                   Z-ADD     dutKCI        VIDCCC
008000900420     C                   Z-ADD     0             KSC
008100900420     C                   MOVEL     *BLANKS       DESFIL
008200900420     C                   MOVEL     *BLANKS       DESCCC
008300900409     C*
008400940726     C                   WRITE     TA51D03
008500000000     C     START         TAG
008600940726     C                   EXFMT     TA51D01
008700900420     C** FINE LAVORO
008800060202     C   KC
008900060202     cor 08              goto      fine
009000900420     C* DECODIFICO
009100900420     C     KCLI          CHAIN     CNACO000                           30
009200900420     C  N30              MOVEL     ACORAG        DESCCC
009300900420     C   30              MOVEL     *BLANKS       DESCCC
009400090707     c
009500090707     C* CONTROLLO sbilitazioni utente
009600020415      * solo se sono in filiale
009700020415     c                   if        simfel > 0
009800090707     C**                 MOVEL     VIDFIL        FILC              3
009900090707     C**   FILC          LOOKUP    skpog                                  30
010000090707     c*
010100090707     c                   clear                   tntaa1ds
010200090707     c                   eval      itaa1caut='§utecli'
010300090707     c                   eval      itaa1fil =vidfil
010400090707     c                   clear                   kpjbu
010500090707     c                   movel     tntaa1ds      kpjbu
010600090707     c                   call      'TNTAA1C'
010700090707     c                   parm                    kpjba
010800090707     c                   movel     kpjbu         tntaa1ds
010900090707     c
011000090707     c* Utente non abilitato alla gestione del cliente
011100090707     c                   if        otaa1fabi='N' or simfel=0 or vidfil=0
011200090707     C                   SETON                                            41
011300090707     C                   GOTO      START
011400090707     c                   endif
011500090707     c                   endif
011600900406     C*
011700900420     C     VIDFIL        CHAIN     AZORG                              30
011800090707     c                   if        not *in30
011900090707     C                   MOVEL     ORGDES        DESFIL
012000090707     c                   else
012100090707     C                   MOVEL     *BLANKS       DESFIL
012200090707     C                   SETON                                            41
012300090707     C                   GOTO      START
012400090707     c                   endif
012500900420     C*
012600900420     C* CARICO NUMERO RELATIVO
012700900420     C                   EXSR      CARIC
012800900420     C*
012900940726     C                   WRITE     TA51D04
013000940726     C                   WRITE     TA51D01
013100900420     C*
013200900420     C     FOR02         TAG
013300940726     C                   EXFMT     TA51D02
013400940726     C*  CMD3- FINE LAVORO
013500940726     C   KC              GOTO      FINE
013600900420     C* CMD1- PRENOTAZIONE CODICE
013700900420     C   KA              DO
013800900420     C                   Z-ADD     SAVNUM        NUFNUM
013900900420     C                   UPDATE    FLNUF
014000900420     C                   GOTO      INIZIO
014100900420     C                   END
014200900420     C* ENTER NULLA
014300900420     C                   GOTO      FOR02
014400900420     C*
014500000000     C     FINE          TAG
014600090707     C*
014700090707     C                   clear                   tntaa1ds
014800090707     C                   movel     'C'           Itaa1Tla
014900090707     C                   movel(p)  tntaa1ds      kpjbu
015000090707     C                   CALL      'TNTAA1C'
015100090707     C                   PARM                    KPJBA
015200090707     c
015300000000     C                   SETON                                        LR
015400940726     C*
015500940726     C*--- CARICO PRIMO NUMERO LIBERO PER IL CCC RICHIESTO -----------*
015600900420     C     CARIC         BEGSR
015700930121     C                   Z-ADD     0             AAA
015800060202     C* imposto il PER PRENDERE IL NUMERATORE
015900991202     C                   Z-ADD     151           NUM
016000900420     C*
016100900420     C     KNUF          CHAIN     FLNUF                              30
016200900420     C   30              DO
016300900420     C                   Z-ADD     NUM           NUFCNU
016400900420     C                   Z-ADD     AAA           NUFAAA
016500900420     C                   Z-ADD     VIDFIL        NUFFIL
016600900420     C                   Z-ADD     0             NUFNUM
016700900420     C                   WRITE     FLNUF
016800900420     C     KNUF          CHAIN     FLNUF                              30
016900900420     C                   END
017000900420     C*
017100930121     C                   SETON                                        30
017200930121     C     *IN30         DOWEQ     '1'
017300900420     C                   ADD       1             NUFNUM
017400900420     C     NUFNUM        IFEQ      10000
017500900420     C                   Z-ADD     1             NUFNUM
017600900420     C                   END
017700900420     C*
017800900420     C                   MOVE      NUFNUM        KSC
017900900420     C                   MOVEL     VIDFIL        KSC
018000930121     C     KCLI          SETLL     CNACO000                               30
018100930121     C                   END
018200900420     C*
018300900420     C                   Z-ADD     NUFNUM        SAVNUM            7 0
018400900420     C                   Z-ADD     KSC           VIDKSC
018500900420     C                   SETON                                        42
018600900420     C                   ENDSR
018700900420     C*
018800060202** MSG  Lungh. 78                                                            *
018900090707UTENTE NON ABILITATO ALLA GESTIONE CLIENTI
