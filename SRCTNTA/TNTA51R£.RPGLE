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
001200060202     d wabi            s                   like(UteAut)
001300060202
001400060202     d msg             s             78    dim(10) ctdata perrcd(1)
001500060202     d skpog           s              3    dim(250) inz(*Zeros)
001600060202
001700060202     d Azuteds       e ds                  Extname(Azute00f)
001800060202     d dDatiute      e ds
001900060202     d dlat          e ds
002000060202     d dute01        e ds
002100000000     D KPJBA         E DS
002200060202     d Tibs02ds      e ds
002300060202     d Tibs34ds      e ds
002400060202     d Trul31ds      e ds
002500060202
002600000000     I/SPACE 3
002700000000     C*---------------------------------------------------------------*
002800000000     C     *ENTRY        PLIST
002900000000     C                   PARM                    KPJBA
003000060202
003100060202     c     *dtaara       define    §azute        azuteds
003200060202     c     *dtaara       define    §datiute      ddatiute
003300060202     c                   in(E)     *dtaara
003400060202     c                   If        %error  or rsut = *blanks
003500060202     c                   Clear                   Tibs34ds
003600060202     c                   Call      'TIBS34R'
003700060202     c                   Parm                    Tibs34ds
003800060202     c                   In        *dtaara
003900060202     c                   EndIf
004000060202
004100060202     c                   Clear                   wabi
004200060202     c                   Clear                   dLat
004300060202
004400060202      * Verifica errori e autorità profilo
004500060202s   1c                   Select
004600060202      * se ho errori nei dati utente esco dal pgm
004700060202w   1c                   When      duterr = 'E'
004800060202     c                   Eval      *In08 = *On
004900060202     c                   Eval      *In28 = *On
005000060202     c                   Eval      v1cmsg = msg(01)
005100060202     c                   goto      inizio
005200060202      * se non c'è l'abilitazione
005300060202      * --> se 1° livello, abilitazioni al terminal
005400060202      *     se 2° livello, abilitazioni al punto operativo
005500060202      *     se sede è impossibile ma se capita mando a fine il pgm
005600060202w   1c                   When      uteaut = *Blanks
005700060202if  2c                   If        dutlpo = '1'
005800060202     c                   Eval      wabi   = 'TP'
005900060202e   2c                   EndIf
006000060202if  2c                   If        dutlpo = '2'
006100060202     c                   Eval      wabi   = 'PO'
006200060202e   2c                   EndIf
006300060202if  2c                   If        dutlpo = 'S'
006400060202     c                   Eval      *In08 = *On
006500060202     c                   Eval      *In28 = *On
006600060202     c                   Eval      v1cmsg = msg(01)
006700060202     c                   goto      inizio
006800060202e   2c                   EndIf
006900060202      * carica le abilitazioni del profilo
007000060202x   1c                   Other
007100060202     c                   Movel     utefaf        dute01
007200060202if  2c                   If        §utecli <> *Blanks
007300060202     c                   Eval      wabi = §utecli
007400060202   x2c                   Else
007500060202     c                   Eval      wabi = uteaut
007600060202e   2c                   EndIf
007700060202e   1c                   EndSl
007800060202
007900060202      * controllo se ok l'abilitazione dell'utente
008000060202     c                   Clear                   Tibs02ds
008100060202     c                   Eval      t02mod = 'C'
008200060202     c                   Eval      t02sif = knsif
008300060202     c                   Eval      t02cod = 'LAT'
008400060202     c                   Movel(p)  wabi          t02ke1
008500060202     c                   Call      'TIBS02R'
008600060202     c                   Parm                    kpjba
008700060202     c                   Parm                    Tibs02ds
008800060202     c                   If        t02err = *Blanks
008900060202     c                   Eval      dLat = t02uni
009000060202     c                   EndIf
009100060202      * errore o non abilitato
009200060202     c                   If        t02err <> *Blanks or §latabi = 'S'
009300060202     c                   Eval      *In08 = *On
009400060202     c                   Eval      *In28 = *On
009500060202     c                   Eval      v1cmsg = msg(01)
009600060202     c                   goto      inizio
009700060202     c                   EndIf
009800060202
009900060202      * Reperimento dei P.O. gestibili dall'utente per i codici clienti
010000060202     c                   Clear                   Trul31ds
010100060202     c                   Eval      i31abi = wabi
010200060202     c                   Eval      i31cdi = dutdis
010300060202     c                   Eval      i31car = dutare
010400060202     c                   Eval      i31cpo = dutpou
010500060202     c                   Call      'TRUL31R'
010600060202     c                   Parm                    kpjba
010700060202     c                   Parm                    Trul31ds
010800060202     c                   If        o31pog > *zeros
010900060202     c                   Movea     o31pog        skpog
011000060202     c                   Else
011100060202     c                   Eval      *In08 = *On
011200060202     c                   Eval      *In28 = *On
011300060202     c                   Eval      v1cmsg = msg(01)
011400060202     c                   goto      inizio
011500060202     c                   EndIf
011600940726     C*
011700900420     C*---------------------------------------------------------------*
011800940726     C* ACCESSO TABEL00F
011900900420     C     KTAB          KLIST
012000900420     C                   KFLD                    CODUT
012100940726     C                   KFLD                    COD
012200940726     C* ACCESSO CNACO00F
012300900420     C     KCLI          KLIST
012400900420     C                   KFLD                    CODUT
012500900420     C                   KFLD                    VIDCCC
012600940726     C                   KFLD                    KSC
012700940726     C* ACCESSO FLNUF01L
012800900420     C     KNUF          KLIST
012900940726     C                   KFLD                    AAA
013000940726     C                   KFLD                    NUM
013100900420     C                   KFLD                    VIDFIL
013200940726     C*
013300940726     C* DEFINIZIONE CAMPI
013400940726     C     *LIKE         DEFINE    TBLCOD        COD
013500940726     C     *LIKE         DEFINE    NUFAAA        AAA
013600940726     C     *LIKE         DEFINE    NUFCNU        NUM
013700940726     C     *LIKE         DEFINE    ACOKSC        KSC
013800940726     C*
013900940726     C*---------------------------------------------------------------*
014000900420     C     INIZIO        TAG
014100900420     C                   SETOFF                                       42
014200900420     C                   Z-ADD     0             VIDFIL
014300060202     C                   Z-ADD     dutKCI        VIDCCC
014400900420     C                   Z-ADD     0             KSC
014500900420     C                   MOVEL     *BLANKS       DESFIL
014600900420     C                   MOVEL     *BLANKS       DESCCC
014700900409     C*
014800940726     C                   WRITE     TA51D03
014900000000     C     START         TAG
015000940726     C                   EXFMT     TA51D01
015100900420     C** FINE LAVORO
015200060202     C   KC
015300060202     cor 08              goto      fine
015400900420     C* DECODIFICO
015500900420     C     KCLI          CHAIN     CNACO000                           30
015600900420     C  N30              MOVEL     ACORAG        DESCCC
015700900420     C   30              MOVEL     *BLANKS       DESCCC
015800900420     C* CONTROLLO FILIALE
015900020415      * solo se sono in filiale
016000020415     c                   if        simfel > 0
016100900420     C                   MOVEL     VIDFIL        FILC              3
016200060202     C     FILC          LOOKUP    skpog                                  30
016300020415     c                   endif
016400020507     c*
016500020507     c                   if        vidfil=0
016600020507     c                   setoff                                       30
016700020507     c                   endif
016800060202
016900020507     c                   if        simfel=0
017000020507     c                   setoff                                       30
017100020507     c                   endif
017200961129     C*
017300900420     C  N30              SETON                                            41
017400900420     C  N30              GOTO      START
017500900406     C*
017600900420     C     VIDFIL        CHAIN     AZORG                              30
017700900420     C  N30              MOVEL     ORGDES        DESFIL
017800900420     C   30              MOVEL     *BLANKS       DESFIL
017900900420     C*
018000900420     C* CARICO NUMERO RELATIVO
018100900420     C                   EXSR      CARIC
018200900420     C*
018300940726     C                   WRITE     TA51D04
018400940726     C                   WRITE     TA51D01
018500900420     C*
018600900420     C     FOR02         TAG
018700940726     C                   EXFMT     TA51D02
018800940726     C*  CMD3- FINE LAVORO
018900940726     C   KC              GOTO      FINE
019000900420     C* CMD1- PRENOTAZIONE CODICE
019100900420     C   KA              DO
019200900420     C                   Z-ADD     SAVNUM        NUFNUM
019300900420     C                   UPDATE    FLNUF
019400900420     C                   GOTO      INIZIO
019500900420     C                   END
019600900420     C* ENTER NULLA
019700900420     C                   GOTO      FOR02
019800900420     C*
019900000000     C     FINE          TAG
020000000000     C                   SETON                                        LR
020100940726     C*
020200940726     C*--- CARICO PRIMO NUMERO LIBERO PER IL CCC RICHIESTO -----------*
020300900420     C     CARIC         BEGSR
020400930121     C                   Z-ADD     0             AAA
020500060202     C* imposto il PER PRENDERE IL NUMERATORE
020600991202     C                   Z-ADD     151           NUM
020700900420     C*
020800900420     C     KNUF          CHAIN     FLNUF                              30
020900900420     C   30              DO
021000900420     C                   Z-ADD     NUM           NUFCNU
021100900420     C                   Z-ADD     AAA           NUFAAA
021200900420     C                   Z-ADD     VIDFIL        NUFFIL
021300900420     C                   Z-ADD     0             NUFNUM
021400900420     C                   WRITE     FLNUF
021500900420     C     KNUF          CHAIN     FLNUF                              30
021600900420     C                   END
021700900420     C*
021800930121     C                   SETON                                        30
021900930121     C     *IN30         DOWEQ     '1'
022000900420     C                   ADD       1             NUFNUM
022100900420     C     NUFNUM        IFEQ      10000
022200900420     C                   Z-ADD     1             NUFNUM
022300900420     C                   END
022400900420     C*
022500900420     C                   MOVE      NUFNUM        KSC
022600900420     C                   MOVEL     VIDFIL        KSC
022700930121     C     KCLI          SETLL     CNACO000                               30
022800930121     C                   END
022900900420     C*
023000900420     C                   Z-ADD     NUFNUM        SAVNUM            7 0
023100900420     C                   Z-ADD     KSC           VIDKSC
023200900420     C                   SETON                                        42
023300900420     C                   ENDSR
023400900420     C*
023500060202** MSG  Lungh. 78                                                            *
023600060202Utente non autorizzato alla Gestione anagrafico clienti                       01
