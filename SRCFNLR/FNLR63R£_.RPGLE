000100950313     H DECEDIT('0,') DATEDIT(*DMY.)
000200950313      *---------*----------------------------------------------------*
000300950313      * FNLR63R *                                                    *
000400950313      *---------*                                                    *
000500950313      *         - GESTIONE CHIUSURA DISTINTA ESTERO -                *
000600950315      *             -- SUBFILE LINEE DI ARRIVO --                    *
000700950313      *--------------------------------------------------------------*
000800000211     FFNLR63D   CF   E             WORKSTN SFILE(LR63S01:NRR1)
001000000211     FFNCDE01L  IF   E           K DISK    USROPN   COMMIT
001100950315     FTABEL00F  UF   E           K DISK
001200000211     FAZORG01L  IF   E           K DISK
001300950313      *----------------------------------------------------------------*
001400950313      *    s c h i e r e                                               *
001500950313      *----------------------------------------------------------------*
001700020506     D £1              S              3  0 DIM(30)                              FIL. GESTITE
001800950510     D ERR             S             70    DIM(3) CTDATA PERRCD(1)
001900000211      *
002000950313     D                SDS
002100950313     D  NOMPGM                 1     10
002200950313      *----------------------------------------------------------------
002300000211     D FNLR63DS      E DS
002301020830     D FNLv55DS      E DS
002400950313      *
002500020121     D OG143         E DS
002600950313     D DS4G          E DS
002601020828     d trul33ds      e ds
002602020828     d fnlr6ds       e ds
002701020828     D ddatiute      e ds
002702020828     D azuteds       e ds                  extname(AZUTE00F)
002703020828     D tibs34ds      E DS                  inz
002800020506     d trul06ds      e ds
002900020506     d  lin                    1     90  0 dim(30)
003000950313      *
003100950313     D KPJBA         E DS
003200950313     D  LIBSYS                92    100
003300950313     D  MSGERR               453    502
003301020830     d dataiso         s               d
003400950313      *---------------------------------------------------------------*
003500950313      * FLUSSO PRINCIPALE                                             *
003600950313      *---------------------------------------------------------------*
003700950313      *  Azzeramento campi primo subfile
003800950313     C                   EXSR      INZS01
003900000211      *
004000950313      *  Loop gestione videate
004100000211     C                   DOU       Wfine = 'S'
004200000211     c                   select
004300000211     C                   when      Wtpvid = '1'
004400000211     C                   exsr      GESS01
004500000211     C                   when      Wtpvid = '4'
004600000211     C                   exsr      GESD04
004700000211     C                   endsl
004800000211     C                   ENDDO
004900000211      *
005000950313     C     FINE          TAG
005100000211      *
005200000211     C                   if        WRICH = 'S'
005300950313     C                   MOVEL     'U'           D63TRI
005400950314     C                   EXSR      CALL63
005500000211     C                   endif
005600950320     C*
005700000211     C                   eval      *inlr = *on
005800950313      *---------------------------------------------------------*
005900950313      *  Inizializzazione del primo subfile                     *
006000950313      *---------------------------------------------------------*
006100950313     C     INZS01        BEGSR
006200950313      *
006300950313     C                   Z-ADD     1             NR1KEY
006400000211     C                   clear                   NRR1
006500000211      *
006600000211     C                   eval      *in20 = *off
006700000211     C                   eval      *in21 = *off
006800000211     C                   eval      *in28 = *off
006900950315     C                   WRITE     LR63C01
007000000211     C                   eval      *in20 = *on
007100000211     C                   eval      *in21 = *on
007200000211     C                   eval      *in40 = *off
007300000211      *
007400000211      *  Carico linee di arrivo estere e DPD
007500950317     C                   DO        §F            X                 3 0
007600000211     C                   clear                   ORGFL1
007700000211     C     £1(X)         CHAIN     AZORG01L
007800020121      *
007900020121      * imposta i campi della DS
008000020121     c                   If        %found
008100020121     C                   movel     ORGDE3        OG143
008200020121     c                   endif
008300000211      *
008400000211     c                   If        %found  and  ORGfva = *blanks  and
008600020729     c                             (§ogNTW = 'FED' or §ogNTW = 'EEX' or
008700020729     c                              §ogNTW = 'EUP' or §ogNTW = 'DPD')
008900000211      *
009000000211     C     ORGFIL        CHAIN     FNCDE01L
009100000211     C                   IF        %found
009200000211     C                   add       1             NRR1              4 0
009300000211     C                   clear                   V1CSCE
009400000211     C                   move      £1(X)         V1CLNA
009500000211     C                   movel     ORGDES        V1DLNA
009600950313     C                   WRITE     LR63S01
009700000211     C                   ENDIF
009800000211      *
009900000211     C                   ENDIF
010000000211     C                   ENDDO
010100000211      *
010200950313     C     NRR1          IFEQ      0
010300950313     C                   MOVEL     '4'           WTPVID
010400950313     C                   END
010500000211      *
010600950313     C                   ENDSR
010700950313      *---------------------------------------------------------*
010800950313      *  Gestione del subfile                                   *
010900950313      *---------------------------------------------------------*
011000950313     C     GESS01        BEGSR
011100950313      *
011200950313     C                   WRITE     LR63Z01
011300950313     C                   EXFMT     LR63C01
011400950313     C                   SETOFF                                       2840
011500950317     C                   SETON                                        3334
011600950313     C                   Z-ADD     1             NR1KEY
011700950606     C                   MOVEL     'N'           WRICA             1
011800950313     C*  Fine Lavoro
011900950313     C     *INKC         IFEQ      '1'
012000950313     C                   MOVEL     'S'           WFINE
012100950313     C                   GOTO      FINVD1
012200950313     C                   END
012300950313     C*  Controlli
012400950313     C                   EXSR      CTR01
012500950606     C*  Ricarica Subfile
012600950606     C     WRICA         IFEQ      'S'
012700950606     C                   EXSR      INZS01
012800950606     C                   END
012900950313     C*
013000950313     C     FINVD1        ENDSR
013100950313      *---------------------------------------------------------*
013200950313      *  Gestione della quarta  videata                         *
013300950313      *---------------------------------------------------------*
013400950313     C     GESD04        BEGSR
013500950313      *
013600950313     C                   EXFMT     LR63D04
013700950317     C                   SETON                                        3334
013800950313     C*  Fine Lavoro
013900950313     C     *INKC         IFEQ      '1'
014000950313     C                   MOVEL     'S'           WFINE
014100950313     C                   END
014200950313     C*
014300950313     C                   ENDSR
014400950313      *---------------------------------------------------------*
014500950313      *  Controlli                                              *
014600950313      *---------------------------------------------------------*
014700950313     C     CTR01         BEGSR
016224020829     c*
016300950313     C                   READC     LR63S01                                32
016400950313     C     *IN32         DOWEQ     '0'
016500950313     C     *IN28         ANDEQ     '0'
016600950317     C     WFINE         ANDNE     'S'
016700950313     C                   SETOFF                                       40
016800950313     C     V1CSCE        IFEQ      '1'
016801020902     C                   Z-ADD     V1CLNA        D63LNA
016802020902     C                   MOVEL     V1DLNA        D63DLA
016803020902     c* reperisce il po di gestione
016804020902     c                   exsr      srfgs
016805020902      * controllo se il numeratore è allocato
016806020902     c                   if        d55tfa <>0
016807020902     C                   CLEAR                   TRUL33DS
016808020902     c                   eval      i33tla = 'L'
016809020902     C                   Z-ADD     0             I33AAA
016810020902     C                   Z-ADD     1             I33ope
016811020902     C                   Z-ADD     12            I33CNU
016812020902     C                   Z-ADD     d55tfa        I33PO1
016813020902     C                   Z-ADD     1             I33NUM
016814020902     c                   movel(p)  trul33ds      kpjbu
016815020902     c                   call      'TRUL33R'
016816020902     c                   parm                    kpjba
016817020902     c                   movel     kpjbu         trul33ds
016818020902     c                   else
016819020902     C                   SETON                                        28
016820020902     C                   GOTO      FINCT1
016821020902     c                   end
016822020902     c* record numeratore allocato
016823020902     C                   IF        O33ERR = 02
016824020902     C                   SETON                                        28
016825020902     C                   MOVEL     ERR(3)        $MSG
016826020902     C                   GOTO      FINCT1
016827020902     C                   END
016900950315     C*
017000000426     C* Aggancio la tabella 4G x reperire i dati del padroncino,
017100000426     C* l'ora di consegna, ecc.. per le distinte delle linee
017200000426     C* estere
017300950315     C                   Z-ADD     1             KKUT
017400950313     C                   MOVEL     '4G'          KCOD
017500950313     C                   MOVEL     *BLANKS       KKEY
017600020904     C                   MOVEL     d55tfa        KKEY
017900950315     C     KTAB          CHAIN     TABEL00F                           3433
018000950313     C     *IN33         IFEQ      '1'
018100950313     C                   SETON                                        4028
018200950313     C                   MOVEL     ERR(1)        $MSG
018300950313     C                   Z-ADD     NRR1          NR1KEY
018400950317     C                   GOTO      UPDREC
018500950313     C                   ELSE
018600950315     C     *IN34         IFEQ      '1'
018700950313     C                   SETON                                        4028
018800950313     C                   MOVEL     ERR(2)        $MSG
018900950313     C                   Z-ADD     NRR1          NR1KEY
019000950317     C                   GOTO      UPDREC
019100950313     C                   ELSE
019200950317     C                   MOVEL     TBLUNI        DS4G
019300950317     C                   EXSR      CALL63
019400950317     C                   MOVEL     ' '           V1CSCE
019500950317     C                   MOVEL     'S'           WRICH             1
019600950313     C                   END
019700950313     C                   END
019800950313     C*
019900950317     C                   END
020000950317     C*
020100950315     C     UPDREC        TAG
020200950317     C                   SETON                                        22
020300950313     C                   UPDATE    LR63S01
020400950313     C                   SETOFF                                       22
020500950313     C  N28              READC     LR63S01                                32
020600950313     C                   END
020700950313     C*
020800950314     C     FINCT1        ENDSR
020900950313      *---------------------------------------------------------*
021000020830      *  reperisce il po di gestione                            *
021100950313      *---------------------------------------------------------*
021200020830     C     srfgs         BEGSR
021201020830     C*
021204020830     c                   clear                   fnlv55ds
021205020830     c                   eval      d55tpt = '6'
021206020830     c                   movel     d63lna        d55lin
021207020830     c                   movel     *date         dataiso
021208020830     c                   movel     dataiso       d55drf
021210020830     c                   call      'FNLV55R'
021211020902     c                   parm                    fnlv55ds
021218020830     C*
021219020830     C                   ENDSR
021220020830      *---------------------------------------------------------*
021221020830      *  Controlli                                              *
021222020830      *---------------------------------------------------------*
021223020830     C     CALL63        BEGSR
021300950313      *
021400950313      *  Aggiorno il numeratore delle distinte
021500950320     C     D63TRI        IFNE      'U'
021501020830     c* prende il numero della distinta del po in gestione
021502020828     c                   do        *hival
021503020828     C                   CLEAR                   TRUL33DS
021504020829     c                   eval      i33tla = 'L'
021505020828     C                   Z-ADD     0             I33AAA
021506020828     C                   Z-ADD     12            I33CNU
021507020830     C                   Z-ADD     d55tfa        I33PO1
021508020828     C                   Z-ADD     1             I33NUM
021509020829     c                   movel(p)  trul33ds      kpjbu
021510020829     c                   call      'TRUL33R'
021511020829     c                   parm                    kpjba
021512020829     c                   movel     kpjbu         trul33ds
021513020828     C                   IF        O33ERR = 0
021514020828     c* controllo che il numero non è già stato usato
021515020828     c                   clear                   fnlr6ds
021516020828     c                   eval      d6tla = 'L'
021517020828     C                   Z-ADD     8             D6npg
021518020902     C                   Z-ADD     d55tfa        D6fgs
021519020828     C                   Z-ADD     O33Nrf        D6NDT
021520020829     c                   movel(p)  fnlr6ds       kpjbu
021521020828     c                   call      'FNLR6R'
021522020829     c                   parm                    kpjba
021523020829     c                   movel     kpjbu         fnlr6ds
021524020828     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
021525020828     c* come foglio o in una bolla
021526020828     c                   if        d6flg <> 'N'
021527020828     c                   iter
021528020828     c                   else
021529020828     C                   Z-ADD     O33Nrf        D63NDT
021530020828     c                   leave
021531020828     C                   END
021532020828     C                   END
021533020828     C                   ENDdo
022900950320     C                   END
023000950313     C*
023100000426      *  Passo i dati reperiti dalla 4G al pgm di caricamento dati
023200950315     C                   MOVEL     *BLANKS       D63F03
023300950315     C                   MOVEL     *BLANKS       D63F12
023400950315     C                   Z-ADD     §4GPDR        D63PDR
023500950314     C                   Z-ADD     §4GHMC        D63HMC
023600950314     C                   MOVEL     §4GFPP        D63FPP
023700950314     C                   MOVEL     §4GPEP        D63PEP
023800000211     C                   MOVEL     FNLR63DS      KPJBU
023900950313     C                   CALL      'FNLR63R1'
024000950313     C                   PARM                    KPJBA
024100000211     C                   MOVEL     KPJBU         FNLR63DS
024200950313     C*
024300950317     C     KTAB          CHAIN     TABEL00F                           3433
024400950315     C  N33
024500950315     CANN34              EXCEPT    NOAGG
024600950313     C*  Fine Lavoro
024700950314     C     D63F03        IFEQ      '1'
024800950313     C                   MOVEL     'S'           WFINE
024900950313     C                   SETON                                        31
025000950606     C                   ELSE
025100950606     C                   MOVEL     'S'           WRICA             1
025200950313     C                   END
025300950315     C*
025400000211     C                   CLEAR                   FNLR63DS
025500950313     C*
025600950313     C                   ENDSR
025700950313     C*-----------------------------------------------------*
025800950313     C*  Operazioni iniziali
025900950313     C*-----------------------------------------------------*
026000950313     C     *INZSR        BEGSR
026100950313     C*
026200950313     C     *ENTRY        PLIST
026300950313     C                   PARM                    KPJBA
026400950313     C*-----------------------------------------------------*
026401020828     C* Decodifica Elaboratore
026402020828     c     *dtaara       define    §azute        azuteds
026403020828     c     *dtaara       define    §datiute      ddatiute
026404020828     C                   in(E)     *dtaara
026405020828     C                   IF        %Error  or  RSUT = *blanks
026406020828     C                   call      'TIBS34R'
026407020828     C                   parm                    Tibs34Ds
026408020828     C                   in        *dtaara
026409020828     c                   ENDIF
027300950314     C*
027400950313     C     KTAB          KLIST
027500950313     C                   KFLD                    KKUT
027600950313     C                   KFLD                    KCOD
027700950313     C                   KFLD                    KKEY
027800950313     C*----------------------------------------------------*
027900950313     C*  Definizione delle variabili
028200950313     C     *LIKE         DEFINE    TBLKUT        KKUT
028300950313     C     *LIKE         DEFINE    TBLCOD        KCOD
028400950313     C     *LIKE         DEFINE    TBLKEY        KKEY
028500950315     C*----------------------------------------------------*
028600950420     C                   CLEAR                   DS4G
028700000211     C                   CLEAR                   FNLR63DS
028800950314     C                   TIME                    WHHDAT           12 0
028900950313     C                   MOVEL     ' '           WFINE             1
029000950313     C                   MOVEL     '1'           WTPVID            1
029100950313     C                   MOVEL     'P'           D63TRI
029300950317     C                   MOVEL     'N'           WRICH
029400950315     C*-----------------------------------------------------*
029500950315     C* Apertura controllata dei files
029600950315     C                   OPEN      FNCDE01L                             16
029700950315     C   16              GOTO      FINE                                         OPEN vado a fine
029800950313     C*----------------------------------------------------*
029900950313     C*  Caricamento £1
030000020506     c                   clear                   trul06ds
030100020506     c                   eval      d06cod = '£1'
030200020828     c                   movel     dutpou        d06key
030300020506     c                   movel     trul06ds      kpjbu
030400020506     c                   call      'TRUL06R'
030500020506     c                   parm                    kpjba
030600020506     c                   movel     kpjbu         trul06ds
030700020506     c                   movea     lin           £1
030800020506     C                   Z-ADD     0             §F                3 0
030900020730     c                   Z-ADD     1             XZ                3 0
031000020730      * Deve contare quante sono le filiali gestite per caricare in seguito il sfl
031100020730     C     *zeros        LOOKUP    £1(XZ)                                 51
031200020730     C   51              if        XZ > 0
031300020730     c     xz            sub       1             §F
031400020730     c                   endif
031500020730      *
031600950313     C*
031700950313     C                   ENDSR
031800950313     OTABEL     E            NOAGG
031900950315**
032000950315Un altro operatore sta chiudendo la stessa linea di arrivo !!!        01
032100950315Linea di arrivo non presente su tabella 4G !!!                        02
032200020829Numeratore distinta di consegna allocato attendi 5'' e ritenta        03
