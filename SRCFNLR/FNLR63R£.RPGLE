000100950313     H DECEDIT('0,') DATEDIT(*DMY.)
000200950313      *---------*----------------------------------------------------*
000300950313      * FNLR63R *                                                    *
000400950313      *---------*                                                    *
000500950313      *         - GESTIONE CHIUSURA DISTINTA ESTERO -                *
000600950315      *             -- SUBFILE LINEE DI ARRIVO --                    *
000700950313      *--------------------------------------------------------------*
000800000211     FFNLR63D   CF   E             WORKSTN SFILE(LR63S01:NRR1)
000900000211     FFNCDE01L  IF   E           K DISK    USROPN   COMMIT
001000950315     FTABEL00F  UF   E           K DISK
001100000211     FAZORG01L  IF   E           K DISK
001200950313      *----------------------------------------------------------------*
001300950313      *    s c h i e r e                                               *
001400950313      *----------------------------------------------------------------*
001500120910     D LnaUser         S              3  0 DIM(30)                              FIL. GESTITE
001600950510     D ERR             S             70    DIM(3) CTDATA PERRCD(1)
001700000211      *
001800950313     D                SDS
001900950313     D  NOMPGM                 1     10
002000950313      *----------------------------------------------------------------
002100000211     D FNLR63DS      E DS
002200020830     D FNLv55DS      E DS
002300950313      *
002400020121     D OG143         E DS
002500950313     D DS4G          E DS
002600020828     d trul33ds      e ds
002700020828     d fnlr6ds       e ds
002800020828     D ddatiute      e ds
002900020828     D azuteds       e ds                  extname(AZUTE00F)
003000020828     D tibs34ds      E DS                  inz
003100120910     d**** trul06ds      e ds
003200120910     d****  lin                    1     90  0 dim(30)
003300120910     D TRUL09DS      E DS
003400120910     D  LIN                   19    108  0 DIM(30)                              P.O. COMODO
003500950313      *
003600950313     D KPJBA         E DS
003700950313     D  LIBSYS                92    100
003800950313     D  MSGERR               453    502
003900020830     d dataiso         s               d
004000950313      *---------------------------------------------------------------*
004100950313      * FLUSSO PRINCIPALE                                             *
004200950313      *---------------------------------------------------------------*
004300950313      *  Azzeramento campi primo subfile
004400950313     C                   EXSR      INZS01
004500000211      *
004600950313      *  Loop gestione videate
004700000211     C                   DOU       Wfine = 'S'
004800000211     c                   select
004900000211     C                   when      Wtpvid = '1'
005000000211     C                   exsr      GESS01
005100000211     C                   when      Wtpvid = '4'
005200000211     C                   exsr      GESD04
005300000211     C                   endsl
005400000211     C                   ENDDO
005500000211      *
005600950313     C     FINE          TAG
005700000211      *
005800000211     C                   if        WRICH = 'S'
005900950313     C                   MOVEL     'U'           D63TRI
006000950314     C                   EXSR      CALL63
006100000211     C                   endif
006200950320     C*
006300000211     C                   eval      *inlr = *on
006400950313      *---------------------------------------------------------*
006500950313      *  Inizializzazione del primo subfile                     *
006600950313      *---------------------------------------------------------*
006700950313     C     INZS01        BEGSR
006800950313      *
006900950313     C                   Z-ADD     1             NR1KEY
007000000211     C                   clear                   NRR1
007100000211      *
007200000211     C                   eval      *in20 = *off
007300000211     C                   eval      *in21 = *off
007400000211     C                   eval      *in28 = *off
007500950315     C                   WRITE     LR63C01
007600000211     C                   eval      *in20 = *on
007700000211     C                   eval      *in21 = *on
007800000211     C                   eval      *in40 = *off
007900000211      *
008000000211      *  Carico linee di arrivo estere e DPD
008100950317     C                   DO        §F            X                 3 0
008200000211     C                   clear                   ORGFL1
008300120910     C     LnaUser(x)    CHAIN     AZORG01L
008400020121      *
008500020121      * imposta i campi della DS
008600020121     c                   If        %found
008700020121     C                   movel     ORGDE3        OG143
008800020121     c                   endif
008900000211      *
009000000211     c                   If        %found  and  ORGfva = *blanks  and
009100020729     c                             (§ogNTW = 'FED' or §ogNTW = 'EEX' or
009200020729     c                              §ogNTW = 'EUP' or §ogNTW = 'DPD')
009300000211      *
009400000211     C     ORGFIL        CHAIN     FNCDE01L
009500000211     C                   IF        %found
009600000211     C                   add       1             NRR1              4 0
009700000211     C                   clear                   V1CSCE
009800120910     C                   move      LnaUser(x)    V1CLNA
009900000211     C                   movel     ORGDES        V1DLNA
010000950313     C                   WRITE     LR63S01
010100000211     C                   ENDIF
010200000211      *
010300000211     C                   ENDIF
010400000211     C                   ENDDO
010500000211      *
010600950313     C     NRR1          IFEQ      0
010700950313     C                   MOVEL     '4'           WTPVID
010800950313     C                   END
010900000211      *
011000950313     C                   ENDSR
011100950313      *---------------------------------------------------------*
011200950313      *  Gestione del subfile                                   *
011300950313      *---------------------------------------------------------*
011400950313     C     GESS01        BEGSR
011500950313      *
011600950313     C                   WRITE     LR63Z01
011700950313     C                   EXFMT     LR63C01
011800950313     C                   SETOFF                                       2840
011900950317     C                   SETON                                        3334
012000950313     C                   Z-ADD     1             NR1KEY
012100950606     C                   MOVEL     'N'           WRICA             1
012200950313     C*  Fine Lavoro
012300950313     C     *INKC         IFEQ      '1'
012400950313     C                   MOVEL     'S'           WFINE
012500950313     C                   GOTO      FINVD1
012600950313     C                   END
012700950313     C*  Controlli
012800950313     C                   EXSR      CTR01
012900950606     C*  Ricarica Subfile
013000950606     C     WRICA         IFEQ      'S'
013100950606     C                   EXSR      INZS01
013200950606     C                   END
013300950313     C*
013400950313     C     FINVD1        ENDSR
013500950313      *---------------------------------------------------------*
013600950313      *  Gestione della quarta  videata                         *
013700950313      *---------------------------------------------------------*
013800950313     C     GESD04        BEGSR
013900950313      *
014000950313     C                   EXFMT     LR63D04
014100950317     C                   SETON                                        3334
014200950313     C*  Fine Lavoro
014300950313     C     *INKC         IFEQ      '1'
014400950313     C                   MOVEL     'S'           WFINE
014500950313     C                   END
014600950313     C*
014700950313     C                   ENDSR
014800950313      *---------------------------------------------------------*
014900950313      *  Controlli                                              *
015000950313      *---------------------------------------------------------*
015100950313     C     CTR01         BEGSR
015200020829     c*
015300950313     C                   READC     LR63S01                                32
015400950313     C     *IN32         DOWEQ     '0'
015500950313     C     *IN28         ANDEQ     '0'
015600950317     C     WFINE         ANDNE     'S'
015700950313     C                   SETOFF                                       40
015800950313     C     V1CSCE        IFEQ      '1'
015900020902     C                   Z-ADD     V1CLNA        D63LNA
016000020902     C                   MOVEL     V1DLNA        D63DLA
016100020902     c* reperisce il po di gestione
016200020902     c                   exsr      srfgs
016300020902      * controllo se il numeratore è allocato
016400020902     c                   if        d55tfa <>0
016500020902     C                   CLEAR                   TRUL33DS
016600020902     c                   eval      i33tla = 'L'
016700020902     C                   Z-ADD     0             I33AAA
016800020902     C                   Z-ADD     1             I33ope
016900020902     C                   Z-ADD     12            I33CNU
017000020902     C                   Z-ADD     d55tfa        I33PO1
017100020902     C                   Z-ADD     1             I33NUM
017200020902     c                   movel(p)  trul33ds      kpjbu
017300020902     c                   call      'TRUL33R'
017400020902     c                   parm                    kpjba
017500020902     c                   movel     kpjbu         trul33ds
017600020902     c                   else
017700020902     C                   SETON                                        28
017800020902     C                   GOTO      FINCT1
017900020902     c                   end
018000020902     c* record numeratore allocato
018100020902     C                   IF        O33ERR = 02
018200020902     C                   SETON                                        28
018300020902     C                   MOVEL     ERR(3)        $MSG
018400020902     C                   GOTO      FINCT1
018500020902     C                   END
018600950315     C*
018700000426     C* Aggancio la tabella 4G x reperire i dati del padroncino,
018800000426     C* l'ora di consegna, ecc.. per le distinte delle linee
018900000426     C* estere
019000950315     C                   Z-ADD     1             KKUT
019100950313     C                   MOVEL     '4G'          KCOD
019200950313     C                   MOVEL     *BLANKS       KKEY
019300020904     C                   MOVEL     d55tfa        KKEY
019400950315     C     KTAB          CHAIN     TABEL00F                           3433
019500950313     C     *IN33         IFEQ      '1'
019600950313     C                   SETON                                        4028
019700950313     C                   MOVEL     ERR(1)        $MSG
019800950313     C                   Z-ADD     NRR1          NR1KEY
019900950317     C                   GOTO      UPDREC
020000950313     C                   ELSE
020100950315     C     *IN34         IFEQ      '1'
020200950313     C                   SETON                                        4028
020300950313     C                   MOVEL     ERR(2)        $MSG
020400950313     C                   Z-ADD     NRR1          NR1KEY
020500950317     C                   GOTO      UPDREC
020600950313     C                   ELSE
020700950317     C                   MOVEL     TBLUNI        DS4G
020800950317     C                   EXSR      CALL63
020900950317     C                   MOVEL     ' '           V1CSCE
021000950317     C                   MOVEL     'S'           WRICH             1
021100950313     C                   END
021200950313     C                   END
021300950313     C*
021400950317     C                   END
021500950317     C*
021600950315     C     UPDREC        TAG
021700950317     C                   SETON                                        22
021800950313     C                   UPDATE    LR63S01
021900950313     C                   SETOFF                                       22
022000950313     C  N28              READC     LR63S01                                32
022100950313     C                   END
022200950313     C*
022300950314     C     FINCT1        ENDSR
022400950313      *---------------------------------------------------------*
022500020830      *  reperisce il po di gestione                            *
022600950313      *---------------------------------------------------------*
022700020830     C     srfgs         BEGSR
022800020830     C*
022900020830     c                   clear                   fnlv55ds
023000020830     c                   eval      d55tpt = '6'
023100020830     c                   movel     d63lna        d55lin
023200020830     c                   movel     *date         dataiso
023300020830     c                   movel     dataiso       d55drf
023400020830     c                   call      'FNLV55R'
023500020902     c                   parm                    fnlv55ds
023600020830     C*
023700020830     C                   ENDSR
023800020830      *---------------------------------------------------------*
023900020830      *  Controlli                                              *
024000020830      *---------------------------------------------------------*
024100020830     C     CALL63        BEGSR
024200950313      *
024300950313      *  Aggiorno il numeratore delle distinte
024400950320     C     D63TRI        IFNE      'U'
024500020830     c* prende il numero della distinta del po in gestione
024600020828     c                   do        *hival
024700020828     C                   CLEAR                   TRUL33DS
024800020829     c                   eval      i33tla = 'L'
024900020828     C                   Z-ADD     0             I33AAA
025000020828     C                   Z-ADD     12            I33CNU
025100020830     C                   Z-ADD     d55tfa        I33PO1
025200020828     C                   Z-ADD     1             I33NUM
025300020829     c                   movel(p)  trul33ds      kpjbu
025400020829     c                   call      'TRUL33R'
025500020829     c                   parm                    kpjba
025600020829     c                   movel     kpjbu         trul33ds
025700020828     C                   IF        O33ERR = 0
025800020828     c* controllo che il numero non è già stato usato
025900020828     c                   clear                   fnlr6ds
026000020828     c                   eval      d6tla = 'L'
026100020828     C                   Z-ADD     8             D6npg
026200020902     C                   Z-ADD     d55tfa        D6fgs
026300020828     C                   Z-ADD     O33Nrf        D6NDT
026400020829     c                   movel(p)  fnlr6ds       kpjbu
026500020828     c                   call      'FNLR6R'
026600020829     c                   parm                    kpjba
026700020829     c                   movel     kpjbu         fnlr6ds
026800020828     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
026900020828     c* come foglio o in una bolla
027000020828     c                   if        d6flg <> 'N'
027100020828     c                   iter
027200020828     c                   else
027300020828     C                   Z-ADD     O33Nrf        D63NDT
027400020828     c                   leave
027500020828     C                   END
027600020828     C                   END
027700020828     C                   ENDdo
027800950320     C                   END
027900950313     C*
028000000426      *  Passo i dati reperiti dalla 4G al pgm di caricamento dati
028100950315     C                   MOVEL     *BLANKS       D63F03
028200950315     C                   MOVEL     *BLANKS       D63F12
028300950315     C                   Z-ADD     §4GPDR        D63PDR
028400950314     C                   Z-ADD     §4GHMC        D63HMC
028500950314     C                   MOVEL     §4GFPP        D63FPP
028600950314     C                   MOVEL     §4GPEP        D63PEP
028700000211     C                   MOVEL     FNLR63DS      KPJBU
028800950313     C                   CALL      'FNLR63R1'
028900950313     C                   PARM                    KPJBA
029000000211     C                   MOVEL     KPJBU         FNLR63DS
029100950313     C*
029200950317     C     KTAB          CHAIN     TABEL00F                           3433
029300950315     C  N33
029400950315     CANN34              EXCEPT    NOAGG
029500950313     C*  Fine Lavoro
029600950314     C     D63F03        IFEQ      '1'
029700950313     C                   MOVEL     'S'           WFINE
029800950313     C                   SETON                                        31
029900950606     C                   ELSE
030000950606     C                   MOVEL     'S'           WRICA             1
030100950313     C                   END
030200950315     C*
030300000211     C                   CLEAR                   FNLR63DS
030400950313     C*
030500950313     C                   ENDSR
030600950313     C*-----------------------------------------------------*
030700950313     C*  Operazioni iniziali
030800950313     C*-----------------------------------------------------*
030900950313     C     *INZSR        BEGSR
031000950313     C*
031100950313     C     *ENTRY        PLIST
031200950313     C                   PARM                    KPJBA
031300950313     C*-----------------------------------------------------*
031400020828     C* Decodifica Elaboratore
031500020828     c     *dtaara       define    §azute        azuteds
031600020828     c     *dtaara       define    §datiute      ddatiute
031700020828     C                   in(E)     *dtaara
031800020828     C                   IF        %Error  or  RSUT = *blanks
031900020828     C                   call      'TIBS34R'
032000020828     C                   parm                    Tibs34Ds
032100020828     C                   in        *dtaara
032200020828     c                   ENDIF
032300950314     C*
032400950313     C     KTAB          KLIST
032500950313     C                   KFLD                    KKUT
032600950313     C                   KFLD                    KCOD
032700950313     C                   KFLD                    KKEY
032800950313     C*----------------------------------------------------*
032900950313     C*  Definizione delle variabili
033000950313     C     *LIKE         DEFINE    TBLKUT        KKUT
033100950313     C     *LIKE         DEFINE    TBLCOD        KCOD
033200950313     C     *LIKE         DEFINE    TBLKEY        KKEY
033300950315     C*----------------------------------------------------*
033400950420     C                   CLEAR                   DS4G
033500000211     C                   CLEAR                   FNLR63DS
033600950314     C                   TIME                    WHHDAT           12 0
033700950313     C                   MOVEL     ' '           WFINE             1
033800950313     C                   MOVEL     '1'           WTPVID            1
033900950313     C                   MOVEL     'P'           D63TRI
034000950317     C                   MOVEL     'N'           WRICH
034100950315     C*-----------------------------------------------------*
034200950315     C* Apertura controllata dei files
034300950315     C                   OPEN      FNCDE01L                             16
034400950315     C   16              GOTO      FINE                                         OPEN vado a fine
034500950313     C*----------------------------------------------------*
034600950313     C*  Caricamento £1
034700120910     c*******            clear                   trul06ds
034800120910     c*******            eval      d06cod = '£1'
034900120910     c*******            movel     dutpou        d06key
035000120910     c*******            movel     trul06ds      kpjbu
035100120910     c*******            call      'TRUL06R'
035200120910     c*******            parm                    kpjba
035300120910     c*******            movel     kpjbu         trul06ds
035400120910     c*******            movea     lin           £1
035500120910      **
035600120910     c* Carico x terminal  di arrivo
035700120910     c                   clear                   trul09ds
035800120910     c                   movel     dutPOU        i09tfa
035900120910     c                   movel     'TFA'         i09mod
036000120910     c                   call      'TRUL09R'
036100120910     c                   parm                    trul09ds
036200120910     c                   movea     lin           LnaUser
036300120910     c
036400020506     C                   Z-ADD     0             §F                3 0
036500020730     c                   Z-ADD     1             XZ                3 0
036600020730      * Deve contare quante sono le filiali gestite per caricare in seguito il sfl
036700120910     C     *zeros        LOOKUP    LnaUser(xz)                            51
036800020730     C   51              if        XZ > 0
036900020730     c     xz            sub       1             §F
037000020730     c                   endif
037100020730      *
037200950313     C*
037300950313     C                   ENDSR
037400950313     OTABEL     E            NOAGG
037500950315**
037600950315Un altro operatore sta chiudendo la stessa linea di arrivo !!!        01
037700950315Linea di arrivo non presente su tabella 4G !!!                        02
037800020829Numeratore distinta di consegna allocato attendi 5'' e ritenta        03
