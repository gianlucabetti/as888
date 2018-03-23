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
001500170509     D LnaUser         S              3  0 DIM(100)                             FIL. GESTITE
001600170509     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
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
003300170509     D TRUL09DS      E DS
003400170509     D  LIN                   19    108  0 DIM(30)                              P.O. COMODO
003500170509      ****-?Caricamento filiali contabili
003600170509     d TRUL31ds      e ds                  inz
003700170509     d $POg            s              3    inz   dim(250)
003800170509      ****-?
003900950313      *
004000950313     D KPJBA         E DS
004100950313     D  LIBSYS                92    100
004200950313     D  MSGERR               453    502
004300020830     d dataiso         s               d
004400950313      *---------------------------------------------------------------*
004500950313      * FLUSSO PRINCIPALE                                             *
004600950313      *---------------------------------------------------------------*
004700950313      *  Azzeramento campi primo subfile
004800950313     C                   EXSR      INZS01
004900000211      *
005000950313      *  Loop gestione videate
005100000211     C                   DOU       Wfine = 'S'
005200000211     c                   select
005300000211     C                   when      Wtpvid = '1'
005400000211     C                   exsr      GESS01
005500000211     C                   when      Wtpvid = '4'
005600000211     C                   exsr      GESD04
005700000211     C                   endsl
005800000211     C                   ENDDO
005900000211      *
006000950313     C     FINE          TAG
006100000211      *
006200000211     C                   if        WRICH = 'S'
006300950313     C                   MOVEL     'U'           D63TRI
006400950314     C                   EXSR      CALL63
006500000211     C                   endif
006600950320     C*
006700000211     C                   eval      *inlr = *on
006800950313      *---------------------------------------------------------*
006900950313      *  Inizializzazione del primo subfile                     *
007000950313      *---------------------------------------------------------*
007100950313     C     INZS01        BEGSR
007200950313      *
007300950313     C                   Z-ADD     1             NR1KEY
007400000211     C                   clear                   NRR1
007500000211      *
007600000211     C                   eval      *in20 = *off
007700000211     C                   eval      *in21 = *off
007800000211     C                   eval      *in28 = *off
007900950315     C                   WRITE     LR63C01
008000000211     C                   eval      *in20 = *on
008100000211     C                   eval      *in21 = *on
008200000211     C                   eval      *in40 = *off
008300000211      *
008400000211      *  Carico linee di arrivo estere e DPD
008500950317     C                   DO        §F            X                 3 0
008600000211     C                   clear                   ORGFL1
008700120910     C     LnaUser(x)    CHAIN     AZORG01L
008800020121      *
008900020121      * imposta i campi della DS
009000020121     c                   If        %found
009100020121     C                   movel     ORGDE3        OG143
009200020121     c                   endif
009300000211      *
009400000211     c                   If        %found  and  ORGfva = *blanks  and
009500020729     c                             (§ogNTW = 'FED' or §ogNTW = 'EEX' or
009600020729     c                              §ogNTW = 'EUP' or §ogNTW = 'DPD')
009700000211      *
009800000211     C     ORGFIL        CHAIN     FNCDE01L
009900000211     C                   IF        %found
010000000211     C                   add       1             NRR1              4 0
010100000211     C                   clear                   V1CSCE
010200120910     C                   move      LnaUser(x)    V1CLNA
010300000211     C                   movel     ORGDES        V1DLNA
010400950313     C                   WRITE     LR63S01
010500000211     C                   ENDIF
010600000211      *
010700000211     C                   ENDIF
010800000211     C                   ENDDO
010900000211      *
011000950313     C     NRR1          IFEQ      0
011100950313     C                   MOVEL     '4'           WTPVID
011200950313     C                   END
011300000211      *
011400950313     C                   ENDSR
011500950313      *---------------------------------------------------------*
011600950313      *  Gestione del subfile                                   *
011700950313      *---------------------------------------------------------*
011800950313     C     GESS01        BEGSR
011900950313      *
012000950313     C                   WRITE     LR63Z01
012100950313     C                   EXFMT     LR63C01
012200950313     C                   SETOFF                                       2840
012300950317     C                   SETON                                        3334
012400950313     C                   Z-ADD     1             NR1KEY
012500950606     C                   MOVEL     'N'           WRICA             1
012600950313     C*  Fine Lavoro
012700950313     C     *INKC         IFEQ      '1'
012800950313     C                   MOVEL     'S'           WFINE
012900950313     C                   GOTO      FINVD1
013000950313     C                   END
013100950313     C*  Controlli
013200950313     C                   EXSR      CTR01
013300950606     C*  Ricarica Subfile
013400950606     C     WRICA         IFEQ      'S'
013500950606     C                   EXSR      INZS01
013600950606     C                   END
013700950313     C*
013800950313     C     FINVD1        ENDSR
013900950313      *---------------------------------------------------------*
014000950313      *  Gestione della quarta  videata                         *
014100950313      *---------------------------------------------------------*
014200950313     C     GESD04        BEGSR
014300950313      *
014400950313     C                   EXFMT     LR63D04
014500950317     C                   SETON                                        3334
014600950313     C*  Fine Lavoro
014700950313     C     *INKC         IFEQ      '1'
014800950313     C                   MOVEL     'S'           WFINE
014900950313     C                   END
015000950313     C*
015100950313     C                   ENDSR
015200950313      *---------------------------------------------------------*
015300950313      *  Controlli                                              *
015400950313      *---------------------------------------------------------*
015500950313     C     CTR01         BEGSR
015600020829     c*
015700950313     C                   READC     LR63S01                                32
015800950313     C     *IN32         DOWEQ     '0'
015900950313     C     *IN28         ANDEQ     '0'
016000950317     C     WFINE         ANDNE     'S'
016100950313     C                   SETOFF                                       40
016200950313     C     V1CSCE        IFEQ      '1'
016300020902     C                   Z-ADD     V1CLNA        D63LNA
016400020902     C                   MOVEL     V1DLNA        D63DLA
016500020902     c* reperisce il po di gestione
016600020902     c                   exsr      srfgs
016700020902      * controllo se il numeratore è allocato
016800020902     c                   if        d55tfa <>0
016900020902     C                   CLEAR                   TRUL33DS
017000020902     c                   eval      i33tla = 'L'
017100020902     C                   Z-ADD     0             I33AAA
017200020902     C                   Z-ADD     1             I33ope
017300020902     C                   Z-ADD     12            I33CNU
017400020902     C                   Z-ADD     d55tfa        I33PO1
017500020902     C                   Z-ADD     1             I33NUM
017600020902     c                   movel(p)  trul33ds      kpjbu
017700020902     c                   call      'TRUL33R'
017800020902     c                   parm                    kpjba
017900020902     c                   movel     kpjbu         trul33ds
018000020902     c                   else
018100020902     C                   SETON                                        28
018200020902     C                   GOTO      FINCT1
018300020902     c                   end
018400020902     c* record numeratore allocato
018500020902     C                   IF        O33ERR = 02
018600020902     C                   SETON                                        28
018700020902     C                   MOVEL     ERR(3)        $MSG
018800020902     C                   GOTO      FINCT1
018900020902     C                   END
019000950315     C*
019100000426     C* Aggancio la tabella 4G x reperire i dati del padroncino,
019200000426     C* l'ora di consegna, ecc.. per le distinte delle linee
019300000426     C* estere
019400950315     C                   Z-ADD     1             KKUT
019500950313     C                   MOVEL     '4G'          KCOD
019600950313     C                   MOVEL     *BLANKS       KKEY
019700020904     C                   MOVEL     d55tfa        KKEY
019800950315     C     KTAB          CHAIN     TABEL00F                           3433
019900950313     C     *IN33         IFEQ      '1'
020000950313     C                   SETON                                        4028
020100950313     C                   MOVEL     ERR(1)        $MSG
020200950313     C                   Z-ADD     NRR1          NR1KEY
020300950317     C                   GOTO      UPDREC
020400950313     C                   ELSE
020500950315     C     *IN34         IFEQ      '1'
020600950313     C                   SETON                                        4028
020700950313     C                   MOVEL     ERR(2)        $MSG
020800950313     C                   Z-ADD     NRR1          NR1KEY
020900950317     C                   GOTO      UPDREC
021000950313     C                   ELSE
021100950317     C                   MOVEL     TBLUNI        DS4G
021200950317     C                   EXSR      CALL63
021300950317     C                   MOVEL     ' '           V1CSCE
021400950317     C                   MOVEL     'S'           WRICH             1
021500950313     C                   END
021600950313     C                   END
021700950313     C*
021800950317     C                   END
021900950317     C*
022000950315     C     UPDREC        TAG
022100950317     C                   SETON                                        22
022200950313     C                   UPDATE    LR63S01
022300950313     C                   SETOFF                                       22
022400950313     C  N28              READC     LR63S01                                32
022500950313     C                   END
022600950313     C*
022700950314     C     FINCT1        ENDSR
022800950313      *---------------------------------------------------------*
022900020830      *  reperisce il po di gestione                            *
023000950313      *---------------------------------------------------------*
023100020830     C     srfgs         BEGSR
023200020830     C*
023300020830     c                   clear                   fnlv55ds
023400020830     c                   eval      d55tpt = '6'
023500020830     c                   movel     d63lna        d55lin
023600020830     c                   movel     *date         dataiso
023700020830     c                   movel     dataiso       d55drf
023800020830     c                   call      'FNLV55R'
023900020902     c                   parm                    fnlv55ds
024000020830     C*
024100020830     C                   ENDSR
024200020830      *---------------------------------------------------------*
024300020830      *  Controlli                                              *
024400020830      *---------------------------------------------------------*
024500020830     C     CALL63        BEGSR
024600950313      *
024700950313      *  Aggiorno il numeratore delle distinte
024800950320     C     D63TRI        IFNE      'U'
024900020830     c* prende il numero della distinta del po in gestione
025000020828     c                   do        *hival
025100020828     C                   CLEAR                   TRUL33DS
025200020829     c                   eval      i33tla = 'L'
025300020828     C                   Z-ADD     0             I33AAA
025400020828     C                   Z-ADD     12            I33CNU
025500020830     C                   Z-ADD     d55tfa        I33PO1
025600020828     C                   Z-ADD     1             I33NUM
025700020829     c                   movel(p)  trul33ds      kpjbu
025800020829     c                   call      'TRUL33R'
025900020829     c                   parm                    kpjba
026000020829     c                   movel     kpjbu         trul33ds
026100020828     C                   IF        O33ERR = 0
026200020828     c* controllo che il numero non è già stato usato
026300020828     c                   clear                   fnlr6ds
026400020828     c                   eval      d6tla = 'L'
026500020828     C                   Z-ADD     8             D6npg
026600020902     C                   Z-ADD     d55tfa        D6fgs
026700020828     C                   Z-ADD     O33Nrf        D6NDT
026800020829     c                   movel(p)  fnlr6ds       kpjbu
026900020828     c                   call      'FNLR6R'
027000020829     c                   parm                    kpjba
027100020829     c                   movel     kpjbu         fnlr6ds
027200020828     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
027300020828     c* come foglio o in una bolla
027400020828     c                   if        d6flg <> 'N'
027500020828     c                   iter
027600020828     c                   else
027700020828     C                   Z-ADD     O33Nrf        D63NDT
027800020828     c                   leave
027900020828     C                   END
028000020828     C                   END
028100020828     C                   ENDdo
028200950320     C                   END
028300950313     C*
028400000426      *  Passo i dati reperiti dalla 4G al pgm di caricamento dati
028500950315     C                   MOVEL     *BLANKS       D63F03
028600950315     C                   MOVEL     *BLANKS       D63F12
028700950315     C                   Z-ADD     §4GPDR        D63PDR
028800950314     C                   Z-ADD     §4GHMC        D63HMC
028900950314     C                   MOVEL     §4GFPP        D63FPP
029000950314     C                   MOVEL     §4GPEP        D63PEP
029100000211     C                   MOVEL     FNLR63DS      KPJBU
029200950313     C                   CALL      'FNLR63R1'
029300950313     C                   PARM                    KPJBA
029400000211     C                   MOVEL     KPJBU         FNLR63DS
029500950313     C*
029600950317     C     KTAB          CHAIN     TABEL00F                           3433
029700950315     C  N33
029800950315     CANN34              EXCEPT    NOAGG
029900950313     C*  Fine Lavoro
030000950314     C     D63F03        IFEQ      '1'
030100950313     C                   MOVEL     'S'           WFINE
030200950313     C                   SETON                                        31
030300950606     C                   ELSE
030400950606     C                   MOVEL     'S'           WRICA             1
030500950313     C                   END
030600950315     C*
030700000211     C                   CLEAR                   FNLR63DS
030800950313     C*
030900950313     C                   ENDSR
031000950313     C*-----------------------------------------------------*
031100950313     C*  Operazioni iniziali
031200950313     C*-----------------------------------------------------*
031300950313     C     *INZSR        BEGSR
031400950313     C*
031500950313     C     *ENTRY        PLIST
031600950313     C                   PARM                    KPJBA
031700950313     C*-----------------------------------------------------*
031800020828     C* Decodifica Elaboratore
031900020828     c     *dtaara       define    §azute        azuteds
032000020828     c     *dtaara       define    §datiute      ddatiute
032100020828     C                   in(E)     *dtaara
032200020828     C                   IF        %Error  or  RSUT = *blanks
032300020828     C                   call      'TIBS34R'
032400020828     C                   parm                    Tibs34Ds
032500020828     C                   in        *dtaara
032600020828     c                   ENDIF
032700950314     C*
032800950313     C     KTAB          KLIST
032900950313     C                   KFLD                    KKUT
033000950313     C                   KFLD                    KCOD
033100950313     C                   KFLD                    KKEY
033200950313     C*----------------------------------------------------*
033300950313     C*  Definizione delle variabili
033400950313     C     *LIKE         DEFINE    TBLKUT        KKUT
033500950313     C     *LIKE         DEFINE    TBLCOD        KCOD
033600950313     C     *LIKE         DEFINE    TBLKEY        KKEY
033700950315     C*----------------------------------------------------*
033800950420     C                   CLEAR                   DS4G
033900000211     C                   CLEAR                   FNLR63DS
034000950314     C                   TIME                    WHHDAT           12 0
034100950313     C                   MOVEL     ' '           WFINE             1
034200950313     C                   MOVEL     '1'           WTPVID            1
034300950313     C                   MOVEL     'P'           D63TRI
034400950317     C                   MOVEL     'N'           WRICH
034500950315     C*-----------------------------------------------------*
034600950315     C* Apertura controllata dei files
034700950315     C                   OPEN      FNCDE01L                             16
034800950315     C   16              GOTO      FINE                                         OPEN vado a fine
034900950313     C*----------------------------------------------------*
035000950313     C*  Caricamento £1
035100120910     c*******            clear                   trul06ds
035200120910     c*******            eval      d06cod = '£1'
035300120910     c*******            movel     dutpou        d06key
035400120910     c*******            movel     trul06ds      kpjbu
035500120910     c*******            call      'TRUL06R'
035600120910     c*******            parm                    kpjba
035700120910     c*******            movel     kpjbu         trul06ds
035800120910     c*******            movea     lin           £1
035900120910      **
036000170509      **  ATTENZIONE: l'UTENTE che deve eseguire la chiusura DISTINTA ESTERO
036100170509      **   deve essere quello che appartiene al P.O.CONTABILE
036200170509      **  questo xché:
036300170509      **   in seguito,la CHIUSURA x OPERATORE vede ARBIPP (P.O.Contabile)
036400170509      **  e ARBIPP viene aggiornato con DUTPOU dell'utente nel FNLR53R.
036500170509      **  quindi:
036600170509      **    >> P.O.CONTABILE è la chiave di tutto il meccanismo <<
036700170509     c     dutPOU        chain     azORG01L
036800170509     c                   if        %Found(azORG01L)
036900170509     c                   if        dutpou <> orgFC0
037000170509      *  messaggio a video "UTENTE non abilitato alla CHIUSURA DISTINTA" e
037100170509      *  chiusura del pgm
037200170509     c                   dow       *inKC = '0'
037300170509     c                   exfmt     LR63D05
037400170509     c                   enddo
037500170509     C                   goto      FINE
037600170509     c                   end
037700170509     c                   end
037800170509      **
037900170509      ** es.linea DPD 196
038000170509      **    da gestire da 049 Torino  - 196 ha P.O.contabile 049
038100170509      **
038200170509      **  ELIMINATO IL CARICAMENTO DELLE LINEE come TERMINAL di ARRIVO
038300170509     c* Carico x terminal  di arrivo  (lo faceva prima)
038400170509     c****               clear                   trul09ds
038500170509     c****               movel     dutPOU        i09tfa
038600170509     c****               movel     'TFA'         i09mod
038700170509     c****               call      'TRUL09R'
038800170509     c****               parm                    trul09ds
038900170509     c********           movea     lin           LnaUser
039000170509      ****
039100170509      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
039200170509     c                   clear                   TRUL31DS
039300170509     c                   clear                   LNAuser
039400170509     c                   eval      I31abi = 'AC'
039500170509     c                   eval      I31cpo = dutPOU
039600170509     c                   call      'TRUL31R'
039700170509     c                   parm                    KPJBA
039800170509     c                   parm                    TRUL31DS
039900170509     c                   if        o31pog > *zeros
040000170509     c                   movea     o31pog        $POg
040100170509     c                   z-add     0             LUsr              3 0
040200170509     c                   do        100           USR               3 0
040300170509     c                   if        $POg(usr) <> *blank
040400170509     c                   add       1             LUsr
040500170509     c                   movel     $POg(usr)     LnaUser(LUsr)
040600170509     c                   end
040700170509     c                   enddo
040800170509     c                   endif
040900170509     C*
041000170509     c*** controlla quante sono caricate
041100020506     C                   Z-ADD     0             §F                3 0
041200020730     c                   Z-ADD     1             XZ                3 0
041300020730      * Deve contare quante sono le filiali gestite per caricare in seguito il sfl
041400120910     C     *zeros        LOOKUP    LnaUser(xz)                            51
041500020730     C   51              if        XZ > 0
041600020730     c     xz            sub       1             §F
041700020730     c                   endif
041800020730      *
041900950313     C*
042000950313     C                   ENDSR
042100170509     C*---------------------------------------------------------------------------
042200950313     OTABEL     E            NOAGG
042300950315**
042400950315Un altro operatore sta chiudendo la stessa linea di arrivo !!!        01
042500950315Linea di arrivo non presente su tabella 4G !!!                        02
042600020829Numeratore distinta di consegna allocato attendi 5'' e ritenta        03
042700170509
