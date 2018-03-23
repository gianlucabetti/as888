000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020419      * FNLR28R *----------------------------------------------------*
000300020419      *         - FILTRO LANCIO IDA
000400020419      *--------------------------------------------------------------*
000500941222     FFNLR28D   CF   E             WORKSTN
000600941222     FFNFVV01L  IF   E           K DISK
000700930121     FTABEL00F  IF   E           K DISK
000800941222     D*
000900911015     D SKI             S              1    DIM(5)                               SKI DI UNO
001000930121     D L1              S              3  0 DIM(30)                              FIL GESTITE
001100930121     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
001200930121     D L6S             S              3  0 DIM(30)                              FIL GEST SIMFE
001300020419
001400020419     d wa              s              2  0
001500020419     d wb              s              2  0
001600020419     d wc              s              2  0
001700020419
001800941222     D*
001900941222     D DSLR01        E DS
002000930205     D DS7N          E DS
002100920831     D KPJBA         E DS
002200101202      *
002300101202      * - Parametri per richiamo al pgm di controllo profilo utenti
002400101202     d TIBS34ds      e ds
002500101202      * - Ds di riferimento al file esterno AzUte00F
002600101202     d AZUTEds       e ds                  ExtName(AzUte00F)
002700101202      * - Ds per dati organigramma
002800101202     d dDatiUte      e ds
002900101202      *
003000070706     D WLBDAT          DS
003100070706     D  G02DAT                 1      8  0
003200070706     D  G02INV                 9     16  0
003300070706     D  G02ERR                17     17
003400070706     D  G02TGI                18     22  0
003500020419      * ds per trul06r
003600020419     d trul06ds      e ds
003700020419     d  lin                    1     90  0 dim(30)
003800020419
003900900702     C****************************************************************
004000900702     C*  RIEPILOGO INDICATORI
004100900702     C***************************************************************
004200920715     C* 40/42 - ERRORE
004300900702     C* 31/32 - DI COMODO
004400900702     C*****************************************************************
004500000000     C     *ENTRY        PLIST
004600000000     C                   PARM                    KPJBA
004700941222     C*
004800900511     C*---------------------------------------------------------------*
004900910320     C     KFVV          KLIST
005000911015     C                   KFLD                    COMNPG            1 0
005100911015     C                   KFLD                    COMNFV            5 0
005200941222     C                   KFLD                    VIDFGS
005300930121     C     KTAB2         KLIST
005400930121     C                   KFLD                    CODUT
005500930121     C                   KFLD                    COD               2
005600930121     C                   KFLD                    KEY               8
005700930121     C*
005800930121     C                   EXSR      OPERAZ
005900911015     C*
006000911015     C                   MOVEL     *BLANKS       VIDNFV
006100911015     C                   MOVEL     'N'           VIDSN
006200920930     C                   MOVEL     'S'           VIDSSP
006300101202     C*
006400040227     C                   MOVEL     '2'           VIDNPG
006500040227     C*
006600900509     C     FOR01         TAG
006700941222     C                   EXFMT     LR28D01
006800910320     C** CMD3 - FINE LAVORO
006900900509     C   KC              GOTO      FINE
007000930121     C** CMD18- CAMBIO FILIALE
007100930121     C   KS              SETON                                        04
007200930121     C   KS              GOTO      FOR01
007300930121     C*
007400941222     C   04VIDFGS        IFNE      SIMFEL
007500941222     C     VIDFGS        LOOKUP    L1                                     30
007600930121     C  N30              SETON                                        50
007700930121     C  N30              GOTO      FOR01
007800930121     C*
007900930121     C* NON POSSO CAMBIARE SE E' IN MIA L6
008000941222     C     VIDFGS        LOOKUP    L6S                                    49
008100930121     C   49              GOTO      FOR01
008200930121     C                   END
008300930121     C*
008400930205     C   04              EXSR      CARL6
008500910801     C*  CONTROLLI
008600910801     C                   EXSR      CONTR
008700941222     C  NKF
008800910801     COR 90              GOTO      FOR01
008900941222     C* F6 - ELABORAZIONE
009000941228     C                   CLEAR                   KPJBU
009100941228     C                   CLEAR                   DSLR01
009200941228     C                   Z-ADD     VIDFGS        DLRFGS
009300941228     C                   MOVEL     VIDNPG        DLRNPG
009400941228     C                   MOVEL     VIDNFV        DLRNFV
009500941228     C                   MOVEL     VIDSN         DLRFS1
009600941228     C                   MOVEL     DSLR01        KPJBU
009700950817     C                   MOVEL     'LR32'        KCOAZ
009800900702     C                   CALL      'BCH10'
009900900702     C                   PARM                    KPJBA
010000920930     C*
010100920930     C* SE SELEZIONATO STAMPO SPUNTA ARRIVI
010200920930     C     VIDSSP        IFEQ      'S'
010300941227     C                   CLEAR                   DSLR01
010400941227     C                   Z-ADD     VIDFGS        DLRFGS
010500941227     C                   MOVEL     VIDNPG        DLRNPG
010600941227     C                   MOVEL     VIDNFV        DLRNFV
010700941228     C                   MOVEL     'S'           DLRFS1                         'S'=STAMPA SPUNTE
010800941228     C                   Z-ADD     *HIVAL        DLRNPA                         NR.PISTOLA FINALE
010900941228     C                   Z-ADD     *HIVAL        DLRSRA                         SERIE FINALE
011000941227     C                   MOVEL     DSLR01        KPJBU
011100941222     C                   MOVEL     'LR06'        KCOAZ
011200920930     C                   CALL      'BCH10'
011300920930     C                   PARM                    KPJBA
011400920930     C                   END
011500910320     C*
011600000000     C     FINE          TAG
011700000000     C                   SETON                                        LR
011800941222     C**************************************************************************
011900941222     C* OPERAZIONI INIZIALI
012000941222     C**************************************************************************
012100930121     C     OPERAZ        BEGSR
012200101202      *
012300101202      * impostazione campi fissi
012400101202     C                   Z-ADD     1             CODUT             1 0
012500101202      *
012600101202      * Reperimento dati utente/aziendali
012700101202     c                   exsr      sr_DatiJob
012800930121     C*
012900070706     C* VEDO SE SONO 1° o 2° livello
013000930121     C*
013100101202     c     dutLPO        ifeq      '2'
013200101202     c     dutLPO        oreq      *blanks
013300101202     c                   movel     dutPOU        vidfgs
013400930121     C                   SETON                                        05
013500930205     C                   ELSE
013600941222     C                   MOVEL     SIMFEL        VIDFGS
013700930121     C                   END
013800930205     C*
013900930205     C                   EXSR      CARL6
014000930121     C*
014100020419     C* CARICO FILIALI GESTITE £1
014200930121     C*
014300020419     c                   clear                   trul06ds
014400020419     c                   eval      d06cod = '£1'
014500020419     c                   movel     simfel        d06key
014600020419     c                   movel     trul06ds      kpjbu
014700020419     c                   call      'TRUL06R'
014800020419     c                   parm                    kpjba
014900020419     c                   movel     kpjbu         trul06ds
015000020419     c                   movea     lin           l1
015100020419
015200020419     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE
015300020419     c                   clear                   wc
015400020419 b1  c                   do        30            wa
015500020422     c                   if        l1(wa) = *zeros
015600020422     c                   leave
015700020422     c                   endif
015800020419     c                   clear                   kpjbu
015900020419     c                   clear                   trul06ds
016000020419     c                   eval      d06cod = '£6'
016100020419     c                   movel     l1(wa)        d06key
016200020419     c                   eval      d06esc = 'S'
016300020419     c                   movel     trul06ds      kpjbu
016400020419     c                   call      'TRUL06R'
016500020419     c                   parm                    kpjba
016600020419     c                   movel     kpjbu         trul06ds
016700020419 b2  c                   do        30            wb
016800020419     c                   if        lin(wb) = *zeros
016900020419     c                   leave
017000020422     c                   endif
017100020419     c                   add       1             wc
017200020419     c                   z-add     lin(wb)       l6s(wc)
017300020419 e2  c                   enddo
017400020419 e1  c                   enddo
017500070706     c* data del giorno
017600070706     C                   TIME                    W0140            14 0
017700070706     C* UDATE IN GGMMAAAA
017800070706     C                   MOVE      W0140         WDTGIO            8 0
017900070706     C*
018000070706     C* UDATE IN AAAAMMGG
018100070706     C                   Z-ADD     WDTGIO        G02DAT
018200070706     C                   MOVEL     *BLANK        G02ERR
018300070706     C                   CALL      'XSRDA8'
018400070706     C                   PARM                    WLBDAT
018500070706     C                   MOVEL     G02INV        DATEU             8 0
018600070706     C*
018700020419
018800941222     C*
018900930121     C                   ENDSR
019000941222     C**************************************************************************
019100941222     C* CARICO L6
019200941222     C**************************************************************************
019300930121     C     CARL6         BEGSR
019400941222     C*
019500020419     c                   clear                   trul06ds
019600020419     c                   eval      d06cod = '£6'
019700020419     c                   movel     vidfgs        d06key
019800020419     c                   movel     trul06ds      kpjbu
019900020419     c                   call      'TRUL06R'
020000020419     c                   parm                    kpjba
020100020419     c                   movel     kpjbu         trul06ds
020200020419     c                   movea     lin           l6
020300941222     C*
020400930121     C                   ENDSR
020500941222     C**************************************************************************
020600941222     C* CONTROLLO CAMPI
020700941222     C**************************************************************************
020800910320     C     CONTR         BEGSR
020900941222     C*
021000920624     C                   SETOFF                                       9030
021100930205     C* CATEGORIA FOGLI
021200930205     C     VIDNPG        IFEQ      ' '
021300930205     C                   SETON                                        5190
021400930205     C                   GOTO      ENDCT1
021500930205     C                   END
021600930205     C     VIDNPG        IFEQ      '?'
021700930205     C                   MOVEL     CODUT         §KUT
021800930205     C                   MOVEL     '7N'          §COD
021900930205     C                   MOVE      ' '           VIDNPG
022000930205     C                   MOVEL     *BLANKS       §KEY
022100930205     C                   CALL      'X§TABER'
022200930205     C                   PARM                    §KUT              1 0
022300930205     C                   PARM                    §COD              2
022400930205     C                   PARM                    §KEY              8
022500930205     C                   PARM                    §DES             30
022600930205     C                   MOVEL     §KEY          VIDNPG
022700930205     C                   SETON                                        90
022800930205     C                   GOTO      ENDCT1
022900930205     C                   END
023000930205     C* CONTROLLO
023100930205     C                   MOVEL     '7N'          COD
023200930205     C                   MOVEL     *BLANKS       KEY
023300930205     C                   MOVEL     VIDNPG        KEY
023400930205     C* INESISTENTE
023500930205     C     KTAB2         CHAIN     TABEL                              51
023600930205     C* ANNULLATA
023700930205     C  N51TBLFLG        IFNE      ' '
023800930205     C                   SETON                                        51
023900930205     C                   ELSE
024000930205     C     VIDNPG        IFNE      '2'
024100930205     C                   SETON                                        51
024200930205     C                   END
024300930205     C                   END
024400930205     C* ERRATA
024500930205     C   51              SETON                                        90
024600930205     C   51              GOTO      ENDCT1
024700930205     C*
024800930205     C                   MOVEL     TBLUNI        DS7N
024900930205     C* NON UTILIZZABILE DALLA FILIALE IN GESTIONE
025000930205     C     §7NUTI        IFEQ      '9'
025100930205     C                   SETON                                        5290
025200930205     C                   GOTO      ENDCT1
025300930205     C*
025400930205     C                   ELSE
025500930205     C*
025600930205     C* SOLO DALLA PRIMO LIVELLO UTILIZZABILE
025700930205     C     §7NUTI        IFEQ      '1'
025800941222     C     VIDFGS        ANDNE     SIMFEL
025900930205     C                   SETON                                        5290
026000930205     C                   GOTO      ENDCT1
026100930205     C                   END
026200930205     C     §7NUTI        IFEQ      '2'
026300941222     C     VIDFGS        ANDEQ     SIMFEL
026400930205     C                   SETON                                        5290
026500930205     C                   GOTO      ENDCT1
026600930205     C                   END
026700930205     C                   END
026800930205     C                   MOVEL     VIDNPG        COMNPG            1 0
026900930205     C*
027000911015     C* NUMERO FOGLIO VIAGGIO
027100911015     C     VIDNFV        IFEQ      *BLANKS
027200911015     C     VIDNFV        OREQ      *ZEROS
027300911015     C                   SETON                                        4090
027400911015     C                   GOTO      ENDCT1
027500911015     C*
027600911015     C                   ELSE
027700911015     C*
027800911015     C                   MOVEA     VIDNFV        SKI
027900911015     C* RICERCA
028000911015     C     '?'           LOOKUP    SKI                                    09
028100911015     C   09              DO
028200941222     C                   CLEAR                   DSLR01
028300941222     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
028400941222     C                   MOVEL     VIDNPG        DLRNPG                         CATEGORIA FOGLIO
028500941222     C                   MOVEL     '2'           DLRTFV                         TIPO FOGLIO
028600941222     C                   MOVEL     'FNLR28R'     DLRPGM                         NOME PROGRAMMA
028700941222     C                   Z-ADD     31129999      DLRGAL                         AL GG/MM/AAAA
028800941222     C                   Z-ADD     99991231      DLRAAL                         AL AAAA/MM/GG
028900941222     C                   MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
029000941222     C                   MOVEL     DSLR01        KPJBU
029100941222     C                   CALL      'FNLR02R'
029200911015     C                   PARM                    KPJBA
029300941222     C                   MOVEL     KPJBU         DSLR01
029400941222     C                   MOVEL     DLRNFV        VIDNFV
029500911015     C                   SETON                                        90
029600911015     C                   GOTO      ENDCT1
029700911015     C                   END
029800911015     C* CONTROLLI
029900911015     C                   TESTN                   VIDNFV               31
030000911015     C  N31              SETON                                        30
030100911015     C*
030200911015     C  N30              MOVEL     VIDNFV        COMNFV
030300941222     C  N30KFVV          CHAIN     FNFVV01L                           30
030400911015     C* ANNULLATO
030500911015     C  N30FVVATB        IFNE      *BLANKS
030600911015     C                   SETON                                        30
030700911015     C                   END
030800911015     C   30              SETON                                        4090
030900911015     C   30              GOTO      ENDCT1
031000930121     C*
031100930121     C* INGESTIONE AD ALTRA FILIALE
031200941222     C     FVVFGS        LOOKUP    L6                                     30
031300930121     C  N30              SETON                                        4890
031400930121     C  N30              GOTO      ENDCT1
031500911015     C* CHIUSO
031600911015     C  N30FVVFCF        IFNE      *BLANKS
031700911015     C                   SETON                                        4290
031800911015     C                   END
031900070706     c*
032000070706     c* non posso chiudere un foglio in data di domani
032100070706     c                   if        vidsn='S' and fvvdfv>dateu
032200070706     C                   SETON                                        5390
032300070706     c                   endif
032400911015     C*
032500920713     C                   END
032600910320     C*
032700911015     C     ENDCT1        ENDSR
032800101202
032900101202      *---------------------------------------------------------------*
033000101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
033100101202      *---------------------------------------------------------------*
033200101202     c     sr_DatiJob    BEGSR
033300101202      *
033400101202     c     *dtaara       define    §azute        AZUTEds
033500101202     c     *dtaara       define    §datiute      dDatiUte
033600101202      *
033700101202     c                   in(E)     *dtaara
033800101202     c                   IF        %Error or RSUT = *blanks
033900101202     c                   clear                   TIBS34ds
034000101202     c                   call      'TIBS34R'
034100101202     c                   parm                    TIBS34ds
034200101202     c                   in        *dtaara
034300101202     c                   ENDIF
034400101202      *
034500101202     c                   ENDSR
