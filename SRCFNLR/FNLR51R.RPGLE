000100911024     H DECEDIT('0,') DATEDIT(*DMY.)
000200030916      * FNLR51R *-----------------------------------------------------*
000300030916      *         - ELENCO BOLLE DA CONSEGNARE  FILTRO
000400030916      *---------------------------------------------------------------*
000500030916      *    A T T E N Z I O N E     A T T E N Z I O N E     ! ! ! !    *
000600030916      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - -    *
000700030916      * Questo pgm. viene richiamato dal pgm. "FNLR11R":              *
000800030916      *   in tal caso vengono "sfruttati" i parametri di default      *
000900030924      *   impostati per la videata.                                   *
001000030916      * PRIMA DI MODIFICARE TALI DEFAULT:                             *
001100030916      *   assincerarsi che tale modifica NON abbia conseguenze        *
001200030916      *   negative per il chiamante (ed i suoi successivi chiamati).  *
001300030916      *---------------------------------------------------------------*
001400941214     FFNLR51D   CF   E             WORKSTN
001500921130     FAZORG01L  IF   E           K DISK
001600970513     FFNFVV01L  IF   E           K DISK
001700030826     D*
001800911104     D ZNP             S              2    DIM(10) DESCEND                      ZONE VIDEO
001900970515     D L1              S              3  0 DIM(30)
002000941214     D*
002100941216     D PARAM           DS                  INZ
002200921130     D  LNA                    1     87  0
002300970516     D                                     DIM(29) DESCEND
002400911024     D  ZNC                   88    107
002500911104     D                                     DIM(10) DESCEND                      ZONE VIDEO BUO
002600941216     D  PARDAT               108    115  0
002700941216     D  VIDABI               116    117
002800941216     D  PARLNP               118    120
002900941216     D  PARNFV               121    125
003000941216     D  PARGES               126    128  0
003100950215     D  PAROUT               129    129
003200950621     D  VIDINC               130    131
003300950621     D  VIDIMA               132    133
003400960321     D  PARDFA               134    141  0
003500970515     D  PARFIA               142    146  0
003600970515     D  PARFGS               147    149  0
003700121211     D**VIDSNP               150    150
003800000803     D  VIDASS               151    152
003900000803     D  VIDRES               153    154
004000021205     D  VIDblo               155    156
004100030826     D  PARfr                252    256
004200970513     D* DS PER TRUL06R - CARICAMENTO £X
004300970513     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004400970513     D  LIN                    1     90  0
004500970515     D                                     DIM(30)
004600970513     D DSLR01        E DS                  INZ
004700911024     D KPJBA         E DS
004800911024     D CNCR80        E DS
004900911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005000020220
005100020220     d og143         e ds
005200020220
005300911024     D                 DS
005400911024     D  ZONCON                 1     20
005500911104     D  VIDZN1                 1      2
005600911104     D  VIDZN2                 3      4
005700911104     D  VIDZN3                 5      6
005800911104     D  VIDZN4                 7      8
005900911104     D  VIDZN5                 9     10
006000911104     D  VIDZN6                11     12
006100911104     D  VIDZN7                13     14
006200911104     D  VIDZN8                15     16
006300911104     D  VIDZN9                17     18
006400911104     D  VIDZ10                19     20
006500911115     D WLBDAT          DS
006600941214     D  G02DAT                 1      8  0
006700941214     D  G02INV                 9     16  0
006800941214     D  G02ERR                17     17
006900941214     D  G02TGI                18     22  0
007000970515     D WGIDAT          DS                  INZ
007100970515     D  GIODAT                 1      8  0
007200970515     D  GIOINV                 9     16  0
007300970515     D  GIOTGI                17     21  0
007400091021
007500091021     d xx              s              2  0
007600121210     d utecar          s                   like(orgcar)
007700030924
007800030924     d                SDS
007900030924     d  VIDpgm           *proc
008000911024     C*---------------------------------------------------------------*
008100911024     C     *ENTRY        PLIST
008200911024     C                   PARM                    KPJBA
008300030826      *
008400030826      * Se pgm. richiamato per la creazione del file di lavoro per
008500030826      *  il controllo delle bolle in buca cambio il "titolo" a
008600030826      *  video
008700030826if  1c                   if        %subst(KPJBU:252:5) = 'WRKF '
008800030826     c                   eval      *in22 = *on
008900030826e   1c                   endif
009000941214     C*
009100911024     C                   Z-ADD     1             CODUT
009200911024     C                   CALL      'X§PARUT'
009300911024     C                   PARM                    UT§DSE
009400911024     C                   MOVEL     REC80         CNCR80
009500941214     C                   MOVEL     RAGUT         RSUT
009600911024     C*
009700030826     C* SE PROFILO DI P.O. DI 2° LIVELLO PRENDO LE FILIALI AD ESSO
009800030826     C*   ASSOCIATE
009900911024     C*
010000020429     c                   z-add     simpou        vidlnp
010100020429     c     simtpp        ifeq      '2'
010200020429     c     simtpp        oreq      *blanks
010300030826     c     *in22         oreq      *on
010400020429     c                   setoff                                       21
010500020429     c                   else
010600020429     c                   seton                                        21
010700121210     c* prendo l'area dell'utente
010800121210     c     simpou        chain     azorg01l
010900121210     c                   if        %found(azorg01l)
011000121210     c                   eval      utecar=orgcar
011100121210     c                   endif
011200121210     c
011300020429     c                   endif
011400970515     C**
011500970515     C* CARICO LA L1
011600970515     C**
011700970515     C                   CLEAR                   DSUL06
011800970515     C                   MOVE      '£1'          D06COD
011900970515     C                   MOVEL     SIMFEL        D06KEY
012000970515     C                   MOVEL     DSUL06        KPJBU
012100970515     C*
012200970515     C                   CALL      'TRUL06R'
012300970515     C                   PARM                    KPJBA
012400970515     C                   MOVEL     KPJBU         DSUL06
012500970515     C                   MOVEA     LIN           L1
012600970515     C*
012700970515     C                   DO        30            B                 3 0
012800971201     C     L1(B)         CHAIN     AZORG                              31
012900971201     C     *IN31         IFEQ      *OFF
013000020220     c                   eval      og143 = orgde3
013100020220     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP' or
013200020220     c                             §ogntw = 'FED' or §ogntw = 'DPD'
013300970515     C                   SETON                                        01
013400020220     c                   endif
013500970515     C                   ENDIF
013600970515     C                   ENDDO
013700970515     C**
013800911024     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
013900970515     C**
014000921130     C                   EXSR      CARL6
014100970515     C*
014200911024     C*---------------------------------------------------------------*
014300911024     C*
014400911024     C     INIZI1        TAG
014500911024     C*
014600960517     C                   CLEAR                   VIDABI
014700000803     C                   CLEAR                   VIDASS
014800000803     C                   CLEAR                   VIDRES
014900021205     C                   movel     'NO'          VIDblo
015000950621     C                   MOVE      *BLANKS       VIDINC
015100030924if  1c                   if        NOT *in22
015200960517     C                   MOVEL     'SI'          VIDIMA
015300030924x   1c                   else
015400030924     c                   clear                   VIDima
015500030924e   1c                   endif
015600911025     C                   SETON                                        20
015700911024     C                   Z-ADD     *ZEROS        VIDLNA
015800911104     C                   MOVE      *BLANKS       ZONCON
015900941214     C                   TIME                    WTIME            14 0          ORA E DATA
016000941214     C                   MOVE      WTIME         V01DTA
016100950621     C                   MOVEL     *ZEROS        PARNFV
016200960321     C                   MOVE      WTIME         VIDDFA
016300970513     C                   CLEAR                   VIDNFV
016400970513     C                   CLEAR                   VIDFGS
016500030826if  1c                   if        NOT *in22
016600030826     c                   clear                   PARfr
016700030826x   1c                   else
016800030826     c                   eval      PARfr = 'WRKF '
016900030826     c                   exsr      DecodLNP
017000030826e   1c                   endif
017100911024     C     INIZIO        TAG
017200030826if  1c                   if        NOT *in22
017300941214     C                   EXFMT     LR51D01
017400030826x   1c                   else
017500030826     c                   exfmt     LR51D02
017600030826e   1c                   endif
017700921130     C**
017800921130     C** CMD 3 - FINE LAVORO
017900030826     c   kc              eval      %subst(KPJBU:252:5) = '*F3  '
018000911024     C   KC              GOTO      FINE
018100911025     C*
018200921130     C** CMD18 - CAMBIO FILIALE
018300911025     C   21
018400911025     CAN KS              SETOFF                                       20
018500921130     C   21
018600921130     CAN KS              GOTO      INIZIO
018700921130     C*
018800921130     C* CONTROLLO FILIALE DI CONSEGNA
018900121210     c                   if        *in21 and not *in20
019000121210     c                   clear                   filcar            1
019100121210     C
019200121210     C     VIDLNP        IFNE      300
019300971201     C     VIDLNP        LOOKUP    L1                                     31
019400121210     c                   if        not *in31
019500121210     c     vidlnp        chain     azorg01l
019600150526     c* se non c'e' in £1 verifico se richiesti i soli incasso se la filiale contabile
019700150526     c*  corrisponde alla filiale utente
019800150526     c                   if        vidinc='SI' and %found(azorg01l) and
019900150526     c                             orgfc0=simpou
020000150526     c                   eval      filcar='S'
020100150526     c                   seton                                        31
020200150526     c                   endif
020300150526     c
020400150526     c* se non c'e' in £1 verifico se fa parte dell'area della filiale utente
020500150526     c  n31              if        %found(azorg01l) and orgcar = utecar
020600121210     c                   seton                                        31
020700121210     c                   eval      filcar='S'
020800121210     c                   endif
020900121211     c                   endif
021000121210     c
021100921130     C* NON UNA MIA FILIALE
021200971201     C  N31              SETON                                        40
021300971201     C  N31              GOTO      INIZIO
021400971201     C                   END
021500921130     C*
021600970513     C** RICARICO LA L6
021700121210     C
021800121210     C                   EXSR      CARL6
021900121210     c                   endif
022000911025     C*
022100911024     C* CONTROLLO
022200921130     C                   EXSR      CONTR
022300941214     C  NKF
022400911024     COR 90              GOTO      INIZIO
022500921130     C*
022600941214     C* F6 - ELABORAZIONE
022700921130     C*
022800921130     C* SISTEMO LE ZONE IN ORDINE
022900921130     C                   MOVEL     *BLANKS       ZNC
023000921130     C                   MOVEA     ZONCON        ZNP
023100921130     C                   Z-ADD     1             G                 2 0
023200921130     C                   Z-ADD     1             H                 2 0
023300921130     C* MUOVO LE ZONE DEL A VIDEO IN MODO CONTINUO NELLA TABELLA
023400921130     C*
023500921130     C     ZONCON        IFNE      *BLANKS
023600921130     C     G             DOWLE     10
023700921130     C     ZNP(G)        IFNE      *BLANKS
023800921130     C                   MOVE      ZNP(G)        ZNC(H)
023900921130     C                   ADD       1             H
024000921130     C                   END
024100921130     C*
024200921130     C                   ADD       1             G
024300921130     C                   END
024400921130     C*
024500921130     C                   ELSE
024600921130     C                   MOVEL     ZNP           ZNC
024700921130     C                   END
024800921130     C*
024900921130     C                   SORTA     ZNC
025000921130     C                   MOVEA     ZNC           ZONCON
025100921130     C*
025200921130     C* LINEE DI ARRIVO
025300921130     C     VIDLNA        IFNE      0
025400921130     C                   MOVEA     *ZEROS        LNA
025500921130     C                   MOVEL     VIDLNA        LNA(1)
025600921130     C                   ELSE
025700921130     C                   MOVEA     LIN           LNA
025800921130     C                   END
025900970513     C* FILIALE GESTIONE --> MI SERVE PER LE SPUNTE IMA
026000950621     C                   MOVEL     VIDLNP        PARLNP
026100970516     C* NUMERO FOGLIO IMA SE IMMESSO
026200970516     C                   MOVEL     VIDNFV        PARFIA
026300970516     C                   MOVEL     DLRFGS        PARFGS
026400921130     C*
026500921130     C                   SORTA     LNA
026600921130     C* BOLLE ABILITATE SI/NO
026700920218     C     VIDABI        IFEQ      *BLANKS
026800941216     C                   Z-ADD     99991231      PARDAT
026900920218     C                   END
027000921130     C*
027100950215     C                   CLEAR                   PAROUT                         OUTQ DEL TERMINALE
027200911024     C                   MOVEL     PARAM         KPJBU
027300030826if  1c                   if        NOT *in22
027400941214     C                   MOVEL     'LR52'        KCOAZ
027500911024     C                   CALL      'BCH10'
027600911024     C                   PARM                    KPJBA
027700030826x   1c                   else
027800030826     c                   call      'FNLR52C'
027900030826     c                   parm                    KPJBA
028000030826e   1c                   endif
028100911024     C*
028200911024     C     FINE          TAG
028300911024     C                   SETON                                        LR
028400030826      *
028500030826      *---------------------------------------------------------------*
028600030826      * DecodLNP   > Decodifica P.O. di partenza                      *
028700030826      *---------------------------------------------------------------*
028800030826     c     DecodLNP      BegSr
028900030826      *
029000030826     c     VIDlnp        chain     AZORG
029100030826if  2c                   if        NOT %found(AZORG01L)
029200030826     c                             or  ORGfva = '*'
029300030826     c                   movel     *all'? '      ORGdes
029400030826e   2c                   endif
029500030826      *
029600030826     c                   movel     ORGdes        V2Dlnp
029700030826      *
029800030826     c                   EndSr
029900030826      *
030000941214     C**************************************************************************
030100941214     C*    CONTROLLI FORMATO1
030200941214     C**************************************************************************
030300911024     C     CONTR         BEGSR
030400941214     C*
030500911024     C                   SETOFF                                       90
030600921130     C* SOLO SE >< 0
030700911024     C     VIDLNA        IFNE      0
030800911024     C     VIDLNA        LOOKUP    LIN                                    70
030900921130     C  N70              SETON                                        9023       ERRORE
031000921130     C  N70              GOTO      ENDCTR
031100911024     C                   END
031200921130     C*
031300960321     C* CONTROLLO DATA CONSEGNA RICHISTA
031400921130     C                   MOVE      V01DTA        G02DAT
031500921130     C                   MOVEL     *BLANK        G02ERR
031600941214     C                   CALL      'XSRDA8'
031700921130     C                   PARM                    WLBDAT
031800921130     C     G02ERR        COMP      '1'                                    10
031900921130     C   10              SETON                                        90
032000921130     C   10              GOTO      ENDCTR
032100921130     C*
032200941214     C                   Z-ADD     G02DAT        V01DTA
032300941216     C                   Z-ADD     G02INV        PARDAT
032400960321     C* CONTROLLO DATA FOGLIO ARRIVI
032500960321     C                   MOVE      VIDDFA        G02DAT
032600960321     C                   MOVEL     *BLANK        G02ERR
032700960321     C                   CALL      'XSRDA8'
032800960321     C                   PARM                    WLBDAT
032900960321     C     G02ERR        COMP      '1'                                    41
033000960321     C   41              SETON                                        90
033100960321     C   41              GOTO      ENDCTR
033200960321     C*
033300960321     C                   Z-ADD     G02DAT        VIDDFA
033400960321     C                   Z-ADD     G02INV        PARDFA
033500960321     C*
033600950621     C* SE RICHEISTE LE SPEDIZIONI SOLO INCASSO NON POSSO
033700000824     C*  CHIDERE PARZIALIZZAZIONI PER IMA, ASSEGNATO, RESO
033800950621     C     VIDINC        IFEQ      'SI'
033900000824     C     VIDIMA        IFNE      '  '
034000000824     C     VIDASS        ORNE      '  '
034100000824     C     VIDRES        ORNE      '  '
034200950621     C                   SETON                                        5090
034300950621     C                   GOTO      ENDCTR
034400950621     C                   ENDIF
034500000824     C                   ENDIF
034600970515     C**
034700970515     C* IMPOSTO LA FILIALE GESTIONE
034800970516     C                   CLEAR                   DSLR01
034900970515    1C     VIDFGS        IFGT      0
035000970515    2C     VIDFGS        IFEQ      300
035100970515     C                   Z-ADD     SIMFEL        DLRFGS
035200970515   X2C                   ELSE
035300970515     C                   Z-ADD     VIDFGS        DLRFGS                         FIL.GESTIONE
035400970515    2C                   ENDIF
035500970515     C**
035600970515   X1C                   ELSE
035700970515     C**
035800970515     C                   Z-ADD     VIDLNP        DLRFGS                         FIL.GESTIONE
035900970515    1C                   ENDIF
036000970515     C**
036100970513     C* CONTROLLO FOGLIO IMA
036200970513     C     '?'           SCAN      VIDNFV                                 90
036300970513    1C     *IN90         IFEQ      *ON
036400970513     C                   MOVEL     'S'           DLRRIC                         PGM.RICHIAMATO
036500970513     C                   MOVEL     IMANPG        DLRNPG                         CATEGORIA
036600970513     C                   Z-ADD     WMENO1        DLRGDA                         MENO 2 GIORNI G/M/A
036700970513     C                   Z-ADD     WMENO2        DLRADA                         MENO 2 GIORNI A/M/G
036800970513     C                   MOVEL     'FNLR52R'     DLRPGM                         PROGRAMMA
036900970513     C                   MOVEL     DSLR01        KPJBU
037000970513     C                   CALL      'FNLR02R'
037100970513     C                   PARM                    KPJBA
037200970513     C                   MOVEL     KPJBU         DSLR01
037300970513     C                   MOVEL     DLRNFV        VIDNFV
037400970513     C                   MOVEL     DLRFGS        VIDFGS
037500970513     C                   GOTO      ENDCTR
037600970515    1C                   ENDIF
037700970513     C*
037800970515    1C     VIDNFV        IFNE      *ZEROS
037900970515     C     VIDNFV        ANDNE     *BLANKS
038000970515     C*
038100970513     C                   TESTN                   VIDNFV               31
038200970515     C                   MOVE      VIDNFV        W001A             1
038300970515     C     *IN31         IFEQ      *OFF
038400970515     C     W001A         ORLT      '0'
038500970515     C                   SETON                                        5190
038600970515     C                   GOTO      ENDCTR
038700970515     C                   ENDIF
038800970515     C                   MOVEL     VIDNFV        KNFV
038900970515     C     KFVV          CHAIN     FNFVV01L                           31
039000970516     C     *IN31         IFEQ      *ON
039100970515     C     FVVATB        ORNE      ' '
039200970515     C                   SETON                                        5190
039300970515     C                   GOTO      ENDCTR
039400970515     C                   ENDIF
039500970515     C     FVVSPG        IFNE      'A'
039600970515     C                   SETON                                        5290
039700970515     C                   GOTO      ENDCTR
039800970515     C                   ENDIF
039900970516     C                   MOVEL     FVVFGS        VIDFGS
040000970515    1C                   ENDIF
040100091021     c* Zone di consegna: se immessi caratteri alfanumerici --> errore
040200091021     C                   MOVEA     zoncon        znp
040300091021     c                   do        10            xx
040400091021     c                   if        znp(xx)<>*blanks
040500091021     c                   testn                   znp(xx)              3132
040600091021     c                   if        (*in31=*off and *in32=*off)
040700091021     c                             or %subst(znp(xx):2:1)<'0'
040800091021     c                   seton                                        4290
040900091021     C                   GOTO      ENDCTR
041000091021     c                   endif
041100091021     c                   endif
041200091021     c                   enddo
041300121211     c
041400150526     c* Se richiesta filiale dell'area obbligatorio chiedere le sped solo incasso
041500121211     c                   if        filcar='S' and vidinc<>'SI'
041600121211     c                   seton                                        4390
041700121211     C                   GOTO      ENDCTR
041800121211     c                   endif
041900911024     C*
042000911024     C     ENDCTR        ENDSR
042100941214     C**************************************************************************
042200941214     C*    CARICO L6
042300941214     C**************************************************************************
042400921130     C     CARL6         BEGSR
042500921130     C*
042600970513     C* SE E' LINEA 300 --> CARICO TUTTE LE ESTERE DELLA L1
042700970513     C     VIDLNP        IFEQ      300
042800970513     C* PRENDO SOLO LE ESTERE
042900970513     C                   Z-ADD     1             A                 3 0
043000970513     C                   Z-ADD     1             B                 3 0
043100970513     C                   CLEAR                   LIN
043200970513     C     L1(A)         DOWGT     0
043300971201     C     L1(A)         CHAIN     AZORG                              31
043400971201     C     *IN31         IFEQ      *OFF
043500020220     c                   eval      og143 = orgde3
043600020220     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP' or
043700020220     c                             §ogntw = 'FED' or §ogntw = 'DPD'
043800970513     C                   Z-ADD     L1(A)         LIN(B)
043900970513     C                   ADD       1             B
044000020220     c                   endif
044100970513     C                   ENDIF
044200970513     C                   ADD       1             A
044300970513     C                   ENDDO
044400970513     C*
044500970513     C                   ELSE
044600970513     C                   CLEAR                   DSUL06
044700970513     C                   MOVE      '£6'          D06COD
044800970513     C                   MOVEL     VIDLNP        D06KEY
044900970513     C                   MOVEL     DSUL06        KPJBU
045000970513     C*
045100970513     C                   CALL      'TRUL06R'
045200970513     C                   PARM                    KPJBA
045300970513     C                   MOVEL     KPJBU         DSUL06
045400921130     C                   END
045500921130     C*
045600921130     C                   ENDSR
045700970513     C**************************************************************************
045800970513     C*    SUBROUTINE DI INIZIO
045900970513     C**************************************************************************
046000970513     C     *INZSR        BEGSR
046100970513     C                   TIME                    WTIME            14 0          ORA E DATA
046200970513     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
046300970513     C                   Z-ADD     WDATE         G02DAT
046400970513     C                   Z-ADD     *ZERO         G02INV
046500970513     C                   MOVEL     *BLANK        G02ERR
046600970513     C                   CALL      'XSRDA8'
046700970513     C                   PARM                    WLBDAT
046800970513     C                   Z-ADD     G02INV        WDATEU            8 0
046900970513     C* DATA INIZIALE: UDATE -2
047000970513     C     G02TGI        SUB       2             GIOTGI
047100970513     C*
047200970513     C                   CALL      'XSRGI8'
047300970513     C                   PARM                    WGIDAT
047400970513     C                   Z-ADD     GIODAT        WMENO1            8 0          MENO 5 GIORNI G/M/A
047500970513     C                   Z-ADD     GIOINV        WMENO2            8 0          MENO 5 GIORNI A/M/G
047600970513     C**
047700970513     C     KFVV          KLIST
047800970513     C                   KFLD                    IMANPG
047900970513     C                   KFLD                    KNFV
048000970516     C                   KFLD                    DLRFGS
048100970513     C**
048200970513     C     *LIKE         DEFINE    FVVNPG        IMANPG
048300970513     C     *LIKE         DEFINE    FVVNFV        KNFV
048400970513     C                   Z-ADD     3             IMANPG
048500970513     C                   ENDSR
