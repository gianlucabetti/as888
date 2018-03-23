000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200941025     H* FNLV19R  *---------------------------------------------------*
000300061115     H*     SCRITTURA FIAR4 / tita4
000400921021     H*--------------------------------------------------------------*
000500941024     FTABEL00F  IF   E           K DISK
000600061115     FFiAR401L  UF A E           K DISK    usropn
000700061115     Ftitas30c  iF   E           K DISK    usropn
000800061115     Ftita430c  UF A E           K DISK    usropn  prefix(ar4:3)
000900941025     D K35             S              1    DIM(35)
001000941025     D K35B            S              1    DIM(35)
001100061115     d cmdt            s             60    dim(03) ctdata perrcd(1)
001200940218      *
001300060214     D* DS PER FNLV19R - DS DEI CAMPI PER SCITTURA FiAR4
001400941025     D DSLV19        E DS                  EXTNAME(FNLV19DS)
001500061115     D tibs55ds      E DS
001600941025     D DS7K          E DS
001700060215     d Dataiso         s               d   datfmt(*iso)
001800061115     D
001900061115     d comman          s             80
002000061115     d lenght          s             15  5 inz(80)
002100061115     d AggFil          s              1
002200061115     d AggSede         s              1
002300061115     d
002400941021     I*
002500061115     I*
002600061115     ITITAS000      01
002700061115     ITITAS010      02
002800061115     ITITASP00      03
002900061115     I*
003000061115     ITITA4000      04
003100061115     ITITA4P00      05
003200921021     C*---------------------------------------------------------------*
003300921021     C     *ENTRY        PLIST
003400941025     C                   PARM                    DSLV19
003500000000     C*---------------------------------------------------------------*
003600941025     C* TIPO LANCIO, D19TLA "C" -           CHIUSO CON LR
003700941025     C* TIPO LANCIO, D19TLA "L" - ELABORO E CHIUDO CON LR
003800941025     C* TIPO LANCIO, D18TLA " " - ELABORO E CHIUDO IN RETRN
003900941025    1C     D19TLA        IFNE      'C'
004000941025     C                   CLEAR                   D19ERR
004100061115     c* Apertura file se non aperti
004200061115     c                   exsr      APRI
004300061115     c*
004400061115     c                   if        *in99
004500061115     c                   movel     '1'           d19err
004600061115     c                   goto      fine
004700061115     c                   endif
004800941025     C**
004900941025     C* DESCRIZIONI NON IMMESSE
005000941025     C**
005100941025    2C     D19NT1        IFEQ      *BLANKS
005200941025    2C     D19NT2        ANDEQ     *BLANKS
005300941025     C*
005400941025     C* SE C'ERA DELETO RECORD
005500061115     c* Filiale
005600061115    3c                   if        d19agg=' ' or d19agg='E'
005700060215     C     KBOL          DELETE    Fiar4000                           3033
005800941025     C*
005900941025     C* ERRORE: RECORD NON DISPONIBILE
006000941025     C     *IN33         IFEQ      *ON
006100941025     C                   MOVEL     '2'           D19ERR
006200941025     C                   ENDIF
006300061115    3C                   ENDIF
006400061115     c* Sede
006500061115    3c                   if        d19agg='S' or d19agg='E'
006600061115     c                   setoff                                       0405
006700061115     C     KBOL          chain     TITA430c                           3033
006800061115     c                   if        not *in30 and not *in33
006900061115     C   04              DELETE    TITA4000
007000061115     C   05              DELETE    TITA4p00
007100061115     c                   endif
007200061115     C*
007300061115     C* ERRORE: RECORD NON DISPONIBILE
007400061115     C     *IN33         IFEQ      *ON
007500061115     C                   MOVEL     '2'           D19ERR
007600061115     C                   ENDIF
007700061115    3C                   ENDIF
007800941025     C*
007900941025   X2C                   ELSE
008000941025     C**
008100941025     C* DESCRIZIONE     IMMESSA
008200941025     C**
008300941025     C* CONTROLLO
008400941025     C                   EXSR      CTRDES
008500941025     C*
008600941025     C* NON C'E' ERRORE MEMORIZZO LE NOTE
008700941025    3C     D19ERR        IFEQ      *BLANKS
008800941025     C*
008900941025     C* CHAIN SU TABEL CON TIPO RECORD
009000941025     C                   MOVEL     '7K'          COD
009100941025     C                   MOVEL(P)  D19TRC        KEY
009200941025     C     KTAB          CHAIN     TABEL                              30
009300941025     C  N30              MOVEL     TBLUNI        DS7K
009400941025     C   30              CLEAR                   DS7K
009500061115     c
009600061115     c                   clear                   aggsede
009700061115     c                   clear                   aggfil
009800061115     c* Per SEDE vedo se c'e' flag in tabella 7K
009900061115     c                   if        d19agg='S' or d19agg='E' and §7ksed='S'
010000061115     c                   eval      aggsede='S'
010100061115     c                   endif
010200061115     c                   if        d19agg=' ' or d19agg='E'
010300061115     c                   eval      aggfil='S'
010400061115     c                   endif
010500941025     C*
010600941025     C* VEDO PER QUANTI CARATTERI LA DEVO TENERE
010700941025     C                   MOVEA     D19NT1        K35
010800941025     C                   Z-ADD     35            X                 2 0
010900941025    4C     K35(X)        DOWEQ     *BLANKS
011000941025     C                   SUB       1             X
011100941025    4C                   ENDDO
011200941025     C*
011300941025     C* LA DESCRIZIONE SUPERE IL MINIMO DI CARATTERI --> POSSO SCRIVERL
011400941025    4C     X             IFGE      §7KMNC
011500941025     C*
011600060223     c* In update , solo se cambiato
011700061115     c* FILIALE
011800061115    6c                   if        aggfil='S'
011900061115     C     KBOL          CHAIN     FiAR4000                           3033
012000061115     C   33              MOVEL     '2'           D19ERR
012100061115    7c  n33              if        not %found(fiar401l) or (%found(fiar401l)
012200061115     c                             and d19nt1<>ar4not)
012300061115     c
012400061115    8c                   if        not %found(fiar401l)
012500061115     C                   CLEAR                   FiAR4000
012600061115     c                   seton                                        30
012700061115    8c                   endif
012800060223     c
012900061115     c                   EXSR      RiempiCampi
013000060215     c
013100060215     C   30              WRITE     FiAR4000
013200060215     C  n30              UPDATE    FiAR4000
013300061115    7c                   else
013400061115     c                   unlock    fiar401l
013500061115    7c                   endif
013600061115    6c                   endif
013700061115     c* SEDE
013800061115    6c                   if        aggsede='S'
013900061115     c                   setoff                                       0405
014000061115     C     KBOL          CHAIN     tita430c                           3033
014100061115     C   33              MOVEL     '2'           D19ERR
014200061115    7c  n33              if        not %found(tita430c) or (%found(tita430c)
014300061115     c                             and d19nt1<>ar4not)
014400061115     c
014500061115    8c                   if        not %found(tita430c)
014600061115     C                   CLEAR                   tita4000
014700061115     C                   CLEAR                   tita4P00
014800061115     c                   seton                                        30
014900061115    8c                   endif
015000061115     c
015100061115     c                   EXSR      RiempiCampi
015200061115     c
015300061115     c
015400061115    8c                   if        *in30
015500061115     c                   setoff                                       010203
015600061115     c     ktas          chain     titas30c
015700061115     c  n03              write     tita4000
015800061115     c   03              write     tita4p00
015900061115   x8c                   else
016000061115     c   04              update    tita4000
016100061115     c   05              update    tita4p00
016200061115    8c                   endif
016300061115   x7c                   else
016400061115     c                   unlock    tita430c
016500061115    7c                   endif
016600061115     c
016700061115    6C                   ENDIF
016800941025     C*
016900941025   X4C                   ELSE
017000941025     C*
017100941025     C* NON SUPERA I CARATTERI MINIMI PER SCRIVERE IL RECORD --> DELETO
017200061115   4ac                   if        aggfil='S'
017300060215     C     KBOL          DELETE    FiAR4000                           3033
017400941025     C*
017500941025     C* ERRORE: RECORD NON DISPONIBILE
017600941025    5C     *IN33         IFEQ      *ON
017700941025     C                   MOVEL     '2'           D19ERR
017800941025    5C                   ENDIF
017900061115   4aC                   ENDIF
018000061115     c
018100061115    5c                   if        aggsede='S'
018200061115     c                   setoff                                       0506
018300061115     C     KBOL          chain     TITA430c                           3033
018400061115    6c                   if        not *in30 and not *in33
018500061115     C   04              DELETE    TITA4000
018600061115     C   05              DELETE    TITA4p00
018700061115    6c                   endif
018800061115     C*
018900061115     C* ERRORE: RECORD NON DISPONIBILE
019000061115    6C     *IN33         IFEQ      *ON
019100061115     C                   MOVEL     '2'           D19ERR
019200061115    6C                   ENDIF
019300061115    5c                   endif
019400970520     C*
019500970520     C* PULISCO IL CAMPO PASSATO PER FAR SAPERE AL CHIAMANTE CHE
019600970520     C*  LA 2 DESCRIZIONE NON E' STATA MEMORIZZATA
019700970520     C                   CLEAR                   D19NT1
019800061115    4C                   ENDIF
019900941025     C*
020000941025    3C                   ENDIF
020100941025    2C                   ENDIF
020200941025    1C                   ENDIF
020300941025     C*
020400941025     C* CHIUSURA PGM
020500061115     c     fine          tag
020600061115     c
020700941025    1C     D19TLA        IFEQ      ' '
020800941025     C                   RETURN
020900941025     C                   ELSE
021000941025     C                   SETON                                        LR
021100941025    1C                   ENDIF
021200941021     C*
021300941024     C* OPERAZIONI INIZIALI ------------------------------------------*
021400941024     C     *INZSR        BEGSR
021500941025     C     KBOL          KLIST
021600941025     C                   KFLD                    D19AAS
021700941025     C                   KFLD                    D19LNP
021800941025     C                   KFLD                    D19NRS
021900941025     C                   KFLD                    D19NSP
022000941025     C                   KFLD                    D19TRC
022100061115     C     Ktas          KLIST
022200061115     C                   KFLD                    D19AAS
022300061115     C                   KFLD                    D19LNP
022400061115     C                   KFLD                    D19NRS
022500061115     C                   KFLD                    D19NSP
022600941025     C     KTAB          KLIST
022700941025     C                   KFLD                    CODUT             1 0
022800941025     C                   KFLD                    COD               2
022900941025     C                   KFLD                    KEY               8
023000941025     C*
023100941025     C                   Z-ADD     1             CODUT
023200941024     C                   ENDSR
023300941025     C*
023400941025     C* CONTROLLO LE DESCRIZIONI IMMESSE -----------------------------*
023500941025     C     CTRDES        BEGSR
023600941025     C*
023700941025    1C     D19NT1        IFNE      *BLANKS
023800941025     C     D19NT2        ANDNE     *BLANKS
023900941025     C*
024000941025     C                   MOVEA     D19NT1        K35B
024100941025     C                   Z-ADD     35            X
024200941025    2C     K35B(X)       DOWEQ     *BLANKS
024300941025     C                   SUB       1             X
024400941025    2C                   ENDDO
024500941025     C*
024600941025     C                   MOVEA     D19NT2        K35
024700941025     C                   Z-ADD     35            Y                 3 0
024800941025    2C     K35(Y)        DOWEQ     *BLANKS
024900941025     C                   SUB       1             Y
025000941025    2C                   ENDDO
025100941025     C*
025200941025     C                   ADD       Y             X
025300941025    2C     X             IFLT      35
025400941025     C* SOTTRAGGO DA 36 PERCHE' DEVE RIMANARE UNO SPAZIO VUOTO TRA LE
025500941025     C*  3 DESCRIZIONI
025600941025     C     36            SUB       Y             X
025700960118     C                   MOVEA     K35           K35B(X)
025800941025     C                   MOVEA     K35B          D19NT1
025900941025     C                   CLEAR                   D19NT2
026000941025     C                   ELSE
026100941025     C                   MOVEL     '1'           D19ERR
026200941025    2C                   ENDIF
026300941025    1C                   ENDIF
026400941025     C*
026500941025     C* SE PIENA SOLO LA 2 SCRIVO LA 1
026600941025    1C     D19NT1        IFEQ      *BLANKS
026700941025     C     D19NT2        ANDNE     *BLANKS
026800941025     C                   MOVEL     D19NT2        D19NT1
026900941025     C                   CLEAR                   D19NT2
027000941025    1C                   ENDIF
027100941025     C*
027200941025     C                   ENDSR
027300061115     c
027400061115     c* Riempo i campi dei files -----------------------------------------*
027500061115     c     Riempicampi   BEGSR
027600061115    6C     *IN30         IFEQ      *ON
027700061115     C                   MOVEL     D19AAS        AR4AAS
027800061115     C                   MOVEL     D19LNP        AR4LNP
027900061115     C                   MOVEL     D19NRS        AR4NRS
028000061115     C                   MOVEL     D19NSP        AR4NSP
028100061115     C                   MOVEL     D19TRC        AR4TRC
028200061115    6c                   endif
028300061115     c
028400061115     C                   MOVEL     D19NT1        AR4NOT
028500061115     C*
028600061115     C                   TIME                    W0140            14 0
028700061115     C                   MOVE      W0140         WDTGIO            8 0
028800061115     c     *eur          move      wdtgio        dataiso
028900061115     c                   movel     dataiso       ar4duv
029000061115     c
029100061115    6c                   if        d19ftr<>*blanks
029200061115     c                   movel     d19ftr        ar4ftr
029300061115     c                   movel     dataiso       ar4dtr
029400061115     c                   else
029500061115     c                   clear                   ar4ftr
029600061115     c                   clear                   ar4dtr
029700061115    6c                   endif
029800061115     c                   ENDSR
029900061115     c*---------------------------------------------------------------------
030000061115     c* cerco tutti i legami bolla
030100061115     c*---------------------------------------------------------------------
030200061115     c     APRI          BEGSR
030300061115     C
030400061115     c                   if        not %open(titas30c)
030500061115     c* Reperisco S.I.
030600061115     c                   clear                   tibs55ds
030700061115     c* come p.o. di partenza un p.o. di filiale
030800061115     c                   eval      i50ppo=001
030900061115     c* come p.o. di partenza la sede
031000061115     c                   eval      i50apo=046
031100061115     c                   call      'TIBS55R'
031200061115     c                   parm                    tibs55ds
031300061115     c
031400061115     c                   Clear                   comman
031500061115     c                   Movel(p)  cmdt(1)       comman
031600061115     c                   Eval      %Subst(comman:30:10) = o50alg
031700061115     c                   Eval      comman =
031800061115     c                             %trim(comman) + '/TITAS30C)'
031900061115     c                   Call      'QCMDEXC'                            99
032000061115     c                   Parm                    comman
032100061115     c                   Parm                    lenght
032200061115     c  n99              Open      TitaS30c
032300061115     C   99              GOTO      ENDAPRI
032400061115     c                   endif
032500061115     C
032600061115     c                   if        not %open(tita430c)
032700061115     c                   Clear                   comman
032800061115     c                   Movel(p)  cmdt(2)       comman
032900061115     c                   Eval      %Subst(comman:30:10) = o50alg
033000061115     c                   Eval      comman =
033100061115     c                             %trim(comman) + '/TITA430C)'
033200061115     c                   Call      'QCMDEXC'                            99
033300061115     c                   Parm                    comman
033400061115     c                   Parm                    lenght
033500061115     c  n99              Open      Tita430c
033600061115     C   99              GOTO      ENDAPRI
033700061115     c                   endif
033800061115     C
033900061115     c                   if        not %open(fiar401l)
034000061115     c                   Clear                   comman
034100061115     c                   Movel(p)  cmdt(3)       comman
034200061115     c                   Eval      %Subst(comman:30:10) = o50pla
034300061115     c                   Eval      comman =
034400061115     c                             %trim(comman) + '/FIAR401L)'
034500061115     c                   Call      'QCMDEXC'                            99
034600061115     c                   Parm                    comman
034700061115     c                   Parm                    lenght
034800061115     c  n99              Open      FIAR401L
034900061115     C   99              GOTO      ENDAPRI
035000061115     c                   endif
035100061115     c
035200061115     c     ENDAPRI       ENDSR
035300061115**
035400061115OVRDBF FILE(TITAS30C) TOFILE(
035500061115OVRDBF FILE(TITA430C) TOFILE(
035600061115OVRDBF FILE(fiar401L) tofile(
