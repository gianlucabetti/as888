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
003500070420     c                   parm                    P19vuo            1
003600070420     c* il Secondo parametro serve per permettere la scrittura di un record
003700070420     c* con ar4not vuoto a prescindere anche da quanto memorizzato in §7kmnc
003800070420     c* Un esempio di utilizzo è nel pgm FNLRG2R.
003900070420     c* Se = 'S' scrive il record anche se d19nt1/nt2 vuoti
004000070420     c* Se <>'S' deleta record se d19nt1/nt2 vuoti
004100000000     C*---------------------------------------------------------------*
004200941025     C* TIPO LANCIO, D19TLA "C" -           CHIUSO CON LR
004300941025     C* TIPO LANCIO, D19TLA "L" - ELABORO E CHIUDO CON LR
004400941025     C* TIPO LANCIO, D18TLA " " - ELABORO E CHIUDO IN RETRN
004500941025    1C     D19TLA        IFNE      'C'
004600941025     C                   CLEAR                   D19ERR
004700061115     c* Apertura file se non aperti
004800061115     c                   exsr      APRI
004900061115     c*
005000061115     c                   if        *in99
005100061115     c                   movel     '1'           d19err
005200061115     c                   goto      fine
005300061115     c                   endif
005400070420     c*
005500070420     c* Pgm richiamato per la scrittura/aggior.del record con NOT vuoto
005600070420     c*
005700070420     c                   if        %parms>1 and p19vuo='S'
005800070420     c                             and d19nt1=*blanks and d19nt2=*blanks
005900070420     c                   exsr      sr_tab7k
006000070420     c                   exsr      sr_aggfile
006100070420     c                   goto      fine
006200070420     c                   endif
006300070420     c
006400941025     C**
006500941025     C* DESCRIZIONI NON IMMESSE
006600941025     C**
006700941025    2C     D19NT1        IFEQ      *BLANKS
006800941025    2C     D19NT2        ANDEQ     *BLANKS
006900941025     C*
007000941025     C* SE C'ERA DELETO RECORD
007100061115     c* Filiale
007200061115    3c                   if        d19agg=' ' or d19agg='E'
007300060215     C     KBOL          DELETE    Fiar4000                           3033
007400941025     C*
007500941025     C* ERRORE: RECORD NON DISPONIBILE
007600941025     C     *IN33         IFEQ      *ON
007700941025     C                   MOVEL     '2'           D19ERR
007800941025     C                   ENDIF
007900061115    3C                   ENDIF
008000061115     c* Sede
008100061115    3c                   if        d19agg='S' or d19agg='E'
008200061115     c                   setoff                                       0405
008300061115     C     KBOL          chain     TITA430c                           3033
008400061115     c                   if        not *in30 and not *in33
008500061115     C   04              DELETE    TITA4000
008600061115     C   05              DELETE    TITA4p00
008700061115     c                   endif
008800061115     C*
008900061115     C* ERRORE: RECORD NON DISPONIBILE
009000061115     C     *IN33         IFEQ      *ON
009100061115     C                   MOVEL     '2'           D19ERR
009200061115     C                   ENDIF
009300061115    3C                   ENDIF
009400941025     C*
009500941025   X2C                   ELSE
009600941025     C**
009700941025     C* DESCRIZIONE     IMMESSA
009800941025     C**
009900941025     C* CONTROLLO
010000941025     C                   EXSR      CTRDES
010100941025     C*
010200941025     C* NON C'E' ERRORE MEMORIZZO LE NOTE
010300941025    3C     D19ERR        IFEQ      *BLANKS
010400941025     C*
010500070420     c                   exsr      sr_tab7K
010600941025     C*
010700941025     C* VEDO PER QUANTI CARATTERI LA DEVO TENERE
010800941025     C                   MOVEA     D19NT1        K35
010900941025     C                   Z-ADD     35            X                 2 0
011000941025    4C     K35(X)        DOWEQ     *BLANKS
011100941025     C                   SUB       1             X
011200941025    4C                   ENDDO
011300941025     C*
011400941025     C* LA DESCRIZIONE SUPERE IL MINIMO DI CARATTERI --> POSSO SCRIVERL
011500941025    4C     X             IFGE      §7KMNC
011600070420     c                   exsr      sr_aggfile
011700941025     C*
011800941025   X4C                   ELSE
011900941025     C*
012000941025     C* NON SUPERA I CARATTERI MINIMI PER SCRIVERE IL RECORD --> DELETO
012100061115   4ac                   if        aggfil='S'
012200060215     C     KBOL          DELETE    FiAR4000                           3033
012300941025     C*
012400941025     C* ERRORE: RECORD NON DISPONIBILE
012500941025    5C     *IN33         IFEQ      *ON
012600941025     C                   MOVEL     '2'           D19ERR
012700941025    5C                   ENDIF
012800061115   4aC                   ENDIF
012900061115     c
013000061115    5c                   if        aggsede='S'
013100061115     c                   setoff                                       0506
013200061115     C     KBOL          chain     TITA430c                           3033
013300061115    6c                   if        not *in30 and not *in33
013400061115     C   04              DELETE    TITA4000
013500061115     C   05              DELETE    TITA4p00
013600061115    6c                   endif
013700061115     C*
013800061115     C* ERRORE: RECORD NON DISPONIBILE
013900061115    6C     *IN33         IFEQ      *ON
014000061115     C                   MOVEL     '2'           D19ERR
014100061115    6C                   ENDIF
014200061115    5c                   endif
014300970520     C*
014400970520     C* PULISCO IL CAMPO PASSATO PER FAR SAPERE AL CHIAMANTE CHE
014500970520     C*  LA 2 DESCRIZIONE NON E' STATA MEMORIZZATA
014600970520     C                   CLEAR                   D19NT1
014700061115    4C                   ENDIF
014800941025     C*
014900941025    3C                   ENDIF
015000941025    2C                   ENDIF
015100941025    1C                   ENDIF
015200941025     C*
015300941025     C* CHIUSURA PGM
015400061115     c     fine          tag
015500061115     c
015600941025    1C     D19TLA        IFEQ      ' '
015700941025     C                   RETURN
015800941025     C                   ELSE
015900941025     C                   SETON                                        LR
016000941025    1C                   ENDIF
016100941021     C*
016200941024     C* OPERAZIONI INIZIALI ------------------------------------------*
016300941024     C     *INZSR        BEGSR
016400941025     C     KBOL          KLIST
016500941025     C                   KFLD                    D19AAS
016600941025     C                   KFLD                    D19LNP
016700941025     C                   KFLD                    D19NRS
016800941025     C                   KFLD                    D19NSP
016900941025     C                   KFLD                    D19TRC
017000061115     C     Ktas          KLIST
017100061115     C                   KFLD                    D19AAS
017200061115     C                   KFLD                    D19LNP
017300061115     C                   KFLD                    D19NRS
017400061115     C                   KFLD                    D19NSP
017500941025     C     KTAB          KLIST
017600941025     C                   KFLD                    CODUT             1 0
017700941025     C                   KFLD                    COD               2
017800941025     C                   KFLD                    KEY               8
017900941025     C*
018000941025     C                   Z-ADD     1             CODUT
018100941024     C                   ENDSR
018200941025     C*
018300941025     C* CONTROLLO LE DESCRIZIONI IMMESSE -----------------------------*
018400941025     C     CTRDES        BEGSR
018500941025     C*
018600941025    1C     D19NT1        IFNE      *BLANKS
018700941025     C     D19NT2        ANDNE     *BLANKS
018800941025     C*
018900941025     C                   MOVEA     D19NT1        K35B
019000941025     C                   Z-ADD     35            X
019100941025    2C     K35B(X)       DOWEQ     *BLANKS
019200941025     C                   SUB       1             X
019300941025    2C                   ENDDO
019400941025     C*
019500941025     C                   MOVEA     D19NT2        K35
019600941025     C                   Z-ADD     35            Y                 3 0
019700941025    2C     K35(Y)        DOWEQ     *BLANKS
019800941025     C                   SUB       1             Y
019900941025    2C                   ENDDO
020000941025     C*
020100941025     C                   ADD       Y             X
020200941025    2C     X             IFLT      35
020300941025     C* SOTTRAGGO DA 36 PERCHE' DEVE RIMANARE UNO SPAZIO VUOTO TRA LE
020400941025     C*  3 DESCRIZIONI
020500941025     C     36            SUB       Y             X
020600960118     C                   MOVEA     K35           K35B(X)
020700941025     C                   MOVEA     K35B          D19NT1
020800941025     C                   CLEAR                   D19NT2
020900941025     C                   ELSE
021000941025     C                   MOVEL     '1'           D19ERR
021100941025    2C                   ENDIF
021200941025    1C                   ENDIF
021300941025     C*
021400941025     C* SE PIENA SOLO LA 2 SCRIVO LA 1
021500941025    1C     D19NT1        IFEQ      *BLANKS
021600941025     C     D19NT2        ANDNE     *BLANKS
021700941025     C                   MOVEL     D19NT2        D19NT1
021800941025     C                   CLEAR                   D19NT2
021900941025    1C                   ENDIF
022000941025     C*
022100941025     C                   ENDSR
022200061115     c
022300061115     c* Riempo i campi dei files -----------------------------------------*
022400061115     c     Riempicampi   BEGSR
022500061115    6C     *IN30         IFEQ      *ON
022600061115     C                   MOVEL     D19AAS        AR4AAS
022700061115     C                   MOVEL     D19LNP        AR4LNP
022800061115     C                   MOVEL     D19NRS        AR4NRS
022900061115     C                   MOVEL     D19NSP        AR4NSP
023000061115     C                   MOVEL     D19TRC        AR4TRC
023100061115    6c                   endif
023200061115     c
023300061115     C                   MOVEL     D19NT1        AR4NOT
023400061115     C*
023500061115     C                   TIME                    W0140            14 0
023600061115     C                   MOVE      W0140         WDTGIO            8 0
023700061115     c     *eur          move      wdtgio        dataiso
023800061115     c                   movel     dataiso       ar4duv
023900061115     c
024000061115    6c                   if        d19ftr<>*blanks
024100061115     c                   movel     d19ftr        ar4ftr
024200061115     c                   movel     dataiso       ar4dtr
024300061115     c                   else
024400061115     c                   clear                   ar4ftr
024500061115     c                   clear                   ar4dtr
024600061115    6c                   endif
024700061115     c                   ENDSR
024800061115     c*---------------------------------------------------------------------
024900061115     c* cerco tutti i legami bolla
025000061115     c*---------------------------------------------------------------------
025100061115     c     APRI          BEGSR
025200061115     C
025300061115     c                   if        not %open(titas30c)
025400061115     c* Reperisco S.I.
025500061115     c                   clear                   tibs55ds
025600061115     c* come p.o. di partenza un p.o. di filiale
025700061115     c                   eval      i50ppo=001
025800061115     c* come p.o. di partenza la sede
025900061115     c                   eval      i50apo=046
026000061115     c                   call      'TIBS55R'
026100061115     c                   parm                    tibs55ds
026200061115     c
026300061115     c                   Clear                   comman
026400061115     c                   Movel(p)  cmdt(1)       comman
026500061115     c                   Eval      %Subst(comman:30:10) = o50alg
026600061115     c                   Eval      comman =
026700061115     c                             %trim(comman) + '/TITAS30C)'
026800061115     c                   Call      'QCMDEXC'                            99
026900061115     c                   Parm                    comman
027000061115     c                   Parm                    lenght
027100061115     c  n99              Open      TitaS30c
027200061115     C   99              GOTO      ENDAPRI
027300061115     c                   endif
027400061115     C
027500061115     c                   if        not %open(tita430c)
027600061115     c                   Clear                   comman
027700061115     c                   Movel(p)  cmdt(2)       comman
027800061115     c                   Eval      %Subst(comman:30:10) = o50alg
027900061115     c                   Eval      comman =
028000061115     c                             %trim(comman) + '/TITA430C)'
028100061115     c                   Call      'QCMDEXC'                            99
028200061115     c                   Parm                    comman
028300061115     c                   Parm                    lenght
028400061115     c  n99              Open      Tita430c
028500061115     C   99              GOTO      ENDAPRI
028600061115     c                   endif
028700061115     C
028800061115     c                   if        not %open(fiar401l)
028900061115     c                   Clear                   comman
029000061115     c                   Movel(p)  cmdt(3)       comman
029100061115     c                   Eval      %Subst(comman:30:10) = o50pla
029200061115     c                   Eval      comman =
029300061115     c                             %trim(comman) + '/FIAR401L)'
029400061115     c                   Call      'QCMDEXC'                            99
029500061115     c                   Parm                    comman
029600061115     c                   Parm                    lenght
029700061115     c  n99              Open      FIAR401L
029800061115     C   99              GOTO      ENDAPRI
029900061115     c                   endif
030000061115     c
030100061115     c     ENDAPRI       ENDSR
030200070420     c*---------------------------------------------------------------------
030300070420     c* Chain tabella 7k ed imposto flag per aggiornamento file sede/filiale
030400070420     c*---------------------------------------------------------------------
030500070420     c     sr_tab7k      BEGSR
030600070420     C* CHAIN SU TABEL CON TIPO RECORD
030700070420     C                   MOVEL     '7K'          COD
030800070420     C                   MOVEL(P)  D19TRC        KEY
030900070420     C     KTAB          CHAIN     TABEL                              30
031000070420     C  N30              MOVEL     TBLUNI        DS7K
031100070420     C   30              CLEAR                   DS7K
031200070420     c
031300070420     c                   clear                   aggsede
031400070420     c                   clear                   aggfil
031500070420     c* Per SEDE vedo se c'e' flag in tabella 7K
031600070420     c                   if        d19agg='S' or d19agg='E' and §7ksed='S'
031700070420     c                   eval      aggsede='S'
031800070420     c                   endif
031900070420     c                   if        d19agg=' ' or d19agg='E'
032000070420     c                   eval      aggfil='S'
032100070420     c                   endif
032200070420     c                   endsr
032300070420     c*---------------------------------------------------------------------
032400070420     c* Aggiornamento file fiar4/tita4                                 liale
032500070420     c*---------------------------------------------------------------------
032600070420     c     sr_aggfile    BEGSR
032700070420     C*
032800070420     c* In update , solo se cambiato
032900070420     c* FILIALE
033000070420    6c                   if        aggfil='S'
033100070420     C     KBOL          CHAIN     FiAR4000                           3033
033200070420     C   33              MOVEL     '2'           D19ERR
033300070420    7c  n33              if        not %found(fiar401l) or (%found(fiar401l)
033400070420     c                             and d19nt1<>ar4not)
033500070420     c
033600070420    8c                   if        not %found(fiar401l)
033700070420     C                   CLEAR                   FiAR4000
033800070420     c                   seton                                        30
033900070420    8c                   endif
034000070420     c
034100070420     c                   EXSR      RiempiCampi
034200070420     c
034300070420     C   30              WRITE     FiAR4000
034400070420     C  n30              UPDATE    FiAR4000
034500070420    7c                   else
034600070420     c                   unlock    fiar401l
034700070420    7c                   endif
034800070420    6c                   endif
034900070420     c* SEDE
035000070420    6c                   if        aggsede='S'
035100070420     c                   setoff                                       0405
035200070420     C     KBOL          CHAIN     tita430c                           3033
035300070420     C   33              MOVEL     '2'           D19ERR
035400070420    7c  n33              if        not %found(tita430c) or (%found(tita430c)
035500070420     c                             and d19nt1<>ar4not)
035600070420     c
035700070420    8c                   if        not %found(tita430c)
035800070420     C                   CLEAR                   tita4000
035900070420     C                   CLEAR                   tita4P00
036000070420     c                   seton                                        30
036100070420    8c                   endif
036200070420     c
036300070420     c                   EXSR      RiempiCampi
036400070420     c
036500070420     c
036600070420    8c                   if        *in30
036700070420     c                   setoff                                       010203
036800070420     c     ktas          chain     titas30c
036900070420     c  n03              write     tita4000
037000070420     c   03              write     tita4p00
037100070420   x8c                   else
037200070420     c   04              update    tita4000
037300070420     c   05              update    tita4p00
037400070420    8c                   endif
037500070420   x7c                   else
037600070420     c                   unlock    tita430c
037700070420    7c                   endif
037800070420     c
037900070420    6C                   ENDIF
038000070420     c                   endsr
038100061115**
038200061115OVRDBF FILE(TITAS30C) TOFILE(
038300061115OVRDBF FILE(TITA430C) TOFILE(
038400061115OVRDBF FILE(fiar401L) tofile(
