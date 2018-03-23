000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300890921     F*
000400090424     FTITAS31C  IF   E           K DISK
000500990823     FTITA730C  IF   E           K DISK
000600050905     FFNORM01L  IF   E           K DISK
000700090423     FAZORG01L  if   e           k disk
000800910830     FTABEL00F  IF   E           K DISK
000900040531     FFIAR531C  IF   E           K DISK
001000050906     FTIVGD00F  UF A E             DISK
001100050906     D VSV             S              1    DIM(30)                              SIGLA VARIE
001200990910     D VVA             S             11  3 DIM(30)                              IMPOR VARIE
001300090423     D VALNUM          S             10    DIM(1) CTDATA PERRCD(10)
001400890921     D KPJBA         E DS
001500990817     D DS4C          E DS
001600050906     D DSQT1         E DS
001700050906     D DS$2          E DS
001800030516     D DTA4A         E DS
001900160209     D DTA4M         E DS
002000151016     D DAR5FAT       E DS
002100050905     D tivafds       e ds                  extname(tivaf00t)
002200090423     D PARAM           DS
002300090423     D  PARDATADA                     8S 0
002400090423     D  PARDATAA                      8S 0
002500090424     D  PARCLI                        7S 0
002600090424     D  PARTIPCLI                     1A
002700090423     D  PARCLIVGD                     8A
002800090424     D  PARTIPFIL                     2A
002900090424     D  PARGGBACK                     3S 0
003000090424     D  PARGGDAELAB                   3S 0
003100090424
003200010402     D*------------------
003300030917      * DS FIAR5
003400030917     d dAr5Bnb       e ds
003500050126
003600050126     d Tibs02ds      e ds
003700111026     d****** dvus          e ds
003800051219
003900051219     d Og143         e ds                  inz
004000060127
004100060127     D trul47ds      e ds
004200030917      *
004300021212     D Wtara           s                   like(§QTTPC)
004400021212     D Wntara          s                   like(TASPKF)
004500030917
004600030917     d kAr5Trd         s                   Like(Ar5Trd)
004700030515
004800030515     D Rma15           s             15
004900030515     D WRma15          s             15
005000030515     D II              s              2  0
005100030515     D Pos             s              2  0
005200160209
005300160209     D*--------------------
005400160209     D* DEFINIZIONI A PROCEDURE ESTERNE
005500160209     D*--------------------
005600160209     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
005700160209
005800010402
005900010402
006000060127     C*
006100090424     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente
006200060127     C                   clear                   trul47ds
006300060127     C                   eval      d47opz  = 'I'
006400090424     C                   eval      d47tip  = PARTIPFIL
006500060127     C                   eval      d47lck  = 'N'
006600060127     C                   eval      d47chkj = 'N'
006700090423     C                   eval      d47pgm  = 'TRTC28R2'
006800060127     C                   call      'TRUL47R'
006900060127     C                   parm                    trul47ds
007000060127     C*
007100060127     C* Se elaborazione consentita => proseguo
007200060127     C                   if        d47sts <> 'A'
007300060127     C*
007400921023     C                   EXSR      DEFCAM
007500090424      *
007600090424      * Gestisco se richiesto cliente SINGOLO o UNIFICANTE
007700090424     C                   if        PARTIPCLI = 'U'
007800040806      *
007900921023     C     KTB           SETLL     TABEL
008000040806    1C                   DO        *HIVAL
008100921023     C     KTB           READE     TABEL                                  58
008200040806    2C  N58              DO
008300040806    3C     TBLFLG        IFNE      '*'
008400921023     C                   MOVEL     TBLUNI        DS4C
008500040806    4C     §4CFSD        IFNE      *BLANK                                        CLI DA SCAR
008600050906     C                   MOVE      §4CFKS        KSCU              7 0
008700050906     C                   MOVEL     TBLKEY        KSC               7 0
008800981203     C*
008900090424    5C     PARCLI        IFGT      *ZERO                                         PARZIALIZZ.
009000090424     C     KSCU          ANDEQ     PARCLI                                        CLIENTE
009100000721     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
009200090424   e5C                   ENDIF
009300090424   e4C                   ENDIF
009400090424   e3C                   ENDIF
009500090424   e2C                   ENDDO
009600090424   e1C  N58              ENDDO
009700060127     C*
009800090424     C                   else
009900090424     C                   eval      KSC = PARCLI
010000090424     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
010100090424     C                   endif
010200090424     C*
010300060127     C                   endif
010400060127     C*
010500060127     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
010600060127     C                   clear                   trul47ds
010700060127     C                   eval      d47opz  = 'F'
010800090424     C                   eval      d47tip  = PARTIPFIL
010900060127     C                   call      'TRUL47R'
011000060127     C                   parm                    trul47ds
011100160209     C*
011200160209     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
011300160209     C                   CALL      'UBTA400R'
011400160209     C                   PARM                    UBTA4IOPZ
011500160209     C                   PARM      'C'           UBTA4ITLA
011600160209     C                   PARM                    UBTA4IAAS
011700160209     C                   PARM                    UBTA4ILNP
011800160209     C                   PARM                    UBTA4INRS
011900160209     C                   PARM                    UBTA4INSP
012000160209     C                   PARM                    UBTA4ITRC
012100160209     C                   PARM                    UBTA4OERR
012200160209     C                   PARM                    UBTA4ODS
012300160209     C                   PARM                    UBTA4OLEN
012400160209     C                   PARM                    UBTA4ODATI
012500991110     C*
012600921023     C                   SETON                                        LR
012700910830     C*----------------------------------------------------*
012800910830     C     DEFCAM        BEGSR
012900021212      *
013000021212      ** Aggancio tabella $2 - Varie forzate a programma
013100990817     C                   Z-ADD     1             KTBKUT
013200990817     C                   MOVEL     '$2'          KTBCOD
013300990817     C                   MOVEL     1             KTBKEY
013400990817     C     KEYTAB        CHAIN     TABEL00F                           99
013500990817IF  1C     *IN99         IFEQ      *OFF
013600990817     C     TBLFLG        ANDEQ     *BLANKS
013700990817     C                   MOVEL     TBLUNI        DS$2
013800990817E   1C                   ENDIF
013900021212      *
014000050905      ** Aggancio tabella QT - campi standard tassazione
014100021212     C                   MOVEL     'QT'          KTBCOD
014200021212     C                   MOVEL     1             KTBKEY
014300021212     C     KEYTAB        CHAIN     TABEL00F                           99
014400021212IF  1C     *IN99         IFEQ      *OFF
014500021212     C     TBLFLG        ANDEQ     *BLANKS
014600021212     C                   MOVEL     TBLUNI        DSQT1
014700021212E   1C                   ENDIF
014800990817     C*
014900021212      ** Imposto key per tabella clienti con ritorno info fattura
015000990817     C                   MOVE      1             TBLKUT
015100990817     C                   MOVEL     '4C'          TBLCOD
015200990817     C                   MOVEL     *BLANKS       TBLKEY
015300990817     C*
015400910830     C                   ENDSR
015500050906     C*--------------------------------------------------------
015600090423     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
015700000124     C*--------------------------------------------------------
015800921023     C     WRTFIL        BEGSR
015900090423     C*
016000090424     C* Mi posiziono x cliente
016100090423     C                   setoff                                       70
016200090423     C*
016300090424     C     KTAS          SETLL     TITAS31C
016400090424     C                   IF        %found(titas31C)
016500090424     C                   READ      titas31c
016600090424     C                   DOW       not %eof(titas31c) and not *in70
016700090423     C*
016800090423     C* Verifico se data spedizione <= DATAA
016900090424     C                   if        tasKSC <> KSC
017000090423     C                   seton                                        70
017100090423     C                   else
017200090424     C                   if        tasAAS*10000+tasMGS >= wDataDa AND
017300090424     C                             tasAAS*10000+tasMGS <= wDataA
017400050905     C*
017500050905     C* Inizializzo il buffer d output
017600050905     C                   clear                   TIVAFDS
017700160209     C*
017800160209     C* Reperisco TITA4 tipo record 'A'
017900160209     C                   MOVEL     'A'           UBTA4ITRC
018000160209     C                   EXSR      RTVTA4
018100050905     C*
018200941122     C                   MOVEL     TASTBL        VAFTBL
018300941122     C                   Z-ADD     TASLNP        VAFLNP
018400941122     C                   Z-ADD     TASMGS        VAFMGS
018500941122     C                   Z-ADD     TASAAS        VAFAAS
018600941122     C                   Z-ADD     TASNRS        VAFNRS
018700941122     C                   Z-ADD     TASNSP        VAFNSP
018800941122     C                   Z-ADD     TASKSC        VAFKSC
018900941122     C                   MOVEL     TASFIN        VAFFIN
019000941122     C                   Z-ADD     TASLNA        VAFLNA
019100030917      * se particolità varia = 'O' recupero da FIAR5 il numero colli originali
019200030917      *
019300050906     c                   If        %Subst(TasGva:1:1) = 'O'
019400050906
019500050906     c                   eval      kAr5Trd  = 'BNB'
019600050906     c                   clear                   dAr5Bnb
019700050906     c     kFiar5        Chain     Fiar531c
019800050906     c                   If        %Found(Fiar531c)
019900050906     c                   Movel     Ar5Uni        dAr5Bnb
020000050906     c                   EndIf
020100050906      * Colli originali
020200050906     c                   Eval      VafNcl = §Ar5bcor
020300030917
020400030917     c                   else
020500030917      * Colli TITAS
020600941122     C                   Z-ADD     TASNCL        VAFNCL
020700030917     c                   endif
020800030917
020900941122     C                   Z-ADD     TASVLF        VAFVLF
021000941122     C                   Z-ADD     TASPOR        VAFPOR
021100021211      * PESO
021200021211     C                   Z-ADD     TASPKF        VAFPKB
021300021211     C                   IF        TASFPF = 'V'
021400150716     C*
021500150716     C                   IF        TASDFT <= §QTDST
021600150716     C     §QTTCO        MULT      TASNCP        WTARA
021700150716     C     TASPKC        SUB(h)    WTARA         WNTARA
021800150716     C                   Z-ADD     WNTARA        VAFPKC
021900150716     C                   ELSE
022000150716     C     §QTTPC        MULT      TASNCP        WTARA
022100150716     C     TASPKC        SUB(h)    WTARA         WNTARA
022200150716     C                   Z-ADD     WNTARA        VAFPKC
022300150716     C                   ENDIF
022400150716     C*
022500021211     C                   ELSE
022600151020     C                   IF        TASFPF <> 'D'
022700150716     C                   Z-ADD     *zeros        VAFPKC
022800151020     C                   ENDIF
022900021211     C                   ENDIF
023000000124     C* AZZERO CAMPI NON SEMPRE PRESENTI
023100990910     C                   MOVE      *ZERO         WSV
023200990910     C                   MOVE      *ZERO         WVA
023300990910     C                   MOVEA     *BLANKS       VSV
023400990910     C                   MOVEA     *ZERO         VVA
023500990910     C                   CLEAR                   A
023600990910     C                   CLEAR                   VAFSV1
023700990910     C                   CLEAR                   VAFVA1
023800990910     C                   CLEAR                   VAFSV2
023900990910     C                   CLEAR                   VAFVA2
024000990910     C                   CLEAR                   VAFSV3
024100990910     C                   CLEAR                   VAFVA3
024200000124     C                   CLEAR                   VAFSV4
024300000124     C                   CLEAR                   VAFVA4
024400000124     C                   CLEAR                   VAFSV5
024500000124     C                   CLEAR                   VAFVA5
024600000124     C                   CLEAR                   VAFSV6
024700000124     C                   CLEAR                   VAFVA6
024800000124     C                   CLEAR                   VAFSV7
024900000124     C                   CLEAR                   VAFVA7
025000000124     C                   CLEAR                   VAFSV8
025100000124     C                   CLEAR                   VAFVA8
025200990910     C                   CLEAR                   VAFVAX
025300000124     C*
025400000124     C                   MOVEL     TASSV1        WSV
025500990910     C                   Z-ADD     TASVA1        WVA
025600990910     C                   EXSR      CONTR
025700990910     C                   MOVEL     TASSV2        WSV
025800990910     C                   Z-ADD     TASVA2        WVA
025900990910     C                   EXSR      CONTR
026000990910     C                   MOVEL     TASSV3        WSV
026100990910     C                   Z-ADD     TASVA3        WVA
026200990910     C                   EXSR      CONTR
026300990817     C*
026400941122     C                   Z-ADD     TASDFT        VAFDFT
026500941122     C                   Z-ADD     TASNFT        VAFNFT
026600030515      *
026700030515     c                   if        tascbo <> 'FO'
026800941122     C                   Z-ADD     TASRMN        VAFRMN
026900030515     c                   endif
027000030515      *
027100941122     C                   MOVEL     TASFTC        VAFFTC
027200941122     C                   MOVEL     TASTSP        VAFTSP
027300941122     C                   MOVEL     TASCTS        VAFCTS
027400941122     C                   MOVEL     TASFAP        VAFFAP
027500941122     C                   Z-ADD     TASCTR        VAFCTR
027600941122     C                   Z-ADD     TASQFT        VAFQFT
027700941122     C                   MOVEL     TASTC2        VAFTC2
027800000124     C                   Z-ADD     TASIMV        VAFIMV
027900000124     C                   Z-ADD     TASFIV        VAFFIV
028000000124     C                   MOVEL     TASDIV        VAFDIV
028100000124     C                   Z-ADD     TASDRT        VAFDRT
028200160209     C                   MOVEL     §TA4ANAS      VAFNAS
028300000124     C                   MOVEL     TASCAD        VAFCAD
028400000124     C                   MOVEL     TASLOD        VAFLOD
028500000124     C                   MOVEL     TASPRD        VAFPRD
028600000124     C                   MOVEL     TASNZD        VAFNZD
028700000124     C                   MOVEL     TASVAS        VAFVAS
028800000124     C                   Z-ADD     TASIAS        VAFIAS
028900000124    *C                   MOVEL     *blanks       VAFFPC
029000000124    *C                   MOVEL     *blanks       VAFFVC
029100000124    *C                   Z-ADD     *zeros        VAFVLC
029200941122     C     TASSV3        IFNE      *BLANK
029300941122     C                   EXSR      SRTA7
029400941122     C                   END
029500030515     C                   MOVEL     *BLANKS       VAFRMA
029600030515     C* se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
029700160222     c                   if        tascbo <> 'FO'
029800160209     C                   MOVEL     DTA4A         VAFRMA
029900000124     C                   ENDIF
030000030515     C* se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
030100160222     c                   if        tascbo = 'FO'
030200030515     c                   if        §ta4arma <> *blanks
030300030515     C                   MOVEL     §TA4ARMA      Rma15
030400030515      * verifico se campo numerico escludendo i blanks
030500030515     c                   eval      II = 1
030600030515     c                   dow       II <= %len(%trim(Rma15))
030700030515      *
030800030515     c                   If        %subst(%trim(Rma15):II:1) <> *blank
030900030515     c                   eval      WRma15 = (%trim(Wrma15)
031000030515     c                                      + %subst(%trim(Rma15):II:1))
031100030515     c                   endif
031200030515      *
031300030515     c                   eval      II = II + 1
031400030515      *
031500030515     c                   enddo
031600030515     c                   clear                   rma15
031700030515     c                   evalr     rma15 = %trim(Wrma15)
031800030515      *
031900030515     c                   eval      II = 1
032000030515     c                   setoff                                       29
032100030515     c                   dow       II <= %len(%trim(Rma15))
032200030515      *
032300030515     c                   If        %subst(%trim(Rma15):II:1) <> *blank
032400030516     c                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
032500030516     c                             VALNUM(1))
032600030515     c                   if        pos = 0
032700030515     c                   seton                                        29
032800030515     c                   leave
032900030515     c                   endif
033000030515     c                   endif
033100030515      *
033200030515     c                   eval      II = II + 1
033300030515      *
033400030515     c                   enddo
033500030515     c                   if        *in29 = *OFF
033600030516     c                   eval      VAFRMA= %editc(tasrmn:'Z')
033700030516     C                   MOVEL     rma15         VAFRMN
033800030515     c                   else
033900030515     C                   EVAL      VAFRMA = §ta4arma
034000030515     C                   EVAL      VAFRMN = tasrmn
034100030515     c                   endif
034200030515      *
034300030515     C                   endif
034400030515     C                   endif
034500990910     C*
034600990910     C* SCRIVO LE VARIE
034700990910     C                   Z-ADD     1             A
034800990910     C     VSV(A)        DOWNE     ' '
034900990910     C                   SELECT
035000990910     C     VAFSV1        WHENEQ    ' '
035100990910     C                   MOVEL     VSV(A)        VAFSV1
035200990910     C                   Z-ADD     VVA(A)        VAFVA1
035300990910     C*
035400990910     C     VAFSV2        WHENEQ    ' '
035500990910     C                   MOVEL     VSV(A)        VAFSV2
035600990910     C                   Z-ADD     VVA(A)        VAFVA2
035700990910     C*
035800990910     C     VAFSV3        WHENEQ    ' '
035900990910     C                   MOVEL     VSV(A)        VAFSV3
036000990910     C                   Z-ADD     VVA(A)        VAFVA3
036100000124     C*
036200000124     C     VAFSV4        WHENEQ    ' '
036300000124     C                   MOVEL     VSV(A)        VAFSV4
036400000124     C                   Z-ADD     VVA(A)        VAFVA4
036500000124     C*
036600000124     C     VAFSV5        WHENEQ    ' '
036700000124     C                   MOVEL     VSV(A)        VAFSV5
036800000124     C                   Z-ADD     VVA(A)        VAFVA5
036900000124     C*
037000000124     C     VAFSV6        WHENEQ    ' '
037100000124     C                   MOVEL     VSV(A)        VAFSV6
037200000124     C                   Z-ADD     VVA(A)        VAFVA6
037300000124     C*
037400000124     C     VAFSV7        WHENEQ    ' '
037500000124     C                   MOVEL     VSV(A)        VAFSV7
037600000124     C                   Z-ADD     VVA(A)        VAFVA7
037700000124     C*
037800000124     C     VAFSV8        WHENEQ    ' '
037900000124     C                   MOVEL     VSV(A)        VAFSV8
038000000124     C                   Z-ADD     VVA(A)        VAFVA8
038100990910     C                   OTHER
038200990910     C                   ADD       VVA(A)        VAFVAX
038300990910     C                   ENDSL
038400990910     C*
038500990910     C                   ADD       1             A
038600990910     C                   ENDDO
038700050126      * Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
038800050126     c                   Clear                   VafRmx
038900050126     C                   MOVEL     VAFKSC        WLB3              3 0
039000051219     c     WLB3          chain     AZORG
039100051219     c                   if            %found(AZORG01L)
039200051219     c                             and ORGfva = *blanks
039300051219     c                   movel     ORGde3        Og143
039400051219     c                   else
039500051219     c                   clear                   Og143
039600051219     c                   endif
039700050126
039800050126     c                   Select
039900050126
040000050126     C* Recupera Riferimento Alfanumerico da ORM
040100050126     C                   When      §4cRfo = 'S'
040200160209     C*
040300160209     C* Reperisco TITA4 tipo record 'M'
040400160209     C                   MOVEL     'M'           UBTA4ITRC
040500160209     C                   EXSR      RTVTA4
040600160209     C*
040700160209     C                   IF        DTA4M <> *zeros
040800050126     C     KORM          CHAIN     FNORM01L                           61
040900050126     C     *IN61         IFEQ      *OFF
041000050126     C                   MOVEL     ORMRFA        VAFRMX
041100050126     C                   ENDIF
041200050126     C                   ENDIF
041300160209     C*
041400050126      * Ritorna al cliente il campo XCO
041500111026     c***                When      §4cRfo = 'X' and TasXco <> *Blanks
041600050126      * recupero il valore da tornare dalla tabella VUS
041700111026     c***                Clear                   Tibs02ds
041800111026     c***                Clear                   dvus
041900111026     c***                Eval      T02mod = 'C'
042000111026     c***                Eval      T02sif = knsif
042100111026     c***                Eval      T02cod = 'VUS'
042200111026     c***                Eval      T02ke1 = TasXco
042300111026     c***                Call      'TIBS02R'
042400111026     c***                Parm                    kpjba
042500111026     c***                Parm                    Tibs02ds
042600111026     c***                If        T02err = *blanks
042700111026     c***                Eval      dvus = T02uni
042800111026     c***                EndIf
042900111026     c***                Eval      vafrmx = §vuvaf
043000050126
043100160209     C*
043200000122     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
043300051219     c                   When      §OGntw = 'DPD'
043400000125     C                   movel     TASDCM        VAFRMX
043500090423     C
043600050126     c                   EndSl
043700151016     C*
043800151016     C* Eseguo eventuali "forzature"
043900151016     C                   EXSR      RTVAR5FAT
044000060203     C*
044100060203     C* Quindi scarico il buffer nel file d out generico download
044200050905     C                   exsr      WRIVGD
044300050905     C*
044400090423     C                   endif
044500090424     C                   endif
044600090423     C*
044700090423     C* Proseguo la lettura delle bolle
044800090424     C                   read      titas31c
044900090423     C*
045000090423     C                   ENDDO
045100090423     C                   ENDIF
045200090423     C*
045300000124     C                   ENDSR
045400000124     C*-----------------------------------------------------------*
045500000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
045600000124     C*-----------------------------------------------------------*
045700941122     C     SRTA7         BEGSR
045800990817     C*
045900990823     C     KTA7          SETLL     TITA730C
046000990823     C     KTA7          READE     TITA730C                               59
046100990817DO  1C     *IN59         DOWEQ     *OFF
046200990910     C                   MOVEL     TA7SVN        WSV
046300990910     C                   Z-ADD     TA7VAN        WVA
046400990910     C                   EXSR      CONTR
046500990910     C*
046600990823     C     KTA7          READE     TITA730C                               59
046700990817E   1C                   ENDDO
046800990817     C*
046900941122     C                   ENDSR
047000990910     C*--------------------------------------------------------
047100000124     C* CONTROLLO SE VARIE GIA' PRESENTI
047200000124     C*--------------------------------------------------------
047300990910     C     CONTR         BEGSR
047400000124IF  1C     WSV           IFNE      ' '
047500990910     C*
047600990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
047700990910     C     WSV           ANDNE     §$2IVA
047800990910     C                   ADD       1             A                 3 0
047900990910     C                   MOVEL     WSV           VSV(A)
048000990910     C                   Z-ADD     WVA           VVA(A)
048100990910E   4C                   ENDIF
048200000124E   1C                   ENDIF
048300000124     C                   ENDSR
048400090423
048500090423
048600050905
048700050905      /TITLE Scrittura record TIVAF in file TIVGD00F (file VAS generico download)
048800050905     C     wriVGD        BEGSR
048900050905     C*
049000050905     C                   clear                   tivgd000
049100090423     C                   eval      vgdDTA = TIVAFDS
049200090424     C                   eval      vgdTIP = PARTIPFIL
049300090423     C                   eval      vgdKSU = PARCLIVGD
049400090423     C                   eval      vgdTSC = 'WW'
049500050905     C                   eval      vgdDAT = datcor
049600090423     C                   eval      vgdPGM = 'TRTC28R2'
049700050905     C                   write     tivgd000
049800050905     C*
049900050905     C                   ENDSR
050000151016
050100151016
050200151016
050300151016     C     RTVAR5FAT     BEGSR
050400151016     C*
050500151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
050600151028     C                   SETOFF                                       7172
050700151016     C                   EVAL      kAr5Trd = 'FAT'
050800151016     C                   CLEAR                   DAR5FAT
050900151016     C     kFiar5        CHAIN     FIAR531C
051000151016     C                   IF        %found(FIAR531C)
051100151016     C                   MOVEL     AR5UNI        DAR5FAT
051200161214     C                   IF        §AR5FPTAS = 'D' AND §AR5PKTAS > *zeros
051300151028     C                   SETON                                        71
051400151016     C                   EVAL      VAFPKC = §AR5PKTAS
051500151016     C                   ENDIF
051600161214     C                   IF        §AR5FVTAS = 'D' AND §AR5VLTAS > *zeros
051700151028     C                   SETON                                        72
051800160505     C                   EVAL      VAFVLC = §AR5VLTAS
051900151016     C                   ENDIF
052000151016     C                   ENDIF
052100151016     C*
052200151016     C                   ENDSR
052300160209
052400160209
052500160209
052600160209     C     RTVTA4        BEGSR
052700160209     C*
052800160209     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
052900160209     C                   CLEAR                   DTA4A
053000160209     C                   CLEAR                   DTA4M
053100160209     C*
053200160209     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
053300160209     C                   CALL      'UBTA400R'
053400160209     C                   PARM      *blanks       UBTA4IOPZ
053500160209     C                   PARM      *blanks       UBTA4ITLA
053600160209     C                   PARM      tasAAS        UBTA4IAAS
053700160209     C                   PARM      tasLNP        UBTA4ILNP
053800160209     C                   PARM      tasNRS        UBTA4INRS
053900160209     C                   PARM      tasNSP        UBTA4INSP
054000160209     C                   PARM                    UBTA4ITRC
054100160209     C                   PARM                    UBTA4OERR
054200160209     C                   PARM                    UBTA4ODS
054300160209     C                   PARM                    UBTA4OLEN
054400160209     C                   PARM                    UBTA4ODATI
054500160209     C*
054600160209     C                   IF        UBTA4OERR = *zeros
054700160209     C                   SELECT
054800160209     C* Gestione output tipo record 'A'
054900160209     C                   WHEN      UBTA4ODS = 'DTA4A'
055000160209     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
055100160209     C* Gestione output tipo record 'M'
055200160209     C                   WHEN      UBTA4ODS = 'DTA4M'
055300160209     C                   EVAL      DTA4M = %subst(UBTA4ODATI:1:UBTA4OLEN)
055400160209     C                   ENDSL
055500160209     C*
055600160209     C                   ENDIF
055700160209     C*
055800160209     C                   ENDSR
055900151016
056000151016
056100151016
056200090424     C*----------------------------------------------------*
056300090424     C     *INZSR        BEGSR
056400090424     C*
056500090424     C     *ENTRY        PLIST
056600090424     C                   PARM                    KPJBA
056700090424     C                   MOVEL     KPJBU         PARAM
056800090424      *
056900090424      * Calcola la data corrente
057000160209     c                   z-add     *zeros        datcor            8 0          *d.corrente aa/m/g
057100160209     C                   eval      datcor = %dec(%date() : *ISO)
057200090424      *
057300090424      * Se nn già ricevute nei parametri d lancio calcolo le date DA/A
057400090424     C                   z-add     *hival        wDataDa           8 0
057500090424     C                   z-add     *hival        wDataA            8 0
057600090424     C                   if        PARDATADA > *zeros AND
057700090424     C                             PARDATAA  > *zeros
057800090424     C                   z-add     PARDATADA     wDataDa
057900090424     C                   z-add     PARDATAA      wDataA
058000090424     C                   else
058100090424     C                   eval      wDataDa=%dec(%date(datcor)-
058200090424     C                                     %days(PARGGBACK))
058300090424     C                   eval      wDataA =%dec(%date(wDataDa)+
058400090424     C                                     %days(PARGGDAELAB))
058500090424     C                   endif
058600090424      *
058700090424      * Verifico i parametri ricevuti in ingresso
058800090424     C                   if        PARCLIVGD = *zeros
058900090424     C                   movel     *all'0'       PARCLIVGD
059000090424     C                   move      PARCLI        PARCLIVGD
059100090424     C                   endif
059200090424      *
059300090424     C* TABEL
059400090424     C     KTB           KLIST
059500090424     C                   KFLD                    TBLKUT
059600090424     C                   KFLD                    TBLCOD
059700090424      * FIAR
059800090424     c     kFiar5        Klist
059900090424     c                   Kfld                    TasAas
060000090424     c                   Kfld                    TasLnp
060100090424     c                   Kfld                    TasNrs
060200090424     c                   Kfld                    TasNsp
060300090424     c                   Kfld                    kAr5Trd
060400090424     C* TITAS
060500090424     C     KTAS          KLIST
060600090424     C                   KFLD                    KSC
060700090424     C* TITA7
060800090424     C     KTA7          KLIST
060900090424     C                   KFLD                    TASAAS
061000090424     C                   KFLD                    TASLNP
061100090424     C                   KFLD                    TASNRS
061200090424     C                   KFLD                    TASNSP
061300090424     C                   KFLD                    TASTBL
061400090424     C     *LIKE         DEFINE    TASVA1        WVA
061500090424     C     *LIKE         DEFINE    TASSV1        WSV
061600090424     C*
061700090424     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
061800090424     C     KORM          KLIST
061900160209     C                   KFLD                    §TA4MPOE
062000160209     C                   KFLD                    §TA4MNSR
062100160209     C                   KFLD                    §TA4MNOR
062200160209     C                   KFLD                    §TA4MNRV
062300090424     C*
062400090424     C* LEGGE IL RECORD SIGLE VARIE
062500090424     C     *LIKE         DEFINE    TBLKUT        KTBKUT
062600090424     C     *LIKE         DEFINE    TBLCOD        KTBCOD
062700090424     C     *LIKE         DEFINE    TBLKEY        KTBKEY
062800090424     C     KEYTAB        KLIST
062900090424     C                   KFLD                    KTBKUT
063000090424     C                   KFLD                    KTBCOD
063100090424     C                   KFLD                    KTBKEY
063200090424     C*
063300090424     C                   ENDSR
063400090423**         VALNUM
0635000904230123456789
