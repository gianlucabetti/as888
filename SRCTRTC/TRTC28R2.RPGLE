000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200170227     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300890921     H*----------------------------------------------------*
000400890921     F*
000500090424     FTITAS31C  IF   E           K DISK
000600990823     FTITA730C  IF   E           K DISK
000700050905     FFNORM01L  IF   E           K DISK
000800090423     FAZORG01L  if   e           k disk
000900910830     FTABEL00F  IF   E           K DISK
001000040531     FFIAR531C  IF   E           K DISK
001100050906     FTIVGD00F  UF A E             DISK
001200050906     D VSV             S              1    DIM(30)                              SIGLA VARIE
001300990910     D VVA             S             11  3 DIM(30)                              IMPOR VARIE
001400090423     D VALNUM          S             10    DIM(1) CTDATA PERRCD(10)
001500170227
001600170227     D psds           sds
001700170227     D  procname         *PROC
001800890921     D KPJBA         E DS
001900990817     D DS4C          E DS
002000050906     D DSQT1         E DS
002100050906     D DS$2          E DS
002200030516     D DTA4A         E DS
002300160209     D DTA4M         E DS
002400151016     D DAR5FAT       E DS
002500050905     D tivafds       e ds                  extname(tivaf00t)
002600170227     D TIS7VASDS     E DS
002700170227
002800090423     D PARAM           DS
002900090423     D  PARDATADA                     8S 0
003000090423     D  PARDATAA                      8S 0
003100090424     D  PARCLI                        7S 0
003200090424     D  PARTIPCLI                     1A
003300090423     D  PARCLIVGD                     8A
003400090424     D  PARTIPFIL                     2A
003500090424     D  PARGGBACK                     3S 0
003600090424     D  PARGGDAELAB                   3S 0
003700090424
003800010402     D*------------------
003900030917      * DS FIAR5
004000030917     d dAr5Bnb       e ds
004100050126
004200050126     d Tibs02ds      e ds
004300111026     d****** dvus          e ds
004400051219
004500051219     d Og143         e ds                  inz
004600060127
004700030917      *
004800021212     D Wtara           s                   like(§QTTPC)
004900021212     D Wntara          s                   like(TASPKF)
005000030917
005100030917     d kAr5Trd         s                   Like(Ar5Trd)
005200030515
005300030515     D Rma15           s             15
005400030515     D WRma15          s             15
005500030515     D II              s              2  0
005600030515     D Pos             s              2  0
005700160209
005800160209     D*--------------------
005900160209     D* DEFINIZIONI A PROCEDURE ESTERNE
006000160209     D*--------------------
006100160209     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
006200160209
006300010402
006400010402
006500170227     C*
006600170227     C* Inizializzo variabili d wrk
006700170227     C                   movel     'N'           wProcedi          1
006800170227     C*
006900170227     C* Inizializzo la transazione
007000170227     C                   CLEAR                   TIS7VASDS
007100170227     C                   EVAL      i§7VASOPZ = 'PRG'
007200170227     C                   CALL(e)   'TIS7VASR1'
007300170227     C                   PARM                    TIS7VASDS
007400170227     C*
007500170227     C* Verifico esito chiamata
007600170227     C                   if        not %error AND
007700170227     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
007800170227     C                   movel     'S'           wProcedi
007900170227     C                   endif
008000170227     C*
008100170227     C* Se ok a procedere => elaboro
008200170227     C                   if        wProcedi = 'S'
008300060127     C*
008400921023     C                   EXSR      DEFCAM
008500090424      *
008600090424      * Gestisco se richiesto cliente SINGOLO o UNIFICANTE
008700090424     C                   if        PARTIPCLI = 'U'
008800040806      *
008900921023     C     KTB           SETLL     TABEL
009000040806    1C                   DO        *HIVAL
009100921023     C     KTB           READE     TABEL                                  58
009200040806    2C  N58              DO
009300040806    3C     TBLFLG        IFNE      '*'
009400921023     C                   MOVEL     TBLUNI        DS4C
009500040806    4C     §4CFSD        IFNE      *BLANK                                        CLI DA SCAR
009600050906     C                   MOVE      §4CFKS        KSCU              7 0
009700050906     C                   MOVEL     TBLKEY        KSC               7 0
009800981203     C*
009900090424    5C     PARCLI        IFGT      *ZERO                                         PARZIALIZZ.
010000090424     C     KSCU          ANDEQ     PARCLI                                        CLIENTE
010100000721     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
010200090424   e5C                   ENDIF
010300090424   e4C                   ENDIF
010400090424   e3C                   ENDIF
010500090424   e2C                   ENDDO
010600090424   e1C  N58              ENDDO
010700060127     C*
010800090424     C                   else
010900090424     C                   eval      KSC = PARCLI
011000090424     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
011100090424     C                   endif
011200090424     C*
011300060127     C                   endif
011400170227     C*
011500170227     C* Finalizzo la transazione
011600170227     C                   EVAL      i§7VASOPZ  = 'RLS'
011700170227     C                   EVAL      i§7VASTIP  = PARTIPFIL
011800170227     C                   EVAL      i§7VASKSU  = PARCLIVGD
011900170227     C                   EVAL      i§7VASTSC  = 'WW'
012000170227     C                   EVAL      i§7VASSTO  = '?'
012100170227     C                   EVAL      i§7VASSTTO = 'G'
012200170227     C                   EVAL      i§7VASPRG  = o§7VASPRG
012300170227     C                   CALL(e)   'TIS7VASR1'
012400170227     C                   PARM                    TIS7VASDS
012500170227     C*
012600170227     C* Verifico esito chiamata
012700170227     C                   if        %error OR o§7VASOK = *off
012800170227     C                   exsr      EXEERR
012900170227     C                   endif
013000160209     C*
013100160209     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
013200160209     C                   CALL      'UBTA400R'
013300160209     C                   PARM                    UBTA4IOPZ
013400160209     C                   PARM      'C'           UBTA4ITLA
013500160209     C                   PARM                    UBTA4IAAS
013600160209     C                   PARM                    UBTA4ILNP
013700160209     C                   PARM                    UBTA4INRS
013800160209     C                   PARM                    UBTA4INSP
013900160209     C                   PARM                    UBTA4ITRC
014000160209     C                   PARM                    UBTA4OERR
014100160209     C                   PARM                    UBTA4ODS
014200160209     C                   PARM                    UBTA4OLEN
014300160209     C                   PARM                    UBTA4ODATI
014400991110     C*
014500921023     C                   SETON                                        LR
014600910830     C*----------------------------------------------------*
014700910830     C     DEFCAM        BEGSR
014800021212      *
014900021212      ** Aggancio tabella $2 - Varie forzate a programma
015000990817     C                   Z-ADD     1             KTBKUT
015100990817     C                   MOVEL     '$2'          KTBCOD
015200990817     C                   MOVEL     1             KTBKEY
015300990817     C     KEYTAB        CHAIN     TABEL00F                           99
015400990817IF  1C     *IN99         IFEQ      *OFF
015500990817     C     TBLFLG        ANDEQ     *BLANKS
015600990817     C                   MOVEL     TBLUNI        DS$2
015700990817E   1C                   ENDIF
015800021212      *
015900050905      ** Aggancio tabella QT - campi standard tassazione
016000021212     C                   MOVEL     'QT'          KTBCOD
016100021212     C                   MOVEL     1             KTBKEY
016200021212     C     KEYTAB        CHAIN     TABEL00F                           99
016300021212IF  1C     *IN99         IFEQ      *OFF
016400021212     C     TBLFLG        ANDEQ     *BLANKS
016500021212     C                   MOVEL     TBLUNI        DSQT1
016600021212E   1C                   ENDIF
016700990817     C*
016800021212      ** Imposto key per tabella clienti con ritorno info fattura
016900990817     C                   MOVE      1             TBLKUT
017000990817     C                   MOVEL     '4C'          TBLCOD
017100990817     C                   MOVEL     *BLANKS       TBLKEY
017200990817     C*
017300910830     C                   ENDSR
017400050906     C*--------------------------------------------------------
017500090423     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
017600000124     C*--------------------------------------------------------
017700921023     C     WRTFIL        BEGSR
017800090423     C*
017900090424     C* Mi posiziono x cliente
018000090423     C                   setoff                                       70
018100090423     C*
018200090424     C     KTAS          SETLL     TITAS31C
018300090424     C                   IF        %found(titas31C)
018400090424     C                   READ      titas31c
018500090424     C                   DOW       not %eof(titas31c) and not *in70
018600090423     C*
018700090423     C* Verifico se data spedizione <= DATAA
018800090424     C                   if        tasKSC <> KSC
018900090423     C                   seton                                        70
019000090423     C                   else
019100090424     C                   if        tasAAS*10000+tasMGS >= wDataDa AND
019200090424     C                             tasAAS*10000+tasMGS <= wDataA
019300050905     C*
019400050905     C* Inizializzo il buffer d output
019500050905     C                   clear                   TIVAFDS
019600160209     C*
019700160209     C* Reperisco TITA4 tipo record 'A'
019800160209     C                   MOVEL     'A'           UBTA4ITRC
019900160209     C                   EXSR      RTVTA4
020000050905     C*
020100941122     C                   MOVEL     TASTBL        VAFTBL
020200941122     C                   Z-ADD     TASLNP        VAFLNP
020300941122     C                   Z-ADD     TASMGS        VAFMGS
020400941122     C                   Z-ADD     TASAAS        VAFAAS
020500941122     C                   Z-ADD     TASNRS        VAFNRS
020600941122     C                   Z-ADD     TASNSP        VAFNSP
020700941122     C                   Z-ADD     TASKSC        VAFKSC
020800941122     C                   MOVEL     TASFIN        VAFFIN
020900941122     C                   Z-ADD     TASLNA        VAFLNA
021000030917      * se particolità varia = 'O' recupero da FIAR5 il numero colli originali
021100030917      *
021200050906     c                   If        %Subst(TasGva:1:1) = 'O'
021300050906
021400050906     c                   eval      kAr5Trd  = 'BNB'
021500050906     c                   clear                   dAr5Bnb
021600050906     c     kFiar5        Chain     Fiar531c
021700050906     c                   If        %Found(Fiar531c)
021800050906     c                   Movel     Ar5Uni        dAr5Bnb
021900050906     c                   EndIf
022000050906      * Colli originali
022100050906     c                   Eval      VafNcl = §Ar5bcor
022200030917
022300030917     c                   else
022400030917      * Colli TITAS
022500941122     C                   Z-ADD     TASNCL        VAFNCL
022600030917     c                   endif
022700030917
022800941122     C                   Z-ADD     TASVLF        VAFVLF
022900941122     C                   Z-ADD     TASPOR        VAFPOR
023000021211      * PESO
023100021211     C                   Z-ADD     TASPKF        VAFPKB
023200021211     C                   IF        TASFPF = 'V'
023300150716     C*
023400150716     C                   IF        TASDFT <= §QTDST
023500150716     C     §QTTCO        MULT      TASNCP        WTARA
023600150716     C     TASPKC        SUB(h)    WTARA         WNTARA
023700150716     C                   Z-ADD     WNTARA        VAFPKC
023800150716     C                   ELSE
023900150716     C     §QTTPC        MULT      TASNCP        WTARA
024000150716     C     TASPKC        SUB(h)    WTARA         WNTARA
024100150716     C                   Z-ADD     WNTARA        VAFPKC
024200150716     C                   ENDIF
024300150716     C*
024400021211     C                   ELSE
024500151020     C                   IF        TASFPF <> 'D'
024600150716     C                   Z-ADD     *zeros        VAFPKC
024700151020     C                   ENDIF
024800021211     C                   ENDIF
024900000124     C* AZZERO CAMPI NON SEMPRE PRESENTI
025000990910     C                   MOVE      *ZERO         WSV
025100990910     C                   MOVE      *ZERO         WVA
025200990910     C                   MOVEA     *BLANKS       VSV
025300990910     C                   MOVEA     *ZERO         VVA
025400990910     C                   CLEAR                   A
025500990910     C                   CLEAR                   VAFSV1
025600990910     C                   CLEAR                   VAFVA1
025700990910     C                   CLEAR                   VAFSV2
025800990910     C                   CLEAR                   VAFVA2
025900990910     C                   CLEAR                   VAFSV3
026000990910     C                   CLEAR                   VAFVA3
026100000124     C                   CLEAR                   VAFSV4
026200000124     C                   CLEAR                   VAFVA4
026300000124     C                   CLEAR                   VAFSV5
026400000124     C                   CLEAR                   VAFVA5
026500000124     C                   CLEAR                   VAFSV6
026600000124     C                   CLEAR                   VAFVA6
026700000124     C                   CLEAR                   VAFSV7
026800000124     C                   CLEAR                   VAFVA7
026900000124     C                   CLEAR                   VAFSV8
027000000124     C                   CLEAR                   VAFVA8
027100990910     C                   CLEAR                   VAFVAX
027200000124     C*
027300000124     C                   MOVEL     TASSV1        WSV
027400990910     C                   Z-ADD     TASVA1        WVA
027500990910     C                   EXSR      CONTR
027600990910     C                   MOVEL     TASSV2        WSV
027700990910     C                   Z-ADD     TASVA2        WVA
027800990910     C                   EXSR      CONTR
027900990910     C                   MOVEL     TASSV3        WSV
028000990910     C                   Z-ADD     TASVA3        WVA
028100990910     C                   EXSR      CONTR
028200990817     C*
028300941122     C                   Z-ADD     TASDFT        VAFDFT
028400941122     C                   Z-ADD     TASNFT        VAFNFT
028500030515      *
028600030515     c                   if        tascbo <> 'FO'
028700941122     C                   Z-ADD     TASRMN        VAFRMN
028800030515     c                   endif
028900030515      *
029000941122     C                   MOVEL     TASFTC        VAFFTC
029100941122     C                   MOVEL     TASTSP        VAFTSP
029200941122     C                   MOVEL     TASCTS        VAFCTS
029300941122     C                   MOVEL     TASFAP        VAFFAP
029400941122     C                   Z-ADD     TASCTR        VAFCTR
029500941122     C                   Z-ADD     TASQFT        VAFQFT
029600941122     C                   MOVEL     TASTC2        VAFTC2
029700000124     C                   Z-ADD     TASIMV        VAFIMV
029800000124     C                   Z-ADD     TASFIV        VAFFIV
029900000124     C                   MOVEL     TASDIV        VAFDIV
030000000124     C                   Z-ADD     TASDRT        VAFDRT
030100160209     C                   MOVEL     §TA4ANAS      VAFNAS
030200000124     C                   MOVEL     TASCAD        VAFCAD
030300000124     C                   MOVEL     TASLOD        VAFLOD
030400000124     C                   MOVEL     TASPRD        VAFPRD
030500000124     C                   MOVEL     TASNZD        VAFNZD
030600000124     C                   MOVEL     TASVAS        VAFVAS
030700000124     C                   Z-ADD     TASIAS        VAFIAS
030800000124    *C                   MOVEL     *blanks       VAFFPC
030900000124    *C                   MOVEL     *blanks       VAFFVC
031000000124    *C                   Z-ADD     *zeros        VAFVLC
031100941122     C     TASSV3        IFNE      *BLANK
031200941122     C                   EXSR      SRTA7
031300941122     C                   END
031400030515     C                   MOVEL     *BLANKS       VAFRMA
031500030515     C* se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
031600160222     c                   if        tascbo <> 'FO'
031700160209     C                   MOVEL     DTA4A         VAFRMA
031800000124     C                   ENDIF
031900030515     C* se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
032000160222     c                   if        tascbo = 'FO'
032100030515     c                   if        §ta4arma <> *blanks
032200030515     C                   MOVEL     §TA4ARMA      Rma15
032300030515      * verifico se campo numerico escludendo i blanks
032400030515     c                   eval      II = 1
032500030515     c                   dow       II <= %len(%trim(Rma15))
032600030515      *
032700030515     c                   If        %subst(%trim(Rma15):II:1) <> *blank
032800030515     c                   eval      WRma15 = (%trim(Wrma15)
032900030515     c                                      + %subst(%trim(Rma15):II:1))
033000030515     c                   endif
033100030515      *
033200030515     c                   eval      II = II + 1
033300030515      *
033400030515     c                   enddo
033500030515     c                   clear                   rma15
033600030515     c                   evalr     rma15 = %trim(Wrma15)
033700030515      *
033800030515     c                   eval      II = 1
033900030515     c                   setoff                                       29
034000030515     c                   dow       II <= %len(%trim(Rma15))
034100030515      *
034200030515     c                   If        %subst(%trim(Rma15):II:1) <> *blank
034300030516     c                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
034400030516     c                             VALNUM(1))
034500030515     c                   if        pos = 0
034600030515     c                   seton                                        29
034700030515     c                   leave
034800030515     c                   endif
034900030515     c                   endif
035000030515      *
035100030515     c                   eval      II = II + 1
035200030515      *
035300030515     c                   enddo
035400030515     c                   if        *in29 = *OFF
035500030516     c                   eval      VAFRMA= %editc(tasrmn:'Z')
035600030516     C                   MOVEL     rma15         VAFRMN
035700030515     c                   else
035800030515     C                   EVAL      VAFRMA = §ta4arma
035900030515     C                   EVAL      VAFRMN = tasrmn
036000030515     c                   endif
036100030515      *
036200030515     C                   endif
036300030515     C                   endif
036400990910     C*
036500990910     C* SCRIVO LE VARIE
036600990910     C                   Z-ADD     1             A
036700990910     C     VSV(A)        DOWNE     ' '
036800990910     C                   SELECT
036900990910     C     VAFSV1        WHENEQ    ' '
037000990910     C                   MOVEL     VSV(A)        VAFSV1
037100990910     C                   Z-ADD     VVA(A)        VAFVA1
037200990910     C*
037300990910     C     VAFSV2        WHENEQ    ' '
037400990910     C                   MOVEL     VSV(A)        VAFSV2
037500990910     C                   Z-ADD     VVA(A)        VAFVA2
037600990910     C*
037700990910     C     VAFSV3        WHENEQ    ' '
037800990910     C                   MOVEL     VSV(A)        VAFSV3
037900990910     C                   Z-ADD     VVA(A)        VAFVA3
038000000124     C*
038100000124     C     VAFSV4        WHENEQ    ' '
038200000124     C                   MOVEL     VSV(A)        VAFSV4
038300000124     C                   Z-ADD     VVA(A)        VAFVA4
038400000124     C*
038500000124     C     VAFSV5        WHENEQ    ' '
038600000124     C                   MOVEL     VSV(A)        VAFSV5
038700000124     C                   Z-ADD     VVA(A)        VAFVA5
038800000124     C*
038900000124     C     VAFSV6        WHENEQ    ' '
039000000124     C                   MOVEL     VSV(A)        VAFSV6
039100000124     C                   Z-ADD     VVA(A)        VAFVA6
039200000124     C*
039300000124     C     VAFSV7        WHENEQ    ' '
039400000124     C                   MOVEL     VSV(A)        VAFSV7
039500000124     C                   Z-ADD     VVA(A)        VAFVA7
039600000124     C*
039700000124     C     VAFSV8        WHENEQ    ' '
039800000124     C                   MOVEL     VSV(A)        VAFSV8
039900000124     C                   Z-ADD     VVA(A)        VAFVA8
040000990910     C                   OTHER
040100990910     C                   ADD       VVA(A)        VAFVAX
040200990910     C                   ENDSL
040300990910     C*
040400990910     C                   ADD       1             A
040500990910     C                   ENDDO
040600050126      * Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
040700050126     c                   Clear                   VafRmx
040800050126     C                   MOVEL     VAFKSC        WLB3              3 0
040900051219     c     WLB3          chain     AZORG
041000051219     c                   if            %found(AZORG01L)
041100051219     c                             and ORGfva = *blanks
041200051219     c                   movel     ORGde3        Og143
041300051219     c                   else
041400051219     c                   clear                   Og143
041500051219     c                   endif
041600050126
041700050126     c                   Select
041800050126
041900050126     C* Recupera Riferimento Alfanumerico da ORM
042000050126     C                   When      §4cRfo = 'S'
042100160209     C*
042200160209     C* Reperisco TITA4 tipo record 'M'
042300160209     C                   MOVEL     'M'           UBTA4ITRC
042400160209     C                   EXSR      RTVTA4
042500160209     C*
042600160209     C                   IF        DTA4M <> *zeros
042700050126     C     KORM          CHAIN     FNORM01L                           61
042800050126     C     *IN61         IFEQ      *OFF
042900050126     C                   MOVEL     ORMRFA        VAFRMX
043000050126     C                   ENDIF
043100050126     C                   ENDIF
043200160209     C*
043300050126      * Ritorna al cliente il campo XCO
043400111026     c***                When      §4cRfo = 'X' and TasXco <> *Blanks
043500050126      * recupero il valore da tornare dalla tabella VUS
043600111026     c***                Clear                   Tibs02ds
043700111026     c***                Clear                   dvus
043800111026     c***                Eval      T02mod = 'C'
043900111026     c***                Eval      T02sif = knsif
044000111026     c***                Eval      T02cod = 'VUS'
044100111026     c***                Eval      T02ke1 = TasXco
044200111026     c***                Call      'TIBS02R'
044300111026     c***                Parm                    kpjba
044400111026     c***                Parm                    Tibs02ds
044500111026     c***                If        T02err = *blanks
044600111026     c***                Eval      dvus = T02uni
044700111026     c***                EndIf
044800111026     c***                Eval      vafrmx = §vuvaf
044900050126
045000160209     C*
045100000122     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
045200051219     c                   When      §OGntw = 'DPD'
045300000125     C                   movel     TASDCM        VAFRMX
045400090423     C
045500050126     c                   EndSl
045600151016     C*
045700151016     C* Eseguo eventuali "forzature"
045800151016     C                   EXSR      RTVAR5FAT
045900060203     C*
046000060203     C* Quindi scarico il buffer nel file d out generico download
046100050905     C                   exsr      WRIVGD
046200050905     C*
046300090423     C                   endif
046400090424     C                   endif
046500090423     C*
046600090423     C* Proseguo la lettura delle bolle
046700090424     C                   read      titas31c
046800090423     C*
046900090423     C                   ENDDO
047000090423     C                   ENDIF
047100090423     C*
047200000124     C                   ENDSR
047300000124     C*-----------------------------------------------------------*
047400000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
047500000124     C*-----------------------------------------------------------*
047600941122     C     SRTA7         BEGSR
047700990817     C*
047800990823     C     KTA7          SETLL     TITA730C
047900990823     C     KTA7          READE     TITA730C                               59
048000990817DO  1C     *IN59         DOWEQ     *OFF
048100990910     C                   MOVEL     TA7SVN        WSV
048200990910     C                   Z-ADD     TA7VAN        WVA
048300990910     C                   EXSR      CONTR
048400990910     C*
048500990823     C     KTA7          READE     TITA730C                               59
048600990817E   1C                   ENDDO
048700990817     C*
048800941122     C                   ENDSR
048900990910     C*--------------------------------------------------------
049000000124     C* CONTROLLO SE VARIE GIA' PRESENTI
049100000124     C*--------------------------------------------------------
049200990910     C     CONTR         BEGSR
049300000124IF  1C     WSV           IFNE      ' '
049400990910     C*
049500990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
049600990910     C     WSV           ANDNE     §$2IVA
049700990910     C                   ADD       1             A                 3 0
049800990910     C                   MOVEL     WSV           VSV(A)
049900990910     C                   Z-ADD     WVA           VVA(A)
050000990910E   4C                   ENDIF
050100000124E   1C                   ENDIF
050200000124     C                   ENDSR
050300090423
050400090423
050500050905
050600050905      /TITLE Scrittura record TIVAF in file TIVGD00F (file VAS generico download)
050700050905     C     wriVGD        BEGSR
050800050905     C*
050900050905     C                   clear                   tivgd000
051000090423     C                   eval      vgdDTA = TIVAFDS
051100090424     C                   eval      vgdTIP = PARTIPFIL
051200090423     C                   eval      vgdKSU = PARCLIVGD
051300090423     C                   eval      vgdTSC = 'WW'
051400050905     C                   eval      vgdDAT = datcor
051500170227     C                   eval      vgdPRG = o§7VASPRG
051600170227     C                   eval      vgdPGM = procname
051700170227     C                   eval      vgdSTO = '?'
051800050905     C                   write     tivgd000
051900050905     C*
052000050905     C                   ENDSR
052100151016
052200151016
052300151016
052400151016     C     RTVAR5FAT     BEGSR
052500151016     C*
052600151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
052700151028     C                   SETOFF                                       7172
052800151016     C                   EVAL      kAr5Trd = 'FAT'
052900151016     C                   CLEAR                   DAR5FAT
053000151016     C     kFiar5        CHAIN     FIAR531C
053100151016     C                   IF        %found(FIAR531C)
053200151016     C                   MOVEL     AR5UNI        DAR5FAT
053300170130     C                   IF        (§AR5FPTAS = 'T' OR §AR5FPTAS = 'D') AND
053400170130     C                             §AR5PKTAS > *zeros
053500170130     C                   SETON                                        71
053600170130     C                   EVAL      VAFPKC = §AR5PKTAS
053700170130     C                   ENDIF
053800170130     C                   IF        (§AR5FVTAS = 'T' OR §AR5FVTAS = 'D') AND
053900170130     C                             §AR5VLTAS > *zeros
054000170130     C                   SETON                                        72
054100170130     C                   EVAL      VAFVLC = §AR5VLTAS
054200170130     C                   ENDIF
054300151016     C                   ENDIF
054400151016     C*
054500151016     C                   ENDSR
054600160209
054700160209
054800160209
054900160209     C     RTVTA4        BEGSR
055000160209     C*
055100160209     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
055200160209     C                   CLEAR                   DTA4A
055300160209     C                   CLEAR                   DTA4M
055400160209     C*
055500160209     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
055600160209     C                   CALL      'UBTA400R'
055700160209     C                   PARM      *blanks       UBTA4IOPZ
055800160209     C                   PARM      *blanks       UBTA4ITLA
055900160209     C                   PARM      tasAAS        UBTA4IAAS
056000160209     C                   PARM      tasLNP        UBTA4ILNP
056100160209     C                   PARM      tasNRS        UBTA4INRS
056200160209     C                   PARM      tasNSP        UBTA4INSP
056300160209     C                   PARM                    UBTA4ITRC
056400160209     C                   PARM                    UBTA4OERR
056500160209     C                   PARM                    UBTA4ODS
056600160209     C                   PARM                    UBTA4OLEN
056700160209     C                   PARM                    UBTA4ODATI
056800160209     C*
056900160209     C                   IF        UBTA4OERR = *zeros
057000160209     C                   SELECT
057100160209     C* Gestione output tipo record 'A'
057200160209     C                   WHEN      UBTA4ODS = 'DTA4A'
057300160209     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
057400160209     C* Gestione output tipo record 'M'
057500160209     C                   WHEN      UBTA4ODS = 'DTA4M'
057600160209     C                   EVAL      DTA4M = %subst(UBTA4ODATI:1:UBTA4OLEN)
057700160209     C                   ENDSL
057800160209     C*
057900160209     C                   ENDIF
058000160209     C*
058100160209     C                   ENDSR
058200170227
058300170227
058400170227
058500170227     C*------------------------------------------------------------------------*
058600170227     C* EXEERR - Routine di esecuzione azioni su Errore
058700170227     C*------------------------------------------------------------------------*
058800170227     C     EXEERR        BEGSR
058900170227     C*
059000170227     C                   dump(A)
059100170227     C                   rolbk
059200170227     C                   seton                                        lr
059300170227     C                   return
059400170227     C*
059500170227     C                   ENDSR
059600170227     C*------------------------------------------------------------------------*
059700151016
059800151016
059900151016
060000090424     C*----------------------------------------------------*
060100090424     C     *INZSR        BEGSR
060200090424     C*
060300090424     C     *ENTRY        PLIST
060400090424     C                   PARM                    KPJBA
060500090424     C                   MOVEL     KPJBU         PARAM
060600090424      *
060700090424      * Calcola la data corrente
060800160209     c                   z-add     *zeros        datcor            8 0          *d.corrente aa/m/g
060900160209     C                   eval      datcor = %dec(%date() : *ISO)
061000090424      *
061100090424      * Se nn già ricevute nei parametri d lancio calcolo le date DA/A
061200090424     C                   z-add     *hival        wDataDa           8 0
061300090424     C                   z-add     *hival        wDataA            8 0
061400090424     C                   if        PARDATADA > *zeros AND
061500090424     C                             PARDATAA  > *zeros
061600090424     C                   z-add     PARDATADA     wDataDa
061700090424     C                   z-add     PARDATAA      wDataA
061800090424     C                   else
061900090424     C                   eval      wDataDa=%dec(%date(datcor)-
062000090424     C                                     %days(PARGGBACK))
062100090424     C                   eval      wDataA =%dec(%date(wDataDa)+
062200090424     C                                     %days(PARGGDAELAB))
062300090424     C                   endif
062400090424      *
062500090424      * Verifico i parametri ricevuti in ingresso
062600090424     C                   if        PARCLIVGD = *zeros
062700090424     C                   movel     *all'0'       PARCLIVGD
062800090424     C                   move      PARCLI        PARCLIVGD
062900090424     C                   endif
063000090424      *
063100090424     C* TABEL
063200090424     C     KTB           KLIST
063300090424     C                   KFLD                    TBLKUT
063400090424     C                   KFLD                    TBLCOD
063500090424      * FIAR
063600090424     c     kFiar5        Klist
063700090424     c                   Kfld                    TasAas
063800090424     c                   Kfld                    TasLnp
063900090424     c                   Kfld                    TasNrs
064000090424     c                   Kfld                    TasNsp
064100090424     c                   Kfld                    kAr5Trd
064200090424     C* TITAS
064300090424     C     KTAS          KLIST
064400090424     C                   KFLD                    KSC
064500090424     C* TITA7
064600090424     C     KTA7          KLIST
064700090424     C                   KFLD                    TASAAS
064800090424     C                   KFLD                    TASLNP
064900090424     C                   KFLD                    TASNRS
065000090424     C                   KFLD                    TASNSP
065100090424     C                   KFLD                    TASTBL
065200090424     C     *LIKE         DEFINE    TASVA1        WVA
065300090424     C     *LIKE         DEFINE    TASSV1        WSV
065400090424     C*
065500090424     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
065600090424     C     KORM          KLIST
065700160209     C                   KFLD                    §TA4MPOE
065800160209     C                   KFLD                    §TA4MNSR
065900160209     C                   KFLD                    §TA4MNOR
066000160209     C                   KFLD                    §TA4MNRV
066100090424     C*
066200090424     C* LEGGE IL RECORD SIGLE VARIE
066300090424     C     *LIKE         DEFINE    TBLKUT        KTBKUT
066400090424     C     *LIKE         DEFINE    TBLCOD        KTBCOD
066500090424     C     *LIKE         DEFINE    TBLKEY        KTBKEY
066600090424     C     KEYTAB        KLIST
066700090424     C                   KFLD                    KTBKUT
066800090424     C                   KFLD                    KTBCOD
066900090424     C                   KFLD                    KTBKEY
067000090424     C*
067100090424     C                   ENDSR
067200090423**         VALNUM
0673000904230123456789
